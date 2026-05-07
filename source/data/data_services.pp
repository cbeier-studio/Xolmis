{ Xolmis Data Services library

  Copyright (C) 2026 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit data_services;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, StrUtils,
  models_record_types, models_bands, models_birds;

type

  { TBandMovementService }

  TBandMovementService = class
  private
    FBandRepo: TBandRepository;
    FHistoryRepo: TBandHistoryRepository;

    function CanTransitionTo(const ABand: TBand; ANewStatus: TBandStatus): Boolean;
    procedure ApplyStatusChange(ABand: TBand; ANewStatus: TBandStatus);
    procedure LogEvent(ABand: TBand; AEvent: TBandEvent; ADate: TDate;
      const AOrderNumber: Integer = 0;
      const ASupplierId: Integer = 0;
      const ASenderId: Integer = 0;
      const ARequesterId: Integer = 0;
      const ANotes: String = '');
  public
    constructor Create(aBandRepo: TBandRepository);
    destructor Destroy; override;

    procedure ReceiveFromSupplier(ABand: TBand; ASupplierId, ARequesterId: Integer; ADate: TDate;
      const AOrderNumber: Integer = 0; const ANotes: String = '');
    procedure ReceiveTransfer(ABand: TBand; ASenderId, ACarrierId: Integer; ADate: TDate;
      const ANotes: String = '');
    procedure UseInCapture(ABand: TBand; AIndividualId: Integer; ADate: TDate;
      const ANotes: String = '');
    procedure RemoveFromIndividual(ABand: TBand; AIndividualId: Integer; ADate: TDate; const ANotes: String = '');
    procedure TransferToAnotherPerson(ABand: TBand; ANewCarrierId: Integer; ADate: TDate;
      const ANotes: String = '');
    procedure MarkAsBroken(ABand: TBand; ADate: TDate; const ANotes: String = '');
    procedure MarkAsLost(ABand: TBand; ADate: TDate; const ANotes: String = '');
    procedure ReturnToSupplier(ABand: TBand; ASupplierId: Integer; ADate: TDate;
      const ANotes: String = '');

    procedure UndoBandUse(ABand: TBand);
    procedure UndoBandRemoval(ABand: TBand; AIndividualId: Integer);
  end;

  { TIndividualUpdateService }

  TIndividualUpdateService = class
  private
    FIndRepo: TIndividualRepository;
  public
    constructor Create(ARepo: TIndividualRepository);
    destructor Destroy; override;

    procedure ApplyCaptureToIndividual(Capture: TCapture);
    procedure ApplyBanding(Capture: TCapture);
    procedure ApplyBandRemoval(Capture: TCapture);
    procedure ApplyBandChange(OldCapture, NewCapture: TCapture);
    procedure UndoCaptureFromIndividual(Capture: TCapture);
  end;


implementation

uses
  utils_locale, utils_global, data_types, udm_main;

{ TBandMovementService }

constructor TBandMovementService.Create(aBandRepo: TBandRepository);
begin
  inherited Create;
  FBandRepo := aBandRepo;
  FHistoryRepo := TBandHistoryRepository.Create(DMM.sqlCon);
end;

procedure TBandMovementService.ApplyStatusChange(ABand: TBand; ANewStatus: TBandStatus);
begin
  if not CanTransitionTo(ABand, ANewStatus) then
    raise Exception.CreateFmt(rsInvalidBandStatusTransition,
      [BAND_STATUSES[ABand.Status], BAND_STATUSES[ANewStatus]]);

  ABand.Status := ANewStatus;
  FBandRepo.Update(ABand);
end;

function TBandMovementService.CanTransitionTo(const ABand: TBand; ANewStatus: TBandStatus): Boolean;
begin
  case ABand.Status of
    bstAvailable:
      Result := ANewStatus in [bstUsed, bstTransferred, bstBroken, bstLost, bstReturned];
    bstUsed:
      Result := ANewStatus in [bstRemoved];
    bstTransferred, bstReturned, bstOrdered:
      Result := ANewStatus in [bstAvailable, bstUsed];
    bstBroken, bstLost, bstRemoved:
      Result := False;
  else
    Result := False;
  end;
end;

destructor TBandMovementService.Destroy;
begin
  FHistoryRepo.Free;
  inherited Destroy;
end;

procedure TBandMovementService.LogEvent(ABand: TBand; AEvent: TBandEvent; ADate: TDate;
  const AOrderNumber: Integer; const ASupplierId: Integer; const ASenderId: Integer;
  const ARequesterId: Integer; const ANotes: String);
var
  H: TBandHistory;
begin
  H := TBandHistory.Create;
  try
    H.BandId := ABand.Id;
    H.EventType := AEvent;
    H.EventDate := ADate;
    H.OrderNumber := AOrderNumber;
    H.SupplierId := ASupplierId;
    H.SenderId := ASenderId;
    H.RequesterId := ARequesterId;
    H.Notes := ANotes;

    FHistoryRepo.Insert(H);
  finally
    H.Free;
  end;
end;

procedure TBandMovementService.MarkAsBroken(ABand: TBand; ADate: TDate; const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ApplyStatusChange(ABand, bstBroken);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevDischarge, ADate, 0, 0, 0, ABand.CarrierId, ANotes);
end;

procedure TBandMovementService.MarkAsLost(ABand: TBand; ADate: TDate; const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ApplyStatusChange(ABand, bstLost);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevDischarge, ADate, 0, 0, 0, ABand.CarrierId, ANotes);
end;

procedure TBandMovementService.ReceiveFromSupplier(ABand: TBand; ASupplierId, ARequesterId: Integer;
  ADate: TDate; const AOrderNumber: Integer; const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.SupplierId := ASupplierId;
    ABand.RequesterId := ARequesterId;
    ABand.CarrierId := ARequesterId;
    ApplyStatusChange(ABand, bstAvailable);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevReceive, ADate, AOrderNumber,
    ASupplierId, 0, ARequesterId, ANotes);
end;

procedure TBandMovementService.ReceiveTransfer(ABand: TBand; ASenderId, ACarrierId: Integer; ADate: TDate;
  const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.RequesterId := ACarrierId;
    ABand.CarrierId := ACarrierId;
    ApplyStatusChange(ABand, bstAvailable);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevTransfer, ADate, 0, 0,
    ASenderId, ACarrierId, ANotes);
end;

procedure TBandMovementService.RemoveFromIndividual(ABand: TBand; AIndividualId: Integer; ADate: TDate;
  const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FBandRepo.GetById(ABand.Id, FOldBand);
    ABand.IndividualId := 0;
    ApplyStatusChange(ABand, bstRemoved);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevDischarge, ADate, 0, 0, 0, ABand.CarrierId, ANotes);
end;

procedure TBandMovementService.ReturnToSupplier(ABand: TBand; ASupplierId: Integer; ADate: TDate;
  const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.SupplierId := ASupplierId;
    ABand.CarrierId := 0;
    ApplyStatusChange(ABand, bstReturned);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevReturn, ADate, 0, ASupplierId, 0, 0, ANotes);
end;

procedure TBandMovementService.TransferToAnotherPerson(ABand: TBand; ANewCarrierId: Integer; ADate: TDate;
  const ANotes: String);
var
  OldCarrier: Integer;
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    OldCarrier := ABand.RequesterId;
    ABand.CarrierId := ANewCarrierId;
    ABand.RequesterId := ANewCarrierId;
    ApplyStatusChange(ABand, bstTransferred);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevTransfer, ADate, 0, 0,
    OldCarrier, ANewCarrierId, ANotes);
end;

procedure TBandMovementService.UseInCapture(ABand: TBand; AIndividualId: Integer; ADate: TDate;
  const ANotes: String);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.IndividualId := AIndividualId;
    ApplyStatusChange(ABand, bstUsed);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevUse, ADate, 0, 0, 0, 0, ANotes);
end;

procedure TBandMovementService.UndoBandUse(ABand: TBand);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.IndividualId := 0;
    ABand.Status := bstAvailable;
    FBandRepo.Update(ABand);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // delete the last bevUse history entry for this band
  FHistoryRepo.DeleteLastByBandAndEvent(ABand.Id, bevUse);
end;

procedure TBandMovementService.UndoBandRemoval(ABand: TBand; AIndividualId: Integer);
var
  FOldBand: TBand;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.IndividualId := AIndividualId;
    ABand.Status := bstUsed;
    FBandRepo.Update(ABand);

    // write the record history
    WriteDiff(tbBands, FOldBand, ABand);
  finally
    FreeAndNil(FOldBand);
  end;

  // delete the last bevDischarge history entry for this band
  FHistoryRepo.DeleteLastByBandAndEvent(ABand.Id, bevDischarge);
end;

{ TIndividualUpdateService }

constructor TIndividualUpdateService.Create(ARepo: TIndividualRepository);
begin
  inherited Create;
  FIndRepo := ARepo;
end;

procedure TIndividualUpdateService.ApplyBandChange(OldCapture, NewCapture: TCapture);
var
  Ind: TIndividual;
begin
  if (NewCapture.IndividualId <= 0) then
    Exit;

  Ind := TIndividual.Create;
  try
    FIndRepo.GetById(NewCapture.IndividualId, Ind);

    // Band change
    if (OldCapture.BandId <> NewCapture.BandId) and (NewCapture.BandId > 0) then
    begin
      Ind.BandChangeDate := NewCapture.CaptureDate;
      Ind.BandId := NewCapture.BandId;
      FIndRepo.Update(Ind);
    end;

  finally
    Ind.Free;
  end;
end;

procedure TIndividualUpdateService.ApplyBanding(Capture: TCapture);
var
  Ind: TIndividual;
begin
  if (Capture.IndividualId <= 0) or (Capture.BandId <= 0) then
    Exit;

  Ind := TIndividual.Create;
  try
    FIndRepo.GetById(Capture.IndividualId, Ind);

    if Ind.BandingDate = NullDate then
    begin
      Ind.BandId := Capture.BandId;
      Ind.BandingDate := Capture.CaptureDate;
      FIndRepo.Update(Ind);
    end;

  finally
    Ind.Free;
  end;
end;

procedure TIndividualUpdateService.ApplyBandRemoval(Capture: TCapture);
var
  FIndividual, FOldIndividual: TIndividual;
begin
  if (Capture.IndividualId <= 0) or (Capture.RemovedBandId <= 0) then
    Exit;

  FIndividual := TIndividual.Create;
  FOldIndividual := TIndividual.Create();
  try
    FIndRepo.GetById(Capture.IndividualId, FIndividual);
    FOldIndividual.Assign(FIndividual);

    if FIndividual.BandId = Capture.RemovedBandId then
    begin
      FIndividual.RemovedBandId := Capture.RemovedBandId;
      FIndividual.BandId := Capture.BandId;
      FIndividual.BandChangeDate := Capture.CaptureDate;

      // Update colored bands
      FIndividual.RightTarsus := Capture.RightTarsus;
      FIndividual.LeftTarsus := Capture.LeftTarsus;
      FIndividual.RightTibia := Capture.RightTibia;
      FIndividual.LeftTibia := Capture.LeftTibia;

      FIndRepo.Update(FIndividual);
    end;

    // write the record history
    WriteDiff(tbIndividuals, FOldIndividual, FIndividual);
  finally
    FOldIndividual.Free;
    FIndividual.Free;
  end;
end;

procedure TIndividualUpdateService.ApplyCaptureToIndividual(Capture: TCapture);
var
  FIndividual, FOldIndividual: TIndividual;
begin
  if Capture.IndividualId <= 0 then
    Exit;

  FIndividual := TIndividual.Create;
  FOldIndividual := TIndividual.Create();
  try
    FIndRepo.GetById(Capture.IndividualId, FIndividual);
    FOldIndividual.Assign(FIndividual);

    // 1. Update the main band
    if (Capture.BandId > 0) and (FIndividual.BandId <> Capture.BandId) then
    begin
      // If it did not has band → initial banding
      if FIndividual.BandId = 0 then
        FIndividual.BandingDate := Capture.CaptureDate;

      FIndividual.BandId := Capture.BandId;
      //FIndividual.BandName := ;
    end;

    // 2. Update double band
    //if (Capture.DoubleBandId > 0) and (FIndividual.DoubleBandId <> Capture.DoubleBandId) then
    //  FIndividual.DoubleBandId := Capture.DoubleBandId;

    // 3. Update removed band
    if (Capture.RemovedBandId > 0) and (FIndividual.RemovedBandId <> Capture.RemovedBandId) then
    begin
      FIndividual.RemovedBandId := Capture.RemovedBandId;
      FIndividual.BandChangeDate := Capture.CaptureDate;
    end;

    // 4. Update colored bands
    FIndividual.RightTarsus := Capture.RightTarsus;
    FIndividual.LeftTarsus := Capture.LeftTarsus;
    FIndividual.RightTibia := Capture.RightTibia;
    FIndividual.LeftTibia := Capture.LeftTibia;

    // 5. Save
    FIndRepo.Update(FIndividual);

    // write the record history
    WriteDiff(tbIndividuals, FOldIndividual, FIndividual);
  finally
    FOldIndividual.Free;
    FIndividual.Free;
  end;
end;

destructor TIndividualUpdateService.Destroy;
begin
  inherited Destroy;
end;

procedure TIndividualUpdateService.UndoCaptureFromIndividual(Capture: TCapture);
var
  FIndividual, FOldIndividual: TIndividual;
begin
  if Capture.IndividualId <= 0 then
    Exit;

  FIndividual := TIndividual.Create;
  FOldIndividual := TIndividual.Create;
  try
    FIndRepo.GetById(Capture.IndividualId, FIndividual);
    FOldIndividual.Assign(FIndividual);

    // Undo band removal effect: restore the removed band as the current band
    if (Capture.RemovedBandId > 0) and (FIndividual.RemovedBandId = Capture.RemovedBandId) then
    begin
      FIndividual.RemovedBandId := 0;
      FIndividual.BandChangeDate := NullDate;
      // Restore the removed band as the current band (undo the swap)
      if FIndividual.BandId = Capture.BandId then
        FIndividual.BandId := Capture.RemovedBandId;
      FIndividual.RightTarsus := EmptyStr;
      FIndividual.LeftTarsus := EmptyStr;
      FIndividual.RightTibia := EmptyStr;
      FIndividual.LeftTibia := EmptyStr;
    end
    // Undo initial banding effect: clear band and banding date
    else if (Capture.BandId > 0) and (FIndividual.BandId = Capture.BandId) then
    begin
      FIndividual.BandId := 0;
      FIndividual.BandingDate := NullDate;
      FIndividual.RightTarsus := EmptyStr;
      FIndividual.LeftTarsus := EmptyStr;
      FIndividual.RightTibia := EmptyStr;
      FIndividual.LeftTibia := EmptyStr;
    end;

    FIndRepo.Update(FIndividual);

    // write the record history
    WriteDiff(tbIndividuals, FOldIndividual, FIndividual);
  finally
    FOldIndividual.Free;
    FIndividual.Free;
  end;
end;

end.

