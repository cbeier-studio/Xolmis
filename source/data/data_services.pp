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
      Result := ANewStatus in [bstAvailable];
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
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ApplyStatusChange(ABand, bstBroken);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
  finally
    FreeAndNil(FOldBand);
  end;

  // write the band history
  LogEvent(ABand, bevDischarge, ADate, 0, 0, 0, ABand.CarrierId, ANotes);
end;

procedure TBandMovementService.MarkAsLost(ABand: TBand; ADate: TDate; const ANotes: String);
var
  FOldBand: TBand;
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ApplyStatusChange(ABand, bstLost);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
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
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.SupplierId := ASupplierId;
    ABand.RequesterId := ARequesterId;
    ABand.CarrierId := ARequesterId;
    ApplyStatusChange(ABand, bstAvailable);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
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
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.RequesterId := ACarrierId;
    ABand.CarrierId := ACarrierId;
    ApplyStatusChange(ABand, bstAvailable);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
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
  FIndividual, FOldIndividual: TIndividual;
  FIndividualRepo: TIndividualRepository;
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FBandRepo.GetById(ABand.Id, FOldBand);
    ABand.IndividualId := 0;
    ApplyStatusChange(ABand, bstRemoved);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
  finally
    FreeAndNil(FOldBand);
  end;

  FIndividualRepo := TIndividualRepository.Create(DMM.sqlCon);
  FIndividual := TIndividual.Create();
  FOldIndividual := TIndividual.Create();
  try
    FIndividualRepo.GetById(AIndividualId, FIndividual);
    FOldIndividual.Assign(FIndividual);
    if (FIndividual.Id > 0) then
    begin
      FIndividual.RemovedBandId := ABand.Id;
      FIndividual.BandChangeDate := ADate;
    end;

    // write the record history
    lstDiff := TStringList.Create;
    try
      if FIndividual.Diff(FOldIndividual, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbIndividuals, haEdited, FOldIndividual.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
  finally
    FreeAndNil(FIndividual);
    FIndividualRepo.Free;
  end;

  // write the band history
  LogEvent(ABand, bevDischarge, ADate, 0, 0, 0, ABand.CarrierId, ANotes);
end;

procedure TBandMovementService.ReturnToSupplier(ABand: TBand; ASupplierId: Integer; ADate: TDate;
  const ANotes: String);
var
  FOldBand: TBand;
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.SupplierId := ASupplierId;
    ABand.CarrierId := 0;
    ApplyStatusChange(ABand, bstReturned);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
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
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    OldCarrier := ABand.RequesterId;
    ABand.CarrierId := ANewCarrierId;
    ABand.RequesterId := ANewCarrierId;
    ApplyStatusChange(ABand, bstTransferred);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
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
  FIndividual, FOldIndividual: TIndividual;
  FIndividualRepo: TIndividualRepository;
  FOldBand: TBand;
  lstDiff: TStrings;
  D: String;
begin
  FOldBand := TBand.Create();
  try
    FOldBand.Assign(ABand);

    ABand.IndividualId := AIndividualId;
    ApplyStatusChange(ABand, bstUsed);

    // write the record history
    lstDiff := TStringList.Create;
    try
      if ABand.Diff(FOldBand, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbBands, haEdited, FOldBand.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
  finally
    FreeAndNil(FOldBand);
  end;

  FIndividualRepo := TIndividualRepository.Create(DMM.sqlCon);
  FIndividual := TIndividual.Create();
  FOldIndividual := TIndividual.Create();
  try
    FIndividualRepo.GetById(AIndividualId, FIndividual);
    FOldIndividual.Assign(FIndividual);
    if (FIndividual.Id > 0) then
    begin
      if FIndividual.BandChangeDate <> NullDate then
        FIndividual.BandingDate := ADate;
      FIndividual.BandId := ABand.Id;
      FIndividual.BandName := ABand.Size + ' ' + IntToStr(ABand.Number);
    end;

    // write the record history
    lstDiff := TStringList.Create;
    try
      if FIndividual.Diff(FOldIndividual, lstDiff) then
      begin
        for D in lstDiff do
          WriteRecHistory(tbIndividuals, haEdited, FOldIndividual.Id,
            ExtractDelimited(1, D, [';']),
            ExtractDelimited(2, D, [';']),
            ExtractDelimited(3, D, [';']));
      end;
    finally
      FreeAndNil(lstDiff);
    end;
  finally
    FreeAndNil(FOldIndividual);
    FreeAndNil(FIndividual);
    FIndividualRepo.Free;
  end;

  // write the band history
  LogEvent(ABand, bevUse, ADate, 0, 0, 0, 0, ANotes);
end;

end.

