{ Xolmis Bands models

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit models_bands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fgl, fpjson, DateUtils, models_record_types;

type
  TBirdMark = class
    BodyPart: TBodyPart;
    Index: Integer;
    Color: TBandColorCode;
    MarkType: TMarkType;
    Inscription: String;
    BandId: Integer;
  end;

  TBirdMarks = specialize TFPGObjectList<TBirdMark>;

type

  { TBand }

  TBand = class(TXolmisRecord)
  protected
    FFullName: String;
    FSize: String;
    FNumber: Integer;
    FStatus: TBandStatus;
    FSource: TBandSource;
    FPrefix: String;
    FSuffix: String;
    FBandColor: String;
    FBandType: TMarkType;
    FSupplierId: Integer;
    FRequesterId: Integer;
    FCarrierId: Integer;
    FIndividualId: Integer;
    FProjectId: Integer;
    FReported: Boolean;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TBand; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TBand): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property Size: String read FSize write FSize;
    property Number: Integer read FNumber write FNumber;
    property Status: TBandStatus read FStatus write FStatus;
    property Source: TBandSource read FSource write FSource;
    property Prefix: String read FPrefix write FPrefix;
    property Suffix: String read FSuffix write FSuffix;
    property BandColor: String read FBandColor write FBandColor;
    property BandType: TMarkType read FBandType write FBandType;
    property SupplierId: Integer read FSupplierId write FSupplierId;
    property RequesterId: Integer read FRequesterId write FRequesterId;
    property CarrierId: Integer read FCarrierId write FCarrierId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property Reported: Boolean read FReported write FReported;
    property Notes: String read FNotes write FNotes;
  end;

  { TBandRepository }

  TBandRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByNumber(const aSize: String; const aNumber: Integer; E: TBand);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TBandHistory }

  TBandHistory = class(TXolmisRecord)
  protected
    FBandId: Integer;
    FEventType: TBandEvent;
    FEventDate: TDate;
    FOrderNumber: Integer;
    FSupplierId: Integer;
    FSenderId: Integer;
    FRequesterId: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TBandHistory; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TBandHistory): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property BandId: Integer read FBandId write FBandId;
    property EventType: TBandEvent read FEventType write FEventType;
    property EventDate: TDate read FEventDate write FEventDate;
    property OrderNumber: Integer read FOrderNumber write FOrderNumber;
    property SupplierId: Integer read FSupplierId write FSupplierId;
    property SenderId: Integer read FSenderId write FSenderId;
    property RequesterId: Integer read FRequesterId write FRequesterId;
    property Notes: String read FNotes write FNotes;
  end;

  { TBandHistoryRepository }

  TBandHistoryRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  utils_system, utils_global, models_users, utils_validations, utils_fullnames,
  data_consts, data_columns, data_setparam, data_getvalue,
  utils_locale, udm_main;

{ TBand }

constructor TBand.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TBand.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TBand then
  begin
    FFullName := TBand(Source).FullName;
    FSize := TBand(Source).Size;
    FNumber := TBand(Source).Number;
    FStatus := TBand(Source).Status;
    FSource := TBand(Source).Source;
    FPrefix := TBand(Source).Prefix;
    FSuffix := TBand(Source).Suffix;
    FSupplierId := TBand(Source).SupplierId;
    FBandColor := TBand(Source).BandColor;
    FBandType := TBand(Source).BandType;
    FRequesterId := TBand(Source).RequesterId;
    FCarrierId := TBand(Source).CarrierId;
    FIndividualId := TBand(Source).IndividualId;
    FProjectId := TBand(Source).ProjectId;
    FReported := TBand(Source).Reported;
    FNotes := TBand(Source).Notes;
  end;
end;

procedure TBand.Clear;
begin
  inherited;
  FFullName := EmptyStr;
  FSize := EmptyStr;
  FNumber := 0;
  FStatus := bstAvailable;
  FSource := bscAcquiredFromSupplier;
  FPrefix := EmptyStr;
  FSuffix := EmptyStr;
  FSupplierId := 0;
  FBandColor := EmptyStr;
  FBandType := mkButtEndBand;
  FRequesterId := 0;
  FCarrierId := 0;
  FIndividualId := 0;
  FProjectId := 0;
  FReported := False;
  FNotes := EmptyStr;
end;

function TBand.Clone: TXolmisRecord;
begin
  Result := TBand(inherited Clone);
end;

function TBand.Diff(const aOld: TBand; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSize, aOld.Size, FSize, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNumber, aOld.Number, FNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStatus, aOld.Status, FStatus, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSource, aOld.Source, FSource, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPrefix, aOld.Prefix, FPrefix, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSuffix, aOld.Suffix, FSuffix, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSupplierID, aOld.SupplierId, FSupplierId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscColor, aOld.BandColor, FBandColor, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.BandType, FBandType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRequesterID, aOld.RequesterId, FRequesterId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCarrierID, aOld.CarrierId, FCarrierId, R) then
    Changes.Add(R);
  //if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
  //  Changes.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscReported, aOld.Reported, FReported, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TBand.EqualsTo(const Other: TBand): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TBand.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName     := Obj.Get('full_name', '');
    FSize         := Obj.Get('band_size', '');
    FNumber       := Obj.Get('band_number', 0);
    case Obj.Get('band_status', '') of
      'D': FStatus := bstAvailable;
      'U': FStatus := bstUsed;
      'R': FStatus := bstRemoved;
      'Q': FStatus := bstBroken;
      'P': FStatus := bstLost;
      'T': FStatus := bstTransferred;
    end;
    case Obj.Get('band_source', '') of
      'A': FSource := bscAcquiredFromSupplier;
      'T': FSource := bscTransferBetweenBanders;
      'L': FSource := bscLivingBirdBandedByOthers;
      'D': FSource := bscDeadBirdBandedByOthers;
      'F': FSource := bscFoundLoose;
    end;
    FPrefix       := Obj.Get('prefix', '');
    FSuffix       := Obj.Get('suffix', '');
    FBandColor    := Obj.Get('band_color', '');
    case Obj.Get('band_type', '') of
      'A': FBandType := mkButtEndBand;
      'F': FBandType := mkFlag;
      'N': FBandType := mkCollar;
      'W': FBandType := mkWingTag;
      'T': FBandType := mkTriangularBand;
      'L': FBandType := mkLockOnBand;
      'R': FBandType := mkRivetBand;
      'C': FBandType := mkClosedBand;
      'O': FBandType := mkOther;
    end;
    FSupplierId   := Obj.Get('supplier_id', 0);
    FRequesterId  := Obj.Get('requester_id', 0);
    FCarrierId    := Obj.Get('carrier_id', 0);
    FIndividualId := Obj.Get('individual_id', 0);
    FProjectId    := Obj.Get('project_id', 0);
    FReported     := Obj.Get('reported', False);
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TBand.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('band_size', FSize);
    JSONObject.Add('band_number', FNumber);
    JSONObject.Add('band_status', BAND_STATUSES[FStatus]);
    JSONObject.Add('band_source', BAND_SOURCES[FSource]);
    JSONObject.Add('prefix', FPrefix);
    JSONObject.Add('suffix', FSuffix);
    JSONObject.Add('band_color', FBandColor);
    JSONObject.Add('band_type', MARK_TYPES[FBandType]);
    JSONObject.Add('supplier_id', FSupplierId);
    JSONObject.Add('requester_id', FRequesterId);
    JSONObject.Add('carrier_id', FCarrierId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('reported', FReported);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TBand.ToString: String;
begin
  Result := Format('Band(Id=%d, FullName=%s, Size=%s, Number=%d, Status=%d, Source=%d, Prefix=%s, Suffix=%s, ' +
    'BandColor=%s, BandType=%d, SupplierId=%d, RequesterId=%d, CarrierId=%d, IndividualId=%d, ProjectId=%d, ' +
    'Reported=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FSize, FNumber, FStatus, FSource, FPrefix, FSuffix, FBandColor, Ord(FBandType), FSupplierId,
    FRequesterId, FCarrierId, FIndividualId, FProjectId, BoolToStr(FReported, 'True', 'False'), FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TBand.Validate(out Msg: string): Boolean;
begin
  if FSize = EmptyStr then
  begin
    Msg := 'Size required.';
    Exit(False);
  end;
  if FNumber = 0 then
  begin
    Msg := 'Number required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TBandRepository }

procedure TBandRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBand;
begin
  if not (E is TBand) then
    raise Exception.Create('Delete: Expected TBand');

  R := TBand(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TBandRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_BAND_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_BAND_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_BAND_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TBand) then
    raise Exception.Create('FindBy: Expected TBand');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT ' +
      'band_id, ' +
      'band_size, ' +
      'band_number, ' +
      'band_status, ' +
      'band_type, ' +
      'band_prefix, ' +
      'band_suffix, ' +
      'band_color, ' +
      'band_source, ' +
      'supplier_id, ' +
      'requester_id, ' +
      'carrier_id, ' +
      'individual_id, ' +
      'project_id, ' +
      'band_reported, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM bands');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TBand(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TBandRepository.FindByNumber(const aSize: String; const aNumber: Integer; E: TBand);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT band_id FROM bands');
    Add('WHERE (band_size = :asize)');
    Add('AND (band_number = :anumber)');
    ParamByName('ASIZE').AsString := aSize;
    ParamByName('ANUMBER').AsInteger := aNumber;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TBand) then
    raise Exception.Create('GetById: Expected TBand');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'band_id, ' +
      'band_size, ' +
      'band_number, ' +
      'band_status, ' +
      'band_type, ' +
      'band_prefix, ' +
      'band_suffix, ' +
      'band_color, ' +
      'band_source, ' +
      'supplier_id, ' +
      'requester_id, ' +
      'carrier_id, ' +
      'individual_id, ' +
      'project_id, ' +
      'band_reported, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM bands');
    Add('WHERE band_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TBand(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TBand;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TBand) then
    raise Exception.Create('Hydrate: Expected TBand');

  R := TBand(E);
  with aDataSet do
  begin
    R.Id := FieldByName('band_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.Size := FieldByName('band_size').AsString;
    R.Number := FieldByName('band_number').AsInteger;
    case FieldByName('band_status').AsString of
      'D': R.Status := bstAvailable;
      'U': R.Status := bstUsed;
      'R': R.Status := bstRemoved;
      'Q': R.Status := bstBroken;
      'P': R.Status := bstLost;
      'T': R.Status := bstTransferred;
    end;
    case FieldByName('band_source').AsString of
      'A': R.Source := bscAcquiredFromSupplier;
      'T': R.Source := bscTransferBetweenBanders;
      'L': R.Source := bscLivingBirdBandedByOthers;
      'D': R.Source := bscDeadBirdBandedByOthers;
      'F': R.Source := bscFoundLoose;
    end;
    R.Prefix := FieldByName('band_prefix').AsString;
    R.Suffix := FieldByName('band_suffix').AsString;
    R.SupplierId := FieldByName('supplier_id').AsInteger;
    R.BandColor := FieldByName('band_color').AsString;
    case FieldByName('band_type').AsString of
      'A': R.BandType := mkButtEndBand;
      'F': R.BandType := mkFlag;
      'N': R.BandType := mkCollar;
      'W': R.BandType := mkWingTag;
      'T': R.BandType := mkTriangularBand;
      'L': R.BandType := mkLockOnBand;
      'R': R.BandType := mkRivetBand;
      'C': R.BandType := mkClosedBand;
      'O': R.BandType := mkOther;
    end;
    R.RequesterId := FieldByName('requester_id').AsInteger;
    R.CarrierId := FieldByName('carrier_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.Reported := FieldByName('band_reported').AsBoolean;
    R.Notes := FieldByName('notes').AsString;
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBandRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBand;
begin
  if not (E is TBand) then
    raise Exception.Create('Insert: Expected TBand');

  R := TBand(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO bands (' +
      'band_size, ' +
      'band_number, ' +
      'band_status, ' +
      'band_type, ' +
      'band_prefix, ' +
      'band_suffix, ' +
      'band_color, ' +
      'band_source, ' +
      'supplier_id, ' +
      'requester_id, ' +
      'carrier_id, ' +
      'project_id, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':band_size, ' +
      ':band_number, ' +
      ':band_status, ' +
      ':band_type, ' +
      ':band_prefix, ' +
      ':band_suffix, ' +
      ':band_color, ' +
      ':band_source, ' +
      ':supplier_id, ' +
      ':requester_id, ' +
      ':carrier_id, ' +
      ':project_id, ' +
      ':notes, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));');

    SetStrParam(ParamByName('band_size'), R.Size);
    SetIntParam(ParamByName('band_number'), R.Number);
    SetStrParam(ParamByName('band_status'), BAND_STATUSES[R.Status]);
    SetStrParam(ParamByName('band_type'), MARK_TYPES[R.BandType]);
    SetStrParam(ParamByName('band_prefix'), R.Prefix);
    SetStrParam(ParamByName('band_suffix'), R.Suffix);
    SetStrParam(ParamByName('band_color'), R.BandColor);
    SetStrParam(ParamByName('band_source'), BAND_SOURCES[R.Source]);
    SetForeignParam(ParamByName('supplier_id'), R.SupplierId);
    SetForeignParam(ParamByName('requester_id'), R.RequesterId);
    SetForeignParam(ParamByName('carrier_id'), R.CarrierId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    R.FullName := GetBandFullname(R.Size, R.Number, R.SupplierId);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandRepository.TableName: string;
begin
  Result := TBL_BANDS;
end;

procedure TBandRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBand;
begin
  if not (E is TBand) then
    raise Exception.Create('Update: Expected TBand');

  R := TBand(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TBandRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE bands SET ' +
      'band_size = :band_size, ' +
      'band_number = :band_number, ' +
      'band_status = :band_status, ' +
      'band_type = :band_type, ' +
      'band_prefix = :band_prefix, ' +
      'band_suffix = :band_suffix, ' +
      'band_color = :band_color, ' +
      'band_source = :band_source, ' +
      'supplier_id = :supplier_id, ' +
      'requester_id = :requester_id, ' +
      'carrier_id = :carrier_id, ' +
      'project_id = :project_id, ' +
      'notes = :notes, ' +
      'full_name = :full_name, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (band_id = :band_id)');

    SetStrParam(ParamByName('band_size'), R.Size);
    SetIntParam(ParamByName('band_number'), R.Number);
    SetStrParam(ParamByName('band_status'), BAND_STATUSES[R.Status]);
    SetStrParam(ParamByName('band_type'), MARK_TYPES[R.BandType]);
    SetStrParam(ParamByName('band_prefix'), R.Prefix);
    SetStrParam(ParamByName('band_suffix'), R.Suffix);
    SetStrParam(ParamByName('band_color'), R.BandColor);
    SetStrParam(ParamByName('band_source'), BAND_SOURCES[R.Source]);
    SetForeignParam(ParamByName('supplier_id'), R.SupplierId);
    SetForeignParam(ParamByName('requester_id'), R.RequesterId);
    SetForeignParam(ParamByName('carrier_id'), R.CarrierId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    R.FullName := GetBandFullname(R.Size, R.Number, R.SupplierId);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('band_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TBandHistory }

constructor TBandHistory.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TBandHistory.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TBandHistory then
  begin
    FBandId := TBandHistory(Source).BandId;
    FEventDate := TBandHistory(Source).EventDate;
    FOrderNumber := TBandHistory(Source).OrderNumber;
    FEventType := TBandHistory(Source).EventType;
    FSupplierId := TBandHistory(Source).SupplierId;
    FRequesterId := TBandHistory(Source).RequesterId;
    FSenderId := TBandHistory(Source).SenderId;
    FNotes := TBandHistory(Source).Notes;
  end;
end;

procedure TBandHistory.Clear;
begin
  inherited Clear;
  FBandId := 0;
  FEventDate := NullDate;
  FOrderNumber := 0;
  FEventType := bevUse;
  FSupplierId := 0;
  FRequesterId := 0;
  FSenderId := 0;
  FNotes := EmptyStr;
end;

function TBandHistory.Clone: TXolmisRecord;
begin
  Result := TBandHistory(inherited Clone);
end;

function TBandHistory.Diff(const aOld: TBandHistory; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff('Event type', aOld.EventType, FEventType, R) then
    Changes.Add(R);
  if FieldValuesDiff('Order number', aOld.OrderNumber, FOrderNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff('Event date', aOld.EventDate, FEventDate, R) then
    Changes.Add(R);
  if FieldValuesDiff('Supplier ID', aOld.SupplierId, FSupplierId, R) then
    Changes.Add(R);
  if FieldValuesDiff('Requester ID', aOld.RequesterId, FRequesterId, R) then
    Changes.Add(R);
  if FieldValuesDiff('Sender ID', aOld.SenderId, FSenderId, R) then
    Changes.Add(R);
  if FieldValuesDiff('Notes', aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TBandHistory.EqualsTo(const Other: TBandHistory): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TBandHistory.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FBandId       := Obj.Get('band_id', 0);
    FEventDate    := Obj.Get('event_date', NullDate);
    case Obj.Get('event_type', '') of
      'O': FEventType := bevOrder;
      'C': FEventType := bevReceive;
      'T': FEventType := bevTransfer;
      'R': FEventType := bevRetrieve;
      'P': FEventType := bevReport;
      'U': FEventType := bevUse;
      'D': FEventType := bevDischarge;
    end;
    FSupplierId   := Obj.Get('supplier_id', 0);
    FOrderNumber  := Obj.Get('order_number', 0);
    FRequesterId  := Obj.Get('requester_id', 0);
    FSenderId     := Obj.Get('sender_id', 0);
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TBandHistory.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('band_id', FBandId);
    JSONObject.Add('event_date', FEventDate);
    JSONObject.Add('event_type', BAND_EVENTS[FEventType]);
    JSONObject.Add('supplier_id', FSupplierId);
    JSONObject.Add('order_number', FOrderNumber);
    JSONObject.Add('requester_id', FRequesterId);
    JSONObject.Add('sender_id', FSenderId);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TBandHistory.ToString: String;
begin
  Result := Format('BandHistory(Id=%d, EventDate=%s, EventType=%s, SupplierId=%d, OrderNumber=%d, ' +
    'RequesterId=%d, SenderId=%d, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, DateToStr(FEventDate), BAND_EVENTS[FEventType], FSupplierId, FOrderNumber, FRequesterId, FSenderId,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TBandHistory.Validate(out Msg: string): Boolean;
begin
  if FEventDate = NullDate then
  begin
    Msg := 'EventDate required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TBandHistoryRepository }

procedure TBandHistoryRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBandHistory;
begin
  if not (E is TBandHistory) then
    raise Exception.Create('Delete: Expected TBandHistory');

  R := TBandHistory(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TBandHistory.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_EVENT_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandHistoryRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_EVENT_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistoryRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_EVENT_ID, COL_EVENT_DATE, COL_ORDER_NUMBER); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TBandHistory) then
    raise Exception.Create('FindBy: Expected TBandHistory');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT ' +
      'event_id, ' +
      'band_id, ' +
      'event_type, ' +
      'event_date, ' +
      'order_number, ' +
      'supplier_id, ' +
      'sender_id, ' +
      'requester_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM band_history');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TBandHistory(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TBandHistoryRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TBandHistory) then
    raise Exception.Create('GetById: Expected TBandHistory');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'event_id, ' +
      'band_id, ' +
      'event_type, ' +
      'event_date, ' +
      'order_number, ' +
      'supplier_id, ' +
      'sender_id, ' +
      'requester_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM band_history');
    Add('WHERE event_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TBandHistory(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistoryRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TBandHistory;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TBandHistory) then
    raise Exception.Create('Hydrate: Expected TBandHistory');

  R := TBandHistory(E);
  with aDataSet do
  begin
    R.Id := FieldByName('event_id').AsInteger;
    R.BandId := FieldByName('band_id').AsInteger;
    R.EventDate := FieldByName('event_date').AsDateTime;
    R.OrderNumber := FieldByName('order_number').AsInteger;
    case FieldByName('event_type').AsString of
      'O': R.EventType := bevOrder;
      'C': R.EventType := bevReceive;
      'T': R.EventType := bevTransfer;
      'R': R.EventType := bevRetrieve;
      'P': R.EventType := bevReport;
      'U': R.EventType := bevUse;
      'D': R.EventType := bevDischarge;
    end;
    R.SupplierId := FieldByName('supplier_id').AsInteger;
    R.RequesterId := FieldByName('requester_id').AsInteger;
    R.SenderId := FieldByName('sender_id').AsInteger;
    R.Notes := FieldByName('notes').AsString;
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBandHistoryRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBandHistory;
begin
  if not (E is TBandHistory) then
    raise Exception.Create('Insert: Expected TBandHistory');

  R := TBandHistory(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO band_history (' +
      'band_id, ' +
      'event_date, ' +
      'notes, ' +
      'event_type, ' +
      'supplier_id, ' +
      'order_number, ' +
      'requester_id, ' +
      'sender_id, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':band_id, ' +
      'date(:event_date), ' +
      ':notes, ' +
      ':event_type, ' +
      ':supplier_id, ' +
      ':order_number, ' +
      ':requester_id, ' +
      ':sender_id, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');
    ParamByName('band_id').AsInteger := R.BandId;
    SetDateParam(ParamByName('event_date'), R.EventDate);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('event_type'), BAND_EVENTS[R.EventType]);
    SetForeignParam(ParamByName('supplier_id'), R.SupplierId);
    SetIntParam(ParamByName('order_number'), R.OrderNumber);
    SetForeignParam(ParamByName('requester_id'), R.RequesterId);
    SetForeignParam(ParamByName('sender_id'), R.SenderId);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandHistoryRepository.TableName: string;
begin
  Result := TBL_BAND_HISTORY;
end;

procedure TBandHistoryRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TBandHistory;
begin
  if not (E is TBandHistory) then
    raise Exception.Create('Update: Expected TBandHistory');

  R := TBandHistory(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TBandHistory.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE band_history SET ' +
      'band_id = :band_id, ' +
      'event_date = date(:event_date), ' +
      'notes = :notes, ' +
      'event_type = :event_type, ' +
      'supplier_id = :supplier_id, ' +
      'order_number = :order_number, ' +
      'requester_id = :requester_id, ' +
      'sender_id = :sender_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (event_id = :event_id)');
    ParamByName('band_id').AsInteger := R.BandId;
    SetDateParam(ParamByName('event_date'), R.EventDate);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('event_type'), BAND_EVENTS[R.EventType]);
    SetForeignParam(ParamByName('supplier_id'), R.SupplierId);
    SetIntParam(ParamByName('order_number'), R.OrderNumber);
    SetForeignParam(ParamByName('requester_id'), R.RequesterId);
    SetForeignParam(ParamByName('sender_id'), R.SenderId);
    //ParamByName('marked_status').AsBoolean := FMarked;
    //ParamByName('active_status').AsBoolean := FActive;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ParamByName('event_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

