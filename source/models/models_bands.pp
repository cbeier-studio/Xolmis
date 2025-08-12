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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TBand; out Changes: TStrings): Boolean;
    function Equals(const Other: TBand): Boolean;
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

  TBandRepository = class
  private
    FConn: TSQLConnection;
    FTrans: TSQLTransaction;
    function NewQuery: TSQLQuery;
    function TableName: string; inline;
    procedure Hydrate(aDataSet: TDataSet; E: TBand);
  public
    constructor Create(AConn: TSQLConnection);

    function GetById(const Id: Integer): TBand;
    function Exists(const Id: Integer): Boolean;
    function FindBy(const FieldName: String; const Value: Variant): TBand;
    function FindByNumber(const aSize: String; const aNumber: Integer): TBand;
    procedure Insert(E: TBand);
    procedure Update(E: TBand);
    procedure Save(E: TBand);
    procedure Delete(E: TBand);
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TBandHistory; out Changes: TStrings): Boolean;
    function Equals(const Other: TBandHistory): Boolean;
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

  TBandHistoryRepository = class
  private
    FConn: TSQLConnection;
    FTrans: TSQLTransaction;
    function NewQuery: TSQLQuery;
    function TableName: string; inline;
    procedure Hydrate(aDataSet: TDataSet; E: TBandHistory);
  public
    constructor Create(AConn: TSQLConnection);

    function GetById(const Id: Integer): TBandHistory;
    function Exists(const Id: Integer): Boolean;
    function FindBy(const FieldName: String; const Value: Variant): TBandHistory;
    procedure Insert(E: TBandHistory);
    procedure Update(E: TBandHistory);
    procedure Save(E: TBandHistory);
    procedure Delete(E: TBandHistory);
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

function TBand.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Size', FSize);
    JSONObject.Add('Number', FNumber);
    JSONObject.Add('Status', BAND_STATUSES[FStatus]);
    JSONObject.Add('Source', BAND_SOURCES[FSource]);
    JSONObject.Add('Prefix', FPrefix);
    JSONObject.Add('Suffix', FSuffix);
    JSONObject.Add('Color', FBandColor);
    JSONObject.Add('Type', MARK_TYPES[FBandType]);
    JSONObject.Add('Supplier', FSupplierId);
    JSONObject.Add('Requester', FRequesterId);
    JSONObject.Add('Carrier', FCarrierId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Reported', FReported);
    JSONObject.Add('Notes', FNotes);

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
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Acitve=%s)',
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

function TBand.Diff(const aOld: TBand; out Changes: TStrings): Boolean;
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

function TBand.Equals(const Other: TBand): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TBand.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName     := Obj.Get('Name', '');
    FSize         := Obj.Get('Size', '');
    FNumber       := Obj.Get('Number', 0);
    case Obj.Get('Status', '') of
      'D': FStatus := bstAvailable;
      'U': FStatus := bstUsed;
      'R': FStatus := bstRemoved;
      'Q': FStatus := bstBroken;
      'P': FStatus := bstLost;
      'T': FStatus := bstTransferred;
    end;
    case Obj.Get('Source', '') of
      'A': FSource := bscAcquiredFromSupplier;
      'T': FSource := bscTransferBetweenBanders;
      'L': FSource := bscLivingBirdBandedByOthers;
      'D': FSource := bscDeadBirdBandedByOthers;
      'F': FSource := bscFoundLoose;
    end;
    FPrefix       := Obj.Get('Prefix', '');
    FSuffix       := Obj.Get('Suffix', '');
    FBandColor    := Obj.Get('Color', '');
    case Obj.Get('Type', '') of
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
    FSupplierId   := Obj.Get('Supplier', 0);
    FRequesterId  := Obj.Get('Requester', 0);
    FCarrierId    := Obj.Get('Carrier', 0);
    FIndividualId := Obj.Get('Individual', 0);
    FProjectId    := Obj.Get('Project', 0);
    FReported     := Obj.Get('Reported', False);
    FNotes        := Obj.Get('Notes', '');
  finally
    Obj.Free;
  end;
end;

{ TBandRepository }

constructor TBandRepository.Create(AConn: TSQLConnection);
begin
  inherited Create;
  if AConn = nil then
    raise Exception.Create(rsRepositoryConnectionCannotBeNil);
  FConn := AConn;
  FTrans := AConn.Transaction;
end;

procedure TBandRepository.Delete(E: TBand);
var
  Qry: TSQLQuery;
begin
  if E.Id = 0 then
    raise Exception.CreateFmt('TBand.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM bands');
      Add('WHERE (band_id = :aid)');

      ParamByName('aid').AsInteger := E.Id;

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
    SQL.Text := 'select 1 as x from %tablename where id=:id limit 1';
    MacroByName('tablename').Value := TableName;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandRepository.FindBy(const FieldName: String; const Value: Variant): TBand;
const
  ALLOWED: array[0..1] of string = (COL_BAND_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  Result := nil;
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
    //SQLConnection := DMM.sqlCon;
    //SQLTransaction := DMM.sqlTrans;
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
      Result := TBand.Create;
      Hydrate(Qry, Result);
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

function TBandRepository.FindByNumber(const aSize: String; const aNumber: Integer): TBand;
var
  Qry: TSQLQuery;
begin
  Result := nil;

  Qry := NewQuery;
  with Qry, SQL do
  try
    //Database := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT band_id FROM bands');
    Add('WHERE (band_size = :asize)');
    Add('AND (band_number = :anumber)');
    ParamByName('ASIZE').AsString := aSize;
    ParamByName('ANUMBER').AsInteger := aNumber;
    Open;
    if not EOF then
    begin
      Result := TBand.Create;
      Hydrate(Qry, Result);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandRepository.GetById(const Id: Integer): TBand;
var
  Qry: TSQLQuery;
begin
  Result := nil;

  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
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
      Result := TBand.Create;
      Hydrate(Qry, Result);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandRepository.Hydrate(aDataSet: TDataSet; E: TBand);
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;

  with aDataSet do
  begin
    E.Id := FieldByName('band_id').AsInteger;
    E.FullName := FieldByName('full_name').AsString;
    E.Size := FieldByName('band_size').AsString;
    E.Number := FieldByName('band_number').AsInteger;
    case FieldByName('band_status').AsString of
      'D': E.Status := bstAvailable;
      'U': E.Status := bstUsed;
      'R': E.Status := bstRemoved;
      'Q': E.Status := bstBroken;
      'P': E.Status := bstLost;
      'T': E.Status := bstTransferred;
    end;
    case FieldByName('band_source').AsString of
      'A': E.Source := bscAcquiredFromSupplier;
      'T': E.Source := bscTransferBetweenBanders;
      'L': E.Source := bscLivingBirdBandedByOthers;
      'D': E.Source := bscDeadBirdBandedByOthers;
      'F': E.Source := bscFoundLoose;
    end;
    E.Prefix := FieldByName('band_prefix').AsString;
    E.Suffix := FieldByName('band_suffix').AsString;
    E.SupplierId := FieldByName('supplier_id').AsInteger;
    E.BandColor := FieldByName('band_color').AsString;
    case FieldByName('band_type').AsString of
      'A': E.BandType := mkButtEndBand;
      'F': E.BandType := mkFlag;
      'N': E.BandType := mkCollar;
      'W': E.BandType := mkWingTag;
      'T': E.BandType := mkTriangularBand;
      'L': E.BandType := mkLockOnBand;
      'R': E.BandType := mkRivetBand;
      'C': E.BandType := mkClosedBand;
      'O': E.BandType := mkOther;
    end;
    E.RequesterId := FieldByName('requester_id').AsInteger;
    E.CarrierId := FieldByName('carrier_id').AsInteger;
    E.IndividualId := FieldByName('individual_id').AsInteger;
    E.ProjectId := FieldByName('project_id').AsInteger;
    E.Reported := FieldByName('band_reported').AsBoolean;
    E.Notes := FieldByName('notes').AsString;
    E.UserInserted := FieldByName('user_inserted').AsInteger;
    E.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), E.InsertDate);
    GetTimeStamp(FieldByName('update_date'), E.UpdateDate);
    E.Exported := FieldByName('exported_status').AsBoolean;
    E.Marked := FieldByName('marked_status').AsBoolean;
    E.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBandRepository.Insert(E: TBand);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

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

      SetStrParam(ParamByName('band_size'), E.Size);
      SetIntParam(ParamByName('band_number'), E.Number);
      SetStrParam(ParamByName('band_status'), BAND_STATUSES[E.Status]);
      SetStrParam(ParamByName('band_type'), MARK_TYPES[E.BandType]);
      SetStrParam(ParamByName('band_prefix'), E.Prefix);
      SetStrParam(ParamByName('band_suffix'), E.Suffix);
      SetStrParam(ParamByName('band_color'), E.BandColor);
      SetStrParam(ParamByName('band_source'), BAND_SOURCES[E.Source]);
      SetForeignParam(ParamByName('supplier_id'), E.SupplierId);
      SetForeignParam(ParamByName('requester_id'), E.RequesterId);
      SetForeignParam(ParamByName('carrier_id'), E.CarrierId);
      SetForeignParam(ParamByName('project_id'), E.ProjectId);
      E.FullName := GetBandFullname(E.Size, E.Number, E.SupplierId);
      SetStrParam(ParamByName('full_name'), E.FullName);
      SetStrParam(ParamByName('notes'), E.Notes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    E.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandRepository.NewQuery: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.DataBase := FConn;
  Result.Transaction := FTrans;
end;

procedure TBandRepository.Save(E: TBand);
begin
  if E = nil then
    Exit;

  if E.IsNew then
    Insert(E)
  else
  begin
    if Exists(E.Id) then
      Update(E)
    else
      Insert(E);
  end;
end;

function TBandRepository.TableName: string;
begin
  Result := TBL_BANDS;
end;

procedure TBandRepository.Update(E: TBand);
var
  Qry: TSQLQuery;
begin
  if E.Id = 0 then
    raise Exception.CreateFmt('TBand.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  //Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

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

    SetStrParam(ParamByName('band_size'), E.Size);
    SetIntParam(ParamByName('band_number'), E.Number);
    SetStrParam(ParamByName('band_status'), BAND_STATUSES[E.Status]);
    SetStrParam(ParamByName('band_type'), MARK_TYPES[E.BandType]);
    SetStrParam(ParamByName('band_prefix'), E.Prefix);
    SetStrParam(ParamByName('band_suffix'), E.Suffix);
    SetStrParam(ParamByName('band_color'), E.BandColor);
    SetStrParam(ParamByName('band_source'), BAND_SOURCES[E.Source]);
    SetForeignParam(ParamByName('supplier_id'), E.SupplierId);
    SetForeignParam(ParamByName('requester_id'), E.RequesterId);
    SetForeignParam(ParamByName('carrier_id'), E.CarrierId);
    SetForeignParam(ParamByName('project_id'), E.ProjectId);
    E.FullName := GetBandFullname(E.Size, E.Number, E.SupplierId);
    SetStrParam(ParamByName('full_name'), E.FullName);
    SetStrParam(ParamByName('notes'), E.Notes);
    ParamByName('marked_status').AsBoolean := E.Marked;
    ParamByName('active_status').AsBoolean := E.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('band_id').AsInteger := E.Id;

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

function TBandHistory.Diff(const aOld: TBandHistory; out Changes: TStrings): Boolean;
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

function TBandHistory.Equals(const Other: TBandHistory): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TBandHistory.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FBandId       := Obj.Get('Band', 0);
    FEventDate    := Obj.Get('Date', NullDate);
    case Obj.Get('Type', '') of
      'O': FEventType := bevOrder;
      'C': FEventType := bevReceive;
      'T': FEventType := bevTransfer;
      'R': FEventType := bevRetrieve;
      'P': FEventType := bevReport;
      'U': FEventType := bevUse;
      'D': FEventType := bevDischarge;
    end;
    FSupplierId   := Obj.Get('Supplier', 0);
    FOrderNumber  := Obj.Get('Order number', 0);
    FRequesterId  := Obj.Get('Requester', 0);
    FSenderId     := Obj.Get('Sender', 0);
    FNotes        := Obj.Get('Notes', '');
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
    JSONObject.Add('Band', FBandId);
    JSONObject.Add('Date', FEventDate);
    JSONObject.Add('Type', BAND_EVENTS[FEventType]);
    JSONObject.Add('Supplier', FSupplierId);
    JSONObject.Add('Order number', FOrderNumber);
    JSONObject.Add('Requester', FRequesterId);
    JSONObject.Add('Sender', FSenderId);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TBandHistory.ToString: String;
begin
  Result := Format('BandHistory(Id=%d, EventDate=%s, EventType=%s, SupplierId=%d, OrderNumber=%d, ' +
    'RequesterId=%d, SenderId=%d, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Acitve=%s)',
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

constructor TBandHistoryRepository.Create(AConn: TSQLConnection);
begin
  inherited Create;
  if AConn = nil then
    raise Exception.Create(rsRepositoryConnectionCannotBeNil);
  FConn := AConn;
  FTrans := AConn.Transaction;
end;

procedure TBandHistoryRepository.Delete(E: TBandHistory);
var
  Qry: TSQLQuery;
begin
  if E.Id = 0 then
    raise Exception.CreateFmt('TBandHistory.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM band_history');
      Add('WHERE (event_id = :aid)');

      ParamByName('aid').AsInteger := E.Id;

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
    SQL.Text := 'select 1 as x from %tablename where id=:id limit 1';
    MacroByName('tablename').Value := TableName;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandHistoryRepository.FindBy(const FieldName: String; const Value: Variant): TBandHistory;
const
  ALLOWED: array[0..2] of string = (COL_EVENT_ID, COL_EVENT_DATE, COL_ORDER_NUMBER); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  Result := nil;
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
    //SQLConnection := DMM.sqlCon;
    //SQLTransaction := DMM.sqlTrans;
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
      Result := TBandHistory.Create;
      Hydrate(Qry, Result);
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

function TBandHistoryRepository.GetById(const Id: Integer): TBandHistory;
var
  Qry: TSQLQuery;
begin
  Result := nil;

  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
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
      Result := TBandHistory.Create;
      Hydrate(Qry, Result);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistoryRepository.Hydrate(aDataSet: TDataSet; E: TBandHistory);
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;

  with aDataSet do
  begin
    E.Id := FieldByName('event_id').AsInteger;
    E.BandId := FieldByName('band_id').AsInteger;
    E.EventDate := FieldByName('event_date').AsDateTime;
    E.OrderNumber := FieldByName('order_number').AsInteger;
    case FieldByName('event_type').AsString of
      'O': E.EventType := bevOrder;
      'C': E.EventType := bevReceive;
      'T': E.EventType := bevTransfer;
      'R': E.EventType := bevRetrieve;
      'P': E.EventType := bevReport;
      'U': E.EventType := bevUse;
      'D': E.EventType := bevDischarge;
    end;
    E.SupplierId := FieldByName('supplier_id').AsInteger;
    E.RequesterId := FieldByName('requester_id').AsInteger;
    E.SenderId := FieldByName('sender_id').AsInteger;
    E.Notes := FieldByName('notes').AsString;
    E.UserInserted := FieldByName('user_inserted').AsInteger;
    E.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), E.InsertDate);
    GetTimeStamp(FieldByName('update_date'), E.UpdateDate);
    E.Exported := FieldByName('exported_status').AsBoolean;
    E.Marked := FieldByName('marked_status').AsBoolean;
    E.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBandHistoryRepository.Insert(E: TBandHistory);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

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
    ParamByName('band_id').AsInteger := E.BandId;
    SetDateParam(ParamByName('event_date'), E.EventDate);
    SetStrParam(ParamByName('notes'), E.Notes);
    SetStrParam(ParamByName('event_type'), BAND_EVENTS[E.EventType]);
    SetForeignParam(ParamByName('supplier_id'), E.SupplierId);
    SetIntParam(ParamByName('order_number'), E.OrderNumber);
    SetForeignParam(ParamByName('requester_id'), E.RequesterId);
    SetForeignParam(ParamByName('sender_id'), E.SenderId);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    E.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandHistoryRepository.NewQuery: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.DataBase := FConn;
  Result.Transaction := FTrans;
end;

procedure TBandHistoryRepository.Save(E: TBandHistory);
begin
  if E = nil then
    Exit;

  if E.IsNew then
    Insert(E)
  else
  begin
    if Exists(E.Id) then
      Update(E)
    else
      Insert(E);
  end;
end;

function TBandHistoryRepository.TableName: string;
begin
  Result := TBL_BAND_HISTORY;
end;

procedure TBandHistoryRepository.Update(E: TBandHistory);
var
  Qry: TSQLQuery;
begin
  if E.Id = 0 then
    raise Exception.CreateFmt('TBandHistory.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  //Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    //DataBase := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;

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
    ParamByName('band_id').AsInteger := E.BandId;
    SetDateParam(ParamByName('event_date'), E.EventDate);
    SetStrParam(ParamByName('notes'), E.Notes);
    SetStrParam(ParamByName('event_type'), BAND_EVENTS[E.EventType]);
    SetForeignParam(ParamByName('supplier_id'), E.SupplierId);
    SetIntParam(ParamByName('order_number'), E.OrderNumber);
    SetForeignParam(ParamByName('requester_id'), E.RequesterId);
    SetForeignParam(ParamByName('sender_id'), E.SenderId);
    //ParamByName('marked_status').AsBoolean := FMarked;
    //ParamByName('active_status').AsBoolean := FActive;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ParamByName('event_id').AsInteger := E.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

