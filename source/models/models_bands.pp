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
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aSize: String; aNumber: Integer): Boolean;
    function Diff(aOld: TBand; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TBand);
    function ToJSON: String;
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
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TBandHistory; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TBandHistory);
    function ToJSON: String;
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

implementation

uses
  utils_system, utils_global, models_users, utils_validations, utils_fullnames, data_columns, data_setparam, data_getvalue,
  utils_locale, udm_main;

{ TBand }

constructor TBand.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TBand.Copy(aFrom: TBand);
begin
  FFullName := aFrom.FullName;
  FSize := aFrom.Size;
  FNumber := aFrom.Number;
  FStatus := aFrom.Status;
  FSource := aFrom.Source;
  FPrefix := aFrom.Prefix;
  FSuffix := aFrom.Suffix;
  FSupplierId := aFrom.SupplierId;
  FBandColor := aFrom.BandColor;
  FBandType := aFrom.BandType;
  FRequesterId := aFrom.RequesterId;
  FCarrierId := aFrom.CarrierId;
  FIndividualId := aFrom.IndividualId;
  FProjectId := aFrom.ProjectId;
  FReported := aFrom.Reported;
  FNotes := aFrom.Notes;
end;

procedure TBand.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBand.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM bands');
      Add('WHERE (band_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBand.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBand.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('band_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FSize := FieldByName('band_size').AsString;
    FNumber := FieldByName('band_number').AsInteger;
    case FieldByName('band_status').AsString of
      'D': FStatus := bstAvailable;
      'U': FStatus := bstUsed;
      'R': FStatus := bstRemoved;
      'Q': FStatus := bstBroken;
      'P': FStatus := bstLost;
      'T': FStatus := bstTransfered;
    end;
    case FieldByName('band_source').AsString of
      'A': FSource := bscAcquiredFromSupplier;
      'T': FSource := bscTransferBetweenBanders;
      'L': FSource := bscLivingBirdBandedByOthers;
      'D': FSource := bscDeadBirdBandedByOthers;
      'F': FSource := bscFoundLoose;
    end;
    FPrefix := FieldByName('band_prefix').AsString;
    FSuffix := FieldByName('band_suffix').AsString;
    FSupplierId := FieldByName('supplier_id').AsInteger;
    FBandColor := FieldByName('band_color').AsString;
    case FieldByName('band_type').AsString of
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
    FRequesterId := FieldByName('requester_id').AsInteger;
    FCarrierId := FieldByName('carrier_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FReported := FieldByName('band_reported').AsBoolean;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), FInsertDate);
    GetTimeStamp(FieldByName('update_date'), FUpdateDate);
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBand.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetStrParam(ParamByName('band_size'), FSize);
      SetIntParam(ParamByName('band_number'), FNumber);
      SetStrParam(ParamByName('band_status'), BAND_STATUSES[FStatus]);
      SetStrParam(ParamByName('band_type'), MARK_TYPES[FBandType]);
      SetStrParam(ParamByName('band_prefix'), FPrefix);
      SetStrParam(ParamByName('band_suffix'), FSuffix);
      SetStrParam(ParamByName('band_color'), FBandColor);
      SetStrParam(ParamByName('band_source'), BAND_SOURCES[FSource]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('carrier_id'), FCarrierId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      FFullName := GetBandFullname(FSize, FNumber, FSupplierId);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBand.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
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

procedure TBand.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBand.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetStrParam(ParamByName('band_size'), FSize);
      SetIntParam(ParamByName('band_number'), FNumber);
      SetStrParam(ParamByName('band_status'), BAND_STATUSES[FStatus]);
      SetStrParam(ParamByName('band_type'), MARK_TYPES[FBandType]);
      SetStrParam(ParamByName('band_prefix'), FPrefix);
      SetStrParam(ParamByName('band_suffix'), FSuffix);
      SetStrParam(ParamByName('band_color'), FBandColor);
      SetStrParam(ParamByName('band_source'), BAND_SOURCES[FSource]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('carrier_id'), FCarrierId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      FFullName := GetBandFullname(FSize, FNumber, FSupplierId);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('band_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBand.Find(aSize: String; aNumber: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT band_id FROM bands');
    Add('WHERE (band_size = :asize)');
    Add('AND (band_number = :anumber)');
    ParamByName('ASIZE').AsString := aSize;
    ParamByName('ANUMBER').AsInteger := aNumber;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('band_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBand.Diff(aOld: TBand; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSize, aOld.Size, FSize, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNumber, aOld.Number, FNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStatus, aOld.Status, FStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSource, aOld.Source, FSource, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPrefix, aOld.Prefix, FPrefix, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSuffix, aOld.Suffix, FSuffix, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupplierID, aOld.SupplierId, FSupplierId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscColor, aOld.BandColor, FBandColor, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.BandType, FBandType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRequesterID, aOld.RequesterId, FRequesterId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCarrierID, aOld.CarrierId, FCarrierId, R) then
    aList.Add(R);
  //if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscReported, aOld.Reported, FReported, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TBandHistory }

constructor TBandHistory.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
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

procedure TBandHistory.Copy(aFrom: TBandHistory);
begin
  FBandId := aFrom.BandId;
  FEventDate := aFrom.EventDate;
  FOrderNumber := aFrom.OrderNumber;
  FEventType := aFrom.EventType;
  FSupplierId := aFrom.SupplierId;
  FRequesterId := aFrom.RequesterId;
  FSenderId := aFrom.SenderId;
  FNotes := aFrom.Notes;
end;

procedure TBandHistory.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBandHistory.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM band_history');
      Add('WHERE (event_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistory.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandHistory.Diff(aOld: TBandHistory; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Event type', aOld.EventType, FEventType, R) then
    aList.Add(R);
  if FieldValuesDiff('Order number', aOld.OrderNumber, FOrderNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Event date', aOld.EventDate, FEventDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Supplier ID', aOld.SupplierId, FSupplierId, R) then
    aList.Add(R);
  if FieldValuesDiff('Requester ID', aOld.RequesterId, FRequesterId, R) then
    aList.Add(R);
  if FieldValuesDiff('Sender ID', aOld.SenderId, FSenderId, R) then
    aList.Add(R);
  if FieldValuesDiff('Notes', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

procedure TBandHistory.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      ParamByName('band_id').AsInteger := FBandId;
      SetDateParam(ParamByName('event_date'), FEventDate);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('event_type'), BAND_EVENTS[FEventType]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetIntParam(ParamByName('order_number'), FOrderNumber);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('sender_id'), FSenderId);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistory.LoadFromDataSet(aDataSet: TDataSet);
//var
//  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('event_id').AsInteger;
    FBandId := FieldByName('band_id').AsInteger;
    FEventDate := FieldByName('event_date').AsDateTime;
    FOrderNumber := FieldByName('order_number').AsInteger;
    case FieldByName('event_type').AsString of
      'O': FEventType := bevOrder;
      'C': FEventType := bevReceive;
      'T': FEventType := bevTransfer;
      'R': FEventType := bevRetrieve;
      'P': FEventType := bevReport;
      'U': FEventType := bevUse;
      'D': FEventType := bevDischarge;
    end;
    FSupplierId := FieldByName('supplier_id').AsInteger;
    FRequesterId := FieldByName('requester_id').AsInteger;
    FSenderId := FieldByName('sender_id').AsInteger;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), FInsertDate);
    GetTimeStamp(FieldByName('update_date'), FUpdateDate);
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TBandHistory.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TBandHistory.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Band', FBandId);
    JSONObject.Add('Date', FEventDate);
    JSONObject.Add('Notes', FNotes);
    JSONObject.Add('Type', BAND_EVENTS[FEventType]);
    JSONObject.Add('Supplier', FSupplierId);
    JSONObject.Add('Order number', FOrderNumber);
    JSONObject.Add('Requester', FRequesterId);
    JSONObject.Add('Sender', FSenderId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TBandHistory.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBandHistory.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      ParamByName('band_id').AsInteger := FBandId;
      SetDateParam(ParamByName('event_date'), FEventDate);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('event_type'), BAND_EVENTS[FEventType]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetIntParam(ParamByName('order_number'), FOrderNumber);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('sender_id'), FSenderId);
      //ParamByName('marked_status').AsBoolean := FMarked;
      //ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;
      ParamByName('event_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

