{ Xolmis Permits models

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

unit models_permits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fpjson, DateUtils, TypInfo, DB, SQLDB,
  models_record_types;

type

  { TPermit }

  TPermit = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FName: String;
    FNumber: String;
    FPermitType: String;
    FDispatcher: String;
    FDispatchDate: TDate;
    FExpireDate: TDate;
    FFileName: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPermit; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPermit);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Name: String read FName write FName;
    property Number: String read FNumber write FNumber;
    property PermitType: String read FPermitType write FPermitType;
    property Dispatcher: String read FDispatcher write FDispatcher;
    property DispatchDate: TDate read FDispatchDate write FDispatchDate;
    property ExpireDate: TDate read FExpireDate write FExpireDate;
    property FileName: String read FFileName write FFileName;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  utils_locale, models_users, utils_global, utils_validations, data_columns, data_setparam, udm_main;

{ TPermit }

function TPermit.Diff(aOld: TPermit; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNumber, aOld.Number, FNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.PermitType, FPermitType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDispatcher, aOld.Dispatcher, FDispatcher, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDispatchDate, aOld.DispatchDate, FDispatchDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExpireDate, aOld.ExpireDate, FExpireDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFileName, aOld.FileName, FFileName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPermit.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
      'permit_id, ' +
      'project_id, ' +
      'permit_name, ' +
      'permit_number, ' +
      'permit_type, ' +
      'dispatcher_name, ' +
      'dispatch_date, ' +
      'expire_date, ' +
      'notes, ' +
      'permit_filename, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM legal');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

constructor TPermit.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TPermit.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FName := EmptyStr;
  FNumber := EmptyStr;
  FPermitType := EmptyStr;
  FDispatcher := EmptyStr;
  FDispatchDate := NullDate;
  FExpireDate := NullDate;
  FFileName := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TPermit.Copy(aFrom: TPermit);
begin
  FProjectId := aFrom.ProjectId;
  FName := aFrom.Name;
  FNumber := aFrom.Number;
  FPermitType := aFrom.PermitType;
  FDispatcher := aFrom.Dispatcher;
  FDispatchDate := aFrom.DispatchDate;
  FExpireDate := aFrom.ExpireDate;
  FFileName := aFrom.FileName;
  FNotes := aFrom.Notes;
end;

procedure TPermit.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermit.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM legal');
      Add('WHERE (permit_id = :aid)');

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

procedure TPermit.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'permit_id, ' +
      'project_id, ' +
      'permit_name, ' +
      'permit_number, ' +
      'permit_type, ' +
      'dispatcher_name, ' +
      'dispatch_date, ' +
      'expire_date, ' +
      'notes, ' +
      'permit_filename, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM legal');
    Add('WHERE permit_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermit.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('permit_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FName := FieldByName('permit_name').AsString;
    FNumber := FieldByName('permit_number').AsString;
    FPermitType := FieldByName('permit_type').AsString;
    FDispatcher := FieldByName('dispatcher_name').AsString;
    if not (FieldByName('dispatch_date').IsNull) then
      FDispatchDate := FieldByName('dispatch_date').AsDateTime;
    if not (FieldByName('expire_date').IsNull) then
      FExpireDate := FieldByName('expire_date').AsDateTime;
    FFileName := FieldByName('permit_filename').AsString;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TPermit.Insert;
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
      Add('INSERT INTO legal (' +
        'project_id, ' +
        'permit_name, ' +
        'permit_number, ' +
        'permit_type, ' +
        'dispatcher_name, ' +
        'dispatch_date, ' +
        'expire_date, ' +
        'notes, ' +
        'permit_filename, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':project_id, ' +
        ':permit_name, ' +
        ':permit_number, ' +
        ':permit_type, ' +
        ':dispatcher_name, ' +
        'date(:dispatch_date), ' +
        'date(:expire_date), ' +
        ':notes, ' +
        ':permit_filename, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetStrParam(ParamByName('permit_name'), FName);
      SetStrParam(ParamByName('permit_number'), FNumber);
      SetStrParam(ParamByName('permit_type'), FPermitType);
      SetStrParam(ParamByName('dispatcher_name'), FDispatcher);
      SetDateParam(ParamByName('dispatch_date'), FDispatchDate);
      SetDateParam(ParamByName('expire_date'), FExpireDate);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('permit_filename'), FFileName);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
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

procedure TPermit.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPermit.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Name', FName);
    JSONObject.Add('Number', FNumber);
    JSONObject.Add('Type', FPermitType);
    JSONObject.Add('Dispatcher', FDispatcher);
    JSONObject.Add('Dispatch date', FDispatchDate);
    JSONObject.Add('Expire date', FExpireDate);
    JSONObject.Add('Filename', FFileName);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPermit.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermit.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE legal SET ' +
        'project_id = :project_id, ' +
        'permit_name = :permit_name, ' +
        'permit_number = :permit_number, ' +
        'permit_type = :permit_type, ' +
        'dispatcher_name = :dispatcher_name, ' +
        'dispatch_date = date(:dispatch_date), ' +
        'expire_date = date(:expire_date), ' +
        'notes = :notes, ' +
        'permit_filename = :permit_filename, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (permit_id = :permit_id)');

      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetStrParam(ParamByName('permit_name'), FName);
      SetStrParam(ParamByName('permit_number'), FNumber);
      SetStrParam(ParamByName('permit_type'), FPermitType);
      SetStrParam(ParamByName('dispatcher_name'), FDispatcher);
      SetDateParam(ParamByName('dispatch_date'), FDispatchDate);
      SetDateParam(ParamByName('expire_date'), FExpireDate);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('permit_filename'), FFileName);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('permit_id').AsInteger := FId;

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

