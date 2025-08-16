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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TPermit; out Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TPermit): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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

  { TPermitRepository }

  TPermitRepository = class(TXolmisRepository)
  private
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
  utils_locale, models_users, utils_global, utils_validations, data_consts, data_columns, data_setparam,
  data_getvalue, udm_main;

{ TPermit }

constructor TPermit.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TPermit.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TPermit then
  begin
    FProjectId := TPermit(Source).ProjectId;
    FName := TPermit(Source).Name;
    FNumber := TPermit(Source).Number;
    FPermitType := TPermit(Source).PermitType;
    FDispatcher := TPermit(Source).Dispatcher;
    FDispatchDate := TPermit(Source).DispatchDate;
    FExpireDate := TPermit(Source).ExpireDate;
    FFileName := TPermit(Source).FileName;
    FNotes := TPermit(Source).Notes;
  end;
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

function TPermit.Clone: TXolmisRecord;
begin
  Result := TPermit(inherited Clone);
end;

function TPermit.Diff(const aOld: TPermit; out Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNumber, aOld.Number, FNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.PermitType, FPermitType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDispatcher, aOld.Dispatcher, FDispatcher, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDispatchDate, aOld.DispatchDate, FDispatchDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExpireDate, aOld.ExpireDate, FExpireDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFileName, aOld.FileName, FFileName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TPermit.EqualsTo(const Other: TPermit): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TPermit.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FProjectId    := Obj.Get('project_id', 0);
    FName         := Obj.Get('permit_name', '');
    FNumber       := Obj.Get('permit_number', '');
    FPermitType   := Obj.Get('permit_type', '');
    FDispatcher   := Obj.Get('dispatcher', '');
    FDispatchDate := StrToDate(Obj.Get('dispatch_date', NULL_DATE_STR));
    FExpireDate   := StrToDate(Obj.Get('expire_date', NULL_DATE_STR));
    FFileName     := Obj.Get('filename', '');
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TPermit.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('permit_name', FName);
    JSONObject.Add('permit_number', FNumber);
    JSONObject.Add('permit_type', FPermitType);
    JSONObject.Add('dispatcher', FDispatcher);
    JSONObject.Add('dispatch_date', FDispatchDate);
    JSONObject.Add('expire_date', FExpireDate);
    JSONObject.Add('filename', FFileName);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TPermit.ToString: String;
begin
  Result := Format('Permit(Id=%d, Name=%s, Number=%s, PermitType=%s, Dispatcher=%s, DispatchDate=%s, ' +
    'ExpireDate=%s, ProjectId=%d, FileName=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FNumber, FPermitType, FDispatcher, DateToStr(FDispatchDate), DateToStr(FExpireDate),
    FProjectId, FFileName, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TPermit.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;
  if FPermitType = EmptyStr then
  begin
    Msg := 'PermitType required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TPermitRepository }

procedure TPermitRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPermit;
begin
  if not (E is TPermit) then
    raise Exception.Create('Delete: Expected TPermit');

  R := TPermit(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPermit.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_PERMIT_ID;
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

function TPermitRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_PERMIT_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermitRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..4] of string = (COL_PERMIT_ID, COL_PERMIT_NAME, COL_PERMIT_NUMBER, COL_DISPATCH_DATE, COL_EXPIRE_DATE); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TPermit) then
    raise Exception.Create('FindBy: Expected TPermit');

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
      Hydrate(Qry, TPermit(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TPermitRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TPermit) then
    raise Exception.Create('GetById: Expected TPermit');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TPermit(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermitRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TPermit;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TPermit) then
    raise Exception.Create('Hydrate: Expected TPermit');

  R := TPermit(E);
  with aDataSet do
  begin
    R.Id := FieldByName('permit_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.Name := FieldByName('permit_name').AsString;
    R.Number := FieldByName('permit_number').AsString;
    R.PermitType := FieldByName('permit_type').AsString;
    R.Dispatcher := FieldByName('dispatcher_name').AsString;
    if not (FieldByName('dispatch_date').IsNull) then
      R.DispatchDate := FieldByName('dispatch_date').AsDateTime;
    if not (FieldByName('expire_date').IsNull) then
      R.ExpireDate := FieldByName('expire_date').AsDateTime;
    R.FileName := FieldByName('permit_filename').AsString;
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

procedure TPermitRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPermit;
begin
  if not (E is TPermit) then
    raise Exception.Create('Insert: Expected TPermit');

  R := TPermit(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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

    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetStrParam(ParamByName('permit_name'), R.Name);
    SetStrParam(ParamByName('permit_number'), R.Number);
    SetStrParam(ParamByName('permit_type'), R.PermitType);
    SetStrParam(ParamByName('dispatcher_name'), R.Dispatcher);
    SetDateParam(ParamByName('dispatch_date'), R.DispatchDate);
    SetDateParam(ParamByName('expire_date'), R.ExpireDate);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('permit_filename'), R.FileName);
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

function TPermitRepository.TableName: string;
begin
  Result := TBL_PERMITS;
end;

procedure TPermitRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPermit;
begin
  if not (E is TPermit) then
    raise Exception.Create('Update: Expected TPermit');

  R := TPermit(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPermit.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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

      SetForeignParam(ParamByName('project_id'), R.ProjectId);
      SetStrParam(ParamByName('permit_name'), R.Name);
      SetStrParam(ParamByName('permit_number'), R.Number);
      SetStrParam(ParamByName('permit_type'), R.PermitType);
      SetStrParam(ParamByName('dispatcher_name'), R.Dispatcher);
      SetDateParam(ParamByName('dispatch_date'), R.DispatchDate);
      SetDateParam(ParamByName('expire_date'), R.ExpireDate);
      SetStrParam(ParamByName('notes'), R.Notes);
      SetStrParam(ParamByName('permit_filename'), R.FileName);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('permit_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

