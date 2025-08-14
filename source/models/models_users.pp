{ Xolmis Users Data library

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit models_users;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

type
  { TUser }

  TUser = class(TXolmisRecord)
  private
    FFullName: String;
    FUserName: String;
    FRank: TUserRank;
    FAllowManageCollection: Boolean;
    FAllowPrint: Boolean;
    FAllowExport: Boolean;
    FAllowImport: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function IsAdmin: Boolean;
    function IsVisitor: Boolean;
    function Diff(const aOld: TUser; out Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TUser): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property UserName: String read FUserName write FUserName;
    property Rank: TUserRank read FRank write FRank;
    property AllowManageCollection: Boolean read FAllowManageCollection write FAllowManageCollection;
    property AllowPrint: Boolean read FAllowPrint write FAllowPrint;
    property AllowExport: Boolean read FAllowExport write FAllowExport;
    property AllowImport: Boolean read FAllowImport write FAllowImport;
  end;

  { TUserRepository }

  TUserRepository = class(TXolmisRepository)
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

var
  ActiveUser: TUser;

implementation

uses utils_global, utils_validations, utils_locale, data_consts, data_columns, data_getvalue, udm_main;

{ TUser }

constructor TUser.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TUser.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TUser then
  begin
    FFullName := TUser(Source).FullName;
    FUserName := TUser(Source).UserName;
    FRank := TUser(Source).Rank;
    FAllowManageCollection := TUser(Source).AllowManageCollection;
    FAllowPrint := TUser(Source).AllowPrint;
    FAllowExport := TUser(Source).AllowExport;
    FAllowImport := TUser(Source).AllowImport;
  end;
end;

procedure TUser.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FUserName := EmptyStr;
  FRank := urStandard;
  FAllowManageCollection := False;
  FAllowPrint := False;
  FAllowExport := False;
  FAllowImport := False;
end;

function TUser.Clone: TXolmisRecord;
begin
  Result := TUser(inherited Clone);
end;

function TUser.Diff(const aOld: TUser; out Changes: TStrings): Boolean;
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
  if FieldValuesDiff(rscUsername, aOld.UserName, FUserName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.Rank, FRank, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscManageCollection, aOld.AllowManageCollection, FAllowManageCollection, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPrintReports, aOld.AllowPrint, FAllowPrint, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExportData, aOld.AllowExport, FAllowExport, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscImportData, aOld.AllowImport, FAllowImport, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TUser.EqualsTo(const Other: TUser): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TUser.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName := Obj.Get('full_name', '');
    FUserName := Obj.Get('user_name', '');
    FGuid     := Obj.Get('guid', '');
    case Obj.Get('rank', '') of
      'A': FRank := urAdministrator;
      'S': FRank := urStandard;
      'V': FRank := urVisitor;
    end;
    FAllowManageCollection  := Obj.Get('allow_manage_collection', False);
    FAllowPrint             := Obj.Get('allow_print', False);
    FAllowExport            := Obj.Get('allow_export', False);
    FAllowImport            := Obj.Get('allow_import', False);
  finally
    Obj.Free;
  end;
end;

function TUser.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('user_name', FUserName);
    JSONObject.Add('guid', FGuid);
    JSONObject.Add('rank', USER_RANKS[FRank]);
    JSONObject.Add('allow_manage_collection', FAllowManageCollection);
    JSONObject.Add('allow_print', FAllowPrint);
    JSONObject.Add('allow_export', FAllowExport);
    JSONObject.Add('allow_import', FAllowImport);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TUser.ToString: String;
begin
  Result := Format('User(Id=%d, UserName=%s, FullName=%s, GUID=%s, Rank=%s, ' +
    'AllowManageCollection=%s, AllowPrint=%s, AllowExport=%s, AllowImport=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FUserName, FFullName, FGuid, USER_RANKS[FRank], BoolToStr(FAllowManageCollection, 'True', 'False'),
    BoolToStr(FAllowPrint, 'True', 'False'), BoolToStr(FAllowExport, 'True', 'False'),
    BoolToStr(FAllowImport, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TUser.Validate(out Msg: string): Boolean;
begin
  if FUserName = EmptyStr then
  begin
    Msg := 'UserName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

function TUser.IsAdmin: Boolean;
begin
  Result := FRank = urAdministrator;
end;

function TUser.IsVisitor: Boolean;
begin
  Result := FRank = urVisitor;
end;

{ TUserRepository }

procedure TUserRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TUser;
begin
  if not (E is TUser) then
    raise Exception.Create('Delete: Expected TUser');

  R := TUser(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TUser.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_USER_ID;
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

function TUserRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_USER_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TUserRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..3] of string = (COL_USER_ID, COL_FULL_NAME, COL_USER_NAME, COL_UUID); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TUser) then
    raise Exception.Create('FindBy: Expected TUser');

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
      'user_id, ' +
      'full_name, ' +
      'user_name, ' +
      'user_password, ' +
      'user_rank, ' +
      'allow_collection_edit, ' +
      'allow_print, ' +
      'allow_export, ' +
      'allow_import, ' +
      'uuid, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM users');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TUser(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TUserRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TUser) then
    raise Exception.Create('GetById: Expected TUser');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'user_id, ' +
      'full_name, ' +
      'user_name, ' +
      'user_password, ' +
      'user_rank, ' +
      'allow_collection_edit, ' +
      'allow_print, ' +
      'allow_export, ' +
      'allow_import, ' +
      'uuid, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM users');
    Add('WHERE user_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TUser(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TUserRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TUser;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TUser) then
    raise Exception.Create('Hydrate: Expected TUser');

  R := TUser(E);
  with aDataSet do
  begin
    R.Id := FieldByName('user_id').AsInteger;
    R.Guid := FieldByName('uuid').AsString;
    R.FullName := FieldByName('full_name').AsString;
    R.UserName := FieldByName('user_name').AsString;
    case FieldByName('user_rank').AsString of
      'A': R.Rank := urAdministrator;
      'S': R.Rank := urStandard;
      'V': R.Rank := urVisitor;
    end;
    R.AllowManageCollection := FieldByName('allow_collection_edit').AsBoolean;
    R.AllowPrint := FieldByName('allow_print').AsBoolean;
    R.AllowExport := FieldByName('allow_export').AsBoolean;
    R.AllowImport := FieldByName('allow_import').AsBoolean;
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

procedure TUserRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TUser;
begin
  if not (E is TUser) then
    raise Exception.Create('Insert: Expected TUser');

  R := TUser(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO users (' +
      'full_name, ' +
      'user_name, ' +
      //'user_password, ' +
      'user_rank, ' +
      'allow_collection_edit, ' +
      'allow_print, ' +
      'allow_export, ' +
      'allow_import, ' +
      'uuid, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':full_name, ' +
      ':user_name, ' +
      //':user_password, ' +
      ':user_rank, ' +
      ':allow_collection_edit, ' +
      ':allow_print, ' +
      ':allow_export, ' +
      ':allow_import, ' +
      ':uuid, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('user_name').AsString := R.UserName;
    ParamByName('uuid').AsString := R.Guid;
    ParamByName('user_rank').AsString := USER_RANKS[R.Rank];
    ParamByName('allow_collection_edit').AsBoolean := R.AllowManageCollection;
    ParamByName('allow_print').AsBoolean := R.AllowPrint;
    ParamByName('allow_export').AsBoolean := R.AllowExport;
    ParamByName('allow_import').AsBoolean := R.AllowImport;
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

function TUserRepository.TableName: string;
begin
  Result := TBL_USERS;
end;

procedure TUserRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TUser;
begin
  if not (E is TUser) then
    raise Exception.Create('Update: Expected TUser');

  R := TUser(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TUser.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE users SET ' +
      'full_name = :full_name, ' +
      'user_name = :user_name, ' +
      //'user_password = :user_password, ' +
      'user_rank = :user_rank, ' +
      'allow_collection_edit = :allow_collection_edit, ' +
      'allow_print = :allow_print, ' +
      'allow_export = :allow_export, ' +
      'allow_import = :allow_import, ' +
      'uuid = :uuid, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_inserted = :user_inserted, ' +
      'insert_date = datetime(''now'',''subsec'') ');
    Add('WHERE (user_id = :user_id)');
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('user_name').AsString := R.UserName;
    ParamByName('uuid').AsString := R.Guid;
    ParamByName('user_rank').AsString := USER_RANKS[R.Rank];
    ParamByName('allow_collection_edit').AsBoolean := R.AllowManageCollection;
    ParamByName('allow_print').AsBoolean := R.AllowPrint;
    ParamByName('allow_export').AsBoolean := R.AllowExport;
    ParamByName('allow_import').AsBoolean := R.AllowImport;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ParamByName('user_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

