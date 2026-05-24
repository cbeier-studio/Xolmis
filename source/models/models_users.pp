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
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types, io_core,
  models_access_control;

type
  { TUser }

  TUser = class(TXolmisRecord)
  private
    FFullName: String;
    FUserName: String;
    FRoleId: Integer;
    FRoleName: String;
    FPermissions: TStringList;
    procedure SetRoleId(const Value: Integer);
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function IsAdmin: Boolean;
    function IsVisitor: Boolean;
    function HasPermission(const PermissionName: String): Boolean;
    procedure LoadPermissions;
    function Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean; override;
    function EqualsTo(const Other: TUser): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property UserName: String read FUserName write FUserName;
    property RoleId: Integer read FRoleId write SetRoleId;
    property RoleName: String read FRoleName write FRoleName;
  end;

  { TUserRepository }

  TUserRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure HydrateFromRow(const ARow: TXRow; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

var
  ActiveUser: TUser;

implementation

uses
  utils_global, utils_validations, utils_locale, utils_conversions,
  data_consts, data_columns, data_getvalue, data_providers,
  udm_main;

{ TUser }

constructor TUser.Create(aValue: Integer);
begin
  inherited Create;
  FPermissions := TStringList.Create;
  FPermissions.CaseSensitive := False;
  FPermissions.Sorted := True;
  FPermissions.Duplicates := dupIgnore;
  if aValue <> 0 then
    FId := aValue;
end;

destructor TUser.Destroy;
begin
  FreeAndNil(FPermissions);
  inherited Destroy;
end;

procedure TUser.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TUser then
  begin
    FFullName := TUser(Source).FullName;
    FUserName := TUser(Source).UserName;
    FRoleId := TUser(Source).RoleId;
    FRoleName := TUser(Source).RoleName;
    FPermissions.Assign(TUser(Source).FPermissions);
  end;
end;

procedure TUser.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FUserName := EmptyStr;
  FRoleId := ROLE_STANDARD_ID;
  FRoleName := EmptyStr;
  if Assigned(FPermissions) then
    FPermissions.Clear;
end;

function TUser.Clone: TXolmisRecord;
begin
  Result := TUser(inherited Clone);
end;

procedure TUser.SetRoleId(const Value: Integer);
begin
  if FRoleId = Value then
    Exit;

  FRoleId := Value;
  FPermissions.Clear;
end;

function TUser.HasPermission(const PermissionName: String): Boolean;
begin
  if FRoleId = ROLE_ADMIN_ID then
    Exit(True);

  if FPermissions.Count = 0 then
    LoadPermissions;

  Result := FPermissions.IndexOf(PermissionName) >= 0;
end;

function TUser.Diff(const OldRec: TXolmisRecord; var Changes: TStrings): Boolean;
var
  aOld: TUser;
  R: String;
begin
  Result := False;

  if not (OldRec is TUser) then
    Exit(False);

  aOld := TUser(OldRec);

  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscUsername, aOld.UserName, FUserName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.RoleName, FRoleName, R) then
    Changes.Add(R);
  if FieldValuesDiff('RoleId', aOld.RoleId, FRoleId, R) then
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
    FFullName               := Obj.Get('full_name', '');
    FUserName               := Obj.Get('user_name', '');
    FGuid                   := Obj.Get('guid', '');
    FRoleId                 := Obj.Get('role_id', ROLE_STANDARD_ID);
    FRoleName               := Obj.Get('role_name', '');
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
    JSONObject.Add('role_id', FRoleId);
    JSONObject.Add('role_name', FRoleName);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TUser.ToString: String;
begin
  Result := Format('User(Id=%d, UserName=%s, FullName=%s, GUID=%s, RoleId=%d, RoleName=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FUserName, FFullName, FGuid, FRoleId, FRoleName,
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

  if FRoleId <= 0 then
  begin
    Msg := 'Role required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

function TUser.IsAdmin: Boolean;
begin
  Result := FRoleId = ROLE_ADMIN_ID;
end;

function TUser.IsVisitor: Boolean;
begin
  Result := FRoleId = ROLE_GUEST_ID;
end;

procedure TUser.LoadPermissions;
begin
  LoadPermissionsForRole(DMM.sqlCon, FRoleId, FPermissions);
end;

{ TUserRepository }

procedure TUserRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TUser;
  RoleText: String;
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
      //Clear;
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

    Add(xProvider.Users.SelectTable(swcFieldValue));

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

procedure TUserRepository.FindByRow(const ARow: TXRow; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TUser) then
    raise Exception.Create('FindByRow: Expected TUser');

  Qry := NewQuery;
  with Qry, SQL do
  try
    //Clear;
    Add(xProvider.Users.SelectTable(swcNone));
    Add('WHERE (user_name = :aname)');

    ParamByName('aname').AsString := ARow.Values['user_name'];
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

procedure TUserRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TUser) then
    raise Exception.Create('GetById: Expected TUser');

  Qry := NewQuery;
  with Qry, SQL do
  try
    //Clear;
    //Add(xProvider.Users.SelectTable(swcId));
    Add(xProvider.Users.SelectAll(swcId));

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
    if FindField('role_id') <> nil then
      R.RoleId := FieldByName('role_id').AsInteger
    else
      R.RoleId := LegacyRankToRoleId(FieldByName('user_rank').AsString);

    if FindField('role_name') <> nil then
      R.RoleName := FieldByName('role_name').AsString
    else
      R.RoleName := FieldByName('user_rank').AsString;

    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
    if FindField('inactivated_by') <> nil then
      R.InactivatedBy := FieldByName('inactivated_by').AsString;
  end;
end;

procedure TUserRepository.HydrateFromRow(const ARow: TXRow; E: TXolmisRecord);
var
  R: TUser;
begin
  if (ARow = nil) or (E = nil) then
    Exit;
  if not (E is TUser) then
    raise Exception.Create('HydrateFromRow: Expected TUser');

  R := TUser(E);
  if ARow.IndexOfName('uuid') >= 0 then
    R.Guid := ARow.Values['uuid'];
  if ARow.IndexOfName('full_name') >= 0 then
    R.FullName := ARow.Values['full_name'];
  if ARow.IndexOfName('user_name') >= 0 then
    R.UserName := ARow.Values['user_name'];
  if ARow.IndexOfName('role_id') >= 0 then
    R.RoleId := StrToIntDef(ARow.Values['role_id'], ROLE_STANDARD_ID)
  else
    if ARow.IndexOfName('user_rank') >= 0 then
      R.RoleId := LegacyRankToRoleId(ARow.Values['user_rank']);
  if ARow.IndexOfName('role_name') >= 0 then
    R.RoleName := ARow.Values['role_name']
  else
  if ARow.IndexOfName('user_rank') >= 0 then
    R.RoleName := ARow.Values['user_rank'];
end;

procedure TUserRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TUser;
  RoleText: String;
begin
  if not (E is TUser) then
    raise Exception.Create('Insert: Expected TUser');

  R := TUser(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    //Clear;
    Add(xProvider.Users.Insert);

    RoleText := R.RoleName;
    if RoleText = EmptyStr then
      RoleText := GetRoleNameById(DMM.sqlCon, R.RoleId);

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('user_name').AsString := R.UserName;
    ParamByName('uuid').AsString := R.Guid;
    ParamByName('role_id').AsInteger := R.RoleId;
    ParamByName('user_rank').AsString := RoleText;
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
  RoleText: String;
begin
  if not (E is TUser) then
    raise Exception.Create('Update: Expected TUser');

  R := TUser(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TUser.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    //Clear;
    Add(xProvider.Users.Update);

    RoleText := R.RoleName;
    if RoleText = EmptyStr then
      RoleText := GetRoleNameById(DMM.sqlCon, R.RoleId);

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('user_name').AsString := R.UserName;
    ParamByName('uuid').AsString := R.Guid;
    ParamByName('role_id').AsInteger := R.RoleId;
    ParamByName('user_rank').AsString := RoleText;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('user_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

