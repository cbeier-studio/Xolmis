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

unit cbs_users;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, cbs_record_types;

type
  TUserRank = (urAdministrator, urStandard, urVisitor);

const
  UserRankStr: array[TUserRank] of Char = ('A', 'S', 'V');

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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function IsAdmin: Boolean;
    function IsVisitor: Boolean;
    function Diff(aOld: TUser; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TUser);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property UserName: String read FUserName write FUserName;
    property Rank: TUserRank read FRank write FRank;
    property AllowManageCollection: Boolean read FAllowManageCollection write FAllowManageCollection;
    property AllowPrint: Boolean read FAllowPrint write FAllowPrint;
    property AllowExport: Boolean read FAllowExport write FAllowExport;
    property AllowImport: Boolean read FAllowImport write FAllowImport;
  end;

var
  ActiveUser: TUser;

implementation

uses cbs_global, cbs_validations, udm_main;

{ TUser }

function TUser.Diff(aOld: TUser; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Nome', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome de usu'#225'rio', aOld.UserName, FUserName, R) then
    aList.Add(R);
  if FieldValuesDiff('Tipo', aOld.Rank, FRank, R) then
    aList.Add(R);
  if FieldValuesDiff('Gerenciar acervo', aOld.AllowManageCollection, FAllowManageCollection, R) then
    aList.Add(R);
  if FieldValuesDiff('Imprimir relat'#243'rios', aOld.AllowPrint, FAllowPrint, R) then
    aList.Add(R);
  if FieldValuesDiff('Exportar dados', aOld.AllowExport, FAllowExport, R) then
    aList.Add(R);
  if FieldValuesDiff('Importar dados', aOld.AllowImport, FAllowImport, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TUser.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

constructor TUser.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
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

procedure TUser.Copy(aFrom: TUser);
begin
  FFullName := aFrom.FullName;
  FUserName := aFrom.UserName;
  FRank := aFrom.Rank;
  FAllowManageCollection := aFrom.AllowManageCollection;
  FAllowPrint := aFrom.AllowPrint;
  FAllowExport := aFrom.AllowExport;
  FAllowImport := aFrom.AllowImport;
end;

procedure TUser.Delete;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM users');
    Add('WHERE (user_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TUser.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    Add('WHERE user_id = :keyv');
    ParamByName('KEYV').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TUser.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
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
    ParamByName('full_name').AsString := FFullName;
    ParamByName('user_name').AsString := FUserName;
    ParamByName('uuid').AsString := FGuid;
    ParamByName('user_rank').AsString := UserRankStr[FRank];
    ParamByName('allow_collection_edit').AsBoolean := FAllowManageCollection;
    ParamByName('allow_print').AsBoolean := FAllowPrint;
    ParamByName('allow_export').AsBoolean := FAllowExport;
    ParamByName('allow_import').AsBoolean := FAllowImport;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TUser.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('user_id').AsInteger;
    FGuid := FieldByName('uuid').AsString;
    FFullName := FieldByName('full_name').AsString;
    FUserName := FieldByName('user_name').AsString;
    case FieldByName('user_rank').AsString of
      'A': FRank := urAdministrator;
      'S': FRank := urStandard;
      'V': FRank := urVisitor;
    end;
    FAllowManageCollection := FieldByName('allow_collection_edit').AsBoolean;
    FAllowPrint := FieldByName('allow_print').AsBoolean;
    FAllowExport := FieldByName('allow_export').AsBoolean;
    FAllowImport := FieldByName('allow_import').AsBoolean;
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

procedure TUser.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TUser.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('GUID', FGuid);
    JSONObject.Add('Fullname', FFullName);
    JSONObject.Add('Username', FUserName);
    JSONObject.Add('Rank', UserRankStr[FRank]);
    JSONObject.Add('Manage collection', FAllowManageCollection);
    JSONObject.Add('Print', FAllowPrint);
    JSONObject.Add('Export', FAllowExport);
    JSONObject.Add('Import', FAllowImport);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TUser.Update;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
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
    ParamByName('full_name').AsString := FFullName;
    ParamByName('user_name').AsString := FUserName;
    ParamByName('uuid').AsString := FGuid;
    ParamByName('user_rank').AsString := UserRankStr[FRank];
    ParamByName('allow_collection_edit').AsBoolean := FAllowManageCollection;
    ParamByName('allow_print').AsBoolean := FAllowPrint;
    ParamByName('allow_export').AsBoolean := FAllowExport;
    ParamByName('allow_import').AsBoolean := FAllowImport;
    ParamByName('marked_status').AsBoolean := FMarked;
    ParamByName('active_status').AsBoolean := FActive;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ParamByName('user_id').AsInteger := FId;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TUser.IsAdmin: Boolean;
begin
  Result := FRank = urAdministrator;
end;

function TUser.IsVisitor: Boolean;
begin
  Result := FRank = urVisitor;
end;

end.

