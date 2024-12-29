unit cbs_users;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, cbs_record_types;

type

  { TUser }

  TUser = class(TXolmisRecord)
  private
    FFullName: String;
    FUserName: String;
    FRank: String;
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
  published
    property FullName: String read FFullName write FFullName;
    property UserName: String read FUserName write FUserName;
    property Rank: String read FRank write FRank;
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
  FRank := EmptyStr;
  FAllowManageCollection := False;
  FAllowPrint := False;
  FAllowExport := False;
  FAllowImport := False;
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
    Add('SELECT * FROM users');
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

procedure TUser.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('user_id').AsInteger;
    FGuid := FieldByName('uuid').AsString;
    FFullName := FieldByName('full_name').AsString;
    FUserName := FieldByName('user_name').AsString;
    FRank := FieldByName('user_rank').AsString;
    FAllowManageCollection := FieldByName('allow_collection_edit').AsBoolean;
    FAllowPrint := FieldByName('allow_print').AsBoolean;
    FAllowExport := FieldByName('allow_export').AsBoolean;
    FAllowImport := FieldByName('allow_import').AsBoolean;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FInsertDate := FieldByName('insert_date').AsDateTime;
    FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

function TUser.IsAdmin: Boolean;
begin
  Result := FRank = 'A';
end;

function TUser.IsVisitor: Boolean;
begin
  Result := FRank = 'V';
end;

end.

