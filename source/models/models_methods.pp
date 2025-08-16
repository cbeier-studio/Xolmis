{ Xolmis Methods models

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

unit models_methods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

type

  { TMethod }

  TMethod = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FCategory: String;
    FEbirdName: String;
    FDescription: String;
    FRecommendedUses: String;
    FNotes: String;
    FCanDelete: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TMethod; out Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TMethod): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property Category: String read FCategory write FCategory;
    property EbirdName: String read FEbirdName write FEbirdName;
    property Description: String read FDescription write FDescription;
    property RecommendedUses: String read FRecommendedUses write FRecommendedUses;
    property Notes: String read FNotes write FNotes;
    property CanDelete: Boolean read FCanDelete write FCanDelete;
  end;

  { TMethodRepository }

  TMethodRepository = class(TXolmisRepository)
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
  utils_locale, utils_global, models_users, utils_validations, utils_fullnames, data_columns, data_getvalue,
  data_consts, data_setparam, udm_main;

{ TMethod }

constructor TMethod.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TMethod.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TMethod then
  begin
    FName := TMethod(Source).Name;
    FAbbreviation := TMethod(Source).Abbreviation;
    FCategory := TMethod(Source).Category;
    FEbirdName := TMethod(Source).EbirdName;
    FDescription := TMethod(Source).Description;
    FRecommendedUses := TMethod(Source).RecommendedUses;
    FNotes := TMethod(Source).Notes;
    FCanDelete := TMethod(Source).CanDelete;
  end;
end;

procedure TMethod.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FCategory := EmptyStr;
  FEbirdName := EmptyStr;
  FDescription := EmptyStr;
  FRecommendedUses := EmptyStr;
  FNotes := EmptyStr;
  FCanDelete := True;
end;

function TMethod.Clone: TXolmisRecord;
begin
  Result := TMethod(inherited Clone);
end;

function TMethod.Diff(const aOld: TMethod; out Changes: TStrings): Boolean;
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
  if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCategory, aOld.Category, FCategory, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRecommendedUses, aOld.RecommendedUses, FRecommendedUses, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TMethod.EqualsTo(const Other: TMethod): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TMethod.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FName             := Obj.Get('method_name', '');
    FAbbreviation     := Obj.Get('abbreviation', '');
    FCategory         := Obj.Get('category', '');
    FEbirdName        := Obj.Get('ebird_name', '');
    FDescription      := Obj.Get('description', '');
    FRecommendedUses  := Obj.Get('recommended_uses', '');
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TMethod.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('method_name', FName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('category', FCategory);
    JSONObject.Add('ebird_name', FEbirdName);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('recommended_uses', FRecommendedUses);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TMethod.ToString: String;
begin
  Result := Format('Method(Id=%d, Name=%s, Abbreviation=%s, Category=%s, EbirdName=%s, Description=%s, ' +
    'RecommendedUses=%s, Notes=%s, CanDelete=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FAbbreviation, FCategory, FEbirdName, FDescription, FRecommendedUses, FNotes,
    BoolToStr(FCanDelete, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TMethod.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;
  if FAbbreviation = EmptyStr then
  begin
    Msg := 'Abbreviation required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TMethodRepository }

procedure TMethodRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Delete: Expected TMethod');

  R := TMethod(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TMethodRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_METHOD_ID;
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

function TMethodRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_METHOD_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..3] of string = (COL_METHOD_ID, COL_METHOD_NAME, COL_METHOD_ABBREVIATION, COL_EBIRD_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TMethod) then
    raise Exception.Create('FindBy: Expected TMethod');

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
        'method_id, ' +
        'method_name, ' +
        'abbreviation, ' +
        'ebird_name, ' +
        'category, ' +
        'description, ' +
        'recommended_uses, ' +
        'notes, ' +
        'can_delete, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM methods');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TMethod(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TMethodRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TMethod) then
    raise Exception.Create('GetById: Expected TMethod');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'method_id, ' +
        'method_name, ' +
        'abbreviation, ' +
        'ebird_name, ' +
        'category, ' +
        'description, ' +
        'recommended_uses, ' +
        'notes, ' +
        'can_delete, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM methods');
    Add('WHERE method_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TMethod(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TMethod;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TMethod) then
    raise Exception.Create('Hydrate: Expected TMethod');

  R := TMethod(E);
  with aDataSet do
  begin
    R.Id := FieldByName('method_id').AsInteger;
    R.Name := FieldByName('method_name').AsString;
    R.Abbreviation := FieldByName('abbreviation').AsString;
    R.Category := FieldByName('category').AsString;
    R.EbirdName := FieldByName('ebird_name').AsString;
    R.Description := FieldByName('description').AsString;
    R.RecommendedUses := FieldByName('recommended_uses').AsString;
    R.Notes := FieldByName('notes').AsString;
    R.CanDelete := FieldByName('can_delete').AsBoolean;
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

procedure TMethodRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Insert: Expected TMethod');

  R := TMethod(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO methods (' +
        'method_name, ' +
        'abbreviation, ' +
        'category, ' +
        'ebird_name, ' +
        'description, ' +
        'recommended_uses, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':method_name, ' +
        ':abbreviation, ' +
        ':category, ' +
        ':ebird_name, ' +
        ':description, ' +
        ':recommended_uses, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('method_name').AsString := R.Name;
      ParamByName('abbreviation').AsString := R.Abbreviation;
      SetStrParam(ParamByName('category'), R.Category);
      SetStrParam(ParamByName('ebird_name'), R.EbirdName);
      SetStrParam(ParamByName('description'), R.Description);
      SetStrParam(ParamByName('recommended_uses'), R.RecommendedUses);
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

function TMethodRepository.TableName: string;
begin
  Result := TBL_METHODS;
end;

procedure TMethodRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Update: Expected TMethod');

  R := TMethod(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TMethodRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE methods SET ' +
      'method_name = :method_name, ' +
      'abbreviation = :abbreviation, ' +
      'category = :category, ' +
      'ebird_name = :ebird_name, ' +
      'description = :description, ' +
      'recommended_uses = :recommended_uses, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (method_id = :method_id)');

    ParamByName('method_name').AsString := R.Name;
    ParamByName('abbreviation').AsString := R.Abbreviation;
    SetStrParam(ParamByName('category'), R.Category);
    SetStrParam(ParamByName('ebird_name'), R.EbirdName);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('recommended_uses'), R.RecommendedUses);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('method_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

