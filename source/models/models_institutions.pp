{ Xolmis Institutions models

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

unit models_institutions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fpjson, DateUtils, TypInfo, DB, SQLDB,
  models_record_types;

type

  { TInstitution }

  TInstitution = class(TXolmisRecord)
  protected
    FFullName: String;
    FAbbreviation: String;
    FManagerName: String;
    FAddress1: String;
    FAddress2: String;
    FNeighborhood: String;
    FPostalCode: String;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FPhone: String;
    FEmail: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TInstitution; out Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TInstitution): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property ManagerName: String read FManagerName write FManagerName;
    property Address1: String read FAddress1 write FAddress1;
    property Address2: String read FAddress2 write FAddress2;
    property Neighborhood: String read FNeighborhood write FNeighborhood;
    property PostalCode: String read FPostalCode write FPostalCode;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property Phone: String read FPhone write FPhone;
    property Email: String read FEmail write FEmail;
    property Notes: String read FNotes write FNotes;
  end;

  { TInstitutionRepository }

  TInstitutionRepository = class(TXolmisRepository)
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
  utils_locale, models_users, utils_validations, data_consts, data_columns, data_setparam, data_getvalue, udm_main;

{ TInstitution }

constructor TInstitution.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TInstitution.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TInstitution then
  begin
    FFullName := TInstitution(Source).FullName;
    FAbbreviation := TInstitution(Source).Abbreviation;
    FManagerName := TInstitution(Source).ManagerName;
    FAddress1 := TInstitution(Source).Address1;
    FAddress2 := TInstitution(Source).Address2;
    FNeighborhood := TInstitution(Source).Neighborhood;
    FPostalCode := TInstitution(Source).PostalCode;
    FMunicipalityId := TInstitution(Source).MunicipalityId;
    FStateId := TInstitution(Source).StateId;
    FCountryId := TInstitution(Source).CountryId;
    FEmail := TInstitution(Source).Email;
    FPhone := TInstitution(Source).Phone;
    FNotes := TInstitution(Source).Notes;
  end;
end;

procedure TInstitution.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAbbreviation := EmptyStr;
  FManagerName := EmptyStr;
  FAddress1 := EmptyStr;
  FAddress2 := EmptyStr;
  FNeighborhood := EmptyStr;
  FPostalCode := EmptyStr;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FEmail := EmptyStr;
  FPhone := EmptyStr;
  FNotes := EmptyStr;
end;

function TInstitution.Clone: TXolmisRecord;
begin
  Result := TInstitution(inherited Clone);
end;

function TInstitution.Diff(const aOld: TInstitution; out Changes: TStrings): Boolean;
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
  if FieldValuesDiff(rscAcronym, aOld.Abbreviation, FAbbreviation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscManager, aOld.ManagerName, FManagerName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAddress1, aOld.Address1, FAddress1, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAddress2, aOld.Address2, FAddress2, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNeighborhood, aOld.Neighborhood, FNeighborhood, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscZipCode, aOld.PostalCode, FPostalCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMunicipalityID, aOld.MunicipalityId, FMunicipalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStateID, aOld.StateId, FStateId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCountryID, aOld.CountryId, FCountryId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPhone, aOld.Phone, FPhone, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEmail, aOld.Email, FEmail, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TInstitution.EqualsTo(const Other: TInstitution): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TInstitution.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName       := Obj.Get('full_name', '');
    FAbbreviation        := Obj.Get('abbreviation', '');
    FManagerName    := Obj.Get('manager_name', '');
    FAddress1       := Obj.Get('address_1', '');
    FAddress2       := Obj.Get('address_2', '');
    FNeighborhood   := Obj.Get('neighborhood', '');
    FPostalCode        := Obj.Get('postal_code', '');
    FMunicipalityId := Obj.Get('municipality_id', 0);
    FStateId        := Obj.Get('state_id', 0);
    FCountryId      := Obj.Get('country_id', 0);
    FEmail          := Obj.Get('email', '');
    FPhone          := Obj.Get('phone', '');
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TInstitution.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('manager_name', FManagerName);
    JSONObject.Add('address_1', FAddress1);
    JSONObject.Add('address_2', FAddress2);
    JSONObject.Add('neighborhood', FNeighborhood);
    JSONObject.Add('postal_code', FPostalCode);
    JSONObject.Add('municipality_id', FMunicipalityId);
    JSONObject.Add('state_id', FStateId);
    JSONObject.Add('country_id', FCountryId);
    JSONObject.Add('email', FEmail);
    JSONObject.Add('phone', FPhone);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TInstitution.ToString: String;
begin
  Result := Format('Institution(Id=%d, FullName=%s, Abbreviation=%s, ManagerName=%s, Address1=%s, Address2=%s, ' +
    'Neighborhood=%s, PostalCode=%s, MunicipalityId=%d, StateId=%d, CountryId=%d, ' +
    'Email=%s, Phone=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FAbbreviation, FManagerName, FAddress1, FAddress2, FNeighborhood, FPostalCode, FMunicipalityId,
    FStateId, FCountryId, FEmail, FPhone, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TInstitution.Validate(out Msg: string): Boolean;
begin
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TInstitutionRepository }

procedure TInstitutionRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TInstitution;
begin
  if not (E is TInstitution) then
    raise Exception.Create('Delete: Expected TInstitution');

  R := TInstitution(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TInstitution.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_INSTITUTION_ID;
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

function TInstitutionRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_INSTITUTION_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TInstitutionRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..6] of string = (COL_INSTITUTION_ID, COL_FULL_NAME, COL_ABBREVIATION, COL_MANAGER_NAME,
    COL_EMAIL_ADDRESS, COL_PHONE_NUMBER, COL_ADDRESS_1); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TInstitution) then
    raise Exception.Create('FindBy: Expected TInstitution');

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
      'institution_id, ' +
      'full_name, ' +
      'acronym, ' +
      'address_1, ' +
      'address_2, ' +
      'neighborhood, ' +
      'zip_code, ' +
      'municipality_id, ' +
      'state_id, ' +
      'country_id, ' +
      'manager_name, ' +
      'email_addr, ' +
      'phone_num, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM institutions');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TInstitution(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TInstitutionRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TInstitution) then
    raise Exception.Create('GetById: Expected TInstitution');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'institution_id, ' +
      'full_name, ' +
      'acronym, ' +
      'address_1, ' +
      'address_2, ' +
      'neighborhood, ' +
      'zip_code, ' +
      'municipality_id, ' +
      'state_id, ' +
      'country_id, ' +
      'manager_name, ' +
      'email_addr, ' +
      'phone_num, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM institutions');
    Add('WHERE institution_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TInstitution(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TInstitutionRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TInstitution;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TInstitution) then
    raise Exception.Create('Hydrate: Expected TInstitution');

  R := TInstitution(E);
  with aDataSet do
  begin
    R.Id := FieldByName('institution_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.Abbreviation := FieldByName('acronym').AsString;
    R.ManagerName := FieldByName('manager_name').AsString;
    R.Address1 := FieldByName('address_1').AsString;
    R.Address2 := FieldByName('address_2').AsString;
    R.Neighborhood := FieldByName('neighborhood').AsString;
    R.PostalCode := FieldByName('zip_code').AsString;
    R.MunicipalityId := FieldByName('municipality_id').AsInteger;
    R.StateId := FieldByName('state_id').AsInteger;
    R.CountryId := FieldByName('country_id').AsInteger;
    R.Email := FieldByName('email_addr').AsString;
    R.Phone := FieldByName('phone_num').AsString;
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

procedure TInstitutionRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TInstitution;
begin
  if not (E is TInstitution) then
    raise Exception.Create('Insert: Expected TInstitution');

  R := TInstitution(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO institutions (' +
      'full_name, ' +
      'acronym, ' +
      'address_1, ' +
      'address_2, ' +
      'neighborhood, ' +
      'zip_code, ' +
      'municipality_id, ' +
      'state_id, ' +
      'country_id, ' +
      'manager_name, ' +
      'email_addr, ' +
      'phone_num, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':full_name, ' +
      ':acronym, ' +
      ':address_1, ' +
      ':address_2, ' +
      ':neighborhood, ' +
      ':zip_code, ' +
      ':municipality_id, ' +
      ':state_id, ' +
      ':country_id, ' +
      ':manager_name, ' +
      ':email_addr, ' +
      ':phone_num, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('acronym').AsString := R.Abbreviation;
    SetStrParam(ParamByName('address_1'), R.Address1);
    SetStrParam(ParamByName('address_2'), R.Address2);
    SetStrParam(ParamByName('neighborhood'), R.Neighborhood);
    SetStrParam(ParamByName('zip_code'), R.PostalCode);
    SetForeignParam(ParamByName('country_id'), R.CountryId);
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    SetStrParam(ParamByName('manager_name'), R.ManagerName);
    SetStrParam(ParamByName('email_addr'), R.Email);
    SetStrParam(ParamByName('phone_num'), R.Phone);
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

function TInstitutionRepository.TableName: string;
begin
  Result := TBL_INSTITUTIONS;
end;

procedure TInstitutionRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TInstitution;
begin
  if not (E is TInstitution) then
    raise Exception.Create('Update: Expected TInstitution');

  R := TInstitution(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TInstitution.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE institutions SET ' +
      'full_name = :full_name, ' +
      'acronym = :acronym, ' +
      'address_1 = :address_1, ' +
      'address_2 = :address_2, ' +
      'neighborhood = :neighborhood, ' +
      'zip_code = :zip_code, ' +
      'municipality_id = :municipality_id, ' +
      'state_id = :state_id, ' +
      'country_id = :country_id, ' +
      'manager_name = :manager_name, ' +
      'email_addr = :email_addr, ' +
      'phone_num = :phone_num, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (institution_id = :institution_id)');

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('acronym').AsString := R.Abbreviation;
    SetStrParam(ParamByName('address_1'), R.Address1);
    SetStrParam(ParamByName('address_2'), R.Address2);
    SetStrParam(ParamByName('neighborhood'), R.Neighborhood);
    SetStrParam(ParamByName('zip_code'), R.PostalCode);
    SetForeignParam(ParamByName('country_id'), R.CountryId);
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    SetStrParam(ParamByName('manager_name'), R.ManagerName);
    SetStrParam(ParamByName('email_addr'), R.Email);
    SetStrParam(ParamByName('phone_num'), R.Phone);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('institution_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

