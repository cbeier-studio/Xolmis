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
    FAcronym: String;
    FManagerName: String;
    FAddress1: String;
    FAddress2: String;
    FNeighborhood: String;
    FZipCode: String;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FPhone: String;
    FEmail: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TInstitution; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TInstitution);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property Acronym: String read FAcronym write FAcronym;
    property ManagerName: String read FManagerName write FManagerName;
    property Address1: String read FAddress1 write FAddress1;
    property Address2: String read FAddress2 write FAddress2;
    property Neighborhood: String read FNeighborhood write FNeighborhood;
    property ZipCode: String read FZipCode write FZipCode;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property Phone: String read FPhone write FPhone;
    property Email: String read FEmail write FEmail;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  utils_locale, models_users, utils_validations, data_columns, data_setparam, udm_main;

{ TInstitution }

function TInstitution.Diff(aOld: TInstitution; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff(rscManager, aOld.ManagerName, FManagerName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAddress1, aOld.Address1, FAddress1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAddress2, aOld.Address2, FAddress2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNeighborhood, aOld.Neighborhood, FNeighborhood, R) then
    aList.Add(R);
  if FieldValuesDiff(rscZipCode, aOld.ZipCode, FZipCode, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMunicipalityID, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStateID, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCountryID, aOld.CountryId, FCountryId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhone, aOld.Phone, FPhone, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEmail, aOld.Email, FEmail, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TInstitution.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

constructor TInstitution.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TInstitution.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAcronym := EmptyStr;
  FManagerName := EmptyStr;
  FAddress1 := EmptyStr;
  FAddress2 := EmptyStr;
  FNeighborhood := EmptyStr;
  FZipCode := EmptyStr;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FEmail := EmptyStr;
  FPhone := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TInstitution.Copy(aFrom: TInstitution);
begin
  FFullName := aFrom.FullName;
  FAcronym := aFrom.Acronym;
  FManagerName := aFrom.ManagerName;
  FAddress1 := aFrom.Address1;
  FAddress2 := aFrom.Address2;
  FNeighborhood := aFrom.Neighborhood;
  FZipCode := aFrom.ZipCode;
  FMunicipalityId := aFrom.MunicipalityId;
  FStateId := aFrom.StateId;
  FCountryId := aFrom.CountryId;
  FEmail := aFrom.Email;
  FPhone := aFrom.Phone;
  FNotes := aFrom.Notes;
end;

procedure TInstitution.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TInstitution.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM institutions');
      Add('WHERE (institution_id = :aid)');

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

procedure TInstitution.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TInstitution.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('institution_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FAcronym := FieldByName('acronym').AsString;
    FManagerName := FieldByName('manager_name').AsString;
    FAddress1 := FieldByName('address_1').AsString;
    FAddress2 := FieldByName('address_2').AsString;
    FNeighborhood := FieldByName('neighborhood').AsString;
    FZipCode := FieldByName('zip_code').AsString;
    FMunicipalityId := FieldByName('municipality_id').AsInteger;
    FStateId := FieldByName('state_id').AsInteger;
    FCountryId := FieldByName('country_id').AsInteger;
    FEmail := FieldByName('email_addr').AsString;
    FPhone := FieldByName('phone_num').AsString;
    FNotes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TInstitution.Insert;
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

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      SetStrParam(ParamByName('address_1'), FAddress1);
      SetStrParam(ParamByName('address_2'), FAddress2);
      SetStrParam(ParamByName('neighborhood'), FNeighborhood);
      SetStrParam(ParamByName('zip_code'), FZipCode);
      SetForeignParam(ParamByName('country_id'), FCountryId);
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetStrParam(ParamByName('manager_name'), FManagerName);
      SetStrParam(ParamByName('email_addr'), FEmail);
      SetStrParam(ParamByName('phone_num'), FPhone);
      SetStrParam(ParamByName('notes'), FNotes);
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

procedure TInstitution.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TInstitution.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Abbreviation', FAcronym);
    JSONObject.Add('Manager name', FManagerName);
    JSONObject.Add('Address 1', FAddress1);
    JSONObject.Add('Address 2', FAddress2);
    JSONObject.Add('Neighborhood', FNeighborhood);
    JSONObject.Add('Zip code', FZipCode);
    JSONObject.Add('Municipality', FMunicipalityId);
    JSONObject.Add('State', FStateId);
    JSONObject.Add('Country', FCountryId);
    JSONObject.Add('E-mail', FEmail);
    JSONObject.Add('Phone number', FPhone);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TInstitution.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TInstitution.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      SetStrParam(ParamByName('address_1'), FAddress1);
      SetStrParam(ParamByName('address_2'), FAddress2);
      SetStrParam(ParamByName('neighborhood'), FNeighborhood);
      SetStrParam(ParamByName('zip_code'), FZipCode);
      SetForeignParam(ParamByName('country_id'), FCountryId);
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetStrParam(ParamByName('manager_name'), FManagerName);
      SetStrParam(ParamByName('email_addr'), FEmail);
      SetStrParam(ParamByName('phone_num'), FPhone);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('institution_id').AsInteger := FId;

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

