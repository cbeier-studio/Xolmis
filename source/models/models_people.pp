{ Xolmis People models

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

unit models_people;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fpjson, DateUtils, TypInfo, DB, SQLDB,
  models_record_types;

type

  { TPerson }

  TPerson = class(TXolmisRecord)
  protected
    FFullName: String;
    FAcronym: String;
    FCitation: String;
    FTitleTreatment: String;
    FGender: String;
    FBirthDate: TDate;
    FDeathDate: TDate;
    FIdDocument1: String;
    FIdDocument2: String;
    FEmail: String;
    FPhone1: String;
    FPhone2: String;
    FAddress1: String;
    FAddress2: String;
    FNeighborhood: String;
    FZipCode: String;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FInstitutionId: Integer;
    FInstitutionName: String;
    FDepartment: String;
    FJobRole: String;
    FLattesUri: String;
    FOrcidUri: String;
    FTwitterUri: String;
    FInstagramUri: String;
    FWebsiteUri: String;
    FProfileColor: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPerson; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPerson);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property Acronym: String read FAcronym write FAcronym;
    property Citation: String read FCitation write FCitation;
    property TitleTreatment: String read FTitleTreatment write FTitleTreatment;
    property Gender: String read FGender write FGender;
    property BirthDate: TDate read FBirthDate write FBirthDate;
    property DeathDate: TDate read FDeathDate write FDeathDate;
    property IdDocument1: String read FIdDocument1 write FIdDocument1;
    property IdDocument2: String read FIdDocument2 write FIdDocument2;
    property Email: String read FEmail write FEmail;
    property Phone1: String read FPhone1 write FPhone1;
    property Phone2: String read FPhone2 write FPhone2;
    property Address1: String read FAddress1 write FAddress1;
    property Address2: String read FAddress2 write FAddress2;
    property Neighborhood: String read FNeighborhood write FNeighborhood;
    property ZipCode: String read FZipCode write FZipCode;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property InstitutionId: Integer read FInstitutionId write FInstitutionId;
    property InstitutionName: String read FInstitutionName write FInstitutionName;
    property Department: String read FDepartment write FDepartment;
    property JobRole: String read FJobRole write FJobRole;
    property LattesUri: String read FLattesUri write FLattesUri;
    property OrcidUri: String read FOrcidUri write FOrcidUri;
    property TwitterUri: String read FTwitterUri write FTwitterUri;
    property InstagramUri: String read FInstagramUri write FInstagramUri;
    property WebsiteUri: String read FWebsiteUri write FWebsiteUri;
    property ProfileColor: String read FProfileColor write FProfileColor;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  utils_locale, models_users, utils_global, utils_validations, data_columns, data_setparam, udm_main;

{ TPerson }

function TPerson.Diff(aOld: TPerson; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCitation, aOld.Citation, FCitation, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTreatment, aOld.TitleTreatment, FTitleTreatment, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGender, aOld.Gender, FGender, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBirthDate, aOld.BirthDate, FBirthDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDeathDate, aOld.DeathDate, FDeathDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRG, aOld.IdDocument1, FIdDocument1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCPF, aOld.IdDocument2, FIdDocument2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEmail, aOld.Email, FEmail, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhone, aOld.Phone1, FPhone1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMobilePhone, aOld.Phone2, FPhone2, R) then
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
  if FieldValuesDiff(rscInstitutionID, aOld.InstitutionId, FInstitutionId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDepartment, aOld.Department, FDepartment, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRole, aOld.JobRole, FJobRole, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLattes, aOld.LattesUri, FLattesUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOrcid, aOld.OrcidUri, FOrcidUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscXTwitter, aOld.TwitterUri, FTwitterUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscInstagram, aOld.InstagramUri, FInstagramUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWebsite, aOld.WebsiteUri, FWebsiteUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProfileColor, aOld.ProfileColor, FProfileColor, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPerson.Find(const FieldName: String; const Value: Variant): Boolean;
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
      'p.person_id, ' +
      'p.full_name, ' +
      'p.acronym, ' +
      'p.citation, ' +
      'p.title_treatment, ' +
      'p.national_id_card, ' +
      'p.social_security_number, ' +
      'p.gender, ' +
      'p.birth_date, ' +
      'p.death_date, ' +
      'p.email_addr, ' +
      'p.phone_1, ' +
      'p.phone_2, ' +
      'p.address_1, ' +
      'p.address_2, ' +
      'p.neighborhood, ' +
      'p.zip_code, ' +
      'p.country_id, ' +
      'p.state_id, ' +
      'p.municipality_id, ' +
      'p.institution_id, ' +
      'i.full_name AS institution_name' +
      'p.department, ' +
      'p.job_role, ' +
      'p.lattes_uri, ' +
      'p.orcid_uri, ' +
      'p.twitter_uri, ' +
      'p.instagram_uri, ' +
      'p.website_uri, ' +
      'p.profile_color, ' +
      'p.notes, ' +
      'p.profile_image, ' +
      'p.user_inserted, ' +
      'p.user_updated, ' +
      'datetime(p.insert_date, ''localtime'') AS insert_date, ' +
      'datetime(p.update_date, ''localtime'') AS update_date, ' +
      'p.exported_status, ' +
      'p.marked_status, ' +
      'p.active_status ' +
      'FROM people AS p');
    Add('LEFT JOIN institutions AS i ON p.institution_id = i.institution_id');
    Add('WHERE p.%afield = :avalue');
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

constructor TPerson.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TPerson.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAcronym := EmptyStr;
  FCitation := EmptyStr;
  FTitleTreatment := EmptyStr;
  FGender := EmptyStr;
  FBirthDate := NullDate;
  FDeathDate := NullDate;
  FIdDocument1 := EmptyStr;
  FIdDocument2 := EmptyStr;
  FEmail := EmptyStr;
  FPhone1 := EmptyStr;
  FPhone2 := EmptyStr;
  FAddress1 := EmptyStr;
  FAddress2 := EmptyStr;
  FNeighborhood := EmptyStr;
  FZipCode := EmptyStr;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FInstitutionId := 0;
  FInstitutionName := EmptyStr;
  FDepartment := EmptyStr;
  FJobRole := EmptyStr;
  FLattesUri := EmptyStr;
  FOrcidUri := EmptyStr;
  FTwitterUri := EmptyStr;
  FInstagramUri := EmptyStr;
  FWebsiteUri := EmptyStr;
  FProfileColor := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TPerson.Copy(aFrom: TPerson);
begin
  FFullName := aFrom.FullName;
  FAcronym := aFrom.Acronym;
  FCitation := aFrom.Citation;
  FTitleTreatment := aFrom.TitleTreatment;
  FGender := aFrom.Gender;
  FBirthDate := aFrom.BirthDate;
  FDeathDate := aFrom.DeathDate;
  FIdDocument1 := aFrom.IdDocument1;
  FIdDocument2 := aFrom.IdDocument2;
  FEmail := aFrom.Email;
  FPhone1 := aFrom.Phone1;
  FPhone2 := aFrom.Phone2;
  FAddress1 := aFrom.Address1;
  FAddress2 := aFrom.Address2;
  FNeighborhood := aFrom.Neighborhood;
  FZipCode := aFrom.ZipCode;
  FMunicipalityId := aFrom.MunicipalityId;
  FStateId := aFrom.StateId;
  FCountryId := aFrom.CountryId;
  FInstitutionId := aFrom.InstitutionId;
  FInstitutionName := aFrom.InstitutionName;
  FDepartment := aFrom.Department;
  FJobRole := aFrom.JobRole;
  FLattesUri := aFrom.LattesUri;
  FOrcidUri := aFrom.OrcidUri;
  FTwitterUri := aFrom.TwitterUri;
  FInstagramUri := aFrom.InstagramUri;
  FWebsiteUri := aFrom.WebsiteUri;
  FProfileColor := aFrom.ProfileColor;
  FNotes := aFrom.Notes;
end;

procedure TPerson.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPerson.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM people');
      Add('WHERE (person_id = :aid)');

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

procedure TPerson.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'p.person_id, ' +
        'p.full_name, ' +
        'p.acronym, ' +
        'p.citation, ' +
        'p.title_treatment, ' +
        'p.national_id_card, ' +
        'p.social_security_number, ' +
        'p.gender, ' +
        'p.birth_date, ' +
        'p.death_date, ' +
        'p.email_addr, ' +
        'p.phone_1, ' +
        'p.phone_2, ' +
        'p.address_1, ' +
        'p.address_2, ' +
        'p.neighborhood, ' +
        'p.zip_code, ' +
        'p.country_id, ' +
        'p.state_id, ' +
        'p.municipality_id, ' +
        'p.institution_id, ' +
        'i.full_name AS institution_name, ' +
        'p.department, ' +
        'p.job_role, ' +
        'p.lattes_uri, ' +
        'p.orcid_uri, ' +
        'p.twitter_uri, ' +
        'p.instagram_uri, ' +
        'p.website_uri, ' +
        'p.profile_color, ' +
        'p.notes, ' +
        'p.profile_image, ' +
        'p.user_inserted, ' +
        'p.user_updated, ' +
        'datetime(p.insert_date, ''localtime'') AS insert_date, ' +
        'datetime(p.update_date, ''localtime'') AS update_date, ' +
        'p.exported_status, ' +
        'p.marked_status, ' +
        'p.active_status ' +
      'FROM people AS p');
    Add('LEFT JOIN institutions AS i ON p.institution_id = i.institution_id');
    Add('WHERE p.person_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPerson.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('person_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FAcronym := FieldByName('acronym').AsString;
    FCitation := FieldByName('citation').AsString;
    FTitleTreatment := FieldByName('title_treatment').AsString;
    FGender := FieldByName('gender').AsString;
    if (FieldByName('birth_date').IsNull) then
      FBirthDate := NullDate
    else
      FBirthDate := FieldByName('birth_date').AsDateTime;
    if (FieldByName('death_date').IsNull) then
      FDeathDate := NullDate
    else
      FDeathDate := FieldByName('death_date').AsDateTime;
    FIdDocument1 := FieldByName('national_id_card').AsString;
    FIdDocument2 := FieldByName('social_security_number').AsString;
    FEmail := FieldByName('email_addr').AsString;
    FPhone1 := FieldByName('phone_1').AsString;
    FPhone2 := FieldByName('phone_2').AsString;
    FAddress1 := FieldByName('address_1').AsString;
    FAddress2 := FieldByName('address_2').AsString;
    FNeighborhood := FieldByName('neighborhood').AsString;
    FZipCode := FieldByName('zip_code').AsString;
    FMunicipalityId := FieldByName('municipality_id').AsInteger;
    FStateId := FieldByName('state_id').AsInteger;
    FCountryId := FieldByName('country_id').AsInteger;
    FInstitutionId := FieldByName('institution_id').AsInteger;
    FInstitutionName := FieldByName('institution_name').AsString;
    FDepartment := FieldByName('department').AsString;
    FJobRole := FieldByName('job_role').AsString;
    FLattesUri := FieldByName('lattes_uri').AsString;
    FOrcidUri := FieldByName('orcid_uri').AsString;
    FTwitterUri := FieldByName('twitter_uri').AsString;
    FInstagramUri := FieldByName('instagram_uri').AsString;
    FWebsiteUri := FieldByName('website_uri').AsString;
    FProfileColor := FieldByName('profile_color').AsString;
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

procedure TPerson.Insert;
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
      Add('INSERT INTO people (' +
        'full_name, ' +
        'acronym, ' +
        'citation, ' +
        'title_treatment, ' +
        'national_id_card, ' +
        'social_security_number, ' +
        'gender, ' +
        'birth_date, ' +
        'death_date, ' +
        'email_addr, ' +
        'phone_1, ' +
        'phone_2, ' +
        'address_1, ' +
        'address_2, ' +
        'neighborhood, ' +
        'zip_code, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'institution_id, ' +
        'department, ' +
        'job_role, ' +
        'lattes_uri, ' +
        'orcid_uri, ' +
        'twitter_uri, ' +
        'instagram_uri, ' +
        'website_uri, ' +
        'profile_color, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':full_name, ' +
        ':acronym, ' +
        ':citation, ' +
        ':title_treatment, ' +
        ':national_id_card, ' +
        ':social_security_number, ' +
        ':gender, ' +
        'date(:birth_date), ' +
        'date(:death_date), ' +
        ':email_addr, ' +
        ':phone_1, ' +
        ':phone_2, ' +
        ':address_1, ' +
        ':address_2, ' +
        ':neighborhood, ' +
        ':zip_code, ' +
        ':country_id, ' +
        ':state_id, ' +
        ':municipality_id, ' +
        ':institution_id, ' +
        ':department, ' +
        ':job_role, ' +
        ':lattes_uri, ' +
        ':orcid_uri, ' +
        ':twitter_uri, ' +
        ':instagram_uri, ' +
        ':website_uri, ' +
        ':profile_color, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      ParamByName('citation').AsString := FCitation;
      SetStrParam(ParamByName('title_treatment'), FTitleTreatment);
      SetStrParam(ParamByName('national_id_card'), FIdDocument1);
      SetStrParam(ParamByName('social_security_number'), FIdDocument2);
      SetStrParam(ParamByName('gender'), FGender);
      SetDateParam(ParamByName('birth_date'), FBirthDate);
      SetDateParam(ParamByName('death_date'), FDeathDate);
      SetStrParam(ParamByName('email_addr'), FEmail);
      SetStrParam(ParamByName('phone_1'), FPhone1);
      SetStrParam(ParamByName('phone_2'), FPhone2);
      SetStrParam(ParamByName('address_1'), FAddress1);
      SetStrParam(ParamByName('address_2'), FAddress2);
      SetStrParam(ParamByName('neighborhood'), FNeighborhood);
      SetStrParam(ParamByName('zip_code'), FZipCode);
      SetForeignParam(ParamByName('country_id'), FCountryId);
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetForeignParam(ParamByName('institution_id'), FInstitutionId);
      SetStrParam(ParamByName('department'), FDepartment);
      SetStrParam(ParamByName('job_role'), FJobRole);
      SetStrParam(ParamByName('lattes_uri'), FLattesUri);
      SetStrParam(ParamByName('orcid_uri'), FOrcidUri);
      SetStrParam(ParamByName('twitter_uri'), FTwitterUri);
      SetStrParam(ParamByName('instagram_uri'), FInstagramUri);
      SetStrParam(ParamByName('website_uri'), FWebsiteUri);
      SetStrParam(ParamByName('profile_color'), FProfileColor);
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

procedure TPerson.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPerson.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Full name', FFullName);
    JSONObject.Add('Abbreviation', FAcronym);
    JSONObject.Add('Citation', FCitation);
    JSONObject.Add('Title', FTitleTreatment);
    JSONObject.Add('Gender', FGender);
    JSONObject.Add('Birth date', FBirthDate);
    JSONObject.Add('Death date', FDeathDate);
    JSONObject.Add('ID document 1', FIdDocument1);
    JSONObject.Add('ID document 2', FIdDocument2);
    JSONObject.Add('E-mail', FEmail);
    JSONObject.Add('Phone number 1', FPhone1);
    JSONObject.Add('Phone number 2', FPhone2);
    JSONObject.Add('Address 1', FAddress1);
    JSONObject.Add('Address 2', FAddress2);
    JSONObject.Add('Neighborhood', FNeighborhood);
    JSONObject.Add('Zip code', FZipCode);
    JSONObject.Add('Municipality', FMunicipalityId);
    JSONObject.Add('State', FStateId);
    JSONObject.Add('Country', FCountryId);
    JSONObject.Add('Institution ID', FInstitutionId);
    JSONObject.Add('Institution', FInstitutionName);
    JSONObject.Add('Department', FDepartment);
    JSONObject.Add('Job role', FJobRole);
    JSONObject.Add('Lattes', FLattesUri);
    JSONObject.Add('Orcid', FOrcidUri);
    JSONObject.Add('X/Twitter', FTwitterUri);
    JSONObject.Add('Instagram', FInstagramUri);
    JSONObject.Add('Website', FWebsiteUri);
    JSONObject.Add('Profile color', FProfileColor);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPerson.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPerson.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE people SET ' +
        'full_name = :full_name, ' +
        'acronym = :acronym, ' +
        'citation = :citation, ' +
        'title_treatment = :title_treatment, ' +
        'national_id_card = :national_id_card, ' +
        'social_security_number = :social_security_number, ' +
        'gender = :gender, ' +
        'birth_date = date(:birth_date), ' +
        'death_date = date(:death_date), ' +
        'email_addr = :email_addr, ' +
        'phone_1 = :phone_1, ' +
        'phone_2 = :phone_2, ' +
        'address_1 = :address_1, ' +
        'address_2 = :address_2, ' +
        'neighborhood = :neighborhood, ' +
        'zip_code = :zip_code, ' +
        'country_id = :country_id, ' +
        'state_id = :state_id, ' +
        'municipality_id = :municipality_id, ' +
        'institution_id = :institution_id, ' +
        'department = :department, ' +
        'job_role = :job_role, ' +
        'lattes_uri = :lattes_uri, ' +
        'orcid_uri = :orcid_uri, ' +
        'twitter_uri = :twitter_uri, ' +
        'instagram_uri = :instagram_uri, ' +
        'website_uri = :website_uri, ' +
        'profile_color = :profile_color, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (person_id = :person_id)');

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      ParamByName('citation').AsString := FCitation;
      SetStrParam(ParamByName('title_treatment'), FTitleTreatment);
      SetStrParam(ParamByName('national_id_card'), FIdDocument1);
      SetStrParam(ParamByName('social_security_number'), FIdDocument2);
      SetStrParam(ParamByName('gender'), FGender);
      SetDateParam(ParamByName('birth_date'), FBirthDate);
      SetDateParam(ParamByName('death_date'), FDeathDate);
      SetStrParam(ParamByName('email_addr'), FEmail);
      SetStrParam(ParamByName('phone_1'), FPhone1);
      SetStrParam(ParamByName('phone_2'), FPhone2);
      SetStrParam(ParamByName('address_1'), FAddress1);
      SetStrParam(ParamByName('address_2'), FAddress2);
      SetStrParam(ParamByName('neighborhood'), FNeighborhood);
      SetStrParam(ParamByName('zip_code'), FZipCode);
      SetForeignParam(ParamByName('country_id'), FCountryId);
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetForeignParam(ParamByName('institution_id'), FInstitutionId);
      SetStrParam(ParamByName('department'), FDepartment);
      SetStrParam(ParamByName('job_role'), FJobRole);
      SetStrParam(ParamByName('lattes_uri'), FLattesUri);
      SetStrParam(ParamByName('orcid_uri'), FOrcidUri);
      SetStrParam(ParamByName('twitter_uri'), FTwitterUri);
      SetStrParam(ParamByName('instagram_uri'), FInstagramUri);
      SetStrParam(ParamByName('website_uri'), FWebsiteUri);
      SetStrParam(ParamByName('profile_color'), FProfileColor);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('person_id').AsInteger := FId;

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

