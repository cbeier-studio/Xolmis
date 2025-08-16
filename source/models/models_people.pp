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
    FAbbreviation: String;
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
    FPostalCode: String;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FInstitutionId: Integer;
    FInstitutionName: String;
    FDepartment: String;
    FJobRole: String;
    FLattesUri: String;
    FOrcidUri: String;
    FXTwitterUri: String;
    FInstagramUri: String;
    FWebsiteUri: String;
    FProfileColor: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TPerson; out Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TPerson): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String; virtual;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
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
    property PostalCode: String read FPostalCode write FPostalCode;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property InstitutionId: Integer read FInstitutionId write FInstitutionId;
    property InstitutionName: String read FInstitutionName write FInstitutionName;
    property Department: String read FDepartment write FDepartment;
    property JobRole: String read FJobRole write FJobRole;
    property LattesUri: String read FLattesUri write FLattesUri;
    property OrcidUri: String read FOrcidUri write FOrcidUri;
    property XTwitterUri: String read FXTwitterUri write FXTwitterUri;
    property InstagramUri: String read FInstagramUri write FInstagramUri;
    property WebsiteUri: String read FWebsiteUri write FWebsiteUri;
    property ProfileColor: String read FProfileColor write FProfileColor;
    property Notes: String read FNotes write FNotes;
  end;

  { TPersonRepository }

  TPersonRepository = class(TXolmisRepository)
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

{ TPerson }

constructor TPerson.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TPerson.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TPerson then
  begin
    FFullName := TPerson(Source).FullName;
    FAbbreviation := TPerson(Source).Abbreviation;
    FCitation := TPerson(Source).Citation;
    FTitleTreatment := TPerson(Source).TitleTreatment;
    FGender := TPerson(Source).Gender;
    FBirthDate := TPerson(Source).BirthDate;
    FDeathDate := TPerson(Source).DeathDate;
    FIdDocument1 := TPerson(Source).IdDocument1;
    FIdDocument2 := TPerson(Source).IdDocument2;
    FEmail := TPerson(Source).Email;
    FPhone1 := TPerson(Source).Phone1;
    FPhone2 := TPerson(Source).Phone2;
    FAddress1 := TPerson(Source).Address1;
    FAddress2 := TPerson(Source).Address2;
    FNeighborhood := TPerson(Source).Neighborhood;
    FPostalCode := TPerson(Source).PostalCode;
    FMunicipalityId := TPerson(Source).MunicipalityId;
    FStateId := TPerson(Source).StateId;
    FCountryId := TPerson(Source).CountryId;
    FInstitutionId := TPerson(Source).InstitutionId;
    FInstitutionName := TPerson(Source).InstitutionName;
    FDepartment := TPerson(Source).Department;
    FJobRole := TPerson(Source).JobRole;
    FLattesUri := TPerson(Source).LattesUri;
    FOrcidUri := TPerson(Source).OrcidUri;
    FXTwitterUri := TPerson(Source).XTwitterUri;
    FInstagramUri := TPerson(Source).InstagramUri;
    FWebsiteUri := TPerson(Source).WebsiteUri;
    FProfileColor := TPerson(Source).ProfileColor;
    FNotes := TPerson(Source).Notes;
  end;
end;

procedure TPerson.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAbbreviation := EmptyStr;
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
  FPostalCode := EmptyStr;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FInstitutionId := 0;
  FInstitutionName := EmptyStr;
  FDepartment := EmptyStr;
  FJobRole := EmptyStr;
  FLattesUri := EmptyStr;
  FOrcidUri := EmptyStr;
  FXTwitterUri := EmptyStr;
  FInstagramUri := EmptyStr;
  FWebsiteUri := EmptyStr;
  FProfileColor := EmptyStr;
  FNotes := EmptyStr;
end;

function TPerson.Clone: TXolmisRecord;
begin
  Result := TPerson(inherited Clone);
end;

function TPerson.Diff(const aOld: TPerson; out Changes: TStrings): Boolean;
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
  if FieldValuesDiff(rscCitation, aOld.Citation, FCitation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTreatment, aOld.TitleTreatment, FTitleTreatment, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscGender, aOld.Gender, FGender, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBirthDate, aOld.BirthDate, FBirthDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDeathDate, aOld.DeathDate, FDeathDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRG, aOld.IdDocument1, FIdDocument1, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCPF, aOld.IdDocument2, FIdDocument2, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEmail, aOld.Email, FEmail, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPhone, aOld.Phone1, FPhone1, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMobilePhone, aOld.Phone2, FPhone2, R) then
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
  if FieldValuesDiff(rscInstitutionID, aOld.InstitutionId, FInstitutionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDepartment, aOld.Department, FDepartment, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRole, aOld.JobRole, FJobRole, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLattes, aOld.LattesUri, FLattesUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOrcid, aOld.OrcidUri, FOrcidUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscXTwitter, aOld.XTwitterUri, FXTwitterUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInstagram, aOld.InstagramUri, FInstagramUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWebsite, aOld.WebsiteUri, FWebsiteUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProfileColor, aOld.ProfileColor, FProfileColor, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TPerson.EqualsTo(const Other: TPerson): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TPerson.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName         := Obj.Get('full_name', '');
    FAbbreviation     := Obj.Get('abbreviation', '');
    FCitation         := Obj.Get('citation', '');
    FTitleTreatment   := Obj.Get('title_treatment', '');
    FGender           := Obj.Get('gender', '');
    FBirthDate        := StrToDate(Obj.Get('birth_date', NULL_DATE_STR));
    FDeathDate        := StrToDate(Obj.Get('death_date', NULL_DATE_STR));
    FIdDocument1      := Obj.Get('document_number_1', '');
    FIdDocument2      := Obj.Get('document_number_2', '');
    FEmail            := Obj.Get('email', '');
    FPhone1           := Obj.Get('phone_1', '');
    FPhone2           := Obj.Get('phone_2', '');
    FAddress1         := Obj.Get('address_1', '');
    FAddress2         := Obj.Get('address_2', '');
    FNeighborhood     := Obj.Get('neighborhood', '');
    FPostalCode       := Obj.Get('postal_code', '');
    FMunicipalityId   := Obj.Get('municipality_id', 0);
    FStateId          := Obj.Get('state_id', 0);
    FCountryId        := Obj.Get('country_id', 0);
    FInstitutionId    := Obj.Get('institution_id', 0);
    FInstitutionName  := Obj.Get('institution_name', '');
    FDepartment       := Obj.Get('department', '');
    FJobRole          := Obj.Get('job_role', '');
    FLattesUri        := Obj.Get('lattes', '');
    FOrcidUri         := Obj.Get('orcid', '');
    FXTwitterUri      := Obj.Get('x_twitter', '');
    FInstagramUri     := Obj.Get('instagram', '');
    FWebsiteUri       := Obj.Get('website', '');
    FProfileColor     := Obj.Get('profile_color', '');
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TPerson.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('citation', FCitation);
    JSONObject.Add('title_treatment', FTitleTreatment);
    JSONObject.Add('gender', FGender);
    JSONObject.Add('birth_date', FBirthDate);
    JSONObject.Add('death_date', FDeathDate);
    JSONObject.Add('document_number_1', FIdDocument1);
    JSONObject.Add('document_number_2', FIdDocument2);
    JSONObject.Add('email', FEmail);
    JSONObject.Add('phone_1', FPhone1);
    JSONObject.Add('phone_2', FPhone2);
    JSONObject.Add('address_1', FAddress1);
    JSONObject.Add('address_2', FAddress2);
    JSONObject.Add('neighborhood', FNeighborhood);
    JSONObject.Add('postal_code', FPostalCode);
    JSONObject.Add('municipality_id', FMunicipalityId);
    JSONObject.Add('state_id', FStateId);
    JSONObject.Add('country_id', FCountryId);
    JSONObject.Add('institution_id', FInstitutionId);
    JSONObject.Add('institution_name', FInstitutionName);
    JSONObject.Add('department', FDepartment);
    JSONObject.Add('job_role', FJobRole);
    JSONObject.Add('lattes', FLattesUri);
    JSONObject.Add('orcid', FOrcidUri);
    JSONObject.Add('x_twitter', FXTwitterUri);
    JSONObject.Add('instagram', FInstagramUri);
    JSONObject.Add('website', FWebsiteUri);
    JSONObject.Add('profile_color', FProfileColor);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TPerson.ToString: String;
begin
  Result := Format('Person(Id=%d, FullName=%s, Abbreviation=%s, Citation=%s, TitleTreatment=%s, Gender=%s, ' +
    'BirthDate=%s, DeathDate=%s, IdDocument1=%s, IdDocument2=%s, Email=%s, Phone1=%s, Phone2=%s, Address1=%s, ' +
    'Address2=%s, Neighborhood=%s, PostalCode=%s, MunicipalityId=%d, StateId=%d, CountryId=%d, InstitutionId=%d, ' +
    'InstitutionName=%s, Department=%s, JobRole=%s, LattesUri=%s, OrcidUri=%s, XTwitterUri=%s, InstagramUri=%s, ' +
    'WebsiteUri=%s, ProfileColor=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FAbbreviation, FCitation, FTitleTreatment, FGender, DateToStr(FBirthDate), DateToStr(FDeathDate),
    FIdDocument1, FIdDocument2, FEmail, FPhone1, FPhone2, FAddress1, FAddress2, FNeighborhood, FPostalCode,
    FMunicipalityId, FStateId, FCountryId, FInstitutionId, FInstitutionName, FDepartment, FJobRole, FLattesUri,
    FOrcidUri, FXTwitterUri, FInstagramUri, FWebsiteUri, FProfileColor, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TPerson.Validate(out Msg: string): Boolean;
begin
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
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

{ TPersonRepository }

procedure TPersonRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPerson;
begin
  if not (E is TPerson) then
    raise Exception.Create('Delete: Expected TPerson');

  R := TPerson(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPerson.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_PERSON_ID;
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

function TPersonRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_PERSON_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPersonRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..8] of string = (COL_PERSON_ID, COL_FULL_NAME, COL_ABBREVIATION, COL_CITATION,
    COL_DOCUMENT_NUMBER_1, COL_DOCUMENT_NUMBER_2, COL_EMAIL_ADDRESS, COL_PHONE_1, COL_PHONE_2); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TPerson) then
    raise Exception.Create('FindBy: Expected TPerson');

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
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TPerson(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TPersonRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TPerson) then
    raise Exception.Create('GetById: Expected TPerson');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    Add('WHERE p.person_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TPerson(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPersonRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TPerson;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TPerson) then
    raise Exception.Create('Hydrate: Expected TPerson');

  R := TPerson(E);
  with aDataSet do
  begin
    R.Id := FieldByName('person_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.Abbreviation := FieldByName('acronym').AsString;
    R.Citation := FieldByName('citation').AsString;
    R.TitleTreatment := FieldByName('title_treatment').AsString;
    R.Gender := FieldByName('gender').AsString;
    if (FieldByName('birth_date').IsNull) then
      R.BirthDate := NullDate
    else
      R.BirthDate := FieldByName('birth_date').AsDateTime;
    if (FieldByName('death_date').IsNull) then
      R.DeathDate := NullDate
    else
      R.DeathDate := FieldByName('death_date').AsDateTime;
    R.IdDocument1 := FieldByName('national_id_card').AsString;
    R.IdDocument2 := FieldByName('social_security_number').AsString;
    R.Email := FieldByName('email_addr').AsString;
    R.Phone1 := FieldByName('phone_1').AsString;
    R.Phone2 := FieldByName('phone_2').AsString;
    R.Address1 := FieldByName('address_1').AsString;
    R.Address2 := FieldByName('address_2').AsString;
    R.Neighborhood := FieldByName('neighborhood').AsString;
    R.PostalCode := FieldByName('zip_code').AsString;
    R.MunicipalityId := FieldByName('municipality_id').AsInteger;
    R.StateId := FieldByName('state_id').AsInteger;
    R.CountryId := FieldByName('country_id').AsInteger;
    R.InstitutionId := FieldByName('institution_id').AsInteger;
    R.InstitutionName := FieldByName('institution_name').AsString;
    R.Department := FieldByName('department').AsString;
    R.JobRole := FieldByName('job_role').AsString;
    R.LattesUri := FieldByName('lattes_uri').AsString;
    R.OrcidUri := FieldByName('orcid_uri').AsString;
    R.XTwitterUri := FieldByName('twitter_uri').AsString;
    R.InstagramUri := FieldByName('instagram_uri').AsString;
    R.WebsiteUri := FieldByName('website_uri').AsString;
    R.ProfileColor := FieldByName('profile_color').AsString;
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

procedure TPersonRepository.Insert(E: TXolmisRecord);
var
  R: TPerson;
  Qry: TSQLQuery;
begin
  if not (E is TPerson) then
    raise Exception.Create('Insert: Expected TPerson');

  R := TPerson(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('acronym').AsString := R.Abbreviation;
    ParamByName('citation').AsString := R.Citation;
    SetStrParam(ParamByName('title_treatment'), R.TitleTreatment);
    SetStrParam(ParamByName('national_id_card'), R.IdDocument1);
    SetStrParam(ParamByName('social_security_number'), R.IdDocument2);
    SetStrParam(ParamByName('gender'), R.Gender);
    SetDateParam(ParamByName('birth_date'), R.BirthDate);
    SetDateParam(ParamByName('death_date'), R.DeathDate);
    SetStrParam(ParamByName('email_addr'), R.Email);
    SetStrParam(ParamByName('phone_1'), R.Phone1);
    SetStrParam(ParamByName('phone_2'), R.Phone2);
    SetStrParam(ParamByName('address_1'), R.Address1);
    SetStrParam(ParamByName('address_2'), R.Address2);
    SetStrParam(ParamByName('neighborhood'), R.Neighborhood);
    SetStrParam(ParamByName('zip_code'), R.PostalCode);
    SetForeignParam(ParamByName('country_id'), R.CountryId);
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
    SetStrParam(ParamByName('department'), R.Department);
    SetStrParam(ParamByName('job_role'), R.JobRole);
    SetStrParam(ParamByName('lattes_uri'), R.LattesUri);
    SetStrParam(ParamByName('orcid_uri'), R.OrcidUri);
    SetStrParam(ParamByName('twitter_uri'), R.XTwitterUri);
    SetStrParam(ParamByName('instagram_uri'), R.InstagramUri);
    SetStrParam(ParamByName('website_uri'), R.WebsiteUri);
    SetStrParam(ParamByName('profile_color'), R.ProfileColor);
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

function TPersonRepository.TableName: string;
begin
  Result := TBL_PEOPLE;
end;

procedure TPersonRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TPerson;
begin
  if not (E is TPerson) then
    raise Exception.Create('Update: Expected TPerson');

  R := TPerson(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TPerson.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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

    ParamByName('full_name').AsString := R.FullName;
    ParamByName('acronym').AsString := R.Abbreviation;
    ParamByName('citation').AsString := R.Citation;
    SetStrParam(ParamByName('title_treatment'), R.TitleTreatment);
    SetStrParam(ParamByName('national_id_card'), R.IdDocument1);
    SetStrParam(ParamByName('social_security_number'), R.IdDocument2);
    SetStrParam(ParamByName('gender'), R.Gender);
    SetDateParam(ParamByName('birth_date'), R.BirthDate);
    SetDateParam(ParamByName('death_date'), R.DeathDate);
    SetStrParam(ParamByName('email_addr'), R.Email);
    SetStrParam(ParamByName('phone_1'), R.Phone1);
    SetStrParam(ParamByName('phone_2'), R.Phone2);
    SetStrParam(ParamByName('address_1'), R.Address1);
    SetStrParam(ParamByName('address_2'), R.Address2);
    SetStrParam(ParamByName('neighborhood'), R.Neighborhood);
    SetStrParam(ParamByName('zip_code'), R.PostalCode);
    SetForeignParam(ParamByName('country_id'), R.CountryId);
    SetForeignParam(ParamByName('state_id'), R.StateId);
    SetForeignParam(ParamByName('municipality_id'), R.MunicipalityId);
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
    SetStrParam(ParamByName('department'), R.Department);
    SetStrParam(ParamByName('job_role'), R.JobRole);
    SetStrParam(ParamByName('lattes_uri'), R.LattesUri);
    SetStrParam(ParamByName('orcid_uri'), R.OrcidUri);
    SetStrParam(ParamByName('twitter_uri'), R.XTwitterUri);
    SetStrParam(ParamByName('instagram_uri'), R.InstagramUri);
    SetStrParam(ParamByName('website_uri'), R.WebsiteUri);
    SetStrParam(ParamByName('profile_color'), R.ProfileColor);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('person_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

