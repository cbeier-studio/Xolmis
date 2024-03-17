unit cbs_entities;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, Variants, Types,
  { Data }
  DB, SQLDB,
  { CBS }
  cbs_record_types,
  { Forms }
  udm_main;

type

  { TProject }

  TProject = class(TXolmisRecord)
  protected
    FTitle: String;
    FShortTitle: String;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FWebsiteUri: String;
    FEmailAddress: String;
    FContactName: String;
    FAbstract: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TProject; var aList: TStrings): Boolean;
  published
    property Title: String read FTitle write FTitle;
    property ShortTitle: String read FShortTitle write FShortTitle;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property WebsiteUri: String read FWebsiteUri write FWebsiteUri;
    property EmailAddress: String read FEmailAddress write FEmailAddress;
    property ContactName: String read FContactName write FContactName;
    property ProjectAbstract: String read FAbstract write FAbstract;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TProjectMember }

  TProjectMember = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FPersonId: Integer;
    FProjectManager: Boolean;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TProjectMember; var aList: TStrings): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property PersonId: Integer read FPersonId write FPersonId;
    property IsProjectManager: Boolean read FProjectManager write FProjectManager;
  end;

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
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TPermit; var aList: TStrings): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Name: String read FName write FName;
    property Number: String read FNumber write FNumber;
    property PermitType: String read FPermitType write FPermitType;
    property Dispatcher: String read FDispatcher write FDispatcher;
    property DispatchDate: TDate read FDispatchDate write FDispatchDate;
    property ExpireDate: TDate read FExpireDate write FExpireDate;
    property FileName: String read FFileName write FFileName;
  end;

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
    function Diff(aOld: TInstitution; var aList: TStrings): Boolean;
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

type

  { TPerson }

  TPerson = class(TXolmisRecord)
  protected
    FFullName: String;
    FAcronym: String;
    FCitation: String;
    FTitleTreatment: String;
    FGenre: String;
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
    function Diff(aOld: TPerson; var aList: TStrings): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property Acronym: String read FAcronym write FAcronym;
    property Citation: String read FCitation write FCitation;
    property TitleTreatment: String read FTitleTreatment write FTitleTreatment;
    property Genre: String read FGenre write FGenre;
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

uses cbs_locale, cbs_data, cbs_validations;


{ TProject }

function TProject.Diff(aOld: TProject; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Título', aOld.Title, FTitle, R) then
    aList.Add(R);
  if FieldValuesDiff('Título curto', aOld.ShortTitle, FShortTitle, R) then
    aList.Add(R);
  if FieldValuesDiff('Data inicial', aOld.StartDate, FStartDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Data final', aOld.EndDate, FEndDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Website', aOld.WebsiteUri, FWebsiteUri, R) then
    aList.Add(R);
  if FieldValuesDiff('E-mail', aOld.EmailAddress, FEmailAddress, R) then
    aList.Add(R);
  if FieldValuesDiff('Contato', aOld.ContactName, FContactName, R) then
    aList.Add(R);
  if FieldValuesDiff('Resumo', aOld.ProjectAbstract, FAbstract, R) then
    aList.Add(R);
  if FieldValuesDiff('Anotações', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

constructor TProject.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TProject.Clear;
begin
  inherited Clear;
  FTitle := EmptyStr;
  FShortTitle := EmptyStr;
  FStartDate := StrToDate('30/12/1500');
  FEndDate := StrToDate('30/12/1500');
  FWebsiteUri := EmptyStr;
  FEmailAddress := EmptyStr;
  FContactName := EmptyStr;
  FAbstract := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TProject.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM projects');
    Add('WHERE project_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('project_id').AsInteger;
      FTitle := FieldByName('project_title').AsString;
      FStartDate := FieldByName('start_date').AsDateTime;
      FEndDate := FieldByName('end_date').AsDateTime;
      FShortTitle := FieldByName('short_title').AsString;
      FWebsiteUri := FieldByName('website_uri').AsString;
      FEmailAddress := FieldByName('email_addr').AsString;
      FContactName := FieldByName('contact_name').AsString;
      FNotes := FieldByName('notes').AsString;
      FAbstract := FieldByName('project_abstract').AsString;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TInstitution }

function TInstitution.Diff(aOld: TInstitution; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rsCaptionName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Abreviatura', aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff('Respons'#225'vel', aOld.ManagerName, FManagerName, R) then
    aList.Add(R);
  if FieldValuesDiff('Endere'#231'o', aOld.Address1, FAddress1, R) then
    aList.Add(R);
  if FieldValuesDiff('Complemento', aOld.Address2, FAddress2, R) then
    aList.Add(R);
  if FieldValuesDiff('Bairro', aOld.Neighborhood, FNeighborhood, R) then
    aList.Add(R);
  if FieldValuesDiff('CEP', aOld.ZipCode, FZipCode, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionState, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, FCountryId, R) then
    aList.Add(R);
  if FieldValuesDiff('Telefone', aOld.Phone, FPhone, R) then
    aList.Add(R);
  if FieldValuesDiff('E-mail', aOld.Email, FEmail, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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

procedure TInstitution.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM institutions');
    Add('WHERE institution_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TPerson }

function TPerson.Diff(aOld: TPerson; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rsCaptionName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Abreviatura', aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff('Citação', aOld.Citation, FCitation, R) then
    aList.Add(R);
  if FieldValuesDiff('Tratamento', aOld.TitleTreatment, FTitleTreatment, R) then
    aList.Add(R);
  if FieldValuesDiff('Gênero', aOld.Genre, FGenre, R) then
    aList.Add(R);
  if FieldValuesDiff(rsDateBirth, aOld.BirthDate, FBirthDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rsDateDeath, aOld.DeathDate, FDeathDate, R) then
    aList.Add(R);
  if FieldValuesDiff('RG', aOld.IdDocument1, FIdDocument1, R) then
    aList.Add(R);
  if FieldValuesDiff('CPF', aOld.IdDocument2, FIdDocument2, R) then
    aList.Add(R);
  if FieldValuesDiff('E-mail', aOld.Email, FEmail, R) then
    aList.Add(R);
  if FieldValuesDiff('Telefone', aOld.Phone1, FPhone1, R) then
    aList.Add(R);
  if FieldValuesDiff('Celular', aOld.Phone2, FPhone2, R) then
    aList.Add(R);
  if FieldValuesDiff('Endereço', aOld.Address1, FAddress1, R) then
    aList.Add(R);
  if FieldValuesDiff('Complemento', aOld.Address2, FAddress2, R) then
    aList.Add(R);
  if FieldValuesDiff('Bairro', aOld.Neighborhood, FNeighborhood, R) then
    aList.Add(R);
  if FieldValuesDiff('CEP', aOld.ZipCode, FZipCode, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionState, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, FCountryId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionInstitution, aOld.InstitutionId, FInstitutionId, R) then
    aList.Add(R);
  if FieldValuesDiff('Departamento', aOld.Department, FDepartment, R) then
    aList.Add(R);
  if FieldValuesDiff('Cargo', aOld.JobRole, FJobRole, R) then
    aList.Add(R);
  if FieldValuesDiff('Lattes', aOld.LattesUri, FLattesUri, R) then
    aList.Add(R);
  if FieldValuesDiff('Orcid', aOld.OrcidUri, FOrcidUri, R) then
    aList.Add(R);
  if FieldValuesDiff('Twitter', aOld.TwitterUri, FTwitterUri, R) then
    aList.Add(R);
  if FieldValuesDiff('Instagram', aOld.InstagramUri, FInstagramUri, R) then
    aList.Add(R);
  if FieldValuesDiff('Website', aOld.WebsiteUri, FWebsiteUri, R) then
    aList.Add(R);
  if FieldValuesDiff('Cor do perfil', aOld.ProfileColor, FProfileColor, R) then
    aList.Add(R);
  if FieldValuesDiff('Anotações', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  FGenre := EmptyStr;
  FBirthDate := StrToDate('30/12/1500');
  FDeathDate := StrToDate('30/12/1500');
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

procedure TPerson.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT p.*,');
    Add('(SELECT i.full_name FROM institutions AS i WHERE i.institution_id = p.institution_id) AS institution_name');
    Add('FROM people AS p');
    Add('WHERE p.person_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('person_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FAcronym := FieldByName('acronym').AsString;
      FCitation := FieldByName('citation').AsString;
      FTitleTreatment := FieldByName('title_treatment').AsString;
      FGenre := FieldByName('gender').AsString;
      FBirthDate := FieldByName('birth_date').AsDateTime;
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
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TProjectMember }

function TProjectMember.Diff(aOld: TProjectMember; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Pesquisador', aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Respons'#225'vel', aOld.IsProjectManager, FProjectManager, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

constructor TProjectMember.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TProjectMember.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FPersonId := 0;
  FProjectManager := False;
end;

procedure TProjectMember.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM project_team');
    Add('WHERE project_member_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('project_member_id').AsInteger;
      FProjectId := FieldByName('project_id').AsInteger;
      FPersonId := FieldByName('person_id').AsInteger;
      FProjectManager := FieldByName('project_manager').AsBoolean;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TPermit }

function TPermit.Diff(aOld: TPermit; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Nome', aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff('Número', aOld.Number, FNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Tipo', aOld.PermitType, FPermitType, R) then
    aList.Add(R);
  if FieldValuesDiff('Expedidor', aOld.Dispatcher, FDispatcher, R) then
    aList.Add(R);
  if FieldValuesDiff('Data de emiss'#227'o', aOld.DispatchDate, FDispatchDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Data de validade', aOld.ExpireDate, FExpireDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Anexo', aOld.FileName, FFileName, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

constructor TPermit.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TPermit.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FName := EmptyStr;
  FNumber := EmptyStr;
  FPermitType := EmptyStr;
  FDispatcher := EmptyStr;
  FDispatchDate := StrToDate('30/12/1500');
  FExpireDate := StrToDate('30/12/1500');
  FFileName := EmptyStr;
end;

procedure TPermit.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM legal');
    Add('WHERE permit_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('permit_id').AsInteger;
      FProjectId := FieldByName('project_id').AsInteger;
      FName := FieldByName('permit_name').AsString;
      FNumber := FieldByName('permit_number').AsString;
      FPermitType := FieldByName('permit_type').AsString;
      FDispatcher := FieldByName('dispatcher_name').AsString;
      FDispatchDate := FieldByName('dispatch_date').AsDateTime;
      FExpireDate := FieldByName('expire_date').AsDateTime;
      FFileName := FieldByName('permit_filename').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

end.
