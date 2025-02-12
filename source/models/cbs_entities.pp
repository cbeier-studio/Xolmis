{ Xolmis Entities Data library

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit cbs_entities;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, Variants, fpjson, DateUtils,
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
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TProject; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TProject);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
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
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TProjectMember; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TProjectMember);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
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
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPermit; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPermit);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Name: String read FName write FName;
    property Number: String read FNumber write FNumber;
    property PermitType: String read FPermitType write FPermitType;
    property Dispatcher: String read FDispatcher write FDispatcher;
    property DispatchDate: TDate read FDispatchDate write FDispatchDate;
    property ExpireDate: TDate read FExpireDate write FExpireDate;
    property FileName: String read FFileName write FFileName;
    property Notes: String read FNotes write FNotes;
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

uses cbs_locale, cbs_users, cbs_global, cbs_validations, cbs_datacolumns;


{ TProject }

function TProject.Diff(aOld: TProject; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscTitle, aOld.Title, FTitle, R) then
    aList.Add(R);
  if FieldValuesDiff(rscShortTitle, aOld.ShortTitle, FShortTitle, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStartDate, aOld.StartDate, FStartDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEndDate, aOld.EndDate, FEndDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWebsite, aOld.WebsiteUri, FWebsiteUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEmail, aOld.EmailAddress, FEmailAddress, R) then
    aList.Add(R);
  if FieldValuesDiff(rscContactPerson, aOld.ContactName, FContactName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAbstract, aOld.ProjectAbstract, FAbstract, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TProject.Find(const FieldName: String; const Value: Variant): Boolean;
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
      'project_id, ' +
      'project_title, ' +
      'short_title, ' +
      'start_date, ' +
      'end_date, ' +
      'website_uri, ' +
      'email_addr, ' +
      'contact_name, ' +
      'project_file, ' +
      'contract_file, ' +
      'project_abstract, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM projects');
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
  FStartDate := NullDate;
  FEndDate := NullDate;
  FWebsiteUri := EmptyStr;
  FEmailAddress := EmptyStr;
  FContactName := EmptyStr;
  FAbstract := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TProject.Copy(aFrom: TProject);
begin
  FTitle := aFrom.Title;
  FShortTitle := aFrom.ShortTitle;
  FStartDate := aFrom.StartDate;
  FEndDate := aFrom.EndDate;
  FWebsiteUri := aFrom.WebsiteUri;
  FEmailAddress := aFrom.EmailAddress;
  FContactName := aFrom.ContactName;
  FAbstract := aFrom.ProjectAbstract;
  FNotes := aFrom.Notes;
end;

procedure TProject.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProject.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM projects');
      Add('WHERE (project_id = :aid)');

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

procedure TProject.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'project_id, ' +
      'project_title, ' +
      'short_title, ' +
      'start_date, ' +
      'end_date, ' +
      'website_uri, ' +
      'email_addr, ' +
      'contact_name, ' +
      'project_file, ' +
      'contract_file, ' +
      'project_abstract, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM projects');
    Add('WHERE project_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProject.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('project_id').AsInteger;
    FTitle := FieldByName('project_title').AsString;
    if (FieldByName('start_date').IsNull) then
      FStartDate := NullDate
    else
      FStartDate := FieldByName('start_date').AsDateTime;
    if (FieldByName('end_date').IsNull) then
      FEndDate := NullDate
    else
      FEndDate := FieldByName('end_date').AsDateTime;
    FShortTitle := FieldByName('short_title').AsString;
    FWebsiteUri := FieldByName('website_uri').AsString;
    FEmailAddress := FieldByName('email_addr').AsString;
    FContactName := FieldByName('contact_name').AsString;
    FNotes := FieldByName('notes').AsString;
    FAbstract := FieldByName('project_abstract').AsString;
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

procedure TProject.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO projects (' +
        'project_title, ' +
        'short_title, ' +
        'start_date, ' +
        'end_date, ' +
        'website_uri, ' +
        'email_addr, ' +
        'contact_name, ' +
        'project_abstract, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':project_title, ' +
        ':short_title, ' +
        'date(:start_date), ' +
        'date(:end_date), ' +
        ':website_uri, ' +
        ':email_addr, ' +
        ':contact_name, ' +
        ':project_abstract, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('project_title').AsString := FTitle;
      ParamByName('short_title').AsString := FShortTitle;
      ParamByName('start_date').AsString := FormatDateTime('yyyy-mm-dd', FStartDate);
      ParamByName('end_date').AsString := FormatDateTime('yyyy-mm-dd', FEndDate);
      ParamByName('website_uri').AsString := FWebsiteUri;
      ParamByName('email_addr').AsString := FEmailAddress;
      ParamByName('contact_name').AsString := FContactName;
      ParamByName('project_abstract').AsString := FAbstract;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProject.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TProject.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Title', FTitle);
    JSONObject.Add('Short title', FShortTitle);
    JSONObject.Add('Start date', FStartDate);
    JSONObject.Add('End date', FEndDate);
    JSONObject.Add('Website', FWebsiteUri);
    JSONObject.Add('E-mail', FEmailAddress);
    JSONObject.Add('Contact', FContactName);
    JSONObject.Add('Abstract', FAbstract);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TProject.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProject.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE projects SET ' +
        'project_title = :project_title, ' +
        'short_title = :short_title, ' +
        'start_date = date(:start_date), ' +
        'end_date = date(:end_date), ' +
        'website_uri = :website_uri, ' +
        'email_addr = :email_addr, ' +
        'contact_name = :contact_name, ' +
        'project_abstract = :project_abstract, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (project_id = :project_id)');

      ParamByName('project_title').AsString := FTitle;
      ParamByName('short_title').AsString := FShortTitle;
      ParamByName('start_date').AsString := FormatDateTime('yyyy-mm-dd', FStartDate);
      ParamByName('end_date').AsString := FormatDateTime('yyyy-mm-dd', FEndDate);
      ParamByName('website_uri').AsString := FWebsiteUri;
      ParamByName('email_addr').AsString := FEmailAddress;
      ParamByName('contact_name').AsString := FContactName;
      ParamByName('project_abstract').AsString := FAbstract;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('project_id').AsInteger := FId;

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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      ParamByName('address_1').AsString := FAddress1;
      ParamByName('address_2').AsString := FAddress2;
      ParamByName('neighborhood').AsString := FNeighborhood;
      ParamByName('zip_code').AsString := FZipCode;
      if (FCountryId > 0) then
        ParamByName('country_id').AsInteger := FCountryId
      else
        ParamByName('country_id').Clear;
      if (FStateId > 0) then
        ParamByName('state_id').AsInteger := FStateId
      else
        ParamByName('state_id').Clear;
      if (FMunicipalityId > 0) then
        ParamByName('municipality_id').AsInteger := FMunicipalityId
      else
        ParamByName('municipality_id').Clear;
      ParamByName('manager_name').AsString := FManagerName;
      ParamByName('email_addr').AsString := FEmail;
      ParamByName('phone_num').AsString := FPhone;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      ParamByName('address_1').AsString := FAddress1;
      ParamByName('address_2').AsString := FAddress2;
      ParamByName('neighborhood').AsString := FNeighborhood;
      ParamByName('zip_code').AsString := FZipCode;
      if (FCountryId > 0) then
        ParamByName('country_id').AsInteger := FCountryId
      else
        ParamByName('country_id').Clear;
      if (FStateId > 0) then
        ParamByName('state_id').AsInteger := FStateId
      else
        ParamByName('state_id').Clear;
      if (FMunicipalityId > 0) then
        ParamByName('municipality_id').AsInteger := FMunicipalityId
      else
        ParamByName('municipality_id').Clear;
      ParamByName('manager_name').AsString := FManagerName;
      ParamByName('email_addr').AsString := FEmail;
      ParamByName('phone_num').AsString := FPhone;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('institution_id').AsInteger := FId;

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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      ParamByName('citation').AsString := FCitation;
      ParamByName('title_treatment').AsString := FTitleTreatment;
      ParamByName('national_id_card').AsString := FIdDocument1;
      ParamByName('social_security_number').AsString := FIdDocument2;
      ParamByName('gender').AsString := FGender;
      ParamByName('birth_date').AsString := FormatDateTime('yyyy-mm-dd', FBirthDate);
      ParamByName('death_date').AsString := FormatDateTime('yyyy-mm-dd', FDeathDate);
      ParamByName('email_addr').AsString := FEmail;
      ParamByName('phone_1').AsString := FPhone1;
      ParamByName('phone_2').AsString := FPhone2;
      ParamByName('address_1').AsString := FAddress1;
      ParamByName('address_2').AsString := FAddress2;
      ParamByName('neighborhood').AsString := FNeighborhood;
      ParamByName('zip_code').AsString := FZipCode;
      if (FCountryId > 0) then
        ParamByName('country_id').AsInteger := FCountryId
      else
        ParamByName('country_id').Clear;
      if (FStateId > 0) then
        ParamByName('state_id').AsInteger := FStateId
      else
        ParamByName('state_id').Clear;
      if (FMunicipalityId > 0) then
        ParamByName('municipality_id').AsInteger := FMunicipalityId
      else
        ParamByName('municipality_id').Clear;
      if (FInstitutionId > 0) then
        ParamByName('institution_id').AsInteger := FInstitutionId
      else
        ParamByName('institution_id').Clear;
      ParamByName('department').AsString := FDepartment;
      ParamByName('job_role').AsString := FJobRole;
      ParamByName('lattes_uri').AsString := FLattesUri;
      ParamByName('orcid_uri').AsString := FOrcidUri;
      ParamByName('twitter_uri').AsString := FTwitterUri;
      ParamByName('instagram_uri').AsString := FInstagramUri;
      ParamByName('website_uri').AsString := FWebsiteUri;
      ParamByName('profile_color').AsString := FProfileColor;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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

      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      ParamByName('citation').AsString := FCitation;
      ParamByName('title_treatment').AsString := FTitleTreatment;
      ParamByName('national_id_card').AsString := FIdDocument1;
      ParamByName('social_security_number').AsString := FIdDocument2;
      ParamByName('gender').AsString := FGender;
      ParamByName('birth_date').AsString := FormatDateTime('yyyy-mm-dd', FBirthDate);
      ParamByName('death_date').AsString := FormatDateTime('yyyy-mm-dd', FDeathDate);
      ParamByName('email_addr').AsString := FEmail;
      ParamByName('phone_1').AsString := FPhone1;
      ParamByName('phone_2').AsString := FPhone2;
      ParamByName('address_1').AsString := FAddress1;
      ParamByName('address_2').AsString := FAddress2;
      ParamByName('neighborhood').AsString := FNeighborhood;
      ParamByName('zip_code').AsString := FZipCode;
      if (FCountryId > 0) then
        ParamByName('country_id').AsInteger := FCountryId
      else
        ParamByName('country_id').Clear;
      if (FStateId > 0) then
        ParamByName('state_id').AsInteger := FStateId
      else
        ParamByName('state_id').Clear;
      if (FMunicipalityId > 0) then
        ParamByName('municipality_id').AsInteger := FMunicipalityId
      else
        ParamByName('municipality_id').Clear;
      if (FInstitutionId > 0) then
        ParamByName('institution_id').AsInteger := FInstitutionId
      else
        ParamByName('institution_id').Clear;
      ParamByName('department').AsString := FDepartment;
      ParamByName('job_role').AsString := FJobRole;
      ParamByName('lattes_uri').AsString := FLattesUri;
      ParamByName('orcid_uri').AsString := FOrcidUri;
      ParamByName('twitter_uri').AsString := FTwitterUri;
      ParamByName('instagram_uri').AsString := FInstagramUri;
      ParamByName('website_uri').AsString := FWebsiteUri;
      ParamByName('profile_color').AsString := FProfileColor;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('person_id').AsInteger := FId;

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

{ TProjectMember }

function TProjectMember.Diff(aOld: TProjectMember; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscManager, aOld.IsProjectManager, FProjectManager, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TProjectMember.Find(const FieldName: String; const Value: Variant): Boolean;
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
      'project_member_id, ' +
      'project_id, ' +
      'person_id, ' +
      'project_manager, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_team');
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

procedure TProjectMember.Copy(aFrom: TProjectMember);
begin
  FProjectId := aFrom.ProjectId;
  FPersonId := aFrom.PersonId;
  FProjectManager := aFrom.IsProjectManager;
end;

procedure TProjectMember.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectMember.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM project_team');
      Add('WHERE (project_member_id = :aid)');

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

procedure TProjectMember.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'project_member_id, ' +
      'project_id, ' +
      'person_id, ' +
      'project_manager, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_team');
    Add('WHERE project_member_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectMember.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('project_member_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FPersonId := FieldByName('person_id').AsInteger;
    FProjectManager := FieldByName('project_manager').AsBoolean;
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

procedure TProjectMember.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO project_team (' +
        'project_id, ' +
        'person_id, ' +
        'project_manager, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':project_id, ' +
        ':person_id, ' +
        ':project_manager, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('project_manager').AsBoolean := FProjectManager;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectMember.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TProjectMember.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Person', FPersonId);
    JSONObject.Add('Is project manager', FProjectManager);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TProjectMember.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectMember.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE project_team SET ' +
        'project_id = :project_id, ' +
        'person_id = :person_id, ' +
        'project_manager = :project_manager, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (project_member_id = :project_member_id)');

      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('project_manager').AsBoolean := FProjectManager;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('project_member_id').AsInteger := FId;

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

{ TPermit }

function TPermit.Diff(aOld: TPermit; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNumber, aOld.Number, FNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.PermitType, FPermitType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDispatcher, aOld.Dispatcher, FDispatcher, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDispatchDate, aOld.DispatchDate, FDispatchDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExpireDate, aOld.ExpireDate, FExpireDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFileName, aOld.FileName, FFileName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPermit.Find(const FieldName: String; const Value: Variant): Boolean;
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
      'permit_id, ' +
      'project_id, ' +
      'permit_name, ' +
      'permit_number, ' +
      'permit_type, ' +
      'dispatcher_name, ' +
      'dispatch_date, ' +
      'expire_date, ' +
      'notes, ' +
      'permit_filename, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM legal');
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
  FDispatchDate := NullDate;
  FExpireDate := NullDate;
  FFileName := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TPermit.Copy(aFrom: TPermit);
begin
  FProjectId := aFrom.ProjectId;
  FName := aFrom.Name;
  FNumber := aFrom.Number;
  FPermitType := aFrom.PermitType;
  FDispatcher := aFrom.Dispatcher;
  FDispatchDate := aFrom.DispatchDate;
  FExpireDate := aFrom.ExpireDate;
  FFileName := aFrom.FileName;
  FNotes := aFrom.Notes;
end;

procedure TPermit.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermit.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM legal');
      Add('WHERE (permit_id = :aid)');

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

procedure TPermit.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'permit_id, ' +
      'project_id, ' +
      'permit_name, ' +
      'permit_number, ' +
      'permit_type, ' +
      'dispatcher_name, ' +
      'dispatch_date, ' +
      'expire_date, ' +
      'notes, ' +
      'permit_filename, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM legal');
    Add('WHERE permit_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermit.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('permit_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FName := FieldByName('permit_name').AsString;
    FNumber := FieldByName('permit_number').AsString;
    FPermitType := FieldByName('permit_type').AsString;
    FDispatcher := FieldByName('dispatcher_name').AsString;
    if not (FieldByName('dispatch_date').IsNull) then
      FDispatchDate := FieldByName('dispatch_date').AsDateTime;
    if not (FieldByName('expire_date').IsNull) then
      FExpireDate := FieldByName('expire_date').AsDateTime;
    FFileName := FieldByName('permit_filename').AsString;
    FNotes := FieldByName('notes').AsString;
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

procedure TPermit.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO legal (' +
        'project_id, ' +
        'permit_name, ' +
        'permit_number, ' +
        'permit_type, ' +
        'dispatcher_name, ' +
        'dispatch_date, ' +
        'expire_date, ' +
        'notes, ' +
        'permit_filename, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':project_id, ' +
        ':permit_name, ' +
        ':permit_number, ' +
        ':permit_type, ' +
        ':dispatcher_name, ' +
        'date(:dispatch_date), ' +
        'date(:expire_date), ' +
        ':notes, ' +
        ':permit_filename, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      if (FProjectId > 0) then
        ParamByName('project_id').AsInteger := FProjectId
      else
        ParamByName('project_id').Clear;
      ParamByName('permit_name').AsString := FName;
      ParamByName('permit_number').AsString := FNumber;
      ParamByName('permit_type').AsString := FPermitType;
      ParamByName('dispatcher_name').AsString := FDispatcher;
      ParamByName('dispatch_date').AsString := FormatDateTime('yyyy-mm-dd', FDispatchDate);
      ParamByName('expire_date').AsString := FormatDateTime('yyyy-mm-dd', FExpireDate);
      ParamByName('notes').AsString := FNotes;
      ParamByName('permit_filename').AsString := FFileName;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermit.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPermit.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Name', FName);
    JSONObject.Add('Number', FNumber);
    JSONObject.Add('Type', FPermitType);
    JSONObject.Add('Dispatcher', FDispatcher);
    JSONObject.Add('Dispatch date', FDispatchDate);
    JSONObject.Add('Expire date', FExpireDate);
    JSONObject.Add('Filename', FFileName);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPermit.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermit.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE legal SET ' +
        'project_id = :project_id, ' +
        'permit_name = :permit_name, ' +
        'permit_number = :permit_number, ' +
        'permit_type = :permit_type, ' +
        'dispatcher_name = :dispatcher_name, ' +
        'dispatch_date = date(:dispatch_date), ' +
        'expire_date = date(:expire_date), ' +
        'notes = :notes, ' +
        'permit_filename = :permit_filename, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (permit_id = :permit_id)');

      if (FProjectId > 0) then
        ParamByName('project_id').AsInteger := FProjectId
      else
        ParamByName('project_id').Clear;
      ParamByName('permit_name').AsString := FName;
      ParamByName('permit_number').AsString := FNumber;
      ParamByName('permit_type').AsString := FPermitType;
      ParamByName('dispatcher_name').AsString := FDispatcher;
      ParamByName('dispatch_date').AsString := FormatDateTime('yyyy-mm-dd', FDispatchDate);
      ParamByName('expire_date').AsString := FormatDateTime('yyyy-mm-dd', FExpireDate);
      ParamByName('notes').AsString := FNotes;
      ParamByName('permit_filename').AsString := FFileName;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('permit_id').AsInteger := FId;

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

end.
