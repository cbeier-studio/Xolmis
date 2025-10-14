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

unit models_projects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, fpjson, DateUtils, TypInfo, fgl,
  DB, SQLDB,
  models_record_types,
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
    FProtocolNumber: String;
    FMainGoal: String;
    FRisks: String;
    FAbstract: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TProject; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TProject): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Title: String read FTitle write FTitle;
    property ShortTitle: String read FShortTitle write FShortTitle;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property WebsiteUri: String read FWebsiteUri write FWebsiteUri;
    property EmailAddress: String read FEmailAddress write FEmailAddress;
    property ContactName: String read FContactName write FContactName;
    property ProtocolNumber: String read FProtocolNumber write FProtocolNumber;
    property MainGoal: String read FMainGoal write FMainGoal;
    property Risks: String read FRisks write FRisks;
    property ProjectAbstract: String read FAbstract write FAbstract;
    property Notes: String read FNotes write FNotes;
  end;

  { TProjectRepository }

  TProjectRepository = class(TXolmisRepository)
  protected
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

type

  { TProjectMember }

  TProjectMember = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FPersonId: Integer;
    FProjectManager: Boolean;
    FInstitutionId: Integer;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TProjectMember; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TProjectMember): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property PersonId: Integer read FPersonId write FPersonId;
    property IsProjectManager: Boolean read FProjectManager write FProjectManager;
    property InstitutionId: Integer read FInstitutionId write FInstitutionId;
  end;

  { TProjectMemberRepository }

  TProjectMemberRepository = class(TXolmisRepository)
  protected
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

type

  { TProjectGoal }

  TProjectGoal = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FDescription: String;
    FStatus: TGoalStatus;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TProjectGoal; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TProjectGoal): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Description: String read FDescription write FDescription;
    property Status: TGoalStatus read FStatus write FStatus;
  end;

  { TProjectGoalRepository }

  TProjectGoalRepository = class(TXolmisRepository)
  protected
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

type

  { TProjectActivity }

  TProjectActivity = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FDescription: String;
    FStartDate: TDate;
    FTargetDate: TDate;
    FEndDate: TDate;
    FGoalId: Integer;
    FStatus: TActivityStatus;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TProjectActivity; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TProjectActivity): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Description: String read FDescription write FDescription;
    property StartDate: TDate read FStartDate write FStartDate;
    property TargetDate: TDate read FTargetDate write FTargetDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property GoalId: Integer read FGoalId write FGoalId;
    property Status: TActivityStatus read FStatus write FStatus;
  end;

  { TProjectActivityRepository }

  TProjectActivityRepository = class(TXolmisRepository)
  protected
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

  { TProjectRubric }

  TProjectRubric = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FFundingSource: String;
    FRubric: String;
    FItemName: String;
    FAmount: Double;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TProjectRubric; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TProjectRubric): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property FundingSource: String read FFundingSource write FFundingSource;
    property Rubric: String read FRubric write FRubric;
    property ItemName: String read FItemName write FItemName;
    property Amount: Double read FAmount write FAmount;
  end;

  { TProjectRubricRepository }

  TProjectRubricRepository = class(TXolmisRepository)
  protected
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

  { TProjectExpense }

  TProjectExpense = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FBudgetId: Integer;
    FDescription: String;
    FExpenseDate: TDate;
    FAmount: Double;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TProjectExpense; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TProjectExpense): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property BudgetId: Integer read FBudgetId write FBudgetId;
    property Description: String read FDescription write FDescription;
    property ExpenseDate: TDate read FExpenseDate write FExpenseDate;
    property Amount: Double read FAmount write FAmount;
  end;

  { TProjectExpenseRepository }

  TProjectExpenseRepository = class(TXolmisRepository)
  protected
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

var
  ProjectActivityPropsDict: specialize TFPGMap<String, String>;

  { Classes helpers }
  procedure InitProjectActivityPropsDict;


implementation

uses
  utils_locale, models_users, utils_global, utils_validations,
  data_columns, data_setparam, data_consts, data_getvalue;

procedure InitProjectActivityPropsDict;
begin
  if Assigned(ProjectActivityPropsDict) then
    Exit;

  ProjectActivityPropsDict := specialize TFPGMap<String, String>.Create;

  ProjectActivityPropsDict.Add('ProjectId', rscProjectID);
  ProjectActivityPropsDict.Add('Description', rscDescription);
  ProjectActivityPropsDict.Add('StartDate', rscStartDate);
  ProjectActivityPropsDict.Add('TargetDate', rscTargetDate);
  ProjectActivityPropsDict.Add('EndDate', rscEndDate);
  ProjectActivityPropsDict.Add('GoalId', rscGoalID);
  ProjectActivityPropsDict.Add('Status', rscStatus);
end;


{ TProject }

constructor TProject.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TProject.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TProject then
  begin
    FTitle := TProject(Source).Title;
    FShortTitle := TProject(Source).ShortTitle;
    FStartDate := TProject(Source).StartDate;
    FEndDate := TProject(Source).EndDate;
    FWebsiteUri := TProject(Source).WebsiteUri;
    FEmailAddress := TProject(Source).EmailAddress;
    FContactName := TProject(Source).ContactName;
    FProtocolNumber := TProject(Source).ProtocolNumber;
    FMainGoal := TProject(Source).MainGoal;
    FRisks := TProject(Source).Risks;
    FAbstract := TProject(Source).ProjectAbstract;
    FNotes := TProject(Source).Notes;
  end;
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
  FProtocolNumber := EmptyStr;
  FMainGoal := EmptyStr;
  FRisks := EmptyStr;
  FAbstract := EmptyStr;
  FNotes := EmptyStr;
end;

function TProject.Clone: TXolmisRecord;
begin
  Result := TProject(inherited Clone);
end;

function TProject.Diff(const aOld: TProject; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscTitle, aOld.Title, FTitle, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscShortTitle, aOld.ShortTitle, FShortTitle, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStartDate, aOld.StartDate, FStartDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEndDate, aOld.EndDate, FEndDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWebsite, aOld.WebsiteUri, FWebsiteUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEmail, aOld.EmailAddress, FEmailAddress, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscContactPerson, aOld.ContactName, FContactName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProtocolNr, aOld.ProtocolNumber, FProtocolNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMainGoal, aOld.MainGoal, FMainGoal, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRisks, aOld.Risks, FRisks, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAbstract, aOld.ProjectAbstract, FAbstract, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TProject.EqualsTo(const Other: TProject): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TProject.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FTitle          := Obj.Get('title', '');
    FShortTitle     := Obj.Get('short_title', '');
    FStartDate      := Obj.Get('start_date', NullDate);
    FEndDate        := Obj.Get('end_date', NullDate);
    FWebsiteUri     := Obj.Get('website', '');
    FEmailAddress   := Obj.Get('email', '');
    FContactName    := Obj.Get('contact', '');
    FProtocolNumber := Obj.Get('protocol_number', '');
    FMainGoal       := Obj.Get('main_goal', '');
    FRisks          := Obj.Get('risks', '');
    FAbstract       := Obj.Get('abstract', '');
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TProject.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('title', FTitle);
    JSONObject.Add('short_title', FShortTitle);
    JSONObject.Add('start_date', FStartDate);
    JSONObject.Add('end_date', FEndDate);
    JSONObject.Add('website', FWebsiteUri);
    JSONObject.Add('email', FEmailAddress);
    JSONObject.Add('contact', FContactName);
    JSONObject.Add('protocol_number', FProtocolNumber);
    JSONObject.Add('main_goal', FMainGoal);
    JSONObject.Add('risks', FRisks);
    JSONObject.Add('abstract', FAbstract);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TProject.ToString: String;
begin
  Result := Format('Project(Id=%d, Title=%s, ShortTitle=%s, StartDate=%s, EndDate=%s, WebsiteUri=%s, ' +
    'EmailAddress=%s, ContactName=%s, ProtocolNumber=%s, MainGoal=%s, Risks=%s, Abstract=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FTitle, FShortTitle, DateToStr(FStartDate), DateToStr(FEndDate), FWebsiteUri, FEmailAddress,
    FContactName, FProtocolNumber, FMainGoal, FRisks, FAbstract, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TProject.Validate(out Msg: string): Boolean;
begin
  if FTitle = EmptyStr then
  begin
    Msg := 'Title required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TProjectRepository }

procedure TProjectRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProject;
begin
  if not (E is TProject) then
    raise Exception.Create('Delete: Expected TProject');

  R := TProject(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_PROJECT_ID;
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

function TProjectRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_PROJECT_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_PROJECT_ID, COL_PROJECT_TITLE, COL_SHORT_TITLE); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TProject) then
    raise Exception.Create('FindBy: Expected TProject');

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
      'project_id, ' +
      'project_title, ' +
      'short_title, ' +
      'start_date, ' +
      'end_date, ' +
      'website_uri, ' +
      'email_addr, ' +
      'contact_name, ' +
      'protocol_number, ' +
      'main_goal, ' +
      'risks, ' +
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
      Hydrate(Qry, TProject(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TProject) then
    raise Exception.Create('GetById: Expected TProject');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
      'protocol_number, ' +
      'main_goal, ' +
      'risks, ' +
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TProject(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TProject;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TProject) then
    raise Exception.Create('Hydrate: Expected TProject');

  R := TProject(E);
  with aDataSet do
  begin
    R.Id := FieldByName('project_id').AsInteger;
    R.Title := FieldByName('project_title').AsString;
    if (FieldByName('start_date').IsNull) then
      R.StartDate := NullDate
    else
      R.StartDate := FieldByName('start_date').AsDateTime;
    if (FieldByName('end_date').IsNull) then
      R.EndDate := NullDate
    else
      R.EndDate := FieldByName('end_date').AsDateTime;
    R.ShortTitle := FieldByName('short_title').AsString;
    R.WebsiteUri := FieldByName('website_uri').AsString;
    R.EmailAddress := FieldByName('email_addr').AsString;
    R.ContactName := FieldByName('contact_name').AsString;
    R.ProtocolNumber := FieldByName('protocol_number').AsString;
    R.MainGoal := FieldByName('main_goal').AsString;
    R.Risks := FieldByName('risks').AsString;
    R.Notes := FieldByName('notes').AsString;
    R.ProjectAbstract := FieldByName('project_abstract').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TProjectRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProject;
begin
  if not (E is TProject) then
    raise Exception.Create('Insert: Expected TProject');

  R := TProject(E);
  Qry := NewQuery;
  with Qry, SQL do
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
      'protocol_number, ' +
      'main_goal, ' +
      'risks, ' +
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
      ':protocol_number, ' +
      ':main_goal, ' +
      ':risks, ' +
      ':project_abstract, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('project_title').AsString := R.Title;
    ParamByName('short_title').AsString := R.ShortTitle;
    SetDateParam(ParamByName('start_date'), R.StartDate);
    SetDateParam(ParamByName('end_date'), R.EndDate);
    SetStrParam(ParamByName('website_uri'), R.WebsiteUri);
    SetStrParam(ParamByName('email_addr'), R.EmailAddress);
    SetStrParam(ParamByName('contact_name'), R.ContactName);
    SetStrParam(ParamByName('protocol_number'), R.ProtocolNumber);
    SetStrParam(ParamByName('main_goal'), R.MainGoal);
    SetStrParam(ParamByName('risks'), R.Risks);
    SetStrParam(ParamByName('project_abstract'), R.ProjectAbstract);
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

function TProjectRepository.TableName: string;
begin
  Result := TBL_PROJECTS;
end;

procedure TProjectRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProject;
begin
  if not (E is TProject) then
    raise Exception.Create('Update: Expected TProject');

  R := TProject(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
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
      'protocol_number = :protocol_number, ' +
      'main_goal = :main_goal, ' +
      'risks = :risks, ' +
      'project_abstract = :project_abstract, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (project_id = :project_id)');

    ParamByName('project_title').AsString := R.Title;
    ParamByName('short_title').AsString := R.ShortTitle;
    SetDateParam(ParamByName('start_date'), R.StartDate);
    SetDateParam(ParamByName('end_date'), R.EndDate);
    SetStrParam(ParamByName('website_uri'), R.WebsiteUri);
    SetStrParam(ParamByName('email_addr'), R.EmailAddress);
    SetStrParam(ParamByName('contact_name'), R.ContactName);
    SetStrParam(ParamByName('protocol_number'), R.ProtocolNumber);
    SetStrParam(ParamByName('main_goal'), R.MainGoal);
    SetStrParam(ParamByName('risks'), R.Risks);
    SetStrParam(ParamByName('project_abstract'), R.ProjectAbstract);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('project_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TProjectMember }

constructor TProjectMember.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TProjectMember.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TProjectMember then
  begin
    FProjectId := TProjectMember(Source).ProjectId;
    FPersonId := TProjectMember(Source).PersonId;
    FProjectManager := TProjectMember(Source).IsProjectManager;
    FInstitutionId := TProjectMember(Source).InstitutionId;
  end;
end;

procedure TProjectMember.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FPersonId := 0;
  FProjectManager := False;
  FInstitutionId := 0;
end;

function TProjectMember.Clone: TXolmisRecord;
begin
  Result := TProjectMember(inherited Clone);
end;

function TProjectMember.Diff(const aOld: TProjectMember; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscManager, aOld.IsProjectManager, FProjectManager, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TProjectMember.EqualsTo(const Other: TProjectMember): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TProjectMember.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FProjectId      := Obj.Get('project_id', 0);
    FPersonId       := Obj.Get('person_id', 0);
    FProjectManager := Obj.Get('project_manager', False);
    FInstitutionId  := Obj.Get('institution_id', 0);
  finally
    Obj.Free;
  end;
end;

function TProjectMember.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('person_id', FPersonId);
    JSONObject.Add('project_manager', FProjectManager);
    JSONObject.Add('institution_id', FInstitutionId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TProjectMember.ToString: String;
begin
  Result := Format('ProjectMember(Id=%d, ProjectId=%d, PersonId=%d, ProjectManager=%s, InstitutionId=%d, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FProjectId, FPersonId, BoolToStr(FProjectManager, 'True', 'False'), FInstitutionId,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TProjectMember.Validate(out Msg: string): Boolean;
begin
  if FProjectId = 0 then
  begin
    Msg := 'Project required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TProjectMemberRepository }

procedure TProjectMemberRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectMember;
begin
  if not (E is TProjectMember) then
    raise Exception.Create('Delete: Expected TProjectMember');

  R := TProjectMember(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectMemberRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_PROJECT_MEMBER_ID;
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

function TProjectMemberRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_PROJECT_MEMBER_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectMemberRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_PROJECT_MEMBER_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TProjectMember) then
    raise Exception.Create('FindBy: Expected TProjectMember');

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
      'project_member_id, ' +
      'project_id, ' +
      'person_id, ' +
      'project_manager, ' +
      'institution_id, ' +
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
      Hydrate(Qry, TProjectMember(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectMemberRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TProjectMember) then
    raise Exception.Create('GetById: Expected TProjectMember');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'project_member_id, ' +
      'project_id, ' +
      'person_id, ' +
      'project_manager, ' +
      'institution_id, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_team');
    Add('WHERE project_member_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TProjectMember(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectMemberRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TProjectMember;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TProjectMember) then
    raise Exception.Create('Hydrate: Expected TProjectMember');

  R := TProjectMember(E);
  with aDataSet do
  begin
    R.Id := FieldByName('project_member_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.PersonId := FieldByName('person_id').AsInteger;
    R.IsProjectManager := FieldByName('project_manager').AsBoolean;
    R.InstitutionId := FieldByName('institution_id').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TProjectMemberRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectMember;
begin
  if not (E is TProjectMember) then
    raise Exception.Create('Insert: Expected TProjectMember');

  R := TProjectMember(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO project_team (' +
      'project_id, ' +
      'person_id, ' +
      'project_manager, ' +
      'institution_id, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':project_id, ' +
      ':person_id, ' +
      ':project_manager, ' +
      ':institution_id, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('project_id').AsInteger := R.ProjectId;
    ParamByName('person_id').AsInteger := R.PersonId;
    ParamByName('project_manager').AsBoolean := R.IsProjectManager;
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
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

function TProjectMemberRepository.TableName: string;
begin
  Result := TBL_PROJECT_TEAM;
end;

procedure TProjectMemberRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectMember;
begin
  if not (E is TProjectMember) then
    raise Exception.Create('Update: Expected TProjectMember');

  R := TProjectMember(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectMemberRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE project_team SET ' +
      'project_id = :project_id, ' +
      'person_id = :person_id, ' +
      'project_manager = :project_manager, ' +
      'institution_id = :institution_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (project_member_id = :project_member_id)');

    ParamByName('project_id').AsInteger := R.ProjectId;
    ParamByName('person_id').AsInteger := R.PersonId;
    ParamByName('project_manager').AsBoolean := R.IsProjectManager;
    SetForeignParam(ParamByName('institution_id'), R.InstitutionId);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('project_member_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TProjectGoal }

constructor TProjectGoal.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TProjectGoal.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TProjectGoal then
  begin
    FProjectId := TProjectGoal(Source).ProjectId;
    FDescription := TProjectGoal(Source).Description;
    FStatus := TProjectGoal(Source).Status;
  end;
end;

procedure TProjectGoal.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FDescription := EmptyStr;
  FStatus := gstPending;
end;

function TProjectGoal.Clone: TXolmisRecord;
begin
  Result := TProjectGoal(inherited Clone);
end;

function TProjectGoal.Diff(const aOld: TProjectGoal; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStatus, aOld.Status, FStatus, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TProjectGoal.EqualsTo(const Other: TProjectGoal): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TProjectGoal.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FProjectId    := Obj.Get('project_id', 0);
    FDescription  := Obj.Get('description', '');
    case Obj.Get('status', '') of
      'P': FStatus := gstPending;
      'R': FStatus := gstReached;
      'C': FStatus := gstCanceled;
    end;
  finally
    Obj.Free;
  end;
end;

function TProjectGoal.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('status', GOAL_STATUSES[FStatus]);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TProjectGoal.ToString: String;
begin
  Result := Format('ProjectGoal(Id=%d, ProjectId=%d, Description=%s, Status=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FProjectId, FDescription, GOAL_STATUSES[FStatus],
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TProjectGoal.Validate(out Msg: string): Boolean;
begin
  if FProjectId = 0 then
  begin
    Msg := 'Project required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TProjectGoalRepository }

procedure TProjectGoalRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectGoal;
begin
  if not (E is TProjectGoal) then
    raise Exception.Create('Delete: Expected TProjectGoal');

  R := TProjectGoal(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectGoalRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_GOAL_ID;
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

function TProjectGoalRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_GOAL_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectGoalRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_GOAL_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TProjectGoal) then
    raise Exception.Create('FindBy: Expected TProjectGoal');

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
      'goal_id, ' +
      'project_id, ' +
      'goal_description, ' +
      'goal_status, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_goals');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TProjectGoal(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectGoalRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TProjectGoal) then
    raise Exception.Create('GetById: Expected TProjectGoal');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'goal_id, ' +
      'project_id, ' +
      'goal_description, ' +
      'goal_status, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_goals');
    Add('WHERE goal_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TProjectGoal(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectGoalRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TProjectGoal;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TProjectGoal) then
    raise Exception.Create('Hydrate: Expected TProjectGoal');

  R := TProjectGoal(E);
  with aDataSet do
  begin
    R.Id := FieldByName('goal_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.Description := FieldByName('goal_description').AsString;
    case FieldByName('goal_status').AsString of
      'P': R.Status := gstPending;
      'R': R.Status := gstReached;
      'C': R.Status := gstCanceled;
    end;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TProjectGoalRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectGoal;
begin
  if not (E is TProjectGoal) then
    raise Exception.Create('Insert: Expected TProjectGoal');

  R := TProjectGoal(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO project_goals (' +
      'project_id, ' +
      'goal_description, ' +
      'goal_status, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':project_id, ' +
      ':goal_description, ' +
      ':goal_status, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('project_id').AsInteger := R.ProjectId;
    ParamByName('goal_description').AsString := R.Description;
    case R.Status of
      gstPending:   ParamByName('goal_status').AsString := 'P';
      gstReached:   ParamByName('goal_status').AsString := 'R';
      gstCanceled:  ParamByName('goal_status').AsString := 'C';
    end;
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

function TProjectGoalRepository.TableName: string;
begin
  Result := TBL_PROJECT_GOALS;
end;

procedure TProjectGoalRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectGoal;
begin
  if not (E is TProjectGoal) then
    raise Exception.Create('Update: Expected TProjectGoal');

  R := TProjectGoal(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectGoalRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE project_goals SET ' +
      'project_id = :project_id, ' +
      'goal_description = :goal_description, ' +
      'goal_status = :goal_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (goal_id = :goal_id)');

    ParamByName('project_id').AsInteger := R.ProjectId;
    ParamByName('goal_description').AsString := R.Description;
    case R.Status of
      gstPending:   ParamByName('goal_status').AsString := 'P';
      gstReached:   ParamByName('goal_status').AsString := 'R';
      gstCanceled:  ParamByName('goal_status').AsString := 'C';
    end;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('goal_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TProjectActivity }

constructor TProjectActivity.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TProjectActivity.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TProjectActivity then
  begin
    FProjectId := TProjectActivity(Source).ProjectId;
    FDescription := TProjectActivity(Source).Description;
    FStartDate := TProjectActivity(Source).StartDate;
    FTargetDate := TProjectActivity(Source).TargetDate;
    FEndDate := TProjectActivity(Source).EndDate;
    FGoalId := TProjectActivity(Source).GoalId;
    FStatus := TProjectActivity(Source).Status;
  end;
end;

procedure TProjectActivity.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FDescription := EmptyStr;
  FStartDate := NullDate;
  FTargetDate := NullDate;
  FEndDate := NullDate;
  FGoalId := 0;
  FStatus := astToDo;
end;

function TProjectActivity.Clone: TXolmisRecord;
begin
  Result := TProjectActivity(inherited Clone);
end;

function TProjectActivity.Diff(const aOld: TProjectActivity; var Changes: TStrings): Boolean;
var
  PropList: PPropList;
  PropCount, I: Integer;
  PropInfo: PPropInfo;
  OldValue, NewValue, FriendlyName: string;
begin
  Result := False;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  InitProjectActivityPropsDict;

  PropCount := GetPropList(Self.ClassInfo, tkProperties, @PropList);
  try
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      OldValue := GetPropValue(aOld, PropInfo, True);
      NewValue := GetPropValue(Self, PropInfo, True);
      if OldValue <> NewValue then
      begin
        if not ProjectActivityPropsDict.TryGetData(PropInfo^.Name, FriendlyName) then
          FriendlyName := PropInfo^.Name;
        Changes.Add(Format('%s;%s;%s', [FriendlyName, OldValue, NewValue]));
        Result := True;
      end;
    end;
  finally
    if Assigned(ProjectActivityPropsDict) then
      ProjectActivityPropsDict.Free;
    FreeMem(PropList);
  end;
end;

function TProjectActivity.EqualsTo(const Other: TProjectActivity): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TProjectActivity.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FProjectId    := Obj.Get('project_id', 0);
    FDescription  := Obj.Get('description', '');
    FStartDate    := Obj.Get('start_date', NullDate);
    FTargetDate   := Obj.Get('target_date', NullDate);
    FEndDate      := Obj.Get('end_date', NullDate);
    FGoalId       := Obj.Get('goal_id', 0);
    case Obj.Get('status', '') of
      'T': FStatus := astToDo;
      'P': FStatus := astInProgress;
      'F': FStatus := astDone;
      'C': FStatus := astCanceled;
      'D': FStatus := astDelayed;
      'R': FStatus := astNeedsReview;
      'B': FStatus := astBlocked;
    end;
  finally
    Obj.Free;
  end;
end;

function TProjectActivity.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('start_date', FStartDate);
    JSONObject.Add('target_date', FTargetDate);
    JSONObject.Add('end_date', FEndDate);
    JSONObject.Add('goal_id', FGoalId);
    JSONObject.Add('status', ACTIVITY_STATUSES[FStatus]);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TProjectActivity.ToString: String;
begin
  Result := Format('ProjectActivity(Id=%d, ProjectId=%d, Description=%s, StartDate=%s, TargetDate=%s, EndDate=%s, ' +
    'GoalId=%d, Status=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FProjectId, FDescription, DateToStr(FStartDate), DateToStr(FTargetDate), DateToStr(FEndDate),
    FGoalId, ACTIVITY_STATUSES[FStatus],
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TProjectActivity.Validate(out Msg: string): Boolean;
begin
  if FProjectId = 0 then
  begin
    Msg := 'Project required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TProjectActivityRepository }

procedure TProjectActivityRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectActivity;
begin
  if not (E is TProjectActivity) then
    raise Exception.Create('Delete: Expected TProjectActivity');

  R := TProjectActivity(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectActivityRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_CHRONOGRAM_ID;
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

function TProjectActivityRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_CHRONOGRAM_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectActivityRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_CHRONOGRAM_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TProjectActivity) then
    raise Exception.Create('FindBy: Expected TProjectActivity');

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
      'chronogram_id, ' +
      'project_id, ' +
      'description, ' +
      'date(start_date), ' +
      'date(target_date), ' +
      'date(end_date), ' +
      'goal_id, ' +
      'progress_status, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_chronograms');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TProjectActivity(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectActivityRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TProjectActivity) then
    raise Exception.Create('GetById: Expected TProjectActivity');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'chronogram_id, ' +
      'project_id, ' +
      'description, ' +
      'date(start_date), ' +
      'date(target_date), ' +
      'date(end_date), ' +
      'goal_id, ' +
      'progress_status, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_chronograms');
    Add('WHERE chronogram_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TProjectActivity(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectActivityRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TProjectActivity;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TProjectActivity) then
    raise Exception.Create('Hydrate: Expected TProjectActivity');

  R := TProjectActivity(E);
  with aDataSet do
  begin
    R.Id := FieldByName('chronogram_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.Description := FieldByName('description').AsString;
    if not FieldByName('start_date').IsNull then
      R.StartDate := FieldByName('start_date').AsDateTime;
    if not FieldByName('target_date').IsNull then
      R.TargetDate := FieldByName('target_date').AsDateTime;
    if not FieldByName('end_date').IsNull then
      R.EndDate := FieldByName('end_date').AsDateTime;
    R.GoalId := FieldByName('goal_id').AsInteger;
    case FieldByName('progress_status').AsString of
      'T': R.Status := astToDo;
      'P': R.Status := astInProgress;
      'F': R.Status := astDone;
      'C': R.Status := astCanceled;
      'D': R.Status := astDelayed;
      'R': R.Status := astNeedsReview;
      'B': R.Status := astBlocked;
    end;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TProjectActivityRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectActivity;
begin
  if not (E is TProjectActivity) then
    raise Exception.Create('Insert: Expected TProjectActivity');

  R := TProjectActivity(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO project_chronograms (' +
      'project_id, ' +
      'description, ' +
      'start_date, ' +
      'target_date, ' +
      'end_date, ' +
      'goal_id, ' +
      'progress_status, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':project_id, ' +
      ':description, ' +
      'date(:start_date), ' +
      'date(:target_date), ' +
      'date(:end_date), ' +
      ':goal_id, ' +
      ':progress_status, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    ParamByName('description').AsString := R.Description;
    SetDateParam(ParamByName('start_date'), R.StartDate);
    SetDateParam(ParamByName('target_date'), R.TargetDate);
    SetDateParam(ParamByName('end_date'), R.EndDate);
    SetForeignParam(ParamByName('goal_id'), R.GoalId);
    case R.Status of
      astToDo:        ParamByName('progress_status').AsString := 'T';
      astInProgress:  ParamByName('progress_status').AsString := 'P';
      astDone:        ParamByName('progress_status').AsString := 'F';
      astCanceled:    ParamByName('progress_status').AsString := 'C';
      astDelayed:     ParamByName('progress_status').AsString := 'D';
      astNeedsReview: ParamByName('progress_status').AsString := 'R';
      astBlocked:     ParamByName('progress_status').AsString := 'B';
    end;
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

function TProjectActivityRepository.TableName: string;
begin
  Result := TBL_PROJECT_CHRONOGRAM;
end;

procedure TProjectActivityRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectActivity;
begin
  if not (E is TProjectActivity) then
    raise Exception.Create('Update: Expected TProjectActivity');

  R := TProjectActivity(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectActivityRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE project_chronograms SET ' +
        'project_id = :project_id, ' +
        'description = :description, ' +
        'start_date = date(:start_date), ' +
        'target_date = date(:target_date), ' +
        'end_date = date(:end_date), ' +
        'goal_id = :goal_id, ' +
        'progress_status = :progress_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (chronogram_id = :chronogram_id)');

    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    ParamByName('description').AsString := R.Description;
    SetDateParam(ParamByName('start_date'), R.StartDate);
    SetDateParam(ParamByName('target_date'), R.TargetDate);
    SetDateParam(ParamByName('end_date'), R.EndDate);
    SetForeignParam(ParamByName('goal_id'), R.GoalId);
    case R.Status of
      astToDo:        ParamByName('progress_status').AsString := 'T';
      astInProgress:  ParamByName('progress_status').AsString := 'P';
      astDone:        ParamByName('progress_status').AsString := 'F';
      astCanceled:    ParamByName('progress_status').AsString := 'C';
      astDelayed:     ParamByName('progress_status').AsString := 'D';
      astNeedsReview: ParamByName('progress_status').AsString := 'R';
      astBlocked:     ParamByName('progress_status').AsString := 'B';
    end;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('chronogram_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TProjectRubric }

constructor TProjectRubric.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TProjectRubric.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TProjectRubric then
  begin
    FProjectId := TProjectRubric(Source).ProjectId;
    FFundingSource := TProjectRubric(Source).FundingSource;
    FRubric := TProjectRubric(Source).Rubric;
    FItemName := TProjectRubric(Source).ItemName;
    FAmount := TProjectRubric(Source).Amount;
  end;
end;

procedure TProjectRubric.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FFundingSource := EmptyStr;
  FRubric := EmptyStr;
  FItemName := EmptyStr;
  FAmount := 0.0;
end;

function TProjectRubric.Clone: TXolmisRecord;
begin
  Result := TProjectRubric(inherited Clone);
end;

function TProjectRubric.Diff(const aOld: TProjectRubric; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFundingSource, aOld.FundingSource, FFundingSource, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRubric, aOld.Rubric, FRubric, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscItem, aOld.ItemName, FItemName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAmount, aOld.Amount, FAmount, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TProjectRubric.EqualsTo(const Other: TProjectRubric): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TProjectRubric.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FProjectId      := Obj.Get('project_id', 0);
    FFundingSource  := Obj.Get('funding_source', '');
    FRubric         := Obj.Get('rubric', '');
    FItemName       := Obj.Get('item', '');
    FAmount         := Obj.Get('amount', 0.0);
  finally
    Obj.Free;
  end;
end;

function TProjectRubric.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('funding_source', FFundingSource);
    JSONObject.Add('rubric', FRubric);
    JSONObject.Add('item', FItemName);
    JSONObject.Add('amount', FAmount);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TProjectRubric.ToString: String;
begin
  Result := Format('ProjectRubric(Id=%d, ProjectId=%d, FundingSource=%s, Rubric=%s, ItemName=%s, Amount=%f, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FProjectId, FFundingSource, FRubric, FItemName, FAmount,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TProjectRubric.Validate(out Msg: string): Boolean;
begin
  if FProjectId = 0 then
  begin
    Msg := 'Project required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TProjectRubricRepository }

procedure TProjectRubricRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectRubric;
begin
  if not (E is TProjectRubric) then
    raise Exception.Create('Delete: Expected TProjectRubric');

  R := TProjectRubric(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectRubricRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_BUDGET_ID;
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

function TProjectRubricRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_BUDGET_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectRubricRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_BUDGET_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TProjectRubric) then
    raise Exception.Create('FindBy: Expected TProjectRubric');

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
      'budget_id, ' +
      'project_id, ' +
      'funding_source, ' +
      'rubric, ' +
      'item_name, ' +
      'amount, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_budgets');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TProjectRubric(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectRubricRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TProjectRubric) then
    raise Exception.Create('GetById: Expected TProjectRubric');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'budget_id, ' +
      'project_id, ' +
      'funding_source, ' +
      'rubric, ' +
      'item_name, ' +
      'amount, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_budgets');
    Add('WHERE budget_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TProjectRubric(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectRubricRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TProjectRubric;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TProjectRubric) then
    raise Exception.Create('Hydrate: Expected TProjectRubric');

  R := TProjectRubric(E);
  with aDataSet do
  begin
    R.Id := FieldByName('budget_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.FundingSource := FieldByName('funding_source').AsString;
    R.Rubric := FieldByName('rubric').AsString;
    R.ItemName := FieldByName('item_name').AsString;
    R.Amount := FieldByName('amount').AsFloat;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TProjectRubricRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectRubric;
begin
  if not (E is TProjectRubric) then
    raise Exception.Create('Insert: Expected TProjectRubric');

  R := TProjectRubric(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO project_budgets (' +
      'project_id, ' +
      'funding_source, ' +
      'rubric, ' +
      'item_name, ' +
      'amount, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':project_id, ' +
      ':funding_source, ' +
      ':rubric, ' +
      ':item_name, ' +
      ':amount, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('project_id').AsInteger := R.ProjectId;
    SetStrParam(ParamByName('funding_source'), R.FundingSource);
    SetStrParam(ParamByName('rubric'), R.Rubric);
    SetStrParam(ParamByName('item_name'), R.ItemName);
    SetFloatParam(ParamByName('amount'), R.Amount);
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

function TProjectRubricRepository.TableName: string;
begin
  Result := TBL_PROJECT_BUDGET;
end;

procedure TProjectRubricRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectRubric;
begin
  if not (E is TProjectRubric) then
    raise Exception.Create('Update: Expected TProjectRubric');

  R := TProjectRubric(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectRubricRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE project_budgets SET ' +
      'project_id = :project_id, ' +
      'funding_source = :funding_source, ' +
      'rubric = :rubric, ' +
      'item_name = :item_name, ' +
      'amount = :amount, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (budget_id = :budget_id)');

    ParamByName('project_id').AsInteger := R.ProjectId;
    SetStrParam(ParamByName('funding_source'), R.FundingSource);
    SetStrParam(ParamByName('rubric'), R.Rubric);
    SetStrParam(ParamByName('item_name'), R.ItemName);
    SetFloatParam(ParamByName('amount'), R.Amount);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('budget_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TProjectExpense }

constructor TProjectExpense.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TProjectExpense.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TProjectExpense then
  begin
    FProjectId := TProjectExpense(Source).ProjectId;
    FBudgetId := TProjectExpense(Source).BudgetId;
    FDescription := TProjectExpense(Source).Description;
    FExpenseDate := TProjectExpense(Source).ExpenseDate;
    FAmount := TProjectExpense(Source).Amount;
  end;
end;

procedure TProjectExpense.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FBudgetId := 0;
  FDescription := EmptyStr;
  FExpenseDate := NullDate;
  FAmount := 0.0;
end;

function TProjectExpense.Clone: TXolmisRecord;
begin
  Result := TProjectExpense(inherited Clone);
end;

function TProjectExpense.Diff(const aOld: TProjectExpense; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscBudgetID, aOld.BudgetId, FBudgetId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.ExpenseDate, FExpenseDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAmount, aOld.Amount, FAmount, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TProjectExpense.EqualsTo(const Other: TProjectExpense): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TProjectExpense.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FProjectId    := Obj.Get('project_id', 0);
    FBudgetId     := Obj.Get('budget_id', 0);
    FDescription  := Obj.Get('description', '');
    FExpenseDate  := Obj.Get('date', NullDate);
    FAmount       := Obj.Get('amount', 0.0);
  finally
    Obj.Free;
  end;
end;

function TProjectExpense.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('budget_id', FBudgetId);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('date', FExpenseDate);
    JSONObject.Add('amount', FAmount);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TProjectExpense.ToString: String;
begin
  Result := Format('ProjectExpense(Id=%d, ProjectId=%d, BudgetId=%d, Description=%s, ExpenseDate=%s, Amount=%f, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FProjectId, FBudgetId, FDescription, DateToStr(FExpenseDate), FAmount,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TProjectExpense.Validate(out Msg: string): Boolean;
begin
  if FProjectId = 0 then
  begin
    Msg := 'Project required.';
    Exit(False);
  end;
  if FBudgetId = 0 then
  begin
    Msg := 'Budget required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TProjectExpenseRepository }

procedure TProjectExpenseRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectExpense;
begin
  if not (E is TProjectExpense) then
    raise Exception.Create('Delete: Expected TProjectExpense');

  R := TProjectExpense(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectExpenseRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_EXPENSE_ID;
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

function TProjectExpenseRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_EXPENSE_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectExpenseRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_EXPENSE_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TProjectExpense) then
    raise Exception.Create('FindBy: Expected TProjectExpense');

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
      'expense_id, ' +
      'project_id, ' +
      'budget_id, ' +
      'item_description, ' +
      'date(expense_date), ' +
      'amount, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_expenses');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TProjectExpense(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectExpenseRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TProjectExpense) then
    raise Exception.Create('GetById: Expected TProjectExpense');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'expense_id, ' +
      'project_id, ' +
      'budget_id, ' +
      'item_description, ' +
      'date(expense_date), ' +
      'amount, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM project_expenses');
    Add('WHERE expense_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TProjectExpense(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectExpenseRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TProjectExpense;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TProjectExpense) then
    raise Exception.Create('Hydrate: Expected TProjectExpense');

  R := TProjectExpense(E);
  with aDataSet do
  begin
    R.Id := FieldByName('expense_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.BudgetId := FieldByName('budget_id').AsInteger;
    R.Description := FieldByName('item_description').AsString;
    if not FieldByName('expense_date').IsNull then
      R.ExpenseDate := FieldByName('expense_date').AsDateTime;
    R.Amount := FieldByName('amount').AsFloat;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TProjectExpenseRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectExpense;
begin
  if not (E is TProjectExpense) then
    raise Exception.Create('Insert: Expected TProjectExpense');

  R := TProjectExpense(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO project_expenses (' +
      'project_id, ' +
      'budget_id, ' +
      'item_description, ' +
      'expense_date, ' +
      'amount, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':project_id, ' +
      ':budget_id, ' +
      ':item_description, ' +
      'date(:expense_date), ' +
      ':amount, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('project_id').AsInteger := R.ProjectId;
    SetForeignParam(ParamByName('budget_id'), R.BudgetId);
    SetStrParam(ParamByName('item_description'), R.Description);
    SetDateParam(ParamByName('expense_date'), R.ExpenseDate);
    SetFloatParam(ParamByName('amount'), R.Amount);
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

function TProjectExpenseRepository.TableName: string;
begin
  Result := TBL_PROJECT_EXPENSES;
end;

procedure TProjectExpenseRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TProjectExpense;
begin
  if not (E is TProjectExpense) then
    raise Exception.Create('Update: Expected TProjectExpense');

  R := TProjectExpense(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TProjectExpenseRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE project_expenses SET ' +
      'project_id = :project_id, ' +
      'budget_id = :budget_id, ' +
      'item_description = :item_description, ' +
      'expense_date = date(:expense_date), ' +
      'amount = :amount, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (expense_id = :expense_id)');

    ParamByName('project_id').AsInteger := R.ProjectId;
    SetForeignParam(ParamByName('budget_id'), R.BudgetId);
    SetStrParam(ParamByName('item_description'), R.Description);
    SetDateParam(ParamByName('expense_date'), R.ExpenseDate);
    SetFloatParam(ParamByName('amount'), R.Amount);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('expense_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.
