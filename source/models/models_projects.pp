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
  { System }
  Classes, SysUtils, Variants, fpjson, DateUtils, TypInfo, fgl,
  { Data }
  DB, SQLDB,
  { CBS }
  models_record_types,
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
    FProtocolNumber: String;
    FMainGoal: String;
    FRisks: String;
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
    property ProtocolNumber: String read FProtocolNumber write FProtocolNumber;
    property MainGoal: String read FMainGoal write FMainGoal;
    property Risks: String read FRisks write FRisks;
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
    FInstitutionId: Integer;
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
    property InstitutionId: Integer read FInstitutionId write FInstitutionId;
  end;

type

  { TProjectGoal }

  TProjectGoal = class(TXolmisRecord)
  protected
    FProjectId: Integer;
    FDescription: String;
    FStatus: TGoalStatus;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TProjectGoal; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TProjectGoal);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Description: String read FDescription write FDescription;
    property Status: TGoalStatus read FStatus write FStatus;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TProjectActivity; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TProjectActivity);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property Description: String read FDescription write FDescription;
    property StartDate: TDate read FStartDate write FStartDate;
    property TargetDate: TDate read FTargetDate write FTargetDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property GoalId: Integer read FGoalId write FGoalId;
    property Status: TActivityStatus read FStatus write FStatus;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TProjectRubric; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TProjectRubric);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property FundingSource: String read FFundingSource write FFundingSource;
    property Rubric: String read FRubric write FRubric;
    property ItemName: String read FItemName write FItemName;
    property Amount: Double read FAmount write FAmount;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TProjectExpense; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TProjectExpense);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property ProjectId: Integer read FProjectId write FProjectId;
    property BudgetId: Integer read FBudgetId write FBudgetId;
    property Description: String read FDescription write FDescription;
    property ExpenseDate: TDate read FExpenseDate write FExpenseDate;
    property Amount: Double read FAmount write FAmount;
  end;

var
  ProjectActivityPropsDict: specialize TFPGMap<String, String>;

  { Classes helpers }
  procedure InitProjectActivityPropsDict;


implementation

uses
  utils_locale, models_users, utils_global, utils_validations, data_columns, data_setparam;

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
  if FieldValuesDiff(rscProtocolNr, aOld.ProtocolNumber, FProtocolNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMainGoal, aOld.MainGoal, FMainGoal, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRisks, aOld.Risks, FRisks, R) then
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
  FProtocolNumber := EmptyStr;
  FMainGoal := EmptyStr;
  FRisks := EmptyStr;
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
  FProtocolNumber := aFrom.ProtocolNumber;
  FMainGoal := aFrom.MainGoal;
  FRisks := aFrom.Risks;
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
    FProtocolNumber := FieldByName('protocol_number').AsString;
    FMainGoal := FieldByName('main_goal').AsString;
    FRisks := FieldByName('risks').AsString;
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

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('project_title').AsString := FTitle;
      ParamByName('short_title').AsString := FShortTitle;
      SetDateParam(ParamByName('start_date'), FStartDate);
      SetDateParam(ParamByName('end_date'), FEndDate);
      SetStrParam(ParamByName('website_uri'), FWebsiteUri);
      SetStrParam(ParamByName('email_addr'), FEmailAddress);
      SetStrParam(ParamByName('contact_name'), FContactName);
      SetStrParam(ParamByName('protocol_number'), FProtocolNumber);
      SetStrParam(ParamByName('main_goal'), FMainGoal);
      SetStrParam(ParamByName('risks'), FRisks);
      SetStrParam(ParamByName('project_abstract'), FAbstract);
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
    JSONObject.Add('Protocol number', FProtocolNumber);
    JSONObject.Add('Main goal', FMainGoal);
    JSONObject.Add('Risks', FRisks);
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

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('project_title').AsString := FTitle;
      ParamByName('short_title').AsString := FShortTitle;
      SetDateParam(ParamByName('start_date'), FStartDate);
      SetDateParam(ParamByName('end_date'), FEndDate);
      SetStrParam(ParamByName('website_uri'), FWebsiteUri);
      SetStrParam(ParamByName('email_addr'), FEmailAddress);
      SetStrParam(ParamByName('contact_name'), FContactName);
      SetStrParam(ParamByName('protocol_number'), FProtocolNumber);
      SetStrParam(ParamByName('main_goal'), FMainGoal);
      SetStrParam(ParamByName('risks'), FRisks);
      SetStrParam(ParamByName('project_abstract'), FAbstract);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('project_id').AsInteger := FId;

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
  FInstitutionId := 0;
end;

procedure TProjectMember.Copy(aFrom: TProjectMember);
begin
  FProjectId := aFrom.ProjectId;
  FPersonId := aFrom.PersonId;
  FProjectManager := aFrom.IsProjectManager;
  FInstitutionId := aFrom.InstitutionId;
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
    FInstitutionId := FieldByName('institution_id').AsInteger;
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

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('project_manager').AsBoolean := FProjectManager;
      SetForeignParam(ParamByName('institution_id'), FInstitutionId);
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
    JSONObject.Add('Institution', FInstitutionId);

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

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE project_team SET ' +
        'project_id = :project_id, ' +
        'person_id = :person_id, ' +
        'project_manager = :project_manager, ' +
        'institution_id = :institution_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (project_member_id = :project_member_id)');

      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('project_manager').AsBoolean := FProjectManager;
      SetForeignParam(ParamByName('institution_id'), FInstitutionId);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('project_member_id').AsInteger := FId;

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

{ TProjectGoal }

constructor TProjectGoal.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TProjectGoal.Clear;
begin
  inherited Clear;
  FProjectId := 0;
  FDescription := EmptyStr;
  FStatus := gstPending;
end;

procedure TProjectGoal.Copy(aFrom: TProjectGoal);
begin
  FProjectId := aFrom.ProjectId;
  FDescription := aFrom.Description;
  FStatus := aFrom.Status;
end;

procedure TProjectGoal.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectGoal.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM project_goals');
      Add('WHERE (goal_id = :aid)');

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

function TProjectGoal.Diff(aOld: TProjectGoal; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStatus, aOld.Status, FStatus, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TProjectGoal.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectGoal.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectGoal.Insert;
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

      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('goal_description').AsString := FDescription;
      case FStatus of
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

procedure TProjectGoal.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('goal_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FDescription := FieldByName('goal_description').AsString;
    case FieldByName('goal_status').AsString of
      'P': FStatus := gstPending;
      'R': FStatus := gstReached;
      'C': FStatus := gstCanceled;
    end;
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

procedure TProjectGoal.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TProjectGoal.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Status', GOAL_STATUSES[FStatus]);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TProjectGoal.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectGoal.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE project_goals SET ' +
        'project_id = :project_id, ' +
        'goal_description = :goal_description, ' +
        'goal_status = :goal_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (goal_id = :goal_id)');

      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('goal_description').AsString := FDescription;
      case FStatus of
        gstPending:   ParamByName('goal_status').AsString := 'P';
        gstReached:   ParamByName('goal_status').AsString := 'R';
        gstCanceled:  ParamByName('goal_status').AsString := 'C';
      end;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('goal_id').AsInteger := FId;

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

{ TProjectActivity }

constructor TProjectActivity.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
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

procedure TProjectActivity.Copy(aFrom: TProjectActivity);
begin
  FProjectId := aFrom.ProjectId;
  FDescription := aFrom.Description;
  FStartDate := aFrom.StartDate;
  FTargetDate := aFrom.TargetDate;
  FEndDate := aFrom.EndDate;
  FGoalId := aFrom.GoalId;
  FStatus := aFrom.Status;
end;

procedure TProjectActivity.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectActivity.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM project_chronograms');
      Add('WHERE (chronogram_id = :aid)');

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

function TProjectActivity.Diff(aOld: TProjectActivity; var aList: TStrings): Boolean;
var
  PropList: PPropList;
  PropCount, I: Integer;
  PropInfo: PPropInfo;
  OldValue, NewValue, FriendlyName: string;
begin
  Result := False;

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
        aList.Add(Format('%s;%s;%s', [FriendlyName, OldValue, NewValue]));
        Result := True;
      end;
    end;
  finally
    if Assigned(ProjectActivityPropsDict) then
      ProjectActivityPropsDict.Free;
    FreeMem(PropList);
  end;
end;

function TProjectActivity.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectActivity.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectActivity.Insert;
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

      SetForeignParam(ParamByName('project_id'), FProjectId);
      ParamByName('description').AsString := FDescription;
      SetDateParam(ParamByName('start_date'), FStartDate);
      SetDateParam(ParamByName('target_date'), FTargetDate);
      SetDateParam(ParamByName('end_date'), FEndDate);
      SetForeignParam(ParamByName('goal_id'), FGoalId);
      case FStatus of
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

procedure TProjectActivity.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('chronogram_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FDescription := FieldByName('description').AsString;
    if not FieldByName('start_date').IsNull then
      FStartDate := FieldByName('start_date').AsDateTime;
    if not FieldByName('target_date').IsNull then
      FTargetDate := FieldByName('target_date').AsDateTime;
    if not FieldByName('end_date').IsNull then
      FEndDate := FieldByName('end_date').AsDateTime;
    FGoalId := FieldByName('goal_id').AsInteger;
    case FieldByName('progress_status').AsString of
      'T': FStatus := astToDo;
      'P': FStatus := astInProgress;
      'F': FStatus := astDone;
      'C': FStatus := astCanceled;
      'D': FStatus := astDelayed;
      'R': FStatus := astNeedsReview;
      'B': FStatus := astBlocked;
    end;
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

procedure TProjectActivity.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TProjectActivity.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Start Date', FStartDate);
    JSONObject.Add('Target Date', FTargetDate);
    JSONObject.Add('End Date', FEndDate);
    JSONObject.Add('Goal ID', FGoalId);
    JSONObject.Add('Status', ACTIVITY_STATUSES[FStatus]);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TProjectActivity.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectActivity.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetForeignParam(ParamByName('project_id'), FProjectId);
      ParamByName('description').AsString := FDescription;
      SetDateParam(ParamByName('start_date'), FStartDate);
      SetDateParam(ParamByName('target_date'), FTargetDate);
      SetDateParam(ParamByName('end_date'), FEndDate);
      SetForeignParam(ParamByName('goal_id'), FGoalId);
      case FStatus of
        astToDo:        ParamByName('progress_status').AsString := 'T';
        astInProgress:  ParamByName('progress_status').AsString := 'P';
        astDone:        ParamByName('progress_status').AsString := 'F';
        astCanceled:    ParamByName('progress_status').AsString := 'C';
        astDelayed:     ParamByName('progress_status').AsString := 'D';
        astNeedsReview: ParamByName('progress_status').AsString := 'R';
        astBlocked:     ParamByName('progress_status').AsString := 'B';
      end;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('chronogram_id').AsInteger := FId;

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

{ TProjectRubric }

constructor TProjectRubric.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
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

procedure TProjectRubric.Copy(aFrom: TProjectRubric);
begin
  FProjectId := aFrom.ProjectId;
  FFundingSource := aFrom.FundingSource;
  FRubric := aFrom.Rubric;
  FItemName := aFrom.ItemName;
  FAmount := aFrom.Amount;
end;

procedure TProjectRubric.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectRubric.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM project_budgets');
      Add('WHERE (budget_id = :aid)');

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

function TProjectRubric.Diff(aOld: TProjectRubric; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFundingSource, aOld.FundingSource, FFundingSource, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRubric, aOld.Rubric, FRubric, R) then
    aList.Add(R);
  if FieldValuesDiff(rscItem, aOld.ItemName, FItemName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAmount, aOld.Amount, FAmount, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TProjectRubric.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectRubric.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectRubric.Insert;
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

      ParamByName('project_id').AsInteger := FProjectId;
      SetStrParam(ParamByName('funding_source'), FFundingSource);
      SetStrParam(ParamByName('rubric'), FRubric);
      SetStrParam(ParamByName('item_name'), FItemName);
      SetFloatParam(ParamByName('amount'), FAmount);
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

procedure TProjectRubric.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('budget_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FFundingSource := FieldByName('funding_source').AsString;
    FRubric := FieldByName('rubric').AsString;
    FItemName := FieldByName('item_name').AsString;
    FAmount := FieldByName('amount').AsFloat;
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

procedure TProjectRubric.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TProjectRubric.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project ID', FProjectId);
    JSONObject.Add('Funding Source', FFundingSource);
    JSONObject.Add('Rubric', FRubric);
    JSONObject.Add('Item', FItemName);
    JSONObject.Add('Amount', FAmount);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TProjectRubric.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectRubric.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('project_id').AsInteger := FProjectId;
      SetStrParam(ParamByName('funding_source'), FFundingSource);
      SetStrParam(ParamByName('rubric'), FRubric);
      SetStrParam(ParamByName('item_name'), FItemName);
      SetFloatParam(ParamByName('amount'), FAmount);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('budget_id').AsInteger := FId;

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

{ TProjectExpense }

constructor TProjectExpense.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
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

procedure TProjectExpense.Copy(aFrom: TProjectExpense);
begin
  FProjectId := aFrom.ProjectId;
  FBudgetId := aFrom.BudgetId;
  FDescription := aFrom.Description;
  FExpenseDate := aFrom.ExpenseDate;
  FAmount := aFrom.Amount;
end;

procedure TProjectExpense.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectExpense.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM project_expenses');
      Add('WHERE (expense_id = :aid)');

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

function TProjectExpense.Diff(aOld: TProjectExpense; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscBudgetID, aOld.BudgetId, FBudgetId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.ExpenseDate, FExpenseDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAmount, aOld.Amount, FAmount, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TProjectExpense.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TProjectExpense.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TProjectExpense.Insert;
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

      ParamByName('project_id').AsInteger := FProjectId;
      SetForeignParam(ParamByName('budget_id'), FBudgetId);
      SetStrParam(ParamByName('item_description'), FDescription);
      SetDateParam(ParamByName('expense_date'), FExpenseDate);
      SetFloatParam(ParamByName('amount'), FAmount);
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

procedure TProjectExpense.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('expense_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FBudgetId := FieldByName('budget_id').AsInteger;
    FDescription := FieldByName('item_description').AsString;
    if not FieldByName('expense_date').IsNull then
      FExpenseDate := FieldByName('expense_date').AsDateTime;
    FAmount := FieldByName('amount').AsFloat;
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

procedure TProjectExpense.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TProjectExpense.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Project ID', FProjectId);
    JSONObject.Add('Budget ID', FBudgetId);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Date', FExpenseDate);
    JSONObject.Add('Amount', FAmount);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TProjectExpense.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TProjectExpense.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      ParamByName('project_id').AsInteger := FProjectId;
      SetForeignParam(ParamByName('budget_id'), FBudgetId);
      SetStrParam(ParamByName('item_description'), FDescription);
      SetDateParam(ParamByName('expense_date'), FExpenseDate);
      SetFloatParam(ParamByName('amount'), FAmount);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('expense_id').AsInteger := FId;

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
