unit providers_projects;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TProjectsSQL }

  TProjectsSQL = class(TInterfacedObject, IProjectsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectDateTree(Grouped: Boolean): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  { TProjectTeamsSQL }

  TProjectTeamsSQL = class(TInterfacedObject, IProjectTeamsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

  { TProjectGoalsSQL }

  TProjectGoalsSQL = class(TInterfacedObject, IProjectGoalsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  { TProjectChronogramsSQL }

  TProjectChronogramsSQL = class(TInterfacedObject, IProjectChronogramsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

  { TProjectBudgetsSQL }

  TProjectBudgetsSQL = class(TInterfacedObject, IProjectBudgetsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  { TProjectExpensesSQL }

  TProjectExpensesSQL = class(TInterfacedObject, IProjectExpensesSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

implementation

{ TProjectsSQL }

constructor TProjectsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TProjectsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS projects (' +
      'project_id       INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'project_title    VARCHAR (150) NOT NULL UNIQUE,' +
      'short_title      VARCHAR (60),' +
      'start_date       DATE,' +
      'end_date         DATE,' +
      'website_uri      VARCHAR (200),' +
      'email_addr       VARCHAR (100),' +
      'contact_name     VARCHAR (100),' +
      'protocol_number  VARCHAR (30),' +
      'project_status VARCHAR (5),' +
      'main_goal        TEXT,' +
      'risks            TEXT,' +
      'project_abstract TEXT,' +
      'notes            TEXT,' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN       DEFAULT (0),' +
      'marked_status    BOOLEAN       DEFAULT (0),' +
      'active_status    BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TProjectsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM projects ' +
    'WHERE project_id = :aid';
end;

function TProjectsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT project_id, project_title, short_title FROM projects ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((project_title ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
            'OR (short_title ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM)) ' +
          'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
  end;
end;

function TProjectsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO projects (' +
      'project_title, ' +
      'short_title, ' +
      'start_date, ' +
      'end_date, ' +
      'website_uri, ' +
      'email_addr, ' +
      'contact_name, ' +
      'protocol_number, ' +
      'project_status, ' +
      'main_goal, ' +
      'risks, ' +
      'project_abstract, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_title, ' +
      ':short_title, ' +
      'date(:start_date), ' +
      'date(:end_date), ' +
      ':website_uri, ' +
      ':email_addr, ' +
      ':contact_name, ' +
      ':protocol_number, ' +
      ':project_status, ' +
      ':main_goal, ' +
      ':risks, ' +
      ':project_abstract, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TProjectsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM projects ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (project_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (project_id = :project_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (project_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TProjectsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', pj.start_date) AS ano, ' +
      'strftime(''%m'', pj.start_date) AS mes, ' +
      'strftime(''%d'', pj.start_date) AS dia ' +
    'FROM projects AS pj ' +
    'WHERE (pj.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'strftime(''%Y'', pj.end_date) AS ano, ' +
      'strftime(''%m'', pj.end_date) AS mes, ' +
      'strftime(''%d'', pj.end_date) AS dia ' +
    'FROM projects AS pj ' +
    'WHERE (pj.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TProjectsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'project_id, ' +
      'project_title, ' +
      'short_title, ' +
      'start_date, ' +
      'end_date, ' +
      'website_uri, ' +
      'email_addr, ' +
      'contact_name, ' +
      'protocol_number, ' +
      'project_status, ' +
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
    'FROM projects ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (project_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (project_id = :project_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (project_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TProjectsSQL.Update: String;
begin
  Result :=
    'UPDATE projects SET ' +
      'project_title = :project_title, ' +
      'short_title = :short_title, ' +
      'start_date = date(:start_date), ' +
      'end_date = date(:end_date), ' +
      'website_uri = :website_uri, ' +
      'email_addr = :email_addr, ' +
      'contact_name = :contact_name, ' +
      'protocol_number = :protocol_number, ' +
      'project_status = :project_status, ' +
      'main_goal = :main_goal, ' +
      'risks = :risks, ' +
      'project_abstract = :project_abstract, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (project_id = :project_id) ';
end;

{ TProjectTeamsSQL }

constructor TProjectTeamsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TProjectTeamsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS project_team (' +
      'project_member_id INTEGER  UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'project_id        INTEGER  REFERENCES projects (project_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'person_id         INTEGER  NOT NULL REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'project_manager   BOOLEAN  DEFAULT (0),' +
      'institution_id    INTEGER  REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
      'user_inserted     INTEGER,' +
      'user_updated      INTEGER,' +
      'insert_date       DATETIME,' +
      'update_date       DATETIME,' +
      'exported_status   BOOLEAN  DEFAULT (0),' +
      'marked_status     BOOLEAN  DEFAULT (0),' +
      'active_status     BOOLEAN  DEFAULT (1)' +
    ');';
end;

function TProjectTeamsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM project_team ' +
    'WHERE project_member_id = :aid';
end;

function TProjectTeamsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO project_team (' +
      'project_id, ' +
      'person_id, ' +
      'project_manager, ' +
      'institution_id, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_id, ' +
      ':person_id, ' +
      ':project_manager, ' +
      ':institution_id, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec'')) ';
end;

function TProjectTeamsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT pt.*, ' +
      'p.abbreviation AS person_abbrev, ' +
      'p.full_name AS person_name, ' +
      'p.profile_color AS person_color, ' +
      'it.abbreviation AS institution_name ' +
    'FROM project_team AS pt ' +
    'LEFT JOIN people AS p ON pt.person_id = p.person_id ' +
    'LEFT JOIN institutions AS it ON pt.institition_id = it.institution_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (pt.project_member_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (pt.project_member_id = :project_member_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (pt.project_member_id = -1) AND (pt.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (pt.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (pt.active_status = 1) AND (pt.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (pt.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (pt.active_status = 1) AND (pt.project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectTeamsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
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
    'FROM project_team ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (project_member_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (project_member_id = :project_member_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (project_member_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectTeamsSQL.Update: String;
begin
  Result :=
    'UPDATE project_team SET ' +
      'project_id = :project_id, ' +
      'person_id = :person_id, ' +
      'project_manager = :project_manager, ' +
      'institution_id = :institution_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (project_member_id = :project_member_id) ';
end;

{ TProjectGoalsSQL }

constructor TProjectGoalsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TProjectGoalsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS project_goals (' +
      'goal_id          INTEGER     PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'project_id       INTEGER     REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'goal_description TEXT,' +
      'goal_status      VARCHAR (5),' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN     DEFAULT (0),' +
      'marked_status    BOOLEAN     DEFAULT (0),' +
      'active_status    BOOLEAN     DEFAULT (1)' +
    ');';
end;

function TProjectGoalsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM project_goals ' +
    'WHERE goal_id = :aid';
end;

function TProjectGoalsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT goal_id, goal_description FROM project_goals ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (goal_description ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
          'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
  end;
end;

function TProjectGoalsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO project_goals (' +
      'project_id, ' +
      'goal_description, ' +
      'goal_status, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_id, ' +
      ':goal_description, ' +
      ':goal_status, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TProjectGoalsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM project_goals ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (goal_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (goal_id = :goal_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (goal_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectGoalsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
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
    'FROM project_goals ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (goal_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (goal_id = :goal_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (goal_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectGoalsSQL.Update: String;
begin
  Result :=
    'UPDATE project_goals SET ' +
      'project_id = :project_id, ' +
      'goal_description = :goal_description, ' +
      'goal_status = :goal_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (goal_id = :goal_id) ';
end;

{ TProjectChronogramsSQL }

constructor TProjectChronogramsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TProjectChronogramsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS project_chronograms (' +
      'chronogram_id   INTEGER     PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'project_id      INTEGER     REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'description     TEXT,' +
      'start_date      DATE,' +
      'target_date     DATE,' +
      'end_date        DATE,' +
      'goal_id         INTEGER     REFERENCES project_goals (goal_id) ON UPDATE CASCADE,' +
      'progress_status VARCHAR (5),' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN     DEFAULT (0),' +
      'marked_status   BOOLEAN     DEFAULT (0),' +
      'active_status   BOOLEAN     DEFAULT (1)' +
    ');';
end;

function TProjectChronogramsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM project_chronograms ' +
    'WHERE chronogram_id = :aid';
end;

function TProjectChronogramsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO project_chronograms (' +
      'project_id, ' +
      'description, ' +
      'start_date, ' +
      'target_date, ' +
      'end_date, ' +
      'goal_id, ' +
      'progress_status, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_id, ' +
      ':description, ' +
      'date(:start_date), ' +
      'date(:target_date), ' +
      'date(:end_date), ' +
      ':goal_id, ' +
      ':progress_status, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TProjectChronogramsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM project_chronograms ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (chronogram_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (chronogram_id = :chronogram_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (chronogram_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectChronogramsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
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
    'FROM project_chronograms ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (chronogram_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (chronogram_id = :chronogram_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (chronogram_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectChronogramsSQL.Update: String;
begin
  Result :=
    'UPDATE project_chronograms SET ' +
      'project_id = :project_id, ' +
      'description = :description, ' +
      'start_date = date(:start_date), ' +
      'target_date = date(:target_date), ' +
      'end_date = date(:end_date), ' +
      'goal_id = :goal_id, ' +
      'progress_status = :progress_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (chronogram_id = :chronogram_id) ';
end;

{ TProjectBudgetsSQL }

constructor TProjectBudgetsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TProjectBudgetsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS project_budgets (' +
      'budget_id       INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'project_id      INTEGER      REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'funding_source  VARCHAR (60),' +
      'rubric          VARCHAR (60) NOT NULL,' +
      'item_name       VARCHAR (60),' +
      'amount          REAL,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN      DEFAULT (0),' +
      'marked_status   BOOLEAN      DEFAULT (0),' +
      'active_status   BOOLEAN      DEFAULT (1)' +
    ');';
end;

function TProjectBudgetsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM project_budgets ' +
    'WHERE budget_id = :aid';
end;

function TProjectBudgetsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result :=
    'SELECT budget_id, ' +
      'CASE ' +
        'WHEN TRIM(COALESCE(item_name, '''')) = '''' THEN rubric ' +
        'ELSE rubric || '': '' || item_name ' +
      'END AS rubric_item ' +
    'FROM project_budgets ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (rubric_item ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
          'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
  end;
end;

function TProjectBudgetsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO project_budgets (' +
      'project_id, ' +
      'funding_source, ' +
      'rubric, ' +
      'item_name, ' +
      'amount, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_id, ' +
      ':funding_source, ' +
      ':rubric, ' +
      ':item_name, ' +
      ':amount, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TProjectBudgetsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM project_budgets ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (budget_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (budget_id = :budget_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (budget_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectBudgetsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
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
    'FROM project_budgets ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (budget_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (budget_id = :budget_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (budget_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectBudgetsSQL.Update: String;
begin
  Result :=
    'UPDATE project_budgets SET ' +
      'project_id = :project_id, ' +
      'funding_source = :funding_source, ' +
      'rubric = :rubric, ' +
      'item_name = :item_name, ' +
      'amount = :amount, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (budget_id = :budget_id) ';
end;

{ TProjectExpensesSQL }

constructor TProjectExpensesSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TProjectExpensesSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS project_expenses (' +
      'expense_id       INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'project_id       INTEGER      REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'budget_id        INTEGER      REFERENCES project_budgets (budget_id) ON UPDATE CASCADE,' +
      'item_description VARCHAR (60) NOT NULL,' +
      'expense_date     DATE,' +
      'amount           REAL,' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN      DEFAULT (0),' +
      'marked_status    BOOLEAN      DEFAULT (0),' +
      'active_status    BOOLEAN      DEFAULT (1)' +
    ');';
end;

function TProjectExpensesSQL.Delete: String;
begin
  Result :=
    'DELETE FROM project_expenses ' +
    'WHERE expense_id = :aid';
end;

function TProjectExpensesSQL.Insert: String;
begin
  Result :=
    'INSERT INTO project_expenses (' +
      'project_id, ' +
      'budget_id, ' +
      'item_description, ' +
      'expense_date, ' +
      'amount, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_id, ' +
      ':budget_id, ' +
      ':item_description, ' +
      'date(:expense_date), ' +
      ':amount, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TProjectExpensesSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM project_expenses ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (expense_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (expense_id = :expense_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (expense_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectExpensesSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
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
    'FROM project_expenses ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (expense_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (expense_id = :expense_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (expense_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (project_id = :project_id) ';
    swcFindText: ;
  end;
end;

function TProjectExpensesSQL.Update: String;
begin
  Result :=
    'UPDATE project_expenses SET ' +
      'project_id = :project_id, ' +
      'budget_id = :budget_id, ' +
      'item_description = :item_description, ' +
      'expense_date = date(:expense_date), ' +
      'amount = :amount, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (expense_id = :expense_id) ';
end;

end.

