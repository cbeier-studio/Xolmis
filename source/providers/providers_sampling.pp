unit providers_sampling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TExpeditionsSQL }

  TExpeditionsSQL = class(TInterfacedObject, IExpeditionsSQL)
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

  { TSurveysSQL }

  TSurveysSQL = class(TInterfacedObject, ISurveysSQL)
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

  { TSurveyTeamsSQL }

  TSurveyTeamsSQL = class(TInterfacedObject, ISurveyTeamsSQL)
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

  { TNetsEffortSQL }

  TNetsEffortSQL = class(TInterfacedObject, INetsEffortSQL)
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

  { TVegetationsSQL }

  TVegetationsSQL = class(TInterfacedObject, IVegetationsSQL)
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

  { TWeatherLogsSQL }

  TWeatherLogsSQL = class(TInterfacedObject, IWeatherLogsSQL)
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

{ TExpeditionsSQL }

constructor TExpeditionsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TExpeditionsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS expeditions (' +
      'expedition_id   INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'expedition_name VARCHAR (150) NOT NULL,' +
      'start_date      DATE,' +
      'end_date        DATE,' +
      'duration        INTEGER       AS ( (strftime(''%j'', end_date) - strftime(''%j'', start_date) ) + 1) VIRTUAL,' +
      'project_id      INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'description     TEXT,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN       DEFAULT (0),' +
      'marked_status   BOOLEAN       DEFAULT (0),' +
      'active_status   BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TExpeditionsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM expeditions ' +
    'WHERE expedition_id = :aid';
end;

function TExpeditionsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT expedition_id, expedition_name FROM expeditions ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (expedition_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
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

function TExpeditionsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO expeditions (' +
      'expedition_name, ' +
      'start_date, ' +
      'end_date, ' +
      'project_id, ' +
      'description, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':expedition_name, ' +
      'date(:start_date), ' +
      'date(:end_date), ' +
      ':project_id, ' +
      ':description, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TExpeditionsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT x.*, ' +
      'pj.short_title AS project_name ' +
    'FROM expeditions AS x ' +
    'LEFT JOIN projects AS pj ON x.project_id = pj.project_id';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (x.expedition_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (x.expedition_id = :expedition_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (x.expedition_id = -1) AND (x.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (x.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (x.active_status = 1) AND (x.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (x.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TExpeditionsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', x.start_date) AS ano, ' +
      'strftime(''%m'', x.start_date) AS mes, ' +
      'strftime(''%d'', x.start_date) AS dia ' +
    'FROM expeditions AS x ' +
    'WHERE (x.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'strftime(''%Y'', x.end_date) AS ano, ' +
      'strftime(''%m'', x.end_date) AS mes, ' +
      'strftime(''%d'', x.end_date) AS dia ' +
    'FROM expeditions AS x ' +
    'WHERE (x.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TExpeditionsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'expedition_id, ' +
      'expedition_name, ' +
      'start_date, ' +
      'end_date, ' +
      'duration, ' +
      'project_id, ' +
      'description, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM expeditions';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (expedition_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (expedition_id = :expedition_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (expedition_id = -1) AND (active_status = 1) ';
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

function TExpeditionsSQL.Update: String;
begin
  Result :=
    'UPDATE expeditions SET ' +
      'expedition_name = :expedition_name, ' +
      'start_date = date(:start_date), ' +
      'end_date = date(:end_date), ' +
      'project_id = :project_id, ' +
      'description = :description, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (expedition_id = :expedition_id) ';
end;

{ TSurveysSQL }

constructor TSurveysSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSurveysSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS surveys (' +
      'survey_id                INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'survey_date              DATE          NOT NULL,' +
      'start_time               TIME,' +
      'end_time                 TIME,' +
      'duration                 INTEGER,' +
      'method_id                INTEGER       REFERENCES methods (method_id) ON UPDATE CASCADE,' +
      'net_station_id           INTEGER       REFERENCES sampling_plots (sampling_plot_id) ON UPDATE CASCADE,' +
      'expedition_id            INTEGER       REFERENCES expeditions (expedition_id) ON UPDATE CASCADE,' +
      'project_id               INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'locality_id              INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'sample_id                VARCHAR (30),' +
      'start_latitude           REAL,' +
      'start_longitude          REAL,' +
      'end_latitude             REAL,' +
      'end_longitude            REAL,' +
      'coordinate_precision     VARCHAR (3),' +
      'observers_tally          INTEGER,' +
      'area_total               REAL,' +
      'distance_total           REAL,' +
      'nets_total               INTEGER,' +
      'habitat                  TEXT,' +
      'net_rounds               TEXT,' +
      'full_name                VARCHAR (100),' +
      'notes                    TEXT,' +
      'user_inserted            INTEGER,' +
      'user_updated             INTEGER,' +
      'insert_date              DATETIME,' +
      'update_date              DATETIME,' +
      'exported_status          BOOLEAN       DEFAULT (0),' +
      'marked_status            BOOLEAN       DEFAULT (0),' +
      'active_status            BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TSurveysSQL.Delete: String;
begin
  Result :=
    'DELETE FROM surveys ' +
    'WHERE survey_id = :aid';
end;

function TSurveysSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT survey_id, full_name FROM surveys ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
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

function TSurveysSQL.Insert: String;
begin
  Result :=
    'INSERT INTO surveys (' +
        'survey_date, ' +
        'start_time, ' +
        'end_time, ' +
        'duration, ' +
        'method_id, ' +
        'net_station_id, ' +
        'expedition_id, ' +
        'project_id, ' +
        'locality_id, ' +
        'sample_id, ' +
        'start_latitude, ' +
        'start_longitude, ' +
        'end_latitude, ' +
        'end_longitude, ' +
        'coordinate_precision, ' +
        'observers_tally, ' +
        'area_total, ' +
        'distance_total, ' +
        'nets_total, ' +
        'habitat, ' +
        'net_rounds, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ' +
    'VALUES (' +
        'date(:survey_date), ' +
        'time(:start_time), ' +
        'time(:end_time), ' +
        ':duration, ' +
        ':method_id, ' +
        ':net_station_id, ' +
        ':expedition_id, ' +
        ':project_id, ' +
        ':locality_id, ' +
        ':sample_id, ' +
        ':start_latitude, ' +
        ':start_longitude, ' +
        ':end_latitude, ' +
        ':end_longitude, ' +
        ':coordinate_precision, ' +
        ':observers_tally, ' +
        ':area_total, ' +
        ':distance_total, ' +
        ':nets_total, ' +
        ':habitat, ' +
        ':net_rounds, ' +
        ':full_name, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''));';
end;

function TSurveysSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT sv.*, ' +
      'x.expedition_name AS expedition_name, ' +
      'gl.full_name AS locality_name, ' +
      'gl.country_id AS country_id, ' +
      'gl.state_id AS state_id, ' +
      'gl.municipality_id AS municipality_id, ' +
      'gm.site_name AS municipality_name, ' +
      'gs.site_name AS state_name, ' +
      'gc.site_name AS country_name, ' +
      'pl.full_name AS net_station_name, ' +
      'mt.method_name AS method_name, ' +
      'pj.short_title AS project_name, ' +
      'CAST(COALESCE(ne.net_effort, 0) AS REAL) AS net_effort ' +
    'FROM surveys AS sv ' +
    'LEFT JOIN expeditions AS x ON sv.expedition_id = x.expedition_id ' +
    'LEFT JOIN gazetteer AS gl ON sv.locality_id = gl.site_id ' +
    'LEFT JOIN gazetteer AS gm ON gl.municipality_id = gm.site_id ' +
    'LEFT JOIN gazetteer AS gs ON gl.state_id = gs.site_id ' +
    'LEFT JOIN gazetteer AS gc ON gl.country_id = gc.site_id ' +
    'LEFT JOIN sampling_plots AS pl ON sv.net_station_id = pl.sampling_plot_id ' +
    'LEFT JOIN methods AS mt ON sv.method_id = mt.method_id ' +
    'LEFT JOIN projects AS pj ON sv.project_id = pj.project_id ' +
    'LEFT JOIN ( ' +
      'SELECT ef.survey_id, SUM(ef.net_area * ef.open_time_total) AS net_effort ' +
      'FROM nets_effort AS ef ' +
      'WHERE ef.active_status = 1 ' +
      'GROUP BY ef.survey_id ' +
    ') AS ne ON sv.survey_id = ne.survey_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (sv.survey_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (sv.survey_id = :survey_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (sv.survey_id = -1) AND (sv.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (sv.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (sv.active_status = 1) AND (sv.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (sv.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (sv.active_status = 1) AND (sv.expedition_id = :expedition_id) ';
    swcFindText: ;
  end;
end;

function TSurveysSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', sv.survey_date) AS ano, ' +
      'strftime(''%m'', sv.survey_date) AS mes, ' +
      'strftime(''%d'', sv.survey_date) AS dia ' +
    'FROM surveys AS sv ' +
    'WHERE (sv.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TSurveysSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'survey_id, ' +
      'survey_date, ' +
      'start_time, ' +
      'end_time, ' +
      'duration, ' +
      'method_id, ' +
      'net_station_id, ' +
      'expedition_id, ' +
      'project_id, ' +
      'locality_id, ' +
      'sample_id, ' +
      'start_latitude, ' +
      'start_longitude, ' +
      'end_latitude, ' +
      'end_longitude, ' +
      'coordinate_precision, ' +
      'observers_tally, ' +
      'area_total, ' +
      'distance_total, ' +
      'nets_total, ' +
      'habitat, ' +
      'net_rounds, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM surveys ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (survey_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (survey_id = :survey_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (survey_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (expedition_id = :expedition_id) ';
    swcFindText: ;
  end;
end;

function TSurveysSQL.Update: String;
begin
  Result :=
    'UPDATE surveys SET ' +
      'survey_date = date(:survey_date), ' +
      'start_time = time(:start_time), ' +
      'end_time = time(:end_time), ' +
      'duration = :duration, ' +
      'method_id = :method_id, ' +
      'net_station_id = :net_station_id, ' +
      'expedition_id = :expedition_id, ' +
      'project_id = :project_id, ' +
      'locality_id = :locality_id, ' +
      'sample_id = :sample_id, ' +
      'start_latitude = :start_latitude, ' +
      'start_longitude = :start_longitude, ' +
      'end_latitude = :end_latitude, ' +
      'end_longitude = :end_longitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'observers_tally = :observers_tally, ' +
      'area_total = :area_total, ' +
      'distance_total = :distance_total, ' +
      'nets_total = :nets_total, ' +
      'habitat = :habitat, ' +
      'net_rounds = :net_rounds, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'exported_status = :exported_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (survey_id = :survey_id) ';
end;

{ TSurveyTeamsSQL }

constructor TSurveyTeamsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSurveyTeamsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS survey_team (' +
      'survey_member_id INTEGER  UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'survey_id        INTEGER,' +
      'person_id        INTEGER  NOT NULL REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'visitor          BOOLEAN  DEFAULT (0),' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN  DEFAULT (0),' +
      'marked_status    BOOLEAN  DEFAULT (0),' +
      'active_status    BOOLEAN  DEFAULT (1)' +
    ');';
end;

function TSurveyTeamsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM survey_team ' +
    'WHERE survey_member_id = :aid';
end;

function TSurveyTeamsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO survey_team (' +
      'survey_id, ' +
      'person_id, ' +
      'visitor, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':survey_id, ' +
      ':person_id, ' +
      ':visitor, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TSurveyTeamsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT st.*, ' +
      'p.abbreviation AS person_abbrev, ' +
      'p.full_name AS person_name, ' +
      'p.profile_color AS person_color ' +
    'FROM survey_team AS st ' +
    'LEFT JOIN people AS p ON st.person_id = p.person_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (st.survey_member_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (st.survey_member_id = :survey_member_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (st.survey_member_id = -1) AND (st.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (st.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (st.active_status = 1) AND (st.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (st.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (st.active_status = 1) AND (st.survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TSurveyTeamsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'survey_member_id, ' +
      'survey_id, ' +
      'person_id, ' +
      'visitor, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM survey_team ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (survey_member_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (survey_member_id = :survey_member_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (survey_member_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TSurveyTeamsSQL.Update: String;
begin
  Result :=
    'UPDATE survey_team SET ' +
      'survey_id = :survey_id, ' +
      'person_id = :person_id, ' +
      'visitor = :visitor, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec''),' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (survey_member_id = :survey_member_id) ';
end;

{ TNetsEffortSQL }

constructor TNetsEffortSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TNetsEffortSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS nets_effort (' +
      'net_id           INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'survey_id        INTEGER,' +
      'net_station_id   INTEGER      REFERENCES sampling_plots (sampling_plot_id) ON UPDATE CASCADE,' +
      'permanent_net_id INTEGER      REFERENCES permanent_nets (permanent_net_id) ON UPDATE CASCADE,' +
      'net_number       INTEGER,' +
      'longitude        REAL,' +
      'latitude         REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'sample_date      DATE,' +
      'net_open_1       TIME,' +
      'net_close_1      TIME,' +
      'net_open_2       TIME,' +
      'net_close_2      TIME,' +
      'net_open_3       TIME,' +
      'net_close_3      TIME,' +
      'net_open_4       TIME,' +
      'net_close_4      TIME,' +
      'open_time_total  REAL         AS (CAST ( (strftime(''%s'', ifnull(net_close_1, 0) ) - strftime(''%s'', ifnull(net_open_1, 0) ) ) AS REAL) / 60 / 60 + CAST ( (strftime(''%s'', ifnull(net_close_2, 0) ) - strftime(''%s'', ifnull(net_open_2, 0) ) ) AS REAL) / 60 / 60 + CAST ( (strftime(''%s'', ifnull(net_close_3, 0) ) - strftime(''%s'', ifnull(net_open_3, 0) ) ) AS REAL) / 60 / 60 + CAST ( (strftime(''%s'', ifnull(net_close_4, 0) ) - strftime(''%s'', ifnull(net_open_4, 0) ) ) AS REAL) / 60 / 60) STORED,' +
      'net_length       REAL,' +
      'net_height       REAL,' +
      'net_area         REAL         AS (ifnull(net_length, 0) * ifnull(net_height, 0) ) STORED,' +
      'net_mesh         INTEGER,' +
      'full_name        VARCHAR (40),' +
      'notes            TEXT,' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN      DEFAULT (0),' +
      'marked_status    BOOLEAN      DEFAULT (0),' +
      'active_status    BOOLEAN      DEFAULT (1)' +
    ');';
end;

function TNetsEffortSQL.Delete: String;
begin
  Result :=
    'DELETE FROM nets_effort ' +
    'WHERE net_id = :aid';
end;

function TNetsEffortSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT net_id, full_name, net_number FROM nets_effort ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
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

function TNetsEffortSQL.Insert: String;
begin
  Result :=
    'INSERT INTO nets_effort (' +
      'survey_id, ' +
      'net_station_id, ' +
      'permanent_net_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'sample_date, ' +
      'net_open_1, ' +
      'net_close_1, ' +
      'net_open_2, ' +
      'net_close_2, ' +
      'net_open_3, ' +
      'net_close_3, ' +
      'net_open_4, ' +
      'net_close_4, ' +
      'net_length, ' +
      'net_height, ' +
      'net_mesh, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':survey_id, ' +
      ':net_station_id, ' +
      ':permanent_net_id, ' +
      ':net_number, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      'date(:sample_date), ' +
      'time(:net_open_1), ' +
      'time(:net_close_1), ' +
      'time(:net_open_2), ' +
      'time(:net_close_2), ' +
      'time(:net_open_3), ' +
      'time(:net_close_3), ' +
      'time(:net_open_4), ' +
      'time(:net_close_4), ' +
      ':net_length, ' +
      ':net_height, ' +
      ':net_mesh, ' +
      ':full_name, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));';
end;

function TNetsEffortSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM nets_effort ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (net_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (net_id = :net_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (net_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TNetsEffortSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'net_id, ' +
      'survey_id, ' +
      'net_station_id, ' +
      'permanent_net_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'sample_date, ' +
      'net_open_1, ' +
      'net_close_1, ' +
      'net_open_2, ' +
      'net_close_2, ' +
      'net_open_3, ' +
      'net_close_3, ' +
      'net_open_4, ' +
      'net_close_4, ' +
      'open_time_total, ' +
      'net_length, ' +
      'net_height, ' +
      'net_area, ' +
      'net_mesh, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM nets_effort ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (net_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (net_id = :net_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (net_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TNetsEffortSQL.Update: String;
begin
  Result :=
    'UPDATE nets_effort SET ' +
      'survey_id = :survey_id, ' +
      'net_station_id = :net_station_id, ' +
      'permanent_net_id = :permanent_net_id, ' +
      'net_number = :net_number, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'sample_date = :sample_date, ' +
      'net_open_1 = :net_open_1, ' +
      'net_close_1 = :net_close_1, ' +
      'net_open_2 = :net_open_2, ' +
      'net_close_2 = :net_close_2, ' +
      'net_open_3 = :net_open_3, ' +
      'net_close_3 = :net_close_3, ' +
      'net_open_4 = :net_open_4, ' +
      'net_close_4 = :net_close_4, ' +
      'net_length = :net_length, ' +
      'net_height = :net_height, ' +
      'net_mesh = :net_mesh, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (net_id = :net_id) ';
end;

{ TVegetationsSQL }

constructor TVegetationsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TVegetationsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS vegetation (' +
      'vegetation_id       INTEGER  PRIMARY KEY AUTOINCREMENT NOT NULL UNIQUE,' +
      'survey_id           INTEGER,' +
      'sample_date         DATE     NOT NULL,' +
      'sample_time         TIME,' +
      'longitude           REAL,' +
      'latitude            REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'observer_id         INTEGER,' +
      'herbs_proportion    INTEGER,' +
      'herbs_distribution  INTEGER,' +
      'herbs_avg_height    INTEGER,' +
      'shrubs_proportion   INTEGER,' +
      'shrubs_distribution INTEGER,' +
      'shrubs_avg_height   INTEGER,' +
      'trees_proportion    INTEGER,' +
      'trees_distribution  INTEGER,' +
      'trees_avg_height    INTEGER,' +
      'notes               TEXT,' +
      'user_inserted       INTEGER,' +
      'user_updated        INTEGER,' +
      'insert_date         DATETIME,' +
      'update_date         DATETIME,' +
      'exported_status     BOOLEAN  DEFAULT (0),' +
      'marked_status       BOOLEAN  DEFAULT (0),' +
      'active_status       BOOLEAN  DEFAULT (1)' +
    ');';
end;

function TVegetationsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM vegetation ' +
    'WHERE vegetation_id = :aid';
end;

function TVegetationsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO vegetation (' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'observer_id, ' +
      'herbs_proportion, ' +
      'herbs_distribution, ' +
      'herbs_avg_height, ' +
      'shrubs_proportion, ' +
      'shrubs_distribution, ' +
      'shrubs_avg_height, ' +
      'trees_proportion, ' +
      'trees_distribution, ' +
      'trees_avg_height, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':survey_id, ' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':observer_id, ' +
      ':herbs_proportion, ' +
      ':herbs_distribution, ' +
      ':herbs_avg_height, ' +
      ':shrubs_proportion, ' +
      ':shrubs_distribution, ' +
      ':shrubs_avg_height, ' +
      ':trees_proportion, ' +
      ':trees_distribution, ' +
      ':trees_avg_height, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TVegetationsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM vegetation ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (vegetation_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (vegetation_id = :vegetation_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (vegetation_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TVegetationsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'vegetation_id, ' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'observer_id, ' +
      'herbs_proportion, ' +
      'herbs_distribution, ' +
      'herbs_avg_height, ' +
      'shrubs_proportion, ' +
      'shrubs_distribution, ' +
      'shrubs_avg_height, ' +
      'trees_proportion, ' +
      'trees_distribution, ' +
      'trees_avg_height, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM vegetation ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (vegetation_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (vegetation_id = :vegetation_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (vegetation_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TVegetationsSQL.Update: String;
begin
  Result :=
    'UPDATE vegetation SET ' +
      'survey_id = :survey_id, ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'observer_id = :observer_id, ' +
      'herbs_proportion = :herbs_proportion, ' +
      'herbs_distribution = :herbs_distribution, ' +
      'herbs_avg_height = :herbs_avg_height, ' +
      'shrubs_proportion = :shrubs_proportion, ' +
      'shrubs_distribution = :shrubs_distribution, ' +
      'shrubs_avg_height = :shrubs_avg_height, ' +
      'trees_proportion = :trees_proportion, ' +
      'trees_distribution = :trees_distribution, ' +
      'trees_avg_height = :trees_avg_height, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (vegetation_id = :vegetation_id) ';
end;

{ TWeatherLogsSQL }

constructor TWeatherLogsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TWeatherLogsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS weather_logs (' +
      'weather_id           INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'survey_id            INTEGER,' +
      'sample_date          DATE     NOT NULL,' +
      'sample_time          TIME,' +
      'sample_moment        CHAR (1),' +
      'observer_id          INTEGER,' +
      'cloud_cover          INTEGER,' +
      'precipitation        CHAR (1),' +
      'rainfall             INTEGER,' +
      'temperature          REAL,' +
      'wind_speed_bft       INTEGER,' +
      'wind_speed_kmh       REAL,' +
      'wind_direction       VARCHAR (5),' +
      'relative_humidity    REAL,' +
      'atmospheric_pressure REAL,' +
      'notes                TEXT,' +
      'user_inserted        INTEGER,' +
      'user_updated         INTEGER,' +
      'insert_date          DATETIME,' +
      'update_date          DATETIME,' +
      'exported_status      BOOLEAN  DEFAULT (0),' +
      'marked_status        BOOLEAN  DEFAULT (0),' +
      'active_status        BOOLEAN  DEFAULT (1)' +
    ');';
end;

function TWeatherLogsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM weather_logs ' +
    'WHERE weather_id = :aid';
end;

function TWeatherLogsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO weather_logs (' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'sample_moment, ' +
      'observer_id, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'rainfall, ' +
      'temperature, ' +
      'wind_speed_bft, ' +
      'wind_speed_kmh, ' +
      'wind_direction, ' +
      'relative_humidity, ' +
      'atmospheric_pressure, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':survey_id, ' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':sample_moment, ' +
      ':observer_id, ' +
      ':cloud_cover, ' +
      ':precipitation, ' +
      ':rainfall, ' +
      ':temperature, ' +
      ':wind_speed_bft, ' +
      ':wind_speed_kmh, ' +
      ':wind_direction, ' +
      ':relative_humidity, ' +
      ':atmospheric_pressure, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TWeatherLogsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM weather_logs ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (weather_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (weather_id = :weather_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (weather_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TWeatherLogsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'weather_id, ' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'sample_moment, ' +
      'observer_id, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'rainfall, ' +
      'temperature, ' +
      'wind_speed_bft, ' +
      'wind_speed_kmh, ' +
      'wind_direction, ' +
      'relative_humidity, ' +
      'atmospheric_pressure, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM weather_logs ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (weather_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (weather_id = :weather_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (weather_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
    swcFindText: ;
  end;
end;

function TWeatherLogsSQL.Update: String;
begin
  Result :=
    'UPDATE weather_logs SET ' +
      'survey_id = :survey_id, ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'sample_moment = :sample_moment, ' +
      'observer_id = :observer_id, ' +
      'cloud_cover = :cloud_cover, ' +
      'precipitation = :precipitation, ' +
      'rainfall = :rainfall, ' +
      'temperature = :temperature, ' +
      'wind_speed_bft = :wind_speed_bft, ' +
      'wind_speed_kmh = :wind_speed_kmh, ' +
      'wind_direction = :wind_direction, ' +
      'relative_humidity = :relative_humidity, ' +
      'atmospheric_pressure = :atmospheric_pressure, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (weather_id = :weather_id) ';
end;

end.

