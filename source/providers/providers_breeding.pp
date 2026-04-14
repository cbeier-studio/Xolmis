unit providers_breeding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TNestsSQL }

  TNestsSQL = class(TInterfacedObject, INestsSQL)
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

  { TNestOwnersSQL }

  TNestOwnersSQL = class(TInterfacedObject, INestOwnersSQL)
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

  { TNestRevisionsSQL }

  TNestRevisionsSQL = class(TInterfacedObject, INestRevisionsSQL)
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

  { TEggsSQL }

  TEggsSQL = class(TInterfacedObject, IEggsSQL)
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

implementation

{ TNestsSQL }

constructor TNestsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TNestsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS nests (' +
      'nest_id               INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'field_number          VARCHAR (20)  UNIQUE NOT NULL,' +
      'observer_id           INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'project_id            INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'locality_id           INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'longitude             REAL,' +
      'latitude              REAL,' +
      'coordinate_precision  VARCHAR (3),' +
      'taxon_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'nest_shape            VARCHAR (5),' +
      'support_type          VARCHAR (10),' +
      'support_plant_1_id    INTEGER       REFERENCES botanic_taxa (taxon_id) ON UPDATE CASCADE,' +
      'support_plant_2_id    INTEGER       REFERENCES botanic_taxa (taxon_id) ON UPDATE CASCADE,' +
      'other_support         VARCHAR (60),' +
      'height_above_ground   REAL,' +
      'internal_max_diameter REAL,' +
      'internal_min_diameter REAL,' +
      'external_max_diameter REAL,' +
      'external_min_diameter REAL,' +
      'internal_height       REAL,' +
      'external_height       REAL,' +
      'edge_distance         REAL,' +
      'center_distance       REAL,' +
      'nest_cover            INTEGER,' +
      'plant_max_diameter    REAL,' +
      'plant_min_diameter    REAL,' +
      'plant_height          REAL,' +
      'plant_dbh             REAL,' +
      'construction_days     REAL,' +
      'incubation_days       REAL,' +
      'nestling_days         REAL,' +
      'active_days           REAL,' +
      'nest_fate             CHAR (1),' +
      'loss_cause            VARCHAR (5),' +
      'nest_productivity     INTEGER,' +
      'found_date            DATE,' +
      'last_date             DATE,' +
      'full_name             VARCHAR (100),' +
      'description           TEXT,' +
      'notes                 TEXT,' +
      'user_inserted         INTEGER,' +
      'user_updated          INTEGER,' +
      'insert_date           DATETIME,' +
      'update_date           DATETIME,' +
      'exported_status       BOOLEAN       DEFAULT (0),' +
      'marked_status         BOOLEAN       DEFAULT (0),' +
      'active_status         BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TNestsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM nests ' +
    'WHERE nest_id = :aid';
end;

function TNestsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT nest_id, full_name FROM nests ';

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

function TNestsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO nests (' +
      'field_number, ' +
      'observer_id, ' +
      'project_id, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'taxon_id, ' +
      'nest_shape, ' +
      'support_type, ' +
      'support_plant_1_id, ' +
      'support_plant_2_id, ' +
      'other_support, ' +
      'height_above_ground, ' +
      'internal_max_diameter, ' +
      'internal_min_diameter, ' +
      'external_max_diameter, ' +
      'external_min_diameter, ' +
      'internal_height, ' +
      'external_height, ' +
      'edge_distance, ' +
      'center_distance, ' +
      'nest_cover, ' +
      'plant_max_diameter, ' +
      'plant_min_diameter, ' +
      'plant_height, ' +
      'plant_dbh, ' +
      'nest_fate, ' +
      'loss_cause, ' +
      'nest_productivity, ' +
      'found_date, ' +
      'last_date, ' +
      'full_name, ' +
      'description, ' +
      'notes, ' +
      'construction_days, ' +
      'incubation_days, ' +
      'nestling_days, ' +
      'active_days, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':field_number, ' +
      ':observer_id, ' +
      ':project_id, ' +
      ':locality_id, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':taxon_id, ' +
      ':nest_shape, ' +
      ':support_type, ' +
      ':support_plant_1_id, ' +
      ':support_plant_2_id, ' +
      ':other_support, ' +
      ':height_above_ground, ' +
      ':internal_max_diameter, ' +
      ':internal_min_diameter, ' +
      ':external_max_diameter, ' +
      ':external_min_diameter, ' +
      ':internal_height, ' +
      ':external_height, ' +
      ':edge_distance, ' +
      ':center_distance, ' +
      ':nest_cover, ' +
      ':plant_max_diameter, ' +
      ':plant_min_diameter, ' +
      ':plant_height, ' +
      ':plant_dbh, ' +
      ':nest_fate, ' +
      ':loss_cause, ' +
      ':nest_productivity, ' +
      'date(:found_date), ' +
      'date(:last_date), ' +
      ':full_name, ' +
      ':description, ' +
      ':notes, ' +
      ':construction_days, ' +
      ':incubation_days, ' +
      ':nestling_days, ' +
      ':active_days, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));';
end;

function TNestsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT n.*, ' +
      'p.full_name AS observer_name, ' +
      'pj.project_title AS project_name, ' +
      'g.site_name AS locality_name, ' +
      'g.country_id AS country_id, ' +
      'g.state_id AS state_id, ' +
      'g.municipality_id AS municipality_id, ' +
      'z.full_name AS taxon_name, ' +
      'z.formatted_name AS taxon_formatted_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id, ' +
      'bt1.taxon_name AS support_plant_1_name, ' +
      'bt2.taxon_name AS support_plant_2_name ' +
    'FROM nests AS n ' +
    'LEFT JOIN people AS p ON n.observer_id = p.person_id ' +
    'LEFT JOIN projects AS pj ON n.project_id = pj.project_id ' +
    'LEFT JOIN gazetteer AS g ON n.locality_id = g.site_id ' +
    'LEFT JOIN zoo_taxa AS z ON n.taxon_id = z.taxon_id ' +
    'LEFT JOIN botanic_taxa AS bt1 ON n.support_plant_1_id = bt1.taxon_id ' +
    'LEFT JOIN botanic_taxa AS bt2 ON n.support_plant_2_id = bt2.taxon_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (n.nest_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (n.nest_id = :nest_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (n.nest_id = -1) AND (n.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (n.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (n.active_status = 1) AND (n.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (n.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TNestsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', n.found_date) AS ano, ' +
      'strftime(''%m'', n.found_date) AS mes, ' +
      'strftime(''%d'', n.found_date) AS dia ' +
    'FROM nests AS n ' +
    'WHERE (n.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'strftime(''%Y'', n.last_date) AS ano, ' +
      'strftime(''%m'', n.last_date) AS mes, ' +
      'strftime(''%d'', n.last_date) AS dia ' +
    'FROM nests AS n ' +
    'WHERE (n.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TNestsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'nest_id, ' +
      'field_number, ' +
      'observer_id, ' +
      'project_id, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'taxon_id, ' +
      'nest_shape, ' +
      'support_type, ' +
      'support_plant_1_id, ' +
      'support_plant_2_id, ' +
      'other_support, ' +
      'height_above_ground, ' +
      'internal_max_diameter, ' +
      'internal_min_diameter, ' +
      'external_max_diameter, ' +
      'external_min_diameter, ' +
      'internal_height, ' +
      'external_height, ' +
      'edge_distance, ' +
      'center_distance, ' +
      'nest_cover, ' +
      'plant_max_diameter, ' +
      'plant_min_diameter, ' +
      'plant_height, ' +
      'plant_dbh, ' +
      'construction_days, ' +
      'incubation_days, ' +
      'nestling_days, ' +
      'active_days, ' +
      'nest_fate, ' +
      'loss_cause, ' +
      'nest_productivity, ' +
      'found_date, ' +
      'last_date, ' +
      'full_name, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM nests ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (nest_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (nest_id = :nest_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (nest_id = -1) AND (active_status = 1) ';
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

function TNestsSQL.Update: String;
begin
  Result :=
    'UPDATE nests SET ' +
      'field_number = :field_number, ' +
      'observer_id = :observer_id, ' +
      'project_id = :project_id, ' +
      'locality_id = :locality_id, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'taxon_id = :taxon_id, ' +
      'nest_shape = :nest_shape, ' +
      'support_type = :support_type, ' +
      'support_plant_1_id = :support_plant_1_id, ' +
      'support_plant_2_id = :support_plant_2_id, ' +
      'other_support = :other_support, ' +
      'height_above_ground = :height_above_ground, ' +
      'internal_max_diameter = :internal_max_diameter, ' +
      'internal_min_diameter = :internal_min_diameter, ' +
      'external_max_diameter = :external_max_diameter, ' +
      'external_min_diameter = :external_min_diameter, ' +
      'internal_height = :internal_height, ' +
      'external_height = :external_height, ' +
      'edge_distance = :edge_distance, ' +
      'center_distance = :center_distance, ' +
      'nest_cover = :nest_cover, ' +
      'plant_max_diameter = :plant_max_diameter, ' +
      'plant_min_diameter = :plant_min_diameter, ' +
      'plant_height = :plant_height, ' +
      'plant_dbh = :plant_dbh, ' +
      'nest_fate = :nest_fate, ' +
      'loss_cause = :loss_cause, ' +
      'nest_productivity = :nest_productivity, ' +
      'found_date = date(:found_date), ' +
      'last_date = date(:last_date), ' +
      'full_name = :full_name, ' +
      'description = :description, ' +
      'notes = :notes, ' +
      'construction_days = :construction_days, ' +
      'incubation_days = :incubation_days, ' +
      'nestling_days = :nestling_days, ' +
      'active_days = :active_days, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (nest_id = :nest_id) ';
end;

{ TNestOwnersSQL }

constructor TNestOwnersSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TNestOwnersSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS nest_owners (' +
      'nest_owner_id   INTEGER     PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'nest_id         INTEGER     REFERENCES nests (nest_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'role            VARCHAR (5),' +
      'individual_id   INTEGER,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN     DEFAULT (0),' +
      'marked_status   BOOLEAN     DEFAULT (0),' +
      'active_status   BOOLEAN     DEFAULT (1)' +
    ');';
end;

function TNestOwnersSQL.Delete: String;
begin
  Result :=
    'DELETE FROM nest_owners ' +
    'WHERE nest_owner_id = :aid';
end;

function TNestOwnersSQL.Insert: String;
begin
  Result :=
    'INSERT INTO nest_owners (' +
      'nest_id, ' +
      'role, ' +
      'individual_id, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':nest_id, ' +
      ':role, ' +
      ':individual_id, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TNestOwnersSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT no.*, ' +
      'i.full_name AS individual_name ' +
    'FROM nest_owners AS no ' +
    'LEFT JOIN individuals AS i ON no.individual_id = i.individual_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (no.nest_owner_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (no.nest_owner_id = :nest_owner_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (no.nest_owner_id = -1) AND (no.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (no.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (no.active_status = 1) AND (no.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (no.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (no.active_status = 1) AND (no.nest_id = :nest_id) ';
    swcFindText: ;
  end;
end;

function TNestOwnersSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'nest_owner_id, ' +
      'nest_id, ' +
      'role, ' +
      'individual_id, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM nest_owners ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (nest_owner_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (nest_owner_id = :nest_owner_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (nest_owner_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (nest_id = :nest_id) ';
    swcFindText: ;
  end;
end;

function TNestOwnersSQL.Update: String;
begin
  Result :=
    'UPDATE nest_owners SET ' +
      'nest_id = :nest_id, ' +
      'role = :role, ' +
      'individual_id = :individual_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (nest_owner_id = :nest_owner_id) ';
end;

{ TNestRevisionsSQL }

constructor TNestRevisionsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TNestRevisionsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS nest_revisions (' +
      'nest_revision_id             INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'nest_id                      INTEGER       REFERENCES nests (nest_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'full_name                    VARCHAR (100),' +
      'revision_date                DATE,' +
      'revision_time                TIME,' +
      'observer_1_id                INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'observer_2_id                INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'nest_status                  CHAR (1),' +
      'host_eggs_tally              INTEGER,' +
      'host_nestlings_tally         INTEGER,' +
      'nidoparasite_eggs_tally      INTEGER,' +
      'nidoparasite_nestlings_tally INTEGER,' +
      'nidoparasite_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'have_philornis_larvae        BOOLEAN       DEFAULT (0),' +
      'nest_stage                   CHAR (1),' +
      'notes                        TEXT,' +
      'user_inserted                INTEGER,' +
      'user_updated                 INTEGER,' +
      'insert_date                  DATETIME,' +
      'update_date                  DATETIME,' +
      'exported_status              BOOLEAN       DEFAULT (0),' +
      'marked_status                BOOLEAN       DEFAULT (0),' +
      'active_status                BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TNestRevisionsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM nest_revisions ' +
    'WHERE nest_revision_id = :aid';
end;

function TNestRevisionsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT nest_revision_id, full_name FROM nest_revisions ';

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

function TNestRevisionsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO nest_revisions (' +
      'nest_id, ' +
      'full_name, ' +
      'revision_date, ' +
      'revision_time, ' +
      'observer_1_id, ' +
      'observer_2_id, ' +
      'nest_status, ' +
      'host_eggs_tally, ' +
      'host_nestlings_tally, ' +
      'nidoparasite_eggs_tally, ' +
      'nidoparasite_nestlings_tally, ' +
      'nidoparasite_id, ' +
      'have_philornis_larvae, ' +
      'nest_stage, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':nest_id, ' +
      ':full_name, ' +
      'date(:revision_date), ' +
      'time(:revision_time), ' +
      ':observer_1_id, ' +
      ':observer_2_id, ' +
      ':nest_status, ' +
      ':host_eggs_tally, ' +
      ':host_nestlings_tally, ' +
      ':nidoparasite_eggs_tally, ' +
      ':nidoparasite_nestlings_tally, ' +
      ':nidoparasite_id, ' +
      ':have_philornis_larvae, ' +
      ':nest_stage, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TNestRevisionsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT nr.*, ' +
      'p1.acronym AS observer_1_name, ' +
      'p2.acronym AS observer_2_name, ' +
      'z.full_name AS nidoparasite_name ' +
    'FROM nest_revisions AS nr ' +
    'LEFT JOIN people AS p1 ON nr.observer_1_id = p1.person_id ' +
    'LEFT JOIN people AS p2 ON nr.observer_2_id = p2.person_id ' +
    'LEFT JOIN zoo_taxa AS z ON nr.nidoparasite_id = z.taxon_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (nr.nest_revision_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (nr.nest_revision_id = :nest_revision_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (nr.nest_revision_id = -1) AND (nr.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (nr.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (nr.active_status = 1) AND (nr.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (nr.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (nr.active_status = 1) AND (nr.nest_id = :nest_id) ';
    swcFindText: ;
  end;
end;

function TNestRevisionsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', nr.revision_date) AS ano, ' +
      'strftime(''%m'', nr.revision_date) AS mes, ' +
      'strftime(''%d'', nr.revision_date) AS dia ' +
    'FROM nest_revisions AS nr ' +
    'WHERE (nr.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TNestRevisionsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'nest_revision_id, ' +
      'nest_id, ' +
      'full_name, ' +
      'revision_date, ' +
      'revision_time, ' +
      'observer_1_id, ' +
      'observer_2_id, ' +
      'nest_status, ' +
      'host_eggs_tally, ' +
      'host_nestlings_tally, ' +
      'nidoparasite_eggs_tally, ' +
      'nidoparasite_nestlings_tally, ' +
      'nidoparasite_id, ' +
      'have_philornis_larvae, ' +
      'nest_stage, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM nest_revisions ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (nest_revision_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (nest_revision_id = :nest_revision_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (nest_revision_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (nest_id = :nest_id) ';
    swcFindText: ;
  end;
end;

function TNestRevisionsSQL.Update: String;
begin
  Result :=
    'UPDATE nest_revisions SET ' +
      'nest_id = :nest_id, ' +
      'full_name = :full_name, ' +
      'revision_date = date(:revision_date), ' +
      'revision_time = time(:revision_time), ' +
      'observer_1_id = :observer_1_id, ' +
      'observer_2_id = :observer_2_id, ' +
      'nest_status = :nest_status, ' +
      'host_eggs_tally = :host_eggs_tally, ' +
      'host_nestlings_tally = :host_nestlings_tally, ' +
      'nidoparasite_eggs_tally = :nidoparasite_eggs_tally, ' +
      'nidoparasite_nestlings_tally = :nidoparasite_nestlings_tally, ' +
      'nidoparasite_id = :nidoparasite_id, ' +
      'have_philornis_larvae = :have_philornis_larvae, ' +
      'nest_stage = :nest_stage, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (nest_revision_id = :nest_revision_id) ';
end;

{ TEggsSQL }

constructor TEggsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TEggsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS eggs (' +
      'egg_id           INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'nest_id          INTEGER       REFERENCES nests (nest_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'egg_seq          INTEGER,' +
      'field_number     VARCHAR (20),' +
      'taxon_id         INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'eggshell_color   VARCHAR (40),' +
      'eggshell_pattern CHAR (5),' +
      'eggshell_texture CHAR (5),' +
      'egg_shape        CHAR (5),' +
      'egg_width        REAL,' +
      'egg_length       REAL,' +
      'egg_mass         REAL,' +
      'egg_volume       REAL,' +
      'egg_stage        CHAR (5),' +
      'egg_hatched      BOOLEAN       DEFAULT (1),' +
      'measure_date     DATE,' +
      'researcher_id    INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'individual_id    INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'host_egg         BOOLEAN       DEFAULT (1),' +
      'description      TEXT,' +
      'full_name        VARCHAR (100),' +
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

function TEggsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM eggs ' +
    'WHERE egg_id = :aid';
end;

function TEggsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT egg_id, full_name FROM eggs ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
          'AND (active_status = 1)';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1)';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1)';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0)';
  end;
end;

function TEggsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO eggs (' +
      'field_number, ' +
      'nest_id, ' +
      'egg_seq, ' +
      'egg_shape, ' +
      'egg_width, ' +
      'egg_length, ' +
      'egg_mass, ' +
      'egg_volume, ' +
      'egg_stage, ' +
      'eggshell_color, ' +
      'eggshell_pattern, ' +
      'eggshell_texture, ' +
      'egg_hatched, ' +
      'researcher_id, ' +
      'individual_id, ' +
      'measure_date, ' +
      'taxon_id, ' +
      'host_egg, ' +
      'description, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':field_number, ' +
      ':nest_id, ' +
      ':egg_seq, ' +
      ':egg_shape, ' +
      ':egg_width, ' +
      ':egg_length, ' +
      ':egg_mass, ' +
      ':egg_volume, ' +
      ':egg_stage, ' +
      ':eggshell_color, ' +
      ':eggshell_pattern, ' +
      ':eggshell_texture, ' +
      ':egg_hatched, ' +
      ':researcher_id, ' +
      ':individual_id, ' +
      'date(:measure_date), ' +
      ':taxon_id, ' +
      ':host_egg, ' +
      ':description, ' +
      ':notes, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));';
end;

function TEggsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT e.*, ' +
      'p.acronym AS researcher_name, ' +
      'i.full_name AS individual_name, ' +
      'z.full_name AS taxon_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id ' +
    'FROM eggs AS e ' +
    'LEFT JOIN people AS p ON e.researcher_id = p.person_id ' +
    'LEFT JOIN individuals AS i ON e.individual_id = i.individual_id ' +
    'LEFT JOIN zoo_taxa AS z ON e.taxon_id = z.taxon_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (e.egg_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (e.egg_id = :egg_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (e.egg_id = -1) AND (e.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (e.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (e.active_status = 1) AND (e.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (e.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (e.active_status = 1) AND (e.nest_id = :nest_id) ';
    swcFindText: ;
  end;
end;

function TEggsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', e.measure_date) AS ano, ' +
      'strftime(''%m'', e.measure_date) AS mes, ' +
      'strftime(''%d'', e.measure_date) AS dia ' +
    'FROM eggs AS e ' +
    'WHERE (e.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TEggsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'egg_id, ' +
      'nest_id, ' +
      'egg_seq, ' +
      'field_number, ' +
      'taxon_id, ' +
      'eggshell_color, ' +
      'eggshell_pattern, ' +
      'eggshell_texture, ' +
      'egg_shape, ' +
      'egg_width, ' +
      'egg_length, ' +
      'egg_mass, ' +
      'egg_volume, ' +
      'egg_stage, ' +
      'egg_hatched, ' +
      'measure_date, ' +
      'researcher_id, ' +
      'individual_id, ' +
      'host_egg, ' +
      'description, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM eggs ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (egg_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (egg_id = :egg_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (egg_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (nest_id = :nest_id) ';
    swcFindText: ;
  end;
end;

function TEggsSQL.Update: String;
begin
  Result :=
    'UPDATE eggs SET ' +
      'field_number = :field_number, ' +
      'egg_seq = :egg_seq, ' +
      'nest_id = :nest_id, ' +
      'egg_shape = :egg_shape, ' +
      'egg_width = :egg_width, ' +
      'egg_length = :egg_length, ' +
      'egg_mass = :egg_mass, ' +
      'egg_volume = :egg_volume, ' +
      'egg_stage = :egg_stage, ' +
      'eggshell_color = :eggshell_color, ' +
      'eggshell_pattern = :eggshell_pattern, ' +
      'eggshell_texture = :eggshell_texture, ' +
      'egg_hatched = :egg_hatched, ' +
      'researcher_id = :researcher_id, ' +
      'individual_id = :individual_id, ' +
      'measure_date = date(:measure_date), ' +
      'taxon_id = :taxon_id, ' +
      'host_egg = :host_egg, ' +
      'description = :description, ' +
      'notes = :notes, ' +
      'full_name = :full_name, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (egg_id = :egg_id) ';
end;

end.

