unit providers_birds;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TIndividualsSQL }

  TIndividualsSQL = class(TInterfacedObject, IIndividualsSQL)
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

  { TCapturesSQL }

  TCapturesSQL = class(TInterfacedObject, ICapturesSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause; aParent: TTableType): String;
    function SelectAll(aWhere: TSQLWhereClause; aParent: TTableType): String;
    function SelectDateTree(Grouped: Boolean): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
    function DistinctCameras: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

  { TFeathersSQL }

  TFeathersSQL = class(TInterfacedObject, IFeathersSQL)
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

{ TIndividualsSQL }

constructor TIndividualsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TIndividualsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS individuals (' +
      'individual_id         INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'formatted_name        VARCHAR (150),' +
      'full_name             VARCHAR (120),' +
      'taxon_id              INTEGER       NOT NULL REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'individual_sex        CHAR (1),' +
      'individual_age        CHAR (1),' +
      'nest_id               INTEGER       REFERENCES nests (nest_id) ON UPDATE CASCADE,' +
      'birth_date            VARCHAR (15),' +
      'birth_day             INTEGER,' +
      'birth_month           INTEGER,' +
      'birth_year            INTEGER,' +
      'banding_date          DATE,' +
      'band_change_date      DATE,' +
      'band_id               INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
      'double_band_id        INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
      'removed_band_id       INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
      'right_leg_below       VARCHAR (10),' +
      'left_leg_below        VARCHAR (10),' +
      'right_leg_above       VARCHAR (10),' +
      'left_leg_above        VARCHAR (10),' +
      'father_id             INTEGER,' +
      'mother_id             INTEGER,' +
      'death_date            VARCHAR (15),' +
      'death_day             INTEGER,' +
      'death_month           INTEGER,' +
      'death_year            INTEGER,' +
      'recognizable_markings TEXT,' +
      'notes                 TEXT,' +
      'user_inserted         INTEGER,' +
      'user_updated          INTEGER,' +
      'insert_date           DATETIME,' +
      'update_date           DATETIME,' +
      'exported_status       BOOLEAN       DEFAULT (0),' +
      'queued_status         BOOLEAN       DEFAULT (0),' +
      'marked_status         BOOLEAN       DEFAULT (0),' +
      'active_status         BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TIndividualsSQL.Delete: String;
begin
  //case FBackend of
  //  dbSQLite,
  //  dbPostgres,
  //  dbMariaDB:
      Result :=
        'DELETE FROM individuals WHERE individual_id = :aid';
  //end;
end;

function TIndividualsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT individual_id, full_name FROM individuals ';

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

function TIndividualsSQL.Insert: String;
begin
  //case FBackend of
  //  dbSQLite,
  //  dbPostgres,
  //  dbMariaDB:
  Result :=
    'INSERT INTO individuals (' +
      'taxon_id, ' +
      'individual_sex, ' +
      'individual_age, ' +
      'nest_id, ' +
      'birth_date, ' +
      'birth_day, ' +
      'birth_month, ' +
      'birth_year, ' +
      'banding_date, ' +
      'band_change_date, ' +
      'band_id, ' +
      'double_band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'father_id, ' +
      'mother_id, ' +
      'death_date, ' +
      'death_day, ' +
      'death_month, ' +
      'death_year, ' +
      'recognizable_markings, ' +
      'notes, ' +
      'formatted_name, ' +
      'full_name, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':taxon_id, ' +
      ':individual_sex, ' +
      ':individual_age, ' +
      ':nest_id, ' +
      ':birth_date, ' +
      ':birth_day, ' +
      ':birth_month, ' +
      ':birth_year, ' +
      'date(:banding_date), ' +
      'date(:band_change_date), ' +
      ':band_id, ' +
      ':double_band_id, ' +
      ':removed_band_id, ' +
      ':right_leg_below, ' +
      ':left_leg_below, ' +
      ':right_leg_above, ' +
      ':left_leg_above, ' +
      ':father_id, ' +
      ':mother_id, ' +
      ':death_date, ' +
      ':death_day, ' +
      ':death_month, ' +
      ':death_year, ' +
      ':recognizable_markings, ' +
      ':notes, ' +
      ':formatted_name, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
  //end;
end;

function TIndividualsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  //case FBackend of
  //  dbSQLite:
  Result :=
    'SELECT ' +
      'individual_id, ' +
      'formatted_name, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_sex, ' +
      'individual_age, ' +
      'nest_id, ' +
      'birth_date, ' +
      'birth_day, ' +
      'birth_month, ' +
      'birth_year, ' +
      'banding_date, ' +
      'band_change_date, ' +
      'band_id, ' +
      'double_band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'father_id, ' +
      'mother_id, ' +
      'death_date, ' +
      'death_day, ' +
      'death_month, ' +
      'death_year, ' +
      'recognizable_markings, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'queued_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM individuals ';

  //  dbPostgres:
  //    Result :=
  //      'SELECT i.individual_id, i.full_name, i.taxon_id, i.sex, i.age ' +
  //      'FROM individuals i';
  //
  //  dbMariaDB:
  //    Result :=
  //      'SELECT i.individual_id, i.full_name, i.taxon_id, i.sex, i.age ' +
  //      'FROM individuals i';
  //end;

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (individual_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (individual_id = :individual_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (individual_id = -1) AND (active_status = 1) ';
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

function TIndividualsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  //case FBackend of
  //  dbSQLite:
  Result :=
    'SELECT i.*, ' +
      'z.full_name AS taxon_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id, ' +
      'n.full_name AS nest_name, ' +
      '(b1.band_size||'' ''||b1.band_number) AS band_name, ' +
      '(b2.band_size||'' ''||b2.band_number) AS double_band_name, ' +
      '(b3.band_size||'' ''||b3.band_number) AS removed_band_name, ' +
      'fi.full_name AS father_name, ' +
      'mi.full_name AS mother_name, ' +
      '(SELECT CAST(SUM(c.active_status) AS INTEGER) FROM captures AS c ' +
        'WHERE c.individual_id = i.individual_id) AS captures_tally ' +
    'FROM individuals AS i ' +
    'LEFT JOIN zoo_taxa AS z ON i.taxon_id = z.taxon_id ' +
    'LEFT JOIN nests AS n ON i.nest_id = n.nest_id ' +
    'LEFT JOIN bands AS b1 ON i.band_id = b1.band_id ' +
    'LEFT JOIN bands AS b2 ON i.double_band_id = b2.band_id ' +
    'LEFT JOIN bands AS b3 ON i.removed_band_id = b3.band_id ' +
    'LEFT JOIN individuals AS fi ON i.father_id = fi.individual_id ' +
    'LEFT JOIN individuals AS mi ON i.mother_id = mi.individual_id ';

  //  dbPostgres:
  //    Result :=
  //      'SELECT i.individual_id, i.full_name, i.taxon_id, i.sex, i.age ' +
  //      'FROM individuals i';
  //
  //  dbMariaDB:
  //    Result :=
  //      'SELECT i.individual_id, i.full_name, i.taxon_id, i.sex, i.age ' +
  //      'FROM individuals i';
  //end;

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (i.individual_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (i.individual_id = :individual_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (i.individual_id = -1) AND (i.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (i.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (i.active_status = 1) AND (i.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (i.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TIndividualsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'i.birth_year AS ano, ' +
      'i.birth_month AS mes, ' +
      'i.birth_day AS dia ' +
    'FROM individuals AS i ' +
    'WHERE (i.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'CAST(strftime(''%Y'', i.banding_date) AS INTEGER) AS ano, ' +
      'CAST(strftime(''%m'', i.banding_date) AS INTEGER) AS mes, ' +
      'CAST(strftime(''%d'', i.banding_date) AS INTEGER) AS dia ' +
    'FROM individuals AS i ' +
    'WHERE (i.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'CAST(strftime(''%Y'', i.band_change_date) AS INTEGER) AS ano, ' +
      'CAST(strftime(''%m'', i.band_change_date) AS INTEGER) AS mes, ' +
      'CAST(strftime(''%d'', i.band_change_date) AS INTEGER) AS dia ' +
    'FROM individuals AS i ' +
    'WHERE (i.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'i.death_year AS ano, ' +
      'i.death_month AS mes, ' +
      'i.death_day AS dia ' +
    'FROM individuals AS i ' +
    'WHERE (i.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TIndividualsSQL.Update: String;
begin
  //case FBackend of
  //  dbSQLite,
  //  dbPostgres,
  //  dbMariaDB:
  Result :=
    'UPDATE individuals SET ' +
      'taxon_id = :taxon_id, ' +
      'individual_sex = :individual_sex, ' +
      'individual_age = :individual_age, ' +
      'nest_id = :nest_id, ' +
      'birth_date = :birth_date, ' +
      'birth_day = :birth_day, ' +
      'birth_month = :birth_month, ' +
      'birth_year = :birth_year, ' +
      'banding_date = date(:banding_date), ' +
      'band_change_date = date(:band_change_date), ' +
      'band_id = :band_id, ' +
      'double_band_id = :double_band_id, ' +
      'removed_band_id = :removed_band_id, ' +
      'right_leg_below = :right_leg_below, ' +
      'left_leg_below = :left_leg_below, ' +
      'right_leg_above = :right_leg_above, ' +
      'left_leg_above = :left_leg_above, ' +
      'father_id = :father_id, ' +
      'mother_id = :mother_id, ' +
      'death_date = :death_date, ' +
      'death_day = :death_day, ' +
      'death_month = :death_month, ' +
      'death_year = :death_year, ' +
      'recognizable_markings = :recognizable_markings, ' +
      'notes = :notes, ' +
      'formatted_name = :formatted_name, ' +
      'full_name = :full_name, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (individual_id = :individual_id) ';
  //end;
end;

{ TCapturesSQL }

constructor TCapturesSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TCapturesSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS captures (' +
      'capture_id             INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'survey_id              INTEGER,' +
      'individual_id          INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'taxon_id               INTEGER       NOT NULL REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'full_name              VARCHAR (120),' +
      'project_id             INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
      'capture_date           DATE          NOT NULL,' +
      'capture_time           TIME,' +
      'locality_id            INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'net_station_id         INTEGER       REFERENCES sampling_plots (sampling_plot_id) ON UPDATE CASCADE,' +
      'net_id                 INTEGER       REFERENCES nets_effort (net_id) ON UPDATE CASCADE,' +
      'longitude              REAL,' +
      'latitude               REAL,' +
      'coordinate_precision   VARCHAR (3),' +
      'bander_id              INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'annotator_id           INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'subject_status         CHAR (5),' +
      'capture_type           CHAR (5)      NOT NULL,' +
      'subject_sex            CHAR (5),' +
      'how_sexed              VARCHAR (10),' +
      'band_id                INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
      'removed_band_id        INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
      'right_leg_below        VARCHAR (10),' +
      'left_leg_below         VARCHAR (10),' +
      'right_leg_above        VARCHAR (10),' +
      'left_leg_above         VARCHAR (10),' +
      'weight                 REAL,' +
      'tarsus_length          REAL,' +
      'tarsus_diameter        REAL,' +
      'culmen_length          REAL,' +
      'exposed_culmen         REAL,' +
      'bill_width             REAL,' +
      'bill_height            REAL,' +
      'nostril_bill_tip       REAL,' +
      'skull_length           REAL,' +
      'halux_length_total     REAL,' +
      'halux_length_finger    REAL,' +
      'halux_length_claw      REAL,' +
      'right_wing_chord       REAL,' +
      'first_secondary_chord  REAL,' +
      'tail_length            REAL,' +
      'central_retrix_length  REAL,' +
      'external_retrix_length REAL,' +
      'total_length           REAL,' +
      'feather_mites          VARCHAR (15),' +
      'fat                    CHAR (5),' +
      'brood_patch            CHAR (5),' +
      'cloacal_protuberance   CHAR (5),' +
      //'old_molt               CHAR (5),' +
      //'old_primaries_molt     VARCHAR (20),' +
      //'old_secondaries_molt   VARCHAR (40),' +
      //'old_retrices_molt      VARCHAR (20),' +
      //'old_body_molt          VARCHAR (20),' +
      'body_molt              CHAR (5),' +
      'flight_feathers_molt   CHAR (5),' +
      'flight_feathers_wear   CHAR (5),' +
      'molt_limits            VARCHAR (20),' +
      'cycle_code             CHAR (10),' +
      'subject_age            CHAR (5),' +
      'how_aged               CHAR (10),' +
      'skull_ossification     CHAR (5),' +
      'kipps_index            REAL,' +
      'glucose                REAL,' +
      'hemoglobin             REAL,' +
      'hematocrit             REAL,' +
      'philornis_larvae_tally INTEGER,' +
      'blood_sample           BOOLEAN       DEFAULT (0),' +
      'feather_sample         BOOLEAN       DEFAULT (0),' +
      'claw_sample            BOOLEAN       DEFAULT (0),' +
      'feces_sample           BOOLEAN       DEFAULT (0),' +
      'parasite_sample        BOOLEAN       DEFAULT (0),' +
      'subject_collected      BOOLEAN       DEFAULT (0),' +
      'subject_recorded       BOOLEAN       DEFAULT (0),' +
      'subject_photographed   BOOLEAN       DEFAULT (0),' +
      'field_number           VARCHAR (10),' +
      'photographer_1_id      INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'photographer_2_id      INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'start_photo_number     VARCHAR (20),' +
      'end_photo_number       VARCHAR (20),' +
      'camera_name            VARCHAR (50),' +
      'escaped                BOOLEAN       DEFAULT (0),' +
      'needs_review           BOOLEAN       DEFAULT (0),' +
      'notes                  TEXT,' +
      'user_inserted          INTEGER,' +
      'user_updated           INTEGER,' +
      'insert_date            DATETIME,' +
      'update_date            DATETIME,' +
      'exported_status        BOOLEAN       DEFAULT (0),' +
      'marked_status          BOOLEAN       DEFAULT (0),' +
      'active_status          BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TCapturesSQL.Delete: String;
begin
  Result :=
    'DELETE FROM captures ' +
    'WHERE (capture_id = :aid)';
end;

function TCapturesSQL.DistinctCameras: String;
begin
  Result :=
    'SELECT camera_name ' +
    'FROM captures ' +
    'WHERE (active_status = 1) AND (camera_name NOT NULL) AND (camera_name <> '''') ' +
    'GROUP BY camera_name ' +
    'ORDER BY camera_name ASC ';
end;

function TCapturesSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT capture_id, full_name FROM captures ';

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

function TCapturesSQL.Insert: String;
begin
  Result :=
    'INSERT INTO captures (' +
      'survey_id, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_date, ' +
      'capture_time, ' +
      'locality_id, ' +
      'net_station_id, ' +
      'net_id, ' +
      'latitude, ' +
      'longitude, ' +
      'coordinate_precision, ' +
      'bander_id, ' +
      'annotator_id, ' +
      'subject_status, ' +
      'capture_type, ' +
      'subject_sex, ' +
      'how_sexed, ' +
      'band_id, ' +
      'weight, ' +
      'tarsus_length, ' +
      'tarsus_diameter, ' +
      'exposed_culmen, ' +
      'bill_width, ' +
      'bill_height, ' +
      'nostril_bill_tip, ' +
      'skull_length, ' +
      'right_wing_chord, ' +
      'first_secondary_chord, ' +
      'tail_length, ' +
      'fat, ' +
      'brood_patch, ' +
      'cloacal_protuberance, ' +
      'body_molt, ' +
      'flight_feathers_molt, ' +
      'flight_feathers_wear, ' +
      'molt_limits, ' +
      'cycle_code, ' +
      'subject_age, ' +
      'how_aged, ' +
      'skull_ossification, ' +
      'kipps_index, ' +
      'glucose, ' +
      'hemoglobin, ' +
      'hematocrit, ' +
      'blood_sample, ' +
      'feather_sample, ' +
      'subject_photographed, ' +
      'photographer_1_id, ' +
      'photographer_2_id, ' +
      'start_photo_number, ' +
      'end_photo_number, ' +
      'camera_name, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'escaped, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':survey_id, ' +
      ':full_name, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      'date(:capture_date), ' +
      'time(:capture_time), ' +
      ':locality_id, ' +
      ':net_station_id, ' +
      ':net_id, ' +
      ':latitude, ' +
      ':longitude, ' +
      ':coordinate_precision, ' +
      ':bander_id, ' +
      ':annotator_id, ' +
      ':subject_status, ' +
      ':capture_type, ' +
      ':subject_sex, ' +
      ':how_sexed, ' +
      ':band_id, ' +
      ':weight, ' +
      ':tarsus_length, ' +
      ':tarsus_diameter, ' +
      ':exposed_culmen, ' +
      ':bill_width, ' +
      ':bill_height, ' +
      ':nostril_bill_tip, ' +
      ':skull_length, ' +
      ':right_wing_chord, ' +
      ':first_secondary_chord, ' +
      ':tail_length, ' +
      ':fat, ' +
      ':brood_patch, ' +
      ':cloacal_protuberance, ' +
      ':body_molt, ' +
      ':flight_feathers_molt, ' +
      ':flight_feathers_wear, ' +
      ':molt_limits, ' +
      ':cycle_code, ' +
      ':subject_age, ' +
      ':how_aged, ' +
      ':skull_ossification, ' +
      ':kipps_index, ' +
      ':glucose, ' +
      ':hemoglobin, ' +
      ':hematocrit, ' +
      ':blood_sample, ' +
      ':feather_sample, ' +
      ':subject_photographed, ' +
      ':photographer_1_id, ' +
      ':photographer_2_id, ' +
      ':start_photo_number, ' +
      ':end_photo_number, ' +
      ':camera_name, ' +
      ':removed_band_id, ' +
      ':right_leg_below, ' +
      ':left_leg_below, ' +
      ':escaped, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TCapturesSQL.SelectAll(aWhere: TSQLWhereClause; aParent: TTableType): String;
begin
  Result :=
    'SELECT c.*, ' +
      'z.full_name AS taxon_name, ' +
      'z.formatted_name AS taxon_formatted_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id, ' +
      'sv.full_name AS survey_name, ' +
      'pl.full_name AS net_station_name, ' +
      'ef.net_number AS net_number, ' +
      'g.site_name AS locality_name, ' +
      'g.country_id AS country_id, ' +
      'g.state_id AS state_id, ' +
      'g.municipality_id AS municipality_id, ' +
      'p1.acronym AS bander_name, ' +
      'p2.acronym AS annotator_name, ' +
      '(b1.band_size||'' ''||b1.band_number) AS band_name, ' +
      '(b2.band_size||'' ''||b2.band_number) AS removed_band_name, ' +
      'f1.acronym AS photographer_1_name, ' +
      'f2.acronym AS photographer_2_name ' +
    'FROM captures AS c ' +
    'LEFT JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id ' +
    'LEFT JOIN surveys AS sv ON c.survey_id = sv.survey_id ' +
    'LEFT JOIN sampling_plots AS pl ON c.net_station_id = pl.sampling_plot_id ' +
    'LEFT JOIN nets_effort AS ef ON c.net_id = ef.net_id ' +
    'LEFT JOIN gazetteer AS g ON c.locality_id = g.site_id ' +
    'LEFT JOIN people AS p1 ON c.bander_id = p1.person_id ' +
    'LEFT JOIN people AS p2 ON c.annotator_id = p2.person_id ' +
    'LEFT JOIN people AS f1 ON c.photographer_1_id = f1.person_id ' +
    'LEFT JOIN people AS f2 ON c.photographer_2_id = f2.person_id ' +
    'LEFT JOIN bands AS b1 ON c.band_id = b1.band_id ' +
    'LEFT JOIN bands AS b2 ON c.removed_band_id = b2.band_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (c.capture_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (c.capture_id = :capture_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (c.capture_id = -1) AND (c.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (c.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (c.active_status = 1) AND (c.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (c.active_status = 0) ';
    swcActiveParent:
      begin
        if aParent = tbIndividuals then
          Result := Result + 'WHERE (c.active_status = 1) AND (c.individual_id = :individual_id) '
        else
        if aParent = tbSurveys then
          Result := Result + 'WHERE (c.active_status = 1) AND (c.survey_id = :survey_id) ';
      end;
    swcFindText: ;
  end;
end;

function TCapturesSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', c.capture_date) AS ano, ' +
      'strftime(''%m'', c.capture_date) AS mes, ' +
      'strftime(''%d'', c.capture_date) AS dia ' +
    'FROM captures AS c ' +
    'WHERE (c.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TCapturesSQL.SelectTable(aWhere: TSQLWhereClause; aParent: TTableType): String;
begin
  Result :=
    'SELECT ' +
      'capture_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'taxon_id, ' +
      'full_name, ' +
      'project_id, ' +
      'capture_date, ' +
      'capture_time, ' +
      'locality_id, ' +
      'net_station_id, ' +
      'net_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'bander_id, ' +
      'annotator_id, ' +
      'subject_status, ' +
      'capture_type, ' +
      'subject_sex, ' +
      'how_sexed, ' +
      'band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'weight, ' +
      'tarsus_length, ' +
      'tarsus_diameter, ' +
      'culmen_length, ' +
      'exposed_culmen, ' +
      'bill_width, ' +
      'bill_height, ' +
      'nostril_bill_tip, ' +
      'skull_length, ' +
      'halux_length_total, ' +
      'halux_length_finger, ' +
      'halux_length_claw, ' +
      'right_wing_chord, ' +
      'first_secondary_chord, ' +
      'tail_length, ' +
      'central_retrix_length, ' +
      'external_retrix_length, ' +
      'total_length, ' +
      'feather_mites, ' +
      'fat, ' +
      'brood_patch, ' +
      'cloacal_protuberance, ' +
      'body_molt, ' +
      'flight_feathers_molt, ' +
      'flight_feathers_wear, ' +
      'molt_limits, ' +
      'cycle_code, ' +
      'subject_age, ' +
      'how_aged, ' +
      'skull_ossification, ' +
      'kipps_index, ' +
      'glucose, ' +
      'hemoglobin, ' +
      'hematocrit, ' +
      'philornis_larvae_tally, ' +
      'blood_sample, ' +
      'feather_sample, ' +
      'claw_sample, ' +
      'feces_sample, ' +
      'parasite_sample, ' +
      'subject_collected, ' +
      'subject_recorded, ' +
      'subject_photographed, ' +
      'field_number, ' +
      'photographer_1_id, ' +
      'photographer_2_id, ' +
      'start_photo_number, ' +
      'end_photo_number, ' +
      'camera_name, ' +
      'escaped, ' +
      'needs_review, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM captures ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (capture_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (capture_id = :capture_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (capture_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      begin
        if aParent = tbIndividuals then
          Result := Result + 'WHERE (active_status = 1) AND (individual_id = :individual_id) '
        else
        if aParent = tbSurveys then
          Result := Result + 'WHERE (active_status = 1) AND (survey_id = :survey_id) ';
      end;
    swcFindText: ;
  end;
end;

function TCapturesSQL.Update: String;
begin
  Result :=
    'UPDATE captures SET ' +
      'survey_id = :survey_id, ' +
      'full_name = :full_name, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'project_id = :project_id, ' +
      'capture_date = date(:capture_date), ' +
      'capture_time = time(:capture_time), ' +
      'locality_id = :locality_id, ' +
      'net_station_id = :net_station_id, ' +
      'net_id = :net_id, ' +
      'latitude = :latitude, ' +
      'longitude = :longitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'bander_id = :bander_id, ' +
      'annotator_id = :annotator_id, ' +
      'subject_status = :subject_status, ' +
      'capture_type = :capture_type, ' +
      'subject_sex = :subject_sex, ' +
      'how_sexed = :how_sexed, ' +
      'band_id = :band_id, ' +
      'weight = :weight, ' +
      'tarsus_length = :tarsus_length, ' +
      'tarsus_diameter = :tarsus_diameter, ' +
      'culmen_length = :culmen_length, ' +
      'exposed_culmen = :exposed_culmen, ' +
      'bill_width = :bill_width, ' +
      'bill_height = :bill_height, ' +
      'nostril_bill_tip = :nostril_bill_tip, ' +
      'skull_length = :skull_length, ' +
      'right_wing_chord = :right_wing_chord, ' +
      'first_secondary_chord = :first_secondary_chord, ' +
      'tail_length = :tail_length, ' +
      'fat = :fat, ' +
      'brood_patch = :brood_patch, ' +
      'cloacal_protuberance = :cloacal_protuberance, ' +
      'body_molt = :body_molt, ' +
      'flight_feathers_molt = :flight_feathers_molt, ' +
      'flight_feathers_wear = :flight_feathers_wear, ' +
      'molt_limits = :molt_limits, ' +
      'cycle_code = :cycle_code, ' +
      'subject_age = :subject_age, ' +
      'how_aged = :how_aged, ' +
      'skull_ossification = :skull_ossification, ' +
      'halux_length_total = :halux_length_total, ' +
      'halux_length_finger = :halux_length_finger, ' +
      'halux_length_claw = :halux_length_claw, ' +
      'central_retrix_length = :central_retrix_length, ' +
      'external_retrix_length = :external_retrix_length, ' +
      'total_length = :total_length, ' +
      'feather_mites = :feather_mites, ' +
      'philornis_larvae_tally = :philornis_larvae_tally, ' +
      'kipps_index = :kipps_index, ' +
      'glucose = :glucose, ' +
      'hemoglobin = :hemoglobin, ' +
      'hematocrit = :hematocrit, ' +
      'field_number = :field_number, ' +
      'blood_sample = :blood_sample, ' +
      'feather_sample = :feather_sample, ' +
      'claw_sample = :claw_sample, ' +
      'feces_sample = :feces_sample, ' +
      'parasite_sample = :parasite_sample, ' +
      'subject_collected = :subject_collected, ' +
      'subject_recorded = :subject_recorded, ' +
      'subject_photographed = :subject_photographed, ' +
      'photographer_1_id = :photographer_1_id, ' +
      'photographer_2_id = :photographer_2_id, ' +
      'start_photo_number = :start_photo_number, ' +
      'end_photo_number = :end_photo_number, ' +
      'camera_name = :camera_name, ' +
      'removed_band_id = :removed_band_id, ' +
      'right_leg_below = :right_leg_below, ' +
      'left_leg_below = :left_leg_below, ' +
      'right_leg_above = :right_leg_above, ' +
      'left_leg_above = :left_leg_above, ' +
      'escaped = :escaped, ' +
      'needs_review = :needs_review, ' +
      'notes = :notes, ' +
      'exported_status = :exported_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (capture_id = :capture_id) ';
end;

{ TFeathersSQL }

constructor TFeathersSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TFeathersSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS feathers (' +
      'feather_id       INTEGER     PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'sample_date      DATE        NOT NULL,' +
      'sample_time      TIME,' +
      'taxon_id         INTEGER     REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE NOT NULL,' +
      'locality_id      INTEGER     REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'individual_id    INTEGER     REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'capture_id       INTEGER     REFERENCES captures (capture_id) ON UPDATE CASCADE,' +
      'sighting_id      INTEGER     REFERENCES sightings (sighting_id) ON UPDATE CASCADE,' +
      'observer_id      INTEGER     REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'source_type      VARCHAR (5),' +
      'symmetrical      VARCHAR (5),' +
      'feather_trait    VARCHAR (5),' +
      'feather_number   INTEGER,' +
      'body_side        VARCHAR (5),' +
      'grown_percent    REAL,' +
      'feather_length   REAL,' +
      'feather_area     REAL,' +
      'feather_mass     REAL,' +
      'rachis_width     REAL,' +
      'growth_bar_width REAL,' +
      'barb_density     REAL,' +
      'feather_age      VARCHAR (5),' +
      'full_name        VARCHAR (200),' +
      'notes            TEXT,' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN     DEFAULT (0),' +
      'marked_status    BOOLEAN     DEFAULT (0),' +
      'active_status    BOOLEAN     DEFAULT (1)' +
    ');';
end;

function TFeathersSQL.Delete: String;
begin
  Result :=
    'DELETE FROM feathers ' +
    'WHERE (feather_id = :aid)';
end;

function TFeathersSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT feather_id, full_name FROM feathers ';

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

function TFeathersSQL.Insert: String;
begin
  Result :=
    'INSERT INTO feathers (' +
      'sample_date, ' +
      'sample_time, ' +
      'taxon_id, ' +
      'locality_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'observer_id, ' +
      'source_type, ' +
      'symmetrical, ' +
      'feather_trait, ' +
      'feather_number, ' +
      'body_side, ' +
      'grown_percent, ' +
      'feather_length, ' +
      'feather_area, ' +
      'feather_mass, ' +
      'rachis_width, ' +
      'growth_bar_width, ' +
      'barb_density, ' +
      'feather_age, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':taxon_id, ' +
      ':locality_id, ' +
      ':individual_id, ' +
      ':capture_id, ' +
      ':sighting_id, ' +
      ':observer_id, ' +
      ':source_type, ' +
      ':symmetrical, ' +
      ':feather_trait, ' +
      ':feather_number, ' +
      ':body_side, ' +
      ':grown_percent, ' +
      ':feather_length, ' +
      ':feather_area, ' +
      ':feather_mass, ' +
      ':rachis_width, ' +
      ':growth_bar_width, ' +
      ':barb_density, ' +
      ':feather_age, ' +
      ':full_name, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TFeathersSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ft.*, ' +
      'z.full_name AS taxon_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id, ' +
      'i.full_name AS individual_name, ' +
      'p.acronym AS observer_name, ' +
      'c.full_name AS capture_name, ' +
      'st.full_name AS sighting_name, ' +
      'g.country_id AS country_id, ' +
      'g.state_id AS state_id, ' +
      'g.municipality_id AS municipality_id, ' +
      'g.site_name AS locality_name ' +
    'FROM feathers AS ft ' +
    'LEFT JOIN zoo_taxa AS z ON ft.taxon_id = z.taxon_id ' +
    'LEFT JOIN individuals AS i ON ft.individual_id = i.individual_id ' +
    'LEFT JOIN people AS p ON ft.observer_id = p.person_id ' +
    'LEFT JOIN captures AS c ON ft.capture_id = c.capture_id ' +
    'LEFT JOIN sightings AS st ON ft.sighting_id = st.sighting_id ' +
    'LEFT JOIN gazetteer AS g ON ft.locality_id = g.site_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (ft.feather_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (ft.feather_id = :feather_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (ft.feather_id = -1) AND (ft.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (ft.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (ft.active_status = 1) AND (ft.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (ft.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (ft.active_status = 1) AND (ft.individual_id = :individual_id) ';
    swcFindText: ;
  end;
end;

function TFeathersSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', ft.sample_date) AS ano, ' +
      'strftime(''%m'', ft.sample_date) AS mes, ' +
      'strftime(''%d'', ft.sample_date) AS dia ' +
    'FROM feathers AS ft ' +
    'WHERE (ft.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TFeathersSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'feather_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'taxon_id, ' +
      'locality_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'observer_id, ' +
      'source_type, ' +
      'symmetrical, ' +
      'feather_trait, ' +
      'feather_number, ' +
      'body_side, ' +
      'grown_percent, ' +
      'feather_length, ' +
      'feather_area, ' +
      'feather_mass, ' +
      'rachis_width, ' +
      'growth_bar_width, ' +
      'barb_density, ' +
      'feather_age, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM feathers ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (feather_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (feather_id = :feather_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (feather_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (individual_id = :individual_id) ';
    swcFindText: ;
  end;
end;

function TFeathersSQL.Update: String;
begin
  Result :=
    'UPDATE feathers SET ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'taxon_id = :taxon_id, ' +
      'locality_id = :locality_id, ' +
      'individual_id = :individual_id, ' +
      'capture_id = :capture_id, ' +
      'sighting_id = :sighting_id, ' +
      'observer_id = :observer_id, ' +
      'source_type = :source_type, ' +
      'symmetrical = :symmetrical, ' +
      'feather_trait = :feather_trait, ' +
      'feather_number = :feather_number, ' +
      'body_side = :body_side, ' +
      'grown_percent = :grown_percent, ' +
      'feather_length = :feather_length, ' +
      'feather_area = :feather_area, ' +
      'feather_mass = :feather_mass, ' +
      'rachis_width = :rachis_width, ' +
      'growth_bar_width = :growth_bar_width, ' +
      'barb_density = :barb_density, ' +
      'feather_age = :feather_age, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'exported_status = :exported_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (feather_id = :feather_id) ';
end;

end.

