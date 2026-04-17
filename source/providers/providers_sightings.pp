unit providers_sightings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TSightingsSQL }

  TSightingsSQL = class(TInterfacedObject, ISightingsSQL)
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
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

implementation

{ TSightingsSQL }

constructor TSightingsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSightingsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS sightings (' +
      'sighting_id          INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'survey_id            INTEGER,' +
      'individual_id        INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'sighting_date        DATE,' +
      'sighting_time        TIME,' +
      'locality_id          INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'longitude            REAL,' +
      'latitude             REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'method_id            INTEGER       REFERENCES methods (method_id) ON UPDATE CASCADE,' +
      'mackinnon_list_num   INTEGER,' +
      'observer_id          INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'taxon_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'subjects_tally       INTEGER,' +
      'subject_distance     REAL,' +
      'flight_height        REAL,' +
      'flight_direction     VARCHAR (5),' +
      'subject_seen         BOOLEAN       DEFAULT (0),' +
      'subject_heard        BOOLEAN       DEFAULT (0),' +
      'subject_photographed BOOLEAN       DEFAULT (0),' +
      'subject_recorded     BOOLEAN       DEFAULT (0),' +
      'subject_captured     BOOLEAN       DEFAULT (0),' +
      'males_tally          VARCHAR (10),' +
      'females_tally        VARCHAR (10),' +
      'not_sexed_tally      VARCHAR (10),' +
      'adults_tally         VARCHAR (10),' +
      'immatures_tally      VARCHAR (10),' +
      'not_aged_tally       VARCHAR (10),' +
      'new_captures_tally   INTEGER,' +
      'recaptures_tally     INTEGER,' +
      'unbanded_tally       INTEGER,' +
      'detection_type       VARCHAR (30),' +
      'breeding_status      VARCHAR (30),' +
      'out_of_sample        BOOLEAN       DEFAULT (0),' +
      'ebird_available      BOOLEAN       DEFAULT (0),' +
      'full_name            VARCHAR (100),' +
      'notes                TEXT,' +
      'user_inserted        INTEGER,' +
      'user_updated         INTEGER,' +
      'insert_date          DATETIME,' +
      'update_date          DATETIME,' +
      'exported_status      BOOLEAN       DEFAULT (0),' +
      'marked_status        BOOLEAN       DEFAULT (0),' +
      'active_status        BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TSightingsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM sightings ' +
    'WHERE sighting_id = :aid';
end;

function TSightingsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT sighting_id, full_name FROM sightings ';

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

function TSightingsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO sightings (' +
      'survey_id, ' +
      'individual_id, ' +
      'sighting_date, ' +
      'sighting_time, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'method_id, ' +
      'mackinnon_list_num, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'subjects_tally, ' +
      'subject_distance, ' +
      'flight_height, ' +
      'flight_direction, ' +
      'subject_seen, ' +
      'subject_heard, ' +
      'subject_photographed, ' +
      'subject_recorded, ' +
      'subject_captured, ' +
      'males_tally, ' +
      'females_tally, ' +
      'not_sexed_tally, ' +
      'adults_tally, ' +
      'immatures_tally, ' +
      'not_aged_tally, ' +
      'new_captures_tally, ' +
      'recaptures_tally, ' +
      'unbanded_tally, ' +
      'detection_type, ' +
      'breeding_status, ' +
      'out_of_sample, ' +
      'ebird_available, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':survey_id, ' +
      ':individual_id, ' +
      'date(:sighting_date), ' +
      //'(CASE WHEN :sighting_time IS NULL THEN NULL ELSE time(:sighting_time) END),' +
      'time(:sighting_time), ' +
      ':locality_id, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':method_id, ' +
      ':mackinnon_list_num, ' +
      ':observer_id, ' +
      ':taxon_id, ' +
      ':subjects_tally, ' +
      ':subject_distance, ' +
      ':flight_height, ' +
      ':flight_direction, ' +
      ':subject_seen, ' +
      ':subject_heard, ' +
      ':subject_photographed, ' +
      ':subject_recorded, ' +
      ':subject_captured, ' +
      ':males_tally, ' +
      ':females_tally, ' +
      ':not_sexed_tally, ' +
      ':adults_tally, ' +
      ':immatures_tally, ' +
      ':not_aged_tally, ' +
      ':new_captures_tally, ' +
      ':recaptures_tally, ' +
      ':unbanded_tally, ' +
      ':detection_type, ' +
      ':breeding_status, ' +
      ':out_of_sample, ' +
      ':ebird_available, ' +
      ':full_name, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TSightingsSQL.SelectAll(aWhere: TSQLWhereClause; aParent: TTableType): String;
begin
  Result :=
    'SELECT s.*, ' +
      'z.scientific_name AS taxon_name, ' +
      'z.formatted_name AS taxon_formatted_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id, ' +
      'i.full_name AS individual_name, ' +
      'p.full_name AS observer_name, ' +
      'sv.full_name AS survey_name, ' +
      'mt.method_name AS method_name, ' +
      'g.full_name AS locality_name, ' +
      'g.country_id AS country_id, ' +
      'g.state_id AS state_id, ' +
      'g.municipality_id AS municipality_id ' +
    'FROM sightings AS s ' +
    'LEFT JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id ' +
    'LEFT JOIN individuals AS i ON s.individual_id = i.individual_id ' +
    'LEFT JOIN people AS p ON s.observer_id = p.person_id ' +
    'LEFT JOIN surveys AS sv ON s.survey_id = sv.survey_id ' +
    'LEFT JOIN methods AS mt ON s.method_id = mt.method_id ' +
    'LEFT JOIN gazetteer AS g ON s.locality_id = g.site_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (s.sighting_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (s.sighting_id = :sighting_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (s.sighting_id = -1) AND (s.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (s.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (s.active_status = 1) AND (s.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (s.active_status = 0) ';
    swcActiveParent:
      begin
        if aParent = tbIndividuals then
          Result := Result + 'WHERE (s.active_status = 1) AND (s.individual_id = :individual_id) '
        else
        if aParent = tbSurveys then
          Result := Result + 'WHERE (s.active_status = 1) AND (s.survey_id = :survey_id) ';
      end;
    swcFindText: ;
  end;
end;

function TSightingsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', s.sighting_date) AS ano, ' +
      'strftime(''%m'', s.sighting_date) AS mes, ' +
      'strftime(''%d'', s.sighting_date) AS dia ' +
    'FROM sightings AS s ' +
    'WHERE (s.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TSightingsSQL.SelectTable(aWhere: TSQLWhereClause; aParent: TTableType): String;
begin
  Result :=
    'SELECT ' +
      'sighting_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'sighting_date, ' +
      'sighting_time, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'method_id, ' +
      'mackinnon_list_num, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'subjects_tally, ' +
      'subject_distance, ' +
      'flight_height, ' +
      'flight_direction, ' +
      'subject_seen, ' +
      'subject_heard, ' +
      'subject_photographed, ' +
      'subject_recorded, ' +
      'subject_captured, ' +
      'males_tally, ' +
      'females_tally, ' +
      'not_sexed_tally, ' +
      'adults_tally, ' +
      'immatures_tally, ' +
      'not_aged_tally, ' +
      'new_captures_tally, ' +
      'recaptures_tally, ' +
      'unbanded_tally, ' +
      'detection_type, ' +
      'breeding_status, ' +
      'out_of_sample, ' +
      'ebird_available, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM sightings ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (sighting_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (sighting_id = :sighting_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (sighting_id = -1) AND (active_status = 1) ';
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

function TSightingsSQL.Update: String;
begin
  Result :=
    'UPDATE sightings SET ' +
      'survey_id = :survey_id, ' +
      'individual_id = :individual_id, ' +
      'sighting_date = date(:sighting_date), ' +
      //'sighting_time = (CASE WHEN :sighting_time IS NULL THEN NULL ELSE time(:sighting_time) END),' +
      'sighting_time = time(:sighting_time), ' +
      'locality_id = :locality_id, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'method_id = :method_id, ' +
      'mackinnon_list_num = :mackinnon_list_num, ' +
      'observer_id = :observer_id, ' +
      'taxon_id = :taxon_id, ' +
      'subjects_tally = :subjects_tally, ' +
      'subject_distance = :subject_distance, ' +
      'flight_height = :flight_height, ' +
      'flight_direction = :flight_direction, ' +
      'subject_seen = :subject_seen, ' +
      'subject_heard = :subject_heard, ' +
      'subject_photographed = :subject_photographed, ' +
      'subject_recorded = :subject_recorded, ' +
      'subject_captured = :subject_captured, ' +
      'males_tally = :males_tally, ' +
      'females_tally = :females_tally, ' +
      'not_sexed_tally = :not_sexed_tally, ' +
      'adults_tally = :adults_tally, ' +
      'immatures_tally = :immatures_tally, ' +
      'not_aged_tally = :not_aged_tally, ' +
      'new_captures_tally = :new_captures_tally, ' +
      'recaptures_tally = :recaptures_tally, ' +
      'unbanded_tally = :unbanded_tally, ' +
      'detection_type = :detection_type, ' +
      'breeding_status = :breeding_status, ' +
      'out_of_sample = :out_of_sample, ' +
      'ebird_available = :ebird_available, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (sighting_id = :sighting_id) ';
end;

end.

