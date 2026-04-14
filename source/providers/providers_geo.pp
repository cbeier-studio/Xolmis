unit providers_geo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types, models_record_types;

type

  { TGazetteerSQL }

  TGazetteerSQL = class(TInterfacedObject, IGazetteerSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function SelectHierarchy: String;
    function SelectTree(const aTableFilter: TTableType): String;
    function Insert: String;
    function Update: String;
    function UpdateHierarchy: String;
    function Delete: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TGazetteerFilters): String;
  end;

  { TPoiLibrarySQL }

  TPoiLibrarySQL = class(TInterfacedObject, IPoiLibrarySQL)
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

{ TGazetteerSQL }

constructor TGazetteerSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TGazetteerSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS gazetteer (' +
      'site_id         INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'site_name       VARCHAR (60)  NOT NULL,' +
      'site_acronym    VARCHAR (10),' +
      'longitude       REAL,' +
      'latitude        REAL,' +
      'altitude        REAL,' +
      'site_rank       CHAR (1),' +
      'parent_site_id  INTEGER,' +
      'country_id      INTEGER,' +
      'state_id        INTEGER,' +
      'municipality_id INTEGER,' +
      'full_name       VARCHAR (180),' +
      'ebird_name      VARCHAR (150),' +
      'language        VARCHAR (10),' +
      'description     TEXT,' +
      'notes           TEXT,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN       DEFAULT (0),' +
      'marked_status   BOOLEAN       DEFAULT (0),' +
      'active_status   BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TGazetteerSQL.Delete: String;
begin
  Result :=
    'DELETE FROM gazetteer ' +
    'WHERE site_id = :aid';
end;

function TGazetteerSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; aRankFilter: TGazetteerFilters
  ): String;
var
  F: TGazetteerFilter;
  strFiltro, strOr: String;
begin
  strOr := EmptyStr;
  strFiltro := EmptyStr;

  Result := 'SELECT site_id, full_name, site_name, site_acronym FROM gazetteer ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
          'OR (site_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
          'OR (site_acronym ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM)) ';
      if not (gfAll in aRankFilter) then
      begin
        Result := Result + 'AND (';
        for F in aRankFilter do
        begin
          case F of
            gfAll: ; // do nothing
            gfCountries:
              strFiltro := strOr + '(site_rank = ''P'') ';
            gfStates:
              strFiltro := strOr + '(site_rank = ''E'') ';
            gfRegions:
              strFiltro := strOr + '(site_rank = ''R'') ';
            gfCities:
              strFiltro := strOr + '(site_rank = ''M'') ';
            gfDistricts:
              strFiltro := strOr + '(site_rank = ''D'') ';
            gfLocalities:
              strFiltro := strOr + '(site_rank = ''L'') ';
          end;
          Result := Result + strFiltro;
          strOr := 'OR ';
        end;
        Result := Result + ')';
      end;
      Result := Result + 'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
  end;
end;

function TGazetteerSQL.Insert: String;
begin
  Result :=
    'INSERT INTO gazetteer (' +
      'site_name, ' +
      'site_acronym, ' +
      'longitude, ' +
      'latitude, ' +
      'altitude, ' +
      'site_rank, ' +
      'parent_site_id, ' +
      'country_id, ' +
      'state_id, ' +
      'municipality_id, ' +
      'full_name, ' +
      'ebird_name, ' +
      'language, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':site_name, ' +
      ':site_acronym, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':altitude, ' +
      ':site_rank, ' +
      ':parent_site_id, ' +
      ':country_id, ' +
      ':state_id, ' +
      ':municipality_id, ' +
      ':full_name, ' +
      ':ebird_name, ' +
      ':language, ' +
      ':description, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TGazetteerSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT g.*, ' +
      'gp.site_name AS parent_site_name, ' +
      'gm.site_name AS municipality_name, ' +
      'gs.site_name AS state_name, ' +
      'gc.site_name AS country_name ' +
    'FROM gazetteer AS g ' +
    'LEFT JOIN gazetteer AS gp ON g.parent_site_id = gp.site_id ' +
    'LEFT JOIN gazetteer AS gm ON g.municipality_id = gm.site_id ' +
    'LEFT JOIN gazetteer AS gs ON g.state_id = gs.site_id ' +
    'LEFT JOIN gazetteer AS gc ON g.country_id = gc.site_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (g.site_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (g.site_id = :site_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (g.site_id = -1) AND (g.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (g.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (g.active_status = 1) AND (g.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (g.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TGazetteerSQL.SelectHierarchy: String;
begin
  Result :=
    'SELECT country_id, state_id, municipality_id FROM gazetteer ' +
    'WHERE site_id = :asite ';
end;

function TGazetteerSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'site_id, ' +
      'site_name, ' +
      'site_acronym, ' +
      'longitude, ' +
      'latitude, ' +
      'altitude, ' +
      'site_rank, ' +
      'parent_site_id, ' +
      'country_id, ' +
      'state_id, ' +
      'municipality_id, ' +
      'full_name, ' +
      'ebird_name, ' +
      'language, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM gazetteer ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (site_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (site_id = :site_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (site_id = -1) AND (active_status = 1) ';
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

function TGazetteerSQL.SelectTree(const aTableFilter: TTableType): String;
begin
  Result :=
    'WITH SiteDetails AS (' +
      'SELECT ' +
        'site_id, ' +
        'site_name, ' +
        'municipality_id, ' +
        'state_id, ' +
        'country_id ' +
      'FROM gazetteer' +
    ') ';

  if (aTableFilter = tbSurveys) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT sv.locality_id, g.municipality_id, g.state_id, g.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM surveys AS sv ' +
      'JOIN SiteDetails AS g ON sv.locality_id = g.site_id ' +
      'JOIN SiteDetails AS m ON g.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON g.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON g.country_id = p.site_id ' +
      'WHERE (sv.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbCaptures) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT c.locality_id, g.municipality_id, g.state_id, g.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM captures AS c ' +
      'JOIN SiteDetails AS g ON c.locality_id = g.site_id ' +
      'JOIN SiteDetails AS m ON g.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON g.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON g.country_id = p.site_id ' +
      'WHERE (c.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbFeathers) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT ft.locality_id, g.municipality_id, g.state_id, g.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM feathers AS ft ' +
      'JOIN SiteDetails AS g ON ft.locality_id = g.site_id ' +
      'JOIN SiteDetails AS m ON g.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON g.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON g.country_id = p.site_id ' +
      'WHERE (ft.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbSightings) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT st.locality_id, g.municipality_id, g.state_id, g.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM sightings AS st ' +
      'JOIN SiteDetails AS g ON st.locality_id = g.site_id ' +
      'JOIN SiteDetails AS m ON g.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON g.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON g.country_id = p.site_id ' +
      'WHERE (st.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbNests) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT n.locality_id, g.municipality_id, g.state_id, g.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM nests AS n ' +
      'JOIN SiteDetails AS g ON n.locality_id = g.site_id ' +
      'JOIN SiteDetails AS m ON g.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON g.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON g.country_id = p.site_id ' +
      'WHERE (n.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbInstitutions) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT it.municipality_id, it.state_id, it.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM institutions AS it ' +
      'JOIN SiteDetails AS m ON it.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON it.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON it.country_id = p.site_id ' +
      'WHERE (it.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbPeople) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT pp.municipality_id, pp.state_id, pp.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM people AS pp ' +
      'JOIN SiteDetails AS m ON pp.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON pp.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON pp.country_id = p.site_id ' +
      'WHERE (pp.active_status = 1) ';
  if (aTableFilter = tbNone) then
    Result := Result + 'UNION ';
  if (aTableFilter = tbSpecimens) or (aTableFilter = tbNone) then
    Result := Result +
      'SELECT sp.locality_id, g.municipality_id, g.state_id, g.country_id, ' +
        'm.site_name AS municipality_name, ' +
        's.site_name AS state_name, ' +
        'p.site_name AS country_name ' +
      'FROM specimens AS sp ' +
      'JOIN SiteDetails AS g ON sp.locality_id = g.site_id ' +
      'JOIN SiteDetails AS m ON g.municipality_id = m.site_id ' +
      'JOIN SiteDetails AS s ON g.state_id = s.site_id ' +
      'JOIN SiteDetails AS p ON g.country_id = p.site_id ' +
      'WHERE (sp.active_status = 1) ';

  if (aTableFilter = tbInstitutions) then
    Result := Result + 'GROUP BY it.country_id, it.state_id, it.municipality_id '
  else
  if (aTableFilter = tbPeople) then
    Result := Result + 'GROUP BY pp.country_id, pp.state_id, pp.municipality_id '
  else
    Result := Result + 'GROUP BY g.country_id, g.state_id, g.municipality_id ';

  Result := Result +
    'ORDER BY country_name ASC, state_name ASC, municipality_name ASC ';
end;

function TGazetteerSQL.Update: String;
begin
  Result :=
    'UPDATE gazetteer SET ' +
      'site_name = :site_name, ' +
      'site_acronym = :site_acronym, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'altitude = :altitude, ' +
      'site_rank = :site_rank, ' +
      'parent_site_id = :parent_site_id, ' +
      'country_id = :country_id, ' +
      'state_id = :state_id, ' +
      'municipality_id = :municipality_id, ' +
      'full_name = :full_name, ' +
      'ebird_name = :ebird_name, ' +
      'language = :language, ' +
      'description = :description, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (site_id = :site_id) ';
end;

function TGazetteerSQL.UpdateHierarchy: String;
begin
  Result :=
    'UPDATE gazetteer SET ' +
      'country_id = :country_id, ' +
      'state_id = :state_id, ' +
      'municipality_id = :municipality_id ' +
    'WHERE site_id = :aid';
end;

{ TPoiLibrarySQL }

constructor TPoiLibrarySQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TPoiLibrarySQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS poi_library (' +
      'poi_id          INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'sample_date     DATE         NOT NULL,' +
      'sample_time     TIME,' +
      'poi_name        VARCHAR (60),' +
      'longitude       REAL,' +
      'latitude        REAL,' +
      'altitude        REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'observer_id     INTEGER      REFERENCES people (person_id),' +
      'taxon_id        INTEGER      REFERENCES zoo_taxa (taxon_id),' +
      'individual_id   INTEGER      REFERENCES individuals (individual_id) ON DELETE CASCADE,' +
      'sighting_id     INTEGER,' +
      'survey_id       INTEGER      REFERENCES surveys (survey_id),' +
      'notes           TEXT,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN      DEFAULT (0),' +
      'marked_status   BOOLEAN      DEFAULT (0),' +
      'active_status   BOOLEAN      DEFAULT (1)' +
    ');';
end;

function TPoiLibrarySQL.Delete: String;
begin
  Result :=
    'DELETE FROM poi_library ' +
    'WHERE poi_id = :aid';
end;

function TPoiLibrarySQL.Insert: String;
begin
  Result :=
    'INSERT INTO poi_library (' +
      'sample_date, ' +
      'sample_time, ' +
      'poi_name, ' +
      'longitude, ' +
      'latitude, ' +
      'altitude, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'sighting_id, ' +
      'survey_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':poi_name, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':altitude, ' +
      ':observer_id, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':sighting_id, ' +
      ':survey_id, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TPoiLibrarySQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT poi.*, ' +
      'p.full_name AS observer_name, ' +
      'z.full_name AS taxon_name, ' +
      'i.full_name AS individual_name, ' +
      's.full_name AS sighting_name, ' +
      'sv.full_name AS survey_name ' +
    'FROM poi_library AS poi ' +
    'LEFT JOIN people AS p ON poi.observer_id = p.person_id ' +
    'LEFT JOIN zoo_taxa AS z ON poi.taxon_id = z.taxon_id ' +
    'LEFT JOIN individuals AS i ON poi.individual_id = i.individual_id ' +
    'LEFT JOIN sightings AS s ON poi.sighting_id = s.sighting_id ' +
    'LEFT JOIN surveys AS sv ON poi.survey_id = sv.survey_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (poi.poi_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (poi.poi_id = :poi_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (poi.poi_id = -1) AND (poi.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (poi.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (poi.active_status = 1) AND (poi.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (poi.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TPoiLibrarySQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'poi_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'poi_name, ' +
      'longitude, ' +
      'latitude, ' +
      'altitude, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'sighting_id, ' +
      'survey_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM poi_library ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (poi_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (poi_id = :poi_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (poi_id = -1) AND (active_status = 1) ';
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

function TPoiLibrarySQL.Update: String;
begin
  Result :=
    'UPDATE poi_library SET ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'poi_name = :poi_name, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'altitude = :altitude, ' +
      'observer_id = :observer_id, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'sighting_id = :sighting_id, ' +
      'survey_id = :survey_id, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (poi_id = :poi_id) ';
end;

end.

