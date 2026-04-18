unit providers_specimens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TSpecimensSQL }

  TSpecimensSQL = class(TInterfacedObject, ISpecimensSQL)
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

  { TSpecimenCollectorsSQL }

  TSpecimenCollectorsSQL = class(TInterfacedObject, ISpecimenCollectorsSQL)
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

  { TSamplePrepsSQL }

  TSamplePrepsSQL = class(TInterfacedObject, ISamplePrepsSQL)
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

implementation

{ TSpecimensSQL }

constructor TSpecimensSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSpecimensSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS specimens (' +
      'specimen_id      INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'field_number     VARCHAR (20),' +
      'full_name        VARCHAR (100),' +
      'sample_type      CHAR (5),' +
      'taxon_id         INTEGER,' +
      'individual_id    INTEGER,' +
      'nest_id          INTEGER,' +
      'egg_id           INTEGER,' +
      'collection_date  DATE,' +
      'collection_day   INTEGER,' +
      'collection_month INTEGER,' +
      'collection_year  INTEGER,' +
      'locality_id      INTEGER,' +
      'longitude        REAL,' +
      'latitude         REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'institution_id INTEGER REFERENCES institutions(institution_id) ON UPDATE CASCADE,' +
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

function TSpecimensSQL.Delete: String;
begin
  Result :=
    'DELETE FROM specimens ' +
    'WHERE specimen_id = :aid';
end;

function TSpecimensSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT specimen_id, full_name FROM specimens ';

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

function TSpecimensSQL.Insert: String;
begin
  Result :=
    'INSERT INTO specimens (' +
      'field_number, ' +
      'full_name, ' +
      'sample_type, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'nest_id, ' +
      'egg_id, ' +
      'collection_day, ' +
      'collection_month, ' +
      'collection_year, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'institution_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':field_number, ' +
      ':full_name, ' +
      ':sample_type, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':nest_id, ' +
      ':egg_id, ' +
      ':collection_day, ' +
      ':collection_month, ' +
      ':collection_year, ' +
      ':locality_id, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':institution_id, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TSpecimensSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT sp.*, ' +
      'z.scientific_name AS taxon_name, ' +
      'z.order_id AS order_id, ' +
      'z.family_id AS family_id, ' +
      'z.genus_id AS genus_id, ' +
      'z.species_id AS species_id, ' +
      'g.site_name AS locality_name, ' +
      'g.country_id AS country_id, ' +
      'g.state_id AS state_id, ' +
      'g.municipality_id AS municipality_id, ' +
      'i.full_name AS individual_name, ' +
      'n.full_name AS nest_name, ' +
      'e.full_name AS egg_name, ' +
      'it.full_name AS institution_name ' +
    'FROM specimens AS sp ' +
    'LEFT JOIN zoo_taxa AS z ON sp.taxon_id = z.taxon_id ' +
    'LEFT JOIN gazetteer AS g ON sp.locality_id = g.site_id ' +
    'LEFT JOIN individuals AS i ON sp.individual_id = i.individual_id ' +
    'LEFT JOIN nests AS n ON sp.nest_id = n.nest_id ' +
    'LEFT JOIN eggs AS e ON sp.egg_id = e.egg_id ' +
    'LEFT JOIN institutions AS it ON sp.institution_id = it.institution_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (sp.specimen_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (sp.specimen_id = :specimen_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (sp.specimen_id = -1) AND (sp.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (sp.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (sp.active_status = 1) AND (sp.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (sp.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TSpecimensSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', sp.collection_date) AS ano, ' +
      'strftime(''%m'', sp.collection_date) AS mes, ' +
      'strftime(''%d'', sp.collection_date) AS dia ' +
    'FROM specimens AS sp ' +
    'WHERE (sp.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TSpecimensSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'specimen_id, ' +
      'field_number, ' +
      'full_name, ' +
      'sample_type, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'nest_id, ' +
      'egg_id, ' +
      'collection_date, ' +
      'collection_day, ' +
      'collection_month, ' +
      'collection_year, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'institution_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM specimens ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (specimen_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (specimen_id = :specimen_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (specimen_id = -1) AND (active_status = 1) ';
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

function TSpecimensSQL.Update: String;
begin
  Result :=
    'UPDATE specimens SET ' +
      'field_number = :field_number, ' +
      'full_name = :full_name, ' +
      'sample_type = :sample_type, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'nest_id = :nest_id, ' +
      'egg_id = :egg_id, ' +
      'collection_day = :collection_day, ' +
      'collection_month = :collection_month, ' +
      'collection_year = :collection_year, ' +
      'locality_id = :locality_id, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'institution_id = :institution_id, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (specimen_id = :specimen_id) ';
end;

{ TSpecimenCollectorsSQL }

constructor TSpecimenCollectorsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSpecimenCollectorsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS specimen_collectors (' +
      'collector_id    INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'specimen_id     INTEGER,' +
      'person_id       INTEGER,' +
      'collector_seq   INTEGER,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN  DEFAULT (0),' +
      'marked_status   BOOLEAN  DEFAULT (0),' +
      'active_status   BOOLEAN  DEFAULT (1)' +
    ');';
end;

function TSpecimenCollectorsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM specimen_collectors ' +
    'WHERE collector_id = :aid';
end;

function TSpecimenCollectorsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO specimen_collectors (' +
      'specimen_id, ' +
      'person_id, ' +
      'collector_seq, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':specimen_id, ' +
      ':person_id, ' +
      ':collector_seq, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TSpecimenCollectorsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM specimen_collectors ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (collector_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (collector_id = :collector_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (collector_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (specimen_id = :specimen_id) ';
    swcFindText: ;
  end;
end;

function TSpecimenCollectorsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'collector_id, ' +
      'specimen_id, ' +
      'person_id, ' +
      'collector_seq, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM specimen_collectors ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (collector_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (collector_id = :collector_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (collector_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (specimen_id = :specimen_id) ';
    swcFindText: ;
  end;
end;

function TSpecimenCollectorsSQL.Update: String;
begin
  Result :=
    'UPDATE specimen_collectors SET ' +
      'specimen_id = :specimen_id, ' +
      'person_id = :person_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (collector_id = :collector_id) ';
end;

{ TSamplePrepsSQL }

constructor TSamplePrepsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSamplePrepsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS sample_preps (' +
      'sample_prep_id   INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'specimen_id      INTEGER       REFERENCES specimens ON DELETE CASCADE ON UPDATE CASCADE,' +
      'accession_num    VARCHAR (20),' +
      'full_name        VARCHAR (100),' +
      'accession_type   CHAR (5),' +
      'accession_seq    INTEGER,' +
      'taxon_id         INTEGER,' +
      'individual_id    INTEGER,' +
      'nest_id          INTEGER,' +
      'egg_id           INTEGER,' +
      'preparation_date DATE,' +
      'preparer_id      INTEGER,' +
      'institution_id INTEGER REFERENCES institutions(institution_id) ON UPDATE CASCADE,' +
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

function TSamplePrepsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM sample_preps ' +
    'WHERE sample_prep_id = :aid';
end;

function TSamplePrepsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT sample_prep_id, full_name FROM sample_preps ';

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

function TSamplePrepsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO sample_preps (' +
      'specimen_id, ' +
      'accession_num, ' +
      'full_name, ' +
      'accession_type, ' +
      'accession_seq, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'nest_id, ' +
      'egg_id, ' +
      'preparation_date, ' +
      'preparer_id, ' +
      'institution_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':specimen_id, ' +
      ':accession_num, ' +
      ':full_name, ' +
      ':accession_type, ' +
      ':accession_seq, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':nest_id, ' +
      ':egg_id, ' +
      'date(:preparation_date), ' +
      ':preparer_id, ' +
      ':institution_id, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TSamplePrepsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM sample_preps ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (sample_prep_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (sample_prep_id = :sample_prep_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (sample_prep_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (specimen_id = :specimen_id) ';
    swcFindText: ;
  end;
end;

function TSamplePrepsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'sample_prep_id, ' +
      'specimen_id, ' +
      'accession_num, ' +
      'full_name, ' +
      'accession_type, ' +
      'accession_seq, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'nest_id, ' +
      'egg_id, ' +
      'preparation_date, ' +
      'preparer_id, ' +
      'institution_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM sample_preps ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (sample_prep_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (sample_prep_id = :sample_prep_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (sample_prep_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (specimen_id = :specimen_id) ';
    swcFindText: ;
  end;
end;

function TSamplePrepsSQL.Update: String;
begin
  Result :=
    'UPDATE sample_preps SET ' +
      'specimen_id = :specimen_id, ' +
      'accession_num = :accession_num, ' +
      'full_name = :full_name, ' +
      'accession_type = :accession_type, ' +
      'accession_seq = :accession_seq, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'nest_id = :nest_id, ' +
      'egg_id = :egg_id, ' +
      'preparation_date = date(:preparation_date), ' +
      'preparer_id = :preparer_id, ' +
      'institution_id = :institution_id, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ' +
    'WHERE (sample_prep_id = :sample_prep_id) ';
end;

end.

