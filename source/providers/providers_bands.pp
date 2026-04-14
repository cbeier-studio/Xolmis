unit providers_bands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TBandsSQL }

  TBandsSQL = class(TInterfacedObject, IBandsSQL)
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
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; OnlyAvailable: Boolean = False): String;
  end;

  { TBandHistorySQL }

  TBandHistorySQL = class(TInterfacedObject, IBandHistorySQL)
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

{ TBandsSQL }

constructor TBandsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TBandsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS bands (' +
      'band_id         INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'band_size       VARCHAR (5),' +
      'band_number     INTEGER,' +
      'band_status     VARCHAR (5),' +
      'band_type       VARCHAR (5),' +
      'band_prefix     VARCHAR (10),' +
      'band_suffix     VARCHAR (10),' +
      'band_color      VARCHAR (10),' +
      'band_source     VARCHAR (5),' +
      'supplier_id     INTEGER      REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
      'requester_id    INTEGER      REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'carrier_id      INTEGER      REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'individual_id   INTEGER      REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'project_id      INTEGER,' +
      'band_reported   BOOLEAN      DEFAULT (0),' +
      'notes           TEXT,' +
      'full_name       VARCHAR (40),' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN      DEFAULT (0),' +
      'marked_status   BOOLEAN      DEFAULT (0),' +
      'active_status   BOOLEAN      DEFAULT (1)' +
    ');';
end;

function TBandsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM bands ' +
    'WHERE band_id = :aid';
end;

function TBandsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType; OnlyAvailable: Boolean): String;
begin
  Result :=
    'SELECT band_id, full_name FROM bands ';

    case aWhere of
      swcNone: ;
      swcId: ;
      swcUpdateId: ;
      swcFieldValue: ;
      swcActiveEmpty:
        Result := Result + 'WHERE (active_status = 1) AND (band_id = -1) ';
      swcActiveAll:
        Result := Result + 'WHERE (active_status = 1) ';
      swcActiveMarked:
        Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1) ';
      swcInactive:
        Result := Result + 'WHERE (active_status = 0) ';
      swcActiveParent: ;
      swcFindText:
      begin
        Result := Result +
          'WHERE (full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ';
        if OnlyAvailable then
          Result := Result + 'AND (band_status = ''A'') ';
        Result := Result + 'AND (active_status = 1) ';
      end;
    end;
end;

function TBandsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO bands (' +
      'band_size, ' +
      'band_number, ' +
      'band_status, ' +
      'band_type, ' +
      'band_prefix, ' +
      'band_suffix, ' +
      'band_color, ' +
      'band_source, ' +
      'supplier_id, ' +
      'requester_id, ' +
      'carrier_id, ' +
      'project_id, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':band_size, ' +
      ':band_number, ' +
      ':band_status, ' +
      ':band_type, ' +
      ':band_prefix, ' +
      ':band_suffix, ' +
      ':band_color, ' +
      ':band_source, ' +
      ':supplier_id, ' +
      ':requester_id, ' +
      ':carrier_id, ' +
      ':project_id, ' +
      ':notes, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));';
end;

function TBandsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT b.*, ' +
      'it.acronym AS supplier_name, ' +
      'p1.full_name AS requester_name, ' +
      'p2.full_name AS carrier_name, ' +
      'i.full_name AS individual_name, ' +
      'pj.short_title AS project_name ' +
    'FROM bands AS b ' +
    'LEFT JOIN institutions AS it ON b.supplier_id = it.institution_id ' +
    'LEFT JOIN people AS p1 ON b.requester_id = p1.person_id ' +
    'LEFT JOIN people AS p2 ON b.carrier_id = p2.person_id ' +
    'LEFT JOIN individuals AS i ON b.individual_id = i.individual_id ' +
    'LEFT JOIN projects AS pj ON b.project_id = pj.project_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (b.band_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (b.band_id = :band_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (b.band_id = -1) AND (b.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (b.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (b.active_status = 1) AND (b.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (b.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TBandsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'band_id, ' +
      'band_size, ' +
      'band_number, ' +
      'band_status, ' +
      'band_type, ' +
      'band_prefix, ' +
      'band_suffix, ' +
      'band_color, ' +
      'band_source, ' +
      'supplier_id, ' +
      'requester_id, ' +
      'carrier_id, ' +
      'individual_id, ' +
      'project_id, ' +
      'band_reported, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM bands ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (band_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (band_id = :band_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (band_id = -1) AND (active_status = 1) ';
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

function TBandsSQL.Update: String;
begin
  Result :=
    'UPDATE bands SET ' +
      'band_size = :band_size, ' +
      'band_number = :band_number, ' +
      'band_status = :band_status, ' +
      'band_type = :band_type, ' +
      'band_prefix = :band_prefix, ' +
      'band_suffix = :band_suffix, ' +
      'band_color = :band_color, ' +
      'band_source = :band_source, ' +
      'supplier_id = :supplier_id, ' +
      'requester_id = :requester_id, ' +
      'carrier_id = :carrier_id, ' +
      'project_id = :project_id, ' +
      'notes = :notes, ' +
      'full_name = :full_name, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (band_id = :band_id) ';
end;

{ TBandHistorySQL }

constructor TBandHistorySQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TBandHistorySQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS band_history (' +
      'event_id        INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
      'band_id         INTEGER  REFERENCES bands (band_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'event_type      CHAR (5) NOT NULL,' +
      'event_date      DATE,' +
      'order_number    INTEGER,' +
      'supplier_id     INTEGER  REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
      'sender_id       INTEGER  REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'requester_id    INTEGER  REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'notes           TEXT,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN  DEFAULT (0),' +
      'marked_status   BOOLEAN  DEFAULT (0),' +
      'active_status   BOOLEAN  DEFAULT (1)' +
    ');';
end;

function TBandHistorySQL.Delete: String;
begin
  Result :=
    'DELETE FROM band_history ' +
    'WHERE event_id = :aid';
end;

function TBandHistorySQL.Insert: String;
begin
  Result :=
    'INSERT INTO band_history (' +
      'band_id, ' +
      'event_date, ' +
      'notes, ' +
      'event_type, ' +
      'supplier_id, ' +
      'order_number, ' +
      'requester_id, ' +
      'sender_id, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':band_id, ' +
      'date(:event_date), ' +
      ':notes, ' +
      ':event_type, ' +
      ':supplier_id, ' +
      ':order_number, ' +
      ':requester_id, ' +
      ':sender_id, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TBandHistorySQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT bh.*, ' +
      'it.acronym AS supplier_name, ' +
      'p1.full_name AS requester_name, ' +
      'p2.full_name AS sender_name ' +
    'FROM band_history AS bh ' +
    'LEFT JOIN institutions AS it ON bh.supplier_id = it.institution_id ' +
    'LEFT JOIN people AS p1 ON bh.requester_id = p1.person_id ' +
    'LEFT JOIN people AS p2 ON bh.sender_id = p2.person_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (bh.event_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (bh.event_id = :event_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (bh.event_id = -1) AND (bh.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (bh.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (bh.active_status = 1) AND (bh.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (bh.active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (bh.active_status = 1) AND (bh.band_id = :band_id) ';
    swcFindText: ;
  end;
end;

function TBandHistorySQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'event_id, ' +
      'band_id, ' +
      'event_type, ' +
      'event_date, ' +
      'order_number, ' +
      'supplier_id, ' +
      'sender_id, ' +
      'requester_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM band_history ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (event_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (event_id = :event_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (event_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (band_id = :band_id) ';
    swcFindText: ;
  end;
end;

function TBandHistorySQL.Update: String;
begin
  Result :=
    'UPDATE band_history SET ' +
      'band_id = :band_id, ' +
      'event_date = date(:event_date), ' +
      'notes = :notes, ' +
      'event_type = :event_type, ' +
      'supplier_id = :supplier_id, ' +
      'order_number = :order_number, ' +
      'requester_id = :requester_id, ' +
      'sender_id = :sender_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (event_id = :event_id) ';
end;

end.

