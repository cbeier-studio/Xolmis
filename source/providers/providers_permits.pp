unit providers_permits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TPermitsSQL }

  TPermitsSQL = class(TInterfacedObject, IPermitsSQL)
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

{ TPermitsSQL }

constructor TPermitsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TPermitsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS permits (' +
      'permit_id       INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'project_id      INTEGER       REFERENCES projects (project_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'permit_name     VARCHAR (150),' +
      'permit_number   VARCHAR (30),' +
      'permit_type     VARCHAR (5),' +
      'dispatcher_name VARCHAR (100) NOT NULL,' +
      'dispatch_date   DATE,' +
      'expire_date     DATE,' +
      'permit_status VARCHAR (5),' +
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

function TPermitsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM permits ' +
    'WHERE permit_id = :aid';
end;

function TPermitsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT permit_id, permit_name FROM permits ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE (permit_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
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

function TPermitsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO permits (' +
      'project_id, ' +
      'permit_name, ' +
      'permit_number, ' +
      'permit_type, ' +
      'dispatcher_name, ' +
      'dispatch_date, ' +
      'expire_date, ' +
      'notes, ' +
      'permit_status, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':project_id, ' +
      ':permit_name, ' +
      ':permit_number, ' +
      ':permit_type, ' +
      ':dispatcher_name, ' +
      'date(:dispatch_date), ' +
      'date(:expire_date), ' +
      ':notes, ' +
      ':permit_status, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TPermitsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT l.*, ' +
      'pj.short_title AS project_name ' +
    'FROM permits AS l ' +
    'LEFT JOIN projects AS pj ON l.project_id = pj.project_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (l.permit_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (l.permit_id = :permit_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (l.permit_id = -1) AND (l.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (l.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (l.active_status = 1) AND (l.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (l.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TPermitsSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', l.dispatch_date) AS ano, ' +
      'strftime(''%m'', l.dispatch_date) AS mes, ' +
      'strftime(''%d'', l.dispatch_date) AS dia ' +
    'FROM permits AS l ' +
    'WHERE (l.active_status = 1) ' +
    'UNION ' +
    'SELECT ' +
      'strftime(''%Y'', l.expire_date) AS ano, ' +
      'strftime(''%m'', l.expire_date) AS mes, ' +
      'strftime(''%d'', l.expire_date) AS dia ' +
    'FROM permits AS l ' +
    'WHERE (l.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TPermitsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'permit_id, ' +
      'project_id, ' +
      'permit_name, ' +
      'permit_number, ' +
      'permit_type, ' +
      'dispatcher_name, ' +
      'dispatch_date, ' +
      'expire_date, ' +
      'notes, ' +
      'permit_status, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM permits ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (permit_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (permit_id = :permit_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (permit_id = -1) AND (active_status = 1) ';
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

function TPermitsSQL.Update: String;
begin
  Result :=
    'UPDATE permits SET ' +
      'project_id = :project_id, ' +
      'permit_name = :permit_name, ' +
      'permit_number = :permit_number, ' +
      'permit_type = :permit_type, ' +
      'dispatcher_name = :dispatcher_name, ' +
      'dispatch_date = date(:dispatch_date), ' +
      'expire_date = date(:expire_date), ' +
      'notes = :notes, ' +
      'permit_status = :permit_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (permit_id = :permit_id) ';
end;

end.

