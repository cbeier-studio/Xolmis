unit providers_system;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TConnectionsSQL }

  TConnectionsSQL = class(TInterfacedObject, IConnectionsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function CreateIndexConnectionName: String;
    function SelectTable(aWhere: TSQLWhereClause): String;
    function SelectAll(aWhere: TSQLWhereClause): String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

  { TUsersSQL }

  TUsersSQL = class(TInterfacedObject, IUsersSQL)
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

  { TDBMetadataSQL }

  TDBMetadataSQL = class(TInterfacedObject, IDBMetadataSQL)
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

  { TRecordHistorySQL }

  TRecordHistorySQL = class(TInterfacedObject, IRecordHistorySQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable: String;
    function SelectAll: String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

  { TRecordVerificationsSQL }

  TRecordVerificationsSQL = class(TInterfacedObject, IRecordVerificationsSQL)
  private
    FBackend: TDatabaseBackend;
  public
    constructor Create(ABackend: TDatabaseBackend);

    function CreateTable: String;
    function SelectTable: String;
    function SelectAll: String;
    function Insert: String;
    function Update: String;
    function Delete: String;
  end;

implementation

{ TConnectionsSQL }

constructor TConnectionsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TConnectionsSQL.CreateIndexConnectionName: String;
begin
  Result :=
    'CREATE UNIQUE INDEX idx_connection_name ON connections ( ' +
      'connection_name COLLATE NOCASE );'
end;

function TConnectionsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS connections ( ' +
      'connection_id    INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL, ' +
      'connection_name  VARCHAR (40)  NOT NULL, ' +
      'database_type    INTEGER       NOT NULL, ' +
      'database_server  VARCHAR (30), ' +
      'database_port    INTEGER, ' +
      'database_name    VARCHAR (200) NOT NULL, ' +
      'user_name        VARCHAR (20), ' +
      'user_password    TEXT, ' +
      'last_backup      DATETIME,' +
      'insert_date      DATETIME, ' +
      'update_date      DATETIME );';
end;

function TConnectionsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM connections ' +
    'WHERE connection_id = :aid';
end;

function TConnectionsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO connections (' +
      'connection_name, ' +
      'database_type, ' +
      'database_server, ' +
      'database_port, ' +
      'database_name, ' +
      'user_name, ' +
      'user_password, ' +
      'last_backup, ' +
      'insert_date) ' +
    'VALUES (' +
      ':connection_name, ' +
      ':database_type, ' +
      ':database_server, ' +
      ':database_port, ' +
      ':database_name, ' +
      ':user_name, ' +
      ':user_password, ' +
      'datetime(:last_backup), ' +
      'datetime(''now'',''subsec''))';
end;

function TConnectionsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM connections ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (connection_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (connection_id = :connection_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (connection_id = -1) AND (active_status = 1) ';
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

function TConnectionsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'connection_id, ' +
      'connection_name, ' +
      'database_type, ' +
      'database_server, ' +
      'database_port, ' +
      'database_name, ' +
      'user_name, ' +
      'user_password, ' +
      'datetime(last_backup, ''localtime'') AS last_backup, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date ' +
    'FROM connections ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (connection_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (connection_id = :connection_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (connection_id = -1) AND (active_status = 1) ';
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

function TConnectionsSQL.Update: String;
begin
  Result :=
    'UPDATE connections SET ' +
      'connection_name = :connection_name, ' +
      'database_type = :database_type, ' +
      'database_server = :database_server, ' +
      'database_port = :database_port, ' +
      'database_name = :database_name, ' +
      'user_name = :user_name, ' +
      'user_password = :user_password, ' +
      'last_backup = datetime(:last_backup), ' +
      'insert_date = datetime(''now'',''subsec'') ' +
    'WHERE (connection_id = :connection_id) ';
end;

{ TUsersSQL }

constructor TUsersSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TUsersSQL.CreateTable: String;
begin
  case FBackend of
    dbSqlite,
    dbPostgre,
    dbMaria:
      Result :=
        'CREATE TABLE IF NOT EXISTS users (' +
          'user_id               INTEGER      PRIMARY KEY AUTOINCREMENT,' +
          'full_name             VARCHAR (60) NOT NULL,' +
          'user_name             VARCHAR (30) UNIQUE NOT NULL,' +
          'user_password         TEXT,' +
          'user_rank             VARCHAR (5),' +
          'allow_collection_edit BOOLEAN      DEFAULT (1),' +
          'allow_print           BOOLEAN      DEFAULT (1),' +
          'allow_export          BOOLEAN      DEFAULT (1),' +
          'allow_import          BOOLEAN      DEFAULT (1),' +
          'uuid                  VARCHAR (40),' +
          'user_inserted         INTEGER,' +
          'user_updated          INTEGER,' +
          'insert_date           DATETIME,' +
          'update_date           DATETIME,' +
          'exported_status       BOOLEAN      DEFAULT (0),' +
          'marked_status         BOOLEAN      DEFAULT (0),' +
          'active_status         BOOLEAN      DEFAULT (1),' +
          'inactivated_by        VARCHAR (5)' +
        ');';
    dbFirebird:
      Result :=
        'CREATE TABLE users (' +
          'user_id               INTEGER      GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,' +
          'full_name             VARCHAR(60)  NOT NULL,' +
          'user_name             VARCHAR(30)  UNIQUE NOT NULL,' +
          'user_password         BLOB SUB_TYPE TEXT,' +
          'user_rank             VARCHAR(5),' +
          'allow_collection_edit BOOLEAN      DEFAULT TRUE,' +
          'allow_print           BOOLEAN      DEFAULT TRUE,' +
          'allow_export          BOOLEAN      DEFAULT TRUE,' +
          'allow_import          BOOLEAN      DEFAULT TRUE,' +
          'uuid                  VARCHAR(40),' +
          'user_inserted         INTEGER,' +
          'user_updated          INTEGER,' +
          'insert_date           TIMESTAMP,' +
          'update_date           TIMESTAMP,' +
          'exported_status       BOOLEAN      DEFAULT FALSE,' +
          'marked_status         BOOLEAN      DEFAULT FALSE,' +
          'active_status         BOOLEAN      DEFAULT TRUE,' +
          'inactivated_by        VARCHAR(5)' +
        ');';
  end;
end;

function TUsersSQL.Delete: String;
begin
  Result :=
    'DELETE FROM users ' +
    'WHERE user_id = :aid';
end;

function TUsersSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT user_id, user_name, full_name FROM users ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
            'OR (user_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM)) ' +
          'AND (active_status = 1) ';
    end;
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (marked_status = 1) AND (active_status = 1)';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0)';
  end;
end;

function TUsersSQL.Insert: String;
begin
  Result :=
    'INSERT INTO users (' +
      'full_name, ' +
      'user_name, ' +
      //'user_password, ' +
      'user_rank, ' +
      'allow_collection_edit, ' +
      'allow_print, ' +
      'allow_export, ' +
      'allow_import, ' +
      'uuid, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':full_name, ' +
      ':user_name, ' +
      //':user_password, ' +
      ':user_rank, ' +
      ':allow_collection_edit, ' +
      ':allow_print, ' +
      ':allow_export, ' +
      ':allow_import, ' +
      ':uuid, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TUsersSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM users ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (user_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (user_id = :user_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (user_id = -1) AND (active_status = 1) ';
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

function TUsersSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'user_id, ' +
      'full_name, ' +
      'user_name, ' +
      'user_password, ' +
      'user_rank, ' +
      'allow_collection_edit, ' +
      'allow_print, ' +
      'allow_export, ' +
      'allow_import, ' +
      'uuid, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM users ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (user_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (user_id = :user_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (user_id = -1) AND (active_status = 1) ';
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

function TUsersSQL.Update: String;
begin
  Result :=
    'UPDATE users SET ' +
      'full_name = :full_name, ' +
      'user_name = :user_name, ' +
      //'user_password = :user_password, ' +
      'user_rank = :user_rank, ' +
      'allow_collection_edit = :allow_collection_edit, ' +
      'allow_print = :allow_print, ' +
      'allow_export = :allow_export, ' +
      'allow_import = :allow_import, ' +
      'uuid = :uuid, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (user_id = :user_id) ';
end;

{ TDBMetadataSQL }

constructor TDBMetadataSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TDBMetadataSQL.CreateTable: String;
begin
  case FBackend of
    dbSqlite,
    dbPostgre,
    dbMaria:
      Result :=
        'CREATE TABLE IF NOT EXISTS db_metadata (' +
          'property_name  VARCHAR (40)  PRIMARY KEY UNIQUE NOT NULL, ' +
          'property_value VARCHAR (150) );';
    dbFirebird:
      Result :=
        'CREATE TABLE db_metadata (' +
          'property_name VARCHAR(40) PRIMARY KEY, ' +
          'property_value VARCHAR(150)' +
        ');';
  end;
end;

function TDBMetadataSQL.Delete: String;
begin
  Result :=
    'DELETE FROM db_metadata ' +
    'WHERE property_name = :aid';
end;

function TDBMetadataSQL.Insert: String;
begin
  case FBackend of
    dbSqlite:
    begin
      Result :=
        'INSERT OR REPLACE INTO db_metadata (' +
          'property_name, ' +
          'property_value) ' +
        'VALUES (' +
          ':property_name, ' +
          ':property_value)';
    end;
    dbFirebird: ;
    dbPostgre:
    begin
      Result :=
        'INSERT INTO db_metadata (property_name, property_value) ' +
        'VALUES (:aname, :avalue) ' +
        'ON CONFLICT (property_name) ' +
        'DO UPDATE SET property_value = EXCLUDED.property_value';
    end;
    dbMaria: ;
  end;
end;

function TDBMetadataSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM db_metadata ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (property_name = :aname) ';
    swcUpdateId:
      Result := Result + 'WHERE (property_name = :property_name) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (property_name ISNULL) ';
    swcActiveAll: ;
    swcActiveMarked: ;
    swcInactive: ;
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TDBMetadataSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'property_name, ' +
      'property_value ' +
    'FROM db_metadata ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (property_name = :aname) ';
    swcUpdateId:
      Result := Result + 'WHERE (property_name = :property_name) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (property_name ISNULL) ';
    swcActiveAll: ;
    swcActiveMarked: ;
    swcInactive: ;
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TDBMetadataSQL.Update: String;
begin
  Result :=
    'UPDATE db_metadata SET ' +
      'property_value = :property_value ' +
    'WHERE (property_name = :property_name) ';
end;

{ TRecordHistorySQL }

constructor TRecordHistorySQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TRecordHistorySQL.CreateTable: String;
begin
  case FBackend of
    dbSqlite,
    dbPostgre,
    dbMaria:
      Result :=
        'CREATE TABLE IF NOT EXISTS record_history (' +
          'event_id     INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
          'event_date   DATETIME,' +
          'user_id      INTEGER      REFERENCES users (user_id) ON DELETE SET NULL ON UPDATE CASCADE,' +
          'event_action VARCHAR (30),' +
          'event_table  VARCHAR (40),' +
          'record_id    INTEGER,' +
          'event_field  VARCHAR (60),' +
          'old_value    TEXT,' +
          'new_value    TEXT,' +
          'notes        TEXT,' +
          'FOREIGN KEY (user_id)' +
            'REFERENCES users (user_id) ON DELETE SET NULL ON UPDATE CASCADE' +
        ');';
    dbFirebird:
      Result :=
        'CREATE TABLE record_history (' +
          'event_id     INTEGER      GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,' +
          'event_date   TIMESTAMP,' +
          'user_id      INTEGER      REFERENCES users(user_id) ON DELETE SET NULL ON UPDATE CASCADE,' +
          'event_action VARCHAR(30),' +
          'event_table  VARCHAR(40),' +
          'record_id    INTEGER,' +
          'event_field  VARCHAR(60),' +
          'old_value    BLOB SUB_TYPE TEXT,' +
          'new_value    BLOB SUB_TYPE TEXT,' +
          'notes        BLOB SUB_TYPE TEXT' +
        ');';
  end;
end;

function TRecordHistorySQL.Delete: String;
begin
  Result :=
    'DELETE FROM record_history ' +
    'WHERE event_id = :aid';
end;

function TRecordHistorySQL.Insert: String;
begin
  Result :=
    'INSERT INTO record_history (' +
      'event_date, ' +
      'user_id, ' +
      'event_action, ' +
      'event_table, ' +
      'record_id, ' +
      'event_field, ' +
      'old_value, ' +
      'new_value, ' +
      'notes) ' +
    'VALUES (' +
      'datetime(''now'',''localtime''), ' +
      ':user_id, ' +
      ':event_action, ' +
      ':event_table, ' +
      ':record_id, ' +
      ':event_field, ' +
      ':old_value, ' +
      ':new_value, ' +
      ':notes)';
end;

function TRecordHistorySQL.SelectAll: String;
begin
  Result :=
    'SELECT h.event_date, h.event_action, h.event_field, ' +
      'h.old_value, h.new_value, h.notes, h.user_id, ' +
      'u.user_name AS user_name ' +
    'FROM record_history AS h ' +
    'LEFT JOIN users AS u ON h.user_id = u.user_id ' +
    'WHERE h.event_table = :tabname ' +
      'AND h.record_id = :cod ';
end;

function TRecordHistorySQL.SelectTable: String;
begin
  Result :=
    'SELECT ' +
      'event_id, ' +
      'event_date, ' +
      'user_id, ' +
      'event_action, ' +
      'event_table, ' +
      'record_id, ' +
      'event_field, ' +
      'old_value, ' +
      'new_value, ' +
      'notes ' +
    'FROM record_history ';
end;

function TRecordHistorySQL.Update: String;
begin
  Result :=
    'UPDATE record_history SET ' +
      'event_date = datetime(:event_date), ' +
      'user_id = :user_id, ' +
      'event_action = :event_action, ' +
      'event_table = :event_table, ' +
      'record_id = :record_id, ' +
      'event_field = :event_field, ' +
      'old_value = :old_value, ' +
      'new_value = :new_value, ' +
      'notes = :notes ' +
    'WHERE (event_id = :event_id) ';
end;

{ TRecordVerificationsSQL }

constructor TRecordVerificationsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TRecordVerificationsSQL.CreateTable: String;
begin
  case FBackend of
    dbSqlite,
    dbPostgre,
    dbMaria:
      Result :=
        'CREATE TABLE IF NOT EXISTS record_verifications (' +
          'verification_id     INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
          'table_name          VARCHAR (40) NOT NULL,' +
          'record_id           INTEGER      NOT NULL,' +
          'verification_date   DATETIME,' +
          'verification_status VARCHAR (5)  NOT NULL,' +
          'person_id           INTEGER      REFERENCES people (person_id) ON UPDATE CASCADE,' +
          'notes               TEXT' +
        ');';
    dbFirebird:
      Result :=
        'CREATE TABLE record_verifications (' +
          'verification_id     INTEGER      GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,' +
          'table_name          VARCHAR(40)  NOT NULL,' +
          'record_id           INTEGER      NOT NULL,' +
          'verification_date   TIMESTAMP,' +
          'verification_status VARCHAR(5)   NOT NULL,' +
          'person_id           INTEGER      REFERENCES people(person_id) ON UPDATE CASCADE,' +
          'notes               BLOB SUB_TYPE TEXT' +
        ');';
  end;
end;

function TRecordVerificationsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM record_verifications ' +
    'WHERE verification_id = :aid';
end;

function TRecordVerificationsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO record_verifications (' +
      'table_name, ' +
      'record_id, ' +
      'verification_date, ' +
      'verification_status, ' +
      'person_id) ' +
      'notes ' +
    'VALUES (' +
      ':table_name, ' +
      ':record_id, ' +
      'datetime(:verification_date), ' +
      ':verification_status, ' +
      ':person_id, ' +
      ':notes)';
end;

function TRecordVerificationsSQL.SelectAll: String;
begin
  Result :=
    'SELECT h.verification_date, h.verification_status, h.table_name, ' +
      'h.record_id, h.verification_id, h.notes, h.person_id, ' +
      'p.full_name AS person_name ' +
    'FROM record_verifications AS h ' +
    'LEFT JOIN people AS p ON h.person_id = p.person_id ' +
    'WHERE h.table_name = :tabname ' +
      'AND h.record_id = :cod ';
end;

function TRecordVerificationsSQL.SelectTable: String;
begin
  Result :=
    'SELECT ' +
      'verification_id, ' +
      'table_name, ' +
      'record_id, ' +
      'verification_date, ' +
      'verification_status, ' +
      'person_id, ' +
      'notes ' +
    'FROM record_verifications ';
end;

function TRecordVerificationsSQL.Update: String;
begin
  Result :=
    'UPDATE record_verifications SET ' +
      'table_name = :table_name, ' +
      'record_id = :record_id, ' +
      'verification_date = datetime(:verification_date), ' +
      'verification_status = :verification_status, ' +
      'person_id = :person_id, ' +
      'notes = :notes ' +
    'WHERE (verification_id = :verification_id) ';
end;

end.

