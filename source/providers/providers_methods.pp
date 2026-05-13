unit providers_methods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TMethodsSQL }

  TMethodsSQL = class(TInterfacedObject, IMethodsSQL)
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
    function DistinctCategories: String;
    function Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
  end;

implementation

{ TMethodsSQL }

constructor TMethodsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TMethodsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS methods (' +
      'method_id        INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'method_name      VARCHAR (100) UNIQUE NOT NULL,' +
      'abbreviation     VARCHAR (20),' +
      'ebird_name       VARCHAR (60),' +
      'category         VARCHAR (30),' +
      'description      TEXT,' +
      'recommended_uses TEXT,' +
      'notes            TEXT,' +
      'can_delete       BOOLEAN       DEFAULT (1),' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN       DEFAULT (0),' +
      'marked_status    BOOLEAN       DEFAULT (0),' +
      'active_status    BOOLEAN       DEFAULT (1),' +
      'inactivated_by   VARCHAR (5)' +
    ');';
end;

function TMethodsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM methods ' +
    'WHERE method_id = :aid ';
end;

function TMethodsSQL.DistinctCategories: String;
begin
  Result :=
    'SELECT DISTINCT category FROM methods ' +
    'WHERE (category != '''') AND (category NOT NULL) ' +
    'ORDER BY category ASC ';
end;

function TMethodsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT method_id, method_name, abbreviation FROM methods ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((method_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
            'OR (abbreviation ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM)) ' +
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

function TMethodsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO methods (' +
      'method_name, ' +
      'abbreviation, ' +
      'category, ' +
      'ebird_name, ' +
      'description, ' +
      'recommended_uses, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':method_name, ' +
      ':abbreviation, ' +
      ':category, ' +
      ':ebird_name, ' +
      ':description, ' +
      ':recommended_uses, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TMethodsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM methods ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (method_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (method_id = :method_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (method_id = -1) AND (active_status = 1) ';
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

function TMethodsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'method_id, ' +
      'method_name, ' +
      'abbreviation, ' +
      'ebird_name, ' +
      'category, ' +
      'description, ' +
      'recommended_uses, ' +
      'notes, ' +
      'can_delete, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM methods ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (method_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (method_id = :method_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (method_id = -1) AND (active_status = 1) ';
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

function TMethodsSQL.Update: String;
begin
  Result :=
    'UPDATE methods SET ' +
      'method_name = :method_name, ' +
      'abbreviation = :abbreviation, ' +
      'category = :category, ' +
      'ebird_name = :ebird_name, ' +
      'description = :description, ' +
      'recommended_uses = :recommended_uses, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (method_id = :method_id) ';
end;

end.

