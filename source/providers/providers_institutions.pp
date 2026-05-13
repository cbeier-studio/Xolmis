unit providers_institutions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TInstitutionsSQL }

  TInstitutionsSQL = class(TInterfacedObject, IInstitutionsSQL)
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

{ TInstitutionsSQL }

constructor TInstitutionsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TInstitutionsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS institutions (' +
      'institution_id  INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'full_name       VARCHAR (100) NOT NULL UNIQUE,' +
      'abbreviation         VARCHAR (15),' +
      'address_1       VARCHAR (100),' +
      'address_2       VARCHAR (40),' +
      'neighborhood    VARCHAR (60),' +
      'postal_code        VARCHAR (15),' +
      'municipality_id INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'state_id        INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'country_id      INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'manager_name    VARCHAR (100),' +
      'email_addr      VARCHAR (60),' +
      'phone_num       VARCHAR (20),' +
      'notes           TEXT,' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN       DEFAULT (0),' +
      'marked_status   BOOLEAN       DEFAULT (0),' +
      'active_status   BOOLEAN       DEFAULT (1),' +
      'inactivated_by  VARCHAR (5)' +
    ');';
end;

function TInstitutionsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM institutions ' +
    'WHERE institution_id = :aid';
end;

function TInstitutionsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT institution_id, full_name, abbreviation FROM institutions ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
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

function TInstitutionsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO institutions (' +
      'full_name, ' +
      'abbreviation, ' +
      'address_1, ' +
      'address_2, ' +
      'neighborhood, ' +
      'postal_code, ' +
      'municipality_id, ' +
      'state_id, ' +
      'country_id, ' +
      'manager_name, ' +
      'email_addr, ' +
      'phone_num, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':full_name, ' +
      ':abbreviation, ' +
      ':address_1, ' +
      ':address_2, ' +
      ':neighborhood, ' +
      ':postal_code, ' +
      ':municipality_id, ' +
      ':state_id, ' +
      ':country_id, ' +
      ':manager_name, ' +
      ':email_addr, ' +
      ':phone_num, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TInstitutionsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT it.*, ' +
      'gm.site_name AS municipality_name, ' +
      'gs.site_name AS state_name, ' +
      'gc.site_name AS country_name ' +
    'FROM institutions AS it ' +
    'LEFT JOIN gazetteer AS gm ON it.municipality_id = gm.site_id ' +
    'LEFT JOIN gazetteer AS gs ON it.state_id = gs.site_id ' +
    'LEFT JOIN gazetteer AS gc ON it.country_id = gc.site_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (it.institution_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (it.institution_id = :institution_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (it.institution_id = -1) AND (it.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (it.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (it.active_status = 1) AND (it.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (it.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TInstitutionsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'institution_id, ' +
      'full_name, ' +
      'abbreviation, ' +
      'address_1, ' +
      'address_2, ' +
      'neighborhood, ' +
      'postal_code, ' +
      'municipality_id, ' +
      'state_id, ' +
      'country_id, ' +
      'manager_name, ' +
      'email_addr, ' +
      'phone_num, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM institutions ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (institution_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (institution_id = :institution_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (institution_id = -1) AND (active_status = 1) ';
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

function TInstitutionsSQL.Update: String;
begin
  Result :=
    'UPDATE institutions SET ' +
      'full_name = :full_name, ' +
      'abbreviation = :abbreviation, ' +
      'address_1 = :address_1, ' +
      'address_2 = :address_2, ' +
      'neighborhood = :neighborhood, ' +
      'postal_code = :postal_code, ' +
      'municipality_id = :municipality_id, ' +
      'state_id = :state_id, ' +
      'country_id = :country_id, ' +
      'manager_name = :manager_name, ' +
      'email_addr = :email_addr, ' +
      'phone_num = :phone_num, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (institution_id = :institution_id) ';
end;

end.

