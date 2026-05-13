unit providers_people;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TPeopleSQL }

  TPeopleSQL = class(TInterfacedObject, IPeopleSQL)
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

{ TPeopleSQL }

constructor TPeopleSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TPeopleSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS people (' +
      'person_id              INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'full_name              VARCHAR (100) NOT NULL,' +
      'abbreviation           VARCHAR (10)  UNIQUE NOT NULL,' +
      'citation               VARCHAR (100),' +
      'title_treatment        VARCHAR (10),' +
      'id_document_1          VARCHAR (15),' +
      'id_document_2          VARCHAR (15),' +
      'gender                 VARCHAR (5),' +
      'birth_date             DATE,' +
      'death_date             DATE,' +
      'email_addr             VARCHAR (60),' +
      'phone_1                VARCHAR (20),' +
      'phone_2                VARCHAR (20),' +
      'address_1              VARCHAR (100),' +
      'address_2              VARCHAR (60),' +
      'neighborhood           VARCHAR (60),' +
      'postal_code            VARCHAR (15),' +
      'country_id             INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'state_id               INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'municipality_id        INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'institution_id         INTEGER       REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
      'department             VARCHAR (100),' +
      'job_role               VARCHAR (100),' +
      'lattes_uri             VARCHAR (30),' +
      'orcid_uri              VARCHAR (30),' +
      'twitter_uri            VARCHAR (50),' +
      'instagram_uri          VARCHAR (50),' +
      'website_uri            VARCHAR (100),' +
      'profile_color          VARCHAR (30),' +
      'notes                  TEXT,' +
      'profile_image          BLOB,' +
      'user_inserted          INTEGER,' +
      'user_updated           INTEGER,' +
      'insert_date            DATETIME,' +
      'update_date            DATETIME,' +
      'exported_status        BOOLEAN       DEFAULT (0),' +
      'marked_status          BOOLEAN       DEFAULT (0),' +
      'active_status          BOOLEAN       DEFAULT (1),' +
      'inactivated_by         VARCHAR (5)' +
    ');';
end;

function TPeopleSQL.Delete: String;
begin
  Result :=
    'DELETE FROM people ' +
    'WHERE person_id = :aid';
end;

function TPeopleSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT person_id, full_name, citation, abbreviation FROM people ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
            'OR (citation ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
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

function TPeopleSQL.Insert: String;
begin
  Result :=
    'INSERT INTO people (' +
      'full_name, ' +
      'abbreviation, ' +
      'citation, ' +
      'title_treatment, ' +
      'id_document_1, ' +
      'id_document_2, ' +
      'gender, ' +
      'birth_date, ' +
      'death_date, ' +
      'email_addr, ' +
      'phone_1, ' +
      'phone_2, ' +
      'address_1, ' +
      'address_2, ' +
      'neighborhood, ' +
      'postal_code, ' +
      'country_id, ' +
      'state_id, ' +
      'municipality_id, ' +
      'institution_id, ' +
      'department, ' +
      'job_role, ' +
      'lattes_uri, ' +
      'orcid_uri, ' +
      'twitter_uri, ' +
      'instagram_uri, ' +
      'website_uri, ' +
      'profile_color, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':full_name, ' +
      ':abbreviation, ' +
      ':citation, ' +
      ':title_treatment, ' +
      ':id_document_1, ' +
      ':id_document_2, ' +
      ':gender, ' +
      'date(:birth_date), ' +
      'date(:death_date), ' +
      ':email_addr, ' +
      ':phone_1, ' +
      ':phone_2, ' +
      ':address_1, ' +
      ':address_2, ' +
      ':neighborhood, ' +
      ':postal_code, ' +
      ':country_id, ' +
      ':state_id, ' +
      ':municipality_id, ' +
      ':institution_id, ' +
      ':department, ' +
      ':job_role, ' +
      ':lattes_uri, ' +
      ':orcid_uri, ' +
      ':twitter_uri, ' +
      ':instagram_uri, ' +
      ':website_uri, ' +
      ':profile_color, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TPeopleSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT p.*, ' +
      'gm.site_name AS municipality_name, ' +
      'gs.site_name AS state_name, ' +
      'gc.site_name AS country_name, ' +
      'it.full_name AS institution_name ' +
    'FROM people AS p ' +
    'LEFT JOIN gazetteer AS gm ON p.municipality_id = gm.site_id ' +
    'LEFT JOIN gazetteer AS gs ON p.state_id = gs.site_id ' +
    'LEFT JOIN gazetteer AS gc ON p.country_id = gc.site_id ' +
    'LEFT JOIN institutions AS it ON p.institution_id = it.institution_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (p.person_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (p.person_id = :person_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (p.person_id = -1) AND (p.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (p.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (p.active_status = 1) AND (p.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (p.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TPeopleSQL.SelectDateTree(Grouped: Boolean): String;
begin
  Result :=
    'SELECT ' +
      'strftime(''%Y'', p.birth_date) AS ano, ' +
      'strftime(''%m'', p.birth_date) AS mes, ' +
      'strftime(''%d'', p.birth_date) AS dia ' +
    'FROM people AS p ' +
    'WHERE (p.active_status = 1) ';

  if Grouped then
    Result := Result +
      'GROUP BY ano, mes, dia ' +
      'ORDER BY ano DESC, mes ASC, dia ASC';
end;

function TPeopleSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'p.person_id, ' +
      'p.full_name, ' +
      'p.abbreviation, ' +
      'p.citation, ' +
      'p.title_treatment, ' +
      'p.id_document_1, ' +
      'p.id_document_2, ' +
      'p.gender, ' +
      'p.birth_date, ' +
      'p.death_date, ' +
      'p.email_addr, ' +
      'p.phone_1, ' +
      'p.phone_2, ' +
      'p.address_1, ' +
      'p.address_2, ' +
      'p.neighborhood, ' +
      'p.postal_code, ' +
      'p.country_id, ' +
      'p.state_id, ' +
      'p.municipality_id, ' +
      'p.institution_id, ' +
      'i.full_name AS institution_name' +
      'p.department, ' +
      'p.job_role, ' +
      'p.lattes_uri, ' +
      'p.orcid_uri, ' +
      'p.twitter_uri, ' +
      'p.instagram_uri, ' +
      'p.website_uri, ' +
      'p.profile_color, ' +
      'p.notes, ' +
      'p.profile_image, ' +
      'p.user_inserted, ' +
      'p.user_updated, ' +
      'datetime(p.insert_date, ''localtime'') AS insert_date, ' +
      'datetime(p.update_date, ''localtime'') AS update_date, ' +
      'p.exported_status, ' +
      'p.marked_status, ' +
      'p.active_status ' +
    'FROM people AS p ' +
    'LEFT JOIN institutions AS i ON p.institution_id = i.institution_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (p.person_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (p.person_id = :person_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (p.person_id = -1) AND (p.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (p.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (p.active_status = 1) AND (p.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (p.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TPeopleSQL.Update: String;
begin
  Result :=
    'UPDATE people SET ' +
      'full_name = :full_name, ' +
      'abbreviation = :abbreviation, ' +
      'citation = :citation, ' +
      'title_treatment = :title_treatment, ' +
      'id_document_1 = :id_document_1, ' +
      'id_document_2 = :id_document_2, ' +
      'gender = :gender, ' +
      'birth_date = date(:birth_date), ' +
      'death_date = date(:death_date), ' +
      'email_addr = :email_addr, ' +
      'phone_1 = :phone_1, ' +
      'phone_2 = :phone_2, ' +
      'address_1 = :address_1, ' +
      'address_2 = :address_2, ' +
      'neighborhood = :neighborhood, ' +
      'postal_code = :postal_code, ' +
      'country_id = :country_id, ' +
      'state_id = :state_id, ' +
      'municipality_id = :municipality_id, ' +
      'institution_id = :institution_id, ' +
      'department = :department, ' +
      'job_role = :job_role, ' +
      'lattes_uri = :lattes_uri, ' +
      'orcid_uri = :orcid_uri, ' +
      'twitter_uri = :twitter_uri, ' +
      'instagram_uri = :instagram_uri, ' +
      'website_uri = :website_uri, ' +
      'profile_color = :profile_color, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ' +
    'WHERE (person_id = :person_id) ';
end;

end.

