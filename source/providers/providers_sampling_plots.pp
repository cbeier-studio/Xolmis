unit providers_sampling_plots;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TSamplingPlotsSQL }

  TSamplingPlotsSQL = class(TInterfacedObject, ISamplingPlotsSQL)
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

  { TPermanentNetsSQL }

  TPermanentNetsSQL = class(TInterfacedObject, IPermanentNetsSQL)
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

{ TSamplingPlotsSQL }

constructor TSamplingPlotsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TSamplingPlotsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS sampling_plots (' +
      'sampling_plot_id INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'full_name        VARCHAR (100) NOT NULL UNIQUE,' +
      'acronym          VARCHAR (10)  NOT NULL UNIQUE,' +
      'longitude        REAL,' +
      'latitude         REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'area_shape       VARCHAR (5),' +
      'locality_id      INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'description      TEXT,' +
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

function TSamplingPlotsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM sampling_plots ' +
    'WHERE sampling_plot_id = :aid';
end;

function TSamplingPlotsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT sampling_plot_id, full_name, acronym FROM sampling_plots ';

  case aWhere of
    swcNone: ;
    swcFindText:
    begin
      Result := Result +
        'WHERE ((full_name ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM) ' +
            'OR (acronym ' + CRITERIA_OPERATORS[aCriteria] + ' :VALPARAM)) ' +
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

function TSamplingPlotsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO sampling_plots (' +
      'full_name, ' +
      'acronym, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'area_shape, ' +
      'locality_id, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':full_name, ' +
      ':acronym, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':area_shape, ' +
      ':locality_id, ' +
      ':description, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TSamplingPlotsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT pl.*, ' +
      'gl.site_name AS locality_name, ' +
      'gl.country_id AS country_id, ' +
      'gl.state_id AS state_id, ' +
      'gl.municipality_id AS municipality_id, ' +
      'gm.site_name AS municipality_name, ' +
      'gs.site_name AS state_name, ' +
      'gc.site_name AS country_name ' +
    'FROM sampling_plots AS pl ' +
    'LEFT JOIN gazetteer AS gl ON pl.locality_id = gl.site_id ' +
    'LEFT JOIN gazetteer AS gm ON gl.municipality_id = gm.site_id ' +
    'LEFT JOIN gazetteer AS gs ON gl.state_id = gs.site_id ' +
    'LEFT JOIN gazetteer AS gc ON gl.country_id = gc.site_id ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (pl.sampling_plot_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (pl.sampling_plot_id = :sampling_plot_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (pl.sampling_plot_id = -1) AND (pl.active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (pl.active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (pl.active_status = 1) AND (pl.marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (pl.active_status = 0) ';
    swcActiveParent: ;
    swcFindText: ;
  end;
end;

function TSamplingPlotsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'sampling_plot_id, ' +
      'full_name, ' +
      'acronym, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'area_shape, ' +
      'locality_id, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM sampling_plots ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (sampling_plot_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (sampling_plot_id = :sampling_plot_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (sampling_plot_id = -1) AND (active_status = 1) ';
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

function TSamplingPlotsSQL.Update: String;
begin
  Result :=
    'UPDATE sampling_plots SET ' +
      'full_name = :full_name, ' +
      'acronym = :acronym, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'area_shape = :area_shape, ' +
      'locality_id = :locality_id, ' +
      'description = :description, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (sampling_plot_id = :sampling_plot_id) ';
end;

{ TPermanentNetsSQL }

constructor TPermanentNetsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TPermanentNetsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS permanent_nets (' +
      'permanent_net_id INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'net_station_id   INTEGER       NOT NULL REFERENCES sampling_plots (sampling_plot_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
      'net_number       INTEGER       NOT NULL,' +
      'longitude        REAL,' +
      'latitude         REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'notes            VARCHAR (150),' +
      'full_name        VARCHAR (50),' +
      'user_inserted    INTEGER,' +
      'user_updated     INTEGER,' +
      'insert_date      DATETIME,' +
      'update_date      DATETIME,' +
      'exported_status  BOOLEAN       DEFAULT (0),' +
      'marked_status    BOOLEAN       DEFAULT (0),' +
      'active_status    BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TPermanentNetsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM permanent_nets ' +
    'WHERE permanent_net_id = :aid';
end;

function TPermanentNetsSQL.Find(aWhere: TSQLWhereClause; aCriteria: TCriteriaType): String;
begin
  Result := 'SELECT permanent_net_id, full_name FROM permanent_nets ';

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

function TPermanentNetsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO permanent_nets (' +
      'sampling_plot_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':sampling_plot_id, ' +
      ':net_number, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':notes, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))';
end;

function TPermanentNetsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM permanent_nets ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (permanent_net_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (permanent_net_id = :permanent_net_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (permanent_net_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (sampling_plot_id = :sampling_plot_id) ';
    swcFindText: ;
  end;
end;

function TPermanentNetsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'permanent_net_id, ' +
      'sampling_plot_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM permanent_nets ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (permanent_net_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (permanent_net_id = :permanent_net_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (permanent_net_id = -1) AND (active_status = 1) ';
    swcActiveAll:
      Result := Result + 'WHERE (active_status = 1) ';
    swcActiveMarked:
      Result := Result + 'WHERE (active_status = 1) AND (marked_status = 1) ';
    swcInactive:
      Result := Result + 'WHERE (active_status = 0) ';
    swcActiveParent:
      Result := Result + 'WHERE (active_status = 1) AND (sampling_plot_id = :sampling_plot_id) ';
    swcFindText: ;
  end;
end;

function TPermanentNetsSQL.Update: String;
begin
  Result :=
    'UPDATE permanent_nets SET ' +
      'sampling_plot_id = :sampling_plot_id, ' +
      'net_number = :net_number, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'notes = :notes, ' +
      'full_name = :full_name, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status ' +
    'WHERE (permanent_net_id = :permanent_net_id) ';
end;

end.

