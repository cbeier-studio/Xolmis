unit providers_media;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, data_providers, data_types;

type

  { TAudiosSQL }

  TAudiosSQL = class(TInterfacedObject, IAudiosSQL)
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

  { TDocumentsSQL }

  TDocumentsSQL = class(TInterfacedObject, IDocumentsSQL)
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

  { TImagesSQL }

  TImagesSQL = class(TInterfacedObject, IImagesSQL)
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

  { TVideosSQL }

  TVideosSQL = class(TInterfacedObject, IVideosSQL)
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

{ TAudiosSQL }

constructor TAudiosSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TAudiosSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS audio_library (' +
      'audio_id          INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'full_name         VARCHAR (100),' +
      'taxon_id          INTEGER,' +
      'individual_id     INTEGER,' +
      'specimen_id       INTEGER,' +
      'sighting_id       INTEGER,' +
      'audio_type        VARCHAR (15),' +
      'locality_id       INTEGER,' +
      'recording_date    DATE,' +
      'recorder_id       INTEGER,' +
      'recording_time    TIME,' +
      'longitude         REAL,' +
      'latitude          REAL,' +
      'coordinate_precision CHAR (1),' +
      'temperature       REAL,' +
      'cloud_cover       INTEGER,' +
      'precipitation     CHAR (1),' +
      'relative_humidity INTEGER,' +
      'wind_speed        INTEGER,' +
      'recording_context VARCHAR (60),' +
      'playback_used     BOOLEAN       DEFAULT (0),' +
      'subjects_tally    INTEGER,' +
      'habitat           VARCHAR (60),' +
      'recorder_model    VARCHAR (60),' +
      'mic_model         VARCHAR (60),' +
      'filter_model      VARCHAR (60),' +
      'distance          REAL,' +
      'license_type      VARCHAR (20),' +
      'license_year      INTEGER,' +
      'license_uri       VARCHAR (200),' +
      'license_notes     VARCHAR (100),' +
      'license_owner     VARCHAR (150),' +
      'audio_file        VARCHAR (250),' +
      'subtitle          TEXT,' +
      'notes             TEXT,' +
      'user_inserted     INTEGER,' +
      'user_updated      INTEGER,' +
      'insert_date       DATETIME,' +
      'update_date       DATETIME,' +
      'exported_status   BOOLEAN       DEFAULT (0),' +
      'marked_status     BOOLEAN       DEFAULT (0),' +
      'active_status     BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TAudiosSQL.Delete: String;
begin
  Result :=
    'DELETE FROM audio_library ' +
    'WHERE audio_id = :aid';
end;

function TAudiosSQL.Insert: String;
begin
  Result :=
    'INSERT INTO audio_library (' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'specimen_id, ' +
      'sighting_id, ' +
      'audio_type, ' +
      'locality_id, ' +
      'recording_date, ' +
      'recorder_id, ' +
      'recording_time, ' +
      'coordinate_precision, ' +
      'longitude, ' +
      'latitude, ' +
      'temperature, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'relative_humidity, ' +
      'wind_speed, ' +
      'recording_context, ' +
      'playback_used, ' +
      'subjects_tally, ' +
      'habitat, ' +
      'recorder_model, ' +
      'mic_model, ' +
      'filter_model, ' +
      'distance, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'audio_file, ' +
      'subtitle, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':full_name, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':specimen_id, ' +
      ':sighting_id, ' +
      ':audio_type, ' +
      ':locality_id, ' +
      'date(:recording_date), ' +
      ':recorder_id, ' +
      'time(:recording_time), ' +
      ':coordinate_precision, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':temperature, ' +
      ':cloud_cover, ' +
      ':precipitation, ' +
      ':relative_humidity, ' +
      ':wind_speed, ' +
      ':recording_context, ' +
      ':playback_used, ' +
      ':subjects_tally, ' +
      ':habitat, ' +
      ':recorder_model, ' +
      ':mic_model, ' +
      ':filter_model, ' +
      ':distance, ' +
      ':license_type, ' +
      ':license_year, ' +
      ':license_uri, ' +
      ':license_notes, ' +
      ':license_owner, ' +
      ':audio_file, ' +
      ':subtitle, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TAudiosSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT * FROM audio_library ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (audio_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (audio_id = :audio_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (audio_id = -1) AND (active_status = 1) ';
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

function TAudiosSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'audio_id, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'specimen_id, ' +
      //'survey_id' +
      'sighting_id, ' +
      'audio_type, ' +
      'locality_id, ' +
      'recording_date, ' +
      'recorder_id, ' +
      'recording_time, ' +
      'coordinate_precision, ' +
      'longitude, ' +
      'latitude, ' +
      'temperature, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'relative_humidity, ' +
      'wind_speed, ' +
      'recording_context, ' +
      'playback_used, ' +
      'subjects_tally, ' +
      'habitat, ' +
      'recorder_model, ' +
      'mic_model, ' +
      'filter_model, ' +
      'distance, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'audio_file, ' +
      'subtitle, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM audio_library ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (audio_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (audio_id = :audio_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (audio_id = -1) AND (active_status = 1) ';
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

function TAudiosSQL.Update: String;
begin
  Result :=
    'UPDATE audio_library SET ' +
      'full_name = :full_name, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'specimen_id = :specimen_id, ' +
      'sighting_id = :sighting_id, ' +
      'audio_type = :audio_type, ' +
      'locality_id = :locality_id, ' +
      'recording_date = date(:recording_date), ' +
      'recorder_id = :recorder_id, ' +
      'recording_time = time(:recording_time), ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'temperature = :temperature, ' +
      'cloud_cover = :cloud_cover, ' +
      'precipitation = :precipitation, ' +
      'relative_humidity = :relative_humidity, ' +
      'wind_speed = :wind_speed, ' +
      'recording_context = :recording_context, ' +
      'playback_used = :playback_used, ' +
      'subjects_tally = :subjects_tally, ' +
      'habitat = :habitat, ' +
      'recorder_model = :recorder_model, ' +
      'mic_model = :mic_model, ' +
      'filter_model = :filter_model, ' +
      'distance = :distance, ' +
      'license_type = :license_type, ' +
      'license_year = :license_year, ' +
      'license_uri = :license_uri, ' +
      'license_notes = :license_notes, ' +
      'license_owner = :license_owner, ' +
      'audio_file = :audio_file, ' +
      'subtitle = :subtitle, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ' +
    'WHERE (audio_id = :audio_id) ';
end;

{ TDocumentsSQL }

constructor TDocumentsSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TDocumentsSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS documents (' +
      'document_id     INTEGER       PRIMARY KEY AUTOINCREMENT NOT NULL UNIQUE,' +
      'permit_id       INTEGER       REFERENCES legal (permit_id) ON DELETE CASCADE,' +
      'project_id      INTEGER       REFERENCES projects (project_id) ON DELETE CASCADE,' +
      'person_id       INTEGER       REFERENCES people (person_id) ON DELETE CASCADE,' +
      'individual_id   INTEGER       REFERENCES individuals (individual_id) ON DELETE CASCADE,' +
      'capture_id      INTEGER       REFERENCES captures (capture_id) ON DELETE CASCADE,' +
      'sighting_id     INTEGER       REFERENCES sightings (sighting_id) ON DELETE CASCADE,' +
      'specimen_id     INTEGER       REFERENCES specimens (specimen_id) ON DELETE CASCADE,' +
      'expedition_id   INTEGER       REFERENCES expeditions (expedition_id) ON DELETE CASCADE,' +
      'survey_id       INTEGER       REFERENCES surveys (survey_id) ON DELETE CASCADE,' +
      'nest_id         INTEGER       REFERENCES nests (nest_id) ON DELETE CASCADE,' +
      'net_station_id  INTEGER       REFERENCES sampling_plots (sampling_plot_id) ON DELETE CASCADE,' +
      'method_id       INTEGER       REFERENCES methods (method_id) ON DELETE CASCADE,' +
      'document_type   CHAR (5)      NOT NULL,' +
      'document_name   VARCHAR (120),' +
      'document_path   VARCHAR (250) NOT NULL,' +
      'document_date   DATE,' +
      'document_time   TIME,' +
      'license_type    VARCHAR (20),' +
      'license_year    INTEGER,' +
      'license_uri     VARCHAR (200),' +
      'license_notes   VARCHAR (100),' +
      'license_owner   VARCHAR (150),' +
      'user_inserted   INTEGER,' +
      'user_updated    INTEGER,' +
      'insert_date     DATETIME,' +
      'update_date     DATETIME,' +
      'exported_status BOOLEAN       DEFAULT (0),' +
      'marked_status   BOOLEAN       DEFAULT (0),' +
      'active_status   BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TDocumentsSQL.Delete: String;
begin
  Result :=
    'DELETE FROM documents ' +
    'WHERE document_id = :aid';
end;

function TDocumentsSQL.Insert: String;
begin
  Result :=
    'INSERT INTO documents (' +
      'permit_id, ' +
      'project_id, ' +
      'person_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'specimen_id, ' +
      'expedition_id, ' +
      'survey_id, ' +
      'nest_id, ' +
      'net_station_id, ' +
      'method_id, ' +
      'document_type, ' +
      'document_name, ' +
      'document_path, ' +
      'document_date, ' +
      'document_time, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':permit_id, ' +
      ':project_id, ' +
      ':person_id, ' +
      ':individual_id, ' +
      ':capture_id, ' +
      ':sighting_id, ' +
      ':specimen_id, ' +
      ':expedition_id, ' +
      ':survey_id, ' +
      ':nest_id, ' +
      ':net_station_id, ' +
      ':method_id, ' +
      ':document_type, ' +
      ':document_name, ' +
      ':document_path, ' +
      'date(:document_date), ' +
      'time(:document_time), ' +
      ':license_type, ' +
      ':license_year, ' +
      ':license_uri, ' +
      ':license_notes, ' +
      ':license_owner, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TDocumentsSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT * FROM documents ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (document_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (document_id = :document_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (document_id = -1) AND (active_status = 1) ';
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

function TDocumentsSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'document_id, ' +
      'permit_id, ' +
      'project_id, ' +
      'person_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'specimen_id, ' +
      'expedition_id, ' +
      'survey_id, ' +
      'nest_id, ' +
      'net_station_id, ' +
      'method_id, ' +
      'document_type, ' +
      'document_name, ' +
      'document_path, ' +
      'document_date, ' +
      'document_time, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM documents ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (document_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (document_id = :document_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (document_id = -1) AND (active_status = 1) ';
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

function TDocumentsSQL.Update: String;
begin
  Result :=
    'UPDATE documents SET ' +
      'permit_id = :permit_id, ' +
      'project_id = :project_id, ' +
      'person_id = :person_id, ' +
      'individual_id = :individual_id, ' +
      'capture_id = :capture_id, ' +
      'sighting_id = :sighting_id, ' +
      'specimen_id = :specimen_id, ' +
      'expedition_id = :expedition_id, ' +
      'survey_id = :survey_id, ' +
      'nest_id = :nest_id, ' +
      'net_station_id = :net_station_id, ' +
      'method_id = :method_id, ' +
      'document_type = :document_type, ' +
      'document_name = :document_name, ' +
      'document_path = :document_path, ' +
      'document_date = date(:document_date), ' +
      'document_time = time(:document_time), ' +
      'license_type = :license_type, ' +
      'license_year = :license_year, ' +
      'license_uri = :license_uri, ' +
      'license_notes = :license_notes, ' +
      'license_owner = :license_owner, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ' +
    'WHERE (document_id = :document_id) ';
end;

{ TImagesSQL }

constructor TImagesSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TImagesSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS images (' +
      'image_id             INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'image_date           DATE,' +
      'image_time           TIME,' +
      'image_type           CHAR (5),' +
      'taxon_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'individual_id        INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'capture_id           INTEGER       REFERENCES captures (capture_id) ON UPDATE CASCADE,' +
      'feather_id           INTEGER       REFERENCES feathers (feather_id) ON UPDATE CASCADE,' +
      'locality_id          INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'author_id            INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'survey_id            INTEGER,' +
      'sighting_id          INTEGER       REFERENCES sightings (sighting_id) ON UPDATE CASCADE,' +
      'nest_id              INTEGER       REFERENCES nests (nest_id) ON UPDATE CASCADE,' +
      'nest_revision_id     INTEGER       REFERENCES nest_revisions (nest_revision_id) ON UPDATE CASCADE,' +
      'egg_id               INTEGER       REFERENCES eggs (egg_id) ON UPDATE CASCADE,' +
      'specimen_id          INTEGER       REFERENCES specimens (specimen_id) ON UPDATE CASCADE,' +
      'image_filename       VARCHAR (300),' +
      'coordinate_precision CHAR (1),' +
      'longitude            REAL,' +
      'latitude             REAL,' +
      'license_type         VARCHAR (20),' +
      'license_year         INTEGER,' +
      'license_uri          VARCHAR (200),' +
      'license_notes        VARCHAR (100),' +
      'license_owner        VARCHAR (150),' +
      'subtitle             TEXT,' +
      'image_thumbnail      BLOB,' +
      'user_inserted        INTEGER,' +
      'user_updated         INTEGER,' +
      'insert_date          DATETIME,' +
      'update_date          DATETIME,' +
      'exported_status      BOOLEAN       DEFAULT (0),' +
      'marked_status        BOOLEAN       DEFAULT (0),' +
      'active_status        BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TImagesSQL.Delete: String;
begin
  Result :=
    'DELETE FROM images ' +
    'WHERE image_id = :aid';
end;

function TImagesSQL.Insert: String;
begin
  Result :=
    'INSERT INTO images (' +
      'image_date, ' +
      'image_time, ' +
      'image_type, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'feather_id, ' +
      'locality_id, ' +
      'author_id, ' +
      'survey_id, ' +
      'sighting_id, ' +
      'nest_id, ' +
      'nest_revision_id, ' +
      'egg_id, ' +
      'specimen_id, ' +
      'image_filename, ' +
      'coordinate_precision, ' +
      'longitude, ' +
      'latitude, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'subtitle, ' +
      'image_thumbnail, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      'date(:image_date), ' +
      'time(:image_time), ' +
      ':image_type, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':capture_id, ' +
      ':feather_id, ' +
      ':locality_id, ' +
      ':author_id, ' +
      ':survey_id, ' +
      ':sighting_id, ' +
      ':nest_id, ' +
      ':nest_revision_id, ' +
      ':egg_id, ' +
      ':specimen_id, ' +
      ':image_filename, ' +
      ':coordinate_precision, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':license_type, ' +
      ':license_year, ' +
      ':license_uri, ' +
      ':license_notes, ' +
      ':license_owner, ' +
      ':subtitle, ' +
      ':image_thumbnail, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TImagesSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM images ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (image_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (image_id = :image_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (image_id = -1) AND (active_status = 1) ';
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

function TImagesSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'image_id, ' +
      'image_date, ' +
      'image_time, ' +
      'image_type, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'feather_id, ' +
      'locality_id, ' +
      'author_id, ' +
      'survey_id, ' +
      'sighting_id, ' +
      'nest_id, ' +
      'nest_revision_id, ' +
      'egg_id, ' +
      'specimen_id, ' +
      'image_filename, ' +
      'coordinate_precision, ' +
      'longitude, ' +
      'latitude, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'subtitle, ' +
      'image_thumbnail, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM images ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (image_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (image_id = :image_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (image_id = -1) AND (active_status = 1) ';
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

function TImagesSQL.Update: String;
begin
  Result :=
    'UPDATE images SET ' +
      'image_date = date(:image_date), ' +
      'image_time = time(:image_time), ' +
      'image_type = :image_type, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'capture_id = :capture_id, ' +
      'feather_id = :feather_id, ' +
      'locality_id = :locality_id, ' +
      'author_id = :author_id, ' +
      'survey_id = :survey_id, ' +
      'sighting_id = :sighting_id, ' +
      'nest_id = :nest_id, ' +
      'nest_revision_id = :nest_revision_id, ' +
      'egg_id = :egg_id, ' +
      'specimen_id = :specimen_id, ' +
      'image_filename = :image_filename, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'license_type = :license_type, ' +
      'license_year = :license_year, ' +
      'license_uri = :license_uri, ' +
      'license_notes = :license_notes, ' +
      'license_owner = :license_owner, ' +
      'subtitle = :subtitle, ' +
      'image_thumbnail = :image_thumbnail, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ' +
    'WHERE (image_id = :image_id) ';
end;

{ TVideosSQL }

constructor TVideosSQL.Create(ABackend: TDatabaseBackend);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TVideosSQL.CreateTable: String;
begin
  Result :=
    'CREATE TABLE IF NOT EXISTS videos (' +
      'video_id          INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
      'full_name         VARCHAR (100),' +
      'taxon_id          INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
      'individual_id     INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
      'capture_id        INTEGER       REFERENCES captures (capture_id) ON UPDATE CASCADE,' +
      'survey_id         INTEGER       REFERENCES surveys (survey_id) ON UPDATE CASCADE,' +
      'sighting_id       INTEGER       REFERENCES sightings (sighting_id) ON UPDATE CASCADE,' +
      'nest_id           INTEGER       REFERENCES nests (nest_id) ON UPDATE CASCADE,' +
      'nest_revision_id  INTEGER       REFERENCES nest_revisions (nest_revision_id) ON UPDATE CASCADE,' +
      'video_type        VARCHAR (15),' +
      'locality_id       INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
      'recording_date    DATE,' +
      'recording_time    TIME,' +
      'recorder_id       INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
      'longitude         REAL,' +
      'latitude          REAL,' +
      'coordinate_precision VARCHAR (3),' +
      'recording_context VARCHAR (60),' +
      'habitat           VARCHAR (60),' +
      'camera_model      VARCHAR (60),' +
      'distance          REAL,' +
      'license_type      VARCHAR (20),' +
      'license_year      INTEGER,' +
      'license_uri       VARCHAR (200),' +
      'license_notes     VARCHAR (100),' +
      'license_owner     VARCHAR (150),' +
      'file_path         VARCHAR (250),' +
      'subtitle          TEXT,' +
      'notes             TEXT,' +
      'user_inserted     INTEGER,' +
      'user_updated      INTEGER,' +
      'insert_date       DATETIME,' +
      'update_date       DATETIME,' +
      'exported_status   BOOLEAN       DEFAULT (0),' +
      'marked_status     BOOLEAN       DEFAULT (0),' +
      'active_status     BOOLEAN       DEFAULT (1)' +
    ');';
end;

function TVideosSQL.Delete: String;
begin
  Result :=
    'DELETE FROM videos ' +
    'WHERE video_id = :aid';
end;

function TVideosSQL.Insert: String;
begin
  Result :=
    'INSERT INTO videos (' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'nest_id, ' +
      'nest_revision_id, ' +
      'survey_id, ' +
      'sighting_id, ' +
      'video_type, ' +
      'locality_id, ' +
      'recording_date, ' +
      'recorder_id, ' +
      'recording_time, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'recording_context, ' +
      'habitat, ' +
      'camera_model, ' +
      'distance, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'file_path, ' +
      'subtitle, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ' +
    'VALUES (' +
      ':full_name, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':capture_id, ' +
      ':nest_id, ' +
      ':nest_revision_id, ' +
      ':survey_id, ' +
      ':sighting_id, ' +
      ':video_type, ' +
      ':locality_id, ' +
      'date(:recording_date), ' +
      ':recorder_id, ' +
      'time(:recording_time), ' +
      ':longitude, ' +
      ':latitude, ' +
      ':coordinate_precision, ' +
      ':recording_context, ' +
      ':habitat, ' +
      ':camera_model, ' +
      ':distance, ' +
      ':license_type, ' +
      ':license_year, ' +
      ':license_uri, ' +
      ':license_notes, ' +
      ':license_owner, ' +
      ':file_path, ' +
      ':subtitle, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))';
end;

function TVideosSQL.SelectAll(aWhere: TSQLWhereClause): String;
begin
  Result := 'SELECT * FROM videos ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (video_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (video_id = :video_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (video_id = -1) AND (active_status = 1) ';
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

function TVideosSQL.SelectTable(aWhere: TSQLWhereClause): String;
begin
  Result :=
    'SELECT ' +
      'video_id, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'nest_id, ' +
      'nest_revision_id, ' +
      'survey_id, ' +
      'sighting_id, ' +
      'video_type, ' +
      'locality_id, ' +
      'recording_date, ' +
      'recorder_id, ' +
      'recording_time, ' +
      'longitude, ' +
      'latitude, ' +
      'coordinate_precision, ' +
      'recording_context, ' +
      'habitat, ' +
      'camera_model, ' +
      'distance, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'file_path, ' +
      'subtitle, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
    'FROM videos ';

  case aWhere of
    swcNone: ;
    swcId:
      Result := Result + 'WHERE (video_id = :cod) ';
    swcUpdateId:
      Result := Result + 'WHERE (video_id = :video_id) ';
    swcFieldValue:
      Result := Result + 'WHERE (%afield = :avalue) ';
    swcActiveEmpty:
      Result := Result + 'WHERE (video_id = -1) AND (active_status = 1) ';
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

function TVideosSQL.Update: String;
begin
  Result :=
    'UPDATE videos SET ' +
      'full_name = :full_name, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'capture_id = :capture_id, ' +
      'nest_id = :nest_id, ' +
      'nest_revision_id = :nest_revision_id, ' +
      'survey_id = :survey_id, ' +
      'sighting_id = :sighting_id, ' +
      'video_type = :video_type, ' +
      'locality_id = :locality_id, ' +
      'recording_date = date(:recording_date), ' +
      'recorder_id = :recorder_id, ' +
      'recording_time = time(:recording_time), ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'coordinate_precision = :coordinate_precision, ' +
      'recording_context = :recording_context, ' +
      'habitat = :habitat, ' +
      'camera_model = :camera_model, ' +
      'distance = :distance, ' +
      'license_type = :license_type, ' +
      'license_year = :license_year, ' +
      'license_uri = :license_uri, ' +
      'license_notes = :license_notes, ' +
      'license_owner = :license_owner, ' +
      'file_path = :file_path, ' +
      'subtitle = :subtitle, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ' +
    'WHERE (video_id = :video_id) ';
end;

end.

