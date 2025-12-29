unit data_schema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, DB, SQLDB, Variants, data_types, utils_gis;

type

  { TFieldSchema }

  TFieldSchema = class
  public
    Name: String;
    DisplayName: String;
    DarwinCoreName: String;
    ExportName: String;
    DataType: TSearchDataType;
    SortType: TSortType;
    IsPrimaryKey: Boolean;
    IsForeignKey: Boolean;
    IsVirtual: Boolean;
    LookupInfo: TRelationalField;
    Rules: TValidationRules;
    DefaultValue: Variant;
    CoordinateFormat: TMapCoordinateType;
  public
    procedure ValidateValue(const V: Variant);
    function ConvertValue(const S: String): Variant;
    function LookupTableName: string;
    function LookupValueExists(const V: Variant): Boolean;
    function ResolveLookup(const S: string): Variant;
  end;

  TFieldSchemaList = specialize TFPGObjectList<TFieldSchema>;

  { TTableSchema }

  TTableSchema = class
  public
    TableType: TTableType;
    TableName: String;
    DisplayName: String;
    Fields: TFieldSchemaList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetField(const Name: String): TFieldSchema;
  end;

  TTableSchemaList = specialize TFPGObjectList<TTableSchema>;

  { TDatabaseSchema }

  TDatabaseSchema = class
  public
    Tables: TTableSchemaList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTable(T: TTableType): TTableSchema; overload;
    function GetTable(Name: String): TTableSchema; overload;
  end;

var
  DBSchema: TDatabaseSchema;

  function DBQuerySingleInt(const SQL, ParamName: string; const ParamValue: Variant; out ResultValue: Integer): Boolean;

  function AddField(ATableSchema: TTableSchema; const AName: string; AType: TSearchDataType;
    ARequired: Boolean = False; AMaxLen: Integer = 0;
    AIsPK: Boolean = False; AIsFK: Boolean = False;
    ALookup: TTableType = tbNone; ALookupField: String = ''): TFieldSchema;

  procedure RegisterBandsSchema(DB: TDatabaseSchema);
  procedure RegisterBotanicTaxaSchema(DB: TDatabaseSchema);
  procedure RegisterCapturesSchema(DB: TDatabaseSchema);
  procedure RegisterEggsSchema(DB: TDatabaseSchema);
  procedure RegisterExpeditionsSchema(DB: TDatabaseSchema);
  procedure RegisterFeathersSchema(DB: TDatabaseSchema);
  procedure RegisterGazetteerSchema(DB: TDatabaseSchema);
  procedure RegisterIndividualsSchema(DB: TDatabaseSchema);
  procedure RegisterInstitutionsSchema(DB: TDatabaseSchema);
  procedure RegisterMethodsSchema(DB: TDatabaseSchema);
  procedure RegisterNestOwnersSchema(DB: TDatabaseSchema);
  procedure RegisterNestRevisionsSchema(DB: TDatabaseSchema);
  procedure RegisterNestsSchema(DB: TDatabaseSchema);
  procedure RegisterNetsEffortSchema(DB: TDatabaseSchema);
  procedure RegisterPeopleSchema(DB: TDatabaseSchema);
  procedure RegisterPermanentNetsSchema(DB: TDatabaseSchema);
  procedure RegisterPermitsSchema(DB: TDatabaseSchema);
  procedure RegisterPoiLibrarySchema(DB: TDatabaseSchema);
  procedure RegisterProjectBudgetsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectChronogramsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectExpensesSchema(DB: TDatabaseSchema);
  procedure RegisterProjectGoalsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectsSchema(DB: TDatabaseSchema);
  procedure RegisterProjectTeamSchema(DB: TDatabaseSchema);
  procedure RegisterSamplePrepsSchema(DB: TDatabaseSchema);
  procedure RegisterSamplingPlotsSchema(DB: TDatabaseSchema);
  procedure RegisterSightingsSchema(DB: TDatabaseSchema);
  procedure RegisterSpecimenCollectorsSchema(DB: TDatabaseSchema);
  procedure RegisterSpecimensSchema(DB: TDatabaseSchema);
  procedure RegisterSurveysSchema(DB: TDatabaseSchema);
  procedure RegisterSurveyTeamSchema(DB: TDatabaseSchema);
  procedure RegisterVegetationSchema(DB: TDatabaseSchema);
  procedure RegisterWeatherLogsSchema(DB: TDatabaseSchema);

  procedure RegisterDatabaseSchema;

implementation

uses
  utils_locale, data_consts, udm_main;

function AddField(ATableSchema: TTableSchema; const AName: string; AType: TSearchDataType; ARequired: Boolean;
  AMaxLen: Integer; AIsPK: Boolean; AIsFK: Boolean; ALookup: TTableType; ALookupField: String): TFieldSchema;
begin
  Result := TFieldSchema.Create;
  Result.Name := AName;
  Result.DataType := AType;
  Result.Rules.RequiredField := ARequired;
  Result.Rules.MaxLength := AMaxLen;
  Result.IsPrimaryKey := AIsPK;
  Result.IsForeignKey := AIsFK;
  Result.LookupInfo.LookupTable := ALookup;
  Result.LookupInfo.LookupField := ALookupField;
  //Result.DefaultValue := ADefault;
  ATableSchema.Fields.Add(Result);
end;

function DBQuerySingleInt(const SQL, ParamName: string; const ParamValue: Variant; out ResultValue: Integer
  ): Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  ResultValue := 0;

  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := DMM.sqlCon;
    Q.Transaction := DMM.sqlTrans;
    Q.SQL.Text := SQL;

    Q.Params.ParamByName(ParamName).Value := ParamValue;

    Q.Open;

    if not Q.EOF then
    begin
      ResultValue := Q.Fields[0].AsInteger;
      Result := True;
    end;

  finally
    Q.Free;
  end;
end;

procedure RegisterBandsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbBands;
  T.TableName := TBL_BANDS;
  T.DisplayName := LocaleTablesDict[tbBands];

  AddField(T, 'band_id', sdtInteger, True, 0, True);
  AddField(T, 'band_size', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z';
  AddField(T, 'band_number', sdtInteger);
  AddField(T, 'band_status', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'D,U,R,Q,P,T';
  AddField(T, 'band_type', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'A,F,N,W,T,L,R,C,O';
  AddField(T, 'band_prefix', sdtText, False, 10);
  AddField(T, 'band_suffix', sdtText, False, 10);
  AddField(T, 'band_color', sdtText, False, 10);
  AddField(T, 'band_source', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'A,T,L,D,F';
  AddField(T, 'supplier_id', sdtInteger, False, 0, False, True, tbInstitutions);
  AddField(T, 'requester_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'carrier_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'individual_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'band_reported', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'notes', sdtText);
  AddField(T, 'full_name', sdtText, False, 40);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterBotanicTaxaSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbBotanicTaxa;
  T.TableName := TBL_BOTANIC_TAXA;
  T.DisplayName := LocaleTablesDict[tbBotanicTaxa];

  AddField(T, 'taxon_id', sdtInteger, True, 0, True);
  AddField(T, 'taxon_name', sdtText, True, 100);
  AddField(T, 'authorship', sdtText, False, 100);
  AddField(T, 'formatted_name', sdtText, False, 180);
  AddField(T, 'vernacular_name', sdtText, False, 100);
  AddField(T, 'rank_id', sdtInteger, True, 0, False, True, tbTaxonRanks);
  AddField(T, 'parent_taxon_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'valid_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'order_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'family_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'genus_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'species_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterCapturesSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbCaptures;
  T.TableName := TBL_CAPTURES;
  T.DisplayName := LocaleTablesDict[tbCaptures];

  AddField(T, 'capture_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_id', sdtInteger);
  AddField(T, 'individual_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'taxon_id', sdtInteger, True, 0, False, True, tbZooTaxa);
  AddField(T, 'full_name', sdtText, False, 120);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'capture_date', sdtDate, True);
  AddField(T, 'capture_time', sdtTime);
  AddField(T, 'locality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'net_station_id', sdtInteger, False, 0, False, True, tbSamplingPlots);
  AddField(T, 'net_id', sdtInteger, False, 0, False, True, tbNetsEffort);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'bander_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'annotator_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'subject_status', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,I,W,X,D';
  AddField(T, 'capture_type', sdtList, True, 5);
  T.Fields.Last.Rules.ValueList := 'N,R,S,C,U';
  AddField(T, 'subject_sex', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'M,F,U';
  AddField(T, 'how_sexed', sdtText, False, 10);
  AddField(T, 'band_id', sdtInteger, False, 0, False, True, tbBands);
  AddField(T, 'removed_band_id', sdtInteger, False, 0, False, True, tbBands);
  AddField(T, 'right_leg_below', sdtText, False, 10);
  AddField(T, 'left_leg_below', sdtText, False, 10);
  AddField(T, 'right_leg_above', sdtText, False, 10);
  AddField(T, 'left_leg_above', sdtText, False, 10);
  AddField(T, 'weight', sdtFloat);
  AddField(T, 'tarsus_length', sdtFloat);
  AddField(T, 'tarsus_diameter', sdtFloat);
  AddField(T, 'culmen_length', sdtFloat);
  AddField(T, 'exposed_culmen', sdtFloat);
  AddField(T, 'bill_width', sdtFloat);
  AddField(T, 'bill_height', sdtFloat);
  AddField(T, 'nostril_bill_tip', sdtFloat);
  AddField(T, 'skull_length', sdtFloat);
  AddField(T, 'halux_length_total', sdtFloat);
  AddField(T, 'halux_length_finger', sdtFloat);
  AddField(T, 'halux_length_claw', sdtFloat);
  AddField(T, 'right_wing_chord', sdtFloat);
  AddField(T, 'first_secondary_chord', sdtFloat);
  AddField(T, 'tail_length', sdtFloat);
  AddField(T, 'central_retrix_length', sdtFloat);
  AddField(T, 'external_retrix_length', sdtFloat);
  AddField(T, 'total_length', sdtFloat);
  AddField(T, 'feather_mites', sdtText, False, 15);
  AddField(T, 'fat', sdtText, False, 5);
  AddField(T, 'brood_patch', sdtText, False, 5);
  AddField(T, 'cloacal_protuberance', sdtText, False, 5);
  AddField(T, 'body_molt', sdtText, False, 5);
  AddField(T, 'flight_feathers_molt', sdtText, False, 5);
  AddField(T, 'flight_feathers_wear', sdtText, False, 5);
  AddField(T, 'molt_limits', sdtText, False, 20);
  AddField(T, 'cycle_code', sdtText, False, 10);
  AddField(T, 'subject_age', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  AddField(T, 'how_aged', sdtText, False, 10);
  AddField(T, 'skull_ossification', sdtText, False, 5);
  AddField(T, 'kipps_index', sdtFloat);
  AddField(T, 'glucose', sdtFloat);
  AddField(T, 'hemoglobin', sdtFloat);
  AddField(T, 'hematocrit', sdtFloat);
  AddField(T, 'philornis_larvae_tally', sdtInteger);
  AddField(T, 'blood_sample', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'feather_sample', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'claw_sample', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'feces_sample', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'parasite_sample', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_collected', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_recorded', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_photographed', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'field_number', sdtText, False, 10);
  AddField(T, 'photographer_1_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'photographer_2_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'start_photo_number', sdtText, False, 20);
  AddField(T, 'end_photo_number', sdtText, False, 20);
  AddField(T, 'camera_name', sdtText, False, 50);
  AddField(T, 'escaped', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'needs_review', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterEggsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbEggs;
  T.TableName := TBL_EGGS;
  T.DisplayName := LocaleTablesDict[tbEggs];

  AddField(T, 'egg_id', sdtInteger, True, 0, True);
  AddField(T, 'nest_id', sdtInteger, False, 0, False, True, tbNests);
  AddField(T, 'egg_seq', sdtInteger);
  AddField(T, 'field_number', sdtText, False, 20);
  AddField(T, 'taxon_id', sdtInteger, False, 0, False, True, tbZooTaxa);
  AddField(T, 'eggshell_color', sdtText, False, 40);
  AddField(T, 'eggshell_pattern', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,P,B,S,T,W,PS,BS';
  AddField(T, 'eggshell_texture', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,C,S,G,P';
  AddField(T, 'egg_shape', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,S,E,O,P,C,B,Y,L';
  AddField(T, 'egg_width', sdtFloat);
  AddField(T, 'egg_length', sdtFloat);
  AddField(T, 'egg_mass', sdtFloat);
  AddField(T, 'egg_volume', sdtFloat);
  AddField(T, 'egg_stage', sdtText, False, 5);
  AddField(T, 'egg_hatched', sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  AddField(T, 'measure_date', sdtDate);
  AddField(T, 'researcher_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'individual_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'host_egg', sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  AddField(T, 'description', sdtText);
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterExpeditionsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbExpeditions;
  T.TableName := TBL_EXPEDITIONS;
  T.DisplayName := LocaleTablesDict[tbExpeditions];

  AddField(T, 'expedition_id', sdtInteger, True, 0, True);
  AddField(T, 'expedition_name', sdtText, True, 150);
  AddField(T, 'start_date', sdtDate);
  AddField(T, 'end_date', sdtDate);
  // Virtual field → must not be imported
  AddField(T, 'duration', sdtInteger);
  T.Fields.Last.IsVirtual := True;
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'description', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterFeathersSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbFeathers;
  T.TableName := TBL_FEATHERS;
  T.DisplayName := LocaleTablesDict[tbFeathers];

  AddField(T, 'feather_id', sdtInteger, True, 0, True);
  AddField(T, 'sample_date', sdtDate, True);
  AddField(T, 'sample_time', sdtTime);
  AddField(T, 'taxon_id', sdtInteger, True, 0, False, True, tbZooTaxa);
  AddField(T, 'locality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'individual_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'capture_id', sdtInteger, False, 0, False, True, tbCaptures);
  AddField(T, 'sighting_id', sdtInteger, False, 0, False, True, tbSightings);
  AddField(T, 'observer_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'source_type', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,C,S,P';
  AddField(T, 'symmetrical', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,S,A';
  AddField(T, 'feather_trait', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'B,P,S,R,PC,GC,MC,LC,CC,AL';
  AddField(T, 'feather_number', sdtInteger);
  AddField(T, 'body_side', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'NA,R,L';
  AddField(T, 'grown_percent', sdtFloat);
  AddField(T, 'feather_length', sdtFloat);
  AddField(T, 'feather_area', sdtFloat);
  AddField(T, 'feather_mass', sdtFloat);
  AddField(T, 'rachis_width', sdtFloat);
  AddField(T, 'growth_bar_width', sdtFloat);
  AddField(T, 'barb_density', sdtFloat);
  AddField(T, 'feather_age', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  AddField(T, 'full_name', sdtText, False, 200);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterGazetteerSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbGazetteer;
  T.TableName := TBL_GAZETTEER;
  T.DisplayName := LocaleTablesDict[tbGazetteer];

  AddField(T, 'site_id', sdtInteger, True, 0, True);
  AddField(T, 'site_name', sdtText, True, 60);
  AddField(T, 'site_acronym', sdtText, False, 10);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'altitude', sdtFloat);
  AddField(T, 'site_rank', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'P,E,R,M,D,L';
  AddField(T, 'parent_site_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'country_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'state_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'municipality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'full_name', sdtText, False, 180);
  AddField(T, 'ebird_name', sdtText, False, 150);
  AddField(T, 'language', sdtText, False, 10);
  AddField(T, 'description', sdtText);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterIndividualsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbIndividuals;
  T.TableName := TBL_INDIVIDUALS;
  T.DisplayName := LocaleTablesDict[tbIndividuals];

  AddField(T, 'individual_id', sdtInteger, True, 0, True);
  AddField(T, 'formatted_name', sdtText, False, 150);
  AddField(T, 'full_name', sdtText, False, 120);
  AddField(T, 'taxon_id', sdtInteger, True, 0, False, True, tbZooTaxa);
  AddField(T, 'individual_sex', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'M,F,U';
  AddField(T, 'individual_age', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,N,F,J,A,Y,S,T,4,5';
  AddField(T, 'nest_id', sdtInteger, False, 0, False, True, tbNests);
  AddField(T, 'birth_date', sdtText, False, 15);
  AddField(T, 'birth_day', sdtInteger);
  AddField(T, 'birth_month', sdtInteger);
  AddField(T, 'birth_year', sdtInteger);
  AddField(T, 'banding_date', sdtDate);
  AddField(T, 'band_change_date', sdtDate);
  AddField(T, 'band_id', sdtInteger, False, 0, False, True, tbBands);
  AddField(T, 'double_band_id', sdtInteger, False, 0, False, True, tbBands);
  AddField(T, 'removed_band_id', sdtInteger, False, 0, False, True, tbBands);
  AddField(T, 'right_leg_below', sdtText, False, 10);
  AddField(T, 'left_leg_below', sdtText, False, 10);
  AddField(T, 'right_leg_above', sdtText, False, 10);
  AddField(T, 'left_leg_above', sdtText, False, 10);
  AddField(T, 'father_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'mother_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'death_date', sdtText, False, 15);
  AddField(T, 'death_day', sdtInteger);
  AddField(T, 'death_month', sdtInteger);
  AddField(T, 'death_year', sdtInteger);
  AddField(T, 'recognizable_markings', sdtText);
  AddField(T, 'notes', sdtText);
  AddField(T, 'user_inserted', sdtInteger);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterInstitutionsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbInstitutions;
  T.TableName := TBL_INSTITUTIONS;
  T.DisplayName := LocaleTablesDict[tbInstitutions];

  AddField(T, 'institution_id', sdtInteger, True, 0, True);
  AddField(T, 'full_name', sdtText, True, 100);
  AddField(T, 'acronym', sdtText, False, 15);
  AddField(T, 'address_1', sdtText, False, 100);
  AddField(T, 'address_2', sdtText, False, 40);
  AddField(T, 'neighborhood', sdtText, False, 60);
  AddField(T, 'zip_code', sdtText, False, 15);
  AddField(T, 'municipality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'state_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'country_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'manager_name', sdtText, False, 100);
  AddField(T, 'email_addr', sdtText, False, 60);
  AddField(T, 'phone_num', sdtText, False, 20);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;


procedure RegisterMethodsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
  F: TFieldSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbMethods;
  T.TableName := TBL_METHODS;
  T.DisplayName := LocaleTablesDict[tbMethods];

  AddField(T, 'method_id', sdtInteger, True, 0, True);
  AddField(T, 'method_name', sdtText, True, 100);
  AddField(T, 'abbreviation', sdtText, False, 20);
  AddField(T, 'ebird_name', sdtText, False, 60);
  AddField(T, 'category', sdtText, False, 30);
  AddField(T, 'description', sdtText);
  AddField(T, 'recommended_uses', sdtText);
  AddField(T, 'notes', sdtText);
  AddField(T, 'can_delete', sdtBoolean);
  T.Fields.Last.DefaultValue := 1;
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterNestOwnersSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNestOwners;
  T.TableName := TBL_NEST_OWNERS;
  T.DisplayName := LocaleTablesDict[tbNestOwners];

  AddField(T, 'nest_owner_id', sdtInteger, True, 0, True);
  AddField(T, 'nest_id', sdtInteger, False, 0, False, True, tbNests);
  AddField(T, 'role', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'U,M,F,H,O';
  AddField(T, 'individual_id', sdtInteger);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterNestRevisionsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNestRevisions;
  T.TableName := TBL_NEST_REVISIONS;
  T.DisplayName := LocaleTablesDict[tbNestRevisions];

  AddField(T, 'nest_revision_id', sdtInteger, True, 0, True);
  AddField(T, 'nest_id', sdtInteger, False, 0, False, True, tbNests);
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'revision_date', sdtDate);
  AddField(T, 'revision_time', sdtTime);
  AddField(T, 'observer_1_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'observer_2_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'nest_status', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,I,A';
  AddField(T, 'host_eggs_tally', sdtInteger);
  AddField(T, 'host_nestlings_tally', sdtInteger);
  AddField(T, 'nidoparasite_eggs_tally', sdtInteger);
  AddField(T, 'nidoparasite_nestlings_tally', sdtInteger);
  AddField(T, 'nidoparasite_id', sdtInteger, False, 0, False, True, tbZooTaxa);
  AddField(T, 'have_philornis_larvae', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'nest_stage', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,X,C,L,I,H,N';
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterNestsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNests;
  T.TableName := TBL_NESTS;
  T.DisplayName := LocaleTablesDict[tbNests];

  AddField(T, 'nest_id', sdtInteger, True, 0, True);
  AddField(T, 'field_number', sdtText, True, 20);
  AddField(T, 'observer_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'locality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'taxon_id', sdtInteger, False, 0, False, True, tbZooTaxa);
  AddField(T, 'nest_shape', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'SC,CP,PT,SP,PD,PL,MN,BR,CV';
  AddField(T, 'support_type', sdtList, False, 10);
  T.Fields.Last.Rules.ValueList := 'G,H,F,L,D,C,R,B,A,O';
  AddField(T, 'support_plant_1_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'support_plant_2_id', sdtInteger, False, 0, False, True, tbBotanicTaxa);
  AddField(T, 'other_support', sdtText, False, 60);
  AddField(T, 'height_above_ground', sdtFloat);
  AddField(T, 'internal_max_diameter', sdtFloat);
  AddField(T, 'internal_min_diameter', sdtFloat);
  AddField(T, 'external_max_diameter', sdtFloat);
  AddField(T, 'external_min_diameter', sdtFloat);
  AddField(T, 'internal_height', sdtFloat);
  AddField(T, 'external_height', sdtFloat);
  AddField(T, 'edge_distance', sdtFloat);
  AddField(T, 'center_distance', sdtFloat);
  AddField(T, 'nest_cover', sdtInteger);
  AddField(T, 'plant_max_diameter', sdtFloat);
  AddField(T, 'plant_min_diameter', sdtFloat);
  AddField(T, 'plant_height', sdtFloat);
  AddField(T, 'plant_dbh', sdtFloat);
  AddField(T, 'construction_days', sdtFloat);
  AddField(T, 'incubation_days', sdtFloat);
  AddField(T, 'nestling_days', sdtFloat);
  AddField(T, 'active_days', sdtFloat);
  AddField(T, 'nest_fate', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'U,L,S';
  AddField(T, 'nest_productivity', sdtInteger);
  AddField(T, 'found_date', sdtDate);
  AddField(T, 'last_date', sdtDate);
  AddField(T, 'loss_cause', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'PRE,PAR,DIS,WEA,FIR,ABD,POL,HDT,IMN';
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'description', sdtText);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterNetsEffortSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbNetsEffort;
  T.TableName := TBL_NETS_EFFORT;
  T.DisplayName := LocaleTablesDict[tbNetsEffort];

  AddField(T, 'net_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_id', sdtInteger);
  AddField(T, 'net_station_id', sdtInteger, False, 0, False, True, tbSamplingPlots);
  AddField(T, 'permanent_net_id', sdtInteger, False, 0, False, True, tbPermanentNets);
  AddField(T, 'net_number', sdtInteger);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'sample_date', sdtDate);
  AddField(T, 'net_open_1', sdtTime);
  AddField(T, 'net_close_1', sdtTime);
  AddField(T, 'net_open_2', sdtTime);
  AddField(T, 'net_close_2', sdtTime);
  AddField(T, 'net_open_3', sdtTime);
  AddField(T, 'net_close_3', sdtTime);
  AddField(T, 'net_open_4', sdtTime);
  AddField(T, 'net_close_4', sdtTime);
  AddField(T, 'open_time_total', sdtFloat);
  T.Fields.Last.IsVirtual := True;
  AddField(T, 'net_length', sdtFloat);
  AddField(T, 'net_height', sdtFloat);
  AddField(T, 'net_area', sdtFloat);
  T.Fields.Last.IsVirtual := True;
  AddField(T, 'net_mesh', sdtInteger);
  AddField(T, 'full_name', sdtText, False, 40);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterPeopleSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPeople;
  T.TableName := TBL_PEOPLE;
  T.DisplayName := LocaleTablesDict[tbPeople];

  AddField(T, 'person_id', sdtInteger, True, 0, True);
  AddField(T, 'full_name', sdtText, True, 100);
  AddField(T, 'acronym', sdtText, True, 10);
  AddField(T, 'citation', sdtText, False, 100);
  AddField(T, 'title_treatment', sdtText, False, 15);
  AddField(T, 'national_id_card', sdtText, False, 15);
  AddField(T, 'social_security_number', sdtText, False, 15);
  AddField(T, 'gender', sdtText, False, 15);
  AddField(T, 'birth_date', sdtDate);
  AddField(T, 'death_date', sdtDate);
  AddField(T, 'email_addr', sdtText, False, 60);
  AddField(T, 'phone_1', sdtText, False, 20);
  AddField(T, 'phone_2', sdtText, False, 20);
  AddField(T, 'address_1', sdtText, False, 100);
  AddField(T, 'address_2', sdtText, False, 60);
  AddField(T, 'neighborhood', sdtText, False, 60);
  AddField(T, 'zip_code', sdtText, False, 15);
  AddField(T, 'country_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'state_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'municipality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'institution_id', sdtInteger, False, 0, False, True, tbInstitutions);
  AddField(T, 'department', sdtText, False, 100);
  AddField(T, 'job_role', sdtText, False, 100);
  AddField(T, 'lattes_uri', sdtText, False, 30);
  AddField(T, 'orcid_uri', sdtText, False, 30);
  AddField(T, 'twitter_uri', sdtText, False, 50);
  AddField(T, 'instagram_uri', sdtText, False, 50);
  AddField(T, 'website_uri', sdtText, False, 100);
  //AddField(T, 'profile_color', sdtText, False, 30);
  AddField(T, 'notes', sdtText);
  //AddField('profile_image', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterPermanentNetsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPermanentNets;
  T.TableName := TBL_PERMANENT_NETS;
  T.DisplayName := LocaleTablesDict[tbPermanentNets];

  AddField(T, 'permanent_net_id', sdtInteger, True, 0, True);
  AddField(T, 'sampling_plot_id', sdtInteger, True, 0, False, True, tbSamplingPlots);
  AddField(T, 'net_number', sdtInteger, True);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'notes', sdtText, False, 150);
  AddField(T, 'full_name', sdtText, False, 50);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterPermitsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPermits;
  T.TableName := TBL_PERMITS;
  T.DisplayName := LocaleTablesDict[tbPermits];

  AddField(T, 'permit_id', sdtInteger, True, 0, True);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'permit_name', sdtText, False, 150);
  AddField(T, 'permit_number', sdtText, False, 30);
  AddField(T, 'permit_type', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'B,C,R,E,T,O';
  AddField(T, 'dispatcher_name', sdtText, True, 100);
  AddField(T, 'dispatch_date', sdtDate);
  AddField(T, 'expire_date', sdtDate);
  AddField(T, 'notes', sdtText);
  AddField(T, 'permit_filename', sdtText, False, 200);
  //AddField('permit_file', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterPoiLibrarySchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbPoiLibrary;
  T.TableName := TBL_POI_LIBRARY;
  T.DisplayName := LocaleTablesDict[tbPoiLibrary];

  AddField(T, 'poi_id', sdtInteger, True, 0, True);
  AddField(T, 'sample_date', sdtDate, True);
  AddField(T, 'sample_time', sdtTime);
  AddField(T, 'poi_name', sdtText, False, 60);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'altitude', sdtFloat);
  AddField(T, 'observer_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'taxon_id', sdtInteger, False, 0, False, True, tbZooTaxa);
  AddField(T, 'individual_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'sighting_id', sdtInteger, False, 0, False, True, tbSightings);
  AddField(T, 'survey_id', sdtInteger, False, 0, False, True, tbSurveys);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterProjectBudgetsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectBudgets;
  T.TableName := TBL_PROJECT_BUDGET;
  T.DisplayName := LocaleTablesDict[tbProjectBudgets];

  AddField(T, 'budget_id', sdtInteger, True, 0, True);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'funding_source', sdtText, False, 60);
  AddField(T, 'rubric', sdtText, True, 60);
  AddField(T, 'item_name', sdtText, False, 60);
  AddField(T, 'amount', sdtFloat);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterProjectChronogramsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectChronograms;
  T.TableName := TBL_PROJECT_CHRONOGRAM;
  T.DisplayName := LocaleTablesDict[tbProjectChronograms];

  AddField(T, 'chronogram_id', sdtInteger, True, 0, True);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'description', sdtText);
  AddField(T, 'start_date', sdtDate);
  AddField(T, 'target_date', sdtDate);
  AddField(T, 'end_date', sdtDate);
  AddField(T, 'goal_id', sdtInteger, False, 0, False, True, tbProjectGoals);
  AddField(T, 'progress_status', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'T,P,F,C,D,R,B';
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterProjectExpensesSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectExpenses;
  T.TableName := TBL_PROJECT_EXPENSES;
  T.DisplayName := LocaleTablesDict[tbProjectExpenses];

  AddField(T, 'expense_id', sdtInteger, True, 0, True);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'budget_id', sdtInteger, False, 0, False, True, tbProjectBudgets);
  AddField(T, 'item_description', sdtText, True, 60);
  AddField(T, 'expense_date', sdtDate);
  AddField(T, 'amount', sdtFloat);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterProjectGoalsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectGoals;
  T.TableName := TBL_PROJECT_GOALS;
  T.DisplayName := LocaleTablesDict[tbProjectGoals];

  AddField(T, 'goal_id', sdtInteger, True, 0, True);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'goal_description', sdtText);
  AddField(T, 'goal_status', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'P,R,C';
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterProjectsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjects;
  T.TableName := TBL_PROJECTS;
  T.DisplayName := LocaleTablesDict[tbProjects];

  AddField(T, 'project_id', sdtInteger, True, 0, True);
  AddField(T, 'project_title', sdtText, True, 150);
  AddField(T, 'short_title', sdtText, False, 60);
  AddField(T, 'start_date', sdtDate);
  AddField(T, 'end_date', sdtDate);
  AddField(T, 'website_uri', sdtText, False, 200);
  AddField(T, 'email_addr', sdtText, False, 100);
  AddField(T, 'contact_name', sdtText, False, 100);
  AddField(T, 'protocol_number', sdtText, False, 30);
  AddField(T, 'project_abstract', sdtText);
  AddField(T, 'main_goal', sdtText);
  AddField(T, 'risks', sdtText);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterProjectTeamSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbProjectTeams;
  T.TableName := TBL_PROJECT_TEAM;
  T.DisplayName := LocaleTablesDict[tbProjectTeams];

  AddField(T, 'project_member_id', sdtInteger, True, 0, True);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'person_id', sdtInteger, True, 0, False, True, tbPeople);
  AddField(T, 'project_manager', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'institution_id', sdtInteger, False, 0, False, True, tbInstitutions);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSamplePrepsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSamplePreps;
  T.TableName := TBL_SAMPLE_PREPS;
  T.DisplayName := LocaleTablesDict[tbSamplePreps];

  AddField(T, 'sample_prep_id', sdtInteger, True, 0, True);
  AddField(T, 'specimen_id', sdtInteger);
  AddField(T, 'accession_num', sdtText, False, 20);
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'accession_type', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'NS,SS,MS,OW,WS,PS,N,EGG,P,F,BD,BL,BS,SX,GS,MC,TS,EYE,T,S,G,M';
  AddField(T, 'accession_seq', sdtInteger);
  AddField(T, 'taxon_id', sdtInteger);
  AddField(T, 'individual_id', sdtInteger);
  AddField(T, 'nest_id', sdtInteger);
  AddField(T, 'egg_id', sdtInteger);
  AddField(T, 'preparation_date', sdtDate);
  AddField(T, 'preparer_id', sdtInteger);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSamplingPlotsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSamplingPlots;
  T.TableName := TBL_SAMPLING_PLOTS;
  T.DisplayName := LocaleTablesDict[tbSamplingPlots];

  AddField(T, 'sampling_plot_id', sdtInteger, True, 0, True);
  AddField(T, 'full_name', sdtText, True, 100);
  AddField(T, 'acronym', sdtText, True, 10);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'area_shape', sdtText, False, 5);
  AddField(T, 'locality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'description', sdtText);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSightingsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSightings;
  T.TableName := TBL_SIGHTINGS;
  T.DisplayName := LocaleTablesDict[tbSightings];

  AddField(T, 'sighting_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_id', sdtInteger);
  AddField(T, 'individual_id', sdtInteger, False, 0, False, True, tbIndividuals);
  AddField(T, 'sighting_date', sdtDate);
  AddField(T, 'sighting_time', sdtTime);
  AddField(T, 'locality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'method_id', sdtInteger, False, 0, False, True, tbMethods);
  AddField(T, 'mackinnon_list_num', sdtInteger);
  AddField(T, 'observer_id', sdtInteger, False, 0, False, True, tbPeople);
  AddField(T, 'taxon_id', sdtInteger, False, 0, False, True, tbZooTaxa);
  AddField(T, 'subjects_tally', sdtInteger);
  AddField(T, 'subject_distance', sdtFloat);
  AddField(T, 'flight_height', sdtFloat);
  AddField(T, 'flight_direction', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,E,W,NE,NW,SE,SW';
  AddField(T, 'subject_seen', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_heard', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_photographed', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_recorded', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'subject_captured', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'males_tally', sdtText, False, 10);
  AddField(T, 'females_tally', sdtText, False, 10);
  AddField(T, 'not_sexed_tally', sdtText, False, 10);
  AddField(T, 'adults_tally', sdtText, False, 10);
  AddField(T, 'immatures_tally', sdtText, False, 10);
  AddField(T, 'not_aged_tally', sdtText, False, 10);
  AddField(T, 'new_captures_tally', sdtInteger);
  AddField(T, 'recaptures_tally', sdtInteger);
  AddField(T, 'unbanded_tally', sdtInteger);
  AddField(T, 'detection_type', sdtText, False, 30);
  AddField(T, 'breeding_status', sdtText, False, 30);
  AddField(T, 'not_surveying', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'ebird_available', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSpecimenCollectorsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSpecimenCollectors;
  T.TableName := TBL_SPECIMEN_COLLECTORS;
  T.DisplayName := LocaleTablesDict[tbSpecimenCollectors];

  AddField(T, 'collector_id', sdtInteger, True, 0, True);
  AddField(T, 'specimen_id', sdtInteger);
  AddField(T, 'person_id', sdtInteger);
  AddField(T, 'collector_seq', sdtInteger);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSpecimensSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSpecimens;
  T.TableName := TBL_SPECIMENS;
  T.DisplayName := LocaleTablesDict[tbSpecimens];

  AddField(T, 'specimen_id', sdtInteger, True, 0, True);
  AddField(T, 'field_number', sdtText, False, 20);
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'sample_type', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'WS,PS,N,B,E,P,F,BS,C,S,T,D,R';
  AddField(T, 'taxon_id', sdtInteger);
  AddField(T, 'individual_id', sdtInteger);
  AddField(T, 'nest_id', sdtInteger);
  AddField(T, 'egg_id', sdtInteger);
  AddField(T, 'collection_date', sdtDate);
  AddField(T, 'collection_day', sdtInteger);
  AddField(T, 'collection_month', sdtInteger);
  AddField(T, 'collection_year', sdtInteger);
  AddField(T, 'locality_id', sdtInteger);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSurveysSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSurveys;
  T.TableName := TBL_SURVEYS;
  T.DisplayName := LocaleTablesDict[tbSurveys];

  AddField(T, 'survey_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_date', sdtDate, True);
  AddField(T, 'start_time', sdtTime);
  AddField(T, 'end_time', sdtTime);
  AddField(T, 'duration', sdtInteger);
  AddField(T, 'method_id', sdtInteger, False, 0, False, True, tbMethods);
  AddField(T, 'net_station_id', sdtInteger, False, 0, False, True, tbSamplingPlots);
  AddField(T, 'expedition_id', sdtInteger, False, 0, False, True, tbExpeditions);
  AddField(T, 'project_id', sdtInteger, False, 0, False, True, tbProjects);
  AddField(T, 'locality_id', sdtInteger, False, 0, False, True, tbGazetteer);
  AddField(T, 'sample_id', sdtText, False, 30);
  AddField(T, 'start_latitude', sdtFloat);
  AddField(T, 'start_longitude', sdtFloat);
  AddField(T, 'end_latitude', sdtFloat);
  AddField(T, 'end_longitude', sdtFloat);
  AddField(T, 'observers_tally', sdtInteger);
  AddField(T, 'area_total', sdtFloat);
  AddField(T, 'distance_total', sdtFloat);
  AddField(T, 'nets_total', sdtInteger);
  AddField(T, 'habitat', sdtText);
  AddField(T, 'net_rounds', sdtText);
  AddField(T, 'full_name', sdtText, False, 100);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterSurveyTeamSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbSurveyTeams;
  T.TableName := TBL_SURVEY_TEAM;
  T.DisplayName := LocaleTablesDict[tbSurveyTeams];

  AddField(T, 'survey_member_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_id', sdtInteger);
  AddField(T, 'person_id', sdtInteger, True, 0, False, True, tbPeople);
  AddField(T, 'visitor', sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterVegetationSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbVegetation;
  T.TableName := TBL_VEGETATION;
  T.DisplayName := LocaleTablesDict[tbVegetation];

  AddField(T, 'vegetation_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_id', sdtInteger);
  AddField(T, 'sample_date', sdtDate, True);
  AddField(T, 'sample_time', sdtTime);
  AddField(T, 'longitude', sdtFloat);
  AddField(T, 'latitude', sdtFloat);
  AddField(T, 'observer_id', sdtInteger);
  AddField(T, 'herbs_proportion', sdtInteger);
  AddField(T, 'herbs_distribution', sdtInteger);
  AddField(T, 'herbs_avg_height', sdtInteger);
  AddField(T, 'shrubs_proportion', sdtInteger);
  AddField(T, 'shrubs_distribution', sdtInteger);
  AddField(T, 'shrubs_avg_height', sdtInteger);
  AddField(T, 'trees_proportion', sdtInteger);
  AddField(T, 'trees_distribution', sdtInteger);
  AddField(T, 'trees_avg_height', sdtInteger);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterWeatherLogsSchema(DB: TDatabaseSchema);
var
  T: TTableSchema;
begin
  T := TTableSchema.Create;
  T.TableType := tbWeatherLogs;
  T.TableName := TBL_WEATHER_LOGS;
  T.DisplayName := LocaleTablesDict[tbWeatherLogs];

  AddField(T, 'weather_id', sdtInteger, True, 0, True);
  AddField(T, 'survey_id', sdtInteger);
  AddField(T, 'sample_date', sdtDate, True);
  AddField(T, 'sample_time', sdtTime);
  AddField(T, 'sample_moment', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'S,M,E';
  AddField(T, 'observer_id', sdtInteger);
  AddField(T, 'cloud_cover', sdtInteger);
  AddField(T, 'precipitation', sdtList, False, 1);
  T.Fields.Last.Rules.ValueList := 'N,F,M,D,R';
  AddField(T, 'rainfall', sdtInteger);
  AddField(T, 'temperature', sdtFloat);
  AddField(T, 'wind_speed_bft', sdtInteger);
  AddField(T, 'wind_speed_kmh', sdtFloat);
  AddField(T, 'wind_direction', sdtList, False, 5);
  T.Fields.Last.Rules.ValueList := 'N,S,E,W,NE,NW,SE,SW';
  AddField(T, 'relative_humidity', sdtFloat);
  AddField(T, 'atmospheric_pressure', sdtFloat);
  AddField(T, 'notes', sdtText);
  AddField(T, COL_USER_INSERTED, sdtInteger);
  AddField(T, COL_USER_UPDATED, sdtInteger);
  AddField(T, COL_INSERT_DATE, sdtDateTime);
  AddField(T, COL_UPDATE_DATE, sdtDateTime);
  AddField(T, COL_EXPORTED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_MARKED_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 0;
  AddField(T, COL_ACTIVE_STATUS, sdtBoolean);
  T.Fields.Last.DefaultValue := 1;

  DB.Tables.Add(T);
end;

procedure RegisterDatabaseSchema;
begin
  if not Assigned(DBSchema) then
    DBSchema := TDatabaseSchema.Create;

  DBSchema.Tables.Clear;

  RegisterBotanicTaxaSchema(DBSchema);
  RegisterGazetteerSchema(DBSchema);
  RegisterInstitutionsSchema(DBSchema);
  RegisterProjectsSchema(DBSchema);
  RegisterExpeditionsSchema(DBSchema);
  RegisterSamplingPlotsSchema(DBSchema);
  RegisterPermanentNetsSchema(DBSchema);
  RegisterSurveysSchema(DBSchema);
  RegisterNetsEffortSchema(DBSchema);
  RegisterPeopleSchema(DBSchema);
  RegisterNestsSchema(DBSchema);
  RegisterBandsSchema(DBSchema);
  RegisterIndividualsSchema(DBSchema);
  RegisterEggsSchema(DBSchema);
  RegisterSightingsSchema(DBSchema);
  RegisterCapturesSchema(DBSchema);
  RegisterFeathersSchema(DBSchema);
  RegisterNestOwnersSchema(DBSchema);
  RegisterNestRevisionsSchema(DBSchema);
  RegisterPoiLibrarySchema(DBSchema);
  RegisterProjectGoalsSchema(DBSchema);
  RegisterProjectBudgetsSchema(DBSchema);
  RegisterProjectChronogramsSchema(DBSchema);
  RegisterProjectExpensesSchema(DBSchema);
  RegisterProjectTeamSchema(DBSchema);
  RegisterPermitsSchema(DBSchema);
  RegisterVegetationSchema(DBSchema);
  RegisterWeatherLogsSchema(DBSchema);
  RegisterSurveyTeamSchema(DBSchema);
  RegisterSpecimensSchema(DBSchema);
  RegisterSpecimenCollectorsSchema(DBSchema);
  RegisterSamplePrepsSchema(DBSchema);
end;

{ TFieldSchema }

function TFieldSchema.ConvertValue(const S: String): Variant;
var
  VInt: Int64;
  VFloat: Double;
  VDate, VTime, VDateTime: TDateTime;
begin
  // Empty field → use default or NULL
  if Trim(S) = '' then
  begin
    if not VarIsEmpty(DefaultValue) then
      Exit(DefaultValue)
    else
      Exit(Null);
  end;

  case DataType of

    sdtText:
      Result := S;

    sdtInteger:
      begin
        if TryStrToInt64(S, VInt) then
          Result := VInt
        else
          raise Exception.CreateFmt(rsErrorInvalidIntegerForField, [S, Name]);
      end;

    sdtFloat:
      begin
        if TryStrToFloat(S, VFloat) then
          Result := VFloat
        else
          raise Exception.CreateFmt(rsErrorInvalidNumberForField, [S, Name]);
      end;

    sdtBoolean:
      begin
        if SameText(S, 'true') or SameText(S, '1') or SameText(S, 'yes') or SameText(S, 'sim') then
          Result := True
        else if SameText(S, 'false') or SameText(S, '0') or SameText(S, 'no') or SameText(S, 'não') then
          Result := False
        else
          raise Exception.CreateFmt(rsErrorInvalidBooleanForField, [S, Name]);
      end;

    sdtDate:
      begin
        if TryStrToDate(S, VDate) then
          Result := VDate
        else
          raise Exception.CreateFmt(rsErrorInvalidDateForField, [S, Name]);
      end;

    sdtTime:
      begin
        if TryStrToTime(S, VTime) then
          Result := VTime
        else
          raise Exception.CreateFmt(rsErrorInvalidTimeForField, [S, Name]);
      end;

    sdtDateTime:
      begin
        if TryStrToDateTime(S, VDateTime) then
          Result := VDateTime
        else if TryStrToDate(S, VDate) then
          Result := VDate
        else if TryStrToTime(S, VTime) then
          Result := VTime
        else
          raise Exception.CreateFmt(rsErrorInvalidDateTimeForField, [S, Name]);
      end;

    sdtLookup:
      begin
        Result := ResolveLookup(S);
      end;

    sdtList:
      begin
        Result := S;
      end;

  else
    raise Exception.CreateFmt(rsErrorUnsupportedDataTypeForField, [Name]);
  end;
end;

function TFieldSchema.LookupTableName: string;
begin
  Result := TABLE_NAMES[LookupInfo.LookupTable];
end;

function TFieldSchema.LookupValueExists(const V: Variant): Boolean;
var
  SQL: string;
  Dummy: Integer;
begin
  if VarIsNull(V) then
    Exit(False);

  SQL := Format(
    'SELECT 1 FROM %s WHERE id = :id LIMIT 1',
    [LookupTableName]
  );

  Result := DBQuerySingleInt(SQL, 'id', V, Dummy);
end;

function TFieldSchema.ResolveLookup(const S: string): Variant;
var
  SQL, LookupIdField: string;
  LookupID: Integer;
begin
  // Empty value → return NULL
  if Trim(S) = '' then
    Exit(Null);

  // If already is integer, assume ID
  if TryStrToInt(S, LookupID) then
    Exit(LookupID);

  LookupInfo.LookupKeyField := PRIMARY_KEY_FIELDS[LookupInfo.LookupTable];

  if LookupInfo.LookupField = EmptyStr then
    LookupInfo.LookupField := LookupInfo.LookupKeyField;

  SQL := Format(
    'SELECT %s FROM %s WHERE %s = :value LIMIT 1',
    [LookupInfo.LookupKeyField, LookupTableName, LookupInfo.LookupField]
  );

  if not DBQuerySingleInt(SQL, 'value', S, LookupID) then
    raise Exception.CreateFmt(rsErrorNotFoundLookupValue, [S, LookupTableName]);

  Result := LookupID;
end;

procedure TFieldSchema.ValidateValue(const V: Variant);
var
  AllowedValues: TStringList;
begin
  // Required field
  if Rules.RequiredField and VarIsNull(V) then
    raise Exception.CreateFmt(rsErrorRequiredField, [Name]);

  // Maximum length (for text)
  if (DataType = sdtText) and (Rules.MaxLength > 0) then
    if (not VarIsNull(V)) and (Length(VarToStr(V)) > Rules.MaxLength) then
      raise Exception.CreateFmt(rsErrorValueExceededMaxLength, [VarToStr(V), Rules.MaxLength, Name]);

  // List of allowed values
  AllowedValues := TStringList.Create;
  try
    AllowedValues.CommaText := Rules.ValueList;
    if (DataType = sdtList) and (Rules.ValueList <> EmptyStr) then
      if (not VarIsNull(V)) and (AllowedValues.IndexOf(VarToStr(V)) < 0) then
        raise Exception.CreateFmt(rsErrorValueNotAllowedForField, [VarToStr(V), Name, Rules.ValueList]);
  finally
    AllowedValues.Free;
  end;

  // Lookup: validate if ID exists
  if (DataType = sdtLookup) and (not VarIsNull(V)) then
  begin
    if not LookupValueExists(V) then
      raise Exception.CreateFmt(rsErrorLookupValueNotFoundForField, [VarToStr(V), LookupTableName]);
  end;
end;

{ TTableSchema }

constructor TTableSchema.Create;
begin
  Fields := TFieldSchemaList.Create();
end;

destructor TTableSchema.Destroy;
begin
  Fields.Free;
  inherited Destroy;
end;

function TTableSchema.GetField(const Name: String): TFieldSchema;
var
  F: TFieldSchema;
begin
  Result := nil;
  for F in Fields do
    if SameText(F.Name, Name) then
      Exit(F);
end;

{ TDatabaseSchema }

constructor TDatabaseSchema.Create;
begin
  Tables := TTableSchemaList.Create();
end;

destructor TDatabaseSchema.Destroy;
begin
  Tables.Free;
  inherited Destroy;
end;

function TDatabaseSchema.GetTable(Name: String): TTableSchema;
var
  Tbl: TTableSchema;
begin
  Result := nil;
  for Tbl in Tables do
    if SameText(Tbl.TableName, Name) then
      Exit(Tbl);
end;

function TDatabaseSchema.GetTable(T: TTableType): TTableSchema;
var
  Tbl: TTableSchema;
begin
  Result := nil;
  for Tbl in Tables do
    if Tbl.TableType = T then
      Exit(Tbl);
end;

end.

