unit cbs_breeding;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, cbs_record_types;

type
  TNestFate = (nfLoss, nfSuccess, nfUnknown);

const
  NestFates: array [TNestFate] of Char = ('L', 'S', 'U');

type

  { TNest }

  TNest = class(TXolmisRecord)
  protected
    FFieldNumber: String;
    FFullName: String;
    FObserverId: Integer;
    FLocalityId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FTaxonId: Integer;
    FSupportType: String;
    FSupportPlant1Id: Integer;
    FSupportPlant2Id: Integer;
    FOtherSupport: String;
    FHeightAboveGround: Double;
    FProjectId: Integer;
    FInternalDiameter: Double;
    FExternalDiameter: Double;
    FInternalHeight: Double;
    FExternalHeight: Double;
    FEdgeDistance: Double;
    FCenterDistance: Double;
    FNestCover: Integer;
    FPlantMaxDiameter: Double;
    FPlantMinDiameter: Double;
    FPlantHeight: Double;
    FPlantDbh: Double;
    FConstructionDays: Double;
    FIncubationDays: Double;
    FNestlingDays: Double;
    FActiveDays: Double;
    FNestFate: String;
    FNestProductivity: Integer;
    FFoundDate: TDate;
    FLastDate: TDate;
    FDescription: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TNest; var aList: TStrings): Boolean;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property FullName: String read FFullName write FFullName;
    property ObserverId: Integer read FObserverId write FObserverId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property ProjectId: Integer read FProjectId write FProjectId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property SupportType: String read FSupportType write FSupportType;
    property SupportPlant1Id: Integer read FSupportPlant1Id write FSupportPlant1Id;
    property SupportPlant2Id: Integer read FSupportPlant2Id write FSupportPlant2Id;
    property OtherSupport: String read FOtherSupport write FOtherSupport;
    property HeightAboveGround: Double read FHeightAboveGround write FHeightAboveGround;
    property InternalDiameter: Double read FInternalDiameter write FInternalDiameter;
    property ExternalDiameter: Double read FExternalDiameter write FExternalDiameter;
    property InternalHeight: Double read FInternalHeight write FInternalHeight;
    property ExternalHeight: Double read FExternalHeight write FExternalHeight;
    property EdgeDistance: Double read FEdgeDistance write FEdgeDistance;
    property CenterDistance: Double read FCenterDistance write FCenterDistance;
    property NestCover: Integer read FNestCover write FNestCover;
    property PlantMaxDiameter: Double read FPlantMaxDiameter write FPlantMaxDiameter;
    property PlantMinDiameter: Double read FPlantMinDiameter write FPlantMinDiameter;
    property PlantHeight: Double read FPlantHeight write FPlantHeight;
    property PlantDbh: Double read FPlantDbh write FPlantDbh;
    property ConstructionDays: Double read FConstructionDays write FConstructionDays;
    property IncubationDays: Double read FIncubationDays write FIncubationDays;
    property NestlingDays: Double read FNestlingDays write FNestlingDays;
    property ActiveDays: Double read FActiveDays write FActiveDays;
    property NestFate: String read FNestFate write FNestFate;
    property NestProductivity: Integer read FNestProductivity write FNestProductivity;
    property FoundDate: TDate read FFoundDate write FFoundDate;
    property LastDate: TDate read FLastDate write FLastDate;
    property Description: String read FDescription write FDescription;
  end;

type

  { TNestOwner }

  TNestOwner = class(TXolmisRecord)
  private
    FIndividualId: Integer;
    FNestId: Integer;
    FRole: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TNestOwner; var aList: TStrings): Boolean;
  published
    property NestId: Integer read FNestId write FNestId;
    property Role: String read FRole write FRole;
    property IndividualId: Integer read FIndividualId write FIndividualId;
  end;

type
  TEggShape = (esSpherical, esElliptical, esOval, esPiriform);

const
  EggShapes: array [TEggShape] of Char = ('S', 'E', 'O', 'P');

type

  { TEgg }

  TEgg = class(TXolmisRecord)
  protected
    FFieldNumber: String;
    FNestId: Integer;
    FFullName: String;
    FEggShape: String;
    FWidth: Double;
    FLength: Double;
    FMass: Double;
    FVolume: Double;
    FEggStage: String;
    FEggshellColor: String;
    FEggshellPattern: String;
    FEggshellTexture: String;
    FEggHatched: Boolean;
    FIndividualId: Integer;
    FMeasureDate: TDate;
    FTaxonId: Integer;
    FDescription: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TEgg; var aList: TStrings): Boolean;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property NestId: Integer read FNestId write FNestId;
    property FullName: String read FFullName write FFullName;
    property EggShape: String read FEggShape write FEggShape;
    property Width: Double read FWidth write FWidth;
    property Length: Double read FLength write FLength;
    property Mass: Double read FMass write FMass;
    property Volume: Double read FVolume write FVolume;
    property EggStage: String read FEggStage write FEggStage;
    property EggshellColor: String read FEggshellColor write FEggshellColor;
    property EggshellPattern: String read FEggshellPattern write FEggshellPattern;
    property EggshellTexture: String read FEggshellTexture write FEggshellTexture;
    property EggHatched: Boolean read FEggHatched write FEggHatched;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property MeasureDate: TDate read FMeasureDate write FMeasureDate;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property Description: String read FDescription write FDescription;
  end;

type
  TNestStatus = (nstInactive, nstActive, nstUnknown);
  TNestStage = (nsgInactive, nsgConstruction, nsgLaying, nsgIncubation, nsgHatching, nsgNestling,
    nsgUnknown);

const
  NestStates: array [TNestStatus] of Char = ('I', 'A', 'U');
  NestStages: array [TNestStage] of Char = ('X', 'C', 'L', 'I', 'H', 'N', 'U');

type

  { TNestRevision }

  TNestRevision = class(TXolmisRecord)
  protected
    FNestId: Integer;
    FFullName: String;
    FRevisionDate: TDate;
    FRevisionTime: TTime;
    FObserver1Id: Integer;
    FObserver2Id: Integer;
    FNestStatus: String;
    FHostEggsTally: Integer;
    FHostNestlingsTally: Integer;
    FNidoparasiteEggsTally: Integer;
    FNidoparasiteNestlingsTally: Integer;
    FNidoparasiteId: Integer;
    FHavePhilornisLarvae: Boolean;
    FNestStage: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TNestRevision; var aList: TStrings): Boolean;
  published
    property NestId: Integer read FNestId write FNestId;
    property FullName: String read FFullName write FFullName;
    property RevisionDate: TDate read FRevisionDate write FRevisionDate;
    property RevisionTime: TTime read FRevisionTime write FRevisionTime;
    property Observer1Id: Integer read FObserver1Id write FObserver1Id;
    property Observer2Id: Integer read FObserver2Id write FObserver2Id;
    property NestStatus: String read FNestStatus write FNestStatus;
    property HostEggsTally: Integer read FHostEggsTally write FHostEggsTally;
    property HostNestlingsTally: Integer read FHostNestlingsTally write FHostNestlingsTally;
    property NidoparasiteEggsTally: Integer read FNidoparasiteEggsTally write FNidoparasiteEggsTally;
    property NidoparasiteNestlingsTally: Integer read FNidoparasiteNestlingsTally write FNidoparasiteNestlingsTally;
    property NidoparasiteId: Integer read FNidoparasiteId write FNidoparasiteId;
    property HavePhilornisLarvae: Boolean read FHavePhilornisLarvae write FHavePhilornisLarvae;
    property NestStage: String read FNestStage write FNestStage;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  cbs_locale, cbs_validations, udm_main;

{ TNestRevision }

constructor TNestRevision.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TNestRevision.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FNestId := 0;
  FRevisionDate := StrToDate('30/12/1500');
  FRevisionTime := StrToTime('00:00:00');
  FObserver1Id := 0;
  FObserver2Id := 0;
  FNestStatus := EmptyStr;
  FHostEggsTally := 0;
  FHostNestlingsTally := 0;
  FNidoparasiteEggsTally := 0;
  FNidoparasiteNestlingsTally := 0;
  FNidoparasiteId := 0;
  FHavePhilornisLarvae := False;
  FNestStage := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TNestRevision.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM nest_revisions');
    Add('WHERE nest_revision_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('nest_revision_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FNestId := FieldByName('nest_id').AsInteger;
      FRevisionDate := FieldByName('revision_date').AsDateTime;
      FRevisionTime := FieldByName('revision_time').AsDateTime;
      FObserver1Id := FieldByName('observer_1_id').AsInteger;
      FObserver2Id := FieldByName('observer_2_id').AsInteger;
      FNestStatus := FieldByName('nest_status').AsString;
      FHostEggsTally := FieldByName('host_eggs_tally').AsInteger;
      FHostNestlingsTally := FieldByName('host_nestlings_tally').AsInteger;
      FNidoparasiteEggsTally := FieldByName('nidoparasite_eggs_tally').AsInteger;
      FNidoparasiteNestlingsTally := FieldByName('nidoparasite_nestlings_tally').AsInteger;
      FNidoparasiteId := FieldByName('nidoparasite_id').AsInteger;
      FHavePhilornisLarvae := FieldByName('have_philornis_larvae').AsBoolean;
      FNestStage := FieldByName('nest_stage').AsString;
      FNotes := FieldByName('notes').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestRevision.Diff(aOld: TNestRevision; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rsCaptionNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionDate, aOld.RevisionDate, FRevisionDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Hora', aOld.RevisionTime, FRevisionTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Observador 1', aOld.Observer1Id, FObserver1Id, R) then
    aList.Add(R);
  if FieldValuesDiff('Observador 2', aOld.Observer2Id, FObserver2Id, R) then
    aList.Add(R);
  if FieldValuesDiff('Status', aOld.NestStatus, FNestStatus, R) then
    aList.Add(R);
  if FieldValuesDiff('# ovos (hospedeiro)', aOld.HostEggsTally, FHostEggsTally, R) then
    aList.Add(R);
  if FieldValuesDiff('# ninhegos (hospedeiro)', aOld.HostNestlingsTally, FHostNestlingsTally, R) then
    aList.Add(R);
  if FieldValuesDiff('# ovos nidoparasita', aOld.NidoparasiteEggsTally, FNidoparasiteEggsTally, R) then
    aList.Add(R);
  if FieldValuesDiff('# ninhegos nidoparasita', aOld.NidoparasiteNestlingsTally, FNidoparasiteNestlingsTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Nidoparasita', aOld.NidoparasiteId, FNidoparasiteId, R) then
    aList.Add(R);
  if FieldValuesDiff('Philornis', aOld.HavePhilornisLarvae, FHavePhilornisLarvae, R) then
    aList.Add(R);
  if FieldValuesDiff('Estágio', aOld.NestStage, FNestStage, R) then
    aList.Add(R);
  if FieldValuesDiff('Anotações', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TEgg }

constructor TEgg.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TEgg.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FFieldNumber := EmptyStr;
  FNestId := 0;
  FEggShape := EmptyStr;
  FWidth := 0.0;
  FLength := 0.0;
  FMass := 0.0;
  FVolume := 0.0;
  FEggStage := EmptyStr;
  FEggshellColor := EmptyStr;
  FEggshellPattern := EmptyStr;
  FEggshellTexture := EmptyStr;
  FEggHatched := False;
  FIndividualId := 0;
  FMeasureDate := StrToDate('30/12/1500');
  FTaxonId := 0;
  FDescription := EmptyStr;
end;

procedure TEgg.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM eggs');
    Add('WHERE egg_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('egg_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FFieldNumber := FieldByName('field_number').AsString;
      FNestId := FieldByName('nest_id').AsInteger;
      FEggShape := FieldByName('egg_shape').AsString;
      FWidth := FieldByName('egg_width').AsFloat;
      FLength := FieldByName('egg_length').AsFloat;
      FMass := FieldByName('egg_mass').AsFloat;
      FVolume := FieldByName('egg_volume').AsFloat;
      FEggStage := FieldByName('egg_stage').AsString;
      FEggshellColor := FieldByName('eggshell_color').AsString;
      FEggshellPattern := FieldByName('eggshell_pattern').AsString;
      FEggshellTexture := FieldByName('eggshell_texture').AsString;
      FEggHatched := FieldByName('egg_hatched').AsBoolean;
      FIndividualId := FieldByName('individual_id').AsInteger;
      FMeasureDate := FieldByName('measure_date').AsDateTime;
      FTaxonId := FieldByName('taxon_id').AsInteger;
      FDescription := FieldByName('description').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TEgg.Diff(aOld: TEgg; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  if FieldValuesDiff('Número de campo', aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff('Formato', aOld.EggShape, FEggShape, R) then
    aList.Add(R);
  if FieldValuesDiff('Largura', aOld.Width, FWidth, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento', aOld.Length, FLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Massa', aOld.Mass, FMass, R) then
    aList.Add(R);
  if FieldValuesDiff('Volume', aOld.Volume, FVolume, R) then
    aList.Add(R);
  if FieldValuesDiff('Estágio', aOld.EggStage, FEggStage, R) then
    aList.Add(R);
  if FieldValuesDiff('Cor', aOld.EggshellColor, FEggshellColor, R) then
    aList.Add(R);
  if FieldValuesDiff('Manchas', aOld.EggshellPattern, FEggshellPattern, R) then
    aList.Add(R);
  if FieldValuesDiff('Textura', aOld.EggshellTexture, FEggshellTexture, R) then
    aList.Add(R);
  if FieldValuesDiff('Eclodiu', aOld.EggHatched, FEggHatched, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff('Data da medição', aOld.MeasureDate, FMeasureDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Descrição', aOld.Description, FDescription, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TNest }

constructor TNest.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TNest.Clear;
begin
  inherited Clear;
  FFieldNumber := EmptyStr;
  FFullName := EmptyStr;
  FObserverId := 0;
  FLocalityId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FTaxonId := 0;
  FSupportType := EmptyStr;
  FSupportPlant1Id := 0;
  FSupportPlant2Id := 0;
  FOtherSupport := EmptyStr;
  FHeightAboveGround := 0.0;
  FProjectId := 0;
  FInternalDiameter := 0.0;
  FExternalDiameter := 0.0;
  FInternalHeight := 0.0;
  FExternalHeight := 0.0;
  FEdgeDistance := 0.0;
  FCenterDistance := 0.0;
  FNestCover := 0;
  FPlantMaxDiameter := 0.0;
  FPlantMinDiameter := 0.0;
  FPlantHeight := 0.0;
  FPlantDbh := 0.0;
  FConstructionDays := 0.0;
  FIncubationDays := 0.0;
  FNestlingDays := 0.0;
  FActiveDays := 0.0;
  FNestFate := EmptyStr;
  FNestProductivity := 0;
  FFoundDate := StrToDate('30/12/1500');
  FLastDate := StrToDate('30/12/1500');
  FDescription := EmptyStr;
end;

procedure TNest.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM nests');
    Add('WHERE nest_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('nest_id').AsInteger;
      FFieldNumber := FieldByName('field_number').AsString;
      FFullName := FieldByName('full_name').AsString;
      FObserverId := FieldByName('observer_id').AsInteger;
      FLocalityId := FieldByName('locality_id').AsInteger;
      FLatitude := FieldByName('latitude').AsFloat;
      FLongitude := FieldByName('longitude').AsFloat;
      FTaxonId := FieldByName('taxon_id').AsInteger;
      FSupportType := FieldByName('support_type').AsString;
      FSupportPlant1Id := FieldByName('support_plant_1_id').AsInteger;
      FSupportPlant2Id := FieldByName('support_plant_2_id').AsInteger;
      FOtherSupport := FieldByName('other_support').AsString;
      FHeightAboveGround := FieldByName('height_above_ground').AsFloat;
      FProjectId := FieldByName('project_id').AsInteger;
      FInternalDiameter := FieldByName('internal_diameter').AsFloat;
      FExternalDiameter := FieldByName('external_diameter').AsFloat;
      FInternalHeight := FieldByName('internal_height').AsFloat;
      FExternalHeight := FieldByName('external_height').AsFloat;
      FEdgeDistance := FieldByName('edge_distance').AsFloat;
      FCenterDistance := FieldByName('center_distance').AsFloat;
      FNestCover := FieldByName('nest_cover').AsInteger;
      FPlantMaxDiameter := FieldByName('plant_max_diameter').AsFloat;
      FPlantMinDiameter := FieldByName('plant_min_diameter').AsFloat;
      FPlantHeight := FieldByName('plant_height').AsFloat;
      FPlantDbh := FieldByName('plant_dbh').AsFloat;
      FConstructionDays := FieldByName('construction_days').AsFloat;
      FIncubationDays := FieldByName('incubation_days').AsFloat;
      FNestlingDays := FieldByName('nestling_days').AsFloat;
      FActiveDays := FieldByName('active_days').AsFloat;
      FNestFate := FieldByName('nest_fate').AsString;
      FNestProductivity := FieldByName('nest_productivity').AsInteger;
      FFoundDate := FieldByName('found_date').AsDateTime;
      FLastDate := FieldByName('last_date').AsDateTime;
      FDescription := FieldByName('description').AsString;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNest.Diff(aOld: TNest; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('N'#186' campo', aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Observador', aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionLocality, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Tipo suporte', aOld.SupportType, FSupportType, R) then
    aList.Add(R);
  if FieldValuesDiff('Planta suporte 1', aOld.SupportPlant1Id, FSupportPlant1Id, R) then
    aList.Add(R);
  if FieldValuesDiff('Planta suporte 2', aOld.SupportPlant2Id, FSupportPlant2Id, R) then
    aList.Add(R);
  if FieldValuesDiff('Outro suporte', aOld.OtherSupport, FOtherSupport, R) then
    aList.Add(R);
  if FieldValuesDiff('Altura solo', aOld.HeightAboveGround, FHeightAboveGround, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionProject, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff('Di'#226'metro interno', aOld.InternalDiameter, FInternalDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff('Di'#226'metro externo', aOld.ExternalDiameter, FExternalDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff('Altura interna', aOld.InternalHeight, FInternalHeight, R) then
    aList.Add(R);
  if FieldValuesDiff('Altura externa', aOld.ExternalHeight, FExternalHeight, R) then
    aList.Add(R);
  if FieldValuesDiff('Dist'#226'ncia borda', aOld.EdgeDistance, FEdgeDistance, R) then
    aList.Add(R);
  if FieldValuesDiff('Dist'#226'ncia fuste', aOld.CenterDistance, FCenterDistance, R) then
    aList.Add(R);
  if FieldValuesDiff('Diâmetro máximo da planta', aOld.PlantMaxDiameter, FPlantMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff('Diâmetro mínimo da planta', aOld.PlantMinDiameter, FPlantMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff('Altura da planta', aOld.PlantHeight, FPlantHeight, R) then
    aList.Add(R);
  if FieldValuesDiff('Diâmetro à altura do peito', aOld.PlantDbh, FPlantDbh, R) then
    aList.Add(R);
  if FieldValuesDiff('Cobertura', aOld.NestCover, FNestCover, R) then
    aList.Add(R);
  if FieldValuesDiff('Dias constru'#231#227'o', aOld.ConstructionDays, FConstructionDays, R) then
    aList.Add(R);
  if FieldValuesDiff('Dias incuba'#231#227'o', aOld.IncubationDays, FIncubationDays, R) then
    aList.Add(R);
  if FieldValuesDiff('Dias ninhego', aOld.NestlingDays, FNestlingDays, R) then
    aList.Add(R);
  if FieldValuesDiff('Dias ativo', aOld.ActiveDays, FActiveDays, R) then
    aList.Add(R);
  if FieldValuesDiff('Destino', aOld.NestFate, FNestFate, R) then
    aList.Add(R);
  if FieldValuesDiff('Produtividade', aOld.NestProductivity, FNestProductivity, R) then
    aList.Add(R);
  if FieldValuesDiff('Data encontro', aOld.FoundDate, FFoundDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Data final', aOld.LastDate, FLastDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Descri'#231#227'o', aOld.Description, FDescription, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TNestOwner }

constructor TNestOwner.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TNestOwner.Clear;
begin
  inherited Clear;
  FNestId := 0;
  FRole := EmptyStr;
  FIndividualId := 0;
end;

function TNestOwner.Diff(aOld: TNestOwner; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Role', aOld.Role, FRole, R) then
    aList.Add(R);
  if FieldValuesDiff('Individual', aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

procedure TNestOwner.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM nest_owners');
    Add('WHERE nest_owner_id = :anid');
    ParamByName('ANID').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('nest_owner_id').AsInteger;
      FNestId := FieldByName('nest_id').AsInteger;
      FRole := FieldByName('role').AsString;
      FIndividualId := FieldByName('individual_id').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

