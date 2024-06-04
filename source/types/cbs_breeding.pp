{ Xolmis Breeding Data library

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

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
    FNestShape: String;
    FSupportType: String;
    FSupportPlant1Id: Integer;
    FSupportPlant2Id: Integer;
    FOtherSupport: String;
    FHeightAboveGround: Double;
    FProjectId: Integer;
    FInternalMaxDiameter: Double;
    FInternalMinDiameter: Double;
    FExternalMaxDiameter: Double;
    FExternalMinDiameter: Double;
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
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer); overload;
    procedure GetData(aDataSet: TDataSet); overload;
    function Find(aFieldNumber: String; aTaxon, aSite: Integer; aDate: TDate): Boolean;
    function Diff(aOld: TNest; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property FullName: String read FFullName write FFullName;
    property ObserverId: Integer read FObserverId write FObserverId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property ProjectId: Integer read FProjectId write FProjectId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property NestShape: String read FNestShape write FNestShape;
    property SupportType: String read FSupportType write FSupportType;
    property SupportPlant1Id: Integer read FSupportPlant1Id write FSupportPlant1Id;
    property SupportPlant2Id: Integer read FSupportPlant2Id write FSupportPlant2Id;
    property OtherSupport: String read FOtherSupport write FOtherSupport;
    property HeightAboveGround: Double read FHeightAboveGround write FHeightAboveGround;
    property InternalMaxDiameter: Double read FInternalMaxDiameter write FInternalMaxDiameter;
    property InternalMinDiameter: Double read FInternalMinDiameter write FInternalMinDiameter;
    property ExternalMaxDiameter: Double read FExternalMaxDiameter write FExternalMaxDiameter;
    property ExternalMinDiameter: Double read FExternalMinDiameter write FExternalMinDiameter;
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
    property Notes: String read FNotes write FNotes;
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
    procedure GetData(aKey: Integer); overload;
    procedure GetData(aDataSet: TDataSet); overload;
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
    procedure GetData(aKey: Integer); overload;
    procedure GetData(aDataSet: TDataSet); overload;
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
    procedure GetData(aKey: Integer); overload;
    procedure GetData(aDataSet: TDataSet); overload;
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
  cbs_locale, cbs_datacolumns, cbs_validations, udm_main;

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
      GetData(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevision.GetData(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
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
end;

function TNestRevision.Diff(aOld: TNestRevision; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.RevisionDate, FRevisionDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.RevisionTime, FRevisionTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserver1ID, aOld.Observer1Id, FObserver1Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserver2ID, aOld.Observer2Id, FObserver2Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStatus, aOld.NestStatus, FNestStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggsHost, aOld.HostEggsTally, FHostEggsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestlingsHost, aOld.HostNestlingsTally, FHostNestlingsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggsNidoparasite, aOld.NidoparasiteEggsTally, FNidoparasiteEggsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestlingsNidoparasite, aOld.NidoparasiteNestlingsTally, FNidoparasiteNestlingsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNidoparasiteID, aOld.NidoparasiteId, FNidoparasiteId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHasPhilornisLarvae, aOld.HavePhilornisLarvae, FHavePhilornisLarvae, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestStage, aOld.NestStage, FNestStage, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
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
      GetData(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEgg.GetData(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
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
end;

function TEgg.Diff(aOld: TEgg; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggShape, aOld.EggShape, FEggShape, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWidth, aOld.Width, FWidth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLength, aOld.Length, FLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMass, aOld.Mass, FMass, R) then
    aList.Add(R);
  if FieldValuesDiff(rscVolume, aOld.Volume, FVolume, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStage, aOld.EggStage, FEggStage, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggshellColor, aOld.EggshellColor, FEggshellColor, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggshellPattern, aOld.EggshellPattern, FEggshellPattern, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggshellTexture, aOld.EggshellTexture, FEggshellTexture, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHatched, aOld.EggHatched, FEggHatched, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.MeasureDate, FMeasureDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
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
  FNestShape := EmptyStr;
  FSupportType := EmptyStr;
  FSupportPlant1Id := 0;
  FSupportPlant2Id := 0;
  FOtherSupport := EmptyStr;
  FHeightAboveGround := 0.0;
  FProjectId := 0;
  FInternalMaxDiameter := 0.0;
  FInternalMinDiameter := 0.0;
  FExternalMaxDiameter := 0.0;
  FExternalMinDiameter := 0.0;
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
  FNotes := EmptyStr;
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
      GetData(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNest.GetData(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('nest_id').AsInteger;
    FFieldNumber := FieldByName('field_number').AsString;
    FFullName := FieldByName('full_name').AsString;
    FObserverId := FieldByName('observer_id').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FNestShape := FieldByName('nest_shape').AsString;
    FSupportType := FieldByName('support_type').AsString;
    FSupportPlant1Id := FieldByName('support_plant_1_id').AsInteger;
    FSupportPlant2Id := FieldByName('support_plant_2_id').AsInteger;
    FOtherSupport := FieldByName('other_support').AsString;
    FHeightAboveGround := FieldByName('height_above_ground').AsFloat;
    FProjectId := FieldByName('project_id').AsInteger;
    FInternalMaxDiameter := FieldByName('internal_max_diameter').AsFloat;
    FInternalMinDiameter := FieldByName('internal_min_diameter').AsFloat;
    FExternalMaxDiameter := FieldByName('external_max_diameter').AsFloat;
    FExternalMinDiameter := FieldByName('external_min_diameter').AsFloat;
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
    FNotes := FieldByName('notes').AsString;
    FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNest.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('INSERT INTO nests (field_number, observer_id, project_id, locality_id, longitude, latitude, ' +
      'taxon_id, nest_shape, support_type, support_plant_1_id, support_plant_2_id, other_support, ' +
      'height_above_ground, internal_max_diameter, internal_min_diameter, external_max_diameter, ' +
      'external_min_diameter, internal_height, external_height, edge_distance, center_distance, ' +
      'nest_cover, plant_max_diameter, plant_min_diameter, plant_height, plant_dbh, nest_fate,' +
      'nest_productivity, found_date, last_date, full_name, description, notes, ' +
      'construction_days, incubation_days, nestling_days, active_days, ' +
      'user_inserted, insert_date) ');
    Add('VALUES (:afieldnumber, :aobserver, :aproject, :alocality, :alongitude, :alatitude, ' +
      ':ataxon, :ashape, :asupporttype, :asupportplant1, :asupportplant2, :aothersupport, ' +
      ':aheightaboveground, :amaxinternaldiam, :amininternaldiam, :amaxexternaldiam, ' +
      ':aminexternaldiam, :ainternalheight, :aexternalheight, :aedgedistance, :acenterdistance, ' +
      ':acover, :amaxplantdiam, :aminplantdiam, :aplantheight, :aplantdbh, :afate, ' +
      ':aproductivity, :afounddate, :alastdate, :afullname, :adescription, :anotes, ' +
      ':constructiondays, :incubationdays, :nestlingdays, :activedays, ' +
      ':auser, datetime(''now'',''localtime''));');
    ParamByName('AFIELDNUMBER').AsString := FFieldNumber;
    ParamByName('AOBSERVER').AsInteger := FObserverId;
    ParamByName('APROJECT').AsInteger := FProjectId;
    ParamByName('ALOCALITY').AsInteger := FLocalityId;
    ParamByName('ALONGITUDE').AsFloat := FLongitude;
    ParamByName('ALATITUDE').AsFloat := FLatitude;
    ParamByName('ATAXON').AsInteger := FTaxonId;
    ParamByName('ASHAPE').AsString := FNestShape;
    ParamByName('ASUPPORTTYPE').AsString := FSupportType;
    ParamByName('ASUPPORTPLANT1').AsInteger := FSupportPlant1Id;
    ParamByName('ASUPPORTPLANT2').AsInteger := FSupportPlant2Id;
    ParamByName('AOTHERSUPPORT').AsString := FOtherSupport;
    ParamByName('AHEIGHTABOVEGROUND').AsFloat := FHeightAboveGround;
    ParamByName('AMAXINTERNALDIAM').AsFloat := FInternalMaxDiameter;
    ParamByName('AMININTERNALDIAM').AsFloat := FInternalMinDiameter;
    ParamByName('AMAXEXTERNALDIAM').AsFloat := FExternalMaxDiameter;
    ParamByName('AMINEXTERNALDIAM').AsFloat := FExternalMinDiameter;
    ParamByName('AINTERNALHEIGHT').AsFloat := FInternalHeight;
    ParamByName('AEXTERNALHEIGHT').AsFloat := FExternalHeight;
    ParamByName('AEDGEDISTANCE').AsFloat := FEdgeDistance;
    ParamByName('ACENTERDISTANCE').AsFloat := FCenterDistance;
    ParamByName('ACOVER').AsFloat := FNestCover;
    ParamByName('AMAXPLANTDIAM').AsFloat := FPlantMaxDiameter;
    ParamByName('AMINPLANTDIAM').AsFloat := FPlantMinDiameter;
    ParamByName('APLANTHEIGHT').AsFloat := FPlantHeight;
    ParamByName('APLANTDBH').AsFloat := FPlantDbh;
    ParamByName('AFATE').AsString := FNestFate;
    ParamByName('APRODUCTIVITY').AsInteger := FNestProductivity;
    ParamByName('AFOUNDDATE').AsDateTime := FFoundDate;
    ParamByName('ALASTDATE').AsDateTime := FLastDate;
    ParamByName('AFULLNAME').AsString := FFullName;
    ParamByName('ADESCRIPTION').AsString := FDescription;
    ParamByName('ANOTES').AsString := FNotes;
    ParamByName('CONSTRUCTIONDAYS').AsFloat := FConstructionDays;
    ParamByName('INCUBATIONDAYS').AsFloat := FIncubationDays;
    ParamByName('NESTLINGDAYS').AsFloat := FNestlingDays;
    ParamByName('ACTIVEDAYS').AsFloat := FActiveDays;
    ParamByName('AUSER').AsInteger := FUserInserted;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM nests');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNest.Update;
begin

end;

function TNest.Diff(aOld: TNest; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscShape, aOld.NestShape, FNestShape, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupportType, aOld.SupportType, FSupportType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupportPlant1ID, aOld.SupportPlant1Id, FSupportPlant1Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupportPlant2ID, aOld.SupportPlant2Id, FSupportPlant2Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOtherSupport, aOld.OtherSupport, FOtherSupport, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHeightAboveGround, aOld.HeightAboveGround, FHeightAboveGround, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMaxInternalDiameter, aOld.InternalMaxDiameter, FInternalMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMinInternalDiameter, aOld.InternalMinDiameter, FInternalMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMaxExternalDiameter, aOld.ExternalMaxDiameter, FExternalMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMinExternalDiameter, aOld.ExternalMinDiameter, FExternalMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscInternalHeight, aOld.InternalHeight, FInternalHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExternalHeight, aOld.ExternalHeight, FExternalHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantEdgeDistance, aOld.EdgeDistance, FEdgeDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantCenterDistance, aOld.CenterDistance, FCenterDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMaxPlantDiameter, aOld.PlantMaxDiameter, FPlantMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMinPlantDiameter, aOld.PlantMinDiameter, FPlantMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantHeight, aOld.PlantHeight, FPlantHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantDBH, aOld.PlantDbh, FPlantDbh, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCover, aOld.NestCover, FNestCover, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBuildingDays, aOld.ConstructionDays, FConstructionDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIncubationDays, aOld.IncubationDays, FIncubationDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestlingDays, aOld.NestlingDays, FNestlingDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscActiveDays, aOld.ActiveDays, FActiveDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestFate, aOld.NestFate, FNestFate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestProductivity, aOld.NestProductivity, FNestProductivity, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFoundDate, aOld.FoundDate, FFoundDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLastDateActive, aOld.LastDate, FLastDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TNest.Find(aFieldNumber: String; aTaxon, aSite: Integer; aDate: TDate): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT nest_id FROM nests');
    Add('WHERE (field_number = :afieldnumber)');
    Add('AND (taxon_id = :ataxon)');
    Add('AND (locality_id = :asite)');
    Add('AND (found_date = :adate)');
    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;
    ParamByName('ATAXON').AsInteger := aTaxon;
    ParamByName('ASITE').AsInteger := aSite;
    ParamByName('ADATE').AsDateTime := aDate;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('nest_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
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

  if FieldValuesDiff(rscRole, aOld.Role, FRole, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
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
      GetData(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestOwner.GetData(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
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
end;

end.

