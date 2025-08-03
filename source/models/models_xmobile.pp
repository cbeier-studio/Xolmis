{ Xolmis Mobile library

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit models_xmobile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, DateUtils, RegExpr, StrUtils,
  models_sampling, models_birds, models_geo, models_breeding;

type
  TMobileContentType = (mctEmpty, mctInventory, mctInventories, mctNest, mctNests, mctSpecimens);
  TMobileInventoryType = (invQualitativeFree, invQualitativeTimed, invQualitativeInterval, invMackinnonList,
                          invTransectionCount, invPointCount, invBanding, invCasual);

type

  { TMobilePoi }

  TMobilePoi = class
  public
    FId: Integer;
    FSpeciesId: Integer;
    FSampleTime: TDateTime;
    FLongitude: Extended;
    FLatitude: Extended;
  public
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure ToPoi(aPoi: TPoi);
  end;

  TMobilePoiList = specialize TFPGObjectList<TMobilePoi>;

  { TMobileSpecies }

  TMobileSpecies = class
  public
    FId: Integer;
    FInventoryId: String;
    FSpeciesName: String;
    FIsOutOfInventory: Boolean;
    FCount: Integer;
    FNotes: String;
    FSampleTime: TDateTime;
    FPoiList: TMobilePoiList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure LoadPoiList(JSON: TJSONArray);
    procedure ToSighting(aSighting: TSighting);
  end;

  { TMobileVegetation }

  TMobileVegetation = class
  public
    FId: Integer;
    FInventoryId: String;
    FSampleTime: TDateTime;
    FLongitude: Extended;
    FLatitude: Extended;
    FHerbsProportion: Integer;
    FHerbsDistribution: TStratumDistribution;
    FHerbsHeight: Integer;
    FShrubsProportion: Integer;
    FShrubsDistribution: TStratumDistribution;
    FShrubsHeight: Integer;
    FTreesProportion: Integer;
    FTreesDistribution: TStratumDistribution;
    FTreesHeight: Integer;
    FNotes: String;
  public
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure ToVegetation(aVegetation: TVegetation);
  end;

  { TMobileWeather }

  TMobileWeather = class
  public
    FId: Integer;
    FInventoryId: String;
    FSampleTime: TDateTime;
    FCloudCover: Integer;
    FPrecipitation: TPrecipitation;
    FTemperature: Double;
    FWindSpeed: Integer;
  public
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure ToWeatherLog(aWeatherLog: TWeatherLog);
  end;

  TMobileSpeciesList = specialize TFPGObjectList<TMobileSpecies>;
  TMobileVegetationList = specialize TFPGObjectList<TMobileVegetation>;
  TMobileWeatherList = specialize TFPGObjectList<TMobileWeather>;

  { TMobileInventory }

  TMobileInventory = class
  public
    FId: String;
    FType: TMobileInventoryType;
    FDuration: Integer;
    FMaxSpecies: Integer;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FStartLongitude: Extended;
    FStartLatitude: Extended;
    FEndLongitude: Extended;
    FEndLatitude: Extended;
    FLocalityName: String;
    FCurrentInterval: Integer;
    FIntervalsWithoutNewSpecies: Integer;
    FCurrentIntervalSpeciesCount: Integer;
    FSurveyKey: Integer;
    FObserver: String;
    FListNumber: Integer;
    FImport: Boolean;
    FSpeciesList: TMobileSpeciesList;
    FVegetationList: TMobileVegetationList;
    FWeatherList: TMobileWeatherList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure LoadSpeciesList(JSON: TJSONArray);
    procedure LoadVegetationList(JSON: TJSONArray);
    procedure LoadWeatherList(JSON: TJSONArray);
    procedure ToSurvey(aSurvey: TSurvey);
  end;

  TMobileInventoryList = specialize TFPGObjectList<TMobileInventory>;

type

  { TMobileEgg }

  TMobileEgg = class
  public
    FId: Integer;
    FNestId: Integer;
    FSampleTime: TDateTime;
    FFieldNumber: String;
    FEggShape: TEggShape;
    FWidth: Double;
    FLength: Double;
    FMass: Double;
    FSpeciesName: String;
  public
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure ToEgg(aEgg: TEgg);
  end;

  { TMobileNestRevision }

  TMobileNestRevision = class
  public
    FId: Integer;
    FNestId: Integer;
    FSampleTime: TDateTime;
    FNestStatus: TNestStatus;
    FNestStage: TNestStage;
    FEggsHost: Integer;
    FNestlingsHost: Integer;
    FEggsParasite: Integer;
    FNestlingsParasite: Integer;
    FHasPhilornisLarvae: Boolean;
    FNotes: String;
  public
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure ToNestRevision(aRevision: TNestRevision);
  end;

  TMobileEggList = specialize TFPGObjectList<TMobileEgg>;
  TMobileRevisionList = specialize TFPGObjectList<TMobileNestRevision>;

  { TMobileNest }

  TMobileNest = class
  public
    FId: Integer;
    FFieldNumber: String;
    FSpeciesName: String;
    FLocalityName: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FSupport: String;
    FHeightAboveGround: Double;
    FFoundTime: TDateTime;
    FLastTime: TDateTime;
    FNestFate: TNestFate;
    FMale: String;
    FFemale: String;
    FHelpers: String;
    FIsActive: Boolean;
    FObserver: String;
    FNestKey: Integer;
    FImport: Boolean;
    FRevisionList: TMobileRevisionList;
    FEggList: TMobileEggList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure LoadRevisionList(JSON: TJSONArray);
    procedure LoadEggList(JSON: TJSONArray);
    procedure ToNest(aNest: TNest);
  end;

  TMobileNestList = specialize TFPGObjectList<TMobileNest>;

type

  { TMobileSpecimen }

  TMobileSpecimen = class
  public
    FId: Integer;
    FSampleTime: TDateTime;
    FFieldNumber: String;
    FType: TSpecimenType;
    FLongitude: Extended;
    FLatitude: Extended;
    FLocality: String;
    FSpeciesName: String;
    FNotes: String;
    FIsPending: Boolean;
    FObserver: String;
    FSpecimenKey: Integer;
    FImport: Boolean;
  public
    procedure Clear;
    procedure FromJSON(JSON: TJSONData);
    procedure ToSpecimen(aSpecimen: TSpecimen);
  end;

  TMobileSpecimenList = specialize TFPGObjectList<TMobileSpecimen>;


implementation

uses
  utils_locale, utils_global, data_consts, data_getvalue, utils_conversions;

{ TMobilePoi }

procedure TMobilePoi.Clear;
begin
  FId := 0;
  FSpeciesId := 0;
  FSampleTime := NullDateTime;
  FLongitude := 0.0;
  FLatitude := 0.0;
end;

procedure TMobilePoi.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FSpeciesId := JSONObj.Get('speciesId', 0);
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    FLongitude := JSONObj.Get('longitude', 0.0);
    FLatitude := JSONObj.Get('latitude', 0.0);
  end;
end;

procedure TMobilePoi.ToPoi(aPoi: TPoi);
begin
  aPoi.TaxonId := FSpeciesId;
  aPoi.SampleDate := FSampleTime;
  aPoi.SampleTime := FSampleTime;
  aPoi.Longitude := FLongitude;
  aPoi.Latitude := FLatitude;
end;

{ TMobileSpecies }

constructor TMobileSpecies.Create;
begin
  FPoiList := TMobilePoiList.Create;
end;

procedure TMobileSpecies.Clear;
begin
  FId := 0;
  FInventoryId := EmptyStr;
  FSpeciesName := EmptyStr;
  FIsOutOfInventory := False;
  FCount := 0;
  FNotes := EmptyStr;
  FSampleTime := NullDateTime;

  FPoiList.Clear;
end;

destructor TMobileSpecies.Destroy;
begin
  FPoiList.Free;
  inherited Destroy;
end;

procedure TMobileSpecies.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
  PoiArray: TJSONArray;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FInventoryId := JSONObj.Get('inventoryId', '');
    FSpeciesName := JSONObj.Get('name', '');
    FIsOutOfInventory := JSONObj.Get('isOutOfInventory', 0) = 1;
    FCount := JSONObj.Get('count', 0);
    FNotes := JSONObj.Get('notes', '');
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    PoiArray := JSONObj.FindPath('pois') as TJSONArray;
    if Assigned(PoiArray) then
      LoadPoiList(PoiArray);
  end;
end;

procedure TMobileSpecies.LoadPoiList(JSON: TJSONArray);
var
  i: Integer;
  PoiObj: TMobilePoi;
begin
  FPoiList.Clear;
  for i := 0 to JSON.Count - 1 do
  begin
    PoiObj := TMobilePoi.Create;
    PoiObj.FromJSON(JSON.Items[i]);
    FPoiList.Add(PoiObj);
  end;
end;

procedure TMobileSpecies.ToSighting(aSighting: TSighting);
begin
  aSighting.TaxonId := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, FSpeciesName);
  aSighting.NotSurveying := FIsOutOfInventory;
  aSighting.SubjectTally := FCount;
  aSighting.Notes := FNotes;
  aSighting.SightingDate := FSampleTime;
  aSighting.SightingTime := FSampleTime;
end;

{ TMobileVegetation }

procedure TMobileVegetation.Clear;
begin
  FId := 0;
  FInventoryId := EmptyStr;
  FSampleTime := NullDateTime;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FHerbsProportion := 0;
  FHerbsDistribution := disNone;
  FHerbsHeight := 0;
  FShrubsProportion := 0;
  FShrubsDistribution := disNone;
  FShrubsHeight := 0;
  FTreesProportion := 0;
  FTreesDistribution := disNone;
  FTreesHeight := 0;
  FNotes := EmptyStr;
end;

procedure TMobileVegetation.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FInventoryId := JSONObj.Get('inventoryId', '');
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    FLongitude := JSONObj.Get('longitude', 0.0);
    FLatitude := JSONObj.Get('latitude', 0.0);
    FHerbsProportion := JSONObj.Get('herbsProportion', 0);
    FHerbsDistribution := TStratumDistribution(JSONObj.Get('herbsDistribution', Integer(disNone)));
    FHerbsHeight := JSONObj.Get('herbsHeight', 0);
    FShrubsProportion := JSONObj.Get('shrubsProportion', 0);
    FShrubsDistribution := TStratumDistribution(JSONObj.Get('shrubsDistribution', Integer(disNone)));
    FShrubsHeight := JSONObj.Get('shrubsHeight', 0);
    FTreesProportion := JSONObj.Get('treesProportion', 0);
    FTreesDistribution := TStratumDistribution(JSONObj.Get('treesDistribution', Integer(disNone)));
    FTreesHeight := JSONObj.Get('treesHeight', 0);
    FNotes := JSONObj.Get('notes', '');
  end;
end;

procedure TMobileVegetation.ToVegetation(aVegetation: TVegetation);
begin
  aVegetation.SampleDate := FSampleTime;
  aVegetation.SampleTime := FSampleTime;
  aVegetation.Longitude := FLongitude;
  aVegetation.Latitude := FLatitude;
  aVegetation.HerbsProportion := FHerbsProportion;
  aVegetation.HerbsDistribution := FHerbsDistribution;
  aVegetation.HerbsAvgHeight := FHerbsHeight;
  aVegetation.ShrubsProportion := FShrubsProportion;
  aVegetation.ShrubsDistribution := FShrubsDistribution;
  aVegetation.ShrubsAvgHeight := FShrubsHeight;
  aVegetation.TreesProportion := FTreesProportion;
  aVegetation.TreesDistribution := FTreesDistribution;
  aVegetation.TreesAvgHeight := FTreesHeight;
  aVegetation.Notes := FNotes;
end;

{ TMobileWeather }

procedure TMobileWeather.Clear;
begin
  FId := 0;
  FInventoryId := EmptyStr;
  FSampleTime := NullDateTime;
  FCloudCover := 0;
  FPrecipitation := wpEmpty;
  FTemperature := 0;
  FWindSpeed := 0;
end;

procedure TMobileWeather.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FInventoryId := JSONObj.Get('inventoryId', '');
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    FCloudCover := JSONObj.Get('cloudCover', 0);
    FPrecipitation := TPrecipitation(JSONObj.Get('precipitation', Integer(wpNone)));
    FTemperature := JSONObj.Get('temperature', 0);
    FWindSpeed := JSONObj.Get('windSpeed', 0);
  end;
end;

procedure TMobileWeather.ToWeatherLog(aWeatherLog: TWeatherLog);
begin
  aWeatherLog.SampleDate := FSampleTime;
  aWeatherLog.SampleTime := FSampleTime;
  aWeatherLog.CloudCover := FCloudCover;
  aWeatherLog.Precipitation := FPrecipitation;
  aWeatherLog.Temperature := FTemperature;
  aWeatherLog.WindSpeedBft := FWindSpeed;
end;

{ TMobileInventory }

constructor TMobileInventory.Create;
begin
  FSpeciesList := TMobileSpeciesList.Create;
  FVegetationList := TMobileVegetationList.Create;
  FWeatherList := TMobileWeatherList.Create;
end;

procedure TMobileInventory.Clear;
begin
  FId := EmptyStr;
  FType := invQualitativeFree;
  FDuration := 0;
  FMaxSpecies := 0;
  FStartTime := NullDateTime;
  FEndTime := NullDateTime;
  FStartLongitude := 0.0;
  FStartLatitude := 0.0;
  FEndLongitude := 0.0;
  FEndLatitude := 0.0;
  FLocalityName := EmptyStr;
  FCurrentInterval := 0;
  FIntervalsWithoutNewSpecies := 0;
  FCurrentIntervalSpeciesCount := 0;
  FSurveyKey := 0;
  FObserver := EmptyStr;
  FListNumber := 0;
  FImport := False;

  FSpeciesList.Clear;
  FVegetationList.Clear;
  FWeatherList.Clear;
end;

destructor TMobileInventory.Destroy;
begin
  FWeatherList.Free;
  FVegetationList.Free;
  FSpeciesList.Free;
  inherited Destroy;
end;

procedure TMobileInventory.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
  SpeciesArray, VegetationArray, WeatherArray: TJSONArray;
  aListNr: String;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FImport := True;
    FId := JSONObj.Get('id', '');
    FType := TMobileInventoryType(JSONObj.Get('type', Integer(invQualitativeFree)));
    FDuration := JSONObj.Get('duration', 0);
    FMaxSpecies := JSONObj.Get('maxSpecies', 0);
    FStartTime := DartISO8601ToDate(JSONObj.Get('startTime', '1500-12-30T00:00:00'));
    FEndTime := DartISO8601ToDate(JSONObj.Get('endTime', '1500-12-30T00:00:00'));
    FStartLongitude := JSONObj.Get('startLongitude', 0.0);
    FStartLatitude := JSONObj.Get('startLatitude', 0.0);
    FEndLongitude := JSONObj.Get('endLongitude', 0.0);
    FEndLatitude := JSONObj.Get('endLatitude', 0.0);
    FLocalityName := JSONObj.Get('localityName', '');
    FCurrentInterval := JSONObj.Get('currentInterval', 0);
    FIntervalsWithoutNewSpecies := JSONObj.Get('intervalWithoutNewSpecies', 0);
    FCurrentIntervalSpeciesCount := JSONObj.Get('currentIntervalSpeciesCount', 0);

    if ExecRegExpr('^[A-Za-z]{2,}-[A-Za-z]{2,}-[0-9]{8}-[A-Z]{0,1}[0-9]{2,}$', FId) then
    begin
      // Get locality abbreviation
      FLocalityName := ExtractDelimited(1, FId, ['-']);
      // Get observer abbreviation
      FObserver := ExtractDelimited(2, FId, ['-']);
      // Get Mackinnon list number
      if FType = invMackinnonList then
      begin
        aListNr := ExtractDelimited(4, FId, ['-']);
        ReplaceStr(aListNr, 'L', '');
        FListNumber := StrToInt(aListNr);
      end;
    end;

    SpeciesArray := JSONObj.FindPath('speciesList') as TJSONArray;
    if Assigned(SpeciesArray) then
      LoadSpeciesList(SpeciesArray);
    VegetationArray := JSONObj.FindPath('vegetationList') as TJSONArray;
    if Assigned(VegetationArray) then
      LoadVegetationList(VegetationArray);
    WeatherArray := JSONObj.FindPath('weatherList') as TJSONArray;
    if Assigned(WeatherArray) then
      LoadWeatherList(WeatherArray);
  end;
end;

procedure TMobileInventory.LoadSpeciesList(JSON: TJSONArray);
var
  i: Integer;
  SpeciesObj: TMobileSpecies;
begin
  FSpeciesList.Clear;
  for i := 0 to JSON.Count - 1 do
  begin
    SpeciesObj := TMobileSpecies.Create;
    SpeciesObj.FromJSON(JSON.Items[i]);
    FSpeciesList.Add(SpeciesObj);
  end;
end;

procedure TMobileInventory.LoadVegetationList(JSON: TJSONArray);
var
  i: Integer;
  VegetationObj: TMobileVegetation;
begin
  FVegetationList.Clear;
  for i := 0 to JSON.Count - 1 do
  begin
    VegetationObj := TMobileVegetation.Create;
    VegetationObj.FromJSON(JSON.Items[i]);
    FVegetationList.Add(VegetationObj);
  end;
end;

procedure TMobileInventory.LoadWeatherList(JSON: TJSONArray);
var
  i: Integer;
  WeatherObj: TMobileWeather;
begin
  FWeatherList.Clear;
  for i := 0 to JSON.Count - 1 do
  begin
    WeatherObj := TMobileWeather.Create;
    WeatherObj.FromJSON(JSON.Items[i]);
    FWeatherList.Add(WeatherObj);
  end;
end;

procedure TMobileInventory.ToSurvey(aSurvey: TSurvey);
begin
  aSurvey.SampleId := FId;
  case FType of
    invQualitativeFree:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileQualitativeFree);
    invQualitativeTimed:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileQualitativeTimed);
    invQualitativeInterval:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileQualitativeInterval);
    invMackinnonList:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileMackinnonList);
    invTransectionCount:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileTransectionCount);
    invPointCount:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobilePointCount);
    invBanding:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileBanding);
    invCasual:
      aSurvey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileCasual);
  end;
  //aSurvey.Duration := FDuration;
  aSurvey.SurveyDate := FStartTime;
  aSurvey.StartTime := FStartTime;
  aSurvey.EndTime := FEndTime;
  aSurvey.StartLongitude := FStartLongitude;
  aSurvey.StartLatitude := FStartLatitude;
  aSurvey.EndLongitude := FEndLongitude;
  aSurvey.EndLatitude := FEndLatitude;
  aSurvey.LocalityId := GetSiteKey(FLocalityName);
end;

{ TMobileEgg }

procedure TMobileEgg.Clear;
begin
  FId := 0;
  FNestId := 0;
  FFieldNumber := EmptyStr;
  FSampleTime := NullDateTime;
  FEggShape := esUnknown;
  FWidth := 0.0;
  FLength := 0.0;
  FMass := 0.0;
  FSpeciesName := EmptyStr;
end;

procedure TMobileEgg.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FNestId := JSONObj.Get('nestId', 0);
    FFieldNumber := JSONObj.Get('fieldNumber', '');
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    FEggShape := TEggShape(JSONObj.Get('cloudCover', Integer(esUnknown)));
    FWidth := JSONObj.Get('width', 0.0);
    FLength := JSONObj.Get('length', 0.0);
    FMass := JSONObj.Get('mass', 0.0);
    FSpeciesName := JSONObj.Get('speciesName', '');
  end;
end;

procedure TMobileEgg.ToEgg(aEgg: TEgg);
begin
  aEgg.FieldNumber := FFieldNumber;
  aEgg.MeasureDate := FSampleTime;
  aEgg.EggShape := FEggShape;
  aEgg.Width := FWidth;
  aEgg.Length := FLength;
  aEgg.Mass := FMass;
  aEgg.TaxonId := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, FSpeciesName);
end;

{ TMobileNestRevision }

procedure TMobileNestRevision.Clear;
begin
  FId := 0;
  FNestId := 0;
  FSampleTime := NullDateTime;
  FNestStatus := nstUnknown;
  FNestStage := nsgUnknown;
  FEggsHost := 0;
  FNestlingsHost := 0;
  FEggsParasite := 0;
  FNestlingsParasite := 0;
  FHasPhilornisLarvae := False;
  FNotes := EmptyStr;
end;

procedure TMobileNestRevision.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FNestId := JSONObj.Get('nestId', 0);
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    FNestStatus := TNestStatus(JSONObj.Get('nestStatus', Integer(nstUnknown)));
    FNestStage := TNestStage(JSONObj.Get('nestStage', Integer(nsgUnknown)));
    FEggsHost := JSONObj.Get('eggsHost', 0);
    FNestlingsHost := JSONObj.Get('nestlingsHost', 0);
    FEggsParasite := JSONObj.Get('eggsParasite', 0);
    FNestlingsParasite := JSONObj.Get('nestlingsParasite', 0);
    FHasPhilornisLarvae := JSONObj.Get('hasPhilornisLarvae', 0) = 1;
    FNotes := JSONObj.Get('notes', '');
  end;
end;

procedure TMobileNestRevision.ToNestRevision(aRevision: TNestRevision);
begin
  aRevision.RevisionDate := FSampleTime;
  aRevision.RevisionTime := FSampleTime;
  aRevision.NestStatus := FNestStatus;
  aRevision.NestStage := FNestStage;
  aRevision.HostEggsTally := FEggsHost;
  aRevision.HostNestlingsTally := FNestlingsHost;
  aRevision.NidoparasiteEggsTally := FEggsParasite;
  aRevision.NidoparasiteNestlingsTally := FNestlingsParasite;
  aRevision.HavePhilornisLarvae := FHasPhilornisLarvae;
  aRevision.Notes := FNotes;
end;

{ TMobileNest }

constructor TMobileNest.Create;
begin
  FRevisionList := TMobileRevisionList.Create;
  FEggList := TMobileEggList.Create;
end;

procedure TMobileNest.Clear;
begin
  FId := 0;
  FFieldNumber := EmptyStr;
  FSpeciesName := EmptyStr;
  FLocalityName := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FSupport := EmptyStr;
  FHeightAboveGround := 0;
  FFoundTime := NullDateTime;
  FLastTime := NullDateTime;
  FNestFate := nfUnknown;
  FMale := EmptyStr;
  FFemale := EmptyStr;
  FHelpers := EmptyStr;
  FIsActive := False;
  FObserver := EmptyStr;
  FNestKey := 0;
end;

destructor TMobileNest.Destroy;
begin
  FEggList.Free;
  FRevisionList.Free;
  inherited Destroy;
end;

procedure TMobileNest.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
  RevisionArray, EggArray: TJSONArray;
  RE: TRegExpr;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FFieldNumber := JSONObj.Get('fieldNumber', '');
    FSpeciesName := JSONObj.Get('speciesName', '');
    FLocalityName := JSONObj.Get('localityName', '');
    FLongitude := JSONObj.Get('longitude', 0.0);
    FLatitude := JSONObj.Get('latitude', 0.0);
    FSupport := JSONObj.Get('support', '');
    FHeightAboveGround := JSONObj.Get('heightAboveGround', 0.0);
    FFoundTime := DartISO8601ToDate(JSONObj.Get('foundTime', '1500-12-30T00:00:00'));
    FLastTime := DartISO8601ToDate(JSONObj.Get('lastTime', '1500-12-30T00:00:00'));
    FNestFate := TNestFate(JSONObj.Get('nestFate', Integer(nfUnknown)));
    FMale := JSONObj.Get('male', '');
    FFemale := JSONObj.Get('female', '');
    FHelpers := JSONObj.Get('helpers', '');
    FIsActive := JSONObj.Get('isActive', 0) = 1;

    if ExecRegExpr('^[A-Za-z]*[0-9]*$', FFieldNumber) then
    begin
      RE := TRegExpr.Create;
      try
        RE.Expression := '^([A-Za-z]+)';
        if RE.Exec(FFieldNumber) then
          FObserver := RE.Match[1]
        else
          FObserver := '';
      finally
        RE.Free;
      end;
    end;

    RevisionArray := JSONObj.FindPath('revisionsList') as TJSONArray;
    if Assigned(RevisionArray) then
      LoadRevisionList(RevisionArray);
    EggArray := JSONObj.FindPath('eggsList') as TJSONArray;
    if Assigned(EggArray) then
      LoadEggList(EggArray);
  end;
end;

procedure TMobileNest.LoadEggList(JSON: TJSONArray);
var
  i: Integer;
  EggObj: TMobileEgg;
begin
  FEggList.Clear;
  for i := 0 to JSON.Count - 1 do
  begin
    EggObj := TMobileEgg.Create;
    EggObj.FromJSON(JSON.Items[i]);
    FEggList.Add(EggObj);
  end;
end;

procedure TMobileNest.LoadRevisionList(JSON: TJSONArray);
var
  i: Integer;
  RevisionObj: TMobileNestRevision;
begin
  FRevisionList.Clear;
  for i := 0 to JSON.Count - 1 do
  begin
    RevisionObj := TMobileNestRevision.Create;
    RevisionObj.FromJSON(JSON.Items[i]);
    FRevisionList.Add(RevisionObj);
  end;
end;

procedure TMobileNest.ToNest(aNest: TNest);
begin
  aNest.FieldNumber := FFieldNumber;
  aNest.TaxonId := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, FSpeciesName);
  aNest.LocalityId := GetSiteKey(FLocalityName);
  aNest.Longitude := FLongitude;
  aNest.Latitude := FLatitude;
  aNest.SupportPlant1Id := GetKey('botanic_taxa', COL_TAXON_ID, COL_TAXON_NAME, FSupport);
  aNest.HeightAboveGround := FHeightAboveGround;
  aNest.FoundDate := FFoundTime;
  aNest.LastDate := FLastTime;
  aNest.NestFate := FNestFate;
end;

{ TMobileSpecimen }

procedure TMobileSpecimen.Clear;
begin
  FId := 0;
  FFieldNumber := EmptyStr;
  FSampleTime := NullDateTime;
  FType := sptEmpty;
  FSpeciesName := EmptyStr;
  FLocality := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FNotes := EmptyStr;
  FIsPending := False;
  FObserver := EmptyStr;
  FSpecimenKey := 0;
end;

procedure TMobileSpecimen.FromJSON(JSON: TJSONData);
var
  JSONObj: TJSONObject;
  RE: TRegExpr;
begin
  if JSON is TJSONObject then
  begin
    JSONObj := TJSONObject(JSON);
    FId := JSONObj.Get('id', 0);
    FFieldNumber := JSONObj.Get('fieldNumber', '');
    FSampleTime := DartISO8601ToDate(JSONObj.Get('sampleTime', '1500-12-30T00:00:00'));
    FType := TSpecimenType(JSONObj.Get('type', Integer(sptEmpty)));
    FSpeciesName := JSONObj.Get('speciesName', '');
    FLocality := JSONObj.Get('locality', '');
    FLongitude := JSONObj.Get('longitude', 0.0);
    FLatitude := JSONObj.Get('latitude', 0.0);
    FNotes := JSONObj.Get('notes', '');
    FIsPending := JSONObj.Get('isPending', 0) = 1;

    if ExecRegExpr('^[A-Za-z]*[0-9]*$', FFieldNumber) then
    begin
      RE := TRegExpr.Create;
      try
        RE.Expression := '^([A-Za-z]+)';
        if RE.Exec(FFieldNumber) then
          FObserver := RE.Match[1]
        else
          FObserver := '';
      finally
        RE.Free;
      end;
    end;
  end;
end;

procedure TMobileSpecimen.ToSpecimen(aSpecimen: TSpecimen);
var
  y, m, d: Word;
begin
  aSpecimen.FieldNumber := FFieldNumber;
  DecodeDate(FSampleTime, y, m, d);
  aSpecimen.CollectionYear := y;
  aSpecimen.CollectionMonth := m;
  aSpecimen.CollectionDay := d;
  aSpecimen.SampleType := FType;
  aSpecimen.Longitude := FLongitude;
  aSpecimen.Latitude := FLatitude;
  aSpecimen.TaxonId := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, FSpeciesName);
  aSpecimen.LocalityId := GetSiteKey(FLocality);
  aSpecimen.Notes := FNotes;
end;

end.

