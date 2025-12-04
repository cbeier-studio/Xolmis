{ Xolmis Bird Data library

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

unit models_birds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

type

  { TIndividual }

  TIndividual = class(TXolmisRecord)
  protected
    FFullName: String;
    FTaxonId: Integer;
    FSex: TSex;
    FAge: TAge;
    FNestId: Integer;
    FBirthDate: String;
    FBirthDay: Integer;
    FBirthMonth: Integer;
    FBirthYear: Integer;
    FBandingDate: TDate;
    FBandChangeDate: TDate;
    FBandId: Integer;
    FBandName: String;
    FDoubleBandId: Integer;
    FRemovedBandId: Integer;
    FRightLegBelow: String;
    FLeftLegBelow: String;
    FRightLegAbove: String;
    FLeftLegAbove: String;
    FFatherId: Integer;
    FMotherId: Integer;
    FDeathDate: String;
    FDeathDay: Integer;
    FDeathMonth: Integer;
    FDeathYear: Integer;
    FRecognizableMarkings: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TIndividual; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TIndividual): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property Sex: TSex read FSex write FSex;
    property Age: TAge read FAge write FAge;
    property NestId: Integer read FNestId write FNestId;
    property BirthDate: String read FBirthDate write FBirthDate;
    property BirthDay: Integer read FBirthDay write FBirthDay;
    property BirthMonth: Integer read FBirthMonth write FBirthMonth;
    property BirthYear: Integer read FBirthYear write FBirthYear;
    property BandingDate: TDate read FBandingDate write FBandingDate;
    property BandChangeDate: TDate read FBandChangeDate write FBandChangeDate;
    property BandId: Integer read FBandId write FBandId;
    property BandName: String read FBandName write FBandName;
    property DoubleBandId: Integer read FDoubleBandId write FDoubleBandId;
    property RemovedBandId: Integer read FRemovedBandId write FRemovedBandId;
    property RightLegBelow: String read FRightLegBelow write FRightLegBelow;
    property LeftLegBelow: String read FLeftLegBelow write FLeftLegBelow;
    property RightLegAbove: String read FRightLegAbove write FRightLegAbove;
    property LeftLegAbove: String read FLeftLegAbove write FLeftLegAbove;
    property FatherId: Integer read FFatherId write FFatherId;
    property MotherId: Integer read FMotherId write FMotherId;
    property DeathDate: String read FDeathDate write FDeathDate;
    property DeathDay: Integer read FDeathDay write FDeathDay;
    property DeathMonth: Integer read FDeathMonth write FDeathMonth;
    property DeathYear: Integer read FDeathYear write FDeathYear;
    property RecognizableMarkings: String read FRecognizableMarkings write FRecognizableMarkings;
    property Notes: String read FNotes write FNotes;
  end;

  { TIndividualRepository }

  TIndividualRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByBand(const aTaxon, aBand: Integer; aRightLeg: String = ''; aLeftLeg: String = ''; E: TIndividual = nil);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TCapture }

  TCapture = class(TXolmisRecord)
  protected
    FFullName: String;
    FSurveyId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FProjectId: Integer;
    FCaptureDate: TDate;
    FCaptureTime: TTime;
    FLocalityId: Integer;
    FNetStationId: Integer;
    FNetId: Integer;
    FLongitude: Extended;
    FLatitude: Extended;
    FBanderId: Integer;
    FAnnotatorId: Integer;
    FSubjectStatus: TSubjectStatus;
    FCaptureType: TCaptureType;
    FSubjectSex: TSex;
    FHowSexed: String;
    FBandId: Integer;
    FRemovedBandId: Integer;
    FRightLegBelow: String;
    FLeftLegBelow: String;
    FRightLegAbove: String;
    FLeftLegAbove: String;
    FWeight: Double;
    FTarsusLength: Double;
    FTarsusDiameter: Double;
    FCulmenLength: Double;
    FExposedCulmen: Double;
    FBillWidth: Double;
    FBillHeight: Double;
    FNostrilBillTip: Double;
    FSkullLength: Double;
    FHaluxLengthTotal: Double;
    FHaluxLengthFinger: Double;
    FHaluxLengthClaw: Double;
    FRightWingChord: Double;
    FFirstSecondaryChord: Double;
    FTailLength: Double;
    FCentralRetrixLength: Double;
    FExternalRetrixLength: Double;
    FTotalLength: Double;
    FFeatherMites: String;
    FFat: String;
    FBroodPatch: String;
    FCloacalProtuberance: String;
    //FOldMolt: String;
    //FOldPrimariesMolt: String;
    //FOldSecondariesMolt: String;
    //FOldRetricesMolt: String;
    //FOldBodyMolt: String;
    FBodyMolt: String;
    FFlightFeathersMolt: String;
    FFlightFeathersWear: String;
    FMoltLimits: String;
    FCycleCode: String;
    FSubjectAge: TAge;
    FHowAged: String;
    FSkullOssification: String;
    FKippsIndex: Double;
    FGlucose: Double;
    FHemoglobin: Double;
    FHematocrit: Double;
    FPhilornisLarvaeTally: Integer;
    FBloodSample: Boolean;
    FFeatherSample: Boolean;
    FClawSample: Boolean;
    FFecesSample: Boolean;
    FParasiteSample: Boolean;
    FSubjectRecorded: Boolean;
    FSubjectCollected: Boolean;
    FSubjectPhotographed: Boolean;
    FFieldNumber: String;
    FPhotographer1Id: Integer;
    FPhotographer2Id: Integer;
    FStartPhotoNumber: String;
    FEndPhotoNumber: String;
    FCameraName: String;
    FEscaped: Boolean;
    FNeedsReview: Boolean;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TCapture; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TCapture): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property CaptureDate: TDate read FCaptureDate write FCaptureDate;
    property CaptureTime: TTime read FCaptureTime write FCaptureTime;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property NetStationId: Integer read FNetStationId write FNetStationId;
    property NetId: Integer read FNetId write FNetId;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property BanderId: Integer read FBanderId write FBanderId;
    property AnnotatorId: Integer read FAnnotatorId write FAnnotatorId;
    property SubjectStatus: TSubjectStatus read FSubjectStatus write FSubjectStatus;
    property CaptureType: TCaptureType read FCaptureType write FCaptureType;
    property SubjectSex: TSex read FSubjectSex write FSubjectSex;
    property HowSexed: String read FHowSexed write FHowSexed;
    property BandId: Integer read FBandId write FBandId;
    property RemovedBandId: Integer read FRemovedBandId write FRemovedBandId;
    property RightLegBelow: String read FRightLegBelow write FRightLegBelow;
    property LeftLegBelow: String read FLeftLegBelow write FLeftLegBelow;
    property RightLegAbove: String read FRightLegAbove write FRightLegAbove;
    property LeftLegAbove: String read FLeftLegAbove write FLeftLegAbove;
    property Weight: Double read FWeight write FWeight;
    property TarsusLength: Double read FTarsusLength write FTarsusLength;
    property TarsusDiameter: Double read FTarsusDiameter write FTarsusDiameter;
    property CulmenLength: Double read FCulmenLength write FCulmenLength;
    property ExposedCulmen: Double read FExposedCulmen write FExposedCulmen;
    property BillWidth: Double read FBillWidth write FBillWidth;
    property BillHeight: Double read FBillHeight write FBillHeight;
    property NostrilBillTip: Double read FNostrilBillTip write FNostrilBillTip;
    property SkullLength: Double read FSkullLength write FSkullLength;
    property HaluxLengthTotal: Double read FHaluxLengthTotal write FHaluxLengthTotal;
    property HaluxLengthFinger: Double read FHaluxLengthFinger write FHaluxLengthFinger;
    property HaluxLengthClaw: Double read FHaluxLengthClaw write FHaluxLengthClaw;
    property RightWingChord: Double read FRightWingChord write FRightWingChord;
    property FirstSecondaryChord: Double read FFirstSecondaryChord write FFirstSecondaryChord;
    property TailLength: Double read FTailLength write FTailLength;
    property CentralRetrixLength: Double read FCentralRetrixLength write FCentralRetrixLength;
    property ExternalRetrixLength: Double read FExternalRetrixLength write FExternalRetrixLength;
    property TotalLength: Double read FTotalLength write FTotalLength;
    property FeatherMites: String read FFeatherMites write FFeatherMites;
    property Fat: String read FFat write FFat;
    property BroodPatch: String read FBroodPatch write FBroodPatch;
    property CloacalProtuberance: String read FCloacalProtuberance write FCloacalProtuberance;
    //property OldMolt: String read FOldMolt write FoldMolt;
    //property OldPrimariesMolt: String read FOldPrimariesMolt write FOldPrimariesMolt;
    //property OldSecondariesMolt: String read FOldSecondariesMolt write FOldSecondariesMolt;
    //property OldRetricesMolt: String read FOldRetricesMolt write FOldRetricesMolt;
    //property OldBodyMolt: String read FOldBodyMolt write FOldBodyMolt;
    property BodyMolt: String read FBodyMolt write FBodyMolt;
    property FlightFeathersMolt: String read FFlightFeathersMolt write FFlightFeathersMolt;
    property FlightFeathersWear: String read FFlightFeathersWear write FFlightFeathersWear;
    property MoltLimits: String read FMoltLimits write FMoltLimits;
    property CycleCode: String read FCycleCode write FCycleCode;
    property SubjectAge: TAge read FSubjectAge write FSubjectAge;
    property HowAged: String read FHowAged write FHowAged;
    property SkullOssification: String read FSkullOssification write FSkullOssification;
    property KippsIndex: Double read FKippsIndex write FKippsIndex;
    property Glucose: Double read FGlucose write FGlucose;
    property Hemoglobin: Double read FHemoglobin write FHemoglobin;
    property Hematocrit: Double read FHematocrit write FHematocrit;
    property PhilornisLarvaeTally: Integer read FPhilornisLarvaeTally write FPhilornisLarvaeTally;
    property BloodSample: Boolean read FBloodSample write FBloodSample;
    property FeatherSample: Boolean read FFeatherSample write FFeatherSample;
    property ClawSample: Boolean read FClawSample write FClawSample;
    property FecesSample: Boolean read FFecesSample write FFecesSample;
    property ParasiteSample: Boolean read FParasiteSample write FParasiteSample;
    property SubjectRecorded: Boolean read FSubjectRecorded write FSubjectRecorded;
    property SubjectCollected: Boolean read FSubjectCollected write FSubjectCollected;
    property SubjectPhotographed: Boolean read FSubjectPhotographed write FSubjectPhotographed;
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property Photographer1Id: Integer read FPhotographer1Id write FPhotographer1Id;
    property Photographer2Id: Integer read FPhotographer2Id write FPhotographer2Id;
    property StartPhotoNumber: String read FStartPhotoNumber write FStartPhotoNumber;
    property EndPhotoNumber: String read FEndPhotoNumber write FEndPhotoNumber;
    property CameraName: String read FCameraName write FCameraName;
    property Escaped: Boolean read FEscaped write FEscaped;
    property NeedsReview: Boolean read FNeedsReview write FNeedsReview;
    property Notes: String read FNotes write FNotes;
  end;

  { TCaptureRepository }

  TCaptureRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByBand(const aTaxon, aBand: Integer; aCaptureType, aDate, aTime: String; E: TCapture);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TFeather }

  TFeather = class(TXolmisRecord)
  protected
    FSampleDate: TDate;
    FSampleTime: TTime;
    FTaxonId: Integer;
    FLocalityId: Integer;
    FIndividualId: Integer;
    FCaptureId: Integer;
    FSightingId: Integer;
    FObserverId: Integer;
    FSourceType: TFeatherDataSource;
    FSymmetrical: TSymmetry;
    FFeatherTrait: TFeatherTrait;
    FFeatherNumber: Integer;
    FBodySide: TBodySide;
    FPercentGrown: Double;
    FFeatherLength: Double;
    FFeatherArea: Double;
    FFeatherMass: Double;
    FRachisWidth: Double;
    FGrowthBarWidth: Double;
    FBarbDensity: Double;
    FFeatherAge: TFeatherAge;
    FFullName: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TFeather; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TFeather): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property SampleTime: TTime read FSampleTime write FSampleTime;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SightingId: Integer read FSightingId write FSightingId;
    property ObserverId: Integer read FObserverId write FObserverId;
    property SourceType: TFeatherDataSource read FSourceType write FSourceType;
    property Symmetrical: TSymmetry read FSymmetrical write FSymmetrical;
    property FeatherTrait: TFeatherTrait read FFeatherTrait write FFeatherTrait;
    property FeatherNumber: Integer read FFeatherNumber write FFeatherNumber;
    property BodySide: TBodySide read FBodySide write FBodySide;
    property PercentGrown: Double read FPercentGrown write FPercentGrown;
    property FeatherLength: Double read FFeatherLength write FFeatherLength;
    property FeatherArea: Double read FFeatherArea write FFeatherArea;
    property FeatherMass: Double read FFeatherMass write FFeatherMass;
    property RachisWidth: Double read FRachisWidth write FRachisWidth;
    property GrowthBarWidth: Double read FGrowthBarWidth write FGrowthBarWidth;
    property BarbDensity: Double read FBarbDensity write FBarbDensity;
    property FeatherAge: TFeatherAge read FFeatherAge write FFeatherAge;
    property FullName: String read FFullName write FFullName;
    property Notes: String read FNotes write FNotes;
  end;

  { TFeatherRepository }

  TFeatherRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  utils_system, utils_global, models_users, utils_validations, utils_fullnames,
  data_columns, data_setparam, data_getvalue, data_consts,
  utils_locale, udm_main;

{ TFeather }

constructor TFeather.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TFeather.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TFeather then
  begin
    FSampleDate := TFeather(Source).SampleDate;
    FSampleTime := TFeather(Source).SampleTime;
    FTaxonId := TFeather(Source).TaxonId;
    FLocalityId := TFeather(Source).LocalityId;
    FIndividualId := TFeather(Source).IndividualId;
    FCaptureId := TFeather(Source).CaptureId;
    FSightingId := TFeather(Source).SightingId;
    FObserverId := TFeather(Source).ObserverId;
    FSourceType := TFeather(Source).SourceType;
    FSymmetrical := TFeather(Source).Symmetrical;
    FFeatherTrait := TFeather(Source).FeatherTrait;
    FFeatherNumber := TFeather(Source).FeatherNumber;
    FBodySide := TFeather(Source).BodySide;
    FPercentGrown := TFeather(Source).PercentGrown;
    FFeatherLength := TFeather(Source).FeatherLength;
    FFeatherArea := TFeather(Source).FeatherArea;
    FFeatherMass := TFeather(Source).FeatherMass;
    FRachisWidth := TFeather(Source).RachisWidth;
    FGrowthBarWidth := TFeather(Source).GrowthBarWidth;
    FBarbDensity := TFeather(Source).BarbDensity;
    FFeatherAge := TFeather(Source).FeatherAge;
    FNotes := TFeather(Source).Notes;
  end;
end;

procedure TFeather.Clear;
begin
  inherited Clear;
  FSampleDate := NullDate;
  FSampleTime := NullTime;
  FTaxonId := 0;
  FLocalityId := 0;
  FIndividualId := 0;
  FCaptureId := 0;
  FSightingId := 0;
  FObserverId := 0;
  FSourceType := fdsUnknown;
  FSymmetrical := symUnknown;
  FFeatherTrait := ftrBody;
  FFeatherNumber := 0;
  FBodySide := bsdNotApplicable;
  FPercentGrown := 0.0;
  FFeatherLength := 0.0;
  FFeatherArea := 0.0;
  FFeatherMass := 0.0;
  FRachisWidth := 0.0;
  FGrowthBarWidth := 0.0;
  FBarbDensity := 0.0;
  FFeatherAge := fageUnknown;
  FFullName := EmptyStr;
  FNotes := EmptyStr;
end;

function TFeather.Clone: TXolmisRecord;
begin
  Result := TFeather(inherited Clone);
end;

function TFeather.Diff(const aOld: TFeather; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSource, aOld.SourceType, FSourceType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSymmetry, aOld.Symmetrical, FSymmetrical, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFeatherTrait, aOld.FeatherTrait, FFeatherTrait, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFeatherNumber, aOld.FeatherNumber, FFeatherNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBodySide, aOld.BodySide, FBodySide, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPercentGrown, aOld.PercentGrown, FPercentGrown, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLength, aOld.FeatherLength, FFeatherLength, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscArea, aOld.FeatherArea, FFeatherArea, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMass, aOld.FeatherMass, FFeatherMass, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRachisWidth, aOld.RachisWidth, FRachisWidth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscGrowthBarWidth, aOld.GrowthBarWidth, FGrowthBarWidth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBarbDensity, aOld.BarbDensity, FBarbDensity, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAge, aOld.FeatherAge, FFeatherAge, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TFeather.EqualsTo(const Other: TFeather): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TFeather.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSampleDate   := Obj.Get('sample_date', NullDate);
    FSampleTime   := Obj.Get('sample_time', NullTime);
    FTaxonId      := Obj.Get('taxon_id', 0);
    FLocalityId   := Obj.Get('locality_id', 0);
    FIndividualId := Obj.Get('individual_id', 0);
    FCaptureId    := Obj.Get('capture_id', 0);
    FSightingId   := Obj.Get('sighting_id', 0);
    FObserverId   := Obj.Get('observer_id', 0);
    case Obj.Get('feather_source', '') of
      'U': FSourceType := fdsUnknown;
      'C': FSourceType := fdsCapture;
      'S': FSourceType := fdsSighting;
      'P': FSourceType := fdsPhoto;
    else
      FSourceType := fdsUnknown;
    end;
    case Obj.Get('symmetry', '') of
      'U': FSymmetrical := symUnknown;
      'S': FSymmetrical := symSymmetrical;
      'A': FSymmetrical := symAsymmetrical;
    else
      FSymmetrical := symUnknown;
    end;
    case Obj.Get('feather_trait', '') of
      'B':  FFeatherTrait := ftrBody;
      'P':  FFeatherTrait := ftrPrimary;
      'S':  FFeatherTrait := ftrSecondary;
      'R':  FFeatherTrait := ftrRectrix;
      'PC': FFeatherTrait := ftrPrimaryCovert;
      'GC': FFeatherTrait := ftrGreatCovert;
      'MC': FFeatherTrait := ftrMedianCovert;
      'LC': FFeatherTrait := ftrLesserCovert;
      'CC': FFeatherTrait := ftrCarpalCovert;
      'AL': FFeatherTrait := ftrAlula;
    end;
    FFeatherNumber       := Obj.Get('feather_number', 0);
    case Obj.Get('body_side', '') of
      'NA': FBodySide := bsdNotApplicable;
      'R':  FBodySide := bsdRight;
      'L':  FBodySide := bsdLeft;
    else
      FBodySide := bsdNotApplicable;
    end;
    FPercentGrown   := Obj.Get('percent_grown', 0.0);
    FFeatherLength  := Obj.Get('feather_length', 0.0);
    FFeatherArea    := Obj.Get('feather_area', 0.0);
    FFeatherMass    := Obj.Get('feather_mass', 0.0);
    FRachisWidth    := Obj.Get('rachis_width', 0.0);
    FGrowthBarWidth := Obj.Get('growth_bar_width', 0.0);
    FBarbDensity    := Obj.Get('barb_density', 0.0);
    case Obj.Get('feather_age', '') of
      'U': FFeatherAge := fageUnknown;
      'N': FFeatherAge := fageNestling;
      'F': FFeatherAge := fageFledgling;
      'A': FFeatherAge := fageAdult;
      'Y': FFeatherAge := fageFirstYear;
      'S': FFeatherAge := fageSecondYear;
      'T': FFeatherAge := fageThirdYear;
      '4': FFeatherAge := fageFourthYear;
      '5': FFeatherAge := fageFifthYear;
    else
      FFeatherAge := fageUnknown;
    end;
    FFullName     := Obj.Get('full_name', '');
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TFeather.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('sample_date', FSampleDate);
    JSONObject.Add('sample_time', FSampleTime);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('capture_id', FCaptureId);
    JSONObject.Add('sighting_id', FSightingId);
    JSONObject.Add('observer_id', FObserverId);
    JSONObject.Add('feather_source', FEATHER_DATA_SOURCES[FSourceType]);
    JSONObject.Add('symmetry', SYMMETRIES[FSymmetrical]);
    JSONObject.Add('feather_trait', FEATHER_TRAITS[FFeatherTrait]);
    JSONObject.Add('feather_number', FFeatherNumber);
    JSONObject.Add('body_side', BODY_SIDES[FBodySide]);
    JSONObject.Add('percent_grown', FPercentGrown);
    JSONObject.Add('feather_length', FFeatherLength);
    JSONObject.Add('feather_area', FFeatherArea);
    JSONObject.Add('feather_mass', FFeatherMass);
    JSONObject.Add('rachis_width', FRachisWidth);
    JSONObject.Add('growth_bar_width', FGrowthBarWidth);
    JSONObject.Add('barb_density', FBarbDensity);
    JSONObject.Add('feather_age', FEATHER_AGES[FFeatherAge]);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TFeather.ToString: String;
begin
  Result := Format('Feather(Id=%d, SampleDate=%s, SampleTime=%s, TaxonId=%d, LocalityId=%d, IndividualId=%d, ' +
    'CaptureId=%d, SightingId=%d, ObserverId=%d, SourceType=%s, Symmetry=%s, FeatherTrait=%s, FeatherNumber=%d, ' +
    'BodySide=%s, PercentGrown=%f, FeatherLength=%f, FeatherArea=%f, FeatherMass=%f, RachisWidth=%f, ' +
    'GrowthBarWidth=%f, BarbDensity=%f, FeatherAge=%s, FullName=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, DateToStr(FSampleDate), TimeToStr(FSampleTime), FTaxonId, FLocalityId, FIndividualId, FCaptureId,
    FSightingId, FObserverId, FEATHER_DATA_SOURCES[FSourceType], SYMMETRIES[FSymmetrical], FEATHER_TRAITS[FFeatherTrait],
    FFeatherNumber, BODY_SIDES[FBodySide], FPercentGrown, FFeatherLength, FFeatherArea, FFeatherMass,
    FRachisWidth, FGrowthBarWidth, FBarbDensity, FEATHER_AGES[FFeatherAge], FFullName, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TFeather.Validate(out Msg: string): Boolean;
begin
  if FSampleDate = NullDate then
  begin
    Msg := 'Sample date required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TFeatherRepository }

procedure TFeatherRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TFeather;
begin
  if not (E is TFeather) then
    raise Exception.Create('Delete: Expected TFeather');

  R := TFeather(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TFeatherRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_FEATHER_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TFeatherRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_FEATHER_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TFeatherRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_FEATHER_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TFeather) then
    raise Exception.Create('FindBy: Expected TFeather');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT ' +
      'feather_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'taxon_id, ' +
      'locality_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'observer_id, ' +
      'source_type, ' +
      'symmetrical, ' +
      'feather_trait, ' +
      'feather_number, ' +
      'body_side, ' +
      'grown_percent, ' +
      'feather_length, ' +
      'feather_area, ' +
      'feather_mass, ' +
      'rachis_width, ' +
      'growth_bar_width, ' +
      'barb_density, ' +
      'feather_age, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM feathers');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TFeather(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TFeatherRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TFeather) then
    raise Exception.Create('GetById: Expected TFeather');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'feather_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'taxon_id, ' +
      'locality_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'observer_id, ' +
      'source_type, ' +
      'symmetrical, ' +
      'feather_trait, ' +
      'feather_number, ' +
      'body_side, ' +
      'grown_percent, ' +
      'feather_length, ' +
      'feather_area, ' +
      'feather_mass, ' +
      'rachis_width, ' +
      'growth_bar_width, ' +
      'barb_density, ' +
      'feather_age, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM feathers');
    Add('WHERE feather_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TFeather(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TFeatherRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TFeather;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TFeather) then
    raise Exception.Create('Hydrate: Expected TFeather');

  R := TFeather(E);
  with aDataSet do
  begin
    R.Id := FieldByName('feather_id').AsInteger;
    R.SampleDate := FieldByName('sample_date').AsDateTime;
    R.SampleTime := FieldByName('sample_time').AsDateTime;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.CaptureId := FieldByName('capture_id').AsInteger;
    R.SightingId := FieldByName('sighting_id').AsInteger;
    R.ObserverId := FieldByName('observer_id').AsInteger;
    case FieldByName('source_type').AsString of
      'U': R.SourceType := fdsUnknown;
      'C': R.SourceType := fdsCapture;
      'S': R.SourceType := fdsSighting;
      'P': R.SourceType := fdsPhoto;
    else
      R.SourceType := fdsUnknown;
    end;
    case FieldByName('symmetrical').AsString of
      'U': R.Symmetrical := symUnknown;
      'S': R.Symmetrical := symSymmetrical;
      'A': R.Symmetrical := symAsymmetrical;
    else
      R.Symmetrical := symUnknown;
    end;
    case FieldByName('feather_trait').AsString of
      'B': R.FeatherTrait := ftrBody;
      'P': R.FeatherTrait := ftrPrimary;
      'S': R.FeatherTrait := ftrSecondary;
      'R': R.FeatherTrait := ftrRectrix;
      'PC': R.FeatherTrait := ftrPrimaryCovert;
      'GC': R.FeatherTrait := ftrGreatCovert;
      'MC': R.FeatherTrait := ftrMedianCovert;
      'LC': R.FeatherTrait := ftrLesserCovert;
      'CC': R.FeatherTrait := ftrCarpalCovert;
      'AL': R.FeatherTrait := ftrAlula;
    end;
    R.FeatherNumber := FieldByName('feather_number').AsInteger;
    case FieldByName('body_side').AsString of
      'NA': R.BodySide := bsdNotApplicable;
      'R': R.BodySide := bsdRight;
      'L': R.BodySide := bsdLeft;
    else
      R.BodySide := bsdNotApplicable;
    end;
    R.PercentGrown := FieldByName('grown_percent').AsFloat;
    R.FeatherLength := FieldByName('feather_length').AsFloat;
    R.FeatherArea := FieldByName('feather_area').AsFloat;
    R.FeatherMass := FieldByName('feather_mass').AsFloat;
    R.RachisWidth := FieldByName('rachis_width').AsFloat;
    R.GrowthBarWidth := FieldByName('growth_bar_width').AsFloat;
    R.BarbDensity := FieldByName('barb_density').AsFloat;
    case FieldByName('feather_age').AsString of
      'U': R.FeatherAge := fageUnknown;
      'N': R.FeatherAge := fageNestling;
      'F': R.FeatherAge := fageFledgling;
      'A': R.FeatherAge := fageAdult;
      'Y': R.FeatherAge := fageFirstYear;
      'S': R.FeatherAge := fageSecondYear;
      'T': R.FeatherAge := fageThirdYear;
      '4': R.FeatherAge := fageFourthYear;
      '5': R.FeatherAge := fageFifthYear;
    else
      R.FeatherAge := fageUnknown;
    end;
    R.FullName := FieldByName('full_name').AsString;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TFeatherRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TFeather;
begin
  if not (E is TFeather) then
    raise Exception.Create('Insert: Expected TFeather');

  R := TFeather(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO feathers (' +
      'sample_date, ' +
      'sample_time, ' +
      'taxon_id, ' +
      'locality_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sighting_id, ' +
      'observer_id, ' +
      'source_type, ' +
      'symmetrical, ' +
      'feather_trait, ' +
      'feather_number, ' +
      'body_side, ' +
      'grown_percent, ' +
      'feather_length, ' +
      'feather_area, ' +
      'feather_mass, ' +
      'rachis_width, ' +
      'growth_bar_width, ' +
      'barb_density, ' +
      'feather_age, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':taxon_id, ' +
      ':locality_id, ' +
      ':individual_id, ' +
      ':capture_id, ' +
      ':sighting_id, ' +
      ':observer_id, ' +
      ':source_type, ' +
      ':symmetrical, ' +
      ':feather_trait, ' +
      ':feather_number, ' +
      ':body_side, ' +
      ':grown_percent, ' +
      ':feather_length, ' +
      ':feather_area, ' +
      ':feather_mass, ' +
      ':rachis_width, ' +
      ':growth_bar_width, ' +
      ':barb_density, ' +
      ':feather_age, ' +
      ':full_name, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    ParamByName('source_type').AsString := FEATHER_DATA_SOURCES[R.SourceType];
    ParamByName('symmetrical').AsString := SYMMETRIES[R.Symmetrical];
    ParamByName('feather_trait').AsString := FEATHER_TRAITS[R.FeatherTrait];
    SetIntParam(ParamByName('feather_number'), R.FeatherNumber);
    ParamByName('body_side').AsString := BODY_SIDES[R.BodySide];
    ParamByName('grown_percent').AsFloat := R.PercentGrown;
    SetFloatParam(ParamByName('feather_length'), R.FeatherLength);
    SetFloatParam(ParamByName('feather_area'), R.FeatherArea);
    SetFloatParam(ParamByName('feather_mass'), R.FeatherMass);
    SetFloatParam(ParamByName('rachis_width'), R.RachisWidth);
    SetFloatParam(ParamByName('growth_bar_width'), R.GrowthBarWidth);
    SetFloatParam(ParamByName('barb_density'), R.BarbDensity);
    ParamByName('feather_age').AsString := FEATHER_AGES[R.FeatherAge];
    ParamByName('full_name').AsString := GetFeatherFullname(R.SampleDate, R.TaxonId, FEATHER_TRAITS[R.FeatherTrait],
      R.FeatherNumber, BODY_SIDES[R.BodySide], FEATHER_AGES[R.FeatherAge]);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TFeatherRepository.TableName: string;
begin
  Result := TBL_FEATHERS;
end;

procedure TFeatherRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TFeather;
begin
  if not (E is TFeather) then
    raise Exception.Create('Update: Expected TFeather');

  R := TFeather(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TFeatherRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE feathers SET ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'taxon_id = :taxon_id, ' +
      'locality_id = :locality_id, ' +
      'individual_id = :individual_id, ' +
      'capture_id = :capture_id, ' +
      'sighting_id = :sighting_id, ' +
      'observer_id = :observer_id, ' +
      'source_type = :source_type, ' +
      'symmetrical = :symmetrical, ' +
      'feather_trait = :feather_trait, ' +
      'feather_number = :feather_number, ' +
      'body_side = :body_side, ' +
      'grown_percent = :grown_percent, ' +
      'feather_length = :feather_length, ' +
      'feather_area = :feather_area, ' +
      'feather_mass = :feather_mass, ' +
      'rachis_width = :rachis_width, ' +
      'growth_bar_width = :growth_bar_width, ' +
      'barb_density = :barb_density, ' +
      'feather_age = :feather_age, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'exported_status = :exported_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (feather_id = :feather_id)');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    ParamByName('source_type').AsString := FEATHER_DATA_SOURCES[R.SourceType];
    ParamByName('symmetrical').AsString := SYMMETRIES[R.Symmetrical];
    ParamByName('feather_trait').AsString := FEATHER_TRAITS[R.FeatherTrait];
    SetIntParam(ParamByName('feather_number'), R.FeatherNumber);
    ParamByName('body_side').AsString := BODY_SIDES[R.BodySide];
    ParamByName('grown_percent').AsFloat := R.PercentGrown;
    SetFloatParam(ParamByName('feather_length'), R.FeatherLength);
    SetFloatParam(ParamByName('feather_area'), R.FeatherArea);
    SetFloatParam(ParamByName('feather_mass'), R.FeatherMass);
    SetFloatParam(ParamByName('rachis_width'), R.RachisWidth);
    SetFloatParam(ParamByName('growth_bar_width'), R.GrowthBarWidth);
    SetFloatParam(ParamByName('barb_density'), R.BarbDensity);
    ParamByName('feather_age').AsString := FEATHER_AGES[R.FeatherAge];
    ParamByName('full_name').AsString := GetFeatherFullname(R.SampleDate, R.TaxonId, FEATHER_TRAITS[R.FeatherTrait],
      R.FeatherNumber, BODY_SIDES[R.BodySide], FEATHER_AGES[R.FeatherAge]);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('feather_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TCapture }

constructor TCapture.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TCapture.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCapture then
  begin
    FFullName := TCapture(Source).FullName;
    FSurveyId := TCapture(Source).SurveyId;
    FTaxonId := TCapture(Source).TaxonId;
    FIndividualId := TCapture(Source).IndividualId;
    FProjectId := TCapture(Source).ProjectId;
    FCaptureDate := TCapture(Source).CaptureDate;
    FCaptureTime := TCapture(Source).CaptureTime;
    FLocalityId := TCapture(Source).LocalityId;
    FNetStationId := TCapture(Source).NetStationId;
    FNetId := TCapture(Source).NetId;
    FLatitude := TCapture(Source).Latitude;
    FLongitude := TCapture(Source).Longitude;
    FBanderId := TCapture(Source).BanderId;
    FAnnotatorId := TCapture(Source).AnnotatorId;
    FSubjectStatus := TCapture(Source).SubjectStatus;
    FCaptureType := TCapture(Source).CaptureType;
    FSubjectSex := TCapture(Source).SubjectSex;
    FHowSexed := TCapture(Source).HowSexed;
    FBandId := TCapture(Source).BandId;
    FRemovedBandId := TCapture(Source).RemovedBandId;
    FRightLegBelow := TCapture(Source).RightLegBelow;
    FLeftLegBelow := TCapture(Source).LeftLegBelow;
    FRightLegAbove := TCapture(Source).RightLegAbove;
    FLeftLegAbove := TCapture(Source).LeftLegAbove;
    FWeight := TCapture(Source).Weight;
    FTarsusLength := TCapture(Source).TarsusLength;
    FTarsusDiameter := TCapture(Source).TarsusDiameter;
    FCulmenLength := TCapture(Source).CulmenLength;
    FExposedCulmen := TCapture(Source).ExposedCulmen;
    FBillWidth := TCapture(Source).BillWidth;
    FBillHeight := TCapture(Source).BillHeight;
    FNostrilBillTip := TCapture(Source).NostrilBillTip;
    FSkullLength := TCapture(Source).SkullLength;
    FHaluxLengthTotal := TCapture(Source).HaluxLengthTotal;
    FHaluxLengthFinger := TCapture(Source).HaluxLengthFinger;
    FHaluxLengthClaw := TCapture(Source).HaluxLengthClaw;
    FRightWingChord := TCapture(Source).RightWingChord;
    FFirstSecondaryChord := TCapture(Source).FirstSecondaryChord;
    FTailLength := TCapture(Source).TailLength;
    FCentralRetrixLength := TCapture(Source).CentralRetrixLength;
    FExternalRetrixLength := TCapture(Source).ExternalRetrixLength;
    FTotalLength := TCapture(Source).TotalLength;
    FFeatherMites := TCapture(Source).FeatherMites;
    FFat := TCapture(Source).Fat;
    FBroodPatch := TCapture(Source).BroodPatch;
    FCloacalProtuberance := TCapture(Source).CloacalProtuberance;
    FBodyMolt := TCapture(Source).BodyMolt;
    FFlightFeathersMolt := TCapture(Source).FlightFeathersMolt;
    FFlightFeathersWear := TCapture(Source).FlightFeathersWear;
    FMoltLimits := TCapture(Source).MoltLimits;
    FCycleCode := TCapture(Source).CycleCode;
    FSubjectAge := TCapture(Source).SubjectAge;
    FHowAged := TCapture(Source).HowAged;
    FSkullOssification := TCapture(Source).SkullOssification;
    FKippsIndex := TCapture(Source).KippsIndex;
    FGlucose := TCapture(Source).Glucose;
    FHemoglobin := TCapture(Source).Hemoglobin;
    FHematocrit := TCapture(Source).Hematocrit;
    FPhilornisLarvaeTally := TCapture(Source).PhilornisLarvaeTally;
    FBloodSample := TCapture(Source).BloodSample;
    FFeatherSample := TCapture(Source).FeatherSample;
    FClawSample := TCapture(Source).ClawSample;
    FFecesSample := TCapture(Source).FecesSample;
    FParasiteSample := TCapture(Source).ParasiteSample;
    FSubjectRecorded := TCapture(Source).SubjectRecorded;
    FSubjectCollected := TCapture(Source).SubjectCollected;
    FSubjectPhotographed := TCapture(Source).SubjectPhotographed;
    FFieldNumber := TCapture(Source).FieldNumber;
    FPhotographer1Id := TCapture(Source).Photographer1Id;
    FPhotographer2Id := TCapture(Source).Photographer2Id;
    FStartPhotoNumber := TCapture(Source).StartPhotoNumber;
    FEndPhotoNumber := TCapture(Source).EndPhotoNumber;
    FCameraName := TCapture(Source).CameraName;
    FEscaped := TCapture(Source).Escaped;
    FNeedsReview := TCapture(Source).NeedsReview;
    FNotes := TCapture(Source).Notes;
  end;
end;

procedure TCapture.Clear;
begin
  inherited;
  FFullName := EmptyStr;
  FSurveyId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FProjectId := 0;
  FCaptureDate := NullDate;
  FCaptureTime := NullTime;
  FLocalityId := 0;
  FNetStationId := 0;
  FNetId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FBanderId := 0;
  FAnnotatorId := 0;
  FSubjectStatus := sstNormal;
  FCaptureType := cptNew;
  FSubjectSex := sexUnknown;
  FHowSexed := EmptyStr;
  FBandId := 0;
  FRemovedBandId := 0;
  FRightLegBelow := EmptyStr;
  FLeftLegBelow := EmptyStr;
  FRightLegAbove := EmptyStr;
  FLeftLegAbove := EmptyStr;
  FWeight := 0.0;
  FTarsusLength := 0.0;
  FTarsusDiameter := 0.0;
  FCulmenLength := 0.0;
  FExposedCulmen := 0.0;
  FBillWidth := 0.0;
  FBillHeight := 0.0;
  FNostrilBillTip := 0.0;
  FSkullLength := 0.0;
  FHaluxLengthTotal := 0.0;
  FHaluxLengthFinger := 0.0;
  FHaluxLengthClaw := 0.0;
  FRightWingChord := 0.0;
  FFirstSecondaryChord := 0.0;
  FTailLength := 0.0;
  FCentralRetrixLength := 0.0;
  FExternalRetrixLength := 0.0;
  FTotalLength := 0.0;
  FFeatherMites := EmptyStr;
  FFat := EmptyStr;
  FBroodPatch := EmptyStr;
  FCloacalProtuberance := EmptyStr;
  FBodyMolt := EmptyStr;
  FFlightFeathersMolt := EmptyStr;
  FFlightFeathersWear := EmptyStr;
  FMoltLimits := EmptyStr;
  FCycleCode := EmptyStr;
  FSubjectAge := ageUnknown;
  FHowAged := EmptyStr;
  FSkullOssification := EmptyStr;
  FKippsIndex := 0.0;
  FGlucose := 0.0;
  FHemoglobin := 0.0;
  FHematocrit := 0.0;
  FPhilornisLarvaeTally := 0;
  FBloodSample := False;
  FFeatherSample := False;
  FClawSample := False;
  FFecesSample := False;
  FParasiteSample := False;
  FSubjectRecorded := False;
  FSubjectCollected := False;
  FSubjectPhotographed := False;
  FFieldNumber := EmptyStr;
  FPhotographer1Id := 0;
  FPhotographer2Id := 0;
  FStartPhotoNumber := EmptyStr;
  FEndPhotoNumber := EmptyStr;
  FCameraName := EmptyStr;
  FEscaped := False;
  FNeedsReview := False;
  FNotes := EmptyStr;
end;

function TCapture.Clone: TXolmisRecord;
begin
  Result := TCapture(inherited Clone);
end;

function TCapture.Diff(const aOld: TCapture; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.CaptureDate, FCaptureDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.CaptureTime, FCaptureTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.NetStationId, FNetStationId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnetID, aOld.NetId, FNetId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBanderID, aOld.BanderId, FBanderId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAnnotatorID, aOld.AnnotatorId, FAnnotatorId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStatus, aOld.SubjectStatus, FSubjectStatus, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.CaptureType, FCaptureType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSex, aOld.SubjectSex, FSubjectSex, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHowWasSexed, aOld.HowSexed, FHowSexed, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBandID, aOld.BandId, FBandId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRemovedBandID, aOld.RemovedBandId, FRemovedBandId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRightTarsus, aOld.RightLegBelow, FRightLegBelow, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLeftTarsus, aOld.LeftLegBelow, FLeftLegBelow, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWeight, aOld.Weight, FWeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTarsusLength, aOld.TarsusLength, FTarsusLength, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTarsusDiameter, aOld.TarsusDiameter, FTarsusDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTotalCulmen, aOld.CulmenLength, FCulmenLength, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExposedCulmen, aOld.ExposedCulmen, FExposedCulmen, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBillWidth, aOld.BillWidth, FBillWidth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBillHeight, aOld.BillHeight, FBillHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNostrilToBillTip, aOld.NostrilBillTip, FNostrilBillTip, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSkullLength, aOld.SkullLength, FSkullLength, R) then
    Changes.Add(R);
  //if FieldValuesDiff(rscHaluxLengthTotal, aOld.HaluxLengthTotal, FHaluxLengthTotal, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscHaluxLengthFinger, aOld.HaluxLengthFinger, FHaluxLengthFinger, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscHaluxLengthClaw, aOld.HaluxLengthClaw, FHaluxLengthClaw, R) then
  //  Changes.Add(R);
  if FieldValuesDiff(rscRightWingChord, aOld.RightWingChord, FRightWingChord, R) then
    Changes.Add(R);
  if FieldValuesDiff(rsc1stSecondaryChord, aOld.FirstSecondaryChord, FFirstSecondaryChord, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTailLength, aOld.TailLength, FTailLength, R) then
    Changes.Add(R);
  //if FieldValuesDiff(rscCentralRetrixLength, aOld.CentralRetrixLength, FCentralRetrixLength, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscExternalRetrixLength, aOld.ExternalRetrixLength, FExternalRetrixLength, R) then
  //  Changes.Add(R);
  if FieldValuesDiff(rscTotalLength, aOld.TotalLength, FTotalLength, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFeatherMites, aOld.FeatherMites, FFeatherMites, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFat, aOld.Fat, FFat, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBroodPatch, aOld.BroodPatch, FBroodPatch, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloacalProtuberance, aOld.CloacalProtuberance, FCloacalProtuberance, R) then
    Changes.Add(R);
  //if FieldValuesDiff('Muda (leg.)', aOld.OldMolt, FOldMolt, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff('Muda prim'#225'rias (leg.)', aOld.OldPrimariesMolt, FOldPrimariesMolt, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff('Muda secund'#225'rias (leg.)', aOld.OldSecondariesMolt, FOldSecondariesMolt, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff('Muda retrizes (leg.)', aOld.OldRetricesMolt, FOldRetricesMolt, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff('Muda corpo (leg.)', aOld.OldBodyMolt, FOldBodyMolt, R) then
  //  Changes.Add(R);
  if FieldValuesDiff(rscBodyMolt, aOld.BodyMolt, FBodyMolt, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFlightFeathersMolt, aOld.FlightFeathersMolt, FFlightFeathersMolt, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFlightFeathersWear, aOld.FlightFeathersWear, FFlightFeathersWear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMoltLimits, aOld.MoltLimits, FMoltLimits, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMoltCycle, aOld.CycleCode, FCycleCode, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAge, aOld.SubjectAge, FSubjectAge, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHowWasAged, aOld.HowAged, FHowAged, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSkullOssification, aOld.SkullOssification, FSkullOssification, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscKippsDistance, aOld.KippsIndex, FKippsIndex, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscGlucose, aOld.Glucose, FGlucose, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHemoglobin, aOld.Hemoglobin, FHemoglobin, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHematocrit, aOld.Hematocrit, FHematocrit, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscQuantPhilornisLarvae, aOld.PhilornisLarvaeTally, FPhilornisLarvaeTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBlood, aOld.BloodSample, FBloodSample, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFeathers, aOld.FeatherSample, FFeatherSample, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscClaw, aOld.ClawSample, FClawSample, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFeces, aOld.FecesSample, FFecesSample, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscParasites, aOld.ParasiteSample, FParasiteSample, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCollectedWhole, aOld.SubjectCollected, FSubjectCollected, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRecorded, aOld.SubjectRecorded, FSubjectRecorded, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPhotographed, aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPhotographer1ID, aOld.Photographer1Id, FPhotographer1Id, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPhotographer2ID, aOld.Photographer2Id, FPhotographer2Id, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInitialPhotoNr, aOld.StartPhotoNumber, FStartPhotoNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFinalPhotoNr, aOld.EndPhotoNumber, FEndPhotoNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCamera, aOld.CameraName, FCameraName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEscaped, aOld.Escaped, FEscaped, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNeedsReview, aOld.NeedsReview, FNeedsReview, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TCapture.EqualsTo(const Other: TCapture): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TCapture.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName     := Obj.Get('full_name', '');
    FSurveyId     := Obj.Get('survey_id', 0);
    FTaxonId      := Obj.Get('taxon_id', 0);
    FIndividualId := Obj.Get('individual_id', 0);
    FCaptureDate  := Obj.Get('capture_date', NullDate);
    FCaptureTime  := Obj.Get('capture_time', NullTime);
    FLocalityId   := Obj.Get('locality_id', 0);
    FNetStationId := Obj.Get('net_station_id', 0);
    FNetId        := Obj.Get('net_id', 0);
    FLongitude    := Obj.Get('longitude', 0.0);
    FLatitude     := Obj.Get('latitude', 0.0);
    FBanderId     := Obj.Get('bander_id', 0);
    FAnnotatorId  := Obj.Get('annotator_id', 0);
    case Obj.Get('subject_status', '') of
      'N': FSubjectStatus := sstNormal;
      'I': FSubjectStatus := sstInjured;
      'W': FSubjectStatus := sstWingSprain;
      'X': FSubjectStatus := sstStressed;
      'D': FSubjectStatus := sstDead;
    end;
    case Obj.Get('capture_type', '') of
      'N': FCaptureType := cptNew;
      'R': FCaptureType := cptRecapture;
      'S': FCaptureType := cptSameDay;
      'C': FCaptureType := cptChangeBand;
      'U': FCaptureType := cptUnbanded;
    end;
    case Obj.Get('sex', '') of
      'U': FSubjectSex := sexUnknown;
      'F': FSubjectSex := sexFemale;
      'M': FSubjectSex := sexMale;
    else
      FSubjectSex := sexUnknown;
    end;
    FHowSexed             := Obj.Get('how_was_sexed', '');
    FBandId               := Obj.Get('band_id', 0);
    FWeight               := Obj.Get('weight', 0.0);
    FTarsusLength         := Obj.Get('tarsus_length', 0.0);
    FTarsusDiameter       := Obj.Get('tarsus_diameter', 0.0);
    FExposedCulmen        := Obj.Get('exposed_culmen', 0.0);
    FBillWidth            := Obj.Get('bill_width', 0.0);
    FBillHeight           := Obj.Get('bill_height', 0.0);
    FNostrilBillTip       := Obj.Get('nostril_to_bill_tip', 0.0);
    FSkullLength          := Obj.Get('skull_length', 0.0);
    FRightWingChord       := Obj.Get('right_wing_chord', 0.0);
    FFirstSecondaryChord  := Obj.Get('first_secondary_chord', 0.0);
    FTailLength           := Obj.Get('tail_length', 0.0);
    FFat                  := Obj.Get('fat', '');
    FBroodPatch           := Obj.Get('brood_patch', '');
    FCloacalProtuberance  := Obj.Get('cloacal_protuberance', '');
    FBodyMolt             := Obj.Get('body_molt', '');
    FFlightFeathersMolt   := Obj.Get('flight_feathers_molt', '');
    FFlightFeathersWear   := Obj.Get('flight_feathers_wear', '');
    FMoltLimits           := Obj.Get('molt_limits', '');
    FCycleCode            := Obj.Get('cycle_code', '');
    FHowAged              := Obj.Get('how_was_aged', '');
    FSkullOssification    := Obj.Get('skull_ossification', '');
    FKippsIndex           := Obj.Get('kipps_index', 0.0);
    FGlucose              := Obj.Get('glucose', 0.0);
    FHemoglobin           := Obj.Get('hemoglobin', 0.0);
    FHematocrit           := Obj.Get('hematocrit', 0.0);
    FBloodSample          := Obj.Get('blood_sample', False);
    FFeatherSample        := Obj.Get('feather_sample', False);
    FSubjectPhotographed  := Obj.Get('photographed', False);
    FPhotographer1Id      := Obj.Get('photographer_1_id', 0);
    FPhotographer2Id      := Obj.Get('photographer_2_id', 0);
    FStartPhotoNumber     := Obj.Get('start_photo_number', '');
    FEndPhotoNumber       := Obj.Get('end_photo_number', '');
    FCameraName           := Obj.Get('camera_name', '');
    FRemovedBandId        := Obj.Get('removed_band_id', 0);
    FRightLegBelow        := Obj.Get('right_tarsus', '');
    FLeftLegBelow         := Obj.Get('left_tarsus', '');
    FRightLegAbove        := Obj.Get('right_tibia', '');
    FLeftLegAbove         := Obj.Get('left_tibia', '');
    FEscaped              := Obj.Get('escaped', False);
    FNeedsReview          := Obj.Get('needs_review', False);
    FNotes                := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TCapture.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('capture_date', FCaptureDate);
    JSONObject.Add('capture_time', FCaptureTime);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('net_station_id', FNetStationId);
    JSONObject.Add('net_id', FNetId);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('bander_id', FBanderId);
    JSONObject.Add('annotator_id', FAnnotatorId);
    JSONObject.Add('subject_status', SUBJECT_STATUSES[FSubjectStatus]);
    JSONObject.Add('capture_type', CAPTURE_TYPES[FCaptureType]);
    JSONObject.Add('sex', SEXES[FSubjectSex]);
    JSONObject.Add('how_was_sexed', FHowSexed);
    JSONObject.Add('band_id', FBandId);
    JSONObject.Add('weight', FWeight);
    JSONObject.Add('tarsus_length', FTarsusLength);
    JSONObject.Add('tarsus_diameter', FTarsusDiameter);
    JSONObject.Add('exposed_culmen', FExposedCulmen);
    JSONObject.Add('bill_width', FBillWidth);
    JSONObject.Add('bill_height', FBillHeight);
    JSONObject.Add('nostril_to_bill_tip', FNostrilBillTip);
    JSONObject.Add('skull_length', FSkullLength);
    JSONObject.Add('right_wing_chord', FRightWingChord);
    JSONObject.Add('first_secondary_chord', FFirstSecondaryChord);
    JSONObject.Add('tail_length', FTailLength);
    JSONObject.Add('fat', FFat);
    JSONObject.Add('brood_patch', FBroodPatch);
    JSONObject.Add('cloacal_protuberance', FCloacalProtuberance);
    JSONObject.Add('body_molt', FBodyMolt);
    JSONObject.Add('flight_feathers_molt', FFlightFeathersMolt);
    JSONObject.Add('flight_feathers_wear', FFlightFeathersWear);
    JSONObject.Add('molt_limits', FMoltLimits);
    JSONObject.Add('cycle_code', FCycleCode);
    JSONObject.Add('how_was_aged', FHowAged);
    JSONObject.Add('skull_ossification', FSkullOssification);
    JSONObject.Add('kipps_index', FKippsIndex);
    JSONObject.Add('glucose', FGlucose);
    JSONObject.Add('hemoglobin', FHemoglobin);
    JSONObject.Add('hematocrit', FHematocrit);
    JSONObject.Add('blood_sample', FBloodSample);
    JSONObject.Add('feather_sample', FFeatherSample);
    JSONObject.Add('photographed', FSubjectPhotographed);
    JSONObject.Add('photographer_1_id', FPhotographer1Id);
    JSONObject.Add('photographer_2_id', FPhotographer2Id);
    JSONObject.Add('start_photo_number', FStartPhotoNumber);
    JSONObject.Add('end_photo_number', FEndPhotoNumber);
    JSONObject.Add('camera_name', FCameraName);
    JSONObject.Add('removed_band_id', FRemovedBandId);
    JSONObject.Add('right_tarsus', FRightLegBelow);
    JSONObject.Add('left_tarsus', FLeftLegBelow);
    JSONObject.Add('right_tibia', FRightLegAbove);
    JSONObject.Add('left_tibia', FLeftLegAbove);
    JSONObject.Add('escaped', FEscaped);
    JSONObject.Add('needs_review', FNeedsReview);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TCapture.ToString: String;
begin
  Result := Format('Capture(Id=%d, FullName=%s, SurveyId=%d, TaxonId=%d, IndividualId=%d, CaptureDate=%s, ' +
    'CaptureTime=%s, LocalityId=%d, NetStationId=%d, NetId=%d, Longitude=%f, Latitude=%f, BanderId=%d, ' +
    'AnnotatorId=%d, SubjectStatus=%s, CaptureType=%s, Sex=%s, HowSexed=%s, BandId=%d, Weight=%f, ' +
    'TarsusLength=%f, TarsusDiameter=%f, ExposedCulmen=%f, BillWidth=%f, BillHeight=%f, NostrilBillTip=%f, ' +
    'SkullLength=%f, RightWingChord=%f, FirstSecondaryChord=%f, TailLength=%f, Fat=%s, BroodPatch=%s, ' +
    'CloacalProtuberance=%s, BodyMolt=%s, FlightFeathersMolt=%s, FlightFeathersWear=%s, MoltLimits=%s, ' +
    'CycleCode=%s, HowAged=%s, SkullOssification=%s, KippsIndex=%f, Glucose=%f, Hemoglobin=%f, Hematocrit=%f, ' +
    'BloodSample=%s, FeatherSample=%s, Photographed=%s, Photographer1Id=%d, Photographer2Id=%d, ' +
    'StartPhotoNumber=%s, EndPhotoNumber=%s, CameraName=%s, RemovedBandId=%d, RightTarsus=%s, LeftTarsus=%s, ' +
    'RightTibia=%s, LeftTibia=%s, Escaped=%s, NeedsReview=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FSurveyId, FTaxonId, FIndividualId, DateToStr(FCaptureDate), TimeToStr(FCaptureTime),
    FLocalityId, FNetStationId, FNetId, FLongitude, FLatitude, FBanderId, FAnnotatorId, SUBJECT_STATUSES[FSubjectStatus],
    CAPTURE_TYPES[FCaptureType], SEXES[FSubjectSex], FHowSexed, FBandId, FWeight, FTarsusLength, FTarsusDiameter,
    FExposedCulmen, FBillWidth, FBillHeight, FNostrilBillTip, FSkullLength, FRightWingChord, FFirstSecondaryChord,
    FTailLength, FFat, FBroodPatch, FCloacalProtuberance, FBodyMolt, FFlightFeathersMolt, FFlightFeathersWear,
    FMoltLimits, FCycleCode, FHowAged, FSkullOssification, FKippsIndex, FGlucose, FHemoglobin, FHematocrit,
    BoolToStr(FBloodSample, 'True', 'False'), BoolToStr(FFeatherSample, 'True', 'False'),
    BoolToStr(FSubjectPhotographed, 'True', 'False'), FPhotographer1Id, FPhotographer2Id, FStartPhotoNumber,
    FEndPhotoNumber, FCameraName, FRemovedBandId, FRightLegBelow, FLeftLegBelow, FRightLegAbove, FLeftLegAbove,
    BoolToStr(FEscaped, 'True', 'False'), BoolToStr(FNeedsReview, 'True', 'False'), FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TCapture.Validate(out Msg: string): Boolean;
begin
  if FCaptureDate = NullDate then
  begin
    Msg := 'Capture date required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TCaptureRepository }

procedure TCaptureRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TCapture;
begin
  if not (E is TCapture) then
    raise Exception.Create('Delete: Expected TCapture');

  R := TCapture(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TCaptureRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_CAPTURE_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TCaptureRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_CAPTURE_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCaptureRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_CAPTURE_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TCapture) then
    raise Exception.Create('FindBy: Expected TCapture');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT ' +
      'capture_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'taxon_id, ' +
      'full_name, ' +
      'project_id, ' +
      'capture_date, ' +
      'capture_time, ' +
      'locality_id, ' +
      'net_station_id, ' +
      'net_id, ' +
      'longitude, ' +
      'latitude, ' +
      'bander_id, ' +
      'annotator_id, ' +
      'subject_status, ' +
      'capture_type, ' +
      'subject_sex, ' +
      'how_sexed, ' +
      'band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'weight, ' +
      'tarsus_length, ' +
      'tarsus_diameter, ' +
      'culmen_length, ' +
      'exposed_culmen, ' +
      'bill_width, ' +
      'bill_height, ' +
      'nostril_bill_tip, ' +
      'skull_length, ' +
      'halux_length_total, ' +
      'halux_length_finger, ' +
      'halux_length_claw, ' +
      'right_wing_chord, ' +
      'first_secondary_chord, ' +
      'tail_length, ' +
      'central_retrix_length, ' +
      'external_retrix_length, ' +
      'total_length, ' +
      'feather_mites, ' +
      'fat, ' +
      'brood_patch, ' +
      'cloacal_protuberance, ' +
      'body_molt, ' +
      'flight_feathers_molt, ' +
      'flight_feathers_wear, ' +
      'molt_limits, ' +
      'cycle_code, ' +
      'subject_age, ' +
      'how_aged, ' +
      'skull_ossification, ' +
      'kipps_index, ' +
      'glucose, ' +
      'hemoglobin, ' +
      'hematocrit, ' +
      'philornis_larvae_tally, ' +
      'blood_sample, ' +
      'feather_sample, ' +
      'claw_sample, ' +
      'feces_sample, ' +
      'parasite_sample, ' +
      'subject_collected, ' +
      'subject_recorded, ' +
      'subject_photographed, ' +
      'field_number, ' +
      'photographer_1_id, ' +
      'photographer_2_id, ' +
      'start_photo_number, ' +
      'end_photo_number, ' +
      'camera_name, ' +
      'escaped, ' +
      'needs_review, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM captures');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TCapture(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TCaptureRepository.FindByBand(const aTaxon, aBand: Integer; aCaptureType, aDate, aTime: String;
  E: TCapture);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM captures');
    Add('WHERE (taxon_id = :ataxon)');
    Add('AND (band_id = :aband)');
    Add('AND (capture_type = :anature)');
    Add('AND (date(capture_date) = date(:adate))');
    Add('AND (time(capture_time) = time(:atime))');
    ParamByName('ATAXON').AsInteger := aTaxon;
    ParamByName('ABAND').AsInteger := aBand;
    ParamByName('ANATURE').AsString := aCaptureType;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('ATIME').AsString := aTime;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCaptureRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TCapture) then
    raise Exception.Create('GetById: Expected TCapture');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'capture_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'taxon_id, ' +
      'full_name, ' +
      'project_id, ' +
      'capture_date, ' +
      'capture_time, ' +
      'locality_id, ' +
      'net_station_id, ' +
      'net_id, ' +
      'longitude, ' +
      'latitude, ' +
      'bander_id, ' +
      'annotator_id, ' +
      'subject_status, ' +
      'capture_type, ' +
      'subject_sex, ' +
      'how_sexed, ' +
      'band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'weight, ' +
      'tarsus_length, ' +
      'tarsus_diameter, ' +
      'culmen_length, ' +
      'exposed_culmen, ' +
      'bill_width, ' +
      'bill_height, ' +
      'nostril_bill_tip, ' +
      'skull_length, ' +
      'halux_length_total, ' +
      'halux_length_finger, ' +
      'halux_length_claw, ' +
      'right_wing_chord, ' +
      'first_secondary_chord, ' +
      'tail_length, ' +
      'central_retrix_length, ' +
      'external_retrix_length, ' +
      'total_length, ' +
      'feather_mites, ' +
      'fat, ' +
      'brood_patch, ' +
      'cloacal_protuberance, ' +
      'body_molt, ' +
      'flight_feathers_molt, ' +
      'flight_feathers_wear, ' +
      'molt_limits, ' +
      'cycle_code, ' +
      'subject_age, ' +
      'how_aged, ' +
      'skull_ossification, ' +
      'kipps_index, ' +
      'glucose, ' +
      'hemoglobin, ' +
      'hematocrit, ' +
      'philornis_larvae_tally, ' +
      'blood_sample, ' +
      'feather_sample, ' +
      'claw_sample, ' +
      'feces_sample, ' +
      'parasite_sample, ' +
      'subject_collected, ' +
      'subject_recorded, ' +
      'subject_photographed, ' +
      'field_number, ' +
      'photographer_1_id, ' +
      'photographer_2_id, ' +
      'start_photo_number, ' +
      'end_photo_number, ' +
      'camera_name, ' +
      'escaped, ' +
      'needs_review, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM captures');
    Add('WHERE capture_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TCapture(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCaptureRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TCapture;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TCapture) then
    raise Exception.Create('Hydrate: Expected TCapture');

  R := TCapture(E);
  with aDataSet do
  begin
    R.Id := FieldByName('capture_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.CaptureDate := FieldByName('capture_date').AsDateTime;
    R.CaptureTime := FieldByName('capture_time').AsDateTime;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    R.NetStationId := FieldByName('net_station_id').AsInteger;
    R.NetId := FieldByName('net_id').AsInteger;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.BanderId := FieldByName('bander_id').AsInteger;
    R.AnnotatorId := FieldByName('annotator_id').AsInteger;
    case FieldByName('subject_status').AsString of
      'N': R.SubjectStatus := sstNormal;
      'I': R.SubjectStatus := sstInjured;
      'W': R.SubjectStatus := sstWingSprain;
      'X': R.SubjectStatus := sstStressed;
      'D': R.SubjectStatus := sstDead;
    end;
    case FieldByName('capture_type').AsString of
      'N': R.CaptureType := cptNew;
      'R': R.CaptureType := cptRecapture;
      'S': R.CaptureType := cptSameDay;
      'C': R.CaptureType := cptChangeBand;
    else
      R.CaptureType := cptUnbanded;
    end;
    case FieldByName('subject_sex').AsString of
      'M': R.SubjectSex := sexMale;
      'F': R.SubjectSex := sexFemale;
    else
      R.SubjectSex := sexUnknown;
    end;
    R.HowSexed := FieldByName('how_sexed').AsString;
    R.BandId := FieldByName('band_id').AsInteger;
    R.RemovedBandId := FieldByName('removed_band_id').AsInteger;
    R.RightLegBelow := FieldByName('right_leg_below').AsString;
    R.LeftLegBelow := FieldByName('left_leg_below').AsString;
    R.RightLegAbove := FieldByName('right_leg_above').AsString;
    R.LeftLegAbove := FieldByName('left_leg_above').AsString;
    R.Weight := FieldByName('weight').AsFloat;
    R.TarsusLength := FieldByName('tarsus_length').AsFloat;
    R.TarsusDiameter := FieldByName('tarsus_diameter').AsFloat;
    R.CulmenLength := FieldByName('culmen_length').AsFloat;
    R.ExposedCulmen := FieldByName('exposed_culmen').AsFloat;
    R.BillWidth := FieldByName('bill_width').AsFloat;
    R.BillHeight := FieldByName('bill_height').AsFloat;
    R.NostrilBillTip := FieldByName('nostril_bill_tip').AsFloat;
    R.SkullLength := FieldByName('skull_length').AsFloat;
    if FindField('halux_length_total') <> nil then
      R.HaluxLengthTotal := FieldByName('halux_length_total').AsFloat;
    if FindField('halux_length_finger') <> nil then
      R.HaluxLengthFinger := FieldByName('halux_length_finger').AsFloat;
    if FindField('halux_length_claw') <> nil then
      R.HaluxLengthClaw := FieldByName('halux_length_claw').AsFloat;
    R.RightWingChord := FieldByName('right_wing_chord').AsFloat;
    R.FirstSecondaryChord := FieldByName('first_secondary_chord').AsFloat;
    R.TailLength := FieldByName('tail_length').AsFloat;
    if FindField('central_retrix_length') <> nil then
      R.CentralRetrixLength := FieldByName('central_retrix_length').AsFloat;
    if FindField('external_retrix_length') <> nil then
      R.ExternalRetrixLength := FieldByName('external_retrix_length').AsFloat;
    R.TotalLength := FieldByName('total_length').AsFloat;
    if FindField('feather_mites') <> nil then
      R.FeatherMites := FieldByName('feather_mites').AsString;
    R.Fat := FieldByName('fat').AsString;
    R.BroodPatch := FieldByName('brood_patch').AsString;
    R.CloacalProtuberance := FieldByName('cloacal_protuberance').AsString;
    R.BodyMolt := FieldByName('body_molt').AsString;
    R.FlightFeathersMolt := FieldByName('flight_feathers_molt').AsString;
    R.FlightFeathersWear := FieldByName('flight_feathers_wear').AsString;
    R.MoltLimits := FieldByName('molt_limits').AsString;
    R.CycleCode := FieldByName('cycle_code').AsString;
    case FieldByName('subject_age').AsString of
      'N': R.SubjectAge := ageNestling;
      'F': R.SubjectAge := ageFledgling;
      'J': R.SubjectAge := ageJuvenile;
      'A': R.SubjectAge := ageAdult;
      'Y': R.SubjectAge := ageFirstYear;
      'S': R.SubjectAge := ageSecondYear;
      'T': R.SubjectAge := ageThirdYear;
      '4': R.SubjectAge := ageFourthYear;
      '5': R.SubjectAge := ageFifthYear;
    else
      R.SubjectAge := ageUnknown;
    end;
    R.HowAged := FieldByName('how_aged').AsString;
    R.SkullOssification := FieldByName('skull_ossification').AsString;
    R.KippsIndex := FieldByName('kipps_index').AsFloat;
    R.Glucose := FieldByName('glucose').AsFloat;
    R.Hemoglobin := FieldByName('hemoglobin').AsFloat;
    R.Hematocrit := FieldByName('hematocrit').AsFloat;
    R.PhilornisLarvaeTally := FieldByName('philornis_larvae_tally').AsInteger;
    R.BloodSample := FieldByName('blood_sample').AsBoolean;
    R.FeatherSample := FieldByName('feather_sample').AsBoolean;
    R.ClawSample := FieldByName('claw_sample').AsBoolean;
    R.FecesSample := FieldByName('feces_sample').AsBoolean;
    R.ParasiteSample := FieldByName('parasite_sample').AsBoolean;
    R.SubjectRecorded := FieldByName('subject_recorded').AsBoolean;
    R.SubjectCollected := FieldByName('subject_collected').AsBoolean;
    R.SubjectPhotographed := FieldByName('subject_photographed').AsBoolean;
    R.FieldNumber := FieldByName('field_number').AsString;
    R.Photographer1Id := FieldByName('photographer_1_id').AsInteger;
    R.Photographer2Id := FieldByName('photographer_2_id').AsInteger;
    R.StartPhotoNumber := FieldByName('start_photo_number').AsString;
    R.EndPhotoNumber := FieldByName('end_photo_number').AsString;
    R.CameraName := FieldByName('camera_name').AsString;
    R.Escaped := FieldByName('escaped').AsBoolean;
    R.NeedsReview := FieldByName('needs_review').AsBoolean;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TCaptureRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TCapture;
begin
  if not (E is TCapture) then
    raise Exception.Create('Insert: Expected TCapture');

  R := TCapture(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO captures (' +
      'survey_id, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_date, ' +
      'capture_time, ' +
      'locality_id, ' +
      'net_station_id, ' +
      'net_id, ' +
      'latitude, ' +
      'longitude, ' +
      'bander_id, ' +
      'annotator_id, ' +
      'subject_status, ' +
      'capture_type, ' +
      'subject_sex, ' +
      'how_sexed, ' +
      'band_id, ' +
      'weight, ' +
      'tarsus_length, ' +
      'tarsus_diameter, ' +
      'exposed_culmen, ' +
      'bill_width, ' +
      'bill_height, ' +
      'nostril_bill_tip, ' +
      'skull_length, ' +
      'right_wing_chord, ' +
      'first_secondary_chord, ' +
      'tail_length, ' +
      'fat, ' +
      'brood_patch, ' +
      'cloacal_protuberance, ' +
      'body_molt, ' +
      'flight_feathers_molt, ' +
      'flight_feathers_wear, ' +
      'molt_limits, ' +
      'cycle_code, ' +
      'subject_age, ' +
      'how_aged, ' +
      'skull_ossification, ' +
      'kipps_index, ' +
      'glucose, ' +
      'hemoglobin, ' +
      'hematocrit, ' +
      'blood_sample, ' +
      'feather_sample, ' +
      'subject_photographed, ' +
      'photographer_1_id, ' +
      'photographer_2_id, ' +
      'start_photo_number, ' +
      'end_photo_number, ' +
      'camera_name, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'escaped, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date)');
    Add('VALUES (' +
      ':survey_id, ' +
      ':full_name, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      'date(:capture_date), ' +
      'time(:capture_time), ' +
      ':locality_id, ' +
      ':net_station_id, ' +
      ':net_id, ' +
      ':latitude, ' +
      ':longitude, ' +
      ':bander_id, ' +
      ':annotator_id, ' +
      ':subject_status, ' +
      ':capture_type, ' +
      ':subject_sex, ' +
      ':how_sexed, ' +
      ':band_id, ' +
      ':weight, ' +
      ':tarsus_length, ' +
      ':tarsus_diameter, ' +
      ':exposed_culmen, ' +
      ':bill_width, ' +
      ':bill_height, ' +
      ':nostril_bill_tip, ' +
      ':skull_length, ' +
      ':right_wing_chord, ' +
      ':first_secondary_chord, ' +
      ':tail_length, ' +
      ':fat, ' +
      ':brood_patch, ' +
      ':cloacal_protuberance, ' +
      ':body_molt, ' +
      ':flight_feathers_molt, ' +
      ':flight_feathers_wear, ' +
      ':molt_limits, ' +
      ':cycle_code, ' +
      ':subject_age, ' +
      ':how_aged, ' +
      ':skull_ossification, ' +
      ':kipps_index, ' +
      ':glucose, ' +
      ':hemoglobin, ' +
      ':hematocrit, ' +
      ':blood_sample, ' +
      ':feather_sample, ' +
      ':subject_photographed, ' +
      ':photographer_1_id, ' +
      ':photographer_2_id, ' +
      ':start_photo_number, ' +
      ':end_photo_number, ' +
      ':camera_name, ' +
      ':removed_band_id, ' +
      ':right_leg_below, ' +
      ':left_leg_below, ' +
      ':escaped, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    R.FullName := GetCaptureFullname(R.CaptureDate, R.TaxonId, R.BandId, SEXES[R.SubjectSex],
      CAPTURE_TYPES[R.CaptureType], R.CycleCode, False);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetDateParam(ParamByName('capture_date'), R.CaptureDate);
    SetTimeParam(ParamByName('capture_time'), R.CaptureTime);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetForeignParam(ParamByName('net_station_id'), R.NetStationId);
    SetForeignParam(ParamByName('net_id'), R.NetId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('bander_id'), R.BanderId);
    SetForeignParam(ParamByName('annotator_id'), R.AnnotatorId);
    SetStrParam(ParamByName('subject_status'), SUBJECT_STATUSES[R.SubjectStatus]);
    SetStrParam(ParamByName('capture_type'), CAPTURE_TYPES[R.CaptureType]);
    SetStrParam(ParamByName('subject_sex'), SEXES[R.SubjectSex]);
    SetStrParam(ParamByName('how_sexed'), R.HowSexed);
    SetForeignParam(ParamByName('band_id'), R.BandId);
    SetFloatParam(ParamByName('weight'), R.Weight);
    SetFloatParam(ParamByName('tarsus_length'), R.TarsusLength);
    SetFloatParam(ParamByName('tarsus_diameter'), R.TarsusDiameter);
    SetFloatParam(ParamByName('exposed_culmen'), R.ExposedCulmen);
    SetFloatParam(ParamByName('bill_width'), R.BillWidth);
    SetFloatParam(ParamByName('bill_height'), R.BillHeight);
    SetFloatParam(ParamByName('nostril_bill_tip'), R.NostrilBillTip);
    SetFloatParam(ParamByName('skull_length'), R.SkullLength);
    SetFloatParam(ParamByName('right_wing_chord'), R.RightWingChord);
    SetFloatParam(ParamByName('first_secondary_chord'), R.FirstSecondaryChord);
    SetFloatParam(ParamByName('tail_length'), R.TailLength);
    SetStrParam(ParamByName('fat'), R.Fat);
    SetStrParam(ParamByName('brood_patch'), R.BroodPatch);
    SetStrParam(ParamByName('cloacal_protuberance'), R.CloacalProtuberance);
    SetStrParam(ParamByName('body_molt'), R.BodyMolt);
    SetStrParam(ParamByName('flight_feathers_molt'), R.FlightFeathersMolt);
    SetStrParam(ParamByName('flight_feathers_wear'), R.FlightFeathersWear);
    SetStrParam(ParamByName('molt_limits'), R.MoltLimits);
    SetStrParam(ParamByName('cycle_code'), R.CycleCode);
    SetStrParam(ParamByName('subject_age'), AGES[R.SubjectAge]);
    SetStrParam(ParamByName('how_aged'), R.HowAged);
    SetStrParam(ParamByName('skull_ossification'), R.SkullOssification);
    SetFloatParam(ParamByName('kipps_index'), R.KippsIndex);
    SetFloatParam(ParamByName('glucose'), R.Glucose);
    SetFloatParam(ParamByName('hemoglobin'), R.Hemoglobin);
    SetFloatParam(ParamByName('hematocrit'), R.Hematocrit);
    ParamByName('blood_sample').AsBoolean := R.BloodSample;
    ParamByName('feather_sample').AsBoolean := R.FeatherSample;
    SetForeignParam(ParamByName('photographer_1_id'), R.Photographer1Id);
    SetForeignParam(ParamByName('photographer_2_id'), R.Photographer2Id);
    if (R.Photographer1Id > 0) then
      R.SubjectPhotographed := True;
    ParamByName('subject_photographed').AsBoolean := R.SubjectPhotographed;
    SetStrParam(ParamByName('start_photo_number'), R.StartPhotoNumber);
    SetStrParam(ParamByName('end_photo_number'), R.EndPhotoNumber);
    SetStrParam(ParamByName('camera_name'), R.CameraName);
    SetForeignParam(ParamByName('removed_band_id'), R.RemovedBandId);
    SetStrParam(ParamByName('right_leg_below'), R.RightLegBelow);
    SetStrParam(ParamByName('left_leg_below'), R.LeftLegBelow);
    ParamByName('escaped').AsBoolean := R.Escaped;
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TCaptureRepository.TableName: string;
begin
  Result := TBL_CAPTURES;
end;

procedure TCaptureRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TCapture;
begin
  if not (E is TCapture) then
    raise Exception.Create('Update: Expected TCapture');

  R := TCapture(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TCaptureRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE captures SET ' +
      'survey_id = :survey_id, ' +
      'full_name = :full_name, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'project_id = :project_id, ' +
      'capture_date = date(:capture_date), ' +
      'capture_time = time(:capture_time), ' +
      'locality_id = :locality_id, ' +
      'net_station_id = :net_station_id, ' +
      'net_id = :net_id, ' +
      'latitude = :latitude, ' +
      'longitude = :longitude, ' +
      'bander_id = :bander_id, ' +
      'annotator_id = :annotator_id, ' +
      'subject_status = :subject_status, ' +
      'capture_type = :capture_type, ' +
      'subject_sex = :subject_sex, ' +
      'how_sexed = :how_sexed, ' +
      'band_id = :band_id, ' +
      'weight = :weight, ' +
      'tarsus_length = :tarsus_length, ' +
      'tarsus_diameter = :tarsus_diameter, ' +
      'culmen_length = :culmen_length, ' +
      'exposed_culmen = :exposed_culmen, ' +
      'bill_width = :bill_width, ' +
      'bill_height = :bill_height, ' +
      'nostril_bill_tip = :nostril_bill_tip, ' +
      'skull_length = :skull_length, ' +
      'right_wing_chord = :right_wing_chord, ' +
      'first_secondary_chord = :first_secondary_chord, ' +
      'tail_length = :tail_length, ' +
      'fat = :fat, ' +
      'brood_patch = :brood_patch, ' +
      'cloacal_protuberance = :cloacal_protuberance, ' +
      'body_molt = :body_molt, ' +
      'flight_feathers_molt = :flight_feathers_molt, ' +
      'flight_feathers_wear = :flight_feathers_wear, ' +
      'molt_limits = :molt_limits, ' +
      'cycle_code = :cycle_code, ' +
      'subject_age = :subject_age, ' +
      'how_aged = :how_aged, ' +
      'skull_ossification = :skull_ossification, ' +
      'halux_length_total = :halux_length_total, ' +
      'halux_length_finger = :halux_length_finger, ' +
      'halux_length_claw = :halux_length_claw, ' +
      'central_retrix_length = :central_retrix_length, ' +
      'external_retrix_length = :external_retrix_length, ' +
      'total_length = :total_length, ' +
      'feather_mites = :feather_mites, ' +
      'philornis_larvae_tally = :philornis_larvae_tally, ' +
      'kipps_index = :kipps_index, ' +
      'glucose = :glucose, ' +
      'hemoglobin = :hemoglobin, ' +
      'hematocrit = :hematocrit, ' +
      'field_number = :field_number, ' +
      'blood_sample = :blood_sample, ' +
      'feather_sample = :feather_sample, ' +
      'claw_sample = :claw_sample, ' +
      'feces_sample = :feces_sample, ' +
      'parasite_sample = :parasite_sample, ' +
      'subject_collected = :subject_collected, ' +
      'subject_recorded = :subject_recorded, ' +
      'subject_photographed = :subject_photographed, ' +
      'photographer_1_id = :photographer_1_id, ' +
      'photographer_2_id = :photographer_2_id, ' +
      'start_photo_number = :start_photo_number, ' +
      'end_photo_number = :end_photo_number, ' +
      'camera_name = :camera_name, ' +
      'removed_band_id = :removed_band_id, ' +
      'right_leg_below = :right_leg_below, ' +
      'left_leg_below = :left_leg_below, ' +
      'right_leg_above = :right_leg_above, ' +
      'left_leg_above = :left_leg_above, ' +
      'escaped = :escaped, ' +
      'needs_review = :needs_review, ' +
      'notes = :notes, ' +
      'exported_status = :exported_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'')');
    Add('WHERE (capture_id = :capture_id)');

    R.FullName := GetCaptureFullname(R.CaptureDate, R.TaxonId, R.BandId, SEXES[R.SubjectSex],
      CAPTURE_TYPES[R.CaptureType], R.CycleCode, False);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetDateParam(ParamByName('capture_date'), R.CaptureDate);
    SetTimeParam(ParamByName('capture_time'), R.CaptureTime);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetForeignParam(ParamByName('net_station_id'), R.NetStationId);
    SetForeignParam(ParamByName('net_id'), R.NetId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('bander_id'), R.BanderId);
    SetForeignParam(ParamByName('annotator_id'), R.AnnotatorId);
    SetStrParam(ParamByName('subject_status'), SUBJECT_STATUSES[R.SubjectStatus]);
    SetStrParam(ParamByName('capture_type'), CAPTURE_TYPES[R.CaptureType]);
    SetStrParam(ParamByName('subject_sex'), SEXES[R.SubjectSex]);
    SetStrParam(ParamByName('how_sexed'), R.HowSexed);
    SetForeignParam(ParamByName('band_id'), R.BandId);
    SetFloatParam(ParamByName('weight'), R.Weight);
    SetFloatParam(ParamByName('tarsus_length'), R.TarsusLength);
    SetFloatParam(ParamByName('tarsus_diameter'), R.TarsusDiameter);
    SetFloatParam(ParamByName('exposed_culmen'), R.ExposedCulmen);
    SetFloatParam(ParamByName('bill_width'), R.BillWidth);
    SetFloatParam(ParamByName('bill_height'), R.BillHeight);
    SetFloatParam(ParamByName('nostril_bill_tip'), R.NostrilBillTip);
    SetFloatParam(ParamByName('skull_length'), R.SkullLength);
    SetFloatParam(ParamByName('right_wing_chord'), R.RightWingChord);
    SetFloatParam(ParamByName('first_secondary_chord'), R.FirstSecondaryChord);
    SetFloatParam(ParamByName('tail_length'), R.TailLength);
    SetStrParam(ParamByName('fat'), R.Fat);
    SetStrParam(ParamByName('brood_patch'), R.BroodPatch);
    SetStrParam(ParamByName('cloacal_protuberance'), R.CloacalProtuberance);
    SetStrParam(ParamByName('body_molt'), R.BodyMolt);
    SetStrParam(ParamByName('flight_feathers_molt'), R.FlightFeathersMolt);
    SetStrParam(ParamByName('flight_feathers_wear'), R.FlightFeathersWear);
    SetStrParam(ParamByName('molt_limits'), R.MoltLimits);
    SetStrParam(ParamByName('cycle_code'), R.CycleCode);
    SetStrParam(ParamByName('subject_age'), AGES[R.SubjectAge]);
    SetStrParam(ParamByName('how_aged'), R.HowAged);
    SetStrParam(ParamByName('skull_ossification'), R.SkullOssification);
    SetFloatParam(ParamByName('kipps_index'), R.KippsIndex);
    SetFloatParam(ParamByName('glucose'), R.Glucose);
    SetFloatParam(ParamByName('hemoglobin'), R.Hemoglobin);
    SetFloatParam(ParamByName('hematocrit'), R.Hematocrit);
    ParamByName('blood_sample').AsBoolean := R.BloodSample;
    ParamByName('feather_sample').AsBoolean := R.FeatherSample;
    SetForeignParam(ParamByName('photographer_1_id'), R.Photographer1Id);
    SetForeignParam(ParamByName('photographer_2_id'), R.Photographer2Id);
    if (R.Photographer1Id > 0) then
      R.SubjectPhotographed := True;
    ParamByName('subject_photographed').AsBoolean := R.SubjectPhotographed;
    SetStrParam(ParamByName('start_photo_number'), R.StartPhotoNumber);
    SetStrParam(ParamByName('end_photo_number'), R.EndPhotoNumber);
    SetStrParam(ParamByName('camera_name'), R.CameraName);
    SetForeignParam(ParamByName('removed_band_id'), R.RemovedBandId);
    SetStrParam(ParamByName('right_leg_below'), R.RightLegBelow);
    SetStrParam(ParamByName('left_leg_below'), R.LeftLegBelow);
    ParamByName('escaped').AsBoolean := R.Escaped;
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('capture_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TIndividual }

constructor TIndividual.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TIndividual.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TIndividual then
  begin
    FFullName := TIndividual(Source).FullName;
    FTaxonId := TIndividual(Source).TaxonId;
    FSex := TIndividual(Source).Sex;
    FAge := TIndividual(Source).Age;
    FNestId := TIndividual(Source).NestId;
    FBirthDate := TIndividual(Source).BirthDate;
    FBirthDay := TIndividual(Source).BirthDay;
    FBirthMonth := TIndividual(Source).BirthMonth;
    FBirthYear := TIndividual(Source).BirthYear;
    FBandingDate := TIndividual(Source).BandingDate;
    FBandChangeDate := TIndividual(Source).BandChangeDate;
    FBandId := TIndividual(Source).BandId;
    FDoubleBandId := TIndividual(Source).DoubleBandId;
    FRemovedBandId := TIndividual(Source).RemovedBandId;
    FRightLegBelow := TIndividual(Source).RightLegBelow;
    FLeftLegBelow := TIndividual(Source).LeftLegBelow;
    FRightLegAbove := TIndividual(Source).RightLegAbove;
    FLeftLegAbove := TIndividual(Source).LeftLegAbove;
    FFatherId := TIndividual(Source).FatherId;
    FMotherId := TIndividual(Source).MotherId;
    FDeathDate := TIndividual(Source).DeathDate;
    FDeathDay := TIndividual(Source).DeathDay;
    FDeathMonth := TIndividual(Source).DeathMonth;
    FDeathYear := TIndividual(Source).DeathYear;
    FRecognizableMarkings := TIndividual(Source).RecognizableMarkings;
    FNotes := TIndividual(Source).Notes;
  end;
end;

procedure TIndividual.Clear;
begin
  inherited;
  FFullName := EmptyStr;
  FTaxonId := 0;
  FSex := sexUnknown;
  FAge := ageUnknown;
  FNestId := 0;
  FBirthDate := EmptyStr;
  FBirthDay := 0;
  FBirthMonth := 0;
  FBirthYear := 0;
  FBandingDate := NullDate;
  FBandChangeDate := NullDate;
  FBandId := 0;
  FDoubleBandId := 0;
  FRemovedBandId := 0;
  FRightLegBelow := EmptyStr;
  FLeftLegBelow := EmptyStr;
  FRightLegAbove := EmptyStr;
  FLeftLegAbove := EmptyStr;
  FFatherId := 0;
  FMotherId := 0;
  FDeathDate := EmptyStr;
  FDeathDay := 0;
  FDeathMonth := 0;
  FDeathYear := 0;
  FRecognizableMarkings := EmptyStr;
  FNotes := EmptyStr;
end;

function TIndividual.Clone: TXolmisRecord;
begin
  Result := TIndividual(inherited Clone);
end;

function TIndividual.Diff(const aOld: TIndividual; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSex, aOld.Sex, FSex, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAge, aOld.Age, FAge, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBirthDate, aOld.BirthDate, FBirthDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBirthDay, aOld.BirthDay, FBirthDay, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBirthMonth, aOld.BirthMonth, FBirthMonth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBirthYear, aOld.BirthYear, FBirthYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBandingDate, aOld.BandingDate, FBandingDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBandChangeDate, aOld.BandChangeDate, FBandChangeDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBandID, aOld.BandId, FBandId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDoubleBandID, aOld.DoubleBandId, FDoubleBandId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRemovedBandID, aOld.RemovedBandId, FRemovedBandId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRightTarsus, aOld.RightLegBelow, FRightLegBelow, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLeftTarsus, aOld.LeftLegBelow, FLeftLegBelow, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRightTibia, aOld.RightLegAbove, FRightLegAbove, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLeftTibia, aOld.LeftLegAbove, FLeftLegAbove, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFatherID, aOld.FatherId, FFatherId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMotherID, aOld.MotherId, FMotherId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDeathDate, aOld.DeathDate, FDeathDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDeathDay, aOld.DeathDay, FDeathDay, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDeathMonth, aOld.DeathMonth, FDeathMonth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDeathYear, aOld.DeathYear, FDeathYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRecognizableMarkings, aOld.RecognizableMarkings, FRecognizableMarkings, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TIndividual.EqualsTo(const Other: TIndividual): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TIndividual.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName     := Obj.Get('full_name', '');
    FTaxonId      := Obj.Get('taxon_id', 0);
    case Obj.Get('sex', '') of
      'U': FSex := sexUnknown;
      'F': FSex := sexFemale;
      'M': FSex := sexMale;
    else
      FSex := sexUnknown;
    end;
    case Obj.Get('age', '') of
      'U': FAge := ageUnknown;
      'N': FAge := ageNestling;
      'F': FAge := ageFledgling;
      'J': FAge := ageJuvenile;
      'A': FAge := ageAdult;
      'Y': FAge := ageFirstYear;
      'S': FAge := ageSecondYear;
      'T': FAge := ageThirdYear;
      '4': FAge := ageFourthYear;
      '5': FAge := ageFifthYear;
    else
      FAge := ageUnknown;
    end;
    FNestId         := Obj.Get('nest_id', 0);
    FBirthYear      := Obj.Get('birth_year', 0);
    FBirthMonth     := Obj.Get('birth_month', 0);
    FBirthDay       := Obj.Get('birth_day', 0);
    FBandingDate    := Obj.Get('banding_date', NullDate);
    FBandChangeDate := Obj.Get('band_change_date', NullDate);
    FBandId         := Obj.Get('band_id', 0);
    FDoubleBandId   := Obj.Get('double_band_id', 0);
    FRemovedBandId  := Obj.Get('removed_band_id', 0);
    FRightLegBelow  := Obj.Get('right_tarsus', '');
    FLeftLegBelow   := Obj.Get('left_tarsus', '');
    FRightLegAbove  := Obj.Get('right_tibia', '');
    FLeftLegAbove   := Obj.Get('left_tibia', '');
    FFatherId       := Obj.Get('father_id', 0);
    FMotherId       := Obj.Get('mother_id', 0);
    FDeathYear      := Obj.Get('death_year', 0);
    FDeathMonth     := Obj.Get('death_month', 0);
    FDeathDay       := Obj.Get('death_day', 0);
    FRecognizableMarkings := Obj.Get('recognizable_markings', '');
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TIndividual.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('sex', SEXES[FSex]);
    JSONObject.Add('age', AGES[FAge]);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('birth_year', FBirthYear);
    JSONObject.Add('birth_month', FBirthMonth);
    JSONObject.Add('birth_day', FBirthDay);
    JSONObject.Add('banding_date', FBandingDate);
    JSONObject.Add('band_change_date', FBandChangeDate);
    JSONObject.Add('band_id', FBandId);
    JSONObject.Add('double_band_id', FDoubleBandId);
    JSONObject.Add('removed_band_id', FRemovedBandId);
    JSONObject.Add('right_tarsus', FRightLegBelow);
    JSONObject.Add('left_tarsus', FLeftLegBelow);
    JSONObject.Add('right_tibia', FRightLegAbove);
    JSONObject.Add('left_tibia', FLeftLegAbove);
    JSONObject.Add('father_id', FFatherId);
    JSONObject.Add('mother_id', FMotherId);
    JSONObject.Add('death_year', FDeathYear);
    JSONObject.Add('death_month', FDeathMonth);
    JSONObject.Add('death_day', FDeathDay);
    JSONObject.Add('recognizable_markings', FRecognizableMarkings);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TIndividual.ToString: String;
begin
  Result := Format('Individual(Id=%d, FullName=%s, TaxonId=%d, Sex=%s, Age=%s, NestId=%d, BirthYear=%d, ' +
    'BirthMonth=%d, BirthDay=%d, BandingDate=%s, BandChangeDate=%s, BandId=%d, DoubleBandId=%d, RemovedBandId=%d, ' +
    'RightTarsus=%s, LeftTarsus=%s, RightTibia=%s, LeftTibia=%s, FatherId=%d, MotherId=%d, DeathYear=%d, ' +
    'DeathMonth=%d, DeathDay=%d, RecognizableMarkings=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FTaxonId, SEXES[FSex], AGES[FAge], FNestId, FBirthYear, FBirthMonth, FBirthDay,
    DateToStr(FBandingDate), DateToStr(FBandChangeDate), FBandId, FDoubleBandId, FRemovedBandId,
    FRightLegBelow, FLeftLegBelow, FRightLegAbove, FLeftLegAbove, FFatherId, FMotherId, FDeathYear,
    FDeathMonth, FDeathDay, FRecognizableMarkings, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TIndividual.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'Taxon required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TIndividualRepository }

procedure TIndividualRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TIndividual;
begin
  if not (E is TIndividual) then
    raise Exception.Create('Delete: Expected TIndividual');

  R := TIndividual(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TIndividualRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_INDIVIDUAL_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TIndividualRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_INDIVIDUAL_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TIndividualRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_INDIVIDUAL_ID, COL_FULL_NAME, COL_BAND_ID); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TIndividual) then
    raise Exception.Create('FindBy: Expected TIndividual');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT ' +
      'individual_id, ' +
      'formatted_name, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_sex, ' +
      'individual_age, ' +
      'nest_id, ' +
      'birth_date, ' +
      'birth_day, ' +
      'birth_month, ' +
      'birth_year, ' +
      'banding_date, ' +
      'band_change_date, ' +
      'band_id, ' +
      'double_band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'father_id, ' +
      'mother_id, ' +
      'death_date, ' +
      'death_day, ' +
      'death_month, ' +
      'death_year, ' +
      'recognizable_markings, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'queued_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM individuals');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TIndividual(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TIndividualRepository.FindByBand(const aTaxon, aBand: Integer; aRightLeg: String; aLeftLeg: String;
  E: TIndividual);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM individuals');
    Add('WHERE (taxon_id = :taxon_id)');
    Add('AND (band_id = :band_id)');
    if (aRightLeg <> EmptyStr) then
    begin
      Add('AND (right_leg_below = :right_tarsus)');
      ParamByName('right_tarsus').AsString := aRightLeg;
    end;
    if (aLeftLeg <> EmptyStr) then
    begin
      Add('AND (left_leg_below = :left_tarsus)');
      ParamByName('left_tarsus').AsString := aLeftLeg;
    end;
    ParamByName('taxon_id').AsInteger := aTaxon;
    ParamByName('band_id').AsInteger := aBand;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TIndividualRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TIndividual) then
    raise Exception.Create('GetById: Expected TIndividual');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'individual_id, ' +
      'formatted_name, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_sex, ' +
      'individual_age, ' +
      'nest_id, ' +
      'birth_date, ' +
      'birth_day, ' +
      'birth_month, ' +
      'birth_year, ' +
      'banding_date, ' +
      'band_change_date, ' +
      'band_id, ' +
      'double_band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'father_id, ' +
      'mother_id, ' +
      'death_date, ' +
      'death_day, ' +
      'death_month, ' +
      'death_year, ' +
      'recognizable_markings, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'queued_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM individuals');
    Add('WHERE individual_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TIndividual(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TIndividualRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TIndividual;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TIndividual) then
    raise Exception.Create('Hydrate: Expected TIndividual');

  R := TIndividual(E);
  with aDataSet do
  begin
    R.Id := FieldByName('individual_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    case FieldByName('individual_sex').AsString of
      'M': R.Sex := sexMale;
      'F': R.Sex := sexFemale;
    else
      R.Sex := sexUnknown;
    end;
    case FieldByName('individual_age').AsString of
      'N': R.Age := ageNestling;
      'F': R.Age := ageFledgling;
      'J': R.Age := ageJuvenile;
      'A': R.Age := ageAdult;
      'Y': R.Age := ageFirstYear;
      'S': R.Age := ageSecondYear;
      'T': R.Age := ageThirdYear;
      '4': R.Age := ageFourthYear;
      '5': R.Age := ageFifthYear;
    else
      R.Age := ageUnknown;
    end;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.BirthDate := FieldByName('birth_date').AsString;
    R.BirthDay := FieldByName('birth_day').AsInteger;
    R.BirthMonth := FieldByName('birth_month').AsInteger;
    R.BirthYear := FieldByName('birth_year').AsInteger;
    if not FieldByName('banding_date').IsNull then
      R.BandingDate := FieldByName('banding_date').AsDateTime
    else
      R.BandingDate := NullDate;
    if not FieldByName('band_change_date').IsNull then
      R.BandChangeDate := FieldByName('band_change_date').AsDateTime
    else
      R.BandChangeDate := NullDate;
    R.BandId := FieldByName('band_id').AsInteger;
    R.DoubleBandId := FieldByName('double_band_id').AsInteger;
    R.RemovedBandId := FieldByName('removed_band_id').AsInteger;
    R.RightLegBelow := FieldByName('right_leg_below').AsString;
    R.LeftLegBelow := FieldByName('left_leg_below').AsString;
    R.RightLegAbove := FieldByName('right_leg_above').AsString;
    R.LeftLegAbove := FieldByName('left_leg_above').AsString;
    R.FatherId := FieldByName('father_id').AsInteger;
    R.MotherId := FieldByName('mother_id').AsInteger;
    R.DeathDate := FieldByName('death_date').AsString;
    R.DeathDay := FieldByName('death_day').AsInteger;
    R.DeathMonth := FieldByName('death_month').AsInteger;
    R.DeathYear := FieldByName('death_year').AsInteger;
    R.RecognizableMarkings := FieldByName('recognizable_markings').AsString;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TIndividualRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TIndividual;
  Birth, Death: TPartialDate;
begin
  if not (E is TIndividual) then
    raise Exception.Create('Insert: Expected TIndividual');

  R := TIndividual(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO individuals (' +
      'taxon_id, ' +
      'individual_sex, ' +
      'individual_age, ' +
      'nest_id, ' +
      'birth_date, ' +
      'birth_day, ' +
      'birth_month, ' +
      'birth_year, ' +
      'banding_date, ' +
      'band_change_date, ' +
      'band_id, ' +
      'double_band_id, ' +
      'removed_band_id, ' +
      'right_leg_below, ' +
      'left_leg_below, ' +
      'right_leg_above, ' +
      'left_leg_above, ' +
      'father_id, ' +
      'mother_id, ' +
      'death_date, ' +
      'death_day, ' +
      'death_month, ' +
      'death_year, ' +
      'recognizable_markings, ' +
      'notes, ' +
      'formatted_name, ' +
      'full_name, ' +
      'user_inserted, ' +
      'insert_date)');
    Add('VALUES (' +
      ':taxon_id, ' +
      ':individual_sex, ' +
      ':individual_age, ' +
      ':nest_id, ' +
      ':birth_date, ' +
      ':birth_day, ' +
      ':birth_month, ' +
      ':birth_year, ' +
      'date(:banding_date), ' +
      'date(:band_change_date), ' +
      ':band_id, ' +
      ':double_band_id, ' +
      ':removed_band_id, ' +
      ':right_leg_below, ' +
      ':left_leg_below, ' +
      ':right_leg_above, ' +
      ':left_leg_above, ' +
      ':father_id, ' +
      ':mother_id, ' +
      ':death_date, ' +
      ':death_day, ' +
      ':death_month, ' +
      ':death_year, ' +
      ':recognizable_markings, ' +
      ':notes, ' +
      ':formatted_name, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetStrParam(ParamByName('individual_sex'), SEXES[R.Sex]);
    SetStrParam(ParamByName('individual_age'), AGES[R.Age]);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    if R.BirthYear > 0 then
    begin
      ParamByName('birth_year').AsInteger := R.BirthYear;
      ParamByName('birth_month').AsInteger := R.BirthMonth;
      ParamByName('birth_day').AsInteger := R.BirthDay;
      Birth.Encode(R.BirthYear, R.BirthMonth, R.BirthDay, '.');
      SetStrParam(ParamByName('birth_date'), Birth.ToString);
    end
    else
    begin
      ParamByName('birth_year').Clear;
      ParamByName('birth_month').Clear;
      ParamByName('birth_day').Clear;
      ParamByName('birth_date').Clear;
    end;
    SetForeignParam(ParamByName('band_id'), R.BandId);
    SetForeignParam(ParamByName('double_band_id'), R.DoubleBandId);
    SetForeignParam(ParamByName('removed_band_id'), R.RemovedBandId);
    SetDateParam(ParamByName('banding_date'), R.BandingDate);
    SetDateParam(ParamByName('band_change_date'), R.BandChangeDate);
    SetStrParam(ParamByName('recognizable_markings'), R.RecognizableMarkings);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetForeignParam(ParamByName('father_id'), R.FatherId);
    SetForeignParam(ParamByName('mother_id'), R.MotherId);
    if R.DeathYear > 0 then
    begin
      ParamByName('death_year').AsInteger := R.DeathYear;
      ParamByName('death_month').AsInteger := R.DeathMonth;
      ParamByName('death_day').AsInteger := R.DeathDay;
      Death.Encode(R.DeathYear, R.DeathMonth, R.DeathDay, '.');
      SetStrParam(ParamByName('death_date'), Death.ToString);
    end
    else
    begin
      ParamByName('death_year').Clear;
      ParamByName('death_month').Clear;
      ParamByName('death_day').Clear;
      ParamByName('death_date').Clear;
    end;
    SetStrParam(ParamByName('formatted_name'), GetIndividualFullname(R.TaxonId, R.BandId, R.RightLegBelow, R.LeftLegBelow, SEXES[R.Sex], True));
    R.FullName := GetIndividualFullname(R.TaxonId, R.BandId, R.RightLegBelow, R.LeftLegBelow, SEXES[R.Sex], False);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('right_leg_below'), R.RightLegBelow);
    SetStrParam(ParamByName('left_leg_below'), R.LeftLegBelow);
    SetStrParam(ParamByName('right_leg_above'), R.RightLegAbove);
    SetStrParam(ParamByName('left_leg_above'), R.LeftLegAbove);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TIndividualRepository.TableName: string;
begin
  Result := TBL_INDIVIDUALS;
end;

procedure TIndividualRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TIndividual;
  Birth, Death: TPartialDate;
begin
  if not (E is TIndividual) then
    raise Exception.Create('Update: Expected TIndividual');

  R := TIndividual(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TIndividualRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE individuals SET ' +
      'taxon_id = :taxon_id, ' +
      'individual_sex = :individual_sex, ' +
      'individual_age = :individual_age, ' +
      'nest_id = :nest_id, ' +
      'birth_date = :birth_date, ' +
      'birth_day = :birth_day, ' +
      'birth_month = :birth_month, ' +
      'birth_year = :birth_year, ' +
      'banding_date = date(:banding_date), ' +
      'band_change_date = date(:band_change_date), ' +
      'band_id = :band_id, ' +
      'double_band_id = :double_band_id, ' +
      'removed_band_id = :removed_band_id, ' +
      'right_leg_below = :right_leg_below, ' +
      'left_leg_below = :left_leg_below, ' +
      'right_leg_above = :right_leg_above, ' +
      'left_leg_above = :left_leg_above, ' +
      'father_id = :father_id, ' +
      'mother_id = :mother_id, ' +
      'death_date = :death_date, ' +
      'death_day = :death_day, ' +
      'death_month = :death_month, ' +
      'death_year = :death_year, ' +
      'recognizable_markings = :recognizable_markings, ' +
      'notes = :notes, ' +
      'formatted_name = :formatted_name, ' +
      'full_name = :full_name, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'')');
    Add('WHERE (individual_id = :individual_id)');

    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetStrParam(ParamByName('individual_sex'), SEXES[R.Sex]);
    SetStrParam(ParamByName('individual_age'), AGES[R.Age]);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    if R.BirthYear > 0 then
    begin
      ParamByName('birth_year').AsInteger := R.BirthYear;
      ParamByName('birth_month').AsInteger := R.BirthMonth;
      ParamByName('birth_day').AsInteger := R.BirthDay;
      Birth.Encode(R.BirthYear, R.BirthMonth, R.BirthDay, '.');
      SetStrParam(ParamByName('birth_date'), Birth.ToString);
    end
    else
    begin
      ParamByName('birth_year').Clear;
      ParamByName('birth_month').Clear;
      ParamByName('birth_day').Clear;
      ParamByName('birth_date').Clear;
    end;
    SetForeignParam(ParamByName('band_id'), R.BandId);
    SetForeignParam(ParamByName('double_band_id'), R.DoubleBandId);
    SetForeignParam(ParamByName('removed_band_id'), R.RemovedBandId);
    SetDateParam(ParamByName('banding_date'), R.BandingDate);
    SetDateParam(ParamByName('band_change_date'), R.BandChangeDate);
    SetStrParam(ParamByName('recognizable_markings'), R.RecognizableMarkings);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetForeignParam(ParamByName('father_id'), R.FatherId);
    SetForeignParam(ParamByName('mother_id'), R.MotherId);
    if R.DeathYear > 0 then
    begin
      ParamByName('death_year').AsInteger := R.DeathYear;
      ParamByName('death_month').AsInteger := R.DeathMonth;
      ParamByName('death_day').AsInteger := R.DeathDay;
      Death.Encode(R.DeathYear, R.DeathMonth, R.DeathDay, '.');
      SetStrParam(ParamByName('death_date'), Death.ToString);
    end
    else
    begin
      ParamByName('death_year').Clear;
      ParamByName('death_month').Clear;
      ParamByName('death_day').Clear;
      ParamByName('death_date').Clear;
    end;
    SetStrParam(ParamByName('formatted_name'), GetIndividualFullname(R.TaxonId, R.BandId, R.RightLegBelow, R.LeftLegBelow, SEXES[R.Sex], True));
    R.FullName := GetIndividualFullname(R.TaxonId, R.BandId, R.RightLegBelow, R.LeftLegBelow, SEXES[R.Sex], False);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('right_leg_below'), R.RightLegBelow);
    SetStrParam(ParamByName('left_leg_below'), R.LeftLegBelow);
    SetStrParam(ParamByName('right_leg_above'), R.RightLegAbove);
    SetStrParam(ParamByName('left_leg_above'), R.LeftLegAbove);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('individual_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

