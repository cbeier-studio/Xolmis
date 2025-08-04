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
  Classes, SysUtils, DB, SQLDB, fgl, fpjson, DateUtils, models_record_types;

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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aTaxon, aBand: Integer; aRightLeg: String = ''; aLeftLeg: String = ''): Boolean;
    function Diff(aOld: TIndividual; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TIndividual);
    function ToJSON: String;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    procedure Update;
    function Find(aTaxon, aBand: Integer; aCaptureType, aDate, aTime: String): Boolean;
    function Diff(aOld: TCapture; var aList: TStrings): Boolean;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TCapture);
    function ToJSON: String;
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
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TFeather; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TFeather);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
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
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  utils_system, utils_global, models_users, utils_validations, utils_fullnames, data_columns, data_setparam, data_getvalue,
  utils_locale, udm_main;

{ TFeather }

constructor TFeather.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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
  FNotes := EmptyStr;
end;

procedure TFeather.Copy(aFrom: TFeather);
begin
  FSampleDate := aFrom.SampleDate;
  FSampleTime := aFrom.SampleTime;
  FTaxonId := aFrom.TaxonId;
  FLocalityId := aFrom.LocalityId;
  FIndividualId := aFrom.IndividualId;
  FCaptureId := aFrom.CaptureId;
  FSightingId := aFrom.SightingId;
  FObserverId := aFrom.ObserverId;
  FSourceType := aFrom.SourceType;
  FSymmetrical := aFrom.Symmetrical;
  FFeatherTrait := aFrom.FeatherTrait;
  FFeatherNumber := aFrom.FeatherNumber;
  FBodySide := aFrom.BodySide;
  FPercentGrown := aFrom.PercentGrown;
  FFeatherLength := aFrom.FeatherLength;
  FFeatherArea := aFrom.FeatherArea;
  FFeatherMass := aFrom.FeatherMass;
  FRachisWidth := aFrom.RachisWidth;
  FGrowthBarWidth := aFrom.GrowthBarWidth;
  FBarbDensity := aFrom.BarbDensity;
  FFeatherAge := aFrom.FeatherAge;
  FNotes := aFrom.Notes;
end;

procedure TFeather.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TFeather.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('DELETE FROM feathers');
      Add('WHERE (feather_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TFeather.Diff(aOld: TFeather; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSource, aOld.SourceType, FSourceType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSymmetry, aOld.Symmetrical, FSymmetrical, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFeatherTrait, aOld.FeatherTrait, FFeatherTrait, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFeatherNumber, aOld.FeatherNumber, FFeatherNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBodySide, aOld.BodySide, FBodySide, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPercentGrown, aOld.PercentGrown, FPercentGrown, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLength, aOld.FeatherLength, FFeatherLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscArea, aOld.FeatherArea, FFeatherArea, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMass, aOld.FeatherMass, FFeatherMass, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRachisWidth, aOld.RachisWidth, FRachisWidth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGrowthBarWidth, aOld.GrowthBarWidth, FGrowthBarWidth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBarbDensity, aOld.BarbDensity, FBarbDensity, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAge, aOld.FeatherAge, FFeatherAge, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TFeather.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TFeather.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TFeather.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('capture_id'), FCaptureId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      ParamByName('source_type').AsString := FEATHER_DATA_SOURCES[FSourceType];
      ParamByName('symmetrical').AsString := SYMMETRIES[FSymmetrical];
      ParamByName('feather_trait').AsString := FEATHER_TRAITS[FFeatherTrait];
      SetIntParam(ParamByName('feather_number'), FFeatherNumber);
      ParamByName('body_side').AsString := BODY_SIDES[FBodySide];
      ParamByName('grown_percent').AsFloat := FPercentGrown;
      SetFloatParam(ParamByName('feather_length'), FFeatherLength);
      SetFloatParam(ParamByName('feather_area'), FFeatherArea);
      SetFloatParam(ParamByName('feather_mass'), FFeatherMass);
      SetFloatParam(ParamByName('rachis_width'), FRachisWidth);
      SetFloatParam(ParamByName('growth_bar_width'), FGrowthBarWidth);
      SetFloatParam(ParamByName('barb_density'), FBarbDensity);
      ParamByName('feather_age').AsString := FEATHER_AGES[FFeatherAge];
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TFeather.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('feather_id').AsInteger;
    FSampleDate := FieldByName('sample_date').AsDateTime;
    FSampleTime := FieldByName('sample_time').AsDateTime;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FCaptureId := FieldByName('capture_id').AsInteger;
    FSightingId := FieldByName('sighting_id').AsInteger;
    FObserverId := FieldByName('observer_id').AsInteger;
    case FieldByName('source_type').AsString of
      'U': FSourceType := fdsUnknown;
      'C': FSourceType := fdsCapture;
      'S': FSourceType := fdsSighting;
      'P': FSourceType := fdsPhoto;
    else
      FSourceType := fdsUnknown;
    end;
    case FieldByName('symmetrical').AsString of
      'U': FSymmetrical := symUnknown;
      'S': FSymmetrical := symSymmetrical;
      'A': FSymmetrical := symAsymmetrical;
    else
      FSymmetrical := symUnknown;
    end;
    case FieldByName('feather_trait').AsString of
      'B': FFeatherTrait := ftrBody;
      'P': FFeatherTrait := ftrPrimary;
      'S': FFeatherTrait := ftrSecondary;
      'R': FFeatherTrait := ftrRectrix;
      'PC': FFeatherTrait := ftrPrimaryCovert;
      'GC': FFeatherTrait := ftrGreatCovert;
      'MC': FFeatherTrait := ftrMedianCovert;
      'LC': FFeatherTrait := ftrLesserCovert;
      'CC': FFeatherTrait := ftrCarpalCovert;
      'AL': FFeatherTrait := ftrAlula;
    end;
    FFeatherNumber := FieldByName('feather_number').AsInteger;
    case FieldByName('body_side').AsString of
      'NA': FBodySide := bsdNotApplicable;
      'R': FBodySide := bsdRight;
      'L': FBodySide := bsdLeft;
    else
      FBodySide := bsdNotApplicable;
    end;
    FPercentGrown := FieldByName('grown_percent').AsFloat;
    FFeatherLength := FieldByName('feather_length').AsFloat;
    FFeatherArea := FieldByName('feather_area').AsFloat;
    FFeatherMass := FieldByName('feather_mass').AsFloat;
    FRachisWidth := FieldByName('rachis_width').AsFloat;
    FGrowthBarWidth := FieldByName('growth_bar_width').AsFloat;
    FBarbDensity := FieldByName('barb_density').AsFloat;
    case FieldByName('feather_age').AsString of
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
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), FInsertDate);
    GetTimeStamp(FieldByName('update_date'), FUpdateDate);
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TFeather.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TFeather.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Date', FSampleDate);
    JSONObject.Add('Time', FSampleTime);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Capture', FCaptureId);
    JSONObject.Add('Sighting', FSightingId);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Source', FEATHER_DATA_SOURCES[FSourceType]);
    JSONObject.Add('Symmetry', SYMMETRIES[FSymmetrical]);
    JSONObject.Add('Feather trait', FEATHER_TRAITS[FFeatherTrait]);
    JSONObject.Add('Feather number', FFeatherNumber);
    JSONObject.Add('Body side', BODY_SIDES[FBodySide]);
    JSONObject.Add('Percent grown', FPercentGrown);
    JSONObject.Add('Length', FFeatherLength);
    JSONObject.Add('Area', FFeatherArea);
    JSONObject.Add('Mass', FFeatherMass);
    JSONObject.Add('Rachis width', FRachisWidth);
    JSONObject.Add('Growth bar width', FGrowthBarWidth);
    JSONObject.Add('Barb density', FBarbDensity);
    JSONObject.Add('Age', FEATHER_AGES[FFeatherAge]);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TFeather.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TFeather.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
        'notes = :notes, ' +
        'exported_status = :exported_status, ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (feather_id = :feather_id)');

      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('capture_id'), FCaptureId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      ParamByName('source_type').AsString := FEATHER_DATA_SOURCES[FSourceType];
      ParamByName('symmetrical').AsString := SYMMETRIES[FSymmetrical];
      ParamByName('feather_trait').AsString := FEATHER_TRAITS[FFeatherTrait];
      SetIntParam(ParamByName('feather_number'), FFeatherNumber);
      ParamByName('body_side').AsString := BODY_SIDES[FBodySide];
      ParamByName('grown_percent').AsFloat := FPercentGrown;
      SetFloatParam(ParamByName('feather_length'), FFeatherLength);
      SetFloatParam(ParamByName('feather_area'), FFeatherArea);
      SetFloatParam(ParamByName('feather_mass'), FFeatherMass);
      SetFloatParam(ParamByName('rachis_width'), FRachisWidth);
      SetFloatParam(ParamByName('growth_bar_width'), FGrowthBarWidth);
      SetFloatParam(ParamByName('barb_density'), FBarbDensity);
      ParamByName('feather_age').AsString := FEATHER_AGES[FFeatherAge];
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('exported_status').AsBoolean := FExported;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('feather_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TCapture }

constructor TCapture.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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
  //FOldMolt := EmptyStr;
  //FOldPrimariesMolt := EmptyStr;
  //FOldSecondariesMolt := EmptyStr;
  //FOldRetricesMolt := EmptyStr;
  //FOldBodyMolt := EmptyStr;
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

procedure TCapture.Copy(aFrom: TCapture);
begin
  FFullName := aFrom.FullName;
  FSurveyId := aFrom.SurveyId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FProjectId := aFrom.ProjectId;
  FCaptureDate := aFrom.CaptureDate;
  FCaptureTime := aFrom.CaptureTime;
  FLocalityId := aFrom.LocalityId;
  FNetStationId := aFrom.NetStationId;
  FNetId := aFrom.NetId;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FBanderId := aFrom.BanderId;
  FAnnotatorId := aFrom.AnnotatorId;
  FSubjectStatus := aFrom.SubjectStatus;
  FCaptureType := aFrom.CaptureType;
  FSubjectSex := aFrom.SubjectSex;
  FHowSexed := aFrom.HowSexed;
  FBandId := aFrom.BandId;
  FRemovedBandId := aFrom.RemovedBandId;
  FRightLegBelow := aFrom.RightLegBelow;
  FLeftLegBelow := aFrom.LeftLegBelow;
  FRightLegAbove := aFrom.RightLegAbove;
  FLeftLegAbove := aFrom.LeftLegAbove;
  FWeight := aFrom.Weight;
  FTarsusLength := aFrom.TarsusLength;
  FTarsusDiameter := aFrom.TarsusDiameter;
  FCulmenLength := aFrom.CulmenLength;
  FExposedCulmen := aFrom.ExposedCulmen;
  FBillWidth := aFrom.BillWidth;
  FBillHeight := aFrom.BillHeight;
  FNostrilBillTip := aFrom.NostrilBillTip;
  FSkullLength := aFrom.SkullLength;
  FHaluxLengthTotal := aFrom.HaluxLengthTotal;
  FHaluxLengthFinger := aFrom.HaluxLengthFinger;
  FHaluxLengthClaw := aFrom.HaluxLengthClaw;
  FRightWingChord := aFrom.RightWingChord;
  FFirstSecondaryChord := aFrom.FirstSecondaryChord;
  FTailLength := aFrom.TailLength;
  FCentralRetrixLength := aFrom.CentralRetrixLength;
  FExternalRetrixLength := aFrom.ExternalRetrixLength;
  FTotalLength := aFrom.TotalLength;
  FFeatherMites := aFrom.FeatherMites;
  FFat := aFrom.Fat;
  FBroodPatch := aFrom.BroodPatch;
  FCloacalProtuberance := aFrom.CloacalProtuberance;
  //FOldMolt := aFrom.OldMolt;
  //FOldPrimariesMolt := aFrom.OldPrimariesMolt;
  //FOldSecondariesMolt := aFrom.OldSecondariesMolt;
  //FOldRetricesMolt := aFrom.OldRetricesMolt;
  //FOldBodyMolt := aFrom.OldBodyMolt;
  FBodyMolt := aFrom.BodyMolt;
  FFlightFeathersMolt := aFrom.FlightFeathersMolt;
  FFlightFeathersWear := aFrom.FlightFeathersWear;
  FMoltLimits := aFrom.MoltLimits;
  FCycleCode := aFrom.CycleCode;
  FSubjectAge := aFrom.SubjectAge;
  FHowAged := aFrom.HowAged;
  FSkullOssification := aFrom.SkullOssification;
  FKippsIndex := aFrom.KippsIndex;
  FGlucose := aFrom.Glucose;
  FHemoglobin := aFrom.Hemoglobin;
  FHematocrit := aFrom.Hematocrit;
  FPhilornisLarvaeTally := aFrom.PhilornisLarvaeTally;
  FBloodSample := aFrom.BloodSample;
  FFeatherSample := aFrom.FeatherSample;
  FClawSample := aFrom.ClawSample;
  FFecesSample := aFrom.FecesSample;
  FParasiteSample := aFrom.ParasiteSample;
  FSubjectRecorded := aFrom.SubjectRecorded;
  FSubjectCollected := aFrom.SubjectCollected;
  FSubjectPhotographed := aFrom.SubjectPhotographed;
  FFieldNumber := aFrom.FieldNumber;
  FPhotographer1Id := aFrom.Photographer1Id;
  FPhotographer2Id := aFrom.Photographer2Id;
  FStartPhotoNumber := aFrom.StartPhotoNumber;
  FEndPhotoNumber := aFrom.EndPhotoNumber;
  FCameraName := aFrom.CameraName;
  FEscaped := aFrom.Escaped;
  FNeedsReview := aFrom.NeedsReview;
  FNotes := aFrom.Notes;
end;

procedure TCapture.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TCapture.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('DELETE FROM captures');
      Add('WHERE (capture_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCapture.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
      //'old_molt, ' +
      //'old_primaries_molt, ' +
      //'old_secondaries_molt, ' +
      //'old_retrices_molt, ' +
      //'old_body_molt, ' +
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCapture.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('capture_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FCaptureDate := FieldByName('capture_date').AsDateTime;
    FCaptureTime := FieldByName('capture_time').AsDateTime;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FNetStationId := FieldByName('net_station_id').AsInteger;
    FNetId := FieldByName('net_id').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FBanderId := FieldByName('bander_id').AsInteger;
    FAnnotatorId := FieldByName('annotator_id').AsInteger;
    case FieldByName('subject_status').AsString of
      'N': FSubjectStatus := sstNormal;
      'I': FSubjectStatus := sstInjured;
      'W': FSubjectStatus := sstWingSprain;
      'X': FSubjectStatus := sstStressed;
      'D': FSubjectStatus := sstDead;
    end;
    case FieldByName('capture_type').AsString of
      'N': FCaptureType := cptNew;
      'R': FCaptureType := cptRecapture;
      'S': FCaptureType := cptSameDay;
      'C': FCaptureType := cptChangeBand;
    else
      FCaptureType := cptUnbanded;
    end;
    case FieldByName('subject_sex').AsString of
      'M': FSubjectSex := sexMale;
      'F': FSubjectSex := sexFemale;
    else
      FSubjectSex := sexUnknown;
    end;
    FHowSexed := FieldByName('how_sexed').AsString;
    FBandId := FieldByName('band_id').AsInteger;
    FRemovedBandId := FieldByName('removed_band_id').AsInteger;
    FRightLegBelow := FieldByName('right_leg_below').AsString;
    FLeftLegBelow := FieldByName('left_leg_below').AsString;
    FRightLegAbove := FieldByName('right_leg_above').AsString;
    FLeftLegAbove := FieldByName('left_leg_above').AsString;
    FWeight := FieldByName('weight').AsFloat;
    FTarsusLength := FieldByName('tarsus_length').AsFloat;
    FTarsusDiameter := FieldByName('tarsus_diameter').AsFloat;
    FCulmenLength := FieldByName('culmen_length').AsFloat;
    FExposedCulmen := FieldByName('exposed_culmen').AsFloat;
    FBillWidth := FieldByName('bill_width').AsFloat;
    FBillHeight := FieldByName('bill_height').AsFloat;
    FNostrilBillTip := FieldByName('nostril_bill_tip').AsFloat;
    FSkullLength := FieldByName('skull_length').AsFloat;
    if FindField('halux_length_total') <> nil then
      FHaluxLengthTotal := FieldByName('halux_length_total').AsFloat;
    if FindField('halux_length_finger') <> nil then
      FHaluxLengthFinger := FieldByName('halux_length_finger').AsFloat;
    if FindField('halux_length_claw') <> nil then
      FHaluxLengthClaw := FieldByName('halux_length_claw').AsFloat;
    FRightWingChord := FieldByName('right_wing_chord').AsFloat;
    FFirstSecondaryChord := FieldByName('first_secondary_chord').AsFloat;
    FTailLength := FieldByName('tail_length').AsFloat;
    if FindField('central_retrix_length') <> nil then
      FCentralRetrixLength := FieldByName('central_retrix_length').AsFloat;
    if FindField('external_retrix_length') <> nil then
      FExternalRetrixLength := FieldByName('external_retrix_length').AsFloat;
    FTotalLength := FieldByName('total_length').AsFloat;
    if FindField('feather_mites') <> nil then
      FFeatherMites := FieldByName('feather_mites').AsString;
    FFat := FieldByName('fat').AsString;
    FBroodPatch := FieldByName('brood_patch').AsString;
    FCloacalProtuberance := FieldByName('cloacal_protuberance').AsString;
    //FOldMolt := FieldByName('old_molt').AsString;
    //FOldPrimariesMolt := FieldByName('old_primaries_molt').AsString;
    //FOldSecondariesMolt := FieldByName('old_secondaries_molt').AsString;
    //FOldRetricesMolt := FieldByName('old_retrices_molt').AsString;
    //FOldBodyMolt := FieldByName('old_body_molt').AsString;
    FBodyMolt := FieldByName('body_molt').AsString;
    FFlightFeathersMolt := FieldByName('flight_feathers_molt').AsString;
    FFlightFeathersWear := FieldByName('flight_feathers_wear').AsString;
    FMoltLimits := FieldByName('molt_limits').AsString;
    FCycleCode := FieldByName('cycle_code').AsString;
    case FieldByName('subject_age').AsString of
      'N': FSubjectAge := ageNestling;
      'F': FSubjectAge := ageFledgling;
      'J': FSubjectAge := ageJuvenile;
      'A': FSubjectAge := ageAdult;
      'Y': FSubjectAge := ageFirstYear;
      'S': FSubjectAge := ageSecondYear;
      'T': FSubjectAge := ageThirdYear;
      '4': FSubjectAge := ageFourthYear;
      '5': FSubjectAge := ageFifthYear;
    else
      FSubjectAge := ageUnknown;
    end;
    FHowAged := FieldByName('how_aged').AsString;
    FSkullOssification := FieldByName('skull_ossification').AsString;
    FKippsIndex := FieldByName('kipps_index').AsFloat;
    FGlucose := FieldByName('glucose').AsFloat;
    FHemoglobin := FieldByName('hemoglobin').AsFloat;
    FHematocrit := FieldByName('hematocrit').AsFloat;
    FPhilornisLarvaeTally := FieldByName('philornis_larvae_tally').AsInteger;
    FBloodSample := FieldByName('blood_sample').AsBoolean;
    FFeatherSample := FieldByName('feather_sample').AsBoolean;
    FClawSample := FieldByName('claw_sample').AsBoolean;
    FFecesSample := FieldByName('feces_sample').AsBoolean;
    FParasiteSample := FieldByName('parasite_sample').AsBoolean;
    FSubjectRecorded := FieldByName('subject_recorded').AsBoolean;
    FSubjectCollected := FieldByName('subject_collected').AsBoolean;
    FSubjectPhotographed := FieldByName('subject_photographed').AsBoolean;
    FFieldNumber := FieldByName('field_number').AsString;
    FPhotographer1Id := FieldByName('photographer_1_id').AsInteger;
    FPhotographer2Id := FieldByName('photographer_2_id').AsInteger;
    FStartPhotoNumber := FieldByName('start_photo_number').AsString;
    FEndPhotoNumber := FieldByName('end_photo_number').AsString;
    FCameraName := FieldByName('camera_name').AsString;
    FEscaped := FieldByName('escaped').AsBoolean;
    FNeedsReview := FieldByName('needs_review').AsBoolean;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), FInsertDate);
    GetTimeStamp(FieldByName('update_date'), FUpdateDate);
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TCapture.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      FFullName := GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, SEXES[FSubjectSex],
        CAPTURE_TYPES[FCaptureType], FCycleCode, False);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetDateParam(ParamByName('capture_date'), FCaptureDate);
      SetTimeParam(ParamByName('capture_time'), FCaptureTime);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetForeignParam(ParamByName('net_station_id'), FNetStationId);
      SetForeignParam(ParamByName('net_id'), FNetId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('bander_id'), FBanderId);
      SetForeignParam(ParamByName('annotator_id'), FAnnotatorId);
      SetStrParam(ParamByName('subject_status'), SUBJECT_STATUSES[FSubjectStatus]);
      SetStrParam(ParamByName('capture_type'), CAPTURE_TYPES[FCaptureType]);
      SetStrParam(ParamByName('subject_sex'), SEXES[FSubjectSex]);
      SetStrParam(ParamByName('how_sexed'), FHowSexed);
      SetForeignParam(ParamByName('band_id'), FBandId);
      SetFloatParam(ParamByName('weight'), FWeight);
      SetFloatParam(ParamByName('tarsus_length'), FTarsusLength);
      SetFloatParam(ParamByName('tarsus_diameter'), FTarsusDiameter);
      SetFloatParam(ParamByName('exposed_culmen'), FExposedCulmen);
      SetFloatParam(ParamByName('bill_width'), FBillWidth);
      SetFloatParam(ParamByName('bill_height'), FBillHeight);
      SetFloatParam(ParamByName('nostril_bill_tip'), FNostrilBillTip);
      SetFloatParam(ParamByName('skull_length'), FSkullLength);
      SetFloatParam(ParamByName('right_wing_chord'), FRightWingChord);
      SetFloatParam(ParamByName('first_secondary_chord'), FFirstSecondaryChord);
      SetFloatParam(ParamByName('tail_length'), FTailLength);
      SetStrParam(ParamByName('fat'), FFat);
      SetStrParam(ParamByName('brood_patch'), FBroodPatch);
      SetStrParam(ParamByName('cloacal_protuberance'), FCloacalProtuberance);
      SetStrParam(ParamByName('body_molt'), FBodyMolt);
      SetStrParam(ParamByName('flight_feathers_molt'), FFlightFeathersMolt);
      SetStrParam(ParamByName('flight_feathers_wear'), FFlightFeathersWear);
      SetStrParam(ParamByName('molt_limits'), FMoltLimits);
      SetStrParam(ParamByName('cycle_code'), FCycleCode);
      SetStrParam(ParamByName('subject_age'), AGES[FSubjectAge]);
      SetStrParam(ParamByName('how_aged'), FHowAged);
      SetStrParam(ParamByName('skull_ossification'), FSkullOssification);
      SetFloatParam(ParamByName('kipps_index'), FKippsIndex);
      SetFloatParam(ParamByName('glucose'), FGlucose);
      SetFloatParam(ParamByName('hemoglobin'), FHemoglobin);
      SetFloatParam(ParamByName('hematocrit'), FHematocrit);
      ParamByName('blood_sample').AsBoolean := FBloodSample;
      ParamByName('feather_sample').AsBoolean := FFeatherSample;
      SetForeignParam(ParamByName('photographer_1_id'), FPhotographer1Id);
      SetForeignParam(ParamByName('photographer_2_id'), FPhotographer2Id);
      if (FPhotographer1Id > 0) then
        FSubjectPhotographed := True;
      ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
      SetStrParam(ParamByName('start_photo_number'), FStartPhotoNumber);
      SetStrParam(ParamByName('end_photo_number'), FEndPhotoNumber);
      SetStrParam(ParamByName('camera_name'), FCameraName);
      SetForeignParam(ParamByName('removed_band_id'), FRemovedBandId);
      SetStrParam(ParamByName('right_leg_below'), FRightLegBelow);
      SetStrParam(ParamByName('left_leg_below'), FLeftLegBelow);
      ParamByName('escaped').AsBoolean := FEscaped;
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TCapture.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TCapture.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Date', FCaptureDate);
    JSONObject.Add('Time', FCaptureTime);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Net station', FNetStationId);
    JSONObject.Add('Net', FNetId);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Bander', FBanderId);
    JSONObject.Add('Annotator', FAnnotatorId);
    JSONObject.Add('Subject status', SUBJECT_STATUSES[FSubjectStatus]);
    JSONObject.Add('Type', CAPTURE_TYPES[FCaptureType]);
    JSONObject.Add('Sex', SEXES[FSubjectSex]);
    JSONObject.Add('How was sexed', FHowSexed);
    JSONObject.Add('Band', FBandId);
    JSONObject.Add('Weight', FWeight);
    JSONObject.Add('Tarsus length', FTarsusLength);
    JSONObject.Add('Tarsus diameter', FTarsusDiameter);
    JSONObject.Add('Exposed culmen', FExposedCulmen);
    JSONObject.Add('Bill width', FBillWidth);
    JSONObject.Add('Bill height', FBillHeight);
    JSONObject.Add('Nostril to bill tip', FNostrilBillTip);
    JSONObject.Add('Skull length', FSkullLength);
    JSONObject.Add('Right wing chord', FRightWingChord);
    JSONObject.Add('First secondary chord', FFirstSecondaryChord);
    JSONObject.Add('Tail length', FTailLength);
    JSONObject.Add('Fat', FFat);
    JSONObject.Add('Brood patch', FBroodPatch);
    JSONObject.Add('Cloacal protuberance', FCloacalProtuberance);
    JSONObject.Add('Body molt', FBodyMolt);
    JSONObject.Add('Flight feathers molt', FFlightFeathersMolt);
    JSONObject.Add('Flight feathers wear', FFlightFeathersWear);
    JSONObject.Add('Molt limits', FMoltLimits);
    JSONObject.Add('Cycle code', FCycleCode);
    JSONObject.Add('How was aged', FHowAged);
    JSONObject.Add('Skull ossification', FSkullOssification);
    JSONObject.Add('Kipps index', FKippsIndex);
    JSONObject.Add('Glucose', FGlucose);
    JSONObject.Add('Hemoglobin', FHemoglobin);
    JSONObject.Add('Hematocrit', FHematocrit);
    JSONObject.Add('Blood sample', FBloodSample);
    JSONObject.Add('Feather sample', FFeatherSample);
    JSONObject.Add('Photographed', FSubjectPhotographed);
    JSONObject.Add('Photographer 1', FPhotographer1Id);
    JSONObject.Add('Photographer 2', FPhotographer2Id);
    JSONObject.Add('Start photo number', FStartPhotoNumber);
    JSONObject.Add('End photo number', FEndPhotoNumber);
    JSONObject.Add('Camera name', FCameraName);
    JSONObject.Add('Removed band', FRemovedBandId);
    JSONObject.Add('Right tarsus', FRightLegBelow);
    JSONObject.Add('Left tarsus', FLeftLegBelow);
    JSONObject.Add('Right tibia', FRightLegAbove);
    JSONObject.Add('Left tibia', FLeftLegAbove);
    JSONObject.Add('Escaped', FEscaped);
    JSONObject.Add('Needs review', FNeedsReview);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TCapture.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TCapture.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      FFullName := GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, SEXES[FSubjectSex],
        CAPTURE_TYPES[FCaptureType], FCycleCode, False);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetDateParam(ParamByName('capture_date'), FCaptureDate);
      SetTimeParam(ParamByName('capture_time'), FCaptureTime);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetForeignParam(ParamByName('net_station_id'), FNetStationId);
      SetForeignParam(ParamByName('net_id'), FNetId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('bander_id'), FBanderId);
      SetForeignParam(ParamByName('annotator_id'), FAnnotatorId);
      SetStrParam(ParamByName('subject_status'), SUBJECT_STATUSES[FSubjectStatus]);
      SetStrParam(ParamByName('capture_type'), CAPTURE_TYPES[FCaptureType]);
      SetStrParam(ParamByName('subject_sex'), SEXES[FSubjectSex]);
      SetStrParam(ParamByName('how_sexed'), FHowSexed);
      SetForeignParam(ParamByName('band_id'), FBandId);
      SetFloatParam(ParamByName('weight'), FWeight);
      SetFloatParam(ParamByName('tarsus_length'), FTarsusLength);
      SetFloatParam(ParamByName('tarsus_diameter'), FTarsusDiameter);
      SetFloatParam(ParamByName('exposed_culmen'), FExposedCulmen);
      SetFloatParam(ParamByName('bill_width'), FBillWidth);
      SetFloatParam(ParamByName('bill_height'), FBillHeight);
      SetFloatParam(ParamByName('nostril_bill_tip'), FNostrilBillTip);
      SetFloatParam(ParamByName('skull_length'), FSkullLength);
      SetFloatParam(ParamByName('right_wing_chord'), FRightWingChord);
      SetFloatParam(ParamByName('first_secondary_chord'), FFirstSecondaryChord);
      SetFloatParam(ParamByName('tail_length'), FTailLength);
      SetStrParam(ParamByName('fat'), FFat);
      SetStrParam(ParamByName('brood_patch'), FBroodPatch);
      SetStrParam(ParamByName('cloacal_protuberance'), FCloacalProtuberance);
      SetStrParam(ParamByName('body_molt'), FBodyMolt);
      SetStrParam(ParamByName('flight_feathers_molt'), FFlightFeathersMolt);
      SetStrParam(ParamByName('flight_feathers_wear'), FFlightFeathersWear);
      SetStrParam(ParamByName('molt_limits'), FMoltLimits);
      SetStrParam(ParamByName('cycle_code'), FCycleCode);
      SetStrParam(ParamByName('subject_age'), AGES[FSubjectAge]);
      SetStrParam(ParamByName('how_aged'), FHowAged);
      SetStrParam(ParamByName('skull_ossification'), FSkullOssification);
      SetFloatParam(ParamByName('kipps_index'), FKippsIndex);
      SetFloatParam(ParamByName('glucose'), FGlucose);
      SetFloatParam(ParamByName('hemoglobin'), FHemoglobin);
      SetFloatParam(ParamByName('hematocrit'), FHematocrit);
      ParamByName('blood_sample').AsBoolean := FBloodSample;
      ParamByName('feather_sample').AsBoolean := FFeatherSample;
      SetForeignParam(ParamByName('photographer_1_id'), FPhotographer1Id);
      SetForeignParam(ParamByName('photographer_2_id'), FPhotographer2Id);
      if (FPhotographer1Id > 0) then
        FSubjectPhotographed := True;
      ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
      SetStrParam(ParamByName('start_photo_number'), FStartPhotoNumber);
      SetStrParam(ParamByName('end_photo_number'), FEndPhotoNumber);
      SetStrParam(ParamByName('camera_name'), FCameraName);
      SetForeignParam(ParamByName('removed_band_id'), FRemovedBandId);
      SetStrParam(ParamByName('right_leg_below'), FRightLegBelow);
      SetStrParam(ParamByName('left_leg_below'), FLeftLegBelow);
      ParamByName('escaped').AsBoolean := FEscaped;
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('needs_review').AsBoolean := FNeedsReview;
      ParamByName('exported_status').AsBoolean := FExported;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('capture_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TCapture.Find(aTaxon, aBand: Integer; aCaptureType, aDate, aTime: String): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT capture_id FROM captures');
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
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('capture_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TCapture.Diff(aOld: TCapture; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.CaptureDate, FCaptureDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.CaptureTime, FCaptureTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.NetStationId, FNetStationId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnetID, aOld.NetId, FNetId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBanderID, aOld.BanderId, FBanderId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAnnotatorID, aOld.AnnotatorId, FAnnotatorId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStatus, aOld.SubjectStatus, FSubjectStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.CaptureType, FCaptureType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSex, aOld.SubjectSex, FSubjectSex, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHowWasSexed, aOld.HowSexed, FHowSexed, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBandID, aOld.BandId, FBandId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRemovedBandID, aOld.RemovedBandId, FRemovedBandId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRightTarsus, aOld.RightLegBelow, FRightLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLeftTarsus, aOld.LeftLegBelow, FLeftLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWeight, aOld.Weight, FWeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTarsusLength, aOld.TarsusLength, FTarsusLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTarsusDiameter, aOld.TarsusDiameter, FTarsusDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTotalCulmen, aOld.CulmenLength, FCulmenLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExposedCulmen, aOld.ExposedCulmen, FExposedCulmen, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBillWidth, aOld.BillWidth, FBillWidth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBillHeight, aOld.BillHeight, FBillHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNostrilToBillTip, aOld.NostrilBillTip, FNostrilBillTip, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSkullLength, aOld.SkullLength, FSkullLength, R) then
    aList.Add(R);
  //if FieldValuesDiff(rscHaluxLengthTotal, aOld.HaluxLengthTotal, FHaluxLengthTotal, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rscHaluxLengthFinger, aOld.HaluxLengthFinger, FHaluxLengthFinger, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rscHaluxLengthClaw, aOld.HaluxLengthClaw, FHaluxLengthClaw, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscRightWingChord, aOld.RightWingChord, FRightWingChord, R) then
    aList.Add(R);
  if FieldValuesDiff(rsc1stSecondaryChord, aOld.FirstSecondaryChord, FFirstSecondaryChord, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTailLength, aOld.TailLength, FTailLength, R) then
    aList.Add(R);
  //if FieldValuesDiff(rscCentralRetrixLength, aOld.CentralRetrixLength, FCentralRetrixLength, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rscExternalRetrixLength, aOld.ExternalRetrixLength, FExternalRetrixLength, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscTotalLength, aOld.TotalLength, FTotalLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFeatherMites, aOld.FeatherMites, FFeatherMites, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFat, aOld.Fat, FFat, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBroodPatch, aOld.BroodPatch, FBroodPatch, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloacalProtuberance, aOld.CloacalProtuberance, FCloacalProtuberance, R) then
    aList.Add(R);
  //if FieldValuesDiff('Muda (leg.)', aOld.OldMolt, FOldMolt, R) then
  //  aList.Add(R);
  //if FieldValuesDiff('Muda prim'#225'rias (leg.)', aOld.OldPrimariesMolt, FOldPrimariesMolt, R) then
  //  aList.Add(R);
  //if FieldValuesDiff('Muda secund'#225'rias (leg.)', aOld.OldSecondariesMolt, FOldSecondariesMolt, R) then
  //  aList.Add(R);
  //if FieldValuesDiff('Muda retrizes (leg.)', aOld.OldRetricesMolt, FOldRetricesMolt, R) then
  //  aList.Add(R);
  //if FieldValuesDiff('Muda corpo (leg.)', aOld.OldBodyMolt, FOldBodyMolt, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscBodyMolt, aOld.BodyMolt, FBodyMolt, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFlightFeathersMolt, aOld.FlightFeathersMolt, FFlightFeathersMolt, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFlightFeathersWear, aOld.FlightFeathersWear, FFlightFeathersWear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMoltLimits, aOld.MoltLimits, FMoltLimits, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMoltCycle, aOld.CycleCode, FCycleCode, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAge, aOld.SubjectAge, FSubjectAge, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHowWasAged, aOld.HowAged, FHowAged, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSkullOssification, aOld.SkullOssification, FSkullOssification, R) then
    aList.Add(R);
  if FieldValuesDiff(rscKippsDistance, aOld.KippsIndex, FKippsIndex, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGlucose, aOld.Glucose, FGlucose, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHemoglobin, aOld.Hemoglobin, FHemoglobin, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHematocrit, aOld.Hematocrit, FHematocrit, R) then
    aList.Add(R);
  if FieldValuesDiff(rscQuantPhilornisLarvae, aOld.PhilornisLarvaeTally, FPhilornisLarvaeTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBlood, aOld.BloodSample, FBloodSample, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFeathers, aOld.FeatherSample, FFeatherSample, R) then
    aList.Add(R);
  if FieldValuesDiff(rscClaw, aOld.ClawSample, FClawSample, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFeces, aOld.FecesSample, FFecesSample, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParasites, aOld.ParasiteSample, FParasiteSample, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectedWhole, aOld.SubjectCollected, FSubjectCollected, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRecorded, aOld.SubjectRecorded, FSubjectRecorded, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhotographed, aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhotographer1ID, aOld.Photographer1Id, FPhotographer1Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhotographer2ID, aOld.Photographer2Id, FPhotographer2Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscInitialPhotoNr, aOld.StartPhotoNumber, FStartPhotoNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFinalPhotoNr, aOld.EndPhotoNumber, FEndPhotoNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCamera, aOld.CameraName, FCameraName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEscaped, aOld.Escaped, FEscaped, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNeedsReview, aOld.NeedsReview, FNeedsReview, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TIndividual }

constructor TIndividual.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TIndividual.Copy(aFrom: TIndividual);
begin
  FFullName := aFrom.FullName;
  FTaxonId := aFrom.TaxonId;
  FSex := aFrom.Sex;
  FAge := aFrom.Age;
  FNestId := aFrom.NestId;
  FBirthDate := aFrom.BirthDate;
  FBirthDay := aFrom.BirthDay;
  FBirthMonth := aFrom.BirthMonth;
  FBirthYear := aFrom.BirthYear;
  FBandingDate := aFrom.BandingDate;
  FBandChangeDate := aFrom.BandChangeDate;
  FBandId := aFrom.BandId;
  FDoubleBandId := aFrom.DoubleBandId;
  FRemovedBandId := aFrom.RemovedBandId;
  FRightLegBelow := aFrom.RightLegBelow;
  FLeftLegBelow := aFrom.LeftLegBelow;
  FRightLegAbove := aFrom.RightLegAbove;
  FLeftLegAbove := aFrom.LeftLegAbove;
  FFatherId := aFrom.FatherId;
  FMotherId := aFrom.MotherId;
  FDeathDate := aFrom.DeathDate;
  FDeathDay := aFrom.DeathDay;
  FDeathMonth := aFrom.DeathMonth;
  FDeathYear := aFrom.DeathYear;
  FRecognizableMarkings := aFrom.RecognizableMarkings;
  FNotes := aFrom.Notes;
end;

procedure TIndividual.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TIndividual.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM individuals');
      Add('WHERE (individual_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TIndividual.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TIndividual.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('individual_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    case FieldByName('individual_sex').AsString of
      'M': FSex := sexMale;
      'F': FSex := sexFemale;
    else
      FSex := sexUnknown;
    end;
    case FieldByName('individual_age').AsString of
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
    FNestId := FieldByName('nest_id').AsInteger;
    FBirthDate := FieldByName('birth_date').AsString;
    FBirthDay := FieldByName('birth_day').AsInteger;
    FBirthMonth := FieldByName('birth_month').AsInteger;
    FBirthYear := FieldByName('birth_year').AsInteger;
    if not FieldByName('banding_date').IsNull then
      FBandingDate := FieldByName('banding_date').AsDateTime
    else
      FBandingDate := NullDate;
    if not FieldByName('band_change_date').IsNull then
      FBandChangeDate := FieldByName('band_change_date').AsDateTime
    else
      FBandChangeDate := NullDate;
    FBandId := FieldByName('band_id').AsInteger;
    FDoubleBandId := FieldByName('double_band_id').AsInteger;
    FRemovedBandId := FieldByName('removed_band_id').AsInteger;
    FRightLegBelow := FieldByName('right_leg_below').AsString;
    FLeftLegBelow := FieldByName('left_leg_below').AsString;
    FRightLegAbove := FieldByName('right_leg_above').AsString;
    FLeftLegAbove := FieldByName('left_leg_above').AsString;
    FFatherId := FieldByName('father_id').AsInteger;
    FMotherId := FieldByName('mother_id').AsInteger;
    FDeathDate := FieldByName('death_date').AsString;
    FDeathDay := FieldByName('death_day').AsInteger;
    FDeathMonth := FieldByName('death_month').AsInteger;
    FDeathYear := FieldByName('death_year').AsInteger;
    FRecognizableMarkings := FieldByName('recognizable_markings').AsString;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), FInsertDate);
    GetTimeStamp(FieldByName('update_date'), FUpdateDate);
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TIndividual.Insert;
var
  Qry: TSQLQuery;
  Birth, Death: TPartialDate;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetStrParam(ParamByName('individual_sex'), SEXES[FSex]);
      SetStrParam(ParamByName('individual_age'), AGES[FAge]);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      if FBirthYear > 0 then
      begin
        ParamByName('birth_year').AsInteger := FBirthYear;
        ParamByName('birth_month').AsInteger := FBirthMonth;
        ParamByName('birth_day').AsInteger := FBirthDay;
        Birth.Encode(FBirthYear, FBirthMonth, FBirthDay, '.');
        SetStrParam(ParamByName('birth_date'), Birth.ToString);
      end
      else
      begin
        ParamByName('birth_year').Clear;
        ParamByName('birth_month').Clear;
        ParamByName('birth_day').Clear;
        ParamByName('birth_date').Clear;
      end;
      SetForeignParam(ParamByName('band_id'), FBandId);
      SetForeignParam(ParamByName('double_band_id'), FDoubleBandId);
      SetForeignParam(ParamByName('removed_band_id'), FRemovedBandId);
      SetDateParam(ParamByName('banding_date'), FBandingDate);
      SetDateParam(ParamByName('band_change_date'), FBandChangeDate);
      SetStrParam(ParamByName('recognizable_markings'), FRecognizableMarkings);
      SetStrParam(ParamByName('notes'), FNotes);
      SetForeignParam(ParamByName('father_id'), FFatherId);
      SetForeignParam(ParamByName('mother_id'), FMotherId);
      if FDeathYear > 0 then
      begin
        ParamByName('death_year').AsInteger := FDeathYear;
        ParamByName('death_month').AsInteger := FDeathMonth;
        ParamByName('death_day').AsInteger := FDeathDay;
        Death.Encode(FDeathYear, FDeathMonth, FDeathDay, '.');
        SetStrParam(ParamByName('death_date'), Death.ToString);
      end
      else
      begin
        ParamByName('death_year').Clear;
        ParamByName('death_month').Clear;
        ParamByName('death_day').Clear;
        ParamByName('death_date').Clear;
      end;
      SetStrParam(ParamByName('formatted_name'), GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, SEXES[FSex], True));
      FFullName := GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, SEXES[FSex], False);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('right_leg_below'), FRightLegBelow);
      SetStrParam(ParamByName('left_leg_below'), FLeftLegBelow);
      SetStrParam(ParamByName('right_leg_above'), FRightLegAbove);
      SetStrParam(ParamByName('left_leg_above'), FLeftLegAbove);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TIndividual.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TIndividual.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    //JSONObject.Add('Name (Formatted)', FFormattedName);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Sex', SEXES[FSex]);
    JSONObject.Add('Age', AGES[FAge]);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Birth year', FBirthYear);
    JSONObject.Add('Birth month', FBirthMonth);
    JSONObject.Add('Birth day', FBirthDay);
    JSONObject.Add('Banding date', FBandingDate);
    JSONObject.Add('Band change date', FBandChangeDate);
    JSONObject.Add('Band', FBandId);
    JSONObject.Add('Double band', FDoubleBandId);
    JSONObject.Add('Removed band', FRemovedBandId);
    JSONObject.Add('Right tarsus', FRightLegBelow);
    JSONObject.Add('Left tarsus', FLeftLegBelow);
    JSONObject.Add('Right tibia', FRightLegAbove);
    JSONObject.Add('Left tibia', FLeftLegAbove);
    JSONObject.Add('Father', FFatherId);
    JSONObject.Add('Mother', FMotherId);
    JSONObject.Add('Death year', FDeathYear);
    JSONObject.Add('Death month', FDeathMonth);
    JSONObject.Add('Death day', FDeathDay);
    JSONObject.Add('Recognizable markings', FRecognizableMarkings);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TIndividual.Update;
var
  Qry: TSQLQuery;
  Birth, Death: TPartialDate;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TIndividual.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetStrParam(ParamByName('individual_sex'), SEXES[FSex]);
      SetStrParam(ParamByName('individual_age'), AGES[FAge]);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      if FBirthYear > 0 then
      begin
        ParamByName('birth_year').AsInteger := FBirthYear;
        ParamByName('birth_month').AsInteger := FBirthMonth;
        ParamByName('birth_day').AsInteger := FBirthDay;
        Birth.Encode(FBirthYear, FBirthMonth, FBirthDay, '.');
        SetStrParam(ParamByName('birth_date'), Birth.ToString);
      end
      else
      begin
        ParamByName('birth_year').Clear;
        ParamByName('birth_month').Clear;
        ParamByName('birth_day').Clear;
        ParamByName('birth_date').Clear;
      end;
      SetForeignParam(ParamByName('band_id'), FBandId);
      SetForeignParam(ParamByName('double_band_id'), FDoubleBandId);
      SetForeignParam(ParamByName('removed_band_id'), FRemovedBandId);
      SetDateParam(ParamByName('banding_date'), FBandingDate);
      SetDateParam(ParamByName('band_change_date'), FBandChangeDate);
      SetStrParam(ParamByName('recognizable_markings'), FRecognizableMarkings);
      SetStrParam(ParamByName('notes'), FNotes);
      SetForeignParam(ParamByName('father_id'), FFatherId);
      SetForeignParam(ParamByName('mother_id'), FMotherId);
      if FDeathYear > 0 then
      begin
        ParamByName('death_year').AsInteger := FDeathYear;
        ParamByName('death_month').AsInteger := FDeathMonth;
        ParamByName('death_day').AsInteger := FDeathDay;
        Death.Encode(FDeathYear, FDeathMonth, FDeathDay, '.');
        SetStrParam(ParamByName('death_date'), Death.ToString);
      end
      else
      begin
        ParamByName('death_year').Clear;
        ParamByName('death_month').Clear;
        ParamByName('death_day').Clear;
        ParamByName('death_date').Clear;
      end;
      SetStrParam(ParamByName('formatted_name'), GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, SEXES[FSex], True));
      FFullName := GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, SEXES[FSex], False);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('right_leg_below'), FRightLegBelow);
      SetStrParam(ParamByName('left_leg_below'), FLeftLegBelow);
      SetStrParam(ParamByName('right_leg_above'), FRightLegAbove);
      SetStrParam(ParamByName('left_leg_above'), FLeftLegAbove);
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('individual_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TIndividual.Find(aTaxon, aBand: Integer; aRightLeg: String; aLeftLeg: String): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT individual_id FROM individuals');
    Add('WHERE (taxon_id = :ataxon)');
    Add('AND (band_id = :aband)');
    if (aRightLeg <> EmptyStr) then
    begin
      Add('AND (right_leg_below = :rightleg)');
      ParamByName('RIGHTLEG').AsString := aRightLeg;
    end;
    if (aLeftLeg <> EmptyStr) then
    begin
      Add('AND (left_leg_below = :leftleg)');
      ParamByName('LEFTLEG').AsString := aLeftLeg;
    end;
    ParamByName('ATAXON').AsInteger := aTaxon;
    ParamByName('ABAND').AsInteger := aBand;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('individual_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TIndividual.Diff(aOld: TIndividual; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSex, aOld.Sex, FSex, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAge, aOld.Age, FAge, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBirthDate, aOld.BirthDate, FBirthDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBirthDay, aOld.BirthDay, FBirthDay, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBirthMonth, aOld.BirthMonth, FBirthMonth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBirthYear, aOld.BirthYear, FBirthYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBandingDate, aOld.BandingDate, FBandingDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBandChangeDate, aOld.BandChangeDate, FBandChangeDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBandID, aOld.BandId, FBandId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDoubleBandID, aOld.DoubleBandId, FDoubleBandId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRemovedBandID, aOld.RemovedBandId, FRemovedBandId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRightTarsus, aOld.RightLegBelow, FRightLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLeftTarsus, aOld.LeftLegBelow, FLeftLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRightTibia, aOld.RightLegAbove, FRightLegAbove, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLeftTibia, aOld.LeftLegAbove, FLeftLegAbove, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFatherID, aOld.FatherId, FFatherId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMotherID, aOld.MotherId, FMotherId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDeathDate, aOld.DeathDate, FDeathDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDeathDay, aOld.DeathDay, FDeathDay, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDeathMonth, aOld.DeathMonth, FDeathMonth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDeathYear, aOld.DeathYear, FDeathYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRecognizableMarkings, aOld.RecognizableMarkings, FRecognizableMarkings, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

end.

