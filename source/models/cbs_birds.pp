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

unit cbs_birds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DB, SQLDB, fpjson, DateUtils, cbs_record_types;

type
  TBandColorCode = (ccNone = -1, ccMetal = 0, ccAnotherMetal = 1, ccYellow = 2, ccOrange = 3,
    ccRed = 4, ccCarmine = 5, ccPink = 6, ccViolet = 7, ccPaleBlue = 8, ccBlue = 9, ccGreen = 10,
    ccLime = 11, ccUmber = 12, ccWhite = 13, ccSilver = 14, ccBlack = 15);

const
  BandColors: array [0..15, 0..1] of String = (('M', '$00C0C0C0'), ('A', '$00008080'),
    ('Y', '$0000FFFF'), ('O', '$001AB5FF'), ('R', '$000000FF'), ('C', '$00FF00FF'),
    ('K', '$00D2A6FF'), ('V', '$00FF3C9D'), ('P', '$00FFA54A'), ('B', '$00FF0000'),
    ('G', '$00008000'), ('L', '$0000FF00'), ('U', '$00004080'), ('W', '$00FFFFFF'),
    ('S', '$00808080'), ('N', '$00000000'));

type
  TBodyPart = (bpRightTibia, bpLeftTibia, bpRightTarsus, bpLeftTarsus, bpRightWing, bpLeftWing, bpNeck);
  TMarkType = (mkButtEndBand, mkFlag, mkCollar, mkWingTag, mkTriangularBand, mkLockOnBand, mkRivetBand,
    mkClosedBand, mkOther);
  TBandStatus = (bstAvailable, bstUsed, bstRemoved, bstBroken, bstLost, bstTransfered);
  TBandSource = (bscAcquiredFromSupplier, bscTransferBetweenBanders, bscLivingBirdBandedByOthers,
    bscDeadBirdBandedByOthers, bscFoundLoose);
  TBandEvent = (bevOrder, bevReceive, bevTransfer, bevRetrieve, bevReport, bevUse, bevDischarge);

  TBirdMark = class
    BodyPart: TBodyPart;
    Index: Integer;
    Color: TBandColorCode;
    MarkType: TMarkType;
    Inscription: String;
    BandId: Integer;
  end;

  TBirdMarks = specialize TObjectList<TBirdMark>;

const
  CEMAVEBandSizes: array[1..19] of Char = ('A','C','D','E','F','G','H','J','L','M','N','P','R','S','T','U','V','X','Z');
  BandStatusStr: array[TBandStatus] of Char = ('D', 'U', 'R', 'Q', 'P', 'T');
  MarkTypesStr: array[TMarkType] of Char = ('A', 'F', 'N', 'W', 'T', 'L', 'R', 'C', 'O');
  BandSourceStr: array[TBandSource] of Char = ('A', 'T', 'L', 'D', 'F');
  BandEventStr: array[TBandEvent] of Char = ('O', 'C', 'T', 'R', 'P', 'U', 'D');

type

  { TBand }

  TBand = class(TXolmisRecord)
  protected
    FFullName: String;
    FSize: String;
    FNumber: Integer;
    FStatus: TBandStatus;
    FSource: TBandSource;
    FPrefix: String;
    FSuffix: String;
    FBandColor: String;
    FBandType: TMarkType;
    FSupplierId: Integer;
    FRequesterId: Integer;
    FCarrierId: Integer;
    FIndividualId: Integer;
    FProjectId: Integer;
    FReported: Boolean;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aSize: String; aNumber: Integer): Boolean;
    function Diff(aOld: TBand; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TBand);
    function ToJSON: String;
  published
    property FullName: String read FFullName write FFullName;
    property Size: String read FSize write FSize;
    property Number: Integer read FNumber write FNumber;
    property Status: TBandStatus read FStatus write FStatus;
    property Source: TBandSource read FSource write FSource;
    property Prefix: String read FPrefix write FPrefix;
    property Suffix: String read FSuffix write FSuffix;
    property BandColor: String read FBandColor write FBandColor;
    property BandType: TMarkType read FBandType write FBandType;
    property SupplierId: Integer read FSupplierId write FSupplierId;
    property RequesterId: Integer read FRequesterId write FRequesterId;
    property CarrierId: Integer read FCarrierId write FCarrierId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property Reported: Boolean read FReported write FReported;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TBandHistory }

  TBandHistory = class(TXolmisRecord)
  protected
    FBandId: Integer;
    FEventType: TBandEvent;
    FEventDate: TDate;
    FOrderNumber: Integer;
    FSupplierId: Integer;
    FSenderId: Integer;
    FRequesterId: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TBandHistory; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TBandHistory);
    function ToJSON: String;
  published
    property BandId: Integer read FBandId write FBandId;
    property EventType: TBandEvent read FEventType write FEventType;
    property EventDate: TDate read FEventDate write FEventDate;
    property OrderNumber: Integer read FOrderNumber write FOrderNumber;
    property SupplierId: Integer read FSupplierId write FSupplierId;
    property SenderId: Integer read FSenderId write FSenderId;
    property RequesterId: Integer read FRequesterId write FRequesterId;
    property Notes: String read FNotes write FNotes;
  end;

type
  TSex = (sexUnknown, sexMale, sexFemale);
  TAge = (ageUnknown, ageNestling, ageFledgling, ageJuvenile, ageAdult, ageFirstYear, ageSecondYear, ageThirdYear,
    ageFourthYear, ageFifthYear);

const
  Sexes: array[TSex] of String = ('U', 'M', 'F');
  Ages: array[TAge] of String = ('U', 'N', 'F', 'J', 'A', 'Y', 'S', 'T', '4', '5');

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
  TCaptureType = (cptNew, cptRecapture, cptSameDay, cptChangeBand, cptUnbanded);
  TSubjectStatus = (sstNormal, sstInjured, sstWingSprain, sstStressed, sstDead);

const
  CaptureTypeStr: array[TCaptureType] of Char = ('N', 'R', 'S', 'C', 'U');
  SubjectStatusStr: array[TSubjectStatus] of Char = ('N', 'I', 'W', 'X', 'D');

  CloacalProtuberanceValues: array [0 .. 4] of String = ('U', 'N', 'S', 'M', 'L');
  BroodPatchValues: array [0 .. 4] of String          = ('F', 'N', 'V', 'W', 'O');
  FatValues: array [0 .. 7] of String                 = ('N', 'T', 'L', 'H', 'F', 'B', 'G', 'V');
  BodyMoltValues: array [0 .. 6] of String            = ('N', 'T', 'S', 'H', 'G', 'A', 'F');
  FlightMoltValues: array [0 .. 2] of String          = ('N', 'S', 'A');
  FeatherWearValues: array [0 .. 5] of String         = ('N', 'S', 'L', 'M', 'H', 'X');
  SkullValues: array [0 .. 6] of String               = ('N', 'T', 'L', 'H', 'G', 'A', 'F');

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
  TFeatherDataSource = (fdsUnknown, fdsCapture, fdsSighting, fdsPhoto);
  TSymmetry = (symUnknown, symSymmetrical, symAsymmetrical);
  TFeatherTrait = (ftrBody, ftrPrimary, ftrSecondary, ftrRectrix, ftrPrimaryCovert, ftrGreatCovert,
    ftrMedianCovert, ftrLesserCovert, ftrCarpalCovert, ftrAlula);
  TBodySide = (bsdNotApplicable, bsdRight, bsdLeft);
  TFeatherAge = (fageUnknown, fageNestling, fageFledgling, fageAdult, fageFirstYear, fageSecondYear, fageThirdYear,
    fageFourthYear, fageFifthYear);

const
  FeatherDataSourceStr: array[TFeatherDataSource] of String = ('U', 'C', 'S', 'P');
  SymmetryStr: array[TSymmetry] of String = ('U', 'S', 'A');
  FeatherTraitStr: array[TFeatherTrait] of String = ('B', 'P', 'S', 'R', 'PC', 'GC', 'MC', 'LC', 'CC', 'AL');
  BodySideStr: array[TBodySide] of String = ('NA', 'R', 'L');
  FeatherAgeStr: array[TFeatherAge] of String = ('U', 'N', 'F', 'A', 'Y', 'S', 'T', '4', '5');

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

type

  { TSighting }

  TSighting = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FSightingDate: TDate;
    FSightingTime: TTime;
    FLocalityId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FObserverId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FSubjectTally: Integer;
    FSubjectDistance: Double;
    FMethodId: Integer;
    FMackinnonListNumber: Integer;
    FSubjectCaptured: Boolean;
    FSubjectSeen: Boolean;
    FSubjectHeard: Boolean;
    FSubjectPhotographed: Boolean;
    FSubjectRecorded: Boolean;
    FMalesTally: String;
    FFemalesTally: String;
    FNotSexedTally: String;
    FAdultsTally: String;
    FImmatureTally: String;
    FNotAgedTally: String;
    FRecapturesTally: Integer;
    FNewCapturesTally: Integer;
    FUnbandedTally: Integer;
    FDetectionType: String;
    FBreedingStatus: String;
    FNotSurveying: Boolean;
    FIsOnEbird: Boolean;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    procedure Update;
    function Diff(aOld: TSighting; var aList: TStrings): Boolean;
    function Find(aSurvey, aTaxon, aObserver: Integer): Boolean;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSighting);
    function ToJSON: String;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingDate: TDate read FSightingDate write FSightingDate;
    property SightingTime: TTime read FSightingTime write FSightingTime;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property ObserverId: Integer read FObserverId write FObserverId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SubjectTally: Integer read FSubjectTally write FSubjectTally;
    property SubjectDistance: Double read FSubjectDistance write FSubjectDistance;
    property MethodId: Integer read FMethodId write FMethodId;
    property MackinnonListNumber: Integer read FMackinnonListNumber write FMackinnonListNumber;
    property SubjectCaptured: Boolean read FSubjectCaptured write FSubjectCaptured;
    property SubjectSeen: Boolean read FSubjectSeen write FSubjectSeen;
    property SubjectHeard: Boolean read FSubjectHeard write FSubjectHeard;
    property SubjectPhotographed: Boolean read FSubjectPhotographed write FSubjectPhotographed;
    property SubjectRecorded: Boolean read FSubjectRecorded write FSubjectRecorded;
    property MalesTally: String read FMalesTally write FMalesTally;
    property FemalesTally: String read FFemalesTally write FFemalesTally;
    property NotSexedTally: String read FNotSexedTally write FNotSexedTally;
    property AdultsTally: String read FAdultsTally write FAdultsTally;
    property ImmatureTally: String read FImmatureTally write FImmatureTally;
    property NotAgedTally: String read FNotAgedTally write FNotAgedTally;
    property RecapturesTally: Integer read FRecapturesTally write FRecapturesTally;
    property NewCapturesTally: Integer read FNewCapturesTally write FNewCapturesTally;
    property UnbandedTally: Integer read FUnbandedTally write FUnbandedTally;
    property DetectionType: String read FDetectionType write FDetectionType;
    property BreedingStatus: String read FBreedingStatus write FBreedingStatus;
    property NotSurveying: Boolean read FNotSurveying write FNotSurveying;
    property IsOnEbird: Boolean read FIsOnEbird write FIsOnEbird;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  cbs_system, cbs_global, cbs_users, cbs_validations, cbs_fullnames, cbs_datacolumns, cbs_setparam, cbs_getvalue,
  cbs_locale, udm_main;

{ TBandHistory }

constructor TBandHistory.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TBandHistory.Clear;
begin
  inherited Clear;
  FBandId := 0;
  FEventDate := NullDate;
  FOrderNumber := 0;
  FEventType := bevUse;
  FSupplierId := 0;
  FRequesterId := 0;
  FSenderId := 0;
  FNotes := EmptyStr;
end;

procedure TBandHistory.Copy(aFrom: TBandHistory);
begin
  FBandId := aFrom.BandId;
  FEventDate := aFrom.EventDate;
  FOrderNumber := aFrom.OrderNumber;
  FEventType := aFrom.EventType;
  FSupplierId := aFrom.SupplierId;
  FRequesterId := aFrom.RequesterId;
  FSenderId := aFrom.SenderId;
  FNotes := aFrom.Notes;
end;

procedure TBandHistory.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBandHistory.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM band_history');
      Add('WHERE (event_id = :aid)');

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

procedure TBandHistory.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'event_id, ' +
      'band_id, ' +
      'event_type, ' +
      'event_date, ' +
      'order_number, ' +
      'supplier_id, ' +
      'sender_id, ' +
      'requester_id, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM band_history');
    Add('WHERE event_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBandHistory.Diff(aOld: TBandHistory; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Event type', aOld.EventType, FEventType, R) then
    aList.Add(R);
  if FieldValuesDiff('Order number', aOld.OrderNumber, FOrderNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Event date', aOld.EventDate, FEventDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Supplier ID', aOld.SupplierId, FSupplierId, R) then
    aList.Add(R);
  if FieldValuesDiff('Requester ID', aOld.RequesterId, FRequesterId, R) then
    aList.Add(R);
  if FieldValuesDiff('Sender ID', aOld.SenderId, FSenderId, R) then
    aList.Add(R);
  if FieldValuesDiff('Notes', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

procedure TBandHistory.Insert;
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
      Add('INSERT INTO band_history (' +
        'band_id, ' +
        'event_date, ' +
        'notes, ' +
        'event_type, ' +
        'supplier_id, ' +
        'order_number, ' +
        'requester_id, ' +
        'sender_id, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':band_id, ' +
        'date(:event_date), ' +
        ':notes, ' +
        ':event_type, ' +
        ':supplier_id, ' +
        ':order_number, ' +
        ':requester_id, ' +
        ':sender_id, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('band_id').AsInteger := FBandId;
      SetDateParam(ParamByName('event_date'), FEventDate);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('event_type'), BandEventStr[FEventType]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetIntParam(ParamByName('order_number'), FOrderNumber);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('sender_id'), FSenderId);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistory.LoadFromDataSet(aDataSet: TDataSet);
//var
//  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('event_id').AsInteger;
    FBandId := FieldByName('band_id').AsInteger;
    FEventDate := FieldByName('event_date').AsDateTime;
    FOrderNumber := FieldByName('order_number').AsInteger;
    case FieldByName('event_type').AsString of
      'O': FEventType := bevOrder;
      'C': FEventType := bevReceive;
      'T': FEventType := bevTransfer;
      'R': FEventType := bevRetrieve;
      'P': FEventType := bevReport;
      'U': FEventType := bevUse;
      'D': FEventType := bevDischarge;
    end;
    FSupplierId := FieldByName('supplier_id').AsInteger;
    FRequesterId := FieldByName('requester_id').AsInteger;
    FSenderId := FieldByName('sender_id').AsInteger;
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

procedure TBandHistory.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TBandHistory.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Band', FBandId);
    JSONObject.Add('Date', FEventDate);
    JSONObject.Add('Notes', FNotes);
    JSONObject.Add('Type', BandEventStr[FEventType]);
    JSONObject.Add('Supplier', FSupplierId);
    JSONObject.Add('Order number', FOrderNumber);
    JSONObject.Add('Requester', FRequesterId);
    JSONObject.Add('Sender', FSenderId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TBandHistory.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBandHistory.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE band_history SET ' +
        'band_id = :band_id, ' +
        'event_date = date(:event_date), ' +
        'notes = :notes, ' +
        'event_type = :event_type, ' +
        'supplier_id = :supplier_id, ' +
        'order_number = :order_number, ' +
        'requester_id = :requester_id, ' +
        'sender_id = :sender_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (event_id = :event_id)');
      ParamByName('band_id').AsInteger := FBandId;
      SetDateParam(ParamByName('event_date'), FEventDate);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('event_type'), BandEventStr[FEventType]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetIntParam(ParamByName('order_number'), FOrderNumber);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('sender_id'), FSenderId);
      //ParamByName('marked_status').AsBoolean := FMarked;
      //ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;
      ParamByName('event_id').AsInteger := FId;

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

{ TSighting }

constructor TSighting.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSighting.Clear;
begin
  inherited;
  FSurveyId := 0;
  FSightingDate := NullDate;
  FSightingTime := NullTime;
  FLocalityId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FObserverId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FSubjectTally := 0;
  FSubjectDistance := 0.0;
  FMethodId := 0;
  FMackinnonListNumber := 0;
  FSubjectCaptured := False;
  FSubjectSeen := False;
  FSubjectHeard := False;
  FSubjectPhotographed := False;
  FSubjectRecorded := False;
  FMalesTally := EmptyStr;
  FFemalesTally := EmptyStr;
  FNotSexedTally := EmptyStr;
  FAdultsTally := EmptyStr;
  FImmatureTally := EmptyStr;
  FNotAgedTally := EmptyStr;
  FRecapturesTally := 0;
  FNewCapturesTally := 0;
  FUnbandedTally := 0;
  FDetectionType := EmptyStr;
  FBreedingStatus := EmptyStr;
  FNotSurveying := False;
  FIsOnEbird := False;
  FNotes := EmptyStr;
end;

procedure TSighting.Copy(aFrom: TSighting);
begin
  FSurveyId := aFrom.SurveyId;
  FSightingDate := aFrom.SightingDate;
  FSightingTime := aFrom.SightingTime;
  FLocalityId := aFrom.LocalityId;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FObserverId := aFrom.ObserverId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FSubjectTally := aFrom.SubjectTally;
  FSubjectDistance := aFrom.SubjectDistance;
  FMethodId := aFrom.MethodId;
  FMackinnonListNumber := aFrom.MackinnonListNumber;
  FSubjectCaptured := aFrom.SubjectCaptured;
  FSubjectSeen := aFrom.SubjectSeen;
  FSubjectHeard := aFrom.SubjectHeard;
  FSubjectPhotographed := aFrom.SubjectPhotographed;
  FSubjectRecorded := aFrom.SubjectRecorded;
  FMalesTally := aFrom.MalesTally;
  FFemalesTally := aFrom.FemalesTally;
  FNotSexedTally := aFrom.NotSexedTally;
  FAdultsTally := aFrom.AdultsTally;
  FImmatureTally := aFrom.ImmatureTally;
  FNotAgedTally := aFrom.NotAgedTally;
  FRecapturesTally := aFrom.RecapturesTally;
  FNewCapturesTally := aFrom.NewCapturesTally;
  FUnbandedTally := aFrom.UnbandedTally;
  FDetectionType := aFrom.DetectionType;
  FBreedingStatus := aFrom.BreedingStatus;
  FNotSurveying := aFrom.NotSurveying;
  FIsOnEbird := aFrom.IsOnEbird;
  FNotes := aFrom.Notes;
end;

procedure TSighting.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSighting.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM sightings');
      Add('WHERE (sighting_id = :aid)');

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

procedure TSighting.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT ' +
      'sighting_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'sighting_date, ' +
      'sighting_time, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'method_id, ' +
      'mackinnon_list_num, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'subjects_tally, ' +
      'subject_distance, ' +
      'subject_seen, ' +
      'subject_heard, ' +
      'subject_photographed, ' +
      'subject_recorded, ' +
      'subject_captured, ' +
      'males_tally, ' +
      'females_tally, ' +
      'not_sexed_tally, ' +
      'adults_tally, ' +
      'immatures_tally, ' +
      'not_aged_tally, ' +
      'new_captures_tally, ' +
      'recaptures_tally, ' +
      'unbanded_tally, ' +
      'detection_type, ' +
      'breeding_status, ' +
      'not_surveying, ' +
      'ebird_available, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM sightings');
    Add('WHERE sighting_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSighting.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('sighting_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FSightingDate := FieldByName('sighting_date').AsDateTime;
    FSightingTime := FieldByName('sighting_time').AsDateTime;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FObserverId := FieldByName('observer_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FSubjectTally := FieldByName('subjects_tally').AsInteger;
    FSubjectDistance := FieldByName('subject_distance').AsFloat;
    FMethodId := FieldByName('method_id').AsInteger;
    FMackinnonListNumber := FieldByName('mackinnon_list_num').AsInteger;
    FSubjectCaptured := FieldByName('subject_captured').AsBoolean;
    FSubjectSeen := FieldByName('subject_seen').AsBoolean;
    FSubjectHeard := FieldByName('subject_heard').AsBoolean;
    FSubjectPhotographed := FieldByName('subject_photographed').AsBoolean;
    FSubjectRecorded := FieldByName('subject_recorded').AsBoolean;
    FMalesTally := FieldByName('males_tally').AsString;
    FFemalesTally := FieldByName('females_tally').AsString;
    FNotSexedTally := FieldByName('not_sexed_tally').AsString;
    FAdultsTally := FieldByName('adults_tally').AsString;
    FImmatureTally := FieldByName('immatures_tally').AsString;
    FNotAgedTally := FieldByName('not_aged_tally').AsString;
    FRecapturesTally := FieldByName('recaptures_tally').AsInteger;
    FNewCapturesTally := FieldByName('new_captures_tally').AsInteger;
    FUnbandedTally := FieldByName('unbanded_tally').AsInteger;
    FDetectionType := FieldByName('detection_type').AsString;
    FBreedingStatus := FieldByName('breeding_status').AsString;
    FNotSurveying := FieldByName('not_surveying').AsBoolean;
    FIsOnEbird := FieldByName('ebird_available').AsBoolean;
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

procedure TSighting.Insert;
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
      Add('INSERT INTO sightings (' +
        'survey_id, ' +
        'individual_id, ' +
        'sighting_date, ' +
        'sighting_time, ' +
        'locality_id, ' +
        'longitude, ' +
        'latitude, ' +
        'method_id, ' +
        'mackinnon_list_num, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'subjects_tally, ' +
        'subject_distance, ' +
        'subject_seen, ' +
        'subject_heard, ' +
        'subject_photographed, ' +
        'subject_recorded, ' +
        'subject_captured, ' +
        'males_tally, ' +
        'females_tally, ' +
        'not_sexed_tally, ' +
        'adults_tally, ' +
        'immatures_tally, ' +
        'not_aged_tally, ' +
        'new_captures_tally, ' +
        'recaptures_tally, ' +
        'unbanded_tally, ' +
        'detection_type, ' +
        'breeding_status, ' +
        'not_surveying, ' +
        'ebird_available, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':survey_id, ' +
        ':individual_id, ' +
        'date(:sighting_date), ' +
        //'(CASE WHEN :sighting_time IS NULL THEN NULL ELSE time(:sighting_time) END),' +
        'time(:sighting_time), ' +
        ':locality_id, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':method_id, ' +
        ':mackinnon_list_num, ' +
        ':observer_id, ' +
        ':taxon_id, ' +
        ':subjects_tally, ' +
        ':subject_distance, ' +
        ':subject_seen, ' +
        ':subject_heard, ' +
        ':subject_photographed, ' +
        ':subject_recorded, ' +
        ':subject_captured, ' +
        ':males_tally, ' +
        ':females_tally, ' +
        ':not_sexed_tally, ' +
        ':adults_tally, ' +
        ':immatures_tally, ' +
        ':not_aged_tally, ' +
        ':new_captures_tally, ' +
        ':recaptures_tally, ' +
        ':unbanded_tally, ' +
        ':detection_type, ' +
        ':breeding_status, ' +
        ':not_surveying, ' +
        ':ebird_available, ' +
        ':full_name, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');

      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetDateParam(ParamByName('sighting_date'), FSightingDate);
      SetTimeParam(ParamByName('sighting_time'), FSightingTime);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('method_id'), FMethodId);
      SetIntParam(ParamByName('mackinnon_list_num'), FMackinnonListNumber);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetIntParam(ParamByName('subjects_tally'), FSubjectTally);
      SetFloatParam(ParamByName('subject_distance'), FSubjectDistance);
      ParamByName('subject_captured').AsBoolean := FSubjectCaptured;
      ParamByName('subject_seen').AsBoolean := FSubjectSeen;
      ParamByName('subject_heard').AsBoolean := FSubjectHeard;
      ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
      ParamByName('subject_recorded').AsBoolean := FSubjectRecorded;
      SetStrParam(ParamByName('males_tally'), FMalesTally);
      SetStrParam(ParamByName('females_tally'), FFemalesTally);
      SetStrParam(ParamByName('not_sexed_tally'), FNotSexedTally);
      SetStrParam(ParamByName('adults_tally'), FAdultsTally);
      SetStrParam(ParamByName('immatures_tally'), FImmatureTally);
      SetStrParam(ParamByName('not_aged_tally'), FNotAgedTally);
      SetIntParam(ParamByName('new_captures_tally'), FNewCapturesTally);
      SetIntParam(ParamByName('recaptures_tally'), FRecapturesTally);
      SetIntParam(ParamByName('unbanded_tally'), FUnbandedTally);
      SetStrParam(ParamByName('detection_type'), FDetectionType);
      SetStrParam(ParamByName('breeding_status'), FBreedingStatus);
      ParamByName('not_surveying').AsBoolean := FNotSurveying;
      ParamByName('ebird_available').AsBoolean := FIsOnEbird;
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

procedure TSighting.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSighting.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Date', FSightingDate);
    JSONObject.Add('Time', FSightingTime);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Subjects tally', FSubjectTally);
    JSONObject.Add('Subject distance', FSubjectDistance);
    JSONObject.Add('Method', FMethodId);
    JSONObject.Add('Mackinnon list number', FMackinnonListNumber);
    JSONObject.Add('Captured', FSubjectCaptured);
    JSONObject.Add('Seen', FSubjectSeen);
    JSONObject.Add('Heard', FSubjectHeard);
    JSONObject.Add('Photographed', FSubjectPhotographed);
    JSONObject.Add('Recorded', FSubjectRecorded);
    JSONObject.Add('Males', FMalesTally);
    JSONObject.Add('Females', FFemalesTally);
    JSONObject.Add('Not sexed', FNotSexedTally);
    JSONObject.Add('Adults', FAdultsTally);
    JSONObject.Add('Immatures', FImmatureTally);
    JSONObject.Add('Not aged', FNotAgedTally);
    JSONObject.Add('New captures', FNewCapturesTally);
    JSONObject.Add('Recaptures', FRecapturesTally);
    JSONObject.Add('Unbanded', FUnbandedTally);
    JSONObject.Add('Detection type', FDetectionType);
    JSONObject.Add('Breeding status', FBreedingStatus);
    JSONObject.Add('Not surveying', FNotSurveying);
    JSONObject.Add('Is on eBird', FIsOnEbird);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSighting.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSighting.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE sightings SET ' +
        'survey_id = :survey_id, ' +
        'individual_id = :individual_id, ' +
        'sighting_date = date(:sighting_date), ' +
        //'sighting_time = (CASE WHEN :sighting_time IS NULL THEN NULL ELSE time(:sighting_time) END),' +
        'sighting_time = time(:sighting_time), ' +
        'locality_id = :locality_id, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'method_id = :method_id, ' +
        'mackinnon_list_num = :mackinnon_list_num, ' +
        'observer_id = :observer_id, ' +
        'taxon_id = :taxon_id, ' +
        'subjects_tally = :subjects_tally, ' +
        'subject_distance = :subject_distance, ' +
        'subject_seen = :subject_seen, ' +
        'subject_heard = :subject_heard, ' +
        'subject_photographed = :subject_photographed, ' +
        'subject_recorded = :subject_recorded, ' +
        'subject_captured = :subject_captured, ' +
        'males_tally = :males_tally, ' +
        'females_tally = :females_tally, ' +
        'not_sexed_tally = :not_sexed_tally, ' +
        'adults_tally = :adults_tally, ' +
        'immatures_tally = :immatures_tally, ' +
        'not_aged_tally = :not_aged_tally, ' +
        'new_captures_tally = :new_captures_tally, ' +
        'recaptures_tally = :recaptures_tally, ' +
        'unbanded_tally = :unbanded_tally, ' +
        'detection_type = :detection_type, ' +
        'breeding_status = :breeding_status, ' +
        'not_surveying = :not_surveying, ' +
        'ebird_available = :ebird_available, ' +
        'full_name = :full_name, ' +
        'notes = :notes, ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (sighting_id = :sighting_id)');

      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetDateParam(ParamByName('sighting_date'), FSightingDate);
      SetTimeParam(ParamByName('sighting_time'), FSightingTime);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('method_id'), FMethodId);
      SetIntParam(ParamByName('mackinnon_list_num'), FMackinnonListNumber);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetIntParam(ParamByName('subjects_tally'), FSubjectTally);
      SetFloatParam(ParamByName('subject_distance'), FSubjectDistance);
      ParamByName('subject_captured').AsBoolean := FSubjectCaptured;
      ParamByName('subject_seen').AsBoolean := FSubjectSeen;
      ParamByName('subject_heard').AsBoolean := FSubjectHeard;
      ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
      ParamByName('subject_recorded').AsBoolean := FSubjectRecorded;
      SetStrParam(ParamByName('males_tally'), FMalesTally);
      SetStrParam(ParamByName('females_tally'), FFemalesTally);
      SetStrParam(ParamByName('not_sexed_tally'), FNotSexedTally);
      SetStrParam(ParamByName('adults_tally'), FAdultsTally);
      SetStrParam(ParamByName('immatures_tally'), FImmatureTally);
      SetStrParam(ParamByName('not_aged_tally'), FNotAgedTally);
      SetIntParam(ParamByName('new_captures_tally'), FNewCapturesTally);
      SetIntParam(ParamByName('recaptures_tally'), FRecapturesTally);
      SetIntParam(ParamByName('unbanded_tally'), FUnbandedTally);
      SetStrParam(ParamByName('detection_type'), FDetectionType);
      SetStrParam(ParamByName('breeding_status'), FBreedingStatus);
      ParamByName('not_surveying').AsBoolean := FNotSurveying;
      ParamByName('ebird_available').AsBoolean := FIsOnEbird;
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('sighting_id').AsInteger := FId;

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

function TSighting.Diff(aOld: TSighting; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.SightingDate, FSightingDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SightingTime, FSightingTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividuals, aOld.SubjectTally, FSubjectTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDistanceM, aOld.SubjectDistance, FSubjectDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMackinnonList, aOld.MackinnonListNumber, FMackinnonListNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCaptured, aOld.SubjectCaptured, FSubjectCaptured, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSeen, aOld.SubjectSeen, FSubjectSeen, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHeard, aOld.SubjectHeard, FSubjectHeard, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhotographed, aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAudioRecorded, aOld.SubjectRecorded, FSubjectRecorded, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMales, aOld.MalesTally, FMalesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFemales, aOld.FemalesTally, FFemalesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAdults, aOld.AdultsTally, FAdultsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscImmatures, aOld.ImmatureTally, FImmatureTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRecaptures, aOld.RecapturesTally, FRecapturesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNewCaptures, aOld.NewCapturesTally, FNewCapturesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscUnbanded, aOld.UnbandedTally, FUnbandedTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDetectionType, aOld.DetectionType, FDetectionType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBreedingCode, aOld.BreedingStatus, FBreedingStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOutOfSample, aOld.NotSurveying, FNotSurveying, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIsInEBird, aOld.IsOnEbird, FIsOnEbird, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSighting.Find(aSurvey, aTaxon, aObserver: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT sighting_id FROM sightings');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (taxon_id = :ataxon)');
    if aObserver > 0 then
      Add('AND (observer_id = :aobserver)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('ATAXON').AsInteger := aTaxon;
    if aObserver > 0 then
      ParamByName('AOBSERVER').AsInteger := aObserver;
    //  Add('and (time(AMO_HORA_INICIAL,''localtime'') =
    //    time('+QuotedStr(TimeToStr(Reg.RecordTime))+',''localtime''))');
//    GravaLogSQL(SQL);
    Open;
    Result := RecordCount > 0;
    if Result then
    begin
      GetData(FieldByName('sighting_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

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
      ParamByName('source_type').AsString := FeatherDataSourceStr[FSourceType];
      ParamByName('symmetrical').AsString := SymmetryStr[FSymmetrical];
      ParamByName('feather_trait').AsString := FeatherTraitStr[FFeatherTrait];
      SetIntParam(ParamByName('feather_number'), FFeatherNumber);
      ParamByName('body_side').AsString := BodySideStr[FBodySide];
      ParamByName('grown_percent').AsFloat := FPercentGrown;
      SetFloatParam(ParamByName('feather_length'), FFeatherLength);
      SetFloatParam(ParamByName('feather_area'), FFeatherArea);
      SetFloatParam(ParamByName('feather_mass'), FFeatherMass);
      SetFloatParam(ParamByName('rachis_width'), FRachisWidth);
      SetFloatParam(ParamByName('growth_bar_width'), FGrowthBarWidth);
      SetFloatParam(ParamByName('barb_density'), FBarbDensity);
      ParamByName('feather_age').AsString := FeatherAgeStr[FFeatherAge];
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
    JSONObject.Add('Source', FeatherDataSourceStr[FSourceType]);
    JSONObject.Add('Symmetry', SymmetryStr[FSymmetrical]);
    JSONObject.Add('Feather trait', FeatherTraitStr[FFeatherTrait]);
    JSONObject.Add('Feather number', FFeatherNumber);
    JSONObject.Add('Body side', BodySideStr[FBodySide]);
    JSONObject.Add('Percent grown', FPercentGrown);
    JSONObject.Add('Length', FFeatherLength);
    JSONObject.Add('Area', FFeatherArea);
    JSONObject.Add('Mass', FFeatherMass);
    JSONObject.Add('Rachis width', FRachisWidth);
    JSONObject.Add('Growth bar width', FGrowthBarWidth);
    JSONObject.Add('Barb density', FBarbDensity);
    JSONObject.Add('Age', FeatherAgeStr[FFeatherAge]);
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
      ParamByName('source_type').AsString := FeatherDataSourceStr[FSourceType];
      ParamByName('symmetrical').AsString := SymmetryStr[FSymmetrical];
      ParamByName('feather_trait').AsString := FeatherTraitStr[FFeatherTrait];
      SetIntParam(ParamByName('feather_number'), FFeatherNumber);
      ParamByName('body_side').AsString := BodySideStr[FBodySide];
      ParamByName('grown_percent').AsFloat := FPercentGrown;
      SetFloatParam(ParamByName('feather_length'), FFeatherLength);
      SetFloatParam(ParamByName('feather_area'), FFeatherArea);
      SetFloatParam(ParamByName('feather_mass'), FFeatherMass);
      SetFloatParam(ParamByName('rachis_width'), FRachisWidth);
      SetFloatParam(ParamByName('growth_bar_width'), FGrowthBarWidth);
      SetFloatParam(ParamByName('barb_density'), FBarbDensity);
      ParamByName('feather_age').AsString := FeatherAgeStr[FFeatherAge];
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

      FFullName := GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, Sexes[FSubjectSex],
        CaptureTypeStr[FCaptureType], FCycleCode, False);
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
      SetStrParam(ParamByName('subject_status'), SubjectStatusStr[FSubjectStatus]);
      SetStrParam(ParamByName('capture_type'), CaptureTypeStr[FCaptureType]);
      SetStrParam(ParamByName('subject_sex'), Sexes[FSubjectSex]);
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
      SetStrParam(ParamByName('subject_age'), Ages[FSubjectAge]);
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
    JSONObject.Add('Subject status', SubjectStatusStr[FSubjectStatus]);
    JSONObject.Add('Type', CaptureTypeStr[FCaptureType]);
    JSONObject.Add('Sex', Sexes[FSubjectSex]);
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

      FFullName := GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, Sexes[FSubjectSex],
        CaptureTypeStr[FCaptureType], FCycleCode, False);
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
      SetStrParam(ParamByName('subject_status'), SubjectStatusStr[FSubjectStatus]);
      SetStrParam(ParamByName('capture_type'), CaptureTypeStr[FCaptureType]);
      SetStrParam(ParamByName('subject_sex'), Sexes[FSubjectSex]);
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
      SetStrParam(ParamByName('subject_age'), Ages[FSubjectAge]);
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
      SetStrParam(ParamByName('individual_sex'), Sexes[FSex]);
      SetStrParam(ParamByName('individual_age'), Ages[FAge]);
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
      SetStrParam(ParamByName('formatted_name'), GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], True));
      FFullName := GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], False);
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
    JSONObject.Add('Sex', Sexes[FSex]);
    JSONObject.Add('Age', Ages[FAge]);
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
      SetStrParam(ParamByName('individual_sex'), Sexes[FSex]);
      SetStrParam(ParamByName('individual_age'), Ages[FAge]);
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
      SetStrParam(ParamByName('formatted_name'), GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], True));
      FFullName := GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], False);
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

{ TBand }

constructor TBand.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TBand.Clear;
begin
  inherited;
  FFullName := EmptyStr;
  FSize := EmptyStr;
  FNumber := 0;
  FStatus := bstAvailable;
  FSource := bscAcquiredFromSupplier;
  FPrefix := EmptyStr;
  FSuffix := EmptyStr;
  FSupplierId := 0;
  FBandColor := EmptyStr;
  FBandType := mkButtEndBand;
  FRequesterId := 0;
  FCarrierId := 0;
  FIndividualId := 0;
  FProjectId := 0;
  FReported := False;
  FNotes := EmptyStr;
end;

procedure TBand.Copy(aFrom: TBand);
begin
  FFullName := aFrom.FullName;
  FSize := aFrom.Size;
  FNumber := aFrom.Number;
  FStatus := aFrom.Status;
  FSource := aFrom.Source;
  FPrefix := aFrom.Prefix;
  FSuffix := aFrom.Suffix;
  FSupplierId := aFrom.SupplierId;
  FBandColor := aFrom.BandColor;
  FBandType := aFrom.BandType;
  FRequesterId := aFrom.RequesterId;
  FCarrierId := aFrom.CarrierId;
  FIndividualId := aFrom.IndividualId;
  FProjectId := aFrom.ProjectId;
  FReported := aFrom.Reported;
  FNotes := aFrom.Notes;
end;

procedure TBand.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBand.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM bands');
      Add('WHERE (band_id = :aid)');

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

procedure TBand.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'band_id, ' +
      'band_size, ' +
      'band_number, ' +
      'band_status, ' +
      'band_type, ' +
      'band_prefix, ' +
      'band_suffix, ' +
      'band_color, ' +
      'band_source, ' +
      'supplier_id, ' +
      'requester_id, ' +
      'carrier_id, ' +
      'individual_id, ' +
      'project_id, ' +
      'band_reported, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM bands');
    Add('WHERE band_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBand.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('band_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FSize := FieldByName('band_size').AsString;
    FNumber := FieldByName('band_number').AsInteger;
    case FieldByName('band_status').AsString of
      'D': FStatus := bstAvailable;
      'U': FStatus := bstUsed;
      'R': FStatus := bstRemoved;
      'Q': FStatus := bstBroken;
      'P': FStatus := bstLost;
      'T': FStatus := bstTransfered;
    end;
    case FieldByName('band_source').AsString of
      'A': FSource := bscAcquiredFromSupplier;
      'T': FSource := bscTransferBetweenBanders;
      'L': FSource := bscLivingBirdBandedByOthers;
      'D': FSource := bscDeadBirdBandedByOthers;
      'F': FSource := bscFoundLoose;
    end;
    FPrefix := FieldByName('band_prefix').AsString;
    FSuffix := FieldByName('band_suffix').AsString;
    FSupplierId := FieldByName('supplier_id').AsInteger;
    FBandColor := FieldByName('band_color').AsString;
    case FieldByName('band_type').AsString of
      'A': FBandType := mkButtEndBand;
      'F': FBandType := mkFlag;
      'N': FBandType := mkCollar;
      'W': FBandType := mkWingTag;
      'T': FBandType := mkTriangularBand;
      'L': FBandType := mkLockOnBand;
      'R': FBandType := mkRivetBand;
      'C': FBandType := mkClosedBand;
      'O': FBandType := mkOther;
    end;
    FRequesterId := FieldByName('requester_id').AsInteger;
    FCarrierId := FieldByName('carrier_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FReported := FieldByName('band_reported').AsBoolean;
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

procedure TBand.Insert;
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
      Add('INSERT INTO bands (' +
        'band_size, ' +
        'band_number, ' +
        'band_status, ' +
        'band_type, ' +
        'band_prefix, ' +
        'band_suffix, ' +
        'band_color, ' +
        'band_source, ' +
        'supplier_id, ' +
        'requester_id, ' +
        'carrier_id, ' +
        'project_id, ' +
        'notes, ' +
        'full_name, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':band_size, ' +
        ':band_number, ' +
        ':band_status, ' +
        ':band_type, ' +
        ':band_prefix, ' +
        ':band_suffix, ' +
        ':band_color, ' +
        ':band_source, ' +
        ':supplier_id, ' +
        ':requester_id, ' +
        ':carrier_id, ' +
        ':project_id, ' +
        ':notes, ' +
        ':full_name, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''));');

      SetStrParam(ParamByName('band_size'), FSize);
      SetIntParam(ParamByName('band_number'), FNumber);
      SetStrParam(ParamByName('band_status'), BandStatusStr[FStatus]);
      SetStrParam(ParamByName('band_type'), MarkTypesStr[FBandType]);
      SetStrParam(ParamByName('band_prefix'), FPrefix);
      SetStrParam(ParamByName('band_suffix'), FSuffix);
      SetStrParam(ParamByName('band_color'), FBandColor);
      SetStrParam(ParamByName('band_source'), BandSourceStr[FSource]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('carrier_id'), FCarrierId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      FFullName := GetBandFullname(FSize, FNumber, FSupplierId);
      SetStrParam(ParamByName('full_name'), FFullName);
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

procedure TBand.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TBand.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Size', FSize);
    JSONObject.Add('Number', FNumber);
    JSONObject.Add('Status', BandStatusStr[FStatus]);
    JSONObject.Add('Source', BandSourceStr[FSource]);
    JSONObject.Add('Prefix', FPrefix);
    JSONObject.Add('Suffix', FSuffix);
    JSONObject.Add('Color', FBandColor);
    JSONObject.Add('Type', MarkTypesStr[FBandType]);
    JSONObject.Add('Supplier', FSupplierId);
    JSONObject.Add('Requester', FRequesterId);
    JSONObject.Add('Carrier', FCarrierId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Reported', FReported);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TBand.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TBand.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE bands SET ' +
        'band_size = :band_size, ' +
        'band_number = :band_number, ' +
        'band_status = :band_status, ' +
        'band_type = :band_type, ' +
        'band_prefix = :band_prefix, ' +
        'band_suffix = :band_suffix, ' +
        'band_color = :band_color, ' +
        'band_source = :band_source, ' +
        'supplier_id = :supplier_id, ' +
        'requester_id = :requester_id, ' +
        'carrier_id = :carrier_id, ' +
        'project_id = :project_id, ' +
        'notes = :notes, ' +
        'full_name = :full_name, ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (band_id = :band_id)');

      SetStrParam(ParamByName('band_size'), FSize);
      SetIntParam(ParamByName('band_number'), FNumber);
      SetStrParam(ParamByName('band_status'), BandStatusStr[FStatus]);
      SetStrParam(ParamByName('band_type'), MarkTypesStr[FBandType]);
      SetStrParam(ParamByName('band_prefix'), FPrefix);
      SetStrParam(ParamByName('band_suffix'), FSuffix);
      SetStrParam(ParamByName('band_color'), FBandColor);
      SetStrParam(ParamByName('band_source'), BandSourceStr[FSource]);
      SetForeignParam(ParamByName('supplier_id'), FSupplierId);
      SetForeignParam(ParamByName('requester_id'), FRequesterId);
      SetForeignParam(ParamByName('carrier_id'), FCarrierId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      FFullName := GetBandFullname(FSize, FNumber, FSupplierId);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('band_id').AsInteger := FId;

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

function TBand.Find(aSize: String; aNumber: Integer): Boolean;
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
    Add('SELECT band_id FROM bands');
    Add('WHERE (band_size = :asize)');
    Add('AND (band_number = :anumber)');
    ParamByName('ASIZE').AsString := aSize;
    ParamByName('ANUMBER').AsInteger := aNumber;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('band_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TBand.Diff(aOld: TBand; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSize, aOld.Size, FSize, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNumber, aOld.Number, FNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStatus, aOld.Status, FStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSource, aOld.Source, FSource, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPrefix, aOld.Prefix, FPrefix, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSuffix, aOld.Suffix, FSuffix, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupplierID, aOld.SupplierId, FSupplierId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscColor, aOld.BandColor, FBandColor, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.BandType, FBandType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRequesterID, aOld.RequesterId, FRequesterId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCarrierID, aOld.CarrierId, FCarrierId, R) then
    aList.Add(R);
  //if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscReported, aOld.Reported, FReported, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

end.

