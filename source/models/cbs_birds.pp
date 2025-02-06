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

  { TMolt }

  TMolt = class(TXolmisRecord)
  protected
    FFullName: String;
    FSurveyId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FCaptureId: Integer;
    FCaptureDate: TDate;
    FCaptureTime: TTime;
    FBanderId: Integer;
    FBandId: Integer;
    FPrimary1: Double;
    FPrimary2: Double;
    FPrimary3: Double;
    FPrimary4: Double;
    FPrimary5: Double;
    FPrimary6: Double;
    FPrimary7: Double;
    FPrimary8: Double;
    FPrimary9: Double;
    FPrimary10: Double;
    FSecondary1: Double;
    FSecondary2: Double;
    FSecondary3: Double;
    FSecondary4: Double;
    FSecondary5: Double;
    FSecondary6: Double;
    FSecondary7: Double;
    FSecondary8: Double;
    FSecondary9: Double;
    FRetrix1: Double;
    FRetrix2: Double;
    FRetrix3: Double;
    FRetrix4: Double;
    FRetrix5: Double;
    FRetrix6: Double;
    FPrimaryCovert1: Double;
    FPrimaryCovert2: Double;
    FPrimaryCovert3: Double;
    FPrimaryCovert4: Double;
    FPrimaryCovert5: Double;
    FPrimaryCovert6: Double;
    FPrimaryCovert7: Double;
    FPrimaryCovert8: Double;
    FPrimaryCovert9: Double;
    FCarpalCovert: Double;
    FGreatCovert1: Double;
    FGreatCovert2: Double;
    FGreatCovert3: Double;
    FGreatCovert4: Double;
    FGreatCovert5: Double;
    FGreatCovert6: Double;
    FGreatCovert7: Double;
    FGreatCovert8: Double;
    FGreatCovert9: Double;
    FGreatCovert10: Double;
    FAlula1: Double;
    FAlula2: Double;
    FAlula3: Double;
    FLeastCoverts: Double;
    FMedianCoverts: Double;
    FGrowthBarWidth: Double;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TMolt; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TMolt);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property CaptureDate: TDate read FCaptureDate write FCaptureDate;
    property CaptureTime: TTime read FCaptureTime write FCaptureTime;
    property BanderId: Integer read FBanderId write FBanderId;
    property BandId: Integer read FBandId write FBandId;
    property Primary1: Double read FPrimary1 write FPrimary1;
    property Primary2: Double read FPrimary2 write FPrimary2;
    property Primary3: Double read FPrimary3 write FPrimary3;
    property Primary4: Double read FPrimary4 write FPrimary4;
    property Primary5: Double read FPrimary5 write FPrimary5;
    property Primary6: Double read FPrimary6 write FPrimary6;
    property Primary7: Double read FPrimary7 write FPrimary7;
    property Primary8: Double read FPrimary8 write FPrimary8;
    property Primary9: Double read FPrimary9 write FPrimary9;
    property Primary10: Double read FPrimary10 write FPrimary10;
    property Secondary1: Double read FSecondary1 write FSecondary1;
    property Secondary2: Double read FSecondary2 write FSecondary2;
    property Secondary3: Double read FSecondary3 write FSecondary3;
    property Secondary4: Double read FSecondary4 write FSecondary4;
    property Secondary5: Double read FSecondary5 write FSecondary5;
    property Secondary6: Double read FSecondary6 write FSecondary6;
    property Secondary7: Double read FSecondary7 write FSecondary7;
    property Secondary8: Double read FSecondary8 write FSecondary8;
    property Secondary9: Double read FSecondary9 write FSecondary9;
    property Retrix1: Double read FRetrix1 write FRetrix1;
    property Retrix2: Double read FRetrix2 write FRetrix2;
    property Retrix3: Double read FRetrix3 write FRetrix3;
    property Retrix4: Double read FRetrix4 write FRetrix4;
    property Retrix5: Double read FRetrix5 write FRetrix5;
    property Retrix6: Double read FRetrix6 write FRetrix6;
    property PrimaryCovert1: Double read FPrimaryCovert1 write FPrimaryCovert1;
    property PrimaryCovert2: Double read FPrimaryCovert2 write FPrimaryCovert2;
    property PrimaryCovert3: Double read FPrimaryCovert3 write FPrimaryCovert3;
    property PrimaryCovert4: Double read FPrimaryCovert4 write FPrimaryCovert4;
    property PrimaryCovert5: Double read FPrimaryCovert5 write FPrimaryCovert5;
    property PrimaryCovert6: Double read FPrimaryCovert6 write FPrimaryCovert6;
    property PrimaryCovert7: Double read FPrimaryCovert7 write FPrimaryCovert7;
    property PrimaryCovert8: Double read FPrimaryCovert8 write FPrimaryCovert8;
    property PrimaryCovert9: Double read FPrimaryCovert9 write FPrimaryCovert9;
    property CarpalCovert: Double read FCarpalCovert write FCarpalCovert;
    property GreatCovert1: Double read FGreatCovert1 write FGreatCovert1;
    property GreatCovert2: Double read FGreatCovert2 write FGreatCovert2;
    property GreatCovert3: Double read FGreatCovert3 write FGreatCovert3;
    property GreatCovert4: Double read FGreatCovert4 write FGreatCovert4;
    property GreatCovert5: Double read FGreatCovert5 write FGreatCovert5;
    property GreatCovert6: Double read FGreatCovert6 write FGreatCovert6;
    property GreatCovert7: Double read FGreatCovert7 write FGreatCovert7;
    property GreatCovert8: Double read FGreatCovert8 write FGreatCovert8;
    property GreatCovert9: Double read FGreatCovert9 write FGreatCovert9;
    property GreatCovert10: Double read FGreatCovert10 write FGreatCovert10;
    property Alula1: Double read FAlula1 write FAlula1;
    property Alula2: Double read FAlula2 write FAlula2;
    property Alula3: Double read FAlula3 write FAlula3;
    property LeastCoverts: Double read FLeastCoverts write FLeastCoverts;
    property MedianCoverts: Double read FMedianCoverts write FMedianCoverts;
    property GrowthBarWidth: Double read FGrowthBarWidth write FGrowthBarWidth;
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
  cbs_system, cbs_global, cbs_users, cbs_validations, cbs_fullnames, cbs_datacolumns, udm_main;

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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM band_history');
    Add('WHERE (event_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
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
    ParamByName('event_date').AsString := FormatDateTime('yyyy-MM-dd', FEventDate);
    ParamByName('notes').AsString := FNotes;
    ParamByName('event_type').AsString := BandEventStr[FEventType];
    if FSupplierId > 0 then
      ParamByName('supplier_id').AsInteger := FSupplierId
    else
      ParamByName('supplier_id').Clear;
    ParamByName('order_number').AsInteger := FOrderNumber;
    if FRequesterId > 0 then
      ParamByName('requester_id').AsInteger := FRequesterId
    else
      ParamByName('requester_id').Clear;
    if FSenderId > 0 then
      ParamByName('sender_id').AsInteger := FSenderId
    else
      ParamByName('sender_id').Clear;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TBandHistory.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
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
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
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
    ParamByName('event_date').AsString := FormatDateTime('yyyy-MM-dd', FEventDate);
    ParamByName('notes').AsString := FNotes;
    ParamByName('event_type').AsString := BandEventStr[FEventType];
    if FSupplierId > 0 then
      ParamByName('supplier_id').AsInteger := FSupplierId
    else
      ParamByName('supplier_id').Clear;
    ParamByName('order_number').AsInteger := FOrderNumber;
    if FRequesterId > 0 then
      ParamByName('requester_id').AsInteger := FRequesterId
    else
      ParamByName('requester_id').Clear;
    if FSenderId > 0 then
      ParamByName('sender_id').AsInteger := FSenderId
    else
      ParamByName('sender_id').Clear;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ParamByName('event_id').AsInteger := FId;

    ExecSQL;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM sightings');
    Add('WHERE (sighting_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
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
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
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
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
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
    ParamByName('survey_id').AsInteger := FSurveyId;
    ParamByName('individual_id').AsInteger := FIndividualId;
    ParamByName('taxon_id').AsInteger := FTaxonId;
    ParamByName('sighting_date').AsString := FormatDateTime('yyyy-MM-dd', FSightingDate);
    ParamByName('sighting_time').AsString := TimeToStr(FSightingTime);
    ParamByName('locality_id').AsInteger := FLocalityId;
    if (FLongitude <> 0) and (FLatitude <> 0) then
    begin
      ParamByName('longitude').AsFloat := FLongitude;
      ParamByName('latitude').AsFloat := FLatitude;
    end
    else
    begin
      ParamByName('longitude').Clear;
      ParamByName('latitude').Clear;
    end;
    ParamByName('method_id').AsInteger := FMethodId;
    ParamByName('mackinnon_list_num').AsInteger := FMackinnonListNumber;
    ParamByName('observer_id').AsInteger := FObserverId;
    if FSubjectTally > 0 then
      ParamByName('subjects_tally').AsInteger := FSubjectTally
    else
      ParamByName('subjects_tally').Clear;
    if FSubjectDistance > 0 then
      ParamByName('subject_distance').AsFloat := FSubjectDistance
    else
      ParamByName('subject_distance').Clear;
    ParamByName('subject_captured').AsBoolean := FSubjectCaptured;
    ParamByName('subject_seen').AsBoolean := FSubjectSeen;
    ParamByName('subject_heard').AsBoolean := FSubjectHeard;
    ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
    ParamByName('subject_recorded').AsBoolean := FSubjectRecorded;
    if FMalesTally <> EmptyStr then
      ParamByName('males_tally').AsString := FMalesTally
    else
      ParamByName('males_tally').Clear;
    if FFemalesTally <> EmptyStr then
      ParamByName('females_tally').AsString := FFemalesTally
    else
      ParamByName('females_tally').Clear;
    if FNotSexedTally <> EmptyStr then
      ParamByName('not_sexed_tally').AsString := FNotSexedTally
    else
      ParamByName('not_sexed_tally').Clear;
    if FAdultsTally <> EmptyStr then
      ParamByName('adults_tally').AsString := FAdultsTally
    else
      ParamByName('adults_tally').Clear;
    if FImmatureTally <> EmptyStr then
      ParamByName('immatures_tally').AsString := FImmatureTally
    else
      ParamByName('immatures_tally').Clear;
    if FNotAgedTally <> EmptyStr then
      ParamByName('not_aged_tally').AsString := FNotAgedTally
    else
      ParamByName('not_aged_tally').Clear;
    if FNewCapturesTally > 0 then
      ParamByName('new_captures_tally').AsInteger := FNewCapturesTally
    else
      ParamByName('new_captures_tally').Clear;
    if FRecapturesTally > 0 then
      ParamByName('recaptures_tally').AsInteger := FRecapturesTally
    else
      ParamByName('recaptures_tally').Clear;
    if FUnbandedTally > 0 then
      ParamByName('unbanded_tally').AsInteger := FUnbandedTally
    else
      ParamByName('unbanded_tally').Clear;
    ParamByName('detection_type').AsString := FDetectionType;
    ParamByName('breeding_status').AsString := FBreedingStatus;
    ParamByName('not_surveying').AsBoolean := FNotSurveying;
    ParamByName('ebird_available').AsBoolean := FIsOnEbird;
    ParamByName('notes').AsString := FNotes;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;

    //// Get the taxon hierarchy
    //if (FTaxonId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
    //  Add('WHERE taxon_id = :ataxon');
    //  ParamByName('ataxon').AsInteger := FTaxonId;
    //  Open;
    //  FOrderId := FieldByName('order_id').AsInteger;
    //  FFamilyId := FieldByName('family_id').AsInteger;
    //  FGenusId := FieldByName('genus_id').AsInteger;
    //  FSpeciesId := FieldByName('species_id').AsInteger;
    //  Close;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add('UPDATE sightings SET');
    //Add('  order_id = :order_id,');
    //Add('  family_id = :family_id,');
    //Add('  genus_id = :genus_id,');
    //Add('  species_id = :species_id');
    //Add('WHERE sighting_id = :aid');
    //ParamByName('order_id').AsInteger := FOrderId;
    //if (FFamilyId > 0) then
    //  ParamByName('family_id').AsInteger := FFamilyId
    //else
    //  ParamByName('family_id').Clear;
    //if (FGenusId > 0) then
    //  ParamByName('genus_id').AsInteger := FGenusId
    //else
    //  ParamByName('genus_id').Clear;
    //if (FSpeciesId > 0) then
    //  ParamByName('species_id').AsInteger := FSpeciesId
    //else
    //  ParamByName('species_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
    //
    //// Get the site hierarchy
    //if (FLocalityId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
    //  Add('WHERE site_id = :asite');
    //  ParamByName('ASITE').AsInteger := FLocalityId;
    //  Open;
    //  FCountryId := FieldByName('country_id').AsInteger;
    //  FStateId := FieldByName('state_id').AsInteger;
    //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
    //  Close;
    //end;
    //// Save the site hierarchy
    //Clear;
    //Add('UPDATE sightings SET');
    //Add('  country_id = :country_id,');
    //Add('  state_id = :state_id,');
    //Add('  municipality_id = :municipality_id');
    //Add('WHERE sighting_id = :aid');
    //ParamByName('country_id').AsInteger := FCountryId;
    //if (FStateId > 0) then
    //  ParamByName('state_id').AsInteger := FStateId
    //else
    //  ParamByName('state_id').Clear;
    //if (FMunicipalityId > 0) then
    //  ParamByName('municipality_id').AsInteger := FMunicipalityId
    //else
    //  ParamByName('municipality_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('UPDATE sightings SET ' +
      'survey_id = :survey_id, ' +
      'individual_id = :individual_id, ' +
      'sighting_date = date(:sighting_date), ' +
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
    ParamByName('survey_id').AsInteger := FSurveyId;
    ParamByName('individual_id').AsInteger := FIndividualId;
    ParamByName('taxon_id').AsInteger := FTaxonId;
    ParamByName('sighting_date').AsString := FormatDateTime('yyyy-MM-dd', FSightingDate);
    ParamByName('sighting_time').AsString := TimeToStr(FSightingTime);
    ParamByName('locality_id').AsInteger := FLocalityId;
    if (FLongitude <> 0) and (FLatitude <> 0) then
    begin
      ParamByName('longitude').AsFloat := FLongitude;
      ParamByName('latitude').AsFloat := FLatitude;
    end
    else
    begin
      ParamByName('longitude').Clear;
      ParamByName('latitude').Clear;
    end;
    ParamByName('method_id').AsInteger := FMethodId;
    ParamByName('mackinnon_list_num').AsInteger := FMackinnonListNumber;
    ParamByName('observer_id').AsInteger := FObserverId;
    if FSubjectTally > 0 then
      ParamByName('subjects_tally').AsInteger := FSubjectTally
    else
      ParamByName('subjects_tally').Clear;
    if FSubjectDistance > 0 then
      ParamByName('subject_distance').AsFloat := FSubjectDistance
    else
      ParamByName('subject_distance').Clear;
    ParamByName('subject_captured').AsBoolean := FSubjectCaptured;
    ParamByName('subject_seen').AsBoolean := FSubjectSeen;
    ParamByName('subject_heard').AsBoolean := FSubjectHeard;
    ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
    ParamByName('subject_recorded').AsBoolean := FSubjectRecorded;
    if FMalesTally <> EmptyStr then
      ParamByName('males_tally').AsString := FMalesTally
    else
      ParamByName('males_tally').Clear;
    if FFemalesTally <> EmptyStr then
      ParamByName('females_tally').AsString := FFemalesTally
    else
      ParamByName('females_tally').Clear;
    if FNotSexedTally <> EmptyStr then
      ParamByName('not_sexed_tally').AsString := FNotSexedTally
    else
      ParamByName('not_sexed_tally').Clear;
    if FAdultsTally <> EmptyStr then
      ParamByName('adults_tally').AsString := FAdultsTally
    else
      ParamByName('adults_tally').Clear;
    if FImmatureTally <> EmptyStr then
      ParamByName('immatures_tally').AsString := FImmatureTally
    else
      ParamByName('immatures_tally').Clear;
    if FNotAgedTally <> EmptyStr then
      ParamByName('not_aged_tally').AsString := FNotAgedTally
    else
      ParamByName('not_aged_tally').Clear;
    if FNewCapturesTally > 0 then
      ParamByName('new_captures_tally').AsInteger := FNewCapturesTally
    else
      ParamByName('new_captures_tally').Clear;
    if FRecapturesTally > 0 then
      ParamByName('recaptures_tally').AsInteger := FRecapturesTally
    else
      ParamByName('recaptures_tally').Clear;
    if FUnbandedTally > 0 then
      ParamByName('unbanded_tally').AsInteger := FUnbandedTally
    else
      ParamByName('unbanded_tally').Clear;
    ParamByName('detection_type').AsString := FDetectionType;
    ParamByName('breeding_status').AsString := FBreedingStatus;
    ParamByName('not_surveying').AsBoolean := FNotSurveying;
    ParamByName('ebird_available').AsBoolean := FIsOnEbird;
    ParamByName('notes').AsString := FNotes;
    ParamByName('marked_status').AsBoolean := FMarked;
    ParamByName('active_status').AsBoolean := FActive;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('sighting_id').AsInteger := FId;

    ExecSQL;

    //// Get the taxon hierarchy
    //if (FTaxonId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
    //  Add('WHERE taxon_id = :ataxon');
    //  ParamByName('ataxon').AsInteger := FTaxonId;
    //  Open;
    //  FOrderId := FieldByName('order_id').AsInteger;
    //  FFamilyId := FieldByName('family_id').AsInteger;
    //  FGenusId := FieldByName('genus_id').AsInteger;
    //  FSpeciesId := FieldByName('species_id').AsInteger;
    //  Close;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add('UPDATE sightings SET');
    //Add('  order_id = :order_id,');
    //Add('  family_id = :family_id,');
    //Add('  genus_id = :genus_id,');
    //Add('  species_id = :species_id');
    //Add('WHERE sighting_id = :aid');
    //ParamByName('order_id').AsInteger := FOrderId;
    //if (FFamilyId > 0) then
    //  ParamByName('family_id').AsInteger := FFamilyId
    //else
    //  ParamByName('family_id').Clear;
    //if (FGenusId > 0) then
    //  ParamByName('genus_id').AsInteger := FGenusId
    //else
    //  ParamByName('genus_id').Clear;
    //if (FSpeciesId > 0) then
    //  ParamByName('species_id').AsInteger := FSpeciesId
    //else
    //  ParamByName('species_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
    //
    //// Get the site hierarchy
    //if (FLocalityId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
    //  Add('WHERE site_id = :asite');
    //  ParamByName('ASITE').AsInteger := FLocalityId;
    //  Open;
    //  FCountryId := FieldByName('country_id').AsInteger;
    //  FStateId := FieldByName('state_id').AsInteger;
    //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
    //  Close;
    //end;
    //// Save the site hierarchy
    //Clear;
    //Add('UPDATE sightings SET');
    //Add('  country_id = :country_id,');
    //Add('  state_id = :state_id,');
    //Add('  municipality_id = :municipality_id');
    //Add('WHERE sighting_id = :aid');
    //ParamByName('country_id').AsInteger := FCountryId;
    //if (FStateId > 0) then
    //  ParamByName('state_id').AsInteger := FStateId
    //else
    //  ParamByName('state_id').Clear;
    //if (FMunicipalityId > 0) then
    //  ParamByName('municipality_id').AsInteger := FMunicipalityId
    //else
    //  ParamByName('municipality_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
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
  //if FieldValuesDiff(rsCaptionOrder, aOld.OrderId, OrderId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionFamily, aOld.FamilyId, FamilyId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionGenus, aOld.GenusId, GenusId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionSpecies, aOld.SpeciesId, SpeciesId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, MunicipalityId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionState, aOld.StateId, StateId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, CountryId, R) then
  //  aList.Add(R);
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

{ TMolt }

constructor TMolt.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TMolt.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FSurveyId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FCaptureId := 0;
  FCaptureDate := NullDate;
  FCaptureTime := NullTime;
  FBanderId := 0;
  FBandId := 0;
  FPrimary1 := 0.0;
  FPrimary2 := 0.0;
  FPrimary3 := 0.0;
  FPrimary4 := 0.0;
  FPrimary5 := 0.0;
  FPrimary6 := 0.0;
  FPrimary7 := 0.0;
  FPrimary8 := 0.0;
  FPrimary9 := 0.0;
  FPrimary10 := 0.0;
  FSecondary1 := 0.0;
  FSecondary2 := 0.0;
  FSecondary3 := 0.0;
  FSecondary4 := 0.0;
  FSecondary5 := 0.0;
  FSecondary6 := 0.0;
  FSecondary7 := 0.0;
  FSecondary8 := 0.0;
  FSecondary9 := 0.0;
  FRetrix1 := 0.0;
  FRetrix2 := 0.0;
  FRetrix3 := 0.0;
  FRetrix4 := 0.0;
  FRetrix5 := 0.0;
  FRetrix6 := 0.0;
  FPrimaryCovert1 := 0.0;
  FPrimaryCovert2 := 0.0;
  FPrimaryCovert3 := 0.0;
  FPrimaryCovert4 := 0.0;
  FPrimaryCovert5 := 0.0;
  FPrimaryCovert6 := 0.0;
  FPrimaryCovert7 := 0.0;
  FPrimaryCovert8 := 0.0;
  FPrimaryCovert9 := 0.0;
  FCarpalCovert := 0.0;
  FGreatCovert1 := 0.0;
  FGreatCovert2 := 0.0;
  FGreatCovert3 := 0.0;
  FGreatCovert4 := 0.0;
  FGreatCovert5 := 0.0;
  FGreatCovert6 := 0.0;
  FGreatCovert7 := 0.0;
  FGreatCovert8 := 0.0;
  FGreatCovert9 := 0.0;
  FGreatCovert10 := 0.0;
  FAlula1 := 0.0;
  FAlula2 := 0.0;
  FAlula3 := 0.0;
  FLeastCoverts := 0.0;
  FMedianCoverts := 0.0;
  FGrowthBarWidth := 0.0;
  FNotes := EmptyStr;
end;

procedure TMolt.Copy(aFrom: TMolt);
begin
  FFullName := aFrom.FullName;
  FSurveyId := aFrom.SurveyId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FCaptureId := aFrom.CaptureId;
  FCaptureDate := aFrom.CaptureDate;
  FCaptureTime := aFrom.CaptureTime;
  FBanderId := aFrom.BanderId;
  FBandId := aFrom.BandId;
  FPrimary1 := aFrom.Primary1;
  FPrimary2 := aFrom.Primary2;
  FPrimary3 := aFrom.Primary3;
  FPrimary4 := aFrom.Primary4;
  FPrimary5 := aFrom.Primary5;
  FPrimary6 := aFrom.Primary6;
  FPrimary7 := aFrom.Primary7;
  FPrimary8 := aFrom.Primary8;
  FPrimary9 := aFrom.Primary9;
  FPrimary10 := aFrom.Primary10;
  FSecondary1 := aFrom.Secondary1;
  FSecondary2 := aFrom.Secondary2;
  FSecondary3 := aFrom.Secondary3;
  FSecondary4 := aFrom.Secondary4;
  FSecondary5 := aFrom.Secondary5;
  FSecondary6 := aFrom.Secondary6;
  FSecondary7 := aFrom.Secondary7;
  FSecondary8 := aFrom.Secondary8;
  FSecondary9 := aFrom.Secondary9;
  FRetrix1 := aFrom.Retrix1;
  FRetrix2 := aFrom.Retrix2;
  FRetrix3 := aFrom.Retrix3;
  FRetrix4 := aFrom.Retrix4;
  FRetrix5 := aFrom.Retrix5;
  FRetrix6 := aFrom.Retrix6;
  FPrimaryCovert1 := aFrom.PrimaryCovert1;
  FPrimaryCovert2 := aFrom.PrimaryCovert2;
  FPrimaryCovert3 := aFrom.PrimaryCovert3;
  FPrimaryCovert4 := aFrom.PrimaryCovert4;
  FPrimaryCovert5 := aFrom.PrimaryCovert5;
  FPrimaryCovert6 := aFrom.PrimaryCovert6;
  FPrimaryCovert7 := aFrom.PrimaryCovert7;
  FPrimaryCovert8 := aFrom.PrimaryCovert8;
  FPrimaryCovert9 := aFrom.PrimaryCovert9;
  FCarpalCovert := aFrom.CarpalCovert;
  FGreatCovert1 := aFrom.GreatCovert1;
  FGreatCovert2 := aFrom.GreatCovert2;
  FGreatCovert3 := aFrom.GreatCovert3;
  FGreatCovert4 := aFrom.GreatCovert4;
  FGreatCovert5 := aFrom.GreatCovert5;
  FGreatCovert6 := aFrom.GreatCovert6;
  FGreatCovert7 := aFrom.GreatCovert7;
  FGreatCovert8 := aFrom.GreatCovert8;
  FGreatCovert9 := aFrom.GreatCovert9;
  FGreatCovert10 := aFrom.GreatCovert10;
  FAlula1 := aFrom.Alula1;
  FAlula2 := aFrom.Alula2;
  FAlula3 := aFrom.Alula3;
  FLeastCoverts := aFrom.LeastCoverts;
  FMedianCoverts := aFrom.MedianCoverts;
  FGrowthBarWidth := aFrom.GrowthBarWidth;
  FNotes := aFrom.Notes;
end;

procedure TMolt.Delete;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM molts');
    Add('WHERE (molt_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMolt.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'molt_id, ' +
      'survey_id, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'bander_id, ' +
      'band_id, ' +
      'p1_molt, ' +
      'p2_molt, ' +
      'p3_molt, ' +
      'p4_molt, ' +
      'p5_molt, ' +
      'p6_molt, ' +
      'p7_molt, ' +
      'p8_molt, ' +
      'p9_molt, ' +
      'p10_molt, ' +
      's1_molt, ' +
      's2_molt, ' +
      's3_molt, ' +
      's4_molt, ' +
      's5_molt, ' +
      's6_molt, ' +
      's7_molt, ' +
      's8_molt, ' +
      's9_molt, ' +
      'r1_molt, ' +
      'r2_molt, ' +
      'r3_molt, ' +
      'r4_molt, ' +
      'r5_molt, ' +
      'r6_molt, ' +
      'pc1_molt, ' +
      'pc2_molt, ' +
      'pc3_molt, ' +
      'pc4_molt, ' +
      'pc5_molt, ' +
      'pc6_molt, ' +
      'pc7_molt, ' +
      'pc8_molt, ' +
      'pc9_molt, ' +
      'cc_molt, ' +
      'gc1_molt, ' +
      'gc2_molt, ' +
      'gc3_molt, ' +
      'gc4_molt, ' +
      'gc5_molt, ' +
      'gc6_molt, ' +
      'gc7_molt, ' +
      'gc8_molt, ' +
      'gc9_molt, ' +
      'gc10_molt, ' +
      'al1_molt, ' +
      'al2_molt, ' +
      'al3_molt, ' +
      'lc_molt, ' +
      'mc_molt, ' +
      'growth_bar_size, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM molts');
    Add('WHERE molt_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMolt.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('molt_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FCaptureId := FieldByName('capture_id').AsInteger;
    FCaptureDate := FieldByName('sample_date').AsDateTime;
    FCaptureTime := FieldByName('sample_time').AsDateTime;
    FBanderId := FieldByName('bander_id').AsInteger;
    FBandId := FieldByName('band_id').AsInteger;
    FPrimary1 := FieldByName('p1_molt').AsFloat;
    FPrimary2 := FieldByName('p2_molt').AsFloat;
    FPrimary3 := FieldByName('p3_molt').AsFloat;
    FPrimary4 := FieldByName('p4_molt').AsFloat;
    FPrimary5 := FieldByName('p5_molt').AsFloat;
    FPrimary6 := FieldByName('p6_molt').AsFloat;
    FPrimary7 := FieldByName('p7_molt').AsFloat;
    FPrimary8 := FieldByName('p8_molt').AsFloat;
    FPrimary9 := FieldByName('p9_molt').AsFloat;
    FPrimary10 := FieldByName('p10_molt').AsFloat;
    FSecondary1 := FieldByName('s1_molt').AsFloat;
    FSecondary2 := FieldByName('s2_molt').AsFloat;
    FSecondary3 := FieldByName('s3_molt').AsFloat;
    FSecondary4 := FieldByName('s4_molt').AsFloat;
    FSecondary5 := FieldByName('s5_molt').AsFloat;
    FSecondary6 := FieldByName('s6_molt').AsFloat;
    FSecondary7 := FieldByName('s7_molt').AsFloat;
    FSecondary8 := FieldByName('s8_molt').AsFloat;
    FSecondary9 := FieldByName('s9_molt').AsFloat;
    FRetrix1 := FieldByName('r1_molt').AsFloat;
    FRetrix2 := FieldByName('r2_molt').AsFloat;
    FRetrix3 := FieldByName('r3_molt').AsFloat;
    FRetrix4 := FieldByName('r4_molt').AsFloat;
    FRetrix5 := FieldByName('r5_molt').AsFloat;
    FRetrix6 := FieldByName('r6_molt').AsFloat;
    FPrimaryCovert1 := FieldByName('pc1_molt').AsFloat;
    FPrimaryCovert2 := FieldByName('pc2_molt').AsFloat;
    FPrimaryCovert3 := FieldByName('pc3_molt').AsFloat;
    FPrimaryCovert4 := FieldByName('pc4_molt').AsFloat;
    FPrimaryCovert5 := FieldByName('pc5_molt').AsFloat;
    FPrimaryCovert6 := FieldByName('pc6_molt').AsFloat;
    FPrimaryCovert7 := FieldByName('pc7_molt').AsFloat;
    FPrimaryCovert8 := FieldByName('pc8_molt').AsFloat;
    FPrimaryCovert9 := FieldByName('pc9_molt').AsFloat;
    FCarpalCovert := FieldByName('cc_molt').AsFloat;
    FGreatCovert1 := FieldByName('gc1_molt').AsFloat;
    FGreatCovert2 := FieldByName('gc2_molt').AsFloat;
    FGreatCovert3 := FieldByName('gc3_molt').AsFloat;
    FGreatCovert4 := FieldByName('gc4_molt').AsFloat;
    FGreatCovert5 := FieldByName('gc5_molt').AsFloat;
    FGreatCovert6 := FieldByName('gc6_molt').AsFloat;
    FGreatCovert7 := FieldByName('gc7_molt').AsFloat;
    FGreatCovert8 := FieldByName('gc8_molt').AsFloat;
    FGreatCovert9 := FieldByName('gc9_molt').AsFloat;
    FGreatCovert10 := FieldByName('gc10_molt').AsFloat;
    FAlula1 := FieldByName('al1_molt').AsFloat;
    FAlula2 := FieldByName('al2_molt').AsFloat;
    FAlula3 := FieldByName('al3_molt').AsFloat;
    FLeastCoverts := FieldByName('lc_molt').AsFloat;
    FMedianCoverts := FieldByName('mc_molt').AsFloat;
    FGrowthBarWidth := FieldByName('growth_bar_size').AsFloat;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TMolt.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO molts (' +
        'survey_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'sample_date, ' +
        'sample_time, ' +
        'bander_id, ' +
        'band_id, ' +
        'p1_molt, ' +
        'p2_molt, ' +
        'p3_molt, ' +
        'p4_molt, ' +
        'p5_molt, ' +
        'p6_molt, ' +
        'p7_molt, ' +
        'p8_molt, ' +
        'p9_molt, ' +
        'p10_molt, ' +
        's1_molt, ' +
        's2_molt, ' +
        's3_molt, ' +
        's4_molt, ' +
        's5_molt, ' +
        's6_molt, ' +
        's7_molt, ' +
        's8_molt, ' +
        's9_molt, ' +
        'r1_molt, ' +
        'r2_molt, ' +
        'r3_molt, ' +
        'r4_molt, ' +
        'r5_molt, ' +
        'r6_molt, ' +
        'pc1_molt, ' +
        'pc2_molt, ' +
        'pc3_molt, ' +
        'pc4_molt, ' +
        'pc5_molt, ' +
        'pc6_molt, ' +
        'pc7_molt, ' +
        'pc8_molt, ' +
        'pc9_molt, ' +
        'cc_molt, ' +
        'gc1_molt, ' +
        'gc2_molt, ' +
        'gc3_molt, ' +
        'gc4_molt, ' +
        'gc5_molt, ' +
        'gc6_molt, ' +
        'gc7_molt, ' +
        'gc8_molt, ' +
        'gc9_molt, ' +
        'gc10_molt, ' +
        'al1_molt, ' +
        'al2_molt, ' +
        'al3_molt, ' +
        'lc_molt, ' +
        'mc_molt, ' +
        'growth_bar_size, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':survey_id, ' +
        ':full_name, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':capture_id, ' +
        'date(:sample_date), ' +
        'time(:sample_time), ' +
        ':bander_id, ' +
        ':band_id, ' +
        ':p1_molt, ' +
        ':p2_molt, ' +
        ':p3_molt, ' +
        ':p4_molt, ' +
        ':p5_molt, ' +
        ':p6_molt, ' +
        ':p7_molt, ' +
        ':p8_molt, ' +
        ':p9_molt, ' +
        ':p10_molt, ' +
        ':s1_molt, ' +
        ':s2_molt, ' +
        ':s3_molt, ' +
        ':s4_molt, ' +
        ':s5_molt, ' +
        ':s6_molt, ' +
        ':s7_molt, ' +
        ':s8_molt, ' +
        ':s9_molt, ' +
        ':r1_molt, ' +
        ':r2_molt, ' +
        ':r3_molt, ' +
        ':r4_molt, ' +
        ':r5_molt, ' +
        ':r6_molt, ' +
        ':pc1_molt, ' +
        ':pc2_molt, ' +
        ':pc3_molt, ' +
        ':pc4_molt, ' +
        ':pc5_molt, ' +
        ':pc6_molt, ' +
        ':pc7_molt, ' +
        ':pc8_molt, ' +
        ':pc9_molt, ' +
        ':cc_molt, ' +
        ':gc1_molt, ' +
        ':gc2_molt, ' +
        ':gc3_molt, ' +
        ':gc4_molt, ' +
        ':gc5_molt, ' +
        ':gc6_molt, ' +
        ':gc7_molt, ' +
        ':gc8_molt, ' +
        ':gc9_molt, ' +
        ':gc10_molt, ' +
        ':al1_molt, ' +
        ':al2_molt, ' +
        ':al3_molt, ' +
        ':lc_molt, ' +
        ':mc_molt, ' +
        ':growth_bar_size, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('full_name').AsString := FFullName;
      if (FSurveyId > 0) then
        ParamByName('survey_id').AsInteger := FSurveyId
      else
        ParamByName('survey_id').Clear;
      if (FTaxonId > 0) then
        ParamByName('taxon_id').AsInteger := FTaxonId
      else
        ParamByName('taxon_id').Clear;
      if (FIndividualId > 0) then
        ParamByName('individual_id').AsInteger := FIndividualId
      else
        ParamByName('individual_id').Clear;
      if (FCaptureId > 0) then
        ParamByName('capture_id').AsInteger := FCaptureId
      else
        ParamByName('capture_id').Clear;
      ParamByName('sample_date').AsString := DateToStr(FCaptureDate);
      ParamByName('sample_time').AsString := TimeToStr(FCaptureTime);
      if (FBanderId > 0) then
        ParamByName('bander_id').AsInteger := FBanderId
      else
        ParamByName('bander_id').Clear;
      if (FBandId > 0) then
        ParamByName('band_id').AsInteger := FBandId
      else
        ParamByName('band_id').Clear;
      ParamByName('p1_molt').AsFloat := FPrimary1;
      ParamByName('p2_molt').AsFloat := FPrimary2;
      ParamByName('p3_molt').AsFloat := FPrimary3;
      ParamByName('p4_molt').AsFloat := FPrimary4;
      ParamByName('p5_molt').AsFloat := FPrimary5;
      ParamByName('p6_molt').AsFloat := FPrimary6;
      ParamByName('p7_molt').AsFloat := FPrimary7;
      ParamByName('p8_molt').AsFloat := FPrimary8;
      ParamByName('p9_molt').AsFloat := FPrimary9;
      ParamByName('p10_molt').AsFloat := FPrimary10;
      ParamByName('s1_molt').AsFloat := FSecondary1;
      ParamByName('s2_molt').AsFloat := FSecondary2;
      ParamByName('s3_molt').AsFloat := FSecondary3;
      ParamByName('s4_molt').AsFloat := FSecondary4;
      ParamByName('s5_molt').AsFloat := FSecondary5;
      ParamByName('s6_molt').AsFloat := FSecondary6;
      ParamByName('s7_molt').AsFloat := FSecondary7;
      ParamByName('s8_molt').AsFloat := FSecondary8;
      ParamByName('s9_molt').AsFloat := FSecondary9;
      ParamByName('r1_molt').AsFloat := FRetrix1;
      ParamByName('r2_molt').AsFloat := FRetrix2;
      ParamByName('r3_molt').AsFloat := FRetrix3;
      ParamByName('r4_molt').AsFloat := FRetrix4;
      ParamByName('r5_molt').AsFloat := FRetrix5;
      ParamByName('r6_molt').AsFloat := FRetrix6;
      ParamByName('pc1_molt').AsFloat := FPrimaryCovert1;
      ParamByName('pc2_molt').AsFloat := FPrimaryCovert2;
      ParamByName('pc3_molt').AsFloat := FPrimaryCovert3;
      ParamByName('pc4_molt').AsFloat := FPrimaryCovert4;
      ParamByName('pc5_molt').AsFloat := FPrimaryCovert5;
      ParamByName('pc6_molt').AsFloat := FPrimaryCovert6;
      ParamByName('pc7_molt').AsFloat := FPrimaryCovert7;
      ParamByName('pc8_molt').AsFloat := FPrimaryCovert8;
      ParamByName('pc9_molt').AsFloat := FPrimaryCovert9;
      ParamByName('cc_molt').AsFloat := FCarpalCovert;
      ParamByName('gc1_molt').AsFloat := FGreatCovert1;
      ParamByName('gc2_molt').AsFloat := FGreatCovert2;
      ParamByName('gc3_molt').AsFloat := FGreatCovert3;
      ParamByName('gc4_molt').AsFloat := FGreatCovert4;
      ParamByName('gc5_molt').AsFloat := FGreatCovert5;
      ParamByName('gc6_molt').AsFloat := FGreatCovert6;
      ParamByName('gc7_molt').AsFloat := FGreatCovert7;
      ParamByName('gc8_molt').AsFloat := FGreatCovert8;
      ParamByName('gc9_molt').AsFloat := FGreatCovert9;
      ParamByName('gc10_molt').AsFloat := FGreatCovert10;
      ParamByName('al1_molt').AsFloat := FAlula1;
      ParamByName('al2_molt').AsFloat := FAlula2;
      ParamByName('al3_molt').AsFloat := FAlula3;
      ParamByName('lc_molt').AsFloat := FLeastCoverts;
      ParamByName('mc_molt').AsFloat := FMedianCoverts;
      if (FGrowthBarWidth > 0) then
        ParamByName('growth_bar_size').AsFloat := FGrowthBarWidth
      else
        ParamByName('growth_bar_size').Clear;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //// Get the taxon hierarchy
      //if (FTaxonId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
      //  Add('WHERE taxon_id = :ataxon');
      //  ParamByName('ataxon').AsInteger := FTaxonId;
      //  Open;
      //  FOrderId := FieldByName('order_id').AsInteger;
      //  FFamilyId := FieldByName('family_id').AsInteger;
      //  FGenusId := FieldByName('genus_id').AsInteger;
      //  FSpeciesId := FieldByName('species_id').AsInteger;
      //  Close;
      //end;
      //// Save the taxon hierarchy
      //Clear;
      //Add('UPDATE molts SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE molt_id = :aid');
      //ParamByName('order_id').AsInteger := FOrderId;
      //if (FFamilyId > 0) then
      //  ParamByName('family_id').AsInteger := FFamilyId
      //else
      //  ParamByName('family_id').Clear;
      //if (FGenusId > 0) then
      //  ParamByName('genus_id').AsInteger := FGenusId
      //else
      //  ParamByName('genus_id').Clear;
      //if (FSpeciesId > 0) then
      //  ParamByName('species_id').AsInteger := FSpeciesId
      //else
      //  ParamByName('species_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMolt.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TMolt.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Capture', FCaptureId);
    JSONObject.Add('Date', FCaptureDate);
    JSONObject.Add('Time', FCaptureTime);
    JSONObject.Add('Bander', FBanderId);
    JSONObject.Add('Band', FBandId);
    JSONObject.Add('Primary 1', FPrimary1);
    JSONObject.Add('Primary 2', FPrimary2);
    JSONObject.Add('Primary 3', FPrimary3);
    JSONObject.Add('Primary 4', FPrimary4);
    JSONObject.Add('Primary 5', FPrimary5);
    JSONObject.Add('Primary 6', FPrimary6);
    JSONObject.Add('Primary 7', FPrimary7);
    JSONObject.Add('Primary 8', FPrimary8);
    JSONObject.Add('Primary 9', FPrimary9);
    JSONObject.Add('Primary 10', FPrimary10);
    JSONObject.Add('Secondary 1', FSecondary1);
    JSONObject.Add('Secondary 2', FSecondary2);
    JSONObject.Add('Secondary 3', FSecondary3);
    JSONObject.Add('Secondary 4', FSecondary4);
    JSONObject.Add('Secondary 5', FSecondary5);
    JSONObject.Add('Secondary 6', FSecondary6);
    JSONObject.Add('Secondary 7', FSecondary7);
    JSONObject.Add('Secondary 8', FSecondary8);
    JSONObject.Add('Secondary 9', FSecondary9);
    JSONObject.Add('Retrix 1', FRetrix1);
    JSONObject.Add('Retrix 2', FRetrix2);
    JSONObject.Add('Retrix 3', FRetrix3);
    JSONObject.Add('Retrix 4', FRetrix4);
    JSONObject.Add('Retrix 5', FRetrix5);
    JSONObject.Add('Retrix 6', FRetrix6);
    JSONObject.Add('Primary covert 1', FPrimaryCovert1);
    JSONObject.Add('Primary covert 2', FPrimaryCovert2);
    JSONObject.Add('Primary covert 3', FPrimaryCovert3);
    JSONObject.Add('Primary covert 4', FPrimaryCovert4);
    JSONObject.Add('Primary covert 5', FPrimaryCovert5);
    JSONObject.Add('Primary covert 6', FPrimaryCovert6);
    JSONObject.Add('Primary covert 7', FPrimaryCovert7);
    JSONObject.Add('Primary covert 8', FPrimaryCovert8);
    JSONObject.Add('Primary covert 9', FPrimaryCovert9);
    JSONObject.Add('Carpal covert', FCarpalCovert);
    JSONObject.Add('Great covert 1', FGreatCovert1);
    JSONObject.Add('Great covert 2', FGreatCovert2);
    JSONObject.Add('Great covert 3', FGreatCovert3);
    JSONObject.Add('Great covert 4', FGreatCovert4);
    JSONObject.Add('Great covert 5', FGreatCovert5);
    JSONObject.Add('Great covert 6', FGreatCovert6);
    JSONObject.Add('Great covert 7', FGreatCovert7);
    JSONObject.Add('Great covert 8', FGreatCovert8);
    JSONObject.Add('Great covert 9', FGreatCovert9);
    JSONObject.Add('Great covert 10', FGreatCovert10);
    JSONObject.Add('Alula 1', FAlula1);
    JSONObject.Add('Alula 2', FAlula2);
    JSONObject.Add('Alula 3', FAlula3);
    JSONObject.Add('Least coverts', FLeastCoverts);
    JSONObject.Add('Median coverts', FMedianCoverts);
    JSONObject.Add('Growth bar width', FGrowthBarWidth);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TMolt.Update;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE molts SET ' +
        'survey_id = :survey_id, ' +
        'full_name = :full_name, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'capture_id = :capture_id, ' +
        'sample_date = date(:sample_date), ' +
        'sample_time = time(:sample_time), ' +
        'bander_id = :bander_id, ' +
        'band_id = :band_id, ' +
        'p1_molt = :p1_molt, ' +
        'p2_molt = :p2_molt, ' +
        'p3_molt = :p3_molt, ' +
        'p4_molt = :p4_molt, ' +
        'p5_molt = :p5_molt, ' +
        'p6_molt = :p6_molt, ' +
        'p7_molt = :p7_molt, ' +
        'p8_molt = :p8_molt, ' +
        'p9_molt = :p9_molt, ' +
        'p10_molt = :p10_molt, ' +
        's1_molt = :s1_molt, ' +
        's2_molt = :s2_molt, ' +
        's3_molt = :s3_molt, ' +
        's4_molt = :s4_molt, ' +
        's5_molt = :s5_molt, ' +
        's6_molt = :s6_molt, ' +
        's7_molt = :s7_molt, ' +
        's8_molt = :s8_molt, ' +
        's9_molt = :s9_molt, ' +
        'r1_molt = :r1_molt, ' +
        'r2_molt = :r2_molt, ' +
        'r3_molt = :r3_molt, ' +
        'r4_molt = :r4_molt, ' +
        'r5_molt = :r5_molt, ' +
        'r6_molt = :r6_molt, ' +
        'pc1_molt = :pc1_molt, ' +
        'pc2_molt = :pc2_molt, ' +
        'pc3_molt = :pc3_molt, ' +
        'pc4_molt = :pc4_molt, ' +
        'pc5_molt = :pc5_molt, ' +
        'pc6_molt = :pc6_molt, ' +
        'pc7_molt = :pc7_molt, ' +
        'pc8_molt = :pc8_molt, ' +
        'pc9_molt = :pc9_molt, ' +
        'cc_molt = :cc_molt, ' +
        'gc1_molt = :gc1_molt, ' +
        'gc2_molt = :gc2_molt, ' +
        'gc3_molt = :gc3_molt, ' +
        'gc4_molt = :gc4_molt, ' +
        'gc5_molt = :gc5_molt, ' +
        'gc6_molt = :gc6_molt, ' +
        'gc7_molt = :gc7_molt, ' +
        'gc8_molt = :gc8_molt, ' +
        'gc9_molt = :gc9_molt, ' +
        'gc10_molt = :gc10_molt, ' +
        'al1_molt = :al1_molt, ' +
        'al2_molt = :al2_molt, ' +
        'al3_molt = :al3_molt, ' +
        'lc_molt = :lc_molt, ' +
        'mc_molt = :mc_molt, ' +
        'growth_bar_size = :growth_bar_size, ' +
        'notes = :notes, ' +
        'exported_status = :exported_status, ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (molt_id = :molt_id)');

      ParamByName('full_name').AsString := FFullName;
      if (FSurveyId > 0) then
        ParamByName('survey_id').AsInteger := FSurveyId
      else
        ParamByName('survey_id').Clear;
      if (FTaxonId > 0) then
        ParamByName('taxon_id').AsInteger := FTaxonId
      else
        ParamByName('taxon_id').Clear;
      if (FIndividualId > 0) then
        ParamByName('individual_id').AsInteger := FIndividualId
      else
        ParamByName('individual_id').Clear;
      if (FCaptureId > 0) then
        ParamByName('capture_id').AsInteger := FCaptureId
      else
        ParamByName('capture_id').Clear;
      ParamByName('sample_date').AsString := DateToStr(FCaptureDate);
      ParamByName('sample_time').AsString := TimeToStr(FCaptureTime);
      if (FBanderId > 0) then
        ParamByName('bander_id').AsInteger := FBanderId
      else
        ParamByName('bander_id').Clear;
      if (FBandId > 0) then
        ParamByName('band_id').AsInteger := FBandId
      else
        ParamByName('band_id').Clear;
      ParamByName('p1_molt').AsFloat := FPrimary1;
      ParamByName('p2_molt').AsFloat := FPrimary2;
      ParamByName('p3_molt').AsFloat := FPrimary3;
      ParamByName('p4_molt').AsFloat := FPrimary4;
      ParamByName('p5_molt').AsFloat := FPrimary5;
      ParamByName('p6_molt').AsFloat := FPrimary6;
      ParamByName('p7_molt').AsFloat := FPrimary7;
      ParamByName('p8_molt').AsFloat := FPrimary8;
      ParamByName('p9_molt').AsFloat := FPrimary9;
      ParamByName('p10_molt').AsFloat := FPrimary10;
      ParamByName('s1_molt').AsFloat := FSecondary1;
      ParamByName('s2_molt').AsFloat := FSecondary2;
      ParamByName('s3_molt').AsFloat := FSecondary3;
      ParamByName('s4_molt').AsFloat := FSecondary4;
      ParamByName('s5_molt').AsFloat := FSecondary5;
      ParamByName('s6_molt').AsFloat := FSecondary6;
      ParamByName('s7_molt').AsFloat := FSecondary7;
      ParamByName('s8_molt').AsFloat := FSecondary8;
      ParamByName('s9_molt').AsFloat := FSecondary9;
      ParamByName('r1_molt').AsFloat := FRetrix1;
      ParamByName('r2_molt').AsFloat := FRetrix2;
      ParamByName('r3_molt').AsFloat := FRetrix3;
      ParamByName('r4_molt').AsFloat := FRetrix4;
      ParamByName('r5_molt').AsFloat := FRetrix5;
      ParamByName('r6_molt').AsFloat := FRetrix6;
      ParamByName('pc1_molt').AsFloat := FPrimaryCovert1;
      ParamByName('pc2_molt').AsFloat := FPrimaryCovert2;
      ParamByName('pc3_molt').AsFloat := FPrimaryCovert3;
      ParamByName('pc4_molt').AsFloat := FPrimaryCovert4;
      ParamByName('pc5_molt').AsFloat := FPrimaryCovert5;
      ParamByName('pc6_molt').AsFloat := FPrimaryCovert6;
      ParamByName('pc7_molt').AsFloat := FPrimaryCovert7;
      ParamByName('pc8_molt').AsFloat := FPrimaryCovert8;
      ParamByName('pc9_molt').AsFloat := FPrimaryCovert9;
      ParamByName('cc_molt').AsFloat := FCarpalCovert;
      ParamByName('gc1_molt').AsFloat := FGreatCovert1;
      ParamByName('gc2_molt').AsFloat := FGreatCovert2;
      ParamByName('gc3_molt').AsFloat := FGreatCovert3;
      ParamByName('gc4_molt').AsFloat := FGreatCovert4;
      ParamByName('gc5_molt').AsFloat := FGreatCovert5;
      ParamByName('gc6_molt').AsFloat := FGreatCovert6;
      ParamByName('gc7_molt').AsFloat := FGreatCovert7;
      ParamByName('gc8_molt').AsFloat := FGreatCovert8;
      ParamByName('gc9_molt').AsFloat := FGreatCovert9;
      ParamByName('gc10_molt').AsFloat := FGreatCovert10;
      ParamByName('al1_molt').AsFloat := FAlula1;
      ParamByName('al2_molt').AsFloat := FAlula2;
      ParamByName('al3_molt').AsFloat := FAlula3;
      ParamByName('lc_molt').AsFloat := FLeastCoverts;
      ParamByName('mc_molt').AsFloat := FMedianCoverts;
      if (FGrowthBarWidth > 0) then
        ParamByName('growth_bar_size').AsFloat := FGrowthBarWidth
      else
        ParamByName('growth_bar_size').Clear;
      ParamByName('notes').AsString := FNotes;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('exported_status').AsBoolean := FExported;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;
      ParamByName('molt_id').AsInteger := FId;

      ExecSQL;

      //// Get the taxon hierarchy
      //if (FTaxonId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
      //  Add('WHERE taxon_id = :ataxon');
      //  ParamByName('ataxon').AsInteger := FTaxonId;
      //  Open;
      //  FOrderId := FieldByName('order_id').AsInteger;
      //  FFamilyId := FieldByName('family_id').AsInteger;
      //  FGenusId := FieldByName('genus_id').AsInteger;
      //  FSpeciesId := FieldByName('species_id').AsInteger;
      //  Close;
      //end;
      //// Save the taxon hierarchy
      //Clear;
      //Add('UPDATE molts SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE molt_id = :aid');
      //ParamByName('order_id').AsInteger := FOrderId;
      //if (FFamilyId > 0) then
      //  ParamByName('family_id').AsInteger := FFamilyId
      //else
      //  ParamByName('family_id').Clear;
      //if (FGenusId > 0) then
      //  ParamByName('genus_id').AsInteger := FGenusId
      //else
      //  ParamByName('genus_id').Clear;
      //if (FSpeciesId > 0) then
      //  ParamByName('species_id').AsInteger := FSpeciesId
      //else
      //  ParamByName('species_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TMolt.Diff(aOld: TMolt; var aList: TStrings): Boolean;
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
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.CaptureDate, FCaptureDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.CaptureTime, FCaptureTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBanderID, aOld.BanderId, FBanderId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBandID, aOld.BandId, FBandId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP1, aOld.Primary1, FPrimary1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP2, aOld.Primary2, FPrimary2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP3, aOld.Primary3, FPrimary3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP4, aOld.Primary4, FPrimary4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP5, aOld.Primary5, FPrimary5, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP6, aOld.Primary6, FPrimary6, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP7, aOld.Primary7, FPrimary7, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP8, aOld.Primary8, FPrimary8, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP9, aOld.Primary9, FPrimary9, R) then
    aList.Add(R);
  if FieldValuesDiff(rscP10, aOld.Primary10, FPrimary10, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS1, aOld.Secondary1, FSecondary1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS2, aOld.Secondary2, FSecondary2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS3, aOld.Secondary3, FSecondary3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS4, aOld.Secondary4, FSecondary4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS5, aOld.Secondary5, FSecondary5, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS6, aOld.Secondary6, FSecondary6, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS7, aOld.Secondary7, FSecondary7, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS8, aOld.Secondary8, FSecondary8, R) then
    aList.Add(R);
  if FieldValuesDiff(rscS9, aOld.Secondary9, FSecondary9, R) then
    aList.Add(R);
  if FieldValuesDiff(rscR1, aOld.Retrix1, FRetrix1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscR2, aOld.Retrix2, FRetrix2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscR3, aOld.Retrix3, FRetrix3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscR4, aOld.Retrix4, FRetrix4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscR5, aOld.Retrix5, FRetrix5, R) then
    aList.Add(R);
  if FieldValuesDiff(rscR6, aOld.Retrix6, FRetrix6, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC1, aOld.PrimaryCovert1, FPrimaryCovert1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC2, aOld.PrimaryCovert2, FPrimaryCovert2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC3, aOld.PrimaryCovert3, FPrimaryCovert3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC4, aOld.PrimaryCovert4, FPrimaryCovert4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC5, aOld.PrimaryCovert5, FPrimaryCovert5, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC6, aOld.PrimaryCovert6, FPrimaryCovert6, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC7, aOld.PrimaryCovert7, FPrimaryCovert7, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC8, aOld.PrimaryCovert8, FPrimaryCovert8, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPC9, aOld.PrimaryCovert9, FPrimaryCovert9, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCC, aOld.CarpalCovert, FCarpalCovert, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC1, aOld.GreatCovert1, FGreatCovert1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC2, aOld.GreatCovert2, FGreatCovert2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC3, aOld.GreatCovert3, FGreatCovert3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC4, aOld.GreatCovert4, FGreatCovert4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC5, aOld.GreatCovert5, FGreatCovert5, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC6, aOld.GreatCovert6, FGreatCovert6, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC7, aOld.GreatCovert7, FGreatCovert7, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC8, aOld.GreatCovert8, FGreatCovert8, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC9, aOld.GreatCovert9, FGreatCovert9, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGC10, aOld.GreatCovert10, FGreatCovert10, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAl1, aOld.Alula1, FAlula1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAl2, aOld.Alula2, FAlula2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAl3, aOld.Alula3, FAlula3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLC, aOld.LeastCoverts, FLeastCoverts, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMC, aOld.MedianCoverts, FMedianCoverts, R) then
    aList.Add(R);
  if FieldValuesDiff(rscGrowthBarWidth, aOld.GrowthBarWidth, FGrowthBarWidth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TMolt.Find(const FieldName: String; const Value: Variant): Boolean;
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
      'molt_id, ' +
      'survey_id, ' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'bander_id, ' +
      'band_id, ' +
      'p1_molt, ' +
      'p2_molt, ' +
      'p3_molt, ' +
      'p4_molt, ' +
      'p5_molt, ' +
      'p6_molt, ' +
      'p7_molt, ' +
      'p8_molt, ' +
      'p9_molt, ' +
      'p10_molt, ' +
      's1_molt, ' +
      's2_molt, ' +
      's3_molt, ' +
      's4_molt, ' +
      's5_molt, ' +
      's6_molt, ' +
      's7_molt, ' +
      's8_molt, ' +
      's9_molt, ' +
      'r1_molt, ' +
      'r2_molt, ' +
      'r3_molt, ' +
      'r4_molt, ' +
      'r5_molt, ' +
      'r6_molt, ' +
      'pc1_molt, ' +
      'pc2_molt, ' +
      'pc3_molt, ' +
      'pc4_molt, ' +
      'pc5_molt, ' +
      'pc6_molt, ' +
      'pc7_molt, ' +
      'pc8_molt, ' +
      'pc9_molt, ' +
      'cc_molt, ' +
      'gc1_molt, ' +
      'gc2_molt, ' +
      'gc3_molt, ' +
      'gc4_molt, ' +
      'gc5_molt, ' +
      'gc6_molt, ' +
      'gc7_molt, ' +
      'gc8_molt, ' +
      'gc9_molt, ' +
      'gc10_molt, ' +
      'al1_molt, ' +
      'al2_molt, ' +
      'al3_molt, ' +
      'lc_molt, ' +
      'mc_molt, ' +
      'growth_bar_size, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM molts');
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM captures');
    Add('WHERE (capture_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
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
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
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
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
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

    ParamByName('survey_id').AsInteger := FSurveyId;
    ParamByName('full_name').AsString :=
      GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, Sexes[FSubjectSex], CaptureTypeStr[FCaptureType], FCycleCode, False);
    ParamByName('taxon_id').AsInteger := FTaxonId;
    ParamByName('individual_id').AsInteger := FIndividualId;
    ParamByName('capture_date').AsString := FormatDateTime('yyyy-mm-dd', FCaptureDate);
    ParamByName('capture_time').AsString := FormatDateTime('hh:nn', FCaptureTime);
    ParamByName('locality_id').AsInteger := FLocalityId;
    ParamByName('net_station_id').AsInteger := FNetStationId;
    if (FNetId = 0) then
      ParamByName('net_id').Clear
    else
      ParamByName('net_id').AsInteger := FNetId;
    if (FLatitude > 200.0) then
      ParamByName('latitude').Clear
    else
      ParamByName('latitude').AsFloat := FLatitude;
    if (FLongitude > 200.0) then
      ParamByName('longitude').Clear
    else
      ParamByName('longitude').AsFloat := FLongitude;
    ParamByName('bander_id').AsInteger := FBanderId;
    ParamByName('annotator_id').AsInteger := FAnnotatorId;
    ParamByName('subject_status').AsString := SubjectStatusStr[FSubjectStatus];
    ParamByName('capture_type').AsString := CaptureTypeStr[FCaptureType];
    ParamByName('subject_sex').AsString := Sexes[FSubjectSex];
    ParamByName('how_sexed').AsString := FHowSexed;
    if (FBandId = 0) then
      ParamByName('band_id').Clear
    else
      ParamByName('band_id').AsInteger := FBandId;
    if (FWeight = 0.0) then
      ParamByName('weight').Clear
    else
      ParamByName('weight').AsFloat := FWeight;
    if (FTarsusLength = 0.0) then
      ParamByName('tarsus_length').Clear
    else
      ParamByName('tarsus_length').AsFloat := FTarsusLength;
    if (FTarsusDiameter = 0.0) then
      ParamByName('tarsus_diameter').Clear
    else
      ParamByName('tarsus_diameter').AsFloat := FTarsusDiameter;
    if (FExposedCulmen = 0.0) then
      ParamByName('exposed_culmen').Clear
    else
      ParamByName('exposed_culmen').AsFloat := FExposedCulmen;
    if (FBillWidth = 0.0) then
      ParamByName('bill_width').Clear
    else
      ParamByName('bill_width').AsFloat := FBillWidth;
    if (FBillHeight = 0.0) then
      ParamByName('bill_height').Clear
    else
      ParamByName('bill_height').AsFloat := FBillHeight;
    if (FNostrilBillTip = 0.0) then
      ParamByName('nostril_bill_tip').Clear
    else
      ParamByName('nostril_bill_tip').AsFloat := FNostrilBillTip;
    if (FSkullLength = 0.0) then
      ParamByName('skull_length').Clear
    else
      ParamByName('skull_length').AsFloat := FSkullLength;
    if (FRightWingChord = 0.0) then
      ParamByName('right_wing_chord').Clear
    else
      ParamByName('right_wing_chord').AsFloat := FRightWingChord;
    if (FFirstSecondaryChord = 0.0) then
      ParamByName('first_secondary_chord').Clear
    else
      ParamByName('first_secondary_chord').AsFloat := FFirstSecondaryChord;
    if (FTailLength = 0.0) then
      ParamByName('tail_length').Clear
    else
      ParamByName('tail_length').AsFloat := FTailLength;
    ParamByName('fat').AsString := FFat;
    ParamByName('brood_patch').AsString := FBroodPatch;
    ParamByName('cloacal_protuberance').AsString := FCloacalProtuberance;
    ParamByName('body_molt').AsString := FBodyMolt;
    ParamByName('flight_feathers_molt').AsString := FFlightFeathersMolt;
    ParamByName('flight_feathers_wear').AsString := FFlightFeathersWear;
    ParamByName('molt_limits').AsString := FMoltLimits;
    ParamByName('cycle_code').AsString := FCycleCode;
    ParamByName('how_aged').AsString := FHowAged;
    ParamByName('skull_ossification').AsString := FSkullOssification;
    if (FKippsIndex = 0.0) then
      ParamByName('kipps_index').Clear
    else
      ParamByName('kipps_index').AsFloat := FKippsIndex;
    if (FGlucose = 0.0) then
      ParamByName('glucose').Clear
    else
      ParamByName('glucose').AsFloat := FGlucose;
    if (FHemoglobin = 0.0) then
      ParamByName('hemoglobin').Clear
    else
      ParamByName('hemoglobin').AsFloat := FHemoglobin;
    if (FHematocrit = 0.0) then
      ParamByName('hematocrit').Clear
    else
      ParamByName('hematocrit').AsFloat := FHematocrit;
    ParamByName('blood_sample').AsInteger := Integer(FBloodSample);
    ParamByName('feather_sample').AsInteger := Integer(FFeatherSample);
    if (FPhotographer1Id > 0) then
    begin
      ParamByName('subject_photographed').AsInteger := 1;
      ParamByName('photographer_1_id').AsInteger := FPhotographer1Id;
      if (FPhotographer2Id > 0) then
        ParamByName('photographer_2_id').AsInteger := FPhotographer2Id;
    end else
    begin
      ParamByName('subject_photographed').AsInteger := 0;
      ParamByName('photographer_1_id').Clear;
      ParamByName('photographer_2_id').Clear;
    end;
    if (FStartPhotoNumber <> EmptyStr) then
      ParamByName('start_photo_number').AsInteger := StrToInt(FStartPhotoNumber);
    if (FEndPhotoNumber <> EmptyStr) then
      ParamByName('end_photo_number').AsInteger := StrToInt(FEndPhotoNumber);
    ParamByName('camera_name').AsString := FCameraName;
    if (FRemovedBandId = 0) then
      ParamByName('removed_band_id').Clear
    else
      ParamByName('removed_band_id').AsInteger := FRemovedBandId;
    ParamByName('right_leg_below').AsString := FRightLegBelow;
    ParamByName('left_leg_below').AsString := FLeftLegBelow;
    ParamByName('escaped').AsInteger := Integer(FEscaped);
    ParamByName('notes').AsString := FNotes;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;

    //// Get the taxon hierarchy
    //if (FTaxonId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
    //  Add('WHERE taxon_id = :ataxon');
    //  ParamByName('ataxon').AsInteger := FTaxonId;
    //  Open;
    //  FOrderId := FieldByName('order_id').AsInteger;
    //  FFamilyId := FieldByName('family_id').AsInteger;
    //  FGenusId := FieldByName('genus_id').AsInteger;
    //  FSpeciesId := FieldByName('species_id').AsInteger;
    //  Close;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add('UPDATE captures SET');
    //Add('  order_id = :order_id,');
    //Add('  family_id = :family_id,');
    //Add('  genus_id = :genus_id,');
    //Add('  species_id = :species_id');
    //Add('WHERE capture_id = :aid');
    //ParamByName('order_id').AsInteger := FOrderId;
    //if (FFamilyId > 0) then
    //  ParamByName('family_id').AsInteger := FFamilyId
    //else
    //  ParamByName('family_id').Clear;
    //if (FGenusId > 0) then
    //  ParamByName('genus_id').AsInteger := FGenusId
    //else
    //  ParamByName('genus_id').Clear;
    //if (FSpeciesId > 0) then
    //  ParamByName('species_id').AsInteger := FSpeciesId
    //else
    //  ParamByName('species_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
    //
    //// Get the site hierarchy
    //if (FLocalityId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
    //  Add('WHERE site_id = :asite');
    //  ParamByName('ASITE').AsInteger := FLocalityId;
    //  Open;
    //  FCountryId := FieldByName('country_id').AsInteger;
    //  FStateId := FieldByName('state_id').AsInteger;
    //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
    //  Close;
    //end;
    //// Save the site hierarchy
    //Clear;
    //Add('UPDATE captures SET');
    //Add('  country_id = :country_id,');
    //Add('  state_id = :state_id,');
    //Add('  municipality_id = :municipality_id');
    //Add('WHERE capture_id = :aid');
    //ParamByName('country_id').AsInteger := FCountryId;
    //if (FStateId > 0) then
    //  ParamByName('state_id').AsInteger := FStateId
    //else
    //  ParamByName('state_id').Clear;
    //if (FMunicipalityId > 0) then
    //  ParamByName('municipality_id').AsInteger := FMunicipalityId
    //else
    //  ParamByName('municipality_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
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
      'flight_feathers_wear = :fligth_feathers_wear, ' +
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
    ParamByName('survey_id').AsInteger := FSurveyId;
    ParamByName('full_name').AsString :=
      GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, Sexes[FSubjectSex], CaptureTypeStr[FCaptureType], FCycleCode, False);
    ParamByName('taxon_id').AsInteger := FTaxonId;
    ParamByName('individual_id').AsInteger := FIndividualId;
    if (FProjectId = 0) then
      ParamByName('project_id').Clear
    else
      ParamByName('project_id').AsInteger := FProjectId;
    ParamByName('capture_date').AsString := FormatDateTime('yyyy-mm-dd', FCaptureDate);
    ParamByName('capture_time').AsString := FormatDateTime('hh:nn', FCaptureTime);
    ParamByName('locality_id').AsInteger := FLocalityId;
    ParamByName('net_station_id').AsInteger := FNetStationId;
    if (FNetId = 0) then
      ParamByName('net_id').Clear
    else
      ParamByName('net_id').AsInteger := FNetId;
    if (FLatitude > 200.0) then
      ParamByName('latitude').Clear
    else
      ParamByName('latitude').AsFloat := FLatitude;
    if (FLongitude > 200.0) then
      ParamByName('longitude').Clear
    else
      ParamByName('longitude').AsFloat := FLongitude;
    ParamByName('bander_id').AsInteger := FBanderId;
    ParamByName('annotator_id').AsInteger := FAnnotatorId;
    ParamByName('subject_status').AsString := SubjectStatusStr[FSubjectStatus];
    ParamByName('capture_type').AsString := CaptureTypeStr[FCaptureType];
    ParamByName('subject_sex').AsString := Sexes[FSubjectSex];
    ParamByName('how_sexed').AsString := FHowSexed;
    if (FBandId = 0) then
      ParamByName('band_id').Clear
    else
      ParamByName('band_id').AsInteger := FBandId;
    if (FWeight = 0.0) then
      ParamByName('weight').Clear
    else
      ParamByName('weight').AsFloat := FWeight;
    if (FTarsusLength = 0.0) then
      ParamByName('tarsus_length').Clear
    else
      ParamByName('tarsus_length').AsFloat := FTarsusLength;
    if (FTarsusDiameter = 0.0) then
      ParamByName('tarsus_diameter').Clear
    else
      ParamByName('tarsus_diameter').AsFloat := FTarsusDiameter;
    if (FCulmenLength = 0.0) then
      ParamByName('culmen_length').Clear
    else
      ParamByName('culmen_length').AsFloat := FCulmenLength;
    if (FExposedCulmen = 0.0) then
      ParamByName('exposed_culmen').Clear
    else
      ParamByName('exposed_culmen').AsFloat := FExposedCulmen;
    if (FBillWidth = 0.0) then
      ParamByName('bill_width').Clear
    else
      ParamByName('bill_width').AsFloat := FBillWidth;
    if (FBillHeight = 0.0) then
      ParamByName('bill_height').Clear
    else
      ParamByName('bill_height').AsFloat := FBillHeight;
    if (FNostrilBillTip = 0.0) then
      ParamByName('nostril_bill_tip').Clear
    else
      ParamByName('nostril_bill_tip').AsFloat := FNostrilBillTip;
    if (FSkullLength = 0.0) then
      ParamByName('skull_length').Clear
    else
      ParamByName('skull_length').AsFloat := FSkullLength;
    if (FRightWingChord = 0.0) then
      ParamByName('right_wing_chord').Clear
    else
      ParamByName('right_wing_chord').AsFloat := FRightWingChord;
    if (FFirstSecondaryChord = 0.0) then
      ParamByName('first_secondary_chord').Clear
    else
      ParamByName('first_secondary_chord').AsFloat := FFirstSecondaryChord;
    if (FTailLength = 0.0) then
      ParamByName('tail_length').Clear
    else
      ParamByName('tail_length').AsFloat := FTailLength;
    ParamByName('fat').AsString := FFat;
    ParamByName('brood_patch').AsString := FBroodPatch;
    ParamByName('cloacal_protuberance').AsString := FCloacalProtuberance;
    ParamByName('body_molt').AsString := FBodyMolt;
    ParamByName('flight_feathers_molt').AsString := FFlightFeathersMolt;
    ParamByName('flight_feathers_wear').AsString := FFlightFeathersWear;
    ParamByName('molt_limits').AsString := FMoltLimits;
    ParamByName('cycle_code').AsString := FCycleCode;
    ParamByName('subject_age').AsString := Ages[FSubjectAge];
    ParamByName('how_aged').AsString := FHowAged;
    ParamByName('skull_ossification').AsString := FSkullOssification;
    if (FHaluxLengthTotal = 0.0) then
      ParamByName('halux_length_total').Clear
    else
      ParamByName('halux_length_total').AsFloat := FHaluxLengthTotal;
    if (FHaluxLengthFinger = 0.0) then
      ParamByName('halux_length_finger').Clear
    else
      ParamByName('halux_length_finger').AsFloat := FHaluxLengthFinger;
    if (FHaluxLengthClaw = 0.0) then
      ParamByName('halux_length_claw').Clear
    else
      ParamByName('halux_length_claw').AsFloat := FHaluxLengthClaw;
    if (FCentralRetrixLength = 0.0) then
      ParamByName('central_retrix_length').Clear
    else
      ParamByName('central_retrix_length').AsFloat := FCentralRetrixLength;
    if (FExternalRetrixLength = 0.0) then
      ParamByName('external_retrix_length').Clear
    else
      ParamByName('external_retrix_length').AsFloat := FExternalRetrixLength;
    if (FTotalLength = 0.0) then
      ParamByName('total_length').Clear
    else
      ParamByName('total_length').AsFloat := FTotalLength;
    ParamByName('feather_mites').AsString := FFeatherMites;
    if (FPhilornisLarvaeTally = 0.0) then
      ParamByName('philornis_larvae_tally').Clear
    else
      ParamByName('philornis_larvae_tally').AsInteger := FPhilornisLarvaeTally;
    if (FKippsIndex = 0.0) then
      ParamByName('kipps_index').Clear
    else
      ParamByName('kipps_index').AsFloat := FKippsIndex;
    if (FGlucose = 0.0) then
      ParamByName('glucose').Clear
    else
      ParamByName('glucose').AsFloat := FGlucose;
    if (FHemoglobin = 0.0) then
      ParamByName('hemoglobin').Clear
    else
      ParamByName('hemoglobin').AsFloat := FHemoglobin;
    if (FHematocrit = 0.0) then
      ParamByName('hematocrit').Clear
    else
      ParamByName('hematocrit').AsFloat := FHematocrit;
    ParamByName('field_number').AsString := FFieldNumber;
    ParamByName('blood_sample').AsInteger := Integer(FBloodSample);
    ParamByName('feather_sample').AsInteger := Integer(FFeatherSample);
    ParamByName('claw_sample').AsInteger := Integer(FClawSample);
    ParamByName('feces_sample').AsInteger := Integer(FFecesSample);
    ParamByName('parasite_sample').AsInteger := Integer(FParasiteSample);
    ParamByName('subject_collected').AsInteger := Integer(FSubjectCollected);
    ParamByName('subject_recorded').AsInteger := Integer(FSubjectRecorded);
    if (FPhotographer1Id > 0) then
    begin
      ParamByName('subject_photographed').AsInteger := 1;
      ParamByName('photographer_1_id').AsInteger := FPhotographer1Id;
      if (FPhotographer2Id > 0) then
        ParamByName('photographer_2_id').AsInteger := FPhotographer2Id;
    end else
    begin
      ParamByName('subject_photographed').AsInteger := 0;
      ParamByName('photographer_1_id').Clear;
      ParamByName('photographer_2_id').Clear;
    end;
    if (FStartPhotoNumber <> EmptyStr) then
      ParamByName('start_photo_number').AsInteger := StrToInt(FStartPhotoNumber);
    if (FEndPhotoNumber <> EmptyStr) then
      ParamByName('end_photo_number').AsInteger := StrToInt(FEndPhotoNumber);
    ParamByName('camera_name').AsString := FCameraName;
    if (FRemovedBandId = 0) then
      ParamByName('removed_band_id').Clear
    else
      ParamByName('removed_band_id').AsInteger := FRemovedBandId;
    ParamByName('right_leg_below').AsString := FRightLegBelow;
    ParamByName('left_leg_below').AsString := FLeftLegBelow;
    ParamByName('right_leg_above').AsString := FRightLegAbove;
    ParamByName('left_leg_above').AsString := FLeftLegAbove;
    ParamByName('escaped').AsBoolean := FEscaped;
    ParamByName('needs_review').AsBoolean := FNeedsReview;
    ParamByName('notes').AsString := FNotes;
    ParamByName('exported_status').AsBoolean := FExported;
    ParamByName('marked_status').AsBoolean := FMarked;
    ParamByName('active_status').AsBoolean := FActive;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('capture_id').AsInteger := FId;

    ExecSQL;

    //// Get the taxon hierarchy
    //if (FTaxonId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
    //  Add('WHERE taxon_id = :ataxon');
    //  ParamByName('ataxon').AsInteger := FTaxonId;
    //  Open;
    //  FOrderId := FieldByName('order_id').AsInteger;
    //  FFamilyId := FieldByName('family_id').AsInteger;
    //  FGenusId := FieldByName('genus_id').AsInteger;
    //  FSpeciesId := FieldByName('species_id').AsInteger;
    //  Close;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add('UPDATE captures SET');
    //Add('  order_id = :order_id,');
    //Add('  family_id = :family_id,');
    //Add('  genus_id = :genus_id,');
    //Add('  species_id = :species_id');
    //Add('WHERE capture_id = :aid');
    //ParamByName('order_id').AsInteger := FOrderId;
    //if (FFamilyId > 0) then
    //  ParamByName('family_id').AsInteger := FFamilyId
    //else
    //  ParamByName('family_id').Clear;
    //if (FGenusId > 0) then
    //  ParamByName('genus_id').AsInteger := FGenusId
    //else
    //  ParamByName('genus_id').Clear;
    //if (FSpeciesId > 0) then
    //  ParamByName('species_id').AsInteger := FSpeciesId
    //else
    //  ParamByName('species_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
    //
    //// Get the site hierarchy
    //if (FLocalityId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
    //  Add('WHERE site_id = :asite');
    //  ParamByName('ASITE').AsInteger := FLocalityId;
    //  Open;
    //  FCountryId := FieldByName('country_id').AsInteger;
    //  FStateId := FieldByName('state_id').AsInteger;
    //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
    //  Close;
    //end;
    //// Save the site hierarchy
    //Clear;
    //Add('UPDATE captures SET');
    //Add('  country_id = :country_id,');
    //Add('  state_id = :state_id,');
    //Add('  municipality_id = :municipality_id');
    //Add('WHERE capture_id = :aid');
    //ParamByName('country_id').AsInteger := FCountryId;
    //if (FStateId > 0) then
    //  ParamByName('state_id').AsInteger := FStateId
    //else
    //  ParamByName('state_id').Clear;
    //if (FMunicipalityId > 0) then
    //  ParamByName('municipality_id').AsInteger := FMunicipalityId
    //else
    //  ParamByName('municipality_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM individuals');
    Add('WHERE (individual_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
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
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
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
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
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
    ParamByName('taxon_id').AsInteger := FTaxonId;
    ParamByName('individual_sex').AsString := Sexes[FSex];
    ParamByName('individual_age').AsString := Ages[FAge];
    if (FNestId > 0) then
      ParamByName('nest_id').AsInteger := FNestId
    else
      ParamByName('nest_id').Clear;
    ParamByName('birth_year').AsInteger := FBirthYear;
    ParamByName('birth_month').AsInteger := FBirthMonth;
    ParamByName('birth_day').AsInteger := FBirthDay;
    Birth.Encode(FBirthYear, FBirthMonth, FBirthDay);
    ParamByName('birth_date').AsString := Birth.ToString;
    if (FBandId > 0) then
      ParamByName('band_id').AsInteger := FBandId
    else
      ParamByName('band_id').Clear;
    if (FDoubleBandId > 0) then
      ParamByName('double_band_id').AsInteger := FDoubleBandId
    else
      ParamByName('double_band_id').Clear;
    if (FRemovedBandId > 0) then
      ParamByName('removed_band_id').AsInteger := FRemovedBandId
    else
      ParamByName('removed_band_id').Clear;
    if not DateIsNull(FBandingDate) then
      ParamByName('banding_date').AsString := DateToStr(FBandingDate)
    else
      ParamByName('banding_date').Clear;
    if not DateIsNull(FBandChangeDate) then
      ParamByName('band_change_date').AsString := DateToStr(FBandChangeDate)
    else
      ParamByName('band_change_date').Clear;
    ParamByName('recognizable_markings').AsString := FRecognizableMarkings;
    ParamByName('notes').AsString := FNotes;
    if (FFatherId > 0) then
      ParamByName('father_id').AsInteger := FFatherId
    else
      ParamByName('father_id').Clear;
    if (FMotherId > 0) then
      ParamByName('mother_id').AsInteger := FMotherId
    else
      ParamByName('mother_id').Clear;
    ParamByName('death_year').AsInteger := FDeathYear;
    ParamByName('death_month').AsInteger := FDeathMonth;
    ParamByName('death_day').AsInteger := FDeathDay;
    Death.Encode(FDeathYear, FDeathMonth, FDeathDay);
    ParamByName('death_date').AsString := Death.ToString;
    ParamByName('formatted_name').AsString :=
      GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], True);
    ParamByName('full_name').AsString :=
      GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], False);
    ParamByName('right_leg_below').AsString := FRightLegBelow;
    ParamByName('left_leg_below').AsString := FLeftLegBelow;
    ParamByName('right_leg_above').AsString := FRightLegAbove;
    ParamByName('left_leg_above').AsString := FLeftLegAbove;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;

    //// Get the taxon hierarchy
    //if (FTaxonId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
    //  Add('WHERE taxon_id = :ataxon');
    //  ParamByName('ataxon').AsInteger := FTaxonId;
    //  Open;
    //  FOrderId := FieldByName('order_id').AsInteger;
    //  FFamilyId := FieldByName('family_id').AsInteger;
    //  FGenusId := FieldByName('genus_id').AsInteger;
    //  FSpeciesId := FieldByName('species_id').AsInteger;
    //  Close;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add('UPDATE individuals SET');
    //Add('  order_id = :order_id,');
    //Add('  family_id = :family_id,');
    //Add('  genus_id = :genus_id,');
    //Add('  species_id = :species_id');
    //Add('WHERE individual_id = :aid');
    //ParamByName('order_id').AsInteger := FOrderId;
    //if (FFamilyId > 0) then
    //  ParamByName('family_id').AsInteger := FFamilyId
    //else
    //  ParamByName('family_id').Clear;
    //if (FGenusId > 0) then
    //  ParamByName('genus_id').AsInteger := FGenusId
    //else
    //  ParamByName('genus_id').Clear;
    //if (FSpeciesId > 0) then
    //  ParamByName('species_id').AsInteger := FSpeciesId
    //else
    //  ParamByName('species_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
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
      'banding_date = :banding_date, ' +
      'band_change_date = :band_change_date, ' +
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
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'')');
    Add('WHERE (individual_id = :individual_id)');
    ParamByName('taxon_id').AsInteger := FTaxonId;
    ParamByName('individual_sex').AsString := Sexes[FSex];
    ParamByName('individual_age').AsString := Ages[FAge];
    if (FNestId > 0) then
      ParamByName('nest_id').AsInteger := FNestId
    else
      ParamByName('nest_id').Clear;
    ParamByName('birth_year').AsInteger := FBirthYear;
    ParamByName('birth_month').AsInteger := FBirthMonth;
    ParamByName('birth_day').AsInteger := FBirthDay;
    Birth.Encode(FBirthYear, FBirthMonth, FBirthDay);
    ParamByName('birth_date').AsString := Birth.ToString;
    if (FBandId > 0) then
      ParamByName('band_id').AsInteger := FBandId
    else
      ParamByName('band_id').Clear;
    if (FDoubleBandId > 0) then
      ParamByName('double_band_id').AsInteger := FDoubleBandId
    else
      ParamByName('double_band_id').Clear;
    if (FRemovedBandId > 0) then
      ParamByName('removed_band_id').AsInteger := FRemovedBandId
    else
      ParamByName('removed_band_id').Clear;
    if not DateIsNull(FBandingDate) then
      ParamByName('banding_date').AsString := DateToStr(FBandingDate)
    else
      ParamByName('banding_date').Clear;
    if not DateIsNull(FBandChangeDate) then
      ParamByName('band_change_date').AsString := DateToStr(FBandChangeDate)
    else
      ParamByName('band_change_date').Clear;
    ParamByName('recognizable_markings').AsString := FRecognizableMarkings;
    ParamByName('notes').AsString := FNotes;
    if (FFatherId > 0) then
      ParamByName('father_id').AsInteger := FFatherId
    else
      ParamByName('father_id').Clear;
    if (FMotherId > 0) then
      ParamByName('mother_id').AsInteger := FMotherId
    else
      ParamByName('mother_id').Clear;
    ParamByName('death_year').AsInteger := FDeathYear;
    ParamByName('death_month').AsInteger := FDeathMonth;
    ParamByName('death_day').AsInteger := FDeathDay;
    Death.Encode(FDeathYear, FDeathMonth, FDeathDay);
    ParamByName('death_date').AsString := Death.ToString;
    ParamByName('formatted_name').AsString :=
      GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], True);
    ParamByName('full_name').AsString :=
      GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, Sexes[FSex], False);
    ParamByName('right_leg_below').AsString := FRightLegBelow;
    ParamByName('left_leg_below').AsString := FLeftLegBelow;
    ParamByName('right_leg_above').AsString := FRightLegAbove;
    ParamByName('left_leg_above').AsString := FLeftLegAbove;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ParamByName('individual_id').AsInteger := FId;
    ExecSQL;

    //// Get the taxon hierarchy
    //if (FTaxonId > 0) then
    //begin
    //  Clear;
    //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
    //  Add('WHERE taxon_id = :ataxon');
    //  ParamByName('ataxon').AsInteger := FTaxonId;
    //  Open;
    //  FOrderId := FieldByName('order_id').AsInteger;
    //  FFamilyId := FieldByName('family_id').AsInteger;
    //  FGenusId := FieldByName('genus_id').AsInteger;
    //  FSpeciesId := FieldByName('species_id').AsInteger;
    //  Close;
    //end;
    //// Save the taxon hierarchy
    //Clear;
    //Add('UPDATE individuals SET');
    //Add('  order_id = :order_id,');
    //Add('  family_id = :family_id,');
    //Add('  genus_id = :genus_id,');
    //Add('  species_id = :species_id');
    //Add('WHERE individual_id = :aid');
    //ParamByName('order_id').AsInteger := FOrderId;
    //if (FFamilyId > 0) then
    //  ParamByName('family_id').AsInteger := FFamilyId
    //else
    //  ParamByName('family_id').Clear;
    //if (FGenusId > 0) then
    //  ParamByName('genus_id').AsInteger := FGenusId
    //else
    //  ParamByName('genus_id').Clear;
    //if (FSpeciesId > 0) then
    //  ParamByName('species_id').AsInteger := FSpeciesId
    //else
    //  ParamByName('species_id').Clear;
    //ParamByName('aid').AsInteger := FId;
    //ExecSQL;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('DELETE FROM bands');
    Add('WHERE (band_id = :aid)');

    ParamByName('aid').AsInteger := FId;

    ExecSQL;
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
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
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
    FCarrierId := FieldByName('carrier_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FReported := FieldByName('band_reported').AsBoolean;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
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
      ':carrier_id, ' +
      ':project_id, ' +
      ':notes, ' +
      ':full_name, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));');
    ParamByName('band_size').AsString := FSize;
    ParamByName('band_number').AsInteger := FNumber;
    ParamByName('band_status').AsString := BandStatusStr[FStatus];
    ParamByName('band_type').AsString := MarkTypesStr[FBandType];
    if (FPrefix <> EmptyStr) then
      ParamByName('band_prefix').AsString := FPrefix
    else
      ParamByName('band_prefix').Clear;
    if (FSuffix <> EmptyStr) then
      ParamByName('band_suffix').AsString := FSuffix
    else
      ParamByName('band_suffix').Clear;
    if (FBandColor <> EmptyStr) then
      ParamByName('band_color').AsString := FBandColor
    else
      ParamByName('band_color').Clear;
    ParamByName('band_source').AsString := BandSourceStr[FSource];
    ParamByName('supplier_id').AsInteger := FSupplierId;
    if (FCarrierId > 0) then
      ParamByName('carrier_id').AsInteger := FCarrierId
    else
      ParamByName('carrier_id').Clear;
    if (FProjectId > 0) then
      ParamByName('project_id').AsInteger := FProjectId
    else
      ParamByName('project_id').Clear;
    ParamByName('full_name').AsString := GetBandFullname(FSize, FNumber, FSupplierId);
    ParamByName('notes').AsString := FNotes;
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    FId := Fields[0].AsInteger;
    Close;
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
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
      'carrier_id = :carrier_id, ' +
      'project_id = :project_id, ' +
      'notes = :notes, ' +
      'full_name = :full_name, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (band_id = :band_id)');
    ParamByName('band_size').AsString := FSize;
    ParamByName('band_number').AsInteger := FNumber;
    ParamByName('band_status').AsString := BandStatusStr[FStatus];
    ParamByName('band_type').AsString := MarkTypesStr[FBandType];
    if (FPrefix <> EmptyStr) then
      ParamByName('band_prefix').AsString := FPrefix
    else
      ParamByName('band_prefix').Clear;
    if (FSuffix <> EmptyStr) then
      ParamByName('band_suffix').AsString := FSuffix
    else
      ParamByName('band_suffix').Clear;
    if (FBandColor <> EmptyStr) then
      ParamByName('band_color').AsString := FBandColor
    else
      ParamByName('band_color').Clear;
    ParamByName('band_source').AsString := BandSourceStr[FSource];
    ParamByName('supplier_id').AsInteger := FSupplierId;
    if (FCarrierId > 0) then
      ParamByName('carrier_id').AsInteger := FCarrierId
    else
      ParamByName('carrier_id').Clear;
    if (FProjectId > 0) then
      ParamByName('project_id').AsInteger := FProjectId
    else
      ParamByName('project_id').Clear;
    ParamByName('marked_status').AsBoolean := FMarked;
    ParamByName('active_status').AsBoolean := FActive;
    ParamByName('full_name').AsString := GetBandFullname(FSize, FNumber, FSupplierId);
    ParamByName('notes').AsString := FNotes;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('band_id').AsInteger := FId;
    ExecSQL;
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

