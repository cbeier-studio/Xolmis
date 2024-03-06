unit cbs_birds;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Generics.Collections, DB, SQLDB, cbs_record_types;

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

  TBirdMark = class
    BodyPart: TBodyPart;
    Index: Integer;
    Color: TBandColorCode;
    MarkType: TMarkType;
    Inscription: String;
    BandId: Integer;
    //IndividualId: Integer;
  end;

  TBirdMarks = specialize TObjectList<TBirdMark>;

  //{ TBands }
  //
  //TBands = record
  //  Band1: TBandColorCode;
  //  Band2: TBandColorCode;
  //  Band3: TBandColorCode;
  //  Band4: TBandColorCode;
  //  function Count: Integer;
  //  function ToString: String;
  //  procedure Clear;
  //end;
  //
  //{ TColorBands }
  //
  //TColorBands = record
  //  RightLeg: TBands;
  //  LeftLeg: TBands;
  //  procedure Clear;
  //  function ToString: String;
  //  function Count: Integer;
  //end;

type

  { TBand }

  TBand = class(TXolmisRecord)
  protected
    FFullName: String;
    FSize: String;
    FNumber: Integer;
    FStatus: String;
    FSource: String;
    FPrefix: String;
    FSuffix: String;
    FBandColor: String;
    FBandType: String;
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
    procedure Insert;
    function Find(aSize: String; aNumber: Integer): Boolean;
    function Diff(aOld: TBand; var aList: TStrings): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property Size: String read FSize write FSize;
    property Number: Integer read FNumber write FNumber;
    property Status: String read FStatus write FStatus;
    property Source: String read FSource write FSource;
    property Prefix: String read FPrefix write FPrefix;
    property Suffix: String read FSuffix write FSuffix;
    property BandColor: String read FBandColor write FBandColor;
    property BandType: String read FBandType write FBandType;
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
    FEventType: String;
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
    function Diff(aOld: TBandHistory; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
  published
    property BandId: Integer read FBandId write FBandId;
    property EventType: String read FEventType write FEventType;
    property EventDate: TDate read FEventDate write FEventDate;
    property OrderNumber: Integer read FOrderNumber write FOrderNumber;
    property SupplierId: Integer read FSupplierId write FSupplierId;
    property SenderId: Integer read FSenderId write FSenderId;
    property RequesterId: Integer read FRequesterId write FRequesterId;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TIndividual }

  TIndividual = class(TXolmisRecord)
  protected
    FFullName: String;
    FTaxonId: Integer;
    FFamilyId: Integer;
    FGenusId: Integer;
    FOrderId: Integer;
    FSpeciesId: Integer;
    FSubfamilyId: Integer;
    FSex: String;
    FAge: String;
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
    procedure Insert;
    function Find(aTaxon, aBand: Integer; aRightLeg: String = ''; aLeftLeg: String = ''): Boolean;
    function Diff(aOld: TIndividual; var aList: TStrings): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property OrderId: Integer read FOrderId write FOrderId;
    property FamilyId: Integer read FFamilyId write FFamilyId;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property GenusId: Integer read FGenusId write FGenusId;
    property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    property Sex: String read FSex write FSex;
    property Age: String read FAge write FAge;
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

const
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
    FOrderId: Integer;
    FFamilyId: Integer;
    FGenusId: Integer;
    FSpeciesId: Integer;
    FIndividualId: Integer;
    FProjectId: Integer;
    FCaptureDate: TDate;
    FCaptureTime: TTime;
    FCountryId: Integer;
    FStateId: Integer;
    FMunicipalityId: Integer;
    FLocalityId: Integer;
    FNetStationId: Integer;
    FNetId: Integer;
    FLongitude: Extended;
    FLatitude: Extended;
    FBanderId: Integer;
    FAnnotatorId: Integer;
    FSubjectStatus: String;
    FCaptureType: String;
    FSubjectSex: String;
    FHowSexed: String;
    FBandId: Integer;
    FRemovedBandId: Integer;
    FRightLegBelow: String;
    FLeftLegBelow: String;
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
    FOldMolt: String;
    FOldPrimariesMolt: String;
    FOldSecondariesMolt: String;
    FOldRetricesMolt: String;
    FOldBodyMolt: String;
    FBodyMolt: String;
    FFlightFeathersMolt: String;
    FFlightFeathersWear: String;
    FMoltLimits: String;
    FCycleCode: String;
    FSubjectAge: String;
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
    procedure Insert;
    procedure Update;
    function Find(aTaxon, aBand: Integer; aCaptureType, aDate, aTime: String): Boolean;
    function Diff(aOld: TCapture; var aList: TStrings): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property OrderId: Integer read FOrderId write FOrderId;
    property FamilyId: Integer read FFamilyId write FFamilyId;
    property GenusId: Integer read FGenusId write FGenusId;
    property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property CaptureDate: TDate read FCaptureDate write FCaptureDate;
    property CaptureTime: TTime read FCaptureTime write FCaptureTime;
    property CountryId: Integer read FCountryId write FCountryId;
    property StateId: Integer read FStateId write FStateId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property NetStationId: Integer read FNetStationId write FNetStationId;
    property NetId: Integer read FNetId write FNetId;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property BanderId: Integer read FBanderId write FBanderId;
    property AnnotatorId: Integer read FAnnotatorId write FAnnotatorId;
    property SubjectStatus: String read FSubjectStatus write FSubjectStatus;
    property CaptureType: String read FCaptureType write FCaptureType;
    property SubjectSex: String read FSubjectSex write FSubjectSex;
    property HowSexed: String read FHowSexed write FHowSexed;
    property BandId: Integer read FBandId write FBandId;
    property RemovedBandId: Integer read FRemovedBandId write FRemovedBandId;
    property RightLegBelow: String read FRightLegBelow write FRightLegBelow;
    property LeftLegBelow: String read FLeftLegBelow write FLeftLegBelow;
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
    property OldMolt: String read FOldMolt write FoldMolt;
    property OldPrimariesMolt: String read FOldPrimariesMolt write FOldPrimariesMolt;
    property OldSecondariesMolt: String read FOldSecondariesMolt write FOldSecondariesMolt;
    property OldRetricesMolt: String read FOldRetricesMolt write FOldRetricesMolt;
    property OldBodyMolt: String read FOldBodyMolt write FOldBodyMolt;
    property BodyMolt: String read FBodyMolt write FBodyMolt;
    property FlightFeathersMolt: String read FFlightFeathersMolt write FFlightFeathersMolt;
    property FlightFeathersWear: String read FFlightFeathersWear write FFlightFeathersWear;
    property MoltLimits: String read FMoltLimits write FMoltLimits;
    property CycleCode: String read FCycleCode write FCycleCode;
    property SubjectAge: String read FSubjectAge write FSubjectAge;
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
    function Diff(aOld: TMolt; var aList: TStrings): Boolean;
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
    FOrderId: Integer;
    FFamilyId: Integer;
    FGenusId: Integer;
    FSpeciesId: Integer;
    FCountryId: Integer;
    FStateId: Integer;
    FMunicipalityId: Integer;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure Insert;
    procedure Update;
    function Diff(aOld: TSighting; var aList: TStrings): Boolean;
    function Find(aSurvey, aTaxon: Integer): Boolean;
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
    property OrderId: Integer read FOrderId write FOrderId;
    property FamilyId: Integer read FFamilyId write FFamilyId;
    property GenusId: Integer read FGenusId write FGenusId;
    property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    property CountryId: Integer read FCountryId write FCountryId;
    property StateId: Integer read FStateId write FStateId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
  end;

implementation

uses
  cbs_locale, cbs_validations, cbs_fullnames, udm_main;

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
  FEventDate := StrToDate('30/12/1500');
  FOrderNumber := 0;
  FEventType := EmptyStr;
  FSupplierId := 0;
  FRequesterId := 0;
  FSenderId := 0;
  FNotes := EmptyStr;
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
    Add('SELECT * FROM band_history');
    Add('WHERE event_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('event_id').AsInteger;
      FBandId := FieldByName('band_id').AsInteger;
      FEventDate := FieldByName('event_date').AsDateTime;
      FOrderNumber := FieldByName('order_number').AsInteger;
      FEventType := FieldByName('event_type').AsString;
      FSupplierId := FieldByName('supplier_id').AsInteger;
      FRequesterId := FieldByName('requester_id').AsInteger;
      FSenderId := FieldByName('sender_id').AsInteger;
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
    Add('INSERT INTO band_history (band_id, ' +
      'event_date, notes, event_type, supplier_id, order_number, requester_id, ' +
      'sender_id, user_inserted, insert_date) ');
    Add('VALUES (:aband, date(:adate), :anote, :atype, :asupplier, :aorder, ' +
      ':arequester, :asender, :auser, datetime(''now'',''localtime''));');
    ParamByName('ABAND').AsInteger := FBandId;
    ParamByName('ADATE').AsString := FormatDateTime('yyyy-MM-dd', FEventDate);
    ParamByName('ANOTE').AsString := FNotes;
    ParamByName('ATYPE').AsString := FEventType;
    if FSupplierId > 0 then
      ParamByName('ASUPPLIER').AsInteger := FSupplierId
    else
      ParamByName('ASUPPLIER').Clear;
    ParamByName('AORDER').AsInteger := FOrderNumber;
    if FRequesterId > 0 then
      ParamByName('AREQUESTER').AsInteger := FRequesterId
    else
      ParamByName('AREQUESTER').Clear;
    if FSenderId > 0 then
      ParamByName('ASENDER').AsInteger := FSenderId
    else
      ParamByName('ASENDER').Clear;
    ParamByName('AUSER').AsInteger := FUserInserted;
//    GravaLogSQL(SQL);
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM band_history');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
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
    Clear;
    Add('UPDATE band_history SET event_type = :atype, ');
    Add('supplier_id = :asupplier, requester_id = :arequester, ');
    Add('sender_id = :asender, order_number = :aorder, notes = :anote, ');
    Add('user_updated = :auser, update_date = datetime(''now'',''localtime'') ');
    Add('WHERE event_id = :aid;');
    ParamByName('ANOTE').DataType := ftMemo;
    ParamByName('ANOTE').AsString := FNotes;
    ParamByName('ATYPE').AsString := FEventType;
    ParamByName('ASUPPLIER').AsInteger := FSupplierId;
    ParamByName('AREQUESTER').AsInteger := FRequesterId;
    ParamByName('ASENDER').AsInteger := FSenderId;
    ParamByName('AORDER').AsInteger := FOrderNumber;
    ParamByName('AUSER').AsInteger := FUserUpdated;
    ParamByName('AID').AsInteger := FId;
//    GravaLogSQL(Qry.SQL);
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
  FSightingDate := StrToDate('30/12/1500');
  FSightingTime := StrToTime('00:00:00');
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
  FOrderId := 0;
  FFamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
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
    Add('SELECT * FROM sightings');
    Add('WHERE sighting_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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
      FOrderId := FieldByName('order_id').AsInteger;
      FFamilyId := FieldByName('family_id').AsInteger;
      FGenusId := FieldByName('genus_id').AsInteger;
      FSpeciesId := FieldByName('species_id').AsInteger;
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
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
    Add('INSERT INTO sightings (survey_id, taxon_id, ' +
      'sighting_date, notes, breeding_status, subjects_tally, order_id, family_id, ' +
      'genus_id, species_id, ebird_available, user_inserted, insert_date) ');
    Add('VALUES (:asurvey, :ataxon, date(:adate), :anote, :breeding, ' +
      ':aquant, :aorder, :afamily, :agenus, :aspecies, 1, :auser, datetime(''now'',''localtime''));');
    ParamByName('ASURVEY').AsInteger := FSurveyId;
    ParamByName('ATAXON').AsInteger := FTaxonId;
    ParamByName('ADATE').AsString := FormatDateTime('yyyy-MM-dd', FSightingDate);
    ParamByName('ANOTE').AsString := FNotes;
    ParamByName('BREEDING').AsString := FBreedingStatus;
    if FSubjectTally > 0 then
      ParamByName('AQUANT').AsInteger := FSubjectTally
    else
      ParamByName('AQUANT').Clear;
    ParamByName('AORDER').AsInteger := FOrderId;
    ParamByName('AFAMILY').AsInteger := FFamilyId;
    ParamByName('AGENUS').AsInteger := FGenusId;
    ParamByName('ASPECIES').AsInteger := FSpeciesId;
    ParamByName('AUSER').AsInteger := FUserInserted;
//    GravaLogSQL(SQL);
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM sightings');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
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
    Clear;
    Add('UPDATE sightings SET ebird_available = 1, ');
    Add('notes = :anote, breeding_status = :breeding, ');
    if FSubjectTally > 0 then
      Add('subjects_tally = :aquant, ');
    Add('user_updated = :auser, update_date = datetime(''now'',''localtime'') ');
    Add('WHERE sighting_id = :ni;');
    ParamByName('ANOTE').DataType := ftMemo;
    ParamByName('ANOTE').AsString := FNotes;
    ParamByName('BREEDING').AsString := FBreedingStatus;
    if FSubjectTally > 0 then
      ParamByName('AQUANT').AsInteger := FSubjectTally;
    ParamByName('AUSER').AsInteger := FUserUpdated;
    ParamByName('NI').AsInteger := FId;
//    GravaLogSQL(Qry.SQL);
    ExecSQL;
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

  if FieldValuesDiff('Amostragem', aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionDate, aOld.SightingDate, FSightingDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Hora', aOld.SightingTime, FSightingTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Observador', aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff('N'#186' de indiv'#237'duos', aOld.SubjectTally, FSubjectTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Dist'#226'ncia', aOld.SubjectDistance, FSubjectDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMethod, aOld.MethodId, FMethodId, R) then
    aList.Add(R);
  if FieldValuesDiff('N'#186' da lista', aOld.MackinnonListNumber, FMackinnonListNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Capturado', aOld.SubjectCaptured, FSubjectCaptured, R) then
    aList.Add(R);
  if FieldValuesDiff('Visto', aOld.SubjectSeen, FSubjectSeen, R) then
    aList.Add(R);
  if FieldValuesDiff('Ouvido', aOld.SubjectHeard, FSubjectHeard, R) then
    aList.Add(R);
  if FieldValuesDiff('Fotografado', aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    aList.Add(R);
  if FieldValuesDiff('Gravado som', aOld.SubjectRecorded, FSubjectRecorded, R) then
    aList.Add(R);
  if FieldValuesDiff('Machos', aOld.MalesTally, FMalesTally, R) then
    aList.Add(R);
  if FieldValuesDiff('F'#234'meas', aOld.FemalesTally, FFemalesTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Adultos', aOld.AdultsTally, FAdultsTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Imaturos', aOld.ImmatureTally, FImmatureTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Recapturas', aOld.RecapturesTally, FRecapturesTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Novas capturas', aOld.NewCapturesTally, FNewCapturesTally, R) then
    aList.Add(R);
  if FieldValuesDiff('N'#227'o marcados', aOld.UnbandedTally, FUnbandedTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Tipo de detec'#231#227'o', aOld.DetectionType, FDetectionType, R) then
    aList.Add(R);
  if FieldValuesDiff('Status reprodutivo', aOld.BreedingStatus, FBreedingStatus, R) then
    aList.Add(R);
  if FieldValuesDiff('Fora da amostragem', aOld.NotSurveying, FNotSurveying, R) then
    aList.Add(R);
  if FieldValuesDiff('Est'#225' no eBird', aOld.IsOnEbird, FIsOnEbird, R) then
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
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSighting.Find(aSurvey, aTaxon: Integer): Boolean;
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
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('ATAXON').AsInteger := aTaxon;
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

//{ TColorBands }
//
//procedure TColorBands.Clear;
//begin
//  RightLeg.Clear;
//  LeftLeg.Clear;
//end;
//
//function TColorBands.ToString: String;
//begin
//  Result := Format(rs_ColorBandsRightLeft, [RightLeg.ToString, LeftLeg.ToString]);
//end;
//
//function TColorBands.Count: Integer;
//begin
//  Result := RightLeg.Count + LeftLeg.Count;
//end;
//
//{ TBands }
//
//function TBands.Count: Integer;
//var
//  T: Integer;
//begin
//  T := 0;
//
//  if Band1 <> ccNone then
//    Inc(T);
//  if Band2 <> ccNone then
//    Inc(T);
//  if Band3 <> ccNone then
//    Inc(T);
//  if Band4 <> ccNone then
//    Inc(T);
//
//  Result := T;
//end;
//
//function TBands.ToString: String;
//var
//  S: String;
//  L: TStringList;
//  i: Integer;
//begin
//  S := '';
//
//  L := TStringList.Create;
//  try
//    if Band1 <> ccNone then
//    begin
//      L.Add(BandColors[Ord(Band1), 0]);
//    end;
//    if Band2 <> ccNone then
//    begin
//      L.Add(BandColors[Ord(Band2), 0]);
//    end;
//    if Band3 <> ccNone then
//    begin
//      L.Add(BandColors[Ord(Band3), 0]);
//    end;
//    if Band4 <> ccNone then
//    begin
//      L.Add(BandColors[Ord(Band4), 0]);
//    end;
//
//    for i := 0 to L.Count - 1 do
//    begin
//      S := S + L[i];
//      if i < L.Count - 1 then
//        S := S + ',';
//    end;
//  finally
//    FreeAndNil(L);
//  end;
//
//  Result := S;
//end;
//
//procedure TBands.Clear;
//begin
//  Band1 := ccNone;
//  Band2 := ccNone;
//  Band3 := ccNone;
//  Band4 := ccNone;
//end;

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
  FCaptureDate := StrToDate('30/12/1500');
  FCaptureTime := StrToTime('00:00:00');
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

procedure TMolt.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM molts');
    Add('WHERE molt_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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

function TMolt.Diff(aOld: TMolt; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Amostragem', aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff('Captura', aOld.CaptureId, FCaptureId, R) then
    aList.Add(R);
  if FieldValuesDiff('Data captura', aOld.CaptureDate, FCaptureDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Hora captura', aOld.CaptureTime, FCaptureTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Anilhador', aOld.BanderId, FBanderId, R) then
    aList.Add(R);
  if FieldValuesDiff('Anilha', aOld.BandId, FBandId, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P1', aOld.Primary1, FPrimary1, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P2', aOld.Primary2, FPrimary2, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P3', aOld.Primary3, FPrimary3, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P4', aOld.Primary4, FPrimary4, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P5', aOld.Primary5, FPrimary5, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P6', aOld.Primary6, FPrimary6, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P7', aOld.Primary7, FPrimary7, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P8', aOld.Primary8, FPrimary8, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P9', aOld.Primary9, FPrimary9, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda P10', aOld.Primary10, FPrimary10, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S1', aOld.Secondary1, FSecondary1, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S2', aOld.Secondary2, FSecondary2, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S3', aOld.Secondary3, FSecondary3, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S4', aOld.Secondary4, FSecondary4, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S5', aOld.Secondary5, FSecondary5, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S6', aOld.Secondary6, FSecondary6, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S7', aOld.Secondary7, FSecondary7, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S8', aOld.Secondary8, FSecondary8, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda S9', aOld.Secondary9, FSecondary9, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda R1', aOld.Retrix1, FRetrix1, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda R2', aOld.Retrix2, FRetrix2, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda R3', aOld.Retrix3, FRetrix3, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda R4', aOld.Retrix4, FRetrix4, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda R5', aOld.Retrix5, FRetrix5, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda R6', aOld.Retrix6, FRetrix6, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC1', aOld.PrimaryCovert1, FPrimaryCovert1, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC2', aOld.PrimaryCovert2, FPrimaryCovert2, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC3', aOld.PrimaryCovert3, FPrimaryCovert3, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC4', aOld.PrimaryCovert4, FPrimaryCovert4, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC5', aOld.PrimaryCovert5, FPrimaryCovert5, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC6', aOld.PrimaryCovert6, FPrimaryCovert6, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC7', aOld.PrimaryCovert7, FPrimaryCovert7, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC8', aOld.PrimaryCovert8, FPrimaryCovert8, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda PC9', aOld.PrimaryCovert9, FPrimaryCovert9, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda CC', aOld.CarpalCovert, FCarpalCovert, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC1', aOld.GreatCovert1, FGreatCovert1, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC2', aOld.GreatCovert2, FGreatCovert2, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC3', aOld.GreatCovert3, FGreatCovert3, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC4', aOld.GreatCovert4, FGreatCovert4, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC5', aOld.GreatCovert5, FGreatCovert5, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC6', aOld.GreatCovert6, FGreatCovert6, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC7', aOld.GreatCovert7, FGreatCovert7, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC8', aOld.GreatCovert8, FGreatCovert8, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC9', aOld.GreatCovert9, FGreatCovert9, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda GC10', aOld.GreatCovert10, FGreatCovert10, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda Al1', aOld.Alula1, FAlula1, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda Al2', aOld.Alula2, FAlula2, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda Al3', aOld.Alula3, FAlula3, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda LC', aOld.LeastCoverts, FLeastCoverts, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda MC', aOld.MedianCoverts, FMedianCoverts, R) then
    aList.Add(R);
  if FieldValuesDiff('Barra de crescimento', aOld.GrowthBarWidth, FGrowthBarWidth, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  FOrderId := 0;
  FFamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
  FIndividualId := 0;
  FProjectId := 0;
  FCaptureDate := StrToDate('30/12/1500');
  FCaptureTime := StrToTime('00:00:00');
  FCountryId := 0;
  FStateId := 0;
  FMunicipalityId := 0;
  FLocalityId := 0;
  FNetStationId := 0;
  FNetId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FBanderId := 0;
  FAnnotatorId := 0;
  FSubjectStatus := EmptyStr;
  FCaptureType := EmptyStr;
  FSubjectSex := EmptyStr;
  FHowSexed := EmptyStr;
  FBandId := 0;
  FRemovedBandId := 0;
  FRightLegBelow := EmptyStr;
  FLeftLegBelow := EmptyStr;
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
  FOldMolt := EmptyStr;
  FOldPrimariesMolt := EmptyStr;
  FOldSecondariesMolt := EmptyStr;
  FOldRetricesMolt := EmptyStr;
  FOldBodyMolt := EmptyStr;
  FBodyMolt := EmptyStr;
  FFlightFeathersMolt := EmptyStr;
  FFlightFeathersWear := EmptyStr;
  FMoltLimits := EmptyStr;
  FCycleCode := EmptyStr;
  FSubjectAge := EmptyStr;
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

procedure TCapture.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM captures');
    Add('WHERE capture_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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
      FSubjectStatus := FieldByName('subject_status').AsString;
      FCaptureType := FieldByName('capture_type').AsString;
      FSubjectSex := FieldByName('subject_sex').AsString;
      FHowSexed := FieldByName('how_sexed').AsString;
      FBandId := FieldByName('band_id').AsInteger;
      FRemovedBandId := FieldByName('removed_band_id').AsInteger;
      FRightLegBelow := FieldByName('right_leg_below').AsString;
      FLeftLegBelow := FieldByName('left_leg_below').AsString;
      FWeight := FieldByName('weight').AsFloat;
      FTarsusLength := FieldByName('tarsus_length').AsFloat;
      FTarsusDiameter := FieldByName('tarsus_diameter').AsFloat;
      FCulmenLength := FieldByName('culmen_length').AsFloat;
      FExposedCulmen := FieldByName('exposed_culmen').AsFloat;
      FBillWidth := FieldByName('bill_width').AsFloat;
      FBillHeight := FieldByName('bill_height').AsFloat;
      FNostrilBillTip := FieldByName('nostril_bill_tip').AsFloat;
      FSkullLength := FieldByName('skull_length').AsFloat;
      FHaluxLengthTotal := FieldByName('halux_length_total').AsFloat;
      FHaluxLengthFinger := FieldByName('halux_length_finger').AsFloat;
      FHaluxLengthClaw := FieldByName('halux_length_claw').AsFloat;
      FRightWingChord := FieldByName('right_wing_chord').AsFloat;
      FFirstSecondaryChord := FieldByName('first_secondary_chord').AsFloat;
      FTailLength := FieldByName('tail_length').AsFloat;
      FCentralRetrixLength := FieldByName('central_retrix_length').AsFloat;
      FExternalRetrixLength := FieldByName('external_retrix_length').AsFloat;
      FTotalLength := FieldByName('total_length').AsFloat;
      FFeatherMites := FieldByName('feather_mites').AsString;
      FFat := FieldByName('fat').AsString;
      FBroodPatch := FieldByName('brood_patch').AsString;
      FCloacalProtuberance := FieldByName('cloacal_protuberance').AsString;
      FOldMolt := FieldByName('old_molt').AsString;
      FOldPrimariesMolt := FieldByName('old_primaries_molt').AsString;
      FOldSecondariesMolt := FieldByName('old_secondaries_molt').AsString;
      FOldRetricesMolt := FieldByName('old_retrices_molt').AsString;
      FOldBodyMolt := FieldByName('old_body_molt').AsString;
      FBodyMolt := FieldByName('body_molt').AsString;
      FFlightFeathersMolt := FieldByName('flight_feathers_molt').AsString;
      FFlightFeathersWear := FieldByName('flight_feathers_wear').AsString;
      FMoltLimits := FieldByName('molt_limits').AsString;
      FCycleCode := FieldByName('cycle_code').AsString;
      FSubjectAge := FieldByName('subject_age').AsString;
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
    Add('INSERT INTO captures (survey_id, full_name, ');
    Add('taxon_id, individual_id, capture_date, capture_time, locality_id, net_station_id, ');
    Add('net_id, latitude, longitude, bander_id, annotator_id, subject_status, ');
    Add('capture_type, subject_sex, how_sexed, band_id, weight, tarsus_length, ');
    Add('tarsus_diameter, exposed_culmen, bill_width, bill_length, ');
    Add('nostril_bill_tip, skull_length, right_wing_chord, first_secondary_chord, tail_length, ');
    Add('fat, brood_patch, cloacal_protuberance, body_molt, flight_feathers_molt, ');
    Add('flight_feathers_wear, molt_limits, cycle_code, how_aged, skull_ossification, ');
    Add('kipps_index, glucose, hemoglobin, hematocrit, blood_sample, ');
    Add('feather_sample, subject_photographed, photographer_1_id, photographer_2_id, ');
    Add('start_photo_number, end_photo_number, camera_name, removed_band_id, ');
    Add('right_leg_below, left_leg_below, escaped, notes, ');
    Add('order_id, family_id, genus_id, species_id, ');
    Add('municipality_id, state_id, country_id, user_inserted, insert_date)');

    Add('VALUES (:asurvey, :aname, ');
    Add(':ataxon, :aindividual, date(:adate), time(:atime), :alocal, :astation, ');
    Add(':anet, :alatitude, :alongitude, :abander, :arecorder, ');
    Add(':astatus, :anature, :asex, :howsexed, :aband, :weight, ');
    Add(':tarsuslength, :tarsusdiameter, :expculmen, :billwidth, :billheight, :billnp, ');
    Add(':skulllength, :rightwing, :firstsecondary, :taillength, ');
    Add(':fat, :broodpatch, :cloacalprotuberance, :bodymolt, :ffmolt, :ffwear, ');
    Add(':moltlimits, :cyclecode, :howaged, :skullossification, ');
    Add(':kippsindex, :glucose, :hemoglobin, :hematocrit, ');
    Add(':haveblood, :havefeather, :havephotos, ');
    Add(':photographer1, :photographer2, :firstphoto, :lastphoto, :cameraname, ');
    Add(':removedband, :rightleg, :leftleg, :escaped, :anote, ');
    Add(':aorder, :afamily, :agenus, :aspecies, :amunicipio, :aestado, :apais, ');
    Add(':auser, datetime(''now'',''localtime''));');

    ParamByName('ASURVEY').AsInteger := FSurveyId;
    ParamByName('ANAME').AsString :=
      GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, FSubjectSex, FCaptureType, FCycleCode, False);
    ParamByName('ATAXON').AsInteger := FTaxonId;
    ParamByName('AINDIVIDUAL').AsInteger := FIndividualId;
    ParamByName('ADATE').AsString := FormatDateTime('yyyy-mm-dd', FCaptureDate);
    ParamByName('ATIME').AsString := FormatDateTime('hh:nn', FCaptureTime);
    ParamByName('ALOCAL').AsInteger := FLocalityId;
    ParamByName('ASTATION').AsInteger := FNetStationId;
    if (FNetId = 0) then
      ParamByName('ANET').Clear
    else
      ParamByName('ANET').AsInteger := FNetId;
    if (FLatitude > 200.0) then
      ParamByName('ALATITUDE').Clear
    else
      ParamByName('ALATITUDE').AsFloat := FLatitude;
    if (FLongitude > 200.0) then
      ParamByName('ALONGITUDE').Clear
    else
      ParamByName('ALONGITUDE').AsFloat := FLongitude;
    ParamByName('ABANDER').AsInteger := FBanderId;
    ParamByName('ARECORDER').AsInteger := FAnnotatorId;
    ParamByName('ASTATUS').AsString := FSubjectStatus;
    ParamByName('ANATURE').AsString := FCaptureType;
    ParamByName('ASEX').AsString := FSubjectSex;
    ParamByName('HOWSEXED').AsString := FHowSexed;
    if (FBandId = 0) then
      ParamByName('ABAND').Clear
    else
      ParamByName('ABAND').AsInteger := FBandId;
    if (FWeight = 0.0) then
      ParamByName('WEIGHT').Clear
    else
      ParamByName('WEIGHT').AsFloat := FWeight;
    if (FTarsusLength = 0.0) then
      ParamByName('TARSUSLENGTH').Clear
    else
      ParamByName('TARSUSLENGTH').AsFloat := FTarsusLength;
    if (FTarsusDiameter = 0.0) then
      ParamByName('TARSUSDIAMETER').Clear
    else
      ParamByName('TARSUSDIAMETER').AsFloat := FTarsusDiameter;
    if (FExposedCulmen = 0.0) then
      ParamByName('EXPCULMEN').Clear
    else
      ParamByName('EXPCULMEN').AsFloat := FExposedCulmen;
    if (FBillWidth = 0.0) then
      ParamByName('BILLWIDTH').Clear
    else
      ParamByName('BILLWIDTH').AsFloat := FBillWidth;
    if (FBillHeight = 0.0) then
      ParamByName('BILLHEIGHT').Clear
    else
      ParamByName('BILLHEIGHT').AsFloat := FBillHeight;
    if (FNostrilBillTip = 0.0) then
      ParamByName('BILLNP').Clear
    else
      ParamByName('BILLNP').AsFloat := FNostrilBillTip;
    if (FSkullLength = 0.0) then
      ParamByName('SKULLLENGTH').Clear
    else
      ParamByName('SKULLLENGTH').AsFloat := FSkullLength;
    if (FRightWingChord = 0.0) then
      ParamByName('RIGHTWING').Clear
    else
      ParamByName('RIGHTWING').AsFloat := FRightWingChord;
    if (FFirstSecondaryChord = 0.0) then
      ParamByName('FIRSTSECONDARY').Clear
    else
      ParamByName('FIRSTSECONDARY').AsFloat := FFirstSecondaryChord;
    if (FTailLength = 0.0) then
      ParamByName('TAILLENGTH').Clear
    else
      ParamByName('TAILLENGTH').AsFloat := FTailLength;
    ParamByName('FAT').AsString := FFat;
    ParamByName('BROODPATCH').AsString := FBroodPatch;
    ParamByName('CLOACALPROTUBERANCE').AsString := FCloacalProtuberance;
    ParamByName('BODYMOLT').AsString := FBodyMolt;
    ParamByName('FFMOLT').AsString := FFlightFeathersMolt;
    ParamByName('FFWEAR').AsString := FFlightFeathersWear;
    ParamByName('MOLTLIMITS').AsString := FMoltLimits;
    ParamByName('CYCLECODE').AsString := FCycleCode;
    ParamByName('HOWAGED').AsString := FHowAged;
    ParamByName('SKULLOSSIFICATION').AsString := FSkullOssification;
    if (FKippsIndex = 0.0) then
      ParamByName('KIPPSINDEX').Clear
    else
      ParamByName('KIPPSINDEX').AsFloat := FKippsIndex;
    if (FGlucose = 0.0) then
      ParamByName('GLUCOSE').Clear
    else
      ParamByName('GLUCOSE').AsFloat := FGlucose;
    if (FHemoglobin = 0.0) then
      ParamByName('HEMOGLOBIN').Clear
    else
      ParamByName('HEMOGLOBIN').AsFloat := FHemoglobin;
    if (FHematocrit = 0.0) then
      ParamByName('HEMATOCRIT').Clear
    else
      ParamByName('HEMATOCRIT').AsFloat := FHematocrit;
    ParamByName('HAVEBLOOD').AsInteger := Integer(FBloodSample);
    ParamByName('HAVEFEATHER').AsInteger := Integer(FFeatherSample);
    if (FPhotographer1Id > 0) then
    begin
      ParamByName('HAVEPHOTOS').AsInteger := 1;
      ParamByName('PHOTOGRAPHER1').AsInteger := FPhotographer1Id;
      if (FPhotographer2Id > 0) then
        ParamByName('PHOTOGRAPHER2').AsInteger := FPhotographer2Id;
    end else
    begin
      ParamByName('HAVEPHOTOS').AsInteger := 0;
      ParamByName('PHOTOGRAPHER1').Clear;
      ParamByName('PHOTOGRAPHER2').Clear;
    end;
    if (FStartPhotoNumber <> EmptyStr) then
      ParamByName('FIRSTPHOTO').AsInteger := StrToInt(FStartPhotoNumber);
    if (FEndPhotoNumber <> EmptyStr) then
      ParamByName('LASTPHOTO').AsInteger := StrToInt(FEndPhotoNumber);
    ParamByName('CAMERANAME').AsString := FCameraName;
    if (FRemovedBandId = 0) then
      ParamByName('REMOVEDBAND').Clear
    else
      ParamByName('REMOVEDBAND').AsInteger := FRemovedBandId;
    ParamByName('RIGHTLEG').AsString := FRightLegBelow;
    ParamByName('LEFTLEG').AsString := FLeftLegBelow;
    ParamByName('ESCAPED').AsInteger := Integer(FEscaped);
    ParamByName('ANOTE').AsString := FNotes;
    ParamByName('AORDER').AsInteger := FOrderId;
    ParamByName('AFAMILY').AsInteger := FFamilyId;
    ParamByName('AGENUS').AsInteger := FGenusId;
    ParamByName('ASPECIES').AsInteger := FSpeciesId;
    ParamByName('AMUNICIPIO').AsInteger := FMunicipalityId;
    ParamByName('AESTADO').AsInteger := FStateId;
    ParamByName('APAIS').AsInteger := FCountryId;
    ParamByName('AUSER').AsInteger := FUserInserted;

    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM captures');
    Open;
    FId := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
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
    Add('UPDATE captures SET ');
    Add('survey_id = :asurvey,');
    Add('full_name = :aname,');
    Add('locality_id = :alocal,');
    Add('net_station_id = :astation,');
    Add('net_id = :anet,');
    Add('latitude = :alatitude,');
    Add('longitude = :alongitude,');
    Add('cycle_code = :cyclecode,');
    Add('notes = :anote,');
    Add('user_updated = :auser, update_date = datetime(''now'',''localtime'') ');
    Add('WHERE capture_id = :ni;');
    ParamByName('NI').AsInteger := FId;
    ParamByName('ASURVEY').AsInteger := FSurveyId;
    ParamByName('ANAME').AsString :=
      GetCaptureFullname(FCaptureDate, FTaxonId, FBandId, FSubjectSex, FCaptureType, FCycleCode, False);
    ParamByName('ALOCAL').AsInteger := FLocalityId;
    ParamByName('ASTATION').AsInteger := FNetStationId;
    if (FNetId = 0) then
      ParamByName('ANET').Clear
    else
      ParamByName('ANET').AsInteger := FNetId;
    if (FLatitude > 200.0) then
      ParamByName('ALATITUDE').Clear
    else
      ParamByName('ALATITUDE').AsFloat := FLatitude;
    if (FLongitude > 200.0) then
      ParamByName('ALONGITUDE').Clear
    else
      ParamByName('ALONGITUDE').AsFloat := FLongitude;
    ParamByName('CYCLECODE').AsString := FCycleCode;
    ParamByName('ANOTE').AsString := FNotes;
    ParamByName('AUSER').AsInteger := FUserUpdated;

    ExecSQL;
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

  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Amostragem', aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionProject, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff('Data captura', aOld.CaptureDate, FCaptureDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Hora captura', aOld.CaptureTime, FCaptureTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionLocality, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionSamplingPlot, aOld.NetStationId, FNetStationId, R) then
    aList.Add(R);
  if FieldValuesDiff('Rede n'#186, aOld.NetId, FNetId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff('Anilhador', aOld.BanderId, FBanderId, R) then
    aList.Add(R);
  if FieldValuesDiff('Anotador', aOld.AnnotatorId, FAnnotatorId, R) then
    aList.Add(R);
  if FieldValuesDiff('Status', aOld.SubjectStatus, FSubjectStatus, R) then
    aList.Add(R);
  if FieldValuesDiff('Natureza', aOld.CaptureType, FCaptureType, R) then
    aList.Add(R);
  if FieldValuesDiff('Sexo', aOld.SubjectSex, FSubjectSex, R) then
    aList.Add(R);
  if FieldValuesDiff('Sexado como', aOld.HowSexed, FHowSexed, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionBand, aOld.BandId, FBandId, R) then
    aList.Add(R);
  if FieldValuesDiff('Anilha removida', aOld.RemovedBandId, FRemovedBandId, R) then
    aList.Add(R);
  if FieldValuesDiff('Tarso direito', aOld.RightLegBelow, FRightLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff('Tarso esquerdo', aOld.LeftLegBelow, FLeftLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff('Peso', aOld.Weight, FWeight, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento tarso', aOld.TarsusLength, FTarsusLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Di'#226'metro tarso', aOld.TarsusDiameter, FTarsusDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff('C'#250'lmen total', aOld.CulmenLength, FCulmenLength, R) then
    aList.Add(R);
  if FieldValuesDiff('C'#250'lmen exposto', aOld.ExposedCulmen, FExposedCulmen, R) then
    aList.Add(R);
  if FieldValuesDiff('Largura bico', aOld.BillWidth, FBillWidth, R) then
    aList.Add(R);
  if FieldValuesDiff('Altura bico', aOld.BillHeight, FBillHeight, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento narina-ponta', aOld.NostrilBillTip, FNostrilBillTip, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento cr'#226'nio', aOld.SkullLength, FSkullLength, R) then
    aList.Add(R);
  if FieldValuesDiff('H'#225'lux total', aOld.HaluxLengthTotal, FHaluxLengthTotal, R) then
    aList.Add(R);
  if FieldValuesDiff('H'#225'lux dedo', aOld.HaluxLengthFinger, FHaluxLengthFinger, R) then
    aList.Add(R);
  if FieldValuesDiff('H'#225'lux garra', aOld.HaluxLengthClaw, FHaluxLengthClaw, R) then
    aList.Add(R);
  if FieldValuesDiff('Corda asa direita', aOld.RightWingChord, FRightWingChord, R) then
    aList.Add(R);
  if FieldValuesDiff('Primeira secund'#225'ria', aOld.FirstSecondaryChord, FFirstSecondaryChord, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento cauda', aOld.TailLength, FTailLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Retriz central', aOld.CentralRetrixLength, FCentralRetrixLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Retriz externa', aOld.ExternalRetrixLength, FExternalRetrixLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento total', aOld.TotalLength, FTotalLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Plum'#237'colas', aOld.FeatherMites, FFeatherMites, R) then
    aList.Add(R);
  if FieldValuesDiff('Gordura', aOld.Fat, FFat, R) then
    aList.Add(R);
  if FieldValuesDiff('Placa incuba'#231#227'o', aOld.BroodPatch, FBroodPatch, R) then
    aList.Add(R);
  if FieldValuesDiff('Protuber'#226'ncia cloacal', aOld.CloacalProtuberance, FCloacalProtuberance, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda (leg.)', aOld.OldMolt, FOldMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda prim'#225'rias (leg.)', aOld.OldPrimariesMolt, FOldPrimariesMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda secund'#225'rias (leg.)', aOld.OldSecondariesMolt, FOldSecondariesMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda retrizes (leg.)', aOld.OldRetricesMolt, FOldRetricesMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda corpo (leg.)', aOld.OldBodyMolt, FOldBodyMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda corpo', aOld.BodyMolt, FBodyMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Muda voo', aOld.FlightFeathersMolt, FFlightFeathersMolt, R) then
    aList.Add(R);
  if FieldValuesDiff('Desgaste voo', aOld.FlightFeathersWear, FFlightFeathersWear, R) then
    aList.Add(R);
  if FieldValuesDiff('Limites muda', aOld.MoltLimits, FMoltLimits, R) then
    aList.Add(R);
  if FieldValuesDiff('Ciclo muda', aOld.CycleCode, FCycleCode, R) then
    aList.Add(R);
  if FieldValuesDiff('Idade', aOld.SubjectAge, FSubjectAge, R) then
    aList.Add(R);
  if FieldValuesDiff('Etariado como', aOld.HowAged, FHowAged, R) then
    aList.Add(R);
  if FieldValuesDiff('Ossifica'#231#227'o craniana', aOld.SkullOssification, FSkullOssification, R) then
    aList.Add(R);
  if FieldValuesDiff(#205'ndice de Kipp', aOld.KippsIndex, FKippsIndex, R) then
    aList.Add(R);
  if FieldValuesDiff('Glicose', aOld.Glucose, FGlucose, R) then
    aList.Add(R);
  if FieldValuesDiff('Hemoglobina', aOld.Hemoglobin, FHemoglobin, R) then
    aList.Add(R);
  if FieldValuesDiff('Hemat'#243'crito', aOld.Hematocrit, FHematocrit, R) then
    aList.Add(R);
  if FieldValuesDiff('Larvas Philornis', aOld.PhilornisLarvaeTally, FPhilornisLarvaeTally, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletado sangue', aOld.BloodSample, FBloodSample, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletadas penas', aOld.FeatherSample, FFeatherSample, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletada garra', aOld.ClawSample, FClawSample, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletadas fezes', aOld.FecesSample, FFecesSample, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletado parasita', aOld.ParasiteSample, FParasiteSample, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletado indiv'#237'duo', aOld.SubjectCollected, FSubjectCollected, R) then
    aList.Add(R);
  if FieldValuesDiff('Gravado som', aOld.SubjectRecorded, FSubjectRecorded, R) then
    aList.Add(R);
  if FieldValuesDiff('Fotografado', aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    aList.Add(R);
  if FieldValuesDiff('N'#186' de campo', aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Fot'#243'grafo', aOld.Photographer1Id, FPhotographer1Id, R) then
    aList.Add(R);
  if FieldValuesDiff('Fot'#243'grafo 2', aOld.Photographer2Id, FPhotographer2Id, R) then
    aList.Add(R);
  if FieldValuesDiff('Foto inicial', aOld.StartPhotoNumber, FStartPhotoNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Foto final', aOld.EndPhotoNumber, FEndPhotoNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('C'#226'mera', aOld.CameraName, FCameraName, R) then
    aList.Add(R);
  if FieldValuesDiff('Escapou', aOld.Escaped, FEscaped, R) then
    aList.Add(R);
  if FieldValuesDiff('Revisar', aOld.NeedsReview, FNeedsReview, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
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
  FOrderId := 0;
  FFamilyId := 0;
  FSubfamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
  FSex := EmptyStr;
  FAge := EmptyStr;
  FNestId := 0;
  FBirthDate := EmptyStr;
  FBirthDay := 0;
  FBirthMonth := 0;
  FBirthYear := 0;
  FBandingDate := StrToDate('30/12/1500');
  FBandChangeDate := StrToDate('30/12/1500');
  FBandId := 0;
  FBandName := EmptyStr;
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

procedure TIndividual.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM individuals');
    Add('WHERE individual_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('individual_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FTaxonId := FieldByName('taxon_id').AsInteger;
      FOrderId := FieldByName('order_id').AsInteger;
      FFamilyId := FieldByName('family_id').AsInteger;
      FSubfamilyId := FieldByName('subfamily_id').AsInteger;
      FGenusId := FieldByName('genus_id').AsInteger;
      FSpeciesId := FieldByName('species_id').AsInteger;
      FSex := FieldByName('individual_sex').AsString;
      FAge := FieldByName('individual_age').AsString;
      FNestId := FieldByName('nest_id').AsInteger;
      FBirthDate := FieldByName('birth_date').AsString;
      FBirthDay := FieldByName('birth_day').AsInteger;
      FBirthMonth := FieldByName('birth_month').AsInteger;
      FBirthYear := FieldByName('birth_year').AsInteger;
      FBandingDate := FieldByName('banding_date').AsDateTime;
      FBandChangeDate := FieldByName('band_change_date').AsDateTime;
      FBandId := FieldByName('band_id').AsInteger;
      FBandName := FieldByName('band_name').AsString;
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

procedure TIndividual.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('INSERT INTO individuals (taxon_id, order_id, ' +
      'family_id, subfamily_id, genus_id, species_id, band_id, band_name, ' +
      'formatted_name, full_name, right_leg_below, left_leg_below, user_inserted, insert_date)');
    Add('VALUES (:ataxon, :aorder, :afamily, :asubfamily, :agenus, ' +
      ':aspecies, :aband, :abandname, :aformattedname, :aname, :rightleg, :leftleg, ' +
      ':auser, datetime(''now'',''localtime''));');
    ParamByName('ATAXON').AsInteger := FTaxonId;
    ParamByName('AORDER').AsInteger := FOrderId;
    ParamByName('AFAMILY').AsInteger := FFamilyId;
    ParamByName('ASUBFAMILY').AsInteger := FSubfamilyId;
    ParamByName('AGENUS').AsInteger := FGenusId;
    ParamByName('ASPECIES').AsInteger := FSpeciesId;
    ParamByName('ABAND').AsInteger := FBandId;
    ParamByName('ABANDNAME').AsString := FBandName;
    ParamByName('AFORMATTEDNAME').AsString :=
      GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, FSex, True);
    ParamByName('ANAME').AsString :=
      GetIndividualFullname(FTaxonId, FBandId, FRightLegBelow, FLeftLegBelow, FSex, False);
    ParamByName('RIGHTLEG').AsString := FRightLegBelow;
    ParamByName('LEFTLEG').AsString := FLeftLegBelow;
    ParamByName('AUSER').AsInteger := FUserInserted;
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM individuals');
    Open;
    FId := Fields[0].AsInteger;
    Close;
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

  if FieldValuesDiff('Full name', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Sex', aOld.Sex, FSex, R) then
    aList.Add(R);
  if FieldValuesDiff('Age', aOld.Age, FAge, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff('Birth date', aOld.BirthDate, FBirthDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Birth day', aOld.BirthDay, FBirthDay, R) then
    aList.Add(R);
  if FieldValuesDiff('Birth month', aOld.BirthMonth, FBirthMonth, R) then
    aList.Add(R);
  if FieldValuesDiff('Birth year', aOld.BirthYear, FBirthYear, R) then
    aList.Add(R);
  if FieldValuesDiff('Banding date', aOld.BandingDate, FBandingDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Band change date', aOld.BandChangeDate, FBandChangeDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionBand, aOld.BandId, FBandId, R) then
    aList.Add(R);
  if FieldValuesDiff('Double band', aOld.DoubleBandId, FDoubleBandId, R) then
    aList.Add(R);
  if FieldValuesDiff('Removed band', aOld.RemovedBandId, FRemovedBandId, R) then
    aList.Add(R);
  if FieldValuesDiff('Right tarsus', aOld.RightLegBelow, FRightLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff('Left Tarsus', aOld.LeftLegBelow, FLeftLegBelow, R) then
    aList.Add(R);
  if FieldValuesDiff('Right tibia', aOld.RightLegAbove, FRightLegAbove, R) then
    aList.Add(R);
  if FieldValuesDiff('Left tibia', aOld.LeftLegAbove, FLeftLegAbove, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionFather, aOld.FatherId, FFatherId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMother, aOld.MotherId, FMotherId, R) then
    aList.Add(R);
  if FieldValuesDiff('Death date', aOld.DeathDate, FDeathDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Death day', aOld.DeathDay, FDeathDay, R) then
    aList.Add(R);
  if FieldValuesDiff('Death month', aOld.DeathMonth, FDeathMonth, R) then
    aList.Add(R);
  if FieldValuesDiff('Death year', aOld.DeathYear, FDeathYear, R) then
    aList.Add(R);
  if FieldValuesDiff('Recognizable markings', aOld.RecognizableMarkings, FRecognizableMarkings, R) then
    aList.Add(R);
  if FieldValuesDiff('Notes', aOld.Notes, FNotes, R) then
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
  FStatus := EmptyStr;
  FSource := EmptyStr;
  FPrefix := EmptyStr;
  FSuffix := EmptyStr;
  FSupplierId := 0;
  FBandColor := EmptyStr;
  FBandType := EmptyStr;
  FCarrierId := 0;
  FIndividualId := 0;
  FProjectId := 0;
  FReported := False;
  FNotes := EmptyStr;
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
    Add('SELECT * FROM bands');
    Add('WHERE band_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('band_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FSize := FieldByName('band_size').AsString;
      FNumber := FieldByName('band_number').AsInteger;
      FStatus := FieldByName('band_status').AsString;
      FSource := FieldByName('band_source').AsString;
      FPrefix := FieldByName('band_prefix').AsString;
      FSuffix := FieldByName('band_suffix').AsString;
      FSupplierId := FieldByName('supplier_id').AsInteger;
      FBandColor := FieldByName('band_color').AsString;
      FBandType := FieldByName('band_type').AsString;
      FCarrierId := FieldByName('carrier_id').AsInteger;
      FIndividualId := FieldByName('individual_id').AsInteger;
      FProjectId := FieldByName('project_id').AsInteger;
      FReported := FieldByName('band_reported').AsBoolean;
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

procedure TBand.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('INSERT INTO bands (band_size, band_number, ' +
      'band_status, supplier_id, band_type, full_name, user_inserted, insert_date) ');
    Add('VALUES (:asize, :anumber, :astatus, :asupplier, :atype, ' +
      ':aname, :auser, datetime(''now'',''localtime''));');
    ParamByName('ASIZE').AsString := FSize;
    ParamByName('ANUMBER').AsInteger := FNumber;
    ParamByName('ASTATUS').AsString := 'D';
    ParamByName('ASUPPLIER').AsInteger := FSupplierId;
    ParamByName('ATYPE').AsString := 'A';
    ParamByName('ANAME').AsString := GetBandFullname(FSize, FNumber, FSupplierId);
    ParamByName('AUSER').AsInteger := FUserInserted;
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM bands');
    Open;
    FId := Fields[0].AsInteger;
    Close;
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

  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Tamanho', aOld.Size, FSize, R) then
    aList.Add(R);
  if FieldValuesDiff('Número', aOld.Number, FNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Status', aOld.Status, FStatus, R) then
    aList.Add(R);
  if FieldValuesDiff('Origem', aOld.Source, FSource, R) then
    aList.Add(R);
  if FieldValuesDiff('Prefixo', aOld.Prefix, FPrefix, R) then
    aList.Add(R);
  if FieldValuesDiff('Sufixo', aOld.Suffix, FSuffix, R) then
    aList.Add(R);
  if FieldValuesDiff('Fornecedor', aOld.SupplierId, FSupplierId, R) then
    aList.Add(R);
  if FieldValuesDiff('Cor', aOld.BandColor, FBandColor, R) then
    aList.Add(R);
  if FieldValuesDiff('Tipo', aOld.BandType, FBandType, R) then
    aList.Add(R);
  if FieldValuesDiff('Anilhador portador', aOld.CarrierId, FCarrierId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionProject, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff('Reportada', aOld.Reported, FReported, R) then
    aList.Add(R);
  if FieldValuesDiff('Anotações', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

end.

