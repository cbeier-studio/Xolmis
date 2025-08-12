{ Xolmis Record Types library

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

unit models_record_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TTaxonHierarchy = record
    OrderId: Integer;
    FamilyId: Integer;
    SubfamilyId: Integer;
    GenusId: Integer;
    SpeciesId: Integer;
    SubspeciesGroupId: Integer;
  end;

  TSiteHierarchy = record
    CountryId: Integer;
    StateId: Integer;
    MunicipalityId: Integer;
  end;

type

  { TXolmisRecord }

  TXolmisRecord = class(TPersistent)
  protected
    FId: Integer;
    FGuid: String;
    FUserInserted: Integer;
    FUserUpdated: Integer;
    FInsertDate: TDateTime;
    FUpdateDate: TDateTime;
    FMarked: Boolean;
    FExported: Boolean;
    FActive: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; virtual;
    function IsNew: Boolean; inline;
  published
    property Id: Integer read FId write FId;
    property Guid: String read FGuid write FGuid;
    property UserInserted: Integer read FUserInserted write FUserInserted;
    property UserUpdated: Integer read FUserUpdated write FUserUpdated;
    property InsertDate: TDateTime read FInsertDate write FInsertDate;
    property UpdateDate: TDateTime read FUpdateDate write FUpdateDate;
    property Marked: Boolean read FMarked write FMarked;
    property Exported: Boolean read FExported write FExported;
    property Active: Boolean read FActive write FActive;
  end;

  TXolmisRecordClass = class of TXolmisRecord;

type

  { TCustomTaxon }

  TCustomTaxon = class(TXolmisRecord)
  protected
    FFullName: String;
    FFormattedName: String;
    FAuthorship: String;
    //FRankId: Integer;
    FParentTaxonId: Integer;
    FValidId: Integer;
    FOrderId: Integer;
    FFamilyId: Integer;
    FGenusId: Integer;
    FSpeciesId: Integer;
  public
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce; virtual;
  published
    property FullName: String read FFullName write FFullName;
    property FormattedName: String read FFormattedName write FFormattedName;
    property Authorship: String read FAuthorship write FAuthorship;
    //property RankId: Integer read FRankId write FRankId;
    property ParentTaxonId: Integer read FParentTaxonId write FParentTaxonId;
    property ValidId: Integer read FValidId write FValidId;
    property OrderId: Integer read FOrderId write FOrderId;
    property FamilyId: Integer read FFamilyId write FFamilyId;
    property GenusId: Integer read FGenusId write FGenusId;
    property SpeciesId: Integer read FSpeciesId write FSpeciesId;
  end;

{ Enumerations and types used in records }

type
  // Users
  TUserRank = (urAdministrator, urStandard, urVisitor);

  // Individuals and Captures
  TSex = (sexUnknown, sexMale, sexFemale);
  TAge = (ageUnknown, ageNestling, ageFledgling, ageJuvenile, ageAdult, ageFirstYear, ageSecondYear, ageThirdYear,
    ageFourthYear, ageFifthYear);

  // Captures
  TCaptureType = (cptNew, cptRecapture, cptSameDay, cptChangeBand, cptUnbanded);
  TSubjectStatus = (sstNormal, sstInjured, sstWingSprain, sstStressed, sstDead);

  // Feathers
  TFeatherDataSource = (fdsUnknown, fdsCapture, fdsSighting, fdsPhoto);
  TSymmetry = (symUnknown, symSymmetrical, symAsymmetrical);
  TFeatherTrait = (ftrBody, ftrPrimary, ftrSecondary, ftrRectrix, ftrPrimaryCovert, ftrGreatCovert,
    ftrMedianCovert, ftrLesserCovert, ftrCarpalCovert, ftrAlula);
  TBodySide = (bsdNotApplicable, bsdRight, bsdLeft);
  TFeatherAge = (fageUnknown, fageNestling, fageFledgling, fageAdult, fageFirstYear, fageSecondYear, fageThirdYear,
    fageFourthYear, fageFifthYear);

  // Bands
  TBandColorCode = (ccNone = -1, ccMetal = 0, ccAnotherMetal = 1, ccYellow = 2, ccOrange = 3,
    ccRed = 4, ccCarmine = 5, ccPink = 6, ccViolet = 7, ccPaleBlue = 8, ccBlue = 9, ccGreen = 10,
    ccLime = 11, ccUmber = 12, ccWhite = 13, ccSilver = 14, ccBlack = 15);
  TBodyPart = (bpRightTibia, bpLeftTibia, bpRightTarsus, bpLeftTarsus, bpRightWing, bpLeftWing, bpNeck);
  TMarkType = (mkButtEndBand, mkFlag, mkCollar, mkWingTag, mkTriangularBand, mkLockOnBand, mkRivetBand,
    mkClosedBand, mkOther);
  TBandStatus = (bstAvailable, bstUsed, bstRemoved, bstBroken, bstLost, bstTransferred);
  TBandSource = (bscAcquiredFromSupplier, bscTransferBetweenBanders, bscLivingBirdBandedByOthers,
    bscDeadBirdBandedByOthers, bscFoundLoose);
  TBandEvent = (bevOrder, bevReceive, bevTransfer, bevRetrieve, bevReport, bevUse, bevDischarge);

  // Botanical taxnomy
  TQualifier = (qfNone, qfSpuh, qfConfer, qfAffinis, qfQuestion);
  TAddendum = (adNone, adGenus, adSpecies, adInfraspecies);
  TBotanicalRank = (brNone,
    {Realm}
    brRealm, brSubrealm,
    {Kingdom}
    brKingdom, brSubkingdom,
    {Phylum}
    brSuperphylum, brPhylum, brSubphylum,
    {Class}
    brSuperclass, brClass, brSubclass,
    {Order}
    brSuperorder, brOrder, brSuborder, brInfraorder,
    {Family}
    brSuperfamily, brEpifamily, brFamily, brSubfamily, brInfrafamily,
    {Tribe}
    brTribe, brSubtribe, brInfratribe,
    {Genus}
    brSupergenus, brGenus, brSubgenus,
    {Section}
    brSection, brSubsection,
    {Series}
    brSeries, brSubseries,
    {Species}
    brSuperspecies, brSpecies,
    {Subspecies}
    brSubspecies,
    {Variety}
    brVariety, brSubvariety,
    {Form}
    brForm, brSubform,
    {Special ranks}
    brCultivarGroup, brCultivar, brGrex, brHybrid);
  TBotanicalRankMap = specialize TFPGMap<String, TBotanicalRank>;

  // Bird taxonomy
  TBirdTaxonomy = (btClements, btIoc, btCbro);
  TBirdTaxonomies = set of TBirdTaxonomy;
  TTaxonomyAction = (taNew, taSplit, taLump, taMove, taUpdate);
  TZooRank = (trNone,
    {Domain}
    trDomain, trSubDomain,
    {Kingdom}
    trHyperkingdom, trSuperkingdom, trKingdom, trSubkingdom, trInfrakingdom, trParvkingdom,
    {Phylum}
    trSuperphylum, trPhylum, trSubphylum, trInfraphylum, trMicrophylum,
    {Class}
    trSuperclass, trClass, trSubclass, trInfraclass, trSubterclass, trParvclass,
    {Division}
    trSuperdivision, trDivision, trSubdivision, trInfradivision,
    {Legion}
    trSuperlegion, trLegion, trSublegion, trInfralegion,
    {Cohort}
    trSupercohort, trCohort, trSubcohort, trInfracohort,
    {Order}
    trGigaorder, trMegaorder, trGrandorder, trHyperorder, trSuperorder, trSeriesOrder,
    trOrder, trNanorder, trHypoorder, trMinorder, trSuborder, trInfraorder, trParvorder,
    {Section}
    trSection, trSubsection,
    {Family}
    trGigafamily, trMegafamily, trGrandfamily, trHyperfamily, trSuperfamily, trEpifamily,
    trSeriesFamily, trGroupFamily, trFamily, trSubfamily, trInfrafamily,
    {Tribe}
    trSupertribe, trTribe, trSubtribe, trInfratribe,
    {Genus}
    trSupergenus, trGenus, trSubgenus,
    {Species}
    trSuperspecies, trSpecies,
    {Subspecies}
    trSubspecies, trMonotypicGroup, trPolitypicGroup,
    {eBird special taxa}
    trForm, trSpuh, trHybrid, trIntergrade, trDomestic, trSlash);
  TEbirdRank = (erSpuh, erHybrid, erIntergrade, erDomestic, erSlash, erForm);
  TTaxonFilter = (tfAll, tfMain, {tfKingdoms, tfPhyla, tfClasses,} tfOrders, tfFamilies, tfTribes,
    tfGenera, tfSpecies, tfSubspecies, tfSubspeciesGroups, tfSpuhs, tfSlashes, tfForms, tfDomestics,
    tfHybrids, tfIntergrades);
  TTaxonFilters = set of TTaxonFilter;

  // Charts
  TChartCounts = record
    XValue: Integer;
    YValues: array of Double;
  end;

  // Gazetteer and geography
  TSiteRank = (srNone, srCountry, srState, srRegion, srMunicipality, srDistrict, srLocality);
  TGazetteerFilter = (gfAll, gfCountries, gfStates, gfRegions, gfCities, gfDistricts, gfLocalities);
  TGazetteerFilters = set of TGazetteerFilter;
  TCoordinatePrecision = (cpEmpty = -1, cpExact, cpApproximated, cpReference);

  // Projects
  TGoalStatus = (gstPending, gstReached, gstCanceled);
  TActivityStatus = (astToDo, astInProgress, astDone, astCanceled, astDelayed, astNeedsReview, astBlocked);

  // Where is it used?
  TAuthor = record
    Id: Integer;
    Citation: String;
  end;
  TAuthors = array of TAuthor;

  // Weather
  TWeatherSampleMoment = (wmNone, wmStart, wmMiddle, wmEnd);
  TPrecipitation = (wpEmpty = -1, wpNone, wpFog, wpMist, wpDrizzle, wpRain);

  // Vegetation
  TStratumDistribution = (
    disNone,
    disRare,
    disFewSparseIndividuals,
    disOnePatch,
    disOnePatchFewSparseIndividuals,
    disManySparseIndividuals,
    disOnePatchManySparseIndividuals,
    disFewPatches,
    disFewPatchesSparseIndividuals,
    disManyPatches,
    disManyPatchesSparseIndividuals,
    disHighDensityIndividuals,
    disContinuousCoverWithGaps,
    disContinuousDenseCover,
    disContinuousDenseCoverWithEdge
  );

  // Specimens
  TSpecimenType = (
    sptEmpty = -1,
    sptWholeCarcass,
    sptPartialCarcass,
    sptNest,
    sptBones,
    sptEgg,
    sptParasites,
    sptFeathers,
    sptBlood,
    sptClaw,
    sptSwab,
    sptTissues,
    sptFeces,
    sptRegurgite
  );

  // Nests and Eggs
  TNestFate = (nfLoss, nfSuccess, nfUnknown);
  TNestRole = (nrlUnknown, nrlMale, nrlFemale, nrlHelper, nrlOffspring);
  TEggShape = (esUnknown, esSpherical, esElliptical, esOval, esPiriform, esConical, esBiconical, esCylindrical,
    esLongitudinal);
  TEggshellPattern = (espUnknown, espSpots, espBlotches, espSquiggles, espStreaks, espScrawls, espSpotsSquiggles,
    espBlotchesSquiggles);
  TEggshellTexture = (estUnknown, estChalky, estShiny, estGlossy, estPitted);
  TNestStatus = (nstInactive, nstActive, nstUnknown);
  TNestStage = (nsgInactive, nsgConstruction, nsgLaying, nsgIncubation, nsgHatching, nsgNestling, nsgUnknown);

  // Images
  TImageType = (
    itEmpty = -1,
    itBirdInHandFlank,
    itBirdInHandBelly,
    itBirdInHandBack,
    itBirdInHandWing,
    itBirdInHandTail,
    itBirdInHandHead,
    itBirdInHandFeet,
    itFreeBirdStanding,
    itFreeBirdFlying,
    itFreeBirdSwimming,
    itFreeBirdForraging,
    itFreeBirdCopulating,
    itFreeBirdBuildingNest,
    itFreeBirdDisplaying,
    itFreeBirdIncubating,
    itFreeBirdVocalizing,
    itFreeBirdAgonistic,
    itDeadBird,
    itBirdFlock,
    itBirdNest,
    itBirdEgg,
    itBirdNestling,
    itEctoparasite,
    itFootprint,
    itFeather,
    itFeces,
    itFood,
    itEnvironment,
    itFieldwork,
    itTeam
  );

  // Xolmis Mobile
  TMobileContentType = (mctEmpty, mctInventory, mctInventories, mctNest, mctNests, mctSpecimens);
  TMobileInventoryType = (invQualitativeFree, invQualitativeTimed, invQualitativeInterval, invMackinnonList,
                          invTransectionCount, invPointCount, invBanding, invCasual);

{ Constants used in records }

const
  // Users
  USER_RANKS: array[TUserRank] of Char = ('A', 'S', 'V');

  // Individuals and Captures
  SEXES: array[TSex] of String = ('U', 'M', 'F');
  AGES: array[TAge] of String = ('U', 'N', 'F', 'J', 'A', 'Y', 'S', 'T', '4', '5');

  // Captures
  CAPTURE_TYPES: array[TCaptureType] of Char = ('N', 'R', 'S', 'C', 'U');
  SUBJECT_STATUSES: array[TSubjectStatus] of Char = ('N', 'I', 'W', 'X', 'D');
  CLOACAL_PROTUBERANCE_VALUES: array [0 .. 4] of String = ('U', 'N', 'S', 'M', 'L');
  BROOD_PATCH_VALUES: array [0 .. 4] of String          = ('F', 'N', 'V', 'W', 'O');
  FAT_VALUES: array [0 .. 7] of String                  = ('N', 'T', 'L', 'H', 'F', 'B', 'G', 'V');
  BODY_MOLT_VALUES: array [0 .. 6] of String            = ('N', 'T', 'S', 'H', 'G', 'A', 'F');
  FLIGHT_MOLT_VALUES: array [0 .. 2] of String          = ('N', 'S', 'A');
  FEATHER_WEAR_VALUES: array [0 .. 5] of String         = ('N', 'S', 'L', 'M', 'H', 'X');
  SKULL_OSSIFICATION_VALUES: array [0 .. 6] of String   = ('N', 'T', 'L', 'H', 'G', 'A', 'F');

  // Feathers
  FEATHER_DATA_SOURCES: array[TFeatherDataSource] of String = ('U', 'C', 'S', 'P');
  SYMMETRIES: array[TSymmetry] of String = ('U', 'S', 'A');
  FEATHER_TRAITS: array[TFeatherTrait] of String = ('B', 'P', 'S', 'R', 'PC', 'GC', 'MC', 'LC', 'CC', 'AL');
  BODY_SIDES: array[TBodySide] of String = ('NA', 'R', 'L');
  FEATHER_AGES: array[TFeatherAge] of String = ('U', 'N', 'F', 'A', 'Y', 'S', 'T', '4', '5');

  // Bands
  BAND_COLORS: array [0..15, 0..1] of String = (('M', '$00C0C0C0'), ('A', '$00008080'),
    ('Y', '$0000FFFF'), ('O', '$001AB5FF'), ('R', '$000000FF'), ('C', '$00FF00FF'),
    ('K', '$00D2A6FF'), ('V', '$00FF3C9D'), ('P', '$00FFA54A'), ('B', '$00FF0000'),
    ('G', '$00008000'), ('L', '$0000FF00'), ('U', '$00004080'), ('W', '$00FFFFFF'),
    ('S', '$00808080'), ('N', '$00000000'));
  CEMAVE_BAND_SIZES: array[1..19] of Char = ('A','C','D','E','F','G','H','J','L','M','N','P','R','S','T','U','V','X','Z');
  BAND_STATUSES: array[TBandStatus] of Char = ('D', 'U', 'R', 'Q', 'P', 'T');
  MARK_TYPES: array[TMarkType] of Char = ('A', 'F', 'N', 'W', 'T', 'L', 'R', 'C', 'O');
  BAND_SOURCES: array[TBandSource] of Char = ('A', 'T', 'L', 'D', 'F');
  BAND_EVENTS: array[TBandEvent] of Char = ('O', 'C', 'T', 'R', 'P', 'U', 'D');

  // Botanical taxonomy
  QUALIFIERS: array[TQualifier] of String = ('', 'sp.', 'cf.', 'aff.', '?');
  BOTANICAL_RANKS: array[TBotanicalRank] of String = ('', 'R.', 'SR.', 'K.', 'sk.', 'SPh.', 'ph.',
    'subph.', 'sc.', 'c.', 'subc.', 'superod.', 'ord.', 'subord.', 'infraord.',
    'superfam.', 'epifam.', 'fam.', 'subfam.', 'infrafam.', 'tr.', 'subtr.', 'infratr.',
    'superg.', 'g.', 'subg.', 'sect.', 'subsect.', 'ser.', 'subser.',
    'supersp.', 'sp.', 'subsp.', 'var.', 'subvar.', 'f.', 'subf.',
    'cultivar group', 'cultivar', 'grex', 'hybrid');
  INFRA_RANKS: set of TBotanicalRank = [brSubspecies, brVariety, brSubvariety, brForm, brSubform];

  // Bird taxonomy
  TAXONOMY_NAMES: array [0 .. 2] of String = ('Clements/eBird', 'IOC', 'CBRO');
  ZOOLOGICAL_RANKS: array[TZooRank] of String = ('', 'D.', 'SD.', 'HK.', 'SK.', 'K.', 'sk.', 'ik.', 'pk.', 'SPh.', 'ph.',
    'subph.', 'infraph.', 'microph.', 'sc.', 'c.', 'subc.', 'infrac.', 'stc.', 'parvc.', 'sdiv.',
    'div.', 'subdiv.', 'infradiv.', 'sleg.', 'leg.', 'subleg.', 'infraleg.', 'scoh.', 'coh.',
    'subcoh.', 'infracoh.', 'Gord.', 'Mord.', 'grandord.', 'Hord.', 'superod.', 'seriesord.',
    'ord.', 'nord.', 'hypoord.', 'minord.', 'subord.', 'infraord.', 'parvord.', 'sect.', 'subsect.',
    'Gfam.', 'Mfam.', 'grandfam.', 'hyperfam.', 'superfam.', 'epifam.', 'seriesfam.', 'groupfam.',
    'fam.', 'subfam.', 'infrafam.', 'supertr.', 'tr.', 'subtr.', 'infratr.', 'superg.', 'g.',
    'subg.', 'supersp.', 'sp.', 'ssp.', 'grp. (mono)', 'grp. (poli)', 'f.', 'spuh', 'hybrid',
    'intergrade', 'domest.', 'slash');

  // Gazetteer and geography
  SITE_RANKS: array[TSiteRank] of String = ('', 'P', 'E', 'R', 'M', 'D', 'L');
  COORDINATE_PRECISIONS: array[TCoordinatePrecision] of String = ('', 'E', 'A', 'R');

  // Projects
  GOAL_STATUSES: array [TGoalStatus] of String = ('P', 'R', 'C');
  ACTIVITY_STATUSES: array[TActivityStatus] of String = ('T','P','F','C','D','R','B');

  // Weather
  SAMPLE_MOMENTS: array [TWeatherSampleMoment] of String = ('', 'S', 'M', 'E');
  PRECIPITATION_VALUES: array [TPrecipitation] of String = ('', 'N', 'F', 'M', 'D', 'R');

  // Specimens
  SPECIMEN_TYPES: array [0..12] of String = ('WS', 'PS', 'N', 'B', 'E', 'P', 'F', 'BS', 'C', 'S', 'T', 'D', 'R');

  // Nests and Eggs
  NEST_FATES: array [TNestFate] of Char = ('L', 'S', 'U');
  NEST_ROLES: array[TNestRole] of Char = ('U', 'M', 'F', 'H', 'O');
  EGG_SHAPES: array [TEggShape] of Char = ('U', 'S', 'E', 'O', 'P', 'C', 'B', 'Y', 'L');
  EGGSHELL_PATTERNS: array[TEggshellPattern] of String = ('U', 'P', 'B', 'S', 'T', 'W', 'PS', 'BS');
  EGGSHELL_TEXTURES: array[TEggshellTexture] of Char = ('U', 'C', 'S', 'G', 'P');
  NEST_STATUSES: array[TNestStatus] of Char = ('I', 'A', 'U');
  NEST_STAGES: array[TNestStage] of Char = ('X', 'C', 'L', 'I', 'H', 'N', 'U');

  // Images
  IMAGE_TYPES: array[TImageType] of String = (
    '',
    'flank',
    'belly',
    'back',
    'wing',
    'tail',
    'head',
    'feet',
    'stand',
    'fly',
    'swim',
    'forr',
    'copul',
    'build',
    'disp',
    'incub',
    'vocal',
    'agon',
    'dead',
    'flock',
    'nest',
    'egg',
    'nstln',
    'paras',
    'fprnt',
    'feath',
    'feces',
    'food',
    'envir',
    'fwork',
    'team'
  );

implementation

uses
  utils_global;

{ TCustomTaxon }

procedure TCustomTaxon.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCustomTaxon then
  begin
    FullName       := TCustomTaxon(Source).FullName;
    FormattedName  := TCustomTaxon(Source).FormattedName;
    Authorship     := TCustomTaxon(Source).Authorship;
    ParentTaxonId  := TCustomTaxon(Source).ParentTaxonId;
    ValidId        := TCustomTaxon(Source).ValidId;
    OrderId        := TCustomTaxon(Source).OrderId;
    FamilyId       := TCustomTaxon(Source).FamilyId;
    GenusId        := TCustomTaxon(Source).GenusId;
    SpeciesId      := TCustomTaxon(Source).SpeciesId;
  end;
end;

procedure TCustomTaxon.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FFormattedName := EmptyStr;
  FAuthorship := EmptyStr;
  //FRankId := 0;
  FParentTaxonId := 0;
  FValidId := 0;
  FOrderId := 0;
  FFamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
end;

function TCustomTaxon.Clone: TXolmisRecord;
begin
  Result := TCustomTaxon.Create;
  Result.Assign(Self);
end;

{ TXolmisRecord }

constructor TXolmisRecord.Create;
begin
  inherited Create;
  Clear;
end;

procedure TXolmisRecord.Assign(Source: TPersistent);
begin
  if Source is TXolmisRecord then
  begin
    FId := TXolmisRecord(Source).FId;
    FGuid := TXolmisRecord(Source).FGuid;
    FUserInserted := TXolmisRecord(Source).FUserInserted;
    FUserUpdated := TXolmisRecord(Source).FUserUpdated;
    FInsertDate := TXolmisRecord(Source).FInsertDate;
    FUpdateDate := TXolmisRecord(Source).FUpdateDate;
    FMarked := TXolmisRecord(Source).FMarked;
    FExported := TXolmisRecord(Source).FExported;
    FActive := TXolmisRecord(Source).FActive;
  end
  else
    inherited Assign(Source);
end;

procedure TXolmisRecord.AssignTo(Dest: TPersistent);
begin
  if Dest is TXolmisRecord then
    TXolmisRecord(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TXolmisRecord.Clear;
begin
  FId := 0;
  FGuid := EmptyStr;
  FUserInserted := 0;
  FUserUpdated := 0;
  FInsertDate := NullDateTime;
  FUpdateDate := NullDateTime;
  FMarked := False;
  FExported := False;
  FActive := False;
end;

function TXolmisRecord.Clone: TXolmisRecord;
var
  C: TClass;
begin
  C := ClassType;
  Result := TXolmisRecordClass(C).Create;
  Result.Assign(Self);
end;

function TXolmisRecord.IsNew: Boolean;
begin
  Result := FId = 0;
end;

end.

