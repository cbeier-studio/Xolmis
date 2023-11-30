unit lib_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, DB, SQLDB, StrUtils, Generics.Collections, RegExpr, CheckLst;

const
  NomeApp: String           = 'Xolmis Taxonomies Editor';
  PrereleaseStage: String   = 'Alpha';
  AdminId: Integer          = 1;

  ZooRanks: array of String = ('D.', 'SD.', 'HK.', 'SK.', 'K.', 'sk.', 'ik.', 'pk.', 'SPh.', 'ph.',
    'subph.', 'infraph.', 'microph.', 'sc.', 'c.', 'subc.', 'infrac.', 'stc.', 'parvc.', 'sdiv.',
    'div.', 'subdiv.', 'infradiv.', 'sleg.', 'leg.', 'subleg.', 'infraleg.', 'scoh.', 'coh.',
    'subcoh.', 'infracoh.', 'Gord.', 'Mord.', 'grandord.', 'Hord.', 'superod.', 'seriesord.',
    'ord.', 'nord.', 'hypoord.', 'minord.', 'subord.', 'infraord.', 'parvord.', 'sect.', 'subsect.',
    'Gfam.', 'Mfam.', 'grandfam.', 'hyperfam.', 'superfam.', 'epifam.', 'seriesfam.', 'groupfam.',
    'fam.', 'subfam.', 'infrafam.', 'supertr.', 'tr.', 'subtr.', 'infratr.', 'superg.', 'g.',
    'subg.', 'supersp.', 'sp.', 'ssp.', 'grp. (mono)', 'grp. (poli)', 'f.', 'spuh', 'hybrid',
    'intergrade', 'domest.', 'slash');

type
  TZooRank = ({Domain} trDomain, trSubDomain,
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

  TTaxonFilter = (tfAll, tfMain, {tfKingdoms, tfPhyla, tfClasses,} tfOrders, tfFamilies, tfTribes,
    tfGenera, tfSpecies, tfSubspecies, tfSubspeciesGroups, tfSpuhs, tfSlashes, tfForms, tfDomestics,
    tfHybrids, tfIntergrades);
  TTaxonFilters = set of TTaxonFilter;

  TBirdTaxonomy = (btClements, btIoc, btCbro);
  TBirdTaxonomies = set of TBirdTaxonomy;

  TTaxonomyAction = (taNew, taSplit, taLump, taMove, taUpdate);

  TTableType = (tbNone,
    tbTaxonRanks,
    tbZooTaxa,
    tbPackages,
    tbTaxonChanges);

  TFilterValue = (fvNone, fvReset, fvAll, fvMarked, fvUnmarked, fvDeleted, fvQueued);
  TCriteriaType = (crNone,
    crLike, crStartLike, crEqual, crDistinct,
    crBetween, crMoreThan, crLessThan,
    crNull, crNotNull);
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TSortType = (stNone, stAlphanumeric, stNumeric, stDateTime, stBoolean, stTaxonomic);
  TSearchDataType = (sdtText, sdtInteger, sdtFloat, sdtDate, sdtTime, sdtDateTime, sdtBoolean, sdtList,
    sdtLookup);
  TSortedField = class
    FieldName: String;
    Direction: TSortDirection;
    Collation: String;
    Lookup: Boolean;
  end;
  TSortedFields = specialize TObjectList<TSortedField>;
  TFilterType = (tcTexto, tcInteiro, tcDecimal, tcData, tcHora, tcDataHora, tcLista, tcBool, tcLookup);
  TSQLAndOr = (aoNone, aoAnd, aoOr);
  TRecordActiveStatus = (rsAll, rsActive, rsInactive, rsNone);

type

  TCustomSearchField = class
  private
    FDataType: TSearchDataType;
    FCriteria: TCriteriaType;
    FValue1: String;
    FValue2: String;
    FLookup: Boolean;
  public

  published
    property DataType: TSearchDataType read FDataType write FDataType default sdtText;
    property Criteria: TCriteriaType read FCriteria write FCriteria default crLike;
    property Value1: String read FValue1 write FValue1;
    property Value2: String read FValue2 write FValue2;
    property Lookup: Boolean read FLookup write FLookup default False;
  end;

  { TSearchField }

  TSearchField = class(TCustomSearchField)
  private
    FFieldName: String;
    FDisplayName: String;
  public
    constructor Create(aFieldName, aDisplayName: String; aDataType: TSearchDataType = sdtText;
      aCriteria: TCriteriaType = crLike; IsLookup: Boolean = False;
      aValue1: String = ''; aValue2: String = '');
  published
    property FieldName: String read FFieldName write FFieldName;
    property DisplayName: String read FDisplayName write FDisplayName;
  end;

  TSearchFields = specialize TObjectList<TSearchField>;

type

  { TSearchGroup }

  TSearchGroup = class
  private
    FFields: TSearchFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Fields: TSearchFields read FFields write FFields;
  end;

  TSearchGroups = specialize TObjectList<TSearchGroup>;

  { TCustomSearch }

  TCustomSearch = class
  private
    FTableType: TTableType;
    FTableAlias: String;
    FFields: TSearchGroups;
    FQuickFilters: TSearchGroups;
    FSortFields: TSortedFields;
    FDataSet: TSQLQuery;
    FRecordActive: TRecordActiveStatus;
    function GetCount: Integer;
    function GetSQLString: String;
  public
    constructor Create(aTable: TTableType);
    destructor Destroy; override;
    procedure Reset;
    function RunSearch: Integer;
  published
    property TableType: TTableType read FTableType write FTableType;
    property TableAlias: String read FTableAlias write FTableAlias;
    property Fields: TSearchGroups read FFields write FFields;
    property QuickFilters: TSearchGroups read FQuickFilters write FQuickFilters;
    property SortFields: TSortedFields read FSortFields write FSortFields;
    property Count: Integer read GetCount;
    property DataSet: TSQLQuery read FDataSet write FDataSet;
    property RecordActive: TRecordActiveStatus read FRecordActive write FRecordActive default rsActive;
  end;

type

  { TXolmisRecord }

  TXolmisRecord = class
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
  public
    procedure Clear; virtual;
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

  { TRank }

  TRank = class(TXolmisRecord)
  protected
    FName: String;
    FAcronym: String;
    FRankIndex: Integer;
    FMainRank: Boolean;
    FSubrank: Boolean;
    FInfrarank: Boolean;
    FInfraspecific: Boolean;
    FZoologicalCode: Boolean;
    FBotanicalCode: Boolean;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
  published
    property Name: String read FName write FName;
    property Acronym: String read FAcronym write FAcronym;
    property RankIndex: Integer read FRankIndex write FRankIndex;
    property MainRank: Boolean read FMainRank write FMainRank;
    property Subrank: Boolean read FSubrank write FSubrank;
    property Infrarank: Boolean read FInfrarank write FInfrarank;
    property Infraspecific: Boolean read FInfraspecific write FInfraspecific;
    property ZoologicalCode: Boolean read FZoologicalCode write FZoologicalCode;
    property BotanicalCode: Boolean read FBotanicalCode write FBotanicalCode;
  end;

  { TCustomTaxon }

  TCustomTaxon = class(TXolmisRecord)
    protected
      FFullName: String;
      FFormattedName: String;
      FAuthorship: String;
      FRankId: Integer;
      FParentTaxonId: Integer;
      FValidId: Integer;
      FOrderId: Integer;
      FFamilyId: Integer;
      FGenusId: Integer;
      FSpeciesId: Integer;
    public
      procedure Clear; override;
    published
      property FullName: String read FFullName write FFullName;
      property FormattedName: String read FFormattedName write FFormattedName;
      property Authorship: String read FAuthorship write FAuthorship;
      property RankId: Integer read FRankId write FRankId;
      property ParentTaxonId: Integer read FParentTaxonId write FParentTaxonId;
      property ValidId: Integer read FValidId write FValidId;
      property OrderId: Integer read FOrderId write FOrderId;
      property FamilyId: Integer read FFamilyId write FFamilyId;
      property GenusId: Integer read FGenusId write FGenusId;
      property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    end;

  { TTaxon }

  TTaxon = class(TCustomTaxon)
  protected
    FEnglishName: String;
    FPortugueseName: String;
    FSpanishName: String;
    FSortNum: Double;
    FQuickCode: String;
    FExtinct: Boolean;
    FExtinctionYear: String;
    FDistribution: String;
    FEbirdCode: String;
    FClementsTaxonomy: Boolean;
    FSubfamilyId: Integer;
    FGenusEpithet: String;
    FSpeciesEpithet: String;
    FSubspeciesGroupId: Integer;
    FSubspeciesGroupEpithet: String;
    FSubspeciesEpithet: String;
    FIncertaeSedis: Integer;
    FIocTaxonomy: Boolean;
    FIocEnglishName: String;
    FIocParentTaxonId: Integer;
    FIocRankId: Integer;
    FIocValidId: Integer;
    FIocDistribution: String;
    FIocSortNum: Double;
    FCbroTaxonomy: Boolean;
    FOtherPortugueseNames: String;
    FCbroParentTaxonId: Integer;
    FCbroRankId: Integer;
    FCbroValidId: Integer;
    FCbroSortNum: Double;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TTaxon; var aList: TStrings): Boolean;
  published
    property EnglishName: String read FEnglishName write FEnglishName;
    property PortugueseName: String read FPortugueseName write FPortugueseName;
    property SpanishName: String read FSpanishName write FSpanishName;
    property SortNum: Double read FSortNum write FSortNum;
    property QuickCode: String read FQuickCode write FQuickCode;
    property Extinct: Boolean read FExtinct write FExtinct;
    property ExtinctionYear: String read FExtinctionYear write FExtinctionYear;
    property Distribution: String read FDistribution write FDistribution;
    property EbirdCode: String read FEbirdCode write FEbirdCode;
    property ClementsTaxonomy: Boolean read FClementsTaxonomy write FClementsTaxonomy;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property GenusEpithet: String read FGenusEpithet write FGenusEpithet;
    property SpeciesEpithet: String read FSpeciesEpithet write FSpeciesEpithet;
    property SubspeciesGroupId: Integer read FSubspeciesGroupId write FSubspeciesGroupId;
    property SubspeciesGroupEpithet: String read FSubspeciesGroupEpithet write FSubspeciesGroupEpithet;
    property SubspeciesEpithet: String read FSubspeciesEpithet write FSubspeciesEpithet;
    property IncertaeSedis: Integer read FIncertaeSedis write FIncertaeSedis;
    property IocTaxonomy: Boolean read FIocTaxonomy write FIocTaxonomy;
    property IocEnglishName: String read FIocEnglishName write FIocEnglishName;
    property IocParentTaxonId: Integer read FIocParentTaxonId write FIocParentTaxonId;
    property IocRankId: Integer read FIocRankId write FIocRankId;
    property IocValidId: Integer read FIocValidId write FIocValidId;
    property IocDistribution: String read FIocDistribution write FIocDistribution;
    property IocSortNum: Double read FIocSortNum write FIocSortNum;
    property CbroTaxonomy: Boolean read FCbroTaxonomy write FCbroTaxonomy;
    property OtherPortugueseNames: String read FOtherPortugueseNames write FOtherPortugueseNames;
    property CbroParentTaxonId: Integer read FCbroParentTaxonId write FCbroParentTaxonId;
    property CbroRankId: Integer read FCbroRankId write FCbroRankId;
    property CbroValidId: Integer read FCbroValidId write FCbroValidId;
    property CbroSortNum: Double read FCbroSortNum write FCbroSortNum;
  end;

const
  TableNames: array[TTableType] of String = ('',
    'taxon_ranks',
    'zoo_taxa',
    'packages',
    'taxon_changes');
  CriteriaOperators: array[TCriteriaType] of String = ('',
    'LIKE', 'LIKE', '=', 'DISTINCT',
    'BETWEEN', '>=', '<=',
    'ISNULL', 'NOTNULL');
  SortDirections: array [TSortDirection] of String = ('', 'ASC', 'DESC');
  SearchDataTypes: array[TFilterType] of String = ('Text', 'Integer', 'Float', 'Date', 'Time',
    'DateTime', 'Boolean', 'List', 'Lookup');
  SQLAndOrStr: array [TSQLAndOr] of String = ('', 'AND', 'OR');

  { System variables }
  function InstallDir: String;
  function AppDataDir: String;
  function TempDir: String;

  function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
  function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;

  function GetLastInsertedKey(aTableType: TTableType): Integer;
  function RegistroExiste(aTabela: TTableType; aCampo, aValor: String): Boolean;

  { Taxonomies management }
  function GetRankType(aKey: Integer): TZooRank;
  function GetRankFromTaxon(aTaxonKey: Integer): Integer;

  function FormattedBirdName(aName: String; aRank: Integer; aAuthor: String = ''): String;

  procedure LoadTaxaRanks(aConnection: TSQLConnection; aList: TCheckListBox);

  procedure RewriteTaxonHierarchy;

  procedure SplitTaxon(aSubspecies: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure LumpTaxon(aSpecies, ToSpecies: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
    ExecNow: Boolean = True);

  procedure MoveToSpecies(aSubspecies, ToSpecies: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure MoveToGenus(aSpecies, ToGenus: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure MoveToFamily(aFamily: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure MoveToOrder(aOrder: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
    ExecNow: Boolean = True);

  procedure UpdateScientificName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateEnglishName(aTaxon: Integer; aNewName: String; aTaxonomy: TBirdTaxonomies;
    aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdatePortuguesName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateOutrosPortugues(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateSpanishName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateAuthorship(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateDistribution(aTaxon: Integer; aDist: String; aTaxonomy: TBirdTaxonomies;
    aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateExtinction(aTaxon: Integer; IsExtinct: Boolean; aYear: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);

  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl;
    UseValid: Boolean; var aCod: Integer; const aInit: String = ''): Boolean; overload;
  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
    aKeyField, aNameField: String; UseValid: Boolean; const aInit: String = ''): Boolean; overload;

var
  Closing: Boolean;
  Parar: Boolean;

resourcestring
  rsCaptionFind = 'Find';
  rsErrorTableNotFound = 'Table %s not found.';
  rsErrorRewritingHierarchy = 'An error occurred during hierarchy rewriting. All changes will be reverted back.';

implementation

uses
  udlg_find, udm_taxa;

{ ---------------------------------------------------------------------------------------- }
{ System variables }
{ ---------------------------------------------------------------------------------------- }

// Get the application/installation path
function InstallDir: String;
var
  s: String;
begin
  s := ExtractFilePath(Application.ExeName);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
end;

// Get the Xolmis' AppData path
function AppDataDir: String;
var
  s: String;
begin
  s := GetAppConfigDir(False);
  s := IncludeTrailingPathDelimiter(s);

  Result:= s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

// Get the Xolmis' Temp path
function TempDir: String;
var
  s: String;
begin
  s := ConcatPaths([GetTempDir(False), NomeApp]);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
var
  Qry: TSQLQuery;
begin
  if aNameValue = '' then
    Result := 0
  else
  begin
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := dmTaxa.sqlCon;
      Clear;
      Add('SELECT %keyf FROM %tabname WHERE %uniquef = :uniquev');
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      MacroByName('UNIQUEF').Value := aNameField;
      ParamByName('UNIQUEV').AsString := aNameValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
        Result := FieldByName(aKeyField).AsInteger
      else
        Result := 0;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;
var
  Qry: TSQLQuery;
begin
  if aKeyValue > 0 then
  begin
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := dmTaxa.sqlCon;
      Clear;
      Add('SELECT %uniquef FROM %tabname WHERE %keyf = :keyv');
      MacroByName('UNIQUEF').Value := aNameField;
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('KEYV').AsInteger := aKeyValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
      begin
        Result := FieldByName(aNameField).AsString;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetLastInsertedKey(aTableType: TTableType): Integer;
var
  Qry: TSQLQuery;
begin
  Result := 0;

  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := dmTaxa.sqlCon;
    Transaction := dmTaxa.sqlTrans;
    Clear;
    { SQLite }
    Add('SELECT DISTINCT last_insert_rowid() FROM %tabname');
    MacroByName('TABNAME').Value := TableNames[aTableType];
    Open;
    Result := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function RegistroExiste(aTabela: TTableType; aCampo, aValor: String): Boolean;
var
  i: Integer;
  Qry: TSQLQuery;
begin
  Result := False;
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT count(%afield) FROM %tabname WHERE %afield = :keyv');
    MacroByName('AFIELD').Value := aCampo;
    MacroByName('TABNAME').Value := TableNames[aTabela];
    ParamByName('KEYV').AsString := aValor;
    // GravaLogSQL(SQL);
    Open;
    i := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  Result := i > 0;
end;

function GetRankType(aKey: Integer): TZooRank;
var
  aRank: TRank;
  i: Integer;
begin
  Result := trDomain;
  aRank := TRank.Create(aKey);
  try
    for i := 0 to High(ZooRanks) do
      if aRank.Acronym = ZooRanks[i] then
        Result := TZooRank(i);
  finally
    FreeAndNil(aRank);
  end;
end;

function GetRankFromTaxon(aTaxonKey: Integer): Integer;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT rank_id FROM zoo_taxa WHERE taxon_id = :keyv');
    ParamByName('KEYV').AsInteger := aTaxonKey;
    // GravaLogSQL(SQL);
    Open;
    if not(IsEmpty) then
      Result := FieldByName('rank_id').AsInteger
    else
      Result := 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function FormattedBirdName(aName: String; aRank: Integer; aAuthor: String): String;
const
  colorGroup: String      = 'green';
  colorSlash: String      = 'maroon';
  //colorSp: String         = 'black';
  colorSpuh: String       = 'purple';
  colorEnglish: String    = 'teal';
  colorDomestic: String   = 'cornflowerblue'; //'$00FF870F';
  colorForm: String       = 'cadetblue'; //'$00CCA400';
  colorHybrid: String     = 'darkslateblue'; //'$00D2003F';
  colorIntergrade: String = 'goldenrod'; //'$0000D2D2';
  colorAuthorship: String = 'gray';
  Bracks: array of String = ('(', ')', '[', ']');
var
  b: Integer;
  nome, aBracket, outBrackets, Parent1, Parent2: String;
begin
  Result := EmptyStr;
  nome := EmptyStr;
  aBracket := EmptyStr;
  outBrackets := EmptyStr;
  b := 0;

  case GetRankType(aRank) of
    trDomain..trInfratribe:
      begin
        nome := aName;
      end;
    trSupergenus..trSubspecies:
      begin
        nome := '<i>' + aName + '</i>';
      end;
    trMonotypicGroup:
      begin
        nome := Format('<i>%s %s <font color="%s">%s</font></i>', [ExtractWord(1, aName, [' ']),
          ExtractWord(2, aName, [' ']), colorGroup, ExtractWord(3, aName, [' '])]);
      end;
    trPolitypicGroup:
      begin
        if (Pos('/', aName) > 0) then
          nome := Format('<i>%s %s <font color="%s">%s</font></i>', [ExtractWord(1, aName, [' ']),
            ExtractWord(2, aName, [' ']), colorGroup, ExtractWord(3, aName, [' '])])
        else
        if (Pos('[', aName) > 0) then
        begin
          aBracket := Trim(ExtractDelimited(2, aName, ['[',']']));
          nome := Format('<i>%s %s</i> <font color="%s">[<i>%s</i> %s]</font>',
            [ExtractWord(1, aName, [' ']), ExtractWord(2, aName, [' ']), colorGroup,
            ExtractWord(1, aBracket, [' ']), ExtractWord(2, aBracket, [' '])]);
        end;
      end;
    trSpuh:
      begin
        if (Pos('(', aName) > 0) then
        begin
          outBrackets := Trim(ExtractDelimited(1, aName, Brackets));
          aBracket := Trim(ExtractDelimited(2, aName, Brackets));

          if IsWordPresent('Domestic', aBracket, [' ']) then
            aBracket := Format('<font color="%s">(%s)</font>', [colorDomestic, aBracket])
          else
          if ExecRegExpr('^[a-z].+ complex$', aBracket) then
            aBracket := Format('<font color="%s">(<i>%s</i> complex)</font>', [colorEnglish,
              ExtractWord(1, aBracket, [' '])])
          else
          if ExecRegExpr('^former .+ sp.$', aBracket) then
            aBracket := Format('<font color="%s">(former <i>%s</i> sp.)</font>', [colorEnglish,
              ExtractWord(2, aBracket, [' '])])
          else
            aBracket := Format('<font color="%s">(%s)</font>', [colorEnglish, aBracket])
        end
        else
          outBrackets := aName;

        if (Pos('/', outBrackets) > 0) then
        begin
          if not IsWordPresent('sp.', outBrackets, [' ']) then
          begin
            nome := Format('<font color="%s"><i>%s</i></font>', [colorSpuh, outBrackets]);
          end
          else
          begin
            if ExecRegExpr('.+(formes|idae|inae).*', outBrackets) then
            begin
              nome := Format('<font color="%s">%s</font> <b>sp.</b>', [colorSpuh,
                ExtractWord(1, outBrackets, [' '])]);
            end
            else
            if IsWordPresent('eagle', outBrackets, ['/', ' ']) then
            begin
              nome := Format('<font color="%s"><i>%s</i>/%s</font> <b>sp.</b>', [colorSpuh,
                ExtractWord(1, outBrackets, ['/', ' ']), ExtractWord(2, outBrackets, ['/', ' '])]);
            end
            else
            begin
              nome := Format('<font color="%s"><i>%s</i></font> <b>sp.</b>',
                [colorSpuh, ExtractWord(1, outBrackets, [' '])]);
            end;
          end;
        end
        else
        begin
          if ExecRegExpr('.+(formes|idae|inae).*', outBrackets) then
          begin
            nome := Format('<font color="%s">%s</font> <b>sp.</b>', [colorSpuh,
              ExtractWord(1, outBrackets, [' '])]);
          end
          else
          begin
            nome := Format('<font color="%s"><i>%s</i></font> <b>sp.</b>',
              [colorSpuh, ExtractWord(1, outBrackets, [' '])]);
          end;
        end;

        if (aBracket <> EmptyStr) then
          nome := nome + ' ' + aBracket;
      end;
    trSlash:
      begin
        if (Pos('(', aName) > 0) then
        begin
          outBrackets := Trim(ExtractDelimited(1, aName, Brackets));
          aBracket := Format('<font color="%s">(%s)</font>', [colorEnglish,
            Trim(ExtractDelimited(2, aName, Brackets))]);
        end
        else
          outBrackets := aName;

        if (Pos('sp.', outBrackets) > 0) then
        begin
          if ExecRegExpr('.+ sp.\/.+', outBrackets) then
          begin
            nome := Format('<font color="%s"><i>%s</i></font> <b>sp.</b><font color="%s">/<i>%s</i></font>',
              [colorSlash, ExtractWord(1, outBrackets, [' ']), colorSlash,
              ExtractWord(2, outBrackets, ['/'])]);
          end
          else
          begin
            outBrackets := StringReplace(outBrackets, ' sp.', '', []);
            nome := Format('<font color="%s"><i>%s</i></font> <b>sp.</b>',
              [colorSlash, outBrackets]);
          end;
        end
        else
        if ExecRegExpr('.+\/[A-Z].+', outBrackets) then
          nome := Format('<font color="%s"><i>%s</i></font>',
              [colorSlash, outBrackets])
        else
        if (WordCount(outBrackets, [' ']) = 2) then
          nome := Format('<i>%s <font color="%s">%s</font></i>',
              [ExtractWord(1, outBrackets, [' ']), colorSlash, ExtractWord(2, outBrackets, [' '])]);

        if (aBracket <> EmptyStr) then
          nome := nome + ' ' + aBracket;
      end;
    trHybrid:
      begin
        if (Pos(' x ', aName) > 0) then
        begin
          aName := StringReplace(aName, ' x ', ' | ', [rfReplaceAll]);
          Parent1 := Trim(ExtractDelimited(1, aName, ['|']));
          Parent2 := Trim(ExtractDelimited(2, aName, ['|']));
        end
        else
        begin
          Parent1 := aName;
          Parent2 := EmptyStr;
        end;

        if (Pos('(', Parent1) > 0) then
        begin
          if IsWordPresent('Domestic', Parent1, [' '] + Brackets) then
            aBracket := Format('<font color="%s">(%s)</font>', [colorDomestic,
              Trim(ExtractDelimited(2, Parent1, Brackets))])
          else
          if IsWordPresent('hybrid', Parent1, [' '] + Brackets) then
            aBracket := Format('<font color="%s">(%s)</font>', [colorHybrid,
              Trim(ExtractDelimited(2, Parent1, Brackets))]);
          Parent1 := Trim(ExtractDelimited(1, Parent1, Brackets));
        end;

        if (Pos('sp.', Parent1) > 0) then
          Parent1 := Format('<i>%s</i> <b>sp.</b>', [Trim(ExtractDelimited(1, Parent1, [' ']))])
        else
          Parent1 := Format('<i>%s</i>', [Parent1]);

        if (aBracket <> EmptyStr) then
          Parent1 := Parent1 + ' ' + aBracket;

        aBracket := EmptyStr;
        if (Parent2 <> EmptyStr) then
        begin
          if (Pos('(', Parent2) > 0) then
          begin
            if IsWordPresent('Domestic', Parent2, [' '] + Brackets) then
              aBracket := Format('<font color="%s">(%s)</font>', [colorDomestic,
                Trim(ExtractDelimited(2, Parent2, Brackets))])
            else
            if IsWordPresent('hybrid', Parent2, [' '] + Brackets) then
              aBracket := Format('<font color="%s">(%s)</font>', [colorHybrid,
                Trim(ExtractDelimited(2, Parent2, Brackets))])
            else
              aBracket := Format('<font color="%s">(%s)</font>', [colorEnglish,
                Trim(ExtractDelimited(2, Parent2, Brackets))]);
            Parent2 := Trim(ExtractDelimited(1, Parent2, Brackets));
          end;

          if (Pos('sp.', Parent2) > 0) then
          begin
            if ExecRegExpr('.+(formes|idae|inae)', ExtractDelimited(1, Parent2, [' '])) then
              Parent2 := Format('%s <b>sp.</b>', [Trim(ExtractDelimited(1, Parent2, [' ']))])
            else
              Parent2 := Format('<i>%s</i> <b>sp.</b>', [Trim(ExtractDelimited(1, Parent2, [' ']))]);
          end
          else
            Parent2 := Format('<i>%s</i>', [Parent2]);

          if (aBracket <> EmptyStr) then
            Parent2 := Parent2 + ' ' + aBracket;
        end;

        if (Parent2 <> EmptyStr) then
          nome := Format('%s <font color="%s"><b>×</b></font> %s', [Parent1, colorHybrid, Parent2])
        else
          nome := Parent1;
      end;
    trIntergrade:
      begin
        aBracket := Trim(ExtractDelimited(2, aName, Brackets));
        if (aBracket <> EmptyStr) then
        begin
          if (Pos(' x ', aBracket) > 0) then
            aBracket := Format('<font color="%s">[<i>%s</i> Group <font color="%s"><b>×</b></font> <i>%s</i> Group]</font>',
              [colorGroup, ExtractWord(1, aBracket, [' ']), colorIntergrade, ExtractWord(4, aBracket, [' '])])
          else
          if IsWordPresent('intergrade', aBracket, [' ']) then
            aBracket := Format('<font color="%s">(<i>%s</i> intergrade)</font>', [colorIntergrade,
              ExtractWord(1, aBracket, [' '])])
          else
          if IsWordPresent('Group', aBracket, [' ']) then
            aBracket := Format('<font color="%s">[<i>%s</i> Group]</font>', [colorGroup,
              ExtractWord(1, aBracket, [' '])]);
        end;

        if (Pos(' x ', aName) = 0) then
        begin
          nome := Format('<i>%s</i> %s', [Trim(ExtractDelimited(1, aName, Brackets)), aBracket]);
        end
        else
        begin
          if ExecRegExpr('.+ \[.+ x .+\]', aName) then
          begin
            nome := Format('<i>%s</i> %s', [Trim(ExtractDelimited(1, aName, Brackets)), aBracket]);
          end
          else
          begin
            if (Pos(' x ', aName) > 0) then
            begin
              aName := StringReplace(aName, ' x ', ' | ', [rfReplaceAll]);
              Parent1 := Trim(ExtractDelimited(1, aName, ['|']));
              Parent2 := Trim(ExtractDelimited(2, aName, ['|']));
            end
            else
            begin
              Parent1 := aName;
              Parent2 := EmptyStr;
            end;

            if (Pos(']', Parent1) > 0) then
              Parent1 := Format('<i>%s</i> %s', [Trim(ExtractDelimited(1, Parent1, Brackets)), aBracket])
            else
            if (Pos('/', Parent1) > 0) then
              Parent1 := Format('<i>%s %s <font color="%s">%s</font></i>', [ExtractWord(1, Parent1, [' ']),
                ExtractWord(2, Parent1, [' ']), colorGroup, ExtractWord(3, Parent1, [' '])])
            else
              Parent1 := Format('<i>%s</i>', [Parent1]);

            if (Pos('[', Parent2) > 0) then
              Parent2 := aBracket
            else
            if (Pos('/', Parent2) > 0) then
              Parent2 := Format('<font color="%s"><i>%s</i></font>', [colorGroup, Parent2])
            else
              Parent2 := Format('<i>%s</i>', [Parent2]);

            nome := Format('%s <font color="%s"><b>×</b></font> %s', [Parent1, colorIntergrade,
              Parent2]);
          end;
        end;
      end;
    trForm:
      begin
        if (Pos('(', aName) > 0) or (Pos('[', aName) > 0) then
        begin
          if (Pos('(', aName) > 0) then
            b := 0
          else
          if (Pos('[', aName) > 0) then
            b := 2;

          if ExecRegExpr('.+(formes|idae|inae)', ExtractDelimited(1, aName, Brackets)) then
            nome := Format('%s <font color="%s">%s%s%s</font>', [Trim(ExtractDelimited(1, aName, Brackets)),
              colorForm, Bracks[b], Trim(ExtractDelimited(2, aName, Brackets)), Bracks[b + 1]])
          else
            nome := Format('<i>%s</i> <font color="%s">%s%s%s</font>', [Trim(ExtractDelimited(1, aName, Brackets)),
              colorForm, Bracks[b], Trim(ExtractDelimited(2, aName, Brackets)), Bracks[b + 1]]);
        end
        else
        if (WordCount(aName, [' ']) = 3) then
          nome := Format('<i>%s %s <font color="%s">%s</font></i>', [ExtractWord(1, aName, [' ']),
            ExtractWord(2, aName, [' ']), colorForm, ExtractWord(3, aName, [' '])]);
      end;
    trDomestic:
      begin
        if (Pos('(', aName) > 0) then
          nome := Format('<i>%s</i> <font color="%s">(%s)</font>', [Trim(ExtractDelimited(1, aName, Brackets)),
            colorDomestic, Trim(ExtractDelimited(2, aName, Brackets))]);
      end;
  end;
  { Authorship }
  if aAuthor <> EmptyStr then
    nome := Format('%s <font color="%s">%s</font>', [nome, colorAuthorship, aAuthor]);

  Result := nome;
end;

procedure LoadTaxaRanks(aConnection: TSQLConnection; aList: TCheckListBox);
var
  nome: String;
  // i: Integer;
  Qry: TSQLQuery;
  Lista: TStrings;
  // STree: TMemoryStream;
begin
  Lista := TStringList.Create;
  // STree:= TMemoryStream.Create;
  // STree.Position:= 0;
  Qry := TSQLQuery.Create(aConnection);
  Qry.Database := aConnection;
  with Qry do
  try
    SQL.Clear;

    SQL.Add('SELECT DISTINCT t.rank_id,');
    SQL.Add('(SELECT l.rank_name FROM taxon_ranks AS l WHERE l.rank_id = t.rank_id) AS rank_name,');
    SQL.Add('(SELECT n.rank_seq FROM taxon_ranks AS n WHERE n.rank_id = t.rank_id) AS sort_num');
    SQL.Add('FROM zoo_taxa AS t WHERE (t.rank_id > 0) AND (t.active_status = 1)');
    SQL.Add('ORDER BY sort_num ASC');

    Open;
    if RecordCount > 0 then
    begin
      // PBar.Max:= RecordCount;
      // PBar.Position:= 0;
      // PBar.Visible:= True;
      aList.Items.BeginUpdate;
      aList.Items.Clear;
      nome := '';

      // Cria lista para arvore
      First;
      repeat
        nome := FieldByName('rank_name').AsString;
        Lista.Add(nome);

        // PBar.Position:= RecNo;
        Next;
      until EOF;
      // Lista.SaveToFile('TaxonTree.txt');
      // Lista.SaveToStream(STree);
      // STree.SaveToFile('TaxonTreeStream.txt');
      // STree.Position:= 0;
      aList.Items.Assign(Lista);
      aList.Items.EndUpdate;
      // PBar.Visible:= False;
      // PBar.Position:= 0;
    end;
    Close;
  finally
    FreeAndNil(Qry);
    Lista.Free;
    // STree.Free;
  end;
end;

procedure RewriteTaxonHierarchy;
var
  Qry: TSQLQuery;
  iOrder, iFamily, iSubfamily, iGenus, iSpecies, iMonoGroup, iPoliGroup, iSubspecies: Integer;
begin
  //dlgProgress := TdlgProgress.Create(nil);
  //try
    //dlgProgress.lblTitle.Caption := rsTitleTaxonHierarchy;
    //dlgProgress.lStatus.Caption := rsProgressPreparing;
    //dlgProgress.PBar.Style := pbstMarquee;
    //dlgProgress.Show;
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      DataBase := dmTaxa.sqlCon;
      Transaction := dmTaxa.sqlTrans;
      MacroCheck := True;
      //dlgProgress.PBar.Position := 0;

      iOrder := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ord.');
      iFamily := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'fam.');
      iSubfamily := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'subfam.');
      iGenus := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'g.');
      iSpecies := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.');
      iMonoGroup := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (mono)');
      iPoliGroup := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (poli)');
      iSubspecies := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');

      dmTaxa.sqlTrans.StartTransaction;
      try
        { Order }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionOrder)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET order_id = taxon_id');
        Add('WHERE zoo_taxa.rank_id = :rank_id');
        ParamByName('RANK_ID').AsInteger := iOrder;
        ExecSQL;
        Application.ProcessMessages;

        { Family }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionFamily)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET family_id = zoo_taxa.taxon_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iFamily;
        ExecSQL;
        Application.ProcessMessages;

        { Subfamily }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionSubfamily)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subfamily_id = zoo_taxa.taxon_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSubfamily;
        ExecSQL;
        Application.ProcessMessages;

        { Genus }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionGenus)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET genus_id = zoo_taxa.taxon_id, subfamily_id = parent.subfamily_id, ');
        Add('family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iGenus;
        ExecSQL;
        Application.ProcessMessages;

        { Species }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionSpecies)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET species_id = zoo_taxa.taxon_id, genus_id = parent.genus_id, ');
        Add('subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSpecies;
        ExecSQL;
        Application.ProcessMessages;

        { Mono and politypic groups }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionSspGroup)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subspecies_group_id = zoo_taxa.taxon_id, species_id = parent.species_id, genus_id = parent.genus_id, ');
        Add('subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iMonoGroup;
        ExecSQL;
        Application.ProcessMessages;
        ParamByName('RANK_ID').AsInteger := iPoliGroup;
        ExecSQL;
        Application.ProcessMessages;

        { Subspecies, domestic, form }
        //dlgProgress.lStatus.Caption := Format(rsProgressRewritingHierarchy,
        //    [AnsiLowerCase(rsCaptionSubspecificTaxa)]);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subspecies_group_id = parent.subspecies_group_id, species_id = parent.species_id, ' +
          'genus_id = parent.genus_id, subfamily_id = parent.subfamily_id, family_id = parent.family_id, ' +
          'order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id, ' +
          'subspecies_group_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSubspecies;
        ExecSQL;
        Application.ProcessMessages;

        //dlgProgress.lStatus.Caption := rsProgressFinishing;
        dmTaxa.sqlTrans.Commit;
      except
        dmTaxa.sqlTrans.Rollback;
        raise Exception.Create(rsErrorRewritingHierarchy);
      end;
    finally
      FreeAndNil(Qry);
    end;
  //finally
  //  dlgProgress.Close;
  //  FreeAndNil(dlgProgress);
  //end;
end;

procedure SplitTaxon(aSubspecies: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery; ExecNow: Boolean);
var
  OldName, NewName: String;
  SpRank, ParentGenus, ValidSp, ExistingId: Integer;
  Ssp: TTaxon;
begin
  ExistingId := 0;
  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSubspecies);
  NewName := ExtractWord(1, OldName, [' ']) + ' ' + ExtractWord(3, OldName, [' ']);
  SpRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.');
  ParentGenus := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(1, OldName, [' ']));
  Ssp := TTaxon.Create(aSubspecies);

  try
    // If taxon exists
    if RegistroExiste(tbZooTaxa, 'full_name', NewName) = True then
    begin
      ExistingId := GetKey('zoo_taxa', 'taxon_id', 'full_name', NewName);
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;

        SQL.Add('UPDATE zoo_taxa SET ');
        if (btClements in aTaxonomy) then { Clementes/eBird }
        begin
          SQL.Add('clements_taxonomy = 1,');
          SQL.Add('valid_id = null,');
          // SQL.Add('TAX_ENGLISH = '+QuotedStr(Ssp.NomeEnglish)+',');
          SQL.Add('distribution = :geodist,');
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
        end;
        if (btIOC in aTaxonomy) then { IOC }
        begin
          SQL.Add('ioc_taxonomy = 1,');
          SQL.Add('ioc_rank_id = :nivel_ioc,');
          SQL.Add('ioc_parent_raxon_id = :sup_ioc,');
          SQL.Add('ioc_distribution = :geodist_ioc,');
          ParamByName('NIVEL_IOC').AsInteger := SpRank;
          ParamByName('SUP_IOC').AsInteger := ParentGenus;
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.IocDistribution;
        end;
        if (btCBRO in aTaxonomy) then { CBRO }
        begin
          SQL.Add('cbro_taxonomy = 1,');
          SQL.Add('cbro_rank_id = :nivel_cbro,');
          SQL.Add('cbro_parent_taxon_id = :sup_cbro,');
          ParamByName('NIVEL_CBRO').AsInteger := SpRank;
          ParamByName('SUP_CBRO').AsInteger := ParentGenus;
        end;
        SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
        SQL.Add('WHERE taxon_id = :ataxon;');
        ParamByName('AUSER').AsInteger := 1;
        ParamByName('ATAXON').AsInteger := ExistingId;

        if ExecNow then
          ExecSQL;
      end;
    end
    else

    // If taxon does not exist
    begin
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        // List fields
        SQL.Add('INSERT INTO zoo_taxa (full_name, formatted_name, authorship, english_name,');
        SQL.Add('rank_id, parent_taxon_id, extinct, extinction_year, species_id, genus_id,');
        SQL.Add('subfamily_id, family_id, order_id, subspecies_group_id, genus_name, species_epithet,');
        if (btClements in aTaxonomy) then
          SQL.Add('clements_taxonomy, distribution, ebird_code,');
        if (btIOC in aTaxonomy) then
          SQL.Add('ioc_taxonomy, ioc_rank_id, ioc_parent_taxon_id, ioc_english_name, ioc_distribution,');
        if (btCBRO in aTaxonomy) then
          SQL.Add('cbro_taxonomy, cbro_rank_id, cbro_parent_taxon_id,');
        SQL.Add('insert_date, user_inserted) ');
        // List values
        SQL.Add('VALUES (:aname, :aformattedname, :autoria, :aenglish, :anivel, :asup,');
        SQL.Add(':aextinto, :anoextinto, :aspecies, :agenus, :asubfamily, :afamily, :aorder, 0,');
        SQL.Add(':agenusname, :aepithet,');
        if (btClements in aTaxonomy) then
        begin
          SQL.Add('1, :geodist, :aebirdcode,');
        end;
        if (btIOC in aTaxonomy) then
        begin
          SQL.Add('1, :anivelioc, :asupioc, :aenglishioc, :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then
        begin
          SQL.Add('1, :anivelcbro, :asupcbro,');
        end;
        SQL.Add('datetime(''now'',''localtime''), :auser);');
        ParamByName('ANAME').AsString := NewName;
        ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, SpRank);
        ParamByName('AUTORIA').AsString := Ssp.Authorship;
        ParamByName('AENGLISH').AsString := Ssp.EnglishName;
        ParamByName('ANIVEL').AsInteger := SpRank;
        ParamByName('ASUP').AsInteger := ParentGenus;
        ParamByName('AEXTINTO').AsInteger := Integer(Ssp.Extinct);
        ParamByName('ANOEXTINTO').AsString := Ssp.ExtinctionYear;
        ParamByName('ASPECIES').AsInteger := GetLastInsertedKey(tbZooTaxa) + 1;
        ParamByName('AGENUS').AsInteger := ParentGenus;
        ParamByName('ASUBFAMILY').AsInteger := Ssp.SubfamilyId;
        ParamByName('AFAMILY').AsInteger := Ssp.FamilyId;
        ParamByName('AORDER').AsInteger := Ssp.OrderId;
        ParamByName('AGENUSNAME').AsString := ExtractWord(1, OldName, [' ']);
        ParamByName('AEPITHET').AsString := ExtractWord(3, OldName, [' ']);
        if (btClements in aTaxonomy) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
          ParamByName('AEBIRDCODE').AsString := Ssp.EbirdCode;
        end;
        if (btIOC in aTaxonomy) then
        begin
          ParamByName('ANIVELIOC').AsInteger := SpRank;
          ParamByName('ASUPIOC').AsInteger := ParentGenus;
          ParamByName('AENGLISHIOC').AsString := Ssp.IocEnglishName;
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        if (btCBRO in aTaxonomy) then
        begin
          ParamByName('ANIVELCBRO').AsInteger := SpRank;
          ParamByName('ASUPCBRO').AsInteger := ParentGenus;
        end;
        ParamByName('AUSER').AsInteger := AdminId;

        if ExecNow then
          ExecSQL;
      end;
    end;

    // Update subspecies
    if ExistingId > 0 then
      ValidSp := ExistingId
    else
      ValidSp := GetLastInsertedKey(tbZooTaxa);
    with aDataset do
    begin
      if ExecNow then
        SQL.Clear;
      SQL.Add('UPDATE zoo_taxa SET ');
      if (btClements in aTaxonomy) then { Clementes/eBird }
      begin
        SQL.Add('clements_taxonomy = 0,');
        SQL.Add('valid_id = :aval,');
      end;
      if (btIOC in aTaxonomy) then { IOC }
      begin
        SQL.Add('ioc_taxonomy = 0,');
        SQL.Add('ioc_rank_id = :arank,');
        SQL.Add('ioc_valid_id = :aval,');
      end;
      if (btCBRO in aTaxonomy) then { CBRO }
      begin
        SQL.Add('cbro_taxonomy = 0,');
        SQL.Add('cbro_rank_id = :arank,');
        SQL.Add('cbro_valid_id = :aval,');
      end;
      SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
      SQL.Add('WHERE taxon_id = :ataxon;');
      if (Params.FindParam('ARANK') <> nil) then
      begin
        ParamByName('ARANK').AsInteger := SpRank;
      end;
      ParamByName('AVAL').AsInteger := ValidSp;
      ParamByName('AUSER').AsInteger := AdminId;
      ParamByName('ATAXON').AsInteger := aSubspecies;
      if ExecNow then
        ExecSQL;
    end;

  finally
    FreeAndNil(Ssp);
  end;
end;

procedure LumpTaxon(aSpecies, ToSpecies: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery; ExecNow: Boolean
  );
var
  OldName, LumpToName, NewName: String;
  SspRank, ParentSp, ValidSp, ExistingId: Integer;
  Ssp, LumpToSp: TTaxon;
begin
  ExistingId := 0;
  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSpecies);
  LumpToName := GetName('zoo_taxa', 'full_name', 'taxon_id', ToSpecies);
  NewName := LumpToName + ' ' + ExtractWord(2, OldName, [' ']);
  SspRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');
  ParentSp := ToSpecies;
  Ssp := TTaxon.Create(aSpecies);
  LumpToSp := TTaxon.Create(ToSpecies);

  try
    // If taxon exists
    if RegistroExiste(tbZooTaxa, 'full_name', NewName) = True then
    begin
      ExistingId := GetKey('zoo_taxa', 'taxon_id', 'full_name', NewName);
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        SQL.Add('UPDATE zoo_taxa SET ');
        if (btClements in aTaxonomy) then { Clementes/eBird }
        begin
          SQL.Add('clements_taxonomy = 1,');
          SQL.Add('valid_id = null,');
          SQL.Add('distribution = :geodist,');
        end;
        if (btIOC in aTaxonomy) then { IOC }
        begin
          SQL.Add('ioc_taxonomy = 1,');
          SQL.Add('ioc_rank_id = :arank,');
          SQL.Add('ioc_parent_taxon_id = :asup,');
          SQL.Add('ioc_distribution = :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then { CBRO }
        begin
          SQL.Add('cbro_taxonomy = 1,');
          SQL.Add('cbro_rank_id = :arank,');
          SQL.Add('cbro_parent_taxon_id = :asup,');
        end;
        SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
        SQL.Add('WHERE taxon_id = :ataxon;');
        if (Params.FindParam('ARANK') <> nil) then
        begin
          ParamByName('ARANK').AsInteger := SspRank;
        end;
        if (Params.FindParam('ASUP') <> nil) then
        begin
          ParamByName('ASUP').AsInteger := ParentSp;
        end;
        if (Params.FindParam('GEODIST') <> nil) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
        end;
        if (Params.FindParam('GEODISTIOC') <> nil) then
        begin
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        ParamByName('AUSER').AsInteger := AdminId;
        ParamByName('ATAXON').AsInteger := ExistingId;
        if ExecNow then
          ExecSQL;
      end;
    end
    else

    // If taxon does not exist
    begin
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        // List fields
        SQL.Add('INSERT INTO zoo_taxa (full_name, formatted_name, authorship, english_name,');
        SQL.Add('rank_id, parent_taxon_id, extinct, extinction_year, species_id, genus_id,');
        SQL.Add('subfamily_id, family_id, order_id, subspecies_group_id, genus_name, species_epithet,');
        if (btClements in aTaxonomy) then
          SQL.Add('clements_taxonomy, distribution, ebird_code,');
        if (btIOC in aTaxonomy) then
          SQL.Add('ioc_taxonomy, ioc_rank_id, ioc_parent_taxon_id, ioc_english_name, ioc_distribution,');
        if (btCBRO in aTaxonomy) then
          SQL.Add('cbro_taxonomy, cbro_rank_id, cbro_parent_taxon_id,');
        SQL.Add('insert_date, user_inserted) ');
        // List values
        SQL.Add('VALUES (:aname, :aformattedname, :autoria, :aenglish, :anivel, :asup,');
        SQL.Add(':aextinto, :anoextinto, :aspecies, :agenus, :asubfamily, :afamily, :aorder, 0,');
        SQL.Add(':agenusname, :aepithet,');
        if (btClements in aTaxonomy) then
        begin
          SQL.Add('1, :geodist, :aebirdcode,');
        end;
        if (btIOC in aTaxonomy) then
        begin
          SQL.Add('1, :anivelioc, :asupioc, :aenglishioc, :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then
        begin
          SQL.Add('1, :anivelcbro, :asupcbro,');
        end;
        SQL.Add('datetime(''now'',''localtime''), :auser);');
        ParamByName('ANAME').AsString := NewName;
        ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, SspRank);
        ParamByName('AUTORIA').AsString := Ssp.Authorship;
        ParamByName('AENGLISH').AsString := Ssp.EnglishName;
        ParamByName('ANIVEL').AsInteger := SspRank;
        ParamByName('ASUP').AsInteger := ParentSp;
        ParamByName('AEXTINTO').AsInteger := Integer(Ssp.Extinct);
        ParamByName('ANOEXTINTO').AsString := Ssp.ExtinctionYear;
        ParamByName('ASPECIES').AsInteger := LumpToSp.SpeciesId;
        ParamByName('AGENUS').AsInteger := LumpToSp.GenusId;
        ParamByName('ASUBFAMILY').AsInteger := LumpToSp.SubfamilyId;
        ParamByName('AFAMILY').AsInteger := LumpToSp.FamilyId;
        ParamByName('AORDER').AsInteger := LumpToSp.OrderId;
        ParamByName('AGENUSNAME').AsString := ExtractWord(1, NewName, [' ']);
        ParamByName('AEPITHET').AsString := ExtractWord(2, NewName, [' ']);
        if (btClements in aTaxonomy) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
          ParamByName('AEBIRDCODE').AsString := Ssp.EbirdCode;
        end;
        if (btIOC in aTaxonomy) then
        begin
          ParamByName('ANIVELIOC').AsInteger := SspRank;
          ParamByName('ASUPIOC').AsInteger := ParentSp;
          ParamByName('AENGLISHIOC').AsString := Ssp.IocEnglishName;
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        if (btCBRO in aTaxonomy) then
        begin
          ParamByName('ANIVELCBRO').AsInteger := SspRank;
          ParamByName('ASUPCBRO').AsInteger := ParentSp;
        end;
        ParamByName('AUSER').AsInteger := AdminId;

        if ExecNow then
          ExecSQL;
      end;
    end;

    // Update subspecies
    if ExistingId > 0 then
      ValidSp := ExistingId
    else
      ValidSp := GetLastInsertedKey(tbZooTaxa);
    with aDataset do
    begin
      if ExecNow then
        SQL.Clear;
      SQL.Add('UPDATE zoo_taxa SET ');
      if (btClements in aTaxonomy) then { Clementes/eBird }
      begin
        SQL.Add('clements_taxonomy = 0,');
        SQL.Add('valid_id = :avalid,');
      end;
      if (btIOC in aTaxonomy) then { IOC }
      begin
        SQL.Add('ioc_taxonomy = 0,');
        SQL.Add('ioc_rank_id = :arank,');
        SQL.Add('ioc_valid_id = :avalid,');
      end;
      if (btCBRO in aTaxonomy) then { CBRO }
      begin
        SQL.Add('cbro_taxonomy = 0,');
        SQL.Add('cbro_rank_id = :arank,');
        SQL.Add('cbro_valid_id = :avalid,');
      end;
      SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
      SQL.Add('WHERE taxon_id = :ataxon;');
      if (Params.FindParam('ARANK') <> nil) then
      begin
        ParamByName('ARANK').AsInteger := SspRank;
      end;
      if (Params.FindParam('AVALID') <> nil) then
      begin
        ParamByName('AVALID').AsInteger := ValidSp;
      end;
      ParamByName('AUSER').AsInteger := AdminId;
      ParamByName('ATAXON').AsInteger := aSpecies;

      if ExecNow then
        ExecSQL;
    end;

  finally
    FreeAndNil(Ssp);
    FreeAndNil(LumpToSp);
  end;
end;

procedure MoveToSpecies(aSubspecies, ToSpecies: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
  ExecNow: Boolean);
var
  OldName, MoveToName, NewName: String;
  OldRankId, ParentSp, ValidSsp, ExistingId: Integer;
  Ssp, MoveToSp: TTaxon;
  OldRank: TZooRank;
begin
  ExistingId := 0;
  OldRankId := GetRankFromTaxon(aSubspecies);
  OldRank := GetRankType(OldRankId);
  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSubspecies);
  MoveToName := GetName('zoo_taxa', 'full_name', 'taxon_id', ToSpecies);
  if OldRank = trPolitypicGroup then
    NewName := MoveToName + ' ' + Trim(ExtractWord(2, OldName, Brackets))
  else
    NewName := MoveToName + ' ' + ExtractWord(3, OldName, [' ']);
  ParentSp := ToSpecies;
  Ssp := TTaxon.Create(aSubspecies);
  MoveToSp := TTaxon.Create(ToSpecies);
  //GravaLog('MOVE TO SPECIES', OldName + ' -> ' + MoveToName + ' = ' + NewName);

  try
    // If taxon exists
    if RegistroExiste(tbZooTaxa, 'full_name', NewName) = True then
    begin
      ExistingId := GetKey('zoo_taxa', 'taxon_id', 'full_name', NewName);
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        SQL.Add('UPDATE zoo_taxa SET ');
        if (btClements in aTaxonomy) then { Clementes/eBird }
        begin
          SQL.Add('clements_taxonomy = 1,');
          case OldRank of
            trMonotypicGroup:
              begin
                SQL.Add('rank_id = :arankmono,');
                SQL.Add('formatted_name = :aformattedmono,');
              end;
            trPolitypicGroup:
              begin
                SQL.Add('rank_id = :arankpoli,');
                SQL.Add('formatted_name = :aformattedpoli,');
              end;
          else
            SQL.Add('rank_id = :arank,');
            SQL.Add('formatted_name = :aformattedname,');
          end;
          SQL.Add('valid_id = null,');
          // SQL.Add('TAX_ENGLISH = '+QuotedStr(Ssp.NomeEnglish)+',');
          SQL.Add('distribution = :geodist,');
        end;
        if (btIOC in aTaxonomy) then { IOC }
        begin
          SQL.Add('ioc_taxonomy = 1,');
          SQL.Add('ioc_rank_id = :arank,');
          SQL.Add('ioc_parent_taxon_id = :asup,');
          SQL.Add('ioc_distribution = :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then { CBRO }
        begin
          SQL.Add('cbro_taxonomy = 1,');
          SQL.Add('cbro_rank_id = :arank,');
          SQL.Add('cbro_parent_taxon_id = :asup,');
        end;
        SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
        SQL.Add('WHERE taxon_id = :ataxon;');
        if (Params.FindParam('ASUP') <> nil) then
        begin
          ParamByName('ASUP').AsInteger := ParentSp;
        end;
        if (Params.FindParam('GEODIST') <> nil) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
        end;
        if (Params.FindParam('GEODISTIOC') <> nil) then
        begin
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        case OldRank of
          trMonotypicGroup:
            begin
              ParamByName('ARANKMONO').AsInteger := OldRankId;
              ParamByName('AFORMATTEDMONO').AsString := FormattedBirdName(NewName, OldRankId);
            end;
          trPolitypicGroup:
            begin
              ParamByName('ARANKPOLI').AsInteger := OldRankId;
              ParamByName('AFORMATTEDPOLI').AsString := FormattedBirdName(NewName, OldRankId);
            end;
        else
          ParamByName('ARANK').AsInteger := Ssp.RankId;
          ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, Ssp.RankId);
        end;
        ParamByName('AUSER').AsInteger := AdminId;
        ParamByName('ATAXON').AsInteger := ExistingId;

        if ExecNow then
          ExecSQL;
      end;
    end
    else

    // If taxon does not exist
    begin
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        // List fields
        SQL.Add('INSERT INTO zoo_taxa (full_name, formatted_name, authorship, english_name,');
        SQL.Add('rank_id, parent_taxon_id, extinct, extinction_year, species_id, genus_id, subfamily_id,');
        SQL.Add('family_id, order_id, subspecies_group_id, genus_name, species_epithet, subspecies_epithet,');
        if (btClements in aTaxonomy) then
          SQL.Add('clements_taxonomy, distribution, ebird_code,');
        if (btIOC in aTaxonomy) then
          SQL.Add('ioc_taxonomy, ioc_rank_id, ioc_parent_taxon_id, ioc_english_name, ioc_distribution,');
        if (btCBRO in aTaxonomy) then
          SQL.Add('cbro_taxonomy, cbro_rank_id, cbro_parent_taxon_id,');
        SQL.Add('insert_date, user_inserted) ');
        // List values
        SQL.Add('VALUES (:aname, :aformattedname, :autoria, :aenglish, :anivel, :asup,');
        SQL.Add(':aextinto, :anoextinto, :aspecies, :agenus, :asubfamily, :afamily, :aorder, 0,');
        SQL.Add(':agenusname, :aepithet, :asspepithet,');
        if (btClements in aTaxonomy) then
        begin
          SQL.Add('1, :geodist, :aebirdcode,');
        end;
        if (btIOC in aTaxonomy) then
        begin
          SQL.Add('1, :anivelioc, :asupioc, :aenglishioc, :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then
        begin
          SQL.Add('1, :anivelcbro, :asupcbro,');
        end;
        SQL.Add('datetime(''now'',''localtime''), :auser);');
        ParamByName('ANAME').AsString := NewName;
        ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, OldRankId);
        ParamByName('AUTORIA').AsString := Ssp.Authorship;
        ParamByName('AENGLISH').AsString := Ssp.EnglishName;
        ParamByName('ANIVEL').AsInteger := OldRankId;
        ParamByName('ASUP').AsInteger := ParentSp;
        ParamByName('AEXTINTO').AsInteger := Integer(Ssp.Extinct);
        ParamByName('ANOEXTINTO').AsString := Ssp.ExtinctionYear;
        ParamByName('ASPECIES').AsInteger := MoveToSp.SpeciesId;
        ParamByName('AGENUS').AsInteger := MoveToSp.GenusId;
        ParamByName('ASUBFAMILY').AsInteger := MoveToSp.SubfamilyId;
        ParamByName('AFAMILY').AsInteger := MoveToSp.FamilyId;
        ParamByName('AORDER').AsInteger := MoveToSp.OrderId;
        ParamByName('AGENUSNAME').AsString := MoveToSp.GenusEpithet;
        ParamByName('AEPITHET').AsString := MoveToSp.SpeciesEpithet;
        ParamByName('ASSPEPITHET').AsString := ExtractWord(3, NewName, [' ']);
        if (btClements in aTaxonomy) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
          ParamByName('AEBIRDCODE').AsString := Ssp.EbirdCode;
        end;
        if (btIOC in aTaxonomy) then
        begin
          ParamByName('ANIVELIOC').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');
          ParamByName('ASUPIOC').AsInteger := ParentSp;
          ParamByName('AENGLISHIOC').AsString := Ssp.IocEnglishName;
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        if (btCBRO in aTaxonomy) then
        begin
          ParamByName('ANIVELCBRO').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');
          ParamByName('ASUPCBRO').AsInteger := ParentSp;
        end;
        ParamByName('AUSER').AsInteger := AdminId;

        if ExecNow then
          ExecSQL;
      end;
    end;

    // Update subspecies
    if ExistingId > 0 then
      ValidSsp := ExistingId
    else
      ValidSsp := GetLastInsertedKey(tbZooTaxa);
    with aDataset do
    begin
      if ExecNow then
        SQL.Clear;
      SQL.Add('UPDATE zoo_taxa SET ');
      if (btClements in aTaxonomy) then { Clementes/eBird }
      begin
        SQL.Add('clements_taxonomy = 0,');
        SQL.Add('valid_id = :avalid,');
      end;
      if (btIOC in aTaxonomy) then { IOC }
      begin
        SQL.Add('ioc_taxonomy = 0,');
        SQL.Add('ioc_rank_id = :arank,');
        SQL.Add('ioc_valid_id = :avalid,');
      end;
      if (btCBRO in aTaxonomy) then { CBRO }
      begin
        SQL.Add('cbro_taxonomy = 0,');
        SQL.Add('cbro_rank_id = :arank,');
        SQL.Add('cbro_valid_id = :avalid,');
      end;
      SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
      SQL.Add('WHERE taxon_id = :ataxon;');
      if (Params.FindParam('ARANK') <> nil) then
      begin
        ParamByName('ARANK').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');
      end;
      if (Params.FindParam('AVALID') <> nil) then
      begin
        ParamByName('AVALID').AsInteger := ValidSsp;
      end;
      ParamByName('AUSER').AsInteger := AdminId;
      ParamByName('ATAXON').AsInteger := aSubspecies;

      if ExecNow then
        ExecSQL;
    end;

  finally
    FreeAndNil(Ssp);
    FreeAndNil(MoveToSp);
  end;
end;

procedure MoveToGenus(aSpecies, ToGenus: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery; ExecNow: Boolean
  );
var
  OldName, MoveToName, NewName: String;
  SpRank, ParentGenus, ValidSp, ExistingId: Integer;
  Ssp, Gen: TTaxon;
begin
  ExistingId := 0;
  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSpecies);
  MoveToName := GetName('zoo_taxa', 'full_name', 'taxon_id', ToGenus);
  NewName := MoveToName + ' ' + ExtractWord(2, OldName, [' ']);
  SpRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.');
  ParentGenus := ToGenus;
  Ssp := TTaxon.Create(aSpecies);
  Gen := TTaxon.Create(ToGenus);

  try
    // If taxon exists
    if RegistroExiste(tbZooTaxa, 'full_name', NewName) = True then
    begin
      ExistingId := GetKey('zoo_taxa', 'taxon_id', 'full_name', NewName);
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        SQL.Add('UPDATE zoo_taxa SET ');
        if (btClements in aTaxonomy) then { Clementes/eBird }
        begin
          SQL.Add('clements_taxonomy = 1,');
          SQL.Add('valid_id = null,');
          // SQL.Add('TAX_ENGLISH = '+QuotedStr(Ssp.NomeEnglish)+',');
          SQL.Add('distribution = :geodist,');
        end;
        if (btIOC in aTaxonomy) then { IOC }
        begin
          SQL.Add('ioc_taxonomy = 1,');
          SQL.Add('ioc_rank_id = :arank,');
          SQL.Add('ioc_parent_taxon_id = :asup,');
          SQL.Add('ioc_distribution = :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then { CBRO }
        begin
          SQL.Add('cbro_taxonomy = 1,');
          SQL.Add('cbro_rank_id = :arank,');
          SQL.Add('cbro_parent_taxon_id = :asup,');
        end;
        SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
        SQL.Add('WHERE taxon_id = :ataxon;');

        if (Params.FindParam('ARANK') <> nil) then
        begin
          ParamByName('ARANK').AsInteger := SpRank;
        end;
        if (Params.FindParam('ASUP') <> nil) then
        begin
          ParamByName('ASUP').AsInteger := ParentGenus;
        end;
        if (Params.FindParam('GEODIST') <> nil) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
        end;
        if (Params.FindParam('GEODISTIOC') <> nil) then
        begin
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        ParamByName('AUSER').AsInteger := AdminId;
        ParamByName('ATAXON').AsInteger := ExistingId;

        if ExecNow then
          ExecSQL;
      end;
    end
    else
    // If taxon does not exist
    begin
      with aDataset do
      begin
        if ExecNow then
          SQL.Clear;
        // List fields
        SQL.Add('INSERT INTO zoo_taxa (full_name, formatted_name, authorship, english_name,');
        SQL.Add('rank_id, parent_taxon_id, extinct, extinction_year, species_id, genus_id, subfamily_id,');
        SQL.Add('family_id, order_id, subspecies_group_id, genus_name, species_epithet, subspecies_epithet,');
        if (btClements in aTaxonomy) then
          SQL.Add('clements_taxonomy, distribution, ebird_code,');
        if (btIOC in aTaxonomy) then
          SQL.Add('ioc_taxonomy, ioc_rank_id, ioc_parent_taxon_id, ioc_english_name, ioc_distribution,');
        if (btCBRO in aTaxonomy) then
          SQL.Add('cbro_taxonomy, cbro_rank_id, cbro_parent_taxon_id,');
        SQL.Add('insert_date, user_inserted) ');
        // List values
        SQL.Add('VALUES (:aname, :aformattedname, :autoria, :aenglish, :anivel, :asup,');
        SQL.Add(':aextinto, :anoextinto, :aspecies, :agenus, :asubfamily, :afamily, :aorder, 0,');
        SQL.Add(':agenusname, :aepithet, null,');
        if (btClements in aTaxonomy) then
        begin
          SQL.Add('1, :geodist, :aebirdcode,');
        end;
        if (btIOC in aTaxonomy) then
        begin
          SQL.Add('1, :anivelioc, :asupioc, :aenglishioc, :geodistioc,');
        end;
        if (btCBRO in aTaxonomy) then
        begin
          SQL.Add('1, :anivelcbro, :asupcbro,');
        end;
        SQL.Add('datetime(''now'',''localtime''), :auser);');
        ParamByName('ANAME').AsString := NewName;
        ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, SpRank);
        ParamByName('AUTORIA').AsString := Ssp.Authorship;
        ParamByName('AENGLISH').AsString := Ssp.EnglishName;
        ParamByName('ANIVEL').AsInteger := SpRank;
        ParamByName('ASUP').AsInteger := ParentGenus;
        ParamByName('AEXTINTO').AsInteger := Integer(Ssp.Extinct);
        ParamByName('ANOEXTINTO').AsString := Ssp.ExtinctionYear;
        ParamByName('ASPECIES').AsInteger := GetLastInsertedKey(tbZooTaxa) + 1;
        ParamByName('AGENUS').AsInteger := ToGenus;
        ParamByName('ASUBFAMILY').AsInteger := Gen.SubfamilyId;
        ParamByName('AFAMILY').AsInteger := Gen.FamilyId;
        ParamByName('AORDER').AsInteger := Gen.OrderId;
        ParamByName('AGENUSNAME').AsString := Gen.FullName;
        ParamByName('AEPITHET').AsString := ExtractWord(2, NewName, [' ']);
        if (btClements in aTaxonomy) then
        begin
          ParamByName('GEODIST').DataType := ftMemo;
          ParamByName('GEODIST').AsString := Ssp.Distribution;
          ParamByName('AEBIRDCODE').AsString := Ssp.EbirdCode;
        end;
        if (btIOC in aTaxonomy) then
        begin
          ParamByName('ANIVELIOC').AsInteger := SpRank;
          ParamByName('ASUPIOC').AsInteger := ParentGenus;
          ParamByName('AENGLISHIOC').AsString := Ssp.IocEnglishName;
          ParamByName('GEODISTIOC').DataType := ftMemo;
          ParamByName('GEODISTIOC').AsString := Ssp.IocDistribution;
        end;
        if (btCBRO in aTaxonomy) then
        begin
          ParamByName('ANIVELCBRO').AsInteger := SpRank;
          ParamByName('ASUPCBRO').AsInteger := ParentGenus;
        end;
        ParamByName('AUSER').AsInteger := AdminId;

        if ExecNow then
          ExecSQL;
      end;
    end;

    // Update subspecies
    if ExistingId > 0 then
      ValidSp := ExistingId
    else
      ValidSp := GetLastInsertedKey(tbZooTaxa);
    with aDataset do
    begin
      if ExecNow then
        SQL.Clear;
      SQL.Add('UPDATE zoo_taxa SET ');
      if (btClements in aTaxonomy) then { Clementes/eBird }
      begin
        SQL.Add('clements_taxonomy = 0,');
        SQL.Add('valid_id = :avalid,');
      end;
      if (btIOC in aTaxonomy) then { IOC }
      begin
        SQL.Add('ioc_taxonomy = 0,');
        SQL.Add('ioc_rank_id = :arank,');
        SQL.Add('ioc_valid_id = :avalid,');
      end;
      if (btCBRO in aTaxonomy) then { CBRO }
      begin
        SQL.Add('cbro_taxonomy = 0,');
        SQL.Add('cbro_rank_id = :arank,');
        SQL.Add('cbro_valid_id = :avalid,');
      end;
      SQL.Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
      SQL.Add('WHERE taxon_id = :ataxon;');
      if (Params.FindParam('ARANK') <> nil) then
        begin
          ParamByName('ARANK').AsInteger := SpRank;
        end;
        if (Params.FindParam('AVALID') <> nil) then
        begin
          ParamByName('AVALID').AsInteger := ValidSp;
        end;
        ParamByName('AUSER').AsInteger := AdminId;
        ParamByName('ATAXON').AsInteger := aSpecies;

      if ExecNow then
        ExecSQL;
    end;

  finally
    FreeAndNil(Ssp);
    FreeAndNil(Gen);
  end;
end;

procedure MoveToFamily(aFamily: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  { #todo : Move taxon to family }
end;

procedure MoveToOrder(aOrder: Integer; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  { #todo : Move taxon to order }
end;

procedure UpdateScientificName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
var
  RankId: Integer;
begin
  RankId := GetRankFromTaxon(aTaxon);
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET full_name = :newname, formatted_name = :newhtml WHERE taxon_id = :ataxon;');
    ParamByName('NEWNAME').AsString := aNewName;
    ParamByName('NEWHTML').AsString := FormattedBirdName(aNewName, RankId);
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateEnglishName(aTaxon: Integer; aNewName: String; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
  ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    if (btClements in aTaxonomy) then
      Add('UPDATE zoo_taxa SET english_name = :newname WHERE taxon_id = :ataxon;');
    if (btIOC in aTaxonomy) then
      Add('UPDATE zoo_taxa SET ioc_english_name = :newname WHERE taxon_id = :ataxon;');
    ParamByName('NEWNAME').AsString := aNewName;
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdatePortuguesName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET portuguese_name = :newname WHERE taxon_id = :ataxon;');
    ParamByName('NEWNAME').AsString := aNewName;
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateOutrosPortugues(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET other_portuguese_names = :newname WHERE taxon_id = :ataxon;');
    ParamByName('NEWNAME').AsString := aNewName;
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateSpanishName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET spanish_name = :newname WHERE taxon_id = :ataxon;');
    ParamByName('NEWNAME').AsString := aNewName;
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateAuthorship(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET authorship = :autor WHERE taxon_id = :ataxon;');
    ParamByName('AUTOR').AsString := aNewName;
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateDistribution(aTaxon: Integer; aDist: String; aTaxonomy: TBirdTaxonomies; aDataset: TSQLQuery;
  ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    if (btClements in aTaxonomy) then
      Add('UPDATE zoo_taxa SET distribution = :geodist WHERE taxon_id = :ataxon;');
    if (btIOC in aTaxonomy) then
      Add('UPDATE zoo_taxa SET ioc_distribution = :geodist WHERE taxon_id = :ataxon;');
    ParamByName('GEODIST').AsString := aDist;
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateExtinction(aTaxon: Integer; IsExtinct: Boolean; aYear: String; aDataset: TSQLQuery;
  ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    if IsExtinct = True then
    begin
      Add('UPDATE zoo_taxa SET extinct = 1, extinction_year = :ayear WHERE taxon_id = :ataxon;');
      ParamByName('AYEAR').AsString := aYear;
    end
    else
      Add('UPDATE zoo_taxa SET extinct = 0 WHERE taxon_id = :ataxon;');
    ParamByName('ATAXON').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; UseValid: Boolean; var aCod: Integer;
  const aInit: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    FiltroTaxon := aFiltro;
    UsarValido := UseValid;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Init := aInit;
    if ShowModal = mrOK then
    begin
      aCod := dlgFind.Codigo;
      if aControl is TCustomEdit then
      begin
        TCustomEdit(aControl).Text := dlgFind.Nome;
        TCustomEdit(aControl).Modified := True;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset; aKeyField,
  aNameField: String; UseValid: Boolean; const aInit: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
  try
    FiltroTaxon := aFiltro;
    UsarValido := UseValid;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    Init := aInit;
    if ShowModal = mrOK then
    begin
      CanEdit(aDataSet);
      aDataSet.FieldByName(aKeyField).AsInteger := dlgFindTaxon.Codigo;
      aDataSet.FieldByName(aNameField).AsString := dlgFindTaxon.Nome;
      Result := True;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end;
  finally
    FreeAndNil(dlgFindTaxon);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

{ TSearchField }

constructor TSearchField.Create(aFieldName, aDisplayName: String; aDataType: TSearchDataType;
  aCriteria: TCriteriaType; IsLookup: Boolean; aValue1: String; aValue2: String);
begin
  FFieldName := aFieldName;
  FDisplayName := aDisplayName;
  FDataType := aDataType;
  FCriteria := aCriteria;
  FLookup := IsLookup;
  FValue1 := aValue1;
  FValue2 := aValue2;
end;

{ TSearchGroup }

constructor TSearchGroup.Create;
begin
  FFields := TSearchFields.Create(True);
end;

destructor TSearchGroup.Destroy;
begin
  FFields.Clear;
  FFields.Free;
  inherited Destroy;
end;

{ TCustomSearch }

constructor TCustomSearch.Create(aTable: TTableType);
begin
  FTableType := aTable;
  FTableAlias := TableAliases[aTable];
  FFields := TSearchGroups.Create(True);
  FQuickFilters := TSearchGroups.Create(True);
  FSortFields := TSortedFields.Create(True);
  FRecordActive := rsActive;
end;

destructor TCustomSearch.Destroy;
begin
  FFields.Clear;
  FQuickFilters.Clear;
  FSortFields.Clear;
  FFields.Free;
  FQuickFilters.Free;
  FSortFields.Free;
  inherited Destroy;
end;

function TCustomSearch.GetCount: Integer;
begin
  Result := 0;
  if not Assigned(FFields) then
    Exit;

  Result := FFields.Count + FQuickFilters.Count;
end;

function TCustomSearch.GetSQLString: String;
const
  MaskNull: String = '(%s %s) ';
  MaskV1: String = '(%s %s %s) ';
  MaskV2: String = '(%s %s %s AND %s) ';
  MaskDateV1: String = '(date(%s) %s date(%s)) ';
  MaskDateV2: String = '(date(%s) %s date(%s) AND date(%s)) ';
  MaskTimeV1: String = '(time(%s) %s time(%s)) ';
  MaskTimeV2: String = '(time(%s) %s time(%s) AND time(%s)) ';
  MaskDateTimeV1: String = '(datetime(%s) %s datetime(%s)) ';
  MaskDateTimeV2: String = '(datetime(%s) %s datetime(%s) AND datetime(%s)) ';
var
  i, f: Integer;
  V1, V2, S, AndOrWhere, Msk, aCriteria, aSort: String;

  // Select mask using criteria and data type
  function GetValueMask(aCriteriaType: TCriteriaType; aDataType: TSearchDataType): String;
  begin
    Result := MaskV1;

    if aCriteriaType in [crNull, crNotNull] then
      Result := MaskNull;

    case aDataType of
      sdtText, sdtList, sdtLookup:
        Result := MaskV1;
      sdtBoolean:
        Result := MaskV1;
      sdtInteger, sdtFloat:
        begin
          if aCriteriaType = crBetween then
            Result := MaskV2
          else
            Result := MaskV1;
        end;
      sdtDate:
        begin
          if aCriteriaType = crBetween then
            Result := MaskDateV2
          else
            Result := MaskDateV1;
        end;
      sdtTime:
        begin
          if aCriteriaType = crBetween then
            Result := MaskTimeV2
          else
            Result := MaskTimeV1;
        end;
      sdtDateTime:
        begin
          if aCriteriaType = crBetween then
            Result := MaskDateTimeV2
          else
            Result := MaskDateTimeV1;
        end;
    end;
  end;

  procedure PrepareValues(aCriteriaType: TCriteriaType; aDataType: TSearchDataType;
      var aValue1, aValue2: String);
  begin
    aValue1 := Trim(aValue1);
    aValue2 := Trim(aValue2);

    case aDataType of
      sdtText, sdtList, sdtLookup:
        begin
          if ExecRegExpr('^.+\+.+$', aValue1) then
            aValue1 := WildcardSyllables(aValue1) + '%'
          else
            aValue1 := WildcardWords(aValue1) + '%';

          if aCriteriaType = crLike then
            aValue1 := '%' + aValue1;

          aValue1 := QuotedStr(aValue1);
        end;
      sdtInteger, sdtFloat:
        begin
          aValue1 := StringReplace(aValue1, ',', '.', [rfReplaceAll]);
          aValue2 := StringReplace(aValue2, ',', '.', [rfReplaceAll]);
        end;
      sdtBoolean: ;
      sdtDate: ;
      sdtTime: ;
      sdtDateTime: ;
    end;
  end;

begin
  Result := EmptyStr;
  Msk := MaskV1;
  V1 := EmptyStr;
  V2 := EmptyStr;

  FDataSet.SQL.Clear;

  SetSelectSQL(FDataSet.SQL, FTableType, FTableAlias);
  AndOrWhere := 'WHERE ';

  // Add fields in WHERE clause
  if FQuickFilters.Count > 0 then
  begin
    // Iterate groups
    for i := 0 to (FQuickFilters.Count - 1) do
    begin
      if FQuickFilters[i].Fields.Count > 0 then
      begin
        FDataSet.SQL.Add(AndOrWhere + '(');
        // Iterate group fields
        for f := 0 to (FQuickFilters[i].Fields.Count - 1) do
        begin
          S := EmptyStr;
          with FQuickFilters[i].Fields[f] do
          begin
            PrepareValues(FCriteria, FDataType, FValue1, FValue2);
            Msk := GetValueMask(FCriteria, FDataType);

            // Fieldname, criteria, and values
            case Criteria of
              crLike, crStartLike, crEqual, crMoreThan, crLessThan:
              begin
                if FQuickFilters[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FieldName, CriteriaOperators[Criteria], Value1])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CriteriaOperators[Criteria], Value1]);
              end;
              crBetween:
              begin
                if FQuickFilters[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FieldName, CriteriaOperators[Criteria], Value1, Value2])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CriteriaOperators[Criteria], Value1, Value2]);
              end;
              crNull, crNotNull:
              begin
                if FQuickFilters[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FieldName, CriteriaOperators[Criteria]])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CriteriaOperators[Criteria]]);
              end;
            end;

            // Close parenthesis, and AND/OR
            if f < (FQuickFilters[i].Fields.Count - 1) then
            //  S := Trim(S) + ')'
            //else
              S := S + 'OR ';
          end;

          //if i = 0 then
          //  FDataSet.SQL.Add(AndOrWhere + S)
          //else
            FDataSet.SQL.Add(S);
        end;
        FDataSet.SQL.Add(')');
        AndOrWhere := 'AND ';
      end;
    end;
  end;

  if FFields.Count > 0 then
  begin
    // Iterate groups
    for i := 0 to (FFields.Count - 1) do
    begin
      if FFields[i].Fields.Count > 0 then
      begin
        FDataSet.SQL.Add(AndOrWhere + '(');
        // Iterate group fields
        for f := 0 to (FFields[i].Fields.Count - 1) do
        begin
          S := EmptyStr;
          with FFields[i].Fields[f] do
          begin
            PrepareValues(FCriteria, FDataType, FValue1, FValue2);
            Msk := GetValueMask(FCriteria, FDataType);

            // Fieldname, criteria, and values
            case Criteria of
              crLike, crStartLike, crEqual, crMoreThan, crLessThan:
              begin
                if FFields[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FieldName, CriteriaOperators[Criteria], Value1])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CriteriaOperators[Criteria], Value1]);
              end;
              crBetween:
              begin
                if FFields[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FieldName, CriteriaOperators[Criteria], Value1, Value2])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CriteriaOperators[Criteria], Value1, Value2]);
              end;
              crNull, crNotNull:
              begin
                if FFields[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FieldName, CriteriaOperators[Criteria]])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CriteriaOperators[Criteria]]);
              end;
            end;

            // Close parenthesis, and AND/OR
            if f < (FFields[i].Fields.Count - 1) then
            //  S := S + ') '
            //else
              S := S + 'OR ';
          end;

          //if i = 0 then
          //  FDataSet.SQL.Add(AndOrWhere + S)
          //else
            FDataSet.SQL.Add(S);
        end;
        FDataSet.SQL.Add(')');
        AndOrWhere := 'AND ';
      end;
    end;
  end;
  S := EmptyStr;

  case FRecordActive of
    rsAll: S := EmptyStr;
    rsActive: S := '1';
    rsInactive: S := '0';
    rsNone: S := EmptyStr;
  end;
  if S <> EmptyStr then
    if FTableAlias <> EmptyStr then
      FDataSet.SQL.Add(AndOrWhere + '(' + FTableAlias + '.active_status = ' + S + ')')
    else
      FDataSet.SQL.Add(AndOrWhere + '(active_status = ' + S + ')');

  // Add fields in ORDER BY clause
  if FSortFields.Count > 0 then
  begin
    aSort := EmptyStr;

    for i := 0 to (FSortFields.Count - 1) do
    begin
      // Field name
      if (FSortFields.Items[i].Lookup) or (FTableAlias = EmptyStr) then
        aSort := aSort + FSortFields.Items[i].FieldName
      else
        aSort := aSort + FTableAlias + '.' + FSortFields.Items[i].FieldName;

      // Collation
      if FSortFields.Items[i].Collation <> EmptyStr then
        aSort := aSort + ' COLLATE ' + FSortFields.Items[i].Collation;

      // Direction
      aSort := aSort + ' ' + SortDirections[FSortFields.Items[i].Direction];

      if i < (FSortFields.Count - 1) then
        aSort := aSort + ', ';
    end;
    FDataSet.SQL.Add('ORDER BY ' + aSort);
  end;

  {$IFDEF DEBUG}
  LogSQL(FDataSet.SQL);
  {$ENDIF}
end;

procedure TCustomSearch.Reset;
begin
  FRecordActive := rsActive;
  FFields.Clear;
  FQuickFilters.Clear;
end;

function TCustomSearch.RunSearch: Integer;
begin
  if FDataSet.Active then
    FDataSet.Close;

  GetSQLString;

  FDataSet.Open;
  Result := FDataSet.RecordCount;
end;

{ TXolmisRecord }

procedure TXolmisRecord.Clear;
begin
  FId := 0;
  FGuid := EmptyStr;
  FUserInserted := 0;
  FUserUpdated := 0;
  FInsertDate := StrToDateTime('30/12/1500 00:00:00');
  FUpdateDate := StrToDateTime('30/12/1500 00:00:00');
  FMarked := False;
  FExported := False;
  FActive := False;
end;

{ TRank }

constructor TRank.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TRank.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAcronym := EmptyStr;
  FRankIndex := 0;
  FMainRank := False;
  FSubrank := False;
  FInfrarank := False;
  FInfraspecific := False;
  FZoologicalCode := False;
  FBotanicalCode := False;
end;

procedure TRank.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT * FROM taxon_ranks');
    Add('WHERE rank_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('rank_id').AsInteger;
      FName := FieldByName('rank_name').AsString;
      FAcronym := FieldByName('rank_acronym').AsString;
      FRankIndex := FieldByName('rank_seq').AsInteger;
      FMainRank := FieldByName('main_rank').AsBoolean;
      FSubrank := FieldByName('subrank').AsBoolean;
      FInfrarank := FieldByName('infrarank').AsBoolean;
      FInfraspecific := FieldByName('infraspecific').AsBoolean;
      FZoologicalCode := FieldByName('zoological_code').AsBoolean;
      FBotanicalCode := FieldByName('botanical_code').AsBoolean;
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

{ TCustomTaxon }

procedure TCustomTaxon.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FFormattedName := EmptyStr;
  FAuthorship := EmptyStr;
  FRankId := 0;
  FParentTaxonId := 0;
  FValidId := 0;
  FOrderId := 0;
  FFamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
end;

{ TTaxon }

constructor TTaxon.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TTaxon.Clear;
begin
  inherited Clear;
  FEnglishName := EmptyStr;
  FPortugueseName := EmptyStr;
  FSpanishName := EmptyStr;
  FSortNum := 0.0;
  FQuickCode := EmptyStr;
  FExtinct := False;
  FExtinctionYear := EmptyStr;
  FDistribution := EmptyStr;
  FEbirdCode := EmptyStr;
  FClementsTaxonomy := False;
  FSubfamilyId := 0;
  FGenusEpithet := EmptyStr;
  FSpeciesEpithet := EmptyStr;
  FSubspeciesGroupId := 0;
  FSubspeciesGroupEpithet := EmptyStr;
  FSubspeciesEpithet := EmptyStr;
  FIncertaeSedis := 0;
  FIocTaxonomy := False;
  FIocEnglishName := EmptyStr;
  FIocParentTaxonId := 0;
  FIocRankId := 0;
  FIocValidId := 0;
  FIocDistribution := EmptyStr;
  FIocSortNum := 0.0;
  FCbroTaxonomy := False;
  FOtherPortugueseNames := EmptyStr;
  FCbroParentTaxonId := 0;
  FCbroRankId := 0;
  FCbroValidId := 0;
  FCbroSortNum := 0.0;
end;

function TTaxon.Diff(aOld: TTaxon; var aList: TStrings): Boolean;
begin

end;

procedure TTaxon.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT * FROM zoo_taxa');
    Add('WHERE taxon_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('taxon_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FFormattedName := FieldByName('formatted_name').AsString;
      FParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
      FRankId := FieldByName('rank_id').AsInteger;
      FAuthorship := FieldByName('authorship').AsString;
      FSortNum := FieldByName('sort_num').AsFloat;
      FQuickCode := FieldByName('quick_code').AsString;
      FEnglishName := FieldByName('english_name').AsString;
      FPortugueseName := FieldByName('portuguese_name').AsString;
      FSpanishName := FieldByName('spanish_name').AsString;
      FValidId := FieldByName('valid_id').AsInteger;
      FExtinct := FieldByName('extinct').AsBoolean;
      FExtinctionYear := FieldByName('extinction_year').AsString;
      FDistribution := FieldByName('distribution').AsString;
      FEbirdCode := FieldByName('ebird_code').AsString;
      FClementsTaxonomy := FieldByName('clements_taxonomy').AsBoolean;
      FOrderId := FieldByName('order_id').AsInteger;
      FFamilyId := FieldByName('family_id').AsInteger;
      FSubfamilyId := FieldByName('subfamily_id').AsInteger;
      FGenusId := FieldByName('genus_id').AsInteger;
      FGenusEpithet := FieldByName('genus_name').AsString;
      FSpeciesId := FieldByName('species_id').AsInteger;
      FSpeciesEpithet := FieldByName('species_epithet').AsString;
      FSubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
      FSubspeciesGroupEpithet := FieldByName('group_name').AsString;
      FSubspeciesEpithet := FieldByName('subspecies_epithet').AsString;
      FIncertaeSedis := FieldByName('incertae_sedis').AsInteger;
      FIocTaxonomy := FieldByName('ioc_taxonomy').AsBoolean;
      FIocEnglishName := FieldByName('ioc_english_name').AsString;
      FIocParentTaxonId := FieldByName('ioc_parent_taxon_id').AsInteger;
      FIocRankId := FieldByName('ioc_rank_id').AsInteger;
      FIocValidId := FieldByName('ioc_valid_id').AsInteger;
      FIocDistribution := FieldByName('ioc_distribution').AsString;
      FIocSortNum := FieldByName('ioc_sort_num').AsFloat;
      FCbroTaxonomy := FieldByName('cbro_taxonomy').AsBoolean;
      FOtherPortugueseNames := FieldByName('other_portuguese_names').AsString;
      FCbroParentTaxonId := FieldByName('cbro_parent_taxon_id').AsInteger;
      FCbroRankId := FieldByName('cbro_rank_id').AsInteger;
      FCbroValidId := FieldByName('cbro_valid_id').AsInteger;
      FCbroSortNum := FieldByName('cbro_sort_num').AsFloat;
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

end.

