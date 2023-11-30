unit dev_types;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Controls, DateUtils, RegExpr, DB, SQLDB, StrUtils, lcltype, lclintf, Windows,
  DBCtrls;

type
  TFormPosition = record
    X: Integer;
    Y: Integer;
    Height: Integer;
    Width: Integer;
  end;

type
  TDBManager = (dbSqlite = 0, dbPostgre = 1, dbFirebird = 2, dbMaria = 3, dbSqlserver = 4);

  TDBParams = record
    Name: String;
    Manager: TDBManager;
    IsLocal: Boolean;
    Database: String;
    Server: String;
    Port: Integer;
    Protocol: String;
    OpenMode: String;
    CharacterSet: String;
    StringFormat: String;
    UserName: String;
    Password: String;
    procedure Clear;
  end;

type
  TPartialDate = record
    Year: Integer;
    Month: Byte;
    Day: Byte;
    RomanMonth: Boolean;
    YearFirst: Boolean;
    Separator: Char;
    function ToString: String;
    procedure Clear;
    procedure Today;
  end;

type
  TSeparator = (spNone, spSemicolon, spComma, spColon, spPeriod, spPipe, spSlash, spHyphen,
    spUnderline);

const
  StrSeparators: array of Char = [#0, ';', ',', ':', '.', '|', '/', '-', '_'];
  Qualifiers: array [0 .. 4] of String = ('', 'sp.', 'cf.', 'aff.', '?');

type
  TQualificador = (qfNone = 0, qfSpuh = 1, qfConfer = 2, qfAffinis = 3, qfQuestion = 4);
  TAdendo = (adNone = 0, adGenus = 1, adSpecies = 2, adInfrasp = 3);

type
  TScientificName = record
    Name: String;
    Qualifier: TQualificador;
    Adendum: TAdendo;
    TaxonRank: Integer;
    EpithetInfra: String;
    Authorship: String;
  end;

  TBirdTaxonomy = set of (btClements, btIOC, btCBRO, bteBird);
  TTaxonomyAction = (taSplit, taLump, taMove, taUpdate);
  TZooRank = (trDomain, trSubDomain, trHyperkingdom, trSuperkingdom, trKingdom, trSubkingdom,
    trInfrakingdom, trParvkingdom, trSuperphylum, trPhylum, trSubphylum, trInfraphylum,
    trMicrophylum, trSuperclass, trClass, trSubclass, trInfraclass, trSubterclass, trParvclass,
    trSuperdivision, trDivision, trSubdivision, trInfradivision, trSuperlegion, trLegion,
    trSublegion, trInfralegion, trSupercohort, trCohort, trSubcohort, trInfracohort, trGigaorder,
    trMegaorder, trGrandorder, trHyperorder, trSuperorder, trSeriesOrder, trOrder, trNanorder,
    trHypoorder, trMinorder, trSuborder, trInfraorder, trParvorder, trSection, trSubsection,
    trGigafamily, trMegafamily, trGrandfamily, trHyperfamily, trSuperfamily, trEpifamily,
    trSeriesFamily, trGroupFamily, trFamily, trSubfamily, trInfrafamily, trSupertribe, trTribe,
    trSubtribe, trInfratribe, trSupergenus, trGenus, trSubgenus, trSuperspecies, trSpecies,
    trSubspecies, trForm);
  TEbirdRank = (erSpuh, erHybrid, erIntergrade, erDomestic, erSlash);

const
  NomeTaxonomia: array [0 .. 2] of String = ('Clements/eBird', 'IOC', 'CBRO');

const
  ZooRanks: array of String = ['D.', 'SD.', 'HK.', 'SK.', 'K.', 'sk.', 'ik.', 'pk.', 'SPh.', 'ph.',
    'subph.', 'infraph.', 'microph.', 'sc.', 'c.', 'subc.', 'infrac.', 'stc.', 'parvc.', 'sdiv.',
    'div.', 'subdiv.', 'infradiv.', 'sleg.', 'leg.', 'subleg.', 'infraleg.', 'scoh.', 'coh.',
    'subcoh.', 'infracoh.', 'Gord.', 'Mord.', 'grandord.', 'Hord.', 'superod.', 'seriesord.',
    'ord.', 'nord.', 'hypoord.', 'minord.', 'subord.', 'infraord.', 'parvord.', 'sect.', 'subsect.',
    'Gfam.', 'Mfam.', 'grandfam.', 'hyperfam.', 'superfam.', 'epifam.', 'seriesfam.', 'groupfam.',
    'fam.', 'subfam.', 'infrafam.', 'supertr.', 'tr.', 'subtr.', 'infratr.', 'superg.', 'g.',
    'subg.', 'supersp.', 'sp.', 'ssp.', 'f.'];

const
  StrCriteria: array of String = ['', 'like', 'like', '=', 'between', '>=', '<=', 'isnull', 'notnull'];

type
  TCriteriaType = (crNone, crLike, crStartLike, crEqual, crBetween, crMoreThan, crLessThan, crNull,
    crNotNull);
  TCampoType = (ctString, ctInteger, ctFloat, ctDate, ctTime, ctDateTime, ctBoolean);
  TFilterType = (tcTexto, tcInteiro, tcDecimal, tcData, tcHora, tcDataHora, tcLista, tcBool,
    tcLookup);
  TFilterValue = (fvNone, fvReset, fvAll, fvMarked, fvUnmarked, fvDeleted, fvQueued);
  TExternalFilter = (efNone, efId, efOrder, efFamily, efGenus, efSpecies, efQueued);
  TConsultaType = (csNone, csName, csNumber, csDate, csBoolean);

  TRecordStatus = record
    Status: (rsAll, rsActive, rsInactive);
    Mark: (rmAll, rmMarked, rmUnmarked);
    Queue: (rqAll, rqQueued, rqUnqueued);
    Share: (rxAll, rxExported, rxNotExported);
    procedure Clear;
  end;

  TFilterField = record
    FieldName: String;
    ReadableName: String;
    TableAlias: String;
    FilterType: TFilterType;
    Criteria: TCriteriaType;
    Value1: String;
    Value2: String;
    OpenParenthesis: Word;
    CloseParenthesis: Word;
    AndOr: (aoNone, aoAnd, aoOr);
    function ToSQL(aTabela, aLookupField, aKeyField: String): String;
    function ToHTML: String;
    function ToString: String;
    procedure Clear;
  end;

  TSearch = record
    FieldNames: TStrings;
    FilterType: TFilterType;
    Criteria: TCriteriaType;
    Value1: String;
    Value2: String;
    procedure Create;
    procedure Free;
    function ToSQL: String;
    function ToString: String;
    procedure Clear;
    function IsEmpty: Boolean;
  end;

  TSortDirection = (sdNone, sdAscending, sdDescending);
  TSortType = (stNone, stAlfanumeric, stNumeric, stDateTime, stBoolean, stTaxonomic);

  TSortedField = record
    FieldName: String;
    Direction: TSortDirection;
  end;

  TSortedFields = array of TSortedField;

type
  TTaxonFilter = (tfNone, tfMain, tfKingdoms, tfPhyla, tfClasses, tfOrders, tfFamilies, tfTribes,
    tfGenera, tfSpecies, tfInfrasp, tfFamTribes, tfGenSpecies, tfSpeciesGrp, tfSppInfrasp);

type
  TCampo = record
    FieldName: String;
    Caption: String;
    NumericKey: Boolean;
    TextualKey: Boolean;
    DwCField: String;
    DataType: TCampoType;
    FilterType: TFilterType;
    LookupTable: String;
    LookupKey: String;
    LookupField: String;
    LookupFieldName: String;
    MinValue: Extended;
    MaxValue: Extended;
    ListValues: String;
    Visible: Boolean;
    CanSort: Boolean;
  end;

  TTabelaType = (tbNone, tbVersions, tbTables, tbDataMapping, tbReports, tbUsageStats, tbUsers,
    tbUserHistory, tbCollections, tbBatchMaster, tbBatchDetail, tbProjectTeams, tbPermits,
    tbGazetteer, tbBotany, tbNests, tbNestRevisions, tbEggs, tbNetStations, tbSpecimenTypes,
    tbTaxonRanks, tbTaxa, tbProjects, tbInstitutions, tbResearchers, tbSurveys, tbMethods,
    tbBanders, tbNetEffort, tbSightings, tbSpecimens, tbSpecimenPreps, tbNetSites, tbBands,
    tbIndividuals, tbMorphometry, tbIndividualMolt, tbImportTemp);


const
  MesRomano: array of String = ['00', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X',
    'XI', 'XII'];

  TiposCampo: array of String = ['String', 'Integer', 'Float', 'Date', 'Time', 'DateTime',
    'Boolean'];
  TiposFiltro: array of String = ['Texto', 'Inteiro', 'Decimal', 'Data', 'Hora', 'DataHora',
    'Lista', 'Bool', 'Lookup'];

resourcestring
  rs_FilterAnd = 'e';
  rs_FilterOr = 'ou';
  rs_FilterLike = 'contém';
  rs_FilterStartLike = 'inicia com';
  rs_FilterEqual = 'igual a';
  rs_FilterMoreThan = 'maior ou igual a';
  rs_FilterLessThan = 'menor ou igual a';
  rs_FilterBetween = 'entre';
  rs_FilterNull = 'vazio';
  rs_FilterNotNull = 'não vazio';

  { Hashtags }
const
  AllQS: array of String        = ['#tudo', '#all'];
  MarkedQS: array of String     = ['#marcados', '#marked'];
  UnmarkedQS: array of String   = ['#naomarcados', '#unmarked'];
  FilterQS: array of String     = ['#filtro', '#filter'];
  DeletedQS: array of String    = ['#lixo', '#deleted'];
  PrintQueueQS: array of String = ['#fila', '#queued', '#toprint'];
  OrderQS: array of String      = ['#ordem', '#order', '#ord'];
  FamilyQS: array of String     = ['#familia', '#family', '#fam'];
  GenusQS: array of String      = ['#genero', '#genus', '#gen'];
  SpeciesQS: array of String    = ['#especie', '#species', '#sp'];
  SiteQS: array of String       = ['#local', '#site'];
  QualifierQS: array of String  = ['#quali', '#qualifier'];
  SuperiorQS: array of String   = ['#superior', '#parent'];
  RankQS: array of String       = ['#nivel', '#categoria', '#rank'];
  ListsQS: array of String      = ['#listas', '#lists'];
  LabelsQS: array of String     = ['#etiquetas', '#labels', '#etiq'];
  SqlQS: array of String        = ['#sql', '#sqlfilter'];

  function QualificadorToString(const aQualifier: TQualificador): String;
  function StringToQualificador(const aStr: String): TQualificador;

  function FindSortedField(const aFieldName: String; aSortedFields: TSortedFields): Integer;
  procedure AddSortedField(const aFieldName: String; aDirection: TSortDirection;
    var aSortedFields: TSortedFields);
  procedure DeleteSortedField(const aFieldName: String; var aSortedFields: TSortedFields);
  procedure ResetSortedFields(const aDefaultField: String; aDefaultDirection: TSortDirection;
    var aSortedFields: TSortedFields);

  function CampoByName(const aCampoName: String): TCampoType;
  function FilterByName(const aFilterName: String): TFilterType;
  function GetModifier(aModifier: String): TFilterValue;

  procedure SetSelect(const aSQL: TStrings; aTable: TTabelaType; var aAlias: String);
  procedure SetCount(const aSQL: TStrings; aTable: TTabelaType);

  procedure SetMetodos(const aSQL: TStrings; aFilter: TFilterValue;
    const aOrder, aDirection: String); overload;
  procedure SetMetodos(const aSQL: TStrings; aFilter: TFilterValue); overload;

  procedure CountIndividuos(const aSQL: TStrings);

  function TableSearch(aQuery: TSQLQuery; aTabela: TTabelaType; aSearch: TSearch;
    aQuickFilter: TStrings; aModifier: TRecordStatus; aOrder: TSortedFields;
    aWhere: TStrings): Boolean;
  function CarregaCodigo(aTabela, aCampoNome, aValor: String): Int64;
  function CarregaNome(aTabela, aCampoNome, aCampoCodigo: String; aValor: Int64): String;
  function GetNivelFromTaxon(aCodigo: Int64): Int64;
  function GetRank(const aCodigo: Int64): TZooRank;
  function IsInfraspecific(const aRank: Int64): Boolean;

  function MontaNomeCientifico(aSciName: TScientificName; Formatted: Boolean): String;
  function WordSearch(aText: String): String;
  function SyllableSearch(aText: String): String;

  procedure MakeRounded(Control: TWinControl);

  function BuscarTaxon(const aInit: String; aFiltro: TTaxonFilter; aEdit: TDBEdit;
    UseValid: Boolean; var aCod: Int64): Boolean; overload;
  function BuscarTaxon(const aInit: String; aFiltro: TTaxonFilter; aEdit: TDBEdit;
    aDataset: TDataset; aKeyField, aNomeField: String; UseValid: Boolean): Boolean; overload;

implementation

uses dm.dev, dlg.findtaxon;

function QualificadorToString(const aQualifier: TQualificador): String;
begin
  Result := Qualifiers[Ord(aQualifier)];
end;

function StringToQualificador(const aStr: String): TQualificador;
var
  i: Integer;
begin
  for i := 0 to 4 do
    if Qualifiers[i] = aStr then
      Result := TQualificador(i);
end;

function FindSortedField(const aFieldName: String; aSortedFields: TSortedFields): Integer;
var
  i: Integer;
begin
  Result := -1;

  if not Assigned(aSortedFields) then
    Exit;

  if Length(aSortedFields) = 0 then
    Exit;

  for i := Low(aSortedFields) to High(aSortedFields) do
    if aSortedFields[i].FieldName = aFieldName then
      Result := i;
end;

procedure AddSortedField(const aFieldName: String; aDirection: TSortDirection;
  var aSortedFields: TSortedFields);
var
  n: Integer;
begin
  if not Assigned(aSortedFields) then
    Exit;

  n := FindSortedField(aFieldName, aSortedFields);
  // Se o campo já está na lista
  if n >= 0 then
  begin
    case aSortedFields[n].Direction of
      sdNone:
        aSortedFields[n].Direction := aDirection;
      sdAscending:
        aSortedFields[n].Direction := sdDescending;
      sdDescending:
        aSortedFields[n].Direction := sdAscending;
    end;
  end
  else
  // Se não está na lista, adiciona o campo
  begin
    n := Length(aSortedFields) + 1;
    SetLength(aSortedFields, n);
    aSortedFields[n].FieldName := aFieldName;
    aSortedFields[n].Direction := aDirection;
  end;
end;

procedure DeleteSortedField(const aFieldName: String; var aSortedFields: TSortedFields);
var
  n: Integer;
begin
  if not Assigned(aSortedFields) then
    Exit;

  n := FindSortedField(aFieldName, aSortedFields);
  // Se o campo já está na lista
  if n >= 0 then
  begin
    Delete(aSortedFields, n, 1);
  end;
end;

procedure ResetSortedFields(const aDefaultField: String; aDefaultDirection: TSortDirection;
  var aSortedFields: TSortedFields);
begin
  if not Assigned(aSortedFields) then
    Exit;

  SetLength(aSortedFields, 1);
  aSortedFields[0].FieldName := aDefaultField;
  aSortedFields[0].Direction := aDefaultDirection;
end;

function CampoByName(const aCampoName: String): TCampoType;
var
  i: Integer;
begin
  for i := 0 to High(TiposCampo) do
  begin
    if TiposCampo[i] = aCampoName then
    begin
      Result := TCampoType(i);
      Break;
    end;
  end;
end;

function FilterByName(const aFilterName: String): TFilterType;
var
  i: Integer;
begin
  for i := 0 to High(TiposFiltro) do
  begin
    if TiposFiltro[i] = aFilterName then
    begin
      Result := TFilterType(i);
      Break;
    end;
  end;
end;

function GetModifier(aModifier: String): TFilterValue;
begin
  Result := fvNone;
  //GravaLog('HASHTAG', aModifier);

  if MatchStr(aModifier, AllQS) then { #tudo }
    Result := fvAll
  else
  if MatchStr(aModifier, MarkedQS) then { #marcados }
    Result := fvMarked
  else
  if MatchStr(aModifier, UnmarkedQS) then { #naomarcados }
    Result := fvUnmarked
  else
  if MatchStr(aModifier, DeletedQS) then { #lixo }
    Result := fvDeleted
  else
  if MatchStr(aModifier, PrintQueueQS) then { #fila }
    Result := fvQueued;
end;

procedure SetSelect(const aSQL: TStrings; aTable: TTabelaType; var aAlias: String);
begin
  aAlias := '';
  case aTable of
    tbNone:
      ;
    tbVersions:
      ;

  end;
end;

procedure SetCount(const aSQL: TStrings; aTable: TTabelaType);
begin
  case aTable of
    tbNone:
      ;
    tbVersions:
      ;

  end;
end;

procedure SetMetodos(const aSQL: TStrings; aFilter: TFilterValue; const aOrder, aDirection: String);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    Add('select * from AUX_METODOS');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('where (reg_ativo = 1)');
      fvAll:
        Add('where (reg_ativo = 1)');
      fvMarked:
        Add('where (reg_ativo = 1) and (reg_marcado = 1)');
      fvDeleted:
        Add('where (reg_ativo = 0)');
    end;
    if Trim(aOrder) <> '' then
    begin
      if aDirection = '' then
        AD := 'asc'
      else
        AD := aDirection;
      Add('order by ' + aOrder + AD);
    end;
  end;
end;

procedure SetMetodos(const aSQL: TStrings; aFilter: TFilterValue);
begin
  SetMetodos(aSQL, aFilter, '', '');
end;

procedure CountIndividuos(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('with');
    Add('LISTA as (');
    Add('select a.reg_ativo as ATIVO, a.reg_marcado as MARCADO, a.*,');
    Add('(select f.INS_ABREVIATURA from AUX_INSTITUICOES f where f.reg_num_interno=a.ANI_FORNECEDOR) as NOME_FORNECEDOR,');
    Add('(select s.PES_NOME from AUX_PESQUISADORES s where s.reg_num_interno=a.ANI_SOLICITANTE) as NOME_SOLICITANTE,');
    Add('(select p.PES_NOME from AUX_PESQUISADORES p where p.reg_num_interno=a.ANI_PORTADOR) as NOME_PORTADOR,');
    Add('(select r.PES_NOME from AUX_PESQUISADORES r where r.reg_num_interno=a.ANI_REMETENTE) as NOME_REMETENTE,');
    Add('(select i.IND_NOME_COMPLETO from XOL_INDIVIDUOS i where i.reg_num_interno=a.IND_CODIGO) as NOME_INDIVIDUO,');
    Add('(select l.PRJ_TITULO from AUX_PROJETOS l where l.reg_num_interno=a.PRJ_CODIGO) as NOME_PROJETO');
    Add('from XOL_ANILHAS a');
    // Add(')');
  end;
end;

function TableSearch(aQuery: TSQLQuery; aTabela: TTabelaType; aSearch: TSearch;
  aQuickFilter: TStrings; aModifier: TRecordStatus; aOrder: TSortedFields;
  aWhere: TStrings): Boolean;
var
  AndWhere, aAlias, aSort, aDir: String;
  i: Integer;
begin
  aAlias := EmptyStr;
  AndWhere := 'where ';
  aWhere.Clear;
  Result := False;

  with aQuery, SQL do
  begin
    Close;
    Clear;
    SetSelect(SQL, aTabela, aAlias);

    // Valor digitado no campo Pesquisar
    if not aSearch.IsEmpty then
    begin
      Add(aSearch.ToSQL);
      aWhere.Add(aSearch.ToSQL);
      AndWhere := 'and ';
    end;

    // Filtros rápidos selecionados
    if aQuickFilter.Count > 0 then
    begin
      for i := 0 to aQuickFilter.Count - 1 do
      begin
        Add(AndWhere + aQuickFilter[i]);
        aWhere.Add(AndWhere + aQuickFilter[i]);
        AndWhere := 'and ';
      end;
    end;

    // Registro ativo ou inativo
    case aModifier.Status of
      rsAll:
        ;
      rsActive:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_ativo = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_ativo = 1)');
          AndWhere := 'and ';
        end;
      rsInactive:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_ativo = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_ativo = 0)');
          AndWhere := 'and ';
        end;
    end;
    // Registro marcado ou desmarcado
    case aModifier.Mark of
      rmAll:
        ;
      rmMarked:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_marcado = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_marcado = 1)');
          AndWhere := 'and ';
        end;
      rmUnmarked:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_marcado = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_marcado = 0)');
          AndWhere := 'and ';
        end;
    end;
    // Registro na fila de impressão ou não
    case aModifier.Queue of
      rqAll:
        ;
      rqQueued:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 1)');
          AndWhere := 'and ';
        end;
      rqUnqueued:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 0)');
          AndWhere := 'and ';
        end;
    end;
    // Registro já compartilhado ou não
    case aModifier.Share of
      rxAll:
        ;
      rxExported:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_exported = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_exported = 1)');
          AndWhere := 'and ';
        end;
      rxNotExported:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_exported = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_exported = 0)');
          AndWhere := 'and ';
        end;
    end;

    // Coleção ativa
    //if aCollection.Codigo > 0 then
    //begin
    //  Add(AndWhere + '(' + aAlias + 'reg_collection = ' + IntToStr(aCollection.Codigo) + ')');
    //  aWhere.Add(AndWhere + '(' + aAlias + 'reg_collection = ' + IntToStr(aCollection.Codigo) + ')');
    //  AndWhere := 'and ';
    //end;

    // Ordenação dos registros
    if Length(aOrder) > 0 then
    begin
      aSort := '';
      aDir := '';
      for i := Low(aOrder) to High(aOrder) do
      begin
        case aOrder[i].Direction of
          sdNone:
            ;
          sdAscending:
            aDir := 'asc';
          sdDescending:
            aDir := 'desc';
        end;
        if Pos('NOME_', aOrder[i].FieldName) = 1 then
          aSort := aSort + aOrder[i].FieldName + {' collate PT_BR ' +} aDir
        else
          aSort := aSort + aAlias + aOrder[i].FieldName + {' collate PT_BR ' +} aDir;
        if i < High(aOrder) then
          aSort := aSort + ', ';
      end;
      Add('order by ' + aSort);
    end;

    //GravaLogSQL(SQL);
    Open;
  end;

  Result := not aQuery.IsEmpty;
end;

function CarregaCodigo(aTabela, aCampoNome, aValor: String): Int64;
var
  //cpoCodigo: String;
  Qry: TSQLQuery;
begin
  if aValor = '' then
    Result := 0
  else
  begin
    //if aTabela = 'ADM_USERS' then
    //  cpoCodigo := 'USR_CODIGO'
    //else
    //  cpoCodigo := 'reg_num_interno';
    Qry := TSQLQuery.Create(DMD.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMD.sqlCon;
      Clear;
      Add('select reg_num_interno from %TABNAME where %UNIQUEF = :UNIQUEV');
      MacroByName('TABNAME').AsString := aTabela;
      MacroByName('UNIQUEF').AsString := aCampoNome;
      ParamByName('UNIQUEV').AsString := aValor;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
        Result := FieldByName('reg_num_interno').AsLargeInt
      else
        Result := 0;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function CarregaNome(aTabela, aCampoNome, aCampoCodigo: String; aValor: Int64): String;
var
  Qry: TSQLQuery;
begin
  if aValor > 0 then
  begin
    Qry := TSQLQuery.Create(DMD.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMD.sqlCon;
      Clear;
      Add('select %UNIQUEF from %TABNAME where %KEYF = :KEYV');
      MacroByName('UNIQUEF').AsString := aCampoNome;
      MacroByName('TABNAME').AsString := aTabela;
      MacroByName('KEYF').AsString := aCampoCodigo;
      ParamByName('KEYV').AsLargeInt := aValor;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
      begin
        Result := FieldByName(aCampoNome).AsString;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetNivelFromTaxon(aCodigo: Int64): Int64;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMD.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMD.sqlCon;
    Clear;
    Add('select TAX_NIVEL from AUX_TAXONS where reg_num_interno = :KEYV');
    ParamByName('KEYV').AsLargeInt := aCodigo;
    // GravaLogSQL(SQL);
    Open;
    if not(IsEmpty) then
      Result := FieldByName('TAX_NIVEL').AsLargeInt
    else
      Result := 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetRank(const aCodigo: Int64): TZooRank;
var
  ab: String;
  i: Integer;
begin
  ab := CarregaNome('AUX_TAX_NIVEIS', 'reg_num_interno', 'NIV_ABREVIATURA', aCodigo);
  for i := 0 to (Length(ZooRanks) - 1) do
    if (ab = ZooRanks[i]) then
    begin
      Result := TZooRank(i);
      Break;
    end;
end;

function IsInfraspecific(const aRank: Int64): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMD.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMD.sqlCon;
    Clear;
    Add('select NIV_INFRA_ESPECIFICO from AUX_TAX_NIVEIS where reg_num_interno = :KEYV');
    ParamByName('KEYV').AsLargeInt := aRank;
    Open;
    Result := FieldByName('NIV_INFRA_ESPECIFICO').AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function MontaNomeCientifico(aSciName: TScientificName; Formatted: Boolean): String;
var
  Html, bName, nRank, Epi, Quali: String;
  totalParts: Integer;
  Parts: TStringList;
const
  Italicos: array of String = ['g.', 'subg.', 'sect.', 'subsect.', 'ser.', 'subser.', 'sp.',
    'subsp.', 'var.', 'subvar.', 'f.', 'subf.'];
begin
  if Trim(aSciName.Name) = '' then
  begin
    Result := '';
    Exit;
  end;

  // Nível taxonômico
  nRank := CarregaNome('AUX_TAX_NIVEIS', 'NIV_NOME_ABREV', 'reg_num_interno', aSciName.TaxonRank);

  Parts := TStringList.Create;
  totalParts := ExtractStrings([' '], [' '], PAnsiChar(aSciName.Name), Parts);
  // Binomio
  if totalParts <= 2 then
    bName := aSciName.Name
  else
    bName := Parts[0] + ' ' + Parts[1];
  if MatchText(nRank, Italicos) then
    if Formatted then
      bName := '<i>' + bName + '</i>';

  // Epíteto infraespecífico
  if IsInfraspecific(aSciName.TaxonRank) then
    if (aSciName.EpithetInfra <> '') then
    begin
      if Formatted then
        Epi := '<font color="gray">' + nRank + '</font> <i>' + aSciName.EpithetInfra + '</i>'
      else
        Epi := nRank + ' ' + aSciName.EpithetInfra;
    end
    else
      Epi := '';

  // Qualificador e Adendo
  if aSciName.Qualifier <> qfNone then
  begin
    if Formatted then
      Quali := '<font color="gray">' + QualificadorToString(aSciName.Qualifier) + '</font>'
    else
      Quali := QualificadorToString(aSciName.Qualifier);
  end
  else
    Quali := '';

  // Monta nome científico sem autor
  case aSciName.Qualifier of
    qfNone:
      Html := bName + ' ' + Epi;
    qfConfer, qfAffinis:
      begin
        case aSciName.Adendum of
          adNone:
            Html := bName + ' ' + Epi;
          adGenus:
            Html := Quali + ' ' + bName + ' ' + Epi;
          adSpecies:
            begin
              if Formatted then
                Html := StringReplace(bName, ' ', '</i> ' + Quali + ' <i>', []) + ' ' + Epi
              else
                Html := StringReplace(bName, ' ', ' ' + Quali + ' ', []) + ' ' + Epi;
            end;
          adInfrasp:
            Html := bName + ' ' + Epi + ' ' + Quali;
        end;
      end;
    qfSpuh:
      Html := bName + ' ' + Quali;
    qfQuestion:
      begin
        if (totalParts = 1) then
          Html := bName + ' ' + Quali
        else
          case aSciName.Adendum of
            adNone:
              Html := bName + ' ' + Epi;
            adGenus:
              Html := Quali + ' ' + bName + ' ' + Epi;
            adSpecies:
              begin
                if Formatted then
                  Html := StringReplace(bName, ' ', '</i> ' + Quali + ' <i>', []) + ' ' + Epi
                else
                  Html := StringReplace(bName, ' ', ' ' + Quali + ' ', []) + ' ' + Epi;
              end;
            adInfrasp:
              Html := bName + ' ' + Epi + ' ' + Quali;
          end;
      end;
  end;
  Html := Trim(Html);

  // Autoria
  if (aSciName.Authorship <> '') then
    if Formatted then
      Html := Html + ' <font color="gray">' + aSciName.Authorship + '</font>'
    else
      Html := Html + ' ' + aSciName.Authorship;
  Parts.Free;

  Result := Html;
end;

function WordSearch(aText: String): String;
var
  i, total: Integer;
  Words: TStringList;
begin
  Words := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  total := ExtractStrings([' '], [' '], PAnsiChar(aText), Words);
  if total > 1 then
    for i := 0 to Words.Count - 1 do
      if ExecRegExpr('^#[a-z]+$', Words[i]) then
        Result := Result + ''
      else
        if i = 0 then
          Result := AnsiDequotedStr(Words[i], '"') + '% '
        else
          if i = Words.Count - 1 then
            Result := Result + '%' + AnsiDequotedStr(Words[i], '"')
          else
            Result := Result + '%' + AnsiDequotedStr(Words[i], '"') + '% ';
  if total = 1 then
    if ExecRegExpr('^#[a-z]+$', aText) then
      Result := ''
    else
      Result := AnsiDequotedStr(aText, '"');
end;

function SyllableSearch(aText: String): String;
var
  i, total: Integer;
  Syllables: TStringList;
begin
  Result := '';
  Syllables := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  total := ExtractStrings([' ', '+'], [' '], PAnsiChar(aText), Syllables);
  if total > 1 then
    for i := 0 to Syllables.Count - 1 do
      if ExecRegExpr('^#[a-z]+$', Syllables[i]) then
        Result := Result + ''
      else
        if i = 0 then
          Result := AnsiDequotedStr(Syllables[i], '"') + '%'
        else
          if i = Syllables.Count - 1 then
            Result := Result + AnsiDequotedStr(Syllables[i], '"')
          else
            Result := Result + AnsiDequotedStr(Syllables[i], '"') + '%';
  if total = 1 then
    if ExecRegExpr('^#[a-z]+$', aText) then
      Result := ''
    else
      Result := AnsiDequotedStr(aText, '"');
end;

procedure MakeRounded(Control: TWinControl);
var
  R: TRect;
  Rgn: HRGN;
begin
  with Control do
  begin
    R := ClientRect;
    Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 20, 20);
    Perform(EM_GETRECT, 0, lParam(@R));
    InflateRect(R, -4, -4);
    Perform(EM_SETRECTNP, 0, lParam(@R));
    SetWindowRgn(Handle, Rgn, True);
    Invalidate;
  end;
end;

function BuscarTaxon(const aInit: String; aFiltro: TTaxonFilter; aEdit: TDBEdit;
  UseValid: Boolean; var aCod: Int64): Boolean;
var
  P: TFormPosition;
begin
  Result := False;
  //GravaLog('ABRE BUSCA', aEdit.Name + ':AUX_TAXONS');
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
    try
      FiltroTaxon := aFiltro;
      UsarValido := UseValid;
      P.X := PontoPosTela(aEdit).X;
      P.Y := PontoPosTela(aEdit).Y;
      P.Height := aEdit.Height;
      P.Width := aEdit.Width;
      WindowPos := P;
      Init := aInit;
      if ShowModal = mrOK then
      begin
        aEdit.Text := dlgFindTaxon.Nome;
        aCod := dlgFindTaxon.Codigo;
        Result := True;
        aEdit.Modified := True;
      end;
    finally
      FreeAndNil(dlgFindTaxon);
      //GravaLog('FECHA BUSCA', '');
    end;
end;

function BuscarTaxon(const aInit: String; aFiltro: TTaxonFilter; aEdit: TDBEdit;
  aDataset: TDataset; aKeyField, aNomeField: String; UseValid: Boolean): Boolean;
var
  P: TFormPosition;
begin
  Result := False;
  //GravaLog('ABRE BUSCA', aEdit.Name + ':AUX_TAXONS');
  dlgFindTaxon := TdlgFindTaxon.Create(nil);
  with dlgFindTaxon do
    try
      FiltroTaxon := aFiltro;
      UsarValido := UseValid;
      P.X := PontoPosTela(aEdit).X;
      P.Y := PontoPosTela(aEdit).Y;
      P.Height := aEdit.Height;
      P.Width := aEdit.Width;
      WindowPos := P;
      Init := aInit;
      if ShowModal = mrOK then
      begin
        with aDataset do
        begin
          if not Active then
          begin
            Open;
            Append;
          end
          else
          begin
            if not(State in [dsInsert, dsEdit]) then
              if RecordCount > 0 then
                Edit
              else
                Append;
          end;
          FieldByName(aKeyField).AsLargeInt := dlgFindTaxon.Codigo;
          FieldByName(aNomeField).AsWideString := dlgFindTaxon.Nome;
        end;
        Result := True;
        aEdit.Modified := True;
      end;
    finally
      FreeAndNil(dlgFindTaxon);
      //GravaLog('FECHA BUSCA', '');
    end;
end;

{ TPartialDate }

procedure TPartialDate.Clear;
begin
  Year := 0;
  Month := 0;
  Day := 0;
  RomanMonth := True;
  YearFirst := False;
  Separator := '.';
end;

procedure TPartialDate.Today;
var
  a, m, d: Word;
begin
  DecodeDate(DateUtils.Today, a, m, d);
  Year := a;
  Month := m;
  Day := d;
  RomanMonth := True;
  YearFirst := False;
  Separator := '.';
end;

function TPartialDate.ToString: String;
var
  S, m, d, Y: String;
begin
  if RomanMonth then
    m := MesRomano[Month]
  else
    m := Format('%2.2d', [Month]);
  d := Format('%2.2d', [Day]);
  Y := Format('%4.4d', [YearOf(EncodeDate(Year, 1, 1))]);
  if YearFirst then
    S := Y + Separator + m + Separator + d
  else
    S := d + Separator + m + Separator + Y;

  Result := S;
end;

{ TFilterField }

procedure TFilterField.Clear;
begin
  FieldName := EmptyStr;
  ReadableName := EmptyStr;
  TableAlias := EmptyStr;
  FilterType := tcTexto;
  Criteria := crLike;
  Value1 := EmptyStr;
  Value2 := EmptyStr;
  OpenParenthesis := 1;
  CloseParenthesis := 1;
  AndOr := aoNone;
end;

function TFilterField.ToHTML: String;
const
  ColorAndOr: String = 'clHotLight';
  ColorCriteria: String = '$000F87FF';
  FormatTagFont: String = '<font color="%s">%s</font> ';
  { (deprecated) for use with TNextStyleSheet
  idAndOr: String = 'plus';
  idCriteria: String = 'sinal';
  FormatTagSpan: String = '<span id="%s">%s</span>'; }
var
  OP, CP, CR, AO, VL1: String;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  case AndOr of
    aoNone:
      AO := EmptyStr;
    aoAnd:
      AO := Format(FormatTagFont, [ColorAndOr, Trim(rs_FilterAnd)]);
    aoOr:
      AO := Format(FormatTagFont, [ColorAndOr, Trim(rs_FilterOr)]);
  end;
  case Criteria of
    crLike:
      begin
        if ExecRegExpr('^%[A-Za-z0-9 .,-@]+%$', Value1) then
          CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterLike)])
        else
          CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterStartLike)]);
      end;
    crEqual:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterEqual)]);
    crBetween:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterBetween)]);
    crMoreThan:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterMoreThan)]);
    crLessThan:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterLessThan)]);
    crNull:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterNull)]);
    crNotNull:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterNotNull)]);
  end;
  VL1 := StringReplace(Value1, '%', '', [rfReplaceAll]);
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s%s<i>%s</i> <b>%s</b>%s', [AO, OP, ReadableName, CR, CP])
  else
    if Value2 <> EmptyStr then
      Result := Format('%s%s<i>%s</i> %s <b>%s</b> <font color="%s">%s</font> <b>%s</b>%s',
        [AO, OP, ReadableName, CR, Value1, ColorCriteria, Trim(rs_FilterAnd), Value2, CP])
    else
      Result := Format('%s%s<i>%s</i> %s <b>%s</b>%s', [AO, OP, ReadableName, CR, VL1, CP]);
end;

function TFilterField.ToSQL(aTabela, aLookupField, aKeyField: String): String;
var
  OP, CP, CR, AO, FN, VL1: String;
  v: Integer;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  FN := TableAlias + FieldName;
  case FilterType of
    tcData:
      FN := 'date(' + FN + ')';
    tcHora:
      FN := 'time(' + FN + ')';
    tcDataHora:
      FN := 'datetime(' + FN + ')';
  end;
  case AndOr of
    aoNone:
      AO := '';
    aoAnd:
      AO := 'and';
    aoOr:
      AO := 'or';
  end;
  CR := StrCriteria[Ord(Criteria)];

  if FilterType = tcLookup then
  begin
    with TSQLQuery.Create(DMD.sqlCon) do
    try
      MacroCheck := True;
      DataBase := DMD.sqlCon;
      SQL.Add('select %KEYF from %TABNAME where %LOOKUP = :VLOOK');
      MacroByName('KEYF').AsString := aKeyField;
      MacroByName('TABNAME').AsString := aTabela;
      MacroByName('LOOKUP').AsString := aLookupField;
      ParamByName('VLOOK').AsString := Value1;
      Open;
      v := Fields[0].AsInteger;
      Close;
      VL1 := IntToStr(v);
    finally
      Free;
    end;
  end
  else
    VL1 := Value1;
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s %s%s %s%s', [AO, OP, FN, CR, CP])
  else
    if (Value2 <> '') then
      Result := Format('%s %s%s %s %s and %s%s', [AO, OP, FN, CR, Value1, Value2, CP])
    else
      Result := Format('%s %s%s %s %s%s', [AO, OP, FN, CR, VL1, CP]);
end;

function TFilterField.ToString: String;
var
  OP, CP, CR, AO, VL1: String;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  case AndOr of
    aoNone:
      AO := EmptyStr;
    aoAnd:
      AO := rs_FilterAnd;
    aoOr:
      AO := rs_FilterOr;
  end;
  case Criteria of
    crLike:
      CR := rs_FilterLike;
    crStartLike:
      CR := rs_FilterStartLike;
    crEqual:
      CR := rs_FilterEqual;
    crBetween:
      CR := rs_FilterBetween;
    crMoreThan:
      CR := rs_FilterMoreThan;
    crLessThan:
      CR := rs_FilterLessThan;
    crNull:
      CR := rs_FilterNull;
    crNotNull:
      CR := rs_FilterNotNull;
  end;
  VL1 := StringReplace(Value1, '%', '', [rfReplaceAll]);
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s %s%s %s%s', [AO, OP, ReadableName, CR, CP])
  else
    if Value2 <> '' then
      Result := Format('%s %s%s %s %s e %s%s', [AO, OP, ReadableName, CR, Value1, Value2, CP])
    else
      Result := Format('%s %s%s %s %s%s', [AO, OP, ReadableName, CR, VL1, CP]);
end;

{ TDBParams }

procedure TDBParams.Clear;
begin
  Name := 'XolmisDB';
  Manager := dbSqlite;
  IsLocal := True;
  Database := EmptyStr;
  Server := EmptyStr;
  Port := 0;
  Protocol := EmptyStr;
  OpenMode := EmptyStr;
  CharacterSet := EmptyStr;
  StringFormat := EmptyStr;
  UserName := EmptyStr;
  Password := EmptyStr;
end;

{ TSearch }

procedure TSearch.Clear;
begin
  FieldNames.Clear;
  Criteria := crNone;
  FilterType := tcTexto;
  Value1 := EmptyStr;
  Value2 := EmptyStr;
end;

procedure TSearch.Create;
begin
  FieldNames := TStringList.Create;
end;

procedure TSearch.Free;
begin
  FreeAndNil(FieldNames);
end;

function TSearch.IsEmpty: Boolean;
begin
  Result := FieldNames.Count = 0;
end;

function TSearch.ToSQL: String;
const
  FMaskNull: String = '(%s %s) ';
  FMaskV1: String = '(%s %s %s) ';
  FMaskV2: String = '(%s %s %s and %s) ';
  FMaskDateV1: String = '(date(%s) %s date(%s)) ';
  FMaskDateV2: String = '(date(%s) %s date(%s) and date(%s)) ';
  FMaskTimeV1: String = '(time(%s) %s time(%s)) ';
  FMaskTimeV2: String = '(time(%s) %s time(%s) and time(%s)) ';
  FMaskDateTimeV1: String = '(datetime(%s) %s datetime(%s)) ';
  FMaskDateTimeV2: String = '(datetime(%s) %s datetime(%s) and datetime(%s)) ';
var
  i: Integer;
  V1, V2, S, Msk, Operador: String;
begin
  Result := EmptyStr;

  Operador := StrCriteria[Ord(Criteria)];
  S := 'where ';
  V1 := Trim(Value1);
  V2 := Trim(Value2);
  if Criteria in [crNull, crNotNull] then
    Msk := FMaskNull;
  if V1 <> EmptyStr then
  begin
    case FilterType of
      tcTexto, tcLista, tcLookup:
        begin
          if Pos('+', V1) > 0 then
            V1 := SyllableSearch(V1) + '%'
          else
            V1 := WordSearch(V1) + '%';
          if Criteria = crLike then
            V1 := '%' + V1;
          Msk := FMaskV1;
        end;
      tcBool:
        Msk := FMaskV1;
      tcInteiro, tcDecimal:
        begin
          V1 := StringReplace(V1, ',', '.', [rfReplaceAll]);
          V2 := StringReplace(V2, ',', '.', [rfReplaceAll]);
          if Criteria = crBetween then
            Msk := FMaskV2
          else
            Msk := FMaskV1;
        end;
      tcData:
        begin
          if Criteria = crBetween then
            Msk := FMaskDateV2
          else
            Msk := FMaskDateV1;
        end;
      tcHora:
        begin
          if Criteria = crBetween then
            Msk := FMaskTimeV2
          else
            Msk := FMaskTimeV1;
        end;
      tcDataHora:
        begin
          if Criteria = crBetween then
            Msk := FMaskDateTimeV2
          else
            Msk := FMaskDateTimeV1;
        end;
    end;
  end;

  if FieldNames.Count > 1 then
    S := S + '(';

  for i := 0 to FieldNames.Count - 1 do
  begin
    case Criteria of
      crNone: ;
      crLike, crStartLike, crEqual, crMoreThan, crLessThan:
        S := S + Format(Msk, [FieldNames[i], Operador, V1]);
      crBetween:
        S := S + Format(Msk, [FieldNames[i], Operador, V1, V2]);
      crNull, crNotNull:
        S := S + Format(Msk, [FieldNames[i], Operador]);
    end;

    if i < FieldNames.Count - 1 then
        S := S + 'or ';
  end;

  if FieldNames.Count > 1 then
    S := S + ')';

  Result := S;
end;

function TSearch.ToString: String;
begin
  Result := EmptyStr;

  { TODO TSearch.ToString }
end;

{ TRecordStatus }

procedure TRecordStatus.Clear;
begin
  Status := rsActive;
  Mark := rmAll;
  Queue := rqAll;
  Share := rxAll;
end;

end.

