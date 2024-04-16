unit cbs_datatypes;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Dialogs, DB, SQLDB, Generics.Collections, RegExpr;

type
  TDBManager = (dbSqlite, dbFirebird, dbPostgre, dbMaria);

  { TDBParams }

  TDBParams = record
    Name: String;
    Manager: TDBManager;
    VendorLib: String;
    IsLocal: Boolean;
    Database: String;
    Server: String;
    Port: Integer;
    Protocol: (dptLocal, dptTCPIP, dptNetBEUI, dptSPX);
    OpenMode: (omCreateIfNotExist, omReadWrite, omReadOnly);
    PageSize: Integer;
    CharacterSet: String;
    StringFormat: (sfUnicode, sfAnsi, sfChoose);
    Compress: Boolean;
    GUIDFormat: (idfString, idfBinary);
    GUIDEndian: (ideLittle, ideBig);
    UserName: String;
    Password: String;
    procedure Clear;
    procedure LoadParams;
    function TestConnection: Boolean;
  end;

type
  TTableType = (tbNone,
    tbUsers,
    tbRecordHistory,
    tbGazetteer,
    tbNetStations,
    tbPermanentNets,
    tbInstitutions,
    tbPeople,
    tbProjects,
    tbProjectTeams,
    tbPermits,
    tbTaxonRanks,
    tbZooTaxa,
    tbBotanicTaxa,
    tbBands,
    tbBandHistory,
    tbIndividuals,
    tbCaptures,
    tbMolts,
    tbNests,
    tbNestOwners,
    tbNestRevisions,
    tbEggs,
    tbMethods,
    tbExpeditions,
    tbSurveys,
    tbSurveyTeams,
    tbNetsEffort,
    tbWeatherLogs,
    tbSightings,
    tbSpecimens,
    tbSamplePreps,
    tbSpecimenCollectors,
    tbImages,
    tbAudioLibrary);

const
  TableAliases: array [TTableType] of String = ('',
    'u','rh','g','ns','pn','it','p','pj','pt','l','r','z','bt','b','bh','i','c','m',
    'n','no','nr','e','mt','x','sv','st','ef','wl','s','sp','pp','sc','img','aud');
  TableNames: array [TTableType] of String = ('',
    'users',
    'record_history',
    'gazetteer',
    'net_stations',
    'permanent_nets',
    'institutions',
    'people',
    'projects',
    'project_team',
    'legal',
    'taxon_ranks',
    'zoo_taxa',
    'botanic_taxa',
    'bands',
    'band_history',
    'individuals',
    'captures',
    'molts',
    'nests',
    'nest_owners',
    'nest_revisions',
    'eggs',
    'methods',
    'expeditions',
    'surveys',
    'survey_team',
    'nets_effort',
    'weather_logs',
    'sightings',
    'specimens',
    'sample_preps',
    'specimen_collectors',
    'images',
    'audio_library');

type
  TCriteriaType = (crNone,
    crLike, crStartLike, crEqual, crDistinct,
    crBetween, crMoreThan, crLessThan,
    crNull, crNotNull
  );
  TTableFieldType = (ctString, ctInteger, ctFloat, ctDate, ctTime, ctDateTime, ctBoolean);
  TFilterType = (tcTexto, tcInteiro, tcDecimal, tcData, tcHora, tcDataHora, tcLista, tcBool, tcLookup);
  TSearchDataType = (sdtText, sdtInteger, sdtFloat, sdtDate, sdtTime, sdtDateTime, sdtBoolean, sdtList,
    sdtLookup);
  TFilterValue = (fvNone, fvReset, fvAll, fvMarked, fvUnmarked, fvDeleted, fvQueued);
  TSeparator = (spNone, spSemicolon, spComma, spColon, spPeriod, spPipe, spSlash, spHyphen, spUnderline);
  TSQLAndOr = (aoNone, aoAnd, aoOr);
  TRecordActiveStatus = (rsAll, rsActive, rsInactive, rsNone);

const
  CriteriaOperators: array[TCriteriaType] of String = ('',
    'LIKE', 'LIKE', '=', '!=',
    'BETWEEN', '>=', '<=',
    'ISNULL', 'NOTNULL');
  AndOrStr: array[TSQLAndOr] of String = ('', 'AND', 'OR');
  TiposCampo: array[TTableFieldType] of String = ('String', 'Integer', 'Float', 'Date', 'Time',
    'DateTime', 'Boolean');
  TiposFiltro: array[TFilterType] of String = ('Texto', 'Inteiro', 'Decimal', 'Data', 'Hora',
    'DataHora', 'Lista', 'Bool', 'Lookup');
  SearchDataTypes: array[TFilterType] of String = ('Text', 'Integer', 'Float', 'Date', 'Time',
    'DateTime', 'Boolean', 'List', 'Lookup');
  StrSeparators: array [TSeparator] of Char = (#0, ';', ',', ':', '.', '|', '/', '-', '_');

type

  { TRecordStatus }

  TRecordStatus = record
    Status: TRecordActiveStatus;
    Mark: (rmAll, rmMarked, rmUnmarked);
    Queue: (rqAll, rqQueued, rqUnqueued);
    Share: (rxAll, rxExported, rxNotExported);
    procedure Reset;
  end;

  { TFilterField (deprecated) }

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
    AndOr: TSQLAndOr;
    function ToSQL(aTabela, aLookupField, aKeyField: String): String;
    function ToHTML: String;
    function ToString: String;
    procedure Clear;
  end;

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

  { TMultiSearchField }

  //TMultiSearchField = class(TCustomSearchField)
  //private
  //  FFieldNames: TStrings;
  //  FDisplayNames: TStrings;
  //public
  //  constructor Create;
  //  destructor Destroy; override;
  //published
  //  property FieldNames: String read FFieldNames write FFieldNames;
  //  property DisplayNames: String read FDisplayNames write FDisplayNames;
  //end;

  TSearchFields = specialize TObjectList<TSearchField>;

type
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TSortType = (stNone, stAlfanumeric, stNumeric, stDateTime, stBoolean, stTaxonomic);

  TSortedField = class
    FieldName: String;
    Direction: TSortDirection;
    Collation: String;
    Lookup: Boolean;
  end;

  TSortedFields = specialize TObjectList<TSortedField>;

const
  SortDirections: array [TSortDirection] of String = ('', 'ASC', 'DESC');

type

  { TSearchGroup }

  TSearchGroup = class
  private
    FFields: TSearchFields;
    FAndOr: TSQLAndOr;
  public
    constructor Create(aAndOr: TSQLAndOr = aoOr);
    destructor Destroy; override;
  published
    property Fields: TSearchFields read FFields write FFields;
    property AndOr: TSQLAndOr read FAndOr write FAndOr default aoOr;
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

  { TIndividualSearch }

  TIndividualSearch = class(TCustomSearch)
  private
    FSex: Char;
    FAge: Char;
    FWithRecaptures: Boolean;
    FWithColorBands: Boolean;
    FTaxa: TStrings;
    FDates: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Sex: Char read FSex write FSex;
    property Age: Char read FAge write FAge;
    property WithRecaptures: Boolean read FWithRecaptures write FWithRecaptures;
    property WithColorBands: Boolean read FWithColorBands write FWithColorBands;
    property Taxa: TStrings read FTaxa write FTaxa;
    property Dates: TStrings read FDates write FDates;
  end;


//type

  { TSearch (deprecated) }

  //TSearch = class
  //private
  //  FFieldNames: TStrings;
  //  FFilterType: TFilterType;
  //  FCriteria: TCriteriaType;
  //  FValue1: String;
  //  FValue2: String;
  //  function GetSQLString: String;
  //  function GetReadableString: String;
  //  function GetFilterString: String;
  //public
  //  constructor Create;
  //  destructor Destroy; override;
  //  procedure Clear;
  //  function IsEmpty: Boolean;
  //published
  //  property FieldNames: TStrings read FFieldNames write FFieldNames;
  //  property FilterType: TFilterType read FFilterType write FFilterType;
  //  property Criteria: TCriteriaType read FCriteria write FCriteria;
  //  property Value1: String read FValue1 write FValue1;
  //  property Value2: String read FValue2 write FValue2;
  //  property SQLString: String read GetSQLString;
  //  property FilterString: String read GetFilterString;
  //  property ReadableString: String read GetReadableString;
  //end;

type

  { TTableField }

  TTableField = class
    FieldName: String;
    DisplayName: String;
    IntegerKey: Boolean;
    TextualKey: Boolean;
    DarwinCoreName: String;
    DataType: TTableFieldType;
    FilterType: TFilterType;
    LookupTable: String;
    LookupKey: String;
    LookupResult: String;
    LookupName: String;
    MinValue: Extended;
    MaxValue: Extended;
    ValueList: String;
    Visible: Boolean;
    Sorted: Boolean;
    procedure GetData(aTable: TTableType; aFieldName: String = '');
  end;

  TTableFields = specialize TObjectList<TTableField>;

type

  { TTableInfo }

  TTableInfo = class
  private
    FTableName: String;
    FDisplayName: String;
    FVisible: Boolean;
    FShowExport: Boolean;
    FShowImport: Boolean;
    FShowFilter: Boolean;
    FFields: TTableFields;
    procedure GetData(aTableType: TTableType); overload;
    procedure GetData(aDisplayName: String); overload;
    procedure GetFields(AlphaSort: Boolean);
  public
    constructor Create; overload;
    constructor Create(aTableType: TTableType); overload;
    destructor Destroy; override;
  published
    property TableName: String read FTableName write FTableName;
    property DisplayName: String read FDisplayName write FDisplayName;
    property Visible: Boolean read FVisible write FVisible;
    property ShowExport: Boolean read FShowExport write FShowExport;
    property ShowImport: Boolean read FShowImport write FShowImport;
    property ShowFilter: Boolean read FShowFilter write FShowFilter;
    property Fields: TTableFields read FFields write FFields;
  end;

var
  UID: TGUID;

  function CampoByName(const aCampoName: String): TTableFieldType;
  function FilterByName(const aFilterName: String): TFilterType;

implementation

uses cbs_locale, cbs_global, cbs_conversions, cbs_datasearch, cbs_dialogs, udm_main;

function CampoByName(const aCampoName: String): TTableFieldType;
var
  i: Integer;
begin
  Result := ctString;
  for i := 0 to Ord(High(TTableFieldType)) do
  begin
    if TiposCampo[TTableFieldType(i)] = aCampoName then
    begin
      Result := TTableFieldType(i);
      Break;
    end;
  end;
end;

function FilterByName(const aFilterName: String): TFilterType;
var
  i: Integer;
begin
  Result := tcTexto;
  for i := 0 to Ord(High(TFilterType)) do
  begin
    if TiposFiltro[TFilterType(i)] = aFilterName then
    begin
      Result := TFilterType(i);
      Break;
    end;
  end;
end;

{ TTableInfo }

constructor TTableInfo.Create(aTableType: TTableType);
begin
  GetData(aTableType);
  FFields := TTableFields.Create;
  GetFields(False);
end;

constructor TTableInfo.Create;
begin
  FFields := TTableFields.Create;
end;

destructor TTableInfo.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TTableInfo.GetData(aDisplayName: String);
begin
  with TSQLQuery.Create(DMM.sqlCon), SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM tables_mapping');
    Add('WHERE display_name = :tabname');
    ParamByName('TABNAME').AsString := aDisplayName;
    Open;
    if RecordCount > 0 then
    begin
      FTableName := FieldByName('table_name').AsString;
      FDisplayName := FieldByName('display_name').AsString;
      FVisible := FieldByName('visible_status').AsBoolean;
      FShowExport := FieldByName('export_show').AsBoolean;
      FShowImport := FieldByName('import_show').AsBoolean;
      FShowFilter := FieldByName('filter_show').AsBoolean;
    end;
    Close;
  finally
    Free;
  end;
end;

procedure TTableInfo.GetData(aTableType: TTableType);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM tables_mapping');
    Add('WHERE table_name = :tabname');
    ParamByName('TABNAME').AsString := TableNames[aTableType];
    Open;
    if RecordCount > 0 then
    begin
      FTableName := FieldByName('table_name').AsString;
      FDisplayName := FieldByName('display_name').AsString;
      FVisible := FieldByName('visible_status').AsBoolean;
      FShowExport := FieldByName('export_show').AsBoolean;
      FShowImport := FieldByName('import_show').AsBoolean;
      FShowFilter := FieldByName('filter_show').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTableInfo.GetFields(AlphaSort: Boolean);
var
  i: Integer;
  Lst: TStringList;
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM fields_mapping');
    Add('WHERE table_name = :tabname');
    if AlphaSort then
      Add('ORDER BY field_name ASC')
    else
      Add('ORDER BY sort_num ASC');
    ParamByName('TABNAME').AsString := TableName;
    Open;
    if RecordCount > 0 then
    begin
      First;
      FFields.Clear;
      for i := 0 to RecordCount - 1 do
      begin
        FFields.Add(TTableField.Create);
        FFields.Items[i].FieldName := FieldByName('field_name').AsString;
        FFields.Items[i].DisplayName := FieldByName('display_name').AsString;
        FFields.Items[i].IntegerKey := FieldByName('integer_key').AsBoolean;
        FFields.Items[i].TextualKey := FieldByName('text_key').AsBoolean;
        FFields.Items[i].DarwinCoreName := FieldByName('darwin_core_name').AsString;
        FFields.Items[i].DataType := CampoByName(FieldByName('field_type').AsString);
        FFields.Items[i].FilterType := FilterByName(FieldByName('filter_type').AsString);
        FFields.Items[i].LookupTable := FieldByName('lookup_table').AsString;
        FFields.Items[i].LookupKey := FieldByName('lookup_key').AsString;
        FFields.Items[i].LookupResult := FieldByName('lookup_result').AsString;
        FFields.Items[i].LookupName := FieldByName('lookup_name').AsString;
        FFields.Items[i].MinValue := FieldByName('minimum_value').AsFloat;
        FFields.Items[i].MaxValue := FieldByName('maximum_value').AsFloat;
        if Trim(FieldByName('value_list').AsString) <> EmptyStr then
        try
          Lst := TStringList.Create;
          Lst.Text := FieldByName('value_list').AsString;
          FFields.Items[i].ValueList := Lst.CommaText;
        finally
          FreeAndNil(Lst);
        end;
        FFields.Items[i].Visible := FieldByName('visible_status').AsBoolean;
        FFields.Items[i].Sorted := FieldByName('sorted_status').AsBoolean;
        Next;
      end;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSearch }

//constructor TSearch.Create;
//begin
//  FFieldNames := TStringList.Create;
//end;
//
//destructor TSearch.Destroy;
//begin
//  FreeAndNil(FFieldNames);
//  inherited Destroy;
//end;
//
//function TSearch.GetSQLString: String;
//const
//  FMaskNull: String = '(%s %s) ';
//  FMaskV1: String = '(%s %s %s) ';
//  FMaskV2: String = '(%s %s %s AND %s) ';
//  FMaskDateV1: String = '(date(%s) %s date(%s)) ';
//  FMaskDateV2: String = '(date(%s) %s date(%s) AND date(%s)) ';
//  FMaskTimeV1: String = '(time(%s) %s time(%s)) ';
//  FMaskTimeV2: String = '(time(%s) %s time(%s) AND time(%s)) ';
//  FMaskDateTimeV1: String = '(datetime(%s) %s datetime(%s)) ';
//  FMaskDateTimeV2: String = '(datetime(%s) %s datetime(%s) AND datetime(%s)) ';
//var
//  i: Integer;
//  V1, V2, S, Msk, Operador: String;
//begin
//  Result := EmptyStr;
//  Msk := FMaskV1;
//
//  Operador := CriteriaOperators[Criteria];
//  S := 'WHERE ';
//  V1 := Trim(Value1);
//  V2 := Trim(Value2);
//  if Criteria in [crNull, crNotNull] then
//    Msk := FMaskNull;
//  if V1 <> EmptyStr then
//  begin
//    case FilterType of
//      tcTexto, tcLista, tcLookup:
//        begin
//          if Pos('+', V1) > 0 then
//            V1 := WildcardSyllables(V1) + '%'
//          else
//            V1 := WildcardWords(V1) + '%';
//          if Criteria = crLike then
//            V1 := '%' + V1;
//          V1 := QuotedStr(V1);
//          Msk := FMaskV1;
//        end;
//      tcBool:
//        Msk := FMaskV1;
//      tcInteiro, tcDecimal:
//        begin
//          V1 := StringReplace(V1, ',', '.', [rfReplaceAll]);
//          V2 := StringReplace(V2, ',', '.', [rfReplaceAll]);
//          if Criteria = crBetween then
//            Msk := FMaskV2
//          else
//            Msk := FMaskV1;
//        end;
//      tcData:
//        begin
//          if Criteria = crBetween then
//            Msk := FMaskDateV2
//          else
//            Msk := FMaskDateV1;
//        end;
//      tcHora:
//        begin
//          if Criteria = crBetween then
//            Msk := FMaskTimeV2
//          else
//            Msk := FMaskTimeV1;
//        end;
//      tcDataHora:
//        begin
//          if Criteria = crBetween then
//            Msk := FMaskDateTimeV2
//          else
//            Msk := FMaskDateTimeV1;
//        end;
//    end;
//  end;
//
//  if FieldNames.Count > 1 then
//    S := S + '(';
//
//  for i := 0 to FieldNames.Count - 1 do
//  begin
//    case Criteria of
//      crNone: ;
//      crLike, crStartLike, crEqual, crMoreThan, crLessThan:
//        S := S + Format(Msk, [FieldNames[i], Operador, V1]);
//      crBetween:
//        S := S + Format(Msk, [FieldNames[i], Operador, V1, V2]);
//      crNull, crNotNull:
//        S := S + Format(Msk, [FieldNames[i], Operador]);
//    end;
//
//    if i < FieldNames.Count - 1 then
//        S := S + 'OR ';
//  end;
//
//  if FieldNames.Count > 1 then
//    S := S + ')';
//
//  Result := S;
//end;
//
//function TSearch.GetReadableString: String;
//begin
//  Result := EmptyStr;
//
//  { #TODO: TSearch.GetReadableString }
//end;
//
//function TSearch.GetFilterString: String;
//const
//  FMaskNull: String = '(%s %s) ';
//  FMaskV1: String = '(%s %s %s) ';
//  FMaskV2: String = '(%s %s %s AND %s) ';
//  FMaskDateV1: String = '(date(%s) %s date(%s)) ';
//  FMaskDateV2: String = '(date(%s) %s date(%s) AND date(%s)) ';
//  FMaskTimeV1: String = '(time(%s) %s time(%s)) ';
//  FMaskTimeV2: String = '(time(%s) %s time(%s) AND time(%s)) ';
//  FMaskDateTimeV1: String = '(datetime(%s) %s datetime(%s)) ';
//  FMaskDateTimeV2: String = '(datetime(%s) %s datetime(%s) AND datetime(%s)) ';
//var
//  i: Integer;
//  V1, V2, S, Msk, Operador: String;
//begin
//  Result := EmptyStr;
//  Msk := FMaskV1;
//
//  Operador := CriteriaOperators[Criteria];
//  S := '';
//  V1 := Trim(Value1);
//  V2 := Trim(Value2);
//  if Criteria in [crNull, crNotNull] then
//    Msk := FMaskNull;
//  if V1 <> EmptyStr then
//  begin
//    case FilterType of
//      tcTexto, tcLista, tcLookup:
//        begin
//          if Pos('+', V1) > 0 then
//            V1 := WildcardSyllables(V1, '*') + '*'
//          else
//            V1 := WildcardWords(V1, '*') + '*';
//          if Criteria = crLike then
//            V1 := '*' + V1;
//          V1 := QuotedStr(V1);
//          Operador := '=';
//          Msk := FMaskV1;
//        end;
//      tcBool:
//        Msk := FMaskV1;
//      tcInteiro, tcDecimal:
//        begin
//          V1 := StringReplace(V1, ',', '.', [rfReplaceAll]);
//          V2 := StringReplace(V2, ',', '.', [rfReplaceAll]);
//          if Criteria = crBetween then
//            Msk := FMaskV2
//          else
//            Msk := FMaskV1;
//        end;
//      tcData:
//        begin
//          if Criteria = crBetween then
//            Msk := FMaskDateV2
//          else
//            Msk := FMaskDateV1;
//        end;
//      tcHora:
//        begin
//          if Criteria = crBetween then
//            Msk := FMaskTimeV2
//          else
//            Msk := FMaskTimeV1;
//        end;
//      tcDataHora:
//        begin
//          if Criteria = crBetween then
//            Msk := FMaskDateTimeV2
//          else
//            Msk := FMaskDateTimeV1;
//        end;
//    end;
//  end;
//
//  if FieldNames.Count > 1 then
//    S := S + '(';
//
//  for i := 0 to FieldNames.Count - 1 do
//  begin
//    case Criteria of
//      crNone: ;
//      crLike, crStartLike, crEqual, crMoreThan, crLessThan:
//        S := S + Format(Msk, [FieldNames[i], Operador, V1]);
//      crBetween:
//        S := S + Format(Msk, [FieldNames[i], Operador, V1, V2]);
//      crNull, crNotNull:
//        S := S + Format(Msk, [FieldNames[i], Operador]);
//    end;
//
//    if i < FieldNames.Count - 1 then
//        S := S + 'OR ';
//  end;
//
//  if FieldNames.Count > 1 then
//    S := S + ')';
//
//  Result := S;
//end;
//
//procedure TSearch.Clear;
//begin
//  FieldNames.Clear;
//  Criteria := crNone;
//  FilterType := tcTexto;
//  Value1 := EmptyStr;
//  Value2 := EmptyStr;
//end;
//
//function TSearch.IsEmpty: Boolean;
//begin
//  Result := FieldNames.Count = 0;
//end;

{ TTableField }

procedure TTableField.GetData(aTable: TTableType; aFieldName: String);
var
  Qry: TSQLQuery;
begin
  if (aFieldName = EmptyStr) and (FieldName = EmptyStr) then
  begin
    raise Exception.Create(rsErrorEmptyFieldName);
    Exit;
  end;
  FieldName := aFieldName;

  Qry := TSQLQuery.Create(DMM.sysCon);
  with Qry, SQL do
  try
    DataBase := DMM.sysCon;
    Clear;
    Add('SELECT * FROM fields_mapping');
    Add('WHERE (table_name = :atable) AND (field_name = :afield)');
    ParamByName('ATABLE').AsString := TableNames[aTable];
    ParamByName('AFIELD').AsString := FieldName;
    Open;
    if RecordCount > 0 then
    begin
      DisplayName := FieldByName('display_name').AsString;
      IntegerKey := FieldByName('integer_key').AsBoolean;
      TextualKey := FieldByName('text_key').AsBoolean;
      DarwinCoreName := FieldByName('darwin_core_name').AsString;
      DataType := CampoByName(FieldByName('field_type').AsString);
      FilterType := FilterByName(FieldByName('filter_type').AsString);;
      LookupTable := FieldByName('lookup_table').AsString;
      LookupKey := FieldByName('lookup_key').AsString;
      LookupResult := FieldByName('lookup_result').AsString;
      LookupName := FieldByName('lookup_name').AsString;
      MinValue := FieldByName('minimum_value').AsFloat;
      MaxValue := FieldByName('maximum_value').AsFloat;
      ValueList := FieldByName('value_list').AsString;
      Visible := FieldByName('visible_status').AsBoolean;
      Sorted := FieldByName('sorted_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TFilterField }

function TFilterField.ToSQL(aTabela, aLookupField, aKeyField: String): String;
var
  OP, CP, CR, AO, FN, VL1: String;
  v: Integer;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  case FilterType of
    tcData:
      FN := Format('date(%s)', [TableAlias + FieldName]);
    tcHora:
      FN := Format('time(%s)', [TableAlias + FieldName]);
    tcDataHora:
      FN := Format('datetime(%s)', [TableAlias + FieldName]);
  else
    FN := TableAlias + FieldName;
  end;
  case AndOr of
    aoNone:
      AO := '';
    aoAnd:
      AO := 'AND';
    aoOr:
      AO := 'OR';
  end;
  CR := CriteriaOperators[Criteria];

  if FilterType = tcLookup then
  begin
    with TSQLQuery.Create(DMM.sqlCon) do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      SQL.Add('SELECT %keyf FROM %tabname WHERE %lookup = :vlook');
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

function TFilterField.ToHTML: String;
const
  ColorAndOr: String = 'clHotLight';
  ColorCriteria: String = '$000F87FF';
  FormatTagFont: String = '<font color="%s">%s</font> ';
var
  OP, CP, CR, AO, VL1: String;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  case AndOr of
    aoNone:
      AO := EmptyStr;
    aoAnd:
      AO := Format(FormatTagFont, [ColorAndOr, Trim(rsFilterAnd)]);
    aoOr:
      AO := Format(FormatTagFont, [ColorAndOr, Trim(rsFilterOr)]);
  end;
  case Criteria of
    crLike:
      begin
        if ExecRegExpr('^%[A-Za-z0-9 .,-@]+%$', Value1) then
          CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterLike)])
        else
          CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterStartLike)]);
      end;
    crEqual:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterEqual)]);
    crBetween:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterBetween)]);
    crMoreThan:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterMoreThan)]);
    crLessThan:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterLessThan)]);
    crNull:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterNull)]);
    crNotNull:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rsFilterNotNull)]);
  end;
  VL1 := StringReplace(Value1, '%', '', [rfReplaceAll]);
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s%s<i>%s</i> <b>%s</b>%s', [AO, OP, ReadableName, CR, CP])
  else
    if Value2 <> EmptyStr then
      Result := Format('%s%s<i>%s</i> %s <b>%s</b> <font color="%s">%s</font> <b>%s</b>%s',
        [AO, OP, ReadableName, CR, Value1, ColorCriteria, Trim(rsFilterAnd), Value2, CP])
    else
      Result := Format('%s%s<i>%s</i> %s <b>%s</b>%s', [AO, OP, ReadableName, CR, VL1, CP]);
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
      AO := rsFilterAnd;
    aoOr:
      AO := rsFilterOr;
  end;
  case Criteria of
    crLike:
      CR := rsFilterLike;
    crStartLike:
      CR := rsFilterStartLike;
    crEqual:
      CR := rsFilterEqual;
    crBetween:
      CR := rsFilterBetween;
    crMoreThan:
      CR := rsFilterMoreThan;
    crLessThan:
      CR := rsFilterLessThan;
    crNull:
      CR := rsFilterNull;
    crNotNull:
      CR := rsFilterNotNull;
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

constructor TSearchGroup.Create(aAndOr: TSQLAndOr);
begin
  FFields := TSearchFields.Create(True);
  FAndOr := aAndOr;
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
          if aCriteriaType in [crLike, crStartLike] then
          begin
            if ExecRegExpr('^.+\+.+$', aValue1) then
              aValue1 := WildcardSyllables(aValue1) + '%'
            else
              aValue1 := WildcardWords(aValue1) + '%';

            if aCriteriaType = crLike then
              aValue1 := '%' + aValue1;
          end;

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
    FDataSet.SQL.Add(AndOrWhere + '(');
    // Iterate groups
    for i := 0 to (FQuickFilters.Count - 1) do
    begin
      if FQuickFilters[i].Fields.Count > 0 then
      begin
        FDataSet.SQL.Add('(');

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
              crLike, crStartLike, crEqual, crDistinct, crMoreThan, crLessThan:
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
          end;

          // Close parenthesis, and AND/OR
          if f < (FQuickFilters[i].Fields.Count - 1) then
          begin
            AndOrWhere := AndOrStr[FQuickFilters[i].AndOr] + ' ';
            S := S + AndOrWhere;
          end;

          FDataSet.SQL.Add(S);
        end;
        // Close parenthesis, and AND/OR
        AndOrWhere := 'OR ';
        if i < (FQuickFilters.Count - 1) then
          FDataSet.SQL.Add(') ' + AndOrWhere)
        else
          FDataSet.SQL.Add(')');
      end;
    end;
    FDataSet.SQL.Add(')');
    AndOrWhere := 'AND ';
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
              crLike, crStartLike, crEqual, crDistinct, crMoreThan, crLessThan:
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
            begin
              AndOrWhere := AndOrStr[FFields[i].AndOr] + ' ';
              S := S + AndOrWhere;
            end;
          end;

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
      if (FSortFields[i].Lookup) or (FTableAlias = EmptyStr) then
        aSort := aSort + FSortFields[i].FieldName
      else
        aSort := aSort + FTableAlias + '.' + FSortFields[i].FieldName;

      // Collation
      if FSortFields[i].Collation <> EmptyStr then
        aSort := aSort + ' COLLATE ' + FSortFields[i].Collation;

      // Direction
      aSort := aSort + ' ' + SortDirections[FSortFields[i].Direction];

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
  if not Assigned(FDataSet) then
    Exit;

  if FDataSet.Active then
    FDataSet.Close;

  GetSQLString;

  FDataSet.Open;
  Result := FDataSet.RecordCount;
end;

{ TIndividualSearch }

constructor TIndividualSearch.Create;
begin
  FTaxa := TStringList.Create;
  FDates := TStringList.Create;
end;

destructor TIndividualSearch.Destroy;
begin
  inherited Destroy;
  FTaxa.Free;
  FDates.Free;
end;

{ TRecordStatus }

procedure TRecordStatus.Reset;
begin
  Status := rsActive;
  Mark := rmAll;
  Queue := rqAll;
  Share := rxAll;
end;

{ TDBParams }

procedure TDBParams.Clear;
begin
  Name := EmptyStr;
  Manager := dbSqlite;
  VendorLib := 'sqlite3.dll';
  IsLocal := True;
  Database := EmptyStr;
  Server := EmptyStr;
  Port := 0;
  Protocol := dptLocal;
  OpenMode := omCreateIfNotExist;
  PageSize := 0;
  CharacterSet := EmptyStr;
  StringFormat := sfUnicode;
  Compress := False;
  GUIDFormat := idfString;
  GUIDEndian := ideLittle;
  UserName := EmptyStr;
  Password := EmptyStr;
end;

procedure TDBParams.LoadParams;
var
  C: TDataSet;
begin
  if Name = EmptyStr then
    Exit;

  C := DMM.qsConn;
  if not C.Active then
    C.Open;

  if C.Locate('connection_name', Name, [loCaseInsensitive]) then
  begin
    Manager := TDBManager(C.FieldByName('database_type').AsInteger);
    Database := C.FieldByName('database_name').AsString;
    Server := C.FieldByName('database_server').AsString;
    Port := C.FieldByName('database_port').AsInteger;
    UserName := C.FieldByName('user_name').AsString;
    Password := C.FieldByName('user_password').AsString;
    IsLocal := Server = 'localhost';
    case Manager of
      dbSqlite:
        begin
          VendorLib := 'sqlite3.dll';
          OpenMode := omCreateIfNotExist;
          PageSize := 4096;
          StringFormat := sfUnicode;
          GUIDFormat := idfString;
        end;
      dbFirebird:
        begin
          VendorLib := 'fbclient.dll';
          if IsLocal then
            Protocol := dptLocal
          else
            Protocol := dptTCPIP;
          OpenMode := omCreateIfNotExist;
          PageSize := 8192;
          CharacterSet := 'UTF8';
        end;
      dbPostgre:
        begin
          VendorLib := 'libpq.dll';
          CharacterSet := 'UTF8';
        end;
      dbMaria:
        begin
          VendorLib := 'libmysql.dll';
          CharacterSet := 'utf8';
        end;
    end;
  end else
  begin
    raise Exception.CreateFmt(rsErrorConnectionNotFound, [Name]);
  end;

  C.Close;
end;

function TDBParams.TestConnection: Boolean;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
  aMsg: String;
begin
  Result := False;

  uCon := TSQLConnector.Create(nil);
  try
    LoadDatabaseParams(Self.Name, uCon);

    try
      uTrans := TSQLTransaction.Create(uCon);
      uTrans.Action := caRollbackRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      //uTrans.StartTransaction;
      Result := uCon.Connected;
      //uTrans.RollbackRetaining;
    except
      Result := False;
      //uTrans.RollbackRetaining;
      //raise Exception.Create(rsErrorConnectingDatabase);
    end;
  finally
    //if uCon.Connected then
    //  uCon.Close;
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;

  //if Result then
  //  MsgDlg(rsTitleConnectionTest, rsSuccessfulConnectionTest, mtInformation);
end;

end.

