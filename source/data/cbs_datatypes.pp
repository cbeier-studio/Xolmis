{ Xolmis Data Types library

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

unit cbs_datatypes;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Dialogs, DB, SQLDB, Generics.Collections, RegExpr, StrUtils, fgl;

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
    LastBackup: TDateTime;
    procedure Clear;
    procedure LoadParams;
    procedure Optimize;
    procedure SetLastBackup;
    function TestConnection: Boolean;
    procedure Vacuum;
  end;

type
  TTableType = (tbNone,
    tbUsers,
    tbRecordHistory,
    tbRecordVerifications,
    tbGazetteer,
    tbSamplingPlots,
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
    tbAudioLibrary,
    tbDocuments,
    tbVegetation);

  TTablesDictionary = specialize TFPGMap<String, TTableType>;
  TLocaleTablesDictionary = specialize TFPGMap<TTableType, String>;

const
  TableAliases: array [TTableType] of String = ('',
    'u','rh','rv','g','pl','pn','it','p','pj','pt','l','r','z','bt','b','bh','i','c','m',
    'n','no','nr','e','mt','x','sv','st','ef','wl','s','sp','pp','sc','img','snd','doc','veg');
  TableNames: array [TTableType] of String = ('',
    'users',
    'record_history',
    'record_verifications',
    'gazetteer',
    'sampling_plots',
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
    'audio_library',
    'documents',
    'vegetation');

type
  TCriteriaType = (crNone,
    crLike, crStartLike, crEqual, crNotEqual,
    crBetween, crMoreThan, crLessThan,
    crNull, crNotNull
  );
  TTableFieldType = (ctString, ctInteger, ctFloat, ctDate, ctTime, ctDateTime, ctBoolean);
  TSearchDataType = (sdtText, sdtInteger, sdtFloat, sdtDate, sdtTime, sdtDateTime, sdtBoolean, sdtList,
    sdtLookup, sdtYear, sdtMonthYear);
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
  SearchDataTypes: array[TSearchDataType] of String = ('Text', 'Integer', 'Float', 'Date', 'Time',
    'DateTime', 'Boolean', 'List', 'Lookup', 'Year', 'MonthYear');
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
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TSortType = (stNone, stAlphanumeric, stNumeric, stDateTime, stBoolean, stTaxonomic);

  TSortedField = class
    FieldName: String;
    SortType: TSortType;
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
    FSQLWhere: TStrings;
    FSQLOrderBy: String;
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
    property SQLWhere: TStrings read FSQLWhere;
    property SQLOrderBy: String read FSQLOrderBy;
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

type

  { TTableField }

  TTableField = class
    FieldName: String;
    DisplayName: String;
    IntegerKey: Boolean;
    TextualKey: Boolean;
    DarwinCoreName: String;
    DataType: TTableFieldType;
    FilterType: TSearchDataType;
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
  TablesDict: TTablesDictionary;
  LocaleTablesDict: TLocaleTablesDictionary;

  procedure LoadTablesDict;
  procedure LoadLocaleTablesDict;

  function CampoByName(const aCampoName: String): TTableFieldType;
  function SearchTypeByName(const aFilterName: String): TSearchDataType;

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

function SearchTypeByName(const aFilterName: String): TSearchDataType;
var
  i: Integer;
begin
  Result := sdtText;

  for i := 0 to Ord(High(TSearchDataType)) do
  begin
    if SearchDataTypes[TSearchDataType(i)] = aFilterName then
    begin
      Result := TSearchDataType(i);
      Break;
    end;
  end;
end;

procedure LoadTablesDict;
begin
  if not Assigned(TablesDict) then
    TablesDict := TTablesDictionary.Create;

  TablesDict.Clear;

  TablesDict.Add(rsTitleUsers, tbUsers);
  //tbRecordHistory,
  //tbRecordVerifications,
  TablesDict.Add(rsTitleGazetteer, tbGazetteer);
  TablesDict.Add(rsTitleSamplingPlots, tbSamplingPlots);
  TablesDict.Add(rsTitlePermanentNets, tbPermanentNets);
  TablesDict.Add(rsTitleInstitutions, tbInstitutions);
  TablesDict.Add(rsTitleResearchers, tbPeople);
  TablesDict.Add(rsTitleProjects, tbProjects);
  TablesDict.Add(rsTitleProjectMembers, tbProjectTeams);
  TablesDict.Add(rsTitlePermits, tbPermits);
  TablesDict.Add(rsTitleBotanicTaxa, tbBotanicTaxa);
  TablesDict.Add(rsTitleTaxonRanks, tbTaxonRanks);
  TablesDict.Add(rsTitleZooTaxa, tbZooTaxa);
  TablesDict.Add(rsTitleBands, tbBands);
  //tbBandHistory,
  TablesDict.Add(rsTitleIndividuals, tbIndividuals);
  TablesDict.Add(rsTitleCaptures, tbCaptures);
  TablesDict.Add(rsTitleMolts, tbMolts);
  TablesDict.Add(rsTitleNests, tbNests);
  TablesDict.Add(rsTitleNestOwners, tbNestOwners);
  TablesDict.Add(rsTitleNestRevisions, tbNestRevisions);
  TablesDict.Add(rsTitleEggs, tbEggs);
  TablesDict.Add(rsTitleMethods, tbMethods);
  TablesDict.Add(rsCaptionExpeditions, tbExpeditions);
  TablesDict.Add(rsTitleSurveys, tbSurveys);
  TablesDict.Add(rsTitleSurveyTeam, tbSurveyTeams);
  TablesDict.Add(rsTitleNetsEffort, tbNetsEffort);
  TablesDict.Add(rsTitleWeather, tbWeatherLogs);
  TablesDict.Add(rsTitleSightings, tbSightings);
  TablesDict.Add(rsTitleSpecimens, tbSpecimens);
  TablesDict.Add(rsTitleCollectors, tbSpecimenCollectors);
  TablesDict.Add(rsTitleSamplePreps, tbSamplePreps);
  TablesDict.Add(rsTitleVegetation, tbVegetation);
  //tbImages,
  //tbAudioLibrary,
  //tbDocuments,
end;

procedure LoadLocaleTablesDict;
begin
  if not Assigned(LocaleTablesDict) then
    LocaleTablesDict := TLocaleTablesDictionary.Create;

  LocaleTablesDict.Clear;

  LocaleTablesDict.Add(tbUsers, rsTitleUsers);
  //tbRecordHistory,
  //tbRecordVerifications,
  LocaleTablesDict.Add(tbGazetteer, rsTitleGazetteer);
  LocaleTablesDict.Add(tbSamplingPlots, rsTitleSamplingPlots);
  LocaleTablesDict.Add(tbPermanentNets, rsTitlePermanentNets);
  LocaleTablesDict.Add(tbInstitutions, rsTitleInstitutions);
  LocaleTablesDict.Add(tbPeople, rsTitleResearchers);
  LocaleTablesDict.Add(tbProjects, rsTitleProjects);
  LocaleTablesDict.Add(tbProjectTeams, rsTitleProjectMembers);
  LocaleTablesDict.Add(tbPermits, rsTitlePermits);
  LocaleTablesDict.Add(tbBotanicTaxa, rsTitleBotanicTaxa);
  LocaleTablesDict.Add(tbTaxonRanks, rsTitleTaxonRanks);
  LocaleTablesDict.Add(tbZooTaxa, rsTitleZooTaxa);
  LocaleTablesDict.Add(tbBands, rsTitleBands);
  //tbBandHistory,
  LocaleTablesDict.Add(tbIndividuals, rsTitleIndividuals);
  LocaleTablesDict.Add(tbCaptures, rsTitleCaptures);
  LocaleTablesDict.Add(tbMolts, rsTitleMolts);
  LocaleTablesDict.Add(tbNests, rsTitleNests);
  LocaleTablesDict.Add(tbNestOwners, rsTitleNestOwners);
  LocaleTablesDict.Add(tbNestRevisions, rsTitleNestRevisions);
  LocaleTablesDict.Add(tbEggs, rsTitleEggs);
  LocaleTablesDict.Add(tbMethods, rsTitleMethods);
  LocaleTablesDict.Add(tbExpeditions, rsCaptionExpeditions);
  LocaleTablesDict.Add(tbSurveys, rsTitleSurveys);
  LocaleTablesDict.Add(tbSurveyTeams, rsTitleSurveyTeam);
  LocaleTablesDict.Add(tbNetsEffort, rsTitleNetsEffort);
  LocaleTablesDict.Add(tbWeatherLogs, rsTitleWeather);
  LocaleTablesDict.Add(tbSightings, rsTitleSightings);
  LocaleTablesDict.Add(tbSpecimens, rsTitleSpecimens);
  LocaleTablesDict.Add(tbSpecimenCollectors, rsTitleCollectors);
  LocaleTablesDict.Add(tbSamplePreps, rsTitleSamplePreps);
  LocaleTablesDict.Add(tbVegetation, rsTitleVegetation);
  //tbImages,
  //tbAudioLibrary,
  //tbDocuments,
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
        FFields.Items[i].FilterType := SearchTypeByName(FieldByName('filter_type').AsString);
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
      FilterType := SearchTypeByName(FieldByName('filter_type').AsString);;
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
  FSQLWhere := TStringList.Create;
end;

destructor TCustomSearch.Destroy;
begin
  FSQLWhere.Clear;
  FFields.Clear;
  FQuickFilters.Clear;
  FSortFields.Clear;
  FSQLWhere.Free;
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
  MaskYearV1: String = '(strftime(''%%Y'', %s) %s %s)';
  MaskYearV2: String = '(strftime(''%%Y'', %s) %s %s AND %s)';
  MaskMonthYearV1: String = '(strftime(''%%Y-%%m'', %s) %s %s)';
  MaskMonthYearV2: String = '(strftime(''%%Y-%%m'', %s) %s %s AND %s)';
var
  i, f: Integer;
  S, AndOrWhere, Msk, aSort: String;
  V1, V2: String;

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
      sdtYear:
        begin
          if aCriteriaType = crBetween then
            Result := MaskYearV2
          else
            Result := MaskYearV1;
        end;
      sdtMonthYear:
        begin
          if aCriteriaType = crBetween then
            Result := MaskMonthYearV2
          else
            Result := MaskMonthYearV1;
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
      sdtYear: ;
      sdtMonthYear:
        begin
          //aValue1 := ExtractDelimited(2, aValue1, ['/']) + '-' + ExtractDelimited(1, aValue1, ['/']);
          aValue1 := QuotedStr(aValue1);

          if aCriteriaType = crBetween then
          begin
            //aValue2 := ExtractDelimited(2, aValue2, ['/']) + '-' + ExtractDelimited(1, aValue2, ['/']);
            aValue2 := QuotedStr(aValue2);
          end;
        end;
    end;
  end;

begin
  Result := EmptyStr;
  Msk := MaskV1;

  FDataSet.SQL.Clear;
  FDataSet.RefreshSQL.Clear;
  FSQLWhere.Clear;
  FSQLOrderBy := EmptyStr;

  // Add SELECT ... FROM ... statements
  SetSelectSQL(FDataSet.SQL, FTableType, FTableAlias);
  AndOrWhere := 'WHERE ';

  // Add fields in WHERE clause
  if FQuickFilters.Count > 0 then
  begin
    FDataSet.SQL.Add(AndOrWhere + '(');
    FSQLWhere.Add(AndOrWhere + '(');
    // Iterate groups
    for i := 0 to (FQuickFilters.Count - 1) do
    begin
      if FQuickFilters[i].Fields.Count > 0 then
      begin
        FDataSet.SQL.Add('(');
        FSQLWhere.Add('(');

        // Iterate group fields
        for f := 0 to (FQuickFilters[i].Fields.Count - 1) do
        begin
          S := EmptyStr;
          with FQuickFilters[i].Fields[f] do
          begin
            V1 := FValue1;
            V2 := FValue2;
            PrepareValues(FCriteria, FDataType, V1, V2);
            Msk := GetValueMask(FCriteria, FDataType);

            // Fieldname, criteria, and values
            case FCriteria of
              crLike, crStartLike, crEqual, crNotEqual, crMoreThan, crLessThan:
              begin
                if FQuickFilters[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FFieldName, CriteriaOperators[FCriteria], V1])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FFieldName, CriteriaOperators[FCriteria], V1]);
              end;
              crBetween:
              begin
                if FQuickFilters[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FFieldName, CriteriaOperators[FCriteria], V1, V2])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FFieldName, CriteriaOperators[FCriteria], V1, V2]);
              end;
              crNull, crNotNull:
              begin
                if FQuickFilters[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FFieldName, CriteriaOperators[FCriteria]])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FFieldName, CriteriaOperators[FCriteria]]);
              end;
            end;
          end;

          // AND/OR fields
          if f < (FQuickFilters[i].Fields.Count - 1) then
          begin
            AndOrWhere := AndOrStr[FQuickFilters[i].AndOr] + ' ';
            S := S + AndOrWhere;
          end;

          FDataSet.SQL.Add(S);
          FSQLWhere.Add(S);
        end;
        // Close parenthesis, and AND/OR groups
        AndOrWhere := 'AND ';
        if i < (FQuickFilters.Count - 1) then
        begin
          FDataSet.SQL.Add(') ' + AndOrWhere);
          FSQLWhere.Add(') ' + AndOrWhere);
        end
        else
        begin
          FDataSet.SQL.Add(')');
          FSQLWhere.Add(')');
        end;
      end;
    end;
    FDataSet.SQL.Add(')');
    FSQLWhere.Add(')');
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
        FSQLWhere.Add(AndOrWhere + '(');
        // Iterate group fields
        for f := 0 to (FFields[i].Fields.Count - 1) do
        begin
          S := EmptyStr;
          with FFields[i].Fields[f] do
          begin
            V1 := FValue1;
            V2 := FValue2;
            PrepareValues(FCriteria, FDataType, V1, V2);
            Msk := GetValueMask(FCriteria, FDataType);

            // Fieldname, criteria, and values
            case Criteria of
              crLike, crStartLike, crEqual, crNotEqual, crMoreThan, crLessThan:
              begin
                if FFields[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FFieldName, CriteriaOperators[FCriteria], V1])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FFieldName, CriteriaOperators[FCriteria], V1]);
              end;
              crBetween:
              begin
                if FFields[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FFieldName, CriteriaOperators[FCriteria], V1, V2])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FFieldName, CriteriaOperators[FCriteria], V1, V2]);
              end;
              crNull, crNotNull:
              begin
                if FFields[i].Fields[f].Lookup then
                  S := S + Format(Msk, [FFieldName, CriteriaOperators[FCriteria]])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FFieldName, CriteriaOperators[FCriteria]]);
              end;
            end;

            // AND/OR fields
            if f < (FFields[i].Fields.Count - 1) then
            begin
              AndOrWhere := AndOrStr[FFields[i].AndOr] + ' ';
              S := S + AndOrWhere;
            end;
          end;

          FDataSet.SQL.Add(S);
          FSQLWhere.Add(S);
        end;
        // Close parenthesis, and AND/OR groups
        FDataSet.SQL.Add(')');
        FSQLWhere.Add(')');
        AndOrWhere := 'AND ';
      end;
    end;
  end;
  S := EmptyStr;

  // Active/inactive records
  case FRecordActive of
    rsAll: S := EmptyStr;
    rsActive: S := '1';
    rsInactive: S := '0';
    rsNone: S := EmptyStr;
  end;
  if S <> EmptyStr then
    if FTableAlias <> EmptyStr then
    begin
      FDataSet.SQL.Add(AndOrWhere + '(' + FTableAlias + '.active_status = ' + S + ')');
      FSQLWhere.Add(AndOrWhere + '(' + FTableAlias + '.active_status = ' + S + ')');
    end
    else
    begin
      FDataSet.SQL.Add(AndOrWhere + '(active_status = ' + S + ')');
      FSQLWhere.Add(AndOrWhere + '(active_status = ' + S + ')');
    end;

  // Add fields in ORDER BY clause
  if FSortFields.Count > 0 then
  begin
    aSort := EmptyStr;

    for i := 0 to (FSortFields.Count - 1) do
    begin
      // Field name
      if (FSortFields[i].SortType = stDateTime) then
      begin
        if (FSortFields[i].Lookup) or (FTableAlias = EmptyStr) then
          aSort := aSort + 'datetime(' + FSortFields[i].FieldName + ')'
        else
          aSort := aSort + 'datetime(' + FTableAlias + '.' + FSortFields[i].FieldName + ')';
      end
      else
      begin
        if (FSortFields[i].Lookup) or (FTableAlias = EmptyStr) then
          aSort := aSort + FSortFields[i].FieldName
        else
          aSort := aSort + FTableAlias + '.' + FSortFields[i].FieldName;
      end;

      // Collation
      if FSortFields[i].Collation <> EmptyStr then
        aSort := aSort + ' COLLATE ' + FSortFields[i].Collation;

      // Direction
      aSort := aSort + ' ' + SortDirections[FSortFields[i].Direction];

      if i < (FSortFields.Count - 1) then
        aSort := aSort + ', ';
    end;
    FDataSet.SQL.Add('ORDER BY ' + aSort);
    FSQLOrderBy := 'ORDER BY ' + aSort;
  end;

  //FDataSet.RefreshSQL.Text := FDataSet.SQL.Text;

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
  LastBackup := StrToDateTime('30/12/1500 00:00:00');
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
    LastBackup := C.FieldByName('last_backup').AsDateTime;
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

procedure TDBParams.Optimize;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
begin
  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  try
    LoadDatabaseParams(Self.Name, uCon);

    try
      uTrans.Action := caCommitRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      uCon.ExecuteDirect('PRAGMA optimize;');
      uCon.Close;

      MsgDlg(rsTitleInformation, rsSuccessfulDatabaseOptimization, mtInformation);
    except
      on E: Exception do
        MsgDlg(rsTitleError, Format(rsErrorOptimizingDatabase, [E.Message]), mtError);
    end;
  finally
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

procedure TDBParams.SetLastBackup;
begin
  DMM.sysCon.ExecuteDirect('UPDATE connections SET last_backup = datetime(''now'', ''localtime'') ' +
    'WHERE connection_name = ' + QuotedStr(Name));
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

procedure TDBParams.Vacuum;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
begin
  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  try
    LoadDatabaseParams(Self.Name, uCon);

    try
      uTrans.Action := caCommitRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      uCon.ExecuteDirect('END TRANSACTION;');
      uCon.ExecuteDirect('VACUUM;');
      uCon.ExecuteDirect('BEGIN TRANSACTION;');
      uCon.Close;

      MsgDlg(rsTitleInformation, rsSuccessfulDatabaseVacuum, mtInformation);
    except
      on E: Exception do
        MsgDlg(rsTitleError, Format(rsErrorVacuumingDatabase, [E.Message]), mtError);
    end;
  finally
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

end.

