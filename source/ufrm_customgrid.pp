unit ufrm_customgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StrUtils, RegExpr, DB, SQLDB, DateUtils,
  Grids, DBGrids, ExtCtrls, EditBtn, StdCtrls, ComCtrls, Menus, LCLIntf,
  Buttons, CheckLst, DBCtrls, laz.VirtualTrees, rxswitch, attabs, atshapelinebgra, BCPanel,
  DBControlGrid, ColorSpeedButton, cbs_datatypes, cbs_filters, Types;

type
  { TStringMemoEditor }

  TStringMemoEditor = class(TMemo)
  private
    FGrid: TCustomGrid;
    FCol,FRow:Integer;
  protected
    procedure msgSetValue(var Msg: TGridMessage);
      message GM_SETVALUE;
    procedure msgGetValue(var Msg: TGridMessage);
      message GM_GETVALUE;
    procedure msgSetGrid(var Msg: TGridMessage);
      message GM_SETGRID;
    procedure msgSetBounds(var Msg: TGridMessage);
      message GM_SETBOUNDS;
    procedure msgSelectAll(var Msg: TGridMessage);
      message GM_SELECTALL;
  end;


  { TfrmCustomGrid }

  TfrmCustomGrid = class(TForm)
    DBG: TDBGrid;
    dbgRecycle: TDBControlGrid;
    DBText3: TDBText;
    dsLink: TDataSource;
    dsLink1: TDataSource;
    dsLink2: TDataSource;
    dsLink3: TDataSource;
    dsLink4: TDataSource;
    dsLink5: TDataSource;
    iButtons: TImageList;
    iHeaders: TImageList;
    icoEmptyQuery: TImage;
    lblEmptyQuery: TLabel;
    lblRecordStatus: TLabel;
    lblChildStatus: TLabel;
    lblRecycleModifiedDate: TDBText;
    lblRecycleName: TDBText;
    pEmptyQuery: TBCPanel;
    pmcNewSurvey: TMenuItem;
    pmcNewCollector: TMenuItem;
    pmgInsert: TMenuItem;
    pmsSortAlphabetic: TMenuItem;
    pmsSortTaxonomic: TMenuItem;
    pmsSortCreationDate: TMenuItem;
    pmsSortLastModifiedDate: TMenuItem;
    pmsSortAscending: TMenuItem;
    pmsSortDescending: TMenuItem;
    pmcNewSurveyMember: TMenuItem;
    pmcNewMistnet: TMenuItem;
    pmcNewWeatherLog: TMenuItem;
    pmcNewNestRevision: TMenuItem;
    pmcNewEgg: TMenuItem;
    pmcNewPermanentNet: TMenuItem;
    pmcNewSamplePrep: TMenuItem;
    pmcNewProjectMember: TMenuItem;
    pGrid: TBCPanel;
    cbAgeFilter: TComboBox;
    cbBodyMoltFilter: TComboBox;
    cbFFMoltFilter: TComboBox;
    cbFFWearFilter: TComboBox;
    cbSkullOssificationFilter: TComboBox;
    eCycleCodeFilter: TEditButton;
    eAddChild: TEdit;
    eEndTimeFilter: TTimeEdit;
    eHowAgedFilter: TEditButton;
    eMoltLimitsFilter: TEditButton;
    eStartTimeFilter: TTimeEdit;
    gridChild1: TDBGrid;
    gridChild2: TDBGrid;
    gridChild4: TDBGrid;
    gridChild3: TDBGrid;
    gridChild5: TDBGrid;
    icoAgingFilter: TImage;
    icoBandNotReportedFilter: TImage;
    icoBandSizeFilter10: TImage;
    icoBandSizeFilter11: TImage;
    icoBandSizeFilter12: TImage;
    icoBandSizeFilter13: TImage;
    icoBandSizeFilter14: TImage;
    icoBandSizeFilter5: TImage;
    icoBandSizeFilter6: TImage;
    icoBandSizeFilter7: TImage;
    icoBandSizeFilter8: TImage;
    icoBandSizeFilter9: TImage;
    icoMarkedFilter: TImage;
    icoMarkedFilter12: TImage;
    icoMoltFilter: TImage;
    icoReportedFilter: TImage;
    icoSynonymsFilter: TImage;
    icoTaxonomiesFilter: TImage;
    icoUnmarkedFilter: TImage;
    lblAgeFilter: TLabel;
    lblBodyMoltFilter: TLabel;
    lblChildCount1: TLabel;
    lblChildTag1: TLabel;
    lblClementsFilter: TLabel;
    lblCycleCodeFilter: TLabel;
    lblEndTimeFilter: TLabel;
    lblFFMoltFilter: TLabel;
    lblFFWearFilter: TLabel;
    lblHasSynonymsFilter: TLabel;
    lblHowAgedFilter: TLabel;
    lblMarkedFilter: TLabel;
    lblMoltLimitsFilter: TLabel;
    lblChildCount2: TLabel;
    lblChildTag2: TLabel;
    lblChildCount4: TLabel;
    lblChildTag4: TLabel;
    lblNotReportedFilter: TLabel;
    lblReportedFilter: TLabel;
    lblChildCount3: TLabel;
    lblChildTag3: TLabel;
    lblSkullOssificationFilter: TLabel;
    lblChildCount5: TLabel;
    lblChildTag5: TLabel;
    lblStartTimeFilter: TLabel;
    lblSynonymFilter: TLabel;
    lblTaxonomyCbroFilter: TLabel;
    lblTaxonomyIocFilter: TLabel;
    lblUnmarkedFilter: TLabel;
    nbChilds: TNotebook;
    pAgeFilter: TBCPanel;
    pBodyMoltFilter: TBCPanel;
    pChildCount1: TBCPanel;
    pChildTag1: TBCPanel;
    pChildsBar: TBCPanel;
    pEndTimeFilter: TBCPanel;
    pFFMoltFilter: TBCPanel;
    pFFWearFilter: TBCPanel;
    pgChild1: TPage;
    pgChild2: TPage;
    pgChild4: TPage;
    pgChild3: TPage;
    pgChild5: TPage;
    pHasSynonymsFilter: TBCPanel;
    pHowAgedFilter: TBCPanel;
    pIsSynonymFilter: TBCPanel;
    pmAddChild: TPopupMenu;
    pMarkedFilter: TBCPanel;
    pmcNewCapture: TMenuItem;
    pmcNewMolt: TMenuItem;
    pmcNewNest: TMenuItem;
    pmcNewSighting: TMenuItem;
    pmcNewSpecimen: TMenuItem;
    pmfAdvancedFilter: TMenuItem;
    pmfClearFilters: TMenuItem;
    pmfFilterSelected: TMenuItem;
    pmFilter: TPopupMenu;
    pMoltCycleFilter: TBCPanel;
    pMoltLimitsFilter: TBCPanel;
    pChildCount2: TBCPanel;
    pChildTag2: TBCPanel;
    pChildCount4: TBCPanel;
    pChildTag4: TBCPanel;
    pmSort: TPopupMenu;
    pNotReportedFilter: TBCPanel;
    pRecordStatus: TBCPanel;
    pChildStatus: TBCPanel;
    pRecordToolbar: TBCPanel;
    pColumnsToolbar: TBCPanel;
    pImagesToolbar: TBCPanel;
    pRecycleContent: TPanel;
    pReportedFilter: TBCPanel;
    pChildCount3: TBCPanel;
    pChildTag3: TBCPanel;
    pSkullOssificationFilter: TBCPanel;
    pChildCount5: TBCPanel;
    pChildTag5: TBCPanel;
    pStartTimeFilter: TBCPanel;
    pBandReportFilters: TBCPanel;
    cbBroodPatchFilter: TComboBox;
    cbCloacalProtuberanceFilter: TComboBox;
    cbSexFilter: TComboBox;
    eHowSexedFilter: TEditButton;
    icoBandSizeFilter2: TImage;
    icoBandSizeFilter3: TImage;
    icoBandSizeFilter4: TImage;
    icoSexFilter: TImage;
    lblBroodPatchFilter: TLabel;
    lblCloacalProtuberanceFilter: TLabel;
    lblHowSexedFilter: TLabel;
    lblSexFilter: TLabel;
    pBroodPatchFilter: TBCPanel;
    pCloacalProtuberanceFilter: TBCPanel;
    pHowSexedFilter: TBCPanel;
    pSexFilter: TBCPanel;
    pSexingFilters: TBCPanel;
    cbBandSizeFilter: TComboBox;
    cbBandStatusFilter: TComboBox;
    cbFatFilter: TComboBox;
    cbMaterialFilter: TComboBox;
    cbMethodFilter: TComboBox;
    cbNestFateFilter: TComboBox;
    cbNestSupportFilter: TComboBox;
    cbSiteRankFilter: TComboBox;
    clbTaxonRanksFilter: TCheckListBox;
    icoBandSizeFilter: TImage;
    icoBandStatusFilter: TImage;
    icoDateFilter: TImage;
    icoExtinctFilter: TImage;
    icoFatFilter: TImage;
    icoMaterialFilter: TImage;
    icoMethodFilter: TImage;
    icoNestFateFilter: TImage;
    icoNestSupportFilter: TImage;
    icoSiteFilter: TImage;
    icoSiteRankFilter: TImage;
    icoTaxaFilter: TImage;
    icoTaxonRanksFilter: TImage;
    icoWithColorBandsFilter: TImage;
    icoWithRecapturesFilter: TImage;
    lblBandSizeFilter: TLabel;
    lblBandStatusFilter: TLabel;
    lblCountDateFilter: TLabel;
    lblCountSiteFilter: TLabel;
    lblCountTaxonFilter: TLabel;
    lblCountTaxonRanksFilter: TLabel;
    lblDateFilter: TLabel;
    lblExtinctFilter: TLabel;
    lblFatFilter: TLabel;
    lblMaterialFilter: TLabel;
    lblMethodFilter: TLabel;
    lblNestFateFilter: TLabel;
    lblNestSupportFilter: TLabel;
    lblSiteFilter: TLabel;
    lblSiteRankFilter: TLabel;
    lblTaxonFilter: TLabel;
    lblTaxonRanksFilter: TLabel;
    lblWithColorBandsFilter: TLabel;
    lblWithRecapturesFilter: TLabel;
    pBandSizeFilter: TBCPanel;
    pBandStatusFilter: TBCPanel;
    pDateFilter: TPanel;
    pDatesFilters: TBCPanel;
    pExtinctFilter: TBCPanel;
    pFatFilter: TBCPanel;
    pMaterialFilter: TBCPanel;
    pMethodFilter: TBCPanel;
    pNestFateFilter: TBCPanel;
    pNestSupportFilter: TBCPanel;
    pQuickFiltersContent: TBCPanel;
    pChildToolbar: TBCPanel;
    pSiteFilters: TBCPanel;
    pSiteRankFilter: TBCPanel;
    pSynonymFilters: TBCPanel;
    pMoltingFilters: TBCPanel;
    pTaxonomiesFilters: TBCPanel;
    pTimeFilters: TBCPanel;
    pAgingFilters: TBCPanel;
    pMarksFilters: TBCPanel;
    pTaxonFilters: TBCPanel;
    pTaxonomyCbroFilter: TBCPanel;
    pTaxonomyClementsFilter: TBCPanel;
    pTaxonomyIocFilter: TBCPanel;
    pTaxonRanksFilters: TBCPanel;
    pTitleSiteFilter: TPanel;
    bvSpacerFilters: TBevel;
    dbgImages: TDBControlGrid;
    lblImageDate: TDBText;
    lblImageTime: TDBText;
    lblImageType: TDBText;
    lineRight: TShapeLineBGRA;
    pSideToolbar: TPanel;
    pImageItem: TPanel;
    pChild: TPanel;
    pClient: TPanel;
    pTitleTaxonFilter: TPanel;
    pTitleTaxonRanksFilter: TPanel;
    pUnmarkedFilter: TBCPanel;
    pWithColorBandsFilter: TBCPanel;
    pWithRecapturesFilter: TBCPanel;
    sbAddChild: TSpeedButton;
    sbCancelRecord: TSpeedButton;
    sbDelChild: TSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditChild: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbRowHeightDecrease: TSpeedButton;
    sbEditRecord10: TSpeedButton;
    sbRowHeightDefault: TSpeedButton;
    sbColumnWidthAutoAdjust: TSpeedButton;
    sbColumnHide: TSpeedButton;
    sbEditRecord5: TSpeedButton;
    sbEditRecord6: TSpeedButton;
    sbEditRecord7: TSpeedButton;
    sbEditRecord8: TSpeedButton;
    sbEditRecord9: TSpeedButton;
    sbFirstRecord: TSpeedButton;
    sbFirstChild: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbRowHeightIncrease: TSpeedButton;
    sbInsertRecord2: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbLastChild: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sbNextChild: TSpeedButton;
    sbPriorRecord: TSpeedButton;
    sbPriorChild: TSpeedButton;
    sbChildHistory: TSpeedButton;
    sbRecordHistory: TSpeedButton;
    sbRecycleDelete: TColorSpeedButton;
    sbRecycleRestore: TColorSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbSaveRecord: TSpeedButton;
    sbShowRecord: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowAudio: TSpeedButton;
    sbShowDocs: TSpeedButton;
    sbShowColumns: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbShowSummary: TSpeedButton;
    sbSortChilds1: TSpeedButton;
    sbSortChilds: TSpeedButton;
    sbSortRecords: TSpeedButton;
    sbFilterRecords: TSpeedButton;
    Separator10: TShapeLineBGRA;
    Separator11: TShapeLineBGRA;
    Separator12: TMenuItem;
    Separator14: TMenuItem;
    Separator5: TShapeLineBGRA;
    Separator6: TShapeLineBGRA;
    Separator7: TShapeLineBGRA;
    Separator8: TShapeLineBGRA;
    Separator9: TShapeLineBGRA;
    SplitChild: TSplitter;
    cardImages: TPage;
    cardAudio: TPage;
    cardDocs: TPage;
    cardSummary: TPage;
    cardColumns: TPage;
    cardRecycle: TPage;
    pmtExpandAll: TMenuItem;
    pmtColapseAll: TMenuItem;
    pmtRefresh: TMenuItem;
    pmtClearSelection: TMenuItem;
    pmgRefresh: TMenuItem;
    pmgEdit: TMenuItem;
    pmgDel: TMenuItem;
    pmgRecordHistory: TMenuItem;
    pmGrid: TPopupMenu;
    pmTree: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    SplitRight: TSplitter;
    cpSide: TNotebook;
    pSide: TPanel;
    cardQuickFilters: TPage;
    cardViewRecord: TPage;
    scrollFilter: TScrollBox;
    gridColumns: TStringGrid;
    gridRecord: TStringGrid;
    TimerFind: TTimer;
    TimerUpdate: TTimer;
    titleViewRecord: TLabel;
    titleRecycle: TLabel;
    titleColumns: TLabel;
    titleSummary: TLabel;
    titleDocs: TLabel;
    titleAudios: TLabel;
    titleImages: TLabel;
    titleQuickFilters: TLabel;
    tsBandNotReported: TRxSwitch;
    tsBandReported: TRxSwitch;
    tsfMarked: TRxSwitch;
    tsfUnmarked: TRxSwitch;
    tsfWithColorBandsFilter: TRxSwitch;
    tsfWithRecapturesFilter: TRxSwitch;
    tsHasSynonyms: TRxSwitch;
    tsIsSynonym: TRxSwitch;
    tsTaxonExtinct: TRxSwitch;
    tsTaxonomyCbro: TRxSwitch;
    tsTaxonomyClements: TRxSwitch;
    tsTaxonomyIoc: TRxSwitch;
    tvDateFilter: TLazVirtualStringTree;
    tvSiteFilter: TLazVirtualStringTree;
    tvTaxaFilter: TLazVirtualStringTree;
    procedure cbAgeFilterChange(Sender: TObject);
    procedure cbBandSizeFilterChange(Sender: TObject);
    procedure cbBandStatusFilterChange(Sender: TObject);
    procedure cbBroodPatchFilterChange(Sender: TObject);
    procedure cbCloacalProtuberanceFilterChange(Sender: TObject);
    procedure cbNestFateFilterChange(Sender: TObject);
    procedure cbNestSupportFilterChange(Sender: TObject);
    procedure cbSexFilterChange(Sender: TObject);
    procedure cbSiteRankFilterChange(Sender: TObject);
    procedure cbSkullOssificationFilterChange(Sender: TObject);
    procedure DBGColExit(Sender: TObject);
    procedure DBGEditButtonClick(Sender: TObject);
    procedure DBGEditingDone(Sender: TObject);
    procedure DBGMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure DBGPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
      AState: TGridDrawState);
    procedure DBGSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure dsLink1DataChange(Sender: TObject; Field: TField);
    procedure dsLink1StateChange(Sender: TObject);
    procedure dsLink2DataChange(Sender: TObject; Field: TField);
    procedure dsLink2StateChange(Sender: TObject);
    procedure dsLink3DataChange(Sender: TObject; Field: TField);
    procedure dsLink3StateChange(Sender: TObject);
    procedure dsLink4DataChange(Sender: TObject; Field: TField);
    procedure dsLink4StateChange(Sender: TObject);
    procedure dsLink5DataChange(Sender: TObject; Field: TField);
    procedure dsLink5StateChange(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure dsLinkStateChange(Sender: TObject);
    procedure eAddChildKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridChild1DblClick(Sender: TObject);
    procedure pChildTag1Click(Sender: TObject);
    procedure pChildTag1MouseEnter(Sender: TObject);
    procedure pChildTag1MouseLeave(Sender: TObject);
    procedure pClientResize(Sender: TObject);
    procedure pmcNewCaptureClick(Sender: TObject);
    procedure pmcNewCollectorClick(Sender: TObject);
    procedure pmcNewEggClick(Sender: TObject);
    procedure pmcNewMistnetClick(Sender: TObject);
    procedure pmcNewMoltClick(Sender: TObject);
    procedure pmcNewNestClick(Sender: TObject);
    procedure pmcNewNestRevisionClick(Sender: TObject);
    procedure pmcNewPermanentNetClick(Sender: TObject);
    procedure pmcNewProjectMemberClick(Sender: TObject);
    procedure pmcNewSamplePrepClick(Sender: TObject);
    procedure pmcNewSightingClick(Sender: TObject);
    procedure pmcNewSpecimenClick(Sender: TObject);
    procedure pmcNewSurveyMemberClick(Sender: TObject);
    procedure pmcNewWeatherLogClick(Sender: TObject);
    procedure sbAddChildClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbColumnHideClick(Sender: TObject);
    procedure sbColumnWidthAutoAdjustClick(Sender: TObject);
    procedure sbDelChildClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbEditChildClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFilterRecordsClick(Sender: TObject);
    procedure sbFirstChildClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastChildClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbNextChildClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorChildClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRecordHistoryClick(Sender: TObject);
    procedure sbChildHistoryClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbRowHeightDecreaseClick(Sender: TObject);
    procedure sbRowHeightDefaultClick(Sender: TObject);
    procedure sbRowHeightIncreaseClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShowRecordClick(Sender: TObject);
    procedure sbSortChildsClick(Sender: TObject);
    procedure sbSortRecordsClick(Sender: TObject);
    procedure SplitChildMoved(Sender: TObject);
    procedure SplitRightMoved(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure tsBandNotReportedOff(Sender: TObject);
    procedure tsBandNotReportedOn(Sender: TObject);
    procedure tsBandReportedOff(Sender: TObject);
    procedure tsBandReportedOn(Sender: TObject);
    procedure tsfMarkedOff(Sender: TObject);
    procedure tsfMarkedOn(Sender: TObject);
    procedure tsfUnmarkedOff(Sender: TObject);
    procedure tsfUnmarkedOn(Sender: TObject);
    procedure tsfWithColorBandsFilterOn(Sender: TObject);
    procedure tsfWithRecapturesFilterOn(Sender: TObject);
    procedure tsHasSynonymsOff(Sender: TObject);
    procedure tsHasSynonymsOn(Sender: TObject);
    procedure tsIsSynonymOff(Sender: TObject);
    procedure tsIsSynonymOn(Sender: TObject);
    procedure tsTaxonExtinctOff(Sender: TObject);
    procedure tsTaxonExtinctOn(Sender: TObject);
    procedure tsTaxonomyCbroOff(Sender: TObject);
    procedure tsTaxonomyCbroOn(Sender: TObject);
    procedure tsTaxonomyClementsOff(Sender: TObject);
    procedure tsTaxonomyClementsOn(Sender: TObject);
    procedure tsTaxonomyIocOff(Sender: TObject);
    procedure tsTaxonomyIocOn(Sender: TObject);
    procedure tvDateFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvDateFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
      var Allowed: Boolean);
    procedure tvDateFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvDateFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure tvDateFilterInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure tvSiteFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvSiteFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
      var Allowed: Boolean);
    procedure tvSiteFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvSiteFilterGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvSiteFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure tvSiteFilterInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure tvTaxaFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTaxaFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
      var Allowed: Boolean);
    procedure tvTaxaFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTaxaFilterGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvTaxaFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure tvTaxaFilterInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure tvTaxaFilterPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
    FTableType, FChildTable: TTableType;
    SearchStr: TSearch;
    FSearch: TCustomSearch;
    QuickFilters: TStringList;
    Modifier: TRecordStatus;
    Sorting: TSortedFields;
    WhereSQL: TStrings;
    Filtrado: Boolean;
    FSidePanel, OldSidePanel: Boolean;
    FSideIndex, OldSideIndex: Integer;
    FSearchString, OldSearchString: String;
    BandSizeFilter, BandStatusFilter: String;
    DateFilter, ReportedFilter: String;
    SiteFilter, SiteRankFilter: String;
    ClementsFilter, IocFilter, CbroFilter: String;
    TaxaFilter, RanksFilter, ExtinctFilter: String;
    SexFilter, AgeFilter: String;
    WithColorFilter, WithRecapturesFilter: String;
    IsSynonymFilter, HasSynonymsFilter: String;
    CloacalProtuberanceFilter, BroodPatchFilter: String;
    NestFateFilter, NestSupportFilter: String;
    CanToggle: Boolean;
    FSidePanelFactor: Double;
    FChildPanelFactor: Double;
    cellMemo: TMemo;
    procedure SetSidePanel(aValue: Boolean);
    procedure SetSideIndex(aValue: Integer);
    procedure SetSearchString(aValue: String);
    procedure SetGridAndChild;
    procedure AddGridColumns(aTable: TTableType; aGrid: TDBGrid);
    procedure SetGridColumns(aTable: TTableType; aGrid: TDBGrid);
    procedure AddSortedField(aFieldName: String; aDirection: TSortDirection; aCollation: String = '';
      IsAnAlias: Boolean = False);
    function Search(aValue: String): Boolean;
    function SearchInstitutions(aValue: String): Boolean;
    function SearchPeople(aValue: String): Boolean;
    function SearchProjects(aValue: String): Boolean;
    function SearchPermits(aValue: String): Boolean;
    function SearchGazetteer(aValue: String): Boolean;
    function SearchNetStations(aValue: String): Boolean;
    function SearchTaxonRanks(aValue: String): Boolean;
    function SearchBotanicTaxa(aValue: String): Boolean;
    function SearchZooTaxa(aValue: String): Boolean;
    function SearchBands(aValue: String): Boolean;
    function SearchIndividuals(aValue: String): Boolean;
    function SearchCaptures(aValue: String): Boolean;
    function SearchNestRevisions(aValue: String): Boolean;
    function SearchNests(aValue: String): Boolean;
    function SearchEggs(aValue: String): Boolean;
    function SearchMethods(aValue: String): Boolean;
    function SearchExpeditions(aValue: String): Boolean;
    function SearchSurveys(aValue: String): Boolean;
    function SearchSightings(aValue: String): Boolean;
    function SearchSpecimens(aValue: String): Boolean;
    procedure LoadRecordColumns;
    procedure LoadRecordRow;
    procedure UpdateFilterPanels;
    procedure GetFilters(aList: TStrings);
    procedure GetInstitutionFilters(aList: TStrings);
    procedure GetPeopleFilters(aList: TStrings);
    procedure GetProjectFilters(aList: TStrings);
    procedure GetPermitFilters(aList: TStrings);
    procedure GetGazetteerFilters(aList: TStrings);
    procedure GetNetStationFilters(aList: TStrings);
    procedure GetTaxonRankFilters(aList: TStrings);
    procedure GetBotanicTaxaFilters(aList: TStrings);
    procedure GetZooTaxaFilters(aList: TStrings);
    procedure GetBandFilters(aList: TStrings);
    procedure GetIndividualFilters(aList: TStrings);
    procedure GetCaptureFilters(aList: TStrings);
    procedure GetNestFilters(aList: TStrings);
    procedure GetEggFilters(aList: TStrings);
    procedure GetMethodFilters(aList: TStrings);
    procedure GetExpeditionFilters(aList: TStrings);
    procedure GetSurveyFilters(aList: TStrings);
    procedure GetSightingFilters(aList: TStrings);
    procedure GetSpecimenFilters(aList: TStrings);
    procedure ClearInstitutionFilters;
    procedure ClearPeopleFilters;
    procedure ClearProjectFilters;
    procedure ClearPermitFilters;
    procedure ClearGazetteerFilters;
    procedure ClearNetStationFilters;
    procedure ClearTaxonRankFilters;
    procedure ClearBotanicTaxaFilters;
    procedure ClearZooTaxaFilters;
    procedure ClearBandFilters;
    procedure ClearIndividualFilters;
    procedure ClearCaptureFilters;
    procedure ClearNestFilters;
    procedure ClearEggFilters;
    procedure ClearMethodFilters;
    procedure ClearExpeditionFilters;
    procedure ClearSurveyFilters;
    procedure ClearSightingFilters;
    procedure ClearSpecimenFilters;
    procedure ClearSearch;
    procedure ChildTagClick(aTag, aCountTag: TBCPanel);
    procedure ChildTagMouseEnter(aTag, aCountTag: TBCPanel);
    procedure ChildTagMouseLeave(aTag, aCountTag: TBCPanel);
    function GetChildDataSet: TDataSet;
    procedure UpdateButtons(aDataSet: TDataSet);
    procedure UpdateChildButtons(aDataSet: TDataSet);
    procedure UpdateChildCount;
    procedure UpdateChildStatus;
  public
    property TableType: TTableType read FTableType write FTableType;

    property ShowSidePanel: Boolean read FSidePanel write SetSidePanel;
    property SidePanelIndex: Integer read FSideIndex write SetSideIndex;
    property SearchString: String read FSearchString write SetSearchString;
  end;

var
  frmCustomGrid: TfrmCustomGrid;

implementation

uses
  cbs_locale, cbs_global, cbs_system, cbs_themes, cbs_gis, cbs_birds, cbs_editdialogs, cbs_dialogs,
  cbs_finddialogs, cbs_data, cbs_getvalue, cbs_taxonomy, cbs_datasearch, {$IFDEF DEBUG}cbs_debug,{$ENDIF}
  udm_main, udm_grid, udm_individuals, udm_breeding, udm_sampling, ufrm_main;

{$R *.lfm}

{ TStringMemoEditor }

procedure TStringMemoEditor.msgGetValue(var Msg: TGridMessage);
begin
  Msg.Value := Self.Text;
end;

procedure TStringMemoEditor.msgSelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TStringMemoEditor.msgSetBounds(var Msg: TGridMessage);
var
  R: TRect;
begin
  {$IFNDEF DEBUG}
  TStringGrid(Msg.Grid).RowHeights[Msg.Row] := 52;
  {$ENDIF}
  R := Msg.CellRect;
  R.Height := 52;
  BoundsRect := R;
end;

procedure TStringMemoEditor.msgSetGrid(var Msg: TGridMessage);
begin
  FGrid := Msg.Grid;
  Msg.Options := EO_SELECTALL or EO_HOOKKEYDOWN or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TStringMemoEditor.msgSetValue(var Msg: TGridMessage);
begin
  Self.Text := Msg.Value;
end;

{ TfrmCustomGrid }

procedure TfrmCustomGrid.cbAgeFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbBandSizeFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbBandStatusFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbBroodPatchFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbCloacalProtuberanceFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbNestFateFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbNestSupportFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbSexFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbSiteRankFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.cbSkullOssificationFilterChange(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.ChildTagClick(aTag, aCountTag: TBCPanel);
begin
  if Working then
    Exit;

  Working := True;
  pChildTag1.Border.Color := $00D1D1D1;
  pChildTag1.Border.Width := 1;
  pChildTag2.Border.Color := $00D1D1D1;
  pChildTag2.Border.Width := 1;
  pChildTag3.Border.Color := $00D1D1D1;
  pChildTag3.Border.Width := 1;
  pChildTag4.Border.Color := $00D1D1D1;
  pChildTag4.Border.Width := 1;
  pChildTag5.Border.Color := $00D1D1D1;
  pChildTag5.Border.Width := 1;

  aTag.Background.Color := $00FAEBE8;
  aCountTag.Color := aTag.Background.Color;
  if (pChild.Visible) and (aTag.Tag = nbChilds.PageIndex) then
  begin
    pChild.Visible := False;
    aTag.Border.Color := $00D1D1D1;
    aTag.Border.Width := 1;
  end
  else
  begin
    nbChilds.PageIndex := aTag.Tag;
    if not pChild.Visible then
      pChild.Visible := True;
    pChild.Top := pChildsBar.Top + pChildsBar.Height + 1;
    aTag.Border.Color := $00C75F5B;
    aTag.Border.Width := 2;
  end;
  //splitChild.Visible := pChild.Visible;
  aTag.Background.Color := $00E0C0C0;
  aCountTag.Color := aTag.Background.Color;
  Working := False;
end;

procedure TfrmCustomGrid.ChildTagMouseEnter(aTag, aCountTag: TBCPanel);
begin
  aTag.Background.Color := $00E0C0C0;
  aCountTag.Color := aTag.Background.Color;
end;

procedure TfrmCustomGrid.ChildTagMouseLeave(aTag, aCountTag: TBCPanel);
begin
  aTag.Background.Color := $00FAFAFA;
  aCountTag.Color := aTag.Background.Color;
end;

procedure TfrmCustomGrid.DBGPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
  aTable: TTableType;
begin
  if (Column.Visible = False) then
    Exit;
  if (gdSelected in AState) or (gdFixed in AState) then
    Exit;

  if Sender = DBG then
  begin
    aTable := FTableType
  end
  else
  begin
    case FTableType of
      tbIndividuals:
      begin
        if Sender = gridChild1 then
          aTable := tbCaptures
        else
        if Sender = gridChild2 then
          aTable := tbMolts
        else
        if Sender = gridChild3 then
          aTable := tbSightings
        else
        if Sender = gridChild4 then
          aTable := tbNests
        else
        if Sender = gridChild5 then
          aTable := tbSpecimens;
      end;
      tbNests:
      begin
        if Sender = gridChild1 then
          aTable := tbNestRevisions
        else
        if Sender = gridChild2 then
          aTable := tbEggs
        else
        if Sender = gridChild3 then
          aTable := tbNestOwners;
      end;
      tbSurveys:
      begin
        if Sender = gridChild1 then
          aTable := tbSurveyTeams
        else
        if Sender = gridChild2 then
          aTable := tbNetsEffort
        else
        if Sender = gridChild3 then
          aTable := tbWeatherLogs
        else
        if Sender = gridChild4 then
          aTable := tbCaptures
        else
        if Sender = gridChild5 then
          aTable := tbSightings;
      end;
      tbSpecimens:
      begin
        if Sender = gridChild1 then
          aTable := tbSamplePreps;
      end;
      tbNetStations:
      begin
        if Sender = gridChild1 then
          aTable := tbPermanentNets;
      end;
      tbProjects:
      begin
        if Sender = gridChild1 then
          aTable := tbProjectTeams;
      end;
    end;
  end;

  case aTable of
    tbInstitutions:
      begin
        if Column.FieldName = 'acronym' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbPeople:
      begin
        if Column.FieldName = 'acronym' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbBands:
      begin
        if Column.FieldName = 'band_size' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end
        else
        if Column.FieldName = 'band_status' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
          case Column.Field.AsString of
            'U': // Used
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
              end;
            'D': // Available
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemNeutralFGLight;
              end;
            'R': // Removed
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemMediumBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemMediumFGLight;
              end;
            'Q': // Broken
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
              end;
            'P': // Lost
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
              end;
            'T': // Transfered
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clVioletBrand1Light;
                TDBGrid(Sender).Canvas.Font.Color := clVioletFG2Light;
              end;
          end;
        end;
      end;
    tbIndividuals:
      begin
        if Column.FieldName = 'taxon_name' then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
        else
        if (Column.FieldName = 'individual_sex') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
          case Column.Field.AsString of
            'U':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
              end;
            'M':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
              end;
            'F':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
              end;
          end;
        end
        else
        if Column.FieldName = 'band_full_name' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end
        else
        if Column.FieldName = 'removed_band_name' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbCaptures:
      begin
        if (Column.FieldName = 'taxon_name') then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
        else
        if (Column.FieldName = 'capture_type') then
        begin
          case Column.Field.AsString of
            'N':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clBlueBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryLight;
              end;
            'R', 'S':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
              end;
            'C':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
              end;
            'U':
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
              end;
          end;
        end
        else
        if (Column.FieldName = 'band_name') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
          if (TDBGrid(Sender).Columns.ColumnByFieldname('capture_type').Field.AsString = 'R') or
            (TDBGrid(Sender).Columns.ColumnByFieldname('capture_type').Field.AsString = 'S') then
          begin
            //TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
            TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
          end
          else
          if (TDBGrid(Sender).Columns.ColumnByFieldname('capture_type').Field.AsString = 'C') then
          begin
            //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
            TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
          end;
        end
        else
        if (Column.FieldName = 'removed_band_name') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
          if (TDBGrid(Sender).Columns.ColumnByFieldname('capture_type').Field.AsString = 'C') then
          begin
            //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
            TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
          end;
        end;

        { Paint the cell background red the invalid values }
        if (Column.FieldName = 'cloacal_protuberance') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, CloacalProtuberanceValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end
        else
        if (Column.FieldName = 'brood_patch') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, BroodPatchValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end
        else
        if (Column.FieldName = 'fat') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, FatValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end
        else
        if (Column.FieldName = 'body_molt') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, BodyMoltValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end
        else
        if (Column.FieldName = 'flight_feather_molt') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, FlightMoltValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end
        else
        if (Column.FieldName = 'flight_feather_wear') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, FeatherWearValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end
        else
        if (Column.FieldName = 'skull_ossification') then
        begin
          if (Column.Field.AsString <> '') and
            not (MatchStr(Column.Field.AsString, SkullValues)) then
          begin
            TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          end;
        end;
      end;
    tbMolts:
      begin
        if Column.FieldName = 'sample_date' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbNests:
      begin
        if (Column.FieldName = 'taxon_name') or
          (Column.FieldName = 'support_plant_1_name') or
          (Column.FieldName = 'support_plant_2_name') then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
        else
        if Column.FieldName = 'nest_fate' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
          case Column.Field.AsString of
            'U':       // Unknown
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
              end;
            'P':       // Lost
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
              end;
            'S':       // Success
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
              end;
          end;
        end;
      end;
    tbNestRevisions:
      begin
        if Column.FieldName = 'nidoparasite_name' then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
        else
        if Column.FieldName = 'nest_status' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
          case Column.Field.AsString of
            'U':       // Unknown
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
              end;
            'I':       // Lost
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
              end;
            'A':       // Success
              begin
                TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
                TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
              end;
          end;
        end;
      end;
    tbEggs:
      begin
        if Column.FieldName = 'egg_seq' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end
        else
        if Column.FieldName = 'taxon_name' then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
      end;
    tbExpeditions:
      begin
        if Column.FieldName = 'start_date' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbSurveys:
      begin
        if Column.FieldName = 'survey_date' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbSightings:
      begin
        if Column.FieldName = 'sighting_date' then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end
        else
        if Column.FieldName = 'taxon_name' then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
      end;
    tbNetsEffort:
      begin
        if (Column.FieldName = 'sample_date') or
          (Column.FieldName = 'net_number') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbSpecimens:
      begin
        if (Column.FieldName = 'collection_date') or
          (Column.FieldName = 'field_number') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end
        else
        if Column.FieldName = 'taxon_name' then
        begin
          TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
        end
      end;
    tbSamplePreps:
      begin
        if (Column.FieldName = 'preparation_date') or
          (Column.FieldName = 'accession_num') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbProjects:
      begin
        if (Column.FieldName = 'start_date') or
          (Column.FieldName = 'net_number') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbPermits:
      begin
        if (Column.FieldName = 'expire_date') or
          (Column.FieldName = 'permit_number') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    tbPermanentNets:
      begin
        if (Column.FieldName = 'net_number') then
        begin
          {$IFDEF MSWINDOWS}
          TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
          {$ELSE}
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
          {$ENDIF}
        end;
      end;
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    tbGazetteer: ;
    tbNetStations: ;
    tbProjectTeams: ;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa: ;
    //tbBandHistory: ;
    //tbMethods: ;
    tbSurveyTeams: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  // Here you choose what column will be affected (the columns of the DBGrid not SQL).
  //if Column.Field.DataType in [ftMemo,ftWideMemo] then
  //begin
  //  // The next is not neccesary but you can use it to adjust your text appearance.
  //  // you can change colors, font, size, as well other parameters.
  //  MyTextStyle := TDBGrid(Sender).Canvas.TextStyle;
  //  MyTextStyle.SingleLine := False;
  //  MyTextStyle.Wordbreak  := False;
  //  MyTextStyle.Layout := tlTop;
  //  TDBGrid(Sender).Canvas.TextStyle := MyTextStyle;
  //
  //  // Here how to show any text:
  //  // just assign an event procedure to OnGetText of the Field.
  //  //Column.Field.OnGetText := @DBGridOnGetText;
  //end;
end;

procedure TfrmCustomGrid.DBGSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
begin
  if Opening or Closing then
    Exit;

  case Column.Field.DataType of
    ftMemo:
      begin
        if (cellMemo = nil) then
        begin
          cellMemo := TStringMemoEditor.Create(Self);
          cellMemo.Parent := (Sender as TDBGrid);
          cellMemo.ScrollBars := ssAutoVertical;
          cellMemo.Visible := False;
        end;
        Editor := cellMemo;
      end;
  end;

end;

procedure TfrmCustomGrid.dsLink1DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;
end;

procedure TfrmCustomGrid.dsLink1StateChange(Sender: TObject);
begin
  if Assigned(dsLink1.DataSet) and (nbChilds.PageIndex = 0) then
    UpdateChildButtons(dsLink1.DataSet);
end;

procedure TfrmCustomGrid.dsLink2DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;
end;

procedure TfrmCustomGrid.dsLink2StateChange(Sender: TObject);
begin
  if Assigned(dsLink2.DataSet) and (nbChilds.PageIndex = 1) then
    UpdateChildButtons(dsLink2.DataSet);
end;

procedure TfrmCustomGrid.dsLink3DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;
end;

procedure TfrmCustomGrid.dsLink3StateChange(Sender: TObject);
begin
  if Assigned(dsLink3.DataSet) and (nbChilds.PageIndex = 2) then
    UpdateChildButtons(dsLink3.DataSet);
end;

procedure TfrmCustomGrid.dsLink4DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;
end;

procedure TfrmCustomGrid.dsLink4StateChange(Sender: TObject);
begin
  if Assigned(dsLink4.DataSet) and (nbChilds.PageIndex = 3) then
    UpdateChildButtons(dsLink4.DataSet);
end;

procedure TfrmCustomGrid.dsLink5DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;
end;

procedure TfrmCustomGrid.dsLink5StateChange(Sender: TObject);
begin
  if Assigned(dsLink5.DataSet) and (nbChilds.PageIndex = 4) then
    UpdateChildButtons(dsLink5.DataSet);
end;

procedure TfrmCustomGrid.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  LoadRecordRow;

  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
end;

procedure TfrmCustomGrid.dsLinkStateChange(Sender: TObject);
begin
  if Assigned(dsLink.DataSet) then
    UpdateButtons(dsLink.DataSet);

  pEmptyQuery.Visible := dsLink.DataSet.RecordCount = 0;

  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
end;

procedure TfrmCustomGrid.eAddChildKeyPress(Sender: TObject; var Key: char);
var
  P: Integer;
begin
  // <ENTER/RETURN> Key
  if Key = #13 then
  begin
    case FTableType of
      tbSurveys:
      begin
        case FChildTable of
          tbSurveyTeams:
          begin
            P := GetKey('people', 'person_id', 'acronym', AnsiUpperCase(eAddChild.Text));
            if P > 0 then
            begin
              dsLink1.DataSet.Append;
              dsLink1.DataSet.FieldByName('person_id').AsInteger := P;
              //TabBperson_name.AsWideString := GetName('people', 'full_name', 'person_id', P);
              dsLink1.DataSet.Post;
              eAddChild.Clear;

              UpdateChildButtons(dsLink1.DataSet);
            end;
          end;
          tbSightings: ;
        end;
      end;
      tbProjects:
      begin
        case FChildTable of
          tbProjectTeams:
          begin
            P := GetKey('people', 'person_id', 'acronym', AnsiUpperCase(eAddChild.Text));
            if P > 0 then
            begin
              dsLink1.DataSet.Append;
              dsLink1.DataSet.FieldByName('person_id').AsInteger := P;
              //TabBperson_name.AsWideString := GetName('people', 'full_name', 'person_id', P);
              dsLink1.DataSet.Post;
              eAddChild.Clear;

              UpdateChildButtons(dsLink1.DataSet);
            end;
          end;
        end;
      end;
      tbSpecimens:
      begin
        case FChildTable of
          tbSpecimenCollectors:
          begin
            P := GetKey('people', 'person_id', 'acronym', AnsiUpperCase(eAddChild.Text));
            if P > 0 then
            begin
              dsLink1.DataSet.Append;
              dsLink1.DataSet.FieldByName('person_id').AsInteger := P;
              //TabBperson_name.AsWideString := GetName('people', 'full_name', 'person_id', P);
              dsLink1.DataSet.Post;
              eAddChild.Clear;

              UpdateChildButtons(dsLink1.DataSet);
            end;
          end;
          //tbSamplePreps: ;
        end;
      end;
    end;
    Key := #0;
  end;
end;

procedure TfrmCustomGrid.DBGEditingDone(Sender: TObject);
begin
  EditSourceStr := rsEditedByGrid;

  //if cellMemo.Tag < 0 then
  //  Exit;

  //if not (dsLink.State in [dsEdit, dsInsert]) then
  //  dsLink.DataSet.Edit;
  //
  //dsLink.DataSet.FieldByName(DBG.SelectedField.FieldName).AsString := cellMemo.Text;

  //dsLink.DataSet.Edit;
  //dsLink.DataSet.Fields[cellMemo.Tag].AsString := cellMemo.Lines.Text;
  //dsLink.DataSet.Post;

  // Restore previous row height to default
  //TDBGrid(Sender).BeginUpdate;
  //TDBGrid(Sender).DefaultRowHeight := TDBGrid(Sender).DefaultRowHeight + 1;
  //TDBGrid(Sender).DefaultRowHeight := TDBGrid(Sender).DefaultRowHeight - 1;
  //TDBGrid(Sender).EndUpdate;
  //{$IFNDEF DEBUG}
  //if TStringGrid(Sender).RowCount > TStringGrid(Sender).Row then
  //  TStringGrid(Sender).RowHeights[TStringGrid(Sender).Row] := TStringGrid(Sender).DefaultRowHeight;
  //{$ENDIF}
end;

procedure TfrmCustomGrid.DBGMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);

  function GetNumScrollLines: Integer;
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Result, 0);
  end;

var
  Direction: Shortint;
begin
  Direction := 1;
  if WheelDelta = 0 then
    Exit
  else if WheelDelta > 0 then
    Direction := -1;

  with TDBGrid(Sender) do
  begin
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
      DataSource.DataSet.MoveBy(Direction * GetNumScrollLines);
    Invalidate;
  end;
end;

procedure TfrmCustomGrid.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TimerUpdate.Enabled := False;
  TimerFind.Enabled := False;

  if Assigned(dsLink.DataSet) then
    dsLink.DataSet.Close;

  //tvTaxaFilter.BeginUpdate;
  //try
  //  tvTaxaFilter.Clear;
  //finally
  //  tvTaxaFilter.EndUpdate;
  //end;

  {$IFDEF DEBUG}
  LogDebug('CLOSE: ' + Caption);
  {$ENDIF}

  CloseAction := caFree;
end;

procedure TfrmCustomGrid.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { If editing when closing, ask what the user want to do }
  if (dsLink.State in [dsInsert, dsEdit]) then
  begin
    case MessageDlg(rsModificationsNotSaved, rsPostBeforeClosePrompt, mtConfirmation,
      mbYesNoCancel, 0, mbCancel) of
      { Save modifications and close }
      mrYes:
        begin
          sbSaveRecordClick(nil);
          CanClose := True;
        end;
      { Cancel modifications and close }
      mrNo:
        begin
          sbCancelRecordClick(nil);
          CanClose := True;
        end;
      { Do not close }
      mrCancel:
        CanClose := False;
    end;
  end;
end;

procedure TfrmCustomGrid.FormCreate(Sender: TObject);
begin
  CanToggle := False;
  SearchStr := TSearch.Create;
  Sorting := TSortedFields.Create;
  Modifier.Reset;
  Filtrado := False;
  QuickFilters := TStringList.Create;
  WhereSQL := TStringList.Create;

  OldSidePanel := False;
  OldSideIndex := -1;

  tvTaxaFilter.NodeDataSize := SizeOf(PTaxonNodeData);
  tvSiteFilter.NodeDataSize := SizeOf(PSiteNodeData);
  tvDateFilter.NodeDataSize := SizeOf(PDateNodeData);

  //cellMemo.Tag := -1;
end;

procedure TfrmCustomGrid.FormDestroy(Sender: TObject);
begin
  //if Assigned(DMI) then
  //  FreeAndNil(DMI);
  //if Assigned(DMB) then
  //  FreeAndNil(DMB);

  Sorting.Free;
  FreeAndNil(QuickFilters);
  FreeAndNil(WhereSQL);
  FreeAndNil(FSearch);
  SearchStr.Free;
end;

procedure TfrmCustomGrid.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { FIND = Ctrl + F }
  if (ssCtrl in Shift) and (Key = Ord('F')) then
  begin
    Key := 0;
    if (dsLink.State in [dsInsert, dsEdit]) then
      Exit;

    if frmMain.eSearch.CanSetFocus then
      frmMain.eSearch.SetFocus;
  end;
end;

procedure TfrmCustomGrid.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #27) then     { <ESC> key }
  begin
    if (dsLink.DataSet.State in [dsInsert, dsEdit]) then
    begin
      { CANCEL }
      sbCancelRecordClick(nil);
    end;
    Key := #0;
  end;
end;

procedure TfrmCustomGrid.FormResize(Sender: TObject);
begin
  pSide.Width := Round((ClientWidth - SplitRight.Width - pSideToolbar.Width) * FSidePanelFactor);
end;

procedure TfrmCustomGrid.FormShow(Sender: TObject);
{$IFDEF DEBUG}
var
  Usage: TElapsedTimer;
{$ENDIF}
begin
  FSearch := TCustomSearch.Create(FTableType);

  { Resize panels }
  pSide.Visible := False;
  scrollFilter.VertScrollBar.Position := 0;
  FSidePanelFactor := pSide.Width / (ClientWidth - SplitRight.Width - pSideToolbar.Width);
  FChildPanelFactor := 0.4;
  pChild.Height := Round((pClient.Height - SplitChild.Height) * FChildPanelFactor);
  Application.ProcessMessages;
  {$IFDEF DEBUG}
  Usage := TElapsedTimer.Create(Format('Show %s', [Caption]), 'load master table');
  {$ENDIF}

  { Load datasources }
  SetGridAndChild;

  { Load master grid columns }
  if Assigned(dsLink.DataSet) then
  begin
    {$IFDEF DEBUG}
    Usage.AddPart('load master grid columns');
    {$ENDIF}
    AddGridColumns(FTableType, DBG);
    if not (dsLink.DataSet.Active) then
      dsLink.DataSet.Open;
    //SetGridColumns(FTableType, DBG);
    {$IFDEF DEBUG}
    LogDebug(Format('%s: %d records', [dsLink.DataSet.Name, dsLink.DataSet.RecordCount]));
    {$ENDIF}
    Application.ProcessMessages;
  end;

  { Load child grids columns }
  if Assigned(gridChild1.DataSource) then
  begin
    {$IFDEF DEBUG}
    Usage.AddPart('load detail table');
    {$ENDIF}

    case FTableType of
      tbIndividuals:
      begin
        AddGridColumns(tbCaptures, gridChild1);
        dsLink1.DataSet.Open;
        AddGridColumns(tbMolts, gridChild2);
        dsLink2.DataSet.Open;
        AddGridColumns(tbSightings, gridChild3);
        dsLink3.DataSet.Open;
        AddGridColumns(tbNests, gridChild4);
        dsLink4.DataSet.Open;
        AddGridColumns(tbSpecimens, gridChild5);
        dsLink5.DataSet.Open;
      end;
      tbNests:
      begin
        AddGridColumns(tbNestRevisions, gridChild1);
        dsLink1.DataSet.Open;
        AddGridColumns(tbEggs, gridChild2);
        dsLink2.DataSet.Open;
      end;
      tbSurveys:
      begin
        AddGridColumns(tbSurveyTeams, gridChild1);
        dsLink1.DataSet.Open;
        AddGridColumns(tbNetsEffort, gridChild2);
        dsLink2.DataSet.Open;
        AddGridColumns(tbWeatherLogs, gridChild3);
        dsLink3.DataSet.Open;
        AddGridColumns(tbCaptures, gridChild4);
        dsLink4.DataSet.Open;
        AddGridColumns(tbSightings, gridChild5);
        dsLink5.DataSet.Open;
      end;
      tbSpecimens:
      begin
        AddGridColumns(tbSpecimenCollectors, gridChild1);
        dsLink1.DataSet.Open;
        AddGridColumns(tbSamplePreps, gridChild2);
        dsLink2.DataSet.Open;
      end;
      tbNetStations:
      begin
        AddGridColumns(tbPermanentNets, gridChild1);
        dsLink1.DataSet.Open;
      end;
      tbProjects:
      begin
        AddGridColumns(tbProjectTeams, gridChild1);
        dsLink1.DataSet.Open;
      end;
    end;
    //SetGridColumns(FChildTable, dbgChild);
    Application.ProcessMessages;
  end;

  { Loads images dataset }
  //if (Assigned(imgThumb.DataSource)) and not (imgThumb.DataSource.DataSet.Active) then
  //begin
  //  {$IFDEF DEBUG}
  //  Usage.AddPart('load images table');
  //  {$ENDIF}
  //  dbgImages.DataSource.DataSet.Open;
  //  {$IFDEF DEBUG}
  //  LogDebug(Format('%s: %d records', [dbgImages.DataSource.DataSet.Name, dbgImages.DataSource.DataSet.RecordCount]));
  //  {$ENDIF}
  //end;
  Application.ProcessMessages;

  { Load side panels }
  //navTabs.OptScalePercents := Round(navTabs.OptScalePercents * navTabs.ScaleFactor);
  LoadRecordColumns;
  LoadRecordRow;
  UpdateFilterPanels;
  UpdateButtons(dsLink.DataSet);
  UpdateChildCount;
  if DBG.CanSetFocus then
    DBG.SetFocus;
  CanToggle := True;
  Application.ProcessMessages;

  {$IFDEF DEBUG}
  Usage.StopTimer;
  FreeAndNil(Usage);
  {$ENDIF}
  //TimerUpdate.Enabled := True;
end;

procedure TfrmCustomGrid.pChildTag1Click(Sender: TObject);
var
  aTag, aCountTag: TBCPanel;
begin
  if (Sender = pChildTag1) or (Sender = lblChildTag1) or (Sender = pChildCount1) or (Sender = lblChildCount1) then
  begin
    aTag := pChildTag1;
    aCountTag := pChildCount1;
  end
  else
  if (Sender = pChildTag2) or (Sender = lblChildTag2) or (Sender = pChildCount2) or (Sender = lblChildCount2) then
  begin
    aTag := pChildTag2;
    aCountTag := pChildCount2;
  end
  else
  if (Sender = pChildTag3) or (Sender = lblChildTag3) or (Sender = pChildCount3) or (Sender = lblChildCount3) then
  begin
    aTag := pChildTag3;
    aCountTag := pChildCount3;
  end
  else
  if (Sender = pChildTag4) or (Sender = lblChildTag4) or (Sender = pChildCount4) or (Sender = lblChildCount4) then
  begin
    aTag := pChildTag4;
    aCountTag := pChildCount4;
  end
  else
  if (Sender = pChildTag5) or (Sender = lblChildTag5) or (Sender = pChildCount5) or (Sender = lblChildCount5) then
  begin
    aTag := pChildTag5;
    aCountTag := pChildCount5;
  end;

  ChildTagClick(aTag, aCountTag);
  eAddChild.Visible := False;

  case FTableType of
    tbIndividuals:
      case nbChilds.PageIndex of
        0: FChildTable := tbCaptures;
        1: FChildTable := tbMolts;
        2: FChildTable := tbSightings;
        3: FChildTable := tbNests;
        4: FChildTable := tbSpecimens;
      end;
    tbNests:
      case nbChilds.PageIndex of
        0: FChildTable := tbNestRevisions;
        1: FChildTable := tbEggs;
      end;
    tbExpeditions:
      case nbChilds.PageIndex of
        0: FChildTable := tbSurveys;
      end;
    tbSurveys:
      case nbChilds.PageIndex of
        0:
        begin
          FChildTable := tbSurveyTeams;
          eAddChild.Visible := True;
        end;
        1: FChildTable := tbNetsEffort;
        2: FChildTable := tbWeatherLogs;
        3: FChildTable := tbCaptures;
        4: FChildTable := tbSightings;
      end;
    tbSpecimens:
      case nbChilds.PageIndex of
        0:
        begin
          FChildTable := tbSpecimenCollectors;
          eAddChild.Visible := True;
        end;
        1: FChildTable := tbSamplePreps;
      end;
    tbNetStations:
      case nbChilds.PageIndex of
        0: FChildTable := tbPermanentNets;
      end;
    tbProjects:
      case nbChilds.PageIndex of
        0:
        begin
          FChildTable := tbProjectTeams;
          eAddChild.Visible := True;
        end;
      end;
  end;

  case nbChilds.PageIndex of
    0: UpdateChildButtons(dsLink1.DataSet);
    1: UpdateChildButtons(dsLink2.DataSet);
    2: UpdateChildButtons(dsLink3.DataSet);
    3: UpdateChildButtons(dsLink4.DataSet);
    4: UpdateChildButtons(dsLink5.DataSet);
  end;
end;

procedure TfrmCustomGrid.pChildTag1MouseEnter(Sender: TObject);
var
  aTag, aCountTag: TBCPanel;
begin
  if (Sender = pChildTag1) or (Sender = lblChildTag1) or (Sender = pChildCount1) or (Sender = lblChildCount1) then
  begin
    aTag := pChildTag1;
    aCountTag := pChildCount1;
  end
  else
  if (Sender = pChildTag2) or (Sender = lblChildTag2) or (Sender = pChildCount2) or (Sender = lblChildCount2) then
  begin
    aTag := pChildTag2;
    aCountTag := pChildCount2;
  end
  else
  if (Sender = pChildTag3) or (Sender = lblChildTag3) or (Sender = pChildCount3) or (Sender = lblChildCount3) then
  begin
    aTag := pChildTag3;
    aCountTag := pChildCount3;
  end
  else
  if (Sender = pChildTag4) or (Sender = lblChildTag4) or (Sender = pChildCount4) or (Sender = lblChildCount4) then
  begin
    aTag := pChildTag4;
    aCountTag := pChildCount4;
  end
  else
  if (Sender = pChildTag5) or (Sender = lblChildTag5) or (Sender = pChildCount5) or (Sender = lblChildCount5) then
  begin
    aTag := pChildTag5;
    aCountTag := pChildCount5;
  end;

  ChildTagMouseEnter(aTag, aCountTag);
end;

procedure TfrmCustomGrid.pChildTag1MouseLeave(Sender: TObject);
var
  aTag, aCountTag: TBCPanel;
begin
  if (Sender = pChildTag1) or (Sender = lblChildTag1) or (Sender = pChildCount1) or (Sender = lblChildCount1) then
  begin
    aTag := pChildTag1;
    aCountTag := pChildCount1;
  end
  else
  if (Sender = pChildTag2) or (Sender = lblChildTag2) or (Sender = pChildCount2) or (Sender = lblChildCount2) then
  begin
    aTag := pChildTag2;
    aCountTag := pChildCount2;
  end
  else
  if (Sender = pChildTag3) or (Sender = lblChildTag3) or (Sender = pChildCount3) or (Sender = lblChildCount3) then
  begin
    aTag := pChildTag3;
    aCountTag := pChildCount3;
  end
  else
  if (Sender = pChildTag4) or (Sender = lblChildTag4) or (Sender = pChildCount4) or (Sender = lblChildCount4) then
  begin
    aTag := pChildTag4;
    aCountTag := pChildCount4;
  end
  else
  if (Sender = pChildTag5) or (Sender = lblChildTag5) or (Sender = pChildCount5) or (Sender = lblChildCount5) then
  begin
    aTag := pChildTag5;
    aCountTag := pChildCount5;
  end;

  ChildTagMouseLeave(aTag, aCountTag);
end;

procedure TfrmCustomGrid.pClientResize(Sender: TObject);
begin
  pChild.Height := Round((pClient.Height - SplitChild.Height) * FChildPanelFactor);
end;

procedure TfrmCustomGrid.pmcNewCaptureClick(Sender: TObject);
begin
  case FTableType of
    tbIndividuals:
      EditCapture(DMI.qCaptures, dsLink.DataSet.FieldByName('individual_id').AsInteger, True);
    tbSurveys:
      EditCapture(DMS.qCaptures, dsLink.DataSet.FieldByName('survey_id').AsInteger, True);
  end;

  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
end;

procedure TfrmCustomGrid.pmcNewCollectorClick(Sender: TObject);
begin
  EditCollector(DMG.qSampleCollectors, dsLink.DataSet.FieldByName('specimen_id').AsInteger, True);

  UpdateChildButtons(DMG.qSampleCollectors);
end;

procedure TfrmCustomGrid.pmcNewEggClick(Sender: TObject);
begin
  EditEgg(DMB.qEggs, dsLink.DataSet.FieldByName('nest_id').AsInteger, True);

  UpdateChildButtons(DMB.qEggs);
end;

procedure TfrmCustomGrid.pmcNewMistnetClick(Sender: TObject);
begin
  EditNetEffort(DMS.qNetsEffort, dsLink.DataSet.FieldByName('survey_id').AsInteger, True);

  UpdateChildButtons(DMS.qNetsEffort);
end;

procedure TfrmCustomGrid.pmcNewMoltClick(Sender: TObject);
begin
  EditMolt(DMI.qMolts, dsLink.DataSet.FieldByName('individual_id').AsInteger, True);

  UpdateChildButtons(DMI.qMolts);
end;

procedure TfrmCustomGrid.pmcNewNestClick(Sender: TObject);
begin
  EditNest(DMI.qNests, True);

  UpdateChildButtons(DMI.qNests);
end;

procedure TfrmCustomGrid.pmcNewNestRevisionClick(Sender: TObject);
begin
  EditNestRevision(DMB.qNestRevisions, dsLink.DataSet.FieldByName('nest_id').AsInteger, True);

  UpdateChildButtons(DMB.qNestRevisions);
end;

procedure TfrmCustomGrid.pmcNewPermanentNetClick(Sender: TObject);
begin
  EditPermanentNet(DMG.qPermanentNets, dsLink.DataSet.FieldByName('net_station_id').AsInteger, True);

  UpdateChildButtons(DMG.qPermanentNets);
end;

procedure TfrmCustomGrid.pmcNewProjectMemberClick(Sender: TObject);
begin
  EditProjectMember(DMG.qProjectTeam, dsLink.DataSet.FieldByName('project_id').AsInteger, True);

  UpdateChildButtons(DMG.qProjectTeam);
end;

procedure TfrmCustomGrid.pmcNewSamplePrepClick(Sender: TObject);
begin
  EditSamplePrep(DMG.qSamplePreps, dsLink.DataSet.FieldByName('specimen_id').AsInteger, True);

  UpdateChildButtons(DMG.qSamplePreps);
end;

procedure TfrmCustomGrid.pmcNewSightingClick(Sender: TObject);
begin
  case FTableType of
    tbIndividuals:
      EditSighting(DMI.qSightings, dsLink.DataSet.FieldByName('individual_id').AsInteger, True);
    tbSurveys:
      EditSighting(DMS.qSightings, dsLink.DataSet.FieldByName('survey_id').AsInteger, True);
  end;

  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
end;

procedure TfrmCustomGrid.pmcNewSpecimenClick(Sender: TObject);
begin
  //EditSpecimen(DMI.qSpecimens, True);
end;

procedure TfrmCustomGrid.pmcNewSurveyMemberClick(Sender: TObject);
begin
  EditSurveyMember(DMS.qSurveyTeam, dsLink.DataSet.FieldByName('survey_id').AsInteger, True);

  UpdateChildButtons(DMS.qSurveyTeam);
end;

procedure TfrmCustomGrid.pmcNewWeatherLogClick(Sender: TObject);
begin
  EditWeatherLog(DMS.qWeatherLogs, dsLink.DataSet.FieldByName('survey_id').AsInteger, True);

  UpdateChildButtons(DMS.qWeatherLogs);
end;

procedure TfrmCustomGrid.sbAddChildClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmAddChild.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbCancelRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  if (dsLink.DataSet.Modified) and (XSettings.ConfirmCancel) then
  begin
    if not MsgDlg(rsDiscardChangesTitle, rsCancelEditingPrompt, mtConfirmation) then
      Exit;
  end;

  dsLink.DataSet.Cancel;
  Working := False;
end;

procedure TfrmCustomGrid.sbColumnHideClick(Sender: TObject);
begin
  if DBG.SelectedIndex > -1 then
    DBG.SelectedColumn.Visible := False;
end;

procedure TfrmCustomGrid.sbColumnWidthAutoAdjustClick(Sender: TObject);
begin
  if DBG.SelectedIndex > -1 then
    DBG.AutoAdjustColumns;
end;

procedure TfrmCustomGrid.sbDelChildClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbNetStations:
      case nbChilds.PageIndex of
        0: DeleteRecord(tbPermanentNets, DMG.qPermanentNets);
      end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects: ;
    //tbProjectTeams: ;
    //tbPermits: ;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    //tbBotanicTaxa: ;
    //tbBands: ;
    //tbBandHistory: ;
    tbIndividuals:
      case nbChilds.PageIndex of
        0: DeleteRecord(tbCaptures, DMI.qCaptures);
        1: DeleteRecord(tbMolts, DMI.qMolts);
        2: DeleteRecord(tbSightings, DMI.qSightings);
        3: DeleteRecord(tbNests, DMI.qNests);
        4: DeleteRecord(tbSpecimens, DMI.qSpecimens);
      end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
      case nbChilds.PageIndex of
        0: DeleteRecord(tbNestRevisions, DMB.qNestRevisions);
        1: DeleteRecord(tbEggs, DMB.qEggs);
      end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    //tbExpeditions: ;
    tbSurveys:
      case nbChilds.PageIndex of
        0: DeleteRecord(tbSurveyTeams, DMS.qSurveyTeam);
        1: DeleteRecord(tbNetsEffort, DMS.qNetsEffort);
        2: DeleteRecord(tbWeatherLogs, DMS.qWeatherLogs);
        3: DeleteRecord(tbCaptures, DMS.qCaptures);
        4: DeleteRecord(tbSightings, DMS.qSightings);
      end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
      case nbChilds.PageIndex of
        0: DeleteRecord(tbSamplePreps, DMG.qSamplePreps);
      end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
  Working := False;
end;

procedure TfrmCustomGrid.sbDelRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  DeleteRecord(FTableType, dsLink.DataSet);
  UpdateButtons(dsLink.DataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbEditChildClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbNetStations:
      case nbChilds.PageIndex of
        0: EditPermanentNet(DMG.qPermanentNets, dsLink.DataSet.FieldByName('net_station_id').AsInteger);
      end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects: ;
    //tbProjectTeams: ;
    //tbPermits: ;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    //tbBotanicTaxa: ;
    //tbBands: ;
    //tbBandHistory: ;
    tbIndividuals:
      case nbChilds.PageIndex of
        0: EditCapture(DMI.qCaptures, dsLink.DataSet.FieldByName('individual_id').AsInteger);
        1: EditMolt(DMI.qMolts, dsLink.DataSet.FieldByName('individual_id').AsInteger);
        2: EditSighting(DMI.qSightings, dsLink.DataSet.FieldByName('individual_id').AsInteger);
        3: EditNest(DMI.qNests, False);
        //4: EditSpecimen(DMI.qSpecimens, False);
      end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
      case nbChilds.PageIndex of
        0: EditNestRevision(DMB.qNestRevisions, dsLink.DataSet.FieldByName('nest_id').AsInteger);
        1: EditEgg(DMB.qEggs, dsLink.DataSet.FieldByName('nest_id').AsInteger);
      end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    //tbExpeditions: ;
    tbSurveys:
      case nbChilds.PageIndex of
        0: EditSurveyMember(DMS.qSurveyTeam, dsLink.DataSet.FieldByName('survey_id').AsInteger);
        1: EditNetEffort(DMS.qNetsEffort, dsLink.DataSet.FieldByName('survey_id').AsInteger);
        2: EditWeatherLog(DMS.qWeatherLogs, dsLink.DataSet.FieldByName('survey_id').AsInteger);
        3: EditCapture(DMS.qCaptures, dsLink.DataSet.FieldByName('survey_id').AsInteger);
        4: EditSighting(DMS.qSightings, dsLink.DataSet.FieldByName('survey_id').AsInteger);
      end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
      case nbChilds.PageIndex of
        0: EditSamplePrep(DMG.qSamplePreps, dsLink.DataSet.FieldByName('specimen_id').AsInteger);
      end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
  Working := False;
end;

procedure TfrmCustomGrid.sbEditRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    tbGazetteer: EditSite(dsLink.DataSet);
    tbNetStations: EditNetStation(dsLink.DataSet);
    //tbPermanentNets: ;
    tbInstitutions: EditInstitution(dsLink.DataSet);
    tbPeople: EditPerson(dsLink.DataSet);
    tbProjects: EditProject(dsLink.DataSet);
    //tbProjectTeams: ;
    tbPermits: EditPermit(dsLink.DataSet);
    tbTaxonRanks: ;
    //tbZooTaxa: ;
    tbBotanicTaxa: EditBotanicTaxon(dsLink.DataSet);
    tbBands: EditBand(dsLink.DataSet);
    //tbBandHistory: ;
    tbIndividuals: EditIndividual(dsLink.DataSet);
    tbCaptures: EditCapture(dsLink.DataSet);
    tbMolts: EditMolt(dsLink.DataSet);
    tbNests: EditNest(dsLink.DataSet);
    tbNestRevisions: EditNestRevision(dsLink.DataSet);
    tbEggs: EditEgg(dsLink.DataSet);
    tbMethods: EditMethod(dsLink.DataSet);
    tbExpeditions: EditExpedition(dsLink.DataSet);
    tbSurveys: EditSurvey(dsLink.DataSet);
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: EditSighting(dsLink.DataSet);
    tbSpecimens: EditSpecimen(dsLink.DataSet);
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;
  Working := False;
end;

procedure TfrmCustomGrid.sbFilterRecordsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmFilter.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbFirstChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  aDataSet := GetChildDataSet;

  if Assigned(aDataSet) then
    aDataSet.First;

  UpdateChildButtons(aDataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbFirstRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  dsLink.DataSet.First;
  UpdateButtons(dsLink.DataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbInsertRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    tbGazetteer: EditSite(dsLink.DataSet, True);
    tbNetStations: EditNetStation(dsLink.DataSet, True);
    //tbPermanentNets: ;
    tbInstitutions: EditInstitution(dsLink.DataSet, True);
    tbPeople: EditPerson(dsLink.DataSet, True);
    tbProjects: EditProject(dsLink.DataSet, True);
    //tbProjectTeams: ;
    tbPermits: EditPermit(dsLink.DataSet, 0, True);
    tbTaxonRanks: ;
    //tbZooTaxa: ;
    tbBotanicTaxa: EditBotanicTaxon(dsLink.DataSet, True);
    tbBands: EditBand(dsLink.DataSet, True);
    //tbBandHistory: ;
    tbIndividuals: EditIndividual(dsLink.DataSet, True);
    tbCaptures: EditCapture(dsLink.DataSet, 0, True);
    tbMolts: EditMolt(dsLink.DataSet, 0, True);
    tbNests: EditNest(dsLink.DataSet, True);
    tbNestRevisions: EditNestRevision(dsLink.DataSet, 0, True);
    tbEggs: EditEgg(dsLink.DataSet, 0, True);
    tbMethods: EditMethod(dsLink.DataSet, True);
    tbExpeditions: EditExpedition(dsLink.DataSet, True);
    tbSurveys: EditSurvey(dsLink.DataSet, True);
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: EditSighting(dsLink.DataSet, 0, True);
    tbSpecimens: EditSpecimen(dsLink.DataSet, True);
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;
  Working := False;
end;

procedure TfrmCustomGrid.sbLastChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  aDataSet := GetChildDataSet;

  if Assigned(aDataSet) then
    aDataSet.Last;

  UpdateChildButtons(aDataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbLastRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  dsLink.DataSet.Last;
  UpdateButtons(dsLink.DataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbNextChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  aDataSet := GetChildDataSet;

  if Assigned(aDataSet) then
    aDataSet.Next;

  UpdateChildButtons(aDataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbNextRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  dsLink.DataSet.Next;
  UpdateButtons(dsLink.DataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbPriorChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  aDataSet := GetChildDataSet;

  if Assigned(aDataSet) then
    aDataSet.Prior;

  UpdateChildButtons(aDataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbPriorRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  dsLink.DataSet.Prior;
  UpdateButtons(dsLink.DataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbRecordHistoryClick(Sender: TObject);
var
  aKeyField: String;
begin
  aKeyField := GetPrimaryKey(TableNames[FTableType]);
  ShowHistory(FTableType, dsLink.DataSet.FieldByName(aKeyField).AsInteger);
end;

procedure TfrmCustomGrid.sbChildHistoryClick(Sender: TObject);
var
  aKeyField: String;
  aDataSet: TDataSet;
begin
  aKeyField := GetPrimaryKey(TableNames[FChildTable]);
  case nbChilds.PageIndex of
    0: aDataSet := gridChild1.DataSource.DataSet;
    1: aDataSet := gridChild2.DataSource.DataSet;
    2: aDataSet := gridChild3.DataSource.DataSet;
    3: aDataSet := gridChild4.DataSource.DataSet;
    4: aDataSet := gridChild5.DataSource.DataSet;
  end;
  ShowHistory(FChildTable, aDataSet.FieldByName(aKeyField).AsInteger);
end;

procedure TfrmCustomGrid.sbRefreshRecordsClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  dsLink.DataSet.Refresh;
  UpdateButtons(dsLink.DataSet);
  Working := False;
end;

procedure TfrmCustomGrid.sbRowHeightDecreaseClick(Sender: TObject);
begin
  DBG.DefaultRowHeight := DBG.DefaultRowHeight - 2;
end;

procedure TfrmCustomGrid.sbRowHeightDefaultClick(Sender: TObject);
begin
  DBG.DefaultRowHeight := 25;
end;

procedure TfrmCustomGrid.sbRowHeightIncreaseClick(Sender: TObject);
begin
  DBG.DefaultRowHeight := DBG.DefaultRowHeight + 2;
end;

procedure TfrmCustomGrid.sbSaveRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  dsLink.DataSet.Post;
  Working := False;
end;

procedure TfrmCustomGrid.sbShowRecordClick(Sender: TObject);
begin
  pSide.Visible := (Sender as TSpeedButton).Down;
  SplitRight.Visible := pSide.Visible;

  if (Sender as TSpeedButton).Down then
    cpSide.PageIndex := (Sender as TSpeedButton).Tag;

  frmMain.UpdateMenu(frmMain.PGW.ActivePageComponent);
end;

procedure TfrmCustomGrid.sbSortChildsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmSort.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbSortRecordsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmSort.Popup(X, Y);
end;

procedure TfrmCustomGrid.SplitChildMoved(Sender: TObject);
begin
  FChildPanelFactor := pChild.Height / (pClient.Height - SplitChild.Height);
end;

procedure TfrmCustomGrid.SplitRightMoved(Sender: TObject);
begin
  FSidePanelFactor := pSide.Width / (ClientWidth - SplitRight.Width);
end;

procedure TfrmCustomGrid.TimerFindTimer(Sender: TObject);
{$IFDEF DEBUG}
var
  Usage: TElapsedTimer;
{$ENDIF}
begin
  TimerFind.Enabled := False;
  if not CanToggle then
    Exit;

  {$IFDEF DEBUG}
  Usage := TElapsedTimer.Create('Search');
  {$ENDIF}

  //Search(EP.Text);

  {$IFDEF DEBUG}
  Usage.StopTimer;
  FreeAndNil(Usage);
  {$ENDIF}
end;

procedure TfrmCustomGrid.tsBandNotReportedOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsBandNotReportedOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsBandReportedOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsBandReportedOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsfMarkedOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsfMarkedOn(Sender: TObject);
begin
  if (tsfUnmarked.StateOn = sw_on) then
    tsfUnmarked.StateOn := sw_off;

  if (tsfMarked.StateOn = sw_on) then
  begin
    //FSearch.QuickFilters.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean, crEqual,
    //  False, '1'));
  end;

  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsfUnmarkedOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsfUnmarkedOn(Sender: TObject);
begin
  if (tsfMarked.StateOn = sw_on) then
    tsfMarked.StateOn := sw_off;

  if (tsfUnmarked.StateOn = sw_on) then
    //FSearch.QuickFilters.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean, crEqual,
    //  False, '0'));

  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsfWithColorBandsFilterOn(Sender: TObject);
begin
  if tsfWithColorBandsFilter.StateOn = sw_on then
  begin
    //FSearch.QuickFilters.Add(TSearchField.Create('right_leg_below', 'Right tarsus', sdtText, crDistinct,
    //  False, ''));
    //FSearch.QuickFilters.Add(TSearchField.Create('left_leg_below', 'Left tarsus', sdtText, crDistinct,
    //  False, ''));
    //FSearch.QuickFilters.Add(TSearchField.Create('right_leg_above', 'Right tibia', sdtText, crDistinct,
    //  False, ''));
    //FSearch.QuickFilters.Add(TSearchField.Create('left_leg_above', 'Left tibia', sdtText, crDistinct,
    //  False, ''));
  end;
end;

procedure TfrmCustomGrid.tsfWithRecapturesFilterOn(Sender: TObject);
begin
  Search(FSearchString);
  //if tsfWithRecapturesFilter.StateOn = sw_on then
    //FSearch.QuickFilters.Add(TSearchField.Create('captures_tally', '# of captures', sdtInteger, crMoreThan,
    //  False, '1'));
end;

procedure TfrmCustomGrid.tsHasSynonymsOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsHasSynonymsOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsIsSynonymOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsIsSynonymOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonExtinctOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonExtinctOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonomyCbroOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonomyCbroOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonomyClementsOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonomyClementsOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonomyIocOff(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tsTaxonomyIocOn(Sender: TObject);
begin
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tvDateFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSiteNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Checked := Node^.CheckState = csCheckedNormal;

  if TBaseVirtualTree(Sender).GetNodeLevel(Node) = 2 then
  begin
    if TBaseVirtualTree(Sender).CheckState[Node] = csCheckedNormal then
    begin
      TBaseVirtualTree(Sender).CheckState[TBaseVirtualTree(Sender).NodeParent[Node]] := csMixedNormal;
      TBaseVirtualTree(Sender).CheckState[TBaseVirtualTree(Sender).NodeParent[TBaseVirtualTree(Sender).NodeParent[Node]]] := csMixedNormal;
    end
    else
    begin
      TBaseVirtualTree(Sender).CheckState[TBaseVirtualTree(Sender).NodeParent[Node]] := csUncheckedNormal;
      TBaseVirtualTree(Sender).CheckState[TBaseVirtualTree(Sender).NodeParent[TBaseVirtualTree(Sender).NodeParent[Node]]] := csUncheckedNormal;
    end;
  end
  else
  if TBaseVirtualTree(Sender).GetNodeLevel(Node) = 1 then
  begin
    if TBaseVirtualTree(Sender).CheckState[Node] = csCheckedNormal then
    begin
      TBaseVirtualTree(Sender).CheckState[TBaseVirtualTree(Sender).NodeParent[Node]] := csMixedNormal;
    end
    else
    begin
      TBaseVirtualTree(Sender).CheckState[TBaseVirtualTree(Sender).NodeParent[Node]] := csUncheckedNormal;
    end;
  end;

  GetFilters(QuickFilters);
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tvDateFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := CanToggle;
end;

procedure TfrmCustomGrid.tvDateFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PDateNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Free;
end;

procedure TfrmCustomGrid.tvDateFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PDateNodeData;
begin
  Data := Sender.GetNodeData(Node);

  CellText := Data^.Caption;
end;

procedure TfrmCustomGrid.tvDateFilterInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
end;

procedure TfrmCustomGrid.tvSiteFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSiteNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Checked := Node^.CheckState = csCheckedNormal;

  GetFilters(QuickFilters);
  Search(FSearchString);
end;

procedure TfrmCustomGrid.tvSiteFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := CanToggle;
end;

procedure TfrmCustomGrid.tvSiteFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSiteNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Free;
end;

procedure TfrmCustomGrid.tvSiteFilterGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PSiteNodeData;
begin
  Data := Sender.GetNodeData(Node);
  ImageIndex := Data^.ImageIndex;
end;

procedure TfrmCustomGrid.tvSiteFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PSiteNodeData;
begin
  Data := Sender.GetNodeData(Node);

  CellText := Data^.Caption;
end;

procedure TfrmCustomGrid.tvSiteFilterInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
end;

procedure TfrmCustomGrid.tvTaxaFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTaxonNodeData;
begin
  if not CanToggle then
    Exit;

  Data := Sender.GetNodeData(Node);
  Data^.Checked := Node^.CheckState = csCheckedNormal;

  GetFilters(QuickFilters);
  Search(FSearchString);

end;

procedure TfrmCustomGrid.tvTaxaFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := CanToggle;
end;

procedure TfrmCustomGrid.tvTaxaFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTaxonNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Free;
end;

procedure TfrmCustomGrid.tvTaxaFilterGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTaxonNodeData;
begin
  Data := Sender.GetNodeData(Node);
  ImageIndex := Data^.ImageIndex;
end;

procedure TfrmCustomGrid.tvTaxaFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PTaxonNodeData;
begin
  Data := Sender.GetNodeData(Node);

  CellText := Data^.Caption;
end;

procedure TfrmCustomGrid.tvTaxaFilterInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
end;

procedure TfrmCustomGrid.tvTaxaFilterPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Level: Integer;
begin
  Level := Sender.GetNodeLevel(Node);
  if Level > 1 then
    TargetCanvas.Font.Style := Font.Style + [fsItalic];
end;

procedure TfrmCustomGrid.UpdateButtons(aDataSet: TDataSet);
begin
  if (aDataSet.State in [dsInsert, dsEdit]) then
  begin
    sbEditRecord.Enabled := False;
    sbDelRecord.Enabled := False;
    sbFirstRecord.Enabled := False;
    sbPriorRecord.Enabled := False;
    sbNextRecord.Enabled := False;
    sbLastRecord.Enabled := False;
    sbRecordHistory.Enabled := False;
    sbSortRecords.Enabled := False;

    sbShowQuickFilters.Enabled := False;
    sbShowImages.Enabled := False;
    sbShowAudio.Enabled := False;
    sbShowDocs.Enabled := False;
    sbShowSummary.Enabled := False;
    sbShowRecycle.Enabled := False;

    sbCancelRecord.Visible := True;
    sbSaveRecord.Visible := True;

    pmgRefresh.Enabled := False;

    //navGrid.Enabled := False;
    pSide.Enabled := False;
  end
  else
  begin
    if (aDataSet.Active) and not (TSQLQuery(aDataSet).ReadOnly) then
    begin
      sbEditRecord.Enabled := (aDataSet.RecordCount > 0);
      sbDelRecord.Enabled := (aDataSet.RecordCount > 0);
      sbRecordHistory.Enabled := (aDataSet.RecordCount > 0);
      sbSortRecords.Enabled := (aDataSet.RecordCount > 0);
    end
    else
    begin
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;
      sbRecordHistory.Enabled := False;
      sbSortRecords.Enabled := False;
    end;
    sbFirstRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbPriorRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbNextRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
    sbLastRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

    sbShowQuickFilters.Enabled := True;
    sbShowImages.Enabled := True;
    sbShowAudio.Enabled := True;
    sbShowDocs.Enabled := True;
    sbShowSummary.Enabled := True;
    sbShowRecycle.Enabled := True;

    pmgRefresh.Enabled := True;

    sbSaveRecord.Visible := False;
    sbCancelRecord.Visible := False;

    //navGrid.Enabled := True;
    pSide.Enabled := True;
  end;
  pmgEdit.Enabled := sbEditRecord.Enabled;
  pmgDel.Enabled := sbDelRecord.Enabled;
  pmgRecordHistory.Enabled := sbChildHistory.Enabled;

  if dsLink.DataSet.RecordCount = 1 then
    lblRecordStatus.Caption := Format(rsRecordsFound, [dsLink.DataSet.RecordCount, rsRecords])
  else
  if dsLink.DataSet.RecordCount > 1 then
    lblRecordStatus.Caption := Format(rsRecordsFound, [dsLink.DataSet.RecordCount, rsRecordsPlural])
  else
    lblRecordStatus.Caption := rsNoRecordsFound;
end;

procedure TfrmCustomGrid.UpdateChildButtons(aDataSet: TDataSet);
begin
  if Closing then
    Exit;

  if (aDataSet.State in [dsInsert, dsEdit]) then
  begin
    sbAddChild.Enabled := False;
    sbEditChild.Enabled := False;
    sbDelChild.Enabled := False;
    sbFirstChild.Enabled := False;
    sbPriorChild.Enabled := False;
    sbNextChild.Enabled := False;
    sbLastChild.Enabled := False;
    sbChildHistory.Enabled := False;

    pSide.Enabled := False;
  end
  else
  begin
    if (aDataSet.Active) and not (TSQLQuery(aDataSet).ReadOnly) then
    begin
      sbAddChild.Enabled := (aDataSet.RecordCount > 0);
      sbEditChild.Enabled := (aDataSet.RecordCount > 0);
      sbDelChild.Enabled := (aDataSet.RecordCount > 0);
      sbChildHistory.Enabled := (aDataSet.RecordCount > 0);
    end
    else
    begin
      sbAddChild.Enabled := False;
      sbEditChild.Enabled := False;
      sbDelChild.Enabled := False;
      sbChildHistory.Enabled := False;
    end;
    sbFirstChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbPriorChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbNextChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
    sbLastChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

    pSide.Enabled := True;
  end;

  UpdateChildCount;
end;

procedure TfrmCustomGrid.UpdateChildCount;
begin
  if Closing then
    Exit;

  if not dsLink.DataSet.Active then
  begin
    pChildCount1.Visible := False;
    pChildCount2.Visible := False;
    pChildCount3.Visible := False;
    pChildCount4.Visible := False;
    pChildCount5.Visible := False;
    Exit;
  end;

  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbNetStations:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;
    end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;
    end;
    //tbProjectTeams: ;
    //tbPermits: ;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    //tbBotanicTaxa: ;
    //tbBands: ;
    //tbBandHistory: ;
    tbIndividuals:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;

      if dsLink2.DataSet.RecordCount > 0 then
      begin
        lblChildCount2.Caption := IntToStr(dsLink2.DataSet.RecordCount);
        pChildCount2.Visible := True;
      end
      else
        pChildCount2.Visible := False;

      if dsLink3.DataSet.RecordCount > 0 then
      begin
        lblChildCount3.Caption := IntToStr(dsLink3.DataSet.RecordCount);
        pChildCount3.Visible := True;
      end
      else
        pChildCount3.Visible := False;

      if dsLink4.DataSet.RecordCount > 0 then
      begin
        lblChildCount4.Caption := IntToStr(dsLink4.DataSet.RecordCount);
        pChildCount4.Visible := True;
      end
      else
        pChildCount4.Visible := False;

      if dsLink5.DataSet.RecordCount > 0 then
      begin
        lblChildCount5.Caption := IntToStr(dsLink5.DataSet.RecordCount);
        pChildCount5.Visible := True;
      end
      else
        pChildCount5.Visible := False;
    end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;

      if dsLink2.DataSet.RecordCount > 0 then
      begin
        lblChildCount2.Caption := IntToStr(dsLink2.DataSet.RecordCount);
        pChildCount2.Visible := True;
      end
      else
        pChildCount2.Visible := False;
    end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    //tbExpeditions: ;
    tbSurveys:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;

      if dsLink2.DataSet.RecordCount > 0 then
      begin
        lblChildCount2.Caption := IntToStr(dsLink2.DataSet.RecordCount);
        pChildCount2.Visible := True;
      end
      else
        pChildCount2.Visible := False;

      if dsLink3.DataSet.RecordCount > 0 then
      begin
        lblChildCount3.Caption := IntToStr(dsLink3.DataSet.RecordCount);
        pChildCount3.Visible := True;
      end
      else
        pChildCount3.Visible := False;

      if dsLink4.DataSet.RecordCount > 0 then
      begin
        lblChildCount4.Caption := IntToStr(dsLink4.DataSet.RecordCount);
        pChildCount4.Visible := True;
      end
      else
        pChildCount4.Visible := False;

      if dsLink5.DataSet.RecordCount > 0 then
      begin
        lblChildCount5.Caption := IntToStr(dsLink5.DataSet.RecordCount);
        pChildCount5.Visible := True;
      end
      else
        pChildCount5.Visible := False;
    end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;

      if dsLink2.DataSet.RecordCount > 0 then
      begin
        lblChildCount2.Caption := IntToStr(dsLink2.DataSet.RecordCount);
        pChildCount2.Visible := True;
      end
      else
        pChildCount2.Visible := False;
    end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  UpdateChildStatus;
end;

procedure TfrmCustomGrid.UpdateChildStatus;
var
  DS: TDataSource;
begin
  if FChildTable = tbNone then
    Exit;

  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbNetStations:
    begin
      DS := dsLink1;
    end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects:
    begin
      DS := dsLink1;
    end;
    //tbProjectTeams: ;
    //tbPermits: ;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    //tbBotanicTaxa: ;
    //tbBands: ;
    //tbBandHistory: ;
    tbIndividuals:
    begin
      case nbChilds.PageIndex of
        0: DS := dsLink1;
        1: DS := dsLink2;
        2: DS := dsLink3;
        3: DS := dsLink4;
        4: DS := dsLink5;
      end;
    end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
    begin
      case nbChilds.PageIndex of
        0: DS := dsLink1;
        1: DS := dsLink2;
      end;
    end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    tbExpeditions:
    begin
      DS := dsLink1;
    end;
    tbSurveys:
    begin
      case nbChilds.PageIndex of
        0: DS := dsLink1;
        1: DS := dsLink2;
        2: DS := dsLink3;
        3: DS := dsLink4;
        4: DS := dsLink5;
      end;
    end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
    begin
      case nbChilds.PageIndex of
        0: DS := dsLink1;
        1: DS := dsLink2;
      end;
    end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  if DS.DataSet.RecordCount = 1 then
    lblChildStatus.Caption := Format(rsRecordsFound, [DS.DataSet.RecordCount, rsRecords])
  else
  if DS.DataSet.RecordCount > 1 then
    lblChildStatus.Caption := Format(rsRecordsFound, [DS.DataSet.RecordCount, rsRecordsPlural])
  else
    lblChildStatus.Caption := rsNoRecordsFound;
end;

procedure TfrmCustomGrid.SetSidePanel(aValue: Boolean);
begin
  if FSidePanel <> OldSidePanel then
    OldSidePanel := FSidePanel;
  FSidePanel := aValue;
  pSide.Visible := FSidePanel;
  SplitRight.Visible := pSide.Visible;
end;

procedure TfrmCustomGrid.SetSideIndex(aValue: Integer);
begin
  if ShowSidePanel then
  begin
    if FSideIndex <> OldSideIndex then
      OldSideIndex := FSideIndex;
    FSideIndex := aValue;
    cpSide.PageIndex := FSideIndex;
  end;
end;

procedure TfrmCustomGrid.SetSearchString(aValue: String);
begin
  if not CanToggle then
    Exit;

  if FSearchString <> OldSearchString then
    OldSearchString := FSearchString;
  FSearchString := aValue;

  Search(FSearchString);
end;

procedure TfrmCustomGrid.SetGridAndChild;
begin
  FChildTable := tbNone;

  case FTableType of
    tbInstitutions:
      begin
        Caption := rsTitleInstitutions;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qInstitutions;
        //TableSearch(DMG.qInstitutions, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
      end;
    tbPeople:
      begin
        Caption := rsTitleResearchers;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qPeople;
        //TableSearch(DMG.qPeople, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
      end;
    tbProjects:
      begin
        Caption := rsTitleProjects;
        AddSortedField('project_title', sdAscending);
        dsLink.DataSet := DMG.qProjects;

        lblChildTag1.Caption := rsTitleTeam;
        pChildTag1.Visible := True;
        nbChilds.PageIndex := 0;
        FChildTable := tbProjectTeams;
        dsLink1.DataSet := DMG.qProjectTeam;
        pmcNewProjectMember.Visible := True;

        pChildsBar.Visible := True;
        //TableSearch(DMG.qProjects, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowDocs.Visible := True;
      end;
    tbPermits:
      begin
        Caption := rsTitlePermits;
        AddSortedField('permit_name', sdAscending);
        dsLink.DataSet := DMG.qPermits;
        //TableSearch(DMG.qPermits, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowDocs.Visible := True;
      end;
    tbGazetteer:
      begin
        Caption := rsTitleGazetteer;
        AddSortedField('site_name', sdAscending);
        dsLink.DataSet := DMG.qGazetteer;
        //TableSearch(DMG.qGazetteer, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowDocs.Visible := True;
      end;
    tbNetStations:
      begin
        Caption := rsTitleSamplingPlots;
        AddSortedField('station_name', sdAscending);
        dsLink.DataSet := DMG.qNetStations;

        lblChildTag1.Caption := rsTitlePermanentNets;
        pChildTag1.Visible := True;
        nbChilds.PageIndex := 0;
        FChildTable := tbPermanentNets;
        dsLink1.DataSet := DMG.qPermanentNets;
        pmcNewPermanentNet.Visible := True;

        pChildsBar.Visible := True;
        //TableSearch(DMG.qNetStations, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
      end;
    tbTaxonRanks:
      begin
        Caption := rsTitleTaxonRanks;
        AddSortedField('rank_seq', sdAscending);
        dsLink.DataSet := DMG.qTaxonRanks;
        //TableSearch(DMG.qTaxonRanks, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
      end;
    tbBotanicTaxa:
      begin
        Caption := rsTitleBotanicTaxa;
        AddSortedField('taxon_name', sdAscending);
        dsLink.DataSet := DMG.qBotany;
        //TableSearch(DMG.qBotany, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
      end;
    tbZooTaxa:
      begin

      end;
    tbBands:
      begin
        Caption := rsTitleBands;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qBands;
        //TableSearch(DMG.qBands, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
      end;
    tbIndividuals:
      begin
        Caption := rsTitleIndividuals;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qIndividuals;

        lblChildTag1.Caption := rsTitleCaptures;
        lblChildTag2.Caption := rsTitleMolts;
        lblChildTag3.Caption := rsTitleSightings;
        lblChildTag4.Caption := rsTitleNests;
        lblChildTag5.Caption := rsTitleSpecimens;
        pChildTag1.Visible := True;
        pChildTag2.Visible := True;
        pChildTag3.Visible := True;
        pChildTag4.Visible := True;
        pChildTag5.Visible := True;
        nbChilds.PageIndex := 0;
        if not Assigned(DMI) then
          DMI := TDMI.Create(Self);
        FChildTable := tbCaptures;
        dsLink1.DataSet := DMI.qCaptures;
        dsLink2.DataSet := DMI.qMolts;
        dsLink3.DataSet := DMI.qSightings;
        dsLink4.DataSet := DMI.qNests;
        dsLink5.DataSet := DMI.qSpecimens;
        pmcNewCapture.Visible := True;
        pmcNewMolt.Visible := True;
        pmcNewSighting.Visible := True;
        pmcNewNest.Visible := True;
        pmcNewSpecimen.Visible := True;

        pChildsBar.Visible := True;
        //pChild.Visible := True;
        dbgImages.DataSource := DMI.dsImages;
        lblImageTime.DataSource := dbgImages.DataSource;
        lblImageDate.DataSource := dbgImages.DataSource;
        lblImageType.DataSource := dbgImages.DataSource;
        //imgThumb.DataSource := dbgImages.DataSource;
        //imgThumb.DataField := 'image_thumbnail';
        //TableSearch(DMG.qIndividuals, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbCaptures:
      begin
        Caption := rsTitleCaptures;
        AddSortedField('capture_date', sdDescending);
        dsLink.DataSet := DMG.qCaptures;
        //TableSearch(DMG.qCaptures, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbNests:
      begin
        Caption := rsTitleNests;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qNests;

        lblChildTag1.Caption := rsTitleNestRevisions;
        lblChildTag2.Caption := rsTitleEggs;
        pChildTag1.Visible := True;
        pChildTag2.Visible := True;
        nbChilds.PageIndex := 0;
        if not Assigned(DMB) then
          DMB := TDMB.Create(Self);
        FChildTable := tbNestRevisions;
        dsLink1.DataSet := DMB.qNestRevisions;
        dsLink2.DataSet := DMB.qEggs;
        pmcNewNestRevision.Visible := True;
        pmcNewEgg.Visible := True;

        pChildsBar.Visible := True;
        //TableSearch(DMG.qNests, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbNestRevisions:
      begin
        Caption := rsTitleNestRevisions;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qNestRevisions;
        //TableSearch(DMG.qEggs, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbEggs:
      begin
        Caption := rsTitleEggs;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qEggs;
        //TableSearch(DMG.qEggs, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbMethods:
      begin
        Caption := rsTitleMethods;
        AddSortedField('method_name', sdAscending);
        dsLink.DataSet := DMG.qMethods;
        //TableSearch(DMG.qMethods, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowDocs.Visible := True;
      end;
    tbExpeditions:
      begin
        Caption := rsCaptionExpeditions;
        AddSortedField('start_date', sdDescending);
        dsLink.DataSet := DMG.qExpeditions;

        lblChildTag1.Caption := rsTitleSurveys;
        pChildTag1.Visible := True;
        nbChilds.PageIndex := 0;
        if not Assigned(DMS) then
          DMS := TDMS.Create(Self);
        FChildTable := tbSurveys;
        dsLink1.DataSet := DMS.qSurveys;
        pmcNewSurvey.Visible := True;

        pChildsBar.Visible := True;
        //TableSearch(DMG.qExpeditions, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbSurveys:
      begin
        Caption := rsTitleSurveys;
        AddSortedField('survey_date', sdDescending);
        dsLink.DataSet := DMG.qSurveys;

        lblChildTag1.Caption := rsTitleTeam;
        lblChildTag2.Caption := rsTitleNetsEffort;
        lblChildTag3.Caption := rsTitleWeather;
        lblChildTag4.Caption := rsTitleCaptures;
        lblChildTag5.Caption := rsTitleSightings;
        pChildTag1.Visible := True;
        pChildTag2.Visible := True;
        pChildTag3.Visible := True;
        pChildTag4.Visible := True;
        pChildTag5.Visible := True;
        nbChilds.PageIndex := 0;
        if not Assigned(DMS) then
          DMS := TDMS.Create(Self);
        FChildTable := tbSurveyTeams;
        dsLink1.DataSet := DMS.qSurveyTeam;
        dsLink2.DataSet := DMS.qNetsEffort;
        dsLink3.DataSet := DMS.qWeatherLogs;
        dsLink4.DataSet := DMS.qCaptures;
        dsLink5.DataSet := DMS.qSightings;
        pmcNewSurveyMember.Visible := True;
        pmcNewMistnet.Visible := True;
        pmcNewWeatherLog.Visible := True;
        pmcNewCapture.Visible := True;
        pmcNewSighting.Visible := True;

        pChildsBar.Visible := True;
        //TableSearch(DMG.qSurveys, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbSightings:
      begin
        Caption := rsTitleSightings;
        AddSortedField('sighting_date', sdDescending);
        dsLink.DataSet := DMG.qSightings;
        //TableSearch(DMG.qSightings, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
    tbSpecimens:
      begin
        Caption := rsTitleSpecimens;
        AddSortedField('full_name', sdAscending);
        dsLink.DataSet := DMG.qSpecimens;

        lblChildTag1.Caption := rsTitleCollectors;
        lblChildTag2.Caption := rsTitleSamplePreps;
        pChildTag1.Visible := True;
        pChildTag2.Visible := True;
        nbChilds.PageIndex := 0;
        FChildTable := tbSpecimenCollectors;
        dsLink1.DataSet := DMG.qSampleCollectors;
        dsLink2.DataSet := DMG.qSamplePreps;
        pmcNewCollector.Visible := True;
        pmcNewSamplePrep.Visible := True;

        pChildsBar.Visible := True;
        //TableSearch(DMG.qSpecimens, FTableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
        sbShowImages.Visible := True;
        sbShowAudio.Visible := True;
        sbShowDocs.Visible := True;
      end;
  end;
  FSearch.DataSet := TSQLQuery(dsLink.DataSet);
  //dbVG.DataSource := dsLink;
  SplitChild.Visible := pChild.Visible;
  Search(EmptyStr);
end;

procedure TfrmCustomGrid.AddGridColumns(aTable: TTableType; aGrid: TDBGrid);
var
  C: TColumn;
  i: Integer;
begin
  if aGrid.DataSource = nil then
    Exit;

  try
    aGrid.BeginUpdate;
    for i := 0 to (aGrid.DataSource.DataSet.FieldCount-1) do
      if aGrid.DataSource.DataSet.Fields[i].Visible then
      begin
        C := aGrid.Columns.Add;
        C.FieldName := aGrid.DataSource.DataSet.Fields[i].FieldName;
        //C := nil;
      end;

    SetGridColumns(aTable, aGrid);
  finally
    //aGrid.Columns.LinkFields;
    aGrid.EndUpdate;
  end;

end;

procedure TfrmCustomGrid.SetGridColumns(aTable: TTableType; aGrid: TDBGrid);
begin
  if aGrid.DataSource = nil then
    Exit;

  case aTable of
    tbNone: ;
    tbInstitutions:
      begin
        with aGrid.Columns.ColumnByFieldname('institution_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldname('country_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('state_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('municipality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbPeople:
      begin
        with aGrid.Columns.ColumnByFieldname('person_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldName('gender') do
        begin
          PickList.AddCommaText(rsGenderList);
        end;
        with aGrid.Columns.ColumnByFieldName('title_treatment') do
        begin
          PickList.AddCommaText(rsTreatmentList);
        end;
        if aGrid.DataSource.DataSet.FieldByName('institution_name').Visible then
        with aGrid.Columns.ColumnByFieldname('institution_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('country_name').Visible then
        with aGrid.Columns.ColumnByFieldname('country_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('state_name').Visible then
        with aGrid.Columns.ColumnByFieldname('state_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('municipality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('municipality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('birth_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('death_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbProjects:
      begin
        with aGrid.Columns.ColumnByFieldname('project_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldName('start_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('end_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbPermits:
      begin
        with aGrid.Columns.ColumnByFieldname('permit_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldName('project_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('dispatch_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('expire_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbGazetteer:
      begin
        with aGrid.Columns.ColumnByFieldname('site_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldName('site_rank') do
        begin
          PickList.Add(rsCaptionCountry);
          PickList.Add(rsCaptionState);
          PickList.Add(rsCaptionRegion);
          PickList.Add(rsCaptionMunicipality);
          PickList.Add(rsCaptionDistrict);
          PickList.Add(rsCaptionLocality);
        end;
        if aGrid.DataSource.DataSet.FieldByName('parent_site_name').Visible then
        with aGrid.Columns.ColumnByFieldname('parent_site_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbNetStations:
      begin
        with aGrid.Columns.ColumnByFieldname('net_station_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbPermanentNets:
      begin
        with aGrid.Columns.ColumnByFieldname('permanent_net_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbTaxonRanks:
      begin
        with aGrid.Columns.ColumnByFieldname('rank_id') do
        begin
          ReadOnly:= True;
        end;
      end;
    tbBotanicTaxa:
      begin
        with aGrid.Columns.ColumnByFieldname('taxon_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldname('parent_taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('valid_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbZooTaxa:
      begin

      end;
    tbBands:
      begin
        with aGrid.Columns.ColumnByFieldname('band_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldName('band_size') do
        begin
          PickList.AddCommaText('A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z');
        end;
        with aGrid.Columns.ColumnByFieldName('band_status') do
        begin
          //KeyList.AddCommaText('D,U,R,Q,P');
          PickList.AddCommaText(rsBandStatusList);
        end;
        with aGrid.Columns.ColumnByFieldName('band_source') do
        begin
          //KeyList.AddCommaText('A,T,L,D,F');
          PickList.Add(rsBandAcquiredFromSupplier);
          PickList.Add(rsBandTransferBetweenBanders);
          PickList.Add(rsBandLivingBirdBandedByOthers);
          PickList.Add(rsBandDeadBirdBandedByOthers);
          PickList.Add(rsBandFoundLoose);
        end;
        with aGrid.Columns.ColumnByFieldName('band_type') do
        begin
          //KeyList.AddCommaText('A,F,T,B,C,V');
          PickList.AddCommaText(rsBandTypeList);
        end;
        with aGrid.Columns.ColumnByFieldName('supplier_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('carrier_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('project_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbIndividuals:
      begin
        if aGrid.DataSource.DataSet.FieldByName('individual_id').Visible then
        with aGrid.Columns.ColumnByFieldname('individual_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('taxon_name').Visible then
        with aGrid.Columns.ColumnByFieldName('taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('band_full_name').Visible then
        with aGrid.Columns.ColumnByFieldName('band_full_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('double_band_name').Visible then
        with aGrid.Columns.ColumnByFieldName('double_band_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('removed_band_name').Visible then
        with aGrid.Columns.ColumnByFieldName('removed_band_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('individual_sex').Visible then
        with aGrid.Columns.ColumnByFieldName('individual_sex') do
        begin
          PickList.Add(rsSexUnknown);
          PickList.Add(rsSexMale);
          PickList.Add(rsSexFemale);
        end;
        if aGrid.DataSource.DataSet.FieldByName('individual_age').Visible then
        with aGrid.Columns.ColumnByFieldName('individual_age') do
        begin
          PickList.Add(rsAgeUnknown);
          PickList.Add(rsAgeAdult);
          PickList.Add(rsAgeImmature);
          PickList.Add(rsAgeFledgling);
          PickList.Add(rsAgeNestling);
          PickList.Add(rsAgeFirstYear);
          PickList.Add(rsAgeSecondYear);
          PickList.Add(rsAgeThirdYear);
          PickList.Add(rsAgeFourthYear);
          PickList.Add(rsAgeFifthYear);
        end;
        if aGrid.DataSource.DataSet.FieldByName('banding_date').Visible then
        with aGrid.Columns.ColumnByFieldName('banding_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('band_change_date').Visible then
        with aGrid.Columns.ColumnByFieldName('band_change_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('nest_name').Visible then
        with aGrid.Columns.ColumnByFieldName('nest_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('father_name').Visible then
        with aGrid.Columns.ColumnByFieldName('father_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('mother_name').Visible then
        with aGrid.Columns.ColumnByFieldName('mother_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('right_leg_below').Visible then
        with aGrid.Columns.ColumnByFieldName('right_leg_below') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('left_leg_below').Visible then
        with aGrid.Columns.ColumnByFieldName('left_leg_below') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('right_leg_above').Visible then
        with aGrid.Columns.ColumnByFieldName('right_leg_above') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('left_leg_above').Visible then
        with aGrid.Columns.ColumnByFieldName('left_leg_above') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbCaptures:
      begin
        if aGrid.DataSource.DataSet.FieldByName('capture_id').Visible then
        with aGrid.Columns.ColumnByFieldname('capture_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('capture_date').Visible then
        with aGrid.Columns.ColumnByFieldName('capture_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('taxon_name').Visible then
        with aGrid.Columns.ColumnByFieldName('taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('capture_type').Visible then
        with aGrid.Columns.ColumnByFieldName('capture_type') do
        begin
          PickList.AddCommaText(rsCaptureTypeList);
        end;
        if aGrid.DataSource.DataSet.FieldByName('right_leg_below').Visible then
        with aGrid.Columns.ColumnByFieldName('right_leg_below') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('left_leg_below').Visible then
        with aGrid.Columns.ColumnByFieldName('left_leg_below') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('right_leg_above').Visible then
        with aGrid.Columns.ColumnByFieldName('right_leg_above') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('left_leg_above').Visible then
        with aGrid.Columns.ColumnByFieldName('left_leg_above') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('cloacal_protuberance').Visible then
        with aGrid.Columns.ColumnByFieldName('cloacal_protuberance') do
        begin
          PickList.AddCommaText('U,N,S,M,L');
        end;
        if aGrid.DataSource.DataSet.FieldByName('brood_patch').Visible then
        with aGrid.Columns.ColumnByFieldName('brood_patch') do
        begin
          PickList.AddCommaText('F,N,V,W,O');
        end;
        if aGrid.DataSource.DataSet.FieldByName('fat').Visible then
        with aGrid.Columns.ColumnByFieldName('fat') do
        begin
          PickList.AddCommaText('N,T,L,H,F,B,G,V');
        end;
        if aGrid.DataSource.DataSet.FieldByName('body_molt').Visible then
        with aGrid.Columns.ColumnByFieldName('body_molt') do
        begin
          PickList.AddCommaText('N,T,S,H,G,A,F');
        end;
        if aGrid.DataSource.DataSet.FieldByName('flight_feathers_molt').Visible then
        with aGrid.Columns.ColumnByFieldName('flight_feathers_molt') do
        begin
          PickList.AddCommaText('N,S,A');
        end;
        if aGrid.DataSource.DataSet.FieldByName('flight_feathers_wear').Visible then
        with aGrid.Columns.ColumnByFieldName('flight_feathers_wear') do
        begin
          PickList.AddCommaText('N,S,L,M,H,X');
        end;
        if aGrid.DataSource.DataSet.FieldByName('skull_ossification').Visible then
        with aGrid.Columns.ColumnByFieldName('skull_ossification') do
        begin
          PickList.AddCommaText('N,T,L,H,G,A,F');
        end;
        if aGrid.DataSource.DataSet.FieldByName('molt_limits').Visible then
        with aGrid.Columns.ColumnByFieldName('molt_limits') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('cycle_code').Visible then
        with aGrid.Columns.ColumnByFieldName('cycle_code') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('subject_age').Visible then
        with aGrid.Columns.ColumnByFieldName('subject_age') do
        begin
          PickList.Add(rsAgeUnknown);
          PickList.Add(rsAgeAdult);
          PickList.Add(rsAgeImmature);
          PickList.Add(rsAgeFledgling);
          PickList.Add(rsAgeNestling);
          PickList.Add(rsAgeFirstYear);
          PickList.Add(rsAgeSecondYear);
          PickList.Add(rsAgeThirdYear);
          PickList.Add(rsAgeFourthYear);
          PickList.Add(rsAgeFifthYear);
        end;
        if aGrid.DataSource.DataSet.FieldByName('how_aged').Visible then
        with aGrid.Columns.ColumnByFieldName('how_aged') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('subject_sex').Visible then
        with aGrid.Columns.ColumnByFieldName('subject_sex') do
        begin
          PickList.Add(rsSexMale);
          PickList.Add(rsSexFemale);
          PickList.Add(rsSexUnknown);
        end;
        if aGrid.DataSource.DataSet.FieldByName('how_sexed').Visible then
        with aGrid.Columns.ColumnByFieldName('how_sexed') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('subject_status').Visible then
        with aGrid.Columns.ColumnByFieldName('subject_status') do
        begin
          PickList.Add(rsStatusNormal);
          PickList.Add(rsStatusInjured);
          PickList.Add(rsStatusWingSprain);
          PickList.Add(rsStatusStressed);
          PickList.Add(rsStatusDead);
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('longitude').Visible then
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('latitude').Visible then
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbNests:
      begin
        if aGrid.DataSource.DataSet.FieldByName('nest_id').Visible then
        with aGrid.Columns.ColumnByFieldname('nest_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('taxon_name').Visible then
        with aGrid.Columns.ColumnByFieldName('taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('found_date').Visible then
        with aGrid.Columns.ColumnByFieldName('found_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('last_date').Visible then
        with aGrid.Columns.ColumnByFieldName('last_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbNestRevisions:
      begin
        with aGrid.Columns.ColumnByFieldname('nest_revision_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('revision_date').Visible then
        with aGrid.Columns.ColumnByFieldName('revision_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbEggs:
      begin
        aGrid.Columns.ColumnByFieldname('egg_id').ReadOnly:= True;
        if aGrid.DataSource.DataSet.FieldByName('taxon_name').Visible then
        with aGrid.Columns.ColumnByFieldName('taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('measure_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('individual_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('researcher_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('egg_shape') do
        begin
          PickList.Add(rsEggSpheric);
          PickList.Add(rsEggElliptic);
          PickList.Add(rsEggOvoid);
          PickList.Add(rsEggPyriform);
          PickList.Add(rsEggUnknown);
        end;
      end;
    tbMethods:
      begin
        with aGrid.Columns.ColumnByFieldname('method_id') do
        begin
          ReadOnly:= True;
        end;
      end;
    tbExpeditions:
      begin
        with aGrid.Columns.ColumnByFieldname('expedition_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('start_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldName('end_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbSurveys:
      begin
        with aGrid.Columns.ColumnByFieldname('survey_id') do
        begin
          ReadOnly:= True;
        end;
        with aGrid.Columns.ColumnByFieldName('survey_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('start_longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('start_latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('end_longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('end_latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbWeatherLogs:
      begin
        //with aGrid.Columns.ColumnByFieldname('weather_id') do
        //begin
        //  ReadOnly:= True;
        //end;
        with aGrid.Columns.ColumnByFieldname('sample_moment') do
        begin
          PickList.Add(rsMomentStart);
          PickList.Add(rsMomentMiddle);
          PickList.Add(rsMomentEnd);
        end;
        with aGrid.Columns.ColumnByFieldname('precipitation') do
        begin
          PickList.Add(rsPrecipitationNone);
          PickList.Add(rsPrecipitationFog);
          PickList.Add(rsPrecipitationMist);
          PickList.Add(rsPrecipitationDrizzle);
          PickList.Add(rsPrecipitationRain);
        end;
      end;
    tbSightings:
      begin
        if aGrid.DataSource.DataSet.FieldByName('sighting_id').Visible then
        with aGrid.Columns.ColumnByFieldname('sighting_id') do
        begin
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('survey_name').Visible then
        with aGrid.Columns.ColumnByFieldname('survey_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('taxon_name').Visible then
        with aGrid.Columns.ColumnByFieldName('taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('observer_name').Visible then
        with aGrid.Columns.ColumnByFieldname('observer_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('individual_name').Visible then
        with aGrid.Columns.ColumnByFieldname('individual_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('detection_type').Visible then
        with aGrid.Columns.ColumnByFieldname('detection_type') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('breeding_status').Visible then
        with aGrid.Columns.ColumnByFieldname('breeding_status') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('sighting_date').Visible then
        with aGrid.Columns.ColumnByFieldname('sighting_date') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
    tbSpecimens:
      begin
        if aGrid.DataSource.DataSet.FieldByName('specimen_id').Visible then
        with aGrid.Columns.ColumnByFieldname('specimen_id') do
        begin
          //Footer.ValueType := fvtCount;
          //Footer.Alignment := taCenter;
          ReadOnly:= True;
        end;
        if aGrid.DataSource.DataSet.FieldByName('taxon_name').Visible then
        with aGrid.Columns.ColumnByFieldName('taxon_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        if aGrid.DataSource.DataSet.FieldByName('locality_name').Visible then
        with aGrid.Columns.ColumnByFieldname('locality_name') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('longitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
        with aGrid.Columns.ColumnByFieldname('latitude') do
        begin
          ButtonStyle := cbsEllipsis;
        end;
      end;
  end;

  //aGrid.OptionsExtra := aGrid.OptionsExtra - [dgeAutoColumns];

  {$IFDEF DEBUG}
  LogDebug(Format('%s: %d columns (%d visible)', [aGrid.Name, aGrid.Columns.Count, aGrid.Columns.VisibleCount]));
  {$ENDIF}
end;

procedure TfrmCustomGrid.AddSortedField(aFieldName: String; aDirection: TSortDirection;
  aCollation: String = ''; IsAnAlias: Boolean = False);
var
  p: Integer;
begin
  p := FSearch.SortFields.Add(TSortedField.Create);
  FSearch.SortFields.Items[p].FieldName := aFieldName;
  FSearch.SortFields.Items[p].Direction := aDirection;
  FSearch.SortFields.Items[p].Collation := aCollation;
  FSearch.SortFields.Items[p].Lookup := IsAnAlias;
end;

function TfrmCustomGrid.Search(aValue: String): Boolean;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  {$IFDEF DEBUG}
  LogDebug('Search value: ' + aValue);
  {$ENDIF}
  FSearch.Fields.Clear;
  FSearch.QuickFilters.Clear;
  lblRecordStatus.Caption := rsLoadingRecords;

  case TableType of
    tbNone: ;
    tbProjectTeams: ;
    tbPermits:
      Result := SearchPermits(aValue);
    tbGazetteer:
      Result := SearchGazetteer(aValue);
    tbBotanicTaxa:
      Result := SearchBotanicTaxa(aValue);
    tbNests:
      Result := SearchNests(aValue);
    tbNestRevisions:
      Result := SearchNestRevisions(aValue);
    tbEggs:
      Result := SearchEggs(aValue);
    tbNetStations:
      Result := SearchNetStations(aValue);
    tbTaxonRanks:
      Result := SearchTaxonRanks(aValue);
    tbZooTaxa:
      Result := SearchZooTaxa(aValue);
    tbProjects:
      Result := SearchProjects(aValue);
    tbInstitutions:
      Result := SearchInstitutions(aValue);
    tbPeople:
      Result := SearchPeople(aValue);
    tbExpeditions:
      Result := SearchExpeditions(aValue);
    tbSurveys:
      Result := SearchSurveys(aValue);
    tbMethods:
      Result := SearchMethods(aValue);
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbSightings:
      Result := SearchSightings(aValue);
    tbSpecimens:
      Result := SearchSpecimens(aValue);
    tbSamplePreps: ;
    tbPermanentNets: ;
    tbBands:
      Result := SearchBands(aValue);
    tbIndividuals:
      Result := SearchIndividuals(aValue);
    tbCaptures:
      Result := SearchCaptures(aValue);
    tbMolts: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;
  Working := False;

  UpdateButtons(dsLink.DataSet);
end;

function TfrmCustomGrid.SearchInstitutions(aValue: String): Boolean;
var
  i, g: Integer;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('institution_id', 'Institution (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('acronym', 'Acronym', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('manager_name', 'Manager', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchPeople(aValue: String): Boolean;
var
  i, g: Integer;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('person_id', 'Person (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('birth_date', 'Birth date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('acronym', 'Acronym', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('citation', 'Citation', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchProjects(aValue: String): Boolean;
var
  i, g: Integer;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('project_id', 'Project (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('start_date', 'Start date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('end_date', 'End date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('project_title', 'Title', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('short_title', 'Short title', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('contact_name', 'Contact', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('project_abstract', 'Abstract', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchPermits(aValue: String): Boolean;
var
  i, g: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('permit_id', 'Permit (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('dispatch_date', 'Dispatch date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('expire_date', 'Expire date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('permit_name', 'Permit name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('permit_number', 'Permit number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('dispatcher_name', 'Dispatcher', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchGazetteer(aValue: String): Boolean;
var
  i, g: Longint;
  f: Extended;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('site_id', 'Toponym (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToFloat(aValue, f) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('longitude', 'Longitude', sdtText, crStartLike,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('latitude', 'Latitude', sdtText, crStartLike,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('site_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('site_acronym', 'Acronym', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchNetStations(aValue: String): Boolean;
var
  i, g: Integer;
  f: Extended;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('net_station_id', 'Sampling plot (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToFloat(aValue, f) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('longitude', 'Longitude', sdtText, crStartLike,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('latitude', 'Latitude', sdtText, crStartLike,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('station_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('station_acronym', 'Acronym', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchTaxonRanks(aValue: String): Boolean;
var
  i, g: Longint;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('rank_id', 'Rank (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('rank_name', 'Name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('rank_acronym', 'Acronym', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchBotanicTaxa(aValue: String): Boolean;
var
  i, g: Longint;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Scientific name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('vernacular_name', 'Vernacular name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('authorship', 'Authorship', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchZooTaxa(aValue: String): Boolean;
var
  i, g: Longint;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Scientific name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('english_name', 'English name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('ioc_english_name', 'English name (IOC)', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('spanish_name', 'Spanish name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('portuguese_name', 'Portuguese name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('other_portuguese_names', 'Other portuguese names', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('ebird_code', 'eBird code', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('quick_code', 'Quick code', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchBands(aValue: String): Boolean;
var
  i, g: Integer;
  V1, V2: String;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);
  V1 := EmptyStr;
  V2 := EmptyStr;

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_id', 'Band (ID)', sdtInteger, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_number', 'Band number', sdtText, Crit,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d+[-‒]{1}\d+$', aValue) then
    begin
      Crit := crBetween;
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      { split strings: unicode characters #$002D e #$2012 }
      V1 := ExtractDelimited(0, aValue, ['-', #$2012]);
      V2 := ExtractDelimited(1, aValue, ['-', #$2012]);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_number', 'Band number', sdtInteger, Crit,
        False, V1, V2));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('carrier_name', 'Project', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('supplier_name', 'Project', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('project_name', 'Project', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('individual_name', 'Individual', sdtText, Crit,
        True, aValue));
    end;

  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchIndividuals(aValue: String): Boolean;
var
  i, g: Integer;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('individual_id', 'Individual (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('banding_date', 'Banding date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_change_date', 'Band change date', sdtDate, crEqual,
        False, aValue));
      { #todo : PartialDate fields: birth_date and death_date }
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_full_name', 'Band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('double_band_name', 'Double band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('removed_band_name', 'Removed band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('nest_name', 'Nest', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchCaptures(aValue: String): Boolean;
var
  i, g: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_id', 'Capture (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_date', 'Capture date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, dt) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_time', 'Capture time', sdtTime, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_name', 'Band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('removed_band_name', 'Removed band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('locality_name', 'Locality', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchNests(aValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('nest_id', 'Nest (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('found_date', 'Date found', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('last_date', 'Last date seen', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('field_number', 'Field number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('locality_name', 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Taxon', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchEggs(aValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('egg_id', 'Egg (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('measure_date', 'Measured date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('field_number', 'Field number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Taxon', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchMethods(aValue: String): Boolean;
var
  i, g: Longint;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('method_id', 'Method (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('method_name', 'Name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('method_acronym', 'Acronym', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('ebird_name', 'eBird name', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchNestRevisions(aValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('nest_revision_id', 'Nest revision (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('revision_date', 'Revision date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('nidoparasite_name', 'Taxon', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchExpeditions(aValue: String): Boolean;
var
  i, g: Integer;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('expedition_id', 'Expedition (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('start_date', 'Start date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('end_date', 'End date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('expedition_name', 'Expedition name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('locality_name', 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('description', 'Description', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchSurveys(aValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('survey_id', 'Survey (ID)', sdtInteger, crEqual,
        False, aValue));
      // if i > 999 then
      // Add('or (strftime(''%Y'',AMO_DATA) = '+QuotedStr(aValor)+'))');
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('survey_date', 'Survey date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, Dt) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('start_time', 'Start time', sdtTime, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('end_time', 'End time', sdtTime, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('locality_name', 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('municipality_name', 'Municipality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('state_name', 'State', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchSightings(aValue: String): Boolean;
var
  i, g: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('sighting_id', 'Sighting (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('sighting_date', 'Sighting date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, dt) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('sighting_time', 'Sighting time', sdtTime, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('locality_name', 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('method_name', 'Method', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchSpecimens(aValue: String): Boolean;
var
  i, g: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('specimen_id', 'Specimen (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('collection_date', 'Collection date', sdtDate, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_name', 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('collectors', 'Collectors', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('locality_name', 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('municipality_name', 'Municipality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('state_name', 'State', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('country_name', 'Country', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters(QuickFilters);

  Result := FSearch.RunSearch > 0;
end;

procedure TfrmCustomGrid.LoadRecordColumns;
var
  i: Integer;
begin
  gridRecord.ClearRows;
//  gRecord.AddRow(DBG.Columns.VisibleCount);
  gridRecord.ColWidths[0] := Round(gridRecord.Width * 0.3);

  for i := 0 to (DBG.Columns.Count - 1) do
  begin
    if DBG.Columns.Items[i].Visible then
    begin
      gridRecord.RowCount := gridRecord.RowCount + 1;
      gridRecord.Cells[0, DBG.Columns.VisibleIndex(i)] := DBG.Columns.Items[i].Title.Caption;
      //gridRecord.BestFitRow(DBG.Columns.Item[i].VisibleIndex, 0);

      if (DBG.Columns.Items[i].Field.DataType = ftMemo) or
        (DBG.Columns.Items[i].Field.DataType = ftBlob) then
        gridRecord.RowHeights[i] := gridRecord.DefaultRowHeight * 4;

    end;
  end;
end;

procedure TfrmCustomGrid.LoadRecordRow;
var
  i: Integer;
begin
  for i := 0 to (DBG.Columns.Count - 1) do
  begin
    if DBG.Columns[i].Visible then
    begin
      gridRecord.Cells[1, DBG.Columns.VisibleIndex(i)] :=
        DBG.Columns[i].Field.AsString;
    end;
  end;
end;

procedure TfrmCustomGrid.UpdateFilterPanels;
begin
  case TableType of
    tbGazetteer:
      begin
        cbSiteRankFilter.Items.Clear;
        cbSiteRankFilter.Items.Add('All');
        cbSiteRankFilter.Items.Add(rsCaptionCountry);
        cbSiteRankFilter.Items.Add(rsCaptionState);
        cbSiteRankFilter.Items.Add(rsCaptionRegion);
        cbSiteRankFilter.Items.Add(rsCaptionMunicipality);
        cbSiteRankFilter.Items.Add(rsCaptionDistrict);
        cbSiteRankFilter.Items.Add(rsCaptionLocality);
        pSiteRankFilter.Visible := True;
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
      end;
    tbInstitutions:
      begin
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
      end;
    tbPeople:
      begin
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
      end;
    tbProjects:
      begin
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
      end;
    tbPermits:
      begin
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
      end;
    tbNetStations:
      begin
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
      end;
    tbBotanicTaxa:
      begin
        pTaxonRanksFilters.Visible := True;
        LoadTaxaRanks(DMM.sqlCon, clbTaxonRanksFilter);
        pSynonymFilters.Visible := True;
      end;
    tbZooTaxa:
      begin
        pTaxonRanksFilters.Visible := True;
        LoadTaxaRanks(DMM.sqlCon, clbTaxonRanksFilter);
        pTaxonomiesFilters.Visible := True;
        pSynonymFilters.Visible := True;
        pExtinctFilter.Visible := True;
      end;
    tbBands:
      begin
        pBandSizeFilter.Visible := True;
        cbBandStatusFilter.Items.Clear;
        cbBandStatusFilter.Items.Add('All');
        cbBandStatusFilter.Items.AddCommaText(rsBandStatusList);
        pBandStatusFilter.Visible := True;
        pBandReportFilters.Visible := True;
        //pDatesFilters.Visible := True;
        //LoadDateTreeData(TableType, tvDateFilter);
      end;
    tbIndividuals:
      begin
        pTaxonFilters.Visible := True;
        LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
        pSkullOssificationFilter.Visible := False;
        pHowAgedFilter.Visible := False;
        pAgeFilter.Rounding.RoundOptions := [];
        pAgingFilters.Visible := True;
        pCloacalProtuberanceFilter.Visible := False;
        pBroodPatchFilter.Visible := False;
        pHowSexedFilter.Visible := False;
        pSexFilter.Rounding.RoundOptions := [];
        pSexingFilters.Visible := True;
      end;
    tbCaptures:
      begin
        pTaxonFilters.Visible := True;
        LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
        cbAgeFilter.Items.Clear;
        cbAgeFilter.Items.CommaText := 'All,' + rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeImmature + ',' +
          rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
          rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
        pAgingFilters.Visible := True;
        cbSexFilter.Items.Clear;
        cbSexFilter.Items.CommaText := 'All,' + rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
        pSexingFilters.Visible := True;
        pFatFilter.Visible := True;
        pMoltingFilters.Visible := True;
      end;
    tbNests:
      begin
        pTaxonFilters.Visible := True;
        LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
        cbNestFateFilter.Items.Clear;
        cbNestFateFilter.Items.CommaText := 'All,"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
        pNestFateFilter.Visible := True;
        cbNestSupportFilter.Items.Clear;
        cbNestSupportFilter.Items.CommaText := 'All,"' + rsSupportGround + '","' + rsSupportPlatform + '","' +
          rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportSuspended + '","' +
          rsSupportCavity + '","' + rsSupportArtificial + '","' + rsSupportOther + '"';
        pNestSupportFilter.Visible := True;
      end;
    tbNestRevisions:
      begin
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
      end;
    tbEggs:
      begin
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
      end;
    tbExpeditions:
      begin
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
      end;
    tbSurveys:
      begin
        pMethodFilter.Visible := True;
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
        pTimeFilters.Visible := True;
      end;
    tbSightings:
      begin
        pTaxonFilters.Visible := True;
        LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
      end;
    tbSpecimens:
      begin
        cbMaterialFilter.Items.Clear;
        cbMaterialFilter.Items.Add(rsSpecimenCarcassWhole);
        cbMaterialFilter.Items.Add(rsSpecimenCarcassPartial);
        cbMaterialFilter.Items.Add(rsSpecimenNest);
        cbMaterialFilter.Items.Add(rsSpecimenBones);
        cbMaterialFilter.Items.Add(rsSpecimenEgg);
        cbMaterialFilter.Items.Add(rsSpecimenParasites);
        cbMaterialFilter.Items.Add(rsSpecimenFeathers);
        cbMaterialFilter.Items.Add(rsSpecimenBlood);
        cbMaterialFilter.Items.Add(rsSpecimenClaw);
        cbMaterialFilter.Items.Add(rsSpecimenSwab);
        cbMaterialFilter.Items.Add(rsSpecimenTissues);
        cbMaterialFilter.Items.Add(rsSpecimenFeces);
        cbMaterialFilter.Items.Add(rsSpecimenRegurgite);
        pMaterialFilter.Visible := True;
        pTaxonFilters.Visible := True;
        LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
        pDatesFilters.Visible := True;
        LoadDateTreeData(FTableType, tvDateFilter);
        pSiteFilters.Visible := True;
        LoadSiteTreeData(FTableType, tvSiteFilter, 4);
      end;
  end;
end;

procedure TfrmCustomGrid.GetFilters(aList: TStrings);
var
  sf: Integer;
begin
  if not CanToggle then
    Exit;

  CanToggle := False;
  aList.Clear;

  if (tsfMarked.StateOn = sw_on) then
  begin
    Modifier.Mark := rmMarked;
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (tsfUnmarked.StateOn = sw_on) then
  begin
    Modifier.Mark := rmUnmarked;
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '0'));
  end
  else
    Modifier.Mark := rmAll;

  case TableType of
    tbNone: ;
    tbProjectTeams: ;
    tbPermits:
      GetPermitFilters(aList);
    tbGazetteer:
      GetGazetteerFilters(aList);
    tbBotanicTaxa:
      GetBotanicTaxaFilters(aList);
    tbNests:
      GetNestFilters(aList);
    tbNestRevisions: ;
    tbEggs:
      GetEggFilters(aList);
    tbNetStations:
      GetNetStationFilters(aList);
    tbTaxonRanks:
      GetTaxonRankFilters(aList);
    tbZooTaxa:
      GetZooTaxaFilters(aList);
    tbProjects:
      GetProjectFilters(aList);
    tbInstitutions:
      GetInstitutionFilters(aList);
    tbPeople:
      GetPeopleFilters(aList);
    tbExpeditions:
      GetExpeditionFilters(aList);
    tbSurveys:
      GetSurveyFilters(aList);
    tbMethods:
      GetMethodFilters(aList);
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbSightings:
      GetSightingFilters(aList);
    tbSpecimens:
      GetSpecimenFilters(aList);
    tbSamplePreps: ;
    tbPermanentNets: ;
    tbBands:
      GetBandFilters(aList);
    tbIndividuals:
      GetIndividualFilters(aList);
    tbCaptures:
      GetCaptureFilters(aList);
    tbMolts: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  Filtrado := aList.Count > 0;
  CanToggle := True;
end;

procedure TfrmCustomGrid.GetInstitutionFilters(aList: TStrings);
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetPeopleFilters(aList: TStrings);
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetProjectFilters(aList: TStrings);
begin
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetPermitFilters(aList: TStrings);
begin
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetGazetteerFilters(aList: TStrings);
const
  SiteRanks: array of String = ('P', 'E', 'R', 'M', 'D', 'L');
var
  sf: Integer;
begin
  if cbSiteRankFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('site_rank', 'Site rank', sdtText,
      crEqual, False, SiteRanks[cbSiteRankFilter.ItemIndex - 1]));
  end;
end;

procedure TfrmCustomGrid.GetNetStationFilters(aList: TStrings);
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetTaxonRankFilters(aList: TStrings);
begin

end;

procedure TfrmCustomGrid.GetBotanicTaxaFilters(aList: TStrings);
var
  sf: Integer;
begin
  if RanksFilter <> EmptyStr then
    aList.Add(RanksFilter);

  if tsIsSynonym.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crDistinct, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;
end;

procedure TfrmCustomGrid.GetZooTaxaFilters(aList: TStrings);
var
  sf: Integer;
begin
  if RanksFilter <> EmptyStr then
    aList.Add(RanksFilter);

  if tsTaxonomyClements.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyIoc.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyCbro.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
      crEqual, False, '1'));
  end;

  if tsTaxonExtinct.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;

  if tsIsSynonym.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crDistinct, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;
end;

procedure TfrmCustomGrid.gridChild1DblClick(Sender: TObject);
begin
  if sbEditChild.Enabled then
    sbEditChildClick(Sender);
end;

procedure TfrmCustomGrid.GetBandFilters(aList: TStrings);
const
  BandStatus: array of String = ('D', 'U', 'R', 'T', 'Q', 'P');
var
  sf: Integer;
begin
  if cbBandSizeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('band_size', 'Band size', sdtText,
      crEqual, False, cbBandSizeFilter.Text));
  end;

  if cbBandStatusFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('band_status', 'Band status', sdtText,
      crEqual, False, BandStatus[cbBandStatusFilter.ItemIndex - 1]));
  end;

  if tsBandReported.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('band_reported', 'Band reported', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsBandNotReported.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('band_reported', 'Band reported', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetIndividualFilters(aList: TStrings);
const
  BirdAge: array of String = ('U', 'A', 'I', 'J', 'N', 'F', 'S', 'T', '4', '5');
  BirdSex: array of String = ('M', 'F', 'U');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbSexFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('individual_sex', 'Sex', sdtText,
      crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
  end;
  if cbAgeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('individual_age', 'Age', sdtText,
      crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
  end;

  if tsfWithColorBandsFilter.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('right_leg_below', 'Right tarsus', sdtText,
      crDistinct, False, ''));
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('left_leg_below', 'Left tarsus', sdtText,
      crDistinct, False, ''));
  end;
  if tsfWithRecapturesFilter.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('captures_tally', 'Captures', sdtInteger,
      crMoreThan, True, '2'));
  end;
end;

procedure TfrmCustomGrid.GetCaptureFilters(aList: TStrings);
const
  BirdAge: array of String = ('U', 'A', 'I', 'J', 'N', 'F', 'S', 'T', '4', '5');
  BirdSex: array of String = ('M', 'F', 'U');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbAgeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('subject_age', 'Age', sdtText,
      crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
  end;
  if cbSexFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('subject_sex', 'Sex', sdtText,
      crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
  end;

  if cbCloacalProtuberanceFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('cloacal_protuberance', 'Cloacal protuberance', sdtText,
      crEqual, False, cbCloacalProtuberanceFilter.Text));
  end;
  if cbBroodPatchFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('brood_patch', 'Brood patch', sdtText,
      crEqual, False, cbBroodPatchFilter.Text));
  end;

  if cbFatFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('fat', 'Subcutaneous fat', sdtText,
      crEqual, False, cbFatFilter.Text));
  end;

  if cbBodyMoltFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('body_molt', 'Body molt', sdtText,
      crEqual, False, cbBodyMoltFilter.Text));
  end;
  if cbFFMoltFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('flight_feathers_molt', 'Flight feathers molt', sdtText,
      crEqual, False, cbFFMoltFilter.Text));
  end;
  if cbFFWearFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('flight_feathers_wear', 'Flight feathers wear', sdtText,
      crEqual, False, cbFFWearFilter.Text));
  end;

  if cbSkullOssificationFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('skull_ossification', 'Skull ossification', sdtText,
      crEqual, False, cbSkullOssificationFilter.Text));
  end;
end;

function TfrmCustomGrid.GetChildDataSet: TDataSet;
begin
  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbNetStations:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    //tbProjectTeams: ;
    //tbPermits: ;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    //tbBotanicTaxa: ;
    //tbBands: ;
    //tbBandHistory: ;
    tbIndividuals:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
        2: Result := dsLink3.DataSet;
        3: Result := dsLink4.DataSet;
        4: Result := dsLink5.DataSet;
      end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
      end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    //tbExpeditions: ;
    tbSurveys:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
        2: Result := dsLink3.DataSet;
        3: Result := dsLink4.DataSet;
        4: Result := dsLink5.DataSet;
      end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  else
    Result := nil;
  end;

end;

procedure TfrmCustomGrid.GetNestFilters(aList: TStrings);
const
  NestFate: array of String = ('P', 'S', 'U');
  NestSupport: array of String = ('G', 'P', 'H', 'F', 'S', 'C', 'A', 'O');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbNestFateFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('nest_fate', 'Nest fate', sdtText,
      crEqual, False, NestFate[cbNestFateFilter.ItemIndex - 1]));
  end;
  if cbNestSupportFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('support_type', 'Support type', sdtText,
      crEqual, False, NestSupport[cbNestSupportFilter.ItemIndex - 1]));
  end;
end;

procedure TfrmCustomGrid.GetEggFilters(aList: TStrings);
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetMethodFilters(aList: TStrings);
begin

end;

procedure TfrmCustomGrid.GetExpeditionFilters(aList: TStrings);
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetSurveyFilters(aList: TStrings);
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetSightingFilters(aList: TStrings);
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetSpecimenFilters(aList: TStrings);
const
  SampleTypes: array of String = ('WS', 'PS', 'N', 'B', 'E', 'P', 'F', 'BS', 'C', 'S', 'T', 'D', 'R');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbMaterialFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('sample_type', 'Sample type', sdtText,
      crEqual, False, SampleTypes[cbMaterialFilter.ItemIndex - 1]));
  end;
end;

procedure TfrmCustomGrid.ClearInstitutionFilters;
begin
  if Filtrado then
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    Filtrado := False;
  end;
  SiteFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearPeopleFilters;
begin
  if Filtrado then
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    Filtrado := False;
  end;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearProjectFilters;
begin
  if Filtrado then
  begin
    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    Filtrado := False;
  end;
  DateFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearPermitFilters;
begin

end;

procedure TfrmCustomGrid.ClearGazetteerFilters;
begin
  if Filtrado then
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    cbSiteRankFilter.ItemIndex := -1;

    Filtrado := False;
  end;
  SiteRankFilter := EmptyStr;
  SiteFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearNetStationFilters;
begin
  if Filtrado then
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    Filtrado := False;
  end;
  SiteFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearTaxonRankFilters;
begin

end;

procedure TfrmCustomGrid.ClearBotanicTaxaFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonRanksFilter.Caption := rsNoneSelected;
    clbTaxonRanksFilter.CheckAll(cbUnchecked, False);

    tsIsSynonym.StateOn := sw_off;
    tsHasSynonyms.StateOn := sw_off;

    Filtrado := False;
  end;
  RanksFilter := EmptyStr;
  IsSynonymFilter := EmptyStr;
  HasSynonymsFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearZooTaxaFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonRanksFilter.Caption := rsNoneSelected;
    clbTaxonRanksFilter.CheckAll(cbUnchecked, False);

    tsTaxonomyClements.StateOn := sw_off;
    tsTaxonomyIoc.StateOn := sw_off;
    tsTaxonomyCbro.StateOn := sw_off;

    tsTaxonExtinct.StateOn := sw_off;

    tsIsSynonym.StateOn := sw_off;
    tsHasSynonyms.StateOn := sw_off;

    Filtrado := False;
  end;
  RanksFilter := EmptyStr;
  ClementsFilter := EmptyStr;
  IocFilter := EmptyStr;
  CbroFilter := EmptyStr;
  ExtinctFilter := EmptyStr;
  IsSynonymFilter := EmptyStr;
  HasSynonymsFilter := EmptyStr;
end;

procedure TfrmCustomGrid.DBGColExit(Sender: TObject);
begin
  { #todo : Return row height to default when exit the cell }
  {$IFNDEF DEBUG}
  TDBGrid(Sender).BeginUpdate;
  TDBGrid(Sender).DefaultRowHeight := TDBGrid(Sender).DefaultRowHeight + 1;
  TDBGrid(Sender).DefaultRowHeight := TDBGrid(Sender).DefaultRowHeight - 1;
  TDBGrid(Sender).EndUpdate;
  {$ENDIF}
end;

procedure TfrmCustomGrid.DBGEditButtonClick(Sender: TObject);
begin
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'taxon_name' then
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'taxon_id', 'taxon_name', True);
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'parent_taxon_name' then
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'parent_taxon_id', 'parent_taxon_name', True);
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'valid_name' then
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'valid_id', 'valid_name', True);

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'country_name' then
    FindSiteDlg([gfCountries], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'country_id', 'country_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'state_name' then
    FindSiteDlg([gfStates], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'state_id', 'state_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'municipality_name' then
    FindSiteDlg([gfCities], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'municipality_id', 'municipality_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'locality_name' then
    FindSiteDlg([gfLocalities], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'locality_id', 'locality_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'parent_site_name' then
    FindSiteDlg([gfLocalities], (Sender as TDBGrid).InplaceEditor,
      (Sender as TDBGrid).DataSource.DataSet, 'parent_site_id', 'parent_site_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'institution_name' then
    FindDlg(tbInstitutions, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'institution_id', 'institution_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'supplier_name' then
    FindDlg(tbInstitutions, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'supplier_id', 'supplier_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'survey_name' then
    FindDlg(tbSurveys, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'survey_id', 'survey_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'observer_name' then
    FindDlg(tbPeople, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'observer_id', 'observer_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'carrier_name' then
    FindDlg(tbPeople, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'carrier_id', 'carrier_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'project_name' then
    FindDlg(tbProjects, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'project_id', 'project_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'individual_name' then
    FindDlg(tbIndividuals, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'individual_id', 'individual_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'father_name' then
    FindDlg(tbIndividuals, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'father_id', 'father_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'mother_name' then
    FindDlg(tbIndividuals, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'mother_id', 'mother_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'nest_name' then
    FindDlg(tbNests, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'nest_id', 'nest_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'band_full_name' then
    FindDlg(tbBands, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'band_id', 'band_full_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'double_band_name' then
    FindDlg(tbBands, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'double_band_id', 'double_band_name');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'removed_band_name' then
    FindDlg(tbBands, (Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'removed_band_id', 'removed_band_name');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'detection_type' then
    DetectionDialog((Sender as TDBGrid).DataSource.DataSet.FieldByName('detection_type').AsString,
      (Sender as TDBGrid).DataSource.DataSet, 'detection_type');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'breeding_status' then
    BreedingDialog((Sender as TDBGrid).DataSource.DataSet.FieldByName('breeding_status').AsString,
      (Sender as TDBGrid).DataSource.DataSet, 'breeding_status');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'molt_limits' then
    MoltLimitsDialog((Sender as TDBGrid).DataSource.DataSet.FieldByName('molt_limits').AsString,
      (Sender as TDBGrid).DataSource.DataSet, 'molt_limits');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'cycle_code' then
    MoltCycleDialog((Sender as TDBGrid).DataSource.DataSet.FieldByName('cycle_code').AsString,
      (Sender as TDBGrid).DataSource.DataSet, 'cycle_code');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'how_aged' then
    HowAgedDialog((Sender as TDBGrid).DataSource.DataSet.FieldByName('how_aged').AsString,
      (Sender as TDBGrid).DataSource.DataSet, 'how_aged');
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'how_sexed' then
    HowAgedDialog((Sender as TDBGrid).DataSource.DataSet.FieldByName('how_sexed').AsString,
      (Sender as TDBGrid).DataSource.DataSet, 'how_sexed');

  if (Sender as TDBGrid).SelectedColumn.FieldName = 'right_leg_below' then
    EditColorBands((Sender as TDBGrid).DataSource.DataSet, 'right_leg_below', (Sender as TDBGrid).InplaceEditor);
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'left_leg_below' then
    EditColorBands((Sender as TDBGrid).DataSource.DataSet, 'left_leg_below', (Sender as TDBGrid).InplaceEditor);
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'right_leg_above' then
    EditColorBands((Sender as TDBGrid).DataSource.DataSet, 'right_leg_above', (Sender as TDBGrid).InplaceEditor);
  if (Sender as TDBGrid).SelectedColumn.FieldName = 'left_leg_above' then
    EditColorBands((Sender as TDBGrid).DataSource.DataSet, 'left_leg_above', (Sender as TDBGrid).InplaceEditor);

  if ((Sender as TDBGrid).SelectedColumn.FieldName = 'sighting_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'measure_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'start_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'end_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'survey_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'birth_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'death_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'banding_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'band_change_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'found_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'last_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'revision_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'dispatch_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'expire_date') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'capture_date') then
    CalendarDlg((Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      (Sender as TDBGrid).SelectedColumn.FieldName);

  if ((Sender as TDBGrid).SelectedColumn.FieldName = 'longitude') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'latitude') then
    GeoEditorDlg((Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'longitude', 'latitude');
  if ((Sender as TDBGrid).SelectedColumn.FieldName = 'start_longitude') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'start_latitude') then
    GeoEditorDlg((Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'start_longitude', 'start_latitude');
  if ((Sender as TDBGrid).SelectedColumn.FieldName = 'end_longitude') or
    ((Sender as TDBGrid).SelectedColumn.FieldName = 'end_latitude') then
    GeoEditorDlg((Sender as TDBGrid).InplaceEditor, (Sender as TDBGrid).DataSource.DataSet,
      'end_longitude', 'end_latitude');

end;

procedure TfrmCustomGrid.ClearBandFilters;
begin
  if Filtrado then
  begin
    cbBandSizeFilter.ItemIndex := -1;

    cbBandStatusFilter.ItemIndex := -1;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    tsBandReported.StateOn := sw_off;
    tsBandNotReported.StateOn := sw_off;

    Filtrado := False;
  end;
  DateFilter := EmptyStr;
  BandSizeFilter := EmptyStr;
  BandStatusFilter := EmptyStr;
  ReportedFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearIndividualFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    if cbSexFilter.ItemIndex > 0 then
      cbSexFilter.ItemIndex := -1;

    if cbAgeFilter.ItemIndex > 0 then
      cbAgeFilter.ItemIndex := -1;

    tsfWithColorBandsFilter.StateOn := sw_off;
    tsfWithRecapturesFilter.StateOn := sw_off;

    Filtrado := False;
  end;
  TaxaFilter := EmptyStr;
  DateFilter := EmptyStr;
  SexFilter := EmptyStr;
  AgeFilter := EmptyStr;
  WithColorFilter := EmptyStr;
  WithRecapturesFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearCaptureFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    if cbSexFilter.ItemIndex > 0 then
      cbSexFilter.ItemIndex := -1;
    if cbCloacalProtuberanceFilter.ItemIndex > 0 then
      cbCloacalProtuberanceFilter.ItemIndex := -1;
    if cbBroodPatchFilter.ItemIndex > 0 then
      cbBroodPatchFilter.ItemIndex := -1;
    eHowSexedFilter.Clear;

    Filtrado := False;
  end;
  TaxaFilter := EmptyStr;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
  SexFilter := EmptyStr;
  CloacalProtuberanceFilter := EmptyStr;
  BroodPatchFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearNestFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbNestFateFilter.ItemIndex := -1;

    cbNestSupportFilter.ItemIndex := -1;

    Filtrado := False;
  end;
  TaxaFilter := EmptyStr;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
  NestFateFilter := EmptyStr;
  NestSupportFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearEggFilters;
begin

end;

procedure TfrmCustomGrid.ClearMethodFilters;
begin

end;

procedure TfrmCustomGrid.ClearExpeditionFilters;
begin
  if Filtrado then
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    Filtrado := False;
  end;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearSurveyFilters;
begin
  if Filtrado then
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    Filtrado := False;
  end;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearSightingFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    Filtrado := False;
  end;
  TaxaFilter := EmptyStr;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearSpecimenFilters;
begin
  if Filtrado then
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    Filtrado := False;
  end;
  TaxaFilter := EmptyStr;
  SiteFilter := EmptyStr;
  DateFilter := EmptyStr;
end;

procedure TfrmCustomGrid.ClearSearch;
begin
  if not CanToggle then
    Exit;

  CanToggle := False;
  tsfMarked.StateOn := sw_off;
  tsfUnmarked.StateOn := sw_off;

  case TableType of
    tbNone: ;
    tbProjectTeams: ;
    tbPermits:
      ClearPermitFilters;
    tbGazetteer:
      ClearGazetteerFilters;
    tbBotanicTaxa:
      ClearBotanicTaxaFilters;
    tbNests:
      ClearNestFilters;
    tbNestRevisions: ;
    tbEggs:
      ClearEggFilters;
    tbNetStations:
      ClearNetStationFilters;
    tbTaxonRanks:
      ClearTaxonRankFilters;
    tbZooTaxa:
      ClearZooTaxaFilters;
    tbProjects:
      ClearProjectFilters;
    tbInstitutions:
      ClearInstitutionFilters;
    tbPeople:
      ClearPeopleFilters;
    tbExpeditions:
      ClearExpeditionFilters;
    tbSurveys:
      ClearSurveyFilters;
    tbMethods:
      ClearMethodFilters;
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbSightings:
      ClearSightingFilters;
    tbSpecimens:
      ClearSpecimenFilters;
    tbSamplePreps: ;
    tbPermanentNets: ;
    tbBands:
      ClearBandFilters;
    tbIndividuals:
      ClearIndividualFilters;
    tbCaptures:
      ClearCaptureFilters;
    tbMolts: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  //EP.Clear;
  SearchStr.Clear;
  FSearch.QuickFilters.Clear;
  Modifier.Reset;

  FSearch.RunSearch;
  //TableSearch(TSQLQuery(dsLink.DataSet), TableType, SearchStr, QuickFilters, Modifier, Sorting, WhereSQL);
  CanToggle := True;
end;

end.

