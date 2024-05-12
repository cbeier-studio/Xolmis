unit ufrm_customgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StrUtils, RegExpr, DB, SQLDB,
  DateUtils, Grids, DBGrids, ExtCtrls, EditBtn, StdCtrls, ComCtrls, Menus, LCLIntf, Character,
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
    cbEggShapeFilter: TComboBox;
    cbEggPatternFilter: TComboBox;
    cbEggTextureFilter: TComboBox;
    cbPermitTypeFilter: TComboBox;
    cbNestStatusFilter: TComboBox;
    cbBandTypeFilter: TComboBox;
    cbBandSourceFilter: TComboBox;
    cbCaptureTypeFilter: TComboBox;
    cbCaptureStatusFilter: TComboBox;
    cbNestStageFilter: TComboBox;
    DBG: TDBGrid;
    dbgRecycle: TDBControlGrid;
    lblRecycleId: TDBText;
    dsLink: TDataSource;
    dsLink1: TDataSource;
    dsLink2: TDataSource;
    dsLink3: TDataSource;
    dsLink4: TDataSource;
    dsLink5: TDataSource;
    eNestFilter: TEditButton;
    eIndividualFilter: TEditButton;
    eExpeditionFilter: TEditButton;
    eEggFilter: TEditButton;
    ePlantFilter: TEditButton;
    eSamplingPlotFilter: TEditButton;
    eSurveyFilter: TEditButton;
    ePersonFilter: TEditButton;
    eInstitutionFilter: TEditButton;
    eMethodFilter: TEditButton;
    eProjectFilter: TEditButton;
    iButtons: TImageList;
    icoEggTextureFilter: TImage;
    icoEggShapeFilter: TImage;
    icoReplacedBandFilter: TImage;
    icoPermitTypeFilter: TImage;
    icoNestStatusFilter: TImage;
    icoBandTypeFilter: TImage;
    icoBandSourceFilter: TImage;
    icoCaptureTypeFilter: TImage;
    icoCaptureStatusFilter: TImage;
    icoDontNeedReviewFilter: TImage;
    icoEggFilter: TImage;
    icoNestStageFilter: TImage;
    icoNidoparasiteFilter: TImage;
    icoHatchedFilter: TImage;
    icoPhilornisFilter: TImage;
    icoRecordInEbirdFilter: TImage;
    icoNotEscapedFilter: TImage;
    icoEscapedFilter: TImage;
    icoNestFilter: TImage;
    icoIndividualFilter: TImage;
    icoExpeditionFilter: TImage;
    icoPlantFilter: TImage;
    icoNeedsReviewFilter: TImage;
    icoOutOfSampleFilter: TImage;
    icoEggPatternFilter: TImage;
    icoSamplingPlotFilter: TImage;
    icoSurveyFilter: TImage;
    icoPersonFilter: TImage;
    icoInstitutionFilter: TImage;
    icoProjectFilter: TImage;
    iHeaders: TImageList;
    icoEmptyQuery: TImage;
    lblEggShapeFilter: TLabel;
    lblReplacedBandFilter: TLabel;
    lblPermitTypeFilter: TLabel;
    lblNestStatusFilter: TLabel;
    lblBandTypeFilter: TLabel;
    lblBandSourceFilter: TLabel;
    lblCaptureTypeFilter: TLabel;
    lblCaptureStatusFilter: TLabel;
    lblEggFilter: TLabel;
    lblNestStageFilter: TLabel;
    lblNidoparasiteFilter: TLabel;
    lblHatchedFilter: TLabel;
    lblEggTextureFilter: TLabel;
    lblPhilornisFilter: TLabel;
    lblRecordInEbirdFilter: TLabel;
    lblNotEscapedFilter: TLabel;
    lblEmptyQuery: TLabel;
    lblEscapedFilter: TLabel;
    lblNestFilter: TLabel;
    lblIndividualFilter: TLabel;
    lblExpeditionFilter: TLabel;
    lblDontNeedReviewFilter: TLabel;
    lblPlantFilter: TLabel;
    lblNeedsReviewFilter: TLabel;
    lblOutOfSampleFilter: TLabel;
    lblEggPatternFilter: TLabel;
    lblSamplingPlotFilter: TLabel;
    lblSurveyFilter: TLabel;
    lblPersonFilter: TLabel;
    lblInstitutionFilter: TLabel;
    lblRecordStatus: TLabel;
    lblChildStatus: TLabel;
    lblRecycleModifiedDate: TDBText;
    lblRecycleName: TDBText;
    lblProjectFilter: TLabel;
    pEggShapeFilter: TBCPanel;
    pEggTraitsFilters: TBCPanel;
    pReplacedBandFilter: TBCPanel;
    pPermitTypeFilter: TBCPanel;
    pNestStatusFilter: TBCPanel;
    pBandTypeFilter: TBCPanel;
    pBandSourceFilter: TBCPanel;
    pCaptureTypeFilter: TBCPanel;
    pCaptureStatusFilter: TBCPanel;
    pFiltersToolbar: TBCPanel;
    pEggFilter: TBCPanel;
    pNestStageFilter: TBCPanel;
    pNidoparasiteFilter: TBCPanel;
    pHatchedFilter: TBCPanel;
    pEggTextureFilter: TBCPanel;
    pPhilornisFilter: TBCPanel;
    pRecordInEbirdFilter: TBCPanel;
    pNotEscapedFilter: TBCPanel;
    pEscapedFilter: TBCPanel;
    pNeedsReviewFilters: TBCPanel;
    pmcNewNestOwner: TMenuItem;
    pEmptyQuery: TBCPanel;
    pmcDel: TMenuItem;
    pmcEdit: TMenuItem;
    pmcRefresh: TMenuItem;
    pmGridChild: TPopupMenu;
    pEscapedFilters: TBCPanel;
    pNestFilter: TBCPanel;
    pIndividualFilter: TBCPanel;
    pExpeditionFilter: TBCPanel;
    pDontNeedReviewFilter: TBCPanel;
    pPlantFilter: TBCPanel;
    pNeedsReviewFilter: TBCPanel;
    pOutOfSampleFilter: TBCPanel;
    pEggPatternFilter: TBCPanel;
    pSamplingPlotFilter: TBCPanel;
    pSurveyFilter: TBCPanel;
    pPersonFilter: TBCPanel;
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
    icoSkullOssificationFilter: TImage;
    icoTaxonomyCbroFilter: TImage;
    icoBandSizeFilter12: TImage;
    icoHowAgedFilter: TImage;
    icoEndTimeFilter: TImage;
    icoBandSizeFilter5: TImage;
    icoBandSizeFilter6: TImage;
    icoBandSizeFilter7: TImage;
    icoBandSizeFilter8: TImage;
    icoTaxonomyIocFilter: TImage;
    icoMarkedFilter: TImage;
    icoStartTimeFilter: TImage;
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
    pMoltCycleFilter: TBCPanel;
    pMoltLimitsFilter: TBCPanel;
    pChildCount2: TBCPanel;
    pChildTag2: TBCPanel;
    pChildCount4: TBCPanel;
    pChildTag4: TBCPanel;
    pmSort: TPopupMenu;
    pNotReportedFilter: TBCPanel;
    pInstitutionFilter: TBCPanel;
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
    pProjectFilter: TBCPanel;
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
    rbReplacedBandAll: TRadioButton;
    rbReplacedBandNo: TRadioButton;
    rbReplacedBandYes: TRadioButton;
    rbNidoparasiteAll: TRadioButton;
    rbHatchedAll: TRadioButton;
    rbPhilornisAll: TRadioButton;
    rbNidoparasiteNo: TRadioButton;
    rbHatchedNo: TRadioButton;
    rbPhilornisNo: TRadioButton;
    rbNidoparasiteYes: TRadioButton;
    rbHatchedYes: TRadioButton;
    rbPhilornisYes: TRadioButton;
    rbRecordInEbirdAll: TRadioButton;
    rbOutOfSampleAll: TRadioButton;
    rbOutOfSampleNo: TRadioButton;
    rbRecordInEbirdYes: TRadioButton;
    rbRecordInEbirdNo: TRadioButton;
    rbOutOfSampleYes: TRadioButton;
    sbAddChild: TSpeedButton;
    sbCancelRecord: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbDelChild: TSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditChild: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbShareRecords: TSpeedButton;
    sbRefreshChild: TSpeedButton;
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
    sbChildNewTab: TSpeedButton;
    sbSortChilds: TSpeedButton;
    sbSortRecords: TSpeedButton;
    Separator10: TShapeLineBGRA;
    Separator11: TShapeLineBGRA;
    Separator12: TMenuItem;
    Separator13: TMenuItem;
    Separator15: TMenuItem;
    Separator16: TShapeLineBGRA;
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
    tsDontNeedReview: TRxSwitch;
    tsBandReported: TRxSwitch;
    tsNotEscaped: TRxSwitch;
    tsNeedsReview: TRxSwitch;
    tsfMarked: TRxSwitch;
    tsfUnmarked: TRxSwitch;
    tsfWithColorBandsFilter: TRxSwitch;
    tsfWithRecapturesFilter: TRxSwitch;
    tsHasSynonyms: TRxSwitch;
    tsIsSynonym: TRxSwitch;
    tsEscaped: TRxSwitch;
    tsTaxonExtinct: TRxSwitch;
    tsTaxonomyCbro: TRxSwitch;
    tsTaxonomyClements: TRxSwitch;
    tsTaxonomyIoc: TRxSwitch;
    tvDateFilter: TLazVirtualStringTree;
    tvSiteFilter: TLazVirtualStringTree;
    tvTaxaFilter: TLazVirtualStringTree;
    procedure DBGColExit(Sender: TObject);
    procedure DBGEditButtonClick(Sender: TObject);
    procedure DBGEditingDone(Sender: TObject);
    procedure DBGMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure DBGPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
      AState: TGridDrawState);
    procedure DBGSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure DBGTitleClick(Column: TColumn);
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
    procedure eAddChildEnter(Sender: TObject);
    procedure eAddChildKeyPress(Sender: TObject; var Key: char);
    procedure eCycleCodeFilterButtonClick(Sender: TObject);
    procedure eCycleCodeFilterKeyPress(Sender: TObject; var Key: char);
    procedure eEggFilterButtonClick(Sender: TObject);
    procedure eEggFilterKeyPress(Sender: TObject; var Key: char);
    procedure eExpeditionFilterButtonClick(Sender: TObject);
    procedure eExpeditionFilterKeyPress(Sender: TObject; var Key: char);
    procedure eHowAgedFilterButtonClick(Sender: TObject);
    procedure eHowAgedFilterKeyPress(Sender: TObject; var Key: char);
    procedure eHowSexedFilterButtonClick(Sender: TObject);
    procedure eHowSexedFilterKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualFilterButtonClick(Sender: TObject);
    procedure eIndividualFilterKeyPress(Sender: TObject; var Key: char);
    procedure eInstitutionFilterButtonClick(Sender: TObject);
    procedure eInstitutionFilterKeyPress(Sender: TObject; var Key: char);
    procedure eMethodFilterButtonClick(Sender: TObject);
    procedure eMethodFilterKeyPress(Sender: TObject; var Key: char);
    procedure eMoltLimitsFilterButtonClick(Sender: TObject);
    procedure eMoltLimitsFilterKeyPress(Sender: TObject; var Key: char);
    procedure eNestFilterButtonClick(Sender: TObject);
    procedure eNestFilterKeyPress(Sender: TObject; var Key: char);
    procedure ePersonFilterButtonClick(Sender: TObject);
    procedure ePersonFilterKeyPress(Sender: TObject; var Key: char);
    procedure ePlantFilterButtonClick(Sender: TObject);
    procedure ePlantFilterKeyPress(Sender: TObject; var Key: char);
    procedure eProjectFilterButtonClick(Sender: TObject);
    procedure eProjectFilterKeyPress(Sender: TObject; var Key: char);
    procedure eSamplingPlotFilterButtonClick(Sender: TObject);
    procedure eSamplingPlotFilterKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyFilterButtonClick(Sender: TObject);
    procedure eSurveyFilterKeyPress(Sender: TObject; var Key: char);
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
    procedure pmcNewNestOwnerClick(Sender: TObject);
    procedure pmcNewNestRevisionClick(Sender: TObject);
    procedure pmcNewPermanentNetClick(Sender: TObject);
    procedure pmcNewProjectMemberClick(Sender: TObject);
    procedure pmcNewSamplePrepClick(Sender: TObject);
    procedure pmcNewSightingClick(Sender: TObject);
    procedure pmcNewSpecimenClick(Sender: TObject);
    procedure pmcNewSurveyMemberClick(Sender: TObject);
    procedure pmcNewWeatherLogClick(Sender: TObject);
    procedure pmtClearSelectionClick(Sender: TObject);
    procedure pmtColapseAllClick(Sender: TObject);
    procedure pmtExpandAllClick(Sender: TObject);
    procedure pmtRefreshClick(Sender: TObject);
    procedure sbAddChildClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure sbColumnHideClick(Sender: TObject);
    procedure sbColumnWidthAutoAdjustClick(Sender: TObject);
    procedure sbDelChildClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbEditChildClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
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
    procedure sbRefreshChildClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbRowHeightDecreaseClick(Sender: TObject);
    procedure sbRowHeightDefaultClick(Sender: TObject);
    procedure sbRowHeightIncreaseClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShareRecordsClick(Sender: TObject);
    procedure sbShowRecordClick(Sender: TObject);
    procedure sbSortChildsClick(Sender: TObject);
    procedure sbSortRecordsClick(Sender: TObject);
    procedure SetFilters(Sender: TObject);
    procedure SplitChildMoved(Sender: TObject);
    procedure SplitRightMoved(Sender: TObject);
    procedure tsfMarkedOff(Sender: TObject);
    procedure tsfMarkedOn(Sender: TObject);
    procedure tsfUnmarkedOff(Sender: TObject);
    procedure tsfUnmarkedOn(Sender: TObject);
    procedure tsfWithColorBandsFilterOn(Sender: TObject);
    procedure tsfWithRecapturesFilterOn(Sender: TObject);
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
    FSearch: TCustomSearch;
    Filtrado: Boolean;
    FSidePanel, OldSidePanel: Boolean;
    FSideIndex, OldSideIndex: Integer;
    FSearchString, OldSearchString: String;
    FPersonKeyFilter, FInstitutionKeyFilter, FSurveyKeyFilter, FMethodKeyFilter: Integer;
    FProjectKeyFilter, FNestKeyFilter, FIndividualKeyFilter, FExpeditionKeyFilter: Integer;
    FPlantKeyFilter, FSamplingPlotKeyFilter, FEggKeyFilter: Integer;
    CanToggle: Boolean;
    FSidePanelFactor: Double;
    FChildPanelFactor: Double;
    cellMemo: TMemo;

    procedure AddGridColumns(aTable: TTableType; aGrid: TDBGrid);
    procedure AddSortedField(aFieldName: String; aDirection: TSortDirection; aCollation: String = '';
      IsAnAlias: Boolean = False);

    procedure ChildTagClick(aTag, aCountTag: TBCPanel);
    procedure ChildTagMouseEnter(aTag, aCountTag: TBCPanel);
    procedure ChildTagMouseLeave(aTag, aCountTag: TBCPanel);

    procedure ClearSearch;
    procedure ClearBandFilters;
    procedure ClearBotanicTaxaFilters;
    procedure ClearCaptureFilters;
    procedure ClearEggFilters;
    procedure ClearExpeditionFilters;
    procedure ClearGazetteerFilters;
    procedure ClearIndividualFilters;
    procedure ClearInstitutionFilters;
    procedure ClearMethodFilters;
    procedure ClearNestFilters;
    procedure ClearNestRevisionFilters;
    procedure ClearNetStationFilters;
    procedure ClearPeopleFilters;
    procedure ClearPermitFilters;
    procedure ClearProjectFilters;
    procedure ClearSightingFilters;
    procedure ClearSpecimenFilters;
    procedure ClearSurveyFilters;
    procedure ClearTaxonRankFilters;
    procedure ClearZooTaxaFilters;

    function GetChildDataSet: TDataSet;

    procedure GetFilters;
    procedure GetBandFilters;
    procedure GetBotanicTaxaFilters;
    procedure GetCaptureFilters;
    procedure GetEggFilters;
    procedure GetExpeditionFilters;
    procedure GetGazetteerFilters;
    procedure GetIndividualFilters;
    procedure GetInstitutionFilters;
    procedure GetMethodFilters;
    procedure GetNestFilters;
    procedure GetNestRevisionFilters;
    procedure GetNetStationFilters;
    procedure GetPeopleFilters;
    procedure GetPermitFilters;
    procedure GetProjectFilters;
    procedure GetSightingFilters;
    procedure GetSpecimenFilters;
    procedure GetSurveyFilters;
    procedure GetTaxonRankFilters;
    procedure GetZooTaxaFilters;

    procedure LoadRecordColumns;
    procedure LoadRecordRow;

    procedure PrepareCanvasBands(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasCaptures(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasEggs(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasExpeditions(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasIndividuals(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasInstitutions(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasMolts(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasNestRevisions(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasNests(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasNetsEffort(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasPeople(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasPermanentNets(const Column: TColumn; const sender: TObject);
    procedure PrepareCanvasPermits(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasProjects(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasSamplePreps(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasSightings(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasSpecimens(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasSurveys(var Column: TColumn; var sender: TObject);

    procedure SetColumnsBands(var aGrid: TDBGrid);
    procedure SetColumnsBotanicTaxa(var aGrid: TDBGrid);
    procedure SetColumnsCaptures(var aGrid: TDBGrid);
    procedure SetColumnsEggs(var aGrid: TDBGrid);
    procedure SetColumnsExpeditions(var aGrid: TDBGrid);
    procedure SetColumnsGazetteer(var aGrid: TDBGrid);
    procedure SetColumnsIndividuals(var aGrid: TDBGrid);
    procedure SetColumnsInstitutions(var aGrid: TDBGrid);
    procedure SetColumnsMethods(var aGrid: TDBGrid);
    procedure SetColumnsNestOwners(var aGrid: TDBGrid);
    procedure SetColumnsNestRevisions(var aGrid: TDBGrid);
    procedure SetColumnsNests(var aGrid: TDBGrid);
    procedure SetColumnsNetStations(var aGrid: TDBGrid);
    procedure SetColumnsPeople(var aGrid: TDBGrid);
    procedure SetColumnsPermanentNets(var aGrid: TDBGrid);
    procedure SetColumnsPermits(var aGrid: TDBGrid);
    procedure SetColumnsProjects(var aGrid: TDBGrid);
    procedure SetColumnsSightings(var aGrid: TDBGrid);
    procedure SetColumnsSpecimens(var aGrid: TDBGrid);
    procedure SetColumnsSurveys(var aGrid: TDBGrid);
    procedure SetColumnsTaxonRanks(var aGrid: TDBGrid);
    procedure SetColumnsWeatherLogs(var aGrid: TDBGrid);

    procedure SetGridAndChild;
    procedure SetGridBands;
    procedure SetGridBotanicTaxa;
    procedure SetGridCaptures;
    procedure SetGridColumns(aTable: TTableType; aGrid: TDBGrid);
    procedure SetGridEggs;
    procedure SetGridGazetteer;
    procedure SetGridIndividuals;
    procedure SetGridInstitutions;
    procedure SetGridMethods;
    procedure SetGridNestRevisions;
    procedure SetGridNests;
    procedure SetGridExpeditions;
    procedure SetGridNetStations;
    procedure SetGridPeople;
    procedure SetGridPermits;
    procedure SetGridProjects;
    procedure SetGridSightings;
    procedure SetGridSpecimens;
    procedure SetGridSurveys;
    procedure SetGridTaxonRanks;

    procedure SetSidePanel(aValue: Boolean);
    procedure SetSideIndex(aValue: Integer);
    procedure SetSearchString(aValue: String);

    function Search(aValue: String): Boolean;
    function SearchBands(aValue: String): Boolean;
    function SearchBotanicTaxa(aValue: String): Boolean;
    function SearchCaptures(aValue: String): Boolean;
    function SearchEggs(aValue: String): Boolean;
    function SearchExpeditions(aValue: String): Boolean;
    function SearchGazetteer(aValue: String): Boolean;
    function SearchIndividuals(aValue: String): Boolean;
    function SearchInstitutions(aValue: String): Boolean;
    function SearchMethods(aValue: String): Boolean;
    function SearchNestRevisions(aValue: String): Boolean;
    function SearchNests(aValue: String): Boolean;
    function SearchNetStations(aValue: String): Boolean;
    function SearchPeople(aValue: String): Boolean;
    function SearchPermits(aValue: String): Boolean;
    function SearchProjects(aValue: String): Boolean;
    function SearchSightings(aValue: String): Boolean;
    function SearchSpecimens(aValue: String): Boolean;
    function SearchSurveys(aValue: String): Boolean;
    function SearchTaxonRanks(aValue: String): Boolean;
    function SearchZooTaxa(aValue: String): Boolean;

    procedure UpdateButtons(aDataSet: TDataSet);
    procedure UpdateChildBar;
    procedure UpdateChildButtons(aDataSet: TDataSet);
    procedure UpdateChildCount;
    procedure UpdateChildStatus;
    procedure UpdateGridTitles(aGrid: TDBGrid; aSearch: TCustomSearch);

    procedure UpdateFilterPanels;
    procedure UpdateFilterPanelsBands;
    procedure UpdateFilterPanelsBotanicTaxa;
    procedure UpdateFilterPanelsCaptures;
    procedure UpdateFilterPanelsEggs;
    procedure UpdateFilterPanelsExpeditions;
    procedure UpdateFilterPanelsGazetteer;
    procedure UpdateFilterPanelsIndividuals;
    procedure UpdateFilterPanelsInstitutions;
    procedure UpdateFilterPanelsNestRevisions;
    procedure UpdateFilterPanelsNests;
    procedure UpdateFilterPanelsNetStations;
    procedure UpdateFilterPanelsPeople;
    procedure UpdateFilterPanelsPermits;
    procedure UpdateFilterPanelsProjects;
    procedure UpdateFilterPanelsSightings;
    procedure UpdateFilterPanelsSpecimens;
    procedure UpdateFilterPanelsSurveys;
    procedure UpdateFilterPanelsZooTaxa;
  public
    property TableType: TTableType read FTableType write FTableType;

    property SearchString: String read FSearchString write SetSearchString;
    property ShowSidePanel: Boolean read FSidePanel write SetSidePanel;
    property SidePanelIndex: Integer read FSideIndex write SetSideIndex;
  end;

var
  frmCustomGrid: TfrmCustomGrid;

implementation

uses
  cbs_locale, cbs_global, cbs_system, cbs_themes, cbs_gis, cbs_birds, cbs_editdialogs, cbs_dialogs,
  cbs_finddialogs, cbs_data, cbs_getvalue, cbs_taxonomy, {$IFDEF DEBUG}cbs_debug,{$ENDIF}
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

  UpdateGridTitles(DBG, FSearch);
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

procedure TfrmCustomGrid.ClearBandFilters;
begin
  cbBandSizeFilter.ItemIndex := 0;

  cbBandStatusFilter.ItemIndex := 0;
  cbBandTypeFilter.ItemIndex := 0;
  cbBandSourceFilter.ItemIndex := 0;

  tsBandReported.StateOn := sw_off;
  tsBandNotReported.StateOn := sw_off;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eInstitutionFilter.Clear;
  FInstitutionKeyFilter := 0;
  eProjectFilter.Clear;
  FProjectKeyFilter := 0
end;

procedure TfrmCustomGrid.ClearBotanicTaxaFilters;
begin
  lblCountTaxonRanksFilter.Caption := rsNoneSelected;
  clbTaxonRanksFilter.CheckAll(cbUnchecked, False);

  tsIsSynonym.StateOn := sw_off;
  tsHasSynonyms.StateOn := sw_off;
end;

procedure TfrmCustomGrid.ClearCaptureFilters;
begin
  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  cbCaptureTypeFilter.ItemIndex := 0;
  cbCaptureStatusFilter.ItemIndex := 0;

  cbAgeFilter.ItemIndex := 0;
  cbSkullOssificationFilter.ItemIndex := 0;
  eHowAgedFilter.Clear;

  cbSexFilter.ItemIndex := 0;
  cbCloacalProtuberanceFilter.ItemIndex := 0;
  cbBroodPatchFilter.ItemIndex := 0;
  eHowSexedFilter.Clear;

  cbFatFilter.ItemIndex := 0;

  cbBodyMoltFilter.ItemIndex := 0;
  cbFFMoltFilter.ItemIndex := 0;
  cbFFWearFilter.ItemIndex := 0;
  eMoltLimitsFilter.Clear;
  eCycleCodeFilter.Clear;

  eStartTimeFilter.Clear;
  eEndTimeFilter.Clear;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eSurveyFilter.Clear;
  FSurveyKeyFilter := 0;
  eMethodFilter.Clear;
  FMethodKeyFilter := 0;
  eSamplingPlotFilter.Clear;
  FSamplingPlotKeyFilter := 0;
  eIndividualFilter.Clear;
  FIndividualKeyFilter := 0;

  tsNeedsReview.StateOn := sw_off;
  tsDontNeedReview.StateOn := sw_off;

  tsEscaped.StateOn := sw_off;
  tsNotEscaped.StateOn := sw_off;

  rbPhilornisAll.Checked := True;
  rbReplacedBandAll.Checked := True;
end;

procedure TfrmCustomGrid.ClearEggFilters;
begin
  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  cbEggShapeFilter.ItemIndex := 0;
  cbEggPatternFilter.ItemIndex := 0;
  cbEggTextureFilter.ItemIndex := 0;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eNestFilter.Clear;
  FNestKeyFilter := 0;
  eIndividualFilter.Clear;
  FIndividualKeyFilter := 0;

  rbHatchedAll.Checked := True;
end;

procedure TfrmCustomGrid.ClearExpeditionFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eProjectFilter.Clear;
  FProjectKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearGazetteerFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  cbSiteRankFilter.ItemIndex := 0;
end;

procedure TfrmCustomGrid.ClearIndividualFilters;
begin
  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  cbSexFilter.ItemIndex := 0;
  cbCloacalProtuberanceFilter.ItemIndex := 0;
  cbBroodPatchFilter.ItemIndex := 0;
  eHowSexedFilter.Clear;

  cbAgeFilter.ItemIndex := 0;
  cbSkullOssificationFilter.ItemIndex := 0;
  eHowAgedFilter.Clear;

  tsfWithColorBandsFilter.StateOn := sw_off;
  tsfWithRecapturesFilter.StateOn := sw_off;

  rbReplacedBandAll.Checked := True;

  eNestFilter.Clear;
  FNestKeyFilter := 0;
  eIndividualFilter.Clear;
  FIndividualKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearInstitutionFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;
end;

procedure TfrmCustomGrid.ClearMethodFilters;
begin

end;

procedure TfrmCustomGrid.ClearNestFilters;
begin
  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  cbNestFateFilter.ItemIndex := 0;

  cbNestSupportFilter.ItemIndex := 0;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eProjectFilter.Clear;
  FProjectKeyFilter := 0;
  ePlantFilter.Clear;
  FPlantKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearNestRevisionFilters;
begin
  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  cbNestStatusFilter.ItemIndex := 0;
  cbNestStageFilter.ItemIndex := 0;

  eStartTimeFilter.Clear;
  eEndTimeFilter.Clear;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eNestFilter.Clear;
  FNestKeyFilter := 0;

  rbNidoparasiteAll.Checked := True;
  rbPhilornisAll.Checked := True;
end;

procedure TfrmCustomGrid.ClearNetStationFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;
end;

procedure TfrmCustomGrid.ClearPeopleFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eInstitutionFilter.Clear;
  FInstitutionKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearPermitFilters;
begin
  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eProjectFilter.Clear;
  FProjectKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearProjectFilters;
begin
  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;
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
    //tbProjectTeams: ;
    tbPermits:       ClearPermitFilters;
    tbGazetteer:     ClearGazetteerFilters;
    tbBotanicTaxa:   ClearBotanicTaxaFilters;
    tbNests:         ClearNestFilters;
    tbNestRevisions: ClearNestRevisionFilters;
    tbEggs:          ClearEggFilters;
    tbNetStations:   ClearNetStationFilters;
    tbTaxonRanks:    ClearTaxonRankFilters;
    tbZooTaxa:       ClearZooTaxaFilters;
    tbProjects:      ClearProjectFilters;
    tbInstitutions:  ClearInstitutionFilters;
    tbPeople:        ClearPeopleFilters;
    tbExpeditions:   ClearExpeditionFilters;
    tbSurveys:       ClearSurveyFilters;
    tbMethods:       ClearMethodFilters;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings:     ClearSightingFilters;
    tbSpecimens:     ClearSpecimenFilters;
    //tbSamplePreps: ;
    //tbPermanentNets: ;
    tbBands:         ClearBandFilters;
    tbIndividuals:   ClearIndividualFilters;
    tbCaptures:      ClearCaptureFilters;
    //tbMolts: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  //EP.Clear;
  FSearch.QuickFilters.Clear;

  FSearch.RunSearch;

  CanToggle := True;
end;

procedure TfrmCustomGrid.ClearSightingFilters;
begin
  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eStartTimeFilter.Clear;
  eEndTimeFilter.Clear;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eSurveyFilter.Clear;
  FSurveyKeyFilter := 0;
  eMethodFilter.Clear;
  FMethodKeyFilter := 0;
  eIndividualFilter.Clear;
  FIndividualKeyFilter := 0;

  rbRecordInEbirdAll.Checked := True;
  rbOutOfSampleAll.Checked := True;
end;

procedure TfrmCustomGrid.ClearSpecimenFilters;
begin
  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  cbMaterialFilter.ItemIndex := 0;

  eNestFilter.Clear;
  FNestKeyFilter := 0;
  eEggFilter.Clear;
  FEggKeyFilter := 0;
  eIndividualFilter.Clear;
  FIndividualKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearSurveyFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eStartTimeFilter.Clear;
  eEndTimeFilter.Clear;

  eMethodFilter.Clear;
  FMethodKeyFilter := 0;
  eProjectFilter.Clear;
  FProjectKeyFilter := 0;
  eSamplingPlotFilter.Clear;
  FSamplingPlotKeyFilter := 0;
  eExpeditionFilter.Clear;
  FExpeditionKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearTaxonRankFilters;
begin

end;

procedure TfrmCustomGrid.ClearZooTaxaFilters;
begin
  lblCountTaxonRanksFilter.Caption := rsNoneSelected;
  clbTaxonRanksFilter.CheckAll(cbUnchecked, False);

  tsTaxonomyClements.StateOn := sw_off;
  tsTaxonomyIoc.StateOn := sw_off;
  tsTaxonomyCbro.StateOn := sw_off;

  tsTaxonExtinct.StateOn := sw_off;

  tsIsSynonym.StateOn := sw_off;
  tsHasSynonyms.StateOn := sw_off;
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
  with (Sender as TDBGrid), SelectedColumn do
  begin
    if FieldName = 'taxon_name' then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'taxon_id', 'taxon_name', True);
    if FieldName = 'parent_taxon_name' then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'parent_taxon_id', 'parent_taxon_name', True);
    if FieldName = 'valid_name' then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'valid_id', 'valid_name', True);

    if FieldName = 'country_name' then
      FindSiteDlg([gfCountries], InplaceEditor, DataSource.DataSet, 'country_id', 'country_name');
    if FieldName = 'state_name' then
      FindSiteDlg([gfStates], InplaceEditor, DataSource.DataSet, 'state_id', 'state_name');
    if FieldName = 'municipality_name' then
      FindSiteDlg([gfCities], InplaceEditor, DataSource.DataSet, 'municipality_id', 'municipality_name');
    if FieldName = 'locality_name' then
      FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, 'locality_id', 'locality_name');
    if FieldName = 'parent_site_name' then
      FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, 'parent_site_id', 'parent_site_name');

    if FieldName = 'institution_name' then
      FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, 'institution_id', 'institution_name');
    if FieldName = 'supplier_name' then
      FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, 'supplier_id', 'supplier_name');

    if FieldName = 'survey_name' then
      FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, 'survey_id', 'survey_name');

    if FieldName = 'observer_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_id', 'observer_name');
    if FieldName = 'carrier_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'carrier_id', 'carrier_name');

    if FieldName = 'project_name' then
      FindDlg(tbProjects, InplaceEditor, DataSource.DataSet, 'project_id', 'project_name');

    if FieldName = 'individual_name' then
      FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'individual_id', 'individual_name');
    if FieldName = 'father_name' then
      FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'father_id', 'father_name');
    if FieldName = 'mother_name' then
      FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'mother_id', 'mother_name');

    if FieldName = 'nest_name' then
      FindDlg(tbNests, InplaceEditor, DataSource.DataSet, 'nest_id', 'nest_name');

    if FieldName = 'band_full_name' then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_full_name');
    if FieldName = 'double_band_name' then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'double_band_id', 'double_band_name');
    if FieldName = 'removed_band_name' then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'removed_band_id', 'removed_band_name');

    if FieldName = 'detection_type' then
      DetectionDialog(DataSource.DataSet.FieldByName('detection_type').AsString,
        DataSource.DataSet, 'detection_type');
    if FieldName = 'breeding_status' then
      BreedingDialog(DataSource.DataSet.FieldByName('breeding_status').AsString,
        DataSource.DataSet, 'breeding_status');

    if FieldName = 'molt_limits' then
      MoltLimitsDialog(DataSource.DataSet.FieldByName('molt_limits').AsString, DataSource.DataSet, 'molt_limits');
    if FieldName = 'cycle_code' then
      MoltCycleDialog(DataSource.DataSet.FieldByName('cycle_code').AsString, DataSource.DataSet, 'cycle_code');
    if FieldName = 'how_aged' then
      HowAgedDialog(DataSource.DataSet.FieldByName('how_aged').AsString,  DataSource.DataSet, 'how_aged');
    if FieldName = 'how_sexed' then
      HowAgedDialog(DataSource.DataSet.FieldByName('how_sexed').AsString, DataSource.DataSet, 'how_sexed');

    if FieldName = 'right_leg_below' then
      EditColorBands(DataSource.DataSet, 'right_leg_below', InplaceEditor);
    if FieldName = 'left_leg_below' then
      EditColorBands(DataSource.DataSet, 'left_leg_below', InplaceEditor);
    if FieldName = 'right_leg_above' then
      EditColorBands(DataSource.DataSet, 'right_leg_above', InplaceEditor);
    if FieldName = 'left_leg_above' then
      EditColorBands(DataSource.DataSet, 'left_leg_above', InplaceEditor);

    if (FieldName = 'sighting_date') or
      (FieldName = 'measure_date') or
      (FieldName = 'start_date') or
      (FieldName = 'end_date') or
      (FieldName = 'survey_date') or
      (FieldName = 'birth_date') or
      (FieldName = 'death_date') or
      (FieldName = 'banding_date') or
      (FieldName = 'band_change_date') or
      (FieldName = 'found_date') or
      (FieldName = 'last_date') or
      (FieldName = 'revision_date') or
      (FieldName = 'dispatch_date') or
      (FieldName = 'expire_date') or
      (FieldName = 'capture_date') then
      CalendarDlg(InplaceEditor, DataSource.DataSet, FieldName);

    if (FieldName = 'longitude') or (FieldName = 'latitude') then
      GeoEditorDlg(InplaceEditor, DataSource.DataSet, 'longitude', 'latitude');
    if (FieldName = 'start_longitude') or (FieldName = 'start_latitude') then
      GeoEditorDlg(InplaceEditor, DataSource.DataSet, 'start_longitude', 'start_latitude');
    if (FieldName = 'end_longitude') or (FieldName = 'end_latitude') then
      GeoEditorDlg(InplaceEditor, DataSource.DataSet, 'end_longitude', 'end_latitude');

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

procedure TfrmCustomGrid.DBGPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
var
  //MyTextStyle: TTextStyle;
  aTable: TTableType;
begin
  aTable := tbNone;

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
          aTable := tbNestOwners
        else
        if Sender = gridChild2 then
          aTable := tbNestRevisions
        else
        if Sender = gridChild3 then
          aTable := tbEggs;
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
    tbInstitutions:   PrepareCanvasInstitutions(Column, sender);
    tbPeople:         PrepareCanvasPeople(Column, sender);
    tbBands:          PrepareCanvasBands(Column, sender);
    tbIndividuals:    PrepareCanvasIndividuals(Column, sender);
    tbCaptures:       PrepareCanvasCaptures(Column, sender);
    tbMolts:          PrepareCanvasMolts(Column, sender);
    tbNests:          PrepareCanvasNests(Column, sender);
    //tbNestOwners: ;
    tbNestRevisions:  PrepareCanvasNestRevisions(Column, sender);
    tbEggs:           PrepareCanvasEggs(Column, sender);
    tbExpeditions:    PrepareCanvasExpeditions(Column, sender);
    tbSurveys:        PrepareCanvasSurveys(Column, sender);
    tbSightings:      PrepareCanvasSightings(Column, sender);
    tbNetsEffort:     PrepareCanvasNetsEffort(Column, sender);
    tbSpecimens:      PrepareCanvasSpecimens(Column, sender);
    tbSamplePreps:    PrepareCanvasSamplePreps(Column, sender);
    tbProjects:       PrepareCanvasProjects(Column, sender);
    tbPermits:        PrepareCanvasPermits(Column, sender);
    tbPermanentNets:  PrepareCanvasPermanentNets(Column, sender);
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

procedure TfrmCustomGrid.DBGTitleClick(Column: TColumn);
var
  Direction: TSortDirection;
begin
  Direction := sdAscending;

  if FSearch.SortFields.Count > 0 then
    if Column.FieldName = FSearch.SortFields[0].FieldName then
      if FSearch.SortFields[0].Direction = sdAscending then
        Direction := sdDescending
      else
        Direction := sdAscending;

  FSearch.SortFields.Clear;
  if not (pfInUpdate in DBG.DataSource.DataSet.FieldByName(Column.FieldName).ProviderFlags) then
    AddSortedField(Column.FieldName, Direction, '', True)
  else
    AddSortedField(Column.FieldName, Direction);
  Search(FSearchString);
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

  UpdateChildBar;
end;

procedure TfrmCustomGrid.dsLinkStateChange(Sender: TObject);
begin
  if Assigned(dsLink.DataSet) then
    UpdateButtons(dsLink.DataSet);

  pEmptyQuery.Visible := dsLink.DataSet.RecordCount = 0;

  UpdateChildBar;
end;

procedure TfrmCustomGrid.eAddChildEnter(Sender: TObject);
var
  aSurvey: Integer;
  Qry: TSQLQuery;
begin
  aSurvey := 0;

  if (FTableType = tbExpeditions) and (FChildTable = tbSurveys) then
  begin
    if FindDlg(tbSurveys, eAddChild, aSurvey) then
    begin
      Qry := TSQLQuery.Create(DMM.sqlCon);
      with Qry, SQL do
      try
        SQLConnection := DMM.sqlCon;
        Clear;

        Add('UPDATE surveys INTO expedition_id = :aexpedition WHERE survey_id = :asurvey');
        ParamByName('AEXPEDITION').AsInteger := DBG.DataSource.DataSet.FieldByName('expedition_id').AsInteger;
        ParamByName('ASURVEY').AsInteger := aSurvey;

        ExecSQL;
      finally
        FreeAndNil(Qry);
      end;
    end;
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

procedure TfrmCustomGrid.eCycleCodeFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  MoltCycleDialog(eCycleCodeFilter);
end;

procedure TfrmCustomGrid.eCycleCodeFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eCycleCodeFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eEggFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbEggs, eEggFilter, FEggKeyFilter);
end;

procedure TfrmCustomGrid.eEggFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbEggs, eEggFilter, FEggKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eEggFilter.Clear;
    FEggKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eExpeditionFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbExpeditions, eExpeditionFilter, FExpeditionKeyFilter);
end;

procedure TfrmCustomGrid.eExpeditionFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbExpeditions, eExpeditionFilter, FExpeditionKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eExpeditionFilter.Clear;
    FExpeditionKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eHowAgedFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  HowAgedDialog(eHowAgedFilter);
end;

procedure TfrmCustomGrid.eHowAgedFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eHowAgedFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eHowSexedFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  HowAgedDialog(eHowAgedFilter);
end;

procedure TfrmCustomGrid.eHowSexedFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eHowSexedFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eIndividualFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbIndividuals, eIndividualFilter, FIndividualKeyFilter);
end;

procedure TfrmCustomGrid.eIndividualFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbIndividuals, eIndividualFilter, FIndividualKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eIndividualFilter.Clear;
    FIndividualKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eInstitutionFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbInstitutions, eInstitutionFilter, FInstitutionKeyFilter);
end;

procedure TfrmCustomGrid.eInstitutionFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbInstitutions, eInstitutionFilter, FInstitutionKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eInstitutionFilter.Clear;
    FInstitutionKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eMethodFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbMethods, eMethodFilter, FMethodKeyFilter);
end;

procedure TfrmCustomGrid.eMethodFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbMethods, eMethodFilter, FMethodKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eMethodFilter.Clear;
    FMethodKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eMoltLimitsFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  MoltLimitsDialog(eMoltLimitsFilter);
end;

procedure TfrmCustomGrid.eMoltLimitsFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eMoltLimitsFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eNestFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbNests, eNestFilter, FNestKeyFilter);
end;

procedure TfrmCustomGrid.eNestFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbNests, eNestFilter, FNestKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eNestFilter.Clear;
    FNestKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.ePersonFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbPeople, ePersonFilter, FPersonKeyFilter);
end;

procedure TfrmCustomGrid.ePersonFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbPeople, ePersonFilter, FPersonKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    ePersonFilter.Clear;
    FPersonKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.ePlantFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbBotanicTaxa, ePlantFilter, FPlantKeyFilter);
end;

procedure TfrmCustomGrid.ePlantFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbBotanicTaxa, ePlantFilter, FPlantKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    ePlantFilter.Clear;
    FPlantKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eProjectFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbProjects, eProjectFilter, FProjectKeyFilter);
end;

procedure TfrmCustomGrid.eProjectFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProjectFilter, FProjectKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eProjectFilter.Clear;
    FProjectKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eSamplingPlotFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbNetStations, eSamplingPlotFilter, FSamplingPlotKeyFilter);
end;

procedure TfrmCustomGrid.eSamplingPlotFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbNetStations, eSamplingPlotFilter, FSamplingPlotKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eSamplingPlotFilter.Clear;
    FSamplingPlotKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eSurveyFilterButtonClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  FindDlg(tbSurveys, eSurveyFilter, FSurveyKeyFilter);
end;

procedure TfrmCustomGrid.eSurveyFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not CanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbSurveys, eSurveyFilter, FSurveyKeyFilter, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eSurveyFilter.Clear;
    FSurveyKeyFilter := 0;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (XSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TimerUpdate.Enabled := False;
  //TimerFind.Enabled := False;

  if Assigned(dsLink5.DataSet) then
    dsLink5.DataSet.Close;
  if Assigned(dsLink4.DataSet) then
    dsLink4.DataSet.Close;
  if Assigned(dsLink3.DataSet) then
    dsLink3.DataSet.Close;
  if Assigned(dsLink2.DataSet) then
    dsLink2.DataSet.Close;
  if Assigned(dsLink1.DataSet) then
    dsLink1.DataSet.Close;

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
  Filtrado := False;

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

  FreeAndNil(FSearch);
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
    if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
      (dsLink.DataSet as TSQLQuery).ReadOnly := True;
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
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink2.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink3.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink4.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink5.DataSet as TSQLQuery).ReadOnly := True;
        end;
      end;
      tbNests:
      begin
        AddGridColumns(tbNestOwners, gridChild1);
        dsLink1.DataSet.Open;
        AddGridColumns(tbNestRevisions, gridChild2);
        dsLink2.DataSet.Open;
        AddGridColumns(tbEggs, gridChild3);
        dsLink3.DataSet.Open;
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink2.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink3.DataSet as TSQLQuery).ReadOnly := True;
        end;
      end;
      tbExpeditions:
      begin
        AddGridColumns(tbSurveys, gridChild1);
        dsLink1.DataSet.Open;
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
        end;
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
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink2.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink3.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink4.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink5.DataSet as TSQLQuery).ReadOnly := True;
        end;
      end;
      tbSpecimens:
      begin
        AddGridColumns(tbSpecimenCollectors, gridChild1);
        dsLink1.DataSet.Open;
        AddGridColumns(tbSamplePreps, gridChild2);
        dsLink2.DataSet.Open;
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
          (dsLink2.DataSet as TSQLQuery).ReadOnly := True;
        end;
      end;
      tbNetStations:
      begin
        AddGridColumns(tbPermanentNets, gridChild1);
        dsLink1.DataSet.Open;
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
        end;
      end;
      tbProjects:
      begin
        AddGridColumns(tbProjectTeams, gridChild1);
        dsLink1.DataSet.Open;
        if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
        begin
          (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
        end;
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
  //UpdateGridTitles(DBG, FSearch);
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

procedure TfrmCustomGrid.GetBandFilters;
const
  BandStatus: array of String = ('D', 'U', 'R', 'T', 'Q', 'P');
  BandTypes: array of String = ('A', 'F', 'N', 'W', 'T', 'L', 'R', 'C', 'O');
  BandSources: array of String = ('A', 'T', 'L', 'D', 'F');
var
  sf: Integer;
begin
  if cbBandSizeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_size', 'Band size', sdtText,
      crEqual, False, cbBandSizeFilter.Text));
  end;

  if cbBandStatusFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_status', 'Band status', sdtText,
      crEqual, False, BandStatus[cbBandStatusFilter.ItemIndex - 1]));
  end;

  if cbBandTypeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_type', 'Band type', sdtText,
      crEqual, False, BandTypes[cbBandTypeFilter.ItemIndex - 1]));
  end;

  if cbBandSourceFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_source', 'Band source', sdtText,
      crEqual, False, BandSources[cbBandSourceFilter.ItemIndex - 1]));
  end;

  if tsBandReported.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_reported', 'Band reported', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsBandNotReported.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_reported', 'Band reported', sdtBoolean,
      crEqual, False, '0'));
  end;

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if FInstitutionKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('supplier_id', 'Supplier', sdtInteger,
      crEqual, False, IntToStr(FInstitutionKeyFilter)));
  end;

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('project_id', 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetBotanicTaxaFilters;
var
  sf: Integer;
begin
  //if RanksFilter <> EmptyStr then
  //  aList.Add(RanksFilter);

  if tsIsSynonym.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crNotEqual, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;
end;

procedure TfrmCustomGrid.GetCaptureFilters;
const
  BirdAge: array of String = ('U', 'A', 'I', 'J', 'N', 'F', 'S', 'T', '4', '5');
  BirdSex: array of String = ('M', 'F', 'U');
  CaptureType: array of String = ('N', 'R', 'S', 'C', 'U');
  CaptureStatus: array of String = ('N', 'I', 'W', 'X', 'D');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbCaptureTypeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('capture_type', 'Type', sdtText,
      crEqual, False, CaptureType[cbCaptureTypeFilter.ItemIndex - 1]));
  end;

  if cbCaptureStatusFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('subject_status', 'Status', sdtText,
      crEqual, False, CaptureStatus[cbCaptureStatusFilter.ItemIndex - 1]));
  end;

  if cbAgeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('subject_age', 'Age', sdtText,
      crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
  end;
  if eHowAgedFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('how_aged', 'How was aged', sdtText,
      crLike, False, eHowAgedFilter.Text));
  end;
  if cbSexFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('subject_sex', 'Sex', sdtText,
      crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
  end;
  if eHowSexedFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('how_sexed', 'How was sexed', sdtText,
      crLike, False, eHowSexedFilter.Text));
  end;

  if cbCloacalProtuberanceFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('cloacal_protuberance', 'Cloacal protuberance', sdtText,
      crEqual, False, cbCloacalProtuberanceFilter.Text));
  end;
  if cbBroodPatchFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('brood_patch', 'Brood patch', sdtText,
      crEqual, False, cbBroodPatchFilter.Text));
  end;

  if cbFatFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('fat', 'Subcutaneous fat', sdtText,
      crEqual, False, cbFatFilter.Text));
  end;

  if cbBodyMoltFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('body_molt', 'Body molt', sdtText,
      crEqual, False, cbBodyMoltFilter.Text));
  end;
  if cbFFMoltFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('flight_feathers_molt', 'Flight feathers molt', sdtText,
      crEqual, False, cbFFMoltFilter.Text));
  end;
  if cbFFWearFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('flight_feathers_wear', 'Flight feathers wear', sdtText,
      crEqual, False, cbFFWearFilter.Text));
  end;
  if eMoltLimitsFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('molt_limits', 'Molt limits', sdtText,
      crLike, False, eMoltLimitsFilter.Text));
  end;
  if eCycleCodeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('cycle_code', 'Molt cycle', sdtText,
      crLike, False, eCycleCodeFilter.Text));
  end;

  if cbSkullOssificationFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('skull_ossification', 'Skull ossification', sdtText,
      crEqual, False, cbSkullOssificationFilter.Text));
  end;

  if FPersonKeyFilter > 0 then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('capture_time', 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('capture_time', 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FSurveyKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('survey_id', 'Survey', sdtInteger,
      crEqual, False, IntToStr(FSurveyKeyFilter)));
  end;
  if FMethodKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('method_id', 'Method', sdtInteger,
      crEqual, False, IntToStr(FMethodKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('individual_id', 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if tsNeedsReview.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('needs_review', 'Needs review', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsDontNeedReview.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('Needs review', 'Needs review', sdtBoolean,
      crEqual, False, '0'));
  end;

  if tsEscaped.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('escaped', 'Escaped', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsNotEscaped.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('escaped', 'Escaped', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbPhilornisYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('philornis_larvae_tally', '# Philornis larvae', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbPhilornisNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('philornis_larvae_tally', '# Philornis larvae', sdtInteger,
      crEqual, False, '0'));
  end;

  if rbReplacedBandYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('removed_band_id', 'Removed band', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbReplacedBandNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('removed_band_id', 'Removed band', sdtInteger,
      crEqual, False, '0'));
  end;
end;

function TfrmCustomGrid.GetChildDataSet: TDataSet;
begin
  Result := nil;

  case FTableType of
    tbNetStations:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    tbProjects:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    tbIndividuals:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
        2: Result := dsLink3.DataSet;
        3: Result := dsLink4.DataSet;
        4: Result := dsLink5.DataSet;
      end;
    tbNests:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
        2: Result := dsLink3.DataSet;
      end;
    tbExpeditions:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    tbSurveys:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
        2: Result := dsLink3.DataSet;
        3: Result := dsLink4.DataSet;
        4: Result := dsLink5.DataSet;
      end;
    tbSightings: ;
    tbSpecimens:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    //tbProjectTeams: ;
    //tbPermits: ;
    //tbTaxonRanks: ;
    //tbZooTaxa: ;
    //tbBotanicTaxa: ;
    //tbBands: ;
    //tbBandHistory: ;
    //tbCaptures: ;
    //tbMolts: ;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  else
    Result := nil;
  end;

end;

procedure TfrmCustomGrid.GetEggFilters;
const
  EggShapes: array of String = ('S', 'E', 'O', 'P', 'C', 'B', 'Y', 'L', 'U');
  EggPatterns: array of String = ('P', 'B', 'S', 'T', 'W', 'PS', 'BS', 'U');
  EggTextures: array of String = ('C', 'S', 'G', 'P', 'U');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_id', 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('individual_id', 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if cbEggShapeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('egg_shape', 'Shape', sdtText,
      crEqual, False, EggShapes[cbEggShapeFilter.ItemIndex - 1]));
  end;
  if cbEggPatternFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('eggshell_pattern', 'Pattern', sdtText,
      crEqual, False, EggPatterns[cbEggPatternFilter.ItemIndex - 1]));
  end;
  if cbEggTextureFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('eggshell_texture', 'Texture', sdtText,
      crEqual, False, EggTextures[cbEggTextureFilter.ItemIndex - 1]));
  end;

  if rbHatchedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('egg_hatched', 'Hatched', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbHatchedNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('egg_hatched', 'Hatched', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetExpeditionFilters;
var
  sf: Integer;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('project_id', 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetFilters;
var
  sf: Integer;
begin
  if not CanToggle then
    Exit;

  CanToggle := False;

  if (tsfMarked.StateOn = sw_on) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (tsfUnmarked.StateOn = sw_on) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '0'));
  end;

  case TableType of
    tbNone: ;
    tbProjectTeams: ;
    tbPermits:       GetPermitFilters;
    tbGazetteer:     GetGazetteerFilters;
    tbBotanicTaxa:   GetBotanicTaxaFilters;
    tbNests:         GetNestFilters;
    tbNestRevisions: GetNestRevisionFilters;
    tbEggs:          GetEggFilters;
    tbNetStations:   GetNetStationFilters;
    tbTaxonRanks:    GetTaxonRankFilters;
    tbZooTaxa:       GetZooTaxaFilters;
    tbProjects:      GetProjectFilters;
    tbInstitutions:  GetInstitutionFilters;
    tbPeople:        GetPeopleFilters;
    tbExpeditions:   GetExpeditionFilters;
    tbSurveys:       GetSurveyFilters;
    tbMethods:       GetMethodFilters;
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbSightings:     GetSightingFilters;
    tbSpecimens:     GetSpecimenFilters;
    tbSamplePreps: ;
    tbPermanentNets: ;
    tbBands:         GetBandFilters;
    tbIndividuals:   GetIndividualFilters;
    tbCaptures:      GetCaptureFilters;
    tbMolts: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  Filtrado := FSearch.QuickFilters.Count > 0;
  CanToggle := True;
end;

procedure TfrmCustomGrid.GetGazetteerFilters;
const
  SiteRanks: array of String = ('P', 'E', 'R', 'M', 'D', 'L');
var
  sf: Integer;
begin
  if cbSiteRankFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('site_rank', 'Site rank', sdtText,
      crEqual, False, SiteRanks[cbSiteRankFilter.ItemIndex - 1]));
  end;
end;

procedure TfrmCustomGrid.GetIndividualFilters;
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('individual_sex', 'Sex', sdtText,
      crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
  end;
  if cbAgeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('individual_age', 'Age', sdtText,
      crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
  end;

  if tsfWithColorBandsFilter.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('right_leg_below', 'Right tarsus', sdtText,
      crNotEqual, False, ''));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('left_leg_below', 'Left tarsus', sdtText,
      crNotEqual, False, ''));
  end;
  if tsfWithRecapturesFilter.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('captures_tally', 'Captures', sdtInteger,
      crMoreThan, True, '2'));
  end;

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_id', 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('father_id', 'Father', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('mother_id', 'Mother', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if rbReplacedBandYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('removed_band_id', 'Removed band', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbReplacedBandNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('removed_band_id', 'Removed band', sdtInteger,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetInstitutionFilters;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetMethodFilters;
begin

end;

procedure TfrmCustomGrid.GetNestFilters;
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_fate', 'Nest fate', sdtText,
      crEqual, False, NestFate[cbNestFateFilter.ItemIndex - 1]));
  end;
  if cbNestSupportFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('support_type', 'Support type', sdtText,
      crEqual, False, NestSupport[cbNestSupportFilter.ItemIndex - 1]));
  end;

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('project_id', 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;

  if FPlantKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('support_plant_1_id', 'Support plant 1', sdtInteger,
      crEqual, False, IntToStr(FPlantKeyFilter)));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('support_plant_2_id', 'Support plant 2', sdtInteger,
      crEqual, False, IntToStr(FPlantKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetNestRevisionFilters;
const
  NestStatus: array of String = ('A', 'I', 'U');
  NestStages: array of String = ('C', 'L', 'I', 'H', 'N', 'X', 'U');
var
  sf: Integer;
begin
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbNestStatusFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_status', 'Nest status', sdtText,
      crEqual, False, NestStatus[cbNestStatusFilter.ItemIndex - 1]));
  end;
  if cbNestStageFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_stage', 'Nest stage', sdtText,
      crEqual, False, NestStages[cbNestStageFilter.ItemIndex - 1]));
  end;

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('revision_time', 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('revision_time', 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_id', 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;

  if rbNidoparasiteYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nidoparasite_id', 'Nidoparasite', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbNidoparasiteNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nidoparasite_id', 'Nidoparasite', sdtInteger,
      crEqual, False, '0'));
  end;

  if rbPhilornisYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('have_philornis_larvae', 'Philornis larvae', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbPhilornisNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('have_philornis_larvae', 'Philornis larvae', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetNetStationFilters;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetPeopleFilters;
var
  sf: Integer;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if FInstitutionKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('institution_id', 'Institution', sdtInteger,
      crEqual, False, IntToStr(FInstitutionKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetPermitFilters;
const
  PermitTypes: array of String = ('B', 'C', 'R', 'E', 'T', 'O');
var
  sf: Integer;
begin
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('project_id', 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;

  if cbPermitTypeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('permit_type', 'Permit type', sdtText,
      crEqual, False, PermitTypes[cbPermitTypeFilter.ItemIndex - 1]));
  end;
end;

procedure TfrmCustomGrid.GetProjectFilters;
begin
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
end;

procedure TfrmCustomGrid.GetSightingFilters;
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('sighting_time', 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('sighting_time', 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FSurveyKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('survey_id', 'Survey', sdtInteger,
      crEqual, False, IntToStr(FSurveyKeyFilter)));
  end;
  if FMethodKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('method_id', 'Method', sdtInteger,
      crEqual, False, IntToStr(FMethodKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('individual_id', 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if rbRecordInEbirdYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('ebird_available', 'Record is on eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbRecordInEbirdNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('ebird_available', 'Record is on eBird', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbOutOfSampleYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('not_surveying', 'Out of sample', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbOutOfSampleNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('not_surveying', 'Out of sample', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetSpecimenFilters;
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('sample_type', 'Sample type', sdtText,
      crEqual, False, SampleTypes[cbMaterialFilter.ItemIndex - 1]));
  end;

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('nest_id', 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;
  if FEggKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('egg_id', 'Egg', sdtInteger,
      crEqual, False, IntToStr(FEggKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('individual_id', 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetSurveyFilters;
var
  sf: Integer;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters);
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
    begin
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('start_time', 'Start time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)));
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('end_time', 'End time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)));
    end
    else
    begin
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('start_time', 'Start time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('end_time', 'End time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
    end;
  end;

  if FMethodKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('method_id', 'Method', sdtInteger,
      crEqual, False, IntToStr(FMethodKeyFilter)));
  end;

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('project_id', 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;

  if FExpeditionKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('expedition_id', 'Expedition', sdtInteger,
      crEqual, False, IntToStr(FExpeditionKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetTaxonRankFilters;
begin

end;

procedure TfrmCustomGrid.GetZooTaxaFilters;
var
  sf: Integer;
begin
  //if RanksFilter <> EmptyStr then
  //  aList.Add(RanksFilter);

  if tsTaxonomyClements.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyIoc.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyCbro.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
      crEqual, False, '1'));
  end;

  if tsTaxonExtinct.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;

  if tsIsSynonym.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crNotEqual, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;
end;

procedure TfrmCustomGrid.gridChild1DblClick(Sender: TObject);
begin
  if sbEditChild.Enabled then
    sbEditChildClick(Sender);
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

procedure TfrmCustomGrid.pChildTag1Click(Sender: TObject);
var
  aTag, aCountTag: TBCPanel;
begin
  aTag := nil;
  aCountTag := nil;

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
        0: FChildTable := tbNestOwners;
        1: FChildTable := tbNestRevisions;
        2: FChildTable := tbEggs;
      end;
    tbExpeditions:
      case nbChilds.PageIndex of
        0:
        begin
          FChildTable := tbSurveys;
          eAddChild.Visible := True;
          eAddChild.TextHint := rsHintAddExistingSurvey;
        end;
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
  aTag := nil;
  aCountTag := nil;

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

  UpdateChildBar;
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

procedure TfrmCustomGrid.pmcNewNestOwnerClick(Sender: TObject);
begin
  EditNestOwner(DMB.qNestOwners, dsLink.DataSet.FieldByName('nest_id').AsInteger, True);

  UpdateChildButtons(DMB.qNestOwners);
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

  UpdateChildBar;
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

procedure TfrmCustomGrid.pmtClearSelectionClick(Sender: TObject);
begin
  TBaseVirtualTree(pmTree.PopupComponent).ClearChecked;
end;

procedure TfrmCustomGrid.pmtColapseAllClick(Sender: TObject);
begin
  TBaseVirtualTree(pmTree.PopupComponent).FullCollapse();
end;

procedure TfrmCustomGrid.pmtExpandAllClick(Sender: TObject);
begin
  TBaseVirtualTree(pmTree.PopupComponent).FullExpand();
end;

procedure TfrmCustomGrid.pmtRefreshClick(Sender: TObject);
begin
  if pmTree.PopupComponent = tvTaxaFilter then
    LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);

  if pmTree.PopupComponent = tvSiteFilter then
    LoadSiteTreeData(FTableType, tvSiteFilter, 4);

  if pmTree.PopupComponent = tvDateFilter then
    LoadDateTreeData(FTableType, tvDateFilter);
end;

procedure TfrmCustomGrid.PrepareCanvasBands(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasCaptures(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasEggs(var Column: TColumn; var sender: TObject);
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
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasExpeditions(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasIndividuals(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasInstitutions(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasMolts(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasNests(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasNestRevisions(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasNetsEffort(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasPeople(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasPermanentNets(const Column: TColumn; const sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasPermits(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasProjects(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasSamplePreps(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.PrepareCanvasSightings(var Column: TColumn; var sender: TObject);
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
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasSpecimens(var Column: TColumn; var sender: TObject);
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
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasSurveys(var Column: TColumn; var sender: TObject);
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

procedure TfrmCustomGrid.sbChildHistoryClick(Sender: TObject);
var
  aKeyField: String;
  aDataSet: TDataSet;
begin
  aDataSet := nil;

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

procedure TfrmCustomGrid.sbClearFiltersClick(Sender: TObject);
begin
  ClearSearch;
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
        0: DeleteRecord(tbNestOwners, DMB.qNestOwners);
        1: DeleteRecord(tbNestRevisions, DMB.qNestRevisions);
        2: DeleteRecord(tbEggs, DMB.qEggs);
      end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    tbExpeditions:
      case nbChilds.PageIndex of
        0: DeleteRecord(tbSurveys, DMS.qSurveys);
      end;
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

  UpdateChildBar;
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
        0: EditNestOwner(DMB.qNestOwners, dsLink.DataSet.FieldByName('nest_id').AsInteger);
        1: EditNestRevision(DMB.qNestRevisions, dsLink.DataSet.FieldByName('nest_id').AsInteger);
        2: EditEgg(DMB.qEggs, dsLink.DataSet.FieldByName('nest_id').AsInteger);
      end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    tbExpeditions:
      case nbChilds.PageIndex of
        0: EditSurvey(DMS.qSurveys);
      end;
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

  UpdateChildBar;
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
    tbGazetteer:     EditSite(dsLink.DataSet);
    tbNetStations:   EditNetStation(dsLink.DataSet);
    //tbPermanentNets: ;
    tbInstitutions:  EditInstitution(dsLink.DataSet);
    tbPeople:        EditPerson(dsLink.DataSet);
    tbProjects:      EditProject(dsLink.DataSet);
    //tbProjectTeams: ;
    tbPermits:       EditPermit(dsLink.DataSet);
    tbTaxonRanks: ;
    //tbZooTaxa: ;
    tbBotanicTaxa:   EditBotanicTaxon(dsLink.DataSet);
    tbBands:         EditBand(dsLink.DataSet);
    //tbBandHistory: ;
    tbIndividuals:   EditIndividual(dsLink.DataSet);
    tbCaptures:      EditCapture(dsLink.DataSet);
    tbMolts:         EditMolt(dsLink.DataSet);
    tbNests:         EditNest(dsLink.DataSet);
    tbNestOwners:    EditNestOwner(dsLink.DataSet);
    tbNestRevisions: EditNestRevision(dsLink.DataSet);
    tbEggs:          EditEgg(dsLink.DataSet);
    tbMethods:       EditMethod(dsLink.DataSet);
    tbExpeditions:   EditExpedition(dsLink.DataSet);
    tbSurveys:       EditSurvey(dsLink.DataSet);
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings:     EditSighting(dsLink.DataSet);
    tbSpecimens:     EditSpecimen(dsLink.DataSet);
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;
  Working := False;
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
    tbGazetteer:     EditSite(dsLink.DataSet, True);
    tbNetStations:   EditNetStation(dsLink.DataSet, True);
    //tbPermanentNets: ;
    tbInstitutions:  EditInstitution(dsLink.DataSet, True);
    tbPeople:        EditPerson(dsLink.DataSet, True);
    tbProjects:      EditProject(dsLink.DataSet, True);
    //tbProjectTeams: ;
    tbPermits:       EditPermit(dsLink.DataSet, 0, True);
    tbTaxonRanks: ;
    //tbZooTaxa: ;
    tbBotanicTaxa:   EditBotanicTaxon(dsLink.DataSet, True);
    tbBands:         EditBand(dsLink.DataSet, True);
    //tbBandHistory: ;
    tbIndividuals:   EditIndividual(dsLink.DataSet, True);
    tbCaptures:      EditCapture(dsLink.DataSet, 0, True);
    tbMolts:         EditMolt(dsLink.DataSet, 0, True);
    tbNests:         EditNest(dsLink.DataSet, True);
    tbNestOwners:    EditNestOwner(dsLink.DataSet, 0, True);
    tbNestRevisions: EditNestRevision(dsLink.DataSet, 0, True);
    tbEggs:          EditEgg(dsLink.DataSet, 0, True);
    tbMethods:       EditMethod(dsLink.DataSet, True);
    tbExpeditions:   EditExpedition(dsLink.DataSet, True);
    tbSurveys:       EditSurvey(dsLink.DataSet, True);
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings:     EditSighting(dsLink.DataSet, 0, True);
    tbSpecimens:     EditSpecimen(dsLink.DataSet, True);
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

procedure TfrmCustomGrid.sbRefreshChildClick(Sender: TObject);
var
  DS: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  case nbChilds.PageIndex of
    0: DS := dsLink1.DataSet;
    1: DS := dsLink2.DataSet;
    2: DS := dsLink3.DataSet;
    3: DS := dsLink4.DataSet;
    4: DS := dsLink5.DataSet;
  end;
  if not DS.Active then
    DS.Open;
  DS.Refresh;
  UpdateChildButtons(DS);
  Working := False;
end;

procedure TfrmCustomGrid.sbRefreshRecordsClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  if not dsLink.DataSet.Active then
    dsLink.DataSet.Open;
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

procedure TfrmCustomGrid.sbShareRecordsClick(Sender: TObject);
begin
  ExportDlg(dsLink.DataSet);
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
    tbPermits:       Result := SearchPermits(aValue);
    tbGazetteer:     Result := SearchGazetteer(aValue);
    tbBotanicTaxa:   Result := SearchBotanicTaxa(aValue);
    tbNests:         Result := SearchNests(aValue);
    tbNestRevisions: Result := SearchNestRevisions(aValue);
    tbEggs:          Result := SearchEggs(aValue);
    tbNetStations:   Result := SearchNetStations(aValue);
    tbTaxonRanks:    Result := SearchTaxonRanks(aValue);
    tbZooTaxa:       Result := SearchZooTaxa(aValue);
    tbProjects:      Result := SearchProjects(aValue);
    tbInstitutions:  Result := SearchInstitutions(aValue);
    tbPeople:        Result := SearchPeople(aValue);
    tbExpeditions:   Result := SearchExpeditions(aValue);
    tbSurveys:       Result := SearchSurveys(aValue);
    tbMethods:       Result := SearchMethods(aValue);
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbSightings:     Result := SearchSightings(aValue);
    tbSpecimens:     Result := SearchSpecimens(aValue);
    tbSamplePreps: ;
    tbPermanentNets: ;
    tbBands:         Result := SearchBands(aValue);
    tbIndividuals:   Result := SearchIndividuals(aValue);
    tbCaptures:      Result := SearchCaptures(aValue);
    tbMolts: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;
  Working := False;

  UpdateButtons(dsLink.DataSet);
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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

  Result := FSearch.RunSearch > 0;
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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

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

  GetFilters;

  Result := FSearch.RunSearch > 0;
end;

procedure TfrmCustomGrid.SetColumnsBands(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('band_id').ReadOnly := True;

    ColumnByFieldName('band_size').PickList.AddCommaText('A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z');
    ColumnByFieldName('band_status').PickList.AddCommaText(rsBandStatusList);
    ColumnByFieldName('band_source').PickList.Add(rsBandAcquiredFromSupplier);
    ColumnByFieldName('band_source').PickList.Add(rsBandTransferBetweenBanders);
    ColumnByFieldName('band_source').PickList.Add(rsBandLivingBirdBandedByOthers);
    ColumnByFieldName('band_source').PickList.Add(rsBandDeadBirdBandedByOthers);
    ColumnByFieldName('band_source').PickList.Add(rsBandFoundLoose);
    ColumnByFieldName('band_type').PickList.AddCommaText(rsBandTypeList);

    ColumnByFieldName('supplier_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('carrier_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('project_name').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsBotanicTaxa(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('taxon_id').ReadOnly := True;

    ColumnByFieldname('parent_taxon_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('valid_name').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsCaptures(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName('capture_id').Visible then
      ColumnByFieldname('capture_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('capture_date').Visible then
      ColumnByFieldName('capture_date').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('capture_type').Visible then
      ColumnByFieldName('capture_type').PickList.AddCommaText(rsCaptureTypeList);
    if DataSource.DataSet.FieldByName('right_leg_below').Visible then
      ColumnByFieldName('right_leg_below').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('left_leg_below').Visible then
      ColumnByFieldName('left_leg_below').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('right_leg_above').Visible then
      ColumnByFieldName('right_leg_above').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('left_leg_above').Visible then
      ColumnByFieldName('left_leg_above').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('longitude').Visible then
      ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('latitude').Visible then
      ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('molt_limits').Visible then
      ColumnByFieldName('molt_limits').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('cycle_code').Visible then
      ColumnByFieldName('cycle_code').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('how_aged').Visible then
      ColumnByFieldName('how_aged').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('how_sexed').Visible then
      ColumnByFieldName('how_sexed').ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName('cloacal_protuberance').Visible then
      ColumnByFieldName('cloacal_protuberance').PickList.AddCommaText('U,N,S,M,L');
    if DataSource.DataSet.FieldByName('brood_patch').Visible then
      ColumnByFieldName('brood_patch').PickList.AddCommaText('F,N,V,W,O');
    if DataSource.DataSet.FieldByName('fat').Visible then
      ColumnByFieldName('fat').PickList.AddCommaText('N,T,L,H,F,B,G,V');
    if DataSource.DataSet.FieldByName('body_molt').Visible then
      ColumnByFieldName('body_molt').PickList.AddCommaText('N,T,S,H,G,A,F');
    if DataSource.DataSet.FieldByName('flight_feathers_molt').Visible then
      ColumnByFieldName('flight_feathers_molt').PickList.AddCommaText('N,S,A');
    if DataSource.DataSet.FieldByName('flight_feathers_wear').Visible then
      ColumnByFieldName('flight_feathers_wear').PickList.AddCommaText('N,S,L,M,H,X');
    if DataSource.DataSet.FieldByName('skull_ossification').Visible then
      ColumnByFieldName('skull_ossification').PickList.AddCommaText('N,T,L,H,G,A,F');
    if DataSource.DataSet.FieldByName('subject_age').Visible then
    begin
      ColumnByFieldName('subject_age').PickList.Add(rsAgeUnknown);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeAdult);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeImmature);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeFledgling);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeNestling);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeFirstYear);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeSecondYear);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeThirdYear);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeFourthYear);
      ColumnByFieldName('subject_age').PickList.Add(rsAgeFifthYear);
    end;
    if DataSource.DataSet.FieldByName('subject_sex').Visible then
    begin
      ColumnByFieldName('subject_sex').PickList.Add(rsSexMale);
      ColumnByFieldName('subject_sex').PickList.Add(rsSexFemale);
      ColumnByFieldName('subject_sex').PickList.Add(rsSexUnknown);
    end;
    if DataSource.DataSet.FieldByName('subject_status').Visible then
    begin
      ColumnByFieldName('subject_status').PickList.Add(rsStatusNormal);
      ColumnByFieldName('subject_status').PickList.Add(rsStatusInjured);
      ColumnByFieldName('subject_status').PickList.Add(rsStatusWingSprain);
      ColumnByFieldName('subject_status').PickList.Add(rsStatusStressed);
      ColumnByFieldName('subject_status').PickList.Add(rsStatusDead);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsEggs(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('egg_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('measure_date').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('individual_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('researcher_name').ButtonStyle := cbsEllipsis;

    ColumnByFieldName('egg_shape').PickList.Add(rsEggSpherical);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggElliptical);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggOval);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggPyriform);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggConical);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggBiconical);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggCylindrical);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggLongitudinal);
    ColumnByFieldName('egg_shape').PickList.Add(rsEggUnknown);

    ColumnByFieldName('eggshell_texture').PickList.Add(rsEggChalky);
    ColumnByFieldName('eggshell_texture').PickList.Add(rsEggShiny);
    ColumnByFieldName('eggshell_texture').PickList.Add(rsEggGlossy);
    ColumnByFieldName('eggshell_texture').PickList.Add(rsEggPitted);
    ColumnByFieldName('eggshell_texture').PickList.Add(rsEggUnknown);

    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggSpots);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggBlotches);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggSquiggles);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggStreaks);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggScrawls);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggSpotsSquiggles);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggBlotchesSquiggles);
    ColumnByFieldName('eggshell_pattern').PickList.Add(rsEggUnknown);
  end;
end;

procedure TfrmCustomGrid.SetColumnsExpeditions(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('expedition_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('start_date').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('end_date').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsGazetteer(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('site_id').ReadOnly := True;

    ColumnByFieldName('site_rank').PickList.Add(rsCaptionCountry);
    ColumnByFieldName('site_rank').PickList.Add(rsCaptionState);
    ColumnByFieldName('site_rank').PickList.Add(rsCaptionRegion);
    ColumnByFieldName('site_rank').PickList.Add(rsCaptionMunicipality);
    ColumnByFieldName('site_rank').PickList.Add(rsCaptionDistrict);
    ColumnByFieldName('site_rank').PickList.Add(rsCaptionLocality);

    if DataSource.DataSet.FieldByName('parent_site_name').Visible then
      ColumnByFieldname('parent_site_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsIndividuals(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName('individual_id').Visible then
      ColumnByFieldname('individual_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('band_full_name').Visible then
      ColumnByFieldName('band_full_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('double_band_name').Visible then
      ColumnByFieldName('double_band_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('removed_band_name').Visible then
      ColumnByFieldName('removed_band_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('banding_date').Visible then
      ColumnByFieldName('banding_date').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('band_change_date').Visible then
      ColumnByFieldName('band_change_date').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('nest_name').Visible then
      ColumnByFieldName('nest_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('father_name').Visible then
      ColumnByFieldName('father_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('mother_name').Visible then
      ColumnByFieldName('mother_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('right_leg_below').Visible then
      ColumnByFieldName('right_leg_below').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('left_leg_below').Visible then
      ColumnByFieldName('left_leg_below').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('right_leg_above').Visible then
      ColumnByFieldName('right_leg_above').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('left_leg_above').Visible then
      ColumnByFieldName('left_leg_above').ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName('individual_sex').Visible then
    begin
      ColumnByFieldName('individual_sex').PickList.Add(rsSexUnknown);
      ColumnByFieldName('individual_sex').PickList.Add(rsSexMale);
      ColumnByFieldName('individual_sex').PickList.Add(rsSexFemale);
    end;
    if DataSource.DataSet.FieldByName('individual_age').Visible then
    begin
      ColumnByFieldName('individual_age').PickList.Add(rsAgeUnknown);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeAdult);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeImmature);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeFledgling);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeNestling);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeFirstYear);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeSecondYear);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeThirdYear);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeFourthYear);
      ColumnByFieldName('individual_age').PickList.Add(rsAgeFifthYear);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsInstitutions(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('institution_id').ReadOnly := True;

    ColumnByFieldname('country_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('state_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('municipality_name').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsMethods(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('method_id').ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.SetColumnsNestOwners(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    //ColumnByFieldname('nest_owner_id').ReadOnly:= True;

    ColumnByFieldName('role').PickList.CommaText := rsNestOwnersRoleList;

    if DataSource.DataSet.FieldByName('individual_name').Visible then
      ColumnByFieldName('individual_name').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsNestRevisions(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('nest_revision_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('revision_date').Visible then
      ColumnByFieldName('revision_date').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsNests(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName('nest_id').Visible then
      ColumnByFieldname('nest_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('found_date').Visible then
      ColumnByFieldName('found_date').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('last_date').Visible then
      ColumnByFieldName('last_date').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;

    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapeScrape);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapeCup);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapePlate);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapeSphere);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapePendent);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapePlatform);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapeMound);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapeBurrow);
    ColumnByFieldname('nest_shape').PickList.Add(rsNestShapeCavity);

    ColumnByFieldname('support_type').PickList.Add(rsSupportGround);
    ColumnByFieldname('support_type').PickList.Add(rsSupportHerbBush);
    ColumnByFieldname('support_type').PickList.Add(rsSupportBranchFork);
    ColumnByFieldname('support_type').PickList.Add(rsSupportLeaves);
    ColumnByFieldname('support_type').PickList.Add(rsSupportLedge);
    ColumnByFieldname('support_type').PickList.Add(rsSupportRockCliff);
    ColumnByFieldname('support_type').PickList.Add(rsSupportRavine);
    ColumnByFieldname('support_type').PickList.Add(rsSupportNestBox);
    ColumnByFieldname('support_type').PickList.Add(rsSupportAnthropic);
    ColumnByFieldname('support_type').PickList.Add(rsSupportOther);
  end;
end;

procedure TfrmCustomGrid.SetColumnsNetStations(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('net_station_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsPeople(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('person_id').ReadOnly := True;

    ColumnByFieldName('gender').PickList.AddCommaText(rsGenderList);
    ColumnByFieldName('title_treatment').PickList.AddCommaText(rsTreatmentList);

    if DataSource.DataSet.FieldByName('institution_name').Visible then
      ColumnByFieldname('institution_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('country_name').Visible then
      ColumnByFieldname('country_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('state_name').Visible then
      ColumnByFieldname('state_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('municipality_name').Visible then
      ColumnByFieldname('municipality_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('birth_date').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('death_date').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsPermanentNets(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('permanent_net_id').ReadOnly := True;

    ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsPermits(var aGrid: TDBGrid);
begin
  with aGrid.Columns do
  begin
    ColumnByFieldname('permit_id').ReadOnly := True;

    ColumnByFieldName('project_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('dispatch_date').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('expire_date').ButtonStyle := cbsEllipsis;

    with ColumnByFieldName('permit_type').PickList do
    begin
      Clear;
      Add(rsPermitBanding);
      Add(rsPermitCollection);
      Add(rsPermitResearch);
      Add(rsPermitEntry);
      Add(rsPermitTransport);
      Add(rsPermitOther);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsProjects(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('project_id').ReadOnly := True;

    ColumnByFieldName('start_date').ButtonStyle := cbsEllipsis;
    ColumnByFieldName('end_date').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSightings(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName('sighting_id').Visible then
      ColumnByFieldname('sighting_id').ReadOnly := True;

    if DataSource.DataSet.FieldByName('survey_name').Visible then
      ColumnByFieldname('survey_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('observer_name').Visible then
      ColumnByFieldname('observer_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('individual_name').Visible then
      ColumnByFieldname('individual_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('detection_type').Visible then
      ColumnByFieldname('detection_type').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('breeding_status').Visible then
      ColumnByFieldname('breeding_status').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('sighting_date').Visible then
      ColumnByFieldname('sighting_date').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSpecimens(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName('specimen_id').Visible then
    begin
      ColumnByFieldname('specimen_id').ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSurveys(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('survey_id').ReadOnly := True;

    ColumnByFieldName('survey_date').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('start_longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('start_latitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('end_longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('end_latitude').ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsTaxonRanks(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('rank_id').ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.SetColumnsWeatherLogs(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    //  ColumnByFieldname('weather_id').ReadOnly:= True;

    ColumnByFieldname('sample_moment').PickList.Add(rsMomentStart);
    ColumnByFieldname('sample_moment').PickList.Add(rsMomentMiddle);
    ColumnByFieldname('sample_moment').PickList.Add(rsMomentEnd);

    ColumnByFieldname('precipitation').PickList.Add(rsPrecipitationNone);
    ColumnByFieldname('precipitation').PickList.Add(rsPrecipitationFog);
    ColumnByFieldname('precipitation').PickList.Add(rsPrecipitationMist);
    ColumnByFieldname('precipitation').PickList.Add(rsPrecipitationDrizzle);
    ColumnByFieldname('precipitation').PickList.Add(rsPrecipitationRain);
  end;
end;

procedure TfrmCustomGrid.SetFilters(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  Search(FSearchString);
end;

procedure TfrmCustomGrid.SetGridAndChild;
begin
  FChildTable := tbNone;

  case FTableType of
    tbInstitutions:   SetGridInstitutions;
    tbPeople:         SetGridPeople;
    tbProjects:       SetGridProjects;
    tbPermits:        SetGridPermits;
    tbGazetteer:      SetGridGazetteer;
    tbNetStations:    SetGridNetStations;
    tbTaxonRanks:     SetGridTaxonRanks;
    tbBotanicTaxa:    SetGridBotanicTaxa;
    tbZooTaxa: ;
    tbBands:          SetGridBands;
    tbIndividuals:    SetGridIndividuals;
    tbCaptures:       SetGridCaptures;
    tbNests:          SetGridNests;
    tbNestRevisions:  SetGridNestRevisions;
    tbEggs:           SetGridEggs;
    tbMethods:        SetGridMethods;
    tbExpeditions:    SetGridExpeditions;
    tbSurveys:        SetGridSurveys;
    tbSightings:      SetGridSightings;
    tbSpecimens:      SetGridSpecimens;
  end;
  FSearch.DataSet := TSQLQuery(dsLink.DataSet);

  SplitChild.Visible := pChild.Visible;
  Search(EmptyStr);
end;

procedure TfrmCustomGrid.SetGridBands;
begin
  Caption := rsTitleBands;
  AddSortedField('full_name', sdAscending);
  dsLink.DataSet := DMG.qBands;
end;

procedure TfrmCustomGrid.SetGridBotanicTaxa;
begin
  Caption := rsTitleBotanicTaxa;
  AddSortedField('taxon_name', sdAscending);
  dsLink.DataSet := DMG.qBotany;
end;

procedure TfrmCustomGrid.SetGridCaptures;
begin
  Caption := rsTitleCaptures;
  AddSortedField('capture_date', sdDescending);
  dsLink.DataSet := DMG.qCaptures;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridColumns(aTable: TTableType; aGrid: TDBGrid);
begin
  if aGrid.DataSource = nil then
    Exit;

  case aTable of
    tbNone: ;
    tbInstitutions:   SetColumnsInstitutions(aGrid);
    tbPeople:         SetColumnsPeople(aGrid);
    tbProjects:       SetColumnsProjects(aGrid);
    tbPermits:        SetColumnsPermits(aGrid);
    tbGazetteer:      SetColumnsGazetteer(aGrid);
    tbNetStations:    SetColumnsNetStations(aGrid);
    tbPermanentNets:  SetColumnsPermanentNets(aGrid);
    tbTaxonRanks:     SetColumnsTaxonRanks(aGrid);
    tbBotanicTaxa:    SetColumnsBotanicTaxa(aGrid);
    tbZooTaxa: ;
    tbBands:          SetColumnsBands(aGrid);
    tbIndividuals:    SetColumnsIndividuals(aGrid);
    tbCaptures:       SetColumnsCaptures(aGrid);
    tbNests:          SetColumnsNests(aGrid);
    tbNestOwners:     SetColumnsNestOwners(aGrid);
    tbNestRevisions:  SetColumnsNestRevisions(aGrid);
    tbEggs:           SetColumnsEggs(aGrid);
    tbMethods:        SetColumnsMethods(aGrid);
    tbExpeditions:    SetColumnsExpeditions(aGrid);
    tbSurveys:        SetColumnsSurveys(aGrid);
    tbWeatherLogs:    SetColumnsWeatherLogs(aGrid);
    tbSightings:      SetColumnsSightings(aGrid);
    tbSpecimens:      SetColumnsSpecimens(aGrid);
  end;

  //aGrid.OptionsExtra := aGrid.OptionsExtra - [dgeAutoColumns];

  {$IFDEF DEBUG}
  LogDebug(Format('%s: %d columns (%d visible)', [aGrid.Name, aGrid.Columns.Count, aGrid.Columns.VisibleCount]));
  {$ENDIF}
end;

procedure TfrmCustomGrid.SetGridEggs;
begin
  Caption := rsTitleEggs;
  AddSortedField('full_name', sdAscending);
  dsLink.DataSet := DMG.qEggs;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridExpeditions;
begin
  Caption := rsCaptionExpeditions;
  AddSortedField('start_date', sdDescending);
  dsLink.DataSet := DMG.qExpeditions;

  lblChildTag1.Caption := rsTitleSurveys;
  pChildTag1.Visible := True;
  nbChilds.PageIndex := 0;
  if not Assigned(DMS) then
    DMS := TDMS.Create(nil);
  FChildTable := tbSurveys;
  dsLink1.DataSet := DMS.qSurveys;
  pmcNewSurvey.Visible := True;

  pChildsBar.Visible := True;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridGazetteer;
begin
  Caption := rsTitleGazetteer;
  AddSortedField('site_name', sdAscending);
  dsLink.DataSet := DMG.qGazetteer;

  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridIndividuals;
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
    DMI := TDMI.Create(nil);
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

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridInstitutions;
begin
  Caption := rsTitleInstitutions;
  AddSortedField('full_name', sdAscending);
  dsLink.DataSet := DMG.qInstitutions;
end;

procedure TfrmCustomGrid.SetGridMethods;
begin
  Caption := rsTitleMethods;
  AddSortedField('method_name', sdAscending);
  dsLink.DataSet := DMG.qMethods;

  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNests;
begin
  Caption := rsTitleNests;
  AddSortedField('full_name', sdAscending);
  dsLink.DataSet := DMG.qNests;

  lblChildTag1.Caption := rsTitleNestOwners;
  lblChildTag2.Caption := rsTitleNestRevisions;
  lblChildTag3.Caption := rsTitleEggs;
  pChildTag1.Visible := True;
  pChildTag2.Visible := True;
  pChildTag3.Visible := True;
  nbChilds.PageIndex := 0;
  if not Assigned(DMB) then
    DMB := TDMB.Create(nil);
  FChildTable := tbNestOwners;
  dsLink1.DataSet := DMB.qNestOwners;
  dsLink2.DataSet := DMB.qNestRevisions;
  dsLink3.DataSet := DMB.qEggs;
  pmcNewNestOwner.Visible := True;
  pmcNewNestRevision.Visible := True;
  pmcNewEgg.Visible := True;

  pChildsBar.Visible := True;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNestRevisions;
begin
  Caption := rsTitleNestRevisions;
  AddSortedField('full_name', sdAscending);
  dsLink.DataSet := DMG.qNestRevisions;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNetStations;
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
end;

procedure TfrmCustomGrid.SetGridPeople;
begin
  Caption := rsTitleResearchers;
  AddSortedField('full_name', sdAscending);
  dsLink.DataSet := DMG.qPeople;
end;

procedure TfrmCustomGrid.SetGridPermits;
begin
  Caption := rsTitlePermits;
  AddSortedField('permit_name', sdAscending);
  dsLink.DataSet := DMG.qPermits;

  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridProjects;
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

  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSightings;
begin
  Caption := rsTitleSightings;
  AddSortedField('sighting_date', sdDescending);
  dsLink.DataSet := DMG.qSightings;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSpecimens;
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

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSurveys;
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
    DMS := TDMS.Create(nil);
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

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridTaxonRanks;
begin
  Caption := rsTitleTaxonRanks;
  AddSortedField('rank_seq', sdAscending);
  dsLink.DataSet := DMG.qTaxonRanks;
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

procedure TfrmCustomGrid.SplitChildMoved(Sender: TObject);
begin
  FChildPanelFactor := pChild.Height / (pClient.Height - SplitChild.Height);
end;

procedure TfrmCustomGrid.SplitRightMoved(Sender: TObject);
begin
  FSidePanelFactor := pSide.Width / (ClientWidth - SplitRight.Width);
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
    //FSearch.QuickFilters.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean, crEqual,
    //  False, '1'));

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
  //if tsfWithRecapturesFilter.StateOn = sw_on then
    //FSearch.QuickFilters.Add(TSearchField.Create('captures_tally', '# of captures', sdtInteger, crMoreThan,
    //  False, '1'));
end;

procedure TfrmCustomGrid.tvDateFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSiteNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Checked := Node^.CheckState = csCheckedNormal;

  if Sender.GetNodeLevel(Node) = 2 then
  begin
    if Sender.CheckState[Node] = csCheckedNormal then
    begin
      Sender.CheckState[Sender.NodeParent[Node]] := csMixedNormal;
      Sender.CheckState[Sender.NodeParent[Sender.NodeParent[Node]]] := csMixedNormal;
    end
    else
    begin
      Sender.CheckState[Sender.NodeParent[Node]] := csUncheckedNormal;
      Sender.CheckState[Sender.NodeParent[Sender.NodeParent[Node]]] := csUncheckedNormal;
    end;
  end
  else
  if Sender.GetNodeLevel(Node) = 1 then
  begin
    if Sender.CheckState[Node] = csCheckedNormal then
    begin
      Sender.CheckState[Sender.NodeParent[Node]] := csMixedNormal;
    end
    else
    begin
      Sender.CheckState[Sender.NodeParent[Node]] := csUncheckedNormal;
    end;
  end;

  if Sender.CheckedCount = 1 then
    lblCountDateFilter.Caption := Format(rsOneSelectedFemale, [Sender.CheckedCount])
  else
  if Sender.CheckedCount > 1 then
    lblCountDateFilter.Caption := Format(rsMoreSelectedFemale, [Sender.CheckedCount])
  else
    lblCountDateFilter.Caption := rsNoneSelectedFemale;

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

  if Sender.CheckedCount = 1 then
    lblCountSiteFilter.Caption := Format(rsOneSelected, [Sender.CheckedCount])
  else
  if Sender.CheckedCount > 1 then
    lblCountSiteFilter.Caption := Format(rsMoreSelected, [Sender.CheckedCount])
  else
    lblCountSiteFilter.Caption := rsNoneSelected;

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

  if Sender.CheckedCount = 1 then
    lblCountTaxonFilter.Caption := Format(rsOneSelected, [Sender.CheckedCount])
  else
  if Sender.CheckedCount > 1 then
    lblCountTaxonFilter.Caption := Format(rsMoreSelected, [Sender.CheckedCount])
  else
    lblCountTaxonFilter.Caption := rsNoneSelected;

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
  case aDataSet.State of
    dsInactive:
    begin
      sbInsertRecord.Enabled := False;
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

      sbCancelRecord.Visible := False;
      sbSaveRecord.Visible := False;

      sbRefreshRecords.Enabled := True;

      //navGrid.Enabled := False;
      //frmMain.navTabs.GetTabData((Self.Parent as TPage).PageIndex).TabModified := False;
      pSide.Enabled := False;
    end;
    dsBrowse:
    begin
      sbInsertRecord.Enabled := not (TSQLQuery(aDataSet).ReadOnly);
      sbEditRecord.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDelRecord.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbRecordHistory.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbSortRecords.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);

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

      sbRefreshRecords.Enabled := True;

      sbSaveRecord.Visible := False;
      sbCancelRecord.Visible := False;

      //navGrid.Enabled := True;
      //frmMain.navTabs.GetTabData((Self.Parent as TPage).PageIndex).TabModified := False;
      pSide.Enabled := True;
    end;
    dsEdit, dsInsert:
    begin
      sbInsertRecord.Enabled := False;
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

      sbRefreshRecords.Enabled := False;

      //navGrid.Enabled := False;
      //frmMain.navTabs.GetTabData((Self.Parent as TPage).PageIndex).TabModified := True;
      pSide.Enabled := False;
    end;

  end;
  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgInsert.Enabled := sbInsertRecord.Enabled;
  pmgEdit.Enabled := sbEditRecord.Enabled;
  pmgDel.Enabled := sbDelRecord.Enabled;
  pmgRecordHistory.Enabled := sbChildHistory.Enabled;

  sbClearFilters.Enabled := FSearch.QuickFilters.Count > 0;

  if dsLink.DataSet.RecordCount = 1 then
    lblRecordStatus.Caption := Format(rsRecordsFound, [dsLink.DataSet.RecordCount, rsRecords])
  else
  if dsLink.DataSet.RecordCount > 1 then
    lblRecordStatus.Caption := Format(rsRecordsFound, [dsLink.DataSet.RecordCount, rsRecordsPlural])
  else
    lblRecordStatus.Caption := rsNoRecordsFound;
end;

procedure TfrmCustomGrid.UpdateChildBar;
begin
  if pChildsBar.Visible then
    case nbChilds.PageIndex of
      0: UpdateChildButtons(dsLink1.DataSet);
      1: UpdateChildButtons(dsLink2.DataSet);
      2: UpdateChildButtons(dsLink3.DataSet);
      3: UpdateChildButtons(dsLink4.DataSet);
      4: UpdateChildButtons(dsLink5.DataSet);
    end;
end;

procedure TfrmCustomGrid.UpdateChildButtons(aDataSet: TDataSet);
begin
  if Closing then
    Exit;

  case aDataSet.State of
    dsInactive:
    begin
      sbAddChild.Enabled := False;
      sbEditChild.Enabled := False;
      sbDelChild.Enabled := False;
      sbFirstChild.Enabled := False;
      sbPriorChild.Enabled := False;
      sbNextChild.Enabled := False;
      sbLastChild.Enabled := False;
      sbChildHistory.Enabled := False;
      sbRefreshChild.Enabled := True;

      pSide.Enabled := False;
    end;
    dsBrowse:
    begin
      sbAddChild.Enabled := (dsLink.DataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbEditChild.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDelChild.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbChildHistory.Enabled := (aDataSet.RecordCount > 0);
      sbRefreshChild.Enabled := True;
      sbFirstChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
      sbPriorChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
      sbNextChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
      sbLastChild.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

      pSide.Enabled := True;
    end;
    dsEdit, dsInsert:
    begin
      sbAddChild.Enabled := False;
      sbEditChild.Enabled := False;
      sbDelChild.Enabled := False;
      sbFirstChild.Enabled := False;
      sbPriorChild.Enabled := False;
      sbNextChild.Enabled := False;
      sbLastChild.Enabled := False;
      sbChildHistory.Enabled := False;
      sbRefreshChild.Enabled := False;

      pSide.Enabled := False;
    end;
  end;
  eAddChild.Enabled := sbAddChild.Enabled;
  pmcEdit.Enabled := sbEditChild.Enabled;
  pmcDel.Enabled := sbDelChild.Enabled;
  pmcRefresh.Enabled := sbRefreshChild.Enabled;

  //UpdateChildCount;
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

      if dsLink3.DataSet.RecordCount > 0 then
      begin
        lblChildCount3.Caption := IntToStr(dsLink3.DataSet.RecordCount);
        pChildCount3.Visible := True;
      end
      else
        pChildCount3.Visible := False;
    end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    tbExpeditions:
    begin
      if dsLink1.DataSet.RecordCount > 0 then
      begin
        lblChildCount1.Caption := IntToStr(dsLink1.DataSet.RecordCount);
        pChildCount1.Visible := True;
      end
      else
        pChildCount1.Visible := False;
    end;
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
  DS := nil;
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
        2: DS := dsLink3;
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

procedure TfrmCustomGrid.UpdateGridTitles(aGrid: TDBGrid; aSearch: TCustomSearch);
const
  ArrowAsc: Integer = 3;
  ArrowDesc: Integer = 4;
var
  i: Integer;
begin
  if aGrid.Columns.Count = 0 then
    Exit;

  for i := 0 to aGrid.Columns.Count - 1 do
    aGrid.Columns[i].Title.ImageIndex := -1;

  if aSearch.SortFields.Count = 0 then
    Exit;

  for i := 0 to aSearch.SortFields.Count - 1 do
  begin
    case aSearch.SortFields[i].Direction of
      sdAscending: aGrid.Columns.ColumnByFieldname(aSearch.SortFields[i].FieldName).Title.ImageIndex := ArrowAsc;
      sdDescending: aGrid.Columns.ColumnByFieldname(aSearch.SortFields[i].FieldName).Title.ImageIndex := ArrowDesc;
    end;
  end;
end;

procedure TfrmCustomGrid.UpdateFilterPanels;
begin
  case TableType of
    tbGazetteer:      UpdateFilterPanelsGazetteer;
    tbInstitutions:   UpdateFilterPanelsInstitutions;
    tbPeople:         UpdateFilterPanelsPeople;
    tbProjects:       UpdateFilterPanelsProjects;
    tbPermits:        UpdateFilterPanelsPermits;
    tbNetStations:    UpdateFilterPanelsNetStations;
    tbBotanicTaxa:    UpdateFilterPanelsBotanicTaxa;
    tbZooTaxa:        UpdateFilterPanelsZooTaxa;
    tbBands:          UpdateFilterPanelsBands;
    tbIndividuals:    UpdateFilterPanelsIndividuals;
    tbCaptures:       UpdateFilterPanelsCaptures;
    tbNests:          UpdateFilterPanelsNests;
    tbNestRevisions:  UpdateFilterPanelsNestRevisions;
    tbEggs:           UpdateFilterPanelsEggs;
    tbExpeditions:    UpdateFilterPanelsExpeditions;
    tbSurveys:        UpdateFilterPanelsSurveys;
    tbSightings:      UpdateFilterPanelsSightings;
    tbSpecimens:      UpdateFilterPanelsSpecimens;
  end;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsBands;
begin
  pBandSizeFilter.Visible := True;
  cbBandStatusFilter.Items.Clear;
  cbBandStatusFilter.Items.Add(rsCaptionAll);
  cbBandStatusFilter.Items.AddCommaText(rsBandStatusList);
  pBandStatusFilter.Visible := True;
  cbBandTypeFilter.Items.Clear;
  cbBandTypeFilter.Items.Add(rsCaptionAll);
  cbBandTypeFilter.Items.AddCommaText(rsBandTypeList);
  cbBandSourceFilter.Items.Clear;
  cbBandSourceFilter.Items.CommaText := rsCaptionAll + ',"' + rsBandAcquiredFromSupplier + '","' +
    rsBandTransferBetweenBanders + '","' + rsBandLivingBirdBandedByOthers + '","' +
    rsBandDeadBirdBandedByOthers + '","' + rsBandFoundLoose + '"';
  pBandSourceFilter.Visible := True;
  pBandTypeFilter.Visible := True;
  pBandReportFilters.Visible := True;
  pPersonFilter.Visible := True;
  pInstitutionFilter.Visible := True;
  pProjectFilter.Visible := True;
  //pDatesFilters.Visible := True;
  //LoadDateTreeData(TableType, tvDateFilter);
end;

procedure TfrmCustomGrid.UpdateFilterPanelsBotanicTaxa;
begin
  pTaxonRanksFilters.Visible := True;
  LoadTaxaRanks(DMM.sqlCon, clbTaxonRanksFilter);
  pSynonymFilters.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsCaptures;
begin
  pTaxonFilters.Visible := True;
  LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  cbCaptureTypeFilter.Items.Clear;
  cbCaptureTypeFilter.Items.Add(rsCaptionAll);
  cbCaptureTypeFilter.Items.AddCommaText(rsCaptureTypeList);
  pCaptureTypeFilter.Visible := True;
  cbCaptureStatusFilter.Items.Clear;
  cbCaptureStatusFilter.Items.CommaText := rsCaptionAll + ',"' + rsStatusNormal + '","' + rsStatusInjured + '","' +
    rsStatusWingSprain + '","' + rsStatusStressed + '","' + rsStatusDead + '"';
  pCaptureStatusFilter.Visible := True;
  cbAgeFilter.Items.Clear;
  cbAgeFilter.Items.CommaText := rsCaptionAll + ',' + rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeImmature + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  pAgingFilters.Visible := True;
  cbSexFilter.Items.Clear;
  cbSexFilter.Items.CommaText := rsCaptionAll + ',' + rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  pSexingFilters.Visible := True;
  pFatFilter.Visible := True;
  pMoltingFilters.Visible := True;
  pPersonFilter.Visible := True;
  pTimeFilters.Visible := True;
  pSurveyFilter.Visible := True;
  pMethodFilter.Visible := True;
  pIndividualFilter.Visible := True;
  pSamplingPlotFilter.Visible := True;
  pNeedsReviewFilters.Visible := True;
  pEscapedFilters.Visible := True;
  pPhilornisFilter.Visible := True;
  pReplacedBandFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsEggs;
begin
  pTaxonFilters.Visible := True;
  LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  cbEggShapeFilter.Items.Clear;
  cbEggShapeFilter.Items.CommaText := rsCaptionAll + ',' + rsEggSpherical + ',' + rsEggElliptical + ',' + rsEggOval + ',' +
    rsEggPyriform + ',' + rsEggConical + ',' + rsEggBiconical + ',' + rsEggCylindrical + ',' +
    rsEggLongitudinal + ',' + rsEggUnknown;
  cbEggPatternFilter.Items.Clear;
  cbEggPatternFilter.Items.CommaText := rsCaptionAll + ',"' + rsEggSpots + '","' + rsEggBlotches + '","' +
    rsEggSquiggles + '","' + rsEggStreaks + '","' + rsEggScrawls + '","' + rsEggSpotsSquiggles + '","' +
    rsEggBlotchesSquiggles + '","' + rsEggUnknown + '"';
  cbEggTextureFilter.Items.Clear;
  cbEggTextureFilter.Items.CommaText := rsCaptionAll + ',' + rsEggChalky + ',' + rsEggShiny + ',' + rsEggGlossy + ',' +
    rsEggPitted + ',' + rsEggUnknown;
  pEggTraitsFilters.Visible := True;
  pPersonFilter.Visible := True;
  pNestFilter.Visible := True;
  pIndividualFilter.Visible := True;
  pHatchedFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsExpeditions;
begin
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pProjectFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsGazetteer;
begin
  cbSiteRankFilter.Items.Clear;
  cbSiteRankFilter.Items.Add(rsCaptionAll);
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

procedure TfrmCustomGrid.UpdateFilterPanelsIndividuals;
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
  pNestFilter.Visible := True;
  pIndividualFilter.Visible := True;
  pReplacedBandFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsInstitutions;
begin
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
end;

procedure TfrmCustomGrid.UpdateFilterPanelsNests;
begin
  pTaxonFilters.Visible := True;
  LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  cbNestFateFilter.Items.Clear;
  cbNestFateFilter.Items.CommaText := rsCaptionAll + ',"' + rsNestLost + '","' + rsNestSuccess + '","' +
    rsNestUnknown + '"';
  pNestFateFilter.Visible := True;
  cbNestSupportFilter.Items.Clear;
  cbNestSupportFilter.Items.CommaText := rsCaptionAll + ',"' + rsSupportGround + '","' +
    rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportLeaves + '","' +
    rsSupportLedge + '","' + rsSupportRockCliff + '","' + rsSupportRavine + '","' + rsSupportNestBox + '","' +
    rsSupportAnthropic + '","' + rsSupportOther + '"';
  pNestSupportFilter.Visible := True;
  pPersonFilter.Visible := True;
  pProjectFilter.Visible := True;
  pPlantFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsNestRevisions;
begin
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  cbNestStatusFilter.Items.Clear;
  cbNestStatusFilter.Items.CommaText := rsCaptionAll + ',' + rsNestActive + ',' + rsNestInactive + ',' + rsNestUnknown;
  pNestStatusFilter.Visible := True;
  cbNestStageFilter.Items.CommaText := rsCaptionAll + ',' + rsNestBuilding + ',' + rsNestLaying + ',' +
    rsNestIncubating + ',' + rsNestHatching + ',' + rsNestNestling + ',' + rsNestInactive + ',' +
    rsNestUnknown;
  pNestStageFilter.Visible := True;
  pPersonFilter.Visible := True;
  pTimeFilters.Visible := True;
  pNestFilter.Visible := True;
  pNidoparasiteFilter.Visible := True;
  pPhilornisFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsNetStations;
begin
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
end;

procedure TfrmCustomGrid.UpdateFilterPanelsPeople;
begin
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pInstitutionFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsPermits;
begin
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pProjectFilter.Visible := True;
  cbPermitTypeFilter.Items.Clear;
  cbPermitTypeFilter.Items.CommaText := rsCaptionAll + ',"' + rsPermitBanding + '","' + rsPermitCollection + '","' +
    rsPermitResearch + '","' + rsPermitEntry + '","' + rsPermitTransport + '","' + rsPermitOther + '"';
  pPermitTypeFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsProjects;
begin
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
end;

procedure TfrmCustomGrid.UpdateFilterPanelsSightings;
begin
  pTaxonFilters.Visible := True;
  LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  pPersonFilter.Visible := True;
  pTimeFilters.Visible := True;
  pSurveyFilter.Visible := True;
  pMethodFilter.Visible := True;
  pIndividualFilter.Visible := True;
  pRecordInEbirdFilter.Visible := True;
  pOutOfSampleFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsSpecimens;
begin
  cbMaterialFilter.Items.Clear;
  cbMaterialFilter.Items.Add(rsCaptionAll);
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
  pNestFilter.Visible := True;
  pEggFilter.Visible := True;
  pIndividualFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsSurveys;
begin
  pMethodFilter.Visible := True;
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  pTimeFilters.Visible := True;
  pProjectFilter.Visible := True;
  pSamplingPlotFilter.Visible := True;
  pExpeditionFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsZooTaxa;
begin
  pTaxonRanksFilters.Visible := True;
  LoadTaxaRanks(DMM.sqlCon, clbTaxonRanksFilter);
  pTaxonomiesFilters.Visible := True;
  pSynonymFilters.Visible := True;
  pExtinctFilter.Visible := True;
end;

end.

