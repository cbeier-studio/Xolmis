{ Xolmis Custom Grid form

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

unit ufrm_customgrid;

{$mode objfpc}{$H+}
// Disable hint "marked as inline is not inlined"
{$WARN 6058 OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StrUtils, RegExpr, DB, SQLDB, DateUtils, Grids, fgl,
  DBGrids, ExtCtrls, EditBtn, StdCtrls, ComCtrls, Menus, LCLIntf, LCLType, Character, Buttons, CheckLst,
  DBCtrls, laz.VirtualTrees, TAGraph, TASeries, TADbSource, LR_PGrid, atshapelinebgra, BCPanel, bctypes,
  DBControlGrid, data_types, data_filters, Types, ImgList, ToggleSwitch, DragDropFile, mvMapViewer, mvDE_BGRA,
  mvTypes, mvGpsObj, mvDrawingEngine, mvPluginCommon, mvMapScalePlugin, mvPlugins, LR_Class, DropTarget;

type
  { TStringMemoEditor }

  TStringMemoEditor = class(TMemo)
  private
    FGrid: TCustomGrid;
    FCol,FRow:Integer;
  protected
    procedure msgSetValue(var Msg: TGridMessage);   message GM_SETVALUE;
    procedure msgGetValue(var Msg: TGridMessage);   message GM_GETVALUE;
    procedure msgSetGrid(var Msg: TGridMessage);    message GM_SETGRID;
    procedure msgSetBounds(var Msg: TGridMessage);  message GM_SETBOUNDS;
    procedure msgSelectAll(var Msg: TGridMessage);  message GM_SELECTALL;
  end;

  TCustomPanelTab = class;

  { TfrmCustomGrid }

  TfrmCustomGrid = class(TForm)
    lblProjectBalance: TLabel;
    lblRubricBalance: TLabel;
    MvPluginManager: TMvPluginManager;
    MvPluginManagerLegalNoticePlugin1: TLegalNoticePlugin;
    MvPluginManagerMapScalePlugin1: TMapScalePlugin;
    pmpBandsBalance: TMenuItem;
    pmgBandHistory: TMenuItem;
    pmpBandHistory: TMenuItem;
    pmpTransferBandsTo: TMenuItem;
    pmMore: TPopupMenu;
    sbAddFeathersBatch: TSpeedButton;
    sbEmptyQuickEntry: TSpeedButton;
    sbQuickEntry: TSpeedButton;
    sbInsertBatch: TSpeedButton;
    sbQuickEntryChild: TSpeedButton;
    sbMoreOptions: TSpeedButton;
    sbEmptyNewRecord: TSpeedButton;
    sbEmptyImport: TSpeedButton;
    sbEmptyClearAll: TSpeedButton;
    TimerUpdate: TTimer;
    TimerOpen: TTimer;
    TimerChildUpdate: TTimer;
    txtProjectBalance: TLabel;
    pChildRightPanel: TBCPanel;
    DropAudios: TDropFileTarget;
    DropDocs: TDropFileTarget;
    DropImages: TDropFileTarget;
    dsDocs: TDataSource;
    dsLink6: TDataSource;
    gridChild6: TDBGrid;
    gridDocs: TDBGrid;
    pmdAddDocument: TMenuItem;
    pmdAddLink: TMenuItem;
    pmcNewFeather: TMenuItem;
    pmPrintFeathers: TMenuItem;
    pmcNewExpenseFromRubric: TMenuItem;
    pmcNewProjectActivityFromGoal: TMenuItem;
    pmcNewProjectGoal: TMenuItem;
    pmcNewChronogramActivity: TMenuItem;
    pmcNewBudgetItem: TMenuItem;
    pmcNewExpense: TMenuItem;
    pmrRefresh: TMenuItem;
    pmcNewVegetation: TMenuItem;
    pgChild6: TPage;
    pmPrintSightingsByObserver: TMenuItem;
    pmAddLink: TMenuItem;
    pmAddDocument: TMenuItem;
    pDocsToolbar: TBCPanel;
    pmdAddDoc: TMenuItem;
    pmdDocInfo: TMenuItem;
    pmdDelDoc: TMenuItem;
    pmdOpenDoc: TMenuItem;
    pmdRefreshDocs: TMenuItem;
    pmDocs: TPopupMenu;
    pmAddDocs: TPopupMenu;
    qDocsactive_status: TBooleanField;
    qDocscapture_id: TLongintField;
    qDocsdocument_date: TDateField;
    qDocsdocument_id: TLongintField;
    qDocsdocument_name: TStringField;
    qDocsdocument_path: TStringField;
    qDocsdocument_time: TTimeField;
    qDocsdocument_type: TStringField;
    qDocsexpedition_id: TLongintField;
    qDocsexported_status: TBooleanField;
    qDocsindividual_id: TLongintField;
    qDocsinsert_date: TDateTimeField;
    qDocslicense_notes: TStringField;
    qDocslicense_owner: TStringField;
    qDocslicense_type: TStringField;
    qDocslicense_uri: TStringField;
    qDocslicense_year: TLongintField;
    qDocsmarked_status: TBooleanField;
    qDocsmethod_id: TLongintField;
    qDocsnest_id: TLongintField;
    qDocsnet_station_id: TLongintField;
    qDocspermit_id: TLongintField;
    qDocsperson_id: TLongintField;
    qDocsproject_id: TLongintField;
    qDocssighting_id: TLongintField;
    qDocsspecimen_id: TLongintField;
    qDocssurvey_id: TLongintField;
    qDocsupdate_date: TDateTimeField;
    qDocsuser_inserted: TLongintField;
    qDocsuser_updated: TLongintField;
    qImagesfeather_id: TLongintField;
    sbAddDoc: TSpeedButton;
    sbClearAllFilters: TSpeedButton;
    sbShowChildSidePanel: TSpeedButton;
    sbDocInfo: TSpeedButton;
    sbDelDoc: TSpeedButton;
    sbOpenDoc: TSpeedButton;
    sbSaveRecord: TBitBtn;
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
    dsAudios: TDataSource;
    gridAudios: TDBGrid;
    dbImg: TDBImage;
    dsChart: TDataSource;
    dsImages: TDataSource;
    pAudiosToolbar: TBCPanel;
    pmaAddAudio: TMenuItem;
    pmaDelAudio: TMenuItem;
    pmaAudioInfo: TMenuItem;
    pmAudios: TPopupMenu;
    pmaRefreshAudios: TMenuItem;
    pmaPlayAudio: TMenuItem;
    pmPrintNestsByTaxon: TMenuItem;
    PrintGrid: TFrPrintGrid;
    gridSummary: TDBGrid;
    dsRecycle: TDataSource;
    DBG: TDBGrid;
    dbgRecycle: TDBControlGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoReportedFilter: TImage;
    icoEscapedFilter: TImage;
    icoNeedsReviewFilter: TImage;
    icoRecycleWarning: TImage;
    icoMarkedFilter: TImage;
    iHeadersDark: TImageList;
    iIcons: TImageList;
    iIconsDark: TImageList;
    lblImageID: TDBText;
    lblReportedFilter: TLabel;
    lblEscapedFilter: TLabel;
    lblNeedsReviewFilter: TLabel;
    lblRecycleWarning: TLabel;
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
    icoEggTextureFilter: TImage;
    icoEggShapeFilter: TImage;
    icoReplacedBandFilter: TImage;
    icoPermitTypeFilter: TImage;
    icoNestStatusFilter: TImage;
    icoBandTypeFilter: TImage;
    icoBandSourceFilter: TImage;
    icoCaptureTypeFilter: TImage;
    icoCaptureStatusFilter: TImage;
    icoEggFilter: TImage;
    icoNestStageFilter: TImage;
    icoNidoparasiteFilter: TImage;
    icoHatchedFilter: TImage;
    icoPhilornisFilter: TImage;
    icoRecordInEbirdFilter: TImage;
    icoNestFilter: TImage;
    icoIndividualFilter: TImage;
    icoExpeditionFilter: TImage;
    icoPlantFilter: TImage;
    icoOutOfSampleFilter: TImage;
    icoEggPatternFilter: TImage;
    icoSamplingPlotFilter: TImage;
    icoSurveyFilter: TImage;
    icoPersonFilter: TImage;
    icoInstitutionFilter: TImage;
    icoProjectFilter: TImage;
    iHeaders: TImageList;
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
    lblEmptyQuery: TLabel;
    lblNestFilter: TLabel;
    lblIndividualFilter: TLabel;
    lblExpeditionFilter: TLabel;
    lblPlantFilter: TLabel;
    lblOutOfSampleFilter: TLabel;
    lblEggPatternFilter: TLabel;
    lblMarkedFilter: TLabel;
    lblSamplingPlotFilter: TLabel;
    lblSurveyFilter: TLabel;
    lblPersonFilter: TLabel;
    lblInstitutionFilter: TLabel;
    lblRecordStatus: TLabel;
    lblChildStatus: TLabel;
    lblRecycleModifiedDate: TDBText;
    lblRecycleName: TDBText;
    lblProjectFilter: TLabel;
    cardMap: TPage;
    mapGeo: TMapView;
    pmPrintGrid: TMenuItem;
    pmPrintSightings: TMenuItem;
    pmPrintSightingsBySurvey: TMenuItem;
    pmPrintSightingsByLocality: TMenuItem;
    pmPrintSightingsByProject: TMenuItem;
    pmPrintSightingsByTaxon: TMenuItem;
    pmPrintSpecimens: TMenuItem;
    pmPrintSpecimensByProject: TMenuItem;
    pmPrintSpecimensByLocality: TMenuItem;
    pmPrintSpecimensByTaxon: TMenuItem;
    pmPrintSpecimensByYear: TMenuItem;
    pmPrintMethods: TMenuItem;
    pmPrintBands: TMenuItem;
    pmPrintBandsByCarrier: TMenuItem;
    pmPrintBandsWithHistory: TMenuItem;
    pmPrintBandsByStatus: TMenuItem;
    pmPrintIndividuals: TMenuItem;
    pmPrintIndividualsByTaxon: TMenuItem;
    pmPrintIndividualsByParents: TMenuItem;
    pmPrintCaptures: TMenuItem;
    pmPrintCapturesByProject: TMenuItem;
    pmPrintCapturesByDate: TMenuItem;
    pmPrintCapturesByLocality: TMenuItem;
    pmPrintCapturesByTaxon: TMenuItem;
    pmPrintColoredBands: TMenuItem;
    pmPrintNests: TMenuItem;
    pmPrintNestsByProject: TMenuItem;
    pmPrintNestsByLocality: TMenuItem;
    pmPrintNestsByPeriod: TMenuItem;
    pmPrintEggs: TMenuItem;
    pmPrintEggsByNest: TMenuItem;
    pmPrintExpeditionsByProject: TMenuItem;
    pmPrintEggsByLocality: TMenuItem;
    pmPrintEggsByTaxon: TMenuItem;
    pmPrintInstitutions: TMenuItem;
    pmPrintResearchers: TMenuItem;
    pmPrintProjects: TMenuItem;
    pmPrintPermits: TMenuItem;
    pmPrintPermitsByProject: TMenuItem;
    pmPrintPermitsByExpiration: TMenuItem;
    pmPrintGazetteer: TMenuItem;
    pmPrintGazetteerHierarchical: TMenuItem;
    pmPrintExpeditions: TMenuItem;
    pmPrintSamplingPlots: TMenuItem;
    pmPrintSamplingPlotsByLocality: TMenuItem;
    pmPrintBotanicTaxa: TMenuItem;
    pmPrintBotanicTaxaRecorded: TMenuItem;
    pmPrintBotanicTaxaHierarchical: TMenuItem;
    pmPrintTaxa: TMenuItem;
    pmPrintTaxaHierarchical: TMenuItem;
    pmPrintTaxaRecorded: TMenuItem;
    pmPrintTaxaRecordedByLocality: TMenuItem;
    pmPrintSurveysByExpedition: TMenuItem;
    pmPrintSurveys: TMenuItem;
    pmPrintSurveysByLocality: TMenuItem;
    pmPrintSurveysByProject: TMenuItem;
    pMapToolbar: TBCPanel;
    pmiAddImage: TMenuItem;
    pmiImageInfo: TMenuItem;
    pmiViewImage: TMenuItem;
    pmiDelImage: TMenuItem;
    pmiRefreshImages: TMenuItem;
    pmcColumnsAutoAdjustWidth: TMenuItem;
    pmcColumnSortAsc: TMenuItem;
    pmcColumnSortDesc: TMenuItem;
    pmcHideColumn: TMenuItem;
    MvBGRADraw: TMvBGRADrawingEngine;
    pmMarkColumns: TPopupMenu;
    pmmMarkAllColumns: TMenuItem;
    pmmUnmarkAllColumns: TMenuItem;
    pMsgSummary: TBCPanel;
    pmmMarkAll: TMenuItem;
    pmmUnmarkAll: TMenuItem;
    pmmInvertMarked: TMenuItem;
    pmcRecordVerifications: TMenuItem;
    pmgRecordVerifications: TMenuItem;
    pmgDefaultRowHeight: TMenuItem;
    pmgIncreaseRowHeight: TMenuItem;
    pmgRowHeight: TMenuItem;
    pmgDecreaseRowHeight: TMenuItem;
    pmgAutoSizeColumns: TMenuItem;
    pmcRecordHistory: TMenuItem;
    pmrRestoreRecord: TMenuItem;
    pmrDelPermanently: TMenuItem;
    pEggShapeFilter: TBCPanel;
    pEggTraitsFilters: TBCPanel;
    pmRecycle: TPopupMenu;
    pmMark: TPopupMenu;
    pmColumn: TPopupMenu;
    pmImages: TPopupMenu;
    pmPrint: TPopupMenu;
    pReportedFilter: TBCPanel;
    pEscapedFilter: TBCPanel;
    pNeedsReviewFilter: TBCPanel;
    pRecycleToolbar: TBCPanel;
    pRecycleWarning: TBCPanel;
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
    pmcNewNestOwner: TMenuItem;
    pEmptyQuery: TBCPanel;
    pmcDel: TMenuItem;
    pmcEdit: TMenuItem;
    pmcRefresh: TMenuItem;
    pmGridChild: TPopupMenu;
    pNestFilter: TBCPanel;
    pIndividualFilter: TBCPanel;
    pExpeditionFilter: TBCPanel;
    pPlantFilter: TBCPanel;
    pOutOfSampleFilter: TBCPanel;
    pEggPatternFilter: TBCPanel;
    pMarkedFilter: TBCPanel;
    pSamplingPlotFilter: TBCPanel;
    pSurveyFilter: TBCPanel;
    pPersonFilter: TBCPanel;
    pmcNewSurvey: TMenuItem;
    pmcNewCollector: TMenuItem;
    pmgInsert: TMenuItem;
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
    icoStartTimeFilter: TImage;
    icoMoltFilter: TImage;
    icoSynonymsFilter: TImage;
    icoTaxonomiesFilter: TImage;
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
    lblMoltLimitsFilter: TLabel;
    lblSkullOssificationFilter: TLabel;
    lblStartTimeFilter: TLabel;
    lblSynonymFilter: TLabel;
    lblTaxonomyCbroFilter: TLabel;
    lblTaxonomyIocFilter: TLabel;
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
    pmcNewCapture: TMenuItem;
    pmcNewMolt: TMenuItem;
    pmcNewNest: TMenuItem;
    pmcNewSighting: TMenuItem;
    pmcNewSpecimen: TMenuItem;
    pMoltCycleFilter: TBCPanel;
    pMoltLimitsFilter: TBCPanel;
    pInstitutionFilter: TBCPanel;
    pRecordStatus: TBCPanel;
    pChildStatus: TBCPanel;
    pRecordToolbar: TBCPanel;
    pColumnsToolbar: TBCPanel;
    pImagesToolbar: TBCPanel;
    pRecycleContent: TPanel;
    pSkullOssificationFilter: TBCPanel;
    pStartTimeFilter: TBCPanel;
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
    pWithColorBandsFilter: TBCPanel;
    pWithRecapturesFilter: TBCPanel;
    qAudiosactive_status: TBooleanField;
    qAudiosaudio_file: TBlobField;
    qAudiosaudio_id: TLongintField;
    qAudiosaudio_type: TStringField;
    qAudioscloud_cover: TLongintField;
    qAudiosdistance: TFloatField;
    qAudiosexported_status: TBooleanField;
    qAudiosfilter_model: TStringField;
    qAudiosfull_name: TStringField;
    qAudioshabitat: TStringField;
    qAudiosindividual_id: TLongintField;
    qAudiosindividual_name: TStringField;
    qAudiosinsert_date: TDateTimeField;
    qAudioslatitude: TFloatField;
    qAudioslicense_notes: TStringField;
    qAudioslicense_owner: TStringField;
    qAudioslicense_type: TStringField;
    qAudioslicense_uri: TStringField;
    qAudioslicense_year: TLongintField;
    qAudioslocality_id: TLongintField;
    qAudioslocality_name: TStringField;
    qAudioslongitude: TFloatField;
    qAudiosmarked_status: TBooleanField;
    qAudiosmic_model: TStringField;
    qAudiosnotes: TMemoField;
    qAudiosplayback_used: TBooleanField;
    qAudiosprecipitation: TStringField;
    qAudiosrecorder_id: TLongintField;
    qAudiosrecorder_model: TStringField;
    qAudiosrecorder_name: TStringField;
    qAudiosrecording_context: TStringField;
    qAudiosrecording_date: TDateField;
    qAudiosrecording_time: TTimeField;
    qAudiosrelative_humidity: TLongintField;
    qAudiossighting_id: TLongintField;
    qAudiossighting_name: TStringField;
    qAudiosspecimen_id: TLongintField;
    qAudiosspecimen_name: TStringField;
    qAudiossubjects_tally: TLongintField;
    qAudiossubtitle: TMemoField;
    qAudiostaxon_id: TLongintField;
    qAudiostaxon_name: TStringField;
    qAudiostemperature: TFloatField;
    qAudiosupdate_date: TDateTimeField;
    qAudiosuser_inserted: TLongintField;
    qAudiosuser_updated: TLongintField;
    qAudioswind_speed: TLongintField;
    qImagesactive_status: TBooleanField;
    qImagesauthor_id: TLongintField;
    qImagesauthor_name: TStringField;
    qImagescapture_id: TLongintField;
    qImagescapture_name: TStringField;
    qImagescoordinate_precision: TStringField;
    qImagesegg_id: TLongintField;
    qImagesegg_name: TStringField;
    qImagesexported_status: TBooleanField;
    qImagesimage_date: TDateField;
    qImagesimage_filename: TStringField;
    qImagesimage_id: TLongintField;
    qImagesimage_thumbnail: TBlobField;
    qImagesimage_time: TTimeField;
    qImagesimage_type: TStringField;
    qImagesindividual_id: TLongintField;
    qImagesindividual_name: TStringField;
    qImagesinsert_date: TDateTimeField;
    qImageslatitude: TFloatField;
    qImageslicense_notes: TStringField;
    qImageslicense_owner: TStringField;
    qImageslicense_type: TStringField;
    qImageslicense_uri: TStringField;
    qImageslicense_year: TLongintField;
    qImageslocality_id: TLongintField;
    qImageslocality_name: TStringField;
    qImageslongitude: TFloatField;
    qImagesmarked_status: TBooleanField;
    qImagesnest_id: TLongintField;
    qImagesnest_name: TStringField;
    qImagesnest_revision_id: TLongintField;
    qImagesrevision_name: TStringField;
    qImagessighting_id: TLongintField;
    qImagessighting_name: TStringField;
    qImagesspecimen_id: TLongintField;
    qImagesspecimen_name: TStringField;
    qImagessubtitle: TMemoField;
    qImagessurvey_id: TLongintField;
    qImagessurvey_name: TStringField;
    qImagestaxon_id: TLongintField;
    qImagestaxon_name: TStringField;
    qImagesupdate_date: TDateTimeField;
    qImagesuser_inserted: TLongintField;
    qImagesuser_updated: TLongintField;
    rbReportedAll: TRadioButton;
    rbEscapedAll: TRadioButton;
    rbNeedsReviewAll: TRadioButton;
    rbMarkedAll: TRadioButton;
    rbReportedNo: TRadioButton;
    rbEscapedNo: TRadioButton;
    rbNeedsReviewNo: TRadioButton;
    rbMarkedNo: TRadioButton;
    rbReportedYes: TRadioButton;
    rbEscapedYes: TRadioButton;
    rbNeedsReviewYes: TRadioButton;
    rbReplacedBandAll: TRadioButton;
    rbIsSynonymAll: TRadioButton;
    rbIsSynonymNo: TRadioButton;
    rbIsSynonymYes: TRadioButton;
    rbHasSynonymsAll: TRadioButton;
    rbHasSynonymsNo: TRadioButton;
    rbHasSynonymsYes: TRadioButton;
    rbExtinctAll: TRadioButton;
    rbExtinctNo: TRadioButton;
    rbExtinctYes: TRadioButton;
    rbMarkedYes: TRadioButton;
    rbWithRecapturesAll: TRadioButton;
    rbWithRecapturesNo: TRadioButton;
    rbWithRecapturesYes: TRadioButton;
    rbWithColorBandsAll: TRadioButton;
    rbReplacedBandNo: TRadioButton;
    rbWithColorBandsNo: TRadioButton;
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
    rbWithColorBandsYes: TRadioButton;
    sbAddChild: TSpeedButton;
    sbAddAudio: TSpeedButton;
    sbChildHistory: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbDelAudio: TSpeedButton;
    sbAudioInfo: TSpeedButton;
    sbCancelRecord: TBitBtn;
    sbShareMapPoints: TSpeedButton;
    sbDelChild: TSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditChild: TSpeedButton;
    sbAddNetsBatch: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbDelPermanently: TSpeedButton;
    sbMarkColumns: TSpeedButton;
    sbRecordHistory: TSpeedButton;
    sbRecordVerifications: TSpeedButton;
    sbChildVerifications: TSpeedButton;
    sbMarkRecords: TSpeedButton;
    sbRefreshChild: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbRestoreRecord: TSpeedButton;
    sbShareRecords: TSpeedButton;
    sbRowHeightDecrease: TSpeedButton;
    sbDelImage: TSpeedButton;
    sbRowHeightDefault: TSpeedButton;
    sbColumnWidthAutoAdjust: TSpeedButton;
    sbColumnHide: TSpeedButton;
    sbMoveColumnDown: TSpeedButton;
    sbMoveColumnUp: TSpeedButton;
    sbImageInfo: TSpeedButton;
    sbShareChild: TSpeedButton;
    sbPrint: TSpeedButton;
    sbShowMap: TSpeedButton;
    sbViewImage: TSpeedButton;
    sbFirstRecord: TSpeedButton;
    sbFirstChild: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbRowHeightIncrease: TSpeedButton;
    sbAddImage: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbLastChild: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sbNextChild: TSpeedButton;
    sbPriorRecord: TSpeedButton;
    sbPriorChild: TSpeedButton;
    sbShowRecord: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowAudio: TSpeedButton;
    sbShowDocs: TSpeedButton;
    sbShowColumns: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbShowSummary: TSpeedButton;
    sbPlayAudio: TSpeedButton;
    Separator10: TShapeLineBGRA;
    Separator11: TShapeLineBGRA;
    Separator12: TMenuItem;
    Separator13: TMenuItem;
    Separator14: TMenuItem;
    Separator15: TMenuItem;
    Separator16: TShapeLineBGRA;
    Separator17: TMenuItem;
    Separator18: TMenuItem;
    Separator19: TMenuItem;
    Separator20: TShapeLineBGRA;
    Separator22: TMenuItem;
    Separator23: TMenuItem;
    Separator24: TMenuItem;
    Separator25: TMenuItem;
    Separator26: TShapeLineBGRA;
    Separator27: TMenuItem;
    Separator28: TMenuItem;
    Separator29: TMenuItem;
    Separator30: TShapeLineBGRA;
    Separator31: TMenuItem;
    Separator32: TMenuItem;
    Separator33: TMenuItem;
    Separator34: TMenuItem;
    SeparatorPrint: TMenuItem;
    Separator5: TShapeLineBGRA;
    Separator6: TMenuItem;
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
    cpSide: TNotebook;
    pSide: TPanel;
    cardQuickFilters: TPage;
    cardViewRecord: TPage;
    scrollFilter: TScrollBox;
    gridColumns: TStringGrid;
    gridRecord: TStringGrid;
    qRecycle: TSQLQuery;
    qChart: TSQLQuery;
    qImages: TSQLQuery;
    qAudios: TSQLQuery;
    qDocs: TSQLQuery;
    TimerRecordUpdate: TTimer;
    titleViewRecord: TLabel;
    titleRecycle: TLabel;
    titleColumns: TLabel;
    titleSummary: TLabel;
    titleDocs: TLabel;
    titleAudios: TLabel;
    titleImages: TLabel;
    titleQuickFilters: TLabel;
    titleMap: TLabel;
    tsTaxonomyClements: TToggleSwitch;
    tsTaxonomyIoc: TToggleSwitch;
    tsTaxonomyCbro: TToggleSwitch;
    tvDateFilter: TLazVirtualStringTree;
    tvSiteFilter: TLazVirtualStringTree;
    tvTaxaFilter: TLazVirtualStringTree;
    txtRubricBalance: TLabel;
    procedure DBGColEnter(Sender: TObject);
    procedure DBGColExit(Sender: TObject);
    procedure DBGContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DBGDblClick(Sender: TObject);
    procedure DBGEditButtonClick(Sender: TObject);
    procedure DBGEditingDone(Sender: TObject);
    procedure dbgImagesDblClick(Sender: TObject);
    procedure DBGKeyPress(Sender: TObject; var Key: char);
    procedure DBGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DBGMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DBGMouseUp
      (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      );
    procedure DBGMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure DBGPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
      AState: TGridDrawState);
    procedure DBGSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure DropAudiosDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Longint);
    procedure DropDocsDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Longint);
    procedure DropImagesDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Longint);
    procedure dsAudiosDataChange(Sender: TObject; Field: TField);
    procedure dsAudiosStateChange(Sender: TObject);
    procedure dsDocsDataChange(Sender: TObject; Field: TField);
    procedure dsDocsStateChange(Sender: TObject);
    procedure dsImagesDataChange(Sender: TObject; Field: TField);
    procedure dsImagesStateChange(Sender: TObject);
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
    procedure dsLink6DataChange(Sender: TObject; Field: TField);
    procedure dsLink6StateChange(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure dsLinkStateChange(Sender: TObject);
    procedure dsRecycleStateChange(Sender: TObject);
    procedure eAddChildClick(Sender: TObject);
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
    procedure gridAudiosDblClick(Sender: TObject);
    procedure gridChild1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure gridChild1DblClick(Sender: TObject);
    procedure gridColumnsCheckboxToggled(Sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure gridDocsDblClick(Sender: TObject);
    procedure gridDocsDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure iHeadersGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer;
      var AResultWidth: Integer);
    procedure mapGeoDrawGpsPoint(Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure pClientResize(Sender: TObject);
    procedure pmAddDocumentClick(Sender: TObject);
    procedure pmAddLinkClick(Sender: TObject);
    procedure pmaRefreshAudiosClick(Sender: TObject);
    procedure pmcColumnSortAscClick(Sender: TObject);
    procedure pmcColumnSortDescClick(Sender: TObject);
    procedure pmcHideColumnClick(Sender: TObject);
    procedure pmcNewBudgetItemClick(Sender: TObject);
    procedure pmcNewCaptureClick(Sender: TObject);
    procedure pmcNewChronogramActivityClick(Sender: TObject);
    procedure pmcNewCollectorClick(Sender: TObject);
    procedure pmcNewEggClick(Sender: TObject);
    procedure pmcNewExpenseClick(Sender: TObject);
    procedure pmcNewExpenseFromRubricClick(Sender: TObject);
    procedure pmcNewFeatherClick(Sender: TObject);
    procedure pmcNewMistnetClick(Sender: TObject);
    procedure pmcNewNestClick(Sender: TObject);
    procedure pmcNewNestOwnerClick(Sender: TObject);
    procedure pmcNewNestRevisionClick(Sender: TObject);
    procedure pmcNewPermanentNetClick(Sender: TObject);
    procedure pmcNewProjectActivityFromGoalClick(Sender: TObject);
    procedure pmcNewProjectGoalClick(Sender: TObject);
    procedure pmcNewProjectMemberClick(Sender: TObject);
    procedure pmcNewSamplePrepClick(Sender: TObject);
    procedure pmcNewSightingClick(Sender: TObject);
    procedure pmcNewSpecimenClick(Sender: TObject);
    procedure pmcNewSurveyClick(Sender: TObject);
    procedure pmcNewSurveyMemberClick(Sender: TObject);
    procedure pmcNewVegetationClick(Sender: TObject);
    procedure pmcNewWeatherLogClick(Sender: TObject);
    procedure pmdRefreshDocsClick(Sender: TObject);
    procedure pmGridChildPopup(Sender: TObject);
    procedure pmiRefreshImagesClick(Sender: TObject);
    procedure pmmInvertMarkedClick(Sender: TObject);
    procedure pmmMarkAllClick(Sender: TObject);
    procedure pmmMarkAllColumnsClick(Sender: TObject);
    procedure pmmUnmarkAllClick(Sender: TObject);
    procedure pmmUnmarkAllColumnsClick(Sender: TObject);
    procedure pmpBandHistoryClick(Sender: TObject);
    procedure pmpBandsBalanceClick(Sender: TObject);
    procedure pmPrintBandsByCarrierClick(Sender: TObject);
    procedure pmPrintBandsByStatusClick(Sender: TObject);
    procedure pmPrintBandsClick(Sender: TObject);
    procedure pmPrintBandsWithHistoryClick(Sender: TObject);
    procedure pmPrintExpeditionsClick(Sender: TObject);
    procedure pmPrintGazetteerClick(Sender: TObject);
    procedure pmPrintGridClick(Sender: TObject);
    procedure pmPrintInstitutionsClick(Sender: TObject);
    procedure pmPrintMethodsClick(Sender: TObject);
    procedure pmPrintPermitsClick(Sender: TObject);
    procedure pmPrintProjectsClick(Sender: TObject);
    procedure pmPrintResearchersClick(Sender: TObject);
    procedure pmPrintSamplingPlotsByLocalityClick(Sender: TObject);
    procedure pmPrintSamplingPlotsClick(Sender: TObject);
    procedure pmPrintSightingsByObserverClick(Sender: TObject);
    procedure pmPrintSightingsClick(Sender: TObject);
    procedure pmPrintSpecimensClick(Sender: TObject);
    procedure pmPrintSurveysClick(Sender: TObject);
    procedure pmpTransferBandsToClick(Sender: TObject);
    procedure pmrRefreshClick(Sender: TObject);
    procedure pmtClearSelectionClick(Sender: TObject);
    procedure pmtColapseAllClick(Sender: TObject);
    procedure pmtExpandAllClick(Sender: TObject);
    procedure pmtRefreshClick(Sender: TObject);
    procedure qAudiosaudio_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qAudiosaudio_typeSetText(Sender: TField; const aText: string);
    procedure qAudiosBeforePost(DataSet: TDataSet);
    procedure qAudiosprecipitationGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qAudiosprecipitationSetText(Sender: TField; const aText: string);
    procedure qAudiosrecording_contextGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qAudiosrecording_contextSetText(Sender: TField; const aText: string);
    procedure qDocsBeforePost(DataSet: TDataSet);
    procedure qDocsdocument_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qDocsdocument_typeSetText(Sender: TField; const aText: string);
    procedure qImagesBeforePost(DataSet: TDataSet);
    procedure qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qImagesimage_typeSetText(Sender: TField; const aText: string);
    procedure sbAddAudioClick(Sender: TObject);
    procedure sbAddChildClick(Sender: TObject);
    procedure sbAddDocClick(Sender: TObject);
    procedure sbAddFeathersBatchClick(Sender: TObject);
    procedure sbAddImageClick(Sender: TObject);
    procedure sbAddNetsBatchClick(Sender: TObject);
    procedure sbAudioInfoClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbChildVerificationsClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure sbColumnHideClick(Sender: TObject);
    procedure sbColumnWidthAutoAdjustClick(Sender: TObject);
    procedure sbDelAudioClick(Sender: TObject);
    procedure sbDelChildClick(Sender: TObject);
    procedure sbDelDocClick(Sender: TObject);
    procedure sbDelImageClick(Sender: TObject);
    procedure sbDelPermanentlyClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbDocInfoClick(Sender: TObject);
    procedure sbEditChildClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbEmptyClearAllClick(Sender: TObject);
    procedure sbFirstChildClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbImageInfoClick(Sender: TObject);
    procedure sbInsertBatchClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastChildClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbMarkColumnsClick(Sender: TObject);
    procedure sbMarkRecordsClick(Sender: TObject);
    procedure sbMoreOptionsClick(Sender: TObject);
    procedure sbMoveColumnDownClick(Sender: TObject);
    procedure sbMoveColumnUpClick(Sender: TObject);
    procedure sbNextChildClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbOpenDocClick(Sender: TObject);
    procedure sbPlayAudioClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbPriorChildClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbQuickEntryChildClick(Sender: TObject);
    procedure sbQuickEntryClick(Sender: TObject);
    procedure sbRecordHistoryClick(Sender: TObject);
    procedure sbChildHistoryClick(Sender: TObject);
    procedure sbRecordVerificationsClick(Sender: TObject);
    procedure sbRefreshChildClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbRestoreRecordClick(Sender: TObject);
    procedure sbRowHeightDecreaseClick(Sender: TObject);
    procedure sbRowHeightDefaultClick(Sender: TObject);
    procedure sbRowHeightIncreaseClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShareChildClick(Sender: TObject);
    procedure sbShareMapPointsClick(Sender: TObject);
    procedure sbShareRecordsClick(Sender: TObject);
    procedure sbShowChildSidePanelClick(Sender: TObject);
    procedure sbShowRecordClick(Sender: TObject);
    procedure sbViewImageClick(Sender: TObject);
    procedure SetFilters(Sender: TObject);
    procedure SplitChildMoved(Sender: TObject);
    procedure SplitRightMoved(Sender: TObject);
    procedure TimerChildUpdateTimer(Sender: TObject);
    procedure TimerOpenTimer(Sender: TObject);
    procedure TimerRecordUpdateTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
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
    isFiltered: Boolean;
    FSidePanel, oldSidePanel: Boolean;
    FSideIndex, oldSideIndex: Integer;
    FSearchString, oldSearchString: String;
    FPersonKeyFilter, FInstitutionKeyFilter, FSurveyKeyFilter, FMethodKeyFilter: Integer;
    FProjectKeyFilter, FNestKeyFilter, FIndividualKeyFilter, FExpeditionKeyFilter: Integer;
    FPlantKeyFilter, FSamplingPlotKeyFilter, FEggKeyFilter: Integer;
    FCanToggle: Boolean;
    FIsResizing: Boolean;
    FIsMoving: Boolean;
    FStartX: Integer;
    FStartY: Integer;
    FStartCol: Integer;
    FStartRow: Integer;
    FSidePanelFactor: Double;
    FChildPanelFactor: Double;
    FDragging: Boolean;
    cellMemo: TMemo;

    panelTabs: specialize TFPGList<TCustomPanelTab>;
    procedure CreatePanelTabs;

    procedure AddAudio(aDataSet: TDataSet; aFileName: String);
    procedure AddDocument(aDataSet: TDataSet; aFileName: String);
    procedure AddGridColumns(aTable: TTableType; aGrid: TDBGrid);
    procedure AddOrEditChild(const aTableType: TTableType; const isNew: Boolean);
    procedure AddSortedField(aFieldName: String; aDirection: TSortDirection; aCollation: String = '';
      isAnAlias: Boolean = False);
    procedure ApplyDarkMode;
    procedure CellKeyPress(Sender: TObject; var Key: Char);

    procedure ClearSearch;
    procedure ClearBandFilters;
    procedure ClearBotanicTaxaFilters;
    procedure ClearCaptureFilters;
    procedure ClearEggFilters;
    procedure ClearExpeditionFilters;
    procedure ClearFeatherFilters;
    procedure ClearGazetteerFilters;
    procedure ClearIndividualFilters;
    procedure ClearInstitutionFilters;
    procedure ClearMethodFilters;
    procedure ClearNestFilters;
    procedure ClearNestRevisionFilters;
    procedure ClearSamplingPlotFilters;
    procedure ClearPeopleFilters;
    procedure ClearPermitFilters;
    procedure ClearProjectFilters;
    procedure ClearSightingFilters;
    procedure ClearSpecimenFilters;
    procedure ClearSurveyFilters;
    procedure ClearTaxonRankFilters;
    procedure ClearZooTaxaFilters;

    function GetChildDataSet: TDataSet;

    procedure GetColumns;

    procedure GetFilters;
    procedure GetBandFilters;
    procedure GetBotanicTaxaFilters;
    procedure GetCaptureFilters;
    procedure GetEggFilters;
    procedure GetExpeditionFilters;
    procedure GetFeatherFilters;
    procedure GetGazetteerFilters;
    procedure GetIndividualFilters;
    procedure GetInstitutionFilters;
    procedure GetMethodFilters;
    procedure GetNestFilters;
    procedure GetNestRevisionFilters;
    procedure GetSamplingPlotFilters;
    procedure GetPeopleFilters;
    procedure GetPermitFilters;
    procedure GetProjectFilters;
    procedure GetSightingFilters;
    procedure GetSpecimenFilters;
    procedure GetSurveyFilters;
    procedure GetTaxonRankFilters;
    procedure GetZooTaxaFilters;

    procedure LoadColumnsConfig;
    procedure LoadColumnsConfigGrid;
    procedure LoadRecordColumns;
    procedure LoadRecordRow;

    procedure OpenAsync;
    procedure OpenExpeditionChilds;
    procedure OpenIndividualChilds;
    procedure OpenNestChilds;
    procedure OpenProjectChilds;
    procedure OpenSamplingPlotChilds;
    procedure OpenSpecimenChilds;
    procedure OpenSurveyChilds;

    procedure PrepareCanvasBands(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasCaptures(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasEggs(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasExpeditions(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasFeathers(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasIndividuals(var Column: TColumn; var sender: TObject);
    procedure PrepareCanvasInstitutions(var Column: TColumn; var sender: TObject);
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

    procedure QuickAddChild(aInitialValue: String = '');

    procedure RefreshMap;
    procedure RefreshMapSurvey;

    procedure SaveColumnsConfig;

    procedure SetGridColumns(aTable: TTableType; aGrid: TDBGrid);
    procedure SetColumnsBands(var aGrid: TDBGrid);
    procedure SetColumnsBotanicTaxa(var aGrid: TDBGrid);
    procedure SetColumnsCaptures(var aGrid: TDBGrid);
    procedure SetColumnsEggs(var aGrid: TDBGrid);
    procedure SetColumnsExpeditions(var aGrid: TDBGrid);
    procedure SetColumnsFeathers(var aGrid: TDBGrid);
    procedure SetColumnsGazetteer(var aGrid: TDBGrid);
    procedure SetColumnsIndividuals(var aGrid: TDBGrid);
    procedure SetColumnsInstitutions(var aGrid: TDBGrid);
    procedure SetColumnsMethods(var aGrid: TDBGrid);
    procedure SetColumnsNestOwners(var aGrid: TDBGrid);
    procedure SetColumnsNestRevisions(var aGrid: TDBGrid);
    procedure SetColumnsNests(var aGrid: TDBGrid);
    procedure SetColumnsSamplingPlots(var aGrid: TDBGrid);
    procedure SetColumnsPeople(var aGrid: TDBGrid);
    procedure SetColumnsPermanentNets(var aGrid: TDBGrid);
    procedure SetColumnsPermits(var aGrid: TDBGrid);
    procedure SetColumnsProjects(var aGrid: TDBGrid);
    procedure SetColumnsSightings(var aGrid: TDBGrid);
    procedure SetColumnsSpecimens(var aGrid: TDBGrid);
    procedure SetColumnsSurveys(var aGrid: TDBGrid);
    procedure SetColumnsTaxonRanks(var aGrid: TDBGrid);
    procedure SetColumnsVegetation(var aGrid: TDBGrid);
    procedure SetColumnsWeatherLogs(var aGrid: TDBGrid);

    procedure SetGridAndChild;
    procedure SetGridBands;
    procedure SetGridBotanicTaxa;
    procedure SetGridCaptures;
    procedure SetGridEggs;
    procedure SetGridExpeditions;
    procedure SetGridFeathers;
    procedure SetGridGazetteer;
    procedure SetGridIndividuals;
    procedure SetGridInstitutions;
    procedure SetGridMethods;
    procedure SetGridNestRevisions;
    procedure SetGridNests;
    procedure SetGridPeople;
    procedure SetGridPermits;
    procedure SetGridProjects;
    procedure SetGridSamplingPlots;
    procedure SetGridSightings;
    procedure SetGridSpecimens;
    procedure SetGridSurveys;
    procedure SetGridTaxonRanks;

    procedure SetAudios;
    procedure SetDocs;
    procedure SetImages;
    procedure SetRecycle;
    procedure SetSidePanel(aValue: Boolean);
    procedure SetSideIndex(aValue: Integer);
    procedure SetSearchString(aValue: String);

    function Search(aValue: String): Boolean;
    function SearchBands(aValue: String): Boolean;
    function SearchBotanicTaxa(aValue: String): Boolean;
    function SearchCaptures(aValue: String): Boolean;
    function SearchEggs(aValue: String): Boolean;
    function SearchExpeditions(aValue: String): Boolean;
    function SearchFeathers(aValue: String): Boolean;
    function SearchGazetteer(aValue: String): Boolean;
    function SearchIndividuals(aValue: String): Boolean;
    function SearchInstitutions(aValue: String): Boolean;
    function SearchMethods(aValue: String): Boolean;
    function SearchNestRevisions(aValue: String): Boolean;
    function SearchNests(aValue: String): Boolean;
    function SearchSamplingPlots(aValue: String): Boolean;
    function SearchPeople(aValue: String): Boolean;
    function SearchPermits(aValue: String): Boolean;
    function SearchProjects(aValue: String): Boolean;
    function SearchSightings(aValue: String): Boolean;
    function SearchSpecimens(aValue: String): Boolean;
    function SearchSurveys(aValue: String): Boolean;
    function SearchTaxonRanks(aValue: String): Boolean;
    function SearchZooTaxa(aValue: String): Boolean;

    procedure Summary;

    procedure UpdateButtons(aDataSet: TDataSet);
    procedure UpdateChildBar;
    procedure UpdateChildButtons(aDataSet: TDataSet);
    procedure UpdateChildCount;
    procedure UpdateChildRightPanel;
    procedure UpdateChildStatus;
    procedure UpdateGridTitles(aGrid: TDBGrid; aSearch: TCustomSearch);
    procedure UpdateImageButtons(aDataSet: TDataSet);
    procedure UpdateAudioButtons(aDataSet: TDataSet);
    procedure UpdateDocButtons(aDataSet: TDataSet);
    procedure UpdateRecycleButtons(aDataset: TDataSet);
    procedure UpdateRowHeights;

    procedure UpdateFilterPanels;
    procedure UpdateFilterPanelsBands;
    procedure UpdateFilterPanelsBotanicTaxa;
    procedure UpdateFilterPanelsCaptures;
    procedure UpdateFilterPanelsEggs;
    procedure UpdateFilterPanelsExpeditions;
    procedure UpdateFilterPanelsFeathers;
    procedure UpdateFilterPanelsGazetteer;
    procedure UpdateFilterPanelsIndividuals;
    procedure UpdateFilterPanelsInstitutions;
    procedure UpdateFilterPanelsNestRevisions;
    procedure UpdateFilterPanelsNests;
    procedure UpdateFilterPanelsSamplingPlots;
    procedure UpdateFilterPanelsPeople;
    procedure UpdateFilterPanelsPermits;
    procedure UpdateFilterPanelsProjects;
    procedure UpdateFilterPanelsSightings;
    procedure UpdateFilterPanelsSpecimens;
    procedure UpdateFilterPanelsSurveys;
    procedure UpdateFilterPanelsZooTaxa;
  public
    property TableType: TTableType read FTableType write FTableType;
    property ChildTable: TTableType read FChildTable write FChildTable;

    property SearchString: String read FSearchString write SetSearchString;
    property ShowSidePanel: Boolean read FSidePanel write SetSidePanel;
    property SidePanelIndex: Integer read FSideIndex write SetSideIndex;
  end;

  { TCustomPanelTab }

  TCustomPanelTab = class(TBCPanel)
  private
    FTitleLabel: TLabel;
    FCounterBadge: TBCPanel;
    FCounterLabel: TLabel;
    FPageIndex: Integer;
    FIsActive: Boolean;
    FParentForm: TfrmCustomGrid;
    procedure TabClick(Sender: TObject);
    procedure TabMouseEnter(Sender: TObject);
    procedure TabMouseLeave(Sender: TObject);
    procedure SetActive(Value: Boolean);
  public
    constructor Create(AOwner: TComponent; AParentForm: TfrmCustomGrid; ATitle: string; ACount: Integer; APageIndex: Integer); reintroduce;
    procedure UpdateCounter(ACount: Integer);
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property IsActive: Boolean read FIsActive write SetActive;
  end;

var
  frmCustomGrid: TfrmCustomGrid;

implementation

uses
  utils_locale, utils_global, utils_system, utils_themes, utils_editdialogs, utils_dialogs, utils_math,
  utils_finddialogs, utils_print, utils_validations, utils_gis,
  data_management, data_getvalue, data_columns, data_blobs, data_setparam, data_consts,
  models_taxonomy, models_users, models_record_types,
  udlg_loading, udlg_progress, udlg_exportpreview, udlg_bandsbalance,
  {$IFDEF DEBUG}utils_debug,{$ENDIF} uDarkStyleParams,
  udm_main, udm_grid, udm_individuals, udm_breeding, udm_sampling, udm_reports,
  ufrm_main, ufrm_quickentry, udlg_selectrecord, udlg_bandhistory,
  ubatch_neteffort, ubatch_feathers, ubatch_bands, ubatch_bandstransfer;

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

{ TCustomPanelTab }

constructor TCustomPanelTab.Create
  (AOwner: TComponent; AParentForm: TfrmCustomGrid; ATitle: string; ACount: Integer; APageIndex: Integer);
var
  Panel: TBCPanel;
  LeftBorder: Integer;
begin
  inherited Create(AOwner);

  FPageIndex := APageIndex;
  FIsActive := False;
  FParentForm := AParentForm;
  Panel := Owner as TBCPanel;
  LeftBorder := Panel.Width + (Panel.ComponentCount + 10);

  // BCPanel setting
  Self.Parent := TWinControl(AOwner);
  Self.Height := 50;
  Self.Width := 150;
  Self.Left := LeftBorder;
  Self.Align := alLeft;
  Self.BevelOuter := bvNone;
  Self.BevelInner := bvNone;
  Self.BorderBCStyle := bpsBorder;
  Self.Caption := '';
  Self.Rounding.RoundX := 4;
  Self.Rounding.RoundY := 4;
  Self.ChildSizing.HorizontalSpacing := 8;
  Self.ChildSizing.LeftRightSpacing := 16;
  Self.ChildSizing.TopBottomSpacing := 8;
  Self.OnClick := @TabClick;
  Self.OnMouseEnter := @TabMouseEnter;
  Self.OnMouseLeave := @TabMouseLeave;
  Self.Border.Style := bboSolid;
  if IsDarkModeEnabled then
  begin
    Self.Background.Color := clCardBGSecondaryDark;
    Self.Border.Color := clSolidBGTertiaryDark;
    Self.Color := clCardBGDefaultDark;
  end
  else
  begin
    Self.Border.Color := $00D1D1D1;
    Self.Background.Color := $00FAFAFA;
    Self.Color := $00F3F3F3;
  end;

  // Title label
  FTitleLabel := TLabel.Create(Self);
  FTitleLabel.Parent := Self;
  FTitleLabel.Caption := ATitle;
  FTitleLabel.Top := 5;
  FTitleLabel.Left := 5;
  //if IsDarkModeEnabled then
  //begin
  //  FTitleLabel.Font.Color := $00C75F5B;
  //end
  //else
  //begin
  //  FTitleLabel.Font.Color := clDefault;
  //end;
  FTitleLabel.AnchorVerticalCenterTo(Self);
  FTitleLabel.AnchorSide[akLeft].Side := asrLeft;
  FTitleLabel.AnchorSide[akLeft].Control := Self;
  FTitleLabel.OnClick := @TabClick;
  FTitleLabel.OnMouseEnter := @TabMouseEnter;
  FTitleLabel.OnMouseLeave := @TabMouseLeave;

  // Counter badge
  FCounterBadge := TBCPanel.Create(Self);
  FCounterBadge.Parent := Self;
  FCounterBadge.Height := 18;
  FCounterBadge.Width := 25;
  FCounterBadge.BevelOuter := bvNone;
  FCounterBadge.BevelInner := bvNone;
  FCounterBadge.BorderBCStyle := bpsBorder;
  FCounterBadge.Caption := '';
  FCounterBadge.Rounding.RoundX := 12;
  FCounterBadge.Rounding.RoundY := 12;
  FCounterBadge.OnClick := @TabClick;
  FCounterBadge.OnMouseEnter := @TabMouseEnter;
  FCounterBadge.OnMouseLeave := @TabMouseLeave;
  FCounterBadge.Border.Style := bboNone;
  if IsDarkModeEnabled then
  begin
    FCounterBadge.Border.Color := $00C75F5B;
    FCounterBadge.Background.Color := $00C75F5B;
  end
  else
  begin
    FCounterBadge.Border.Color := $00C75F5B;
    FCounterBadge.Background.Color := $00C75F5B;
  end;
  FCounterBadge.Color := Self.Background.Color;
  FCounterBadge.AnchorVerticalCenterTo(Self);
  FCounterBadge.AnchorSide[akLeft].Side := asrRight;
  FCounterBadge.AnchorSide[akLeft].Control := FTitleLabel;
  FCounterBadge.OnClick := @TabClick;
  FCounterBadge.OnMouseEnter := @TabMouseEnter;
  FCounterBadge.OnMouseLeave := @TabMouseLeave;

  // Counter label
  FCounterLabel := TLabel.Create(FCounterBadge);
  FCounterLabel.Parent := FCounterBadge;
  FCounterLabel.Caption := IntToStr(ACount);
  FCounterLabel.Top := 5;
  FCounterLabel.Left := 5;
  FCounterLabel.BorderSpacing.Left := 8;
  FCounterLabel.BorderSpacing.Right := 8;
  FCounterLabel.BorderSpacing.Top := 2;
  FCounterLabel.BorderSpacing.Bottom := 2;
  FCounterLabel.Font.Style := [fsBold];
  FCounterLabel.Font.Color := $00FAEBE8;
  FCounterLabel.AutoSize := True;
  FCounterLabel.Layout := tlCenter;
  FCounterLabel.Align := alClient;
  FCounterLabel.OnClick := @TabClick;
  FCounterLabel.OnMouseEnter := @TabMouseEnter;
  FCounterLabel.OnMouseLeave := @TabMouseLeave;

  FCounterBadge.AutoSize := True;
  Self.AutoSize := True;
end;

procedure TCustomPanelTab.SetActive(Value: Boolean);
begin
  if FIsActive <> Value then
  begin
    FIsActive := Value;
    if FIsActive then
    begin
      Self.Border.Color := clVioletFGLight;
      Self.Border.Width := 2;
    end
    else
    begin
      if IsDarkModeEnabled then
        Self.Border.Color := clSolidBGTertiaryDark
      else
        Self.Border.Color := $00D1D1D1;
      Self.Border.Width := 1;
    end;
  end;
end;

procedure TCustomPanelTab.TabClick(Sender: TObject);
var
  i: Integer;
  Panel: TBCPanel;
  PanelTab: TCustomPanelTab;
begin
  if isWorking then
    Exit;

  isWorking := True;
  Panel := Owner as TBCPanel;
  // Iterate all tabs and deactivate them
  for i := 0 to Panel.ComponentCount - 1 do
  begin
    if Panel.Components[i] is TCustomPanelTab then
    begin
      PanelTab := TCustomPanelTab(Panel.Components[i]);
      PanelTab.IsActive := False;
    end;
  end;

  if IsDarkModeEnabled then
    Self.Background.Color := clVioletBG1Dark
  else
    Self.Background.Color := clVioletBG1Light;
  FCounterBadge.Color := Self.Background.Color;
  if (FParentForm.pChild.Visible) and (Self.PageIndex = FParentForm.nbChilds.PageIndex) then
  begin
    // Hide child grid, if clicked then activate tab
    FParentForm.pChild.Visible := False;
    if IsDarkModeEnabled then
      Self.Border.Color := clSolidBGTertiaryDark
    else
      Self.Border.Color := $00D1D1D1;
    Self.Border.Width := 1;
  end
  else
  begin
    // Show child grid when there is no active tab
    FParentForm.nbChilds.PageIndex := Self.PageIndex;
    if not FParentForm.pChild.Visible then
      FParentForm.pChild.Visible := True;
    FParentForm.pChild.Top := FParentForm.pChildsBar.Top + FParentForm.pChildsBar.Height + 1;

    // Activate clicked tab
    Self.IsActive := True;
  end;
  //splitChild.Visible := pChild.Visible;
  if IsDarkModeEnabled then
    Self.Background.Color := clVioletBG1Dark
  else
    Self.Background.Color := $00E0C0C0;
  FCounterBadge.Color := Self.Background.Color;

  FParentForm.eAddChild.Visible := False;
  FParentForm.sbAddNetsBatch.Visible := False;
  FParentForm.sbAddFeathersBatch.Visible := False;
  FParentForm.sbShowChildSidePanel.Visible := False;
  FParentForm.pChildRightPanel.Visible := False;

  // Set the child table
  case FParentForm.TableType of
    tbIndividuals:
      case FParentForm.nbChilds.PageIndex of
        0: FParentForm.ChildTable := tbCaptures;
        1:
        begin
          FParentForm.ChildTable := tbFeathers;
          FParentForm.sbAddFeathersBatch.Visible := True;
        end;
        2: FParentForm.ChildTable := tbSightings;
        3: FParentForm.ChildTable := tbNests;
        4: FParentForm.ChildTable := tbSpecimens;
      end;
    tbNests:
      case FParentForm.nbChilds.PageIndex of
        0: FParentForm.ChildTable := tbNestOwners;
        1: FParentForm.ChildTable := tbNestRevisions;
        2: FParentForm.ChildTable := tbEggs;
      end;
    tbExpeditions:
      case FParentForm.nbChilds.PageIndex of
        0:
        begin
          FParentForm.ChildTable := tbSurveys;
          FParentForm.eAddChild.Visible := True;
          //FParentForm.eAddChild.TextHint := rsHintAddExistingSurvey;
        end;
      end;
    tbSurveys:
      case FParentForm.nbChilds.PageIndex of
        0:
        begin
          FParentForm.ChildTable := tbSurveyTeams;
          FParentForm.eAddChild.Visible := True;
        end;
        1:
        begin
          FParentForm.ChildTable := tbNetsEffort;
          FParentForm.sbAddNetsBatch.Visible := True;
        end;
        2: FParentForm.ChildTable := tbWeatherLogs;
        3: FParentForm.ChildTable := tbCaptures;
        4: FParentForm.ChildTable := tbSightings;
        5: FParentForm.ChildTable := tbVegetation;
      end;
    tbSpecimens:
      case FParentForm.nbChilds.PageIndex of
        0:
        begin
          FParentForm.ChildTable := tbSpecimenCollectors;
          FParentForm.eAddChild.Visible := True;
        end;
        1: FParentForm.ChildTable := tbSamplePreps;
      end;
    tbSamplingPlots:
      case FParentForm.nbChilds.PageIndex of
        0: FParentForm.ChildTable := tbPermanentNets;
      end;
    tbProjects:
      case FParentForm.nbChilds.PageIndex of
        0:
        begin
          FParentForm.ChildTable := tbProjectTeams;
          FParentForm.eAddChild.Visible := True;
        end;
        1: FParentForm.ChildTable := tbProjectGoals;
        2: FParentForm.ChildTable := tbProjectChronograms;
        3:
        begin
          FParentForm.ChildTable := tbProjectBudgets;
          FParentForm.sbShowChildSidePanel.Visible := True;
          FParentForm.pChildRightPanel.Visible := FParentForm.sbShowChildSidePanel.Down;
          FParentForm.UpdateChildRightPanel;
        end;
        4: FParentForm.ChildTable := tbProjectExpenses;
      end;
  end;

  case FParentForm.nbChilds.PageIndex of
    0: FParentForm.UpdateChildButtons(FParentForm.dsLink1.DataSet);
    1: FParentForm.UpdateChildButtons(FParentForm.dsLink2.DataSet);
    2: FParentForm.UpdateChildButtons(FParentForm.dsLink3.DataSet);
    3: FParentForm.UpdateChildButtons(FParentForm.dsLink4.DataSet);
    4: FParentForm.UpdateChildButtons(FParentForm.dsLink5.DataSet);
    5: FParentForm.UpdateChildButtons(FParentForm.dsLink6.DataSet);
  end;

  FParentForm.UpdateChildStatus;
  isWorking := False;
end;

procedure TCustomPanelTab.TabMouseEnter(Sender: TObject);
begin
  if IsDarkModeEnabled then
    Self.Background.Color := clVioletBG1Dark
  else
    Self.Background.Color := $00E0C0C0;
  FCounterBadge.Color := Self.Background.Color;
end;

procedure TCustomPanelTab.TabMouseLeave(Sender: TObject);
begin
  if IsDarkModeEnabled then
    Self.Background.Color := clCardBGSecondaryDark
  else
    Self.Background.Color := clCardBGSecondaryLight;
  FCounterBadge.Color := Self.Background.Color;
end;

procedure TCustomPanelTab.UpdateCounter(ACount: Integer);
begin
  if ACount > 0 then
  begin
    FCounterLabel.Caption := IntToStr(ACount);
    FCounterBadge.Visible := True;
  end
  else
  begin
    FCounterBadge.Visible := False;
  end;
end;

{ TfrmCustomGrid }

procedure TfrmCustomGrid.AddAudio(aDataSet: TDataSet; aFileName: String);
var
  relPath: String;
  SearchRec: TSearchRec;
  CreationDate: TDateTime;
begin
  if not (FileExists(aFileName)) then
  begin
    //LogError(Format(rsImageNotFound, [aFileName]));
    Exit;
  end;

  if FindFirst(aFileName, faAnyFile, SearchRec) = 0 then
  try
    CreationDate := FileDateToDateTime(SearchRec.Time);
  finally
    FindClose(SearchRec);
  end;

  relPath := ExtractRelativePath(xSettings.AudiosFolder, aFileName);

  with aDataset do
  begin
    // Check if the image is in the dataset
    if not RecordExists(tbAudioLibrary, COL_AUDIO_FILE, relPath) then
    begin
      Append;
      FieldByName(COL_AUDIO_FILE).AsString := relPath;
    end
    else
    begin
      Locate(COL_AUDIO_FILE, relPath, []);
      Edit;
    end;
    FieldByName(COL_RECORDING_DATE).AsDateTime := CreationDate;
    FieldByName(COL_RECORDING_TIME).AsDateTime := CreationDate;

    Post;
    TSQLQuery(aDataSet).ApplyUpdates;
  end;
end;

procedure TfrmCustomGrid.AddDocument(aDataSet: TDataSet; aFileName: String);
var
  relPath: String;
  SearchRec: TSearchRec;
  CreationDate: TDateTime;
begin
  if not (FileExists(aFileName)) then
  begin
    //LogError(Format(rsImageNotFound, [aFileName]));
    Exit;
  end;

  if FindFirst(aFileName, faAnyFile, SearchRec) = 0 then
  try
    CreationDate := FileDateToDateTime(SearchRec.Time);
  finally
    FindClose(SearchRec);
  end;

  { #todo : Get document type from file extension }

  relPath := ExtractRelativePath(xSettings.DocumentsFolder, aFileName);

  with aDataset do
  begin
    // Check if the document is in the dataset
    if not RecordExists(tbDocuments, COL_DOCUMENT_PATH, relPath) then
    begin
      Append;
      FieldByName(COL_DOCUMENT_PATH).AsString := relPath;
    end
    else
    begin
      Locate(COL_DOCUMENT_PATH, relPath, []);
      Edit;
    end;
    FieldByName(COL_DOCUMENT_DATE).AsDateTime := CreationDate;
    FieldByName(COL_DOCUMENT_TIME).AsDateTime := CreationDate;

    Post;
    TSQLQuery(aDataSet).ApplyUpdates;
  end;
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
    aGrid.Columns.Clear;

    for i := 0 to (aGrid.DataSource.DataSet.FieldCount-1) do
      // Add only fields set as visible
      if aGrid.DataSource.DataSet.Fields[i].Visible then
      begin
        C := aGrid.Columns.Add;
        C.FieldName := aGrid.DataSource.DataSet.Fields[i].FieldName;
        // Set all fields as read only, except marked_status
        C.ReadOnly := not (C.FieldName = COL_MARKED_STATUS);
        //C := nil;
      end;

    SetGridColumns(aTable, aGrid);
  finally
    //aGrid.Columns.LinkFields;
    aGrid.EndUpdate;
  end;
end;

procedure TfrmCustomGrid.AddOrEditChild(const aTableType: TTableType; const isNew: Boolean);
begin
  case aTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbSamplingPlots:
      case nbChilds.PageIndex of
        0: EditPermanentNet(DMG.qPermanentNets, dsLink.DataSet.FieldByName(COL_SAMPLING_PLOT_ID).AsInteger, isNew);
      end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects:
      case nbChilds.PageIndex of
        0: EditProjectMember(DMG.qProjectTeam, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, isNew);
        1: EditProjectGoal(DMG.qProjectGoals, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, isNew);
        2: EditProjectActivity(DMG.qProjectChronogram, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, 0, isNew);
        3: EditProjectRubric(DMG.qProjectBudget, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, isNew);
        4: EditProjectExpense(DMG.qProjectExpenses, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, 0, isNew);
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
        0: EditCapture(DMI.qCaptures, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, 0, isNew);
        1: EditFeather(DMI.qFeathers, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, 0, 0, isNew);
        2: EditSighting(DMI.qSightings, 0, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, isNew);
        3: EditNest(DMI.qNests, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, isNew);
        4: EditSpecimen(DMI.qSpecimens, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, isNew);
      end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
      case nbChilds.PageIndex of
        0: EditNestOwner(DMB.qNestOwners, dsLink.DataSet.FieldByName(COL_NEST_ID).AsInteger, isNew);
        1: EditNestRevision(DMB.qNestRevisions, dsLink.DataSet.FieldByName(COL_NEST_ID).AsInteger, isNew);
        2: EditEgg(DMB.qEggs, dsLink.DataSet.FieldByName(COL_NEST_ID).AsInteger, isNew);
      end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    tbExpeditions:
      case nbChilds.PageIndex of
        0: EditSurvey(DMS.qSurveys, dsLink.DataSet.FieldByName(COL_EXPEDITION_ID).AsInteger, isNew);
      end;
    tbSurveys:
      case nbChilds.PageIndex of
        0: EditSurveyMember(DMS.qSurveyTeam, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, isNew);
        1: EditNetEffort(DMS.qNetsEffort, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, isNew);
        2: EditWeatherLog(DMS.qWeatherLogs, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, isNew);
        3: EditCapture(DMS.qCaptures, 0, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, isNew);
        4: EditSighting(DMS.qSightings, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, 0, isNew);
        5: EditVegetation(DMS.qVegetation, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, isNew);
      end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
      case nbChilds.PageIndex of
        0: EditCollector(DMG.qSampleCollectors, dsLink.DataSet.FieldByName(COL_SPECIMEN_ID).AsInteger, isNew);
        1: EditSamplePrep(DMG.qSamplePreps, dsLink.DataSet.FieldByName(COL_SPECIMEN_ID).AsInteger, isNew);
      end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;
end;

procedure TfrmCustomGrid.AddSortedField(aFieldName: String; aDirection: TSortDirection;
  aCollation: String = ''; isAnAlias: Boolean = False);
var
  p, idx: Integer;
begin
  p := -1;

  for idx := 0 to (FSearch.SortFields.Count - 1) do
    if aFieldName = FSearch.SortFields[idx].FieldName then
    begin
      p := idx;
      Break;
    end;

  if p < 0 then
    p := FSearch.SortFields.Add(TSortedField.Create);

  FSearch.SortFields[p].FieldName := aFieldName;
  if Assigned(FSearch.DataSet) then
    // Define the sort type by field type
    case FSearch.DataSet.FieldByName(aFieldName).DataType of
      ftUnknown, ftGuid:
        FSearch.SortFields[p].SortType := stNone;
      ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo:
        FSearch.SortFields[p].SortType := stAlphanumeric;
      ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc, ftBytes, ftVarBytes:
        FSearch.SortFields[p].SortType := stNumeric;
      ftFloat, ftCurrency, ftBCD, ftFMTBcd:
        FSearch.SortFields[p].SortType := stNumeric;
      ftDate, ftTime, ftDateTime, ftTimeStamp:
        FSearch.SortFields[p].SortType := stDateTime;
      ftBoolean:
        FSearch.SortFields[p].SortType := stBoolean;
    else
      FSearch.SortFields[p].SortType := stNone;
    end;
  FSearch.SortFields[p].Direction := aDirection;
  FSearch.SortFields[p].Collation := aCollation;
  FSearch.SortFields[p].Lookup    := isAnAlias;

  UpdateGridTitles(DBG, FSearch);
end;

procedure TfrmCustomGrid.ApplyDarkMode;
begin
  // Set panels colors
  pClient.Color := clSolidBGBaseDark;
  pSide.Color := clSolidBGTertiaryDark;
  scrollFilter.Color := clSolidBGTertiaryDark;
  pQuickFiltersContent.Background.Color := clSolidBGTertiaryDark;
  pFiltersToolbar.Background.Color := clCardBGDefaultDark;
  pFiltersToolbar.Border.Color := clCardBGSecondaryDark;
  pImagesToolbar.Background.Color := clCardBGDefaultDark;
  pImagesToolbar.Border.Color := clCardBGSecondaryDark;
  pAudiosToolbar.Background.Color := clCardBGDefaultDark;
  pAudiosToolbar.Border.Color := clCardBGSecondaryDark;
  pDocsToolbar.Background.Color := clCardBGDefaultDark;
  pDocsToolbar.Border.Color := clCardBGSecondaryDark;
  pMapToolbar.Background.Color := clCardBGDefaultDark;
  pMapToolbar.Border.Color := clCardBGSecondaryDark;
  pColumnsToolbar.Background.Color := clCardBGDefaultDark;
  pColumnsToolbar.Border.Color := clCardBGSecondaryDark;
  pRecycleToolbar.Background.Color := clCardBGDefaultDark;
  pRecycleToolbar.Border.Color := clCardBGSecondaryDark;
  pRecycleWarning.Background.Color := clSystemAttentionBGDark;
  pRecycleWarning.Border.Color := clSystemAttentionFGDark;
  pMsgSummary.Background.Color := clCardBGDefaultDark;
  pMsgSummary.Border.Color := clCardBGSecondaryDark;
  pMsgSummary.FontEx.Color := clTextPrimaryDark;
  pMsgSummary.Color := gridSummary.Color;

  pEmptyQuery.Background.Color := clCardBGDefaultDark;
  pEmptyQuery.Border.Color := clCardBGSecondaryDark;
  pEmptyQuery.Color := DBG.Color;

  pRecordToolbar.Background.Color := clCardBGDefaultDark;
  pRecordToolbar.Border.Color := clCardBGSecondaryDark;
  pRecordStatus.Background.Color := clCardBGDefaultDark;
  pRecordStatus.Border.Color := clCardBGSecondaryDark;
  pChildToolbar.Background.Color := clCardBGDefaultDark;
  pChildToolbar.Border.Color := clCardBGSecondaryDark;
  pChildStatus.Background.Color := clCardBGDefaultDark;
  pChildStatus.Border.Color := clCardBGSecondaryDark;
  pChildsBar.Background.Color := clCardBGDefaultDark;
  pChildsBar.Border.Color := clCardBGSecondaryDark;
  pChildRightPanel.Background.Color := clSolidBGSecondaryDark;
  pChildRightPanel.Border.Color := clCardBGSecondaryDark;

  pSideToolbar.Color := clSolidBGQuaternaryDark;

  // Set images
  DBG.TitleImageList := iHeadersDark;
  pmGrid.Images := iButtonsDark;
  pmGridChild.Images := iButtonsDark;
  pmColumn.Images := iButtonsDark;
  pmPrint.Images := iButtonsDark;
  pmTree.Images := iButtonsDark;
  pmRecycle.Images := iButtonsDark;
  pmMark.Images := iButtonsDark;
  pmMarkColumns.Images := iButtonsDark;
  pmImages.Images := iButtonsDark;
  pmAudios.Images := iButtonsDark;
  pmDocs.Images := iButtonsDark;
  pmAddChild.Images := DMM.iAddMenuDark;
  pmMore.Images := iButtonsDark;
  icoRecycleWarning.Images := iIconsDark;
  // Set buttons images
  sbInsertRecord.Images := iButtonsDark;
  sbQuickEntry.Images := iButtonsDark;
  sbEditRecord.Images := iButtonsDark;
  sbRecordVerifications.Images := iButtonsDark;
  sbMarkRecords.Images := iButtonsDark;
  sbRecordHistory.Images := iButtonsDark;
  sbShareRecords.Images := iButtonsDark;
  sbPrint.Images := iButtonsDark;
  sbSaveRecord.Images := iButtonsDark;
  sbCancelRecord.Images := iButtonsDark;
  sbDelRecord.Images := iButtonsDark;
  sbRefreshRecords.Images := iButtonsDark;
  sbFirstRecord.Images := iButtonsDark;
  sbPriorRecord.Images := iButtonsDark;
  sbNextRecord.Images := iButtonsDark;
  sbLastRecord.Images := iButtonsDark;
  sbAddChild.Images := iButtonsDark;
  sbQuickEntryChild.Images := iButtonsDark;
  sbAddNetsBatch.Images := iButtonsDark;
  sbAddFeathersBatch.Images := iButtonsDark;
  sbEditChild.Images := iButtonsDark;
  sbChildHistory.Images := iButtonsDark;
  sbChildVerifications.Images := iButtonsDark;
  sbShareChild.Images := iButtonsDark;
  sbDelChild.Images := iButtonsDark;
  sbRefreshChild.Images := iButtonsDark;
  sbFirstChild.Images := iButtonsDark;
  sbPriorChild.Images := iButtonsDark;
  sbNextChild.Images := iButtonsDark;
  sbLastChild.Images := iButtonsDark;
  sbShowChildSidePanel.Images := iButtonsDark;
  sbShowRecord.Images := iButtonsDark;
  sbShowQuickFilters.Images := iButtonsDark;
  sbShowImages.Images := iButtonsDark;
  sbAddImage.Images := iButtonsDark;
  sbImageInfo.Images := iButtonsDark;
  sbViewImage.Images := iButtonsDark;
  sbDelImage.Images := iButtonsDark;
  sbShowAudio.Images := iButtonsDark;
  sbAddAudio.Images := iButtonsDark;
  sbAudioInfo.Images := iButtonsDark;
  sbPlayAudio.Images := iButtonsDark;
  sbDelAudio.Images := iButtonsDark;
  sbShowDocs.Images := iButtonsDark;
  sbAddDoc.Images := iButtonsDark;
  sbDocInfo.Images := iButtonsDark;
  sbOpenDoc.Images := iButtonsDark;
  sbDelDoc.Images := iButtonsDark;
  sbShowMap.Images := iButtonsDark;
  sbShareMapPoints.Images := iButtonsDark;
  sbShowSummary.Images := iButtonsDark;
  sbShowColumns.Images := iButtonsDark;
  sbRowHeightIncrease.Images := iButtonsDark;
  sbRowHeightDecrease.Images := iButtonsDark;
  sbRowHeightDefault.Images := iButtonsDark;
  sbColumnWidthAutoAdjust.Images := iButtonsDark;
  sbMoveColumnDown.Images := iButtonsDark;
  sbMoveColumnUp.Images := iButtonsDark;
  sbMarkColumns.Images := iButtonsDark;
  sbColumnHide.Images := iButtonsDark;
  sbShowRecycle.Images := iButtonsDark;
  sbClearFilters.Images := iButtonsDark;
  sbClearAllFilters.Images := iButtonsDark;
  sbRestoreRecord.Images := iButtonsDark;
  sbDelPermanently.Images := iButtonsDark;
  sbEmptyNewRecord.Images := iButtonsDark;
  sbEmptyQuickEntry.Images := iButtonsDark;
  sbEmptyImport.Images := iButtonsDark;
  sbEmptyClearAll.Images := iButtonsDark;
  sbMoreOptions.Images := iButtonsDark;

  // Set filter cards colors
  pSiteFilters.Background.Color := clCardBGDefaultDark;
  pSiteFilters.Border.Color := clSystemSolidNeutralFGDark;
  pTitleSiteFilter.Color := clCardBGDefaultDark;
  pSiteRankFilter.Background.Color := clCardBGDefaultDark;
  pSiteRankFilter.Border.Color := clSystemSolidNeutralFGDark;
  pTaxonRanksFilters.Background.Color := clCardBGDefaultDark;
  pTaxonRanksFilters.Border.Color := clSystemSolidNeutralFGDark;
  pTitleTaxonRanksFilter.Color := clCardBGDefaultDark;
  pMaterialFilter.Background.Color := clCardBGDefaultDark;
  pMaterialFilter.Border.Color := clSystemSolidNeutralFGDark;
  pMethodFilter.Background.Color := clCardBGDefaultDark;
  pMethodFilter.Border.Color := clSystemSolidNeutralFGDark;
  pBandSizeFilter.Background.Color := clCardBGDefaultDark;
  pBandSizeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pWithRecapturesFilter.Background.Color := clCardBGDefaultDark;
  pWithRecapturesFilter.Border.Color := clSystemSolidNeutralFGDark;
  pFatFilter.Background.Color := clCardBGDefaultDark;
  pFatFilter.Border.Color := clSystemSolidNeutralFGDark;
  pBandStatusFilter.Background.Color := clCardBGDefaultDark;
  pBandStatusFilter.Border.Color := clSystemSolidNeutralFGDark;
  pDatesFilters.Background.Color := clCardBGDefaultDark;
  pDatesFilters.Border.Color := clSystemSolidNeutralFGDark;
  pDateFilter.Color := clCardBGDefaultDark;
  pNestFateFilter.Background.Color := clCardBGDefaultDark;
  pNestFateFilter.Border.Color := clSystemSolidNeutralFGDark;
  pNestSupportFilter.Background.Color := clCardBGDefaultDark;
  pNestSupportFilter.Border.Color := clSystemSolidNeutralFGDark;
  pTaxonFilters.Background.Color := clCardBGDefaultDark;
  pTaxonFilters.Border.Color := clSystemSolidNeutralFGDark;
  pTitleTaxonFilter.Color := clCardBGDefaultDark;
  pExtinctFilter.Background.Color := clCardBGDefaultDark;
  pExtinctFilter.Border.Color := clSystemSolidNeutralFGDark;
  pWithColorBandsFilter.Background.Color := clCardBGDefaultDark;
  pWithColorBandsFilter.Border.Color := clSystemSolidNeutralFGDark;
  pSexFilter.Background.Color := clCardBGDefaultDark;
  pSexFilter.Border.Color := clSystemSolidNeutralFGDark;
  pCloacalProtuberanceFilter.Background.Color := clCardBGDefaultDark;
  pCloacalProtuberanceFilter.Border.Color := clSystemSolidNeutralFGDark;
  pBroodPatchFilter.Background.Color := clCardBGDefaultDark;
  pBroodPatchFilter.Border.Color := clSystemSolidNeutralFGDark;
  pHowSexedFilter.Background.Color := clCardBGDefaultDark;
  pHowSexedFilter.Border.Color := clSystemSolidNeutralFGDark;
  pReportedFilter.Background.Color := clCardBGDefaultDark;
  pReportedFilter.Border.Color := clSystemSolidNeutralFGDark;
  pIsSynonymFilter.Background.Color := clCardBGDefaultDark;
  pIsSynonymFilter.Border.Color := clSystemSolidNeutralFGDark;
  pHasSynonymsFilter.Background.Color := clCardBGDefaultDark;
  pHasSynonymsFilter.Border.Color := clSystemSolidNeutralFGDark;
  pBodyMoltFilter.Background.Color := clCardBGDefaultDark;
  pBodyMoltFilter.Border.Color := clSystemSolidNeutralFGDark;
  pFFMoltFilter.Background.Color := clCardBGDefaultDark;
  pFFMoltFilter.Border.Color := clSystemSolidNeutralFGDark;
  pFFWearFilter.Background.Color := clCardBGDefaultDark;
  pFFWearFilter.Border.Color := clSystemSolidNeutralFGDark;
  pMoltLimitsFilter.Background.Color := clCardBGDefaultDark;
  pMoltLimitsFilter.Border.Color := clSystemSolidNeutralFGDark;
  pMoltCycleFilter.Background.Color := clCardBGDefaultDark;
  pMoltCycleFilter.Border.Color := clSystemSolidNeutralFGDark;
  pTaxonomyClementsFilter.Background.Color := clCardBGDefaultDark;
  pTaxonomyClementsFilter.Border.Color := clSystemSolidNeutralFGDark;
  pTaxonomyIocFilter.Background.Color := clCardBGDefaultDark;
  pTaxonomyIocFilter.Border.Color := clSystemSolidNeutralFGDark;
  pTaxonomyCbroFilter.Background.Color := clCardBGDefaultDark;
  pTaxonomyCbroFilter.Border.Color := clSystemSolidNeutralFGDark;
  pStartTimeFilter.Background.Color := clCardBGDefaultDark;
  pStartTimeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pEndTimeFilter.Background.Color := clCardBGDefaultDark;
  pEndTimeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pAgeFilter.Background.Color := clCardBGDefaultDark;
  pAgeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pSkullOssificationFilter.Background.Color := clCardBGDefaultDark;
  pSkullOssificationFilter.Border.Color := clSystemSolidNeutralFGDark;
  pHowAgedFilter.Background.Color := clCardBGDefaultDark;
  pHowAgedFilter.Border.Color := clSystemSolidNeutralFGDark;
  pMarkedFilter.Background.Color := clCardBGDefaultDark;
  pMarkedFilter.Border.Color := clSystemSolidNeutralFGDark;
  pPersonFilter.Background.Color := clCardBGDefaultDark;
  pPersonFilter.Border.Color := clSystemSolidNeutralFGDark;
  pInstitutionFilter.Background.Color := clCardBGDefaultDark;
  pInstitutionFilter.Border.Color := clSystemSolidNeutralFGDark;
  pSurveyFilter.Background.Color := clCardBGDefaultDark;
  pSurveyFilter.Border.Color := clSystemSolidNeutralFGDark;
  pProjectFilter.Background.Color := clCardBGDefaultDark;
  pProjectFilter.Border.Color := clSystemSolidNeutralFGDark;
  pNestFilter.Background.Color := clCardBGDefaultDark;
  pNestFilter.Border.Color := clSystemSolidNeutralFGDark;
  pIndividualFilter.Background.Color := clCardBGDefaultDark;
  pIndividualFilter.Border.Color := clSystemSolidNeutralFGDark;
  pSamplingPlotFilter.Background.Color := clCardBGDefaultDark;
  pSamplingPlotFilter.Border.Color := clSystemSolidNeutralFGDark;
  pExpeditionFilter.Background.Color := clCardBGDefaultDark;
  pExpeditionFilter.Border.Color := clSystemSolidNeutralFGDark;
  pPlantFilter.Background.Color := clCardBGDefaultDark;
  pPlantFilter.Border.Color := clSystemSolidNeutralFGDark;
  pNeedsReviewFilter.Background.Color := clCardBGDefaultDark;
  pNeedsReviewFilter.Border.Color := clSystemSolidNeutralFGDark;
  pEscapedFilter.Background.Color := clCardBGDefaultDark;
  pEscapedFilter.Border.Color := clSystemSolidNeutralFGDark;
  pRecordInEbirdFilter.Background.Color := clCardBGDefaultDark;
  pRecordInEbirdFilter.Border.Color := clSystemSolidNeutralFGDark;
  pEggFilter.Background.Color := clCardBGDefaultDark;
  pEggFilter.Border.Color := clSystemSolidNeutralFGDark;
  pOutOfSampleFilter.Background.Color := clCardBGDefaultDark;
  pOutOfSampleFilter.Border.Color := clSystemSolidNeutralFGDark;
  pNidoparasiteFilter.Background.Color := clCardBGDefaultDark;
  pNidoparasiteFilter.Border.Color := clSystemSolidNeutralFGDark;
  pHatchedFilter.Background.Color := clCardBGDefaultDark;
  pHatchedFilter.Border.Color := clSystemSolidNeutralFGDark;
  pPhilornisFilter.Background.Color := clCardBGDefaultDark;
  pPhilornisFilter.Border.Color := clSystemSolidNeutralFGDark;
  pCaptureTypeFilter.Background.Color := clCardBGDefaultDark;
  pCaptureTypeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pCaptureStatusFilter.Background.Color := clCardBGDefaultDark;
  pCaptureStatusFilter.Border.Color := clSystemSolidNeutralFGDark;
  pBandTypeFilter.Background.Color := clCardBGDefaultDark;
  pBandTypeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pBandSourceFilter.Background.Color := clCardBGDefaultDark;
  pBandSourceFilter.Border.Color := clSystemSolidNeutralFGDark;
  pNestStatusFilter.Background.Color := clCardBGDefaultDark;
  pNestStatusFilter.Border.Color := clSystemSolidNeutralFGDark;
  pNestStageFilter.Background.Color := clCardBGDefaultDark;
  pNestStageFilter.Border.Color := clSystemSolidNeutralFGDark;
  pEggPatternFilter.Background.Color := clCardBGDefaultDark;
  pEggPatternFilter.Border.Color := clSystemSolidNeutralFGDark;
  pEggTextureFilter.Background.Color := clCardBGDefaultDark;
  pEggTextureFilter.Border.Color := clSystemSolidNeutralFGDark;
  pEggShapeFilter.Background.Color := clCardBGDefaultDark;
  pEggShapeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pPermitTypeFilter.Background.Color := clCardBGDefaultDark;
  pPermitTypeFilter.Border.Color := clSystemSolidNeutralFGDark;
  pReplacedBandFilter.Background.Color := clCardBGDefaultDark;
  pReplacedBandFilter.Border.Color := clSystemSolidNeutralFGDark;

  // Set filter edits images
  eEggFilter.Images := DMM.iEditsDark;
  ePlantFilter.Images := DMM.iEditsDark;
  eExpeditionFilter.Images := DMM.iEditsDark;
  eSamplingPlotFilter.Images := DMM.iEditsDark;
  eIndividualFilter.Images := DMM.iEditsDark;
  eNestFilter.Images := DMM.iEditsDark;
  eProjectFilter.Images := DMM.iEditsDark;
  eSurveyFilter.Images := DMM.iEditsDark;
  eInstitutionFilter.Images := DMM.iEditsDark;
  ePersonFilter.Images := DMM.iEditsDark;
  eHowAgedFilter.Images := DMM.iEditsDark;
  eStartTimeFilter.Images := DMM.iEditsDark;
  eEndTimeFilter.Images := DMM.iEditsDark;
  eCycleCodeFilter.Images := DMM.iEditsDark;
  eMoltLimitsFilter.Images := DMM.iEditsDark;
  eHowSexedFilter.Images := DMM.iEditsDark;
  eMethodFilter.Images := DMM.iEditsDark;

  // Set filter trees colors
  tvSiteFilter.Colors.TreeLineColor := clTextSecondaryDark;
  tvDateFilter.Colors.TreeLineColor := clTextSecondaryDark;
  tvTaxaFilter.Colors.TreeLineColor := clTextSecondaryDark;
  tsTaxonomyClements.Color := pTaxonomyClementsFilter.Background.Color;
  tsTaxonomyIoc.Color := pTaxonomyIocFilter.Background.Color;
  tsTaxonomyCbro.Color := pTaxonomyCbroFilter.Background.Color;

  // Set filter icons
  icoSiteFilter.Images := iIconsDark;
  icoSiteRankFilter.Images := iIconsDark;
  icoTaxonRanksFilter.Images := iIconsDark;
  icoMaterialFilter.Images := iIconsDark;
  icoMethodFilter.Images := iIconsDark;
  icoBandSizeFilter.Images := iIconsDark;
  icoWithRecapturesFilter.Images := iIconsDark;
  icoFatFilter.Images := iIconsDark;
  icoBandStatusFilter.Images := iIconsDark;
  icoDateFilter.Images := iIconsDark;
  icoNestFateFilter.Images := iIconsDark;
  icoNestSupportFilter.Images := iIconsDark;
  icoTaxaFilter.Images := iIconsDark;
  icoExtinctFilter.Images := iIconsDark;
  icoWithColorBandsFilter.Images := iIconsDark;
  icoSexFilter.Images := iIconsDark;
  icoReportedFilter.Images := iIconsDark;
  icoSynonymsFilter.Images := iIconsDark;
  icoMoltFilter.Images := iIconsDark;
  icoTaxonomiesFilter.Images := iIconsDark;
  icoStartTimeFilter.Images := iIconsDark;
  icoAgingFilter.Images := iIconsDark;
  icoMarkedFilter.Images := iIconsDark;
  icoPersonFilter.Images := iIconsDark;
  icoInstitutionFilter.Images := iIconsDark;
  icoSurveyFilter.Images := iIconsDark;
  icoProjectFilter.Images := iIconsDark;
  icoNestFilter.Images := iIconsDark;
  icoIndividualFilter.Images := iIconsDark;
  icoSamplingPlotFilter.Images := iIconsDark;
  icoExpeditionFilter.Images := iIconsDark;
  icoPlantFilter.Images := iIconsDark;
  icoNeedsReviewFilter.Images := iIconsDark;
  icoEscapedFilter.Images := iIconsDark;
  icoRecordInEbirdFilter.Images := iIconsDark;
  icoEggFilter.Images := iIconsDark;
  icoOutOfSampleFilter.Images := iIconsDark;
  icoNidoparasiteFilter.Images := iIconsDark;
  icoHatchedFilter.Images := iIconsDark;
  icoPhilornisFilter.Images := iIconsDark;
  icoCaptureTypeFilter.Images := iIconsDark;
  icoCaptureStatusFilter.Images := iIconsDark;
  icoBandTypeFilter.Images := iIconsDark;
  icoBandSourceFilter.Images := iIconsDark;
  icoNestStatusFilter.Images := iIconsDark;
  icoNestStageFilter.Images := iIconsDark;
  icoEggPatternFilter.Images := iIconsDark;
  icoPermitTypeFilter.Images := iIconsDark;
  icoReplacedBandFilter.Images := iIconsDark;
end;

procedure TfrmCustomGrid.CellKeyPress(Sender: TObject; var Key: Char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    with (Sender as TDBGrid), SelectedColumn do
    begin
      if FieldName = COL_TAXON_NAME then
        FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, COL_TAXON_ID, COL_TAXON_NAME, True, Key);
      if FieldName = COL_NIDOPARASITE_NAME then
        FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, COL_NIDOPARASITE_ID, COL_NIDOPARASITE_NAME, True, Key);

      if FieldName = COL_PARENT_TAXON_NAME then
        FindBotanicDlg([tfAll], InplaceEditor,
          DataSource.DataSet, COL_PARENT_TAXON_ID, COL_PARENT_TAXON_NAME, Key);
      if FieldName = COL_VALID_NAME then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, COL_VALID_ID, COL_VALID_NAME, Key);
      if FieldName = COL_SUPPORT_PLANT_1_NAME then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, COL_SUPPORT_PLANT_1_ID, COL_SUPPORT_PLANT_1_NAME, Key);
      if FieldName = COL_SUPPORT_PLANT_2_NAME then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, COL_SUPPORT_PLANT_2_ID, COL_SUPPORT_PLANT_2_NAME, Key);

      if FieldName = COL_COUNTRY_NAME then
        FindSiteDlg([gfCountries], InplaceEditor, DataSource.DataSet, COL_COUNTRY_ID, COL_COUNTRY_NAME, Key);
      if FieldName = COL_STATE_NAME then
        FindSiteDlg([gfStates], InplaceEditor, DataSource.DataSet, COL_STATE_ID, COL_STATE_NAME, Key);
      if FieldName = COL_MUNICIPALITY_NAME then
        FindSiteDlg([gfCities], InplaceEditor, DataSource.DataSet, COL_MUNICIPALITY_ID, COL_MUNICIPALITY_NAME, Key);
      if FieldName = COL_LOCALITY_NAME then
        FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, COL_LOCALITY_ID, COL_LOCALITY_NAME, Key);
      if FieldName = COL_PARENT_SITE_NAME then
        FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, COL_PARENT_SITE_ID, COL_PARENT_SITE_NAME, Key);

      if FieldName = COL_INSTITUTION_NAME then
        FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, COL_INSTITUTION_ID, COL_INSTITUTION_NAME, False, Key);
      if FieldName = COL_SUPPLIER_NAME then
        FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, COL_SUPPLIER_ID, COL_SUPPLIER_NAME, False, Key);

      if FieldName = COL_EXPEDITION_NAME then
        FindDlg(tbExpeditions, InplaceEditor, DataSource.DataSet, COL_EXPEDITION_ID, COL_EXPEDITION_NAME, False, Key);

      if FieldName = COL_SURVEY_NAME then
        FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, COL_SURVEY_ID, COL_SURVEY_NAME, False, Key);

      if FieldName = COL_NET_STATION_NAME then
        FindDlg(tbSamplingPlots, InplaceEditor, DataSource.DataSet, COL_NET_STATION_ID, COL_NET_STATION_NAME, False, Key);

      if FieldName = COL_OBSERVER_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_ID, COL_OBSERVER_NAME, False, Key);
      if FieldName = COL_OBSERVER_1_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_1_ID, COL_OBSERVER_1_NAME, False, Key);
      if FieldName = COL_OBSERVER_2_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_2_ID, COL_OBSERVER_2_NAME, False, Key);
      if FieldName = COL_CARRIER_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_CARRIER_ID, COL_CARRIER_NAME, False, Key);
      if FieldName = COL_BANDER_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_BANDER_ID, COL_BANDER_NAME, False, Key);
      if FieldName = COL_ANNOTATOR_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_ANNOTATOR_ID, COL_ANNOTATOR_NAME, False, Key);
      if FieldName = COL_PHOTOGRAPHER_1_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_PHOTOGRAPHER_1_ID, COL_PHOTOGRAPHER_1_NAME, False, Key);
      if FieldName = COL_PHOTOGRAPHER_2_NAME then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_PHOTOGRAPHER_2_ID, COL_PHOTOGRAPHER_2_NAME, False, Key);

      if FieldName = COL_PROJECT_NAME then
        FindDlg(tbProjects, InplaceEditor, DataSource.DataSet, COL_PROJECT_ID, COL_PROJECT_NAME, False, Key);

      if FieldName = COL_INDIVIDUAL_NAME then
        FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_INDIVIDUAL_ID, COL_INDIVIDUAL_NAME, False, Key);
      if FieldName = COL_FATHER_NAME then
        FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_FATHER_ID, COL_FATHER_NAME, False, Key);
      if FieldName = COL_MOTHER_NAME then
        FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_MOTHER_ID, COL_MOTHER_NAME, False, Key);

      if FieldName = COL_NEST_NAME then
        FindDlg(tbNests, InplaceEditor, DataSource.DataSet, COL_NEST_ID, COL_NEST_NAME, False, Key);

      if FieldName = COL_EGG_NAME then
        FindDlg(tbEggs, InplaceEditor, DataSource.DataSet, COL_EGG_ID, COL_EGG_NAME, False, Key);

      if FieldName = COL_BAND_NAME then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_BAND_ID, COL_BAND_NAME, False, Key);
      if FieldName = COL_DOUBLE_BAND_NAME then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_DOUBLE_BAND_ID, COL_DOUBLE_BAND_NAME, False, Key);
      if FieldName = COL_REMOVED_BAND_NAME then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_REMOVED_BAND_ID, COL_REMOVED_BAND_NAME, False, Key);
    end;
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    with (Sender as TDBGrid), SelectedColumn do
    begin
      if FieldName = COL_TAXON_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_TAXON_ID).Clear;
        DataSource.DataSet.FieldByName(COL_TAXON_NAME).Clear;
      end;
      if FieldName = COL_NIDOPARASITE_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_NIDOPARASITE_ID).Clear;
        DataSource.DataSet.FieldByName(COL_NIDOPARASITE_NAME).Clear;
      end;

      if FieldName = COL_PARENT_TAXON_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_PARENT_TAXON_ID).Clear;
        DataSource.DataSet.FieldByName(COL_PARENT_TAXON_NAME).Clear;
      end;
      if FieldName = COL_VALID_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_VALID_ID).Clear;
        DataSource.DataSet.FieldByName(COL_VALID_NAME).Clear;
      end;
      if FieldName = COL_SUPPORT_PLANT_1_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_ID).Clear;
        DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_NAME).Clear;
      end;
      if FieldName = COL_SUPPORT_PLANT_2_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_ID).Clear;
        DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_NAME).Clear;
      end;

      if FieldName = COL_COUNTRY_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_COUNTRY_ID).Clear;
        DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Clear;
      end;
      if FieldName = COL_STATE_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_STATE_ID).Clear;
        DataSource.DataSet.FieldByName(COL_STATE_NAME).Clear;
      end;
      if FieldName = COL_MUNICIPALITY_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_MUNICIPALITY_ID).Clear;
        DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Clear;
      end;
      if FieldName = COL_LOCALITY_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_LOCALITY_ID).Clear;
        DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Clear;
      end;
      if FieldName = COL_PARENT_SITE_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_PARENT_SITE_ID).Clear;
        DataSource.DataSet.FieldByName(COL_PARENT_SITE_NAME).Clear;
      end;

      if FieldName = COL_INSTITUTION_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_INSTITUTION_ID).Clear;
        DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Clear;
      end;
      if FieldName = COL_SUPPLIER_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_SUPPLIER_ID).Clear;
        DataSource.DataSet.FieldByName(COL_SUPPLIER_NAME).Clear;
      end;

      if FieldName = COL_EXPEDITION_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_EXPEDITION_ID).Clear;
        DataSource.DataSet.FieldByName(COL_EXPEDITION_NAME).Clear;
      end;

      if FieldName = COL_SURVEY_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_SURVEY_ID).Clear;
        DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Clear;
      end;

      if FieldName = COL_NET_STATION_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_NET_STATION_ID).Clear;
        DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Clear;
      end;

      if FieldName = COL_OBSERVER_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_OBSERVER_ID).Clear;
        DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Clear;
      end;
      if FieldName = COL_OBSERVER_1_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_OBSERVER_1_ID).Clear;
        DataSource.DataSet.FieldByName(COL_OBSERVER_1_NAME).Clear;
      end;
      if FieldName = COL_OBSERVER_2_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_OBSERVER_2_ID).Clear;
        DataSource.DataSet.FieldByName(COL_OBSERVER_2_NAME).Clear;
      end;
      if FieldName = COL_CARRIER_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_CARRIER_ID).Clear;
        DataSource.DataSet.FieldByName(COL_CARRIER_NAME).Clear;
      end;
      if FieldName = COL_BANDER_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_BANDER_ID).Clear;
        DataSource.DataSet.FieldByName(COL_BANDER_NAME).Clear;
      end;
      if FieldName = COL_ANNOTATOR_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_ANNOTATOR_ID).Clear;
        DataSource.DataSet.FieldByName(COL_ANNOTATOR_NAME).Clear;
      end;
      if FieldName = COL_PHOTOGRAPHER_1_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_ID).Clear;
        DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_NAME).Clear;
      end;
      if FieldName = COL_PHOTOGRAPHER_2_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_ID).Clear;
        DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_NAME).Clear;
      end;

      if FieldName = COL_PROJECT_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_PROJECT_ID).Clear;
        DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Clear;
      end;

      if FieldName = COL_INDIVIDUAL_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_INDIVIDUAL_ID).Clear;
        DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Clear;
      end;
      if FieldName = COL_FATHER_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_FATHER_ID).Clear;
        DataSource.DataSet.FieldByName(COL_FATHER_NAME).Clear;
      end;
      if FieldName = COL_MOTHER_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_MOTHER_ID).Clear;
        DataSource.DataSet.FieldByName(COL_MOTHER_NAME).Clear;
      end;

      if FieldName = COL_NEST_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_NEST_ID).Clear;
        DataSource.DataSet.FieldByName(COL_NEST_NAME).Clear;
      end;

      if FieldName = COL_EGG_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_EGG_ID).Clear;
        DataSource.DataSet.FieldByName(COL_EGG_NAME).Clear;
      end;

      if FieldName = COL_BAND_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_BAND_ID).Clear;
        DataSource.DataSet.FieldByName(COL_BAND_NAME).Clear;
      end;
      if FieldName = COL_DOUBLE_BAND_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_DOUBLE_BAND_ID).Clear;
        DataSource.DataSet.FieldByName(COL_DOUBLE_BAND_NAME).Clear;
      end;
      if FieldName = COL_REMOVED_BAND_NAME then
      begin
        DataSource.DataSet.FieldByName(COL_REMOVED_BAND_ID).Clear;
        DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Clear;
      end;
    end;
    Key := #0;
  end;
end;

procedure TfrmCustomGrid.ClearBandFilters;
begin
  cbBandSizeFilter.ItemIndex := 0;

  cbBandStatusFilter.ItemIndex := 0;
  cbBandTypeFilter.ItemIndex := 0;
  cbBandSourceFilter.ItemIndex := 0;

  rbReportedAll.Checked := True;

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

  rbIsSynonymAll.Checked := True;
  rbHasSynonymsAll.Checked := True;
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

  rbNeedsReviewAll.Checked := True;
  rbEscapedAll.Checked := True;

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
  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eProjectFilter.Clear;
  FProjectKeyFilter := 0;
end;

procedure TfrmCustomGrid.ClearFeatherFilters;
begin
  lblCountSiteFilter.Caption := rsNoneSelected;
  tvSiteFilter.ClearChecked;

  lblCountTaxonFilter.Caption := rsNoneSelected;
  tvTaxaFilter.ClearChecked;

  lblCountDateFilter.Caption := rsNoneSelectedFemale;
  tvDateFilter.ClearChecked;

  eStartTimeFilter.Clear;
  eEndTimeFilter.Clear;

  ePersonFilter.Clear;
  FPersonKeyFilter := 0;
  eIndividualFilter.Clear;
  FIndividualKeyFilter := 0;
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

  rbWithColorBandsAll.Checked := True;
  rbWithRecapturesAll.Checked := True;

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

procedure TfrmCustomGrid.ClearSamplingPlotFilters;
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
  if not FCanToggle then
    Exit;

  FCanToggle := False;
  rbMarkedAll.Checked := True;

  case TableType of
    tbNone: ;
    //tbProjectTeams: ;
    tbPermits:       ClearPermitFilters;
    tbGazetteer:     ClearGazetteerFilters;
    tbBotanicTaxa:   ClearBotanicTaxaFilters;
    tbNests:         ClearNestFilters;
    tbNestRevisions: ClearNestRevisionFilters;
    tbEggs:          ClearEggFilters;
    tbSamplingPlots: ClearSamplingPlotFilters;
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
    tbFeathers:      ClearFeatherFilters;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  //EP.Clear;
  FSearch.QuickFilters.Clear;

  FSearch.RunSearch;

  FCanToggle := True;
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

  tsTaxonomyClements.Checked := False;
  tsTaxonomyIoc.Checked := False;
  tsTaxonomyCbro.Checked := False;

  rbExtinctAll.Checked := True;

  rbIsSynonymAll.Checked := True;
  rbHasSynonymsAll.Checked := True;
end;

procedure TfrmCustomGrid.CreatePanelTabs;
var
  PanelTab: TCustomPanelTab;
  Titles: array of string;
  Counts: array of Integer;
  i: Integer;
  XPos: Integer;
begin
  // Define tabs titles
  case FTableType of
    tbIndividuals:
      begin
        Titles := [rsTitleCaptures, rsCaptionFeathers, rsTitleSightings, rsTitleNests, rsTitleSpecimens];
        Counts := [0, 0, 0, 0, 0];
      end;
    tbNests:
      begin
        Titles := [rsTitleNestOwners, rsTitleNestRevisions, rsTitleEggs];
        Counts := [0, 0, 0];
      end;
    tbExpeditions:
      begin
        Titles := [rsTitleSurveys];
        Counts := [0];
      end;
    tbSurveys:
      begin
        Titles := [rsTitleSurveyTeam, rsTitleNetsEffort, rsTitleWeather, rsTitleCaptures, rsTitleSightings, rsTitleVegetation];
        Counts := [0, 0, 0, 0, 0, 0];
      end;
    tbSpecimens:
      begin
        Titles := [rsTitleSpecimenCollectors, rsTitleSamplePreps];
        Counts := [0, 0];
      end;
    tbSamplingPlots:
      begin
        Titles := [rsTitlePermanentNets];
        Counts := [0];
      end;
    tbProjects:
      begin
        Titles := [rsTitleProjectMembers, rsTitleGoals, rsTitleChronogram, rsTitleBudget, rsTitleExpenses];
        Counts := [0, 0, 0, 0, 0];
      end;
  end;

  XPos := 70;

  for i := 0 to High(Titles) do
  begin
    PanelTab := TCustomPanelTab.Create(pChildsBar, Self, Titles[i], Counts[i], i);
    PanelTab.Left := XPos;
    PanelTab.Top := 0; //i * (PanelTab.Height + 5) + 10;

    XPos := XPos + PanelTab.Width + 10;

    panelTabs.Add(PanelTab);
  end;
end;

procedure TfrmCustomGrid.DBGColEnter(Sender: TObject);
begin
  //if sbEditRecord.Enabled then
    gridColumns.Row := DBG.SelectedColumn.Field.Index + 1;

  if sbShowSummary.Visible then
    Summary;
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

procedure TfrmCustomGrid.DBGContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  // if right clicked column header, open the column menu
  if MousePos.Y < TDBGrid(Sender).DefaultRowHeight then
  begin
    pmColumn.PopUp;
    Handled := True;
  end;
  // else, open the grid menu
end;

procedure TfrmCustomGrid.DBGDblClick(Sender: TObject);
begin
  if sbEditRecord.Enabled then
    sbEditRecordClick(nil);
end;

procedure TfrmCustomGrid.DBGEditButtonClick(Sender: TObject);
begin
  with (Sender as TDBGrid), SelectedColumn do
  begin
    // Open find dialog
    if FieldName = COL_TAXON_NAME then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, COL_TAXON_ID, COL_TAXON_NAME, True);
    if FieldName = COL_NIDOPARASITE_NAME then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, COL_NIDOPARASITE_ID, COL_NIDOPARASITE_NAME, True);

    if FieldName = COL_PARENT_TAXON_NAME then
      FindBotanicDlg([tfAll], InplaceEditor,
        DataSource.DataSet, COL_PARENT_TAXON_ID, COL_PARENT_TAXON_NAME);
    if FieldName = COL_VALID_NAME then
      FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, COL_VALID_ID, COL_VALID_NAME);
    if FieldName = COL_SUPPORT_PLANT_1_NAME then
      FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, COL_SUPPORT_PLANT_1_ID, COL_SUPPORT_PLANT_1_NAME);
    if FieldName = COL_SUPPORT_PLANT_2_NAME then
      FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, COL_SUPPORT_PLANT_2_ID, COL_SUPPORT_PLANT_2_NAME);

    if FieldName = COL_COUNTRY_NAME then
      FindSiteDlg([gfCountries], InplaceEditor, DataSource.DataSet, COL_COUNTRY_ID, COL_COUNTRY_NAME);
    if FieldName = COL_STATE_NAME then
      FindSiteDlg([gfStates], InplaceEditor, DataSource.DataSet, COL_STATE_ID, COL_STATE_NAME);
    if FieldName = COL_MUNICIPALITY_NAME then
      FindSiteDlg([gfCities], InplaceEditor, DataSource.DataSet, COL_MUNICIPALITY_ID, COL_MUNICIPALITY_NAME);
    if FieldName = COL_LOCALITY_NAME then
      FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, COL_LOCALITY_ID, COL_LOCALITY_NAME);
    if FieldName = COL_PARENT_SITE_NAME then
      FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, COL_PARENT_SITE_ID, COL_PARENT_SITE_NAME);

    if FieldName = COL_INSTITUTION_NAME then
      FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, COL_INSTITUTION_ID, COL_INSTITUTION_NAME, False);
    if FieldName = COL_SUPPLIER_NAME then
      FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, COL_SUPPLIER_ID, COL_SUPPLIER_NAME, False);

    if FieldName = COL_EXPEDITION_NAME then
      FindDlg(tbExpeditions, InplaceEditor, DataSource.DataSet, COL_EXPEDITION_ID, COL_EXPEDITION_NAME, False);

    if FieldName = COL_SURVEY_NAME then
      FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, COL_SURVEY_ID, COL_SURVEY_NAME, False);

    if FieldName = COL_NET_STATION_NAME then
      FindDlg(tbSamplingPlots, InplaceEditor, DataSource.DataSet, COL_NET_STATION_ID, COL_NET_STATION_NAME, False);

    if FieldName = COL_OBSERVER_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_ID, COL_OBSERVER_NAME, False);
    if FieldName = COL_OBSERVER_1_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_1_ID, COL_OBSERVER_1_NAME, False);
    if FieldName = COL_OBSERVER_2_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_2_ID, COL_OBSERVER_2_NAME, False);
    if FieldName = COL_CARRIER_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_CARRIER_ID, COL_CARRIER_NAME, False);
    if FieldName = COL_BANDER_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_BANDER_ID, COL_BANDER_NAME, False);
    if FieldName = COL_ANNOTATOR_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_ANNOTATOR_ID, COL_ANNOTATOR_NAME, False);
    if FieldName = COL_PHOTOGRAPHER_1_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_PHOTOGRAPHER_1_ID, COL_PHOTOGRAPHER_1_NAME, False);
    if FieldName = COL_PHOTOGRAPHER_2_NAME then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_PHOTOGRAPHER_2_ID, COL_PHOTOGRAPHER_2_NAME, False);

    if FieldName = COL_PROJECT_NAME then
      FindDlg(tbProjects, InplaceEditor, DataSource.DataSet, COL_PROJECT_ID, COL_PROJECT_NAME, False);

    if FieldName = COL_INDIVIDUAL_NAME then
      FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_INDIVIDUAL_ID, COL_INDIVIDUAL_NAME, False);
    if FieldName = COL_FATHER_NAME then
      FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_FATHER_ID, COL_FATHER_NAME, False);
    if FieldName = COL_MOTHER_NAME then
      FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_MOTHER_ID, COL_MOTHER_NAME, False);

    if FieldName = COL_NEST_NAME then
      FindDlg(tbNests, InplaceEditor, DataSource.DataSet, COL_NEST_ID, COL_NEST_NAME, False);

    if FieldName = COL_EGG_NAME then
      FindDlg(tbEggs, InplaceEditor, DataSource.DataSet, COL_EGG_ID, COL_EGG_NAME, False);

    if FieldName = COL_BAND_NAME then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_BAND_ID, COL_BAND_NAME, False);
    if FieldName = COL_DOUBLE_BAND_NAME then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_DOUBLE_BAND_ID, COL_DOUBLE_BAND_NAME, False);
    if FieldName = COL_REMOVED_BAND_NAME then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_REMOVED_BAND_ID, COL_REMOVED_BAND_NAME, False);

    // Open detection dialog
    if FieldName = COL_DETECTION_TYPE then
      DetectionDialog(DataSource.DataSet.FieldByName(COL_DETECTION_TYPE).AsString,
        DataSource.DataSet, COL_DETECTION_TYPE);
    // Open breeding and behavior dialog
    if FieldName = COL_BREEDING_STATUS then
      BreedingDialog(DataSource.DataSet.FieldByName(COL_BREEDING_STATUS).AsString,
        DataSource.DataSet, COL_BREEDING_STATUS);

    // Open molt limits dialog
    if FieldName = COL_MOLT_LIMITS then
      MoltLimitsDialog(DataSource.DataSet.FieldByName(COL_MOLT_LIMITS).AsString, DataSource.DataSet, COL_MOLT_LIMITS);
    // Open molt cycle code dialog
    if FieldName = COL_CYCLE_CODE then
      MoltCycleDialog(DataSource.DataSet.FieldByName(COL_CYCLE_CODE).AsString, DataSource.DataSet, COL_CYCLE_CODE);
    // Open how was aged/sexed dialog
    if FieldName = COL_HOW_AGED then
      HowAgedDialog(DataSource.DataSet.FieldByName(COL_HOW_AGED).AsString,  DataSource.DataSet, COL_HOW_AGED);
    if FieldName = COL_HOW_SEXED then
      HowAgedDialog(DataSource.DataSet.FieldByName(COL_HOW_SEXED).AsString, DataSource.DataSet, COL_HOW_SEXED);

    // Open band colors dialog
    if FieldName = COL_RIGHT_TARSUS then
      EditColorBands(DataSource.DataSet, COL_RIGHT_TARSUS, InplaceEditor);
    if FieldName = COL_LEFT_TARSUS then
      EditColorBands(DataSource.DataSet, COL_LEFT_TARSUS, InplaceEditor);
    if FieldName = COL_RIGHT_TIBIA then
      EditColorBands(DataSource.DataSet, COL_RIGHT_TIBIA, InplaceEditor);
    if FieldName = COL_LEFT_TIBIA then
      EditColorBands(DataSource.DataSet, COL_LEFT_TIBIA, InplaceEditor);

    // Open calendar dialog
    if (FieldName = COL_SIGHTING_DATE) or
      (FieldName = COL_MEASURE_DATE) or
      (FieldName = COL_START_DATE) or
      (FieldName = COL_END_DATE) or
      (FieldName = COL_SURVEY_DATE) or
      (FieldName = COL_BIRTH_DATE) or
      (FieldName = COL_DEATH_DATE) or
      (FieldName = COL_BANDING_DATE) or
      (FieldName = COL_BAND_CHANGE_DATE) or
      (FieldName = COL_FOUND_DATE) or
      (FieldName = COL_LAST_DATE) or
      (FieldName = COL_REVISION_DATE) or
      (FieldName = COL_DISPATCH_DATE) or
      (FieldName = COL_EXPIRE_DATE) or
      (FieldName = COL_SAMPLE_DATE) or
      (FieldName = COL_CAPTURE_DATE) then
      CalendarDlg(InplaceEditor, DataSource.DataSet, FieldName);

    // Open GeoAssist dialog
    if (FieldName = COL_LONGITUDE) or (FieldName = COL_LATITUDE) then
      GeoAssistDlg(InplaceEditor, DataSource.DataSet, COL_LONGITUDE, COL_LATITUDE);
    if (FieldName = COL_START_LONGITUDE) or (FieldName = COL_START_LATITUDE) then
      GeoAssistDlg(InplaceEditor, DataSource.DataSet, COL_START_LONGITUDE, COL_START_LATITUDE);
    if (FieldName = COL_END_LONGITUDE) or (FieldName = COL_END_LATITUDE) then
      GeoAssistDlg(InplaceEditor, DataSource.DataSet, COL_END_LONGITUDE, COL_END_LATITUDE);
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

procedure TfrmCustomGrid.dbgImagesDblClick(Sender: TObject);
begin
  if sbViewImage.Enabled then
    sbViewImageClick(nil);
end;

procedure TfrmCustomGrid.DBGKeyPress(Sender: TObject; var Key: char);
const
  FPress: array of String = (COL_TAXON_NAME, COL_NIDOPARASITE_NAME, COL_PARENT_TAXON_NAME, COL_VALID_NAME,
    COL_SUPPORT_PLANT_1_NAME, COL_SUPPORT_PLANT_2_NAME, COL_COUNTRY_NAME, COL_STATE_NAME, COL_MUNICIPALITY_NAME,
    COL_LOCALITY_NAME, COL_PARENT_SITE_NAME, COL_INSTITUTION_NAME, COL_SUPPLIER_NAME, COL_EXPEDITION_NAME,
    COL_SURVEY_NAME, COL_NET_STATION_NAME, COL_OBSERVER_NAME, COL_OBSERVER_1_NAME, COL_OBSERVER_2_NAME,
    COL_CARRIER_NAME, COL_BANDER_NAME, COL_ANNOTATOR_NAME, COL_PHOTOGRAPHER_1_NAME, COL_PHOTOGRAPHER_2_NAME,
    COL_PROJECT_NAME, COL_INDIVIDUAL_NAME, COL_FATHER_NAME, COL_MOTHER_NAME, COL_NEST_NAME, COL_EGG_NAME,
    COL_BAND_NAME, COL_DOUBLE_BAND_NAME, COL_REMOVED_BAND_NAME);
var
  Grid: TDBGrid;
begin
  FormKeyPress(Sender, Key);

  Grid := TDBGrid(Sender);
  if (Grid.EditorMode) and (Grid.SelectedColumn.FieldName in FPress) then
  begin
    { Alphabetic search in numeric field }
    if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
    begin
      with Grid, SelectedColumn do
      begin
        if FieldName = COL_TAXON_NAME then
          FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, COL_TAXON_ID, COL_TAXON_NAME, True, Key);
        if FieldName = COL_NIDOPARASITE_NAME then
          FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, COL_NIDOPARASITE_ID, COL_NIDOPARASITE_NAME, True, Key);

        if FieldName = COL_PARENT_TAXON_NAME then
          FindBotanicDlg([tfAll], InplaceEditor,
            DataSource.DataSet, COL_PARENT_TAXON_ID, COL_PARENT_TAXON_NAME, Key);
        if FieldName = COL_VALID_NAME then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, COL_VALID_ID, COL_VALID_NAME, Key);
        if FieldName = COL_SUPPORT_PLANT_1_NAME then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, COL_SUPPORT_PLANT_1_ID, COL_SUPPORT_PLANT_1_NAME, Key);
        if FieldName = COL_SUPPORT_PLANT_2_NAME then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, COL_SUPPORT_PLANT_2_ID, COL_SUPPORT_PLANT_2_NAME, Key);

        if FieldName = COL_COUNTRY_NAME then
          FindSiteDlg([gfCountries], InplaceEditor, DataSource.DataSet, COL_COUNTRY_ID, COL_COUNTRY_NAME, Key);
        if FieldName = COL_STATE_NAME then
          FindSiteDlg([gfStates], InplaceEditor, DataSource.DataSet, COL_STATE_ID, COL_STATE_NAME, Key);
        if FieldName = COL_MUNICIPALITY_NAME then
          FindSiteDlg([gfCities], InplaceEditor, DataSource.DataSet, COL_MUNICIPALITY_ID, COL_MUNICIPALITY_NAME, Key);
        if FieldName = COL_LOCALITY_NAME then
          FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, COL_LOCALITY_ID, COL_LOCALITY_NAME, Key);
        if FieldName = COL_PARENT_SITE_NAME then
          FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, COL_PARENT_SITE_ID, COL_PARENT_SITE_NAME, Key);

        if FieldName = COL_INSTITUTION_NAME then
          FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, COL_INSTITUTION_ID, COL_INSTITUTION_NAME, False, Key);
        if FieldName = COL_SUPPLIER_NAME then
          FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, COL_SUPPLIER_ID, COL_SUPPLIER_NAME, False, Key);

        if FieldName = COL_EXPEDITION_NAME then
          FindDlg(tbExpeditions, InplaceEditor, DataSource.DataSet, COL_EXPEDITION_ID, COL_EXPEDITION_NAME, False, Key);

        if FieldName = COL_SURVEY_NAME then
          FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, COL_SURVEY_ID, COL_SURVEY_NAME, False, Key);

        if FieldName = COL_NET_STATION_NAME then
          FindDlg(tbSamplingPlots, InplaceEditor, DataSource.DataSet, COL_NET_STATION_ID, COL_NET_STATION_NAME, False, Key);

        if FieldName = COL_OBSERVER_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_ID, COL_OBSERVER_NAME, False, Key);
        if FieldName = COL_OBSERVER_1_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_1_ID, COL_OBSERVER_1_NAME, False, Key);
        if FieldName = COL_OBSERVER_2_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_OBSERVER_2_ID, COL_OBSERVER_2_NAME, False, Key);
        if FieldName = COL_CARRIER_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_CARRIER_ID, COL_CARRIER_NAME, False, Key);
        if FieldName = COL_BANDER_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_BANDER_ID, COL_BANDER_NAME, False, Key);
        if FieldName = COL_ANNOTATOR_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_ANNOTATOR_ID, COL_ANNOTATOR_NAME, False, Key);
        if FieldName = COL_PHOTOGRAPHER_1_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_PHOTOGRAPHER_1_ID, COL_PHOTOGRAPHER_1_NAME, False, Key);
        if FieldName = COL_PHOTOGRAPHER_2_NAME then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, COL_PHOTOGRAPHER_2_ID, COL_PHOTOGRAPHER_2_NAME, False, Key);

        if FieldName = COL_PROJECT_NAME then
          FindDlg(tbProjects, InplaceEditor, DataSource.DataSet, COL_PROJECT_ID, COL_PROJECT_NAME, False, Key);

        if FieldName = COL_INDIVIDUAL_NAME then
          FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_INDIVIDUAL_ID, COL_INDIVIDUAL_NAME, False, Key);
        if FieldName = COL_FATHER_NAME then
          FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_FATHER_ID, COL_FATHER_NAME, False, Key);
        if FieldName = COL_MOTHER_NAME then
          FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, COL_MOTHER_ID, COL_MOTHER_NAME, False, Key);

        if FieldName = COL_NEST_NAME then
          FindDlg(tbNests, InplaceEditor, DataSource.DataSet, COL_NEST_ID, COL_NEST_NAME, False, Key);

        if FieldName = COL_EGG_NAME then
          FindDlg(tbEggs, InplaceEditor, DataSource.DataSet, COL_EGG_ID, COL_EGG_NAME, False, Key);

        if FieldName = COL_BAND_NAME then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_BAND_ID, COL_BAND_NAME, False, Key);
        if FieldName = COL_DOUBLE_BAND_NAME then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_DOUBLE_BAND_ID, COL_DOUBLE_BAND_NAME, False, Key);
        if FieldName = COL_REMOVED_BAND_NAME then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, COL_REMOVED_BAND_ID, COL_REMOVED_BAND_NAME, False, Key);
      end;
      Key := #0;
    end;
    { CLEAR FIELD VALUE = Backspace }
    if (Key = #8) then
    begin
      with (Sender as TDBGrid), SelectedColumn do
      begin
        if FieldName = COL_TAXON_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_TAXON_ID).Clear;
          DataSource.DataSet.FieldByName(COL_TAXON_NAME).Clear;
        end;
        if FieldName = COL_NIDOPARASITE_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_NIDOPARASITE_ID).Clear;
          DataSource.DataSet.FieldByName(COL_NIDOPARASITE_NAME).Clear;
        end;

        if FieldName = COL_PARENT_TAXON_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_PARENT_TAXON_ID).Clear;
          DataSource.DataSet.FieldByName(COL_PARENT_TAXON_NAME).Clear;
        end;
        if FieldName = COL_VALID_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_VALID_ID).Clear;
          DataSource.DataSet.FieldByName(COL_VALID_NAME).Clear;
        end;
        if FieldName = COL_SUPPORT_PLANT_1_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_ID).Clear;
          DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_NAME).Clear;
        end;
        if FieldName = COL_SUPPORT_PLANT_2_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_ID).Clear;
          DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_NAME).Clear;
        end;

        if FieldName = COL_COUNTRY_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_COUNTRY_ID).Clear;
          DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Clear;
        end;
        if FieldName = COL_STATE_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_STATE_ID).Clear;
          DataSource.DataSet.FieldByName(COL_STATE_NAME).Clear;
        end;
        if FieldName = COL_MUNICIPALITY_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_MUNICIPALITY_ID).Clear;
          DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Clear;
        end;
        if FieldName = COL_LOCALITY_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_LOCALITY_ID).Clear;
          DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Clear;
        end;
        if FieldName = COL_PARENT_SITE_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_PARENT_SITE_ID).Clear;
          DataSource.DataSet.FieldByName(COL_PARENT_SITE_NAME).Clear;
        end;

        if FieldName = COL_INSTITUTION_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_INSTITUTION_ID).Clear;
          DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Clear;
        end;
        if FieldName = COL_SUPPLIER_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_SUPPLIER_ID).Clear;
          DataSource.DataSet.FieldByName(COL_SUPPLIER_NAME).Clear;
        end;

        if FieldName = COL_EXPEDITION_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_EXPEDITION_ID).Clear;
          DataSource.DataSet.FieldByName(COL_EXPEDITION_NAME).Clear;
        end;

        if FieldName = COL_SURVEY_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_SURVEY_ID).Clear;
          DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Clear;
        end;

        if FieldName = COL_NET_STATION_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_NET_STATION_ID).Clear;
          DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Clear;
        end;

        if FieldName = COL_OBSERVER_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_OBSERVER_ID).Clear;
          DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Clear;
        end;
        if FieldName = COL_OBSERVER_1_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_OBSERVER_1_ID).Clear;
          DataSource.DataSet.FieldByName(COL_OBSERVER_1_NAME).Clear;
        end;
        if FieldName = COL_OBSERVER_2_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_OBSERVER_2_ID).Clear;
          DataSource.DataSet.FieldByName(COL_OBSERVER_2_NAME).Clear;
        end;
        if FieldName = COL_CARRIER_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_CARRIER_ID).Clear;
          DataSource.DataSet.FieldByName(COL_CARRIER_NAME).Clear;
        end;
        if FieldName = COL_BANDER_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_BANDER_ID).Clear;
          DataSource.DataSet.FieldByName(COL_BANDER_NAME).Clear;
        end;
        if FieldName = COL_ANNOTATOR_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_ANNOTATOR_ID).Clear;
          DataSource.DataSet.FieldByName(COL_ANNOTATOR_NAME).Clear;
        end;
        if FieldName = COL_PHOTOGRAPHER_1_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_ID).Clear;
          DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_NAME).Clear;
        end;
        if FieldName = COL_PHOTOGRAPHER_2_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_ID).Clear;
          DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_NAME).Clear;
        end;

        if FieldName = COL_PROJECT_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_PROJECT_ID).Clear;
          DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Clear;
        end;

        if FieldName = COL_INDIVIDUAL_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_INDIVIDUAL_ID).Clear;
          DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Clear;
        end;
        if FieldName = COL_FATHER_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_FATHER_ID).Clear;
          DataSource.DataSet.FieldByName(COL_FATHER_NAME).Clear;
        end;
        if FieldName = COL_MOTHER_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_MOTHER_ID).Clear;
          DataSource.DataSet.FieldByName(COL_MOTHER_NAME).Clear;
        end;

        if FieldName = COL_NEST_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_NEST_ID).Clear;
          DataSource.DataSet.FieldByName(COL_NEST_NAME).Clear;
        end;

        if FieldName = COL_EGG_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_EGG_ID).Clear;
          DataSource.DataSet.FieldByName(COL_EGG_NAME).Clear;
        end;

        if FieldName = COL_BAND_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_BAND_ID).Clear;
          DataSource.DataSet.FieldByName(COL_BAND_NAME).Clear;
        end;
        if FieldName = COL_DOUBLE_BAND_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_DOUBLE_BAND_ID).Clear;
          DataSource.DataSet.FieldByName(COL_DOUBLE_BAND_NAME).Clear;
        end;
        if FieldName = COL_REMOVED_BAND_NAME then
        begin
          DataSource.DataSet.FieldByName(COL_REMOVED_BAND_ID).Clear;
          DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Clear;
        end;
      end;
      Key := #0;
    end;
  end;
end;

procedure TfrmCustomGrid.DBGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
  Grid: TDBGrid;
  CellRect: TRect;
begin
  Grid := TDBGrid(Sender);
  Grid.MouseToCell(X, Y, ACol, ARow);

  FIsResizing := False;
  FIsMoving := False;
  FStartX := X;
  FStartY := Y;
  FStartCol := ACol;
  FStartRow := ARow;

  if (Button = mbLeft) and (ARow = 0) then
  begin
    CellRect := Grid.CellRect(ACol, ARow);
    // Check if mouse is near the column right border to resize
    if (Abs(X - CellRect.Right) <= 3) then
    begin
      FIsResizing := True;
    end;
  end;
end;

procedure TfrmCustomGrid.DBGMouseMove
  (Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
begin
  if FIsResizing then
  begin
    // Add here the column resizing logic, if necessary
  end
  else if (ssLeft in Shift) then
  begin
    DeltaX := Abs(X - FStartX);
    DeltaY := Abs(Y - FStartY);

    if (DeltaX > 4) or (DeltaY > 4) then
    begin
      FIsMoving := True;
      // Add here the column moving logic, if necessary
    end;
  end;
end;

procedure TfrmCustomGrid.DBGMouseUp
  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Direction: TSortDirection;
  ACol, ARow: Longint;
  Column: TColumn;
  Grid: TDBGrid;
  idx: Integer;
begin
  Grid := TDBGrid(Sender);
  idx := -1;

  // If was resizing or moving a column, do not sort records
  if FIsResizing or FIsMoving then
  begin
    FIsResizing := False;
    FIsMoving := False;
    Exit;
  end;

  { Sort records on title click }
  if (Button = mbLeft) and (Grid.MouseCoord(X, Y).Y = 0) then
  begin
    Direction := sdAscending;

    Grid.MouseToCell(X, Y, ACol, ARow);
    Column := Grid.Columns[ACol - 1];

    if FSearch.SortFields.Count > 0 then
    begin
      for idx := 0 to (FSearch.SortFields.Count - 1) do
      begin
        if Column.FieldName = FSearch.SortFields[idx].FieldName then
        begin
          if FSearch.SortFields[idx].Direction = sdAscending then
            Direction := sdDescending
          else
            Direction := sdAscending;
          Break;
        end;
      end;
    end;

    // column header click holding Ctrl key, adds column to the sorting
    // if not, sort only the column clicked
    if not (ssCtrl in Shift) then
      FSearch.SortFields.Clear;

    if not (pfInUpdate in Grid.DataSource.DataSet.FieldByName(Column.FieldName).ProviderFlags) then
      AddSortedField(Column.FieldName, Direction, '', True)
    else
      AddSortedField(Column.FieldName, Direction);
    Search(FSearchString);
  end;
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
          aTable := tbFeathers
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
          aTable := tbSightings
        else
        if Sender = gridChild6 then
          aTable := tbVegetation;
      end;
      tbSpecimens:
      begin
        if Sender = gridChild1 then
          aTable := tbSpecimenCollectors
        else
        if Sender = gridChild2 then
          aTable := tbSamplePreps;
      end;
      tbSamplingPlots:
      begin
        if Sender = gridChild1 then
          aTable := tbPermanentNets;
      end;
      tbProjects:
      begin
        if Sender = gridChild1 then
          aTable := tbProjectTeams
        else
        if Sender = gridChild2 then
          aTable := tbProjectGoals
        else
        if Sender = gridChild3 then
          aTable := tbProjectChronograms
        else
        if Sender = gridChild4 then
          aTable := tbProjectBudgets
        else
        if Sender = gridChild5 then
          aTable := tbProjectExpenses;
      end;
    end;
  end;

  case aTable of
    tbInstitutions:   PrepareCanvasInstitutions(Column, sender);
    tbPeople:         PrepareCanvasPeople(Column, sender);
    tbBands:          PrepareCanvasBands(Column, sender);
    tbIndividuals:    PrepareCanvasIndividuals(Column, sender);
    tbCaptures:       PrepareCanvasCaptures(Column, sender);
    tbFeathers:       PrepareCanvasFeathers(Column, Sender);
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
    tbSamplingPlots: ;
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
  if isOpening or isClosing then
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

procedure TfrmCustomGrid.DropAudiosDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint;
  var Effect: Longint);
const
  SupportedAudios: array of String = ('.wav','.mp3','.aac','.flac');
var
  i: Integer;
begin
  dlgProgress := TdlgProgress.Create(nil);
  try
    dlgProgress.Show;
    dlgProgress.Title := rsImportAudiosTitle;
    dlgProgress.Text := rsProgressPreparing;
    dlgProgress.Max := DropAudios.Files.Count;
    stopProcess := False;
    Application.ProcessMessages;
    if not DMM.sqlTrans.Active then
      DMM.sqlCon.StartTransaction;
    try
      for i := 0 to DropAudios.Files.Count - 1 do
      begin
        dlgProgress.Text := Format(rsProgressImportAudios, [i + 1, DropAudios.Files.Count]);

        if (ExtractFileExt(DropAudios.Files[i]) in SupportedAudios) then
          AddAudio(qAudios, DropAudios.Files[i]);
        { #todo : Better treatment of not supported audio files }

        dlgProgress.Position := i + 1;
        Application.ProcessMessages;
        if stopProcess then
          Break;
      end;
      if stopProcess then
        DMM.sqlTrans.RollbackRetaining
      else
        DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
    dlgProgress.Text := rsProgressFinishing;
    dlgProgress.Position := DropAudios.Files.Count;
    Application.ProcessMessages;
    stopProcess := False;
  finally
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;

  // Reject drop if it is a move operation
  //if (Effect = DROPEFFECT_MOVE) then
  //  Effect := DROPEFFECT_NONE;
end;

procedure TfrmCustomGrid.DropDocsDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint;
  var Effect: Longint);
var
  i: Integer;
begin
  dlgProgress := TdlgProgress.Create(nil);
  try
    dlgProgress.Show;
    dlgProgress.Title := rsImportDocsTitle;
    dlgProgress.Text := rsProgressPreparing;
    dlgProgress.Max := DropDocs.Files.Count;
    stopProcess := False;
    Application.ProcessMessages;
    if not DMM.sqlTrans.Active then
      DMM.sqlCon.StartTransaction;
    try
      for i := 0 to DropDocs.Files.Count - 1 do
      begin
        dlgProgress.Text := Format(rsProgressImportDocs, [i + 1, DropDocs.Files.Count]);

        AddDocument(qDocs, DropDocs.Files[i]);

        dlgProgress.Position := i + 1;
        Application.ProcessMessages;
        if stopProcess then
          Break;
      end;
      if stopProcess then
        DMM.sqlTrans.RollbackRetaining
      else
        DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
    dlgProgress.Text := rsProgressFinishing;
    dlgProgress.Position := DropDocs.Files.Count;
    Application.ProcessMessages;
    stopProcess := False;
  finally
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;

  // Reject drop if it is a move operation
  //if (Effect = DROPEFFECT_MOVE) then
  //  Effect := DROPEFFECT_NONE;
end;

procedure TfrmCustomGrid.DropImagesDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint;
  var Effect: Longint);
const
  SupportedImages: array of String = ('.png','.xpm','.bmp','.jpeg','.jpg','.jpe','.jfif','.tif','.tiff','.pbm','.pgm','.ppm');
var
  i: Integer;
begin
  dlgProgress := TdlgProgress.Create(nil);
  try
    dlgProgress.Show;
    dlgProgress.Title := rsImportImagesTitle;
    dlgProgress.Text := rsProgressPreparing;
    dlgProgress.Max := DropImages.Files.Count;
    stopProcess := False;
    Application.ProcessMessages;
    if not DMM.sqlTrans.Active then
      DMM.sqlCon.StartTransaction;
    try
      for i := 0 to DropImages.Files.Count - 1 do
      begin
        dlgProgress.Text := Format(rsProgressImportImages, [i + 1, DropImages.Files.Count]);

        if (ExtractFileExt(DropImages.Files[i]) in SupportedImages) then
          AddImage(qImages, tbImages, COL_IMAGE_FILENAME, COL_IMAGE_THUMBNAIL, DropImages.Files[i]);
        { #todo : Better treatment of not supported image files }

        dlgProgress.Position := i + 1;
        Application.ProcessMessages;
        if stopProcess then
          Break;
      end;
      if stopProcess then
        DMM.sqlTrans.RollbackRetaining
      else
        DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
    dlgProgress.Text := rsProgressFinishing;
    dlgProgress.Position := DropImages.Files.Count;
    Application.ProcessMessages;
    stopProcess := False;
  finally
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;

  // Reject drop if it is a move operation
  //if (Effect = DROPEFFECT_MOVE) then
  //  Effect := DROPEFFECT_NONE;
end;

procedure TfrmCustomGrid.dsAudiosDataChange(Sender: TObject; Field: TField);
begin
  UpdateAudioButtons(qAudios);
  {$IFDEF DEBUG}
  LogDebug('Param: ' + qAudios.Params[0].Name + ' ' + qAudios.Params[0].AsString + '; Count: ' + IntToStr(qAudios.RecordCount));
  {$ENDIF}
end;

procedure TfrmCustomGrid.dsAudiosStateChange(Sender: TObject);
begin
  UpdateAudioButtons(qAudios);
end;

procedure TfrmCustomGrid.dsDocsDataChange(Sender: TObject; Field: TField);
begin
  UpdateDocButtons(qDocs);
  {$IFDEF DEBUG}
  LogDebug('Param: ' + qDocs.Params[0].Name + ' ' + qDocs.Params[0].AsString + '; Count: ' + IntToStr(qDocs.RecordCount));
  {$ENDIF}
end;

procedure TfrmCustomGrid.dsDocsStateChange(Sender: TObject);
begin
  UpdateDocButtons(qDocs);
end;

procedure TfrmCustomGrid.dsImagesDataChange(Sender: TObject; Field: TField);
begin
  UpdateImageButtons(qImages);
  {$IFDEF DEBUG}
  LogDebug('Param: ' + qImages.Params[0].Name + ' ' + qImages.Params[0].AsString + '; Count: ' + IntToStr(qImages.RecordCount));
  {$ENDIF}
end;

procedure TfrmCustomGrid.dsImagesStateChange(Sender: TObject);
begin
  UpdateImageButtons(qImages);
end;

procedure TfrmCustomGrid.dsLink1DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;

  if nbChilds.PageIndex = 0 then
  begin
    TimerChildUpdate.Enabled := False;
    TimerChildUpdate.Enabled := True;
  end;
end;

procedure TfrmCustomGrid.dsLink1StateChange(Sender: TObject);
begin
  if Assigned(dsLink1.DataSet) and (nbChilds.PageIndex = 0) then
    UpdateChildButtons(dsLink1.DataSet);
end;

procedure TfrmCustomGrid.dsLink2DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;

  if nbChilds.PageIndex = 1 then
  begin
    TimerChildUpdate.Enabled := False;
    TimerChildUpdate.Enabled := True;
  end;
end;

procedure TfrmCustomGrid.dsLink2StateChange(Sender: TObject);
begin
  if Assigned(dsLink2.DataSet) and (nbChilds.PageIndex = 1) then
    UpdateChildButtons(dsLink2.DataSet);
end;

procedure TfrmCustomGrid.dsLink3DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;

  if nbChilds.PageIndex = 2 then
  begin
    TimerChildUpdate.Enabled := False;
    TimerChildUpdate.Enabled := True;
  end;
end;

procedure TfrmCustomGrid.dsLink3StateChange(Sender: TObject);
begin
  if Assigned(dsLink3.DataSet) and (nbChilds.PageIndex = 2) then
    UpdateChildButtons(dsLink3.DataSet);
end;

procedure TfrmCustomGrid.dsLink4DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;
  if FChildTable = tbProjectBudgets then
    UpdateChildRightPanel;

  if nbChilds.PageIndex = 3 then
  begin
    TimerChildUpdate.Enabled := False;
    TimerChildUpdate.Enabled := True;
  end;
end;

procedure TfrmCustomGrid.dsLink4StateChange(Sender: TObject);
begin
  if Assigned(dsLink4.DataSet) and (nbChilds.PageIndex = 3) then
    UpdateChildButtons(dsLink4.DataSet);
end;

procedure TfrmCustomGrid.dsLink5DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;

  if nbChilds.PageIndex = 4 then
  begin
    TimerChildUpdate.Enabled := False;
    TimerChildUpdate.Enabled := True;
  end;
end;

procedure TfrmCustomGrid.dsLink5StateChange(Sender: TObject);
begin
  if Assigned(dsLink5.DataSet) and (nbChilds.PageIndex = 4) then
    UpdateChildButtons(dsLink5.DataSet);
end;

procedure TfrmCustomGrid.dsLink6DataChange(Sender: TObject; Field: TField);
begin
  UpdateChildCount;

  if nbChilds.PageIndex = 5 then
  begin
    TimerChildUpdate.Enabled := False;
    TimerChildUpdate.Enabled := True;
  end;
end;

procedure TfrmCustomGrid.dsLink6StateChange(Sender: TObject);
begin
  if Assigned(dsLink6.DataSet) and (nbChilds.PageIndex = 5) then
    UpdateChildButtons(dsLink6.DataSet);
end;

procedure TfrmCustomGrid.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  LoadRecordRow;

  if mapGeo.Active then
    RefreshMap;

  UpdateChildBar;
  UpdateChildRightPanel;

  TimerRecordUpdate.Enabled := False;
  TimerRecordUpdate.Enabled := True;
end;

procedure TfrmCustomGrid.dsLinkStateChange(Sender: TObject);
begin
  if Assigned(dsLink.DataSet) then
    UpdateButtons(dsLink.DataSet);

  pEmptyQuery.Visible := (dsLink.DataSet.RecordCount = 0);

  UpdateChildBar;
end;

procedure TfrmCustomGrid.dsRecycleStateChange(Sender: TObject);
begin
  UpdateRecycleButtons(qRecycle);
end;

procedure TfrmCustomGrid.eAddChildClick(Sender: TObject);
begin
  QuickAddChild;
end;

procedure TfrmCustomGrid.eAddChildKeyPress(Sender: TObject; var Key: char);
begin
  QuickAddChild(Key);
  Key := #0;
end;

procedure TfrmCustomGrid.eCycleCodeFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  MoltCycleDialog(eCycleCodeFilter);
end;

procedure TfrmCustomGrid.eCycleCodeFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eCycleCodeFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eEggFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbEggs, eEggFilter, FEggKeyFilter);
end;

procedure TfrmCustomGrid.eEggFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eExpeditionFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbExpeditions, eExpeditionFilter, FExpeditionKeyFilter);
end;

procedure TfrmCustomGrid.eExpeditionFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eHowAgedFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  HowAgedDialog(eHowAgedFilter);
end;

procedure TfrmCustomGrid.eHowAgedFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eHowAgedFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eHowSexedFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  HowAgedDialog(eHowAgedFilter);
end;

procedure TfrmCustomGrid.eHowSexedFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eHowSexedFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eIndividualFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbIndividuals, eIndividualFilter, FIndividualKeyFilter);
end;

procedure TfrmCustomGrid.eIndividualFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eInstitutionFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbInstitutions, eInstitutionFilter, FInstitutionKeyFilter);
end;

procedure TfrmCustomGrid.eInstitutionFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eMethodFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbMethods, eMethodFilter, FMethodKeyFilter);
end;

procedure TfrmCustomGrid.eMethodFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eMoltLimitsFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  MoltLimitsDialog(eMoltLimitsFilter);
end;

procedure TfrmCustomGrid.eMoltLimitsFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eMoltLimitsFilter.Clear;
    Key := #0;
  end;
  //{ <ENTER/RETURN> key }
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eNestFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbNests, eNestFilter, FNestKeyFilter);
end;

procedure TfrmCustomGrid.eNestFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.ePersonFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbPeople, ePersonFilter, FPersonKeyFilter);
end;

procedure TfrmCustomGrid.ePersonFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.ePlantFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbBotanicTaxa, ePlantFilter, FPlantKeyFilter);
end;

procedure TfrmCustomGrid.ePlantFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eProjectFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbProjects, eProjectFilter, FProjectKeyFilter);
end;

procedure TfrmCustomGrid.eProjectFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eSamplingPlotFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbSamplingPlots, eSamplingPlotFilter, FSamplingPlotKeyFilter);
end;

procedure TfrmCustomGrid.eSamplingPlotFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
    Exit;

  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbSamplingPlots, eSamplingPlotFilter, FSamplingPlotKeyFilter, Key);
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.eSurveyFilterButtonClick(Sender: TObject);
begin
  if not FCanToggle then
    Exit;

  FindDlg(tbSurveys, eSurveyFilter, FSurveyKeyFilter);
end;

procedure TfrmCustomGrid.eSurveyFilterKeyPress(Sender: TObject; var Key: char);
begin
  if not FCanToggle then
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
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TfrmCustomGrid.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TimerChildUpdate.Enabled := False;
  TimerRecordUpdate.Enabled := False;
  TimerUpdate.Enabled := False;
  //TimerFind.Enabled := False;

  // Save the columns layout
  SaveColumnsConfig;

  // Close the datasets
  if qRecycle.Active then
    qRecycle.Close;

  if Assigned(dsLink6.DataSet) then
    dsLink6.DataSet.Close;
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

  LogEvent(leaClose, Caption);

  CloseAction := caFree;
end;

procedure TfrmCustomGrid.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { If editing when isClosing, ask what the user want to do }
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
  FCanToggle := False;
  isFiltered := False;

  oldSidePanel := False;
  oldSideIndex := -1;

  // Initialize the filter trees
  tvTaxaFilter.NodeDataSize := SizeOf(PTaxonNodeData);
  tvSiteFilter.NodeDataSize := SizeOf(PSiteNodeData);
  tvDateFilter.NodeDataSize := SizeOf(PDateNodeData);

  // Initialize the child tabs
  panelTabs := specialize TFPGList<TCustomPanelTab>.Create;

  //cellMemo.Tag := -1;

  // Open reports data module
  if not Assigned(DMR) then
    DMR := TDMR.Create(nil);
end;

procedure TfrmCustomGrid.FormDestroy(Sender: TObject);
var
  PanelTab: TCustomPanelTab;
begin
  //if Assigned(DMI) then
  //  FreeAndNil(DMI);
  //if Assigned(DMB) then
  //  FreeAndNil(DMB);

  // Destroy the child tabs
  for PanelTab in panelTabs do
    PanelTab.Free;
  panelTabs.Free;

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
  pSide.Width := Round((ClientWidth - pSideToolbar.Width) * FSidePanelFactor);
end;

procedure TfrmCustomGrid.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FSearch := TCustomSearch.Create(FTableType);

  mapGeo.CachePath := IncludeTrailingPathDelimiter(ConcatPaths([AppDataDir, 'map-cache']));

  { Resize panels }
  pSide.Visible := False;
  scrollFilter.VertScrollBar.Position := 0;
  FSidePanelFactor := pSide.Width / (ClientWidth - pSideToolbar.Width);
  FChildPanelFactor := 0.4;
  pChild.Height := Round((pClient.Height - SplitChild.Height) * FChildPanelFactor);
  UpdateRowHeights;
  Application.ProcessMessages;

  { Load datasources }
  SetGridAndChild;

  // Use a timer to load the rest to speed up opening
  TimerOpen.Enabled := True;
  TimerUpdate.Enabled := True;
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BAND_SIZE, 'Band size', sdtText,
      crEqual, False, cbBandSizeFilter.Text));
  end;

  if cbBandStatusFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BAND_STATUS, 'Band status', sdtText,
      crEqual, False, BandStatus[cbBandStatusFilter.ItemIndex - 1]));
  end;

  if cbBandTypeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BAND_TYPE, 'Band type', sdtText,
      crEqual, False, BandTypes[cbBandTypeFilter.ItemIndex - 1]));
  end;

  if cbBandSourceFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BAND_SOURCE, 'Band source', sdtText,
      crEqual, False, BandSources[cbBandSourceFilter.ItemIndex - 1]));
  end;

  if rbReportedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BAND_REPORTED, 'Band reported', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbReportedNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BAND_REPORTED, 'Band reported', sdtBoolean,
      crEqual, False, '0'));
  end;

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if FInstitutionKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPLIER_ID, 'Supplier', sdtInteger,
      crEqual, False, IntToStr(FInstitutionKeyFilter)));
  end;

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetBotanicTaxaFilters;
var
  sf, cc, i: Integer;
begin
  cc := 0;
  for i := 0 to clbTaxonRanksFilter.Count - 1 do
    if clbTaxonRanksFilter.Checked[i] then
      Inc(cc);
  if cc > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    for i := 0 to clbTaxonRanksFilter.Count - 1 do
      if clbTaxonRanksFilter.Checked[i] then
        FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create(COL_RANK_ID, 'Rank', sdtInteger,
          crEqual, False, IntToStr(GetKey('taxon_ranks', COL_RANK_ID, COL_RANK_NAME, clbTaxonRanksFilter.Items[i]))));
  end;

  if rbIsSynonymYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, 'Valid name', sdtInteger,
      crNotEqual, False, '0'));
  end;
  if rbIsSynonymNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, 'Valid name', sdtInteger,
      crEqual, False, '0'));
  end;
  //if rbHasSynonymsYes.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;
  //if rbHasSynonymsNo.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crEqual, False, '0'));
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURE_TYPE, 'Type', sdtText,
      crEqual, False, CaptureType[cbCaptureTypeFilter.ItemIndex - 1]));
  end;

  if cbCaptureStatusFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUBJECT_STATUS, 'Status', sdtText,
      crEqual, False, CaptureStatus[cbCaptureStatusFilter.ItemIndex - 1]));
  end;

  if cbAgeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUBJECT_AGE, 'Age', sdtText,
      crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
  end;
  if eHowAgedFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HOW_AGED, 'How was aged', sdtText,
      crLike, False, eHowAgedFilter.Text));
  end;
  if cbSexFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUBJECT_SEX, 'Sex', sdtText,
      crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
  end;
  if eHowSexedFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HOW_SEXED, 'How was sexed', sdtText,
      crLike, False, eHowSexedFilter.Text));
  end;

  if cbCloacalProtuberanceFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CLOACAL_PROTUBERANCE, 'Cloacal protuberance', sdtText,
      crEqual, False, cbCloacalProtuberanceFilter.Text));
  end;
  if cbBroodPatchFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BROOD_PATCH, 'Brood patch', sdtText,
      crEqual, False, cbBroodPatchFilter.Text));
  end;

  if cbFatFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FAT, 'Subcutaneous fat', sdtText,
      crEqual, False, cbFatFilter.Text));
  end;

  if cbBodyMoltFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BODY_MOLT, 'Body molt', sdtText,
      crEqual, False, cbBodyMoltFilter.Text));
  end;
  if cbFFMoltFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FLIGHT_FEATHERS_MOLT, 'Flight feathers molt', sdtText,
      crEqual, False, cbFFMoltFilter.Text));
  end;
  if cbFFWearFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FLIGHT_FEATHERS_WEAR, 'Flight feathers wear', sdtText,
      crEqual, False, cbFFWearFilter.Text));
  end;
  if eMoltLimitsFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_MOLT_LIMITS, 'Molt limits', sdtText,
      crLike, False, eMoltLimitsFilter.Text));
  end;
  if eCycleCodeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CYCLE_CODE, 'Molt cycle', sdtText,
      crLike, False, eCycleCodeFilter.Text));
  end;

  if cbSkullOssificationFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SKULL_OSSIFICATION, 'Skull ossification', sdtText,
      crEqual, False, cbSkullOssificationFilter.Text));
  end;

  if FPersonKeyFilter > 0 then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURE_TIME, 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURE_TIME, 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FSurveyKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SURVEY_ID, 'Survey', sdtInteger,
      crEqual, False, IntToStr(FSurveyKeyFilter)));
  end;
  if FMethodKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_METHOD_ID, 'Method', sdtInteger,
      crEqual, False, IntToStr(FMethodKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if rbNeedsReviewYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEEDS_REVIEW, 'Needs review', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbNeedsReviewNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEEDS_REVIEW, 'Needs review', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbEscapedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_ESCAPED, 'Escaped', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbEscapedNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_ESCAPED, 'Escaped', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbPhilornisYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PHILORNIS_LARVAE_TALLY, '# Philornis larvae', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbPhilornisNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PHILORNIS_LARVAE_TALLY, '# Philornis larvae', sdtInteger,
      crEqual, False, '0'));
  end;

  if rbReplacedBandYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, 'Removed band', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbReplacedBandNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, 'Removed band', sdtInteger,
      crEqual, False, '0'));
  end;
end;

function TfrmCustomGrid.GetChildDataSet: TDataSet;
begin
  Result := nil;

  case FTableType of
    tbSamplingPlots:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
      end;
    tbProjects:
      case nbChilds.PageIndex of
        0: Result := dsLink1.DataSet;
        1: Result := dsLink2.DataSet;
        2: Result := dsLink3.DataSet;
        3: Result := dsLink4.DataSet;
        4: Result := dsLink5.DataSet;
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
        5: Result := dsLink6.DataSet;
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

procedure TfrmCustomGrid.GetColumns;
var
  i: Integer;
begin
  //LoadColumnsConfig;

  gridColumns.RowCount := dsLink.DataSet.Fields.Count + 1;

  for i := 0 to dsLink.DataSet.Fields.Count - 1 do
  begin
    gridColumns.Cells[2, i + 1] := dsLink.DataSet.Fields[i].DisplayName;
    gridColumns.Cells[3, i + 1] := IntToStr(dsLink.DataSet.Fields[i].DisplayWidth);
    if dsLink.DataSet.Fields[i].Visible then
      gridColumns.Cells[1, i + 1] := '1'
    else
      gridColumns.Cells[1, i + 1] := '0';
  end;

  gridColumns.ColWidths[0] := 40;
end;

procedure TfrmCustomGrid.GetEggFilters;
const
  EggShapes: array of String = ('S', 'E', 'O', 'P', 'C', 'B', 'Y', 'L', 'U');
  EggPatterns: array of String = ('P', 'B', 'S', 'T', 'W', 'PS', 'BS', 'U');
  EggTextures: array of String = ('C', 'S', 'G', 'P', 'U');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters, 'z.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if cbEggShapeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_SHAPE, 'Shape', sdtText,
      crEqual, False, EggShapes[cbEggShapeFilter.ItemIndex - 1]));
  end;
  if cbEggPatternFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGGSHELL_PATTERN, 'Pattern', sdtText,
      crEqual, False, EggPatterns[cbEggPatternFilter.ItemIndex - 1]));
  end;
  if cbEggTextureFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGGSHELL_TEXTURE, 'Texture', sdtText,
      crEqual, False, EggTextures[cbEggTextureFilter.ItemIndex - 1]));
  end;

  if rbHatchedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_HATCHED, 'Hatched', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbHatchedNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_HATCHED, 'Hatched', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetExpeditionFilters;
var
  sf: Integer;
begin
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetFeatherFilters;
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters, 'z.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'g.');

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLE_TIME, 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLE_TIME, 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetFilters;
var
  sf: Integer;
begin
  if not FCanToggle then
    Exit;

  FCanToggle := False;

  if (rbMarkedYes.Checked) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_MARKED_STATUS, 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (rbMarkedNo.Checked) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_MARKED_STATUS, 'Marked', sdtBoolean,
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
    tbSamplingPlots: GetSamplingPlotFilters;
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
    tbFeathers:      GetFeatherFilters;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  isFiltered := FSearch.QuickFilters.Count > 0;
  FCanToggle := True;
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SITE_RANK, 'Site rank', sdtText,
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
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters, 'z.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbSexFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_SEX, 'Sex', sdtText,
      crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
  end;
  if cbAgeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_AGE, 'Age', sdtText,
      crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
  end;

  if rbWithColorBandsYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_RIGHT_TARSUS, 'Right tarsus', sdtText,
      crNotEqual, False, ''));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_LEFT_TARSUS, 'Left tarsus', sdtText,
      crNotEqual, False, ''));
  end;
  if rbWithColorBandsNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_RIGHT_TARSUS, 'Right tarsus', sdtText,
      crEqual, False, ''));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_LEFT_TARSUS, 'Left tarsus', sdtText,
      crEqual, False, ''));
  end;

  if rbWithRecapturesYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURES_TALLY, 'Captures', sdtInteger,
      crMoreThan, True, '2'));
  end;
  if rbWithRecapturesNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURES_TALLY, 'Captures', sdtInteger,
      crLessThan, True, '1'));
  end;

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FATHER_ID, 'Father', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_MOTHER_ID, 'Mother', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if rbReplacedBandYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, 'Removed band', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbReplacedBandNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, 'Removed band', sdtInteger,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetInstitutionFilters;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'it.');
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
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters, 'z.');
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'g.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbNestFateFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_FATE, 'Nest fate', sdtText,
      crEqual, False, NestFate[cbNestFateFilter.ItemIndex - 1]));
  end;
  if cbNestSupportFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPORT_TYPE, 'Support type', sdtText,
      crEqual, False, NestSupport[cbNestSupportFilter.ItemIndex - 1]));
  end;

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;

  if FPlantKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_1_ID, 'Support plant 1', sdtInteger,
      crEqual, False, IntToStr(FPlantKeyFilter)));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_2_ID, 'Support plant 2', sdtInteger,
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_STATUS, 'Nest status', sdtText,
      crEqual, False, NestStatus[cbNestStatusFilter.ItemIndex - 1]));
  end;
  if cbNestStageFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_STAGE, 'Nest stage', sdtText,
      crEqual, False, NestStages[cbNestStageFilter.ItemIndex - 1]));
  end;

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REVISION_TIME, 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REVISION_TIME, 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;

  if rbNidoparasiteYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_ID, 'Nidoparasite', sdtInteger,
      crMoreThan, False, '1'));
  end;
  if rbNidoparasiteNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_ID, 'Nidoparasite', sdtInteger,
      crEqual, False, '0'));
  end;

  if rbPhilornisYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HAVE_PHILORNIS_LARVAE, 'Philornis larvae', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbPhilornisNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HAVE_PHILORNIS_LARVAE, 'Philornis larvae', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetSamplingPlotFilters;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'g.');
end;

procedure TfrmCustomGrid.GetPeopleFilters;
var
  sf: Integer;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'p.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if FInstitutionKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INSTITUTION_ID, 'Institution', sdtInteger,
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;

  if cbPermitTypeFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PERMIT_TYPE, 'Permit type', sdtText,
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
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters, 'z.');
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'g.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if ePersonFilter.Text <> EmptyStr then
    PersonFilterToSearch(FTableType, FSearch.QuickFilters, FPersonKeyFilter);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, 'Time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
    else
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, 'Time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
  end;

  if FSurveyKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SURVEY_ID, 'Survey', sdtInteger,
      crEqual, False, IntToStr(FSurveyKeyFilter)));
  end;
  if FMethodKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_METHOD_ID, 'Method', sdtInteger,
      crEqual, False, IntToStr(FMethodKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;

  if rbRecordInEbirdYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EBIRD_AVAILABLE, 'Record is on eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbRecordInEbirdNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EBIRD_AVAILABLE, 'Record is on eBird', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbOutOfSampleYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NOT_SURVEYING, 'Out of sample', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbOutOfSampleNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NOT_SURVEYING, 'Out of sample', sdtBoolean,
      crEqual, False, '0'));
  end;
end;

procedure TfrmCustomGrid.GetSpecimenFilters;
const
  SampleTypes: array of String = ('WS', 'PS', 'N', 'B', 'E', 'P', 'F', 'BS', 'C', 'S', 'T', 'D', 'R');
var
  sf: Integer;
begin
  TaxonFilterToSearch(tvTaxaFilter, FSearch.QuickFilters, 'z.');
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'g.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if cbMaterialFilter.ItemIndex > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLE_TYPE, 'Sample type', sdtText,
      crEqual, False, SampleTypes[cbMaterialFilter.ItemIndex - 1]));
  end;

  if FNestKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, 'Nest', sdtInteger,
      crEqual, False, IntToStr(FNestKeyFilter)));
  end;
  if FEggKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_ID, 'Egg', sdtInteger,
      crEqual, False, IntToStr(FEggKeyFilter)));
  end;

  if FIndividualKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, 'Individual', sdtInteger,
      crEqual, False, IntToStr(FIndividualKeyFilter)));
  end;
end;

procedure TfrmCustomGrid.GetSurveyFilters;
var
  sf: Integer;
begin
  SiteFilterToSearch(tvSiteFilter, FSearch.QuickFilters, 'gl.');
  DateFilterToSearch(FTableType, tvDateFilter, FSearch.QuickFilters);

  if eStartTimeFilter.Text <> EmptyStr then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    if eEndTimeFilter.Text <> EmptyStr then
    begin
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_START_TIME, 'Start time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)));
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_END_TIME, 'End time', sdtTime,
        crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)));
    end
    else
    begin
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_START_TIME, 'Start time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
      FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_END_TIME, 'End time', sdtTime,
        crEqual, False, QuotedStr(eStartTimeFilter.Text)));
    end;
  end;

  if FMethodKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_METHOD_ID, 'Method', sdtInteger,
      crEqual, False, IntToStr(FMethodKeyFilter)));
  end;

  if FProjectKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, 'Project', sdtInteger,
      crEqual, False, IntToStr(FProjectKeyFilter)));
  end;

  if FExpeditionKeyFilter > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EXPEDITION_ID, 'Expedition', sdtInteger,
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

  if tsTaxonomyClements.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyIoc.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyCbro.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
      crEqual, False, '1'));
  end;

  if rbExtinctYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EXTINCT, 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbExtinctNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EXTINCT, 'Extinct', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbIsSynonymYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, 'Valid name', sdtInteger,
      crNotEqual, False, '0'));
  end;
  if rbIsSynonymNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, 'Valid name', sdtInteger,
      crEqual, False, '0'));
  end;

  //if rbHasSynonymsYes.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;
  //if rbHasSynonymsNo.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crEqual, False, '0'));
  //end;
end;

procedure TfrmCustomGrid.gridAudiosDblClick(Sender: TObject);
begin
  if sbPlayAudio.Enabled then
    sbPlayAudioClick(nil);
end;

procedure TfrmCustomGrid.gridChild1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if MousePos.Y < TDBGrid(Sender).DefaultRowHeight then
  begin
    //pmColumn.PopUp;
    Handled := True;
  end;
end;

procedure TfrmCustomGrid.gridChild1DblClick(Sender: TObject);
begin
  if sbEditChild.Enabled then
    sbEditChildClick(Sender);
end;

procedure TfrmCustomGrid.gridColumnsCheckboxToggled(Sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
  if not FCanToggle then
    Exit;

  if aCol = 1 then
  begin
    dsLink.DataSet.Fields[aRow - 1].Visible := aState = cbChecked;

    //SaveColumnsConfig;

    AddGridColumns(FTableType, DBG);
  end;
end;

procedure TfrmCustomGrid.gridDocsDblClick(Sender: TObject);
begin
  if sbOpenDoc.Enabled then
    sbOpenDocClick(nil);
end;

procedure TfrmCustomGrid.gridDocsDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  ImgList: TImageList;
  ImgIndex: Integer;
  ImgRect: TRect;
  ScaleFactor: Single;
  TargetWidth, TargetHeight: Int64;
begin
  ImgIndex := -1;

  if Column.FieldName = 'document_type' then
  begin
    // Get image index from cell value
    case Column.Field.AsString of
      'url': ImgIndex := 0;
      'doc': ImgIndex := 4;
      'spr': ImgIndex := 3;
      'prs': ImgIndex := 5;
      'pdf': ImgIndex := 2;
      'img': ImgIndex := 7;
      'aud': ImgIndex := 8;
      'vid': ImgIndex := 11;
      'cod': ImgIndex := 6;
      'db':  ImgIndex := 9;
      'gis': ImgIndex := 10;
      'oth': ImgIndex := 1;
    end;

    if IsDarkModeEnabled then
      ImgList := DMM.iFilesDark
    else
      ImgList := DMM.iFiles;

    if (ImgIndex >= 0) and (ImgIndex < ImgList.Count) then
    begin
      // Calc scale factor for the screen DPI
      ScaleFactor := Screen.PixelsPerInch / 96; // 96 DPI is the default

      // Calc scaled dimensions, keeping proportion
      TargetWidth := Round((Rect.Width - 4) * ScaleFactor); // 2 pixels of margin on both sides
      TargetHeight := Round((Rect.Height - 4) * ScaleFactor); // 2 pixels of margin on both sides

      // Center image within the cell
      ImgRect.Left := Rect.Left + (Rect.Width - TargetWidth) div 2;
      ImgRect.Top := Rect.Top + (Rect.Height - TargetHeight) div 2;
      ImgRect.Right := ImgRect.Left + TargetWidth;
      ImgRect.Bottom := ImgRect.Top + TargetHeight;

      // Draw image in the cell
      ImgList.DrawForPPI(gridDocs.Canvas, ImgRect.Left, ImgRect.Top, ImgIndex, 16, Screen.PixelsPerInch, ScaleFactor);
    end;
  end
  else
  begin
    // Draw cell normally
    gridDocs.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TfrmCustomGrid.iHeadersGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer;
  var AResultWidth: Integer);
begin
  AResultWidth := AImageWidth * APPI div 96;
end;

procedure TfrmCustomGrid.LoadColumnsConfig;
var
  ColsFile, ColsFolder: String;
begin
  {$IFDEF DEBUG}
  ColsFolder := 'debug_columns\';
  {$ELSE}
  ColsFolder := 'columns\';
  {$ENDIF}

  ColsFile := ConcatPaths([AppDataDir, ColsFolder, TABLE_NAMES[FTableType] + '_columns.json']);
  if not FileExists(ColsFile) then
  begin
    //GetColumns;
    Exit;
  end;

  LoadFieldsSettings(dsLink.dataset, ColsFile);

  LoadColumnsConfigGrid;
end;

procedure TfrmCustomGrid.LoadColumnsConfigGrid;
var
  i: Integer;
  Field: TField;
begin
  gridColumns.RowCount := dsLink.DataSet.Fields.Count + 1;
  for i := 0 to dsLink.DataSet.Fields.Count - 1 do
  begin
    Field := dsLink.DataSet.Fields[i];

    if Field.Visible then
      gridColumns.Cells[1, i+1] := '1'
    else
      gridColumns.Cells[1, i+1] := '0';

    // FieldName column (not visible)
    gridColumns.Cells[2, i+1] := Field.FieldName;

    // if DisplayLabel is empty, use FieldName
    if Trim(Field.DisplayLabel) = EmptyStr then
      gridColumns.Cells[3, i+1] := Field.FieldName
    else
      gridColumns.Cells[3, i+1] := Field.DisplayLabel;
  end;
  gridColumns.ColWidths[0] := 40;

  {$IFDEF DEBUG}
  LogDebug('gridColumns loaded: ' + IntToStr(gridColumns.RowCount));
  {$ENDIF}
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
  if isWorking then
    Exit;

  for i := 0 to (DBG.Columns.Count - 1) do
  begin
    if DBG.Columns[i].Visible then
    begin
      gridRecord.Cells[1, DBG.Columns.VisibleIndex(i)] := DBG.Columns[i].Field.AsString;
    end;
  end;
end;

procedure TfrmCustomGrid.mapGeoDrawGpsPoint(Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
const
  R = 8;
  Rs = 6;
var
  P: TPoint;
  ext: TSize;
begin
  P := TMapView(Sender).LonLatToScreen(APoint.RealPoint);
  if APoint.IdOwner = 0 then
  begin
    ADrawer.BrushColor := clRedFGDark;
    ADrawer.PenColor := clRedBGLight;
    ADrawer.Ellipse(P.X - R, P.Y - R, P.X + R, P.Y + R);
    P.Y := P.Y + R;
  end
  else
  begin
    ADrawer.BrushColor := clYellowFG4Dark;
    ADrawer.PenColor := clYellowBGLight;
    ADrawer.Ellipse(P.X - Rs, P.Y - Rs, P.X + Rs, P.Y + Rs);
    P.Y := P.Y + Rs;
  end;
  ADrawer.BrushStyle := bsSolid;
  ADrawer.PenWidth := 2;

  ext := ADrawer.TextExtent(APoint.Name);
  ADrawer.BrushColor := clWhite;
  ADrawer.BrushStyle := bsClear;
  ADrawer.TextOut(P.X - ext.CX div 2, P.Y + 5, APoint.Name);
end;

procedure TfrmCustomGrid.OpenAsync;
{$IFDEF DEBUG}
var
  Usage: TElapsedTimer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  Usage := TElapsedTimer.Create(Format('Show %s', [Caption]), 'load master table');
  {$ENDIF}

  { Load master grid columns }
  if Assigned(dsLink.DataSet) then
  begin
    {$IFDEF DEBUG}
    Usage.AddPart('load master grid columns');
    {$ENDIF}
    LoadColumnsConfig;
    AddGridColumns(FTableType, DBG);
    if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
      (dsLink.DataSet as TSQLQuery).ReadOnly := True;
    if not (dsLink.DataSet.Active) then
      dsLink.DataSet.Open;
    UpdateGridTitles(DBG, FSearch);
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
      tbIndividuals:    OpenIndividualChilds;
      tbNests:          OpenNestChilds;
      tbExpeditions:    OpenExpeditionChilds;
      tbSurveys:        OpenSurveyChilds;
      tbSpecimens:      OpenSpecimenChilds;
      tbSamplingPlots:  OpenSamplingPlotChilds;
      tbProjects:       OpenProjectChilds;
    end;
    Application.ProcessMessages;
  end;

  { Load side panels }
  {$IFDEF DEBUG}
  Usage.AddPart('load side panels');
  {$ENDIF}
  LoadRecordColumns;
  LoadRecordRow;
  UpdateFilterPanels;
  UpdateButtons(dsLink.DataSet);
  UpdateChildCount;
  if DBG.CanSetFocus then
    DBG.SetFocus;
  if gridColumns.RowCount <= 2 then
    LoadColumnsConfigGrid;
  SetImages;
  SetAudios;
  SetDocs;
  SetRecycle;
  FCanToggle := True;
  Application.ProcessMessages;

  dlgLoading.Hide;

  {$IFDEF DEBUG}
  Usage.StopTimer;
  FreeAndNil(Usage);
  {$ENDIF}
end;

procedure TfrmCustomGrid.OpenExpeditionChilds;
begin
  AddGridColumns(tbSurveys, gridChild1);
  dsLink1.DataSet.Open;
  if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
  begin
    (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.OpenIndividualChilds;
begin
  AddGridColumns(tbCaptures, gridChild1);
  dsLink1.DataSet.Open;
  AddGridColumns(tbFeathers, gridChild2);
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

procedure TfrmCustomGrid.OpenNestChilds;
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

procedure TfrmCustomGrid.OpenProjectChilds;
begin
  AddGridColumns(tbProjectTeams, gridChild1);
  dsLink1.DataSet.Open;
  AddGridColumns(tbProjectGoals, gridChild2);
  dsLink2.DataSet.Open;
  AddGridColumns(tbProjectChronograms, gridChild3);
  dsLink3.DataSet.Open;
  AddGridColumns(tbProjectBudgets, gridChild4);
  dsLink4.DataSet.Open;
  AddGridColumns(tbProjectExpenses, gridChild5);
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

procedure TfrmCustomGrid.OpenSamplingPlotChilds;
begin
  AddGridColumns(tbPermanentNets, gridChild1);
  dsLink1.DataSet.Open;
  if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
  begin
    (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.OpenSpecimenChilds;
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

procedure TfrmCustomGrid.OpenSurveyChilds;
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
  AddGridColumns(tbVegetation, gridChild6);
  dsLink6.DataSet.Open;
  if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
  begin
    (dsLink1.DataSet as TSQLQuery).ReadOnly := True;
    (dsLink2.DataSet as TSQLQuery).ReadOnly := True;
    (dsLink3.DataSet as TSQLQuery).ReadOnly := True;
    (dsLink4.DataSet as TSQLQuery).ReadOnly := True;
    (dsLink5.DataSet as TSQLQuery).ReadOnly := True;
    (dsLink6.DataSet as TSQLQuery).ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.pClientResize(Sender: TObject);
begin
  pChild.Height := Round((pClient.Height - SplitChild.Height) * FChildPanelFactor);
end;

procedure TfrmCustomGrid.pmAddDocumentClick(Sender: TObject);
var
  i: Integer;
begin
  DMM.OpenDocs.InitialDir := xSettings.DocumentsFolder;
  if DMM.OpenDocs.Execute then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsImportDocsTitle;
      dlgProgress.Text := rsProgressPreparing;
      dlgProgress.Max := DMM.OpenDocs.Files.Count;
      stopProcess := False;
      Application.ProcessMessages;
      if not DMM.sqlTrans.Active then
        DMM.sqlCon.StartTransaction;
      try
        for i := 0 to DMM.OpenDocs.Files.Count - 1 do
        begin
          dlgProgress.Text := Format(rsProgressImportDocs, [i + 1, DMM.OpenDocs.Files.Count]);

          AddDocument(qDocs, DMM.OpenDocs.Files[i]);

          dlgProgress.Position := i + 1;
          Application.ProcessMessages;
          if stopProcess then
            Break;
        end;
        if stopProcess then
          DMM.sqlTrans.RollbackRetaining
        else
          DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;
      dlgProgress.Text := rsProgressFinishing;
      dlgProgress.Position := DMM.OpenDocs.Files.Count;
      Application.ProcessMessages;
      stopProcess := False;
    finally
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure TfrmCustomGrid.pmAddLinkClick(Sender: TObject);
begin
  EditDocInfo(qDocs, dsLink.DataSet, FTableType, True);
end;

procedure TfrmCustomGrid.pmaRefreshAudiosClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  qAudios.Refresh;
end;

procedure TfrmCustomGrid.pmcColumnSortAscClick(Sender: TObject);
var
  Direction: TSortDirection;
  ACol, ARow: Longint;
  Column: TColumn;
  Grid: TDBGrid;
begin
  Grid := DBG;
  Grid.MouseToCell(pmColumn.PopupPoint.X, pmColumn.PopupPoint.Y, ACol, ARow);
  Column := Grid.Columns[ACol - 1];

  Direction := sdAscending;

  FSearch.SortFields.Clear;
  if not (pfInUpdate in Grid.DataSource.DataSet.FieldByName(Column.FieldName).ProviderFlags) then
    AddSortedField(Column.FieldName, Direction, '', True)
  else
    AddSortedField(Column.FieldName, Direction);
  Search(FSearchString);
end;

procedure TfrmCustomGrid.pmcColumnSortDescClick(Sender: TObject);
var
  Direction: TSortDirection;
  ACol, ARow: Longint;
  Column: TColumn;
  Grid: TDBGrid;
begin
  Grid := DBG;
  Grid.MouseToCell(pmColumn.PopupPoint.X, pmColumn.PopupPoint.Y, ACol, ARow);
  Column := Grid.Columns[ACol - 1];

  Direction := sdDescending;

  FSearch.SortFields.Clear;
  if not (pfInUpdate in Grid.DataSource.DataSet.FieldByName(Column.FieldName).ProviderFlags) then
    AddSortedField(Column.FieldName, Direction, '', True)
  else
    AddSortedField(Column.FieldName, Direction);
  Search(FSearchString);
end;

procedure TfrmCustomGrid.pmcHideColumnClick(Sender: TObject);
var
  ACol, ARow: Longint;
  Column: TColumn;
begin
  DBG.MouseToCell(pmColumn.PopupPoint.X, pmColumn.PopupPoint.Y, ACol, ARow);
  Column := DBG.Columns[ACol - 1];

  dsLink.DataSet.FieldByName(Column.FieldName).Visible := False;

  //GetColumns;
  AddGridColumns(FTableType, DBG);
end;

procedure TfrmCustomGrid.pmcNewBudgetItemClick(Sender: TObject);
begin
  EditProjectRubric(DMG.qProjectBudget, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, True);

  UpdateChildButtons(DMG.qProjectBudget);
end;

procedure TfrmCustomGrid.pmcNewCaptureClick(Sender: TObject);
begin
  case FTableType of
    tbIndividuals:
      EditCapture(DMI.qCaptures, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, 0, True);
    tbSurveys:
      EditCapture(DMS.qCaptures, 0, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, True);
  end;

  UpdateChildBar;
end;

procedure TfrmCustomGrid.pmcNewChronogramActivityClick(Sender: TObject);
begin
  EditProjectActivity(DMG.qProjectChronogram, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, 0, True);

  UpdateChildButtons(DMG.qProjectChronogram);
end;

procedure TfrmCustomGrid.pmcNewCollectorClick(Sender: TObject);
begin
  EditCollector(DMG.qSampleCollectors, dsLink.DataSet.FieldByName(COL_SPECIMEN_ID).AsInteger, True);

  UpdateChildButtons(DMG.qSampleCollectors);
end;

procedure TfrmCustomGrid.pmcNewEggClick(Sender: TObject);
begin
  EditEgg(DMB.qEggs, dsLink.DataSet.FieldByName(COL_NEST_ID).AsInteger, True);

  UpdateChildButtons(DMB.qEggs);
end;

procedure TfrmCustomGrid.pmcNewExpenseClick(Sender: TObject);
begin
  EditProjectExpense(DMG.qProjectExpenses, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, 0, True);

  UpdateChildButtons(DMG.qProjectExpenses);
end;

procedure TfrmCustomGrid.pmcNewExpenseFromRubricClick(Sender: TObject);
begin
  EditProjectExpense(DMG.qProjectExpenses, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger,
    dsLink4.DataSet.FieldByName(COL_BUDGET_ID).AsInteger, True);

  UpdateChildButtons(DMG.qProjectExpenses);
  UpdateChildRightPanel;
end;

procedure TfrmCustomGrid.pmcNewFeatherClick(Sender: TObject);
begin
  EditFeather(DMI.qFeathers, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, 0, 0, True);

  UpdateChildButtons(DMI.qFeathers);
end;

procedure TfrmCustomGrid.pmcNewMistnetClick(Sender: TObject);
begin
  EditNetEffort(DMS.qNetsEffort, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, True);

  UpdateChildButtons(DMS.qNetsEffort);
end;

procedure TfrmCustomGrid.pmcNewNestClick(Sender: TObject);
begin
  EditNest(DMI.qNests, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, True);

  UpdateChildButtons(DMI.qNests);
end;

procedure TfrmCustomGrid.pmcNewNestOwnerClick(Sender: TObject);
begin
  EditNestOwner(DMB.qNestOwners, dsLink.DataSet.FieldByName(COL_NEST_ID).AsInteger, True);

  UpdateChildButtons(DMB.qNestOwners);
end;

procedure TfrmCustomGrid.pmcNewNestRevisionClick(Sender: TObject);
begin
  EditNestRevision(DMB.qNestRevisions, dsLink.DataSet.FieldByName(COL_NEST_ID).AsInteger, True);

  UpdateChildButtons(DMB.qNestRevisions);
end;

procedure TfrmCustomGrid.pmcNewPermanentNetClick(Sender: TObject);
begin
  EditPermanentNet(DMG.qPermanentNets, dsLink.DataSet.FieldByName(COL_SAMPLING_PLOT_ID).AsInteger, True);

  UpdateChildButtons(DMG.qPermanentNets);
end;

procedure TfrmCustomGrid.pmcNewProjectActivityFromGoalClick(Sender: TObject);
begin
  EditProjectActivity(DMG.qProjectChronogram, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger,
    dsLink2.DataSet.FieldByName(COL_GOAL_ID).AsInteger, True);

  UpdateChildButtons(DMG.qProjectChronogram);
end;

procedure TfrmCustomGrid.pmcNewProjectGoalClick(Sender: TObject);
begin
  EditProjectGoal(DMG.qProjectGoals, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, True);

  UpdateChildButtons(DMG.qProjectGoals);
end;

procedure TfrmCustomGrid.pmcNewProjectMemberClick(Sender: TObject);
begin
  EditProjectMember(DMG.qProjectTeam, dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger, True);

  UpdateChildButtons(DMG.qProjectTeam);
end;

procedure TfrmCustomGrid.pmcNewSamplePrepClick(Sender: TObject);
begin
  EditSamplePrep(DMG.qSamplePreps, dsLink.DataSet.FieldByName(COL_SPECIMEN_ID).AsInteger, True);

  UpdateChildButtons(DMG.qSamplePreps);
end;

procedure TfrmCustomGrid.pmcNewSightingClick(Sender: TObject);
begin
  case FTableType of
    tbIndividuals:
      EditSighting(DMI.qSightings, 0, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, True);
    tbSurveys:
      EditSighting(DMS.qSightings, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, 0, True);
  end;

  UpdateChildBar;
end;

procedure TfrmCustomGrid.pmcNewSpecimenClick(Sender: TObject);
begin
  EditSpecimen(DMI.qSpecimens, dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger, True);
end;

procedure TfrmCustomGrid.pmcNewSurveyClick(Sender: TObject);
begin
  EditSurvey(DMS.qSurveys, dsLink.DataSet.FieldByName(COL_EXPEDITION_ID).AsInteger, True);

  UpdateChildButtons(DMS.qSurveys);
end;

procedure TfrmCustomGrid.pmcNewSurveyMemberClick(Sender: TObject);
begin
  EditSurveyMember(DMS.qSurveyTeam, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, True);

  UpdateChildButtons(DMS.qSurveyTeam);
end;

procedure TfrmCustomGrid.pmcNewVegetationClick(Sender: TObject);
begin
  EditVegetation(DMS.qVegetation, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, True);

  UpdateChildButtons(DMS.qVegetation);
end;

procedure TfrmCustomGrid.pmcNewWeatherLogClick(Sender: TObject);
begin
  EditWeatherLog(DMS.qWeatherLogs, dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger, True);

  UpdateChildButtons(DMS.qWeatherLogs);
end;

procedure TfrmCustomGrid.pmdRefreshDocsClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  qDocs.Refresh;
end;

procedure TfrmCustomGrid.pmGridChildPopup(Sender: TObject);
begin
  pmcNewProjectActivityFromGoal.Visible := (ChildTable = tbProjectGoals) and (dsLink2.DataSet.RecordCount > 0);
  pmcNewExpenseFromRubric.Visible := (ChildTable = tbProjectBudgets) and (dsLink4.DataSet.RecordCount > 0);
end;

procedure TfrmCustomGrid.pmiRefreshImagesClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  qImages.Refresh;
end;

procedure TfrmCustomGrid.pmmInvertMarkedClick(Sender: TObject);
var
  BM: TBookmark;
begin
  FCanToggle := False;

  with dsLink.DataSet do
  try
    BM := Bookmark;
    DisableControls;
    First;
    repeat
      Edit;
      FieldByName(COL_MARKED_STATUS).AsBoolean := not FieldByName(COL_MARKED_STATUS).AsBoolean;
      Post;
      Next;
    until Eof;
  finally
    EnableControls;
    Bookmark := BM;
    FCanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmMarkAllClick(Sender: TObject);
var
  BM: TBookmark;
begin
  FCanToggle := False;

  with dsLink.DataSet do
  try
    BM := Bookmark;
    DisableControls;
    First;
    repeat
      Edit;
      FieldByName(COL_MARKED_STATUS).AsBoolean := True;
      Post;
      Next;
    until Eof;
  finally
    EnableControls;
    Bookmark := BM;
    FCanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmMarkAllColumnsClick(Sender: TObject);
var
  i: Integer;
begin
  FCanToggle := False;

  try
    for i := 0 to dsLink.DataSet.Fields.Count - 1 do
    begin
      gridColumns.Cells[1, i+1] := '1';
      dsLink.DataSet.Fields[i].Visible := True;
    end;

    //GetColumns;
    AddGridColumns(FTableType, DBG);
  finally
    FCanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmUnmarkAllClick(Sender: TObject);
var
  BM: TBookmark;
begin
  FCanToggle := False;

  with dsLink.DataSet do
  try
    BM := Bookmark;
    DisableControls;
    First;
    repeat
      Edit;
      FieldByName(COL_MARKED_STATUS).AsBoolean := False;
      Post;
      Next;
    until Eof;
  finally
    EnableControls;
    Bookmark := BM;
    FCanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmUnmarkAllColumnsClick(Sender: TObject);
var
  i: Integer;
begin
  FCanToggle := False;

  try
    for i := 0 to dsLink.DataSet.Fields.Count - 1 do
    begin
      gridColumns.Cells[1, i+1] := '0';
      dsLink.DataSet.Fields[i].Visible := False;
    end;

    //GetColumns;
    AddGridColumns(FTableType, DBG);
  finally
    FCanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmpBandHistoryClick(Sender: TObject);
begin
  AbreForm(TdlgBandHistory, dlgBandHistory);
end;

procedure TfrmCustomGrid.pmpBandsBalanceClick(Sender: TObject);
begin
  AbreForm(TdlgBandsBalance, dlgBandsBalance);
end;

procedure TfrmCustomGrid.pmPrintBandsByCarrierClick(Sender: TObject);
begin
  DMR.qBands.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  // Reorder records
  if Pos('ORDER BY', DMR.qBands.SQL.Text) > 0 then
    DMR.qBands.SQL.Delete(DMR.qBands.SQL.Count - 1);
  DMR.qBands.SQL.Add('ORDER BY carrier_name ASC, b.band_size ASC, b.band_number ASC');

  PrintPreview(BANDS_BY_CARRIER_REPORT_FILE, DMR.dsBands);
end;

procedure TfrmCustomGrid.pmPrintBandsByStatusClick(Sender: TObject);
begin
  DMR.qBands.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  // Reorder records
  if Pos('ORDER BY', DMR.qBands.SQL.Text) > 0 then
    DMR.qBands.SQL.Delete(DMR.qBands.SQL.Count - 1);
  DMR.qBands.SQL.Add('ORDER BY b.band_status ASC, b.band_size ASC, b.band_number ASC');

  PrintPreview(BANDS_BY_STATUS_REPORT_FILE, DMR.dsBands);
end;

procedure TfrmCustomGrid.pmPrintBandsClick(Sender: TObject);
begin
  DMR.qBands.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(BANDS_REPORT_FILE, DMR.dsBands);
end;

procedure TfrmCustomGrid.pmPrintBandsWithHistoryClick(Sender: TObject);
begin
  DMR.qBands.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;
  DMR.qBandHistory.SQL.Text := DMG.qBandHistory.SQL.Text;

  PrintPreview(BANDS_HISTORY_REPORT_FILE, DMR.dsBands, DMR.dsBandHistory);
end;

procedure TfrmCustomGrid.pmPrintExpeditionsClick(Sender: TObject);
begin
  DMR.qExpeditions.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;
  DMR.qSurveys.SQL.Text := TSQLQuery(dsLink1.DataSet).SQL.Text;
  DMR.qSurveys.DataSource := DMR.dsExpeditions;

  PrintPreview(EXPEDITIONS_REPORT_FILE, DMR.dsExpeditions, DMR.dsSurveys);
end;

procedure TfrmCustomGrid.pmPrintGazetteerClick(Sender: TObject);
begin
  DMR.qGazetteer.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(GAZETTEER_REPORT_FILE, DMR.dsGazetteer);
end;

procedure TfrmCustomGrid.pmPrintGridClick(Sender: TObject);
begin
  PrintGrid.Caption := Caption;
  PrintGrid.PreviewReport;
end;

procedure TfrmCustomGrid.pmPrintInstitutionsClick(Sender: TObject);
begin
  DMR.qInstitutions.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(INSTITUTIONS_REPORT_FILE, DMR.dsInstitutions);
end;

procedure TfrmCustomGrid.pmPrintMethodsClick(Sender: TObject);
begin
  DMR.qMethods.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(METHODS_REPORT_FILE, DMR.dsMethods);
end;

procedure TfrmCustomGrid.pmPrintPermitsClick(Sender: TObject);
begin
  DMR.qPermits.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(PERMITS_REPORT_FILE, DMR.dsPermits);
end;

procedure TfrmCustomGrid.pmPrintProjectsClick(Sender: TObject);
begin
  DMR.qProjects.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;
  DMR.qProjectTeam.SQL.Text := TSQLQuery(dsLink1.DataSet).SQL.Text;

  PrintPreview(PROJECTS_REPORT_FILE, DMR.dsProjects, DMR.dsProjectTeam);
end;

procedure TfrmCustomGrid.pmPrintResearchersClick(Sender: TObject);
begin
  DMR.qPeople.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(PEOPLE_REPORT_FILE, DMR.dsPeople);
end;

procedure TfrmCustomGrid.pmPrintSamplingPlotsByLocalityClick(Sender: TObject);
begin
  DMR.qSamplingPlots.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  // Reorder records
  if Pos('ORDER BY', DMR.qSamplingPlots.SQL.Text) > 0 then
    DMR.qSamplingPlots.SQL.Delete(DMR.qSamplingPlots.SQL.Count - 1);
  DMR.qSamplingPlots.SQL.Add('ORDER BY locality_name ASC, pl.full_name ASC');

  PrintPreview(SAMPLING_PLOTS_BY_LOCALITY_REPORT_FILE, DMR.dsSamplingPlots);
end;

procedure TfrmCustomGrid.pmPrintSamplingPlotsClick(Sender: TObject);
begin
  DMR.qSamplingPlots.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(SAMPLING_PLOTS_REPORT_FILE, DMR.dsSamplingPlots);
end;

procedure TfrmCustomGrid.pmPrintSightingsByObserverClick(Sender: TObject);
var
  Qry, qObservers: TSQLQuery;
  aSurvey: Integer;
begin
  aSurvey := 0;
  // Show dialog to select a survey
  dlgSelectRecord := TdlgSelectRecord.Create(nil);
  with dlgSelectRecord do
  try
    PromptString := rsPromptSelectSurvey;
    if ShowModal = mrOK then
      aSurvey := RecordKey
    else
      Exit;
  finally
    FreeAndNil(dlgSelectRecord);
  end;

  Qry := TSQLQuery.Create(nil);
  qObservers := TSQLQuery.Create(nil);
  try
    Qry.SQLConnection := DMM.sqlCon;
    Qry.SQLTransaction := DMM.sqlTrans;
    qObservers.SQLConnection := DMM.sqlCon;
    qObservers.SQLTransaction := DMM.sqlTrans;

    // Load the list of distinct observers from sightings
    qObservers.SQL.Text := 'SELECT DISTINCT s.observer_id, p.acronym FROM sightings AS s ' +
      'LEFT JOIN people AS p ON s.observer_id = p.person_id ' +
      'WHERE (s.survey_id = :survey_id) and (s.observer_id NOT NULL);';
    qObservers.ParamByName('survey_id').AsInteger := aSurvey;
    qObservers.Open;

    // Check if there are observers in the list
    if qObservers.RecordCount = 0 then
    begin
      MsgDlg(rsTitleInformation, rsNothingToPrint, mtInformation);
      Exit;
    end;

    // Load the number of sightings per observer
    Qry.SQL.Add('SELECT z.full_name, ');
    qObservers.First;
    while not qObservers.EOF do
    begin
      Qry.SQL.Add('MAX(CASE WHEN s.observer_id = ''' + qObservers.FieldByName('observer_id').AsString +
        ''' THEN ''X'' ELSE '' '' END) AS ' + qObservers.FieldByName('acronym').AsString + ', ');
    end;
    Qry.SQL.Add('COUNT(DISTINCT s.observer_id) AS X_Count');
    Qry.SQL.Add('FROM sightings AS s');
    Qry.SQL.Add('LEFT JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
    Qry.SQL.Add('WHERE s.survey_id = :survey_id');
    Qry.SQL.Add('GROUP BY s.taxon_id;');

    Qry.Open;

    // Show the resulting list
    dlgExportPreview := TdlgExportPreview.Create(nil);
    with dlgExportPreview do
    try
      dlgExportPreview.dsLink.DataSet := Qry;
      ShowModal;
    finally
      FreeAndNil(dlgExportPreview);
    end;

    qObservers.Close;
    Qry.Close;
  finally
    FreeAndNil(qObservers);
    FreeAndNil(Qry);
  end;
end;

procedure TfrmCustomGrid.pmPrintSightingsClick(Sender: TObject);
begin
  DMR.qSightings.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;

  PrintPreview(SIGHTINGS_REPORT_FILE, DMR.dsSightings);
end;

procedure TfrmCustomGrid.pmPrintSpecimensClick(Sender: TObject);
begin
  DMR.qSpecimens.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;
  DMR.qSampleCollectors.SQL.Text := TSQLQuery(dsLink1.DataSet).SQL.Text;

  PrintPreview(SPECIMENS_REPORT_FILE, DMR.dsSpecimens, DMR.dsSampleCollectors);
end;

procedure TfrmCustomGrid.pmPrintSurveysClick(Sender: TObject);
begin
  DMR.qSurveys.SQL.Text := TSQLQuery(dsLink.DataSet).SQL.Text;
  DMR.qSurveys.DataSource := nil;

  PrintPreview(SURVEYS_REPORT_FILE, DMR.dsSurveys);
end;

procedure TfrmCustomGrid.pmpTransferBandsToClick(Sender: TObject);
var
  needsRefresh: Boolean;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    batchBandsTransfer := TbatchBandsTransfer.Create(nil);
    with batchBandsTransfer do
    try
      needsRefresh := ShowModal = mrOK;
    finally
      FreeAndNil(batchBandsTransfer);
    end;

    if needsRefresh then
    begin
      UpdateButtons(dsLink.DataSet);
      UpdateFilterPanels;
      UpdateChildRightPanel;
    end;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.pmrRefreshClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    if not dsRecycle.DataSet.Active then
      dsRecycle.DataSet.Open;
    dsRecycle.DataSet.Refresh;
    UpdateRecycleButtons(dsRecycle.DataSet);
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
  end;
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
    LoadTaxaTreeData(FTableType, tvTaxaFilter, 0)
  else
  if pmTree.PopupComponent = tvSiteFilter then
    LoadSiteTreeData(FTableType, tvSiteFilter, 4)
  else
  if pmTree.PopupComponent = tvDateFilter then
    LoadDateTreeData(FTableType, tvDateFilter);
end;

procedure TfrmCustomGrid.PrepareCanvasBands(var Column: TColumn; var sender: TObject);
begin
  if Column.FieldName = COL_BAND_SIZE then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end
  else
  if Column.FieldName = COL_BAND_STATUS then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
    case Column.Field.AsString of
      'U': // Used
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
      'D': // Available
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemNeutralFGLight;
        end;
      end;
      'R': // Removed
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemMediumBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemMediumFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemMediumBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemMediumFGLight;
        end;
      end;
      'Q': // Broken
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'P': // Lost
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
      'T': // Transfered
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clVioletBrand1Dark;
          TDBGrid(Sender).Canvas.Font.Color := clVioletFG1Dark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clVioletBrand1Light;
          TDBGrid(Sender).Canvas.Font.Color := clVioletFG2Light;
        end;
      end;
    end;
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasCaptures(var Column: TColumn; var sender: TObject);
var
  aTaxon: Integer;
begin
  aTaxon := 0;

  if (Column.FieldName = COL_TAXON_NAME) then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if (Column.FieldName = COL_CAPTURE_TYPE) then
  begin
    case Column.Field.AsString of
      'N':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clBlueBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clBlueBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryLight;
        end;
      end;
      'R', 'S':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
      'C':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
      'U':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BAND_NAME) then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'R') or
      (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'S') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
    end
    else
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end
  else
  if (Column.FieldName = COL_REMOVED_BAND_NAME) then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end;

  { Check if UseConditionalFormatting setting is enabled }
  if not xSettings.UseConditionalFormatting then
    Exit;

  { Paint the cell background red for invalid values }
  if (Column.FieldName = COL_CLOACAL_PROTUBERANCE) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, CLOACAL_PROTUBERANCE_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_BROOD_PATCH) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BROOD_PATCH_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FAT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FAT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_BODY_MOLT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BODY_MOLT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FLIGHT_FEATHERS_MOLT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FLIGHT_MOLT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FLIGHT_FEATHERS_WEAR) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FEATHER_WEAR_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_SKULL_OSSIFICATION) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, SKULL_OSSIFICATION_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end;

  { Check if ShowOutliersOnGrid setting is enabled }
  if not xSettings.ShowOutliersOnGrid then
    Exit;

  { Paint the cell background yellow for outliers }
  if (Column.FieldName = COL_RIGHT_WING_CHORD) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_FIRST_SECONDARY_CHORD) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TAIL_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TARSUS_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TARSUS_DIAMETER) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_WEIGHT) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_EXPOSED_CULMEN) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BILL_WIDTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BILL_HEIGHT) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_NOSTRIL_BILL_TIP) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_SKULL_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_KIPPS_INDEX) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end;

end;

procedure TfrmCustomGrid.PrepareCanvasEggs(var Column: TColumn; var sender: TObject);
begin
  if Column.FieldName = COL_EGG_SEQUENCE then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasExpeditions(var Column: TColumn; var sender: TObject);
begin
  if Column.FieldName = COL_START_DATE then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasFeathers(var Column: TColumn;
  var sender: TObject);
begin
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_SAMPLE_DATE then
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
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if (Column.FieldName = COL_INDIVIDUAL_SEX) then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
    case Column.Field.AsString of
      'U':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'M':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
      'F':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
    end;
  end
  else
  if Column.FieldName = COL_BAND_NAME then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end
  else
  if Column.FieldName = COL_REMOVED_BAND_NAME then
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
  if Column.FieldName = COL_ABBREVIATION then
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
  if (Column.FieldName = COL_TAXON_NAME) or
    (Column.FieldName = COL_SUPPORT_PLANT_1_NAME) or
    (Column.FieldName = COL_SUPPORT_PLANT_2_NAME) then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_NEST_FATE then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
    case Column.Field.AsString of
      'U':       // Unknown
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
        end;
      end;
      'P':       // Lost
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'S':       // Success
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
    end;
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasNestRevisions(var Column: TColumn; var sender: TObject);
begin
  if Column.FieldName = COL_NIDOPARASITE_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_NEST_STATUS then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
    case Column.Field.AsString of
      'U':       // Unknown
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
        end;
      end;
      'I':       // Inactive
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'A':       // Active
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
    end;
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasNetsEffort(var Column: TColumn; var sender: TObject);
begin
  if (Column.FieldName = COL_SAMPLE_DATE) or
    (Column.FieldName = COL_NET_NUMBER) then
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
  if Column.FieldName = COL_ABBREVIATION then
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
  if (Column.FieldName = COL_NET_NUMBER) then
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
  if (Column.FieldName = COL_EXPIRE_DATE) or
    (Column.FieldName = COL_PERMIT_NUMBER) then
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
  if (Column.FieldName = COL_START_DATE) or
    (Column.FieldName = COL_PROTOCOL_NUMBER) then
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
  if (Column.FieldName = COL_PREPARATION_DATE) or
    (Column.FieldName = COL_ACCESSION_NUMBER) then
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
  if Column.FieldName = COL_SIGHTING_DATE then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasSpecimens(var Column: TColumn; var sender: TObject);
begin
  if (Column.FieldName = COL_COLLECTION_DATE) or
    (Column.FieldName = COL_FIELD_NUMBER) then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

procedure TfrmCustomGrid.PrepareCanvasSurveys(var Column: TColumn; var sender: TObject);
begin
  if Column.FieldName = COL_SURVEY_DATE then
  begin
    {$IFDEF MSWINDOWS}
    TDBGrid(Sender).Canvas.Font.Name := 'Segoe UI Semibold';
    {$ELSE}
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
    {$ENDIF}
  end;
end;

procedure TfrmCustomGrid.qAudiosaudio_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  { #todo : Audio types GetText }
end;

procedure TfrmCustomGrid.qAudiosaudio_typeSetText(Sender: TField; const aText: string);
begin
  { #todo : Audio types SetText }
end;

procedure TfrmCustomGrid.qAudiosBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  //if not DataSet.FieldByName('taxon_id').IsNull then
  //  GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);
  //
  //if not DataSet.FieldByName('locality_id').IsNull then
  //  GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TfrmCustomGrid.qAudiosprecipitationGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  case Sender.AsString of
    'N': aText := rsPrecipitationNone;
    'F': aText := rsPrecipitationFog;
    'M': aText := rsPrecipitationMist;
    'D': aText := rsPrecipitationDrizzle;
    'R': aText := rsPrecipitationRain;
  end;

  DisplayText := True;
end;

procedure TfrmCustomGrid.qAudiosprecipitationSetText(Sender: TField; const aText: string);
begin
  if aText = rsPrecipitationNone then
    Sender.AsString := 'N'
  else
  if aText = rsPrecipitationFog then
    Sender.AsString := 'F'
  else
  if aText = rsPrecipitationMist then
    Sender.AsString := 'M'
  else
  if aText = rsPrecipitationDrizzle then
    Sender.AsString := 'D'
  else
  if aText = rsPrecipitationRain then
    Sender.AsString := 'R';
end;

procedure TfrmCustomGrid.qAudiosrecording_contextGetText(Sender: TField; var aText: string; DisplayText: Boolean
  );
begin
  { #todo : Recording contexts GetText }
end;

procedure TfrmCustomGrid.qAudiosrecording_contextSetText(Sender: TField; const aText: string);
begin
  { #todo : Recording contexts SetText }
end;

procedure TfrmCustomGrid.qDocsBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);
end;

procedure TfrmCustomGrid.qDocsdocument_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  case Sender.AsString of
    'url': aText := rsDocUrl;
    'doc': aText := rsDocDocument;
    'spr': aText := rsDocSpreadsheet;
    'prs': aText := rsDocPresentation;
    'pdf': aText := rsDocPdf;
    'img': aText := rsDocImage;
    'aud': aText := rsDocAudio;
    'vid': aText := rsDocVideo;
    'cod': aText := rsDocCode;
    'db':  aText := rsDocDatabase;
    'gis': aText := rsDocGis;
    'oth': aText := rsDocOther;
  end;

  DisplayText := True;
end;

procedure TfrmCustomGrid.qDocsdocument_typeSetText(Sender: TField; const aText: string);
begin
  if aText = rsDocUrl then
    Sender.AsString := 'url'
  else
  if aText = rsDocDocument then
    Sender.AsString := 'doc'
  else
  if aText = rsDocSpreadsheet then
    Sender.AsString := 'spr'
  else
  if aText = rsDocPresentation then
    Sender.AsString := 'prs'
  else
  if aText = rsDocPdf then
    Sender.AsString := 'pdf'
  else
  if aText = rsDocImage then
    Sender.AsString := 'img'
  else
  if aText = rsDocAudio then
    Sender.AsString := 'aud'
  else
  if aText = rsDocVideo then
    Sender.AsString := 'vid'
  else
  if aText = rsDocCode then
    Sender.AsString := 'cod'
  else
  if aText = rsDocDatabase then
    Sender.AsString := 'db'
  else
  if aText = rsDocGis then
    Sender.AsString := 'gis'
  else
  if aText = rsDocOther then
    Sender.AsString := 'oth';
end;

procedure TfrmCustomGrid.qImagesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  //if not DataSet.FieldByName('taxon_id').IsNull then
  //  GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);
  //
  //if not DataSet.FieldByName('locality_id').IsNull then
  //  GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TfrmCustomGrid.qImagesimage_typeGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  { #todo : Image types GetText }
end;

procedure TfrmCustomGrid.qImagesimage_typeSetText(Sender: TField; const aText: string);
begin
  { #todo : Image types SetText }
end;

procedure TfrmCustomGrid.QuickAddChild(aInitialValue: String);
var
  aSurvey, aPerson, aInstitution: Integer;
  aInst: Variant;
  Qry: TSQLQuery;
begin
  aSurvey := 0;
  aPerson := 0;
  aInstitution := 0;

  case FTableType of
    tbExpeditions:
    begin
      case FChildTable of
        tbSurveys:
        begin
          if FindDlg(tbSurveys, eAddChild, aSurvey, aInitialValue) then
          begin
            if not DMM.sqlTrans.Active then
              DMM.sqlTrans.StartTransaction;
            try
              Qry := TSQLQuery.Create(DMM.sqlCon);
              with Qry, SQL do
              try
                SQLConnection := DMM.sqlCon;
                Clear;

                Add('UPDATE surveys SET expedition_id = :aexpedition WHERE survey_id = :asurvey');
                ParamByName('AEXPEDITION').AsInteger := DBG.DataSource.DataSet.FieldByName(COL_EXPEDITION_ID).AsInteger;
                ParamByName('ASURVEY').AsInteger := aSurvey;

                ExecSQL;
              finally
                FreeAndNil(Qry);
              end;

              eAddChild.Clear;
              dsLink1.DataSet.Refresh;
              DMM.sqlTrans.CommitRetaining;
            except
              DMM.sqlTrans.RollbackRetaining;
              raise;
            end;
          end;
        end;
      end;
    end;
    tbSurveys:
    begin
      case FChildTable of
        tbSurveyTeams:
        begin
          if FindDlg(tbPeople, eAddChild, aPerson, aInitialValue) then
          begin
            if not DMM.sqlTrans.Active then
              DMM.sqlTrans.StartTransaction;
            try
              Qry := TSQLQuery.Create(DMM.sqlCon);
              with Qry, SQL do
              try
                SQLConnection := DMM.sqlCon;
                SQLTransaction := DMM.sqlTrans;
                Clear;

                Add('INSERT INTO survey_team (' +
                  'survey_id, ' +
                  'person_id, ' +
                  'visitor, ' +
                  'user_inserted, ' +
                  'insert_date)');
                Add('VALUES (' +
                  ':survey_id, ' +
                  ':person_id, ' +
                  ':visitor, ' +
                  ':user_inserted, ' +
                  'datetime(''now'', ''subsec''))');

                SetForeignParam(ParamByName('survey_id'), dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger);
                SetForeignParam(ParamByName('person_id'), aPerson);
                ParamByName('visitor').AsBoolean := False;
                ParamByName('user_inserted').AsInteger := ActiveUser.Id;

                ExecSQL;
              finally
                FreeAndNil(Qry);
              end;

              eAddChild.Clear;
              dsLink1.DataSet.Refresh;
              DMM.sqlTrans.CommitRetaining;
            except
              DMM.sqlTrans.RollbackRetaining;
              raise;
            end;
          end;
        end;
      end;
    end;
    tbProjects:
    begin
      case FChildTable of
        tbProjectTeams:
        begin
          if FindDlg(tbPeople, eAddChild, aPerson, aInitialValue) then
          begin
            if not DMM.sqlTrans.Active then
              DMM.sqlTrans.StartTransaction;
            try
              Qry := TSQLQuery.Create(DMM.sqlCon);
              with Qry, SQL do
              try
                SQLConnection := DMM.sqlCon;
                SQLTransaction := DMM.sqlTrans;
                Clear;

                Add('INSERT INTO project_team (' +
                  'project_id, ' +
                  'person_id, ' +
                  'project_manager, ' +
                  'institution_id, ' +
                  'user_inserted, ' +
                  'insert_date)');
                Add('VALUES (' +
                  ':project_id, ' +
                  ':person_id, ' +
                  ':project_manager, ' +
                  ':institution_id, ' +
                  ':user_inserted, ' +
                  'datetime(''now'', ''subsec''))');

                SetForeignParam(ParamByName('project_id'), dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger);
                SetForeignParam(ParamByName('person_id'), aPerson);
                ParamByName('project_manager').AsBoolean := False;
                aInst := GetFieldValue('people', COL_INSTITUTION_ID, COL_PERSON_ID, aPerson);
                if aInst <> Null then
                begin
                  aInstitution := aInst;
                  SetForeignParam(ParamByName(COL_INSTITUTION_ID), aInstitution);
                end;
                ParamByName('user_inserted').AsInteger := ActiveUser.Id;

                ExecSQL;
              finally
                FreeAndNil(Qry);
              end;

              eAddChild.Clear;
              dsLink1.DataSet.Refresh;
              DMM.sqlTrans.CommitRetaining;
            except
              DMM.sqlTrans.RollbackRetaining;
              raise;
            end;
          end;
        end;
        tbProjectGoals: ;
        tbProjectChronograms: ;
        tbProjectBudgets: ;
        tbProjectExpenses: ;
      end;
    end;
    tbSpecimens:
    begin
      case FChildTable of
        tbSpecimenCollectors:
        begin
          if FindDlg(tbPeople, eAddChild, aPerson, aInitialValue) then
          begin
            if not DMM.sqlTrans.Active then
              DMM.sqlTrans.StartTransaction;
            try
              Qry := TSQLQuery.Create(DMM.sqlCon);
              with Qry, SQL do
              try
                SQLConnection := DMM.sqlCon;
                SQLTransaction := DMM.sqlTrans;
                Clear;

                Add('INSERT INTO specimen_collectors (' +
                  'specimen_id, ' +
                  'person_id, ' +
                  'collector_seq, ' +
                  'user_inserted, ' +
                  'insert_date)');
                Add('VALUES (' +
                  ':specimen_id, ' +
                  ':person_id, ' +
                  ':collector_seq, ' +
                  ':user_inserted, ' +
                  'datetime(''now'', ''subsec''))');

                SetForeignParam(ParamByName('specimen_id'), dsLink.DataSet.FieldByName(COL_SPECIMEN_ID).AsInteger);
                SetForeignParam(ParamByName('person_id'), aPerson);
                ParamByName('collector_seq').AsInteger := GetNextCollectorSeq(dsLink.DataSet.FieldByName(COL_SPECIMEN_ID).AsInteger);
                ParamByName('user_inserted').AsInteger := ActiveUser.Id;

                ExecSQL;
              finally
                FreeAndNil(Qry);
              end;

              eAddChild.Clear;
              dsLink1.DataSet.Refresh;
              DMM.sqlTrans.CommitRetaining;
            except
              DMM.sqlTrans.RollbackRetaining;
              raise;
            end;
          end;
        end;
        tbSamplePreps: ;
      end;
    end;
  end;
end;

procedure TfrmCustomGrid.RefreshMap;
var
  poi: TGpsPoint;
  rp: TRealPoint;
begin
  mapGeo.GPSItems.Clear(0);
  mapGeo.GPSItems.Clear(1);
  mapGeo.Refresh;

  case FTableType of
    tbGazetteer,
    tbSamplingPlots,
    tbCaptures,
    tbNests,
    tbSightings,
    tbSpecimens:
    begin
      rp.Lon := dsLink.DataSet.FieldByName(COL_LONGITUDE).AsFloat;
      rp.Lat := dsLink.DataSet.FieldByName(COL_LATITUDE).AsFloat;
    end;
    tbSurveys:
    begin
      rp.Lon := dsLink.DataSet.FieldByName(COL_START_LONGITUDE).AsFloat;
      rp.Lat := dsLink.DataSet.FieldByName(COL_START_LATITUDE).AsFloat;
      RefreshMapSurvey;
    end;
  end;

  if not (rp.Lon = 0) and not (rp.Lat = 0) then
  begin
    poi := TGpsPoint.CreateFrom(rp);
    mapGeo.GPSItems.Add(poi, 0);
  end;

  if (mapGeo.GPSItems.Count > 0) then
  begin
    mapGeo.ZoomOnArea(mapGeo.GPSItems.BoundingBox);
    if mapGeo.Zoom > 14 then
      mapGeo.Zoom := 14
    else
      mapGeo.Zoom := mapGeo.Zoom - 1;
  end;
end;

procedure TfrmCustomGrid.RefreshMapSurvey;
var
  BM: TBookMark;
  poi: TGpsPoint;
  rp: TRealPoint;
begin
  // Mistnets
  with dsLink2.DataSet do
  begin
    if not dsLink2.DataSet.Active then
      Open;

    if RecordCount > 0 then
    try
      BM := Bookmark;
      DisableControls;
      First;
      repeat
        rp.Lon := FieldByName(COL_LONGITUDE).AsFloat;
        rp.Lat := FieldByName(COL_LATITUDE).AsFloat;
        if (not (rp.Lon = 0) and not (rp.Lat = 0)) then
        begin
          poi := TGpsPoint.CreateFrom(rp);
          mapGeo.GPSItems.Add(poi, 1);
        end;
        Next;
      until EOF;
    finally
      EnableControls;
      Bookmark := BM;
    end;
  end;
end;

procedure TfrmCustomGrid.SaveColumnsConfig;
var
  ColsFile, ColsFolder: String;
begin
  {$IFDEF DEBUG}
  ColsFolder := 'debug_columns\';
  {$ELSE}
  ColsFolder := 'columns\';
  {$ENDIF}
  if not DirectoryExists(AppDataDir + ColsFolder) then
    CreateDir(AppDataDir + ColsFolder);

  ColsFile := ConcatPaths([AppDataDir, ColsFolder, TABLE_NAMES[FTableType] + '_columns.json']);

  SaveFieldsSettings(dsLink.DataSet, ColsFile);
end;

procedure TfrmCustomGrid.sbAddAudioClick(Sender: TObject);
var
  i: Integer;
begin
  DMM.OpenAudios.InitialDir := xSettings.AudiosFolder;
  if DMM.OpenAudios.Execute then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsImportAudiosTitle;
      dlgProgress.Text := rsProgressPreparing;
      dlgProgress.Max := DMM.OpenAudios.Files.Count;
      stopProcess := False;
      Application.ProcessMessages;
      if not DMM.sqlTrans.Active then
        DMM.sqlCon.StartTransaction;
      try
        for i := 0 to DMM.OpenAudios.Files.Count - 1 do
        begin
          dlgProgress.Text := Format(rsProgressImportAudios, [i + 1, DMM.OpenAudios.Files.Count]);

          AddAudio(qAudios, DMM.OpenAudios.Files[i]);

          dlgProgress.Position := i + 1;
          Application.ProcessMessages;
          if stopProcess then
            Break;
        end;
        if stopProcess then
          DMM.sqlTrans.RollbackRetaining
        else
          DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;
      dlgProgress.Text := rsProgressFinishing;
      dlgProgress.Position := DMM.OpenAudios.Files.Count;
      Application.ProcessMessages;
      stopProcess := False;
    finally
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure TfrmCustomGrid.sbAddChildClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  if not pChild.Visible then
  begin
    with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
      pmAddChild.Popup(X, Y);
  end
  else
  begin
    isWorking := True;
    try
      AddOrEditChild(FTableType, True);
    finally
      UpdateChildBar;
      UpdateChildRightPanel;
      isWorking := False;
    end;
  end;
end;

procedure TfrmCustomGrid.sbAddDocClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmAddDocs.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbAddFeathersBatchClick(Sender: TObject);
begin
  batchFeathers := TbatchFeathers.Create(nil);
  with batchFeathers do
  try
    case TableType of
      tbIndividuals: IndividualId := dsLink.DataSet.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
      tbCaptures:    CaptureId := dsLink.DataSet.FieldByName(COL_CAPTURE_ID).AsInteger;
      tbSightings:   SightingId := dsLink.DataSet.FieldByName(COL_SIGHTING_ID).AsInteger;
    end;

    ShowModal;
  finally
    FreeAndNil(batchFeathers);
  end;

  GetChildDataSet.Refresh;
end;

procedure TfrmCustomGrid.sbAddImageClick(Sender: TObject);
var
  i: Integer;
begin
  DMM.OpenImgs.InitialDir := xSettings.ImagesFolder;
  if DMM.OpenImgs.Execute then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsImportImagesTitle;
      dlgProgress.Text := rsProgressPreparing;
      dlgProgress.Max := DMM.OpenImgs.Files.Count;
      stopProcess := False;
      Application.ProcessMessages;
      if not DMM.sqlTrans.Active then
        DMM.sqlCon.StartTransaction;
      try
        for i := 0 to DMM.OpenImgs.Files.Count - 1 do
        begin
          dlgProgress.Text := Format(rsProgressImportImages, [i + 1, DMM.OpenImgs.Files.Count]);

          AddImage(qImages, tbImages, COL_IMAGE_FILENAME, COL_IMAGE_THUMBNAIL, DMM.OpenImgs.Files[i]);

          dlgProgress.Position := i + 1;
          Application.ProcessMessages;
          if stopProcess then
            Break;
        end;
        if stopProcess then
          DMM.sqlTrans.RollbackRetaining
        else
          DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;
      dlgProgress.Text := rsProgressFinishing;
      dlgProgress.Position := DMM.OpenImgs.Files.Count;
      Application.ProcessMessages;
      stopProcess := False;
    finally
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure TfrmCustomGrid.sbAddNetsBatchClick(Sender: TObject);
begin
  batchNetEffort := TbatchNetEffort.Create(nil);
  with batchNetEffort do
  try
    SurveyId := dsLink.DataSet.FieldByName(COL_SURVEY_ID).AsInteger;
    ShowModal;
  finally
    FreeAndNil(batchNetEffort);
  end;

  GetChildDataSet.Refresh;
end;

procedure TfrmCustomGrid.sbAudioInfoClick(Sender: TObject);
begin
  EditAudioInfo(qAudios, dsLink.DataSet, FTableType);
end;

procedure TfrmCustomGrid.sbCancelRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    if (dsLink.DataSet.Modified) and (xSettings.ConfirmCancel) then
    begin
      if not MsgDlg(rsDiscardChangesTitle, rsCancelEditingPrompt, mtConfirmation) then
        Exit;
    end;

    dsLink.DataSet.Cancel;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbChildHistoryClick(Sender: TObject);
var
  aKeyField: String;
  aDataSet: TDataSet;
begin
  aDataSet := nil;

  case nbChilds.PageIndex of
    0: aDataSet := gridChild1.DataSource.DataSet;
    1: aDataSet := gridChild2.DataSource.DataSet;
    2: aDataSet := gridChild3.DataSource.DataSet;
    3: aDataSet := gridChild4.DataSource.DataSet;
    4: aDataSet := gridChild5.DataSource.DataSet;
    5: aDataSet := gridChild6.DataSource.DataSet;
  end;
  aKeyField := GetPrimaryKey(aDataSet);

  ShowHistory(FTableType, FChildTable, aDataSet.FieldByName(aKeyField).AsInteger);
end;

procedure TfrmCustomGrid.sbChildVerificationsClick(Sender: TObject);
var
  DS: TDataSet;
begin
  //with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
  //begin
  //  pmVerifications.PopupComponent := sbChildVerifications;
  //  pmVerifications.Popup(X, Y);
  //end;

  case nbChilds.PageIndex of
    0: DS := dsLink1.DataSet;
    1: DS := dsLink2.DataSet;
    2: DS := dsLink3.DataSet;
    3: DS := dsLink4.DataSet;
    4: DS := dsLink5.DataSet;
    5: DS := dsLink6.DataSet;
  end;
  ShowVerifications(FTableType, FChildTable, DS.FieldByName(GetPrimaryKey(DS)).AsInteger);
end;

procedure TfrmCustomGrid.sbClearFiltersClick(Sender: TObject);
begin
  ClearSearch;
end;

procedure TfrmCustomGrid.sbColumnHideClick(Sender: TObject);
begin
  gridColumns.Cells[1, gridColumns.Row] := '0';
  dsLink.DataSet.Fields[gridColumns.Row - 1].Visible := False;

  //GetColumns;
  AddGridColumns(FTableType, DBG);
end;

procedure TfrmCustomGrid.sbColumnWidthAutoAdjustClick(Sender: TObject);
begin
  if DBG.SelectedIndex > -1 then
    DBG.AutoAdjustColumns;
end;

procedure TfrmCustomGrid.sbDelAudioClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    DeleteRecord(tbAudioLibrary, qAudios);
    UpdateAudioButtons(qAudios);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbDelChildClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    case FTableType of
      //tbNone: ;
      //tbUsers: ;
      //tbRecordHistory: ;
      //tbGazetteer: ;
      tbSamplingPlots:
        case nbChilds.PageIndex of
          0: DeleteRecord(tbPermanentNets, DMG.qPermanentNets);
        end;
      //tbPermanentNets: ;
      //tbInstitutions: ;
      //tbPeople: ;
      tbProjects:
        case nbChilds.PageIndex of
          0: DeleteRecord(tbProjectTeams, DMG.qProjectTeam);
          1: DeleteRecord(tbProjectGoals, DMG.qProjectGoals);
          2: DeleteRecord(tbProjectChronograms, DMG.qProjectChronogram);
          3: DeleteRecord(tbProjectBudgets, DMG.qProjectBudget);
          4: DeleteRecord(tbProjectExpenses, DMG.qProjectExpenses);
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
          0: DeleteRecord(tbCaptures, DMI.qCaptures);
          1: DeleteRecord(tbFeathers, DMI.qFeathers);
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
          5: DeleteRecord(tbVegetation, DMS.qVegetation);
        end;
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      tbSightings: ;
      tbSpecimens:
        case nbChilds.PageIndex of
          0: DeleteRecord(tbSpecimenCollectors, DMG.qSampleCollectors);
          1: DeleteRecord(tbSamplePreps, DMG.qSamplePreps);
        end;
      //tbSamplePreps: ;
      //tbImages: ;
      //tbAudioLibrary: ;
    end;
    dsLink1.DataSet.Refresh;
    dsLink2.DataSet.Refresh;
    dsLink3.DataSet.Refresh;
    dsLink4.DataSet.Refresh;
    dsLink5.DataSet.Refresh;
    dsLink6.DataSet.Refresh;
  finally
    UpdateChildBar;
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbDelDocClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    DeleteRecord(tbDocuments, qDocs);
    UpdateDocButtons(qDocs);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbDelImageClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    DeleteRecord(tbImages, qImages);
    UpdateImageButtons(qImages);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbDelPermanentlyClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  if isWorking then
    Exit;

  isWorking := True;
  if MsgDlg(rsRecycleDeleteTitle, rsRecycleDeletePermanentlyPrompt, mtConfirmation) then
  begin
    Qry := TSQLQuery.Create(nil);
    with Qry do
    try
      SQLConnection := DMM.sqlCon;
      MacroCheck := True;
      SQL.Add('DELETE FROM %ftable WHERE (active_status = 0)');
      MacroByName('FTABLE').AsString := TABLE_NAMES[FTableType];
      ExecSQL;
      qRecycle.Refresh;
    finally
      FreeAndNil(Qry);
    end;
  end;
  UpdateRecycleButtons(dsRecycle.DataSet);
  isWorking := False;
end;

procedure TfrmCustomGrid.sbDelRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    // Inactivate the selected record
    DeleteRecord(FTableType, dsLink.DataSet);
    dsLink.DataSet.Refresh;
    UpdateButtons(dsLink.DataSet);

    // Update the recycle bin
    dsRecycle.DataSet.Refresh;
    UpdateRecycleButtons(dsRecycle.DataSet);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbDocInfoClick(Sender: TObject);
begin
  EditDocInfo(qDocs, dsLink.DataSet, FTableType);
end;

procedure TfrmCustomGrid.sbEditChildClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    AddOrEditChild(FTableType, False);
  finally
    UpdateChildBar;
    UpdateChildRightPanel;
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbEditRecordClick(Sender: TObject);
var
  needsRefresh: Boolean;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    case FTableType of
      //tbNone: ;
      //tbUsers: ;
      //tbRecordHistory: ;
      tbGazetteer:     needsRefresh := EditSite(dsLink.DataSet);
      tbSamplingPlots: needsRefresh := EditSamplingPlot(dsLink.DataSet);
      tbInstitutions:  needsRefresh := EditInstitution(dsLink.DataSet);
      tbPeople:        needsRefresh := EditPerson(dsLink.DataSet);
      tbProjects:      needsRefresh := EditProject(dsLink.DataSet);
      tbPermits:       needsRefresh := EditPermit(dsLink.DataSet);
      tbTaxonRanks: ;
      //tbZooTaxa: ;
      tbBotanicTaxa:   needsRefresh := EditBotanicTaxon(dsLink.DataSet);
      tbBands:         needsRefresh := EditBand(dsLink.DataSet);
      //tbBandHistory: ;
      tbIndividuals:   needsRefresh := EditIndividual(dsLink.DataSet);
      tbCaptures:      needsRefresh := EditCapture(dsLink.DataSet);
      tbFeathers:      needsRefresh := EditFeather(dsLink.DataSet);
      tbNests:         needsRefresh := EditNest(dsLink.DataSet);
      tbNestOwners:    needsRefresh := EditNestOwner(dsLink.DataSet);
      tbNestRevisions: needsRefresh := EditNestRevision(dsLink.DataSet);
      tbEggs:          needsRefresh := EditEgg(dsLink.DataSet);
      tbMethods:       needsRefresh := EditMethod(dsLink.DataSet);
      tbExpeditions:   needsRefresh := EditExpedition(dsLink.DataSet);
      tbSurveys:       needsRefresh := EditSurvey(dsLink.DataSet);
      tbSightings:     needsRefresh := EditSighting(dsLink.DataSet);
      tbSpecimens:     needsRefresh := EditSpecimen(dsLink.DataSet);
      //tbImages: ;
      //tbAudioLibrary: ;
    end;

    if needsRefresh then
    begin
      UpdateButtons(dsLink.DataSet);
      UpdateFilterPanels;
      UpdateChildRightPanel;
    end;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbEmptyClearAllClick(Sender: TObject);
begin
  ClearSearch;
  if frmMain.eSearch.CanSetFocus then
  begin
    frmMain.sbClearSearchClick(nil);
  end;
end;

procedure TfrmCustomGrid.sbFirstChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.First;
  finally
    UpdateChildButtons(aDataSet);
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbFirstRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    dsLink.DataSet.First;
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbImageInfoClick(Sender: TObject);
begin
  EditImageInfo(qImages, dsLink.DataSet, FTableType);
end;

procedure TfrmCustomGrid.sbInsertBatchClick(Sender: TObject);
var
  needsRefresh: Boolean;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    case FTableType of
      //tbNone: ;
      tbBands:
      begin
        batchBands := TbatchBands.Create(nil);
        with batchBands do
        try
          needsRefresh := ShowModal = mrOK;
        finally
          FreeAndNil(batchBands);
        end;
      end;
      tbFeathers:
      begin
        batchFeathers := TbatchFeathers.Create(nil);
        with batchFeathers do
        try
          needsRefresh := ShowModal = mrOK;
        finally
          FreeAndNil(batchFeathers);
        end;
      end;
    end;

    if needsRefresh then
    begin
      UpdateButtons(dsLink.DataSet);
      UpdateFilterPanels;
      UpdateChildRightPanel;
    end;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbInsertRecordClick(Sender: TObject);
var
  needsRefresh: Boolean;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    case FTableType of
      //tbNone: ;
      //tbUsers: ;
      //tbRecordHistory: ;
      tbGazetteer:     needsRefresh := EditSite(dsLink.DataSet, True);
      tbSamplingPlots: needsRefresh := EditSamplingPlot(dsLink.DataSet, True);
      tbInstitutions:  needsRefresh := EditInstitution(dsLink.DataSet, True);
      tbPeople:        needsRefresh := EditPerson(dsLink.DataSet, True);
      tbProjects:      needsRefresh := EditProject(dsLink.DataSet, True);
      tbPermits:       needsRefresh := EditPermit(dsLink.DataSet, 0, True);
      tbTaxonRanks: ;
      //tbZooTaxa: ;
      tbBotanicTaxa:   needsRefresh := EditBotanicTaxon(dsLink.DataSet, True);
      tbBands:         needsRefresh := EditBand(dsLink.DataSet, True);
      //tbBandHistory: ;
      tbIndividuals:   needsRefresh := EditIndividual(dsLink.DataSet, True);
      tbCaptures:      needsRefresh := EditCapture(dsLink.DataSet, 0, 0, True);
      tbFeathers:      needsRefresh := EditFeather(dsLink.DataSet, 0, 0, 0, True);
      tbNests:         needsRefresh := EditNest(dsLink.DataSet, 0, True);
      tbNestOwners:    needsRefresh := EditNestOwner(dsLink.DataSet, 0, True);
      tbNestRevisions: needsRefresh := EditNestRevision(dsLink.DataSet, 0, True);
      tbEggs:          needsRefresh := EditEgg(dsLink.DataSet, 0, True);
      tbMethods:       needsRefresh := EditMethod(dsLink.DataSet, True);
      tbExpeditions:   needsRefresh := EditExpedition(dsLink.DataSet, True);
      tbSurveys:       needsRefresh := EditSurvey(dsLink.DataSet, 0, True);
      tbSightings:     needsRefresh := EditSighting(dsLink.DataSet, 0, 0, True);
      tbSpecimens:     needsRefresh := EditSpecimen(dsLink.DataSet, 0, True);
      //tbImages: ;
      //tbAudioLibrary: ;
    end;

    if needsRefresh then
    begin
      UpdateButtons(dsLink.DataSet);
      UpdateFilterPanels;
      UpdateChildRightPanel;
    end;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbLastChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.Last;
  finally
    UpdateChildButtons(aDataSet);
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbLastRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    dsLink.DataSet.Last;
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbMarkColumnsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmMarkColumns.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbMarkRecordsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmMark.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbMoreOptionsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmMore.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbMoveColumnDownClick(Sender: TObject);
var
  r: Integer;
begin
  r := gridColumns.Row - 1;
  if dsLink.DataSet.Fields[r].Index = dsLink.DataSet.FieldCount - 1 then
    Exit;

  dsLink.DataSet.Close;
  dsLink.DataSet.Fields[r].Index := dsLink.DataSet.Fields[r].Index + 1;
  dsLink.DataSet.Open;

  //GetColumns;
  gridColumns.MoveColRow(False, gridColumns.Row, gridColumns.Row + 1);
  //gridColumns.Row := gridColumns.Row + 1;

  AddGridColumns(FTableType, DBG);
end;

procedure TfrmCustomGrid.sbMoveColumnUpClick(Sender: TObject);
var
  r: Integer;
begin
  r := gridColumns.Row - 1;
  if dsLink.DataSet.Fields[r].Index = 0 then
    Exit;

  dsLink.DataSet.Close;
  dsLink.DataSet.Fields[r].Index := dsLink.DataSet.Fields[r].Index - 1;
  dsLink.DataSet.Open;

  //GetColumns;
  gridColumns.MoveColRow(False, gridColumns.Row, gridColumns.Row - 1);
  //gridColumns.Row := gridColumns.Row - 1;

  AddGridColumns(FTableType, DBG);
end;

procedure TfrmCustomGrid.sbNextChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.Next;
  finally
    UpdateChildButtons(aDataSet);
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbNextRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    dsLink.DataSet.Next;
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbOpenDocClick(Sender: TObject);
begin
  if qDocs.FieldByName(COL_DOCUMENT_TYPE).AsString = 'url' then
    OpenUrl(qDocs.FieldByName(COL_DOCUMENT_PATH).AsString)
  else
    OpenDocument(qDocs.FieldByName(COL_DOCUMENT_PATH).AsString);
end;

procedure TfrmCustomGrid.sbPlayAudioClick(Sender: TObject);
begin
  { #todo : Audio player }
  // Temporary solution
  OpenDocument(qAudios.FieldByName(COL_AUDIO_FILE).AsString);
end;

procedure TfrmCustomGrid.sbPrintClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmPrint.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbPriorChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.Prior;
  finally
    UpdateChildButtons(aDataSet);
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbPriorRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    dsLink.DataSet.Prior;
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbQuickEntryChildClick(Sender: TObject);
begin
  frmQuickEntry := TfrmQuickEntry.Create(nil);
  with frmQuickEntry do
  try
    frmQuickEntry.TableType := FChildTable;
    ShowModal;
  finally
    FreeAndNil(frmQuickEntry);
  end;
end;

procedure TfrmCustomGrid.sbQuickEntryClick(Sender: TObject);
begin
  frmQuickEntry := TfrmQuickEntry.Create(nil);
  with frmQuickEntry do
  try
    frmQuickEntry.TableType := FTableType;
    ShowModal;
  finally
    FreeAndNil(frmQuickEntry);
  end;
end;

procedure TfrmCustomGrid.sbRecordHistoryClick(Sender: TObject);
var
  aKeyField: String;
begin
  aKeyField := GetPrimaryKey(dsLink.DataSet);
  ShowHistory(FTableType, tbNone, dsLink.DataSet.FieldByName(aKeyField).AsInteger);
end;

procedure TfrmCustomGrid.sbRecordVerificationsClick(Sender: TObject);
var
  DS: TDataSet;
begin
  //with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
  //begin
  //  pmVerifications.PopupComponent := sbRecordVerifications;
  //  pmVerifications.Popup(X, Y);
  //end;

  DS := dsLink.DataSet;
  ShowVerifications(FTableType, tbNone, DS.FieldByName(GetPrimaryKey(DS)).AsInteger);
end;

procedure TfrmCustomGrid.sbRefreshChildClick(Sender: TObject);
var
  DS: TDataSet;
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    case nbChilds.PageIndex of
      0: DS := dsLink1.DataSet;
      1: DS := dsLink2.DataSet;
      2: DS := dsLink3.DataSet;
      3: DS := dsLink4.DataSet;
      4: DS := dsLink5.DataSet;
      5: DS := dsLink6.DataSet;
    end;
    if not DS.Active then
      DS.Open;
    DS.Refresh;
    UpdateChildButtons(DS);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbRefreshRecordsClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    if not dsLink.DataSet.Active then
      dsLink.DataSet.Open;
    dsLink.DataSet.Refresh;
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbRestoreRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    // Restore the selected record
    RestoreRecord(FTableType, dsRecycle.DataSet);
    dsLink.DataSet.Refresh;
    UpdateButtons(dsLink.DataSet);
    // Update the recycle bin
    dsRecycle.DataSet.Refresh;
    UpdateRecycleButtons(dsRecycle.DataSet);
    dbgRecycle.Refresh;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbRowHeightDecreaseClick(Sender: TObject);
begin
  if xSettings.DefaultRowHeight <= MIN_ROW_HEIGHT then
    Exit;

  oldRowHeight := xSettings.DefaultRowHeight;
  xSettings.DefaultRowHeight := xSettings.DefaultRowHeight - 2;

  //UpdateRowHeights;
end;

procedure TfrmCustomGrid.sbRowHeightDefaultClick(Sender: TObject);
begin
  oldRowHeight := xSettings.DefaultRowHeight;
  xSettings.DefaultRowHeight := DEFAULT_ROW_HEIGHT;

  //UpdateRowHeights;
end;

procedure TfrmCustomGrid.sbRowHeightIncreaseClick(Sender: TObject);
begin
  if xSettings.DefaultRowHeight >= MAX_ROW_HEIGHT then
    Exit;

  oldRowHeight := xSettings.DefaultRowHeight;
  xSettings.DefaultRowHeight := xSettings.DefaultRowHeight + 2;

  //UpdateRowHeights;
end;

procedure TfrmCustomGrid.sbSaveRecordClick(Sender: TObject);
begin
  if isWorking then
    Exit;

  isWorking := True;
  try
    dsLink.DataSet.Post;
  finally
    isWorking := False;
  end;
end;

procedure TfrmCustomGrid.sbShareChildClick(Sender: TObject);
begin
  case nbChilds.PageIndex of
    0: ExportDlg(dsLink1.DataSet);
    1: ExportDlg(dsLink2.DataSet);
    2: ExportDlg(dsLink3.DataSet);
    3: ExportDlg(dsLink4.DataSet);
    4: ExportDlg(dsLink5.DataSet);
    5: ExportDlg(dsLink6.DataSet);
  end;
end;

procedure TfrmCustomGrid.sbShareMapPointsClick(Sender: TObject);
var
  aFilename: String;
  Pts: TMapPointList;
begin
  // Export map points
  DMM.SaveKmlDlg.InitialDir := xSettings.LastPathUsed;
  if DMM.SaveKmlDlg.Execute then
  begin
    aFilename := DMM.SaveKmlDlg.FileName;
    Pts := TMapPointList.Create;
    try
      DBToMapPoints(FTableType, dsLink.DataSet, True, Pts);

      case ExtractFileExt(aFilename) of
        '.kml':     SavePointsToKML(Pts, aFilename);
        '.kmz':     SavePointsToKML(Pts, aFilename, True);
        '.gpx':     SavePointsToGPX(Pts, aFilename);
        '.csv':     SavePointsToCSV(Pts, aFilename);
        '.geojson': SavePointsToGeoJSON(Pts, aFilename);
      end;
    finally
      Pts.Free;
    end;
  end;
end;

procedure TfrmCustomGrid.sbShareRecordsClick(Sender: TObject);
begin
  ExportDlg(dsLink.DataSet);
end;

procedure TfrmCustomGrid.sbShowChildSidePanelClick(Sender: TObject);
begin
  pChildRightPanel.Visible := sbShowChildSidePanel.Down;
  UpdateChildRightPanel;
end;

procedure TfrmCustomGrid.sbShowRecordClick(Sender: TObject);
begin
  pSide.Visible := (Sender as TSpeedButton).Down;
  //SplitRight.Visible := pSide.Visible;

  if (Sender as TSpeedButton).Down then
    cpSide.PageIndex := (Sender as TSpeedButton).Tag;

  if (pSide.Visible) and (cpSide.ActivePageComponent = cardSummary) then
    Summary;

  //frmMain.UpdateMenu(frmMain.PGW.ActivePageComponent);
end;

procedure TfrmCustomGrid.sbViewImageClick(Sender: TObject);
begin
  ViewImage(qImages);
end;

function TfrmCustomGrid.Search(aValue: String): Boolean;
begin
  Result := False;

  if isWorking then
    Exit;

  isWorking := True;
  DBG.BeginUpdate;
  try
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
      tbSamplingPlots: Result := SearchSamplingPlots(aValue);
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
      tbFeathers:      Result := SearchFeathers(aValue);
      tbImages: ;
      tbAudioLibrary: ;
    end;
    UpdateButtons(dsLink.DataSet);
  finally
    isWorking := False;
    DBG.EndUpdate;
  end;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_ID, 'Band (ID)', sdtInteger, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_NUMBER, 'Band number', sdtText, Crit,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d+[-‒]{1}\d+$', aValue) then
    begin
      Crit := crBetween;
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      { split strings: unicode characters #$002D e #$2012 }
      V1 := ExtractDelimited(1, aValue, ['-', #$2012]);
      V2 := ExtractDelimited(2, aValue, ['-', #$2012]);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_NUMBER, 'Band number', sdtInteger, Crit,
        False, V1, V2));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CARRIER_NAME, 'Carrier', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SUPPLIER_NAME, 'Supplier', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PROJECT_NAME, 'Project', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_NAME, 'Individual', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_ID, 'Taxon (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Scientific name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_VERNACULAR_NAME, 'Vernacular name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_AUTHORSHIP, 'Authorship', sdtText, Crit,
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
  V1, V2, m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_ID, 'Capture (ID)', sdtInteger, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, 'Capture date', sdtYear, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, 'Capture date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, dt) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_TIME, 'Capture time', sdtTime, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{4}[-‒]{1}\d{4}$', aValue) then
    begin
      Crit := crBetween;
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      { split strings: unicode characters #$002D e #$2012 }
      V1 := ExtractDelimited(1, aValue, ['-', #$2012]);
      V2 := ExtractDelimited(2, aValue, ['-', #$2012]);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, 'Capture date', sdtYear, Crit,
        False, V1, V2));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, 'Capture date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_NAME, 'Band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_NAME, 'Removed band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_NET_STATION_NAME, 'Misnet station', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BANDER_NAME, 'Bander', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_ANNOTATOR_NAME, 'Annotator', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EGG_ID, 'Egg (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_MEASURE_DATE, 'Measured date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_MEASURE_DATE, 'Measured date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FIELD_NUMBER, 'Field number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Taxon', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EXPEDITION_ID, 'Expedition (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_START_DATE, 'Start date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_END_DATE, 'End date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_START_DATE, 'Start date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_END_DATE, 'End date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EXPEDITION_NAME, 'Expedition name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DESCRIPTION, 'Description', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters;

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchFeathers(aValue: String): Boolean;
var
  i, g: Integer;
  dt: TDateTime;
  Crit: TCriteriaType;
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FEATHER_ID, 'Feather (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, 'Date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, 'Date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Taxon', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_OBSERVER_NAME, 'Observer', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SITE_ID, 'Toponym (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToFloat(aValue, f) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LONGITUDE, 'Longitude', sdtText, crStartLike,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LATITUDE, 'Latitude', sdtText, crStartLike,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SITE_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SITE_ABBREVIATION, 'Acronym', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, 'Individual (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BANDING_DATE, 'Banding date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_CHANGE_DATE, 'Band change date', sdtDate, crEqual,
        False, aValue));
      { #todo : PartialDate fields: birth_date and death_date }
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BANDING_DATE, 'Banding date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_CHANGE_DATE, 'Band change date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_NAME, 'Band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DOUBLE_BAND_NAME, 'Double band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_NAME, 'Removed band', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_NEST_NAME, 'Nest', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_INSTITUTION_ID, 'Institution (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, 'Acronym', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_MANAGER_NAME, 'Manager', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_ID, 'Method (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, 'Name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_ABBREVIATION, 'Acronym', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EBIRD_NAME, 'eBird name', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_NEST_ID, 'Nest (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FOUND_DATE, 'Date found', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LAST_DATE, 'Last date seen', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FOUND_DATE, 'Date found', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LAST_DATE, 'Last date seen', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FIELD_NUMBER, 'Field number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, 'Locality', sdtText, Crit,
        True, aValue));
      { #todo : Check field name }
      FSearch.Fields[g].Fields.Add(TSearchField.Create('z.full_name', 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_OBSERVER_NAME, 'Observer', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_1_NAME, 'Support plant 1', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_2_NAME, 'Support plant 2', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_NEST_REVISION_ID, 'Nest revision (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_REVISION_DATE, 'Revision date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_REVISION_DATE, 'Revision date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_NAME, 'Taxon', sdtText, Crit,
        True, aValue));
    end;
  end;

  GetFilters;

  Result := FSearch.RunSearch > 0;
end;

function TfrmCustomGrid.SearchSamplingPlots(aValue: String): Boolean;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SAMPLING_PLOT_ID, 'Sampling plot (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToFloat(aValue, f) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LONGITUDE, 'Longitude', sdtText, crStartLike,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LATITUDE, 'Latitude', sdtText, crStartLike,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, 'Acronym', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PERSON_ID, 'Person (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, 'Birth date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, 'Death date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, 'Birth date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, 'Death date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, 'Acronym', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CITATION, 'Citation', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PERMIT_ID, 'Permit (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DISPATCH_DATE, 'Dispatch date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EXPIRE_DATE, 'Expire date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DISPATCH_DATE, 'Dispatch date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EXPIRE_DATE, 'Expire date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PERMIT_NAME, 'Permit name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PERMIT_NUMBER, 'Permit number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_DISPATCHER_NAME, 'Dispatcher', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PROJECT_ID, 'Project (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_START_DATE, 'Start date', sdtDate, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_END_DATE, 'End date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_START_DATE, 'Start date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_END_DATE, 'End date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PROJECT_TITLE, 'Title', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SHORT_TITLE, 'Short title', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PROTOCOL_NUMBER, 'Protocol number', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_CONTACT_NAME, 'Contact', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_MAIN_GOAL, 'Main goal', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_RISKS, 'Risks', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_ABSTRACT, 'Abstract', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_ID, 'Sighting (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, 'Sighting date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, dt) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, 'Sighting time', sdtTime, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, 'Sighting date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Taxon', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, 'Locality', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, 'Method', sdtText, Crit,
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SPECIMEN_ID, 'Specimen (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_COLLECTION_DATE, 'Collection date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_COLLECTION_DATE, 'Collection date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, 'Taxon', sdtText, Crit,
        True, aValue));
      //FSearch.Fields[g].Fields.Add(TSearchField.Create('collectors', 'Collectors', sdtText, Crit,
      //  False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, 'Locality', sdtText, Crit,
        True, aValue));
      //FSearch.Fields[g].Fields.Add(TSearchField.Create('municipality_name', 'Municipality', sdtText, Crit,
      //  True, aValue));
      //FSearch.Fields[g].Fields.Add(TSearchField.Create('state_name', 'State', sdtText, Crit,
      //  True, aValue));
      //FSearch.Fields[g].Fields.Add(TSearchField.Create('country_name', 'Country', sdtText, Crit,
      //  True, aValue));
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
  m, y: String;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SURVEY_ID, 'Survey (ID)', sdtInteger, crEqual,
        False, aValue));
      // if i > 999 then
      // Add('or (strftime(''%Y'',AMO_DATA) = '+QuotedStr(aValor)+'))');
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, 'Survey date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, Dt) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_START_TIME, 'Start time', sdtTime, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_END_TIME, 'End time', sdtTime, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, 'Survey date', sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, 'Locality', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_RANK_ID, 'Rank (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_RANK_NAME, 'Name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_RANK_ABBREVIATION, 'Acronym', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_ID, 'Taxon (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, 'Scientific name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_ENGLISH_NAME, 'English name', sdtText, Crit,
        False, aValue));
      //FSearch.Fields[g].Fields.Add(TSearchField.Create('ioc_english_name', 'English name (IOC)', sdtText, Crit,
      //  False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_SPANISH_NAME, 'Spanish name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_PORTUGUESE_NAME, 'Portuguese name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_OTHER_NAMES, 'Other portuguese names', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_EBIRD_CODE, 'eBird code', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create(COL_QUICK_CODE, 'Quick code', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetFilters;

  Result := FSearch.RunSearch > 0;
end;

procedure TfrmCustomGrid.SetAudios;
begin
  with qAudios, SQL do
  begin
    case FTableType of
      tbNone: ;
      //tbZooTaxa: ;
      tbIndividuals:    Add('WHERE (snd.active_status = 1) AND (snd.individual_id = :individual_id)');
      //tbCaptures: ;
      //tbExpeditions: ;
      //tbSurveys: ;
      tbSightings:      Add('WHERE (snd.active_status = 1) AND (snd.sighting_id = :sighting_id)');
      tbSpecimens:      Add('WHERE (snd.active_status = 1) AND (snd.specimen_id = :specimen_id)');
      //tbSamplePreps: ;
    end;
  end;

  if FTableType in [tbIndividuals, tbSightings, tbSpecimens] then
  begin
    qAudios.SQL.Add('ORDER BY snd.recording_date, snd.recording_time ASC');
    qAudios.DataSource := dsLink;
    //{$IFDEF DEBUG}
    //LogSQL(qAudios.SQL);
    //{$ENDIF}
    qAudios.Open;
  end;
end;

procedure TfrmCustomGrid.SetColumnsBands(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_BAND_ID).Visible then
      ColumnByFieldname(COL_BAND_ID).ReadOnly := True;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_ID).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_ID).ReadOnly := True;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_BAND_SIZE).Visible then
      ColumnByFieldName(COL_BAND_SIZE).PickList.AddCommaText('A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z');
    if DataSource.DataSet.FieldByName(COL_BAND_STATUS).Visible then
      ColumnByFieldName(COL_BAND_STATUS).PickList.AddCommaText(rsBandStatusList);
    if DataSource.DataSet.FieldByName(COL_BAND_SOURCE).Visible then
    begin
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandAcquiredFromSupplier);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandTransferBetweenBanders);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandLivingBirdBandedByOthers);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandDeadBirdBandedByOthers);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandFoundLoose);
    end;
    if DataSource.DataSet.FieldByName(COL_BAND_TYPE).Visible then
      ColumnByFieldName(COL_BAND_TYPE).PickList.AddCommaText(rsBandTypeList);

    if DataSource.DataSet.FieldByName(COL_SUPPLIER_NAME).Visible then
      ColumnByFieldName(COL_SUPPLIER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REQUESTER_NAME).Visible then
      ColumnByFieldname(COL_REQUESTER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CARRIER_NAME).Visible then
      ColumnByFieldName(COL_CARRIER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsBotanicTaxa(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_TAXON_ID).Visible then
      ColumnByFieldname(COL_TAXON_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_PARENT_TAXON_NAME).Visible then
      ColumnByFieldname(COL_PARENT_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_VALID_NAME).Visible then
      ColumnByFieldname(COL_VALID_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsCaptures(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_CAPTURE_ID).Visible then
      ColumnByFieldname(COL_CAPTURE_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_CAPTURE_DATE).Visible then
      ColumnByFieldName(COL_CAPTURE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CAPTURE_TYPE).Visible then
      ColumnByFieldName(COL_CAPTURE_TYPE).PickList.AddCommaText(rsCaptureTypeList);
    if DataSource.DataSet.FieldByName(COL_RIGHT_TARSUS).Visible then
      ColumnByFieldName(COL_RIGHT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TARSUS).Visible then
      ColumnByFieldName(COL_LEFT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TIBIA).Visible then
      ColumnByFieldName(COL_RIGHT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TIBIA).Visible then
      ColumnByFieldName(COL_LEFT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Visible then
      ColumnByFieldname(COL_NET_STATION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BANDER_NAME).Visible then
      ColumnByFieldname(COL_BANDER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_ANNOTATOR_NAME).Visible then
      ColumnByFieldname(COL_ANNOTATOR_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_NAME).Visible then
      ColumnByFieldname(COL_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Visible then
      ColumnByFieldname(COL_REMOVED_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MOLT_LIMITS).Visible then
      ColumnByFieldName(COL_MOLT_LIMITS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CYCLE_CODE).Visible then
      ColumnByFieldName(COL_CYCLE_CODE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_HOW_AGED).Visible then
      ColumnByFieldName(COL_HOW_AGED).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_HOW_SEXED).Visible then
      ColumnByFieldName(COL_HOW_SEXED).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_NAME).Visible then
      ColumnByFieldname(COL_PHOTOGRAPHER_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_NAME).Visible then
      ColumnByFieldname(COL_PHOTOGRAPHER_2_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_CLOACAL_PROTUBERANCE).Visible then
      ColumnByFieldName(COL_CLOACAL_PROTUBERANCE).PickList.AddCommaText('U,N,S,M,L');
    if DataSource.DataSet.FieldByName(COL_BROOD_PATCH).Visible then
      ColumnByFieldName(COL_BROOD_PATCH).PickList.AddCommaText('F,N,V,W,O');
    if DataSource.DataSet.FieldByName(COL_FAT).Visible then
      ColumnByFieldName(COL_FAT).PickList.AddCommaText('N,T,L,H,F,B,G,V');
    if DataSource.DataSet.FieldByName(COL_BODY_MOLT).Visible then
      ColumnByFieldName(COL_BODY_MOLT).PickList.AddCommaText('N,T,S,H,G,A,F');
    if DataSource.DataSet.FieldByName(COL_FLIGHT_FEATHERS_MOLT).Visible then
      ColumnByFieldName(COL_FLIGHT_FEATHERS_MOLT).PickList.AddCommaText('N,S,A');
    if DataSource.DataSet.FieldByName(COL_FLIGHT_FEATHERS_WEAR).Visible then
      ColumnByFieldName(COL_FLIGHT_FEATHERS_WEAR).PickList.AddCommaText('N,S,L,M,H,X');
    if DataSource.DataSet.FieldByName(COL_SKULL_OSSIFICATION).Visible then
      ColumnByFieldName(COL_SKULL_OSSIFICATION).PickList.AddCommaText('N,T,L,H,G,A,F');
    if DataSource.DataSet.FieldByName(COL_SUBJECT_AGE).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeUnknown);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeAdult);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeJuvenile);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFledgling);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeNestling);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFirstYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeSecondYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeThirdYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFourthYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFifthYear);
    end;
    if DataSource.DataSet.FieldByName(COL_SUBJECT_SEX).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexMale);
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexFemale);
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexUnknown);
    end;
    if DataSource.DataSet.FieldByName(COL_SUBJECT_STATUS).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusNormal);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusInjured);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusWingSprain);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusStressed);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusDead);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsEggs(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_EGG_ID).Visible then
      ColumnByFieldname(COL_EGG_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MEASURE_DATE).Visible then
      ColumnByFieldName(COL_MEASURE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RESEARCHER_NAME).Visible then
      ColumnByFieldName(COL_RESEARCHER_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_EGG_SHAPE).Visible then
    begin
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggSpherical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggElliptical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggOval);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggPyriform);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggConical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggBiconical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggCylindrical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggLongitudinal);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggUnknown);
    end;

    if DataSource.DataSet.FieldByName(COL_EGGSHELL_TEXTURE).Visible then
    begin
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggChalky);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggShiny);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggGlossy);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggPitted);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggUnknown);
    end;

    if DataSource.DataSet.FieldByName(COL_EGGSHELL_PATTERN).Visible then
    begin
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSpots);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggBlotches);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggStreaks);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggScrawls);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSpotsSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggBlotchesSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggUnknown);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsExpeditions(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_EXPEDITION_ID).Visible then
      ColumnByFieldname(COL_EXPEDITION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldname(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_DATE).Visible then
      ColumnByFieldName(COL_START_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_DATE).Visible then
      ColumnByFieldName(COL_END_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsFeathers(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_FEATHER_ID).Visible then
      ColumnByFieldname(COL_FEATHER_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_DATE).Visible then
      ColumnByFieldName(COL_SAMPLE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_NAME).Visible then
      ColumnByFieldname(COL_SIGHTING_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsGazetteer(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SITE_ID).Visible then
      ColumnByFieldname(COL_SITE_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SITE_RANK).Visible then
    begin
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionCountry);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionState);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionRegion);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionMunicipality);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionDistrict);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionLocality);
    end;

    if DataSource.DataSet.FieldByName(COL_PARENT_SITE_NAME).Visible then
      ColumnByFieldname(COL_PARENT_SITE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsIndividuals(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_ID).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_NAME).Visible then
      ColumnByFieldName(COL_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DOUBLE_BAND_NAME).Visible then
      ColumnByFieldName(COL_DOUBLE_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Visible then
      ColumnByFieldName(COL_REMOVED_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BANDING_DATE).Visible then
      ColumnByFieldName(COL_BANDING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_CHANGE_DATE).Visible then
      ColumnByFieldName(COL_BAND_CHANGE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_FATHER_NAME).Visible then
      ColumnByFieldName(COL_FATHER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MOTHER_NAME).Visible then
      ColumnByFieldName(COL_MOTHER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TARSUS).Visible then
      ColumnByFieldName(COL_RIGHT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TARSUS).Visible then
      ColumnByFieldName(COL_LEFT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TIBIA).Visible then
      ColumnByFieldName(COL_RIGHT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TIBIA).Visible then
      ColumnByFieldName(COL_LEFT_TIBIA).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_SEX).Visible then
    begin
      ColumnByFieldName(COL_INDIVIDUAL_SEX).PickList.Add(rsSexUnknown);
      ColumnByFieldName(COL_INDIVIDUAL_SEX).PickList.Add(rsSexMale);
      ColumnByFieldName(COL_INDIVIDUAL_SEX).PickList.Add(rsSexFemale);
    end;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_AGE).Visible then
    begin
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeUnknown);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeAdult);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeJuvenile);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFledgling);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeNestling);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFirstYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeSecondYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeThirdYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFourthYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFifthYear);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsInstitutions(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_INSTITUTION_ID).Visible then
      ColumnByFieldname(COL_INSTITUTION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Visible then
      ColumnByFieldname(COL_COUNTRY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_STATE_NAME).Visible then
      ColumnByFieldname(COL_STATE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Visible then
      ColumnByFieldname(COL_MUNICIPALITY_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsMethods(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_METHOD_ID).Visible then
      ColumnByFieldname(COL_METHOD_ID).ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.SetColumnsNestOwners(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    //ColumnByFieldname('nest_owner_id').ReadOnly:= True;

    if DataSource.DataSet.FieldByName(COL_ROLE).Visible then
      ColumnByFieldName(COL_ROLE).PickList.CommaText := rsNestOwnersRoleList;

    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsNestRevisions(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NEST_REVISION_ID).Visible then
      ColumnByFieldname(COL_NEST_REVISION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_REVISION_DATE).Visible then
      ColumnByFieldName(COL_REVISION_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_1_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_2_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_2_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NIDOPARASITE_NAME).Visible then
      ColumnByFieldName(COL_NIDOPARASITE_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsNests(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NEST_ID).Visible then
      ColumnByFieldname(COL_NEST_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_FOUND_DATE).Visible then
      ColumnByFieldName(COL_FOUND_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LAST_DATE).Visible then
      ColumnByFieldName(COL_LAST_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_NAME).Visible then
      ColumnByFieldName(COL_SUPPORT_PLANT_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_NAME).Visible then
      ColumnByFieldName(COL_SUPPORT_PLANT_2_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_NEST_SHAPE).Visible then
    begin
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeScrape);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeCup);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePlate);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeSphere);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePendent);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePlatform);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeMound);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeBurrow);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeCavity);
    end;

    if DataSource.DataSet.FieldByName(COL_SUPPORT_TYPE).Visible then
    begin
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportGround);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportHerbBush);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportBranchFork);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportLeaves);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportLedge);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportRockCliff);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportRavine);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportNestBox);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportAnthropic);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportOther);
    end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSamplingPlots(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SAMPLING_PLOT_ID).Visible then
      ColumnByFieldname(COL_SAMPLING_PLOT_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsPeople(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERSON_ID).Visible then
      ColumnByFieldname(COL_PERSON_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_GENDER).Visible then
      ColumnByFieldName(COL_GENDER).PickList.AddCommaText(rsGenderList);
    if DataSource.DataSet.FieldByName(COL_TITLE_TREATMENT).Visible then
      ColumnByFieldName(COL_TITLE_TREATMENT).PickList.AddCommaText(rsTreatmentList);

    if DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Visible then
      ColumnByFieldname(COL_INSTITUTION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Visible then
      ColumnByFieldname(COL_COUNTRY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_STATE_NAME).Visible then
      ColumnByFieldname(COL_STATE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Visible then
      ColumnByFieldname(COL_MUNICIPALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BIRTH_DATE).Visible then
      ColumnByFieldName(COL_BIRTH_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DEATH_DATE).Visible then
      ColumnByFieldName(COL_DEATH_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsPermanentNets(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERMANENT_NET_ID).Visible then
      ColumnByFieldname(COL_PERMANENT_NET_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsPermits(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERMIT_ID).Visible then
      ColumnByFieldname(COL_PERMIT_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DISPATCH_DATE).Visible then
      ColumnByFieldName(COL_DISPATCH_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EXPIRE_DATE).Visible then
      ColumnByFieldName(COL_EXPIRE_DATE).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_PERMIT_TYPE).Visible then
      with ColumnByFieldName(COL_PERMIT_TYPE).PickList do
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
    if DataSource.DataSet.FieldByName(COL_PROJECT_ID).Visible then
      ColumnByFieldname(COL_PROJECT_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_START_DATE).Visible then
      ColumnByFieldName(COL_START_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_DATE).Visible then
      ColumnByFieldName(COL_END_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSightings(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SIGHTING_ID).Visible then
      ColumnByFieldname(COL_SIGHTING_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldname(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DETECTION_TYPE).Visible then
      ColumnByFieldname(COL_DETECTION_TYPE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BREEDING_STATUS).Visible then
      ColumnByFieldname(COL_BREEDING_STATUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_DATE).Visible then
      ColumnByFieldname(COL_SIGHTING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSpecimens(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SPECIMEN_ID).Visible then
    begin
      ColumnByFieldname(COL_SPECIMEN_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_TYPE).Visible then
      with ColumnByFieldName(COL_SAMPLE_TYPE).PickList do
      begin
        Clear;
        Add(rsSpecimenCarcassWhole);
        Add(rsSpecimenCarcassPartial);
        Add(rsSpecimenNest);
        Add(rsSpecimenBones);
        Add(rsSpecimenEgg);
        Add(rsSpecimenParasites);
        Add(rsSpecimenFeathers);
        Add(rsSpecimenBlood);
        Add(rsSpecimenClaw);
        Add(rsSpecimenSwab);
        Add(rsSpecimenTissues);
        Add(rsSpecimenFeces);
        Add(rsSpecimenRegurgite);
      end;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EGG_NAME).Visible then
      ColumnByFieldName(COL_EGG_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsSurveys(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SURVEY_ID).Visible then
      ColumnByFieldname(COL_SURVEY_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_DATE).Visible then
      ColumnByFieldName(COL_SURVEY_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_METHOD_NAME).Visible then
      ColumnByFieldName(COL_METHOD_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EXPEDITION_NAME).Visible then
      ColumnByFieldname(COL_EXPEDITION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Visible then
      ColumnByFieldname(COL_NET_STATION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_LONGITUDE).Visible then
      ColumnByFieldname(COL_START_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_LATITUDE).Visible then
      ColumnByFieldname(COL_START_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_LONGITUDE).Visible then
      ColumnByFieldname(COL_END_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_LATITUDE).Visible then
      ColumnByFieldname(COL_END_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldname(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TfrmCustomGrid.SetColumnsTaxonRanks(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_RANK_ID).Visible then
      ColumnByFieldname(COL_RANK_ID).ReadOnly := True;
  end;
end;

procedure TfrmCustomGrid.SetColumnsVegetation(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_VEGETATION_ID).Visible then
      ColumnByFieldname(COL_VEGETATION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_HERBS_DISTRIBUTION).Visible then
      with ColumnByFieldname(COL_HERBS_DISTRIBUTION).PickList do
      begin
        Add(rsDistributionNone);
        Add(rsDistributionRare);
        Add(rsDistributionFewSparse);
        Add(rsDistributionOnePatch);
        Add(rsDistributionOnePatchFewSparse);
        Add(rsDistributionManySparse);
        Add(rsDistributionOnePatchManySparse);
        Add(rsDistributionFewPatches);
        Add(rsDistributionFewPatchesSparse);
        Add(rsDistributionManyPatches);
        Add(rsDistributionManyPatchesSparse);
        Add(rsDistributionEvenHighDensity);
        Add(rsDistributionContinuousFewGaps);
        Add(rsDistributionContinuousDense);
        Add(rsDistributionContinuousDenseEdge);
      end;
    if DataSource.DataSet.FieldByName(COL_SHRUBS_DISTRIBUTION).Visible then
      with ColumnByFieldname(COL_SHRUBS_DISTRIBUTION).PickList do
      begin
        Add(rsDistributionNone);
        Add(rsDistributionRare);
        Add(rsDistributionFewSparse);
        Add(rsDistributionOnePatch);
        Add(rsDistributionOnePatchFewSparse);
        Add(rsDistributionManySparse);
        Add(rsDistributionOnePatchManySparse);
        Add(rsDistributionFewPatches);
        Add(rsDistributionFewPatchesSparse);
        Add(rsDistributionManyPatches);
        Add(rsDistributionManyPatchesSparse);
        Add(rsDistributionEvenHighDensity);
        Add(rsDistributionContinuousFewGaps);
        Add(rsDistributionContinuousDense);
        Add(rsDistributionContinuousDenseEdge);
      end;
    if DataSource.DataSet.FieldByName(COL_TREES_DISTRIBUTION).Visible then
      with ColumnByFieldname(COL_TREES_DISTRIBUTION).PickList do
      begin
        Add(rsDistributionNone);
        Add(rsDistributionRare);
        Add(rsDistributionFewSparse);
        Add(rsDistributionOnePatch);
        Add(rsDistributionOnePatchFewSparse);
        Add(rsDistributionManySparse);
        Add(rsDistributionOnePatchManySparse);
        Add(rsDistributionFewPatches);
        Add(rsDistributionFewPatchesSparse);
        Add(rsDistributionManyPatches);
        Add(rsDistributionManyPatchesSparse);
        Add(rsDistributionEvenHighDensity);
        Add(rsDistributionContinuousFewGaps);
        Add(rsDistributionContinuousDense);
        Add(rsDistributionContinuousDenseEdge);
      end;
  end;
end;

procedure TfrmCustomGrid.SetColumnsWeatherLogs(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_WEATHER_ID).Visible then
      ColumnByFieldname(COL_WEATHER_ID).ReadOnly:= True;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_MOMENT).Visible then
    begin
      ColumnByFieldname(COL_SAMPLE_MOMENT).PickList.Add(rsMomentStart);
      ColumnByFieldname(COL_SAMPLE_MOMENT).PickList.Add(rsMomentMiddle);
      ColumnByFieldname(COL_SAMPLE_MOMENT).PickList.Add(rsMomentEnd);
    end;

    if DataSource.DataSet.FieldByName(COL_PRECIPITATION).Visible then
    begin
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationNone);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationFog);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationMist);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationDrizzle);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationRain);
    end;
  end;
end;

procedure TfrmCustomGrid.SetDocs;
begin
  with qDocs, SQL do
  begin
    case FTableType of
      tbNone: ;
      //tbUsers: ;
      //tbRecordHistory: ;
      //tbRecordVerifications: ;
      //tbGazetteer: ;
      tbSamplingPlots:  Add('WHERE (active_status = 1) AND (net_station_id = :sampling_plot_id)');
      //tbPermanentNets: ;
      //tbInstitutions: ;
      tbPeople:         Add('WHERE (active_status = 1) AND (person_id = :person_id)');
      tbProjects:       Add('WHERE (active_status = 1) AND (project_id = :project_id)');
      //tbProjectTeams: ;
      tbPermits:        Add('WHERE (active_status = 1) AND (permit_id = :permit_id)');
      //tbTaxonRanks: ;
      //tbZooTaxa: ;
      //tbBotanicTaxa: ;
      //tbBands: ;
      //tbBandHistory: ;
      tbIndividuals:    Add('WHERE (active_status = 1) AND (individual_id = :individual_id)');
      tbCaptures:       Add('WHERE (active_status = 1) AND (capture_id = :capture_id)');
      //tbMolts: ;
      tbNests:          Add('WHERE (active_status = 1) AND (nest_id = :nest_id)');
      //tbNestOwners: ;
      //tbNestRevisions: ;
      //tbEggs: ;
      tbMethods:        Add('WHERE (active_status = 1) AND (method_id = :method_id)');
      tbExpeditions:    Add('WHERE (active_status = 1) AND (expedition_id = :expedition_id)');
      tbSurveys:        Add('WHERE (active_status = 1) AND (survey_id = :survey_id)');
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      //tbWeatherLogs: ;
      tbSightings:      Add('WHERE (active_status = 1) AND (sighting_id = :sighting_id)');
      tbSpecimens:      Add('WHERE (active_status = 1) AND (specimen_id = :specimen_id)');
      //tbSamplePreps: ;
      //tbSpecimenCollectors: ;
    end;
  end;

  if FTableType in [tbIndividuals, tbSightings, tbSpecimens, tbSamplingPlots, tbPeople, tbProjects, tbPermits,
    tbCaptures, tbNests, tbMethods, tbExpeditions, tbSurveys] then
  begin
    qDocs.SQL.Add('ORDER BY document_date, document_time ASC');
    qDocs.DataSource := dsLink;
    //{$IFDEF DEBUG}
    //LogSQL(qDocs.SQL);
    //{$ENDIF}
    qDocs.Open;
  end;
end;

procedure TfrmCustomGrid.SetFilters(Sender: TObject);
begin
  if not FCanToggle then
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
    tbSamplingPlots:  SetGridSamplingPlots;
    tbTaxonRanks:     SetGridTaxonRanks;
    tbBotanicTaxa:    SetGridBotanicTaxa;
    tbZooTaxa: ;
    tbBands:          SetGridBands;
    tbIndividuals:    SetGridIndividuals;
    tbCaptures:       SetGridCaptures;
    tbFeathers:       SetGridFeathers;
    tbNests:          SetGridNests;
    tbNestRevisions:  SetGridNestRevisions;
    tbEggs:           SetGridEggs;
    tbMethods:        SetGridMethods;
    tbExpeditions:    SetGridExpeditions;
    tbSurveys:        SetGridSurveys;
    tbSightings:      SetGridSightings;
    tbSpecimens:      SetGridSpecimens;
  end;
  CreatePanelTabs;
  dsLink.DataSet := FSearch.DataSet;

  SplitChild.Visible := pChild.Visible;
  Search(EmptyStr);
end;

procedure TfrmCustomGrid.SetGridBands;
begin
  Caption := rsTitleBands;
  FSearch.DataSet := DMG.qBands;
  // Set the default data sorting
  AddSortedField(COL_BAND_SIZE, sdAscending);
  AddSortedField(COL_BAND_NUMBER, sdAscending);

  // Set the print menu
  pmPrintBands.Visible := True;
  pmPrintBandsByCarrier.Visible := True;
  pmPrintBandsWithHistory.Visible := True;
  pmPrintBandsByStatus.Visible := True;

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  sbInsertBatch.Visible := True;
  sbMoreOptions.Visible := True;

  // Set the more options menu
  pmpTransferBandsTo.Visible := True;
  pmpBandHistory.Visible := True;
  pmpBandsBalance.Visible := True;

  // Set grid menu
  pmgBandHistory.Visible := True;
end;

procedure TfrmCustomGrid.SetGridBotanicTaxa;
begin
  Caption := rsTitleBotanicalTaxa;
  FSearch.DataSet := DMG.qBotany;
  // Set the default data sorting
  AddSortedField(COL_TAXON_NAME, sdAscending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  // Set the print menu
  pmPrintBotanicTaxa.Visible := True;
  pmPrintBotanicTaxaHierarchical.Visible := True;
  pmPrintBotanicTaxaRecorded.Visible := True;
end;

procedure TfrmCustomGrid.SetGridCaptures;
begin
  Caption := rsTitleCaptures;
  FSearch.DataSet := DMG.qCaptures;
  // Set the default data sorting
  AddSortedField(COL_CAPTURE_DATE, sdDescending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;
  sbShowImages.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintCaptures.Visible := True;
  pmPrintCapturesByDate.Visible := True;
  pmPrintCapturesByProject.Visible := True;
  pmPrintCapturesByLocality.Visible := True;
  pmPrintCapturesByTaxon.Visible := True;
  pmPrintColoredBands.Visible := True;
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
    tbSamplingPlots:  SetColumnsSamplingPlots(aGrid);
    tbPermanentNets:  SetColumnsPermanentNets(aGrid);
    tbTaxonRanks:     SetColumnsTaxonRanks(aGrid);
    tbBotanicTaxa:    SetColumnsBotanicTaxa(aGrid);
    tbZooTaxa: ;
    tbBands:          SetColumnsBands(aGrid);
    tbIndividuals:    SetColumnsIndividuals(aGrid);
    tbCaptures:       SetColumnsCaptures(aGrid);
    tbFeathers:       SetColumnsFeathers(aGrid);
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
  FSearch.DataSet := DMG.qEggs;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  sbShowImages.Visible := True;
  //sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintEggs.Visible := True;
  pmPrintEggsByNest.Visible := True;
  pmPrintEggsByLocality.Visible := True;
  pmPrintEggsByTaxon.Visible := True;
end;

procedure TfrmCustomGrid.SetGridExpeditions;
begin
  Caption := rsCaptionExpeditions;
  FSearch.DataSet := DMG.qExpeditions;
  // Set the default data sorting
  AddSortedField(COL_START_DATE, sdDescending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintExpeditions.Visible := True;
  pmPrintExpeditionsByProject.Visible := True;

  // Set the childs panel
  nbChilds.PageIndex := 0;
  if not Assigned(DMS) then
    DMS := TDMS.Create(nil);
  FChildTable := tbSurveys;
  dsLink1.DataSet := DMS.qSurveys;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewSurvey.Visible := True;
  // Set visible child buttons
  sbChildVerifications.Visible := True;
end;

procedure TfrmCustomGrid.SetGridFeathers;
begin
  Caption := rsCaptionFeathers;
  FSearch.DataSet := DMG.qFeathers;
  // Set the default data sorting
  AddSortedField(COL_SAMPLE_DATE, sdDescending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowImages.Visible := True;
  sbShowSummary.Visible := True;
  sbInsertBatch.ImageIndex := 104;
  sbInsertBatch.DisabledImageIndex := 105;
  sbInsertBatch.Visible := True;

  // Set the print menu
  pmPrintFeathers.Visible := True;
end;

procedure TfrmCustomGrid.SetGridGazetteer;
begin
  Caption := rsTitleGazetteer;
  FSearch.DataSet := DMG.qGazetteer;
  // Set the default data sorting
  AddSortedField(COL_SITE_NAME, sdAscending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;
  //sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintGazetteer.Visible := True;
  pmPrintGazetteerHierarchical.Visible := True;
end;

procedure TfrmCustomGrid.SetGridIndividuals;
begin
  Caption := rsTitleIndividuals;
  FSearch.DataSet := DMG.qIndividuals;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbShowImages.Visible := True;
  sbShowAudio.Visible := True;
  sbShowDocs.Visible := True;
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  // Set the print menu
  pmPrintIndividuals.Visible := True;
  pmPrintIndividualsByTaxon.Visible := True;
  pmPrintIndividualsByParents.Visible := True;
  pmPrintColoredBands.Visible := True;

  //Set the childs panel
  nbChilds.PageIndex := 0;
  if not Assigned(DMI) then
    DMI := TDMI.Create(nil);
  FChildTable := tbCaptures;
  dsLink1.DataSet := DMI.qCaptures;
  dsLink2.DataSet := DMI.qFeathers;
  dsLink3.DataSet := DMI.qSightings;
  dsLink4.DataSet := DMI.qNests;
  dsLink5.DataSet := DMI.qSpecimens;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewCapture.Visible := True;
  pmcNewFeather.Visible := True;
  pmcNewSighting.Visible := True;
  pmcNewNest.Visible := True;
  pmcNewSpecimen.Visible := True;
  // Set visible child buttons
  sbChildVerifications.Visible := True;
end;

procedure TfrmCustomGrid.SetGridInstitutions;
begin
  Caption := rsTitleInstitutions;
  FSearch.DataSet := DMG.qInstitutions;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbShowSummary.Visible := True;

  // Set the print menu
  pmPrintInstitutions.Visible := True;
end;

procedure TfrmCustomGrid.SetGridMethods;
begin
  Caption := rsTitleMethods;
  FSearch.DataSet := DMG.qMethods;
  // Set the default data sorting
  AddSortedField(COL_METHOD_NAME, sdAscending);

  // Set visible buttons
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintMethods.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNests;
begin
  Caption := rsTitleNests;
  FSearch.DataSet := DMG.qNests;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;
  sbShowImages.Visible := True;
  sbShowDocs.Visible := True;
  sbRecordVerifications.Visible := True;

  // Set the print menu
  pmPrintNests.Visible := True;
  pmPrintNestsByPeriod.Visible := True;
  pmPrintNestsByProject.Visible := True;
  pmPrintNestsByLocality.Visible := True;
  pmPrintNestsByTaxon.Visible := True;

  // Set the childs panel
  nbChilds.PageIndex := 0;
  if not Assigned(DMB) then
    DMB := TDMB.Create(nil);
  FChildTable := tbNestOwners;
  dsLink1.DataSet := DMB.qNestOwners;
  dsLink2.DataSet := DMB.qNestRevisions;
  dsLink3.DataSet := DMB.qEggs;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewNestOwner.Visible := True;
  pmcNewNestRevision.Visible := True;
  pmcNewEgg.Visible := True;
  // Set visible child buttons
  sbChildVerifications.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNestRevisions;
begin
  Caption := rsTitleNestRevisions;
  FSearch.DataSet := DMG.qNestRevisions;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSamplingPlots;
begin
  Caption := rsTitleSamplingPlots;
  FSearch.DataSet := DMG.qSamplingPlots;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintSamplingPlots.Visible := True;
  pmPrintSamplingPlotsByLocality.Visible := True;

  // Set the childs panel
  nbChilds.PageIndex := 0;
  FChildTable := tbPermanentNets;
  dsLink1.DataSet := DMG.qPermanentNets;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewPermanentNet.Visible := True;
  // Set visible child buttons
  sbChildVerifications.Visible := True;
end;

procedure TfrmCustomGrid.SetGridPeople;
begin
  Caption := rsTitleResearchers;
  FSearch.DataSet := DMG.qPeople;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbShowSummary.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintResearchers.Visible := True;
end;

procedure TfrmCustomGrid.SetGridPermits;
begin
  Caption := rsTitlePermits;
  FSearch.DataSet := DMG.qPermits;
  // Set the default data sorting
  AddSortedField(COL_PERMIT_NAME, sdAscending);

  // Set visible buttons
  sbShowSummary.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintPermits.Visible := True;
  pmPrintPermitsByExpiration.Visible := True;
  pmPrintPermitsByProject.Visible := True;
end;

procedure TfrmCustomGrid.SetGridProjects;
begin
  Caption := rsTitleProjects;
  FSearch.DataSet := DMG.qProjects;
  // Set the default data sorting
  AddSortedField(COL_PROJECT_TITLE, sdAscending);

  // Set visible buttons
  sbShowSummary.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintProjects.Visible := True;

  // Set the childs panel
  nbChilds.PageIndex := 0;
  FChildTable := tbProjectTeams;
  dsLink1.DataSet := DMG.qProjectTeam;
  dsLink2.DataSet := DMG.qProjectGoals;
  dsLink3.DataSet := DMG.qProjectChronogram;
  dsLink4.DataSet := DMG.qProjectBudget;
  dsLink5.DataSet := DMG.qProjectExpenses;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewProjectMember.Visible := True;
  pmcNewProjectGoal.Visible := True;
  pmcNewChronogramActivity.Visible := True;
  pmcNewBudgetItem.Visible := True;
  pmcNewExpense.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSightings;
begin
  Caption := rsTitleSightings;
  FSearch.DataSet := DMG.qSightings;
  // Set the default data sorting
  AddSortedField(COL_SIGHTING_DATE, sdDescending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;
  sbShowImages.Visible := True;
  sbShowAudio.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintSightings.Visible := True;
  pmPrintSightingsBySurvey.Visible := True;
  pmPrintSightingsByObserver.Visible := True;
  pmPrintSightingsByProject.Visible := True;
  pmPrintSightingsByLocality.Visible := True;
  pmPrintSightingsByTaxon.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSpecimens;
begin
  Caption := rsTitleSpecimens;
  FSearch.DataSet := DMG.qSpecimens;
  // Set the default data sorting
  AddSortedField(COL_FULL_NAME, sdAscending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowImages.Visible := True;
  sbShowAudio.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintSpecimens.Visible := True;
  pmPrintSpecimensByYear.Visible := True;
  pmPrintSpecimensByProject.Visible := True;
  pmPrintSpecimensByLocality.Visible := True;
  pmPrintSpecimensByTaxon.Visible := True;

  // Set the childs panel
  nbChilds.PageIndex := 0;
  FChildTable := tbSpecimenCollectors;
  dsLink1.DataSet := DMG.qSampleCollectors;
  dsLink2.DataSet := DMG.qSamplePreps;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewCollector.Visible := True;
  pmcNewSamplePrep.Visible := True;
  // Set visible child buttons
  sbChildVerifications.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSurveys;
begin
  Caption := rsTitleSurveys;
  FSearch.DataSet := DMG.qSurveys;
  // Set the default data sorting
  AddSortedField(COL_SURVEY_DATE, sdDescending);

  // Set visible buttons
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowImages.Visible := True;
  sbShowDocs.Visible := True;

  // Set the print menu
  pmPrintSurveys.Visible := True;
  pmPrintSurveysByExpedition.Visible := True;
  pmPrintSurveysByLocality.Visible := True;
  pmPrintSurveysByProject.Visible := True;

  // Set the childs panel
  nbChilds.PageIndex := 0;
  if not Assigned(DMS) then
    DMS := TDMS.Create(nil);
  FChildTable := tbSurveyTeams;
  dsLink1.DataSet := DMS.qSurveyTeam;
  dsLink2.DataSet := DMS.qNetsEffort;
  dsLink3.DataSet := DMS.qWeatherLogs;
  dsLink4.DataSet := DMS.qCaptures;
  dsLink5.DataSet := DMS.qSightings;
  dsLink6.DataSet := DMS.qVegetation;
  pChildsBar.Visible := True;
  // Set the new child menu
  pmcNewSurveyMember.Visible := True;
  pmcNewMistnet.Visible := True;
  pmcNewWeatherLog.Visible := True;
  pmcNewCapture.Visible := True;
  pmcNewSighting.Visible := True;
  pmcNewVegetation.Visible := True;
  // Set visible child buttons
  sbChildVerifications.Visible := True;
end;

procedure TfrmCustomGrid.SetGridTaxonRanks;
begin
  Caption := rsTitleTaxonRanks;
  FSearch.DataSet := DMG.qTaxonRanks;
  // Set the default data sorting
  AddSortedField(COL_RANK_SEQUENCE, sdAscending);
end;

procedure TfrmCustomGrid.SetImages;
begin
  with qImages, SQL do
  begin
    case FTableType of
      tbNone: ;
      //tbGazetteer: ;
      //tbSamplingPlots: ;
      //tbPermanentNets: ;
      //tbInstitutions: ;
      //tbPeople: ;
      //tbProjects: ;
      //tbPermits: ;
      //tbZooTaxa: ;
      //tbBotanicTaxa: ;
      tbIndividuals:    Add('WHERE (img.active_status = 1) AND (img.individual_id = :individual_id)');
      tbCaptures:       Add('WHERE (img.active_status = 1) AND (img.capture_id = :capture_id)');
      tbFeathers:       Add('WHERE (img.active_status = 1) AND (img.feather_id = :feather_id)');
      tbNests:          Add('WHERE (img.active_status = 1) AND (img.nest_id = :nest_id)');
      tbNestRevisions:  Add('WHERE (img.active_status = 1) AND (img.nest_revision_id = :nest_revision_id)');
      tbEggs:           Add('WHERE (img.active_status = 1) AND (img.egg_id = :egg_id)');
      //tbMethods: ;
      //tbExpeditions: ;
      tbSurveys:        Add('WHERE (img.active_status = 1) AND (img.survey_id = :survey_id)');
      //tbWeatherLogs: ;
      tbSightings:      Add('WHERE (img.active_status = 1) AND (img.sighting_id = :sighting_id)');
      tbSpecimens:      Add('WHERE (img.active_status = 1) AND (img.specimen_id = :specimen_id)');
      //tbSamplePreps: ;
      //tbVegetation: ;
    end;
  end;

  if FTableType in [tbIndividuals, tbCaptures, tbNests, tbNestRevisions, tbEggs, tbSurveys, tbSightings, tbSpecimens] then
  begin
    qImages.SQL.Add('ORDER BY img.image_date, img.image_time ASC');
    qImages.DataSource := dsLink;
    //{$IFDEF DEBUG}
    //LogSQL(qImages.SQL);
    //{$ENDIF}
    qImages.Open;
  end;
end;

procedure TfrmCustomGrid.SetRecycle;
begin

  case FTableType of
    tbNone: ;
    tbUsers: ;
    tbRecordHistory: ;
    tbGazetteer:
    begin
      qRecycle.MacroByName('FID').AsString := COL_SITE_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_SITE_NAME;
    end;
    tbSamplingPlots:
    begin
      qRecycle.MacroByName('FID').AsString := COL_SAMPLING_PLOT_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbPermanentNets: ;
    tbInstitutions:
    begin
      qRecycle.MacroByName('FID').AsString := COL_INSTITUTION_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbPeople:
    begin
      qRecycle.MacroByName('FID').AsString := COL_PERSON_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbProjects:
    begin
      qRecycle.MacroByName('FID').AsString := COL_PROJECT_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_PROJECT_TITLE;
    end;
    tbProjectTeams: ;
    tbPermits:
    begin
      qRecycle.MacroByName('FID').AsString := COL_PERMIT_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_PERMIT_NAME;
    end;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa:
    begin
      qRecycle.MacroByName('FID').AsString := COL_TAXON_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_TAXON_NAME;
    end;
    tbBands:
    begin
      qRecycle.MacroByName('FID').AsString := COL_BAND_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbBandHistory: ;
    tbIndividuals:
    begin
      qRecycle.MacroByName('FID').AsString := COL_INDIVIDUAL_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbCaptures:
    begin
      qRecycle.MacroByName('FID').AsString := COL_CAPTURE_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbFeathers:
    begin
      qRecycle.MacroByName('FID').AsString := COL_FEATHER_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FEATHER_TRAIT;
    end;
    tbNests:
    begin
      qRecycle.MacroByName('FID').AsString := COL_NEST_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbNestOwners: ;
    tbNestRevisions:
    begin
      qRecycle.MacroByName('FID').AsString := COL_NEST_REVISION_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbEggs:
    begin
      qRecycle.MacroByName('FID').AsString := COL_EGG_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbMethods:
    begin
      qRecycle.MacroByName('FID').AsString := COL_METHOD_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_METHOD_NAME;
    end;
    tbExpeditions:
    begin
      qRecycle.MacroByName('FID').AsString := COL_EXPEDITION_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_EXPEDITION_NAME;
    end;
    tbSurveys:
    begin
      qRecycle.MacroByName('FID').AsString := COL_SURVEY_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbWeatherLogs: ;
    tbSightings:
    begin
      qRecycle.MacroByName('FID').AsString := COL_SIGHTING_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbSpecimens:
    begin
      qRecycle.MacroByName('FID').AsString := COL_SPECIMEN_ID;
      qRecycle.MacroByName('FNAME').AsString := COL_FULL_NAME;
    end;
    tbSamplePreps: ;
    tbSpecimenCollectors: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;
  qRecycle.MacroByName('FTABLE').AsString := TABLE_NAMES[FTableType];
  lblRecycleId.DataField := qRecycle.MacroByName('FID').AsString;
  lblRecycleName.DataField := qRecycle.MacroByName('FNAME').AsString;

  qRecycle.Open;

  lblRecycleWarning.Caption := Format(rsRecycleAutoDeleteInfo, [xSettings.ClearDeletedPeriod * 30]);
  pRecycleWarning.Visible := xSettings.ClearDeletedPeriod > 0;
end;

procedure TfrmCustomGrid.SetSidePanel(aValue: Boolean);
begin
  if FSidePanel <> oldSidePanel then
    oldSidePanel := FSidePanel;
  FSidePanel := aValue;
  pSide.Visible := FSidePanel;
  //SplitRight.Visible := pSide.Visible;
end;

procedure TfrmCustomGrid.SetSideIndex(aValue: Integer);
begin
  if ShowSidePanel then
  begin
    if FSideIndex <> oldSideIndex then
      oldSideIndex := FSideIndex;
    FSideIndex := aValue;
    cpSide.PageIndex := FSideIndex;
  end;
end;

procedure TfrmCustomGrid.SetSearchString(aValue: String);
begin
  if not FCanToggle then
    Exit;

  if FSearchString <> oldSearchString then
    oldSearchString := FSearchString;
  FSearchString := aValue;

  Search(FSearchString);
end;

procedure TfrmCustomGrid.SplitChildMoved(Sender: TObject);
begin
  FChildPanelFactor := pChild.Height / (pClient.Height - SplitChild.Height);
end;

procedure TfrmCustomGrid.SplitRightMoved(Sender: TObject);
begin
  FSidePanelFactor := pSide.Width / (ClientWidth);
end;

procedure TfrmCustomGrid.Summary;
begin
  if not sbShowSummary.Down then
    Exit;

  try
    pMsgSummary.Visible := True;
    gridSummary.BeginUpdate;
    case FTableType of
      tbNone: ;
      tbGazetteer:          SummaryGazetteer(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbSamplingPlots:      SummarySamplingPlots(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbPermanentNets: ;
      tbInstitutions:       SummaryInstitutions(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbPeople:             SummaryPeople(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbProjects:           SummaryProjects(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbProjectTeams: ;
      tbPermits:            SummaryPermits(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbTaxonRanks: ;
      //tbZooTaxa: ;
      tbBotanicTaxa:        SummaryBotanicTaxa(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbBands:              SummaryBands(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbIndividuals:        SummaryIndividuals(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbCaptures:           SummaryCaptures(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbFeathers:           SummaryFeathers(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbNests:              SummaryNests(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbNestOwners: ;
      tbNestRevisions:      SummaryNestRevisions(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbEggs:               SummaryEggs(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbMethods: ;
      tbExpeditions:        SummaryExpeditions(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbSurveys:            SummarySurveys(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      //tbWeatherLogs: ;
      tbSightings:          SummarySightings(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      tbSpecimens:          SummarySpecimens(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLString);
      //tbSamplePreps: ;
      //tbSpecimenCollectors: ;
    end;
    TranslateSummary(qChart);
    gridSummary.AutoAdjustColumns;
  finally
    gridSummary.EndUpdate;
    pMsgSummary.Visible := False;
  end;
end;

procedure TfrmCustomGrid.TimerChildUpdateTimer(Sender: TObject);
var
  DS: TDataSet;
  aId, aTotalProblems: Integer;
  aStatus: TRecordReviewStatus;
begin
  TimerRecordUpdate.Enabled := False;
  aTotalProblems := 0;

  if (isClosing) then
    Exit;

  case nbChilds.PageIndex of
    0: DS := dsLink1.DataSet;
    1: DS := dsLink2.DataSet;
    2: DS := dsLink3.DataSet;
    3: DS := dsLink4.DataSet;
    4: DS := dsLink5.DataSet;
    5: DS := dsLink6.DataSet;
  end;

  aId := DS.FieldByName(GetPrimaryKey(DS)).AsInteger;

  aStatus := GetRecordVerification(TABLE_NAMES[FChildTable], aId, aTotalProblems);

  case aStatus of
    rvwNotReviewed: sbChildVerifications.Caption := rsNotReviewed;
    rvwRecordOk: sbChildVerifications.Caption := rsRecordOK;
    rvwRecordWithProblems:
    begin
      if aTotalProblems > 1 then
        sbChildVerifications.Caption := Format(rsTotalProblemsPlural, [aTotalProblems])
      else
        sbChildVerifications.Caption := Format(rsTotalProblems, [aTotalProblems]);
    end;
  end;
end;

procedure TfrmCustomGrid.TimerOpenTimer(Sender: TObject);
begin
  TimerOpen.Enabled := False;
  //try
  //  Screen.BeginTempCursor(crAppStart);
    OpenAsync;
  //finally
  //  Screen.EndTempCursor(crAppStart);
  //end;
end;

procedure TfrmCustomGrid.TimerRecordUpdateTimer(Sender: TObject);
var
  aStatus: TRecordReviewStatus;
  aId, aTotalProblems: Integer;
  DS: TDataSet;
begin
  TimerRecordUpdate.Enabled := False;
  aTotalProblems := 0;

  if (isClosing) then
    Exit;

  DS := dsLink.DataSet;
  aId := DS.FieldByName(GetPrimaryKey(DS)).AsInteger;

  aStatus := GetRecordVerification(TABLE_NAMES[FTableType], aId, aTotalProblems);

  case aStatus of
    rvwNotReviewed: sbRecordVerifications.Caption := rsNotReviewed;
    rvwRecordOk: sbRecordVerifications.Caption := rsRecordOK;
    rvwRecordWithProblems:
    begin
      if aTotalProblems > 1 then
        sbRecordVerifications.Caption := Format(rsTotalProblemsPlural, [aTotalProblems])
      else
        sbRecordVerifications.Caption := Format(rsTotalProblems, [aTotalProblems]);
    end;
  end;
end;

procedure TfrmCustomGrid.TimerUpdateTimer(Sender: TObject);
begin
  if (isWorking) or (isOpening) or (isClosing) then
    Exit;

  if (oldRowHeight <> xSettings.DefaultRowHeight) then
    UpdateRowHeights;
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

  if FCanToggle then
    Search(FSearchString);
end;

procedure TfrmCustomGrid.tvDateFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  //Allowed := FCanToggle;
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

  if FCanToggle then
    Search(FSearchString);
end;

procedure TfrmCustomGrid.tvSiteFilterChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  //Allowed := FCanToggle;
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
  if not FCanToggle then
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
  //Allowed := FCanToggle;
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

procedure TfrmCustomGrid.UpdateAudioButtons(aDataSet: TDataSet);
begin
  if isClosing then
    Exit;

  case aDataSet.State of
    dsInactive:
    begin
      sbAddAudio.Enabled := False;
      sbAudioInfo.Enabled := False;
      sbDelAudio.Enabled := False;
      sbPlayAudio.Enabled := False;

    end;
    dsBrowse:
    begin
      sbAddAudio.Enabled := (dsLink.DataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbAudioInfo.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDelAudio.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbPlayAudio.Enabled := (aDataSet.RecordCount > 0);

    end;
    dsEdit, dsInsert:
    begin
      sbAddAudio.Enabled := False;
      sbAudioInfo.Enabled := False;
      sbDelAudio.Enabled := False;
      sbPlayAudio.Enabled := False;

    end;
  end;

  pmaAddAudio.Enabled := sbAddAudio.Enabled;
  pmaAudioInfo.Enabled := sbAudioInfo.Enabled;
  pmaDelAudio.Enabled := sbDelAudio.Enabled;
  pmaPlayAudio.Enabled := sbPlayAudio.Enabled;
  pmaRefreshAudios.Enabled := sbAddAudio.Enabled;
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
      sbRecordVerifications.Enabled := False;
      sbShareRecords.Enabled := False;
      sbPrint.Enabled := False;
      sbMarkRecords.Enabled := False;

      sbShowQuickFilters.Enabled := False;
      sbShowImages.Enabled := False;
      sbShowAudio.Enabled := False;
      sbShowDocs.Enabled := False;
      sbShowSummary.Enabled := False;
      sbShowRecycle.Enabled := False;
      sbShowMap.Enabled := False;
      sbShowColumns.Enabled := False;

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
      sbRecordVerifications.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbShareRecords.Enabled := (aDataSet.RecordCount > 0) and (ActiveUser.AllowExport);
      sbPrint.Enabled := (aDataSet.RecordCount > 0) and (ActiveUser.AllowPrint);
      sbMarkRecords.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);

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
      sbShowMap.Enabled := True;
      sbShowColumns.Enabled := True;

      sbRefreshRecords.Enabled := True;

      sbSaveRecord.Visible := False;
      sbCancelRecord.Visible := False;

      //navGrid.Enabled := True;
      if (Self.Parent is TPage) and (frmMain.navTabs.TabCount = frmMain.PGW.PageCount) then
        frmMain.navTabs.GetTabData((Self.Parent as TPage).PageIndex).TabModified := False;
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
      sbRecordVerifications.Enabled := False;
      sbShareRecords.Enabled := False;
      sbPrint.Enabled := False;
      sbMarkRecords.Enabled := False;

      sbShowQuickFilters.Enabled := False;
      sbShowImages.Enabled := False;
      sbShowAudio.Enabled := False;
      sbShowDocs.Enabled := False;
      sbShowSummary.Enabled := False;
      sbShowRecycle.Enabled := False;
      sbShowMap.Enabled := False;
      sbShowColumns.Enabled := False;

      sbCancelRecord.Visible := True;
      sbSaveRecord.Visible := True;

      sbRefreshRecords.Enabled := False;

      //navGrid.Enabled := False;
      if (Self.Parent is TPage) and (frmMain.navTabs.TabCount = frmMain.PGW.PageCount) then
        frmMain.navTabs.GetTabData((Self.Parent as TPage).PageIndex).TabModified := True;
      pSide.Enabled := False;
    end;

  end;
  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgInsert.Enabled := sbInsertRecord.Enabled;
  pmgEdit.Enabled := sbEditRecord.Enabled;
  pmgDel.Enabled := sbDelRecord.Enabled;
  pmgRecordHistory.Enabled := sbRecordHistory.Enabled;
  pmgRecordVerifications.Enabled := sbRecordVerifications.Enabled;
  sbInsertBatch.Enabled := sbInsertRecord.Enabled;
  sbQuickEntry.Enabled := sbInsertRecord.Enabled;
  sbMoreOptions.Enabled := sbShareRecords.Enabled;

  sbClearFilters.Enabled := FSearch.QuickFilters.Count > 0;
  sbClearAllFilters.Enabled := sbClearFilters.Enabled;
  sbEmptyClearAll.Visible := (FSearch.QuickFilters.Count > 0) or (FSearchString <> EmptyStr);

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
      5: UpdateChildButtons(dsLink6.DataSet);
    end;
end;

procedure TfrmCustomGrid.UpdateChildButtons(aDataSet: TDataSet);
begin
  if isClosing then
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
      sbChildVerifications.Enabled := False;
      sbShareChild.Enabled := False;
      sbRefreshChild.Enabled := True;

      pSide.Enabled := False;
    end;
    dsBrowse:
    begin
      sbAddChild.Enabled := (dsLink.DataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbEditChild.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDelChild.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbChildHistory.Enabled := (aDataSet.RecordCount > 0);
      sbChildVerifications.Enabled := (aDataSet.RecordCount > 0);
      sbShareChild.Enabled := (aDataSet.RecordCount > 0) and (ActiveUser.AllowExport);
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
      sbChildVerifications.Enabled := False;
      sbShareChild.Enabled := False;
      sbRefreshChild.Enabled := False;

      pSide.Enabled := False;
    end;
  end;
  sbQuickEntryChild.Enabled := sbAddChild.Enabled;
  eAddChild.Enabled := sbAddChild.Enabled;
  sbAddNetsBatch.Enabled := sbAddChild.Enabled;
  sbAddFeathersBatch.Enabled := sbAddChild.Enabled;
  pmcEdit.Enabled := sbEditChild.Enabled;
  pmcDel.Enabled := sbDelChild.Enabled;
  pmcRefresh.Enabled := sbRefreshChild.Enabled;
  pmcRecordHistory.Enabled := sbChildHistory.Enabled;
  pmcRecordVerifications.Enabled := sbChildVerifications.Enabled;

  //UpdateChildCount;
end;

procedure TfrmCustomGrid.UpdateChildCount;
var
  PanelTab: TCustomPanelTab;
begin
  if isClosing then
    Exit;

  if not dsLink.DataSet.Active then
  begin
    for PanelTab in panelTabs do
      PanelTab.UpdateCounter(0);

    Exit;
  end;

  case FTableType of
    //tbNone: ;
    //tbUsers: ;
    //tbRecordHistory: ;
    //tbGazetteer: ;
    tbSamplingPlots:
    begin
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
    end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects:
    begin
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
      panelTabs[1].UpdateCounter(dsLink2.DataSet.RecordCount);
      panelTabs[2].UpdateCounter(dsLink3.DataSet.RecordCount);
      panelTabs[3].UpdateCounter(dsLink4.DataSet.RecordCount);
      panelTabs[4].UpdateCounter(dsLink5.DataSet.RecordCount);
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
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
      panelTabs[1].UpdateCounter(dsLink2.DataSet.RecordCount);
      panelTabs[2].UpdateCounter(dsLink3.DataSet.RecordCount);
      panelTabs[3].UpdateCounter(dsLink4.DataSet.RecordCount);
      panelTabs[4].UpdateCounter(dsLink5.DataSet.RecordCount);
    end;
    //tbCaptures: ;
    //tbMolts: ;
    tbNests:
    begin
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
      panelTabs[1].UpdateCounter(dsLink2.DataSet.RecordCount);
      panelTabs[2].UpdateCounter(dsLink3.DataSet.RecordCount);
    end;
    //tbNestRevisions: ;
    //tbEggs: ;
    //tbMethods: ;
    tbExpeditions:
    begin
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
    end;
    tbSurveys:
    begin
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
      panelTabs[1].UpdateCounter(dsLink2.DataSet.RecordCount);
      panelTabs[2].UpdateCounter(dsLink3.DataSet.RecordCount);
      panelTabs[3].UpdateCounter(dsLink4.DataSet.RecordCount);
      panelTabs[4].UpdateCounter(dsLink5.DataSet.RecordCount);
      panelTabs[5].UpdateCounter(dsLink6.DataSet.RecordCount);
    end;
    //tbSurveyTeams: ;
    //tbNetsEffort: ;
    tbSightings: ;
    tbSpecimens:
    begin
      panelTabs[0].UpdateCounter(dsLink1.DataSet.RecordCount);
      panelTabs[1].UpdateCounter(dsLink2.DataSet.RecordCount);
    end;
    //tbSamplePreps: ;
    //tbImages: ;
    //tbAudioLibrary: ;
  end;

  UpdateChildStatus;
end;

procedure TfrmCustomGrid.UpdateChildRightPanel;
var
  aProjectId, aRubricId: Integer;
begin
  if FChildTable <> tbProjectBudgets then
    Exit;

  aProjectId := dsLink.DataSet.FieldByName(COL_PROJECT_ID).AsInteger;
  aRubricId := dsLink4.DataSet.FieldByName(COL_BUDGET_ID).AsInteger;

  if FTableType = tbProjects then
  begin
    txtProjectBalance.Caption := Format('%s / %s', [FormatFloat(MASK_TWO_DECIMAL, GetProjectBalance(aProjectId)),
      FormatFloat(MASK_TWO_DECIMAL, GetProjectTotalBudget(aProjectId))]);
    txtRubricBalance.Caption := Format('%s / %s', [FormatFloat(MASK_TWO_DECIMAL, GetRubricBalance(aRubricId)),
      FormatFloat(MASK_TWO_DECIMAL, dsLink4.DataSet.FieldByName(COL_AMOUNT).AsFloat)]);
  end;
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
    tbSamplingPlots:
    begin
      DS := dsLink1;
    end;
    //tbPermanentNets: ;
    //tbInstitutions: ;
    //tbPeople: ;
    tbProjects:
    begin
      case nbChilds.PageIndex of
        0: DS := dsLink1;
        1: DS := dsLink2;
        2: DS := dsLink3;
        3: DS := dsLink4;
        4: DS := dsLink5;
      end;
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
        5: DS := dsLink6;
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

procedure TfrmCustomGrid.UpdateDocButtons(aDataSet: TDataSet);
begin
  if isClosing then
    Exit;

  case aDataSet.State of
    dsInactive:
    begin
      sbAddDoc.Enabled := False;
      sbDocInfo.Enabled := False;
      sbDelDoc.Enabled := False;
      sbOpenDoc.Enabled := False;

    end;
    dsBrowse:
    begin
      sbAddDoc.Enabled := (dsLink.DataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDocInfo.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDelDoc.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbOpenDoc.Enabled := (aDataSet.RecordCount > 0);

    end;
    dsEdit, dsInsert:
    begin
      sbAddDoc.Enabled := False;
      sbDocInfo.Enabled := False;
      sbDelDoc.Enabled := False;
      sbOpenDoc.Enabled := False;

    end;
  end;

  pmdAddDoc.Enabled := sbAddDoc.Enabled;
  pmdDocInfo.Enabled := sbDocInfo.Enabled;
  pmdDelDoc.Enabled := sbDelDoc.Enabled;
  pmdOpenDoc.Enabled := sbOpenDoc.Enabled;
  pmdRefreshDocs.Enabled := sbAddDoc.Enabled;
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
    if aGrid.Columns.ColumnByFieldname(aSearch.SortFields[i].FieldName) = nil then
      Break;

    case aSearch.SortFields[i].Direction of
      sdAscending: aGrid.Columns.ColumnByFieldname(aSearch.SortFields[i].FieldName).Title.ImageIndex := ArrowAsc;
      sdDescending: aGrid.Columns.ColumnByFieldname(aSearch.SortFields[i].FieldName).Title.ImageIndex := ArrowDesc;
    end;
  end;
end;

procedure TfrmCustomGrid.UpdateImageButtons(aDataSet: TDataSet);
begin
  if isClosing then
    Exit;

  case aDataSet.State of
    dsInactive:
    begin
      sbAddImage.Enabled := False;
      sbImageInfo.Enabled := False;
      sbDelImage.Enabled := False;
      sbViewImage.Enabled := False;

    end;
    dsBrowse:
    begin
      sbAddImage.Enabled := (dsLink.DataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbImageInfo.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbDelImage.Enabled := (aDataSet.RecordCount > 0) and not (TSQLQuery(aDataSet).ReadOnly);
      sbViewImage.Enabled := (aDataSet.RecordCount > 0);

    end;
    dsEdit, dsInsert:
    begin
      sbAddImage.Enabled := False;
      sbImageInfo.Enabled := False;
      sbDelImage.Enabled := False;
      sbViewImage.Enabled := False;

    end;
  end;

  pmiAddImage.Enabled := sbAddImage.Enabled;
  pmiImageInfo.Enabled := sbImageInfo.Enabled;
  pmiDelImage.Enabled := sbDelImage.Enabled;
  pmiViewImage.Enabled := sbViewImage.Enabled;
  pmiRefreshImages.Enabled := sbViewImage.Enabled;
end;

procedure TfrmCustomGrid.UpdateRecycleButtons(aDataset: TDataSet);
begin
  case aDataset.State of
    dsInactive:
    begin
      sbRestoreRecord.Enabled := False;
      sbDelPermanently.Enabled := False;
    end;
    dsBrowse:
    begin
      sbRestoreRecord.Enabled := (aDataset.RecordCount > 0);
      sbDelPermanently.Enabled := (aDataset.RecordCount > 0);
    end;
    dsEdit, dsInsert:
    begin
      sbRestoreRecord.Enabled := False;
      sbDelPermanently.Enabled := False;
    end;
  end;
end;

procedure TfrmCustomGrid.UpdateRowHeights;
begin
  DBG.DefaultRowHeight := xSettings.DefaultRowHeight;

  gridChild1.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridChild2.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridChild3.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridChild4.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridChild5.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridChild6.DefaultRowHeight := xSettings.DefaultRowHeight;

  gridRecord.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridAudios.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridDocs.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridSummary.DefaultRowHeight := xSettings.DefaultRowHeight;
  gridColumns.DefaultRowHeight := xSettings.DefaultRowHeight;
end;

procedure TfrmCustomGrid.UpdateFilterPanels;
begin
  case TableType of
    tbGazetteer:      UpdateFilterPanelsGazetteer;
    tbInstitutions:   UpdateFilterPanelsInstitutions;
    tbPeople:         UpdateFilterPanelsPeople;
    tbProjects:       UpdateFilterPanelsProjects;
    tbPermits:        UpdateFilterPanelsPermits;
    tbSamplingPlots:  UpdateFilterPanelsSamplingPlots;
    tbBotanicTaxa:    UpdateFilterPanelsBotanicTaxa;
    tbZooTaxa:        UpdateFilterPanelsZooTaxa;
    tbBands:          UpdateFilterPanelsBands;
    tbIndividuals:    UpdateFilterPanelsIndividuals;
    tbCaptures:       UpdateFilterPanelsCaptures;
    tbFeathers:       UpdateFilterPanelsFeathers;
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
  pReportedFilter.Visible := True;
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
  cbAgeFilter.Items.CommaText := rsCaptionAll + ',' + rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
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
  pNeedsReviewFilter.Visible := True;
  pEscapedFilter.Visible := True;
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
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pProjectFilter.Visible := True;
end;

procedure TfrmCustomGrid.UpdateFilterPanelsFeathers;
begin
  pTaxonFilters.Visible := True;
  LoadTaxaTreeData(FTableType, tvTaxaFilter, 0);
  pDatesFilters.Visible := True;
  LoadDateTreeData(FTableType, tvDateFilter);
  pSiteFilters.Visible := True;
  LoadSiteTreeData(FTableType, tvSiteFilter, 4);
  pIndividualFilter.Visible := True;
  pPersonFilter.Visible := True;
  pTimeFilters.Visible := True;
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
  cbAgeFilter.Items.Clear;
  cbAgeFilter.Items.CommaText := rsCaptionAll + ',' + rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  pAgingFilters.Visible := True;
  pCloacalProtuberanceFilter.Visible := False;
  pBroodPatchFilter.Visible := False;
  pHowSexedFilter.Visible := False;
  pSexFilter.Rounding.RoundOptions := [];
  cbSexFilter.Items.Clear;
  cbSexFilter.Items.CommaText := rsCaptionAll + ',' + rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
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

procedure TfrmCustomGrid.UpdateFilterPanelsSamplingPlots;
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

