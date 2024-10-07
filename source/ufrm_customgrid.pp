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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StrUtils, RegExpr, DB, SQLDB, DateUtils, Grids,
  DBGrids, ExtCtrls, EditBtn, StdCtrls, ComCtrls, Menus, LCLIntf, Character, Buttons, CheckLst, DBCtrls,
  laz.VirtualTrees, TAGraph, TASeries, TADbSource, TASources, LR_PGrid, TAGUIConnectorBGRA,
  atshapelinebgra, BCPanel, DBControlGrid, cbs_datatypes, cbs_filters, Types, ImgList, ToggleSwitch,
  mvMapViewer, mvDE_BGRA, mvTypes, mvGpsObj, mvDrawingEngine;

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


  { TfrmCustomGrid }

  TfrmCustomGrid = class(TForm)
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
    pmPrintBandsBalance: TMenuItem;
    pmPrintIndividuals: TMenuItem;
    pmPrintIndividualsByTaxon: TMenuItem;
    pmPrintIndividualsByParents: TMenuItem;
    pmPrintCaptures: TMenuItem;
    pmPrintCapturesByProject: TMenuItem;
    pmPrintExpeditionsByLocality: TMenuItem;
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
    pmvAddVerification: TMenuItem;
    pmvViewVerifications: TMenuItem;
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
    pmVerifications: TPopupMenu;
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
    lblChildCount2: TLabel;
    lblChildTag2: TLabel;
    lblChildCount4: TLabel;
    lblChildTag4: TLabel;
    lblChildCount3: TLabel;
    lblChildTag3: TLabel;
    lblSkullOssificationFilter: TLabel;
    lblChildCount5: TLabel;
    lblChildTag5: TLabel;
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
    pChildCount2: TBCPanel;
    pChildTag2: TBCPanel;
    pChildCount4: TBCPanel;
    pChildTag4: TBCPanel;
    pInstitutionFilter: TBCPanel;
    pRecordStatus: TBCPanel;
    pChildStatus: TBCPanel;
    pRecordToolbar: TBCPanel;
    pColumnsToolbar: TBCPanel;
    pImagesToolbar: TBCPanel;
    pRecycleContent: TPanel;
    pChildCount3: TBCPanel;
    pChildTag3: TBCPanel;
    pSkullOssificationFilter: TBCPanel;
    pChildCount5: TBCPanel;
    pChildTag5: TBCPanel;
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
    qAudiosfamily_id: TLongintField;
    qAudiosfilter_model: TStringField;
    qAudiosfull_name: TStringField;
    qAudiosgenus_id: TLongintField;
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
    qAudiosorder_id: TLongintField;
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
    qAudiosspecies_id: TLongintField;
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
    qImagescountry_id: TLongintField;
    qImagesegg_id: TLongintField;
    qImagesegg_name: TStringField;
    qImagesexported_status: TBooleanField;
    qImagesfamily_id: TLongintField;
    qImagesgenus_id: TLongintField;
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
    qImagesmunicipality_id: TLongintField;
    qImagesnest_id: TLongintField;
    qImagesnest_name: TStringField;
    qImagesnest_revision_id: TLongintField;
    qImagesorder_id: TLongintField;
    qImagesrevision_name: TStringField;
    qImagessighting_id: TLongintField;
    qImagessighting_name: TStringField;
    qImagesspecies_id: TLongintField;
    qImagesspecimen_id: TLongintField;
    qImagesspecimen_name: TStringField;
    qImagesstate_id: TLongintField;
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
    Separator21: TMenuItem;
    Separator22: TMenuItem;
    Separator23: TMenuItem;
    Separator24: TMenuItem;
    Separator25: TMenuItem;
    Separator26: TShapeLineBGRA;
    Separator27: TMenuItem;
    Separator28: TMenuItem;
    Separator29: TMenuItem;
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
    SplitRight: TSplitter;
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
    TimerUpdate: TTimer;
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
    procedure DBGColEnter(Sender: TObject);
    procedure DBGColExit(Sender: TObject);
    procedure DBGContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DBGDblClick(Sender: TObject);
    procedure DBGEditButtonClick(Sender: TObject);
    procedure DBGEditingDone(Sender: TObject);
    procedure dbgImagesDblClick(Sender: TObject);
    procedure DBGKeyPress(Sender: TObject; var Key: char);
    procedure DBGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DBGMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure DBGPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
      AState: TGridDrawState);
    procedure DBGSelectEditor(Sender: TObject; Column: TColumn; var Editor: TWinControl);
    procedure dsAudiosDataChange(Sender: TObject; Field: TField);
    procedure dsAudiosStateChange(Sender: TObject);
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
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure dsLinkStateChange(Sender: TObject);
    procedure dsRecycleStateChange(Sender: TObject);
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
    procedure gridChild1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure gridChild1DblClick(Sender: TObject);
    procedure gridColumnsCheckboxToggled(Sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure iHeadersGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer;
      var AResultWidth: Integer);
    procedure mapGeoDrawGpsPoint(Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure pChildTag1Click(Sender: TObject);
    procedure pChildTag1MouseEnter(Sender: TObject);
    procedure pChildTag1MouseLeave(Sender: TObject);
    procedure pClientResize(Sender: TObject);
    procedure pmaRefreshAudiosClick(Sender: TObject);
    procedure pmcColumnSortAscClick(Sender: TObject);
    procedure pmcColumnSortDescClick(Sender: TObject);
    procedure pmcHideColumnClick(Sender: TObject);
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
    procedure pmiRefreshImagesClick(Sender: TObject);
    procedure pmmInvertMarkedClick(Sender: TObject);
    procedure pmmMarkAllClick(Sender: TObject);
    procedure pmmMarkAllColumnsClick(Sender: TObject);
    procedure pmmUnmarkAllClick(Sender: TObject);
    procedure pmmUnmarkAllColumnsClick(Sender: TObject);
    procedure pmPrintBandsByCarrierClick(Sender: TObject);
    procedure pmPrintBandsByStatusClick(Sender: TObject);
    procedure pmPrintBandsClick(Sender: TObject);
    procedure pmPrintBandsWithHistoryClick(Sender: TObject);
    procedure pmPrintGridClick(Sender: TObject);
    procedure pmPrintInstitutionsClick(Sender: TObject);
    procedure pmPrintMethodsClick(Sender: TObject);
    procedure pmPrintPermitsClick(Sender: TObject);
    procedure pmPrintProjectsClick(Sender: TObject);
    procedure pmPrintResearchersClick(Sender: TObject);
    procedure pmtClearSelectionClick(Sender: TObject);
    procedure pmtColapseAllClick(Sender: TObject);
    procedure pmtExpandAllClick(Sender: TObject);
    procedure pmtRefreshClick(Sender: TObject);
    procedure pmvAddVerificationClick(Sender: TObject);
    procedure pmvViewVerificationsClick(Sender: TObject);
    procedure qAudiosBeforePost(DataSet: TDataSet);
    procedure qImagesBeforePost(DataSet: TDataSet);
    procedure sbAddChildClick(Sender: TObject);
    procedure sbAddImageClick(Sender: TObject);
    procedure sbAddNetsBatchClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbChildVerificationsClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure sbColumnHideClick(Sender: TObject);
    procedure sbColumnWidthAutoAdjustClick(Sender: TObject);
    procedure sbDelChildClick(Sender: TObject);
    procedure sbDelImageClick(Sender: TObject);
    procedure sbDelPermanentlyClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbEditChildClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFirstChildClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbImageInfoClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastChildClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbMarkColumnsClick(Sender: TObject);
    procedure sbMarkRecordsClick(Sender: TObject);
    procedure sbMoveColumnDownClick(Sender: TObject);
    procedure sbMoveColumnUpClick(Sender: TObject);
    procedure sbNextChildClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbPriorChildClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
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
    procedure sbShowRecordClick(Sender: TObject);
    procedure sbViewImageClick(Sender: TObject);
    procedure SetFilters(Sender: TObject);
    procedure SplitChildMoved(Sender: TObject);
    procedure SplitRightMoved(Sender: TObject);
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
    procedure ApplyDarkMode;
    procedure CellKeyPress(Sender: TObject; var Key: Char);

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

    procedure GetColumns;

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

    procedure LoadColumns;
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

    procedure RefreshMap;
    procedure RefreshMapSurvey;

    procedure SaveColumns;

    procedure SetGridColumns(aTable: TTableType; aGrid: TDBGrid);
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

    procedure SetAudios;
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

    procedure Summary;

    procedure UpdateButtons(aDataSet: TDataSet);
    procedure UpdateChildBar;
    procedure UpdateChildButtons(aDataSet: TDataSet);
    procedure UpdateChildCount;
    procedure UpdateChildStatus;
    procedure UpdateGridTitles(aGrid: TDBGrid; aSearch: TCustomSearch);
    procedure UpdateImageButtons(aDataSet: TDataSet);
    procedure UpdateAudioButtons(aDataSet: TDataSet);

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
  cbs_locale, cbs_global, cbs_system, cbs_themes, cbs_gis, cbs_birds, cbs_editdialogs, cbs_dialogs, cbs_math,
  cbs_finddialogs, cbs_data, cbs_getvalue, cbs_taxonomy, cbs_datacolumns, cbs_blobs, cbs_print, udlg_progress,
  {$IFDEF DEBUG}cbs_debug,{$ENDIF} uDarkStyleParams,
  udm_main, udm_grid, udm_individuals, udm_breeding, udm_sampling, ufrm_main, ubatch_neteffort;

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
    aGrid.Columns.Clear;

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
    case FSearch.DataSet.FieldByName(aFieldName).DataType of
      ftUnknown, ftGuid:
        FSearch.SortFields[p].SortType := stNone;
      ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo:
        FSearch.SortFields[p].SortType := stAlphanumeric;
      ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc, ftBytes, ftVarBytes:
        FSearch.SortFields[p].SortType := stNumeric;
      ftBoolean:
        FSearch.SortFields[p].SortType := stBoolean;
      ftFloat, ftCurrency, ftBCD, ftFMTBcd:
        FSearch.SortFields[p].SortType := stNumeric;
      ftDate, ftTime, ftDateTime, ftTimeStamp:
        FSearch.SortFields[p].SortType := stDateTime;
    else
      FSearch.SortFields[p].SortType := stNone;
    end;
  FSearch.SortFields[p].Direction := aDirection;
  FSearch.SortFields[p].Collation := aCollation;
  FSearch.SortFields[p].Lookup    := IsAnAlias;

  UpdateGridTitles(DBG, FSearch);
end;

procedure TfrmCustomGrid.ApplyDarkMode;
begin
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
  pMapToolbar.Background.Color := clCardBGDefaultDark;
  pMapToolbar.Border.Color := clCardBGSecondaryDark;
  pColumnsToolbar.Background.Color := clCardBGDefaultDark;
  pColumnsToolbar.Border.Color := clCardBGSecondaryDark;
  pRecycleToolbar.Background.Color := clCardBGDefaultDark;
  pRecycleToolbar.Border.Color := clCardBGSecondaryDark;
  pRecycleWarning.Background.Color := clSystemAttentionBGDark;
  pRecycleWarning.Border.Color := clSystemAttentionFGDark;
  icoRecycleWarning.Images := iIconsDark;
  pMsgSummary.Background.Color := clCardBGDefaultDark;
  pMsgSummary.Border.Color := clCardBGSecondaryDark;
  pMsgSummary.FontEx.Color := clTextPrimaryDark;
  pMsgSummary.Color := gridSummary.Color;

  DBG.TitleImageList := iHeadersDark;
  pmGrid.Images := iButtonsDark;
  pmGridChild.Images := iButtonsDark;
  pmColumn.Images := iButtonsDark;
  pmPrint.Images := iButtonsDark;
  pmTree.Images := iButtonsDark;
  pmRecycle.Images := iButtonsDark;
  pmMark.Images := iButtonsDark;
  pmMarkColumns.Images := iButtonsDark;
  pmVerifications.Images := iButtonsDark;
  pmImages.Images := iButtonsDark;
  pmAddChild.Images := DMM.iAddMenuDark;

  pEmptyQuery.Background.Color := clCardBGDefaultDark;
  pEmptyQuery.Border.Color := clCardBGSecondaryDark;
  pEmptyQuery.Color := DBG.Color;
  icoEmptyQuery.Images := iButtonsDark;

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
  pChildTag1.Background.Color := clCardBGSecondaryDark;
  pChildTag1.Border.Color := clSolidBGTertiaryDark;
  pChildTag1.Color := clCardBGDefaultDark;
  pChildTag2.Background.Color := clCardBGSecondaryDark;
  pChildTag2.Border.Color := clSolidBGTertiaryDark;
  pChildTag2.Color := clCardBGDefaultDark;
  pChildTag3.Background.Color := clCardBGSecondaryDark;
  pChildTag3.Border.Color := clSolidBGTertiaryDark;
  pChildTag3.Color := clCardBGDefaultDark;
  pChildTag4.Background.Color := clCardBGSecondaryDark;
  pChildTag4.Border.Color := clSolidBGTertiaryDark;
  pChildTag4.Color := clCardBGDefaultDark;
  pChildTag5.Background.Color := clCardBGSecondaryDark;
  pChildTag5.Border.Color := clSolidBGTertiaryDark;
  pChildTag5.Color := clCardBGDefaultDark;
  pChildCount1.Color := pChildTag1.Background.Color;
  pChildCount2.Color := pChildTag2.Background.Color;
  pChildCount3.Color := pChildTag3.Background.Color;
  pChildCount4.Color := pChildTag4.Background.Color;
  pChildCount5.Color := pChildTag5.Background.Color;

  pSideToolbar.Color := clSolidBGQuaternaryDark;

  sbInsertRecord.Images := iButtonsDark;
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
  sbAddNetsBatch.Images := iButtonsDark;
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
  sbRestoreRecord.Images := iButtonsDark;
  sbDelPermanently.Images := iButtonsDark;

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
  tvSiteFilter.Colors.TreeLineColor := clTextSecondaryDark;
  tvDateFilter.Colors.TreeLineColor := clTextSecondaryDark;
  tvTaxaFilter.Colors.TreeLineColor := clTextSecondaryDark;
  tsTaxonomyClements.Color := pTaxonomyClementsFilter.Background.Color;
  tsTaxonomyIoc.Color := pTaxonomyIocFilter.Background.Color;
  tsTaxonomyCbro.Color := pTaxonomyCbroFilter.Background.Color;

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
      if FieldName = 'taxon_name' then
        FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, 'taxon_id', 'taxon_name', True, Key);
      if FieldName = 'nidoparasite_name' then
        FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, 'nidoparasite_id', 'nidoparasite_name', True, Key);

      if FieldName = 'parent_taxon_name' then
        FindBotanicDlg([tfAll], InplaceEditor,
          DataSource.DataSet, 'parent_taxon_id', 'parent_taxon_name', Key);
      if FieldName = 'valid_name' then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, 'valid_id', 'valid_name', Key);
      if FieldName = 'support_plant_1_name' then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, 'support_plant_1_id', 'support_plant_1_name', Key);
      if FieldName = 'support_plant_2_name' then
        FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
          DataSource.DataSet, 'support_plant_2_id', 'support_plant_2_name', Key);

      if FieldName = 'country_name' then
        FindSiteDlg([gfCountries], InplaceEditor, DataSource.DataSet, 'country_id', 'country_name', Key);
      if FieldName = 'state_name' then
        FindSiteDlg([gfStates], InplaceEditor, DataSource.DataSet, 'state_id', 'state_name', Key);
      if FieldName = 'municipality_name' then
        FindSiteDlg([gfCities], InplaceEditor, DataSource.DataSet, 'municipality_id', 'municipality_name', Key);
      if FieldName = 'locality_name' then
        FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, 'locality_id', 'locality_name', Key);
      if FieldName = 'parent_site_name' then
        FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, 'parent_site_id', 'parent_site_name', Key);

      if FieldName = 'institution_name' then
        FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, 'institution_id', 'institution_name', False, Key);
      if FieldName = 'supplier_name' then
        FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, 'supplier_id', 'supplier_name', False, Key);

      if FieldName = 'expedition_name' then
        FindDlg(tbExpeditions, InplaceEditor, DataSource.DataSet, 'expedition_id', 'expedition_name', False, Key);

      if FieldName = 'survey_name' then
        FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, 'survey_id', 'survey_name', False, Key);

      if FieldName = 'net_station_name' then
        FindDlg(tbNetStations, InplaceEditor, DataSource.DataSet, 'net_station_id', 'net_station_name', False, Key);

      if FieldName = 'observer_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_id', 'observer_name', False, Key);
      if FieldName = 'observer_1_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_1_id', 'observer_1_name', False, Key);
      if FieldName = 'observer_2_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_2_id', 'observer_2_name', False, Key);
      if FieldName = 'carrier_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'carrier_id', 'carrier_name', False, Key);
      if FieldName = 'bander_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'bander_id', 'bander_name', False, Key);
      if FieldName = 'annotator_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'annotator_id', 'annotator_name', False, Key);
      if FieldName = 'photographer_1_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'photographer_1_id', 'photographer_1_name', False, Key);
      if FieldName = 'photographer_2_name' then
        FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'photographer_2_id', 'photographer_2_name', False, Key);

      if FieldName = 'project_name' then
        FindDlg(tbProjects, InplaceEditor, DataSource.DataSet, 'project_id', 'project_name', False, Key);

      if FieldName = 'individual_name' then
        FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'individual_id', 'individual_name', False, Key);
      if FieldName = 'father_name' then
        FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'father_id', 'father_name', False, Key);
      if FieldName = 'mother_name' then
        FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'mother_id', 'mother_name', False, Key);

      if FieldName = 'nest_name' then
        FindDlg(tbNests, InplaceEditor, DataSource.DataSet, 'nest_id', 'nest_name', False, Key);

      if FieldName = 'egg_name' then
        FindDlg(tbEggs, InplaceEditor, DataSource.DataSet, 'egg_id', 'egg_name', False, Key);

      if FieldName = 'band_full_name' then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_full_name', False, Key);
      if FieldName = 'band_name' then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_name', False, Key);
      if FieldName = 'double_band_name' then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'double_band_id', 'double_band_name', False, Key);
      if FieldName = 'removed_band_name' then
        FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'removed_band_id', 'removed_band_name', False, Key);
    end;
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    with (Sender as TDBGrid), SelectedColumn do
    begin
      if FieldName = 'taxon_name' then
      begin
        DataSource.DataSet.FieldByName('taxon_id').Clear;
        DataSource.DataSet.FieldByName('taxon_name').Clear;
      end;
      if FieldName = 'nidoparasite_name' then
      begin
        DataSource.DataSet.FieldByName('nidoparasite_id').Clear;
        DataSource.DataSet.FieldByName('nidoparasite_name').Clear;
      end;

      if FieldName = 'parent_taxon_name' then
      begin
        DataSource.DataSet.FieldByName('parent_taxon_id').Clear;
        DataSource.DataSet.FieldByName('parent_taxon_name').Clear;
      end;
      if FieldName = 'valid_name' then
      begin
        DataSource.DataSet.FieldByName('valid_id').Clear;
        DataSource.DataSet.FieldByName('valid_name').Clear;
      end;
      if FieldName = 'support_plant_1_name' then
      begin
        DataSource.DataSet.FieldByName('support_plant_1_id').Clear;
        DataSource.DataSet.FieldByName('support_plant_1_name').Clear;
      end;
      if FieldName = 'support_plant_2_name' then
      begin
        DataSource.DataSet.FieldByName('support_plant_2_id').Clear;
        DataSource.DataSet.FieldByName('support_plant_2_name').Clear;
      end;

      if FieldName = 'country_name' then
      begin
        DataSource.DataSet.FieldByName('country_id').Clear;
        DataSource.DataSet.FieldByName('country_name').Clear;
      end;
      if FieldName = 'state_name' then
      begin
        DataSource.DataSet.FieldByName('state_id').Clear;
        DataSource.DataSet.FieldByName('state_name').Clear;
      end;
      if FieldName = 'municipality_name' then
      begin
        DataSource.DataSet.FieldByName('municipality_id').Clear;
        DataSource.DataSet.FieldByName('municipality_name').Clear;
      end;
      if FieldName = 'locality_name' then
      begin
        DataSource.DataSet.FieldByName('locality_id').Clear;
        DataSource.DataSet.FieldByName('locality_name').Clear;
      end;
      if FieldName = 'parent_site_name' then
      begin
        DataSource.DataSet.FieldByName('parent_site_id').Clear;
        DataSource.DataSet.FieldByName('parent_site_name').Clear;
      end;

      if FieldName = 'institution_name' then
      begin
        DataSource.DataSet.FieldByName('institution_id').Clear;
        DataSource.DataSet.FieldByName('institution_name').Clear;
      end;
      if FieldName = 'supplier_name' then
      begin
        DataSource.DataSet.FieldByName('supplier_id').Clear;
        DataSource.DataSet.FieldByName('supplier_name').Clear;
      end;

      if FieldName = 'expedition_name' then
      begin
        DataSource.DataSet.FieldByName('expedition_id').Clear;
        DataSource.DataSet.FieldByName('expedition_name').Clear;
      end;

      if FieldName = 'survey_name' then
      begin
        DataSource.DataSet.FieldByName('survey_id').Clear;
        DataSource.DataSet.FieldByName('survey_name').Clear;
      end;

      if FieldName = 'net_station_name' then
      begin
        DataSource.DataSet.FieldByName('net_station_id').Clear;
        DataSource.DataSet.FieldByName('net_station_name').Clear;
      end;

      if FieldName = 'observer_name' then
      begin
        DataSource.DataSet.FieldByName('observer_id').Clear;
        DataSource.DataSet.FieldByName('observer_name').Clear;
      end;
      if FieldName = 'observer_1_name' then
      begin
        DataSource.DataSet.FieldByName('observer_1_id').Clear;
        DataSource.DataSet.FieldByName('observer_1_name').Clear;
      end;
      if FieldName = 'observer_2_name' then
      begin
        DataSource.DataSet.FieldByName('observer_2_id').Clear;
        DataSource.DataSet.FieldByName('observer_2_name').Clear;
      end;
      if FieldName = 'carrier_name' then
      begin
        DataSource.DataSet.FieldByName('carrier_id').Clear;
        DataSource.DataSet.FieldByName('carrier_name').Clear;
      end;
      if FieldName = 'bander_name' then
      begin
        DataSource.DataSet.FieldByName('bander_id').Clear;
        DataSource.DataSet.FieldByName('bander_name').Clear;
      end;
      if FieldName = 'annotator_name' then
      begin
        DataSource.DataSet.FieldByName('annotator_id').Clear;
        DataSource.DataSet.FieldByName('annotator_name').Clear;
      end;
      if FieldName = 'photographer_1_name' then
      begin
        DataSource.DataSet.FieldByName('photographer_1_id').Clear;
        DataSource.DataSet.FieldByName('photographer_1_name').Clear;
      end;
      if FieldName = 'photographer_2_name' then
      begin
        DataSource.DataSet.FieldByName('photographer_2_id').Clear;
        DataSource.DataSet.FieldByName('photographer_2_name').Clear;
      end;

      if FieldName = 'project_name' then
      begin
        DataSource.DataSet.FieldByName('project_id').Clear;
        DataSource.DataSet.FieldByName('project_name').Clear;
      end;

      if FieldName = 'individual_name' then
      begin
        DataSource.DataSet.FieldByName('individual_id').Clear;
        DataSource.DataSet.FieldByName('individual_name').Clear;
      end;
      if FieldName = 'father_name' then
      begin
        DataSource.DataSet.FieldByName('father_id').Clear;
        DataSource.DataSet.FieldByName('father_name').Clear;
      end;
      if FieldName = 'mother_name' then
      begin
        DataSource.DataSet.FieldByName('mother_id').Clear;
        DataSource.DataSet.FieldByName('mother_name').Clear;
      end;

      if FieldName = 'nest_name' then
      begin
        DataSource.DataSet.FieldByName('nest_id').Clear;
        DataSource.DataSet.FieldByName('nest_name').Clear;
      end;

      if FieldName = 'egg_name' then
      begin
        DataSource.DataSet.FieldByName('egg_id').Clear;
        DataSource.DataSet.FieldByName('egg_name').Clear;
      end;

      if FieldName = 'band_full_name' then
      begin
        DataSource.DataSet.FieldByName('band_id').Clear;
        DataSource.DataSet.FieldByName('band_full_name').Clear;
      end;
      if FieldName = 'band_name' then
      begin
        DataSource.DataSet.FieldByName('band_id').Clear;
        DataSource.DataSet.FieldByName('band_name').Clear;
      end;
      if FieldName = 'double_band_name' then
      begin
        DataSource.DataSet.FieldByName('double_band_id').Clear;
        DataSource.DataSet.FieldByName('double_band_name').Clear;
      end;
      if FieldName = 'removed_band_name' then
      begin
        DataSource.DataSet.FieldByName('removed_band_id').Clear;
        DataSource.DataSet.FieldByName('removed_band_name').Clear;
      end;
    end;
    Key := #0;
  end;
end;

procedure TfrmCustomGrid.ChildTagClick(aTag, aCountTag: TBCPanel);
begin
  if Working then
    Exit;

  Working := True;
  if IsDarkModeEnabled then
  begin
    pChildTag1.Border.Color := clSolidBGTertiaryDark;
    pChildTag2.Border.Color := clSolidBGTertiaryDark;
    pChildTag3.Border.Color := clSolidBGTertiaryDark;
    pChildTag4.Border.Color := clSolidBGTertiaryDark;
    pChildTag5.Border.Color := clSolidBGTertiaryDark;
  end
  else
  begin
    pChildTag1.Border.Color := $00D1D1D1;
    pChildTag2.Border.Color := $00D1D1D1;
    pChildTag3.Border.Color := $00D1D1D1;
    pChildTag4.Border.Color := $00D1D1D1;
    pChildTag5.Border.Color := $00D1D1D1;
  end;
  pChildTag1.Border.Width := 1;
  pChildTag2.Border.Width := 1;
  pChildTag3.Border.Width := 1;
  pChildTag4.Border.Width := 1;
  pChildTag5.Border.Width := 1;

  if IsDarkModeEnabled then
    aTag.Background.Color := clVioletBG1Dark
  else
    aTag.Background.Color := clVioletBG1Light;
  aCountTag.Color := aTag.Background.Color;
  if (pChild.Visible) and (aTag.Tag = nbChilds.PageIndex) then
  begin
    pChild.Visible := False;
    if IsDarkModeEnabled then
      aTag.Border.Color := clSolidBGTertiaryDark
    else
      aTag.Border.Color := $00D1D1D1;
    aTag.Border.Width := 1;
  end
  else
  begin
    nbChilds.PageIndex := aTag.Tag;
    if not pChild.Visible then
      pChild.Visible := True;
    pChild.Top := pChildsBar.Top + pChildsBar.Height + 1;
    aTag.Border.Color := clVioletFGLight;
    aTag.Border.Width := 2;
  end;
  //splitChild.Visible := pChild.Visible;
  aTag.Background.Color := $00E0C0C0;
  aCountTag.Color := aTag.Background.Color;
  Working := False;
end;

procedure TfrmCustomGrid.ChildTagMouseEnter(aTag, aCountTag: TBCPanel);
begin
  if IsDarkModeEnabled then
    aTag.Background.Color := clVioletBG1Dark
  else
    aTag.Background.Color := $00E0C0C0;
  aCountTag.Color := aTag.Background.Color;
end;

procedure TfrmCustomGrid.ChildTagMouseLeave(aTag, aCountTag: TBCPanel);
begin
  if IsDarkModeEnabled then
    aTag.Background.Color := clCardBGSecondaryDark
  else
    aTag.Background.Color := clCardBGSecondaryLight;
  aCountTag.Color := aTag.Background.Color;
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

  tsTaxonomyClements.Checked := False;
  tsTaxonomyIoc.Checked := False;
  tsTaxonomyCbro.Checked := False;

  rbExtinctAll.Checked := True;

  rbIsSynonymAll.Checked := True;
  rbHasSynonymsAll.Checked := True;
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
  if MousePos.Y < TDBGrid(Sender).DefaultRowHeight then
  begin
    pmColumn.PopUp;
    Handled := True;
  end;
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
    if FieldName = 'taxon_name' then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'taxon_id', 'taxon_name', True);
    if FieldName = 'nidoparasite_name' then
      FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'nidoparasite_id', 'nidoparasite_name', True);

    if FieldName = 'parent_taxon_name' then
      FindBotanicDlg([tfAll], InplaceEditor,
        DataSource.DataSet, 'parent_taxon_id', 'parent_taxon_name');
    if FieldName = 'valid_name' then
      FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'valid_id', 'valid_name');
    if FieldName = 'support_plant_1_name' then
      FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'support_plant_1_id', 'support_plant_1_name');
    if FieldName = 'support_plant_2_name' then
      FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
        DataSource.DataSet, 'support_plant_2_id', 'support_plant_2_name');

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

    if FieldName = 'expedition_name' then
      FindDlg(tbExpeditions, InplaceEditor, DataSource.DataSet, 'expedition_id', 'expedition_name');

    if FieldName = 'survey_name' then
      FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, 'survey_id', 'survey_name');

    if FieldName = 'net_station_name' then
      FindDlg(tbNetStations, InplaceEditor, DataSource.DataSet, 'net_station_id', 'net_station_name');

    if FieldName = 'observer_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_id', 'observer_name');
    if FieldName = 'observer_1_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_1_id', 'observer_1_name');
    if FieldName = 'observer_2_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_2_id', 'observer_2_name');
    if FieldName = 'carrier_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'carrier_id', 'carrier_name');
    if FieldName = 'bander_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'bander_id', 'bander_name');
    if FieldName = 'annotator_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'annotator_id', 'annotator_name');
    if FieldName = 'photographer_1_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'photographer_1_id', 'photographer_1_name');
    if FieldName = 'photographer_2_name' then
      FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'photographer_2_id', 'photographer_2_name');

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

    if FieldName = 'egg_name' then
      FindDlg(tbEggs, InplaceEditor, DataSource.DataSet, 'egg_id', 'egg_name');

    if FieldName = 'band_full_name' then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_full_name');
    if FieldName = 'band_name' then
      FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_name');
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

procedure TfrmCustomGrid.dbgImagesDblClick(Sender: TObject);
begin
  if sbViewImage.Enabled then
    sbViewImageClick(nil);
end;

procedure TfrmCustomGrid.DBGKeyPress(Sender: TObject; var Key: char);
const
  FPress: array of String = ('taxon_name', 'nidoparasite_name', 'parent_taxon_name', 'valid_name',
    'support_plant_1_name', 'support_plant_2_name', 'country_name', 'state_name', 'municipality_name',
    'locality_name', 'parent_site_name', 'institution_name', 'supplier_name', 'expedition_name', 'survey_name',
    'net_station_name', 'observer_name', 'observer_1_name', 'observer_2_name', 'carrier_name', 'bander_name',
    'annotator_name', 'photographer_1_name', 'photographer_2_name', 'project_name', 'individual_name',
    'father_name', 'mother_name', 'nest_name', 'egg_name', 'band_full_name', 'band_name', 'double_band_name',
    'removed_band_name');
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
        if FieldName = 'taxon_name' then
          FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, 'taxon_id', 'taxon_name', True, Key);
        if FieldName = 'nidoparasite_name' then
          FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, 'nidoparasite_id', 'nidoparasite_name', True, Key);

        if FieldName = 'parent_taxon_name' then
          FindBotanicDlg([tfAll], InplaceEditor,
            DataSource.DataSet, 'parent_taxon_id', 'parent_taxon_name', Key);
        if FieldName = 'valid_name' then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, 'valid_id', 'valid_name', Key);
        if FieldName = 'support_plant_1_name' then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, 'support_plant_1_id', 'support_plant_1_name', Key);
        if FieldName = 'support_plant_2_name' then
          FindBotanicDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], InplaceEditor,
            DataSource.DataSet, 'support_plant_2_id', 'support_plant_2_name', Key);

        if FieldName = 'country_name' then
          FindSiteDlg([gfCountries], InplaceEditor, DataSource.DataSet, 'country_id', 'country_name', Key);
        if FieldName = 'state_name' then
          FindSiteDlg([gfStates], InplaceEditor, DataSource.DataSet, 'state_id', 'state_name', Key);
        if FieldName = 'municipality_name' then
          FindSiteDlg([gfCities], InplaceEditor, DataSource.DataSet, 'municipality_id', 'municipality_name', Key);
        if FieldName = 'locality_name' then
          FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, 'locality_id', 'locality_name', Key);
        if FieldName = 'parent_site_name' then
          FindSiteDlg([gfLocalities], InplaceEditor, DataSource.DataSet, 'parent_site_id', 'parent_site_name', Key);

        if FieldName = 'institution_name' then
          FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, 'institution_id', 'institution_name', False, Key);
        if FieldName = 'supplier_name' then
          FindDlg(tbInstitutions, InplaceEditor, DataSource.DataSet, 'supplier_id', 'supplier_name', False, Key);

        if FieldName = 'expedition_name' then
          FindDlg(tbExpeditions, InplaceEditor, DataSource.DataSet, 'expedition_id', 'expedition_name', False, Key);

        if FieldName = 'survey_name' then
          FindDlg(tbSurveys, InplaceEditor, DataSource.DataSet, 'survey_id', 'survey_name', False, Key);

        if FieldName = 'net_station_name' then
          FindDlg(tbNetStations, InplaceEditor, DataSource.DataSet, 'net_station_id', 'net_station_name', False, Key);

        if FieldName = 'observer_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_id', 'observer_name', False, Key);
        if FieldName = 'observer_1_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_1_id', 'observer_1_name', False, Key);
        if FieldName = 'observer_2_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'observer_2_id', 'observer_2_name', False, Key);
        if FieldName = 'carrier_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'carrier_id', 'carrier_name', False, Key);
        if FieldName = 'bander_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'bander_id', 'bander_name', False, Key);
        if FieldName = 'annotator_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'annotator_id', 'annotator_name', False, Key);
        if FieldName = 'photographer_1_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'photographer_1_id', 'photographer_1_name', False, Key);
        if FieldName = 'photographer_2_name' then
          FindDlg(tbPeople, InplaceEditor, DataSource.DataSet, 'photographer_2_id', 'photographer_2_name', False, Key);

        if FieldName = 'project_name' then
          FindDlg(tbProjects, InplaceEditor, DataSource.DataSet, 'project_id', 'project_name', False, Key);

        if FieldName = 'individual_name' then
          FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'individual_id', 'individual_name', False, Key);
        if FieldName = 'father_name' then
          FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'father_id', 'father_name', False, Key);
        if FieldName = 'mother_name' then
          FindDlg(tbIndividuals, InplaceEditor, DataSource.DataSet, 'mother_id', 'mother_name', False, Key);

        if FieldName = 'nest_name' then
          FindDlg(tbNests, InplaceEditor, DataSource.DataSet, 'nest_id', 'nest_name', False, Key);

        if FieldName = 'egg_name' then
          FindDlg(tbEggs, InplaceEditor, DataSource.DataSet, 'egg_id', 'egg_name', False, Key);

        if FieldName = 'band_full_name' then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_full_name', False, Key);
        if FieldName = 'band_name' then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'band_id', 'band_name', False, Key);
        if FieldName = 'double_band_name' then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'double_band_id', 'double_band_name', False, Key);
        if FieldName = 'removed_band_name' then
          FindDlg(tbBands, InplaceEditor, DataSource.DataSet, 'removed_band_id', 'removed_band_name', False, Key);
      end;
      Key := #0;
    end;
    { CLEAR FIELD VALUE = Backspace }
    if (Key = #8) then
    begin
      with (Sender as TDBGrid), SelectedColumn do
      begin
        if FieldName = 'taxon_name' then
        begin
          DataSource.DataSet.FieldByName('taxon_id').Clear;
          DataSource.DataSet.FieldByName('taxon_name').Clear;
        end;
        if FieldName = 'nidoparasite_name' then
        begin
          DataSource.DataSet.FieldByName('nidoparasite_id').Clear;
          DataSource.DataSet.FieldByName('nidoparasite_name').Clear;
        end;

        if FieldName = 'parent_taxon_name' then
        begin
          DataSource.DataSet.FieldByName('parent_taxon_id').Clear;
          DataSource.DataSet.FieldByName('parent_taxon_name').Clear;
        end;
        if FieldName = 'valid_name' then
        begin
          DataSource.DataSet.FieldByName('valid_id').Clear;
          DataSource.DataSet.FieldByName('valid_name').Clear;
        end;
        if FieldName = 'support_plant_1_name' then
        begin
          DataSource.DataSet.FieldByName('support_plant_1_id').Clear;
          DataSource.DataSet.FieldByName('support_plant_1_name').Clear;
        end;
        if FieldName = 'support_plant_2_name' then
        begin
          DataSource.DataSet.FieldByName('support_plant_2_id').Clear;
          DataSource.DataSet.FieldByName('support_plant_2_name').Clear;
        end;

        if FieldName = 'country_name' then
        begin
          DataSource.DataSet.FieldByName('country_id').Clear;
          DataSource.DataSet.FieldByName('country_name').Clear;
        end;
        if FieldName = 'state_name' then
        begin
          DataSource.DataSet.FieldByName('state_id').Clear;
          DataSource.DataSet.FieldByName('state_name').Clear;
        end;
        if FieldName = 'municipality_name' then
        begin
          DataSource.DataSet.FieldByName('municipality_id').Clear;
          DataSource.DataSet.FieldByName('municipality_name').Clear;
        end;
        if FieldName = 'locality_name' then
        begin
          DataSource.DataSet.FieldByName('locality_id').Clear;
          DataSource.DataSet.FieldByName('locality_name').Clear;
        end;
        if FieldName = 'parent_site_name' then
        begin
          DataSource.DataSet.FieldByName('parent_site_id').Clear;
          DataSource.DataSet.FieldByName('parent_site_name').Clear;
        end;

        if FieldName = 'institution_name' then
        begin
          DataSource.DataSet.FieldByName('institution_id').Clear;
          DataSource.DataSet.FieldByName('institution_name').Clear;
        end;
        if FieldName = 'supplier_name' then
        begin
          DataSource.DataSet.FieldByName('supplier_id').Clear;
          DataSource.DataSet.FieldByName('supplier_name').Clear;
        end;

        if FieldName = 'expedition_name' then
        begin
          DataSource.DataSet.FieldByName('expedition_id').Clear;
          DataSource.DataSet.FieldByName('expedition_name').Clear;
        end;

        if FieldName = 'survey_name' then
        begin
          DataSource.DataSet.FieldByName('survey_id').Clear;
          DataSource.DataSet.FieldByName('survey_name').Clear;
        end;

        if FieldName = 'net_station_name' then
        begin
          DataSource.DataSet.FieldByName('net_station_id').Clear;
          DataSource.DataSet.FieldByName('net_station_name').Clear;
        end;

        if FieldName = 'observer_name' then
        begin
          DataSource.DataSet.FieldByName('observer_id').Clear;
          DataSource.DataSet.FieldByName('observer_name').Clear;
        end;
        if FieldName = 'observer_1_name' then
        begin
          DataSource.DataSet.FieldByName('observer_1_id').Clear;
          DataSource.DataSet.FieldByName('observer_1_name').Clear;
        end;
        if FieldName = 'observer_2_name' then
        begin
          DataSource.DataSet.FieldByName('observer_2_id').Clear;
          DataSource.DataSet.FieldByName('observer_2_name').Clear;
        end;
        if FieldName = 'carrier_name' then
        begin
          DataSource.DataSet.FieldByName('carrier_id').Clear;
          DataSource.DataSet.FieldByName('carrier_name').Clear;
        end;
        if FieldName = 'bander_name' then
        begin
          DataSource.DataSet.FieldByName('bander_id').Clear;
          DataSource.DataSet.FieldByName('bander_name').Clear;
        end;
        if FieldName = 'annotator_name' then
        begin
          DataSource.DataSet.FieldByName('annotator_id').Clear;
          DataSource.DataSet.FieldByName('annotator_name').Clear;
        end;
        if FieldName = 'photographer_1_name' then
        begin
          DataSource.DataSet.FieldByName('photographer_1_id').Clear;
          DataSource.DataSet.FieldByName('photographer_1_name').Clear;
        end;
        if FieldName = 'photographer_2_name' then
        begin
          DataSource.DataSet.FieldByName('photographer_2_id').Clear;
          DataSource.DataSet.FieldByName('photographer_2_name').Clear;
        end;

        if FieldName = 'project_name' then
        begin
          DataSource.DataSet.FieldByName('project_id').Clear;
          DataSource.DataSet.FieldByName('project_name').Clear;
        end;

        if FieldName = 'individual_name' then
        begin
          DataSource.DataSet.FieldByName('individual_id').Clear;
          DataSource.DataSet.FieldByName('individual_name').Clear;
        end;
        if FieldName = 'father_name' then
        begin
          DataSource.DataSet.FieldByName('father_id').Clear;
          DataSource.DataSet.FieldByName('father_name').Clear;
        end;
        if FieldName = 'mother_name' then
        begin
          DataSource.DataSet.FieldByName('mother_id').Clear;
          DataSource.DataSet.FieldByName('mother_name').Clear;
        end;

        if FieldName = 'nest_name' then
        begin
          DataSource.DataSet.FieldByName('nest_id').Clear;
          DataSource.DataSet.FieldByName('nest_name').Clear;
        end;

        if FieldName = 'egg_name' then
        begin
          DataSource.DataSet.FieldByName('egg_id').Clear;
          DataSource.DataSet.FieldByName('egg_name').Clear;
        end;

        if FieldName = 'band_full_name' then
        begin
          DataSource.DataSet.FieldByName('band_id').Clear;
          DataSource.DataSet.FieldByName('band_full_name').Clear;
        end;
        if FieldName = 'band_name' then
        begin
          DataSource.DataSet.FieldByName('band_id').Clear;
          DataSource.DataSet.FieldByName('band_name').Clear;
        end;
        if FieldName = 'double_band_name' then
        begin
          DataSource.DataSet.FieldByName('double_band_id').Clear;
          DataSource.DataSet.FieldByName('double_band_name').Clear;
        end;
        if FieldName = 'removed_band_name' then
        begin
          DataSource.DataSet.FieldByName('removed_band_id').Clear;
          DataSource.DataSet.FieldByName('removed_band_name').Clear;
        end;
      end;
      Key := #0;
    end;
  end;
end;

procedure TfrmCustomGrid.DBGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Direction: TSortDirection;
  ACol, ARow: Longint;
  Column: TColumn;
  Grid: TDBGrid;
  idx: Integer;
begin
  Grid := TDBGrid(Sender);
  idx := -1;

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

  if mapGeo.Active then
    RefreshMap;

  UpdateChildBar;
end;

procedure TfrmCustomGrid.dsLinkStateChange(Sender: TObject);
begin
  if Assigned(dsLink.DataSet) then
    UpdateButtons(dsLink.DataSet);

  pEmptyQuery.Visible := (not Working) and (dsLink.DataSet.RecordCount = 0);

  UpdateChildBar;
end;

procedure TfrmCustomGrid.dsRecycleStateChange(Sender: TObject);
begin
  case dsRecycle.State of
    dsInactive:
    begin
      sbRestoreRecord.Enabled := False;
      sbDelPermanently.Enabled := False;
    end;
    dsBrowse:
    begin
      sbRestoreRecord.Enabled := (qRecycle.RecordCount > 0);
      sbDelPermanently.Enabled := (qRecycle.RecordCount > 0);
    end;
    dsEdit, dsInsert:
    begin
      sbRestoreRecord.Enabled := False;
      sbDelPermanently.Enabled := False;
    end;
  end;
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

  SaveColumns;

  if qRecycle.Active then
    qRecycle.Close;

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
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FSearch := TCustomSearch.Create(FTableType);
  mapGeo.CachePath := IncludeTrailingPathDelimiter(ConcatPaths([AppDataDir, 'map-cache']));

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
    LoadColumns;
    AddGridColumns(FTableType, DBG);
    if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
      (dsLink.DataSet as TSQLQuery).ReadOnly := True;
    if not (dsLink.DataSet.Active) then
      dsLink.DataSet.Open;
    UpdateGridTitles(DBG, FSearch);
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
  //Application.ProcessMessages;

  { Load side panels }
  {$IFDEF DEBUG}
  Usage.AddPart('load side panels');
  {$ENDIF}
  //navTabs.OptScalePercents := Round(navTabs.OptScalePercents * navTabs.ScaleFactor);
  LoadRecordColumns;
  LoadRecordRow;
  UpdateFilterPanels;
  UpdateButtons(dsLink.DataSet);
  //UpdateGridTitles(DBG, FSearch);
  UpdateChildCount;
  if DBG.CanSetFocus then
    DBG.SetFocus;
  GetColumns;
  SetImages;
  SetAudios;
  SetRecycle;
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

  if rbReportedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('band_reported', 'Band reported', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbReportedNo.Checked then
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
        FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('rank_id', 'Rank', sdtInteger,
          crEqual, False, IntToStr(GetKey('taxon_ranks', 'rank_id', 'rank_name', clbTaxonRanksFilter.Items[i]))));
  end;

  if rbIsSynonymYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crNotEqual, False, '0'));
  end;
  if rbIsSynonymNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
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

  if rbNeedsReviewYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('needs_review', 'Needs review', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbNeedsReviewNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('Needs review', 'Needs review', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbEscapedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('escaped', 'Escaped', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbEscapedNo.Checked then
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

procedure TfrmCustomGrid.GetColumns;
var
  i: Integer;
begin
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

  if (rbMarkedYes.Checked) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (rbMarkedNo.Checked) then
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

  if rbWithColorBandsYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('right_leg_below', 'Right tarsus', sdtText,
      crNotEqual, False, ''));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('left_leg_below', 'Left tarsus', sdtText,
      crNotEqual, False, ''));
  end;
  if rbWithColorBandsNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('right_leg_below', 'Right tarsus', sdtText,
      crEqual, False, ''));
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('left_leg_below', 'Left tarsus', sdtText,
      crEqual, False, ''));
  end;

  if rbWithRecapturesYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('captures_tally', 'Captures', sdtInteger,
      crMoreThan, True, '2'));
  end;
  if rbWithRecapturesNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('captures_tally', 'Captures', sdtInteger,
      crLessThan, True, '1'));
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
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbExtinctNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbIsSynonymYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crNotEqual, False, '0'));
  end;
  if rbIsSynonymNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
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
  if not CanToggle then
    Exit;

  if aCol = 1 then
    dsLink.DataSet.Fields[aRow - 1].Visible := aState = cbChecked;

  AddGridColumns(FTableType, DBG);
end;

procedure TfrmCustomGrid.iHeadersGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer;
  var AResultWidth: Integer);
begin
  AResultWidth := AImageWidth * APPI div 96;
end;

procedure TfrmCustomGrid.LoadColumns;
var
  ColsFile: String;
  i, f: Integer;
begin
  ColsFile := ConcatPaths([AppDataDir, TableNames[FTableType] + '_columns.xml']);
  if not FileExists(ColsFile) then
    Exit;

  gridColumns.LoadFromFile(ColsFile);
  for i := 1 to gridColumns.RowCount - 1 do
  begin
    for f := 0 to dsLink.DataSet.Fields.Count - 1 do
      if dsLink.DataSet.Fields[f].DisplayName = gridColumns.Cells[2, i] then
        Break;

    dsLink.DataSet.Fields[f].Visible := gridColumns.Cells[1, i] = '1';
    if f <> i - 1 then
      dsLink.DataSet.Fields[f].Index := i - 1;
  end;
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
  if Working then
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
  sbAddNetsBatch.Visible := False;

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
        1:
        begin
          FChildTable := tbNetsEffort;
          sbAddNetsBatch.Visible := True;
        end;
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

  UpdateChildStatus;
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

procedure TfrmCustomGrid.pmaRefreshAudiosClick(Sender: TObject);
begin
  if Working then
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

  GetColumns;
  AddGridColumns(FTableType, DBG);
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

procedure TfrmCustomGrid.pmiRefreshImagesClick(Sender: TObject);
begin
  if Working then
    Exit;

  qImages.Refresh;
end;

procedure TfrmCustomGrid.pmmInvertMarkedClick(Sender: TObject);
var
  BM: TBookmark;
begin
  CanToggle := False;

  with dsLink.DataSet do
  try
    BM := Bookmark;
    DisableControls;
    First;
    repeat
      Edit;
      FieldByName('marked_status').AsBoolean := not FieldByName('marked_status').AsBoolean;
      Post;
      Next;
    until Eof;
  finally
    EnableControls;
    Bookmark := BM;
    CanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmMarkAllClick(Sender: TObject);
var
  BM: TBookmark;
begin
  CanToggle := False;

  with dsLink.DataSet do
  try
    BM := Bookmark;
    DisableControls;
    First;
    repeat
      Edit;
      FieldByName('marked_status').AsBoolean := True;
      Post;
      Next;
    until Eof;
  finally
    EnableControls;
    Bookmark := BM;
    CanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmMarkAllColumnsClick(Sender: TObject);
var
  i: Integer;
begin
  CanToggle := False;

  try
    for i := 0 to dsLink.DataSet.Fields.Count - 1 do
     dsLink.DataSet.Fields[i].Visible := True;

    GetColumns;
    AddGridColumns(FTableType, DBG);
  finally
    CanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmUnmarkAllClick(Sender: TObject);
var
  BM: TBookmark;
begin
  CanToggle := False;

  with dsLink.DataSet do
  try
    BM := Bookmark;
    DisableControls;
    First;
    repeat
      Edit;
      FieldByName('marked_status').AsBoolean := False;
      Post;
      Next;
    until Eof;
  finally
    EnableControls;
    Bookmark := BM;
    CanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmmUnmarkAllColumnsClick(Sender: TObject);
var
  i: Integer;
begin
  CanToggle := False;

  try
    for i := 0 to dsLink.DataSet.Fields.Count - 1 do
     dsLink.DataSet.Fields[i].Visible := False;

    GetColumns;
    AddGridColumns(FTableType, DBG);
  finally
    CanToggle := True;
  end;
end;

procedure TfrmCustomGrid.pmPrintBandsByCarrierClick(Sender: TObject);
begin
  PrintPreview('rep_bands_by_carrier.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintBandsByStatusClick(Sender: TObject);
begin
  PrintPreview('rep_bands_by_status.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintBandsClick(Sender: TObject);
begin
  PrintPreview('rep_bands.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintBandsWithHistoryClick(Sender: TObject);
begin
  PrintPreview('rep_bands_history.lrf', dsLink, DMG.dsBandHistory);
end;

procedure TfrmCustomGrid.pmPrintGridClick(Sender: TObject);
begin
  PrintGrid.PreviewReport;
end;

procedure TfrmCustomGrid.pmPrintInstitutionsClick(Sender: TObject);
begin
  PrintPreview('rep_institutions.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintMethodsClick(Sender: TObject);
begin
  PrintPreview('rep_methods.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintPermitsClick(Sender: TObject);
begin
  PrintPreview('rep_permits.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintProjectsClick(Sender: TObject);
begin
  PrintPreview('rep_projects.lrf', dsLink);
end;

procedure TfrmCustomGrid.pmPrintResearchersClick(Sender: TObject);
begin
  PrintPreview('rep_people.lrf', dsLink);
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

procedure TfrmCustomGrid.pmvAddVerificationClick(Sender: TObject);
var
  DS: TDataSet;
begin
  if pmVerifications.PopupComponent = sbRecordVerifications then
  begin
    DS := dsLink.DataSet;
    AddVerification(FTableType, tbNone, DS.FieldByName(GetPrimaryKey(DS)).AsInteger)
  end
  else
  if pmVerifications.PopupComponent = sbChildVerifications then
  begin
    case nbChilds.PageIndex of
      0: DS := dsLink1.DataSet;
      1: DS := dsLink2.DataSet;
      2: DS := dsLink3.DataSet;
      3: DS := dsLink4.DataSet;
      4: DS := dsLink5.DataSet;
    end;
    AddVerification(FTableType, FChildTable, DS.FieldByName(GetPrimaryKey(DS)).AsInteger);
  end;
end;

procedure TfrmCustomGrid.pmvViewVerificationsClick(Sender: TObject);
var
  DS: TDataSet;
begin
  if pmVerifications.PopupComponent = sbRecordVerifications then
  begin
    DS := dsLink.DataSet;
    ShowVerifications(FTableType, tbNone, DS.FieldByName(GetPrimaryKey(DS)).AsInteger)
  end
  else
  if pmVerifications.PopupComponent = sbChildVerifications then
  begin
    case nbChilds.PageIndex of
      0: DS := dsLink1.DataSet;
      1: DS := dsLink2.DataSet;
      2: DS := dsLink3.DataSet;
      3: DS := dsLink4.DataSet;
      4: DS := dsLink5.DataSet;
    end;
    ShowVerifications(FTableType, FChildTable, DS.FieldByName(GetPrimaryKey(DS)).AsInteger);
  end;
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
  z: Extended;
  aTaxon: Integer;
begin
  z := 0;
  aTaxon := 0;

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
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
    end
    else
    if (TDBGrid(Sender).Columns.ColumnByFieldname('capture_type').Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
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
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end;

  { Paint the cell background red for invalid values }
  if not XSettings.UseConditionalFormatting then
    Exit;

  if (Column.FieldName = 'cloacal_protuberance') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, CloacalProtuberanceValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = 'brood_patch') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BroodPatchValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = 'fat') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FatValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = 'body_molt') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BodyMoltValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = 'flight_feather_molt') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FlightMoltValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = 'flight_feather_wear') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FeatherWearValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = 'skull_ossification') then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, SkullValues)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end;

  { Paint the cell background yellow for outliers }
  if not XSettings.ShowOutliersOnGrid then
    Exit;

  if (Column.FieldName = 'right_wing_chord') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'first_secondary_chord') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'tail_length') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'tarsus_length') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'tarsus_diameter') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'weight') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'exposed_culmen') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'bill_width') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'bill_height') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'nostril_bill_tip') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'skull_length') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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
  if (Column.FieldName = 'kipps_index') then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                    TDBGrid(Sender).Columns.ColumnByFieldname('taxon_name').Field.AsString);
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

procedure TfrmCustomGrid.qAudiosBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  //if not DataSet.FieldByName('locality_id').IsNull then
  //  GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
end;

procedure TfrmCustomGrid.qImagesBeforePost(DataSet: TDataSet);
begin
  SetRecordDateUser(DataSet);

  { Load hierarchies }
  if not DataSet.FieldByName('taxon_id').IsNull then
    GetTaxonHierarchy(DataSet, DataSet.FieldByName('taxon_id').AsInteger);

  if not DataSet.FieldByName('locality_id').IsNull then
    GetSiteHierarchy(DataSet, DataSet.FieldByName('locality_id').AsInteger);
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
    tbNetStations,
    tbCaptures,
    tbNests,
    tbSightings,
    tbSpecimens:
    begin
      rp.Lon := dsLink.DataSet.FieldByName('longitude').AsFloat;
      rp.Lat := dsLink.DataSet.FieldByName('latitude').AsFloat;
    end;
    tbSurveys:
    begin
      rp.Lon := dsLink.DataSet.FieldByName('start_longitude').AsFloat;
      rp.Lat := dsLink.DataSet.FieldByName('start_latitude').AsFloat;
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
        rp.Lon := FieldByName('longitude').AsFloat;
        rp.Lat := FieldByName('latitude').AsFloat;
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

procedure TfrmCustomGrid.SaveColumns;
var
  ColsFile: String;
begin
  ColsFile := ConcatPaths([AppDataDir, TableNames[FTableType] + '_columns.xml']);
  gridColumns.SaveToFile(ColsFile);
end;

procedure TfrmCustomGrid.sbAddChildClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmAddChild.Popup(X, Y);
end;

procedure TfrmCustomGrid.sbAddImageClick(Sender: TObject);
var
  i: Integer;
begin
  DMM.OpenImgs.InitialDir := XSettings.ImagesFolder;
  if DMM.OpenImgs.Execute then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsImportImagesTitle;
      dlgProgress.Text := rsProgressPreparing;
      dlgProgress.Max := DMM.OpenImgs.Files.Count;
      Parar := False;
      Application.ProcessMessages;
      if not DMM.sqlTrans.Active then
        DMM.sqlCon.StartTransaction;
      try
        for i := 0 to DMM.OpenImgs.Files.Count - 1 do
        begin
          dlgProgress.Text := Format(rsProgressImportImages, [i + 1, DMM.OpenImgs.Files.Count]);

          AddImage(qImages, tbImages, 'image_filename', 'image_thumbnail', DMM.OpenImgs.Files[i]);

          dlgProgress.Position := i + 1;
          Application.ProcessMessages;
          if Parar then
            Break;
        end;
        if Parar then
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
      Parar := False;
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
    ShowModal;
  finally
    FreeAndNil(batchNetEffort);
  end;
end;

procedure TfrmCustomGrid.sbCancelRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    if (dsLink.DataSet.Modified) and (XSettings.ConfirmCancel) then
    begin
      if not MsgDlg(rsDiscardChangesTitle, rsCancelEditingPrompt, mtConfirmation) then
        Exit;
    end;

    dsLink.DataSet.Cancel;
  finally
    Working := False;
  end;
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
  ShowHistory(FTableType, FChildTable, aDataSet.FieldByName(aKeyField).AsInteger);
end;

procedure TfrmCustomGrid.sbChildVerificationsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
  begin
    pmVerifications.PopupComponent := sbChildVerifications;
    pmVerifications.Popup(X, Y);
  end;
end;

procedure TfrmCustomGrid.sbClearFiltersClick(Sender: TObject);
begin
  ClearSearch;
end;

procedure TfrmCustomGrid.sbColumnHideClick(Sender: TObject);
begin
  dsLink.DataSet.Fields[gridColumns.Row - 1].Visible := False;

  GetColumns;
  AddGridColumns(FTableType, DBG);
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
  try
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
  finally
    UpdateChildBar;
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbDelImageClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    DeleteRecord(tbImages, qImages);
    UpdateImageButtons(qImages);
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbDelPermanentlyClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  if Working then
    Exit;

  Working := True;
  if MsgDlg(rsRecycleDeleteTitle, rsRecycleDeletePermanentlyPrompt, mtConfirmation) then
  begin
    Qry := TSQLQuery.Create(nil);
    with Qry do
    try
      SQLConnection := DMM.sqlCon;
      MacroCheck := True;
      SQL.Add('DELETE FROM %ftable WHERE (active_status = 0)');
      MacroByName('FTABLE').AsString := TableNames[FTableType];
      ExecSQL;
      qRecycle.Refresh;
    finally
      FreeAndNil(Qry);
    end;
  end;
  Working := False;
end;

procedure TfrmCustomGrid.sbDelRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    DeleteRecord(FTableType, dsLink.DataSet);
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbEditChildClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
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
  finally
    UpdateChildBar;
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbEditRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
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
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbFirstChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.First;
  finally
    UpdateChildButtons(aDataSet);
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbFirstRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    dsLink.DataSet.First;
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbImageInfoClick(Sender: TObject);
begin
  EditImageInfo(qImages, dsLink.DataSet, FTableType);
end;

procedure TfrmCustomGrid.sbInsertRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
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
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbLastChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.Last;
  finally
    UpdateChildButtons(aDataSet);
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbLastRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    dsLink.DataSet.Last;
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
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

  GetColumns;
  gridColumns.Row := gridColumns.Row + 1;

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

  GetColumns;
  gridColumns.Row := gridColumns.Row - 1;

  AddGridColumns(FTableType, DBG);
end;

procedure TfrmCustomGrid.sbNextChildClick(Sender: TObject);
var
  aDataSet: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.Next;
  finally
    UpdateChildButtons(aDataSet);
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbNextRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    dsLink.DataSet.Next;
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
  end;
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
  if Working then
    Exit;

  Working := True;
  try
    aDataSet := GetChildDataSet;

    if Assigned(aDataSet) then
      aDataSet.Prior;
  finally
    UpdateChildButtons(aDataSet);
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbPriorRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    dsLink.DataSet.Prior;
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbRecordHistoryClick(Sender: TObject);
var
  aKeyField: String;
begin
  aKeyField := GetPrimaryKey(TableNames[FTableType]);
  ShowHistory(FTableType, tbNone, dsLink.DataSet.FieldByName(aKeyField).AsInteger);
end;

procedure TfrmCustomGrid.sbRecordVerificationsClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
  begin
    pmVerifications.PopupComponent := sbRecordVerifications;
    pmVerifications.Popup(X, Y);
  end;
end;

procedure TfrmCustomGrid.sbRefreshChildClick(Sender: TObject);
var
  DS: TDataSet;
begin
  if Working then
    Exit;

  Working := True;
  try
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
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbRefreshRecordsClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    if not dsLink.DataSet.Active then
      dsLink.DataSet.Open;
    dsLink.DataSet.Refresh;
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
  end;
end;

procedure TfrmCustomGrid.sbRestoreRecordClick(Sender: TObject);
begin
  if Working then
    Exit;

  Working := True;
  try
    RestoreRecord(FTableType, dsLink.DataSet);
    UpdateButtons(dsLink.DataSet);
  finally
    Working := False;
  end;
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
  try
    dsLink.DataSet.Post;
  finally
    Working := False;
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
  end;
end;

procedure TfrmCustomGrid.sbShareMapPointsClick(Sender: TObject);
var
  aFilename: String;
  Pts: TMapPointList;
begin
  DMM.SaveKmlDlg.InitialDir := XSettings.LastPathUsed;
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

procedure TfrmCustomGrid.sbShowRecordClick(Sender: TObject);
begin
  pSide.Visible := (Sender as TSpeedButton).Down;
  SplitRight.Visible := pSide.Visible;

  if (Sender as TSpeedButton).Down then
    cpSide.PageIndex := (Sender as TSpeedButton).Tag;

  frmMain.UpdateMenu(frmMain.PGW.ActivePageComponent);
end;

procedure TfrmCustomGrid.sbViewImageClick(Sender: TObject);
begin
  ViewImage(qImages);
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
  DBG.BeginUpdate;

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
  DBG.EndUpdate;

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
      V1 := ExtractDelimited(1, aValue, ['-', #$2012]);
      V2 := ExtractDelimited(2, aValue, ['-', #$2012]);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_number', 'Band number', sdtInteger, Crit,
        False, V1, V2));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Full name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('carrier_name', 'Carrier', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('supplier_name', 'Supplier', sdtText, Crit,
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_id', 'Capture (ID)', sdtInteger, crEqual,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_date', 'Capture date', sdtYear, crEqual,
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
    if ExecRegExpr('^\d{4}[-‒]{1}\d{4}$', aValue) then
    begin
      Crit := crBetween;
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      { split strings: unicode characters #$002D e #$2012 }
      V1 := ExtractDelimited(1, aValue, ['-', #$2012]);
      V2 := ExtractDelimited(2, aValue, ['-', #$2012]);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_date', 'Capture date', sdtYear, Crit,
        False, V1, V2));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('capture_date', 'Capture date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create('net_station_name', 'Misnet station', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('bander_name', 'Bander', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('annotator_name', 'Annotator', sdtText, Crit,
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('measure_date', 'Measured date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('start_date', 'Start date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('end_date', 'End date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('banding_date', 'Banding date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('band_change_date', 'Band change date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('found_date', 'Date found', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('last_date', 'Last date seen', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create('observer_name', 'Observer', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('support_plant_1_name', 'Support plant 1', sdtText, Crit,
        True, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('support_plant_2_name', 'Support plant 2', sdtText, Crit,
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('revision_date', 'Revision date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create('death_date', 'Death date', sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('birth_date', 'Birth date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('death_date', 'Death date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('dispatch_date', 'Dispatch date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('expire_date', 'Expire date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('start_date', 'Start date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('end_date', 'End date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('sighting_date', 'Sighting date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('collection_date', 'Collection date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('start_date', 'Start date', sdtMonthYear, crEqual,
        False, y + '-' + m));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('end_date', 'End date', sdtMonthYear, crEqual,
        False, y + '-' + m));
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

procedure TfrmCustomGrid.SetAudios;
begin
  with qImages, SQL do
  begin
    case FTableType of
      tbNone: ;
      //tbUsers: ;
      //tbRecordHistory: ;
      //tbRecordVerifications: ;
      //tbGazetteer: ;
      //tbNetStations: ;
      //tbPermanentNets: ;
      //tbInstitutions: ;
      //tbPeople: ;
      //tbProjects: ;
      //tbProjectTeams: ;
      //tbPermits: ;
      //tbTaxonRanks: ;
      //tbZooTaxa: ;
      //tbBotanicTaxa: ;
      //tbBands: ;
      //tbBandHistory: ;
      tbIndividuals:    Add('WHERE (snd.active_status = 1) AND (snd.individual_id = :individual_id)');
      //tbCaptures: ;
      //tbMolts: ;
      //tbNests: ;
      //tbNestOwners: ;
      //tbNestRevisions: ;
      //tbEggs: ;
      //tbMethods: ;
      //tbExpeditions: ;
      //tbSurveys: ;
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      //tbWeatherLogs: ;
      tbSightings:      Add('WHERE (snd.active_status = 1) AND (snd.sighting_id = :sighting_id)');
      tbSpecimens:      Add('WHERE (snd.active_status = 1) AND (snd.specimen_id = :specimen_id)');
      //tbSamplePreps: ;
      //tbSpecimenCollectors: ;
      //tbImages: ;
      //tbAudioLibrary: ;
    end;
  end;

  if FTableType in [tbIndividuals, tbSightings, tbSpecimens] then
  begin
    qAudios.SQL.Add('ORDER BY snd.recording_date, snd.recording_time ASC');
    qAudios.DataSource := dsLink;
    {$IFDEF DEBUG}
    LogSQL(qAudios.SQL);
    {$ENDIF}
    qAudios.Open;
  end;
end;

procedure TfrmCustomGrid.SetColumnsBands(var aGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    ColumnByFieldname('band_id').ReadOnly := True;
    if DataSource.DataSet.FieldByName('individual_id').Visible then
      ColumnByFieldname('individual_id').ReadOnly := True;
    if DataSource.DataSet.FieldByName('individual_name').Visible then
      ColumnByFieldname('individual_name').ReadOnly := True;

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
    if DataSource.DataSet.FieldByName('net_station_name').Visible then
      ColumnByFieldname('net_station_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('longitude').Visible then
      ColumnByFieldname('longitude').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('latitude').Visible then
      ColumnByFieldname('latitude').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('bander_name').Visible then
      ColumnByFieldname('bander_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('annotator_name').Visible then
      ColumnByFieldname('annotator_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('band_name').Visible then
      ColumnByFieldname('band_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('removed_band_name').Visible then
      ColumnByFieldname('removed_band_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('molt_limits').Visible then
      ColumnByFieldName('molt_limits').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('cycle_code').Visible then
      ColumnByFieldName('cycle_code').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('how_aged').Visible then
      ColumnByFieldName('how_aged').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('how_sexed').Visible then
      ColumnByFieldName('how_sexed').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('photographer_1_name').Visible then
      ColumnByFieldname('photographer_1_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('photographer_2_name').Visible then
      ColumnByFieldname('photographer_2_name').ButtonStyle := cbsEllipsis;

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
    if DataSource.DataSet.FieldByName('project_name').Visible then
      ColumnByFieldname('project_name').ButtonStyle := cbsEllipsis;
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
    if DataSource.DataSet.FieldByName('band_name').Visible then
      ColumnByFieldName('band_name').ButtonStyle := cbsEllipsis;
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
    if DataSource.DataSet.FieldByName('observer_1_name').Visible then
      ColumnByFieldName('observer_1_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('observer_2_name').Visible then
      ColumnByFieldName('observer_2_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('nidoparasite_name').Visible then
      ColumnByFieldName('nidoparasite_name').ButtonStyle := cbsEllipsis;
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
    if DataSource.DataSet.FieldByName('observer_name').Visible then
      ColumnByFieldName('observer_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('support_plant_1_name').Visible then
      ColumnByFieldName('support_plant_1_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('support_plant_2_name').Visible then
      ColumnByFieldName('support_plant_2_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('project_name').Visible then
      ColumnByFieldName('project_name').ButtonStyle := cbsEllipsis;

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

    with ColumnByFieldName('sample_type').PickList do
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

    if DataSource.DataSet.FieldByName('taxon_name').Visible then
      ColumnByFieldName('taxon_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('individual_name').Visible then
      ColumnByFieldName('individual_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('nest_name').Visible then
      ColumnByFieldName('nest_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('egg_name').Visible then
      ColumnByFieldName('egg_name').ButtonStyle := cbsEllipsis;
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
    ColumnByFieldName('method_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('locality_name').Visible then
      ColumnByFieldname('locality_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('country_name').Visible then
      ColumnByFieldname('country_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('state_name').Visible then
      ColumnByFieldname('state_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('municipality_name').Visible then
      ColumnByFieldname('municipality_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('expedition_name').Visible then
      ColumnByFieldname('expedition_name').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('net_station_name').Visible then
      ColumnByFieldname('net_station_name').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('start_longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('start_latitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('end_longitude').ButtonStyle := cbsEllipsis;
    ColumnByFieldname('end_latitude').ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName('project_name').Visible then
      ColumnByFieldname('project_name').ButtonStyle := cbsEllipsis;
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
  dsLink.DataSet := FSearch.DataSet;

  SplitChild.Visible := pChild.Visible;
  Search(EmptyStr);
end;

procedure TfrmCustomGrid.SetGridBands;
begin
  Caption := rsTitleBands;
  FSearch.DataSet := DMG.qBands;
  AddSortedField('band_size', sdAscending);
  AddSortedField('band_number', sdAscending);

  pmPrintBands.Visible := True;
  pmPrintBandsByCarrier.Visible := True;
  pmPrintBandsWithHistory.Visible := True;
  pmPrintBandsByStatus.Visible := True;
  pmPrintBandsBalance.Visible := True;

  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
end;

procedure TfrmCustomGrid.SetGridBotanicTaxa;
begin
  Caption := rsTitleBotanicTaxa;
  FSearch.DataSet := DMG.qBotany;
  AddSortedField('taxon_name', sdAscending);

  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintBotanicTaxa.Visible := True;
  pmPrintBotanicTaxaHierarchical.Visible := True;
  pmPrintBotanicTaxaRecorded.Visible := True;
end;

procedure TfrmCustomGrid.SetGridCaptures;
begin
  Caption := rsTitleCaptures;
  FSearch.DataSet := DMG.qCaptures;
  AddSortedField('capture_date', sdDescending);

  sbRecordVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;
  sbShowImages.Visible := True;
  //sbShowDocs.Visible := True;

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
  FSearch.DataSet := DMG.qEggs;
  AddSortedField('full_name', sdAscending);

  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;
  sbShowImages.Visible := True;
  //sbShowDocs.Visible := True;

  pmPrintEggs.Visible := True;
  pmPrintEggsByNest.Visible := True;
  pmPrintEggsByLocality.Visible := True;
  pmPrintEggsByTaxon.Visible := True;
end;

procedure TfrmCustomGrid.SetGridExpeditions;
begin
  Caption := rsCaptionExpeditions;
  FSearch.DataSet := DMG.qExpeditions;
  AddSortedField('start_date', sdDescending);

  lblChildTag1.Caption := rsTitleSurveys;
  pChildTag1.Visible := True;
  nbChilds.PageIndex := 0;
  if not Assigned(DMS) then
    DMS := TDMS.Create(nil);
  FChildTable := tbSurveys;
  dsLink1.DataSet := DMS.qSurveys;
  pmcNewSurvey.Visible := True;
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintExpeditions.Visible := True;
  pmPrintExpeditionsByLocality.Visible := True;
  pmPrintExpeditionsByProject.Visible := True;

  pChildsBar.Visible := True;
  sbChildVerifications.Visible := True;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridGazetteer;
begin
  Caption := rsTitleGazetteer;
  FSearch.DataSet := DMG.qGazetteer;
  AddSortedField('site_name', sdAscending);

  sbRecordVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;
  //sbShowDocs.Visible := True;

  pmPrintGazetteer.Visible := True;
  pmPrintGazetteerHierarchical.Visible := True;
end;

procedure TfrmCustomGrid.SetGridIndividuals;
begin
  Caption := rsTitleIndividuals;
  FSearch.DataSet := DMG.qIndividuals;
  AddSortedField('full_name', sdAscending);

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
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintIndividuals.Visible := True;
  pmPrintIndividualsByTaxon.Visible := True;
  pmPrintIndividualsByParents.Visible := True;
  pmPrintColoredBands.Visible := True;

  pChildsBar.Visible := True;
  sbChildVerifications.Visible := True;

  sbShowImages.Visible := True;
  sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridInstitutions;
begin
  Caption := rsTitleInstitutions;
  FSearch.DataSet := DMG.qInstitutions;
  AddSortedField('full_name', sdAscending);

  sbShowSummary.Visible := True;

  pmPrintInstitutions.Visible := True;
end;

procedure TfrmCustomGrid.SetGridMethods;
begin
  Caption := rsTitleMethods;
  FSearch.DataSet := DMG.qMethods;
  AddSortedField('method_name', sdAscending);

  pmPrintMethods.Visible := True;

  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNests;
begin
  Caption := rsTitleNests;
  FSearch.DataSet := DMG.qNests;
  AddSortedField('full_name', sdAscending);

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
  sbRecordVerifications.Visible := True;

  pmPrintNests.Visible := True;
  pmPrintNestsByPeriod.Visible := True;
  pmPrintNestsByProject.Visible := True;
  pmPrintNestsByLocality.Visible := True;
  pmPrintNestsByTaxon.Visible := True;

  pChildsBar.Visible := True;
  sbChildVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;

  sbShowImages.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNestRevisions;
begin
  Caption := rsTitleNestRevisions;
  FSearch.DataSet := DMG.qNestRevisions;
  AddSortedField('full_name', sdAscending);

  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  //sbShowImages.Visible := True;
  //sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridNetStations;
begin
  Caption := rsTitleSamplingPlots;
  FSearch.DataSet := DMG.qNetStations;
  AddSortedField('station_name', sdAscending);

  lblChildTag1.Caption := rsTitlePermanentNets;
  pChildTag1.Visible := True;
  nbChilds.PageIndex := 0;
  FChildTable := tbPermanentNets;
  dsLink1.DataSet := DMG.qPermanentNets;
  pmcNewPermanentNet.Visible := True;
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintSamplingPlots.Visible := True;
  pmPrintSamplingPlotsByLocality.Visible := True;

  pChildsBar.Visible := True;
  sbChildVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
end;

procedure TfrmCustomGrid.SetGridPeople;
begin
  Caption := rsTitleResearchers;
  FSearch.DataSet := DMG.qPeople;
  AddSortedField('full_name', sdAscending);

  sbShowSummary.Visible := True;

  pmPrintResearchers.Visible := True;
end;

procedure TfrmCustomGrid.SetGridPermits;
begin
  Caption := rsTitlePermits;
  FSearch.DataSet := DMG.qPermits;
  AddSortedField('permit_name', sdAscending);

  sbShowSummary.Visible := True;
  //sbShowDocs.Visible := True;

  pmPrintPermits.Visible := True;
  pmPrintPermitsByExpiration.Visible := True;
  pmPrintPermitsByProject.Visible := True;
end;

procedure TfrmCustomGrid.SetGridProjects;
begin
  Caption := rsTitleProjects;
  FSearch.DataSet := DMG.qProjects;
  AddSortedField('project_title', sdAscending);

  lblChildTag1.Caption := rsTitleTeam;
  pChildTag1.Visible := True;
  nbChilds.PageIndex := 0;
  FChildTable := tbProjectTeams;
  dsLink1.DataSet := DMG.qProjectTeam;
  pmcNewProjectMember.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintProjects.Visible := True;

  pChildsBar.Visible := True;

  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSightings;
begin
  Caption := rsTitleSightings;
  FSearch.DataSet := DMG.qSightings;
  AddSortedField('sighting_date', sdDescending);

  sbRecordVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintSightings.Visible := True;
  pmPrintSightingsBySurvey.Visible := True;
  pmPrintSightingsByProject.Visible := True;
  pmPrintSightingsByLocality.Visible := True;
  pmPrintSightingsByTaxon.Visible := True;

  sbShowImages.Visible := True;
  sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSpecimens;
begin
  Caption := rsTitleSpecimens;
  FSearch.DataSet := DMG.qSpecimens;
  AddSortedField('full_name', sdAscending);

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
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintSpecimens.Visible := True;
  pmPrintSpecimensByYear.Visible := True;
  pmPrintSpecimensByProject.Visible := True;
  pmPrintSpecimensByLocality.Visible := True;
  pmPrintSpecimensByTaxon.Visible := True;

  pChildsBar.Visible := True;
  sbChildVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;

  sbShowImages.Visible := True;
  sbShowAudio.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridSurveys;
begin
  Caption := rsTitleSurveys;
  FSearch.DataSet := DMG.qSurveys;
  AddSortedField('survey_date', sdDescending);

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
  sbRecordVerifications.Visible := True;
  sbShowSummary.Visible := True;

  pmPrintSurveys.Visible := True;
  pmPrintSurveysByExpedition.Visible := True;
  pmPrintSurveysByLocality.Visible := True;
  pmPrintSurveysByProject.Visible := True;

  pChildsBar.Visible := True;
  sbChildVerifications.Visible := True;
  mapGeo.Active := True;
  sbShowMap.Visible := True;

  sbShowImages.Visible := True;
  //sbShowDocs.Visible := True;
end;

procedure TfrmCustomGrid.SetGridTaxonRanks;
begin
  Caption := rsTitleTaxonRanks;
  FSearch.DataSet := DMG.qTaxonRanks;
  AddSortedField('rank_seq', sdAscending);
end;

procedure TfrmCustomGrid.SetImages;
begin
  with qImages, SQL do
  begin
    case FTableType of
      tbNone: ;
      //tbUsers: ;
      //tbRecordHistory: ;
      //tbRecordVerifications: ;
      //tbGazetteer: ;
      //tbNetStations: ;
      //tbPermanentNets: ;
      //tbInstitutions: ;
      //tbPeople: ;
      //tbProjects: ;
      //tbProjectTeams: ;
      //tbPermits: ;
      //tbTaxonRanks: ;
      //tbZooTaxa: ;
      //tbBotanicTaxa: ;
      //tbBands: ;
      //tbBandHistory: ;
      tbIndividuals:    Add('WHERE (img.active_status = 1) AND (img.individual_id = :individual_id)');
      tbCaptures:       Add('WHERE (img.active_status = 1) AND (img.capture_id = :capture_id)');
      //tbMolts: ;
      tbNests:          Add('WHERE (img.active_status = 1) AND (img.nest_id = :nest_id)');
      //tbNestOwners: ;
      tbNestRevisions:  Add('WHERE (img.active_status = 1) AND (img.nest_revision_id = :nest_revision_id)');
      tbEggs:           Add('WHERE (img.active_status = 1) AND (img.egg_id = :egg_id)');
      //tbMethods: ;
      //tbExpeditions: ;
      tbSurveys:        Add('WHERE (img.active_status = 1) AND (img.survey_id = :survey_id)');
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      //tbWeatherLogs: ;
      tbSightings:      Add('WHERE (img.active_status = 1) AND (img.sighting_id = :sighting_id)');
      tbSpecimens:      Add('WHERE (img.active_status = 1) AND (img.specimen_id = :specimen_id)');
      //tbSamplePreps: ;
      //tbSpecimenCollectors: ;
      //tbImages: ;
      //tbAudioLibrary: ;
    end;
  end;

  if FTableType in [tbIndividuals, tbCaptures, tbNests, tbNestRevisions, tbEggs, tbSurveys, tbSightings, tbSpecimens] then
  begin
    qImages.SQL.Add('ORDER BY img.image_date, img.image_time ASC');
    qImages.DataSource := dsLink;
    {$IFDEF DEBUG}
    LogSQL(qImages.SQL);
    {$ENDIF}
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
      qRecycle.MacroByName('FID').AsString := 'site_id';
      qRecycle.MacroByName('FNAME').AsString := 'site_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'site_id';
      lblRecycleName.DataField := 'site_name';
    end;
    tbNetStations:
    begin
      qRecycle.MacroByName('FID').AsString := 'net_station_id';
      qRecycle.MacroByName('FNAME').AsString := 'station_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'net_station_id';
      lblRecycleName.DataField := 'station_name';
    end;
    tbPermanentNets: ;
    tbInstitutions:
    begin
      qRecycle.MacroByName('FID').AsString := 'institution_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'institution_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbPeople:
    begin
      qRecycle.MacroByName('FID').AsString := 'person_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'person_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbProjects:
    begin
      qRecycle.MacroByName('FID').AsString := 'project_id';
      qRecycle.MacroByName('FNAME').AsString := 'project_title';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'project_id';
      lblRecycleName.DataField := 'project_title';
    end;
    tbProjectTeams: ;
    tbPermits:
    begin
      qRecycle.MacroByName('FID').AsString := 'permit_id';
      qRecycle.MacroByName('FNAME').AsString := 'permit_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'permit_id';
      lblRecycleName.DataField := 'permit_name';
    end;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa:
    begin
      qRecycle.MacroByName('FID').AsString := 'taxon_id';
      qRecycle.MacroByName('FNAME').AsString := 'taxon_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'taxon_id';
      lblRecycleName.DataField := 'taxon_name';
    end;
    tbBands:
    begin
      qRecycle.MacroByName('FID').AsString := 'band_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'band_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbBandHistory: ;
    tbIndividuals:
    begin
      qRecycle.MacroByName('FID').AsString := 'individual_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'individual_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbCaptures:
    begin
      qRecycle.MacroByName('FID').AsString := 'capture_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'capture_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbMolts: ;
    tbNests:
    begin
      qRecycle.MacroByName('FID').AsString := 'nest_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'nest_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbNestOwners: ;
    tbNestRevisions:
    begin
      qRecycle.MacroByName('FID').AsString := 'nest_revision_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'nest_revision_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbEggs:
    begin
      qRecycle.MacroByName('FID').AsString := 'egg_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'egg_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbMethods:
    begin
      qRecycle.MacroByName('FID').AsString := 'method_id';
      qRecycle.MacroByName('FNAME').AsString := 'method_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'method_id';
      lblRecycleName.DataField := 'method_name';
    end;
    tbExpeditions:
    begin
      qRecycle.MacroByName('FID').AsString := 'expedition_id';
      qRecycle.MacroByName('FNAME').AsString := 'expedition_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'expedition_id';
      lblRecycleName.DataField := 'expedition_name';
    end;
    tbSurveys:
    begin
      qRecycle.MacroByName('FID').AsString := 'survey_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'survey_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbSurveyTeams: ;
    tbNetsEffort: ;
    tbWeatherLogs: ;
    tbSightings:
    begin
      qRecycle.MacroByName('FID').AsString := 'sighting_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'sighting_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbSpecimens:
    begin
      qRecycle.MacroByName('FID').AsString := 'specimen_id';
      qRecycle.MacroByName('FNAME').AsString := 'full_name';
      qRecycle.MacroByName('FTABLE').AsString := TableNames[FTableType];
      lblRecycleId.DataField := 'specimen_id';
      lblRecycleName.DataField := 'full_name';
    end;
    tbSamplePreps: ;
    tbSpecimenCollectors: ;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  qRecycle.Open;

  lblRecycleWarning.Caption := Format(rsRecycleAutoDeleteInfo, [XSettings.ClearDeletedPeriod * 30]);
  pRecycleWarning.Visible := XSettings.ClearDeletedPeriod > 0;
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

procedure TfrmCustomGrid.Summary;
begin
  if not sbShowSummary.Down then
    Exit;

  try
    pMsgSummary.Visible := True;
    gridSummary.BeginUpdate;
    case FTableType of
      tbNone: ;
      tbUsers: ;
      tbRecordHistory: ;
      tbRecordVerifications: ;
      tbGazetteer:          SummaryGazetteer(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbNetStations:        SummarySamplingPlots(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbPermanentNets: ;
      tbInstitutions:       SummaryInstitutions(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbPeople:             SummaryPeople(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbProjects:           SummaryProjects(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbProjectTeams: ;
      tbPermits:            SummaryPermits(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbTaxonRanks: ;
      tbZooTaxa: ;
      tbBotanicTaxa:        SummaryBotanicTaxa(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbBands:              SummaryBands(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbBandHistory: ;
      tbIndividuals:        SummaryIndividuals(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbCaptures:           SummaryCaptures(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbMolts: ;
      tbNests:              SummaryNests(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbNestOwners: ;
      tbNestRevisions:      SummaryNestRevisions(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbEggs:               SummaryEggs(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbMethods: ;
      tbExpeditions:        SummaryExpeditions(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbSurveys:            SummarySurveys(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbSurveyTeams: ;
      tbNetsEffort: ;
      tbWeatherLogs: ;
      tbSightings:          SummarySightings(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbSpecimens:          SummarySpecimens(qChart, DBG.SelectedColumn.FieldName, FSearch.SQLWhere.Text);
      tbSamplePreps: ;
      tbSpecimenCollectors: ;
      tbImages: ;
      tbAudioLibrary: ;
    end;
    gridSummary.AutoAdjustColumns;
  finally
    gridSummary.EndUpdate;
    pMsgSummary.Visible := False;
  end;
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

procedure TfrmCustomGrid.UpdateAudioButtons(aDataSet: TDataSet);
begin
  if Closing then
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
  eAddChild.Enabled := sbAddChild.Enabled;
  sbAddNetsBatch.Enabled := sbAddChild.Enabled;
  pmcEdit.Enabled := sbEditChild.Enabled;
  pmcDel.Enabled := sbDelChild.Enabled;
  pmcRefresh.Enabled := sbRefreshChild.Enabled;
  pmcRecordHistory.Enabled := sbChildHistory.Enabled;
  pmcRecordVerifications.Enabled := sbChildVerifications.Enabled;

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
  if Closing then
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

