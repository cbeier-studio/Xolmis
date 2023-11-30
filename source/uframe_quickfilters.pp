unit uframe_quickfilters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, StdCtrls, EditBtn, BCPanel, BCListBox, rxswitch;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    bvSpacerFilters: TBevel;
    cbAgeFilter: TComboBox;
    cbBandSizeFilter: TComboBox;
    cbBandStatusFilter: TComboBox;
    cbBodyMoltFilter: TComboBox;
    cbBroodPatchFilter: TComboBox;
    cbCloacalProtuberanceFilter: TComboBox;
    cbFatFilter: TComboBox;
    cbFFMoltFilter: TComboBox;
    cbFFWearFilter: TComboBox;
    cbMaterialFilter: TComboBox;
    cbMethodFilter: TComboBox;
    cbNestFateFilter: TComboBox;
    cbNestSupportFilter: TComboBox;
    cbSexFilter: TComboBox;
    cbSiteRankFilter: TComboBox;
    cbSkullOssificationFilter: TComboBox;
    clbTaxonRanksFilter: TCheckListBox;
    eCycleCodeFilter: TEditButton;
    eEndTimeFilter: TTimeEdit;
    eHowAgedFilter: TEditButton;
    eHowSexedFilter: TEditButton;
    eMoltLimitsFilter: TEditButton;
    eStartTimeFilter: TTimeEdit;
    icoAgingFilter: TImage;
    icoBandSizeFilter: TImage;
    icoBandSizeFilter1: TImage;
    icoBandSizeFilter10: TImage;
    icoBandSizeFilter11: TImage;
    icoBandSizeFilter12: TImage;
    icoBandSizeFilter13: TImage;
    icoBandSizeFilter14: TImage;
    icoBandSizeFilter2: TImage;
    icoBandSizeFilter3: TImage;
    icoBandSizeFilter4: TImage;
    icoBandSizeFilter5: TImage;
    icoBandSizeFilter6: TImage;
    icoBandSizeFilter7: TImage;
    icoBandSizeFilter8: TImage;
    icoBandSizeFilter9: TImage;
    icoBandStatusFilter: TImage;
    icoDateFilter: TImage;
    icoDeletedFilter: TImage;
    icoMarkedFilter: TImage;
    icoUnmarkedFilter: TImage;
    icoExtinctFilter: TImage;
    icoFatFilter: TImage;
    icoMarkedFilter12: TImage;
    icoMaterialFilter: TImage;
    icoMethodFilter: TImage;
    icoMoltFilter: TImage;
    icoNestFateFilter: TImage;
    icoNestSupportFilter: TImage;
    icoReportedFilter1: TImage;
    icoSexFilter: TImage;
    icoSiteFilter: TImage;
    icoSiteRankFilter: TImage;
    icoSynonymsFilter: TImage;
    icoTaxaFilter: TImage;
    icoTaxonomiesFilter: TImage;
    icoTaxonRanksFilter: TImage;
    icoWithColorBandsFilter: TImage;
    icoWithRecapturesFilter: TImage;
    lblAgeFilter: TLabel;
    lblBandSizeFilter: TLabel;
    lblBandStatusFilter: TLabel;
    lblBodyMoltFilter: TLabel;
    lblBroodPatchFilter: TLabel;
    lblClementsFilter: TLabel;
    lblCloacalProtuberanceFilter: TLabel;
    lblCountDateFilter: TLabel;
    lblCountSiteFilter: TLabel;
    lblCountTaxonFilter: TLabel;
    lblCountTaxonRanksFilter: TLabel;
    lblCycleCodeFilter: TLabel;
    lblDateFilter: TLabel;
    lblEndTimeFilter: TLabel;
    lblExtinctFilter: TLabel;
    lblFatFilter: TLabel;
    lblFFMoltFilter: TLabel;
    lblFFWearFilter: TLabel;
    lblHasSynonymsFilter: TLabel;
    lblHowAgedFilter: TLabel;
    lblHowSexedFilter: TLabel;
    lblLixoFilter: TLabel;
    lblMarkedFilter: TLabel;
    lblUnmarkedFilter: TLabel;
    lblMaterialFilter: TLabel;
    lblMethodFilter: TLabel;
    lblMoltLimitsFilter: TLabel;
    lblNestFateFilter: TLabel;
    lblNestSupportFilter: TLabel;
    lblNotReportedFilter: TLabel;
    lblReportedFilter1: TLabel;
    lblSexFilter: TLabel;
    lblSiteFilter: TLabel;
    lblSiteRankFilter: TLabel;
    lblSkullOssificationFilter: TLabel;
    lblStartTimeFilter: TLabel;
    lblSynonymFilter: TLabel;
    lblTaxonFilter: TLabel;
    lblTaxonomyCbroFilter: TLabel;
    lblTaxonomyIocFilter: TLabel;
    lblTaxonRanksFilter: TLabel;
    lblWithColorBandsFilter: TLabel;
    lblWithRecapturesFilter: TLabel;
    pBandSizeFilter: TBCPanel;
    pBandStatusFilter: TBCPanel;
    pBodyMoltFilter: TBCPanel;
    pBroodPatchFilter: TBCPanel;
    pEndTimeFilter: TBCPanel;
    pStartTimeFilter: TBCPanel;
    pSkullOssificationFilter: TBCPanel;
    pCloacalProtuberanceFilter: TBCPanel;
    pHowAgedFilter: TBCPanel;
    pAgeFilter: TBCPanel;
    pTaxonomyIocFilter: TBCPanel;
    pDateFilter: TPanel;
    pDatesFilters: TBCPanel;
    pExtinctFilter: TBCPanel;
    pFatFilter: TBCPanel;
    pFFMoltFilter: TBCPanel;
    pFFWearFilter: TBCPanel;
    pHasSynonymsFilter: TBCPanel;
    pHowSexedFilter: TBCPanel;
    pTaxonomyCbroFilter: TBCPanel;
    pIsSynonymFilter: TBCPanel;
    pLixoFilter: TBCPanel;
    pMarkedFilter: TBCPanel;
    pUnmarkedFilter: TBCPanel;
    pMaterialFilter: TBCPanel;
    pMethodFilter: TBCPanel;
    pMoltCycleFilter: TBCPanel;
    pMoltLimitsFilter: TBCPanel;
    pNestFateFilter: TBCPanel;
    pNestSupportFilter: TBCPanel;
    pNotReportedFilter: TBCPanel;
    pReportedFilter: TBCPanel;
    pSexFilter: TBCPanel;
    pTaxonomyClementsFilter: TBCPanel;
    pSiteFilters: TBCPanel;
    pSiteRankFilter: TBCPanel;
    pTaxonFilters: TBCPanel;
    pTaxonRanksFilters: TBCPanel;
    pTitleSiteFilter: TPanel;
    pTitleTaxonFilter: TPanel;
    pTitleTaxonRanksFilter: TPanel;
    pWithColorBandsFilter: TBCPanel;
    pWithRecapturesFilter: TBCPanel;
    scrollFilter: TScrollBox;
    tsBandNotReported: TRxSwitch;
    tsBandReported1: TRxSwitch;
    tsfLixo: TRxSwitch;
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
  private

  public

  end;

implementation

initialization
  {$I unit1.lrs}

end.

