unit ufrm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  BCPanel, Buttons, Classes, ComCtrls, DB, fgl, mvDE_BGRA, mvMapViewer, mvTypes, mvGpsObj, SQLDB, DBCtrls, DBGrids,
  httpprotocol, LCLIntf, LCLType, ExtCtrls, Menus, StdCtrls, ColorSpeedButton, BGRAShape, SysUtils, Forms, RegExpr,
  Controls, Graphics, Dialogs, data_types, Grids, attabs, TADbSource, TAGraph, TAGUIConnectorBGRA, Translations,
  TASeries, TASources, Types, mvDrawingEngine, mvPluginCommon, mvMarkerPlugins, mvPlugins, mvMapScalePlugin,
  TACustomSeries, TARadialSeries, TADrawUtils, TAChartUtils, TATools;

type

  { TfrmTaxa }

  TfrmTaxa = class(TForm)
    MvPluginMan: TMvPluginManager;
    mvpLegalNoticePlugin: TLegalNoticePlugin;
    mvpMapScalePlugin: TMapScalePlugin;
    mvpMarkerHintPlugin: TMarkerHintPlugin;
    SaveChartDlg: TSaveDialog;
    sbCopyNestFateChart: TSpeedButton;
    sbCopySeasonalityChart: TSpeedButton;
    sbSaveNestFateChart: TSpeedButton;
    sbSaveSeasonalityChart: TSpeedButton;
    shpMapLegendSightings: TBGRAShape;
    shpMapLegendCaptures: TBGRAShape;
    shpMapLegendNests: TBGRAShape;
    shpMapLegendSpecimens: TBGRAShape;
    chartNestFate: TChart;
    chartSeasonality: TChart;
    chartTools: TChartToolset;
    chartToolsDataPointHintTool1: TDataPointHintTool;
    chartYear: TChart;
    chipConservationStatus: TBCPanel;
    eSearch: TEdit;
    gridTaxa: TDBGrid;
    iconSearch: TImage;
    lblMapLegendSightings: TLabel;
    lblMapLegendCaptures: TLabel;
    lblMapLegendNests: TLabel;
    lblMapLegendSpecimens: TLabel;
    lblRecordsMapTitle: TLabel;
    lblTopLocalitiesTitle: TLabel;
    mapView: TMapView;
    pMapLegendSightings: TPanel;
    pMapLegendCaptures: TPanel;
    pMapLegendNests: TPanel;
    pMapLegendSpecimens: TPanel;
    pMapLegend: TPanel;
    pTaxonList: TPanel;
    pRecordsMap: TBCPanel;
    pMapLocalities: TPanel;
    pSearch: TBCPanel;
    pSummaryIndicators: TFlowPanel;
    pExternalLinks: TFlowPanel;
    pBreedingIndicators: TFlowPanel;
    icoBirdsOfTheWorld: TImage;
    icoEbird: TImage;
    icoWikiaves: TImage;
    icoIucn: TImage;
    icoGbif: TImage;
    icoGoogle: TImage;
    icoGoogleImages: TImage;
    icoGoogleScholar: TImage;
    lblTotalRecords: TLabel;
    lblDistributionTitle: TLabel;
    lblEbird: TLabel;
    lblWikiaves: TLabel;
    lblIucn: TLabel;
    lblGbif: TLabel;
    lblGoogle: TLabel;
    lblGoogleImages: TLabel;
    lblGoogleScholar: TLabel;
    lblRecordsPerYearTitle: TLabel;
    lblSeasonalityTitle: TLabel;
    lblTotalNests: TLabel;
    lblNestProductivity: TLabel;
    lblNestFateTitle: TLabel;
    lblNestLossCausesTitle: TLabel;
    monthlySeries: TBarSeries;
    NestFateSeries: TPieSeries;
    pChartsYearSeasonality: TPanel;
    pNestFateLosses: TPanel;
    pChartRecordsPerYear: TBCPanel;
    pChartSeasonality: TBCPanel;
    pTopLocalities: TBCPanel;
    pTotalNestsIndicator: TBCPanel;
    pNestProductivityIndicator: TBCPanel;
    pChartNestFate: TBCPanel;
    pChartNestLossCauses: TBCPanel;
    sbClearSearch: TColorSpeedButton;
    sbOptionsSearch: TColorSpeedButton;
    scrollCaptures: TScrollBox;
    scrollBreeding: TScrollBox;
    scrollQuality: TScrollBox;
    scrollEcology: TScrollBox;
    scrollOccurrence: TScrollBox;
    sbShareRecords: TSpeedButton;
    sbPrint: TSpeedButton;
    sbSaveRecordsYearChart: TSpeedButton;
    sbCopyRecordsYearChart: TSpeedButton;
    txtNestProductivity: TLabel;
    txtSynonyms: TLabel;
    lblSynonymsTitle: TLabel;
    lblSightingsCount: TLabel;
    lblIndividualsCount: TLabel;
    lblCapturesCount: TLabel;
    lblSpecimensCount: TLabel;
    lblNestsCount: TLabel;
    lblEggsCount: TLabel;
    lblBirdsOfTheWorld: TLabel;
    navTabs: TATTabs;
    nbContent: TNotebook;
    pDistributionSynonyms: TPanel;
    pgQuality: TPage;
    pgEcology: TPage;
    pgCaptures: TPage;
    pgBreeding: TPage;
    pgOccurrence: TPage;
    pgSummary: TPage;
    pTotalRecordsIndicator: TBCPanel;
    chipBirdsOfTheWorld: TBCPanel;
    chipEbird: TBCPanel;
    chipWikiaves: TBCPanel;
    chipIucn: TBCPanel;
    chipGbif: TBCPanel;
    chipGoogle: TBCPanel;
    chipGoogleImage: TBCPanel;
    chipGoogleScholar: TBCPanel;
    pDistribution: TBCPanel;
    pSynonyms: TBCPanel;
    pSightingsCountIndicator: TBCPanel;
    pIndividualsCountIndicator: TBCPanel;
    pCapturesCountIndicator: TBCPanel;
    pSpecimensCountIndicator: TBCPanel;
    pNestsCountIndicator: TBCPanel;
    pEggsCountIndicator: TBCPanel;
    scrollSummary: TScrollBox;
    txtDistribution: TDBText;
    txtExtinctionYear: TDBText;
    txtHierarchy: TLabel;
    txtTotalRecords: TLabel;
    txtSightingsCount: TLabel;
    txtIndividualsCount: TLabel;
    txtCapturesCount: TLabel;
    txtSpecimensCount: TLabel;
    txtNestsCount: TLabel;
    txtEggsCount: TLabel;
    txtTotalNests: TLabel;
    txtVernacularNames: TLabel;
    txtConservationStatus: TLabel;
    pHeader: TBCPanel;
    dsVernacular: TDataSource;
    lblEmptyQuery: TLabel;
    pEmptyQueryOptions: TBCPanel;
    sbEmptyClearAll: TSpeedButton;
    sbEmptyGotoSearch: TSpeedButton;
    txtAuthorship: TDBText;
    txtRank: TDBText;
    txtScientificName: TDBText;
    chartBGRA: TChartGUIConnectorBGRA;
    dsSynonyms: TDataSource;
    dsChilds: TDataSource;
    MvBGRA: TMvBGRADrawingEngine;
    pEmptyQuery: TPanel;
    pmfShowRecordedTaxa: TMenuItem;
    pmOptions: TPopupMenu;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    imgIUCN: TImageList;
    iSearch: TImageList;
    iSearchDark: TImageList;
    pContent: TPanel;
    pmPrint: TPopupMenu;
    pmPrintTaxa: TMenuItem;
    pmPrintTaxaHierarchical: TMenuItem;
    pmPrintTaxaRecorded: TMenuItem;
    pmPrintTaxaRecordedByLocality: TMenuItem;
    SplitRight: TSplitter;
    TimerData: TTimer;
    TimerFind: TTimer;
    dsLink: TDataSource;
    txtEnglishLang: TLabel;
    txtVernacularLang: TLabel;
    txtEnglishNames: TLabel;
    txtVernacularDiv: TLabel;
    yearlySeries: TBarSeries;
    procedure chartToolsDataPointHintTool1Hint(ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
    procedure chipBirdsOfTheWorldClick(Sender: TObject);
    procedure chipEbirdClick(Sender: TObject);
    procedure chipGbifClick(Sender: TObject);
    procedure chipGoogleClick(Sender: TObject);
    procedure chipGoogleImageClick(Sender: TObject);
    procedure chipGoogleScholarClick(Sender: TObject);
    procedure chipIucnClick(Sender: TObject);
    procedure chipWikiavesClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure dsLinkStateChange(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure eSearchEnter(Sender: TObject);
    procedure eSearchExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridTaxaMouseWheel
      (Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure mapViewDrawGpsPoint
      (Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure mvpMarkerHintPluginCreateHintWindow(AMapView: TMapView; out AHintWindow: THintWindow);
    procedure navTabsTabClick(Sender: TObject);
    procedure pCapturesCountIndicatorClick(Sender: TObject);
    procedure pEggsCountIndicatorClick(Sender: TObject);
    procedure pIndividualsCountIndicatorClick(Sender: TObject);
    procedure pmfShowRecordedTaxaClick(Sender: TObject);
    procedure pNestsCountIndicatorClick(Sender: TObject);
    procedure pSightingsCountIndicatorClick(Sender: TObject);
    procedure pSpecimensCountIndicatorClick(Sender: TObject);
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbCopyNestFateChartClick(Sender: TObject);
    procedure sbCopyRecordsYearChartClick(Sender: TObject);
    procedure sbCopySeasonalityChartClick(Sender: TObject);
    procedure sbEmptyClearAllClick(Sender: TObject);
    procedure sbEmptyGotoSearchClick(Sender: TObject);
    procedure sbOptionsSearchClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbSaveNestFateChartClick(Sender: TObject);
    procedure sbSaveRecordsYearChartClick(Sender: TObject);
    procedure sbSaveSeasonalityChartClick(Sender: TObject);
    procedure sbShareRecordsClick(Sender: TObject);
    procedure scrollDataMouseWheel
      (Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure SplitRightMoved(Sender: TObject);
    procedure TimerDataTimer(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure txtValidNameMouseEnter(Sender: TObject);
    procedure txtValidNameMouseLeave(Sender: TObject);
  private
    FSearch: TCustomSearch;
    FSearchString, OldSearchString: String;
    CanToggle: Boolean;
    procedure ApplyDarkMode;
    procedure GetSightingsCount;
    procedure GetIndividualsCount;
    procedure GetCapturesCount;
    procedure GetSpecimensCount;
    procedure GetNestsCount;
    procedure GetEggsCount;
    procedure GetNestProductivity;
    procedure GetSynonyms;
    procedure GetVernacular(const TaxonId: Integer);
    function GetVernacularNameFallback(const TaxonId: Integer): string;
    procedure LoadMonthlyChart;
    procedure LoadNestFateChart;
    procedure LoadRecordsMap;
    procedure LoadYearlyChart;
    function OccurrInBrazil(aTaxonId: Integer): Boolean;
    procedure SaveChart(const AChart: TChart);
    function Search(AValue: String): Boolean;
    procedure SetSearchString(aValue: String);
    procedure UpdateButtons;
  public
    property SearchString: String read FSearchString write SetSearchString;
  end;

var
  frmTaxa: TfrmTaxa;

implementation

uses
  utils_global, utils_locale, utils_themes, utils_conversions, utils_dialogs, utils_taxonomy,
  data_search, data_getvalue, data_consts,
  models_record_types, models_taxonomy, models_access_control, models_users,
  ufrm_main, udm_main, udm_grid, udm_taxa,
  uDarkStyleParams, TADrawerSVG, TADrawerBGRA;

{$R *.lfm}

{ TfrmTaxa }

procedure TfrmTaxa.ApplyDarkMode;
begin
  pEmptyQuery.Color := Color;
  sbEmptyGotoSearch.Images := iButtonsDark;
  sbEmptyClearAll.Images := iButtonsDark;

  pEmptyQueryOptions.Background.Color := ActiveTheme.Background.SolidSecondary;
  pEmptyQueryOptions.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pEmptyQueryOptions.Color := pEmptyQuery.Color;

  pSearch.Background.Color := ActiveTheme.Background.ControlDefault;
  pSearch.Border.Color := ActiveTheme.Border.Default; // ActiveTheme.Background.SolidSecondary;
  pSearch.ParentBackground := True;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateHover.Color := ActiveTheme.Background.SolidSecondary;
  sbClearSearch.StateActive.Color := ActiveTheme.Background.SolidTertiary;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
  sbOptionsSearch.StateHover.Color := ActiveTheme.Background.SolidSecondary;
  sbOptionsSearch.StateActive.Color := ActiveTheme.Background.SolidTertiary;
  sbOptionsSearch.StateNormal.Color := pSearch.Background.Color;
  iconSearch.Images := iSearchDark;
  sbClearSearch.Images := iSearchDark;
  sbOptionsSearch.Images := iSearchDark;

  sbShareRecords.Images := iButtonsDark;
  sbPrint.Images := iButtonsDark;

  pContent.Color := clDefault;

  navTabs.ColorBg := clBtnFace;
  navTabs.ColorTabActive := clBtnFace;
  navTabs.ColorTabOver := clBtnFace;
  navTabs.ColorTabPassive := clSolidBGBaseDark;
  navTabs.ColorFontActive := clTextPrimaryDark;
  navTabs.ColorFontHot := clTextPrimaryDark;

  icoGoogle.Images := iButtonsDark;
  icoGoogleImages.Images := iButtonsDark;
  icoGoogleScholar.Images := iButtonsDark;
  icoBirdsOfTheWorld.Images := iButtonsDark;
  icoEbird.Images := iButtonsDark;
  icoWikiaves.Images := iButtonsDark;
  icoIUCN.Images := iButtonsDark;
  icoGBIF.Images := iButtonsDark;

  chartSeasonality.Color := ActiveTheme.Background.CardDefault;
  chartSeasonality.BackColor := ActiveTheme.Background.CardDefault;
  chartSeasonality.Title.Font.Color := ActiveTheme.Foreground.TextPrimary;
  chartSeasonality.Legend.Font.Color := ActiveTheme.Foreground.TextPrimary;
  chartSeasonality.BottomAxis.Title.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartSeasonality.BottomAxis.Marks.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartSeasonality.LeftAxis.Title.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartSeasonality.LeftAxis.Marks.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartSeasonality.LeftAxis.Grid.Color := ActiveTheme.Border.Default;

  chartYear.Color := ActiveTheme.Background.CardDefault;
  chartYear.BackColor := ActiveTheme.Background.CardDefault;
  chartYear.Title.Font.Color := ActiveTheme.Foreground.TextPrimary;
  chartYear.Legend.Font.Color := ActiveTheme.Foreground.TextPrimary;
  chartYear.BottomAxis.Title.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartYear.BottomAxis.Marks.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartYear.LeftAxis.Title.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartYear.LeftAxis.Marks.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;
  chartYear.LeftAxis.Grid.Color := ActiveTheme.Border.Default;

  chartNestFate.Color := ActiveTheme.Background.CardDefault;
  chartNestFate.BackColor := ActiveTheme.Background.CardDefault;
  chartNestFate.Title.Font.Color := ActiveTheme.Foreground.TextPrimary;
  chartNestFate.Legend.Font.Color := ActiveTheme.Foreground.TextPrimary;
  NestFateSeries.Marks.LabelFont.Color := ActiveTheme.Foreground.TextPrimary;

  pNestProductivityIndicator.Background.Color := ActiveTheme.Background.SolidSecondary;
  pNestProductivityIndicator.Border.Color := ActiveTheme.Border.Control;

  txtTotalRecords.Font.Color := ActiveTheme.Interactive.WindowTitle;

  pTotalRecordsIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pTotalRecordsIndicator.Border.Color := ActiveTheme.Border.Control;
  pSightingsCountIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pSightingsCountIndicator.Border.Color := ActiveTheme.Border.Control;
  pIndividualsCountIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pIndividualsCountIndicator.Border.Color := ActiveTheme.Border.Control;
  pCapturesCountIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pCapturesCountIndicator.Border.Color := ActiveTheme.Border.Control;
  pSpecimensCountIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pSpecimensCountIndicator.Border.Color := ActiveTheme.Border.Control;
  pNestsCountIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pNestsCountIndicator.Border.Color := ActiveTheme.Border.Control;
  pEggsCountIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pEggsCountIndicator.Border.Color := ActiveTheme.Border.Control;

  pDistribution.Background.Color := ActiveTheme.Background.CardDefault;
  pDistribution.Border.Color := ActiveTheme.Border.Control;
  pSynonyms.Background.Color := ActiveTheme.Background.CardDefault;
  pSynonyms.Border.Color := ActiveTheme.Border.Control;

  chipBirdsOfTheWorld.Background.Color := ActiveTheme.Background.CardDefault;
  chipBirdsOfTheWorld.Border.Color := ActiveTheme.Border.Control;
  chipEbird.Background.Color := ActiveTheme.Background.CardDefault;
  chipEbird.Border.Color := ActiveTheme.Border.Control;
  chipGbif.Background.Color := ActiveTheme.Background.CardDefault;
  chipGbif.Border.Color := ActiveTheme.Border.Control;
  chipGoogle.Background.Color := ActiveTheme.Background.CardDefault;
  chipGoogle.Border.Color := ActiveTheme.Border.Control;
  chipGoogleImage.Background.Color := ActiveTheme.Background.CardDefault;
  chipGoogleImage.Border.Color := ActiveTheme.Border.Control;
  chipGoogleScholar.Background.Color := ActiveTheme.Background.CardDefault;
  chipGoogleScholar.Border.Color := ActiveTheme.Border.Control;
  chipIucn.Background.Color := ActiveTheme.Background.CardDefault;
  chipIucn.Border.Color := ActiveTheme.Border.Control;
  chipWikiaves.Background.Color := ActiveTheme.Background.CardDefault;
  chipWikiaves.Border.Color := ActiveTheme.Border.Control;

  pChartRecordsPerYear.Background.Color := ActiveTheme.Background.CardDefault;
  pChartRecordsPerYear.Border.Color := ActiveTheme.Border.Control;
  pChartSeasonality.Background.Color := ActiveTheme.Background.CardDefault;
  pChartSeasonality.Border.Color := ActiveTheme.Border.Control;

  pRecordsMap.Background.Color := ActiveTheme.Background.CardDefault;
  pRecordsMap.Border.Color := ActiveTheme.Border.Control;
  pMapLegend.Color := ActiveTheme.Background.CardDefault;
  pTopLocalities.Background.Color := ActiveTheme.Background.CardDefault;
  pTopLocalities.Border.Color := ActiveTheme.Border.Control;

  pTotalNestsIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pTotalNestsIndicator.Border.Color := ActiveTheme.Border.Control;
  pNestProductivityIndicator.Background.Color := ActiveTheme.Background.CardDefault;
  pNestProductivityIndicator.Border.Color := ActiveTheme.Border.Control;

  pChartNestFate.Background.Color := ActiveTheme.Background.CardDefault;
  pChartNestFate.Border.Color := ActiveTheme.Border.Control;
  pChartNestLossCauses.Background.Color := ActiveTheme.Background.CardDefault;
  pChartNestLossCauses.Border.Color := ActiveTheme.Border.Control;
end;

procedure TfrmTaxa.chartToolsDataPointHintTool1Hint(ATool: TDataPointHintTool; const APoint: TPoint;
  var AHint: String);
var
  ASeries: TBarSeries;
  APie: TPieSeries;
  AName: String;
  AValue: Double;
begin
  if (ATool.Series is TPieSeries) then
  begin
    APie := TPieSeries(ATool.Series);

    //AName  := APie.Source. Styles.Styles[ATool.YIndex].Text;
    AValue := APie.GetYValues(ATool.PointIndex, ATool.YIndex);

    AHint := Format('%d', [Round(AValue)]);
  end;

  if (ATool.Series is TBarSeries) then
  begin
    ASeries := TBarSeries(ATool.Series);

    AName  := ASeries.Styles.Styles[ATool.YIndex].Text;
    AValue := ASeries.GetYValues(ATool.PointIndex, ATool.YIndex);

    AHint := Format('%s: %d', [AName, Round(AValue)]);
    // Se quiser incluir o mês:
    // AHint := Format('Mês %d - %s: %d', [Round(ASeries.GetXValue(ATool.PointIndex)), AName, Round(AValue)]);
  end;
end;

procedure TfrmTaxa.chipBirdsOfTheWorldClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('ebird_code').AsString);
  Url := 'https://birdsoftheworld.org/bow/species/' + FUrlSearch;
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipEbirdClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('ebird_code').AsString);
  Url := 'https://ebird.org/species/' + FUrlSearch;
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipGbifClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('scientific_name').AsString);
  Url := 'https://www.gbif.org/search?q=' + FUrlSearch;
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipGoogleClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('scientific_name').AsString);
  Url := 'https://www.google.com/search?q="' + FUrlSearch + '"';
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipGoogleImageClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('scientific_name').AsString);
  Url := 'https://www.google.com/search?tbm=isch&q="' + FUrlSearch + '"';
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipGoogleScholarClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('scientific_name').AsString);
  Url := 'https://scholar.google.com/scholar?q="' + FUrlSearch + '"';
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipIucnClick(Sender: TObject);
var
  FUrlSearch: String;
  Url: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('scientific_name').AsString);
  Url := 'https://www.iucnredlist.org/search?query=' + FUrlSearch;
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.chipWikiavesClick(Sender: TObject);
var
  FUrlSearch, FVernacular: String;
  Url: String;
  FLang: Integer;
begin
  FLang := 0;
  FVernacular := EmptyStr;

  if GetRankType(dsLink.DataSet.FieldByName('rank_id').AsInteger) = trSpecies then
  begin
    FLang := GetKey(TBL_LANGUAGES, COL_LANGUAGE_ID, COL_LANGUAGE_NAME, rsPortuguese);
    FVernacular := GetVernacularName(dsLink.DataSet.FieldByName('taxon_id').AsInteger, FLang);
    FUrlSearch := HTTPEncode(RemoveDiacritics(FVernacular))
  end
  else
    FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('scientific_name').AsString);
  Url := 'https://www.wikiaves.com.br/wiki/' + FUrlSearch;
  if not HasInternetConnection(Url) then
  begin
    MsgDlg(rsTitleNoInternetConnection, rsErrorNoInternetConnection, mtWarning);
    Exit;
  end;
  OpenUrl(Url);
end;

procedure TfrmTaxa.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  TimerData.Enabled := False;
  TimerData.Enabled := True;

end;

procedure TfrmTaxa.dsLinkStateChange(Sender: TObject);
begin
  pEmptyQuery.Visible := (dsLink.DataSet.RecordCount = 0);

  UpdateButtons;
end;

procedure TfrmTaxa.eSearchChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  sbClearSearch.Visible := Length(eSearch.Text) > 0;
  sbEmptyClearAll.Visible := sbClearSearch.Visible;
end;

procedure TfrmTaxa.eSearchEnter(Sender: TObject);
begin
  //if eSearch.Text = EmptyStr then
  //  pSearch.Width := ClientWidth div 4;
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := ActiveTheme.Background.SolidBase;
    pSearch.Border.Color := ActiveTheme.AccentPalette.Dark1; //clSolidBGTertiaryDark;
  end
  else
  begin
    pSearch.Background.Color := clWhite;
    pSearch.Border.Color := ActiveTheme.AccentFill.Tertiary;
  end;
  //pSearch.Border.Width := 2;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
  sbOptionsSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TfrmTaxa.eSearchExit(Sender: TObject);
begin
  //if eSearch.Text = EmptyStr then
  //  pSearch.Width := 148;
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := ActiveTheme.Background.ControlDefault;
    pSearch.Border.Color := ActiveTheme.Border.Default; // ActiveTheme.Background.SolidSecondary;
  end
  else
  begin
    pSearch.Background.Color := ActiveTheme.Background.CardSecondary;
    pSearch.Border.Color := ActiveTheme.Border.Default;
  end;
  pSearch.Border.Width := 1;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
  sbOptionsSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TfrmTaxa.FormCreate(Sender: TObject);
begin
  CanToggle := True;

  if not Assigned(DMT) then
    DMT := TDMT.Create(nil);
end;

procedure TfrmTaxa.FormDestroy(Sender: TObject);
begin
  dsChilds.DataSet.Close;
  dsVernacular.DataSet.Close;
  dsSynonyms.DataSet.Close;
  dsLink.DataSet.Close;

  if Assigned(DMT) then
    FreeAndNil(DMT);

  FreeAndNil(FSearch);
end;

procedure TfrmTaxa.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    sbClearSearchClick(nil);  { Clear search }

    Key := #0;
  end;
end;

procedure TfrmTaxa.FormResize(Sender: TObject);
begin
  //pEmptyQuery.Left := pToolbar.Left;
  //pEmptyQuery.Width := Width - gridTaxa.Width;
  //pEmptyQuery.Top := gridTaxa.Top;
  //pEmptyQuery.Height := gridTaxa.Height;
end;

procedure TfrmTaxa.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  navTabs.GetTabData(0).TabCaption := rsTaxonSummary;
  navTabs.GetTabData(1).TabCaption := rsTaxonOccurrence;
  navTabs.GetTabData(2).TabCaption := rsTaxonBreeding;
  navTabs.GetTabData(3).TabCaption := rsTaxonCapturesAndMorphometry;
  navTabs.GetTabData(4).TabCaption := rsTaxonEcology;
  navTabs.GetTabData(5).TabCaption := rsTaxonQualityAndGaps;

  FSearch := TCustomSearch.Create(tbZooTaxa);
  FSearch.DataSet := DMG.qTaxa;

  SetZooTaxaSQL(DMG.qTaxa.SQL, fvReset);

  dsLink.DataSet.Open;
  dsVernacular.DataSet.Open;
  dsSynonyms.DataSet.Open;
  dsChilds.DataSet.Open;

  SplitRightMoved(nil);

  pEmptyQuery.AnchorSideLeft.Side := asrRight;
  pEmptyQuery.AnchorSideLeft.Control := SplitRight;
  pEmptyQuery.AnchorSideRight.Side := asrRight;
  pEmptyQuery.AnchorSideRight.Control := Self;
  pEmptyQuery.AnchorSideTop.Side := asrTop;
  pEmptyQuery.AnchorSideTop.Control := pContent;
  pEmptyQuery.AnchorSideBottom.Side := asrBottom;
  pEmptyQuery.AnchorSideBottom.Control := Self;
  pEmptyQuery.Anchors := [akLeft, akRight, akTop, akBottom];
  pEmptyQuery.BringToFront;

  if IsDarkModeEnabled then
  begin
    pHeader.Background.Color := ActiveTheme.Brand.JacarandaDeep;
    pHeader.Border.Color := ActiveTheme.Brand.JacarandaContainer;
  end
  else
  begin
    pHeader.Background.Color := ActiveTheme.Brand.JacarandaContainer;
    pHeader.Border.Color := ActiveTheme.Brand.JacarandaDeep;
  end;
  if IsDarkModeEnabled then
    txtScientificName.Font.Color := ActiveTheme.Brand.JacarandaContainer
  else
    txtScientificName.Font.Color := ActiveTheme.Brand.JacarandaDeep;
  txtAuthorship.Font.Color := ActiveTheme.Foreground.TextPrimary;

  mapView.CachePath := IncludeTrailingPathDelimiter(ConcatPaths([AppDataDir, 'map-cache']));
  mapView.Active := True;
  shpMapLegendSightings.FillColor := clCatGencianaBGLight;
  shpMapLegendSightings.BorderColor := clCatGencianaFGLight;
  shpMapLegendCaptures.FillColor := clCatMandacaruBGLight;
  shpMapLegendCaptures.BorderColor := clCatMandacaruFGLight;
  shpMapLegendNests.FillColor := clCatIpeAmareloBGLight;
  shpMapLegendNests.BorderColor := clCatIpeAmareloFGLight;
  shpMapLegendSpecimens.FillColor := clCatQuaresmeiraBGLight;
  shpMapLegendSpecimens.BorderColor := clCatQuaresmeiraFGLight;

  chartSeasonality.Title.Text.Text := rsSeasonality;
  chartSeasonality.LeftAxis.Title.Caption := rsNumberOfRecords;
  chartSeasonality.BottomAxis.Title.Caption := rsMonth;
  chartYear.Title.Text.Text := rsRecordPerYear;
  chartYear.LeftAxis.Title.Caption := rsNumberOfRecords;
  chartYear.BottomAxis.Title.Caption := rsYear;
  chartNestFate.Title.Text.Text := rsNestFate;
end;

procedure TfrmTaxa.GetCapturesCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM captures');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    txtCapturesCount.Caption := IntToStr(C);
    txtCapturesCount.Enabled := C > 0;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetEggsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM eggs');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    txtEggsCount.Caption := IntToStr(C);
    txtEggsCount.Enabled := C > 0;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetIndividualsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM individuals');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    txtIndividualsCount.Caption := IntToStr(C);
    txtIndividualsCount.Enabled := C > 0;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetNestProductivity;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT avg(nest_productivity) AS counter FROM nests');
    Add('WHERE (nest_productivity > 0) AND (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY taxon_id');
    ParamByName('taxon_id').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    if RecordCount > 0 then
      C := FieldByName('counter').AsInteger
    else
      C := 0;
    Close;

    txtNestProductivity.Caption := IntToStr(C);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetNestsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM nests');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    txtNestsCount.Caption := IntToStr(C);
    txtNestsCount.Enabled := C > 0;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetSightingsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM sightings');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    txtSightingsCount.Caption := IntToStr(C);
    txtSightingsCount.Enabled := C > 0;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetSpecimensCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM specimens');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    txtSpecimensCount.Caption := IntToStr(C);
    txtSpecimensCount.Enabled := C > 0;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetSynonyms;
var
  S: String;
begin
  S := '';

  if not DMG.qSynonymTaxa.IsEmpty then
  begin
    DMG.qSynonymTaxa.DisableControls;
    try
      DMG.qSynonymTaxa.First;
      while not DMG.qSynonymTaxa.EOF do
      begin
        if DMG.qSynonymTaxa.RecNo = 1 then
          S := DMG.qSynonymTaxa.FieldByName(COL_SCIENTIFIC_NAME).AsString
        else
          S := S + LineEnding + DMG.qSynonymTaxa.FieldByName(COL_SCIENTIFIC_NAME).AsString;
        DMG.qSynonymTaxa.Next;
      end;
    finally
      DMG.qSynonymTaxa.EnableControls;
    end;
  end;

  if S <> '' then
  begin
    txtSynonyms.Caption := S;
    txtSynonyms.Enabled := True;
  end
  else
  begin
    txtSynonyms.Caption := rsNoSynonyms;
    txtSynonyms.Enabled := False;
  end;
end;

procedure TfrmTaxa.GetVernacular(const TaxonId: Integer);
var
  Lang, FallbackLang: TLanguageID;
  S, IdiomaApp, MacroLang, CountryCode: string;
  SeparadorPos: Integer;
  Qry: TSQLQuery;
begin
  S := '';
  FallbackLang.LanguageCode := 'en';

  txtVernacularLang.Visible := False;
  txtVernacularNames.Visible := False;
  txtVernacularDiv.Visible := False;

  Lang := GetLanguageID;

  MacroLang := Lang.LanguageCode;
  CountryCode := Lang.CountryCode;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    if LowerCase(MacroLang) <> 'en' then
    begin
      SQL.Text :=
        'SELECT v.vernacular_name ' +
        'FROM zoo_vernacular v ' +
        'INNER JOIN languages l ON v.language_id = l.language_id ' +
        'WHERE v.taxon_id = :TaxonId ' +
        '  AND l.macrolanguage_code = :MacroLang ' +
        '  AND v.active_status = 1 ' +
        '  AND l.active_status = 1 ' +
        'ORDER BY ' +
        '  CASE WHEN l.country_code = :CountryCode THEN 0 ELSE 1 END ASC, ' +
        '  v.preferred DESC ' +
        'LIMIT 1';

      ParamByName('TaxonId').AsInteger := TaxonId;
      ParamByName('MacroLang').AsString := LowerCase(MacroLang);
      ParamByName('CountryCode').AsString := UpperCase(CountryCode);

      try
        Open;

        if not EOF then
          S := FieldByName('vernacular_name').AsString;
      finally
        Close;
      end;

      if S <> '' then
      begin
        if CountryCode <> '' then
          txtVernacularLang.Caption := UpperCase(Format('%s-%s', [MacroLang, CountryCode]))
        else
          txtVernacularLang.Caption := UpperCase(MacroLang);
        txtVernacularLang.Visible := True;
        txtVernacularNames.Caption := S;
        txtVernacularNames.Visible := True;
        txtVernacularDiv.Visible := True;
      end;
    end;

    txtEnglishNames.Caption := GetVernacularNameFallback(TaxonId);

  finally
    FreeAndNil(Qry);
  end;
end;

function TfrmTaxa.GetVernacularNameFallback(const TaxonId: Integer): string;
var
  Qry: TSQLQuery;
begin
  Result := '';
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    SQL.Text :=
      'SELECT vernacular_name FROM zoo_vernacular ' +
      'WHERE taxon_id = :TaxonId AND active_status = 1 ' +
      'ORDER BY preferred DESC LIMIT 1';

    ParamByName('TaxonId').AsInteger := TaxonId;

    try
      Open;
      if not EOF then
        Result := FieldByName('vernacular_name').AsString
      else
        Result := ''; // Ou você pode buscar o nome científico da tabela zoo_taxa
    finally
      Close;
    end;

  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.gridTaxaMouseWheel
  (Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);

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

procedure TfrmTaxa.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  aRank: TZooRank;
begin
  if (Column.FieldName = 'scientific_name') and (Assigned(Column.Field)) then
  begin
    aRank := GetRankType(TDBGrid(Sender).Columns[1].Field.AsInteger);
    if aRank >= trSuperGenus then
      TDBGrid(Sender).Canvas.Font.Style := [fsItalic]
    else
      TDBGrid(Sender).Canvas.Font.Style := [fsBold];

    if not (gdSelected in AState) then
    begin
      if aRank = trSpecies then
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Font.Color := $00FFCD60
        else
          TDBGrid(Sender).Canvas.Font.Color := clNavy;

      if aRank in [trMonotypicGroup, trPolitypicGroup, trForm, trSpuh, trHybrid, trIntergrade, trDomestic, trSlash] then
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Font.Color := $005FCB6C
        else
          TDBGrid(Sender).Canvas.Font.Color := clGreen;

      // Greyed text if not valid taxon
      //if (TDBGrid(Sender).Columns[1].Field.AsInteger > 0) then
      //  if IsDarkModeEnabled then
      //    TDBGrid(Sender).Canvas.Font.Color := $009F9F9F
      //  else
      //    TDBGrid(Sender).Canvas.Font.Color := $00646464;
    end;
  end;
end;

procedure TfrmTaxa.LoadMonthlyChart;
var
  Qry: TSQLQuery;
  i: Integer;
  MonthData: specialize TFPGMap<Integer, TChartCounts>;
  FMonth: Integer;
  RecordTypeIndex: Integer;
  RecordTypeMap: specialize TFPGMap<String, Integer>;
  FCounts: TChartCounts;
begin
  DMT.lcsMonthly.Clear;

  // Initialize the maps
  MonthData := specialize TFPGMap<Integer, TChartCounts>.Create;
  RecordTypeMap := specialize TFPGMap<String, Integer>.Create;

  // Map record_type to indexes
  RecordTypeMap.Add(rsTitleCaptures, 0);
  RecordTypeMap.Add(rsTitleSightings, 1);
  RecordTypeMap.Add(rsTitleNests, 2);
  RecordTypeMap.Add(rsTitleEggs, 3);
  RecordTypeMap.Add(rsCaptionFeathers, 4);
  RecordTypeMap.Add(rsTitleSpecimens, 5);

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    // Load monthly counts
    Clear;
    Add('SELECT');
    Add('  CAST(strftime(''%m'', capture_date) AS INTEGER) AS mes,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleCaptures + ''' AS record_type');
    Add('FROM captures');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY mes');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%m'', sighting_date) AS INTEGER) AS mes,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleSightings + ''' AS record_type');
    Add('FROM sightings');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY mes');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%m'', found_date) AS INTEGER) AS mes,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleNests + ''' AS record_type');
    Add('FROM nests');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY mes');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%m'', measure_date) AS INTEGER) AS mes,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleEggs + ''' AS record_type');
    Add('FROM eggs');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (measure_date NOTNULL)');
    Add('GROUP BY mes');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%m'', sample_date) AS INTEGER) AS mes,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsCaptionFeathers + ''' AS record_type');
    Add('FROM feathers');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY mes');
    Add('UNION ALL');

    Add('SELECT');
    Add('  collection_month AS mes,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleSpecimens + ''' AS record_type');
    Add('FROM specimens');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (mes > 0)');
    Add('GROUP BY mes');

    Add('ORDER BY mes, record_type');

    ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    Open;
    if RecordCount > 0 then
    begin
      First;
      while not EOF do
      begin
        FMonth := FieldByName('mes').AsInteger;
        RecordTypeIndex := RecordTypeMap[FieldByName('record_type').AsString];

        // Verificar se o ano já existe no YearData
        if not MonthData.TryGetData(FMonth, FCounts) then
        begin
          // Criar uma nova estrutura para o ano
          FCounts.XValue := FMonth;
          SetLength(FCounts.YValues, RecordTypeMap.Count);
          FillChar(FCounts.YValues[0], Length(FCounts.YValues) * SizeOf(Double), 0);
        end;

        // Atualizar a contagem para o tipo de registro correspondente
        FCounts.YValues[RecordTypeIndex] := FieldByName('tally').AsFloat;

        // Salvar de volta no dicionário
        MonthData.AddOrSetData(FMonth, FCounts);

        Next;
      end;

      // Iterar pelos anos e adicionar os dados
      for i := 0 to MonthData.Count - 1 do
      begin
        // Adicionar ao ChartSource
        DMT.lcsMonthly.AddXYList(MonthData.Keys[i], MonthData.Data[i].YValues);
      end;
    end;
    Close;

  finally
    FreeAndNil(Qry);
    RecordTypeMap.Free;
    MonthData.Free;
  end;
end;

procedure TfrmTaxa.LoadNestFateChart;
var
  Qry: TSQLQuery;
  L, S, U: Integer;
begin
  chartNestFate.DisableRedrawing;
  try
    DMT.lcsNestFate.Clear;
    L := 0;
    S := 0;
    U := 0;

    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      SQLConnection := DMM.sqlCon;
      SQLTransaction := DMM.sqlTrans;

      // Load monthly counts
      Clear;
      Add('SELECT nest_fate, count(*) AS counter FROM nests');
      Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
      Add('GROUP BY nest_fate');
      Add('ORDER BY nest_fate');

      ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
      Open;
      if RecordCount > 0 then
      begin
        First;
        while not EOF do
        begin
          case FieldByName('nest_fate').AsString of
            'L': L := FieldByName('counter').AsInteger;
            'S': S := FieldByName('counter').AsInteger;
            'U': U := FieldByName('counter').AsInteger;
          end;

          Next;
        end;

        // Colors are set per item, so zero-value fates can be skipped without shifting colors
        if L > 0 then
          DMT.lcsNestFate.Add(0, L, rsNestLost, ChartSeriesColor(3));
        if S > 0 then
          DMT.lcsNestFate.Add(0, S, rsNestSuccess, ChartSeriesColor(7));
        if U > 0 then
          DMT.lcsNestFate.Add(0, U, rsNestUnknown, ActiveTheme.ChartOthers);
      end;
      Close;

    finally
      FreeAndNil(Qry);
    end;

  finally
    chartNestFate.EnableRedrawing;
    chartNestFate.Refresh;
  end;
end;

procedure TfrmTaxa.LoadRecordsMap;
var
  Qry: TSQLQuery;
  poi: TGpsPoint;
  rp: TRealPoint;
  category: Integer;
begin
  // Clear GPS points in the map
  mapView.GPSItems.Clear(0);
  mapView.GPSItems.Clear(1);
  mapView.GPSItems.Clear(2);
  mapView.GPSItems.Clear(3);
  mapView.Refresh;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    // Load coordinates from database
    Add('SELECT DISTINCT full_name, longitude, latitude, ''C'' AS record_type FROM captures');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');
    Add('UNION');
    Add('SELECT DISTINCT full_name, longitude, latitude, ''S'' AS record_type FROM sightings');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');
    Add('UNION');
    Add('SELECT DISTINCT full_name, longitude, latitude, ''N'' AS record_type FROM nests');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');
    Add('UNION');
    Add('SELECT DISTINCT full_name, longitude, latitude, ''E'' AS record_type FROM specimens');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');

    ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    Open;
    if RecordCount > 0 then
    begin
      First;
      while not EOF do
      begin
        // Record type will define the marker color
        case FieldByName('record_type').AsString of
          'C': category := 0;
          'S': category := 1;
          'N': category := 2;
          'E': category := 3;
        end;

        // Prepare and add a GPS point to the map
        rp.Lon := FieldByName('longitude').AsFloat;
        rp.Lat := FieldByName('latitude').AsFloat;
        if (not (rp.Lon = 0) and not (rp.Lat = 0)) then
        begin
          poi := TGPSPointOfInterest.CreateFrom(rp);
          case category of
            0: poi.Name := rsCaptionCapture + ': ' + FieldByName('full_name').AsString;
            1: poi.Name := rsCaptionSighting + ': ' + FieldByName('full_name').AsString;
            2: poi.Name := rsCaptionNest + ': ' + FieldByName('full_name').AsString;
            3: poi.Name := rsCaptionSpecimen + ': ' + FieldByName('full_name').AsString;
          end;

          mapView.GPSItems.Add(poi, category);
        end;

        Next;
      end;

      // Update zoom
      if (mapView.GPSItems.Count > 0) then
      begin
        mapView.ZoomOnArea(mapView.GPSItems.BoundingBox);
        if mapView.Zoom > 14 then
          mapView.Zoom := 14
        else
          mapView.Zoom := mapView.Zoom - 1;
      end;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.LoadYearlyChart;
var
  Qry: TSQLQuery;
  YearData: specialize TFPGMap<Integer, TChartCounts>;
  FYear: Integer;
  RecordTypeIndex, i: Integer;
  RecordTypeMap: specialize TFPGMap<String, Integer>;
  FCounts: TChartCounts;
  Zeros: TChartCounts;
  MinYear, MaxYear, y: Integer;
const
  MinSpan = 5;
begin
  DMT.lcsYearly.Clear;

  // Initialize the maps
  YearData := specialize TFPGMap<Integer, TChartCounts>.Create;
  YearData.Sorted := True;
  RecordTypeMap := specialize TFPGMap<String, Integer>.Create;

  // Map record_type to indexes
  RecordTypeMap.Add(rsTitleCaptures, 0);
  RecordTypeMap.Add(rsTitleSightings, 1);
  RecordTypeMap.Add(rsTitleNests, 2);
  RecordTypeMap.Add(rsTitleEggs, 3);
  RecordTypeMap.Add(rsCaptionFeathers, 4);
  RecordTypeMap.Add(rsTitleSpecimens, 5);

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    // Load yearly counts
    Clear;
    Add('SELECT');
    Add('  CAST(strftime(''%Y'', capture_date) AS INTEGER) AS ano,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleCaptures + ''' AS record_type');
    Add('FROM captures');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY ano');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%Y'', sighting_date) AS INTEGER) AS ano,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleSightings + ''' AS record_type');
    Add('FROM sightings');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY ano');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%Y'', found_date) AS INTEGER) AS ano,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleNests + ''' AS record_type');
    Add('FROM nests');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY ano');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%Y'', measure_date) AS INTEGER) AS ano,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleEggs + ''' AS record_type');
    Add('FROM eggs');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (measure_date NOTNULL)');
    Add('GROUP BY ano');
    Add('UNION ALL');

    Add('SELECT');
    Add('  CAST(strftime(''%Y'', sample_date) AS INTEGER) AS ano,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsCaptionFeathers + ''' AS record_type');
    Add('FROM feathers');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1)');
    Add('GROUP BY ano');
    Add('UNION ALL');

    Add('SELECT');
    Add('  collection_year AS ano,');
    Add('  COUNT(*) AS tally,');
    Add('  ''' + rsTitleSpecimens + ''' AS record_type');
    Add('FROM specimens');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) and (ano > 0)');
    Add('GROUP BY ano');

    Add('ORDER BY ano, record_type');

    ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    Open;
    if RecordCount > 0 then
    begin
      First;
      while not EOF do
      begin
        FYear := FieldByName('ano').AsInteger;
        RecordTypeIndex := RecordTypeMap[FieldByName('record_type').AsString];

        // Verificar se o ano já existe no YearData
        if not YearData.TryGetData(FYear, FCounts) then
        begin
          // Criar uma nova estrutura para o ano
          FCounts.XValue := FYear;
          SetLength(FCounts.YValues, RecordTypeMap.Count);
          FillChar(FCounts.YValues[0], Length(FCounts.YValues) * SizeOf(Double), 0);
        end;

        // Atualizar a contagem para o tipo de registro correspondente
        FCounts.YValues[RecordTypeIndex] := FieldByName('tally').AsFloat;

        // Salvar de volta no dicionário
        YearData.AddOrSetData(FYear, FCounts);

        Next;
      end;

      // Iterar pelos anos e adicionar os dados
      for i := 0 to YearData.Count - 1 do
      begin
        // Adicionar ao ChartSource
        DMT.lcsYearly.AddXYList(YearData.Keys[i], YearData.Data[i].YValues);
      end;
    end;
    Close;

    if YearData.Count > 0 then
    begin
      // Intervalo real dos dados
      MinYear := YearData.Keys[0];
      MaxYear := YearData.Keys[YearData.Count - 1];

      // Garante um número mínimo de "slots" no eixo
      if (MaxYear - MinYear + 1) < MinSpan then
        MinYear := MaxYear - MinSpan + 1;

      // Adiciona TODOS os anos do intervalo, preenchendo com zero os que não têm dados
      for y := MinYear to MaxYear do
      begin
        if YearData.TryGetData(y, FCounts) then
          DMT.lcsYearly.AddXYList(y, FCounts.YValues)
        else
        begin
          SetLength(Zeros.YValues, RecordTypeMap.Count);
          FillChar(Zeros.YValues[0], Length(Zeros.YValues) * SizeOf(Double), 0);
          DMT.lcsYearly.AddXYList(y, Zeros.YValues);
        end;
      end;

      chartYear.Extent.UseXMin := True;
      chartYear.Extent.XMin := MinYear - 0.5;
      chartYear.Extent.UseXMax := True;
      chartYear.Extent.XMax := MaxYear + 0.5;
    end
    else
    begin
      // Sem dados: eixo vazio, mas com extensão fixa para não quebrar o layout
      chartYear.Extent.UseXMin := False;
      chartYear.Extent.UseXMax := False;
    end;

  finally
    FreeAndNil(Qry);
    RecordTypeMap.Free;
    YearData.Free;
  end;
end;

procedure TfrmTaxa.mapViewDrawGpsPoint(Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
const
  R = 8;
var
  P: TPoint;
  ext: TSize;
begin
  // Screen coordinates of the GPS point
  P := TMapView(Sender).LatLonToScreen(APoint.RealPoint);

  // Draw the GPS point with MapMarker bitmap
  //img := TBitmap.Create;
  //try
  //  img.TransparentColor := clBlack;
  //  img.Transparent := True;
  //  img.Width := 16;
  //  img.Height := 16;
  //  DMM.iMaps.DrawForPPI(img.Canvas, 0, 0, APoint.IdOwner, 16, 96, 1);
  //  ADrawer.DrawBitmap(P.X - mapGeo.POIImagesWidth div 2, P.Y - mapGeo.POIImagesWidth, img, True);
  //finally
  //  img.Free;
  //end;
  //end
  //else
  //begin
    // Draw the GPS point as a circle
    // >> Sighting
    if APoint.IdOwner = 1 then
    begin
      ADrawer.BrushColor := clCatGencianaBGLight;
      ADrawer.PenColor := clCatGencianaFGLight;
    end
    else
    // >> Nest
    if APoint.IdOwner = 2 then
    begin
      ADrawer.BrushColor := clCatIpeAmareloBGLight;
      ADrawer.PenColor := clCatIpeAmareloFGLight;
    end
    else
    // >> Specimen
    if APoint.IdOwner = 3 then
    begin
      ADrawer.BrushColor := clCatQuaresmeiraBGLight;
      ADrawer.PenColor := clCatQuaresmeiraFGLight;
    end
    else
    // >> Capture
    begin
      ADrawer.BrushColor := clCatMandacaruBGLight;
      ADrawer.PenColor := clCatMandacaruFGLight;
    end;
    ADrawer.BrushStyle := bsSolid;
    ADrawer.PenWidth := 2;
    ADrawer.Ellipse(P.X - R, P.Y - R, P.X + R, P.Y + R);
    P.Y := P.Y + R;
  //end;

  // Draw the caption of the GPS point
  ext := ADrawer.TextExtent(APoint.Name);
  ADrawer.BrushColor := clWhite;
  ADrawer.BrushStyle := bsClear;
  //ADrawer.TextOut(P.X - ext.CX div 2, P.Y + 5, APoint.Name);
end;

procedure TfrmTaxa.mvpMarkerHintPluginCreateHintWindow(AMapView: TMapView; out AHintWindow: THintWindow);
begin
  AHintWindow := THintWindow.Create(AMapView);
end;

procedure TfrmTaxa.navTabsTabClick(Sender: TObject);
begin
  nbContent.PageIndex := navTabs.TabIndex;
end;

function TfrmTaxa.OccurrInBrazil(aTaxonId: Integer): Boolean;
var
  BrazilId: Integer;
  Qry: TSQLQuery;
begin
  Result := False;

  BrazilId := GetCountryFromCode('BR');
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Add('SELECT taxon_country_id FROM zoo_countries');
    Add('WHERE taxon_id = :taxon_id AND country_id = :country_id AND active_status = 1');
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('country_id').AsInteger := BrazilId;
    Open;
    First;
    if not EOF then
      Result := True;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.pCapturesCountIndicatorClick(Sender: TObject);
begin
  if txtCapturesCount.Enabled then
  begin
    frmMain.actOpenCapturesExecute(nil);
    frmMain.eSearch.Text := dsLink.DataSet.FieldByName('scientific_name').AsString;
  end;
end;

procedure TfrmTaxa.pEggsCountIndicatorClick(Sender: TObject);
begin
  if txtEggsCount.Enabled then
  begin
    frmMain.actOpenEggsExecute(nil);
    frmMain.eSearch.Text := dsLink.DataSet.FieldByName('scientific_name').AsString;
  end;
end;

procedure TfrmTaxa.pIndividualsCountIndicatorClick(Sender: TObject);
begin
  if txtIndividualsCount.Enabled then
  begin
    frmMain.actOpenIndividualsExecute(nil);
    frmMain.eSearch.Text := dsLink.DataSet.FieldByName('scientific_name').AsString;
  end;
end;

procedure TfrmTaxa.pmfShowRecordedTaxaClick(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;
end;

procedure TfrmTaxa.pNestsCountIndicatorClick(Sender: TObject);
begin
  if txtNestsCount.Enabled then
  begin
    frmMain.actOpenNestsExecute(nil);
    frmMain.eSearch.Text := dsLink.DataSet.FieldByName('scientific_name').AsString;
  end;
end;

procedure TfrmTaxa.pSightingsCountIndicatorClick(Sender: TObject);
begin
  if txtSightingsCount.Enabled then
  begin
    frmMain.actOpenSightingsExecute(nil);
    frmMain.eSearch.Text := dsLink.DataSet.FieldByName('scientific_name').AsString;
  end;
end;

procedure TfrmTaxa.pSpecimensCountIndicatorClick(Sender: TObject);
begin
  if txtSpecimensCount.Enabled then
  begin
    frmMain.actOpenSpecimensExecute(nil);
    frmMain.eSearch.Text := dsLink.DataSet.FieldByName('scientific_name').AsString;
  end;
end;

procedure TfrmTaxa.SaveChart(const AChart: TChart);
begin
  if SaveChartDlg.Execute then
  begin
    case SaveChartDlg.FilterIndex of
      0:
      begin
        SaveChartDlg.DefaultExt := '.jpg';
        AChart.SaveToFile(TJPEGImage, SaveChartDlg.FileName);
      end;
      1:
      begin
        SaveChartDlg.DefaultExt := '.png';
        AChart.SaveToFile(TPortableNetworkGraphic, SaveChartDlg.FileName);
      end;
      2:
      begin
        SaveChartDlg.DefaultExt := '.svg';
        AChart.SaveToSVGFile(SaveChartDlg.FileName);
      end;
      3:
      begin
        SaveChartDlg.DefaultExt := '.bmp';
        AChart.SaveToBitmapFile(SaveChartDlg.FileName);
      end;
    end;
  end;
end;

procedure TfrmTaxa.sbClearSearchClick(Sender: TObject);
begin
  eSearch.Clear;
  if eSearch.CanSetFocus then
    eSearch.SetFocus;
end;

procedure TfrmTaxa.sbCopyNestFateChartClick(Sender: TObject);
begin
  chartNestFate.CopyToClipboardBitmap;
end;

procedure TfrmTaxa.sbCopyRecordsYearChartClick(Sender: TObject);
begin
  chartYear.CopyToClipboardBitmap;
end;

procedure TfrmTaxa.sbCopySeasonalityChartClick(Sender: TObject);
begin
  chartSeasonality.CopyToClipboardBitmap;
end;

procedure TfrmTaxa.sbEmptyClearAllClick(Sender: TObject);
begin
  if sbClearSearch.Visible then
    sbClearSearchClick(nil);
end;

procedure TfrmTaxa.sbEmptyGotoSearchClick(Sender: TObject);
begin
  if eSearch.CanSetFocus then
    eSearch.SetFocus;
end;

procedure TfrmTaxa.sbOptionsSearchClick(Sender: TObject);
begin
  with TColorSpeedButton(Sender).ClientToScreen(point(0, TColorSpeedButton(Sender).Height + 1)) do
    pmOptions.Popup(X, Y);
end;

procedure TfrmTaxa.sbPrintClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmPrint.Popup(X, Y);
end;

procedure TfrmTaxa.sbSaveNestFateChartClick(Sender: TObject);
begin
  SaveChart(chartNestFate);
end;

procedure TfrmTaxa.sbSaveRecordsYearChartClick(Sender: TObject);
begin
  SaveChart(chartYear);
end;

procedure TfrmTaxa.sbSaveSeasonalityChartClick(Sender: TObject);
begin
  SaveChart(chartSeasonality);
end;

procedure TfrmTaxa.sbShareRecordsClick(Sender: TObject);
begin
  ExportDlg(dsLink.DataSet);
end;

procedure TfrmTaxa.scrollDataMouseWheel
  (Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);

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
  else
  if WheelDelta > 0 then
    Direction := -1;

  with TScrollBox(Sender) do
  begin
    ScrollBy(0, Direction * GetNumScrollLines);
    Invalidate;
  end;
end;

function TfrmTaxa.Search(AValue: String): Boolean;
var
  i, g: Longint;
  Crit: TCriteriaType;
  subRecorded: String;
begin
  Result := False;
  subRecorded := EmptyStr;

  {$IFDEF DEBUG}
  LogDebug('Search value: ' + aValue);
  {$ENDIF}
  FSearch.TextFilters.Clear;
  FSearch.QuickFilters.Clear;
  FSearch.ScientificNameSearch := Trim(AValue);

  gridTaxa.BeginUpdate;
  try

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
      if ExecRegExpr('^\$.+$', aValue) then
      begin
        Crit := crStartLike;
        aValue := StringReplace(aValue, '$', '', [rfReplaceAll]);
      end;

      // ID
      if TryStrToInt(aValue, i) then
      begin
        g := FSearch.TextFilters.Add(TSearchGroup.Create);
        FSearch.TextFilters[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon (ID)', sdtInteger, crEqual,
          True, aValue));
      end
      else
      // Text
      begin
        g := FSearch.TextFilters.Add(TSearchGroup.Create);
        FSearch.TextFilters[g].Fields.Add(TSearchField.Create('scientific_name', 'Scientific name', sdtText, Crit,
          True, aValue));
        FSearch.TextFilters[g].Fields.Add(TSearchField.Create('ebird_code', 'eBird code', sdtText, Crit,
          True, aValue));
        FSearch.TextFilters[g].Fields.Add(TSearchField.Create('quick_code', 'Quick code', sdtText, Crit,
          True, aValue));
      end;
    end;

    if pmfShowRecordedTaxa.Checked then
    begin
      subRecorded := 'SELECT DISTINCT subi.taxon_id FROM individuals AS subi ' +
        'UNION ALL ' +
        'SELECT DISTINCT subc.taxon_id FROM captures AS subc ' +
        'UNION ALL ' +
        'SELECT DISTINCT subs.taxon_id FROM sightings AS subs ' +
        'UNION ALL ' +
        'SELECT DISTINCT subf.taxon_id FROM feathers AS subf ' +
        'UNION ALL ' +
        'SELECT DISTINCT subn.taxon_id FROM nests AS subn ' +
        'UNION ALL ' +
        'SELECT DISTINCT sube.taxon_id FROM eggs AS sube ' +
        'UNION ALL ' +
        'SELECT DISTINCT subsp.taxon_id FROM specimens AS subsp';

      g := FSearch.TextFilters.Add(TSearchGroup.Create);
      FSearch.TextFilters[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon ID', sdtInteger, crIn,
        True, subRecorded));
    end;

    //GetFilters;

  finally
    gridTaxa.EndUpdate;
  end;

  //UpdateButtons(dsLink.DataSet);

  Result := FSearch.RunSearch > 0;
end;

procedure TfrmTaxa.SetSearchString(aValue: String);
begin
  if not CanToggle then
    Exit;

  if FSearchString <> OldSearchString then
    OldSearchString := FSearchString;
  FSearchString := aValue;

  if FSearchString = EmptyStr then
  begin
    dsLink.DataSet.Close;
    SetZooTaxaSQL(DMG.qTaxa.SQL, fvReset);
    dsLink.DataSet.Open;
  end
  else
    Search(FSearchString);
end;

procedure TfrmTaxa.SplitRightMoved(Sender: TObject);
begin
  pSearch.Width := gridTaxa.Width - pSearch.BorderSpacing.Left - pSearch.BorderSpacing.Right;
end;

procedure TfrmTaxa.TimerDataTimer(Sender: TObject);
var
  aRank: TZooRank;
  sBreadcrumb: String;
  Lang: String;
  N, NSightings, NIndividuals, NCaptures, NSpecimens, NNests, NEggs: Integer;
begin
  TimerData.Enabled := False;

  pHeader.DisableAlign;
  try
    aRank := GetRankType(DMG.qTaxa.FieldByName('rank_id').AsInteger);
    if aRank >= trSuperGenus then
      txtScientificName.Font.Style := [fsItalic]
    else
      txtScientificName.Font.Style := [fsBold];

    case DMG.qTaxa.FieldByName('iucn_status').AsString of
      'LC':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.System.SuccessBG;
        chipConservationStatus.Border.Color := ActiveTheme.System.SuccessFG;
        txtConservationStatus.Font.Color := ActiveTheme.System.SuccessSolid;
        txtConservationStatus.Caption := 'LC - ' + rsLeastConcern;
      end;
      'NT':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.System.CautionBG;
        chipConservationStatus.Border.Color := ActiveTheme.System.CautionFG;
        txtConservationStatus.Font.Color := ActiveTheme.System.CautionSolid;
        txtConservationStatus.Caption := 'NT - ' + rsNearThreatened;
      end;
      'VU':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.System.MediumBG;
        chipConservationStatus.Border.Color := ActiveTheme.System.MediumFG;
        txtConservationStatus.Font.Color := ActiveTheme.System.MediumFG;
        txtConservationStatus.Caption := 'VU - ' + rsVulnerable;
      end;
      'EN':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.Categorical.Pitanga.Background;
        chipConservationStatus.Border.Color := ActiveTheme.Categorical.Pitanga.Foreground;
        txtConservationStatus.Font.Color := ActiveTheme.Categorical.Pitanga.Foreground;
        txtConservationStatus.Caption := 'EN - ' + rsEndangered;
      end;
      'CR':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.System.CriticalBG;
        chipConservationStatus.Border.Color := ActiveTheme.System.CriticalFG;
        txtConservationStatus.Font.Color := ActiveTheme.System.CriticalSolid;
        txtConservationStatus.Caption := 'CR - ' + rsCriticallyEndangered;
      end;
      'EW':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.System.AlternativeBG;
        chipConservationStatus.Border.Color := ActiveTheme.System.AlternativeFG;
        txtConservationStatus.Font.Color := ActiveTheme.System.AlternativeSolid;
        txtConservationStatus.Caption := 'EW - ' + rsExtinctInTheWild;
      end;
      'EX':
      begin
        chipConservationStatus.Background.Color := clBlack;
        chipConservationStatus.Border.Color := clWhite;
        txtConservationStatus.Font.Color := clWhite;
        txtConservationStatus.Caption := 'EX - ' + rsExtinct;
      end;
      'DD':
      begin
        chipConservationStatus.Background.Color := ActiveTheme.Categorical.Outros.Background;
        chipConservationStatus.Border.Color := ActiveTheme.Categorical.Outros.Foreground;
        txtConservationStatus.Font.Color := ActiveTheme.Categorical.Outros.Foreground;
        txtConservationStatus.Caption := 'DD - ' + rsDataDeficient;
      end
    else
      chipConservationStatus.Background.Color := ActiveTheme.Categorical.Outros.Background;
      chipConservationStatus.Border.Color := ActiveTheme.Categorical.Outros.Foreground;
      txtConservationStatus.Font.Color := ActiveTheme.Categorical.Outros.Foreground;
      txtConservationStatus.Caption := 'NE - ' + rsNotEvaluated;
    end;
    chipConservationStatus.ParentBackground := True;

    GetVernacular(dsLink.DataSet.FieldByName('taxon_id').AsInteger);

    sBreadcrumb := '';
    if dsLink.DataSet.FieldByName('order_id').AsInteger > 0 then
    begin
      sBreadcrumb := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, dsLink.DataSet.FieldByName('order_id').AsInteger);
      if (dsLink.DataSet.FieldByName('family_id').AsInteger > 0) and (aRank > trFamily) then
      begin
        sBreadcrumb := sBreadcrumb + ' › ' + GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, dsLink.DataSet.FieldByName('family_id').AsInteger);
        if (dsLink.DataSet.FieldByName('genus_id').AsInteger > 0) and (aRank > trGenus) then
        begin
          sBreadcrumb := sBreadcrumb + ' › ' + GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, dsLink.DataSet.FieldByName('genus_id').AsInteger);
          if (dsLink.DataSet.FieldByName('species_id').AsInteger > 0) and (aRank > trSpecies) then
          begin
            sBreadcrumb := sBreadcrumb + ' › ' + GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, dsLink.DataSet.FieldByName('species_id').AsInteger);
            if (dsLink.DataSet.FieldByName('subspecies_group_id').AsInteger > 0) and (aRank > trSubspecies) then
            begin
              sBreadcrumb := sBreadcrumb + ' › ' + GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, dsLink.DataSet.FieldByName('subspecies_group_id').AsInteger);
            end;
          end;
        end;
      end;
    end;
    txtHierarchy.Caption := sBreadcrumb;
  finally
    pHeader.EnableAlign;
  end;

  if dsLink.DataSet.RecordCount > 0 then
  begin
    DMG.qVernacularTaxa.ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    DMG.qSynonymTaxa.ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    DMG.qChildTaxa.ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    dsVernacular.DataSet.Refresh;
    dsSynonyms.DataSet.Refresh;
    dsChilds.DataSet.Refresh;
  end
  else
  begin
    DMG.qVernacularTaxa.ParamByName('taxon_id').AsInteger := 0;
    DMG.qSynonymTaxa.ParamByName('taxon_id').AsInteger := 0;
    DMG.qChildTaxa.ParamByName('taxon_id').AsInteger := 0;
    dsVernacular.DataSet.Refresh;
    dsSynonyms.DataSet.Refresh;
    dsChilds.DataSet.Refresh;
  end;

  pSummaryIndicators.DisableAlign;
  try
    GetSightingsCount;
    GetIndividualsCount;
    GetCapturesCount;
    GetSpecimensCount;
    GetNestsCount;
    GetEggsCount;

    NSightings := StrToIntDef(txtSightingsCount.Caption, 0);
    NIndividuals := StrToIntDef(txtIndividualsCount.Caption, 0);
    NCaptures := StrToIntDef(txtCapturesCount.Caption, 0);
    NSpecimens := StrToIntDef(txtSpecimensCount.Caption, 0);
    NNests := StrToIntDef(txtNestsCount.Caption, 0);
    NEggs := StrToIntDef(txtEggsCount.Caption, 0);
    N := NSightings + NIndividuals + NCaptures + NSpecimens + NNests + NEggs;
    txtTotalRecords.Caption := IntToStr(N);
    txtTotalNests.Caption := IntToStr(NNests);
  finally
    pSummaryIndicators.EnableAlign;
  end;

  pSynonyms.DisableAlign;
  try
    GetSynonyms;
    if aRank >= trSuperGenus then
      txtSynonyms.Font.Style := [fsItalic]
    else
      txtSynonyms.Font.Style := [];
  finally
    pSynonyms.EnableAlign;
  end;

  pExternalLinks.DisableAlign;
  try
    chipWikiaves.Visible := OccurrInBrazil(dsLink.DataSet.FieldByName('taxon_id').AsInteger);
    chipBirdsOfTheWorld.Visible := dsLink.DataSet.FieldByName('ebird_code').AsString <> EmptyStr;
  finally
    pExternalLinks.EnableAlign;
  end;

  LoadRecordsMap;

  LoadMonthlyChart;
  LoadYearlyChart;

  GetNestProductivity;
  LoadNestFateChart;

  UpdateButtons;
end;

procedure TfrmTaxa.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  SetSearchString(eSearch.Text);
end;

procedure TfrmTaxa.txtValidNameMouseEnter(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := TLabel(Sender).Font.Style + [fsUnderline]
  else
  if Sender is TDBText then
    TDBText(Sender).Font.Style := TDBText(Sender).Font.Style + [fsUnderline];
end;

procedure TfrmTaxa.txtValidNameMouseLeave(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := TLabel(Sender).Font.Style - [fsUnderline]
  else
  if Sender is TDBText then
    TDBText(Sender).Font.Style := TDBText(Sender).Font.Style - [fsUnderline];
end;

procedure TfrmTaxa.UpdateButtons;
begin
  sbShareRecords.Enabled := (ActiveUser.HasPermission(PERM_TAXA_EXPORT)) and not (dsLink.DataSet.IsEmpty);
  sbPrint.Enabled := (ActiveUser.HasPermission(PERM_TAXA_PRINT)) and not (dsLink.DataSet.IsEmpty);
end;

end.
