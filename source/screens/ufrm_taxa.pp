unit ufrm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  BCPanel, Buttons, Classes, ComCtrls, DB, fgl, mvDE_BGRA, mvMapViewer, mvTypes,
  mvGpsObj, SQLDB, DBCtrls, DBGrids, httpprotocol, LCLIntf, LCLType, ExtCtrls, Menus,
  StdCtrls, ColorSpeedButton, SysUtils, Forms, RegExpr, Controls, Graphics,
  Dialogs, data_types, Grids, TADbSource, TAGraph, TAGUIConnectorBGRA,
  TASeries, TASources, Types, mvDrawingEngine;

type

  { TfrmTaxa }

  TfrmTaxa = class(TForm)
    txtNestProductivity: TLabel;
    lblMeanNestProductivity: TLabel;
    pNestProductivity: TBCPanel;
    chartNestFate: TChart;
    NestFateSeries: TPieSeries;
    pBreedingStats: TPanel;
    yearlySeries: TBarSeries;
    chartSeasonality: TChart;
    chartYear: TChart;
    monthlySeries: TBarSeries;
    chartBGRA: TChartGUIConnectorBGRA;
    ckCBRO: TDBCheckBox;
    ckClements: TDBCheckBox;
    ckIOC: TDBCheckBox;
    dsSynonyms: TDataSource;
    dsChilds: TDataSource;
    eSearch: TEdit;
    imgEmptyQuery: TImage;
    lblLinkCaptures: TLabel;
    lblLinkEggs: TLabel;
    lblLinkNests: TLabel;
    lblLinkSpecimens: TLabel;
    lblLinkSightings: TLabel;
    lblLinkIndividuals: TLabel;
    lblEmptyQuery: TLabel;
    mapView: TMapView;
    MvBGRA: TMvBGRADrawingEngine;
    pEmptyQuery: TPanel;
    pmfShowRecordedTaxa: TMenuItem;
    pmOptions: TPopupMenu;
    pTaxonData: TFlowPanel;
    gridChilds: TDBGrid;
    gridSynonyms: TDBGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    iconSearch: TImage;
    imgConservation: TImage;
    imgIUCN: TImageList;
    iSearch: TImageList;
    iSearchDark: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblValidName: TLabel;
    pConservation: TPanel;
    pConstrainedBox: TPanel;
    pContent: TPanel;
    pDetails: TPanel;
    pmPrint: TPopupMenu;
    pmPrintTaxa: TMenuItem;
    pmPrintTaxaHierarchical: TMenuItem;
    pmPrintTaxaRecorded: TMenuItem;
    pmPrintTaxaRecordedByLocality: TMenuItem;
    pScientificName: TPanel;
    pSideToolbar: TPanel;
    pTaxonInfo: TPanel;
    pTaxonomies: TPanel;
    pToolbar: TBCPanel;
    pSearch: TBCPanel;
    pTop: TPanel;
    pViews: TPanel;
    sbClearSearch: TColorSpeedButton;
    sbBirdsOfTheWorld: TSpeedButton;
    sbOptionsSearch: TColorSpeedButton;
    sbPrint: TSpeedButton;
    sbShareRecords: TSpeedButton;
    sbGoogleScholar: TSpeedButton;
    sbEbird: TSpeedButton;
    sbGoogleImages: TSpeedButton;
    sbWikiaves: TSpeedButton;
    sbIUCNRedList: TSpeedButton;
    sbGBIF: TSpeedButton;
    sbGoogleSearch: TSpeedButton;
    scrollData: TScrollBox;
    SplitRight: TSplitter;
    TimerData: TTimer;
    TimerFind: TTimer;
    dsLink: TDataSource;
    gridTaxa: TDBGrid;
    tvHierarchy: TTreeView;
    txtAuthorship: TDBText;
    txtDistribution: TDBText;
    txtDistributionIOC: TDBText;
    txtEnglishName: TDBText;
    txtExtinctionYear: TDBText;
    txtPortugueseName: TDBText;
    txtRank: TDBText;
    txtScientificName: TDBText;
    txtSpanishName: TDBText;
    txtValidName: TDBText;
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
    procedure lblLinkCapturesClick(Sender: TObject);
    procedure lblLinkEggsClick(Sender: TObject);
    procedure lblLinkIndividualsClick(Sender: TObject);
    procedure lblLinkNestsClick(Sender: TObject);
    procedure lblLinkSightingsClick(Sender: TObject);
    procedure lblLinkSpecimensClick(Sender: TObject);
    procedure mapViewDrawGpsPoint
      (Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure pmfShowRecordedTaxaClick(Sender: TObject);
    procedure sbBirdsOfTheWorldClick(Sender: TObject);
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbEbirdClick(Sender: TObject);
    procedure sbGBIFClick(Sender: TObject);
    procedure sbGoogleImagesClick(Sender: TObject);
    procedure sbGoogleScholarClick(Sender: TObject);
    procedure sbGoogleSearchClick(Sender: TObject);
    procedure sbIUCNRedListClick(Sender: TObject);
    procedure sbOptionsSearchClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbShareRecordsClick(Sender: TObject);
    procedure sbWikiavesClick(Sender: TObject);
    procedure scrollDataMouseWheel
      (Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure SplitRightMoved(Sender: TObject);
    procedure TimerDataTimer(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure txtValidNameClick(Sender: TObject);
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
    procedure LoadMonthlyChart;
    procedure LoadNestFateChart;
    procedure LoadRecordsMap;
    procedure LoadYearlyChart;
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
  data_search, data_getvalue,
  models_record_types, models_taxonomy,
  ufrm_main, udm_main, udm_grid, udm_taxa,
  uDarkStyleParams;

{$R *.lfm}

{ TfrmTaxa }

procedure TfrmTaxa.ApplyDarkMode;
begin
  pEmptyQuery.Color := Color;
  imgEmptyQuery.Images := iButtonsDark;

  pSearch.Background.Color := clCardBGDefaultDark;
  pSearch.Border.Color := clSolidBGSecondaryDark;
  pSearch.ParentBackground := True;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateHover.Color := clSolidBGSecondaryDark;
  sbClearSearch.StateActive.Color := clSolidBGTertiaryDark;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
  sbOptionsSearch.StateHover.Color := clSolidBGSecondaryDark;
  sbOptionsSearch.StateActive.Color := clSolidBGTertiaryDark;
  sbOptionsSearch.StateNormal.Color := pSearch.Background.Color;
  iconSearch.Images := iSearchDark;
  sbClearSearch.Images := iSearchDark;
  sbOptionsSearch.Images := iSearchDark;

  pToolbar.Background.Color := clCardBGDefaultDark;
  pToolbar.Border.Color := clCardBGSecondaryDark;
  sbShareRecords.Images := iButtonsDark;
  sbPrint.Images := iButtonsDark;

  sbGoogleSearch.Images := DMM.iWebDark;
  sbGoogleImages.Images := DMM.iWebDark;
  sbGoogleScholar.Images := DMM.iWebDark;
  sbEbird.Images := DMM.iWebDark;
  sbWikiaves.Images := DMM.iWebDark;
  sbIUCNRedList.Images := DMM.iWebDark;
  sbGBIF.Images := DMM.iWebDark;

  chartSeasonality.Title.Font.Color := clTextPrimaryDark;
  chartSeasonality.Legend.Font.Color := clTextPrimaryDark;
  chartSeasonality.BottomAxis.Title.LabelFont.Color := clTextPrimaryDark;
  chartSeasonality.BottomAxis.Marks.LabelFont.Color := clTextPrimaryDark;
  chartSeasonality.LeftAxis.Title.LabelFont.Color := clTextPrimaryDark;
  chartSeasonality.LeftAxis.Marks.LabelFont.Color := clTextPrimaryDark;

  chartYear.Title.Font.Color := clTextPrimaryDark;
  chartYear.Legend.Font.Color := clTextPrimaryDark;
  chartYear.BottomAxis.Title.LabelFont.Color := clTextPrimaryDark;
  chartYear.BottomAxis.Marks.LabelFont.Color := clTextPrimaryDark;
  chartYear.LeftAxis.Title.LabelFont.Color := clTextPrimaryDark;
  chartYear.LeftAxis.Marks.LabelFont.Color := clTextPrimaryDark;

  chartNestFate.Title.Font.Color := clTextPrimaryDark;
  chartNestFate.Legend.Font.Color := clTextPrimaryDark;
  NestFateSeries.Marks.LabelFont.Color := clTextPrimaryDark;

  pNestProductivity.Background.Color := clSolidBGSecondaryDark;
  pNestProductivity.Border.Color := clSystemSolidNeutralFGDark;
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
end;

procedure TfrmTaxa.eSearchEnter(Sender: TObject);
begin
  //if eSearch.Text = EmptyStr then
  //  pSearch.Width := ClientWidth div 4;
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := clSolidBGBaseDark;
    pSearch.Border.Color := clSolidBGTertiaryDark;
  end
  else
  begin
    pSearch.Background.Color := clWhite;
    pSearch.Border.Color := clAccentFillTertiaryLight;
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
    pSearch.Background.Color := clCardBGDefaultDark;
    pSearch.Border.Color := clSolidBGSecondaryDark;
  end
  else
  begin
    pSearch.Background.Color := $00FAFAFA;
    pSearch.Border.Color := clDefaultBorderLight;
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

  FSearch := TCustomSearch.Create(tbZooTaxa);
  FSearch.DataSet := DMG.qTaxa;

  SetZooTaxaSQL(DMG.qTaxa.SQL, fvReset);

  dsLink.DataSet.Open;
  dsSynonyms.DataSet.Open;
  dsChilds.DataSet.Open;

  SplitRightMoved(nil);

  pEmptyQuery.AnchorSideLeft.Side := asrRight;
  pEmptyQuery.AnchorSideLeft.Control := SplitRight;
  pEmptyQuery.AnchorSideRight.Side := asrRight;
  pEmptyQuery.AnchorSideRight.Control := pSideToolbar;
  pEmptyQuery.AnchorSideTop.Control := gridTaxa;
  pEmptyQuery.AnchorSideBottom.Side := asrBottom;
  pEmptyQuery.AnchorSideBottom.Control := gridTaxa;
  pEmptyQuery.Anchors := [akLeft, akRight, akTop, akBottom];
  pEmptyQuery.BringToFront;

  mapView.CachePath := IncludeTrailingPathDelimiter(ConcatPaths([AppDataDir, 'map-cache']));
  mapView.Active := True;

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

    lblLinkCaptures.Caption := Format(rsTitleCaptures + ' (%d)', [C]);
    lblLinkCaptures.Enabled := C > 0;
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

    lblLinkEggs.Caption := Format(rsTitleEggs + ' (%d)', [C]);
    lblLinkEggs.Enabled := C > 0;
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

    lblLinkIndividuals.Caption := Format(rsTitleIndividuals + ' (%d)', [C]);
    lblLinkIndividuals.Enabled := C > 0;
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

    lblLinkNests.Caption := Format(rsTitleNests + ' (%d)', [C]);
    lblLinkNests.Enabled := C > 0;
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

    lblLinkSightings.Caption := Format(rsTitleSightings + ' (%d)', [C]);
    lblLinkSightings.Enabled := C > 0;
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

    lblLinkSpecimens.Caption := Format(rsTitleSpecimens + ' (%d)', [C]);
    lblLinkSpecimens.Enabled := C > 0;
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
  if (Column.FieldName = 'full_name') and (Assigned(Column.Field)) then
  begin
    aRank := GetRankType(TDBGrid(Sender).Columns[2].Field.AsInteger);
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

      if (TDBGrid(Sender).Columns[1].Field.AsInteger > 0) then
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Font.Color := $009F9F9F
        else
          TDBGrid(Sender).Canvas.Font.Color := $00646464;
    end;
  end;
end;

procedure TfrmTaxa.lblLinkCapturesClick(Sender: TObject);
begin
  frmMain.actOpenCapturesExecute(nil);
  frmMain.eSearch.Text := dsLink.DataSet.FieldByName('full_name').AsString;
end;

procedure TfrmTaxa.lblLinkEggsClick(Sender: TObject);
begin
  frmMain.actOpenEggsExecute(nil);
  frmMain.eSearch.Text := dsLink.DataSet.FieldByName('full_name').AsString;
end;

procedure TfrmTaxa.lblLinkIndividualsClick(Sender: TObject);
begin
  frmMain.actOpenIndividualsExecute(nil);
  frmMain.eSearch.Text := dsLink.DataSet.FieldByName('full_name').AsString;
end;

procedure TfrmTaxa.lblLinkNestsClick(Sender: TObject);
begin
  frmMain.actOpenNestsExecute(nil);
  frmMain.eSearch.Text := dsLink.DataSet.FieldByName('full_name').AsString;
end;

procedure TfrmTaxa.lblLinkSightingsClick(Sender: TObject);
begin
  frmMain.actOpenSightingsExecute(nil);
  frmMain.eSearch.Text := dsLink.DataSet.FieldByName('full_name').AsString;
end;

procedure TfrmTaxa.lblLinkSpecimensClick(Sender: TObject);
begin
  frmMain.actOpenSpecimensExecute(nil);
  frmMain.eSearch.Text := dsLink.DataSet.FieldByName('full_name').AsString;
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

      // Adicionar ao ChartSource
      DMT.lcsNestFate.Add(0, L, rsNestLost, clRedBG1Light);
      DMT.lcsNestFate.Add(0, S, rsNestSuccess, clYellowBGLight);
      DMT.lcsNestFate.Add(0, U, rsNestUnknown, clSilver);
    end;
    Close;

  finally
    FreeAndNil(Qry);
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
    Add('SELECT DISTINCT longitude, latitude, ''C'' AS record_type FROM captures');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');
    Add('UNION');
    Add('SELECT DISTINCT longitude, latitude, ''S'' AS record_type FROM sightings');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');
    Add('UNION');
    Add('SELECT DISTINCT longitude, latitude, ''N'' AS record_type FROM nests');
    Add('WHERE (taxon_id = :taxon_id) AND (active_status = 1) AND (longitude NOTNULL) AND (latitude NOTNULL)');
    Add('UNION');
    Add('SELECT DISTINCT longitude, latitude, ''E'' AS record_type FROM specimens');
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
          poi := TGpsPoint.CreateFrom(rp);

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
begin
  DMT.lcsYearly.Clear;

  // Initialize the maps
  YearData := specialize TFPGMap<Integer, TChartCounts>.Create;
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
  P := TMapView(Sender).LonLatToScreen(APoint.RealPoint);

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
      ADrawer.BrushColor := clLightYellowChart; //clYellowFG4Dark;
      ADrawer.PenColor := clYellowChart; //clYellowBGLight;
    end
    else
    // >> Nest
    if APoint.IdOwner = 2 then
    begin
      ADrawer.BrushColor := clLightRedChart; //clRedFGDark;
      ADrawer.PenColor := clRedChart; //clRedBGLight;
    end
    else
    // >> Specimen
    if APoint.IdOwner = 3 then
    begin
      ADrawer.BrushColor := clLightVioletChart; //clVioletFG1Dark;
      ADrawer.PenColor := clVioletChart; //clVioletBG2Dark;
    end
    else
    // >> Capture
    begin
      ADrawer.BrushColor := clLightLimeGreenChart; //clGreenBGDark;
      ADrawer.PenColor := clLimeGreenChart; //clGreenFG2Light;
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
  ADrawer.TextOut(P.X - ext.CX div 2, P.Y + 5, APoint.Name);
end;

procedure TfrmTaxa.pmfShowRecordedTaxaClick(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;
end;

procedure TfrmTaxa.sbBirdsOfTheWorldClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('ebird_code').AsString);
  OpenUrl('https://birdsoftheworld.org/bow/species/' + FUrlSearch);
end;

procedure TfrmTaxa.sbClearSearchClick(Sender: TObject);
begin
  eSearch.Clear;
  if eSearch.CanSetFocus then
    eSearch.SetFocus;
end;

procedure TfrmTaxa.sbEbirdClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('ebird_code').AsString);
  OpenUrl('https://ebird.org/species/' + FUrlSearch);
end;

procedure TfrmTaxa.sbGBIFClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('full_name').AsString);
  OpenUrl('https://www.gbif.org/search?q=' + FUrlSearch);
end;

procedure TfrmTaxa.sbGoogleImagesClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('full_name').AsString);
  OpenUrl('https://www.google.com/search?tbm=isch&q="' + FUrlSearch + '"');
end;

procedure TfrmTaxa.sbGoogleScholarClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('full_name').AsString);
  OpenUrl('https://scholar.google.com/scholar?q="' + FUrlSearch + '"');
end;

procedure TfrmTaxa.sbGoogleSearchClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('full_name').AsString);
  OpenUrl('https://www.google.com/search?q="' + FUrlSearch + '"');
end;

procedure TfrmTaxa.sbIUCNRedListClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('full_name').AsString);
  OpenUrl('https://www.iucnredlist.org/search?query=' + FUrlSearch);
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

procedure TfrmTaxa.sbShareRecordsClick(Sender: TObject);
begin
  ExportDlg(dsLink.DataSet);
end;

procedure TfrmTaxa.sbWikiavesClick(Sender: TObject);
var
  FUrlSearch: String;
begin
  if GetRankType(dsLink.DataSet.FieldByName('rank_id').AsInteger) = trSpecies then
    FUrlSearch := HTTPEncode(RemoveDiacritics(dsLink.DataSet.FieldByName('portuguese_name').AsString))
  else
    FUrlSearch := HTTPEncode(dsLink.DataSet.FieldByName('full_name').AsString);
  OpenUrl('https://www.wikiaves.com.br/wiki/' + FUrlSearch);
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
  FSearch.Fields.Clear;
  FSearch.QuickFilters.Clear;

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

      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon ID', sdtInteger, crIn,
        False, subRecorded));
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
  nOrder, nFamily, nGenus, nSpecies, nGroup: TTreeNode;
  aRank: TZooRank;
begin
  TimerData.Enabled := False;

  aRank := GetRankType(DMG.qTaxa.FieldByName('rank_id').AsInteger);
  if aRank >= trSuperGenus then
    txtScientificName.Font.Style := [fsItalic]
  else
    txtScientificName.Font.Style := [fsBold];

  case DMG.qTaxa.FieldByName('iucn_status').AsString of
    'LC': imgConservation.ImageIndex := 1;
    'NT': imgConservation.ImageIndex := 2;
    'VU': imgConservation.ImageIndex := 3;
    'EN': imgConservation.ImageIndex := 4;
    'CR': imgConservation.ImageIndex := 5;
    'EW': imgConservation.ImageIndex := 6;
    'EX': imgConservation.ImageIndex := 7;
    'DD': imgConservation.ImageIndex := 8;
  else
    imgConservation.ImageIndex := 0;
  end;

  lblValidName.Visible := DMG.qTaxa.FieldByName('valid_id').AsInteger > 0;
  txtValidName.Visible := lblValidName.Visible;

  tvHierarchy.Items.Clear;
  if dsLink.DataSet.FieldByName('order_id').AsInteger > 0 then
  begin
    nOrder := tvHierarchy.Items.Add(nil, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('order_id').AsInteger));
    if dsLink.DataSet.FieldByName('family_id').AsInteger > 0 then
    begin
      nFamily := tvHierarchy.Items.AddChild(nOrder, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('family_id').AsInteger));
      if dsLink.DataSet.FieldByName('genus_id').AsInteger > 0 then
      begin
        nGenus := tvHierarchy.Items.AddChild(nFamily, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('genus_id').AsInteger));
        if dsLink.DataSet.FieldByName('species_id').AsInteger > 0 then
        begin
          nSpecies := tvHierarchy.Items.AddChild(nGenus, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('species_id').AsInteger));
          if dsLink.DataSet.FieldByName('subspecies_group_id').AsInteger > 0 then
          begin
            nGroup := tvHierarchy.Items.AddChild(nSpecies, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('subspecies_group_id').AsInteger));
          end;
        end;
      end;
    end;
    tvHierarchy.FullExpand;
  end;

  if dsLink.DataSet.RecordCount > 0 then
  begin
    DMG.qSynonymTaxa.ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    DMG.qChildTaxa.ParamByName('taxon_id').AsInteger := dsLink.DataSet.FieldByName('taxon_id').AsInteger;
    dsSynonyms.DataSet.Refresh;
    dsChilds.DataSet.Refresh;
  end
  else
  begin
    DMG.qSynonymTaxa.ParamByName('taxon_id').AsInteger := 0;
    DMG.qChildTaxa.ParamByName('taxon_id').AsInteger := 0;
    dsSynonyms.DataSet.Refresh;
    dsChilds.DataSet.Refresh;
  end;

  GetSightingsCount;
  GetIndividualsCount;
  GetCapturesCount;
  GetSpecimensCount;
  GetNestsCount;
  GetEggsCount;

  sbWikiaves.Visible := dsLink.DataSet.FieldByName('cbro_taxonomy').AsBoolean = True;
  sbBirdsOfTheWorld.Visible := dsLink.DataSet.FieldByName('ebird_code').AsString <> EmptyStr;

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

procedure TfrmTaxa.txtValidNameClick(Sender: TObject);
begin
  if (not DMG.qTaxa.FieldByName('valid_id').IsNull) then
    eSearch.Text := DMG.qTaxa.FieldByName('valid_id').AsString;
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
  sbShareRecords.Enabled := dsLink.DataSet.RecordCount > 0;
  sbPrint.Enabled := sbShareRecords.Enabled;
end;

end.

