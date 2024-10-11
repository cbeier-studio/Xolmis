{ Xolmis Home Dashboard form

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

unit ufrm_dashboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Menus, DB, SQLDB,
  DBGrids, ATLinkLabel, TAGraph, TASeries, TASources, TAGUIConnectorBGRA, BCPanel, DateUtils,
  BCButton, BCTypes, mvMapViewer, mvTypes, mvGpsObj, mvDrawingEngine, mvDE_BGRA, ImgList,
  uthread_dashboard;

type

  { TfrmDashboard }

  TfrmDashboard = class(TForm)
    barIndividualsMonth: TBarSeries;
    barSpeciesMonth: TBarSeries;
    ChartBGRA: TChartGUIConnectorBGRA;
    chartIndividuals: TChart;
    chartSpecies: TChart;
    icoShortcutConverter: TImage;
    icoShortcutNewSighting: TImage;
    lblTitleBandsBalance: TLabel;
    lblTitleBirthdays: TLabel;
    lblTitleMapSurveys: TLabel;
    lblTitleChartIndividuals: TLabel;
    lblTitleChartSpecies: TLabel;
    lblTitleLifers: TLabel;
    lblTitlePermitsExpiring: TLabel;
    lblTitleReviseNests: TLabel;
    lblTotalIndividuals: TLabel;
    lblTotalNests: TLabel;
    lblTotalSpecies: TLabel;
    lblTotalSamplings: TLabel;
    lcsSpeciesMonth: TListChartSource;
    lineTotalIndividuals: TShape;
    lineTotalNests: TShape;
    lineTotalSpecies: TShape;
    lineTotalSamplings: TShape;
    linkShowMoreLifers: TATLabelLink;
    mapSurveys: TMapView;
    lcsIndividualsMonth: TListChartSource;
    MvBGRADraw: TMvBGRADrawingEngine;
    pBandsBalance: TBCPanel;
    pBandsContent: TBCPanel;
    pBirthdays: TBCPanel;
    pChartIndividuals: TBCPanel;
    pChartSpecies: TBCPanel;
    pLifers: TBCPanel;
    pMapSurveys: TBCPanel;
    pNumbers: TBCPanel;
    pPermitsExpiring: TBCPanel;
    pReviseNests: TBCPanel;
    pTitleNotificationCenter: TBCPanel;
    pAppUpdateBtns: TBCPanel;
    pTotalIndividuals: TBCPanel;
    pTotalNests: TBCPanel;
    pTotalSpecies: TBCPanel;
    pTotalSamplings: TBCPanel;
    pNewSighting: TBCPanel;
    pShortcutConverter: TBCPanel;
    sbUpdateNow: TBCButton;
    scrollNotifications: TScrollBox;
    TimerReload: TTimer;
    txtAppUpdate: TLabel;
    lblTitleAppUpdate: TLabel;
    pAppUpdate: TBCPanel;
    pLoading: TBCPanel;
    pNotificationCenter: TBCPanel;
    pmRefresh: TMenuItem;
    pmDashboard: TPopupMenu;
    sbUpdateLater: TBCButton;
    TimerLoad: TTimer;
    txtTotalIndividuals: TLabel;
    txtTotalNests: TLabel;
    txtShortcutNewSighting: TLabel;
    txtShortcutConverter: TLabel;
    txtTotalSpecies: TLabel;
    txtTotalSamplings: TLabel;
    vIcons: TImageList;
    pFlow: TPanel;
    sBox: TScrollBox;
    vIconsDark: TImageList;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblTitleBandsBalanceClick(Sender: TObject);
    procedure pFlowChangeBounds(Sender: TObject);
    procedure pmRefreshClick(Sender: TObject);
    procedure pNewSightingClick(Sender: TObject);
    procedure pNewSightingMouseEnter(Sender: TObject);
    procedure pNewSightingMouseLeave(Sender: TObject);
    procedure pNumbersResize(Sender: TObject);
    procedure pShortcutConverterClick(Sender: TObject);
    procedure pShortcutConverterMouseEnter(Sender: TObject);
    procedure pShortcutConverterMouseLeave(Sender: TObject);
    procedure sBoxResize(Sender: TObject);
    procedure sbUpdateLaterClick(Sender: TObject);
    procedure TimerLoadTimer(Sender: TObject);
    procedure TimerReloadTimer(Sender: TObject);
  private
    FBandsLoader: TBandsLoaderThread;
    FBirthdaysLoader: TBirthdaysLoaderThread;
    FLifersLoader: TLifersLoaderThread;
    FNumbersLoader: TNumbersLoaderThread;
    FPermitsLoader: TPermitsLoaderThread;
    FSurveysLoader: TSurveysLoaderThread;
    procedure AddBandBalance(aBandSize: String; aBandQuantity: Integer);
    procedure AddBirthday(aName, aBirthday: String);
    procedure AddLifer(aType, aName, aDate: String);
    procedure AddPermit(aName, aExpireDate: String);
    procedure ApplyDarkMode;
    procedure RefreshBandBalance;
    procedure RefreshBirthday;
    procedure RefreshPermits;
    procedure RefreshNumbers;
    procedure RefreshChart;
    procedure RefreshMap;
    procedure RefreshLifers;
    procedure BandsLoaderTerminated(Sender: TObject);
    procedure BirthdaysLoaderTerminated(Sender: TObject);
    procedure LifersLoaderTerminated(Sender: TObject);
    procedure NumbersLoaderTerminated(Sender: TObject);
    procedure PermitsLoaderTerminated(Sender: TObject);
    procedure SurveysLoaderTerminated(Sender: TObject);
  public

  end;

var
  frmDashboard: TfrmDashboard;

implementation

uses
  cbs_global, cbs_themes, udm_main, ufrm_main,
  {$IFDEF DEBUG}cbs_debug,{$ENDIF}
  uDarkStyleParams;

{$R *.lfm}

{ TfrmDashboard }

procedure TfrmDashboard.AddBandBalance(aBandSize: String; aBandQuantity: Integer);
var
  B: TBCPanel;
  Q: TLabel;
begin
  B := TBCPanel.Create(pBandsContent);
  with B do
  begin
    Constraints.MinHeight := 60;
    Constraints.MinWidth := 60;
    Width := 60;
    ChildSizing.LeftRightSpacing := 8;
    ChildSizing.TopBottomSpacing := 8;
    Caption := aBandSize;
    BorderBCStyle := bpsBorder;
    FontEx.Height := 24;
    FontEx.Style := [fsBold];
    FontEx.TextAlignment := bcaCenterTop;
    Rounding.RoundX := 8;
    Rounding.RoundY := 8;
    if aBandQuantity = 0 then
    begin
      Background.Color := clSystemCriticalFGLight;
      Border.Color := clSystemCriticalFGLight;
      FontEx.Color := clSystemCriticalBGLight;
    end
    else
    begin
      Background.Color := clSystemCautionFGLight;
      Border.Color := clSystemCautionFGLight;
      FontEx.Color := clSystemCautionBGLight;
    end;
    Border.Style := TBCBorderStyle.bboSolid;
    if IsDarkModeEnabled then
      Color := clCardBGDefaultDark
    else
      ParentBackground := True;
    B.Parent := pBandsContent;
  end;

  Q := TLabel.Create(B);
  with Q do
  begin
    Caption := IntToStr(aBandQuantity);
    Alignment := taRightJustify;
    BorderSpacing.Right := 8;
    if aBandQuantity = 0 then
    begin
      Font.Color := clSystemCriticalBGLight;
    end
    else
    begin
      Font.Color := clSystemCautionBGLight;
    end;
    Align := alBottom;
    Q.Parent := B;
  end;
end;

procedure TfrmDashboard.AddBirthday(aName, aBirthday: String);
var
  B: TBCPanel;
  //C,
  N, D: TLabel;
begin
  B := TBCPanel.Create(pBirthdays);
  with B do
  begin
    Top := pBirthdays.Height;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BorderBCStyle := bpsBorder;
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    if IsDarkModeEnabled then
      Background.Color := clCardBGDefaultDark
    else
      Background.Color := clCardBGSecondaryLight;
    ChildSizing.HorizontalSpacing := 8;
    BorderSpacing.Bottom := 2;
    //Border.Style := TBCBorderStyle.bboSolid;
    B.Parent := pBirthdays;
  end;

  //C := TLabel.Create(B);
  //with C do
  //begin
  //  C.Parent := B;
  //  C.AutoSize := False;
  //  Width := 24;
  //  Align := alLeft;
  //  Alignment := taCenter;
  //  Layout := tlCenter;
  //  Caption := '🎂';
  //  Font.Color := clAccentTextPrimaryLight;
  //end;

  D := TLabel.Create(B);
  with D do
  begin
    Align := alRight;
    Alignment := taRightJustify;
    Layout := tlCenter;
    Caption := aBirthday;
    if IsDarkModeEnabled then
      Font.Color := clDefaultFGDark
    else
      Font.Color := clDefaultFG2Light;
    D.Parent := B;
  end;

  N := TLabel.Create(B);
  with N do
  begin
    Align := alClient;
    Layout := tlCenter;
    Caption := aName;
    N.Parent := B;
  end;
end;

procedure TfrmDashboard.AddLifer(aType, aName, aDate: String);
var
  B: TPanel;
  N, D: TLabel;
  I: TImage;
begin
  B := TPanel.Create(pLifers);
  with B do
  begin
    Top := lblTitleLifers.Top + lblTitleLifers.Height + 2;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    //Rounding.RoundX := 0;
    //Rounding.RoundY := 0;
    if IsDarkModeEnabled then
      Color := clCardBGDefaultDark
    else
      Color := clCardBGDefaultLight;
    ChildSizing.HorizontalSpacing := 8;
    //BorderSpacing.Bottom := 2;
    //Border.Style := TBCBorderStyle.bboSolid;
    B.Parent := pLifers;
  end;

  I := TImage.Create(B);
  with I do
  begin
    Align := alLeft;
    Center := True;
    Proportional := True;
    //ImageWidth := 16;
    if IsDarkModeEnabled then
      Images := vIconsDark
    else
      Images := vIcons;
    case aType of
      'C': ImageIndex := 1;
      'S': ImageIndex := 0;
    end;
    AutoSize := True;
    I.Parent := B;
  end;

  D := TLabel.Create(B);
  with D do
  begin
    Align := alRight;
    Alignment := taRightJustify;
    Layout := tlCenter;
    Caption := aDate;
    if IsDarkModeEnabled then
      Font.Color := clDefaultFGDark
    else
      Font.Color := clDefaultFG2Light;
    D.Parent := B;
  end;

  N := TLabel.Create(B);
  with N do
  begin
    Align := alClient;
    Layout := tlCenter;
    Font.Style := [fsItalic];
    Caption := aName;
    N.Parent := B;
  end;
end;

procedure TfrmDashboard.AddPermit(aName, aExpireDate: String);
var
  B: TBCPanel;
  //C,
  N, D: TLabel;
begin
  B := TBCPanel.Create(pPermitsExpiring);
  with B do
  begin
    Top := pPermitsExpiring.Height;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BorderBCStyle := bpsBorder;
    Background.Color := clCardBGSecondaryLight;
    ChildSizing.HorizontalSpacing := 8;
    BorderSpacing.Bottom := 2;
    //Border.Style := TBCBorderStyle.bboSolid;
    B.Parent := pPermitsExpiring;
  end;

  D := TLabel.Create(B);
  with D do
  begin
    Align := alRight;
    Alignment := taRightJustify;
    Layout := tlCenter;
    Caption := aExpireDate;
    Font.Color := clDefaultFG2Light;
    D.Parent := B;
  end;

  N := TLabel.Create(B);
  with N do
  begin
    Align := alClient;
    Layout := tlCenter;
    Caption := aName;
    N.Parent := B;
  end;
end;

procedure TfrmDashboard.ApplyDarkMode;
begin
  sBox.Color := clSolidBGBaseDark;
  pNumbers.Background.Color := clSolidBGBaseDark;
  pFlow.Color := clSolidBGBaseDark;
  pTotalSpecies.Background.Color := clCardBGDefaultDark;
  pTotalSpecies.Border.Color := clSystemSolidNeutralFGDark;
  pTotalIndividuals.Background.Color := clCardBGDefaultDark;
  pTotalIndividuals.Border.Color := clSystemSolidNeutralFGDark;
  pTotalNests.Background.Color := clCardBGDefaultDark;
  pTotalNests.Border.Color := clSystemSolidNeutralFGDark;
  pTotalSamplings.Background.Color := clCardBGDefaultDark;
  pTotalSamplings.Border.Color := clSystemSolidNeutralFGDark;
  pLifers.Background.Color := clCardBGDefaultDark;
  pLifers.Border.Color := clSystemSolidNeutralFGDark;
  pChartIndividuals.Background.Color := clCardBGDefaultDark;
  pChartIndividuals.Border.Color := clSystemSolidNeutralFGDark;
  chartIndividuals.Color := clCardBGDefaultDark;
  chartIndividuals.BackColor := clCardBGDefaultDark;
  chartIndividuals.LeftAxis.Marks.LabelFont.Color := clTextPrimaryDark;
  chartIndividuals.BottomAxis.Marks.LabelFont.Color := clTextPrimaryDark;
  pChartSpecies.Background.Color := clCardBGDefaultDark;
  pChartSpecies.Border.Color := clSystemSolidNeutralFGDark;
  chartSpecies.Color := clCardBGDefaultDark;
  chartSpecies.BackColor := clCardBGDefaultDark;
  chartSpecies.LeftAxis.Marks.LabelFont.Color := clTextPrimaryDark;
  chartSpecies.BottomAxis.Marks.LabelFont.Color := clTextPrimaryDark;
  pMapSurveys.Background.Color := clCardBGDefaultDark;
  pMapSurveys.Border.Color := clSystemSolidNeutralFGDark;

  pNotificationCenter.Background.Color := clVioletBG1Dark;
  pNotificationCenter.Border.Color := clCardBGSecondaryDark;
  pTitleNotificationCenter.Background.Color := clVioletBG1Dark;
  scrollNotifications.Color := clVioletBG1Dark;
  pBandsBalance.Background.Color := clCardBGDefaultDark;
  pBandsBalance.Border.Color := clSystemSolidNeutralFGDark;
  pBandsContent.Background.Color := clCardBGDefaultDark;
  pBirthdays.Background.Color := clCardBGDefaultDark;
  pBirthdays.Border.Color := clSystemSolidNeutralFGDark;
end;

procedure TfrmDashboard.BandsLoaderTerminated(Sender: TObject);
begin
  FreeAndNil(FBandsLoader);
end;

procedure TfrmDashboard.BirthdaysLoaderTerminated(Sender: TObject);
begin
  FreeAndNil(FBirthdaysLoader);
end;

procedure TfrmDashboard.FormDestroy(Sender: TObject);
begin
  TimerLoad.Enabled := False;
  //DMC.qBirthdays.Close;
  //DMC.qLastLifers.Close;
  //DMC.qBandsRunningOut.Close;
  //DMC.qLastSurveys.Close;
  //DMC.qExpiredPermits.Close;

  if Assigned(FBandsLoader) then
    FBandsLoader.Terminate;
  if Assigned(FBirthdaysLoader) then
    FBirthdaysLoader.Terminate;
  if Assigned(FLifersLoader) then
    FLifersLoader.Terminate;
  if Assigned(FNumbersLoader) then
    FNumbersLoader.Terminate;
  if Assigned(FPermitsLoader) then
    FPermitsLoader.Terminate;
  if Assigned(FSurveysLoader) then
    FSurveysLoader.Terminate;
  //mapSurveys.GPSItems.Clear(20);
  //mapSurveys.Active := False;
end;

procedure TfrmDashboard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF DEBUG}
  LogDebug('CLOSE: ' + Caption);
  {$ENDIF}

  //mapSurveys.GPSItems.Clear(20);
  mapSurveys.Active := False;

  CloseAction := caFree;
end;

procedure TfrmDashboard.FormShow(Sender: TObject);
begin
  mapSurveys.CachePath := IncludeTrailingPathDelimiter(ConcatPaths([AppDataDir, 'map-cache']));
  DMM.iMaps.GetBitmap(1, mapSurveys.POIImage);
  mapSurveys.Active := True;

  if IsDarkModeEnabled then
    ApplyDarkMode;

  TimerLoad.Enabled := True;
end;

procedure TfrmDashboard.lblTitleBandsBalanceClick(Sender: TObject);
begin
  frmMain.actViewBandsBalanceExecute(Sender);
end;

procedure TfrmDashboard.LifersLoaderTerminated(Sender: TObject);
begin
  FreeAndNil(FLifersLoader);
end;

procedure TfrmDashboard.NumbersLoaderTerminated(Sender: TObject);
begin
  FreeAndNil(FNumbersLoader);
end;

procedure TfrmDashboard.PermitsLoaderTerminated(Sender: TObject);
begin
  FreeAndNil(FPermitsLoader);
end;

procedure TfrmDashboard.pFlowChangeBounds(Sender: TObject);
begin
  if sBox.Width < ((pChartIndividuals.Constraints.MinWidth * 2) + (pFlow.ChildSizing.HorizontalSpacing * 3)) then
    pFlow.ChildSizing.ControlsPerLine := 1
  else
  if sBox.Width < ((pChartIndividuals.Constraints.MinWidth * 3) + (pFlow.ChildSizing.HorizontalSpacing * 4)) then
    pFlow.ChildSizing.ControlsPerLine := 2
  else
    pFlow.ChildSizing.ControlsPerLine := 3;
  //pFlow.Refresh;
end;

procedure TfrmDashboard.pmRefreshClick(Sender: TObject);
{$IFDEF DEBUG}
var
  Usage: TElapsedTimer;
{$ENDIF}
begin
  if Working then
    Exit;

  Working := True;

  pLoading.Visible := True;
  //Application.ProcessMessages;

  //{$IFDEF DEBUG}
  //Usage := TElapsedTimer.Create(Format('Show %s', [Caption]), 'loading bands running out');
  //{$ENDIF}
  //RefreshBandBalance;
  //Sleep(1000);

  {$IFDEF DEBUG}
  Usage := TElapsedTimer.Create(Format('Show %s', [Caption]), 'loading numbers');
  //Usage.AddPart('loading numbers');
  {$ENDIF}
  RefreshNumbers;

  {$IFDEF DEBUG}
  Usage.AddPart('loading charts');
  {$ENDIF}
  RefreshChart;

  {$IFDEF DEBUG}
  Usage.AddPart('loading survey map');
  {$ENDIF}
  RefreshMap;

  {$IFDEF DEBUG}
  Usage.AddPart('loading last lifers');
  {$ENDIF}
  RefreshLifers;

  pNotificationCenter.BeginUpdate;
  try
    {$IFDEF DEBUG}
    Usage.AddPart('loading expired permits');
    {$ENDIF}
    RefreshPermits;

    {$IFDEF DEBUG}
    Usage.AddPart('loading next birthdays');
    {$ENDIF}
    RefreshBirthday;

    {$IFDEF DEBUG}
    Usage.AddPart('loading bands running out');
    {$ENDIF}
    RefreshBandBalance;
    {$IFDEF DEBUG}
    Usage.StopTimer;
    FreeAndNil(Usage);
    {$ENDIF}

  finally
    pNotificationCenter.EndUpdate;
  end;

  pLoading.Visible := False;
  Application.ProcessMessages;

  Working := False;
end;

procedure TfrmDashboard.pNewSightingClick(Sender: TObject);
begin
  frmMain.pmaNewSightingClick(Sender);
end;

procedure TfrmDashboard.pNewSightingMouseEnter(Sender: TObject);
begin
  if (Sender = pNewSighting) or (Sender = txtShortcutNewSighting) or (Sender = icoShortcutNewSighting) then
    pNewSighting.Background.Color := clBtnHighlight;
end;

procedure TfrmDashboard.pNewSightingMouseLeave(Sender: TObject);
begin
  if (Sender = pNewSighting) or (Sender = txtShortcutNewSighting) or (Sender = icoShortcutNewSighting) then
    pNewSighting.Background.Color := clBtnFace;
end;

procedure TfrmDashboard.pNumbersResize(Sender: TObject);
begin
  if sBox.Width < ((pChartIndividuals.Constraints.MinWidth * 2) + (pFlow.ChildSizing.HorizontalSpacing * 2)) then
    pNumbers.ChildSizing.ControlsPerLine := 2
  else
  if sBox.Width < ((pChartIndividuals.Constraints.MinWidth * 3) + (pFlow.ChildSizing.HorizontalSpacing * 3)) then
    pNumbers.ChildSizing.ControlsPerLine := 4
  else
    pNumbers.ChildSizing.ControlsPerLine := 6;
end;

procedure TfrmDashboard.pShortcutConverterClick(Sender: TObject);
begin
  frmMain.actCoordinatesConverterExecute(Sender);
end;

procedure TfrmDashboard.pShortcutConverterMouseEnter(Sender: TObject);
begin
  if (Sender = pShortcutConverter) or (Sender = txtShortcutConverter) or (Sender = icoShortcutConverter) then
    pShortcutConverter.Background.Color := clBtnHighlight;
end;

procedure TfrmDashboard.pShortcutConverterMouseLeave(Sender: TObject);
begin
  if (Sender = pShortcutConverter) or (Sender = txtShortcutConverter) or (Sender = icoShortcutConverter) then
    pShortcutConverter.Background.Color := clBtnFace;
end;

procedure TfrmDashboard.RefreshBandBalance;
var
  i: Integer;
begin
  pBandsBalance.Visible := False;
  pBandsBalance.AutoSize := True;
  pBandsContent.BeginUpdate;
  for i := (pBandsContent.ComponentCount - 1) downto 0 do
    if pBandsContent.Components[i] is TBCPanel then
      pBandsContent.Components[i].Free;

  if Assigned(FBandsLoader) and not FBandsLoader.Finished then
    FBandsLoader.Terminate;

  try
    FBandsLoader := TBandsLoaderThread.Create(pBandsContent);
    FBandsLoader.OnTerminate := @BandsLoaderTerminated;
    FBandsLoader.Start;

    pBandsBalance.AutoSize := True;
    pBandsBalance.Visible := True;
    pBandsBalance.Top := pLoading.Top + pLoading.Height + 1;
  finally
    pBandsContent.EndUpdate;
    pBandsContent.AutoSize := True;
  end;
end;

procedure TfrmDashboard.RefreshBirthday;
var
  i: Integer;
begin
  pBirthdays.BeginUpdate;
  pBirthdays.Visible := False;
  pBirthdays.AutoSize := False;
  for i := (pBirthdays.ComponentCount - 1) downto 0 do
    if pBirthdays.Components[i] is TPanel then
      pBirthdays.Components[i].Free;

  if Assigned(FBirthdaysLoader) and not FBirthdaysLoader.Finished then
    FBirthdaysLoader.Terminate;

  try
    FBirthdaysLoader := TBirthdaysLoaderThread.Create(pBirthdays);
    FBirthdaysLoader.OnTerminate := @BirthdaysLoaderTerminated;
    FBirthdaysLoader.Start;
  finally
    pBirthdays.EndUpdate;
    pBirthdays.AutoSize := True;
    pBirthdays.Visible := True;
  end;
end;

procedure TfrmDashboard.RefreshChart;
var
  InitialMonth: TDate;
  i: Integer;
  Qry: TSQLQuery;
begin
  InitialMonth := IncMonth(Today, -12);
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('WITH MonthlyCaptures AS (');
    Add('   SELECT STRFTIME(''%Y-%m'', capture_date) AS record_month, COUNT(capture_id) AS quantity');
    Add('   FROM captures');
    Add('   WHERE capture_date > DATE(''now'', ''-12 months'')');
    Add('   GROUP BY record_month )');
    Add('SELECT ROW_NUMBER() OVER (ORDER BY record_month) AS id, record_month, quantity');
    Add('FROM MonthlyCaptures');
    Add('ORDER BY record_month ASC');
    Open;

    if RecordCount > 0 then
    begin
      chartIndividuals.Refresh;
      lcsIndividualsMonth.Clear;
      lcsIndividualsMonth.BeginUpdate;
      try
        for i := 1 to 12 do
        begin
          if Locate('record_month', FormatDateTime('yyyy-mm', IncMonth(InitialMonth, i - 1)), []) then
            lcsIndividualsMonth.Add(
              i,
              FieldByName('quantity').AsFloat,
              FormatDateTime('yyyy-mm', IncMonth(InitialMonth, i - 1)))
          else
            lcsIndividualsMonth.Add(
              i,
              0,
              FormatDateTime('yyyy-mm', IncMonth(InitialMonth, i - 1)))
        end;
        pChartIndividuals.Visible := True;
      finally
        lcsIndividualsMonth.EndUpdate;
      end;
    end;
    //else
    //  pChartIndividuals.Visible := False;
    Close;

    Clear;
    Add('WITH CombinedData AS (');
    Add('   SELECT STRFTIME(''%Y-%m'', t.capture_date) AS record_month, t.taxon_id AS taxon');
    Add('   FROM captures AS t');
    Add('   WHERE t.active_status = 1');
    Add('   UNION ALL');
    Add('   SELECT STRFTIME(''%Y-%m'', s.sighting_date) AS record_month, s.taxon_id AS taxon');
    Add('   FROM sightings AS s');
    Add('   WHERE s.active_status = 1 ),');
    Add('MonthlyCounts AS (');
    Add('   SELECT record_month, COUNT(DISTINCT taxon) AS quantity');
    Add('   FROM CombinedData');
    Add('   GROUP BY record_month )');
    Add('SELECT ROW_NUMBER() OVER (ORDER BY record_month) AS id, record_month, quantity');
    Add('FROM MonthlyCounts');
    Add('WHERE record_month > STRFTIME(''%Y-%m'', ''now'', ''-12 months'')');
    Add('ORDER BY record_month ASC');
    Open;
    if RecordCount > 0 then
    begin
      chartSpecies.Refresh;
      lcsSpeciesMonth.Clear;
      lcsSpeciesMonth.BeginUpdate;
      try
        for i := 1 to 12 do
        begin
          if Locate('record_month', FormatDateTime('yyyy-mm', IncMonth(InitialMonth, i - 1)), []) then
            lcsIndividualsMonth.Add(
              i,
              FieldByName('quantity').AsFloat,
              FormatDateTime('yyyy-mm', IncMonth(InitialMonth, i - 1)))
          else
            lcsIndividualsMonth.Add(
              i,
              0,
              FormatDateTime('yyyy-mm', IncMonth(InitialMonth, i - 1)))
        end;
        pChartSpecies.Visible := True;
      finally
        lcsSpeciesMonth.EndUpdate;
      end;
    end;
    //else
    //  pChartSpecies.Visible := False;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmDashboard.RefreshLifers;
var
  i: Integer;
  //Qry: TSQLQuery;
begin
  //Qry := TSQLQuery.Create(nil);
  //with Qry, SQL do
  //try
  //  Database := DMM.sqlCon;
  //  Clear;
  //  Add('SELECT taxon, nome_taxon, STRFTIME(''%d/%m/%Y'', data_registro) AS data_registro, tipo');
  //  Add('FROM get_last_lifers');
  //  Add('LIMIT 9');
  //  Open;

  pLifers.BeginUpdate;
  pLifers.Visible := False;
  pLifers.AutoSize := False;
  for i := (pLifers.ComponentCount - 1) downto 0 do
    if pLifers.Components[i] is TPanel then
      pLifers.Components[i].Free;

  if Assigned(FLifersLoader) and not FLifersLoader.Finished then
    FLifersLoader.Terminate;

  try
    FLifersLoader := TLifersLoaderThread.Create(pLifers);
    FLifersLoader.OnTerminate := @LifersLoaderTerminated;
    FLifersLoader.Start;
    //if Qry.RecordCount = 0 then
    //  Exit;
    //
    //Last;
    //pLifers.AutoSize := True;
    //repeat
    //  AddLifer(FieldByName('tipo').AsString, FieldByName('nome_taxon').AsString, FieldByName('data_registro').AsString);
    //  Prior;
    //until BOF;
    //
    //pLifers.Visible := True;
    //Top := pLoading.Top + pLoading.Height + 1;
  finally
    //Close;
    //FreeAndNil(Qry);
    pLifers.EndUpdate;
    pLifers.Visible := True;
  end;
end;

procedure TfrmDashboard.RefreshMap;
var
  poi: TGpsPoint;
  rp: TRealPoint;
begin
  pMapSurveys.BeginUpdate;
  //with DMC.qLastSurveys do
  //try

    //if DMC.qLastSurveys.Active then
    //  Refresh
    //else
    //  Open;

    mapSurveys.GPSItems.Clear(20);

  if Assigned(FSurveysLoader) and not FSurveysLoader.Finished then
    FSurveysLoader.Terminate;

  try
    FSurveysLoader := TSurveysLoaderThread.Create(mapSurveys);
    FSurveysLoader.OnTerminate := @SurveysLoaderTerminated;
    FSurveysLoader.Start;
    //if RecordCount > 0 then
    //begin
    //  First;
    //  repeat
    //    rp.Lon := FieldByName('start_longitude').AsFloat;
    //    rp.Lat := FieldByName('start_latitude').AsFloat;
    //    if not (rp.Lon = 0) and not (rp.Lat = 0) then
    //    begin
    //      //mapSurveys.LonLatToScreen(rp);
    //      poi := TGpsPoint.CreateFrom(rp);
    //      //poi.Name := FieldByName('survey_date').AsString;
    //        //+ #10 + FieldByName('locality_name').AsString + #10 +
    //        //FieldByName('method_name').AsString;
    //      mapSurveys.GPSItems.Add(poi, 20);
    //    end;
    //    Next;
    //  until EOF;
      //if mapSurveys.GPSItems.Count > 0 then
      //begin
      //  mapSurveys.ZoomOnArea(mapSurveys.GPSItems.BoundingBox);
      //  mapSurveys.Zoom := mapSurveys.Zoom - 1;
      //  mapSurveys.Visible := True;
      //end;
      //else
      //  mapSurveys.Visible := False;
    //end;
    //else
    //  mapSurveys.Visible := False;
    //Close;
  finally
    pMapSurveys.EndUpdate;
  end;
end;

procedure TfrmDashboard.RefreshNumbers;
//var
//  Qry: TSQLQuery;
begin
  lblTotalSpecies.Caption := '----';
  lblTotalIndividuals.Caption := '----';
  lblTotalNests.Caption := '----';
  lblTotalSamplings.Caption := '----';

  //Qry := TSQLQuery.Create(DMM.sqlCon);
  //with Qry, SQL do
  //try
  //  Database := DMM.sqlCon;
  //  Transaction := DMM.sqlTrans;

  { Total of species }
  if Assigned(FNumbersLoader) and not FNumbersLoader.Finished then
    FNumbersLoader.Terminate;

  FNumbersLoader := TNumbersLoaderThread.Create(lblTotalSpecies, lblTotalIndividuals, lblTotalNests, lblTotalSamplings);
  FNumbersLoader.OnTerminate := @NumbersLoaderTerminated;
  FNumbersLoader.Start;
    //Clear;
    //Add('SELECT COUNT(DISTINCT taxon_id) AS count_species');
    //Add('FROM (');
    //Add(' SELECT taxon_id FROM individuals');
    //Add(' UNION');
    //Add(' SELECT taxon_id FROM captures');
    //Add(' UNION');
    //Add(' SELECT taxon_id FROM sightings');
    //Add(' UNION');
    //Add(' SELECT taxon_id FROM nests');
    //Add(' UNION');
    //Add(' SELECT taxon_id FROM specimens');
    //Add(') AS combined_taxon_ids');
    //Open;
    //lblTotalSpecies.Caption := FieldByName('count_species').AsString;
    //Close;

    { Total of individuals }
    //Clear;
    //Add('SELECT count(*) AS count_individuals FROM individuals WHERE (active_status = 1)');
    //Open;
    //lblTotalIndividuals.Caption := FieldByName('count_individuals').AsString;
    //Close;

    { Total of nests }
    //Clear;
    //Add('SELECT count(*) AS count_nests FROM nests WHERE (active_status = 1)');
    //Open;
    //lblTotalNests.Caption := FieldByName('count_nests').AsString;
    //Close;

    { Total of samplings }
    //Clear;
    //Add('SELECT count(*) AS count_samplings FROM surveys WHERE (active_status = 1)');
    //Open;
    //lblTotalSamplings.Caption := FieldByName('count_samplings').AsString;
    //Close;

  //finally
  //  FreeAndNil(Qry);
  //end;
end;

procedure TfrmDashboard.RefreshPermits;
var
  i: Integer;
  //Qry: TSQLQuery;
begin
  pPermitsExpiring.BeginUpdate;
  pPermitsExpiring.Visible := False;
  pPermitsExpiring.AutoSize := False;
  for i := (pPermitsExpiring.ComponentCount - 1) downto 0 do
    if pPermitsExpiring.Components[i] is TBCPanel then
      pPermitsExpiring.Components[i].Free;

  //Qry := TSQLQuery.Create(nil);
  //with Qry, SQL do
  if Assigned(FPermitsLoader) and not FPermitsLoader.Finished then
    FPermitsLoader.Terminate;

  try
    FPermitsLoader := TPermitsLoaderThread.Create(pPermitsExpiring);
    FPermitsLoader.OnTerminate := @PermitsLoaderTerminated;
    FPermitsLoader.Start;
    //Database := DMM.sqlCon;
    //Transaction := DMM.sqlTrans;
    //Clear;
    //Add('SELECT expire_date, permit_name');
    //Add('FROM get_expired_permits');
    //Add('LIMIT 7');
    //Open;
    //
    //if Qry.RecordCount = 0 then
    //  Exit;
    //
    //Last;
    //pPermitsExpiring.AutoSize := True;
    //while not BOF do
    //begin
    //  AddPermit(FieldByName('permit_name').AsString, FieldByName('expire_date').AsString);
    //  Prior;
    //end;
    //
    //pPermitsExpiring.Visible := True;
    //AutoSize := True;
    //pPermitsExpiring.Top := pLoading.Top + pLoading.Height + 1;
  finally
    //Close;
    //FreeAndNil(Qry);
    pPermitsExpiring.EndUpdate;
  end;
end;

procedure TfrmDashboard.sBoxResize(Sender: TObject);
begin
  if sBox.Height > pFlow.Height then
    pFlow.Height := sBox.Height
  else
    pFlow.Height := pLifers.Top + pLifers.Height + pFlow.ChildSizing.TopBottomSpacing;
end;

procedure TfrmDashboard.sbUpdateLaterClick(Sender: TObject);
begin
  pAppUpdate.Visible := False;
end;

procedure TfrmDashboard.SurveysLoaderTerminated(Sender: TObject);
begin
  FreeAndNil(FSurveysLoader);
end;

procedure TfrmDashboard.TimerLoadTimer(Sender: TObject);
begin
  TimerLoad.Enabled := False;
  TimerLoad.Interval := 30000;
  // TimerLoad.Enabled:= True;

  pmRefreshClick(nil);

  sBox.VertScrollBar.Position := 0;
end;

procedure TfrmDashboard.TimerReloadTimer(Sender: TObject);
begin
  if Working or Closing then
    Exit;

  //if DMM.sqlCon.Connected and DMM.WaitingConnection then
  //begin
  //  DMM.WaitingConnection := False;
  //  pmRefreshClick(nil);
  //end;
end;

end.

