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
  BCButton, BCTypes, mvMapViewer, mvTypes, mvGpsObj, mvDrawingEngine, mvDE_BGRA, ImgList;

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
  public

  end;

var
  frmDashboard: TfrmDashboard;

implementation

uses cbs_global, cbs_themes, udm_main, udm_client, ufrm_main, uDarkStyleParams;

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
    //Top := B.Parent.Height;
    //Height := 44;
    Width := 60;
    //Align := alTop;
    Constraints.MinHeight := 60;
    Constraints.MinWidth := 60;
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
    Align := alBottom;
    Caption := IntToStr(aBandQuantity);
    Alignment := taRightJustify;
    //BorderBCStyle := bpsBorder;
    BorderSpacing.Right := 8;
    //FontEx.Style := [fsBold];
    //Rounding.RoundX := 8;
    //Rounding.RoundY := 8;
    //Rounding.RoundOptions := [rrBottomLeftSquare, rrTopLeftSquare];
    if aBandQuantity = 0 then
    begin
      //Background.Color := clSystemCriticalFGLight;
      Font.Color := clSystemCriticalBGLight;
    end
    else
    begin
      //Background.Color := clSystemCautionFGLight;
      Font.Color := clSystemCautionBGLight;
    end;
    //Q.ParentBackground := True;
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
  B: TBCPanel;
  N, D: TLabel;
  I: TImage;
begin
  B := TBCPanel.Create(pLifers);
  with B do
  begin
    Top := lblTitleLifers.Top + lblTitleLifers.Height + 2;
    Height := 24;
    Align := alTop;
    Caption := EmptyStr;
    BorderBCStyle := bpsBorder;
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    if IsDarkModeEnabled then
      Background.Color := clCardBGDefaultDark
    else
      Background.Color := clCardBGDefaultLight;
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

procedure TfrmDashboard.FormDestroy(Sender: TObject);
begin
  TimerLoad.Enabled := False;
  DMC.qBirthdays.Close;
  DMC.qLastLifers.Close;
  DMC.qBandsRunningOut.Close;
  DMC.qLastSurveys.Close;
  DMC.qExpiredPermits.Close;

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
begin
  if Working then
    Exit;

  Working := True;

  pLoading.Visible := True;
  Application.ProcessMessages;

  RefreshNumbers;
  Application.ProcessMessages;

  RefreshChart;
  Application.ProcessMessages;

  RefreshMap;
  Application.ProcessMessages;

  if DMC.qExpiredPermits.Active then
    DMC.qExpiredPermits.Refresh
  else
    DMC.qExpiredPermits.Open;
  RefreshPermits;
  Application.ProcessMessages;

  if DMC.qBirthdays.Active then
    DMC.qBirthdays.Refresh
  else
    DMC.qBirthdays.Open;
  RefreshBirthday;
  Application.ProcessMessages;

  if DMC.qLastLifers.Active then
    DMC.qLastLifers.Refresh
  else
    DMC.qLastLifers.Open;
  RefreshLifers;
  Application.ProcessMessages;

  RefreshBandBalance;

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
  Qry: TSQLQuery;
begin
  pBandsBalance.Visible := False;
  pBandsBalance.AutoSize := True;
  for i := (pBandsContent.ComponentCount - 1) downto 0 do
    if pBandsContent.Components[i] is TBCPanel then
      pBandsContent.Components[i].Free;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT band_size, saldo, media_dia, media_expedicao');
    Add('FROM get_bands_running_out');
    Add('ORDER BY saldo ASC');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    First;
    repeat
      AddBandBalance(FieldByName('band_size').AsString, FieldByName('saldo').AsInteger);
      Next;
    until EOF;

  finally
    Close;
    FreeAndNil(Qry);
  end;

  pBandsContent.AutoSize := True;
  pBandsBalance.AutoSize := True;
  pBandsBalance.Visible := True;
  pBandsBalance.Top := pLoading.Top + pLoading.Height + 1;
end;

procedure TfrmDashboard.RefreshBirthday;
var
  i: Integer;
begin
  with pBirthdays do
  begin
    Visible := False;
    AutoSize := False;
    for i := (ComponentCount - 1) downto 0 do
      if Components[i] is TBCPanel then
        Components[i].Free;

    if DMC.qBirthdays.RecordCount = 0 then
      Exit;

    with DMC.qBirthdays do
    begin
      Last;
      AutoSize := True;
      repeat
        AddBirthday(FieldByName('full_name').AsString, FieldByName('aniver').AsString);
        Prior;
      until BOF;
    end;

    //AutoSize := False;
    //AutoSize := False;
    //Height := (lblTitleBirthdays.Height + lblTitleBirthdays.BorderSpacing.Bottom) +
    //  (ChildSizing.TopBottomSpacing * 2) + (ChildSizing.VerticalSpacing * (ComponentCount - 2)) +
    //  (24 * (ComponentCount - 2));
    Visible := True;
    //AutoSize := True;
    Top := pLoading.Top + pLoading.Height + 1;
  end;
end;

procedure TfrmDashboard.RefreshChart;
var
  InitialMonth: TDate;
  i: Integer;
begin
  InitialMonth := IncMonth(Today, -12);
  with DMC.qIndividualsMonth do
  begin
    if DMC.qIndividualsMonth.Active then
      Refresh
    else
      Open;
    if DMC.qIndividualsMonth.RecordCount > 0 then
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
  end;

  with DMC.qSpeciesMonth do
  begin
    if DMC.qSpeciesMonth.Active then
      Refresh
    else
      Open;
    if DMC.qIndividualsMonth.RecordCount > 0 then
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
  end;
end;

procedure TfrmDashboard.RefreshLifers;
var
  i: Integer;
begin
  with pLifers do
  begin
    Visible := False;
    AutoSize := False;
    for i := (ComponentCount - 1) downto 0 do
      if Components[i] is TBCPanel then
        Components[i].Free;

    if DMC.qLastLifers.RecordCount = 0 then
      Exit;

    with DMC.qLastLifers do
    begin
      Last;
      AutoSize := True;
      repeat
        AddLifer(FieldByName('tipo').AsString, FieldByName('nome_taxon').AsString, FieldByName('data_registro').AsString);
        Prior;
      until BOF;
    end;

    Visible := True;
    //Top := pLoading.Top + pLoading.Height + 1;
  end;
end;

procedure TfrmDashboard.RefreshMap;
var
  poi: TGpsPoint;
  rp: TRealPoint;
begin
  with DMC.qLastSurveys do
  begin
    if DMC.qLastSurveys.Active then
      Refresh
    else
      Open;

    mapSurveys.GPSItems.Clear(20);

    if RecordCount > 0 then
    begin
      First;
      repeat
        rp.Lon := FieldByName('start_longitude').AsFloat;
        rp.Lat := FieldByName('start_latitude').AsFloat;
        if not (rp.Lon = 0) and not (rp.Lat = 0) then
        begin
          //mapSurveys.LonLatToScreen(rp);
          poi := TGpsPoint.CreateFrom(rp);
          //poi.Name := FieldByName('survey_date').AsString;
            //+ #10 + FieldByName('locality_name').AsString + #10 +
            //FieldByName('method_name').AsString;
          mapSurveys.GPSItems.Add(poi, 20);
        end;
        Next;
      until EOF;
      if mapSurveys.GPSItems.Count > 0 then
      begin
        mapSurveys.ZoomOnArea(mapSurveys.GPSItems.BoundingBox);
        mapSurveys.Zoom := mapSurveys.Zoom - 1;
        mapSurveys.Visible := True;
      end;
      //else
      //  mapSurveys.Visible := False;
    end;
    //else
    //  mapSurveys.Visible := False;
    Close;
  end;
end;

procedure TfrmDashboard.RefreshNumbers;
var
  Qry: TSQLQuery;
begin
  lblTotalSpecies.Caption := '----';
  lblTotalIndividuals.Caption := '----';
  lblTotalNests.Caption := '----';
  lblTotalSamplings.Caption := '----';

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    { Total of species }
    Clear;
    Add('SELECT COUNT(DISTINCT taxon_id) AS count_species');
    Add('FROM (');
    Add(' SELECT taxon_id FROM individuals');
    Add(' UNION');
    Add(' SELECT taxon_id FROM captures');
    Add(' UNION');
    Add(' SELECT taxon_id FROM sightings');
    Add(' UNION');
    Add(' SELECT taxon_id FROM nests');
    Add(' UNION');
    Add(' SELECT taxon_id FROM specimens');
    Add(') AS combined_taxon_ids');
    Open;
    lblTotalSpecies.Caption := FieldByName('count_species').AsString;
    Close;

    { Total of individuals }
    Clear;
    Add('SELECT count(*) AS count_individuals FROM individuals WHERE (active_status = 1)');
    Open;
    lblTotalIndividuals.Caption := FieldByName('count_individuals').AsString;
    Close;

    { Total of nests }
    Clear;
    Add('SELECT count(*) AS count_nests FROM nests WHERE (active_status = 1)');
    Open;
    lblTotalNests.Caption := FieldByName('count_nests').AsString;
    Close;

    { Total of samplings }
    Clear;
    Add('SELECT count(*) AS count_samplings FROM surveys WHERE (active_status = 1)');
    Open;
    lblTotalSamplings.Caption := FieldByName('count_samplings').AsString;
    Close;

  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmDashboard.RefreshPermits;
var
  i: Integer;
begin
  with pPermitsExpiring do
  begin
    Visible := False;
    AutoSize := False;
    for i := (ComponentCount - 1) downto 0 do
      if Components[i] is TBCPanel then
        Components[i].Free;

    if DMC.qExpiredPermits.RecordCount = 0 then
      Exit;

    with DMC.qExpiredPermits do
    begin
      Last;
      AutoSize := True;
      repeat
        AddPermit(FieldByName('permit_name').AsString, FieldByName('expire_date').AsString);
        Prior;
      until BOF;
    end;

    //AutoSize := False;
    //AutoSize := False;
    //Height := (lblTitleBirthdays.Height + lblTitleBirthdays.BorderSpacing.Bottom) +
    //  (ChildSizing.TopBottomSpacing * 2) + (ChildSizing.VerticalSpacing * (ComponentCount - 2)) +
    //  (24 * (ComponentCount - 2));
    Visible := True;
    //AutoSize := True;
    Top := pLoading.Top + pLoading.Height + 1;
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
  if DMM.sqlCon.Connected and DMC.WaitingConnection then
    pmRefreshClick(nil);
end;

end.

