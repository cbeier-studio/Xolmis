{ Xolmis Print Preview dialog

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.

  2026-10-20 - Christian Beier - Converted to use with Xolmis Report Subsystem.
}

unit ufrm_printpreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  LCLIntf, Printers, Menus, BCFluentSlider, uCEFChromium, uCEFWindowParent, uCEFInterfaces, PrintersDlgs;

type

  { TfrmPrintPreview }

  TfrmPrintPreview = class(TForm)
    cefPreview: TCEFWindowParent;
    Chromium: TChromium;
    pmSavePdf: TMenuItem;
    pmZoomActualSize: TMenuItem;
    pmZoomIn: TMenuItem;
    pmZoomOut: TMenuItem;
    pmPrint: TMenuItem;
    pmFirstPage: TMenuItem;
    pmPriorPage: TMenuItem;
    pmNextPage: TMenuItem;
    pmLastPage: TMenuItem;
    pmZoom: TMenuItem;
    pmZoomPageWidth: TMenuItem;
    pmZoomPageFit: TMenuItem;
    pmReport: TPopupMenu;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblPage: TLabel;
    lblZoom: TLabel;
    PrintDlg: TPrintDialog;
    PrinterSetupDlg: TPrinterSetupDialog;
    PBar: TProgressBar;
    pStatusBar: TPanel;
    pToolbar: TPanel;
    SaveDlg: TSaveDialog;
    sbFirstPage: TSpeedButton;
    sbLastPage: TSpeedButton;
    sbNextPage: TSpeedButton;
    sbPrintSettings: TSpeedButton;
    sbPrint: TSpeedButton;
    sbPriorPage: TSpeedButton;
    sbSavePDF: TSpeedButton;
    sbClose: TSpeedButton;
    sbZoom100: TSpeedButton;
    sbZoomAdjust: TSpeedButton;
    sbZoomAdjustWidth: TSpeedButton;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    tbZoom: TBCFluentSlider;
    procedure FormShow(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbFirstPageClick(Sender: TObject);
    procedure sbLastPageClick(Sender: TObject);
    procedure sbNextPageClick(Sender: TObject);
    procedure sbPrintSettingsClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbPriorPageClick(Sender: TObject);
    procedure sbSavePDFClick(Sender: TObject);
    procedure sbZoom100Click(Sender: TObject);
    procedure sbZoomAdjustClick(Sender: TObject);
    procedure sbZoomAdjustWidthClick(Sender: TObject);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure tbZoomChangeValue(Sender: TObject);
  private
    FDataSource, FDetailSource1, FDetailSource2, FDetailSource3, FDetailSource4, FDetailSource5: TDataSource;
    FDetailSource6, FDetailSource7: TDataSource;
    FReportName: String;
    FHTMLContent: String;
    procedure ChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDetailSource1(AValue: TDataSource);
    procedure SetDetailSource2(AValue: TDataSource);
    procedure SetDetailSource3(AValue: TDataSource);
    procedure SetDetailSource4(AValue: TDataSource);
    procedure SetDetailSource5(AValue: TDataSource);
    procedure SetDetailSource6(AValue: TDataSource);
    procedure SetDetailSource7(AValue: TDataSource);
    procedure SetReportName(AValue: String);
    procedure ApplyDarkMode;
  public
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DetailSource1: TDataSource read FDetailSource1 write SetDetailSource1;
    property DetailSource2: TDataSource read FDetailSource2 write SetDetailSource2;
    property DetailSource3: TDataSource read FDetailSource3 write SetDetailSource3;
    property DetailSource4: TDataSource read FDetailSource4 write SetDetailSource4;
    property DetailSource5: TDataSource read FDetailSource5 write SetDetailSource5;
    property DetailSource6: TDataSource read FDetailSource6 write SetDetailSource6;
    property DetailSource7: TDataSource read FDetailSource7 write SetDetailSource7;
    property ReportName: String read FReportName write SetReportName;
    property HTMLContent: String read FHTMLContent write FHTMLContent;
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

uses
  utils_locale, utils_global, uDarkStyleParams;

{$R *.lfm}

{ TfrmPrintPreview }

procedure TfrmPrintPreview.ApplyDarkMode;
begin
  sbPrint.Images := iButtonsDark;
  sbPrintSettings.Images := iButtonsDark;
  sbSavePDF.Images := iButtonsDark;
  sbFirstPage.Images := iButtonsDark;
  sbPriorPage.Images := iButtonsDark;
  sbNextPage.Images := iButtonsDark;
  sbLastPage.Images := iButtonsDark;
  sbZoomAdjustWidth.Images := iButtonsDark;
  sbZoomAdjust.Images := iButtonsDark;
  sbZoom100.Images := iButtonsDark;
  sbZoomOut.Images := iButtonsDark;
  sbZoomIn.Images := iButtonsDark;
  sbClose.Images := iButtonsDark;

  pmReport.Images := iButtonsDark;
end;

procedure TfrmPrintPreview.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  sbPrint.Visible := False;
  sbPrintSettings.Visible := False;
  sbSavePDF.Visible := False;
  sbFirstPage.Visible := False;
  sbPriorPage.Visible := False;
  sbNextPage.Visible := False;
  sbLastPage.Visible := False;
  pStatusBar.Visible := False;
  cefPreview.Align := alClient;

  Chromium.OnAfterCreated := @ChromiumAfterCreated;
  if not Chromium.CreateBrowser(cefPreview, '') then
    raise Exception.Create(rsErrorStartingChromium);
end;

procedure TfrmPrintPreview.ChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  Chromium.LoadString(FHTMLContent, 'about:blank');
end;

procedure TfrmPrintPreview.sbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPrintPreview.sbFirstPageClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbLastPageClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbNextPageClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbPrintSettingsClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbPriorPageClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbPrintClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbSavePDFClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbZoom100Click(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbZoomAdjustClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbZoomAdjustWidthClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbZoomInClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.sbZoomOutClick(Sender: TObject);
begin
end;

procedure TfrmPrintPreview.SetDataSource(AValue: TDataSource);
begin
  FDataSource := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource1(AValue: TDataSource);
begin
  FDetailSource1 := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource2(AValue: TDataSource);
begin
  FDetailSource2 := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource3(AValue: TDataSource);
begin
  FDetailSource3 := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource4(AValue: TDataSource);
begin
  FDetailSource4 := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource5(AValue: TDataSource);
begin
  FDetailSource5 := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource6(AValue: TDataSource);
begin
  FDetailSource6 := AValue;
end;

procedure TfrmPrintPreview.SetDetailSource7(AValue: TDataSource);
begin
  FDetailSource7 := AValue;
end;

procedure TfrmPrintPreview.SetReportName(AValue: String);
begin
  FReportName := ConcatPaths([InstallDir, 'reports\', AValue]);
  // if not FileExists(FReportName) then
  //   MsgDlg(rsTitleError, Format(rsErrorReportNotFound, [AValue]), mtError);
end;

procedure TfrmPrintPreview.tbZoomChangeValue(Sender: TObject);
begin
end;

end.
