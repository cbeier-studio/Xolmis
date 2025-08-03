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
}

unit ufrm_printpreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  LCLIntf, LR_Class, LR_View, LR_DBSet, LR_ChBox, Printers, Menus, BCFluentSlider, PrintersDlgs, lrPDFExport,
  lr_e_fclpdf;

type

  { TfrmPrintPreview }

  TfrmPrintPreview = class(TForm)
    frCheckBoxObj: TfrCheckBoxObject;
    frDataSet: TfrDBDataSet;
    frDetails: TfrDBDataSet;
    frDetails5: TfrDBDataSet;
    frDetails4: TfrDBDataSet;
    frDetails3: TfrDBDataSet;
    frDetails2: TfrDBDataSet;
    frPreview: TfrPreview;
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
    Report: TfrReport;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblPage: TLabel;
    lblZoom: TLabel;
    lrPDFExport: TlrPDFExport;
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
    procedure frPreviewScrollPage(Sender: TObject);
    procedure ReportBeginDoc;
    procedure ReportEndDoc;
    procedure ReportProgress(n: Integer);
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
    FReportName: String;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDetailSource1(AValue: TDataSource);
    procedure SetDetailSource2(AValue: TDataSource);
    procedure SetDetailSource3(AValue: TDataSource);
    procedure SetDetailSource4(AValue: TDataSource);
    procedure SetDetailSource5(AValue: TDataSource);
    procedure SetReportName(AValue: String);
    procedure ApplyDarkMode;
    procedure UpdateButtons;
  public
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DetailSource1: TDataSource read FDetailSource1 write SetDetailSource1;
    property DetailSource2: TDataSource read FDetailSource2 write SetDetailSource2;
    property DetailSource3: TDataSource read FDetailSource3 write SetDetailSource3;
    property DetailSource4: TDataSource read FDetailSource4 write SetDetailSource4;
    property DetailSource5: TDataSource read FDetailSource5 write SetDetailSource5;
    property ReportName: String read FReportName write SetReportName;
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, udlg_loading, udlg_progress, udm_reports, uDarkStyleParams;

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

  if FReportName <> EmptyStr then
  begin
    Report.DoublePass := True;
    Report.LoadFromFile(FReportName);

    Report.PrepareReport;
    Report.ShowPreparedReport;

    sbZoomAdjustWidthClick(nil);
  end;
end;

procedure TfrmPrintPreview.frPreviewScrollPage(Sender: TObject);
begin
  lblPage.Caption := Format(rsPageOfPages, [frPreview.Page, frPreview.AllPages]);

  UpdateButtons;
end;

procedure TfrmPrintPreview.ReportBeginDoc;
begin
  //if not Visible then
  //begin
  //  dlgProgress := TdlgProgress.Create(nil);
  //  dlgProgress.AllowCancel := False;
  //  dlgProgress.Max := Report.Pages.Count;
  //  dlgProgress.Title := rsTitlePrintPreview;
  //  dlgProgress.Text := rsProgressGeneratingReport;
  //  dlgProgress.Show;
  //end;
  //dlgLoading.ringProgress.MaxValue := Report.Pages.Count;
  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsProgressGeneratingReport, 0);
  //PBar.Max := Report.Pages.Count;
  //PBar.Visible := True;
end;

procedure TfrmPrintPreview.ReportEndDoc;
begin
  //if Assigned(dlgProgress) then
  //begin
  //  dlgProgress.Close;
  //  FreeAndNil(dlgProgress);
  //end;
  dlgLoading.Hide;
  //dlgLoading.ringProgress.MaxValue := 100;
  //PBar.Visible := False;
end;

procedure TfrmPrintPreview.ReportProgress(n: Integer);
begin
  //if Assigned(dlgProgress) then
  //  dlgProgress.Position := n;
  dlgLoading.UpdateProgress(rsProgressGeneratingReport, n);
  //PBar.Position := n;
end;

procedure TfrmPrintPreview.sbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPrintPreview.sbFirstPageClick(Sender: TObject);
begin
  frPreview.First;

  UpdateButtons;
end;

procedure TfrmPrintPreview.sbLastPageClick(Sender: TObject);
begin
  frPreview.Last;

  UpdateButtons;
end;

procedure TfrmPrintPreview.sbNextPageClick(Sender: TObject);
begin
  frPreview.Next;

  UpdateButtons;
end;

procedure TfrmPrintPreview.sbPrintSettingsClick(Sender: TObject);
begin
  PrinterSetupDlg.Execute;
end;

procedure TfrmPrintPreview.sbPriorPageClick(Sender: TObject);
begin
  frPreview.Prev;

  UpdateButtons;
end;

procedure TfrmPrintPreview.sbPrintClick(Sender: TObject);
var
  nPages: String;
  nPrinter: Integer;
begin
  nPages := EmptyStr;
  nPrinter := Printer.PrinterIndex;
  Report.DoublePass := True;

  if not Report.PrepareReport then
    Exit;

  PrintDlg.Copies := 1;
  PrintDlg.Collate := True; // ordened copies
  PrintDlg.FromPage := 1; // start page
  PrintDlg.ToPage := Report.EMFPages.Count; // last page
  PrintDlg.MaxPage := Report.EMFPages.Count; // maximum allowed number of pages
  if PrintDlg.Execute then
  begin
    if (Printer.PrinterIndex <> nPrinter ) or // verify if selected printer has changed
        Report.CanRebuild or // ... only makes sense if we can reformat the report
        Report.ChangePrinter(nPrinter, Printer.PrinterIndex) then //... then change printer
      Report.PrepareReport //... and reformat for new printer
    else
      Exit; // we couldn't honour the printer change

    if PrintDlg.PrintRange = prPageNums then // user made page range selection
    begin
      nPages := IntToStr(PrintDlg.FromPage) + '-' + IntToStr(PrintDlg.ToPage);
    end;

    Report.PrintPreparedReport(nPages, PrintDlg.Copies);
  end;
end;

procedure TfrmPrintPreview.sbSavePDFClick(Sender: TObject);
begin
  if SaveDlg.Execute then
  begin
    Report.DoublePass := True;
    if Report.PrepareReport then
    begin
      Report.ExportFilename := SaveDlg.FileName;
      Report.ExportTo(TlrPdfExportFilter, SaveDlg.FileName);

      if xSettings.OpenFileAfterExport then
        OpenDocument(SaveDlg.FileName);
    end;
  end;
end;

procedure TfrmPrintPreview.sbZoom100Click(Sender: TObject);
begin
  tbZoom.Value := 100;
end;

procedure TfrmPrintPreview.sbZoomAdjustClick(Sender: TObject);
begin
  frPreview.OnePage;
  tbZoom.Value := Round(frPreview.Zoom);
end;

procedure TfrmPrintPreview.sbZoomAdjustWidthClick(Sender: TObject);
begin
  frPreview.PageWidth;
  tbZoom.Value := Round(frPreview.Zoom);
end;

procedure TfrmPrintPreview.sbZoomInClick(Sender: TObject);
begin
  if tbZoom.Value < tbZoom.MaxValue then
    if tbZoom.Value >= 100 then
      tbZoom.Value := tbZoom.Value + 50
    else
      tbZoom.Value := tbZoom.Value + 10;
end;

procedure TfrmPrintPreview.sbZoomOutClick(Sender: TObject);
begin
  if tbZoom.Value > tbZoom.MinValue then
    if tbZoom.Value > 100 then
      tbZoom.Value := tbZoom.Value - 50
    else
      tbZoom.Value := tbZoom.Value - 10;
end;

procedure TfrmPrintPreview.SetDataSource(AValue: TDataSource);
begin
  FDataSource := AValue;
  frDataSet.DataSource := FDataSource;
end;

procedure TfrmPrintPreview.SetDetailSource1(AValue: TDataSource);
begin
  FDetailSource1 := AValue;
  frDetails.DataSource := FDetailSource1;
end;

procedure TfrmPrintPreview.SetDetailSource2(AValue: TDataSource);
begin
  FDetailSource2 := AValue;
  frDetails2.DataSource := FDetailSource2;
end;

procedure TfrmPrintPreview.SetDetailSource3(AValue: TDataSource);
begin
  FDetailSource3 := AValue;
  frDetails3.DataSource := FDetailSource3;
end;

procedure TfrmPrintPreview.SetDetailSource4(AValue: TDataSource);
begin
  FDetailSource4 := AValue;
  frDetails4.DataSource := FDetailSource4;
end;

procedure TfrmPrintPreview.SetDetailSource5(AValue: TDataSource);
begin
  FDetailSource5 := AValue;
  frDetails5.DataSource := FDetailSource5;
end;

procedure TfrmPrintPreview.SetReportName(AValue: String);
begin
  FReportName := ConcatPaths([InstallDir, 'reports\', AValue]);
  if not FileExists(FReportName) then
    MsgDlg(rsTitleError, Format(rsErrorReportNotFound, [AValue]), mtError);
end;

procedure TfrmPrintPreview.tbZoomChangeValue(Sender: TObject);
begin
  lblZoom.Caption := IntToStr(tbZoom.Value) + '%';
  frPreview.Zoom := tbZoom.Value;

  UpdateButtons;
end;

procedure TfrmPrintPreview.UpdateButtons;
begin
  sbPrint.Enabled := frPreview.AllPages > 0;
  sbSavePDF.Enabled := frPreview.AllPages > 0;

  sbFirstPage.Enabled := frPreview.Page > 1;
  sbPriorPage.Enabled := frPreview.Page > 1;
  sbNextPage.Enabled := frPreview.Page < frPreview.AllPages;
  sbLastPage.Enabled := frPreview.Page < frPreview.AllPages;

  sbZoomAdjustWidth.Enabled := frPreview.AllPages > 0;
  sbZoomAdjust.Enabled := frPreview.AllPages > 0;
  sbZoom100.Enabled := frPreview.AllPages > 0;
  sbZoomOut.Enabled := (frPreview.AllPages > 0) or (tbZoom.Value > tbZoom.MinValue);
  sbZoomIn.Enabled := (frPreview.AllPages > 0) or (tbZoom.Value < tbZoom.MaxValue);
  tbZoom.Enabled := frPreview.AllPages > 0;

  pmPrint.Enabled := sbPrint.Enabled;
  pmSavePdf.Enabled := sbSavePDF.Enabled;
  pmFirstPage.Enabled := sbFirstPage.Enabled;
  pmPriorPage.Enabled := sbPriorPage.Enabled;
  pmNextPage.Enabled := sbNextPage.Enabled;
  pmLastPage.Enabled := sbLastPage.Enabled;
  pmZoomPageWidth.Enabled := sbZoomAdjustWidth.Enabled;
  pmZoomPageFit.Enabled := sbZoomAdjust.Enabled;
  pmZoomActualSize.Enabled := sbZoom100.Enabled;
  pmZoomIn.Enabled := sbZoomIn.Enabled;
  pmZoomOut.Enabled := sbZoomOut.Enabled;
end;

end.

