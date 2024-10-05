unit ufrm_printpreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  LCLIntf, LR_Class, LR_View, LR_DBSet, LR_ChBox, Printers, PrintersDlgs, lrPDFExport, lr_e_fclpdf;

type

  { TfrmPrintPreview }

  TfrmPrintPreview = class(TForm)
    frCheckBoxObj: TfrCheckBoxObject;
    frDataSet: TfrDBDataSet;
    frDetails: TfrDBDataSet;
    frPreview: TfrPreview;
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
    sbZoom100: TSpeedButton;
    sbZoomAdjust: TSpeedButton;
    sbZoomAdjustWidth: TSpeedButton;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    tbZoom: TTrackBar;
    procedure FormShow(Sender: TObject);
    procedure frPreviewScrollPage(Sender: TObject);
    procedure ReportBeginDoc;
    procedure ReportEndDoc;
    procedure ReportProgress(n: Integer);
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
    procedure tbZoomChange(Sender: TObject);
  private
    FDataSource, FDetailSource: TDataSource;
    FReportName: String;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDetailSource(AValue: TDataSource);
    procedure SetReportName(AValue: String);
    procedure ApplyDarkMode;
    procedure UpdateButtons;
  public
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DetailSource: TDataSource read FDetailSource write SetDetailSource;
    property ReportName: String read FReportName write SetReportName;
  end;

var
  frmPrintPreview: TfrmPrintPreview;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, udlg_progress, uDarkStyleParams;

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
  lblPage.Caption := Format('%d of %d', [frPreview.Page, frPreview.AllPages]);

  UpdateButtons;
end;

procedure TfrmPrintPreview.ReportBeginDoc;
begin
  //if not Visible then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.AllowCancel := False;
    dlgProgress.Max := Report.Pages.Count;
    dlgProgress.Title := rsTitlePrintPreview;
    dlgProgress.Text := rsProgressGeneratingReport;
    dlgProgress.Show;
  end;
  //PBar.Max := Report.Pages.Count;
  //PBar.Visible := True;
end;

procedure TfrmPrintPreview.ReportEndDoc;
begin
  if Assigned(dlgProgress) then
  begin
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;
  //PBar.Visible := False;
end;

procedure TfrmPrintPreview.ReportProgress(n: Integer);
begin
  if Assigned(dlgProgress) then
    dlgProgress.Position := n;
  //PBar.Position := n;
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

      if XSettings.OpenFileAfterExport then
        OpenDocument(SaveDlg.FileName);
    end;
  end;
end;

procedure TfrmPrintPreview.sbZoom100Click(Sender: TObject);
begin
  tbZoom.Position := 100;
end;

procedure TfrmPrintPreview.sbZoomAdjustClick(Sender: TObject);
begin
  frPreview.OnePage;
  tbZoom.Position := Round(frPreview.Zoom);
end;

procedure TfrmPrintPreview.sbZoomAdjustWidthClick(Sender: TObject);
begin
  frPreview.PageWidth;
  tbZoom.Position := Round(frPreview.Zoom);
end;

procedure TfrmPrintPreview.sbZoomInClick(Sender: TObject);
begin
  if tbZoom.Position < tbZoom.Max then
    if tbZoom.Position >= 100 then
      tbZoom.Position := tbZoom.Position + 50
    else
      tbZoom.Position := tbZoom.Position + 10;
end;

procedure TfrmPrintPreview.sbZoomOutClick(Sender: TObject);
begin
  if tbZoom.Position > tbZoom.Min then
    if tbZoom.Position > 100 then
      tbZoom.Position := tbZoom.Position - 50
    else
      tbZoom.Position := tbZoom.Position - 10;
end;

procedure TfrmPrintPreview.SetDataSource(AValue: TDataSource);
begin
  FDataSource := AValue;
  //qPrint.SQL.Text := TSQLQuery(FDataSource.DataSet).SQL.Text;

  if FReportName = ConcatPaths([InstallDir, 'reports\rep_bands_by_status.lrf']) then
  begin
    FDataSource.DataSet.Close;
    if Pos('ORDER BY', TSQLQuery(FDataSource.DataSet).SQL.Text) > 0 then
      TSQLQuery(FDataSource.DataSet).SQL.Delete(TSQLQuery(FDataSource.DataSet).SQL.Count - 1);
    TSQLQuery(FDataSource.DataSet).SQL.Add('ORDER BY b.band_status ASC, b.band_size ASC, b.band_number ASC');
    {$IFDEF DEBUG}
    LogSQL(TSQLQuery(FDataSource.DataSet).SQL);
    {$ENDIF}
    FDataSource.DataSet.Open;
  end
  else
  if FReportName = ConcatPaths([InstallDir, 'reports\rep_bands_by_carrier.lrf']) then
  begin
    FDataSource.DataSet.Close;
    if Pos('ORDER BY', TSQLQuery(FDataSource.DataSet).SQL.Text) > 0 then
      TSQLQuery(FDataSource.DataSet).SQL.Delete(TSQLQuery(FDataSource.DataSet).SQL.Count - 1);
    TSQLQuery(FDataSource.DataSet).SQL.Add('ORDER BY carrier_name ASC, b.band_size ASC, b.band_number ASC');
    {$IFDEF DEBUG}
    LogSQL(TSQLQuery(FDataSource.DataSet).SQL);
    {$ENDIF}
    FDataSource.DataSet.Open;
  end;

  frDataSet.DataSource := FDataSource;
end;

procedure TfrmPrintPreview.SetDetailSource(AValue: TDataSource);
begin
  FDetailSource := AValue;
  //if Assigned(FDetailSource) then
  //  qPrintDetail.SQL.Text := TSQLQuery(FDetailSource.DataSet).SQL.Text;
  frDetails.DataSource := FDetailSource;
end;

procedure TfrmPrintPreview.SetReportName(AValue: String);
begin
  FReportName := ConcatPaths([InstallDir, 'reports\', AValue]);
  if not FileExists(FReportName) then
    MsgDlg(rsTitleError, Format(rsErrorReportNotFound, [AValue]), mtError);
end;

procedure TfrmPrintPreview.tbZoomChange(Sender: TObject);
begin
  lblZoom.Caption := IntToStr(tbZoom.Position) + '%';
  frPreview.Zoom := tbZoom.Position;

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
  sbZoomOut.Enabled := (frPreview.AllPages > 0) or (tbZoom.Position > tbZoom.Min);
  sbZoomIn.Enabled := (frPreview.AllPages > 0) or (tbZoom.Position < tbZoom.Max);
  tbZoom.Enabled := frPreview.AllPages > 0;
end;

end.

