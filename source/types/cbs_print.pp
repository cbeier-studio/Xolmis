unit cbs_print;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DB, LazFileUtils;

  procedure PrintPreview(aReportFile: String; aDataSource: TDataSource; aDetailSource: TDataSource = nil);

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, ufrm_printpreview;

procedure PrintPreview(aReportFile: String; aDataSource: TDataSource; aDetailSource: TDataSource);
begin

  if not FileExists(ConcatPaths([InstallDir, 'reports\', aReportFile])) then
  begin
    MsgDlg(rsTitleError, Format(rsErrorReportNotFound, [aReportFile]), mtError);
    Exit;
  end;

  if aDataSource.DataSet.RecordCount > 0 then
  begin
    frmPrintPreview := TfrmPrintPreview.Create(nil);
    with frmPrintPreview do
    try
      aDataSource.DataSet.DisableControls;
      ReportName := aReportFile;
      DataSource := aDataSource;
      DetailSource := aDetailSource;

      ShowModal;
    finally
      aDataSource.DataSet.EnableControls;
      FreeAndNil(frmPrintPreview);
    end;
  end
  else
    MsgDlg(rsPrintRecordsTitle, rsNothingToPrint, mtInformation);
end;

end.

