unit cbs_print;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB;

  procedure PrintPreview(aDataSource: TDataSource; aReportFile: String);

implementation

uses
  ufrm_printpreview;

procedure PrintPreview(aDataSource: TDataSource; aReportFile: String);
begin
  frmPrintPreview := TfrmPrintPreview.Create(nil);
  with frmPrintPreview do
  try
    DataSource := aDataSource;
    ReportName := aReportFile;
    ShowModal;
  finally
    FreeAndNil(frmPrintPreview);
  end;
end;

end.

