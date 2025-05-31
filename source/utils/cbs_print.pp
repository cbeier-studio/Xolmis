{ Xolmis Print library

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
      ReportName := aReportFile;
      DataSource := aDataSource;
      DetailSource := aDetailSource;

      ShowModal;
    finally
      FreeAndNil(frmPrintPreview);
    end;
  end
  else
    MsgDlg(rsPrintRecordsTitle, rsNothingToPrint, mtInformation);
end;

end.

