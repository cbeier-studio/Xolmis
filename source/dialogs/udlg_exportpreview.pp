{ Xolmis Export Preview Grid dialog

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

unit udlg_exportpreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, DB, DBGrids, ExtCtrls, SQLDB, SysUtils, Forms, Controls, Buttons,
  Graphics, Dialogs, xrs_renderer;

type

  { TdlgExportPreview }

  TdlgExportPreview = class(TForm)
    dsLink: TDataSource;
    gridPreview: TDBGrid;
    iButtonsDark: TImageList;
    iButtons: TImageList;
    pToolbar: TPanel;
    sbClose: TSpeedButton;
    sbPrint: TSpeedButton;
    sbExport: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbExportClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
  private
    FReportRenderer: TReportRenderer;
    FReportTitle: String;
    procedure ApplyDarkMode;
  public
    property ReportTitle: String read FReportTitle write FReportTitle;
  end;

var
  dlgExportPreview: TdlgExportPreview;

implementation

uses
  utils_locale, utils_global, utils_dialogs, xrs_simplelist;

{$R *.lfm}

{ TdlgExportPreview }

procedure TdlgExportPreview.ApplyDarkMode;
begin
  sbPrint.Images := iButtonsDark;
  sbExport.Images := iButtonsDark;
  sbClose.Images := iButtonsDark;
end;

procedure TdlgExportPreview.FormShow(Sender: TObject);
begin
  gridPreview.AutoAdjustColumns;
end;

procedure TdlgExportPreview.sbCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TdlgExportPreview.sbExportClick(Sender: TObject);
begin
  ExportDlg(dsLink.DataSet);
end;

procedure TdlgExportPreview.sbPrintClick(Sender: TObject);
var
  Report: TReportSimpleList;
begin
  //if not ActiveUser.HasPermission(PERM_BANDS_PRINT) or not qBandsBalance.Active or qBandsBalance.IsEmpty then
  //  Exit;

  Report := nil;
  try
    try
      Report := TReportSimpleList.Create(dsLink.DataSet, FReportTitle);
      if not Assigned(FReportRenderer) then
        FReportRenderer := TReportRenderer.Create;
      FReportRenderer.ShowPreview(Report);
    except
      on E: Exception do
      begin
        LogError(Format('Report generation failed (%s): %s', [E.ClassName, E.Message]));
        MsgDlg(rsTitleError, Format(rsErrorGeneratingReport, [FReportTitle]), mtError);
      end;
    end;
  finally
    FreeAndNil(Report);
  end;
end;

end.

