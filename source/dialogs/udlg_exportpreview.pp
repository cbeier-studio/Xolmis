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
  Classes, DB, DBGrids, ExtCtrls, LR_PGrid, SQLDB, SysUtils, Forms, Controls, Buttons,
  Graphics, Dialogs;

type

  { TdlgExportPreview }

  TdlgExportPreview = class(TForm)
    dsLink: TDataSource;
    gridPreview: TDBGrid;
    iButtonsDark: TImageList;
    printGrid: TFrPrintGrid;
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
    procedure ApplyDarkMode;
  public

  end;

var
  dlgExportPreview: TdlgExportPreview;

implementation

uses
  utils_dialogs;

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
begin
  printGrid.PreviewReport;
end;

end.

