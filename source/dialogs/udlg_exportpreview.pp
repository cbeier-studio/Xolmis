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
  cbs_dialogs;

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

