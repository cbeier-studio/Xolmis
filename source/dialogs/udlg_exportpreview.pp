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
    FrPrintGrid1: TFrPrintGrid;
    iButtons: TImageList;
    pToolbar: TPanel;
    sbClose: TSpeedButton;
    sbPrint: TSpeedButton;
    sbExport: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
  private

  public

  end;

var
  dlgExportPreview: TdlgExportPreview;

implementation

{$R *.lfm}

{ TdlgExportPreview }

procedure TdlgExportPreview.FormShow(Sender: TObject);
begin
  gridPreview.AutoAdjustColumns;
end;

procedure TdlgExportPreview.sbCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TdlgExportPreview.sbPrintClick(Sender: TObject);
begin
  FrPrintGrid1.PreviewReport;
end;

end.

