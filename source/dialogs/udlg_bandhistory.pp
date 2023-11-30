unit udlg_bandhistory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, StdCtrls, DBCtrls,
  atshapelinebgra;

type

  { TdlgBandHistory }

  TdlgBandHistory = class(TForm)
    DBG: TDBGrid;
    lblBandNameCaption: TLabel;
    lblBandName: TDBText;
    pHeader: TPanel;
    pBottom: TPanel;
    sbClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  dlgBandHistory: TdlgBandHistory;

implementation

uses cbs_locale, udm_grid;

{$R *.lfm}

{ TdlgBandHistory }

procedure TdlgBandHistory.FormCreate(Sender: TObject);
begin
  DMG.qBandHistory.Open;
end;

procedure TdlgBandHistory.FormDestroy(Sender: TObject);
begin
  DMG.qBandHistory.Close;
end;

procedure TdlgBandHistory.FormShow(Sender: TObject);
begin
  DBG.Columns.ColumnByFieldname('event_type').PickList.CommaText := rsBandEventTypeList;
end;

end.

