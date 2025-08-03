{ Xolmis Bands Balance dialog

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_bandsbalance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, DB, SQLDB, fpcsvexport, atshapelinebgra,
  BCPanel, Grids, DBGrids, Menus, StdCtrls, LR_PGrid;

type

  { TdlgBandsBalance }

  TdlgBandsBalance = class(TForm)
    CSVExporter1: TCSVExporter;
    dbgSaldo: TDBGrid;
    dsBandsBalance: TDataSource;
    FrPrintGrid1: TFrPrintGrid;
    iPopup: TImageList;
    iPopupDark: TImageList;
    lineBottom: TShapeLineBGRA;
    pmgPrint: TMenuItem;
    pMsg: TBCPanel;
    pmgRefresh: TMenuItem;
    pmgSavesAs: TMenuItem;
    pBottom: TPanel;
    pmGrid: TPopupMenu;
    qBandsBalance: TSQLQuery;
    qBandsBalanceband_size: TStringField;
    qBandsBalancemaximo_dia: TLongintField;
    qBandsBalancemedia_dia: TFloatField;
    qBandsBalancesaldo: TLongintField;
    SaveDlg: TSaveDialog;
    sbClose: TButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TimerLoad: TTimer;
    procedure dbgSaldoPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
      AState: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmgPrintClick(Sender: TObject);
    procedure pmgRefreshClick(Sender: TObject);
    procedure pmgSavesAsClick(Sender: TObject);
    procedure TimerLoadTimer(Sender: TObject);
  private
    MediaDiasExpedicao: Integer;
    procedure ApplyDarkMode;
  public

  end;

var
  dlgBandsBalance: TdlgBandsBalance;

implementation

uses
  cbs_locale, cbs_global, cbs_themes, udm_main, udlg_loading, uDarkStyleParams;

{$R *.lfm}

{ TdlgBandsBalance }

procedure TdlgBandsBalance.TimerLoadTimer(Sender: TObject);
begin
  TimerLoad.Enabled := False;
  //pMsg.Visible := True;
  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsCalculatingBandBalance, -1);

  with TSQLQuery.Create(nil) do
  try
    Database := DMM.sqlCon;
    SQL.Add('SELECT media_dias_expedicao FROM get_average_expedition_duration');
    Open;
    MediaDiasExpedicao := Fields[0].AsInteger;
    Close;
  finally
    Free;
  end;

  if qBandsBalance.Active then
    qBandsBalance.Refresh
  else
    qBandsBalance.Open;

  dlgLoading.Hide;
  //pMsg.Visible := False;
end;

procedure TdlgBandsBalance.FormDestroy(Sender: TObject);
begin
  TimerLoad.Enabled := False;

  qBandsBalance.Close;
end;

procedure TdlgBandsBalance.ApplyDarkMode;
begin
  pMsg.Background.Color := clCardBGDefaultDark;
  pMsg.Border.Color := clCardBGSecondaryDark;
  pMsg.FontEx.Color := clTextPrimaryDark;
  pMsg.Color := dbgSaldo.Color;

  pmGrid.Images := iPopupDark;
end;

procedure TdlgBandsBalance.dbgSaldoPrepareCanvas(sender: TObject; DataCol: Integer;
  Column: TColumn; AState: TGridDrawState);
var
  BandAvgPerDay: Integer;
  vSaldo: Integer;
begin
  //vSaldo := 0;
  //if Column.FieldName = 'saldo' then
  //begin
    vSaldo := dbgSaldo.Columns[1].Field.AsInteger;
    BandAvgPerDay := Round(qBandsBalance.FieldByName('media_dia').AsFloat * MediaDiasExpedicao);
    if vSaldo < BandAvgPerDay then
    begin
      with (Sender as TDBGrid) do
      begin
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
        if vSaldo = 0 then
        begin
          if IsDarkModeEnabled then
          begin
            Canvas.Brush.Color := clSystemCriticalBGDark;
            Canvas.Font.Color := clSystemCriticalFGDark;
          end
          else
          begin
            Canvas.Brush.Color := clSystemCriticalBGLight;
            Canvas.Font.Color := clSystemCriticalFGLight;
          end;
        end else
        begin
          if IsDarkModeEnabled then
          begin
            Canvas.Brush.Color := clSystemCautionBGDark;
            Canvas.Font.Color := clSystemCautionFGDark;
          end
          else
          begin
            Canvas.Brush.Color := clSystemCautionBGLight;
            Canvas.Font.Color := clSystemCautionFGLight;
          end;
        end;
      end;
    end;
  //end;
end;

procedure TdlgBandsBalance.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  TimerLoad.Enabled := True;
end;

procedure TdlgBandsBalance.pmgPrintClick(Sender: TObject);
begin
  frPrintGrid1.Caption := Caption;
  frPrintGrid1.PreviewReport;
end;

procedure TdlgBandsBalance.pmgRefreshClick(Sender: TObject);
begin
  TimerLoad.Enabled := True;
end;

procedure TdlgBandsBalance.pmgSavesAsClick(Sender: TObject);
begin
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  if SaveDlg.Execute then
  begin
    CSVExporter1.FileName := SaveDlg.FileName;
    CSVExporter1.Execute;
    //dbgSaldo.SaveToFile(SaveDlg.FileName);
  end;
end;

end.

