{ Xolmis Record Verifications dialog

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

unit udlg_recverifications;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, DBCtrls, StdCtrls,
  Buttons, Menus, data_types, Grids;

type

  { TdlgRecVerifications }

  TdlgRecVerifications = class(TForm)
    btnHelp: TSpeedButton;
    ckAtivo: TDBCheckBox;
    ckExportado: TDBCheckBox;
    ckMarcado: TDBCheckBox;
    dbgHistory: TDBGrid;
    dsHistory: TDataSource;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblRecordId: TLabel;
    pmmNewVerification: TMenuItem;
    pmmDelVerification: TMenuItem;
    pmGrid: TPopupMenu;
    sbDelRecord: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    Separator1: TMenuItem;
    txtRecordId: TDBText;
    pBottom: TPanel;
    pTop: TPanel;
    qHistory: TSQLQuery;
    qHistorynotes: TMemoField;
    qHistoryperson_id: TLongintField;
    qHistoryperson_name: TStringField;
    qHistoryrecord_id: TLongintField;
    qHistorytable_name: TStringField;
    qHistoryverification_date: TDateTimeField;
    qHistoryverification_id: TLongintField;
    qHistoryverification_status: TStringField;
    sbClose: TBitBtn;
    procedure btnHelpClick(Sender: TObject);
    procedure dbgHistoryPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure qHistoryverification_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qHistoryverification_statusSetText(Sender: TField; const aText: string);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
  private
    FTableType, FChildType: TTableType;
    FId: Integer;
    procedure ApplyDarkMode;
  public
    procedure LoadData;

    property TableType: TTableType read FTableType write FTableType default tbNone;
    property ChildType: TTableType read FChildType write FChildType default tbNone;
    property Id: Integer read FId write FId;
  end;

var
  dlgRecVerifications: TdlgRecVerifications;

implementation

uses
  utils_locale, utils_global, utils_system, utils_themes,
  data_columns, data_management, data_consts,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgRecVerifications }

procedure TdlgRecVerifications.ApplyDarkMode;
begin
  sbInsertRecord.Images := iButtonsDark;
  sbDelRecord.Images := iButtonsDark;
  btnHelp.Images := DMM.iEditsDark;

  pmGrid.Images := iButtonsDark;

  txtRecordId.Font.Color := clVioletFG1Dark;
end;

procedure TdlgRecVerifications.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_RECORD_VERIFICATIONS);
end;

procedure TdlgRecVerifications.dbgHistoryPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
begin
  if (Column.FieldName = COL_VERIFICATION_STATUS) then
  begin
    case Column.Field.AsString of
      'OK':     // Record OK
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
      'WT', 'WL', 'WC', 'WM', 'WV':  // Wrong value
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'MD':     // Missing data
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
    end;
  end;
end;

procedure TdlgRecVerifications.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  qHistory.Close;
end;

procedure TdlgRecVerifications.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CLOSE = Esc }
  if (Key = #27) then
  begin
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrClose;
  end;
end;

procedure TdlgRecVerifications.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  TranslateRecordVerifications(qHistory);
  LoadData;
end;

procedure TdlgRecVerifications.LoadData;
var
  DS: TDataSource = nil;
begin
  if qHistory.Active then
    qHistory.Close;

  DS := GetDataSource(FTableType, FChildType);

  if DS = nil then
    Exit;

  txtRecordId.DataSource := DS;
  txtRecordId.DataField := GetPrimaryKey(DS.DataSet);
  txtRecordId.Refresh;
  ckAtivo.DataSource := DS;
  ckMarcado.DataSource := DS;
  ckExportado.DataSource := DS;

  with qHistory do
  begin
    ParamByName('TABNAME').Bound := True;
    if FChildType <> tbNone then
      ParamByName('TABNAME').AsString := TABLE_NAMES[FChildType]
    else
      ParamByName('TABNAME').AsString := TABLE_NAMES[FTableType];
    ParamByName('COD').AsInteger := FId;
    //DataSource := DS;
    Open;
  end;
end;

procedure TdlgRecVerifications.qHistoryverification_statusGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  case Sender.AsString of
    'OK': aText := rsRecordOK;
    'WT': aText := rsWrongTaxon;
    'WL': aText := rsWrongLocation;
    'WC': aText := rsWrongCoordinates;
    'WM': aText := rsWrongMeasurement;
    'WV': aText := rsWrongValues;
    'MD': aText := rsMissingData;
  end;

  DisplayText := True;
end;

procedure TdlgRecVerifications.qHistoryverification_statusSetText(Sender: TField; const aText: string);
begin
  if aText = rsRecordOK then
    Sender.AsString := 'OK'
  else
  if aText = rsWrongTaxon then
    Sender.AsString := 'WT'
  else
  if aText = rsWrongLocation then
    Sender.AsString := 'WL'
  else
  if aText = rsWrongCoordinates then
    Sender.AsString := 'WC'
  else
  if aText = rsWrongMeasurement then
    Sender.AsString := 'WM'
  else
  if aText = rsWrongValues then
    Sender.AsString := 'WV'
  else
  if aText = rsMissingData then
    Sender.AsString := 'MD';
end;

procedure TdlgRecVerifications.sbDelRecordClick(Sender: TObject);
begin
  // Confirmation dialog
  with DMM.TaskDlg do
  begin
    Title := rsDeleteRecordTitle;
    Text := rsDeleteRecordPrompt;
    Caption := rsTitleConfirmation;
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiNone;
    DefaultButton := tcbNo;
    if Execute then
      if ModalResult = mrNo then
        Exit;
  end;

  qHistory.Delete;

  //qHistory.Refresh;
end;

procedure TdlgRecVerifications.sbInsertRecordClick(Sender: TObject);
begin
  AddVerification(FTableType, FChildType, FId);

  qHistory.Refresh;
end;

end.

