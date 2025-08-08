{ Xolmis Record History dialog

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

unit udlg_rechistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, Buttons,
  StdCtrls, DBCtrls, data_types;

type

  { TdlgRecHistory }

  TdlgRecHistory = class(TForm)
    btnHelp: TSpeedButton;
    ckAtivo: TDBCheckBox;
    ckMarcado: TDBCheckBox;
    ckExportado: TDBCheckBox;
    mOldValue: TDBMemo;
    mNewValue: TDBMemo;
    mNotes: TDBMemo;
    dsHistory: TDataSource;
    lblOldValue: TLabel;
    lblNewValue: TLabel;
    lblNotes: TLabel;
    lblUsername: TLabel;
    txtRecordId: TDBText;
    lblRecordId: TLabel;
    txtUsername: TDBText;
    qHistoryevent_action: TStringField;
    qHistoryevent_date: TDateTimeField;
    qHistoryevent_field: TStringField;
    qHistorynew_value: TMemoField;
    qHistorynotes: TMemoField;
    qHistoryold_value: TMemoField;
    qHistoryuser_id: TLongintField;
    qHistoryuser_name: TStringField;
    sbClose: TBitBtn;
    dbgHistory: TDBGrid;
    pBottom: TPanel;
    pTop: TPanel;
    qHistory: TSQLQuery;
    SBox: TScrollBox;
    procedure btnHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure qHistoryevent_actionGetText(Sender: TField; var aText: string; DisplayText: Boolean);
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
  dlgRecHistory: TdlgRecHistory;

implementation

uses
  utils_locale, utils_global, data_management, data_columns, utils_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgRecHistory }

procedure TdlgRecHistory.ApplyDarkMode;
begin
  SBox.Color := clCardBGDefaultDark;
end;

procedure TdlgRecHistory.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_RECORD_HISTORY);
end;

procedure TdlgRecHistory.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  qHistory.Close;

  CloseAction := caFree;
end;

procedure TdlgRecHistory.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgRecHistory.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  TranslateRecordHistory(qHistory);
  LoadData;
end;

procedure TdlgRecHistory.qHistoryevent_actionGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if Sender.AsString = 'I' then
    aText := rsActionCreated
  else
  if Sender.AsString = 'U' then
    aText := rsActionEdited
  else
  if Sender.AsString = 'D' then
    aText := rsActionDeleted
  else
  if Sender.AsString = 'R' then
    aText := rsActionRestored;

  DisplayText := True;
end;

procedure TdlgRecHistory.LoadData;
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

end.

