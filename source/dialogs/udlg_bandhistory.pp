{ Xolmis Band History dialog

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

unit udlg_bandhistory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, StdCtrls, DBCtrls, Buttons;

type

  { TdlgBandHistory }

  TdlgBandHistory = class(TForm)
    btnHelp: TSpeedButton;
    DBG: TDBGrid;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    lblBandNameCaption: TLabel;
    lblBandName: TDBText;
    lblRequester: TDBText;
    lblSender: TDBText;
    lblSupplier: TDBText;
    mNotes: TDBMemo;
    pHeader: TPanel;
    pBottom: TPanel;
    sbClose: TButton;
    SBox: TScrollBox;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ApplyDarkMode;
  public

  end;

var
  dlgBandHistory: TdlgBandHistory;

implementation

uses
  utils_locale, utils_global, utils_themes, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TdlgBandHistory }

procedure TdlgBandHistory.ApplyDarkMode;
begin
  SBox.Color := clCardBGDefaultDark;
end;

procedure TdlgBandHistory.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_BANDS);
end;

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
  if IsDarkModeEnabled then
    ApplyDarkMode;

  DBG.Columns.ColumnByFieldname('event_type').PickList.CommaText := rsBandEventTypeList;
end;

end.

