{ Xolmis Progress dialog

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

unit udlg_progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  atshapelinebgra;

type

  { TdlgProgress }

  TdlgProgress = class(TForm)
    sbCancel: TBitBtn;
    lineBottom: TShapeLineBGRA;
    lStatus: TLabel;
    lblTitle: TLabel;
    pClient: TPanel;
    PBar: TProgressBar;
    pBottom: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure sbCancelClick(Sender: TObject);
  private
    sTitle, sText: String;
    vMin, vMax, vPosition: Integer;
    bAllowCancel: Boolean;
    procedure SetTitle(aTitle: String);
    procedure SetText(aText: String);
    procedure SetMin(aMin: Integer);
    procedure SetMax(aMax: Integer);
    procedure SetPosition(aValue: Integer);
    procedure SetAllowCancel(aValue: Boolean);
  public
    property Title: String read sTitle write SetTitle;
    property Text: String read sText write SetText;
    property Min: Integer read vMin write SetMin default 0;
    property Max: Integer read vMax write SetMax default 100;
    property Position: Integer read vPosition write SetPosition default 0;
    property AllowCancel: Boolean read bAllowCancel write SetAllowCancel default True;
  end;

var
  dlgProgress: TdlgProgress;

implementation

uses cbs_global;

{$R *.lfm}

{ TdlgProgress }

procedure TdlgProgress.sbCancelClick(Sender: TObject);
begin
  Parar := True;
end;

procedure TdlgProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TdlgProgress.SetTitle(aTitle: String);
begin
  sTitle := aTitle;
  lblTitle.Caption := sTitle;
end;

procedure TdlgProgress.SetText(aText: String);
begin
  sText := aText;
  lStatus.Caption := sText;
end;

procedure TdlgProgress.SetMin(aMin: Integer);
begin
  vMin := aMin;
  PBar.Min := vMin;
end;

procedure TdlgProgress.SetMax(aMax: Integer);
begin
  vMax := aMax;
  PBar.Max := vMax;
end;

procedure TdlgProgress.SetPosition(aValue: Integer);
begin
  if aValue < vMin then
    aValue := vMin;
  if aValue > vMax then
    aValue := vMax;

  vPosition := aValue;
  PBar.Position := vPosition;
end;

procedure TdlgProgress.SetAllowCancel(aValue: Boolean);
begin
  bAllowCancel := aValue;
  lineBottom.Visible := bAllowCancel;
  pBottom.Visible := bAllowCancel;
end;

end.

