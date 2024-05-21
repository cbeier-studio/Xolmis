{ Xolmis How Was Aged/Sexed Editor dialog

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

unit ulst_howsexedaged;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst, Buttons,
  atshapelinebgra, BCPanel;

type

  { TlstHowSexedAged }

  TlstHowSexedAged = class(TForm)
    cklAged: TCheckListBox;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pCode: TBCPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure cklAgedClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xDeterm: String;
    function HowAgedCode: String;
  public
    property HowAged: String read xDeterm write xDeterm;
  end;

var
  lstHowSexedAged: TlstHowSexedAged;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{$R *.lfm}

{ TlstHowSexedAged }

procedure TlstHowSexedAged.cklAgedClickCheck(Sender: TObject);
begin
  GravaStat(Name, 'cklAged', 'clickcheck');
  pCode.Caption := HowAgedCode;
end;

procedure TlstHowSexedAged.FormCreate(Sender: TObject);
begin
  HowAged := '';

  cklAged.Header[0] := True;
  cklAged.Header[11] := True;
  cklAged.Header[19] := True;
  cklAged.Header[25] := True;
end;

procedure TlstHowSexedAged.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCELAR = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { APLICAR = Enter }
  if Key = #13 then
  begin
    GravaStat(Name, '', 'Enter');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Enter/Return');
    {$ENDIF}
    Key := #0;
    sbOKClick(nil);
  end;
end;

procedure TlstHowSexedAged.FormShow(Sender: TObject);
var
  C: Char;
  i: Integer;
begin
  cklAged.Items.CommaText := rsHowAgedSexedList;

  if Length(HowAged) > 0 then
  begin
    for i := 1 to Length(HowAged) do
    begin
      C := HowAged[i];
      case C of
        'B': cklAged.Checked[1] := True;
        'C': cklAged.Checked[2] := True;
        '@': cklAged.Checked[3] := True;
        'E': cklAged.Checked[4] := True;
        'I': cklAged.Checked[5] := True;
        'G': cklAged.Checked[6] := True;
        '$': cklAged.Checked[7] := True;
        'S': cklAged.Checked[8] := True;
        'Q': cklAged.Checked[9] := True;
        'Y': cklAged.Checked[10] := True;
        'K': cklAged.Checked[12] := True;
        'A': cklAged.Checked[13] := True;
        'F': cklAged.Checked[14] := True;
        'J': cklAged.Checked[15] := True;
        'M': cklAged.Checked[16] := True;
        'P': cklAged.Checked[17] := True;
        'L': cklAged.Checked[18] := True;
        'W': cklAged.Checked[20] := True;
        'V': cklAged.Checked[21] := True;
        'R': cklAged.Checked[22] := True;
        '=': cklAged.Checked[23] := True;
        '#': cklAged.Checked[24] := True;
        'O': cklAged.Checked[26] := True;
        'U': cklAged.Checked[27] := True;
        'X': cklAged.Checked[28] := True;
        'Z': cklAged.Checked[29] := True;
      end;
    end;
    pCode.Caption := HowAged;
  end;
end;

procedure TlstHowSexedAged.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstHowSexedAged.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if HowAgedCode = '' then
  begin
    MsgDlg('', rsListCheckedNone, mtError);
    Exit;
  end;

  HowAged := HowAgedCode;

  ModalResult := mrOK;
end;

function TlstHowSexedAged.HowAgedCode: String;
var
  L: String;
  i: Integer;
begin
  Result := '';

  L := '';
  for i := 0 to cklAged.Count - 1 do
    if cklAged.Checked[i] then
      case i of
        0:  ;
        1:  L := L + 'B';
        2:  L := L + 'C';
        3:  L := L + '@';
        4:  L := L + 'E';
        5:  L := L + 'I';
        6:  L := L + 'G';
        7:  L := L + '$';
        8:  L := L + 'S';
        9:  L := L + 'Q';
        10: L := L + 'Y';
        11: ;
        12: L := L + 'K';
        13: L := L + 'A';
        14: L := L + 'F';
        15: L := L + 'J';
        16: L := L + 'M';
        17: L := L + 'P';
        18: L := L + 'L';
        19: ;
        20: L := L + 'W';
        21: L := L + 'V';
        22: L := L + 'R';
        23: L := L + '=';
        24: L := L + '#';
        25: ;
        26: L := L + 'O';
        27: L := L + 'U';
        28: L := L + 'X';
        29: L := L + 'Z';
      end;

  Result := L;
end;

end.

