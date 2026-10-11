{ Xolmis Detection Type Editor dialog

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

unit ulst_detectiontype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst, Buttons,
  atshapelinebgra, fpjson, jsonparser;

type

  { TlstDetectionType }

  TlstDetectionType = class(TForm)
    cklDetect: TCheckListBox;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    strJson: String;
    procedure ApplyDarkMode;
  public
    property JsonString: String read strJson write strJson;
  end;

var
  lstDetectionType: TlstDetectionType;

implementation

uses utils_locale, utils_global, utils_dialogs, utils_themes, uDarkStyleParams;

{$R *.lfm}

{ TlstDetectionType }

procedure TlstDetectionType.ApplyDarkMode;
begin

end;

procedure TlstDetectionType.FormCreate(Sender: TObject);
begin
  JsonString := '';
end;

procedure TlstDetectionType.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TlstDetectionType.FormShow(Sender: TObject);
var
  Arr: TJSONArray;
  i: Integer;
  Value: String;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cklDetect.Items.CommaText := rsDetectionTypes;

  if JsonString <> '' then
  begin
    Arr := GetJSON(JsonString) as TJSONArray;
    try
      for i := 0 to Arr.Count - 1 do
      begin
        Value := Arr.Strings[i];

        case Value of
          'visual':        cklDetect.Checked[0] := True;
          'song':          cklDetect.Checked[1] := True;
          'call':          cklDetect.Checked[2] := True;
          'wing_flapping': cklDetect.Checked[3] := True;
          'drumming':      cklDetect.Checked[4] := True;
          'capture':       cklDetect.Checked[5] := True;
          'remote':        cklDetect.Checked[6] := True;
          'track':         cklDetect.Checked[7] := True;
          'other':         cklDetect.Checked[8] := True;
        end;
      end;
    finally
      Arr.Free;
    end;
  end;
end;

procedure TlstDetectionType.sbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TlstDetectionType.sbOKClick(Sender: TObject);
var
  Arr: TJSONArray;
begin
  Arr := TJSONArray.Create;
  try
    if cklDetect.Checked[0] then
      Arr.Add('visual');

    if cklDetect.Checked[1] then
      Arr.Add('song');

    if cklDetect.Checked[2] then
      Arr.Add('call');

    if cklDetect.Checked[3] then
      Arr.Add('wing_flapping');

    if cklDetect.Checked[4] then
      Arr.Add('drumming');

    if cklDetect.Checked[5] then
      Arr.Add('capture');

    if cklDetect.Checked[6] then
      Arr.Add('remote');

    if cklDetect.Checked[7] then
      Arr.Add('track');

    if cklDetect.Checked[8] then
      Arr.Add('other');

    JsonString := Arr.AsJSON;
  finally
    Arr.Free;
  end;

  ModalResult := mrOK;
end;

end.

