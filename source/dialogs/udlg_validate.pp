{ Xolmis Data Validation dialog

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

unit udlg_validate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Pixie.HtmlView, atshapelinebgra;

type

  { TdlgValidate }

  TdlgValidate = class(TForm)
    lineBottom: TShapeLineBGRA;
    LV: TPixieHtmlView;
    sbOK: TButton;
    pBottom: TPanel;
    procedure sbOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FMsgList: TStrings;
    FHtml: TStrings;
    FHeader: String;
    procedure ApplyDarkMode;
  public
    property Header: String read FHeader write FHeader;
    property MessageList: TStrings read FMsgList write FMsgList;
  end;

var
  dlgValidate: TdlgValidate;

implementation

uses utils_locale, utils_global, uDarkStyleParams, Pixie.Types;

{$R *.lfm}

{ TdlgValidate }

procedure TdlgValidate.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBOK', 'click');
  ModalResult := mrOK;
end;

procedure TdlgValidate.ApplyDarkMode;
begin
  LV.ColorScheme := pcsDark;
end;

procedure TdlgValidate.FormCreate(Sender: TObject);
begin
  FMsgList := TStringList.Create;
  FHtml := TStringList.Create;
end;

procedure TdlgValidate.FormDestroy(Sender: TObject);
begin
  FMsgList.Free;
  FHtml.Free;
end;

procedure TdlgValidate.FormKeyPress(Sender: TObject; var Key: char);
begin
  { FECHAR = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, '', 'Esc');
    Key := #0;
    ModalResult := mrOK;
  end;
end;

procedure TdlgValidate.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FHtml.Add('<!DOCTYPE html>');
  FHtml.Add('<html><head><style>');
  FHtml.Add('  body {');
  FHtml.Add('    font-family: Segoe UI, Arial, sans-serif;');
  FHtml.Add('    font-size: 14px;');
  if IsDarkModeEnabled then
  begin
    FHtml.Add('    background-color: #1c1c1c;');
    FHtml.Add('    color: #ffffff;');
  end
  else
  begin
    FHtml.Add('    background-color: #ffffff;');
    FHtml.Add('    color: #000000;');
  end;
  FHtml.Add('  }');
  FHtml.Add('  .alert {');
  if IsDarkModeEnabled then
  begin
    FHtml.Add('    background-color: #433519;');
    FHtml.Add('    color: #fce100;');
  end
  else
  begin
    FHtml.Add('    background-color: #fff4ce;');
    FHtml.Add('    color: #9d5d00;');
  end;
  FHtml.Add('    padding: 10px 12px;');
  FHtml.Add('    margin-bottom: 10px;');
  FHtml.Add('    width: 100%;');
  FHtml.Add('  }');
  FHtml.Add('</style></head>');
  FHtml.Add('<body>');
  if FHeader <> EmptyStr then
  begin
    FHtml.Add('<div class="alert"><p>' + FHeader + '</p></div>');
    FHtml.Add('<br>');
  end
  else
  begin
    FHtml.Add('<div class="alert">');
    if FMsgList.Count > 1 then
      FHtml.Add(Format(rsErrorsFound, [FMsgList.Count]))
    else
      FHtml.Add(Format(rsErrorFound, [FMsgList.Count]));
    FHtml.Add('</div>');
  end;

  FHtml.Add('<ul>');
  for i := 0 to FMsgList.Count - 1 do
  begin
    FHtml.Add('<li>' + FMsgList[i] + '</li>');
  end;
  FHtml.Add('</ul>');
  FHtml.Add('</body>');
  FHtml.Add('</html>');

  LogDebug(FHtml.Text);

  LV.LoadFromString(FHtml.Text);
  //LV.Refresh;
end;

end.

