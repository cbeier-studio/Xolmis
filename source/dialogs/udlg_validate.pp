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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, atshapelinebgra, HtmlView;

type

  { TdlgValidate }

  TdlgValidate = class(TForm)
    LV: THtmlViewer;
    lineBottom: TShapeLineBGRA;
    sbOK: TButton;
    pBottom: TPanel;
    procedure sbOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    lst: TStrings;
    FList: TStrings;
  public
    property Lista: TStrings read lst write lst;
  end;

var
  dlgValidate: TdlgValidate;

implementation

uses cbs_locale, cbs_global;

{$R *.lfm}

{ TdlgValidate }

procedure TdlgValidate.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBOK', 'click');
  ModalResult := mrOK;
end;

procedure TdlgValidate.FormCreate(Sender: TObject);
begin
  lst := TStringList.Create;
  FList := TStringList.Create;
end;

procedure TdlgValidate.FormDestroy(Sender: TObject);
begin
  lst.Free;
  FList.Free;
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
  if lst.Count > 1 then
    FList.Add(Format(rsErrorsFound, [lst.Count]))
  else
    FList.Add(Format(rsErrorFound, [lst.Count]));

  FList.Add('<body>');
  FList.Add('<ul>');
  for i := 0 to lst.Count - 1 do
  begin
    FList.Add('<li>' {'<font color="$001C2BC4">' + #$26A0 + '</font> '} + lst[i] + '</li>');
    // Application.ProcessMessages;
  end;
  FList.Add('</ul>');
  FList.Add('</body>');

  LV.LoadFromString(FList.Text);
  LV.Refresh;
end;

end.

