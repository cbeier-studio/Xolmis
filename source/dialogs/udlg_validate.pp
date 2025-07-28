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
    FMsgList: TStrings;
    FHtml: TStrings;
    FHeader: String;
  public
    property Header: String read FHeader write FHeader;
    property MessageList: TStrings read FMsgList write FMsgList;
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
  FHtml.Add('<body>');
  if FHeader <> EmptyStr then
  begin
    FHtml.Add('<p>' + FHeader + '</p>');
    FHtml.Add('<br>');
  end
  else
  begin
    if FMsgList.Count > 1 then
      FHtml.Add(Format(rsErrorsFound, [FMsgList.Count]))
    else
      FHtml.Add(Format(rsErrorFound, [FMsgList.Count]));
  end;

  FHtml.Add('<ul>');
  for i := 0 to FMsgList.Count - 1 do
  begin
    FHtml.Add('<li>' + FMsgList[i] + '</li>');
  end;
  FHtml.Add('</ul>');
  FHtml.Add('</body>');

  LV.LoadFromString(FHtml.Text);
  LV.Refresh;
end;

end.

