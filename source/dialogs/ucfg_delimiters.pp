{ Xolmis Delimited File Settings dialog

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

unit ucfg_delimiters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ToggleSwitch, atshapelinebgra,
  BCPanel;

type

  { TcfgDelimiters }

  TcfgDelimiters = class(TForm)
    pDecimalSeparator: TBCPanel;
    cbDecimalSeparator: TComboBox;
    lblDecimalSeparator: TLabel;
    pDelimiter: TBCPanel;
    cbDelimiter: TComboBox;
    eOther: TEdit;
    lblDelimiter: TLabel;
    pQuotedAsText: TBCPanel;
    lblQuotedAsText: TLabel;
    pHaveHeader: TBCPanel;
    lblHaveHeader: TLabel;
    lblTitleFields: TLabel;
    lblTitleNumbers: TLabel;
    lineBottom: TShapeLineBGRA;
    pClient: TPanel;
    pBottom: TPanel;
    sbCancel: TButton;
    sbOK: TButton;
    tsQuotedAsText: TToggleSwitch;
    tsHaveHeader: TToggleSwitch;
    procedure cbDelimiterSelect(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    FDelimiter, FDecimal: Char;
    FQuotes, FHeader: Boolean;
    procedure ApplyDarkMode;
    procedure SetQuotedAsText(aValue: Boolean);
    procedure SetDelimiter(aValue: Char);
    procedure SetDecimalSeparator(aValue: Char);
    procedure SetHaveHeader(aValue: Boolean);
  public
    property QuotedAsText: Boolean read FQuotes write FQuotes default False;
    property Delimiter: Char read FDelimiter write FDelimiter default ';';
    property DecimalSeparator: Char read FDecimal write FDecimal default ',';
    property HaveHeader: Boolean read FHeader write FHeader default True;
  end;

var
  cfgDelimiters: TcfgDelimiters;

implementation

uses cbs_global, cbs_graphics, cbs_themes, uDarkStyleParams;

{$R *.lfm}

{ TcfgDelimiters }

procedure TcfgDelimiters.ApplyDarkMode;
begin
  pHaveHeader.Background.Color := clCardBGDefaultDark;
  pHaveHeader.Border.Color := clSystemSolidNeutralFGDark;
  pQuotedAsText.Background.Color := clCardBGDefaultDark;
  pQuotedAsText.Border.Color := clSystemSolidNeutralFGDark;
  pDelimiter.Background.Color := clCardBGDefaultDark;
  pDelimiter.Border.Color := clSystemSolidNeutralFGDark;
  pDecimalSeparator.Background.Color := clCardBGDefaultDark;
  pDecimalSeparator.Border.Color := clSystemSolidNeutralFGDark;

  tsHaveHeader.Color := pHaveHeader.Background.Color;
  tsQuotedAsText.Color := pQuotedAsText.Background.Color;
end;

procedure TcfgDelimiters.cbDelimiterSelect(Sender: TObject);
begin
  eOther.Visible := cbDelimiter.ItemIndex = 3;
end;

procedure TcfgDelimiters.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, TComponent(Sender).Name, 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TcfgDelimiters.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  { #todo : Get the values from config }
  tsQuotedAsText.Checked := FQuotes;

  case FDelimiter of
    ';': cbDelimiter.ItemIndex := 0;
    ',': cbDelimiter.ItemIndex := 1;
    #9 : cbDelimiter.ItemIndex := 2;
  else
    cbDelimiter.ItemIndex := 3;
    eOther.Visible := True;
    eOther.Text := FDelimiter;
  end;

  tsHaveHeader.Checked := FHeader;

  case FDecimal of
    ',': cbDecimalSeparator.ItemIndex := 0;
    '.': cbDecimalSeparator.ItemIndex := 1;
  end;

end;

procedure TcfgDelimiters.sbOKClick(Sender: TObject);
var
  cOther: Char;
  sOther: String;
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');

  { #todo : Validate values }

  { Double quoted values as text }
  QuotedAsText := tsQuotedAsText.Checked;
  { Delimiter }
  cOther := #0;
  if (cbDelimiter.ItemIndex = 3) and (Length(Trim(eOther.Text)) > 0) then
  begin
    sOther := eOther.Text;
    cOther := sOther[1];
  end;
  case cbDelimiter.ItemIndex of
    0: Delimiter := ';'; { semicolon }
    1: Delimiter := ','; { comma }
    2: Delimiter := #9;  { <Tab> }
    3: Delimiter := cOther;   { other delimiter }
  end;
  { Decimal separator }
  case cbDecimalSeparator.ItemIndex of
    0: DecimalSeparator := ',';  { comma }
    1: DecimalSeparator := '.';  { period/point }
  end;
  { Have a header line with column names }
  HaveHeader := tsHaveHeader.Checked;

  ModalResult := mrOK;
end;

procedure TcfgDelimiters.SetQuotedAsText(aValue: Boolean);
begin
  FQuotes := aValue;
  tsQuotedAsText.Checked := FQuotes;
end;

procedure TcfgDelimiters.SetDelimiter(aValue: Char);
begin
  FDelimiter := aValue;
  case FDelimiter of
    ';': cbDelimiter.ItemIndex := 0;
    ',': cbDelimiter.ItemIndex := 1;
    #9 : cbDelimiter.ItemIndex := 2;
  else
    cbDelimiter.ItemIndex := 3;
    eOther.Visible := True;
    eOther.Text := FDelimiter;
  end;
end;

procedure TcfgDelimiters.SetDecimalSeparator(aValue: Char);
begin
  FDecimal := aValue;
  case FDecimal of
    ',': cbDecimalSeparator.ItemIndex := 0;
    '.': cbDecimalSeparator.ItemIndex := 1;
  end;
end;

procedure TcfgDelimiters.SetHaveHeader(aValue: Boolean);
begin
  FHeader := aValue;
  tsHaveHeader.Checked := FHeader;
end;

end.

