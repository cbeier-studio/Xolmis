{ Xolmis Authorship Editor dialog

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

unit udlg_authorship;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, LCLType,
  Character, cbs_system, cbs_sampling;

type

  { TdlgAuthorship }

  TdlgAuthorship = class(TForm)
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblTotal: TLabel;
    lbColetor: TListBox;
    pBottom: TPanel;
    pRight: TPanel;
    sbCancel: TBitBtn;
    sbDel: TSpeedButton;
    sbAdd: TSpeedButton;
    sbDown: TSpeedButton;
    sbOK: TBitBtn;
    sbUp: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lbColetorClick(Sender: TObject);
    procedure lbColetorKeyPress(Sender: TObject; var Key: char);
    procedure sbAddClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbDelClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
  private
    xLinha: String;
    xLimite: Integer;
    xAutores: TAuthors;
    procedure ApplyDarkMode;
    procedure HabilitaBotoes;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property Linha: String read xLinha write xLinha;
    property Autores: TAuthors read xAutores write xAutores;
    property Limite: Integer read xLimite write xLimite;
    procedure SetAutores(aAutores: TAuthors);
  end;

var
  dlgAuthorship: TdlgAuthorship;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_getvalue, cbs_themes, udlg_find, uDarkStyleParams;

{$R *.lfm}

{ TdlgAuthorship }

procedure TdlgAuthorship.ApplyDarkMode;
begin
  sbAdd.Images := iButtonsDark;
  sbDel.Images := iButtonsDark;
  sbUp.Images := iButtonsDark;
  sbDown.Images := iButtonsDark;
end;

procedure TdlgAuthorship.FormCreate(Sender: TObject);
begin
  lbColetor.Items.QuoteChar := #0;
  lbColetor.Items.Delimiter := ';';
  lbColetor.Items.StrictDelimiter := True;
end;

procedure TdlgAuthorship.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
  begin
    if (Key = VK_DOWN) then { MOVER PARA BAIXO = Ctrl + Down }
    begin
      GravaStat(Name, '', 'Ctrl+Down');
      if sbDown.Enabled then
        sbDownClick(nil);
      Key := 0;
    end;
    if (Key = VK_UP) then { MOVER PARA CIMA = Ctrl + Up }
    begin
      GravaStat(Name, '', 'Ctrl+Up');
      if sbUp.Enabled then
        sbUpClick(nil);
      Key := 0;
    end;
  end;
  if (Key = VK_DELETE) then { APAGAR = Delete }
  begin
    GravaStat(Name, '', 'Delete');
    if sbDel.Enabled then
      sbDelClick(nil);
    Key := 0;
  end;
end;

procedure TdlgAuthorship.FormShow(Sender: TObject);
var
  i: Integer;
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  if Length(Linha) > 0 then
  begin
    StringToAuthorList(Linha, xAutores);
    for i := 0 to High(Autores) do
      lbColetor.Items.Add(Autores[i].Citation);
  end;
  if Length(Autores) > 0 then
    for i := 0 to High(Autores) do
      lbColetor.Items.Add(Autores[i].Citation);
  if Limite = 0 then
    Limite := 300;

  // Posição na tela
  //PositionWindow(WindowPos, Self);

  if lbColetor.Items.Count > 0 then
    lbColetor.ItemIndex := 0;
  lbColetor.SetFocus;
  HabilitaBotoes;
end;

procedure TdlgAuthorship.lbColetorClick(Sender: TObject);
begin
  GravaStat(Name, 'lbColetor', 'click');
  HabilitaBotoes;
end;

procedure TdlgAuthorship.lbColetorKeyPress(Sender: TObject; var Key: char);
var
  PControl: TPoint;
begin
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key))
  then
  begin
    if (SBAdd.Enabled) then
    begin
      LogEvent(leaOpen, 'Find (people)');
      dlgFind := TdlgFind.Create(nil);
      with dlgFind do
        try
          TableType := tbPeople;
          InitialValue := Key;
          PControl := sbAdd.ClientToScreen(Point(sbAdd.Left, sbAdd.Top));
          SetDialogPosition(PControl.X, PControl.Y, sbAdd.Width, sbAdd.Height);
          if ShowModal = mrOK then
          begin
            if (lbColetor.Items.Count < Limite) then
            begin
              if (lbColetor.Items.IndexOf(NameSelected) < 0) then
                lbColetor.Items.Add(NameSelected);
              lbColetor.ItemIndex := lbColetor.Items.IndexOf(NameSelected);
            end;
          end;
        finally
          FreeAndNil(dlgFind);
          LogEvent(leaClose, 'Find (people)');
        end;
      lbColetor.SetFocus;
      HabilitaBotoes;
    end;
    Key := #0;
  end;
  { CANCEL = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { APPLY = Enter/Return }
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

procedure TdlgAuthorship.sbAddClick(Sender: TObject);
var
  PControl: TPoint;
begin
  GravaStat(Name, 'sbAdd', 'click');
  LogEvent(leaOpen, 'Find (people)');
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbPeople;
    PControl := sbAdd.ClientToScreen(Point(sbAdd.Left, sbAdd.Top));
    SetDialogPosition(PControl.X, PControl.Y, sbAdd.Width, sbAdd.Height);
    if ShowModal = mrOK then
    begin
      if (lbColetor.Items.Count < Limite) then
      begin
        if (lbColetor.Items.IndexOf(NameSelected) < 0) then
          lbColetor.Items.Add(NameSelected);
        lbColetor.ItemIndex := lbColetor.Items.IndexOf(NameSelected);
      end;
    end;
  finally
    FreeAndNil(dlgFind);
    LogEvent(leaClose, 'Find (people)');
  end;
  lbColetor.SetFocus;
  HabilitaBotoes;
end;

procedure TdlgAuthorship.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'sbCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TdlgAuthorship.sbDelClick(Sender: TObject);
var
  idx: Integer;
begin
  GravaStat(Name, 'sbDel', 'click');
  with lbColetor, Items do
  if Count > 0 then
  begin
    idx := ItemIndex;
    Delete(lbColetor.ItemIndex);
    if (Count > 0) then
      if idx < Count then
        ItemIndex := idx
      else
        ItemIndex := idx - 1;
    SetFocus;
    HabilitaBotoes;
  end;
end;

procedure TdlgAuthorship.sbDownClick(Sender: TObject);
var
  idx: Integer;
begin
  GravaStat(Name, 'sbDown', 'click');
  with lbColetor, Items do
  begin
    if (Count > 0) and (ItemIndex < Count) then
    begin
      idx := ItemIndex;
      Move(idx, idx + 1);
      ItemIndex := idx + 1;
      SetFocus;
      HabilitaBotoes;
    end;
  end;
end;

procedure TdlgAuthorship.sbOKClick(Sender: TObject);
var
  i: Integer;
begin
  GravaStat(Name, 'sbOK', 'click');

  Linha := AuthorListToString(Autores);
  SetLength(xAutores, lbColetor.Count);
  for i := 0 to lbColetor.Count - 1 do
  begin
    Autores[i].Citation := lbColetor.Items[i];
    Autores[i].Id := GetKey('people', 'person_id', 'citation', lbColetor.Items[i]);
  end;

  ModalResult := mrOK;
end;

procedure TdlgAuthorship.sbUpClick(Sender: TObject);
var
  idx: Integer;
begin
  GravaStat(Name, 'sbUp', 'click');
  if (lbColetor.Items.Count > 0) and (lbColetor.ItemIndex >= 0) then
  begin
    idx := lbColetor.ItemIndex;
    lbColetor.Items.Move(idx, idx - 1);
    lbColetor.ItemIndex := idx - 1;
    lbColetor.SetFocus;
    HabilitaBotoes;
  end;
end;

procedure TdlgAuthorship.HabilitaBotoes;
begin
  sbAdd.Enabled := (lbColetor.Items.Count < Limite);
  sbDel.Enabled := (lbColetor.Items.Count > 0) and not(lbColetor.ItemIndex = -1);
  sbUp.Enabled := (lbColetor.Items.Count > 0) and not(lbColetor.Selected[0]) and
    not(lbColetor.ItemIndex = -1);
  sbDown.Enabled := (lbColetor.Items.Count > 0) and
    not(lbColetor.Selected[lbColetor.Items.Count - 1]) and not(lbColetor.ItemIndex = -1);
  lblTotal.Caption := Format(rsTotalResearchers, [lbColetor.Items.Count]);
  if lbColetor.Items.Count = Limite then
  begin
    lblTotal.Font.Color := clSystemCriticalFGLight;
    lblTotal.ShowHint := True;
  end
  else
  begin
    lblTotal.Font.Color := clTextPrimaryLight;
    lblTotal.ShowHint := False;
  end;
end;

procedure TdlgAuthorship.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Width then
    Width := ControlWidth;

  if (X + Width) > Screen.Width then
    Left := (X + ControlWidth) - Width
  else
    Left := X;

  if (Y + ControlHeight + Height) > (Screen.WorkAreaHeight) then
    Top := Y - Height
  else
    Top := Y + ControlHeight;
end;

procedure TdlgAuthorship.SetAutores(aAutores: TAuthors);
begin
  SetLength(xAutores, Length(aAutores));
  xAutores := Copy(aAutores);
end;

end.

