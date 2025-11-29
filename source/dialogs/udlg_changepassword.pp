{ Xolmis Change Password dialog

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

unit udlg_changepassword;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, DBCtrls, DCPblowfish,
  DCPsha256, atshapelinebgra, StdCtrls, DB;

type

  { TdlgChangePassword }

  TdlgChangePassword = class(TForm)
    BCrypt: TDCP_blowfish;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblUsername: TDBText;
    eNewPassword: TEditButton;
    eConfirmPassword: TEditButton;
    imgLogin: TImage;
    lblNewPassword: TLabel;
    lblConfirmPassword: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure eNewPasswordButtonClick(Sender: TObject);
    procedure eNewPasswordChange(Sender: TObject);
    procedure eNewPasswordKeyPress(Sender: TObject; var Key: char);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    xPass: String;
    procedure ApplyDarkMode;
    function ValidatePassword: Boolean;
  public
    property Pass: String read xPass write xPass;
  end;

var
  dlgChangePassword: TdlgChangePassword;

implementation

uses
  utils_locale, utils_global, utils_graphics, utils_dialogs, utils_themes, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgChangePassword }

procedure TdlgChangePassword.FormDestroy(Sender: TObject);
begin
  eNewPassword.Clear;
  eConfirmPassword.Clear;
  Pass := EmptyStr;
end;

procedure TdlgChangePassword.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if Key = #27 then
  begin
    GravaStat(Name, TComponent(Sender).Name, 'Esc');
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TdlgChangePassword.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;
end;

procedure TdlgChangePassword.ApplyDarkMode;
begin
  eNewPassword.Images := iButtonsDark;
  eConfirmPassword.Images := iButtonsDark;

  lblUsername.Font.Color := clVioletFG1Dark;
end;

procedure TdlgChangePassword.eNewPasswordButtonClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'buttonclick');
  if Sender is TEditButton then
    TogglePassView(TEditButton(Sender));
end;

procedure TdlgChangePassword.eNewPasswordChange(Sender: TObject);
begin
  { Enable/disable Apply button }
  sbSave.Enabled := (Length(eNewPassword.Text) > 0) and (Length(eConfirmPassword.Text) > 0);
end;

procedure TdlgChangePassword.eNewPasswordKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    GravaStat(Name, TComponent(Sender).Name, 'Enter');
    if eConfirmPassword.Focused and sbSave.Enabled then
      sbSaveClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

  FormKeyPress(Sender, Key);
end;

procedure TdlgChangePassword.sbSaveClick(Sender: TObject);
begin
  if not ValidatePassword then
    Exit;

  BCrypt.InitStr(BF_KEY, TDCP_sha256);
  Pass := BCrypt.EncryptString(eNewPassword.Text);
  BCrypt.Burn;

  ModalResult := mrOk;
end;

function TdlgChangePassword.ValidatePassword: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;
  eNewPassword.Text := Trim(eNewPassword.Text);
  eConfirmPassword.Text := Trim(eConfirmPassword.Text);

  if (DMM.qUsers.State = dsInsert) and (eNewPassword.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rsNewPassword]));
  if (eConfirmPassword.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rsConfirmPassword]));

  if (Length(eNewPassword.Text) > 0) then
  begin
    // Check the new password minimum length
    if (Length(eNewPassword.Text) < 8) then
    begin
      Msgs.Add(rsMinPasswordLength);
    end;
    // Check the new password confirmation
    if not(eNewPassword.Text = eConfirmPassword.Text) then
    begin
      Msgs.Add(rsConfirmPasswordError);
    end;
  end;

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

