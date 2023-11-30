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
    procedure sbSaveClick(Sender: TObject);
  private
    xPass: String;
    function ValidatePassword: Boolean;
  public
    property Pass: String read xPass write xPass;
  end;

var
  dlgChangePassword: TdlgChangePassword;

implementation

uses cbs_locale, cbs_global, cbs_graphics, cbs_dialogs, udm_main;

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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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

  BCrypt.InitStr(BFKey, TDCP_sha256);
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

