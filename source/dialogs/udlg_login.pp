unit udlg_login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, Buttons, DCPblowfish, DCPsha256, atshapelinebgra;

type

  { TdlgLogin }

  TdlgLogin = class(TForm)
    BCrypt: TDCP_blowfish;
    ePassword: TEditButton;
    eUsername: TEdit;
    imgLogin: TImage;
    lblLogin: TLabel;
    lblPassword: TLabel;
    lblUsername: TLabel;
    lineBottom: TShapeLineBGRA;
    pClient: TPanel;
    pBottom: TPanel;
    sbCancel: TButton;
    sbOK: TButton;
    procedure ePasswordButtonClick(Sender: TObject);
    procedure eUsernameChange(Sender: TObject);
    procedure eUsernameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eUsernameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xCodUser, uForce: Integer;
    xNome, xUser: String;
    nAdmin, LoginOK: Boolean;
    function ValidatePassword: Boolean;
  public
    property NeedAdmin: Boolean read nAdmin write nAdmin;
    property ForceUser: Integer read uForce write uForce;
    property UserCodigo: Integer read xCodUser write xCodUser;
    property UserNome: String read xNome write xNome;
    property UserLogin: String read xUser write xUser;
  end;

var
  dlgLogin: TdlgLogin;

implementation

uses cbs_locale, cbs_global, cbs_graphics, cbs_themes, cbs_dialogs, cbs_getvalue, udm_main, LCLType;

{$R *.lfm}

{ TdlgLogin }

procedure TdlgLogin.eUsernameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { <ARROW DOWN> key }
  if (Key = VK_DOWN) then
  begin
    GravaStat(Name, '', 'Down');
    SelectNext(Sender as TWinControl, True, True);
    Key := 0;
  end;
  { <ARROW UP> key }
  if Key = VK_UP then
  begin
    GravaStat(Name, '', 'Up');
    Key := 0;
    SelectNext(Sender as TWinControl, False, True);
  end;
end;

procedure TdlgLogin.ePasswordButtonClick(Sender: TObject);
begin
  GravaStat(Name, 'ePassword', 'buttonclick');
  TogglePassView(ePassword);
end;

procedure TdlgLogin.eUsernameChange(Sender: TObject);
begin
  { Enable/disable Login button }
  sbOK.Enabled := Length(eUsername.Text) > 0;
end;

procedure TdlgLogin.eUsernameKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    GravaStat(Name, '', 'Enter');
    if ePassword.Focused and SBOK.Enabled then
      SBOKClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
  { <ESC> key }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TdlgLogin.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  { Clear edits }
  eUsername.Clear;
  ePassword.Clear;

  CloseAction := caFree;
end;

procedure TdlgLogin.FormCreate(Sender: TObject);
begin
  { Load username of last login }
  if not(NeedAdmin) and (XSettings.RememberUser) then
    eUsername.Text := XSettings.LastUser;
end;

procedure TdlgLogin.FormShow(Sender: TObject);
begin
  { If it needs administrator permission }
  if NeedAdmin then
  begin
    eUsername.ReadOnly := True;
    eUsername.Text := 'admin';
    { yellow }
    eUsername.Font.Color := clSystemCautionFGLight;
    eUsername.Color := clSystemCautionBGLight;
    //eUsername.RightButton.ImageIndex := 14;
    //eUsername.RightButton.Visible := True;
    lblLogin.Caption := rsAdminUser;
    sbOK.Caption := 'OK';
  end
  else
  begin
    if ForceUser > 0 then
    begin
      { If it needs authentication of a specific user }
      eUsername.Text := GetName('users', 'user_name', 'user_id', ForceUser);
      eUsername.Color := clWhite;
      //eUsername.RightButton.ImageIndex:= 33;
      //eUsername.RightButton.Visible := True;
    end;
    //Caption := Format('%s - %s', [NomeApp, rsTitleLogin]);
    sbOK.Caption := rsLoginButton;
    //eUsername.RightButton.Visible := False;
  end;

  if (eUsername.Text <> EmptyStr) then
    ePassword.SetFocus
  else
    eUsername.SetFocus;
end;

procedure TdlgLogin.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'sbOK', 'click');
  LoginOK := False;

  if not ValidatePassword then
    Exit;

  LoginOK := True;

  { Save settings }
  if not NeedAdmin then
    if XSettings.RememberUser then
      XSettings.LastUser := eUsername.Text;
    //else
    //  DelPreference('SECURITY', 'LastUser');

  { Close dialog }
  if LoginOK then
    ModalResult := mrOK
  else
    ModalResult := mrCancel;
end;

function TdlgLogin.ValidatePassword: Boolean;
var
  Qry: TSQLQuery;
  S: String;
begin
  Result := False;
  eUsername.Text := Trim(eUsername.Text);
  ePassword.Text := Trim(ePassword.Text);
  S := EmptyStr;

  { Check user }
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Clear;
    Add('SELECT user_id, full_name, user_name, user_password FROM users');
    Add('WHERE (active_status = 1) AND (user_name = :login)');
    ParamByName('LOGIN').DataType := ftString;
    ParamByName('LOGIN').AsString := eUsername.Text;
    Open;
    if (IsEmpty) then
    begin
      LogError('User not found');
      MsgDlg('', rsInvalidLogin, mtError);
      if eUsername.CanSetFocus then
        eUsername.SetFocus;
      Exit;
    end;

    { Check password }
    BCrypt.InitStr(BFKey, TDCP_sha256);
    S := BCrypt.EncryptString(ePassword.Text);
    BCrypt.Burn;
    if (FieldByName('user_password').AsString <> S) and
      (FieldByName('user_password').AsString <> EmptyStr) then
    begin
      LogError('Incorrect password');
      MsgDlg('', rsIncorrectPassword, mtError);
      ePassword.SetFocus;
      Exit;
    end;

    UserCodigo := FieldByName('user_id').AsInteger;
    UserNome := FieldByName('full_name').AsString;
    UserLogin := FieldByName('user_name').AsString;

    Result := True;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

