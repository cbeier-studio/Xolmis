unit udlg_firstconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DBCtrls, EditBtn,
  atshapelinebgra, DCPblowfish, DCPsha256, DB, SQLDB;

type

  { TdlgFirstConfig }

  TdlgFirstConfig = class(TForm)
    BCrypt: TDCP_blowfish;
    eNewPass: TEditButton;
    eConfirmPass: TEditButton;
    eDBFile: TEditButton;
    eName: TEdit;
    lblDBFile: TLabel;
    lblDBPass: TLabel;
    lblDBPass1: TLabel;
    lblName: TLabel;
    lblTitleAuthentication: TLabel;
    lblTitleAuthentication1: TLabel;
    lineBottom: TShapeLineBGRA;
    OpenDlg: TOpenDialog;
    pBottom: TPanel;
    pMain: TPanel;
    pTitleAuthentication: TPanel;
    pTitleAuthentication1: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure eConfirmPassButtonClick(Sender: TObject);
    procedure eDBFileButtonClick(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eNewPassButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure sbSaveClick(Sender: TObject);
  private
    FPass: String;
    function IsRequiredFilled: Boolean;
    function ValidatePassword: Boolean;
  public

  end;

var
  dlgFirstConfig: TdlgFirstConfig;

implementation

uses
  cbs_global, cbs_locale, cbs_datatypes, cbs_data, cbs_graphics, cbs_dialogs, udm_main;

{$R *.lfm}

{ TdlgFirstConfig }

procedure TdlgFirstConfig.eConfirmPassButtonClick(Sender: TObject);
begin
  TogglePassView(eConfirmPass);
end;

procedure TdlgFirstConfig.eDBFileButtonClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    eDBFile.Text := OpenDlg.FileName;
  end;
end;

procedure TdlgFirstConfig.eNameChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TdlgFirstConfig.eNameKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if eConfirmPass.Focused and sbSave.Enabled then
      sbSaveClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

  FormKeyPress(Sender, Key);
end;

procedure TdlgFirstConfig.eNewPassButtonClick(Sender: TObject);
begin
  TogglePassView(eNewPass);
end;

procedure TdlgFirstConfig.FormDestroy(Sender: TObject);
begin
  eNewPass.Clear;
  eConfirmPass.Clear;
  FPass := EmptyStr;
end;

procedure TdlgFirstConfig.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if Key = #27 then
  begin
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

function TdlgFirstConfig.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eName.Text <> EmptyStr) and
    (eDBFile.Text <> EmptyStr) and
    (eNewPass.Text <> EmptyStr) and
    (eConfirmPass.Text <> EmptyStr) then
    Result := True;
end;

procedure TdlgFirstConfig.sbSaveClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  if not ValidatePassword then
    Exit;

  BCrypt.InitStr(BFKey, TDCP_sha256);
  FPass := BCrypt.EncryptString(eNewPass.Text);
  BCrypt.Burn;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    // Create new connection
    SQLConnection := DMM.sysCon;
    Clear;
    Add('INSERT INTO connections (connection_name, database_type, database_name, insert_date) ');
    Add('VALUES (:aname, 0, :afile, datetime(''now'', ''localtime''))');
    ParamByName('ANAME').AsString := eName.Text;
    ParamByName('AFILE').AsString := eDBFile.Text;
    ExecSQL;

    if not FileExists(eDBFile.Text) then
      CreateUserDatabase(dbSqlite, eDBFile.Text);

    // Save Admin password
    SQLConnection := DMM.sqlCon;
    Clear;
    Add('UPDATE users SET user_password = :pass');
    Add('WHERE (user_name = :ausername)');
    ParamByName('AUSERNAME').AsString := 'admin';
    ParamByName('PASS').AsString := FPass;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;

  Self.ModalResult := mrOk;
end;

function TdlgFirstConfig.ValidatePassword: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;
  eNewPass.Text := Trim(eNewPass.Text);
  eConfirmPass.Text := Trim(eConfirmPass.Text);

  if (Length(eNewPass.Text) > 0) then
  begin
    // Check the new password minimum length
    if (Length(eNewPass.Text) < 8) then
    begin
      Msgs.Add(rsMinPasswordLength);
    end;
    // Check the new password confirmation
    if not(eNewPass.Text = eConfirmPass.Text) then
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

