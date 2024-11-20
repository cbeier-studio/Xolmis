{ Xolmis First Execution dialog

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

unit udlg_newdatabase;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, DBCtrls, EditBtn, atshapelinebgra, DCPblowfish, DCPsha256, DB,
  SQLDB;

type

  { TdlgNewDatabase }

  TdlgNewDatabase = class(TForm)
    BCrypt: TDCP_blowfish;
    lblRequired: TLabel;
    sbCreateDB: TBitBtn;
    sbCreateUser: TBitBtn;
    sbApplyAdmin: TBitBtn;
    eConfirmPass: TEditButton;
    eAuthor: TEdit;
    eDescription: TEdit;
    eUserConfirmPass: TEditButton;
    eDBFile: TEditButton;
    eName: TEdit;
    eUserName: TEdit;
    eUserFullName: TEdit;
    eNewPass: TEditButton;
    eUserNewPass: TEditButton;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblAuthor: TLabel;
    lblDescription: TLabel;
    lblSubtitleUser: TLabel;
    lblUserConfirmPass: TLabel;
    lblSubtitleConnection: TLabel;
    lblSubtitleAdmin: TLabel;
    lblDBFile: TLabel;
    lblUserName: TLabel;
    lblUserFullName: TLabel;
    lblNewPass: TLabel;
    lblConfirmPass: TLabel;
    lblName: TLabel;
    lblUserNewPass: TLabel;
    lblTitleAuthentication: TLabel;
    lblTitleUser: TLabel;
    lblTitleConnection: TLabel;
    lineBottom: TShapeLineBGRA;
    nbPages: TNotebook;
    OpenDlg: TOpenDialog;
    pCreateUser: TPanel;
    pContentUser: TPanel;
    pgUser: TPage;
    pgConnection: TPage;
    pgAdmin: TPage;
    pContentConnection: TPanel;
    pCreateDB: TPanel;
    pApplyAdmin: TPanel;
    pBottom: TPanel;
    pContentAdmin: TPanel;
    pTitleAuthentication: TPanel;
    pTitleUser: TPanel;
    pTitleConnection: TPanel;
    sbCancel: TButton;
    procedure eConfirmPassButtonClick(Sender: TObject);
    procedure eDBFileButtonClick(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eNewPassButtonClick(Sender: TObject);
    procedure eNewPassChange(Sender: TObject);
    procedure eNewPassKeyPress(Sender: TObject; var Key: char);
    procedure eUserConfirmPassButtonClick(Sender: TObject);
    procedure eUserConfirmPassChange(Sender: TObject);
    procedure eUserNameChange(Sender: TObject);
    procedure eUserNewPassButtonClick(Sender: TObject);
    procedure eUserNewPassChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbApplyAdminClick(Sender: TObject);
    procedure sbCreateDBClick(Sender: TObject);
    procedure sbCreateUserClick(Sender: TObject);
  private
    FPass: String;
    function IsRequiredAdminFilled: Boolean;
    function IsRequiredConnectionFilled: Boolean;
    function IsRequiredUserFilled: Boolean;
    function UpdateAdmin: Boolean;
    function ValidateDatabase: Boolean;
    function ValidatePassword: Boolean;
    function ValidateUser: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  dlgNewDatabase: TdlgNewDatabase;

implementation

uses
  cbs_global, cbs_locale, cbs_datatypes, cbs_data, cbs_graphics, cbs_dialogs, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgNewDatabase }

procedure TdlgNewDatabase.ApplyDarkMode;
begin
  eDBFile.Images := iButtonsDark;
  eNewPass.Images := iButtonsDark;
  eConfirmPass.Images := iButtonsDark;
end;

procedure TdlgNewDatabase.eConfirmPassButtonClick(Sender: TObject);
begin
  TogglePassView(eConfirmPass);
end;

procedure TdlgNewDatabase.eDBFileButtonClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    eDBFile.Text := OpenDlg.FileName;
  end;
end;

procedure TdlgNewDatabase.eNameChange(Sender: TObject);
begin
  sbCreateDB.Enabled := IsRequiredConnectionFilled;
end;

procedure TdlgNewDatabase.eNameKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    //if eConfirmPass.Focused and sbSave.Enabled then
    //  sbSaveClick(nil)
    //else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

  FormKeyPress(Sender, Key);
end;

procedure TdlgNewDatabase.eNewPassButtonClick(Sender: TObject);
begin
  TogglePassView(eNewPass);
end;

procedure TdlgNewDatabase.eNewPassChange(Sender: TObject);
begin
  sbApplyAdmin.Enabled := IsRequiredAdminFilled;
end;

procedure TdlgNewDatabase.eNewPassKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    //if eConfirmPass.Focused and sbSave.Enabled then
    //  sbSaveClick(nil)
    //else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

  FormKeyPress(Sender, Key);
end;

procedure TdlgNewDatabase.eUserConfirmPassButtonClick(Sender: TObject);
begin
  TogglePassView(eUserConfirmPass);
end;

procedure TdlgNewDatabase.eUserConfirmPassChange(Sender: TObject);
begin
  sbCreateUser.Enabled := IsRequiredUserFilled;
end;

procedure TdlgNewDatabase.eUserNameChange(Sender: TObject);
begin
  sbCreateUser.Enabled := IsRequiredUserFilled;
end;

procedure TdlgNewDatabase.eUserNewPassButtonClick(Sender: TObject);
begin
  TogglePassView(eUserNewPass);
end;

procedure TdlgNewDatabase.eUserNewPassChange(Sender: TObject);
begin
  sbCreateUser.Enabled := IsRequiredUserFilled;
end;

procedure TdlgNewDatabase.FormDestroy(Sender: TObject);
begin
  eNewPass.Clear;
  eConfirmPass.Clear;
  FPass := EmptyStr;
end;

procedure TdlgNewDatabase.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if Key = #27 then
  begin
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TdlgNewDatabase.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;
end;

function TdlgNewDatabase.IsRequiredAdminFilled: Boolean;
begin
  Result := False;

  if (eNewPass.Text <> EmptyStr) and
    (eConfirmPass.Text <> EmptyStr) then
    Result := True;
end;

function TdlgNewDatabase.IsRequiredConnectionFilled: Boolean;
begin
  Result := False;

  if (eName.Text <> EmptyStr) and
    (eDBFile.Text <> EmptyStr) then
    Result := True;
end;

function TdlgNewDatabase.IsRequiredUserFilled: Boolean;
begin
  Result := False;

  if (eUserName.Text <> EmptyStr) and
    (eNewPass.Text <> EmptyStr) and
    (eConfirmPass.Text <> EmptyStr) then
    Result := True;
end;

procedure TdlgNewDatabase.sbApplyAdminClick(Sender: TObject);
begin
  if not ValidatePassword then
    Exit;

  sbApplyAdmin.Enabled := False;

  if UpdateAdmin then
  begin
    //MsgDlg(rsTitleAdminPassword, rsSuccessfulUpdateAdminPassword, mtInformation);
    nbPages.PageIndex := nbPages.PageIndex + 1;
  end
  else
    MsgDlg(rsTitleAdminPassword, rsErrorUpdatingAdminPassword, mtError);
end;

procedure TdlgNewDatabase.sbCreateDBClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  sbCreateDB.Enabled := False;

  if not ValidateDatabase then
    Exit;

  if not FileExists(eDBFile.Text) then
    if CreateUserDatabase(dbSqlite, eDBFile.Text, eName.Text, eAuthor.Text, eDescription.Text) then
    begin
      Qry := TSQLQuery.Create(nil);
      with Qry, SQL do
      try
        // Create new connection
        SQLConnection := DMM.sysCon;
        SQLTransaction := DMM.sysTrans;
        Clear;
        Add('INSERT INTO connections (connection_name, database_type, database_name, insert_date) ');
        Add('VALUES (:aname, 0, :afile, datetime(''now'', ''localtime''))');
        ParamByName('ANAME').AsString := eName.Text;
        ParamByName('AFILE').AsString := eDBFile.Text;
        ExecSQL;
      finally
        FreeAndNil(Qry);
      end;

      DMM.sysTrans.CommitRetaining;

      //MsgDlg(rsTitleCreateDatabase, rsSuccessfulDatabaseCreation, mtInformation);
      nbPages.PageIndex := nbPages.PageIndex + 1;
    end
    else
      MsgDlg(rsTitleCreateDatabase, rsErrorDatabaseCreation, mtError);
end;

procedure TdlgNewDatabase.sbCreateUserClick(Sender: TObject);
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
  Qry: TSQLQuery;
  aPass: String;
begin
  sbCreateUser.Enabled := False;

  if not ValidateDatabase then
    Exit;

  BCrypt.InitStr(BFKey, TDCP_sha256);
  aPass := BCrypt.EncryptString(eUserNewPass.Text);
  BCrypt.Burn;

  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  Qry := TSQLQuery.Create(nil);
  try
    uTrans.Action := caRollbackRetaining;
    uCon.Transaction := uTrans;
    uTrans.DataBase := uCon;
    uCon.CharSet := 'UTF-8';
    uCon.ConnectorType := 'SQLite3';
    uCon.LoginPrompt := False;
    uCon.DatabaseName := eDBFile.Text;

    //LoadDatabaseParams(eName.Text, uCon);
    with Qry, SQL do
    try
      SQLConnection := uCon;
      uCon.Open;
      if not uTrans.Active then
        uTrans.StartTransaction;

      Clear;
      Add('INSERT INTO users ( user_name, full_name, user_rank, user_password )');
      Add('VALUES ( :username, :fullname, :rank, :pass );');
      ParamByName('USERNAME').AsString := eUserName.Text;
      ParamByName('FULLNAME').AsString := eUserFullName.Text;
      ParamByName('RANK').AsString := 'S';
      ParamByName('PASS').AsString := aPass;
      ExecSQL;

      uTrans.CommitRetaining;
      Self.ModalResult := mrOK;
    except
      on E: Exception do
      begin
        uTrans.Rollback;
        MsgDlg(rsTitleError, Format(rsErrorCreatingUser, [E.Message]), mtError);
        LogError(E.Message);
      end;
    end;
  finally
    if uCon.Connected then
      uCon.Close;

    FreeAndNil(Qry);
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

function TdlgNewDatabase.UpdateAdmin: Boolean;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
  Qry: TSQLQuery;
begin
  Result := False;

  BCrypt.InitStr(BFKey, TDCP_sha256);
  FPass := BCrypt.EncryptString(eNewPass.Text);
  BCrypt.Burn;

  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  Qry := TSQLQuery.Create(nil);
  try
    uTrans.Action := caRollbackRetaining;
    uCon.Transaction := uTrans;
    uTrans.DataBase := uCon;
    uCon.CharSet := 'UTF-8';
    uCon.ConnectorType := 'SQLite3';
    uCon.LoginPrompt := False;
    uCon.DatabaseName := eDBFile.Text;

    //LoadDatabaseParams(eName.Text, uCon);
    with Qry, SQL do
    try
      SQLConnection := uCon;
      uCon.Open;
      if not uTrans.Active then
        uTrans.StartTransaction;

      Clear;
      Add('UPDATE users SET user_password = :pass');
      Add('WHERE (user_name = :ausername)');
      ParamByName('AUSERNAME').AsString := 'admin';
      ParamByName('PASS').AsString := FPass;
      ExecSQL;

      uTrans.CommitRetaining;

      Result := True;
    except
      on E: Exception do
      begin
        Result := False;
        uTrans.Rollback;
        MsgDlg(rsTitleError, Format(rsErrorWritingAdminPassword, [E.Message]), mtError);
        LogError(E.Message);
      end;
    end;
  finally
    if uCon.Connected then
      uCon.Close;

    FreeAndNil(Qry);
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

function TdlgNewDatabase.ValidateDatabase: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;
  eName.Text := Trim(eName.Text);
  eDBFile.Text := Trim(eDBFile.Text);

  //if (Length(eDBFile.Text) = 0) then
  //begin
  //  // Check if the connection name exists
  //  if (Length(eNewPass.Text) < 8) then
  //  begin
  //    Msgs.Add(rsMinPasswordLength);
  //  end;
  //  // Check if the database file exists
  //  if not(eNewPass.Text = eConfirmPass.Text) then
  //  begin
  //    Msgs.Add(rsConfirmPasswordError);
  //  end;
  //end;

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

function TdlgNewDatabase.ValidatePassword: Boolean;
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

function TdlgNewDatabase.ValidateUser: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;
  eUserNewPass.Text := Trim(eUserNewPass.Text);
  eUserConfirmPass.Text := Trim(eUserConfirmPass.Text);

  if (Length(eUserNewPass.Text) > 0) then
  begin
    // Check the new password minimum length
    if (Length(eUserNewPass.Text) < 8) then
    begin
      Msgs.Add(rsMinPasswordLength);
    end;
    // Check the new password confirmation
    if not(eUserNewPass.Text = eUserConfirmPass.Text) then
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

