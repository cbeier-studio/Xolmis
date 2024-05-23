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
    eConfirmPass: TEditButton;
    eDBFile: TEditButton;
    eName: TEdit;
    eNewPass: TEditButton;
    iButtons: TImageList;
    lblConnectionInstruction: TLabel;
    lblAdminInstruction: TLabel;
    lblDBFile: TLabel;
    lblNewPass: TLabel;
    lblConfirmPass: TLabel;
    lblName: TLabel;
    lblTitleAuthentication: TLabel;
    lblTitleConnection: TLabel;
    lineBottom: TShapeLineBGRA;
    nbPages: TNotebook;
    OpenDlg: TOpenDialog;
    pgConnection: TPage;
    pgAdmin: TPage;
    pContentConnection: TPanel;
    pCreateDB: TPanel;
    pApplyAdmin: TPanel;
    pBottom: TPanel;
    pContentAdmin: TPanel;
    pTitleAuthentication: TPanel;
    pTitleConnection: TPanel;
    sbCancel: TButton;
    sbCreateDB: TButton;
    sbApplyAdmin: TButton;
    procedure eConfirmPassButtonClick(Sender: TObject);
    procedure eDBFileButtonClick(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eNewPassButtonClick(Sender: TObject);
    procedure eNewPassChange(Sender: TObject);
    procedure eNewPassKeyPress(Sender: TObject; var Key: char);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure sbApplyAdminClick(Sender: TObject);
    procedure sbCreateDBClick(Sender: TObject);
  private
    FPass: String;
    function IsRequiredAdminFilled: Boolean;
    function IsRequiredConnectionFilled: Boolean;
    function UpdateAdmin: Boolean;
    function ValidateDatabase: Boolean;
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
  sbCreateDB.Enabled := IsRequiredConnectionFilled;
end;

procedure TdlgFirstConfig.eNameKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgFirstConfig.eNewPassButtonClick(Sender: TObject);
begin
  TogglePassView(eNewPass);
end;

procedure TdlgFirstConfig.eNewPassChange(Sender: TObject);
begin
  sbApplyAdmin.Enabled := IsRequiredAdminFilled;
end;

procedure TdlgFirstConfig.eNewPassKeyPress(Sender: TObject; var Key: char);
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

function TdlgFirstConfig.IsRequiredAdminFilled: Boolean;
begin
  Result := False;

  if (eNewPass.Text <> EmptyStr) and
    (eConfirmPass.Text <> EmptyStr) then
    Result := True;
end;

function TdlgFirstConfig.IsRequiredConnectionFilled: Boolean;
begin
  Result := False;

  if (eName.Text <> EmptyStr) and
    (eDBFile.Text <> EmptyStr) then
    Result := True;
end;

procedure TdlgFirstConfig.sbApplyAdminClick(Sender: TObject);
begin
  if not ValidatePassword then
    Exit;

  sbApplyAdmin.Enabled := False;

  if UpdateAdmin then
  begin
    MsgDlg(rsTitleAdminPassword, rsSuccessfulUpdateAdminPassword, mtInformation);
    Self.ModalResult := mrOk;
  end
  else
    MsgDlg(rsTitleAdminPassword, rsErrorUpdatingAdminPassword, mtError);
end;

procedure TdlgFirstConfig.sbCreateDBClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  sbCreateDB.Enabled := False;

  if not ValidateDatabase then
    Exit;

  if not FileExists(eDBFile.Text) then
    CreateUserDatabase(dbSqlite, eDBFile.Text);

  if FileExists(eDBFile.Text) then
  begin
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
    finally
      FreeAndNil(Qry);
    end;

    MsgDlg(rsTitleCreateDatabase, rsSuccessfulDatabaseCreation, mtInformation);
    nbPages.PageIndex := nbPages.PageIndex + 1;
  end
  else
    MsgDlg(rsTitleCreateDatabase, rsErrorDatabaseCreation, mtError);
end;

function TdlgFirstConfig.UpdateAdmin: Boolean;
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
    LoadDatabaseParams(Self.Name, uCon);
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

      uTrans.Commit;

      Result := True;
    except
      Result := False;

      uTrans.Rollback;
      //raise Exception.Create(rsErrorConnectingDatabase);
    end;
  finally
    if uCon.Connected then
      uCon.Close;

    FreeAndNil(Qry);
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

function TdlgFirstConfig.ValidateDatabase: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;
  eName.Text := Trim(eName.Text);
  eDBFile.Text := Trim(eDBFile.Text);

  if (Length(eDBFile.Text) > 0) then
  begin
    // Check if the connection name exists
    if (Length(eNewPass.Text) < 8) then
    begin
      Msgs.Add(rsMinPasswordLength);
    end;
    // Check if the database file exists
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

