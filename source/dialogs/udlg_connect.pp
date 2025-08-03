{ Xolmis Connection dialog

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

unit udlg_connect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn,
  ATLinkLabel, DCPblowfish, DCPsha256, atshapelinebgra, LCLType;

type

  { TdlgConnect }

  TdlgConnect = class(TForm)
    BCrypt: TDCP_blowfish;
    cbConnection: TComboBox;
    ePassword: TEditButton;
    eUsername: TEdit;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    imgLogin: TImage;
    lblConnection: TLabel;
    lblLogin: TLabel;
    lblPassword: TLabel;
    lblUsername: TLabel;
    lineBottom: TShapeLineBGRA;
    linkManageConn: TATLabelLink;
    pBottom: TPanel;
    pClient: TPanel;
    sbCancel: TButton;
    sbOK: TButton;
    procedure cbConnectionChange(Sender: TObject);
    procedure ePasswordButtonClick(Sender: TObject);
    procedure eUsernameChange(Sender: TObject);
    procedure eUsernameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eUsernameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure linkManageConnClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    sConnect: String;
    sUser: Integer;
    procedure ApplyDarkMode;
    procedure LoadConnectionList;
    procedure UpdateButtons;
    function ValidatePassword: Boolean;
  public
    property SelectedConnection: String read sConnect write sConnect;
  end;

var
  dlgConnect: TdlgConnect;

implementation

uses utils_locale, utils_global, models_users, utils_dialogs, utils_graphics, udm_main, ucfg_database, uDarkStyleParams;

{$R *.lfm}

{ TdlgConnect }

procedure TdlgConnect.ApplyDarkMode;
begin
  pBottom.ParentBackground := True;
  ePassword.Images := iButtonsDark;

end;

procedure TdlgConnect.cbConnectionChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgConnect.ePasswordButtonClick(Sender: TObject);
begin
  GravaStat(Name, 'ePassword.RightButton', 'click');
  TogglePassView(ePassword);
end;

procedure TdlgConnect.eUsernameChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgConnect.eUsernameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { <DOWN ARROW> Key }
  if (Key = VK_DOWN) then
  begin
    GravaStat(Name, '', 'Down');
    SelectNext(Sender as TWinControl, True, True);
    Key := 0;
  end;
  { <UP ARROW> Key }
  if Key = VK_UP then
  begin
    GravaStat(Name, '', 'Up');
    Key := 0;
    SelectNext(Sender as TWinControl, False, True);
  end;
end;

procedure TdlgConnect.eUsernameKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    GravaStat(Name, '', 'Enter');
    if ePassword.Focused and sbOK.Enabled then
      sbOKClick(nil)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
  { <ESC> Key }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    Key := #0;
    dlgConnect.ModalResult := mrCancel;
  end;
end;

procedure TdlgConnect.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Clear edits
  eUsername.Clear;
  ePassword.Clear;

  CloseAction := caFree;
end;

procedure TdlgConnect.FormShow(Sender: TObject);
var
  LastConn: Integer;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  LoadConnectionList;

  // Load connection of last session
  if (xSettings.RememberConnection) then
  begin
    LastConn := cbConnection.Items.IndexOf(xSettings.LastConnection);
    if LastConn >= 0 then
    begin
      cbConnection.ItemIndex := LastConn;
      eUsername.SetFocus;
    end;
  end;

  // Load username of last session
  if (xSettings.RememberUser) then
    eUsername.Text := xSettings.LastUser;

  if (eUsername.Text <> EmptyStr) then
    ePassword.SetFocus;
end;

procedure TdlgConnect.linkManageConnClick(Sender: TObject);
begin
  cfgDatabase := TcfgDatabase.Create(nil);
  with cfgDatabase do
  try
    ShowModal;
    LoadConnectionList;
  finally
    FreeAndNil(cfgDatabase);
  end;
end;

procedure TdlgConnect.LoadConnectionList;
var
  C: TSQLQuery;
begin
  C := DMM.qsConn;
  if not C.Active then
    C.Open;

  cbConnection.Items.BeginUpdate;
  try
    cbConnection.Items.Clear;
    C.DisableControls;
    C.First;
    while not C.Eof do
    begin
      cbConnection.Items.Add(C.FieldByName('connection_name').AsString);
      Application.ProcessMessages;
      C.Next;
    end;
    C.First;
    C.EnableControls;
    cbConnection.Sorted := True;
  finally
    cbConnection.Items.EndUpdate;
  end;
end;

procedure TdlgConnect.sbCancelClick(Sender: TObject);
begin
  sbOK.Enabled := False;
  dlgConnect.ModalResult := mrCancel;
end;

procedure TdlgConnect.sbOKClick(Sender: TObject);
begin
  //GravaStat(Name, 'sbOK', 'click');

  if cbConnection.ItemIndex < 0 then
    Exit;
  if not ValidatePassword then
    Exit;

  sbOK.Enabled := False;

  SelectedConnection := cbConnection.Text;
  ActiveUser.GetData(sUser);

  if xSettings.RememberConnection then
    xSettings.LastConnection := SelectedConnection;
  //else
  //  DelPreference('SECURITY', 'LastConnection');

  if xSettings.RememberUser then
    xSettings.LastUser := eUsername.Text;
  //else
  //  DelPreference('SECURITY', 'LastUser');

  dlgConnect.ModalResult := mrOk;
end;

procedure TdlgConnect.UpdateButtons;
begin
  { Enable/disable OK button }
  sbOK.Enabled := (cbConnection.ItemIndex >= 0) and (Length(eUsername.Text) > 0);
end;

function TdlgConnect.ValidatePassword: Boolean;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
  Qry: TSQLQuery;
  S: String;
begin
  Result := False;
  sUser := 0;
  eUsername.Text := Trim(eUsername.Text);
  ePassword.Text := Trim(ePassword.Text);
  S := EmptyStr;

  { Check user }
  uCon := TSQLConnector.Create(nil);
  try
    LoadDatabaseParams(cbConnection.Text, uCon);

    try
      uTrans := TSQLTransaction.Create(uCon);
      uCon.Transaction := uTrans;
      //try
      //  uCon.Connected := True;
      //except
      //  raise Exception.CreateFmt('Unable to connect to database %s', [uCon.DatabaseName]);
      //end;
      uTrans.StartTransaction;
      Qry := TSQLQuery.Create(uCon);
      with Qry, SQL do
      try
        Qry.Database := uCon;
        Clear;
        Add('SELECT user_id, full_name, user_name, user_password FROM users');
        Add('WHERE (active_status = 1) AND (user_name = :login)');
        ParamByName('LOGIN').AsString := eUsername.Text;
        Qry.Open;
        if (Qry.IsEmpty) then
        begin
          LogError('User not found');
          MsgDlg('', rsInvalidLogin, mtError);
          if eUsername.CanSetFocus then
            eUsername.SetFocus;
          Exit;
        end;

        { Check password }
        BCrypt.InitStr(BF_KEY, TDCP_sha256);
        S := BCrypt.EncryptString(ePassword.Text);
        BCrypt.Burn;
        if (FieldByName('user_password').AsString <> EmptyStr) and
          (FieldByName('user_password').AsString <> S) then
        begin
          LogError('Incorrect password');
          MsgDlg('', rsIncorrectPassword, mtError);
          ePassword.SetFocus;
          Exit;
        end;

        sUser := FieldByName('user_id').AsInteger;

        Result := True;
        Qry.Close;
      finally
        FreeAndNil(Qry);
      end;
      uTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        uTrans.RollbackRetaining;
        LogError(Format(rsErrorValidatingPassword, [E.Message]));
        MsgDlg('', Format(rsErrorValidatingPassword, [E.Message]), mtError);
      end;
    end;
  finally
    if uCon.Connected then
      uCon.Close;
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

end.

