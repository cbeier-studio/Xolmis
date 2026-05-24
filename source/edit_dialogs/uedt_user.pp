{ Xolmis User Editor dialog

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

unit uedt_user;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls, atshapelinebgra,
  StdCtrls, Buttons;

type

  { TedtUser }

  TedtUser = class(TForm)
    btnHelp: TSpeedButton;
    cbUserRank: TComboBox;
    dsUser: TDataSource;
    eFullName: TDBEdit;
    eUsername: TDBEdit;
    lblFullName: TLabel;
    lblRank: TLabel;
    lblUsername: TLabel;
    lineBottom: TShapeLineBGRA;
    pClient: TPanel;
    pBottom: TPanel;
    pFullName: TPanel;
    pRank: TPanel;
    pUsername: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure cbUserRankChange(Sender: TObject);
    procedure dsUserDataChange(Sender: TObject; Field: TField);
    procedure eUsernameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
    procedure LoadRoles;
    procedure SyncRoleSelection;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtUser: TedtUser;

implementation

uses
  utils_locale, utils_global, data_types, data_consts, models_access_control, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtUser }

procedure TedtUser.ApplyDarkMode;
begin
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtUser.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_USERS);
end;

procedure TedtUser.cbUserRankChange(Sender: TObject);
begin
  if not Assigned(dsUser.DataSet) or not (dsUser.State in [dsInsert, dsEdit]) then
    Exit;

  if cbUserRank.ItemIndex < 0 then
    Exit;

  dsUser.DataSet.FieldByName('role_id').AsInteger := PtrInt(cbUserRank.Items.Objects[cbUserRank.ItemIndex]);
  dsUser.DataSet.FieldByName('user_rank').AsString := cbUserRank.Items[cbUserRank.ItemIndex];
end;

procedure TedtUser.dsUserDataChange(Sender: TObject; Field: TField);
begin
  SyncRoleSelection;

  if dsUser.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsUser.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtUser.eUsernameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtUser.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (dsUser.State in [dsInsert, dsEdit]) then
      Exit;

    ModalResult := mrOk;
  end;
end;

procedure TedtUser.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtUser.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  LoadRoles;
  SyncRoleSelection;

  if dsUser.State = dsInsert then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionUser)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionUser)]);
    eUsername.Enabled := False;
  end;
end;

procedure TedtUser.LoadRoles;
var
  Qry: TSQLQuery;
begin
  cbUserRank.Items.Clear;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.Database := DMM.sqlCon;
    Qry.SQL.Text := 'SELECT role_id, role_name FROM roles ORDER BY role_id';
    Qry.Open;
    while not Qry.EOF do
    begin
      cbUserRank.Items.AddObject(Qry.Fields[1].AsString, TObject(PtrInt(Qry.Fields[0].AsInteger)));
      Qry.Next;
    end;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

procedure TedtUser.SyncRoleSelection;
var
  RoleId: Integer;
  i: Integer;
begin
  if not Assigned(dsUser.DataSet) or not dsUser.DataSet.Active then
    Exit;

  if dsUser.DataSet.FindField('role_id') <> nil then
    RoleId := dsUser.DataSet.FieldByName('role_id').AsInteger
  else
    RoleId := ROLE_STANDARD_ID;

  cbUserRank.ItemIndex := -1;
  for i := 0 to cbUserRank.Items.Count - 1 do
    if PtrInt(cbUserRank.Items.Objects[i]) = RoleId then
    begin
      cbUserRank.ItemIndex := i;
      Break;
    end;
end;

function TedtUser.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsUser.DataSet.FieldByName(COL_USER_NAME).AsString <> EmptyStr) and
    (cbUserRank.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtUser.sbSaveClick(Sender: TObject);
begin
  cbUserRankChange(nil);
  ModalResult := mrOK;
end;

function TedtUser.ValidateFields: Boolean;
begin
  Result := True;

end;

end.

