{ Xolmis User Management dialog

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

unit ucfg_users;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, atshapelinebgra, BCPanel,
  StdCtrls, Menus, DBCtrls, DBGrids, DB, SQLDB;

type

  { TcfgUsers }

  TcfgUsers = class(TForm)
    dsUsers: TDataSource;
    gridUsers: TDBGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lineBottom: TShapeLineBGRA;
    pmgRefresh: TMenuItem;
    pmgNew: TMenuItem;
    pChildToolbar: TBCPanel;
    pmgEdit: TMenuItem;
    pmgDelete: TMenuItem;
    pmgChangePassword: TMenuItem;
    pBottom: TPanel;
    pClient: TPanel;
    pmGrid: TPopupMenu;
    sbChangePassword: TSpeedButton;
    sbClose: TButton;
    sbDelete: TSpeedButton;
    sbEdit: TSpeedButton;
    sbNew: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator7: TShapeLineBGRA;
    Separator8: TShapeLineBGRA;
    procedure dsUsersStateChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbChangePasswordClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbNewClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
  public

  end;

var
  cfgUsers: TcfgUsers;

implementation

uses
  utils_global, models_users, data_types, data_management, utils_editdialogs, utils_themes,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TcfgUsers }

procedure TcfgUsers.sbNewClick(Sender: TObject);
begin
  EditUser(True);
end;

procedure TcfgUsers.sbRefreshRecordsClick(Sender: TObject);
begin
  if not dsUsers.DataSet.Active then
    dsUsers.DataSet.Open;

  dsUsers.DataSet.Refresh;
end;

procedure TcfgUsers.sbDeleteClick(Sender: TObject);
begin
  DeleteRecord(tbUsers, dsUsers.DataSet);

  dsUsers.DataSet.Refresh;
end;

procedure TcfgUsers.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrClose;
  end;
end;

procedure TcfgUsers.ApplyDarkMode;
begin
  pChildToolbar.Background.Color := clCardBGDefaultDark;
  pChildToolbar.Border.Color := clCardBGSecondaryDark;
  pClient.Color := clSolidBGBaseDark;

  sbNew.Images := iButtonsDark;
  sbEdit.Images := iButtonsDark;
  sbChangePassword.Images := iButtonsDark;
  sbRefreshRecords.Images := iButtonsDark;
  sbDelete.Images := iButtonsDark;
  pmGrid.Images := iButtonsDark;
end;

procedure TcfgUsers.dsUsersStateChange(Sender: TObject);
begin
  case DMM.dsUsers.State of
    dsInactive:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbChangePassword.Enabled := False;
        sbDelete.Enabled := False;
        sbRefreshRecords.Enabled := True;
        sbClose.Enabled := True;
      end;
    dsBrowse:
      begin
        sbNew.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly;
        sbEdit.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly and (dsUsers.DataSet.RecordCount > 0);
        sbChangePassword.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly and (dsUsers.DataSet.RecordCount > 0);
        sbDelete.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly and (dsUsers.DataSet.RecordCount > 0);
        sbRefreshRecords.Enabled := True;
        sbClose.Enabled := True;
      end;
    dsEdit, dsInsert:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbChangePassword.Enabled := False;
        sbDelete.Enabled := False;
        sbRefreshRecords.Enabled := False;
        sbClose.Enabled := False;
      end;
  end;
  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgNew.Enabled := sbNew.Enabled;
  pmgEdit.Enabled := sbEdit.Enabled;
  pmgChangePassword.Enabled := sbChangePassword.Enabled;
  pmgDelete.Enabled := sbDelete.Enabled;
end;

procedure TcfgUsers.FormDestroy(Sender: TObject);
begin
  dsUsers.DataSet.Close;
end;

procedure TcfgUsers.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if ActiveUser.IsVisitor or not ActiveUser.AllowManageCollection then
    (dsUsers.DataSet as TSQLQuery).ReadOnly := True;
  if not dsUsers.DataSet.Active then
    dsUsers.DataSet.Open;
end;

procedure TcfgUsers.sbChangePasswordClick(Sender: TObject);
begin
  ChangeUserPassword(False);
end;

procedure TcfgUsers.sbEditClick(Sender: TObject);
begin
  EditUser(False);
end;

end.

