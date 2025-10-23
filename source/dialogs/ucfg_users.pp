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
  StdCtrls, Menus, DBCtrls, DBGrids, DB, SQLDB, ColorSpeedButton;

type

  { TcfgUsers }

  TcfgUsers = class(TForm)
    dsUsers: TDataSource;
    eSearch: TEdit;
    gridUsers: TDBGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    iconSearch: TImage;
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
    pSearch: TBCPanel;
    sbChangePassword: TSpeedButton;
    sbClearSearch: TColorSpeedButton;
    sbClose: TButton;
    sbDelete: TSpeedButton;
    sbEdit: TSpeedButton;
    sbNew: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator7: TShapeLineBGRA;
    Separator8: TShapeLineBGRA;
    TimerFind: TTimer;
    procedure dsUsersStateChange(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbChangePasswordClick(Sender: TObject);
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbNewClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
  private
    procedure ApplyDarkMode;
  public

  end;

var
  cfgUsers: TcfgUsers;

implementation

uses
  utils_global, models_users, data_types, data_management, utils_editdialogs, utils_themes, data_search,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TcfgUsers }

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

  pSearch.Background.Color := clCardBGDefaultDark;
  pSearch.Border.Color := clSolidBGSecondaryDark;
  pSearch.ParentBackground := True;
  eSearch.Color := pSearch.Background.Color;
  iconSearch.Images := iButtonsDark;
  sbClearSearch.Images := iButtonsDark;
  sbClearSearch.StateHover.Color := clSolidBGSecondaryDark;
  sbClearSearch.StateActive.Color := clSolidBGTertiaryDark;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
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
        eSearch.Enabled := False;
        sbClearSearch.Enabled := False;
      end;
    dsBrowse:
      begin
        sbNew.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly;
        sbEdit.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly and (dsUsers.DataSet.RecordCount > 0);
        sbChangePassword.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly and (dsUsers.DataSet.RecordCount > 0);
        sbDelete.Enabled := not (dsUsers.DataSet as TSQLQuery).ReadOnly and (dsUsers.DataSet.RecordCount > 0);
        sbRefreshRecords.Enabled := True;
        sbClose.Enabled := True;
        eSearch.Enabled := True;
        sbClearSearch.Enabled := True;
      end;
    dsEdit, dsInsert:
      begin
        sbNew.Enabled := False;
        sbEdit.Enabled := False;
        sbChangePassword.Enabled := False;
        sbDelete.Enabled := False;
        sbRefreshRecords.Enabled := False;
        sbClose.Enabled := False;
        eSearch.Enabled := False;
        sbClearSearch.Enabled := False;
      end;
  end;
  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgNew.Enabled := sbNew.Enabled;
  pmgEdit.Enabled := sbEdit.Enabled;
  pmgChangePassword.Enabled := sbChangePassword.Enabled;
  pmgDelete.Enabled := sbDelete.Enabled;
end;

procedure TcfgUsers.eSearchChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  sbClearSearch.Visible := Length(eSearch.Text) > 0;
end;

procedure TcfgUsers.FormDestroy(Sender: TObject);
begin
  dsUsers.DataSet.Close;
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
    if (eSearch.Text <> EmptyStr) then
      eSearch.Clear
    else
      ModalResult := mrClose;
  end;
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

procedure TcfgUsers.sbClearSearchClick(Sender: TObject);
begin
  LogDebug('Search cleared');

  eSearch.Clear;
  if eSearch.CanSetFocus then
    eSearch.SetFocus;
end;

procedure TcfgUsers.sbDeleteClick(Sender: TObject);
begin
  DeleteRecord(tbUsers, dsUsers.DataSet);

  dsUsers.DataSet.Refresh;
end;

procedure TcfgUsers.sbEditClick(Sender: TObject);
begin
  EditUser(False);
end;

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

procedure TcfgUsers.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  dsUsers.DataSet.Close;
  SetUsersSQL(DMM.qUsers.SQL, fvNone);

  if (eSearch.Text <> EmptyStr) then
  begin
    with DMM.qUsers, SQL do
    begin
      Add('WHERE (user_name LIKE :user_name)');
      Add('AND (active_status = 1)');
      Add('ORDER BY user_name ASC');

      ParamByName('user_name').AsString := '%' + eSearch.Text + '%';
    end;
  end
  else
  begin
    with DMM.qUsers, SQL do
    begin
      Add('WHERE (active_status = 1)');
      Add('ORDER BY user_name ASC');
    end;
  end;
  dsUsers.DataSet.Open;
end;

end.

