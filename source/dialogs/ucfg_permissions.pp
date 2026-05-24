{ Xolmis Roles and Permissions dialog

  Copyright (C) 2026 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit ucfg_permissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Buttons, Menus,
  BCPanel, ATShapeLineBGRA, laz.VirtualTrees, ColorSpeedButton;

type

  { TcfgPermissions }

  TcfgPermissions = class(TForm)
    btnClose: TButton;
    btnHelp: TSpeedButton;
    eSearch: TEdit;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    iconSearch: TImage;
    lineBottom: TShapeLineBGRA;
    lvRoles: TListView;
    pmpExpandAll: TMenuItem;
    pmpCollapseAll: TMenuItem;
    pmpMarkAll: TMenuItem;
    pmpUnmarkAll: TMenuItem;
    pmrNew: TMenuItem;
    pmrEdit: TMenuItem;
    pmrRefresh: TMenuItem;
    pmrDelete: TMenuItem;
    pBottom: TPanel;
    pmRoles: TPopupMenu;
    pmPermissions: TPopupMenu;
    pToolbar: TBCPanel;
    pSearch: TBCPanel;
    sbClearSearch: TColorSpeedButton;
    sbDelete: TSpeedButton;
    sbEdit: TSpeedButton;
    sbNew: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator8: TShapeLineBGRA;
    TimerFind: TTimer;
    vtPermissions: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure eSearchEnter(Sender: TObject);
    procedure eSearchExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvRolesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure pmpCollapseAllClick(Sender: TObject);
    procedure pmpExpandAllClick(Sender: TObject);
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbNewClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure vtPermissionsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtPermissionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtPermissionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
  private
    FLoadingPermissions: Boolean;
    procedure ApplyDarkMode;
    function CurrentRoleId: Integer;
    function CurrentRoleName: String;
    function CurrentRoleDescription: String;
    function RoleCanDelete(const RoleId: Integer): Boolean;
    procedure LoadRoles;
    procedure LoadPermissionsForRole(const RoleId: Integer);
    procedure LoadCurrentRolePermissions;
    procedure SavePermissionChange(const PermissionId: Integer; Checked: Boolean);
  public

  end;

var
  cfgPermissions: TcfgPermissions;

implementation

uses
  models_access_control, utils_locale, utils_dialogs, utils_global, utils_themes, udm_main, uedt_role, uDarkStyleParams;

{$R *.lfm}

type
  PPermissionNodeData = ^TPermissionNodeData;
  TPermissionNodeData = record
    IsGroup: Boolean;
    PermissionId: Integer;
    PermissionGroup: String;
    PermissionName: String;
    Description: String;
  end;

procedure TcfgPermissions.ApplyDarkMode;
begin
  btnHelp.Images := DMM.iEditsDark;
  sbNew.Images := iButtonsDark;
  sbEdit.Images := iButtonsDark;
  sbRefreshRecords.Images := iButtonsDark;
  sbDelete.Images := iButtonsDark;
  pmRoles.Images := iButtonsDark;
  pmPermissions.Images := iButtonsDark;

  pSearch.Background.Color := clCardBGDefaultDark;
  pSearch.Border.Color := clSolidBGSecondaryDark;
  pSearch.ParentBackground := True;
  eSearch.Color := pSearch.Background.Color;
  iconSearch.Images := iButtonsDark;
  sbClearSearch.Images := iButtonsDark;
  sbClearSearch.StateHover.Color := clSolidBGSecondaryDark;
  sbClearSearch.StateActive.Color := clSolidBGTertiaryDark;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;

  pToolbar.Background.Color := ActiveTheme.Background.CardDefault;
  pToolbar.Border.Color := ActiveTheme.Background.CardSecondary;
end;

procedure TcfgPermissions.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TcfgPermissions.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_USERS);
end;

function TcfgPermissions.CurrentRoleDescription: String;
var
  idx: Integer;
begin
  Result := '';
  if (lvRoles.Selected <> nil) and (lvRoles.Selected.SubItems.Count > 0) then
  begin
    idx := lvRoles.Selected.SubItems.IndexOfName('description');
    Result := lvRoles.Selected.SubItems[idx];
  end;
end;

function TcfgPermissions.CurrentRoleId: Integer;
begin
  Result := 0;
  if (lvRoles.Selected <> nil) and Assigned(lvRoles.Selected.Data) then
    Result := PtrInt(lvRoles.Selected.Data);
end;

function TcfgPermissions.CurrentRoleName: String;
begin
  Result := '';
  if lvRoles.Selected <> nil then
    Result := lvRoles.Selected.Caption;
end;

procedure TcfgPermissions.eSearchChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  sbClearSearch.Visible := Length(eSearch.Text) > 0;
end;

procedure TcfgPermissions.eSearchEnter(Sender: TObject);
begin
  if eSearch.Text = EmptyStr then
    pSearch.Width := ClientWidth div 4;
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := ActiveTheme.Background.SolidBase;
    pSearch.Border.Color := ActiveTheme.AccentPalette.Dark1; //clSolidBGTertiaryDark;
  end
  else
  begin
    pSearch.Background.Color := clWhite;
    pSearch.Border.Color := ActiveTheme.AccentFill.Tertiary;
  end;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TcfgPermissions.eSearchExit(Sender: TObject);
begin
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := ActiveTheme.Background.CardSecondary;
    pSearch.Border.Color := ActiveTheme.Background.SolidSecondary;
  end
  else
  begin
    pSearch.Background.Color := ActiveTheme.Background.CardSecondary;
    pSearch.Border.Color := ActiveTheme.Border.Default;
  end;
  pSearch.Border.Width := 1;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TcfgPermissions.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  vtPermissions.NodeDataSize := SizeOf(TPermissionNodeData);

  LoadRoles;
end;

function TcfgPermissions.RoleCanDelete(const RoleId: Integer): Boolean;
begin
  Result := not (RoleId in [ROLE_ADMIN_ID, ROLE_GUEST_ID]);
end;

procedure TcfgPermissions.LoadRoles;
var
  Qry: TSQLQuery;
  Item: TListItem;
begin
  lvRoles.Items.BeginUpdate;
  try
    lvRoles.Items.Clear;

    Qry := TSQLQuery.Create(nil);
    try
      Qry.Database := DMM.sqlCon;
      Qry.SQL.Text := 'SELECT role_id, role_name, description FROM roles ORDER BY role_id';
      Qry.Open;
      while not Qry.EOF do
      begin
        Item := lvRoles.Items.Add;
        Item.Caption := Qry.FieldByName('role_name').AsString;
        Item.SubItems.Add(Qry.FieldByName('description').AsString);
        Item.Data := Pointer(PtrInt(Qry.FieldByName('role_id').AsInteger));
        Qry.Next;
      end;
      Qry.Close;
    finally
      Qry.Free;
    end;
  finally
    lvRoles.Items.EndUpdate;
  end;

  if lvRoles.Items.Count > 0 then
    lvRoles.Items[0].Selected := True;
  LoadCurrentRolePermissions;
end;

procedure TcfgPermissions.LoadCurrentRolePermissions;
begin
  if CurrentRoleId > 0 then
    LoadPermissionsForRole(CurrentRoleId)
  else
    vtPermissions.Clear;

  sbDelete.Enabled := RoleCanDelete(CurrentRoleId);
end;

procedure TcfgPermissions.LoadPermissionsForRole(const RoleId: Integer);
var
  Qry: TSQLQuery;
  GroupMap: TStringList;
  GroupNode: PVirtualNode;
  PermissionNode: PVirtualNode;
  NodeData: PPermissionNodeData;
  CheckedIndex: Integer;
  GroupIndex: Integer;
  CurrentGroup: String;
  FilterText: String;
  SearchBlob: String;
begin
  FLoadingPermissions := True;
  vtPermissions.BeginUpdate;
  try
    vtPermissions.Clear;
    FilterText := LowerCase(Trim(eSearch.Text));

    GroupMap := TStringList.Create;
    try
      GroupMap.Sorted := True;
      GroupMap.Duplicates := dupIgnore;
      GroupMap.CaseSensitive := False;

      Qry := TSQLQuery.Create(nil);
      try
        Qry.Database := DMM.sqlCon;
        Qry.SQL.Text :=
          'SELECT p.permission_id, p.permission_name, p.description, p.permission_group, ' +
            'CASE WHEN rp.role_id IS NULL THEN 0 ELSE 1 END AS is_selected ' +
          'FROM permissions p ' +
          'LEFT JOIN role_permissions rp ON rp.permission_id = p.permission_id AND rp.role_id = :role_id ' +
          'ORDER BY p.permission_group, p.permission_name';
        Qry.ParamByName('role_id').AsInteger := RoleId;
        Qry.Open;

        while not Qry.EOF do
        begin
          if FilterText <> EmptyStr then
          begin
            SearchBlob := LowerCase(
              Qry.FieldByName('permission_name').AsString + ' ' +
              Qry.FieldByName('description').AsString + ' ' +
              Qry.FieldByName('permission_group').AsString // + ' ' +
              //LocalizePermissionName(Qry.FieldByName('permission_name').AsString) + ' ' +
              //LocalizePermissionDescription(Qry.FieldByName('permission_name').AsString,
              //  Qry.FieldByName('description').AsString) + ' ' +
              //LocalizePermissionGroup(Qry.FieldByName('permission_group').AsString)
            );

            if Pos(FilterText, SearchBlob) = 0 then
            begin
              Qry.Next;
              Continue;
            end;
          end;

          CurrentGroup := Qry.FieldByName('permission_group').AsString;
          GroupIndex := GroupMap.IndexOf(CurrentGroup);
          if GroupIndex < 0 then
          begin
            GroupNode := vtPermissions.AddChild(nil);
            NodeData := vtPermissions.GetNodeData(GroupNode);
            NodeData^.IsGroup := True;
            NodeData^.PermissionGroup := CurrentGroup;
            NodeData^.PermissionName := '';
            NodeData^.Description := '';
            NodeData^.PermissionId := 0;
            GroupMap.AddObject(CurrentGroup, TObject(PtrInt(GroupNode)));
          end
          else
            GroupNode := PVirtualNode(PtrInt(GroupMap.Objects[GroupIndex]));

          PermissionNode := vtPermissions.AddChild(GroupNode);
          NodeData := vtPermissions.GetNodeData(PermissionNode);
          NodeData^.IsGroup := False;
          NodeData^.PermissionId := Qry.FieldByName('permission_id').AsInteger;
          NodeData^.PermissionGroup := CurrentGroup;
          NodeData^.PermissionName := Qry.FieldByName('permission_name').AsString;
          NodeData^.Description := Qry.FieldByName('description').AsString;
          vtPermissions.CheckType[PermissionNode] := ctCheckBox;
          if (Qry.FieldByName('is_selected').AsInteger <> 0) then
            vtPermissions.CheckState[PermissionNode] := csCheckedNormal
          else
            vtPermissions.CheckState[PermissionNode] := csUncheckedNormal;

          Qry.Next;
        end;
        Qry.Close;
      finally
        Qry.Free;
      end;
    finally
      GroupMap.Free;
    end;

    vtPermissions.FullExpand;
  finally
    vtPermissions.EndUpdate;
    FLoadingPermissions := False;
  end;
end;

procedure TcfgPermissions.lvRolesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
    LoadCurrentRolePermissions;
end;

procedure TcfgPermissions.pmpCollapseAllClick(Sender: TObject);
begin
  vtPermissions.FullCollapse();
end;

procedure TcfgPermissions.pmpExpandAllClick(Sender: TObject);
begin
  vtPermissions.FullExpand();
end;

procedure TcfgPermissions.SavePermissionChange(const PermissionId: Integer; Checked: Boolean);
var
  Qry: TSQLQuery;
begin
  if CurrentRoleId <= 0 then
    Exit;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.Database := DMM.sqlCon;
    if Checked then
    begin
      Qry.SQL.Text := 'INSERT OR IGNORE INTO role_permissions (role_id, permission_id) VALUES (:role_id, :permission_id)';
      Qry.ParamByName('role_id').AsInteger := CurrentRoleId;
      Qry.ParamByName('permission_id').AsInteger := PermissionId;
      Qry.ExecSQL;
    end
    else
    begin
      Qry.SQL.Text := 'DELETE FROM role_permissions WHERE role_id = :role_id AND permission_id = :permission_id';
      Qry.ParamByName('role_id').AsInteger := CurrentRoleId;
      Qry.ParamByName('permission_id').AsInteger := PermissionId;
      Qry.ExecSQL;
    end;
  finally
    Qry.Free;
  end;
end;

procedure TcfgPermissions.sbClearSearchClick(Sender: TObject);
begin
  LogDebug('Search cleared');

  eSearch.Clear;
  if eSearch.CanSetFocus then
    eSearch.SetFocus;
end;

procedure TcfgPermissions.sbDeleteClick(Sender: TObject);
var
  RoleId: Integer;
begin
  RoleId := CurrentRoleId;
  if RoleId <= 0 then
    Exit;

  if not RoleCanDelete(RoleId) then
  begin
    MsgDlg(rsRoles, rsRoleCannotBeDeleted, mtError);
    Exit;
  end;

  if MessageDlg(rsDeleteRole, rsDeleteRolePrompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  DMM.sqlCon.ExecuteDirect('DELETE FROM roles WHERE role_id = ' + IntToStr(RoleId));
  LoadRoles;
end;

procedure TcfgPermissions.sbEditClick(Sender: TObject);
var
  Qry: TSQLQuery;
  RoleId: Integer;
begin
  RoleId := CurrentRoleId;
  if RoleId <= 0 then
    Exit;

  if not RoleCanDelete(RoleId) then
  begin
    MsgDlg(rsRoles, rsRoleCannotBeEdited, mtError);
    Exit;
  end;

  edtRole := TedtRole.Create(Self);
  with edtRole do
  try
    RoleName := CurrentRoleName;
    Description := CurrentRoleDescription;
    if ShowModal = mrOK then
    begin
      Qry := TSQLQuery.Create(nil);
      try
        Qry.Database := DMM.sqlCon;
        Qry.SQL.Text := 'UPDATE roles SET role_name = :role_name, description = :description WHERE (role_id = :role_id)';
        Qry.ParamByName('role_name').AsString := RoleName;
        Qry.ParamByName('description').AsString := Description;
        Qry.ParamByName('role_id').AsInteger := CurrentRoleId;
        Qry.ExecSQL;
      finally
        Qry.Free;
      end;
    end;
  finally
    FreeAndNil(edtRole);
  end;

  LoadRoles;
end;

procedure TcfgPermissions.sbNewClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  edtRole := TedtRole.Create(Self);
  with edtRole do
  try
    IsNewRecord := True;
    if ShowModal = mrOK then
    begin
      Qry := TSQLQuery.Create(nil);
      try
        Qry.Database := DMM.sqlCon;
        Qry.SQL.Text := 'INSERT INTO roles (role_name, description, can_delete) VALUES (:role_name, :description, 1)';
        Qry.ParamByName('role_name').AsString := RoleName;
        Qry.ParamByName('description').AsString := Description;
        Qry.ExecSQL;
      finally
        Qry.Free;
      end;
    end;
  finally
    FreeAndNil(edtRole);
  end;

  LoadRoles;
end;

procedure TcfgPermissions.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;
  LoadCurrentRolePermissions;
end;

procedure TcfgPermissions.vtPermissionsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PPermissionNodeData;
begin
  if FLoadingPermissions then
    Exit;

  NodeData := Sender.GetNodeData(Node);
  if (NodeData = nil) or NodeData^.IsGroup then
    Exit;

  SavePermissionChange(NodeData^.PermissionId,
    Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed]);
end;

procedure TcfgPermissions.vtPermissionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PPermissionNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData <> nil then
    Finalize(NodeData^);
end;

procedure TcfgPermissions.vtPermissionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  NodeData: PPermissionNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if NodeData^.IsGroup then
    CellText := NodeData^.PermissionGroup
  else
    CellText := NodeData^.Description;
end;

end.
