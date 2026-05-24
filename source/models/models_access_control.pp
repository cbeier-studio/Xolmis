{ Xolmis access control models

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

unit models_access_control;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, data_types, models_record_types;

const
  ROLE_ADMIN_ID = 1;
  ROLE_SUPERVISOR_ID = 2;
  ROLE_STANDARD_ID = 3;
  ROLE_READER_ID = 4;
  ROLE_GUEST_ID = 5;

  // Granular module permissions
  PERM_SPECIMENS_VIEW = 'specimens.view';
  PERM_SPECIMENS_INSERT = 'specimens.insert';
  PERM_SPECIMENS_EDIT = 'specimens.edit';
  PERM_SPECIMENS_DELETE = 'specimens.delete';
  PERM_SPECIMENS_EXPORT = 'specimens.export';
  PERM_SPECIMENS_PRINT = 'specimens.print';

  PERM_INSTITUTIONS_VIEW = 'institutions.view';
  PERM_INSTITUTIONS_INSERT = 'institutions.insert';
  PERM_INSTITUTIONS_EDIT = 'institutions.edit';
  PERM_INSTITUTIONS_DELETE = 'institutions.delete';
  PERM_INSTITUTIONS_EXPORT = 'institutions.export';
  PERM_INSTITUTIONS_PRINT = 'institutions.print';

  PERM_GAZETTEER_VIEW = 'gazetteer.view';
  PERM_GAZETTEER_INSERT = 'gazetteer.insert';
  PERM_GAZETTEER_EDIT = 'gazetteer.edit';
  PERM_GAZETTEER_DELETE = 'gazetteer.delete';
  PERM_GAZETTEER_EXPORT = 'gazetteer.export';
  PERM_GAZETTEER_PRINT = 'gazetteer.print';

  PERM_BREEDING_VIEW = 'breeding.view';
  PERM_BREEDING_INSERT = 'breeding.insert';
  PERM_BREEDING_EDIT = 'breeding.edit';
  PERM_BREEDING_DELETE = 'breeding.delete';
  PERM_BREEDING_EXPORT = 'breeding.export';
  PERM_BREEDING_PRINT = 'breeding.print';

  PERM_BOTANY_VIEW = 'botany.view';
  PERM_BOTANY_INSERT = 'botany.insert';
  PERM_BOTANY_EDIT = 'botany.edit';
  PERM_BOTANY_DELETE = 'botany.delete';
  PERM_BOTANY_EXPORT = 'botany.export';
  PERM_BOTANY_PRINT = 'botany.print';

  PERM_BIRDS_VIEW = 'birds.view';
  PERM_BIRDS_INSERT = 'birds.insert';
  PERM_BIRDS_EDIT = 'birds.edit';
  PERM_BIRDS_DELETE = 'birds.delete';
  PERM_BIRDS_EXPORT = 'birds.export';
  PERM_BIRDS_PRINT = 'birds.print';

  PERM_BANDS_VIEW = 'bands.view';
  PERM_BANDS_INSERT = 'bands.insert';
  PERM_BANDS_EDIT = 'bands.edit';
  PERM_BANDS_DELETE = 'bands.delete';
  PERM_BANDS_EXPORT = 'bands.export';
  PERM_BANDS_PRINT = 'bands.print';

  PERM_PROJECTS_VIEW = 'projects.view';
  PERM_PROJECTS_INSERT = 'projects.insert';
  PERM_PROJECTS_EDIT = 'projects.edit';
  PERM_PROJECTS_DELETE = 'projects.delete';
  PERM_PROJECTS_EXPORT = 'projects.export';
  PERM_PROJECTS_PRINT = 'projects.print';

  PERM_PERMITS_VIEW = 'permits.view';
  PERM_PERMITS_INSERT = 'permits.insert';
  PERM_PERMITS_EDIT = 'permits.edit';
  PERM_PERMITS_DELETE = 'permits.delete';
  PERM_PERMITS_EXPORT = 'permits.export';
  PERM_PERMITS_PRINT = 'permits.print';

  PERM_PEOPLE_VIEW = 'people.view';
  PERM_PEOPLE_INSERT = 'people.insert';
  PERM_PEOPLE_EDIT = 'people.edit';
  PERM_PEOPLE_DELETE = 'people.delete';
  PERM_PEOPLE_EXPORT = 'people.export';
  PERM_PEOPLE_PRINT = 'people.print';

  PERM_METHODS_VIEW = 'methods.view';
  PERM_METHODS_INSERT = 'methods.insert';
  PERM_METHODS_EDIT = 'methods.edit';
  PERM_METHODS_DELETE = 'methods.delete';
  PERM_METHODS_EXPORT = 'methods.export';
  PERM_METHODS_PRINT = 'methods.print';

  PERM_SAMPLING_PLOTS_VIEW = 'sampling_plots.view';
  PERM_SAMPLING_PLOTS_INSERT = 'sampling_plots.insert';
  PERM_SAMPLING_PLOTS_EDIT = 'sampling_plots.edit';
  PERM_SAMPLING_PLOTS_DELETE = 'sampling_plots.delete';
  PERM_SAMPLING_PLOTS_EXPORT = 'sampling_plots.export';
  PERM_SAMPLING_PLOTS_PRINT = 'sampling_plots.print';

  PERM_SAMPLING_VIEW = 'sampling.view';
  PERM_SAMPLING_INSERT = 'sampling.insert';
  PERM_SAMPLING_EDIT = 'sampling.edit';
  PERM_SAMPLING_DELETE = 'sampling.delete';
  PERM_SAMPLING_EXPORT = 'sampling.export';
  PERM_SAMPLING_PRINT = 'sampling.print';

  PERM_SIGHTINGS_VIEW = 'sightings.view';
  PERM_SIGHTINGS_INSERT = 'sightings.insert';
  PERM_SIGHTINGS_EDIT = 'sightings.edit';
  PERM_SIGHTINGS_DELETE = 'sightings.delete';
  PERM_SIGHTINGS_EXPORT = 'sightings.export';
  PERM_SIGHTINGS_PRINT = 'sightings.print';

  PERM_TAXA_VIEW = 'taxa.view';
  PERM_TAXA_EXPORT = 'taxa.export';
  PERM_TAXA_PRINT = 'taxa.print';

  // Globais e especiais
  PERM_VERIFY_RECORDS = 'verify.records';
  PERM_RESTORE_RECORDS = 'restore.records';
  PERM_UNDO_EDITS = 'undo.edits';
  PERM_REPORT_EXPORT = 'report.export';
  PERM_DATABASE_CREATE = 'database.create';
  PERM_DATABASE_EDIT = 'database.edit';
  PERM_DATABASE_DELETE = 'database.delete';
  PERM_DATABASE_MAINTENANCE = 'database.maintenance';

  // Importação granular
  PERM_IMPORT_WIZARD = 'import.wizard';
  PERM_IMPORT_XOLMIS_MOBILE = 'import.xolmis_mobile';
  PERM_IMPORT_BANDING = 'import.banding';
  PERM_IMPORT_NESTS = 'import.nests';
  PERM_IMPORT_EBIRD = 'import.ebird';
  PERM_IMPORT_GEOCOORDS = 'import.geocoords';

  // Administração
  PERM_USERS_MANAGE = 'users.manage';
  PERM_ROLES_MANAGE = 'roles.manage';
  PERM_PERMISSIONS_MANAGE = 'permissions.manage';
  PERM_SYSTEM_MAINTENANCE = 'system.maintenance';

type
  TRole = class(TPersistent)
  private
    FRoleId: Integer;
    FRoleName: String;
    FDescription: String;
    FCanDelete: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function ToString: String; override;
  published
    property RoleId: Integer read FRoleId write FRoleId;
    property RoleName: String read FRoleName write FRoleName;
    property Description: String read FDescription write FDescription;
    property CanDelete: Boolean read FCanDelete write FCanDelete;
  end;

  TPermission = class(TPersistent)
  private
    FPermissionId: Integer;
    FPermissionName: String;
    FDescription: String;
    FPermissionGroup: String;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function ToString: String; override;
  published
    property PermissionId: Integer read FPermissionId write FPermissionId;
    property PermissionName: String read FPermissionName write FPermissionName;
    property Description: String read FDescription write FDescription;
    property PermissionGroup: String read FPermissionGroup write FPermissionGroup;
  end;

function LegacyRankToRoleId(const ARank: String): Integer;
function GetRoleNameById(Connection: TSQLConnection; RoleId: Integer): String;
function GetRoleIdByName(Connection: TSQLConnection; const RoleName: String): Integer;
procedure LoadPermissionsForRole(Connection: TSQLConnection; RoleId: Integer; Permissions: TStrings);
procedure SavePermissionsForRole(Connection: TSQLConnection; RoleId: Integer; PermissionIds: TStrings);

implementation

{ TRole }

procedure TRole.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRole then
  begin
    FRoleId := TRole(Source).RoleId;
    FRoleName := TRole(Source).RoleName;
    FDescription := TRole(Source).Description;
    FCanDelete := TRole(Source).CanDelete;
  end;
end;

procedure TRole.Clear;
begin
  FRoleId := 0;
  FRoleName := EmptyStr;
  FDescription := EmptyStr;
  FCanDelete := True;
end;

function TRole.ToString: String;
begin
  Result := Format('Role(Id=%d, Name=%s, CanDelete=%s)',
    [FRoleId, FRoleName, BoolToStr(FCanDelete, 'True', 'False')]);
end;

{ TPermission }

procedure TPermission.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TPermission then
  begin
    FPermissionId := TPermission(Source).PermissionId;
    FPermissionName := TPermission(Source).PermissionName;
    FDescription := TPermission(Source).Description;
    FPermissionGroup := TPermission(Source).PermissionGroup;
  end;
end;

procedure TPermission.Clear;
begin
  FPermissionId := 0;
  FPermissionName := EmptyStr;
  FDescription := EmptyStr;
  FPermissionGroup := EmptyStr;
end;

function TPermission.ToString: String;
begin
  Result := Format('Permission(Id=%d, Name=%s, Group=%s)',
    [FPermissionId, FPermissionName, FPermissionGroup]);
end;

function LegacyRankToRoleId(const ARank: String): Integer;
begin
  case UpperCase(Trim(ARank)) of
    'A': Result := ROLE_ADMIN_ID;
    'S': Result := ROLE_STANDARD_ID;
    'V': Result := ROLE_GUEST_ID;
  else
    Result := ROLE_STANDARD_ID;
  end;
end;

function GetRoleNameById(Connection: TSQLConnection; RoleId: Integer): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;
  if (Connection = nil) or (RoleId <= 0) then
    Exit;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.Database := Connection;
    Qry.SQL.Text := 'SELECT role_name FROM roles WHERE role_id = :role_id';
    Qry.ParamByName('role_id').AsInteger := RoleId;
    Qry.Open;
    if not Qry.EOF then
      Result := Qry.Fields[0].AsString;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

function GetRoleIdByName(Connection: TSQLConnection; const RoleName: String): Integer;
var
  Qry: TSQLQuery;
begin
  Result := 0;
  if (Connection = nil) or (RoleName = EmptyStr) then
    Exit;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.Database := Connection;
    Qry.SQL.Text := 'SELECT role_id FROM roles WHERE role_name = :role_name COLLATE NOCASE';
    Qry.ParamByName('role_name').AsString := RoleName;
    Qry.Open;
    if not Qry.EOF then
      Result := Qry.Fields[0].AsInteger;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

procedure LoadPermissionsForRole(Connection: TSQLConnection; RoleId: Integer; Permissions: TStrings);
var
  Qry: TSQLQuery;
begin
  if Assigned(Permissions) then
    Permissions.Clear;
  if (Connection = nil) or (RoleId <= 0) or not Assigned(Permissions) then
    Exit;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.Database := Connection;
    Qry.SQL.Text :=
      'SELECT p.permission_name ' +
      'FROM role_permissions rp ' +
      'INNER JOIN permissions p ON p.permission_id = rp.permission_id ' +
      'WHERE rp.role_id = :role_id ' +
      'ORDER BY p.permission_group, p.permission_name';
    Qry.ParamByName('role_id').AsInteger := RoleId;
    Qry.Open;
    while not Qry.EOF do
    begin
      Permissions.Add(Qry.Fields[0].AsString);
      Qry.Next;
    end;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

procedure SavePermissionsForRole(Connection: TSQLConnection; RoleId: Integer; PermissionIds: TStrings);
var
  Qry: TSQLQuery;
  i: Integer;
begin
  if (Connection = nil) or (RoleId <= 0) then
    Exit;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.Database := Connection;
    Qry.SQL.Text := 'DELETE FROM role_permissions WHERE role_id = :role_id';
    Qry.ParamByName('role_id').AsInteger := RoleId;
    Qry.ExecSQL;

    if Assigned(PermissionIds) then
    begin
      Qry.SQL.Text := 'INSERT INTO role_permissions (role_id, permission_id) VALUES (:role_id, :permission_id)';
      for i := 0 to PermissionIds.Count - 1 do
      begin
        if Trim(PermissionIds[i]) = EmptyStr then
          Continue;
        Qry.ParamByName('role_id').AsInteger := RoleId;
        Qry.ParamByName('permission_id').AsInteger := StrToIntDef(PermissionIds[i], 0);
        if Qry.ParamByName('permission_id').AsInteger > 0 then
          Qry.ExecSQL;
      end;
    end;
  finally
    Qry.Free;
  end;
end;

end.
