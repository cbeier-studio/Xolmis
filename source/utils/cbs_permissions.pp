{ Xolmis User Permissions library

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

unit cbs_permissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DB, SQLDB, Forms, Dialogs, cbs_users;

  function UserLogin(const aCodLogin: Integer): Boolean;
  function AdminPermission: Boolean;

  function GetUserPref(aUser: Integer; aPref: String): Boolean;
  function GetUserPermission(aUser: Integer; aPerm: String): Boolean;

implementation

uses cbs_global, udm_main, udlg_login;

{ ---------------------------------------------------------------------------------- }
{ Atenticação de usuário }
{ ---------------------------------------------------------------------------------- }

function UserLogin(const aCodLogin: Integer): Boolean;
begin
  Application.CreateForm(TdlgLogin, dlgLogin);
  try
    dlgLogin.ForceUser := aCodLogin;
    dlgLogin.NeedAdmin := False;
    //GravaStat('dlgLogin', '', 'open');
    if dlgLogin.ShowModal = mrOk then
    begin
      ActiveUser.GetData(dlgLogin.UserCodigo);
      LogInfo('Login Ok');
      Result := True;
    end
    else
    begin
      LogInfo('Login cancelled');
      Result := False;
    end;
  finally
    FreeAndNil(dlgLogin);
  end;
end;

// Solicita permissão do Administrador
function AdminPermission: Boolean;
begin
  Application.CreateForm(TdlgLogin, dlgLogin);
  try
    dlgLogin.NeedAdmin := True;
    GravaStat('dlgLogin', '', 'permission');
    if dlgLogin.ShowModal = mrOk then
    begin
      LogInfo('Admin permission granted');
      Result := True;
    end
    else
    begin
      LogInfo('Admin permission cancelled');
      Result := False;
    end;
  finally
    FreeAndNil(dlgLogin);
  end;
end;

function GetUserPref(aUser: Integer; aPref: String): Boolean;
var
  a: Boolean;
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT user_id, %pref FROM users');
    Add('WHERE (user_id = :auser)');
    MacroByName('PREF').Value := aPref;
    ParamByName('AUSER').DataType := ftInteger;
    ParamByName('AUSER').AsInteger := aUser;
    Open;
    a := FieldByName(aPref).AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  Result := a;
end;

function GetUserPermission(aUser: Integer; aPerm: String): Boolean;
var
  a: Boolean;
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT user_id, %perm FROM users');
    Add('WHERE (user_id = :auser)');
    MacroByName('PERM').Value := aPerm;
    ParamByName('AUSER').DataType := ftInteger;
    ParamByName('AUSER').AsInteger := aUser;
    Open;
    a := FieldByName(aPerm).AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  Result := a;
end;

end.

