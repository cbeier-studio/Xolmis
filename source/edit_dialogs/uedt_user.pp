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
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls, atshapelinebgra,
  StdCtrls;

type

  { TedtUser }

  TedtUser = class(TForm)
    cbUserRank: TDBComboBox;
    ckAllowCollectionEdit: TDBCheckBox;
    ckAllowExport: TDBCheckBox;
    ckAllowImport: TDBCheckBox;
    ckAllowPrint: TDBCheckBox;
    dsUser: TDataSource;
    eFullName: TDBEdit;
    eUsername: TDBEdit;
    lblFullName: TLabel;
    lblPermissions: TLabel;
    lblRank: TLabel;
    lblUsername: TLabel;
    lineBottom: TShapeLineBGRA;
    pClient: TPanel;
    pBottom: TPanel;
    pFullName: TPanel;
    pPermissions: TPanel;
    pRank: TPanel;
    pUsername: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure dsUserDataChange(Sender: TObject; Field: TField);
    procedure eUsernameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtUser: TedtUser;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dataconst;

{$R *.lfm}

{ TedtUser }

procedure TedtUser.dsUserDataChange(Sender: TObject; Field: TField);
begin
  if dsUser.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsUser.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtUser.eUsernameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  cbUserRank.Items.CommaText := rsStandardUser + ',' + rsAdminUser + ',' + rsGuestUser;

  cbUserRank.ItemIndex := cbUserRank.Items.IndexOf(dsUser.DataSet.FieldByName(cbUserRank.DataField).DisplayText);

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

function TedtUser.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsUser.DataSet.FieldByName(COL_USER_NAME).AsString <> EmptyStr) and
    (dsUser.DataSet.FieldByName(COL_USER_RANK).AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtUser.sbSaveClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

function TedtUser.ValidateFields: Boolean;
begin
  Result := True;

end;

end.

