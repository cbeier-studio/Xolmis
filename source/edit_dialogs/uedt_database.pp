{ Xolmis Database Connection Editor dialog

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

unit uedt_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  EditBtn, ComCtrls, DBCtrls, DBEditButton, atshapelinebgra;

type

  { TedtDatabase }

  TedtDatabase = class(TForm)
    ckRemoteDB: TCheckBox;
    cbDBManager: TDBComboBox;
    eDBFile: TDBEditButton;
    dsConn: TDataSource;
    eName: TDBEdit;
    eDBPass: TEditButton;
    eDBServer: TDBEdit;
    eDBPort: TDBEdit;
    eDBUser: TDBEdit;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblTitleAuthentication: TLabel;
    lblDBFile: TLabel;
    lblDBManager: TLabel;
    lblName: TLabel;
    lblDBPass: TLabel;
    lblDBPort: TLabel;
    lblDBServer: TLabel;
    lblDBUser: TLabel;
    lineBottom: TShapeLineBGRA;
    OpenDlg: TOpenDialog;
    pTitleAuthentication: TPanel;
    pMain: TPanel;
    pBottom: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure cbDBManagerSelect(Sender: TObject);
    procedure ckRemoteDBChange(Sender: TObject);
    procedure dsConnDataChange(Sender: TObject; Field: TField);
    procedure dsConnUpdateData(Sender: TObject);
    procedure eDBFileButtonClick(Sender: TObject);
    procedure eDBPassButtonClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  edtDatabase: TedtDatabase;

implementation

uses cbs_locale, cbs_global, cbs_graphics, cbs_datatypes, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtDatabase }

procedure TedtDatabase.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');
  ModalResult := mrCancel;
end;

procedure TedtDatabase.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  OpenDlg.InitialDir := InstallDir;
  OpenDlg.Title := rsTitleSelectDatabaseFile;
  cbDBManagerSelect(nil);
  ckRemoteDBChange(nil);

  cbDBManager.ItemIndex := cbDBManager.Items.IndexOf(dsConn.DataSet.FieldByName(cbDBManager.DataField).DisplayText);
  //eDBFile.Text := dsConn.DataSet.FieldByName('database_name').AsString;
  eDBPass.Text := dsConn.DataSet.FieldByName('user_password').AsString;

  eName.SetFocus;
end;

procedure TedtDatabase.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtDatabase.eDBFileButtonClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'buttonclick');
  if OpenDlg.Execute then
  begin
    eDBFile.Field.AsString := OpenDlg.FileName;
  end;
end;

procedure TedtDatabase.eDBPassButtonClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'buttonclick');
  TogglePassView(eDBPass);
end;

procedure TedtDatabase.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtDatabase.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtDatabase.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (dsConn.State in [dsInsert, dsEdit]) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtDatabase.ApplyDarkMode;
begin
  eDBFile.Images := DMM.iEditsDark;
  eDBPass.Images := iButtonsDark;
end;

procedure TedtDatabase.cbDBManagerSelect(Sender: TObject);
begin
  case TDBManager(cbDBManager.ItemIndex) of
    dbSqlite: { SQLite }
      begin
        ckRemoteDB.Checked := False;
        DMM.qsConn.FieldByName('database_server').AsString := 'localhost';
        eDBServer.Enabled := False;
        lblDBServer.Enabled := False;
        DMM.qsConn.FieldByName('database_port').Clear;
        eDBPort.Enabled := False;
        lblDBPort.Enabled := False;
        DMM.qsConn.FieldByName('user_name').Clear;
        eDBUser.Enabled := False;
        lblDBUser.Enabled := False;
        DMM.qsConn.FieldByName('user_password').Clear;
        eDBPass.Enabled := False;
        eDBPass.Button.Enabled := False;
        lblDBPass.Enabled := False;
        //eDBFile.FButton.Visible := True;
        lblDBFile.Caption := rsLabelFile;
      end;
    dbFirebird: { Firebird }
      begin
        eDBServer.Enabled := True;
        lblDBServer.Enabled := True;
        if DMM.qsConn.FieldByName('database_port').IsNull then
          DMM.qsConn.FieldByName('database_port').AsInteger := 3050;
        eDBPort.Enabled := True;
        lblDBPort.Enabled := True;
        eDBUser.Enabled := True;
        lblDBUser.Enabled := True;
        eDBPass.Enabled := True;
        eDBPass.Button.Enabled := True;
        lblDBPass.Enabled := True;
        //eDBFile.FButton.Visible := True;
        lblDBFile.Caption := rsLabelFile;
      end;
    dbPostgre: { PostgreSQL }
      begin
        eDBServer.Enabled := True;
        lblDBServer.Enabled := True;
        if DMM.qsConn.FieldByName('database_port').IsNull then
          DMM.qsConn.FieldByName('database_port').AsInteger := 5432;
        eDBPort.Enabled := True;
        lblDBPort.Enabled := True;
        eDBUser.Enabled := True;
        lblDBUser.Enabled := True;
        eDBPass.Enabled := True;
        eDBPass.Button.Enabled := True;
        lblDBPass.Enabled := True;
        //eDBFile.FButton.Visible := False;
        lblDBFile.Caption := rsLabelName;
      end;
    dbMaria: { MariaDB }
      begin
        eDBServer.Enabled := True;
        lblDBServer.Enabled := True;
        if DMM.qsConn.FieldByName('database_port').IsNull then
          DMM.qsConn.FieldByName('database_port').AsInteger := 3306;
        eDBPort.Enabled := True;
        lblDBPort.Enabled := True;
        eDBUser.Enabled := True;
        lblDBUser.Enabled := True;
        eDBPass.Enabled := True;
        eDBPass.Button.Enabled := True;
        lblDBPass.Enabled := True;
        //eDBFile.FButton.Visible := False;
        lblDBFile.Caption := rsLabelName;
      end;
  end;
end;

procedure TedtDatabase.ckRemoteDBChange(Sender: TObject);
begin
  if ckRemoteDB.Checked then
  begin
    eDBServer.Enabled := True;
    if DMM.qsConn.FieldByName('database_port').IsNull then
      case TDBManager(cbDBManager.ItemIndex) of
        dbSqlite:
          DMM.qsConn.FieldByName('database_port').Clear;
        dbFirebird:
          DMM.qsConn.FieldByName('database_port').AsInteger := 3050;
        dbPostgre:
          DMM.qsConn.FieldByName('database_port').AsInteger := 5432;
        dbMaria:
          DMM.qsConn.FieldByName('database_port').AsInteger := 3306;
      end;
    eDBPort.Enabled := True;
  end
  else
  begin
    DMM.qsConn.FieldByName('database_server').AsString := 'localhost';
    eDBServer.Enabled := False;
    DMM.qsConn.FieldByName('database_port').Clear;
    eDBPort.Enabled := False;
  end;
end;

procedure TedtDatabase.dsConnDataChange(Sender: TObject; Field: TField);
begin
  if dsConn.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsConn.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtDatabase.dsConnUpdateData(Sender: TObject);
begin
  dsConn.DataSet.FieldByName('user_password').AsString := eDBPass.Text;
end;

procedure TedtDatabase.sbSaveClick(Sender: TObject);
begin
  { #todo : Test database connection }

  edtDatabase.ModalResult := mrOk;
end;

function TedtDatabase.IsRequiredFilled: Boolean;
begin
  Result := False;

  if ckRemoteDB.Checked then
  begin
    if (dsConn.DataSet.FieldByName('connection_name').AsString <> EmptyStr) and
      (dsConn.DataSet.FieldByName('database_type').IsNull = False) and
      (dsConn.DataSet.FieldByName('database_name').AsString <> EmptyStr) and
      (dsConn.DataSet.FieldByName('database_server').AsString <> EmptyStr) and
      (dsConn.DataSet.FieldByName('database_port').IsNull = False) then
      Result := True;
  end else
  begin
    if (dsConn.DataSet.FieldByName('connection_name').AsString <> EmptyStr) and
      (dsConn.DataSet.FieldByName('database_type').IsNull = False) and
      (dsConn.DataSet.FieldByName('database_name').AsString <> EmptyStr) then
      Result := True;
  end;
end;

end.

