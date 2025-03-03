{ Xolmis Data Maintenance dialog

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

unit ufrm_maintenance;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, DateUtils,
  attabs, atshapelinebgra, BCPanel;

type

  { TfrmMaintenance }

  TfrmMaintenance = class(TForm)
    icoRecreateImageThumbnails: TImage;
    icoDatabaseBackup: TImage;
    icoCheckDatabaseIntegrity: TImage;
    icoCheckOptimizeDatabase: TImage;
    icoDatabaseIntegrity: TImage;
    icoOptimizeDatabase: TImage;
    icoSettingsBackup: TImage;
    icoFactoryReset: TImage;
    icoManageLogs: TImage;
    icoClearTemporaryFiles: TImage;
    icoDiagnostic: TImage;
    icoCheckDatabaseBackup: TImage;
    iIcons: TImageList;
    iCheck: TImageList;
    iIconsDark: TImageList;
    iCheckDark: TImageList;
    lblRecreateImageThumbnails: TLabel;
    lblDatabaseBackup: TLabel;
    lblDatabaseIntegrity: TLabel;
    lblOptimizeDatabase: TLabel;
    lblSettingsBackup: TLabel;
    lblFactoryReset: TLabel;
    lblManageLogs: TLabel;
    lblClearTemporaryFiles: TLabel;
    lblDiagnostic: TLabel;
    OpenDlg: TOpenDialog;
    pContent: TPanel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pRecreateImageThumbnails: TBCPanel;
    pDatabaseBackup: TBCPanel;
    pDatabaseIntegrity: TBCPanel;
    pOptimizeDatabase: TBCPanel;
    pSettingsBackup: TBCPanel;
    pFactoryReset: TBCPanel;
    pManageLogs: TBCPanel;
    pClearTemporaryFiles: TBCPanel;
    pDiagnostic: TBCPanel;
    sbClose: TButton;
    sbRecreateImageThumbnails: TBitBtn;
    sbRestoreDatabase: TBitBtn;
    sbBackupDatabase: TBitBtn;
    sbCheckDatabaseIntegrity: TBitBtn;
    sbOptimizeDatabase: TBitBtn;
    sbRestoreSettings: TBitBtn;
    sbFactoryReset: TBitBtn;
    sbClearLogs: TBitBtn;
    sbClearTemporaryFiles: TBitBtn;
    sbDiagnostic: TBitBtn;
    sbBackupSettings: TBitBtn;
    titleRecreateThumbnails: TLabel;
    titleDatabaseBackup: TLabel;
    titleDatabaseIntegrity: TLabel;
    titleOptimizeDatabase: TLabel;
    titleSettingsBackup: TLabel;
    titleFactoryReset: TLabel;
    titleManageLogs: TLabel;
    titleClearTemporaryFiles: TLabel;
    titleDiagnostic: TLabel;
    procedure FormShow(Sender: TObject);
    procedure sbBackupDatabaseClick(Sender: TObject);
    procedure sbClearLogsClick(Sender: TObject);
    procedure sbRecreateImageThumbnailsClick(Sender: TObject);
    procedure sbRestoreDatabaseClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
    procedure CheckDatabaseBackup;
  public

  end;

var
  frmMaintenance: TfrmMaintenance;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_backup, cbs_blobs, cbs_graphics, cbs_data,
  cbs_themes, uDarkStyleParams;

{$R *.lfm}

{ TfrmMaintenance }

procedure TfrmMaintenance.ApplyDarkMode;
begin
  icoDatabaseBackup.Images := iIconsDark;
  icoDatabaseIntegrity.Images := iIconsDark;
  icoOptimizeDatabase.Images := iIconsDark;
  icoSettingsBackup.Images := iIconsDark;
  icoFactoryReset.Images := iIconsDark;
  icoManageLogs.Images := iIconsDark;
  icoClearTemporaryFiles.Images := iIconsDark;
  icoDiagnostic.Images := iIconsDark;
  icoRecreateImageThumbnails.Images := iIconsDark;

  icoCheckDatabaseBackup.Images := iCheckDark;
  icoCheckDatabaseIntegrity.Images := iCheckDark;
  icoCheckOptimizeDatabase.Images := iCheckDark;

  pDatabaseBackup.Background.Color := clSolidBGBaseDark;
  pDatabaseBackup.Border.Color := clSystemSolidNeutralFGDark;
  pDatabaseIntegrity.Background.Color := clSolidBGBaseDark;
  pDatabaseIntegrity.Border.Color := clSystemSolidNeutralFGDark;
  pOptimizeDatabase.Background.Color := clSolidBGBaseDark;
  pOptimizeDatabase.Border.Color := clSystemSolidNeutralFGDark;
  pSettingsBackup.Background.Color := clSolidBGBaseDark;
  pSettingsBackup.Border.Color := clSystemSolidNeutralFGDark;
  pFactoryReset.Background.Color := clSolidBGBaseDark;
  pFactoryReset.Border.Color := clSystemSolidNeutralFGDark;
  pManageLogs.Background.Color := clSolidBGBaseDark;
  pManageLogs.Border.Color := clSystemSolidNeutralFGDark;
  pClearTemporaryFiles.Background.Color := clSolidBGBaseDark;
  pClearTemporaryFiles.Border.Color := clSystemSolidNeutralFGDark;
  pDiagnostic.Background.Color := clSolidBGBaseDark;
  pDiagnostic.Border.Color := clSystemSolidNeutralFGDark;
  pRecreateImageThumbnails.Background.Color := clSolidBGBaseDark;
  pRecreateImageThumbnails.Border.Color := clSystemSolidNeutralFGDark;

  lblDatabaseBackup.Font.Color := $009F9F9F;
  lblDatabaseIntegrity.Font.Color := $009F9F9F;
  lblOptimizeDatabase.Font.Color := $009F9F9F;
  lblSettingsBackup.Font.Color := $009F9F9F;
  lblFactoryReset.Font.Color := $009F9F9F;
  lblManageLogs.Font.Color := $009F9F9F;
  lblClearTemporaryFiles.Font.Color := $009F9F9F;
  lblDiagnostic.Font.Color := $009F9F9F;
  lblRecreateImageThumbnails.Font.Color := $009F9F9F;
end;

procedure TfrmMaintenance.CheckDatabaseBackup;
begin
  icoCheckDatabaseBackup.ImageIndex := 0;
  icoCheckDatabaseBackup.Hint := 'Automatic backup: ENABLED and DONE';

  ConexaoDB.LoadParams;

  case XSettings.AutomaticBackup of
    0:
    begin
      icoCheckDatabaseBackup.ImageIndex := 1;
      icoCheckDatabaseBackup.Hint := 'Automatic backup: DISABLED';
    end;
    1:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 1 then
      begin
        icoCheckDatabaseBackup.ImageIndex := 1;
        icoCheckDatabaseBackup.Hint := 'Automatic backup: ENABLED and NOT DONE';
      end;
    end;
    2:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 7 then
      begin
        icoCheckDatabaseBackup.ImageIndex := 1;
        icoCheckDatabaseBackup.Hint := 'Automatic backup: ENABLED and NOT DONE';
      end;
    end;
    3:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 30 then
      begin
        icoCheckDatabaseBackup.ImageIndex := 1;
        icoCheckDatabaseBackup.Hint := 'Automatic backup: ENABLED and NOT DONE';
      end;
    end;
  end;
end;

procedure TfrmMaintenance.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  CheckDatabaseBackup;
end;

procedure TfrmMaintenance.sbBackupDatabaseClick(Sender: TObject);
begin
  VacuumIntoBackup; //NewBackup;

  CheckDatabaseBackup;
end;

procedure TfrmMaintenance.sbClearLogsClick(Sender: TObject);
var
  FLog: String;
begin
  FLog := ConcatPaths([AppDataDir, LogFile]);
  if FileExists(FLog) then
    DeleteFile(FLog);
end;

procedure TfrmMaintenance.sbRecreateImageThumbnailsClick(Sender: TObject);
begin
  if MsgDlg(rsTitleRecreateThumbnails, rsRecreateThumbnailsPrompt, mtConfirmation) then
    RecreateThumbnails;
end;

procedure TfrmMaintenance.sbRestoreDatabaseClick(Sender: TObject);
begin
  if not MsgDlg(rsTitleRestore, rsRestoreBackupPrompt, mtConfirmation) then
    Exit;

  OpenDlg.InitialDir:= XSettings.BackupFolder;
  if OpenDlg.Execute then
    RestoreBackup(OpenDlg.FileName);
end;

end.

