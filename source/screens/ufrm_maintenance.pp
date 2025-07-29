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
  DB, SQLDB, attabs, atshapelinebgra, BCPanel;

type

  { TfrmMaintenance }

  TfrmMaintenance = class(TForm)
    icoRecreateImageThumbnails: TImage;
    icoDatabaseBackup: TImage;
    icoDatabaseIntegrity: TImage;
    icoOptimizeDatabase: TImage;
    icoSettingsBackup: TImage;
    icoFactoryReset: TImage;
    icoManageLogs: TImage;
    icoClearTemporaryFiles: TImage;
    icoDiagnostic: TImage;
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
    sbDiagnostic: TSpeedButton;
    sbOptimizeDatabase: TSpeedButton;
    sbClearTemporaryFiles: TSpeedButton;
    sbClearLogs: TSpeedButton;
    sbFactoryReset: TSpeedButton;
    sbRestoreSettings: TSpeedButton;
    sbBackupSettings: TSpeedButton;
    sbRestoreDatabase: TSpeedButton;
    sbBackupDatabase: TSpeedButton;
    sbCheckDatabaseIntegrity: TSpeedButton;
    sbRecreateImageThumbnails: TSpeedButton;
    TimerOpen: TTimer;
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
    procedure btnBackupDatabaseClick(Sender: TObject);
    procedure btnClearLogsClick(Sender: TObject);
    procedure btnRecreateImageThumbnailsClick(Sender: TObject);
    procedure btnRestoreDatabaseClick(Sender: TObject);
    procedure sbBackupSettingsClick(Sender: TObject);
    procedure sbCheckDatabaseIntegrityClick(Sender: TObject);
    procedure sbClearTemporaryFilesClick(Sender: TObject);
    procedure sbDiagnosticClick(Sender: TObject);
    procedure sbFactoryResetClick(Sender: TObject);
    procedure sbOptimizeDatabaseClick(Sender: TObject);
    procedure sbRestoreSettingsClick(Sender: TObject);
    procedure TimerOpenTimer(Sender: TObject);
  private
    procedure ApplyDarkMode;
    procedure CheckDatabaseBackup;
    procedure CheckDatabaseIntegrity;
    procedure CheckSystemLogs;
    procedure CheckTemporaryFiles;
    procedure CheckThumbnails;
  public

  end;

var
  frmMaintenance: TfrmMaintenance;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_backup, cbs_blobs, cbs_system,
  cbs_themes, udlg_diagnostic, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TfrmMaintenance }

procedure TfrmMaintenance.ApplyDarkMode;
begin
  icoDatabaseBackup.Images := iCheckDark;
  icoDatabaseIntegrity.Images := iCheckDark;
  icoOptimizeDatabase.Images := iCheckDark;

  icoSettingsBackup.Images := iIconsDark;
  icoFactoryReset.Images := iIconsDark;
  icoManageLogs.Images := iCheckDark;
  icoClearTemporaryFiles.Images := iIconsDark;
  icoDiagnostic.Images := iIconsDark;
  icoRecreateImageThumbnails.Images := iIconsDark;

  sbBackupDatabase.Images := iIconsDark;
  sbRestoreDatabase.Images := iIconsDark;
  sbCheckDatabaseIntegrity.Images := iIconsDark;
  sbOptimizeDatabase.Images := iIconsDark;
  sbBackupSettings.Images := iIconsDark;
  sbRestoreSettings.Images := iIconsDark;
  sbFactoryReset.Images := iIconsDark;
  sbClearLogs.Images := iIconsDark;
  sbClearTemporaryFiles.Images := iIconsDark;
  sbDiagnostic.Images := iIconsDark;
  sbRecreateImageThumbnails.Images := iIconsDark;

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
  icoDatabaseBackup.ImageIndex := 0;
  icoDatabaseBackup.Hint := rsHintBackupEnabledDone;
  lblDatabaseBackup.Caption := rsCaptionEnabled;

  ConexaoDB.LoadParams;

  case XSettings.AutomaticBackup of
    0:
    begin
      icoDatabaseBackup.ImageIndex := 1;
      icoDatabaseBackup.Hint := rsHintBackupDisabled;
      lblDatabaseBackup.Caption := rsCaptionDisabled;
    end;
    1:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 1 then
      begin
        icoDatabaseBackup.ImageIndex := 1;
        icoDatabaseBackup.Hint := rsHintBackupEnabledNotDone;
      end;
    end;
    2:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 7 then
      begin
        icoDatabaseBackup.ImageIndex := 1;
        icoDatabaseBackup.Hint := rsHintBackupEnabledNotDone;
      end;
    end;
    3:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 30 then
      begin
        icoDatabaseBackup.ImageIndex := 1;
        icoDatabaseBackup.Hint := rsHintBackupEnabledNotDone;
      end;
    end;
  end;
end;

procedure TfrmMaintenance.CheckDatabaseIntegrity;
begin
  if ConexaoDB.IntegrityCheck(False) then
  begin
    icoDatabaseIntegrity.ImageIndex := 0;
    icoDatabaseIntegrity.Hint := rsSuccessfulDatabaseIntegrityCheck;
  end
  else
  begin
    icoDatabaseIntegrity.ImageIndex := 2;
    icoDatabaseIntegrity.Hint := rsIntegrityCheckReturnedErrors;
  end;
  icoDatabaseIntegrity.ShowHint := True;
end;

procedure TfrmMaintenance.CheckSystemLogs;
const
  KB = 1024;         // 1 KB = 1024 bytes
  MB = 1024 * KB;    // 1 MB = 1024 KB
  GB = 1024 * MB;    // 1 GB = 1024 MB
var
  FileInfo: TSearchRec;
  TotalSize: string;
  SizeInBytes: Int64;
begin
  SizeInBytes := 0;
  TotalSize := EmptyStr;

  if XSettings.AllowWriteLogs then
  begin
    icoManageLogs.ImageIndex := 0;
    icoManageLogs.Hint := rsCaptionEnabled
  end
  else
  begin
    icoManageLogs.ImageIndex := 1;
    icoManageLogs.Hint := rsCaptionDisabled;
  end;

  // Check if directory exists
  if not DirectoryExists(AppDataDir) then
    raise EDirectoryNotFoundException.CreateFmt(rsErrorFolderNotFound, [AppDataDir]);

  // Find files in directory
  if FindFirst(IncludeTrailingPathDelimiter(AppDataDir) + '*.txt', faAnyFile, FileInfo) = 0 then
  try
    repeat
      // Ignore "." and ".."
      if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
      begin
        SizeInBytes := SizeInBytes + FileInfo.Size;
      end;
    until FindNext(FileInfo) <> 0;
  finally
    FindClose(FileInfo);
  end;

  if SizeInBytes >= GB then
    TotalSize := Format('%.2f GB', [SizeInBytes / GB])
  else if SizeInBytes >= MB then
    TotalSize := Format('%.2f MB', [SizeInBytes / MB])
  else if SizeInBytes >= KB then
    TotalSize := Format('%.2f KB', [SizeInBytes / KB])
  else
    TotalSize := Format('%d bytes', [SizeInBytes]);

  lblManageLogs.Caption := Format('%s / %s', [GetFileSizeReadable(ConcatPaths([AppDataDir, LogFile])), TotalSize]);
end;

procedure TfrmMaintenance.CheckTemporaryFiles;
const
  KB = 1024;         // 1 KB = 1024 bytes
  MB = 1024 * KB;    // 1 MB = 1024 KB
  GB = 1024 * MB;    // 1 GB = 1024 MB
var
  SizeInBytes: Int64;
begin
  SizeInBytes := GetDirectorySize(ConcatPaths([AppDataDir, 'map-cache']));

  if SizeInBytes >= GB then
    lblClearTemporaryFiles.Caption := Format('%.2f GB', [SizeInBytes / GB])
  else if SizeInBytes >= MB then
    lblClearTemporaryFiles.Caption := Format('%.2f MB', [SizeInBytes / MB])
  else if SizeInBytes >= KB then
    lblClearTemporaryFiles.Caption := Format('%.2f KB', [SizeInBytes / KB])
  else
    lblClearTemporaryFiles.Caption := Format('%d bytes', [SizeInBytes]);
end;

procedure TfrmMaintenance.CheckThumbnails;
var
  Qry: TSQLQuery;
  ImageCounter: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    Add('SELECT COUNT(image_id) AS counter FROM images');
    Add('WHERE (image_filename NOTNULL)');
    Open;
    ImageCounter := FieldByName('counter').AsInteger;
    Close;

    lblRecreateImageThumbnails.Caption := IntToStr(ImageCounter) + ' ' + LowerCase(rsCaptionImages);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmMaintenance.FormShow(Sender: TObject);
//var
//  ScreenScale: Single;
  //IconWidth: Integer;
begin
  //ScreenScale := Screen.PixelsPerInch / 96;
  //IconWidth := Round(iIcons.Width * ScreenScale);

  if IsDarkModeEnabled then
    ApplyDarkMode;

  //icoDatabaseBackup.ImageWidth := IconWidth;
  //icoDatabaseIntegrity.ImageWidth := IconWidth;
  //icoOptimizeDatabase.ImageWidth := IconWidth;
  //icoSettingsBackup.ImageWidth := IconWidth;
  //icoFactoryReset.ImageWidth := IconWidth;
  //icoManageLogs.ImageWidth := IconWidth;
  //icoClearTemporaryFiles.ImageWidth := IconWidth;
  //icoDiagnostic.ImageWidth := IconWidth;
  //icoRecreateImageThumbnails.ImageWidth := IconWidth;

  CheckDatabaseBackup;
  CheckSystemLogs;
  CheckTemporaryFiles;
  CheckThumbnails;
  //CheckDatabaseIntegrity;

  TimerOpen.Enabled := True;
end;

procedure TfrmMaintenance.sbBackupSettingsClick(Sender: TObject);
begin
  BackupSettings;
end;

procedure TfrmMaintenance.sbCheckDatabaseIntegrityClick(Sender: TObject);
begin
  ConexaoDB.IntegrityCheck;
end;

procedure TfrmMaintenance.sbClearTemporaryFilesClick(Sender: TObject);
var
  FileInfo: TSearchRec;
  Dir, FilePath: string;
begin
  Dir := ConcatPaths([AppDataDir, 'map-cache\']);

  // Check if directory exists
  if not DirectoryExists(Dir) then
    raise EDirectoryNotFoundException.CreateFmt(rsErrorFolderNotFound, [Dir]);

  // Find files in directory
  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, FileInfo) = 0 then
  try
    repeat
      // Ignore "." and ".."
      if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
      begin
        FilePath := IncludeTrailingPathDelimiter(Dir) + FileInfo.Name;

        // Check if is a file and delete it
        if (FileInfo.Attr and faDirectory) = 0 then
        begin
          if not DeleteFile(FilePath) then
            raise Exception.CreateFmt(rsErrorDeletingFile, [FilePath]);
        end;
      end;
    until FindNext(FileInfo) <> 0;
  finally
    FindClose(FileInfo);
  end;

  MsgDlg(rsTitleInformation, rsSuccessfulClearTemporaryFiles, mtInformation)
end;

procedure TfrmMaintenance.sbDiagnosticClick(Sender: TObject);
begin
  dlgDiagnostic := TdlgDiagnostic.Create(nil);
  try
    dlgDiagnostic.ShowModal;
  finally
    FreeAndNil(dlgDiagnostic);
  end;
end;

procedure TfrmMaintenance.sbFactoryResetClick(Sender: TObject);
begin
  if MsgDlg(rsTitleConfirmation, rsFactoryResetPrompt, mtConfirmation) then
  begin
    XSettings.Reset;
  end;
end;

procedure TfrmMaintenance.sbOptimizeDatabaseClick(Sender: TObject);
begin
  ConexaoDB.Optimize;
end;

procedure TfrmMaintenance.sbRestoreSettingsClick(Sender: TObject);
begin
  OpenDlg.InitialDir := XSettings.BackupFolder;
  if OpenDlg.Execute then
    if RestoreSettings(OpenDlg.FileName) then
      XSettings.LoadFromFile;
end;

procedure TfrmMaintenance.TimerOpenTimer(Sender: TObject);
begin
  TimerOpen.Enabled := False;

  //CheckDatabaseBackup;
  //CheckSystemLogs;
  //CheckTemporaryFiles;
  //CheckThumbnails;
  CheckDatabaseIntegrity;
end;

procedure TfrmMaintenance.btnBackupDatabaseClick(Sender: TObject);
begin
  VacuumIntoBackup; //NewBackup;

  CheckDatabaseBackup;
end;

procedure TfrmMaintenance.btnClearLogsClick(Sender: TObject);
var
  FileInfo: TSearchRec;
  FilePath: string;
  //FLog: String;
begin
  if not MsgDlg(rsTitleConfirmation, rsClearLogsPrompt, mtConfirmation) then
    Exit;

  // Find files in directory
  if FindFirst(IncludeTrailingPathDelimiter(AppDataDir) + '*.txt', faAnyFile, FileInfo) = 0 then
  try
    repeat
      // Ignore "." and ".."
      if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
      begin
        FilePath := IncludeTrailingPathDelimiter(AppDataDir) + FileInfo.Name;

        // Check if is a file and delete it
        if (FileInfo.Attr and faDirectory) = 0 then
        begin
          if not DeleteFile(FilePath) then
            raise Exception.CreateFmt(rsErrorDeletingFile, [FilePath]);
        end;
      end;
    until FindNext(FileInfo) <> 0;
  finally
    FindClose(FileInfo);
  end;

  //FLog := ConcatPaths([AppDataDir, LogFile]);
  //if FileExists(FLog) then
  //  DeleteFile(FLog);
end;

procedure TfrmMaintenance.btnRecreateImageThumbnailsClick(Sender: TObject);
begin
  if MsgDlg(rsTitleRecreateThumbnails, rsRecreateThumbnailsPrompt, mtConfirmation) then
    RecreateThumbnails;
end;

procedure TfrmMaintenance.btnRestoreDatabaseClick(Sender: TObject);
begin
  if not MsgDlg(rsTitleRestore, rsRestoreBackupPrompt, mtConfirmation) then
    Exit;

  OpenDlg.InitialDir := XSettings.BackupFolder;
  if OpenDlg.Execute then
    RestoreBackup(OpenDlg.FileName);
end;

end.

