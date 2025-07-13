{ Xolmis Backup and Restore library

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

unit cbs_backup;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, Zipper, lazfileutils, LCLIntf, fileutil,
  { VCL }
  Forms, Controls, Dialogs,
  { Data }
  SQLDB, sqlite3conn, sqlite3backup,
  { Forms }
  udm_main;

  function NewBackup: Boolean;
  function VacuumIntoBackup(const SuccessMessage: Boolean = True): Boolean;
  function RestoreBackup(aFilename: String): Boolean;
  procedure OnBkpProgress(Sender: TObject; Remaining, PageCount: integer);
  procedure ZipperProgress(Sender: TObject; const Pct: Double);
  procedure RunStartupBackup;

  function BackupSettings: Boolean;
  function RestoreSettings(aFilename: String): Boolean;

implementation

uses cbs_locale, cbs_global, cbs_dialogs, udlg_progress, udlg_loading;

function NewBackup: boolean;
var
  dbName, bkpName, zipName, tmpName: String;
  fzip: TZipper;
  Bck: TSQLite3Backup;
  BckConn: TSQLite3Connection;
  BckTrans: TSQLTransaction;
  aBckProgress: TOnBackupProgress;
begin
  Result := False;

  if not MsgDlg(rsTitleBackup, rsPromptBackupNow, mtConfirmation) then
    Exit;

  if not FileExists(ConexaoDB.Database) then
  begin
    raise EFileNotFoundException.CreateFmt(rsErrorDatabaseNotFound, [ConexaoDB.Database]);
    // Abort;
  end;

  DMM.sqlCon.Close;
  try
    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsTitleBackup;
      dlgProgress.Text := rsPreparingBackup;
      dlgProgress.Min := 0;
      dlgProgress.Max := 100;
      if not (DirectoryExists(XSettings.BackupFolder)) then
        CreateDir(XSettings.BackupFolder);
      dbName := ExtractFileNameWithoutExt(ExtractFileName(DMM.sqlCon.DatabaseName));
      bkpName := Format('backup_%s_%s.sbk', [dbName, FormatDateTime('yyyyMMdd_HHmm', Now)]);
      tmpName := ConcatPaths([TempDir, bkpName]);
      bkpName := ConcatPaths([XSettings.BackupFolder, ChangeFileExt(bkpName, '.zip')]);
      dlgProgress.Max := 100;
      //F_Main.Taskbar.ProgressMaxValue := 100;
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.Normal;
      LogEvent(leaStart, 'Backup: ' + ConexaoDB.Database + ' -> ' + tmpName);
      Application.ProcessMessages;
      BckConn := TSQLite3Connection.Create(nil);
      BckTrans := TSQLTransaction.Create(BckConn);
      BckConn.Transaction := BckTrans;
      BckConn.OpenFlags := [sofReadOnly];
      BckConn.DatabaseName := DMM.sqlCon.DatabaseName;
      BckConn.CharSet := DMM.sqlCon.CharSet;
      BckConn.Params.Add('StringFormat=Unicode');
      BckConn.Params.Add('LockingMode=Exclusive');
      BckConn.Params.Add('Synchronous=Off');

      Bck := TSQLite3Backup.Create;
      try
        //F_Main.Cursor := crHourGlass;
        TMethod(aBckProgress).Code := @OnBkpProgress;
        TMethod(aBckProgress).Data := Pointer(Bck);
        dlgProgress.Text := rsCreatingBackup;
        Bck.OnBackupProgress := aBckProgress;
        Result := Bck.Backup(BckConn, tmpName, False, 'main');
      finally
        Bck.Free;
        BckTrans.Free;
        BckConn.Free;
      end;

      if FileExists(tmpName) then
      begin
        dlgProgress.Text := rsCompressingBackup;
        zipName := ChangeFileExt(tmpName, '.zip');
        LogEvent(leaStart, 'Compressing backup: ' + ExtractFileName(zipName));
        dlgProgress.Max := 100;
        dlgProgress.Position := 0;
        //F_Main.Taskbar.ProgressMaxValue := 100;
        fzip := TZipper.Create;
        try
          //fzip.OnProgress := @ZipperProgress;
          fzip.ZipFile(zipName, tmpName);
        finally
          fzip.Free;
        end;
        CopyFile(zipName, bkpName);
        DeleteFile(PChar(tmpName));
        DeleteFile(PChar(zipName));
        LogEvent(leaFinish, 'Compressing backup: ' + ExtractFileName(zipName));

        if FileExists(bkpName) then
        begin
          ConexaoDB.SetLastBackup;
          MsgDlg(rsTitleBackup, Format(rsSuccessfulBackup, [ExtractFileName(bkpName)]), mtInformation)
        end
        else
          MsgDlg(rsTitleBackup, rsErrorBackupFailed, mtError);
      end;
    finally
      dlgProgress.Position := 100;
      dlgProgress.Text := rsProgressFinishing;
      LogEvent(leaFinish, 'Backup: ' + bkpName);
      //F_Main.Cursor := crDefault;
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.None;
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  finally
    DMM.sqlCon.Open;
  end;

  Result := true;
end;

function VacuumIntoBackup(const SuccessMessage: Boolean): Boolean;
var
  fzip: TZipper;
  dbName, bkpName: String;
  tmpName: String;
  zipName: String;
begin
  Result := False;
  LogEvent(leaStart, 'Backup: ' + ConexaoDB.Database + ' -> ' + tmpName);

  //if not MsgDlg(rsTitleBackup, rsPromptBackupNow, mtConfirmation) then
  //  Exit;

  if not FileExists(ConexaoDB.Database) then
  begin
    raise EFileNotFoundException.CreateFmt(rsErrorDatabaseNotFound, [ConexaoDB.Database]);
    // Abort;
  end;
  dlgLoading.Show;
  dlgLoading.UpdateProgress(rsPreparingBackup, -1);

  //dlgProgress := TdlgProgress.Create(nil);
  try
    //dlgProgress.Show;
    //dlgProgress.Title := rsTitleBackup;
    //dlgProgress.Text := rsPreparingBackup;
    //dlgProgress.Min := 0;
    //dlgProgress.Max := 100;
    //dlgProgress.Indeterminate := True;

    if not (DirectoryExists(XSettings.BackupFolder)) then
      CreateDir(XSettings.BackupFolder);
    dbName := ExtractFileNameWithoutExt(ExtractFileName(DMM.sqlCon.DatabaseName));
    bkpName := Format('backup_%s_%s.sbk', [dbName, FormatDateTime('yyyyMMdd_HHmm', Now)]);
    tmpName := ConcatPaths([TempDir, bkpName]);
    bkpName := ConcatPaths([XSettings.BackupFolder, ChangeFileExt(bkpName, '.zip')]);

    //F_Main.Taskbar.ProgressMaxValue := 100;
    //F_Main.Taskbar.ProgressState := TTaskBarProgressState.Normal;

    Application.ProcessMessages;

    try
      //F_Main.Cursor := crHourGlass;
      dlgLoading.UpdateProgress(rsCreatingBackup, -1);
      //dlgProgress.Text := rsCreatingBackup;
      //Application.ProcessMessages;

      DMM.sqlCon.ExecuteDirect('END TRANSACTION');
      Sleep(200);
      DMM.sqlCon.ExecuteDirect('VACUUM INTO ' + QuotedStr(tmpName));
      Sleep(200);
      DMM.sqlCon.ExecuteDirect('BEGIN TRANSACTION');
      Application.ProcessMessages;

      //DMM.sqlTrans.CommitRetaining;
    except
      //DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

    if FileExists(tmpName) then
    begin
      dlgLoading.UpdateProgress(rsCompressingBackup, 0);
      //dlgProgress.Indeterminate := False;
      //dlgProgress.Text := rsCompressingBackup;
      zipName := ChangeFileExt(tmpName, '.zip');
      LogEvent(leaStart, 'Compressing backup: ' + ExtractFileName(zipName));
      //dlgProgress.Max := 100;
      //dlgProgress.Position := 0;
      //F_Main.Taskbar.ProgressMaxValue := 100;
      fzip := TZipper.Create;
      try
        fzip.OnProgress := @dlgLoading.ZipperProgress;
        //fzip.OnPercent := 1;
        fzip.FileName := zipName;
        fzip.Entries.AddFileEntry(tmpName, ExtractFileName(tmpName));
        fzip.ZipAllFiles;
      finally
        fzip.Free;
      end;
      CopyFile(zipName, bkpName);
      DeleteFile(PChar(tmpName));
      DeleteFile(PChar(zipName));
      LogEvent(leaFinish, 'Compressing backup: ' + ExtractFileName(zipName));

      if FileExists(bkpName) then
      begin
        dlgLoading.UpdateProgress(rsSuccessfulBackup, 100);
        dlgLoading.Hide;
        Result := True;
        ConexaoDB.SetLastBackup;
        if SuccessMessage then
          MsgDlg(rsTitleBackup, Format(rsSuccessfulBackup, [ExtractFileName(bkpName)]), mtInformation)
      end
      else
        MsgDlg(rsTitleBackup, rsErrorBackupFailed, mtError);
    end;
  finally
    //dlgProgress.Position := 100;
    //dlgProgress.Text := rsProgressFinishing;
    //Application.ProcessMessages;
    LogEvent(leaFinish, 'Backup: ' + bkpName);
    //F_Main.Cursor := crDefault;
    //F_Main.Taskbar.ProgressState := TTaskBarProgressState.None;
    //dlgProgress.Close;
    //FreeAndNil(dlgProgress);
  end;

  Result := true;
end;

function RestoreBackup(aFilename: String): boolean;
var
  bkpName: String;
  fzip: TUnZipper;
  Bck: TSQLite3Backup;
  BckConn: TSQLite3Connection;
  BckTrans: TSQLTransaction;
  aBckProgress: TOnBackupProgress;
  zipped: Boolean;
begin
  Result := False;

  if not FileExists(aFilename) then
  begin
    raise EFileNotFoundException.CreateFmt(rsErrorBackupNotFound, [aFilename]);
  end;

  DMM.sqlCon.Close;
  try
    bkpName := aFilename;
    zipped := False;
    //dlgProgress := TdlgProgress.Create(nil);
    try
      dlgLoading.Show;
      dlgLoading.UpdateProgress(rsPreparingRestore, -1);
      //dlgProgress.Show;
      //dlgProgress.Title := rsTitleRestore;
      //dlgProgress.Text := rsPreparingRestore;
      //dlgProgress.Min := 0;
      //dlgProgress.Max := 100;
      LogEvent(leaStart, 'Restore backup: ' + aFilename + ' --> ' + DMM.sqlCon.DatabaseName);
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.Normal;
      Application.ProcessMessages;
        //F_Main.Cursor := crHourGlass;
        if ExtractFileExt(aFilename) = '.zip' then
        begin
          dlgLoading.UpdateProgress(rsDecompressingBackup, 0);
          //dlgProgress.Text := rsDecompressingBackup;
          bkpName := TempDir + ChangeFileExt(ExtractFileName(aFilename), '.sbk');
          zipped := True;
          LogEvent(leaStart, 'Decompressing backup: ' + aFilename);
          //dlgProgress.Max := 100;
          //F_Main.Taskbar.ProgressMaxValue := 100;
          fzip := TUnZipper.Create;
          try
            fzip.OnProgress := @dlgLoading.ZipperProgress;
            fzip.OutputPath := TempDir;
            fzip.Unzip(aFilename);
          finally
            fzip.Free;
          end;
          LogEvent(leaFinish, 'Decompressing backup: ' + aFilename);
        end;

        if FileExists(bkpName) then
        begin
          BckConn := TSQLite3Connection.Create(nil);
          BckTrans := TSQLTransaction.Create(BckConn);
          BckConn.Transaction := BckTrans;
          BckConn.DatabaseName := DMM.sqlCon.DatabaseName;
          BckConn.CharSet := DMM.sqlCon.CharSet;
          BckConn.OpenFlags := [sofReadWrite];
          BckConn.Params.Add('StringFormat=Unicode');
          BckConn.Params.Add('LockingMode=Exclusive');
          BckConn.Params.Add('Synchronous=Off');
          Bck := TSQLite3Backup.Create;
          try
            dlgLoading.UpdateProgress(rsRestoringBackup, 0);
            //TMethod(aBckProgress).Code := @OnBkpProgress;
            //TMethod(aBckProgress).Data := Pointer(Bck);
            //dlgProgress.Text := rsRestoringBackup;
            //Bck.OnBackupProgress := aBckProgress;
            Bck.OnBackupProgress := @dlgLoading.BackupProgress;
            Result := Bck.Restore(bkpName, bckConn, False, 'main');
          finally
            Bck.Free;
            BckTrans.Free;
            BckConn.Free;
          end;

          if zipped then
            DeleteFile(PChar(bkpName));
        end;

        dlgLoading.UpdateProgress(rsProgressFinishing, 100);
        dlgLoading.Hide;

        if Result then
          MsgDlg(rsTitleRestore, Format(rsSuccessfulRestore, [ExtractFileName(aFilename)]), mtInformation)
        else
          MsgDlg(rsTitleRestore, rsErrorRestoreFailed, mtError);
    finally
      //dlgProgress.Position := 100;
      //dlgProgress.Text := rsProgressFinishing;
      LogEvent(leaFinish, 'Restore backup: ' + aFilename);
      //F_Main.Cursor := crDefault;
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.None;
      //dlgProgress.Close;
      //FreeAndNil(dlgProgress);
    end;
  finally
    DMM.sqlCon.Open;
  end;
end;

procedure OnBkpProgress(Sender: TObject; Remaining, PageCount: integer);
begin
  if Assigned(dlgProgress) then
  begin
    if dlgProgress.Max = 100 then
      dlgProgress.Max := PageCount;
    dlgProgress.Position := dlgProgress.Max - PageCount;
    //dlgProgress.Position := 100 * (PageCount - Remaining) div PageCount;
    //F_Main.Taskbar.ProgressValue := dlgProgress.PBar.Position;
    Application.ProcessMessages;
  end;
end;

procedure RunStartupBackup;
begin
  if DMM.sqlCon.Connected then
    DMM.sqlCon.Close;

  if FileExists(ConexaoDB.Database) then
  begin
    if not(DirectoryExists(XSettings.BackupFolder)) then
    begin
      CreateDir(XSettings.BackupFolder);
    end;

    NewBackup;
  end;
end;

procedure ZipperProgress(Sender: TObject; const Pct: Double);
begin
  if Assigned(dlgProgress) then
  begin
    dlgProgress.Position := Round(Pct);
    Application.ProcessMessages;
  end;
  if dlgLoading.Visible then
    dlgLoading.UpdateProgress(rsCompressingBackup, Round(Pct));
end;

function BackupSettings: Boolean;
var
  zipPath, zipName, colsPath, settingsPath, FilePath, TargetPath: String;
  fzip: TZipper;
  FileInfo: TSearchRec;
begin
  Result := False;

  if not (DirectoryExists(XSettings.BackupFolder)) then
    CreateDir(XSettings.BackupFolder);

  settingsPath := ConcatPaths([AppDataDir, DefaultSettingsFile]);
  zipName := Format('backup_settings_%s.zip', [FormatDateTime('yyyyMMdd_HHmm', Now)]);
  zipPath := ConcatPaths([XSettings.BackupFolder, zipName]);
  colsPath := ConcatPaths([AppDataDir, 'columns\']);
  LogEvent(leaStart, 'Compressing settings backup: ' + zipName);

  //F_Main.Taskbar.ProgressMaxValue := 100;
  fzip := TZipper.Create;
  try
    //fzip.OnProgress := @ZipperProgress;
    fzip.FileName := zipPath;
    try
      fzip.Entries.AddFileEntry(settingsPath, ExtractFileName(settingsPath));
      if FindFirstUTF8(ConcatPaths([colsPath, '*']), faAnyFile, FileInfo) = 0 then
      try
        repeat
          // Ignore "." e ".."
          if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
          begin
            FilePath := colsPath + FileInfo.Name;
            TargetPath := 'columns\' + FileInfo.Name;
            fzip.Entries.AddFileEntry(FilePath, TargetPath);
          end;
        until FindNextUTF8(FileInfo) <> 0;
      finally
        FindCloseUTF8(FileInfo);
      end;
      fzip.ZipAllFiles;
    except
      raise;
    end;
  finally
    fzip.Free;
  end;
  //CopyFile(zipName, bkpName);
  //DeleteFile(PChar(tmpName));
  //DeleteFile(PChar(zipName));
  LogEvent(leaFinish, 'Compressing settings backup: ' + zipName);

  if FileExists(zipPath) then
  begin
    Result := True;
    MsgDlg(rsTitleBackup, Format(rsSuccessfulBackup, [zipName]), mtInformation)
  end
  else
    MsgDlg(rsTitleBackup, rsErrorBackupFailed, mtError);
end;

function RestoreSettings(aFilename: String): Boolean;
var
  fzip: TUnZipper;
  targetPath: String;
begin
  Result := False;

  if not FileExists(aFilename) then
    Exit;

  targetPath := AppDataDir;

  fzip := TUnZipper.Create;
  try
    fzip.FileName := aFilename;
    fzip.OutputPath := TargetPath;
    try
      fzip.UnZipAllFiles;
    except
      raise;
    end;
  finally
    fzip.Free;
  end;

  Result := True;
  MsgDlg(rsTitleRestore, Format(rsSuccessfulRestore, [ExtractFileName(aFilename)]), mtInformation);
end;

end.
