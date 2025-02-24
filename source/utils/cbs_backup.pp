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
  function VacuumIntoBackup: Boolean;
  function RestoreBackup(aFilename: String): Boolean;
  procedure OnBkpProgress(Sender: TObject; Remaining, PageCount: integer);
  procedure ZipperProgress(Sender: TObject; const Pct: Double);
  procedure RunStartupBackup;

implementation

uses cbs_locale, cbs_global, cbs_dialogs, udlg_progress;

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
    raise Exception.Create(Format(rsErrorDatabaseNotFound, [ConexaoDB.Database]));
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
      {$IFDEF DEBUG}
      LogDebug('START Backup: ' + ConexaoDB.Database + ' -> ' + tmpName);
      {$ENDIF}
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
        {$IFDEF DEBUG}
        LogDebug('START Compressing backup: ' + ExtractFileName(zipName));
        {$ENDIF}
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
        {$IFDEF DEBUG}
        LogDebug('FINISH Compressing backup: ' + ExtractFileName(zipName));
        {$ENDIF}

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
      {$IFDEF DEBUG}
      LogDebug('FINISH Backup: ' + bkpName);
      {$ENDIF}
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

function VacuumIntoBackup: Boolean;
var
  fzip: TZipper;
  dbName, bkpName: String;
  tmpName: String;
  zipName: String;
begin
  Result := False;

  if not MsgDlg(rsTitleBackup, rsPromptBackupNow, mtConfirmation) then
    Exit;

  if not FileExists(ConexaoDB.Database) then
  begin
    raise Exception.Create(Format(rsErrorDatabaseNotFound, [ConexaoDB.Database]));
    // Abort;
  end;


    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsTitleBackup;
      dlgProgress.Text := rsPreparingBackup;
      dlgProgress.Min := 0;
      dlgProgress.Max := 100;
      dlgProgress.Indeterminate := True;

      if not (DirectoryExists(XSettings.BackupFolder)) then
        CreateDir(XSettings.BackupFolder);
      dbName := ExtractFileNameWithoutExt(ExtractFileName(DMM.sqlCon.DatabaseName));
      bkpName := Format('backup_%s_%s.sbk', [dbName, FormatDateTime('yyyyMMdd_HHmm', Now)]);
      tmpName := ConcatPaths([TempDir, bkpName]);
      bkpName := ConcatPaths([XSettings.BackupFolder, ChangeFileExt(bkpName, '.zip')]);

      //F_Main.Taskbar.ProgressMaxValue := 100;
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.Normal;
      {$IFDEF DEBUG}
      LogDebug('START Backup: ' + ConexaoDB.Database + ' -> ' + tmpName);
      {$ENDIF}
      Application.ProcessMessages;

      try
        //F_Main.Cursor := crHourGlass;
        dlgProgress.Text := rsCreatingBackup;
        Application.ProcessMessages;

        DMM.sqlCon.ExecuteDirect('END TRANSACTION');
        DMM.sqlCon.ExecuteDirect('VACUUM INTO ' + QuotedStr(tmpName));
        DMM.sqlCon.ExecuteDirect('BEGIN TRANSACTION');

        //DMM.sqlTrans.CommitRetaining;
      except
        //DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      if FileExists(tmpName) then
      begin
        dlgProgress.Indeterminate := False;
        dlgProgress.Text := rsCompressingBackup;
        zipName := ChangeFileExt(tmpName, '.zip');
        {$IFDEF DEBUG}
        LogDebug('START Compressing backup: ' + ExtractFileName(zipName));
        {$ENDIF}
        dlgProgress.Max := 100;
        dlgProgress.Position := 0;
        //F_Main.Taskbar.ProgressMaxValue := 100;
        fzip := TZipper.Create;
        try
          //fzip.OnProgress := @ZipperProgress;
          fzip.FileName := zipName;
          fzip.Entries.AddFileEntry(tmpName, ExtractFileName(tmpName));
          fzip.ZipAllFiles;
        finally
          fzip.Free;
        end;
        CopyFile(zipName, bkpName);
        DeleteFile(PChar(tmpName));
        DeleteFile(PChar(zipName));
        {$IFDEF DEBUG}
        LogDebug('FINISH Compressing backup: ' + ExtractFileName(zipName));
        {$ENDIF}

        if FileExists(bkpName) then
        begin
          Result := True;
          ConexaoDB.SetLastBackup;
          MsgDlg(rsTitleBackup, Format(rsSuccessfulBackup, [ExtractFileName(bkpName)]), mtInformation)
        end
        else
          MsgDlg(rsTitleBackup, rsErrorBackupFailed, mtError);
      end;
    finally
      dlgProgress.Position := 100;
      dlgProgress.Text := rsProgressFinishing;
      Application.ProcessMessages;
      {$IFDEF DEBUG}
      LogDebug('FINISH Backup: ' + bkpName);
      {$ENDIF}
      //F_Main.Cursor := crDefault;
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.None;
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
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
    raise Exception.Create(Format(rsErrorBackupNotFound, [aFilename]));
  end;

  DMM.sqlCon.Close;
  try
    bkpName := aFilename;
    zipped := False;
    dlgProgress := TdlgProgress.Create(nil);
    try
      dlgProgress.Show;
      dlgProgress.Title := rsTitleRestore;
      dlgProgress.Text := rsPreparingRestore;
      dlgProgress.Min := 0;
      dlgProgress.Max := 100;
      {$IFDEF DEBUG}
      LogDebug('START Restore backup: ' + aFilename + ' --> ' + DMM.sqlCon.DatabaseName);
      {$ENDIF}
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.Normal;
      Application.ProcessMessages;
        //F_Main.Cursor := crHourGlass;
        if ExtractFileExt(aFilename) = '.zip' then
        begin
          dlgProgress.Text := rsDecompressingBackup;
          bkpName := TempDir + ChangeFileExt(ExtractFileName(aFilename), '.sbk');
          zipped := True;
          {$IFDEF DEBUG}
          LogDebug('START Decompressing backup: ' + aFilename);
          {$ENDIF}
          dlgProgress.Max := 100;
          //F_Main.Taskbar.ProgressMaxValue := 100;
          fzip := TUnZipper.Create;
          try
            //fzip.OnProgress := @ZipperProgress;
            fzip.OutputPath := TempDir;
            fzip.Unzip(aFilename);
          finally
            fzip.Free;
          end;
          {$IFDEF DEBUG}
          LogDebug('FINISH Decompressing backup: ' + aFilename);
          {$ENDIF}
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
            TMethod(aBckProgress).Code := @OnBkpProgress;
            TMethod(aBckProgress).Data := Pointer(Bck);
            dlgProgress.Text := rsRestoringBackup;
            Bck.OnBackupProgress := aBckProgress;
            Result := Bck.Restore(bkpName, bckConn, False, 'main');
          finally
            Bck.Free;
            BckTrans.Free;
            BckConn.Free;
          end;

          if zipped then
            DeleteFile(PChar(bkpName));
        end;

        if Result then
          MsgDlg(rsTitleRestore, Format(rsSuccessfulRestore, [ExtractFileName(aFilename)]), mtInformation)
        else
          MsgDlg(rsTitleRestore, rsErrorRestoreFailed, mtError);
    finally
      dlgProgress.Position := 100;
      dlgProgress.Text := rsProgressFinishing;
      {$IFDEF DEBUG}
      LogDebug('FINISH Restore backup: ' + aFilename);
      {$ENDIF}
      //F_Main.Cursor := crDefault;
      //F_Main.Taskbar.ProgressState := TTaskBarProgressState.None;
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
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
end;

end.
