{ Xolmis Global library

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

unit cbs_global;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, Variants, DateUtils, LCLIntf, lazfileutils, FileUtil, jsonconf,
  { VCL }
  Forms, Controls, Dialogs, Menus, Buttons, Graphics,
  { Data }
  DB, SQLDB,
  { CBS }
  cbs_system, cbs_datatypes,
  { Forms }
  udm_main, ucfg_database;

const
  NomeApp: String           = 'Xolmis';
  PrereleaseStage: String   = 'Beta';

const
  DefaultSettingsFile: String   = 'settings.json';
  LogFile: String               = 'xlmslog.txt';
  GeoBankFile: String           = 'geobank.dat';
  BFKey: String                 = 'support lottery birds sample';

  { Format masks constants }
const
  maskLongitude: String          = '##00.00000000';
  maskLatitude: String           = '#00.00000000';
  maskLongitudeZeroBlank: String = '##00.00000000;;''''';
  maskLatitudeZeroBlank: String  = '#00.00000000;;''''';
  maskNumInterno: String         = '###,###,###,###,###';
  maskInteiro: String            = '#,##0';
  maskInteiroNegative: String    = '#,##0;##,##0';
  maskOneDecimal: String         = '#,##0.0';
  maskTwoDecimal: String         = '#,##0.00';
  maskShortDate: String          = 'dd/mm/yyyy';
  maskSQLiteDate: String         = 'yyyy-mm-dd';
  maskSQLiteDateTime: String     = 'yyyy-mm-dd hh:nn:ss';
  maskDisplayTime: String        = 'hh:nn';
  maskShortTime: String          = '!90:00;1;_';
  maskCEP: String                = '!00000-000;0;_';
  maskPhone: String              = '!(00)_0000-0000;0;_';
  maskCellPhone: String          = '!(00)_000_000_000;0;_';

  { Hashtags }
const
  AllQS: array of String        = ('#tudo', '#all');
  MarkedQS: array of String     = ('#marcados', '#marked');
  UnmarkedQS: array of String   = ('#naomarcados', '#unmarked');
  FilterQS: array of String     = ('#filtro', '#filter');
  DeletedQS: array of String    = ('#lixo', '#deleted');
  PrintQueueQS: array of String = ('#fila', '#queued', '#toprint');
  OrderQS: array of String      = ('#ordem', '#order', '#ord');
  FamilyQS: array of String     = ('#familia', '#family', '#fam');
  GenusQS: array of String      = ('#genero', '#genus', '#gen');
  SpeciesQS: array of String    = ('#especie', '#species', '#sp');
  SiteQS: array of String       = ('#local', '#site');
  QualifierQS: array of String  = ('#quali', '#qualifier');
  ParentQS: array of String     = ('#superior', '#parent');
  RankQS: array of String       = ('#nivel', '#categoria', '#rank');
  ListsQS: array of String      = ('#listas', '#lists');
  SqlQS: array of String        = ('#sql', '#sqlfilter');

type
  THistoryAction = (haCreated, haEdited, haDeleted, haRestored);

const
  HistoryActions: array [THistoryAction] of String = ('I', 'U', 'D', 'R');

type

  { TXolmisSettings }

  TXolmisSettings = class
  private
    Ini: TJSONConfig;
    FFileName: String;
    { General }
    FConfirmCancel, FEnterAsTab, FTerminatedOk: Boolean;
    FClearDeletedPeriod: Integer;
    FLastClearDeleted, FLastDatabaseOptimization, FLastAutoUpdate: TDateTime;
    FLastPathUsed: String;
    { Appearance }
    FSelectedTheme: Integer;
    FShowGridLines, FAutoAdjustColumns: Boolean;
    FUseConditionalFormatting: Boolean;
    FShowOutliersOnGrid: Boolean;
    FDefaultRowHeight: Integer;
    FAlternateRowColor: TColor;
    { Collection }
    FVernacularNamesLanguage, FTaxonomy: Integer;
    FShowSynonyms: Boolean;
    { Media }
    FImagesFolder, FAudiosFolder, FDocumentsFolder: String;
    { Security }
    FRememberUser, FRememberConnection: Boolean;
    FLastUser, FLastConnection: String;
    FAutoUpdates: Integer;
    { Privacy }
    FAllowWriteLogs, FAllowUsageData: Boolean;
    { Backup }
    FBackupFolder: String;
    FAutomaticBackup: Integer;
    FBackupsToKeep: Integer;
    { Versions }
    FClementsVersion, FIocVersion, FCbroVersion: String;
    procedure SetFileName(aValue: String);
    procedure SetImagesFolder(aValue: String);
    procedure SetAudiosFolder(aValue: String);
    procedure SetDocumentsFolder(aValue: String);
    procedure SetBackupFolder(aValue: String);
    procedure SetLastPathUsed(aValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile;
    procedure SaveToFile;
    procedure Reset;
    procedure Delete(aSection, aKey: String);
  published
    property SettingsFile: String read FFileName write SetFileName;
    { General }
    property ConfirmCancel: Boolean read FConfirmCancel write FConfirmCancel;
    property UseEnterAsTab: Boolean read FEnterAsTab write FEnterAsTab;
    property AppTerminatedOk: Boolean read FTerminatedOk write FTerminatedOk;
    property ClearDeletedPeriod: Integer read FClearDeletedPeriod write FClearDeletedPeriod;
    property LastClearDeleted: TDateTime read FLastClearDeleted write FLastClearDeleted;
    property LastDatabaseOptimization: TDateTime read FLastDatabaseOptimization write FLastDatabaseOptimization;
    property LastAutoUpdate: TDateTime read FLastAutoUpdate write FLastAutoUpdate;
    property LastPathUsed: String read FLastPathUsed write SetLastPathUsed;
    { Appearance }
    property SelectedTheme: Integer read FSelectedTheme write FSelectedTheme;
    property ShowGridLines: Boolean read FShowGridLines write FShowGridLines;
    property AutoAdjustColumns: Boolean read FAutoAdjustColumns write FAutoAdjustColumns;
    property UseConditionalFormatting: Boolean read FUseConditionalFormatting write FUseConditionalFormatting;
    property ShowOutliersOnGrid: Boolean read FShowOutliersOnGrid write FShowOutliersOnGrid;
    property DefaultRowHeight: Integer read FDefaultRowHeight write FDefaultRowHeight;
    property AlternateRowColor: TColor read FAlternateRowColor write FAlternateRowColor;
    { Collection }
    property VernacularNamesLanguage: Integer read FVernacularNamesLanguage write FVernacularNamesLanguage;
    property Taxonomy: Integer read FTaxonomy write FTaxonomy;
    property ShowSynonyms: Boolean read FShowSynonyms write FShowSynonyms;
    { Media }
    property ImagesFolder: String read FImagesFolder write SetImagesFolder;
    property AudiosFolder: String read FAudiosFolder write SetAudiosFolder;
    property DocumentsFolder: String read FDocumentsFolder write SetDocumentsFolder;
    { Security }
    property RememberUser: Boolean read FRememberUser write FRememberUser;
    property LastUser: String read FLastUser write FLastUser;
    property RememberConnection: Boolean read FRememberConnection write FRememberConnection;
    property LastConnection: String read FLastConnection write FLastConnection;
    property AutoUpdates: Integer read FAutoUpdates write FAutoUpdates;
    { Privacy }
    property AllowWriteLogs: Boolean read FAllowWriteLogs write FAllowWriteLogs;
    property AllowSendUsageData: Boolean read FAllowUsageData write FAllowUsageData;
    { Backup }
    property BackupFolder: String read FBackupFolder write SetBackupFolder;
    property AutomaticBackup: Integer read FAutomaticBackup write FAutomaticBackup;
    property BackupsToKeep: Integer read FBackupsToKeep write FBackupsToKeep;
    { Versions }
    property ClementsVersion: String read FClementsVersion;
    property IocVersion: String read FIocVersion;
    property CbroVersion: String read FCbroVersion;
  end;


var
  Finalizado: Boolean;
  ConexaoDB: TDBParams;
  XSettings: TXolmisSettings;

var
  Opening, Working, Closing: Boolean;
  Parar: Boolean;
  IsRunning: Boolean;
  MsgValor: String;
  EditSourceStr: String;
  OldPPI: Integer;

  { System logging }
  procedure LogEvent(aAction, Msg: String);
  {$IFDEF DEBUG}
  procedure LogDebug(Msg: String);
  procedure LogSQL(aSQL: TStrings);
  {$ENDIF}
  procedure LogError(Msg: String);
  procedure LogWarning(Msg: String);
  procedure LogInfo(Msg: String);
  function CheckLogsFull: Boolean;
  procedure WriteRecHistory(aTable: TTableType; aAction: THistoryAction; aCodigo: Integer = 0;
    aField: String = ''; aOldValue: String = ''; aNewValue: String = ''; aNote: String = '');
  procedure GravaStat(const aModule, aControl, aEvent: String);

  { System variables }
  function InstallDir: String;
  function AppDataDir: String;
  function TempDir: String;
  function HelpDir: String;

  { System settings and user permissions manipulation }
  function FirstConfig: Boolean;
  function DatabaseConfig: Boolean;
  procedure LoadDatabaseParams(aConnectionName: String; aConnector: TSQLConnector);
  function ConnectDatabase: Boolean;
  procedure CloseDatabase;


implementation

uses
  cbs_locale, cbs_data, cbs_conversions, udlg_connect, udlg_firstconfig;

{ ---------------------------------------------------------------------------------------- }
{ System logging }
{ ---------------------------------------------------------------------------------------- }

procedure LogEvent(aAction, Msg: String);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Debug(aAction + '|' + Msg);
end;

{$IFDEF DEBUG}
procedure LogDebug(Msg: String);
begin
  DMM.evLog.Debug(Msg);
end;

procedure LogSQL(aSQL: TStrings);
begin
  DMM.evLog.Debug(TrimList(aSQL));
end;
{$ENDIF}

procedure LogError(Msg: String);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Error(Msg);
end;

procedure LogWarning(Msg: String);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Warning(Msg);
end;

procedure LogInfo(Msg: String);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Info(Msg);
end;

function CheckLogsFull: Boolean;
var
  Logs: TStringList;
  oldLog, currLog: String;
  LogSize: Int64;
  LogSizeMB: Double;
begin
  Result := False;

  LogSizeMB := 0.0;
  oldLog := EmptyStr;
  currLog := ConcatPaths([AppDataDir, LogFile]);
  if FileExists(currLog) then
  begin
    Logs := TStringList.Create;
    try
      { If the log file is bigger than 10 MB }
      LogSize := FileSize(currLog);
      LogSizeMB := (LogSize / 1000000);
      if (Round(LogSizeMB)) >= 10 then
      begin
        Result := True;
        Logs.Assign(FindAllFiles(AppDataDir, 'xlmslog-old*.txt'));
        oldLog := ConcatPaths([AppDataDir, Format('xlmslog-old-%d.txt', [Logs.Count + 1])]);
        RenameFile(currLog, oldLog);
      end;
    finally
      FreeAndNil(Logs);
    end;
  end;
end;

procedure WriteRecHistory(aTable: TTableType; aAction: THistoryAction; aCodigo: Integer = 0;
    aField: String = ''; aOldValue: String = ''; aNewValue: String = ''; aNote: String = '');
var
  Cod: Integer;
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    { Get the autoincrement value of the last inserted record }
    if (aAction = haCreated) and (aTable <> tbNone) and (aCodigo = 0) then
    begin
      Cod := GetLastInsertedKey(aTable);
    end
    else
      Cod := aCodigo;

    { Add record history entry }
    Clear;
    Add('INSERT INTO record_history (event_date, user_id,');
    Add('event_action, event_table, event_field, record_id, old_value, new_value, notes)');
    Add('VALUES (datetime(''now'',''localtime''), :auser, :oper, :tabname, :afield, :keyv, ' +
      ':oldvalue, :newvalue, :note)');
    ParamByName('AUSER').AsInteger := ActiveUser.Id;
    ParamByName('OPER').AsString := HistoryActions[aAction];
    ParamByName('TABNAME').AsString := TableNames[aTable];
    ParamByName('AFIELD').AsString := aField;
    ParamByName('KEYV').AsInteger := Cod;
    ParamByName('OLDVALUE').AsString := aOldValue;
    ParamByName('NEWVALUE').AsString := aNewValue;
    ParamByName('NOTE').AsString := aNote;

    {$IFDEF DEBUG}
    LogSQL(SQL);
    {$ENDIF}

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ Log usage stats }
procedure GravaStat(const aModule, aControl, aEvent: String);
var
  aStat: TUsageStat;
begin
  if XSettings.AllowSendUsageData then
  begin
    aStat.Module := aModule;
    aStat.Control := aControl;
    aStat.Event := aEvent;
    aStat.AddCount;
  end;
end;

{ ---------------------------------------------------------------------------------------- }
{ System variables }
{ ---------------------------------------------------------------------------------------- }

// Get the application/installation path
function InstallDir: String;
var
  s: String;
begin
  s := ExtractFilePath(Application.ExeName);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
end;

// Get the Xolmis' AppData path
function AppDataDir: String;
var
  s: String;
begin
  s := GetAppConfigDir(False);
  s := IncludeTrailingPathDelimiter(s);

  Result:= s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

// Get the Xolmis' Temp path
function TempDir: String;
var
  s: String;
begin
  s := ConcatPaths([GetTempDir(False), NomeApp]);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function HelpDir: String;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([InstallDir, 'help']));
end;

{ ---------------------------------------------------------------------------------------- }
{ System settings and user permissions manipulation }
{ ---------------------------------------------------------------------------------------- }

function DatabaseConfig: Boolean;
begin
  Result := False;

  Application.CreateForm(TcfgDatabase, cfgDatabase);
  with cfgDatabase do
  try
    {$IFDEF DEBUG}
    LogDebug('OPEN: Manage connections');
    {$ENDIF}
    GravaStat(Name, '', 'open');
    if ShowModal = mrOK then
      Result := True;
  finally
    {$IFDEF DEBUG}
    LogDebug('CLOSE: Manage connections');
    {$ENDIF}
    FreeAndNil(cfgDatabase);
  end;
end;

procedure LoadDatabaseParams(aConnectionName: String; aConnector: TSQLConnector);
const
  { SQLite constants }
  sqliteOpenModes: array of String = ('CreateUTF8', 'ReadWrite', 'ReadOnly');
  sqliteGUIDFormats: array of String = ('String', 'Binary');
  sqliteStringFormats: array of String = ('Unicode', 'ANSI', 'Choose');
  { Firebird constants }
  fbOpenModes: array of String = ('OpenOrCreate', 'Create', 'Open');
  fbProtocols: array of String = ('Local', 'TCPIP', 'NetBEUI', 'SPX');
  fbGUIDEndians: array of String = ('Little', 'Big');
begin
  ConexaoDB.Clear;

  ConexaoDB.Name := aConnectionName;
  ConexaoDB.LoadParams;

  case ConexaoDB.Manager of
    dbSqlite:
      begin
        //aConnector.ConnectionDefName := ConexaoDB.Name;
        aConnector.Params.Clear;
        aConnector.ConnectorType := 'SQLite3';
        aConnector.DatabaseName := ConexaoDB.Database;
        aConnector.Params.Add('StringFormat=' + sqliteStringFormats[Ord(ConexaoDB.StringFormat)]);
        aConnector.Params.Add('OpenMode=' + sqliteOpenModes[Ord(ConexaoDB.OpenMode)]);
        aConnector.Params.Add('GUIDFormat=' + sqliteGUIDFormats[Ord(ConexaoDB.GUIDFormat)]);
        {$IFDEF DEBUG}
        aConnector.Params.Add('LockingMode=Normal');
        aConnector.Params.Add('Synchronous=Full');
        {$ELSE}
        aConnector.Params.Add('LockingMode=Exclusive');
        aConnector.Params.Add('Synchronous=Off');
        {$ENDIF}
      end;
    dbFirebird:
      begin
        //aConnector.ConnectionDefName := ConexaoDB.Name;
        aConnector.Params.Clear;
        aConnector.ConnectorType := 'Firebird';
        aConnector.DatabaseName := ConexaoDB.Database;
        aConnector.HostName := ConexaoDB.Server;
        aConnector.UserName := ConexaoDB.UserName;
        aConnector.Password := ConexaoDB.Password;
        aConnector.Params.Add('Protocol=' + fbProtocols[Ord(ConexaoDB.Protocol)]);
        aConnector.Params.Add('Port=' + IntToStr(ConexaoDB.Port));
        aConnector.Params.Add('CharacterSet=' + ConexaoDB.CharacterSet);
        aConnector.Params.Add('OpenMode=' + fbOpenModes[Ord(ConexaoDB.OpenMode)]);
        aConnector.Params.Add('PageSize=' + IntToStr(ConexaoDB.PageSize));
      end;
  end;
  //aConnector.CheckConnectionDef;
  {$IFDEF DEBUG}
  LogDebug('Database params loaded: ' + aConnector.DatabaseName);
  {$ENDIF}
end;

function ConnectDatabase: Boolean;
var
  dontConnect: Boolean;
//  DateCheck: TDateTime;
begin
  Result := False;
  dontConnect := False;

  dlgConnect := TdlgConnect.Create(nil);
  with dlgConnect do
  try
    if dlgConnect.ShowModal = mrOk then
    begin
      CloseDatabase;
      LoadDatabaseParams(SelectedConnection, DMM.sqlCon);

      // Create database if not exists
      case ConexaoDB.Manager of
        dbSqlite:
          begin
            if not (FileExists(ConexaoDB.Database)) then
              if MessageDlg(rsCreateDatabasePrompt, mtConfirmation, mbYesNo, 0, mbYes) = mrYes then
                CreateUserDatabase(ConexaoDB.Manager, ConexaoDB.Database)
              else
                dontConnect := True;

            // If the last session was not terminated correctly
            //if (Finalizado = False) then
            //  CheckDB;

            // Create a backup for quick restore
            //if (GetPreference('BACKUP', 'BootBackup', True) = True) then
            //  RunStartupBackup;

            // >> Optimize and reindex database
            //DateCheck := GetPreference('DATABASE', 'LastBDCheck', DateOf(Now - 7));
            //if (Finalizado = True) and (Now - DateCheck > 7) then
            //  OptimizeDB;
          end;
        dbFirebird:
          begin

          end;
        dbPostgre: ;
        dbMaria: ;
      end;

      if dontConnect then
        Exit;

      // >> Permanently delete inactive records older than a specified period
      //DateCheck := GetPreference('GENERAL', 'LastClearDeleted', DateOf(Now - 1));
      //if (GetPreference('GENERAL', 'ClearDeletedPeriod', 2) > 0) and (Now - DateCheck > 0) then
      //  ClearDeleted(GetPreference('GENERAL', 'ClearDeletedPeriod', 2) * 30);

      try
        if not DMM.sqlCon.Connected then
          DMM.sqlCon.Open;
        {$IFDEF DEBUG}
        LogDebug('Database connected');
        {$ENDIF}
      except
        Result := False;
        raise EDatabaseError.Create(rsErrorConnectingDatabase);
      end;

      Result := True;
    end;
  finally
    FreeAndNil(dlgConnect);
  end;
end;

procedure CloseDatabase;
begin
  if DMM.sqlCon.Connected then
    DMM.sqlCon.Close;

  ConexaoDB.Clear;
end;

function FirstConfig: Boolean;
begin
  Result := False;

  Application.CreateForm(TdlgFirstConfig, dlgFirstConfig);
  with dlgFirstConfig do
  try
    {$IFDEF DEBUG}
    LogDebug('OPEN: Manage connections');
    {$ENDIF}
    GravaStat(Name, '', 'open');
    if ShowModal = mrOK then
      Result := True;
  finally
    {$IFDEF DEBUG}
    LogDebug('CLOSE: Manage connections');
    {$ENDIF}
    FreeAndNil(dlgFirstConfig);
  end;
end;

{ TXolmisSettings }

procedure TXolmisSettings.SetFileName(aValue: String);
begin
  if FFileName <> EmptyStr then
    SaveToFile;
  FFileName := aValue;
end;

procedure TXolmisSettings.SetImagesFolder(aValue: String);
begin
  FImagesFolder := IncludeTrailingPathDelimiter(aValue);
end;

procedure TXolmisSettings.SetLastPathUsed(aValue: String);
begin
  FLastPathUsed := ExtractFileRoot(aValue);
end;

procedure TXolmisSettings.SetAudiosFolder(aValue: String);
begin
  FAudiosFolder := IncludeTrailingPathDelimiter(aValue);
end;

procedure TXolmisSettings.SetDocumentsFolder(aValue: String);
begin
  FDocumentsFolder := IncludeTrailingPathDelimiter(aValue);
end;

procedure TXolmisSettings.SetBackupFolder(aValue: String);
begin
  FBackupFolder := IncludeTrailingPathDelimiter(aValue);
end;

constructor TXolmisSettings.Create;
begin
  inherited;
  FFileName := ConcatPaths([AppDataDir, DefaultSettingsFile]);
  Ini := TJSONConfig.Create(nil);
  try
    Ini.Formatted:= True;
    Ini.Filename:= FFileName;
  except
    Exit;
  end;
end;

destructor TXolmisSettings.Destroy;
begin
  Ini.Flush;
  Ini.Free;
  inherited Destroy;
end;

procedure TXolmisSettings.LoadFromFile;
begin
  { General }
  FConfirmCancel := Ini.GetValue('/GENERAL/ConfirmCancel', False);
  FEnterAsTab := Ini.GetValue('/GENERAL/EnterAsTab', True);
  FTerminatedOk := Ini.GetValue('/GENERAL/TerminatedOk', True);
  FClearDeletedPeriod := Ini.GetValue('/GENERAL/ClearDeletedPeriod', 2);
  FLastClearDeleted := Ini.GetValue('/GENERAL/LastClearDeleted', StrToDateTime('30/12/1500 00:00:00'));
  FLastDatabaseOptimization := Ini.GetValue('/GENERAL/LastDatabaseOptimization', StrToDateTime('30/12/1500 00:00:00'));
  FLastAutoUpdate := Ini.GetValue('/GENERAL/LastAutoUpdate', StrToDateTime('30/12/1500 00:00:00'));
  { Appearance }
  FSelectedTheme := Ini.GetValue('/APPEARANCE/SelectedTheme', 1);
  FShowGridLines := Ini.GetValue('/APPEARANCE/ShowGridLines', True);
  FAutoAdjustColumns := Ini.GetValue('/APPEARANCE/AutoAdjustColumns', False);
  FUseConditionalFormatting := Ini.GetValue('/APPEARANCE/UseConditionalFormatting', True);
  FShowOutliersOnGrid := Ini.GetValue('/APPEARANCE/ShowOutliersOnGrid', True);
  FDefaultRowHeight := Ini.GetValue('/APPEARANCE/DefaultRowHeight', 25);
  FAlternateRowColor := Ini.GetValue('/APPEARANCE/AlternateRowColor', StringToColor('$00FFFFFF'));
  { Collection }
  FVernacularNamesLanguage := Ini.GetValue('/COLLECTION/VernacularNamesLanguage', 0);
  FTaxonomy := Ini.GetValue('/COLLECTION/Taxonomy', 0);
  FShowSynonyms := Ini.GetValue('/COLLECTION/ShowSynonyms', True);
  { Media }
  FImagesFolder := Ini.GetValue('/MEDIA/ImagesFolder', ConcatPaths([InstallDir, 'images']));
  FAudiosFolder := Ini.GetValue('/MEDIA/AudiosFolder', ConcatPaths([InstallDir, 'sounds']));
  FDocumentsFolder := Ini.GetValue('/MEDIA/DocumentsFolder', ConcatPaths([InstallDir, 'attachments']));
  { Security }
  FRememberUser := Ini.GetValue('/SECURITY/RememberUser', False);
  FRememberConnection := Ini.GetValue('/SECURITY/RememberConnection', True);
  FLastUser := Ini.GetValue('/SECURITY/LastUser', EmptyStr);
  FLastConnection := Ini.GetValue('/SECURITY/LastConnection', EmptyStr);
  FAutoUpdates := Ini.GetValue('/SECURITY/AutoUpdates', 2);
  { Privacy }
  FAllowWriteLogs := Ini.GetValue('/PRIVACY/AllowWriteLogs', False);
  FAllowUsageData := Ini.GetValue('/PRIVACY/AllowUsageData', False);
  { Backup }
  FBackupFolder := Ini.GetValue('/BACKUP/BackupFolder', ConcatPaths([InstallDir, 'backup']));
  FAutomaticBackup := Ini.GetValue('/BACKUP/StartupBackup', 1);
  FBackupsToKeep := Ini.GetValue('/BACKUP/BackupsToKeep', 10);
  { Versions }
  FClementsVersion := Ini.GetValue('/VERSIONS/Clements', '2023');
  FIocVersion := Ini.GetValue('/VERSIONS/IOC', '14.1');
  FCbroVersion := Ini.GetValue('/VERSIONS/CBRO', '2021');
end;

procedure TXolmisSettings.SaveToFile;
begin
  { General }
  Ini.SetValue('/GENERAL/ConfirmCancel', FConfirmCancel);
  Ini.SetValue('/GENERAL/EnterAsTab', FEnterAsTab);
  Ini.SetValue('/GENERAL/TerminatedOk', FTerminatedOk);
  Ini.SetValue('/GENERAL/ClearDeletedPeriod', FClearDeletedPeriod);
  Ini.SetValue('/GENERAL/LastClearDeleted', FLastClearDeleted);
  Ini.SetValue('/GENERAL/LastDatabaseOptimization', FLastDatabaseOptimization);
  Ini.SetValue('/GENERAL/AutoUpdates', FAutoUpdates);
  Ini.SetValue('/GENERAL/LastAutoUpdate', FLastAutoUpdate);
  { Appearance }
  Ini.SetValue('/APPEARANCE/SelectedTheme', FSelectedTheme);
  Ini.SetValue('/APPEARANCE/ShowGridLines', FShowGridLines);
  Ini.SetValue('/APPEARANCE/AutoAdjustColumns', FAutoAdjustColumns);
  Ini.SetValue('/APPEARANCE/UseConditionalFormatting', FUseConditionalFormatting);
  Ini.SetValue('/APPEARANCE/ShowOutliersOnGrid', FShowOutliersOnGrid);
  Ini.SetValue('/APPEARANCE/DefaultRowHeight', FDefaultRowHeight);
  Ini.SetValue('/APPEARANCE/AlternateRowColor', ColorToString(FAlternateRowColor));
  { Collection }
  Ini.SetValue('/COLLECTION/VernacularNamesLanguage', FVernacularNamesLanguage);
  Ini.SetValue('/COLLECTION/Taxonomy', FTaxonomy);
  Ini.SetValue('/COLLECTION/ShowSynonyms', FShowSynonyms);
  { Media }
  Ini.SetValue('/MEDIA/ImagesFolder', FImagesFolder);
  Ini.SetValue('/MEDIA/AudiosFolder', FAudiosFolder);
  Ini.SetValue('/MEDIA/DocumentsFolder', FDocumentsFolder);
  { Security }
  Ini.SetValue('/SECURITY/RememberUser', FRememberUser);
  Ini.SetValue('/SECURITY/RememberConnection', FRememberConnection);
  Ini.SetValue('/SECURITY/LastUser', FLastUser);
  Ini.SetValue('/SECURITY/LastConnection', FLastConnection);
  { Privacy }
  Ini.SetValue('/PRIVACY/AllowWriteLogs', FAllowWriteLogs);
  Ini.SetValue('/PRIVACY/AllowUsageData', FAllowUsageData);
  { Backup }
  Ini.SetValue('/BACKUP/BackupFolder', FBackupFolder);
  Ini.SetValue('/BACKUP/StartupBackup', FAutomaticBackup);
  Ini.SetValue('/BACKUP/BackupsToKeep', FBackupsToKeep);

  Ini.Flush;
end;

procedure TXolmisSettings.Reset;
begin
  Ini.Clear;
  Ini.Flush;

  LoadFromFile;
end;

procedure TXolmisSettings.Delete(aSection, aKey: String);
begin
  Ini.DeleteValue('/' + aSection + '/' + aKey);
end;

end.
