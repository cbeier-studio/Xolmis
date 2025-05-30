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
  Classes, SysUtils, Variants, DateUtils, LCLIntf, lazfileutils, FileUtil, jsonconf, fgl,
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
  DebugSettingsFile: String     = 'settings_debug.json';
  LogFile: String               = 'xlmslog.txt';
  GeoBankFile: String           = 'geobank.dat';
  BFKey: String                 = 'support lottery birds sample';
  NullDateStr: String           = '30/12/1500';
  NullTimeStr: String           = '00:00:00';

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
  TLogEventAction = (leaStarting, leaEnd, leaStart, leaFinish, leaOpen, leaClose, leaActiveTab,
    leaExecute, leaCommit, leaRollback);

const
  HistoryActions: array [THistoryAction] of String = ('I', 'U', 'D', 'R');
  LogEventActions: array[TLogEventAction] of String = ('STARTING', 'END', 'START', 'FINISH', 'OPEN', 'CLOSE',
    'ACTIVE TAB', 'EXECUTE', 'COMMIT', 'ROLLBACK');

type

  { TXolmisSettings }

  TXolmisSettings = class
  private
    FConfig: TJSONConfig;
    FFileName: String;
    { General }
    FStartPage: Integer;
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
    FOpenAfterExport: Boolean;
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
    { CSV options }
    FHaveHeader: Boolean;
    FQuotedAsText: Boolean;
    FDelimiterIndex: Integer;
    FDelimiter: Char;
    FDecimalSeparator: Char;
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
    property StartPage: Integer read FStartPage write FStartPage;
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
    property OpenFileAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport;
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
    { CSV options }
    property HaveHeader: Boolean read FHaveHeader write FHaveHeader;
    property QuotedAsText: Boolean read FQuotedAsText write FQuotedAsText;
    property DelimiterIndex: Integer read FDelimiterIndex write FDelimiterIndex;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
    { Versions }
    property ClementsVersion: String read FClementsVersion;
    property IocVersion: String read FIocVersion;
    property CbroVersion: String read FCbroVersion;
  end;

type
  TNotificationPriority = (npRegular, npImportant);
  TNotificationCategory = (ncMessage, ncAlert, ncSystem);

  { TNotification }

  TNotification = class
  private
    FTitle: String;
    FMessage: String;
    FCategory: TNotificationCategory;
    FPriority: TNotificationPriority;
    FIsRead: Boolean;
    FIsVisible: Boolean;
  public
    constructor Create(const ATitle, AMessage: String; ACategory: TNotificationCategory; APriority: TNotificationPriority);
    procedure MarkAsRead;
    procedure Dismiss;

    property Title: String read FTitle;
    property Message: String read FMessage;
    property Category: TNotificationCategory read FCategory;
    property Priority: TNotificationPriority read FPriority;
    property IsRead: Boolean read FIsRead;
    property IsVisible: Boolean read FIsVisible;
  end;

  TNotificationList = specialize TFPGObjectList<TNotification>;


var
  Finalizado: Boolean;
  ConexaoDB: TDBParams;
  XSettings: TXolmisSettings;
  XNotifications: TNotificationList;

var
  Opening, Working, Closing: Boolean;
  Parar: Boolean;
  IsRunning: Boolean;
  FNotificationsNeedUpdate: Boolean;
  MsgValor: String;
  EditSourceStr: String;
  OldPPI: Integer;

  { System logging }
  procedure LogEvent(aAction: TLogEventAction; Msg: String);
  procedure LogDebug(Msg: String);
  procedure LogSQL(aSQL: TStrings);
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
  function NullDate: TDate;
  function NullTime: TTime;
  function NullDateTime: TDateTime;

  { System settings and user permissions manipulation }
  function NewDatabase: Boolean;
  function DatabaseConfig: Boolean;
  procedure LoadDatabaseParams(aConnectionName: String; aConnector: TSQLConnector);
  function ConnectDatabase: Boolean;
  procedure CloseDatabase;

  { Notification system }
  procedure CreateNotificationList;
  procedure DestroyNotificationList;
  procedure DismissNotification(AIndex: Integer);
  procedure NewNotification(const ATitle, AMessage: String; APriority: TNotificationPriority);
  procedure NewAlert(const ATitle, AMessage: String; APriority: TNotificationPriority);
  procedure NewSystemNotification(const ATitle, AMessage: String; APriority: TNotificationPriority);


implementation

uses
  cbs_locale, cbs_users, cbs_data, cbs_conversions, udlg_connect, udlg_newdatabase;

{ ---------------------------------------------------------------------------------------- }
{ System logging }
{ ---------------------------------------------------------------------------------------- }

procedure LogEvent(aAction: TLogEventAction; Msg: String);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Debug(LogEventActions[aAction] + ' | ' + Msg);
end;

procedure LogDebug(Msg: String);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Debug(Msg);
end;

procedure LogSQL(aSQL: TStrings);
begin
  if not XSettings.AllowWriteLogs then
    Exit;

  DMM.evLog.Debug(TrimList(aSQL));
end;

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
const
  KB = 1024;         // 1 KB = 1024 bytes
  MB = 1024 * KB;    // 1 MB = 1024 KB
  GB = 1024 * MB;    // 1 GB = 1024 MB
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
      LogSizeMB := (LogSize / MB);
      if (Round(LogSizeMB)) >= 10 then
      begin
        Result := True;
        LogWarning('Log file reached maximum size of 10 MB');
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

    //{$IFDEF DEBUG}
    //LogSQL(SQL);
    //{$ENDIF}

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

function NullDate: TDate;
begin
  Result := StrToDate(NullDateStr);
end;

function NullTime: TTime;
begin
  Result := StrToTime(NullTimeStr);
end;

function NullDateTime: TDateTime;
begin
  Result := StrToDateTime(NullDateStr + ' ' + NullTimeStr);
end;

{ ---------------------------------------------------------------------------------------- }
{ System settings and user permissions manipulation }
{ ---------------------------------------------------------------------------------------- }

function DatabaseConfig: Boolean;
begin
  Result := False;

  LogEvent(leaOpen, 'Manage connections');
  Application.CreateForm(TcfgDatabase, cfgDatabase);
  with cfgDatabase do
  try
    GravaStat(Name, '', 'open');
    if ShowModal = mrOK then
      Result := True;
  finally
    FreeAndNil(cfgDatabase);
    LogEvent(leaClose, 'Manage connections');
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
  LogDebug('Database params loaded: ' + aConnector.DatabaseName);
end;

function ConnectDatabase: Boolean;
begin
  Result := False;

  dlgConnect := TdlgConnect.Create(nil);
  with dlgConnect do
  try
    if dlgConnect.ShowModal = mrOk then
    begin
      CloseDatabase;
      LoadDatabaseParams(SelectedConnection, DMM.sqlCon);

      try
        if not DMM.sqlCon.Connected then
          DMM.sqlCon.Open;
        LogDebug('Database connected');
      except
        Result := False;
        raise EDatabaseError.Create(rsErrorConnectingDatabase);
      end;

      UpgradeDatabaseSchema(ConexaoDB.Manager);

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

function NewDatabase: Boolean;
begin
  Result := False;

  LogEvent(leaOpen, 'New database');
  Application.CreateForm(TdlgNewDatabase, dlgNewDatabase);
  with dlgNewDatabase do
  try
    GravaStat(Name, '', 'open');
    if ShowModal = mrOK then
      Result := True;
  finally
    FreeAndNil(dlgNewDatabase);
    LogEvent(leaClose, 'New database');
  end;
end;

{ ---------------------------------------------------------------------------------------- }
{ Notification system }
{ ---------------------------------------------------------------------------------------- }

procedure CreateNotificationList;
begin
  FNotificationsNeedUpdate := False;

  XNotifications := TNotificationList.Create;

  // Add notifications to the list
end;

procedure DestroyNotificationList;
begin
  if Assigned(XNotifications) then
    XNotifications.Free;
end;

procedure DismissNotification(AIndex: Integer);
begin
  XNotifications[AIndex].Dismiss;

  FNotificationsNeedUpdate := True;
end;

procedure NewNotification(const ATitle, AMessage: String; APriority: TNotificationPriority);
var
  ANotification: TNotification;
begin
  ANotification := TNotification.Create(ATitle, AMessage, ncMessage, APriority);

  XNotifications.Add(ANotification);

  FNotificationsNeedUpdate := True;
end;

procedure NewAlert(const ATitle, AMessage: String; APriority: TNotificationPriority);
var
  ANotification: TNotification;
begin
  ANotification := TNotification.Create(ATitle, AMessage, ncAlert, APriority);

  XNotifications.Add(ANotification);

  FNotificationsNeedUpdate := True;
end;

procedure NewSystemNotification(const ATitle, AMessage: String; APriority: TNotificationPriority);
var
  ANotification: TNotification;
begin
  ANotification := TNotification.Create(ATitle, AMessage, ncSystem, APriority);

  XNotifications.Add(ANotification);

  FNotificationsNeedUpdate := True;
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
  {$IFDEF DEBUG}
  FFileName := ConcatPaths([AppDataDir, DebugSettingsFile]);
  {$ELSE}
  FFileName := ConcatPaths([AppDataDir, DefaultSettingsFile]);
  {$ENDIF}
  FConfig := TJSONConfig.Create(nil);
  try
    FConfig.Formatted:= True;
    FConfig.Filename:= FFileName;
  except
    Exit;
  end;
end;

destructor TXolmisSettings.Destroy;
begin
  FConfig.Flush;
  FConfig.Free;
  inherited Destroy;
end;

procedure TXolmisSettings.LoadFromFile;
begin
  { General }
  FStartPage := FConfig.GetValue('/GENERAL/StartPage', 12);
  FConfirmCancel := FConfig.GetValue('/GENERAL/ConfirmCancel', False);
  FEnterAsTab := FConfig.GetValue('/GENERAL/EnterAsTab', True);
  FTerminatedOk := FConfig.GetValue('/GENERAL/TerminatedOk', True);
  FClearDeletedPeriod := FConfig.GetValue('/GENERAL/ClearDeletedPeriod', 2);
  FLastClearDeleted := FConfig.GetValue('/GENERAL/LastClearDeleted', StrToDateTime('30/12/1500 00:00:00'));
  FLastDatabaseOptimization := FConfig.GetValue('/GENERAL/LastDatabaseOptimization', StrToDateTime('30/12/1500 00:00:00'));
  FAutoUpdates := FConfig.GetValue('/GENERAL/AutoUpdates', 2);
  FLastAutoUpdate := FConfig.GetValue('/GENERAL/LastAutoUpdate', StrToDateTime('30/12/1500 00:00:00'));
  { Appearance }
  FSelectedTheme := FConfig.GetValue('/APPEARANCE/SelectedTheme', 1);
  FShowGridLines := FConfig.GetValue('/APPEARANCE/ShowGridLines', True);
  FAutoAdjustColumns := FConfig.GetValue('/APPEARANCE/AutoAdjustColumns', False);
  FUseConditionalFormatting := FConfig.GetValue('/APPEARANCE/UseConditionalFormatting', True);
  FShowOutliersOnGrid := FConfig.GetValue('/APPEARANCE/ShowOutliersOnGrid', True);
  FDefaultRowHeight := FConfig.GetValue('/APPEARANCE/DefaultRowHeight', 25);
  FAlternateRowColor := FConfig.GetValue('/APPEARANCE/AlternateRowColor', StringToColor('$00FFFFFF'));
  { Collection }
  FVernacularNamesLanguage := FConfig.GetValue('/COLLECTION/VernacularNamesLanguage', 0);
  FTaxonomy := FConfig.GetValue('/COLLECTION/Taxonomy', 0);
  FShowSynonyms := FConfig.GetValue('/COLLECTION/ShowSynonyms', True);
  { Media }
  FImagesFolder := FConfig.GetValue('/MEDIA/ImagesFolder', ConcatPaths([InstallDir, 'images']));
  FAudiosFolder := FConfig.GetValue('/MEDIA/AudiosFolder', ConcatPaths([InstallDir, 'sounds']));
  FDocumentsFolder := FConfig.GetValue('/MEDIA/DocumentsFolder', ConcatPaths([InstallDir, 'attachments']));
  FOpenAfterExport := FConfig.GetValue('/MEDIA/OpenAfterExport', True);
  { Security }
  FRememberUser := FConfig.GetValue('/SECURITY/RememberUser', False);
  FRememberConnection := FConfig.GetValue('/SECURITY/RememberConnection', True);
  FLastUser := FConfig.GetValue('/SECURITY/LastUser', EmptyStr);
  FLastConnection := FConfig.GetValue('/SECURITY/LastConnection', EmptyStr);
  { Privacy }
  FAllowWriteLogs := FConfig.GetValue('/PRIVACY/AllowWriteLogs', False);
  FAllowUsageData := FConfig.GetValue('/PRIVACY/AllowUsageData', False);
  { Backup }
  FBackupFolder := FConfig.GetValue('/BACKUP/BackupFolder', ConcatPaths([InstallDir, 'backup']));
  FAutomaticBackup := FConfig.GetValue('/BACKUP/StartupBackup', 1);
  FBackupsToKeep := FConfig.GetValue('/BACKUP/BackupsToKeep', 10);
  { CSV options }
  FHaveHeader := FConfig.GetValue('/CSV/HaveHeader', True);
  FQuotedAsText := FConfig.GetValue('/CSV/QuotedAsText', True);
  FDelimiterIndex := FConfig.GetValue('/CSV/DelimiterIndex', 0);
  FDelimiter := FConfig.GetValue('/CSV/Delimiter', ';')[1];
  FDecimalSeparator := FConfig.GetValue('/CSV/DecimalSeparator', ',')[1];
  { Versions }
  FClementsVersion := FConfig.GetValue('/VERSIONS/Clements', '2023');
  FIocVersion := FConfig.GetValue('/VERSIONS/IOC', '14.1');
  FCbroVersion := FConfig.GetValue('/VERSIONS/CBRO', '2021');
end;

procedure TXolmisSettings.SaveToFile;
begin
  { General }
  FConfig.SetValue('/GENERAL/StartPage', FStartPage);
  FConfig.SetValue('/GENERAL/ConfirmCancel', FConfirmCancel);
  FConfig.SetValue('/GENERAL/EnterAsTab', FEnterAsTab);
  FConfig.SetValue('/GENERAL/TerminatedOk', FTerminatedOk);
  FConfig.SetValue('/GENERAL/ClearDeletedPeriod', FClearDeletedPeriod);
  FConfig.SetValue('/GENERAL/LastClearDeleted', FLastClearDeleted);
  FConfig.SetValue('/GENERAL/LastDatabaseOptimization', FLastDatabaseOptimization);
  FConfig.SetValue('/GENERAL/AutoUpdates', FAutoUpdates);
  FConfig.SetValue('/GENERAL/LastAutoUpdate', FLastAutoUpdate);
  { Appearance }
  FConfig.SetValue('/APPEARANCE/SelectedTheme', FSelectedTheme);
  FConfig.SetValue('/APPEARANCE/ShowGridLines', FShowGridLines);
  FConfig.SetValue('/APPEARANCE/AutoAdjustColumns', FAutoAdjustColumns);
  FConfig.SetValue('/APPEARANCE/UseConditionalFormatting', FUseConditionalFormatting);
  FConfig.SetValue('/APPEARANCE/ShowOutliersOnGrid', FShowOutliersOnGrid);
  FConfig.SetValue('/APPEARANCE/DefaultRowHeight', FDefaultRowHeight);
  FConfig.SetValue('/APPEARANCE/AlternateRowColor', ColorToString(FAlternateRowColor));
  { Collection }
  FConfig.SetValue('/COLLECTION/VernacularNamesLanguage', FVernacularNamesLanguage);
  FConfig.SetValue('/COLLECTION/Taxonomy', FTaxonomy);
  FConfig.SetValue('/COLLECTION/ShowSynonyms', FShowSynonyms);
  { Media }
  FConfig.SetValue('/MEDIA/ImagesFolder', FImagesFolder);
  FConfig.SetValue('/MEDIA/AudiosFolder', FAudiosFolder);
  FConfig.SetValue('/MEDIA/DocumentsFolder', FDocumentsFolder);
  FConfig.SetValue('/MEDIA/OpenAfterExport', FOpenAfterExport);
  { Security }
  FConfig.SetValue('/SECURITY/RememberUser', FRememberUser);
  FConfig.SetValue('/SECURITY/RememberConnection', FRememberConnection);
  FConfig.SetValue('/SECURITY/LastUser', FLastUser);
  FConfig.SetValue('/SECURITY/LastConnection', FLastConnection);
  { Privacy }
  FConfig.SetValue('/PRIVACY/AllowWriteLogs', FAllowWriteLogs);
  FConfig.SetValue('/PRIVACY/AllowUsageData', FAllowUsageData);
  { Backup }
  FConfig.SetValue('/BACKUP/BackupFolder', FBackupFolder);
  FConfig.SetValue('/BACKUP/StartupBackup', FAutomaticBackup);
  FConfig.SetValue('/BACKUP/BackupsToKeep', FBackupsToKeep);
  { CSV options }
  FConfig.SetValue('/CSV/HaveHeader', FHaveHeader);
  FConfig.SetValue('/CSV/QuotedAsText', FQuotedAsText);
  FConfig.SetValue('/CSV/DelimiterIndex', FDelimiterIndex);
  FConfig.SetValue('/CSV/Delimiter', FDelimiter);
  FConfig.SetValue('/CSV/DecimalSeparator', FDecimalSeparator);

  FConfig.Flush;
end;

procedure TXolmisSettings.Reset;
begin
  FConfig.Clear;
  FConfig.Flush;

  LoadFromFile;
end;

procedure TXolmisSettings.Delete(aSection, aKey: String);
begin
  FConfig.DeleteValue('/' + aSection + '/' + aKey);
end;

{ TNotification }

constructor TNotification.Create(const ATitle, AMessage: string; ACategory: TNotificationCategory;
    APriority: TNotificationPriority);
begin
  FTitle := ATitle;
  FMessage := AMessage;
  FCategory := ACategory;
  FPriority := APriority;
  FIsRead := False;
  FIsVisible := True;
end;

procedure TNotification.Dismiss;
begin
  FIsRead := True;
  FIsVisible := False;
end;

procedure TNotification.MarkAsRead;
begin
  FIsRead := True;
end;

end.
