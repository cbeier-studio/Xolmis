{*******************************************************}
{                                                       }
{       Xolmis                                          }
{                                                       }
{       Copyright (C) 2023 Christian Beier              }
{                                                       }
{       Data library                                    }
{                                                       }
{*******************************************************}

unit cbs_data;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, Variants, DateUtils, StrUtils, Generics.Collections, RegExpr, fileutil,
  { VCL }
  Controls, ExtCtrls, Forms, Dialogs, StdCtrls, DBCtrls,
  { Data }
  DB, SQLDB,
  { CBS }
  cbs_datatypes, cbs_taxonomy;

  { System database creation }
  function CreateSystemDatabase(aFilename: String): Boolean;
  procedure CreateConnectionsTable;
  procedure CreateTablesMappingTable;
  procedure CreateFieldsMappingTable;
  procedure CreateUsageDataTable;

  { User database creation }
  function CreateUserDatabase(aProtocol: TDBManager; aFilename: String): Boolean;

  { Database information and management }
  function GetTableType(aTableName: String): TTableType;
  function GetFieldDisplayName(const aTableType: TTableType; aFieldName: String): String;
  function GetPrimaryKey(const aTableName: String): String;
  function IncNumInterno(aUser: Integer): Integer;
  procedure GravaNumInterno(aUser: Integer; aNum: Integer);
  function TableIsEmpty(aTableName: String): Boolean;
  function TableExists(aTableName: String): Boolean;
  function GetLastInsertedKey(const aTable: String): Integer; overload;
  function GetLastInsertedKey(aTableType: TTableType): Integer; overload;
  function GetLastGeneratedKey(GeneratorName: String; AutoIncrement: Boolean = False): Integer;
  procedure DeleteAll(const aTableType: TTableType);
  procedure ClearDeleted(OlderThan: Integer);
  procedure OptimizeDB;
  function CheckDB: Boolean;
  procedure OpenLookupDataSets(aDataSet: TDataSet);

  { Record manipulations }
  function CanEdit(aDataset: TDataset): Boolean;
  procedure DeleteRecord(aTable: TTableType; aDataSet: TDataSet);
  procedure RestoreRecord(aTable: TTableType; aDataSet: TDataSet);
  procedure MarkRecord(aTableName, aFieldName: String; aKeyValue: Integer);
  procedure UnmarkRecord(aTableName, aFieldName: String; aKeyValue: Integer);
  procedure MarkAllRecords(const aTable: TTableType; IsChecked: Boolean; aKeyField: String;
    aModifier: TRecordStatus; aWhere: TStrings); overload;
  procedure MarkAllRecords(const aTable: TTableType; aKeyField: String; aModifier: TRecordStatus;
    aWhere: TStrings; ackMarcados: TCheckBox); overload;
  procedure QueueRecord(aTableName, aFieldName: String; aKeyValue: Integer);
  procedure UnqueueRecord(aTableName, aFieldName: String; aKeyValue: Integer);
  procedure UpdateBand(aBand, aIndividual: Integer; aStatus: String; aDate: TDate);
  procedure UpdateIndividual(aIndividual: Integer; aDate: TDate);
  procedure ChangeIndividualBand(aIndividual: Integer; aNewBand, aRemovedBand: Integer; aDate: TDate;
    aName: String);
  procedure SetRecordDateUser(aDataSet: TDataSet);
  procedure CancelRecord(aDataSet: TDataSet; aFocusControl: TWinControl);

implementation

uses
  cbs_locale, cbs_global, cbs_gis, cbs_dialogs, cbs_conversions, cbs_system, cbs_blobs,
  cbs_permissions, cbs_count, udm_main;

  {
  -----------------------------------------------------------------------------------------
  System database creation
  -----------------------------------------------------------------------------------------
  }

function CreateSystemDatabase(aFilename: String): Boolean;
var
  newFile : Boolean;
begin
  Result := False;
  if DMM.sysCon.Connected then
    DMM.sysCon.Close;

  try
    { Check whether the file already exists }
    newFile := not FileExists(aFilename);

    if newFile then
    begin
      { Create the database and the tables }
      try
        DMM.sysCon.DatabaseName := aFilename;
        //DMM.sysCon.Open;
        if not DMM.sysTrans.Active then
          DMM.sysTrans.StartTransaction;

        CreateConnectionsTable;
        CreateUsageDataTable;
        CreateTablesMappingTable;
        CreateFieldsMappingTable;

        { Populate tables }
        DMM.scriptTablesMap.ExecuteScript;
        DMM.scriptFieldsMap.ExecuteScript;

        DMM.sysTrans.CommitRetaining;
        LogInfo('System database succesfully created');
        Result := True;
      except
        DMM.sysTrans.RollbackRetaining;
        LogError('Unable to create the system database');
      end;
    end;
  except
    LogError('Unable to check if system database file exists');
  end;
end;

procedure CreateConnectionsTable;
begin
  { Create table "connections" }
  DMM.sysCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS connections ( ' +
              'connection_id    INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL, ' +
              'connection_name  VARCHAR (40)  NOT NULL, ' +
              'database_type    INTEGER       NOT NULL, ' +
              'database_server  VARCHAR (30), ' +
              'database_port    INTEGER, ' +
              'database_name    VARCHAR (200) NOT NULL, ' +
              'user_name        VARCHAR (20), ' +
              'user_password    TEXT, ' +
              'insert_date      DATETIME, ' +
              'update_date      DATETIME );');

  { Create index based upon "connection_name" in the "connections" table }
  DMM.sysCon.ExecuteDirect('CREATE UNIQUE INDEX idx_connection_name ON connections ( ' +
              'connection_name COLLATE NOCASE );');
end;

procedure CreateTablesMappingTable;
begin
  { Create table }
  DMM.sysCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS tables_mapping ( ' +
              'table_name     VARCHAR (60), ' +
              'display_name   VARCHAR (60), ' +
              'visible_status BOOLEAN      DEFAULT (1), ' +
              'export_show    BOOLEAN      DEFAULT (1), ' +
              'import_show    BOOLEAN      DEFAULT (1), ' +
              'filter_show    BOOLEAN      DEFAULT (1), ' +
              'insert_date    DATETIME, ' +
              'update_date    DATETIME );');

  { Create indexes }
  DMM.sysCon.ExecuteDirect('CREATE UNIQUE INDEX idx_table_name ON tables_mapping ( ' +
              'table_name COLLATE NOCASE );');
  DMM.sysCon.ExecuteDirect('CREATE INDEX idx_table_display_name ON tables_mapping ( ' +
              'display_name COLLATE NOCASE );');
end;

procedure CreateFieldsMappingTable;
begin
  { Create table }
  DMM.sysCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS fields_mapping ( ' +
              'table_name       VARCHAR (60), ' +
              'field_name       VARCHAR (60), ' +
              'display_name     VARCHAR (60), ' +
              'integer_key      BOOLEAN       DEFAULT (0), ' +
              'text_key         BOOLEAN       DEFAULT (0), ' +
              'darwin_core_name VARCHAR (100), ' +
              'sort_num         INTEGER, ' +
              'field_type       VARCHAR (30), ' +
              'filter_type      VARCHAR (20), ' +
              'lookup_table     VARCHAR (60), ' +
              'lookup_key       VARCHAR (60), ' +
              'lookup_result    VARCHAR (60), ' +
              'lookup_name      VARCHAR (60), ' +
              'minimum_value    REAL, ' +
              'maximum_value    REAL, ' +
              'value_list       TEXT, ' +
              'sorted_status    BOOLEAN       DEFAULT (1), ' +
              'visible_status   BOOLEAN       DEFAULT (1), ' +
              'insert_date      DATETIME, ' +
              'update_date      DATETIME );');

  { Create index }
  DMM.sysCon.ExecuteDirect('CREATE INDEX idx_table_field ON fields_mapping ( ' +
              'table_name COLLATE NOCASE,' +
              'field_name COLLATE NOCASE );');
end;

procedure CreateUsageDataTable;
begin
  { Create table "usage_data" }
  DMM.sysCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS usage_data ( ' +
              'usage_id   INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL, ' +
              'module     VARCHAR (40)  NOT NULL, ' +
              'control    VARCHAR (60), ' +
              'event      VARCHAR (60), ' +
              'run_tally  INTEGER );');

  { Create index based upon "window" in the "usage_data" table }
  DMM.sysCon.ExecuteDirect('CREATE INDEX idx_module_control_event ON usage_data ( ' +
              'module COLLATE NOCASE,' +
              'control COLLATE NOCASE,' +
              'event COLLATE NOCASE );');
end;

  {
  -----------------------------------------------------------------------------------------
  User database creation
  -----------------------------------------------------------------------------------------
  }

function CreateUserDatabase(aProtocol: TDBManager; aFilename: String): Boolean;
begin
  Result := False;
  if DMM.sqlCon.Connected then
    DMM.sqlCon.Close;

  case aProtocol of
    dbSqlite:
    begin
      { Create the database and the tables }
      {$IFDEF DEBUG}
      LogDebug('Creating database: ' + aFilename);
      {$ENDIF}
      try
        Result := CopyFile(ConcatPaths([AppDataDir, 'XolmisDB_template.sqlite3']), aFilename, False, True);

        if Result then
          DMM.sqlCon.DatabaseName := aFilename;

        //DMM.sqlCon.ConnectorType := 'SQLite3';
        //DMM.sqlCon.Open;
        //if not DMM.sqlTrans.Active then
        //  DMM.sqlTrans.StartTransaction;

        { Create tables and stuff }
        //DMM.sqlCon.CreateDB;
        //try
        //  DMM.scriptNewUserDB.ExecuteScript;
        //finally
        //  if DMM.scriptNewUserDB.Aborted then
        //    DMM.sqlTrans.RollbackRetaining
        //  else
        //    DMM.sqlTrans.CommitRetaining;
        //end;

        //if not DMM.sqlTrans.Active then
        //  DMM.sqlTrans.StartTransaction;
        { Populate tables }
        //try
        //  DMM.scriptUserDBInit.ExecuteScript;
        //finally
        //  if DMM.scriptUserDBInit.Aborted then
        //    DMM.sqlTrans.RollbackRetaining
        //  else
        //    DMM.sqlTrans.CommitRetaining;
        //end;

        LogInfo(Format('User database succesfully created (SQLite): %s', [aFileName]));
        Result := True;
      except
        //DMM.sqlTrans.RollbackRetaining;
        LogError(Format('Unable to create the user database (SQLite): %s', [aFileName]));
        Result := False;
      end;
    end;
    dbFirebird: ;
    dbPostgre: ;
    dbMaria: ;
  end;
end;

  {
  -----------------------------------------------------------------------------------------
  Database information and management
  -----------------------------------------------------------------------------------------
  }

function GetTableType(aTableName: String): TTableType;
var
  i, iR: Integer;
begin
  iR := 0;
  for i := 0 to Ord(High(TTableType)) do
    if TableNames[TTableType(i)] = aTableName then
    begin
      iR := i;
      Break;
    end;

  Result := TTableType(iR);
end;

function GetFieldDisplayName(const aTableType: TTableType; aFieldName: String): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Add('SELECT display_name FROM fields_mapping WHERE (table_name = :tabela) AND (field_name = :campo)');
    ParamByName('TABELA').AsString := TableNames[aTableType];
    ParamByName('CAMPO').AsString := aFieldName;
    Open;
    if RecordCount > 0 then
      Result := Fields[0].AsString;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetPrimaryKey(const aTableName: String): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Add('SELECT field_name FROM fields_mapping WHERE (table_name = :tabela) AND (integer_key = 1)');
    ParamByName('TABELA').AsString := aTableName;
    Open;
    if RecordCount > 0 then
      Result := Fields[0].AsString;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function IncNumInterno(aUser: Integer): Integer;
var
  i: Integer;
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT last_internal_id FROM users WHERE user_id = :auser');
    ParamByName('USER').AsInteger := aUser;
    Open;
    i := Fields[0].AsInteger;
    Close;
    inc(i, 1);
  finally
    FreeAndNil(Qry);
  end;
  Result := i;
end;

procedure GravaNumInterno(aUser: Integer; aNum: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE users');
    Add('SET last_internal_id = :lastcode');
    Add('WHERE user_id = :auser');
    ParamByName('LASTCODE').AsInteger := aNum;
    ParamByName('USER').AsInteger := aUser;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

function TableIsEmpty(aTableName: String): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    { SQLite }
    Add('SELECT count(rowid) FROM %tabname');
    MacroByName('TABNAME').Value := aTableName;
    Open;
    Result := Fields[0].AsInteger = 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TableExists(aTableName: String): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    { SQLite }
    Add('SELECT count(name) FROM sqlite_master WHERE name = %tabname');
    MacroByName('TABNAME').Value := aTableName;
    Open;
    Result := Fields[0].AsInteger > 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetLastInsertedKey(const aTable: String): Integer;
var
  Qry: TSQLQuery;
begin
  Result := 0;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    { SQLite }
    Add('SELECT DISTINCT last_insert_rowid() FROM %tabname');
    MacroByName('TABNAME').Value := aTable;
    Open;
    Result := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetLastInsertedKey(aTableType: TTableType): Integer;
begin
  Result := GetLastInsertedKey(TableNames[aTableType]);
end;

function GetLastGeneratedKey(GeneratorName: String; AutoIncrement: Boolean): Integer;
var
  Qry: TSQLQuery;
begin
  Result := 0;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    { Firebird SQL }
    Add('SELECT gen_id(%gerador, :savegen) FROM rdb$database;');
    MacroByName('GERADOR').Value := GeneratorName;
    ParamByName('SAVEGEN').AsInteger := Integer(AutoIncrement);
    Open;
    Result := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure DeleteAll(const aTableType: TTableType);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Add('DELETE FROM %tabname');
    MacroByName('TABNAME').Value := TableNames[aTableType];
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure ClearDeleted(OlderThan: Integer);
var
  Qry: TSQLQuery;
begin
  if OlderThan = 0 then
    Exit;

  if OlderThan < 0 then
    OlderThan := XSettings.ClearDeletedPeriod * 30;

  if not DMM.sqlCon.Connected then
    DMM.sqlCon.Open;

  try
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Transaction := DMM.sqlTrans;
      DMM.sqlTrans.StartTransaction;
      Clear;
      Add('DELETE FROM specimens WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM surveys WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM behavior WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM images WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM individuals WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM captures WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM permanent_nets WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM net_stations WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM nests WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM audio_library WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM botanic_taxa WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM gazetteer WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM institutions WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM methods WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM people WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM projects WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM taxon_ranks WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      Add('DELETE FROM zoo_taxa WHERE (active_status = 0) AND (date(update_date) = ' +
        'date(''now'',''localtime'', :olderthan)));');
      ParamByName('OLDERTHAN').AsString := '-' + IntToStr(OlderThan) + ' days';
      ExecSQL;
    finally
      FreeAndNil(Qry);
    end;
    DMM.sqlTrans.CommitRetaining;
    XSettings.LastClearDeleted := DateOf(Now);
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure OptimizeDB;
begin
  LogInfo('Vacuum database: ' + ConexaoDB.Name);
  DMM.sqlCon.ExecuteDirect('END TRANSACTION;');
  DMM.sqlCon.ExecuteDirect('VACUUM;');

  LogInfo('Optimize database: ' + ConexaoDB.Name);
  DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');

  CheckDB;
  XSettings.LastDatabaseOptimization := DateOf(Now);
  DMM.sqlCon.Open;
end;

function CheckDB: Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Add('PRAGMA integrity_check;');
    Open;
    if RecordCount > 0 then
      Result := Fields[0].AsString = 'ok';
    Close;
  finally
    FreeAndNil(Qry);
  end;
  if not Result then
  begin
    LogWarning('Database integrity check with errors: ' + ConexaoDB.Name);
    MsgDlg('', rsErrorDatabaseCorrupted, mtWarning);
  end
  else
    LogInfo('Database integrity check is OK: ' + ConexaoDB.Name);
end;

procedure OpenLookupDataSets(aDataSet: TDataSet);
var
  i: Integer;
begin
  for i := 0 to (aDataSet.Fields.Count - 1) do
    if (aDataSet.Fields[i].FieldKind = fkLookup) then
      if not aDataSet.Fields[i].LookupDataSet.Active then
        aDataSet.Fields[i].LookupDataSet.Open;
end;

{ ----------------------------------------------------------------------------------------- }
{ Record manipulation }
{ ----------------------------------------------------------------------------------------- }

// If not editing dataset, enter insert or edit mode
function CanEdit(aDataset: TDataset): Boolean;
begin
  Result := False;

  with aDataSet do
  begin
    if not Active then
    begin
      Open;
      Append;

      Result := True;
    end else
    begin
      if not(State in [dsInsert, dsEdit]) then
      begin
        if (RecordCount > 0) then
          Edit
        else
          Append;

        Result := True;
      end;
    end;
  end;
end;

// Inactivate record
procedure DeleteRecord(aTable: TTableType; aDataSet: TDataSet);
var
  Qry: TSQLQuery;
  aKeyField: String;
  aKeyValue: Integer;
begin
  // Confirmation dialog
  with DMM.TaskDlg do
  begin
    Title := rsDeleteRecordTitle;
    Text := rsDeleteRecordPrompt;
    Caption := rsTitleConfirmation;
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiNone;
    DefaultButton := tcbNo;
    FooterIcon := tdiInformation;
    FooterText := rsDeleteRecordFooter;
    if Execute then
      if ModalResult = mrNo then
        Exit;
  end;

  aKeyField := GetPrimaryKey(TableNames[aTable]);
  aKeyValue := aDataSet.FieldByName(aKeyField).AsInteger;

  {$IFDEF DEBUG}
  LogDebug(Format('Record %d from %s set inactive', [aKeyValue, TableNames[aTable]]));
  {$ENDIF}
  try
    DMM.sqlTrans.StartTransaction;
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      Add('UPDATE %tabname');
      Add('SET active_status = 0,');
      Add('update_date = datetime(''now'',''localtime''),');
      Add('user_updated = :auser');
      Add('WHERE %keyf = :cod');
      MacroByName('TABNAME').Value := TableNames[aTable];
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('AUSER').DataType := ftInteger;
      ParamByName('AUSER').AsInteger := ActiveUser.Id;
      ParamByName('COD').AsInteger := aKeyValue;
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}
      ExecSQL;
    finally
      FreeAndNil(Qry);
    end;
    WriteRecHistory(aTable, haDeleted, aKeyValue);
    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

// Activate record
procedure RestoreRecord(aTable: TTableType; aDataSet: TDataSet);
var
  Qry: TSQLQuery;
  aKeyField: String;
  aKeyValue: Integer;
begin
  if not MsgDlg(rsRestoreRecordTitle, rsRestoreRecordPrompt, mtConfirmation) then
    Exit;

  aKeyField := GetPrimaryKey(TableNames[aTable]);
  aKeyValue := aDataSet.FieldByName(aKeyField).AsInteger;

  {$IFDEF DEBUG}
  LogDebug(Format('Record %d from %s set active', [aKeyValue, TableNames[aTable]]));
  {$ENDIF}
  try
    DMM.sqlTrans.StartTransaction;
    WriteRecHistory(aTable, haRestored, aKeyValue);
    Qry := TSQLQuery.Create(DMM.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      Add('UPDATE %tabname');
      Add('SET active_status = 1,');
      Add('update_date = datetime(''now'',''localtime''),');
      Add('user_updated = :auser');
      Add('WHERE %keyf = :cod');
      MacroByName('TABNAME').Value := TableNames[aTable];
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('AUSER').AsInteger := ActiveUser.Id;
      ParamByName('COD').AsInteger := aKeyValue;
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}
      ExecSQL;
    finally
      FreeAndNil(Qry);
    end;
    DMM.sqlTrans.CommitRetaining;
  except
    DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

// Marca o registro
procedure MarkRecord(aTableName, aFieldName: String; aKeyValue: Integer);
var
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Record %d from %s set marked', [aKeyValue, aTableName]));
  {$ENDIF}
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE %tabname');
    Add('SET marked_status = 1');
    Add('WHERE %keyf = :keyv');
    MacroByName('TABNAME').Value := aTableName;
    MacroByName('KEYF').Value := aFieldName;
    ParamByName('KEYV').AsInteger := aKeyValue;
    {$IFDEF DEBUG}
    LogSQL(SQL);
    {$ENDIF}
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

// Desmarca o registro
procedure UnmarkRecord(aTableName, aFieldName: String; aKeyValue: Integer);
var
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Record %d from %s set unmarked', [aKeyValue, aTableName]));
  {$ENDIF}
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE %tabname');
    Add('SET marked_status = 0');
    Add('WHERE %keyf = :keyv');
    MacroByName('TABNAME').Value := aTableName;
    MacroByName('KEYF').Value := aFieldName;
    ParamByName('KEYV').AsInteger := aKeyValue;
    {$IFDEF DEBUG}
    LogSQL(SQL);
    {$ENDIF}
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure MarkAllRecords(const aTable: TTableType; IsChecked: Boolean; aKeyField: String;
  aModifier: TRecordStatus; aWhere: TStrings);
var
  Qry: TSQLQuery;
  sFilter, sChecked: String;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  try
    case aModifier.Status of
      rsAll:
        sFilter := EmptyStr;
      rsActive:
        sFilter := ' (ativo = 1)';
      rsInactive:
        sFilter := ' (ativo = 0)';
    end;

    sChecked := BoolToText(IsChecked, '1', '0');

    with Qry, SQL do
    begin
      MacroCheck := True;
      DataBase := DMM.sqlCon;

      Clear;
      SetCountSQL(SQL, aTable);
      AddStrings(aWhere);
      Add(')');
      Add('UPDATE %atable SET marked_status = :acheck' +
        ' WHERE %afield IN (SELECT %afield FROM lista)');
      MacroByName('TABNAME').Value := TableNames[aTable];
      MacroByName('AFIELD').Value := aKeyField;
      ParamByName('ACHECK').AsInteger := StrToInt(sChecked);
      //GravaLogSQL(SQL);
      ExecSQL;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure MarkAllRecords(const aTable: TTableType; aKeyField: String; aModifier: TRecordStatus;
  aWhere: TStrings; ackMarcados: TCheckBox);
var
  Qry: TSQLQuery;
  // m,ms: Integer;
  TabName: String;
  // sMarked,
  sFilter, Checked: String;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  try
    TabName := TableNames[aTable];

    case aModifier.Status of
      rsAll:
        ;
      rsActive:
        sFilter := ' (ativo = 1)';
      rsInactive:
        sFilter := ' (ativo = 0)';
    end;

    case ackMarcados.State of
      cbUnchecked:
        Checked := '1';
      cbChecked:
        Checked := '0';
      cbGrayed:
        Checked := '1';
    end;

    with Qry, SQL do
    begin
      Database := DMM.sqlCon;
      MacroCheck := True;
      Clear;
      SetCountSQL(SQL, aTable);
      AddStrings(aWhere);
      Add(')');
      Add('UPDATE %atable SET marked_status = :acheck' +
        ' WHERE %afield IN (SELECT %afield FROM lista)');
      MacroByName('ATABLE').Value := TabName;
      MacroByName('AFIELD').Value := aKeyField;
      ParamByName('ACHECK').DataType := ftInteger;
      ParamByName('ACHECK').AsString := Checked;
//      GravaLogSQL(SQL);
      ExecSQL;

      // Clear;
      // SetCountSQL(SQL,aTable);
      // Add('where'+sFilter+' and (reg_marcado = 1)');
      // Add(')');
      // Add('select count(ATIVO) as TOTAL_MARKED from LISTA');
      // GravaLogSQL(SQL);
      // Open;
      // m:= Fields[0].AsInteger;
      // Close;
      //
      // if (aWhere.Count > 0) then
      // begin
      // Clear;
      // SetCountSQL(SQL,aTable);
      // AddStrings(aWhere);
      // Add(')');
      // Add('select sum(MARCADO) as VIEW_MARKED from LISTA');
      // GravaLogSQL(SQL);
      // Open;
      // if Fields[0].IsNull then
      // ms:= 0
      // else ms:= Fields[0].AsInteger;
      // Close;
      // end else ms:= m;
    end;

    // if m = 1 then sMarked:= 'marcado' else sMarked:= 'marcados';
    // if ms = 0 then
    // ackMarcados.State:= cbUnchecked
    // else
    // if ms = m then
    // ackMarcados.State:= cbChecked
    // else
    // ackMarcados.State:= cbGrayed;

    // ackMarcados.Caption:= Format('%d de %d %s',[ms,m,sMarked]);
  finally
    FreeAndNil(Qry);
  end;
end;

// Envia registro para fila de impressão
procedure QueueRecord(aTableName, aFieldName: String; aKeyValue: Integer);
var
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Record %d from %s set queued', [aKeyValue, aTableName]));
  {$ENDIF}
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE %tabname');
    Add('SET queued_status = 1');
    Add('WHERE %keyf = :keyv');
    MacroByName('TABNAME').Value := aTableName;
    MacroByName('KEYF').Value := aFieldName;
    ParamByName('KEYV').AsInteger := aKeyValue;
    //GravaLogSQL(SQL);
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

// Remove registro da fila de impressсo
procedure UnqueueRecord(aTableName, aFieldName: String; aKeyValue: Integer);
var
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Record %d from %s set unqueued', [aKeyValue, aTableName]));
  {$ENDIF}
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE %tabname SET queued_status = 0 WHERE %keyf = :keyv');
    MacroByName('TABNAME').Value := aTableName;
    MacroByName('KEYF').Value := aFieldName;
    ParamByName('KEYV').AsInteger := aKeyValue;
    //GravaLogSQL(SQL);
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure UpdateBand(aBand, aIndividual: Integer; aStatus: String; aDate: TDate);
var
  Dt: String;
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Update band status: %d', [aBand]));
  {$ENDIF}
  Dt := FormatDateTime('yyyy-mm-dd', aDate);
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE bands SET individual_id = :individuo, band_status = :astatus,');
    if (aStatus = 'U') then
      Add('use_date = date(:adate),')
    else
    if (aStatus = 'R') or (aStatus = 'L') or (aStatus = 'Q') then
      Add('discharge_date = date(:adate),');
    Add('update_date = datetime(''now'',''localtime''), user_updated = :auser');
    Add('WHERE band_id = :keyv');
    ParamByName('INDIVIDUO').AsInteger := aIndividual;
    ParamByName('ADATE').AsString := Dt;
    ParamByName('ASTATUS').AsString := aStatus;
    ParamByName('AUSER').AsInteger := ActiveUser.Id;
    ParamByName('KEYV').AsInteger := aBand;
    //GravaLogSQL(SQL);
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure UpdateIndividual(aIndividual: Integer; aDate: TDate);
var
  Dt: String;
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Update individual: %d', [aIndividual]));
  {$ENDIF}
  Dt := FormatDateTime('yyyy-mm-dd', aDate);
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE individuals');
    Add('SET banding_date = date(:adate), ');
    Add('update_date = datetime(''now'',''localtime''),');
    Add('user_updated = :auser');
    Add('WHERE individual_id = :individuo');
    ParamByName('INDIVIDUO').AsInteger := aIndividual;
    ParamByName('ADATE').AsString := Dt;
    ParamByName('AUSER').AsInteger := ActiveUser.Id;
    //GravaLogSQL(SQL);
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure ChangeIndividualBand(aIndividual: Integer; aNewBand, aRemovedBand: Integer; aDate: TDate;
  aName: String);
var
  Dt: String;
  Qry: TSQLQuery;
begin
  {$IFDEF DEBUG}
  LogDebug(Format('Change individual %d band from %d to %d', [aIndividual, aRemovedBand, aNewBand]));
  {$ENDIF}
  Dt := FormatDateTime('yyyy-mm-dd', aDate);
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('UPDATE individuals');
    Add('SET band_id = :newband, ');
    Add('band_name = :bandname, ');
    Add('removed_band_id = :oldband, ');
    Add('band_change_date = date(:adate), ');
    Add('update_date = datetime(''now'',''localtime''),');
    Add('user_updated = :auser');
    Add('WHERE individual_id = :individuo');
    ParamByName('NEWBAND').AsInteger := aNewBand;
    ParamByName('OLDBAND').AsInteger := aRemovedBand;
    ParamByName('BANDNAME').AsString := aName;
    ParamByName('INDIVIDUO').AsInteger := aIndividual;
    ParamByName('ADATE').AsString := Dt;
    ParamByName('AUSER').AsInteger := ActiveUser.Id;
    //GravaLogSQL(SQL);
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure SetRecordDateUser(aDataSet: TDataSet);
var
  aUID: TGUID;
begin
  if (aDataSet.State = dsInsert) then
  begin
    aDataSet.FieldByName('insert_date').AsDateTime := Now;
    aDataSet.FieldByName('user_inserted').AsInteger := ActiveUser.Id;

    if aDataSet.FindField('uuid') <> nil then
      if CreateGUID(aUID) = 0 then
        aDataSet.FieldByName('uuid').AsString := GUIDToString(aUID);
  end
  else
  if (aDataSet.State = dsEdit) and (aDataSet.Modified) then
  begin
    aDataSet.FieldByName('update_date').AsDateTime := Now;
    aDataSet.FieldByName('user_updated').AsInteger := ActiveUser.Id;
  end;
end;

procedure CancelRecord(aDataSet: TDataSet; aFocusControl: TWinControl);
begin
  if (aDataSet.Modified) and (XSettings.ConfirmCancel) then
    if not MsgDlg('', rsCancelEditingPrompt, mtConfirmation) then
      Exit;

  aDataSet.Cancel;
  {$IFDEF DEBUG}
  LogDebug('Canceled editing');
  {$ENDIF}

  if aFocusControl.CanSetFocus then
    aFocusControl.SetFocus;
end;

end.
