{ Xolmis Data library

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

unit cbs_data;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, Variants, DateUtils, RegExpr, fileutil,
  { VCL }
  Controls, ExtCtrls, Forms, Dialogs, StdCtrls, DBCtrls, ComCtrls,
  { Data }
  DB, SQLDB,
  { CBS }
  cbs_datatypes;

const
  SchemaVersion: Integer = 1;

  { System database creation }
  function CreateSystemDatabase(aFilename: String): Boolean;
  procedure CreateConnectionsTable;
  procedure CreateTablesMappingTable;
  procedure CreateFieldsMappingTable;
  procedure CreateUsageDataTable;

  { User database }
  function CreateUserDatabase(aProtocol: TDBManager; aFilename, aName, aAuthor, aDescription: String): Boolean;
  function UpgradeDatabaseSchema(aProtocol: TDBManager): Boolean;
  function ReadDatabaseMetadata(aKey: String): String;
  procedure WriteDatabaseMetadata(aKey, aValue: String);

  procedure CreateDBMetadataTable;
  procedure CreateUsersTable;
  procedure CreateRecordHistoryTable;
  procedure CreateRecordVerificationsTable;
  procedure CreateTaxonRanksTable;
  procedure CreateZooTaxaTable;
  procedure CreateBotanicTaxaTable;
  procedure CreateMethodsTable;
  procedure CreateGazetteerTable;
  procedure CreateInstitutionsTable;
  procedure CreatePeopleTable;
  procedure CreateSamplingPlotsTable;
  procedure CreatePermanentNetsTable;
  procedure CreatePermitsTable;
  procedure CreateProjectsTable;
  procedure CreateProjectTeamTable;
  procedure CreateExpeditionsTable;
  procedure CreateSurveysTable;
  procedure CreateSurveyTeamTable;
  procedure CreateNetsEffortTable;
  procedure CreateWeatherLogsTable;
  procedure CreateVegetationTable;
  procedure CreateBandsTable;
  procedure CreateBandHistoryTable;
  procedure CreateIndividualsTable;
  procedure CreateSightingsTable;
  procedure CreateCapturesTable;
  procedure CreateMoltsTable;
  procedure CreateNestsTable;
  procedure CreateNestOwnersTable;
  procedure CreateNestRevisionsTable;
  procedure CreateEggsTable;
  procedure CreateSpecimensTable;
  procedure CreateSpecimenCollectorsTable;
  procedure CreateSamplePrepsTable;
  procedure CreatePoiLibraryTable;
  procedure CreateImagesTable;
  procedure CreateDocumentsTable;
  procedure CreateAudioLibraryTable;

  procedure CreateNextBirthdaysView;
  procedure CreateLastSurveysView;
  procedure CreateLastLifersView;
  procedure CreateExpiredPermitsView;
  procedure CreateBandsLeftoverView;
  procedure CreateBandsRunningOutView;
  procedure CreateAvgExpeditionDurationView;

  procedure PopulateZooTaxaTable(var aProgressBar: TProgressBar);

  { Database information and management }
  function GetTableType(aTableName: String): TTableType;
  function GetFieldDisplayName(const aTableType: TTableType; aFieldName: String): String;
  function GetPrimaryKey(const aTableName: String): String; overload;
  function GetPrimaryKey(const aDataSet: TDataSet): String; overload;
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
  cbs_locale, cbs_global, cbs_dialogs, cbs_conversions, cbs_system,
  cbs_count, udm_main, udlg_progress;

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
    User database
  -----------------------------------------------------------------------------------------
  }

// Do not forget to update the Create...Table/View procedures
// the SchemaVersion constant and the UpgradeDatabaseSchema function
// when the database schema change
function CreateUserDatabase(aProtocol: TDBManager; aFilename, aName, aAuthor, aDescription: String): Boolean;
begin
  Result := False;
  if DMM.sqlCon.Connected then
    DMM.sqlCon.Close;

  dlgProgress := TdlgProgress.Create(nil);
  try
    dlgProgress.Title := rsTitleCreateDatabase;
    dlgProgress.Text := rsProgressPreparing;
    dlgProgress.Max := 46; // Number of tables and views to create
    dlgProgress.Position := 0;
    case aProtocol of
      dbSqlite:
      begin
        {$IFDEF DEBUG}
        LogDebug('Creating database: ' + aFilename);
        {$ENDIF}
        try
          //Result := CopyFile(ConcatPaths([AppDataDir, 'XolmisDB_template.sqlite3']), aFilename, False, True);
          //
          //if Result then

          DMM.sqlCon.DatabaseName := aFilename;
          DMM.sqlCon.ConnectorType := 'SQLite3';
          DMM.sqlCon.Open;
          if not DMM.sqlTrans.Active then
            DMM.sqlTrans.StartTransaction;

          // Create database file
          dlgProgress.Text := rsProgressPreparing;
          DMM.sqlCon.CreateDB;

          DMM.sqlCon.ExecuteDirect('PRAGMA foreign_keys = off;');

          try
            // Create tables
            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleDBMetadata, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateDBMetadataTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleUsers, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateUsersTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleHistory, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateRecordHistoryTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleVerifications, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateRecordVerificationsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleTaxonRanks, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateTaxonRanksTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleZooTaxa, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateZooTaxaTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleBotanicTaxa, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateBotanicTaxaTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleMethods, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateMethodsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleGazetteer, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateGazetteerTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleInstitutions, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateInstitutionsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleResearchers, dlgProgress.Position + 1, dlgProgress.Max]);
            CreatePeopleTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSamplingPlots, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSamplingPlotsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitlePermanentNets, dlgProgress.Position + 1, dlgProgress.Max]);
            CreatePermanentNetsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitlePermits, dlgProgress.Position + 1, dlgProgress.Max]);
            CreatePermitsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleProjects, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateProjectsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleProjectMembers, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateProjectTeamTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsCaptionExpeditions, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateExpeditionsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSurveys, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSurveysTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSurveyTeam, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSurveyTeamTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleNetsEffort, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateNetsEffortTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleWeather, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateWeatherLogsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleVegetation, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateVegetationTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleBands, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateBandsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleBandHistory, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateBandHistoryTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleIndividuals, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateIndividualsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSightings, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSightingsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleCaptures, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateCapturesTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleMolts, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateMoltsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleNests, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateNestsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleNestOwners, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateNestOwnersTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleNestRevisions, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateNestRevisionsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleEggs, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateEggsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSpecimens, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSpecimensTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSpecimenCollectors, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSpecimenCollectorsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleSamplePreps, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateSamplePrepsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitlePoiLibrary, dlgProgress.Position + 1, dlgProgress.Max]);
            CreatePoiLibraryTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleImages, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateImagesTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleDocuments, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateDocumentsTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleAudioLibrary, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateAudioLibraryTable;
            dlgProgress.Position := dlgProgress.Position + 1;

            // Create views
            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleNextBirthdays, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateNextBirthdaysView;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleLastSurveys, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateLastSurveysView;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleLastLifers, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateLastLifersView;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleExpiredPermits, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateExpiredPermitsView;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleBandsBalance, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateBandsLeftoverView;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleBandsRunningOut, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateBandsRunningOutView;
            dlgProgress.Position := dlgProgress.Position + 1;

            dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleAvgExpeditionDuration, dlgProgress.Position + 1, dlgProgress.Max]);
            CreateAvgExpeditionDurationView;
            dlgProgress.Position := dlgProgress.Position + 1;

            // Populate tables
            dlgProgress.Text := rsProgressPopulatingTables;
            dlgProgress.PBar.Style := TProgressBarStyle.pbstMarquee;
            DMM.scriptUserDBInit.ExecuteScript;

            PopulateZooTaxaTable(dlgProgress.PBar);

          finally
            DMM.sqlCon.ExecuteDirect('PRAGMA foreign_keys = on;');
          end;

          // Write metadata to the database
          dlgProgress.Text := rsProgressFinishing;
          dlgProgress.PBar.Style := TProgressBarStyle.pbstMarquee;
          WriteDatabaseMetadata('name', aName);
          WriteDatabaseMetadata('author', aAuthor);
          WriteDatabaseMetadata('description', aDescription);
          WriteDatabaseMetadata('version', IntToStr(SchemaVersion));

          DMM.sqlTrans.CommitRetaining;

          // Optimize the database
          dlgProgress.Text := rsProgressOptimizingDatabase;
          DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');

          MsgDlg(rsTitleInformation, rsSuccessfulDatabaseCreation, mtInformation);
          LogInfo(Format('User database succesfully created (SQLite): %s', [aFileName]));
          Result := True;
        except
          on E: Exception do
          begin
            DMM.sqlTrans.RollbackRetaining;
            MsgDlg(rsTitleError, Format(rsErrorCreatingDatabaseSchema, [E.Message]), mtError);
            LogError(Format('Unable to create the user database (SQLite): %s', [aFileName]));
            Result := False;
          end;
        end;
      end;
      dbFirebird: ;
      dbPostgre: ;
      dbMaria: ;
    end;

  finally
    FreeAndNil(dlgProgress);
  end;
end;

// Do not forget to update the Create...Table/View procedures
// and the SchemaVersion constant
function UpgradeDatabaseSchema(aProtocol: TDBManager): Boolean;
var
  OldVersion: Integer;
begin
  Result := False;

  OldVersion := StrToIntDef(ReadDatabaseMetadata('version'), SchemaVersion);

  if not DMM.sqlCon.Connected then
    DMM.sqlCon.Open;
  DMM.sqlCon.ExecuteDirect('PRAGMA foreign_keys = off;');

  if not DMM.sqlTrans.Active then
    DMM.sqlTrans.StartTransaction;

  try
    try
      if OldVersion < 2 then
      begin

      end;

      DMM.sqlTrans.CommitRetaining;
      MsgDlg(rsTitleInformation, rsSuccessfulDatabaseUpgrade, mtInformation);
      LogInfo('User database succesfully upgraded');
      Result := True;
    except
      on E: Exception do
      begin
        DMM.sqlTrans.RollbackRetaining;
        MsgDlg(rsTitleError, Format(rsErrorUpgradingDatabaseSchema, [E.Message]), mtError);
        LogError('Unable to upgrade the database schema');
        Result := False;
      end;
    end;

  finally
    DMM.sqlCon.ExecuteDirect('PRAGMA foreign_keys = on;');
  end;

  if Result then
    WriteDatabaseMetadata('version', IntToStr(SchemaVersion));
end;

function ReadDatabaseMetadata(aKey: String): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT * FROM db_metadata');
    Add('WHERE property_name = :aname');
    ParamByName('aname').AsString := aKey;

    Open;
    if RecordCount > 0 then
      Result := FieldByName('property_value').AsString;

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure WriteDatabaseMetadata(aKey, aValue: String);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('INSERT OR REPLACE INTO db_metadata (');
    Add('property_name, property_value)');
    Add('VALUES (:aname, :avalue)');
    ParamByName('aname').AsString := aKey;
    ParamByName('avalue').AsString := aValue;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure CreateDBMetadataTable;
begin
  { Create table "db_metadata" }
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS db_metadata (' +
      'property_name  VARCHAR (40)  PRIMARY KEY UNIQUE NOT NULL, ' +
      'property_value VARCHAR (150) );');
end;

procedure CreateUsersTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS users (' +
    'user_id               INTEGER      PRIMARY KEY AUTOINCREMENT,' +
    'full_name             VARCHAR (60) NOT NULL,' +
    'user_name             VARCHAR (30) UNIQUE NOT NULL,' +
    'user_password         TEXT,' +
    'user_rank             VARCHAR (5),' +
    'allow_collection_edit BOOLEAN      DEFAULT (1),' +
    'allow_print           BOOLEAN      DEFAULT (1),' +
    'allow_export          BOOLEAN      DEFAULT (1),' +
    'allow_import          BOOLEAN      DEFAULT (1),' +
    'uuid                  VARCHAR (40),' +
    'user_inserted         INTEGER,' +
    'user_updated          INTEGER,' +
    'insert_date           DATETIME,' +
    'update_date           DATETIME,' +
    'exported_status       BOOLEAN      DEFAULT (0),' +
    'marked_status         BOOLEAN      DEFAULT (0),' +
    'active_status         BOOLEAN      DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_user_fullname ON users (' +
    'full_name COLLATE NOCASE' +
  ');')
end;

procedure CreateRecordHistoryTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS record_history (' +
    'event_id     INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'event_date   DATETIME,' +
    'user_id      INTEGER,' +
    'event_action VARCHAR (30)' +
    'event_table  VARCHAR (40),' +
    'record_id    INTEGER,' +
    'event_field  VARCHAR (60),' +
    'old_value    TEXT,' +
    'new_value    TEXT,' +
    'notes        TEXT,' +
    'FOREIGN KEY (user_id)' +
      'REFERENCES users (user_id) ON DELETE SET NULL ON UPDATE CASCADE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_history_table_record ON record_history (' +
    'event_table,' +
    'record_id' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_history_date ON record_history (' +
    'event_date COLLATE BINARY' +
  ');');
end;

procedure CreateRecordVerificationsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS record_verifications (' +
    'verification_id     INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'table_name          VARCHAR (40) NOT NULL,' +
    'record_id           INTEGER      NOT NULL,' +
    'verification_date   DATETIME,' +
    'verification_status VARCHAR (5)  NOT NULL,' +
    'person_id           INTEGER,' +
    'notes               TEXT' +
  ');');
end;

procedure CreateTaxonRanksTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS taxon_ranks (' +
    'rank_id         INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'rank_seq        INTEGER      NOT NULL,' +
    'rank_name       VARCHAR (30) NOT NULL UNIQUE,' +
    'rank_acronym    VARCHAR (15),' +
    'main_rank       BOOLEAN      DEFAULT (1),' +
    'subrank         BOOLEAN      DEFAULT (0),' +
    'infrarank       BOOLEAN      DEFAULT (0),' +
    'infraspecific   BOOLEAN      DEFAULT (0),' +
    'iczn            BOOLEAN      DEFAULT (1),' +
    'icbn            BOOLEAN      DEFAULT (1),' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN      DEFAULT (0),' +
    'marked_status   BOOLEAN      DEFAULT (0),' +
    'active_status   BOOLEAN      DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_rank_seq ON taxon_ranks (' +
    'rank_seq COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_rank_acronym ON taxon_ranks (' +
    'rank_acronym COLLATE BINARY' +
  ');');
end;

procedure CreateZooTaxaTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS zoo_taxa (' +
    'taxon_id               INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'full_name              VARCHAR (100) NOT NULL UNIQUE,' +
    'authorship             VARCHAR (150),' +
    'formatted_name         VARCHAR (250),' +
    'english_name           VARCHAR (100),' +
    'portuguese_name        VARCHAR (100),' +
    'spanish_name           VARCHAR (100),' +
    'quick_code             VARCHAR (10),' +
    'rank_id                INTEGER       NOT NULL REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
    'parent_taxon_id        INTEGER,' +
    'valid_id               INTEGER,' +
    'iucn_status            VARCHAR (5),' +
    'extinct                BOOLEAN       DEFAULT (0),' +
    'extinction_year        VARCHAR (25),' +
    'sort_num               REAL,' +
    'group_name             VARCHAR (40),' +
    'order_id               INTEGER,' +
    'family_id              INTEGER,' +
    'subfamily_id           INTEGER,' +
    'genus_id               INTEGER,' +
    'species_id             INTEGER,' +
    'subspecies_group_id    INTEGER,' +
    'incertae_sedis         INTEGER,' +
    'ebird_code             VARCHAR (20),' +
    'clements_taxonomy      BOOLEAN       DEFAULT (0),' +
    'ioc_taxonomy           BOOLEAN       DEFAULT (0),' +
    'ioc_rank_id            INTEGER       REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
    'ioc_parent_taxon_id    INTEGER,' +
    'ioc_valid_id           INTEGER,' +
    'ioc_sort_num           REAL,' +
    'ioc_english_name       VARCHAR (100),' +
    'cbro_taxonomy          BOOLEAN       DEFAULT (0),' +
    'other_portuguese_names VARCHAR (150),' +
    'user_inserted          INTEGER,' +
    'user_updated           INTEGER,' +
    'insert_date            DATETIME,' +
    'update_date            DATETIME,' +
    'exported_status        BOOLEAN       DEFAULT (0),' +
    'marked_status          BOOLEAN       DEFAULT (0),' +
    'active_status          BOOLEAN       DEFAULT (1),' +
    'distribution           TEXT,' +
    'ioc_distribution       TEXT' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_authorship ON zoo_taxa (' +
    'authorship COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_ebird_code ON zoo_taxa (' +
    'ebird_code COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_order ON zoo_taxa (' +
    'order_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_family ON zoo_taxa (' +
    'family_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_genus ON zoo_taxa (' +
    'genus_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_species ON zoo_taxa (' +
    'species_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_subspecies_group ON zoo_taxa (' +
    'subspecies_group_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_parent_taxon ON zoo_taxa (' +
    'parent_taxon_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_parent_taxon_ioc ON zoo_taxa (' +
    'ioc_parent_taxon_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_valid ON zoo_taxa (' +
    'valid_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_valid_ioc ON zoo_taxa (' +
    'ioc_valid_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_english_name ON zoo_taxa (' +
    'english_name COLLATE NOCASE' +
  ';');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_portuguese_name ON zoo_taxa (' +
    'portuguese_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_spanish_name ON zoo_taxa (' +
    'spanish_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_quickcodes ON zoo_taxa (' +
    'quick_code COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_rank ON zoo_taxa (' +
    'rank_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_rank_ioc ON zoo_taxa (' +
    'ioc_rank_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_sort_num ON zoo_taxa (' +
    'sort_num COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_sort_num_ioc ON zoo_taxa (' +
    'ioc_sort_num COLLATE BINARY' +
  ');');
end;

procedure CreateBotanicTaxaTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS botanic_taxa (' +
    'taxon_id        INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'taxon_name      VARCHAR (100) NOT NULL UNIQUE,' +
    'authorship      VARCHAR (100),' +
    'formatted_name  VARCHAR (180),' +
    'vernacular_name VARCHAR (100),' +
    'rank_id         INTEGER       NOT NULL REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
    'parent_taxon_id INTEGER,' +
    'valid_id        INTEGER,' +
    'order_id        INTEGER,' +
    'family_id       INTEGER,' +
    'genus_id        INTEGER,' +
    'species_id      INTEGER,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_parent_taxon ON botanic_taxa (' +
    'parent_taxon_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_valid ON botanic_taxa (' +
    'valid_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_order ON botanic_taxa (' +
    'order_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_family ON botanic_taxa (' +
    'family_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_genus ON botanic_taxa (' +
    'genus_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_species ON botanic_taxa (' +
    'species_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_taxa_vernacular_name ON botanic_taxa (' +
    'vernacular_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_botanic_tax_authorship ON botanic_taxa (' +
    'authorship COLLATE NOCASE' +
  ');');
end;

procedure CreateMethodsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS methods (' +
    'method_id       INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'method_name     VARCHAR (100) UNIQUE NOT NULL,' +
    'method_acronym  VARCHAR (20),' +
    'ebird_name      VARCHAR (60),' +
    'description     TEXT,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_methods_acronym ON methods (' +
    'method_acronym COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_methods_ebird_name ON methods (' +
    'ebird_name COLLATE NOCASE' +
  ');');
end;

procedure CreateGazetteerTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS gazetteer (' +
    'site_id         INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'site_name       VARCHAR (60)  NOT NULL,' +
    'site_acronym    VARCHAR (10),' +
    'longitude       REAL,' +
    'latitude        REAL,' +
    'altitude        REAL,' +
    'site_rank       CHAR (1),' +
    'parent_site_id  INTEGER,' +
    'country_id      INTEGER,' +
    'state_id        INTEGER,' +
    'municipality_id INTEGER,' +
    'full_name       VARCHAR (180),' +
    'ebird_name      VARCHAR (150),' +
    'language        VARCHAR (10),' +
    'description     TEXT,' +
    'notes           TEXT,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_site_name ON gazetteer (' +
    'site_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_fullname ON gazetteer (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_ebird_name ON gazetteer (' +
    'ebird_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_parent_site ON gazetteer (' +
    'parent_site_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_country ON gazetteer (' +
    'country_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_state ON gazetteer (' +
    'state_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_municipality ON gazetteer (' +
    'municipality_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_gazetteer_coordinate ON gazetteer (' +
    'longitude,' +
    'latitude' +
  ');');
end;

procedure CreateInstitutionsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS institutions (' +
    'institution_id  INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'full_name       VARCHAR (100) NOT NULL UNIQUE,' +
    'acronym         VARCHAR (15),' +
    'address_1       VARCHAR (100),' +
    'address_2       VARCHAR (40),' +
    'neighborhood    VARCHAR (60),' +
    'zip_code        VARCHAR (15),' +
    'municipality_id INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id        INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'country_id      INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'manager_name    VARCHAR (100),' +
    'email_addr      VARCHAR (60),' +
    'phone_num       VARCHAR (20),' +
    'notes           TEXT,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_institutions_acronym ON institutions (' +
    'acronym COLLATE NOCASE' +
  ');');
end;

procedure CreatePeopleTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS people (' +
    'person_id              INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'full_name              VARCHAR (100) NOT NULL,' +
    'acronym                VARCHAR (10)  UNIQUE NOT NULL,' +
    'citation               VARCHAR (100),' +
    'title_treatment        VARCHAR (10),' +
    'national_id_card       VARCHAR (15),' +
    'social_security_number VARCHAR (15),' +
    'gender                 VARCHAR (5),' +
    'birth_date             DATE,' +
    'death_date             DATE,' +
    'email_addr             VARCHAR (60),' +
    'phone_1                VARCHAR (20),' +
    'phone_2                VARCHAR (20),' +
    'address_1              VARCHAR (100),' +
    'address_2              VARCHAR (60),' +
    'neighborhood           VARCHAR (60),' +
    'zip_code               VARCHAR (15),' +
    'country_id             INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id               INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id        INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'institution_id         INTEGER       REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
    'department             VARCHAR (100),' +
    'job_role               VARCHAR (100),' +
    'lattes_uri             VARCHAR (30),' +
    'orcid_uri              VARCHAR (30),' +
    'twitter_uri            VARCHAR (50),' +
    'instagram_uri          VARCHAR (50),' +
    'website_uri            VARCHAR (100),' +
    'profile_color          VARCHAR (30),' +
    'notes                  TEXT,' +
    'profile_image          BLOB,' +
    'user_inserted          INTEGER,' +
    'user_updated           INTEGER,' +
    'insert_date            DATETIME,' +
    'update_date            DATETIME,' +
    'exported_status        BOOLEAN       DEFAULT (0),' +
    'marked_status          BOOLEAN       DEFAULT (0),' +
    'active_status          BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_people_fullname ON people (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_people_citation ON people (' +
    'citation COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_people_birthday ON people (' +
    'birth_date COLLATE BINARY' +
  ');');
end;

procedure CreateSamplingPlotsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS sampling_plots (' +
    'sampling_plot_id INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'full_name        VARCHAR (100) NOT NULL UNIQUE,' +
    'acronym          VARCHAR (10)  NOT NULL UNIQUE,' +
    'longitude        REAL,' +
    'latitude         REAL,' +
    'area_shape       VARCHAR (5),' +
    'country_id       INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id         INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id  INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'locality_id      INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'description      TEXT,' +
    'notes            TEXT,' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN       DEFAULT (0),' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreatePermanentNetsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS permanent_nets (' +
    'permanent_net_id INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'net_station_id   INTEGER       NOT NULL REFERENCES sampling_plots (sampling_plot_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'net_number       INTEGER       NOT NULL,' +
    'longitude        REAL,' +
    'latitude         REAL,' +
    'notes            VARCHAR (150),' +
    'full_name        VARCHAR (50),' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN       DEFAULT (0),' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreatePermitsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS legal (' +
    'permit_id       INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'project_id      INTEGER       REFERENCES projects (project_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'permit_name     VARCHAR (150),' +
    'permit_number   VARCHAR (30),' +
    'permit_type     VARCHAR (5),' +
    'dispatcher_name VARCHAR (100) NOT NULL,' +
    'dispatch_date   DATE,' +
    'expire_date     DATE,' +
    'notes           TEXT,' +
    'permit_filename VARCHAR (200),' +
    'permit_file     BLOB,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreateProjectsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS projects (' +
    'project_id       INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'project_title    VARCHAR (150) NOT NULL UNIQUE,' +
    'short_title      VARCHAR (60),' +
    'start_date       DATE,' +
    'end_date         DATE,' +
    'website_uri      VARCHAR (200),' +
    'email_addr       VARCHAR (100),' +
    'contact_name     VARCHAR (100),' +
    'project_file     VARCHAR (200),' +
    'contract_file    VARCHAR (200),' +
    'project_abstract TEXT,' +
    'notes            TEXT,' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN       DEFAULT (0),' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_projects_short_title ON projects (' +
    'short_title COLLATE NOCASE' +
  ');');
end;

procedure CreateProjectTeamTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS project_team (' +
    'project_member_id INTEGER  UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'project_id        INTEGER  REFERENCES projects (project_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'person_id         INTEGER  NOT NULL REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'project_manager   BOOLEAN  DEFAULT (0),' +
    'ser_inserted     INTEGER,' +
    'user_updated      INTEGER,' +
    'insert_date       DATETIME,' +
    'update_date       DATETIME,' +
    'exported_status   BOOLEAN  DEFAULT (0),' +
    'marked_status     BOOLEAN  DEFAULT (0),' +
    'active_status     BOOLEAN  DEFAULT (1)' +
  ');');
end;

procedure CreateExpeditionsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS expeditions (' +
    'expedition_id   INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'expedition_name VARCHAR (150) NOT NULL,' +
    'start_date      DATE,' +
    'end_date        DATE,' +
    'duration        INTEGER       AS ( (strftime(''%j'', end_date) - strftime(''%j'', start_date) ) + 1) VIRTUAL,' +
    'project_id      INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
    'locality_id     INTEGER,' +
    'country_id      INTEGER,' +
    'state_id        INTEGER,' +
    'municipality_id INTEGER,' +
    'description     TEXT,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_expeditions_name ON expeditions (' +
    'expedition_name COLLATE NOCASE' +
  ');');
end;

procedure CreateSurveysTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS surveys (' +
    'survey_id                INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'survey_date              DATE          NOT NULL,' +
    'start_time               TIME,' +
    'end_time                 TIME,' +
    'duration                 INTEGER,' +
    'method_id                INTEGER       REFERENCES methods (method_id) ON UPDATE CASCADE,' +
    'net_station_id           INTEGER       REFERENCES sampling_plots (sampling_plot_id) ON UPDATE CASCADE,' +
    'expedition_id            INTEGER       REFERENCES expeditions (expedition_id) ON UPDATE CASCADE,' +
    'project_id               INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
    'locality_id              INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'country_id               INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id                 INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id          INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'sample_id                VARCHAR (30),' +
    'start_latitude           REAL,' +
    'start_longitude          REAL,' +
    'end_latitude             REAL,' +
    'end_longitude            REAL,' +
    'observers_tally          INTEGER,' +
    'area_total               REAL,' +
    'distance_total           REAL,' +
    'nets_total               INTEGER,' +
    'habitat                  TEXT,' +
    'net_rounds               TEXT,' +
    'full_name                VARCHAR (100),' +
    'notes                    TEXT,' +
    'user_inserted            INTEGER,' +
    'user_updated             INTEGER,' +
    'insert_date              DATETIME,' +
    'update_date              DATETIME,' +
    'exported_status          BOOLEAN       DEFAULT (0),' +
    'marked_status            BOOLEAN       DEFAULT (0),' +
    'active_status            BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_surveys_date ON surveys (' +
    'survey_date COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_surveys_fullname ON surveys (' +
    'full_name COLLATE NOCASE' +
  ');');
end;

procedure CreateSurveyTeamTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS survey_team (' +
    'survey_member_id INTEGER  UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'survey_id        INTEGER,' +
    'person_id        INTEGER  NOT NULL REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'visitor          BOOLEAN  DEFAULT (0),' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN  DEFAULT (0),' +
    'marked_status    BOOLEAN  DEFAULT (0),' +
    'active_status    BOOLEAN  DEFAULT (1)' +
  ');');
end;

procedure CreateNetsEffortTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS nets_effort (' +
    'net_id           INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'survey_id        INTEGER,' +
    'net_station_id   INTEGER      REFERENCES net_stations (net_station_id) ON UPDATE CASCADE,' +
    'permanent_net_id INTEGER      REFERENCES permanent_nets (permanent_net_id) ON UPDATE CASCADE,' +
    'net_number       INTEGER,' +
    'longitude        REAL,' +
    'latitude         REAL,' +
    'sample_date      DATE,' +
    'net_open_1       TIME,' +
    'net_close_1      TIME,' +
    'net_open_2       TIME,' +
    'net_close_2      TIME,' +
    'net_open_3       TIME,' +
    'net_close_3      TIME,' +
    'net_open_4       TIME,' +
    'net_close_4      TIME,' +
    'open_time_total  REAL         AS (CAST ( (strftime(''%s'', ifnull(net_close_1, 0) ) - strftime(''%s'', ifnull(net_open_1, 0) ) ) AS REAL) / 60 / 60 + CAST ( (strftime(''%s'', ifnull(net_close_2, 0) ) - strftime(''%s'', ifnull(net_open_2, 0) ) ) AS REAL) / 60 / 60 + CAST ( (strftime(''%s'', ifnull(net_close_3, 0) ) - strftime(''%s'', ifnull(net_open_3, 0) ) ) AS REAL) / 60 / 60 + CAST ( (strftime(''%s'', ifnull(net_close_4, 0) ) - strftime(''%s'', ifnull(net_open_4, 0) ) ) AS REAL) / 60 / 60) STORED,' +
    'net_length       REAL,' +
    'net_height       REAL,' +
    'net_area         REAL         AS (ifnull(net_length, 0) * ifnull(net_height, 0) ) STORED,' +
    'net_mesh         VARCHAR (15),' +
    'full_name        VARCHAR (40),' +
    'notes            TEXT,' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN      DEFAULT (0),' +
    'marked_status    BOOLEAN      DEFAULT (0),' +
    'active_status    BOOLEAN      DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_nets_effort_survey_netnumber ON nets_effort (' +
    'survey_id,' +
    'net_number' +
  ');');
end;

procedure CreateWeatherLogsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS weather_logs (' +
    'weather_id           INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'survey_id            INTEGER,' +
    'sample_date          DATE     NOT NULL,' +
    'sample_time          TIME,' +
    'sample_moment        CHAR (1),' +
    'observer_id          INTEGER,' +
    'cloud_cover          INTEGER,' +
    'precipitation        CHAR (1),' +
    'rainfall             INTEGER,' +
    'temperature          REAL,' +
    'wind_speed_bft       INTEGER,' +
    'wind_speed_kmh       REAL,' +
    'relative_humidity    REAL,' +
    'atmospheric_pressure REAL,' +
    'notes                TEXT,' +
    'user_inserted        INTEGER,' +
    'user_updated         INTEGER,' +
    'insert_date          DATETIME,' +
    'update_date          DATETIME,' +
    'exported_status      BOOLEAN  DEFAULT (0),' +
    'marked_status        BOOLEAN  DEFAULT (0),' +
    'active_status        BOOLEAN  DEFAULT (1)' +
  ');');
end;

procedure CreateVegetationTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS vegetation (' +
    'vegetation_id       INTEGER  PRIMARY KEY AUTOINCREMENT NOT NULL UNIQUE,' +
    'survey_id           INTEGER,' +
    'sample_date         DATE     NOT NULL,' +
    'sample_time         TIME,' +
    'longitude           REAL,' +
    'latitude            REAL,' +
    'observer_id         INTEGER,' +
    'herbs_proportion    INTEGER,' +
    'herbs_distribution  INTEGER,' +
    'herbs_avg_height    INTEGER,' +
    'shrubs_proportion   INTEGER,' +
    'shrubs_distribution INTEGER,' +
    'shrubs_avg_height   INTEGER,' +
    'trees_proportion    INTEGER,' +
    'trees_distribution  INTEGER,' +
    'trees_avg_height    INTEGER,' +
    'notes               TEXT,' +
    'user_inserted       INTEGER,' +
    'user_updated        INTEGER,' +
    'insert_date         DATETIME,' +
    'update_date         DATETIME,' +
    'exported_status     BOOLEAN  DEFAULT (0),' +
    'marked_status       BOOLEAN  DEFAULT (0),' +
    'active_status       BOOLEAN  DEFAULT (1)' +
  ');');
end;

procedure CreateBandsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS bands (' +
    'band_id         INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'band_size       VARCHAR (5),' +
    'band_number     INTEGER,' +
    'band_status     VARCHAR (5),' +
    'band_type       VARCHAR (5),' +
    'band_prefix     VARCHAR (10),' +
    'band_suffix     VARCHAR (10),' +
    'band_color      VARCHAR (10),' +
    'band_source     VARCHAR (5),' +
    'supplier_id     INTEGER      REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
    'carrier_id      INTEGER      REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'individual_id   INTEGER      REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
    'project_id      INTEGER,' +
    'band_reported   BOOLEAN      DEFAULT (0),' +
    'notes           TEXT,' +
    'full_name       VARCHAR (40),' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN      DEFAULT (0),' +
    'marked_status   BOOLEAN      DEFAULT (0),' +
    'active_status   BOOLEAN      DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_bands_fullname ON bands (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_bands_band_size ON bands (' +
    'band_size' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_bands_number ON bands (' +
    'band_number COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_band_size_status ON bands (' +
    'band_size,' +
    'band_status' +
  ');');
end;

procedure CreateBandHistoryTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS band_history (' +
    'event_id        INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'band_id         INTEGER  REFERENCES bands (band_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'event_type      CHAR (5) NOT NULL,' +
    'event_date      DATE,' +
    'order_number    INTEGER,' +
    'supplier_id     INTEGER  REFERENCES institutions (institution_id) ON UPDATE CASCADE,' +
    'sender_id       INTEGER  REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'requester_id    INTEGER  REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'notes           TEXT,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN  DEFAULT (0),' +
    'marked_status   BOOLEAN  DEFAULT (0),' +
    'active_status   BOOLEAN  DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_band_history_date ON band_history (' +
    'event_date COLLATE BINARY ASC' +
  ');');
end;

procedure CreateIndividualsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS individuals (' +
    'individual_id         INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'formatted_name        VARCHAR (150),' +
    'full_name             VARCHAR (120),' +
    'taxon_id              INTEGER       NOT NULL REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'order_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'family_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'subfamily_id          INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'genus_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'species_id            INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'individual_sex        CHAR (1),' +
    'individual_age        CHAR (1),' +
    'nest_id               INTEGER       REFERENCES nests (nest_id) ON UPDATE CASCADE,' +
    'birth_date            VARCHAR (15),' +
    'birth_day             INTEGER,' +
    'birth_month           INTEGER,' +
    'birth_year            INTEGER,' +
    'banding_date          DATE,' +
    'band_change_date      DATE,' +
    'band_id               INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
    'double_band_id        INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
    'removed_band_id       INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
    'right_leg_below       VARCHAR (10),' +
    'left_leg_below        VARCHAR (10),' +
    'right_leg_above       VARCHAR (10),' +
    'left_leg_above        VARCHAR (10),' +
    'father_id             INTEGER,' +
    'mother_id             INTEGER,' +
    'death_date            VARCHAR (15),' +
    'death_day             INTEGER,' +
    'death_month           INTEGER,' +
    'death_year            INTEGER,' +
    'recognizable_markings TEXT,' +
    'notes                 TEXT,' +
    'user_inserted         INTEGER,' +
    'user_updated          INTEGER,' +
    'insert_date           DATETIME,' +
    'update_date           DATETIME,' +
    'exported_status       BOOLEAN       DEFAULT (0),' +
    'queued_status         BOOLEAN       DEFAULT (0),' +
    'marked_status         BOOLEAN       DEFAULT (0),' +
    'active_status         BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_individuals_fullname ON individuals (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_individuals_taxon ON individuals (' +
    'taxon_id COLLATE BINARY' +
  ');');
end;

procedure CreateSightingsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS sightings (' +
    'sighting_id          INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'survey_id            INTEGER,' +
    'individual_id        INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
    'sighting_date        DATE,' +
    'sighting_time        TIME,' +
    'locality_id          INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'longitude            REAL,' +
    'latitude             REAL,' +
    'method_id            INTEGER       REFERENCES methods (method_id) ON UPDATE CASCADE,' +
    'mackinnon_list_num   INTEGER,' +
    'observer_id          INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'taxon_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'subjects_tally       INTEGER,' +
    'subject_distance     REAL,' +
    'subject_seen         BOOLEAN       DEFAULT (0),' +
    'subject_heard        BOOLEAN       DEFAULT (0),' +
    'subject_photographed BOOLEAN       DEFAULT (0),' +
    'subject_recorded     BOOLEAN       DEFAULT (0),' +
    'subject_captured     BOOLEAN       DEFAULT (0),' +
    'males_tally          VARCHAR (10),' +
    'females_tally        VARCHAR (10),' +
    'not_sexed_tally      VARCHAR (10),' +
    'adults_tally         VARCHAR (10),' +
    'immatures_tally      VARCHAR (10),' +
    'not_aged_tally       VARCHAR (10),' +
    'new_captures_tally   INTEGER,' +
    'recaptures_tally     INTEGER,' +
    'unbanded_tally       INTEGER,' +
    'detection_type       VARCHAR (30),' +
    'breeding_status      VARCHAR (30),' +
    'not_surveying        BOOLEAN       DEFAULT (0),' +
    'ebird_available      BOOLEAN       DEFAULT (0),' +
    'order_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'family_id            INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'genus_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'species_id           INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'country_id           INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id             INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id      INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'full_name            VARCHAR (100),' +
    'notes                TEXT,' +
    'user_inserted        INTEGER,' +
    'user_updated         INTEGER,' +
    'insert_date          DATETIME,' +
    'update_date          DATETIME,' +
    'exported_status      BOOLEAN       DEFAULT (0),' +
    'marked_status        BOOLEAN       DEFAULT (0),' +
    'active_status        BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_sightings_fullname ON sightings (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_sightings_taxon ON sightings (' +
    'taxon_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_sightings_date ON sightings (' +
    'sighting_date COLLATE BINARY' +
  ');');
end;

procedure CreateCapturesTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS captures (' +
    'capture_id             INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'survey_id              INTEGER,' +
    'individual_id          INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
    'taxon_id               INTEGER       NOT NULL REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'full_name              VARCHAR (120),' +
    'project_id             INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
    'capture_date           DATE          NOT NULL,' +
    'capture_time           TIME,' +
    'locality_id            INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'net_station_id         INTEGER       REFERENCES net_stations (net_station_id) ON UPDATE CASCADE,' +
    'net_id                 INTEGER       REFERENCES nets_effort (net_id) ON UPDATE CASCADE,' +
    'longitude              REAL,' +
    'latitude               REAL,' +
    'bander_id              INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'annotator_id           INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'subject_status         CHAR (5),' +
    'capture_type           CHAR (5)      NOT NULL,' +
    'subject_sex            CHAR (5),' +
    'how_sexed              VARCHAR (10),' +
    'band_id                INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
    'removed_band_id        INTEGER       REFERENCES bands (band_id) ON UPDATE CASCADE,' +
    'right_leg_below        VARCHAR (10),' +
    'left_leg_below         VARCHAR (10),' +
    'right_leg_above        VARCHAR (10),' +
    'left_leg_above         VARCHAR (10),' +
    'weight                 REAL,' +
    'tarsus_length          REAL,' +
    'tarsus_diameter        REAL,' +
    'culmen_length          REAL,' +
    'exposed_culmen         REAL,' +
    'bill_width             REAL,' +
    'bill_height            REAL,' +
    'nostril_bill_tip       REAL,' +
    'skull_length           REAL,' +
    'halux_length_total     REAL,' +
    'halux_length_finger    REAL,' +
    'halux_length_claw      REAL,' +
    'right_wing_chord       REAL,' +
    'first_secondary_chord  REAL,' +
    'tail_length            REAL,' +
    'central_retrix_length  REAL,' +
    'external_retrix_length REAL,' +
    'total_length           REAL,' +
    'feather_mites          VARCHAR (15),' +
    'fat                    CHAR (5),' +
    'brood_patch            CHAR (5),' +
    'cloacal_protuberance   CHAR (5),' +
    'old_molt               CHAR (5),' +
    'old_primaries_molt     VARCHAR (20),' +
    'old_secondaries_molt   VARCHAR (40),' +
    'old_retrices_molt      VARCHAR (20),' +
    'old_body_molt          VARCHAR (20),' +
    'body_molt              CHAR (5),' +
    'flight_feathers_molt   CHAR (5),' +
    'flight_feathers_wear   CHAR (5),' +
    'molt_limits            VARCHAR (20),' +
    'cycle_code             CHAR (10),' +
    'subject_age            CHAR (5),' +
    'how_aged               CHAR (10),' +
    'skull_ossification     CHAR (5),' +
    'kipps_index            REAL,' +
    'glucose                REAL,' +
    'hemoglobin             REAL,' +
    'hematocrit             REAL,' +
    'philornis_larvae_tally INTEGER,' +
    'blood_sample           BOOLEAN       DEFAULT (0),' +
    'feather_sample         BOOLEAN       DEFAULT (0),' +
    'claw_sample            BOOLEAN       DEFAULT (0),' +
    'feces_sample           BOOLEAN       DEFAULT (0),' +
    'parasite_sample        BOOLEAN       DEFAULT (0),' +
    'subject_collected      BOOLEAN       DEFAULT (0),' +
    'subject_recorded       BOOLEAN       DEFAULT (0),' +
    'subject_photographed   BOOLEAN       DEFAULT (0),' +
    'field_number           VARCHAR (10),' +
    'photographer_1_id      INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'photographer_2_id      INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'start_photo_number     VARCHAR (20),' +
    'end_photo_number       VARCHAR (20),' +
    'camera_name            VARCHAR (50),' +
    'escaped                BOOLEAN       DEFAULT (0),' +
    'needs_review           BOOLEAN       DEFAULT (0),' +
    'order_id               INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'family_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'genus_id               INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'species_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'country_id             INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id               INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id        INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'notes                  TEXT,' +
    'user_inserted          INTEGER,' +
    'user_updated           INTEGER,' +
    'insert_date            DATETIME,' +
    'update_date            DATETIME,' +
    'exported_status        BOOLEAN       DEFAULT (0),' +
    'marked_status          BOOLEAN       DEFAULT (0),' +
    'active_status          BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_captures_fullname ON captures (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_captures_date ON captures (' +
    'capture_date COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_captures_taxon ON captures (' +
    'taxon_id COLLATE BINARY' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_captures_band ON captures (' +
    'band_id' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_captures_type_band ON captures (' +
    'capture_type,' +
    'band_id' +
  ');');
end;

procedure CreateMoltsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS molts (' +
    'molt_id         INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'survey_id       INTEGER,' +
    'full_name       VARCHAR (40),' +
    'taxon_id        INTEGER      NOT NULL REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'individual_id   INTEGER      REFERENCES individuals (individual_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'capture_id      INTEGER      REFERENCES captures (capture_id) ON UPDATE CASCADE,' +
    'sample_date     DATE         NOT NULL,' +
    'sample_time     TIME,' +
    'bander_id       INTEGER      REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'band_id         INTEGER      REFERENCES bands (band_id) ON UPDATE CASCADE,' +
    'p1_molt         REAL,' +
    'p2_molt         REAL,' +
    'p3_molt         REAL,' +
    'p4_molt         REAL,' +
    'p5_molt         REAL,' +
    'p6_molt         REAL,' +
    'p7_molt         REAL,' +
    'p8_molt         REAL,' +
    'p9_molt         REAL,' +
    'p10_molt        REAL,' +
    's1_molt         REAL,' +
    's2_molt         REAL,' +
    's3_molt         REAL,' +
    's4_molt         REAL,' +
    's5_molt         REAL,' +
    's6_molt         REAL,' +
    's7_molt         REAL,' +
    's8_molt         REAL,' +
    's9_molt         REAL,' +
    'r1_molt         REAL,' +
    'r2_molt         REAL,' +
    'r3_molt         REAL,' +
    'r4_molt         REAL,' +
    'r5_molt         REAL,' +
    'r6_molt         REAL,' +
    'pc1_molt        REAL,' +
    'pc2_molt        REAL,' +
    'pc3_molt        REAL,' +
    'pc4_molt        REAL,' +
    'pc5_molt        REAL,' +
    'pc6_molt        REAL,' +
    'pc7_molt        REAL,' +
    'pc8_molt        REAL,' +
    'pc9_molt        REAL,' +
    'cc_molt         REAL,' +
    'gc1_molt        REAL,' +
    'gc2_molt        REAL,' +
    'gc3_molt        REAL,' +
    'gc4_molt        REAL,' +
    'gc5_molt        REAL,' +
    'gc6_molt        REAL,' +
    'gc7_molt        REAL,' +
    'gc8_molt        REAL,' +
    'gc9_molt        REAL,' +
    'gc10_molt       REAL,' +
    'al1_molt        REAL,' +
    'al2_molt        REAL,' +
    'al3_molt        REAL,' +
    'lc_molt         REAL,' +
    'mc_molt         REAL,' +
    'growth_bar_size REAL,' +
    'notes           TEXT,' +
    'order_id        INTEGER,' +
    'family_id       INTEGER,' +
    'genus_id        INTEGER,' +
    'species_id      INTEGER,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN      DEFAULT (0),' +
    'marked_status   BOOLEAN      DEFAULT (0),' +
    'active_status   BOOLEAN      DEFAULT (1)' +
  ');');
end;

procedure CreateNestsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS nests (' +
    'nest_id               INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'field_number          VARCHAR (20)  UNIQUE NOT NULL,' +
    'observer_id           INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'project_id            INTEGER       REFERENCES projects (project_id) ON UPDATE CASCADE,' +
    'locality_id           INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'longitude             REAL,' +
    'latitude              REAL,' +
    'taxon_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'nest_shape            VARCHAR (5),' +
    'support_type          VARCHAR (10),' +
    'support_plant_1_id    INTEGER       REFERENCES botanic_taxa (taxon_id) ON UPDATE CASCADE,' +
    'support_plant_2_id    INTEGER       REFERENCES botanic_taxa (taxon_id) ON UPDATE CASCADE,' +
    'other_support         VARCHAR (60),' +
    'height_above_ground   REAL,' +
    'internal_max_diameter REAL,' +
    'internal_min_diameter REAL,' +
    'external_max_diameter REAL,' +
    'external_min_diameter REAL,' +
    'internal_height       REAL,' +
    'external_height       REAL,' +
    'edge_distance         REAL,' +
    'center_distance       REAL,' +
    'nest_cover            INTEGER,' +
    'plant_max_diameter    REAL,' +
    'plant_min_diameter    REAL,' +
    'plant_height          REAL,' +
    'plant_dbh             REAL,' +
    'construction_days     REAL,' +
    'incubation_days       REAL,' +
    'nestling_days         REAL,' +
    'active_days           REAL,' +
    'nest_fate             CHAR (1),' +
    'nest_productivity     INTEGER,' +
    'found_date            DATE,' +
    'last_date             DATE,' +
    'order_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'family_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'subfamily_id          INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'genus_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'species_id            INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'country_id            INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id              INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id       INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'full_name             VARCHAR (100),' +
    'description           TEXT,' +
    'notes                 TEXT,' +
    'user_inserted         INTEGER,' +
    'user_updated          INTEGER,' +
    'insert_date           DATETIME,' +
    'update_date           DATETIME,' +
    'exported_status       BOOLEAN       DEFAULT (0),' +
    'marked_status         BOOLEAN       DEFAULT (0),' +
    'active_status         BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_nests_fullname ON nests (' +
    'full_name COLLATE NOCASE' +
  ');');
end;

procedure CreateNestOwnersTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS nest_owners (' +
    'nest_owner_id   INTEGER     PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'nest_id         INTEGER     REFERENCES nests (nest_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'role            VARCHAR (5),' +
    'individual_id   INTEGER,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN     DEFAULT (0),' +
    'marked_status   BOOLEAN     DEFAULT (0),' +
    'active_status   BOOLEAN     DEFAULT (1)' +
  ');');
end;

procedure CreateNestRevisionsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS nest_revisions (' +
    'nest_revision_id             INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'nest_id                      INTEGER       REFERENCES nests (nest_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'full_name                    VARCHAR (100),' +
    'revision_date                DATE,' +
    'revision_time                TIME,' +
    'observer_1_id                INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'observer_2_id                INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'nest_status                  CHAR (1),' +
    'host_eggs_tally              INTEGER,' +
    'host_nestlings_tally         INTEGER,' +
    'nidoparasite_eggs_tally      INTEGER,' +
    'nidoparasite_nestlings_tally INTEGER,' +
    'nidoparasite_id              INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'have_philornis_larvae        BOOLEAN       DEFAULT (0),' +
    'nest_stage                   CHAR (1),' +
    'notes                        TEXT,' +
    'user_inserted                INTEGER,' +
    'user_updated                 INTEGER,' +
    'insert_date                  DATETIME,' +
    'update_date                  DATETIME,' +
    'exported_status              BOOLEAN       DEFAULT (0),' +
    'marked_status                BOOLEAN       DEFAULT (0),' +
    'active_status                BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreateEggsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS eggs (' +
    'egg_id           INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'nest_id          INTEGER       REFERENCES nests (nest_id) ON DELETE CASCADE ON UPDATE CASCADE,' +
    'egg_seq          INTEGER,' +
    'field_number     VARCHAR (20),' +
    'taxon_id         INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'eggshell_color   VARCHAR (40),' +
    'eggshell_pattern CHAR (5),' +
    'eggshell_texture CHAR (5),' +
    'egg_shape        CHAR (5),' +
    'egg_width        REAL,' +
    'egg_length       REAL,' +
    'egg_mass         REAL,' +
    'egg_volume       REAL,' +
    'egg_stage        CHAR (5),' +
    'egg_hatched      BOOLEAN       DEFAULT (1),' +
    'measure_date     DATE,' +
    'researcher_id    INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'individual_id    INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
    'host_egg         BOOLEAN       DEFAULT (1),' +
    'description      TEXT,' +
    'full_name        VARCHAR (100),' +
    'notes            TEXT,' +
    'order_id         INTEGER,' +
    'family_id        INTEGER,' +
    'genus_id         INTEGER,' +
    'species_id       INTEGER,' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN       DEFAULT (0),' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreateSpecimensTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS specimens (' +
    'specimen_id      INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'field_number     VARCHAR (20),' +
    'full_name        VARCHAR (100),' +
    'sample_type      CHAR (5),' +
    'taxon_id         INTEGER,' +
    'individual_id    INTEGER,' +
    'nest_id          INTEGER,' +
    'egg_id           INTEGER,' +
    'collection_date  DATE,' +
    'collection_day   INTEGER,' +
    'collection_month INTEGER,' +
    'collection_year  INTEGER,' +
    'locality_id      INTEGER,' +
    'longitude        REAL,' +
    'latitude         REAL,' +
    'order_id         INTEGER,' +
    'family_id        INTEGER,' +
    'subfamily_id     INTEGER,' +
    'genus_id         INTEGER,' +
    'species_id       INTEGER,' +
    'country_id       INTEGER,' +
    'state_id         INTEGER,' +
    'municipality_id  INTEGER,' +
    'notes            TEXT,' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN       DEFAULT (0),' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_specimens_fullname ON specimens (' +
    'full_name COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_specimens_fieldnumber ON specimens (' +
    'field_number COLLATE NOCASE' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_specimens_date ON specimens (' +
    'collection_date COLLATE BINARY' +
  ');');
end;

procedure CreateSpecimenCollectorsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS specimen_collectors (' +
    'collector_id    INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'specimen_id     INTEGER,' +
    'person_id       INTEGER,' +
    'collector_seq   INTEGER,' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN  DEFAULT (0),' +
    'marked_status   BOOLEAN  DEFAULT (0),' +
    'active_status   BOOLEAN  DEFAULT (1)' +
  ');');
end;

procedure CreateSamplePrepsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS sample_preps (' +
    'sample_prep_id   INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'specimen_id      INTEGER       REFERENCES specimens ON DELETE CASCADE ON UPDATE CASCADE,' +
    'accession_num    VARCHAR (20),' +
    'full_name        VARCHAR (100),' +
    'accession_type   CHAR (5),' +
    'accession_seq    INTEGER,' +
    'taxon_id         INTEGER,' +
    'individual_id    INTEGER,' +
    'nest_id          INTEGER,' +
    'egg_id           INTEGER,' +
    'preparation_date DATE,' +
    'preparer_id      INTEGER,' +
    'order_id         INTEGER,' +
    'family_id        INTEGER,' +
    'subfamily_id     INTEGER,' +
    'genus_id         INTEGER,' +
    'species_id       INTEGER,' +
    'country_id       INTEGER,' +
    'state_id         INTEGER,' +
    'municipality_id  INTEGER,' +
    'notes            TEXT,' +
    'user_inserted    INTEGER,' +
    'user_updated     INTEGER,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'exported_status  BOOLEAN       DEFAULT (0),' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');

  DMM.sqlCon.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_preparation_date ON sample_preps (' +
    'preparation_date COLLATE BINARY' +
  ');');
end;

procedure CreatePoiLibraryTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS poi_library (' +
    'poi_id          INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'sample_date     DATE         NOT NULL,' +
    'sample_time     TIME,' +
    'poi_name        VARCHAR (40),' +
    'longitude       REAL,' +
    'latitude        REAL,' +
    'altitude        REAL,' +
    'observer_id     INTEGER      REFERENCES people (person_id),' +
    'taxon_id        INTEGER      REFERENCES zoo_taxa (taxon_id),' +
    'individual_id   INTEGER      REFERENCES individuals (individual_id) ON DELETE CASCADE,' +
    'sighting_id     INTEGER,' +
    'survey_id       INTEGER      REFERENCES surveys (survey_id),' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN      DEFAULT (0),' +
    'marked_status   BOOLEAN      DEFAULT (0),' +
    'active_status   BOOLEAN      DEFAULT (1)' +
  ');');
end;

procedure CreateImagesTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS images (' +
    'image_id             INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'image_date           DATE,' +
    'image_time           TIME,' +
    'image_type           CHAR (5),' +
    'taxon_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'individual_id        INTEGER       REFERENCES individuals (individual_id) ON UPDATE CASCADE,' +
    'capture_id           INTEGER       REFERENCES captures (capture_id) ON UPDATE CASCADE,' +
    'locality_id          INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'author_id            INTEGER       REFERENCES people (person_id) ON UPDATE CASCADE,' +
    'survey_id            INTEGER,' +
    'sighting_id          INTEGER       REFERENCES sightings (sighting_id) ON UPDATE CASCADE,' +
    'nest_id              INTEGER       REFERENCES nests (nest_id) ON UPDATE CASCADE,' +
    'nest_revision_id     INTEGER       REFERENCES nest_revisions (nest_revision_id) ON UPDATE CASCADE,' +
    'egg_id               INTEGER       REFERENCES eggs (egg_id) ON UPDATE CASCADE,' +
    'specimen_id          INTEGER       REFERENCES specimens (specimen_id) ON UPDATE CASCADE,' +
    'image_filename       VARCHAR (300),' +
    'coordinate_precision CHAR (1),' +
    'longitude            REAL,' +
    'latitude             REAL,' +
    'order_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'family_id            INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'genus_id             INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'species_id           INTEGER       REFERENCES zoo_taxa (taxon_id) ON UPDATE CASCADE,' +
    'country_id           INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'state_id             INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'municipality_id      INTEGER       REFERENCES gazetteer (site_id) ON UPDATE CASCADE,' +
    'license_type         VARCHAR (20),' +
    'license_year         INTEGER,' +
    'license_uri          VARCHAR (200),' +
    'license_notes        VARCHAR (100),' +
    'license_owner        VARCHAR (150),' +
    'subtitle             TEXT,' +
    'image_thumbnail      BLOB,' +
    'user_inserted        INTEGER,' +
    'user_updated         INTEGER,' +
    'insert_date          DATETIME,' +
    'update_date          DATETIME,' +
    'exported_status      BOOLEAN       DEFAULT (0),' +
    'marked_status        BOOLEAN       DEFAULT (0),' +
    'active_status        BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreateDocumentsTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS documents (' +
    'document_id     INTEGER       PRIMARY KEY AUTOINCREMENT NOT NULL UNIQUE,' +
    'permit_id       INTEGER       REFERENCES legal (permit_id) ON DELETE CASCADE,' +
    'project_id      INTEGER       REFERENCES projects (project_id) ON DELETE CASCADE,' +
    'person_id       INTEGER       REFERENCES people (person_id) ON DELETE CASCADE,' +
    'individual_id   INTEGER       REFERENCES individuals (individual_id) ON DELETE CASCADE,' +
    'capture_id      INTEGER       REFERENCES captures (capture_id) ON DELETE CASCADE,' +
    'sighting_id     INTEGER       REFERENCES sightings (sighting_id) ON DELETE CASCADE,' +
    'specimen_id     INTEGER       REFERENCES specimens (specimen_id) ON DELETE CASCADE,' +
    'expedition_id   INTEGER       REFERENCES expeditions (expedition_id) ON DELETE CASCADE,' +
    'survey_id       INTEGER       REFERENCES surveys (survey_id) ON DELETE CASCADE,' +
    'nest_id         INTEGER       REFERENCES nests (nest_id) ON DELETE CASCADE,' +
    'net_station_id  INTEGER       REFERENCES sampling_plots (sampling_plot_id) ON DELETE CASCADE,' +
    'method_id       INTEGER       REFERENCES methods (method_id) ON DELETE CASCADE,' +
    'document_type   CHAR (5)      NOT NULL,' +
    'document_name   VARCHAR (120),' +
    'document_path   VARCHAR (250) NOT NULL,' +
    'document_date   DATE,' +
    'document_time   TIME,' +
    'license_type    VARCHAR (20),' +
    'license_year    INTEGER,' +
    'license_uri     VARCHAR (200),' +
    'license_notes   VARCHAR (100),' +
    'license_owner   VARCHAR (150),' +
    'user_inserted   INTEGER,' +
    'user_updated    INTEGER,' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'exported_status BOOLEAN       DEFAULT (0),' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreateAudioLibraryTable;
begin
  DMM.sqlCon.ExecuteDirect('CREATE TABLE IF NOT EXISTS audio_library (' +
    'audio_id          INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'full_name         VARCHAR (100),' +
    'taxon_id          INTEGER,' +
    'individual_id     INTEGER,' +
    'specimen_id       INTEGER,' +
    'sighting_id       INTEGER,' +
    'audio_type        VARCHAR (15),' +
    'locality_id       INTEGER,' +
    'recording_date    DATE,' +
    'recorder_id       INTEGER,' +
    'recording_time    TIME,' +
    'longitude         REAL,' +
    'latitude          REAL,' +
    'temperature       REAL,' +
    'cloud_cover       INTEGER,' +
    'precipitation     CHAR (1),' +
    'relative_humidity INTEGER,' +
    'wind_speed        INTEGER,' +
    'recording_context VARCHAR (60),' +
    'playback_used     BOOLEAN       DEFAULT (0),' +
    'subjects_tally    INTEGER,' +
    'habitat           VARCHAR (60),' +
    'recorder_model    VARCHAR (60),' +
    'mic_model         VARCHAR (60),' +
    'filter_model      VARCHAR (60),' +
    'distance          REAL,' +
    'license_type      VARCHAR (20),' +
    'license_year      INTEGER,' +
    'license_uri       VARCHAR (200),' +
    'license_notes     VARCHAR (100),' +
    'license_owner     VARCHAR (150),' +
    'audio_file        VARCHAR (250),' +
    'subtitle          TEXT,' +
    'notes             TEXT,' +
    'order_id          INTEGER,' +
    'family_id         INTEGER,' +
    'genus_id          INTEGER,' +
    'species_id        INTEGER,' +
    'country_id        INTEGER,' +
    'state_id          INTEGER,' +
    'municipality_id   INTEGER,' +
    'user_inserted     INTEGER,' +
    'user_updated      INTEGER,' +
    'insert_date       DATETIME,' +
    'update_date       DATETIME,' +
    'exported_status   BOOLEAN       DEFAULT (0),' +
    'marked_status     BOOLEAN       DEFAULT (0),' +
    'active_status     BOOLEAN       DEFAULT (1)' +
  ');');
end;

procedure CreateNextBirthdaysView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_next_birthdays AS' +
    'SELECT full_name,' +
      'birth_date,' +
      'strftime(''%d/%m'', birth_date) AS ANIVER,' +
      'strftime(''%j'', birth_date) - strftime(''%j'', ''now'') AS days_remaining' +
    'FROM people' +
    'WHERE (active_status = 1) AND' +
      '(birth_date NOTNULL)' +
    'ORDER BY CASE WHEN days_remaining >= 0 THEN days_remaining ELSE days_remaining + strftime(''%j'', strftime(''%Y-12-31'', ''now'') ) END;');
end;

procedure CreateLastSurveysView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_last_surveys AS' +
    'SELECT a.survey_date,' +
      'a.start_longitude,' +
      'a.start_latitude,' +
      'm.method_name,' +
      'g.full_name AS locality_name' +
    'FROM surveys AS a' +
    'JOIN methods AS m ON a.method_id = m.method_id' +
    'JOIN gazetteer AS g ON a.locality_id = g.site_id' +
    'WHERE a.active_status = 1' +
    'ORDER BY a.survey_date DESC;');
end;

procedure CreateLastLifersView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_last_lifers AS' +
    'WITH FirstRecords AS (' +
      'SELECT taxon_id,' +
        'MIN(capture_date) AS first_date,' +
        '''C'' AS tipo' +
      'FROM captures' +
      'WHERE active_status = 1' +
      'GROUP BY taxon_id' +
      'UNION ALL' +
      'SELECT taxon_id,' +
        'MIN(sighting_date) AS first_date,' +
        '''S'' AS tipo' +
      'FROM sightings' +
      'WHERE active_status = 1' +
      'GROUP BY taxon_id' +
    ')' +
    'SELECT z.taxon_id AS taxon,' +
      'z.full_name AS nome_taxon,' +
      'date(fr.first_date) AS data_registro,' +
      'fr.tipo' +
    'FROM zoo_taxa z' +
    'LEFT JOIN FirstRecords fr ON z.taxon_id = fr.taxon_id' +
    'WHERE data_registro NOTNULL' +
    'ORDER BY data_registro DESC;');
end;

procedure CreateExpiredPermitsView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_expired_permits AS' +
    'SELECT permit_name,' +
      'expire_date,' +
      'strftime(''%j'', expire_date) - strftime(''%j'', ''now'') AS days_remaining' +
      'FROM legal' +
    'WHERE (days_remaining <= 30) AND' +
      '(active_status = 1)' +
    'ORDER BY days_remaining ASC;');
end;

procedure CreateBandsLeftoverView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_bands_leftover AS' +
    'WITH BandSizes AS (' +
      'SELECT band_size,' +
        'COUNT( * ) AS total_bands' +
      'FROM bands' +
      'GROUP BY band_size' +
    '),' +
    'UsedBands AS (' +
      'SELECT band_size,' +
        'COUNT( * ) AS used_bands' +
      'FROM bands' +
      'WHERE (band_status != ''D'')' +
      'GROUP BY band_size' +
    '),' +
    'DailyAverage AS (' +
      'SELECT b.band_size,' +
        'DATE(c.capture_date) AS capture_date,' +
        'COUNT( * ) AS daily_count' +
      'FROM captures c' +
      'JOIN bands b ON c.band_id = b.band_id' +
      'WHERE c.capture_type = ''N''' +
      'GROUP BY b.band_size,' +
      'DATE(c.capture_date)' +
    '),' +
    'MaxDaily AS (' +
      'SELECT band_size,' +
        'MAX(daily_count) AS max_daily_count' +
      'FROM DailyAverage' +
      'GROUP BY band_size' +
    ')' +
    'SELECT bs.band_size,' +
      '(bs.total_bands - IFNULL(ub.used_bands, 0) ) AS saldo,' +
      'AVG(da.daily_count) AS media_dia,' +
      'md.max_daily_count AS maximo_dia' +
    'FROM BandSizes bs' +
    'LEFT JOIN UsedBands ub ON bs.band_size = ub.band_size' +
    'LEFT JOIN DailyAverage da ON bs.band_size = da.band_size' +
    'LEFT JOIN MaxDaily md ON bs.band_size = md.band_size' +
    'GROUP BY bs.band_size' +
    'ORDER BY bs.band_size;');
end;

procedure CreateBandsRunningOutView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_bands_running_out AS' +
    'WITH grupos AS (' +
      'SELECT date(capture_date) AS capture_date,' +
        'date(capture_date, ''-'' || DENSE_RANK() OVER (ORDER BY date(capture_date) ) || '' days'') AS grp' +
      'FROM captures' +
      'WHERE active_status = 1' +
      'GROUP BY capture_date' +
    '),' +
    'consecutivo AS (' +
      'SELECT COUNT(capture_date) AS consecutive_dates,' +
        'MIN(capture_date) AS min_date,' +
        'MAX(capture_date) AS max_date' +
      'FROM grupos' +
      'GROUP BY grp' +
    '),' +
    'expedicao AS (' +
      'SELECT AVG(consecutive_dates) AS media_dias_expedicao' +
      'FROM consecutivo' +
    '),' +
    'UsedBands AS (' +
      'SELECT bands.band_size AS tamanho,' +
        'COUNT(captures.band_id) AS conta' +
      'FROM captures' +
      'JOIN bands ON captures.band_id = bands.band_id' +
      'WHERE captures.capture_type = ''N'' AND' +
        'captures.active_status = 1' +
      'GROUP BY tamanho,' +
        'capture_date' +
    '),' +
    'DailyAverage AS (' +
      'SELECT ub.tamanho,' +
        'AVG(ub.conta) AS daily_use' +
      'FROM UsedBands AS ub' +
      'GROUP BY ub.tamanho' +
    '),' +
    'AvailableBands AS (' +
      'SELECT b.band_size,' +
        'COUNT(b2.band_number) AS remaining_bands' +
      'FROM bands AS b' +
      'LEFT JOIN bands AS b2 ON b.band_size = b2.band_size AND b2.band_status = ''D''' +
      'GROUP BY b.band_size' +
    ')' +
    'SELECT b1.band_size,' +
      'IFNULL(ab.remaining_bands, 0) AS saldo,' +
      'da.daily_use AS media_dia,' +
      '(da.daily_use * e.media_dias_expedicao) AS media_expedicao' +
    'FROM bands AS b1' +
    'JOIN expedicao AS e' +
    'LEFT JOIN AvailableBands AS ab ON b1.band_size = ab.band_size' +
    'LEFT JOIN DailyAverage AS da ON b1.band_size = da.tamanho' +
    'WHERE saldo < media_expedicao' +
    'GROUP BY b1.band_size' +
    'ORDER BY saldo ASC;');
end;

procedure CreateAvgExpeditionDurationView;
begin
  DMM.sqlCon.ExecuteDirect('CREATE VIEW IF NOT EXISTS get_average_expedition_duration AS' +
    'WITH grupos (' +
      'capture_date,' +
      'grp' +
    ')' +
    'AS (' +
      'SELECT DISTINCT date(c.capture_date),' +
        'date(c.capture_date, ''-'' || DENSE_RANK() OVER (ORDER BY date(capture_date) ) || '' days'') AS gr' +
      'FROM captures AS c' +
      'WHERE (c.active_status = 1)' +
    '),' +
    'consecutivo AS (' +
      'SELECT count( * ) AS consecutive_dates,' +
        'min(grupos.capture_date) AS min_date,' +
        'max(grupos.capture_date) AS max_date' +
      'FROM grupos' +
      'GROUP BY gr' +
      'ORDER BY 1 DESC, 2 DESC' +
    ')' +
    'SELECT avg(consecutive_dates) AS media_dias_expedicao' +
    'FROM consecutivo;');
end;

procedure PopulateZooTaxaTable(var aProgressBar: TProgressBar);
var
  Qry: TSQLQuery;
begin
  with DMM.batchCsvRead do
  try
    Delimiter := ';';
    FirstLineAsSchema := True;
    FileName := ConcatPaths([AppDataDir, 'zoo_taxa_init.csv']);
    CodePage := 'Windows-1252';
    Open;
    aProgressBar.Position := 0;
    aProgressBar.Style := TProgressBarStyle.pbstNormal;
    aProgressBar.Max := RecordCount;

    Qry := TSQLQuery.Create(nil);
    Qry.SQLConnection := DMM.sqlCon;
    Qry.SQLTransaction := DMM.sqlTrans;
    Qry.SQL.Text := 'INSERT INTO zoo_taxa (' +
        'taxon_id,' +
        'full_name,' +
        'authorship,' +
        'formatted_name,' +
        'english_name,' +
        'portuguese_name,' +
        'spanish_name,' +
        'quick_code,' +
        'rank_id,' +
        'parent_taxon_id,' +
        'valid_id,' +
        'iucn_status,' +
        'extinct,' +
        'extinction_year,' +
        'sort_num,' +
        'group_name,' +
        'order_id,' +
        'family_id,' +
        'subfamily_id,' +
        'genus_id,' +
        'species_id,' +
        'subspecies_group_id,' +
        'incertae_sedis,' +
        'ebird_code,' +
        'clements_taxonomy,' +
        'ioc_taxonomy,' +
        'ioc_rank_id,' +
        'ioc_parent_taxon_id,' +
        'ioc_valid_id,' +
        'ioc_sort_num,' +
        'ioc_english_name,' +
        'cbro_taxonomy,' +
        'other_portuguese_names,' +
        'user_inserted,' +
        'user_updated,' +
        'insert_date,' +
        'update_date,' +
        'exported_status,' +
        'marked_status,' +
        'active_status,' +
        'distribution,' +
        'ioc_distribution) ' +
      'VALUES (' +
        ':taxon_id,' +
        ':full_name,' +
        ':authorship,' +
        ':formatted_name,' +
        ':english_name,' +
        ':portuguese_name,' +
        ':spanish_name,' +
        ':quick_code,' +
        ':rank_id,' +
        ':parent_taxon_id,' +
        ':valid_id,' +
        ':iucn_status,' +
        ':extinct,' +
        ':extinction_year,' +
        ':sort_num,' +
        ':group_name,' +
        ':order_id,' +
        ':family_id,' +
        ':subfamily_id,' +
        ':genus_id,' +
        ':species_id,' +
        ':subspecies_group_id,' +
        ':incertae_sedis,' +
        ':ebird_code,' +
        ':clements_taxonomy,' +
        ':ioc_taxonomy,' +
        ':ioc_rank_id,' +
        ':ioc_parent_taxon_id,' +
        ':ioc_valid_id,' +
        ':ioc_sort_num,' +
        ':ioc_english_name,' +
        ':cbro_taxonomy,' +
        ':other_portuguese_names,' +
        ':user_inserted,' +
        ':user_updated,' +
        'datetime(:insert_date),' +
        'datetime(:update_date),' +
        ':exported_status,' +
        ':marked_status,' +
        ':active_status,' +
        ':distribution,' +
        ':ioc_distribution)';
    try
      while not EOF do
      begin
        Qry.ParamByName('taxon_id').AsInteger := FieldByName('taxon_id').AsInteger;
        Qry.ParamByName('full_name').AsString := FieldByName('full_name').AsString;
        Qry.ParamByName('authorship').AsString := FieldByName('authorship').AsString;
        Qry.ParamByName('formatted_name').AsString := FieldByName('formatted_name').AsString;
        Qry.ParamByName('english_name').AsString := FieldByName('english_name').AsString;
        Qry.ParamByName('portuguese_name').AsString := FieldByName('portuguese_name').AsString;
        Qry.ParamByName('spanish_name').AsString := FieldByName('spanish_name').AsString;
        Qry.ParamByName('quick_code').AsString := FieldByName('quick_code').AsString;
        Qry.ParamByName('rank_id').AsInteger := FieldByName('rank_id').AsInteger;
        Qry.ParamByName('parent_taxon_id').AsInteger := FieldByName('parent_taxon_id').AsInteger;
        Qry.ParamByName('valid_id').AsInteger := FieldByName('valid_id').AsInteger;
        Qry.ParamByName('iucn_status').AsString := FieldByName('iucn_status').AsString;
        Qry.ParamByName('extinct').AsBoolean := FieldByName('extinct').AsBoolean;
        Qry.ParamByName('extinction_year').AsString := FieldByName('extinction_year').AsString;
        Qry.ParamByName('sort_num').AsFloat := FieldByName('sort_num').AsFloat;
        Qry.ParamByName('group_name').AsString := FieldByName('group_name').AsString;
        Qry.ParamByName('order_id').AsInteger := FieldByName('order_id').AsInteger;
        Qry.ParamByName('family_id').AsInteger := FieldByName('family_id').AsInteger;
        Qry.ParamByName('subfamily_id').AsInteger := FieldByName('subfamily_id').AsInteger;
        Qry.ParamByName('genus_id').AsInteger := FieldByName('genus_id').AsInteger;
        Qry.ParamByName('species_id').AsInteger := FieldByName('species_id').AsInteger;
        Qry.ParamByName('subspecies_group_id').AsInteger := FieldByName('subspecies_group_id').AsInteger;
        Qry.ParamByName('incertae_sedis').AsBoolean := FieldByName('incertae_sedis').AsBoolean;
        Qry.ParamByName('ebird_code').AsString := FieldByName('ebird_code').AsString;
        Qry.ParamByName('clements_taxonomy').AsBoolean := FieldByName('clements_taxonomy').AsBoolean;
        Qry.ParamByName('ioc_taxonomy').AsBoolean := FieldByName('ioc_taxonomy').AsBoolean;
        Qry.ParamByName('ioc_rank_id').AsInteger := FieldByName('ioc_rank_id').AsInteger;
        Qry.ParamByName('ioc_parent_taxon_id').AsInteger := FieldByName('ioc_parent_taxon_id').AsInteger;
        Qry.ParamByName('ioc_valid_id').AsInteger := FieldByName('ioc_valid_id').AsInteger;
        Qry.ParamByName('ioc_sort_num').AsFloat := FieldByName('ioc_sort_num').AsFloat;
        Qry.ParamByName('ioc_english_name').AsString := FieldByName('ioc_english_name').AsString;
        Qry.ParamByName('cbro_taxonomy').AsBoolean := FieldByName('cbro_taxonomy').AsBoolean;
        Qry.ParamByName('other_portuguese_names').AsString := FieldByName('other_portuguese_names').AsString;
        Qry.ParamByName('user_inserted').AsInteger := FieldByName('user_inserted').AsInteger;
        Qry.ParamByName('user_updated').AsInteger := FieldByName('user_updated').AsInteger;
        Qry.ParamByName('insert_date').AsDateTime := FieldByName('insert_date').AsDateTime;
        Qry.ParamByName('update_date').AsDateTime := FieldByName('update_date').AsDateTime;
        Qry.ParamByName('exported_status').AsBoolean := FieldByName('exported_status').AsBoolean;
        Qry.ParamByName('marked_status').AsBoolean := FieldByName('marked_status').AsBoolean;
        Qry.ParamByName('active_status').AsBoolean := FieldByName('active_status').AsBoolean;
        Qry.ParamByName('distribution').AsString := FieldByName('distribution').AsString;
        Qry.ParamByName('ioc_distribution').AsString := FieldByName('ioc_distribution').AsString;

        Qry.ExecSQL;
        aProgressBar.Position := RecNo;

        Next;
      end;

      DMM.sqlTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        DMM.sqlTrans.RollbackRetaining;
        MsgDlg(rsTitleError, Format(rsErrorPopulatingTables, [E.Message]), mtError);
      end;
    end;
  finally
    FreeAndNil(Qry);
    Close;
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

function GetPrimaryKey(const aDataSet: TDataSet): String;
var
  i: Integer;
begin
  Result := EmptyStr;

  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    for i := 0 to Fields.Count - 1 do
      if pfInKey in Fields[i].ProviderFlags then
        Result := Fields[i].FieldName;
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
