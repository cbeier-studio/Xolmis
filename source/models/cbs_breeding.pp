{ Xolmis Breeding Data library

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

unit cbs_breeding;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, cbs_record_types;

type
  TNestFate = (nfLoss, nfSuccess, nfUnknown);

const
  NestFates: array [TNestFate] of Char = ('L', 'S', 'U');

type

  { TNest }

  TNest = class(TXolmisRecord)
  protected
    FFieldNumber: String;
    FFullName: String;
    FObserverId: Integer;
    FLocalityId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FTaxonId: Integer;
    FNestShape: String;
    FSupportType: String;
    FSupportPlant1Id: Integer;
    FSupportPlant2Id: Integer;
    FOtherSupport: String;
    FHeightAboveGround: Double;
    FProjectId: Integer;
    FInternalMaxDiameter: Double;
    FInternalMinDiameter: Double;
    FExternalMaxDiameter: Double;
    FExternalMinDiameter: Double;
    FInternalHeight: Double;
    FExternalHeight: Double;
    FEdgeDistance: Double;
    FCenterDistance: Double;
    FNestCover: Integer;
    FPlantMaxDiameter: Double;
    FPlantMinDiameter: Double;
    FPlantHeight: Double;
    FPlantDbh: Double;
    FConstructionDays: Double;
    FIncubationDays: Double;
    FNestlingDays: Double;
    FActiveDays: Double;
    FNestFate: TNestFate;
    FNestProductivity: Integer;
    FFoundDate: TDate;
    FLastDate: TDate;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Find(aFieldNumber: String; aTaxon, aSite: Integer; aDate: TDate): Boolean;
    function Diff(aOld: TNest; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TNest);
    function ToJSON: String;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property FullName: String read FFullName write FFullName;
    property ObserverId: Integer read FObserverId write FObserverId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property ProjectId: Integer read FProjectId write FProjectId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property NestShape: String read FNestShape write FNestShape;
    property SupportType: String read FSupportType write FSupportType;
    property SupportPlant1Id: Integer read FSupportPlant1Id write FSupportPlant1Id;
    property SupportPlant2Id: Integer read FSupportPlant2Id write FSupportPlant2Id;
    property OtherSupport: String read FOtherSupport write FOtherSupport;
    property HeightAboveGround: Double read FHeightAboveGround write FHeightAboveGround;
    property InternalMaxDiameter: Double read FInternalMaxDiameter write FInternalMaxDiameter;
    property InternalMinDiameter: Double read FInternalMinDiameter write FInternalMinDiameter;
    property ExternalMaxDiameter: Double read FExternalMaxDiameter write FExternalMaxDiameter;
    property ExternalMinDiameter: Double read FExternalMinDiameter write FExternalMinDiameter;
    property InternalHeight: Double read FInternalHeight write FInternalHeight;
    property ExternalHeight: Double read FExternalHeight write FExternalHeight;
    property EdgeDistance: Double read FEdgeDistance write FEdgeDistance;
    property CenterDistance: Double read FCenterDistance write FCenterDistance;
    property NestCover: Integer read FNestCover write FNestCover;
    property PlantMaxDiameter: Double read FPlantMaxDiameter write FPlantMaxDiameter;
    property PlantMinDiameter: Double read FPlantMinDiameter write FPlantMinDiameter;
    property PlantHeight: Double read FPlantHeight write FPlantHeight;
    property PlantDbh: Double read FPlantDbh write FPlantDbh;
    property ConstructionDays: Double read FConstructionDays write FConstructionDays;
    property IncubationDays: Double read FIncubationDays write FIncubationDays;
    property NestlingDays: Double read FNestlingDays write FNestlingDays;
    property ActiveDays: Double read FActiveDays write FActiveDays;
    property NestFate: TNestFate read FNestFate write FNestFate;
    property NestProductivity: Integer read FNestProductivity write FNestProductivity;
    property FoundDate: TDate read FFoundDate write FFoundDate;
    property LastDate: TDate read FLastDate write FLastDate;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

type
  TNestRole = (nrlUnknown, nrlMale, nrlFemale, nrlHelper, nrlOffspring);

const
  NestRoles: array[TNestRole] of Char = ('U', 'M', 'F', 'H', 'O');

type

  { TNestOwner }

  TNestOwner = class(TXolmisRecord)
  private
    FIndividualId: Integer;
    FNestId: Integer;
    FRole: TNestRole;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TNestOwner; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TNestOwner);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property NestId: Integer read FNestId write FNestId;
    property Role: TNestRole read FRole write FRole;
    property IndividualId: Integer read FIndividualId write FIndividualId;
  end;

type
  TEggShape = (esUnknown, esSpherical, esElliptical, esOval, esPiriform, esConical, esBiconical, esCylindrical,
    esLongitudinal);
  TEggshellPattern = (espUnknown, espSpots, espBlotches, espSquiggles, espStreaks, espScrawls, espSpotsSquiggles,
    espBlotchesSquiggles);
  TEggshellTexture = (estUnknown, estChalky, estShiny, estGlossy, estPitted);

const
  EggShapes: array [TEggShape] of Char = ('U', 'S', 'E', 'O', 'P', 'C', 'B', 'Y', 'L');
  EggshellPatterns: array[TEggshellPattern] of String = ('U', 'P', 'B', 'S', 'T', 'W', 'PS', 'BS');
  EggshellTextures: array[TEggshellTexture] of Char = ('U', 'C', 'S', 'G', 'P');

type

  { TEgg }

  TEgg = class(TXolmisRecord)
  protected
    FFieldNumber: String;
    FEggSeq: Integer;
    FNestId: Integer;
    FFullName: String;
    FEggShape: TEggShape;
    FWidth: Double;
    FLength: Double;
    FMass: Double;
    FVolume: Double;
    FEggStage: String;
    FEggshellColor: String;
    FEggshellPattern: TEggshellPattern;
    FEggshellTexture: TEggshellTexture;
    FEggHatched: Boolean;
    FIndividualId: Integer;
    FResearcherId: Integer;
    FMeasureDate: TDate;
    FTaxonId: Integer;
    FHostEgg: Boolean;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TEgg; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    function Find(aNest: Integer; aFieldNumber, aDate: String; aObserver: Integer): Boolean;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TEgg);
    function ToJSON: String;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property EggSeq: Integer read FEggSeq write FEggSeq;
    property NestId: Integer read FNestId write FNestId;
    property FullName: String read FFullName write FFullName;
    property EggShape: TEggShape read FEggShape write FEggShape;
    property Width: Double read FWidth write FWidth;
    property Length: Double read FLength write FLength;
    property Mass: Double read FMass write FMass;
    property Volume: Double read FVolume write FVolume;
    property EggStage: String read FEggStage write FEggStage;
    property EggshellColor: String read FEggshellColor write FEggshellColor;
    property EggshellPattern: TEggshellPattern read FEggshellPattern write FEggshellPattern;
    property EggshellTexture: TEggshellTexture read FEggshellTexture write FEggshellTexture;
    property EggHatched: Boolean read FEggHatched write FEggHatched;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property ResearcherId: Integer read FResearcherId write FResearcherId;
    property MeasureDate: TDate read FMeasureDate write FMeasureDate;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property HostEgg: Boolean read FHostEgg write FHostEgg;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

type
  TNestStatus = (nstInactive, nstActive, nstUnknown);
  TNestStage = (nsgInactive, nsgConstruction, nsgLaying, nsgIncubation, nsgHatching, nsgNestling, nsgUnknown);

const
  NestStates: array[TNestStatus] of Char = ('I', 'A', 'U');
  NestStages: array[TNestStage] of Char = ('X', 'C', 'L', 'I', 'H', 'N', 'U');

type

  { TNestRevision }

  TNestRevision = class(TXolmisRecord)
  protected
    FNestId: Integer;
    FFullName: String;
    FRevisionDate: TDate;
    FRevisionTime: TTime;
    FObserver1Id: Integer;
    FObserver2Id: Integer;
    FNestStatus: TNestStatus;
    FHostEggsTally: Integer;
    FHostNestlingsTally: Integer;
    FNidoparasiteEggsTally: Integer;
    FNidoparasiteNestlingsTally: Integer;
    FNidoparasiteId: Integer;
    FHavePhilornisLarvae: Boolean;
    FNestStage: TNestStage;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TNestRevision; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    function Find(aNest: Integer; aDate, aTime: String; aObserver: Integer): Boolean;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TNestRevision);
    function ToJSON: String;
  published
    property NestId: Integer read FNestId write FNestId;
    property FullName: String read FFullName write FFullName;
    property RevisionDate: TDate read FRevisionDate write FRevisionDate;
    property RevisionTime: TTime read FRevisionTime write FRevisionTime;
    property Observer1Id: Integer read FObserver1Id write FObserver1Id;
    property Observer2Id: Integer read FObserver2Id write FObserver2Id;
    property NestStatus: TNestStatus read FNestStatus write FNestStatus;
    property HostEggsTally: Integer read FHostEggsTally write FHostEggsTally;
    property HostNestlingsTally: Integer read FHostNestlingsTally write FHostNestlingsTally;
    property NidoparasiteEggsTally: Integer read FNidoparasiteEggsTally write FNidoparasiteEggsTally;
    property NidoparasiteNestlingsTally: Integer read FNidoparasiteNestlingsTally write FNidoparasiteNestlingsTally;
    property NidoparasiteId: Integer read FNidoparasiteId write FNidoparasiteId;
    property HavePhilornisLarvae: Boolean read FHavePhilornisLarvae write FHavePhilornisLarvae;
    property NestStage: TNestStage read FNestStage write FNestStage;
    property Notes: String read FNotes write FNotes;
  end;

implementation

uses
  cbs_locale, cbs_global, cbs_users, cbs_datacolumns, cbs_validations, udm_main;

{ TNestRevision }

constructor TNestRevision.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TNestRevision.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FNestId := 0;
  FRevisionDate := NullDate;
  FRevisionTime := NullTime;
  FObserver1Id := 0;
  FObserver2Id := 0;
  FNestStatus := nstUnknown;
  FHostEggsTally := 0;
  FHostNestlingsTally := 0;
  FNidoparasiteEggsTally := 0;
  FNidoparasiteNestlingsTally := 0;
  FNidoparasiteId := 0;
  FHavePhilornisLarvae := False;
  FNestStage := nsgUnknown;
  FNotes := EmptyStr;
end;

procedure TNestRevision.Copy(aFrom: TNestRevision);
begin
  FFullName := aFrom.FullName;
  FNestId := aFrom.NestId;
  FRevisionDate := aFrom.RevisionDate;
  FRevisionTime := aFrom.RevisionTime;
  FObserver1Id := aFrom.Observer1Id;
  FObserver2Id := aFrom.Observer2Id;
  FNestStatus := aFrom.NestStatus;
  FHostEggsTally := aFrom.HostEggsTally;
  FHostNestlingsTally := aFrom.HostNestlingsTally;
  FNidoparasiteEggsTally := aFrom.NidoparasiteEggsTally;
  FNidoparasiteNestlingsTally := aFrom.NidoparasiteNestlingsTally;
  FNidoparasiteId := aFrom.NidoparasiteId;
  FHavePhilornisLarvae := aFrom.HavePhilornisLarvae;
  FNestStage := aFrom.NestStage;
  FNotes := aFrom.Notes;
end;

procedure TNestRevision.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNestRevision.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM nest_revisions');
      Add('WHERE (nest_revision_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevision.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'nest_revision_id, ' +
      'nest_id, ' +
      'full_name, ' +
      'revision_date, ' +
      'revision_time, ' +
      'observer_1_id, ' +
      'observer_2_id, ' +
      'nest_status, ' +
      'host_eggs_tally, ' +
      'host_nestlings_tally, ' +
      'nidoparasite_eggs_tally, ' +
      'nidoparasite_nestlings_tally, ' +
      'nidoparasite_id, ' +
      'have_philornis_larvae, ' +
      'nest_stage, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM nest_revisions');
    Add('WHERE nest_revision_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevision.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('nest_revision_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FNestId := FieldByName('nest_id').AsInteger;
    FRevisionDate := FieldByName('revision_date').AsDateTime;
    FRevisionTime := FieldByName('revision_time').AsDateTime;
    FObserver1Id := FieldByName('observer_1_id').AsInteger;
    FObserver2Id := FieldByName('observer_2_id').AsInteger;
    case FieldByName('nest_status').AsString of
      'I': FNestStatus := nstInactive;
      'A': FNestStatus := nstActive;
    else
      FNestStatus := nstUnknown;
    end;
    FHostEggsTally := FieldByName('host_eggs_tally').AsInteger;
    FHostNestlingsTally := FieldByName('host_nestlings_tally').AsInteger;
    FNidoparasiteEggsTally := FieldByName('nidoparasite_eggs_tally').AsInteger;
    FNidoparasiteNestlingsTally := FieldByName('nidoparasite_nestlings_tally').AsInteger;
    FNidoparasiteId := FieldByName('nidoparasite_id').AsInteger;
    FHavePhilornisLarvae := FieldByName('have_philornis_larvae').AsBoolean;
    case FieldByName('nest_stage').AsString of
      'X': FNestStage := nsgInactive;
      'C': FNestStage := nsgConstruction;
      'L': FNestStage := nsgLaying;
      'I': FNestStage := nsgIncubation;
      'H': FNestStage := nsgHatching;
      'N': FNestStage := nsgNestling;
    else
      FNestStage := nsgUnknown;
    end;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNestRevision.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO nest_revisions (' +
        'nest_id, ' +
        'full_name, ' +
        'revision_date, ' +
        'revision_time, ' +
        'observer_1_id, ' +
        'observer_2_id, ' +
        'nest_status, ' +
        'host_eggs_tally, ' +
        'host_nestlings_tally, ' +
        'nidoparasite_eggs_tally, ' +
        'nidoparasite_nestlings_tally, ' +
        'nidoparasite_id, ' +
        'have_philornis_larvae, ' +
        'nest_stage, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':nest_id, ' +
        ':full_name, ' +
        'date(:revision_date), ' +
        'time(:revision_time), ' +
        ':observer_1_id, ' +
        ':observer_2_id, ' +
        ':nest_status, ' +
        ':host_eggs_tally, ' +
        ':host_nestlings_tally, ' +
        ':nidoparasite_eggs_tally, ' +
        ':nidoparasite_nestlings_tally, ' +
        ':nidoparasite_id, ' +
        ':have_philornis_larvae, ' +
        ':nest_stage, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('full_name').AsString := FFullname;
      ParamByName('revision_date').AsString := FormatDateTime('yyyy-mm-dd', FRevisionDate);
      ParamByName('revision_time').AsString := FormatDateTime('hh:nn', FRevisionTime);
      ParamByName('observer_1_id').AsInteger := FObserver1Id;
      ParamByName('observer_2_id').AsInteger := FObserver2Id;
      ParamByName('nest_status').AsString := NestStates[FNestStatus];
      ParamByName('host_eggs_tally').AsInteger := FHostEggsTally;
      ParamByName('host_nestlings_tally').AsInteger := FHostNestlingsTally;
      ParamByName('nidoparasite_eggs_tally').AsInteger := FNidoparasiteEggsTally;
      ParamByName('nidoparasite_nestlings_tally').AsInteger := FNidoparasiteNestlingsTally;
      ParamByName('nidoparasite_id').AsInteger := FNidoparasiteId;
      ParamByName('have_philornis_larvae').AsBoolean := FHavePhilornisLarvae;
      ParamByName('nest_stage').AsString := NestStages[FNestStage];
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevision.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TNestRevision.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Full name', FFullName);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Date', FRevisionDate);
    JSONObject.Add('Time', FRevisionTime);
    JSONObject.Add('Observer 1', FObserver1Id);
    JSONObject.Add('Observer 2', FObserver2Id);
    JSONObject.Add('Nest status', NestStates[FNestStatus]);
    JSONObject.Add('Host eggs', FHostEggsTally);
    JSONObject.Add('Host nestlings', FHostNestlingsTally);
    JSONObject.Add('Nidoparasite eggs', FNidoparasiteEggsTally);
    JSONObject.Add('Nidoparasite nestlings', FNidoparasiteNestlingsTally);
    JSONObject.Add('Nidoparasite', FNidoparasiteId);
    JSONObject.Add('Have Philornis larvae', FHavePhilornisLarvae);
    JSONObject.Add('Nest stage', NestStages[FNestStage]);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TNestRevision.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNestRevision.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE nest_revisions SET');
      Add('  nest_id = :nest_id,');
      Add('  full_name = :full_name,');
      Add('  revision_date = date(:revision_date),');
      Add('  revision_time = time(:revision_time),');
      Add('  observer_1_id = :observer_1_id,');
      Add('  observer_2_id = :observer_2_id,');
      Add('  nest_status = :nest_status,');
      Add('  host_eggs_tally = :host_eggs_tally,');
      Add('  host_nestlings_tally = :host_nestlings_tally,');
      Add('  nidoparasite_eggs_tally = :nidoparasite_eggs_tally,');
      Add('  nidoparasite_nestlings_tally = :nidoparasite_nestlings_tally,');
      Add('  nidoparasite_id = :nidoparasite_id,');
      Add('  have_philornis_larvae = :have_philornis_larvae,');
      Add('  nest_stage = :nest_stage,');
      Add('  notes = :notes,');
      Add('  user_updated = :user_updated,');
      Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (nest_revision_id = :nest_revision_id);');
      ParamByName('nest_revision_id').AsInteger := FId;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('full_name').AsString := FFullname;
      ParamByName('revision_date').AsString := FormatDateTime('yyyy-mm-dd', FRevisionDate);
      ParamByName('revision_time').AsString := FormatDateTime('hh:nn', FRevisionTime);
      ParamByName('observer_1_id').AsInteger := FObserver1Id;
      ParamByName('observer_2_id').AsInteger := FObserver2Id;
      ParamByName('nest_status').AsString := NestStates[FNestStatus];
      ParamByName('host_eggs_tally').AsInteger := FHostEggsTally;
      ParamByName('host_nestlings_tally').AsInteger := FHostNestlingsTally;
      ParamByName('nidoparasite_eggs_tally').AsInteger := FNidoparasiteEggsTally;
      ParamByName('nidoparasite_nestlings_tally').AsInteger := FNidoparasiteNestlingsTally;
      ParamByName('nidoparasite_id').AsInteger := FNidoparasiteId;
      ParamByName('have_philornis_larvae').AsBoolean := FHavePhilornisLarvae;
      ParamByName('nest_stage').AsString := NestStages[FNestStage];
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestRevision.Diff(aOld: TNestRevision; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.RevisionDate, FRevisionDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.RevisionTime, FRevisionTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserver1ID, aOld.Observer1Id, FObserver1Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserver2ID, aOld.Observer2Id, FObserver2Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStatus, aOld.NestStatus, FNestStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggsHost, aOld.HostEggsTally, FHostEggsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestlingsHost, aOld.HostNestlingsTally, FHostNestlingsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggsNidoparasite, aOld.NidoparasiteEggsTally, FNidoparasiteEggsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestlingsNidoparasite, aOld.NidoparasiteNestlingsTally, FNidoparasiteNestlingsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNidoparasiteID, aOld.NidoparasiteId, FNidoparasiteId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHasPhilornisLarvae, aOld.HavePhilornisLarvae, FHavePhilornisLarvae, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestStage, aOld.NestStage, FNestStage, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TNestRevision.Find(aNest: Integer; aDate, aTime: String; aObserver: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT nest_revision_id FROM nest_revisions');
    Add('WHERE (nest_id = :anest)');
    Add('AND (date(sample_date) = date(:adate))');
    Add('AND (time(sample_time) = time(:atime))');
    Add('AND (observer_1_id = :aobserver)');
    ParamByName('ANEST').AsInteger := aNest;
    ParamByName('AOBSERVER').AsInteger := aObserver;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('ATIME').AsString := aTime;

    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('nest_revision_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TEgg }

constructor TEgg.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TEgg.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FEggSeq := 0;
  FFieldNumber := EmptyStr;
  FNestId := 0;
  FEggShape := esUnknown;
  FWidth := 0.0;
  FLength := 0.0;
  FMass := 0.0;
  FVolume := 0.0;
  FEggStage := EmptyStr;
  FEggshellColor := EmptyStr;
  FEggshellPattern := espUnknown;
  FEggshellTexture := estUnknown;
  FEggHatched := False;
  FIndividualId := 0;
  FResearcherId := 0;
  FMeasureDate := NullDate;
  FTaxonId := 0;
  FHostEgg := True;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TEgg.Copy(aFrom: TEgg);
begin
  FFullName := aFrom.FullName;
  FEggSeq := aFrom.EggSeq;
  FFieldNumber := aFrom.FieldNumber;
  FNestId := aFrom.NestId;
  FEggShape := aFrom.EggShape;
  FWidth := aFrom.Width;
  FLength := aFrom.Length;
  FMass := aFrom.Mass;
  FVolume := aFrom.Volume;
  FEggStage := aFrom.EggStage;
  FEggshellColor := aFrom.EggshellColor;
  FEggshellPattern := aFrom.EggshellPattern;
  FEggshellTexture := aFrom.EggshellTexture;
  FEggHatched := aFrom.EggHatched;
  FIndividualId := aFrom.IndividualId;
  FResearcherId := aFrom.ResearcherId;
  FMeasureDate := aFrom.MeasureDate;
  FTaxonId := aFrom.TaxonId;
  FHostEgg := aFrom.HostEgg;
  FDescription := aFrom.Description;
  FNotes := aFrom.Notes;
end;

procedure TEgg.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TEgg.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM eggs');
      Add('WHERE (egg_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEgg.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'egg_id, ' +
      'nest_id, ' +
      'egg_seq, ' +
      'field_number, ' +
      'taxon_id, ' +
      'eggshell_color, ' +
      'eggshell_pattern, ' +
      'eggshell_texture, ' +
      'egg_shape, ' +
      'egg_width, ' +
      'egg_length, ' +
      'egg_mass, ' +
      'egg_volume, ' +
      'egg_stage, ' +
      'egg_hatched, ' +
      'measure_date, ' +
      'researcher_id, ' +
      'individual_id, ' +
      'host_egg, ' +
      'description, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM eggs');
    Add('WHERE egg_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEgg.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('egg_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FFieldNumber := FieldByName('field_number').AsString;
    FEggSeq := FieldByName('egg_seq').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    case FieldByName('egg_shape').AsString of
      'S': FEggShape := esSpherical;
      'E': FEggShape := esElliptical;
      'O': FEggShape := esOval;
      'P': FEggShape := esPiriform;
      'C': FEggShape := esConical;
      'B': FEggShape := esBiconical;
      'Y': FEggShape := esCylindrical;
      'L': FEggShape := esLongitudinal;
    else
      FEggShape := esUnknown;
    end;
    FWidth := FieldByName('egg_width').AsFloat;
    FLength := FieldByName('egg_length').AsFloat;
    FMass := FieldByName('egg_mass').AsFloat;
    FVolume := FieldByName('egg_volume').AsFloat;
    FEggStage := FieldByName('egg_stage').AsString;
    FEggshellColor := FieldByName('eggshell_color').AsString;
    case FieldByName('eggshell_pattern').AsString of
      'P':  FEggshellPattern := espSpots;
      'B':  FEggshellPattern := espBlotches;
      'S':  FEggshellPattern := espSquiggles;
      'T':  FEggshellPattern := espStreaks;
      'W':  FEggshellPattern := espScrawls;
      'PS': FEggshellPattern := espSpotsSquiggles;
      'BS': FEggshellPattern := espBlotchesSquiggles;
    else
      FEggshellPattern := espUnknown;
    end;
    case FieldByName('eggshell_texture').AsString of
      'C': FEggshellTexture := estChalky;
      'S': FEggshellTexture := estShiny;
      'G': FEggshellTexture := estGlossy;
      'P': FEggshellTexture := estPitted;
    else
      FEggshellTexture := estUnknown;
    end;
    FEggHatched := FieldByName('egg_hatched').AsBoolean;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FResearcherId := FieldByName('researcher_id').AsInteger;
    FMeasureDate := FieldByName('measure_date').AsDateTime;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FHostEgg := FieldByName('host_egg').AsBoolean;
    FDescription := FieldByName('description').AsString;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TEgg.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO eggs (' +
        'field_number, ' +
        'nest_id, ' +
        'egg_seq, ' +
        'egg_shape, ' +
        'egg_width, ' +
        'egg_length, ' +
        'egg_mass, ' +
        'egg_volume, ' +
        'egg_stage, ' +
        'eggshell_color, ' +
        'eggshell_pattern, ' +
        'eggshell_texture, ' +
        'egg_hatched, ' +
        'researcher_id, ' +
        'individual_id, ' +
        'measure_date, ' +
        'taxon_id, ' +
        'host_egg, ' +
        'description, ' +
        'notes, ' +
        'full_name, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':field_number, ' +
        ':nest_id, ' +
        ':egg_seq, ' +
        ':egg_shape, ' +
        ':egg_width, ' +
        ':egg_length, ' +
        ':egg_mass, ' +
        ':egg_volume, ' +
        ':egg_stage, ' +
        ':eggshell_color, ' +
        ':eggshell_pattern, ' +
        ':eggshell_texture, ' +
        ':egg_hatched, ' +
        ':researcher_id, ' +
        ':individual_id, ' +
        'date(:measure_date), ' +
        ':taxon_id, ' +
        ':host_egg, ' +
        ':description, ' +
        ':notes, ' +
        ':full_name, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''));');
      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('egg_seq').AsInteger := FEggSeq;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('egg_shape').AsString := EggShapes[FEggShape];
      ParamByName('egg_width').AsFloat := FWidth;
      ParamByName('egg_length').AsFloat := FLength;
      ParamByName('egg_mass').AsFloat := FMass;
      ParamByName('egg_volume').AsFloat := FVolume;
      ParamByName('egg_stage').AsString := FEggStage;
      ParamByName('eggshell_color').AsString := FEggshellColor;
      ParamByName('eggshell_pattern').AsString := EggshellPatterns[FEggshellPattern];
      ParamByName('eggshell_texture').AsString := EggshellTextures[FEggshellTexture];
      ParamByName('egg_hatched').AsBoolean := FEggHatched;
      ParamByName('researcher_id').AsInteger := FResearcherId;
      ParamByName('individual_id').AsInteger := FIndividualId;
      ParamByName('measure_date').AsString := FormatDateTime('yyyy-mm-dd', FMeasureDate);
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('host_egg').AsBoolean := FHostEgg;
      ParamByName('description').AsString := FDescription;
      ParamByName('notes').AsString := FNotes;
      ParamByName('full_name').AsString := FFullname;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //// Get the taxon hierarchy
      //if (FTaxonId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
      //  Add('WHERE taxon_id = :ataxon');
      //  ParamByName('ataxon').AsInteger := FTaxonId;
      //  Open;
      //  FOrderId := FieldByName('order_id').AsInteger;
      //  FFamilyId := FieldByName('family_id').AsInteger;
      //  FGenusId := FieldByName('genus_id').AsInteger;
      //  FSpeciesId := FieldByName('species_id').AsInteger;
      //  Close;
      //end;
      //// Save the taxon hierarchy
      //Clear;
      //Add('UPDATE eggs SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE egg_id = :aid');
      //ParamByName('order_id').AsInteger := FOrderId;
      //if (FFamilyId > 0) then
      //  ParamByName('family_id').AsInteger := FFamilyId
      //else
      //  ParamByName('family_id').Clear;
      //if (FGenusId > 0) then
      //  ParamByName('genus_id').AsInteger := FGenusId
      //else
      //  ParamByName('genus_id').Clear;
      //if (FSpeciesId > 0) then
      //  ParamByName('species_id').AsInteger := FSpeciesId
      //else
      //  ParamByName('species_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEgg.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TEgg.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Field number', FFieldNumber);
    JSONObject.Add('Full name', FFullName);
    JSONObject.Add('Egg number', FEggSeq);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Egg shape', EggShapes[FEggShape]);
    JSONObject.Add('Width', FWidth);
    JSONObject.Add('Length', FLength);
    JSONObject.Add('Mass', FMass);
    JSONObject.Add('Volume', FVolume);
    JSONObject.Add('Stage', FEggStage);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Color', FEggshellColor);
    JSONObject.Add('Pattern', EggshellPatterns[FEggshellPattern]);
    JSONObject.Add('Texture', EggshellTextures[FEggshellTexture]);
    JSONObject.Add('Hatched', FEggHatched);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Researcher', FResearcherId);
    JSONObject.Add('Measure date', FMeasureDate);
    JSONObject.Add('Host egg', FHostEgg);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TEgg.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TEgg.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE eggs SET');
      Add('  field_number = :field_number,');
      Add('  egg_seq = :egg_seq,');
      Add('  nest_id = :nest_id,');
      Add('  egg_shape = :egg_shape,');
      Add('  egg_width = :egg_width,');
      Add('  egg_length = :egg_length,');
      Add('  egg_mass = :egg_mass,');
      Add('  egg_volume = :egg_volume,');
      Add('  egg_stage = :egg_stage,');
      Add('  eggshell_color = :eggshell_color,');
      Add('  eggshell_pattern = :eggshell_pattern,');
      Add('  eggshell_texture = :eggshell_texture,');
      Add('  egg_hatched = :egg_hatched,');
      Add('  researcher_id = :researcher_id,');
      Add('  individual_id = :individual_id,');
      Add('  measure_date = date(:measute_date),');
      Add('  taxon_id = :taxon_id,');
      Add('  host_egg = :host_egg,');
      Add('  description = :description,');
      Add('  notes = :notes,');
      Add('  full_name = :full_name,');
      Add('  user_updated = :user_updated,');
      Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (egg_id = :egg_id);');
      ParamByName('egg_id').AsInteger := FId;
      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('egg_seq').AsInteger := FEggSeq;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('egg_shape').AsString := EggShapes[FEggShape];
      ParamByName('egg_width').AsFloat := FWidth;
      ParamByName('egg_length').AsFloat := FLength;
      ParamByName('egg_mass').AsFloat := FMass;
      ParamByName('egg_volume').AsFloat := FVolume;
      ParamByName('egg_stage').AsString := FEggStage;
      ParamByName('eggshell_color').AsString := FEggshellColor;
      ParamByName('eggshell_pattern').AsString := EggshellPatterns[FEggshellPattern];
      ParamByName('eggshell_texture').AsString := EggshellTextures[FEggshellTexture];
      ParamByName('egg_hatched').AsBoolean := FEggHatched;
      ParamByName('researcher_id').AsInteger := FResearcherId;
      ParamByName('individual_id').AsInteger := FIndividualId;
      ParamByName('measure_date').AsString := FormatDateTime('yyyy-mm-dd', FMeasureDate);
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('host_egg').AsBoolean := FHostEgg;
      ParamByName('description').AsString := FDescription;
      ParamByName('notes').AsString := FNotes;
      ParamByName('full_name').AsString := FFullname;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;

      ExecSQL;

      //// Get the taxon hierarchy
      //if (FTaxonId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT order_id, family_id, genus_id, species_id FROM zoo_taxa');
      //  Add('WHERE taxon_id = :ataxon');
      //  ParamByName('ataxon').AsInteger := FTaxonId;
      //  Open;
      //  FOrderId := FieldByName('order_id').AsInteger;
      //  FFamilyId := FieldByName('family_id').AsInteger;
      //  FGenusId := FieldByName('genus_id').AsInteger;
      //  FSpeciesId := FieldByName('species_id').AsInteger;
      //  Close;
      //end;
      //// Save the taxon hierarchy
      //Clear;
      //Add('UPDATE eggs SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE egg_id = :aid');
      //ParamByName('order_id').AsInteger := FOrderId;
      //if (FFamilyId > 0) then
      //  ParamByName('family_id').AsInteger := FFamilyId
      //else
      //  ParamByName('family_id').Clear;
      //if (FGenusId > 0) then
      //  ParamByName('genus_id').AsInteger := FGenusId
      //else
      //  ParamByName('genus_id').Clear;
      //if (FSpeciesId > 0) then
      //  ParamByName('species_id').AsInteger := FSpeciesId
      //else
      //  ParamByName('species_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TEgg.Diff(aOld: TEgg; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggNumber, aOld.EggSeq, FEggSeq, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggShape, aOld.EggShape, FEggShape, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWidth, aOld.Width, FWidth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLength, aOld.Length, FLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMass, aOld.Mass, FMass, R) then
    aList.Add(R);
  if FieldValuesDiff(rscVolume, aOld.Volume, FVolume, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStage, aOld.EggStage, FEggStage, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggshellColor, aOld.EggshellColor, FEggshellColor, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggshellPattern, aOld.EggshellPattern, FEggshellPattern, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggshellTexture, aOld.EggshellTexture, FEggshellTexture, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHatched, aOld.EggHatched, FEggHatched, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscResearcherID, aOld.ResearcherId, FResearcherId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.MeasureDate, FMeasureDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHostEgg, aOld.HostEgg, FHostEgg, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TEgg.Find(aNest: Integer; aFieldNumber, aDate: String; aObserver: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT egg_id FROM eggs');
    Add('WHERE (nest_id = :anest)');
    Add('AND (date(measure_date) = date(:adate))');
    Add('AND (field_number = :afieldnumber)');
    Add('AND (researcher_id = :aobserver)');
    ParamByName('ANEST').AsInteger := aNest;
    ParamByName('AOBSERVER').AsInteger := aObserver;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;

    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('egg_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TNest }

constructor TNest.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TNest.Clear;
begin
  inherited Clear;
  FFieldNumber := EmptyStr;
  FFullName := EmptyStr;
  FObserverId := 0;
  FLocalityId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FTaxonId := 0;
  FNestShape := EmptyStr;
  FSupportType := EmptyStr;
  FSupportPlant1Id := 0;
  FSupportPlant2Id := 0;
  FOtherSupport := EmptyStr;
  FHeightAboveGround := 0.0;
  FProjectId := 0;
  FInternalMaxDiameter := 0.0;
  FInternalMinDiameter := 0.0;
  FExternalMaxDiameter := 0.0;
  FExternalMinDiameter := 0.0;
  FInternalHeight := 0.0;
  FExternalHeight := 0.0;
  FEdgeDistance := 0.0;
  FCenterDistance := 0.0;
  FNestCover := 0;
  FPlantMaxDiameter := 0.0;
  FPlantMinDiameter := 0.0;
  FPlantHeight := 0.0;
  FPlantDbh := 0.0;
  FConstructionDays := 0.0;
  FIncubationDays := 0.0;
  FNestlingDays := 0.0;
  FActiveDays := 0.0;
  FNestFate := nfUnknown;
  FNestProductivity := 0;
  FFoundDate := NullDate;
  FLastDate := NullDate;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TNest.Copy(aFrom: TNest);
begin
  FFieldNumber := aFrom.FieldNumber;
  FFullName := aFrom.FullName;
  FObserverId := aFrom.ObserverId;
  FLocalityId := aFrom.LocalityId;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FTaxonId := aFrom.TaxonId;
  FNestShape := aFrom.NestShape;
  FSupportType := aFrom.SupportType;
  FSupportPlant1Id := aFrom.SupportPlant1Id;
  FSupportPlant2Id := aFrom.SupportPlant2Id;
  FOtherSupport := aFrom.OtherSupport;
  FHeightAboveGround := aFrom.HeightAboveGround;
  FProjectId := aFrom.ProjectId;
  FInternalMaxDiameter := aFrom.InternalMaxDiameter;
  FInternalMinDiameter := aFrom.InternalMinDiameter;
  FExternalMaxDiameter := aFrom.ExternalMaxDiameter;
  FExternalMinDiameter := aFrom.ExternalMinDiameter;
  FInternalHeight := aFrom.InternalHeight;
  FExternalHeight := aFrom.ExternalHeight;
  FEdgeDistance := aFrom.EdgeDistance;
  FCenterDistance := aFrom.CenterDistance;
  FNestCover := aFrom.NestCover;
  FPlantMaxDiameter := aFrom.PlantMaxDiameter;
  FPlantMinDiameter := aFrom.PlantMinDiameter;
  FPlantHeight := aFrom.PlantHeight;
  FPlantDbh := aFrom.PlantDbh;
  FConstructionDays := aFrom.ConstructionDays;
  FIncubationDays := aFrom.IncubationDays;
  FNestlingDays := aFrom.NestlingDays;
  FActiveDays := aFrom.ActiveDays;
  FNestFate := aFrom.NestFate;
  FNestProductivity := aFrom.NestProductivity;
  FFoundDate := aFrom.FoundDate;
  FLastDate := aFrom.LastDate;
  FDescription := aFrom.Description;
  FNotes := aFrom.Notes;
end;

procedure TNest.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNest.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM nests');
      Add('WHERE (nest_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNest.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'nest_id, ' +
      'field_number, ' +
      'observer_id, ' +
      'project_id, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'taxon_id, ' +
      'nest_shape, ' +
      'support_type, ' +
      'support_plant_1_id, ' +
      'support_plant_2_id, ' +
      'other_support, ' +
      'height_above_ground, ' +
      'internal_max_diameter, ' +
      'internal_min_diameter, ' +
      'external_max_diameter, ' +
      'external_min_diameter, ' +
      'internal_height, ' +
      'external_height, ' +
      'edge_distance, ' +
      'center_distance, ' +
      'nest_cover, ' +
      'plant_max_diameter, ' +
      'plant_min_diameter, ' +
      'plant_height, ' +
      'plant_dbh, ' +
      'construction_days, ' +
      'incubation_days, ' +
      'nestling_days, ' +
      'active_days, ' +
      'nest_fate, ' +
      'nest_productivity, ' +
      'found_date, ' +
      'last_date, ' +
      'full_name, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM nests');
    Add('WHERE nest_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNest.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('nest_id').AsInteger;
    FFieldNumber := FieldByName('field_number').AsString;
    FFullName := FieldByName('full_name').AsString;
    FObserverId := FieldByName('observer_id').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FNestShape := FieldByName('nest_shape').AsString;
    FSupportType := FieldByName('support_type').AsString;
    FSupportPlant1Id := FieldByName('support_plant_1_id').AsInteger;
    FSupportPlant2Id := FieldByName('support_plant_2_id').AsInteger;
    FOtherSupport := FieldByName('other_support').AsString;
    FHeightAboveGround := FieldByName('height_above_ground').AsFloat;
    FProjectId := FieldByName('project_id').AsInteger;
    FInternalMaxDiameter := FieldByName('internal_max_diameter').AsFloat;
    FInternalMinDiameter := FieldByName('internal_min_diameter').AsFloat;
    FExternalMaxDiameter := FieldByName('external_max_diameter').AsFloat;
    FExternalMinDiameter := FieldByName('external_min_diameter').AsFloat;
    FInternalHeight := FieldByName('internal_height').AsFloat;
    FExternalHeight := FieldByName('external_height').AsFloat;
    FEdgeDistance := FieldByName('edge_distance').AsFloat;
    FCenterDistance := FieldByName('center_distance').AsFloat;
    FNestCover := FieldByName('nest_cover').AsInteger;
    FPlantMaxDiameter := FieldByName('plant_max_diameter').AsFloat;
    FPlantMinDiameter := FieldByName('plant_min_diameter').AsFloat;
    FPlantHeight := FieldByName('plant_height').AsFloat;
    FPlantDbh := FieldByName('plant_dbh').AsFloat;
    FConstructionDays := FieldByName('construction_days').AsFloat;
    FIncubationDays := FieldByName('incubation_days').AsFloat;
    FNestlingDays := FieldByName('nestling_days').AsFloat;
    FActiveDays := FieldByName('active_days').AsFloat;
    case FieldByName('nest_fate').AsString of
      'L': FNestFate := nfLoss;
      'S': FNestFate := nfSuccess;
    else
      FNestFate := nfUnknown;
    end;
    FNestProductivity := FieldByName('nest_productivity').AsInteger;
    FFoundDate := FieldByName('found_date').AsDateTime;
    FLastDate := FieldByName('last_date').AsDateTime;
    FDescription := FieldByName('description').AsString;
    FNotes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNest.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO nests (' +
        'field_number, ' +
        'observer_id, ' +
        'project_id, ' +
        'locality_id, ' +
        'longitude, ' +
        'latitude, ' +
        'taxon_id, ' +
        'nest_shape, ' +
        'support_type, ' +
        'support_plant_1_id, ' +
        'support_plant_2_id, ' +
        'other_support, ' +
        'height_above_ground, ' +
        'internal_max_diameter, ' +
        'internal_min_diameter, ' +
        'external_max_diameter, ' +
        'external_min_diameter, ' +
        'internal_height, ' +
        'external_height, ' +
        'edge_distance, ' +
        'center_distance, ' +
        'nest_cover, ' +
        'plant_max_diameter, ' +
        'plant_min_diameter, ' +
        'plant_height, ' +
        'plant_dbh, ' +
        'nest_fate,' +
        'nest_productivity, ' +
        'found_date, ' +
        'last_date, ' +
        'full_name, ' +
        'description, ' +
        'notes, ' +
        'construction_days, ' +
        'incubation_days, ' +
        'nestling_days, ' +
        'active_days, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':field_number, ' +
        ':observer_id, ' +
        ':project_id, ' +
        ':locality_id, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':taxon_id, ' +
        ':nest_shape, ' +
        ':support_type, ' +
        ':support_plant_1_id, ' +
        ':support_plant_2_id, ' +
        ':other_support, ' +
        ':height_above_ground, ' +
        ':internal_max_diameter, ' +
        ':internal_min_diameter, ' +
        ':external_max_diameter, ' +
        ':external_min_diameter, ' +
        ':internal_height, ' +
        ':external_height, ' +
        ':edge_distance, ' +
        ':center_distance, ' +
        ':nest_cover, ' +
        ':plant_max_diameter, ' +
        ':plant_min_diameter, ' +
        ':plant_height, ' +
        ':plant_dbh, ' +
        ':nest_fate,' +
        ':nest_productivity, ' +
        'date(:found_date), ' +
        'date(:last_date), ' +
        ':full_name, ' +
        ':description, ' +
        ':notes, ' +
        ':construction_days, ' +
        ':incubation_days, ' +
        ':nestling_days, ' +
        ':active_days, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''));');
      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('observer_id').AsInteger := FObserverId;
      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('longitude').AsFloat := FLongitude;
      ParamByName('latitude').AsFloat := FLatitude;
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('nest_shape').AsString := FNestShape;
      ParamByName('support_type').AsString := FSupportType;
      ParamByName('support_plant_1_id').AsInteger := FSupportPlant1Id;
      ParamByName('support_plant_2_id').AsInteger := FSupportPlant2Id;
      ParamByName('other_support').AsString := FOtherSupport;
      ParamByName('height_above_ground').AsFloat := FHeightAboveGround;
      ParamByName('internal_max_diameter').AsFloat := FInternalMaxDiameter;
      ParamByName('internal_min_diameter').AsFloat := FInternalMinDiameter;
      ParamByName('external_max_diameter').AsFloat := FExternalMaxDiameter;
      ParamByName('external_min_diameter').AsFloat := FExternalMinDiameter;
      ParamByName('internal_height').AsFloat := FInternalHeight;
      ParamByName('external_height').AsFloat := FExternalHeight;
      ParamByName('edge_distance').AsFloat := FEdgeDistance;
      ParamByName('center_distance').AsFloat := FCenterDistance;
      ParamByName('nest_cover').AsFloat := FNestCover;
      ParamByName('plant_max_diameter').AsFloat := FPlantMaxDiameter;
      ParamByName('plant_min_diameter').AsFloat := FPlantMinDiameter;
      ParamByName('plant_height').AsFloat := FPlantHeight;
      ParamByName('plant_dbh').AsFloat := FPlantDbh;
      ParamByName('nest_fate').AsString := NestFates[FNestFate];
      ParamByName('nest_productivity').AsInteger := FNestProductivity;
      ParamByName('found_date').AsString := FormatDateTime('yyyy-mm-dd', FFoundDate);
      ParamByName('last_date').AsString := FormatDateTime('yyyy-mm-dd', FLastDate);
      ParamByName('full_name').AsString := FFullName;
      ParamByName('description').AsString := FDescription;
      ParamByName('notes').AsString := FNotes;
      ParamByName('construction_days').AsFloat := FConstructionDays;
      ParamByName('incubation_days').AsFloat := FIncubationDays;
      ParamByName('nestling_days').AsFloat := FNestlingDays;
      ParamByName('active_days').AsFloat := FActiveDays;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //// Get the taxon hierarchy
      //if (FTaxonId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa');
      //  Add('WHERE taxon_id = :ataxon');
      //  ParamByName('ataxon').AsInteger := FTaxonId;
      //  Open;
      //  FOrderId := FieldByName('order_id').AsInteger;
      //  FFamilyId := FieldByName('family_id').AsInteger;
      //  FSubfamilyId := FieldByName('subfamily_id').AsInteger;
      //  FGenusId := FieldByName('genus_id').AsInteger;
      //  FSpeciesId := FieldByName('species_id').AsInteger;
      //  Close;
      //end;
      //// Save the taxon hierarchy
      //Clear;
      //Add('UPDATE nests SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  subfamily_id = :subfamily_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE nest_id = :aid');
      //ParamByName('order_id').AsInteger := FOrderId;
      //if (FFamilyId > 0) then
      //  ParamByName('family_id').AsInteger := FFamilyId
      //else
      //  ParamByName('family_id').Clear;
      //if (FSubfamilyId > 0) then
      //  ParamByName('subfamily_id').AsInteger := FSubfamilyId
      //else
      //  ParamByName('subfamily_id').Clear;
      //if (FGenusId > 0) then
      //  ParamByName('genus_id').AsInteger := FGenusId
      //else
      //  ParamByName('genus_id').Clear;
      //if (FSpeciesId > 0) then
      //  ParamByName('species_id').AsInteger := FSpeciesId
      //else
      //  ParamByName('species_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;
      //
      //// Get the site hierarchy
      //if (FLocalityId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
      //  Add('WHERE site_id = :asite');
      //  ParamByName('ASITE').AsInteger := FLocalityId;
      //  Open;
      //  FCountryId := FieldByName('country_id').AsInteger;
      //  FStateId := FieldByName('state_id').AsInteger;
      //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
      //  Close;
      //end;
      //// Save the site hierarchy
      //Clear;
      //Add('UPDATE nests SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE nest_id = :aid');
      //ParamByName('country_id').AsInteger := FCountryId;
      //if (FStateId > 0) then
      //  ParamByName('state_id').AsInteger := FStateId
      //else
      //  ParamByName('state_id').Clear;
      //if (FMunicipalityId > 0) then
      //  ParamByName('municipality_id').AsInteger := FMunicipalityId
      //else
      //  ParamByName('municipality_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNest.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TNest.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Field number', FFieldNumber);
    JSONObject.Add('Full name', FFullName);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Nest shape', FNestShape);
    JSONObject.Add('Support type', FSupportType);
    JSONObject.Add('Support plant 1', FSupportPlant1Id);
    JSONObject.Add('Support plant 2', FSupportPlant2Id);
    JSONObject.Add('Other support', FOtherSupport);
    JSONObject.Add('Height above ground', FHeightAboveGround);
    JSONObject.Add('Max internal diameter', FInternalMaxDiameter);
    JSONObject.Add('Min internal diameter', FInternalMinDiameter);
    JSONObject.Add('Max external diameter', FExternalMaxDiameter);
    JSONObject.Add('Min external diameter', FExternalMinDiameter);
    JSONObject.Add('Internal height', FInternalHeight);
    JSONObject.Add('External height', FExternalHeight);
    JSONObject.Add('Edge distance', FEdgeDistance);
    JSONObject.Add('Center distance', FCenterDistance);
    JSONObject.Add('Nest cover', FNestCover);
    JSONObject.Add('Max plant diameter', FPlantMaxDiameter);
    JSONObject.Add('Min plant diameter', FPlantMinDiameter);
    JSONObject.Add('Plant height', FPlantHeight);
    JSONObject.Add('Plant DBH', FPlantDbh);
    JSONObject.Add('Construction days', FConstructionDays);
    JSONObject.Add('Incubation days', FIncubationDays);
    JSONObject.Add('Nestling days', FNestlingDays);
    JSONObject.Add('Active days', FActiveDays);
    JSONObject.Add('Nest fate', NestFates[FNestFate]);
    JSONObject.Add('Nest productivity', FNestProductivity);
    JSONObject.Add('Found date', FFoundDate);
    JSONObject.Add('Last date active', FLastDate);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TNest.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNest.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE nests SET');
      Add('  field_number = :field_number,');
      Add('  observer_id = :observer_id,');
      Add('  project_id = :project_id,');
      Add('  locality_id = :locality_id,');
      Add('  longitude = :longitude,');
      Add('  latitude = :latitude,');
      Add('  taxon_id = :taxon_id,');
      Add('  nest_shape = :nest_shape,');
      Add('  support_type = :support_type,');
      Add('  support_plant_1_id = :support_plant_1_id,');
      Add('  support_plant_2_id = :support_plant_2_id,');
      Add('  other_support = :other_support,');
      Add('  height_above_ground = :height_above_ground,');
      Add('  internal_max_diameter = :internal_max_diameter,');
      Add('  internal_min_diameter = :internal_min_diameter,');
      Add('  external_max_diameter = :external_max_diameter,');
      Add('  external_min_diameter = :external_min_diameter,');
      Add('  internal_height = :internal_height,');
      Add('  external_height = :external_height,');
      Add('  edge_distance = :edge_distance,');
      Add('  center_distance = :center_distance,');
      Add('  nest_cover = :nest_cover,');
      Add('  plant_max_diameter = :plant_max_diameter,');
      Add('  plant_min_diameter = :plant_min_diameter,');
      Add('  plant_height = :plant_height,');
      Add('  plant_dbh = :plant_dbh,');
      Add('  nest_fate = :nest_fate,');
      Add('  nest_productivity = :nest_productivity,');
      Add('  found_date = date(:found_date),');
      Add('  last_date = date(:last_date),');
      Add('  full_name = :full_name,');
      Add('  description = :description,');
      Add('  notes = :notes,');
      Add('  construction_days = :construction_days,');
      Add('  incubation_days = :incubation_days,');
      Add('  nestling_days = :nestling_days,');
      Add('  active_days = :active_days,');
      Add('  user_updated = :user_updated,');
      Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (nest_id = :nest_id);');
      ParamByName('nest_id').AsInteger := FId;
      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('observer_id').AsInteger := FObserverId;
      ParamByName('project_id').AsInteger := FProjectId;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('longitude').AsFloat := FLongitude;
      ParamByName('latitude').AsFloat := FLatitude;
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('nest_shape').AsString := FNestShape;
      ParamByName('support_type').AsString := FSupportType;
      ParamByName('support_plant_1_id').AsInteger := FSupportPlant1Id;
      ParamByName('support_plant_2_id').AsInteger := FSupportPlant2Id;
      ParamByName('other_support').AsString := FOtherSupport;
      ParamByName('height_above_ground').AsFloat := FHeightAboveGround;
      ParamByName('internal_max_diameter').AsFloat := FInternalMaxDiameter;
      ParamByName('internal_min_diameter').AsFloat := FInternalMinDiameter;
      ParamByName('external_max_diameter').AsFloat := FExternalMaxDiameter;
      ParamByName('external_min_diameter').AsFloat := FExternalMinDiameter;
      ParamByName('internal_height').AsFloat := FInternalHeight;
      ParamByName('external_height').AsFloat := FExternalHeight;
      ParamByName('edge_distance').AsFloat := FEdgeDistance;
      ParamByName('center_distance').AsFloat := FCenterDistance;
      ParamByName('nest_cover').AsFloat := FNestCover;
      ParamByName('plant_max_diameter').AsFloat := FPlantMaxDiameter;
      ParamByName('plant_min_diameter').AsFloat := FPlantMinDiameter;
      ParamByName('plant_height').AsFloat := FPlantHeight;
      ParamByName('plant_dbh').AsFloat := FPlantDbh;
      ParamByName('nest_fate').AsString := NestFates[FNestFate];
      ParamByName('nest_productivity').AsInteger := FNestProductivity;
      ParamByName('found_date').AsString := FormatDateTime('yyyy-mm-dd', FFoundDate);
      ParamByName('last_date').AsString := FormatDateTime('yyyy-mm-dd', FLastDate);
      ParamByName('full_name').AsString := FFullName;
      ParamByName('description').AsString := FDescription;
      ParamByName('notes').AsString := FNotes;
      ParamByName('construction_days').AsFloat := FConstructionDays;
      ParamByName('incubation_days').AsFloat := FIncubationDays;
      ParamByName('nestling_days').AsFloat := FNestlingDays;
      ParamByName('active_days').AsFloat := FActiveDays;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;

      ExecSQL;

      //// Get the taxon hierarchy
      //if (FTaxonId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa');
      //  Add('WHERE taxon_id = :ataxon');
      //  ParamByName('ataxon').AsInteger := FTaxonId;
      //  Open;
      //  FOrderId := FieldByName('order_id').AsInteger;
      //  FFamilyId := FieldByName('family_id').AsInteger;
      //  FSubfamilyId := FieldByName('subfamily_id').AsInteger;
      //  FGenusId := FieldByName('genus_id').AsInteger;
      //  FSpeciesId := FieldByName('species_id').AsInteger;
      //  Close;
      //end;
      //// Save the taxon hierarchy
      //Clear;
      //Add('UPDATE nests SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  subfamily_id = :subfamily_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE nest_id = :aid');
      //ParamByName('order_id').AsInteger := FOrderId;
      //if (FFamilyId > 0) then
      //  ParamByName('family_id').AsInteger := FFamilyId
      //else
      //  ParamByName('family_id').Clear;
      //if (FSubfamilyId > 0) then
      //  ParamByName('subfamily_id').AsInteger := FSubfamilyId
      //else
      //  ParamByName('subfamily_id').Clear;
      //if (FGenusId > 0) then
      //  ParamByName('genus_id').AsInteger := FGenusId
      //else
      //  ParamByName('genus_id').Clear;
      //if (FSpeciesId > 0) then
      //  ParamByName('species_id').AsInteger := FSpeciesId
      //else
      //  ParamByName('species_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;
      //
      //// Get the site hierarchy
      //if (FLocalityId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
      //  Add('WHERE site_id = :asite');
      //  ParamByName('ASITE').AsInteger := FLocalityId;
      //  Open;
      //  FCountryId := FieldByName('country_id').AsInteger;
      //  FStateId := FieldByName('state_id').AsInteger;
      //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
      //  Close;
      //end;
      //// Save the site hierarchy
      //Clear;
      //Add('UPDATE nests SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE nest_id = :aid');
      //ParamByName('country_id').AsInteger := FCountryId;
      //if (FStateId > 0) then
      //  ParamByName('state_id').AsInteger := FStateId
      //else
      //  ParamByName('state_id').Clear;
      //if (FMunicipalityId > 0) then
      //  ParamByName('municipality_id').AsInteger := FMunicipalityId
      //else
      //  ParamByName('municipality_id').Clear;
      //ParamByName('aid').AsInteger := FId;
      //ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNest.Diff(aOld: TNest; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscShape, aOld.NestShape, FNestShape, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupportType, aOld.SupportType, FSupportType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupportPlant1ID, aOld.SupportPlant1Id, FSupportPlant1Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSupportPlant2ID, aOld.SupportPlant2Id, FSupportPlant2Id, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOtherSupport, aOld.OtherSupport, FOtherSupport, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHeightAboveGround, aOld.HeightAboveGround, FHeightAboveGround, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMaxInternalDiameter, aOld.InternalMaxDiameter, FInternalMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMinInternalDiameter, aOld.InternalMinDiameter, FInternalMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMaxExternalDiameter, aOld.ExternalMaxDiameter, FExternalMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMinExternalDiameter, aOld.ExternalMinDiameter, FExternalMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscInternalHeight, aOld.InternalHeight, FInternalHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExternalHeight, aOld.ExternalHeight, FExternalHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantEdgeDistance, aOld.EdgeDistance, FEdgeDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantCenterDistance, aOld.CenterDistance, FCenterDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMaxPlantDiameter, aOld.PlantMaxDiameter, FPlantMaxDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMinPlantDiameter, aOld.PlantMinDiameter, FPlantMinDiameter, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantHeight, aOld.PlantHeight, FPlantHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlantDBH, aOld.PlantDbh, FPlantDbh, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCover, aOld.NestCover, FNestCover, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBuildingDays, aOld.ConstructionDays, FConstructionDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIncubationDays, aOld.IncubationDays, FIncubationDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestlingDays, aOld.NestlingDays, FNestlingDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscActiveDays, aOld.ActiveDays, FActiveDays, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestFate, aOld.NestFate, FNestFate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestProductivity, aOld.NestProductivity, FNestProductivity, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFoundDate, aOld.FoundDate, FFoundDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLastDateActive, aOld.LastDate, FLastDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TNest.Find(aFieldNumber: String; aTaxon, aSite: Integer; aDate: TDate): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT nest_id FROM nests');
    Add('WHERE (field_number = :afieldnumber)');
    Add('AND (taxon_id = :ataxon)');
    Add('AND (locality_id = :asite)');
    Add('AND (found_date = :adate)');
    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;
    ParamByName('ATAXON').AsInteger := aTaxon;
    ParamByName('ASITE').AsInteger := aSite;
    ParamByName('ADATE').AsDateTime := aDate;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('nest_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TNestOwner }

constructor TNestOwner.Create(aValue: Integer);
begin
  if (aValue > 0) then
    GetData(aValue)
  else
    Clear;
end;

procedure TNestOwner.Clear;
begin
  inherited Clear;
  FNestId := 0;
  FRole := nrlUnknown;
  FIndividualId := 0;
end;

procedure TNestOwner.Copy(aFrom: TNestOwner);
begin
  FNestId := aFrom.NestId;
  FRole := aFrom.Role;
  FIndividualId := aFrom.IndividualId;
end;

procedure TNestOwner.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNestOwner.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM nest_owners');
      Add('WHERE (nest_owner_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestOwner.Diff(aOld: TNestOwner; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscRole, aOld.Role, FRole, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TNestOwner.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
      'nest_owner_id, ' +
      'nest_id, ' +
      'role, ' +
      'individual_id, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM nest_owners');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TNestOwner.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'nest_owner_id, ' +
      'nest_id, ' +
      'role, ' +
      'individual_id, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM nest_owners');
    Add('WHERE nest_owner_id = :anid');
    ParamByName('ANID').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestOwner.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('nest_owner_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    case FieldByName('role').AsString of
      'M': FRole := nrlMale;
      'F': FRole := nrlFemale;
      'H': FRole := nrlHelper;
      'O': FRole := nrlOffspring;
    else
      FRole := nrlUnknown;
    end;
    FIndividualId := FieldByName('individual_id').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNestOwner.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO nest_owners (' +
        'nest_id, ' +
        'role, ' +
        'individual_id, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':nest_id, ' +
        ':role, ' +
        ':individual_id, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('role').AsString := NestRoles[FRole];
      if (FIndividualId > 0) then
        ParamByName('individual_id').AsInteger := FIndividualId
      else
        ParamByName('individual_id').Clear;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestOwner.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TNestOwner.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Role', NestRoles[FRole]);
    JSONObject.Add('Individual', FIndividualId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TNestOwner.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNestOwner.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE nest_owners SET ' +
        'nest_id = :nest_id, ' +
        'role = :role, ' +
        'individual_id = :individual_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'',''subsec'') ');
      Add('WHERE (nest_owner_id = :nest_owner_id)');

      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('role').AsString := NestRoles[FRole];
      if (FIndividualId > 0) then
        ParamByName('individual_id').AsInteger := FIndividualId
      else
        ParamByName('individual_id').Clear;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;
      ParamByName('nest_owner_id').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

