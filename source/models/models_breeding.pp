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

unit models_breeding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

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
    FLossCause: TLossCause;
    FNestProductivity: Integer;
    FFoundDate: TDate;
    FLastDate: TDate;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TNest; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TNest): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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
    property LossCause: TLossCause read FLossCause write FLossCause;
    property NestProductivity: Integer read FNestProductivity write FNestProductivity;
    property FoundDate: TDate read FFoundDate write FFoundDate;
    property LastDate: TDate read FLastDate write FLastDate;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

  { TNestRepository }

  TNestRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByFieldNumber(aFieldNumber: String; aTaxon, aSite: Integer; aDate: TDate; E: TNest);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TNestOwner }

  TNestOwner = class(TXolmisRecord)
  private
    FIndividualId: Integer;
    FNestId: Integer;
    FRole: TNestRole;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TNestOwner; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TNestOwner): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property NestId: Integer read FNestId write FNestId;
    property Role: TNestRole read FRole write FRole;
    property IndividualId: Integer read FIndividualId write FIndividualId;
  end;

  { TNestOwnerRepository }

  TNestOwnerRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TEgg; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TEgg): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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

  { TEggRepository }

  TEggRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByFieldNumber(aNest: Integer; aFieldNumber, aDate: String; aObserver: Integer; E: TEgg);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TNestRevision; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TNestRevision): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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

  { TNestRevisionRepository }

  TNestRevisionRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByDate(aNest: Integer; aDate, aTime: String; aObserver: Integer; E: TNestRevision);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  utils_locale, utils_global, models_users, data_columns, utils_validations, data_setparam, utils_fullnames,
  data_consts, data_getvalue,
  udm_main;

{ TNestRevision }

constructor TNestRevision.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TNestRevision.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNestRevision then
  begin
    FFullName := TNestRevision(Source).FullName;
    FNestId := TNestRevision(Source).NestId;
    FRevisionDate := TNestRevision(Source).RevisionDate;
    FRevisionTime := TNestRevision(Source).RevisionTime;
    FObserver1Id := TNestRevision(Source).Observer1Id;
    FObserver2Id := TNestRevision(Source).Observer2Id;
    FNestStatus := TNestRevision(Source).NestStatus;
    FHostEggsTally := TNestRevision(Source).HostEggsTally;
    FHostNestlingsTally := TNestRevision(Source).HostNestlingsTally;
    FNidoparasiteEggsTally := TNestRevision(Source).NidoparasiteEggsTally;
    FNidoparasiteNestlingsTally := TNestRevision(Source).NidoparasiteNestlingsTally;
    FNidoparasiteId := TNestRevision(Source).NidoparasiteId;
    FHavePhilornisLarvae := TNestRevision(Source).HavePhilornisLarvae;
    FNestStage := TNestRevision(Source).NestStage;
    FNotes := TNestRevision(Source).Notes;
  end;
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

function TNestRevision.Clone: TXolmisRecord;
begin
  Result := TNestRevision(inherited Clone);
end;

function TNestRevision.Diff(const aOld: TNestRevision; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscNest, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.RevisionDate, FRevisionDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.RevisionTime, FRevisionTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserver1ID, aOld.Observer1Id, FObserver1Id, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserver2ID, aOld.Observer2Id, FObserver2Id, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStatus, aOld.NestStatus, FNestStatus, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggsHost, aOld.HostEggsTally, FHostEggsTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestlingsHost, aOld.HostNestlingsTally, FHostNestlingsTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggsNidoparasite, aOld.NidoparasiteEggsTally, FNidoparasiteEggsTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestlingsNidoparasite, aOld.NidoparasiteNestlingsTally, FNidoparasiteNestlingsTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNidoparasiteID, aOld.NidoparasiteId, FNidoparasiteId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHasPhilornisLarvae, aOld.HavePhilornisLarvae, FHavePhilornisLarvae, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestStage, aOld.NestStage, FNestStage, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TNestRevision.EqualsTo(const Other: TNestRevision): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TNestRevision.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName     := Obj.Get('full_name', '');
    FNestId       := Obj.Get('nest_id', 0);
    FRevisionDate := Obj.Get('revision_date', NullDate);
    FRevisionTime := Obj.Get('revision_time', NullTime);
    FObserver1Id  := Obj.Get('observer_1_id', 0);
    FObserver2Id  := Obj.Get('observer_2_id', 0);
    case Obj.Get('nest_status', '') of
      'I': FNestStatus := nstInactive;
      'A': FNestStatus := nstActive;
    else
      FNestStatus := nstUnknown;
    end;
    case Obj.Get('nest_stage', '') of
      'X': FNestStage := nsgInactive;
      'C': FNestStage := nsgConstruction;
      'L': FNestStage := nsgLaying;
      'I': FNestStage := nsgIncubation;
      'H': FNestStage := nsgHatching;
      'N': FNestStage := nsgNestling;
    else
      FNestStage := nsgUnknown;
    end;
    FHostEggsTally              := Obj.Get('host_eggs', 0);
    FHostNestlingsTally         := Obj.Get('host_nestlings', 0);
    FNidoparasiteEggsTally      := Obj.Get('nidoparasite_eggs', 0);
    FNidoparasiteNestlingsTally := Obj.Get('nidoparasite_nestlings', 0);
    FNidoparasiteId             := Obj.Get('nidoparasite_id', 0);
    FHavePhilornisLarvae        := Obj.Get('have_philornis_larvae', False);
    FNotes                      := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TNestRevision.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('revision_date', FRevisionDate);
    JSONObject.Add('revision_time', FRevisionTime);
    JSONObject.Add('observer_1_id', FObserver1Id);
    JSONObject.Add('observer_2_id', FObserver2Id);
    JSONObject.Add('nest_status', NEST_STATUSES[FNestStatus]);
    JSONObject.Add('host_eggs', FHostEggsTally);
    JSONObject.Add('host_nestlings', FHostNestlingsTally);
    JSONObject.Add('nidoparasite_eggs', FNidoparasiteEggsTally);
    JSONObject.Add('nidoparasite_nestlings', FNidoparasiteNestlingsTally);
    JSONObject.Add('nidoparasite_id', FNidoparasiteId);
    JSONObject.Add('have_philornis_larvae', FHavePhilornisLarvae);
    JSONObject.Add('nest_stage', NEST_STAGES[FNestStage]);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TNestRevision.ToString: String;
begin
  Result := Format('NestRevision(Id=%d, FullName=%s, NestId=%d, RevisionDate=%s, RevisionTime=%s, Observer1Id=%d, ' +
    'Observer2Id=%d, NestStatus=%s, HostEggsTally=%d, HostNestlingsTally=%d, NidoparasiteEggsTally=%d, ' +
    'NidoparasiteNestlingsTally=%d, NidoparasiteId=%d, HavePhilornisLarvae=%s, NestStage=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FNestId, DateToStr(FRevisionDate), TimeToStr(FRevisionTime), FObserver1Id, FObserver2Id,
    NEST_STATUSES[FNestStatus], FHostEggsTally, FHostNestlingsTally, FNidoparasiteEggsTally,
    FNidoparasiteNestlingsTally, FNidoparasiteId, BoolToStr(FHavePhilornisLarvae, 'True', 'False'),
    NEST_STAGES[FNestStage], FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TNestRevision.Validate(out Msg: string): Boolean;
begin
  if FNestId = 0 then
  begin
    Msg := 'Nest required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TNestRevisionRepository }

procedure TNestRevisionRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNestRevision;
begin
  if not (E is TNestRevision) then
    raise Exception.Create('Delete: Expected TNestRevision');

  R := TNestRevision(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNestRevisionRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_NEST_REVISION_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestRevisionRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_NEST_REVISION_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevisionRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_NEST_REVISION_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TNestRevision) then
    raise Exception.Create('FindBy: Expected TNestRevision');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

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
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TNestRevision(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TNestRevisionRepository.FindByDate(aNest: Integer; aDate, aTime: String; aObserver: Integer;
  E: TNestRevision);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM nest_revisions');
    Add('WHERE (nest_id = :anest)');
    Add('AND (date(sample_date) = date(:adate))');
    Add('AND (time(sample_time) = time(:atime))');
    Add('AND (observer_1_id = :aobserver)');
    ParamByName('ANEST').AsInteger := aNest;
    ParamByName('AOBSERVER').AsInteger := aObserver;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('ATIME').AsString := aTime;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevisionRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TNestRevision) then
    raise Exception.Create('GetById: Expected TNestRevision');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TNestRevision(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRevisionRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TNestRevision;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TNestRevision) then
    raise Exception.Create('Hydrate: Expected TNestRevision');

  R := TNestRevision(E);
  with aDataSet do
  begin
    R.Id := FieldByName('nest_revision_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.RevisionDate := FieldByName('revision_date').AsDateTime;
    R.RevisionTime := FieldByName('revision_time').AsDateTime;
    R.Observer1Id := FieldByName('observer_1_id').AsInteger;
    R.Observer2Id := FieldByName('observer_2_id').AsInteger;
    case FieldByName('nest_status').AsString of
      'I': R.NestStatus := nstInactive;
      'A': R.NestStatus := nstActive;
    else
      R.NestStatus := nstUnknown;
    end;
    R.HostEggsTally := FieldByName('host_eggs_tally').AsInteger;
    R.HostNestlingsTally := FieldByName('host_nestlings_tally').AsInteger;
    R.NidoparasiteEggsTally := FieldByName('nidoparasite_eggs_tally').AsInteger;
    R.NidoparasiteNestlingsTally := FieldByName('nidoparasite_nestlings_tally').AsInteger;
    R.NidoparasiteId := FieldByName('nidoparasite_id').AsInteger;
    R.HavePhilornisLarvae := FieldByName('have_philornis_larvae').AsBoolean;
    case FieldByName('nest_stage').AsString of
      'X': R.NestStage := nsgInactive;
      'C': R.NestStage := nsgConstruction;
      'L': R.NestStage := nsgLaying;
      'I': R.NestStage := nsgIncubation;
      'H': R.NestStage := nsgHatching;
      'N': R.NestStage := nsgNestling;
    else
      R.NestStage := nsgUnknown;
    end;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNestRevisionRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNestRevision;
begin
  if not (E is TNestRevision) then
    raise Exception.Create('Insert: Expected TNestRevision');

  R := TNestRevision(E);
  Qry := NewQuery;
  with Qry, SQL do
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
    ParamByName('nest_id').AsInteger := R.NestId;
    R.FullName := GetNestRevisionFullName(R.RevisionDate, R.NestId, NEST_STAGES[R.NestStage], NEST_STATUSES[R.NestStatus]);
    SetStrParam(ParamByName('full_name'), R.Fullname);
    SetDateParam(ParamByName('revision_date'), R.RevisionDate);
    SetTimeParam(ParamByName('revision_time'), R.RevisionTime);
    SetForeignParam(ParamByName('observer_1_id'), R.Observer1Id);
    SetForeignParam(ParamByName('observer_2_id'), R.Observer2Id);
    ParamByName('nest_status').AsString := NEST_STATUSES[R.NestStatus];
    ParamByName('host_eggs_tally').AsInteger := R.HostEggsTally;
    ParamByName('host_nestlings_tally').AsInteger := R.HostNestlingsTally;
    ParamByName('nidoparasite_eggs_tally').AsInteger := R.NidoparasiteEggsTally;
    ParamByName('nidoparasite_nestlings_tally').AsInteger := R.NidoparasiteNestlingsTally;
    SetForeignParam(ParamByName('nidoparasite_id'), R.NidoparasiteId);
    ParamByName('have_philornis_larvae').AsBoolean := R.HavePhilornisLarvae;
    ParamByName('nest_stage').AsString := NEST_STAGES[R.NestStage];
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestRevisionRepository.TableName: string;
begin
  Result := TBL_NEST_REVISIONS;
end;

procedure TNestRevisionRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNestRevision;
begin
  if not (E is TNestRevision) then
    raise Exception.Create('Update: Expected TNestRevision');

  R := TNestRevision(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNestRevisionRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
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

    ParamByName('nest_id').AsInteger := R.NestId;
    R.FullName := GetNestRevisionFullName(R.RevisionDate, R.NestId, NEST_STAGES[R.NestStage], NEST_STATUSES[R.NestStatus]);
    SetStrParam(ParamByName('full_name'), R.Fullname);
    SetDateParam(ParamByName('revision_date'), R.RevisionDate);
    SetTimeParam(ParamByName('revision_time'), R.RevisionTime);
    SetForeignParam(ParamByName('observer_1_id'), R.Observer1Id);
    SetForeignParam(ParamByName('observer_2_id'), R.Observer2Id);
    ParamByName('nest_status').AsString := NEST_STATUSES[R.NestStatus];
    ParamByName('host_eggs_tally').AsInteger := R.HostEggsTally;
    ParamByName('host_nestlings_tally').AsInteger := R.HostNestlingsTally;
    ParamByName('nidoparasite_eggs_tally').AsInteger := R.NidoparasiteEggsTally;
    ParamByName('nidoparasite_nestlings_tally').AsInteger := R.NidoparasiteNestlingsTally;
    SetForeignParam(ParamByName('nidoparasite_id'), R.NidoparasiteId);
    ParamByName('have_philornis_larvae').AsBoolean := R.HavePhilornisLarvae;
    ParamByName('nest_stage').AsString := NEST_STAGES[R.NestStage];
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('nest_revision_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TEgg }

constructor TEgg.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TEgg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TEgg then
  begin
    FFullName := TEgg(Source).FullName;
    FEggSeq := TEgg(Source).EggSeq;
    FFieldNumber := TEgg(Source).FieldNumber;
    FNestId := TEgg(Source).NestId;
    FEggShape := TEgg(Source).EggShape;
    FWidth := TEgg(Source).Width;
    FLength := TEgg(Source).Length;
    FMass := TEgg(Source).Mass;
    FVolume := TEgg(Source).Volume;
    FEggStage := TEgg(Source).EggStage;
    FEggshellColor := TEgg(Source).EggshellColor;
    FEggshellPattern := TEgg(Source).EggshellPattern;
    FEggshellTexture := TEgg(Source).EggshellTexture;
    FEggHatched := TEgg(Source).EggHatched;
    FIndividualId := TEgg(Source).IndividualId;
    FResearcherId := TEgg(Source).ResearcherId;
    FMeasureDate := TEgg(Source).MeasureDate;
    FTaxonId := TEgg(Source).TaxonId;
    FHostEgg := TEgg(Source).HostEgg;
    FDescription := TEgg(Source).Description;
    FNotes := TEgg(Source).Notes;
  end;
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

function TEgg.Clone: TXolmisRecord;
begin
  Result := TEgg(inherited Clone);
end;

function TEgg.Diff(const aOld: TEgg; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggNumber, aOld.EggSeq, FEggSeq, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggShape, aOld.EggShape, FEggShape, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWidth, aOld.Width, FWidth, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLength, aOld.Length, FLength, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMass, aOld.Mass, FMass, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscVolume, aOld.Volume, FVolume, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStage, aOld.EggStage, FEggStage, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggshellColor, aOld.EggshellColor, FEggshellColor, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggshellPattern, aOld.EggshellPattern, FEggshellPattern, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggshellTexture, aOld.EggshellTexture, FEggshellTexture, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHatched, aOld.EggHatched, FEggHatched, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscResearcherID, aOld.ResearcherId, FResearcherId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.MeasureDate, FMeasureDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHostEgg, aOld.HostEgg, FHostEgg, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TEgg.EqualsTo(const Other: TEgg): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TEgg.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFieldNumber  := Obj.Get('field_number', '');
    FFullName     := Obj.Get('full_name', '');
    FEggSeq       := Obj.Get('egg_number', 0);
    FNestId       := Obj.Get('nest_id', 0);
    case Obj.Get('egg_shape', '') of
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
    FWidth          := Obj.Get('width', 0.0);
    FLength         := Obj.Get('length', 0.0);
    FMass           := Obj.Get('mass', 0.0);
    FVolume         := Obj.Get('volume', 0.0);
    FEggStage       := Obj.Get('stage', '');
    FTaxonId        := Obj.Get('taxon_id', 0);
    FEggshellColor  := Obj.Get('color', '');
    case Obj.Get('pattern', '') of
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
    case Obj.Get('texture', '') of
      'C': FEggshellTexture := estChalky;
      'S': FEggshellTexture := estShiny;
      'G': FEggshellTexture := estGlossy;
      'P': FEggshellTexture := estPitted;
    else
      FEggshellTexture := estUnknown;
    end;
    FEggHatched   := Obj.Get('hatched', False);
    FIndividualId := Obj.Get('individual_id', 0);
    FResearcherId := Obj.Get('researcher_id', 0);
    FMeasureDate  := Obj.Get('measure_date', NullDate);
    FHostEgg      := Obj.Get('host_egg', True);
    FDescription  := Obj.Get('description', '');
    FNotes        := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TEgg.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('field_number', FFieldNumber);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('egg_number', FEggSeq);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('egg_shape', EGG_SHAPES[FEggShape]);
    JSONObject.Add('width', FWidth);
    JSONObject.Add('length', FLength);
    JSONObject.Add('mass', FMass);
    JSONObject.Add('volume', FVolume);
    JSONObject.Add('stage', FEggStage);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('color', FEggshellColor);
    JSONObject.Add('pattern', EGGSHELL_PATTERNS[FEggshellPattern]);
    JSONObject.Add('texture', EGGSHELL_TEXTURES[FEggshellTexture]);
    JSONObject.Add('hatched', FEggHatched);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('researcher_id', FResearcherId);
    JSONObject.Add('measure_date', FMeasureDate);
    JSONObject.Add('host_egg', FHostEgg);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TEgg.ToString: String;
begin
  Result := Format('Egg(Id=%d, FullName=%s, FieldNumber=%s, EggSeq=%d, NestId=%d, EggShape=%s, Width=%f, Length=%f, ' +
    'Mass=%f, Volume=%f, EggStage=%s, TaxonId=%d, EggshellColor=%s, EggshellPattern=%s, EggshellTexture=%s, ' +
    'EggHatched=%s, IndividualId=%d, ResearcherId=%d, MeasureDate=%s, HostEgg=%s, Description=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FFieldNumber, FEggSeq, FNestId, EGG_SHAPES[FEggShape], FWidth, FLength, FMass, FVolume,
    FEggStage, FTaxonId, FEggshellColor, EGGSHELL_PATTERNS[FEggshellPattern], EGGSHELL_TEXTURES[FEggshellTexture],
    BoolToStr(FEggHatched, 'True', 'False'), FIndividualId, FResearcherId, DateToStr(FMeasureDate),
    BoolToStr(FHostEgg, 'True', 'False'), FDescription, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TEgg.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'Taxon required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TEggRepository }

procedure TEggRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TEgg;
begin
  if not (E is TEgg) then
    raise Exception.Create('Delete: Expected TEgg');

  R := TEgg(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TEggRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_EGG_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TEggRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_EGG_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEggRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_EGG_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TEgg) then
    raise Exception.Create('FindBy: Expected TEgg');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

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
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TEgg(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TEggRepository.FindByFieldNumber(aNest: Integer; aFieldNumber, aDate: String; aObserver: Integer;
  E: TEgg);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM eggs');
    Add('WHERE (nest_id = :anest)');
    Add('AND (date(measure_date) = date(:adate))');
    Add('AND (field_number = :afieldnumber)');
    Add('AND (researcher_id = :aobserver)');
    ParamByName('ANEST').AsInteger := aNest;
    ParamByName('AOBSERVER').AsInteger := aObserver;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEggRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TEgg) then
    raise Exception.Create('GetById: Expected TEgg');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TEgg(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TEggRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TEgg;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TEgg) then
    raise Exception.Create('Hydrate: Expected TEgg');

  R := TEgg(E);
  with aDataSet do
  begin
    R.Id := FieldByName('egg_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.FieldNumber := FieldByName('field_number').AsString;
    R.EggSeq := FieldByName('egg_seq').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    case FieldByName('egg_shape').AsString of
      'S': R.EggShape := esSpherical;
      'E': R.EggShape := esElliptical;
      'O': R.EggShape := esOval;
      'P': R.EggShape := esPiriform;
      'C': R.EggShape := esConical;
      'B': R.EggShape := esBiconical;
      'Y': R.EggShape := esCylindrical;
      'L': R.EggShape := esLongitudinal;
    else
      R.EggShape := esUnknown;
    end;
    R.Width := FieldByName('egg_width').AsFloat;
    R.Length := FieldByName('egg_length').AsFloat;
    R.Mass := FieldByName('egg_mass').AsFloat;
    R.Volume := FieldByName('egg_volume').AsFloat;
    R.EggStage := FieldByName('egg_stage').AsString;
    R.EggshellColor := FieldByName('eggshell_color').AsString;
    case FieldByName('eggshell_pattern').AsString of
      'P':  R.EggshellPattern := espSpots;
      'B':  R.EggshellPattern := espBlotches;
      'S':  R.EggshellPattern := espSquiggles;
      'T':  R.EggshellPattern := espStreaks;
      'W':  R.EggshellPattern := espScrawls;
      'PS': R.EggshellPattern := espSpotsSquiggles;
      'BS': R.EggshellPattern := espBlotchesSquiggles;
    else
      R.EggshellPattern := espUnknown;
    end;
    case FieldByName('eggshell_texture').AsString of
      'C': R.EggshellTexture := estChalky;
      'S': R.EggshellTexture := estShiny;
      'G': R.EggshellTexture := estGlossy;
      'P': R.EggshellTexture := estPitted;
    else
      R.EggshellTexture := estUnknown;
    end;
    R.EggHatched := FieldByName('egg_hatched').AsBoolean;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.ResearcherId := FieldByName('researcher_id').AsInteger;
    R.MeasureDate := FieldByName('measure_date').AsDateTime;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.HostEgg := FieldByName('host_egg').AsBoolean;
    R.Description := FieldByName('description').AsString;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TEggRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TEgg;
begin
  if not (E is TEgg) then
    raise Exception.Create('Insert: Expected TEgg');

  R := TEgg(E);
  Qry := NewQuery;
  with Qry, SQL do
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

    ParamByName('field_number').AsString := R.FieldNumber;
    ParamByName('egg_seq').AsInteger := R.EggSeq;
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    ParamByName('egg_shape').AsString := EGG_SHAPES[R.EggShape];
    SetFloatParam(ParamByName('egg_width'), R.Width);
    SetFloatParam(ParamByName('egg_length'), R.Length);
    SetFloatParam(ParamByName('egg_mass'), R.Mass);
    SetFloatParam(ParamByName('egg_volume'), R.Volume);
    SetStrParam(ParamByName('egg_stage'), R.EggStage);
    SetStrParam(ParamByName('eggshell_color'), R.EggshellColor);
    ParamByName('eggshell_pattern').AsString := EGGSHELL_PATTERNS[R.EggshellPattern];
    ParamByName('eggshell_texture').AsString := EGGSHELL_TEXTURES[R.EggshellTexture];
    ParamByName('egg_hatched').AsBoolean := R.EggHatched;
    SetForeignParam(ParamByName('researcher_id'), R.ResearcherId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetDateParam(ParamByName('measure_date'), R.MeasureDate);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    ParamByName('host_egg').AsBoolean := R.HostEgg;
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('full_name'), R.Fullname);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TEggRepository.TableName: string;
begin
  Result := TBL_EGGS;
end;

procedure TEggRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TEgg;
begin
  if not (E is TEgg) then
    raise Exception.Create('Update: Expected TEgg');

  R := TEgg(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TEggRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
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
    Add('  measure_date = date(:measure_date),');
    Add('  taxon_id = :taxon_id,');
    Add('  host_egg = :host_egg,');
    Add('  description = :description,');
    Add('  notes = :notes,');
    Add('  full_name = :full_name,');
    Add('  user_updated = :user_updated,');
    Add('  update_date = datetime(''now'',''subsec'')');
    Add('WHERE (egg_id = :egg_id);');

    ParamByName('field_number').AsString := R.FieldNumber;
    ParamByName('egg_seq').AsInteger := R.EggSeq;
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    ParamByName('egg_shape').AsString := EGG_SHAPES[R.EggShape];
    SetFloatParam(ParamByName('egg_width'), R.Width);
    SetFloatParam(ParamByName('egg_length'), R.Length);
    SetFloatParam(ParamByName('egg_mass'), R.Mass);
    SetFloatParam(ParamByName('egg_volume'), R.Volume);
    SetStrParam(ParamByName('egg_stage'), R.EggStage);
    SetStrParam(ParamByName('eggshell_color'), R.EggshellColor);
    ParamByName('eggshell_pattern').AsString := EGGSHELL_PATTERNS[R.EggshellPattern];
    ParamByName('eggshell_texture').AsString := EGGSHELL_TEXTURES[R.EggshellTexture];
    ParamByName('egg_hatched').AsBoolean := R.EggHatched;
    SetForeignParam(ParamByName('researcher_id'), R.ResearcherId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetDateParam(ParamByName('measure_date'), R.MeasureDate);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    ParamByName('host_egg').AsBoolean := R.HostEgg;
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('full_name'), R.Fullname);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('egg_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TNest }

constructor TNest.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TNest.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNest then
  begin
    FFieldNumber := TNest(Source).FieldNumber;
    FFullName := TNest(Source).FullName;
    FObserverId := TNest(Source).ObserverId;
    FLocalityId := TNest(Source).LocalityId;
    FLatitude := TNest(Source).Latitude;
    FLongitude := TNest(Source).Longitude;
    FTaxonId := TNest(Source).TaxonId;
    FNestShape := TNest(Source).NestShape;
    FSupportType := TNest(Source).SupportType;
    FSupportPlant1Id := TNest(Source).SupportPlant1Id;
    FSupportPlant2Id := TNest(Source).SupportPlant2Id;
    FOtherSupport := TNest(Source).OtherSupport;
    FHeightAboveGround := TNest(Source).HeightAboveGround;
    FProjectId := TNest(Source).ProjectId;
    FInternalMaxDiameter := TNest(Source).InternalMaxDiameter;
    FInternalMinDiameter := TNest(Source).InternalMinDiameter;
    FExternalMaxDiameter := TNest(Source).ExternalMaxDiameter;
    FExternalMinDiameter := TNest(Source).ExternalMinDiameter;
    FInternalHeight := TNest(Source).InternalHeight;
    FExternalHeight := TNest(Source).ExternalHeight;
    FEdgeDistance := TNest(Source).EdgeDistance;
    FCenterDistance := TNest(Source).CenterDistance;
    FNestCover := TNest(Source).NestCover;
    FPlantMaxDiameter := TNest(Source).PlantMaxDiameter;
    FPlantMinDiameter := TNest(Source).PlantMinDiameter;
    FPlantHeight := TNest(Source).PlantHeight;
    FPlantDbh := TNest(Source).PlantDbh;
    FConstructionDays := TNest(Source).ConstructionDays;
    FIncubationDays := TNest(Source).IncubationDays;
    FNestlingDays := TNest(Source).NestlingDays;
    FActiveDays := TNest(Source).ActiveDays;
    FNestFate := TNest(Source).NestFate;
    FNestProductivity := TNest(Source).NestProductivity;
    FFoundDate := TNest(Source).FoundDate;
    FLastDate := TNest(Source).LastDate;
    FDescription := TNest(Source).Description;
    FNotes := TNest(Source).Notes;
  end;
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
  FLossCause := nlcUnknown;
  FNestProductivity := 0;
  FFoundDate := NullDate;
  FLastDate := NullDate;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

function TNest.Clone: TXolmisRecord;
begin
  Result := TNest(inherited Clone);
end;

function TNest.Diff(const aOld: TNest; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscShape, aOld.NestShape, FNestShape, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSupportType, aOld.SupportType, FSupportType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSupportPlant1ID, aOld.SupportPlant1Id, FSupportPlant1Id, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSupportPlant2ID, aOld.SupportPlant2Id, FSupportPlant2Id, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOtherSupport, aOld.OtherSupport, FOtherSupport, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHeightAboveGround, aOld.HeightAboveGround, FHeightAboveGround, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMaxInternalDiameter, aOld.InternalMaxDiameter, FInternalMaxDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMinInternalDiameter, aOld.InternalMinDiameter, FInternalMinDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMaxExternalDiameter, aOld.ExternalMaxDiameter, FExternalMaxDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMinExternalDiameter, aOld.ExternalMinDiameter, FExternalMinDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscInternalHeight, aOld.InternalHeight, FInternalHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExternalHeight, aOld.ExternalHeight, FExternalHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPlantEdgeDistance, aOld.EdgeDistance, FEdgeDistance, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPlantCenterDistance, aOld.CenterDistance, FCenterDistance, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMaxPlantDiameter, aOld.PlantMaxDiameter, FPlantMaxDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMinPlantDiameter, aOld.PlantMinDiameter, FPlantMinDiameter, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPlantHeight, aOld.PlantHeight, FPlantHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPlantDBH, aOld.PlantDbh, FPlantDbh, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCover, aOld.NestCover, FNestCover, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBuildingDays, aOld.ConstructionDays, FConstructionDays, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIncubationDays, aOld.IncubationDays, FIncubationDays, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestlingDays, aOld.NestlingDays, FNestlingDays, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscActiveDays, aOld.ActiveDays, FActiveDays, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestFate, aOld.NestFate, FNestFate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLossCause, aOld.LossCause, FLossCause, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestProductivity, aOld.NestProductivity, FNestProductivity, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFoundDate, aOld.FoundDate, FFoundDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLastDateActive, aOld.LastDate, FLastDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TNest.EqualsTo(const Other: TNest): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TNest.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName             := Obj.Get('full_name', '');
    FFieldNumber          := Obj.Get('field_number', '');
    FObserverId           := Obj.Get('observer_id', 0);
    FProjectId            := Obj.Get('project_id', 0);
    FLocalityId           := Obj.Get('locality_id', 0);
    FLongitude            := Obj.Get('longitude', 0.0);
    FLatitude             := Obj.Get('latitude', 0.0);
    FTaxonId              := Obj.Get('taxon_id', 0);
    FNestShape            := Obj.Get('nest_shape', '');
    FSupportType          := Obj.Get('support_type', '');
    FSupportPlant1Id      := Obj.Get('support_plant_1_id', 0);
    FSupportPlant2Id      := Obj.Get('support_plant_2_id', 0);
    FOtherSupport         := Obj.Get('other_support', '');
    FHeightAboveGround    := Obj.Get('height_above_ground', 0.0);
    FInternalMaxDiameter  := Obj.Get('max_internal_diameter', 0.0);
    FInternalMinDiameter  := Obj.Get('min_internal_diameter', 0.0);
    FExternalMaxDiameter  := Obj.Get('max_external_diameter', 0.0);
    FExternalMinDiameter  := Obj.Get('min_external_diameter', 0.0);
    FInternalHeight       := Obj.Get('internal_height', 0.0);
    FExternalHeight       := Obj.Get('external_height', 0.0);
    FEdgeDistance         := Obj.Get('edge_distance', 0.0);
    FCenterDistance       := Obj.Get('center_distance', 0.0);
    FNestCover            := Obj.Get('nest_cover', 0);
    FPlantMaxDiameter     := Obj.Get('max_plant_diameter', 0.0);
    FPlantMinDiameter     := Obj.Get('min_plant_diameter', 0.0);
    FPlantHeight          := Obj.Get('plant_height', 0.0);
    FPlantDbh             := Obj.Get('plant_dbh', 0.0);
    FConstructionDays     := Obj.Get('construction_days', 0.0);
    FIncubationDays       := Obj.Get('incubation_days', 0.0);
    FNestlingDays         := Obj.Get('nestling_days', 0.0);
    FActiveDays           := Obj.Get('active_days', 0.0);
    case Obj.Get('nest_fate', '') of
      'L': FNestFate := nfLoss;
      'S': FNestFate := nfSuccess;
    else
      FNestFate := nfUnknown;
    end;
    case Obj.Get('loss_cause', '') of
      'PRE': FLossCause := nlcPredation;
      'PAR': FLossCause := nlcParasitism;
      'DIS': FLossCause := nlcDisease;
      'WEA': FLossCause := nlcWeather;
      'FIR': FLossCause := nlcFire;
      'ABD': FLossCause := nlcAbandonment;
      'POL': FLossCause := nlcPollution;
      'HDT': FLossCause := nlcHumanDisturbance;
      'IMN': FLossCause := nlcImproperManagement;
    else
      FLossCause := nlcUnknown;
    end;
    FNestProductivity := Obj.Get('nest_productivity', 0);
    FFoundDate        := Obj.Get('found_date', NullDate);
    FLastDate         := Obj.Get('last_date_active', NullDate);
    FDescription      := Obj.Get('description', '');
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TNest.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('field_number', FFieldNumber);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('observer_id', FObserverId);
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('nest_shape', FNestShape);
    JSONObject.Add('support_type', FSupportType);
    JSONObject.Add('support_plant_1_id', FSupportPlant1Id);
    JSONObject.Add('support_plant_2_id', FSupportPlant2Id);
    JSONObject.Add('other_support', FOtherSupport);
    JSONObject.Add('height_above_ground', FHeightAboveGround);
    JSONObject.Add('max_internal_diameter', FInternalMaxDiameter);
    JSONObject.Add('min_internal_diameter', FInternalMinDiameter);
    JSONObject.Add('max_external_diameter', FExternalMaxDiameter);
    JSONObject.Add('min_external_diameter', FExternalMinDiameter);
    JSONObject.Add('internal_height', FInternalHeight);
    JSONObject.Add('external_height', FExternalHeight);
    JSONObject.Add('edge_distance', FEdgeDistance);
    JSONObject.Add('center_distance', FCenterDistance);
    JSONObject.Add('nest_cover', FNestCover);
    JSONObject.Add('max_plant_diameter', FPlantMaxDiameter);
    JSONObject.Add('min_plant_diameter', FPlantMinDiameter);
    JSONObject.Add('plant_height', FPlantHeight);
    JSONObject.Add('plant_dbh', FPlantDbh);
    JSONObject.Add('construction_days', FConstructionDays);
    JSONObject.Add('incubation_days', FIncubationDays);
    JSONObject.Add('nestling_days', FNestlingDays);
    JSONObject.Add('active_days', FActiveDays);
    JSONObject.Add('nest_fate', NEST_FATES[FNestFate]);
    JSONObject.Add('loss_cause', LOSS_CAUSES[FLossCause]);
    JSONObject.Add('nest_productivity', FNestProductivity);
    JSONObject.Add('found_date', FFoundDate);
    JSONObject.Add('last_date_active', FLastDate);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TNest.ToString: String;
begin
  Result := Format('Nest(Id=%d, FullName=%s, FieldNumber=%s, ObserverId=%d, ProjectId=%d, LocalityId=%d, ' +
    'Longitude=%f, Latitude=%f, TaxonId=%d, NestShape=%s, SupportType=%s, SupportPlant1Id=%d, SupportPlant2Id=%d, ' +
    'OtherSupport=%s, HeightAboveGround=%f, InternalMaxDiameter=%f, InternalMinDiameter=%f, ExternalMaxDiameter=%f, ' +
    'ExternalMinDiameter=%f, InternalHeight=%f, ExternalHeight=%f, EdgeDistance=%f, CenterDistance=%f, ' +
    'NestCover=%d, PlantMaxDiameter=%f, PlantMinDiameter=%f, PlantHeight=%f, PlantDbh=%f, ConstructionDays=%f, ' +
    'IncubationDays=%f, NestlingDays=%f, ActiveDays=%f, NestFate=%s, LossCause=%s, NestProductivity=%d, FoundDate=%s, ' +
    'LastDate=%s, Description=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FFieldNumber, FObserverId, FProjectId, FLocalityId, FLongitude, FLatitude, FTaxonId,
    FNestShape, FSupportType, FSupportPlant1Id, FSupportPlant2Id, FOtherSupport, FHeightAboveGround,
    FInternalMaxDiameter, FInternalMinDiameter, FExternalMaxDiameter, FExternalMinDiameter,
    FInternalHeight, FExternalHeight, FEdgeDistance, FCenterDistance, FNestCover, FPlantMaxDiameter,
    FPlantMinDiameter, FPlantHeight, FPlantDbh, FConstructionDays, FIncubationDays, FNestlingDays, FActiveDays,
    NEST_FATES[FNestFate], LOSS_CAUSES[FLossCause], FNestProductivity, DateToStr(FFoundDate), DateToStr(FLastDate),
    FDescription, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TNest.Validate(out Msg: string): Boolean;
begin
  if FFieldNumber = EmptyStr then
  begin
    Msg := 'Field number required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TNestRepository }

procedure TNestRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNest;
begin
  if not (E is TNest) then
    raise Exception.Create('Delete: Expected TNest');

  R := TNest(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNestRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_NEST_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_NEST_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_NEST_ID, COL_FIELD_NUMBER, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TNest) then
    raise Exception.Create('FindBy: Expected TNest');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

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
      'loss_cause, ' +
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
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TNest(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TNestRepository.FindByFieldNumber(aFieldNumber: String; aTaxon, aSite: Integer; aDate: TDate; E: TNest);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM nests');
    Add('WHERE (field_number = :afieldnumber)');
    Add('AND (taxon_id = :ataxon)');
    Add('AND (locality_id = :asite)');
    Add('AND (found_date = :adate)');
    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;
    ParamByName('ATAXON').AsInteger := aTaxon;
    ParamByName('ASITE').AsInteger := aSite;
    ParamByName('ADATE').AsDateTime := aDate;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TNest) then
    raise Exception.Create('GetById: Expected TNest');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
      'loss_cause, ' +
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TNest(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TNest;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TNest) then
    raise Exception.Create('Hydrate: Expected TNest');

  R := TNest(E);
  with aDataSet do
  begin
    R.Id := FieldByName('nest_id').AsInteger;
    R.FieldNumber := FieldByName('field_number').AsString;
    R.FullName := FieldByName('full_name').AsString;
    R.ObserverId := FieldByName('observer_id').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.NestShape := FieldByName('nest_shape').AsString;
    R.SupportType := FieldByName('support_type').AsString;
    R.SupportPlant1Id := FieldByName('support_plant_1_id').AsInteger;
    R.SupportPlant2Id := FieldByName('support_plant_2_id').AsInteger;
    R.OtherSupport := FieldByName('other_support').AsString;
    R.HeightAboveGround := FieldByName('height_above_ground').AsFloat;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.InternalMaxDiameter := FieldByName('internal_max_diameter').AsFloat;
    R.InternalMinDiameter := FieldByName('internal_min_diameter').AsFloat;
    R.ExternalMaxDiameter := FieldByName('external_max_diameter').AsFloat;
    R.ExternalMinDiameter := FieldByName('external_min_diameter').AsFloat;
    R.InternalHeight := FieldByName('internal_height').AsFloat;
    R.ExternalHeight := FieldByName('external_height').AsFloat;
    R.EdgeDistance := FieldByName('edge_distance').AsFloat;
    R.CenterDistance := FieldByName('center_distance').AsFloat;
    R.NestCover := FieldByName('nest_cover').AsInteger;
    R.PlantMaxDiameter := FieldByName('plant_max_diameter').AsFloat;
    R.PlantMinDiameter := FieldByName('plant_min_diameter').AsFloat;
    R.PlantHeight := FieldByName('plant_height').AsFloat;
    R.PlantDbh := FieldByName('plant_dbh').AsFloat;
    R.ConstructionDays := FieldByName('construction_days').AsFloat;
    R.IncubationDays := FieldByName('incubation_days').AsFloat;
    R.NestlingDays := FieldByName('nestling_days').AsFloat;
    R.ActiveDays := FieldByName('active_days').AsFloat;
    case FieldByName('nest_fate').AsString of
      'L': R.NestFate := nfLoss;
      'S': R.NestFate := nfSuccess;
    else
      R.NestFate := nfUnknown;
    end;
    case FieldByName('loss_cause').AsString of
      'PRE': R.LossCause := nlcPredation;
      'PAR': R.LossCause := nlcParasitism;
      'DIS': R.LossCause := nlcDisease;
      'WEA': R.LossCause := nlcWeather;
      'FIR': R.LossCause := nlcFire;
      'ABD': R.LossCause := nlcAbandonment;
      'POL': R.LossCause := nlcPollution;
      'HDT': R.LossCause := nlcHumanDisturbance;
      'IMN': R.LossCause := nlcImproperManagement;
    else
      R.LossCause := nlcUnknown;
    end;
    R.NestProductivity := FieldByName('nest_productivity').AsInteger;
    R.FoundDate := FieldByName('found_date').AsDateTime;
    R.LastDate := FieldByName('last_date').AsDateTime;
    R.Description := FieldByName('description').AsString;
    R.Notes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNestRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNest;
begin
  if not (E is TNest) then
    raise Exception.Create('Insert: Expected TNest');

  R := TNest(E);
  Qry := NewQuery;
  with Qry, SQL do
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
      'nest_fate, ' +
      'loss_cause, ' +
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
      ':nest_fate, ' +
      ':loss_cause, ' +
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

    ParamByName('field_number').AsString := R.FieldNumber;
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetStrParam(ParamByName('nest_shape'), R.NestShape);
    SetStrParam(ParamByName('support_type'), R.SupportType);
    SetForeignParam(ParamByName('support_plant_1_id'), R.SupportPlant1Id);
    SetForeignParam(ParamByName('support_plant_2_id'), R.SupportPlant2Id);
    SetStrParam(ParamByName('other_support'), R.OtherSupport);
    ParamByName('height_above_ground').AsFloat := R.HeightAboveGround;
    SetFloatParam(ParamByName('internal_max_diameter'), R.InternalMaxDiameter);
    SetFloatParam(ParamByName('internal_min_diameter'), R.InternalMinDiameter);
    SetFloatParam(ParamByName('external_max_diameter'), R.ExternalMaxDiameter);
    SetFloatParam(ParamByName('external_min_diameter'), R.ExternalMinDiameter);
    SetFloatParam(ParamByName('internal_height'), R.InternalHeight);
    SetFloatParam(ParamByName('external_height'), R.ExternalHeight);
    SetFloatParam(ParamByName('edge_distance'), R.EdgeDistance);
    SetFloatParam(ParamByName('center_distance'), R.CenterDistance);
    ParamByName('nest_cover').AsFloat := R.NestCover;
    SetFloatParam(ParamByName('plant_max_diameter'), R.PlantMaxDiameter);
    SetFloatParam(ParamByName('plant_min_diameter'), R.PlantMinDiameter);
    SetFloatParam(ParamByName('plant_height'), R.PlantHeight);
    SetFloatParam(ParamByName('plant_dbh'), R.PlantDbh);
    ParamByName('nest_fate').AsString := NEST_FATES[R.NestFate];
    if R.NestFate = nfLoss then
      ParamByName('loss_cause').AsString := LOSS_CAUSES[R.LossCause]
    else
      ParamByName('loss_cause').Clear;
    ParamByName('nest_productivity').AsInteger := R.NestProductivity;
    SetDateParam(ParamByName('found_date'), R.FoundDate);
    SetDateParam(ParamByName('last_date'), R.LastDate);
    R.FullName := GetNestFullName(R.FoundDate, R.TaxonId, R.LocalityId, R.FieldNumber);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetFloatParam(ParamByName('construction_days'), R.ConstructionDays);
    SetFloatParam(ParamByName('incubation_days'), R.IncubationDays);
    SetFloatParam(ParamByName('nestling_days'), R.NestlingDays);
    SetFloatParam(ParamByName('active_days'), R.ActiveDays);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestRepository.TableName: string;
begin
  Result := TBL_NESTS;
end;

procedure TNestRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNest;
begin
  if not (E is TNest) then
    raise Exception.Create('Update: Expected TNest');

  R := TNest(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNestRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
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
    Add('  loss_cause = :loss_cause,');
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

    ParamByName('field_number').AsString := R.FieldNumber;
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetStrParam(ParamByName('nest_shape'), R.NestShape);
    SetStrParam(ParamByName('support_type'), R.SupportType);
    SetForeignParam(ParamByName('support_plant_1_id'), R.SupportPlant1Id);
    SetForeignParam(ParamByName('support_plant_2_id'), R.SupportPlant2Id);
    SetStrParam(ParamByName('other_support'), R.OtherSupport);
    ParamByName('height_above_ground').AsFloat := R.HeightAboveGround;
    SetFloatParam(ParamByName('internal_max_diameter'), R.InternalMaxDiameter);
    SetFloatParam(ParamByName('internal_min_diameter'), R.InternalMinDiameter);
    SetFloatParam(ParamByName('external_max_diameter'), R.ExternalMaxDiameter);
    SetFloatParam(ParamByName('external_min_diameter'), R.ExternalMinDiameter);
    SetFloatParam(ParamByName('internal_height'), R.InternalHeight);
    SetFloatParam(ParamByName('external_height'), R.ExternalHeight);
    SetFloatParam(ParamByName('edge_distance'), R.EdgeDistance);
    SetFloatParam(ParamByName('center_distance'), R.CenterDistance);
    ParamByName('nest_cover').AsFloat := R.NestCover;
    SetFloatParam(ParamByName('plant_max_diameter'), R.PlantMaxDiameter);
    SetFloatParam(ParamByName('plant_min_diameter'), R.PlantMinDiameter);
    SetFloatParam(ParamByName('plant_height'), R.PlantHeight);
    SetFloatParam(ParamByName('plant_dbh'), R.PlantDbh);
    ParamByName('nest_fate').AsString := NEST_FATES[R.NestFate];
    if R.NestFate = nfLoss then
      ParamByName('loss_cause').AsString := LOSS_CAUSES[R.LossCause]
    else
      ParamByName('loss_cause').Clear;
    ParamByName('nest_productivity').AsInteger := R.NestProductivity;
    SetDateParam(ParamByName('found_date'), R.FoundDate);
    SetDateParam(ParamByName('last_date'), R.LastDate);
    R.FullName := GetNestFullName(R.FoundDate, R.TaxonId, R.LocalityId, R.FieldNumber);
    SetStrParam(ParamByName('full_name'), R.FullName);
    SetStrParam(ParamByName('description'), R.Description);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetFloatParam(ParamByName('construction_days'), R.ConstructionDays);
    SetFloatParam(ParamByName('incubation_days'), R.IncubationDays);
    SetFloatParam(ParamByName('nestling_days'), R.NestlingDays);
    SetFloatParam(ParamByName('active_days'), R.ActiveDays);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('nest_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TNestOwner }

constructor TNestOwner.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TNestOwner.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNestOwner then
  begin
    FNestId := TNestOwner(Source).NestId;
    FRole := TNestOwner(Source).Role;
    FIndividualId := TNestOwner(Source).IndividualId;
  end;
end;

procedure TNestOwner.Clear;
begin
  inherited Clear;
  FNestId := 0;
  FRole := nrlUnknown;
  FIndividualId := 0;
end;

function TNestOwner.Clone: TXolmisRecord;
begin
  Result := TNestOwner(inherited Clone);
end;

function TNestOwner.Diff(const aOld: TNestOwner; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscRole, aOld.Role, FRole, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TNestOwner.EqualsTo(const Other: TNestOwner): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TNestOwner.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FNestId     := Obj.Get('nest_id', 0);
    case Obj.Get('role', '') of
      'M': FRole := nrlMale;
      'F': FRole := nrlFemale;
      'H': FRole := nrlHelper;
      'O': FRole := nrlOffspring;
    else
      FRole := nrlUnknown;
    end;
    FIndividualId := Obj.Get('individual_id', 0);
  finally
    Obj.Free;
  end;
end;

function TNestOwner.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('role', NEST_ROLES[FRole]);
    JSONObject.Add('individual_id', FIndividualId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TNestOwner.ToString: String;
begin
  Result := Format('NestOwner(Id=%d, NestId=%d, Role=%s, IndividualId=%d, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FNestId, NEST_ROLES[FRole], FIndividualId,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TNestOwner.Validate(out Msg: string): Boolean;
begin
  if FNestId = 0 then
  begin
    Msg := 'Nest required.';
    Exit(False);
  end;
  if FIndividualId = 0 then
  begin
    Msg := 'Individual required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TNestOwnerRepository }

procedure TNestOwnerRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNestOwner;
begin
  if not (E is TNestOwner) then
    raise Exception.Create('Delete: Expected TNestOwner');

  R := TNestOwner(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNestOwnerRepository.Delete: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := COL_NEST_OWNER_ID;
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestOwnerRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_NEST_OWNER_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestOwnerRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_NEST_OWNER_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TNestOwner) then
    raise Exception.Create('FindBy: Expected TNestOwner');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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
      Hydrate(Qry, TNestOwner(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TNestOwnerRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TNestOwner) then
    raise Exception.Create('GetById: Expected TNestOwner');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    Add('WHERE nest_owner_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TNestOwner(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNestOwnerRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TNestOwner;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TNestOwner) then
    raise Exception.Create('Hydrate: Expected TNestOwner');

  R := TNestOwner(E);
  with aDataSet do
  begin
    R.Id := FieldByName('nest_owner_id').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    case FieldByName('role').AsString of
      'M': R.Role := nrlMale;
      'F': R.Role := nrlFemale;
      'H': R.Role := nrlHelper;
      'O': R.Role := nrlOffspring;
    else
      R.Role := nrlUnknown;
    end;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TNestOwnerRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNestOwner;
begin
  if not (E is TNestOwner) then
    raise Exception.Create('Insert: Expected TNestOwner');

  R := TNestOwner(E);
  Qry := NewQuery;
  with Qry, SQL do
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

    ParamByName('nest_id').AsInteger := R.NestId;
    ParamByName('role').AsString := NEST_ROLES[R.Role];
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    ParamByName('user_inserted').AsInteger := ActiveUser.Id;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNestOwnerRepository.TableName: string;
begin
  Result := TBL_NEST_OWNERS;
end;

procedure TNestOwnerRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNestOwner;
begin
  if not (E is TNestOwner) then
    raise Exception.Create('Update: Expected TNestOwner');

  R := TNestOwner(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNestOwnerRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE nest_owners SET ' +
      'nest_id = :nest_id, ' +
      'role = :role, ' +
      'individual_id = :individual_id, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (nest_owner_id = :nest_owner_id)');

    ParamByName('nest_id').AsInteger := R.NestId;
    ParamByName('role').AsString := NEST_ROLES[R.Role];
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('nest_owner_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

