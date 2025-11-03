{ Xolmis Sampling Data library

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

unit models_sampling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

type

  { TExpedition }

  TExpedition = class(TXolmisRecord)
  protected
    FName: String;
    FStartDate: TDate;
    FEndDate: TDate;
    FProjectId: Integer;
    FDescription: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TExpedition; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TExpedition): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property ProjectId: Integer read FProjectId write FProjectId;
    property Description: String read FDescription write FDescription;
  end;

  { TExpeditionRepository }

  TExpeditionRepository = class(TXolmisRepository)
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

  { TSurvey }

  TSurvey = class(TXolmisRecord)
  protected
    FSurveyDate: TDate;
    FStartTime: TTime;
    FEndTime: TTime;
    FDuration: Integer;
    FMethodId: Integer;
    FNetStationId: Integer;
    FExpeditionId: Integer;
    FLocalityId: Integer;
    FProjectId: Integer;
    FSampleId: String;
    FStartLatitude: Extended;
    FStartLongitude: Extended;
    FEndLatitude: Extended;
    FEndLongitude: Extended;
    FObserversTally: Integer;
    FTotalArea: Double;
    FTotalDistance: Double;
    FTotalNets: Integer;
    FHabitat: String;
    FNetRounds: String;
    FFullName: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSurvey; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TSurvey): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SurveyDate: TDate read FSurveyDate write FSurveyDate;
    property StartTime: TTime read FStartTime write FStartTime;
    property EndTime: TTime read FEndTime write FEndTime;
    property Duration: Integer read FDuration write FDuration;
    property MethodId: Integer read FMethodId write FMethodId;
    property NetStationId: Integer read FNetStationId write FNetStationId;
    property ExpeditionId: Integer read FExpeditionId write FExpeditionId;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property SampleId: String read FSampleId write FSampleId;
    property StartLatitude: Extended read FStartLatitude write FStartLatitude;
    property StartLongitude: Extended read FStartLongitude write FStartLongitude;
    property EndLatitude: Extended read FEndLatitude write FEndLatitude;
    property EndLongitude: Extended read FEndLongitude write FEndLongitude;
    property ObserversTally: Integer read FObserversTally write FObserversTally;
    property TotalArea: Double read FTotalArea write FTotalArea;
    property TotalDistance: Double read FTotalDistance write FTotalDistance;
    property TotalNets: Integer read FTotalNets write FTotalNets;
    property Habitat: String read FHabitat write FHabitat;
    property NetRounds: String read FNetRounds write FNetRounds;
    property FullName: String read FFullName write FFullName;
    property Notes: String read FNotes write FNotes;
  end;

  { TSurveyRepository }

  TSurveyRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindBySiteAndDate(aLocal, aMethod: Integer; aDate: TDateTime; aSampleId: String; aNetStation: Integer; E: TSurvey);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TWeatherLog }

  TWeatherLog = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FAtmosphericPressure: Double;
    FCloudCover: Integer;
    FNotes: String;
    FPrecipitation: TPrecipitation;
    FRainfall: Integer;
    FRelativeHumidity: Double;
    FSampleDate: TDate;
    FSampleMoment: TWeatherSampleMoment;
    FSampleTime: TTime;
    FObserverId: Integer;
    FTemperature: Double;
    FWindSpeedBft: Integer;
    FWindSpeedKmH: Double;
    FWindDirection: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TWeatherLog; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TWeatherLog): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property SampleTime: TTime read FSampleTime write FSampleTime;
    property SampleMoment: TWeatherSampleMoment read FSampleMoment write FSampleMoment;
    property ObserverId: Integer read FObserverId write FObserverId;
    property CloudCover: Integer read FCloudCover write FCloudCover;
    property Precipitation: TPrecipitation read FPrecipitation write FPrecipitation;
    property Rainfall: Integer read FRainfall write FRainfall;
    property Temperature: Double read FTemperature write FTemperature;
    property WindSpeedBft: Integer read FWindSpeedBft write FWindSpeedBft;
    property WindSpeedKmH: Double read FWindSpeedKmH write FWindSpeedKmH;
    property WindDirection: String read FWindDirection write FWindDirection;
    property RelativeHumidity: Double read FRelativeHumidity write FRelativeHumidity;
    property AtmosphericPressure: Double read FAtmosphericPressure write FAtmosphericPressure;
    property Notes: String read FNotes write FNotes;
  end;

  { TWeatherLogRepository }

  TWeatherLogRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindBySurvey(aSurvey: Integer; aDate, aTime: String; aObserver: Integer; E: TWeatherLog);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TSurveyMember }

  TSurveyMember = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FPersonId: Integer;
    FVisitor: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSurveyMember; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TSurveyMember): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property PersonId: Integer read FPersonId write FPersonId;
    property Visitor: Boolean read FVisitor write FVisitor;
  end;

  { TSurveyMemberRepository }

  TSurveyMemberRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindBySurvey(const aSurveyKey, aPersonKey: Integer; E: TSurveyMember);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TNetEffort }

  TNetEffort = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FFullName: String;
    FNetStationId: Integer;
    FPermanentNetId: Integer;
    FNetNumber: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FSampleDate: TDate;
    FNetOpen1: TTime;
    FNetClose1: TTime;
    FNetOpen2: TTime;
    FNetClose2: TTime;
    FNetOpen3: TTime;
    FNetClose3: TTime;
    FNetOpen4: TTime;
    FNetClose4: TTime;
    FTotalOpenTime: Double;
    FNetLength: Double;
    FNetHeight: Double;
    FNetArea: Double;
    FNetMesh: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TNetEffort; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TNetEffort): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property FullName: String read FFullName write FFullName;
    property NetStationId: Integer read FNetStationId write FNetStationId;
    property PermanentNetId: Integer read FPermanentNetId write FPermanentNetId;
    property NetNumber: Integer read FNetNumber write FNetNumber;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property NetOpen1: TTime read FNetOpen1 write FNetOpen1;
    property NetClose1: TTime read FNetClose1 write FNetClose1;
    property NetOpen2: TTime read FNetOpen2 write FNetOpen2;
    property NetClose2: TTime read FNetClose2 write FNetClose2;
    property NetOpen3: TTime read FNetOpen3 write FNetOpen3;
    property NetClose3: TTime read FNetClose3 write FNetClose3;
    property NetOpen4: TTime read FNetOpen4 write FNetOpen4;
    property NetClose4: TTime read FNetClose4 write FNetClose4;
    property TotalOpenTime: Double read FTotalOpenTime write FTotalOpenTime;
    property NetLength: Double read FNetLength write FNetLength;
    property NetHeight: Double read FNetHeight write FNetHeight;
    property NetArea: Double read FNetArea write FNetArea;
    property NetMesh: Integer read FNetMesh write FNetMesh;
    property Notes: String read FNotes write FNotes;
  end;

  { TNetEffortRepository }

  TNetEffortRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindBySurvey(aSurvey: Integer; aNetNumber: String; E: TNetEffort);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

type

  { TVegetation }

  TVegetation = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FSampleDate: TDate;
    FSampleTime: TTime;
    FNotes: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FObserverId: Integer;
    FHerbsProportion: Integer;
    FHerbsDistribution: TStratumDistribution;
    FHerbsAvgHeight: Integer;
    FShrubsProportion: Integer;
    FShrubsDistribution: TStratumDistribution;
    FShrubsAvgHeight: Integer;
    FTreesProportion: Integer;
    FTreesDistribution: TStratumDistribution;
    FTreesAvgHeight: Integer;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TVegetation; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TVegetation): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property SampleTime: TTime read FSampleTime write FSampleTime;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property ObserverId: Integer read FObserverId write FObserverId;
    property HerbsProportion: Integer read FHerbsProportion write FHerbsProportion;
    property HerbsDistribution: TStratumDistribution read FHerbsDistribution write FHerbsDistribution;
    property HerbsAvgHeight: Integer read FHerbsAvgHeight write FHerbsAvgHeight;
    property ShrubsProportion: Integer read FShrubsProportion write FShrubsProportion;
    property ShrubsDistribution: TStratumDistribution read FShrubsDistribution write FShrubsDistribution;
    property ShrubsAvgHeight: Integer read FShrubsAvgHeight write FShrubsAvgHeight;
    property TreesProportion: Integer read FTreesProportion write FTreesProportion;
    property TreesDistribution: TStratumDistribution read FTreesDistribution write FTreesDistribution;
    property TreesAvgHeight: Integer read FTreesAvgHeight write FTreesAvgHeight;
    property Notes: String read FNotes write FNotes;
  end;

  { TVegetationRepository }

  TVegetationRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindBySurvey(aSurvey: Integer; aDate, aTime: String; aLongitude, aLatitude: Extended; aObserver: Integer; E: TVegetation);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  function AuthorListToString(aAuthors: TAuthors): String;
  procedure StringToAuthorList(const aCitation: String; var aAuthors: TAuthors);

implementation

uses
  utils_locale, utils_global, models_users, utils_validations, utils_fullnames, data_columns, data_consts,
  data_setparam, data_getvalue, udm_main;

function AuthorListToString(aAuthors: TAuthors): String;
var
  i: Integer;
  S: String;
begin
  S := '';
  for i := Low(aAuthors) to High(aAuthors) do
  begin
    if (i = 0) then
      S := aAuthors[i].Citation
    else
      S := S + '; ' + aAuthors[i].Citation;
  end;

  Result := S;
end;

procedure StringToAuthorList(const aCitation: String; var aAuthors: TAuthors);
var
  aLista: TStringList;
  i: Integer;
begin
  if Length(aCitation) > 0 then
  begin
    aLista := TStringList.Create;
    aLista.QuoteChar := #0;
    aLista.Delimiter := ';';
    aLista.StrictDelimiter := True;
    aLista.DelimitedText := aCitation;
    SetLength(aAuthors, aLista.Count);
    for i := 0 to aLista.Count - 1 do
    begin
      aAuthors[i].Citation := Trim(aLista[i]);
      with TSQLQuery.Create(DMM.sqlCon) do
      try
        Database := DMM.sqlCon;
        SQL.Add('SELECT person_id FROM people WHERE acronym = :abrev');
        ParamByName('ABREV').DataType := ftString;
        ParamByName('ABREV').AsString := aLista[i];
        Open;
        aAuthors[i].Id := Fields[0].AsInteger;
        Close;
      finally
        Free;
      end;
    end;
    aLista.Free;
  end;
end;

{ TExpedition }

constructor TExpedition.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TExpedition.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TExpedition then
  begin
    FName := TExpedition(Source).Name;
    FStartDate := TExpedition(Source).StartDate;
    FEndDate := TExpedition(Source).EndDate;
    FProjectId := TExpedition(Source).ProjectId;
    FDescription := TExpedition(Source).Description;
  end;
end;

procedure TExpedition.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FStartDate := NullDate;
  FEndDate := NullDate;
  FProjectId := 0;
  FDescription := EmptyStr;
end;

function TExpedition.Clone: TXolmisRecord;
begin
  Result := TExpedition(inherited Clone);
end;

function TExpedition.Diff(const aOld: TExpedition; var Changes: TStrings): Boolean;
var
  R: string;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStartDate, aOld.StartDate, FStartDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEndDate, aOld.EndDate, FEndDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TExpedition.EqualsTo(const Other: TExpedition): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TExpedition.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FName         := Obj.Get('name', '');
    FStartDate    := Obj.Get('start_date', NullDate);
    FEndDate      := Obj.Get('end_date', NullDate);
    FProjectId    := Obj.Get('project_id', 0);
    FDescription  := Obj.Get('description', '');
  finally
    Obj.Free;
  end;
end;

function TExpedition.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('name', FName);
    JSONObject.Add('start_date', FStartDate);
    JSONObject.Add('end_date', FEndDate);
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('description', FDescription);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TExpedition.ToString: String;
begin
  Result := Format('Expedition(Id=%d, Name=%s, StartDate=%s, EndDate=%s, ProjectId=%d, Description=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, DateToStr(FStartDate), DateToStr(FEndDate), FProjectId, FDescription,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TExpedition.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TExpeditionRepository }

procedure TExpeditionRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TExpedition;
begin
  if not (E is TExpedition) then
    raise Exception.Create('Delete: Expected TExpedition');

  R := TExpedition(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TExpeditionRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_EXPEDITION_ID;
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

function TExpeditionRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_EXPEDITION_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TExpeditionRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_EXPEDITION_ID, COL_EXPEDITION_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TExpedition) then
    raise Exception.Create('FindBy: Expected TExpedition');

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
        'expedition_id, ' +
        'expedition_name, ' +
        'start_date, ' +
        'end_date, ' +
        'duration, ' +
        'project_id, ' +
        'description, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM expeditions');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TExpedition(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TExpeditionRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TExpedition) then
    raise Exception.Create('GetById: Expected TExpedition');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'expedition_id, ' +
        'expedition_name, ' +
        'start_date, ' +
        'end_date, ' +
        'duration, ' +
        'project_id, ' +
        'description, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM expeditions');
    Add('WHERE expedition_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TExpedition(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TExpeditionRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TExpedition;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TExpedition) then
    raise Exception.Create('Hydrate: Expected TExpedition');

  R := TExpedition(E);
  with aDataSet do
  begin
    R.Id := FieldByName('expedition_id').AsInteger;
    R.Name := FieldByName('expedition_name').AsString;
    if not (FieldByName('start_date').IsNull) then
      R.StartDate := FieldByName('start_date').AsDateTime
    else
      R.StartDate := NullDate;
    if not (FieldByName('end_date').IsNull) then
      R.EndDate := FieldByName('end_date').AsDateTime
    else
      R.EndDate := NullDate;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.Description := FieldByName('description').AsString;
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

procedure TExpeditionRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TExpedition;
begin
  if not (E is TExpedition) then
    raise Exception.Create('Insert: Expected TExpedition');

  R := TExpedition(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO expeditions (' +
      'expedition_name, ' +
      'start_date, ' +
      'end_date, ' +
      'project_id, ' +
      'description, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':expedition_name, ' +
      'date(:start_date), ' +
      'date(:end_date), ' +
      ':project_id, ' +
      ':description, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    ParamByName('expedition_name').AsString := R.Name;
    SetDateParam(ParamByName('start_date'), R.StartDate);
    SetDateParam(ParamByName('end_date'), R.EndDate);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetStrParam(ParamByName('description'), R.Description);
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

function TExpeditionRepository.TableName: string;
begin
  Result := TBL_EXPEDITIONS;
end;

procedure TExpeditionRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TExpedition;
begin
  if not (E is TExpedition) then
    raise Exception.Create('Update: Expected TExpedition');

  R := TExpedition(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TExpeditionRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE expeditions SET ' +
        'expedition_name = :expedition_name, ' +
        'start_date = date(:start_date), ' +
        'end_date = date(:end_date), ' +
        'project_id = :project_id, ' +
        'description = :description, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (expedition_id = :expedition_id)');

    ParamByName('expedition_name').AsString := R.Name;
    SetDateParam(ParamByName('start_date'), R.StartDate);
    SetDateParam(ParamByName('end_date'), R.EndDate);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetStrParam(ParamByName('description'), R.Description);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('expedition_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TNetEffort }

constructor TNetEffort.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TNetEffort.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNetEffort then
  begin
    FFullName := TNetEffort(Source).FullName;
    FSurveyId := TNetEffort(Source).SurveyId;
    FNetStationId := TNetEffort(Source).NetStationId;
    FPermanentNetId := TNetEffort(Source).PermanentNetId;
    FNetNumber := TNetEffort(Source).NetNumber;
    FLatitude := TNetEffort(Source).Latitude;
    FLongitude := TNetEffort(Source).Longitude;
    FSampleDate := TNetEffort(Source).SampleDate;
    FNetOpen1 := TNetEffort(Source).NetOpen1;
    FNetClose1 := TNetEffort(Source).NetClose1;
    FNetOpen2 := TNetEffort(Source).NetOpen2;
    FNetClose2 := TNetEffort(Source).NetClose2;
    FNetOpen3 := TNetEffort(Source).NetOpen3;
    FNetClose3 := TNetEffort(Source).NetClose3;
    FNetOpen4 := TNetEffort(Source).NetOpen4;
    FNetClose4 := TNetEffort(Source).NetClose4;
    FTotalOpenTime := TNetEffort(Source).TotalOpenTime;
    FNetLength := TNetEffort(Source).NetLength;
    FNetHeight := TNetEffort(Source).FNetHeight;
    FNetArea := TNetEffort(Source).NetArea;
    FNetMesh := TNetEffort(Source).NetMesh;
    FNotes := TNetEffort(Source).Notes;
  end;
end;

procedure TNetEffort.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FSurveyId := 0;
  FNetStationId := 0;
  FPermanentNetId := 0;
  FNetNumber := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FSampleDate := StrToDate('30/12/1500');
  FNetOpen1 := StrToTime('00:00:00');
  FNetClose1 := StrToTime('00:00:00');
  FNetOpen2 := StrToTime('00:00:00');
  FNetClose2 := StrToTime('00:00:00');
  FNetOpen3 := StrToTime('00:00:00');
  FNetClose3 := StrToTime('00:00:00');
  FNetOpen4 := StrToTime('00:00:00');
  FNetClose4 := StrToTime('00:00:00');
  FTotalOpenTime := 0.0;
  FNetLength := 0.0;
  FNetHeight := 0.0;
  FNetArea := 0.0;
  FNetMesh := 0;
  FNotes := EmptyStr;
end;

function TNetEffort.Clone: TXolmisRecord;
begin
  Result := TNetEffort(inherited Clone);
end;

function TNetEffort.Diff(const aOld: TNetEffort; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.NetStationId, FNetStationId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPermanentNetID, aOld.PermanentNetId, FPermanentNetId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnetNr, aOld.NetNumber, FNetNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOpenTime1, aOld.NetOpen1, FNetOpen1, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloseTime1, aOld.NetClose1, FNetClose1, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOpenTime2, aOld.NetOpen2, FNetOpen2, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloseTime2, aOld.NetClose2, FNetClose2, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOpenTime3, aOld.NetOpen3, FNetOpen3, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloseTime3, aOld.NetClose3, FNetClose3, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOpenTime4, aOld.NetOpen4, FNetOpen4, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloseTime4, aOld.NetClose4, FNetClose4, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnetLengthM, aOld.NetLength, FNetLength, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnetHeightM, aOld.NetHeight, FNetHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnetMesh, aOld.NetMesh, FNetMesh, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TNetEffort.EqualsTo(const Other: TNetEffort): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TNetEffort.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName       := Obj.Get('full_name', '');
    FSurveyId       := Obj.Get('survey_id', 0);
    FNetStationId   := Obj.Get('net_station_id', 0);
    FPermanentNetId := Obj.Get('permanent_net_id', 0);
    FNetNumber      := Obj.Get('net_number', 0);
    FLongitude      := Obj.Get('longitude', 0.0);
    FLatitude       := Obj.Get('latitude', 0.0);
    FSampleDate     := Obj.Get('sample_date', NullDate);
    FNetOpen1       := Obj.Get('open_time_1', NullTime);
    FNetClose1      := Obj.Get('close_time_1', NullTime);
    FNetOpen2       := Obj.Get('open_time_2', NullTime);
    FNetClose2      := Obj.Get('close_time_2', NullTime);
    FNetOpen3       := Obj.Get('open_time_3', NullTime);
    FNetClose3      := Obj.Get('close_time_3', NullTime);
    FNetOpen4       := Obj.Get('open_time_4', NullTime);
    FNetClose4      := Obj.Get('close_time_4', NullTime);
    FTotalOpenTime  := Obj.Get('total_open_time', 0.0);
    FNetLength      := Obj.Get('net_length', 0.0);
    FNetHeight      := Obj.Get('net_height', 0.0);
    FNetArea        := Obj.Get('net_area', 0.0);
    FNetMesh        := Obj.Get('net_mesh', 0);
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TNetEffort.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('net_station_id', FNetStationId);
    JSONObject.Add('permanent_net_id', FPermanentNetId);
    JSONObject.Add('net_number', FNetNumber);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('sample_date', FSampleDate);
    JSONObject.Add('open_time_1', FNetOpen1);
    JSONObject.Add('close_time_1', FNetClose1);
    JSONObject.Add('open_time_2', FNetOpen2);
    JSONObject.Add('close_time_2', FNetClose2);
    JSONObject.Add('open_time_3', FNetOpen3);
    JSONObject.Add('close_time_3', FNetClose3);
    JSONObject.Add('open_time_4', FNetOpen4);
    JSONObject.Add('close_time_4', FNetClose4);
    JSONObject.Add('total_open_time', FTotalOpenTime);
    JSONObject.Add('net_length', FNetLength);
    JSONObject.Add('net_height', FNetHeight);
    JSONObject.Add('net_area', FNetArea);
    JSONObject.Add('net_mesh', FNetMesh);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TNetEffort.ToString: String;
begin
  Result := Format('NetEffort(Id=%d, FullName=%s, SurveyId=%d, NetStationId=%d, PermanentNetId=%d, NetNumber=%d, ' +
    'Longitude=%f, Latitude=%f, SampleDate=%s, NetOpen1=%s, NetClose1=%s, NetOpen2=%s, NetClose2=%s, ' +
    'NetOpen3=%s, NetClose3=%s, NetOpen4=%s, NetClose4=%s, TotalOpenTime=%f, NetLength=%f, NetHeight=%f, ' +
    'NetArea=%f, NetMesh=%d, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FSurveyId, FNetStationId, FPermanentNetId, FNetNumber, FLongitude, FLatitude,
    DateToStr(FSampleDate), TimeToStr(FNetOpen1), TimeToStr(FNetClose1), TimeToStr(FNetOpen2), TimeToStr(FNetClose2),
    TimeToStr(FNetOpen3), TimeToStr(FNetClose3), TimeToStr(FNetOpen4), TimeToStr(FNetClose4),
    FTotalOpenTime, FNetLength, FNetHeight, FNetArea, FNetMesh, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TNetEffort.Validate(out Msg: string): Boolean;
begin
  if FSurveyId = 0 then
  begin
    Msg := 'Survey required.';
    Exit(False);
  end;
  if FNetNumber = 0 then
  begin
    Msg := 'Net number required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TNetEffortRepository }

procedure TNetEffortRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNetEffort;
begin
  if not (E is TNetEffort) then
    raise Exception.Create('Delete: Expected TNetEffort');

  R := TNetEffort(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNetEffortRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_NET_ID;
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

function TNetEffortRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_NET_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNetEffortRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_NET_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TNetEffort) then
    raise Exception.Create('FindBy: Expected TNetEffort');

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
        'net_id, ' +
        'survey_id, ' +
        'net_station_id, ' +
        'permanent_net_id, ' +
        'net_number, ' +
        'longitude, ' +
        'latitude, ' +
        'sample_date, ' +
        'net_open_1, ' +
        'net_close_1, ' +
        'net_open_2, ' +
        'net_close_2, ' +
        'net_open_3, ' +
        'net_close_3, ' +
        'net_open_4, ' +
        'net_close_4, ' +
        'open_time_total, ' +
        'net_length, ' +
        'net_height, ' +
        'net_area, ' +
        'net_mesh, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM nets_effort');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TNetEffort(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TNetEffortRepository.FindBySurvey(aSurvey: Integer; aNetNumber: String; E: TNetEffort);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM nets_effort');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (net_number = :anet)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('ANET').AsInteger := StrToInt(aNetNumber);
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

procedure TNetEffortRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TNetEffort) then
    raise Exception.Create('GetById: Expected TNetEffort');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'net_id, ' +
        'survey_id, ' +
        'net_station_id, ' +
        'permanent_net_id, ' +
        'net_number, ' +
        'longitude, ' +
        'latitude, ' +
        'sample_date, ' +
        'net_open_1, ' +
        'net_close_1, ' +
        'net_open_2, ' +
        'net_close_2, ' +
        'net_open_3, ' +
        'net_close_3, ' +
        'net_open_4, ' +
        'net_close_4, ' +
        'open_time_total, ' +
        'net_length, ' +
        'net_height, ' +
        'net_area, ' +
        'net_mesh, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM nets_effort');
    Add('WHERE net_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TNetEffort(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNetEffortRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TNetEffort;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TNetEffort) then
    raise Exception.Create('Hydrate: Expected TNetEffort');

  R := TNetEffort(E);
  with aDataSet do
  begin
    R.Id := FieldByName('net_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.NetStationId := FieldByName('net_station_id').AsInteger;
    R.PermanentNetId := FieldByName('permanent_net_id').AsInteger;
    R.NetNumber := FieldByName('net_number').AsInteger;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.SampleDate := FieldByName('sample_date').AsDateTime;
    R.NetOpen1 := FieldByName('net_open_1').AsDateTime;
    R.NetClose1 := FieldByName('net_close_1').AsDateTime;
    R.NetOpen2 := FieldByName('net_open_2').AsDateTime;
    R.NetClose2 := FieldByName('net_close_2').AsDateTime;
    R.NetOpen3 := FieldByName('net_open_3').AsDateTime;
    R.NetClose3 := FieldByName('net_close_3').AsDateTime;
    R.NetOpen4 := FieldByName('net_open_4').AsDateTime;
    R.NetClose4 := FieldByName('net_close_4').AsDateTime;
    R.TotalOpenTime := FieldByName('open_time_total').AsFloat;
    R.NetLength := FieldByName('net_length').AsFloat;
    R.NetHeight := FieldByName('net_height').AsFloat;
    R.NetArea := FieldByName('net_area').AsFloat;
    R.NetMesh := FieldByName('net_mesh').AsInteger;
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

procedure TNetEffortRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNetEffort;
begin
  if not (E is TNetEffort) then
    raise Exception.Create('Insert: Expected TNetEffort');

  R := TNetEffort(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO nets_effort (' +
      'survey_id, ' +
      'net_station_id, ' +
      'permanent_net_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'sample_date, ' +
      'net_open_1, ' +
      'net_close_1, ' +
      'net_open_2, ' +
      'net_close_2, ' +
      'net_open_3, ' +
      'net_close_3, ' +
      'net_open_4, ' +
      'net_close_4, ' +
      'net_length, ' +
      'net_height, ' +
      'net_mesh, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':survey_id, ' +
      ':net_station_id, ' +
      ':permanent_net_id, ' +
      ':net_number, ' +
      ':longitude, ' +
      ':latitude, ' +
      'date(:sample_date), ' +
      'time(:net_open_1), ' +
      'time(:net_close_1), ' +
      'time(:net_open_2), ' +
      'time(:net_close_2), ' +
      'time(:net_open_3), ' +
      'time(:net_close_3), ' +
      'time(:net_open_4), ' +
      'time(:net_close_4), ' +
      ':net_length, ' +
      ':net_height, ' +
      ':net_mesh, ' +
      ':full_name, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''));');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    ParamByName('net_number').AsInteger := R.NetNumber;
    ParamByName('survey_id').AsInteger := R.SurveyId;
    ParamByName('net_station_id').AsInteger := R.NetStationId;
    SetForeignParam(ParamByName('permanent_net_id'), R.PermanentNetId);
    ParamByName('full_name').AsString := GetNetEffortFullname(R.SampleDate, R.NetStationId, R.NetNumber);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetFloatParam(ParamByName('net_length'), R.NetLength);
    SetFloatParam(ParamByName('net_height'), R.NetHeight);
    SetIntParam(ParamByName('net_mesh'), R.NetMesh);
    ParamByName('notes').AsString := R.Notes;

    SetTimeParam(ParamByName('net_open_1'), R.NetOpen1);
    SetTimeParam(ParamByName('net_close_1'), R.NetClose1);
    SetTimeParam(ParamByName('net_open_2'), R.NetOpen2);
    SetTimeParam(ParamByName('net_close_2'), R.NetClose2);
    SetTimeParam(ParamByName('net_open_3'), R.NetOpen3);
    SetTimeParam(ParamByName('net_close_3'), R.NetClose3);
    SetTimeParam(ParamByName('net_open_4'), R.NetOpen4);
    SetTimeParam(ParamByName('net_close_4'), R.NetClose4);

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

function TNetEffortRepository.TableName: string;
begin
  Result := TBL_NETS_EFFORT;
end;

procedure TNetEffortRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TNetEffort;
begin
  if not (E is TNetEffort) then
    raise Exception.Create('Update: Expected TNetEffort');

  R := TNetEffort(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TNetEffortRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE nets_effort SET ' +
      'survey_id = :survey_id, ' +
      'net_station_id = :net_station_id, ' +
      'permanent_net_id = :permanent_net_id, ' +
      'net_number = :net_number, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'sample_date = :sample_date, ' +
      'net_open_1 = :net_open_1, ' +
      'net_close_1 = :net_close_1, ' +
      'net_open_2 = :net_open_2, ' +
      'net_close_2 = :net_close_2, ' +
      'net_open_3 = :net_open_3, ' +
      'net_close_3 = :net_close_3, ' +
      'net_open_4 = :net_open_4, ' +
      'net_close_4 = :net_close_4, ' +
      'net_length = :net_length, ' +
      'net_height = :net_height, ' +
      'net_mesh = :net_mesh, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (net_id = :net_id)');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    ParamByName('net_number').AsInteger := R.NetNumber;
    ParamByName('survey_id').AsInteger := R.SurveyId;
    ParamByName('net_station_id').AsInteger := R.NetStationId;
    SetForeignParam(ParamByName('permanent_net_id'), R.PermanentNetId);
    ParamByName('full_name').AsString := GetNetEffortFullname(R.SampleDate, R.NetStationId, R.NetNumber);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetFloatParam(ParamByName('net_length'), R.NetLength);
    SetFloatParam(ParamByName('net_height'), R.NetHeight);
    SetIntParam(ParamByName('net_mesh'), R.NetMesh);
    ParamByName('notes').AsString := R.Notes;

    SetTimeParam(ParamByName('net_open_1'), R.NetOpen1);
    SetTimeParam(ParamByName('net_close_1'), R.NetClose1);
    SetTimeParam(ParamByName('net_open_2'), R.NetOpen2);
    SetTimeParam(ParamByName('net_close_2'), R.NetClose2);
    SetTimeParam(ParamByName('net_open_3'), R.NetOpen3);
    SetTimeParam(ParamByName('net_close_3'), R.NetClose3);
    SetTimeParam(ParamByName('net_open_4'), R.NetOpen4);
    SetTimeParam(ParamByName('net_close_4'), R.NetClose4);

    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('net_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TVegetation }

constructor TVegetation.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TVegetation.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVegetation then
  begin
    FSurveyId := TVegetation(Source).SurveyId;
    FSampleDate := TVegetation(Source).SampleDate;
    FSampleTime := TVegetation(Source).SampleTime;
    FNotes := TVegetation(Source).Notes;
    FLongitude := TVegetation(Source).Longitude;
    FLatitude := TVegetation(Source).Latitude;
    FObserverId := TVegetation(Source).ObserverId;
    FHerbsProportion := TVegetation(Source).HerbsProportion;
    FHerbsDistribution := TVegetation(Source).HerbsDistribution;
    FHerbsAvgHeight := TVegetation(Source).HerbsAvgHeight;
    FShrubsProportion := TVegetation(Source).ShrubsProportion;
    FShrubsDistribution := TVegetation(Source).ShrubsDistribution;
    FShrubsAvgHeight := TVegetation(Source).ShrubsAvgHeight;
    FTreesProportion := TVegetation(Source).TreesProportion;
    FTreesDistribution := TVegetation(Source).TreesDistribution;
    FTreesAvgHeight := TVegetation(Source).TreesAvgHeight;
  end;
end;

procedure TVegetation.Clear;
begin
  inherited Clear;
  FSurveyId := 0;
  FSampleDate := NullDate;
  FSampleTime := NullTime;
  FNotes := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FObserverId := 0;
  FHerbsProportion := 0;
  FHerbsDistribution := disNone;
  FHerbsAvgHeight := 0;
  FShrubsProportion := 0;
  FShrubsDistribution := disNone;
  FShrubsAvgHeight := 0;
  FTreesProportion := 0;
  FTreesDistribution := disNone;
  FTreesAvgHeight := 0;
end;

function TVegetation.Clone: TXolmisRecord;
begin
  Result := TVegetation(inherited Clone);
end;

function TVegetation.Diff(const aOld: TVegetation; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProportionOfHerbs, aOld.HerbsProportion, FHerbsProportion, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHerbsDistribution, aOld.HerbsDistribution, FHerbsDistribution, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAvgHeightOfHerbs, aOld.HerbsAvgHeight, FHerbsAvgHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProportionOfShrubs, aOld.ShrubsProportion, FShrubsProportion, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscShrubsDistribution, aOld.ShrubsDistribution, FShrubsDistribution, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAvgHeightOfShrubs, aOld.ShrubsAvgHeight, FShrubsAvgHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProportionOfTrees, aOld.TreesProportion, FTreesProportion, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTreesDistribution, aOld.TreesDistribution, FTreesDistribution, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAvgHeightOfTrees, aOld.TreesAvgHeight, FTreesAvgHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TVegetation.EqualsTo(const Other: TVegetation): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TVegetation.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSurveyId           := Obj.Get('survey_id', 0);
    FSampleDate         := Obj.Get('sample_date', NullDate);
    FSampleTime         := Obj.Get('sample_time', NullTime);
    FLongitude          := Obj.Get('longitude', 0.0);
    FLatitude           := Obj.Get('latitude', 0.0);
    FObserverId         := Obj.Get('observer_id', 0);
    FHerbsProportion    := Obj.Get('herbs_proportion', 0);
    FHerbsDistribution  := TStratumDistribution(Obj.Get('herbs_distribution', 0));
    FHerbsAvgHeight     := Obj.Get('herbs_avg_height', 0);
    FShrubsProportion   := Obj.Get('shrubs_proportion', 0);
    FShrubsDistribution := TStratumDistribution(Obj.Get('shrubs_distribution', 0));
    FShrubsAvgHeight    := Obj.Get('shrubs_avg_height', 0);
    FTreesProportion    := Obj.Get('trees_proportion', 0);
    FTreesDistribution  := TStratumDistribution(Obj.Get('trees_distribution', 0));
    FTreesAvgHeight     := Obj.Get('trees_avg_height', 0);
    FNotes              := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TVegetation.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('sample_date', FSampleDate);
    JSONObject.Add('sample_time', FSampleTime);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('observer_id', FObserverId);
    JSONObject.Add('herbs_distribution', Ord(FHerbsDistribution));
    JSONObject.Add('herbs_proportion', FHerbsProportion);
    JSONObject.Add('herbs_avg_height', FHerbsAvgHeight);
    JSONObject.Add('shrubs_distribution', Ord(FShrubsDistribution));
    JSONObject.Add('shrubs_proportion', FShrubsProportion);
    JSONObject.Add('shrubs_avg_height', FShrubsAvgHeight);
    JSONObject.Add('trees_distribution', Ord(FTreesDistribution));
    JSONObject.Add('trees_proportion', FTreesProportion);
    JSONObject.Add('trees_avg_height', FTreesAvgHeight);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TVegetation.ToString: String;
begin
  Result := Format('Vegetation(Id=%d, SurveyId=%d, SampleDate=%s, SampleTime=%s, Longitude=%f, Latitude=%f, ' +
    'ObserverId=%d, HerbsProportion=%d, HerbsDistribution=%d, HerbsAvgHeight=%d, ShrubsProportion=%d, ' +
    'ShrubsDistribution=%d, ShrubsAvgHeight=%d, TreesProportion=%d, TreesDistribution=%d, TreesAvgHeight=%d, ' +
    'Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FSurveyId, DateToStr(FSampleDate), TimeToStr(FSampleTime), FLongitude, FLatitude, FObserverId,
    FHerbsProportion, Ord(FHerbsDistribution), FHerbsAvgHeight, FShrubsProportion, Ord(FShrubsDistribution),
    FShrubsAvgHeight, FTreesProportion, Ord(FTreesDistribution), FTreesAvgHeight, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TVegetation.Validate(out Msg: string): Boolean;
begin
  if FSurveyId = 0 then
  begin
    Msg := 'Survey required.';
    Exit(False);
  end;
  if FSampleDate = NullDate then
  begin
    Msg := 'Sample date required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TVegetationRepository }

procedure TVegetationRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVegetation;
begin
  if not (E is TVegetation) then
    raise Exception.Create('Delete: Expected TVegetation');

  R := TVegetation(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVegetationRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_VEGETATION_ID;
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

function TVegetationRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_VEGETATION_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVegetationRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_VEGETATION_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TVegetation) then
    raise Exception.Create('FindBy: Expected TVegetation');

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
      'vegetation_id, ' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'longitude, ' +
      'latitude, ' +
      'observer_id, ' +
      'herbs_proportion, ' +
      'herbs_distribution, ' +
      'herbs_avg_height, ' +
      'shrubs_proportion, ' +
      'shrubs_distribution, ' +
      'shrubs_avg_height, ' +
      'trees_proportion, ' +
      'trees_distribution, ' +
      'trees_avg_height, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM vegetation');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TVegetation(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TVegetationRepository.FindBySurvey(aSurvey: Integer; aDate, aTime: String; aLongitude,
  aLatitude: Extended; aObserver: Integer; E: TVegetation);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM vegetation');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (date(sample_date) = date(:adate))');
    Add('AND (time(sample_time) = time(:atime))');
    Add('AND (longitude = :alongitude)');
    Add('AND (latitude = :alatitude)');
    Add('AND (observer_id = :aobserver)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('AOBSERVER').AsInteger := aObserver;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('ATIME').AsString := aTime;
    ParamByName('ALONGITUDE').AsFloat := aLongitude;
    ParamByName('ALATITUDE').AsFloat := aLatitude;
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

procedure TVegetationRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TVegetation) then
    raise Exception.Create('GetById: Expected TVegetation');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'vegetation_id, ' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'longitude, ' +
      'latitude, ' +
      'observer_id, ' +
      'herbs_proportion, ' +
      'herbs_distribution, ' +
      'herbs_avg_height, ' +
      'shrubs_proportion, ' +
      'shrubs_distribution, ' +
      'shrubs_avg_height, ' +
      'trees_proportion, ' +
      'trees_distribution, ' +
      'trees_avg_height, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM vegetation');
    Add('WHERE vegetation_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TVegetation(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVegetationRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TVegetation;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TVegetation) then
    raise Exception.Create('Hydrate: Expected TVegetation');

  R := TVegetation(E);
  with aDataSet do
  begin
    R.Id := FieldByName('vegetation_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.SampleDate := FieldByName('sample_date').AsDateTime;
    R.SampleTime := FieldByName('sample_time').AsDateTime;
    R.Notes := FieldByName('notes').AsString;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.ObserverId := FieldByName('observer_id').AsInteger;
    R.HerbsProportion := FieldByName('herbs_proportion').AsInteger;
    R.HerbsDistribution := TStratumDistribution(FieldByName('herbs_distribution').AsInteger);
    R.HerbsAvgHeight := FieldByName('herbs_avg_height').AsInteger;
    R.ShrubsProportion := FieldByName('shrubs_proportion').AsInteger;
    R.ShrubsDistribution := TStratumDistribution(FieldByName('shrubs_distribution').AsInteger);
    R.ShrubsAvgHeight := FieldByName('shrubs_avg_height').AsInteger;
    R.TreesProportion := FieldByName('trees_proportion').AsInteger;
    R.TreesDistribution := TStratumDistribution(FieldByName('trees_distribution').AsInteger);
    R.TreesAvgHeight := FieldByName('trees_avg_height').AsInteger;
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

procedure TVegetationRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVegetation;
begin
  if not (E is TVegetation) then
    raise Exception.Create('Insert: Expected TVegetation');

  R := TVegetation(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO vegetation (' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'longitude, ' +
      'latitude, ' +
      'observer_id, ' +
      'herbs_proportion, ' +
      'herbs_distribution, ' +
      'herbs_avg_height, ' +
      'shrubs_proportion, ' +
      'shrubs_distribution, ' +
      'shrubs_avg_height, ' +
      'trees_proportion, ' +
      'trees_distribution, ' +
      'trees_avg_height, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':survey_id, ' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':longitude, ' +
      ':latitude, ' +
      ':observer_id, ' +
      ':herbs_proportion, ' +
      ':herbs_distribution, ' +
      ':herbs_avg_height, ' +
      ':shrubs_proportion, ' +
      ':shrubs_distribution, ' +
      ':shrubs_avg_height, ' +
      ':trees_proportion, ' +
      ':trees_distribution, ' +
      ':trees_avg_height, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    ParamByName('survey_id').AsInteger := R.SurveyId;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetStrParam(ParamByName('notes'), R.Notes);

    ParamByName('herbs_proportion').AsInteger := R.HerbsProportion;
    ParamByName('herbs_distribution').AsInteger := Ord(R.HerbsDistribution);
    ParamByName('herbs_avg_height').AsInteger := R.HerbsAvgHeight;
    ParamByName('shrubs_proportion').AsInteger := R.ShrubsProportion;
    ParamByName('shrubs_distribution').AsInteger := Ord(R.ShrubsDistribution);
    ParamByName('shrubs_avg_height').AsInteger := R.ShrubsAvgHeight;
    ParamByName('trees_proportion').AsInteger := R.TreesProportion;
    ParamByName('trees_distribution').AsInteger := Ord(R.TreesDistribution);
    ParamByName('trees_avg_height').AsInteger := R.TreesAvgHeight;

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

function TVegetationRepository.TableName: string;
begin
  Result := TBL_VEGETATION;
end;

procedure TVegetationRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVegetation;
begin
  if not (E is TVegetation) then
    raise Exception.Create('Update: Expected TVegetation');

  R := TVegetation(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVegetationRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE vegetation SET ' +
      'survey_id = :survey_id, ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'observer_id = :observer_id, ' +
      'herbs_proportion = :herbs_proportion, ' +
      'herbs_distribution = :herbs_distribution, ' +
      'herbs_avg_height = :herbs_avg_height, ' +
      'shrubs_proportion = :shrubs_proportion, ' +
      'shrubs_distribution = :shrubs_distribution, ' +
      'shrubs_avg_height = :shrubs_avg_height, ' +
      'trees_proportion = :trees_proportion, ' +
      'trees_distribution = :trees_distribution, ' +
      'trees_avg_height = :trees_avg_height, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (vegetation_id = :vegetation_id)');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    ParamByName('survey_id').AsInteger := R.SurveyId;
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetStrParam(ParamByName('notes'), R.Notes);

    ParamByName('herbs_proportion').AsInteger := R.HerbsProportion;
    ParamByName('herbs_distribution').AsInteger := Ord(R.HerbsDistribution);
    ParamByName('herbs_avg_height').AsInteger := R.HerbsAvgHeight;
    ParamByName('shrubs_proportion').AsInteger := R.ShrubsProportion;
    ParamByName('shrubs_distribution').AsInteger := Ord(R.ShrubsDistribution);
    ParamByName('shrubs_avg_height').AsInteger := R.ShrubsAvgHeight;
    ParamByName('trees_proportion').AsInteger := R.TreesProportion;
    ParamByName('trees_distribution').AsInteger := Ord(R.TreesDistribution);
    ParamByName('trees_avg_height').AsInteger := R.TreesAvgHeight;

    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('vegetation_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSurveyMember }

constructor TSurveyMember.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSurveyMember.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSurveyMember then
  begin
    FSurveyId := TSurveyMember(Source).SurveyId;
    FPersonId := TSurveyMember(Source).PersonId;
    FVisitor := TSurveyMember(Source).Visitor;
  end;
end;

procedure TSurveyMember.Clear;
begin
  inherited Clear;
  FSurveyId := 0;
  FPersonId := 0;
  FVisitor := False;
end;

function TSurveyMember.Clone: TXolmisRecord;
begin
  Result := TSurveyMember(inherited Clone);
end;

function TSurveyMember.Diff(const aOld: TSurveyMember; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscVisitor, aOld.Visitor, FVisitor, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSurveyMember.EqualsTo(const Other: TSurveyMember): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSurveyMember.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSurveyId := Obj.Get('survey_id', 0);
    FPersonId := Obj.Get('person_id', 0);
    FVisitor  := Obj.Get('visitor', False);
  finally
    Obj.Free;
  end;
end;

function TSurveyMember.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('person_id', FPersonId);
    JSONObject.Add('visitor', FVisitor);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSurveyMember.ToString: String;
begin
  Result := Format('SurveyMember(Id=%d, SurveyId=%d, PersonId=%d, Visitor=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FSurveyId, FPersonId, BoolToStr(FVisitor, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSurveyMember.Validate(out Msg: string): Boolean;
begin
  if FSurveyId = 0 then
  begin
    Msg := 'Survey required.';
    Exit(False);
  end;
  if FPersonId = 0 then
  begin
    Msg := 'Person required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSurveyMemberRepository }

procedure TSurveyMemberRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSurveyMember;
begin
  if not (E is TSurveyMember) then
    raise Exception.Create('Delete: Expected TSurveyMember');

  R := TSurveyMember(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSurveyMemberRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SURVEY_MEMBER_ID;
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

function TSurveyMemberRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SURVEY_MEMBER_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurveyMemberRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_SURVEY_MEMBER_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSurveyMember) then
    raise Exception.Create('FindBy: Expected TSurveyMember');

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
        'survey_member_id, ' +
        'survey_id, ' +
        'person_id, ' +
        'visitor, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM survey_team');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSurveyMember(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSurveyMemberRepository.FindBySurvey(const aSurveyKey, aPersonKey: Integer; E: TSurveyMember);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM survey_team');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (person_id = :aperson)');
    ParamByName('asurvey').AsInteger := aSurveyKey;
    ParamByName('aperson').AsInteger := aPersonKey;
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

procedure TSurveyMemberRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSurveyMember) then
    raise Exception.Create('GetById: Expected TSurveyMember');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'survey_member_id, ' +
        'survey_id, ' +
        'person_id, ' +
        'visitor, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM survey_team');
    Add('WHERE survey_member_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSurveyMember(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurveyMemberRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSurveyMember;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSurveyMember) then
    raise Exception.Create('Hydrate: Expected TSurveyMember');

  R := TSurveyMember(E);
  with aDataSet do
  begin
    R.Id := FieldByName('survey_member_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.PersonId := FieldByName('person_id').AsInteger;
    R.Visitor := FieldByName('visitor').AsBoolean;
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

procedure TSurveyMemberRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSurveyMember;
begin
  if not (E is TSurveyMember) then
    raise Exception.Create('Insert: Expected TSurveyMember');

  R := TSurveyMember(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO survey_team (' +
      'survey_id, ' +
      'person_id, ' +
      'visitor, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':survey_id, ' +
      ':person_id, ' +
      ':visitor, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    ParamByName('survey_id').AsInteger := R.SurveyId;
    ParamByName('person_id').AsInteger := R.PersonId;
    ParamByName('visitor').AsBoolean := R.Visitor;
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

function TSurveyMemberRepository.TableName: string;
begin
  Result := TBL_SURVEY_TEAM;
end;

procedure TSurveyMemberRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSurveyMember;
begin
  if not (E is TSurveyMember) then
    raise Exception.Create('Update: Expected TSurveyMember');

  R := TSurveyMember(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSurveyMemberRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE survey_team SET ' +
      'survey_id = :survey_id, ' +
      'person_id = :person_id, ' +
      'visitor = :visitor, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec''),' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (survey_member_id = :survey_member_id)');

    ParamByName('survey_id').AsInteger := R.SurveyId;
    ParamByName('person_id').AsInteger := R.PersonId;
    ParamByName('visitor').AsBoolean := R.Visitor;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('survey_member_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSurvey }

constructor TSurvey.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSurvey.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSurvey then
  begin
    FSurveyDate := TSurvey(Source).SurveyDate;
    FStartTime := TSurvey(Source).StartTime;
    FEndTime := TSurvey(Source).EndTime;
    FDuration := TSurvey(Source).Duration;
    FMethodId := TSurvey(Source).MethodId;
    FNetStationId := TSurvey(Source).NetStationId;
    FExpeditionId := TSurvey(Source).ExpeditionId;
    FLocalityId := TSurvey(Source).LocalityId;
    FProjectId := TSurvey(Source).ProjectId;
    FSampleId := TSurvey(Source).SampleId;
    FStartLatitude := TSurvey(Source).StartLatitude;
    FStartLongitude := TSurvey(Source).StartLongitude;
    FEndLatitude := TSurvey(Source).EndLatitude;
    FEndLongitude := TSurvey(Source).EndLongitude;
    FObserversTally := TSurvey(Source).ObserversTally;
    FTotalArea := TSurvey(Source).TotalArea;
    FTotalDistance := TSurvey(Source).TotalDistance;
    FTotalNets := TSurvey(Source).TotalNets;
    FHabitat := TSurvey(Source).Habitat;
    FNetRounds := TSurvey(Source).NetRounds;
    FFullName := TSurvey(Source).FullName;
    FNotes := TSurvey(Source).Notes;
  end;
end;

procedure TSurvey.Clear;
begin
  inherited;
  FSurveyDate := NullDate;
  FStartTime := NullTime;
  FEndTime := NullTime;
  FDuration := 0;
  FMethodId := 0;
  FNetStationId := 0;
  FExpeditionId := 0;
  FLocalityId := 0;
  FProjectId := 0;
  FSampleId := EmptyStr;
  FStartLatitude := 0.0;
  FStartLongitude := 0.0;
  FEndLatitude := 0.0;
  FEndLongitude := 0.0;
  FObserversTally := 0;
  FTotalArea := 0.0;
  FTotalDistance := 0.0;
  FTotalNets := 0;
  FHabitat := EmptyStr;
  FNetRounds := EmptyStr;
  FFullName := EmptyStr;
  FNotes := EmptyStr;
end;

function TSurvey.Clone: TXolmisRecord;
begin
  Result := TSurvey(inherited Clone);
end;

function TSurvey.Diff(const aOld: TSurvey; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.SurveyDate, FSurveyDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscStartTime, aOld.StartTime, FStartTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEndTime, aOld.EndTime, FEndTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDurationMin, aOld.Duration, FDuration, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.NetStationId, FNetStationId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExpeditionID, aOld.ExpeditionId, FExpeditionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSampleID, aOld.SampleId, FSampleId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.StartLatitude, FStartLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.StartLongitude, FStartLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEndLatitude, aOld.EndLatitude, FEndLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEndLongitude, aOld.EndLongitude, FEndLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObservers, aOld.ObserversTally, FObserversTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAreaHa, aOld.TotalArea, FTotalArea, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDistanceKm, aOld.TotalDistance, FTotalDistance, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnets, aOld.TotalNets, FTotalNets, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHabitat, aOld.Habitat, FHabitat, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMistnetRounds, aOld.NetRounds, FNetRounds, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSurvey.EqualsTo(const Other: TSurvey): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSurvey.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSurveyDate     := Obj.Get('survey_date', NullDate);
    FStartTime      := Obj.Get('start_time', NullTime);
    FEndTime        := Obj.Get('end_time', NullTime);
    FDuration       := Obj.Get('duration', 0);
    FMethodId       := Obj.Get('method_id', 0);
    FNetStationId   := Obj.Get('net_station_id', 0);
    FExpeditionId   := Obj.Get('expedition_id', 0);
    FLocalityId     := Obj.Get('locality_id', 0);
    FProjectId      := Obj.Get('project_id', 0);
    FSampleId       := Obj.Get('sample_id', '');
    FStartLongitude := Obj.Get('start_longitude', 0.0);
    FStartLatitude  := Obj.Get('start_latitude', 0.0);
    FEndLongitude   := Obj.Get('end_longitude', 0.0);
    FEndLatitude    := Obj.Get('end_latitude', 0.0);
    FObserversTally := Obj.Get('observers_tally', 0);
    FTotalArea      := Obj.Get('total_area', 0.0);
    FTotalDistance  := Obj.Get('total_distance', 0.0);
    FTotalNets      := Obj.Get('total_nets', 0);
    FHabitat        := Obj.Get('habitat', '');
    FNetRounds      := Obj.Get('net_rounds', '');
    FFullName       := Obj.Get('full_name', '');
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TSurvey.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('survey_date', FSurveyDate);
    JSONObject.Add('start_time', FStartTime);
    JSONObject.Add('end_time', FEndTime);
    JSONObject.Add('duration', FDuration);
    JSONObject.Add('method_id', FMethodId);
    JSONObject.Add('net_station_id', FNetStationId);
    JSONObject.Add('expedition_id', FExpeditionId);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('sample_id', FSampleId);
    JSONObject.Add('start_longitude', FStartLongitude);
    JSONObject.Add('start_latitude', FStartLatitude);
    JSONObject.Add('end_longitude', FEndLongitude);
    JSONObject.Add('end_latitude', FEndLatitude);
    JSONObject.Add('observers_tally', FObserversTally);
    JSONObject.Add('total_area', FTotalArea);
    JSONObject.Add('total_distance', FTotalDistance);
    JSONObject.Add('total_nets', FTotalNets);
    JSONObject.Add('habitat', FHabitat);
    JSONObject.Add('net_rounds', FNetRounds);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSurvey.ToString: String;
begin
  Result := Format('Survey(Id=%d, SurveyDate=%s, StartTime=%s, EndTime=%s, Duration=%d, MethodId=%d, NetStationId=%d, ' +
    'ExpeditionId=%d, LocalityId=%d, ProjectId=%d, SampleId=%s, StartLongitude=%f, StartLatitude=%f, ' +
    'EndLongitude=%f, EndLatitude=%f, ObserversTally=%d, TotalArea=%f, TotalDistance=%f, TotalNets=%d, Habitat=%s, ' +
    'NetRounds=%s, FullName=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, DateToStr(FSurveyDate), TimeToStr(FStartTime), TimeToStr(FEndTime), FDuration, FMethodId, FNetStationId,
    FExpeditionId, FLocalityId, FProjectId, FSampleId, FStartLongitude, FStartLatitude, FEndLongitude,
    FEndLatitude, FObserversTally, FTotalArea, FTotalDistance, FTotalNets, FHabitat, FNetRounds, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSurvey.Validate(out Msg: string): Boolean;
begin
  if FSurveyDate = NullDate then
  begin
    Msg := 'Survey date required.';
    Exit(False);
  end;
  if FLocalityId = 0 then
  begin
    Msg := 'Locality required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSurveyRepository }

procedure TSurveyRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSurvey;
begin
  if not (E is TSurvey) then
    raise Exception.Create('Delete: Expected TSurvey');

  R := TSurvey(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSurveyRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SURVEY_ID;
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

function TSurveyRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SURVEY_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurveyRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_SURVEY_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSurvey) then
    raise Exception.Create('FindBy: Expected TSurvey');

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
        'survey_id, ' +
        'survey_date, ' +
        'start_time, ' +
        'end_time, ' +
        'duration, ' +
        'method_id, ' +
        'net_station_id, ' +
        'expedition_id, ' +
        'project_id, ' +
        'locality_id, ' +
        'sample_id, ' +
        'start_latitude, ' +
        'start_longitude, ' +
        'end_latitude, ' +
        'end_longitude, ' +
        'observers_tally, ' +
        'area_total, ' +
        'distance_total, ' +
        'nets_total, ' +
        'habitat, ' +
        'net_rounds, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM surveys');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSurvey(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSurveyRepository.FindBySiteAndDate(aLocal, aMethod: Integer; aDate: TDateTime; aSampleId: String;
  aNetStation: Integer; E: TSurvey);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM surveys');
    Add('WHERE (locality_id = :alocal)');
    Add('AND (method_id = :amethod)');
    if aSampleId <> EmptyStr then
      Add('AND (sample_id = :asampleid)');
    if aNetStation > 0 then
      Add('AND (net_station_id = :astation)');
    Add('AND (date(survey_date) = date(:adate))');
    SetIntParam(ParamByName('ALOCAL'), aLocal);
    SetIntParam(ParamByName('AMETHOD'), aMethod);
    if aSampleId <> EmptyStr then
      SetStrParam(ParamByName('ASAMPLEID'), aSampleId);
    if aNetStation > 0 then
      SetIntParam(ParamByName('ASTATION'), aNetStation);
    SetDateParam(ParamByName('ADATE'), aDate);
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

procedure TSurveyRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSurvey) then
    raise Exception.Create('GetById: Expected TSurvey');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'survey_id, ' +
        'survey_date, ' +
        'start_time, ' +
        'end_time, ' +
        'duration, ' +
        'method_id, ' +
        'net_station_id, ' +
        'expedition_id, ' +
        'project_id, ' +
        'locality_id, ' +
        'sample_id, ' +
        'start_latitude, ' +
        'start_longitude, ' +
        'end_latitude, ' +
        'end_longitude, ' +
        'observers_tally, ' +
        'area_total, ' +
        'distance_total, ' +
        'nets_total, ' +
        'habitat, ' +
        'net_rounds, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM surveys');
    Add('WHERE survey_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSurvey(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurveyRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSurvey;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSurvey) then
    raise Exception.Create('Hydrate: Expected TSurvey');

  R := TSurvey(E);
  with aDataSet do
  begin
    R.Id := FieldByName('survey_id').AsInteger;
    R.SurveyDate := FieldByName('survey_date').AsDateTime;
    R.StartTime := FieldByName('start_time').AsDateTime;
    R.EndTime := FieldByName('end_time').AsDateTime;
    R.Duration := FieldByName('duration').AsInteger;
    R.MethodId := FieldByName('method_id').AsInteger;
    R.NetStationId := FieldByName('net_station_id').AsInteger;
    R.ExpeditionId := FieldByName('expedition_id').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.SampleId := FieldByName('sample_id').AsString;
    R.StartLatitude := FieldByName('start_latitude').AsFloat;
    R.StartLongitude := FieldByName('start_longitude').AsFloat;
    R.EndLatitude := FieldByName('end_latitude').AsFloat;
    R.EndLongitude := FieldByName('end_longitude').AsFloat;
    R.ObserversTally := FieldByName('observers_tally').AsInteger;
    R.TotalArea := FieldByName('area_total').AsFloat;
    R.TotalDistance := FieldByName('distance_total').AsFloat;
    R.TotalNets := FieldByName('nets_total').AsInteger;
    R.Habitat := FieldByName('habitat').AsString;
    R.NetRounds := FieldByName('net_rounds').AsString;
    R.FullName := FieldByName('full_name').AsString;
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

procedure TSurveyRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSurvey;
begin
  if not (E is TSurvey) then
    raise Exception.Create('Insert: Expected TSurvey');

  R := TSurvey(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO surveys (' +
        'survey_date, ' +
        'start_time, ' +
        'end_time, ' +
        'duration, ' +
        'method_id, ' +
        'net_station_id, ' +
        'expedition_id, ' +
        'project_id, ' +
        'locality_id, ' +
        'sample_id, ' +
        'start_latitude, ' +
        'start_longitude, ' +
        'end_latitude, ' +
        'end_longitude, ' +
        'observers_tally, ' +
        'area_total, ' +
        'distance_total, ' +
        'nets_total, ' +
        'habitat, ' +
        'net_rounds, ' +
        'full_name, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
    Add('VALUES (' +
        'date(:survey_date), ' +
        'time(:start_time), ' +
        'time(:end_time), ' +
        ':duration, ' +
        ':method_id, ' +
        ':net_station_id, ' +
        ':expedition_id, ' +
        ':project_id, ' +
        ':locality_id, ' +
        ':sample_id, ' +
        ':start_latitude, ' +
        ':start_longitude, ' +
        ':end_latitude, ' +
        ':end_longitude, ' +
        ':observers_tally, ' +
        ':area_total, ' +
        ':distance_total, ' +
        ':nets_total, ' +
        ':habitat, ' +
        ':net_rounds, ' +
        ':full_name, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''));');

    SetDateParam(ParamByName('survey_date'), R.SurveyDate);
    SetTimeParam(ParamByName('start_time'), R.StartTime);
    SetTimeParam(ParamByName('end_time'), R.EndTime);
    SetIntParam(ParamByName('duration'), R.Duration);
    ParamByName('method_id').AsInteger := R.MethodId;
    SetForeignParam(ParamByName('net_station_id'), R.NetStationId);
    SetForeignParam(ParamByName('expedition_id'), R.ExpeditionId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetCoordinateParam(ParamByName('start_longitude'), ParamByName('start_latitude'), R.StartLongitude, R.StartLatitude);
    SetCoordinateParam(ParamByName('end_longitude'), ParamByName('end_latitude'), R.EndLongitude, R.EndLatitude);
    SetStrParam(ParamByName('sample_id'), R.SampleId);
    SetIntParam(ParamByName('observers_tally'), R.ObserversTally);
    SetIntParam(ParamByName('nets_total'), R.TotalNets);
    SetFloatParam(ParamByName('area_total'), R.TotalArea);
    SetFloatParam(ParamByName('distance_total'), R.TotalDistance);
    SetStrParam(ParamByName('habitat'), R.Habitat);
    SetStrParam(ParamByName('net_rounds'), R.NetRounds);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('full_name').AsString := GetSurveyFullname(R.SurveyDate, R.LocalityId, R.MethodId, R.NetStationId, R.SampleId);
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

function TSurveyRepository.TableName: string;
begin
  Result := TBL_SURVEYS;
end;

procedure TSurveyRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSurvey;
begin
  if not (E is TSurvey) then
    raise Exception.Create('Update: Expected TSurvey');

  R := TSurvey(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSurveyRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE surveys SET ' +
      'survey_date = date(:survey_date), ' +
      'start_time = time(:start_time), ' +
      'end_time = time(:end_time), ' +
      'duration = :duration, ' +
      'method_id = :method_id, ' +
      'net_station_id = :net_station_id, ' +
      'expedition_id = :expedition_id, ' +
      'project_id = :project_id, ' +
      'locality_id = :locality_id, ' +
      'sample_id = :sample_id, ' +
      'start_latitude = :start_latitude, ' +
      'start_longitude = :start_longitude, ' +
      'end_latitude = :end_latitude, ' +
      'end_longitude = :end_longitude, ' +
      'observers_tally = :observers_tally, ' +
      'area_total = :area_total, ' +
      'distance_total = :distance_total, ' +
      'nets_total = :nets_total, ' +
      'habitat = :habitat, ' +
      'net_rounds = :net_rounds, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'exported_status = :exported_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (survey_id = :survey_id)');

    SetDateParam(ParamByName('survey_date'), R.SurveyDate);
    SetTimeParam(ParamByName('start_time'), R.StartTime);
    SetTimeParam(ParamByName('end_time'), R.EndTime);
    SetIntParam(ParamByName('duration'), R.Duration);
    ParamByName('method_id').AsInteger := R.MethodId;
    SetForeignParam(ParamByName('net_station_id'), R.NetStationId);
    SetForeignParam(ParamByName('expedition_id'), R.ExpeditionId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetCoordinateParam(ParamByName('start_longitude'), ParamByName('start_latitude'), R.StartLongitude, R.StartLatitude);
    SetCoordinateParam(ParamByName('end_longitude'), ParamByName('end_latitude'), R.EndLongitude, R.EndLatitude);
    SetStrParam(ParamByName('sample_id'), R.SampleId);
    SetIntParam(ParamByName('observers_tally'), R.ObserversTally);
    SetIntParam(ParamByName('nets_total'), R.TotalNets);
    SetFloatParam(ParamByName('area_total'), R.TotalArea);
    SetFloatParam(ParamByName('distance_total'), R.TotalDistance);
    SetStrParam(ParamByName('habitat'), R.Habitat);
    SetStrParam(ParamByName('net_rounds'), R.NetRounds);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('full_name').AsString := GetSurveyFullname(R.SurveyDate, R.LocalityId, R.MethodId, R.NetStationId, R.SampleId);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('survey_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TWeatherLog }

constructor TWeatherLog.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TWeatherLog.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TWeatherLog then
  begin
    FSurveyId := TWeatherLog(Source).SurveyId;
    FAtmosphericPressure := TWeatherLog(Source).AtmosphericPressure;
    FCloudCover := TWeatherLog(Source).CloudCover;
    FNotes := TWeatherLog(Source).Notes;
    FPrecipitation := TWeatherLog(Source).Precipitation;
    FRainfall := TWeatherLog(Source).Rainfall;
    FRelativeHumidity := TWeatherLog(Source).RelativeHumidity;
    FSampleDate := TWeatherLog(Source).SampleDate;
    FSampleMoment := TWeatherLog(Source).SampleMoment;
    FSampleTime := TWeatherLog(Source).SampleTime;
    FObserverId := TWeatherLog(Source).ObserverId;
    FTemperature := TWeatherLog(Source).Temperature;
    FWindSpeedBft := TWeatherLog(Source).WindSpeedBft;
    FWindSpeedKmH := TWeatherLog(Source).WindSpeedKmH;
  end;
end;

procedure TWeatherLog.Clear;
begin
  inherited Clear;
  FSurveyId := 0;
  FAtmosphericPressure := 0;
  FCloudCover := 0;
  FNotes := EmptyStr;
  FPrecipitation := wpEmpty;
  FRainfall := 0;
  FRelativeHumidity := 0;
  FSampleDate := NullDate;
  FSampleMoment := wmNone;
  FSampleTime := NullTime;
  FObserverId := 0;
  FTemperature := 0;
  FWindSpeedBft := 0;
  FWindSpeedKmH := 0;
  FWindDirection := EmptyStr;
end;

function TWeatherLog.Clone: TXolmisRecord;
begin
  Result := TWeatherLog(inherited Clone);
end;

function TWeatherLog.Diff(const aOld: TWeatherLog; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscAtmosphericPressureH, aOld.AtmosphericPressure, FAtmosphericPressure, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloudCover, aOld.CloudCover, FCloudCover, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPrecipitation, aOld.Precipitation, FPrecipitation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRainfallMm, aOld.Rainfall, FRainfall, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRelativeHumidity, aOld.RelativeHumidity, FRelativeHumidity, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMoment, aOld.SampleMoment, FSampleMoment, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTemperatureC, aOld.Temperature, FTemperature, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWindBft, aOld.WindSpeedBft, FWindSpeedBft, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWindKmH, aOld.WindSpeedKmH, FWindSpeedKmH, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWindDirection, aOld.WindDirection, FWindDirection, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TWeatherLog.EqualsTo(const Other: TWeatherLog): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TWeatherLog.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSampleDate := Obj.Get('sample_date', NullDate);
    FSampleTime := Obj.Get('sample_time', NullTime);
    case Obj.Get('sample_moment', '') of
      'S': FSampleMoment := wmStart;
      'M': FSampleMoment := wmMiddle;
      'E': FSampleMoment := wmEnd;
    else
      FSampleMoment := wmNone;
    end;
    FSurveyId             := Obj.Get('survey_id', 0);
    FObserverId           := Obj.Get('observer_id', 0);
    FAtmosphericPressure  := Obj.Get('atmospheric_pressure', 0.0);
    case Obj.Get('precipitation', '') of
      'N': FPrecipitation := wpNone;
      'F': FPrecipitation := wpFog;
      'M': FPrecipitation := wpMist;
      'D': FPrecipitation := wpDrizzle;
      'R': FPrecipitation := wpRain;
    else
      FPrecipitation := wpEmpty;
    end;
    FCloudCover       := Obj.Get('cloud_cover', 0);
    FRainfall         := Obj.Get('rainfall', 0);
    FRelativeHumidity := Obj.Get('relative_humidity', 0.0);
    FTemperature      := Obj.Get('temperature', 0.0);
    FWindSpeedBft     := Obj.Get('wind_speed_bft', 0);
    FWindSpeedKmH     := Obj.Get('wind_speed_kmh', 0.0);
    FWindDirection    := Obj.Get('wind_direction', '');
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TWeatherLog.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('sample_date', FSampleDate);
    JSONObject.Add('sample_moment', SAMPLE_MOMENTS[FSampleMoment]);
    JSONObject.Add('sample_time', FSampleTime);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('observer_id', FObserverId);
    JSONObject.Add('atmospheric_pressure', FAtmosphericPressure);
    JSONObject.Add('cloud_cover', FCloudCover);
    JSONObject.Add('precipitation', PRECIPITATION_VALUES[FPrecipitation]);
    JSONObject.Add('rainfall', FRainfall);
    JSONObject.Add('relative_humidity', FRelativeHumidity);
    JSONObject.Add('temperature', FTemperature);
    JSONObject.Add('wind_speed_bft', FWindSpeedBft);
    JSONObject.Add('wind_speed_kmh', FWindSpeedKmH);
    JSONObject.Add('wind_direction', FWindDirection);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TWeatherLog.ToString: String;
begin
  Result := Format('WeatherLog(Id=%d, SampleDate=%s, SampleTime=%s, SampleMoment=%s, SurveyId=%d, ObserverId=%d, ' +
    'AtmosphericPressure=%f, CloudCover=%d, Precipitation=%s, Rainfall=%d, RelativeHumidity=%f, Temperature=%f, ' +
    'WindSpeedBft=%d, WindSpeedKmH=%f, WindDirection=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, DateToStr(FSampleDate), TimeToStr(FSampleTime), SAMPLE_MOMENTS[FSampleMoment], FSurveyId, FObserverId,
    FAtmosphericPressure, FCloudCover, PRECIPITATION_VALUES[FPrecipitation], FRainfall, FRelativeHumidity,
    FTemperature, FWindSpeedBft, FWindSpeedKmH, FWindDirection, FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TWeatherLog.Validate(out Msg: string): Boolean;
begin
  if FSampleDate = NullDate then
  begin
    Msg := 'Date required.';
    Exit(False);
  end;
  if FSurveyId = 0 then
  begin
    Msg := 'Survey required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TWeatherLogRepository }

procedure TWeatherLogRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TWeatherLog;
begin
  if not (E is TWeatherLog) then
    raise Exception.Create('Delete: Expected TWeatherLog');

  R := TWeatherLog(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TWeatherLogRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_WEATHER_ID;
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

function TWeatherLogRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_WEATHER_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TWeatherLogRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_WEATHER_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TWeatherLog) then
    raise Exception.Create('FindBy: Expected TWeatherLog');

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
      'weather_id, ' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'sample_moment, ' +
      'observer_id, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'rainfall, ' +
      'temperature, ' +
      'wind_speed_bft, ' +
      'wind_speed_kmh, ' +
      'wind_direction, ' +
      'relative_humidity, ' +
      'atmospheric_pressure, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM weather_logs');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TWeatherLog(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TWeatherLogRepository.FindBySurvey(aSurvey: Integer; aDate, aTime: String; aObserver: Integer;
  E: TWeatherLog);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM weather_logs');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (date(sample_date) = date(:adate))');
    Add('AND (time(sample_time) = time(:atime))');
    Add('AND (observer_id = :aobserver)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
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

procedure TWeatherLogRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TWeatherLog) then
    raise Exception.Create('GetById: Expected TWeatherLog');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'weather_id, ' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'sample_moment, ' +
      'observer_id, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'rainfall, ' +
      'temperature, ' +
      'wind_speed_bft, ' +
      'wind_speed_kmh, ' +
      'wind_direction, ' +
      'relative_humidity, ' +
      'atmospheric_pressure, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM weather_logs');
    Add('WHERE weather_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TWeatherLog(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TWeatherLogRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TWeatherLog;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TWeatherLog) then
    raise Exception.Create('Hydrate: Expected TWeatherLog');

  R := TWeatherLog(E);
  with aDataSet do
  begin
    R.Id := FieldByName('weather_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.AtmosphericPressure := FieldByName('atmospheric_pressure').AsFloat;
    R.CloudCover := FieldByName('cloud_cover').AsInteger;
    R.Notes := FieldByName('notes').AsString;
    case FieldByName('precipitation').AsString of
      'N': R.Precipitation := wpNone;
      'F': R.Precipitation := wpFog;
      'M': R.Precipitation := wpMist;
      'D': R.Precipitation := wpDrizzle;
      'R': R.Precipitation := wpRain;
    else
      R.Precipitation := wpEmpty;
    end;
    R.Rainfall := FieldByName('rainfall').AsInteger;
    R.RelativeHumidity := FieldByName('relative_humidity').AsFloat;
    R.SampleDate := FieldByName('sample_date').AsDateTime;
    case FieldByName('sample_moment').AsString of
      'S': R.SampleMoment := wmStart;
      'M': R.SampleMoment := wmMiddle;
      'E': R.SampleMoment := wmEnd;
    else
      R.SampleMoment := wmNone;
    end;
    R.SampleTime := FieldByName('sample_time').AsDateTime;
    R.Temperature := FieldByName('temperature').AsFloat;
    R.ObserverId := FieldByName('observer_id').AsInteger;
    R.WindSpeedBft := FieldByName('wind_speed_bft').AsInteger;
    R.WindSpeedKmH := FieldByName('wind_speed_kmh').AsFloat;
    R.WindDirection := FieldByName('wind_direction').AsString;
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

procedure TWeatherLogRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TWeatherLog;
begin
  if not (E is TWeatherLog) then
    raise Exception.Create('Insert: Expected TWeatherLog');

  R := TWeatherLog(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO weather_logs (' +
      'survey_id, ' +
      'sample_date, ' +
      'sample_time, ' +
      'sample_moment, ' +
      'observer_id, ' +
      'cloud_cover, ' +
      'precipitation, ' +
      'rainfall, ' +
      'temperature, ' +
      'wind_speed_bft, ' +
      'wind_speed_kmh, ' +
      'wind_direction, ' +
      'relative_humidity, ' +
      'atmospheric_pressure, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':survey_id, ' +
      'date(:sample_date), ' +
      'time(:sample_time), ' +
      ':sample_moment, ' +
      ':observer_id, ' +
      ':cloud_cover, ' +
      ':precipitation, ' +
      ':rainfall, ' +
      ':temperature, ' +
      ':wind_speed_bft, ' +
      ':wind_speed_kmh, ' +
      ':wind_direction, ' +
      ':relative_humidity, ' +
      ':atmospheric_pressure, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    ParamByName('sample_moment').AsString := SAMPLE_MOMENTS[R.SampleMoment];
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    ParamByName('cloud_cover').AsInteger := R.CloudCover;
    ParamByName('precipitation').AsString := PRECIPITATION_VALUES[R.Precipitation];
    ParamByName('rainfall').AsInteger := R.Rainfall;
    ParamByName('temperature').AsFloat := R.Temperature;
    ParamByName('wind_speed_bft').AsInteger := R.WindSpeedBft;
    ParamByName('wind_speed_kmh').AsFloat := R.WindSpeedKmH;
    SetStrParam(ParamByName('wind_direction'), R.WindDirection);
    ParamByName('relative_humidity').AsFloat := R.RelativeHumidity;
    SetFloatParam(ParamByName('atmospheric_pressure'), R.AtmosphericPressure);
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

function TWeatherLogRepository.TableName: string;
begin
  Result := TBL_WEATHER_LOGS;
end;

procedure TWeatherLogRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TWeatherLog;
begin
  if not (E is TWeatherLog) then
    raise Exception.Create('Update: Expected TWeatherLog');

  R := TWeatherLog(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TWeatherLogRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE weather_logs SET ' +
      'survey_id = :survey_id, ' +
      'sample_date = date(:sample_date), ' +
      'sample_time = time(:sample_time), ' +
      'sample_moment = :sample_moment, ' +
      'observer_id = :observer_id, ' +
      'cloud_cover = :cloud_cover, ' +
      'precipitation = :precipitation, ' +
      'rainfall = :rainfall, ' +
      'temperature = :temperature, ' +
      'wind_speed_bft = :wind_speed_bft, ' +
      'wind_speed_kmh = :wind_speed_kmh, ' +
      'wind_direction = :wind_direction, ' +
      'relative_humidity = :relative_humidity, ' +
      'atmospheric_pressure = :atmospheric_pressure, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (weather_id = :weather_id)');

    SetDateParam(ParamByName('sample_date'), R.SampleDate);
    SetTimeParam(ParamByName('sample_time'), R.SampleTime);
    ParamByName('sample_moment').AsString := SAMPLE_MOMENTS[R.SampleMoment];
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    ParamByName('cloud_cover').AsInteger := R.CloudCover;
    ParamByName('precipitation').AsString := PRECIPITATION_VALUES[R.Precipitation];
    ParamByName('rainfall').AsInteger := R.Rainfall;
    ParamByName('temperature').AsFloat := R.Temperature;
    ParamByName('wind_speed_bft').AsInteger := R.WindSpeedBft;
    ParamByName('wind_speed_kmh').AsFloat := R.WindSpeedKmH;
    SetStrParam(ParamByName('wind_direction'), R.WindDirection);
    ParamByName('relative_humidity').AsFloat := R.RelativeHumidity;
    SetFloatParam(ParamByName('atmospheric_pressure'), R.AtmosphericPressure);
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('weather_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

