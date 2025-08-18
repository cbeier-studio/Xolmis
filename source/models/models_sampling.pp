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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TExpedition; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TExpedition);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property Name: String read FName write FName;
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property ProjectId: Integer read FProjectId write FProjectId;
    property Description: String read FDescription write FDescription;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Diff(aOld: TSurvey; var aList: TStrings): Boolean;
    function Find(aLocal, aMethod: Integer; aDate: TDateTime; aSampleId: String = ''; aNetStation: Integer = 0): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSurvey);
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
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aSurvey: Integer; aDate, aTime: String; aObserver: Integer): Boolean;
    function Diff(aOld: TWeatherLog; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TWeatherLog);
    function ToJSON: String;
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
    property RelativeHumidity: Double read FRelativeHumidity write FRelativeHumidity;
    property AtmosphericPressure: Double read FAtmosphericPressure write FAtmosphericPressure;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TSurveyMember }

  TSurveyMember = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FPersonId: Integer;
    FVisitor: Boolean;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Diff(aOld: TSurveyMember; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSurveyMember);
    function ToJSON: String;
    function Find(const aSurveyKey, aPersonKey: Integer): Boolean;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property PersonId: Integer read FPersonId write FPersonId;
    property Visitor: Boolean read FVisitor write FVisitor;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure Copy(aFrom: TNetEffort);
    procedure Delete;
    function Diff(aOld: TNetEffort; var aList: TStrings): Boolean;
    function Find(aSurvey: Integer; aNetNumber: String): Boolean;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    procedure Save;
    function ToJSON: String;
    procedure Update;
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
    property TotalOpenTime: Double read FTotalOpenTime;
    property NetLength: Double read FNetLength write FNetLength;
    property NetHeight: Double read FNetHeight write FNetHeight;
    property NetArea: Double read FNetArea;
    property NetMesh: Integer read FNetMesh write FNetMesh;
    property Notes: String read FNotes write FNotes;
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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aSurvey: Integer; aDate, aTime: String; aLongitude, aLatitude: Extended; aObserver: Integer): Boolean;
    function Diff(aOld: TVegetation; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TVegetation);
    function ToJSON: String;
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

  function AuthorListToString(aAuthors: TAuthors): String;
  procedure StringToAuthorList(const aCitation: String; var aAuthors: TAuthors);

implementation

uses
  utils_locale, utils_global, models_users, utils_validations, utils_fullnames, data_columns,
  data_setparam, udm_main;

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
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TExpedition.Copy(aFrom: TExpedition);
begin
  FName := aFrom.Name;
  FStartDate := aFrom.StartDate;
  FEndDate := aFrom.EndDate;
  FProjectId := aFrom.ProjectId;
  FDescription := aFrom.Description;
end;

procedure TExpedition.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TExpedition.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM expeditions');
      Add('WHERE (expedition_id = :aid)');

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

procedure TExpedition.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TExpedition.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('expedition_id').AsInteger;
    FName := FieldByName('expedition_name').AsString;
    if not (FieldByName('start_date').IsNull) then
      FStartDate := FieldByName('start_date').AsDateTime
    else
      FStartDate := NullDate;
    if not (FieldByName('end_date').IsNull) then
      FEndDate := FieldByName('end_date').AsDateTime
    else
      FEndDate := NullDate;
    FProjectId := FieldByName('project_id').AsInteger;
    FDescription := FieldByName('description').AsString;
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

procedure TExpedition.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      ParamByName('expedition_name').AsString := FName;
      SetDateParam(ParamByName('start_date'), FStartDate);
      SetDateParam(ParamByName('end_date'), FEndDate);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetStrParam(ParamByName('description'), FDescription);
      ParamByName('user_inserted').AsInteger := FUserInserted;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TExpedition.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TExpedition.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FName);
    JSONObject.Add('Start date', FStartDate);
    JSONObject.Add('End date', FEndDate);
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Description', FDescription);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TExpedition.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TExpedition.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      ParamByName('expedition_name').AsString := FName;
      SetDateParam(ParamByName('start_date'), FStartDate);
      SetDateParam(ParamByName('end_date'), FEndDate);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetStrParam(ParamByName('description'), FDescription);
      ParamByName('user_updated').AsInteger := FUserInserted;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('expedition_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TExpedition.Diff(aOld: TExpedition; var aList: TStrings): Boolean;
var
  R: string;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStartDate, aOld.StartDate, FStartDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEndDate, aOld.EndDate, FEndDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TExpedition.Find(const FieldName: String; const Value: Variant): Boolean;
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
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

{ TNetEffort }

constructor TNetEffort.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TNetEffort.Copy(aFrom: TNetEffort);
begin
  FFullName := aFrom.FullName;
  FSurveyId := aFrom.SurveyId;
  FNetStationId := aFrom.NetStationId;
  FPermanentNetId := aFrom.PermanentNetId;
  FNetNumber := aFrom.NetNumber;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FSampleDate := aFrom.SampleDate;
  FNetOpen1 := aFrom.NetOpen1;
  FNetClose1 := aFrom.NetClose1;
  FNetOpen2 := aFrom.NetOpen2;
  FNetClose2 := aFrom.NetClose2;
  FNetOpen3 := aFrom.NetOpen3;
  FNetClose3 := aFrom.NetClose3;
  FNetOpen4 := aFrom.NetOpen4;
  FNetClose4 := aFrom.NetClose4;
  FTotalOpenTime := aFrom.TotalOpenTime;
  FNetLength := aFrom.NetLength;
  FNetHeight := aFrom.FNetHeight;
  FNetArea := aFrom.NetArea;
  FNetMesh := aFrom.NetMesh;
  FNotes := aFrom.Notes;
end;

procedure TNetEffort.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNetEffort.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM nets_effort');
      Add('WHERE (net_id = :aid)');

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

function TNetEffort.Diff(aOld: TNetEffort; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.NetStationId, FNetStationId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPermanentNetID, aOld.PermanentNetId, FPermanentNetId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnetNr, aOld.NetNumber, FNetNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOpenTime1, aOld.NetOpen1, FNetOpen1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloseTime1, aOld.NetClose1, FNetClose1, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOpenTime2, aOld.NetOpen2, FNetOpen2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloseTime2, aOld.NetClose2, FNetClose2, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOpenTime3, aOld.NetOpen3, FNetOpen3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloseTime3, aOld.NetClose3, FNetClose3, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOpenTime4, aOld.NetOpen4, FNetOpen4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloseTime4, aOld.NetClose4, FNetClose4, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnetLengthM, aOld.NetLength, FNetLength, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnetHeightM, aOld.NetHeight, FNetHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnetMesh, aOld.NetMesh, FNetMesh, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TNetEffort.Find(aSurvey: Integer; aNetNumber: String): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT net_id FROM nets_effort');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (net_number = :anet)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('ANET').AsInteger := StrToInt(aNetNumber);
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('net_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNetEffort.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNetEffort.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('net_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FNetStationId := FieldByName('net_station_id').AsInteger;
    FPermanentNetId := FieldByName('permanent_net_id').AsInteger;
    FNetNumber := FieldByName('net_number').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FSampleDate := FieldByName('sample_date').AsDateTime;
    FNetOpen1 := FieldByName('net_open_1').AsDateTime;
    FNetClose1 := FieldByName('net_close_1').AsDateTime;
    FNetOpen2 := FieldByName('net_open_2').AsDateTime;
    FNetClose2 := FieldByName('net_close_2').AsDateTime;
    FNetOpen3 := FieldByName('net_open_3').AsDateTime;
    FNetClose3 := FieldByName('net_close_3').AsDateTime;
    FNetOpen4 := FieldByName('net_open_4').AsDateTime;
    FNetClose4 := FieldByName('net_close_4').AsDateTime;
    FTotalOpenTime := FieldByName('open_time_total').AsFloat;
    FNetLength := FieldByName('net_length').AsFloat;
    FNetHeight := FieldByName('net_height').AsFloat;
    FNetArea := FieldByName('net_area').AsFloat;
    FNetMesh := FieldByName('net_mesh').AsInteger;
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

procedure TNetEffort.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      SetDateParam(ParamByName('sample_date'), FSampleDate);
      ParamByName('net_number').AsInteger := FNetNumber;
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('net_station_id').AsInteger := FNetStationId;
      SetForeignParam(ParamByName('permanent_net_id'), FPermanentNetId);
      ParamByName('full_name').AsString := GetNetEffortFullname(FSampleDate, FNetStationId, FNetNumber);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('net_length'), FNetLength);
      SetFloatParam(ParamByName('net_height'), FNetHeight);
      SetIntParam(ParamByName('net_mesh'), FNetMesh);
      ParamByName('notes').AsString := FNotes;

      SetTimeParam(ParamByName('net_open_1'), FNetOpen1);
      SetTimeParam(ParamByName('net_close_1'), FNetClose1);
      SetTimeParam(ParamByName('net_open_2'), FNetOpen2);
      SetTimeParam(ParamByName('net_close_2'), FNetClose2);
      SetTimeParam(ParamByName('net_open_3'), FNetOpen3);
      SetTimeParam(ParamByName('net_close_3'), FNetClose3);
      SetTimeParam(ParamByName('net_open_4'), FNetOpen4);
      SetTimeParam(ParamByName('net_close_4'), FNetClose4);

      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TNetEffort.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TNetEffort.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Net Station', FNetStationId);
    JSONObject.Add('Permanent Net', FPermanentNetId);
    JSONObject.Add('Net number', FNetNumber);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Date', FSampleDate);
    JSONObject.Add('Open 1', FNetOpen1);
    JSONObject.Add('Close 1', FNetClose1);
    JSONObject.Add('Open 2', FNetOpen2);
    JSONObject.Add('Close 2', FNetClose2);
    JSONObject.Add('Open 3', FNetOpen3);
    JSONObject.Add('Close 3', FNetClose3);
    JSONObject.Add('Open 4', FNetOpen4);
    JSONObject.Add('Close 4', FNetClose4);
    JSONObject.Add('Total open time', FTotalOpenTime);
    JSONObject.Add('Net length', FNetLength);
    JSONObject.Add('Net height', FNetHeight);
    JSONObject.Add('Net area', FNetArea);
    JSONObject.Add('Net mesh', FNetMesh);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TNetEffort.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TNetEffort.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      SetDateParam(ParamByName('sample_date'), FSampleDate);
      ParamByName('net_number').AsInteger := FNetNumber;
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('net_station_id').AsInteger := FNetStationId;
      SetForeignParam(ParamByName('permanent_net_id'), FPermanentNetId);
      ParamByName('full_name').AsString := GetNetEffortFullname(FSampleDate, FNetStationId, FNetNumber);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('net_length'), FNetLength);
      SetFloatParam(ParamByName('net_height'), FNetHeight);
      SetIntParam(ParamByName('net_mesh'), FNetMesh);
      ParamByName('notes').AsString := FNotes;

      SetTimeParam(ParamByName('net_open_1'), FNetOpen1);
      SetTimeParam(ParamByName('net_close_1'), FNetClose1);
      SetTimeParam(ParamByName('net_open_2'), FNetOpen2);
      SetTimeParam(ParamByName('net_close_2'), FNetClose2);
      SetTimeParam(ParamByName('net_open_3'), FNetOpen3);
      SetTimeParam(ParamByName('net_close_3'), FNetClose3);
      SetTimeParam(ParamByName('net_open_4'), FNetOpen4);
      SetTimeParam(ParamByName('net_close_4'), FNetClose4);

      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('net_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TVegetation }

constructor TVegetation.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TVegetation.Copy(aFrom: TVegetation);
begin
  FSurveyId := aFrom.SurveyId;
  FSampleDate := aFrom.SampleDate;
  FSampleTime := aFrom.SampleTime;
  FNotes := aFrom.Notes;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FObserverId := aFrom.ObserverId;
  FHerbsProportion := aFrom.HerbsProportion;
  FHerbsDistribution := aFrom.HerbsDistribution;
  FHerbsAvgHeight := aFrom.HerbsAvgHeight;
  FShrubsProportion := aFrom.ShrubsProportion;
  FShrubsDistribution := aFrom.ShrubsDistribution;
  FShrubsAvgHeight := aFrom.ShrubsAvgHeight;
  FTreesProportion := aFrom.TreesProportion;
  FTreesDistribution := aFrom.TreesDistribution;
  FTreesAvgHeight := aFrom.TreesAvgHeight;
end;

procedure TVegetation.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TVegetation.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM vegetation');
      Add('WHERE (vegetation_id = :aid)');

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

function TVegetation.Diff(aOld: TVegetation; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProportionOfHerbs, aOld.HerbsProportion, FHerbsProportion, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHerbsDistribution, aOld.HerbsDistribution, FHerbsDistribution, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAvgHeightOfHerbs, aOld.HerbsAvgHeight, FHerbsAvgHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProportionOfShrubs, aOld.ShrubsProportion, FShrubsProportion, R) then
    aList.Add(R);
  if FieldValuesDiff(rscShrubsDistribution, aOld.ShrubsDistribution, FShrubsDistribution, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAvgHeightOfShrubs, aOld.ShrubsAvgHeight, FShrubsAvgHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProportionOfTrees, aOld.TreesProportion, FTreesProportion, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTreesDistribution, aOld.TreesDistribution, FTreesDistribution, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAvgHeightOfTrees, aOld.TreesAvgHeight, FTreesAvgHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TVegetation.Find(aSurvey: Integer; aDate, aTime: String; aLongitude, aLatitude: Extended; aObserver: Integer): Boolean;
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
    Add('SELECT vegetation_id FROM vegetation');
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
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('vegetation_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVegetation.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVegetation.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataset.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('vegetation_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FSampleDate := FieldByName('sample_date').AsDateTime;
    FSampleTime := FieldByName('sample_time').AsDateTime;
    FNotes := FieldByName('notes').AsString;
    FLongitude := FieldByName('longitude').AsFloat;
    FLatitude := FieldByName('latitude').AsFloat;
    FObserverId := FieldByName('observer_id').AsInteger;
    FHerbsProportion := FieldByName('herbs_proportion').AsInteger;
    FHerbsDistribution := TStratumDistribution(FieldByName('herbs_distribution').AsInteger);
    FHerbsAvgHeight := FieldByName('herbs_avg_height').AsInteger;
    FShrubsProportion := FieldByName('shrubs_proportion').AsInteger;
    FShrubsDistribution := TStratumDistribution(FieldByName('shrubs_distribution').AsInteger);
    FShrubsAvgHeight := FieldByName('shrubs_avg_height').AsInteger;
    FTreesProportion := FieldByName('trees_proportion').AsInteger;
    FTreesDistribution := TStratumDistribution(FieldByName('trees_distribution').AsInteger);
    FTreesAvgHeight := FieldByName('trees_avg_height').AsInteger;
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

procedure TVegetation.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('survey_id').AsInteger := FSurveyId;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetStrParam(ParamByName('notes'), FNotes);

      ParamByName('herbs_proportion').AsInteger := FHerbsProportion;
      ParamByName('herbs_distribution').AsInteger := Ord(FHerbsDistribution);
      ParamByName('herbs_avg_height').AsInteger := FHerbsAvgHeight;
      ParamByName('shrubs_proportion').AsInteger := FShrubsProportion;
      ParamByName('shrubs_distribution').AsInteger := Ord(FShrubsDistribution);
      ParamByName('shrubs_avg_height').AsInteger := FShrubsAvgHeight;
      ParamByName('trees_proportion').AsInteger := FTreesProportion;
      ParamByName('trees_distribution').AsInteger := Ord(FTreesDistribution);
      ParamByName('trees_avg_height').AsInteger := FTreesAvgHeight;

      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVegetation.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TVegetation.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Date', FSampleDate);
    JSONObject.Add('Time', FSampleTime);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Herbs Proportion', FHerbsProportion);
    JSONObject.Add('Herbs Distribution', Ord(FHerbsDistribution));
    JSONObject.Add('Herbs Average Height', FHerbsAvgHeight);
    JSONObject.Add('Shrubs Proportion', FShrubsProportion);
    JSONObject.Add('Shrubs Distribution', Ord(FShrubsDistribution));
    JSONObject.Add('Shrubs Average Height', FShrubsAvgHeight);
    JSONObject.Add('Trees Proportion', FTreesProportion);
    JSONObject.Add('Trees Distribution', Ord(FTreesDistribution));
    JSONObject.Add('Trees Average Height', FTreesAvgHeight);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TVegetation.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TVegetation.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('survey_id').AsInteger := FSurveyId;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetStrParam(ParamByName('notes'), FNotes);

      ParamByName('herbs_proportion').AsInteger := FHerbsProportion;
      ParamByName('herbs_distribution').AsInteger := Ord(FHerbsDistribution);
      ParamByName('herbs_avg_height').AsInteger := FHerbsAvgHeight;
      ParamByName('shrubs_proportion').AsInteger := FShrubsProportion;
      ParamByName('shrubs_distribution').AsInteger := Ord(FShrubsDistribution);
      ParamByName('shrubs_avg_height').AsInteger := FShrubsAvgHeight;
      ParamByName('trees_proportion').AsInteger := FTreesProportion;
      ParamByName('trees_distribution').AsInteger := Ord(FTreesDistribution);
      ParamByName('trees_avg_height').AsInteger := FTreesAvgHeight;

      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('vegetation_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSurveyMember }

constructor TSurveyMember.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSurveyMember.Clear;
begin
  inherited Clear;
  FSurveyId := 0;
  FPersonId := 0;
  FVisitor := False;
end;

procedure TSurveyMember.Copy(aFrom: TSurveyMember);
begin
  FSurveyId := aFrom.SurveyId;
  FPersonId := aFrom.PersonId;
  FVisitor := aFrom.Visitor;
end;

procedure TSurveyMember.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSurveyMember.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM survey_team');
      Add('WHERE (survey_member_id = :aid)');

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

procedure TSurveyMember.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurveyMember.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataset.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('survey_member_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FPersonId := FieldByName('person_id').AsInteger;
    FVisitor := FieldByName('visitor').AsBoolean;
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

procedure TSurveyMember.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO survey_team (' +
        'survey_id, ' +
        'person_id, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':survey_id, ' +
        ':person_id, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurveyMember.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSurveyMember.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Person', FPersonId);
    JSONObject.Add('Visitor', FVisitor);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSurveyMember.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSurveyMember.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('visitor').AsBoolean := FVisitor;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSurveyMember.Diff(aOld: TSurveyMember; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscVisitor, aOld.Visitor, FVisitor, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSurveyMember.Find(const aSurveyKey, aPersonKey: Integer): Boolean;
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
      'active_status' +
      'FROM survey_team');
    Add('WHERE (survey_id = :asurvey) AND (person_id = :aperson)');
    ParamByName('asurvey').AsInteger := aSurveyKey;
    ParamByName('aperson').AsInteger := aPersonKey;
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

{ TSurvey }

constructor TSurvey.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TSurvey.Copy(aFrom: TSurvey);
begin
  FSurveyDate := aFrom.SurveyDate;
  FStartTime := aFrom.StartTime;
  FEndTime := aFrom.EndTime;
  FDuration := aFrom.Duration;
  FMethodId := aFrom.MethodId;
  FNetStationId := aFrom.NetStationId;
  FExpeditionId := aFrom.ExpeditionId;
  FLocalityId := aFrom.LocalityId;
  FProjectId := aFrom.ProjectId;
  FSampleId := aFrom.SampleId;
  FStartLatitude := aFrom.StartLatitude;
  FStartLongitude := aFrom.StartLongitude;
  FEndLatitude := aFrom.EndLatitude;
  FEndLongitude := aFrom.EndLongitude;
  FObserversTally := aFrom.ObserversTally;
  FTotalArea := aFrom.TotalArea;
  FTotalDistance := aFrom.TotalDistance;
  FTotalNets := aFrom.TotalNets;
  FHabitat := aFrom.Habitat;
  FNetRounds := aFrom.NetRounds;
  FFullName := aFrom.FullName;
  FNotes := aFrom.Notes;
end;

procedure TSurvey.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSurvey.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM surveys');
      Add('WHERE (survey_id = :aid)');

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

procedure TSurvey.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurvey.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('survey_id').AsInteger;
    FSurveyDate := FieldByName('survey_date').AsDateTime;
    FStartTime := FieldByName('start_time').AsDateTime;
    FEndTime := FieldByName('end_time').AsDateTime;
    FDuration := FieldByName('duration').AsInteger;
    FMethodId := FieldByName('method_id').AsInteger;
    FNetStationId := FieldByName('net_station_id').AsInteger;
    FExpeditionId := FieldByName('expedition_id').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FSampleId := FieldByName('sample_id').AsString;
    FStartLatitude := FieldByName('start_latitude').AsFloat;
    FStartLongitude := FieldByName('start_longitude').AsFloat;
    FEndLatitude := FieldByName('end_latitude').AsFloat;
    FEndLongitude := FieldByName('end_longitude').AsFloat;
    FObserversTally := FieldByName('observers_tally').AsInteger;
    FTotalArea := FieldByName('area_total').AsFloat;
    FTotalDistance := FieldByName('distance_total').AsFloat;
    FTotalNets := FieldByName('nets_total').AsInteger;
    FHabitat := FieldByName('habitat').AsString;
    FNetRounds := FieldByName('net_rounds').AsString;
    FFullName := FieldByName('full_name').AsString;
    FNotes := FieldByName('notes').AsString;
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
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSurvey.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
      SetDateParam(ParamByName('survey_date'), FSurveyDate);
      SetTimeParam(ParamByName('start_time'), FStartTime);
      SetTimeParam(ParamByName('end_time'), FEndTime);
      SetIntParam(ParamByName('duration'), FDuration);
      ParamByName('method_id').AsInteger := FMethodId;
      SetForeignParam(ParamByName('net_station_id'), FNetStationId);
      SetForeignParam(ParamByName('expedition_id'), FExpeditionId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetCoordinateParam(ParamByName('start_longitude'), ParamByName('start_latitude'), FStartLongitude, FStartLatitude);
      SetCoordinateParam(ParamByName('end_longitude'), ParamByName('end_latitude'), FEndLongitude, FEndLatitude);
      SetStrParam(ParamByName('sample_id'), FSampleId);
      SetIntParam(ParamByName('observers_tally'), FObserversTally);
      SetIntParam(ParamByName('nets_total'), FTotalNets);
      SetFloatParam(ParamByName('area_total'), FTotalArea);
      SetFloatParam(ParamByName('distance_total'), FTotalDistance);
      SetStrParam(ParamByName('habitat'), FHabitat);
      SetStrParam(ParamByName('net_rounds'), FNetRounds);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('full_name').AsString := GetSurveyFullname(FSurveyDate, FLocalityId, FMethodId, FNetStationId, FSampleId);
      ParamByName('user_inserted').AsInteger := FUserInserted;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSurvey.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

procedure TSurvey.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSurvey.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetDateParam(ParamByName('survey_date'), FSurveyDate);
      SetTimeParam(ParamByName('start_time'), FStartTime);
      SetTimeParam(ParamByName('end_time'), FEndTime);
      SetIntParam(ParamByName('duration'), FDuration);
      ParamByName('method_id').AsInteger := FMethodId;
      SetForeignParam(ParamByName('net_station_id'), FNetStationId);
      SetForeignParam(ParamByName('expedition_id'), FExpeditionId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetCoordinateParam(ParamByName('start_longitude'), ParamByName('start_latitude'), FStartLongitude, FStartLatitude);
      SetCoordinateParam(ParamByName('end_longitude'), ParamByName('end_latitude'), FEndLongitude, FEndLatitude);
      SetStrParam(ParamByName('sample_id'), FSampleId);
      SetIntParam(ParamByName('observers_tally'), FObserversTally);
      SetIntParam(ParamByName('nets_total'), FTotalNets);
      SetFloatParam(ParamByName('area_total'), FTotalArea);
      SetFloatParam(ParamByName('distance_total'), FTotalDistance);
      SetStrParam(ParamByName('habitat'), FHabitat);
      SetStrParam(ParamByName('net_rounds'), FNetRounds);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('full_name').AsString := GetSurveyFullname(FSurveyDate, FLocalityId, FMethodId, FNetStationId, FSampleId);
      ParamByName('user_updated').AsInteger := FUserInserted;
      ParamByName('exported_status').AsBoolean := FExported;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('survey_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSurvey.Diff(aOld: TSurvey; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.SurveyDate, FSurveyDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStartTime, aOld.StartTime, FStartTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEndTime, aOld.EndTime, FEndTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDurationMin, aOld.Duration, FDuration, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.NetStationId, FNetStationId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExpeditionID, aOld.ExpeditionId, FExpeditionId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSampleID, aOld.SampleId, FSampleId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.StartLatitude, FStartLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.StartLongitude, FStartLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEndLatitude, aOld.EndLatitude, FEndLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEndLongitude, aOld.EndLongitude, FEndLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObservers, aOld.ObserversTally, FObserversTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAreaHa, aOld.TotalArea, FTotalArea, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDistanceKm, aOld.TotalDistance, FTotalDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnets, aOld.TotalNets, FTotalNets, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHabitat, aOld.Habitat, FHabitat, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMistnetRounds, aOld.NetRounds, FNetRounds, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSurvey.Find(aLocal, aMethod: Integer; aDate: TDateTime; aSampleId: String; aNetStation: Integer): Boolean;
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
    Add('SELECT survey_id FROM surveys');
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
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('survey_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TWeatherLog }

constructor TWeatherLog.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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
end;

procedure TWeatherLog.Copy(aFrom: TWeatherLog);
begin
  FSurveyId := aFrom.SurveyId;
  FAtmosphericPressure := aFrom.AtmosphericPressure;
  FCloudCover := aFrom.CloudCover;
  FNotes := aFrom.Notes;
  FPrecipitation := aFrom.Precipitation;
  FRainfall := aFrom.Rainfall;
  FRelativeHumidity := aFrom.RelativeHumidity;
  FSampleDate := aFrom.SampleDate;
  FSampleMoment := aFrom.SampleMoment;
  FSampleTime := aFrom.SampleTime;
  FObserverId := aFrom.ObserverId;
  FTemperature := aFrom.Temperature;
  FWindSpeedBft := aFrom.WindSpeedBft;
  FWindSpeedKmH := aFrom.WindSpeedKmH;
end;

procedure TWeatherLog.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TWeatherLog.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM weather_logs');
      Add('WHERE (weather_id = :aid)');

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

function TWeatherLog.Diff(aOld: TWeatherLog; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscAtmosphericPressureH, aOld.AtmosphericPressure, FAtmosphericPressure, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloudCover, aOld.CloudCover, FCloudCover, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPrecipitation, aOld.Precipitation, FPrecipitation, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRainfallMm, aOld.Rainfall, FRainfall, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRelativeHumidity, aOld.RelativeHumidity, FRelativeHumidity, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMoment, aOld.SampleMoment, FSampleMoment, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTemperatureC, aOld.Temperature, FTemperature, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWindBft, aOld.WindSpeedBft, FWindSpeedBft, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWindKmH, aOld.WindSpeedKmH, FWindSpeedKmH, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TWeatherLog.Find(aSurvey: Integer; aDate, aTime: String; aObserver: Integer): Boolean;
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
    Add('SELECT weather_id FROM weather_logs');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (date(sample_date) = date(:adate))');
    Add('AND (time(sample_time) = time(:atime))');
    Add('AND (observer_id = :aobserver)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('AOBSERVER').AsInteger := aObserver;
    ParamByName('ADATE').AsString := aDate;
    ParamByName('ATIME').AsString := aTime;

    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('vegetation_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TWeatherLog.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
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
    Add('WHERE weather_id = :anid');
    ParamByName('ANID').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TWeatherLog.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('weather_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FAtmosphericPressure := FieldByName('atmospheric_pressure').AsFloat;
    FCloudCover := FieldByName('cloud_cover').AsInteger;
    FNotes := FieldByName('notes').AsString;
    case FieldByName('precipitation').AsString of
      'N': FPrecipitation := wpNone;
      'F': FPrecipitation := wpFog;
      'M': FPrecipitation := wpMist;
      'D': FPrecipitation := wpDrizzle;
      'R': FPrecipitation := wpRain;
    else
      FPrecipitation := wpEmpty;
    end;
    FRainfall := FieldByName('rainfall').AsInteger;
    FRelativeHumidity := FieldByName('relative_humidity').AsFloat;
    FSampleDate := FieldByName('sample_date').AsDateTime;
    case FieldByName('sample_moment').AsString of
      'S': FSampleMoment := wmStart;
      'M': FSampleMoment := wmMiddle;
      'E': FSampleMoment := wmEnd;
    else
      FSampleMoment := wmNone;
    end;
    FSampleTime := FieldByName('sample_time').AsDateTime;
    FTemperature := FieldByName('temperature').AsFloat;
    FObserverId := FieldByName('observer_id').AsInteger;
    FWindSpeedBft := FieldByName('wind_speed_bft').AsInteger;
    FWindSpeedKmH := FieldByName('wind_speed_kmh').AsFloat;
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

procedure TWeatherLog.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
        ':relative_humidity, ' +
        ':atmospheric_pressure, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('sample_moment').AsString := SAMPLE_MOMENTS[FSampleMoment];
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('cloud_cover').AsInteger := FCloudCover;
      ParamByName('precipitation').AsString := PRECIPITATION_VALUES[FPrecipitation];
      ParamByName('rainfall').AsInteger := FRainfall;
      ParamByName('temperature').AsFloat := FTemperature;
      ParamByName('wind_speed_bft').AsInteger := FWindSpeedBft;
      ParamByName('wind_speed_kmh').AsFloat := FWindSpeedKmH;
      ParamByName('relative_humidity').AsFloat := FRelativeHumidity;
      SetFloatParam(ParamByName('atmospheric_pressure'), FAtmosphericPressure);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TWeatherLog.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TWeatherLog.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Date', FSampleDate);
    JSONObject.Add('Moment', SAMPLE_MOMENTS[FSampleMoment]);
    JSONObject.Add('Time', FSampleTime);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Atmospheric Pressure', FAtmosphericPressure);
    JSONObject.Add('Cloud Cover', FCloudCover);
    JSONObject.Add('Precipitation', PRECIPITATION_VALUES[FPrecipitation]);
    JSONObject.Add('Rainfall', FRainfall);
    JSONObject.Add('Relative Humidity', FRelativeHumidity);
    JSONObject.Add('Temperature', FTemperature);
    JSONObject.Add('Wind Speed (bft)', FWindSpeedBft);
    JSONObject.Add('Wind Speed (km/h)', FWindSpeedKmH);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TWeatherLog.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TWeatherLog.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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
        'relative_humidity = :relative_humidity, ' +
        'atmospheric_pressure = :atmospheric_pressure, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (weather_id = :weather_id)');
      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('sample_moment').AsString := SAMPLE_MOMENTS[FSampleMoment];
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('cloud_cover').AsInteger := FCloudCover;
      ParamByName('precipitation').AsString := PRECIPITATION_VALUES[FPrecipitation];
      ParamByName('rainfall').AsInteger := FRainfall;
      ParamByName('temperature').AsFloat := FTemperature;
      ParamByName('wind_speed_bft').AsInteger := FWindSpeedBft;
      ParamByName('wind_speed_kmh').AsFloat := FWindSpeedKmH;
      ParamByName('relative_humidity').AsFloat := FRelativeHumidity;
      SetFloatParam(ParamByName('atmospheric_pressure'), FAtmosphericPressure);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('weather_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

