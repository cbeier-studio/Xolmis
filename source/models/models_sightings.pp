{ Xolmis Sightings model

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit models_sightings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types;

type

  { TSighting }

  TSighting = class(TXolmisRecord)
  protected
    FSurveyId: Integer;
    FSightingDate: TDate;
    FSightingTime: TTime;
    FLocalityId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FObserverId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FSubjectTally: Integer;
    FSubjectDistance: Double;
    FFlightHeight: Double;
    FFlightDirection: String;
    FMethodId: Integer;
    FMackinnonListNumber: Integer;
    FSubjectCaptured: Boolean;
    FSubjectSeen: Boolean;
    FSubjectHeard: Boolean;
    FSubjectPhotographed: Boolean;
    FSubjectRecorded: Boolean;
    FMalesTally: String;
    FFemalesTally: String;
    FNotSexedTally: String;
    FAdultsTally: String;
    FImmatureTally: String;
    FNotAgedTally: String;
    FRecapturesTally: Integer;
    FNewCapturesTally: Integer;
    FUnbandedTally: Integer;
    FDetectionType: String;
    FBreedingStatus: String;
    FNotSurveying: Boolean;
    FIsOnEbird: Boolean;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSighting; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TSighting): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingDate: TDate read FSightingDate write FSightingDate;
    property SightingTime: TTime read FSightingTime write FSightingTime;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property ObserverId: Integer read FObserverId write FObserverId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SubjectTally: Integer read FSubjectTally write FSubjectTally;
    property SubjectDistance: Double read FSubjectDistance write FSubjectDistance;
    property FlightHeight: Double read FFlightHeight write FFlightHeight;
    property FlightDirection: String read FFlightDirection write FFlightDirection;
    property MethodId: Integer read FMethodId write FMethodId;
    property MackinnonListNumber: Integer read FMackinnonListNumber write FMackinnonListNumber;
    property SubjectCaptured: Boolean read FSubjectCaptured write FSubjectCaptured;
    property SubjectSeen: Boolean read FSubjectSeen write FSubjectSeen;
    property SubjectHeard: Boolean read FSubjectHeard write FSubjectHeard;
    property SubjectPhotographed: Boolean read FSubjectPhotographed write FSubjectPhotographed;
    property SubjectRecorded: Boolean read FSubjectRecorded write FSubjectRecorded;
    property MalesTally: String read FMalesTally write FMalesTally;
    property FemalesTally: String read FFemalesTally write FFemalesTally;
    property NotSexedTally: String read FNotSexedTally write FNotSexedTally;
    property AdultsTally: String read FAdultsTally write FAdultsTally;
    property ImmatureTally: String read FImmatureTally write FImmatureTally;
    property NotAgedTally: String read FNotAgedTally write FNotAgedTally;
    property RecapturesTally: Integer read FRecapturesTally write FRecapturesTally;
    property NewCapturesTally: Integer read FNewCapturesTally write FNewCapturesTally;
    property UnbandedTally: Integer read FUnbandedTally write FUnbandedTally;
    property DetectionType: String read FDetectionType write FDetectionType;
    property BreedingStatus: String read FBreedingStatus write FBreedingStatus;
    property NotSurveying: Boolean read FNotSurveying write FNotSurveying;
    property IsOnEbird: Boolean read FIsOnEbird write FIsOnEbird;
    property Notes: String read FNotes write FNotes;
  end;

  { TSightingRepository }

  TSightingRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByCombo(const aSurvey, aTaxon, aObserver: Integer; E: TSighting);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  utils_locale, utils_system, utils_global, utils_validations, models_users,
  data_consts, data_columns, data_setparam, data_getvalue,
  udm_main;

{ TSighting }

constructor TSighting.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSighting.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSighting then
  begin
    FSurveyId := TSighting(Source).SurveyId;
    FSightingDate := TSighting(Source).SightingDate;
    FSightingTime := TSighting(Source).SightingTime;
    FLocalityId := TSighting(Source).LocalityId;
    FLatitude := TSighting(Source).Latitude;
    FLongitude := TSighting(Source).Longitude;
    FObserverId := TSighting(Source).ObserverId;
    FTaxonId := TSighting(Source).TaxonId;
    FIndividualId := TSighting(Source).IndividualId;
    FSubjectTally := TSighting(Source).SubjectTally;
    FSubjectDistance := TSighting(Source).SubjectDistance;
    FFlightHeight := TSighting(Source).FlightHeight;
    FFlightDirection := TSighting(Source).FlightDirection;
    FMethodId := TSighting(Source).MethodId;
    FMackinnonListNumber := TSighting(Source).MackinnonListNumber;
    FSubjectCaptured := TSighting(Source).SubjectCaptured;
    FSubjectSeen := TSighting(Source).SubjectSeen;
    FSubjectHeard := TSighting(Source).SubjectHeard;
    FSubjectPhotographed := TSighting(Source).SubjectPhotographed;
    FSubjectRecorded := TSighting(Source).SubjectRecorded;
    FMalesTally := TSighting(Source).MalesTally;
    FFemalesTally := TSighting(Source).FemalesTally;
    FNotSexedTally := TSighting(Source).NotSexedTally;
    FAdultsTally := TSighting(Source).AdultsTally;
    FImmatureTally := TSighting(Source).ImmatureTally;
    FNotAgedTally := TSighting(Source).NotAgedTally;
    FRecapturesTally := TSighting(Source).RecapturesTally;
    FNewCapturesTally := TSighting(Source).NewCapturesTally;
    FUnbandedTally := TSighting(Source).UnbandedTally;
    FDetectionType := TSighting(Source).DetectionType;
    FBreedingStatus := TSighting(Source).BreedingStatus;
    FNotSurveying := TSighting(Source).NotSurveying;
    FIsOnEbird := TSighting(Source).IsOnEbird;
    FNotes := TSighting(Source).Notes;
  end;
end;

procedure TSighting.Clear;
begin
  inherited;
  FSurveyId := 0;
  FSightingDate := NullDate;
  FSightingTime := NullTime;
  FLocalityId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FObserverId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FSubjectTally := 0;
  FSubjectDistance := 0.0;
  FFlightHeight := 0.0;
  FFlightDirection := EmptyStr;
  FMethodId := 0;
  FMackinnonListNumber := 0;
  FSubjectCaptured := False;
  FSubjectSeen := False;
  FSubjectHeard := False;
  FSubjectPhotographed := False;
  FSubjectRecorded := False;
  FMalesTally := EmptyStr;
  FFemalesTally := EmptyStr;
  FNotSexedTally := EmptyStr;
  FAdultsTally := EmptyStr;
  FImmatureTally := EmptyStr;
  FNotAgedTally := EmptyStr;
  FRecapturesTally := 0;
  FNewCapturesTally := 0;
  FUnbandedTally := 0;
  FDetectionType := EmptyStr;
  FBreedingStatus := EmptyStr;
  FNotSurveying := False;
  FIsOnEbird := False;
  FNotes := EmptyStr;
end;

function TSighting.Clone: TXolmisRecord;
begin
  Result := TSighting(inherited Clone);
end;

function TSighting.Diff(const aOld: TSighting; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDate, aOld.SightingDate, FSightingDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.SightingTime, FSightingTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividuals, aOld.SubjectTally, FSubjectTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDistanceM, aOld.SubjectDistance, FSubjectDistance, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFlightHeight, aOld.FlightHeight, FFlightHeight, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFlightDirection, aOld.FlightDirection, FFlightDirection, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMackinnonList, aOld.MackinnonListNumber, FMackinnonListNumber, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCaptured, aOld.SubjectCaptured, FSubjectCaptured, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSeen, aOld.SubjectSeen, FSubjectSeen, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHeard, aOld.SubjectHeard, FSubjectHeard, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPhotographed, aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAudioRecorded, aOld.SubjectRecorded, FSubjectRecorded, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMales, aOld.MalesTally, FMalesTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFemales, aOld.FemalesTally, FFemalesTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAdults, aOld.AdultsTally, FAdultsTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscImmatures, aOld.ImmatureTally, FImmatureTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRecaptures, aOld.RecapturesTally, FRecapturesTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNewCaptures, aOld.NewCapturesTally, FNewCapturesTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscUnbanded, aOld.UnbandedTally, FUnbandedTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDetectionType, aOld.DetectionType, FDetectionType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscBreedingCode, aOld.BreedingStatus, FBreedingStatus, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscOutOfSample, aOld.NotSurveying, FNotSurveying, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIsInEBird, aOld.IsOnEbird, FIsOnEbird, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSighting.EqualsTo(const Other: TSighting): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSighting.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FSurveyId             := Obj.Get('survey_id', 0);
    FSightingDate         := StrToDate(Obj.Get('sighting_date', NULL_DATE_STR));
    FSightingTime         := StrToTime(Obj.Get('sighting_time', NULL_TIME_STR));
    FLocalityId           := Obj.Get('locality_id', 0);
    FLongitude            := Obj.Get('longitude', 0.0);
    FLatitude             := Obj.Get('latitude', 0.0);
    FObserverId           := Obj.Get('observer_id', 0);
    FTaxonId              := Obj.Get('taxon_id', 0);
    FIndividualId         := Obj.Get('individual_id', 0);
    FSubjectTally         := Obj.Get('subject_tally', 0);
    FSubjectDistance      := Obj.Get('subject_distance', 0.0);
    FFlightHeight         := Obj.Get('flight_height', 0.0);
    FFlightDirection      := Obj.Get('flight_direction', '');
    FMethodId             := Obj.Get('method_id', 0);
    FMackinnonListNumber  := Obj.Get('mackinnon_list_number', 0);
    FSubjectCaptured      := Obj.Get('captured', False);
    FSubjectSeen          := Obj.Get('seen', False);
    FSubjectHeard         := Obj.Get('heard', False);
    FSubjectPhotographed  := Obj.Get('photographed', False);
    FSubjectRecorded      := Obj.Get('recorded', False);
    FMalesTally           := Obj.Get('males_tally', '');
    FFemalesTally         := Obj.Get('females_tally', '');
    FNotSexedTally        := Obj.Get('not_sexed_tally', '');
    FAdultsTally          := Obj.Get('adults_tally', '');
    FImmatureTally        := Obj.Get('immatures_tally', '');
    FNotAgedTally         := Obj.Get('not_aged_tally', '');
    FNewCapturesTally     := Obj.Get('new_captures_tally', 0);
    FRecapturesTally      := Obj.Get('recaptures_tally', 0);
    FUnbandedTally        := Obj.Get('unbanded_tally', 0);
    FDetectionType        := Obj.Get('detection_type', '');
    FBreedingStatus       := Obj.Get('breeding_status', '');
    FNotSurveying         := Obj.Get('not_surveying', False);
    FIsOnEbird            := Obj.Get('is_on_ebird', False);
    FNotes                := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TSighting.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('sighting_date', DateToStr(FSightingDate));
    JSONObject.Add('sighting_time', TimeToStr(FSightingTime));
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('observer_id', FObserverId);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('subject_tally', FSubjectTally);
    JSONObject.Add('subject_distance', FSubjectDistance);
    JSONObject.Add('flight_height', FFlightHeight);
    JSONObject.Add('flight_direction', FFlightDirection);
    JSONObject.Add('method_id', FMethodId);
    JSONObject.Add('mackinnon_list_number', FMackinnonListNumber);
    JSONObject.Add('captured', FSubjectCaptured);
    JSONObject.Add('seen', FSubjectSeen);
    JSONObject.Add('heard', FSubjectHeard);
    JSONObject.Add('photographed', FSubjectPhotographed);
    JSONObject.Add('recorded', FSubjectRecorded);
    JSONObject.Add('males_tally', FMalesTally);
    JSONObject.Add('females_tally', FFemalesTally);
    JSONObject.Add('not_sexed_tally', FNotSexedTally);
    JSONObject.Add('adults_tally', FAdultsTally);
    JSONObject.Add('immatures_tally', FImmatureTally);
    JSONObject.Add('not_aged_tally', FNotAgedTally);
    JSONObject.Add('new_captures_tally', FNewCapturesTally);
    JSONObject.Add('recaptures_tally', FRecapturesTally);
    JSONObject.Add('unbanded_tally', FUnbandedTally);
    JSONObject.Add('detection_type', FDetectionType);
    JSONObject.Add('breeding_status', FBreedingStatus);
    JSONObject.Add('not_surveying', FNotSurveying);
    JSONObject.Add('is_on_ebird', FIsOnEbird);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSighting.ToString: String;
begin
  Result := Format('Sighting(Id=%d, SurveyId=%d, SightingDate=%s, SightingTime=%s, LocalityId=%d, ' +
    'Longitude=%f, Latitude=%f, ObserverId=%d, TaxonId=%d, IndividualId=%d, SubjectTally=%d, SubjectDistance=%f, ' +
    'FlightHeight=%f, FlightDirection=%s, ' +
    'MethodId=%d, MackinnonListNumber=%d, Captured=%s, Seen=%s, Heard=%s, Photographed=%s, Recorded=%s, ' +
    'MalesTally=%s, FemalesTally=%s, NotSexedTally=%s, AdultsTally=%s, ImmaturesTally=%s, NotAgedTally=%s, ' +
    'NewCapturesTally=%d, RecapturesTally=%d, UnbandedTally=%d, DetectionType=%s, BreedingStatus=%s, ' +
    'NotSurveying=%s, IsOnEbird=%s, Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FSurveyId, DateToStr(FSightingDate), TimeToStr(FSightingTime), FLocalityId, FLongitude, FLatitude,
    FObserverId, FTaxonId, FIndividualId, FSubjectTally, FSubjectDistance,
    FFlightHeight, FFlightDirection, FMethodId, FMackinnonListNumber,
    BoolToStr(FSubjectCaptured, 'True', 'False'), BoolToStr(FSubjectSeen, 'True', 'False'),
    BoolToStr(FSubjectHeard, 'True', 'False'), BoolToStr(FSubjectPhotographed, 'True', 'False'),
    BoolToStr(FSubjectRecorded, 'True', 'False'), FMalesTally, FFemalesTally, FNotSexedTally, FAdultsTally,
    FImmatureTally, FNotAgedTally, FNewCapturesTally, FRecapturesTally, FUnbandedTally, FDetectionType,
    FBreedingStatus, BoolToStr(FNotSurveying, 'True', 'False'), BoolToStr(FIsOnEbird, 'True', 'False'), FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSighting.Validate(out Msg: string): Boolean;
begin
  if FTaxonId <= 0 then
  begin
    Msg := 'TaxonId required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSightingRepository }

procedure TSightingRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSighting;
begin
  if not (E is TSighting) then
    raise Exception.Create('Delete: Expected TSighting');

  R := TSighting(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSightingRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_SIGHTING_ID;
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

function TSightingRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_SIGHTING_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSightingRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = (COL_SIGHTING_ID, COL_FULL_NAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSighting) then
    raise Exception.Create('FindBy: Expected TSighting');

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
      'sighting_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'sighting_date, ' +
      'sighting_time, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'method_id, ' +
      'mackinnon_list_num, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'subjects_tally, ' +
      'subject_distance, ' +
      'flight_height, ' +
      'flight_direction, ' +
      'subject_seen, ' +
      'subject_heard, ' +
      'subject_photographed, ' +
      'subject_recorded, ' +
      'subject_captured, ' +
      'males_tally, ' +
      'females_tally, ' +
      'not_sexed_tally, ' +
      'adults_tally, ' +
      'immatures_tally, ' +
      'not_aged_tally, ' +
      'new_captures_tally, ' +
      'recaptures_tally, ' +
      'unbanded_tally, ' +
      'detection_type, ' +
      'breeding_status, ' +
      'not_surveying, ' +
      'ebird_available, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM sightings');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSighting(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSightingRepository.FindByCombo(const aSurvey, aTaxon, aObserver: Integer; E: TSighting);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT sighting_id FROM sightings');
    Add('WHERE (survey_id = :asurvey)');
    Add('AND (taxon_id = :ataxon)');
    if aObserver > 0 then
      Add('AND (observer_id = :aobserver)');
    ParamByName('ASURVEY').AsInteger := aSurvey;
    ParamByName('ATAXON').AsInteger := aTaxon;
    if aObserver > 0 then
      ParamByName('AOBSERVER').AsInteger := aObserver;
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

procedure TSightingRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSighting) then
    raise Exception.Create('GetById: Expected TSighting');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
      'sighting_id, ' +
      'survey_id, ' +
      'individual_id, ' +
      'sighting_date, ' +
      'sighting_time, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'method_id, ' +
      'mackinnon_list_num, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'subjects_tally, ' +
      'subject_distance, ' +
      'flight_height, ' +
      'flight_direction, ' +
      'subject_seen, ' +
      'subject_heard, ' +
      'subject_photographed, ' +
      'subject_recorded, ' +
      'subject_captured, ' +
      'males_tally, ' +
      'females_tally, ' +
      'not_sexed_tally, ' +
      'adults_tally, ' +
      'immatures_tally, ' +
      'not_aged_tally, ' +
      'new_captures_tally, ' +
      'recaptures_tally, ' +
      'unbanded_tally, ' +
      'detection_type, ' +
      'breeding_status, ' +
      'not_surveying, ' +
      'ebird_available, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'datetime(insert_date, ''localtime'') AS insert_date, ' +
      'datetime(update_date, ''localtime'') AS update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM sightings');
    Add('WHERE sighting_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSighting(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSightingRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSighting;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSighting) then
    raise Exception.Create('Hydrate: Expected TSighting');

  R := TSighting(E);
  with aDataSet do
  begin
    R.Id := FieldByName('sighting_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.SightingDate := FieldByName('sighting_date').AsDateTime;
    R.SightingTime := FieldByName('sighting_time').AsDateTime;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.ObserverId := FieldByName('observer_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.SubjectTally := FieldByName('subjects_tally').AsInteger;
    R.SubjectDistance := FieldByName('subject_distance').AsFloat;
    R.FlightHeight := FieldByName('flight_height').AsFloat;
    R.FlightDirection := FieldByName('flight_direction').AsString;
    R.MethodId := FieldByName('method_id').AsInteger;
    R.MackinnonListNumber := FieldByName('mackinnon_list_num').AsInteger;
    R.SubjectCaptured := FieldByName('subject_captured').AsBoolean;
    R.SubjectSeen := FieldByName('subject_seen').AsBoolean;
    R.SubjectHeard := FieldByName('subject_heard').AsBoolean;
    R.SubjectPhotographed := FieldByName('subject_photographed').AsBoolean;
    R.SubjectRecorded := FieldByName('subject_recorded').AsBoolean;
    R.MalesTally := FieldByName('males_tally').AsString;
    R.FemalesTally := FieldByName('females_tally').AsString;
    R.NotSexedTally := FieldByName('not_sexed_tally').AsString;
    R.AdultsTally := FieldByName('adults_tally').AsString;
    R.ImmatureTally := FieldByName('immatures_tally').AsString;
    R.NotAgedTally := FieldByName('not_aged_tally').AsString;
    R.RecapturesTally := FieldByName('recaptures_tally').AsInteger;
    R.NewCapturesTally := FieldByName('new_captures_tally').AsInteger;
    R.UnbandedTally := FieldByName('unbanded_tally').AsInteger;
    R.DetectionType := FieldByName('detection_type').AsString;
    R.BreedingStatus := FieldByName('breeding_status').AsString;
    R.NotSurveying := FieldByName('not_surveying').AsBoolean;
    R.IsOnEbird := FieldByName('ebird_available').AsBoolean;
    R.Notes := FieldByName('notes').AsString;
    R.UserInserted := FieldByName('user_inserted').AsInteger;
    R.UserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Exported := FieldByName('exported_status').AsBoolean;
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSightingRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSighting;
begin
  if not (E is TSighting) then
    raise Exception.Create('Insert: Expected TSighting');

  R := TSighting(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO sightings (' +
      'survey_id, ' +
      'individual_id, ' +
      'sighting_date, ' +
      'sighting_time, ' +
      'locality_id, ' +
      'longitude, ' +
      'latitude, ' +
      'method_id, ' +
      'mackinnon_list_num, ' +
      'observer_id, ' +
      'taxon_id, ' +
      'subjects_tally, ' +
      'subject_distance, ' +
      'flight_height, ' +
      'flight_direction, ' +
      'subject_seen, ' +
      'subject_heard, ' +
      'subject_photographed, ' +
      'subject_recorded, ' +
      'subject_captured, ' +
      'males_tally, ' +
      'females_tally, ' +
      'not_sexed_tally, ' +
      'adults_tally, ' +
      'immatures_tally, ' +
      'not_aged_tally, ' +
      'new_captures_tally, ' +
      'recaptures_tally, ' +
      'unbanded_tally, ' +
      'detection_type, ' +
      'breeding_status, ' +
      'not_surveying, ' +
      'ebird_available, ' +
      'full_name, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':survey_id, ' +
      ':individual_id, ' +
      'date(:sighting_date), ' +
      //'(CASE WHEN :sighting_time IS NULL THEN NULL ELSE time(:sighting_time) END),' +
      'time(:sighting_time), ' +
      ':locality_id, ' +
      ':longitude, ' +
      ':latitude, ' +
      ':method_id, ' +
      ':mackinnon_list_num, ' +
      ':observer_id, ' +
      ':taxon_id, ' +
      ':subjects_tally, ' +
      ':subject_distance, ' +
      ':flight_height, ' +
      ':flight_direction, ' +
      ':subject_seen, ' +
      ':subject_heard, ' +
      ':subject_photographed, ' +
      ':subject_recorded, ' +
      ':subject_captured, ' +
      ':males_tally, ' +
      ':females_tally, ' +
      ':not_sexed_tally, ' +
      ':adults_tally, ' +
      ':immatures_tally, ' +
      ':not_aged_tally, ' +
      ':new_captures_tally, ' +
      ':recaptures_tally, ' +
      ':unbanded_tally, ' +
      ':detection_type, ' +
      ':breeding_status, ' +
      ':not_surveying, ' +
      ':ebird_available, ' +
      ':full_name, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'',''subsec''))');

    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetDateParam(ParamByName('sighting_date'), R.SightingDate);
    SetTimeParam(ParamByName('sighting_time'), R.SightingTime);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('method_id'), R.MethodId);
    SetIntParam(ParamByName('mackinnon_list_num'), R.MackinnonListNumber);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetIntParam(ParamByName('subjects_tally'), R.SubjectTally);
    SetFloatParam(ParamByName('subject_distance'), R.SubjectDistance);
    SetFloatParam(ParamByName('flight_height'), R.FlightHeight);
    SetStrParam(ParamByName('flight_direction'), R.FlightDirection);
    ParamByName('subject_captured').AsBoolean := R.SubjectCaptured;
    ParamByName('subject_seen').AsBoolean := R.SubjectSeen;
    ParamByName('subject_heard').AsBoolean := R.SubjectHeard;
    ParamByName('subject_photographed').AsBoolean := R.SubjectPhotographed;
    ParamByName('subject_recorded').AsBoolean := R.SubjectRecorded;
    SetStrParam(ParamByName('males_tally'), R.MalesTally);
    SetStrParam(ParamByName('females_tally'), R.FemalesTally);
    SetStrParam(ParamByName('not_sexed_tally'), R.NotSexedTally);
    SetStrParam(ParamByName('adults_tally'), R.AdultsTally);
    SetStrParam(ParamByName('immatures_tally'), R.ImmatureTally);
    SetStrParam(ParamByName('not_aged_tally'), R.NotAgedTally);
    SetIntParam(ParamByName('new_captures_tally'), R.NewCapturesTally);
    SetIntParam(ParamByName('recaptures_tally'), R.RecapturesTally);
    SetIntParam(ParamByName('unbanded_tally'), R.UnbandedTally);
    SetStrParam(ParamByName('detection_type'), R.DetectionType);
    SetStrParam(ParamByName('breeding_status'), R.BreedingStatus);
    ParamByName('not_surveying').AsBoolean := R.NotSurveying;
    ParamByName('ebird_available').AsBoolean := R.IsOnEbird;
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

function TSightingRepository.TableName: string;
begin
  Result := TBL_SIGHTINGS;
end;

procedure TSightingRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSighting;
begin
  if not (E is TSighting) then
    raise Exception.Create('Update: Expected TSighting');

  R := TSighting(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSightingRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE sightings SET ' +
      'survey_id = :survey_id, ' +
      'individual_id = :individual_id, ' +
      'sighting_date = date(:sighting_date), ' +
      //'sighting_time = (CASE WHEN :sighting_time IS NULL THEN NULL ELSE time(:sighting_time) END),' +
      'sighting_time = time(:sighting_time), ' +
      'locality_id = :locality_id, ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'method_id = :method_id, ' +
      'mackinnon_list_num = :mackinnon_list_num, ' +
      'observer_id = :observer_id, ' +
      'taxon_id = :taxon_id, ' +
      'subjects_tally = :subjects_tally, ' +
      'subject_distance = :subject_distance, ' +
      'flight_height = :flight_height, ' +
      'flight_direction = :flight_direction, ' +
      'subject_seen = :subject_seen, ' +
      'subject_heard = :subject_heard, ' +
      'subject_photographed = :subject_photographed, ' +
      'subject_recorded = :subject_recorded, ' +
      'subject_captured = :subject_captured, ' +
      'males_tally = :males_tally, ' +
      'females_tally = :females_tally, ' +
      'not_sexed_tally = :not_sexed_tally, ' +
      'adults_tally = :adults_tally, ' +
      'immatures_tally = :immatures_tally, ' +
      'not_aged_tally = :not_aged_tally, ' +
      'new_captures_tally = :new_captures_tally, ' +
      'recaptures_tally = :recaptures_tally, ' +
      'unbanded_tally = :unbanded_tally, ' +
      'detection_type = :detection_type, ' +
      'breeding_status = :breeding_status, ' +
      'not_surveying = :not_surveying, ' +
      'ebird_available = :ebird_available, ' +
      'full_name = :full_name, ' +
      'notes = :notes, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (sighting_id = :sighting_id)');

    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetDateParam(ParamByName('sighting_date'), R.SightingDate);
    SetTimeParam(ParamByName('sighting_time'), R.SightingTime);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('method_id'), R.MethodId);
    SetIntParam(ParamByName('mackinnon_list_num'), R.MackinnonListNumber);
    SetForeignParam(ParamByName('observer_id'), R.ObserverId);
    SetIntParam(ParamByName('subjects_tally'), R.SubjectTally);
    SetFloatParam(ParamByName('subject_distance'), R.SubjectDistance);
    SetFloatParam(ParamByName('flight_height'), R.FlightHeight);
    SetStrParam(ParamByName('flight_direction'), R.FlightDirection);
    ParamByName('subject_captured').AsBoolean := R.SubjectCaptured;
    ParamByName('subject_seen').AsBoolean := R.SubjectSeen;
    ParamByName('subject_heard').AsBoolean := R.SubjectHeard;
    ParamByName('subject_photographed').AsBoolean := R.SubjectPhotographed;
    ParamByName('subject_recorded').AsBoolean := R.SubjectRecorded;
    SetStrParam(ParamByName('males_tally'), R.MalesTally);
    SetStrParam(ParamByName('females_tally'), R.FemalesTally);
    SetStrParam(ParamByName('not_sexed_tally'), R.NotSexedTally);
    SetStrParam(ParamByName('adults_tally'), R.AdultsTally);
    SetStrParam(ParamByName('immatures_tally'), R.ImmatureTally);
    SetStrParam(ParamByName('not_aged_tally'), R.NotAgedTally);
    SetIntParam(ParamByName('new_captures_tally'), R.NewCapturesTally);
    SetIntParam(ParamByName('recaptures_tally'), R.RecapturesTally);
    SetIntParam(ParamByName('unbanded_tally'), R.UnbandedTally);
    SetStrParam(ParamByName('detection_type'), R.DetectionType);
    SetStrParam(ParamByName('breeding_status'), R.BreedingStatus);
    ParamByName('not_surveying').AsBoolean := R.NotSurveying;
    ParamByName('ebird_available').AsBoolean := R.IsOnEbird;
    SetStrParam(ParamByName('notes'), R.Notes);
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('sighting_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

