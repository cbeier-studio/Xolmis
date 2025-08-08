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
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    procedure Update;
    function Diff(aOld: TSighting; var aList: TStrings): Boolean;
    function Find(aSurvey, aTaxon, aObserver: Integer): Boolean;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSighting);
    function ToJSON: String;
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

implementation

uses
  utils_system, utils_global, models_users, utils_validations, data_columns, data_setparam, data_getvalue,
  utils_locale, udm_main;

{ TSighting }

constructor TSighting.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
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

procedure TSighting.Copy(aFrom: TSighting);
begin
  FSurveyId := aFrom.SurveyId;
  FSightingDate := aFrom.SightingDate;
  FSightingTime := aFrom.SightingTime;
  FLocalityId := aFrom.LocalityId;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FObserverId := aFrom.ObserverId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FSubjectTally := aFrom.SubjectTally;
  FSubjectDistance := aFrom.SubjectDistance;
  FMethodId := aFrom.MethodId;
  FMackinnonListNumber := aFrom.MackinnonListNumber;
  FSubjectCaptured := aFrom.SubjectCaptured;
  FSubjectSeen := aFrom.SubjectSeen;
  FSubjectHeard := aFrom.SubjectHeard;
  FSubjectPhotographed := aFrom.SubjectPhotographed;
  FSubjectRecorded := aFrom.SubjectRecorded;
  FMalesTally := aFrom.MalesTally;
  FFemalesTally := aFrom.FemalesTally;
  FNotSexedTally := aFrom.NotSexedTally;
  FAdultsTally := aFrom.AdultsTally;
  FImmatureTally := aFrom.ImmatureTally;
  FNotAgedTally := aFrom.NotAgedTally;
  FRecapturesTally := aFrom.RecapturesTally;
  FNewCapturesTally := aFrom.NewCapturesTally;
  FUnbandedTally := aFrom.UnbandedTally;
  FDetectionType := aFrom.DetectionType;
  FBreedingStatus := aFrom.BreedingStatus;
  FNotSurveying := aFrom.NotSurveying;
  FIsOnEbird := aFrom.IsOnEbird;
  FNotes := aFrom.Notes;
end;

procedure TSighting.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSighting.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM sightings');
      Add('WHERE (sighting_id = :aid)');

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

procedure TSighting.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
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
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSighting.LoadFromDataSet(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('sighting_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FSightingDate := FieldByName('sighting_date').AsDateTime;
    FSightingTime := FieldByName('sighting_time').AsDateTime;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FObserverId := FieldByName('observer_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FSubjectTally := FieldByName('subjects_tally').AsInteger;
    FSubjectDistance := FieldByName('subject_distance').AsFloat;
    FMethodId := FieldByName('method_id').AsInteger;
    FMackinnonListNumber := FieldByName('mackinnon_list_num').AsInteger;
    FSubjectCaptured := FieldByName('subject_captured').AsBoolean;
    FSubjectSeen := FieldByName('subject_seen').AsBoolean;
    FSubjectHeard := FieldByName('subject_heard').AsBoolean;
    FSubjectPhotographed := FieldByName('subject_photographed').AsBoolean;
    FSubjectRecorded := FieldByName('subject_recorded').AsBoolean;
    FMalesTally := FieldByName('males_tally').AsString;
    FFemalesTally := FieldByName('females_tally').AsString;
    FNotSexedTally := FieldByName('not_sexed_tally').AsString;
    FAdultsTally := FieldByName('adults_tally').AsString;
    FImmatureTally := FieldByName('immatures_tally').AsString;
    FNotAgedTally := FieldByName('not_aged_tally').AsString;
    FRecapturesTally := FieldByName('recaptures_tally').AsInteger;
    FNewCapturesTally := FieldByName('new_captures_tally').AsInteger;
    FUnbandedTally := FieldByName('unbanded_tally').AsInteger;
    FDetectionType := FieldByName('detection_type').AsString;
    FBreedingStatus := FieldByName('breeding_status').AsString;
    FNotSurveying := FieldByName('not_surveying').AsBoolean;
    FIsOnEbird := FieldByName('ebird_available').AsBoolean;
    FNotes := FieldByName('notes').AsString;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), FInsertDate);
    GetTimeStamp(FieldByName('update_date'), FUpdateDate);
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSighting.Insert;
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

      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetDateParam(ParamByName('sighting_date'), FSightingDate);
      SetTimeParam(ParamByName('sighting_time'), FSightingTime);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('method_id'), FMethodId);
      SetIntParam(ParamByName('mackinnon_list_num'), FMackinnonListNumber);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetIntParam(ParamByName('subjects_tally'), FSubjectTally);
      SetFloatParam(ParamByName('subject_distance'), FSubjectDistance);
      ParamByName('subject_captured').AsBoolean := FSubjectCaptured;
      ParamByName('subject_seen').AsBoolean := FSubjectSeen;
      ParamByName('subject_heard').AsBoolean := FSubjectHeard;
      ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
      ParamByName('subject_recorded').AsBoolean := FSubjectRecorded;
      SetStrParam(ParamByName('males_tally'), FMalesTally);
      SetStrParam(ParamByName('females_tally'), FFemalesTally);
      SetStrParam(ParamByName('not_sexed_tally'), FNotSexedTally);
      SetStrParam(ParamByName('adults_tally'), FAdultsTally);
      SetStrParam(ParamByName('immatures_tally'), FImmatureTally);
      SetStrParam(ParamByName('not_aged_tally'), FNotAgedTally);
      SetIntParam(ParamByName('new_captures_tally'), FNewCapturesTally);
      SetIntParam(ParamByName('recaptures_tally'), FRecapturesTally);
      SetIntParam(ParamByName('unbanded_tally'), FUnbandedTally);
      SetStrParam(ParamByName('detection_type'), FDetectionType);
      SetStrParam(ParamByName('breeding_status'), FBreedingStatus);
      ParamByName('not_surveying').AsBoolean := FNotSurveying;
      ParamByName('ebird_available').AsBoolean := FIsOnEbird;
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

procedure TSighting.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSighting.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Date', FSightingDate);
    JSONObject.Add('Time', FSightingTime);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Subjects tally', FSubjectTally);
    JSONObject.Add('Subject distance', FSubjectDistance);
    JSONObject.Add('Method', FMethodId);
    JSONObject.Add('Mackinnon list number', FMackinnonListNumber);
    JSONObject.Add('Captured', FSubjectCaptured);
    JSONObject.Add('Seen', FSubjectSeen);
    JSONObject.Add('Heard', FSubjectHeard);
    JSONObject.Add('Photographed', FSubjectPhotographed);
    JSONObject.Add('Recorded', FSubjectRecorded);
    JSONObject.Add('Males', FMalesTally);
    JSONObject.Add('Females', FFemalesTally);
    JSONObject.Add('Not sexed', FNotSexedTally);
    JSONObject.Add('Adults', FAdultsTally);
    JSONObject.Add('Immatures', FImmatureTally);
    JSONObject.Add('Not aged', FNotAgedTally);
    JSONObject.Add('New captures', FNewCapturesTally);
    JSONObject.Add('Recaptures', FRecapturesTally);
    JSONObject.Add('Unbanded', FUnbandedTally);
    JSONObject.Add('Detection type', FDetectionType);
    JSONObject.Add('Breeding status', FBreedingStatus);
    JSONObject.Add('Not surveying', FNotSurveying);
    JSONObject.Add('Is on eBird', FIsOnEbird);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSighting.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSighting.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
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

      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetDateParam(ParamByName('sighting_date'), FSightingDate);
      SetTimeParam(ParamByName('sighting_time'), FSightingTime);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('method_id'), FMethodId);
      SetIntParam(ParamByName('mackinnon_list_num'), FMackinnonListNumber);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetIntParam(ParamByName('subjects_tally'), FSubjectTally);
      SetFloatParam(ParamByName('subject_distance'), FSubjectDistance);
      ParamByName('subject_captured').AsBoolean := FSubjectCaptured;
      ParamByName('subject_seen').AsBoolean := FSubjectSeen;
      ParamByName('subject_heard').AsBoolean := FSubjectHeard;
      ParamByName('subject_photographed').AsBoolean := FSubjectPhotographed;
      ParamByName('subject_recorded').AsBoolean := FSubjectRecorded;
      SetStrParam(ParamByName('males_tally'), FMalesTally);
      SetStrParam(ParamByName('females_tally'), FFemalesTally);
      SetStrParam(ParamByName('not_sexed_tally'), FNotSexedTally);
      SetStrParam(ParamByName('adults_tally'), FAdultsTally);
      SetStrParam(ParamByName('immatures_tally'), FImmatureTally);
      SetStrParam(ParamByName('not_aged_tally'), FNotAgedTally);
      SetIntParam(ParamByName('new_captures_tally'), FNewCapturesTally);
      SetIntParam(ParamByName('recaptures_tally'), FRecapturesTally);
      SetIntParam(ParamByName('unbanded_tally'), FUnbandedTally);
      SetStrParam(ParamByName('detection_type'), FDetectionType);
      SetStrParam(ParamByName('breeding_status'), FBreedingStatus);
      ParamByName('not_surveying').AsBoolean := FNotSurveying;
      ParamByName('ebird_available').AsBoolean := FIsOnEbird;
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('sighting_id').AsInteger := FId;

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

function TSighting.Diff(aOld: TSighting; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDate, aOld.SightingDate, FSightingDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SightingTime, FSightingTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividuals, aOld.SubjectTally, FSubjectTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDistanceM, aOld.SubjectDistance, FSubjectDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMackinnonList, aOld.MackinnonListNumber, FMackinnonListNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCaptured, aOld.SubjectCaptured, FSubjectCaptured, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSeen, aOld.SubjectSeen, FSubjectSeen, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHeard, aOld.SubjectHeard, FSubjectHeard, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPhotographed, aOld.SubjectPhotographed, FSubjectPhotographed, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAudioRecorded, aOld.SubjectRecorded, FSubjectRecorded, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMales, aOld.MalesTally, FMalesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFemales, aOld.FemalesTally, FFemalesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAdults, aOld.AdultsTally, FAdultsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscImmatures, aOld.ImmatureTally, FImmatureTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRecaptures, aOld.RecapturesTally, FRecapturesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNewCaptures, aOld.NewCapturesTally, FNewCapturesTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscUnbanded, aOld.UnbandedTally, FUnbandedTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDetectionType, aOld.DetectionType, FDetectionType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscBreedingCode, aOld.BreedingStatus, FBreedingStatus, R) then
    aList.Add(R);
  if FieldValuesDiff(rscOutOfSample, aOld.NotSurveying, FNotSurveying, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIsInEBird, aOld.IsOnEbird, FIsOnEbird, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSighting.Find(aSurvey, aTaxon, aObserver: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
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
    //  Add('and (time(AMO_HORA_INICIAL,''localtime'') =
    //    time('+QuotedStr(TimeToStr(Reg.RecordTime))+',''localtime''))');
//    GravaLogSQL(SQL);
    Open;
    Result := RecordCount > 0;
    if Result then
    begin
      GetData(FieldByName('sighting_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

