{ Xolmis Media Data library

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

unit models_media;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types, models_sampling, models_geo;

type

  { TImageData }

  TImageData = class(TXolmisRecord)
  protected
    FImageDate: TDate;
    FImageTime: TTime;
    FImageType: TImageType;
    FFilename: String;
    FSubtitle: String;
    FAuthorId: Integer;
    FCoordinatePrecision: TCoordinatePrecision;
    FLongitude: Double;
    FLatitude: Double;
    FLocalityId: Integer;
    FTaxonId: Integer;
    FCaptureId: Integer;
    FIndividualId: Integer;
    FSurveyId: Integer;
    FSightingId: Integer;
    FNestId: Integer;
    FNestRevisionId: Integer;
    FEggId: Integer;
    FSpecimenId: Integer;
    FLicenseType: String;
    FLicenseYear: Integer;
    FLicenseOwner: String;
    FLicenseNotes: String;
    FLicenseUri: String;
    //FThumbnail: TBitmap;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(const FieldName: String; const Value: Variant): Boolean;
    function Diff(aOld: TImageData; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TImageData);
    function ToJSON: String;
  published
    property ImageDate: TDate read FImageDate write FImageDate;
    property ImageTime: TTime read FImageTime write FImageTime;
    property ImageType: TImageType read FImageType write FImageType;
    property Filename: String read FFilename write FFilename;
    property Subtitle: String read FSubtitle write FSubtitle;
    property AuthorId: Integer read FAuthorId write FAuthorId;
    property CoordinatePrecision: TCoordinatePrecision read FCoordinatePrecision write FCoordinatePrecision;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property NestId: Integer read FNestId write FNestId;
    property NestRevisionId: Integer read FNestRevisionId write FNestRevisionId;
    property EggId: Integer read FEggId write FEggId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property LicenseType: String read FLicenseType write FLicenseType;
    property LicenseYear: Integer read FLicenseYear write FLicenseYear;
    property LicenseOwner: String read FLicenseOwner write FLicenseOwner;
    property LicenseNotes: String read FLicenseNotes write FLicenseNotes;
    property LicenseUri: String read FLicenseUri write FLicenseUri;
  end;

  { TAudioData }

  TAudioData = class(TXolmisRecord)
  protected
    FRecordingDate: TDate;
    FRecordingTime: TTime;
    FAudioType: String;
    FFilename: String;
    FSubtitle: String;
    FAuthorId: Integer;
    FCoordinatePrecision: TCoordinatePrecision;
    FLongitude: Double;
    FLatitude: Double;
    FLocalityId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FSurveyId: Integer;
    FSightingId: Integer;
    FSpecimenId: Integer;
    FTemperature: Double;
    FCloudCover: Integer;
    FPrecipitation: TPrecipitation;
    FRelativeHumidity: Double;
    FWindSpeedBft: Integer;
    FSubjectsTally: Integer;
    FDistance: Double;
    FPlaybackUsed: Boolean;
    FContext: String;
    FHabitat: String;
    FRecorderModel: String;
    FMicModel: String;
    FFilterModel: String;
    FLicenseType: String;
    FLicenseYear: Integer;
    FLicenseOwner: String;
    FLicenseNotes: String;
    FLicenseUri: String;
    FFullName: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(const FieldName: String; const Value: Variant): Boolean;
    function Diff(aOld: TAudioData; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TAudioData);
    function ToJSON: String;
  published
    property RecordingDate: TDate read FRecordingDate write FRecordingDate;
    property RecordingTime: TTime read FRecordingTime write FRecordingTime;
    property AudioType: String read FAudioType write FAudioType;
    property Filename: String read FFilename write FFilename;
    property Subtitle: String read FSubtitle write FSubtitle;
    property AuthorId: Integer read FAuthorId write FAuthorId;
    property CoordinatePrecision: TCoordinatePrecision read FCoordinatePrecision write FCoordinatePrecision;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SightingId: Integer read FSightingId write FSightingId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property Temperature: Double read FTemperature write FTemperature;
    property CloudCover: Integer read FCloudCover write FCloudCover;
    property Precipitation: TPrecipitation read FPrecipitation write FPrecipitation;
    property RelativeHumidity: Double read FRelativeHumidity write FRelativeHumidity;
    property WindSpeedBft: Integer read FWindSpeedBft write FWindSpeedBft;
    property SubjectsTally: Integer read FSubjectsTally write FSubjectsTally;
    property Distance: Double read FDistance write FDistance;
    property PlaybackUsed: Boolean read FPlaybackUsed write FPlaybackUsed;
    property Context: String read FContext write FContext;
    property Habitat: String read FHabitat write FHabitat;
    property RecorderModel: String read FRecorderModel write FRecorderModel;
    property MicModel: String read FMicModel write FMicModel;
    property FilterModel: String read FFilterModel write FFilterModel;
    property LicenseType: String read FLicenseType write FLicenseType;
    property LicenseYear: Integer read FLicenseYear write FLicenseYear;
    property LicenseOwner: String read FLicenseOwner write FLicenseOwner;
    property LicenseNotes: String read FLicenseNotes write FLicenseNotes;
    property LicenseUri: String read FLicenseUri write FLicenseUri;
    property FullName: String read FFullName write FFullName;
    property Notes: String read FNotes write FNotes;
  end;

  { TDocumentData }

  TDocumentData = class(TXolmisRecord)
  protected
    FName: String;
    FDocumentDate: TDate;
    FDocumentTime: TTime;
    FDocumentType: String;
    FFilename: String;
    FAuthorId: Integer;
    FPermitId: Integer;
    FProjectId: Integer;
    FPersonId: Integer;
    FIndividualId: Integer;
    FCaptureId: Integer;
    FSightingId: Integer;
    FExpeditionId: Integer;
    FSurveyId: Integer;
    FNestId: Integer;
    FSpecimenId: Integer;
    FSamplingPlotId: Integer;
    FMethodId: Integer;
    FLicenseType: String;
    FLicenseYear: Integer;
    FLicenseOwner: String;
    FLicenseNotes: String;
    FLicenseUri: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(const FieldName: String; const Value: Variant): Boolean;
    function Diff(aOld: TDocumentData; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TDocumentData);
    function ToJSON: String;
  published
    property Name: String read FName write FName;
    property DocumentDate: TDate read FDocumentDate write FDocumentDate;
    property DocumentTime: TTime read FDocumentTime write FDocumentTime;
    property DocumentType: String read FDocumentType write FDocumentType;
    property FileName: String read FFilename write FFilename;
    property AuthorId: Integer read FAuthorId write FAuthorId;
    property PermitId: Integer read FPermitId write FPermitId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property PersonId: Integer read FPersonId write FPersonId;
    property CaptureId: Integer read FCaptureId write FCaptureId;
    property SightingId: Integer read FSightingId write FSightingId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property ExpeditionId: Integer read FExpeditionId write FExpeditionId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property NestId: Integer read FNestId write FNestId;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property SamplingPlotId: Integer read FSamplingPlotId write FSamplingPlotId;
    property MethodId: Integer read FMethodId write FMethodId;
    property LicenseType: String read FLicenseType write FLicenseType;
    property LicenseYear: Integer read FLicenseYear write FLicenseYear;
    property LicenseOwner: String read FLicenseOwner write FLicenseOwner;
    property LicenseNotes: String read FLicenseNotes write FLicenseNotes;
    property LicenseUri: String read FLicenseUri write FLicenseUri;
  end;

implementation

uses utils_global, utils_locale, utils_validations, data_columns, data_setparam, models_users, udm_main;

{ TImageData }

constructor TImageData.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TImageData.Clear;
begin
  inherited Clear;
  FImageDate := NullDate;
  FImageTime := NullTime;
  FImageType := itEmpty;
  FFilename := EmptyStr;
  FSubtitle := EmptyStr;
  FAuthorId := 0;
  FCoordinatePrecision := cpEmpty;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FLocalityId := 0;
  FTaxonId := 0;
  FCaptureId := 0;
  FIndividualId := 0;
  FSurveyId := 0;
  FSightingId := 0;
  FNestId := 0;
  FNestRevisionId := 0;
  FEggId := 0;
  FSpecimenId := 0;
  FLicenseType := EmptyStr;
  FLicenseYear := 0;
  FLicenseOwner := EmptyStr;
  FLicenseNotes := EmptyStr;
  FLicenseUri := EmptyStr;
end;

procedure TImageData.Copy(aFrom: TImageData);
begin
  FImageDate := aFrom.ImageDate;
  FImageTime := aFrom.ImageTime;
  FImageType := aFrom.ImageType;
  FFilename := aFrom.Filename;
  FSubtitle := aFrom.Subtitle;
  FAuthorId := aFrom.AuthorId;
  FCoordinatePrecision := aFrom.CoordinatePrecision;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FLocalityId := aFrom.LocalityId;
  FTaxonId := aFrom.TaxonId;
  FCaptureId := aFrom.CaptureId;
  FIndividualId := aFrom.IndividualId;
  FSurveyId := aFrom.SurveyId;
  FSightingId := aFrom.SightingId;
  FNestId := aFrom.NestId;
  FNestRevisionId := aFrom.NestRevisionId;
  FEggId := aFrom.EggId;
  FSpecimenId := aFrom.SpecimenId;
  FLicenseType := aFrom.LicenseType;
  FLicenseYear := aFrom.LicenseYear;
  FLicenseOwner := aFrom.LicenseOwner;
  FLicenseNotes := aFrom.LicenseNotes;
  FLicenseUri := aFrom.LicenseUri;
end;

procedure TImageData.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TImageData.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM images');
      Add('WHERE (image_id = :aid)');

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

function TImageData.Diff(aOld: TImageData; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.ImageDate, FImageDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.ImageTime, FImageTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.ImageType, FImageType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSubtitle, aOld.Subtitle, FSubtitle, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpecimenID, aOld.SpecimenId, FSpecimenId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestRevisionID, aOld.NestRevisionId, FNestRevisionId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TImageData.Find(const FieldName: String; const Value: Variant): Boolean;
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
        'image_id, ' +
        'image_date, ' +
        'image_time, ' +
        'image_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'locality_id, ' +
        'author_id, ' +
        'survey_id, ' +
        'sighting_id, ' +
        'nest_id, ' +
        'nest_revision_id, ' +
        'egg_id, ' +
        'specimen_id, ' +
        'image_filename, ' +
        'coordinate_precision, ' +
        'longitude, ' +
        'latitude, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'subtitle, ' +
        'image_thumbnail, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM images');
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

procedure TImageData.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'image_id, ' +
        'image_date, ' +
        'image_time, ' +
        'image_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'locality_id, ' +
        'author_id, ' +
        'survey_id, ' +
        'sighting_id, ' +
        'nest_id, ' +
        'nest_revision_id, ' +
        'egg_id, ' +
        'specimen_id, ' +
        'image_filename, ' +
        'coordinate_precision, ' +
        'longitude, ' +
        'latitude, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'subtitle, ' +
        'image_thumbnail, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM images');
    Add('WHERE image_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TImageData.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO images (' +
        'image_date, ' +
        'image_time, ' +
        'image_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'locality_id, ' +
        'author_id, ' +
        'survey_id, ' +
        'sighting_id, ' +
        'nest_id, ' +
        'nest_revision_id, ' +
        'egg_id, ' +
        'specimen_id, ' +
        'image_filename, ' +
        'coordinate_precision, ' +
        'longitude, ' +
        'latitude, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'subtitle, ' +
        'image_thumbnail, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        'date(:image_date), ' +
        'time(:image_time), ' +
        ':image_type, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':capture_id, ' +
        ':locality_id, ' +
        ':author_id, ' +
        ':survey_id, ' +
        ':sighting_id, ' +
        ':nest_id, ' +
        ':nest_revision_id, ' +
        ':egg_id, ' +
        ':specimen_id, ' +
        ':image_filename, ' +
        ':coordinate_precision, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':license_type, ' +
        ':license_year, ' +
        ':license_uri, ' +
        ':license_notes, ' +
        ':license_owner, ' +
        ':subtitle, ' +
        ':image_thumbnail, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetDateParam(ParamByName('image_date'), FImageDate);
      SetTimeParam(ParamByName('image_time'), FImageTime);
      ParamByName('image_type').AsString := IMAGE_TYPES[FImageType];
      ParamByName('image_filename').AsString := FFilename;
      if FFilename <> EmptyStr then
      begin
        { #todo : Insert image thumbnail using Params }
      end
      else
        ParamByName('image_thumbnail').Clear;
      SetStrParam(ParamByName('subtitle'), FSubtitle);
      SetForeignParam(ParamByName('author_id'), FAuthorId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[FCoordinatePrecision];
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('capture_id'), FCaptureId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('specimen_id'), FSpecimenId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('nest_revision_id'), FNestRevisionId);
      SetForeignParam(ParamByName('egg_id'), FEggId);
      SetStrParam(ParamByName('license_type'), FLicenseType);
      SetIntParam(ParamByName('license_year'), FLicenseYear);
      SetStrParam(ParamByName('license_owner'), FLicenseOwner);
      SetStrParam(ParamByName('license_notes'), FLicenseNotes);
      SetStrParam(ParamByName('license_uri'), FLicenseUri);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
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

procedure TImageData.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('image_id').AsInteger;
    FImageDate := FieldByName('image_date').AsDateTime;
    FImageTime := FieldByName('image_time').AsDateTime;
    case FieldByName('image_type').AsString of
      'flank': FImageType := itBirdInHandFlank;
      'belly': FImageType := itBirdInHandBelly;
      'back':  FImageType := itBirdInHandBack;
      'wing':  FImageType := itBirdInHandWing;
      'tail':  FImageType := itBirdInHandTail;
      'head':  FImageType := itBirdInHandHead;
      'feet':  FImageType := itBirdInHandFeet;
      'stand': FImageType := itFreeBirdStanding;
      'fly':   FImageType := itFreeBirdFlying;
      'swim':  FImageType := itFreeBirdSwimming;
      'forr':  FImageType := itFreeBirdForraging;
      'copul': FImageType := itFreeBirdCopulating;
      'build': FImageType := itFreeBirdBuildingNest;
      'disp':  FImageType := itFreeBirdDisplaying;
      'incub': FImageType := itFreeBirdIncubating;
      'vocal': FImageType := itFreeBirdVocalizing;
      'agon':  FImageType := itFreeBirdAgonistic;
      'dead':  FImageType := itDeadBird;
      'flock': FImageType := itBirdFlock;
      'nest':  FImageType := itBirdNest;
      'egg':   FImageType := itBirdEgg;
      'nstln': FImageType := itBirdNestling;
      'paras': FImageType := itEctoparasite;
      'fprnt': FImageType := itFootprint;
      'feath': FImageType := itFeather;
      'feces': FImageType := itFeces;
      'food':  FImageType := itFood;
      'envir': FImageType := itEnvironment;
      'fwork': FImageType := itFieldwork;
      'team':  FImageType := itTeam;
    else
      FImageType := itEmpty;
    end;
    FFilename := FieldByName('image_filename').AsString;
    FAuthorId := FieldByName('author_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FCaptureId := FieldByName('capture_id').AsInteger;
    FSightingId := FieldByName('sighting_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    FNestRevisionId := FieldByName('nest_revision_id').AsInteger;
    FEggId := FieldByName('egg_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    case FieldByName('coordinate_precision').AsString of
      'E': FCoordinatePrecision := cpExact;
      'A': FCoordinatePrecision := cpApproximated;
      'R': FCoordinatePrecision := cpReference;
    else
      FCoordinatePrecision := cpEmpty;
    end;
    FLongitude := FieldByName('longitude').AsFloat;
    FLatitude := FieldByName('latitude').AsFloat;
    FLicenseType := FieldByName('license_type').AsString;
    FLicenseYear := FieldByName('license_year').AsInteger;
    FLicenseOwner := FieldByName('license_owner').AsString;
    FLicenseNotes := FieldByName('license_notes').AsString;
    FLicenseUri := FieldByName('license_uri').AsString;
    // SQLite may store ImageDate and ImageTime data as ISO8601 string or Julian ImageDate real formats
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

procedure TImageData.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TImageData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Date', FImageDate);
    JSONObject.Add('Time', FImageTime);
    JSONObject.Add('Type', IMAGE_TYPES[FImageType]);
    JSONObject.Add('Filename', FFilename);
    JSONObject.Add('Subtitle', FSubtitle);
    JSONObject.Add('Author', FAuthorId);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Coordinate precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Capture', FCaptureId);
    JSONObject.Add('Sighting', FSightingId);
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Nest revision', FNestRevisionId);
    JSONObject.Add('Egg', FEggId);
    JSONObject.Add('License type', FLicenseType);
    JSONObject.Add('License year', FLicenseYear);
    JSONObject.Add('License owner', FLicenseOwner);
    JSONObject.Add('License notes', FLicenseNotes);
    JSONObject.Add('License URL', FLicenseUri);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TImageData.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TImageData.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE images SET ' +
        'image_date = date(:image_date), ' +
        'image_time = time(:image_time), ' +
        'image_type = :image_type, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'capture_id = :capture_id, ' +
        'locality_id = :locality_id, ' +
        'author_id = :author_id, ' +
        'survey_id = :survey_id, ' +
        'sighting_id = :sighting_id, ' +
        'nest_id = :nest_id, ' +
        'nest_revision_id = :nest_revision_id, ' +
        'egg_id = :egg_id, ' +
        'specimen_id = :specimen_id, ' +
        'image_filename = :image_filename, ' +
        'coordinate_precision = :coordinate_precision, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'license_type = :license_type, ' +
        'license_year = :license_year, ' +
        'license_uri = :license_uri, ' +
        'license_notes = :license_notes, ' +
        'license_owner = :license_owner, ' +
        'subtitle = :subtitle, ' +
        'image_thumbnail = :image_thumbnail, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec'') ');
      Add('WHERE (image_id = :image_id)');

      ParamByName('image_id').AsInteger := FId;
      SetDateParam(ParamByName('image_date'), FImageDate);
      SetTimeParam(ParamByName('image_time'), FImageTime);
      ParamByName('image_type').AsString := IMAGE_TYPES[FImageType];
      ParamByName('image_filename').AsString := FFilename;
      if FFilename <> EmptyStr then
      begin
        { #todo : Insert image thumbnail using Params }
      end
      else
        ParamByName('image_thumbnail').Clear;
      SetStrParam(ParamByName('subtitle'), FSubtitle);
      SetForeignParam(ParamByName('author_id'), FAuthorId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[FCoordinatePrecision];
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('capture_id'), FCaptureId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('specimen_id'), FSpecimenId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('nest_revision_id'), FNestRevisionId);
      SetForeignParam(ParamByName('egg_id'), FEggId);
      SetStrParam(ParamByName('license_type'), FLicenseType);
      SetIntParam(ParamByName('license_year'), FLicenseYear);
      SetStrParam(ParamByName('license_owner'), FLicenseOwner);
      SetStrParam(ParamByName('license_notes'), FLicenseNotes);
      SetStrParam(ParamByName('license_uri'), FLicenseUri);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;

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

{ TAudioData }

constructor TAudioData.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TAudioData.Clear;
begin
  inherited Clear;
  FRecordingDate := NullDate;
  FRecordingTime := NullTime;
  FAudioType := EmptyStr;
  FFilename := EmptyStr;
  FSubtitle := EmptyStr;
  FAuthorId := 0;
  FCoordinatePrecision := cpEmpty;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FLocalityId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FSurveyId := 0;
  FSightingId := 0;
  FSpecimenId := 0;
  FTemperature := 0.0;
  FCloudCover := 0;
  FPrecipitation := wpNone;
  FRelativeHumidity := 0.0;
  FWindSpeedBft := 0;
  FSubjectsTally := 0;
  FDistance := 0.0;
  FPlaybackUsed := False;
  FContext := EmptyStr;
  FHabitat := EmptyStr;
  FRecorderModel := EmptyStr;
  FMicModel := EmptyStr;
  FFilterModel := EmptyStr;
  FLicenseType := EmptyStr;
  FLicenseYear := 0;
  FLicenseOwner := EmptyStr;
  FLicenseNotes := EmptyStr;
  FLicenseUri := EmptyStr;
  FFullName := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TAudioData.Copy(aFrom: TAudioData);
begin
  FRecordingDate := aFrom.RecordingDate;
  FRecordingTime := aFrom.RecordingTime;
  FAudioType := aFrom.AudioType;
  FFilename := aFrom.Filename;
  FSubtitle := aFrom.Subtitle;
  FAuthorId := aFrom.AuthorId;
  FCoordinatePrecision := aFrom.CoordinatePrecision;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FLocalityId := aFrom.LocalityId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FSurveyId := aFrom.SurveyId;
  FSightingId := aFrom.SightingId;
  FSpecimenId := aFrom.SpecimenId;
  FTemperature := aFrom.Temperature;
  FCloudCover := aFrom.CloudCover;
  FPrecipitation := aFrom.Precipitation;
  FRelativeHumidity := aFrom.RelativeHumidity;
  FWindSpeedBft := aFrom.WindSpeedBft;
  FSubjectsTally := aFrom.SubjectsTally;
  FDistance := aFrom.Distance;
  FPlaybackUsed := aFrom.PlaybackUsed;
  FContext := aFrom.Context;
  FHabitat := aFrom.Habitat;
  FRecorderModel := aFrom.RecorderModel;
  FMicModel := aFrom.MicModel;
  FFilterModel := aFrom.FilterModel;
  FLicenseType := aFrom.LicenseType;
  FLicenseYear := aFrom.LicenseYear;
  FLicenseOwner := aFrom.LicenseOwner;
  FLicenseNotes := aFrom.LicenseNotes;
  FLicenseUri := aFrom.LicenseUri;
  FFullName := aFrom.FullName;
  FNotes := aFrom.Notes;
end;

procedure TAudioData.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TAudioData.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM audio_library');
      Add('WHERE (audio_id = :aid)');

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

function TAudioData.Diff(aOld: TAudioData; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.RecordingDate, FRecordingDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.RecordingTime, FRecordingTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.AudioType, FAudioType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSubtitle, aOld.Subtitle, FSubtitle, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpecimenID, aOld.SpecimenId, FSpecimenId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTemperatureC, aOld.Temperature, FTemperature, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCloudCover, aOld.CloudCover, FCloudCover, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPrecipitation, aOld.Precipitation, FPrecipitation, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRelativeHumidity, aOld.RelativeHumidity, FRelativeHumidity, R) then
    aList.Add(R);
  if FieldValuesDiff(rscWindBft, aOld.WindSpeedBft, FWindSpeedBft, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividuals, aOld.SubjectsTally, FSubjectsTally, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDistanceM, aOld.Distance, FDistance, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPlaybackUsed, aOld.PlaybackUsed, FPlaybackUsed, R) then
    aList.Add(R);
  if FieldValuesDiff(rscContext, aOld.Context, FContext, R) then
    aList.Add(R);
  if FieldValuesDiff(rscHabitat, aOld.Habitat, FHabitat, R) then
    aList.Add(R);
  if FieldValuesDiff(rscRecorderModel, aOld.RecorderModel, FRecorderModel, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMicModel, aOld.MicModel, FMicModel, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFilterModel, aOld.FilterModel, FFilterModel, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TAudioData.Find(const FieldName: String; const Value: Variant): Boolean;
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
        'audio_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'specimen_id, ' +
        'sighting_id, ' +
        'audio_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'longitude, ' +
        'latitude, ' +
        'temperature, ' +
        'cloud_cover, ' +
        'precipitation, ' +
        'relative_humidity, ' +
        'wind_speed, ' +
        'recording_context, ' +
        'playback_used, ' +
        'subjects_tally, ' +
        'habitat, ' +
        'recorder_model, ' +
        'mic_model, ' +
        'filter_model, ' +
        'distance, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'audio_file, ' +
        'subtitle, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM audio_library');
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

procedure TAudioData.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'audio_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'specimen_id, ' +
        'sighting_id, ' +
        'audio_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'longitude, ' +
        'latitude, ' +
        'temperature, ' +
        'cloud_cover, ' +
        'precipitation, ' +
        'relative_humidity, ' +
        'wind_speed, ' +
        'recording_context, ' +
        'playback_used, ' +
        'subjects_tally, ' +
        'habitat, ' +
        'recorder_model, ' +
        'mic_model, ' +
        'filter_model, ' +
        'distance, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'audio_file, ' +
        'subtitle, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM audio_library');
    Add('WHERE audio_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TAudioData.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO audio_library (' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'specimen_id, ' +
        'sighting_id, ' +
        'audio_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'longitude, ' +
        'latitude, ' +
        'temperature, ' +
        'cloud_cover, ' +
        'precipitation, ' +
        'relative_humidity, ' +
        'wind_speed, ' +
        'recording_context, ' +
        'playback_used, ' +
        'subjects_tally, ' +
        'habitat, ' +
        'recorder_model, ' +
        'mic_model, ' +
        'filter_model, ' +
        'distance, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'audio_file, ' +
        'subtitle, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':full_name, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':specimen_id, ' +
        ':sighting_id, ' +
        ':audio_type, ' +
        ':locality_id, ' +
        'date(:recording_date), ' +
        ':recorder_id, ' +
        'time(:recording_time), ' +
        ':longitude, ' +
        ':latitude, ' +
        ':temperature, ' +
        ':cloud_cover, ' +
        ':precipitation, ' +
        ':relative_humidity, ' +
        ':wind_speed, ' +
        ':recording_context, ' +
        ':playback_used, ' +
        ':subjects_tally, ' +
        ':habitat, ' +
        ':recorder_model, ' +
        ':mic_model, ' +
        ':filter_model, ' +
        ':distance, ' +
        ':license_type, ' +
        ':license_year, ' +
        ':license_uri, ' +
        ':license_notes, ' +
        ':license_owner, ' +
        ':audio_file, ' +
        ':subtitle, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetDateParam(ParamByName('recording_date'), FRecordingDate);
      SetTimeParam(ParamByName('recording_time'), FRecordingTime);
      ParamByName('audio_type').AsString := FAudioType;

      ParamByName('audio_file').AsString := FFilename;
      SetStrParam(ParamByName('subtitle'), FSubtitle);
      //SetForeignParam(ParamByName('author_id'), FAuthorId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[FCoordinatePrecision];
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('specimen_id'), FSpecimenId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('temperature').AsFloat := FTemperature;
      ParamByName('cloud_cover').AsInteger := FCloudCover;
      case FPrecipitation of
        wpNone:     ParamByName('precipitation').AsString := 'N';
        wpFog:      ParamByName('precipitation').AsString := 'F';
        wpMist:     ParamByName('precipitation').AsString := 'M';
        wpDrizzle:  ParamByName('precipitation').AsString := 'D';
        wpRain:     ParamByName('precipitation').AsString := 'R';
      else
        ParamByName('precipitation').Clear;
      end;
      ParamByName('relative_humidity').AsFloat := FRelativeHumidity;
      ParamByName('wind_speed').AsInteger := FWindSpeedBft;
      SetIntParam(ParamByName('subjects_tally'), FSubjectsTally);
      ParamByName('distance').AsFloat := FDistance;
      SetStrParam(ParamByName('recording_context'), FContext);
      SetStrParam(ParamByName('habitat'), FHabitat);
      ParamByName('playback_used').AsBoolean := FPlaybackUsed;
      SetStrParam(ParamByName('recorder_model'), FRecorderModel);
      SetStrParam(ParamByName('mic_model'), FMicModel);
      SetStrParam(ParamByName('filter_model'), FFilterModel);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('license_type'), FLicenseType);
      SetIntParam(ParamByName('license_year'), FLicenseYear);
      SetStrParam(ParamByName('license_owner'), FLicenseOwner);
      SetStrParam(ParamByName('license_notes'), FLicenseNotes);
      SetStrParam(ParamByName('license_uri'), FLicenseUri);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
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

procedure TAudioData.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('audio_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FRecordingDate := FieldByName('recording_date').AsDateTime;
    FRecordingTime := FieldByName('recording_time').AsDateTime;
    FAudioType := FieldByName('audio_type').AsString;
    FSubtitle := FieldByName('subtitle').AsString;
    FFilename := FieldByName('audio_file').AsString;
    FAuthorId := FieldByName('recorder_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FSurveyId := 0;
    FSightingId := FieldByName('sighting_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    //case FieldByName('coordinate_precision').AsString of
    //  'E': FCoordinatePrecision := cpExact;
    //  'A': FCoordinatePrecision := cpApproximated;
    //  'R': FCoordinatePrecision := cpReference;
    //else
    //  FCoordinatePrecision := cpEmpty;
    //end;
    FLongitude := FieldByName('longitude').AsFloat;
    FLatitude := FieldByName('latitude').AsFloat;
    FTemperature := FieldByName('temperature').AsFloat;
    FCloudCover := FieldByName('cloud_cover').AsInteger;
    case FieldByName('precipitation').AsString of
      'N': FPrecipitation := wpNone;
      'F': FPrecipitation := wpFog;
      'M': FPrecipitation := wpMist;
      'D': FPrecipitation := wpDrizzle;
      'R': FPrecipitation := wpRain;
    else
      FPrecipitation := wpEmpty;
    end;
    FRelativeHumidity := FieldByName('relative_humidity').AsInteger;
    FWindSpeedBft := FieldByName('wind_speed').AsInteger;
    FSubjectsTally := FieldByName('subjects_tally').AsInteger;
    FDistance := FieldByName('distance').AsFloat;
    FPlaybackUsed := FieldByName('playback_used').AsBoolean;
    FContext := FieldByName('recording_context').AsString;
    FHabitat := FieldByName('habitat').AsString;
    FRecorderModel := FieldByName('recorder_model').AsString;
    FMicModel := FieldByName('mic_model').AsString;
    FFilterModel := FieldByName('filter_model').AsString;
    FLicenseType := FieldByName('license_type').AsString;
    FLicenseYear := FieldByName('license_year').AsInteger;
    FLicenseOwner := FieldByName('license_owner').AsString;
    FLicenseNotes := FieldByName('license_notes').AsString;
    FLicenseUri := FieldByName('license_uri').AsString;
    FNotes := FieldByName('notes').AsString;
    // SQLite may store RecordingDate and RecordingTime data as ISO8601 string or Julian RecordingDate real formats
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

procedure TAudioData.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TAudioData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Fullname', FFullName);
    JSONObject.Add('Date', FRecordingDate);
    JSONObject.Add('Time', FRecordingTime);
    JSONObject.Add('Type', FAudioType);
    JSONObject.Add('Filename', FFilename);
    JSONObject.Add('Subtitle', FSubtitle);
    JSONObject.Add('Author', FAuthorId);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Coordinate precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Sighting', FSightingId);
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Temperature', FTemperature);
    JSONObject.Add('Cloud cover', FCloudCover);
    JSONObject.Add('Precipitation', PRECIPITATION_VALUES[FPrecipitation]);
    JSONObject.Add('Relative humidity', FRelativeHumidity);
    JSONObject.Add('Wind speed', FWindSpeedBft);
    JSONObject.Add('Individuals tally', FSubjectsTally);
    JSONObject.Add('Distance', FDistance);
    JSONObject.Add('Context', FContext);
    JSONObject.Add('Habitat', FHabitat);
    JSONObject.Add('Playback used', FPlaybackUsed);
    JSONObject.Add('Recorder model', FRecorderModel);
    JSONObject.Add('Microphone model', FMicModel);
    JSONObject.Add('Filter model', FFilterModel);
    JSONObject.Add('License type', FLicenseType);
    JSONObject.Add('License year', FLicenseYear);
    JSONObject.Add('License owner', FLicenseOwner);
    JSONObject.Add('License notes', FLicenseNotes);
    JSONObject.Add('License URL', FLicenseUri);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TAudioData.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TAudioData.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE audio_library SET ' +
        'full_name = :full_name, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'specimen_id = :specimen_id, ' +
        'sighting_id = :sighting_id, ' +
        'audio_type = :audio_type, ' +
        'locality_id = :locality_id, ' +
        'recording_date = date(:recording_date), ' +
        'recorder_id = :recorder_id, ' +
        'recording_time = time(:recording_time), ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'temperature = :temperature, ' +
        'cloud_cover = :cloud_cover, ' +
        'precipitation = :precipitation, ' +
        'relative_humidity = :relative_humidity, ' +
        'wind_speed = :wind_speed, ' +
        'recording_context = :recording_context, ' +
        'playback_used = :playback_used, ' +
        'subjects_tally = :subjects_tally, ' +
        'habitat = :habitat, ' +
        'recorder_model = :recorder_model, ' +
        'mic_model = :mic_model, ' +
        'filter_model = :filter_model, ' +
        'distance = :distance, ' +
        'license_type = :license_type, ' +
        'license_year = :license_year, ' +
        'license_uri = :license_uri, ' +
        'license_notes = :license_notes, ' +
        'license_owner = :license_owner, ' +
        'audio_file = :audio_file, ' +
        'subtitle = :subtitle, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec'') ');
      Add('WHERE (audio_id = :audio_id)');

      ParamByName('audio_id').AsInteger := FId;
      SetDateParam(ParamByName('recording_date'), FRecordingDate);
      SetTimeParam(ParamByName('recording_time'), FRecordingTime);
      ParamByName('audio_type').AsString := FAudioType;

      ParamByName('audio_file').AsString := FFilename;
      SetStrParam(ParamByName('subtitle'), FSubtitle);
      //SetForeignParam(ParamByName('author_id'), FAuthorId);
      SetForeignParam(ParamByName('locality_id'), FLocalityId);
      ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[FCoordinatePrecision];
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('specimen_id'), FSpecimenId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('temperature').AsFloat := FTemperature;
      ParamByName('cloud_cover').AsInteger := FCloudCover;
      case FPrecipitation of
        wpNone:     ParamByName('precipitation').AsString := 'N';
        wpFog:      ParamByName('precipitation').AsString := 'F';
        wpMist:     ParamByName('precipitation').AsString := 'M';
        wpDrizzle:  ParamByName('precipitation').AsString := 'D';
        wpRain:     ParamByName('precipitation').AsString := 'R';
      else
        ParamByName('precipitation').Clear;
      end;
      ParamByName('relative_humidity').AsFloat := FRelativeHumidity;
      ParamByName('wind_speed').AsInteger := FWindSpeedBft;
      SetIntParam(ParamByName('subjects_tally'), FSubjectsTally);
      ParamByName('distance').AsFloat := FDistance;
      SetStrParam(ParamByName('recording_context'), FContext);
      SetStrParam(ParamByName('habitat'), FHabitat);
      ParamByName('playback_used').AsBoolean := FPlaybackUsed;
      SetStrParam(ParamByName('recorder_model'), FRecorderModel);
      SetStrParam(ParamByName('mic_model'), FMicModel);
      SetStrParam(ParamByName('filter_model'), FFilterModel);
      SetStrParam(ParamByName('notes'), FNotes);
      SetStrParam(ParamByName('license_type'), FLicenseType);
      SetIntParam(ParamByName('license_year'), FLicenseYear);
      SetStrParam(ParamByName('license_owner'), FLicenseOwner);
      SetStrParam(ParamByName('license_notes'), FLicenseNotes);
      SetStrParam(ParamByName('license_uri'), FLicenseUri);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;

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

{ TDocumentData }

constructor TDocumentData.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TDocumentData.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FDocumentDate := NullDate;
  FDocumentTime := NullTime;
  FDocumentType := EmptyStr;
  FFilename := EmptyStr;
  FAuthorId := 0;
  FPermitId := 0;
  FProjectId := 0;
  FPersonId := 0;
  FIndividualId := 0;
  FCaptureId := 0;
  FSightingId := 0;
  FExpeditionId := 0;
  FSurveyId := 0;
  FNestId := 0;
  FSpecimenId := 0;
  FSamplingPlotId := 0;
  FMethodId := 0;
  FLicenseType := EmptyStr;
  FLicenseYear := 0;
  FLicenseOwner := EmptyStr;
  FLicenseNotes := EmptyStr;
  FLicenseUri := EmptyStr;
end;

procedure TDocumentData.Copy(aFrom: TDocumentData);
begin
  FName := aFrom.Name;
  FDocumentDate := aFrom.DocumentDate;
  FDocumentTime := aFrom.DocumentTime;
  FDocumentType := aFrom.DocumentType;
  FFilename := aFrom.FileName;
  FAuthorId := aFrom.AuthorId;
  FPermitId := aFrom.PermitId;
  FProjectId := aFrom.ProjectId;
  FPersonId := aFrom.PersonId;
  FIndividualId := aFrom.IndividualId;
  FCaptureId := aFrom.CaptureId;
  FSightingId := aFrom.SightingId;
  FExpeditionId := aFrom.ExpeditionId;
  FSurveyId := aFrom.SurveyId;
  FNestId := aFrom.NestId;
  FSpecimenId := aFrom.SpecimenId;
  FSamplingPlotId := aFrom.SamplingPlotId;
  FMethodId := aFrom.MethodId;
  FLicenseType := aFrom.LicenseType;
  FLicenseYear := aFrom.LicenseYear;
  FLicenseOwner := aFrom.LicenseOwner;
  FLicenseNotes := aFrom.LicenseNotes;
  FLicenseUri := aFrom.LicenseUri;
end;

procedure TDocumentData.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TDocumentData.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM documents');
      Add('WHERE (document_id = :aid)');

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

function TDocumentData.Diff(aOld: TDocumentData; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.DocumentDate, FDocumentDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.DocumentTime, FDocumentTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.DocumentType, FDocumentType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPermitID, aOld.PermitId, FPermitId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscExpeditionID, aOld.ExpeditionId, FExpeditionId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSpecimenID, aOld.SpecimenId, FSpecimenId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.SamplingPlotId, FSamplingPlotId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TDocumentData.Find(const FieldName: String; const Value: Variant): Boolean;
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
        'document_id, ' +
        'permit_id, ' +
        'project_id, ' +
        'person_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'sighting_id, ' +
        'specimen_id, ' +
        'expedition_id, ' +
        'survey_id, ' +
        'nest_id, ' +
        'net_station_id, ' +
        'method_id, ' +
        'document_type, ' +
        'document_name, ' +
        'document_path, ' +
        'document_date, ' +
        'document_time, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM documents');
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

procedure TDocumentData.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'document_id, ' +
        'permit_id, ' +
        'project_id, ' +
        'person_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'sighting_id, ' +
        'specimen_id, ' +
        'expedition_id, ' +
        'survey_id, ' +
        'nest_id, ' +
        'net_station_id, ' +
        'method_id, ' +
        'document_type, ' +
        'document_name, ' +
        'document_path, ' +
        'document_date, ' +
        'document_time, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM documents');
    Add('WHERE document_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TDocumentData.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO documents (' +
        'permit_id, ' +
        'project_id, ' +
        'person_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'sighting_id, ' +
        'specimen_id, ' +
        'expedition_id, ' +
        'survey_id, ' +
        'nest_id, ' +
        'net_station_id, ' +
        'method_id, ' +
        'document_type, ' +
        'document_name, ' +
        'document_path, ' +
        'document_date, ' +
        'document_time, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':permit_id, ' +
        ':project_id, ' +
        ':person_id, ' +
        ':individual_id, ' +
        ':capture_id, ' +
        ':sighting_id, ' +
        ':specimen_id, ' +
        ':expedition_id, ' +
        ':survey_id, ' +
        ':nest_id, ' +
        ':net_station_id, ' +
        ':method_id, ' +
        ':document_type, ' +
        ':document_name, ' +
        ':document_path, ' +
        'date(:document_date), ' +
        'time(:document_time), ' +
        ':license_type, ' +
        ':license_year, ' +
        ':license_uri, ' +
        ':license_notes, ' +
        ':license_owner, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetDateParam(ParamByName('document_date'), FDocumentDate);
      SetTimeParam(ParamByName('document_time'), FDocumentTime);
      ParamByName('document_name').AsString := FName;
      ParamByName('document_type').AsString := FDocumentType;
      ParamByName('document_path').AsString := FFilename;
      SetForeignParam(ParamByName('permit_id'), FPermitId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetForeignParam(ParamByName('person_id'), FPersonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('capture_id'), FCaptureId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('specimen_id'), FSpecimenId);
      SetForeignParam(ParamByName('expedition_id'), FExpeditionId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('net_station_id'), FSamplingPlotId);
      SetForeignParam(ParamByName('method_id'), FMethodId);
      SetStrParam(ParamByName('license_type'), FLicenseType);
      SetIntParam(ParamByName('license_year'), FLicenseYear);
      SetStrParam(ParamByName('license_owner'), FLicenseOwner);
      SetStrParam(ParamByName('license_notes'), FLicenseNotes);
      SetStrParam(ParamByName('license_uri'), FLicenseUri);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
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

procedure TDocumentData.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('document_id').AsInteger;
    FName := FieldByName('document_name').AsString;
    FDocumentDate := FieldByName('document_date').AsDateTime;
    FDocumentTime := FieldByName('document_time').AsDateTime;
    FDocumentType := FieldByName('document_type').AsString;
    FFilename := FieldByName('document_path').AsString;
    //FAuthorId := FieldByName('author_id').AsInteger;
    FPermitId := FieldByName('permit_id').AsInteger;
    FProjectId := FieldByName('project_id').AsInteger;
    FPersonId := FieldByName('person_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FCaptureId := FieldByName('capture_id').AsInteger;
    FSightingId := FieldByName('sighting_id').AsInteger;
    FExpeditionId := FieldByName('expedition_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FSamplingPlotId := FieldByName('net_station_id').AsInteger;
    FMethodId := FieldByName('method_id').AsInteger;
    FLicenseType := FieldByName('license_type').AsString;
    FLicenseYear := FieldByName('license_year').AsInteger;
    FLicenseOwner := FieldByName('license_owner').AsString;
    FLicenseNotes := FieldByName('license_notes').AsString;
    FLicenseUri := FieldByName('license_uri').AsString;
    // SQLite may store DocumentDate and DocumentTime data as ISO8601 string or Julian DocumentDate real formats
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

procedure TDocumentData.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TDocumentData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Date', FDocumentDate);
    JSONObject.Add('Time', FDocumentTime);
    JSONObject.Add('Name', FName);
    JSONObject.Add('Type', FDocumentType);
    JSONObject.Add('Filename', FFilename);
    JSONObject.Add('Permit', FPermitId);
    JSONObject.Add('Project', FProjectId);
    JSONObject.Add('Person', FPersonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Capture', FCaptureId);
    JSONObject.Add('Sighting', FSightingId);
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Expedition', FExpeditionId);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Sampling Plot', FSamplingPlotId);
    JSONObject.Add('Method', FMethodId);
    JSONObject.Add('License type', FLicenseType);
    JSONObject.Add('License year', FLicenseYear);
    JSONObject.Add('License owner', FLicenseOwner);
    JSONObject.Add('License notes', FLicenseNotes);
    JSONObject.Add('License URL', FLicenseUri);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TDocumentData.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TDocumentData.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE documents SET ' +
        'permit_id = :permit_id, ' +
        'project_id = :project_id, ' +
        'person_id = :person_id, ' +
        'individual_id = :individual_id, ' +
        'capture_id = :capture_id, ' +
        'sighting_id = :sighting_id, ' +
        'specimen_id = :specimen_id, ' +
        'expedition_id = :expedition_id, ' +
        'survey_id = :survey_id, ' +
        'nest_id = :nest_id, ' +
        'net_station_id = :net_station_id, ' +
        'method_id = :method_id, ' +
        'document_type = :document_type, ' +
        'document_name = :document_name, ' +
        'document_path = :document_path, ' +
        'document_date = date(:document_date), ' +
        'document_time = time(:document_time), ' +
        'license_type = :license_type, ' +
        'license_year = :license_year, ' +
        'license_uri = :license_uri, ' +
        'license_notes = :license_notes, ' +
        'license_owner = :license_owner, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (document_id = :document_id)');

      SetDateParam(ParamByName('document_date'), FDocumentDate);
      SetTimeParam(ParamByName('document_time'), FDocumentTime);
      ParamByName('document_name').AsString := FName;
      ParamByName('document_type').AsString := FDocumentType;
      ParamByName('document_path').AsString := FFilename;
      SetForeignParam(ParamByName('permit_id'), FPermitId);
      SetForeignParam(ParamByName('project_id'), FProjectId);
      SetForeignParam(ParamByName('person_id'), FPersonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('capture_id'), FCaptureId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('specimen_id'), FSpecimenId);
      SetForeignParam(ParamByName('expedition_id'), FExpeditionId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      SetForeignParam(ParamByName('nest_id'), FNestId);
      SetForeignParam(ParamByName('net_station_id'), FSamplingPlotId);
      SetForeignParam(ParamByName('method_id'), FMethodId);
      SetStrParam(ParamByName('license_type'), FLicenseType);
      SetIntParam(ParamByName('license_year'), FLicenseYear);
      SetStrParam(ParamByName('license_owner'), FLicenseOwner);
      SetStrParam(ParamByName('license_notes'), FLicenseNotes);
      SetStrParam(ParamByName('license_uri'), FLicenseUri);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('document_id').AsInteger := FId;

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

