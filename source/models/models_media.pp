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
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, models_record_types, utils_global;

type
  TAttachMediaType = (amtImages, amtAudios, amtVideos, amtDocuments);
  TAttachMediaTypes = set of TAttachMediaType;

  TMediaAttachment = record
    Loaded: Boolean;
    TaxonId: Integer;
    IndividualId: Integer;
    CaptureId: Integer;
    FeatherId: Integer;
    LocalityId: Integer;
    AuthorId: Integer;
    SurveyId: Integer;
    SightingId: Integer;
    NestId: Integer;
    NestRevisionId: Integer;
    EggId: Integer;
    SpecimenId: Integer;
    PermitId: Integer;
    ProjectId: Integer;
    ExpeditionId: Integer;
    SamplingPlotId: Integer;
    MethodId: Integer;
  end;

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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TImageData; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TImageData): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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

  { TImageRepository }

  TImageRepository = class(TXolmisRepository)
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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TAudioData; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TAudioData): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
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

  { TAudioRepository }

  TAudioRepository = class(TXolmisRepository)
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

  { TDocumentData }

  TDocumentData = class(TXolmisRecord)
  protected
    FName: String;
    FDocumentDate: TDate;
    FDocumentTime: TTime;
    FDocumentType: TFileCategory;
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
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TDocumentData; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TDocumentData): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property DocumentDate: TDate read FDocumentDate write FDocumentDate;
    property DocumentTime: TTime read FDocumentTime write FDocumentTime;
    property DocumentType: TFileCategory read FDocumentType write FDocumentType;
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

  { TDocumentRepository }

  TDocumentRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByMethod(const aFilePath: String; const aMethodId: Integer; E: TDocumentData);
    procedure FindByIndividual(const aFilePath: String; const aIndividualId: Integer; E: TDocumentData);
    procedure FindByCapture(const aFilePath: String; const aCaptureId: Integer; E: TDocumentData);
    procedure FindByExpedition(const aFilePath: String; const aExpeditionId: Integer; E: TDocumentData);
    procedure FindBySurvey(const aFilePath: String; const aSurveyId: Integer; E: TDocumentData);
    procedure FindBySighting(const aFilePath: String; const aSightingId: Integer; E: TDocumentData);
    procedure FindByNest(const aFilePath: String; const aNestId: Integer; E: TDocumentData);
    procedure FindBySpecimen(const aFilePath: String; const aSpecimenId: Integer; E: TDocumentData);
    procedure FindBySamplingPlot(const aFilePath: String; const aSamplingPlotId: Integer; E: TDocumentData);
    procedure FindByProject(const aFilePath: String; const aProjectId: Integer; E: TDocumentData);
    procedure FindByPermit(const aFilePath: String; const aPermitId: Integer; E: TDocumentData);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TVideoData }

  TVideoData = class(TXolmisRecord)
  protected
    FRecordingDate: TDate;
    FRecordingTime: TTime;
    FVideoType: String;
    FFilename: String;
    FSubtitle: String;
    FAuthorId: Integer;
    FLongitude: Double;
    FLatitude: Double;
    FLocalityId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FCaptureId: Integer;
    FSurveyId: Integer;
    FSightingId: Integer;
    FNestId: Integer;
    FNestRevisionId: Integer;
    FDistance: Double;
    FContext: String;
    FHabitat: String;
    FCameraModel: String;
    FLicenseType: String;
    FLicenseYear: Integer;
    FLicenseOwner: String;
    FLicenseNotes: String;
    FLicenseUri: String;
    FFullName: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TVideoData; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TVideoData): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property RecordingDate: TDate read FRecordingDate write FRecordingDate;
    property RecordingTime: TTime read FRecordingTime write FRecordingTime;
    property VideoType: String read FVideoType write FVideoType;
    property Filename: String read FFilename write FFilename;
    property Subtitle: String read FSubtitle write FSubtitle;
    property AuthorId: Integer read FAuthorId write FAuthorId;
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
    property Distance: Double read FDistance write FDistance;
    property Context: String read FContext write FContext;
    property Habitat: String read FHabitat write FHabitat;
    property CameraModel: String read FCameraModel write FCameraModel;
    property LicenseType: String read FLicenseType write FLicenseType;
    property LicenseYear: Integer read FLicenseYear write FLicenseYear;
    property LicenseOwner: String read FLicenseOwner write FLicenseOwner;
    property LicenseNotes: String read FLicenseNotes write FLicenseNotes;
    property LicenseUri: String read FLicenseUri write FLicenseUri;
    property FullName: String read FFullName write FFullName;
    property Notes: String read FNotes write FNotes;
  end;

  { TVideoRepository }

  TVideoRepository = class(TXolmisRepository)
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

implementation

uses
  utils_locale, utils_validations, data_columns, data_setparam, models_users, data_consts,
  data_getvalue, udm_main;

{ TImageData }

constructor TImageData.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TImageData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TImageData then
  begin
    FImageDate := TImageData(Source).ImageDate;
    FImageTime := TImageData(Source).ImageTime;
    FImageType := TImageData(Source).ImageType;
    FFilename := TImageData(Source).Filename;
    FSubtitle := TImageData(Source).Subtitle;
    FAuthorId := TImageData(Source).AuthorId;
    FCoordinatePrecision := TImageData(Source).CoordinatePrecision;
    FLongitude := TImageData(Source).Longitude;
    FLatitude := TImageData(Source).Latitude;
    FLocalityId := TImageData(Source).LocalityId;
    FTaxonId := TImageData(Source).TaxonId;
    FCaptureId := TImageData(Source).CaptureId;
    FIndividualId := TImageData(Source).IndividualId;
    FSurveyId := TImageData(Source).SurveyId;
    FSightingId := TImageData(Source).SightingId;
    FNestId := TImageData(Source).NestId;
    FNestRevisionId := TImageData(Source).NestRevisionId;
    FEggId := TImageData(Source).EggId;
    FSpecimenId := TImageData(Source).SpecimenId;
    FLicenseType := TImageData(Source).LicenseType;
    FLicenseYear := TImageData(Source).LicenseYear;
    FLicenseOwner := TImageData(Source).LicenseOwner;
    FLicenseNotes := TImageData(Source).LicenseNotes;
    FLicenseUri := TImageData(Source).LicenseUri;
  end;
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

function TImageData.Clone: TXolmisRecord;
begin
  Result := TImageData(inherited Clone);
end;

function TImageData.Diff(const aOld: TImageData; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.ImageDate, FImageDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.ImageTime, FImageTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.ImageType, FImageType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubtitle, aOld.Subtitle, FSubtitle, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSpecimenID, aOld.SpecimenId, FSpecimenId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestRevisionID, aOld.NestRevisionId, FNestRevisionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TImageData.EqualsTo(const Other: TImageData): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TImageData.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FImageDate := Obj.Get('image_date', NullDate);
    FImageTime := Obj.Get('image_time', NullTime);
    case Obj.Get('image_type', '') of
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
    FFilename   := Obj.Get('filename', '');
    FSubtitle   := Obj.Get('subtitle', '');
    FAuthorId   := Obj.Get('author_id', 0);
    FLocalityId := Obj.Get('locality_id', 0);
    case Obj.Get('coordinate_precision', '') of
      'E': FCoordinatePrecision := cpExact;
      'A': FCoordinatePrecision := cpApproximated;
      'R': FCoordinatePrecision := cpReference;
    else
      FCoordinatePrecision := cpEmpty;
    end;
    FLongitude      := Obj.Get('longitude', 0.0);
    FLatitude       := Obj.Get('latitude', 0.0);
    FTaxonId        := Obj.Get('taxon_id', 0);
    FIndividualId   := Obj.Get('individual_id', 0);
    FCaptureId      := Obj.Get('capture_id', 0);
    FSightingId     := Obj.Get('sighting_id', 0);
    FSpecimenId     := Obj.Get('specimen_id', 0);
    FSurveyId       := Obj.Get('survey_id', 0);
    FNestId         := Obj.Get('nest_id', 0);
    FNestRevisionId := Obj.Get('nest_revision_id', 0);
    FEggId          := Obj.Get('egg_id', 0);
    FLicenseType    := Obj.Get('license_type', '');
    FLicenseYear    := Obj.Get('license_year', 0);
    FLicenseOwner   := Obj.Get('license_owner', '');
    FLicenseNotes   := Obj.Get('license_notes', '');
    FLicenseUri     := Obj.Get('license_url', '');
  finally
    Obj.Free;
  end;
end;

function TImageData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('image_date', FImageDate);
    JSONObject.Add('image_time', FImageTime);
    JSONObject.Add('image_type', IMAGE_TYPES[FImageType]);
    JSONObject.Add('filename', FFilename);
    JSONObject.Add('subtitle', FSubtitle);
    JSONObject.Add('author_id', FAuthorId);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('coordinate_precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('capture_id', FCaptureId);
    JSONObject.Add('sighting_id', FSightingId);
    JSONObject.Add('specimen_id', FSpecimenId);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('nest_revision_id', FNestRevisionId);
    JSONObject.Add('egg_id', FEggId);
    JSONObject.Add('license_type', FLicenseType);
    JSONObject.Add('license_year', FLicenseYear);
    JSONObject.Add('license_owner', FLicenseOwner);
    JSONObject.Add('license_notes', FLicenseNotes);
    JSONObject.Add('license_url', FLicenseUri);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TImageData.ToString: String;
begin
  Result := Format('ImageData(Id=%d, ImageDate=%s, ImageTime=%s, ImageType=%s, Filename=%s, Subtitle=%s, AuthorId=%d, ' +
    'LocalityId=%d, CoordinatePrecision=%s, Longitude=%f, Latitude=%f, TaxonId=%d, IndividualId=%d, CaptureId=%d, ' +
    'SightingId=%d, SpecimenId=%d, SurveyId=%d, NestId=%d, NestRevisionId=%d, EggId=%d, LicenseType=%s, ' +
    'LicenseYear=%d, LicenseOwner=%s, LicenseNotes=%s, LicenseUri=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, DateToStr(FImageDate), TimeToStr(FImageTime), IMAGE_TYPES[FImageType], FFilename, FSubtitle, FAuthorId,
    FLocalityId, COORDINATE_PRECISIONS[FCoordinatePrecision], FLongitude, FLatitude, FTaxonId, FIndividualId,
    FCaptureId, FSightingId, FSpecimenId, FSurveyId, FNestId, FNestRevisionId, FEggId, FLicenseType,
    FLicenseYear, FLicenseOwner, FLicenseNotes, FLicenseUri,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TImageData.Validate(out Msg: string): Boolean;
begin
  if FFilename = EmptyStr then
  begin
    Msg := 'Filename required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TImageRepository }

procedure TImageRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TImageData;
begin
  if not (E is TImageData) then
    raise Exception.Create('Delete: Expected TImageData');

  R := TImageData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TImageRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_IMAGE_ID;
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

function TImageRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_IMAGE_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TImageRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_IMAGE_ID, COL_SUBTITLE, COL_IMAGE_FILENAME); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TImageData) then
    raise Exception.Create('FindBy: Expected TImageData');

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
      Hydrate(Qry, TImageData(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TImageRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TImageData) then
    raise Exception.Create('GetById: Expected TImageData');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TImageData(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TImageRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TImageData;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TImageData) then
    raise Exception.Create('Hydrate: Expected TImageData');

  R := TImageData(E);
  with aDataSet do
  begin
    R.Id := FieldByName('image_id').AsInteger;
    R.ImageDate := FieldByName('image_date').AsDateTime;
    R.ImageTime := FieldByName('image_time').AsDateTime;
    case FieldByName('image_type').AsString of
      'flank': R.ImageType := itBirdInHandFlank;
      'belly': R.ImageType := itBirdInHandBelly;
      'back':  R.ImageType := itBirdInHandBack;
      'wing':  R.ImageType := itBirdInHandWing;
      'tail':  R.ImageType := itBirdInHandTail;
      'head':  R.ImageType := itBirdInHandHead;
      'feet':  R.ImageType := itBirdInHandFeet;
      'stand': R.ImageType := itFreeBirdStanding;
      'fly':   R.ImageType := itFreeBirdFlying;
      'swim':  R.ImageType := itFreeBirdSwimming;
      'forr':  R.ImageType := itFreeBirdForraging;
      'copul': R.ImageType := itFreeBirdCopulating;
      'build': R.ImageType := itFreeBirdBuildingNest;
      'disp':  R.ImageType := itFreeBirdDisplaying;
      'incub': R.ImageType := itFreeBirdIncubating;
      'vocal': R.ImageType := itFreeBirdVocalizing;
      'agon':  R.ImageType := itFreeBirdAgonistic;
      'dead':  R.ImageType := itDeadBird;
      'flock': R.ImageType := itBirdFlock;
      'nest':  R.ImageType := itBirdNest;
      'egg':   R.ImageType := itBirdEgg;
      'nstln': R.ImageType := itBirdNestling;
      'paras': R.ImageType := itEctoparasite;
      'fprnt': R.ImageType := itFootprint;
      'feath': R.ImageType := itFeather;
      'feces': R.ImageType := itFeces;
      'food':  R.ImageType := itFood;
      'envir': R.ImageType := itEnvironment;
      'fwork': R.ImageType := itFieldwork;
      'team':  R.ImageType := itTeam;
    else
      R.ImageType := itEmpty;
    end;
    R.Filename := FieldByName('image_filename').AsString;
    R.AuthorId := FieldByName('author_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.CaptureId := FieldByName('capture_id').AsInteger;
    R.SightingId := FieldByName('sighting_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.NestRevisionId := FieldByName('nest_revision_id').AsInteger;
    R.EggId := FieldByName('egg_id').AsInteger;
    R.SpecimenId := FieldByName('specimen_id').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    case FieldByName('coordinate_precision').AsString of
      'E': R.CoordinatePrecision := cpExact;
      'A': R.CoordinatePrecision := cpApproximated;
      'R': R.CoordinatePrecision := cpReference;
    else
      R.CoordinatePrecision := cpEmpty;
    end;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.LicenseType := FieldByName('license_type').AsString;
    R.LicenseYear := FieldByName('license_year').AsInteger;
    R.LicenseOwner := FieldByName('license_owner').AsString;
    R.LicenseNotes := FieldByName('license_notes').AsString;
    R.LicenseUri := FieldByName('license_uri').AsString;
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

procedure TImageRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TImageData;
begin
  if not (E is TImageData) then
    raise Exception.Create('Insert: Expected TImageData');

  R := TImageData(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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

    SetDateParam(ParamByName('image_date'), R.ImageDate);
    SetTimeParam(ParamByName('image_time'), R.ImageTime);
    ParamByName('image_type').AsString := IMAGE_TYPES[R.ImageType];
    ParamByName('image_filename').AsString := R.Filename;
    if R.Filename <> EmptyStr then
    begin
      { #todo : Insert image thumbnail using Params }
    end
    else
      ParamByName('image_thumbnail').Clear;
    SetStrParam(ParamByName('subtitle'), R.Subtitle);
    SetForeignParam(ParamByName('author_id'), R.AuthorId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[R.CoordinatePrecision];
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('specimen_id'), R.SpecimenId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('nest_revision_id'), R.NestRevisionId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
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

function TImageRepository.TableName: string;
begin
  Result := TBL_IMAGES;
end;

procedure TImageRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TImageData;
begin
  if not (E is TImageData) then
    raise Exception.Create('Update: Expected TImageData');

  R := TImageData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TImageRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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

    SetDateParam(ParamByName('image_date'), R.ImageDate);
    SetTimeParam(ParamByName('image_time'), R.ImageTime);
    ParamByName('image_type').AsString := IMAGE_TYPES[R.ImageType];
    ParamByName('image_filename').AsString := R.Filename;
    if R.Filename <> EmptyStr then
    begin
      { #todo : Insert image thumbnail using Params }
    end
    else
      ParamByName('image_thumbnail').Clear;
    SetStrParam(ParamByName('subtitle'), R.Subtitle);
    SetForeignParam(ParamByName('author_id'), R.AuthorId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[R.CoordinatePrecision];
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('specimen_id'), R.SpecimenId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('nest_revision_id'), R.NestRevisionId);
    SetForeignParam(ParamByName('egg_id'), R.EggId);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('image_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TAudioData }

constructor TAudioData.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TAudioData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TAudioData then
  begin
    FRecordingDate := TAudioData(Source).RecordingDate;
    FRecordingTime := TAudioData(Source).RecordingTime;
    FAudioType := TAudioData(Source).AudioType;
    FFilename := TAudioData(Source).Filename;
    FSubtitle := TAudioData(Source).Subtitle;
    FAuthorId := TAudioData(Source).AuthorId;
    FCoordinatePrecision := TAudioData(Source).CoordinatePrecision;
    FLongitude := TAudioData(Source).Longitude;
    FLatitude := TAudioData(Source).Latitude;
    FLocalityId := TAudioData(Source).LocalityId;
    FTaxonId := TAudioData(Source).TaxonId;
    FIndividualId := TAudioData(Source).IndividualId;
    FSurveyId := TAudioData(Source).SurveyId;
    FSightingId := TAudioData(Source).SightingId;
    FSpecimenId := TAudioData(Source).SpecimenId;
    FTemperature := TAudioData(Source).Temperature;
    FCloudCover := TAudioData(Source).CloudCover;
    FPrecipitation := TAudioData(Source).Precipitation;
    FRelativeHumidity := TAudioData(Source).RelativeHumidity;
    FWindSpeedBft := TAudioData(Source).WindSpeedBft;
    FSubjectsTally := TAudioData(Source).SubjectsTally;
    FDistance := TAudioData(Source).Distance;
    FPlaybackUsed := TAudioData(Source).PlaybackUsed;
    FContext := TAudioData(Source).Context;
    FHabitat := TAudioData(Source).Habitat;
    FRecorderModel := TAudioData(Source).RecorderModel;
    FMicModel := TAudioData(Source).MicModel;
    FFilterModel := TAudioData(Source).FilterModel;
    FLicenseType := TAudioData(Source).LicenseType;
    FLicenseYear := TAudioData(Source).LicenseYear;
    FLicenseOwner := TAudioData(Source).LicenseOwner;
    FLicenseNotes := TAudioData(Source).LicenseNotes;
    FLicenseUri := TAudioData(Source).LicenseUri;
    FFullName := TAudioData(Source).FullName;
    FNotes := TAudioData(Source).Notes;
  end;
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

function TAudioData.Clone: TXolmisRecord;
begin
  Result := TAudioData(inherited Clone);
end;

function TAudioData.Diff(const aOld: TAudioData; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.RecordingDate, FRecordingDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.RecordingTime, FRecordingTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.AudioType, FAudioType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubtitle, aOld.Subtitle, FSubtitle, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCoordinatePrecision, aOld.CoordinatePrecision, FCoordinatePrecision, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSpecimenID, aOld.SpecimenId, FSpecimenId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTemperatureC, aOld.Temperature, FTemperature, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCloudCover, aOld.CloudCover, FCloudCover, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPrecipitation, aOld.Precipitation, FPrecipitation, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRelativeHumidity, aOld.RelativeHumidity, FRelativeHumidity, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscWindBft, aOld.WindSpeedBft, FWindSpeedBft, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividuals, aOld.SubjectsTally, FSubjectsTally, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDistanceM, aOld.Distance, FDistance, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPlaybackUsed, aOld.PlaybackUsed, FPlaybackUsed, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscContext, aOld.Context, FContext, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHabitat, aOld.Habitat, FHabitat, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscRecorderModel, aOld.RecorderModel, FRecorderModel, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMicModel, aOld.MicModel, FMicModel, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFilterModel, aOld.FilterModel, FFilterModel, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TAudioData.EqualsTo(const Other: TAudioData): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TAudioData.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName       := Obj.Get('full_name', '');
    FRecordingDate  := Obj.Get('recording_date', NullDate);
    FRecordingTime  := Obj.Get('recording_time', NullTime);
    FAudioType      := Obj.Get('audio_type', '');
    FFilename       := Obj.Get('filename', '');
    FSubtitle       := Obj.Get('subtitle', '');
    FAuthorId       := Obj.Get('author_id', 0);
    FLocalityId     := Obj.Get('locality_id', 0);
    case Obj.Get('coordinate_precision', '') of
      'E': FCoordinatePrecision := cpExact;
      'A': FCoordinatePrecision := cpApproximated;
      'R': FCoordinatePrecision := cpReference;
    else
      FCoordinatePrecision := cpEmpty;
    end;
    FLongitude    := Obj.Get('longitude', 0.0);
    FLatitude     := Obj.Get('latitude', 0.0);
    FTaxonId      := Obj.Get('taxon_id', 0);
    FIndividualId := Obj.Get('individual_id', 0);
    FSightingId   := Obj.Get('sighting_id', 0);
    FSpecimenId   := Obj.Get('specimen_id', 0);
    FSurveyId     := Obj.Get('survey_id', 0);
    FTemperature  := Obj.Get('temperature', 0.0);
    FCloudCover   := Obj.Get('cloud_cover', 0);
    case Obj.Get('precipitation', '') of
      'N': FPrecipitation := wpNone;
      'F': FPrecipitation := wpFog;
      'M': FPrecipitation := wpMist;
      'D': FPrecipitation := wpDrizzle;
      'R': FPrecipitation := wpRain;
    else
      FPrecipitation := wpEmpty;
    end;
    FRelativeHumidity := Obj.Get('relative_humidity', 0.0);
    FWindSpeedBft     := Obj.Get('wind_speed', 0);
    FSubjectsTally    := Obj.Get('individuals_tally', 0);
    FDistance         := Obj.Get('distance', 0.0);
    FContext          := Obj.Get('context', '');
    FHabitat          := Obj.Get('habitat', '');
    FPlaybackUsed     := Obj.Get('playback_used', False);
    FRecorderModel    := Obj.Get('recorder_model', '');
    FMicModel         := Obj.Get('microphone_model', '');
    FFilterModel      := Obj.Get('filter_model', '');
    FLicenseType      := Obj.Get('license_type', '');
    FLicenseYear      := Obj.Get('license_year', 0);
    FLicenseOwner     := Obj.Get('license_owner', '');
    FLicenseNotes     := Obj.Get('license_notes', '');
    FLicenseUri       := Obj.Get('license_url', '');
    FNotes            := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TAudioData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('recording_date', FRecordingDate);
    JSONObject.Add('recording_time', FRecordingTime);
    JSONObject.Add('audio_type', FAudioType);
    JSONObject.Add('filename', FFilename);
    JSONObject.Add('subtitle', FSubtitle);
    JSONObject.Add('author_id', FAuthorId);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('coordinate_precision', COORDINATE_PRECISIONS[FCoordinatePrecision]);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('sighting_id', FSightingId);
    JSONObject.Add('specimen_id', FSpecimenId);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('temperature', FTemperature);
    JSONObject.Add('cloud_cover', FCloudCover);
    JSONObject.Add('precipitation', PRECIPITATION_VALUES[FPrecipitation]);
    JSONObject.Add('relative_humidity', FRelativeHumidity);
    JSONObject.Add('wind_speed', FWindSpeedBft);
    JSONObject.Add('individuals_tally', FSubjectsTally);
    JSONObject.Add('distance', FDistance);
    JSONObject.Add('context', FContext);
    JSONObject.Add('habitat', FHabitat);
    JSONObject.Add('playback_used', FPlaybackUsed);
    JSONObject.Add('recorder_model', FRecorderModel);
    JSONObject.Add('microphone_model', FMicModel);
    JSONObject.Add('filter_model', FFilterModel);
    JSONObject.Add('license_type', FLicenseType);
    JSONObject.Add('license_year', FLicenseYear);
    JSONObject.Add('license_owner', FLicenseOwner);
    JSONObject.Add('license_notes', FLicenseNotes);
    JSONObject.Add('license_url', FLicenseUri);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TAudioData.ToString: String;
begin
  Result := Format('AudioData(Id=%d, FullName=%s, RecordingDate=%s, RecordingTime=%s, AudioType=%s, Filename=%s, Subtitle=%s, ' +
    'AuthorId=%d, LocalityId=%d, CoordinatePrecision=%s, Longitude=%f, Latitude=%f, TaxonId=%d, IndividualId=%d, ' +
    'SightingId=%d, SpecimenId=%d, SurveyId=%d, Temperature=%f, CloudCover=%d, Precipitation=%s, RelativeHumidity=%f, ' +
    'WindSpeedBft=%d, SubjectsTally=%d, Distance=%f, Context=%s, Habitat=%s, PlaybackUsed=%s, RecorderModel=%s, ' +
    'MicModel=%s, FilterModel=%s, LicenseType=%s, LicenseYear=%d, LicenseOwner=%s, LicenseNotes=%s, LicenseUri=%s, ' +
    'Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, DateToStr(FRecordingDate), TimeToStr(FRecordingTime), FAudioType, FFilename, FSubtitle,
    FAuthorId, FLocalityId, COORDINATE_PRECISIONS[FCoordinatePrecision], FLongitude, FLatitude, FTaxonId, FIndividualId,
    FSightingId, FSpecimenId, FSurveyId, FTemperature, FCloudCover, PRECIPITATION_VALUES[FPrecipitation],
    FRelativeHumidity, FWindSpeedBft, FSubjectsTally, FDistance, FContext, FHabitat, BoolToStr(FPlaybackUsed, 'True', 'False'),
    FRecorderModel, FMicModel, FFilterModel, FLicenseType, FLicenseYear, FLicenseOwner, FLicenseNotes, FLicenseUri,
    FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TAudioData.Validate(out Msg: string): Boolean;
begin
  if FFilename = EmptyStr then
  begin
    Msg := 'Filename required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TAudioRepository }

procedure TAudioRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TAudioData;
begin
  if not (E is TAudioData) then
    raise Exception.Create('Delete: Expected TAudioData');

  R := TAudioData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TAudioRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_AUDIO_ID;
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

function TAudioRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_AUDIO_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TAudioRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_AUDIO_ID, COL_FULL_NAME, COL_AUDIO_FILE); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TAudioData) then
    raise Exception.Create('FindBy: Expected TAudioData');

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
        'audio_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'specimen_id, ' +
        //'survey_id, ' +
        'sighting_id, ' +
        'audio_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'coordinate_precision, ' +
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
      Hydrate(Qry, TAudioData(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TAudioRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TAudioData) then
    raise Exception.Create('GetById: Expected TAudioData');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'audio_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'specimen_id, ' +
        //'survey_id' +
        'sighting_id, ' +
        'audio_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'coordinate_precision, ' +
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TAudioData(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TAudioRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TAudioData;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TAudioData) then
    raise Exception.Create('Hydrate: Expected TAudioData');

  R := TAudioData(E);
  with aDataSet do
  begin
    R.Id := FieldByName('audio_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.RecordingDate := FieldByName('recording_date').AsDateTime;
    R.RecordingTime := FieldByName('recording_time').AsDateTime;
    R.AudioType := FieldByName('audio_type').AsString;
    R.Subtitle := FieldByName('subtitle').AsString;
    R.Filename := FieldByName('audio_file').AsString;
    R.AuthorId := FieldByName('recorder_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.SurveyId := 0;
    R.SightingId := FieldByName('sighting_id').AsInteger;
    R.SpecimenId := FieldByName('specimen_id').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    case FieldByName('coordinate_precision').AsString of
      'E': R.CoordinatePrecision := cpExact;
      'A': R.CoordinatePrecision := cpApproximated;
      'R': R.CoordinatePrecision := cpReference;
    else
      R.CoordinatePrecision := cpEmpty;
    end;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Temperature := FieldByName('temperature').AsFloat;
    R.CloudCover := FieldByName('cloud_cover').AsInteger;
    case FieldByName('precipitation').AsString of
      'N': R.Precipitation := wpNone;
      'F': R.Precipitation := wpFog;
      'M': R.Precipitation := wpMist;
      'D': R.Precipitation := wpDrizzle;
      'R': R.Precipitation := wpRain;
    else
      R.Precipitation := wpEmpty;
    end;
    R.RelativeHumidity := FieldByName('relative_humidity').AsInteger;
    R.WindSpeedBft := FieldByName('wind_speed').AsInteger;
    R.SubjectsTally := FieldByName('subjects_tally').AsInteger;
    R.Distance := FieldByName('distance').AsFloat;
    R.PlaybackUsed := FieldByName('playback_used').AsBoolean;
    R.Context := FieldByName('recording_context').AsString;
    R.Habitat := FieldByName('habitat').AsString;
    R.RecorderModel := FieldByName('recorder_model').AsString;
    R.MicModel := FieldByName('mic_model').AsString;
    R.FilterModel := FieldByName('filter_model').AsString;
    R.LicenseType := FieldByName('license_type').AsString;
    R.LicenseYear := FieldByName('license_year').AsInteger;
    R.LicenseOwner := FieldByName('license_owner').AsString;
    R.LicenseNotes := FieldByName('license_notes').AsString;
    R.LicenseUri := FieldByName('license_uri').AsString;
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

procedure TAudioRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TAudioData;
begin
  if not (E is TAudioData) then
    raise Exception.Create('Insert: Expected TAudioData');

  R := TAudioData(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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
      'coordinate_precision, ' +
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
      ':coordinate_precision, ' +
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

    SetDateParam(ParamByName('recording_date'), R.RecordingDate);
    SetTimeParam(ParamByName('recording_time'), R.RecordingTime);
    ParamByName('audio_type').AsString := R.AudioType;

    ParamByName('audio_file').AsString := R.Filename;
    SetStrParam(ParamByName('subtitle'), R.Subtitle);
    SetForeignParam(ParamByName('recorder_id'), R.AuthorId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[R.CoordinatePrecision];
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('specimen_id'), R.SpecimenId);
    //SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    ParamByName('temperature').AsFloat := R.Temperature;
    ParamByName('cloud_cover').AsInteger := R.CloudCover;
    case R.Precipitation of
      wpNone:     ParamByName('precipitation').AsString := 'N';
      wpFog:      ParamByName('precipitation').AsString := 'F';
      wpMist:     ParamByName('precipitation').AsString := 'M';
      wpDrizzle:  ParamByName('precipitation').AsString := 'D';
      wpRain:     ParamByName('precipitation').AsString := 'R';
    else
      ParamByName('precipitation').Clear;
    end;
    ParamByName('relative_humidity').AsFloat := R.RelativeHumidity;
    ParamByName('wind_speed').AsInteger := R.WindSpeedBft;
    SetIntParam(ParamByName('subjects_tally'), R.SubjectsTally);
    ParamByName('distance').AsFloat := R.Distance;
    SetStrParam(ParamByName('recording_context'), R.Context);
    SetStrParam(ParamByName('habitat'), R.Habitat);
    ParamByName('playback_used').AsBoolean := R.PlaybackUsed;
    SetStrParam(ParamByName('recorder_model'), R.RecorderModel);
    SetStrParam(ParamByName('mic_model'), R.MicModel);
    SetStrParam(ParamByName('filter_model'), R.FilterModel);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
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

function TAudioRepository.TableName: string;
begin
  Result := TBL_AUDIO_LIBRARY;
end;

procedure TAudioRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TAudioData;
begin
  if not (E is TAudioData) then
    raise Exception.Create('Update: Expected TAudioData');

  R := TAudioData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TAudioRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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

    SetDateParam(ParamByName('recording_date'), R.RecordingDate);
    SetTimeParam(ParamByName('recording_time'), R.RecordingTime);
    ParamByName('audio_type').AsString := R.AudioType;

    ParamByName('audio_file').AsString := R.Filename;
    SetStrParam(ParamByName('subtitle'), R.Subtitle);
    //SetForeignParam(ParamByName('author_id'), R.AuthorId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[R.CoordinatePrecision];
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('specimen_id'), R.SpecimenId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    ParamByName('temperature').AsFloat := R.Temperature;
    ParamByName('cloud_cover').AsInteger := R.CloudCover;
    case R.Precipitation of
      wpNone:     ParamByName('precipitation').AsString := 'N';
      wpFog:      ParamByName('precipitation').AsString := 'F';
      wpMist:     ParamByName('precipitation').AsString := 'M';
      wpDrizzle:  ParamByName('precipitation').AsString := 'D';
      wpRain:     ParamByName('precipitation').AsString := 'R';
    else
      ParamByName('precipitation').Clear;
    end;
    ParamByName('relative_humidity').AsFloat := R.RelativeHumidity;
    ParamByName('wind_speed').AsInteger := R.WindSpeedBft;
    SetIntParam(ParamByName('subjects_tally'), R.SubjectsTally);
    ParamByName('distance').AsFloat := R.Distance;
    SetStrParam(ParamByName('recording_context'), R.Context);
    SetStrParam(ParamByName('habitat'), R.Habitat);
    ParamByName('playback_used').AsBoolean := R.PlaybackUsed;
    SetStrParam(ParamByName('recorder_model'), R.RecorderModel);
    SetStrParam(ParamByName('mic_model'), R.MicModel);
    SetStrParam(ParamByName('filter_model'), R.FilterModel);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('audio_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TDocumentData }

constructor TDocumentData.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TDocumentData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TDocumentData then
  begin
    FName := TDocumentData(Source).Name;
    FDocumentDate := TDocumentData(Source).DocumentDate;
    FDocumentTime := TDocumentData(Source).DocumentTime;
    FDocumentType := TDocumentData(Source).DocumentType;
    FFilename := TDocumentData(Source).FileName;
    FAuthorId := TDocumentData(Source).AuthorId;
    FPermitId := TDocumentData(Source).PermitId;
    FProjectId := TDocumentData(Source).ProjectId;
    FPersonId := TDocumentData(Source).PersonId;
    FIndividualId := TDocumentData(Source).IndividualId;
    FCaptureId := TDocumentData(Source).CaptureId;
    FSightingId := TDocumentData(Source).SightingId;
    FExpeditionId := TDocumentData(Source).ExpeditionId;
    FSurveyId := TDocumentData(Source).SurveyId;
    FNestId := TDocumentData(Source).NestId;
    FSpecimenId := TDocumentData(Source).SpecimenId;
    FSamplingPlotId := TDocumentData(Source).SamplingPlotId;
    FMethodId := TDocumentData(Source).MethodId;
    FLicenseType := TDocumentData(Source).LicenseType;
    FLicenseYear := TDocumentData(Source).LicenseYear;
    FLicenseOwner := TDocumentData(Source).LicenseOwner;
    FLicenseNotes := TDocumentData(Source).LicenseNotes;
    FLicenseUri := TDocumentData(Source).LicenseUri;
  end;
end;

procedure TDocumentData.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FDocumentDate := NullDate;
  FDocumentTime := NullTime;
  FDocumentType := fcOther;
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

function TDocumentData.Clone: TXolmisRecord;
begin
  Result := TDocumentData(inherited Clone);
end;

function TDocumentData.Diff(const aOld: TDocumentData; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.DocumentDate, FDocumentDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.DocumentTime, FDocumentTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.DocumentType, FDocumentType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPermitID, aOld.PermitId, FPermitId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscProjectID, aOld.ProjectId, FProjectId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscExpeditionID, aOld.ExpeditionId, FExpeditionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSpecimenID, aOld.SpecimenId, FSpecimenId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSamplingPlotID, aOld.SamplingPlotId, FSamplingPlotId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscMethodID, aOld.MethodId, FMethodId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TDocumentData.EqualsTo(const Other: TDocumentData): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TDocumentData.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FDocumentDate   := Obj.Get('document_date', NullDate);
    FDocumentTime   := Obj.Get('document_time', NullTime);
    FName           := Obj.Get('name', '');
    case Obj.Get('document_type', '') of
      'url': FDocumentType := fcUrl;
      'doc': FDocumentType := fcText;
      'spr': FDocumentType := fcSpreadsheet;
      'prs': FDocumentType := fcPresentation;
      'pdf': FDocumentType := fcPdf;
      'img': FDocumentType := fcImage;
      'aud': FDocumentType := fcAudio;
      'vid': FDocumentType := fcVideo;
      'cod': FDocumentType := fcSourceCode;
      'db':  FDocumentType := fcDatabase;
      'gis': FDocumentType := fcGis;
      'scr': FDocumentType := fcScript;
      'web': FDocumentType := fcWebpage;
      'ds':  FDocumentType := fcDataset;
      'sta': FDocumentType := fcStatistic;
      'vec': FDocumentType := fcVectorial;
      'arc': FDocumentType := fcArchive;
      'bib': FDocumentType := fcBibliography;
      'met': FDocumentType := fcMetadata;
      'gen': FDocumentType := fcBioinformatic;
      'ebk': FDocumentType := fcEbook;
      'not': FDocumentType := fcNote;
    else
      FDocumentType := fcOther;
    end;
    FFilename       := Obj.Get('filename', '');
    FPermitId       := Obj.Get('permit_id', 0);
    FProjectId      := Obj.Get('project_id', 0);
    FPersonId       := Obj.Get('person_id', 0);
    FIndividualId   := Obj.Get('individual_id', 0);
    FCaptureId      := Obj.Get('capture_id', 0);
    FSightingId     := Obj.Get('sighting_id', 0);
    FSpecimenId     := Obj.Get('specimen_id', 0);
    FExpeditionId   := Obj.Get('expedition_id', 0);
    FSurveyId       := Obj.Get('survey_id', 0);
    FNestId         := Obj.Get('nest_id', 0);
    FSamplingPlotId := Obj.Get('sampling_plot_id', 0);
    FMethodId       := Obj.Get('method_id', 0);
    FLicenseType    := Obj.Get('license_type', '');
    FLicenseYear    := Obj.Get('license_year', 0);
    FLicenseOwner   := Obj.Get('license_owner', '');
    FLicenseNotes   := Obj.Get('license_notes', '');
    FLicenseUri     := Obj.Get('license_url', '');
  finally
    Obj.Free;
  end;
end;

function TDocumentData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('document_date', FDocumentDate);
    JSONObject.Add('document_time', FDocumentTime);
    JSONObject.Add('name', FName);
    JSONObject.Add('document_type', FILE_CATEGORIES[FDocumentType]);
    JSONObject.Add('filename', FFilename);
    JSONObject.Add('permit_id', FPermitId);
    JSONObject.Add('project_id', FProjectId);
    JSONObject.Add('person_id', FPersonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('capture_id', FCaptureId);
    JSONObject.Add('sighting_id', FSightingId);
    JSONObject.Add('specimen_id', FSpecimenId);
    JSONObject.Add('expedition_id', FExpeditionId);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('sampling_plot_id', FSamplingPlotId);
    JSONObject.Add('method_id', FMethodId);
    JSONObject.Add('license_type', FLicenseType);
    JSONObject.Add('license_year', FLicenseYear);
    JSONObject.Add('license_owner', FLicenseOwner);
    JSONObject.Add('license_notes', FLicenseNotes);
    JSONObject.Add('license_url', FLicenseUri);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TDocumentData.ToString: String;
begin
  Result := Format('Band(Id=%d, DocumentDate=%s, DocumentTime=%s, Name=%s, DocumentType=%s, Filename=%s, PermitId=%d, ' +
    'ProjectId=%d, PersonId=%d, IndividualId=%d, CaptureId=%d, SightingId=%d, SpecimenId=%d, ExpeditionId=%d, ' +
    'SurveyId=%d, NestId=%d, SamplingPlotId=%d, MethodId=%d, LicenseType=%s, LicenseYear=%d, LicenseOwner=%s, ' +
    'LicenseNotes=%s, LicenseUri=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, DateToStr(FDocumentDate), TimeToStr(FDocumentTime), FName, FILE_CATEGORIES[FDocumentType], FFilename, FPermitId, FProjectId,
    FPersonId, FIndividualId, FCaptureId, FSightingId, FSpecimenId, FExpeditionId, FSurveyId, FNestId,
    FSamplingPlotId, FMethodId, FLicenseType, FLicenseYear, FLicenseOwner, FLicenseNotes, FLicenseUri,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TDocumentData.Validate(out Msg: string): Boolean;
begin
  if FFilename = EmptyStr then
  begin
    Msg := 'Filename required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TDocumentRepository }

procedure TDocumentRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TDocumentData;
begin
  if not (E is TDocumentData) then
    raise Exception.Create('Delete: Expected TDocumentData');

  R := TDocumentData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TDocumentRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_DOCUMENT_ID;
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

function TDocumentRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_DOCUMENT_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TDocumentRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_DOCUMENT_ID, COL_DOCUMENT_NAME, COL_DOCUMENT_PATH); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TDocumentData) then
    raise Exception.Create('FindBy: Expected TDocumentData');

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
      Hydrate(Qry, TDocumentData(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TDocumentRepository.FindByCapture(const aFilePath: String; const aCaptureId: Integer; E: TDocumentData
  );
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (capture_id = :capture_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('capture_id').AsInteger := aCaptureId;
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

procedure TDocumentRepository.FindByExpedition(const aFilePath: String; const aExpeditionId: Integer;
  E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (expedition_id = :expedition_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('expedition_id').AsInteger := aExpeditionId;
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

procedure TDocumentRepository.FindByIndividual(const aFilePath: String; const aIndividualId: Integer;
  E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (individual_id = :individual_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('individual_id').AsInteger := aIndividualId;
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

procedure TDocumentRepository.FindByMethod(const aFilePath: String; const aMethodId: Integer; E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (method_id = :method_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('method_id').AsInteger := aMethodId;
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

procedure TDocumentRepository.FindByNest(const aFilePath: String; const aNestId: Integer; E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (nest_id = :nest_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('nest_id').AsInteger := aNestId;
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

procedure TDocumentRepository.FindByPermit(const aFilePath: String; const aPermitId: Integer; E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (permit_id = :permit_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('permit_id').AsInteger := aPermitId;
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

procedure TDocumentRepository.FindByProject(const aFilePath: String; const aProjectId: Integer; E: TDocumentData
  );
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (project_id = :project_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('project_id').AsInteger := aProjectId;
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

procedure TDocumentRepository.FindBySamplingPlot(const aFilePath: String; const aSamplingPlotId: Integer;
  E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (net_station_id = :net_station_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('net_station_id').AsInteger := aSamplingPlotId;
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

procedure TDocumentRepository.FindBySighting(const aFilePath: String; const aSightingId: Integer;
  E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (sighting_id = :sighting_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('sighting_id').AsInteger := aSightingId;
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

procedure TDocumentRepository.FindBySpecimen(const aFilePath: String; const aSpecimenId: Integer;
  E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (specimen_id = :specimen_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('specimen_id').AsInteger := aSpecimenId;
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

procedure TDocumentRepository.FindBySurvey(const aFilePath: String; const aSurveyId: Integer; E: TDocumentData);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT * FROM documents');
    Add('WHERE (document_path = :document_path)');
    Add('AND (survey_id = :survey_id)');
    ParamByName('document_path').AsString := aFilePath;
    ParamByName('survey_id').AsInteger := aSurveyId;
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

procedure TDocumentRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TDocumentData) then
    raise Exception.Create('GetById: Expected TDocumentData');

  Qry := NewQuery;
  with Qry, SQL do
  try
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
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TDocumentData(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TDocumentRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TDocumentData;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TDocumentData) then
    raise Exception.Create('Hydrate: Expected TDocumentData');

  R := TDocumentData(E);
  with aDataSet do
  begin
    R.Id := FieldByName('document_id').AsInteger;
    R.Name := FieldByName('document_name').AsString;
    R.DocumentDate := FieldByName('document_date').AsDateTime;
    R.DocumentTime := FieldByName('document_time').AsDateTime;
    case FieldByName('document_type').AsString of
      'url': R.DocumentType := fcUrl;
      'doc': R.DocumentType := fcText;
      'spr': R.DocumentType := fcSpreadsheet;
      'prs': R.DocumentType := fcPresentation;
      'pdf': R.DocumentType := fcPdf;
      'img': R.DocumentType := fcImage;
      'aud': R.DocumentType := fcAudio;
      'vid': R.DocumentType := fcVideo;
      'cod': R.DocumentType := fcSourceCode;
      'db':  R.DocumentType := fcDatabase;
      'gis': R.DocumentType := fcGis;
      'scr': R.DocumentType := fcScript;
      'web': R.DocumentType := fcWebpage;
      'ds':  R.DocumentType := fcDataset;
      'sta': R.DocumentType := fcStatistic;
      'vec': R.DocumentType := fcVectorial;
      'arc': R.DocumentType := fcArchive;
      'bib': R.DocumentType := fcBibliography;
      'met': R.DocumentType := fcMetadata;
      'gen': R.DocumentType := fcBioinformatic;
      'ebk': R.DocumentType := fcEbook;
      'not': R.DocumentType := fcNote;
    else
      R.DocumentType := fcOther;
    end;
    R.Filename := FieldByName('document_path').AsString;
    //R.AuthorId := FieldByName('author_id').AsInteger;
    R.PermitId := FieldByName('permit_id').AsInteger;
    R.ProjectId := FieldByName('project_id').AsInteger;
    R.PersonId := FieldByName('person_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.CaptureId := FieldByName('capture_id').AsInteger;
    R.SightingId := FieldByName('sighting_id').AsInteger;
    R.ExpeditionId := FieldByName('expedition_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.SpecimenId := FieldByName('specimen_id').AsInteger;
    R.SamplingPlotId := FieldByName('net_station_id').AsInteger;
    R.MethodId := FieldByName('method_id').AsInteger;
    R.LicenseType := FieldByName('license_type').AsString;
    R.LicenseYear := FieldByName('license_year').AsInteger;
    R.LicenseOwner := FieldByName('license_owner').AsString;
    R.LicenseNotes := FieldByName('license_notes').AsString;
    R.LicenseUri := FieldByName('license_uri').AsString;
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

procedure TDocumentRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TDocumentData;
begin
  if not (E is TDocumentData) then
    raise Exception.Create('Insert: Expected TDocumentData');

  R := TDocumentData(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
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

    SetDateParam(ParamByName('document_date'), R.DocumentDate);
    SetTimeParam(ParamByName('document_time'), R.DocumentTime);
    ParamByName('document_name').AsString := R.Name;
    ParamByName('document_type').AsString := FILE_CATEGORIES[R.DocumentType];
    ParamByName('document_path').AsString := R.Filename;
    SetForeignParam(ParamByName('permit_id'), R.PermitId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetForeignParam(ParamByName('person_id'), R.PersonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('specimen_id'), R.SpecimenId);
    SetForeignParam(ParamByName('expedition_id'), R.ExpeditionId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('net_station_id'), R.SamplingPlotId);
    SetForeignParam(ParamByName('method_id'), R.MethodId);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
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

function TDocumentRepository.TableName: string;
begin
  Result := TBL_DOCUMENTS;
end;

procedure TDocumentRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TDocumentData;
begin
  if not (E is TDocumentData) then
    raise Exception.Create('Update: Expected TDocumentData');

  R := TDocumentData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TDocumentRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
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
      'update_date = datetime(''now'', ''subsec'')');
    Add('WHERE (document_id = :document_id)');

    SetDateParam(ParamByName('document_date'), R.DocumentDate);
    SetTimeParam(ParamByName('document_time'), R.DocumentTime);
    ParamByName('document_name').AsString := R.Name;
    ParamByName('document_type').AsString := FILE_CATEGORIES[R.DocumentType];
    ParamByName('document_path').AsString := R.Filename;
    SetForeignParam(ParamByName('permit_id'), R.PermitId);
    SetForeignParam(ParamByName('project_id'), R.ProjectId);
    SetForeignParam(ParamByName('person_id'), R.PersonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('specimen_id'), R.SpecimenId);
    SetForeignParam(ParamByName('expedition_id'), R.ExpeditionId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('net_station_id'), R.SamplingPlotId);
    SetForeignParam(ParamByName('method_id'), R.MethodId);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('document_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TVideoData }

constructor TVideoData.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TVideoData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVideoData then
  begin
    FRecordingDate := TVideoData(Source).RecordingDate;
    FRecordingTime := TVideoData(Source).RecordingTime;
    FVideoType := TVideoData(Source).VideoType;
    FFilename := TVideoData(Source).Filename;
    FSubtitle := TVideoData(Source).Subtitle;
    FAuthorId := TVideoData(Source).AuthorId;
    FLongitude := TVideoData(Source).Longitude;
    FLatitude := TVideoData(Source).Latitude;
    FLocalityId := TVideoData(Source).LocalityId;
    FTaxonId := TVideoData(Source).TaxonId;
    FIndividualId := TVideoData(Source).IndividualId;
    FCaptureId := TVideoData(Source).CaptureId;
    FSurveyId := TVideoData(Source).SurveyId;
    FSightingId := TVideoData(Source).SightingId;
    FNestId := TVideoData(Source).NestId;
    FNestRevisionId := TVideoData(Source).NestRevisionId;
    FDistance := TVideoData(Source).Distance;
    FContext := TVideoData(Source).Context;
    FHabitat := TVideoData(Source).Habitat;
    FCameraModel := TVideoData(Source).CameraModel;
    FLicenseType := TVideoData(Source).LicenseType;
    FLicenseYear := TVideoData(Source).LicenseYear;
    FLicenseOwner := TVideoData(Source).LicenseOwner;
    FLicenseNotes := TVideoData(Source).LicenseNotes;
    FLicenseUri := TVideoData(Source).LicenseUri;
    FFullName := TVideoData(Source).FullName;
    FNotes := TVideoData(Source).Notes;
  end;
end;

procedure TVideoData.Clear;
begin
  inherited Clear;
  FRecordingDate := NullDate;
  FRecordingTime := NullTime;
  FVideoType := EmptyStr;
  FFilename := EmptyStr;
  FSubtitle := EmptyStr;
  FAuthorId := 0;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FLocalityId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FCaptureId := 0;
  FSurveyId := 0;
  FSightingId := 0;
  FNestId := 0;
  FNestRevisionId := 0;
  FDistance := 0.0;
  FContext := EmptyStr;
  FHabitat := EmptyStr;
  FCameraModel := EmptyStr;
  FLicenseType := EmptyStr;
  FLicenseYear := 0;
  FLicenseOwner := EmptyStr;
  FLicenseNotes := EmptyStr;
  FLicenseUri := EmptyStr;
  FFullName := EmptyStr;
  FNotes := EmptyStr;
end;

function TVideoData.Clone: TXolmisRecord;
begin
  Result := TVideoData(inherited Clone);
end;

function TVideoData.Diff(const aOld: TVideoData; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if FieldValuesDiff(rscDate, aOld.RecordingDate, FRecordingDate, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTime, aOld.RecordingTime, FRecordingTime, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscType, aOld.VideoType, FVideoType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFilename, aOld.FileName, FFilename, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSubtitle, aOld.Subtitle, FSubtitle, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscAuthorID, aOld.AuthorId, FAuthorId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCaptureID, aOld.CaptureId, FCaptureId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNestRevisionID, aOld.NestRevisionId, FNestRevisionId, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscDistanceM, aOld.Distance, FDistance, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscContext, aOld.Context, FContext, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscHabitat, aOld.Habitat, FHabitat, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscCameraModel, aOld.CameraModel, FCameraModel, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseType, aOld.LicenseType, FLicenseType, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseYear, aOld.LicenseYear, FLicenseYear, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseOwner, aOld.LicenseOwner, FLicenseOwner, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseNotes, aOld.LicenseNotes, FLicenseNotes, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscLicenseUri, aOld.LicenseUri, FLicenseUri, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    Changes.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TVideoData.EqualsTo(const Other: TVideoData): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TVideoData.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName       := Obj.Get('full_name', '');
    FRecordingDate  := Obj.Get('recording_date', NullDate);
    FRecordingTime  := Obj.Get('recording_time', NullTime);
    FVideoType      := Obj.Get('video_type', '');
    FFilename       := Obj.Get('filename', '');
    FSubtitle       := Obj.Get('subtitle', '');
    FAuthorId       := Obj.Get('author_id', 0);
    FLocalityId     := Obj.Get('locality_id', 0);
    FLongitude      := Obj.Get('longitude', 0.0);
    FLatitude       := Obj.Get('latitude', 0.0);
    FTaxonId        := Obj.Get('taxon_id', 0);
    FIndividualId   := Obj.Get('individual_id', 0);
    FCaptureId      := Obj.Get('capture_id', 0);
    FSightingId     := Obj.Get('sighting_id', 0);
    FNestId         := Obj.Get('nest_id', 0);
    FNestRevisionId := Obj.Get('nest_revision_id', 0);
    FSurveyId       := Obj.Get('survey_id', 0);
    FDistance       := Obj.Get('distance', 0.0);
    FContext        := Obj.Get('context', '');
    FHabitat        := Obj.Get('habitat', '');
    FCameraModel    := Obj.Get('camera_model', '');
    FLicenseType    := Obj.Get('license_type', '');
    FLicenseYear    := Obj.Get('license_year', 0);
    FLicenseOwner   := Obj.Get('license_owner', '');
    FLicenseNotes   := Obj.Get('license_notes', '');
    FLicenseUri     := Obj.Get('license_url', '');
    FNotes          := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TVideoData.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('recording_date', FRecordingDate);
    JSONObject.Add('recording_time', FRecordingTime);
    JSONObject.Add('video_type', FVideoType);
    JSONObject.Add('filename', FFilename);
    JSONObject.Add('subtitle', FSubtitle);
    JSONObject.Add('author_id', FAuthorId);
    JSONObject.Add('locality_id', FLocalityId);
    JSONObject.Add('longitude', FLongitude);
    JSONObject.Add('latitude', FLatitude);
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('individual_id', FIndividualId);
    JSONObject.Add('capture_id', FCaptureId);
    JSONObject.Add('sighting_id', FSightingId);
    JSONObject.Add('nest_id', FNestId);
    JSONObject.Add('nest_revision_id', FNestRevisionId);
    JSONObject.Add('survey_id', FSurveyId);
    JSONObject.Add('distance', FDistance);
    JSONObject.Add('context', FContext);
    JSONObject.Add('habitat', FHabitat);
    JSONObject.Add('camera_model', FCameraModel);
    JSONObject.Add('license_type', FLicenseType);
    JSONObject.Add('license_year', FLicenseYear);
    JSONObject.Add('license_owner', FLicenseOwner);
    JSONObject.Add('license_notes', FLicenseNotes);
    JSONObject.Add('license_url', FLicenseUri);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TVideoData.ToString: String;
begin
  Result := Format('VideoData(Id=%d, FullName=%s, RecordingDate=%s, RecordingTime=%s, VideoType=%s, Filename=%s, Subtitle=%s, ' +
    'AuthorId=%d, LocalityId=%d, Longitude=%f, Latitude=%f, TaxonId=%d, IndividualId=%d, CaptureId=%d, ' +
    'SightingId=%d, NestId=%d, NestRevisionId=%d, SurveyId=%d, ' +
    'Distance=%f, Context=%s, Habitat=%s, CameraModel=%s, ' +
    'LicenseType=%s, LicenseYear=%d, LicenseOwner=%s, LicenseNotes=%s, LicenseUri=%s, ' +
    'Notes=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, DateToStr(FRecordingDate), TimeToStr(FRecordingTime), FVideoType, FFilename, FSubtitle,
    FAuthorId, FLocalityId, FLongitude, FLatitude, FTaxonId, FIndividualId, FCaptureId,
    FSightingId, FNestId, FNestRevisionId, FSurveyId,
    FDistance, FContext, FHabitat,
    FCameraModel, FLicenseType, FLicenseYear, FLicenseOwner, FLicenseNotes, FLicenseUri,
    FNotes,
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TVideoData.Validate(out Msg: string): Boolean;
begin
  if FFilename = EmptyStr then
  begin
    Msg := 'Filename required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TVideoRepository }

procedure TVideoRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVideoData;
begin
  if not (E is TVideoData) then
    raise Exception.Create('Delete: Expected TVideoData');

  R := TVideoData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVideoRepository.Delete: %s.', [rsErrorEmptyId]);

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
      MacroByName('idname').Value := COL_VIDEO_ID;
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

function TVideoRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := COL_VIDEO_ID;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVideoRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = (COL_VIDEO_ID, COL_FULL_NAME, COL_FILE_PATH); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TVideoData) then
    raise Exception.Create('FindBy: Expected TVideoData');

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
        'video_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'nest_id, ' +
        'nest_revision_id, ' +
        'survey_id, ' +
        'sighting_id, ' +
        'video_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'longitude, ' +
        'latitude, ' +
        'recording_context, ' +
        'habitat, ' +
        'camera_model, ' +
        'distance, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'file_path, ' +
        'subtitle, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM videos');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TVideoData(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TVideoRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TVideoData) then
    raise Exception.Create('GetById: Expected TVideoData');

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('SELECT ' +
        'video_id, ' +
        'full_name, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'capture_id, ' +
        'nest_id, ' +
        'nest_revision_id, ' +
        'survey_id, ' +
        'sighting_id, ' +
        'video_type, ' +
        'locality_id, ' +
        'recording_date, ' +
        'recorder_id, ' +
        'recording_time, ' +
        'longitude, ' +
        'latitude, ' +
        'recording_context, ' +
        'habitat, ' +
        'camera_model, ' +
        'distance, ' +
        'license_type, ' +
        'license_year, ' +
        'license_uri, ' +
        'license_notes, ' +
        'license_owner, ' +
        'file_path, ' +
        'subtitle, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM videos');
    Add('WHERE video_id = :cod');
    ParamByName('COD').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TVideoData(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVideoRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TVideoData;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TVideoData) then
    raise Exception.Create('Hydrate: Expected TVideoData');

  R := TVideoData(E);
  with aDataSet do
  begin
    R.Id := FieldByName('video_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.RecordingDate := FieldByName('recording_date').AsDateTime;
    R.RecordingTime := FieldByName('recording_time').AsDateTime;
    R.VideoType := FieldByName('video_type').AsString;
    R.Subtitle := FieldByName('subtitle').AsString;
    R.Filename := FieldByName('file_path').AsString;
    R.AuthorId := FieldByName('recorder_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.IndividualId := FieldByName('individual_id').AsInteger;
    R.CaptureId := FieldByName('capture_id').AsInteger;
    R.SurveyId := FieldByName('survey_id').AsInteger;
    R.SightingId := FieldByName('sighting_id').AsInteger;
    R.NestId := FieldByName('nest_id').AsInteger;
    R.NestRevisionId := FieldByName('nest_revision_id').AsInteger;
    R.LocalityId := FieldByName('locality_id').AsInteger;
    //case FieldByName('coordinate_precision').AsString of
    //  'E': R.CoordinatePrecision := cpExact;
    //  'A': R.CoordinatePrecision := cpApproximated;
    //  'R': R.CoordinatePrecision := cpReference;
    //else
    //  R.CoordinatePrecision := cpEmpty;
    //end;
    R.Longitude := FieldByName('longitude').AsFloat;
    R.Latitude := FieldByName('latitude').AsFloat;
    R.Distance := FieldByName('distance').AsFloat;
    R.Context := FieldByName('recording_context').AsString;
    R.Habitat := FieldByName('habitat').AsString;
    R.CameraModel := FieldByName('camera_model').AsString;
    R.LicenseType := FieldByName('license_type').AsString;
    R.LicenseYear := FieldByName('license_year').AsInteger;
    R.LicenseOwner := FieldByName('license_owner').AsString;
    R.LicenseNotes := FieldByName('license_notes').AsString;
    R.LicenseUri := FieldByName('license_uri').AsString;
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

procedure TVideoRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVideoData;
begin
  if not (E is TVideoData) then
    raise Exception.Create('Insert: Expected TVideoData');

  R := TVideoData(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO videos (' +
      'full_name, ' +
      'taxon_id, ' +
      'individual_id, ' +
      'capture_id, ' +
      'nest_id, ' +
      'nest_revision_id, ' +
      'survey_id, ' +
      'sighting_id, ' +
      'video_type, ' +
      'locality_id, ' +
      'recording_date, ' +
      'recorder_id, ' +
      'recording_time, ' +
      'longitude, ' +
      'latitude, ' +
      'recording_context, ' +
      'habitat, ' +
      'camera_model, ' +
      'distance, ' +
      'license_type, ' +
      'license_year, ' +
      'license_uri, ' +
      'license_notes, ' +
      'license_owner, ' +
      'file_path, ' +
      'subtitle, ' +
      'notes, ' +
      'user_inserted, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':full_name, ' +
      ':taxon_id, ' +
      ':individual_id, ' +
      ':capture_id, ' +
      ':nest_id, ' +
      ':nest_revision_id, ' +
      ':survey_id, ' +
      ':sighting_id, ' +
      ':video_type, ' +
      ':locality_id, ' +
      'date(:recording_date), ' +
      ':recorder_id, ' +
      'time(:recording_time), ' +
      ':longitude, ' +
      ':latitude, ' +
      ':recording_context, ' +
      ':habitat, ' +
      ':camera_model, ' +
      ':distance, ' +
      ':license_type, ' +
      ':license_year, ' +
      ':license_uri, ' +
      ':license_notes, ' +
      ':license_owner, ' +
      ':file_path, ' +
      ':subtitle, ' +
      ':notes, ' +
      ':user_inserted, ' +
      'datetime(''now'', ''subsec''))');

    SetDateParam(ParamByName('recording_date'), R.RecordingDate);
    SetTimeParam(ParamByName('recording_time'), R.RecordingTime);
    ParamByName('video_type').AsString := R.VideoType;

    ParamByName('file_path').AsString := R.Filename;
    SetStrParam(ParamByName('subtitle'), R.Subtitle);
    SetForeignParam(ParamByName('recorder_id'), R.AuthorId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    //ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[R.CoordinatePrecision];
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('nest_revision_id'), R.NestRevisionId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    ParamByName('distance').AsFloat := R.Distance;
    SetStrParam(ParamByName('recording_context'), R.Context);
    SetStrParam(ParamByName('habitat'), R.Habitat);
    SetStrParam(ParamByName('camera_model'), R.CameraModel);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
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

function TVideoRepository.TableName: string;
begin
  Result := TBL_VIDEOS;
end;

procedure TVideoRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVideoData;
begin
  if not (E is TVideoData) then
    raise Exception.Create('Update: Expected TVideoData');

  R := TVideoData(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVideoRepository.Update: %s.', [rsErrorEmptyId]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE videos SET ' +
      'full_name = :full_name, ' +
      'taxon_id = :taxon_id, ' +
      'individual_id = :individual_id, ' +
      'capture_id = :capture_id, ' +
      'nest_id = :nest_id, ' +
      'nest_revision_id = :nest_revision_id, ' +
      'survey_id = :survey_id, ' +
      'sighting_id = :sighting_id, ' +
      'video_type = :video_type, ' +
      'locality_id = :locality_id, ' +
      'recording_date = date(:recording_date), ' +
      'recorder_id = :recorder_id, ' +
      'recording_time = time(:recording_time), ' +
      'longitude = :longitude, ' +
      'latitude = :latitude, ' +
      'recording_context = :recording_context, ' +
      'habitat = :habitat, ' +
      'camera_model = :camera_model, ' +
      'distance = :distance, ' +
      'license_type = :license_type, ' +
      'license_year = :license_year, ' +
      'license_uri = :license_uri, ' +
      'license_notes = :license_notes, ' +
      'license_owner = :license_owner, ' +
      'file_path = :file_path, ' +
      'subtitle = :subtitle, ' +
      'notes = :notes, ' +
      'user_updated = :user_updated, ' +
      'update_date = datetime(''now'', ''subsec'') ');
    Add('WHERE (video_id = :video_id)');

    SetDateParam(ParamByName('recording_date'), R.RecordingDate);
    SetTimeParam(ParamByName('recording_time'), R.RecordingTime);
    ParamByName('video_type').AsString := R.VideoType;

    ParamByName('file_path').AsString := R.Filename;
    SetStrParam(ParamByName('subtitle'), R.Subtitle);
    SetForeignParam(ParamByName('recorder_id'), R.AuthorId);
    SetForeignParam(ParamByName('locality_id'), R.LocalityId);
    //ParamByName('coordinate_precision').AsString := COORDINATE_PRECISIONS[R.CoordinatePrecision];
    SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), R.Longitude, R.Latitude);
    SetForeignParam(ParamByName('taxon_id'), R.TaxonId);
    SetForeignParam(ParamByName('individual_id'), R.IndividualId);
    SetForeignParam(ParamByName('capture_id'), R.CaptureId);
    SetForeignParam(ParamByName('sighting_id'), R.SightingId);
    SetForeignParam(ParamByName('nest_id'), R.NestId);
    SetForeignParam(ParamByName('nest_revision_id'), R.NestRevisionId);
    SetForeignParam(ParamByName('survey_id'), R.SurveyId);
    ParamByName('distance').AsFloat := R.Distance;
    SetStrParam(ParamByName('recording_context'), R.Context);
    SetStrParam(ParamByName('habitat'), R.Habitat);
    SetStrParam(ParamByName('camera_model'), R.CameraModel);
    SetStrParam(ParamByName('notes'), R.Notes);
    SetStrParam(ParamByName('license_type'), R.LicenseType);
    SetIntParam(ParamByName('license_year'), R.LicenseYear);
    SetStrParam(ParamByName('license_owner'), R.LicenseOwner);
    SetStrParam(ParamByName('license_notes'), R.LicenseNotes);
    SetStrParam(ParamByName('license_uri'), R.LicenseUri);
    ParamByName('user_updated').AsInteger := ActiveUser.Id;
    ParamByName('video_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

