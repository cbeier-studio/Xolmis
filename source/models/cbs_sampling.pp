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

unit cbs_sampling;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, DateUtils, cbs_record_types;

type
  TAuthor = record
    Id: Integer;
    Citation: String;
  end;

  TAuthors = array of TAuthor;

type

  { TMethod }

  TMethod = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FEbirdName: String;
    FDescription: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TMethod; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TMethod);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property EbirdName: String read FEbirdName write FEbirdName;
    property Description: String read FDescription write FDescription;
  end;

type

  { TExpedition }

  TExpedition = class(TXolmisRecord)
  protected
    FName: String;
    FStartDate: TDate;
    FEndDate: TDate;
    FLocalityId: Integer;
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
    property LocalityId: Integer read FLocalityId write FLocalityId;
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
    function Find(aLocal: Integer; aDate: String; aNetStation: Integer = 0): Boolean;
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
  TWeatherSampleMoment = (wmNone, wmStart, wmMiddle, wmEnd);
  TPrecipitation = (wpEmpty = -1, wpNone, wpFog, wpMist, wpDrizzle, wpRain);

const
  WeatherSampleMoments: array [TWeatherSampleMoment] of String = ('', 'S', 'M', 'E');
  PrecipitationValues: array [TPrecipitation] of String = ('', 'N', 'F', 'M', 'D', 'R');

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
    function Find(const FieldName: String; const Value: Variant): Boolean;
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
    FNetMesh: String;
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
    property NetMesh: String read FNetMesh write FNetMesh;
    property Notes: String read FNotes write FNotes;
  end;

type
  TStratumDistribution = (
    disNone,
    disRare,
    disFewSparseIndividuals,
    disOnePatch,
    disOnePatchFewSparseIndividuals,
    disManySparseIndividuals,
    disOnePatchManySparseIndividuals,
    disFewPatches,
    disFewPatchesSparseIndividuals,
    disManyPatches,
    disManyPatchesSparseIndividuals,
    disHighDensityIndividuals,
    disContinuousCoverWithGaps,
    disContinuousDenseCover,
    disContinuousDenseCoverWithEdge
  );

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


type

  { TSamplingPlot }

  TSamplingPlot = class(TXolmisRecord)
  protected
    FFullName: String;
    FAcronym: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAreaShape: String;
    FLocalityId: Integer;
    FDescription: String;
    FNotes: String;
  public
    constructor Create (aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Find(aAcronym: String): Boolean;
    function Diff(aOld: TSamplingPlot; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSamplingPlot);
    function ToJSON: String;
  published
    property FullName: String read FFullName write FFullName;
    property Acronym: String read FAcronym write FAcronym;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property AreaShape: String read FAreaShape write FAreaShape;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TPermanentNet }

  TPermanentNet = class(TXolmisRecord)
  protected
    FFullName: String;
    FNetStationId: Integer;
    FNetNumber: Integer;
    FLongitude: Extended;
    FLatitude: Extended;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPermanentNet; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPermanentNet);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property FullName: String read FFullName write FFullName;
    property NetStationId: Integer read FNetStationId write FNetStationId;
    property NetNumber: Integer read FNetNumber write FNetNumber;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TSpecimen }

  TSpecimen = class(TXolmisRecord)
  protected
    FFieldNumber: String;
    FSampleType: String;
    FFullName: String;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FNestId: Integer;
    FEggId: Integer;
    //FCollectionDate: String;
    FCollectionDay: Integer;
    FCollectionMonth: Integer;
    FCollectionYear: Integer;
    FLocalityId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Insert;
    function Find(aFieldNumber: String; aYear, aMonth, aDay: Integer; aTaxon, aLocality: Integer): Boolean;
    function Diff(aOld: TSpecimen; var aList: TStrings): Boolean;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSpecimen);
    function ToJSON: String;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property SampleType: String read FSampleType write FSampleType;
    property FullName: String read FFullName write FFullName;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property NestId: Integer read FNestId write FNestId;
    property EggId: Integer read FEggId write FEggId;
    //property CollectionDate: String read FCollectionDate write FCollectionDate;
    property CollectionDay: Integer read FCollectionDay write FCollectionDay;
    property CollectionMonth: Integer read FCollectionMonth write FCollectionMonth;
    property CollectionYear: Integer read FCollectionYear write FCollectionYear;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property Notes: String read FNotes write FNotes;
  end;

  { TSpecimenCollector }

  TSpecimenCollector = class(TXolmisRecord)
  protected
    FSpecimenId: Integer;
    FPersonId: Integer;
    FCollectorSeq: Integer;
  public
    constructor Create(aValue: Integer = 0);
    function Diff(aOld: TSpecimenCollector; var aList: TStrings): Boolean;
    function Find(const FieldName: String; const Value: Variant): Boolean;
    function ToJSON: String;
    procedure Clear; override;
    procedure Copy(aFrom: TSpecimenCollector);
    procedure Delete;
    procedure GetData(aKey: Integer);
    procedure Insert;
    procedure LoadFromDataSet(aDataSet: TDataSet);
    procedure Save;
    procedure Update;
  published
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property PersonId: Integer read FPersonId write FPersonId;
    property CollectorSeq: Integer read FCollectorSeq write FCollectorSeq;
  end;

type

  { TSamplePrep }

  TSamplePrep = class(TXolmisRecord)
  protected
    FSpecimenId: Integer;
    FAccessionNum: String;
    FFullName: String;
    FAccessionType: String;
    FAccessionSeq: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FNestId: Integer;
    FEggId: Integer;
    FPreparationDate: TDate;
    FPreparerId: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TSamplePrep; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSamplePrep);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
    property AccessionNum: String read FAccessionNum write FAccessionNum;
    property FullName: String read FFullName write FFullName;
    property AccessionType: String read FAccessionType write FAccessionType;
    property AccessionSeq: Integer read FAccessionSeq write FAccessionSeq;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property NestId: Integer read FNestId write FNestId;
    property EggId: Integer read FEggId write FEggId;
    property PreparationDate: TDate read FPreparationDate write FPreparationDate;
    property PreparerId: Integer read FPreparerId write FPreparerId;
    property Notes: String read FNotes write FNotes;
  end;

  function AuthorListToString(aAuthors: TAuthors): String;
  procedure StringToAuthorList(const aCitation: String; var aAuthors: TAuthors);

implementation

uses
  cbs_locale, cbs_global, cbs_users, cbs_validations, cbs_getvalue, cbs_fullnames, cbs_datacolumns,
  cbs_setparam, udm_main;

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

{ TSamplePrep }

constructor TSamplePrep.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSamplePrep.Clear;
begin
  inherited Clear;
  FSpecimenId := 0;
  FFullName := EmptyStr;
  FAccessionNum := EmptyStr;
  FAccessionType := EmptyStr;
  FAccessionSeq := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FNestId := 0;
  FEggId := 0;
  FPreparationDate := StrToDate('30/12/1500');
  FPreparerId := 0;
  FNotes := EmptyStr;
end;

procedure TSamplePrep.Copy(aFrom: TSamplePrep);
begin
  FSpecimenId := aFrom.SpecimenId;
  FFullName := aFrom.FullName;
  FAccessionNum := aFrom.AccessionNum;
  FAccessionType := aFrom.AccessionType;
  FAccessionSeq := aFrom.AccessionSeq;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FNestId := aFrom.NestId;
  FEggId := aFrom.EggId;
  FPreparationDate := aFrom.PreparationDate;
  FPreparerId := aFrom.PreparerId;
  FNotes := aFrom.Notes;
end;

procedure TSamplePrep.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplePrep.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM sample_preps');
      Add('WHERE (sample_prep_id = :aid)');

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

procedure TSamplePrep.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'sample_prep_id, ' +
        'specimen_id, ' +
        'accession_num, ' +
        'full_name, ' +
        'accession_type, ' +
        'accession_seq, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'preparation_date, ' +
        'preparer_id, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM sample_preps');
    Add('WHERE sample_prep_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplePrep.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('sample_prep_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FAccessionNum := FieldByName('accession_num').AsString;
    FAccessionType := FieldByName('accession_type').AsString;
    FAccessionSeq := FieldByName('accession_seq').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    FEggId := FieldByName('egg_id').AsInteger;
    FPreparationDate := FieldByName('preparation_date').AsDateTime;
    FPreparerId := FieldByName('preparer_id').AsInteger;
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

procedure TSamplePrep.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO sample_preps (' +
        'specimen_id, ' +
        'accession_num, ' +
        'full_name, ' +
        'accession_type, ' +
        'accession_seq, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'preparation_date, ' +
        'preparer_id, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':specimen_id, ' +
        ':accession_num, ' +
        ':full_name, ' +
        ':accession_type, ' +
        ':accession_seq, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':nest_id, ' +
        ':egg_id, ' +
        'date(:preparation_date), ' +
        ':preparer_id, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      ParamByName('accession_num').AsString := FAccessionNum;
      ParamByName('full_name').AsString := FFullName;
      ParamByName('accession_type').AsString := FAccessionType;
      ParamByName('accession_seq').AsInteger := FAccessionSeq;
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('individual_id').AsInteger := FIndividualId;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('egg_id').AsInteger := FEggId;
      ParamByName('preparation_date').AsString := FormatDateTime('yyyy-mm-dd', FPreparationDate);
      ParamByName('preparer_id').AsInteger := FPreparerId;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //// Get the site hierarchy
      //if (FSpecimenId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT country_id, state_id, municipality_id FROM specimens');
      //  Add('WHERE specimen_id = :aspecimen');
      //  ParamByName('aspecimen').AsInteger := FSpecimenId;
      //  Open;
      //  FCountryId := FieldByName('country_id').AsInteger;
      //  FStateId := FieldByName('state_id').AsInteger;
      //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
      //  Close;
      //end;
      //// Save the site hierarchy
      //Clear;
      //Add('UPDATE sample_preps SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE sample_prep_id = :aid');
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
      //
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
      //Add('UPDATE sample_preps SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  subfamily_id = :subfamily_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE sample_prep_id = :aid');
      //if (FOrderId > 0) then
      //  ParamByName('order_id').AsInteger := FOrderId
      //else
      //  ParamByName('order_id').Clear;
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

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplePrep.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSamplePrep.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Accession number', FAccessionNum);
    JSONObject.Add('Accession type', FAccessionType);
    JSONObject.Add('Sequence', FAccessionSeq);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Egg', FEggId);
    JSONObject.Add('Preparation date', FPreparationDate);
    JSONObject.Add('Preparer', FPreparerId);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSamplePrep.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplePrep.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE sample_preps SET ' +
        'specimen_id = :specimen_id, ' +
        'accession_num = :accession_num, ' +
        'full_name = :full_name, ' +
        'accession_type = :accession_type, ' +
        'accession_seq = :accession_seq, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'nest_id = :nest_id, ' +
        'egg_id = :egg_id, ' +
        'preparation_date = date(:preparation_date), ' +
        'preparer_id = :preparer_id, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec'') ');
      Add('WHERE (sample_prep_id = :sample_prep_id)');
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      ParamByName('accession_num').AsString := FAccessionNum;
      ParamByName('full_name').AsString := FFullName;
      ParamByName('accession_type').AsString := FAccessionType;
      ParamByName('accession_seq').AsInteger := FAccessionSeq;
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('individual_id').AsInteger := FIndividualId;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('egg_id').AsInteger := FEggId;
      ParamByName('preparation_date').AsString := FormatDateTime('yyyy-mm-dd', FPreparationDate);
      ParamByName('preparer_id').AsInteger := FPreparerId;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;
      ParamByName('sample_prep_id').AsInteger := FId;

      ExecSQL;

      //// Get the site hierarchy
      //if (FSpecimenId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT country_id, state_id, municipality_id FROM specimens');
      //  Add('WHERE specimen_id = :aspecimen');
      //  ParamByName('aspecimen').AsInteger := FSpecimenId;
      //  Open;
      //  FCountryId := FieldByName('country_id').AsInteger;
      //  FStateId := FieldByName('state_id').AsInteger;
      //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
      //  Close;
      //end;
      //// Save the site hierarchy
      //Clear;
      //Add('UPDATE sample_preps SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE sample_prep_id = :aid');
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
      //
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
      //Add('UPDATE sample_preps SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  subfamily_id = :subfamily_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE sample_prep_id = :aid');
      //if (FOrderId > 0) then
      //  ParamByName('order_id').AsInteger := FOrderId
      //else
      //  ParamByName('order_id').Clear;
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

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSamplePrep.Diff(aOld: TSamplePrep; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscAccessionNr, aOld.AccessionNum, FAccessionNum, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.AccessionType, FAccessionType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDuplicateNr, aOld.AccessionSeq, FAccessionSeq, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPreparationDate, aOld.PreparationDate, FPreparationDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscPreparerID, aOld.PreparerId, FPreparerId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSamplePrep.Find(const FieldName: String; const Value: Variant): Boolean;
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
        'sample_prep_id, ' +
        'specimen_id, ' +
        'accession_num, ' +
        'full_name, ' +
        'accession_type, ' +
        'accession_seq, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'preparation_date, ' +
        'preparer_id, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM sample_preps');
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

{ TSpecimen }

constructor TSpecimen.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSpecimen.Clear;
begin
  inherited Clear;
  FFieldNumber := EmptyStr;
  FSampleType := EmptyStr;
  FFullName := EmptyStr;
  FTaxonId := 0;
  FIndividualId := 0;
  FNestId := 0;
  FEggId := 0;
  //FCollectionDate := '00.00.0000';
  FCollectionDay := 0;
  FCollectionMonth := 0;
  FCollectionYear := 0;
  FLocalityId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FNotes := EmptyStr;
end;

procedure TSpecimen.Copy(aFrom: TSpecimen);
begin
  FFieldNumber := aFrom.FieldNumber;
  FSampleType := aFrom.SampleType;
  FFullName := aFrom.FullName;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FNestId := aFrom.NestId;
  FEggId := aFrom.EggId;
  //FCollectionDate := aFrom.CollectionDate;
  FCollectionDay := aFrom.CollectionDay;
  FCollectionMonth := aFrom.CollectionMonth;
  FCollectionYear := aFrom.CollectionYear;
  FLocalityId := aFrom.LocalityId;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FNotes := aFrom.Notes;
end;

procedure TSpecimen.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimen.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM specimens');
      Add('WHERE (specimen_id = :aid)');

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

procedure TSpecimen.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'specimen_id, ' +
        'field_number, ' +
        'full_name, ' +
        'sample_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        'collection_date, ' +
        'collection_day, ' +
        'collection_month, ' +
        'collection_year, ' +
        'locality_id, ' +
        'longitude, ' +
        'latitude, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimens');
    Add('WHERE specimen_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimen.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('specimen_id').AsInteger;
    FFieldNumber := FieldByName('field_number').AsString;
    FSampleType := FieldByName('sample_type').AsString;
    FFullName := FieldByName('full_name').AsString;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FNestId := FieldByName('nest_id').AsInteger;
    FEggId := FieldByName('egg_id').AsInteger;
    //FCollectionDate := FieldByName('collection_date').AsString;
    FCollectionDay := FieldByName('collection_day').AsInteger;
    FCollectionMonth := FieldByName('collection_month').AsInteger;
    FCollectionYear := FieldByName('collection_year').AsInteger;
    FLocalityId := FieldByName('locality_id').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
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

procedure TSpecimen.Insert;
var
  Qry: TSQLQuery;
  //aDate: TPartialDate;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO specimens (' +
        'field_number, ' +
        'full_name, ' +
        'sample_type, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'nest_id, ' +
        'egg_id, ' +
        //'collection_date, ' +
        'collection_day, ' +
        'collection_month, ' +
        'collection_year, ' +
        'locality_id, ' +
        'longitude, ' +
        'latitude, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':field_number, ' +
        ':full_name, ' +
        ':sample_type, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':nest_id, ' +
        ':egg_id, ' +
        //':collection_date, ' +
        ':collection_day, ' +
        ':collection_month, ' +
        ':collection_year, ' +
        ':locality_id, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('sample_type').AsString := FSampleType;
      ParamByName('collection_year').AsInteger := FCollectionYear;
      ParamByName('collection_month').AsInteger := FCollectionMonth;
      ParamByName('collection_day').AsInteger := FCollectionDay;
      //aDate.Year := FCollectionYear;
      //aDate.Month := FCollectionMonth;
      //aDate.Day := FCollectionDay;
      //ParamByName('collective_date').AsString := aDate.ToString;
      ParamByName('individual_id').AsInteger := FIndividualId;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('egg_id').AsInteger := FEggId;
      ParamByName('taxon_id').AsInteger := FTaxonId;
      if FLongitude <> 0 then
        ParamByName('longitude').AsFloat := FLongitude
      else
        ParamByName('longitude').Clear;
      if FLatitude <> 0 then
        ParamByName('latitude').AsFloat := FLatitude
      else
        ParamByName('latitude').Clear;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('notes').AsString := FNotes;
      ParamByName('full_name').AsString := FFullName;

      //GetTaxonHierarchyForSpecimen(Self);
      //ParamByName('order_id').AsInteger := FOrderId;
      //ParamByName('family_id').AsInteger := FFamilyId;
      //ParamByName('genus_id').AsInteger := FGenusId;
      //ParamByName('species_id').AsInteger := FSpeciesId;
      //
      //GetSiteHierarchyForSpecimen(Self);
      //ParamByName('country_id').AsInteger := FCountryId;
      //ParamByName('state_id').AsInteger := FStateId;
      //ParamByName('municipality_id').AsInteger := FMunicipalityId;

      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      //// Get the site hierarchy
      //if (FLocalityId > 0) then
      //begin
      //  Clear;
      //  Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
      //  Add('WHERE site_id = :asite');
      //  ParamByName('asite').AsInteger := FLocalityId;
      //  Open;
      //  FCountryId := FieldByName('country_id').AsInteger;
      //  FStateId := FieldByName('state_id').AsInteger;
      //  FMunicipalityId := FieldByName('municipality_id').AsInteger;
      //  Close;
      //end;
      //// Save the site hierarchy
      //Clear;
      //Add('UPDATE specimens SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE specimen_id = :aid');
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
      //
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
      //Add('UPDATE specimens SET');
      //Add('  order_id = :order_id,');
      //Add('  family_id = :family_id,');
      //Add('  subfamily_id = :subfamily_id,');
      //Add('  genus_id = :genus_id,');
      //Add('  species_id = :species_id');
      //Add('WHERE specimen_id = :aid');
      //if (FOrderId > 0) then
      //  ParamByName('order_id').AsInteger := FOrderId
      //else
      //  ParamByName('order_id').Clear;
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

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimen.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSpecimen.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Field number', FFieldNumber);
    JSONObject.Add('Sample type', FSampleType);
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Nest', FNestId);
    JSONObject.Add('Egg', FEggId);
    //JSONObject.Add('Collection date', FCollectionDate);
    JSONObject.Add('Collection day', FCollectionDay);
    JSONObject.Add('Collection month', FCollectionMonth);
    JSONObject.Add('Collection year', FCollectionYear);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSpecimen.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimen.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE specimens SET ' +
        'field_number = :field_number, ' +
        'full_name = :full_name, ' +
        'sample_type = :sample_type, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'nest_id = :nest_id, ' +
        'egg_id = :egg_id, ' +
        //'collection_date, ' +
        'collection_day = :collection_day, ' +
        'collection_month = :collection_month, ' +
        'collection_year = :collection_year, ' +
        'locality_id = :locality_id, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (specimen_id = :specimen_id)');

      ParamByName('field_number').AsString := FFieldNumber;
      ParamByName('sample_type').AsString := FSampleType;
      ParamByName('collection_year').AsInteger := FCollectionYear;
      ParamByName('collection_month').AsInteger := FCollectionMonth;
      ParamByName('collection_day').AsInteger := FCollectionDay;
      //aDate.Year := FCollectionYear;
      //aDate.Month := FCollectionMonth;
      //aDate.Day := FCollectionDay;
      //ParamByName('collective_date').AsString := aDate.ToString;
      ParamByName('individual_id').AsInteger := FIndividualId;
      ParamByName('nest_id').AsInteger := FNestId;
      ParamByName('egg_id').AsInteger := FEggId;
      ParamByName('taxon_id').AsInteger := FTaxonId;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('notes').AsString := FNotes;
      ParamByName('full_name').AsString := FFullName;

      //GetTaxonHierarchyForSpecimen(Self);
      //ParamByName('order_id').AsInteger := FOrderId;
      //ParamByName('family_id').AsInteger := FFamilyId;
      //ParamByName('genus_id').AsInteger := FGenusId;
      //ParamByName('species_id').AsInteger := FSpeciesId;
      //
      //GetSiteHierarchyForSpecimen(Self);
      //ParamByName('country_id').AsInteger := FCountryId;
      //ParamByName('state_id').AsInteger := FStateId;
      //ParamByName('municipality_id').AsInteger := FMunicipalityId;

      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('specimen_id').AsInteger := FId;

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

function TSpecimen.Diff(aOld: TSpecimen; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscFieldNumber, aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.SampleType, FSampleType, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNestID, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEggID, aOld.EggId, FEggId, R) then
    aList.Add(R);
  //if FieldValuesDiff(rscCollectionDate, aOld.CollectionDate, FCollectionDate, R) then
    //aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectionDay, aOld.CollectionDay, FCollectionDay, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectionMonth, aOld.CollectionMonth, FCollectionMonth, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCollectionYear, aOld.CollectionYear, FCollectionYear, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSpecimen.Find(aFieldNumber: String; aYear, aMonth, aDay: Integer; aTaxon, aLocality: Integer): Boolean;
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
    Add('SELECT specimen_id FROM specimens');
    Add('WHERE (field_number = :afieldnumber)');
    Add('AND (collection_year = :ayear)');
    Add('AND (collection_month = :amonth)');
    Add('AND (collection_day = :aday)');
    Add('AND (taxon_id = :ataxon)');
    Add('AND (locality_id = :alocality)');
    ParamByName('AFIELDNUMBER').AsString := aFieldNumber;
    ParamByName('ALOCALITY').AsInteger := aLocality;
    ParamByName('AYEAR').AsInteger := aYear;
    ParamByName('AMONTH').AsInteger := aMonth;
    ParamByName('ADAY').AsInteger := aDay;
    ParamByName('ALATITUDE').AsInteger := aTaxon;

    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('specimen_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSpecimenCollector }

constructor TSpecimenCollector.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSpecimenCollector.Clear;
begin
  inherited Clear;
  FSpecimenId := 0;
  FPersonId := 0;
  FCollectorSeq := 0;
end;

procedure TSpecimenCollector.Copy(aFrom: TSpecimenCollector);
begin
  FSpecimenId := aFrom.SpecimenId;
  FPersonId := aFrom.PersonId;
  FCollectorSeq := aFrom.CollectorSeq;
end;

procedure TSpecimenCollector.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimenCollector.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM specimen_collectors');
      Add('WHERE (collector_id = :aid)');

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

function TSpecimenCollector.Diff(aOld: TSpecimenCollector; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscPersonID, aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSequence, aOld.CollectorSeq, FCollectorSeq, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSpecimenCollector.Find(const FieldName: String; const Value: Variant): Boolean;
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
        'collector_id, ' +
        'specimen_id, ' +
        'person_id, ' +
        'collector_seq, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimen_collectors');
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

procedure TSpecimenCollector.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'collector_id, ' +
        'specimen_id, ' +
        'person_id, ' +
        'collector_seq, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM specimen_collectors');
    Add('WHERE collector_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSpecimenCollector.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('collector_id').AsInteger;
    FSpecimenId := FieldByName('specimen_id').AsInteger;
    FPersonId := FieldByName('person_id').AsInteger;
    FCollectorSeq := FieldByName('collector_seq').AsInteger;
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

procedure TSpecimenCollector.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO specimen_collectors (' +
        'specimen_id, ' +
        'person_id, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':specimen_id, ' +
        ':person_id, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('user_inserted').AsInteger := FUserInserted;
  //    GravaLogSQL(SQL);
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

procedure TSpecimenCollector.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSpecimenCollector.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Specimen', FSpecimenId);
    JSONObject.Add('Collector', FPersonId);
    JSONObject.Add('Sequence', FCollectorSeq);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSpecimenCollector.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSpecimenCollector.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE specimen_collectors SET ' +
        'specimen_id = :specimen_id, ' +
        'person_id = :person_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (collector_id = :collector_id)');
      ParamByName('specimen_id').AsInteger := FSpecimenId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('user_updated').AsInteger := FUserInserted;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('collector_id').AsInteger := FId;

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
  FLocalityId := 0;
  FProjectId := 0;
  FDescription := EmptyStr;
end;

procedure TExpedition.Copy(aFrom: TExpedition);
begin
  FName := aFrom.Name;
  FStartDate := aFrom.StartDate;
  FEndDate := aFrom.EndDate;
  FLocalityId := aFrom.LocalityId;
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
        'locality_id, ' +
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
    FLocalityId := FieldByName('locality_id').AsInteger;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO expeditions (' +
        'expedition_name, ' +
        'start_date, ' +
        'end_date, ' +
        'project_id, ' +
        'locality_id, ' +
        'description, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':expedition_name, ' +
        'date(:start_date), ' +
        'date(:end_date), ' +
        ':project_id, ' +
        ':locality_id, ' +
        ':description, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('expedition_name').AsString := FName;
      if not DateIsNull(FStartDate) then
        ParamByName('start_date').AsString := FormatDateTime('yyyy-mm-dd', FStartDate)
      else
        ParamByName('start_date').Clear;
      if not DateIsNull(FEndDate) then
        ParamByName('end_date').AsString := FormatDateTime('yyyy-mm-dd', FEndDate)
      else
        ParamByName('end_date').Clear;
      if FProjectId > 0 then
        ParamByName('project_id').AsInteger := FProjectId
      else
        ParamByName('project_id').Clear;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('description').AsString := FDescription;
      ParamByName('user_inserted').AsInteger := FUserInserted;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

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
      //Add('UPDATE expeditions SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE expedition_id = :aid');
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
    JSONObject.Add('Locality', FLocalityId);
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE expeditions SET ' +
        'expedition_name = :expedition_name, ' +
        'start_date = date(:start_date), ' +
        'end_date = date(:end_date), ' +
        'project_id = :project_id, ' +
        'locality_id = :locality_id, ' +
        'description = :description, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (expedition_id = :expedition_id)');
      ParamByName('expedition_name').AsString := FName;
      if not DateIsNull(FStartDate) then
        ParamByName('start_date').AsString := FormatDateTime('yyyy-mm-dd', FStartDate)
      else
        ParamByName('start_date').Clear;
      if not DateIsNull(FEndDate) then
        ParamByName('end_date').AsString := FormatDateTime('yyyy-mm-dd', FEndDate)
      else
        ParamByName('end_date').Clear;
      if FProjectId > 0 then
        ParamByName('project_id').AsInteger := FProjectId
      else
        ParamByName('project_id').Clear;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('description').AsString := FDescription;
      ParamByName('user_updated').AsInteger := FUserInserted;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('expedition_id').AsInteger := FId;

      ExecSQL;

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
      //Add('UPDATE expeditions SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE expedition_id = :aid');
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
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
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
        'locality_id, ' +
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
  FNetMesh := EmptyStr;
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
    FNetMesh := FieldByName('net_mesh').AsString;
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
      SetStrParam(ParamByName('net_mesh'), FNetMesh);
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
      ParamByName('sample_date').AsString := FormatDateTime('yyyy-mm-dd', FSampleDate);
      ParamByName('net_number').AsInteger := FNetNumber;
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('net_station_id').AsInteger := FNetStationId;
      if (FPermanentNetId > 0) then
        ParamByName('permanent_net_id').AsInteger := FPermanentNetId
      else
        ParamByName('permanent_net_id').Clear;
      ParamByName('full_name').AsString := GetNetEffortFullname(FSampleDate, FNetStationId, FNetNumber);
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      if (FNetLength > 0) then
        ParamByName('net_length').AsFloat := FNetLength
      else
        ParamByName('net_length').Clear;
      if (FNetHeight > 0) then
        ParamByName('net_height').AsFloat := FNetHeight
      else
        ParamByName('net_height').Clear;
      if (FNetMesh <> EmptyStr) then
        ParamByName('net_mesh').AsString := FNetMesh
      else
        ParamByName('net_mesh').Clear;
      ParamByName('notes').AsString := FNotes;

      // if the field has 1 second, it is NULL
      if FNetOpen1 <> StrToTime('00:00:01') then
        ParamByName('net_open_1').AsString := TimeToStr(FNetOpen1)
      else
        ParamByName('net_open_1').Clear;
      if FNetClose1 <> StrToTime('00:00:01') then
        ParamByName('net_close_1').AsString := TimeToStr(FNetClose1)
      else
        ParamByName('net_close_1').Clear;

      if FNetOpen2 <> StrToTime('00:00:01') then
        ParamByName('net_open_2').AsString := TimeToStr(FNetOpen2)
      else
        ParamByName('net_open_2').Clear;
      if FNetClose2 <> StrToTime('00:00:01') then
        ParamByName('net_close_2').AsString := TimeToStr(FNetClose2)
      else
        ParamByName('net_close_2').Clear;

      if FNetOpen3 <> StrToTime('00:00:01') then
        ParamByName('net_open_3').AsString := TimeToStr(FNetOpen3)
      else
        ParamByName('net_open_3').Clear;
      if FNetClose3 <> StrToTime('00:00:01') then
        ParamByName('net_close_3').AsString := TimeToStr(FNetClose3)
      else
        ParamByName('net_close_3').Clear;

      if FNetOpen4 <> StrToTime('00:00:01') then
        ParamByName('net_open_4').AsString := TimeToStr(FNetOpen4)
      else
        ParamByName('net_open_4').Clear;
      if FNetClose4 <> StrToTime('00:00:01') then
        ParamByName('net_close_4').AsString := TimeToStr(FNetClose4)
      else
        ParamByName('net_close_4').Clear;

      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('net_id').AsInteger := FId;

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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
      ParamByName('sample_date').AsString := FormatDateTime('yyyy-mm-dd', FSampleDate);
      ParamByName('sample_time').AsString := FormatDateTime('hh:nn', FSampleTime);
      ParamByName('survey_id').AsInteger := FSurveyId;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      ParamByName('observer_id').AsInteger := FObserverId;
      ParamByName('notes').AsString := FNotes;

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

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
      ParamByName('sample_date').AsString := FormatDateTime('yyyy-mm-dd', FSampleDate);
      ParamByName('sample_time').AsString := FormatDateTime('hh:nn', FSampleTime);
      ParamByName('survey_id').AsInteger := FSurveyId;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      ParamByName('observer_id').AsInteger := FObserverId;
      ParamByName('notes').AsString := FNotes;

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

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
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

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('person_id').AsInteger := FPersonId;
      ParamByName('visitor').AsBoolean := FVisitor;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;

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

function TSurveyMember.Find(const FieldName: String; const Value: Variant): Boolean;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
      ParamByName('survey_date').AsString := FormatDateTime('yyyy-mm-dd', FSurveyDate);
      ParamByName('start_time').AsString := TimeToStr(FStartTime);
      if not TimeIsNull(FEndTime) then
        ParamByName('end_time').AsString := TimeToStr(FEndTime)
      else
        ParamByName('end_time').Clear;
      if FDuration > 0 then
        ParamByName('duration').AsInteger := FDuration
      else
        ParamByName('duration').Clear;
      ParamByName('method_id').AsInteger := FMethodId;
      if FNetStationId > 0 then
        ParamByName('net_station_id').AsInteger := FNetStationId
      else
        ParamByName('net_station_id').Clear;
      if FExpeditionId > 0 then
        ParamByName('expedition_id').AsInteger := FExpeditionId
      else
        ParamByName('expedition_id').Clear;
      if FProjectId > 0 then
        ParamByName('project_id').AsInteger := FProjectId
      else
        ParamByName('project_id').Clear;
      ParamByName('locality_id').AsInteger := FLocalityId;
      if (FStartLongitude <> 0) and (FStartLatitude <> 0) then
      begin
        ParamByName('start_longitude').AsFloat := FStartLongitude;
        ParamByName('start_latitude').AsFloat := FStartLatitude;
      end
      else
      begin
        ParamByName('start_longitude').Clear;
        ParamByName('start_latitude').Clear;
      end;
      if (FEndLongitude <> 0) and (FEndLatitude <> 0) then
      begin
        ParamByName('end_longitude').AsFloat := FEndLongitude;
        ParamByName('end_latitude').AsFloat := FEndLatitude;
      end
      else
      begin
        ParamByName('end_longitude').Clear;
        ParamByName('end_latitude').Clear;
      end;
      if FSampleId <> EmptyStr then
        ParamByName('sample_id').AsString := FSampleId
      else
        ParamByName('sample_id').Clear;
      if FObserversTally > 0 then
        ParamByName('observers_tally').AsInteger := FObserversTally
      else
        ParamByName('observers_tally').Clear;
      if FTotalNets > 0 then
        ParamByName('nets_total').AsInteger := FTotalNets
      else
        ParamByName('nets_total').Clear;
      if FTotalArea > 0 then
        ParamByName('area_total').AsFloat := FTotalArea
      else
        ParamByName('area_total').Clear;
      if FTotalDistance > 0 then
        ParamByName('distance_total').AsFloat := FTotalDistance
      else
        ParamByName('distance_total').Clear;
      if FHabitat <> EmptyStr then
        ParamByName('habitat').AsString := FHabitat
      else
        ParamByName('habitat').Clear;
      if FNetRounds <> EmptyStr then
        ParamByName('net_rounds').AsString := FNetRounds
      else
        ParamByName('net_rounds').Clear;
      if FNotes <> EmptyStr then
        ParamByName('notes').AsString := FNotes
      else
        ParamByName('notes').Clear;
      ParamByName('full_name').AsString := GetSurveyFullname(FSurveyDate, FLocalityId, FMethodId, 0, '');
      ParamByName('user_inserted').AsInteger := FUserInserted;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

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
      //Add('UPDATE surveys SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE survey_id = :aid');
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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

      ParamByName('survey_date').AsString := FormatDateTime('yyyy-mm-dd', FSurveyDate);
      ParamByName('start_time').AsString := TimeToStr(FStartTime);
      if not TimeIsNull(FEndTime) then
        ParamByName('end_time').AsString := TimeToStr(FEndTime)
      else
        ParamByName('end_time').Clear;
      if FDuration > 0 then
        ParamByName('duration').AsInteger := FDuration
      else
        ParamByName('duration').Clear;
      ParamByName('method_id').AsInteger := FMethodId;
      if FNetStationId > 0 then
        ParamByName('net_station_id').AsInteger := FNetStationId
      else
        ParamByName('net_station_id').Clear;
      if FExpeditionId > 0 then
        ParamByName('expedition_id').AsInteger := FExpeditionId
      else
        ParamByName('expedition_id').Clear;
      if FProjectId > 0 then
        ParamByName('project_id').AsInteger := FProjectId
      else
        ParamByName('project_id').Clear;
      ParamByName('locality_id').AsInteger := FLocalityId;
      if (FStartLongitude <> 0) and (FStartLatitude <> 0) then
      begin
        ParamByName('start_longitude').AsFloat := FStartLongitude;
        ParamByName('start_latitude').AsFloat := FStartLatitude;
      end
      else
      begin
        ParamByName('start_longitude').Clear;
        ParamByName('start_latitude').Clear;
      end;
      if (FEndLongitude <> 0) and (FEndLatitude <> 0) then
      begin
        ParamByName('end_longitude').AsFloat := FEndLongitude;
        ParamByName('end_latitude').AsFloat := FEndLatitude;
      end
      else
      begin
        ParamByName('end_longitude').Clear;
        ParamByName('end_latitude').Clear;
      end;
      if FSampleId <> EmptyStr then
        ParamByName('sample_id').AsString := FSampleId
      else
        ParamByName('sample_id').Clear;
      if FObserversTally > 0 then
        ParamByName('observers_tally').AsInteger := FObserversTally
      else
        ParamByName('observers_tally').Clear;
      if FTotalNets > 0 then
        ParamByName('nets_total').AsInteger := FTotalNets
      else
        ParamByName('nets_total').Clear;
      if FTotalArea > 0 then
        ParamByName('area_total').AsFloat := FTotalArea
      else
        ParamByName('area_total').Clear;
      if FTotalDistance > 0 then
        ParamByName('distance_total').AsFloat := FTotalDistance
      else
        ParamByName('distance_total').Clear;
      if FHabitat <> EmptyStr then
        ParamByName('habitat').AsString := FHabitat
      else
        ParamByName('habitat').Clear;
      if FNetRounds <> EmptyStr then
        ParamByName('net_rounds').AsString := FNetRounds
      else
        ParamByName('net_rounds').Clear;
      if FNotes <> EmptyStr then
        ParamByName('notes').AsString := FNotes
      else
        ParamByName('notes').Clear;
      ParamByName('full_name').AsString := GetSurveyFullname(FSurveyDate, FLocalityId, FMethodId, 0, '');
      ParamByName('user_updated').AsInteger := FUserInserted;
      ParamByName('exported_status').AsBoolean := FExported;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('survey_id').AsInteger := FId;

      ExecSQL;

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
      //Add('UPDATE surveys SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE survey_id = :aid');
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

function TSurvey.Find(aLocal: Integer; aDate: String; aNetStation: Integer): Boolean;
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
    if aNetStation > 0 then
      Add('AND (net_station_id = :astation)');
    Add('AND (date(survey_date) = date(:adate))');
    ParamByName('ALOCAL').AsInteger := aLocal;
    if aNetStation > 0 then
      ParamByName('ASTATION').AsInteger := aNetStation;
    ParamByName('ADATE').AsString := aDate;
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
      ParamByName('sample_date').AsString := FormatDateTime('yyyy-mm-dd', FSampleDate);
      ParamByName('sample_time').AsString := TimeToStr(FSampleTime);
      ParamByName('sample_moment').AsString := WeatherSampleMoments[FSampleMoment];
      ParamByName('observer_id').AsInteger := FObserverId;
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('cloud_cover').AsInteger := FCloudCover;
      ParamByName('precipitation').AsString := PrecipitationValues[FPrecipitation];
      ParamByName('rainfall').AsInteger := FRainfall;
      ParamByName('temperature').AsFloat := FTemperature;
      ParamByName('wind_speed_bft').AsInteger := FWindSpeedBft;
      ParamByName('wind_speed_kmh').AsFloat := FWindSpeedKmH;
      ParamByName('relative_humidity').AsFloat := FRelativeHumidity;
      ParamByName('atmospheric_pressure').AsFloat := FAtmosphericPressure;
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
    JSONObject.Add('Moment', WeatherSampleMoments[FSampleMoment]);
    JSONObject.Add('Time', FSampleTime);
    JSONObject.Add('Survey', FSurveyId);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Atmospheric Pressure', FAtmosphericPressure);
    JSONObject.Add('Cloud Cover', FCloudCover);
    JSONObject.Add('Precipitation', PrecipitationValues[FPrecipitation]);
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

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
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
        'relative_humidity = :relative_humidity, ' +
        'atmospheric_pressure = :atmospheric_pressure, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (weather_id = :weather_id)');
      ParamByName('sample_date').AsString := FormatDateTime('yyyy-mm-dd', FSampleDate);
      ParamByName('sample_time').AsString := TimeToStr(FSampleTime);
      ParamByName('sample_moment').AsString := WeatherSampleMoments[FSampleMoment];
      ParamByName('observer_id').AsInteger := FObserverId;
      ParamByName('survey_id').AsInteger := FSurveyId;
      ParamByName('cloud_cover').AsInteger := FCloudCover;
      ParamByName('precipitation').AsString := PrecipitationValues[FPrecipitation];
      ParamByName('rainfall').AsInteger := FRainfall;
      ParamByName('temperature').AsFloat := FTemperature;
      ParamByName('wind_speed_bft').AsInteger := FWindSpeedBft;
      ParamByName('wind_speed_kmh').AsFloat := FWindSpeedKmH;
      ParamByName('relative_humidity').AsFloat := FRelativeHumidity;
      ParamByName('atmospheric_pressure').AsFloat := FAtmosphericPressure;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('weather_id').AsInteger := FId;

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

{ TSamplingPlot }

constructor TSamplingPlot.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSamplingPlot.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FAcronym := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FAreaShape := EmptyStr;
  FLocalityId := 0;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TSamplingPlot.Copy(aFrom: TSamplingPlot);
begin
  FFullName := aFrom.FullName;
  FAcronym := aFrom.Acronym;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FAreaShape := aFrom.AreaShape;
  FLocalityId := aFrom.LocalityId;
  FDescription := aFrom.Description;
  FNotes := aFrom.Notes;
end;

procedure TSamplingPlot.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplingPlot.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM sampling_plots');
      Add('WHERE (sampling_plot_id = :aid)');

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

procedure TSamplingPlot.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'sampling_plot_id, ' +
      'full_name, ' +
      'acronym, ' +
      'longitude, ' +
      'latitude, ' +
      'area_shape, ' +
      'locality_id, ' +
      'description, ' +
      'notes, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM sampling_plots');
    Add('WHERE sampling_plot_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSamplingPlot.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('sampling_plot_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FAcronym := FieldByName('acronym').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAreaShape := FieldByName('area_shape').AsString;
    FLocalityId := FieldByName('locality_id').AsInteger;
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

procedure TSamplingPlot.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO sampling_plots (' +
        'full_name, ' +
        'acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'area_shape, ' +
        //'country_id, ' +
        //'state_id, ' +
        //'municipality_id, ' +
        'locality_id, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':full_name, ' +
        ':acronym, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':area_shape, ' +
        //':country_id, ' +
        //':state_id, ' +
        //':municipality_id, ' +
        ':locality_id, ' +
        ':description, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      ParamByName('area_shape').AsString := FAreaShape;
      //ParamByName('country_id').AsInteger := FCountryId;
      //ParamByName('state_id').AsInteger := FStateId;
      //ParamByName('municipality_id').AsString := FMunicipalityId;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('description').AsString := FDescription;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the autoincrement key inserted
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

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
      //Add('UPDATE sampling_plots SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE sampling_plot_id = :aid');
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

procedure TSamplingPlot.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSamplingPlot.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Abbreviation', FAcronym);
    JSONObject.Add('Area shape', FAreaShape);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Locality', FLocalityId);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSamplingPlot.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSamplingPlot.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE sampling_plots SET ' +
        'full_name = :full_name, ' +
        'acronym = :acronym, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'area_shape = :area_shape, ' +
        //'country_id, ' +
        //'state_id, ' +
        //'municipality_id, ' +
        'locality_id = :locality_id, ' +
        'description = :description, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (sampling_plot_id = :sampling_plot_id)');
      ParamByName('full_name').AsString := FFullName;
      ParamByName('acronym').AsString := FAcronym;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      ParamByName('area_shape').AsString := FAreaShape;
      //ParamByName('country_id').AsInteger := FCountryId;
      //ParamByName('state_id').AsInteger := FStateId;
      //ParamByName('municipality_id').AsString := FMunicipalityId;
      ParamByName('locality_id').AsInteger := FLocalityId;
      ParamByName('description').AsString := FDescription;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('sampling_plot_id').AsInteger := FId;

      ExecSQL;

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
      //Add('UPDATE sampling_plots SET');
      //Add('  country_id = :country_id,');
      //Add('  state_id = :state_id,');
      //Add('  municipality_id = :municipality_id');
      //Add('WHERE sampling_plot_id = :aid');
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

function TSamplingPlot.Find(aAcronym: String): Boolean;
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
    Add('SELECT sampling_plot_id FROM sampling_plots');
    Add('WHERE (acronym = :aacronym)');
    ParamByName('AACRONYM').AsString := aAcronym;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('sampling_plot_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSamplingPlot.Diff(aOld: TSamplingPlot; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAreaShape, aOld.AreaShape, FAreaShape, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLocalityID, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  //if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, FMunicipalityId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionState, aOld.StateId, FStateId, R) then
  //  aList.Add(R);
  //if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, FCountryId, R) then
  //  aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TPermanentNet }

constructor TPermanentNet.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TPermanentNet.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FNetStationId := 0;
  FNetNumber := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FNotes := EmptyStr;
end;

procedure TPermanentNet.Copy(aFrom: TPermanentNet);
begin
  FFullName := aFrom.FullName;
  FNetStationId := aFrom.NetStationId;
  FNetNumber := aFrom.NetNumber;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FNotes := aFrom.Notes;
end;

procedure TPermanentNet.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermanentNet.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM permanent_nets');
      Add('WHERE (permanent_net_id = :aid)');

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

procedure TPermanentNet.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
      'permanent_net_id, ' +
      'sampling_plot_id, ' +
      'net_number, ' +
      'longitude, ' +
      'latitude, ' +
      'notes, ' +
      'full_name, ' +
      'user_inserted, ' +
      'user_updated, ' +
      'insert_date, ' +
      'update_date, ' +
      'exported_status, ' +
      'marked_status, ' +
      'active_status ' +
      'FROM permanent_nets');
    Add('WHERE permanent_net_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPermanentNet.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('permanent_net_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FNetStationId := FieldByName('net_station_id').AsInteger;
    FNetNumber := FieldByName('net_number').AsInteger;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
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
    FUserInserted := FieldByName('user_inserted').AsInteger;
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

procedure TPermanentNet.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('INSERT INTO permanent_nets (' +
        'sampling_plot_id, ' +
        'net_number, ' +
        'longitude, ' +
        'latitude, ' +
        'notes, ' +
        'full_name, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':sampling_plot_id, ' +
        ':net_number, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':notes, ' +
        ':full_name, ' +
        ':user_inserted, ' +
        'datetime(''now'',''subsec''))');
      ParamByName('sampling_plot_id').AsInteger := FNetStationId;
      ParamByName('full_name').AsString := FFullName;
      ParamByName('net_number').AsInteger := FNetNumber;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
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

procedure TPermanentNet.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPermanentNet.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FFullName);
    JSONObject.Add('Net station', FNetStationId);
    JSONObject.Add('Net number', FNetNumber);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPermanentNet.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPermanentNet.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE permanent_nets SET ' +
        'sampling_plot_id = :sampling_plot_id, ' +
        'net_number = :net_number, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'notes = :notes, ' +
        'full_name = :full_name, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (permanent_net_id = :permanent_net_id)');
      ParamByName('sampling_plot_id').AsInteger := FNetStationId;
      ParamByName('full_name').AsString := FFullName;
      ParamByName('net_number').AsInteger := FNetNumber;
      if (FLongitude <> 0) and (FLatitude <> 0) then
      begin
        ParamByName('longitude').AsFloat := FLongitude;
        ParamByName('latitude').AsFloat := FLatitude;
      end
      else
      begin
        ParamByName('longitude').Clear;
        ParamByName('latitude').Clear;
      end;
      ParamByName('notes').AsString := FNotes;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('permanent_net_id').AsInteger := FId;

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

function TPermanentNet.Diff(aOld: TPermanentNet; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscMistnetNr, aOld.NetNumber, FNetNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPermanentNet.Find(const FieldName: String; const Value: Variant): Boolean;
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

    Add('SELECT * FROM permanent_nets');
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

{ TMethod }

constructor TMethod.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TMethod.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FEbirdName := EmptyStr;
  FDescription := EmptyStr;
end;

procedure TMethod.Copy(aFrom: TMethod);
begin
  FName := aFrom.Name;
  FAbbreviation := aFrom.Abbreviation;
  FEbirdName := aFrom.EbirdName;
  FDescription := aFrom.Description;
end;

procedure TMethod.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TMethod.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM methods');
      Add('WHERE (method_id = :aid)');

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

procedure TMethod.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'method_id, ' +
        'method_name, ' +
        'method_acronym, ' +
        'ebird_name, ' +
        'description, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM methods');
    Add('WHERE method_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethod.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('method_id').AsInteger;
    FName := FieldByName('method_name').AsString;
    FAbbreviation := FieldByName('method_acronym').AsString;
    FEbirdName := FieldByName('ebird_name').AsString;
    FDescription := FieldByName('description').AsString;
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

procedure TMethod.Insert;
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
      Add('INSERT INTO methods (' +
        'method_name, ' +
        'method_acronym, ' +
        'ebird_name, ' +
        'description, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':method_name, ' +
        ':method_acronym, ' +
        ':ebird_name, ' +
        ':description, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('method_name').AsString := FName;
      ParamByName('method_acronym').AsString := FAbbreviation;
      ParamByName('ebird_name').AsString := FEbirdName;
      ParamByName('description').AsString := FDescription;
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

procedure TMethod.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TMethod.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FName);
    JSONObject.Add('Abbreviation', FAbbreviation);
    JSONObject.Add('EbirdName', FEbirdName);
    JSONObject.Add('Description', FDescription);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TMethod.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TMethod.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('UPDATE methods SET ' +
        'method_name = :method_name, ' +
        'method_acronym = :method_acronym, ' +
        'ebird_name = :ebird_name, ' +
        'description = :description, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (method_id = :method_id)');

      ParamByName('method_name').AsString := FName;
      ParamByName('method_acronym').AsString := FAbbreviation;
      ParamByName('ebird_name').AsString := FEbirdName;
      ParamByName('description').AsString := FDescription;
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('method_id').AsInteger := FId;

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

function TMethod.Diff(aOld: TMethod; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Abbreviation, FAbbreviation, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TMethod.Find(const FieldName: String; const Value: Variant): Boolean;
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

    Add('SELECT * FROM methods');
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

end.

