unit cbs_sampling;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, cbs_record_types;

type
  TAuthor = record
    Id: Integer;
    Citation: String;
  end;

  TAuthors = array of TAuthor;

const
  PrecipitationValues: array [0 .. 4] of String = ('N', 'F', 'M', 'D', 'R');

type

  { TMethod }

  TMethod = class(TXolmisRecord)
  protected
    FName: String;
    FAcronym: String;
    FDescription: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TMethod; var aList: TStrings): Boolean;
  published
    property Name: String read FName write FName;
    property Acronym: String read FAcronym write FAcronym;
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
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FDescription: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TExpedition; var aList: TStrings): Boolean;
  published
    property Name: String read FName write FName;
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property ProjectId: Integer read FProjectId write FProjectId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
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
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FSampleId: String;
    FStartLatitude: Extended;
    FStartLongitude: Extended;
    FEndLatitude: Extended;
    FEndLongitude: Extended;
    FTotalArea: Double;
    FTotalDistance: Double;
    FTotalNets: Integer;
    FCloudCoverOpen: Integer;
    FCloudCoverMid: Integer;
    FCloudCoverClose: Integer;
    FPrecipitationOpen: String;
    FPrecipitationMid: String;
    FPrecipitationClose: String;
    FTemperatureOpen: Double;
    FTemperatureMid: Double;
    FTemperatureClose: Double;
    FWindOpen: Integer;
    FWindMid: Integer;
    FWindClose: Integer;
    FHumidityOpen: Double;
    FHumidityMid: Double;
    FHumidityClose: Double;
    FHabitat: String;
    FNetRounds: String;
    FFullName: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure Insert;
    function Diff(aOld: TSurvey; var aList: TStrings): Boolean;
    function Find(aLocal: Integer; aDate: String; aNetStation: Integer = 0): Boolean;
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
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property SampleId: String read FSampleId write FSampleId;
    property StartLatitude: Extended read FStartLatitude write FStartLatitude;
    property StartLongitude: Extended read FStartLongitude write FStartLongitude;
    property EndLatitude: Extended read FEndLatitude write FEndLatitude;
    property EndLongitude: Extended read FEndLongitude write FEndLongitude;
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
    FPrecipitation: String;
    FRainfall: Integer;
    FRelativeHumidity: Double;
    FSampleDate: TDate;
    FSampleMoment: String;
    FSampleTime: TTime;
    FTemperature: Double;
    FWindSpeedBft: Integer;
    FWindSpeedKmH: Double;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TWeatherLog; var aList: TStrings): Boolean;
  published
    property SurveyId: Integer read FSurveyId write FSurveyId;
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property SampleTime: TTime read FSampleTime write FSampleTime;
    property SampleMoment: String read FSampleMoment write FSampleMoment;
    property CloudCover: Integer read FCloudCover write FCloudCover;
    property Precipitation: String read FPrecipitation write FPrecipitation;
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
    function Diff(aOld: TSurveyMember; var aList: TStrings): Boolean;
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
    FTotalOpenTime: Double;
    FNetLength: Double;
    FNetHeight: Double;
    FNetArea: Double;
    FNetMesh: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Find(aSurvey: Integer; aNetNumber: String): Boolean;
    function Diff(aOld: TNetEffort; var aList: TStrings): Boolean;
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
    property TotalOpenTime: Double read FTotalOpenTime write FTotalOpenTime;
    property NetLength: Double read FNetLength write FNetLength;
    property NetHeight: Double read FNetHeight write FNetHeight;
    property NetArea: Double read FNetArea write FNetArea;
    property NetMesh: String read FNetMesh write FNetMesh;
    property Notes: String read FNotes write FNotes;
  end;

type

  { TNetStation }

  TNetStation = class(TXolmisRecord)
  protected
    FName: String;
    FAcronym: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FLocalityId: Integer;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
  public
    constructor Create (aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Find(aAcronym: String): Boolean;
    function Diff(aOld: TNetStation; var aList: TStrings): Boolean;
  published
    property Name: String read FName write FName;
    property Acronym: String read FAcronym write FAcronym;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
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
    function Diff(aOld: TPermanentNet; var aList: TStrings): Boolean;
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
    FOrderId: Integer;
    FFamilyId: Integer;
    FSubfamilyId: Integer;
    FGenusId: Integer;
    FSpeciesId: Integer;
    FIndividualId: Integer;
    FNestId: Integer;
    FEggId: Integer;
    FCollectionDate: String;
    FCollectionDay: Integer;
    FCollectionMonth: Integer;
    FCollectionYear: Integer;
    FLocalityId: Integer;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FLatitude: Extended;
    FLongitude: Extended;
    FCollectors: String;
    FCollector1: Integer;
    FCollector2: Integer;
    FCollector3: Integer;
    FCollector4: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TSpecimen; var aList: TStrings): Boolean;
  published
    property FieldNumber: String read FFieldNumber write FFieldNumber;
    property SampleType: String read FSampleType write FSampleType;
    property FullName: String read FFullName write FFullName;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property OrderId: Integer read FOrderId write FOrderId;
    property FamilyId: Integer read FFamilyId write FFamilyId;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property GenusId: Integer read FGenusId write FGenusId;
    property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property NestId: Integer read FNestId write FNestId;
    property EggId: Integer read FEggId write FEggId;
    property CollectionDate: String read FCollectionDate write FCollectionDate;
    property CollectionDay: Integer read FCollectionDay write FCollectionDay;
    property CollectionMonth: Integer read FCollectionMonth write FCollectionMonth;
    property CollectionYear: Integer read FCollectionYear write FCollectionYear;
    property LocalityId: Integer read FLocalityId write FLocalityId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property Collectors: String read FCollectors write FCollectors;
    property Collector1: Integer read FCollector1 write FCollector1;
    property Collector2: Integer read FCollector2 write FCollector2;
    property Collector3: Integer read FCollector3 write FCollector3;
    property Collector4: Integer read FCollector4 write FCollector4;
    property Notes: String read FNotes write FNotes;
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
    FOrderId: Integer;
    FFamilyId: Integer;
    FSubfamilyId: Integer;
    FGenusId: Integer;
    FSpeciesId: Integer;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    function Diff(aOld: TSamplePrep; var aList: TStrings): Boolean;
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
    property OrderId: Integer read FOrderId write FOrderId;
    property FamilyId: Integer read FFamilyId write FFamilyId;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property GenusId: Integer read FGenusId write FGenusId;
    property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property Notes: String read FNotes write FNotes;
  end;

  function AuthorListToString(aAuthors: TAuthors): String;
  procedure StringToAuthorList(const aCitation: String; var aAuthors: TAuthors);

implementation

uses
  cbs_locale, cbs_validations, cbs_fullnames, udm_main;

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
  FOrderId := 0;
  FFamilyId := 0;
  FSubfamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
  FIndividualId := 0;
  FNestId := 0;
  FEggId := 0;
  FPreparationDate := StrToDate('30/12/1500');
  FPreparerId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FNotes := EmptyStr;
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
    Add('SELECT * FROM sample_preps');
    Add('WHERE sample_prep_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('sample_prep_id').AsInteger;
      FSpecimenId := FieldByName('specimen_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FAccessionNum := FieldByName('accession_num').AsString;
      FAccessionType := FieldByName('accession_type').AsString;
      FAccessionSeq := FieldByName('accession_seq').AsInteger;
      FTaxonId := FieldByName('taxon_id').AsInteger;
      FOrderId := FieldByName('order_id').AsInteger;
      FFamilyId := FieldByName('family_id').AsInteger;
      FSubfamilyId := FieldByName('subfamily_id').AsInteger;
      FGenusId := FieldByName('genus_id').AsInteger;
      FSpeciesId := FieldByName('species_id').AsInteger;
      FIndividualId := FieldByName('individual_id').AsInteger;
      FNestId := FieldByName('nest_id').AsInteger;
      FEggId := FieldByName('egg_id').AsInteger;
      FPreparationDate := FieldByName('preparation_date').AsDateTime;
      FPreparerId := FieldByName('preparer_id').AsInteger;
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
      FNotes := FieldByName('notes').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
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

  if FieldValuesDiff('N'#186' tombo', aOld.AccessionNum, FAccessionNum, R) then
    aList.Add(R);
  if FieldValuesDiff('Material', aOld.AccessionType, FAccessionType, R) then
    aList.Add(R);
  if FieldValuesDiff('N'#186' sequencial', aOld.AccessionSeq, FAccessionSeq, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff('Ovo', aOld.EggId, FEggId, R) then
    aList.Add(R);
  if FieldValuesDiff('Data de preparo', aOld.PreparationDate, FPreparationDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Preparador', aOld.PreparerId, FPreparerId, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  FOrderId := 0;
  FFamilyId := 0;
  FSubfamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
  FIndividualId := 0;
  FNestId := 0;
  FEggId := 0;
  FCollectionDate := '00.00.0000';
  FCollectionDay := 0;
  FCollectionMonth := 0;
  FCollectionYear := 0;
  FLocalityId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FCollectors := EmptyStr;
  FCollector1 := 0;
  FCollector2 := 0;
  FCollector3 := 0;
  FCollector4 := 0;
  FNotes := EmptyStr;
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
    Add('SELECT * FROM specimens');
    Add('WHERE specimen_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('specimen_id').AsInteger;
      FFieldNumber := FieldByName('field_number').AsString;
      FSampleType := FieldByName('sample_type').AsString;
      FFullName := FieldByName('full_name').AsString;
      FTaxonId := FieldByName('taxon_id').AsInteger;
      FOrderId := FieldByName('order_id').AsInteger;
      FFamilyId := FieldByName('family_id').AsInteger;
      FSubfamilyId := FieldByName('subfamily_id').AsInteger;
      FGenusId := FieldByName('genus_id').AsInteger;
      FSpeciesId := FieldByName('species_id').AsInteger;
      FIndividualId := FieldByName('individual_id').AsInteger;
      FNestId := FieldByName('nest_id').AsInteger;
      FEggId := FieldByName('egg_id').AsInteger;
      FCollectionDate := FieldByName('collection_date').AsString;
      FCollectionDay := FieldByName('collection_day').AsInteger;
      FCollectionMonth := FieldByName('collection_month').AsInteger;
      FCollectionYear := FieldByName('collection_year').AsInteger;
      FLocalityId := FieldByName('locality_id').AsInteger;
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
      FLatitude := FieldByName('latitude').AsFloat;
      FLongitude := FieldByName('longitude').AsFloat;
      FCollectors := FieldByName('collectors').AsString;
      FCollector1 := FieldByName('collector_1').AsInteger;
      FCollector2 := FieldByName('collector_2').AsInteger;
      FCollector3 := FieldByName('collector_3').AsInteger;
      FCollector4 := FieldByName('collector_4').AsInteger;
      FNotes := FieldByName('notes').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
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

  if FieldValuesDiff('N'#186' campo', aOld.FieldNumber, FFieldNumber, R) then
    aList.Add(R);
  if FieldValuesDiff('Material', aOld.SampleType, FSampleType, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionTaxon, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionLocality, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionIndividual, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionNest, aOld.NestId, FNestId, R) then
    aList.Add(R);
  if FieldValuesDiff('Ovo', aOld.EggId, FEggId, R) then
    aList.Add(R);
  if FieldValuesDiff('Data coleta', aOld.CollectionDate, FCollectionDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff('Dia coleta', aOld.CollectionDay, FCollectionDay, R) then
    aList.Add(R);
  if FieldValuesDiff('M'#234's coleta', aOld.CollectionMonth, FCollectionMonth, R) then
    aList.Add(R);
  if FieldValuesDiff('Ano coleta', aOld.CollectionYear, FCollectionYear, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletores', aOld.Collectors, FCollectors, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletor 1', aOld.Collector1, FCollector1, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletor 2', aOld.Collector2, FCollector2, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletor 3', aOld.Collector3, FCollector3, R) then
    aList.Add(R);
  if FieldValuesDiff('Coletor 4', aOld.Collector4, FCollector4, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  Name := EmptyStr;
  StartDate := StrToDate('30/12/1500');
  EndDate := StrToDate('30/12/1500');
  LocalityId := 0;
  ProjectId := 0;
  MunicipalityId := 0;
  StateId := 0;
  CountryId := 0;
  Description := EmptyStr;
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
    Add('SELECT * FROM expeditions');
    Add('WHERE expedition_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('expedition_id').AsInteger;
      FName := FieldByName('expedition_name').AsString;
      FStartDate := FieldByName('start_date').AsDateTime;
      FEndDate := FieldByName('end_date').AsDateTime;
      FLocalityId := FieldByName('locality_id').AsInteger;
      FProjectId := FieldByName('project_id').AsInteger;
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
      FDescription := FieldByName('description').AsString;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
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

  if FieldValuesDiff(rsCaptionName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff('Data inicial', aOld.StartDate, FStartDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Data final', aOld.EndDate, FEndDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionLocality, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionProject, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff('Descri'#231#227'o', aOld.Description, FDescription, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  inherited;
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
  FTotalOpenTime := 0.0;
  FNetLength := 0.0;
  FNetHeight := 0.0;
  FNetArea := 0.0;
  FNetMesh := EmptyStr;
  FNotes := EmptyStr;
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
    Add('SELECT * FROM nets_effort');
    Add('WHERE net_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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
      FTotalOpenTime := FieldByName('open_time_total').AsFloat;
      FNetLength := FieldByName('net_length').AsFloat;
      FNetHeight := FieldByName('net_height').AsFloat;
      FNetArea := FieldByName('net_area').AsFloat;
      FNetMesh := FieldByName('net_mesh').AsString;
      FNotes := FieldByName('note').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
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

function TNetEffort.Diff(aOld: TNetEffort; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionSamplingPlot, aOld.NetStationId, FNetStationId, R) then
    aList.Add(R);
  if FieldValuesDiff('Rede permanente', aOld.PermanentNetId, FPermanentNetId, R) then
    aList.Add(R);
  if FieldValuesDiff('N'#186' da rede', aOld.NetNumber, FNetNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Abertura 1', aOld.NetOpen1, FNetOpen1, R) then
    aList.Add(R);
  if FieldValuesDiff('Fechamento 1', aOld.NetClose1, FNetClose1, R) then
    aList.Add(R);
  if FieldValuesDiff('Abertura 2', aOld.NetOpen2, FNetOpen2, R) then
    aList.Add(R);
  if FieldValuesDiff('Fechamento 2', aOld.NetClose2, FNetClose2, R) then
    aList.Add(R);
  if FieldValuesDiff('Abertura 3', aOld.NetOpen3, FNetOpen3, R) then
    aList.Add(R);
  if FieldValuesDiff('Fechamento 3', aOld.NetClose3, FNetClose3, R) then
    aList.Add(R);
  if FieldValuesDiff('Tempo aberta', aOld.TotalOpenTime, FTotalOpenTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Comprimento', aOld.NetLength, FNetLength, R) then
    aList.Add(R);
  if FieldValuesDiff('Altura', aOld.NetHeight, FNetHeight, R) then
    aList.Add(R);
  if FieldValuesDiff(#193'rea', aOld.NetArea, FNetArea, R) then
    aList.Add(R);
  if FieldValuesDiff('Malha', aOld.NetMesh, FNetMesh, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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

procedure TSurveyMember.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM survey_team');
    Add('WHERE survey_member_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('survey_member_id').AsInteger;
      FSurveyId := FieldByName('survey_id').AsInteger;
      FPersonId := FieldByName('person_id').AsInteger;
      FVisitor := FieldByName('visitor').AsBoolean;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
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

  if FieldValuesDiff('Pesquisador', aOld.PersonId, FPersonId, R) then
    aList.Add(R);
  if FieldValuesDiff('Visitante', aOld.Visitor, FVisitor, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  FSurveyDate := StrToDate('30/12/1500');
  FStartTime := StrToTime('00:00:00');
  FEndTime := StrToTime('00:00:00');
  FDuration := 0;
  FMethodId := 0;
  FNetStationId := 0;
  FExpeditionId := 0;
  FLocalityId := 0;
  FProjectId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FSampleId := EmptyStr;
  FStartLatitude := 0.0;
  FStartLongitude := 0.0;
  FEndLatitude := 0.0;
  FEndLongitude := 0.0;
  FTotalArea := 0.0;
  FTotalDistance := 0.0;
  FTotalNets := 0;
  FCloudCoverOpen := 0;
  FCloudCoverMid := 0;
  FCloudCoverClose := 0;
  FPrecipitationOpen := EmptyStr;
  FPrecipitationMid := EmptyStr;
  FPrecipitationClose := EmptyStr;
  FTemperatureOpen := 0.0;
  FTemperatureMid := 0.0;
  FTemperatureClose := 0.0;
  FWindOpen := 0;
  FWindMid := 0;
  FWindClose := 0;
  FHumidityOpen := 0.0;
  FHumidityMid := 0.0;
  FHumidityClose := 0.0;
  FHabitat := EmptyStr;
  FNetRounds := EmptyStr;
  FFullName := EmptyStr;
  FNotes := EmptyStr;
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
    Add('SELECT * FROM surveys');
    Add('WHERE survey_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
      FSampleId := FieldByName('sample_id').AsString;
      FStartLatitude := FieldByName('start_latitude').AsFloat;
      FStartLongitude := FieldByName('start_longitude').AsFloat;
      FEndLatitude := FieldByName('end_latitude').AsFloat;
      FEndLongitude := FieldByName('end_longitude').AsFloat;
      FTotalArea := FieldByName('area_total').AsFloat;
      FTotalDistance := FieldByName('distance_total').AsFloat;
      FTotalNets := FieldByName('nets_total').AsInteger;
//      TotalNetEffort := FieldByName('AMO_NET_EFFORT').AsFloat;
      FCloudCoverOpen := FieldByName('cloud_cover_start').AsInteger;
      FCloudCoverMid := FieldByName('cloud_cover_middle').AsInteger;
      FCloudCoverClose := FieldByName('cloud_cover_end').AsInteger;
      FPrecipitationOpen := FieldByName('precipitation_start').AsString;
      FPrecipitationMid := FieldByName('precipitation_middle').AsString;
      FPrecipitationClose := FieldByName('precipitation_end').AsString;
      FTemperatureOpen := FieldByName('temperature_start').AsFloat;
      FTemperatureMid := FieldByName('temperature_middle').AsFloat;
      FTemperatureClose := FieldByName('temperature_end').AsFloat;
      FWindOpen := FieldByName('wind_speed_start').AsInteger;
      FWindMid := FieldByName('wind_speed_middle').AsInteger;
      FWindClose := FieldByName('wind_speed_end').AsInteger;
      FHumidityOpen := FieldByName('relative_humidity_start').AsFloat;
      FHumidityMid := FieldByName('relative_humidity_middle').AsFloat;
      FHumidityClose := FieldByName('relative_humidity_end').AsFloat;
      FHabitat := FieldByName('habitat').AsString;
      FNetRounds := FieldByName('net_rounds').AsString;
      FFullName := FieldByName('full_name').AsString;
      FNotes := FieldByName('notes').AsString;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
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
    Clear;
    Add('INSERT INTO surveys (survey_date, start_time, ' +
      'duration, method_id, full_name, locality_id, municipality_id, state_id, ' +
      'country_id, notes, area_total, distance_total, user_inserted, insert_date) ');
    Add('VALUES (date(:adate), time(:atime), :aduration, :amethod, ' +
      ':aname, :alocal, :municipio, :estado, :pais, :anote, :area, :distance, ' +
      ':auser, datetime(''now'',''localtime''));');
    ParamByName('ADATE').AsString := FormatDateTime('yyyy-mm-dd', FSurveyDate);
    ParamByName('ATIME').AsString := TimeToStr(FStartTime);
    ParamByName('ADURATION').AsInteger := FDuration;
    ParamByName('AMETHOD').AsInteger := FMethodId;
    ParamByName('ANAME').AsString := GetSurveyFullname(FSurveyDate, FLocalityId, FMethodId, 0, '');
    ParamByName('ALOCAL').AsInteger := FLocalityId;
    ParamByName('MUNICIPIO').AsInteger := FMunicipalityId;
    ParamByName('ESTADO').AsInteger := FStateId;
    ParamByName('PAIS').AsInteger := FCountryId;
    ParamByName('ANOTE').AsString := FNotes;
    if FTotalArea > 0 then
      ParamByName('AREA').AsFloat := FTotalArea
    else
      ParamByName('AREA').Clear;
    if FTotalDistance > 0 then
      ParamByName('DISTANCE').AsFloat := FTotalDistance
    else
      ParamByName('DISTANCE').Clear;
    ParamByName('AUSER').AsInteger := FUserInserted;
//    GravaLogSQL(SQL);
    ExecSQL;

    // Get the autoincrement key inserted
    Clear;
    Add('SELECT DISTINCT last_insert_rowid() FROM surveys');
    Open;
    FId := Fields[0].AsInteger;
    Close;
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

  if FieldValuesDiff(rsCaptionDate, aOld.SurveyDate, FSurveyDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Hora inicial', aOld.StartTime, FStartTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Hora final', aOld.EndTime, FEndTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Dura'#231#227'o', aOld.Duration, FDuration, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMethod, aOld.MethodId, FMethodId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionSamplingPlot, aOld.NetStationId, FNetStationId, R) then
    aList.Add(R);
  if FieldValuesDiff('Expedi'#231#227'o', aOld.ExpeditionId, FExpeditionId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionLocality, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionState, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, FCountryId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionProject, aOld.ProjectId, FProjectId, R) then
    aList.Add(R);
  if FieldValuesDiff('ID amostra', aOld.SampleId, FSampleId, R) then
    aList.Add(R);
  if FieldValuesDiff('Latitude inicial', aOld.StartLatitude, FStartLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff('Longitude inicial', aOld.StartLongitude, FStartLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff('Latitude final', aOld.EndLatitude, FEndLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff('Longitude final', aOld.EndLongitude, FEndLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(#193'rea', aOld.TotalArea, FTotalArea, R) then
    aList.Add(R);
  if FieldValuesDiff('Dist'#226'ncia', aOld.TotalDistance, FTotalDistance, R) then
    aList.Add(R);
  if FieldValuesDiff('Total redes', aOld.TotalNets, FTotalNets, R) then
    aList.Add(R);
  if FieldValuesDiff('Habitat', aOld.Habitat, FHabitat, R) then
    aList.Add(R);
  if FieldValuesDiff('Revis'#245'es rede', aOld.NetRounds, FNetRounds, R) then
    aList.Add(R);
  if FieldValuesDiff('Nome completo', aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
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
  FPrecipitation := EmptyStr;
  FRainfall := 0;
  FRelativeHumidity := 0;
  FSampleDate := StrToDate('30/12/1500');
  FSampleMoment := EmptyStr;
  FSampleTime := StrToTime('00:00:00');
  FTemperature := 0;
  FWindSpeedBft := 0;
  FWindSpeedKmH := 0;
end;

function TWeatherLog.Diff(aOld: TWeatherLog; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff('Atmospheric pressure', aOld.AtmosphericPressure, FAtmosphericPressure, R) then
    aList.Add(R);
  if FieldValuesDiff('Cloud cover', aOld.CloudCover, FCloudCover, R) then
    aList.Add(R);
  if FieldValuesDiff('Precipitation', aOld.Precipitation, FPrecipitation, R) then
    aList.Add(R);
  if FieldValuesDiff('Rainfall', aOld.Rainfall, FRainfall, R) then
    aList.Add(R);
  if FieldValuesDiff('Relative humidity', aOld.RelativeHumidity, FRelativeHumidity, R) then
    aList.Add(R);
  if FieldValuesDiff('Date', aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff('Time', aOld.SampleTime, FSampleTime, R) then
    aList.Add(R);
  if FieldValuesDiff('Moment', aOld.SampleMoment, FSampleMoment, R) then
    aList.Add(R);
  if FieldValuesDiff('Temperature', aOld.Temperature, FTemperature, R) then
    aList.Add(R);
  if FieldValuesDiff('Wind speed (Bft)', aOld.WindSpeedBft, FWindSpeedBft, R) then
    aList.Add(R);
  if FieldValuesDiff('Wind speed (km/h)', aOld.WindSpeedKmH, FWindSpeedKmH, R) then
    aList.Add(R);
  if FieldValuesDiff('Notes', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
    Add('SELECT * FROM weather_logs');
    Add('WHERE weather_id = :anid');
    ParamByName('ANID').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('weather_id').AsInteger;
      FSurveyId := FieldByName('survey_id').AsInteger;
      FAtmosphericPressure := FieldByName('atmospheric_pressure').AsFloat;
      FCloudCover := FieldByName('cloud_cover').AsInteger;
      FNotes := FieldByName('notes').AsString;
      FPrecipitation := FieldByName('precipitation').AsString;
      FRainfall := FieldByName('rainfall').AsInteger;
      FRelativeHumidity := FieldByName('relative_humidity').AsFloat;
      FSampleDate := FieldByName('sample_date').AsDateTime;
      FSampleMoment := FieldByName('sample_moment').AsString;
      FSampleTime := FieldByName('sample_time').AsDateTime;
      FTemperature := FieldByName('temperature').AsFloat;
      FWindSpeedBft := FieldByName('wind_speed_bft').AsInteger;
      FWindSpeedKmH := FieldByName('wind_speed_kmh').AsFloat;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TNetStation }

constructor TNetStation.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TNetStation.Clear;
begin
  inherited;
  FName := EmptyStr;
  FAcronym := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FLocalityId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
end;

procedure TNetStation.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM net_stations');
    Add('WHERE net_station_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('net_station_id').AsInteger;
      FName := FieldByName('station_name').AsString;
      FAcronym := FieldByName('station_acronym').AsString;
      FLatitude := FieldByName('latitude').AsFloat;
      FLongitude := FieldByName('longitude').AsFloat;
      FLocalityId := FieldByName('locality_id').AsInteger;
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNetStation.Find(aAcronym: String): Boolean;
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
    Add('SELECT net_station_id FROM net_stations');
    Add('WHERE (station_acronym = :aacronym)');
    ParamByName('AACRONYM').AsString := aAcronym;
    Open;
    Result := RecordCount > 0;
    if Result = True then
    begin
      GetData(FieldByName('net_station_id').AsInteger);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TNetStation.Diff(aOld: TNetStation; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rsCaptionName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff('Sigla', aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionLocality, aOld.LocalityId, FLocalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionMunicipality, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionState, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rsCaptionCountry, aOld.CountryId, FCountryId, R) then
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

procedure TPermanentNet.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM permanent_nets');
    Add('WHERE permanent_net_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
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
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
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

  if FieldValuesDiff('N'#186' de rede', aOld.NetNumber, FNetNumber, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rsLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff('Anota'#231#245'es', aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
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
  inherited;
  FName := EmptyStr;
  FAcronym := EmptyStr;
  FDescription := EmptyStr;
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
    Add('SELECT * FROM methods');
    Add('WHERE method_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('method_id').AsInteger;
      FName := FieldByName('method_name').AsString;
      FAcronym := FieldByName('method_acronym').AsString;
      FDescription := FieldByName('description').AsString;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
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

  if FieldValuesDiff(rsCaptionName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff('Abreviatura', aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff('Descrição', aOld.Description, FDescription, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

end.

