unit cbs_import;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  { System }
  SysUtils, Classes, Forms, Dialogs, StrUtils, ComCtrls,
  { Data }
  DB, SQLDB, SdfData;

const
  EbirdSchema: String = 'Submission ID,Common Name,Scientific Name,Taxonomic Order,' +
    'Count,State/Province,County,Location ID,Location,Latitude,Longitude,Date,Time,' +
    'Protocol,Duration (Min),All Obs Reported,Distance Traveled (km),Area Covered (ha),' +
    'Number of Observers,Breeding Code,Observation Details,Checklist Comments,ML Catalog Numbers';

  BandingJournalSchema: String = 'LOCALITY;NET STATION;SAMPLING DATE;START TIME;END TIME;LONGITUDE;LATITUDE;' +
    'TEAM;NOTES';

  WeatherLogSchema: String = 'TIME;MOMENT;CLOUD COVER;PRECIPITATION;TEMPERATURE;WIND SPEED;HUMIDITY';

  NetEffortSchema: String = 'LOCALITY;NET STATION;SAMPLING DATE;NET NUMBER;LONGITUDE;LATITUDE;' +
    'OPEN TIME 1;CLOSE TIME 1;OPEN TIME 2;CLOSE TIME 2;OPEN TIME 3;CLOSE TIME 3;OPEN TIME 4;CLOSE TIME 4;NOTES';

  BandingSchema: String = 'LOCALITY;STATION;DATA;RECORDER;BANDER;CAP TIME;NET SITE NAME;NEW_RECAP;' +
    'BAND_CODE;BAND NUMBER;RIGHT LEG;LEFT LEG;SPECIES NAME;CP;BP;FAT;BODY MOLT;FF MOLT;FF WEAR;' +
    'RIGHT WING;FIRST SECONDARY;TAIL;TARSUS LENGTH;RIGHT TARSUS DIAMETER;WEIGHT;' +
    'MOLT LIMITS;SKULL;CYCLE CODE;HOW AGED;SEX;HOW SEXED;STATUS;ESCAPED;NOTES;' +
    'REMOVED BAND;PHOTOGRAPHER;INITIAL PHOTO NUMBER;FINAL PHOTO NUMBER;CAMERA NAME;PHOTO NAME FORMULA;' +
    'CRANIO;CULMEN EXPOSTO;NP;LARGURA BICO;ALTURA BICO;SANGUE;PENAS;LONGITUDE;LATITUDE;' +
    'KIPPS;GLICOSE;HEMOGLOBINA;HEMATOCRITO;GPS NUMBER';

  NestSchema: String = 'field_number;taxon;male;female;latitude;longitude;altitude;locality;' +
    'height_above_ground;support_plant_1;support_plant_2;max_internal_diameter;min_internal_diameter;' +
    'max_external_diameter;min_external_diameter;internal_height;external_height;plant_center_distance;' +
    'plant_edge_distance;nest_cover;max_plant_diameter;min_plant_diameter;plant_height;plant_dbh;' +
    'productivity;nest_fate;philornis_larvae;found_stage;cause_of_loss;loss_stage;found_day;' +
    'last_day_active;last_seen;nest_age;nest_days_egg;nest_days_nestling;notes';

  NestRevisionSchema: String = 'nest;date;observer;status;eggs_tally;nestlings_tally;photos;notes';

  EggSchema: String = 'nest;date;egg_num;length;width;mass;shape;color;photos;notes';

type

  { TEbirdDownloadFormat }

  TEbirdDownloadFormat = record
    SubmissionID: String;
    CommonName: String;
    ScientificName: String;
    TaxonomicOrder: Integer;
    Count: String;
    StateProvince: String;
    County: String;
    LocationID: String;
    LocationName: String;
    Latitude: Extended;
    Longitude: Extended;
    RecordDate: TDate;
    RecordTime: TTime;
    Protocol: String;
    Duration: Integer;
    AllObsReported: Boolean;
    DistanceTraveled: Double;
    AreaCovered: Double;
    NumberObservers: Integer;
    BreedingCode: String;
    ObservationDetails: String;
    ChecklistComments: String;
    MLCatalogNumber: String;
    procedure GetData(aValue: String);
    procedure Clear;
  end;

  TWeatherSample = record
    SamplingTime: TTime;
    SamplingMoment: (wmNone, wmStart, wmMiddle, wmEnd);
    CloudCover: Integer;
    Precipitation: (wpEmpty = -1, wpNone, wpFog, wpMist, wpDrizzle, wpRain);
    Temperature: Double;
    WindSpeed: Integer;
    Humidity: Double;
  end;

  TNetBout = record
    OpenTime: TTime;
    CloseTime: TTime;
  end;

  { TBandingJournal }

  TBandingJournal = record
    Locality: String;
    NetStation: String;
    SamplingDate: TDate;
    StartTime: TTime;
    EndTime: TTime;
    Longitude: Extended;
    Latitude: Extended;
    Team: String;
    Weather1: TWeatherSample;
    Weather2: TWeatherSample;
    Weather3: TWeatherSample;
    Weather4: TWeatherSample;
    Notes: String;
    procedure Clear;
  end;

  { TBandingEffort }

  TBandingEffort = record
    Locality: String;
    NetStation: String;
    SamplingDate: TDate;
    NetNumber: Integer;
    Longitude: Extended;
    Latitude: Extended;
    NetBout1: TNetBout;
    NetBout2: TNetBout;
    NetBout3: TNetBout;
    NetBout4: TNetBout;
    Notes: String;
    procedure Clear;
  end;

  { TBandingData }

  TBandingData = record
    Locality: String;
    NetStation: String;
    CaptureMonth: Integer;
    CaptureDay: Integer;
    CaptureDate: TDate;
    Recorder: String;
    Bander: String;
    CaptureTime: TTime;
    NetSiteName: String;
    CaptureType: String;
    BandSize: String;
    BandNumber: Integer;
    RightLeg: String;
    LeftLeg: String;
    SpeciesCode: String;
    SpeciesName: String;
    CloacalProtuberance: String;
    BroodPatch: String;
    Fat: String;
    BodyMolt: String;
    FlightFeathersMolt: String;
    FlightFeathersWear: String;
    RightWingChord: Double;
    FirstSecondaryChord: Double;
    TailLength: Double;
    TarsusLength: Double;
    RightTarsusDiameter: Double;
    Weight: Double;
    MoltLimits: String;
    SkullOssification: String;
    CycleCode: String;
    HowAged: String;
    Sex: String;
    HowSexed: String;
    SubjectStatus: String;
    Escaped: Boolean;
    Notes: String;
    RemovedBand: String;
    Photographer1: String;
    Photographer2: String;
    StartPhotoNumber: Integer;
    EndPhotoNumber: Integer;
    CameraName: String;
    PhotoNameFormula: String;
    SkullLength: Double;
    ExposedCulmen: Double;
    NostrilBillTip: Double;
    BillWidth: Double;
    BillHeight: Double;
    BloodSample: Boolean;
    FeatherSample: Boolean;
    Longitude: Extended;
    Latitude: Extended;
    KippsIndex: Double;
    Glucose: Double;
    Hemoglobin: Double;
    Hematocrit: Double;
    GPSNumber: String;
    procedure Clear;
  end;

  TNestRecord = record
    FieldNumber: String;
    Taxon: String;
    Male: String;
    Female: String;
    Latitude: Extended;
    Longitude: Extended;
    Altitude: Double;
    Locality: String;
    HeightAboveGround: Double;
    SupportPlant1: String;
    SupportPlant2: String;
    MaxInternalDiameter: Double;
    MinInternalDiameter: Double;
    MaxExternalDiameter: Double;
    MinExternalDiameter: Double;
    InternalHeight: Double;
    ExternalHeight: Double;
    PlantCenterDistance: Double;
    PlantEdgeDistance: Double;
    NestCover: Double;
    MaxPlantDiameter: Double;
    MinPlantDiameter: Double;
    PlantHeight: Double;
    PlantDbh: Double;
    Productivity: Integer;
    NestFate: String;
    PhilornisLarvae: Boolean;
    FoundStage: String;
    CauseOfLoss: String;
    LossStage: String;
    FoundDay: String;
    LastDayActive: String;
    LastDaySeen: String;
    NestAge: Double;
    NestDaysEgg: Double;
    NestDaysNestling: Double;
    Notes: String;
  end;

  TNestJournal = record
    Nest: String;
    Date: TDate;
    Observer: String;
    Status: String;
    EggsTally: Integer;
    NestlingsTally: Integer;
    Photos: String;
    Notes: String;
  end;

  TEggRecord = record
    Nest: String;
    Date: TDate;
    EggNum: Integer;
    Length: Double;
    Width: Double;
    Mass: Double;
    Shape: String;
    Color: String;
    Photos: String;
    Notes: String;
  end;

  procedure ImportEbirdData(aCSVFile: String);

  procedure ImportBandingDataV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportBandingJournalV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportBandingEffortV1(aCSVFile: String; aProgressBar: TProgressBar = nil);

  procedure ImportNestDataV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportNestRevisionsV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportEggDataV1(aCSVFile: String; aProgressBar: TProgressBar = nil);

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_datatypes, cbs_data, cbs_taxonomy, cbs_birds, cbs_sampling, cbs_gis,
  cbs_breeding, cbs_system, cbs_getvalue, cbs_fullnames, udm_main, udlg_progress;

procedure ImportEbirdData(aCSVFile: String);
var
  CSV: TSdfDataSet;
  Reg: TEbirdDownloadFormat;
  Taxon: TTaxon;
  Toponimo: TSite;
  Survey: TSurvey;
  Sight: TSighting;
  Quant: Integer;
  RDate: String;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg(rsTitleImportFile, Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  Parar := False;
  dlgProgress := TdlgProgress.Create(nil);
  dlgProgress.Show;
  dlgProgress.Title := rsTitleImportFile;
  dlgProgress.Text := rsLoadingCSVFile;

  CSV := TSdfDataSet.Create(nil);
  try
    { Load CSV file using TSdfDataSet }
    with CSV do
    begin
      Delimiter := ',';
      FirstLineAsSchema := True;
      CodePage := 'UTF-8';
      Schema.AddDelimitedText(EbirdSchema, ',', True);
      FileName := aCSVFile;
      Open;
    end;

    dlgProgress.Position := 0;
    dlgProgress.Max := CSV.RecordCount;
    DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      repeat
        dlgProgress.Text := Format(rsProgressRecords,[CSV.RecNo, CSV.RecordCount]);
        Reg.Clear;
        // FS:= TFormatSettings.Create;
        // FS.ShortDateFormat:= 'yyyy-mm-dd';
        // FS.ShortTimeFormat:= 'hh:nn am/pm';
        // FS.DateSeparator:= '-';
        // FS.DecimalSeparator:= '.';

        { Loading field values into TEbirdDownloadFormat }
        { 0 = Submission ID }
        Reg.SubmissionID := CSV.FieldByName('Submission ID').AsString;
        { 1 = Common Name }
        Reg.CommonName := CSV.FieldByName('Common Name').AsString;
        { 2 = Scientific Name }
        Reg.ScientificName := CSV.FieldByName('Scientific Name').AsString;
        { 3 = Taxonomic Order }
        Reg.TaxonomicOrder := CSV.FieldByName('Taxonomic Order').AsInteger;
        { 4 = Count }
        Reg.Count := CSV.FieldByName('Count').AsString;
        { 5 = State/Province }
        Reg.StateProvince := CSV.FieldByName('State/Province').AsString;
        { 6 = County }
        Reg.County := CSV.FieldByName('County').AsString;
        { 7 = Location ID }
        Reg.LocationID := CSV.FieldByName('Location ID').AsString;
        { 8 = Location }
        Reg.LocationName := CSV.FieldByName('Location').AsString;
        { 9 = Latitude }
        if (CSV.FieldByName('Latitude').AsString <> '') then
          Reg.Latitude := CSV.FieldByName('Latitude').AsFloat;
        { 10 = Longitude }
        if (CSV.FieldByName('Longitude').AsString <> '') then
          Reg.Longitude := CSV.FieldByName('Longitude').AsFloat;
        { 11 = Date }
        Reg.RecordDate := CSV.FieldByName('Date').AsDateTime;
        { 12 = Time }
        if (CSV.FieldByName('Time').AsString <> '') then
          Reg.RecordTime := CSV.FieldByName('Time').AsDateTime;
        { 13 = Protocol }
        Reg.Protocol := CSV.FieldByName('Protocol').AsString;
        { 14 = Duration (Min) }
        if (CSV.FieldByName('Duration (Min)').AsString <> '') then
          Reg.Duration := CSV.FieldByName('Duration (Min)').AsInteger;
        { 15 = All Obs Reported }
        Reg.AllObsReported := CSV.FieldByName('All Obs Reported').AsBoolean;
        { 16 = Distance Traveled (km) }
        if (CSV.FieldByName('Distance Traveled (km)').AsString <> '') then
          Reg.DistanceTraveled := CSV.FieldByName('Distance Traveled (km)').AsFloat;
        { 17 = Area Covered (ha) }
        if (CSV.FieldByName('Area Covered (ha)').AsString <> '') then
          Reg.AreaCovered := CSV.FieldByName('Area Covered (ha)').AsFloat;
        { 18 = Number of Observers }
        if (CSV.FieldByName('Number of Observers').AsString <> '') then
          Reg.NumberObservers := CSV.FieldByName('Number of Observers').AsInteger;
        { 19 = Breeding Code }
        if (CSV.FieldByName('Breeding Code').AsString <> '') then
          Reg.BreedingCode := ExtractDelimited(1, CSV.FieldByName('Breeding Code').AsString, [' ']);
        { 20 = Observation Details }
        Reg.ObservationDetails := CSV.FieldByName('Observation Details').AsString;
        { 21 = Checklist Comments }
        Reg.ChecklistComments := CSV.FieldByName('Checklist Comments').AsString;
        { 22 = ML Catalog Numbers }
        Reg.MLCatalogNumber := CSV.FieldByName('ML Catalog Numbers').AsString;

        { Load other variables }
        RDate := FormatDateTime(maskSQLiteDate, Reg.RecordDate);
        if Reg.Count <> 'X' then
          Quant := StrToInt(Reg.Count)
        else
          Quant := 0;

        Toponimo := TSite.Create(GetKey('gazetteer', 'site_id', 'ebird_name', Reg.LocationName));
        Taxon := TTaxon.Create(GetKey('zoo_taxa', 'taxon_id', 'full_name', Reg.ScientificName));
        Survey := TSurvey.Create;
        Sight := TSighting.Create;
        try
          { Find survey (Amostragem) }
          if Survey.Find(Toponimo.Id, RDate) = False then
          begin
            { Create a survey if it does not exist }
            Survey.SurveyDate := Reg.RecordDate;
            Survey.StartTime := Reg.RecordTime;
            Survey.Duration := Reg.Duration;
            Survey.MethodId := GetKey('methods', 'method_id', 'ebird_name', Reg.Protocol);
            Survey.LocalityId := Toponimo.Id;
            Survey.MunicipalityId := Toponimo.MunicipalityId;
            Survey.StateId := Toponimo.StateId;
            Survey.CountryId := Toponimo.CountryId;
            Survey.Notes := Reg.ChecklistComments;
            Survey.TotalArea := Reg.AreaCovered;
            Survey.TotalDistance := Reg.DistanceTraveled;
            Survey.UserInserted := ActiveUser.Id;

            Survey.Insert;
          end;

          { Check if the record already exists }
          if Sight.Find(Survey.Id, Taxon.Id) = False then
          begin
            { Insert record if it does not exist }
            Sight.SurveyId := Survey.Id;
            Sight.TaxonId := Taxon.Id;
            Sight.SightingDate := Reg.RecordDate;
            Sight.Notes := Reg.ObservationDetails;
            Sight.BreedingStatus := Reg.BreedingCode;
            Sight.SubjectTally := Quant;
            Sight.OrderId := Taxon.OrderId;
            Sight.FamilyId := Taxon.FamilyId;
            Sight.GenusId := Taxon.GenusId;
            Sight.SpeciesId := Taxon.SpeciesId;
            Sight.UserInserted := ActiveUser.Id;

            Sight.Insert;
          end
          else
          begin
            { Update record if it exists }
            Sight.Notes := Reg.ObservationDetails;
            Sight.BreedingStatus := Reg.BreedingCode;
            Sight.SubjectTally := Quant;
            Sight.UserUpdated := ActiveUser.Id;

            Sight.Update;
          end;
        finally
          FreeAndNil(Taxon);
          FreeAndNil(Toponimo);
          FreeAndNil(Survey);
          FreeAndNil(Sight);
        end;

        dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or Parar;

      if Parar then
      begin
        DMM.sqlTrans.Rollback;
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        MsgDlg(rsTitleImportFile, rsSuccessfulImportEbird, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;
end;

procedure ImportBandingDataV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingData;
  Taxon: TTaxon;
  Toponimo: TSite;
  Survey: TSurvey;
  Band, RemovedBand: TBand;
  Individuo: TIndividual;
  Captura: TCapture;
  NetStation: TNetStation;
  NetSite: TNetEffort;
  strDate, strTime: String;
  CodAnilha: Integer;
  NetLat, NetLong: Extended;
  // FS: TFormatSettings;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  Parar := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    with CSV do
    begin
      Delimiter := ';';
      FirstLineAsSchema := True;
      CodePage := 'Windows-1252';
      Schema.AddDelimitedText(BandingSchema, ';', True);
      FileName := aCSVFile;
      Open;
    end;

    if Assigned(aProgressBar) then
    begin
      aProgressBar.Position := 0;
      aProgressBar.Max := CSV.RecordCount;
    end
    else
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Position := 0;
      dlgProgress.Max := CSV.RecordCount;
    end;
    DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      repeat
        if Assigned(dlgProgress) then
          dlgProgress.Text := Format(rsProgressRecords, [CSV.RecNo, CSV.RecordCount]);
        // Reset variables
        Reg.Clear;
        CodAnilha := 0;
        NetLat := 500.0;
        NetLong := 500.0;
        strDate := '';
        // Reg.GetData(CSV[i]);
        // FS:= TFormatSettings.Create;
        // FS.ShortDateFormat:= 'dd/mm/yyyy';
        // FS.ShortTimeFormat:= 'hh:nn';
        // FS.DateSeparator:= '/';
        // FS.DecimalSeparator:= ',';

        { 0 = LOCALITY }
        Reg.Locality := CSV.FieldByName('LOCALITY').AsString;
        { 1 = STATION }
        Reg.NetStation := CSV.FieldByName('STATION').AsString;
        { X = MONTH
          if (CSV.Fields[1].AsString <> '') then
          Reg.CaptureMonth:= CSV.Fields[1].AsInteger;
          X = DAY
          if (CSV.Fields[2].AsString <> '') then
          Reg.CaptureDay:= CSV.Fields[2].AsInteger; }
        { 2 = DATA }
        if (not CSV.FieldByName('DATA').IsNull) then
          Reg.CaptureDate := CSV.FieldByName('DATA').AsDateTime;
        { 3 = RECORDER }
        Reg.Recorder := AnsiUpperCase(CSV.FieldByName('RECORDER').AsString);
        { 4 = BANDER }
        Reg.Bander := AnsiUpperCase(CSV.FieldByName('BANDER').AsString);
        { 5 = CAP TIME }
        if (not CSV.FieldByName('CAP TIME').IsNull) then
          Reg.CaptureTime := CSV.FieldByName('CAP TIME').AsDateTime
        else
          Reg.CaptureTime := StrToTime('00:00:01');
        { 6 = NET SITE NAME }
        if (CSV.FieldByName('NET SITE NAME').AsString = '') then
          Reg.NetSiteName := '0'
        else
          Reg.NetSiteName := CSV.FieldByName('NET SITE NAME').AsString;
        { 7 = NEW_RECAP }
        Reg.CaptureType := AnsiUpperCase(CSV.FieldByName('NEW_RECAP').AsString);
        { 8 = BAND_CODE }
        if (Reg.CaptureType <> 'U') then
          Reg.BandSize := AnsiUpperCase(CSV.FieldByName('BAND_CODE').AsString);
        { 9 = BAND NUMBER }
        if (Reg.BandSize <> '') and (Reg.CaptureType <> 'U') then
          Reg.BandNumber := CSV.FieldByName('BAND NUMBER').AsInteger;
        { 10 = RIGHT LEG }
        Reg.RightLeg := AnsiUpperCase(CSV.FieldByName('RIGHT LEG').AsString);
        { 11 = LEFT LEG }
        Reg.LeftLeg := AnsiUpperCase(CSV.FieldByName('LEFT LEG').AsString);
        { X = SPECIES CODE
          Reg.SpeciesCode:= AnsiUpperCase(CSV.Fields[13].AsString); }
        { 12 = SPECIES NAME }
        Reg.SpeciesName := CSV.FieldByName('SPECIES NAME').AsString;
        { 13 = CP }
        Reg.CloacalProtuberance := AnsiUpperCase(CSV.FieldByName('CP').AsString);
        { 14 = BP }
        Reg.BroodPatch := AnsiUpperCase(CSV.FieldByName('BP').AsString);
        { 15 = FAT }
        Reg.Fat := AnsiUpperCase(CSV.FieldByName('FAT').AsString);
        { 16 = BODY MOLT }
        Reg.BodyMolt := AnsiUpperCase(CSV.FieldByName('BODY MOLT').AsString);
        { 17 = FF MOLT }
        Reg.FlightFeathersMolt := AnsiUpperCase(CSV.FieldByName('FF MOLT').AsString);
        { 18 = FF WEAR }
        Reg.FlightFeathersWear := AnsiUpperCase(CSV.FieldByName('FF WEAR').AsString);
        { 19 = RIGHT WING }
        if (not CSV.FieldByName('RIGHT WING').IsNull) then
          Reg.RightWingChord := CSV.FieldByName('RIGHT WING').AsFloat;
        { 20 = FIRST SECONDARY }
        if (not CSV.FieldByName('FIRST SECONDARY').IsNull) then
          Reg.FirstSecondaryChord := CSV.FieldByName('FIRST SECONDARY').AsFloat;
        { 21 = TAIL }
        if (not CSV.FieldByName('TAIL').IsNull) then
          Reg.TailLength := CSV.FieldByName('TAIL').AsFloat;
        { 22 = TARSUS LENGTH }
        if (not CSV.FieldByName('TARSUS LENGTH').IsNull) then
          Reg.TarsusLength := CSV.FieldByName('TARSUS LENGTH').AsFloat;
        { 23 = RIGHT TARSUS DIAMETER }
        if (not CSV.FieldByName('RIGHT TARSUS DIAMETER').IsNull) then
          Reg.RightTarsusDiameter := CSV.FieldByName('RIGHT TARSUS DIAMETER').AsFloat;
        { 24 = WEIGHT }
        if (not CSV.FieldByName('WEIGHT').IsNull) then
          Reg.Weight := CSV.FieldByName('WEIGHT').AsFloat;
        { 25 = MOLT LIMITS }
        Reg.MoltLimits := AnsiUpperCase(CSV.FieldByName('MOLT LIMITS').AsString);
        { 26 = SKULL }
        Reg.SkullOssification := AnsiUpperCase(CSV.FieldByName('SKULL').AsString);
        { 27 = CYCLE CODE }
        Reg.CycleCode := AnsiUpperCase(CSV.FieldByName('CYCLE CODE').AsString);
        { 28 = HOW AGED }
        Reg.HowAged := AnsiUpperCase(CSV.FieldByName('HOW AGED').AsString);
        { 29 = SEX }
        Reg.Sex := AnsiUpperCase(CSV.FieldByName('SEX').AsString);
        { 30 = HOW SEXED }
        Reg.HowSexed := AnsiUpperCase(CSV.FieldByName('HOW SEXED').AsString);
        { 31 = STATUS }
        Reg.SubjectStatus := AnsiUpperCase(CSV.FieldByName('STATUS').AsString);
        { 32 = ESCAPED }
        if (not CSV.FieldByName('ESCAPED').IsNull) then
          Reg.Escaped := CSV.FieldByName('ESCAPED').AsBoolean;
        { 33 = NOTES }
        Reg.Notes := CSV.FieldByName('NOTES').AsString;
        { 34 = REMOVED BAND }
        Reg.RemovedBand := CSV.FieldByName('REMOVED BAND').AsString;
        { 35 = PHOTOGRAPHER }
        Reg.Photographer1 := AnsiUpperCase(CSV.FieldByName('PHOTOGRAPHER').AsString);
        if Pos('/', Reg.Photographer1) > 0 then
        begin
          Reg.Photographer2 := Trim(ExtractWord(2, Reg.Photographer1, ['/']));
          Reg.Photographer1 := Trim(ExtractWord(1, Reg.Photographer1, ['/']));
        end;
        { 36 = INITIAL PHOTO NUMBER }
        if (not CSV.FieldByName('INITIAL PHOTO NUMBER').IsNull) then
          Reg.StartPhotoNumber := CSV.FieldByName('INITIAL PHOTO NUMBER').AsInteger;
        { 37 = FINAL PHOTO NUMBER }
        if (not CSV.FieldByName('FINAL PHOTO NUMBER').IsNull) then
          Reg.EndPhotoNumber := CSV.FieldByName('FINAL PHOTO NUMBER').AsInteger;
        { 38 = CAMERA NAME }
        Reg.CameraName := CSV.FieldByName('CAMERA NAME').AsString;
        { 39 = PHOTO NAME FORMULA }
        Reg.PhotoNameFormula := CSV.FieldByName('PHOTO NAME FORMULA').AsString;
        { 40 = CRANIO }
        if (not CSV.FieldByName('CRANIO').IsNull) then
          Reg.SkullLength := CSV.FieldByName('CRANIO').AsFloat;
        { 41 = CULMEN EXPOSTO }
        if (not CSV.FieldByName('CULMEN EXPOSTO').IsNull) then
          Reg.ExposedCulmen := CSV.FieldByName('CULMEN EXPOSTO').AsFloat;
        { 42 = NP }
        if (not CSV.FieldByName('NP').IsNull) then
          Reg.NostrilBillTip := CSV.FieldByName('NP').AsFloat;
        { 43 = LARGURA BICO }
        if (not CSV.FieldByName('LARGURA BICO').IsNull) then
          Reg.BillWidth := CSV.FieldByName('LARGURA BICO').AsFloat;
        { 44 = ALTURA BICO }
        if (not CSV.FieldByName('ALTURA BICO').IsNull) then
          Reg.BillHeight := CSV.FieldByName('ALTURA BICO').AsFloat;
        { 45 = SANGUE }
        if (not CSV.FieldByName('SANGUE').IsNull) then
          Reg.BloodSample := CSV.FieldByName('SANGUE').AsBoolean;
        { 46 = PENAS }
        if (not CSV.FieldByName('PENAS').IsNull) then
          Reg.FeatherSample := CSV.FieldByName('PENAS').AsBoolean;
        { 47 = LONGITUDE }
        if (not CSV.FieldByName('LONGITUDE').IsNull) then
          Reg.Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
        { 48 = LATITUDE }
        if (not CSV.FieldByName('LATITUDE').IsNull) then
          Reg.Latitude := CSV.FieldByName('LATITUDE').AsFloat;
        { 49 = KIPPS }
        if (not CSV.FieldByName('KIPPS').IsNull) then
          Reg.KippsIndex := CSV.FieldByName('KIPPS').AsFloat;
        { 50 = GLICOSE }
        if (not CSV.FieldByName('GLICOSE').IsNull) then
          Reg.Glucose := CSV.FieldByName('GLICOSE').AsFloat;
        { 51 = HEMOGLOBINA }
        if (not CSV.FieldByName('HEMOGLOBINA').IsNull) then
          Reg.Hemoglobin := CSV.FieldByName('HEMOGLOBINA').AsFloat;
        { 52 = HEMATOCRITO }
        if (not CSV.FieldByName('HEMATOCRITO').IsNull) then
          Reg.Hematocrit := CSV.FieldByName('HEMATOCRITO').AsFloat;
        { 53 = GPS NUMBER }
        Reg.GPSNumber := CSV.FieldByName('GPS NUMBER').AsString;

        // If it is a capture record (including recapture and band change)
        if (Trim(Reg.SpeciesName) <> EmptyStr) then
        begin
          strDate := FormatDateTime(maskSQLiteDate, Reg.CaptureDate);
          strTime := FormatDateTime(maskDisplayTime, Reg.CaptureTime);

          try
            Taxon := TTaxon.Create(GetKey('zoo_taxa', 'taxon_id', 'full_name', Reg.SpeciesName));
            NetStation := TNetStation.Create;
            Toponimo := TSite.Create;
            NetSite := TNetEffort.Create;
            Survey := TSurvey.Create;
            Band := TBand.Create;
            RemovedBand := TBand.Create;
            Individuo := TIndividual.Create;
            Captura := TCapture.Create;

            // Get valid taxon
            if Taxon.ValidId > 0 then
              Taxon.GetData(Taxon.ValidId);

            // Get net station and locality
            if NetStation.Find(Reg.NetStation) then
            begin
              Toponimo.GetData(NetStation.LocalityId);
            end;

            // Get survey
            Survey.Find(Toponimo.Id, strDate, NetStation.Id);

            // Get net and coordinates
            if (Reg.NetSiteName <> EmptyStr) then
            begin
              if NetSite.Find(Survey.Id, Reg.NetSiteName) then
              begin
                NetLat := NetSite.Latitude;
                NetLong := NetSite.Longitude;
              end;
            end;

            // Get band
            if (Reg.BandNumber > 0) then
            begin
              if not Band.Find(Reg.BandSize, Reg.BandNumber) then
              begin
                // If does not exist, insert the new band
                Band.Size := Reg.BandSize;
                Band.Number := Reg.BandNumber;
                Band.Status := 'D';
                Band.SupplierId := GetKey('institutions', 'institution_id', 'acronym', 'CEMAVE');
                Band.BandType := 'A';
                Band.UserInserted := ActiveUser.Id;

                Band.Insert;
              end;
            end;

            // Get removed band
            if (Trim(Reg.RemovedBand) <> EmptyStr) then
            begin
              if WordCount(Reg.RemovedBand, [' ']) = 2 then
              begin
                if not RemovedBand.Find(ExtractWord(1, Reg.RemovedBand, [' ']),
                  StrToInt(ExtractWord(2, Reg.RemovedBand, [' ']))) then
                begin
                  // If does not exist, insert the removed band
                  RemovedBand.Size := ExtractWord(1, Reg.RemovedBand, [' ']);
                  RemovedBand.Number := StrToInt(ExtractWord(2, Reg.RemovedBand, [' ']));
                  RemovedBand.Status := 'D';
                  RemovedBand.SupplierId :=
                    GetKey('institutions', 'institution_id', 'acronym', 'CEMAVE');
                  RemovedBand.BandType := 'A';
                  RemovedBand.UserInserted := ActiveUser.Id;

                  RemovedBand.Insert;
                end;
              end;
            end;

            // Get individual
            if (Reg.CaptureType = 'C') then
              CodAnilha := RemovedBand.Id
            else
              CodAnilha := Band.Id;

            if not Individuo.Find(Taxon.Id, CodAnilha, Reg.RightLeg, Reg.LeftLeg) then
            begin
              // If does not exist, insert the individual
              Individuo.TaxonId := Taxon.Id;
              Individuo.OrderId := Taxon.OrderId;
              Individuo.FamilyId := Taxon.FamilyId;
              Individuo.SubfamilyId := Taxon.SubfamilyId;
              Individuo.GenusId := Taxon.GenusId;
              Individuo.SpeciesId := Taxon.SpeciesId;
              Individuo.BandId := CodAnilha;
              if (Reg.CaptureType = 'C') then
                Individuo.BandName := Reg.RemovedBand
              else
                Individuo.BandName := Format('%s %d', [Reg.BandSize, Reg.BandNumber]);
              Individuo.RightLegBelow := Reg.RightLeg;
              Individuo.LeftLegBelow := Reg.LeftLeg;
              Individuo.UserInserted := ActiveUser.Id;

              Individuo.Insert;
            end;

            // Check if the capture record exists
            if not Captura.Find(Taxon.Id, CodAnilha, Reg.CaptureType, strDate, strTime) then
            begin
              // If does not exist, insert the record
              Captura.SurveyId := Survey.Id;
              Captura.TaxonId := Taxon.Id;
              Captura.IndividualId := Individuo.Id;
              Captura.CaptureDate := Reg.CaptureDate;
              Captura.CaptureTime := Reg.CaptureTime;
              Captura.LocalityId := Toponimo.Id;
              Captura.NetStationId := NetStation.Id;
              Captura.NetId := NetSite.Id;
              Captura.Latitude := NetLat;
              Captura.Longitude := NetLong;
              Captura.BanderId := GetKey('people', 'person_id', 'acronym', Reg.Bander);
              Captura.AnnotatorId := GetKey('people', 'person_id', 'acronym', Reg.Recorder);
              Captura.SubjectStatus := Reg.SubjectStatus;
              Captura.CaptureType := Reg.CaptureType;
              Captura.SubjectSex := Reg.Sex;
              Captura.HowSexed := Reg.HowSexed;
              Captura.BandId := Band.Id;
              Captura.Weight := Reg.Weight;
              Captura.TarsusLength := Reg.TarsusLength;
              Captura.TarsusDiameter := Reg.RightTarsusDiameter;
              Captura.ExposedCulmen := Reg.ExposedCulmen;
              Captura.BillWidth := Reg.BillWidth;
              Captura.BillHeight := Reg.BillHeight;
              Captura.NostrilBillTip := Reg.NostrilBillTip;
              Captura.SkullLength := Reg.SkullLength;
              Captura.RightWingChord := Reg.RightWingChord;
              Captura.FirstSecondaryChord := Reg.FirstSecondaryChord;
              Captura.TailLength := Reg.TailLength;
              Captura.Fat := Reg.Fat;
              Captura.BroodPatch := Reg.BroodPatch;
              Captura.CloacalProtuberance := Reg.CloacalProtuberance;
              Captura.BodyMolt := Reg.BodyMolt;
              Captura.FlightFeathersMolt := Reg.FlightFeathersMolt;
              Captura.FlightFeathersWear := Reg.FlightFeathersWear;
              Captura.MoltLimits := Reg.MoltLimits;
              Captura.CycleCode := Reg.CycleCode;
              Captura.HowAged := Reg.HowAged;
              Captura.SkullOssification := Reg.SkullOssification;
              Captura.KippsIndex := Reg.KippsIndex;
              Captura.Glucose := Reg.Glucose;
              Captura.Hemoglobin := Reg.Hemoglobin;
              Captura.Hematocrit := Reg.Hematocrit;
              Captura.BloodSample := Reg.BloodSample;
              Captura.FeatherSample := Reg.FeatherSample;
              if (Trim(Reg.Photographer1) <> EmptyStr) then
              begin
                Captura.SubjectPhotographed := True;
                Captura.Photographer1Id :=
                  GetKey('people', 'person_id', 'acronym', Reg.Photographer1);
                if (Trim(Reg.Photographer2) <> EmptyStr) then
                  Captura.Photographer2Id :=
                    GetKey('people', 'person_id', 'acronym', Reg.Photographer2);
              end else
              begin
                Captura.SubjectPhotographed := False;
                Captura.Photographer1Id := 0;
                Captura.Photographer2Id := 0;
              end;
              Captura.StartPhotoNumber := IntToStr(Reg.StartPhotoNumber);
              Captura.EndPhotoNumber := IntToStr(Reg.EndPhotoNumber);
              Captura.CameraName := Reg.CameraName;
              Captura.RemovedBandId := RemovedBand.Id;
              Captura.RightLegBelow := Reg.RightLeg;
              Captura.LeftLegBelow := Reg.LeftLeg;
              Captura.Escaped := Reg.Escaped;
              Captura.Notes := Reg.Notes;
              Captura.OrderId := Taxon.OrderId;
              Captura.FamilyId := Taxon.FamilyId;
              Captura.GenusId := Taxon.GenusID;
              Captura.SpeciesId := Taxon.SpeciesId;
              Captura.MunicipalityId := Toponimo.MunicipalityId;
              Captura.StateId := Toponimo.StateId;
              Captura.CountryId := Toponimo.CountryId;
              Captura.UserInserted := ActiveUser.Id;

              Captura.Insert;
            end
            else
            begin
              // If exists, update the record
              Captura.SurveyId := Survey.Id;
              Captura.LocalityId := Toponimo.Id;
              Captura.NetStationId := NetStation.Id;
              Captura.NetId := NetSite.Id;
              Captura.Latitude := NetLat;
              Captura.Longitude := NetLong;
              Captura.CycleCode := Reg.CycleCode;
              Captura.Notes := Reg.Notes;
              Captura.UserUpdated := ActiveUser.Id;

              Captura.Update;
            end;

            // Update band status
            if (Trim(Reg.RemovedBand) <> '') then
              UpdateBand(RemovedBand.Id, Individuo.Id, 'R', Reg.CaptureDate);
            UpdateBand(Band.Id, Individuo.Id, 'U', Reg.CaptureDate);

            // Update individual band
            if Reg.CaptureType = 'N' then
            begin
              UpdateIndividual(Individuo.Id, Reg.CaptureDate);
            end;
            if Reg.CaptureType = 'C' then
              ChangeIndividualBand(Individuo.Id, Band.Id, RemovedBand.Id, Reg.CaptureDate,
                Reg.RemovedBand);
          finally
            FreeAndNil(Taxon);
            FreeAndNil(NetStation);
            FreeAndNil(Toponimo);
            FreeAndNil(NetSite);
            FreeAndNil(Survey);
            FreeAndNil(Band);
            FreeAndNil(RemovedBand);
            FreeAndNil(Individuo);
            FreeAndNil(Captura);
          end;
        end
        else
        // If it is a band record
        begin
          Band := TBand.Create;
          try
            // Get band
            if (Reg.BandNumber > 0) then
            begin
              if not Band.Find(Reg.BandSize, Reg.BandNumber) then
              begin
                // If does not exist, insert the new band
                Band.Size := Reg.BandSize;
                Band.Number := Reg.BandNumber;
                Band.Status := 'D';
                Band.SupplierId := GetKey('institutions', 'institution_id', 'acronym', 'CEMAVE');
                Band.BandType := 'A';
                Band.UserInserted := ActiveUser.Id;

                Band.Insert;
              end;
            end;

            // Update band status
            if (Reg.CaptureType = 'L') then    // Lost band
              UpdateBand(Band.Id, 0, 'L', Reg.CaptureDate)
            else
            if (Reg.CaptureType = 'Q') then    // Broken band
              UpdateBand(Band.Id, 0, 'Q', Reg.CaptureDate);
          finally
            FreeAndNil(Band);
          end;
        end;

        if Assigned(aProgressBar) then
          aProgressBar.Position := CSV.RecNo
        else
        if Assigned(dlgProgress) then
          dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or Parar;
      // end;

      if Parar then
      begin
        DMM.sqlTrans.Rollback;
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        MsgDlg(rsTitleImportFile, rsSuccessfulImportCaptures, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure ImportBandingJournalV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingJournal;
  Toponimo: TSite;
  NetStation: TNetStation;
  Survey: TSurvey;
  Weather1, Weather2, Weather3, Weather4: TWeatherLog;
  Member: TSurveyMember;
  strDate: String;
  pp: Integer;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  Parar := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    with CSV do
    begin
      Delimiter := ';';
      FirstLineAsSchema := True;
      CodePage := 'Windows-1252';
      Schema.AddDelimitedText(BandingJournalSchema, ';', True);
      FileName := aCSVFile;
      Open;
    end;

    if Assigned(aProgressBar) then
    begin
      aProgressBar.Position := 0;
      aProgressBar.Max := CSV.RecordCount;
    end
    else
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Position := 0;
      dlgProgress.Max := CSV.RecordCount;
    end;
    DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      repeat
        if Assigned(dlgProgress) then
          dlgProgress.Text := Format(rsProgressRecords, [CSV.RecNo, CSV.RecordCount]);
        // Reset variables
        Reg.Clear;
        strDate := '';

        { 0 - Locality }
        Reg.Locality := CSV.FieldByName('LOCALITY').AsString;
        { 1 - NetStation }
        Reg.NetStation := CSV.FieldByName('STATION').AsString;
        { 2 - SamplingDate }
        Reg.SamplingDate := CSV.FieldByName('DATE').AsDateTime;
        { 3 - StartTime }
        if (not CSV.FieldByName('START TIME').IsNull) then
          Reg.StartTime := CSV.FieldByName('START TIME').AsDateTime;
        { 4 - EndTime }
        if (not CSV.FieldByName('END TIME').IsNull) then
          Reg.EndTime := CSV.FieldByName('END TIME').AsDateTime;
        { 5 - Longitude }
        if (not CSV.FieldByName('LONGITUDE').IsNull) then
          Reg.Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
        { 6 - Latitude }
        if (not CSV.FieldByName('LATITUDE').IsNull) then
          Reg.Latitude := CSV.FieldByName('LATITUDE').AsFloat;
        { 7 - Team }
        Reg.Team := CSV.FieldByName('TEAM').AsString;
        { 8 - Notes }
        Reg.Notes := CSV.FieldByName('NOTES').AsString;
        { 9 - Weather Time 1 }
        if (not CSV.FieldByName('WEATHER TIME 1').IsNull) then
          Reg.Weather1.SamplingTime := CSV.FieldByName('WEATHER TIME 1').AsDateTime;
        { 10 - Weather Moment 1 }
        if (not CSV.FieldByName('WEATHER MOMENT 1').IsNull) then
          case CSV.FieldByName('WEATHER MOMENT 1').AsString of
            'S': Reg.Weather1.SamplingMoment := wmStart;
            'M': Reg.Weather1.SamplingMoment := wmMiddle;
            'E': Reg.Weather1.SamplingMoment := wmEnd;
          else
            Reg.Weather1.SamplingMoment := wmNone;
          end;
        { 11 - Cloud Cover 1 }
        if (not CSV.FieldByName('CLOUD COVER 1').IsNull) then
          Reg.Weather1.CloudCover := CSV.FieldByName('CLOUD COVER 1').AsInteger;
        { 12 - Precipitation 1 }
        if (not CSV.FieldByName('PRECIPITATION 1').IsNull) then
          case CSV.FieldByName('PRECIPITATION 1').AsString of
            'N': Reg.Weather1.Precipitation := wpNone;
            'F': Reg.Weather1.Precipitation := wpFog;
            'M': Reg.Weather1.Precipitation := wpMist;
            'D': Reg.Weather1.Precipitation := wpDrizzle;
            'R': Reg.Weather1.Precipitation := wpRain;
          end;
        { 13 - Temperature 1 }
        if (not CSV.FieldByName('TEMPERATURE 1').IsNull) then
          Reg.Weather1.Temperature := CSV.FieldByName('TEMPERATURE 1').AsFloat;
        { 14 - Wind Speed 1 }
        if (not CSV.FieldByName('WIND SPEED 1').IsNull) then
          Reg.Weather1.WindSpeed := CSV.FieldByName('WIND SPEED 1').AsInteger;
        { 15 - Humidity 1 }
        if (not CSV.FieldByName('HUMIDITY 1').IsNull) then
          Reg.Weather1.Humidity := CSV.FieldByName('HUMIDITY 1').AsFloat;
        { 16 - Weather Time 2 }
        if (not CSV.FieldByName('WEATHER TIME 2').IsNull) then
          Reg.Weather2.SamplingTime := CSV.FieldByName('WEATHER TIME 2').AsDateTime;
        { 17 - Weather Moment 2 }
        if (not CSV.FieldByName('WEATHER MOMENT 2').IsNull) then
          case CSV.FieldByName('WEATHER MOMENT 2').AsString of
            'S': Reg.Weather2.SamplingMoment := wmStart;
            'M': Reg.Weather2.SamplingMoment := wmMiddle;
            'E': Reg.Weather2.SamplingMoment := wmEnd;
          else
            Reg.Weather2.SamplingMoment := wmNone;
          end;
        { 18 - Cloud Cover 2 }
        if (not CSV.FieldByName('CLOUD COVER 2').IsNull) then
          Reg.Weather2.CloudCover := CSV.FieldByName('CLOUD COVER 2').AsInteger;
        { 19 - Precipitation 2 }
        if (not CSV.FieldByName('PRECIPITATION 2').IsNull) then
          case CSV.FieldByName('PRECIPITATION 2').AsString of
            'N': Reg.Weather2.Precipitation := wpNone;
            'F': Reg.Weather2.Precipitation := wpFog;
            'M': Reg.Weather2.Precipitation := wpMist;
            'D': Reg.Weather2.Precipitation := wpDrizzle;
            'R': Reg.Weather2.Precipitation := wpRain;
          end;
        { 20 -Temperature 2 }
        if (not CSV.FieldByName('TEMPERATURE 2').IsNull) then
          Reg.Weather2.Temperature := CSV.FieldByName('TEMPERATURE 2').AsFloat;
        { 21 - Wind Speed 2 }
        if (not CSV.FieldByName('WIND SPEED 2').IsNull) then
          Reg.Weather2.WindSpeed := CSV.FieldByName('WIND SPEED 2').AsInteger;
        { 22 - Humidity 2 }
        if (not CSV.FieldByName('HUMIDITY 2').IsNull) then
          Reg.Weather2.Humidity := CSV.FieldByName('HUMIDITY 2').AsFloat;
        { 23 - Weather Time 3 }
        if (not CSV.FieldByName('WEATHER TIME 3').IsNull) then
          Reg.Weather3.SamplingTime := CSV.FieldByName('WEATHER TIME 3').AsDateTime;
        { 24 - Weather Moment 3 }
        if (not CSV.FieldByName('WEATHER MOMENT 3').IsNull) then
          case CSV.FieldByName('WEATHER MOMENT 3').AsString of
            'S': Reg.Weather3.SamplingMoment := wmStart;
            'M': Reg.Weather3.SamplingMoment := wmMiddle;
            'E': Reg.Weather3.SamplingMoment := wmEnd;
          else
            Reg.Weather3.SamplingMoment := wmNone;
          end;
        { 25 - Cloud Cover 3 }
        if (not CSV.FieldByName('CLOUD COVER 3').IsNull) then
          Reg.Weather3.CloudCover := CSV.FieldByName('CLOUD COVER 3').AsInteger;
        { 26 - Precipitation 3 }
        if (not CSV.FieldByName('PRECIPITATION 3').IsNull) then
          case CSV.FieldByName('PRECIPITATION 3').AsString of
            'N': Reg.Weather3.Precipitation := wpNone;
            'F': Reg.Weather3.Precipitation := wpFog;
            'M': Reg.Weather3.Precipitation := wpMist;
            'D': Reg.Weather3.Precipitation := wpDrizzle;
            'R': Reg.Weather3.Precipitation := wpRain;
          end;
        { 27 - Temperature 3 }
        if (not CSV.FieldByName('TEMPERATURE 3').IsNull) then
          Reg.Weather3.Temperature := CSV.FieldByName('TEMPERATURE 3').AsFloat;
        { 28 - Wind Speed 3 }
        if (not CSV.FieldByName('WIND SPEED 3').IsNull) then
          Reg.Weather3.WindSpeed := CSV.FieldByName('WIND SPEED 3').AsInteger;
        { 29 - Humidity 3 }
        if (not CSV.FieldByName('HUMIDITY 3').IsNull) then
          Reg.Weather3.Humidity := CSV.FieldByName('HUMIDITY 3').AsFloat;
        { 30 - Weather Time 4 }
        if (not CSV.FieldByName('WEATHER TIME 4').IsNull) then
          Reg.Weather4.SamplingTime := CSV.FieldByName('WEATHER TIME 4').AsDateTime;
        { 31 - Weather Moment 4 }
        if (not CSV.FieldByName('WEATHER MOMENT 4').IsNull) then
          case CSV.FieldByName('WEATHER MOMENT 4').AsString of
            'S': Reg.Weather4.SamplingMoment := wmStart;
            'M': Reg.Weather4.SamplingMoment := wmMiddle;
            'E': Reg.Weather4.SamplingMoment := wmEnd;
          else
            Reg.Weather4.SamplingMoment := wmNone;
          end;
        { 32 - Cloud Cover 4 }
        if (not CSV.FieldByName('CLOUD COVER 4').IsNull) then
          Reg.Weather4.CloudCover := CSV.FieldByName('CLOUD COVER 4').AsInteger;
        { 33 - Precipitation 4 }
        if (not CSV.FieldByName('PRECIPITATION 4').IsNull) then
          case CSV.FieldByName('PRECIPITATION 4').AsString of
            'N': Reg.Weather4.Precipitation := wpNone;
            'F': Reg.Weather4.Precipitation := wpFog;
            'M': Reg.Weather4.Precipitation := wpMist;
            'D': Reg.Weather4.Precipitation := wpDrizzle;
            'R': Reg.Weather4.Precipitation := wpRain;
          end;
        { 34 - Temperature 4 }
        if (not CSV.FieldByName('TEMPERATURE 4').IsNull) then
          Reg.Weather4.Temperature := CSV.FieldByName('TEMPERATURE 4').AsFloat;
        { 35 - Wind Speed 4 }
        if (not CSV.FieldByName('WIND SPEED 4').IsNull) then
          Reg.Weather4.WindSpeed := CSV.FieldByName('WIND SPEED 4').AsInteger;
        { 36 - Humidity 4 }
        if (not CSV.FieldByName('HUMIDITY 4').IsNull) then
          Reg.Weather4.Humidity := CSV.FieldByName('HUMIDITY 4').AsFloat;

        strDate := FormatDateTime(maskSQLiteDate, Reg.SamplingDate);
        //if not CSV.FieldByName('START TIME').IsNull then
        //  strStartTime := FormatDateTime(maskDisplayTime, Reg.StartTime)
        //else
        //  strStartTime := EmptyStr;
        //if not CSV.FieldByName('END TIME').IsNull then
        //  strEndTime := FormatDateTime(maskDisplayTime, Reg.EndTime)
        //else
        //  strEndTime := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 1').IsNull then
        //  strWeatherTime1 := FormatDateTime(maskDisplayTime, Reg.Weather1.SamplingTime)
        //else
        //  strWeatherTime1 := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 2').IsNull then
        //  strWeatherTime2 := FormatDateTime(maskDisplayTime, Reg.Weather2.SamplingTime)
        //else
        //  strWeatherTime2 := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 3').IsNull then
        //  strWeatherTime3 := FormatDateTime(maskDisplayTime, Reg.Weather3.SamplingTime)
        //else
        //  strWeatherTime3 := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 4').IsNull then
        //  strWeatherTime4 := FormatDateTime(maskDisplayTime, Reg.Weather4.SamplingTime)
        //else
        //  strWeatherTime4 := EmptyStr;


        try
          NetStation := TNetStation.Create;
          Toponimo := TSite.Create;
          Survey := TSurvey.Create;

          // Get net station and locality
          if NetStation.Find(Reg.NetStation) then
          begin
            Toponimo.GetData(NetStation.LocalityId);
          end;

          // Check if the survey exists
          if not Survey.Find(Toponimo.Id, strDate, NetStation.Id) then
          begin
            Survey.SurveyDate := Reg.SamplingDate;
            Survey.StartTime := Reg.StartTime;
            Survey.EndTime := Reg.EndTime;

            Survey.Duration := 0;
            Survey.MethodId := GetKey('methods', 'method_id', 'method_acronym', 'Banding');
            Survey.NetStationId := NetStation.Id;
            Survey.LocalityId := Toponimo.Id;
            Survey.MunicipalityId := Toponimo.MunicipalityId;
            Survey.StateId := Toponimo.StateId;
            Survey.CountryId := Toponimo.CountryId;
            Survey.StartLongitude := Reg.Longitude;
            Survey.StartLatitude := Reg.Latitude;
            //Survey.EndLongitude := ;
            //Survey.EndLatitude := ;
            //Survey.TotalNets := ;
            Survey.Notes := Reg.Notes;

            Survey.Insert;

            if Survey.Id > 0 then
            begin
              // Insert record history
              WriteRecHistory(tbSurveys, haCreated, Survey.Id, '', '', '', rsInsertedByImport);

              // Insert the survey team
              if (Reg.Team <> EmptyStr) then
              try
                Member := TSurveyMember.Create;

                for pp := 0 to (WordCount(Reg.Team, [',', ';']) - 1) do
                begin
                  Member.SurveyId := Survey.Id;
                  Member.PersonId := GetKey('people', 'person_id', 'acronym', ExtractWord(pp, Reg.Team, [',', ';']));

                  Member.Insert;
                  WriteRecHistory(tbSurveyTeams, haCreated, Member.Id, '', '', '', rsInsertedByImport);
                end;

              finally
                FreeAndNil(Member);
              end;

              // Insert the weather logs
              if (Reg.Weather1.Precipitation <> wpEmpty) or (Reg.Weather1.CloudCover >= 0) then
              try
                Weather1 := TWeatherLog.Create;
                Weather1.SurveyId := Survey.Id;
                Weather1.SampleDate := Reg.SamplingDate;
                Weather1.SampleTime := Reg.Weather1.SamplingTime;
                case Reg.Weather1.SamplingMoment of
                  wmStart:  Weather1.SampleMoment := 'S';
                  wmMiddle: Weather1.SampleMoment := 'M';
                  wmEnd:    Weather1.SampleMoment := 'E';
                else
                  Weather1.SampleMoment := EmptyStr;
                end;
                Weather1.Temperature := Reg.Weather1.Temperature;
                case Reg.Weather1.Precipitation of
                  wpNone:    Weather1.Precipitation := 'N';
                  wpFog:     Weather1.Precipitation := 'F';
                  wpMist:    Weather1.Precipitation := 'M';
                  wpDrizzle: Weather1.Precipitation := 'D';
                  wpRain:    Weather1.Precipitation := 'R';
                else
                  Weather1.Precipitation := EmptyStr;
                end;
                Weather1.CloudCover := Reg.Weather1.CloudCover;
                Weather1.WindSpeedBft := Reg.Weather1.WindSpeed;
                Weather1.RelativeHumidity := Reg.Weather1.Humidity;

                Weather1.Insert;
                WriteRecHistory(tbWeatherLogs, haCreated, Weather1.Id, '', '', '', rsInsertedByImport);
              finally
                FreeAndNil(Weather1);
              end;

              if (Reg.Weather2.Precipitation <> wpEmpty) or (Reg.Weather2.CloudCover >= 0) then
              try
                Weather2 := TWeatherLog.Create;
                Weather2.SurveyId := Survey.Id;
                Weather2.SampleDate := Reg.SamplingDate;
                Weather2.SampleTime := Reg.Weather2.SamplingTime;
                case Reg.Weather2.SamplingMoment of
                  wmStart:  Weather2.SampleMoment := 'S';
                  wmMiddle: Weather2.SampleMoment := 'M';
                  wmEnd:    Weather2.SampleMoment := 'E';
                else
                  Weather2.SampleMoment := EmptyStr;
                end;
                Weather2.Temperature := Reg.Weather2.Temperature;
                case Reg.Weather2.Precipitation of
                  wpNone:    Weather2.Precipitation := 'N';
                  wpFog:     Weather2.Precipitation := 'F';
                  wpMist:    Weather2.Precipitation := 'M';
                  wpDrizzle: Weather2.Precipitation := 'D';
                  wpRain:    Weather2.Precipitation := 'R';
                else
                  Weather2.Precipitation := EmptyStr;
                end;
                Weather2.CloudCover := Reg.Weather2.CloudCover;
                Weather2.WindSpeedBft := Reg.Weather2.WindSpeed;
                Weather2.RelativeHumidity := Reg.Weather2.Humidity;

                Weather2.Insert;
                WriteRecHistory(tbWeatherLogs, haCreated, Weather2.Id, '', '', '', rsInsertedByImport);
              finally
                FreeAndNil(Weather2);
              end;

              if (Reg.Weather3.Precipitation <> wpEmpty) or (Reg.Weather3.CloudCover >= 0) then
              try
                Weather3 := TWeatherLog.Create;
                Weather3.SurveyId := Survey.Id;
                Weather3.SampleDate := Reg.SamplingDate;
                Weather3.SampleTime := Reg.Weather3.SamplingTime;
                case Reg.Weather3.SamplingMoment of
                  wmStart:  Weather3.SampleMoment := 'S';
                  wmMiddle: Weather3.SampleMoment := 'M';
                  wmEnd:    Weather3.SampleMoment := 'E';
                else
                  Weather3.SampleMoment := EmptyStr;
                end;
                Weather3.Temperature := Reg.Weather3.Temperature;
                case Reg.Weather3.Precipitation of
                  wpNone:    Weather3.Precipitation := 'N';
                  wpFog:     Weather3.Precipitation := 'F';
                  wpMist:    Weather3.Precipitation := 'M';
                  wpDrizzle: Weather3.Precipitation := 'D';
                  wpRain:    Weather3.Precipitation := 'R';
                else
                  Weather3.Precipitation := EmptyStr;
                end;
                Weather3.CloudCover := Reg.Weather3.CloudCover;
                Weather3.WindSpeedBft := Reg.Weather3.WindSpeed;
                Weather3.RelativeHumidity := Reg.Weather3.Humidity;

                Weather3.Insert;
                WriteRecHistory(tbWeatherLogs, haCreated, Weather3.Id, '', '', '', rsInsertedByImport);
              finally
                FreeAndNil(Weather3);
              end;

              if (Reg.Weather4.Precipitation <> wpEmpty) or (Reg.Weather4.CloudCover >= 0) then
              try
                Weather4 := TWeatherLog.Create;
                Weather4.SurveyId := Survey.Id;
                Weather4.SampleDate := Reg.SamplingDate;
                Weather4.SampleTime := Reg.Weather4.SamplingTime;
                case Reg.Weather4.SamplingMoment of
                  wmStart:  Weather4.SampleMoment := 'S';
                  wmMiddle: Weather4.SampleMoment := 'M';
                  wmEnd:    Weather4.SampleMoment := 'E';
                else
                  Weather4.SampleMoment := EmptyStr;
                end;
                Weather4.Temperature := Reg.Weather4.Temperature;
                case Reg.Weather4.Precipitation of
                  wpNone:    Weather4.Precipitation := 'N';
                  wpFog:     Weather4.Precipitation := 'F';
                  wpMist:    Weather4.Precipitation := 'M';
                  wpDrizzle: Weather4.Precipitation := 'D';
                  wpRain:    Weather4.Precipitation := 'R';
                else
                  Weather4.Precipitation := EmptyStr;
                end;
                Weather4.CloudCover := Reg.Weather4.CloudCover;
                Weather4.WindSpeedBft := Reg.Weather4.WindSpeed;
                Weather4.RelativeHumidity := Reg.Weather4.Humidity;

                Weather4.Insert;
                WriteRecHistory(tbWeatherLogs, haCreated, Weather4.Id, '', '', '', rsInsertedByImport);
              finally
                FreeAndNil(Weather4);
              end;
            end;
          end;

        finally
          FreeAndNil(NetStation);
          FreeAndNil(Toponimo);
          FreeAndNil(Survey);
        end;

        if Assigned(aProgressBar) then
          aProgressBar.Position := CSV.RecNo
        else
        if Assigned(dlgProgress) then
          dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or Parar;

      if Parar then
      begin
        DMM.sqlTrans.Rollback;
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        MsgDlg(rsTitleImportFile, rsSuccessfulImportBandingJournal, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure ImportBandingEffortV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingEffort;
  Toponimo: TSite;
  NetStation: TNetStation;
  Survey: TSurvey;
  NetSite: TNetEffort;
  strDate: String;
begin
  { #todo : Import banding effort }
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  Parar := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    with CSV do
    begin
      Delimiter := ';';
      FirstLineAsSchema := True;
      CodePage := 'Windows-1252';
      Schema.AddDelimitedText(NetEffortSchema, ';', True);
      FileName := aCSVFile;
      Open;
    end;

    if Assigned(aProgressBar) then
    begin
      aProgressBar.Position := 0;
      aProgressBar.Max := CSV.RecordCount;
    end
    else
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Position := 0;
      dlgProgress.Max := CSV.RecordCount;
    end;
    DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      repeat
        if Assigned(dlgProgress) then
          dlgProgress.Text := Format(rsProgressRecords, [CSV.RecNo, CSV.RecordCount]);
        // Reset variables
        Reg.Clear;
        strDate := '';

        { 0 - Locality }
        Reg.Locality := CSV.FieldByName('LOCALITY').AsString;
        { 1 - NetStation }
        Reg.NetStation := CSV.FieldByName('STATION').AsString;
        { 2 - SamplingDate }
        Reg.SamplingDate := CSV.FieldByName('SAMPLING DATE').AsDateTime;
        { 3 - NetNumber }
        Reg.NetNumber := CSV.FieldByName('NET NUMBER').AsInteger;
        { 4 - Longitude }
        if (not CSV.FieldByName('LONGITUDE').IsNull) then
          Reg.Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
        { 5 - Latitude }
        if (not CSV.FieldByName('LATITUDE').IsNull) then
          Reg.Latitude := CSV.FieldByName('LATITUDE').AsFloat;
        { 6 - OpenTime 1 }
        // to know the time is NULL, put 1 second in the field
        if (not CSV.FieldByName('OPEN TIME 1').IsNull) then
          Reg.NetBout1.OpenTime := CSV.FieldByName('OPEN TIME 1').AsDateTime
        else
          Reg.NetBout1.OpenTime := StrToTime('00:00:01');
        { 7 - CloseTime 1 }
        if (not CSV.FieldByName('CLOSE TIME 1').IsNull) then
          Reg.NetBout1.CloseTime := CSV.FieldByName('CLOSE TIME 1').AsDateTime
        else
          Reg.NetBout1.CloseTime := StrToTime('00:00:01');
        { 8 - OpenTime 2 }
        if (not CSV.FieldByName('OPEN TIME 2').IsNull) then
          Reg.NetBout2.OpenTime := CSV.FieldByName('OPEN TIME 2').AsDateTime
        else
          Reg.NetBout2.OpenTime := StrToTime('00:00:01');
        { 9 - CloseTime 2 }
        if (not CSV.FieldByName('CLOSE TIME 2').IsNull) then
          Reg.NetBout2.CloseTime := CSV.FieldByName('CLOSE TIME 2').AsDateTime
        else
          Reg.NetBout2.CloseTime := StrToTime('00:00:01');
        { 10 - OpenTime 3 }
        if (not CSV.FieldByName('OPEN TIME 3').IsNull) then
          Reg.NetBout3.OpenTime := CSV.FieldByName('OPEN TIME 3').AsDateTime
        else
          Reg.NetBout3.OpenTime := StrToTime('00:00:01');
        { 11 - CloseTime 3 }
        if (not CSV.FieldByName('CLOSE TIME 3').IsNull) then
          Reg.NetBout3.CloseTime := CSV.FieldByName('CLOSE TIME 3').AsDateTime
        else
          Reg.NetBout3.CloseTime := StrToTime('00:00:01');
        { 12 - OpenTime 4 }
        if (not CSV.FieldByName('OPEN TIME 4').IsNull) then
          Reg.NetBout4.OpenTime := CSV.FieldByName('OPEN TIME 4').AsDateTime
        else
          Reg.NetBout4.OpenTime := StrToTime('00:00:01');
        { 13 - CloseTime 4 }
        if (not CSV.FieldByName('CLOSE TIME 4').IsNull) then
          Reg.NetBout4.CloseTime := CSV.FieldByName('CLOSE TIME 4').AsDateTime
        else
          Reg.NetBout4.CloseTime := StrToTime('00:00:01');
        { 14 - Notes }
        Reg.Notes := CSV.FieldByName('NOTES').AsString;

        try
          NetStation := TNetStation.Create;
          Toponimo := TSite.Create;
          Survey := TSurvey.Create;
          NetSite := TNetEffort.Create;

          // Get net station and locality
          if NetStation.Find(Reg.NetStation) then
          begin
            Toponimo.GetData(NetStation.LocalityId);
          end;

          // Check if the survey exists
          if Survey.Find(Toponimo.Id, strDate, NetStation.Id) then
          begin
            // Check if the net site exists
            if not NetSite.Find(Survey.Id, IntToStr(Reg.NetNumber)) then
            begin
              // Insert the net effort
              NetSite.SurveyId := Survey.Id;
              NetSite.NetStationId := NetStation.Id;
              NetSite.SampleDate := Reg.SamplingDate;
              NetSite.NetNumber := Reg.NetNumber;
              NetSite.Longitude := Reg.Longitude;
              NetSite.Latitude := Reg.Latitude;
              NetSite.Notes := Reg.Notes;
              NetSite.NetOpen1 := Reg.NetBout1.OpenTime;
              NetSite.NetClose1 := Reg.NetBout1.CloseTime;
              NetSite.NetOpen2 := Reg.NetBout2.OpenTime;
              NetSite.NetClose2 := Reg.NetBout2.CloseTime;
              NetSite.NetOpen3 := Reg.NetBout3.OpenTime;
              NetSite.NetClose3 := Reg.NetBout3.CloseTime;
              NetSite.NetOpen4 := Reg.NetBout4.OpenTime;
              NetSite.NetClose4 := Reg.NetBout4.CloseTime;

              NetSite.Insert;

              // Insert record history
              WriteRecHistory(tbNetsEffort, haCreated, NetSite.Id, '', '', '', rsInsertedByImport);

            end;
          end;

        finally
          FreeAndNil(NetSite);
          FreeAndNil(NetStation);
          FreeAndNil(Toponimo);
          FreeAndNil(Survey);
        end;

        if Assigned(aProgressBar) then
          aProgressBar.Position := CSV.RecNo
        else
        if Assigned(dlgProgress) then
          dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or Parar;

      if Parar then
      begin
        DMM.sqlTrans.Rollback;
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        MsgDlg(rsTitleImportFile, rsSuccessfulImportBandingEffort, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure ImportNestDataV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  //Reg: TNestRecord;
  Toponimo: TSite;
  Taxon: TTaxon;
  Nest: TNest;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  Parar := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    with CSV do
    begin
      Delimiter := ';';
      FirstLineAsSchema := True;
      CodePage := 'Windows-1252';
      //Schema.AddDelimitedText(NetEffortSchema, ';', True);
      FileName := aCSVFile;
      Open;
    end;

    if Assigned(aProgressBar) then
    begin
      aProgressBar.Position := 0;
      aProgressBar.Max := CSV.RecordCount;
    end
    else
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Position := 0;
      dlgProgress.Max := CSV.RecordCount;
    end;
    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      repeat
        if Assigned(dlgProgress) then
          dlgProgress.Text := Format(rsProgressRecords, [CSV.RecNo, CSV.RecordCount]);
        // Reset variables

        try
          Taxon := TTaxon.Create;
          Toponimo := TSite.Create;
          Nest := TNest.Create;

          // Get taxon
          if (CSV.FieldByName('taxon').AsString <> EmptyStr) then
            Taxon.GetData(GetKey('zoo_taxa', 'taxon_id', 'full_name', CSV.FieldByName('taxon').AsString));

          // Get locality
          if (CSV.FieldByName('locality').AsString <> EmptyStr) then
            Toponimo.GetData(GetKey('gazetteer', 'site_id', 'site_name', CSV.FieldByName('locality').AsString));


          // Check if the nest exists
          if not Nest.Find(CSV.FieldByName('field_number').AsString, Taxon.Id, Toponimo.Id,
                    StrToDate(CSV.FieldByName('found_day').AsString)) then
          begin
            Nest.FieldNumber := CSV.FieldByName('field_number').AsString;
            //Nest.ObserverId := GetKey('people', 'person_id', 'acronym', CSV.FieldByName('observer').AsString);
            Nest.LocalityId := Toponimo.Id;
            Nest.Latitude := CSV.FieldByName('latitude').AsFloat;
            Nest.Longitude := CSV.FieldByName('longitude').AsFloat;
            Nest.TaxonId := Taxon.Id;
            //Nest.SupportType := CSV.FieldByName('support_type').AsString;
            Nest.SupportPlant1Id := GetKey('botanic_taxa', 'taxon_id', 'taxon_name', CSV.FieldByName('support_plant_1').AsString);
            Nest.SupportPlant2Id := GetKey('botanic_taxa', 'taxon_id', 'taxon_name', CSV.FieldByName('support_plant_2').AsString);
            //Nest.OtherSupport := CSV.FieldByName('other_support').AsString;
            Nest.HeightAboveGround := CSV.FieldByName('height_above_ground').AsFloat;
            //Nest.ProjectId := GetKey('projects', 'project_id', 'project_title', CSV.FieldByName('project').AsString);
            Nest.InternalMaxDiameter := CSV.FieldByName('max_internal_diameter').AsFloat;
            Nest.InternalMinDiameter := CSV.FieldByName('min_internal_diameter').AsFloat;
            Nest.ExternalMaxDiameter := CSV.FieldByName('max_external_diameter').AsFloat;
            Nest.ExternalMinDiameter := CSV.FieldByName('min_external_diameter').AsFloat;
            Nest.InternalHeight := CSV.FieldByName('internal_height').AsFloat;
            Nest.ExternalHeight := CSV.FieldByName('external_height').AsFloat;
            Nest.EdgeDistance := CSV.FieldByName('plant_edge_distance').AsFloat;
            Nest.CenterDistance := CSV.FieldByName('plant_center_distance').AsFloat;
            Nest.NestCover := CSV.FieldByName('nest_cover').AsInteger;
            Nest.PlantMaxDiameter := CSV.FieldByName('max_plant_diameter').AsFloat;
            Nest.PlantMinDiameter := CSV.FieldByName('min_plant_diameter').AsFloat;
            Nest.PlantHeight := CSV.FieldByName('plant_height').AsFloat;
            Nest.PlantDbh := CSV.FieldByName('plant_dbh').AsFloat;
            //Nest.ConstructionDays: Double;
            Nest.IncubationDays := CSV.FieldByName('nest_days_egg').AsFloat;
            Nest.NestlingDays := CSV.FieldByName('nest_days_nestling').AsFloat;
            Nest.ActiveDays := Nest.IncubationDays + Nest.NestlingDays;
            Nest.NestFate := CSV.FieldByName('nest_fate').AsString;
            Nest.NestProductivity := CSV.FieldByName('productivity').AsInteger;
            Nest.FoundDate := StrToDate(CSV.FieldByName('found_day').AsString);
            Nest.LastDate := StrToDate(CSV.FieldByName('last_day_active').AsString);
            //Nest.Description := CSV.FieldByName('description').AsString;
            Nest.FullName := GetNestFullName(Nest.FoundDate, Nest.TaxonId, Nest.LocalityId, Nest.FieldNumber);
            Nest.UserInserted := ActiveUser.Id;

            Nest.Insert;

            // Insert record history
            WriteRecHistory(tbNests, haCreated, Nest.Id, '', '', '', rsInsertedByImport);

          end;

        finally
          FreeAndNil(Nest);
          FreeAndNil(Toponimo);
          FreeAndNil(Taxon);
        end;

        if Assigned(aProgressBar) then
          aProgressBar.Position := CSV.RecNo
        else
        if Assigned(dlgProgress) then
          dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or Parar;

      if Parar then
      begin
        DMM.sqlTrans.Rollback;
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
        begin
          dlgProgress.Text := rsProgressFinishing;
          MsgDlg(rsTitleImportFile, rsSuccessfulImportBandingEffort, mtInformation);
        end;
        DMM.sqlTrans.CommitRetaining;
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
  end;
end;

procedure ImportNestRevisionsV1(aCSVFile: String; aProgressBar: TProgressBar);
begin

end;

procedure ImportEggDataV1(aCSVFile: String; aProgressBar: TProgressBar);
begin

end;

{ TBandingEffort }

procedure TBandingEffort.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := StrToDate('30/12/1500');
  NetNumber := 0;
  Longitude := 0.0;
  Latitude := 0.0;
  Notes := EmptyStr;

  NetBout1.OpenTime := StrToTime('00:00:01');
  NetBout1.CloseTime := StrToTime('00:00:01');

  NetBout2.OpenTime := StrToTime('00:00:01');
  NetBout2.CloseTime := StrToTime('00:00:01');

  NetBout3.OpenTime := StrToTime('00:00:01');
  NetBout3.CloseTime := StrToTime('00:00:01');

  NetBout4.OpenTime := StrToTime('00:00:01');
  NetBout4.CloseTime := StrToTime('00:00:01');
end;

{ TBandingJournal }

procedure TBandingJournal.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := StrToDate('30/12/1500');
  StartTime := StrToTime('00:00:01');
  EndTime := StrToTime('00:00:01');
  Longitude := 0.0;
  Latitude := 0.0;
  Team := EmptyStr;
  Notes := EmptyStr;

  Weather1.SamplingTime := StrToTime('00:00:01');
  Weather1.SamplingMoment := wmNone;
  Weather1.CloudCover := 0;
  Weather1.Precipitation := wpEmpty;
  Weather1.Temperature := 0.0;
  Weather1.WindSpeed := 0;
  Weather1.Humidity := 0.0;

  Weather2.SamplingTime := StrToTime('00:00:01');
  Weather2.SamplingMoment := wmNone;
  Weather2.CloudCover := 0;
  Weather2.Precipitation := wpEmpty;
  Weather2.Temperature := 0.0;
  Weather2.WindSpeed := 0;
  Weather2.Humidity := 0.0;

  Weather3.SamplingTime := StrToTime('00:00:01');
  Weather3.SamplingMoment := wmNone;
  Weather3.CloudCover := 0;
  Weather3.Precipitation := wpEmpty;
  Weather3.Temperature := 0.0;
  Weather3.WindSpeed := 0;
  Weather3.Humidity := 0.0;

  Weather4.SamplingTime := StrToTime('00:00:01');
  Weather4.SamplingMoment := wmNone;
  Weather4.CloudCover := 0;
  Weather4.Precipitation := wpEmpty;
  Weather4.Temperature := 0.0;
  Weather4.WindSpeed := 0;
  Weather4.Humidity := 0.0;
end;

{ TBandingData }

procedure TBandingData.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  CaptureMonth := 0;
  CaptureDay := 0;
  CaptureDate := StrToDate('30/12/1500');
  Recorder := EmptyStr;
  Bander := EmptyStr;
  CaptureTime := StrToTime('00:00:00');
  NetSiteName := EmptyStr;
  CaptureType := EmptyStr;
  BandSize := EmptyStr;
  BandNumber := 0;
  RightLeg := EmptyStr;
  LeftLeg := EmptyStr;
  SpeciesCode := EmptyStr;
  SpeciesName := EmptyStr;
  CloacalProtuberance := EmptyStr;
  BroodPatch := EmptyStr;
  Fat := EmptyStr;
  BodyMolt := EmptyStr;
  FlightFeathersMolt := EmptyStr;
  FlightFeathersWear := EmptyStr;
  RightWingChord := 0.0;
  FirstSecondaryChord := 0.0;
  TailLength := 0.0;
  TarsusLength := 0.0;
  RightTarsusDiameter := 0.0;
  Weight := 0.0;
  MoltLimits := EmptyStr;
  SkullOssification := EmptyStr;
  CycleCode := EmptyStr;
  HowAged := EmptyStr;
  Sex := EmptyStr;
  HowSexed := EmptyStr;
  SubjectStatus := EmptyStr;
  Escaped := False;
  Notes := EmptyStr;
  RemovedBand := EmptyStr;
  Photographer1 := EmptyStr;
  Photographer2 := EmptyStr;
  StartPhotoNumber := 0;
  EndPhotoNumber := 0;
  CameraName := EmptyStr;
  SkullLength := 0.0;
  ExposedCulmen := 0.0;
  NostrilBillTip := 0.0;
  BillWidth := 0.0;
  BillHeight := 0.0;
  BloodSample := False;
  FeatherSample := False;
  Longitude := 0.0;
  Latitude := 0.0;
  KippsIndex := 0.0;
  Glucose := 0.0;
  Hemoglobin := 0.0;
  Hematocrit := 0.0;
  GPSNumber := EmptyStr;
end;

{ TEbirdDownloadFormat }

procedure TEbirdDownloadFormat.GetData(aValue: String);
var
  Valores: TStringList;
  i: Integer;
begin
  Valores := TStringList.Create;
  Valores.Delimiter := ',';
  Valores.QuoteChar := '"';
  Valores.StrictDelimiter := True;
  try
    // tf:= ExtractStrings([','],[' '],PChar(aValue),Valores);
    Valores.CommaText := aValue;
    for i := 0 to Valores.Count do
    begin
      case i of
        0:  SubmissionID := Valores[i];
        1:  CommonName := Valores[i];
        2:  ScientificName := Valores[i];
        3:  TaxonomicOrder := StrToInt(Valores[i]);
        4:  Count := Valores[i];
        5:  StateProvince := Valores[i];
        6:  County := Valores[i];
        7:  LocationID := Valores[i];
        8:  LocationName := Valores[i];
        9:  Latitude := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        10: Longitude := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        11: RecordDate := StrToDate(Valores[i]);
        12: RecordTime := StrToTime(Valores[i]);
        13: Protocol := Valores[i];
        14: Duration := StrToInt(Valores[i]);
        15: AllObsReported := Boolean(StrToInt(Valores[i]));
        16: DistanceTraveled := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        17: AreaCovered := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        18: NumberObservers := StrToInt(Valores[i]);
        19: BreedingCode := Valores[i];
        20: ObservationDetails := Valores[i];
        21: ChecklistComments := Valores[i];
        22: MLCatalogNumber := Valores[i];
      end;
    end;
  finally
    Valores.Free;
  end;
end;

procedure TEbirdDownloadFormat.Clear;
begin
  SubmissionID := EmptyStr;
  CommonName := EmptyStr;
  ScientificName := EmptyStr;
  TaxonomicOrder := 0;
  Count := EmptyStr;
  StateProvince := EmptyStr;
  County := EmptyStr;
  LocationID := EmptyStr;
  LocationName := EmptyStr;
  Latitude := 0.0;
  Longitude := 0.0;
  RecordDate := StrToDate('01/01/1900');
  RecordTime := StrToTime('00:00:00');
  Protocol := EmptyStr;
  Duration := 0;
  AllObsReported := False;
  DistanceTraveled := 0.0;
  AreaCovered := 0.0;
  NumberObservers := 0;
  BreedingCode := EmptyStr;
  ObservationDetails := EmptyStr;
  ChecklistComments := EmptyStr;
  MLCatalogNumber := EmptyStr;
end;

end.
