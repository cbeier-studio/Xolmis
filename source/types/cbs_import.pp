unit cbs_import;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  { System }
  SysUtils, Classes, Dialogs, StrUtils,
  { Data }
  DB, SQLDB, SdfData;

const
  EbirdSchema: String = 'Submission ID,Common Name,Scientific Name,Taxonomic Order,' +
    'Count,State/Province,County,Location ID,Location,Latitude,Longitude,Date,Time,' +
    'Protocol,Duration (Min),All Obs Reported,Distance Traveled (km),Area Covered (ha),' +
    'Number of Observers,Breeding Code,Observation Details,Checklist Comments,ML Catalog Numbers';

  BandingSchema: String = 'LOCALITY;STATION;DATA;RECORDER;BANDER;CAP TIME;NET SITE NAME;NEW_RECAP;' +
    'BAND_CODE;BAND NUMBER;RIGHT LEG;LEFT LEG;SPECIES NAME;CP;BP;FAT;BODY MOLT;FF MOLT;FF WEAR;' +
    'RIGHT WING;FIRST SECONDARY;TAIL;TARSUS LENGTH;RIGHT TARSUS DIAMETER;WEIGHT;' +
    'MOLT LIMITS;SKULL;CYCLE CODE;HOW AGED;SEX;HOW SEXED;STATUS;ESCAPED;NOTES;' +
    'REMOVED BAND;PHOTOGRAPHER;INITIAL PHOTO NUMBER;FINAL PHOTO NUMBER;CAMERA NAME;PHOTO NAME FORMULA;' +
    'CRANIO;CULMEN EXPOSTO;NP;LARGURA BICO;ALTURA BICO;SANGUE;PENAS;LONGITUDE;LATITUDE;' +
    'KIPPS;GLICOSE;HEMOGLOBINA;HEMATOCRITO;GPS NUMBER';

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
    SamplingMoment: (wmStart, wmMiddle, wmEnd);
    CloudCover: Integer;
    Precipitation: (wpNone, wpFog, wpMist, wpDrizzle, wpRain);
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

  procedure ImportEbirdData(aCSVFile: String);
  procedure ImportBandingDataV1(aCSVFile: String);
  procedure ImportBandingJournalV1(aCSVFile: String);
  procedure ImportBandingEffortV1(aCSVFile: String);

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_data, cbs_taxonomy, cbs_birds, cbs_sampling, cbs_gis,
  cbs_system, cbs_getvalue, udm_main, udlg_progress;

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
        RDate := FormatDateTime('yyyy-mm-dd', Reg.RecordDate);
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

procedure ImportBandingDataV1(aCSVFile: String);
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
  dlgProgress := TdlgProgress.Create(nil);
  dlgProgress.Show;
  dlgProgress.Title := rsTitleImportFile;
  dlgProgress.Text := rsLoadingCSVFile;
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

    dlgProgress.Position := 0;
    dlgProgress.Max := CSV.RecordCount;
    DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      repeat
        dlgProgress.Text := Format(rsProgressRecords, [CSV.RecNo, CSV.RecordCount]);
        // Reinicia variáveis
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
          Reg.CaptureTime := CSV.FieldByName('CAP TIME').AsDateTime;
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
          strDate := FormatDateTime('yyyy-mm-dd', Reg.CaptureDate);
          strTime := FormatDateTime('hh:nn', Reg.CaptureTime);

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

        dlgProgress.Position := CSV.RecNo;
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
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
  end;
end;

procedure ImportBandingJournalV1(aCSVFile: String);
begin
  { #todo : Import banding journal }

  { Locality
    Station
    Date
    Time Start
    Time Stop
    Team
    Cloud_cover OPEN
    Cloud_cover MID
    Cloud_cover CLOSE
    Preciptation OPEN
    Preciptation MID
    Preciptation CLOSE
    Temperature OPEN
    Temperature MID
    Temperature CLOSE
    Wind-Beaufort OPEN
    Wind-Beaufort MID
    Wind-Beaufort CLOSE
    XCOORD
    YCOORD
    NOTES }

end;

procedure ImportBandingEffortV1(aCSVFile: String);
begin
  { #todo : Import banding effort }

  { Locality
    Station
    Date
    Net_Number
    XCOORD
    YCOORD
    Open1
    Close1
    Open2
    Close2
    Open3
    Close3
    Notes }

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

  NetBout1.OpenTime := StrToTime('00:00:00');
  NetBout1.CloseTime := StrToTime('00:00:00');

  NetBout2.OpenTime := StrToTime('00:00:00');
  NetBout2.CloseTime := StrToTime('00:00:00');

  NetBout3.OpenTime := StrToTime('00:00:00');
  NetBout3.CloseTime := StrToTime('00:00:00');

  NetBout4.OpenTime := StrToTime('00:00:00');
  NetBout4.CloseTime := StrToTime('00:00:00');
end;

{ TBandingJournal }

procedure TBandingJournal.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := StrToDate('30/12/1500');
  StartTime := StrToTime('00:00:00');
  EndTime := StrToTime('00:00:00');
  Longitude := 0.0;
  Latitude := 0.0;
  Team := EmptyStr;
  Notes := EmptyStr;

  Weather1.SamplingTime := StrToTime('00:00:00');
  Weather1.SamplingMoment := wmStart;
  Weather1.CloudCover := 0;
  Weather1.Precipitation := wpNone;
  Weather1.Temperature := 0.0;
  Weather1.WindSpeed := 0;
  Weather1.Humidity := 0.0;

  Weather2.SamplingTime := StrToTime('00:00:00');
  Weather2.SamplingMoment := wmStart;
  Weather2.CloudCover := 0;
  Weather2.Precipitation := wpNone;
  Weather2.Temperature := 0.0;
  Weather2.WindSpeed := 0;
  Weather2.Humidity := 0.0;

  Weather3.SamplingTime := StrToTime('00:00:00');
  Weather3.SamplingMoment := wmStart;
  Weather3.CloudCover := 0;
  Weather3.Precipitation := wpNone;
  Weather3.Temperature := 0.0;
  Weather3.WindSpeed := 0;
  Weather3.Humidity := 0.0;

  Weather4.SamplingTime := StrToTime('00:00:00');
  Weather4.SamplingMoment := wmStart;
  Weather4.CloudCover := 0;
  Weather4.Precipitation := wpNone;
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
        0:
          SubmissionID := Valores[i];
        1:
          CommonName := Valores[i];
        2:
          ScientificName := Valores[i];
        3:
          TaxonomicOrder := StrToInt(Valores[i]);
        4:
          Count := Valores[i];
        5:
          StateProvince := Valores[i];
        6:
          County := Valores[i];
        7:
          LocationID := Valores[i];
        8:
          LocationName := Valores[i];
        9:
          Latitude := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        10:
          Longitude := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        11:
          RecordDate := StrToDate(Valores[i]);
        12:
          RecordTime := StrToTime(Valores[i]);
        13:
          Protocol := Valores[i];
        14:
          Duration := StrToInt(Valores[i]);
        15:
          AllObsReported := Boolean(StrToInt(Valores[i]));
        16:
          DistanceTraveled := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        17:
          AreaCovered := StrToFloat(StringReplace(Valores[i], '.', ',', []));
        18:
          NumberObservers := StrToInt(Valores[i]);
        19:
          BreedingCode := Valores[i];
        20:
          ObservationDetails := Valores[i];
        21:
          ChecklistComments := Valores[i];
        22:
          MLCatalogNumber := Valores[i];
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
