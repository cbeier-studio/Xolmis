{ Xolmis CSV Xolmis Banding Import Format library

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

unit io_banding_csv;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StrUtils, ComCtrls, DateUtils, Controls,
  DB, SQLDB, SdfData, fpjson, jsonparser,
  models_sampling, models_record_types, io_core;

const
  BANDING_JOURNAL_SCHEMA: String = 'LOCALITY;STATION;DATE;START TIME;END TIME;LONGITUDE;LATITUDE;TOTAL NETS;TEAM;NOTES;NET CHECKING BOUTS';

  WEATHER_LOG_SCHEMA: String = 'LOCALITY;STATION;DATE;TIME;MOMENT;OBSERVER;CLOUD COVER;PRECIPITATION;TEMPERATURE;WIND SPEED;HUMIDITY;ATM PRESSURE';

  NET_EFFORT_SCHEMA: String = 'LOCALITY;STATION;DATE;NET NUMBER;LONGITUDE;LATITUDE;' +
    'OPEN TIME 1;CLOSE TIME 1;OPEN TIME 2;CLOSE TIME 2;OPEN TIME 3;CLOSE TIME 3;OPEN TIME 4;CLOSE TIME 4;NOTES';

  BANDING_SCHEMA: String = 'LOCALITY;STATION;DATE;RECORDER;BANDER;CAP TIME;NET SITE NAME;TYPE;' +
    'BAND CODE;BAND NUMBER;RIGHT TARSUS;LEFT TARSUS;SPECIES NAME;CP;BP;FAT;BODY MOLT;FF MOLT;FF WEAR;' +
    'RIGHT WING;FIRST SECONDARY;TAIL;TARSUS LENGTH;RIGHT TARSUS DIAMETER;WEIGHT;' +
    'MOLT LIMITS;SKULL;CYCLE CODE;HOW AGED;SEX;HOW SEXED;STATUS;ESCAPED;NOTES;' +
    'REMOVED BAND;PHOTOGRAPHER;INITIAL PHOTO NUMBER;FINAL PHOTO NUMBER;CAMERA NAME;PHOTO NAME FORMULA;' +
    'SKULL LENGTH;EXPOSED CULMEN;NP;BILL WIDTH;BILL HEIGHT;BLOOD SAMPLE;FEATHER SAMPLE;LONGITUDE;LATITUDE;' +
    'KIPPS;GLUCOSE;HEMOGLOBIN;HEMATOCRIT;GPS NUMBER';

type
  TWeatherSample = record
    Locality: String;
    NetStation: String;
    SamplingDate: TDate;
    SamplingTime: TTime;
    SamplingMoment: TWeatherSampleMoment;
    Observer: String;
    CloudCover: Integer;
    Precipitation: TPrecipitation;
    Temperature: Double;
    WindSpeed: Integer;
    Humidity: Double;
    AtmosphericPressure: Double;
  end;

  { TWeatherSampleHelper }

  TWeatherSampleHelper = record helper for TWeatherSample
    procedure Clear;
    procedure FromCSV(CSV: TSdfDataSet);
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
    TotalNets: Integer;
    Team: String;
    Notes: String;
    NetCheckingBouts: String;
  end;

  { TBandingJournalHelper }

  TBandingJournalHelper = record helper for TBandingJournal
    procedure Clear;
    procedure FromCSV(CSV: TSdfDataSet);
  end;

  { TBandingEffort }

  TBandingEffort = record
    Locality: String;
    NetStation: String;
    SamplingDate: TDate;
    NetNumber: String;
    Longitude: Extended;
    Latitude: Extended;
    NetBout1: TNetBout;
    NetBout2: TNetBout;
    NetBout3: TNetBout;
    NetBout4: TNetBout;
    Notes: String;
  end;

  { TBandingEffortHelper }

  TBandingEffortHelper = record helper for TBandingEffort
    procedure Clear;
    procedure FromCSV(CSV: TSdfDataSet);
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
    RightTarsus: String;
    LeftTarsus: String;
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
    InitialPhotoNumber: Integer;
    FinalPhotoNumber: Integer;
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
  end;

  { TBandingDataHelper }

  TBandingDataHelper = record helper for TBandingData
    procedure Clear;
    procedure FromCSV(CSV: TSdfDataSet);
  end;

  procedure LoadBandingFile(const aCSVFile: String; CSV: TSdfDataSet);
  procedure ImportBandingDataV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar = nil);
  procedure ImportBandingJournalV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar = nil);
  procedure ImportBandingEffortV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar = nil);
  procedure ImportBandingWeatherLogV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar = nil);

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_system, utils_validations, utils_conversions,
  data_types, data_getvalue, data_consts, data_services,
  models_users, models_taxonomy, models_birds, models_geo, models_bands,
  models_sampling_plots, io_csv,
  udm_main, udlg_progress, udlg_taxonnotfound;

procedure LoadBandingFile(const aCSVFile: String; CSV: TSdfDataSet);
begin
  with CSV do
  begin
    Delimiter := ';';
    FirstLineAsSchema := True;
    CodePage := 'Windows-1252';
    Schema.AddDelimitedText(BANDING_SCHEMA, ';', True);
    FileName := aCSVFile;
    Open;
  end;
end;

procedure ImportBandingDataV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingData;
  SiteRepo: TSiteRepository;
  Toponimo: TSite;
  SurveyRepo: TSurveyRepository;
  Survey: TSurvey;
  BandRepo: TBandRepository;
  Band, RemovedBand: TBand;
  IndividualRepo: TIndividualRepository;
  Individuo: TIndividual;
  CaptureRepo: TCaptureRepository;
  Captura, OldCaptura: TCapture;
  NetStation: TSamplingPlot;
  SPlotRepo: TSamplingPlotRepository;
  NetRepo: TNetEffortRepository;
  NetSite: TNetEffort;
  strDate, strTime, FSpeciesName: String;
  FBandId, FRemovedBandId, FMethodId, FTaxonId, FSiteId, FStationId: Integer;
  NetLat, NetLong: Extended;
  MoveBand: TBandMovementService;
  UpdInd: TIndividualUpdateService;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Capture import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  if not ValidateCSVSchema(aCSVFile, BANDING_SCHEMA, 'banding') then
    Exit;

  LogEvent(leaStart, Format('Import banding file: %s', [aCSVFile]));
  stopProcess := False;
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
    LoadBandingFile(aCSVFile, CSV);
    LogInfo(Format('CSV file loaded with %d records.', [CSV.RecordCount]));

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
        Reg.Clear;
        FBandId := 0;
        FRemovedBandId := 0;
        NetLat := 0.0;
        NetLong := 0.0;
        strDate := '';

        // Load the record data
        Reg.FromCSV(CSV);

        FSpeciesName := EmptyStr;
        if (Reg.SpeciesName <> EmptyStr) then
          FSpeciesName := Reg.SpeciesName
        else
          FSpeciesName := Reg.SpeciesCode;

        // If it is a capture record (including recapture and band change)
        if (Reg.CaptureType[1] in ['N','R','S','C']) then
        begin
          strDate := FormatDateTime(MASK_ISO_DATE, Reg.CaptureDate);
          strTime := FormatDateTime(MASK_DISPLAY_TIME, Reg.CaptureTime);
          IndividualRepo := TIndividualRepository.Create(DMM.sqlCon);
          CaptureRepo := TCaptureRepository.Create(DMM.sqlCon);
          BandRepo := TBandRepository.Create(DMM.sqlCon);
          SiteRepo := TSiteRepository.Create(DMM.sqlCon);
          SPlotRepo := TSamplingPlotRepository.Create(DMM.sqlCon);
          SurveyRepo := TSurveyRepository.Create(DMM.sqlCon);
          NetRepo := TNetEffortRepository.Create(DMM.sqlCon);
          MoveBand := TBandMovementService.Create(BandRepo);
          UpdInd := TIndividualUpdateService.Create(IndividualRepo);

          try
            NetStation := TSamplingPlot.Create;
            Toponimo := TSite.Create;
            NetSite := TNetEffort.Create;
            Survey := TSurvey.Create;
            Band := TBand.Create;
            RemovedBand := TBand.Create;
            Individuo := TIndividual.Create;
            Captura := TCapture.Create;
            OldCaptura := TCapture.Create;
            FMethodId := GetMethodKey(rsMobileBanding);

            // Get valid taxon
            if (FSpeciesName <> EmptyStr) then
            begin
              FTaxonId := GetValidTaxon(FSpeciesName);
              if (FTaxonId <= 0) then
              begin
                case Options.UnknownTaxonPolicy of
                  //utpAddCustomTaxon: ;
                  utpAsk:
                  begin
                    dlgTaxonNotFound := TdlgTaxonNotFound.Create(nil);
                    with dlgTaxonNotFound do
                    try
                      ShowCustomTaxonOption := False;
                      TaxonName := FSpeciesName;
                      if ShowModal = mrOK then
                      begin
                        case SelectedOption of
                          0: ; // FCustomTaxon
                          1: FTaxonId := TaxonId;
                          2: raise Exception.Create(rsImportAbortedByUser);
                        end;
                      end;
                    finally
                      FreeAndNil(dlgTaxonNotFound);
                    end;
                  end;
                  utpAbort:
                    raise Exception.CreateFmt(rsErrorTaxonNotFound, [FSpeciesName]);
                  //utpIgnore: ;
                end;
              end;
            end
            else
              raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingData.SpeciesName/SpeciesCode']);

            // Get net station
            if (Reg.NetStation <> EmptyStr) then
            begin
              FStationId := GetSamplingPlotKey(Reg.NetStation);
              if (FStationId > 0) then
                SPlotRepo.GetById(FStationId, NetStation)
              else
                raise Exception.CreateFmt(rsErrorSamplingPlotNotFound, [Reg.NetStation]);
            end
            else
              raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingData.NetStation']);

            // Get toponym
            if (Reg.Locality <> EmptyStr) then
            begin
              FSiteId := GetSiteKey(Reg.Locality);
              if (FSiteId > 0) then
                SiteRepo.GetById(FSiteId, Toponimo)
              else
                raise Exception.CreateFmt(rsErrorToponymNotFound, [Reg.Locality]);
            end
            else
              raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingData.Locality']);

            // Get survey
            SurveyRepo.FindBySiteAndDate(Toponimo.Id, FMethodId, Reg.CaptureDate, '', NetStation.Id, Survey);
            if Survey.IsNew then
              raise Exception.CreateFmt(rsErrorSurveyNotFound,
                [Format('SiteId=%d; MethodId=%d; SamplingDate=%s; NetStationId=%d',
                  [Toponimo.Id, FMethodId, DateToStr(Reg.CaptureDate), NetStation.Id])]);

            // Get net and coordinates
            if (Reg.NetSiteName <> EmptyStr) then
            begin
              NetRepo.FindBySurvey(Survey.Id, Reg.NetSiteName, NetSite);
              if not (NetSite.IsNew) then
              begin
                NetLat := NetSite.Latitude;
                NetLong := NetSite.Longitude;
              end;
            end;

            // Get band
            if (Reg.BandNumber > 0) then
            begin
              BandRepo.FindByNumber(Reg.BandSize, Reg.BandNumber, Band);
              if (Band.IsNew) then
              begin
                // If does not exist, insert the new band
                Band.Size := Reg.BandSize;
                Band.Number := Reg.BandNumber;
                Band.Status := bstAvailable;
                Band.SupplierId := xSettings.DefaultBandSupplier;
                Band.BandType := mkButtEndBand;
                Band.UserInserted := ActiveUser.Id;

                BandRepo.Insert(Band);
                // Insert record history
                WriteRecHistory(tbBands, haCreated, Band.Id, '', '', '', rsInsertedByImport);
                LogInfo(Format('Band record inserted with ID=%d', [Band.Id]));
              end;
            end;

            // Get removed band
            if (Trim(Reg.RemovedBand) <> EmptyStr) then
            begin
              Reg.RemovedBand := NormalizeWhitespace(Reg.RemovedBand, True);
              FRemovedBandId := GetBandKey(Reg.RemovedBand);
              if FRemovedBandId > 0 then
                BandRepo.GetById(FRemovedBandId, RemovedBand);
              if (RemovedBand.IsNew) then
              begin
                // If does not exist, insert the removed band
                if WordCount(Reg.RemovedBand, [' ']) = 2 then
                begin
                  RemovedBand.Size := ExtractWord(1, Reg.RemovedBand, [' ']);
                  RemovedBand.Number := StrToInt(ExtractWord(2, Reg.RemovedBand, [' ']));
                end
                else
                if WordCount(Reg.RemovedBand, [' ']) = 1 then
                begin
                  RemovedBand.Size := Reg.RemovedBand[1];
                  RemovedBand.Number := StrToInt(Copy(Reg.RemovedBand, 2, Length(Reg.RemovedBand)));
                end;
                RemovedBand.Status := bstAvailable;
                RemovedBand.SupplierId := xSettings.DefaultBandSupplier;
                RemovedBand.BandType := mkButtEndBand;
                RemovedBand.UserInserted := ActiveUser.Id;

                BandRepo.Insert(RemovedBand);
                // Insert record history
                WriteRecHistory(tbBands, haCreated, RemovedBand.Id, '', '', '', rsInsertedByImport);
                LogInfo(Format('Removed band record inserted with ID=%d', [RemovedBand.Id]));
              end;
            end;

            // Get individual
            if (Reg.CaptureType = 'C') then
              FBandId := RemovedBand.Id
            else
              FBandId := Band.Id;

            IndividualRepo.FindByBand(FTaxonId, FBandId, Reg.RightTarsus, Reg.LeftTarsus, Individuo);
            if (Individuo.IsNew) then
            begin
              // If does not exist, insert the individual
              Individuo.TaxonId := FTaxonId;
              Individuo.BandId := FBandId;
              if (Reg.CaptureType = 'C') then
                Individuo.BandName := Reg.RemovedBand
              else
                Individuo.BandName := Format('%s %d', [Reg.BandSize, Reg.BandNumber]);
              Individuo.RightTarsus := Reg.RightTarsus;
              Individuo.LeftTarsus := Reg.LeftTarsus;
              Individuo.UserInserted := ActiveUser.Id;

              IndividualRepo.Insert(Individuo);
              // Insert record history
              WriteRecHistory(tbIndividuals, haCreated, Individuo.Id, '', '', '', rsInsertedByImport);
              LogInfo(Format('Individual record inserted with ID=%d', [Individuo.Id]));
            end;

            // Check if the capture record exists
            CaptureRepo.FindByBand(FTaxonId, FBandId, Reg.CaptureType, strDate, strTime, Captura);
            if (Captura.IsNew) then
            begin
              // If does not exist, insert the record
              Captura.SurveyId := Survey.Id;
              Captura.TaxonId := FTaxonId;
              Captura.IndividualId := Individuo.Id;
              Captura.CaptureDate := Reg.CaptureDate;
              Captura.CaptureTime := Reg.CaptureTime;
              Captura.LocalityId := Toponimo.Id;
              Captura.NetStationId := NetStation.Id;
              Captura.NetId := NetSite.Id;
              if ((xSettings.AutoFillCoordinates) and (NetLat = 0) and (NetLong = 0)) then
              begin
                if ((NetStation.Longitude <> 0) and (NetStation.Latitude <> 0)) then
                begin
                  Captura.Latitude := NetStation.Latitude;
                  Captura.Longitude := NetStation.Longitude;
                  Captura.CoordinatePrecision := cpApproximated;
                end
                else
                if ((Survey.StartLongitude <> 0) and (Survey.StartLatitude <> 0)) then
                begin
                  Captura.Latitude := Survey.StartLatitude;
                  Captura.Longitude := Survey.StartLongitude;
                  Captura.CoordinatePrecision := cpApproximated;
                end
                else
                if ((Toponimo.Longitude <> 0) and (Toponimo.Latitude <> 0)) then
                begin
                  Captura.Latitude := Toponimo.Latitude;
                  Captura.Longitude := Toponimo.Longitude;
                  Captura.CoordinatePrecision := cpReference;
                end;
              end
              else
              if ((NetLat <> 0) and (NetLong <> 0)) then
              begin
                Captura.Latitude := NetLat;
                Captura.Longitude := NetLong;
                Captura.CoordinatePrecision := cpExact;
              end;
              Captura.BanderId := GetPersonKey(Reg.Bander);
              Captura.AnnotatorId := GetPersonKey(Reg.Recorder);
              Captura.SubjectStatus := StrToSubjectStatus(Reg.SubjectStatus);
              Captura.CaptureType := StrToCaptureType(Reg.CaptureType);
              Captura.SubjectSex := StrToSex(Reg.Sex);
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
              { #todo : Get Age by Cycle Code and Molt Limits }
              Captura.HowAged := Reg.HowAged;
              Captura.SkullOssification := Reg.SkullOssification;
              Captura.KippsDistance := Reg.KippsIndex;
              Captura.Glucose := Reg.Glucose;
              Captura.Hemoglobin := Reg.Hemoglobin;
              Captura.Hematocrit := Reg.Hematocrit;
              Captura.BloodSample := Reg.BloodSample;
              Captura.FeatherSample := Reg.FeatherSample;
              if (Trim(Reg.Photographer1) <> EmptyStr) then
              begin
                Captura.SubjectPhotographed := True;
                Captura.Photographer1Id :=
                  GetPersonKey(Reg.Photographer1);
                if (Trim(Reg.Photographer2) <> EmptyStr) then
                  Captura.Photographer2Id :=
                    GetPersonKey(Reg.Photographer2);
              end else
              begin
                Captura.SubjectPhotographed := False;
                Captura.Photographer1Id := 0;
                Captura.Photographer2Id := 0;
              end;
              Captura.InitialPhotoNumber := IntToStr(Reg.InitialPhotoNumber);
              Captura.FinalPhotoNumber := IntToStr(Reg.FinalPhotoNumber);
              Captura.CameraName := Reg.CameraName;
              Captura.RemovedBandId := RemovedBand.Id;
              Captura.RightTarsus := Reg.RightTarsus;
              Captura.LeftTarsus := Reg.LeftTarsus;
              Captura.Escaped := Reg.Escaped;
              Captura.Notes := Reg.Notes;

              CaptureRepo.Insert(Captura);
              // Insert record history
              WriteRecHistory(tbCaptures, haCreated, Captura.Id, '', '', '', rsInsertedByImport);
              LogInfo(Format('Capture record inserted with ID=%d', [Captura.Id]));
            end
            else
            begin
              // If exists, update the record
              OldCaptura.Assign(Captura);

              Captura.SurveyId := Survey.Id;
              Captura.LocalityId := Toponimo.Id;
              Captura.NetStationId := NetStation.Id;
              Captura.NetId := NetSite.Id;
              if ((xSettings.AutoFillCoordinates) and (NetLat = 0) and (NetLong = 0)) then
              begin
                if ((NetStation.Longitude <> 0) and (NetStation.Latitude <> 0)) then
                begin
                  Captura.Latitude := NetStation.Latitude;
                  Captura.Longitude := NetStation.Longitude;
                  Captura.CoordinatePrecision := cpApproximated;
                end
                else
                if ((Survey.StartLongitude <> 0) and (Survey.StartLatitude <> 0)) then
                begin
                  Captura.Latitude := Survey.StartLatitude;
                  Captura.Longitude := Survey.StartLongitude;
                  Captura.CoordinatePrecision := cpApproximated;
                end
                else
                if ((Toponimo.Longitude <> 0) and (Toponimo.Latitude <> 0)) then
                begin
                  Captura.Latitude := Toponimo.Latitude;
                  Captura.Longitude := Toponimo.Longitude;
                  Captura.CoordinatePrecision := cpReference;
                end;
              end
              else
              if ((NetLat <> 0) and (NetLong <> 0)) then
              begin
                Captura.Latitude := NetLat;
                Captura.Longitude := NetLong;
                Captura.CoordinatePrecision := cpExact;
              end;
              Captura.BanderId := GetPersonKey(Reg.Bander);
              Captura.AnnotatorId := GetPersonKey(Reg.Recorder);
              Captura.SubjectStatus := StrToSubjectStatus(Reg.SubjectStatus);
              Captura.CaptureType := StrToCaptureType(Reg.CaptureType);
              Captura.SubjectSex := StrToSex(Reg.Sex);
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
              Captura.KippsDistance := Reg.KippsIndex;
              Captura.Glucose := Reg.Glucose;
              Captura.Hemoglobin := Reg.Hemoglobin;
              Captura.Hematocrit := Reg.Hematocrit;
              Captura.BloodSample := Reg.BloodSample;
              Captura.FeatherSample := Reg.FeatherSample;
              if (Trim(Reg.Photographer1) <> EmptyStr) then
              begin
                Captura.SubjectPhotographed := True;
                Captura.Photographer1Id :=
                  GetPersonKey(Reg.Photographer1);
                if (Trim(Reg.Photographer2) <> EmptyStr) then
                  Captura.Photographer2Id :=
                    GetPersonKey(Reg.Photographer2);
              end else
              begin
                Captura.SubjectPhotographed := False;
                Captura.Photographer1Id := 0;
                Captura.Photographer2Id := 0;
              end;
              Captura.InitialPhotoNumber := IntToStr(Reg.InitialPhotoNumber);
              Captura.FinalPhotoNumber := IntToStr(Reg.FinalPhotoNumber);
              Captura.CameraName := Reg.CameraName;
              Captura.RemovedBandId := RemovedBand.Id;
              Captura.RightTarsus := Reg.RightTarsus;
              Captura.LeftTarsus := Reg.LeftTarsus;
              Captura.Escaped := Reg.Escaped;
              Captura.Notes := Reg.Notes;

              CaptureRepo.Update(Captura);

              // Insert record history
              WriteDiff(tbCaptures, OldCaptura, Captura, rsEditedByImport);
              LogInfo(Format('Capture record with ID=%d updated', [Captura.Id]));
            end;

            // Update band status
            if (Trim(Reg.RemovedBand) <> EmptyStr) then
            begin
              MoveBand.RemoveFromIndividual(RemovedBand, Individuo.Id, Reg.CaptureDate);
              LogInfo(Format('Band ID=%d status updated to removed', [RemovedBand.Id]));
            end;
            MoveBand.UseInCapture(Band, Individuo.Id, Reg.CaptureDate);
            LogInfo(Format('Band ID=%d status updated to used', [Band.Id]));

            // Update individual band
            if Reg.CaptureType = 'N' then
            begin
              UpdInd.ApplyCaptureToIndividual(Captura);
              LogInfo(Format('Individual ID=%d banding date updated', [Individuo.Id]));
            end
            else
            if Reg.CaptureType = 'C' then
            begin
              UpdInd.ApplyBandRemoval(Captura);
              LogInfo(Format('Individual ID=%d band updated with ID=%d (removed band ID=%d)',
                [Individuo.Id, Band.Id, RemovedBand.Id]));
            end;
          finally
            FreeAndNil(NetStation);
            FreeAndNil(Toponimo);
            FreeAndNil(NetSite);
            FreeAndNil(Survey);
            FreeAndNil(Band);
            FreeAndNil(RemovedBand);
            FreeAndNil(Individuo);
            FreeAndNil(Captura);
            FreeAndNil(OldCaptura);
            UpdInd.Free;
            MoveBand.Free;
            BandRepo.Free;
            SiteRepo.Free;
            SPlotRepo.Free;
            CaptureRepo.Free;
            IndividualRepo.Free;
            NetRepo.Free;
            SurveyRepo.Free;
          end;
        end
        else
        // If it is a band record
        begin
          BandRepo := TBandRepository.Create(DMM.sqlCon);
          MoveBand := TBandMovementService.Create(BandRepo);
          Band := TBand.Create;
          try
            // Get band
            if (Reg.BandNumber > 0) then
            begin
              BandRepo.FindByNumber(Reg.BandSize, Reg.BandNumber, Band);
              if (Band.IsNew) then
              begin
                // If does not exist, insert the new band
                Band.Size := Reg.BandSize;
                Band.Number := Reg.BandNumber;
                Band.Status := bstAvailable;
                Band.SupplierId := xSettings.DefaultBandSupplier;
                Band.BandType := mkButtEndBand;
                Band.UserInserted := ActiveUser.Id;

                BandRepo.Insert(Band);
                // Insert record history
                WriteRecHistory(tbBands, haCreated, Band.Id, '', '', '', rsInsertedByImport);
                LogInfo(Format('Band record inserted with ID=%d', [Band.Id]));
              end;
            end;

            // Update band status
            if (Reg.CaptureType = 'L') then    // Lost band
            begin
              MoveBand.MarkAsLost(Band, Reg.CaptureDate);
              //UpdateBand(Band.Id, 0, 'L', Reg.CaptureDate);
              LogInfo(Format('Band ID=%d status updated to lost', [Band.Id]));
            end
            else
            if (Reg.CaptureType = 'B') then    // Broken band
            begin
              MoveBand.MarkAsBroken(Band, Reg.CaptureDate);
              //UpdateBand(Band.Id, 0, 'B', Reg.CaptureDate);
              LogInfo(Format('Band ID=%d status updated to broken', [Band.Id]));
            end;
          finally
            FreeAndNil(Band);
            MoveBand.Free;
            BandRepo.Free;
          end;
        end;

        if Assigned(aProgressBar) then
          aProgressBar.Position := CSV.RecNo
        else
        if Assigned(dlgProgress) then
          dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or stopProcess;
      // end;

      if stopProcess then
      begin
        DMM.sqlTrans.Rollback;
        LogWarning('Capture import canceled by user, transaction rolled back.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Capture import finished successfully, transaction committed.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsSuccessfulImportCaptures, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during capture import, transaction rolled back.');
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
    LogEvent(leaFinish, 'Import banding file');
  end;
end;

procedure ImportBandingJournalV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar);

  procedure UpdateProgress(Current, Total: Integer);
  begin
    if Assigned(aProgressBar) then
    begin
      aProgressBar.Position := Current;
      aProgressBar.Max := Total;
    end
    else if Assigned(dlgProgress) then
    begin
      dlgProgress.Position := Current;
      dlgProgress.Max := Total;
      dlgProgress.Text := Format(rsProgressRecords, [Current, Total]);
    end;
  end;

  procedure InsertSurveyTeam(const TeamStr: String; SurveyId: Integer);
  var
    Member, OldMember: TSurveyMember;
    i, FPersonId: Integer;
    MemberRepo: TSurveyMemberRepository;
    FPersonName: String;
  begin
    if TeamStr = '' then
      Exit;

    MemberRepo := TSurveyMemberRepository.Create(DMM.sqlCon);
    Member := TSurveyMember.Create;
    OldMember := TSurveyMember.Create;
    try
      for i := 1 to WordCount(TeamStr, [',',';']) do
      begin
        FPersonName := ExtractWord(i, TeamStr, [',',';']);
        FPersonId := GetPersonKey(FPersonName);
        if (FPersonId <= 0) then
          raise Exception.CreateFmt('InsertSurveyTeam: ' + rsErrorObserverNotFound, [FPersonName]);

        MemberRepo.FindBySurvey(SurveyId, FPersonId, Member);
        if not Member.IsNew then
        begin
          OldMember.Assign(Member);

          case Options.ExistingRecordPolicy of
            erpIgnoreExisting: ;
            erpUpdateExisting:
            begin
              Member.SurveyId := SurveyId;
              Member.PersonId := FPersonId;

              MemberRepo.Update(Member);
              LogInfo(Format('Survey member record with ID=%d updated', [Member.Id]));

              WriteDiff(tbSurveyTeams, OldMember, Member, rsEditedByImport);
            end;
          end;
        end
        else
        begin
          Member.SurveyId := SurveyId;
          Member.PersonId := FPersonId;

          MemberRepo.Insert(Member);
          LogInfo(Format('Survey member record inserted with ID=%d', [Member.Id]));

          WriteRecHistory(tbSurveyTeams, haCreated, Member.Id, '', '', '', rsInsertedByImport);
        end;
      end;
    finally
      Member.Free;
      OldMember.Free;
      MemberRepo.Free;
    end;
  end;

var
  CSV: TSdfDataSet;
  Reg: TBandingJournal;
  Survey, OldSurvey: TSurvey;
  aMethod, FSiteId, FStationId: Integer;
  SurveyRepo: TSurveyRepository;
  Toponimo: TSite;
  NetStation: TSamplingPlot;
  SiteRepo: TSiteRepository;
  SPlotRepo: TSamplingPlotRepository;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Banding journal import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  if not ValidateCSVSchema(aCSVFile, BANDING_JOURNAL_SCHEMA, 'banding journal') then
    Exit;

  LogEvent(leaStart, Format('Import banding journal: %s', [aCSVFile]));
  stopProcess := False;

  // initialize progress bar or dialog
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;

  SurveyRepo := TSurveyRepository.Create(DMM.sqlCon);
  SiteRepo := TSiteRepository.Create(DMM.sqlCon);
  SPlotRepo := TSamplingPlotRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);

  try
    // CSV settings
    CSV.Delimiter := ';';
    CSV.FirstLineAsSchema := True;
    CSV.CodePage := 'Windows-1252';
    CSV.Schema.AddDelimitedText(BANDING_JOURNAL_SCHEMA, ';', True);
    CSV.FileName := aCSVFile;
    CSV.Open;
    LogInfo(Format('CSV file loaded with %d records.', [CSV.RecordCount]));

    UpdateProgress(0, CSV.RecordCount);

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      aMethod := GetMethodKey(rsMobileBanding);
      CSV.First;
      while not (CSV.Eof or stopProcess) do
      begin
        Reg.Clear;
        Reg.FromCSV(CSV);

        NetStation := TSamplingPlot.Create;
        Toponimo := TSite.Create;
        Survey := TSurvey.Create;
        OldSurvey := TSurvey.Create;
        try
          if (Reg.NetStation <> EmptyStr) then
          begin
            FStationId := GetSamplingPlotKey(Reg.NetStation);
            if (FStationId > 0) then
              SPlotRepo.GetById(FStationId, NetStation)
            else
              raise Exception.CreateFmt(rsErrorSamplingPlotNotFound, [Reg.NetStation]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingJournal.NetStation']);

          if (Reg.Locality <> EmptyStr) then
          begin
            FSiteId := GetSiteKey(Reg.Locality);
            if (FSiteId > 0) then
              SiteRepo.GetById(FSiteId, Toponimo)
            else
              raise Exception.CreateFmt(rsErrorToponymNotFound, [Reg.Locality]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingJournal.Locality']);

          SurveyRepo.FindBySiteAndDate(Toponimo.Id, aMethod, Reg.SamplingDate, '', NetStation.Id, Survey);
          if not Survey.IsNew then
          begin
            OldSurvey.Assign(Survey);

            case Options.ExistingRecordPolicy of
              erpIgnoreExisting: ;
              erpUpdateExisting:
              begin
                // fill Survey data
                Survey.SurveyDate := Reg.SamplingDate;
                Survey.StartTime := Reg.StartTime;
                Survey.EndTime := Reg.EndTime;
                Survey.MethodId := aMethod;
                Survey.NetStationId := NetStation.Id;
                Survey.LocalityId := Toponimo.Id;
                if ((xSettings.AutoFillCoordinates) and (Reg.Longitude = 0) and (Reg.Latitude = 0)) then
                begin
                  if ((NetStation.Longitude <> 0) and (NetStation.Latitude <> 0)) then
                  begin
                    Survey.StartLatitude := NetStation.Latitude;
                    Survey.StartLongitude := NetStation.Longitude;
                    Survey.CoordinatePrecision := cpApproximated;
                  end
                  else
                  if ((Toponimo.Longitude <> 0) and (Toponimo.Latitude <> 0)) then
                  begin
                    Survey.StartLatitude := Toponimo.Latitude;
                    Survey.StartLongitude := Toponimo.Longitude;
                    Survey.CoordinatePrecision := cpReference;
                  end;
                end
                else
                if ((Reg.Longitude <> 0) and (Reg.Latitude <> 0)) then
                begin
                  Survey.StartLatitude := Reg.Latitude;
                  Survey.StartLongitude := Reg.Longitude;
                  Survey.CoordinatePrecision := cpExact;
                end;
                Survey.Notes := Reg.Notes;

                SurveyRepo.Update(Survey);
                LogInfo(Format('Survey record with ID=%d updated', [Survey.Id]));

                WriteDiff(tbSurveys, OldSurvey, Survey, rsEditedByImport);
                InsertSurveyTeam(Reg.Team, Survey.Id);
              end;
            end;
          end
          else
          begin
            // fill Survey data
            Survey.SurveyDate := Reg.SamplingDate;
            Survey.StartTime := Reg.StartTime;
            Survey.EndTime := Reg.EndTime;
            Survey.MethodId := aMethod;
            Survey.NetStationId := NetStation.Id;
            Survey.LocalityId := Toponimo.Id;
            if ((xSettings.AutoFillCoordinates) and (Reg.Longitude = 0) and (Reg.Latitude = 0)) then
            begin
              if ((NetStation.Longitude <> 0) and (NetStation.Latitude <> 0)) then
              begin
                Survey.StartLatitude := NetStation.Latitude;
                Survey.StartLongitude := NetStation.Longitude;
                Survey.CoordinatePrecision := cpApproximated;
              end
              else
              if ((Toponimo.Longitude <> 0) and (Toponimo.Latitude <> 0)) then
              begin
                Survey.StartLatitude := Toponimo.Latitude;
                Survey.StartLongitude := Toponimo.Longitude;
                Survey.CoordinatePrecision := cpReference;
              end;
            end
            else
            if ((Reg.Longitude <> 0) and (Reg.Latitude <> 0)) then
            begin
              Survey.StartLatitude := Reg.Latitude;
              Survey.StartLongitude := Reg.Longitude;
              Survey.CoordinatePrecision := cpExact;
            end;
            Survey.Notes := Reg.Notes;

            SurveyRepo.Insert(Survey);
            LogInfo(Format('Survey record inserted with ID=%d', [Survey.Id]));

            if not Survey.IsNew then
            begin
              WriteRecHistory(tbSurveys, haCreated, Survey.Id, '', '', '', rsInsertedByImport);
              InsertSurveyTeam(Reg.Team, Survey.Id);
            end;
          end;
        finally
          NetStation.Free;
          Toponimo.Free;
          Survey.Free;
          OldSurvey.Free;
        end;

        UpdateProgress(CSV.RecNo, CSV.RecordCount);
        Application.ProcessMessages;
        CSV.Next;
      end;

      if stopProcess then
      begin
        DMM.sqlTrans.Rollback;
        LogWarning('Banding journal import canceled by user, transaction rolled back.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Banding journal import finished successfully, transaction committed.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsSuccessfulImportBandingJournal, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during banding journal import, transaction rolled back.');
      raise;
    end;
  finally
    CSV.Close;
    CSV.Free;
    SiteRepo.Free;
    SPlotRepo.Free;
    SurveyRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      dlgProgress.Free;
    end;
    LogEvent(leaFinish, 'Import banding journal');
  end;
end;

procedure ImportBandingEffortV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingEffort;
  Survey: TSurvey;
  SurveyRepo: TSurveyRepository;
  NetSite, OldNet: TNetEffort;
  NetRepo: TNetEffortRepository;
  aMethod, FSiteId, FStationId: Integer;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Banding effort import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  if not ValidateCSVSchema(aCSVFile, NET_EFFORT_SCHEMA, 'net effort') then
    Exit;

  LogEvent(leaStart, Format('Import banding effort: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  SurveyRepo := TSurveyRepository.Create(DMM.sqlCon);
  NetRepo := TNetEffortRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    with CSV do
    begin
      Delimiter := ';';
      FirstLineAsSchema := True;
      CodePage := 'Windows-1252';
      Schema.AddDelimitedText(NET_EFFORT_SCHEMA, ';', True);
      FileName := aCSVFile;
      Open;
    end;
    LogInfo(Format('CSV file loaded with %d records.', [CSV.RecordCount]));

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
      aMethod := GetMethodKey(rsMobileBanding);
      CSV.First;
      repeat
        if Assigned(dlgProgress) then
          dlgProgress.Text := Format(rsProgressRecords, [CSV.RecNo, CSV.RecordCount]);
        // Reset variables
        Reg.Clear;

        Reg.FromCSV(CSV);

        try
          Survey := TSurvey.Create;
          NetSite := TNetEffort.Create;
          OldNet := TNetEffort.Create;

          if (Reg.NetStation <> EmptyStr) then
          begin
            FStationId := GetSamplingPlotKey(Reg.NetStation);
            if (FStationId <= 0) then
              raise Exception.CreateFmt(rsErrorSamplingPlotNotFound, [Reg.NetStation]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingEffort.NetStation']);

          if (Reg.Locality <> EmptyStr) then
          begin
            FSiteId := GetSiteKey(Reg.Locality);
            if (FSiteId <= 0) then
              raise Exception.CreateFmt(rsErrorToponymNotFound, [Reg.Locality]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, ['TBandingEffort.Locality']);

          SurveyRepo.FindBySiteAndDate(FSiteId, aMethod, Reg.SamplingDate, '', FStationId, Survey);
          if Survey.IsNew then
            raise Exception.CreateFmt(rsErrorSurveyNotFound,
              [Format('SiteId=%d; MethodId=%d; SamplingDate=%s; NetStationId=%d',
                [FSiteId, aMethod, DateToStr(Reg.SamplingDate), FStationId])]);

          // Check if the net site exists
          NetRepo.FindBySurvey(Survey.Id, Reg.NetNumber, NetSite);
          if not (NetSite.IsNew) then
          begin
            OldNet.Assign(NetSite);

            case Options.ExistingRecordPolicy of
              erpIgnoreExisting: ;
              erpUpdateExisting:
              begin
                // replace existing net effort
                NetSite.SurveyId := Survey.Id;
                NetSite.NetStationId := FStationId;
                NetSite.SampleDate := Reg.SamplingDate;
                NetSite.NetNumber := StrToInt(Reg.NetNumber);
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

                NetRepo.Update(NetSite);
                LogInfo(Format('Net effort record with ID=%d updated', [NetSite.Id]));

                // Insert record history
                WriteDiff(tbNetsEffort, OldNet, NetSite, rsEditedByImport);
              end;
            end;
          end
          else
          begin
            // Insert new net effort
            NetSite.SurveyId := Survey.Id;
            NetSite.NetStationId := FStationId;
            NetSite.SampleDate := Reg.SamplingDate;
            NetSite.NetNumber := StrToInt(Reg.NetNumber);
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

            NetRepo.Insert(NetSite);
            LogInfo(Format('Net effort record inserted with ID=%d', [NetSite.Id]));

            // Insert record history
            WriteRecHistory(tbNetsEffort, haCreated, NetSite.Id, '', '', '', rsInsertedByImport);

          end;

        finally
          FreeAndNil(NetSite);
          FreeAndNil(OldNet);
          FreeAndNil(Survey);
        end;

        if Assigned(aProgressBar) then
          aProgressBar.Position := CSV.RecNo
        else
        if Assigned(dlgProgress) then
          dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      until CSV.Eof or stopProcess;

      if stopProcess then
      begin
        DMM.sqlTrans.Rollback;
        LogWarning('Banding effort import canceled by user, transaction rolled back.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Banding effort import finished successfully, transaction committed.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsSuccessfulImportBandingEffort, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during banding effort import, transaction rolled back.');
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    NetRepo.Free;
    SurveyRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Import banding effort')
  end;
end;

procedure ImportBandingWeatherLogV1(aCSVFile: String; Options: TImportOptions; aProgressBar: TProgressBar);

  procedure UpdateProgress(Current, Total: Integer);
  begin
    if Assigned(aProgressBar) then
    begin
      aProgressBar.Position := Current;
      aProgressBar.Max := Total;
    end
    else if Assigned(dlgProgress) then
    begin
      dlgProgress.Position := Current;
      dlgProgress.Max := Total;
      dlgProgress.Text := Format(rsProgressRecords, [Current, Total]);
    end;
  end;

var
  CSV: TSdfDataSet;
  Reg: TWeatherSample;
  Survey: TSurvey;
  aMethod, FSiteId, FStationId, FObserverId: Integer;
  SurveyRepo: TSurveyRepository;
  W, OldW: TWeatherLog;
  WeatherRepo: TWeatherLogRepository;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Banding weather log import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  if not ValidateCSVSchema(aCSVFile, WEATHER_LOG_SCHEMA, 'banding weather log') then
    Exit;

  LogEvent(leaStart, Format('Import banding weather log: %s', [aCSVFile]));
  stopProcess := False;

  // initialize progress bar or dialog
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;

  WeatherRepo := TWeatherLogRepository.Create(DMM.sqlCon);
  SurveyRepo := TSurveyRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);

  try
    // CSV settings
    CSV.Delimiter := ';';
    CSV.FirstLineAsSchema := True;
    CSV.CodePage := 'Windows-1252';
    CSV.Schema.AddDelimitedText(WEATHER_LOG_SCHEMA, ';', True);
    CSV.FileName := aCSVFile;
    CSV.Open;
    LogInfo(Format('CSV file loaded with %d records.', [CSV.RecordCount]));

    UpdateProgress(0, CSV.RecordCount);

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      aMethod := GetMethodKey(rsMobileBanding);
      CSV.First;
      while not (CSV.Eof or stopProcess) do
      begin
        Reg.Clear;
        Reg.FromCSV(CSV);

        Survey := TSurvey.Create;
        W := TWeatherLog.Create;
        OldW := TWeatherLog.Create;
        try
          if (Reg.NetStation <> EmptyStr) then
          begin
            FStationId := GetSamplingPlotKey(Reg.NetStation);
            if (FStationId <= 0) then
              raise Exception.CreateFmt(rsErrorSamplingPlotNotFound, [Reg.NetStation]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, ['TWeatherSample.NetStation']);

          if (Reg.Locality <> EmptyStr) then
          begin
            FSiteId := GetSiteKey(Reg.Locality);
            if (FSiteId <= 0) then
              raise Exception.CreateFmt(rsErrorToponymNotFound, [Reg.Locality]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, ['TWeatherSample.Locality']);

          SurveyRepo.FindBySiteAndDate(FSiteId, aMethod, Reg.SamplingDate, '', FStationId, Survey);
          if Survey.IsNew then
            raise Exception.CreateFmt(rsErrorSurveyNotFound,
              [Format('SiteId=%d; MethodId=%d; SamplingDate=%s; NetStationId=%d',
                [FSiteId, aMethod, DateToStr(Reg.SamplingDate), FStationId])]);

          if (Reg.Observer <> EmptyStr) then
          begin
            FObserverId := GetPersonKey(Reg.Observer);
            if (FObserverId <= 0) then
              raise Exception.CreateFmt(rsErrorObserverNotFound, [Reg.Observer]);
          end;

          WeatherRepo.FindBySurvey(Survey.Id, DateToStr(Reg.SamplingDate), TimeToStr(Reg.SamplingTime), FObserverId, W);
          if not (W.IsNew) then
          begin
            OldW.Assign(W);

            case Options.ExistingRecordPolicy of
              erpIgnoreExisting: ;
              erpUpdateExisting:
              begin
                // replace existing weather log
                W.SurveyId := Survey.Id;
                W.SampleDate := Reg.SamplingDate;
                W.SampleTime := Reg.SamplingTime;
                W.SampleMoment := Reg.SamplingMoment;
                W.ObserverId := FObserverId;
                W.Temperature := Reg.Temperature;
                W.Precipitation := Reg.Precipitation;
                W.CloudCover := Reg.CloudCover;
                W.WindSpeedBft := Reg.WindSpeed;
                W.RelativeHumidity := Reg.Humidity;
                W.AtmosphericPressure := Reg.AtmosphericPressure;

                WeatherRepo.Update(W);
                LogInfo(Format('Weather record with ID=%d updated', [W.Id]));

                WriteDiff(tbWeatherLogs, OldW, W, rsEditedByImport);
              end;
            end;
          end
          else
          begin
            // insert new weather log
            W.SurveyId := Survey.Id;
            W.SampleDate := Reg.SamplingDate;
            W.SampleTime := Reg.SamplingTime;
            W.SampleMoment := Reg.SamplingMoment;
            W.ObserverId := FObserverId;
            W.Temperature := Reg.Temperature;
            W.Precipitation := Reg.Precipitation;
            W.CloudCover := Reg.CloudCover;
            W.WindSpeedBft := Reg.WindSpeed;
            W.RelativeHumidity := Reg.Humidity;
            W.AtmosphericPressure := Reg.AtmosphericPressure;

            WeatherRepo.Insert(W);
            LogInfo(Format('Weather record inserted with ID=%d', [W.Id]));

            if not W.IsNew then
              WriteRecHistory(tbWeatherLogs, haCreated, W.Id, '', '', '', rsInsertedByImport);
          end;
        finally
          Survey.Free;
          W.Free;
          OldW.Free;
        end;

        UpdateProgress(CSV.RecNo, CSV.RecordCount);
        Application.ProcessMessages;
        CSV.Next;
      end;

      if stopProcess then
      begin
        DMM.sqlTrans.Rollback;
        LogWarning('Banding weather log import canceled by user, transaction rolled back.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
          dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Banding weather log import finished successfully, transaction committed.');
        if not Assigned(dlgProgress) then
          MsgDlg(rsTitleImportFile, rsSuccessfulImportBandingWeatherLog, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during banding weather log import, transaction rolled back.');
      raise;
    end;
  finally
    CSV.Close;
    CSV.Free;
    SurveyRepo.Free;
    WeatherRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      dlgProgress.Free;
    end;
    LogEvent(leaFinish, 'Import banding weather log');
  end;
end;

{ TBandingEffortHelper }

procedure TBandingEffortHelper.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := NullDate;
  NetNumber := EmptyStr;
  Longitude := 0.0;
  Latitude := 0.0;
  Notes := EmptyStr;

  NetBout1.OpenTime := NullTime;
  NetBout1.CloseTime := NullTime;

  NetBout2.OpenTime := NullTime;
  NetBout2.CloseTime := NullTime;

  NetBout3.OpenTime := NullTime;
  NetBout3.CloseTime := NullTime;

  NetBout4.OpenTime := NullTime;
  NetBout4.CloseTime := NullTime;
end;

procedure TBandingEffortHelper.FromCSV(CSV: TSdfDataSet);
var
  sDate: TDateTime;
begin
  { 0 - Locality }
  Locality := CSV.FieldByName('LOCALITY').AsString;
  { 1 - NetStation }
  NetStation := CSV.FieldByName('STATION').AsString;
  { 2 - SamplingDate }
  if not TryParseDateFlexible(CSV.FieldByName('DATE').AsString, sDate) then
    raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('DATE').AsString, 'TBandingEffort.SamplingDate'])
  else
    SamplingDate := sDate;
  { 3 - NetNumber }
  NetNumber := CSV.FieldByName('NET NUMBER').AsString;
  { 4 - Longitude }
  if (not CSV.FieldByName('LONGITUDE').IsNull) then
    Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
  { 5 - Latitude }
  if (not CSV.FieldByName('LATITUDE').IsNull) then
    Latitude := CSV.FieldByName('LATITUDE').AsFloat;
  { 6 - OpenTime 1 }
  if (not CSV.FieldByName('OPEN TIME 1').IsNull) then
    NetBout1.OpenTime := CSV.FieldByName('OPEN TIME 1').AsDateTime
  else
    NetBout1.OpenTime := NullTime;
  { 7 - CloseTime 1 }
  if (not CSV.FieldByName('CLOSE TIME 1').IsNull) then
    NetBout1.CloseTime := CSV.FieldByName('CLOSE TIME 1').AsDateTime
  else
    NetBout1.CloseTime := NullTime;
  { 8 - OpenTime 2 }
  if (not CSV.FieldByName('OPEN TIME 2').IsNull) then
    NetBout2.OpenTime := CSV.FieldByName('OPEN TIME 2').AsDateTime
  else
    NetBout2.OpenTime := NullTime;
  { 9 - CloseTime 2 }
  if (not CSV.FieldByName('CLOSE TIME 2').IsNull) then
    NetBout2.CloseTime := CSV.FieldByName('CLOSE TIME 2').AsDateTime
  else
    NetBout2.CloseTime := NullTime;
  { 10 - OpenTime 3 }
  if (not CSV.FieldByName('OPEN TIME 3').IsNull) then
    NetBout3.OpenTime := CSV.FieldByName('OPEN TIME 3').AsDateTime
  else
    NetBout3.OpenTime := NullTime;
  { 11 - CloseTime 3 }
  if (not CSV.FieldByName('CLOSE TIME 3').IsNull) then
    NetBout3.CloseTime := CSV.FieldByName('CLOSE TIME 3').AsDateTime
  else
    NetBout3.CloseTime := NullTime;
  { 12 - OpenTime 4 }
  if (not CSV.FieldByName('OPEN TIME 4').IsNull) then
    NetBout4.OpenTime := CSV.FieldByName('OPEN TIME 4').AsDateTime
  else
    NetBout4.OpenTime := NullTime;
  { 13 - CloseTime 4 }
  if (not CSV.FieldByName('CLOSE TIME 4').IsNull) then
    NetBout4.CloseTime := CSV.FieldByName('CLOSE TIME 4').AsDateTime
  else
    NetBout4.CloseTime := NullTime;
  { 14 - Notes }
  Notes := CSV.FieldByName('NOTES').AsString;
end;

{ TWeatherSampleHelper }

procedure TWeatherSampleHelper.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := NullDate;
  SamplingTime := NullTime;
  SamplingMoment := wmNone;
  Observer := EmptyStr;
  CloudCover := -1;
  Precipitation := wpEmpty;
  Temperature := 0;
  WindSpeed := 0;
  Humidity := 0;
  AtmosphericPressure := 0;
end;

procedure TWeatherSampleHelper.FromCSV(CSV: TSdfDataSet);
var
  sDate: TDateTime;
begin
  { 0 - Locality }
  Locality := CSV.FieldByName('LOCALITY').AsString;
  { 1 - NetStation }
  NetStation := CSV.FieldByName('STATION').AsString;
  { 2 - SamplingDate }
  if not TryParseDateFlexible(CSV.FieldByName('DATE').AsString, sDate) then
    raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('DATE').AsString, 'TWeatherSample.SamplingDate'])
  else
    SamplingDate := sDate;
  { 3 - SamplingTime }
  if (not CSV.FieldByName('TIME').IsNull) then
    SamplingTime := CSV.FieldByName('TIME').AsDateTime;
  { 4 - Sampling Moment }
  if (not CSV.FieldByName('MOMENT').IsNull) then
    SamplingMoment := StrToSampleMoment(CSV.FieldByName('MOMENT').AsString);
  { 5 - Observer }
  Observer := CSV.FieldByName('OBSERVER').AsString;
  { 6 - Cloud Cover }
  if (not CSV.FieldByName('CLOUD COVER').IsNull) then
    CloudCover := CSV.FieldByName('CLOUD COVER').AsInteger;
  { 7 - Precipitation }
  if (not CSV.FieldByName('PRECIPITATION').IsNull) then
    Precipitation := StrToPrecipitation(CSV.FieldByName('PRECIPITATION').AsString);
  { 8 - Temperature }
  if (not CSV.FieldByName('TEMPERATURE').IsNull) then
    Temperature := CSV.FieldByName('TEMPERATURE').AsFloat;
  { 9 - Wind Speed }
  if (not CSV.FieldByName('WIND SPEED').IsNull) then
    WindSpeed := CSV.FieldByName('WIND SPEED').AsInteger;
  { 10 - Humidity }
  if (not CSV.FieldByName('HUMIDITY').IsNull) then
    Humidity := CSV.FieldByName('HUMIDITY').AsFloat;
  { 11 - Atmospheric Pressure }
  if (not CSV.FieldByName('ATM PRESSURE').IsNull) then
    AtmosphericPressure := CSV.FieldByName('ATM PRESSURE').AsFloat;
end;

{ TBandingJournalHelper }

procedure TBandingJournalHelper.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := NullDate;
  StartTime := NullTime;
  EndTime := NullTime;
  Longitude := 0.0;
  Latitude := 0.0;
  TotalNets := 0;
  Team := EmptyStr;
  Notes := EmptyStr;
  NetCheckingBouts := EmptyStr;
end;

procedure TBandingJournalHelper.FromCSV(CSV: TSdfDataSet);
var
  sDate: TDateTime;
begin
  { 0 - Locality }
  Locality := CSV.FieldByName('LOCALITY').AsString;
  { 1 - NetStation }
  NetStation := CSV.FieldByName('STATION').AsString;
  { 2 - SamplingDate }
  if not TryParseDateFlexible(CSV.FieldByName('DATE').AsString, sDate) then
    raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('DATE').AsString, 'TBandingJournal.SamplingDate'])
  else
    SamplingDate := sDate;
  { 3 - StartTime }
  if (not CSV.FieldByName('START TIME').IsNull) then
    StartTime := CSV.FieldByName('START TIME').AsDateTime;
  { 4 - EndTime }
  if (not CSV.FieldByName('END TIME').IsNull) then
    EndTime := CSV.FieldByName('END TIME').AsDateTime;
  { 5 - Longitude }
  if (not CSV.FieldByName('LONGITUDE').IsNull) then
    Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
  { 6 - Latitude }
  if (not CSV.FieldByName('LATITUDE').IsNull) then
    Latitude := CSV.FieldByName('LATITUDE').AsFloat;
  { 7 - Total Nets }
  if (not CSV.FieldByName('TOTAL NETS').IsNull) then
    TotalNets := CSV.FieldByName('TOTAL NETS').AsInteger;
  { 8 - Team }
  Team := CSV.FieldByName('TEAM').AsString;
  { 9 - Notes }
  Notes := CSV.FieldByName('NOTES').AsString;
  { 10 - Net Checking Bouts }
  NetCheckingBouts := CSV.FieldByName('NET CHECKING BOUTS').AsString;
end;

{ TBandingDataHelper }

procedure TBandingDataHelper.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  CaptureMonth := 0;
  CaptureDay := 0;
  CaptureDate := NullDate;
  Recorder := EmptyStr;
  Bander := EmptyStr;
  CaptureTime := NullTime;
  NetSiteName := EmptyStr;
  CaptureType := EmptyStr;
  BandSize := EmptyStr;
  BandNumber := 0;
  RightTarsus := EmptyStr;
  LeftTarsus := EmptyStr;
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
  InitialPhotoNumber := 0;
  FinalPhotoNumber := 0;
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

procedure TBandingDataHelper.FromCSV(CSV: TSdfDataSet);
var
  sDate: TDateTime;
begin
  Locality := CSV.FieldByName('LOCALITY').AsString;
  NetStation := CSV.FieldByName('STATION').AsString;
  if (not CSV.FieldByName('DATE').IsNull) then
    if not TryParseDateFlexible(CSV.FieldByName('DATE').AsString, sDate) then
      raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('DATE').AsString, 'TBandingData.CaptureDate'])
    else
      CaptureDate := sDate;
  Recorder := AnsiUpperCase(CSV.FieldByName('RECORDER').AsString);
  Bander := AnsiUpperCase(CSV.FieldByName('BANDER').AsString);
  if (not CSV.FieldByName('CAP TIME').IsNull) then
    CaptureTime := CSV.FieldByName('CAP TIME').AsDateTime
  else
    CaptureTime := NullTime;
  if (CSV.FieldByName('NET SITE NAME').AsString = '') then
    NetSiteName := '0'
  else
    NetSiteName := CSV.FieldByName('NET SITE NAME').AsString;
  CaptureType := AnsiUpperCase(CSV.FieldByName('TYPE').AsString);
  if (CaptureType <> 'U') then
    BandSize := AnsiUpperCase(CSV.FieldByName('BAND CODE').AsString);
  if (BandSize <> '') and (CaptureType <> 'U') then
    BandNumber := CSV.FieldByName('BAND NUMBER').AsInteger;
  RightTarsus := AnsiUpperCase(CSV.FieldByName('RIGHT TARSUS').AsString);
  LeftTarsus := AnsiUpperCase(CSV.FieldByName('LEFT TARSUS').AsString);
  SpeciesName := CSV.FieldByName('SPECIES NAME').AsString;
  CloacalProtuberance := AnsiUpperCase(CSV.FieldByName('CP').AsString);
  BroodPatch := AnsiUpperCase(CSV.FieldByName('BP').AsString);
  Fat := AnsiUpperCase(CSV.FieldByName('FAT').AsString);
  BodyMolt := AnsiUpperCase(CSV.FieldByName('BODY MOLT').AsString);
  FlightFeathersMolt := AnsiUpperCase(CSV.FieldByName('FF MOLT').AsString);
  FlightFeathersWear := AnsiUpperCase(CSV.FieldByName('FF WEAR').AsString);
  if (not CSV.FieldByName('RIGHT WING').IsNull) then
    RightWingChord := CSV.FieldByName('RIGHT WING').AsFloat;
  if (not CSV.FieldByName('FIRST SECONDARY').IsNull) then
    FirstSecondaryChord := CSV.FieldByName('FIRST SECONDARY').AsFloat;
  if (not CSV.FieldByName('TAIL').IsNull) then
    TailLength := CSV.FieldByName('TAIL').AsFloat;
  if (not CSV.FieldByName('TARSUS LENGTH').IsNull) then
    TarsusLength := CSV.FieldByName('TARSUS LENGTH').AsFloat;
  if (not CSV.FieldByName('RIGHT TARSUS DIAMETER').IsNull) then
    RightTarsusDiameter := CSV.FieldByName('RIGHT TARSUS DIAMETER').AsFloat;
  if (not CSV.FieldByName('WEIGHT').IsNull) then
    Weight := CSV.FieldByName('WEIGHT').AsFloat;
  MoltLimits := AnsiUpperCase(CSV.FieldByName('MOLT LIMITS').AsString);
  SkullOssification := AnsiUpperCase(CSV.FieldByName('SKULL').AsString);
  CycleCode := AnsiUpperCase(CSV.FieldByName('CYCLE CODE').AsString);
  HowAged := AnsiUpperCase(CSV.FieldByName('HOW AGED').AsString);
  Sex := AnsiUpperCase(CSV.FieldByName('SEX').AsString);
  HowSexed := AnsiUpperCase(CSV.FieldByName('HOW SEXED').AsString);
  SubjectStatus := AnsiUpperCase(CSV.FieldByName('STATUS').AsString);
  if (not CSV.FieldByName('ESCAPED').IsNull) then
    Escaped := CSV.FieldByName('ESCAPED').AsBoolean;
  Notes := CSV.FieldByName('NOTES').AsString;
  RemovedBand := CSV.FieldByName('REMOVED BAND').AsString;
  Photographer1 := AnsiUpperCase(CSV.FieldByName('PHOTOGRAPHER').AsString);
  if Pos('/', Photographer1) > 0 then
  begin
    Photographer2 := Trim(ExtractWord(2, Photographer1, ['/']));
    Photographer1 := Trim(ExtractWord(1, Photographer1, ['/']));
  end;
  if (not CSV.FieldByName('INITIAL PHOTO NUMBER').IsNull) then
    InitialPhotoNumber := CSV.FieldByName('INITIAL PHOTO NUMBER').AsInteger;
  if (not CSV.FieldByName('FINAL PHOTO NUMBER').IsNull) then
    FinalPhotoNumber := CSV.FieldByName('FINAL PHOTO NUMBER').AsInteger;
  CameraName := CSV.FieldByName('CAMERA NAME').AsString;
  PhotoNameFormula := CSV.FieldByName('PHOTO NAME FORMULA').AsString;
  if (not CSV.FieldByName('SKULL LENGTH').IsNull) then
    SkullLength := CSV.FieldByName('SKULL LENGTH').AsFloat;
  if (not CSV.FieldByName('EXPOSED CULMEN').IsNull) then
    ExposedCulmen := CSV.FieldByName('EXPOSED CULMEN').AsFloat;
  if (not CSV.FieldByName('NP').IsNull) then
    NostrilBillTip := CSV.FieldByName('NP').AsFloat;
  if (not CSV.FieldByName('BILL WIDTH').IsNull) then
    BillWidth := CSV.FieldByName('BILL WIDTH').AsFloat;
  if (not CSV.FieldByName('BILL HEIGHT').IsNull) then
    BillHeight := CSV.FieldByName('BILL HEIGHT').AsFloat;
  if (not CSV.FieldByName('BLOOD SAMPLE').IsNull) then
    BloodSample := CSV.FieldByName('BLOOD SAMPLE').AsBoolean;
  if (not CSV.FieldByName('FEATHER SAMPLE').IsNull) then
    FeatherSample := CSV.FieldByName('FEATHER SAMPLE').AsBoolean;
  if (not CSV.FieldByName('LONGITUDE').IsNull) then
    Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
  if (not CSV.FieldByName('LATITUDE').IsNull) then
    Latitude := CSV.FieldByName('LATITUDE').AsFloat;
  if (not CSV.FieldByName('KIPPS').IsNull) then
    KippsIndex := CSV.FieldByName('KIPPS').AsFloat;
  if (not CSV.FieldByName('GLUCOSE').IsNull) then
    Glucose := CSV.FieldByName('GLUCOSE').AsFloat;
  if (not CSV.FieldByName('HEMOGLOBIN').IsNull) then
    Hemoglobin := CSV.FieldByName('HEMOGLOBIN').AsFloat;
  if (not CSV.FieldByName('HEMATOCRIT').IsNull) then
    Hematocrit := CSV.FieldByName('HEMATOCRIT').AsFloat;
  GPSNumber := CSV.FieldByName('GPS NUMBER').AsString;
end;

end.

