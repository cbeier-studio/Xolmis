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
  Classes, SysUtils, Forms, Dialogs, StrUtils, ComCtrls, DateUtils,
  DB, SQLDB, SdfData, fpjson, jsonparser,
  models_sampling, models_record_types;

const
  BANDING_JOURNAL_SCHEMA: String = 'LOCALITY;STATION;DATE;START TIME;END TIME;LONGITUDE;LATITUDE;' +
    'TEAM;NOTES;WEATHER TIME 1;WEATHER MOMENT 1;CLOUD COVER 1;PRECIPITATION 1;TEMPERATURE 1;WIND SPEED 1;HUMIDITY 1;' +
    'WEATHER TIME 2;WEATHER MOMENT 2;CLOUD COVER 2;PRECIPITATION 2;TEMPERATURE 2;WIND SPEED 2;HUMIDITY 2;' +
    'WEATHER TIME 3;WEATHER MOMENT 3;CLOUD COVER 3;PRECIPITATION 3;TEMPERATURE 3;WIND SPEED 3;HUMIDITY 3;' +
    'WEATHER TIME 4;WEATHER MOMENT 4;CLOUD COVER 4;PRECIPITATION 4;TEMPERATURE 4;WIND SPEED 4;HUMIDITY 4';

  WEATHER_LOG_SCHEMA: String = 'TIME;MOMENT;CLOUD COVER;PRECIPITATION;TEMPERATURE;WIND SPEED;HUMIDITY';

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
    SamplingTime: TTime;
    SamplingMoment: TWeatherSampleMoment;
    CloudCover: Integer;
    Precipitation: TPrecipitation;
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
    NetNumber: Integer;
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
  procedure ImportBandingDataV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportBandingJournalV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportBandingEffortV1(aCSVFile: String; aProgressBar: TProgressBar = nil);

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_system, utils_validations, utils_conversions,
  data_types, data_getvalue, data_consts, data_services,
  models_users, models_taxonomy, models_birds, models_geo, models_bands,
  models_sampling_plots, io_csv,
  udm_main, udlg_progress;

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

procedure ImportBandingDataV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingData;
  TaxonRepo: TTaxonRepository;
  Taxon: TTaxon;
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
  strDate, strTime: String;
  CodAnilha, RemAnilha, aMethod: Integer;
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
        CodAnilha := 0;
        RemAnilha := 0;
        NetLat := 500.0;
        NetLong := 500.0;
        strDate := '';

        // Load the record data
        Reg.FromCSV(CSV);

        // If it is a capture record (including recapture and band change)
        if (Trim(Reg.SpeciesName) <> EmptyStr) then
        begin
          strDate := FormatDateTime(MASK_ISO_DATE, Reg.CaptureDate);
          strTime := FormatDateTime(MASK_DISPLAY_TIME, Reg.CaptureTime);
          TaxonRepo := TTaxonRepository.Create(DMM.sqlCon);
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
            Taxon := TTaxon.Create();
            TaxonRepo.GetById(GetValidTaxon(Reg.SpeciesName), Taxon);
            NetStation := TSamplingPlot.Create;
            Toponimo := TSite.Create;
            NetSite := TNetEffort.Create;
            Survey := TSurvey.Create;
            Band := TBand.Create;
            RemovedBand := TBand.Create;
            Individuo := TIndividual.Create;
            Captura := TCapture.Create;
            OldCaptura := TCapture.Create;
            aMethod := GetMethodKey(rsMobileBanding);

            // Get valid taxon
            TaxonRepo.GetById(GetValidTaxon(Reg.SpeciesName), Taxon);

            // Get toponym
            SiteRepo.GetById(GetSiteKey(Reg.Locality), Toponimo);

            // Get net station and locality
            SPlotRepo.FindBy(COL_ABBREVIATION, Reg.NetStation, NetStation);
            if not (NetStation.IsNew) and (Toponimo.IsNew) then
            begin
              SiteRepo.GetById(NetStation.LocalityId, Toponimo);
            end;

            // Get survey
            SurveyRepo.FindBySiteAndDate(Toponimo.Id, aMethod, Reg.CaptureDate, '', NetStation.Id, Survey);

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
              RemAnilha := GetBandKey(Reg.RemovedBand);
              if RemAnilha > 0 then
                BandRepo.GetById(RemAnilha, RemovedBand);
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
              CodAnilha := RemovedBand.Id
            else
              CodAnilha := Band.Id;

            IndividualRepo.FindByBand(Taxon.Id, CodAnilha, Reg.RightTarsus, Reg.LeftTarsus, Individuo);
            if (Individuo.IsNew) then
            begin
              // If does not exist, insert the individual
              Individuo.TaxonId := Taxon.Id;
              Individuo.BandId := CodAnilha;
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
            CaptureRepo.FindByBand(Taxon.Id, CodAnilha, Reg.CaptureType, strDate, strTime, Captura);
            if (Captura.IsNew) then
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
              Captura.UserInserted := ActiveUser.Id;

              CaptureRepo.Insert(Captura);
              // Insert record history
              WriteRecHistory(tbCaptures, haCreated, Captura.Id, '', '', '', rsInsertedByImport);
              LogInfo(Format('Capture record inserted with ID=%d', [Captura.Id]));
            end
            else
            begin
              // If exists, update the record
              CaptureRepo.GetById(Captura.Id, OldCaptura);

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
              Captura.CycleCode := Reg.CycleCode;
              Captura.Notes := Reg.Notes;
              Captura.UserUpdated := ActiveUser.Id;

              CaptureRepo.Update(Captura);

              // Insert record history
              WriteDiff(tbCaptures, OldCaptura, Captura, rsEditedByImport);
              LogInfo(Format('Capture record with ID=%d updated', [Captura.Id]));
            end;

            // Update band status
            if (Trim(Reg.RemovedBand) <> '') then
            begin
              MoveBand.RemoveFromIndividual(RemovedBand, Individuo.Id, Reg.CaptureDate);
              //UpdateBand(RemovedBand.Id, Individuo.Id, 'R', Reg.CaptureDate);
              LogInfo(Format('Band ID=%d status updated to removed', [RemovedBand.Id]));
            end;
            MoveBand.UseInCapture(Band, Individuo.Id, Reg.CaptureDate);
            //UpdateBand(Band.Id, Individuo.Id, 'U', Reg.CaptureDate);
            LogInfo(Format('Band ID=%d status updated to used', [Band.Id]));

            // Update individual band
            if Reg.CaptureType = 'N' then
            begin
              UpdInd.ApplyCaptureToIndividual(Captura);
              //UpdateIndividual(Individuo.Id, Reg.CaptureDate);
              LogInfo(Format('Individual ID=%d banding date updated', [Individuo.Id]));
            end;
            if Reg.CaptureType = 'C' then
            begin
              UpdInd.ApplyBandRemoval(Captura);
              //ChangeIndividualBand(Individuo.Id, Band.Id, RemovedBand.Id, Reg.CaptureDate,
              //  Reg.RemovedBand);
              LogInfo(Format('Individual ID=%d band updated with ID=%d (removed band ID=%d)',
                [Individuo.Id, Band.Id, RemovedBand.Id]));
            end;
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
            FreeAndNil(OldCaptura);
            UpdInd.Free;
            MoveBand.Free;
            BandRepo.Free;
            SiteRepo.Free;
            SPlotRepo.Free;
            CaptureRepo.Free;
            IndividualRepo.Free;
            TaxonRepo.Free;
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
            if (Reg.CaptureType = 'Q') then    // Broken band
            begin
              MoveBand.MarkAsBroken(Band, Reg.CaptureDate);
              //UpdateBand(Band.Id, 0, 'Q', Reg.CaptureDate);
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

procedure ImportBandingJournalV1(aCSVFile: String; aProgressBar: TProgressBar);

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

  procedure InsertWeatherLog(const RegWeather: TWeatherSample; SurveyId: Integer; aSampleDate: TDateTime);
  var
    W: TWeatherLog;
    WeatherRepo: TWeatherLogRepository;
  begin
    if (RegWeather.Precipitation <> wpEmpty) or (RegWeather.CloudCover >= 0) then
    begin
      WeatherRepo := TWeatherLogRepository.Create(DMM.sqlCon);
      W := TWeatherLog.Create;
      try
        W.SurveyId := SurveyId;
        W.SampleDate := aSampleDate;
        W.SampleTime := RegWeather.SamplingTime;
        W.SampleMoment := RegWeather.SamplingMoment;
        W.Temperature := RegWeather.Temperature;
        W.Precipitation := RegWeather.Precipitation;
        W.CloudCover := RegWeather.CloudCover;
        W.WindSpeedBft := RegWeather.WindSpeed;
        W.RelativeHumidity := RegWeather.Humidity;

        WeatherRepo.Insert(W);
        LogInfo(Format('Weather record inserted with ID=%d', [W.Id]));

        WriteRecHistory(tbWeatherLogs, haCreated, W.Id, '', '', '', rsInsertedByImport);
      finally
        W.Free;
        WeatherRepo.Free;
      end;
    end;
  end;

  procedure InsertSurveyTeam(const TeamStr: String; SurveyId: Integer);
  var
    Member: TSurveyMember;
    i: Integer;
    MemberRepo: TSurveyMemberRepository;
  begin
    if TeamStr = '' then
      Exit;

    MemberRepo := TSurveyMemberRepository.Create(DMM.sqlCon);
    Member := TSurveyMember.Create;
    try
      for i := 1 to WordCount(TeamStr, [',',';']) do
      begin
        Member.SurveyId := SurveyId;
        Member.PersonId := GetPersonKey(ExtractWord(i, TeamStr, [',',';']));
        MemberRepo.Insert(Member);
        LogInfo(Format('Survey member record inserted with ID=%d', [Member.Id]));

        WriteRecHistory(tbSurveyTeams, haCreated, Member.Id, '', '', '', rsInsertedByImport);
      end;
    finally
      Member.Free;
      MemberRepo.Free;
    end;
  end;

var
  CSV: TSdfDataSet;
  Reg: TBandingJournal;
  NetStation: TSamplingPlot;
  Toponimo: TSite;
  Survey: TSurvey;
  aMethod: Integer;
  SurveyRepo: TSurveyRepository;
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
        try
          SPlotRepo.FindBy(COL_ABBREVIATION, Reg.NetStation, NetStation);
          if NetStation.Id > 0 then
            SiteRepo.GetById(NetStation.LocalityId, Toponimo);

          SurveyRepo.FindBySiteAndDate(Toponimo.Id, aMethod, Reg.SamplingDate, '', NetStation.Id, Survey);
          if Survey.IsNew then
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

              // insert weather logs
              InsertWeatherLog(Reg.Weather1, Survey.Id, Reg.SamplingDate);
              InsertWeatherLog(Reg.Weather2, Survey.Id, Reg.SamplingDate);
              InsertWeatherLog(Reg.Weather3, Survey.Id, Reg.SamplingDate);
              InsertWeatherLog(Reg.Weather4, Survey.Id, Reg.SamplingDate);
            end;
          end;
        finally
          NetStation.Free;
          Toponimo.Free;
          Survey.Free;
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

procedure ImportBandingEffortV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingEffort;
  SiteRepo: TSiteRepository;
  Toponimo: TSite;
  NetStation: TSamplingPlot;
  SPlotRepo: TSamplingPlotRepository;
  Survey: TSurvey;
  SurveyRepo: TSurveyRepository;
  NetSite: TNetEffort;
  NetRepo: TNetEffortRepository;
  aMethod: Integer;
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
  SiteRepo := TSiteRepository.Create(DMM.sqlCon);
  SPlotRepo := TSamplingPlotRepository.Create(DMM.sqlCon);
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
          NetStation := TSamplingPlot.Create;
          Toponimo := TSite.Create;
          Survey := TSurvey.Create;
          NetSite := TNetEffort.Create;

          // Get net station and locality
          SPlotRepo.FindBy(COL_ABBREVIATION, Reg.NetStation, NetStation);
          if NetStation.Id > 0 then
          begin
            SiteRepo.GetById(NetStation.LocalityId, Toponimo);
          end;

          // Check if the survey exists
          SurveyRepo.FindBySiteAndDate(Toponimo.Id, aMethod, Reg.SamplingDate, '', NetStation.Id, Survey);
          if not (Survey.IsNew) then
          begin
            // Check if the net site exists
            NetRepo.FindBySurvey(Survey.Id, IntToStr(Reg.NetNumber), NetSite);
            if (NetSite.IsNew) then
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

              NetRepo.Insert(NetSite);
              LogInfo(Format('Net effort record inserted with ID=%d', [NetSite.Id]));

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
    SiteRepo.Free;
    SPlotRepo.Free;
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

{ TBandingEffortHelper }

procedure TBandingEffortHelper.Clear;
begin
  Locality := EmptyStr;
  NetStation := EmptyStr;
  SamplingDate := NullDate;
  NetNumber := 0;
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
  NetNumber := CSV.FieldByName('NET NUMBER').AsInteger;
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
  Team := EmptyStr;
  Notes := EmptyStr;

  Weather1.SamplingTime := NullTime;
  Weather1.SamplingMoment := wmNone;
  Weather1.CloudCover := -1;
  Weather1.Precipitation := wpEmpty;
  Weather1.Temperature := 0.0;
  Weather1.WindSpeed := 0;
  Weather1.Humidity := 0.0;

  Weather2.SamplingTime := NullTime;
  Weather2.SamplingMoment := wmNone;
  Weather2.CloudCover := -1;
  Weather2.Precipitation := wpEmpty;
  Weather2.Temperature := 0.0;
  Weather2.WindSpeed := 0;
  Weather2.Humidity := 0.0;

  Weather3.SamplingTime := NullTime;
  Weather3.SamplingMoment := wmNone;
  Weather3.CloudCover := -1;
  Weather3.Precipitation := wpEmpty;
  Weather3.Temperature := 0.0;
  Weather3.WindSpeed := 0;
  Weather3.Humidity := 0.0;

  Weather4.SamplingTime := NullTime;
  Weather4.SamplingMoment := wmNone;
  Weather4.CloudCover := -1;
  Weather4.Precipitation := wpEmpty;
  Weather4.Temperature := 0.0;
  Weather4.WindSpeed := 0;
  Weather4.Humidity := 0.0;
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
  { 7 - Team }
  Team := CSV.FieldByName('TEAM').AsString;
  { 8 - Notes }
  Notes := CSV.FieldByName('NOTES').AsString;
  { 9 - Weather Time 1 }
  if (not CSV.FieldByName('WEATHER TIME 1').IsNull) then
    Weather1.SamplingTime := CSV.FieldByName('WEATHER TIME 1').AsDateTime;
  { 10 - Weather Moment 1 }
  if (not CSV.FieldByName('WEATHER MOMENT 1').IsNull) then
    Weather1.SamplingMoment := StrToSampleMoment(CSV.FieldByName('WEATHER MOMENT 1').AsString);
  { 11 - Cloud Cover 1 }
  if (not CSV.FieldByName('CLOUD COVER 1').IsNull) then
    Weather1.CloudCover := CSV.FieldByName('CLOUD COVER 1').AsInteger;
  { 12 - Precipitation 1 }
  if (not CSV.FieldByName('PRECIPITATION 1').IsNull) then
    Weather1.Precipitation := StrToPrecipitation(CSV.FieldByName('PRECIPITATION 1').AsString);
  { 13 - Temperature 1 }
  if (not CSV.FieldByName('TEMPERATURE 1').IsNull) then
    Weather1.Temperature := CSV.FieldByName('TEMPERATURE 1').AsFloat;
  { 14 - Wind Speed 1 }
  if (not CSV.FieldByName('WIND SPEED 1').IsNull) then
    Weather1.WindSpeed := CSV.FieldByName('WIND SPEED 1').AsInteger;
  { 15 - Humidity 1 }
  if (not CSV.FieldByName('HUMIDITY 1').IsNull) then
    Weather1.Humidity := CSV.FieldByName('HUMIDITY 1').AsFloat;
  { 16 - Weather Time 2 }
  if (not CSV.FieldByName('WEATHER TIME 2').IsNull) then
    Weather2.SamplingTime := CSV.FieldByName('WEATHER TIME 2').AsDateTime;
  { 17 - Weather Moment 2 }
  if (not CSV.FieldByName('WEATHER MOMENT 2').IsNull) then
    Weather2.SamplingMoment := StrToSampleMoment(CSV.FieldByName('WEATHER MOMENT 2').AsString);
  { 18 - Cloud Cover 2 }
  if (not CSV.FieldByName('CLOUD COVER 2').IsNull) then
    Weather2.CloudCover := CSV.FieldByName('CLOUD COVER 2').AsInteger;
  { 19 - Precipitation 2 }
  if (not CSV.FieldByName('PRECIPITATION 2').IsNull) then
    Weather2.Precipitation := StrToPrecipitation(CSV.FieldByName('PRECIPITATION 2').AsString);
  { 20 -Temperature 2 }
  if (not CSV.FieldByName('TEMPERATURE 2').IsNull) then
    Weather2.Temperature := CSV.FieldByName('TEMPERATURE 2').AsFloat;
  { 21 - Wind Speed 2 }
  if (not CSV.FieldByName('WIND SPEED 2').IsNull) then
    Weather2.WindSpeed := CSV.FieldByName('WIND SPEED 2').AsInteger;
  { 22 - Humidity 2 }
  if (not CSV.FieldByName('HUMIDITY 2').IsNull) then
    Weather2.Humidity := CSV.FieldByName('HUMIDITY 2').AsFloat;
  { 23 - Weather Time 3 }
  if (not CSV.FieldByName('WEATHER TIME 3').IsNull) then
    Weather3.SamplingTime := CSV.FieldByName('WEATHER TIME 3').AsDateTime;
  { 24 - Weather Moment 3 }
  if (not CSV.FieldByName('WEATHER MOMENT 3').IsNull) then
    Weather3.SamplingMoment := StrToSampleMoment(CSV.FieldByName('WEATHER MOMENT 3').AsString);
  { 25 - Cloud Cover 3 }
  if (not CSV.FieldByName('CLOUD COVER 3').IsNull) then
    Weather3.CloudCover := CSV.FieldByName('CLOUD COVER 3').AsInteger;
  { 26 - Precipitation 3 }
  if (not CSV.FieldByName('PRECIPITATION 3').IsNull) then
    Weather3.Precipitation := StrToPrecipitation(CSV.FieldByName('PRECIPITATION 3').AsString);
  { 27 - Temperature 3 }
  if (not CSV.FieldByName('TEMPERATURE 3').IsNull) then
    Weather3.Temperature := CSV.FieldByName('TEMPERATURE 3').AsFloat;
  { 28 - Wind Speed 3 }
  if (not CSV.FieldByName('WIND SPEED 3').IsNull) then
    Weather3.WindSpeed := CSV.FieldByName('WIND SPEED 3').AsInteger;
  { 29 - Humidity 3 }
  if (not CSV.FieldByName('HUMIDITY 3').IsNull) then
    Weather3.Humidity := CSV.FieldByName('HUMIDITY 3').AsFloat;
  { 30 - Weather Time 4 }
  if (not CSV.FieldByName('WEATHER TIME 4').IsNull) then
    Weather4.SamplingTime := CSV.FieldByName('WEATHER TIME 4').AsDateTime;
  { 31 - Weather Moment 4 }
  if (not CSV.FieldByName('WEATHER MOMENT 4').IsNull) then
    Weather4.SamplingMoment := StrToSampleMoment(CSV.FieldByName('WEATHER MOMENT 4').AsString);
  { 32 - Cloud Cover 4 }
  if (not CSV.FieldByName('CLOUD COVER 4').IsNull) then
    Weather4.CloudCover := CSV.FieldByName('CLOUD COVER 4').AsInteger;
  { 33 - Precipitation 4 }
  if (not CSV.FieldByName('PRECIPITATION 4').IsNull) then
    Weather4.Precipitation := StrToPrecipitation(CSV.FieldByName('PRECIPITATION 4').AsString);
  { 34 - Temperature 4 }
  if (not CSV.FieldByName('TEMPERATURE 4').IsNull) then
    Weather4.Temperature := CSV.FieldByName('TEMPERATURE 4').AsFloat;
  { 35 - Wind Speed 4 }
  if (not CSV.FieldByName('WIND SPEED 4').IsNull) then
    Weather4.WindSpeed := CSV.FieldByName('WIND SPEED 4').AsInteger;
  { 36 - Humidity 4 }
  if (not CSV.FieldByName('HUMIDITY 4').IsNull) then
    Weather4.Humidity := CSV.FieldByName('HUMIDITY 4').AsFloat;
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

