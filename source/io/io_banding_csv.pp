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
  BANDING_JOURNAL_SCHEMA: String = 'LOCALITY;NET STATION;SAMPLING DATE;START TIME;END TIME;LONGITUDE;LATITUDE;' +
    'TEAM;NOTES';

  WEATHER_LOG_SCHEMA: String = 'TIME;MOMENT;CLOUD COVER;PRECIPITATION;TEMPERATURE;WIND SPEED;HUMIDITY';

  NET_EFFORT_SCHEMA: String = 'LOCALITY;NET STATION;SAMPLING DATE;NET NUMBER;LONGITUDE;LATITUDE;' +
    'OPEN TIME 1;CLOSE TIME 1;OPEN TIME 2;CLOSE TIME 2;OPEN TIME 3;CLOSE TIME 3;OPEN TIME 4;CLOSE TIME 4;NOTES';

  BANDING_SCHEMA: String = 'LOCALITY;STATION;DATA;RECORDER;BANDER;CAP TIME;NET SITE NAME;NEW_RECAP;' +
    'BAND_CODE;BAND NUMBER;RIGHT LEG;LEFT LEG;SPECIES NAME;CP;BP;FAT;BODY MOLT;FF MOLT;FF WEAR;' +
    'RIGHT WING;FIRST SECONDARY;TAIL;TARSUS LENGTH;RIGHT TARSUS DIAMETER;WEIGHT;' +
    'MOLT LIMITS;SKULL;CYCLE CODE;HOW AGED;SEX;HOW SEXED;STATUS;ESCAPED;NOTES;' +
    'REMOVED BAND;PHOTOGRAPHER;INITIAL PHOTO NUMBER;FINAL PHOTO NUMBER;CAMERA NAME;PHOTO NAME FORMULA;' +
    'CRANIO;CULMEN EXPOSTO;NP;LARGURA BICO;ALTURA BICO;SANGUE;PENAS;LONGITUDE;LATITUDE;' +
    'KIPPS;GLICOSE;HEMOGLOBINA;HEMATOCRITO;GPS NUMBER';

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
  utils_locale, utils_global, utils_dialogs, utils_system,
  data_types, data_management, data_getvalue, data_consts,
  models_users, models_taxonomy, models_birds, models_geo, models_bands,
  models_sampling_plots,
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
  Survey: TSurvey;
  BandRepo: TBandRepository;
  Band, RemovedBand: TBand;
  IndividualRepo: TIndividualRepository;
  Individuo: TIndividual;
  CaptureRepo: TCaptureRepository;
  Captura: TCapture;
  NetStation: TSamplingPlot;
  SPlotRepo: TSamplingPlotRepository;
  NetSite: TNetEffort;
  strDate, strTime: String;
  CodAnilha, aMethod: Integer;
  NetLat, NetLong: Extended;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

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
    Reg.FromCSV(CSV);

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

          try
            Taxon := TTaxon.Create();
            TaxonRepo.GetById(GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, Reg.SpeciesName), Taxon);
            NetStation := TSamplingPlot.Create;
            Toponimo := TSite.Create;
            NetSite := TNetEffort.Create;
            Survey := TSurvey.Create;
            Band := TBand.Create;
            RemovedBand := TBand.Create;
            Individuo := TIndividual.Create;
            Captura := TCapture.Create;
            aMethod := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileBanding);

            // Get valid taxon
            if Taxon.ValidId > 0 then
              TaxonRepo.GetById(Taxon.ValidId, Taxon);

            // Get net station and locality
            SPlotRepo.FindBy(COL_ABBREVIATION, Reg.NetStation, NetStation);
            if NetStation.Id > 0 then
            begin
              SiteRepo.GetById(NetStation.LocalityId, Toponimo);
            end;

            // Get survey
            Survey.Find(Toponimo.Id, aMethod, Reg.CaptureDate, '', NetStation.Id);

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
              BandRepo.FindByNumber(Reg.BandSize, Reg.BandNumber, Band);
              if (Band = nil) then
              begin
                // If does not exist, insert the new band
                Band.Size := Reg.BandSize;
                Band.Number := Reg.BandNumber;
                Band.Status := bstAvailable;
                Band.SupplierId := GetKey('institutions', COL_INSTITUTION_ID, COL_ABBREVIATION, 'CEMAVE');
                Band.BandType := mkButtEndBand;
                Band.UserInserted := ActiveUser.Id;

                BandRepo.Insert(Band);
              end;
            end;

            // Get removed band
            if (Trim(Reg.RemovedBand) <> EmptyStr) then
            begin
              if WordCount(Reg.RemovedBand, [' ']) = 2 then
              begin
                BandRepo.FindByNumber(ExtractWord(1, Reg.RemovedBand, [' ']),
                  StrToInt(ExtractWord(2, Reg.RemovedBand, [' '])), RemovedBand);
                if (RemovedBand = nil) then
                begin
                  // If does not exist, insert the removed band
                  RemovedBand.Size := ExtractWord(1, Reg.RemovedBand, [' ']);
                  RemovedBand.Number := StrToInt(ExtractWord(2, Reg.RemovedBand, [' ']));
                  RemovedBand.Status := bstAvailable;
                  RemovedBand.SupplierId :=
                    GetKey('institutions', COL_INSTITUTION_ID, COL_ABBREVIATION, 'CEMAVE');
                  RemovedBand.BandType := mkButtEndBand;
                  RemovedBand.UserInserted := ActiveUser.Id;

                  BandRepo.Insert(RemovedBand);
                end;
              end;
            end;

            // Get individual
            if (Reg.CaptureType = 'C') then
              CodAnilha := RemovedBand.Id
            else
              CodAnilha := Band.Id;

            IndividualRepo.FindByBand(Taxon.Id, CodAnilha, Reg.RightLeg, Reg.LeftLeg, Individuo);
            if (Individuo.Id = 0) then
            begin
              // If does not exist, insert the individual
              Individuo.TaxonId := Taxon.Id;
              Individuo.BandId := CodAnilha;
              if (Reg.CaptureType = 'C') then
                Individuo.BandName := Reg.RemovedBand
              else
                Individuo.BandName := Format('%s %d', [Reg.BandSize, Reg.BandNumber]);
              Individuo.RightLegBelow := Reg.RightLeg;
              Individuo.LeftLegBelow := Reg.LeftLeg;
              Individuo.UserInserted := ActiveUser.Id;

              IndividualRepo.Insert(Individuo);
            end;

            // Check if the capture record exists
            CaptureRepo.FindByBand(Taxon.Id, CodAnilha, Reg.CaptureType, strDate, strTime, Captura);
            if (Captura.Id = 0) then
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
              Captura.BanderId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Reg.Bander);
              Captura.AnnotatorId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Reg.Recorder);
              case Reg.SubjectStatus of
                'N': Captura.SubjectStatus := sstNormal;
                'I': Captura.SubjectStatus := sstInjured;
                'W': Captura.SubjectStatus := sstWingSprain;
                'X': Captura.SubjectStatus := sstStressed;
                'D': Captura.SubjectStatus := sstDead;
              end;
              case Reg.CaptureType of
                'N': Captura.CaptureType := cptNew;
                'R': Captura.CaptureType := cptRecapture;
                'S': Captura.CaptureType := cptSameDay;
                'C': Captura.CaptureType := cptChangeBand;
              else
                Captura.CaptureType := cptUnbanded;
              end;
              case Reg.Sex of
                'M': Captura.SubjectSex := sexMale;
                'F': Captura.SubjectSex := sexFemale;
              else
                Captura.SubjectSex := sexUnknown;
              end;
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
                  GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Reg.Photographer1);
                if (Trim(Reg.Photographer2) <> EmptyStr) then
                  Captura.Photographer2Id :=
                    GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, Reg.Photographer2);
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
              Captura.UserInserted := ActiveUser.Id;

              CaptureRepo.Insert(Captura);
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

              CaptureRepo.Update(Captura);
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
            BandRepo.Free;
            SiteRepo.Free;
            SPlotRepo.Free;
            CaptureRepo.Free;
            IndividualRepo.Free;
            TaxonRepo.Free;
          end;
        end
        else
        // If it is a band record
        begin
          BandRepo := TBandRepository.Create(DMM.sqlCon);
          Band := TBand.Create;
          try
            // Get band
            if (Reg.BandNumber > 0) then
            begin
              BandRepo.FindByNumber(Reg.BandSize, Reg.BandNumber, Band);
              if (Band = nil) then
              begin
                // If does not exist, insert the new band
                Band.Size := Reg.BandSize;
                Band.Number := Reg.BandNumber;
                Band.Status := bstAvailable;
                Band.SupplierId := GetKey('institutions', COL_INSTITUTION_ID, COL_ABBREVIATION, 'CEMAVE');
                Band.BandType := mkButtEndBand;
                Band.UserInserted := ActiveUser.Id;

                BandRepo.Insert(Band);
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
    LogEvent(leaFinish, 'Import banding file');
  end;
end;

procedure ImportBandingJournalV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  Reg: TBandingJournal;
  SiteRepo: TSiteRepository;
  Toponimo: TSite;
  NetStation: TSamplingPlot;
  SPlotRepo: TSamplingPlotRepository;
  Survey: TSurvey;
  Weather1, Weather2, Weather3, Weather4: TWeatherLog;
  Member: TSurveyMember;
  strDate: String;
  pp, aMethod: Integer;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, Format('Import banding journal: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
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
      Schema.AddDelimitedText(BANDING_JOURNAL_SCHEMA, ';', True);
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

        Reg.FromCSV(CSV);

        strDate := FormatDateTime(MASK_ISO_DATE, Reg.SamplingDate);
        //if not CSV.FieldByName('START TIME').IsNull then
        //  strStartTime := FormatDateTime(MASK_DISPLAY_TIME, Reg.StartTime)
        //else
        //  strStartTime := EmptyStr;
        //if not CSV.FieldByName('END TIME').IsNull then
        //  strEndTime := FormatDateTime(MASK_DISPLAY_TIME, Reg.EndTime)
        //else
        //  strEndTime := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 1').IsNull then
        //  strWeatherTime1 := FormatDateTime(MASK_DISPLAY_TIME, Reg.Weather1.SamplingTime)
        //else
        //  strWeatherTime1 := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 2').IsNull then
        //  strWeatherTime2 := FormatDateTime(MASK_DISPLAY_TIME, Reg.Weather2.SamplingTime)
        //else
        //  strWeatherTime2 := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 3').IsNull then
        //  strWeatherTime3 := FormatDateTime(MASK_DISPLAY_TIME, Reg.Weather3.SamplingTime)
        //else
        //  strWeatherTime3 := EmptyStr;
        //if not CSV.FieldByName('WEATHER TIME 4').IsNull then
        //  strWeatherTime4 := FormatDateTime(MASK_DISPLAY_TIME, Reg.Weather4.SamplingTime)
        //else
        //  strWeatherTime4 := EmptyStr;


        try
          NetStation := TSamplingPlot.Create;
          Toponimo := TSite.Create;
          Survey := TSurvey.Create;
          aMethod := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileBanding);

          // Get net station and locality
          SPlotRepo.FindBy(COL_ABBREVIATION, Reg.NetStation, NetStation);
          if NetStation.Id > 0 then
          begin
            SiteRepo.GetById(NetStation.LocalityId, Toponimo);
          end;

          // Check if the survey exists
          if not Survey.Find(Toponimo.Id, aMethod, Reg.SamplingDate, '', NetStation.Id) then
          begin
            Survey.SurveyDate := Reg.SamplingDate;
            Survey.StartTime := Reg.StartTime;
            Survey.EndTime := Reg.EndTime;

            Survey.Duration := 0;
            Survey.MethodId := GetKey('methods', COL_METHOD_ID, COL_METHOD_ABBREVIATION, 'Banding');
            Survey.NetStationId := NetStation.Id;
            Survey.LocalityId := Toponimo.Id;
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
                  Member.PersonId := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, ExtractWord(pp, Reg.Team, [',', ';']));

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
                Weather1.SampleMoment := Reg.Weather1.SamplingMoment;
                Weather1.Temperature := Reg.Weather1.Temperature;
                Weather1.Precipitation := Reg.Weather1.Precipitation;
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
                Weather2.SampleMoment := Reg.Weather2.SamplingMoment;
                Weather2.Temperature := Reg.Weather2.Temperature;
                Weather2.Precipitation := Reg.Weather2.Precipitation;
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
                Weather3.SampleMoment := Reg.Weather3.SamplingMoment;
                Weather3.Temperature := Reg.Weather3.Temperature;
                Weather3.Precipitation := Reg.Weather3.Precipitation;
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
                Weather4.SampleMoment := Reg.Weather4.SamplingMoment;
                Weather4.Temperature := Reg.Weather4.Temperature;
                Weather4.Precipitation := Reg.Weather4.Precipitation;
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
      until CSV.Eof or stopProcess;

      if stopProcess then
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
    SiteRepo.Free;
    SPlotRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Import banding journal')
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
  NetSite: TNetEffort;
  strDate: String;
  aMethod: Integer;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, Format('Import banding effort: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
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

        Reg.FromCSV(CSV);

        try
          NetStation := TSamplingPlot.Create;
          Toponimo := TSite.Create;
          Survey := TSurvey.Create;
          NetSite := TNetEffort.Create;
          aMethod := GetKey('methods', COL_METHOD_ID, COL_METHOD_NAME, rsMobileBanding);

          // Get net station and locality
          SPlotRepo.FindBy(COL_ABBREVIATION, Reg.NetStation, NetStation);
          if NetStation.Id > 0 then
          begin
            SiteRepo.GetById(NetStation.LocalityId, Toponimo);
          end;

          // Check if the survey exists
          if Survey.Find(Toponimo.Id, aMethod, Reg.SamplingDate, '', NetStation.Id) then
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
      until CSV.Eof or stopProcess;

      if stopProcess then
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
    SiteRepo.Free;
    SPlotRepo.Free;
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
begin
  { 0 - Locality }
  Locality := CSV.FieldByName('LOCALITY').AsString;
  { 1 - NetStation }
  NetStation := CSV.FieldByName('STATION').AsString;
  { 2 - SamplingDate }
  SamplingDate := CSV.FieldByName('SAMPLING DATE').AsDateTime;
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
  Weather1.CloudCover := 0;
  Weather1.Precipitation := wpEmpty;
  Weather1.Temperature := 0.0;
  Weather1.WindSpeed := 0;
  Weather1.Humidity := 0.0;

  Weather2.SamplingTime := NullTime;
  Weather2.SamplingMoment := wmNone;
  Weather2.CloudCover := 0;
  Weather2.Precipitation := wpEmpty;
  Weather2.Temperature := 0.0;
  Weather2.WindSpeed := 0;
  Weather2.Humidity := 0.0;

  Weather3.SamplingTime := NullTime;
  Weather3.SamplingMoment := wmNone;
  Weather3.CloudCover := 0;
  Weather3.Precipitation := wpEmpty;
  Weather3.Temperature := 0.0;
  Weather3.WindSpeed := 0;
  Weather3.Humidity := 0.0;

  Weather4.SamplingTime := NullTime;
  Weather4.SamplingMoment := wmNone;
  Weather4.CloudCover := 0;
  Weather4.Precipitation := wpEmpty;
  Weather4.Temperature := 0.0;
  Weather4.WindSpeed := 0;
  Weather4.Humidity := 0.0;
end;

procedure TBandingJournalHelper.FromCSV(CSV: TSdfDataSet);
begin
  { 0 - Locality }
  Locality := CSV.FieldByName('LOCALITY').AsString;
  { 1 - NetStation }
  NetStation := CSV.FieldByName('STATION').AsString;
  { 2 - SamplingDate }
  SamplingDate := CSV.FieldByName('DATE').AsDateTime;
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
    case CSV.FieldByName('WEATHER MOMENT 1').AsString of
      'S': Weather1.SamplingMoment := wmStart;
      'M': Weather1.SamplingMoment := wmMiddle;
      'E': Weather1.SamplingMoment := wmEnd;
    else
      Weather1.SamplingMoment := wmNone;
    end;
  { 11 - Cloud Cover 1 }
  if (not CSV.FieldByName('CLOUD COVER 1').IsNull) then
    Weather1.CloudCover := CSV.FieldByName('CLOUD COVER 1').AsInteger;
  { 12 - Precipitation 1 }
  if (not CSV.FieldByName('PRECIPITATION 1').IsNull) then
    case CSV.FieldByName('PRECIPITATION 1').AsString of
      'N': Weather1.Precipitation := wpNone;
      'F': Weather1.Precipitation := wpFog;
      'M': Weather1.Precipitation := wpMist;
      'D': Weather1.Precipitation := wpDrizzle;
      'R': Weather1.Precipitation := wpRain;
    end;
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
    case CSV.FieldByName('WEATHER MOMENT 2').AsString of
      'S': Weather2.SamplingMoment := wmStart;
      'M': Weather2.SamplingMoment := wmMiddle;
      'E': Weather2.SamplingMoment := wmEnd;
    else
      Weather2.SamplingMoment := wmNone;
    end;
  { 18 - Cloud Cover 2 }
  if (not CSV.FieldByName('CLOUD COVER 2').IsNull) then
    Weather2.CloudCover := CSV.FieldByName('CLOUD COVER 2').AsInteger;
  { 19 - Precipitation 2 }
  if (not CSV.FieldByName('PRECIPITATION 2').IsNull) then
    case CSV.FieldByName('PRECIPITATION 2').AsString of
      'N': Weather2.Precipitation := wpNone;
      'F': Weather2.Precipitation := wpFog;
      'M': Weather2.Precipitation := wpMist;
      'D': Weather2.Precipitation := wpDrizzle;
      'R': Weather2.Precipitation := wpRain;
    end;
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
    case CSV.FieldByName('WEATHER MOMENT 3').AsString of
      'S': Weather3.SamplingMoment := wmStart;
      'M': Weather3.SamplingMoment := wmMiddle;
      'E': Weather3.SamplingMoment := wmEnd;
    else
      Weather3.SamplingMoment := wmNone;
    end;
  { 25 - Cloud Cover 3 }
  if (not CSV.FieldByName('CLOUD COVER 3').IsNull) then
    Weather3.CloudCover := CSV.FieldByName('CLOUD COVER 3').AsInteger;
  { 26 - Precipitation 3 }
  if (not CSV.FieldByName('PRECIPITATION 3').IsNull) then
    case CSV.FieldByName('PRECIPITATION 3').AsString of
      'N': Weather3.Precipitation := wpNone;
      'F': Weather3.Precipitation := wpFog;
      'M': Weather3.Precipitation := wpMist;
      'D': Weather3.Precipitation := wpDrizzle;
      'R': Weather3.Precipitation := wpRain;
    end;
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
    case CSV.FieldByName('WEATHER MOMENT 4').AsString of
      'S': Weather4.SamplingMoment := wmStart;
      'M': Weather4.SamplingMoment := wmMiddle;
      'E': Weather4.SamplingMoment := wmEnd;
    else
      Weather4.SamplingMoment := wmNone;
    end;
  { 32 - Cloud Cover 4 }
  if (not CSV.FieldByName('CLOUD COVER 4').IsNull) then
    Weather4.CloudCover := CSV.FieldByName('CLOUD COVER 4').AsInteger;
  { 33 - Precipitation 4 }
  if (not CSV.FieldByName('PRECIPITATION 4').IsNull) then
    case CSV.FieldByName('PRECIPITATION 4').AsString of
      'N': Weather4.Precipitation := wpNone;
      'F': Weather4.Precipitation := wpFog;
      'M': Weather4.Precipitation := wpMist;
      'D': Weather4.Precipitation := wpDrizzle;
      'R': Weather4.Precipitation := wpRain;
    end;
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

procedure TBandingDataHelper.FromCSV(CSV: TSdfDataSet);
begin
  Locality := CSV.FieldByName('LOCALITY').AsString;
  NetStation := CSV.FieldByName('STATION').AsString;
  if (not CSV.FieldByName('DATA').IsNull) then
    CaptureDate := CSV.FieldByName('DATA').AsDateTime;
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
  CaptureType := AnsiUpperCase(CSV.FieldByName('NEW_RECAP').AsString);
  if (CaptureType <> 'U') then
    BandSize := AnsiUpperCase(CSV.FieldByName('BAND_CODE').AsString);
  if (BandSize <> '') and (CaptureType <> 'U') then
    BandNumber := CSV.FieldByName('BAND NUMBER').AsInteger;
  RightLeg := AnsiUpperCase(CSV.FieldByName('RIGHT LEG').AsString);
  LeftLeg := AnsiUpperCase(CSV.FieldByName('LEFT LEG').AsString);
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
    StartPhotoNumber := CSV.FieldByName('INITIAL PHOTO NUMBER').AsInteger;
  if (not CSV.FieldByName('FINAL PHOTO NUMBER').IsNull) then
    EndPhotoNumber := CSV.FieldByName('FINAL PHOTO NUMBER').AsInteger;
  CameraName := CSV.FieldByName('CAMERA NAME').AsString;
  PhotoNameFormula := CSV.FieldByName('PHOTO NAME FORMULA').AsString;
  if (not CSV.FieldByName('CRANIO').IsNull) then
    SkullLength := CSV.FieldByName('CRANIO').AsFloat;
  if (not CSV.FieldByName('CULMEN EXPOSTO').IsNull) then
    ExposedCulmen := CSV.FieldByName('CULMEN EXPOSTO').AsFloat;
  if (not CSV.FieldByName('NP').IsNull) then
    NostrilBillTip := CSV.FieldByName('NP').AsFloat;
  if (not CSV.FieldByName('LARGURA BICO').IsNull) then
    BillWidth := CSV.FieldByName('LARGURA BICO').AsFloat;
  if (not CSV.FieldByName('ALTURA BICO').IsNull) then
    BillHeight := CSV.FieldByName('ALTURA BICO').AsFloat;
  if (not CSV.FieldByName('SANGUE').IsNull) then
    BloodSample := CSV.FieldByName('SANGUE').AsBoolean;
  if (not CSV.FieldByName('PENAS').IsNull) then
    FeatherSample := CSV.FieldByName('PENAS').AsBoolean;
  if (not CSV.FieldByName('LONGITUDE').IsNull) then
    Longitude := CSV.FieldByName('LONGITUDE').AsFloat;
  if (not CSV.FieldByName('LATITUDE').IsNull) then
    Latitude := CSV.FieldByName('LATITUDE').AsFloat;
  if (not CSV.FieldByName('KIPPS').IsNull) then
    KippsIndex := CSV.FieldByName('KIPPS').AsFloat;
  if (not CSV.FieldByName('GLICOSE').IsNull) then
    Glucose := CSV.FieldByName('GLICOSE').AsFloat;
  if (not CSV.FieldByName('HEMOGLOBINA').IsNull) then
    Hemoglobin := CSV.FieldByName('HEMOGLOBINA').AsFloat;
  if (not CSV.FieldByName('HEMATOCRITO').IsNull) then
    Hematocrit := CSV.FieldByName('HEMATOCRITO').AsFloat;
  GPSNumber := CSV.FieldByName('GPS NUMBER').AsString;
end;

end.

