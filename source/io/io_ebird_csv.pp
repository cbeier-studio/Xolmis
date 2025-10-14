{ Xolmis CSV eBird Record Format library

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

unit io_ebird_csv;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StrUtils, ComCtrls, DateUtils, fgl,
  DB, SQLDB, SdfData, fpjson, jsonparser, fpjsondataset,
  models_sampling, models_record_types;

const
  EBIRD_SCHEMA: String = 'Submission ID,Common Name,Scientific Name,Taxonomic Order,' +
    'Count,State/Province,County,Location ID,Location,Latitude,Longitude,Date,Time,' +
    'Protocol,Duration (Min),All Obs Reported,Distance Traveled (km),Area Covered (ha),' +
    'Number of Observers,Breeding Code,Observation Details,Checklist Comments,ML Catalog Numbers';

type
  TEbirdRecordFormat = record
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
  end;

  { TEbirdRecordFormatHelper }
type
  TEbirdRecordFormatHelper = record helper for TEbirdRecordFormat
    procedure Clear;
    procedure FromCSV(CSV: TSdfDataSet);
  end;

  procedure LoadEbirdFile(const aCSVFile: String; CSV: TSdfDataSet);
  procedure ImportEbirdData(aCSVFile: String);

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_system,
  data_types, data_getvalue, data_consts,
  models_users, models_taxonomy, models_geo, models_sightings,
  udm_main, udlg_progress;

procedure LoadEbirdFile(const aCSVFile: String; CSV: TSdfDataSet);
begin
  with CSV do
  begin
    Delimiter := ',';
    FirstLineAsSchema := True;
    CodePage := 'UTF-8';
    Schema.AddDelimitedText(EBIRD_SCHEMA, ',', True);
    FileName := aCSVFile;
    Open;
  end;
end;

procedure ImportEbirdData(aCSVFile: String);
var
  CSV: TSdfDataSet;
  Reg: TEbirdRecordFormat;
  Taxon: TTaxon;
  Toponimo: TSite;
  Survey: TSurvey;
  SurveyRepo: TSurveyRepository;
  Sight: TSighting;
  SightRepo: TSightingRepository;
  Quant, aMethod: Integer;
  RDate: String;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg(rsTitleImportFile, Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, Format('Import eBird file: %s', [aCSVFile]));
  stopProcess := False;
  dlgProgress := TdlgProgress.Create(nil);
  dlgProgress.Show;
  dlgProgress.Title := rsTitleImportFile;
  dlgProgress.Text := rsLoadingCSVFile;

  SurveyRepo := TSurveyRepository.Create(DMM.sqlCon);
  SightRepo := TSightingRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);
  try
    { Load CSV file using TSdfDataSet }
    //LoadEbirdFile(aCSVFile, CSV);
    Reg.FromCSV(CSV);

    dlgProgress.Position := 0;
    dlgProgress.Max := CSV.RecordCount;
    DMM.sqlTrans.StartTransaction;
    try
      CSV.First;
      while not CSV.EOF do
      begin
        if stopProcess then
          Break;

        dlgProgress.Text := Format(rsProgressRecords,[CSV.RecNo, CSV.RecordCount]);

        { Loading field values into TEbirdRecordFormat }
        Reg.FromCSV(CSV);

        { Load other variables }
        RDate := FormatDateTime(MASK_ISO_DATE, Reg.RecordDate);
        Quant := StrToIntDef(Reg.Count, 0);

        Toponimo := TSite.Create(GetKey('gazetteer', COL_SITE_ID, COL_EBIRD_NAME, Reg.LocationName));
        Taxon := TTaxon.Create(GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, Reg.ScientificName));
        Survey := TSurvey.Create;
        Sight := TSighting.Create;
        aMethod := GetKey('methods', COL_METHOD_ID, COL_EBIRD_NAME, Reg.Protocol);
        try
          { Find survey (Amostragem) }
          SurveyRepo.FindBySiteAndDate(Toponimo.Id, aMethod, Reg.RecordDate, '', 0, Survey);
          if (Survey.Id = 0) then
          begin
            { Create a survey if it does not exist }
            Survey.SurveyDate := Reg.RecordDate;
            Survey.StartTime := Reg.RecordTime;
            Survey.Duration := Reg.Duration;
            Survey.MethodId := aMethod;
            Survey.LocalityId := Toponimo.Id;
            Survey.Notes := Reg.ChecklistComments;
            Survey.TotalArea := Reg.AreaCovered;
            Survey.TotalDistance := Reg.DistanceTraveled;
            Survey.UserInserted := ActiveUser.Id;

            SurveyRepo.Insert(Survey);
          end;

          { Check if the record already exists }
          SightRepo.FindByCombo(Survey.Id, Taxon.Id, 0, Sight);
          if Sight.Id > 0 then
          begin
            { Insert record if it does not exist }
            Sight.SurveyId := Survey.Id;
            Sight.TaxonId := Taxon.Id;
            Sight.SightingDate := Reg.RecordDate;
            Sight.MethodId := aMethod;
            Sight.Notes := Reg.ObservationDetails;
            Sight.BreedingStatus := Reg.BreedingCode;
            Sight.SubjectTally := Quant;
            Sight.UserInserted := ActiveUser.Id;

            SightRepo.Insert(Sight);
          end
          else
          begin
            { Update record if it exists }
            Sight.Notes := Reg.ObservationDetails;
            Sight.BreedingStatus := Reg.BreedingCode;
            Sight.SubjectTally := Quant;
            Sight.UserUpdated := ActiveUser.Id;

            SightRepo.Update(Sight);
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
      end;

      if stopProcess then
      begin
        DMM.sqlTrans.Rollback;
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
        MsgDlg(rsTitleImportFile, rsSuccessfulImportEbird, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    SightRepo.Free;
    SurveyRepo.Free;
    dlgProgress.Close;
    FreeAndNil(dlgProgress);
    LogEvent(leaFinish, 'Import eBird file');
  end;
end;

{ TEbirdRecordFormatHelper }

procedure TEbirdRecordFormatHelper.Clear;
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
  RecordTime := NullTime;
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

procedure TEbirdRecordFormatHelper.FromCSV(CSV: TSdfDataSet);
var
  EnglishFS: TFormatSettings;
  FieldValue: String;
begin
  // Get English format for date, time and numbers
  GetLocaleFormatSettings(1033, EnglishFS);

  Clear;

  SubmissionID := CSV.FieldByName('Submission ID').AsString;
  CommonName := CSV.FieldByName('Common Name').AsString;
  ScientificName := CSV.FieldByName('Scientific Name').AsString;
  TaxonomicOrder := CSV.FieldByName('Taxonomic Order').AsInteger;
  Count := CSV.FieldByName('Count').AsString;
  StateProvince := CSV.FieldByName('State/Province').AsString;
  County := CSV.FieldByName('County').AsString;
  LocationID := CSV.FieldByName('Location ID').AsString;
  LocationName := CSV.FieldByName('Location').AsString;

  FieldValue := CSV.FieldByName('Latitude').AsString;
  if (FieldValue <> '') then
    Latitude := StrToFloat(FieldValue, EnglishFS);

  FieldValue := CSV.FieldByName('Longitude').AsString;
  if (FieldValue <> '') then
    Longitude := StrToFloat(FieldValue, EnglishFS);

  FieldValue := CSV.FieldByName('Date').AsString;
  RecordDate := StrToDate(FieldValue, EnglishFS);

  FieldValue := CSV.FieldByName('Time').AsString;
  if (FieldValue <> '') then
    RecordTime := StrToTime(FieldValue, EnglishFS);

  Protocol := CSV.FieldByName('Protocol').AsString;
  if (CSV.FieldByName('Duration (Min)').AsString <> '') then
    Duration := CSV.FieldByName('Duration (Min)').AsInteger;
  AllObsReported := CSV.FieldByName('All Obs Reported').AsBoolean;

  FieldValue := CSV.FieldByName('Distance Traveled (km)').AsString;
  if (FieldValue <> '') then
    DistanceTraveled := StrToFloat(FieldValue, EnglishFS);

  FieldValue := CSV.FieldByName('Area Covered (ha)').AsString;
  if (FieldValue <> '') then
    AreaCovered := StrToFloat(FieldValue, EnglishFS);

  if (CSV.FieldByName('Number of Observers').AsString <> '') then
    NumberObservers := CSV.FieldByName('Number of Observers').AsInteger;
  if (CSV.FieldByName('Breeding Code').AsString <> '') then
    BreedingCode := ExtractDelimited(1, CSV.FieldByName('Breeding Code').AsString, [' ']);
  ObservationDetails := CSV.FieldByName('Observation Details').AsString;
  ChecklistComments := CSV.FieldByName('Checklist Comments').AsString;
  MLCatalogNumber := CSV.FieldByName('ML Catalog Numbers').AsString;
end;

end.

