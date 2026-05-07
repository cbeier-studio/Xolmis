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
  Classes, SysUtils, Forms, Dialogs, StrUtils, ComCtrls, DateUtils,
  DB, SQLDB, SdfData, fpjson, jsonparser,
  models_sampling, models_record_types, io_core;

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
    procedure ToRow(Row: TXRow);
  end;

  procedure LoadEbirdFile(const aCSVFile: String; CSV: TSdfDataSet);
  procedure ImportEbirdData(aCSVFile: String);
  procedure ExportEbirdData(aOutputFile: String; SelectedRecords: TList = nil);
  function ValidateEbirdSchema(const FileName: String): Boolean;

implementation

uses
  {$IFDEF DARWIN}iosxlocale,{$ENDIF}
  utils_locale, utils_global, utils_dialogs, utils_system,
  data_types, data_getvalue, data_consts, io_csv,
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
  Sight, OldSight: TSighting;
  SightRepo: TSightingRepository;
  Quant, aMethod: Integer;
  RDate: String;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('eBird import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg(rsTitleImportFile, Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  if not ValidateEbirdSchema(aCSVFile) then
    Exit;

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
    LoadEbirdFile(aCSVFile, CSV);
    // Reg.FromCSV(CSV);
    LogInfo(Format('CSV file loaded with %d records.', [CSV.RecordCount]));

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

        Toponimo := TSite.Create(GetSiteKey(Reg.LocationName));
        Taxon := TTaxon.Create(GetValidTaxon(Reg.ScientificName));
        Survey := TSurvey.Create;
        Sight := TSighting.Create;
        OldSight := TSighting.Create;
        aMethod := GetMethodKey(Reg.Protocol);
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

            // Insert record history
            WriteRecHistory(tbSurveys, haCreated, Survey.Id, '', '', '', rsInsertedByImport);
            LogInfo(Format('Survey record inserted with ID=%d', [Survey.Id]));
          end;

          { Check if the record already exists }
          SightRepo.FindByCombo(Survey.Id, Taxon.Id, 0, Sight);
          if Sight.IsNew then
          begin
            { Insert record if it does not exist }
            Sight.SurveyId := Survey.Id;
            Sight.TaxonId := Taxon.Id;
            Sight.SightingDate := Reg.RecordDate;
            Sight.MethodId := aMethod;
            if ((xSettings.AutoFillCoordinates) and (Reg.Longitude = 0) and (Reg.Latitude = 0)) then
            begin
              Sight.Latitude := Toponimo.Latitude;
              Sight.Longitude := Toponimo.Longitude;
              Sight.CoordinatePrecision := cpReference;
            end
            else
            begin
              Sight.Latitude := Reg.Latitude;
              Sight.Longitude := Reg.Longitude;
              Sight.CoordinatePrecision := cpExact;
            end;
            Sight.Notes := Reg.ObservationDetails;
            Sight.BreedingStatus := Reg.BreedingCode;
            Sight.SubjectTally := Quant;
            Sight.UserInserted := ActiveUser.Id;

            SightRepo.Insert(Sight);

            // Insert record history
            WriteRecHistory(tbSightings, haCreated, Sight.Id, '', '', '', rsInsertedByImport);
            LogInfo(Format('Sighting record inserted with ID=%d', [Sight.Id]));
          end
          else
          begin
            { Update record if it exists }
            SightRepo.GetById(Sight.Id, OldSight);

            if ((xSettings.AutoFillCoordinates) and (Reg.Longitude = 0) and (Reg.Latitude = 0)) then
            begin
              Sight.Latitude := Toponimo.Latitude;
              Sight.Longitude := Toponimo.Longitude;
              Sight.CoordinatePrecision := cpReference;
            end
            else
            begin
              Sight.Latitude := Reg.Latitude;
              Sight.Longitude := Reg.Longitude;
              Sight.CoordinatePrecision := cpExact;
            end;
            Sight.Notes := Reg.ObservationDetails;
            Sight.BreedingStatus := Reg.BreedingCode;
            Sight.SubjectTally := Quant;
            Sight.UserUpdated := ActiveUser.Id;

            SightRepo.Update(Sight);

            // Insert record history
            WriteDiff(tbSightings, OldSight, Sight, rsEditedByImport);
            LogInfo(Format('Sighting record with ID=%d updated', [Sight.Id]));
          end;
        finally
          FreeAndNil(Taxon);
          FreeAndNil(Toponimo);
          FreeAndNil(Survey);
          FreeAndNil(Sight);
          FreeAndNil(OldSight);
        end;

        dlgProgress.Position := CSV.RecNo;
        Application.ProcessMessages;
        CSV.Next;
      end;

      if stopProcess then
      begin
        DMM.sqlTrans.Rollback;
        LogWarning('eBird import canceled by user, transaction rolled back.');
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        dlgProgress.Text := rsProgressFinishing;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('eBird import finished successfully, transaction committed.');
        DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
        LogInfo('Database optimized.');
        MsgDlg(rsTitleImportFile, rsSuccessfulImportEbird, mtInformation);
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during eBird import, transaction rolled back.');
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

function ValidateEbirdSchema(const FileName: String): Boolean;
var
  F: TextFile;
  Header: String;
  Cols, SchemaCols: TStringList;
  i: Integer;
begin
  Result := False;
  if not FileExists(FileName) then
  begin
    LogError('File not found: ' + FileName);
    MsgDlg(rsTitleValidateSchema, Format(rsErrorFileNotFound, [FileName]), mtError);
    Exit;
  end;

  LogEvent(leaStart, 'Validate eBird Record CSV schema');
  AssignFile(F, FileName, CP_UTF8);  // open file with UTF8 encoding
  Reset(F);
  try
    SchemaCols := TStringList.Create;
    SchemaCols.Delimiter := ',';
    SchemaCols.StrictDelimiter := True;
    SchemaCols.DelimitedText := EBIRD_SCHEMA;
    ReadLn(F, Header);
    Cols := TStringList.Create;
    Cols.Delimiter := ',';
    Cols.StrictDelimiter := True;
    Cols.DelimitedText := Header;
    try
      for i := 0 to SchemaCols.Count - 1 do
      begin
        if Cols.IndexOf(SchemaCols[i]) = -1 then
        begin
          LogError(Format('Required column "%s" not found', [SchemaCols[i]]));
          MsgDlg(rsTitleValidateSchema, Format(rsRequiredColumnNotFound, [SchemaCols[i]]), mtError);
          Exit;
        end;
      end;
      Result := True;
      LogDebug('CSV header is valid for eBird schema.');
    finally
      Cols.Free;
      SchemaCols.Free;
    end;
  finally
    CloseFile(F);
    LogEvent(leaFinish, 'Validate eBird Record CSV schema');
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

procedure TEbirdRecordFormatHelper.ToRow(Row: TXRow);
var
  EnglishFS: TFormatSettings;
  TimeStr: String;
begin
  EnglishFS := DefaultFormatSettings;

  // Get English format for date, time and numbers
  {$IFDEF WINDOWS}
  GetLocaleFormatSettings(1033, EnglishFS);
  {$ENDIF}
  {$IFDEF DARWIN}
  GetMacFormatSettings(EnglishFS);
  EnglishFS.DecimalSeparator := '.';
  EnglishFS.ThousandSeparator := ',';
  EnglishFS.DateSeparator := '/';
  EnglishFS.ShortDateFormat := 'MM/DD/YYYY';
  {$ENDIF}

  Row.Values['Submission ID'] := SubmissionID;
  Row.Values['Common Name'] := CommonName;
  Row.Values['Scientific Name'] := ScientificName;
  if TaxonomicOrder > 0 then
    Row.Values['Taxonomic Order'] := IntToStr(TaxonomicOrder)
  else
    Row.Values['Taxonomic Order'] := '';
  Row.Values['Count'] := Count;
  Row.Values['State/Province'] := StateProvince;
  Row.Values['County'] := County;
  Row.Values['Location ID'] := LocationID;
  Row.Values['Location'] := LocationName;

  if Latitude <> 0.0 then
    Row.Values['Latitude'] := Format('%.8f', [Latitude], EnglishFS)
  else
    Row.Values['Latitude'] := '';

  if Longitude <> 0.0 then
    Row.Values['Longitude'] := Format('%.8f', [Longitude], EnglishFS)
  else
    Row.Values['Longitude'] := '';

  if RecordDate > 0 then
    Row.Values['Date'] := FormatDateTime('mm/dd/yyyy', RecordDate, EnglishFS)
  else
    Row.Values['Date'] := '';

  if RecordTime > 0 then
    TimeStr := FormatDateTime('hh:nn', RecordTime, EnglishFS)
  else
    TimeStr := '';
  Row.Values['Time'] := TimeStr;

  Row.Values['Protocol'] := Protocol;
  if Duration > 0 then
    Row.Values['Duration (Min)'] := IntToStr(Duration)
  else
    Row.Values['Duration (Min)'] := '';

  if AllObsReported then
    Row.Values['All Obs Reported'] := 'true'
  else
    Row.Values['All Obs Reported'] := 'false';

  if DistanceTraveled > 0.0 then
    Row.Values['Distance Traveled (km)'] := Format('%.2f', [DistanceTraveled], EnglishFS)
  else
    Row.Values['Distance Traveled (km)'] := '';

  if AreaCovered > 0.0 then
    Row.Values['Area Covered (ha)'] := Format('%.2f', [AreaCovered], EnglishFS)
  else
    Row.Values['Area Covered (ha)'] := '';

  if NumberObservers > 0 then
    Row.Values['Number of Observers'] := IntToStr(NumberObservers)
  else
    Row.Values['Number of Observers'] := '';

  Row.Values['Breeding Code'] := BreedingCode;
  Row.Values['Observation Details'] := ObservationDetails;
  Row.Values['Checklist Comments'] := ChecklistComments;
  Row.Values['ML Catalog Numbers'] := MLCatalogNumber;
end;

procedure TEbirdRecordFormatHelper.FromCSV(CSV: TSdfDataSet);
var
  EnglishFS: TFormatSettings;
  FieldValue: String;
begin
  EnglishFS := DefaultFormatSettings;

  // Get English format for date, time and numbers
  {$IFDEF WINDOWS}
  GetLocaleFormatSettings(1033, EnglishFS);
  {$ENDIF}
  {$IFDEF DARWIN}
  GetMacFormatSettings(EnglishFS);
  EnglishFS.DecimalSeparator := '.';
  EnglishFS.ThousandSeparator := ',';
  EnglishFS.DateSeparator := '/';
  EnglishFS.ShortDateFormat := 'MM/DD/YYYY';
  {$ENDIF}

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

procedure ExportEbirdData(aOutputFile: String; SelectedRecords: TList = nil);
var
  FileStream: TFileStream;
  Exporter: TXolmisCSVExporter;
  ExportOptions: TExportOptions;
  Qry: TSQLQuery;
  Reg: TEbirdRecordFormat;
  Row: TXRow;
  IdList: TStringList;
  IdCSV: String;
  i, RecId, ExportedCount: Integer;
begin
  if aOutputFile = '' then
  begin
    LogError('eBird export aborted: output file path not specified');
    Exit;
  end;

  LogEvent(leaStart, Format('Export eBird file: %s', [aOutputFile]));

  try
    FileStream := TFileStream.Create(aOutputFile, fmCreate);
    Exporter := TXolmisCSVExporter.Create;
    Qry := TSQLQuery.Create(nil);
    IdList := TStringList.Create;

    try
      FillChar(ExportOptions, SizeOf(ExportOptions), 0);

      // Configure export options for eBird format
      ExportOptions.Delimiter := ',';
      ExportOptions.QuoteChar := '"';
      ExportOptions.HasHeader := True;
      ExportOptions.TrimFields := True;
      ExportOptions.IgnoreNulls := False;
      ExportOptions.Encoding := 'UTF-8';
      ExportOptions.ExportFields := EBIRD_SCHEMA;
      ExportOptions.DateFormat := 'MM/DD/YYYY';
      ExportOptions.TimeFormat := 'HH:MM';
      ExportOptions.DecimalSeparator := '.';
      ExportOptions.OnProgress := nil;
      ExportOptions.Cancel := nil;

      // Build selected IDs list (stored as pointers to integer IDs).
      if Assigned(SelectedRecords) and (SelectedRecords.Count > 0) then
      begin
        IdList.Sorted := True;
        IdList.Duplicates := dupIgnore;
        for i := 0 to SelectedRecords.Count - 1 do
        begin
          RecId := PtrInt(SelectedRecords[i]);
          if (RecId > 0) and (RecId <= High(Integer)) then
            IdList.Add(IntToStr(RecId));
        end;
      end;

      Qry.DataBase := DMM.sqlCon;
      Qry.Transaction := DMM.sqlTrans;
      with Qry.SQL do
      begin
        Clear;
        Add('SELECT s.sighting_id, s.subjects_tally, s.latitude AS sight_latitude, s.longitude AS sight_longitude,');
        Add('       s.sighting_date, s.sighting_time, s.breeding_status, s.notes AS sighting_notes,');
        Add('       z.scientific_name, z.english_name, z.order_id,');
        Add('       sv.survey_date, sv.start_time, sv.duration, sv.distance_total, sv.area_total, sv.observers_tally, sv.notes AS survey_notes,');
        Add('       mt.method_name, mt.ebird_name AS method_ebird_name,');
        Add('       gl.abbreviation AS locality_abbreviation, gl.site_name AS locality_site_name, gl.full_name AS locality_full_name,');
        Add('       gl.ebird_name AS locality_ebird_name, gl.latitude AS locality_latitude, gl.longitude AS locality_longitude,');
        Add('       gm.site_name AS municipality_name, gs.site_name AS state_name, gs.abbreviation AS state_abbreviation,');
        Add('       gc.abbreviation AS country_abbreviation');
        Add('FROM sightings AS s');
        Add('LEFT JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
        Add('LEFT JOIN surveys AS sv ON s.survey_id = sv.survey_id');
        Add('LEFT JOIN methods AS mt ON s.method_id = mt.method_id');
        Add('LEFT JOIN gazetteer AS gl ON s.locality_id = gl.site_id');
        Add('LEFT JOIN gazetteer AS gm ON gl.municipality_id = gm.site_id');
        Add('LEFT JOIN gazetteer AS gs ON gl.state_id = gs.site_id');
        Add('LEFT JOIN gazetteer AS gc ON gl.country_id = gc.site_id');
        Add('WHERE s.active_status = 1');

        if IdList.Count > 0 then
        begin
          IdCSV := IdList.CommaText;
          Add('AND s.sighting_id IN (' + IdCSV + ')');
        end;

        Add('ORDER BY s.sighting_date, s.sighting_time, s.sighting_id');
      end;

      Qry.Open;
      ExportedCount := 0;
      while not Qry.EOF do
      begin
        Reg.Clear;

        Reg.SubmissionID := '';
        Reg.CommonName := Qry.FieldByName('english_name').AsString;
        if Reg.CommonName = '' then
          Reg.CommonName := Qry.FieldByName('scientific_name').AsString;

        Reg.ScientificName := Qry.FieldByName('scientific_name').AsString;
        Reg.TaxonomicOrder := Qry.FieldByName('order_id').AsInteger;

        if Qry.FieldByName('subjects_tally').AsInteger > 0 then
          Reg.Count := IntToStr(Qry.FieldByName('subjects_tally').AsInteger)
        else
          Reg.Count := 'X';

        if (Qry.FieldByName('country_abbreviation').AsString <> '') and
           (Qry.FieldByName('state_abbreviation').AsString <> '') then
          Reg.StateProvince := Qry.FieldByName('country_abbreviation').AsString + '-' +
            Qry.FieldByName('state_abbreviation').AsString
        else
          Reg.StateProvince := Qry.FieldByName('state_name').AsString;

        Reg.County := Qry.FieldByName('municipality_name').AsString;
        Reg.LocationID := Qry.FieldByName('locality_abbreviation').AsString;

        Reg.LocationName := Qry.FieldByName('locality_ebird_name').AsString;
        if Reg.LocationName = '' then
          Reg.LocationName := Qry.FieldByName('locality_full_name').AsString;
        if Reg.LocationName = '' then
          Reg.LocationName := Qry.FieldByName('locality_site_name').AsString;

        if (not Qry.FieldByName('sight_latitude').IsNull) and
           (Qry.FieldByName('sight_latitude').AsFloat <> 0.0) then
          Reg.Latitude := Qry.FieldByName('sight_latitude').AsFloat
        else
          Reg.Latitude := Qry.FieldByName('locality_latitude').AsFloat;

        if (not Qry.FieldByName('sight_longitude').IsNull) and
           (Qry.FieldByName('sight_longitude').AsFloat <> 0.0) then
          Reg.Longitude := Qry.FieldByName('sight_longitude').AsFloat
        else
          Reg.Longitude := Qry.FieldByName('locality_longitude').AsFloat;

        if not Qry.FieldByName('sighting_date').IsNull then
          Reg.RecordDate := Qry.FieldByName('sighting_date').AsDateTime
        else
          Reg.RecordDate := Qry.FieldByName('survey_date').AsDateTime;

        if not Qry.FieldByName('sighting_time').IsNull then
          Reg.RecordTime := Qry.FieldByName('sighting_time').AsDateTime
        else
        if not Qry.FieldByName('start_time').IsNull then
          Reg.RecordTime := Qry.FieldByName('start_time').AsDateTime
        else
          Reg.RecordTime := NullTime;

        Reg.Protocol := Qry.FieldByName('method_ebird_name').AsString;
        if Reg.Protocol = '' then
          Reg.Protocol := Qry.FieldByName('method_name').AsString;

        Reg.Duration := Qry.FieldByName('duration').AsInteger;
        Reg.AllObsReported := False;
        Reg.DistanceTraveled := Qry.FieldByName('distance_total').AsFloat;
        Reg.AreaCovered := Qry.FieldByName('area_total').AsFloat;
        Reg.NumberObservers := Qry.FieldByName('observers_tally').AsInteger;
        Reg.BreedingCode := Qry.FieldByName('breeding_status').AsString;
        Reg.ObservationDetails := Qry.FieldByName('sighting_notes').AsString;
        Reg.ChecklistComments := Qry.FieldByName('survey_notes').AsString;
        Reg.MLCatalogNumber := '';

        Row := TXRow.Create;
        Reg.ToRow(Row);
        Exporter.AddRow(Row);

        Inc(ExportedCount);
        Qry.Next;
      end;
      Qry.Close;

      // Export to CSV file
      Exporter.Export(FileStream, ExportOptions, nil);

      LogInfo(Format('eBird export completed successfully (%d rows).', [ExportedCount]));
      MsgDlg(rsExportEbird, Format(rsSuccessfulExportEbird, [aOutputFile]), mtInformation);

    finally
      IdList.Free;
      Qry.Free;
      Exporter.Free;
      FileStream.Free;
    end;

  except
    on E: Exception do
    begin
      LogError(Format('Exception during eBird export: %s', [E.Message]));
      MsgDlg(rsExportEbird, Format(rsErrorExportingEbird, [E.Message]), mtError);
    end;
  end;

  LogEvent(leaFinish, 'Export eBird file');
end;

end.

