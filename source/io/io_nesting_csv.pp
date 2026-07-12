{ Xolmis CSV Xolmis Nesting Import Format library

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

unit io_nesting_csv;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Dialogs, ComCtrls, DateUtils, Controls,
  DB, SQLDB, SdfData, fpjson, jsonparser,
  models_record_types, io_core;

const
  NEST_SCHEMA: String = 'field_number;observer;taxon;male;female;latitude;longitude;altitude;locality;' +
    'height_above_ground;support_type;support_plant_1;support_plant_2;other_support;max_internal_diameter;min_internal_diameter;' +
    'max_external_diameter;min_external_diameter;internal_height;external_height;plant_center_distance;' +
    'plant_edge_distance;nest_cover;max_plant_diameter;min_plant_diameter;plant_height;plant_dbh;' +
    'productivity;nest_fate;philornis_larvae;found_stage;cause_of_loss;loss_stage;found_day;' +
    'last_day_active;last_seen;nest_age;nest_days_building;nest_days_egg;nest_days_nestling;description;notes';

  NEST_REVISION_SCHEMA: String = 'nest;date;time;observer;status;stage;host_eggs_tally;host_nestlings_tally;nidoparasite_eggs_tally;' +
    'nidoparasite_nestlings_tally;nidoparasite;photos;notes';

  EGG_SCHEMA: String = 'field_number;nest;observer;date;egg_num;taxon;length;width;mass;shape;pattern;texture;color;hatched;description;photos;individual;notes';

  procedure LoadNestingFile(const aCSVFile: String; CSV: TSdfDataSet);
  procedure ImportNestDataV1(aCSVFile: String; aOptions: TImportOptions; aProgressBar: TProgressBar = nil);
  procedure ImportNestRevisionsV1(aCSVFile: String; aOptions: TImportOptions; aProgressBar: TProgressBar = nil);
  procedure ImportEggDataV1(aCSVFile: String; aOptions: TImportOptions; aProgressBar: TProgressBar = nil);

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_system, utils_fullnames, utils_conversions, utils_validations,
  data_types, data_getvalue, data_consts, data_columns, io_csv,
  models_users, models_taxonomy, models_geo, models_breeding,
  udm_main, udlg_progress, udlg_taxonnotfound;

procedure ImportNestDataV1(aCSVFile: String; aOptions: TImportOptions; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  SiteRepo: TSiteRepository;
  Toponimo: TSite;
  NestRepo: TNestRepository;
  Nest, OldNest: TNest;
  OwnerRepo: TNestOwnerRepository;
  FOwner: TNestOwner;
  FCustomTaxon: String;
  FObserverId, FTaxonId, FSiteId: Integer;
  FFoundDate: TDate;
  dummyDT: TDateTime;
  FS: TFormatSettings;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Nest import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  if not ValidateCSVSchema(aCSVFile, NEST_SCHEMA, 'nests') then
    Exit;

  LogEvent(leaStart, Format('Import nests file: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;

  FS := DefaultFormatSettings;

  NestRepo := TNestRepository.Create(DMM.sqlCon);
  SiteRepo := TSiteRepository.Create(DMM.sqlCon);
  OwnerRepo := TNestOwnerRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    LoadNestingFile(aCSVFile, CSV);
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
        FFoundDate := NullDate;
        FTaxonId := 0;
        FSiteId := 0;
        FObserverId := 0;
        FCustomTaxon := EmptyStr;

        if (TryStrToDate(CSV.FieldByName('found_day').AsString, dummyDT, FS) or
          TryParseDateFlexible(CSV.FieldByName('found_day').AsString, dummyDT)) then
          FFoundDate := dummyDT
        else
          raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('found_day').AsString, rscDate]);

        try
          Toponimo := TSite.Create;
          Nest := TNest.Create;
          OldNest := TNest.Create;
          FOwner := TNestOwner.Create();

          // Get taxon
          if (CSV.FieldByName('taxon').AsString <> EmptyStr) then
          begin
            FTaxonId := GetValidTaxon(CSV.FieldByName('taxon').AsString);
            if (FTaxonId <= 0) then
            begin
              case aOptions.UnknownTaxonPolicy of
                utpAddCustomTaxon:
                  FCustomTaxon := CSV.FieldByName('taxon').AsString;
                utpAsk:
                begin
                  dlgTaxonNotFound := TdlgTaxonNotFound.Create(nil);
                  with dlgTaxonNotFound do
                  try
                    TaxonName := CSV.FieldByName('taxon').AsString;
                    if ShowModal = mrOK then
                    begin
                      case SelectedOption of
                        0: FCustomTaxon := CSV.FieldByName('taxon').AsString;
                        1: FTaxonId := TaxonId;
                        2: raise Exception.Create(rsImportAbortedByUser);
                      end;
                    end;
                  finally
                    FreeAndNil(dlgTaxonNotFound);
                  end;
                end;
                utpAbort:
                  raise Exception.CreateFmt(rsErrorTaxonNotFound, [CSV.FieldByName('taxon').AsString]);
              end;
            end;
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, [rscTaxonID]);

          // Get locality
          if (CSV.FieldByName('locality').AsString <> EmptyStr) then
          begin
            FSiteId := GetSiteKey(CSV.FieldByName('locality').AsString);
            if (FSiteId <= 0) then
            begin
              raise Exception.CreateFmt(rsErrorToponymNotFound, [CSV.FieldByName('locality').AsString]);
            end
            else
              SiteRepo.GetById(FSiteId, Toponimo);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, [rscLocalityID]);

          // Get observer
          if (CSV.FieldByName('observer').AsString <> EmptyStr) then
          begin
            FObserverId := GetPersonKey(CSV.FieldByName('observer').AsString);
            if (FObserverId <= 0) then
            begin
              raise Exception.CreateFmt(rsErrorObserverNotFound, [CSV.FieldByName('observer').AsString]);
            end;
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, [rscObserverID]);


          // Check if the nest exists
          NestRepo.FindByFieldNumber(CSV.FieldByName('field_number').AsString, FTaxonId, Toponimo.Id,
                    FFoundDate, '', Nest);
          if not (Nest.IsNew) then
          begin
            EditSourceStr := rsEditedByImport;

            case aOptions.ExistingRecordPolicy of
              erpIgnoreExisting: ;
              erpUpdateExisting:
              begin
                // replace the existing nest
                OldNest.Assign(Nest);

                Nest.FieldNumber := CSV.FieldByName('field_number').AsString;
                Nest.ObserverId := FObserverId;
                Nest.LocalityId := Toponimo.Id;
                if ((xSettings.AutoFillCoordinates) and
                  (CSV.FieldByName('longitude').AsFloat = 0) and (CSV.FieldByName('latitude').AsFloat = 0)) then
                begin
                  Nest.Latitude := Toponimo.Latitude;
                  Nest.Longitude := Toponimo.Longitude;
                  Nest.CoordinatePrecision := cpReference;
                end
                else
                begin
                  Nest.Latitude := CSV.FieldByName('latitude').AsFloat;
                  Nest.Longitude := CSV.FieldByName('longitude').AsFloat;
                  Nest.CoordinatePrecision := cpExact;
                end;
                Nest.TaxonId := FTaxonId;
                Nest.SupportType := CSV.FieldByName('support_type').AsString;
                Nest.SupportPlant1Id := GetValidBotanicalTaxon(CSV.FieldByName('support_plant_1').AsString);
                Nest.SupportPlant2Id := GetValidBotanicalTaxon(CSV.FieldByName('support_plant_2').AsString);
                Nest.OtherSupport := CSV.FieldByName('other_support').AsString;
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
                Nest.BuildingDays := CSV.FieldByName('nest_days_building').AsFloat;
                Nest.IncubationDays := CSV.FieldByName('nest_days_egg').AsFloat;
                Nest.NestlingDays := CSV.FieldByName('nest_days_nestling').AsFloat;
                Nest.ActiveDays := Nest.IncubationDays + Nest.NestlingDays;
                Nest.NestFate := StrToNestFate(CSV.FieldByName('nest_fate').AsString);
                Nest.NestProductivity := CSV.FieldByName('productivity').AsInteger;
                Nest.FoundDate := FFoundDate;
                if Trim(CSV.FieldByName('last_day_active').AsString) = EmptyStr then
                  Nest.LastDate := NullDate
                else
                if (TryStrToDate(CSV.FieldByName('last_day_active').AsString, dummyDT, FS) or
                  TryParseDateFlexible(CSV.FieldByName('last_day_active').AsString, dummyDT)) then
                  Nest.LastDate := dummyDT
                else
                  raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('last_day_active').AsString, rscLastDateActive]);
                Nest.Description := CSV.FieldByName('description').AsString;

                NestRepo.Update(Nest);
                LogInfo(Format('Nest record ID=%d updated', [Nest.Id]));

                // Insert record history
                WriteDiff(tbNests, OldNest, Nest, EditSourceStr);

                // Insert nest owners
                if (CSV.FieldByName('male').AsString <> EmptyStr) then
                begin
                  FOwner.Clear;
                  OwnerRepo.FindByNest(Nest.Id, GetIndividualKey(CSV.FieldByName('male').AsString), FOwner);
                  if FOwner.IsNew then
                  begin
                    FOwner.NestId := Nest.Id;
                    FOwner.IndividualId := GetIndividualKey(CSV.FieldByName('male').AsString);
                    FOwner.Role := nrlMale;
                    OwnerRepo.Insert(FOwner);

                    // Insert record history
                    WriteRecHistory(tbNestOwners, haCreated, FOwner.Id, '', '', '', rsInsertedByImport);
                  end;
                end;

                if (CSV.FieldByName('female').AsString <> EmptyStr) then
                begin
                  FOwner.Clear;
                  OwnerRepo.FindByNest(Nest.Id, GetIndividualKey(CSV.FieldByName('female').AsString), FOwner);
                  if FOwner.IsNew then
                  begin
                    FOwner.NestId := Nest.Id;
                    FOwner.IndividualId := GetIndividualKey(CSV.FieldByName('female').AsString);
                    FOwner.Role := nrlFemale;
                    OwnerRepo.Insert(FOwner);

                    // Insert record history
                    WriteRecHistory(tbNestOwners, haCreated, FOwner.Id, '', '', '', rsInsertedByImport);
                  end;
                end;
              end;
              //erpAllowDuplicates: ;
            end;
          end
          else
          begin
            EditSourceStr := rsInsertedByImport;

            // if not, create a new nest
            Nest.FieldNumber := CSV.FieldByName('field_number').AsString;
            Nest.ObserverId := FObserverId;
            Nest.LocalityId := Toponimo.Id;
            if ((xSettings.AutoFillCoordinates) and
              (CSV.FieldByName('longitude').AsFloat = 0) and (CSV.FieldByName('latitude').AsFloat = 0)) then
            begin
              Nest.Latitude := Toponimo.Latitude;
              Nest.Longitude := Toponimo.Longitude;
              Nest.CoordinatePrecision := cpReference;
            end
            else
            begin
              Nest.Latitude := CSV.FieldByName('latitude').AsFloat;
              Nest.Longitude := CSV.FieldByName('longitude').AsFloat;
              Nest.CoordinatePrecision := cpExact;
            end;
            Nest.TaxonId := FTaxonId;
            Nest.SupportType := CSV.FieldByName('support_type').AsString;
            Nest.SupportPlant1Id := GetValidBotanicalTaxon(CSV.FieldByName('support_plant_1').AsString);
            Nest.SupportPlant2Id := GetValidBotanicalTaxon(CSV.FieldByName('support_plant_2').AsString);
            Nest.OtherSupport := CSV.FieldByName('other_support').AsString;
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
            Nest.BuildingDays := CSV.FieldByName('nest_days_building').AsFloat;
            Nest.IncubationDays := CSV.FieldByName('nest_days_egg').AsFloat;
            Nest.NestlingDays := CSV.FieldByName('nest_days_nestling').AsFloat;
            Nest.ActiveDays := Nest.IncubationDays + Nest.NestlingDays;
            Nest.NestFate := StrToNestFate(CSV.FieldByName('nest_fate').AsString);
            Nest.NestProductivity := CSV.FieldByName('productivity').AsInteger;
            Nest.FoundDate := FFoundDate;
            if Trim(CSV.FieldByName('last_day_active').AsString) = EmptyStr then
              Nest.LastDate := NullDate
            else
            if (TryStrToDate(CSV.FieldByName('last_day_active').AsString, dummyDT, FS) or
              TryParseDateFlexible(CSV.FieldByName('last_day_active').AsString, dummyDT)) then
              Nest.LastDate := dummyDT
            else
              raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('last_day_active').AsString, rscLastDateActive]);
            Nest.Description := CSV.FieldByName('description').AsString;

            NestRepo.Insert(Nest);
            LogInfo(Format('Nest record inserted with ID=%d', [Nest.Id]));

            // Insert record history
            WriteRecHistory(tbNests, haCreated, Nest.Id, '', '', '', rsInsertedByImport);

            // Insert nest owners
            if (CSV.FieldByName('male').AsString <> EmptyStr) then
            begin
              FOwner.Clear;
              OwnerRepo.FindByNest(Nest.Id, GetIndividualKey(CSV.FieldByName('male').AsString), FOwner);
              if FOwner.IsNew then
              begin
                FOwner.NestId := Nest.Id;
                FOwner.IndividualId := GetIndividualKey(CSV.FieldByName('male').AsString);
                FOwner.Role := nrlMale;
                OwnerRepo.Insert(FOwner);

                // Insert record history
                WriteRecHistory(tbNestOwners, haCreated, FOwner.Id, '', '', '', rsInsertedByImport);
              end;
            end;

            if (CSV.FieldByName('female').AsString <> EmptyStr) then
            begin
              FOwner.Clear;
              OwnerRepo.FindByNest(Nest.Id, GetIndividualKey(CSV.FieldByName('female').AsString), FOwner);
              if FOwner.IsNew then
              begin
                FOwner.NestId := Nest.Id;
                FOwner.IndividualId := GetIndividualKey(CSV.FieldByName('female').AsString);
                FOwner.Role := nrlFemale;
                OwnerRepo.Insert(FOwner);

                // Insert record history
                WriteRecHistory(tbNestOwners, haCreated, FOwner.Id, '', '', '', rsInsertedByImport);
              end;
            end;
          end;

        finally
          FreeAndNil(FOwner);
          FreeAndNil(Nest);
          FreeAndNil(OldNest);
          FreeAndNil(Toponimo);
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
        LogWarning('Nest import canceled by user, transaction rolled back.');
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
        begin
          dlgProgress.Text := rsProgressFinishing;
          MsgDlg(rsTitleImportFile, rsSuccessfulImportNests, mtInformation);
        end;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Nest import finished successfully, transaction committed.');
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during nest import, transaction rolled back.');
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    OwnerRepo.Free;
    SiteRepo.Free;
    NestRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Import nests file');
  end;
end;

procedure ImportNestRevisionsV1(aCSVFile: String; aOptions: TImportOptions; aProgressBar: TProgressBar);
var
  RevisionRepo: TNestRevisionRepository;
  Revision, OldRevision: TNestRevision;
  NestRepo: TNestRepository;
  Nest: TNest;
  CSV: TSdfDataSet;
  aDate: TDate;
  aTime: TTime;
  dummyDT: TDateTime;
  FS: TFormatSettings;
  aObserver, FNidoparasiteId, FNestId: Integer;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Nest revision import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  FS := DefaultFormatSettings;

  if not ValidateCSVSchema(aCSVFile, NEST_REVISION_SCHEMA, 'nest revisions') then
    Exit;

  LogEvent(leaStart, Format('Import nest revisions: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  NestRepo := TNestRepository.Create(DMM.sqlCon);
  RevisionRepo := TNestRevisionRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    LoadNestingFile(aCSVFile, CSV);
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
        aDate := NullDate;
        aTime := NullTime;
        aObserver := 0;
        FNidoparasiteId := 0;
        FNestId := 0;

        if (TryStrToDate(CSV.FieldByName('date').AsString, dummyDT, FS) or
          TryParseDateFlexible(CSV.FieldByName('date').AsString, dummyDT)) then
          aDate := dummyDT
        else
          raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('date').AsString, rscDate]);
        if (TryStrToTime(CSV.FieldByName('time').AsString, dummyDT, FS) or
          TryParseTimeFlexible(CSV.FieldByName('time').AsString, dummyDT)) then
          aTime := dummyDT
        else
          raise Exception.CreateFmt(rsErrorInvalidTimeForField, [CSV.FieldByName('time').AsString, rscTime]);

        if Trim(CSV.FieldByName('observer').AsString) <> EmptyStr then
        begin
          aObserver := GetPersonKey(CSV.FieldByName('observer').AsString);
        end
        else
          aObserver := 0;

        try
          Nest := TNest.Create;
          Revision := TNestRevision.Create;
          OldRevision := TNestRevision.Create;

          // Get taxon
          if (CSV.FieldByName('nidoparasite').AsString <> EmptyStr) then
          begin
            FNidoparasiteId := GetValidTaxon(CSV.FieldByName('nidoparasite').AsString);
            if (FNidoparasiteId <= 0) then
              raise Exception.CreateFmt(rsErrorTaxonNotFound, [CSV.FieldByName('nidoparasite').AsString]);
          end;

          // Get nest
          if (CSV.FieldByName('nest').AsString <> EmptyStr) then
          begin
            FNestId := GetKey(TBL_NESTS, COL_NEST_ID, COL_FIELD_NUMBER, CSV.FieldByName('nest').AsString);
            if (FNestId > 0) then
              NestRepo.GetById(FNestId, Nest)
            else
              raise Exception.CreateFmt(rsErrorNestNotFound, [CSV.FieldByName('nest').AsString]);
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, [rscNestID]);

          // Check if the nest revision exists
          RevisionRepo.FindByDate(Nest.Id, DateToStr(aDate), TimeToStr(aTime), aObserver, Revision);
          if not (Revision.IsNew) then
          begin
            case aOptions.ExistingRecordPolicy of
              erpIgnoreExisting: ;
              erpUpdateExisting:
              begin
                EditSourceStr := rsEditedByImport;

                // replace existing nest revision
                Revision.NestId := Nest.Id;
                Revision.RevisionDate := aDate;
                Revision.RevisionTime := aTime;
                Revision.Observer1Id := aObserver;
                Revision.Observer2Id := 0;
                Revision.NestStatus := StrToNestStatus(CSV.FieldByName('status').AsString);
                Revision.HostEggsTally := CSV.FieldByName('host_eggs_tally').AsInteger;
                Revision.HostNestlingsTally := CSV.FieldByName('host_nestlings_tally').AsInteger;
                Revision.NidoparasiteEggsTally := CSV.FieldByName('nidoparasite_eggs_tally').AsInteger;
                Revision.NidoparasiteNestlingsTally := CSV.FieldByName('nidoparasite_nestlings_tally').AsInteger;
                Revision.NidoparasiteId := FNidoparasiteId;
                Revision.HavePhilornisLarvae := False;
                Revision.NestStage := StrToNestStage(CSV.FieldByName('stage').AsString);
                Revision.Notes := CSV.FieldByName('notes').AsString;

                RevisionRepo.Update(Revision);
                LogInfo(Format('Nest revision record ID=%d updated', [Revision.Id]));

                // Insert record history
                WriteDiff(tbNestRevisions, OldRevision, Revision, EditSourceStr);
              end;
            end;
          end
          else
          begin
            EditSourceStr := rsInsertedByImport;

            // insert new nest revision
            Revision.NestId := Nest.Id;
            Revision.RevisionDate := aDate;
            Revision.RevisionTime := aTime;
            Revision.Observer1Id := aObserver;
            Revision.Observer2Id := 0;
            Revision.NestStatus := StrToNestStatus(CSV.FieldByName('status').AsString);
            Revision.HostEggsTally := CSV.FieldByName('host_eggs_tally').AsInteger;
            Revision.HostNestlingsTally := CSV.FieldByName('host_nestlings_tally').AsInteger;
            Revision.NidoparasiteEggsTally := CSV.FieldByName('nidoparasite_eggs_tally').AsInteger;
            Revision.NidoparasiteNestlingsTally := CSV.FieldByName('nidoparasite_nestlings_tally').AsInteger;
            Revision.NidoparasiteId := FNidoparasiteId;
            Revision.HavePhilornisLarvae := False;
            Revision.NestStage := StrToNestStage(CSV.FieldByName('stage').AsString);
            Revision.Notes := CSV.FieldByName('notes').AsString;

            RevisionRepo.Insert(Revision);
            LogInfo(Format('Nest revision record inserted with ID=%d', [Revision.Id]));

            // Insert record history
            WriteRecHistory(tbNestRevisions, haCreated, Revision.Id, '', '', '', rsInsertedByImport);
          end;
        finally
          FreeAndNil(Nest);
          FreeAndNil(Revision);
          FreeAndNil(OldRevision);
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
        LogWarning('Nest revision import canceled by user, transaction rolled back.');
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
        begin
          dlgProgress.Text := rsProgressFinishing;
          MsgDlg(rsTitleImportFile, rsSuccessfulImportNestRevisions, mtInformation);
        end;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Nest revision import finished successfully, transaction committed.');
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during nest revision import, transaction rolled back.');
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    RevisionRepo.Free;
    NestRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Import nest revisions');
  end;
end;

procedure ImportEggDataV1(aCSVFile: String; aOptions: TImportOptions; aProgressBar: TProgressBar);
var
  aObserver, FTaxonId, FIndividualId: Integer;
  FCustomTaxon: String;
  NestRepo: TNestRepository;
  Nest: TNest;
  EggRepo: TEggRepository;
  Egg, OldEgg: TEgg;
  CSV: TSdfDataSet;
  aDate: TDate;
  dummyDT: TDateTime;
  FS: TFormatSettings;
begin
  if not FileExists(aCSVFile) then
  begin
    LogError(Format('Egg import aborted: file not found (%s)', [aCSVFile]));
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  FS := DefaultFormatSettings;

  if not ValidateCSVSchema(aCSVFile, EGG_SCHEMA, 'eggs') then
    Exit;

  LogEvent(leaStart, Format('Import eggs file: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  NestRepo := TNestRepository.Create(DMM.sqlCon);
  EggRepo := TEggRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    LoadNestingFile(aCSVFile, CSV);
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
        aDate := NullDate;
        aObserver := 0;
        FTaxonId := 0;
        FIndividualId := 0;
        FCustomTaxon := EmptyStr;

        if (TryStrToDate(CSV.FieldByName('date').AsString, dummyDT, FS) or
          TryParseDateFlexible(CSV.FieldByName('date').AsString, dummyDT)) then
          aDate := dummyDT
        else
          raise Exception.CreateFmt(rsErrorInvalidDateForField, [CSV.FieldByName('date').AsString, rscDate]);

        if Trim(CSV.FieldByName('observer').AsString) <> EmptyStr then
        begin
          aObserver := GetPersonKey(CSV.FieldByName('observer').AsString);
        end
        else
          aObserver := 0;

        if Trim(CSV.FieldByName('individual').AsString) <> EmptyStr then
        begin
          FIndividualId := GetIndividualKey(CSV.FieldByName('individual').AsString);
        end;

        try
          Nest := TNest.Create;
          Egg := TEgg.Create;
          OldEgg := TEgg.Create;

          // Get taxon
          if (CSV.FieldByName('taxon').AsString <> EmptyStr) then
          begin
            FTaxonId := GetValidTaxon(CSV.FieldByName('taxon').AsString);
            if (FTaxonId <= 0) then
            begin
              case aOptions.UnknownTaxonPolicy of
                utpAddCustomTaxon:
                  FCustomTaxon := CSV.FieldByName('taxon').AsString;
                utpAsk:
                begin
                  dlgTaxonNotFound := TdlgTaxonNotFound.Create(nil);
                  with dlgTaxonNotFound do
                  try
                    TaxonName := CSV.FieldByName('taxon').AsString;
                    if ShowModal = mrOK then
                    begin
                      case SelectedOption of
                        0: FCustomTaxon := CSV.FieldByName('taxon').AsString;
                        1: FTaxonId := TaxonId;
                        2: raise Exception.Create(rsImportAbortedByUser);
                      end;
                    end;
                  finally
                    FreeAndNil(dlgTaxonNotFound);
                  end;
                end;
                utpAbort:
                  raise Exception.CreateFmt(rsErrorTaxonNotFound, [CSV.FieldByName('taxon').AsString]);
              end;
            end;
          end
          else
            raise Exception.CreateFmt(rsErrorRequiredField, [rscTaxonID]);

          // Get nest
          if (CSV.FieldByName('nest').AsString <> EmptyStr) then
          begin
            NestRepo.GetById(GetKey(TBL_NESTS, COL_NEST_ID, COL_FIELD_NUMBER, CSV.FieldByName('nest').AsString), Nest);
            if (Nest.IsNew) then
              raise Exception.CreateFmt(rsErrorNestNotFound, [CSV.FieldByName('nest').AsString]);
          end;

          // Check if the egg exists
          EggRepo.FindByFieldNumber(Nest.Id, CSV.FieldByName('field_number').AsString, DateToStr(aDate), aObserver, Egg);
          if not (Egg.IsNew) then
          begin
            case aOptions.ExistingRecordPolicy of
              erpIgnoreExisting: ;
              erpUpdateExisting:
              begin
                EditSourceStr := rsEditedByImport;

                // replace existing egg
                Egg.FieldNumber := CSV.FieldByName('field_number').AsString;
                Egg.EggSeq := CSV.FieldByName('egg_num').AsInteger;
                Egg.NestId := Nest.Id;
                Egg.EggShape := StrToEggShape(CSV.FieldByName('shape').AsString);
                Egg.Width := CSV.FieldByName('width').AsFloat;
                Egg.Length := CSV.FieldByName('length').AsFloat;
                Egg.Mass := CSV.FieldByName('mass').AsFloat;
                // Egg.Volume := CSV.FieldByName('volume').AsFloat;
                // Egg.EggStage := CSV.FieldByName('stage').AsString;
                Egg.EggshellColor := CSV.FieldByName('color').AsString;
                Egg.EggshellPattern := StrToEggPattern(CSV.FieldByName('pattern').AsString);
                Egg.EggshellTexture := StrToEggTexture(CSV.FieldByName('texture').AsString);
                Egg.EggHatched := CSV.FieldByName('hatched').AsBoolean;
                Egg.IndividualId := FIndividualId;
                Egg.ObserverId := aObserver;
                Egg.MeasureDate := aDate;
                Egg.TaxonId := FTaxonId;
                Egg.HostEgg := Nest.TaxonId = Egg.TaxonId;
                Egg.Description := CSV.FieldByName('description').AsString;
                Egg.Notes := CSV.FieldByName('notes').AsString;
                // Egg.FullName := CSV.FieldByName('full_name').AsString;

                EggRepo.Update(Egg);
                LogInfo(Format('Egg record ID=%d updated', [Egg.Id]));

                // Insert record history
                WriteDiff(tbEggs, OldEgg, Egg, EditSourceStr);
              end;
            end;
          end
          else
          begin
            EditSourceStr := rsInsertedByImport;

            // insert new egg
            Egg.FieldNumber := CSV.FieldByName('field_number').AsString;
            Egg.EggSeq := CSV.FieldByName('egg_num').AsInteger;
            Egg.NestId := Nest.Id;
            Egg.EggShape := StrToEggShape(CSV.FieldByName('shape').AsString);
            Egg.Width := CSV.FieldByName('width').AsFloat;
            Egg.Length := CSV.FieldByName('length').AsFloat;
            Egg.Mass := CSV.FieldByName('mass').AsFloat;
            // Egg.Volume := CSV.FieldByName('volume').AsFloat;
            // Egg.EggStage := CSV.FieldByName('stage').AsString;
            Egg.EggshellColor := CSV.FieldByName('color').AsString;
            Egg.EggshellPattern := StrToEggPattern(CSV.FieldByName('pattern').AsString);
            Egg.EggshellTexture := StrToEggTexture(CSV.FieldByName('texture').AsString);
            Egg.EggHatched := CSV.FieldByName('hatched').AsBoolean;
            Egg.IndividualId := FIndividualId;
            Egg.ObserverId := aObserver;
            Egg.MeasureDate := aDate;
            Egg.TaxonId := FTaxonId;
            Egg.HostEgg := Nest.TaxonId = Egg.TaxonId;
            Egg.Description := CSV.FieldByName('description').AsString;
            Egg.Notes := CSV.FieldByName('notes').AsString;
            // Egg.FullName := CSV.FieldByName('full_name').AsString;

            EggRepo.Insert(Egg);
            LogInfo(Format('Egg record inserted with ID=%d', [Egg.Id]));

            // Insert record history
            WriteRecHistory(tbEggs, haCreated, Egg.Id, '', '', '', rsInsertedByImport);
          end;
        finally
          FreeAndNil(Nest);
          FreeAndNil(Egg);
          FreeAndNil(OldEgg);
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
        LogWarning('Egg import canceled by user, transaction rolled back.');
        MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
      end
      else
      begin
        if Assigned(dlgProgress) then
        begin
          dlgProgress.Text := rsProgressFinishing;
          MsgDlg(rsTitleImportFile, rsSuccessfulImportEggs, mtInformation);
        end;
        DMM.sqlTrans.CommitRetaining;
        LogInfo('Egg import finished successfully, transaction committed.');
      end;
    except
      DMM.sqlTrans.RollbackRetaining;
      LogError('Exception during egg import, transaction rolled back.');
      raise;
    end;

  finally
    CSV.Close;
    FreeAndNil(CSV);
    EggRepo.Free;
    NestRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Import eggs file');
  end;
end;

procedure LoadNestingFile(const aCSVFile: String; CSV: TSdfDataSet);
begin
  with CSV do
  begin
    Delimiter := ';';
    FirstLineAsSchema := True;
    CodePage := 'Windows-1252';
    //Schema.AddDelimitedText(NEST_SCHEMA, ';', True);
    FileName := aCSVFile;
    Open;
  end;
end;

end.

