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
  Classes, SysUtils, Forms, Dialogs, StrUtils, ComCtrls, DateUtils, fgl,
  DB, SQLDB, SdfData, fpjson, jsonparser, fpjsondataset,
  models_sampling, models_record_types;

const
  NEST_SCHEMA: String = 'field_number;taxon;male;female;latitude;longitude;altitude;locality;' +
    'height_above_ground;support_plant_1;support_plant_2;max_internal_diameter;min_internal_diameter;' +
    'max_external_diameter;min_external_diameter;internal_height;external_height;plant_center_distance;' +
    'plant_edge_distance;nest_cover;max_plant_diameter;min_plant_diameter;plant_height;plant_dbh;' +
    'productivity;nest_fate;philornis_larvae;found_stage;cause_of_loss;loss_stage;found_day;' +
    'last_day_active;last_seen;nest_age;nest_days_egg;nest_days_nestling;notes';

  NEST_REVISION_SCHEMA: String = 'nest;date;observer;status;eggs_tally;nestlings_tally;photos;notes';

  EGG_SCHEMA: String = 'nest;date;egg_num;length;width;mass;shape;color;photos;notes';

type
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

  procedure LoadNestingFile(const aCSVFile: String; CSV: TSdfDataSet);
  procedure ImportNestDataV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportNestRevisionsV1(aCSVFile: String; aProgressBar: TProgressBar = nil);
  procedure ImportEggDataV1(aCSVFile: String; aProgressBar: TProgressBar = nil);

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_system, utils_fullnames,
  data_types, data_management, data_getvalue, data_consts,
  models_users, models_taxonomy, models_birds, models_geo, models_breeding, models_sightings, models_bands,
  udm_main, udlg_progress;

procedure ImportNestDataV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  CSV: TSdfDataSet;
  //Reg: TNestRecord;
  SiteRepo: TSiteRepository;
  Toponimo: TSite;
  Taxon: TTaxon;
  Nest: TNest;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, Format('Import nests file: %s', [aCSVFile]));
  stopProcess := False;
  if not Assigned(aProgressBar) then
  begin
    dlgProgress := TdlgProgress.Create(nil);
    dlgProgress.Show;
    dlgProgress.Title := rsTitleImportFile;
    dlgProgress.Text := rsLoadingCSVFile;
  end;
  SiteRepo := TSiteRepository.Create(DMM.sqlCon);
  CSV := TSdfDataSet.Create(nil);
  try
    { Define CSV format settings }
    LoadNestingFile(aCSVFile, CSV);

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
            Taxon.GetData(GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, CSV.FieldByName('taxon').AsString));

          // Get locality
          if (CSV.FieldByName('locality').AsString <> EmptyStr) then
            SiteRepo.GetById(GetSiteKey(CSV.FieldByName('locality').AsString), Toponimo);


          // Check if the nest exists
          if not Nest.Find(CSV.FieldByName('field_number').AsString, Taxon.Id, Toponimo.Id,
                    StrToDate(CSV.FieldByName('found_day').AsString)) then
          begin
            // if not, create a new nest
            Nest.FieldNumber := CSV.FieldByName('field_number').AsString;
            //Nest.ObserverId := GetKey('people', 'person_id', 'acronym', CSV.FieldByName('observer').AsString);
            Nest.LocalityId := Toponimo.Id;
            Nest.Latitude := CSV.FieldByName('latitude').AsFloat;
            Nest.Longitude := CSV.FieldByName('longitude').AsFloat;
            Nest.TaxonId := Taxon.Id;
            //Nest.SupportType := CSV.FieldByName('support_type').AsString;
            Nest.SupportPlant1Id := GetKey('botanic_taxa', COL_TAXON_ID, COL_TAXON_NAME, CSV.FieldByName('support_plant_1').AsString);
            Nest.SupportPlant2Id := GetKey('botanic_taxa', COL_TAXON_ID, COL_TAXON_NAME, CSV.FieldByName('support_plant_2').AsString);
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
            case CSV.FieldByName('nest_fate').AsString of
              'L': Nest.NestFate := nfLoss;
              'S': Nest.NestFate := nfSuccess;
            else
              Nest.NestFate := nfUnknown;
            end;
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
      until CSV.Eof or stopProcess;

      if stopProcess then
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
    SiteRepo.Free;
    if Assigned(dlgProgress) then
    begin
      dlgProgress.Close;
      FreeAndNil(dlgProgress);
    end;
    LogEvent(leaFinish, 'Import nests file');
  end;
end;

procedure ImportNestRevisionsV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  Revision: TNestRevision;
  Nest: TNest;
  Taxon: TTaxon;
  CSV: TSdfDataSet;
  aDate, aTime: String;
  aObserver: Integer;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, Format('Import nest revisions: %s', [aCSVFile]));
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
    LoadNestingFile(aCSVFile, CSV);

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
        aDate := CSV.FieldByName('revision_date').AsString;
        aTime := CSV.FieldByName('revision_time').AsString;

        if CSV.FieldByName('observer').AsString <> EmptyStr then
        begin
          aObserver := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, CSV.FieldByName('observer').AsString);
        end
        else
          aObserver := 0;

        try
          Taxon := TTaxon.Create;
          Nest := TNest.Create;
          Revision := TNestRevision.Create;

          // Get taxon
          if (CSV.FieldByName('nidoparasite').AsString <> EmptyStr) then
            Taxon.GetData(GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, CSV.FieldByName('nidoparasite').AsString));

          // Get nest
          if (CSV.FieldByName('nest').AsString <> EmptyStr) then
            Nest.GetData(GetKey('nests', COL_NEST_ID, COL_FIELD_NUMBER, CSV.FieldByName('nest').AsString));

          // Check if the nest revision exists
          if not Revision.Find(Nest.Id, aDate, aTime, aObserver) then
          begin
            Revision.NestId := Nest.Id;
            Revision.RevisionDate := CSV.FieldByName('revision_date').AsDateTime;
            Revision.RevisionTime := CSV.FieldByName('revision_time').AsDateTime;
            Revision.Observer1Id := aObserver;
            Revision.Observer2Id := 0;
            case CSV.FieldByName('nest_status').AsString of
              'I': Revision.NestStatus := nstInactive;
              'A': Revision.NestStatus := nstActive;
            else
              Revision.NestStatus := nstUnknown;
            end;
            Revision.HostEggsTally := CSV.FieldByName('host_eggs_tally').AsInteger;
            Revision.HostNestlingsTally := CSV.FieldByName('host_nestlings_tally').AsInteger;
            Revision.NidoparasiteEggsTally := CSV.FieldByName('nidoparasite_eggs_tally').AsInteger;
            Revision.NidoparasiteNestlingsTally := CSV.FieldByName('nidoparasite_nestlings_tally').AsInteger;
            Revision.NidoparasiteId := Taxon.Id;
            Revision.HavePhilornisLarvae := False;
            case CSV.FieldByName('nest_stage').AsString of
              'X': Revision.NestStage := nsgInactive;
              'C': Revision.NestStage := nsgConstruction;
              'L': Revision.NestStage := nsgLaying;
              'I': Revision.NestStage := nsgIncubation;
              'H': Revision.NestStage := nsgHatching;
              'N': Revision.NestStage := nsgNestling;
            else
              Revision.NestStage := nsgUnknown;
            end;
            Revision.Notes := CSV.FieldByName('notes').AsString;
            Revision.FullName := GetNestRevisionFullName(Revision.RevisionDate, Revision.NestId, NEST_STAGES[Revision.NestStage], NEST_STATUSES[Revision.NestStatus]);
            Revision.UserInserted := ActiveUser.Id;

            Revision.Insert;

            // Insert record history
            WriteRecHistory(tbNestRevisions, haCreated, Revision.Id, '', '', '', rsInsertedByImport);

          end;

        finally
          FreeAndNil(Nest);
          FreeAndNil(Taxon);
          FreeAndNil(Revision);
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
    LogEvent(leaFinish, 'Import nest revisions');
  end;
end;

procedure ImportEggDataV1(aCSVFile: String; aProgressBar: TProgressBar);
var
  aObserver: Integer;
  Taxon: TTaxon;
  Nest: TNest;
  Egg: TEgg;
  CSV: TSdfDataSet;
  aDate: String;
begin
  if not FileExists(aCSVFile) then
  begin
    MsgDlg('', Format(rsErrorFileNotFound, [aCSVFile]), mtError);
    Exit;
  end;

  LogEvent(leaStart, Format('Import eggs file: %s', [aCSVFile]));
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
    LoadNestingFile(aCSVFile, CSV);

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
        aDate := CSV.FieldByName('revision_date').AsString;

        if CSV.FieldByName('observer').AsString <> EmptyStr then
        begin
          aObserver := GetKey('people', COL_PERSON_ID, COL_ABBREVIATION, CSV.FieldByName('observer').AsString);
        end
        else
          aObserver := 0;

        try
          Taxon := TTaxon.Create;
          Nest := TNest.Create;
          Egg := TEgg.Create;

          // Get taxon
          if (CSV.FieldByName('nidoparasite').AsString <> EmptyStr) then
            Taxon.GetData(GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME, CSV.FieldByName('nidoparasite').AsString));

          // Get nest
          if (CSV.FieldByName('nest').AsString <> EmptyStr) then
            Nest.GetData(GetKey('nests', COL_NEST_ID, COL_FIELD_NUMBER, CSV.FieldByName('nest').AsString));

          // Check if the egg exists
          if not Egg.Find(Nest.Id, CSV.FieldByName('field_number').AsString, aDate, aObserver) then
          begin
            Egg.FieldNumber := CSV.FieldByName('field_number').AsString;
            Egg.EggSeq := CSV.FieldByName('egg_seq').AsInteger;
            Egg.NestId := Nest.Id;
            case CSV.FieldByName('egg_shape').AsString of
              'S': Egg.EggShape := esSpherical;
              'E': Egg.EggShape := esElliptical;
              'O': Egg.EggShape := esOval;
              'P': Egg.EggShape := esPiriform;
              'C': Egg.EggShape := esConical;
              'B': Egg.EggShape := esBiconical;
              'Y': Egg.EggShape := esCylindrical;
              'L': Egg.EggShape := esLongitudinal;
            else
              Egg.EggShape := esUnknown;
            end;
            Egg.Width := CSV.FieldByName('egg_width').AsFloat;
            Egg.Length := CSV.FieldByName('egg_length').AsFloat;
            Egg.Mass := CSV.FieldByName('egg_mass').AsFloat;
            Egg.Volume := CSV.FieldByName('egg_volume').AsFloat;
            Egg.EggStage := CSV.FieldByName('egg_stage').AsString;
            Egg.EggshellColor := CSV.FieldByName('eggshell_color').AsString;
            case CSV.FieldByName('eggshell_pattern').AsString of
              'P':  Egg.EggshellPattern := espSpots;
              'B':  Egg.EggshellPattern := espBlotches;
              'S':  Egg.EggshellPattern := espSquiggles;
              'T':  Egg.EggshellPattern := espStreaks;
              'W':  Egg.EggshellPattern := espScrawls;
              'PS': Egg.EggshellPattern := espSpotsSquiggles;
              'BS': Egg.EggshellPattern := espBlotchesSquiggles;
            else
              Egg.EggshellPattern := espUnknown;
            end;
            case CSV.FieldByName('eggshell_texture').AsString of
              'C': Egg.EggshellTexture := estChalky;
              'S': Egg.EggshellTexture := estShiny;
              'G': Egg.EggshellTexture := estGlossy;
              'P': Egg.EggshellTexture := estPitted;
            else
              Egg.EggshellTexture := estUnknown;
            end;
            Egg.EggHatched := CSV.FieldByName('egg_hatched').AsBoolean;
            Egg.IndividualId := CSV.FieldByName('individual_id').AsInteger;
            Egg.ResearcherId := aObserver;
            Egg.MeasureDate := StrToDate(aDate);
            Egg.TaxonId := Taxon.Id;
            Egg.HostEgg := Nest.TaxonId = Egg.TaxonId;
            Egg.Description := CSV.FieldByName('description').AsString;
            Egg.Notes := CSV.FieldByName('notes').AsString;
            Egg.FullName := CSV.FieldByName('full_name').AsString;
            Egg.UserInserted := ActiveUser.Id;

            Egg.Insert;

            // Insert record history
            WriteRecHistory(tbEggs, haCreated, Egg.Id, '', '', '', rsInsertedByImport);

          end;

        finally
          FreeAndNil(Nest);
          FreeAndNil(Taxon);
          FreeAndNil(Egg);
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

