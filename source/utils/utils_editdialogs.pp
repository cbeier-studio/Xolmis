{ Xolmis Edit Dialogs library

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit utils_editdialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, StrUtils, System.UITypes, Variants, data_types;

  function EditConnection(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;

  function EditMethod(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSite(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSamplingPlot(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditPermanentNet(aDataSet: TDataSet; aNetStation: Integer; IsNew: Boolean = False): Boolean;
  function EditInstitution(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditPerson(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditProject(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditProjectMember(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditProjectGoal(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditProjectActivity(aDataSet: TDataSet; aProject: Integer = 0; aGoal: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditProjectRubric(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditProjectExpense(aDataSet: TDataSet; aProject: Integer = 0; aRubric: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditPermit(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditBotanicTaxon(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditBand(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditIndividual(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditCapture(aDataSet: TDataSet; aIndividual: Integer = 0; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditFeather(aDataSet: TDataSet; aIndividual: Integer = 0; aCapture: Integer = 0;
    aSighting: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNest(aDataSet: TDataSet; aIndividual: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNestOwner(aDataSet: TDataSet; aNest: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNestRevision(aDataSet: TDataSet; aNest: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditEgg(aDataSet: TDataSet; aNest: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditExpedition(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSurvey(aDataSet: TDataSet; aExpedition: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSurveyMember(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNetEffort(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditWeatherLog(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditVegetation(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSighting(aDataSet: TDataSet; aSurvey: Integer = 0; aIndividual: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSpecimen(aDataSet: TDataSet; aIndividual: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditCollector(aDataSet: TDataSet; aSpecimen: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSamplePrep(aDataSet: TDataSet; aSpecimen: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditImageInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditAudioInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditDocInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditVideoInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditUser(IsNew: Boolean = False): Boolean;
  function ChangeUserPassword(IsNew: Boolean = False): Boolean;

implementation

uses
  utils_locale, utils_global, utils_permissions, utils_dialogs,
  data_getvalue,
  models_record_types, models_geo, models_sampling, models_botany, models_breeding, models_birds, models_projects,
  models_media, models_bands, models_sightings, models_institutions, models_people, models_permits,
  models_specimens, models_sampling_plots, models_methods,
  udm_main, udm_grid,
  udlg_changepassword, uedt_user, uedt_site, uedt_bands, uedt_expedition, uedt_capture,
  uedt_survey, uedt_samplingplot, uedt_institution, uedt_person, uedt_botanictaxon, uedt_individual,
  uedt_nest, uedt_egg, uedt_nestrevision, uedt_neteffort, uedt_permanentnet, uedt_sighting,
  uedt_method, uedt_weatherlog, uedt_project, uedt_permit, uedt_specimen, uedt_sampleprep, uedt_nestowner,
  uedt_imageinfo, uedt_audioinfo, uedt_documentinfo, uedt_vegetation, uedt_database, uedt_collector,
  uedt_projectmember, uedt_surveymember, uedt_projectgoal, uedt_projectactivity, uedt_projectrubric,
  uedt_projectexpense, uedt_feather, uedt_videoinfo;

function EditMethod(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TMethodRepository;
  FRecord, FOldRecord: TMethod;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Method edit dialog');
  Application.CreateForm(TedtMethod, edtMethod);
  FOldRecord := nil;
  FRepo := TMethodRepository.Create(DMM.sqlCon);
  with edtMethod do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TMethod.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TMethod.Create(aDataSet.FieldByName('method_id').AsInteger);
      FRecord := TMethod.Create(aDataSet.FieldByName('method_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Method := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Method);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Method.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbMethods, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbMethods, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('method_id', Method.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtMethod);
    LogEvent(leaClose, 'Method edit dialog');
  end;
end;

function EditSite(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TSiteRepository;
  FRecord, FOldRecord: TSite;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Gazetteer edit dialog');
  Application.CreateForm(TedtSite, edtSite);
  FOldRecord := nil;
  FRepo := TSiteRepository.Create(DMM.sqlCon);
  with edtSite do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSite.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSite.Create(aDataSet.FieldByName('site_id').AsInteger);
      FRecord := TSite.Create(aDataSet.FieldByName('site_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Site := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Site);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Site.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbGazetteer, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbGazetteer, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('site_id', Site.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSite);
    LogEvent(leaClose, 'Gazetteer edit dialog');
  end;
end;

function EditSamplingPlot(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TSamplingPlotRepository;
  FRecord, FOldRecord: TSamplingPlot;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Sampling plot edit dialog');
  Application.CreateForm(TedtSamplingPlot, edtSamplingPlot);
  FRepo := TSamplingPlotRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtSamplingPlot do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSamplingPlot.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSamplingPlot.Create(aDataSet.FieldByName('sampling_plot_id').AsInteger);
      FRecord := TSamplingPlot.Create(aDataSet.FieldByName('sampling_plot_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    SamplingPlot := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(SamplingPlot);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if SamplingPlot.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSamplingPlots, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSamplingPlots, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('sampling_plot_id', SamplingPlot.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSamplingPlot);
    LogEvent(leaClose, 'Sampling plot edit dialog');
  end;
end;

function EditPermanentNet(aDataSet: TDataSet; aNetStation: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TPermanentNetRepository;
  FRecord, FOldRecord: TPermanentNet;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Permanent net edit dialog');
  Application.CreateForm(TedtPermanentNet, edtPermanentNet);
  FRepo := TPermanentNetRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtPermanentNet do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TPermanentNet.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TPermanentNet.Create(aDataSet.FieldByName('permanent_net_id').AsInteger);
      FRecord := TPermanentNet.Create(aDataSet.FieldByName('permanent_net_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    PermanentNet := FRecord;
    SamplingPlotId := aNetStation;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(PermanentNet);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if PermanentNet.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbPermanentNets, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbPermanentNets, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('permanent_net_id', PermanentNet.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtPermanentNet);
    LogEvent(leaClose, 'Permanent net edit dialog');
  end;
end;

function EditInstitution(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TInstitutionRepository;
  FRecord, FOldRecord: TInstitution;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Institution edit dialog');
  Application.CreateForm(TedtInstitution, edtInstitution);
  FOldRecord := nil;
  FRepo := TInstitutionRepository.Create(DMM.sqlCon);
  with edtInstitution do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TInstitution.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TInstitution.Create(aDataSet.FieldByName('institution_id').AsInteger);
      FRecord := TInstitution.Create(aDataSet.FieldByName('institution_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Institution := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Institution);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Institution.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbInstitutions, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbInstitutions, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('institution_id', Institution.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtInstitution);
    LogEvent(leaClose, 'Institution edit dialog');
  end;
end;

function EditPerson(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TPersonRepository;
  FRecord, FOldRecord: TPerson;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Person edit dialog');
  Application.CreateForm(TedtPerson, edtPerson);
  FOldRecord := nil;
  FRepo := TPersonRepository.Create(DMM.sqlCon);
  with edtPerson do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TPerson.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TPerson.Create(aDataSet.FieldByName('person_id').AsInteger);
      FRecord := TPerson.Create(aDataSet.FieldByName('person_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Person := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Person);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Person.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbPeople, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbPeople, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('person_id', Person.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtPerson);
    LogEvent(leaClose, 'Person edit dialog');
  end;
end;

function EditProject(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TProjectRepository;
  FRecord, FOldRecord: TProject;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Project edit dialog');
  Application.CreateForm(TedtProject, edtProject);
  FRepo := TProjectRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtProject do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProject.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TProject.Create(aDataSet.FieldByName('project_id').AsInteger);
      FRecord := TProject.Create(aDataSet.FieldByName('project_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Project := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Project);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Project.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbProjects, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbProjects, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('project_id', Project.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtProject);
    LogEvent(leaClose, 'Project edit dialog');
  end;
end;

function EditProjectMember(aDataSet: TDataSet; aProject: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TProjectMemberRepository;
  FRecord, FOldRecord: TProjectMember;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Project member edit dialog');
  Application.CreateForm(TedtProjectMember, edtProjectMember);
  FRepo := TProjectMemberRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtProjectMember do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProjectMember.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TProjectMember.Create(aDataSet.FieldByName('project_member_id').AsInteger);
      FRecord := TProjectMember.Create(aDataSet.FieldByName('project_member_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    ProjectMember := FRecord;
    ProjectId := aProject;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(ProjectMember);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if ProjectMember.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbProjectTeams, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbProjectTeams, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('project_member_id', ProjectMember.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtProjectMember);
    LogEvent(leaClose, 'Project member edit dialog');
  end;
end;

function EditProjectGoal(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
var
  FRepo: TProjectGoalRepository;
  FRecord, FOldRecord: TProjectGoal;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Project goal edit dialog');
  Application.CreateForm(TedtProjectGoal, edtProjectGoal);
  FRepo := TProjectGoalRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtProjectGoal do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProjectGoal.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TProjectGoal.Create(aDataSet.FieldByName('goal_id').AsInteger);
      FRecord := TProjectGoal.Create(aDataSet.FieldByName('goal_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    ProjectGoal := FRecord;
    ProjectId := aProject;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(ProjectGoal);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if ProjectGoal.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbProjectGoals, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbProjectGoals, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('goal_id', ProjectGoal.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtProjectGoal);
    LogEvent(leaClose, 'Project goal edit dialog');
  end;
end;

function EditProjectActivity(aDataSet: TDataSet; aProject: Integer = 0; aGoal: Integer = 0; IsNew: Boolean = False): Boolean;
var
  FRepo: TProjectActivityRepository;
  FRecord, FOldRecord: TProjectActivity;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Project activity edit dialog');
  Application.CreateForm(TedtProjectActivity, edtProjectActivity);
  FRepo := TProjectActivityRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtProjectActivity do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProjectActivity.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TProjectActivity.Create(aDataSet.FieldByName('chronogram_id').AsInteger);
      FRecord := TProjectActivity.Create(aDataSet.FieldByName('chronogram_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    ProjectActivity := FRecord;
    ProjectId := aProject;
    if aGoal > 0 then
      GoalId := aGoal;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(ProjectActivity);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if ProjectActivity.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbProjectChronograms, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbProjectChronograms, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('chronogram_id', ProjectActivity.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtProjectActivity);
    LogEvent(leaClose, 'Project activity edit dialog');
  end;
end;

function EditProjectRubric(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
var
  FRepo: TProjectRubricRepository;
  FRecord, FOldRecord: TProjectRubric;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Project rubric edit dialog');
  Application.CreateForm(TedtProjectRubric, edtProjectRubric);
  FRepo := TProjectRubricRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtProjectRubric do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProjectRubric.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TProjectRubric.Create(aDataSet.FieldByName('budget_id').AsInteger);
      FRecord := TProjectRubric.Create(aDataSet.FieldByName('budget_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    ProjectRubric := FRecord;
    ProjectId := aProject;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(ProjectRubric);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if ProjectRubric.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbProjectBudgets, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbProjectBudgets, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('budget_id', ProjectRubric.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtProjectRubric);
    LogEvent(leaClose, 'Project rubric edit dialog');
  end;
end;

function EditProjectExpense(aDataSet: TDataSet; aProject: Integer = 0; aRubric: Integer = 0; IsNew: Boolean = False): Boolean;
var
  FRepo: TProjectExpenseRepository;
  FRecord, FOldRecord: TProjectExpense;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Project expense edit dialog');
  Application.CreateForm(TedtProjectExpense, edtProjectExpense);
  FRepo := TProjectExpenseRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtProjectExpense do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProjectExpense.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TProjectExpense.Create(aDataSet.FieldByName('expense_id').AsInteger);
      FRecord := TProjectExpense.Create(aDataSet.FieldByName('expense_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    ProjectExpense := FRecord;
    ProjectId := aProject;
    if aRubric > 0 then
      RubricId := aRubric;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(ProjectExpense);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if ProjectExpense.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbProjectExpenses, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbProjectExpenses, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('expense_id', ProjectExpense.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtProjectExpense);
    LogEvent(leaClose, 'Project expense edit dialog');
  end;
end;

function EditPermit(aDataSet: TDataSet; aProject: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TPermitRepository;
  FRecord, FOldRecord: TPermit;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Permit edit dialog');
  Application.CreateForm(TedtPermit, edtPermit);
  FOldRecord := nil;
  FRepo := TPermitRepository.Create(DMM.sqlCon);
  with edtPermit do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TPermit.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TPermit.Create(aDataSet.FieldByName('permit_id').AsInteger);
      FRecord := TPermit.Create(aDataSet.FieldByName('permit_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Permit := FRecord;
    ProjectId := aProject;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Permit);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Permit.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbPermits, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbPermits, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('permit_id', Permit.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtPermit);
    LogEvent(leaClose, 'Permit edit dialog');
  end;
end;

function EditBotanicTaxon(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TBotanicalTaxonRepository;
  FRecord, FOldRecord: TBotanicalTaxon;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Botanical taxon edit dialog');
  Application.CreateForm(TedtBotanicTaxon, edtBotanicTaxon);
  FOldRecord := nil;
  FRepo := TBotanicalTaxonRepository.Create(DMM.sqlCon);
  with edtBotanicTaxon do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TBotanicalTaxon.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TBotanicalTaxon.Create(aDataSet.FieldByName('taxon_id').AsInteger);
      FRecord := TBotanicalTaxon.Create(aDataSet.FieldByName('taxon_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Taxon := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Taxon);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Taxon.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbBotanicTaxa, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbBotanicTaxa, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('taxon_id', Taxon.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtBotanicTaxon);
    LogEvent(leaClose, 'Botanical taxon edit dialog');
  end;
end;

function EditBand(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TBandRepository;
  FRecord, FOldRecord: TBand;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Band edit dialog');
  Application.CreateForm(TedtBands, edtBands);
  FRepo := TBandRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtBands do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TBand.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TBand.Create(aDataSet.FieldByName('band_id').AsInteger);
      FRecord := TBand.Create(aDataSet.FieldByName('band_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Band := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Band);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Band.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbBands, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbBands, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('band_id', Band.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtBands);
    LogEvent(leaClose, 'Band edit dialog');
  end;
end;

function EditIndividual(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TIndividual;
  FRepo: TIndividualRepository;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Individual edit dialog');
  Application.CreateForm(TedtIndividual, edtIndividual);
  FOldRecord := nil;
  FRepo := TIndividualRepository.Create(DMM.sqlCon);
  with edtIndividual do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TIndividual.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TIndividual.Create(aDataSet.FieldByName('individual_id').AsInteger);
      FRecord := TIndividual.Create(aDataSet.FieldByName('individual_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Individual := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Individual);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Individual.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbIndividuals, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbIndividuals, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('individual_id', Individual.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtIndividual);
    LogEvent(leaClose, 'Individual edit dialog');
  end;
end;

function EditCapture(aDataSet: TDataSet; aIndividual: Integer; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TCapture;
  FRepo: TCaptureRepository;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Capture edit dialog');
  edtCapture := TedtCapture.Create(nil);
  FRepo := TCaptureRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtCapture do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TCapture.Create();
      if aSurvey > 0 then
      begin
        FRecord.CaptureDate := VarToDateTime(GetFieldValue('surveys', 'survey_date', 'survey_id', aSurvey));
        FRecord.LocalityId := GetFieldValue('surveys', 'locality_id', 'survey_id', aSurvey);
        FRecord.SurveyId := aSurvey;
      end;
      if aIndividual > 0 then
      begin
        FRecord.TaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', aIndividual);
        FRecord.BandId := GetFieldValue('individuals', 'band_id', 'individual_id', aIndividual);
        FRecord.IndividualId := aIndividual;
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TCapture.Create(aDataSet.FieldByName('capture_id').AsInteger);
      FRecord := TCapture.Create(aDataSet.FieldByName('capture_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Capture := FRecord;
    IndividualId := aIndividual;
    pIndividual.Visible := aIndividual = 0;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Capture);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Capture.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbCaptures, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbCaptures, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('capture_id', Capture.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtCapture);
    LogEvent(leaClose, 'Capture edit dialog');
  end;
end;

function EditNest(aDataSet: TDataSet; aIndividual: Integer; IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TNest;
  FRepo: TNestRepository;
  FOwner: TNestOwner;
  FRepoOwner: TNestOwnerRepository;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Nest edit dialog');
  Application.CreateForm(TedtNest, edtNest);
  FRepo := TNestRepository.Create(DMM.sqlCon);
  FRepoOwner := TNestOwnerRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtNest do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNest.Create();
      if aIndividual > 0 then
      begin
        FRecord.TaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', aIndividual);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TNest.Create(aDataSet.FieldByName('nest_id').AsInteger);
      FRecord := TNest.Create(aDataSet.FieldByName('nest_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Nest := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Nest);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Nest.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbNests, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbNests, haCreated, 0, '', '', '', rsInsertedByForm);

        { Is linked to an individual }
        if aIndividual > 0 then
        try
          FOwner := TNestOwner.Create();
          FOwner.IndividualId := aIndividual;
          FOwner.NestId := Nest.Id;
          FOwner.Role := nrlUnknown;
          FRepoOwner.Insert(FOwner);
        finally
          FreeAndNil(FOwner);
        end;

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('nest_id', Nest.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepoOwner.Free;
    FRepo.Free;
    FreeAndNil(edtNest);
    LogEvent(leaClose, 'Nest edit dialog');
  end;
end;

function EditNestOwner(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TNestOwner;
  FRepo: TNestOwnerRepository;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Nest owner edit dialog');
  Application.CreateForm(TedtNestOwner, edtNestOwner);
  FRepo := TNestOwnerRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtNestOwner do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNestOwner.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TNestOwner.Create(aDataSet.FieldByName('nest_owner_id').AsInteger);
      FRecord := TNestOwner.Create(aDataSet.FieldByName('nest_owner_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    NestOwner := FRecord;
    NestId := aNest;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(NestOwner);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if NestOwner.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbNestOwners, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbNestOwners, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('nest_owner_id', NestOwner.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtNestOwner);
    LogEvent(leaClose, 'Nest owner edit dialog');
  end;
end;

function EditNestRevision(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TNestRevision;
  FRepo: TNestRevisionRepository;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Nest revision edit dialog');
  Application.CreateForm(TedtNestRevision, edtNestRevision);
  FRepo := TNestRevisionRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtNestRevision do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNestRevision.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TNestRevision.Create(aDataSet.FieldByName('nest_revision_id').AsInteger);
      FRecord := TNestRevision.Create(aDataSet.FieldByName('nest_revision_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    NestRevision := FRecord;
    NestId := aNest;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(NestRevision);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if NestRevision.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbNestRevisions, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbNestRevisions, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('nest_revision_id', NestRevision.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtNestRevision);
    LogEvent(leaClose, 'Nest revision edit dialog');
  end;
end;

function EditEgg(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TEgg;
  FRepo: TEggRepository;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Egg edit dialog');
  Application.CreateForm(TedtEgg, edtEgg);
  FRepo := TEggRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtEgg do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TEgg.Create();
      if aNest > 0 then
      begin
        FRecord.TaxonId := GetFieldValue('nests', 'taxon_id', 'nest_id', aNest);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TEgg.Create(aDataSet.FieldByName('egg_id').AsInteger);
      FRecord := TEgg.Create(aDataSet.FieldByName('egg_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Egg := FRecord;
    NestId := aNest;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Egg);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Egg.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbEggs, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbEggs, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('egg_id', Egg.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtEgg);
    LogEvent(leaClose, 'Egg edit dialog');
  end;
end;

function EditExpedition(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  FRepo: TExpeditionRepository;
  FRecord, FOldRecord: TExpedition;
  lstDiff: TStrings;
  D: String;
begin
  Result := False;

  LogEvent(leaOpen, 'Expedition edit dialog');
  Application.CreateForm(TedtExpedition, edtExpedition);
  FRepo := TExpeditionRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtExpedition do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TExpedition.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TExpedition.Create(aDataSet.FieldByName('expedition_id').AsInteger);
      FRecord := TExpedition.Create(aDataSet.FieldByName('expedition_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Expedition := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Expedition);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Expedition.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbExpeditions, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbExpeditions, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('expedition_id', Expedition.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtExpedition);
    LogEvent(leaClose, 'Expedition edit dialog');
  end;
end;

function EditSurvey(aDataSet: TDataSet; aExpedition: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TSurveyRepository;
  FRecord, FOldRecord: TSurvey;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Survey edit dialog');
  Application.CreateForm(TedtSurvey, edtSurvey);
  FRepo := TSurveyRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtSurvey do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSurvey.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSurvey.Create(aDataSet.FieldByName('survey_id').AsInteger);
      FRecord := TSurvey.Create(aDataSet.FieldByName('survey_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Survey := FRecord;
    ExpeditionId := aExpedition;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Survey);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Survey.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSurveys, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSurveys, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('survey_id', Survey.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSurvey);
    LogEvent(leaClose, 'Survey edit dialog');
  end;
end;

function EditSurveyMember(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TSurveyMemberRepository;
  FRecord, FOldRecord: TSurveyMember;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Survey member edit dialog');
  Application.CreateForm(TedtSurveyMember, edtSurveyMember);
  FRepo := TSurveyMemberRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtSurveyMember do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSurveyMember.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSurveyMember.Create(aDataSet.FieldByName('survey_member_id').AsInteger);
      FRecord := TSurveyMember.Create(aDataSet.FieldByName('survey_member_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    SurveyMember := FRecord;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(SurveyMember);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if SurveyMember.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSurveyTeams, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSurveyTeams, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('survey_member_id', SurveyMember.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSurveyMember);
    LogEvent(leaClose, 'Survey member edit dialog');
  end;
end;

function EditNetEffort(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TNetEffortRepository;
  FRecord, FOldRecord: TNetEffort;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Net effort edit dialog');
  Application.CreateForm(TedtNetEffort, edtNetEffort);
  FRepo := TNetEffortRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtNetEffort do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNetEffort.Create();
      if aSurvey > 0 then
        FRecord.SampleDate := VarToDateTime(GetFieldValue('surveys', 'survey_date', 'survey_id', aSurvey));
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TNetEffort.Create(aDataSet.FieldByName('net_id').AsInteger);
      FRecord := TNetEffort.Create(aDataSet.FieldByName('net_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    NetEffort := FRecord;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(NetEffort);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if NetEffort.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbNetsEffort, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbNetsEffort, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('net_id', NetEffort.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtNetEffort);
    LogEvent(leaClose, 'Net effort edit dialog');
  end;
end;

function EditWeatherLog(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TWeatherLogRepository;
  FRecord, FOldRecord: TWeatherLog;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Weather log edit dialog');
  Application.CreateForm(TedtWeatherLog, edtWeatherLog);
  FRepo := TWeatherLogRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtWeatherLog do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TWeatherLog.Create();
      if aSurvey > 0 then
        FRecord.SampleDate := VarToDateTime(GetFieldValue('surveys', 'survey_date', 'survey_id', aSurvey));
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TWeatherLog.Create(aDataSet.FieldByName('weather_id').AsInteger);
      FRecord := TWeatherLog.Create(aDataSet.FieldByName('weather_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    WeatherLog := FRecord;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(WeatherLog);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if WeatherLog.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbWeatherLogs, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbWeatherLogs, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('weather_id', WeatherLog.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtWeatherLog);
    LogEvent(leaClose, 'Weather log edit dialog');
  end;
end;

function EditSighting(aDataSet: TDataSet; aSurvey: Integer; aIndividual: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TSightingRepository;
  FRecord, FOldRecord: TSighting;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Sighting edit dialog');
  Application.CreateForm(TedtSighting, edtSighting);
  FOldRecord := nil;
  FRepo := TSightingRepository.Create(DMM.sqlCon);
  with edtSighting do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if aDataSet <> DMG.qSightings then
      pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TSighting.Create();
      if aSurvey > 0 then
      begin
        FRecord.SightingDate := VarToDateTime(GetFieldValue('surveys', 'survey_date', 'survey_id', aSurvey));
        FRecord.MethodId := GetFieldValue('surveys', 'method_id', 'survey_id', aSurvey);
        FRecord.LocalityId := GetFieldValue('surveys', 'locality_id', 'survey_id', aSurvey);
      end;
      if aIndividual > 0 then
      begin
        FRecord.TaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', aIndividual);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSighting.Create(aDataSet.FieldByName('sighting_id').AsInteger);
      FRecord := TSighting.Create(aDataSet.FieldByName('sighting_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Sighting := FRecord;
    SurveyId := aSurvey;
    IndividualId := aIndividual;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Sighting);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Sighting.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSightings, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSightings, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('sighting_id', Sighting.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSighting);
    LogEvent(leaClose, 'Sighting edit dialog');
  end;
end;

function EditSpecimen(aDataSet: TDataSet; aIndividual: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TSpecimenRepository;
  FRecord, FOldRecord: TSpecimen;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Specimen edit dialog');
  Application.CreateForm(TedtSpecimen, edtSpecimen);
  FRepo := TSpecimenRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtSpecimen do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSpecimen.Create();
      if aIndividual > 0 then
      begin
        FRecord.IndividualId := aIndividual;
        FRecord.TaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', aIndividual);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSpecimen.Create(aDataSet.FieldByName('specimen_id').AsInteger);
      FRecord := TSpecimen.Create(aDataSet.FieldByName('specimen_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Specimen := FRecord;
    IndividualId := aIndividual;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Specimen);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Specimen.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSpecimens, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSpecimens, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('specimen_id', Specimen.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSpecimen);
    LogEvent(leaClose, 'Specimen edit dialog');
  end;
end;

function EditCollector(aDataSet: TDataSet; aSpecimen: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TSpecimenCollectorRepository;
  FRecord, FOldRecord: TSpecimenCollector;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Specimen collector edit dialog');
  Application.CreateForm(TedtCollector, edtCollector);
  FRepo := TSpecimenCollectorRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtCollector do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSpecimenCollector.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSpecimenCollector.Create(aDataSet.FieldByName('collector_id').AsInteger);
      FRecord := TSpecimenCollector.Create(aDataSet.FieldByName('collector_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    SpecimenCollector := FRecord;
    SpecimenId := aSpecimen;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(SpecimenCollector);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if SpecimenCollector.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSpecimenCollectors, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSpecimenCollectors, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('collector_id', SpecimenCollector.Id, []);
      finally
        aDataSet.EnableControls;
      end;

    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtCollector);
    LogEvent(leaClose, 'Specimen collector edit dialog');
  end;
end;

function EditSamplePrep(aDataSet: TDataSet; aSpecimen: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TSamplePrepRepository;
  FRecord, FOldRecord: TSamplePrep;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Sample prep edit dialog');
  Application.CreateForm(TedtSamplePrep, edtSamplePrep);
  FRepo := TSamplePrepRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtSamplePrep do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSamplePrep.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSamplePrep.Create(aDataSet.FieldByName('sample_prep_id').AsInteger);
      FRecord := TSamplePrep.Create(aDataSet.FieldByName('sample_prep_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    SamplePrep := FRecord;
    SpecimenId := aSpecimen;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(SamplePrep);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if SamplePrep.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbSamplePreps, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbSamplePreps, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('sample_prep_id', SamplePrep.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtSamplePrep);
    LogEvent(leaClose, 'Sample prep edit dialog');
  end;
end;

function EditUser(IsNew: Boolean): Boolean;
begin
  LogEvent(leaOpen, 'User edit dialog');
  Application.CreateForm(TedtUser, edtUser);
  with edtUser do
  try
    if IsNew then
    begin
      DMM.qUsers.Append;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      DMM.qUsers.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if IsNew then
      begin
        if ChangeUserPassword(IsNew) then
          DMM.qUsers.Post
        else
          DMM.qUsers.Cancel;
      end
      else
        DMM.qUsers.Post;
    end
    else
      DMM.qUsers.Cancel;
  finally
    FreeAndNil(edtUser);
    LogEvent(leaClose, 'User edit dialog');
  end;
end;

function ChangeUserPassword(IsNew: Boolean): Boolean;
begin
  Result := False;

  if not IsNew then
    if not UserLogin(DMM.qUsers.FieldByName('user_id').AsInteger) then
      Exit;

  LogEvent(leaOpen, 'Change password dialog');
  Application.CreateForm(TdlgChangePassword, dlgChangePassword);
  with dlgChangePassword do
  try
    if not IsNew then
      DMM.qUsers.Edit;

    Result := ShowModal = mrOk;
    if Result then
    begin
      DMM.qUsers.FieldByName('user_password').AsString := Pass;
      if not IsNew then
        DMM.qUsers.Post;
      LogDebug('Password changed');
    end else
    begin
      if not IsNew then
        DMM.qUsers.Cancel;
    end;
  finally
    FreeAndNil(dlgChangePassword);
    LogEvent(leaClose, 'Change password dialog');
  end;
end;

function EditImageInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  FRepo: TImageRepository;
  FRecord, FOldRecord: TImageData;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Image edit dialog');
  Application.CreateForm(TedtImageInfo, edtImageInfo);
  FRepo := TImageRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtImageInfo do
  try
    edtImageInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TImageData.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TImageData.Create(aDataSet.FieldByName('image_id').AsInteger);
      FRecord := TImageData.Create(aDataSet.FieldByName('image_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Image := FRecord;
    case aMasterType of
      //tbSamplingPlots: ;
      //tbPeople: ;
      tbIndividuals:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
      end;
      tbCaptures:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        CaptureId := aMaster.FieldByName('capture_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
      end;
      //tbMolts: ;
      tbNests:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        NestId := aMaster.FieldByName('nest_id').AsInteger;
      end;
      tbNestRevisions:
      begin
        NestId := aMaster.FieldByName('nest_id').AsInteger;
        NestRevisionId := aMaster.FieldByName('nest_revision_id').AsInteger;
      end;
      tbEggs:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        NestId := aMaster.FieldByName('nest_id').AsInteger;
        EggId := aMaster.FieldByName('egg_id').AsInteger;
      end;
      //tbMethods: ;
      //tbExpeditions: ;
      tbSurveys:
      begin
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
      end;
      tbSightings:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        SightingId := aMaster.FieldByName('sighting_id').AsInteger;
      end;
      tbSpecimens:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SpecimenId := aMaster.FieldByName('specimen_id').AsInteger;
      end;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Image);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Image.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbImages, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbImages, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('image_id', Image.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtImageInfo);
    LogEvent(leaClose, 'Image edit dialog');
  end;
end;

function EditAudioInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  FRepo: TAudioRepository;
  FRecord, FOldRecord: TAudioData;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Audio edit dialog');
  Application.CreateForm(TedtAudioInfo, edtAudioInfo);
  FRepo := TAudioRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtAudioInfo do
  try
    edtAudioInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TAudioData.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TAudioData.Create(aDataSet.FieldByName('audio_id').AsInteger);
      FRecord := TAudioData.Create(aDataSet.FieldByName('audio_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    AudioRecording := FRecord;
    case aMasterType of
      tbIndividuals:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
      end;
      //tbCaptures: ;
      //tbNests: ;
      //tbNestRevisions: ;
      //tbExpeditions: ;
      tbSurveys:
      begin
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
      end;
      tbSightings:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        SightingId := aMaster.FieldByName('sighting_id').AsInteger;
      end;
      tbSpecimens:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SpecimenId := aMaster.FieldByName('specimen_id').AsInteger;
      end;
      //tbSamplePreps: ;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(AudioRecording);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if AudioRecording.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbAudioLibrary, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbAudioLibrary, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('audio_id', AudioRecording.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtAudioInfo);
    LogEvent(leaClose, 'Audio edit dialog');
  end;
end;

function EditDocInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  FRepo: TDocumentRepository;
  FRecord, FOldRecord: TDocumentData;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Document edit dialog');
  Application.CreateForm(TedtDocumentInfo, edtDocumentInfo);
  FRepo := TDocumentRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtDocumentInfo do
  try
    edtDocumentInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TDocumentData.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TDocumentData.Create(aDataSet.FieldByName('document_id').AsInteger);
      FRecord := TDocumentData.Create(aDataSet.FieldByName('document_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Document := FRecord;
    case aMasterType of
      tbSamplingPlots:
      begin
        SamplingPlotId := aMaster.FieldByName('sampling_plot_id').AsInteger;
      end;
      //tbInstitutions: ;
      tbPeople:
      begin
        PersonId := aMaster.FieldByName('person_id').AsInteger;
      end;
      tbProjects:
      begin
        ProjectId := aMaster.FieldByName('project_id').AsInteger;
      end;
      tbPermits:
      begin
        PermitId := aMaster.FieldByName('permit_id').AsInteger;
      end;
      //tbBandHistory: ;
      tbIndividuals:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
      end;
      tbCaptures:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        CaptureId := aMaster.FieldByName('capture_id').AsInteger;
      end;
      tbNests:
      begin
        NestId := aMaster.FieldByName('nest_id').AsInteger;
      end;
      tbMethods:
      begin
        MethodId := aMaster.FieldByName('method_id').AsInteger;
      end;
      tbExpeditions:
      begin
        ExpeditionId := aMaster.FieldByName('expedition_id').AsInteger;
      end;
      tbSurveys:
      begin
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        ExpeditionId := aMaster.FieldByName('expedition_id').AsInteger;
      end;
      tbSightings:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        SightingId := aMaster.FieldByName('sighting_id').AsInteger;
      end;
      tbSpecimens:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        SpecimenId := aMaster.FieldByName('specimen_id').AsInteger;
      end;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Document);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Document.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbDocuments, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbDocuments, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('document_id', Document.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtDocumentInfo);
    LogEvent(leaClose, 'Document edit dialog');
  end;
end;

function EditVegetation(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  FRepo: TVegetationRepository;
  FRecord, FOldRecord: TVegetation;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Vegetation edit dialog');
  Application.CreateForm(TedtVegetation, edtVegetation);
  FRepo := TVegetationRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtVegetation do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TVegetation.Create();
      if aSurvey > 0 then
        FRecord.SampleDate := VarToDateTime(GetFieldValue('surveys', 'survey_date', 'survey_id', aSurvey));
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TVegetation.Create(aDataSet.FieldByName('vegetation_id').AsInteger);
      FRecord := TVegetation.Create(aDataSet.FieldByName('vegetation_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Vegetation := FRecord;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Vegetation);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Vegetation.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbVegetation, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbVegetation, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('vegetation_id', Vegetation.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtVegetation);
    LogEvent(leaClose, 'Vegetation edit dialog');
  end;
end;

function EditConnection(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  LogEvent(leaOpen, 'Connection edit dialog');
  edtDatabase := TedtDatabase.Create(Application);
  with edtDatabase do
  try
    dsConn.DataSet := aDataSet;
    aDataSet.Append;
    aDataSet.FieldByName('database_type').AsInteger := 0;
    Result := ShowModal = mrOK;
    if Result then
    begin
      if not FileExists(aDataSet.FieldByName('database_name').AsString) then
      begin
        MsgDlg(rsTitleCreateDatabase, rsUseNewDatabaseOption, mtWarning);
        aDataSet.Cancel;
      end
      else
        aDataSet.Post;
    end
    else
      aDataSet.Cancel;
  finally
    FreeAndNil(edtDatabase);
    LogEvent(leaClose, 'Connection edit dialog');
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditFeather(aDataSet: TDataSet; aIndividual: Integer; aCapture: Integer; aSighting: Integer;
  IsNew: Boolean): Boolean;
var
  FRecord, FOldRecord: TFeather;
  FRepo: TFeatherRepository;
  lstDiff: TStrings;
  D: String;
  aTime: Variant;
begin
  LogEvent(leaOpen, 'Feather edit dialog');
  Application.CreateForm(TedtFeather, edtFeather);
  FRepo := TFeatherRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtFeather do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TFeather.Create();
      if aIndividual > 0 then
        FRecord.TaxonId := GetFieldValue('individuals', 'taxon_id', 'individual_id', aIndividual);
      if aCapture > 0 then
      begin
        FRecord.TaxonId := GetFieldValue('captures', 'taxon_id', 'capture_id', aCapture);
        FRecord.SampleDate := VarToDateTime(GetFieldValue('captures', 'capture_date', 'capture_id', aCapture));
        aTime := GetFieldValue('captures', 'capture_time', 'capture_id', aCapture);
        if aTime <> Null then
          FRecord.SampleTime := VarToDateTime(aTime);
        FRecord.LocalityId := GetFieldValue('captures', 'locality_id', 'capture_id', aCapture);
        FRecord.ObserverId := GetFieldValue('captures', 'bander_id', 'capture_id', aCapture);
      end;
      if aSighting > 0 then
      begin
        FRecord.TaxonId := GetFieldValue('sightings', 'taxon_id', 'sighting_id', aSighting);
        FRecord.SampleDate := VarToDateTime(GetFieldValue('sightings', 'sighting_date', 'sighting_id', aSighting));
        aTime := GetFieldValue('sightings', 'sighting_time', 'sighting_id', aSighting);
        if aTime <> Null then
          FRecord.SampleTime := VarToDateTime(aTime);
        FRecord.LocalityId := GetFieldValue('sightings', 'locality_id', 'sighting_id', aSighting);
        FRecord.ObserverId := GetFieldValue('sightings', 'observer_id', 'sighting_id', aSighting);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TFeather.Create(aDataSet.FieldByName('feather_id').AsInteger);
      FRecord := TFeather.Create(aDataSet.FieldByName('feather_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Feather := FRecord;
    IndividualId := aIndividual;
    CaptureId := aCapture;
    SightingId := aSighting;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Feather);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Feather.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbFeathers, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbFeathers, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('feather_id', Feather.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtFeather);
    LogEvent(leaClose, 'Feather edit dialog');
  end;
end;

function EditVideoInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  FRepo: TVideoRepository;
  FRecord, FOldRecord: TVideoData;
  lstDiff: TStrings;
  D: String;
begin
  LogEvent(leaOpen, 'Video edit dialog');
  Application.CreateForm(TedtVideoInfo, edtVideoInfo);
  FRepo := TVideoRepository.Create(DMM.sqlCon);
  FOldRecord := nil;
  with edtVideoInfo do
  try
    edtVideoInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TVideoData.Create();
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TVideoData.Create(aDataSet.FieldByName('video_id').AsInteger);
      FRecord := TVideoData.Create(aDataSet.FieldByName('video_id').AsInteger);
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Video := FRecord;
    case aMasterType of
      tbIndividuals:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
      end;
      tbCaptures:
      begin
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        CaptureId := aMaster.FieldByName('capture_id').AsInteger;
      end;
      tbSurveys:
      begin
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
      end;
      tbSightings:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        IndividualId := aMaster.FieldByName('individual_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        SurveyId := aMaster.FieldByName('survey_id').AsInteger;
        SightingId := aMaster.FieldByName('sighting_id').AsInteger;
      end;
      tbNests:
      begin
        TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        NestId := aMaster.FieldByName('nest_id').AsInteger;
      end;
      tbNestRevisions:
      begin
        //TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        //LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        NestId := aMaster.FieldByName('nest_id').AsInteger;
        NestRevisionId := aMaster.FieldByName('nest_revision_id').AsInteger;
      end;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      if not DMM.sqlTrans.Active then
        DMM.sqlTrans.StartTransaction;
      try
        FRepo.Save(Video);

        { Save changes to the record history }
        if Assigned(FOldRecord) then
        begin
          lstDiff := TStringList.Create;
          try
            if Video.Diff(FOldRecord, lstDiff) then
            begin
              for D in lstDiff do
                WriteRecHistory(tbVideos, haEdited, FOldRecord.Id,
                  ExtractDelimited(1, D, [';']),
                  ExtractDelimited(2, D, [';']),
                  ExtractDelimited(3, D, [';']), EditSourceStr);
            end;
          finally
            FreeAndNil(lstDiff);
          end;
        end
        else
          WriteRecHistory(tbVideos, haCreated, 0, '', '', '', rsInsertedByForm);

        DMM.sqlTrans.CommitRetaining;
      except
        DMM.sqlTrans.RollbackRetaining;
        raise;
      end;

      // Go to record
      if not aDataSet.Active then
        Exit;
      aDataSet.DisableControls;
      try
        aDataSet.Refresh;
        aDataSet.Locate('video_id', Video.Id, []);
      finally
        aDataSet.EnableControls;
      end;
    end;
  finally
    if Assigned(FOldRecord) then
      FreeAndNil(FOldRecord);
    FRecord.Free;
    FRepo.Free;
    FreeAndNil(edtVideoInfo);
    LogEvent(leaClose, 'Video edit dialog');
  end;
end;

end.

