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
  data_getvalue, data_consts,
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
      FOldRecord := TMethod.Create();
      FRecord := TMethod.Create();
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
          WriteDiff(tbMethods, FOldRecord, Method, EditSourceStr);
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
        aDataSet.Locate(COL_METHOD_ID, Method.Id, []);
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
      FOldRecord := TSite.Create();
      FRecord := TSite.Create();
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
          WriteDiff(tbGazetteer, FOldRecord, Site, EditSourceStr);
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
        aDataSet.Locate(COL_SITE_ID, Site.Id, []);
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
      FOldRecord := TSamplingPlot.Create();
      FRecord := TSamplingPlot.Create();
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
          WriteDiff(tbSamplingPlots, FOldRecord, SamplingPlot, EditSourceStr);
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
        aDataSet.Locate(COL_SAMPLING_PLOT_ID, SamplingPlot.Id, []);
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
      FOldRecord := TPermanentNet.Create();
      FRecord := TPermanentNet.Create();
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
          WriteDiff(tbPermanentNets, FOldRecord, PermanentNet, EditSourceStr);
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
        aDataSet.Locate(COL_PERMANENT_NET_ID, PermanentNet.Id, []);
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
      FOldRecord := TInstitution.Create();
      FRecord := TInstitution.Create();
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
          WriteDiff(tbInstitutions, FOldRecord, Institution, EditSourceStr);
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
        aDataSet.Locate(COL_INSTITUTION_ID, Institution.Id, []);
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
      FOldRecord := TPerson.Create();
      FRecord := TPerson.Create();
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
          WriteDiff(tbPeople, FOldRecord, Person, EditSourceStr);
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
        aDataSet.Locate(COL_PERSON_ID, Person.Id, []);
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
      FOldRecord := TProject.Create();
      FRecord := TProject.Create();
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
          WriteDiff(tbProjects, FOldRecord, Project, EditSourceStr);
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
        aDataSet.Locate(COL_PROJECT_ID, Project.Id, []);
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
      FOldRecord := TProjectMember.Create();
      FRecord := TProjectMember.Create();
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
          WriteDiff(tbProjectTeams, FOldRecord, ProjectMember, EditSourceStr);
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
        aDataSet.Locate(COL_PROJECT_MEMBER_ID, ProjectMember.Id, []);
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
      FOldRecord := TProjectGoal.Create();
      FRecord := TProjectGoal.Create();
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
          WriteDiff(tbProjectGoals, FOldRecord, ProjectGoal, EditSourceStr);
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
        aDataSet.Locate(COL_GOAL_ID, ProjectGoal.Id, []);
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
      FOldRecord := TProjectActivity.Create();
      FRecord := TProjectActivity.Create();
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
          WriteDiff(tbProjectChronograms, FOldRecord, ProjectActivity, EditSourceStr);
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
        aDataSet.Locate(COL_CHRONOGRAM_ID, ProjectActivity.Id, []);
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
      FOldRecord := TProjectRubric.Create();
      FRecord := TProjectRubric.Create();
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
          WriteDiff(tbProjectBudgets, FOldRecord, ProjectRubric, EditSourceStr);
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
        aDataSet.Locate(COL_BUDGET_ID, ProjectRubric.Id, []);
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
      FOldRecord := TProjectExpense.Create();
      FRecord := TProjectExpense.Create();
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
          WriteDiff(tbProjectExpenses, FOldRecord, ProjectExpense, EditSourceStr);
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
        aDataSet.Locate(COL_EXPENSE_ID, ProjectExpense.Id, []);
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
      FOldRecord := TPermit.Create();
      FRecord := TPermit.Create();
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
          WriteDiff(tbPermits, FOldRecord, Permit, EditSourceStr);
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
        aDataSet.Locate(COL_PERMIT_ID, Permit.Id, []);
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
      FOldRecord := TBotanicalTaxon.Create();
      FRecord := TBotanicalTaxon.Create();
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
          WriteDiff(tbBotanicTaxa, FOldRecord, Taxon, EditSourceStr);
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
        aDataSet.Locate(COL_TAXON_ID, Taxon.Id, []);
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
      FOldRecord := TBand.Create();
      FRecord := TBand.Create();
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
          WriteDiff(tbBands, FOldRecord, Band, EditSourceStr);
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
        aDataSet.Locate(COL_BAND_ID, Band.Id, []);
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
      FOldRecord := TIndividual.Create();
      FRecord := TIndividual.Create();
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
          WriteDiff(tbIndividuals, FOldRecord, Individual, EditSourceStr);
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
        aDataSet.Locate(COL_INDIVIDUAL_ID, Individual.Id, []);
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
        FRecord.CaptureDate := VarToDateTime(GetFieldValue(TBL_SURVEYS, COL_SURVEY_DATE, COL_SURVEY_ID, aSurvey));
        FRecord.LocalityId := GetFieldValue(TBL_SURVEYS, COL_LOCALITY_ID, COL_SURVEY_ID, aSurvey);
        FRecord.SurveyId := aSurvey;
      end;
      if aIndividual > 0 then
      begin
        FRecord.TaxonId := GetFieldValue(TBL_INDIVIDUALS, COL_TAXON_ID, COL_INDIVIDUAL_ID, aIndividual);
        FRecord.BandId := GetFieldValue(TBL_INDIVIDUALS, COL_BAND_ID, COL_INDIVIDUAL_ID, aIndividual);
        FRecord.IndividualId := aIndividual;
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TCapture.Create();
      FRecord := TCapture.Create();
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
          WriteDiff(tbCaptures, FOldRecord, Capture, EditSourceStr);
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
        aDataSet.Locate(COL_CAPTURE_ID, Capture.Id, []);
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
        FRecord.TaxonId := GetFieldValue(TBL_INDIVIDUALS, COL_TAXON_ID, COL_INDIVIDUAL_ID, aIndividual);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TNest.Create();
      FRecord := TNest.Create();
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
          WriteDiff(tbNests, FOldRecord, Nest, EditSourceStr);
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
        aDataSet.Locate(COL_NEST_ID, Nest.Id, []);
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
      FOldRecord := TNestOwner.Create();
      FRecord := TNestOwner.Create();
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
          WriteDiff(tbNestOwners, FOldRecord, NestOwner, EditSourceStr);
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
        aDataSet.Locate(COL_NEST_OWNER_ID, NestOwner.Id, []);
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
      FOldRecord := TNestRevision.Create();
      FRecord := TNestRevision.Create();
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
          WriteDiff(tbNestRevisions, FOldRecord, NestRevision, EditSourceStr);
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
        aDataSet.Locate(COL_NEST_REVISION_ID, NestRevision.Id, []);
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
        FRecord.TaxonId := GetFieldValue(TBL_NESTS, COL_TAXON_ID, COL_NEST_ID, aNest);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TEgg.Create();
      FRecord := TEgg.Create();
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
          WriteDiff(tbEggs, FOldRecord, Egg, EditSourceStr);
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
        aDataSet.Locate(COL_EGG_ID, Egg.Id, []);
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
      FOldRecord := TExpedition.Create();
      FRecord := TExpedition.Create();
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
          WriteDiff(tbExpeditions, FOldRecord, Expedition, EditSourceStr);
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
        aDataSet.Locate(COL_EXPEDITION_ID, Expedition.Id, []);
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
      FOldRecord := TSurvey.Create();
      FRecord := TSurvey.Create();
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
          WriteDiff(tbSurveys, FOldRecord, Survey, EditSourceStr);
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
        aDataSet.Locate(COL_SURVEY_ID, Survey.Id, []);
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
      FOldRecord := TSurveyMember.Create();
      FRecord := TSurveyMember.Create();
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
          WriteDiff(tbSurveyTeams, FOldRecord, SurveyMember, EditSourceStr);
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
        aDataSet.Locate(COL_SURVEY_MEMBER_ID, SurveyMember.Id, []);
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
        FRecord.SampleDate := VarToDateTime(GetFieldValue(TBL_SURVEYS, COL_SURVEY_DATE, COL_SURVEY_ID, aSurvey));
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TNetEffort.Create();
      FRecord := TNetEffort.Create();
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
          WriteDiff(tbNetsEffort, FOldRecord, NetEffort, EditSourceStr);
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
        aDataSet.Locate(COL_NET_ID, NetEffort.Id, []);
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
        FRecord.SampleDate := VarToDateTime(GetFieldValue(TBL_SURVEYS, COL_SURVEY_DATE, COL_SURVEY_ID, aSurvey));
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TWeatherLog.Create();
      FRecord := TWeatherLog.Create();
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
          WriteDiff(tbWeatherLogs, FOldRecord, WeatherLog, EditSourceStr);
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
        aDataSet.Locate(COL_WEATHER_ID, WeatherLog.Id, []);
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
        FRecord.SightingDate := VarToDateTime(GetFieldValue(TBL_SURVEYS, COL_SURVEY_DATE, COL_SURVEY_ID, aSurvey));
        FRecord.MethodId := GetFieldValue(TBL_SURVEYS, COL_METHOD_ID, COL_SURVEY_ID, aSurvey);
        FRecord.LocalityId := GetFieldValue(TBL_SURVEYS, COL_LOCALITY_ID, COL_SURVEY_ID, aSurvey);
      end;
      if aIndividual > 0 then
      begin
        FRecord.TaxonId := GetFieldValue(TBL_INDIVIDUALS, COL_TAXON_ID, COL_INDIVIDUAL_ID, aIndividual);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSighting.Create();
      FRecord := TSighting.Create();
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
          WriteDiff(tbSightings, FOldRecord, Sighting, EditSourceStr);
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
        aDataSet.Locate(COL_SIGHTING_ID, Sighting.Id, []);
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
        FRecord.TaxonId := GetFieldValue(TBL_INDIVIDUALS, COL_TAXON_ID, COL_INDIVIDUAL_ID, aIndividual);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TSpecimen.Create();
      FRecord := TSpecimen.Create();
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
          WriteDiff(tbSpecimens, FOldRecord, Specimen, EditSourceStr);
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
        aDataSet.Locate(COL_SPECIMEN_ID, Specimen.Id, []);
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
      FOldRecord := TSpecimenCollector.Create();
      FRecord := TSpecimenCollector.Create();
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
          WriteDiff(tbSpecimenCollectors, FOldRecord, SpecimenCollector, EditSourceStr);
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
        aDataSet.Locate(COL_COLLECTOR_ID, SpecimenCollector.Id, []);
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
      FOldRecord := TSamplePrep.Create();
      FRecord := TSamplePrep.Create();
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
          WriteDiff(tbSamplePreps, FOldRecord, SamplePrep, EditSourceStr);
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
        aDataSet.Locate(COL_SAMPLE_PREP_ID, SamplePrep.Id, []);
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
      DMM.qUsers.FieldByName(COL_USER_PASSWORD).AsString := Pass;
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
      FOldRecord := TImageData.Create();
      FRecord := TImageData.Create();
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
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
      end;
      tbCaptures:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        CaptureId := aMaster.FieldByName(COL_CAPTURE_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
      end;
      //tbMolts: ;
      tbNests:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        NestId := aMaster.FieldByName(COL_NEST_ID).AsInteger;
      end;
      tbNestRevisions:
      begin
        NestId := aMaster.FieldByName(COL_NEST_ID).AsInteger;
        NestRevisionId := aMaster.FieldByName(COL_NEST_REVISION_ID).AsInteger;
      end;
      tbEggs:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        NestId := aMaster.FieldByName(COL_NEST_ID).AsInteger;
        EggId := aMaster.FieldByName(COL_EGG_ID).AsInteger;
      end;
      //tbMethods: ;
      //tbExpeditions: ;
      tbSurveys:
      begin
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
      end;
      tbSightings:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        SightingId := aMaster.FieldByName(COL_SIGHTING_ID).AsInteger;
      end;
      tbSpecimens:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SpecimenId := aMaster.FieldByName(COL_SPECIMEN_ID).AsInteger;
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
          WriteDiff(tbImages, FOldRecord, Image, EditSourceStr);
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
        aDataSet.Locate(COL_IMAGE_ID, Image.Id, []);
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
      FOldRecord := TAudioData.Create();
      FRecord := TAudioData.Create();
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    AudioRecording := FRecord;
    case aMasterType of
      tbIndividuals:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
      end;
      //tbCaptures: ;
      //tbNests: ;
      //tbNestRevisions: ;
      //tbExpeditions: ;
      tbSurveys:
      begin
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
      end;
      tbSightings:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        SightingId := aMaster.FieldByName(COL_SIGHTING_ID).AsInteger;
      end;
      tbSpecimens:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SpecimenId := aMaster.FieldByName(COL_SPECIMEN_ID).AsInteger;
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
          WriteDiff(tbAudioLibrary, FOldRecord, AudioRecording, EditSourceStr);
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
        aDataSet.Locate(COL_AUDIO_ID, AudioRecording.Id, []);
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
      FOldRecord := TDocumentData.Create();
      FRecord := TDocumentData.Create();
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Document := FRecord;
    case aMasterType of
      tbSamplingPlots:
      begin
        SamplingPlotId := aMaster.FieldByName(COL_SAMPLING_PLOT_ID).AsInteger;
      end;
      //tbInstitutions: ;
      tbPeople:
      begin
        PersonId := aMaster.FieldByName(COL_PERSON_ID).AsInteger;
      end;
      tbProjects:
      begin
        ProjectId := aMaster.FieldByName(COL_PROJECT_ID).AsInteger;
      end;
      tbPermits:
      begin
        PermitId := aMaster.FieldByName(COL_PERMIT_ID).AsInteger;
      end;
      //tbBandHistory: ;
      tbIndividuals:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
      end;
      tbCaptures:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        CaptureId := aMaster.FieldByName(COL_CAPTURE_ID).AsInteger;
      end;
      tbNests:
      begin
        NestId := aMaster.FieldByName(COL_NEST_ID).AsInteger;
      end;
      tbMethods:
      begin
        MethodId := aMaster.FieldByName(COL_METHOD_ID).AsInteger;
      end;
      tbExpeditions:
      begin
        ExpeditionId := aMaster.FieldByName(COL_EXPEDITION_ID).AsInteger;
      end;
      tbSurveys:
      begin
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        ExpeditionId := aMaster.FieldByName(COL_EXPEDITION_ID).AsInteger;
      end;
      tbSightings:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        SightingId := aMaster.FieldByName(COL_SIGHTING_ID).AsInteger;
      end;
      tbSpecimens:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        SpecimenId := aMaster.FieldByName(COL_SPECIMEN_ID).AsInteger;
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
          WriteDiff(tbDocuments, FOldRecord, Document, EditSourceStr);
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
        aDataSet.Locate(COL_DOCUMENT_ID, Document.Id, []);
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
        FRecord.SampleDate := VarToDateTime(GetFieldValue(TBL_SURVEYS, COL_SURVEY_DATE, COL_SURVEY_ID, aSurvey));
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TVegetation.Create();
      FRecord := TVegetation.Create();
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
          WriteDiff(tbVegetation, FOldRecord, Vegetation, EditSourceStr);
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
        aDataSet.Locate(COL_VEGETATION_ID, Vegetation.Id, []);
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
        FRecord.TaxonId := GetFieldValue(TBL_INDIVIDUALS, COL_TAXON_ID, COL_INDIVIDUAL_ID, aIndividual);
      if aCapture > 0 then
      begin
        FRecord.TaxonId := GetFieldValue(TBL_CAPTURES, COL_TAXON_ID, COL_CAPTURE_ID, aCapture);
        FRecord.SampleDate := VarToDateTime(GetFieldValue(TBL_CAPTURES, COL_CAPTURE_DATE, COL_CAPTURE_ID, aCapture));
        aTime := GetFieldValue(TBL_CAPTURES, COL_CAPTURE_TIME, COL_CAPTURE_ID, aCapture);
        if aTime <> Null then
          FRecord.SampleTime := VarToDateTime(aTime);
        FRecord.LocalityId := GetFieldValue(TBL_CAPTURES, COL_LOCALITY_ID, COL_CAPTURE_ID, aCapture);
        FRecord.ObserverId := GetFieldValue(TBL_CAPTURES, COL_BANDER_ID, COL_CAPTURE_ID, aCapture);
      end;
      if aSighting > 0 then
      begin
        FRecord.TaxonId := GetFieldValue(TBL_SIGHTINGS, COL_TAXON_ID, COL_SIGHTING_ID, aSighting);
        FRecord.SampleDate := VarToDateTime(GetFieldValue(TBL_SIGHTINGS, COL_SIGHTING_DATE, COL_SIGHTING_ID, aSighting));
        aTime := GetFieldValue(TBL_SIGHTINGS, COL_SIGHTING_TIME, COL_SIGHTING_ID, aSighting);
        if aTime <> Null then
          FRecord.SampleTime := VarToDateTime(aTime);
        FRecord.LocalityId := GetFieldValue(TBL_SIGHTINGS, COL_LOCALITY_ID, COL_SIGHTING_ID, aSighting);
        FRecord.ObserverId := GetFieldValue(TBL_SIGHTINGS, COL_OBSERVER_ID, COL_SIGHTING_ID, aSighting);
      end;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FOldRecord := TFeather.Create();
      FRecord := TFeather.Create();
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
          WriteDiff(tbFeathers, FOldRecord, Feather, EditSourceStr);
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
        aDataSet.Locate(COL_FEATHER_ID, Feather.Id, []);
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
      FOldRecord := TVideoData.Create();
      FRecord := TVideoData.Create();
      FRepo.Hydrate(aDataSet, FOldRecord);
      FRecord.Assign(FOldRecord);
      EditSourceStr := rsEditedByForm;
    end;
    Video := FRecord;
    case aMasterType of
      tbIndividuals:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
      end;
      tbCaptures:
      begin
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        CaptureId := aMaster.FieldByName(COL_CAPTURE_ID).AsInteger;
      end;
      tbSurveys:
      begin
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
      end;
      tbSightings:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        IndividualId := aMaster.FieldByName(COL_INDIVIDUAL_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        SurveyId := aMaster.FieldByName(COL_SURVEY_ID).AsInteger;
        SightingId := aMaster.FieldByName(COL_SIGHTING_ID).AsInteger;
      end;
      tbNests:
      begin
        TaxonId := aMaster.FieldByName(COL_TAXON_ID).AsInteger;
        LocalityId := aMaster.FieldByName(COL_LOCALITY_ID).AsInteger;
        NestId := aMaster.FieldByName(COL_NEST_ID).AsInteger;
      end;
      tbNestRevisions:
      begin
        //TaxonId := aMaster.FieldByName('taxon_id').AsInteger;
        //LocalityId := aMaster.FieldByName('locality_id').AsInteger;
        NestId := aMaster.FieldByName(COL_NEST_ID).AsInteger;
        NestRevisionId := aMaster.FieldByName(COL_NEST_REVISION_ID).AsInteger;
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
          WriteDiff(tbVideos, FOldRecord, Video, EditSourceStr);
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
        aDataSet.Locate(COL_VIDEO_ID, Video.Id, []);
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

