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

unit cbs_editdialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, System.UITypes, cbs_datatypes;

  function EditConnection(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;

  function EditMethod(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSite(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSamplingPlot(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditPermanentNet(aDataSet: TDataSet; aNetStation: Integer; IsNew: Boolean = False): Boolean;
  function EditInstitution(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditPerson(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditProject(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditProjectMember(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditPermit(aDataSet: TDataSet; aProject: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditBotanicTaxon(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditBand(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditIndividual(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditCapture(aDataSet: TDataSet; aIndividual: Integer = 0; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditMolt(aDataSet: TDataSet; aIndividual: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNest(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditNestOwner(aDataSet: TDataSet; aNest: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNestRevision(aDataSet: TDataSet; aNest: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditEgg(aDataSet: TDataSet; aNest: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditExpedition(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSurvey(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSurveyMember(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditNetEffort(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditWeatherLog(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditVegetation(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSighting(aDataSet: TDataSet; aSurvey: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSpecimen(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditCollector(aDataSet: TDataSet; aSpecimen: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditSamplePrep(aDataSet: TDataSet; aSpecimen: Integer = 0; IsNew: Boolean = False): Boolean;
  function EditImageInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditAudioInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditDocInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean = False): Boolean;
  function EditUser(IsNew: Boolean = False): Boolean;
  function ChangeUserPassword(IsNew: Boolean = False): Boolean;

implementation

uses
  cbs_locale, cbs_global, cbs_permissions, cbs_finddialogs, cbs_dialogs, cbs_gis, cbs_sampling, cbs_botany,
  cbs_breeding, cbs_birds, cbs_entities, cbs_media,
  udm_main, udm_grid, udlg_changepassword, uedt_user, uedt_site, uedt_bands, uedt_expedition, uedt_capture,
  uedt_survey, uedt_samplingplot, uedt_institution, uedt_person, uedt_botanictaxon, uedt_individual,
  uedt_nest, uedt_egg, uedt_molt, uedt_nestrevision, uedt_neteffort, uedt_permanentnet, uedt_sighting,
  uedt_method, uedt_weatherlog, uedt_project, uedt_permit, uedt_specimen, uedt_sampleprep, uedt_nestowner,
  uedt_imageinfo, uedt_audioinfo, uedt_documentinfo, uedt_vegetation, uedt_database;

function EditMethod(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TMethod;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Method');
  Application.CreateForm(TedtMethod, edtMethod);
  with edtMethod do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TMethod.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TMethod.Create(aDataSet.FieldByName('method_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Method := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Method.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtMethod);
    LogInfo('CLOSE EDIT DIALOG: Method');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditSite(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TSite;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Gazetteer');
  Application.CreateForm(TedtSite, edtSite);
  with edtSite do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSite.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TSite.Create(aDataSet.FieldByName('site_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Site := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Site.Save;
      //aDataSet.Post;
    end;
    //else
      //aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtSite);
    LogInfo('CLOSE EDIT DIALOG: Gazetteer');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditSamplingPlot(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TSamplingPlot;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Sampling plot');
  Application.CreateForm(TedtSamplingPlot, edtSamplingPlot);
  with edtSamplingPlot do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSamplingPlot.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TSamplingPlot.Create(aDataSet.FieldByName('sampling_plot_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    SamplingPlot := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      SamplingPlot.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtSamplingPlot);
    LogInfo('CLOSE EDIT DIALOG: Sampling plot');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditPermanentNet(aDataSet: TDataSet; aNetStation: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TPermanentNet;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Permanent net');
  Application.CreateForm(TedtPermanentNet, edtPermanentNet);
  with edtPermanentNet do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TPermanentNet.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TPermanentNet.Create(aDataSet.FieldByName('permanent_net_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    PermanentNet := FRecord;
    Result := ShowModal = mrOk;
    if Result then
    PermanentNet.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtPermanentNet);
    LogInfo('CLOSE EDIT DIALOG: Permanent net');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditInstitution(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TInstitution;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Institution');
  Application.CreateForm(TedtInstitution, edtInstitution);
  with edtInstitution do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TInstitution.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TInstitution.Create(aDataSet.FieldByName('institution_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Institution := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Institution.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtInstitution);
    LogInfo('CLOSE EDIT DIALOG: Institution');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditPerson(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TPerson;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Person');
  Application.CreateForm(TedtPerson, edtPerson);
  with edtPerson do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TPerson.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TPerson.Create(aDataSet.FieldByName('person_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Person := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Person.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtPerson);
    LogInfo('CLOSE EDIT DIALOG: Person');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditProject(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TProject;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Project');
  Application.CreateForm(TedtProject, edtProject);
  with edtProject do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TProject.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TProject.Create(aDataSet.FieldByName('project_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Project := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Project.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtProject);
    LogInfo('CLOSE EDIT DIALOG: Project');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditProjectMember(aDataSet: TDataSet; aProject: Integer; IsNew: Boolean): Boolean;
var
  MemberKey: Integer;
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Result := False;

  if FindDlg(tbPeople, nil, MemberKey) then
  begin
    try
      if IsNew then
      begin
        aDataSet.Insert;
        EditSourceStr := rsInsertedByForm;
      end else
      begin
        aDataSet.Edit;
        EditSourceStr := rsEditedByForm;
      end;
      aDataSet.FieldByName('person_id').AsInteger := MemberKey;
      aDataSet.Post;
      Result := True;
    except
      aDataSet.Cancel;
    end;
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditPermit(aDataSet: TDataSet; aProject: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TPermit;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Permit');
  Application.CreateForm(TedtPermit, edtPermit);
  with edtPermit do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TPermit.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TPermit.Create(aDataSet.FieldByName('permit_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Permit := FRecord;
    ProjectId := aProject;
    Result := ShowModal = mrOk;
    if Result then
      Permit.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtPermit);
    LogInfo('CLOSE EDIT DIALOG: Permit');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditBotanicTaxon(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TBotanicTaxon;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Botanic taxon');
  Application.CreateForm(TedtBotanicTaxon, edtBotanicTaxon);
  with edtBotanicTaxon do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TBotanicTaxon.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TBotanicTaxon.Create(aDataSet.FieldByName('taxon_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Taxon := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Taxon.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtBotanicTaxon);
    LogInfo('CLOSE EDIT DIALOG: Botanic taxon');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditBand(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TBand;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Band');
  Application.CreateForm(TedtBands, edtBands);
  with edtBands do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TBand.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TBand.Create(aDataSet.FieldByName('band_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Band := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Band.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtBands);
    LogInfo('CLOSE EDIT DIALOG: Band');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditIndividual(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TIndividual;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Individual');
  Application.CreateForm(TedtIndividual, edtIndividual);
  with edtIndividual do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TIndividual.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TIndividual.Create(aDataSet.FieldByName('individual_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Individual := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Individual.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtIndividual);
    LogInfo('CLOSE EDIT DIALOG: Individual');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditCapture(aDataSet: TDataSet; aIndividual: Integer; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TCapture;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Capture');
  edtCapture := TedtCapture.Create(nil);
  with edtCapture do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TCapture.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TCapture.Create(aDataSet.FieldByName('capture_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Capture := FRecord;
    IndividualId := aIndividual;
    pIndividual.Visible := aIndividual = 0;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
      Capture.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtCapture);
    LogInfo('CLOSE EDIT DIALOG: Capture');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditMolt(aDataSet: TDataSet; aIndividual: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtMolt, edtMolt);
  with edtMolt do
  try
    dsLink.DataSet := aDataSet;
    if IsNew then
    begin
      aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Result := ShowModal = mrOk;
    if Result then
      aDataSet.Post
    else
      aDataSet.Cancel;
  finally
    FreeAndNil(edtMolt);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditNest(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TNest;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Nest');
  Application.CreateForm(TedtNest, edtNest);
  with edtNest do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNest.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TNest.Create(aDataSet.FieldByName('nest_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Nest := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Nest.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtNest);
    LogInfo('CLOSE EDIT DIALOG: Nest');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditNestOwner(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
//  CloseQueryAfter: Boolean;
  FRecord: TNestOwner;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Nest owner');
  Application.CreateForm(TedtNestOwner, edtNestOwner);
  with edtNestOwner do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNestOwner.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TNestOwner.Create(aDataSet.FieldByName('nest_owner_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    NestOwner := FRecord;
    NestId := aNest;
    Result := ShowModal = mrOk;
    if Result then
      NestOwner.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtNestOwner);
    LogInfo('CLOSE EDIT DIALOG: Nest owner');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditNestRevision(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TNestRevision;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Nest revision');
  Application.CreateForm(TedtNestRevision, edtNestRevision);
  with edtNestRevision do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNestRevision.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TNestRevision.Create(aDataSet.FieldByName('nest_revision_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    NestRevision := FRecord;
    NestId := aNest;
    Result := ShowModal = mrOk;
    if Result then
      NestRevision.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtNestRevision);
    LogInfo('CLOSE EDIT DIALOG: Nest revision');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditEgg(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TEgg;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Egg');
  Application.CreateForm(TedtEgg, edtEgg);
  with edtEgg do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TEgg.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TEgg.Create(aDataSet.FieldByName('egg_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Egg := FRecord;
    NestId := aNest;
    Result := ShowModal = mrOk;
    if Result then
      Egg.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtEgg);
    LogInfo('CLOSE EDIT DIALOG: Egg');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditExpedition(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TExpedition;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  Result := False;

  LogInfo('OPEN EDIT DIALOG: Expedition');
  Application.CreateForm(TedtExpedition, edtExpedition);
  with edtExpedition do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TExpedition.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TExpedition.Create(aDataSet.FieldByName('expedition_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Expedition := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Expedition.Save;
    //begin
    //  aDataSet.Post;
    //  Result := True;
    //end
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtExpedition);
    LogInfo('CLOSE EDIT DIALOG: Expedition');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditSurvey(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TSurvey;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Survey');
  Application.CreateForm(TedtSurvey, edtSurvey);
  with edtSurvey do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TSurvey.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TSurvey.Create(aDataSet.FieldByName('survey_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Survey := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Survey.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtSurvey);
    LogInfo('CLOSE EDIT DIALOG: Survey');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditSurveyMember(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  MemberKey: Integer;
  //CloseQueryAfter: Boolean;
  FRecord: TSurveyMember;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  Result := False;
  MemberKey := 0;

  if FindDlg(tbPeople, nil, MemberKey) then
  begin
    try
      if IsNew then
      begin
        FRecord := TSurveyMember.Create();
        //aDataSet.Insert;
        EditSourceStr := rsInsertedByForm;
      end else
      begin
        FRecord := TSurveyMember.Create(aDataSet.FieldByName('survey_member_id').AsInteger);
        //aDataSet.Edit;
        EditSourceStr := rsEditedByForm;
      end;
      FRecord.PersonId := MemberKey;
      FRecord.Save;
      //aDataSet.FieldByName('person_id').AsInteger := MemberKey;
      //aDataSet.Post;
      aDataSet.Refresh;
      Result := True;
    //except
    //  aDataSet.Cancel;
    //end;
    finally
      FRecord.Free;
    end;
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditNetEffort(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TNetEffort;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Net effort');
  Application.CreateForm(TedtNetEffort, edtNetEffort);
  with edtNetEffort do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TNetEffort.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TNetEffort.Create(aDataSet.FieldByName('net_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    NetEffort := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      NetEffort.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtNetEffort);
    LogInfo('CLOSE EDIT DIALOG: Net effort');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditWeatherLog(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TWeatherLog;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Weather log');
  Application.CreateForm(TedtWeatherLog, edtWeatherLog);
  with edtWeatherLog do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TWeatherLog.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TWeatherLog.Create(aDataSet.FieldByName('weather_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    WeatherLog := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      WeatherLog.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtWeatherLog);
    LogInfo('CLOSE EDIT DIALOG: Weather log');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditSighting(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TSighting;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Sighting');
  Application.CreateForm(TedtSighting, edtSighting);
  with edtSighting do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if aDataSet <> DMG.qSightings then
      pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TSighting.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TSighting.Create(aDataSet.FieldByName('sighting_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Sighting := FRecord;
    SurveyId := aSurvey;
    Result := ShowModal = mrOk;
    if Result then
      Sighting.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtSighting);
    LogInfo('CLOSE EDIT DIALOG: Sighting');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditSpecimen(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TSpecimen;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Specimen');
  Application.CreateForm(TedtSpecimen, edtSpecimen);
  with edtSpecimen do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    //if aDataSet <> DMG.qSpecimens then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TSpecimen.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TSpecimen.Create(aDataSet.FieldByName('specimen_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Specimen := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Specimen.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtSpecimen);
    LogInfo('CLOSE EDIT DIALOG: Specimen');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditCollector(aDataSet: TDataSet; aSpecimen: Integer; IsNew: Boolean): Boolean;
var
  MemberKey: Integer;
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Result := False;

  if FindDlg(tbPeople, nil, MemberKey) then
  begin
    try
      if IsNew then
      begin
        aDataSet.Insert;
        EditSourceStr := rsInsertedByForm;
      end else
      begin
        aDataSet.Edit;
        EditSourceStr := rsEditedByForm;
      end;
      aDataSet.FieldByName('person_id').AsInteger := MemberKey;
      aDataSet.Post;
      Result := True;
    except
      aDataSet.Cancel;
    end;
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditSamplePrep(aDataSet: TDataSet; aSpecimen: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TSamplePrep;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Sample prep');
  Application.CreateForm(TedtSamplePrep, edtSamplePrep);
  with edtSamplePrep do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TSamplePrep.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TSamplePrep.Create(aDataSet.FieldByName('sample_prep_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    SamplePrep := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      SamplePrep.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtSamplePrep);
    LogInfo('CLOSE EDIT DIALOG: Sample prep');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditUser(IsNew: Boolean): Boolean;
begin
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
        if ChangeUserPassword(IsNew) then
          DMM.qUsers.Post
        else
          DMM.qUsers.Cancel;
    end
    else
      DMM.qUsers.Cancel;
  finally
    FreeAndNil(edtUser);
  end;
end;

function ChangeUserPassword(IsNew: Boolean): Boolean;
begin
  Result := False;

  if not IsNew then
    if not UserLogin(DMM.qUsers.FieldByName('user_id').AsInteger) then
      Exit;

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
    end else
    begin
      if not IsNew then
        DMM.qUsers.Cancel;
    end;
  finally
    FreeAndNil(dlgChangePassword);
  end;
end;

function EditImageInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TImageData;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Image');
  Application.CreateForm(TedtImageInfo, edtImageInfo);
  with edtImageInfo do
  try
    edtImageInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TImageData.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TImageData.Create(aDataSet.FieldByName('image_id').AsInteger);
      //aDataSet.Edit;
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
      Image.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtImageInfo);
    LogInfo('CLOSE EDIT DIALOG: Image');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditAudioInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TAudioData;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Audio');
  Application.CreateForm(TedtAudioInfo, edtAudioInfo);
  with edtAudioInfo do
  try
    edtAudioInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TAudioData.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TAudioData.Create(aDataSet.FieldByName('audio_id').AsInteger);
      //aDataSet.Edit;
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
      AudioRecording.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtAudioInfo);
    LogInfo('CLOSE EDIT DIALOG: Audio');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditDocInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TDocumentData;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Document');
  Application.CreateForm(TedtDocumentInfo, edtDocumentInfo);
  with edtDocumentInfo do
  try
    edtDocumentInfo.dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      FRecord := TDocumentData.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TDocumentData.Create(aDataSet.FieldByName('document_id').AsInteger);
      //aDataSet.Edit;
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
      Document.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtDocumentInfo);
    LogInfo('CLOSE EDIT DIALOG: Document');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
end;

function EditVegetation(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  //CloseQueryAfter: Boolean;
  FRecord: TVegetation;
begin
  //CloseQueryAfter := False;
  //if not aDataSet.Active then
  //begin
  //  aDataSet.Open;
  //  CloseQueryAfter := True;
  //end;

  LogInfo('OPEN EDIT DIALOG: Vegetation');
  Application.CreateForm(TedtVegetation, edtVegetation);
  with edtVegetation do
  try
    dsLink.DataSet := aDataSet;
    IsNewRecord := IsNew;
    if IsNew then
    begin
      FRecord := TVegetation.Create();
      //aDataSet.Insert;
      EditSourceStr := rsInsertedByForm;
    end else
    begin
      FRecord := TVegetation.Create(aDataSet.FieldByName('vegetation_id').AsInteger);
      //aDataSet.Edit;
      EditSourceStr := rsEditedByForm;
    end;
    Vegetation := FRecord;
    Result := ShowModal = mrOk;
    if Result then
      Vegetation.Save;
    //  aDataSet.Post
    //else
    //  aDataSet.Cancel;
  finally
    FRecord.Free;
    FreeAndNil(edtVegetation);
    LogInfo('CLOSE EDIT DIALOG: Vegetation');
  end;

  //if CloseQueryAfter then
  //  aDataSet.Close;
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
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

end.

