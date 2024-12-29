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
  function EditCapture(aDataSet: TDataSet; aIndividual: Integer = 0; IsNew: Boolean = False): Boolean;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtInstitution, edtInstitution);
  with edtInstitution do
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
    FreeAndNil(edtInstitution);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditPerson(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtPerson, edtPerson);
  with edtPerson do
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
    FreeAndNil(edtPerson);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditProject(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtProject, edtProject);
  with edtProject do
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
    FreeAndNil(edtProject);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtPermit, edtPermit);
  with edtPermit do
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
    FreeAndNil(edtPermit);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtBands, edtBands);
  with edtBands do
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
    FreeAndNil(edtBands);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditIndividual(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtIndividual, edtIndividual);
  with edtIndividual do
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
    FreeAndNil(edtIndividual);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditCapture(aDataSet: TDataSet; aIndividual: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  edtCapture := TedtCapture.Create(nil);
  with edtCapture do
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
    FreeAndNil(edtCapture);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtNest, edtNest);
  with edtNest do
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
    FreeAndNil(edtNest);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditNestOwner(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtNestOwner, edtNestOwner);
  with edtNestOwner do
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
    FreeAndNil(edtNestOwner);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditNestRevision(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtNestRevision, edtNestRevision);
  with edtNestRevision do
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
    FreeAndNil(edtNestRevision);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditEgg(aDataSet: TDataSet; aNest: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtEgg, edtEgg);
  with edtEgg do
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
    FreeAndNil(edtEgg);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtSighting, edtSighting);
  with edtSighting do
  try
    dsLink.DataSet := aDataSet;
    if aDataSet <> DMG.qSightings then
      pSurvey.Visible := True;
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
    FreeAndNil(edtSighting);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtImageInfo, edtImageInfo);
  with edtImageInfo do
  try
    edtImageInfo.dsLink.DataSet := aDataSet;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      aDataSet.Insert;
      case aMasterType of
        //tbNone: ;
        //tbUsers: ;
        //tbRecordHistory: ;
        //tbRecordVerifications: ;
        //tbGazetteer: ;
        //tbSamplingPlots: ;
        //tbPermanentNets: ;
        //tbInstitutions: ;
        //tbPeople: ;
        //tbProjects: ;
        //tbProjectTeams: ;
        //tbPermits: ;
        //tbTaxonRanks: ;
        //tbZooTaxa: ;
        //tbBotanicTaxa: ;
        //tbBands: ;
        //tbBandHistory: ;
        tbIndividuals:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
        end;
        tbCaptures:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
          aDataSet.FieldByName('survey_id').AsInteger := aMaster.FieldByName('survey_id').AsInteger;
        end;
        //tbMolts: ;
        tbNests:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        end;
        //tbNestOwners: ;
        tbNestRevisions:
        begin
          aDataSet.FieldByName('nest_id').AsInteger := aMaster.FieldByName('nest_id').AsInteger;
        end;
        tbEggs:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          aDataSet.FieldByName('nest_id').AsInteger := aMaster.FieldByName('nest_id').AsInteger;
        end;
        //tbMethods: ;
        //tbExpeditions: ;
        tbSurveys:
        begin
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        end;
        //tbSurveyTeams: ;
        //tbNetsEffort: ;
        //tbWeatherLogs: ;
        tbSightings:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
          aDataSet.FieldByName('survey_id').AsInteger := aMaster.FieldByName('survey_id').AsInteger;
        end;
        tbSpecimens:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        end;
        //tbSamplePreps: ;
        //tbSpecimenCollectors: ;
        //tbImages: ;
        //tbAudioLibrary: ;
      end;
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
    FreeAndNil(edtImageInfo);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditAudioInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtAudioInfo, edtAudioInfo);
  with edtAudioInfo do
  try
    edtAudioInfo.dsLink.DataSet := aDataSet;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      aDataSet.Insert;
      case aMasterType of
        //tbNone: ;
        //tbUsers: ;
        //tbRecordHistory: ;
        //tbRecordVerifications: ;
        //tbGazetteer: ;
        //tbSamplingPlots: ;
        //tbPermanentNets: ;
        //tbInstitutions: ;
        //tbPeople: ;
        //tbProjects: ;
        //tbProjectTeams: ;
        //tbPermits: ;
        //tbTaxonRanks: ;
        //tbZooTaxa: ;
        //tbBotanicTaxa: ;
        //tbBands: ;
        //tbBandHistory: ;
        tbIndividuals:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
        end;
        //tbCaptures:
        //begin
        //  aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
        //  aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
        //  aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        //  aDataSet.FieldByName('survey_id').AsInteger := aMaster.FieldByName('survey_id').AsInteger;
        //end;
        //tbMolts: ;
        tbNests:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        end;
        //tbNestOwners: ;
        //tbNestRevisions: ;
        //tbEggs: ;
        //tbMethods: ;
        //tbExpeditions: ;
        //tbSurveys:
        //begin
        //  aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        //end;
        //tbSurveyTeams: ;
        //tbNetsEffort: ;
        //tbWeatherLogs: ;
        tbSightings:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
          //aDataSet.FieldByName('survey_id').AsInteger := aMaster.FieldByName('survey_id').AsInteger;
        end;
        tbSpecimens:
        begin
          aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        end;
        //tbSamplePreps: ;
        //tbSpecimenCollectors: ;
        //tbImages: ;
        //tbAudioLibrary: ;
      end;
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
    FreeAndNil(edtAudioInfo);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditDocInfo(aDataSet, aMaster: TDataSet; aMasterType: TTableType; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtDocumentInfo, edtDocumentInfo);
  with edtDocumentInfo do
  try
    edtDocumentInfo.dsLink.DataSet := aDataSet;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
    if IsNew then
    begin
      aDataSet.Insert;
      case aMasterType of
        //tbNone: ;
        //tbUsers: ;
        //tbRecordHistory: ;
        //tbRecordVerifications: ;
        //tbGazetteer: ;
        //tbSamplingPlots: ;
        //tbPermanentNets: ;
        //tbInstitutions: ;
        //tbPeople: ;
        //tbProjects: ;
        //tbProjectTeams: ;
        //tbPermits: ;
        //tbTaxonRanks: ;
        //tbZooTaxa: ;
        //tbBotanicTaxa: ;
        //tbBands: ;
        //tbBandHistory: ;
        //tbIndividuals:
        //begin
        //  aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
        //end;
        tbCaptures:
        begin
          //aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          //aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
          aDataSet.FieldByName('survey_id').AsInteger := aMaster.FieldByName('survey_id').AsInteger;
        end;
        //tbMolts: ;
        //tbNests:
        //begin
        //  aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
        //  aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        //end;
        //tbNestOwners: ;
        //tbNestRevisions: ;
        //tbEggs: ;
        //tbMethods: ;
        //tbExpeditions: ;
        //tbSurveys:
        //begin
        //  aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        //end;
        //tbSurveyTeams: ;
        //tbNetsEffort: ;
        //tbWeatherLogs: ;
        tbSightings:
        begin
          //aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          //aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
          aDataSet.FieldByName('survey_id').AsInteger := aMaster.FieldByName('survey_id').AsInteger;
        end;
        tbSpecimens:
        begin
          //aDataSet.FieldByName('taxon_id').AsInteger := aMaster.FieldByName('taxon_id').AsInteger;
          aDataSet.FieldByName('individual_id').AsInteger := aMaster.FieldByName('individual_id').AsInteger;
          //aDataSet.FieldByName('locality_id').AsInteger := aMaster.FieldByName('locality_id').AsInteger;
        end;
        //tbSamplePreps: ;
        //tbSpecimenCollectors: ;
        //tbImages: ;
        //tbAudioLibrary: ;
      end;
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
    FreeAndNil(edtDocumentInfo);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
    if ShowModal = mrOk then
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

