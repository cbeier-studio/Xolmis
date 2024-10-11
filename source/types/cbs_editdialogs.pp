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
  Classes, SysUtils, Forms, DB, System.UITypes, cbs_datatypes;

  function EditMethod(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditSite(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
  function EditNetStation(aDataSet: TDataSet; IsNew: Boolean = False): Boolean;
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
  cbs_locale, cbs_global, cbs_permissions, cbs_finddialogs,
  udm_main, udm_grid, udlg_changepassword, uedt_user, uedt_site, uedt_bands, uedt_expedition, uedt_capture,
  uedt_survey, uedt_netstation, uedt_institution, uedt_person, uedt_botanictaxon, uedt_individual,
  uedt_nest, uedt_egg, uedt_molt, uedt_nestrevision, uedt_neteffort, uedt_permanentnet, uedt_sighting,
  uedt_method, uedt_weatherlog, uedt_project, uedt_permit, uedt_specimen, uedt_sampleprep, uedt_nestowner,
  uedt_imageinfo, uedt_audioinfo, uedt_documentinfo;

function EditMethod(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtMethod, edtMethod);
  with edtMethod do
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
    FreeAndNil(edtMethod);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditSite(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtSite, edtSite);
  with edtSite do
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
    FreeAndNil(edtSite);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditNetStation(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtNetStation, edtNetStation);
  with edtNetStation do
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
    FreeAndNil(edtNetStation);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditPermanentNet(aDataSet: TDataSet; aNetStation: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtPermanentNet, edtPermanentNet);
  with edtPermanentNet do
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
    FreeAndNil(edtPermanentNet);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtBotanicTaxon, edtBotanicTaxon);
  with edtBotanicTaxon do
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
    FreeAndNil(edtBotanicTaxon);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Result := False;

  Application.CreateForm(TedtExpedition, edtExpedition);
  with edtExpedition do
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
    if ShowModal = mrOk then
    begin
      aDataSet.Post;
      Result := True;
    end
    else
      aDataSet.Cancel;
  finally
    FreeAndNil(edtExpedition);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditSurvey(aDataSet: TDataSet; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtSurvey, edtSurvey);
  with edtSurvey do
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
    FreeAndNil(edtSurvey);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditSurveyMember(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
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

function EditNetEffort(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtNetEffort, edtNetEffort);
  with edtNetEffort do
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
    FreeAndNil(edtNetEffort);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
end;

function EditWeatherLog(aDataSet: TDataSet; aSurvey: Integer; IsNew: Boolean): Boolean;
var
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtWeatherLog, edtWeatherLog);
  with edtWeatherLog do
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
    FreeAndNil(edtWeatherLog);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtSpecimen, edtSpecimen);
  with edtSpecimen do
  try
    dsLink.DataSet := aDataSet;
    //if aDataSet <> DMG.qSpecimens then
    //  pSurvey.Visible := True;
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
    FreeAndNil(edtSpecimen);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
  CloseQueryAfter: Boolean;
begin
  CloseQueryAfter := False;
  if not aDataSet.Active then
  begin
    aDataSet.Open;
    CloseQueryAfter := True;
  end;

  Application.CreateForm(TedtSamplePrep, edtSamplePrep);
  with edtSamplePrep do
  try
    dsLink.DataSet := aDataSet;
    //if aDataSet <> DMG.qSamplePreps then
    //  pSurvey.Visible := True;
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
    FreeAndNil(edtSamplePrep);
  end;

  if CloseQueryAfter then
    aDataSet.Close;
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
        //tbNetStations: ;
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
        //tbNetStations: ;
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
        //tbNetStations: ;
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

end.

