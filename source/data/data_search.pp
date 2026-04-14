{ Xolmis Data Search library

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

unit data_search;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, RegExpr, StrUtils, data_types;

  { SQL Select and filtering }
  function GetModifier(aModifier: String): TFilterValue;

  procedure SetSelectSQL(const aSQL: TStrings; aTable: TTableType; var aAlias: String);

  procedure SetMethodsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetSpecimensSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetSurveysSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetExpeditionsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetSightingsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetSurveyTeamSQL(const aSQL: TStrings; aSurvey: Integer);
  procedure SetBotanicTaxaSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetGazetteerSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetSamplingPlotsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetInstitutionsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetNestsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetNestRevisionsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetEggsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetIndividualsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetCapturesSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetFeathersSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetTaxonRanksSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetPeopleSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetProjectsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetProjectTeamSQL(const aSQL: TStrings; aProject: Integer);
  procedure SetPermitsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetZooTaxaSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetBandsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetUsersSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');
  procedure SetConnectionsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');

implementation

uses utils_global, data_providers;

{
 ----------------------------------------------------------------------------
   SQL Select and filtering
 ----------------------------------------------------------------------------
}

function GetModifier(aModifier: String): TFilterValue;
begin
  Result := fvNone;

  if MatchStr(aModifier, HASHTAG_ALL) then { #tudo }
    Result := fvAll
  else
  if MatchStr(aModifier, HASHTAG_MARKED) then { #marcados }
    Result := fvMarked
  else
  if MatchStr(aModifier, HASHTAG_UNMARKED) then { #naomarcados }
    Result := fvUnmarked
  else
  if MatchStr(aModifier, HASHTAG_DELETED) then { #lixo }
    Result := fvDeleted
  else
  if MatchStr(aModifier, HASHTAG_PRINT_QUEUE) then { #fila }
    Result := fvQueued;
end;

// ----------------------------------------------------------------------------
// Load default SQL Select for each table
// ----------------------------------------------------------------------------

procedure SetSelectSQL(const aSQL: TStrings; aTable: TTableType; var aAlias: String);
begin
  //aAlias := '';
  case aTable of
    tbNone: ;
    tbUsers:
      begin
        SetUsersSQL(aSQL, fvNone);
      end;
    //tbRecordHistory: ;
    tbPermits:
      begin
        SetPermitsSQL(aSQL, fvNone);
        //aAlias:= EmptyStr;
      end;
    tbGazetteer:
      begin
        SetGazetteerSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbBotanicTaxa:
      begin
        SetBotanicTaxaSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbNests:
      begin
        SetNestsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbNestRevisions:
      begin
        SetNestRevisionsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbEggs:
      begin
        SetEggsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbSamplingPlots:
      begin
        SetSamplingPlotsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbTaxonRanks:
      SetTaxonRanksSQL(aSQL, fvNone);
    tbZooTaxa:
      begin
        SetZooTaxaSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbProjects:
      begin
        SetProjectsSQL(aSQL, fvNone);
        aAlias:= EmptyStr;
      end;
    tbProjectTeams:
      ;
    tbInstitutions:
      begin
        SetInstitutionsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbPeople:
      begin
        SetPeopleSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbExpeditions:
      SetExpeditionsSQL(aSQL, fvNone);
    tbSurveys:
      begin
        SetSurveysSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbMethods:
      begin
        SetMethodsSQL(aSQL, fvNone);
        aAlias := EmptyStr;
      end;
    tbSurveyTeams:
      ;
    tbNetsEffort:
      ;
    tbSightings:
      begin
        SetSightingsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbSpecimens:
      begin
        SetSpecimensSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbSamplePreps:
      ;
    tbPermanentNets:
      ;
    tbBands:
      begin
        SetBandsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbIndividuals:
      begin
        SetIndividualsSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbCaptures:
      begin
        SetCapturesSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbFeathers:
      begin
        SetFeathersSQL(aSQL, fvNone);
        //aAlias := TABLE_ALIASES[aTable] + '.';
      end;
    tbImages:
      ;
    tbAudioLibrary:
      ;
  end;

end;

procedure SetMethodsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = '';
  aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Methods.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Methods.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Methods.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Methods.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Methods.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetSpecimensSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Specimens.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Specimens.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Specimens.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Specimens.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Specimens.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetSurveysSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Surveys.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Surveys.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Surveys.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Surveys.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Surveys.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetExpeditionsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Expeditions.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Expeditions.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Expeditions.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Expeditions.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Expeditions.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetSightingsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Sightings.SelectAll(swcNone, tbNone));
      fvReset:
        Add(xProvider.Sightings.SelectAll(swcActiveEmpty, tbNone));
      fvAll:
        Add(xProvider.Sightings.SelectAll(swcActiveAll, tbNone));
      fvMarked:
        Add(xProvider.Sightings.SelectAll(swcActiveMarked, tbNone));
      fvDeleted:
        Add(xProvider.Sightings.SelectAll(swcInactive, tbNone));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetSurveyTeamSQL(const aSQL: TStrings; aSurvey: Integer);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;

    if aSurvey > 0 then
    begin
      Add(xProvider.SurveyTeams.SelectAll(swcNone));
      Add('WHERE (st.active_status = 1) AND (st.survey_id = ' + IntToStr(aSurvey) + ')')
    end
    else
      Add(xProvider.SurveyTeams.SelectAll(swcActiveParent));

    AD := 'ASC';
    Add('ORDER BY st.visitor ASC, person_name ' {COLLATE pt_BR '} + AD);
  end;
end;

procedure SetBotanicTaxaSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.BotanicalTaxa.SelectAll(swcNone));
      fvReset:
        Add(xProvider.BotanicalTaxa.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.BotanicalTaxa.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.BotanicalTaxa.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.BotanicalTaxa.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetGazetteerSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Gazetteer.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Gazetteer.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Gazetteer.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Gazetteer.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Gazetteer.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetSamplingPlotsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.SamplingPlots.SelectAll(swcNone));
      fvReset:
        Add(xProvider.SamplingPlots.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.SamplingPlots.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.SamplingPlots.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.SamplingPlots.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetInstitutionsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Institutions.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Institutions.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Institutions.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Institutions.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Institutions.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetNestsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = '';
  aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Nests.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Nests.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Nests.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Nests.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Nests.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetNestRevisionsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String; aDirection: String);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.NestRevisions.SelectAll(swcNone));
      fvReset:
        Add(xProvider.NestRevisions.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.NestRevisions.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.NestRevisions.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.NestRevisions.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetEggsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String; aDirection: String);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Eggs.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Eggs.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Eggs.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Eggs.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Eggs.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetIndividualsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Individuals.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Individuals.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Individuals.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Individuals.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Individuals.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetCapturesSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Captures.SelectAll(swcNone, tbNone));
      fvReset:
        Add(xProvider.Captures.SelectAll(swcActiveEmpty, tbNone));
      fvAll:
        Add(xProvider.Captures.SelectAll(swcActiveAll, tbNone));
      fvMarked:
        Add(xProvider.Captures.SelectAll(swcActiveMarked, tbNone));
      fvDeleted:
        Add(xProvider.Captures.SelectAll(swcInactive, tbNone));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetTaxonRanksSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = '';
  aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.TaxonRanks.SelectAll(swcNone));
      fvReset:
        Add(xProvider.TaxonRanks.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.TaxonRanks.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.TaxonRanks.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.TaxonRanks.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetPeopleSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.People.SelectAll(swcNone));
      fvReset:
        Add(xProvider.People.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.People.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.People.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.People.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetProjectsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Projects.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Projects.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Projects.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Projects.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Projects.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetProjectTeamSQL(const aSQL: TStrings; aProject: Integer);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;

    if aProject > 0 then
    begin
      Add(xProvider.ProjectTeams.SelectAll(swcNone));
      Add('WHERE (pt.active_status = 1) AND (pt.project_id = ' + IntToStr(aProject) + ')')
    end
    else
      Add(xProvider.ProjectTeams.SelectAll(swcActiveParent));

    AD := 'ASC';
    Add('ORDER BY pt.project_manager ASC, person_name ' {COLLATE pt_BR '} + AD);
  end;
end;

procedure SetPermitsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String; aDirection: String);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Permits.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Permits.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Permits.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Permits.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Permits.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetZooTaxaSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = '';
  aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.ZooTaxa.SelectAll(swcNone));
      fvReset:
        Add(xProvider.ZooTaxa.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.ZooTaxa.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.ZooTaxa.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.ZooTaxa.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetBandsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = '';
  aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Bands.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Bands.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Bands.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Bands.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Bands.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetUsersSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Users.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Users.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Users.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Users.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Users.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetFeathersSQL(
  const aSQL: TStrings; aFilter: TFilterValue; aSorting: String; aDirection: String
  );
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Feathers.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Feathers.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Feathers.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Feathers.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Feathers.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

procedure SetConnectionsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String; aDirection: String);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    case aFilter of
      fvNone:
        Add(xProvider.Connections.SelectAll(swcNone));
      fvReset:
        Add(xProvider.Connections.SelectAll(swcActiveEmpty));
      fvAll:
        Add(xProvider.Connections.SelectAll(swcActiveAll));
      fvMarked:
        Add(xProvider.Connections.SelectAll(swcActiveMarked));
      fvDeleted:
        Add(xProvider.Connections.SelectAll(swcInactive));
    end;
    if Trim(aSorting) <> '' then
    begin
      if aDirection = '' then
        AD := 'ASC'
      else
        AD := aDirection;
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} AD);
    end;
  end;
end;

end.

