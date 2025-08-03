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

unit cbs_datasearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, RegExpr, StrUtils, cbs_datatypes;

  { Search records (deprecated) }
  //function TableSearch(aQuery: TSQLQuery; aTable: TTableType; aSearch: TSearch;
  //  aQuickFilter: TStrings; aModifier: TRecordStatus; aSorting: TSortedFields;
  //  aWhere: TStrings): Boolean;

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

implementation

uses cbs_global;

{ ----------------------------------------------------------------------------------------- }
{ Search records }
{ ----------------------------------------------------------------------------------------- }

//function TableSearch(aQuery: TSQLQuery; aTable: TTableType; aSearch: TSearch;
//  aQuickFilter: TStrings; aModifier: TRecordStatus; aSorting: TSortedFields;
//  aWhere: TStrings): Boolean;
//var
//  AndWhere, aAlias, aSort, aDir: String;
//  i: Integer;
//  SF: TSortedField;
//begin
//  aAlias := EmptyStr;
//  AndWhere := 'WHERE ';
//  aWhere.Clear;
//  Result := False;
//
//  with aQuery, SQL do
//  begin
//    Close;
//    Clear;
//    SetSelectSQL(SQL, aTable, aAlias);
//
//    // Value typed in the Search field
//    if not aSearch.IsEmpty then
//    begin
//      //aQuery.FilterOptions := [foCaseInsensitive];
//      //aQuery.Filter := aSearch.FilterString;
//      //aQuery.Filtered := True;
//      Add(aSearch.SQLString);
//      aWhere.Add(aSearch.SQLString);
//      AndWhere := 'AND ';
//    end;
//    //else
//    //  aQuery.Filtered := False;
//
//    // Quick filters applied
//    if aQuickFilter.Count > 0 then
//    begin
//      for i := 0 to aQuickFilter.Count - 1 do
//      begin
//        Add(AndWhere + aQuickFilter[i]);
//        aWhere.Add(AndWhere + aQuickFilter[i]);
//        AndWhere := 'AND ';
//      end;
//    end;
//
//    // Record active or not
//    case aModifier.Status of
//      rsAll:
//        ;
//      rsActive:
//        begin
//          Add(AndWhere + '(' + aAlias + 'active_status = 1)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'active_status = 1)');
//          AndWhere := 'AND ';
//        end;
//      rsInactive:
//        begin
//          Add(AndWhere + '(' + aAlias + 'active_status = 0)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'active_status = 0)');
//          AndWhere := 'AND ';
//        end;
//      rsNone:
//        begin
//          Add(AndWhere + '(' + aAlias + 'active_status = -1)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'active_status = -1)');
//          AndWhere := 'AND ';
//        end;
//    end;
//    // Record marked or not
//    case aModifier.Mark of
//      rmAll:
//        ;
//      rmMarked:
//        begin
//          Add(AndWhere + '(' + aAlias + 'marked_status = 1)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'marked_status = 1)');
//          AndWhere := 'AND ';
//        end;
//      rmUnmarked:
//        begin
//          Add(AndWhere + '(' + aAlias + 'marked_status = 0)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'marked_status = 0)');
//          AndWhere := 'AND ';
//        end;
//    end;
//    // Record queued or not
//    case aModifier.Queue of
//      rqAll:
//        ;
//      rqQueued:
//        begin
//          Add(AndWhere + '(' + aAlias + 'queued_status = 1)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'queued_status = 1)');
//          AndWhere := 'AND ';
//        end;
//      rqUnqueued:
//        begin
//          Add(AndWhere + '(' + aAlias + 'queued_status = 0)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'queued_status = 0)');
//          AndWhere := 'AND ';
//        end;
//    end;
//    // Record already exported or not
//    case aModifier.Share of
//      rxAll:
//        ;
//      rxExported:
//        begin
//          Add(AndWhere + '(' + aAlias + 'exported_status = 1)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'exported_status = 1)');
//          AndWhere := 'AND ';
//        end;
//      rxNotExported:
//        begin
//          Add(AndWhere + '(' + aAlias + 'exported_status = 0)');
//          aWhere.Add(AndWhere + '(' + aAlias + 'exported_status = 0)');
//          AndWhere := 'AND ';
//        end;
//    end;
//
//    // Record sorting
//    if aSorting.Count > 0 then
//    begin
//      aSort := '';
//      aDir := '';
//      for i := 0 to (aSorting.Count - 1) do
//      begin
//        SF := aSorting.Items[i];
//        aDir := SortDirections[SF.Direction];
//        //case SF^.Direction of
//        //  sdNone:
//        //    ;
//        //  sdAscending:
//        //    aDir := 'ASC';
//        //  sdDescending:
//        //    aDir := 'DESC';
//        //end;
//        if (ExecRegExpr('.*\_name$', SF.FieldName)) and (SF.FieldName <> 'full_name') then
//          aSort := aSort + SF.FieldName + ' ' +{' COLLATE pt_BR ' +} aDir
//        else
//          aSort := aSort + aAlias + SF.FieldName + ' ' +{' COLLATE pt_BR ' +} aDir;
//        if i < (aSorting.Count - 1) then
//          aSort := aSort + ', ';
//      end;
//      Add('ORDER BY ' + aSort);
//    end;
//
//    {$IFDEF DEBUG}
//    LogSQL(SQL);
//    {$ENDIF}
//    Open;
//  end;
//
//  Result := not aQuery.IsEmpty;
//end;

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
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbBotanicTaxa:
      begin
        SetBotanicTaxaSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbNests:
      begin
        SetNestsSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbNestRevisions:
      begin
        SetNestRevisionsSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbEggs:
      begin
        SetEggsSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbSamplingPlots:
      begin
        SetSamplingPlotsSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbTaxonRanks:
      SetTaxonRanksSQL(aSQL, fvNone);
    tbZooTaxa:
      begin
        SetZooTaxaSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
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
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbPeople:
      begin
        SetPeopleSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbExpeditions:
      SetExpeditionsSQL(aSQL, fvNone);
    tbSurveys:
      begin
        SetSurveysSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
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
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbSpecimens:
      begin
        SetSpecimensSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbSamplePreps:
      ;
    tbPermanentNets:
      ;
    tbBands:
      begin
        SetBandsSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbIndividuals:
      begin
        SetIndividualsSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbCaptures:
      begin
        SetCapturesSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
      end;
    tbFeathers:
      begin
        SetFeathersSQL(aSQL, fvNone);
        //aAlias := TableAliases[aTable] + '.';
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
    Add('SELECT * FROM methods');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (active_status = 1)');
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
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
    Add('SELECT sp.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.order_id AS order_id,');
    Add('  z.family_id AS family_id,');
    Add('  z.genus_id AS genus_id,');
    Add('  z.species_id AS species_id,');
    Add('  g.site_name AS locality_name,');
    Add('  g.country_id AS country_id,');
    Add('  g.state_id AS state_id,');
    Add('  g.municipality_id AS municipality_id,');
    Add('  i.full_name AS individual_name,');
    Add('  n.full_name AS nest_name,');
    Add('  e.full_name AS egg_name');
    Add('FROM specimens AS sp');
    Add('LEFT JOIN zoo_taxa AS z ON sp.taxon_id = z.taxon_id');
    Add('LEFT JOIN gazetteer AS g ON sp.locality_id = g.site_id');
    Add('LEFT JOIN individuals AS i ON sp.individual_id = i.individual_id');
    Add('LEFT JOIN nests AS n ON sp.nest_id = n.nest_id');
    Add('LEFT JOIN eggs AS e ON sp.egg_id = e.egg_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (sp.specimen_id = -1) AND (sp.active_status = 1)');
      fvAll:
        Add('WHERE (sp.active_status = 1)');
      fvMarked:
        Add('WHERE (sp.active_status = 1) AND (sp.marked_status = 1)');
      fvDeleted:
        Add('WHERE (sp.active_status = 0)');
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
    Add('SELECT sv.*,');
    Add('  x.expedition_name AS expedition_name,');
    Add('  gl.full_name AS locality_name,');
    Add('  gl.country_id AS country_id,');
    Add('  gl.state_id AS state_id,');
    Add('  gl.municipality_id AS municipality_id,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name,');
    Add('  pl.full_name AS net_station_name,');
    Add('  mt.method_name AS method_name,');
    Add('  pj.short_title AS project_name,');
    Add('  CAST(COALESCE(ne.net_effort, 0) AS REAL) AS net_effort');
    Add('FROM surveys AS sv');
    Add('LEFT JOIN expeditions AS x ON sv.expedition_id = x.expedition_id');
    Add('LEFT JOIN gazetteer AS gl ON sv.locality_id = gl.site_id');
    Add('LEFT JOIN gazetteer AS gm ON gl.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON gl.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON gl.country_id = gc.site_id');
    Add('LEFT JOIN sampling_plots AS pl ON sv.net_station_id = pl.sampling_plot_id');
    Add('LEFT JOIN methods AS mt ON sv.method_id = mt.method_id');
    Add('LEFT JOIN projects AS pj ON sv.project_id = pj.project_id');
    Add('LEFT JOIN (');
    Add('  SELECT ef.survey_id, SUM(ef.net_area * ef.open_time_total) AS net_effort');
    Add('  FROM nets_effort AS ef');
    Add('  WHERE ef.active_status = 1');
    Add('  GROUP BY ef.survey_id');
    Add(') AS ne ON sv.survey_id = ne.survey_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (sv.survey_id = -1) AND (sv.active_status = 1)');
      fvAll:
        Add('WHERE (sv.active_status = 1)');
      fvMarked:
        Add('WHERE (sv.active_status = 1) AND (sv.marked_status = 1)');
      fvDeleted:
        Add('WHERE (sv.active_status = 0)');
    end;
    // if ActiveCollection.Codigo > 0 then
    // Add('and (a.reg_collection = '+IntToStr(ActiveCollection.Codigo)+')');
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
    Add('SELECT x.*,');
    Add('  pj.short_title AS project_name');
    Add('FROM expeditions AS x');
    Add('LEFT JOIN projects AS pj ON x.project_id = pj.project_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (x.expedition_id = -1) AND (x.active_status = 1)');
      fvAll:
        Add('WHERE (x.active_status = 1)');
      fvMarked:
        Add('WHERE (x.active_status = 1) AND (x.marked_status = 1)');
      fvDeleted:
        Add('WHERE (x.active_status = 0)');
    end;
    // if ActiveCollection.Codigo > 0 then
    // Add('and (a.reg_collection = '+IntToStr(ActiveCollection.Codigo)+')');
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
    Add('SELECT s.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.formatted_name AS taxon_formatted_name,');
    Add('  z.order_id AS order_id,');
    Add('  z.family_id AS family_id,');
    Add('  z.genus_id AS genus_id,');
    Add('  z.species_id AS species_id,');
    Add('  i.full_name AS individual_name,');
    Add('  p.full_name AS observer_name,');
    Add('  sv.full_name AS survey_name,');
    Add('  mt.method_name AS method_name,');
    Add('  g.full_name AS locality_name,');
    Add('  g.country_id AS country_id,');
    Add('  g.state_id AS state_id,');
    Add('  g.municipality_id AS municipality_id');
    Add('FROM sightings AS s');
    Add('LEFT JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
    Add('LEFT JOIN individuals AS i ON s.individual_id = i.individual_id');
    Add('LEFT JOIN people AS p ON s.observer_id = p.person_id');
    Add('LEFT JOIN surveys AS sv ON s.survey_id = sv.survey_id');
    Add('LEFT JOIN methods AS mt ON s.method_id = mt.method_id');
    Add('LEFT JOIN gazetteer AS g ON s.locality_id = g.site_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (s.sighting_id = -1) AND (s.active_status = 1)');
      fvAll:
        Add('WHERE (s.active_status = 1)');
      fvMarked:
        Add('WHERE (s.active_status = 1) AND (s.marked_status = 1)');
      fvDeleted:
        Add('WHERE (s.active_status = 0)');
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
    Add('SELECT st.*,');
    Add('  p.acronym AS person_acronym,');
    Add('  p.full_name AS person_name,');
    Add('  p.profile_color AS person_color');
    Add('FROM survey_team AS st');
    Add('LEFT JOIN people AS p ON st.person_id = p.person_id');
    if aSurvey > 0 then
      Add('WHERE (st.active_status = 1) AND (st.survey_id = ' + IntToStr(aSurvey) + ')')
    else
      Add('WHERE (st.active_status = 1) AND (st.survey_id = :survey_id)');
    // case aFilter of
    // fvNone: ; // do nothing
    // fvReset: Add('WHERE (a.reg_num_interno = -1) and (a.active_status = 1)');
    // fvAll: Add('WHERE (a.active_status = 1)');
    // fvMarked: Add('WHERE (a.active_status = 1) and (a.marked_status = 1)');
    // fvDeleted: Add('WHERE (a.active_status = 0)');
    // end;
    // if Trim(aOrder) <> '' then
    // begin
    // if aDirection = '' then
    // AD:= 'ASC'
    // else AD:= aDirection;
    AD := 'ASC';
    Add('ORDER BY st.visitor ASC, person_name ' {COLLATE pt_BR '} + AD);
    // end;
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
    Add('SELECT bt.*,');
    Add('  btp.taxon_name AS parent_taxon_name,');
    Add('  btv.taxon_name AS valid_name');
    Add('FROM botanic_taxa AS bt');
    Add('LEFT JOIN botanic_taxa AS btp ON bt.parent_taxon_id = btp.taxon_id');
    Add('LEFT JOIN botanic_taxa AS btv ON bt.valid_id = btv.taxon_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (bt.taxon_id = -1) AND (bt.active_status=1)');
      fvAll:
        Add('WHERE (bt.active_status = 1)');
      fvMarked:
        Add('WHERE (bt.active_status = 1) AND (bt.marked_status = 1)');
      fvDeleted:
        Add('WHERE (bt.active_status = 0)');
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
    Add('SELECT g.*,');
    Add('   gp.site_name AS parent_site_name,');
    Add('   gm.site_name AS municipality_name,');
    Add('   gs.site_name AS state_name,');
    Add('   gc.site_name AS country_name');
    Add('FROM gazetteer AS g');
    Add('LEFT JOIN gazetteer AS gp ON g.parent_site_id = gp.site_id');
    Add('LEFT JOIN gazetteer AS gm ON g.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON g.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON g.country_id = gc.site_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (g.site_id = -1) AND (g.active_status = 1)');
      fvAll:
        Add('WHERE (g.active_status = 1)');
      fvMarked:
        Add('WHERE (g.active_status = 1) AND (g.marked_status = 1)');
      fvDeleted:
        Add('WHERE (g.active_status = 0)');
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
    Add('SELECT pl.*,');
    Add('  gl.site_name AS locality_name,');
    Add('  gl.country_id AS country_id,');
    Add('  gl.state_id AS state_id,');
    Add('  gl.municipality_id AS municipality_id,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name');
    Add('FROM sampling_plots AS pl');
    Add('LEFT JOIN gazetteer AS gl ON pl.locality_id = gl.site_id');
    Add('LEFT JOIN gazetteer AS gm ON gl.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON gl.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON gl.country_id = gc.site_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (pl.sampling_plot_id = -1) AND (pl.active_status = 1)');
      fvAll:
        Add('WHERE (pl.active_status = 1)');
      fvMarked:
        Add('WHERE (pl.active_status = 1) AND (pl.marked_status = 1)');
      fvDeleted:
        Add('WHERE (pl.active_status = 0)');
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
    Add('SELECT it.*,');
    Add('   gm.site_name AS municipality_name,');
    Add('   gs.site_name AS state_name,');
    Add('   gc.site_name AS country_name');
    Add('FROM institutions AS it');
    Add('LEFT JOIN gazetteer AS gm ON it.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON it.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON it.country_id = gc.site_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (it.institution_id = -1) AND (it.active_status = 1)');
      fvAll:
        Add('WHERE (it.active_status = 1)');
      fvMarked:
        Add('WHERE (it.active_status = 1) AND (it.marked_status = 1)');
      fvDeleted:
        Add('WHERE (it.active_status = 0)');
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
    Add('SELECT n.*,');
    Add('  p.full_name AS observer_name,');
    Add('  pj.project_title AS project_name,');
    Add('  g.site_name AS locality_name,');
    Add('  g.country_id AS country_id,');
    Add('  g.state_id AS state_id,');
    Add('  g.municipality_id AS municipality_id,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.formatted_name AS taxon_formatted_name,');
    Add('  z.order_id AS order_id,');
    Add('  z.family_id AS family_id,');
    Add('  z.genus_id AS genus_id,');
    Add('  z.species_id AS species_id,');
    Add('  bt1.taxon_name AS support_plant_1_name,');
    Add('  bt2.taxon_name AS support_plant_2_name');
    Add('FROM nests AS n');
    Add('LEFT JOIN people AS p ON n.observer_id = p.person_id');
    Add('LEFT JOIN projects AS pj ON n.project_id = pj.project_id');
    Add('LEFT JOIN gazetteer AS g ON n.locality_id = g.site_id');
    Add('LEFT JOIN zoo_taxa AS z ON n.taxon_id = z.taxon_id');
    Add('LEFT JOIN botanic_taxa AS bt1 ON n.support_plant_1_id = bt1.taxon_id');
    Add('LEFT JOIN botanic_taxa AS bt2 ON n.support_plant_2_id = bt2.taxon_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (n.nest_id = -1) AND (n.active_status = 1)');
      fvAll:
        Add('WHERE (n.active_status = 1)');
      fvMarked:
        Add('WHERE (n.active_status = 1) AND (n.marked_status = 1)');
      fvDeleted:
        Add('WHERE (n.active_status = 0)');
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
    Add('SELECT nr.*,');
    Add('  p1.acronym AS observer_1_name,');
    Add('  p2.acronym AS observer_2_name,');
    Add('  z.full_name AS nidoparasite_name');
    Add('FROM nest_revisions AS nr');
    Add('LEFT JOIN people AS p1 ON nr.observer_1_id = p1.person_id');
    Add('LEFT JOIN people AS p2 ON nr.observer_2_id = p2.person_id');
    Add('LEFT JOIN zoo_taxa AS z ON nr.nidoparasite_id = z.taxon_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (nr.nest_revision_id = -1) AND (nr.active_status = 1)');
      fvAll:
        Add('WHERE (nr.active_status = 1)');
      fvMarked:
        Add('WHERE (nr.active_status = 1) AND (nr.marked_status = 1)');
      fvDeleted:
        Add('WHERE (nr.active_status = 0)');
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
    Add('SELECT e.*,');
    Add('  p.acronym AS researcher_name,');
    Add('  i.full_name AS individual_name,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.order_id AS order_id,');
    Add('  z.family_id AS family_id,');
    Add('  z.genus_id AS genus_id,');
    Add('  z.species_id AS species_id');
    Add('FROM eggs AS e');
    Add('LEFT JOIN people AS p ON e.researcher_id = p.person_id');
    Add('LEFT JOIN individuals AS i ON e.individual_id = i.individual_id');
    Add('LEFT JOIN zoo_taxa AS z ON e.taxon_id = z.taxon_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (e.nest_id = -1) AND (e.active_status = 1)');
      fvAll:
        Add('WHERE (e.active_status = 1)');
      fvMarked:
        Add('WHERE (e.active_status = 1) AND (e.marked_status = 1)');
      fvDeleted:
        Add('WHERE (e.active_status = 0)');
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
    Add('SELECT i.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.order_id AS order_id,');
    Add('  z.family_id AS family_id,');
    Add('  z.genus_id AS genus_id,');
    Add('  z.species_id AS species_id,');
    Add('  n.full_name AS nest_name,');
    Add('  (b1.band_size||'' ''||b1.band_number) AS band_name,');
    Add('  (b2.band_size||'' ''||b2.band_number) AS double_band_name,');
    Add('  (b3.band_size||'' ''||b3.band_number) AS removed_band_name,');
    Add('  fi.full_name AS father_name,');
    Add('  mi.full_name AS mother_name,');
    Add('  (SELECT CAST(SUM(c.active_status) AS INTEGER) FROM captures AS c');
    Add('    WHERE c.individual_id = i.individual_id) AS captures_tally');
    Add('FROM individuals AS i');
    Add('LEFT JOIN zoo_taxa AS z ON i.taxon_id = z.taxon_id');
    Add('LEFT JOIN nests AS n ON i.nest_id = n.nest_id');
    Add('LEFT JOIN bands AS b1 ON i.band_id = b1.band_id');
    Add('LEFT JOIN bands AS b2 ON i.double_band_id = b2.band_id');
    Add('LEFT JOIN bands AS b3 ON i.removed_band_id = b3.band_id');
    Add('LEFT JOIN individuals AS fi ON i.father_id = fi.individual_id');
    Add('LEFT JOIN individuals AS mi ON i.mother_id = mi.individual_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (i.individual_id = -1) AND (i.active_status = 1)');
      fvAll:
        Add('WHERE (i.active_status = 1)');
      fvMarked:
        Add('WHERE (i.active_status = 1) AND (i.marked_status = 1)');
      fvDeleted:
        Add('WHERE (i.active_status = 0)');
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
    Add('SELECT c.*,');
    Add('   z.full_name AS taxon_name,');
    Add('   z.formatted_name AS taxon_formatted_name,');
    Add('   z.order_id AS order_id,');
    Add('   z.family_id AS family_id,');
    Add('   z.genus_id AS genus_id,');
    Add('   z.species_id AS species_id,');
    Add('   sv.full_name AS survey_name,');
    Add('   pl.full_name AS net_station_name,');
    Add('   ef.net_number AS net_number,');
    Add('   g.site_name AS locality_name,');
    Add('   g.country_id AS country_id,');
    Add('   g.state_id AS state_id,');
    Add('   g.municipality_id AS municipality_id,');
    Add('   p1.acronym AS bander_name,');
    Add('   p2.acronym AS annotator_name,');
    Add('   (b1.band_size||'' ''||b1.band_number) AS band_name,');
    Add('   (b2.band_size||'' ''||b2.band_number) AS removed_band_name,');
    Add('   f1.acronym AS photographer_1_name,');
    Add('   f2.acronym AS photographer_2_name');
    Add('FROM captures AS c');
    Add('LEFT JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id');
    Add('LEFT JOIN surveys AS sv ON c.survey_id = sv.survey_id');
    Add('LEFT JOIN sampling_plots AS pl ON c.net_station_id = pl.sampling_plot_id');
    Add('LEFT JOIN nets_effort AS ef ON c.net_id = ef.net_id');
    Add('LEFT JOIN gazetteer AS g ON c.locality_id = g.site_id');
    Add('LEFT JOIN people AS p1 ON c.bander_id = p1.person_id');
    Add('LEFT JOIN people AS p2 ON c.annotator_id = p2.person_id');
    Add('LEFT JOIN people AS f1 ON c.photographer_1_id = f1.person_id');
    Add('LEFT JOIN people AS f2 ON c.photographer_2_id = f2.person_id');
    Add('LEFT JOIN bands AS b1 ON c.band_id = b1.band_id');
    Add('LEFT JOIN bands AS b2 ON c.removed_band_id = b2.band_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (c.capture_id = -1) AND (c.active_status = 1)');
      fvAll:
        Add('WHERE (c.active_status = 1)');
      fvMarked:
        Add('WHERE (c.active_status = 1) AND (c.marked_status = 1)');
      fvDeleted:
        Add('WHERE (c.active_status = 0)');
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
    Add('SELECT * FROM taxon_ranks');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (active_status = 1)');
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
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
    Add('SELECT p.*,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name,');
    Add('  it.full_name AS institution_name');
    Add('FROM people AS p');
    Add('LEFT JOIN gazetteer AS gm ON p.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON p.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON p.country_id = gc.site_id');
    Add('LEFT JOIN institutions AS it ON p.institution_id = it.institution_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (p.person_id = -1) AND (p.active_status = 1)');
      fvAll:
        Add('WHERE (p.active_status = 1)');
      fvMarked:
        Add('WHERE (p.active_status = 1) AND (p.marked_status = 1)');
      fvDeleted:
        Add('WHERE (p.active_status = 0)');
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
    Add('SELECT * FROM projects');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (project_id = -1) AND (active_status = 1)');
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
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
    Add('SELECT pt.*,');
    Add('  p.acronym AS person_acronym,');
    Add('  p.full_name AS person_name,');
    Add('  p.profile_color AS person_color');
    Add('FROM project_team AS pt');
    Add('LEFT JOIN people AS p ON pt.person_id = p.person_id');
    if aProject > 0 then
      Add('WHERE (pt.active_status = 1) AND (pt.project_id = ' + IntToStr(aProject) + ')')
    else
      Add('WHERE (pt.active_status = 1) AND (pt.project_id = :project_id)');
    // case aFilter of
    // fvNone: ; // do nothing
    // fvReset: Add('WHERE (a.reg_num_interno = -1) and (a.active_status = 1)');
    // fvAll: Add('WHERE (a.active_status = 1)');
    // fvMarked: Add('WHERE (a.active_status = 1) and (a.marked_status = 1)');
    // fvDeleted: Add('WHERE (a.active_status = 0)');
    // end;
    // if Trim(aOrder) <> '' then
    // begin
    // if aDirection = '' then
    // AD:= 'ASC'
    // else AD:= aDirection;
    AD := 'ASC';
    Add('ORDER BY pt.project_manager ASC, person_name ' {COLLATE pt_BR '} + AD);
    // end;
  end;
end;

procedure SetPermitsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String; aDirection: String);
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT l.*,');
    Add('  pj.short_title AS project_name');
    Add('FROM legal AS l');
    Add('LEFT JOIN projects AS pj ON l.project_id = pj.project_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (l.permit_id = -1) AND (l.active_status = 1)');
      fvAll:
        Add('WHERE (l.active_status = 1)');
      fvMarked:
        Add('WHERE (l.active_status = 1) AND (l.marked_status = 1)');
      fvDeleted:
        Add('WHERE (l.active_status = 0)');
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
    Add('SELECT z.*,');
    Add('    r.rank_name AS rank_name,');
    Add('    u.full_name AS parent_taxon_name,');
    Add('    v.full_name AS valid_name,');
    Add('    ui.full_name AS ioc_parent_name,');
    Add('    vi.full_name AS ioc_valid_name,');
    Add('    o.full_name AS order_name,');
    Add('    f.full_name AS family_name,');
    Add('    s.full_name AS subfamily_name,');
    Add('    n.full_name AS genero_name,');
    Add('    e.full_name AS species_name,');
    Add('    g.full_name AS subspecies_group_name');
    Add('FROM zoo_taxa AS z');
    Add('LEFT JOIN taxon_ranks AS r ON z.rank_id = r.rank_id');
    Add('LEFT JOIN zoo_taxa AS u ON z.parent_taxon_id = u.taxon_id');
    Add('LEFT JOIN zoo_taxa AS v ON z.valid_id = v.taxon_id');
    Add('LEFT JOIN zoo_taxa AS ui ON z.ioc_parent_taxon_id = ui.taxon_id');
    Add('LEFT JOIN zoo_taxa AS vi ON z.ioc_valid_id = vi.taxon_id');
    Add('LEFT JOIN zoo_taxa AS o ON z.order_id = o.taxon_id');
    Add('LEFT JOIN zoo_taxa AS f ON z.family_id = f.taxon_id');
    Add('LEFT JOIN zoo_taxa AS s ON z.subfamily_id = s.taxon_id');
    Add('LEFT JOIN zoo_taxa AS n ON z.genus_id = n.taxon_id');
    Add('LEFT JOIN zoo_taxa AS e ON z.species_id = e.taxon_id');
    Add('LEFT JOIN zoo_taxa AS g ON z.subspecies_group_id = g.taxon_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (z.taxon_id = -1) AND (z.active_status = 1)');
      fvAll:
        Add('WHERE (z.active_status = 1)');
      fvMarked:
        Add('WHERE (z.active_status = 1) AND (z.marked_status = 1)');
      fvDeleted:
        Add('WHERE (z.active_status = 0)');
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
    Add('SELECT b.*,');
    Add('  it.acronym AS supplier_name,');
    Add('  p1.full_name AS requester_name,');
    Add('  p2.full_name AS carrier_name,');
    Add('  i.full_name AS individual_name,');
    Add('  pj.short_title AS project_name');
    Add('FROM bands AS b');
    Add('LEFT JOIN institutions AS it ON b.supplier_id = it.institution_id');
    Add('LEFT JOIN people AS p1 ON b.requester_id = p1.person_id');
    Add('LEFT JOIN people AS p2 ON b.carrier_id = p2.person_id');
    Add('LEFT JOIN individuals AS i ON b.individual_id = i.individual_id');
    Add('LEFT JOIN projects AS pj ON b.project_id = pj.project_id');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (b.band_id = -1) AND (b.active_status = 1)');
      fvAll:
        Add('WHERE (b.active_status = 1)');
      fvMarked:
        Add('WHERE (b.active_status = 1) AND (b.marked_status = 1)');
      fvDeleted:
        Add('WHERE (b.active_status = 0)');
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
    Add('SELECT * FROM users');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (user_id = -1) AND (active_status = 1)');
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
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
    Add('SELECT ft.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.order_id AS order_id,');
    Add('  z.family_id AS family_id,');
    Add('  z.genus_id AS genus_id,');
    Add('  z.species_id AS species_id,');
    Add('  i.full_name AS individual_name,');
    Add('  p.acronym AS observer_name,');
    Add('  c.full_name AS capture_name,');
    Add('  st.full_name AS sighting_name,');
    Add('  g.country_id AS country_id,');
    Add('  g.state_id AS state_id,');
    Add('  g.municipality_id AS municipality_id,');
    Add('  g.site_name AS locality_name');
    Add('FROM feathers AS ft');
    Add('LEFT JOIN zoo_taxa AS z ON ft.taxon_id = z.taxon_id');
    Add('LEFT JOIN individuals AS i ON ft.individual_id = i.individual_id');
    Add('LEFT JOIN people AS p ON ft.observer_id = p.person_id');
    Add('LEFT JOIN captures AS c ON ft.capture_id = c.capture_id');
    Add('LEFT JOIN sightings AS st ON ft.sighting_id = st.sighting_id');
    Add('LEFT JOIN gazetteer AS g ON ft.locality_id = g.site_id ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (ft.feather_id = -1) AND (ft.active_status = 1)');
      fvAll:
        Add('WHERE (ft.active_status = 1)');
      fvMarked:
        Add('WHERE (ft.active_status = 1) AND (ft.marked_status = 1)');
      fvDeleted:
        Add('WHERE (ft.active_status = 0)');
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

