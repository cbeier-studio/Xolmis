unit cbs_datasearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, RegExpr, StrUtils, cbs_datatypes;

  { Search records }
  function TableSearch(aQuery: TSQLQuery; aTable: TTableType; aSearch: TSearch;
    aQuickFilter: TStrings; aModifier: TRecordStatus; aSorting: TSortedFields;
    aWhere: TStrings): Boolean;

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
  procedure SetNetStationsSQL(const aSQL: TStrings; aFilter: TFilterValue;
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
  procedure SetReportsSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: String = '');

implementation

uses cbs_global;

{ ----------------------------------------------------------------------------------------- }
{ Search records }
{ ----------------------------------------------------------------------------------------- }

function TableSearch(aQuery: TSQLQuery; aTable: TTableType; aSearch: TSearch;
  aQuickFilter: TStrings; aModifier: TRecordStatus; aSorting: TSortedFields;
  aWhere: TStrings): Boolean;
var
  AndWhere, aAlias, aSort, aDir: String;
  i: Integer;
  SF: TSortedField;
begin
  aAlias := EmptyStr;
  AndWhere := 'WHERE ';
  aWhere.Clear;
  Result := False;

  with aQuery, SQL do
  begin
    Close;
    Clear;
    SetSelectSQL(SQL, aTable, aAlias);

    // Value typed in the Search field
    if not aSearch.IsEmpty then
    begin
      //aQuery.FilterOptions := [foCaseInsensitive];
      //aQuery.Filter := aSearch.FilterString;
      //aQuery.Filtered := True;
      Add(aSearch.SQLString);
      aWhere.Add(aSearch.SQLString);
      AndWhere := 'AND ';
    end;
    //else
    //  aQuery.Filtered := False;

    // Quick filters applied
    if aQuickFilter.Count > 0 then
    begin
      for i := 0 to aQuickFilter.Count - 1 do
      begin
        Add(AndWhere + aQuickFilter[i]);
        aWhere.Add(AndWhere + aQuickFilter[i]);
        AndWhere := 'AND ';
      end;
    end;

    // Record active or not
    case aModifier.Status of
      rsAll:
        ;
      rsActive:
        begin
          Add(AndWhere + '(' + aAlias + 'active_status = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'active_status = 1)');
          AndWhere := 'AND ';
        end;
      rsInactive:
        begin
          Add(AndWhere + '(' + aAlias + 'active_status = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'active_status = 0)');
          AndWhere := 'AND ';
        end;
      rsNone:
        begin
          Add(AndWhere + '(' + aAlias + 'active_status = -1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'active_status = -1)');
          AndWhere := 'AND ';
        end;
    end;
    // Record marked or not
    case aModifier.Mark of
      rmAll:
        ;
      rmMarked:
        begin
          Add(AndWhere + '(' + aAlias + 'marked_status = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'marked_status = 1)');
          AndWhere := 'AND ';
        end;
      rmUnmarked:
        begin
          Add(AndWhere + '(' + aAlias + 'marked_status = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'marked_status = 0)');
          AndWhere := 'AND ';
        end;
    end;
    // Record queued or not
    case aModifier.Queue of
      rqAll:
        ;
      rqQueued:
        begin
          Add(AndWhere + '(' + aAlias + 'queued_status = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'queued_status = 1)');
          AndWhere := 'AND ';
        end;
      rqUnqueued:
        begin
          Add(AndWhere + '(' + aAlias + 'queued_status = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'queued_status = 0)');
          AndWhere := 'AND ';
        end;
    end;
    // Record already exported or not
    case aModifier.Share of
      rxAll:
        ;
      rxExported:
        begin
          Add(AndWhere + '(' + aAlias + 'exported_status = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'exported_status = 1)');
          AndWhere := 'AND ';
        end;
      rxNotExported:
        begin
          Add(AndWhere + '(' + aAlias + 'exported_status = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'exported_status = 0)');
          AndWhere := 'AND ';
        end;
    end;

    // Record sorting
    if aSorting.Count > 0 then
    begin
      aSort := '';
      aDir := '';
      for i := 0 to (aSorting.Count - 1) do
      begin
        SF := aSorting.Items[i];
        aDir := SortDirections[SF.Direction];
        //case SF^.Direction of
        //  sdNone:
        //    ;
        //  sdAscending:
        //    aDir := 'ASC';
        //  sdDescending:
        //    aDir := 'DESC';
        //end;
        if (ExecRegExpr('.*\_name$', SF.FieldName)) and (SF.FieldName <> 'full_name') then
          aSort := aSort + SF.FieldName + ' ' +{' COLLATE pt_BR ' +} aDir
        else
          aSort := aSort + aAlias + SF.FieldName + ' ' +{' COLLATE pt_BR ' +} aDir;
        if i < (aSorting.Count - 1) then
          aSort := aSort + ', ';
      end;
      Add('ORDER BY ' + aSort);
    end;

    {$IFDEF DEBUG}
    LogSQL(SQL);
    {$ENDIF}
    Open;
  end;

  Result := not aQuery.IsEmpty;
end;

{
 ----------------------------------------------------------------------------
   SQL Select and filtering
 ----------------------------------------------------------------------------
}

function GetModifier(aModifier: String): TFilterValue;
begin
  Result := fvNone;

  if MatchStr(aModifier, AllQS) then { #tudo }
    Result := fvAll
  else
  if MatchStr(aModifier, MarkedQS) then { #marcados }
    Result := fvMarked
  else
  if MatchStr(aModifier, UnmarkedQS) then { #naomarcados }
    Result := fvUnmarked
  else
  if MatchStr(aModifier, DeletedQS) then { #lixo }
    Result := fvDeleted
  else
  if MatchStr(aModifier, PrintQueueQS) then { #fila }
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
        aAlias:= EmptyStr;
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
    tbNetStations:
      begin
        SetNetStationsSQL(aSQL, fvNone);
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
    tbMolts:
      ;
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
    Add('(SELECT z.full_name FROM zoo_taxa AS z WHERE z.taxon_id = sp.taxon_id) AS taxon_name,');
    Add('(SELECT g.site_name FROM gazetteer AS g WHERE g.site_id = sp.locality_id) AS locality_name,');
    Add('(SELECT i.full_name FROM individuals AS i WHERE i.individual_id = sp.individual_id) AS individual_name,');
    Add('(SELECT n.full_name FROM nests AS n WHERE n.nest_id = sp.nest_id) AS nest_name,');
    Add('(SELECT e.full_name FROM eggs AS e WHERE e.egg_id = sp.egg_id) AS egg_name');
    Add('FROM specimens AS sp');
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
    Add('(SELECT x.full_name FROM expeditions AS x WHERE x.expedition_id = sv.expedition_id) AS expedition_name,');
    Add('(SELECT gm.site_name FROM gazetteer AS gm WHERE gm.site_id = sv.municipality_id) AS municipality_name,');
    Add('(SELECT gs.site_name FROM gazetteer AS gs WHERE gs.site_id = sv.state_id) AS state_name,');
    Add('(SELECT gc.site_name FROM gazetteer AS gc WHERE gc.site_id = sv.country_id) AS country_name,');
    Add('(SELECT gl.full_name FROM gazetteer AS gl WHERE gl.site_id = sv.locality_id) AS locality_name,');
    Add('(SELECT ns.station_name FROM net_stations AS ns WHERE ns.net_station_id = sv.net_station_id) AS net_station_name,');
    Add('(SELECT mt.method_name FROM methods AS mt WHERE mt.method_id = sv.method_id) AS method_name,');
    Add('(SELECT pj.project_title FROM projects AS pj WHERE pj.project_id = sv.project_id) AS project_name,');
    Add('(SELECT cast(sum(ef.net_area) + sum(ef.open_time_total) AS real) FROM nets_effort AS ef '
      + 'WHERE (ef.survey_id = sv.survey_id) AND (ef.active_status = 1)) AS net_effort');
    Add('FROM surveys AS sv');
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
    Add('(SELECT gm.site_name FROM gazetteer AS gm WHERE gm.site_id = x.municipality_id) AS municipality_name,');
    Add('(SELECT gs.site_name FROM gazetteer AS gs WHERE gs.site_id = x.state_id) AS state_name,');
    Add('(SELECT gc.site_name FROM gazetteer AS gc WHERE gc.site_id = x.country_id) AS country_name,');
    Add('(SELECT gl.full_name FROM gazetteer AS gl WHERE gl.site_id = x.locality_id) AS locality_name,');
    Add('(SELECT pj.project_title FROM projects AS pj WHERE pj.project_id = x.project_id) AS project_name');
    Add('FROM expeditions AS x');
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
    Add('(SELECT z1.full_name FROM zoo_taxa AS z1 WHERE z1.taxon_id = s.taxon_id) AS taxon_name,');
    Add('(SELECT z2.formatted_name FROM zoo_taxa AS z2 WHERE z2.taxon_id = s.taxon_id) AS taxon_formatted_name,');
    Add('(SELECT i.full_name FROM individuals AS i WHERE i.individual_id = s.individual_id) AS individual_name,');
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = s.observer_id) AS observer_name,');
    Add('(SELECT sv.full_name FROM surveys AS sv WHERE sv.survey_id = s.survey_id) AS survey_name,');
    Add('(SELECT mt.method_name FROM methods AS mt WHERE mt.method_id = s.method_id) AS method_name,');
    Add('(SELECT g.full_name FROM gazetteer AS g WHERE g.site_id = s.locality_id) AS locality_name');
    Add('FROM sightings AS s');
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
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = st.person_id) AS person_name');
    Add('FROM survey_team AS st');
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
    Add('(SELECT btp.taxon_name FROM botanic_taxa AS btp WHERE btp.taxon_id = bt.parent_taxon_id) AS parent_taxon_name,');
    Add('(SELECT btv.taxon_name FROM botanic_taxa AS btv WHERE btv.taxon_id = bt.valid_id) AS valid_name');
    Add('FROM botanic_taxa AS bt');
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
    Add('(SELECT gp.site_name FROM gazetteer AS gp WHERE gp.site_id = g.parent_site_id) AS parent_site_name,');
    Add('(SELECT gm.site_name FROM gazetteer AS gm WHERE gm.site_id = g.municipality_id) AS municipality_name,');
    Add('(SELECT gs.site_name FROM gazetteer AS gs WHERE gs.site_id = g.state_id) AS state_name,');
    Add('(SELECT gc.site_name FROM gazetteer AS gc WHERE gc.site_id = g.country_id) AS country_name');
    Add('FROM gazetteer AS g');
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

procedure SetNetStationsSQL(const aSQL: TStrings; aFilter: TFilterValue;
  aSorting: String = ''; aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT ns.*,');
    Add('(SELECT gl.site_name FROM gazetteer AS gl WHERE gl.site_id = ns.locality_id) AS locality_name,');
    Add('(SELECT gm.site_name FROM gazetteer AS gm WHERE gm.site_id = ns.municipality_id) AS municipality_name,');
    Add('(SELECT gs.site_name FROM gazetteer AS gs WHERE gs.site_id = ns.state_id) AS state_name,');
    Add('(SELECT gc.site_name FROM gazetteer AS gc WHERE gc.site_id = ns.country_id) AS country_name');
    Add('FROM net_stations AS ns');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        Add('WHERE (ns.net_station_id = -1) AND (ns.active_status = 1)');
      fvAll:
        Add('WHERE (ns.active_status = 1)');
      fvMarked:
        Add('WHERE (ns.active_status = 1) AND (ns.marked_status = 1)');
      fvDeleted:
        Add('WHERE (ns.active_status = 0)');
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
    Add('(SELECT gm.site_name FROM gazetteer AS gm WHERE gm.site_id = it.municipality_id) AS municipality_name,');
    Add('(SELECT gs.site_name FROM gazetteer AS gs WHERE gs.site_id = it.state_id) AS state_name,');
    Add('(SELECT gc.site_name FROM gazetteer AS gc WHERE gc.site_id = it.country_id) AS country_name');
    Add('FROM institutions AS it');
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
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = n.observer_id) AS observer_name,');
    Add('(SELECT pj.project_title FROM projects AS pj WHERE pj.project_id = n.project_id) AS project_name,');
    Add('(SELECT g.site_name FROM gazetteer AS g WHERE g.site_id = n.locality_id) AS locality_name,');
    Add('(SELECT z1.full_name FROM zoo_taxa AS z1 WHERE z1.taxon_id = n.taxon_id) AS taxon_name,');
    Add('(SELECT z2.formatted_name FROM zoo_taxa AS z2 WHERE z2.taxon_id = n.taxon_id) AS taxon_formatted_name,');
    //Add('(SELECT mi.full_name FROM individuals AS m WHERE m.individual_id = n.male_id) AS male_name,');
    //Add('(SELECT fi.full_name FROM individuals As f WHERE f.individual_id = n.female_id) AS female_name,');
    //Add('(SELECT h1.full_name FROM individuals AS h1 WHERE h1.individual_id = n.helper_1_id) AS helper_1_name,');
    //Add('(SELECT h2.full_name FROM individuals AS h2 WHERE h2.individual_id = n.helper_2_id) AS helper_2_name,');
    //Add('(SELECT h3.full_name FROM individuals AS h3 WHERE h3.individual_id = n.helper_3_id) AS helper_3_name,');
    Add('(SELECT bt1.taxon_name FROM botanic_taxa AS bt1 WHERE bt1.taxon_id = n.support_plant_1_id) AS support_plant_1_name,');
    Add('(SELECT bt2.taxon_name FROM botanic_taxa AS bt2 WHERE bt2.taxon_id = n.support_plant_2_id) AS support_plant_2_name');
    Add('FROM nests AS n');
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
    Add('(SELECT p1.acronym FROM people AS p1 WHERE p1.person_id = nr.observer_1_id) AS observer_1_name,');
    Add('(SELECT p2.acronym FROM people AS p2 WHERE p2.person_id = nr.observer_2_id) AS observer_2_name,');
    Add('(SELECT z.full_name FROM zoo_taxa AS z WHERE z.taxon_id = nr.nidoparasite_id) AS nidoparasite_name');
    Add('FROM nest_revisions AS nr');
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
    Add('(SELECT p.acronym FROM people AS p WHERE p.person_id = e.researcher_id) AS researcher_name,');
    Add('(SELECT i.full_name FROM individuals AS i WHERE i.individual_id = e.individual_id) AS individual_name,');
    Add('(SELECT z.full_name FROM zoo_taxa AS z WHERE z.taxon_id = e.taxon_id) AS taxon_name');
    Add('FROM eggs AS e');
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
    Add('(SELECT z1.full_name FROM zoo_taxa AS z1 WHERE z1.taxon_id = i.taxon_id) AS taxon_name,');
    Add('(SELECT z2.formatted_name FROM zoo_taxa AS z2 WHERE z2.taxon_id = i.taxon_id) AS taxon_formatted_name,');
    Add('(SELECT n.full_name FROM nests AS n WHERE n.nest_id = i.nest_id) AS nest_name,');
    Add('(SELECT b1.full_name FROM bands AS b1 WHERE b1.band_id = i.band_id) AS band_full_name,');
    Add('(SELECT b2.full_name FROM bands AS b2 WHERE b2.band_id = i.double_band_id) AS double_band_name,');
    Add('(SELECT b3.full_name FROM bands AS b3 WHERE b3.band_id = i.removed_band_id) AS removed_band_name,');
    Add('(SELECT fi.full_name FROM individuals AS fi WHERE fi.individual_id = i.father_id) AS father_name,');
    Add('(SELECT mi.full_name FROM individuals AS mi WHERE mi.individual_id = i.mother_id) AS mother_name,');
    Add('(SELECT CAST(sum(c.active_status) AS INTEGER) FROM captures AS c WHERE c.individual_id = i.individual_id) AS captures_tally');
    Add('FROM individuals AS i');
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
    Add('(SELECT z1.full_name FROM zoo_taxa AS z1 WHERE z1.taxon_id = c.taxon_id) AS taxon_name,');
    Add('(SELECT z2.formatted_name FROM zoo_taxa AS z2 WHERE z2.taxon_id = c.taxon_id) AS taxon_formatted_name,');
    Add('(SELECT sv.full_name FROM surveys AS sv WHERE sv.survey_id = c.survey_id) AS survey_name,');
    Add('(SELECT ns.station_name FROM net_stations AS ns WHERE ns.net_station_id = c.net_station_id) AS net_station_name,');
    Add('(SELECT ef.net_number FROM nets_effort AS ef WHERE ef.net_id = c.net_id) AS net_number,');
    Add('(SELECT g.site_name FROM gazetteer As g WHERE g.site_id = c.locality_id) AS locality_name,');
    Add('(SELECT p1.acronym FROM people AS p1 WHERE p1.person_id = c.bander_id) AS bander_name,');
    Add('(SELECT p2.acronym FROM people AS p2 WHERE p2.person_id = c.annotator_id) AS annotator_name,');
    Add('(SELECT (b1.band_size||'' ''||b1.band_number) FROM bands AS b1 WHERE b1.band_id = c.band_id) AS band_name,');
    Add('(SELECT (b2.band_size||'' ''||b2.band_number) FROM bands AS b2 WHERE b2.band_id = c.removed_band_id) AS removed_band_name,');
    Add('(SELECT f1.acronym FROM people AS f1 WHERE f1.person_id = c.photographer_1_id) AS photographer_1_name,');
    Add('(SELECT f2.acronym FROM people AS f2 WHERE f2.person_id = c.photographer_2_id) AS photographer_2_name');
    Add('FROM captures AS c');
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
    Add('(SELECT gm.site_name FROM gazetteer AS gm WHERE gm.site_id = p.municipality_id) AS municipality_name,');
    Add('(SELECT ge.site_name FROM gazetteer AS ge WHERE ge.site_id = p.state_id) AS state_name,');
    Add('(SELECT gc.site_name FROM gazetteer AS gc WHERE gc.site_id = p.country_id) AS country_name,');
    Add('(SELECT it.full_name FROM institutions AS it WHERE it.institution_id = p.institution_id) AS institution_name');
    Add('FROM people AS p');
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
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = pt.person_id) AS person_name');
    Add('FROM project_team AS pt');
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
    Add('(SELECT pj.short_title FROM projects AS pj WHERE pj.project_id = l.project_id) AS project_name');
    Add('FROM legal AS l');
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
    Add('(SELECT u.full_name FROM zoo_taxa AS u WHERE u.taxon_id = z.parent_taxon_id) AS parent_taxon_name,');
    Add('(SELECT v.full_name FROM zoo_taxa AS v WHERE v.taxon_id = z.valid_id) AS valid_name,');
    Add('(SELECT ui.full_name FROM zoo_taxa AS ui WHERE ui.taxon_id = z.ioc_parent_taxon_id) AS ioc_parent_name,');
    Add('(SELECT vi.full_name FROM zoo_taxa AS vi WHERE vi.taxon_id = z.ioc_valid_id) AS ioc_valid_name,');
    Add('(SELECT uc.full_name FROM zoo_taxa AS uc WHERE uc.taxon_id = z.cbro_parent_taxon_id) AS cbro_parent_name,');
    Add('(SELECT vc.full_name FROM zoo_taxa AS vc WHERE vc.taxon_id = z.cbro_valid_id) AS cbro_valid_name,');
    Add('(SELECT o.full_name FROM zoo_taxa AS o WHERE o.taxon_id = z.order_id) AS order_name,');
    Add('(SELECT f.full_name FROM zoo_taxa AS f WHERE f.taxon_id = z.family_id) AS family_name,');
    Add('(SELECT s.full_name FROM zoo_taxa AS s WHERE s.taxon_id = z.subfamily_id) AS subfamily_name,');
    Add('(SELECT n.full_name FROM zoo_taxa AS n WHERE n.taxon_id = z.genus_id) AS genero_name,');
    Add('(SELECT e.full_name FROM zoo_taxa AS e WHERE e.taxon_id = z.species_id) AS species_name,');
    Add('(SELECT g.full_name FROM zoo_taxa AS g WHERE g.taxon_id = z.subspecies_group_id) AS subspecies_group_name');
    Add('FROM zoo_taxa AS z');
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
    Add('(SELECT it.acronym FROM institutions AS it WHERE it.institution_id = b.supplier_id) AS supplier_name,');
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = b.carrier_id) AS carrier_name,');
    Add('(SELECT i.formatted_name FROM individuals AS i WHERE i.individual_id = b.individual_id) AS individual_name,');
    Add('(SELECT pj.project_title FROM projects AS pj WHERE pj.project_id = b.project_id) AS project_name');
    Add('FROM bands AS b');
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

procedure SetReportsSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String = '';
  aDirection: String = '');
var
  AD: String;
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT * FROM reports');
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

end.

