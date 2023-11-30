unit cbs_count;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, DB, SQLDB, cbs_datatypes;

  function MarkedCount(aTabela: TTableType; aModifier: TRecordStatus; aWhere: TStrings;
    aField: String = ''): Integer;
  procedure TableCount(const aTabela: TTableType; aQuery: TSQLQuery; aModifier: TRecordStatus;
    aWhere: TStrings; strTotal: TLabel; ackMarcados: TCheckBox);
  procedure RegAtual(aDataset: TDataset; strAtual: TLabel);

  procedure SetCountSQL(const aSQL: TStrings; aTable: TTableType);

  procedure CountIndividualsSQL(const aSQL: TStrings);
  procedure CountSpecimensSQL(const aSQL: TStrings);
  procedure CountSurveysSQL(const aSQL: TStrings);
  procedure CountExpeditionsSQL(const aSQL: TStrings);
  procedure CountSightingsSQL(const aSQL: TStrings);
  procedure CountBotanicTaxaSQL(const aSQL: TStrings);
  procedure CountGazetteerSQL(const aSQL: TStrings);
  procedure CountNetStationsSQL(const aSQL: TStrings);
  procedure CountInstitutionsSQL(const aSQL: TStrings);
  procedure CountNestsSQL(const aSQL: TStrings);
  procedure CountCapturesSQL(const aSQL: TStrings);
  procedure CountPeopleSQL(const aSQL: TStrings);
  procedure CountProjectsSQL(const aSQL: TStrings);
  procedure CountZooTaxaSQL(const aSQL: TStrings);
  procedure CountBandsSQL(const aSQL: TStrings);
  procedure CountUsersSQL(const aSQL: TStrings);

implementation

uses cbs_locale, udm_main;

function MarkedCount(aTabela: TTableType; aModifier: TRecordStatus; aWhere: TStrings;
  aField: String): Integer;
var
  Qry: TSQLQuery;
  ms: Integer;
  sFilter: String;
begin
  Result := 0;

  Qry := TSQLQuery.Create(DMM.sqlCon);
  try
    case aModifier.Status of
      rsAll:
        ;
      rsActive:
        sFilter := ' (ativo = 1)';
      rsInactive:
        sFilter := ' (ativo = 0)';
    end;

    with Qry, SQL do
    begin
      MacroCheck := True;
      Database := DMM.sqlCon;
      Clear;
      SetCountSQL(SQL, aTabela);
      if (aWhere.Count > 0) then
      begin
        AddStrings(aWhere);
      end;
      Add(')');
      Add('SELECT sum(%afield) AS view_marked FROM lista');
      if aField = EmptyStr then
        aField := 'marcado';
      MacroByName('AFIELD').Value := aField;
//      GravaLogSQL(SQL);
      Open;
      if Fields[0].IsNull then
        ms := 0
      else
        ms := Fields[0].AsInteger;
      Close;
    end;

    Result := ms;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TableCount(const aTabela: TTableType; aQuery: TSQLQuery; aModifier: TRecordStatus;
  aWhere: TStrings; strTotal: TLabel; ackMarcados: TCheckBox);
var
  Qry: TSQLQuery;
  t, f, m, ms: Integer;
  sFound, sReg, sMarked, sFilter: String;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  try
    case aModifier.Status of
      rsAll:
        sFilter := EmptyStr;
      rsActive:
        sFilter := ' (ativo = 1)';
      rsInactive:
        sFilter := ' (ativo = 0)';
    end;

    with Qry, SQL do
    begin
      MacroCheck := True;
      DataBase := DMM.sqlCon;
      Clear;
      SetCountSQL(SQL, aTabela);
      Add('WHERE %filtro');
      Add(')');
      Add('SELECT count(ativo) AS records_total FROM lista');
      MacroByName('FILTRO').Value := sFilter;
      //GravaLogSQL(SQL);
      Open;
      t := Fields[0].AsInteger;
      Close;

      Clear;
      SetCountSQL(SQL, aTabela);
      Add('WHERE %filtro AND (marked_status = 1)');
      Add(')');
      Add('SELECT count(ativo) AS marked_total FROM lista');
      MacroByName('FILTRO').Value := sFilter;
      //GravaLogSQL(SQL);
      Open;
      m := Fields[0].AsInteger;
      Close;

      if (aWhere.Count > 0) then
      begin
        Clear;
        SetCountSQL(SQL, aTabela);
        AddStrings(aWhere);
        Add(')');
        Add('SELECT sum(marcado) AS view_marked FROM lista');
        //GravaLogSQL(SQL);
        Open;
        if Fields[0].IsNull then
          ms := 0
        else
          ms := Fields[0].AsInteger;
        Close;
      end
      else
        ms := m;
    end;
    if aQuery.Active then
      f := aQuery.RecordCount
    else
      f := 0;
    if f = 1 then
      sFound := rsFound
    else
      sFound := rsFoundPlural;
    if t = 1 then
      sReg := rsRecords
    else
      sReg := rsRecordsPlural;
    if m = 1 then
      sMarked := rsMarked
    else
      sMarked := rsMarkedPlural;
    ackMarcados.Enabled := f > 0;
    if ms = 0 then
      ackMarcados.State := cbUnchecked
    else
      if ms = m then
        ackMarcados.State := cbChecked
      else
        ackMarcados.State := cbGrayed;

    strTotal.Caption := Format(rsTotalRecords, [sFound, f, t, sReg]);
    ackMarcados.Caption := Format(rsTotalMarked, [ms, m, sMarked]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure RegAtual(aDataset: TDataset; strAtual: TLabel);
var
  f, a: Integer;
begin
  if aDataset.Active then
  begin
    f := aDataset.RecordCount;
    a := aDataset.RecNo;
  end
  else
  begin
    f := 0;
    a := 0;
  end;

  strAtual.Caption := Format(rsRecordNumber, [a, f]);
end;

procedure SetCountSQL(const aSQL: TStrings; aTable: TTableType);
begin
  case aTable of
    tbNone:
      ;
    tbUsers:
      CountUsersSQL(aSQL);
    tbRecordHistory:
      ;
    tbProjectTeams:
      ;
    tbPermits:
      ;
    tbGazetteer:
      CountGazetteerSQL(aSQL);
    tbBotanicTaxa:
      CountBotanicTaxaSQL(aSQL);
    tbNests:
      CountNestsSQL(aSQL);
    tbNestRevisions:
      ;
    tbEggs:
      ;
    tbNetStations:
      CountNetStationsSQL(aSQL);
    tbTaxonRanks:
      ;
    tbZooTaxa:
      CountZooTaxaSQL(aSQL);
    tbProjects:
      CountProjectsSQL(aSQL);
    tbInstitutions:
      CountInstitutionsSQL(aSQL);
    tbPeople:
      CountPeopleSQL(aSQL);
    tbExpeditions:
      CountExpeditionsSQL(aSQL);
    tbSurveys:
      CountSurveysSQL(aSQL);
    tbMethods:
      ;
    tbSurveyTeams:
      ;
    tbNetsEffort:
      ;
    tbSightings:
      CountSightingsSQL(aSQL);
    tbSpecimens:
      CountSpecimensSQL(aSQL);
    tbSamplePreps:
      ;
    tbPermanentNets:
      ;
    tbBands:
      CountBandsSQL(aSQL);
    tbIndividuals:
      CountIndividualsSQL(aSQL);
    tbCaptures:
      CountCapturesSQL(aSQL);
    tbMolts:
      ;
    tbImages:
      ;
    tbAudioLibrary:
      ;
  end;

end;

procedure CountIndividualsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT i.active_status AS ativo, i.marked_status AS marcado, i.*,');
    Add('(SELECT t.full_name FROM zoo_taxa AS t WHERE t.taxon_id = i.taxon_id) AS taxon_name,');
    Add('(SELECT h.formatted_name FROM zoo_taxa AS h WHERE h.taxon_id = i.taxon_id) AS taxon_formatted_name,');
    Add('(SELECT n.full_name FROM nests AS n WHERE n.nest_id = i.nest_id) AS nest_name,');
    Add('(SELECT a.full_name FROM bands AS a WHERE a.band_id = i.band_id) AS band_full_name,');
    Add('(SELECT d.full_name FROM bands AS d WHERE d.band_id = i.double_band_id) AS double_band_name,');
    Add('(SELECT r.full_name FROM bands AS r WHERE r.band_id = i.removed_band_id) AS removed_band_name,');
    Add('(SELECT p.full_name FROM individuals AS p WHERE p.individual_id = i.father_id) AS father_name,');
    Add('(SELECT m.full_name FROM individuals AS m WHERE m.individual_id = i.mother_id) AS mother_name,');
    // Add('(SELECT count(c.reg_num_interno) FROM XOL_MORFOMETRIA c WHERE (c.IND_CODIGO = i.reg_num_interno) and (c.active_status = 1)) as QUANT_CAPTURAS');
    Add('(SELECT sum(c.active_status) FROM captures AS c WHERE c.individual_id = i.individual_id) AS captures_tally');
    Add('FROM individuals AS i');
    // Add(')');
  end;
end;

procedure CountSpecimensSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT s.active_status AS ativo, s.marked_status AS marcado, s.*,');
    Add('(SELECT t.full_name FROM zoo_taxa AS t WHERE t.taxon_id = s.taxon_id) AS taxon_name,');
    Add('(SELECT g.site_name FROM gazetteer AS g WHERE g.site_id = s.locality_id) AS locality_name,');
    Add('(SELECT i.full_name FROM individuals AS i WHERE i.individual_id = s.individual_id) AS individual_name,');
    Add('(SELECT n.full_name FROM nests AS n WHERE n.nest_id = s.nest_id) AS nest_name,');
    Add('(SELECT o.full_name FROM eggs AS o WHERE o.egg_id = s.egg_id) AS egg_name');
    Add('FROM specimens AS s');
    // Add(')');
  end;
end;

procedure CountSurveysSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT a.active_status AS ativo, a.marked_status AS marcado, a.*,');
    Add('(SELECT m.site_name FROM gazetteer AS m WHERE m.site_id = a.municipality_id) AS municipality_name,');
    Add('(SELECT e.site_name FROM gazetteer AS e WHERE e.site_id = a.state_id) AS state_name,');
    Add('(SELECT p.site_name FROM gazetteer AS p WHERE p.site_id = a.country_id) AS country_name,');
    Add('(SELECT l.full_name FROM gazetteer AS l WHERE l.site_id = a.locality_id) AS locality_name,');
    Add('(SELECT s.station_name FROM net_stations AS s WHERE s.net_station_id = a.net_station_id) AS net_station_name,');
    Add('(SELECT t.method_name FROM methods AS t WHERE t.method_id = a.method_id) AS method_name,');
    Add('(SELECT j.project_title FROM projects AS j WHERE j.project_id = a.project_id) AS project_name,');
    Add('(SELECT cast(sum(net.net_area) + sum(net.open_time_total) AS real) FROM nets_effort AS net '
      + 'WHERE (net.survey_id = a.survey_id) AND (net.active_status = 1)) AS net_effort');
    Add('FROM surveys AS a');
    // Add(')');
  end;
end;

procedure CountExpeditionsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT active_status AS ativo, marked_status AS marcado, *');
//    Add('(SELECT m.site_name FROM gazetteer AS m WHERE m.site_id = a.municipality_id) AS municipality_name,');
//    Add('(SELECT e.site_name FROM gazetteer AS e WHERE e.site_id = a.state_id) AS state_name,');
//    Add('(SELECT p.site_name FROM gazetteer AS p WHERE p.site_id = a.country_id) AS country_name,');
//    Add('(SELECT l.full_name FROM gazetteer AS l WHERE l.site_id = a.locality_id) AS locality_name,');
//    Add('(SELECT j.project_title FROM projects AS j WHERE j.project_id = a.project_id) AS project_name,');
    Add('FROM expeditions');
    // Add(')');
  end;
end;

procedure CountSightingsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT c.active_status AS ativo, c.marked_status AS marcado, c.*,');
    Add('(SELECT t.full_name FROM zoo_taxa AS t WHERE t.taxon_id = c.taxon_id) AS taxon_name,');
    Add('(SELECT h.formatted_name FROM zoo_taxa AS h WHERE h.taxon_id = c.taxon_id) AS taxon_html_name,');
    Add('(SELECT i.full_name FROM individuals AS i WHERE i.individual_id = c.individual_id) AS individual_name,');
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = c.observer_id) AS observer_name,');
    Add('(SELECT a.full_name FROM surveys AS a WHERE a.survey_id = c.survey_id) AS survey_name,');
    Add('(SELECT m.method_name FROM methods AS m WHERE m.method_id = c.method_id) AS method_name,');
    Add('(SELECT g.full_name FROM gazetteer AS g WHERE g.site_id = c.locality_id) AS locality_name');
    Add('FROM sightings AS c');
    // Add(')');
  end;
end;

procedure CountBotanicTaxaSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT b.active_status AS ativo, b.marked_status AS marcado, b.*,');
    Add('(SELECT s.taxon_name FROM botanic_taxa AS s WHERE s.taxon_id = b.parent_taxon_id) AS parent_name,');
    Add('(SELECT v.taxon_name FROM botanic_taxa AS v WHERE v.taxon_id = b.valid_id) AS valid_name');
    Add('FROM botanic_taxa AS b');
    // Add(')');
  end;
end;

procedure CountGazetteerSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT g.active_status AS ativo, g.marked_status AS marcado, g.*,');
    Add('(SELECT s.site_name FROM gazetteer AS s WHERE s.site_id = g.parent_site_id) AS parent_site_name,');
    Add('(SELECT m.site_name FROM gazetteer AS m WHERE m.site_id = g.municipality_id) AS municipality_name,');
    Add('(SELECT e.site_name FROM gazetteer AS e WHERE e.site_id = g.state_id) AS state_name,');
    Add('(SELECT p.site_name FROM gazetteer AS p WHERE p.site_id = g.country_id) AS country_name');
    Add('FROM gazetteer AS g');
    // Add(')');
  end;
end;

procedure CountNetStationsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT n.active_status AS ativo, n.marked_status AS marcado, n.*,');
    Add('(SELECT l.site_name FROM gazetteer AS l WHERE l.site_id = n.locality_id) AS locality_name,');
    Add('(SELECT m.site_name FROM gazetteer AS m WHERE m.site_id = n.municipality_id) AS municipality_name,');
    Add('(SELECT e.site_name FROM gazetteer AS e WHERE e.site_id = n.state_id) AS state_name,');
    Add('(SELECT p.site_name FROM gazetteer AS p WHERE p.site_id = n.country_id) AS country_name');
    Add('FROM net_stations AS n');
    // Add(')');
  end;
end;

procedure CountInstitutionsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT i.active_status AS ativo, i.marked_status AS marcado, i.*,');
    Add('(SELECT m.site_name FROM gazetteer AS m WHERE m.site_id = i.municipality_id) AS municipality_name,');
    Add('(SELECT e.site_name FROM gazetteer AS e WHERE e.site_id = i.state_id) AS state_name,');
    Add('(SELECT c.site_name FROM gazetteer AS c WHERE c.site_id = i.country_id) AS country_name');
    Add('FROM institutions AS i');
    // Add(')');
  end;
end;

procedure CountNestsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT n.active_status AS ativo, n.marked_status AS marcado, n.*,');
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = n.observer_id) AS observer_name,');
    Add('(SELECT j.project_title FROM projects AS j WHERE j.project_id = n.project_id) AS project_name,');
    Add('(SELECT g.site_name FROM gazetteer AS g WHERE g.site_id = n.locality_id) AS locality_name,');
    Add('(SELECT t.full_name FROM zoo_taxa AS t WHERE t.taxon_id = n.taxon_id) AS taxon_name,');
    Add('(SELECT t.formatted_name FROM zoo_taxa AS t WHERE t.taxon_id = n.taxon_id) AS taxon_html_name,');
    Add('(SELECT m.full_name FROM individuals AS m WHERE m.individual_id = n.male_id) AS male_name,');
    Add('(SELECT f.full_name FROM individuals AS f WHERE f.individual_id = n.female_id) AS female_name,');
    Add('(SELECT h1.full_name FROM individuals AS h1 WHERE h1.individual_id = n.helper_1_id) AS helper_1_name,');
    Add('(SELECT h2.full_name FROM individuals AS h2 WHERE h2.individual_id = n.helper_2_id) AS helper_2_name,');
    Add('(SELECT h3.full_name FROM individuals AS h3 WHERE h3.individual_id = n.helper_3_id) AS helper_3_name,');
    Add('(SELECT b.taxon_name FROM botanic_taxa AS b WHERE b.taxon_id = n.support_plant_1_id) AS support_plant_1_name,');
    Add('(SELECT b2.taxon_name FROM botanic_taxa AS b2 WHERE b2.taxon_id = n.support_plant_2_id) AS support_plant_2_name');
    Add('FROM nests AS n');
    // Add(')');
  end;
end;

procedure CountCapturesSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT m.active_status AS ativo, m.marked_status AS marcado, m.*,');
    Add('(SELECT t.full_name FROM zoo_taxa AS t WHERE t.taxon_id = m.taxon_id) AS taxon_name,');
    Add('(SELECT h.formatted_name FROM zoo_taxa AS h WHERE h.taxon_id = m.taxon_id) AS taxon_formatted_name,');
    Add('(SELECT s.full_name FROM surveys AS s WHERE s.survey_id = m.survey_id) AS survey_name,');
    Add('(SELECT ns.station_name FROM net_stations AS ns WHERE ns.net_station_id = m.net_station_id) AS net_station_name,');
    Add('(SELECT n.net_number FROM nets_effort AS n WHERE n.net_id = m.net_id) AS net_num,');
    Add('(SELECT g.site_name FROM gazetteer AS g WHERE g.site_id = m.locality_id) AS locality_name,');
    Add('(SELECT b.acronym FROM people AS b WHERE b.person_id = m.bander_id) AS bander_name,');
    Add('(SELECT w.acronym FROM people AS w WHERE w.person_id = m.annotator_id) AS annotator_name,');
    Add('(SELECT a.band_size||'' ''||a.band_number FROM bands AS a WHERE a.band_id = m.band_id) AS band_name,');
    Add('(SELECT r.band_size||'' ''||r.band_number FROM bands AS r WHERE r.band_id = m.removed_band_id) AS removed_band_name,');
    Add('(SELECT f.acronym FROM people AS f WHERE f.person_id = m.photographer_1_id) AS photographer_1_name,');
    Add('(SELECT f2.acronym FROM people AS f2 WHERE f2.person_id = m.photographer_2_id) AS photographer_2_name');
    Add('FROM captures AS m');
    // Add(')');
  end;
end;

procedure CountPeopleSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT p.active_status AS ativo, p.marked_status AS marcado, p.*,');
    Add('(SELECT m.site_name FROM gazetteer AS m WHERE m.site_id = p.municipality_id) AS municipality_name,');
    Add('(SELECT e.site_name FROM gazetteer AS e WHERE e.site_id = p.state_id) AS state_name,');
    Add('(SELECT c.site_name FROM gazetteer AS c WHERE c.site_id = p.country_id) AS country_name,');
    Add('(SELECT i.full_name FROM institutions AS i WHERE i.institution_id = p.institution_id) AS institution_name');
    Add('FROM people AS p');
    // Add(')');
  end;
end;

procedure CountProjectsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT active_status AS ativo, marked_status AS marcado, *');
    Add('FROM projects');
    // Add(')');
  end;
end;

procedure CountZooTaxaSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT t.active_status AS ativo, t.marked_status AS marcado, t.*,');
    Add('(SELECT u.full_name FROM zoo_taxa AS u WHERE u.taxon_id = t.parent_taxon_id) AS parent_taxon_name,');
    Add('(SELECT v.full_name FROM zoo_taxa AS v WHERE v.taxon_id = t.valid_id) AS valid_name,');
    Add('(SELECT ui.full_name FROM zoo_taxa AS ui WHERE ui.taxon_id = t.ioc_parent_taxon_id) AS ioc_parent_name,');
    Add('(SELECT vi.full_name FROM zoo_taxa AS vi WHERE vi.taxon_id = t.ioc_valid_id) AS ioc_valid_name,');
    Add('(SELECT uc.full_name FROM zoo_taxa AS uc WHERE uc.taxon_id = t.cbro_parent_taxon_id) AS cbro_parent_name,');
    Add('(SELECT vc.full_name FROM zoo_taxa AS vc WHERE vc.taxon_id = t.cbro_valid_id) AS cbro_valid_name,');
    Add('(SELECT o.full_name FROM zoo_taxa AS o WHERE o.taxon_id = t.order_id) AS order_name,');
    Add('(SELECT f.full_name FROM zoo_taxa AS f WHERE f.taxon_id = t.family_id) AS family_name,');
    Add('(SELECT s.full_name FROM zoo_taxa AS s WHERE s.taxon_id = t.subfamily_id) AS subfamily_name,');
    Add('(SELECT n.full_name FROM zoo_taxa AS n WHERE n.taxon_id = t.genus_id) AS genero_name,');
    Add('(SELECT e.full_name FROM zoo_taxa AS e WHERE e.taxon_id = t.species_id) AS species_name,');
    Add('(SELECT g.full_name FROM zoo_taxa AS g WHERE g.taxon_id = t.subspecies_group_id) AS subspecies_group_name');
    Add('FROM zoo_taxa AS t');
    // Add(')');
  end;
end;

procedure CountBandsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT a.active_status AS ativo, a.marked_status AS marcado, a.*,');
    Add('(SELECT f.acronym FROM institutions AS f WHERE f.institution_id = a.supplier_id) AS supplier_name,');
    Add('(SELECT s.full_name FROM people AS s WHERE s.person_id = a.requester_id) AS requester_name,');
    Add('(SELECT p.full_name FROM people AS p WHERE p.person_id = a.carrier_id) AS carrier_name,');
    Add('(SELECT r.full_name FROM people AS r WHERE r.person_id = a.sender_id) AS sender_name,');
    Add('(SELECT i.formatted_name FROM individuals AS i WHERE i.individual_id = a.individual_id) AS individual_name,');
    Add('(SELECT l.project_title FROM projects AS l WHERE l.project_id = a.project_id) AS project_name');
    Add('FROM bands AS a');
    // Add(')');
  end;
end;

procedure CountUsersSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH');
    Add('lista AS (');
    Add('SELECT active_status AS ativo, marked_status AS marcado, *');
    Add('FROM users');
    // Add(')');
  end;
end;

end.

