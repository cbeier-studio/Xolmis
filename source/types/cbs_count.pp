{ Xolmis Record Count library

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
  procedure CountSamplingPlotsSQL(const aSQL: TStrings);
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
    tbSamplingPlots:
      CountSamplingPlotsSQL(aSQL);
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
    Add('WITH lista AS (');
    Add(' SELECT i.active_status AS ativo, i.marked_status AS marcado, i.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.formatted_name AS taxon_formatted_name,');
    Add('  n.full_name AS nest_name,');
    Add('  b1.full_name AS band_full_name,');
    Add('  b2.full_name AS double_band_name,');
    Add('  b3.full_name AS removed_band_name,');
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
    // Add(')');
  end;
end;

procedure CountSpecimensSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT sp.active_status AS ativo, sp.marked_status AS marcado, sp.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  g.site_name AS locality_name,');
    Add('  i.full_name AS individual_name,');
    Add('  n.full_name AS nest_name,');
    Add('  e.full_name AS egg_name');
    Add('FROM specimens AS sp');
    Add('LEFT JOIN zoo_taxa AS z ON sp.taxon_id = z.taxon_id');
    Add('LEFT JOIN gazetteer AS g ON sp.locality_id = g.site_id');
    Add('LEFT JOIN individuals AS i ON sp.individual_id = i.individual_id');
    Add('LEFT JOIN nests AS n ON sp.nest_id = n.nest_id');
    Add('LEFT JOIN eggs AS e ON sp.egg_id = e.egg_id');
    // Add(')');
  end;
end;

procedure CountSurveysSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT sv.active_status AS ativo, sv.marked_status AS marcado, sv.*,');
    Add('  x.expedition_name AS expedition_name,');
    Add('  gl.full_name AS locality_name,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name,');
    Add('  ns.station_name AS net_station_name,');
    Add('  mt.method_name AS method_name,');
    Add('  pj.short_title AS project_name,');
    Add('  (SELECT CAST((SUM(ef.net_area) + SUM(ef.open_time_total)) AS REAL) FROM nets_effort AS ef');
    Add('    WHERE (ef.survey_id = sv.survey_id) AND (ef.active_status = 1)) AS net_effort');
    Add('FROM surveys AS sv');
    Add('LEFT JOIN expeditions AS x ON sv.expedition_id = x.expedition_id');
    Add('LEFT JOIN gazetteer AS gl ON sv.locality_id = gl.site_id');
    Add('LEFT JOIN gazetteer AS gm ON sv.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON sv.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON sv.country_id = gc.site_id');
    Add('LEFT JOIN net_stations AS ns ON sv.net_station_id = ns.net_station_id');
    Add('LEFT JOIN methods AS mt ON sv.method_id = mt.method_id');
    Add('LEFT JOIN projects AS pj ON sv.project_id = pj.project_id');
    // Add(')');
  end;
end;

procedure CountExpeditionsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT x.active_status AS ativo, x.marked_status AS marcado, x.*');
    Add('  gl.full_name AS locality_name,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name,');
    Add('  pj.short_title AS project_name');
    Add('FROM expeditions AS x');
    Add('LEFT JOIN gazetteer AS gl ON x.locality_id = gl.site_id');
    Add('LEFT JOIN gazetteer AS gm ON x.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON x.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON x.country_id = gc.site_id');
    Add('LEFT JOIN projects AS pj ON x.project_id = pj.project_id');
    // Add(')');
  end;
end;

procedure CountSightingsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT s.active_status AS ativo, s.marked_status AS marcado, s.*,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.formatted_name AS taxon_formatted_name,');
    Add('  i.full_name AS individual_name,');
    Add('  p.full_name AS observer_name,');
    Add('  sv.full_name AS survey_name,');
    Add('  mt.method_name AS method_name,');
    Add('  g.full_name AS locality_name');
    Add('FROM sightings AS s');
    Add('LEFT JOIN zoo_taxa AS z ON s.taxon_id = z.taxon_id');
    Add('LEFT JOIN individuals AS i ON s.individual_id = i.individual_id');
    Add('LEFT JOIN people AS p ON s.observer_id = p.person_id');
    Add('LEFT JOIN surveys AS sv ON s.survey_id = sv.survey_id');
    Add('LEFT JOIN methods AS mt ON s.method_id = mt.method_id');
    Add('LEFT JOIN gazetteer AS g ON s.locality_id = g.site_id');
    // Add(')');
  end;
end;

procedure CountBotanicTaxaSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT bt.active_status AS ativo, bt.marked_status AS marcado, bt.*,');
    Add('  btp.taxon_name AS parent_taxon_name,');
    Add('  btv.taxon_name AS valid_name');
    Add('FROM botanic_taxa AS bt');
    Add('LEFT JOIN botanic_taxa AS btp ON bt.parent_taxon_id = btp.taxon_id');
    Add('LEFT JOIN botanic_taxa AS btv ON bt.valid_id = btv.taxon_id');
    // Add(')');
  end;
end;

procedure CountGazetteerSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT g.active_status AS ativo, g.marked_status AS marcado, g.*,');
    Add('   gp.site_name AS parent_site_name,');
    Add('   gm.site_name AS municipality_name,');
    Add('   gs.site_name AS state_name,');
    Add('   gc.site_name AS country_name');
    Add('FROM gazetteer AS g');
    Add('LEFT JOIN gazetteer AS gp ON g.parent_site_id = gp.site_id');
    Add('LEFT JOIN gazetteer AS gm ON g.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON g.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON g.country_id = gc.site_id');
    // Add(')');
  end;
end;

procedure CountSamplingPlotsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT pl.active_status AS ativo, ns.marked_status AS marcado, pl.*,');
    Add('  gl.site_name AS locality_name,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name');
    Add('FROM sampling_plots AS pl');
    Add('LEFT JOIN gazetteer AS gl ON pl.locality_id = gl.site_id');
    Add('LEFT JOIN gazetteer AS gm ON pl.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON pl.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON pl.country_id = gc.site_id');
    // Add(')');
  end;
end;

procedure CountInstitutionsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT it.active_status AS ativo, it.marked_status AS marcado, it.*,');
    Add('   gm.site_name AS municipality_name,');
    Add('   gs.site_name AS state_name,');
    Add('   gc.site_name AS country_name');
    Add('FROM institutions AS it');
    Add('LEFT JOIN gazetteer AS gm ON it.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON it.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON it.country_id = gc.site_id');
    // Add(')');
  end;
end;

procedure CountNestsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT n.active_status AS ativo, n.marked_status AS marcado, n.*,');
    Add('  p.full_name AS observer_name,');
    Add('  pj.project_title AS project_name,');
    Add('  g.site_name locality_name,');
    Add('  z.full_name AS taxon_name,');
    Add('  z.formatted_name AS taxon_formatted_name,');
    Add('  bt1.taxon_name AS support_plant_1_name,');
    Add('  bt2.taxon_name AS support_plant_2_name');
    Add('FROM nests AS n');
    Add('LEFT JOIN people AS p ON n.observer_id = p.person_id');
    Add('LEFT JOIN projects AS pj ON n.project_id = pj.project_id');
    Add('LEFT JOIN gazetteer AS g ON n.locality_id = g.site_id');
    Add('LEFT JOIN zoo_taxa AS z ON n.taxon_id = z.taxon_id');
    Add('LEFT JOIN botanic_taxa AS bt1 ON n.support_plant_1_id = bt1.taxon_id');
    Add('LEFT JOIN botanic_taxa AS bt2 ON n.support_plant_2_id = bt2.taxon_id');
    // Add(')');
  end;
end;

procedure CountCapturesSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT c.active_status AS ativo, c.marked_status AS marcado, c.*,');
    Add('   z.full_name AS taxon_name,');
    Add('   z.formatted_name AS taxon_formatted_name,');
    Add('   sv.full_name AS survey_name,');
    Add('   ns.station_name AS net_station_name,');
    Add('   ef.net_number AS net_number,');
    Add('   g.site_name AS locality_name,');
    Add('   p1.acronym AS bander_name,');
    Add('   p2.acronym AS annotator_name,');
    Add('   (b1.band_size||'' ''||b1.band_number) AS band_name,');
    Add('   (b2.band_size||'' ''||b2.band_number) AS removed_band_name,');
    Add('   f1.acronym AS photographer_1_name,');
    Add('   f2.acronym AS photographer_2_name');
    Add('FROM captures AS c');
    Add('LEFT JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id');
    Add('LEFT JOIN surveys AS sv ON c.survey_id = sv.survey_id');
    Add('LEFT JOIN net_stations AS ns ON c.net_station_id = ns.net_station_id');
    Add('LEFT JOIN nets_effort AS ef ON c.net_id = ef.net_id');
    Add('LEFT JOIN gazetteer AS g ON c.locality_id = g.site_id');
    Add('LEFT JOIN people AS p1 ON c.bander_id = p1.person_id');
    Add('LEFT JOIN people AS p2 ON c.annotator_id = p2.person_id');
    Add('LEFT JOIN people AS f1 ON c.photographer_1_id = f1.person_id');
    Add('LEFT JOIN people AS f2 ON c.photographer_2_id = f2.person_id');
    Add('LEFT JOIN bands AS b1 ON c.band_id = b1.band_id');
    Add('LEFT JOIN bands AS b2 ON c.removed_band_id = b2.band_id');
    // Add(')');
  end;
end;

procedure CountPeopleSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT p.active_status AS ativo, p.marked_status AS marcado, p.*,');
    Add('  gm.site_name AS municipality_name,');
    Add('  gs.site_name AS state_name,');
    Add('  gc.site_name AS country_name,');
    Add('  it.full_name AS institution_name');
    Add('FROM people AS p');
    Add('LEFT JOIN gazetteer AS gm ON p.municipality_id = gm.site_id');
    Add('LEFT JOIN gazetteer AS gs ON p.state_id = gs.site_id');
    Add('LEFT JOIN gazetteer AS gc ON p.country_id = gc.site_id');
    Add('LEFT JOIN institutions AS it ON p.institution_id = it.institution_id');
    // Add(')');
  end;
end;

procedure CountProjectsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
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
    Add('WITH lista AS (');
    Add('SELECT z.active_status AS ativo, z.marked_status AS marcado, z.*,');
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
    // Add(')');
  end;
end;

procedure CountBandsSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT b.active_status AS ativo, b.marked_status AS marcado, b.*,');
    Add('  it.acronym AS supplier_name,');
    Add('  p.full_name AS carrier_name,');
    Add('  i.full_name AS individual_name,');
    Add('  pj.short_title AS project_name');
    Add('FROM bands AS b');
    Add('LEFT JOIN institutions AS it ON b.supplier_id = it.institution_id');
    Add('LEFT JOIN people AS p ON b.carrier_id = p.person_id');
    Add('LEFT JOIN individuals AS i ON b.individual_id = i.individual_id');
    Add('LEFT JOIN projects AS pj ON b.project_id = pj.project_id');
    // Add(')');
  end;
end;

procedure CountUsersSQL(const aSQL: TStrings);
begin
  with aSQL do
  begin
    Clear;
    Add('WITH lista AS (');
    Add('SELECT active_status AS ativo, marked_status AS marcado, *');
    Add('FROM users');
    // Add(')');
  end;
end;

end.

