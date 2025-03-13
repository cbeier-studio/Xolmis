{ Xolmis Data Filters library

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

unit cbs_filters;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, LCLIntf, gettext, StrUtils,
  { VCL }
  Graphics, ImgList, Controls, Dialogs, Forms, laz.VirtualTrees,
  { Data }
  DB, SQLDB,
  { CBS }
  cbs_datatypes, cbs_taxonomy, cbs_gis;

type

  TDatePart = (dpYear, dpMonth, dpDay);

  { TBasicNodeData }

  TBasicNodeData = class
    Caption    : String;
    Id         : Integer;
    ImageIndex : Integer;
    Checked    : Boolean;
  end;
  PBasicNodeData = ^TBasicNodeData;

  TTaxonNodeData = class(TBasicNodeData)
    Rank: TZooRank;
  end;
  PTaxonNodeData = ^TTaxonNodeData;

  TSiteNodeData = class(TBasicNodeData)
    Rank: TSiteRank;
  end;
  PSiteNodeData = ^TSiteNodeData;

  TDateNodeData = class(TBasicNodeData)
    Part: TDatePart;
  end;
  PDateNodeData = ^TDateNodeData;

  { Load lists and trees }
  procedure LoadTaxaTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);

  procedure LoadSpecimenDateTree(aSQL: TStrings);
  procedure LoadNestDateTree(aSQL: TStrings);
  procedure LoadNestRevisionDateTree(aSQL: TStrings);
  procedure LoadEggDateTree(aSQL: TStrings);
  procedure LoadSightingDateTree(aSQL: TStrings);
  procedure LoadCaptureDateTree(aSQL: TStrings);
  procedure LoadMoltDateTree(aSQL: TStrings);
  procedure LoadFeatherDateTree(aSQL: TStrings);
  procedure LoadExpeditionDateTree(aSQL: TStrings);
  procedure LoadSurveyDateTree(aSQL: TStrings);
  procedure LoadIndividualDateTree(aSQL: TStrings);
  procedure LoadProjectDateTree(aSQL: TStrings);
  procedure LoadPermitDateTree(aSQL: TStrings);
  procedure LoadPeopleDateTree(aSQL: TStrings);
  procedure LoadDateTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);

  procedure LoadSiteTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);

  { Filter auxiliary functions }
  procedure FilterSurveyDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterExpeditionDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterSpecimenDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterNestDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterSightingDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterCaptureDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterMoltDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterBandDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterIndividualDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterProjectDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  procedure FilterPeopleDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);

  { Filter and sort records }
  function Filtrar(aTabela: TTableType; aDataset: TSQLQuery; aWhere: TStrings; aFilterShow: TStrings = nil): Boolean;
  function TaxonFilterToString(aVirtualTree: TBaseVirtualTree; aPrefix: String = ''): String;
  function TaxonFilterToSearch(aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
  function SiteFilterToString(aVirtualTree: TBaseVirtualTree; aPrefix: String = ''): String;
  function SiteFilterToSearch(aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
  function DateFilterToString(aTable: TTableType; aVirtualTree: TBaseVirtualTree; var aTotal: Integer): String;
  function DateFilterToSearch(aTable: TTableType; aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
  function PersonFilterToSearch(aTable: TTableType; aSearchGroup: TSearchGroups; aKey: Integer = 0): Boolean;


implementation

uses cbs_global, udm_main;

procedure LoadTaxaTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);
var
  aOrder, aFamily, aSpecies: String;
  aOrderId, aFamilyId, aSpeciesId: Integer;
  Qry: TSQLQuery;
  Data: TTaxonNodeData;
  xNode, orderParent, familyParent: PVirtualNode;
begin
  //Lista := TStringList.Create;
  //STree := TMemoryStream.Create;
  //STree.Position := 0;
  Qry := TSQLQuery.Create(DMM.sqlCon);
  Qry.Database := DMM.sqlCon;
  with Qry, SQL do
  try
    SQL.Clear;

    Add('WITH TaxaDetails AS (');
    Add('SELECT');
    Add('    taxon_id,');
    Add('    full_name,');
    Add('    sort_num,');
    Add('    species_id,');
    Add('    family_id,');
    Add('    order_id');
    Add('FROM zoo_taxa');
    Add(')');
    case aTable of
      tbNone:
        begin
          Add('SELECT i.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM individuals AS i');
          Add('JOIN TaxaDetails AS z ON i.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (i.active_status = 1)');
          Add('UNION');
          Add('SELECT c.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM captures AS c');
          Add('JOIN TaxaDetails AS z ON c.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (c.active_status = 1)');
          Add('UNION');
          Add('SELECT m.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM molts AS m');
          Add('JOIN TaxaDetails AS z ON m.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (m.active_status = 1)');
          Add('UNION');
          Add('SELECT ac.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM sightings AS ac');
          Add('JOIN TaxaDetails AS z ON ac.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (ac.active_status = 1)');
          Add('UNION');
          Add('SELECT n.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM nests AS n');
          Add('JOIN TaxaDetails AS z ON n.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (n.active_status = 1)');
          Add('UNION');
          Add('SELECT e.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM eggs AS e');
          Add('JOIN TaxaDetails AS z ON e.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (e.active_status = 1)');
          Add('UNION');
          Add('SELECT sp.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM specimens AS sp');
          Add('JOIN TaxaDetails AS z ON sp.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (sp.active_status = 1)');
        end;
      tbIndividuals:
        begin
          Add('SELECT i.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM individuals AS i');
          Add('JOIN TaxaDetails AS z ON i.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (i.active_status = 1)');
        end;
      tbCaptures:
        begin
          Add('SELECT c.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM captures AS c');
          Add('JOIN TaxaDetails AS z ON c.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (c.active_status = 1)');
        end;
      tbMolts:
        begin
          Add('SELECT m.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM molts AS m');
          Add('JOIN TaxaDetails AS z ON m.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (m.active_status = 1)');
        end;
      tbFeathers:
        begin
          Add('SELECT ft.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM feathers AS ft');
          Add('JOIN TaxaDetails AS z ON ft.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (ft.active_status = 1)');
        end;
      tbSightings:
        begin
          Add('SELECT ac.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM sightings AS ac');
          Add('JOIN TaxaDetails AS z ON ac.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (ac.active_status = 1)');
        end;
      tbNests:
        begin
          Add('SELECT n.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM nests AS n');
          Add('JOIN TaxaDetails AS z ON n.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (n.active_status = 1)');
        end;
      tbEggs:
        begin
          Add('SELECT e.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM eggs AS e');
          Add('JOIN TaxaDetails AS z ON e.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (e.active_status = 1)');
        end;
      tbSpecimens:
        begin
          Add('SELECT sp.taxon_id, z.species_id, z.family_id, z.order_id,');
          Add('  s.full_name AS species_name,');
          Add('  f.full_name AS family_name,');
          Add('  o.full_name AS order_name,');
          Add('  z.sort_num AS sort_num');
          Add('FROM specimens AS sp');
          Add('JOIN TaxaDetails AS z ON sp.taxon_id = z.taxon_id');
          Add('JOIN TaxaDetails AS s ON z.species_id = s.taxon_id');
          Add('JOIN TaxaDetails AS f ON z.family_id = f.taxon_id');
          Add('JOIN TaxaDetails AS o ON z.order_id = o.taxon_id');
          Add('WHERE (sp.active_status = 1)');
        end;
      //tbImages: ;
      //tbAudioRecordings: ;
    else
      Exit;
    end;

    Add('GROUP BY z.order_id, z.family_id, z.species_id');
    Add('ORDER BY sort_num ASC');
    //{$IFDEF DEBUG}
    //LogSQL(SQL);
    //{$ENDIF}
    Open;
    if RecordCount > 0 then
    try
      // PBar.Max:= RecordCount;
      // PBar.Position:= 0;
      // PBar.Visible:= True;
      aVirtualTree.BeginUpdate;
      if aVirtualTree.TotalCount > 0 then
        aVirtualTree.Clear;
      aOrder := EmptyStr;
      aFamily := EmptyStr;
      aSpecies := EmptyStr;
      orderParent := nil;
      familyParent := nil;

      // Cria lista para arvore
      First;
      repeat
        if (FieldByName('order_name').AsString <> aOrder) and
          (FieldByName('order_name').AsString <> EmptyStr) then
        begin
          aOrder := FieldByName('order_name').AsString;
          aOrderId := FieldByName('order_id').AsInteger;
          Data := TTaxonNodeData.Create;
          Data.Caption := aOrder;
          Data.Id := aOrderId;
          Data.Rank := trOrder;
          Data.ImageIndex := FirstIconIndex;
          Data.Checked := False;
          xNode := aVirtualTree.AddChild(nil, Data);
          orderParent := xNode;
        end;
        if (FieldByName('family_name').AsString <> aFamily) and
          (FieldByName('family_name').AsString <> EmptyStr) then
        begin
          aFamily := FieldByName('family_name').AsString;
          aFamilyId := FieldByName('family_id').AsInteger;
          Data := TTaxonNodeData.Create;
          Data.Caption := aFamily;
          Data.Id := aFamilyId;
          Data.Rank := trFamily;
          Data.ImageIndex := FirstIconIndex;
          if FirstIconIndex > -1 then
            Data.ImageIndex := Data.ImageIndex + 1;
          Data.Checked := False;
          xNode := aVirtualTree.AddChild(orderParent, Data);
          familyParent := xNode;
        end;
        if (FieldByName('species_name').AsString <> aSpecies) and
          (FieldByName('species_name').AsString <> EmptyStr) then
        begin
          aSpecies := FieldByName('species_name').AsString;
          aSpeciesId := FieldByName('species_id').AsInteger;
          Data := TTaxonNodeData.Create;
          Data.Caption := aSpecies;
          Data.Id := aSpeciesId;
          Data.Rank := trSpecies;
          Data.ImageIndex := FirstIconIndex;
          if FirstIconIndex > -1 then
            Data.ImageIndex := Data.ImageIndex + 2;
          Data.Checked := False;
          aVirtualTree.AddChild(familyParent, Data);
        end;

        // PBar.Position:= RecNo;
        Next;
      until EOF;
      // Lista.SaveToFile('TaxonTree.txt');
      //Lista.SaveToStream(STree);
      // STree.SaveToFile('TaxonTreeStream.txt');
      //STree.Position := 0;
      //aTree.LoadFromStream(STree);
    finally
      aVirtualTree.EndUpdate;
      // PBar.Visible:= False;
      // PBar.Position:= 0;
      Close;
    end;
  finally
    FreeAndNil(Qry);
    //Lista.Free;
    //STree.Free;
  end;
  aVirtualTree.FullCollapse;
  //for i := 0 to aTree.Items.Count - 1 do
  //begin
  //  aTree.Items.Item[i].ImageIndex := aTree.Items.Item[i].Level;
  //  aTree.Items.Item[i].SelectedIndex := aTree.Items.Item[i].Level;
  //end;
  aVirtualTree.ClearSelection;
  // SBarTaxon.Caption:= 'Táxons com registros: '+IntToStr(TV.Items.Count);
end;

procedure LoadSpecimenDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', sp.collection_date) AS ano,');
  aSQL.Add('strftime(''%m'', sp.collection_date) AS mes,');
  aSQL.Add('strftime(''%d'', sp.collection_date) AS dia');
  aSQL.Add('FROM specimens AS sp WHERE (sp.active_status = 1)');
end;

procedure LoadNestDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', n.found_date) AS ano,');
  aSQL.Add('strftime(''%m'', n.found_date) AS mes,');
  aSQL.Add('strftime(''%d'', n.found_date) AS dia');
  aSQL.Add('FROM nests AS n WHERE (n.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', n.last_date) AS ano,');
  aSQL.Add('strftime(''%m'', n.last_date) AS mes,');
  aSQL.Add('strftime(''%d'', n.last_date) AS dia');
  aSQL.Add('FROM nests AS n WHERE (n.active_status = 1)');
end;

procedure LoadNestRevisionDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', nr.revision_date) AS ano,');
  aSQL.Add('strftime(''%m'', nr.revision_date) AS mes,');
  aSQL.Add('strftime(''%d'', nr.revision_date) AS dia');
  aSQL.Add('FROM nest_revisions AS nr WHERE (nr.active_status = 1)');
end;

procedure LoadEggDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', e.measure_date) AS ano,');
  aSQL.Add('strftime(''%m'', e.measure_date) AS mes,');
  aSQL.Add('strftime(''%d'', e.measure_date) AS dia');
  aSQL.Add('FROM eggs AS e WHERE (e.active_status = 1)');
end;

procedure LoadSightingDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', s.sighting_date) AS ano,');
  aSQL.Add('strftime(''%m'', s.sighting_date) AS mes,');
  aSQL.Add('strftime(''%d'', s.sighting_date) AS dia');
  aSQL.Add('FROM sightings AS s WHERE (s.active_status = 1)');
end;

procedure LoadCaptureDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', c.capture_date) AS ano,');
  aSQL.Add('strftime(''%m'', c.capture_date) AS mes,');
  aSQL.Add('strftime(''%d'', c.capture_date) AS dia');
  aSQL.Add('FROM captures AS c WHERE (c.active_status = 1)');
end;

procedure LoadMoltDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', m.sample_date) AS ano,');
  aSQL.Add('strftime(''%m'', m.sample_date) AS mes,');
  aSQL.Add('strftime(''%d'', m.sample_date) AS dia');
  aSQL.Add('FROM molts AS m WHERE (m.active_status = 1)');
end;

procedure LoadFeatherDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', ft.sample_date) AS ano,');
  aSQL.Add('strftime(''%m'', ft.sample_date) AS mes,');
  aSQL.Add('strftime(''%d'', ft.sample_date) AS dia');
  aSQL.Add('FROM feathers AS ft WHERE (ft.active_status = 1)');
end;

procedure LoadExpeditionDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', x.start_date) AS ano,');
  aSQL.Add('strftime(''%m'', x.start_date) AS mes,');
  aSQL.Add('strftime(''%d'', x.start_date) AS dia');
  aSQL.Add('FROM expeditions AS x WHERE (x.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', x.end_date) AS ano,');
  aSQL.Add('strftime(''%m'', x.end_date) AS mes,');
  aSQL.Add('strftime(''%d'', x.end_date) AS dia');
  aSQL.Add('FROM expeditions AS x WHERE (x.active_status = 1)');
end;

procedure LoadSurveyDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', sv.survey_date) AS ano,');
  aSQL.Add('strftime(''%m'', sv.survey_date) AS mes,');
  aSQL.Add('strftime(''%d'', sv.survey_date) AS dia');
  aSQL.Add('FROM surveys AS sv WHERE (sv.active_status = 1)');
end;

procedure LoadIndividualDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('i.birth_year AS ano,');
  aSQL.Add('i.birth_month AS mes,');
  aSQL.Add('i.birth_day AS dia');
  aSQL.Add('FROM individuals AS i WHERE (i.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', i.banding_date) AS ano,');
  aSQL.Add('strftime(''%m'', i.banding_date) AS mes,');
  aSQL.Add('strftime(''%d'', i.banding_date) AS dia');
  aSQL.Add('FROM individuals AS i WHERE (i.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', i.band_change_date) AS ano,');
  aSQL.Add('strftime(''%m'', i.band_change_date) AS mes,');
  aSQL.Add('strftime(''%d'', i.band_change_date) AS dia');
  aSQL.Add('FROM individuals AS i WHERE (i.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('i.death_year AS ano,');
  aSQL.Add('i.death_month AS mes,');
  aSQL.Add('i.death_day AS dia');
  aSQL.Add('FROM individuals AS i WHERE (i.active_status = 1)');
end;

procedure LoadProjectDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', pj.start_date) AS ano,');
  aSQL.Add('strftime(''%m'', pj.start_date) AS mes,');
  aSQL.Add('strftime(''%d'', pj.start_date) AS dia');
  aSQL.Add('FROM projects AS pj WHERE (pj.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', pj.end_date) AS ano,');
  aSQL.Add('strftime(''%m'', pj.end_date) AS mes,');
  aSQL.Add('strftime(''%d'', pj.end_date) AS dia');
  aSQL.Add('FROM projects AS pj WHERE (pj.active_status = 1)');
end;

procedure LoadPermitDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', l.dispatch_date) AS ano,');
  aSQL.Add('strftime(''%m'', l.dispatch_date) AS mes,');
  aSQL.Add('strftime(''%d'', l.dispatch_date) AS dia');
  aSQL.Add('FROM legal AS l WHERE (l.active_status = 1) UNION');
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', l.expire_date) AS ano,');
  aSQL.Add('strftime(''%m'', l.expire_date) AS mes,');
  aSQL.Add('strftime(''%d'', l.expire_date) AS dia');
  aSQL.Add('FROM legal AS l WHERE (l.active_status = 1)');
end;

procedure LoadPeopleDateTree(aSQL: TStrings);
begin
  aSQL.Add('SELECT ');
  aSQL.Add('strftime(''%Y'', p.birth_date) AS ano,');
  aSQL.Add('strftime(''%m'', p.birth_date) AS mes,');
  aSQL.Add('strftime(''%d'', p.birth_date) AS dia');
  aSQL.Add('FROM people AS p WHERE (p.active_status = 1)');
end;

procedure LoadDateTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);
var
  Dia, Mes, Ano, L, FBL: String;
  // i: Integer;
  Qry: TSQLQuery;
  Q: TStrings;
  FS: TFormatSettings;
  Lang: Integer;
  Data: TBasicNodeData;
  xNode, monthParent, yearParent: PVirtualNode;
begin
  //Lista := TStringList.Create;
  //STree := TMemoryStream.Create;
  //STree.Position := 0;
  Qry := TSQLQuery.Create(DMM.sqlCon);
  Q := Qry.SQL;
  try
    Qry.DataBase := DMM.sqlCon;
    Q.Clear;

    case aTable of
      tbNone:
        begin
          LoadSpecimenDateTree(Q);
          Q.Add('UNION');
          LoadNestDateTree(Q);
          Q.Add('UNION');
          LoadNestRevisionDateTree(Q);
          Q.Add('UNION');
          LoadEggDateTree(Q);
          Q.Add('UNION');
          LoadSightingDateTree(Q);
          Q.Add('UNION');
          LoadCaptureDateTree(Q);
          Q.Add('UNION');
          LoadMoltDateTree(Q);
          Q.Add('UNION');
          LoadExpeditionDateTree(Q);
          Q.Add('UNION');
          LoadSurveyDateTree(Q);
          Q.Add('UNION');
          LoadIndividualDateTree(Q);
          Q.Add('UNION');
          LoadProjectDateTree(Q);
          Q.Add('UNION');
          LoadPermitDateTree(Q);
          Q.Add('UNION');
          LoadPeopleDateTree(Q);
        end;
      tbRecordHistory: ;
      tbPermits:       LoadPermitDateTree(Q);
      tbNests:         LoadNestDateTree(Q);
      tbNestRevisions: LoadNestRevisionDateTree(Q);
      tbEggs:          LoadEggDateTree(Q);
      tbProjects:      LoadProjectDateTree(Q);
      tbInstitutions: ;
      tbPeople:        LoadPeopleDateTree(Q);
      tbExpeditions:   LoadExpeditionDateTree(Q);
      tbSurveys:       LoadSurveyDateTree(Q);
      tbNetsEffort: ;
      tbSightings:     LoadSightingDateTree(Q);
      tbSpecimens:     LoadSpecimenDateTree(Q);
      tbSamplePreps: ;
      tbBands: ;
      tbIndividuals:   LoadIndividualDateTree(Q);
      tbCaptures:      LoadCaptureDateTree(Q);
      tbMolts:         LoadMoltDateTree(Q);
      tbFeathers:      LoadFeatherDateTree(Q);
      tbImages: ;
      tbAudioLibrary: ;
    end;

    Q.Add('GROUP BY ano, mes, dia');
    Q.Add('ORDER BY ano DESC, mes ASC, dia ASC');
    //{$IFDEF DEBUG}
    //LogDebug(Qry.SQL.Text);
    //{$ENDIF}
    Qry.Open;

    if Qry.RecordCount > 0 then
    try
      // PBar.Max:= RecordCount;
      // PBar.Position:= 0;
      // PBar.Visible:= True;

      aVirtualTree.BeginUpdate;
      if aVirtualTree.TotalCount > 0 then
        aVirtualTree.Clear;
      Ano := EmptyStr;
      Mes := EmptyStr;
      Dia := EmptyStr;
      Data := nil;
      XNode := nil;
      yearParent := nil;
      monthParent := nil;

      //FS := TFormatSettings.Create;
      L := EmptyStr;
      FBL := EmptyStr;
      GetLanguageIDs(L, FBL);
      if Lowercase(L) = 'en' then
        Lang := $0409   // English (USA)
      else
        Lang := $0416;  // Portuguese (Brazil)
      {$IFDEF MSWINDOWS}
      SysUtils.GetLocaleFormatSettings(Lang, FS);
      {$ENDIF}
      {$IFDEF DARWIN}
      GetNSFormatSettings(FS, L);
      {$ENDIF}
      {$IFDEF LINUX}
      FS := FormatSettings;
      {$ENDIF}

      // Create list for the treelist
      Qry.First;
      repeat
        if (Qry.FieldByName('ano').AsString <> Ano) and
          (Qry.FieldByName('ano').AsString <> EmptyStr) and
          (Qry.FieldByName('ano').AsInteger > 0) then
        begin
          Ano := Qry.FieldByName('ano').AsString;
          Mes := EmptyStr;
          Data := TBasicNodeData.Create;
          Data.Caption := Ano;
          Data.Id := 0;
          Data.ImageIndex := FirstIconIndex;
          Data.Checked := False;
          xNode := aVirtualTree.AddChild(nil, Data);
          aVirtualTree.CheckType[xNode] := ctTriStateCheckBox;
          yearParent := xNode;
        end;
        if (Qry.FieldByName('mes').AsString <> Mes) and
          (Qry.FieldByName('mes').AsString <> EmptyStr) and
          (Qry.FieldByName('mes').AsInteger > 0) then
        begin
          Mes := Qry.FieldByName('mes').AsString;
          //Lista.Add(#9 + FS.LongMonthNames[Qry.FieldByName('mes').AsInteger]);
          Dia := EmptyStr;
          Data := TBasicNodeData.Create;
          Data.Caption := FS.LongMonthNames[StrToInt(Mes)];
          Data.Id := 0;
          Data.ImageIndex := FirstIconIndex;
          if FirstIconIndex > -1 then
            Data.ImageIndex := Data.ImageIndex + 1;
          Data.Checked := False;
          xNode := aVirtualTree.AddChild(yearParent, Data);
          aVirtualTree.CheckType[xNode] := ctTriStateCheckBox;
          monthParent := xNode;
        end;
        if (Qry.FieldByName('dia').AsString <> Dia) and
          (Qry.FieldByName('dia').AsString <> EmptyStr) and
          (Qry.FieldByName('dia').AsInteger > 0) then
        begin
          Dia := Qry.FieldByName('dia').AsString;
          Data := TBasicNodeData.Create;
          Data.Caption := Dia;
          Data.Id := 0;
          Data.ImageIndex := FirstIconIndex;
          if FirstIconIndex > -1 then
            Data.ImageIndex := Data.ImageIndex + 2;
          Data.Checked := False;
          xNode := aVirtualTree.AddChild(monthParent, Data);
        end;

        // PBar.Position:= RecNo;
        Qry.Next;
      until Qry.EOF;
      //Lista.SaveToStream(STree);
      // STree.SaveToFile('TaxonTreeStream.txt');
      //STree.Position := 0;
      //try
        //aVirtualTree.LoadFromStream(STree);
      //except
      //{$IFDEF DEBUG}
      //  Lista.SaveToFile(ConcatPaths([InstallDir, 'date_tree.txt']));
      //{$ENDIF}
      //  raise;
      //end;
    finally
      aVirtualTree.EndUpdate;

      // PBar.Visible:= False;
      // PBar.Position:= 0;
    end;
    Qry.Close;
  finally
    FreeAndNil(Qry);
    //Lista.Free;
    //STree.Free;
  end;
  aVirtualTree.FullCollapse;
  // for i := 0 to aVirtualTree.Items.Count-1 do
  // begin
  // aVirtualTree.Items.Item[i].ImageIndex:= aVirtualTree.Items.Item[i].Level + FirstIconIndex;
  // aVirtualTree.Items.Item[i].SelectedIndex:= aVirtualTree.Items.Item[i].Level + FirstIconIndex;
  // end;
  aVirtualTree.ClearSelection;
  // SBarTaxon.Caption:= 'Táxons com registros: '+IntToStr(TV.Items.Count);
end;

procedure LoadSiteTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);
var
  Mun, Est, Pais: String;
  Qry: TSQLQuery;
  //Lista: TStrings;
  //STree: TMemoryStream;
  Data: TSiteNodeData;
  xNode, stateParent, countryParent: PVirtualNode;
begin
  //Lista := TStringList.Create;
  //STree := TMemoryStream.Create;
  //STree.Position := 0;
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;

    Add('WITH SiteDetails AS (');
    Add('SELECT');
    Add('    site_id,');
    Add('    site_name,');
    Add('    municipality_id,');
    Add('    state_id,');
    Add('    country_id');
    Add('FROM gazetteer');
    Add(')');
    case aTable of
      tbNone:
        begin
          Add('SELECT e.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM specimens AS e');
          Add('JOIN SiteDetails AS g ON e.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (e.active_status = 1)');
          Add('UNION');
          Add('SELECT n.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM nests AS n');
          Add('JOIN SiteDetails AS g ON n.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (n.active_status = 1)');
          Add('UNION');
          Add('SELECT ac.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM sightings AS ac');
          Add('JOIN SiteDetails AS g ON ac.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (ac.active_status = 1)');
          Add('UNION');
          Add('SELECT c.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM captures AS c');
          Add('JOIN SiteDetails AS g ON c.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (c.active_status = 1)');
          Add('UNION');
          Add('SELECT xp.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM expeditions AS xp');
          Add('JOIN SiteDetails AS g ON xp.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (xp.active_status = 1)');
          Add('UNION');
          Add('SELECT a.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM surveys AS a');
          Add('JOIN SiteDetails AS g ON a.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (a.active_status = 1)');
          Add('UNION');
          Add('SELECT i.municipality_id, i.state_id, i.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM institutions AS i');
          Add('JOIN SiteDetails AS m ON i.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON i.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON i.country_id = p.site_id');
          Add('WHERE (i.active_status = 1)');
          Add('UNION');
          Add('SELECT r.municipality_id, r.state_id, r.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM people AS r');
          Add('JOIN SiteDetails AS m ON r.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON r.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON r.country_id = p.site_id');
          Add('WHERE (r.active_status = 1)');
        end;
      tbNests:
        begin
          Add('SELECT n.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM nests AS n');
          Add('JOIN SiteDetails AS g ON n.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (n.active_status = 1)');
        end;
      tbInstitutions:
        begin
          Add('SELECT i.municipality_id, i.state_id, i.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM institutions AS i');
          Add('JOIN SiteDetails AS m ON i.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON i.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON i.country_id = p.site_id');
          Add('WHERE (i.active_status = 1)');
        end;
      tbPeople:
        begin
          Add('SELECT r.municipality_id, r.state_id, r.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM people AS r');
          Add('JOIN SiteDetails AS m ON r.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON r.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON r.country_id = p.site_id');
          Add('WHERE (r.active_status = 1)');
        end;
      tbExpeditions:
        begin
          Add('SELECT xp.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM expeditions AS xp');
          Add('JOIN SiteDetails AS g ON xp.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (xp.active_status = 1)');
        end;
      tbSurveys:
        begin
          Add('SELECT a.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM surveys AS a');
          Add('JOIN SiteDetails AS g ON a.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (a.active_status = 1)');
        end;
      tbSightings:
        begin
          Add('SELECT ac.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM sightings AS ac');
          Add('JOIN SiteDetails AS g ON ac.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (ac.active_status = 1)');
        end;
      tbSpecimens:
        begin
          Add('SELECT e.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM specimens AS e');
          Add('JOIN SiteDetails AS g ON e.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (e.active_status = 1)');
        end;
      tbCaptures:
        begin
          Add('SELECT c.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM captures AS c');
          Add('JOIN SiteDetails AS g ON c.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (c.active_status = 1)');
        end;
      tbFeathers:
        begin
          Add('SELECT ft.locality_id, g.municipality_id, g.state_id, g.country_id,');
          Add('   m.site_name AS municipality_name,');
          Add('   s.site_name AS state_name,');
          Add('   p.site_name AS country_name');
          Add('FROM feathers AS ft');
          Add('JOIN SiteDetails AS g ON ft.locality_id = g.site_id');
          Add('JOIN SiteDetails AS m ON g.municipality_id = m.site_id');
          Add('JOIN SiteDetails AS s ON g.state_id = s.site_id');
          Add('JOIN SiteDetails AS p ON g.country_id = p.site_id');
          Add('WHERE (ft.active_status = 1)');
        end;
      //tbImages: ;
      //tbAudioLibrary: ;
    else
      Exit;
    end;

    if aTable = tbInstitutions then
      SQL.Add('GROUP BY i.country_id, i.state_id, i.municipality_id')
    else
    if aTable = tbPeople then
      SQL.Add('GROUP BY r.country_id, r.state_id, r.municipality_id')
    else
      SQL.Add('GROUP BY g.country_id, g.state_id, g.municipality_id');
    SQL.Add('ORDER BY country_name ASC, state_name ASC, municipality_name ASC');
    //{$IFDEF DEBUG}
    //LogDebug(SQL.Text);
    //{$ENDIF}
    Open;
    if RecordCount > 0 then
    try
      // PBar.Max:= RecordCount;
      // PBar.Position:= 0;
      // PBar.Visible:= True;

      aVirtualTree.BeginUpdate;
      if aVirtualTree.TotalCount > 0 then
        aVirtualTree.Clear;
      Pais := EmptyStr;
      Est := EmptyStr;
      Mun := EmptyStr;
      Data := nil;
      XNode := nil;
      countryParent := nil;
      stateParent := nil;

      // Create list for the treelist
      Qry.First;
      repeat
        if (Qry.FieldByName('country_name').AsString <> Pais) and
          (Qry.FieldByName('country_name').AsString <> EmptyStr) then
        begin
          Pais := Qry.FieldByName('country_name').AsString;
          Data := TSiteNodeData.Create;
          Data.Caption := Pais;
          Data.Id := Qry.FieldByName('country_id').AsInteger;
          Data.ImageIndex := FirstIconIndex;
          Data.Checked := False;
          Data.Rank := srCountry;
          xNode := aVirtualTree.AddChild(nil, Data);
          countryParent := xNode;
        end;
        if (Qry.FieldByName('state_name').AsString <> Est) and
          (Qry.FieldByName('state_name').AsString <> EmptyStr) then
        begin
          Est := Qry.FieldByName('state_name').AsString;
          Data := TSiteNodeData.Create;
          Data.Caption := Est;
          Data.Id := Qry.FieldByName('state_id').AsInteger;
          Data.ImageIndex := FirstIconIndex;
          if FirstIconIndex > -1 then
            Data.ImageIndex := Data.ImageIndex + 1;
          Data.Checked := False;
          Data.Rank := srState;
          xNode := aVirtualTree.AddChild(countryParent, Data);
          stateParent := xNode;
        end;
        if (Qry.FieldByName('municipality_name').AsString <> Mun) and
          (Qry.FieldByName('municipality_name').AsString <> EmptyStr) then
        begin
          Mun := Qry.FieldByName('municipality_name').AsString;
          Data := TSiteNodeData.Create;
          Data.Caption := Mun;
          Data.Id := Qry.FieldByName('municipality_id').AsInteger;
          Data.ImageIndex := FirstIconIndex;
          if FirstIconIndex > -1 then
            Data.ImageIndex := Data.ImageIndex + 2;
          Data.Checked := False;
          Data.Rank := srMunicipality;
          xNode := aVirtualTree.AddChild(stateParent, Data);
        end;

        // PBar.Position:= RecNo;
        Qry.Next;
      until Qry.EOF;
      // Lista.SaveToFile('TaxonTree.txt');
      //Lista.SaveToStream(STree);
      // STree.SaveToFile('TaxonTreeStream.txt');
      //STree.Position := 0;
      //aVirtualTree.LoadFromStream(STree);
    finally
      aVirtualTree.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
    //Lista.Free;
    //STree.Free;
  end;
  aVirtualTree.FullCollapse;
  //for i := 0 to aVirtualTree.TotalCount - 1 do
  //begin
  //  aVirtualTree..Items.Item[i].ImageIndex := aVirtualTree.Items.Item[i].Level + FirstIconIndex;
  //  aVirtualTree.Items.Item[i].SelectedIndex := aVirtualTree.Items.Item[i].Level + FirstIconIndex;
  //end;
  aVirtualTree.ClearSelection;
  // SBarTaxon.Caption:= 'Táxons com registros: '+IntToStr(TV.Items.Count);
end;

{ ------------------------------------------------------------------------------------------ }
{ Filter auxiliary functions }
{ ------------------------------------------------------------------------------------------ }

procedure FilterSurveyDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '(strftime(''%Y'', a.survey_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ')';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', sv.survey_date)', 'Survey date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '((strftime(''%Y'', a.survey_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.survey_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', sv.survey_date)', 'Survey date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '((strftime(''%Y'', a.survey_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.survey_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', a.survey_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', sv.survey_date)', 'Survey date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterExpeditionDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '((strftime(''%Y'', start_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', end_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + '))';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', x.start_date)', 'Start date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', x.end_date)', 'End date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '(((strftime(''%Y'', start_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', start_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', end_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', end_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', x.start_date)', 'Start date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', x.end_date)', 'End date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '(((strftime(''%Y'', start_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', start_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', start_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', end_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', end_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', end_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', x.start_date)', 'Start date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', x.end_date)', 'End date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterSpecimenDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '(strftime(''%Y'', s.collection_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ')';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', sp.collection_date)', 'Collection date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '((strftime(''%Y'', s.collection_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', s.collection_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', sp.collection_date)', 'Collection date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '((strftime(''%Y'', s.collection_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', s.collection_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', s.collection_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', sp.collection_date)', 'Collection date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterNestDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '((strftime(''%Y'', n.found_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', n.last_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + '))';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', n.found_date)', 'Found date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', n.last_date)', 'Last date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '(((strftime(''%Y'', n.found_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', n.found_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', n.last_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', n.last_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', n.found_date)', 'Found date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', n.last_date)', 'Last date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '(((strftime(''%Y'', n.found_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', n.found_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', n.found_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', n.last_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', n.last_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', n.last_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', n.found_date)', 'Found date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', n.last_date)', 'Last date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterSightingDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '(strftime(''%Y'', c.sighting_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ')';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', s.sighting_date)', 'Sighting date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '((strftime(''%Y'', c.sighting_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', c.sighting_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', s.sighting_date)', 'Sighting date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '((strftime(''%Y'', c.sighting_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', c.sighting_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', c.sighting_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', s.sighting_date)', 'Sighting date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterCaptureDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '(strftime(''%Y'', m.capture_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ')';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', c.capture_date)', 'Capture date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '((strftime(''%Y'', m.capture_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', m.capture_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', c.capture_date)', 'Capture date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '((strftime(''%Y'', m.capture_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', m.capture_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', m.capture_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', c.capture_date)', 'Capture date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterMoltDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '(strftime(''%Y'', m.capture_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ')';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', m.sample_date)', 'Date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '((strftime(''%Y'', m.capture_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', m.capture_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', m.sample_date)', 'Date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '((strftime(''%Y'', m.capture_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', m.capture_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', m.capture_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + '))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', m.sample_date)', 'Date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterBandDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '((strftime(''%Y'', a.order_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', a.receipt_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', a.use_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', a.discharge_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', a.report_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + '))';
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '(((strftime(''%Y'', a.order_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.order_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', a.receipt_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.receipt_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', a.use_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.use_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', a.discharge_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.discharge_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', a.report_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.report_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')))';
    end
    else
    { Dia > 0 }
    begin
      //Result := '(((strftime(''%Y'', a.order_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.order_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', a.order_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', a.receipt_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.receipt_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', a.receipt_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', a.use_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.use_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', a.use_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', a.discharge_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.discharge_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', a.discharge_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', a.report_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', a.report_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', a.report_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')))';
    end;
  end;
end;

procedure FilterIndividualDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin

  if aMonth <= 0 then
  begin
    //Result := '((i.birth_year = ' + Format('%4.4d', [aYear]) + ') OR ' +
    //  '(strftime(''%Y'', i.banding_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', i.band_change_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(i.death_year = ' + Format('%4.4d', [aYear]) + '))';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('(i.birth_year)', 'Birth date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', i.banding_date)', 'Banding date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', i.band_change_date)', 'Band change date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
    aSearchGroup[sf].Fields.Add(TSearchField.Create('(i.death_year)', 'Death date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '(((i.birth_year = ' + Format('%4.4d', [aYear]) + ') AND ' +
      //  '(i.birth_month = ' + Format('%2.2d', [aMonth]) + ')) OR ' +
      //  '((strftime(''%Y'', i.banding_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', i.banding_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', i.band_change_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', i.band_change_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((i.death_year = ' + Format('%4.4d', [aYear]) + ') AND ' +
      //  '(i.death_month = ' + Format('%2.2d', [aMonth]) + ')))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month)', 'Birth date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', i.banding_date)', 'Banding date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', i.band_change_date)', 'Band change date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month)', 'Death date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '(((i.birth_year = ' + Format('%4.4d', [aYear]) + ') AND ' +
      //  '(i.birth_month = ' + Format('%2.2d', [aMonth]) + ') AND ' +
      //  '(i.birth_day = ' + Format('%2.2d', [aDay]) + ')) OR ' +
      //  '((strftime(''%Y'', i.banding_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', i.banding_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', i.banding_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', i.band_change_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', i.band_change_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', i.band_change_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((i.death_year = ' + Format('%4.4d', [aYear]) + ') AND ' +
      //  '(i.death_month = ' + Format('%2.2d', [aMonth]) + ') AND ' +
      //  '(i.death_day = ' + Format('%2.2d', [aDay]) + ')))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month||''-''||i.birth_day)', 'Birth date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', i.banding_date)', 'Banding date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', i.band_change_date)', 'Band change date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month||''-''||i.birth_day)', 'Death date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterProjectDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '((strftime(''%Y'', start_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') OR ' +
    //  '(strftime(''%Y'', end_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + '))';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', start_date)', 'Start date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', end_date)', 'End date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '(((strftime(''%Y'', start_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', start_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')) OR ' +
      //  '((strftime(''%Y'', end_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', end_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ')))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', start_date)', 'Start date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', end_date)', 'End date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '(((strftime(''%Y'', start_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', start_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', start_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')) OR ' +
      //  '((strftime(''%Y'', end_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ') AND ' +
      //  '(strftime(''%m'', end_date) = ' + QuotedStr(Format('%2.2d', [aMonth])) + ') AND ' +
      //  '(strftime(''%d'', end_date) = ' + QuotedStr(Format('%2.2d', [aDay])) + ')))';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', start_date)', 'Start date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', end_date)', 'End date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterPeopleDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
var
  sf: Integer;
begin
  if aMonth <= 0 then
  begin
    //Result := '(strftime(''%Y'', p.birth_date) = ' + QuotedStr(Format('%4.4d', [aYear])) + ')';
    sf := aSearchGroup.Add(TSearchGroup.Create);
    aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y'', p.birth_date)', 'Birth date', sdtText,
      crEqual, True, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      //Result := '(strftime(''%Y-%m'', p.birth_date) = ' + QuotedStr(Format('%4.4d-%2.2d', [aYear, aMonth])) + ')';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m'', p.birth_date)', 'Birth date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Dia > 0 }
    begin
      //Result := '(strftime(''%Y-%m-%d'', p.birth_date) = ' + QuotedStr(Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])) + ')';
      sf := aSearchGroup.Add(TSearchGroup.Create);
      aSearchGroup[sf].Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', p.birth_date)', 'Birth date', sdtText,
        crEqual, True, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

function Filtrar(aTabela: TTableType; aDataset: TSQLQuery; aWhere: TStrings; aFilterShow: TStrings = nil): Boolean;
//var
//  i: Integer;
//  isWhere: Boolean;
begin
  //GravaLog('ABRE FILTRO', aTabela.TableName);
  //F_Filter := TF_Filter.Create(nil);
  //with F_Filter do
  //  try
  //    Tabela := aTabela;
  //    Ordenar := False;
  //    if ShowModal = mrOK then
  //    begin
  //      aDataset.Close;
  //      aDataset.SQL.Clear;
  //      aDataset.SQL.Assign(Filtro);
  //      GravaLogSQL(aDataset.SQL);
  //      isWhere := False;
  //      aWhere.Clear;
  //      i := 0;
  //      repeat
  //        if TRegEx.IsMatch(Filtro[i], '^where [A-Z0-9.''_=%,;<> ]+$', [roIgnoreCase]) then
  //          isWhere := True;
  //        if TRegEx.IsMatch(Filtro[i], '^order by [A-Z0-9.''_=%,; ]+$', [roIgnoreCase]) then
  //          isWhere := False;
  //        if isWhere then
  //          aWhere.Add(Filtro[i]);
  //        Inc(i);
  //        Application.ProcessMessages;
  //      until i = Filtro.Count;
  //      if Assigned(aFilterShow) then
  //        aFilterShow.Assign(FiltroShow);
  //      Result := True;
  //    end
  //    else
  //      Result := False;
  //  finally
  //    FreeAndNil(F_Filter);
  //    GravaLog('FECHA FILTRO', '');
  //  end;
end;

function TaxonFilterToString(aVirtualTree: TBaseVirtualTree; aPrefix: String): String;
var
  Data: PTaxonNodeData;
  Node: PVirtualNode;
  i: Integer;
begin
  Result := EmptyStr;
  i := 0;

  if aVirtualTree.CheckedCount > 0 then
  begin
    Node := aVirtualTree.GetFirstChecked;
    while Assigned(Node) do
    begin
      Inc(i);

      Data := aVirtualTree.GetNodeData(Node);
      case Data^.Rank of
        trOrder:   Result := Result + '(' + aPrefix + 'order_id = ' + IntToStr(Data^.Id) + ')';
        trFamily:  Result := Result + '(' + aPrefix + 'family_id = ' + IntToStr(Data^.Id) + ')';
        trSpecies: Result := Result + '(' + aPrefix + 'species_id = ' + IntToStr(Data^.Id) + ')';
      end;
      if i < (aVirtualTree.CheckedCount) then
        Result := Result + ' OR ';

      Node := aVirtualTree.GetNextChecked(Node);
    end;
    if aVirtualTree.CheckedCount > 1 then
      Result := '(' + Result + ')';
  end;
end;

function TaxonFilterToSearch(aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
var
  Data: PTaxonNodeData;
  Node: PVirtualNode;
  i, sf: Integer;
begin
  Result := 0;
  i := 0;

  if aVirtualTree.CheckedCount > 0 then
  begin
    sf := aSearchGroup.Add(TSearchGroup.Create);

    Node := aVirtualTree.GetFirstChecked;
    while Assigned(Node) do
    begin
      Inc(i);

      Data := aVirtualTree.GetNodeData(Node);
      case Data^.Rank of
        trOrder:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + 'order_id', 'Order', sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        trFamily:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + 'family_id', 'Family', sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        trSpecies:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + 'species_id', 'Species', sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
      end;

      Node := aVirtualTree.GetNextChecked(Node);
    end;

  end;
  Result := aVirtualTree.CheckedCount;
end;

function SiteFilterToString(aVirtualTree: TBaseVirtualTree; aPrefix: String): String;
var
  Data: PSiteNodeData;
  Node: PVirtualNode;
  i: Integer;
begin
  Result := EmptyStr;
  i := 0;

  if aVirtualTree.CheckedCount > 0 then
  begin
    Node := aVirtualTree.GetFirstChecked;
    while Assigned(Node) do
    begin
      Inc(i);

      Data := aVirtualTree.GetNodeData(Node);
      case Data^.Rank of
        srCountry:      Result := Result + '(' + aPrefix + 'country_id = ' + IntToStr(Data^.Id) + ')';
        srState:        Result := Result + '(' + aPrefix + 'state_id = ' + IntToStr(Data^.Id) + ')';
        srMunicipality: Result := Result + '(' + aPrefix + 'municipality_id = ' + IntToStr(Data^.Id) + ')';
      end;
      if i < (aVirtualTree.CheckedCount) then
        Result := Result + ' OR ';

      Node := aVirtualTree.GetNextChecked(Node);
    end;
    if aVirtualTree.CheckedCount > 1 then
      Result := '(' + Result + ')';
  end;
end;

function SiteFilterToSearch(aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
var
  Data: PSiteNodeData;
  Node: PVirtualNode;
  i, sf: Integer;
begin
  Result := 0;
  i := 0;

  if aVirtualTree.CheckedCount > 0 then
  begin
    sf := aSearchGroup.Add(TSearchGroup.Create);

    Node := aVirtualTree.GetFirstChecked;
    while Assigned(Node) do
    begin
      Inc(i);

      Data := aVirtualTree.GetNodeData(Node);
      case Data^.Rank of
        srCountry:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + 'country_id', 'Country', sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        srState:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + 'state_id', 'State', sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        srMunicipality:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + 'municipality_id', 'Municipality', sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
      end;

      Node := aVirtualTree.GetNextChecked(Node);
    end;

  end;
  Result := aVirtualTree.CheckedCount;
end;

function DateFilterToString(aTable: TTableType; aVirtualTree: TBaseVirtualTree; var aTotal: Integer): String;
var
  Ano, Mes, Dia: Integer;
  MesAllChecked, MesAllUnchecked, DiaAllChecked, DiaAllUnchecked: Boolean;
  idx, i, im, Id: Integer;
  //FS: TFormatSettings;
  chList: TStrings;
  //Data: PBasicNodeRec;
  //aNode: PVirtualNode;

  //function FilterDates(aYear, aMonth, aDay: Integer): String;
  //begin
  //  Result := EmptyStr;
  //  if aYear <= 0 then
  //    Exit;
  //
  //  case aTable of
  //    tbNone: ;
  //    tbRecordHistory: ;
  //    tbPermits: ;
  //    tbNests:
  //      Result := FilterNestDates(aYear, aMonth, aDay);
  //    tbNestRevisions: ;
  //    tbEggs: ;
  //    tbProjects:
  //      Result := FilterProjectDates(aYear, aMonth, aDay);
  //    tbInstitutions: ;
  //    tbPeople:
  //      Result := FilterPeopleDates(aYear, aMonth, aDay);
  //    tbExpeditions:
  //      Result := FilterExpeditionDates(aYear, aMonth, aDay);
  //    tbSurveys:
  //      Result := FilterSurveyDates(aYear, aMonth, aDay);
  //    tbNetsEffort: ;
  //    tbSightings:
  //      Result := FilterSightingDates(aYear, aMonth, aDay);
  //    tbSpecimens:
  //      Result := FilterSpecimenDates(aYear, aMonth, aDay);
  //    tbSamplePreps: ;
  //    tbBands:
  //      Result := FilterBandDates(aYear, aMonth, aDay);
  //    tbIndividuals:
  //      Result := FilterIndividualDates(aYear, aMonth, aDay);
  //    tbCaptures:
  //      Result := FilterCaptureDates(aYear, aMonth, aDay);
  //    tbMolts: ;
  //    tbImages: ;
  //    tbAudioLibrary: ;
  //  end;
  //end;

begin
  Result := EmptyStr;

  chList := TStringList.Create;
  Ano := 0;
  Mes := 0;
  Dia := 0;
  MesAllChecked := True;
  MesAllUnchecked := True;
  DiaAllChecked := True;
  DiaAllUnchecked := True;
  //FS := TFormatSettings.Create;
  //aTree.NodeDataSize := SizeOf(TMyRec);

  //for idx := 0 to aTree.CheckedCount - 1 do
  //begin
  //  aNode := aTree.GetFirstChecked();
  //  if .Level = 0 then
  //  begin
  //    if aTree.Checked[aTree.Items[idx]] = True then // ano marcado
  //    begin
  //      Inc(aTotal);
  //      Ano := StrToInt(aTree.Items[idx].Text);
  //      Mes := 0;
  //      Dia := 0;
  //      if aTree.Items[idx].HasChildren then // ano possui meses?
  //      begin
  //        MesAllChecked := True;
  //        MesAllUnchecked := True;
  //        for im := 0 to aTree.Items[idx].Count - 1 do
  //        begin
  //          if aTree.Checked[aTree.Items[idx].Item[im]] = True then
  //          // mês marcado
  //          begin
  //            Inc(aTotal);
  //            MesAllUnchecked := False;
  //            // Ano:= StrToInt(aTree.Items[idx].Text);
  //            Mes := IndexText(aTree.Items[idx].Item[im].Text, FS.LongMonthNames) + 1;
  //            Dia := 0;
  //            if aTree.Items[idx].Item[im].HasChildren then // mês possui dias?
  //            begin
  //              DiaAllChecked := True;
  //              DiaAllUnchecked := True;
  //              for Id := 0 to aTree.Items[idx].Item[im].Count - 1 do
  //              begin
  //                if aTree.Checked[aTree.Items[idx].Item[im].Item[Id]] = True then // dia marcado
  //                begin
  //                  Inc(aTotal);
  //                  DiaAllUnchecked := False;
  //                  // Ano:= StrToInt(aTree.Items[idx].Text);
  //                  // Mes:= IndexText(aTree.Items[idx].Item[im].Text,FS.LongMonthNames)+1;
  //                  Dia := StrToInt(aTree.Items[idx].Item[im].Item[Id].Text);
  //                  chList.Add(DateFilterToString(Ano, Mes, Dia));
  //                end
  //                else
  //                begin
  //                  DiaAllChecked := False;
  //                end;
  //              end;
  //            end
  //            else
  //            begin
  //              chList.Add(DateFilterToString(Ano, Mes, Dia));
  //            end;
  //            if (DiaAllChecked = True) or (DiaAllUnchecked = True) then
  //            begin
  //              Dia := 0;
  //              chList.Add(DateFilterToString(Ano, Mes, Dia));
  //            end;
  //          end
  //          else
  //          begin
  //            MesAllChecked := False;
  //          end;
  //        end;
  //        if (MesAllChecked = True) or (MesAllUnchecked = True) then
  //        begin
  //          Mes := 0;
  //          Dia := 0;
  //          chList.Add(DateFilterToString(Ano, Mes, Dia));
  //        end;
  //      end
  //      else
  //      begin
  //        chList.Add(DateFilterToString(Ano, Mes, Dia));
  //      end;
  //    end;
  //  end;
  //end;
  //
  //Result := chList.Count > 0;
  //
  //aFilterLine := '';
  //if chList.Count > 0 then
  //begin
  //  for i := 0 to chList.Count - 1 do
  //  begin
  //    if i > 0 then
  //      aFilterLine := aFilterLine + ' or ' + chList[i]
  //    else
  //      aFilterLine := chList[i];
  //  end;
  //  aFilterLine := '(' + aFilterLine + ')';
  //end;
  //
  //FreeAndNil(chList);
end;

function DateFilterToSearch(aTable: TTableType; aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
var
  Data, DataM, DataD: PDateNodeData;
  Node, NodeM, NodeD: PVirtualNode;
  sf, aTotal: Integer;
  Ano, Mes, Dia: Integer;
  MesAllChecked, MesAllUnchecked, DiaAllChecked, DiaAllUnchecked: Boolean;
  //idx, im, Id: Integer;
  L, FBL: String;
  FS: TFormatSettings;
  Lang: Integer;
  //chList: TStrings;
  //Data: PBasicNodeRec;
  //aNode: PVirtualNode;

  procedure FilterDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroups);
  begin
    if aYear <= 0 then
      Exit;

    case aTable of
      tbNone: ;
      tbRecordHistory: ;
      tbPermits: ;
      tbNests:         FilterNestDates(aYear, aMonth, aDay, aSearchGroup);
      tbNestRevisions: ;
      tbEggs: ;
      tbProjects:      FilterProjectDates(aYear, aMonth, aDay, aSearchGroup);
      tbInstitutions: ;
      tbPeople:        FilterPeopleDates(aYear, aMonth, aDay, aSearchGroup);
      tbExpeditions:   FilterExpeditionDates(aYear, aMonth, aDay, aSearchGroup);
      tbSurveys:       FilterSurveyDates(aYear, aMonth, aDay, aSearchGroup);
      tbNetsEffort: ;
      tbSightings:     FilterSightingDates(aYear, aMonth, aDay, aSearchGroup);
      tbSpecimens:     FilterSpecimenDates(aYear, aMonth, aDay, aSearchGroup);
      tbSamplePreps: ;
      //tbBands:         FilterBandDates(aYear, aMonth, aDay, aSearchGroup);
      tbIndividuals:   FilterIndividualDates(aYear, aMonth, aDay, aSearchGroup);
      tbCaptures:      FilterCaptureDates(aYear, aMonth, aDay, aSearchGroup);
      tbMolts: ;
      tbImages: ;
      tbAudioLibrary: ;
    end;
  end;

begin
  Result := 0;
  aTotal := 0;

  if aVirtualTree.CheckedCount > 0 then
  begin
    L := EmptyStr;
    FBL := EmptyStr;
    GetLanguageIDs(L, FBL);
    if Lowercase(L) = 'en' then
      Lang := $0409   // English (USA)
    else
      Lang := $0416;  // Portuguese (Brazil)
    {$IFDEF MSWINDOWS}
    SysUtils.GetLocaleFormatSettings(Lang, FS);
    {$ENDIF}
    {$IFDEF DARWIN}
    GetNSFormatSettings(FS, L);
    {$ENDIF}
    {$IFDEF LINUX}
    FS := FormatSettings;
    {$ENDIF}

    Node := aVirtualTree.GetFirst;
    while Assigned(Node) do
    begin
      if aVirtualTree.CheckState[Node] in [csCheckedNormal, csMixedNormal] then // ano marcado
      begin
        Inc(aTotal);
        Data := aVirtualTree.GetNodeData(Node);
        Ano := StrToInt(Data^.Caption);
        Mes := 0;
        Dia := 0;
        if aVirtualTree.HasChildren[Node] then // ano possui meses?
        begin
          MesAllChecked := True;
          MesAllUnchecked := True;
          NodeM := aVirtualTree.GetFirstChild(Node);
          while Assigned(NodeM) do
          begin
            if aVirtualTree.CheckState[NodeM] in [csCheckedNormal, csMixedNormal] then  // mês marcado
            begin
              Inc(aTotal);
              DataM := aVirtualTree.GetNodeData(NodeM);
              MesAllUnchecked := False;
              Mes := IndexText(DataM^.Caption, FS.LongMonthNames) + 1;
              Dia := 0;
              if aVirtualTree.HasChildren[NodeM] then // mês possui dias?
              begin
                DiaAllChecked := True;
                DiaAllUnchecked := True;

                NodeD := aVirtualTree.GetFirstChild(NodeM);
                while Assigned(NodeD) do
                begin
                  if aVirtualTree.CheckState[NodeD] = csCheckedNormal then  // dia marcado
                  begin
                    Inc(aTotal);
                    DataD := aVirtualTree.GetNodeData(NodeD);
                    DiaAllUnchecked := False;
                    Dia := StrToInt(DataD^.Caption);
                    FilterDates(Ano, Mes, Dia, aSearchGroup);
                  end
                  else
                  begin
                    DiaAllChecked := False;
                  end;
                  NodeD := aVirtualTree.GetNextSibling(NodeD);
                end;
              end
              else
              begin
                FilterDates(Ano, Mes, Dia, aSearchGroup);
              end;
              if (DiaAllChecked = True) or (DiaAllUnchecked = True) then
              begin
                Dia := 0;
                FilterDates(Ano, Mes, Dia, aSearchGroup);
              end;
            end
            else
            begin
              MesAllChecked := False;
            end;
            NodeM := aVirtualTree.GetNextSibling(NodeM);
          end;
          if (MesAllChecked = True) or (MesAllUnchecked = True) then
          begin
            Mes := 0;
            Dia := 0;
            FilterDates(Ano, Mes, Dia, aSearchGroup);
          end;
        end
        else
        begin
          FilterDates(Ano, Mes, Dia, aSearchGroup);
        end;
      end;

      Node := aVirtualTree.GetNextSibling(Node);
    end;
  end;

  Result := aTotal;
end;

function PersonFilterToSearch(aTable: TTableType; aSearchGroup: TSearchGroups; aKey: Integer): Boolean;
var
  sf: Integer;
begin
  Result := False;
  if aKey <= 0 then
    Exit;

  sf := aSearchGroup.Add(TSearchGroup.Create);

  case aTable of
    tbNone: ;
    tbNests:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('observer_id', 'Observer', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbNestRevisions:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('observer_1_id', 'Observer 1', sdtInteger,
          crEqual, False, IntToStr(aKey)));
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('observer_2_id', 'Observer 2', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbEggs:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('researcher_id', 'Researcher', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbSightings:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('observer_id', 'Observer', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbSpecimens: ;
    tbSamplePreps: ;
    tbBands:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('carrier_id', 'Carrier', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbCaptures:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('bander_id', 'Bander', sdtInteger,
          crEqual, False, IntToStr(aKey)));
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('annotator_id', 'Annotator', sdtInteger,
          crEqual, False, IntToStr(aKey)));
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('photographer_1_id', 'Photographer 1', sdtInteger,
          crEqual, False, IntToStr(aKey)));
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('photographer_2_id', 'Photographer 2', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbFeathers:
      begin
        aSearchGroup.Items[sf].Fields.Add(TSearchField.Create('observer_id', 'Observer', sdtInteger,
          crEqual, False, IntToStr(aKey)));
      end;
    tbImages: ;
    tbAudioLibrary: ;
  end;

  Result := True;
end;

end.
