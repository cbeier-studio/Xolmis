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

unit data_filters;

{$mode objfpc}{$H+}

interface

uses
  { System }
  Classes, SysUtils, LCLIntf, gettext, StrUtils, {$IFDEF DARWIN}iosxlocale,{$ENDIF}
  { VCL }
  Graphics, ImgList, Controls, Dialogs, Forms, laz.VirtualTrees,
  { Data }
  DB, SQLDB,
  { CBS }
  data_types, models_taxonomy, models_geo, models_record_types;

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
  procedure LoadMethodCategories(aList: TStrings);

  procedure LoadTaxaTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);

  procedure LoadSpecimenDateTree(aSQL: TStrings);
  procedure LoadNestDateTree(aSQL: TStrings);
  procedure LoadNestRevisionDateTree(aSQL: TStrings);
  procedure LoadEggDateTree(aSQL: TStrings);
  procedure LoadSightingDateTree(aSQL: TStrings);
  procedure LoadCaptureDateTree(aSQL: TStrings);
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
  procedure FilterSurveyDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterExpeditionDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterSpecimenDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterNestDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterSightingDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterCaptureDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterIndividualDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterProjectDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  procedure FilterPeopleDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);

  { Filter and sort records }
  procedure AddBooleanFilter(aSearch: TCustomSearch; const FieldName, Caption: String; YesChecked, NoChecked: Boolean);
  procedure AddExactTextFilter(aSearch: TCustomSearch; const FieldName, Caption: String; aText: String);
  procedure AddLookupFilter(aSearch: TCustomSearch; const FieldNames, Captions: array of String; LookupId: Integer);
  procedure AddNonZeroIntegerFilter(aSearch: TCustomSearch; const FieldName, Caption: String; YesChecked, NoChecked: Boolean);
  procedure AddTimeFilter(aSearch: TCustomSearch; const FieldName, Caption: String; const TimeStart, TimeEnd: String);
  procedure AddTimeIntervalFilter(aSearch: TCustomSearch; const FieldStart, FieldEnd: String; const TimeStart, TimeEnd: String);

  function TaxonFilterToString(aVirtualTree: TBaseVirtualTree; aPrefix: String = ''): String;
  function TaxonFilterToSearch(aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
  function SiteFilterToString(aVirtualTree: TBaseVirtualTree; aPrefix: String = ''): String;
  function SiteFilterToSearch(aVirtualTree: TBaseVirtualTree; aSearchGroup: TSearchGroups; aPrefix: String = ''): Integer;
  function DateFilterToSearch(aTable: TTableType; aVirtualTree: TBaseVirtualTree; aSearchGroups: TSearchGroups; aPrefix: String = ''): Integer;

implementation

uses utils_global, data_consts, data_columns, udm_main;

procedure LoadMethodCategories(aList: TStrings);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  Qry.Database := DMM.sqlCon;
  with Qry, SQL do
  try
    SQL.Clear;

    Add(xProvider.Methods.DistinctCategories);

    Open;
    if RecordCount > 0 then
    begin
      First;
      // the list must be cleared before calling this method
      while not EOF do
      begin
        aList.Add(FieldByName(COL_CATEGORY).AsString);

        Next;
      end;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure LoadTaxaTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);
var
  aOrder, aFamily, aSpecies: String;
  aOrderId, aFamilyId, aSpeciesId: Integer;
  Qry: TSQLQuery;
  Data: TTaxonNodeData;
  xNode, orderParent, familyParent: PVirtualNode;
begin
  // Set database query for taxon hierarchy
  Qry := TSQLQuery.Create(DMM.sqlCon);
  Qry.Database := DMM.sqlCon;
  with Qry, SQL do
  try
    SQL.Clear;

    Add(xProvider.ZooTaxa.SelectTree(aTable));
    //{$IFDEF DEBUG}
    //LogSQL(SQL);
    //{$ENDIF}
    Open;
    if RecordCount > 0 then
    try
      aVirtualTree.BeginUpdate;
      if aVirtualTree.TotalCount > 0 then
        aVirtualTree.Clear;
      aOrder := EmptyStr;
      aFamily := EmptyStr;
      aSpecies := EmptyStr;
      orderParent := nil;
      familyParent := nil;

      // Add taxa to the tree
      First;
      repeat
        // Order
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
        // Family
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
        // Species
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

        Next;
      until EOF;
    finally
      aVirtualTree.EndUpdate;
      Close;
    end;
  finally
    FreeAndNil(Qry);
  end;
  aVirtualTree.FullCollapse;
  aVirtualTree.ClearSelection;
end;

procedure LoadSpecimenDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Specimens.SelectDateTree(False));
end;

procedure LoadNestDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Nests.SelectDateTree(False));
end;

procedure LoadNestRevisionDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.NestRevisions.SelectDateTree(False));
end;

procedure LoadEggDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Eggs.SelectDateTree(False));
end;

procedure LoadSightingDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Sightings.SelectDateTree(False));
end;

procedure LoadCaptureDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Captures.SelectDateTree(False));
end;

procedure LoadFeatherDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Feathers.SelectDateTree(False));
end;

procedure LoadExpeditionDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Expeditions.SelectDateTree(False));
end;

procedure LoadSurveyDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Surveys.SelectDateTree(False));
end;

procedure LoadIndividualDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Individuals.SelectDateTree(False));
end;

procedure LoadProjectDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Projects.SelectDateTree(False));
end;

procedure LoadPermitDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.Permits.SelectDateTree(False));
end;

procedure LoadPeopleDateTree(aSQL: TStrings);
begin
  aSQL.Add(xProvider.People.SelectDateTree(False));
end;

procedure LoadDateTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);
var
  Dia, Mes, Ano, L, FBL: String;
  Qry: TSQLQuery;
  Q: TStrings;
  FS: TFormatSettings;
  Lang: Integer;
  Data: TBasicNodeData;
  xNode, monthParent, yearParent: PVirtualNode;
begin
  // Set database query for date hierarchy
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

      // Get the date format settings
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
      GetMacFormatSettings(FS);
      if Lang = $0409 then // English (USA)
      begin
        FS.DecimalSeparator := '.';
        FS.ThousandSeparator := ',';
        FS.DateSeparator := '/';
        FS.ShortDateFormat := 'MM/DD/YYYY';
      end
      else
      if Lang = $0416 then // Portuguese (Brazil)
      begin
        FS.DecimalSeparator := ',';
        FS.ThousandSeparator := '.';
        FS.DateSeparator := '/';
        FS.ShortDateFormat := 'DD/MM/YYYY';
      end;
      {$ENDIF}
      {$IFDEF LINUX}
      FS := FormatSettings;
      {$ENDIF}

      // Add dates to the tree
      Qry.First;
      repeat
        // Year
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
        // Month
        if (Qry.FieldByName('mes').AsString <> Mes) and
          (Qry.FieldByName('mes').AsString <> EmptyStr) and
          (Qry.FieldByName('mes').AsInteger > 0) then
        begin
          Mes := Qry.FieldByName('mes').AsString;
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
        // Day
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

        Qry.Next;
      until Qry.EOF;
    finally
      aVirtualTree.EndUpdate;
    end;
    Qry.Close;
  finally
    FreeAndNil(Qry);
  end;
  aVirtualTree.FullCollapse;
  aVirtualTree.ClearSelection;
end;

procedure LoadSiteTreeData(aTable: TTableType; aVirtualTree: TBaseVirtualTree; FirstIconIndex: Integer = -1);
var
  Mun, Est, Pais: String;
  Qry: TSQLQuery;
  Data: TSiteNodeData;
  xNode, stateParent, countryParent: PVirtualNode;
begin
  // Set database query for site hierarchy
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;

    Add(xProvider.Gazetteer.SelectTree(aTable));

    //{$IFDEF DEBUG}
    //LogDebug(SQL.Text);
    //{$ENDIF}
    Open;
    if RecordCount > 0 then
    try
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

      // Add sites for the tree
      Qry.First;
      repeat
        // Country
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
        // State
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
        // Municipality
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

        Qry.Next;
      until Qry.EOF;
    finally
      aVirtualTree.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  aVirtualTree.FullCollapse;
  aVirtualTree.ClearSelection;
end;

{ ------------------------------------------------------------------------------------------ }
{ Filter auxiliary functions }
{ ------------------------------------------------------------------------------------------ }

procedure FilterSurveyDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', sv.survey_date)', 'Survey date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', sv.survey_date)', 'Survey date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', sv.survey_date)', 'Survey date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterExpeditionDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', x.start_date)', 'Start date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', x.end_date)', 'End date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', x.start_date)', 'Start date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', x.end_date)', 'End date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', x.start_date)', 'Start date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', x.end_date)', 'End date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterSpecimenDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', sp.collection_date)', 'Collection date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', sp.collection_date)', 'Collection date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', sp.collection_date)', 'Collection date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterNestDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', n.found_date)', 'Found date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', n.last_date)', 'Last date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', n.found_date)', 'Found date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', n.last_date)', 'Last date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', n.found_date)', 'Found date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', n.last_date)', 'Last date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterSightingDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', s.sighting_date)', 'Sighting date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', s.sighting_date)', 'Sighting date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', s.sighting_date)', 'Sighting date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterCaptureDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', c.capture_date)', 'Capture date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', c.capture_date)', 'Capture date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', c.capture_date)', 'Capture date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterIndividualDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin

  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('(i.birth_year)', 'Birth date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', i.banding_date)', 'Banding date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', i.band_change_date)', 'Band change date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('(i.death_year)', 'Death date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month)', 'Birth date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', i.banding_date)', 'Banding date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', i.band_change_date)', 'Band change date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month)', 'Death date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month||''-''||i.birth_day)', 'Birth date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', i.banding_date)', 'Banding date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', i.band_change_date)', 'Band change date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('(i.birth_year||''-''||i.birth_month||''-''||i.birth_day)', 'Death date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterProjectDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', start_date)', 'Start date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', end_date)', 'End date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', start_date)', 'Start date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', end_date)', 'End date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', start_date)', 'Start date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', end_date)', 'End date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure FilterPeopleDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
begin
  if aMonth <= 0 then
  begin
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', p.birth_date)', 'Birth date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
    aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y'', p.death_date)', 'Death date', sdtText,
      crEqual, False, Format('%4.4d', [aYear])));
  end
  else
  begin
    if aDay <= 0 then
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', p.birth_date)', 'Birth date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m'', p.death_date)', 'Death date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d', [aYear, aMonth])));
    end
    else
    { Day > 0 }
    begin
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', p.birth_date)', 'Birth date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
      aSearchGroup.Fields.Add(TSearchField.Create('strftime(''%Y-%m-%d'', p.death_date)', 'Death date', sdtText,
        crEqual, False, Format('%4.4d-%2.2d-%2.2d', [aYear, aMonth, aDay])));
    end;
  end;
end;

procedure AddBooleanFilter(aSearch: TCustomSearch; const FieldName, Caption: String; YesChecked, NoChecked: Boolean);
var
  sf: Integer;
  Value: String;
begin
  if YesChecked then
    Value := '1'
  else if NoChecked then
    Value := '0'
  else
    Exit; // no filter selected

  sf := aSearch.QuickFilters.Add(TSearchGroup.Create);
  aSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(FieldName, Caption, sdtBoolean, crEqual, True, Value));
end;

procedure AddExactTextFilter(aSearch: TCustomSearch; const FieldName, Caption: String; aText: String);
var
  sf: Integer;
begin
  sf := aSearch.QuickFilters.Add(TSearchGroup.Create);
  aSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(FieldName, Caption, sdtText,
        crEqual, True, aText));
end;

procedure AddLookupFilter(aSearch: TCustomSearch; const FieldNames, Captions: array of String; LookupId: Integer);
var
  sf: Integer;
  i: Integer;
begin
  if LookupId <= 0 then
    Exit;

  if Length(FieldNames) <> Length(Captions) then
    raise Exception.Create('FieldNames and Captions must have the same length.');

  sf := aSearch.QuickFilters.Add(TSearchGroup.Create);

  for i := 0 to High(FieldNames) do
    aSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(FieldNames[i], Captions[i], sdtInteger, crEqual,
        False, IntToStr(LookupId)));
end;

procedure AddNonZeroIntegerFilter(aSearch: TCustomSearch; const FieldName, Caption: String; YesChecked, NoChecked: Boolean);
var
  sf: Integer;
  Value: String;
  Crit: TCriteriaType;
begin
  if YesChecked then
  begin
    Value := '1';
    Crit := crMoreThan;
  end
  else
  if NoChecked then
  begin
    Value := '0';
    Crit := crEqual;
  end
  else
    Exit; // no filter selected

  sf := aSearch.QuickFilters.Add(TSearchGroup.Create);
  aSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(FieldName, Caption, sdtInteger, Crit, True, Value));
  if NoChecked then
  begin
    aSearch.QuickFilters[sf].Fields.Add(TSearchField.Create(FieldName, Caption, sdtInteger, crNull, True));
  end;
end;

procedure AddTimeFilter(aSearch: TCustomSearch; const FieldName, Caption: String; const TimeStart, TimeEnd: String);
var
  sf: Integer;
begin
  if (TimeStart = '') and (TimeEnd = '') then
    Exit;

  sf := aSearch.QuickFilters.Add(TSearchGroup.Create);

  if (TimeStart <> '') and (TimeEnd = '') then
  begin
    // exact time
    aSearch.QuickFilters[sf].Fields.Add(
      TSearchField.Create(FieldName, Caption, sdtTime, crEqual, True, QuotedStr(TimeStart)));
  end
  else
  if (TimeStart <> '') and (TimeEnd <> '') then
  begin
    // interval
    aSearch.QuickFilters[sf].Fields.Add(
      TSearchField.Create(FieldName, Caption, sdtTime, crBetween, True, QuotedStr(TimeStart), QuotedStr(TimeEnd)));
  end;
end;

procedure AddTimeIntervalFilter(aSearch: TCustomSearch; const FieldStart, FieldEnd: String; const TimeStart, TimeEnd: String);
var
  sf: Integer;
begin
  if (TimeStart = '') and (TimeEnd = '') then
    Exit;

  sf := aSearch.QuickFilters.Add(TSearchGroup.Create(aoAnd));

  if (TimeStart <> '') and (TimeEnd <> '') then
  begin
    // time within interval
    // reg.start_time <= filter_end
    aSearch.QuickFilters[sf].Fields.Add(
      TSearchField.Create(FieldStart, rscStartTime, sdtTime, crLessThan, True, QuotedStr(TimeEnd)));
    // reg.end_time >= filter_start
    aSearch.QuickFilters[sf].Fields.Add(
      TSearchField.Create(FieldEnd, rscEndTime, sdtTime, crMoreThan, True, QuotedStr(TimeStart)));
  end
  else
  if (TimeStart <> '') and (TimeEnd = '') then
  begin
    // exact time
    aSearch.QuickFilters[sf].Fields.Add(
      TSearchField.Create(FieldStart, rscStartTime, sdtTime, crEqual, True, QuotedStr(TimeStart)));
    aSearch.QuickFilters[sf].Fields.Add(
      TSearchField.Create(FieldEnd, rscEndTime, sdtTime, crEqual, True, QuotedStr(TimeStart)));
  end
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
        trOrder:   Result := Result + '(' + aPrefix + COL_ORDER_ID + ' = ' + IntToStr(Data^.Id) + ')';
        trFamily:  Result := Result + '(' + aPrefix + COL_FAMILY_ID + ' = ' + IntToStr(Data^.Id) + ')';
        trSpecies: Result := Result + '(' + aPrefix + COL_SPECIES_ID + ' = ' + IntToStr(Data^.Id) + ')';
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
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + COL_ORDER_ID, rscOrderID, sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        trFamily:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + COL_FAMILY_ID, rscFamilyID, sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        trSpecies:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + COL_SPECIES_ID, rscSpeciesID, sdtInteger,
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
        srCountry:      Result := Result + '(' + aPrefix + COL_COUNTRY_ID + ' = ' + IntToStr(Data^.Id) + ')';
        srState:        Result := Result + '(' + aPrefix + COL_STATE_ID + ' = ' + IntToStr(Data^.Id) + ')';
        srMunicipality: Result := Result + '(' + aPrefix + COL_MUNICIPALITY_ID + ' = ' + IntToStr(Data^.Id) + ')';
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
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + COL_COUNTRY_ID, rscCountryID, sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        srState:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + COL_STATE_ID, rscStateID, sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
        srMunicipality:
          aSearchGroup.Items[sf].Fields.Add(TSearchField.Create(aPrefix + COL_MUNICIPALITY_ID, rscMunicipalityID, sdtInteger,
            crEqual, False, IntToStr(Data^.Id)));
      end;

      Node := aVirtualTree.GetNextChecked(Node);
    end;

  end;
  Result := aVirtualTree.CheckedCount;
end;

function DateFilterToSearch(aTable: TTableType; aVirtualTree: TBaseVirtualTree; aSearchGroups: TSearchGroups; aPrefix: String = ''): Integer;
var
  Node, YearNode, MonthNode: PVirtualNode;
  Data: PDateNodeData;
  YearValue, MonthValue, DayValue: Integer;
  L, FBL: String;
  FS: TFormatSettings;
  Lang: Integer;
  sf: Integer;
  BeforeCount: Integer;
  Years, Months, Days: TStringList;
  i: Integer;
  Y, M, D, YM: String;

  function EnsureDateGroup: TSearchGroup;
  begin
    if sf < 0 then
      sf := aSearchGroups.Add(TSearchGroup.Create);
    Result := aSearchGroups[sf];
  end;

  procedure FilterDates(aYear, aMonth, aDay: Integer; aSearchGroup: TSearchGroup);
  begin
    if aYear <= 0 then
      Exit;

    case aTable of
      tbNone: ;
      tbPermits: ;
      tbNests:         FilterNestDates(aYear, aMonth, aDay, aSearchGroup);
      tbNestRevisions: ;
      tbEggs: ;
      tbProjects:      FilterProjectDates(aYear, aMonth, aDay, aSearchGroup);
      tbPeople:        FilterPeopleDates(aYear, aMonth, aDay, aSearchGroup);
      tbExpeditions:   FilterExpeditionDates(aYear, aMonth, aDay, aSearchGroup);
      tbSurveys:       FilterSurveyDates(aYear, aMonth, aDay, aSearchGroup);
      tbSightings:     FilterSightingDates(aYear, aMonth, aDay, aSearchGroup);
      tbSpecimens:     FilterSpecimenDates(aYear, aMonth, aDay, aSearchGroup);
      tbIndividuals:   FilterIndividualDates(aYear, aMonth, aDay, aSearchGroup);
      tbCaptures:      FilterCaptureDates(aYear, aMonth, aDay, aSearchGroup);
    end;
  end;

  procedure AddDateFilter(aYear, aMonth, aDay: Integer);
  begin
    BeforeCount := EnsureDateGroup.Fields.Count;
    FilterDates(aYear, aMonth, aDay, EnsureDateGroup);
    if EnsureDateGroup.Fields.Count > BeforeCount then
      Inc(Result);
  end;

begin
  Result := 0;
  sf := -1;

  if aVirtualTree.CheckedCount <= 0 then
    Exit;

  // Get the date format settings
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
  GetMacFormatSettings(FS);
  if Lang = $0409 then // English (USA)
  begin
    FS.DecimalSeparator := '.';
    FS.ThousandSeparator := ',';
    FS.DateSeparator := '/';
    FS.ShortDateFormat := 'MM/DD/YYYY';
  end
  else
  if Lang = $0416 then // Portuguese (Brazil)
  begin
    FS.DecimalSeparator := ',';
    FS.ThousandSeparator := '.';
    FS.DateSeparator := '/';
    FS.ShortDateFormat := 'DD/MM/YYYY';
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  FS := FormatSettings;
  {$ENDIF}

  Years := TStringList.Create;
  Months := TStringList.Create;
  Days := TStringList.Create;
  try
    Years.Sorted := True;
    Years.Duplicates := dupIgnore;
    Months.Sorted := True;
    Months.Duplicates := dupIgnore;

    Days.Sorted := True;
    Days.Duplicates := dupIgnore;

    Node := aVirtualTree.GetFirstChecked;
    while Assigned(Node) do
    begin
      case aVirtualTree.GetNodeLevel(Node) of
        0:
          begin
            Data := aVirtualTree.GetNodeData(Node);
            YearValue := StrToIntDef(Data^.Caption, 0);
            if YearValue > 0 then
              Years.Add(Format('%4.4d', [YearValue]));
          end;

        1:
          begin
            YearNode := Node^.Parent;
            if Assigned(YearNode) then
            begin
              Data := aVirtualTree.GetNodeData(YearNode);
              YearValue := StrToIntDef(Data^.Caption, 0);

              Data := aVirtualTree.GetNodeData(Node);
              MonthValue := IndexText(Data^.Caption, FS.LongMonthNames) + 1;

              if (YearValue > 0) and (MonthValue > 0) then
                Months.Add(Format('%4.4d-%2.2d', [YearValue, MonthValue]));
            end;
          end;

        2:
          begin
            MonthNode := Node^.Parent;
            if Assigned(MonthNode) and Assigned(MonthNode^.Parent) then
            begin
              YearNode := MonthNode^.Parent;

              Data := aVirtualTree.GetNodeData(YearNode);
              YearValue := StrToIntDef(Data^.Caption, 0);

              Data := aVirtualTree.GetNodeData(MonthNode);
              MonthValue := IndexText(Data^.Caption, FS.LongMonthNames) + 1;

              Data := aVirtualTree.GetNodeData(Node);
              DayValue := StrToIntDef(Data^.Caption, 0);

              if (YearValue > 0) and (MonthValue > 0) and (DayValue > 0) then
                Days.Add(Format('%4.4d-%2.2d-%2.2d', [YearValue, MonthValue, DayValue]));
            end;
          end;
      end;

      Node := aVirtualTree.GetNextChecked(Node);
    end;

    for i := Months.Count - 1 downto 0 do
    begin
      Y := ExtractDelimited(1, Months[i], ['-']);
      if Years.IndexOf(Y) >= 0 then
        Months.Delete(i);
    end;

    for i := Days.Count - 1 downto 0 do
    begin
      Y := ExtractDelimited(1, Days[i], ['-']);
      M := ExtractDelimited(2, Days[i], ['-']);
      YM := Y + '-' + M;

      if (Years.IndexOf(Y) >= 0) or (Months.IndexOf(YM) >= 0) then
        Days.Delete(i);
    end;

    for i := 0 to Years.Count - 1 do
    begin
      Y := Years[i];
      AddDateFilter(StrToIntDef(Y, 0), 0, 0);
    end;

    for i := 0 to Months.Count - 1 do
    begin
      Y := ExtractDelimited(1, Months[i], ['-']);
      M := ExtractDelimited(2, Months[i], ['-']);
      AddDateFilter(StrToIntDef(Y, 0), StrToIntDef(M, 0), 0);
    end;

    for i := 0 to Days.Count - 1 do
    begin
      Y := ExtractDelimited(1, Days[i], ['-']);
      M := ExtractDelimited(2, Days[i], ['-']);
      D := ExtractDelimited(3, Days[i], ['-']);
      AddDateFilter(StrToIntDef(Y, 0), StrToIntDef(M, 0), StrToIntDef(D, 0));
    end;

    if (sf >= 0) and (aSearchGroups[sf].Fields.Count = 0) then
      aSearchGroups.Delete(sf);
  finally
    Years.Free;
    Months.Free;
    Days.Free;
  end;
end;

end.
