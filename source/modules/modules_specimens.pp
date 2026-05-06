{ Xolmis Specimens Module controllers

  Copyright (C) 2026 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit modules_specimens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, DateUtils,
  data_types, modules_core;

type

  { TSpecimensModuleController }

  TSpecimensModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSpecimensSubmoduleController }

  TSpecimensSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSpecimenCollectorsSubmoduleController }

  TSpecimenCollectorsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSamplePrepsSubmoduleController }

  TSamplePrepsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, utils_validations, utils_system,
  data_consts, data_columns, data_filters, models_media,
  uDarkStyleParams,
  udm_main, udm_grid, udm_individuals, ufrm_customgrid;

{ TSpecimensModuleController }

constructor TSpecimensModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSpecimens;
  FCaptionText := rsTitleSpecimens;
  FDataSet := DMG.qSpecimens;
  FSupportedMedia := [amtImages, amtAudios, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages, gufShowAudios, gufShowDocs];
  FPrintUiFlags := [pufSpecimens, pufSpecimensByLocality, pufSpecimensByProject, pufSpecimensByTaxon,
    pufSpecimensByYear];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufSampleType, fufNest, fufEgg, fufIndividual,
    fufInstitution];

  AddDefaultSort(COL_FULL_NAME, sdAscending);

  FSubmodules.Add(TSpecimenCollectorsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TSamplePrepsSubmoduleController.Create(FOwner));
end;

procedure TSpecimensModuleController.ApplyFilters;
const
  SampleTypes: array of String = ('WS', 'PS', 'N', 'B', 'E', 'P', 'F', 'BS', 'C', 'S', 'T', 'D', 'R');
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Taxa
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    // Sites
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Sample type
    if cbMaterialFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_SAMPLE_TYPE, rscType, SampleTypes[cbMaterialFilter.ItemIndex - 1]);
    end;
    // Nest
    AddLookupFilter(SearchConfig, [COL_NEST_ID], [rscNest], NestIdFilter);
    // Egg
    AddLookupFilter(SearchConfig, [COL_EGG_ID], [rscEgg], EggIdFilter);
    // Individual
    AddLookupFilter(SearchConfig, [COL_INDIVIDUAL_ID], [rscIndividual], IndividualIdFilter);
    // Individual
    AddLookupFilter(SearchConfig, [COL_INSTITUTION_ID], [rscInstitution], InstitutionIdFilter);
  end;
end;

procedure TSpecimensModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbMaterialFilter.ItemIndex := 0;

    eNestFilter.Clear;
    NestIdFilter := 0;
    eEggFilter.Clear;
    EggIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;
    eInstitutionFilter.Clear;
    InstitutionIdFilter := 0;
  end;
end;

procedure TSpecimensModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SPECIMEN_ID).Visible then
    begin
      ColumnByFieldname(COL_SPECIMEN_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
    if DataSource.DataSet.FieldByName(COL_SAMPLE_TYPE).Visible then
      with ColumnByFieldName(COL_SAMPLE_TYPE).PickList do
      begin
        Clear;
        Add(rsSpecimenCarcassWhole);
        Add(rsSpecimenCarcassPartial);
        Add(rsSpecimenNest);
        Add(rsSpecimenBones);
        Add(rsSpecimenEgg);
        Add(rsSpecimenParasites);
        Add(rsSpecimenFeathers);
        Add(rsSpecimenBlood);
        Add(rsSpecimenClaw);
        Add(rsSpecimenSwab);
        Add(rsSpecimenTissues);
        Add(rsSpecimenFeces);
        Add(rsSpecimenRegurgite);
      end;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EGG_NAME).Visible then
      ColumnByFieldName(COL_EGG_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Visible then
      ColumnByFieldName(COL_INSTITUTION_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSpecimensModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_COLLECTION_DATE) or
    (Column.FieldName = COL_FIELD_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

function TSpecimensModuleController.Search(AValue: String): Boolean;
var
  i, g, m, y: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
  dia, mes, ano: Word;
  PartialStart, PartialEnd: TPartialDate;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^\$.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, '$', '', [rfReplaceAll]);
    end;

    with TfrmCustomGrid(FOwner) do
    begin
      // Date interval
      if TryParsePartialDateIntervalFlexible(aValue, PartialStart, PartialEnd) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_COLLECTION_YEAR, rscCollectionDate, sdtSplitDate, crBetween,
            True, PartialStart.ToSearchKey, PartialEnd.ToSearchKey, '', COL_COLLECTION_MONTH, COL_COLLECTION_DAY)
        );
      end
      else
      // ID and year
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SPECIMEN_ID, rscId, sdtInteger, crEqual,
          True, aValue));
        if IsLikelyYear(i) then
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_COLLECTION_YEAR, rscCollectionYear, sdtSplitDate, crEqual,
            True, aValue));
      end
      else
      // Date
      if TryParseDateFlexible(aValue, dt) then
      begin
        DecodeDate(dt, ano, mes, dia);
        //aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_COLLECTION_YEAR, rscCollectionDate, sdtSplitDate, crEqual,
          True, IntToStr(ano), IntToStr(mes), IntToStr(dia), COL_COLLECTION_MONTH, COL_COLLECTION_DAY));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        //aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_COLlECTION_YEAR, rscCollectionDate, sdtSplitDate, crEqual,
          True, IntToStr(y), IntToStr(m), '', COL_COLLECTION_MONTH));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_INSTITUTION_NAME, rscInstitution, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TSpecimensSubmoduleController }

constructor TSpecimensSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSpecimens;
  FCaptionText := rsTitleSpecimens;
  FDataSet := DMI.qSpecimens;
  FGrid := TfrmCustomGrid(FOwner).gridChild5;
  FPageIndex := 4;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TSpecimensSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SPECIMEN_ID).Visible then
    begin
      ColumnByFieldname(COL_SPECIMEN_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
    if DataSource.DataSet.FieldByName(COL_SAMPLE_TYPE).Visible then
      with ColumnByFieldName(COL_SAMPLE_TYPE).PickList do
      begin
        Clear;
        Add(rsSpecimenCarcassWhole);
        Add(rsSpecimenCarcassPartial);
        Add(rsSpecimenNest);
        Add(rsSpecimenBones);
        Add(rsSpecimenEgg);
        Add(rsSpecimenParasites);
        Add(rsSpecimenFeathers);
        Add(rsSpecimenBlood);
        Add(rsSpecimenClaw);
        Add(rsSpecimenSwab);
        Add(rsSpecimenTissues);
        Add(rsSpecimenFeces);
        Add(rsSpecimenRegurgite);
      end;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EGG_NAME).Visible then
      ColumnByFieldName(COL_EGG_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Visible then
      ColumnByFieldName(COL_INSTITUTION_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSpecimensSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_COLLECTION_DATE) or
    (Column.FieldName = COL_FIELD_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

{ TSpecimenCollectorsSubmoduleController }

constructor TSpecimenCollectorsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSpecimenCollectors;
  FCaptionText := rsTitleSpecimenCollectors;
  FDataSet := DMG.qSampleCollectors;
  FGrid := TfrmCustomGrid(FOwner).gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_COLLECTOR_SEQUENCE, sdAscending);
end;

procedure TSpecimenCollectorsSubmoduleController.ConfigureColumns;
begin

end;

procedure TSpecimenCollectorsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TSamplePrepsSubmoduleController }

constructor TSamplePrepsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSamplePreps;
  FCaptionText := rsTitleSamplePreps;
  FDataSet := DMG.qSamplePreps;
  FGrid := TfrmCustomGrid(FOwner).gridChild2;
  FPageIndex := 1;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TSamplePrepsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Visible then
      ColumnByFieldName(COL_INSTITUTION_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSamplePrepsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

end.

