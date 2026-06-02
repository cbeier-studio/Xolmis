{ Xolmis Occurrence Points Module controllers

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

unit modules_pois;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, DateUtils,
  data_types, modules_core;

type

  { TPoiModuleController }

  TPoiModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TPoiSubmoduleController }

  TPoiSubmoduleController = class(TSubmoduleController)
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
  udm_main, udm_grid, udm_individuals, udm_sampling, ufrm_customgrid;

{ TPoiModuleController }

constructor TPoiModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbPoiLibrary;
  FCaptionText := rsTitleOccurrencePoints;
  FDataSet := DMG.qPois;
  FSupportedMedia := [amtImages, amtAudios, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages, gufShowAudios, gufShowDocs];
  FPrintUiFlags := [];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufTimeInterval, fufPerson, fufSurvey, fufIndividual];

  AddDefaultSort(COL_POI_NAME, sdAscending);
end;

procedure TPoiModuleController.ApplyFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Taxa
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Time interval
    AddTimeFilter(SearchConfig, COL_SAMPLE_TIME, rscTime, eStartTimeFilter.Text, eEndTimeFilter.Text);
    // Observer
    AddLookupFilter(SearchConfig, [COL_OBSERVER_ID], [rscPerson], PersonIdFilter);
    // Survey
    AddLookupFilter(SearchConfig, [COL_SURVEY_ID], [rscSurvey], SurveyIdFilter);
    // Individual
    AddLookupFilter(SearchConfig, [COL_INDIVIDUAL_ID], [rscIndividual], IndividualIdFilter);
  end;
end;

procedure TPoiModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eSurveyFilter.Clear;
    SurveyIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;
  end;
end;

procedure TPoiModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_POI_ID).Visible then
    begin
      ColumnByFieldname(COL_POI_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;

    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_NAME).Visible then
      ColumnByFieldName(COL_SIGHTING_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldName(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TPoiModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_SAMPLE_DATE) or
    (Column.FieldName = COL_POI_NAME) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

function TPoiModuleController.Search(AValue: String): Boolean;
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
          TSearchField.Create(COL_SAMPLE_DATE, rscDate, sdtDate, crBetween,
            True, PartialStart.ToStartDateString, PartialEnd.ToEndDateString)
        );
      end
      else
      // ID and year
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_POI_ID, rscId, sdtInteger, crEqual,
          True, aValue));
        if IsLikelyYear(i) then
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, rscDate, sdtYear, crEqual,
            True, aValue));
      end
      else
      // Date
      if TryParseDateFlexible(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, rscDate, sdtDate, crEqual,
          True, aValue));
      end
      else
      // Time
      if TryParseTimeFlexible(aValue, dt) then
      begin
        aValue := FormatDateTime('hh:nn:ss', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SAMPLE_TIME, rscTime, sdtTime, crEqual,
          True, aValue));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, rscDate, sdtMonthYear, crEqual,
          True, aValue));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_POI_NAME, rscName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_OBSERVER_NAME, rscObserver, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TPoiSubmoduleController }

constructor TPoiSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbPoiLibrary;
  FCaptionText := rsTitleOccurrencePoints;
  case TfrmCustomGrid(FOwner).TableType of
    tbSurveys:
      begin
        FDataSet := DMS.qPois;
        FGrid := TfrmCustomGrid(FOwner).gridChild7;
        FPageIndex := 6;
      end;
    tbIndividuals:
      begin
        FDataSet := DMI.qPois;
        FGrid := TfrmCustomGrid(FOwner).gridChild6;
        FPageIndex := 5;
      end;
    tbSightings:
      begin
        FDataSet := DMG.qSightingPois;
        FGrid := TfrmCustomGrid(FOwner).gridChild1;
        FPageIndex := 0;
      end;
  end;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_POI_NAME, sdAscending);
end;

procedure TPoiSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_POI_ID).Visible then
    begin
      ColumnByFieldname(COL_POI_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;

    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_NAME).Visible then
      ColumnByFieldName(COL_SIGHTING_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldName(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TPoiSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_SAMPLE_DATE) or
    (Column.FieldName = COL_POI_NAME) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

end.

