{ Xolmis Sightings Module controllers

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

unit modules_sightings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TSightingsModuleController }

  TSightingsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSightingsSubmoduleController }

  TSightingsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, utils_validations,
  data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, udm_sampling, udm_individuals, ufrm_customgrid, uDarkStyleParams;

{ TSightingsModuleController }

constructor TSightingsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSightings;
  FCaptionText := rsTitleSightings;
  FDataSet := DMG.qSightings;
  FSupportedMedia := [amtImages, amtAudios, amtVideos, amtDocuments];
  FUiFlags := [gufShowImages, gufShowAudios, gufShowVideos, gufShowDocs, gufShowSummary, gufShowMap,
    gufShowVerifications];
  FPrintUiFlags := [pufSightings, pufSightingsByLocality, pufSightingsByObserver, pufSightingsByProject,
    pufSightingsBySurvey, pufSightingsByTaxon];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufTimeInterval, fufPerson, fufSurvey, fufMethod,
    fufIndividual, fufIsOnEbird, fufOutOfSample];

  AddDefaultSort(COL_SIGHTING_DATE, sdDescending);
end;

procedure TSightingsModuleController.ApplyFilters;
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
    // Person
    AddLookupFilter(SearchConfig, [COL_OBSERVER_ID], [rscObserver], PersonIdFilter);
    // Time interval
    AddTimeFilter(SearchConfig, COL_SIGHTING_TIME, rscTime, eStartTimeFilter.Text, eEndTimeFilter.Text);
    // Survey
    AddLookupFilter(SearchConfig, [COL_SURVEY_ID], [rscSurvey], SurveyIdFilter);
    // Method
    AddLookupFilter(SearchConfig, [COL_METHOD_ID], [rscMethod], MethodIdFilter);
    // Individual
    AddLookupFilter(SearchConfig, [COL_INDIVIDUAL_ID], [rscIndividual], IndividualIdFilter);
    // Is in eBird
    AddBooleanFilter(SearchConfig, COL_EBIRD_AVAILABLE, rscIsInEBird, rbRecordInEbirdYes.Checked, rbRecordInEbirdNo.Checked);
    // Out of sample
    AddBooleanFilter(SearchConfig, COL_OUT_OF_SAMPLE, rscOutOfSample, rbOutOfSampleYes.Checked, rbOutOfSampleNo.Checked);
  end;
end;

procedure TSightingsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eSurveyFilter.Clear;
    SurveyIdFilter := 0;
    eMethodFilter.Clear;
    MethodIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;

    rbRecordInEbirdAll.Checked := True;
    rbOutOfSampleAll.Checked := True;
  end;
end;

procedure TSightingsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SIGHTING_ID).Visible then
      ColumnByFieldname(COL_SIGHTING_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldname(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DETECTION_TYPE).Visible then
      ColumnByFieldname(COL_DETECTION_TYPE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BREEDING_STATUS).Visible then
      ColumnByFieldname(COL_BREEDING_STATUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_DATE).Visible then
      ColumnByFieldname(COL_SIGHTING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
  end;
end;

procedure TSightingsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_SIGHTING_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_OUT_OF_SAMPLE).Field.AsBoolean = True) then
    begin
      if IsDarkModeEnabled then
      begin
        TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
        TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
      end
      else
      begin
        TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
        TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
      end;
    end;
  end;
end;

function TSightingsModuleController.Search(AValue: String): Boolean;
var
  i, g, m, y, y1, y2: Longint;
  dt, Dt1, Dt2, Tm1, Tm2: TDateTime;
  Crit: TCriteriaType;
  V1, V2: String;
  M1, M2: Integer;
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
      // Year interval
      if TryParseYearInterval(aValue, y1, y2) then
      begin
        V1 := IntToStr(y1);
        V2 := IntToStr(y2);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtYear, crBetween,
          True, V1, V2));
      end
      else
      // Date interval
      if TryParseDateIntervalFlexible(aValue, Dt1, Dt2) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtDate, crBetween,
            True, FormatDateTime('yyyy-mm-dd', Dt1), FormatDateTime('yyyy-mm-dd', Dt2))
        );
      end
      else
      // Time interval
      if TryParseTimeIntervalFlexible(aValue, Tm1, Tm2) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_SIGHTING_TIME, rscTime, sdtTime, crBetween,
            True, FormatDateTime('hh:nn:ss', Tm1), FormatDateTime('hh:nn:ss', Tm2))
        );
      end
      else
      // Month/year interval
      if TryParseMonthYearInterval(aValue, Y1, M1, Y2, M2) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtMonthYear, crBetween,
            True,
            Format('%.4d-%.2d', [Y1, M1]),
            Format('%.4d-%.2d', [Y2, M2]))
        );
      end
      else
      // ID and year
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SIGHTING_ID, rscSightingID, sdtInteger, crEqual,
          True, aValue));
        if IsLikelyYear(i) then
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtYear, crEqual,
            True, aValue));
      end
      else
      // Date
      if TryParseDateFlexible(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtDate, crEqual,
          True, aValue));
      end
      else
      // Time
      if TryParseTimeFlexible(aValue, dt) then
      begin
        aValue := FormatDateTime('hh:nn:ss', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, rscTime, sdtTime, crEqual,
          True, aValue));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtMonthYear, crEqual,
          True, aValue));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, rscMethod, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TSightingsSubmoduleController }

constructor TSightingsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSightings;
  FCaptionText := rsTitleSightings;
  case TfrmCustomGrid(FOwner).TableType of
    tbSurveys:
      begin
        FDataSet := DMS.qSightings;
        FGrid := TfrmCustomGrid(FOwner).gridChild5;
        FPageIndex := 4;
      end;
    tbIndividuals:
      begin
        FDataSet := DMI.qSightings;
        FGrid := TfrmCustomGrid(FOwner).gridChild3;
        FPageIndex := 2;
      end;
  end;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_SIGHTING_DATE, sdDescending);
end;

procedure TSightingsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SIGHTING_ID).Visible then
      ColumnByFieldname(COL_SIGHTING_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldname(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DETECTION_TYPE).Visible then
      ColumnByFieldname(COL_DETECTION_TYPE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BREEDING_STATUS).Visible then
      ColumnByFieldname(COL_BREEDING_STATUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_DATE).Visible then
      ColumnByFieldname(COL_SIGHTING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
  end;
end;

procedure TSightingsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_SIGHTING_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_OUT_OF_SAMPLE).Field.AsBoolean = True) then
    begin
      if IsDarkModeEnabled then
      begin
        TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
        TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
      end
      else
      begin
        TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
        TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
      end;
    end;
  end;
end;

end.

