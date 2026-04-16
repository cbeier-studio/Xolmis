{ Xolmis People Module controllers

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

unit modules_people;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TPeopleModuleController }

  TPeopleModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_validations,
  data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, ufrm_customgrid;

{ TPeopleModuleController }

constructor TPeopleModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbPeople;
  FCaptionText := rsTitleResearchers;
  FDataSet := DMG.qPeople;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowDocs, gufShowSummary];
  FPrintUiFlags := [pufResearchers];
  FFilterUiFlags := [fufMarked, fufSites, fufDates, fufInstitution];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TPeopleModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Sites
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'p.');
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Institution
    AddLookupFilter(SearchConfig, [COL_INSTITUTION_ID], [rscInstitution], InstitutionIdFilter);
  end;
end;

procedure TPeopleModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eInstitutionFilter.Clear;
    InstitutionIdFilter := 0;
  end;
end;

procedure TPeopleModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERSON_ID).Visible then
      ColumnByFieldname(COL_PERSON_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_GENDER).Visible then
      ColumnByFieldName(COL_GENDER).PickList.AddCommaText(rsGenderList);
    if DataSource.DataSet.FieldByName(COL_TITLE_TREATMENT).Visible then
      ColumnByFieldName(COL_TITLE_TREATMENT).PickList.AddCommaText(rsTreatmentList);

    if DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Visible then
      ColumnByFieldname(COL_INSTITUTION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Visible then
      ColumnByFieldname(COL_COUNTRY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_STATE_NAME).Visible then
      ColumnByFieldname(COL_STATE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Visible then
      ColumnByFieldname(COL_MUNICIPALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BIRTH_DATE).Visible then
      ColumnByFieldName(COL_BIRTH_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DEATH_DATE).Visible then
      ColumnByFieldName(COL_DEATH_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TPeopleModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_ABBREVIATION then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TPeopleModuleController.Search(AValue: String): Boolean;
var
  i, g, m, y: Integer;
  dt: TDateTime;
  Crit: TCriteriaType;
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
      // ID
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PERSON_ID, rscId, sdtInteger, crEqual,
          True, aValue));
        if IsLikelyYear(i) then
        begin
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, rscDate, sdtYear, crEqual,
            False, aValue));
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, rscDate, sdtYear, crEqual,
            False, aValue));
        end;
      end
      else
      // Date
      if TryParseDateFlexible(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, rscBirthDate, sdtDate, crEqual,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, rscDeathDate, sdtDate, crEqual,
          True, aValue));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, rscBirthDate, sdtMonthYear, crEqual,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, rscDeathDate, sdtMonthYear, crEqual,
          True, aValue));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_CITATION, rscCitation, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

