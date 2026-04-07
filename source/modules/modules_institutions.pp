{ Xolmis Institutions Module controllers

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

unit modules_institutions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TInstitutionsModuleController }

  TInstitutionsModuleController = class(TModuleController)
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
  utils_locale, utils_graphics, data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, ufrm_customgrid;

{ TInstitutionsModuleController }

constructor TInstitutionsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbInstitutions;
  FCaptionText := rsTitleInstitutions;
  FDataSet := DMG.qInstitutions;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowSummary];
  FPrintUiFlags := [pufInstitutions];
  FFilterUiFlags := [fufMarked, fufSites];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TInstitutionsModuleController.ApplyFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Sites
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'it.');
  end;
end;

procedure TInstitutionsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;
  end;
end;

procedure TInstitutionsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_INSTITUTION_ID).Visible then
      ColumnByFieldname(COL_INSTITUTION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Visible then
      ColumnByFieldname(COL_COUNTRY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_STATE_NAME).Visible then
      ColumnByFieldname(COL_STATE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Visible then
      ColumnByFieldname(COL_MUNICIPALITY_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TInstitutionsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_ABBREVIATION then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TInstitutionsModuleController.Search(AValue: String): Boolean;
var
  i, g: Integer;
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
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_INSTITUTION_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_MANAGER_NAME, rscManager, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

