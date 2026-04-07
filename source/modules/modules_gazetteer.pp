{ Xolmis Gazetteer Module controllers

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

unit modules_gazetteer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TGazetteerModuleController }

  TGazetteerModuleController = class(TModuleController)
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
  utils_locale, data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, ufrm_customgrid;

{ TGazetteerModuleController }

constructor TGazetteerModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbGazetteer;
  FCaptionText := rsTitleGazetteer;
  FDataSet := DMG.qGazetteer;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowMap, gufShowSummary, gufShowMoreOptions, gufShowAddCountries,
    gufShowAddMunicipalities];
  FPrintUiFlags := [pufGazetteer];
  FFilterUiFlags := [fufMarked, fufSiteRank, fufSites];

  AddDefaultSort(COL_SITE_NAME, sdAscending);
end;

procedure TGazetteerModuleController.ApplyFilters;
const
  SiteRanks: array of String = ('P', 'E', 'R', 'M', 'D', 'L');
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Site rank
    if cbSiteRankFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_SITE_RANK, rscType, SiteRanks[cbSiteRankFilter.ItemIndex - 1]);
    end;
    // Sites
    { #todo : apply filter for sites in TGazetteerModuleController }
  end;
end;

procedure TGazetteerModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    cbSiteRankFilter.ItemIndex := 0;
  end;
end;

procedure TGazetteerModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SITE_ID).Visible then
      ColumnByFieldname(COL_SITE_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SITE_RANK).Visible then
    begin
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionCountry);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionState);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionRegion);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionMunicipality);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionDistrict);
      ColumnByFieldName(COL_SITE_RANK).PickList.Add(rsCaptionLocality);
    end;

    if DataSource.DataSet.FieldByName(COL_PARENT_SITE_NAME).Visible then
      ColumnByFieldname(COL_PARENT_SITE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TGazetteerModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

function TGazetteerModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
  f: Extended;
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
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SITE_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToFloat(aValue, f) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LONGITUDE, rscLongitude, sdtText, crStartLike,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LATITUDE, rscLatitude, sdtText, crStartLike,
          False, aValue));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SITE_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SITE_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

