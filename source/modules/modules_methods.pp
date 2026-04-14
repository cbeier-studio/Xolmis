{ Xolmis Methods Module controllers

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

unit modules_methods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, DBGrids, RegExpr, data_types, modules_core;

type

  { TMethodsModuleController }

  TMethodsModuleController = class(TModuleController)
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

{ TMethodsModuleController }

constructor TMethodsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbMethods;
  FCaptionText := rsTitleMethods;
  FDataSet := DMG.qMethods;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowDocs, gufShowSummary];
  FPrintUiFlags := [pufMethods];
  FFilterUiFlags := [fufMarked, fufCategory];

  AddDefaultSort(COL_METHOD_NAME, sdAscending);
end;

procedure TMethodsModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Category
    if cbCategoryFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_CATEGORY, rscCategory, cbCategoryFilter.Text);
    end;
  end;
end;

procedure TMethodsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    cbCategoryFilter.ItemIndex := 0;
  end;
end;

procedure TMethodsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_METHOD_ID).Visible then
      ColumnByFieldname(COL_METHOD_ID).ReadOnly := True;
  end;
end;

procedure TMethodsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_ABBREVIATION) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TMethodsModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
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
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_METHOD_ID, rscId, sdtInteger, crEqual,
          True, aValue));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, rscName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_METHOD_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_EBIRD_NAME, rscEBirdName, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

