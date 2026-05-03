{ Xolmis Botanical Taxa Module controllers

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

unit modules_botany;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, StdCtrls,
  data_types, modules_core;

type

  { TBotanicalTaxaModuleController }

  TBotanicalTaxaModuleController = class(TModuleController)
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
  utils_locale, data_consts, data_columns, data_filters, data_getvalue, models_record_types, models_media,
  udm_main, udm_grid, ufrm_customgrid;

{ TBotanicalTaxaModuleController }

constructor TBotanicalTaxaModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbBotanicTaxa;
  FCaptionText := rsTitleBotanicalTaxa;
  FDataSet := DMG.qBotany;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary];
  FPrintUiFlags := [pufBotanicalTaxa, pufBotanicalTaxaRecorded];
  FFilterUiFlags := [fufMarked, fufTaxonRanks, fufSynonyms];

  AddDefaultSort(COL_SCIENTIFIC_NAME, sdAscending);
end;

procedure TBotanicalTaxaModuleController.ApplyFilters;
var
  sf, cc, i: Integer;
begin
  cc := 0;
  with TfrmCustomGrid(FOwner) do
  begin
    // Taxon rank
    for i := 0 to clbTaxonRanksFilter.Count - 1 do
      if clbTaxonRanksFilter.Checked[i] then
        Inc(cc);
    if cc > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      for i := 0 to clbTaxonRanksFilter.Count - 1 do
        if clbTaxonRanksFilter.Checked[i] then
          SearchConfig.QuickFilters.Items[sf].Fields.Add(TSearchField.Create(COL_RANK_ID, rscTaxonomicRank, sdtInteger,
            crEqual, True, IntToStr(GetRankKey(clbTaxonRanksFilter.Items[i], ncBotanical))));
    end;
    // Is synonym
    if rbIsSynonymYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, rscValidName, sdtInteger,
        crNotEqual, True, '0'));
    end;
    if rbIsSynonymNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, rscValidName, sdtInteger,
        crEqual, True, '0'));
    end;
    // Has synonyms
    //if rbHasSynonymsYes.Checked then
    //begin
    //  sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
    //  SearchConfig.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
    //    crMoreThan, True, '1'));
    //end;
    //if rbHasSynonymsNo.Checked then
    //begin
    //  sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
    //  SearchConfig.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
    //    crEqual, True, '0'));
    //end;
  end;
end;

procedure TBotanicalTaxaModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountTaxonRanksFilter.Caption := rsNoneSelected;
    clbTaxonRanksFilter.CheckAll(cbUnchecked, False);

    rbIsSynonymAll.Checked := True;
    rbHasSynonymsAll.Checked := True;
  end;
end;

procedure TBotanicalTaxaModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_TAXON_ID).Visible then
      ColumnByFieldname(COL_TAXON_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_PARENT_TAXON_NAME).Visible then
      ColumnByFieldname(COL_PARENT_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_VALID_NAME).Visible then
      ColumnByFieldname(COL_VALID_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TBotanicalTaxaModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

function TBotanicalTaxaModuleController.Search(AValue: String): Boolean;
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
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_TAXON_ID, rscId, sdtInteger, crEqual,
          True, aValue));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SCIENTIFIC_NAME, rscScientificName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_VERNACULAR_NAME, rscVernacularNameS, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_AUTHORSHIP, rscAuthorship, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

