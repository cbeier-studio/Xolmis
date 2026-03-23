unit modules_botany;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, StdCtrls,
  data_types, modules_core, ufrm_customgrid;

type

  { TBotanicalTaxaModuleController }

  TBotanicalTaxaModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

implementation

uses
  utils_locale, data_consts, data_columns, data_filters, data_getvalue, models_media, udm_main, udm_grid;

{ TBotanicalTaxaModuleController }

constructor TBotanicalTaxaModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbBotanicTaxa;
  FCaptionText := rsTitleBotanicalTaxa;
  FDataSet := DMG.qBotany;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufPrintMain, gufPrintRecorded];
  FFilterUiFlags := [fufMarked, fufTaxonRanks, fufSynonyms];

  AddDefaultSort(COL_TAXON_NAME, sdAscending);
end;

procedure TBotanicalTaxaModuleController.ApplyFilters;
var
  sf, cc, i: Integer;
begin
  cc := 0;
  with FOwner do
  begin
    for i := 0 to clbTaxonRanksFilter.Count - 1 do
      if clbTaxonRanksFilter.Checked[i] then
        Inc(cc);
    if cc > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      for i := 0 to clbTaxonRanksFilter.Count - 1 do
        if clbTaxonRanksFilter.Checked[i] then
          SearchConfig.QuickFilters.Items[sf].Fields.Add(TSearchField.Create(COL_RANK_ID, rscTaxonomicRank, sdtInteger,
            crEqual, False, IntToStr(GetKey('taxon_ranks', COL_RANK_ID, COL_RANK_NAME, clbTaxonRanksFilter.Items[i]))));
    end;

    if rbIsSynonymYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, rscValidName, sdtInteger,
        crNotEqual, False, '0'));
    end;
    if rbIsSynonymNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_VALID_ID, rscValidName, sdtInteger,
        crEqual, False, '0'));
    end;
    //if rbHasSynonymsYes.Checked then
    //begin
    //  sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
    //  SearchConfig.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
    //    crMoreThan, False, '1'));
    //end;
    //if rbHasSynonymsNo.Checked then
    //begin
    //  sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
    //  SearchConfig.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
    //    crEqual, False, '0'));
    //end;
  end;
end;

procedure TBotanicalTaxaModuleController.ClearFilters;
begin
  with FOwner do
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

procedure TBotanicalTaxaModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
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
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscScientificName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_VERNACULAR_NAME, rscVernacularNameS, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_AUTHORSHIP, rscAuthorship, sdtText, Crit,
        False, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

end.

