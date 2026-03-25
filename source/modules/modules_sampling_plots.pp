unit modules_sampling_plots;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TSamplingPlotsModuleController }

  TSamplingPlotsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TPermanentNetsSubmoduleController }

  TPermanentNetsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, ufrm_customgrid;

{ TSamplingPlotsModuleController }

constructor TSamplingPlotsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSamplingPlots;
  FCaptionText := rsTitleSamplingPlots;
  FDataSet := DMG.qSamplingPlots;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowMap, gufShowSummary, gufShowDocs];
  FPrintUiFlags := [pufSamplingPlots, pufSamplingPlotsByLocality];
  FFilterUiFlags := [fufMarked, fufSiteRank, fufSites];

  AddDefaultSort(COL_FULL_NAME, sdAscending);

  FSubmodules.Add(TPermanentNetsSubmoduleController.Create(FOwner));
end;

procedure TSamplingPlotsModuleController.ApplyFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Sites
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');
  end;
end;

procedure TSamplingPlotsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;
  end;
end;

procedure TSamplingPlotsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SAMPLING_PLOT_ID).Visible then
      ColumnByFieldname(COL_SAMPLING_PLOT_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSamplingPlotsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

function TSamplingPlotsModuleController.Search(AValue: String): Boolean;
var
  i, g: Integer;
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
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SAMPLING_PLOT_ID, rscId, sdtInteger, crEqual,
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
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TPermanentNetsSubmoduleController }

constructor TPermanentNetsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbPermanentNets;
  FCaptionText := rsTitlePermanentNets;
  FDataSet := DMG.qPermanentNets;
  FGrid := TfrmCustomGrid(FOwner).gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_NET_NUMBER, sdAscending);
end;

procedure TPermanentNetsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERMANENT_NET_ID).Visible then
      ColumnByFieldname(COL_PERMANENT_NET_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TPermanentNetsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_NET_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

end.

