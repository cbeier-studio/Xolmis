unit modules_institutions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TInstitutionsModuleController }

  TInstitutionsModuleController = class(TModuleController)
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
  utils_locale, utils_graphics, data_consts, data_columns, data_filters, models_media, udm_main, udm_grid;

{ TInstitutionsModuleController }

constructor TInstitutionsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbInstitutions;
  FCaptionText := rsTitleInstitutions;
  FDataSet := DMG.qInstitutions;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowSummary, gufPrintMain];
  FFilterUiFlags := [fufMarked, fufSites];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TInstitutionsModuleController.ApplyFilters;
begin
  with FOwner do
  begin
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'it.');
  end;
end;

procedure TInstitutionsModuleController.ClearFilters;
begin
  with FOwner do
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

procedure TInstitutionsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
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

    if TryStrToInt(aValue, i) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_INSTITUTION_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, rscAbbreviation, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_MANAGER_NAME, rscManager, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

end.

