unit modules_methods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, DBGrids, RegExpr, data_types, modules_core, ufrm_customgrid;

type

  { TMethodsModuleController }

  TMethodsModuleController = class(TModuleController)
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
  utils_locale, utils_graphics, data_consts, data_columns, models_media, udm_main, udm_grid;

{ TMethodsModuleController }

constructor TMethodsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbMethods;
  FCaptionText := rsTitleMethods;
  FDataSet := DMG.qMethods;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowDocs, gufShowSummary, gufPrintMain];
  FFilterUiFlags := [fufMarked, fufCategory];

  AddDefaultSort(COL_METHOD_NAME, sdAscending);
end;

procedure TMethodsModuleController.ApplyFilters;
var
  sf: Integer;
begin
  if FOwner.cbCategoryFilter.ItemIndex > 0 then
  begin
    sf := FOwner.SearchConfig.QuickFilters.Add(TSearchGroup.Create);
    FOwner.SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CATEGORY, rscCategory, sdtText,
      crEqual, False, FOwner.cbCategoryFilter.Text));
  end;
end;

procedure TMethodsModuleController.ClearFilters;
begin
  FOwner.cbCategoryFilter.ItemIndex := 0;
end;

procedure TMethodsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_METHOD_ID).Visible then
      ColumnByFieldname(COL_METHOD_ID).ReadOnly := True;
  end;
end;

procedure TMethodsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
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

    if TryStrToInt(aValue, i) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, rscName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_ABBREVIATION, rscAbbreviation, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EBIRD_NAME, rscEBirdName, sdtText, Crit,
        False, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

end.

