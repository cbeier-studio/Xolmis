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
  utils_locale, utils_graphics, data_consts, data_columns, models_media,
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
    if cbCategoryFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CATEGORY, rscCategory, sdtText,
        crEqual, False, cbCategoryFilter.Text));
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
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, rscName, sdtText, Crit,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EBIRD_NAME, rscEBirdName, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

