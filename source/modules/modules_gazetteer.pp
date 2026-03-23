unit modules_gazetteer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TGazetteerModuleController }

  TGazetteerModuleController = class(TModuleController)
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
  utils_locale, data_consts, data_columns, data_filters, models_media, udm_main, udm_grid;

{ TGazetteerModuleController }

constructor TGazetteerModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbGazetteer;
  FCaptionText := rsTitleGazetteer;
  FDataSet := DMG.qGazetteer;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowMap, gufShowSummary, gufShowMoreOptions, gufShowAddCountries,
    gufShowAddMunicipalities, gufPrintMain];
  FFilterUiFlags := [fufMarked, fufSiteRank, fufSites];

  AddDefaultSort(COL_SITE_NAME, sdAscending);
end;

procedure TGazetteerModuleController.ApplyFilters;
const
  SiteRanks: array of String = ('P', 'E', 'R', 'M', 'D', 'L');
var
  sf: Integer;
begin
  with FOwner do
  begin
    if cbSiteRankFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SITE_RANK, rscType, sdtText,
        crEqual, False, SiteRanks[cbSiteRankFilter.ItemIndex - 1]));
    end;
    { #todo : apply filter for sites in TGazetteerModuleController }
  end;
end;

procedure TGazetteerModuleController.ClearFilters;
begin
  with FOwner do
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

procedure TGazetteerModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
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

    if TryStrToInt(aValue, i) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SITE_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToFloat(aValue, f) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LONGITUDE, rscLongitude, sdtText, crStartLike,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LATITUDE, rscLatitude, sdtText, crStartLike,
        False, aValue));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SITE_NAME, rscFullName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SITE_ABBREVIATION, rscAbbreviation, sdtText, Crit,
        False, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

end.

