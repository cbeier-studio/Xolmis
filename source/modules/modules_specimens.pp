unit modules_specimens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TSpecimensModuleController }

  TSpecimensModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSpecimensSubmoduleController }

  TSpecimensSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSpecimenCollectorsSubmoduleController }

  TSpecimenCollectorsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSamplePrepsSubmoduleController }

  TSamplePrepsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, data_consts, data_columns, data_filters, models_media,
  uDarkStyleParams,
  udm_main, udm_grid, udm_individuals;

{ TSpecimensModuleController }

constructor TSpecimensModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSpecimens;
  FCaptionText := rsTitleSpecimens;
  FDataSet := DMG.qSpecimens;
  FSupportedMedia := [amtImages, amtAudios, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages, gufShowAudios, gufShowDocs,
    gufPrintMain, gufPrintByDate, gufPrintByProject, gufPrintByLocality, gufPrintByTaxon];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufSampleType, fufNest, fufEgg, fufIndividual];

  AddDefaultSort(COL_FULL_NAME, sdAscending);

  FSubmodules.Add(TSpecimenCollectorsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TSamplePrepsSubmoduleController.Create(FOwner));
end;

procedure TSpecimensModuleController.ApplyFilters;
const
  SampleTypes: array of String = ('WS', 'PS', 'N', 'B', 'E', 'P', 'F', 'BS', 'C', 'S', 'T', 'D', 'R');
var
  sf: Integer;
begin
  with FOwner do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if cbMaterialFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLE_TYPE, rscType, sdtText,
        crEqual, False, SampleTypes[cbMaterialFilter.ItemIndex - 1]));
    end;

    if NestIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, rscNest, sdtInteger,
        crEqual, False, IntToStr(NestIdFilter)));
    end;
    if EggIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_ID, rscEgg, sdtInteger,
        crEqual, False, IntToStr(EggIdFilter)));
    end;

    if IndividualIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, rscIndividual, sdtInteger,
        crEqual, False, IntToStr(IndividualIdFilter)));
    end;
  end;
end;

procedure TSpecimensModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbMaterialFilter.ItemIndex := 0;

    eNestFilter.Clear;
    NestIdFilter := 0;
    eEggFilter.Clear;
    EggIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;
  end;
end;

procedure TSpecimensModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SPECIMEN_ID).Visible then
    begin
      ColumnByFieldname(COL_SPECIMEN_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_TYPE).Visible then
      with ColumnByFieldName(COL_SAMPLE_TYPE).PickList do
      begin
        Clear;
        Add(rsSpecimenCarcassWhole);
        Add(rsSpecimenCarcassPartial);
        Add(rsSpecimenNest);
        Add(rsSpecimenBones);
        Add(rsSpecimenEgg);
        Add(rsSpecimenParasites);
        Add(rsSpecimenFeathers);
        Add(rsSpecimenBlood);
        Add(rsSpecimenClaw);
        Add(rsSpecimenSwab);
        Add(rsSpecimenTissues);
        Add(rsSpecimenFeces);
        Add(rsSpecimenRegurgite);
      end;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EGG_NAME).Visible then
      ColumnByFieldName(COL_EGG_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSpecimensModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if (Column.FieldName = COL_COLLECTION_DATE) or
    (Column.FieldName = COL_FIELD_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

function TSpecimensModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
  m, y: String;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SPECIMEN_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_COLLECTION_DATE, rscCollectionDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_COLLECTION_DATE, rscCollectionDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
        True, aValue));
      //FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create('collectors', 'Collectors', sdtText, Crit,
      //  False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
        True, aValue));
      //FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create('municipality_name', 'Municipality', sdtText, Crit,
      //  True, aValue));
      //FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create('state_name', 'State', sdtText, Crit,
      //  True, aValue));
      //FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create('country_name', 'Country', sdtText, Crit,
      //  True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TSpecimensSubmoduleController }

constructor TSpecimensSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSpecimens;
  FCaptionText := rsTitleSpecimens;
  FDataSet := DMI.qSpecimens;
  FGrid := FOwner.gridChild5;
  FPageIndex := 4;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TSpecimensSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SPECIMEN_ID).Visible then
    begin
      ColumnByFieldname(COL_SPECIMEN_ID).ReadOnly := True;
      //ColumnByFieldname('specimen_id').Footer.ValueType := fvtCount;
      //ColumnByFieldname('specimen_id').Footer.Alignment := taCenter;
    end;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_TYPE).Visible then
      with ColumnByFieldName(COL_SAMPLE_TYPE).PickList do
      begin
        Clear;
        Add(rsSpecimenCarcassWhole);
        Add(rsSpecimenCarcassPartial);
        Add(rsSpecimenNest);
        Add(rsSpecimenBones);
        Add(rsSpecimenEgg);
        Add(rsSpecimenParasites);
        Add(rsSpecimenFeathers);
        Add(rsSpecimenBlood);
        Add(rsSpecimenClaw);
        Add(rsSpecimenSwab);
        Add(rsSpecimenTissues);
        Add(rsSpecimenFeces);
        Add(rsSpecimenRegurgite);
      end;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EGG_NAME).Visible then
      ColumnByFieldName(COL_EGG_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSpecimensSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if (Column.FieldName = COL_COLLECTION_DATE) or
    (Column.FieldName = COL_FIELD_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

{ TSpecimenCollectorsSubmoduleController }

constructor TSpecimenCollectorsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSpecimenCollectors;
  FCaptionText := rsTitleSpecimenCollectors;
  FDataSet := DMG.qSampleCollectors;
  FGrid := FOwner.gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_COLLECTOR_SEQUENCE, sdAscending);
end;

procedure TSpecimenCollectorsSubmoduleController.ConfigureColumns;
begin

end;

procedure TSpecimenCollectorsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin

end;

{ TSamplePrepsSubmoduleController }

constructor TSamplePrepsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSamplePreps;
  FCaptionText := rsTitleSamplePreps;
  FDataSet := DMG.qSamplePreps;
  FGrid := FOwner.gridChild2;
  FPageIndex := 1;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TSamplePrepsSubmoduleController.ConfigureColumns;
begin

end;

procedure TSamplePrepsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin

end;

end.

