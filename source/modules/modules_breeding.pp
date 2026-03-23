unit modules_breeding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TNestsModuleController }

  TNestsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TNestsSubmoduleController }

  TNestsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TNestRevisionsModuleController }

  TNestRevisionsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TNestRevisionsSubmoduleController }

  TNestRevisionsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TEggsModuleController }

  TEggsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TEggsSubmoduleController }

  TEggsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TNestOwnersSubmoduleController }

  TNestOwnersSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, data_consts, data_columns, data_filters, models_media,
  uDarkStyleParams,
  udm_main, udm_grid, udm_breeding, udm_individuals;

{ TNestsModuleController }

constructor TNestsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbNests;
  FCaptionText := rsTitleNests;
  FDataSet := DMG.qNests;
  FSupportedMedia := [amtImages, amtVideos, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages, gufShowVideos, gufShowDocs,
    gufPrintMain, gufPrintByDate, gufPrintByProject, gufPrintByLocality, gufPrintByTaxon];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufNestFate, fufSupportPlant, fufSupportType,
    fufPerson, fufProject];

  AddDefaultSort(COL_FULL_NAME, sdAscending);

  FSubmodules.Add(TNestOwnersSubmoduleController.Create(FOwner));
  FSubmodules.Add(TNestRevisionsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TEggsSubmoduleController.Create(FOwner));
end;

procedure TNestsModuleController.ApplyFilters;
const
  NestFate: array of String = ('P', 'S', 'U');
  NestSupport: array of String = ('G', 'P', 'H', 'F', 'S', 'C', 'A', 'O');
var
  sf: Integer;
begin
  with FOwner do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if cbNestFateFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_FATE, rscNestFate, sdtText,
        crEqual, False, NestFate[cbNestFateFilter.ItemIndex - 1]));
    end;
    if cbNestSupportFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPORT_TYPE, rscSupportType, sdtText,
        crEqual, False, NestSupport[cbNestSupportFilter.ItemIndex - 1]));
    end;

    if ePersonFilter.Text <> EmptyStr then
      PersonFilterToSearch(FTableType, SearchConfig.QuickFilters, PersonIdFilter);

    if ProjectIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, rscProject, sdtInteger,
        crEqual, False, IntToStr(ProjectIdFilter)));
    end;

    if SupportPlantIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_1_ID, rscSupportPlant1, sdtInteger,
        crEqual, False, IntToStr(SupportPlantIdFilter)));
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_2_ID, rscSupportPlant2, sdtInteger,
        crEqual, False, IntToStr(SupportPlantIdFilter)));
    end;
  end;
end;

procedure TNestsModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbNestFateFilter.ItemIndex := 0;

    cbNestSupportFilter.ItemIndex := 0;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eProjectFilter.Clear;
    ProjectIdFilter := 0;
    ePlantFilter.Clear;
    SupportPlantIdFilter := 0;
  end;
end;

procedure TNestsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NEST_ID).Visible then
      ColumnByFieldname(COL_NEST_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_FOUND_DATE).Visible then
      ColumnByFieldName(COL_FOUND_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LAST_DATE).Visible then
      ColumnByFieldName(COL_LAST_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_NAME).Visible then
      ColumnByFieldName(COL_SUPPORT_PLANT_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_NAME).Visible then
      ColumnByFieldName(COL_SUPPORT_PLANT_2_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_NEST_SHAPE).Visible then
    begin
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeScrape);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeCup);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePlate);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeSphere);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePendent);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePlatform);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeMound);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeBurrow);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeCavity);
    end;

    if DataSource.DataSet.FieldByName(COL_SUPPORT_TYPE).Visible then
    begin
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportGround);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportHerbBush);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportBranchFork);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportLeaves);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportLedge);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportRockCliff);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportRavine);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportNestBox);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportAnthropic);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportOther);
    end;
  end;
end;

procedure TNestsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if (Column.FieldName = COL_TAXON_NAME) or
    (Column.FieldName = COL_SUPPORT_PLANT_1_NAME) or
    (Column.FieldName = COL_SUPPORT_PLANT_2_NAME) then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_NEST_FATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    case Column.Field.AsString of
      'U':       // Unknown
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
        end;
      end;
      'L':       // Lost
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'S':       // Success
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
    end;
  end;
end;

function TNestsModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_NEST_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FOUND_DATE, rscFoundDate, sdtDate, crEqual,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LAST_DATE, rscLastDateActive, sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FOUND_DATE, rscFoundDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LAST_DATE, rscLastDateActive, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FIELD_NUMBER, rscFieldNumber, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
        True, aValue));
      { #todo : Check field name }
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create('z.full_name', rscTaxon, sdtText, Crit,
        True, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_OBSERVER_NAME, rscObserver, sdtText, Crit,
        True, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_1_NAME, rscSupportPlant1, sdtText, Crit,
        True, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_2_NAME, rscSupportPlant2, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TNestsSubmoduleController }

constructor TNestsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbNests;
  FCaptionText := rsTitleNests;
  FDataSet := DMI.qNests;
  FGrid := FOwner.gridChild4;
  FPageIndex := 3;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TNestsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NEST_ID).Visible then
      ColumnByFieldname(COL_NEST_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_FOUND_DATE).Visible then
      ColumnByFieldName(COL_FOUND_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LAST_DATE).Visible then
      ColumnByFieldName(COL_LAST_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_1_NAME).Visible then
      ColumnByFieldName(COL_SUPPORT_PLANT_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SUPPORT_PLANT_2_NAME).Visible then
      ColumnByFieldName(COL_SUPPORT_PLANT_2_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_NEST_SHAPE).Visible then
    begin
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeScrape);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeCup);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePlate);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeSphere);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePendent);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapePlatform);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeMound);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeBurrow);
      ColumnByFieldname(COL_NEST_SHAPE).PickList.Add(rsNestShapeCavity);
    end;

    if DataSource.DataSet.FieldByName(COL_SUPPORT_TYPE).Visible then
    begin
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportGround);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportHerbBush);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportBranchFork);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportLeaves);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportLedge);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportRockCliff);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportRavine);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportNestBox);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportAnthropic);
      ColumnByFieldname(COL_SUPPORT_TYPE).PickList.Add(rsSupportOther);
    end;
  end;
end;

procedure TNestsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if (Column.FieldName = COL_TAXON_NAME) or
    (Column.FieldName = COL_SUPPORT_PLANT_1_NAME) or
    (Column.FieldName = COL_SUPPORT_PLANT_2_NAME) then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_NEST_FATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    case Column.Field.AsString of
      'U':       // Unknown
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
        end;
      end;
      'L':       // Lost
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'S':       // Success
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
    end;
  end;
end;

{ TNestRevisionsModuleController }

constructor TNestRevisionsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbNestRevisions;
  FCaptionText := rsTitleNestRevisions;
  FDataSet := DMG.qNestRevisions;
  FSupportedMedia := [amtImages, amtVideos];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowImages, gufShowVideos, gufPrintByNest];
  FFilterUiFlags := [fufMarked, fufDates, fufNestStatus, fufNestStage, fufTimeInterval, fufNest, fufPerson,
    fufNidoparasitePresence, fufPhilornisPresence];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TNestRevisionsModuleController.ApplyFilters;
const
  NestStatus: array of String = ('A', 'I', 'U');
  NestStages: array of String = ('C', 'L', 'I', 'H', 'N', 'X', 'U');
var
  sf: Integer;
begin
  with FOwner do
  begin
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if cbNestStatusFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_STATUS, rscStatus, sdtText,
        crEqual, False, NestStatus[cbNestStatusFilter.ItemIndex - 1]));
    end;
    if cbNestStageFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_STAGE, rscNestStage, sdtText,
        crEqual, False, NestStages[cbNestStageFilter.ItemIndex - 1]));
    end;

    if ePersonFilter.Text <> EmptyStr then
      PersonFilterToSearch(FTableType, SearchConfig.QuickFilters, PersonIdFilter);

    if eStartTimeFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      if eEndTimeFilter.Text <> EmptyStr then
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REVISION_TIME, rscTime, sdtTime,
          crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
      else
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REVISION_TIME, rscTime, sdtTime,
          crEqual, False, QuotedStr(eStartTimeFilter.Text)));
    end;

    if NestIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, rscNest, sdtInteger,
        crEqual, False, IntToStr(NestIdFilter)));
    end;

    if rbNidoparasiteYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_ID, rscNidoparasite, sdtInteger,
        crMoreThan, False, '1'));
    end;
    if rbNidoparasiteNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_ID, rscNidoparasite, sdtInteger,
        crEqual, False, '0'));
    end;

    if rbPhilornisYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HAVE_PHILORNIS_LARVAE, rscHasPhilornisLarvae, sdtBoolean,
        crEqual, False, '1'));
    end;
    if rbPhilornisNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HAVE_PHILORNIS_LARVAE, rscHasPhilornisLarvae, sdtBoolean,
        crEqual, False, '0'));
    end;
  end;
end;

procedure TNestRevisionsModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbNestStatusFilter.ItemIndex := 0;
    cbNestStageFilter.ItemIndex := 0;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eNestFilter.Clear;
    NestIdFilter := 0;

    rbNidoparasiteAll.Checked := True;
    rbPhilornisAll.Checked := True;
  end;
end;

procedure TNestRevisionsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NEST_REVISION_ID).Visible then
      ColumnByFieldname(COL_NEST_REVISION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_REVISION_DATE).Visible then
      ColumnByFieldName(COL_REVISION_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_1_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_2_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_2_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NIDOPARASITE_NAME).Visible then
      ColumnByFieldName(COL_NIDOPARASITE_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TNestRevisionsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_NIDOPARASITE_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_NEST_STATUS then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    case Column.Field.AsString of
      'U':       // Unknown
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
        end;
      end;
      'I':       // Inactive
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'A':       // Active
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
    end;
  end;
end;

function TNestRevisionsModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_NEST_REVISION_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_REVISION_DATE, rscDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_REVISION_DATE, rscDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_NAME, rscNidoparasite, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TNestRevisionsSubmoduleController }

constructor TNestRevisionsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbNestRevisions;
  FCaptionText := rsTitleNestRevisions;
  FDataSet := DMB.qNestRevisions;
  FGrid := FOwner.gridChild2;
  FPageIndex := 1;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TNestRevisionsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NEST_REVISION_ID).Visible then
      ColumnByFieldname(COL_NEST_REVISION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_REVISION_DATE).Visible then
      ColumnByFieldName(COL_REVISION_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_1_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_2_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_2_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NIDOPARASITE_NAME).Visible then
      ColumnByFieldName(COL_NIDOPARASITE_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TNestRevisionsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_NIDOPARASITE_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_NEST_STATUS then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    case Column.Field.AsString of
      'U':       // Unknown
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSolidNeutralBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSolidNeutralFGLight;
        end;
      end;
      'I':       // Inactive
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCriticalFGLight;
        end;
      end;
      'A':       // Active
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
        end;
      end;
    end;
  end;
end;

{ TEggsModuleController }

constructor TEggsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbEggs;
  FCaptionText := rsTitleEggs;
  FDataSet := DMG.qEggs;
  FSupportedMedia := [amtImages];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowImages, gufPrintMain, gufPrintByNest,
    gufPrintByLocality, gufPrintByTaxon];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufEggSpecs, fufPerson, fufNest, fufIndividual, fufEggHatched];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TEggsModuleController.ApplyFilters;
const
  EggShapes: array of String = ('S', 'E', 'O', 'P', 'C', 'B', 'Y', 'L', 'U');
  EggPatterns: array of String = ('P', 'B', 'S', 'T', 'W', 'PS', 'BS', 'U');
  EggTextures: array of String = ('C', 'S', 'G', 'P', 'U');
var
  sf: Integer;
begin
  with FOwner do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if ePersonFilter.Text <> EmptyStr then
      PersonFilterToSearch(FTableType, SearchConfig.QuickFilters, PersonIdFilter);

    if NestIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, rscNest, sdtInteger,
        crEqual, False, IntToStr(NestIdFilter)));
    end;

    if IndividualIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, rscIndividual, sdtInteger,
        crEqual, False, IntToStr(IndividualIdFilter)));
    end;

    if cbEggShapeFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_SHAPE, rscEggShape, sdtText,
        crEqual, False, EggShapes[cbEggShapeFilter.ItemIndex - 1]));
    end;
    if cbEggPatternFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGGSHELL_PATTERN, rscEggshellPattern, sdtText,
        crEqual, False, EggPatterns[cbEggPatternFilter.ItemIndex - 1]));
    end;
    if cbEggTextureFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGGSHELL_TEXTURE, rscEggshellTexture, sdtText,
        crEqual, False, EggTextures[cbEggTextureFilter.ItemIndex - 1]));
    end;

    if rbHatchedYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_HATCHED, rscHatched, sdtBoolean,
        crEqual, False, '1'));
    end;
    if rbHatchedNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EGG_HATCHED, rscHatched, sdtBoolean,
        crEqual, False, '0'));
    end;
  end;
end;

procedure TEggsModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbEggShapeFilter.ItemIndex := 0;
    cbEggPatternFilter.ItemIndex := 0;
    cbEggTextureFilter.ItemIndex := 0;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eNestFilter.Clear;
    NestIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;

    rbHatchedAll.Checked := True;
  end;
end;

procedure TEggsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_EGG_ID).Visible then
      ColumnByFieldname(COL_EGG_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MEASURE_DATE).Visible then
      ColumnByFieldName(COL_MEASURE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RESEARCHER_NAME).Visible then
      ColumnByFieldName(COL_RESEARCHER_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_EGG_SHAPE).Visible then
    begin
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggSpherical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggElliptical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggOval);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggPyriform);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggConical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggBiconical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggCylindrical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggLongitudinal);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggUnknown);
    end;

    if DataSource.DataSet.FieldByName(COL_EGGSHELL_TEXTURE).Visible then
    begin
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggChalky);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggShiny);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggGlossy);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggPitted);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggUnknown);
    end;

    if DataSource.DataSet.FieldByName(COL_EGGSHELL_PATTERN).Visible then
    begin
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSpots);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggBlotches);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggStreaks);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggScrawls);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSpotsSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggBlotchesSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggUnknown);
    end;
  end;
end;

procedure TEggsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_EGG_SEQUENCE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

function TEggsModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
  Dt: TDateTime;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EGG_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_MEASURE_DATE, rscDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_MEASURE_DATE, rscDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME,rscFullName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FIELD_NUMBER, rscFieldNumber, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TEggsSubmoduleController }

constructor TEggsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbEggs;
  FCaptionText := rsTitleEggs;
  FDataSet := DMB.qEggs;
  FGrid := FOwner.gridChild3;
  FPageIndex := 2;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TEggsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_EGG_ID).Visible then
      ColumnByFieldname(COL_EGG_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MEASURE_DATE).Visible then
      ColumnByFieldName(COL_MEASURE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RESEARCHER_NAME).Visible then
      ColumnByFieldName(COL_RESEARCHER_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_EGG_SHAPE).Visible then
    begin
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggSpherical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggElliptical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggOval);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggPyriform);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggConical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggBiconical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggCylindrical);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggLongitudinal);
      ColumnByFieldName(COL_EGG_SHAPE).PickList.Add(rsEggUnknown);
    end;

    if DataSource.DataSet.FieldByName(COL_EGGSHELL_TEXTURE).Visible then
    begin
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggChalky);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggShiny);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggGlossy);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggPitted);
      ColumnByFieldName(COL_EGGSHELL_TEXTURE).PickList.Add(rsEggUnknown);
    end;

    if DataSource.DataSet.FieldByName(COL_EGGSHELL_PATTERN).Visible then
    begin
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSpots);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggBlotches);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggStreaks);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggScrawls);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggSpotsSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggBlotchesSquiggles);
      ColumnByFieldName(COL_EGGSHELL_PATTERN).PickList.Add(rsEggUnknown);
    end;
  end;
end;

procedure TEggsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_EGG_SEQUENCE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

{ TNestOwnersSubmoduleController }

constructor TNestOwnersSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbNestOwners;
  FCaptionText := rsTitleNestOwners;
  FDataSet := DMB.qNestOwners;
  FGrid := FOwner.gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_ROLE, sdAscending);
end;

procedure TNestOwnersSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    //ColumnByFieldname('nest_owner_id').ReadOnly:= True;

    if DataSource.DataSet.FieldByName(COL_ROLE).Visible then
      ColumnByFieldName(COL_ROLE).PickList.CommaText := rsNestOwnersRoleList;

    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldName(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TNestOwnersSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin

end;

end.

