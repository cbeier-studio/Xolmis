unit modules_sampling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TExpeditionsModuleController }

  TExpeditionsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSurveysModuleController }

  TSurveysModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSurveysSubmoduleController }

  TSurveysSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSurveyMembersSubmoduleController }

  TSurveyMembersSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TNetsEffortSubmoduleController }

  TNetsEffortSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TWeatherLogsSubmoduleController }

  TWeatherLogsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TVegetationsSubmoduleController }

  TVegetationsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, data_consts, data_columns, data_filters, models_media,
  modules_birds, modules_sightings,
  uDarkStyleParams,
  udm_main, udm_grid, udm_sampling;

{ TExpeditionsModuleController }

constructor TExpeditionsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbExpeditions;
  FCaptionText := rsCaptionExpeditions;
  FDataSet := DMG.qExpeditions;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowDocs, gufPrintMain, gufPrintByProject];
  FFilterUiFlags := [fufMarked, fufDates, fufProject];

  AddDefaultSort(COL_START_DATE, sdDescending);

  FSubmodules.Add(TSurveysSubmoduleController.Create(FOwner));
end;

procedure TExpeditionsModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with FOwner do
  begin
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if ProjectIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, rscProject, sdtInteger,
        crEqual, False, IntToStr(ProjectIdFilter)));
    end;
  end;
end;

procedure TExpeditionsModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eProjectFilter.Clear;
    ProjectIdFilter := 0;
  end;
end;

procedure TExpeditionsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_EXPEDITION_ID).Visible then
      ColumnByFieldname(COL_EXPEDITION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldname(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_DATE).Visible then
      ColumnByFieldName(COL_START_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_DATE).Visible then
      ColumnByFieldName(COL_END_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TExpeditionsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_START_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TExpeditionsModuleController.Search(AValue: String): Boolean;
var
  i, g: Integer;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EXPEDITION_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscStartDate, sdtDate, crEqual,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_END_DATE, rscEndDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscStartDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_END_DATE, rscEndDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EXPEDITION_NAME, rscName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DESCRIPTION, rscDescription, sdtText, Crit,
        False, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TSurveysModuleController }

constructor TSurveysModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSurveys;
  FCaptionText := rsTitleSurveys;
  FDataSet := DMG.qSurveys;
  FSupportedMedia := [amtImages, amtVideos, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages,
    gufShowVideos, gufShowDocs,
    gufPrintMain, gufPrintByLocality, gufPrintByProject, gufPrintByExpedition];
  FFilterUiFlags := [fufMarked, fufMethod, fufDates, fufSites, fufTimeInterval, fufSamplingPlot,
    fufExpedition, fufProject];

  AddDefaultSort(COL_SURVEY_DATE, sdDescending);

  FSubmodules.Add(TSurveyMembersSubmoduleController.Create(FOwner));
  FSubmodules.Add(TNetsEffortSubmoduleController.Create(FOwner));
  FSubmodules.Add(TWeatherLogsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TCapturesSubmoduleController.Create(FOwner));
  FSubmodules.Add(TSightingsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TVegetationsSubmoduleController.Create(FOwner));
end;

procedure TSurveysModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with FOwner do
  begin
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'gl.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if eStartTimeFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      if eEndTimeFilter.Text <> EmptyStr then
      begin
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_START_TIME, rscStartTime, sdtTime,
          crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)));
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_END_TIME, rscEndTime, sdtTime,
          crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)));
      end
      else
      begin
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_START_TIME, rscStartTime, sdtTime,
          crEqual, False, QuotedStr(eStartTimeFilter.Text)));
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_END_TIME, rscEndTime, sdtTime,
          crEqual, False, QuotedStr(eStartTimeFilter.Text)));
      end;
    end;

    if MethodIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_METHOD_ID, rscMethod, sdtInteger,
        crEqual, False, IntToStr(MethodIdFilter)));
    end;

    if ProjectIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PROJECT_ID, rscProject, sdtInteger,
        crEqual, False, IntToStr(ProjectIdFilter)));
    end;

    if SamplingPlotIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLING_PLOT_ID, rscSamplingPlot, sdtInteger,
        crEqual, False, IntToStr(SamplingPlotIdFilter)));
    end;

    if ExpeditionIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EXPEDITION_ID, rscExpedition, sdtInteger,
        crEqual, False, IntToStr(ExpeditionIdFilter)));
    end;
  end;
end;

procedure TSurveysModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    eMethodFilter.Clear;
    MethodIdFilter := 0;
    eProjectFilter.Clear;
    ProjectIdFilter := 0;
    eSamplingPlotFilter.Clear;
    SamplingPlotIdFilter := 0;
    eExpeditionFilter.Clear;
    ExpeditionIdFilter := 0;
  end;
end;

procedure TSurveysModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SURVEY_ID).Visible then
      ColumnByFieldname(COL_SURVEY_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_DATE).Visible then
      ColumnByFieldName(COL_SURVEY_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_METHOD_NAME).Visible then
      ColumnByFieldName(COL_METHOD_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EXPEDITION_NAME).Visible then
      ColumnByFieldname(COL_EXPEDITION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Visible then
      ColumnByFieldname(COL_NET_STATION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_LONGITUDE).Visible then
      ColumnByFieldname(COL_START_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_LATITUDE).Visible then
      ColumnByFieldname(COL_START_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_LONGITUDE).Visible then
      ColumnByFieldname(COL_END_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_LATITUDE).Visible then
      ColumnByFieldname(COL_END_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldname(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSurveysModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_SURVEY_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TSurveysModuleController.Search(AValue: String): Boolean;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SURVEY_ID, rscId, sdtInteger, crEqual,
        False, aValue));
      // if i > 999 then
      // Add('or (strftime(''%Y'',AMO_DATA) = '+QuotedStr(aValor)+'))');
    end
    else
    if TryStrToDate(aValue, Dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', Dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, Dt) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_START_TIME, rscStartTime, sdtTime, crEqual,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_END_TIME, rscEndTime, sdtTime, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TSurveysSubmoduleController }

constructor TSurveysSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSurveys;
  FCaptionText := rsTitleSurveys;
  FDataSet := DMS.qSurveys;
  FGrid := FOwner.gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_SURVEY_DATE, sdDescending);
end;

procedure TSurveysSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SURVEY_ID).Visible then
      ColumnByFieldname(COL_SURVEY_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_DATE).Visible then
      ColumnByFieldName(COL_SURVEY_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_METHOD_NAME).Visible then
      ColumnByFieldName(COL_METHOD_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EXPEDITION_NAME).Visible then
      ColumnByFieldname(COL_EXPEDITION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Visible then
      ColumnByFieldname(COL_NET_STATION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_LONGITUDE).Visible then
      ColumnByFieldname(COL_START_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_START_LATITUDE).Visible then
      ColumnByFieldname(COL_START_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_LONGITUDE).Visible then
      ColumnByFieldname(COL_END_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_LATITUDE).Visible then
      ColumnByFieldname(COL_END_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldname(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSurveysSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_SURVEY_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

{ TSurveyMembersSubmoduleController }

constructor TSurveyMembersSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSurveyTeams;
  FCaptionText := rsTitleSurveyTeam;
  FDataSet := DMS.qSurveyTeam;
  FGrid := FOwner.gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_PERSON_NAME, sdAscending);
end;

procedure TSurveyMembersSubmoduleController.ConfigureColumns;
begin

end;

procedure TSurveyMembersSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin

end;

{ TNetsEffortSubmoduleController }

constructor TNetsEffortSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbNetsEffort;
  FCaptionText := rsTitleNetsEffort;
  FDataSet := DMS.qNetsEffort;
  FGrid := FOwner.gridChild2;
  FPageIndex := 1;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TNetsEffortSubmoduleController.ConfigureColumns;
begin

end;

procedure TNetsEffortSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if (Column.FieldName = COL_SAMPLE_DATE) or
    (Column.FieldName = COL_NET_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

{ TWeatherLogsSubmoduleController }

constructor TWeatherLogsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbWeatherLogs;
  FCaptionText := rsTitleWeather;
  FDataSet := DMS.qWeatherLogs;
  FGrid := FOwner.gridChild3;
  FPageIndex := 2;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_SAMPLE_DATE, sdAscending);
  AddDefaultSort(COL_SAMPLE_TIME, sdAscending);
end;

procedure TWeatherLogsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_WEATHER_ID).Visible then
      ColumnByFieldname(COL_WEATHER_ID).ReadOnly:= True;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_MOMENT).Visible then
    begin
      ColumnByFieldname(COL_SAMPLE_MOMENT).PickList.Add(rsMomentStart);
      ColumnByFieldname(COL_SAMPLE_MOMENT).PickList.Add(rsMomentMiddle);
      ColumnByFieldname(COL_SAMPLE_MOMENT).PickList.Add(rsMomentEnd);
    end;

    if DataSource.DataSet.FieldByName(COL_PRECIPITATION).Visible then
    begin
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationNone);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationFog);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationMist);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationDrizzle);
      ColumnByFieldname(COL_PRECIPITATION).PickList.Add(rsPrecipitationRain);
    end;
  end;
end;

procedure TWeatherLogsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin

end;

{ TVegetationsSubmoduleController }

constructor TVegetationsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbVegetation;
  FCaptionText := rsTitleVegetation;
  FDataSet := DMS.qVegetation;
  FGrid := FOwner.gridChild6;
  FPageIndex := 5;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_SAMPLE_DATE, sdAscending);
  AddDefaultSort(COL_SAMPLE_TIME, sdAscending);
end;

procedure TVegetationsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_VEGETATION_ID).Visible then
      ColumnByFieldname(COL_VEGETATION_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_HERBS_DISTRIBUTION).Visible then
      with ColumnByFieldname(COL_HERBS_DISTRIBUTION).PickList do
      begin
        Add(rsDistributionNone);
        Add(rsDistributionRare);
        Add(rsDistributionFewSparse);
        Add(rsDistributionOnePatch);
        Add(rsDistributionOnePatchFewSparse);
        Add(rsDistributionManySparse);
        Add(rsDistributionOnePatchManySparse);
        Add(rsDistributionFewPatches);
        Add(rsDistributionFewPatchesSparse);
        Add(rsDistributionManyPatches);
        Add(rsDistributionManyPatchesSparse);
        Add(rsDistributionEvenHighDensity);
        Add(rsDistributionContinuousFewGaps);
        Add(rsDistributionContinuousDense);
        Add(rsDistributionContinuousDenseEdge);
      end;
    if DataSource.DataSet.FieldByName(COL_SHRUBS_DISTRIBUTION).Visible then
      with ColumnByFieldname(COL_SHRUBS_DISTRIBUTION).PickList do
      begin
        Add(rsDistributionNone);
        Add(rsDistributionRare);
        Add(rsDistributionFewSparse);
        Add(rsDistributionOnePatch);
        Add(rsDistributionOnePatchFewSparse);
        Add(rsDistributionManySparse);
        Add(rsDistributionOnePatchManySparse);
        Add(rsDistributionFewPatches);
        Add(rsDistributionFewPatchesSparse);
        Add(rsDistributionManyPatches);
        Add(rsDistributionManyPatchesSparse);
        Add(rsDistributionEvenHighDensity);
        Add(rsDistributionContinuousFewGaps);
        Add(rsDistributionContinuousDense);
        Add(rsDistributionContinuousDenseEdge);
      end;
    if DataSource.DataSet.FieldByName(COL_TREES_DISTRIBUTION).Visible then
      with ColumnByFieldname(COL_TREES_DISTRIBUTION).PickList do
      begin
        Add(rsDistributionNone);
        Add(rsDistributionRare);
        Add(rsDistributionFewSparse);
        Add(rsDistributionOnePatch);
        Add(rsDistributionOnePatchFewSparse);
        Add(rsDistributionManySparse);
        Add(rsDistributionOnePatchManySparse);
        Add(rsDistributionFewPatches);
        Add(rsDistributionFewPatchesSparse);
        Add(rsDistributionManyPatches);
        Add(rsDistributionManyPatchesSparse);
        Add(rsDistributionEvenHighDensity);
        Add(rsDistributionContinuousFewGaps);
        Add(rsDistributionContinuousDense);
        Add(rsDistributionContinuousDenseEdge);
      end;
  end;
end;

procedure TVegetationsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin

end;

end.

