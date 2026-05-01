{ Xolmis Sampling Data Modules controllers

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

unit modules_sampling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, DateUtils,
  data_types, modules_core;

type

  { TExpeditionsModuleController }

  TExpeditionsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSurveysModuleController }

  TSurveysModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSurveysSubmoduleController }

  TSurveysSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TSurveyMembersSubmoduleController }

  TSurveyMembersSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TNetsEffortSubmoduleController }

  TNetsEffortSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TWeatherLogsSubmoduleController }

  TWeatherLogsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TVegetationsSubmoduleController }

  TVegetationsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, utils_validations,
  data_consts, data_columns, data_filters, models_media,
  modules_birds, modules_sightings,
  uDarkStyleParams,
  udm_main, udm_grid, udm_sampling, ufrm_customgrid;

{ TExpeditionsModuleController }

constructor TExpeditionsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbExpeditions;
  FCaptionText := rsCaptionExpeditions;
  FDataSet := DMG.qExpeditions;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowDocs];
  FPrintUiFlags := [pufExpeditions, pufExpeditionsByProject];
  FFilterUiFlags := [fufMarked, fufDates, fufProject];

  AddDefaultSort(COL_START_DATE, sdDescending);

  FSubmodules.Add(TSurveysSubmoduleController.Create(FOwner));
end;

procedure TExpeditionsModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Project
    AddLookupFilter(SearchConfig, [COL_PROJECT_ID], [rscProject], ProjectIdFilter);
  end;
end;

procedure TExpeditionsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
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

procedure TExpeditionsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_START_DATE) or
    (Column.FieldName = COL_END_DATE) then
  begin
    if (Column.Field.AsDateTime > Today) then
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
  end;
end;

function TExpeditionsModuleController.Search(AValue: String): Boolean;
var
  i, g, m, y: Integer;
  dt: TDateTime;
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
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_EXPEDITION_ID, rscId, sdtInteger, crEqual,
          True, aValue));
        if IsLikelyYear(i) then
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscDate, sdtYear, crIntersect,
            False, aValue, '', '', COL_END_DATE));
      end
      else
      // Date
      if TryParseDateFlexible(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscDate, sdtDate, crIntersect,
          True, aValue, '', '', COL_END_DATE));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscDate, sdtMonthYear, crIntersect,
          True, aValue, '', '', COL_END_DATE));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_EXPEDITION_NAME, rscName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DESCRIPTION, rscDescription, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TSurveysModuleController }

constructor TSurveysModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSurveys;
  FCaptionText := rsTitleSurveys;
  FDataSet := DMG.qSurveys;
  FSupportedMedia := [amtImages, amtVideos, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages,
    gufShowVideos, gufShowDocs];
  FPrintUiFlags := [pufSurveys, pufSurveysByExpedition, pufSurveysByLocality, pufSurveysByProject];
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
  with TfrmCustomGrid(FOwner) do
  begin
    // Sites
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'gl.');
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Time interval
    AddTimeIntervalFilter(SearchConfig, COL_START_TIME, COL_END_TIME, eStartTimeFilter.Text, eEndTimeFilter.Text);
    // Method
    AddLookupFilter(SearchConfig, [COL_METHOD_ID], [rscMethod], MethodIdFilter);
    // Project
    AddLookupFilter(SearchConfig, [COL_PROJECT_ID], [rscProject], ProjectIdFilter);
    // Sampling plot
    AddLookupFilter(SearchConfig, [COL_SAMPLING_PLOT_ID], [rscSamplingPlot], SamplingPlotIdFilter);
    // Expedition
    AddLookupFilter(SearchConfig, [COL_EXPEDITION_ID], [rscExpedition], ExpeditionIdFilter);
  end;
end;

procedure TSurveysModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
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

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
  end;
end;

procedure TSurveysModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_SURVEY_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (Column.Field.AsDateTime > Today) then
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
  end;
end;

function TSurveysModuleController.Search(AValue: String): Boolean;
var
  i, g, m, y: Longint;
  Dt, Dt1, Dt2, Tm1, Tm2: TDateTime;
  Crit: TCriteriaType;
  y1, y2, M1, M2: Integer;
  V1, V2: String;
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
      // Year interval
      if TryParseYearInterval(aValue, y1, y2) then
      begin
        V1 := IntToStr(y1);
        V2 := IntToStr(y2);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtYear, crBetween,
          True, V1, V2));
      end
      else
      // Date interval
      if TryParseDateIntervalFlexible(aValue, Dt1, Dt2) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtDate, crBetween,
            True, FormatDateTime('yyyy-mm-dd', Dt1), FormatDateTime('yyyy-mm-dd', Dt2))
        );
      end
      else
      // Time interval
      if TryParseTimeIntervalFlexible(aValue, Tm1, Tm2) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_START_TIME, rscTime, sdtTime, crIntersect,
            True, FormatDateTime('hh:nn:ss', Tm1), FormatDateTime('hh:nn:ss', Tm2), '', COL_END_TIME)
        );
      end
      else
      // Month/year interval
      if TryParseMonthYearInterval(aValue, Y1, M1, Y2, M2) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtMonthYear, crBetween,
            True,
            Format('%.4d-%.2d', [Y1, M1]),
            Format('%.4d-%.2d', [Y2, M2]))
        );
      end
      else
      // ID and year
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SURVEY_ID, rscId, sdtInteger, crEqual,
          True, aValue));
        if IsLikelyYear(i) then
          SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtYear, crEqual,
            True, aValue));
      end
      else
      // Date
      if TryParseDateFlexible(aValue, Dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', Dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtDate, crEqual,
          True, aValue));
      end
      else
      // Time
      if TryParseTimeFlexible(aValue, Dt) then
      begin
        aValue := FormatDateTime('hh:nn:ss', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_TIME, rscTime, sdtTime, crIntersect,
          True, aValue, '', '', COL_END_TIME));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SURVEY_DATE, rscDate, sdtMonthYear, crEqual,
          True, aValue));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TSurveysSubmoduleController }

constructor TSurveysSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSurveys;
  FCaptionText := rsTitleSurveys;
  FDataSet := DMS.qSurveys;
  FGrid := TfrmCustomGrid(FOwner).gridChild1;
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

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
  end;
end;

procedure TSurveysSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_SURVEY_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (Column.Field.AsDateTime > Today) then
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
  end;
end;

{ TSurveyMembersSubmoduleController }

constructor TSurveyMembersSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbSurveyTeams;
  FCaptionText := rsCaptionTeam;
  FDataSet := DMS.qSurveyTeam;
  FGrid := TfrmCustomGrid(FOwner).gridChild1;
  FPageIndex := 0;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_PERSON_NAME, sdAscending);
end;

procedure TSurveyMembersSubmoduleController.ConfigureColumns;
begin

end;

procedure TSurveyMembersSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TNetsEffortSubmoduleController }

constructor TNetsEffortSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbNetsEffort;
  FCaptionText := rsTitleNetsEffort;
  FDataSet := DMS.qNetsEffort;
  FGrid := TfrmCustomGrid(FOwner).gridChild2;
  FPageIndex := 1;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TNetsEffortSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_NET_ID).Visible then
      ColumnByFieldName(COL_NET_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
  end;
end;

procedure TNetsEffortSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_SAMPLE_DATE) or
    (Column.FieldName = COL_NET_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

{ TWeatherLogsSubmoduleController }

constructor TWeatherLogsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbWeatherLogs;
  FCaptionText := rsTitleWeather;
  FDataSet := DMS.qWeatherLogs;
  FGrid := TfrmCustomGrid(FOwner).gridChild3;
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

procedure TWeatherLogsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TVegetationsSubmoduleController }

constructor TVegetationsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbVegetation;
  FCaptionText := rsCaptionVegetation;
  FDataSet := DMS.qVegetation;
  FGrid := TfrmCustomGrid(FOwner).gridChild6;
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

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
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

procedure TVegetationsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

end.

