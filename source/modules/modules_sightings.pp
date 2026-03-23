unit modules_sightings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TSightingsModuleController }

  TSightingsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

  { TSightingsSubmoduleController }

  TSightingsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, udm_sampling, udm_individuals;

{ TSightingsModuleController }

constructor TSightingsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSightings;
  FCaptionText := rsTitleSightings;
  FDataSet := DMG.qSightings;
  FSupportedMedia := [amtImages, amtAudios, amtVideos, amtDocuments];
  FUiFlags := [gufShowImages, gufShowAudios, gufShowVideos, gufShowDocs, gufShowSummary, gufShowMap,
    gufShowVerifications, gufPrintMain, gufPrintByLocality, gufPrintByProject, gufPrintByTaxon,
    gufPrintByObserver, gufPrintBySurvey];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufTimeInterval, fufPerson, fufSurvey, fufMethod,
    fufIndividual, fufIsOnEbird, fufOutOfSample];

  AddDefaultSort(COL_SIGHTING_DATE, sdDescending);
end;

procedure TSightingsModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with FOwner do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if ePersonFilter.Text <> EmptyStr then
      PersonFilterToSearch(FTableType, SearchConfig.QuickFilters, PersonIdFilter);

    if eStartTimeFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      if eEndTimeFilter.Text <> EmptyStr then
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, rscTime, sdtTime,
          crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
      else
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, rscTime, sdtTime,
          crEqual, False, QuotedStr(eStartTimeFilter.Text)));
    end;

    if SurveyIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SURVEY_ID, rscSurvey, sdtInteger,
        crEqual, False, IntToStr(SurveyIdFilter)));
    end;
    if MethodIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_METHOD_ID, rscMethod, sdtInteger,
        crEqual, False, IntToStr(MethodIdFilter)));
    end;

    if IndividualIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, rscIndividual, sdtInteger,
        crEqual, False, IntToStr(IndividualIdFilter)));
    end;

    if rbRecordInEbirdYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EBIRD_AVAILABLE, rscIsInEBird, sdtBoolean,
        crEqual, False, '1'));
    end;
    if rbRecordInEbirdNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_EBIRD_AVAILABLE, rscIsInEBird, sdtBoolean,
        crEqual, False, '0'));
    end;

    if rbOutOfSampleYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NOT_SURVEYING, rscOutOfSample, sdtBoolean,
        crEqual, False, '1'));
    end;
    if rbOutOfSampleNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NOT_SURVEYING, rscOutOfSample, sdtBoolean,
        crEqual, False, '0'));
    end;
  end;
end;

procedure TSightingsModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eSurveyFilter.Clear;
    SurveyIdFilter := 0;
    eMethodFilter.Clear;
    MethodIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;

    rbRecordInEbirdAll.Checked := True;
    rbOutOfSampleAll.Checked := True;
  end;
end;

procedure TSightingsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SIGHTING_ID).Visible then
      ColumnByFieldname(COL_SIGHTING_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldname(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DETECTION_TYPE).Visible then
      ColumnByFieldname(COL_DETECTION_TYPE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BREEDING_STATUS).Visible then
      ColumnByFieldname(COL_BREEDING_STATUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_DATE).Visible then
      ColumnByFieldname(COL_SIGHTING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSightingsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_SIGHTING_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

function TSightingsModuleController.Search(AValue: String): Boolean;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_ID, rscSightingID, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if TryStrToTime(aValue, dt) then
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_TIME, rscTime, sdtTime, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SIGHTING_DATE, rscDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
        True, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
        True, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_METHOD_NAME, rscMethod, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

{ TSightingsSubmoduleController }

constructor TSightingsSubmoduleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbSightings;
  FCaptionText := rsTitleSightings;
  case FOwner.TableType of
    tbSurveys:
      begin
        FDataSet := DMS.qSightings;
        FGrid := FOwner.gridChild5;
        FPageIndex := 4;
      end;
    tbIndividuals:
      begin
        FDataSet := DMI.qSightings;
        FGrid := FOwner.gridChild3;
        FPageIndex := 2;
      end;
  end;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_SIGHTING_DATE, sdDescending);
end;

procedure TSightingsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_SIGHTING_ID).Visible then
      ColumnByFieldname(COL_SIGHTING_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SURVEY_NAME).Visible then
      ColumnByFieldname(COL_SURVEY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldname(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DETECTION_TYPE).Visible then
      ColumnByFieldname(COL_DETECTION_TYPE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BREEDING_STATUS).Visible then
      ColumnByFieldname(COL_BREEDING_STATUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_DATE).Visible then
      ColumnByFieldname(COL_SIGHTING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TSightingsSubmoduleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if Column.FieldName = COL_SIGHTING_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end;
end;

end.

