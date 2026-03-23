unit modules_birds;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TIndividualsModuleController }

  TIndividualsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TCapturesModuleController }

  TCapturesModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TCapturesSubmoduleController }

  TCapturesSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TFeathersModuleController }

  TFeathersModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TFeathersSubmoduleController }

  TFeathersSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_global, utils_themes, utils_math, utils_graphics,
  data_consts, data_columns, data_filters, data_getvalue,
  modules_sightings, modules_breeding, modules_specimens,
  models_media, models_record_types, uDarkStyleParams,
  udm_main, udm_grid, udm_sampling, udm_individuals, ufrm_customgrid;

{ TIndividualsModuleController }

constructor TIndividualsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbIndividuals;
  FCaptionText := rsTitleIndividuals;
  FDataSet := DMG.qIndividuals;
  FSupportedMedia := [amtImages, amtAudios, amtVideos, amtDocuments];
  FUiFlags := [gufShowImages, gufShowVideos, gufShowAudios, gufShowDocs, gufShowSummary, gufShowVerifications];
  FPrintUiFlags := [pufIndividuals, pufIndividualsByTaxon, pufIndividualsByParents];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufAge, fufSex, fufNest, fufIndividual, fufBandReplaced];

  AddDefaultSort(COL_FULL_NAME, sdAscending);

  FSubmodules.Add(TCapturesSubmoduleController.Create(FOwner));
  FSubmodules.Add(TFeathersSubmoduleController.Create(FOwner));
  FSubmodules.Add(TSightingsSubmoduleController.create(FOwner));
  FSubmodules.Add(TNestsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TSpecimensSubmoduleController.Create(FOwner));
end;

procedure TIndividualsModuleController.ApplyFilters;
const
  BirdAge: array of String = ('U', 'A', 'I', 'J', 'N', 'F', 'S', 'T', '4', '5');
  BirdSex: array of String = ('M', 'F', 'U');
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if cbSexFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_SEX, rscSex, sdtText,
        crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
    end;
    if cbAgeFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_AGE, rscAge, sdtText,
        crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
    end;

    if rbWithColorBandsYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_RIGHT_TARSUS, rscRightTarsus, sdtText,
        crNotEqual, False, ''));
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_LEFT_TARSUS, rscLeftTarsus, sdtText,
        crNotEqual, False, ''));
    end;
    if rbWithColorBandsNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_RIGHT_TARSUS, rscRightTarsus, sdtText,
        crEqual, False, ''));
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_LEFT_TARSUS, rscLeftTarsus, sdtText,
        crEqual, False, ''));
    end;

    if rbWithRecapturesYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURES_TALLY, rscCaptures, sdtInteger,
        crMoreThan, True, '2'));
    end;
    if rbWithRecapturesNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURES_TALLY, rscCaptures, sdtInteger,
        crLessThan, True, '1'));
    end;

    if NestIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEST_ID, rscNest, sdtInteger,
        crEqual, False, IntToStr(NestIdFilter)));
    end;

    if IndividualIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FATHER_ID, rscFather, sdtInteger,
        crEqual, False, IntToStr(IndividualIdFilter)));
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_MOTHER_ID, rscMother, sdtInteger,
        crEqual, False, IntToStr(IndividualIdFilter)));
    end;

    if rbReplacedBandYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, rscRemovedBand, sdtInteger,
        crMoreThan, False, '1'));
    end;
    if rbReplacedBandNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, rscRemovedBand, sdtInteger,
        crEqual, False, '0'));
    end;
  end;
end;

procedure TIndividualsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbSexFilter.ItemIndex := 0;
    cbCloacalProtuberanceFilter.ItemIndex := 0;
    cbBroodPatchFilter.ItemIndex := 0;
    eHowSexedFilter.Clear;

    cbAgeFilter.ItemIndex := 0;
    cbSkullOssificationFilter.ItemIndex := 0;
    eHowAgedFilter.Clear;

    rbWithColorBandsAll.Checked := True;
    rbWithRecapturesAll.Checked := True;

    rbReplacedBandAll.Checked := True;

    eNestFilter.Clear;
    NestIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;
  end;
end;

procedure TIndividualsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_ID).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_NAME).Visible then
      ColumnByFieldName(COL_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DOUBLE_BAND_NAME).Visible then
      ColumnByFieldName(COL_DOUBLE_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Visible then
      ColumnByFieldName(COL_REMOVED_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BANDING_DATE).Visible then
      ColumnByFieldName(COL_BANDING_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_CHANGE_DATE).Visible then
      ColumnByFieldName(COL_BAND_CHANGE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NEST_NAME).Visible then
      ColumnByFieldName(COL_NEST_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_FATHER_NAME).Visible then
      ColumnByFieldName(COL_FATHER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MOTHER_NAME).Visible then
      ColumnByFieldName(COL_MOTHER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TARSUS).Visible then
      ColumnByFieldName(COL_RIGHT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TARSUS).Visible then
      ColumnByFieldName(COL_LEFT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TIBIA).Visible then
      ColumnByFieldName(COL_RIGHT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TIBIA).Visible then
      ColumnByFieldName(COL_LEFT_TIBIA).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_SEX).Visible then
    begin
      ColumnByFieldName(COL_INDIVIDUAL_SEX).PickList.Add(rsSexUnknown);
      ColumnByFieldName(COL_INDIVIDUAL_SEX).PickList.Add(rsSexMale);
      ColumnByFieldName(COL_INDIVIDUAL_SEX).PickList.Add(rsSexFemale);
    end;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_AGE).Visible then
    begin
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeUnknown);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeAdult);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeJuvenile);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFledgling);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeNestling);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFirstYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeSecondYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeThirdYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFourthYear);
      ColumnByFieldName(COL_INDIVIDUAL_AGE).PickList.Add(rsAgeFifthYear);
    end;
  end;
end;

procedure TIndividualsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if (Column.FieldName = COL_INDIVIDUAL_SEX) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    case Column.Field.AsString of
      'U':
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
      'M':
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
      'F':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
    end;
  end
  else
  if Column.FieldName = COL_BAND_NAME then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if Column.FieldName = COL_REMOVED_BAND_NAME then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TIndividualsModuleController.Search(AValue: String): Boolean;
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

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BANDING_DATE, rscBandingDate, sdtDate, crEqual,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_CHANGE_DATE, rscBandChangeDate, sdtDate, crEqual,
          False, aValue));
        { #todo : PartialDate fields: birth_date and death_date }
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BANDING_DATE, rscBandingDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_CHANGE_DATE, rscBandChangeDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_NAME, rscBand, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DOUBLE_BAND_NAME, rscDoubleBand, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_NAME, rscRemovedBand, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_NEST_NAME, rscNest, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TCapturesModuleController }

constructor TCapturesModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbCaptures;
  FCaptionText := rsTitleCaptures;
  FDataSet := DMG.qCaptures;
  FSupportedMedia := [amtImages, amtVideos, amtDocuments];
  FUiFlags := [gufShowImages, gufShowVideos, gufShowDocs, gufShowSummary, gufShowMap,
    gufShowVerifications];
  FPrintUiFlags := [pufCaptures, pufCapturesByDate, pufCapturesByLocality, pufCapturesByProject, pufCapturesByTaxon];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufCaptureType, fufReleaseStatus, fufAgingSpecs,
    fufSexingSpecs,fufFat, fufMoltSpecs, fufTimeInterval, fufPerson, fufSurvey, fufMethod, fufIndividual,
    fufSamplingPlot, fufNeedsReview, fufEscaped, fufPhilornisPresence, fufBandReplaced];

  AddDefaultSort(COL_CAPTURE_DATE, sdDescending);
end;

procedure TCapturesModuleController.ApplyFilters;
const
  BirdAge: array of String = ('U', 'A', 'I', 'J', 'N', 'F', 'S', 'T', '4', '5');
  BirdSex: array of String = ('M', 'F', 'U');
  CaptureType: array of String = ('N', 'R', 'S', 'C', 'U');
  CaptureStatus: array of String = ('N', 'I', 'W', 'X', 'D');
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters);
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters);
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if cbCaptureTypeFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURE_TYPE, rscType, sdtText,
        crEqual, False, CaptureType[cbCaptureTypeFilter.ItemIndex - 1]));
    end;

    if cbCaptureStatusFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUBJECT_STATUS, rscStatus, sdtText,
        crEqual, False, CaptureStatus[cbCaptureStatusFilter.ItemIndex - 1]));
    end;

    if cbAgeFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUBJECT_AGE, rscAge, sdtText,
        crEqual, False, BirdAge[cbAgeFilter.ItemIndex - 1]));
    end;
    if eHowAgedFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HOW_AGED, rscHowWasAged, sdtText,
        crLike, False, eHowAgedFilter.Text));
    end;
    if cbSexFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SUBJECT_SEX, rscSex, sdtText,
        crEqual, False, BirdSex[cbSexFilter.ItemIndex - 1]));
    end;
    if eHowSexedFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_HOW_SEXED, rscHowWasSexed, sdtText,
        crLike, False, eHowSexedFilter.Text));
    end;

    if cbCloacalProtuberanceFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CLOACAL_PROTUBERANCE, rscCloacalProtuberance, sdtText,
        crEqual, False, cbCloacalProtuberanceFilter.Text));
    end;
    if cbBroodPatchFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BROOD_PATCH, rscBroodPatch, sdtText,
        crEqual, False, cbBroodPatchFilter.Text));
    end;

    if cbFatFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FAT, rscFat, sdtText,
        crEqual, False, cbFatFilter.Text));
    end;

    if cbBodyMoltFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_BODY_MOLT, rscBodyMolt, sdtText,
        crEqual, False, cbBodyMoltFilter.Text));
    end;
    if cbFFMoltFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FLIGHT_FEATHERS_MOLT, rscFlightFeathersMolt, sdtText,
        crEqual, False, cbFFMoltFilter.Text));
    end;
    if cbFFWearFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_FLIGHT_FEATHERS_WEAR, rscFlightFeathersWear, sdtText,
        crEqual, False, cbFFWearFilter.Text));
    end;
    if eMoltLimitsFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_MOLT_LIMITS, rscMoltLimits, sdtText,
        crLike, False, eMoltLimitsFilter.Text));
    end;
    if eCycleCodeFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CYCLE_CODE, rscMoltCycle, sdtText,
        crLike, False, eCycleCodeFilter.Text));
    end;

    if cbSkullOssificationFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SKULL_OSSIFICATION, rscSkullOssification, sdtText,
        crEqual, False, cbSkullOssificationFilter.Text));
    end;

    if PersonIdFilter > 0 then
      PersonFilterToSearch(FTableType, SearchConfig.QuickFilters, PersonIdFilter);

    if eStartTimeFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      if eEndTimeFilter.Text <> EmptyStr then
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURE_TIME, rscTime, sdtTime,
          crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
      else
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_CAPTURE_TIME, rscTime, sdtTime,
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

    if rbNeedsReviewYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEEDS_REVIEW, rscNeedsReview, sdtBoolean,
        crEqual, False, '1'));
    end;
    if rbNeedsReviewNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_NEEDS_REVIEW, rscNeedsReview, sdtBoolean,
        crEqual, False, '0'));
    end;

    if rbEscapedYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_ESCAPED, rscEscaped, sdtBoolean,
        crEqual, False, '1'));
    end;
    if rbEscapedNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_ESCAPED, rscEscaped, sdtBoolean,
        crEqual, False, '0'));
    end;

    if rbPhilornisYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PHILORNIS_LARVAE_TALLY, rscQuantPhilornisLarvae, sdtInteger,
        crMoreThan, False, '1'));
    end;
    if rbPhilornisNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PHILORNIS_LARVAE_TALLY, rscQuantPhilornisLarvae, sdtInteger,
        crEqual, False, '0'));
    end;

    if rbReplacedBandYes.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, rscRemovedBand, sdtInteger,
        crMoreThan, False, '1'));
    end;
    if rbReplacedBandNo.Checked then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_ID, rscRemovedBand, sdtInteger,
        crEqual, False, '0'));
    end;
  end;
end;

procedure TCapturesModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    cbCaptureTypeFilter.ItemIndex := 0;
    cbCaptureStatusFilter.ItemIndex := 0;

    cbAgeFilter.ItemIndex := 0;
    cbSkullOssificationFilter.ItemIndex := 0;
    eHowAgedFilter.Clear;

    cbSexFilter.ItemIndex := 0;
    cbCloacalProtuberanceFilter.ItemIndex := 0;
    cbBroodPatchFilter.ItemIndex := 0;
    eHowSexedFilter.Clear;

    cbFatFilter.ItemIndex := 0;

    cbBodyMoltFilter.ItemIndex := 0;
    cbFFMoltFilter.ItemIndex := 0;
    cbFFWearFilter.ItemIndex := 0;
    eMoltLimitsFilter.Clear;
    eCycleCodeFilter.Clear;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eSurveyFilter.Clear;
    SurveyIdFilter := 0;
    eMethodFilter.Clear;
    MethodIdFilter := 0;
    eSamplingPlotFilter.Clear;
    SamplingPlotIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;

    rbNeedsReviewAll.Checked := True;
    rbEscapedAll.Checked := True;

    rbPhilornisAll.Checked := True;
    rbReplacedBandAll.Checked := True;
  end;
end;

procedure TCapturesModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_CAPTURE_ID).Visible then
      ColumnByFieldname(COL_CAPTURE_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_CAPTURE_DATE).Visible then
      ColumnByFieldName(COL_CAPTURE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CAPTURE_TYPE).Visible then
      ColumnByFieldName(COL_CAPTURE_TYPE).PickList.AddCommaText(rsCaptureTypeList);
    if DataSource.DataSet.FieldByName(COL_RIGHT_TARSUS).Visible then
      ColumnByFieldName(COL_RIGHT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TARSUS).Visible then
      ColumnByFieldName(COL_LEFT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TIBIA).Visible then
      ColumnByFieldName(COL_RIGHT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TIBIA).Visible then
      ColumnByFieldName(COL_LEFT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Visible then
      ColumnByFieldname(COL_NET_STATION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BANDER_NAME).Visible then
      ColumnByFieldname(COL_BANDER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_ANNOTATOR_NAME).Visible then
      ColumnByFieldname(COL_ANNOTATOR_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_NAME).Visible then
      ColumnByFieldname(COL_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Visible then
      ColumnByFieldname(COL_REMOVED_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MOLT_LIMITS).Visible then
      ColumnByFieldName(COL_MOLT_LIMITS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CYCLE_CODE).Visible then
      ColumnByFieldName(COL_CYCLE_CODE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_HOW_AGED).Visible then
      ColumnByFieldName(COL_HOW_AGED).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_HOW_SEXED).Visible then
      ColumnByFieldName(COL_HOW_SEXED).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_NAME).Visible then
      ColumnByFieldname(COL_PHOTOGRAPHER_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_NAME).Visible then
      ColumnByFieldname(COL_PHOTOGRAPHER_2_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_CLOACAL_PROTUBERANCE).Visible then
      ColumnByFieldName(COL_CLOACAL_PROTUBERANCE).PickList.AddCommaText('U,N,S,M,L');
    if DataSource.DataSet.FieldByName(COL_BROOD_PATCH).Visible then
      ColumnByFieldName(COL_BROOD_PATCH).PickList.AddCommaText('F,N,V,W,O');
    if DataSource.DataSet.FieldByName(COL_FAT).Visible then
      ColumnByFieldName(COL_FAT).PickList.AddCommaText('N,T,L,H,F,B,G,V');
    if DataSource.DataSet.FieldByName(COL_BODY_MOLT).Visible then
      ColumnByFieldName(COL_BODY_MOLT).PickList.AddCommaText('N,T,S,H,G,A,F');
    if DataSource.DataSet.FieldByName(COL_FLIGHT_FEATHERS_MOLT).Visible then
      ColumnByFieldName(COL_FLIGHT_FEATHERS_MOLT).PickList.AddCommaText('N,S,A');
    if DataSource.DataSet.FieldByName(COL_FLIGHT_FEATHERS_WEAR).Visible then
      ColumnByFieldName(COL_FLIGHT_FEATHERS_WEAR).PickList.AddCommaText('N,S,L,M,H,X');
    if DataSource.DataSet.FieldByName(COL_SKULL_OSSIFICATION).Visible then
      ColumnByFieldName(COL_SKULL_OSSIFICATION).PickList.AddCommaText('N,T,L,H,G,A,F');
    if DataSource.DataSet.FieldByName(COL_SUBJECT_AGE).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeUnknown);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeAdult);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeJuvenile);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFledgling);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeNestling);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFirstYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeSecondYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeThirdYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFourthYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFifthYear);
    end;
    if DataSource.DataSet.FieldByName(COL_SUBJECT_SEX).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexMale);
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexFemale);
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexUnknown);
    end;
    if DataSource.DataSet.FieldByName(COL_SUBJECT_STATUS).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusNormal);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusInjured);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusWingSprain);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusStressed);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusDead);
    end;
  end;
end;

procedure TCapturesModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
var
  aTaxon: Integer;
begin
  aTaxon := 0;

  if (Column.FieldName = COL_TAXON_NAME) then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if (Column.FieldName = COL_CAPTURE_TYPE) then
  begin
    case Column.Field.AsString of
      'N':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clBlueBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clBlueBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryLight;
        end;
      end;
      'R', 'S':
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
      'C':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
      'U':
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
    end;
  end
  else
  if (Column.FieldName = COL_BAND_NAME) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'R') or
      (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'S') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
    end
    else
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end
  else
  if (Column.FieldName = COL_REMOVED_BAND_NAME) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end;

  { Check if UseConditionalFormatting setting is enabled }
  if not xSettings.UseConditionalFormatting then
    Exit;

  { Paint the cell background red for invalid values }
  if (Column.FieldName = COL_CLOACAL_PROTUBERANCE) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, CLOACAL_PROTUBERANCE_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_BROOD_PATCH) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BROOD_PATCH_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FAT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FAT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_BODY_MOLT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BODY_MOLT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FLIGHT_FEATHERS_MOLT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FLIGHT_MOLT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FLIGHT_FEATHERS_WEAR) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FEATHER_WEAR_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_SKULL_OSSIFICATION) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, SKULL_OSSIFICATION_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end;

  { Check if ShowOutliersOnGrid setting is enabled }
  if not xSettings.ShowOutliersOnGrid then
    Exit;

  { Paint the cell background yellow for outliers }
  if (Column.FieldName = COL_RIGHT_WING_CHORD) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_FIRST_SECONDARY_CHORD) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TAIL_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TARSUS_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TARSUS_DIAMETER) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_WEIGHT) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_EXPOSED_CULMEN) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BILL_WIDTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BILL_HEIGHT) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_NOSTRIL_BILL_TIP) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_SKULL_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_KIPPS_INDEX) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end;
end;

function TCapturesModuleController.Search(AValue: String): Boolean;
var
  i, g: Longint;
  dt: TDateTime;
  Crit: TCriteriaType;
  V1, V2, m, y: String;
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
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_ID, rscId, sdtInteger, crEqual,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, rscDate, sdtYear, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, rscDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if TryStrToTime(aValue, dt) then
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_TIME, rscDate, sdtTime, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{4}[-‒]{1}\d{4}$', aValue) then
      begin
        Crit := crBetween;
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        { split strings: unicode characters #$002D e #$2012 }
        V1 := ExtractDelimited(1, aValue, ['-', #$2012]);
        V2 := ExtractDelimited(2, aValue, ['-', #$2012]);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, rscDate, sdtYear, Crit,
          False, V1, V2));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CAPTURE_DATE, rscDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BAND_NAME, rscBand, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_REMOVED_BAND_NAME, rscRemovedBand, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_NET_STATION_NAME, rscSamplingPlot, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BANDER_NAME, rscBander, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_ANNOTATOR_NAME, rscAnnotator, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TCapturesSubmoduleController }

constructor TCapturesSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbCaptures;
  FCaptionText := rsTitleCaptures;
  case TfrmCustomGrid(FOwner).TableType of
    tbSurveys:
      begin
        FDataSet := DMS.qCaptures;
        FGrid := TfrmCustomGrid(FOwner).gridChild4;
        FPageIndex := 3;
      end;
    tbIndividuals:
      begin
        FDataSet := DMI.qCaptures;
        FGrid := TfrmCustomGrid(FOwner).gridChild1;
        FPageIndex := 0;
      end;
  end;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_CAPTURE_DATE, sdDescending);
end;

procedure TCapturesSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_CAPTURE_ID).Visible then
      ColumnByFieldname(COL_CAPTURE_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_CAPTURE_DATE).Visible then
      ColumnByFieldName(COL_CAPTURE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CAPTURE_TYPE).Visible then
      ColumnByFieldName(COL_CAPTURE_TYPE).PickList.AddCommaText(rsCaptureTypeList);
    if DataSource.DataSet.FieldByName(COL_RIGHT_TARSUS).Visible then
      ColumnByFieldName(COL_RIGHT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TARSUS).Visible then
      ColumnByFieldName(COL_LEFT_TARSUS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_RIGHT_TIBIA).Visible then
      ColumnByFieldName(COL_RIGHT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LEFT_TIBIA).Visible then
      ColumnByFieldName(COL_LEFT_TIBIA).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_NET_STATION_NAME).Visible then
      ColumnByFieldname(COL_NET_STATION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LONGITUDE).Visible then
      ColumnByFieldname(COL_LONGITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LATITUDE).Visible then
      ColumnByFieldname(COL_LATITUDE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BANDER_NAME).Visible then
      ColumnByFieldname(COL_BANDER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_ANNOTATOR_NAME).Visible then
      ColumnByFieldname(COL_ANNOTATOR_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BAND_NAME).Visible then
      ColumnByFieldname(COL_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REMOVED_BAND_NAME).Visible then
      ColumnByFieldname(COL_REMOVED_BAND_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MOLT_LIMITS).Visible then
      ColumnByFieldName(COL_MOLT_LIMITS).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CYCLE_CODE).Visible then
      ColumnByFieldName(COL_CYCLE_CODE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_HOW_AGED).Visible then
      ColumnByFieldName(COL_HOW_AGED).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_HOW_SEXED).Visible then
      ColumnByFieldName(COL_HOW_SEXED).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_1_NAME).Visible then
      ColumnByFieldname(COL_PHOTOGRAPHER_1_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PHOTOGRAPHER_2_NAME).Visible then
      ColumnByFieldname(COL_PHOTOGRAPHER_2_NAME).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_CLOACAL_PROTUBERANCE).Visible then
      ColumnByFieldName(COL_CLOACAL_PROTUBERANCE).PickList.AddCommaText('U,N,S,M,L');
    if DataSource.DataSet.FieldByName(COL_BROOD_PATCH).Visible then
      ColumnByFieldName(COL_BROOD_PATCH).PickList.AddCommaText('F,N,V,W,O');
    if DataSource.DataSet.FieldByName(COL_FAT).Visible then
      ColumnByFieldName(COL_FAT).PickList.AddCommaText('N,T,L,H,F,B,G,V');
    if DataSource.DataSet.FieldByName(COL_BODY_MOLT).Visible then
      ColumnByFieldName(COL_BODY_MOLT).PickList.AddCommaText('N,T,S,H,G,A,F');
    if DataSource.DataSet.FieldByName(COL_FLIGHT_FEATHERS_MOLT).Visible then
      ColumnByFieldName(COL_FLIGHT_FEATHERS_MOLT).PickList.AddCommaText('N,S,A');
    if DataSource.DataSet.FieldByName(COL_FLIGHT_FEATHERS_WEAR).Visible then
      ColumnByFieldName(COL_FLIGHT_FEATHERS_WEAR).PickList.AddCommaText('N,S,L,M,H,X');
    if DataSource.DataSet.FieldByName(COL_SKULL_OSSIFICATION).Visible then
      ColumnByFieldName(COL_SKULL_OSSIFICATION).PickList.AddCommaText('N,T,L,H,G,A,F');
    if DataSource.DataSet.FieldByName(COL_SUBJECT_AGE).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeUnknown);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeAdult);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeJuvenile);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFledgling);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeNestling);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFirstYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeSecondYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeThirdYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFourthYear);
      ColumnByFieldName(COL_SUBJECT_AGE).PickList.Add(rsAgeFifthYear);
    end;
    if DataSource.DataSet.FieldByName(COL_SUBJECT_SEX).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexMale);
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexFemale);
      ColumnByFieldName(COL_SUBJECT_SEX).PickList.Add(rsSexUnknown);
    end;
    if DataSource.DataSet.FieldByName(COL_SUBJECT_STATUS).Visible then
    begin
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusNormal);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusInjured);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusWingSprain);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusStressed);
      ColumnByFieldName(COL_SUBJECT_STATUS).PickList.Add(rsStatusDead);
    end;
  end;
end;

procedure TCapturesSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
var
  aTaxon: Integer;
begin
  aTaxon := 0;

  if (Column.FieldName = COL_TAXON_NAME) then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if (Column.FieldName = COL_CAPTURE_TYPE) then
  begin
    case Column.Field.AsString of
      'N':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clBlueBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clBlueBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clAccentTextPrimaryLight;
        end;
      end;
      'R', 'S':
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
      'C':
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
        end;
      end;
      'U':
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
    end;
  end
  else
  if (Column.FieldName = COL_BAND_NAME) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'R') or
      (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'S') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemSuccessBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemSuccessFGLight;
    end
    else
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end
  else
  if (Column.FieldName = COL_REMOVED_BAND_NAME) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (TDBGrid(Sender).Columns.ColumnByFieldname(COL_CAPTURE_TYPE).Field.AsString = 'C') then
    begin
      //TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGDark
      else
        TDBGrid(Sender).Canvas.Font.Color := clSystemCautionFGLight;
    end;
  end;

  { Check if UseConditionalFormatting setting is enabled }
  if not xSettings.UseConditionalFormatting then
    Exit;

  { Paint the cell background red for invalid values }
  if (Column.FieldName = COL_CLOACAL_PROTUBERANCE) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, CLOACAL_PROTUBERANCE_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_BROOD_PATCH) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BROOD_PATCH_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FAT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FAT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_BODY_MOLT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, BODY_MOLT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FLIGHT_FEATHERS_MOLT) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FLIGHT_MOLT_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_FLIGHT_FEATHERS_WEAR) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, FEATHER_WEAR_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end
  else
  if (Column.FieldName = COL_SKULL_OSSIFICATION) then
  begin
    if (Column.Field.AsString <> '') and
      not (MatchStr(Column.Field.AsString, SKULL_OSSIFICATION_VALUES)) then
    begin
      if IsDarkModeEnabled then
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGDark
      else
        TDBGrid(Sender).Canvas.Brush.Color := clSystemCriticalBGLight;
    end;
  end;

  { Check if ShowOutliersOnGrid setting is enabled }
  if not xSettings.ShowOutliersOnGrid then
    Exit;

  { Paint the cell background yellow for outliers }
  if (Column.FieldName = COL_RIGHT_WING_CHORD) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_FIRST_SECONDARY_CHORD) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TAIL_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TARSUS_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_TARSUS_DIAMETER) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_WEIGHT) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_EXPOSED_CULMEN) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BILL_WIDTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_BILL_HEIGHT) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_NOSTRIL_BILL_TIP) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_SKULL_LENGTH) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end
  else
  if (Column.FieldName = COL_KIPPS_INDEX) then
  begin
    if (Column.Field.AsFloat <> 0.0) then
    begin
      aTaxon := GetKey('zoo_taxa', COL_TAXON_ID, COL_FULL_NAME,
                    TDBGrid(Sender).Columns.ColumnByFieldname(COL_TAXON_NAME).Field.AsString);
      if IsOutlier(aTaxon, Column.FieldName, Column.Field.AsFloat, 3) then
      begin
        if IsDarkModeEnabled then
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGDark
        else
          TDBGrid(Sender).Canvas.Brush.Color := clSystemCautionBGLight;
      end;
    end;
  end;
end;

{ TFeathersModuleController }

constructor TFeathersModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbFeathers;
  FCaptionText := rsTitleFeathersAndMolt;
  FDataSet := DMG.qFeathers;
  FSupportedMedia := [amtImages];
  FUiFlags := [gufShowVerifications, gufShowImages, gufShowSummary, gufShowInsertBatch];
  FPrintUiFlags := [pufFeathers];
  FFilterUiFlags := [fufMarked, fufTaxa, fufDates, fufSites, fufIndividual, fufPerson, fufTimeInterval];

  AddDefaultSort(COL_SAMPLE_DATE, sdDescending);
end;

procedure TFeathersModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');

    if ePersonFilter.Text <> EmptyStr then
      PersonFilterToSearch(FTableType, SearchConfig.QuickFilters, PersonIdFilter);

    if eStartTimeFilter.Text <> EmptyStr then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      if eEndTimeFilter.Text <> EmptyStr then
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLE_TIME, rscTime, sdtTime,
          crBetween, False, QuotedStr(eStartTimeFilter.Text), QuotedStr(eEndTimeFilter.Text)))
      else
        SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_SAMPLE_TIME, rscTime, sdtTime,
          crEqual, False, QuotedStr(eStartTimeFilter.Text)));
    end;

    if IndividualIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_ID, rscIndividual, sdtInteger,
        crEqual, False, IntToStr(IndividualIdFilter)));
    end;
  end;
end;

procedure TFeathersModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountTaxonFilter.Caption := rsNoneSelected;
    tvTaxaFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eStartTimeFilter.Clear;
    eEndTimeFilter.Clear;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eIndividualFilter.Clear;
    IndividualIdFilter := 0;
  end;
end;

procedure TFeathersModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_FEATHER_ID).Visible then
      ColumnByFieldname(COL_FEATHER_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_DATE).Visible then
      ColumnByFieldName(COL_SAMPLE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_NAME).Visible then
      ColumnByFieldname(COL_SIGHTING_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TFeathersModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_SAMPLE_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TFeathersModuleController.Search(AValue: String): Boolean;
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

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FEATHER_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, rscDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_SAMPLE_DATE, rscDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
          True, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_OBSERVER_NAME, rscObserver, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TFeathersSubmoduleController }

constructor TFeathersSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbFeathers;
  FCaptionText := rsTitleFeathersAndMolt;
  FDataSet := DMI.qFeathers;
  FGrid := TfrmCustomGrid(FOwner).gridChild2;
  FPageIndex := 1;
  FUiFlags := [gufShowVerifications];

  AddDefaultSort(COL_SAMPLE_DATE, sdDescending);
end;

procedure TFeathersSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_FEATHER_ID).Visible then
      ColumnByFieldname(COL_FEATHER_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_SAMPLE_DATE).Visible then
      ColumnByFieldName(COL_SAMPLE_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TAXON_NAME).Visible then
      ColumnByFieldName(COL_TAXON_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_OBSERVER_NAME).Visible then
      ColumnByFieldName(COL_OBSERVER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_LOCALITY_NAME).Visible then
      ColumnByFieldname(COL_LOCALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_SIGHTING_NAME).Visible then
      ColumnByFieldname(COL_SIGHTING_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TFeathersSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_TAXON_NAME then
  begin
    TDBGrid(Sender).Canvas.Font.Style := TDBGrid(Sender).Canvas.Font.Style + [fsItalic];
  end
  else
  if Column.FieldName = COL_SAMPLE_DATE then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

end.

