{ Xolmis Breeding Data Modules controllers

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

unit modules_breeding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TNestsModuleController }

  TNestsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TNestsSubmoduleController }

  TNestsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TNestRevisionsModuleController }

  TNestRevisionsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TNestRevisionsSubmoduleController }

  TNestRevisionsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TEggsModuleController }

  TEggsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TEggsSubmoduleController }

  TEggsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TNestOwnersSubmoduleController }

  TNestOwnersSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, utils_themes, data_consts, data_columns, data_filters, models_media,
  uDarkStyleParams,
  udm_main, udm_grid, udm_breeding, udm_individuals, ufrm_customgrid;

{ TNestsModuleController }

constructor TNestsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbNests;
  FCaptionText := rsTitleNests;
  FDataSet := DMG.qNests;
  FSupportedMedia := [amtImages, amtVideos, amtDocuments];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowMap, gufShowImages, gufShowVideos, gufShowDocs];
  FPrintUiFlags := [pufNests, pufNestsByDate, pufNestsByLocality, pufNestsByProject, pufNestsByTaxon];
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
  with TfrmCustomGrid(FOwner) do
  begin
    // Taxa
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    // Sites
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'g.');
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Nest fate
    if cbNestFateFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_NEST_FATE, rscNestFate, NestFate[cbNestFateFilter.ItemIndex - 1]);
    end;
    // Support type
    if cbNestSupportFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_SUPPORT_TYPE, rscSupportType, NestSupport[cbNestSupportFilter.ItemIndex - 1]);
    end;
    // Person
    AddLookupFilter(SearchConfig, [COL_OBSERVER_ID], [rscObserver], PersonIdFilter);
    // Project
    AddLookupFilter(SearchConfig, [COL_PROJECT_ID], [rscProject], ProjectIdFilter);
    // Support plant
    AddLookupFilter(SearchConfig, [COL_SUPPORT_PLANT_1_ID, COL_SUPPORT_PLANT_2_ID],
      [rscSupportPlant1, rscSupportPlant2], SupportPlantIdFilter);
  end;
end;

procedure TNestsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
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

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
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

procedure TNestsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
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

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_NEST_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, Dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', Dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FOUND_DATE, rscFoundDate, sdtDate, crEqual,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LAST_DATE, rscLastDateActive, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FOUND_DATE, rscFoundDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LAST_DATE, rscLastDateActive, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FIELD_NUMBER, rscFieldNumber, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_LOCALITY_NAME, rscLocality, sdtText, Crit,
          True, aValue));
        { #todo : Check field name }
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create('z.full_name', rscTaxon, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_OBSERVER_NAME, rscObserver, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_1_NAME, rscSupportPlant1, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SUPPORT_PLANT_2_NAME, rscSupportPlant2, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TNestsSubmoduleController }

constructor TNestsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbNests;
  FCaptionText := rsTitleNests;
  FDataSet := DMI.qNests;
  FGrid := TfrmCustomGrid(FOwner).gridChild4;
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

    if DataSource.DataSet.FieldByName(COL_COORDINATE_PRECISION).Visible then
    begin
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsExactCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsApproximatedCoordinate);
      ColumnByFieldName(COL_COORDINATE_PRECISION).PickList.Add(rsReferenceCoordinate);
    end;
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

procedure TNestsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
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

constructor TNestRevisionsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbNestRevisions;
  FCaptionText := rsTitleNestRevisions;
  FDataSet := DMG.qNestRevisions;
  FSupportedMedia := [amtImages, amtVideos];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowImages, gufShowVideos];
  FPrintUiFlags := [];
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
  with TfrmCustomGrid(FOwner) do
  begin
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Nest status
    if cbNestStatusFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_NEST_STATUS, rscStatus, NestStatus[cbNestStatusFilter.ItemIndex - 1]);
    end;
    // Nest stage
    if cbNestStageFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_NEST_STAGE, rscNestStage, NestStages[cbNestStageFilter.ItemIndex - 1]);
    end;
    // Person
    AddLookupFilter(SearchConfig, [COL_OBSERVER_1_ID, COL_OBSERVER_2_ID],
      [rscObserver1, rscObserver2], PersonIdFilter);
    // Time interval
    AddTimeFilter(SearchConfig, COL_REVISION_TIME, rscTime, eStartTimeFilter.Text, eEndTimeFilter.Text);
    // Nest
    AddLookupFilter(SearchConfig, [COL_NEST_ID], [rscNest], NestIdFilter);
    // Nidoparasite presence
    AddNonZeroIntegerFilter(SearchConfig, COL_NIDOPARASITE_ID, rscNidoparasite, rbNidoparasiteYes.Checked, rbNidoparasiteNo.Checked);
    // Philornis presence
    AddBooleanFilter(SearchConfig, COL_HAVE_PHILORNIS_LARVAE, rscHasPhilornisLarvae, rbPhilornisYes.Checked, rbPhilornisNo.Checked);
  end;
end;

procedure TNestRevisionsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
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

procedure TNestRevisionsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
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

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_NEST_REVISION_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, Dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', Dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_REVISION_DATE, rscDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_REVISION_DATE, rscDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_NIDOPARASITE_NAME, rscNidoparasite, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TNestRevisionsSubmoduleController }

constructor TNestRevisionsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbNestRevisions;
  FCaptionText := rsTitleNestRevisions;
  FDataSet := DMB.qNestRevisions;
  FGrid := TfrmCustomGrid(FOwner).gridChild2;
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

procedure TNestRevisionsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
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

constructor TEggsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbEggs;
  FCaptionText := rsTitleEggs;
  FDataSet := DMG.qEggs;
  FSupportedMedia := [amtImages];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowImages];
  FPrintUiFlags := [pufEggs, pufEggsByLocality, pufEggsByNest, pufEggsByTaxon];
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
  with TfrmCustomGrid(FOwner) do
  begin
    // Taxa
    TaxonFilterToSearch(tvTaxaFilter, SearchConfig.QuickFilters, 'z.');
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Person
    AddLookupFilter(SearchConfig, [COL_RESEARCHER_ID], [rscResearcher], PersonIdFilter);
    // Nest
    AddLookupFilter(SearchConfig, [COL_NEST_ID], [rscNest], NestIdFilter);
    // Individual
    AddLookupFilter(SearchConfig, [COL_INDIVIDUAL_ID], [rscIndividual], IndividualIdFilter);
    // Egg shape
    if cbEggShapeFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_EGG_SHAPE, rscEggShape, EggShapes[cbEggShapeFilter.ItemIndex - 1]);
    end;
    // Eggshell pattern
    if cbEggPatternFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_EGGSHELL_PATTERN, rscEggshellPattern, EggPatterns[cbEggPatternFilter.ItemIndex - 1]);
    end;
    // Eggshell texture
    if cbEggTextureFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_EGGSHELL_TEXTURE, rscEggshellTexture, EggTextures[cbEggTextureFilter.ItemIndex - 1]);
    end;
    // Egg hatched
    AddBooleanFilter(SearchConfig, COL_EGG_HATCHED, rscHatched, rbHatchedYes.Checked, rbHatchedNo.Checked);
  end;
end;

procedure TEggsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
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

procedure TEggsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
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

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_EGG_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, Dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', Dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_MEASURE_DATE, rscDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_MEASURE_DATE, rscDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME,rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FIELD_NUMBER, rscFieldNumber, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_TAXON_NAME, rscTaxon, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TEggsSubmoduleController }

constructor TEggsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbEggs;
  FCaptionText := rsTitleEggs;
  FDataSet := DMB.qEggs;
  FGrid := TfrmCustomGrid(FOwner).gridChild3;
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

procedure TEggsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
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

constructor TNestOwnersSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbNestOwners;
  FCaptionText := rsTitleNestOwners;
  FDataSet := DMB.qNestOwners;
  FGrid := TfrmCustomGrid(FOwner).gridChild1;
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

procedure TNestOwnersSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

end.

