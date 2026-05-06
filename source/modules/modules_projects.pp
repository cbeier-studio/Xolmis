{ Xolmis Projects Module controllers

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

unit modules_projects;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, DateUtils,
  data_types, modules_core;

type

  { TProjectsModuleController }

  TProjectsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TProjectMembersSubmoduleController }

  TProjectMembersSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TProjectGoalsSubmoduleController }

  TProjectGoalsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TProjectChronogramsSubmoduleController }

  TProjectChronogramsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TProjectBudgetsSubmoduleController }

  TProjectBudgetsSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

  { TProjectExpensesSubmoduleController }

  TProjectExpensesSubmoduleController = class(TSubmoduleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_global, utils_graphics, utils_themes, utils_validations, utils_system,
  data_consts, data_columns, data_filters, models_media,
  uDarkStyleParams,
  udm_main, udm_grid, ufrm_customgrid;

{ TProjectsModuleController }

constructor TProjectsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjects;
  FCaptionText := rsTitleProjects;
  FDataSet := DMG.qProjects;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowSummary, gufShowDocs];
  FPrintUiFlags := [pufProjects];
  FFilterUiFlags := [fufMarked, fufDates];

  AddDefaultSort(COL_PROJECT_TITLE, sdAscending);

  FSubmodules.Add(TProjectMembersSubmoduleController.Create(FOwner));
  FSubmodules.Add(TProjectGoalsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TProjectChronogramsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TProjectBudgetsSubmoduleController.Create(FOwner));
  FSubmodules.Add(TProjectExpensesSubmoduleController.Create(FOwner));
end;

procedure TProjectsModuleController.ApplyFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
  end;
end;

procedure TProjectsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;
  end;
end;

procedure TProjectsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PROJECT_ID).Visible then
      ColumnByFieldname(COL_PROJECT_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_START_DATE).Visible then
      ColumnByFieldName(COL_START_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_DATE).Visible then
      ColumnByFieldName(COL_END_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TProjectsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_PROTOCOL_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if (Column.FieldName = COL_END_DATE) then
  begin
    if not (Column.Field.IsNull) and (Column.Field.AsDateTime < Today) then
      //(TDBGrid(Sender).Columns.ColumnByFieldname(COL_PROJECT_STATUS).Field.AsString = 'R') then
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
end;

function TProjectsModuleController.Search(AValue: String): Boolean;
var
  i, g, m, y, y1, y2, M1, M2: Integer;
  dt, Dt1, Dt2: TDateTime;
  Crit: TCriteriaType;
  V1, V2: String;
  PartialStart, PartialEnd: TPartialDate;
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
      // Date interval
      if TryParsePartialDateIntervalFlexible(aValue, PartialStart, PartialEnd) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(
          TSearchField.Create(COL_START_DATE, rscDate, sdtDate, crIntersect,
            False, PartialStart.ToStartDateString, PartialEnd.ToEndDateString, '', COL_END_DATE)
        );
      end
      else
      // ID and year
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PROJECT_ID, rscId, sdtInteger, crEqual,
          False, aValue));
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
          False, aValue, '', '', COL_END_DATE));
      end
      else
      // Month/year
      if TryParseMonthYearFlexible(aValue, y, m) then
      begin
        aValue := Format('%.4d-%.2d', [y, m]);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscDate, sdtMonthYear, crIntersect,
          False, aValue, '', '', COL_END_DATE));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PROJECT_TITLE, rscTitle, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SHORT_TITLE, rscShortTitle, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PROTOCOL_NUMBER, rscProtocolNr, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_CONTACT_NAME, rscContactPerson, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_MAIN_GOAL, rscMainGoal, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_RISKS, rscRisks, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_ABSTRACT, rscAbstract, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

{ TProjectMembersSubmoduleController }

constructor TProjectMembersSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjectTeams;
  FCaptionText := rsCaptionTeam;
  FDataSet := DMG.qProjectTeam;
  FGrid := TfrmCustomGrid(FOwner).gridChild1;
  FPageIndex := 0;
  FUiFlags := [];

  AddDefaultSort(COL_PERSON_NAME, sdAscending);
end;

procedure TProjectMembersSubmoduleController.ConfigureColumns;
begin

end;

procedure TProjectMembersSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TProjectGoalsSubmoduleController }

constructor TProjectGoalsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjectGoals;
  FCaptionText := rsCaptionGoals;
  FDataSet := DMG.qProjectGoals;
  FGrid := TfrmCustomGrid(FOwner).gridChild2;
  FPageIndex := 1;
  FUiFlags := [];

  AddDefaultSort(COL_GOAL_DESCRIPTION, sdAscending);
end;

procedure TProjectGoalsSubmoduleController.ConfigureColumns;
begin

end;

procedure TProjectGoalsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TProjectChronogramsSubmoduleController }

constructor TProjectChronogramsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjectChronograms;
  FCaptionText := rsCaptionChronogram;
  FDataSet := DMG.qProjectChronogram;
  FGrid := TfrmCustomGrid(FOwner).gridChild3;
  FPageIndex := 2;
  FUiFlags := [];

  AddDefaultSort(COL_DESCRIPTION, sdAscending);
end;

procedure TProjectChronogramsSubmoduleController.ConfigureColumns;
begin
  with FGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_CHRONOGRAM_ID).Visible then
      ColumnByFieldname(COL_CHRONOGRAM_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_START_DATE).Visible then
      ColumnByFieldName(COL_START_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_TARGET_DATE).Visible then
      ColumnByFieldName(COL_TARGET_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_END_DATE).Visible then
      ColumnByFieldName(COL_END_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_GOAL_DESCRIPTION).Visible then
      ColumnByFieldName(COL_GOAL_DESCRIPTION).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TProjectChronogramsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_TARGET_DATE) then
  begin
    if not (Column.Field.IsNull) and (Column.Field.AsDateTime < Today) and
      not ((TDBGrid(Sender).Columns.ColumnByFieldname(COL_PROGRESS_STATUS).Field.AsString = 'C') or
      (TDBGrid(Sender).Columns.ColumnByFieldname(COL_PROGRESS_STATUS).Field.AsString = 'F')) then
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
  end
  else
  if (Column.FieldName = COL_PROGRESS_STATUS) and
    (xSettings.UseConditionalFormatting) then
  begin
    case Column.Field.AsString of
      'T':     // To do
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
      'P':     // In progress
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
      'R':     // Needs review
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
      'B':    // Blocked
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
      'D':    // Delayed
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clTagBrightOrangeBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clTagBrightOrangeFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clTagBrightOrangeBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clTagBrightOrangeFGLight;
        end;
      end;
      'C':    // Cancelled
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
      'F':   // Done
      begin
        if IsDarkModeEnabled then
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clTagVioletBGDark;
          TDBGrid(Sender).Canvas.Font.Color := clTagVioletFGDark;
        end
        else
        begin
          TDBGrid(Sender).Canvas.Brush.Color := clTagVioletBGLight;
          TDBGrid(Sender).Canvas.Font.Color := clTagVioletFGLight;
        end;
      end;
    end;
  end;
end;

{ TProjectBudgetsSubmoduleController }

constructor TProjectBudgetsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjectBudgets;
  FCaptionText := rsCaptionBudget;
  FDataSet := DMG.qProjectBudget;
  FGrid := TfrmCustomGrid(FOwner).gridChild4;
  FPageIndex := 3;
  FUiFlags := [];

  AddDefaultSort(COL_RUBRIC, sdAscending);
  AddDefaultSort(COL_ITEM_NAME, sdAscending);
end;

procedure TProjectBudgetsSubmoduleController.ConfigureColumns;
begin

end;

procedure TProjectBudgetsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TProjectExpensesSubmoduleController }

constructor TProjectExpensesSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjectExpenses;
  FCaptionText := rsCaptionExpenses;
  FDataSet := DMG.qProjectExpenses;
  FGrid := TfrmCustomGrid(FOwner).gridChild5;
  FPageIndex := 4;
  FUiFlags := [];

  AddDefaultSort(COL_EXPENSE_DATE, sdDescending);
end;

procedure TProjectExpensesSubmoduleController.ConfigureColumns;
begin

end;

procedure TProjectExpensesSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

end.

