unit modules_projects;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
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
  utils_locale, utils_graphics, utils_themes, data_consts, data_columns, data_filters, models_media,
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
  if (Column.FieldName = COL_START_DATE) or
    (Column.FieldName = COL_PROTOCOL_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TProjectsModuleController.Search(AValue: String): Boolean;
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
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PROJECT_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscStartDate, sdtDate, crEqual,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_END_DATE, rscEndDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_START_DATE, rscStartDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_END_DATE, rscEndDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
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
  FCaptionText := rsTitleProjectMembers;
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
  FCaptionText := rsTitleProjectGoals;
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
  FCaptionText := rsTitleProjectChronograms;
  FDataSet := DMG.qProjectChronogram;
  FGrid := TfrmCustomGrid(FOwner).gridChild3;
  FPageIndex := 2;
  FUiFlags := [];

  AddDefaultSort(COL_DESCRIPTION, sdAscending);
end;

procedure TProjectChronogramsSubmoduleController.ConfigureColumns;
begin

end;

procedure TProjectChronogramsSubmoduleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin

end;

{ TProjectBudgetsSubmoduleController }

constructor TProjectBudgetsSubmoduleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbProjectBudgets;
  FCaptionText := rsTitleProjectBudgets;
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
  FCaptionText := rsTitleProjectExpenses;
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

