unit modules_people;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TPeopleModuleController }

  TPeopleModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TForm); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(Column: TColumn; Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, ufrm_customgrid;

{ TPeopleModuleController }

constructor TPeopleModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbPeople;
  FCaptionText := rsTitleResearchers;
  FDataSet := DMG.qPeople;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowDocs, gufShowSummary];
  FPrintUiFlags := [pufResearchers];
  FFilterUiFlags := [fufMarked, fufSites, fufDates, fufInstitution];

  AddDefaultSort(COL_FULL_NAME, sdAscending);
end;

procedure TPeopleModuleController.ApplyFilters;
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    SiteFilterToSearch(tvSiteFilter, SearchConfig.QuickFilters, 'p.');
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);

    if InstitutionIdFilter > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_INSTITUTION_ID, rscInstitution, sdtInteger,
        crEqual, False, IntToStr(InstitutionIdFilter)));
    end;
  end;
end;

procedure TPeopleModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    lblCountSiteFilter.Caption := rsNoneSelected;
    tvSiteFilter.ClearChecked;

    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eInstitutionFilter.Clear;
    InstitutionIdFilter := 0;
  end;
end;

procedure TPeopleModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERSON_ID).Visible then
      ColumnByFieldname(COL_PERSON_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_GENDER).Visible then
      ColumnByFieldName(COL_GENDER).PickList.AddCommaText(rsGenderList);
    if DataSource.DataSet.FieldByName(COL_TITLE_TREATMENT).Visible then
      ColumnByFieldName(COL_TITLE_TREATMENT).PickList.AddCommaText(rsTreatmentList);

    if DataSource.DataSet.FieldByName(COL_INSTITUTION_NAME).Visible then
      ColumnByFieldname(COL_INSTITUTION_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_COUNTRY_NAME).Visible then
      ColumnByFieldname(COL_COUNTRY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_STATE_NAME).Visible then
      ColumnByFieldname(COL_STATE_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_MUNICIPALITY_NAME).Visible then
      ColumnByFieldname(COL_MUNICIPALITY_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_BIRTH_DATE).Visible then
      ColumnByFieldName(COL_BIRTH_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DEATH_DATE).Visible then
      ColumnByFieldName(COL_DEATH_DATE).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TPeopleModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if Column.FieldName = COL_ABBREVIATION then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TPeopleModuleController.Search(AValue: String): Boolean;
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
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_PERSON_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, rscBirthDate, sdtDate, crEqual,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, rscDeathDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_BIRTH_DATE, rscBirthDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DEATH_DATE, rscDeathDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.Fields.Add(TSearchGroup.Create);
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_ABBREVIATION, rscAbbreviation, sdtText, Crit,
          False, aValue));
        SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_CITATION, rscCitation, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

