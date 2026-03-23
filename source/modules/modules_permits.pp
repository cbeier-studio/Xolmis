unit modules_permits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core, ufrm_customgrid;

type

  { TPermitsModuleController }

  TPermitsModuleController = class(TModuleController)
  public
    constructor Create(AOwner: TfrmCustomGrid); override;

    procedure ConfigureColumns(AGrid: TDBGrid); override;
    procedure ClearFilters; override;
    procedure ApplyFilters; override;
    function Search(AValue: String): Boolean; override;
    procedure PrepareCanvas(var Column: TColumn; var Sender: TObject); override;
  end;

implementation

uses
  utils_locale, utils_graphics, data_consts, data_columns, data_filters, models_media, udm_main, udm_grid;

{ TPermitsModuleController }

constructor TPermitsModuleController.Create(AOwner: TfrmCustomGrid);
begin
  inherited Create(AOwner);
  FTableType := tbPermits;
  FCaptionText := rsTitlePermits;
  FDataSet := DMG.qPermits;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowSummary, gufShowDocs, gufPrintMain, gufPrintByDate, gufPrintByProject];
  FFilterUiFlags := [fufMarked, fufDates, fufProject, fufPermitType];

  AddDefaultSort(COL_PERMIT_NAME, sdAscending);
end;

procedure TPermitsModuleController.ApplyFilters;
const
  PermitTypes: array of String = ('B', 'C', 'R', 'E', 'T', 'O');
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

    if cbPermitTypeFilter.ItemIndex > 0 then
    begin
      sf := SearchConfig.QuickFilters.Add(TSearchGroup.Create);
      SearchConfig.QuickFilters[sf].Fields.Add(TSearchField.Create(COL_PERMIT_TYPE, rscType, sdtText,
        crEqual, False, PermitTypes[cbPermitTypeFilter.ItemIndex - 1]));
    end;
  end;
end;

procedure TPermitsModuleController.ClearFilters;
begin
  with FOwner do
  begin
    lblCountDateFilter.Caption := rsNoneSelectedFemale;
    tvDateFilter.ClearChecked;

    eProjectFilter.Clear;
    ProjectIdFilter := 0;
  end;
end;

procedure TPermitsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_PERMIT_ID).Visible then
      ColumnByFieldname(COL_PERMIT_ID).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_DISPATCH_DATE).Visible then
      ColumnByFieldName(COL_DISPATCH_DATE).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_EXPIRE_DATE).Visible then
      ColumnByFieldName(COL_EXPIRE_DATE).ButtonStyle := cbsEllipsis;

    if DataSource.DataSet.FieldByName(COL_PERMIT_TYPE).Visible then
      with ColumnByFieldName(COL_PERMIT_TYPE).PickList do
      begin
        Clear;
        Add(rsPermitBanding);
        Add(rsPermitCollection);
        Add(rsPermitResearch);
        Add(rsPermitEntry);
        Add(rsPermitTransport);
        Add(rsPermitOther);
      end;
  end;
end;

procedure TPermitsModuleController.PrepareCanvas(var Column: TColumn; var Sender: TObject);
begin
  if (Column.FieldName = COL_EXPIRE_DATE) or
    (Column.FieldName = COL_PERMIT_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end;
end;

function TPermitsModuleController.Search(AValue: String): Boolean;
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
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_PERMIT_ID, rscId, sdtInteger, crEqual,
        False, aValue));
    end
    else
    if TryStrToDate(aValue, dt) then
    begin
      aValue := FormatDateTime('yyyy-mm-dd', dt);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DISPATCH_DATE, rscDispatchDate, sdtDate, crEqual,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EXPIRE_DATE, rscExpireDate, sdtDate, crEqual,
        False, aValue));
    end
    else
    if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
    begin
      aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
      m := ExtractDelimited(1, aValue, ['/']);
      y := ExtractDelimited(2, aValue, ['/']);
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DISPATCH_DATE, rscDispatchDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_EXPIRE_DATE, rscExpireDate, sdtMonthYear, crEqual,
        False, y + '-' + m));
    end
    else
    begin
      g := FOwner.SearchConfig.Fields.Add(TSearchGroup.Create);
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_PERMIT_NAME, rscName, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_PERMIT_NUMBER, rscPermitNumber, sdtText, Crit,
        False, aValue));
      FOwner.SearchConfig.Fields[g].Fields.Add(TSearchField.Create(COL_DISPATCHER_NAME, rscDispatcher, sdtText, Crit,
        True, aValue));
    end;
  end;

  ApplyFilters;

  Result := FOwner.SearchConfig.RunSearch > 0;
end;

end.

