{ Xolmis Permits Module controllers

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

unit modules_permits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils, DateUtils,
  data_types, modules_core;

type

  { TPermitsModuleController }

  TPermitsModuleController = class(TModuleController)
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
  utils_locale, utils_graphics, utils_themes,
  data_consts, data_columns, data_filters, models_media,
  udm_main, udm_grid, ufrm_customgrid, uDarkStyleParams;

{ TPermitsModuleController }

constructor TPermitsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbPermits;
  FCaptionText := rsTitlePermits;
  FDataSet := DMG.qPermits;
  FSupportedMedia := [amtDocuments];
  FUiFlags := [gufShowSummary, gufShowDocs];
  FPrintUiFlags := [pufPermits, pufPermitsByDate, pufPermitsByProject];
  FFilterUiFlags := [fufMarked, fufDates, fufProject, fufPermitType];

  AddDefaultSort(COL_PERMIT_NAME, sdAscending);
end;

procedure TPermitsModuleController.ApplyFilters;
const
  PermitTypes: array of String = ('B', 'C', 'R', 'E', 'T', 'O');
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Dates
    DateFilterToSearch(FTableType, tvDateFilter, SearchConfig.QuickFilters);
    // Project
    AddLookupFilter(SearchConfig, [COL_PROJECT_ID], [rscProject], ProjectIdFilter);
    // Permit type
    if cbPermitTypeFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_PERMIT_TYPE, rscType, PermitTypes[cbPermitTypeFilter.ItemIndex - 1]);
    end;
  end;
end;

procedure TPermitsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
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

procedure TPermitsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_PERMIT_NUMBER) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if (Column.FieldName = COL_EXPIRE_DATE) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    if (Column.Field.AsDateTime < Today) then
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

    with TfrmCustomGrid(FOwner) do
    begin
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PERMIT_ID, rscId, sdtInteger, crEqual,
          False, aValue));
      end
      else
      if TryStrToDate(aValue, dt) then
      begin
        aValue := FormatDateTime('yyyy-mm-dd', dt);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DISPATCH_DATE, rscDispatchDate, sdtDate, crEqual,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_EXPIRE_DATE, rscExpireDate, sdtDate, crEqual,
          False, aValue));
      end
      else
      if ExecRegExpr('^\d{2}[/]{1}\d{4}$', aValue) then
      begin
        aValue := StringReplace(aValue, ' ', '', [rfReplaceAll]);
        m := ExtractDelimited(1, aValue, ['/']);
        y := ExtractDelimited(2, aValue, ['/']);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DISPATCH_DATE, rscDispatchDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_EXPIRE_DATE, rscExpireDate, sdtMonthYear, crEqual,
          False, y + '-' + m));
      end
      else
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PERMIT_NAME, rscName, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PERMIT_NUMBER, rscPermitNumber, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_DISPATCHER_NAME, rscDispatcher, sdtText, Crit,
          True, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

