{ Xolmis Band Module controllers

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

unit modules_bands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, DB, SQLDB, Grids, DBGrids, RegExpr, StrUtils,
  data_types, modules_core;

type

  { TBandsModuleController }

  TBandsModuleController = class(TModuleController)
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
  utils_locale, utils_global, utils_graphics, utils_themes, utils_validations,
  data_consts, data_columns, data_filters,
  uDarkStyleParams,
  udm_grid, ufrm_customgrid;

{ TBandsModuleController }

constructor TBandsModuleController.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FTableType := tbBands;
  FCaptionText := rsTitleBands;
  FDataSet := DMG.qBands;
  FSupportedMedia := [];
  FUiFlags := [gufShowVerifications, gufShowSummary, gufShowInsertBatch, gufShowMoreOptions,
    gufShowReceiveBands, gufShowTransferBands, gufShowBandsBalance, gufShowBandHistory];
  FFilterUiFlags := [fufMarked, fufBandSize, fufBandStatus, fufBandType, fufBandSource,
    fufPerson, fufInstitution, fufProject];

  AddDefaultSort(COL_BAND_SIZE, sdAscending);
  AddDefaultSort(COL_BAND_NUMBER, sdAscending);
end;

procedure TBandsModuleController.ApplyFilters;
const
  BandStatus: array of String = ('O', 'A', 'U', 'R', 'T', 'B', 'L');
  BandTypes: array of String = ('A', 'F', 'N', 'W', 'T', 'L', 'R', 'C', 'O');
  BandSources: array of String = ('A', 'T', 'L', 'D', 'F');
var
  sf: Integer;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    // Band size
    if cbBandSizeFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_BAND_SIZE, rscSize, cbBandSizeFilter.Text);
    end;
    // Band status
    if cbBandStatusFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_BAND_STATUS, rscStatus, BandStatus[cbBandStatusFilter.ItemIndex - 1]);
    end;
    // Band type
    if cbBandTypeFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_BAND_TYPE, rscType, BandTypes[cbBandTypeFilter.ItemIndex - 1]);
    end;
    // Band source
    if cbBandSourceFilter.ItemIndex > 0 then
    begin
      AddExactTextFilter(SearchConfig, COL_BAND_SOURCE, rscSource, BandSources[cbBandSourceFilter.ItemIndex - 1]);
    end;
    // Band reported
    //AddBooleanFilter(SearchConfig, COL_BAND_REPORTED, rscReported, rbReportedYes.Checked, rbReportedNo.Checked);
    // Person
    AddLookupFilter(SearchConfig, [COL_CARRIER_ID], [rscCarrier], PersonIdFilter);
    // Institution
    AddLookupFilter(SearchConfig, [COL_SUPPLIER_ID], [rscSupplier], InstitutionIdFilter);
    // Project
    AddLookupFilter(SearchConfig, [COL_PROJECT_ID], [rscProject], ProjectIdFilter);
  end;
end;

procedure TBandsModuleController.ClearFilters;
begin
  with TfrmCustomGrid(FOwner) do
  begin
    cbBandSizeFilter.ItemIndex := 0;

    cbBandStatusFilter.ItemIndex := 0;
    cbBandTypeFilter.ItemIndex := 0;
    cbBandSourceFilter.ItemIndex := 0;

    //rbReportedAll.Checked := True;

    ePersonFilter.Clear;
    PersonIdFilter := 0;
    eInstitutionFilter.Clear;
    InstitutionIdFilter := 0;
    eProjectFilter.Clear;
    ProjectIdFilter := 0;
  end;
end;

procedure TBandsModuleController.ConfigureColumns(AGrid: TDBGrid);
begin
  with aGrid, Columns do
  begin
    if DataSource.DataSet.FieldByName(COL_BAND_ID).Visible then
      ColumnByFieldname(COL_BAND_ID).ReadOnly := True;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_ID).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_ID).ReadOnly := True;
    if DataSource.DataSet.FieldByName(COL_INDIVIDUAL_NAME).Visible then
      ColumnByFieldname(COL_INDIVIDUAL_NAME).ReadOnly := True;

    if DataSource.DataSet.FieldByName(COL_BAND_SIZE).Visible then
      ColumnByFieldName(COL_BAND_SIZE).PickList.AddCommaText('A,C,D,E,F,G,H,J,L,M,N,P,R,S,T,U,V,X,Z');
    if DataSource.DataSet.FieldByName(COL_BAND_STATUS).Visible then
      ColumnByFieldName(COL_BAND_STATUS).PickList.AddCommaText(rsBandStatusList);
    if DataSource.DataSet.FieldByName(COL_BAND_SOURCE).Visible then
    begin
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandAcquiredFromSupplier);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandTransferBetweenBanders);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandLivingBirdBandedByOthers);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandDeadBirdBandedByOthers);
      ColumnByFieldName(COL_BAND_SOURCE).PickList.Add(rsBandFoundLoose);
    end;
    if DataSource.DataSet.FieldByName(COL_BAND_TYPE).Visible then
      ColumnByFieldName(COL_BAND_TYPE).PickList.AddCommaText(rsBandTypeList);

    if DataSource.DataSet.FieldByName(COL_SUPPLIER_NAME).Visible then
      ColumnByFieldName(COL_SUPPLIER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_REQUESTER_NAME).Visible then
      ColumnByFieldname(COL_REQUESTER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_CARRIER_NAME).Visible then
      ColumnByFieldName(COL_CARRIER_NAME).ButtonStyle := cbsEllipsis;
    if DataSource.DataSet.FieldByName(COL_PROJECT_NAME).Visible then
      ColumnByFieldName(COL_PROJECT_NAME).ButtonStyle := cbsEllipsis;
  end;
end;

procedure TBandsModuleController.PrepareCanvas(Column: TColumn; Sender: TObject);
begin
  if (Column.FieldName = COL_BAND_SIZE) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
  end
  else
  if (Column.FieldName = COL_BAND_STATUS) and
    (xSettings.UseConditionalFormatting) then
  begin
    SetBoldFont(TDBGrid(Sender).Canvas.Font);
    case Column.Field.AsString of
      'U': // Used
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.System.SuccessBG;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.System.SuccessFG;
      end;
      'A': // Available
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.System.NeutralBG;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.System.NeutralFG;
      end;
      'R': // Removed
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.System.MediumBG;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.System.MediumFG;
      end;
      'B': // Broken
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.System.CriticalBG;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.System.CriticalFG;
      end;
      'L': // Lost
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.System.CautionBG;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.System.CautionFG;
      end;
      'T': // Transferred
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.Categorical.Jacaranda.Background;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.Categorical.Jacaranda.Foreground;
      end;
      'X': // Returned
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.Categorical.Azul.Background;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.Categorical.Azul.Foreground;
      end;
      'O': // Ordered
      begin
        TDBGrid(Sender).Canvas.Brush.Color := ActiveTheme.System.AttentionBG;
        TDBGrid(Sender).Canvas.Font.Color := ActiveTheme.System.AttentionFG;
      end;
    end;
  end;
end;

function TBandsModuleController.Search(AValue: String): Boolean;
var
  i, i1, i2, g: Integer;
  V1, V2: String;
  Crit: TCriteriaType;
begin
  Result := False;

  Crit := crLike;
  aValue := Trim(aValue);
  V1 := EmptyStr;
  V2 := EmptyStr;

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
      // Band number interval
      if TryParseIntegerInterval(aValue, i1, i2) then
      begin
        V1 := IntToStr(i1);
        V2 := IntToStr(i2);
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_BAND_NUMBER, rscNumber, sdtInteger, crBetween,
          True, V1, V2));
      end
      else
      // ID and band number
      if TryStrToInt(aValue, i) then
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_BAND_ID, rscId, sdtInteger, crEqual,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_BAND_NUMBER, rscNumber, sdtText, Crit,
          True, aValue));
      end
      else
      // Text
      begin
        g := SearchConfig.TextFilters.Add(TSearchGroup.Create);
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_FULL_NAME, rscFullName, sdtText, Crit,
          True, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_CARRIER_NAME, rscCarrier, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_SUPPLIER_NAME, rscSupplier, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_PROJECT_NAME, rscProject, sdtText, Crit,
          False, aValue));
        SearchConfig.TextFilters[g].Fields.Add(TSearchField.Create(COL_INDIVIDUAL_NAME, rscIndividual, sdtText, Crit,
          False, aValue));
      end;
    end;
  end;

  ApplyFilters;

  Result := TfrmCustomGrid(FOwner).SearchConfig.RunSearch > 0;
end;

end.

