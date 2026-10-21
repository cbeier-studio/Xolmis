{ Xolmis Find Taxon dialog

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_findtaxon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, SQLDB, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, BCPanel, ColorSpeedButton, RegExpr, StrUtils,
  utils_system, models_taxonomy, data_types, Grids, models_record_types, Types;

type

  { TdlgFindTaxon }

  TdlgFindTaxon = class(TForm)
    uList: TDrawGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblEmptyQuery: TLabel;
    dsFind: TDataSource;
    EP: TEdit;
    pEmptyQuery: TBCPanel;
    pSearchBar: TPanel;
    pEP: TBCPanel;
    qFind: TSQLQuery;
    sbOptions: TColorSpeedButton;
    sbClose: TColorSpeedButton;
    TimerFind: TTimer;
    procedure dsFindStateChange(Sender: TObject);
    procedure EPChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure uListClick(Sender: TObject);
    procedure uListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure uListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure uListSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    xCod: Integer;
    xNome, xCodStr, xInit, xFiltro: String;
    xTaxFiltro: TTaxonFilters;
    xValid: Boolean;
    oOrder, oDirection: String;
    FTaxonList: TTaxonList;
    function Pesquisar(aValor: String): Boolean;
    function PesquisarHashtag(aValor: String): Boolean;
    procedure ApplyDarkMode;
    procedure MoveGridRow(Delta: Integer);
    procedure GetRankFilter(const aSQL: TStrings; aFilter: TTaxonFilters);
    procedure SetSelect(const aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure LoadTaxonMetadataFromDB;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property Codigo: Integer read xCod write xCod default 0;
    property CodigoStr: String read xCodStr write xCodStr;
    property Nome: String read xNome write xNome;
    property Init: String read xInit write xInit;
    property Filtro: String read xFiltro write xFiltro;
    property FiltroTaxon: TTaxonFilters read xTaxFiltro write xTaxFiltro;
    property UsarValido: Boolean read xValid write xValid;
  end;

var
  dlgFindTaxon: TdlgFindTaxon;

implementation

uses
  utils_locale, utils_global, data_getvalue, data_consts, utils_conversions, utils_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgFindTaxon }

procedure TdlgFindTaxon.ApplyDarkMode;
begin
  pEP.Background.Color := clSystemSolidNeutralBGDark;
  pEP.Border.Color := clSystemNeutralBGDark;
  pEP.ParentBackground := True;
  EP.Color := pEP.Background.Color;
  sbClose.Images := iButtonsDark;
  sbClose.StateHover.Color := clSolidBGBaseDark;
  sbClose.StateActive.Color := clSolidBGSecondaryDark;
  sbClose.StateNormal.Color := pEP.Background.Color;
  sbOptions.Images := iButtonsDark;
  sbOptions.StateHover.Color := clSolidBGBaseDark;
  sbOptions.StateActive.Color := clSolidBGSecondaryDark;
  sbOptions.StateNormal.Color := pEP.Background.Color;

  //uList.AlternateColor := clSolidBGSecondaryDark;

  pEmptyQuery.Background.Color := clCardBGDefaultDark;
  pEmptyQuery.Border.Color := clCardBGSecondaryDark;
  pEmptyQuery.Color := uList.Color;
end;

procedure TdlgFindTaxon.dsFindStateChange(Sender: TObject);
begin
  pEmptyQuery.Visible := (dsFind.DataSet.RecordCount = 0);
end;

procedure TdlgFindTaxon.EPChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  if Length(Trim(EP.Text)) = 0 then
  begin
    // Empty search: close the dataset and clear the list instead of querying
    if qFind.Active then
      qFind.Close;
    uList.Enabled := False;
    LoadTaxonMetadataFromDB;
    Exit;
  end;
  TimerFind.Enabled := True;
end;

procedure TdlgFindTaxon.FormCreate(Sender: TObject);
begin
  xNome := EmptyStr;
  xCodStr := EmptyStr;

  FTaxonList := TTaxonList.Create(True);
end;

procedure TdlgFindTaxon.FormDestroy(Sender: TObject);
begin
  // Fecha tabelas
  if qFind.Active then
    qFind.Close;

  FTaxonList.Free;
end;

procedure TdlgFindTaxon.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then
    Exit;

  // Navigation is driven only by the grid; OnSelectCell syncs the dataset
  case Key of
    VK_DOWN: MoveGridRow(1);
    VK_UP: MoveGridRow(-1);
    VK_NEXT: MoveGridRow(Max(1, uList.VisibleRowCount - 1));
    VK_PRIOR: MoveGridRow(-Max(1, uList.VisibleRowCount - 1));
  else
    Exit;
  end;
  Key := 0;
end;

procedure TdlgFindTaxon.MoveGridRow(Delta: Integer);
var
  NewRow: Integer;
begin
  if (FTaxonList.Count = 0) or (not uList.Enabled) then
    Exit;
  NewRow := uList.Row + Delta;
  if NewRow < 0 then
    NewRow := 0;
  if NewRow > uList.RowCount - 1 then
    NewRow := uList.RowCount - 1;
  uList.Row := NewRow;
end;

procedure TdlgFindTaxon.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if Key = #27 then
  begin
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    if not EP.Focused then
    begin
      EP.SelectAll;
      if EP.CanSetFocus then
        EP.SetFocus;
    end
    else
      ModalResult := mrCancel;
  end;
  { NEXT CONTROL / OK = Enter/Return }
  if (Key = #13) then
  begin
    if (EP.Focused) then
    begin
      if (qFind.RecordCount > 0) { and (DBG.CanSetFocus) } then
        uListClick(nil);
      // DBG.SetFocus;
      Key := #0;
    end
    else
    if (uList.Focused) then
    begin
      if (qFind.RecordCount > 0) then
        uListClick(nil);
      Key := #0;
    end;
  end;
end;

procedure TdlgFindTaxon.FormShow(Sender: TObject);
begin
  //PositionWindow(WindowPos, Self);
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  oOrder := COL_SCIENTIFIC_NAME;
  oDirection := 'ASC';
  EP.SetFocus;
  if Length(Init) > 0 then
  begin
    EP.Text := Init;
    EP.SelStart := Length(EP.Text);
  end;
end;

procedure TdlgFindTaxon.sbOptionsClick(Sender: TObject);
begin
  LogDebug('Search cleared');
  if qFind.Active then
    qFind.Close;
  uList.Enabled := False;
  EP.Clear;
  EP.SetFocus;
  TimerFind.Enabled := False;
end;

procedure TdlgFindTaxon.sbCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdlgFindTaxon.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  if Length(Trim(EP.Text)) = 0 then
    Exit;

  uList.Enabled := Pesquisar(EP.Text);

  LoadTaxonMetadataFromDB;
end;

procedure TdlgFindTaxon.uListClick(Sender: TObject);
begin
  if (qFind.RecordCount > 0) then
  begin
    if (UsarValido) then
    begin
      Codigo := qFind.FieldByName(COL_TAXON_ID).AsInteger;
      Nome := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, Codigo);
    end
    else
    begin
      Codigo := qFind.FieldByName(COL_TAXON_ID).AsInteger;
      Nome := qFind.FieldByName(COL_SCIENTIFIC_NAME).AsString;
    end;
    CodigoStr := IntToStr(Codigo);
  end;

  ModalResult := mrOK;
end;

// TCustomGrid.Click fires on every selection change (including keyboard), so confirm only on a real mouse click
procedure TdlgFindTaxon.uListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  C, R: Integer;
begin
  if Button <> mbLeft then
    Exit;
  uList.MouseToCell(X, Y, C, R);
  if (R >= 0) and (R < FTaxonList.Count) then
    uListClick(Sender);
end;

procedure TdlgFindTaxon.uListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
const
  CellPadding = 6;
  LineHeight = 20;

  // Truncate text with a trailing ellipsis if it doesn't fit in AMaxWidth
  function EllipsisText(ACanvas: TCanvas; const AText: String; AMaxWidth: Integer): String;
  begin
    Result := AText;
    if ACanvas.TextWidth(Result) <= AMaxWidth then
      Exit;
    while (Length(Result) > 0) and (ACanvas.TextWidth(Result + '...') > AMaxWidth) do
      Delete(Result, Length(Result), 1);
    Result := Result + '...';
  end;

var
  Grid: TDrawGrid;
  RText: TRect;
  TextTop: Integer;
  Item: TTaxonItem;
begin
  Grid := TDrawGrid(Sender);

  if (aCol < 0) then
    Exit;
  if (aRow < 0) or (aRow >= FTaxonList.Count) then
    Exit;
  Item := FTaxonList[aRow];

  // Background (highlight for selected row)
  if gdSelected in aState then
  begin
    Grid.Canvas.Brush.Color := clHighlight;
    Grid.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    Grid.Canvas.Brush.Color := clWindow;
    Grid.Canvas.Font.Color := clWindowText;
  end;
  Grid.Canvas.FillRect(aRect);

  // Square thumbnail filling the row height on the left, metadata on the right
  RText := Rect(aRect.Left + CellPadding, aRect.Top + CellPadding, aRect.Right - CellPadding, aRect.Bottom - CellPadding);

  // Metadata (everything except Subtitle), drawn to the right of the thumbnail
  Grid.Canvas.Font.Italic := True;
  Grid.Canvas.TextOut(RText.Left, RText.Top, EllipsisText(Grid.Canvas, Item.ScientificName, RText.Width));
  Grid.Canvas.Font.Italic := False;
  TextTop := RText.Top + LineHeight;

  if gdSelected in aState then
  begin
    Grid.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    Grid.Canvas.Font.Color := ActiveTheme.Foreground.TextSecondary;
  end;
  Grid.Canvas.TextOut(RText.Left, TextTop, EllipsisText(Grid.Canvas, Trim(Item.VernacularName), RText.Width));
end;

procedure TdlgFindTaxon.uListSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  if not qFind.Active then
    Exit;

  if (uList.RowCount > 0) then
    qFind.RecNo := aRow + 1;
end;

function TdlgFindTaxon.Pesquisar(aValor: String): Boolean;
var
  Criterio: TCriteriaType;
begin
  Result := False;

  Criterio := crLike;

  // aValor:= Trim(AnsiLowerCase(aValor));
  if ExecRegExpr('^#.+$', aValor) then
  begin
    PesquisarHashtag(aValor);
    // end else
    // if TRegEx.IsMatch(aValor,'^#[a-z]+:[a-z]+$',[roIgnoreCase]) then
    // begin
    // HashtagNome(aValor);
  end
  else
  begin
    if ExecRegExpr('^=.+$', aValor) then
    begin
      Criterio := crEqual;
      aValor := StringReplace(aValor, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^\$.+$', aValor) then
    begin
      Criterio := crStartLike;
      aValor := StringReplace(aValor, '$', '', [rfReplaceAll]);
    end;

    with qFind do
    begin
      Close;
      {$IFDEF DEBUG}
      LogDebug('Search: ' + aValor);
      {$ENDIF}
      SetSelect(SQL, fvReset, Criterio);
      case xSettings.VernacularNamesLanguage of
        0: ParamByName('LANG').AsInteger := GetLanguageKey(rsPortuguese);
        1: ParamByName('LANG').AsInteger := GetLanguageKey(rsEnglish);
        2: ParamByName('LANG').AsInteger := GetLanguageKey(rsSpanish);
      end;
      case Criterio of
        crLike:
          begin
            if Pos('+', aValor) > 0 then
              aValor := WildcardSyllables(aValor)
            else
              aValor := WildcardWords(aValor);
            Params.ParamByName('VALPARAM').AsString := '%' + aValor + '%';
          end;
        crStartLike:
          begin
            if Pos('+', aValor) > 0 then
              aValor := WildcardSyllables(aValor)
            else
              aValor := WildcardWords(aValor);
            Params.ParamByName('VALPARAM').AsString := aValor + '%';
          end;
        crEqual:
          Params.ParamByName('VALPARAM').AsString := aValor;
        crBetween:
          Params.ParamByName('VALPARAM').AsString := aValor;
        crMoreThan:
          Params.ParamByName('VALPARAM').AsString := aValor;
        crLessThan:
          Params.ParamByName('VALPARAM').AsString := aValor;
        crNull:
          Params.ParamByName('VALPARAM').Clear;
        crNotNull:
          Params.ParamByName('VALPARAM').Clear;
      end;

      {$IFDEF DEBUG}
      //LogSQL(SQL);
      LogDebug('VALPARAM = ' + aValor);
      {$ENDIF}
      Open;
      First;
      Result := RecordCount > 0;
    end;
  end;
end;

function TdlgFindTaxon.PesquisarHashtag(aValor: String): Boolean;
begin
  if MatchStr(aValor, HASHTAG_MARKED) then { #marcados }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValor);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvMarked, crNone);
      //{$IFDEF DEBUG}
      //LogSQL(SQL);
      //{$ENDIF}
      Open;
    end;
  end;
  if MatchStr(aValor, HASHTAG_ALL) then { #tudo }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValor);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvAll, crNone);
      //{$IFDEF DEBUG}
      //LogSQL(SQL);
      //{$ENDIF}
      Open;
    end;
  end;

  Result := qFind.RecordCount > 0;
end;

procedure TdlgFindTaxon.GetRankFilter(const aSQL: TStrings; aFilter: TTaxonFilters);
var
  F: TTaxonFilter;
  AndOr: String;
begin
  if (tfAll in aFilter) then
  begin
    Exit;
  end
  else
  if (tfMain in aFilter) then
  begin
    aSQL.Add('AND (t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
    aSQL.Add('WHERE taxon_ranks.main_rank = 1)) ');
  end
  else
  begin
    AndOr := 'AND (';
    for F in aFilter do
    begin
      case F of
        // tfKingdoms: Add('AND ((NIV_CODIGO = 1) or (NIV_CODIGO = 12)) ');
        // tfPhyla: Add('AND ((NIV_CODIGO = 2) or (NIV_CODIGO = 13)) ');
        // tfClasses: Add('AND ((NIV_CODIGO = 3) or (NIV_CODIGO = 14)) ');
        tfOrders:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation LIKE ''%ord.'')) ');
        end;
        tfFamilies:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation LIKE ''%fam.'')) ');
        end;
        tfTribes:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation LIKE ''%tr.'')) ');
        end;
        tfGenera:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation LIKE ''%g.'')) ');
        end;
        tfSpecies:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE (taxon_ranks.abbreviation = ''supersp.'') OR (taxon_ranks.abbreviation = ''sp.''))) ');
        end;
        tfSubspecies:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE (taxon_ranks.abbreviation = ''ssp.'')');
          //if not (tfSubspeciesGroups in aFilter) then
          //  aSQL.Add('OR (taxon_ranks.abbreviation = ''grp. (mono)'')');
          aSQL.Add(')) ');
        end;
        tfSubspeciesGroups:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation LIKE ''grp. %'')) ');
        end;
        tfSpuhs:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation = ''spuh'')) ');
        end;
        tfSlashes:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation = ''slash'')) ');
        end;
        tfForms:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation = ''form'')) ');
        end;
        tfDomestics:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation = ''domest.'')) ');
        end;
        tfHybrids:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation = ''hybrid'')) ');
        end;
        tfIntergrades:
        begin
          aSQL.Add(AndOr + '(t.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.abbreviation = ''intergrade'')) ');
        end;
      end;
      AndOr := 'OR ';
    end;
    aSQL.Add(')');
  end;
end;

procedure TdlgFindTaxon.LoadTaxonMetadataFromDB;
var
  Item: TTaxonItem;
begin
  FTaxonList.Clear;

  if not qFind.Active then
    Exit;

  // Avoid re-entrant DataChange events while scrolling through qFind
  qFind.DisableControls;
  try
    qFind.First;
    while not qFind.EOF do
    begin
      Item := TTaxonItem.Create;
      Item.TaxonID := qFind.FieldByName('taxon_id').AsInteger;
      Item.ScientificName := qFind.FieldByName('scientific_name').AsString;
      Item.VernacularName := qFind.FieldByName('vernacular_name').AsString;

      FTaxonList.Add(Item);
      qFind.Next;
    end;
  finally
    qFind.EnableControls;
  end;

  uList.RowCount := FTaxonList.Count;

  uList.Invalidate;
end;

procedure TdlgFindTaxon.SetSelect(const aSQL: TStrings; aFilter: TFilterValue;
  aCriteria: TCriteriaType);
var
  AD, Operador: String;
begin
  Operador := CRITERIA_OPERATORS[aCriteria];
  AD := 'ASC';

  with aSQL do
  begin
    Clear;

    // Valid taxa
    Add('SELECT t.taxon_id AS taxon_id, t.scientific_name AS scientific_name, t.formatted_name AS formatted_name,');
    Add('  v.vernacular_name AS vernacular_name');
    Add('FROM zoo_taxa AS t');
    Add('LEFT JOIN zoo_vernacular v ON v.taxon_id = t.taxon_id');
    Add('  AND v.language_id = :LANG');
    Add('  AND v.preferred = 1');
    Add('  AND v.active_status = 1');

    // Filters
    case aFilter of
      fvNone: ;
      fvReset:
        begin
          Add('WHERE ((t.scientific_name ' + Operador + ' :VALPARAM)');
          Add('   OR (t.ebird_code ' + Operador + ' :VALPARAM)');
          Add('   OR (t.quick_code ' + Operador + ' :VALPARAM))');

          if not (tfAll in FiltroTaxon) then
            GetRankFilter(aSQL, FiltroTaxon);

          Add('AND (t.active_status = 1)');
        end;
      fvAll:
        Add('WHERE (t.active_status = 1)');
      fvMarked:
        Add('WHERE (t.marked_status = 1) AND (t.active_status = 1)');
      fvDeleted:
        Add('WHERE (t.active_status = 0)');
    end;

    // Synonyms
    if xSettings.ShowSynonyms then
    begin
      Add('UNION');

      Add('SELECT s.taxon_id AS taxon_id, s.scientific_name AS scientific_name, s.formatted_name AS formatted_name,');
      Add('  v2.vernacular_name AS vernacular_name');
      Add('FROM zoo_synonyms AS s');
      Add('LEFT JOIN zoo_vernacular v2 ON v2.taxon_id = s.taxon_id');
      Add('  AND v2.language_id = :LANG');
      Add('  AND v2.preferred = 1');
      Add('  AND v2.active_status = 1');

      case aFilter of
        fvNone: ;
        fvReset:
          begin
            Add('WHERE (s.scientific_name ' + Operador + ' :VALPARAM)');
            Add('AND (s.active_status = 1)');
          end;
        fvAll:
          Add('WHERE (s.active_status = 1)');
        fvMarked:
          Add('WHERE (s.marked_status = 1) AND (s.active_status = 1)');
        fvDeleted:
          Add('WHERE (s.active_status = 0)');
      end;
    end;

    // Ordering
    if Trim(oOrder) <> '' then
    begin
      if oDirection <> '' then
        AD := oDirection;

      Add('ORDER BY ' + oOrder + ' ' + AD);
    end;
  end;
end;

procedure TdlgFindTaxon.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Width then
    Width := ControlWidth + (2 * pSearchBar.ChildSizing.LeftRightSpacing);

  if (X + Width) > Screen.Width then
    Left := (X + ControlWidth) - Width
  else
    Left := X - pSearchBar.ChildSizing.LeftRightSpacing;

  if (Y + ControlHeight + Height) > (Screen.WorkAreaHeight) then
  begin
    pSearchBar.Align := alBottom;
    Top := Y - Height + ControlHeight;
  end
  else
    Top := Y - pSearchBar.ChildSizing.TopBottomSpacing;
end;

end.

