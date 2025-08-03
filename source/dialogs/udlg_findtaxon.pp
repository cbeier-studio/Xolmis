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
  Classes, SysUtils, DB, SQLDB, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls,
  DBControlGrid, StdCtrls, Buttons, BCPanel, ColorSpeedButton, HtmlView, RegExpr, StrUtils,
  cbs_system, cbs_taxonomy, cbs_datatypes, Grids, HTMLUn2, HtmlGlobals;

type

  { TdlgFindTaxon }

  TdlgFindTaxon = class(TForm)
    iButtons: TImageList;
    iButtonsDark: TImageList;
    lblName: THtmlViewer;
    lblVernacular: TDBText;
    dsFind: TDataSource;
    EP: TEdit;
    lblName1: TDBText;
    pSearchBar: TPanel;
    pContent: TPanel;
    pEP: TBCPanel;
    qFind: TSQLQuery;
    sbOptions: TColorSpeedButton;
    sbClose: TColorSpeedButton;
    TimerFind: TTimer;
    uList: TDBControlGrid;
    procedure EPChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure lblNameEnter(Sender: TObject);
    procedure pContentClick(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure uListPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
    xCod: Integer;
    xNome, xCodStr, xInit, xFiltro: String;
    xTaxFiltro: TTaxonFilters;
    xValid: Boolean;
    oOrder, oDirection: String;
    function Pesquisar(aValor: String): Boolean;
    function PesquisarHashtag(aValor: String): Boolean;
    procedure ApplyDarkMode;
    procedure GetRankFilter(const aSQL: TStrings; aFilter: TTaxonFilters);
    procedure SetSelect(const aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
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
  cbs_global, cbs_getvalue, cbs_conversions, cbs_themes, uDarkStyleParams;

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

  uList.AlternateColor := clSolidBGSecondaryDark;
  lblVernacular.Font.Color := clGray;
end;

procedure TdlgFindTaxon.EPChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;
  if Length(EP.Text) = 0 then
  //begin
  //  EP.RightButton.Visible := True;
  //end
  //else
  begin
    //EP.RightButton.Visible := False;
    qFind.Close;
  end;
end;

procedure TdlgFindTaxon.FormCreate(Sender: TObject);
begin
  xNome := EmptyStr;
  xCodStr := EmptyStr;
end;

procedure TdlgFindTaxon.FormDestroy(Sender: TObject);
begin
  // Fecha tabelas
  if qFind.Active then
    qFind.Close;
end;

procedure TdlgFindTaxon.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) then { MOVER SELECAO PARA BAIXO = Down }
  begin
    // if DBG.Enabled then
    // begin
    // if DBG.SelectedRow < DBG.RowCount-1 then
    // DBG.SelectedRow:= DBG.SelectedRow + 1;
    // end;
    if (qFind.RecordCount > 0) and (qFind.RecNo < qFind.RecordCount) then
      qFind.Next;
    Key := 0;
  end;
  if (Key = VK_UP) then { MOVER SELECAO PARA CIMA = Up }
  begin
    // if DBG.Enabled then
    // begin
    // if DBG.SelectedRow > 0 then
    // DBG.SelectedRow:= DBG.SelectedRow - 1;
    // end;
    if (qFind.RecordCount > 0) and (qFind.RecNo > 0) then
      qFind.Prior;
    Key := 0;
  end;
  if (Key = VK_NEXT) then { MOVER SELECAO PARA BAIXO = PageDown }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo < qFind.RecordCount) then
      qFind.MoveBy(8);
    Key := 0;
  end;
  if (Key = VK_PRIOR) then { MOVER SELECAO PARA CIMA = PageUp }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo > 0) then
      qFind.MoveBy(-8);
    Key := 0;
  end;
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
        pContentClick(nil);
      // DBG.SetFocus;
      Key := #0;
    end
    else
    if (uList.Focused) then
    begin
      if (qFind.RecordCount > 0) then
        pContentClick(nil);
      Key := #0;
    end;
  end;
end;

procedure TdlgFindTaxon.FormShow(Sender: TObject);
var
  Verna: Integer;
begin
  //PositionWindow(WindowPos, Self);
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  Verna := xSettings.VernacularNamesLanguage;
  case Verna of
    0:
      lblVernacular.DataField := 'portuguese_name';
    1:
      lblVernacular.DataField := 'english_name';
    2:
      lblVernacular.DataField := 'spanish_name';
  end;
  oOrder := 'full_name';
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

  uList.Enabled := Pesquisar(EP.Text);
end;

procedure TdlgFindTaxon.uListPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  //if (gdSelected in aState) or (gdFocused in aState) then
  //  pContent.Color := clHighlight
  //else
  //  pContent.Color := clWhite;
  lblName.DefBackground := pContent.Color;

  if qFind.Active then
    lblName.LoadFromString(qFind.FieldByName('formatted_name').AsString)
  else
    lblName.Clear;
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
    if ExecRegExpr('^:.+$', aValor) then
    begin
      Criterio := crStartLike;
      aValor := StringReplace(aValor, ':', '', [rfReplaceAll]);
    end;

    with qFind do
    begin
      Close;
      {$IFDEF DEBUG}
      LogDebug('Search: ' + aValor);
      {$ENDIF}
      SetSelect(SQL, fvReset, Criterio);
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
    aSQL.Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
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
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym LIKE ''%ord.'')) ');
        end;
        tfFamilies:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym LIKE ''%fam.'')) ');
        end;
        tfTribes:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym LIKE ''%tr.'')) ');
        end;
        tfGenera:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym LIKE ''%g.'')) ');
        end;
        tfSpecies:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE (taxon_ranks.rank_acronym = ''supersp.'') OR (taxon_ranks.rank_acronym = ''sp.''))) ');
        end;
        tfSubspecies:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE (taxon_ranks.rank_acronym = ''ssp.'')');
          //if not (tfSubspeciesGroups in aFilter) then
          //  aSQL.Add('OR (taxon_ranks.rank_acronym = ''grp. (mono)'')');
          aSQL.Add(')) ');
        end;
        tfSubspeciesGroups:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym LIKE ''grp. %'')) ');
        end;
        tfSpuhs:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym = ''spuh'')) ');
        end;
        tfSlashes:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym = ''slash'')) ');
        end;
        tfForms:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym = ''form'')) ');
        end;
        tfDomestics:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym = ''domest.'')) ');
        end;
        tfHybrids:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym = ''hybrid'')) ');
        end;
        tfIntergrades:
        begin
          aSQL.Add(AndOr + '(zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
          aSQL.Add('WHERE taxon_ranks.rank_acronym = ''intergrade'')) ');
        end;
      end;
      AndOr := 'OR ';
    end;
    aSQL.Add(')');
  end;
end;

procedure TdlgFindTaxon.lblNameEnter(Sender: TObject);
begin
  pContentClick(Sender);
end;

procedure TdlgFindTaxon.pContentClick(Sender: TObject);
begin
  if (qFind.RecordCount > 0) then
  begin
    if (UsarValido) and (qFind.FieldByName('valid_id').AsInteger > 0) then
    begin
      Codigo := qFind.FieldByName('valid_id').AsInteger;
      Nome := GetName('zoo_taxa', 'full_name', 'taxon_id', Codigo);
    end
    else
    begin
      Codigo := qFind.FieldByName('taxon_id').AsInteger;
      Nome := qFind.FieldByName('full_name').AsString;
    end;
    CodigoStr := IntToStr(Codigo);
  end;

  ModalResult := mrOK;
end;

procedure TdlgFindTaxon.SetSelect(const aSQL: TStrings; aFilter: TFilterValue;
  aCriteria: TCriteriaType);
var
  AD, Operador: String;
begin
  Operador := CriteriaOperators[aCriteria];
  AD := 'ASC';

  with aSQL do
  begin
    Clear;
    Add('SELECT taxon_id, full_name, formatted_name, valid_id, english_name, portuguese_name, spanish_name');
    Add('FROM zoo_taxa ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE ((full_name ' + Operador + ' :VALPARAM) ');
          Add('OR (english_name ' + Operador + ' :VALPARAM) ');
          Add('OR (ioc_english_name ' + Operador + ' :VALPARAM) ');
          Add('OR (portuguese_name ' + Operador + ' :VALPARAM) ');
          Add('OR (spanish_name ' + Operador + ' :VALPARAM) ');
          Add('OR (other_portuguese_names ' + Operador + ' :VALPARAM) ');
          Add('OR (ebird_code ' + Operador + ' :VALPARAM) ');
          Add('OR (quick_code ' + Operador + ' :VALPARAM)) ');

          if not (tfAll in FiltroTaxon) then
            GetRankFilter(aSQL, FiltroTaxon);
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;

    case xSettings.Taxonomy of
      0: Add('AND (clements_taxonomy = 1)');
      1: Add('AND (ioc_taxonomy = 1)');
      2: Add('AND (cbro_taxonomy = 1)');
    end;
    if not xSettings.ShowSynonyms then
      Add('AND ((valid_id = 0) OR (valid_id ISNULL))');

    if Trim(oOrder) <> EmptyStr then
    begin
      if oDirection <> EmptyStr then
        AD := oDirection;
      Add('ORDER BY ' + oOrder + {' collate pt_BR ' +} ' ' + AD);
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

