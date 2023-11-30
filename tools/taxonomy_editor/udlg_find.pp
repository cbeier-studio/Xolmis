unit udlg_find;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLType,
  StdCtrls, Buttons, Grids, DBGrids, BCPanel, RegExpr, StrUtils,
  lib_taxa;

type

  { TdlgFind }

  TdlgFind = class(TForm)
    uList: TDBGrid;
    dsFind: TDataSource;
    EP: TEdit;
    pEP: TBCPanel;
    sbClearSearch: TSpeedButton;
    sbClose: TSpeedButton;
    pHeader: TPanel;
    qFind: TSQLQuery;
    TimerFind: TTimer;
    procedure EPChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure uListCellClick(Column: TColumn);
  private
    FTableType: TTableType;
    FKeySelected: Integer;
    FNameSelected, FKeyStr, FInitial, FFilter: String;
    FTaxonFilter: TTaxonFilters;
    FSortField, FSortDirection: String;
    FKeyField, FFullNameField, FFormattedNameField: String;
    function Search(aValue: String): Boolean;
    function HashtagFilter(aValue: String): Boolean;
    procedure SetSelect(const aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure SetupFields(aKeyField, aNameField: String; aFormattedNameField: String = '');
    function GetCriteria(aCriteria: TCriteriaType): String;
    procedure FindTaxonRanks(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure FindTaxa(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
    procedure SetupResult(aKeyField, aNameField: String);
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property TableType: TTableType read FTableType write FTableType;
    property KeySelected: Integer read FKeySelected write FKeySelected default 0;
    property KeySelectedStr: String read FKeyStr write FKeyStr;
    property NameSelected: String read FNameSelected write FNameSelected;
    property InitialValue: String read FInitial write FInitial;
    property Filter: String read FFilter write FFilter;
    property TaxonFilter: TTaxonFilters read FTaxonFilter write FTaxonFilter;
  end;

var
  dlgFind: TdlgFind;

implementation

{$R *.lfm}

{ TdlgFind }

procedure TdlgFind.EPChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  if Length(EP.Text) = 0 then
  begin
    sbClearSearch.Visible := True;
  end
  else
  begin
    sbClearSearch.Visible := False;
    qFind.Close;
  end;
end;

procedure TdlgFind.FindTaxa(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
  F: TTaxonFilter;
begin
  Operador := GetCriteria(aCriteria);

  with aSQL do
  begin
    Add('SELECT taxon_id, full_name, formatted_name, valid_id FROM zoo_taxa ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE (full_name ' + Operador + ' :VALPARAM) ');
          if not (tfAll in FTaxonFilter) then
          begin
            if (tfMain in FTaxonFilter) then
            begin
              Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
              Add('WHERE taxon_ranks.main_rank = 1)) ');
            end
            else
            begin
              for F in FTaxonFilter do
              begin
                case F of
                  // tfKingdoms: Add('AND ((NIV_CODIGO = 1) or (NIV_CODIGO = 12)) ');
                  // tfPhyla: Add('AND ((NIV_CODIGO = 2) or (NIV_CODIGO = 13)) ');
                  // tfClasses: Add('AND ((NIV_CODIGO = 3) or (NIV_CODIGO = 14)) ');
                  tfOrders:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%ord.'')) ');
                  end;
                  tfFamilies:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%fam.'')) ');
                  end;
                  tfTribes:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%tr.'')) ');
                  end;
                  tfGenera:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''%g.'')) ');
                  end;
                  tfSpecies:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE (taxon_ranks.rank_acronym = ''supersp.'') OR (taxon_ranks.rank_acronym = ''sp.''))) ');
                  end;
                  tfSubspecies:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE (taxon_ranks.rank_acronym = ''ssp.'')');
                    if not (tfSubspeciesGroups in FTaxonFilter) then
                      Add('OR (taxon_ranks.rank_acronym = ''grp. (mono)'')');
                    Add(')) ');
                  end;
                  tfSubspeciesGroups:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym LIKE ''grp. %'')) ');
                  end;
                  tfSpuhs:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''spuh'')) ');
                  end;
                  tfSlashes:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''slash'')) ');
                  end;
                  tfForms:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''form'')) ');
                  end;
                  tfDomestics:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''domest.'')) ');
                  end;
                  tfHybrids:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''hybrid'')) ');
                  end;
                  tfIntergrades:
                  begin
                    Add('AND (zoo_taxa.rank_id IN (SELECT taxon_ranks.rank_id FROM taxon_ranks ');
                    Add('WHERE taxon_ranks.rank_acronym = ''intergrade'')) ');
                  end;
                end;
              end;
            end;
          end;
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FindTaxonRanks(aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  Operador: String;
begin
  Operador := CriteriaOperators[aCriteria];

  with aSQL do
  begin
    Add('SELECT rank_id, rank_name FROM taxon_ranks ');
    case aFilter of
      fvNone:
        ; // do nothing
      fvReset:
        begin
          Add('WHERE ((rank_name ' + Operador + ' :VALPARAM) ');
          Add('OR (rank_acronym ' + Operador + ' :VALPARAM)) ');
          Add('AND (active_status = 1)');
        end;
      fvAll:
        Add('WHERE (active_status = 1)');
      fvMarked:
        Add('WHERE (marked_status = 1) AND (active_status = 1)');
      fvDeleted:
        Add('WHERE (active_status = 0)');
    end;
  end;
end;

procedure TdlgFind.FormCreate(Sender: TObject);
begin
  FKeyStr := EmptyStr;
  FNameSelected := EmptyStr;
end;

procedure TdlgFind.FormDestroy(Sender: TObject);
begin
  // Close table
  if qFind.Active then
    qFind.Close;
end;

procedure TdlgFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) then { MOVE SELECTION DOWN = Down }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo < qFind.RecordCount) then
      qFind.Next;
    Key := 0;
  end;
  if (Key = VK_UP) then { MOVE SELECTION UP = Up }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo > 0) then
      qFind.Prior;
    Key := 0;
  end;
  if (Key = VK_NEXT) then { MOVE SELECTION A PAGE DOWN = PageDown }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo < qFind.RecordCount) then
      qFind.MoveBy(8);
    Key := 0;
  end;
  if (Key = VK_PRIOR) then { MOVE SELECTION A PAGE UP = PageUp }
  begin
    if (qFind.RecordCount > 0) and (qFind.RecNo > 0) then
      qFind.MoveBy(-8);
    Key := 0;
  end;
end;

procedure TdlgFind.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCELAR = Esc }
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
  { PROXIMO CAMPO / OK = Enter }
  if (Key = #13) then
  begin
    if (EP.Focused) or (uList.Focused) then
    begin
      if (qFind.RecordCount > 0) { and (DBG.CanSetFocus) } then
        uListCellClick(uList.Columns.Items[0]);
      // DBG.SetFocus;
      Key := #0;
    end;
  end;
end;

procedure TdlgFind.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcSmall);
  {$ENDIF}

  if FTableType = tbNone then
  begin
    MsgDlg(rsCaptionFind, Format(rsErrorTableNotFound, [TableNames[FTableType]]), mtError);
    ModalResult := mrCancel;
  end;

  case FTableType of
    tbTaxonRanks:
      SetupFields('rank_id', 'rank_name');
    tbZooTaxa:
      SetupFields('zoo_taxa', 'full_name', 'formatted_name');
  end;

  FSortDirection := 'ASC';
  EP.SetFocus;
  if Length(FInitial) > 0 then
  begin
    EP.Text := FInitial;
    EP.SelStart := Length(EP.Text);
  end;

  if (FFormattedNameField <> EmptyStr) then
    uList.Columns[0].FieldName := FFormattedNameField
  else
    uList.Columns[0].FieldName := FFullNameField;
end;

function TdlgFind.GetCriteria(aCriteria: TCriteriaType): String;
begin
  Result := CriteriaOperators[aCriteria];
end;

function TdlgFind.HashtagFilter(aValue: String): Boolean;
begin
  if MatchStr(aValue, MarkedQS) then { #marked }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValue);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvMarked, crNone);
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}
      Open;
    end;
  end;
  if MatchStr(aValue, AllQS) then { #all }
  begin
    {$IFDEF DEBUG}
    LogDebug('Search hashtag: ' + aValue);
    {$ENDIF}
    with qFind, SQL do
    begin
      Close;
      SetSelect(SQL, fvAll, crNone);
      {$IFDEF DEBUG}
      LogSQL(SQL);
      {$ENDIF}
      Open;
    end;
  end;

  Result := qFind.RecordCount > 0;
end;

procedure TdlgFind.sbClearSearchClick(Sender: TObject);
begin
  {$IFDEF DEBUG}
  LogDebug('Clear search');
  {$ENDIF}
  if qFind.Active then
    qFind.Close;
  uList.Enabled := False;
  EP.Clear;
  EP.SetFocus;
  TimerFind.Enabled := False;
end;

procedure TdlgFind.sbCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TdlgFind.Search(aValue: String): Boolean;
var
  Criterio: TCriteriaType;
begin
  Criterio := crLike;
  if ExecRegExpr('^#.+$', aValue) then
  begin
    HashtagFilter(aValue);
    // end else
    // if ExecRegExpr('^#[a-z]+:[a-z]+$', aValue) then
    // begin
    // HashtagNome(aValue);
  end
  else
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Criterio := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Criterio := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    with qFind do
    begin
      Close;
      {$IFDEF DEBUG}
      LogDebug('Search value: ' + aValue);
      {$ENDIF}
      SetSelect(SQL, fvReset, Criterio);
      case Criterio of
        crLike:
          begin
            if Pos('+', aValue) > 0 then
              aValue := WildcardSyllables(aValue)
            else
              aValue := WildcardWords(aValue);
            Params.ParamByName('VALPARAM').AsString := '%' + aValue + '%';
          end;
        crStartLike:
          begin
            if Pos('+', aValue) > 0 then
              aValue := WildcardSyllables(aValue)
            else
              aValue := WildcardWords(aValue);
            Params.ParamByName('VALPARAM').AsString := aValue + '%';
          end;
        crEqual:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crBetween:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crMoreThan:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crLessThan:
          Params.ParamByName('VALPARAM').AsString := aValue;
        crNull:
          Params.ParamByName('VALPARAM').Clear;
        crNotNull:
          Params.ParamByName('VALPARAM').Clear;
      end;

      {$IFDEF DEBUG}
      LogSQL(SQL);
      LogDebug('SQL: VALPARAM = ' + aValue);
      {$ENDIF}
      Open;
      First;
      Result := RecordCount > 0;
    end;
  end;
end;

procedure TdlgFind.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth + (pHeader.ChildSizing.LeftRightSpacing * 2);

  if (X + Self.Width) > Screen.WorkAreaWidth then
    Self.Left := X - Self.Width
  else
    Self.Left := X - pHeader.ChildSizing.LeftRightSpacing;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y - pHeader.ChildSizing.TopBottomSpacing;
end;

procedure TdlgFind.SetSelect(const aSQL: TStrings; aFilter: TFilterValue; aCriteria: TCriteriaType);
var
  AD: String;
begin
  AD := 'ASC';

  with aSQL do
  begin
    Clear;

    case FTableType of
      tbTaxonRanks:
        FindTaxonRanks(aSQL, aFilter, aCriteria);
      tbZooTaxa:
        FindTaxa(aSQL, aFilter, aCriteria);
    end;

    if Trim(FSortField) <> EmptyStr then
    begin
      if FSortDirection <> EmptyStr then
        AD := FSortDirection;
      Add('ORDER BY ' + FSortField + {' collate pt_BR ' +} ' ' + AD);
    end;
  end;
end;

procedure TdlgFind.SetupFields(aKeyField, aNameField: String; aFormattedNameField: String);
begin
  Caption := Format('%s %s', [Caption, TableNames[FTableType]]);
  FKeyField := aKeyField;
  FFullNameField := aNameField;
  FFormattedNameField := aFormattedNameField;
  FSortField := aNameField;
end;

procedure TdlgFind.SetupResult(aKeyField, aNameField: String);
begin
  FKeySelected := qFind.FieldByName(aKeyField).AsInteger;
  FNameSelected := qFind.FieldByName(aNameField).AsString;

  if (FTableType = tbZooTaxa) then
    FNameSelected := GetName('zoo_taxa', 'full_name', 'taxon_id', FKeySelected);
end;

procedure TdlgFind.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  uList.Enabled := Search(EP.Text);
end;

procedure TdlgFind.uListCellClick(Column: TColumn);
begin
  if (qFind.RecordCount > 0) then
  begin
    case FTableType of
      tbTaxonRanks:
        SetupResult('rank_id', 'rank_name');
      tbZooTaxa:
        if (qFind.FieldByName('valid_id').AsInteger > 0) then
          SetupResult('valid_id', 'full_name')
        else
          SetupResult('taxon_id', 'full_name');
    end;

    FKeyStr := IntToStr(FKeySelected);
  end;
  ModalResult := mrOK;
end;

end.

