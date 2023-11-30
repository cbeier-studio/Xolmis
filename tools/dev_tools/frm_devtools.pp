unit frm_devtools;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Buttons, EditBtn,
  DBCtrls, StdCtrls, CheckLst, JvDBHTLabel, DBGrids, rxswitch,
  DBControlGrid, ExtendedNotebook, Types, RegExpr, dev_types, attabs;

type

  { TfrmDevTools }

  TfrmDevTools = class(TForm)
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    DBGrid5: TDBGrid;
    btnParar: TBitBtn;
    btnNumInterno: TButton;
    btnSepararNomes: TButton;
    btnFormatarNomes: TButton;
    btnHierarquia: TButton;
    btnGeneros: TButton;
    btnQuickCode: TButton;
    btnForeignNumInterno: TButton;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBCheckBox3: TDBCheckBox;
    DBCheckBox4: TDBCheckBox;
    DBCheckBox5: TDBCheckBox;
    DBCheckBox6: TDBCheckBox;
    DBCheckBox7: TDBCheckBox;
    DBCheckBox8: TDBCheckBox;
    ckMarcadoTaxa: TDBCheckBox;
    DBComboBox1: TDBComboBox;
    DBComboBox2: TDBComboBox;
    cbTipoValor: TDBComboBox;
    cbTipoCampo: TDBComboBox;
    DBComboBox3: TDBComboBox;
    DBComboBox4: TDBComboBox;
    DBComboBox5: TDBComboBox;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    DBEdit7: TDBEdit;
    DBMemo1: TDBMemo;
    eFindM5: TEditButton;
    navMethods: TDBNavigator;
    navTabs: TATTabs;
    navReport: TDBNavigator;
    navTabela: TDBNavigator;
    navCampo: TDBNavigator;
    eFindM4: TEditButton;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pDBTaxa: TPanel;
    pTopMethods: TPanel;
    peCampos: TPanel;
    pCampos: TPanel;
    pToolsCampos: TPanel;
    sboxCampos: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    LS: TLabel;
    lblRecCount: TLabel;
    Panel1: TPanel;
    pTabela: TPanel;
    peTabela: TPanel;
    pToolsTab: TPanel;
    PG: TExtendedNotebook;
    pMethods4: TPanel;
    pMethods5: TPanel;
    PBar: TProgressBar;
    splitCampoEdit: TSplitter;
    splitTabCampo: TSplitter;
    TimerFind: TTimer;
    tsDataMap: TTabSheet;
    tsReports: TTabSheet;
    tsBatch: TTabSheet;
    tsMethods: TTabSheet;
    procedure ckRankFilterClickCheck(Sender: TObject);
    procedure etAutoriaChange(Sender: TObject);
    procedure etAutoriaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure etNomeExit(Sender: TObject);
    procedure etNomeKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbFiltersTaxaClick(Sender: TObject);
    procedure sbSuperiorCBROClick(Sender: TObject);
    procedure sbSuperiorClick(Sender: TObject);
    procedure sbSuperiorIOCClick(Sender: TObject);
    procedure sbtClearFiltersClick(Sender: TObject);
    procedure sbValidoCBROClick(Sender: TObject);
    procedure sbValidoClick(Sender: TObject);
    procedure sbValidoIOCClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure tsCBROFilterOff(Sender: TObject);
    procedure tsCBROFilterOn(Sender: TObject);
    procedure tsEbirdFilterOff(Sender: TObject);
    procedure tsEbirdFilterOn(Sender: TObject);
    procedure tsExtinctFilterOff(Sender: TObject);
    procedure tsExtinctFilterOn(Sender: TObject);
    procedure tsfDesmarcadosOff(Sender: TObject);
    procedure tsfDesmarcadosOn(Sender: TObject);
    procedure tsfMarcadosOff(Sender: TObject);
    procedure tsfMarcadosOn(Sender: TObject);
    procedure tsIOCFilterOff(Sender: TObject);
    procedure tsIOCFilterOn(Sender: TObject);
    procedure tsSynonymFilterOff(Sender: TObject);
    procedure tsSynonymFilterOn(Sender: TObject);
  private
    Parar: Boolean;
    CodAdm,CodCB: LargeInt;
    SearchStr: TSearch;
    QuickFilters: TStringList;
    Modifier: TRecordStatus;
    Ordenacao: TSortedFields;
    WhereSQL: TStrings;
    Filtrado: Boolean;
    NivelFilter, EbirdFilter, IocFilter, CbroFilter: String;
    ExtintoFilter, SinonimoFilter, ValidoFilter: String;
    SkipCompleteAutor: Boolean;
    procedure GetFiltersTaxa(aList: TStrings);
    function PesquisarTaxa(aValor: String): Boolean;
    procedure ReiniciaTaxa;
    procedure RoundPanels;
    procedure InitTaxa;
    procedure FreeTaxa;
    function ValidaTaxa: Boolean;
  public

  end;

var
  frmDevTools: TfrmDevTools;

implementation

uses dm.dev, dm.taxa;

{$R *.lfm}

{ TfrmDevTools }

procedure TfrmDevTools.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  case PG.ActivePageIndex of
    0: PesquisarTaxa(eFindTaxa.Text);
  end;
end;

procedure TfrmDevTools.tsCBROFilterOff(Sender: TObject);
begin
  CbroFilter := '';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsCBROFilterOn(Sender: TObject);
begin
  CbroFilter := '(TAX_NOME_CBRO = 1)';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsEbirdFilterOff(Sender: TObject);
begin
  EbirdFilter := '';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsEbirdFilterOn(Sender: TObject);
begin
  EbirdFilter := '(TAX_NOME_EBIRD = 1)';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsExtinctFilterOff(Sender: TObject);
begin
  ExtintoFilter := '';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsExtinctFilterOn(Sender: TObject);
begin
  ExtintoFilter := '(TAX_EXTINCT = 1)';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsfDesmarcadosOff(Sender: TObject);
begin
  if not(tsfMarcados.StateOn = sw_on) and not(tsfDesmarcados.StateOn = sw_on) then
    Modifier.Mark := rmAll;

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsfDesmarcadosOn(Sender: TObject);
begin
  Modifier.Mark := rmUnmarked;
  tsfMarcados.StateOn := sw_off;

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsfMarcadosOff(Sender: TObject);
begin
  if not(tsfMarcados.StateOn = sw_on) and not(tsfDesmarcados.StateOn = sw_on) then
    Modifier.Mark := rmAll;

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsfMarcadosOn(Sender: TObject);
begin
  Modifier.Mark := rmMarked;
  tsfDesmarcados.StateOn := sw_off;

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsIOCFilterOff(Sender: TObject);
begin
  IocFilter := '';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsIOCFilterOn(Sender: TObject);
begin
  IocFilter := '(TAX_NOME_IOC = 1)';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsSynonymFilterOff(Sender: TObject);
begin
  SinonimoFilter := '';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.tsSynonymFilterOn(Sender: TObject);
begin
  SinonimoFilter := '(TAX_VALIDO > 0)';

  PesquisarTaxa(eFindTaxa.Text);
end;

procedure TfrmDevTools.GetFiltersTaxa(aList: TStrings);
begin
  aList.Clear;
  if NivelFilter <> '' then
    aList.Add(NivelFilter);
  if EbirdFilter <> '' then
    aList.Add(EbirdFilter);
  if IocFilter <> '' then
    aList.Add(IocFilter);
  if CbroFilter <> '' then
    aList.Add(CbroFilter);
  if ExtintoFilter <> '' then
    aList.Add(ExtintoFilter);
  if SinonimoFilter <> '' then
    aList.Add(SinonimoFilter);
  if ValidoFilter <> '' then
    aList.Add(ValidoFilter);

  Filtrado := aList.Count > 0;

  //if Filtrado = True then
  //  sbFiltrosNome.ImageName := 'funnel-flash-sel'
  //else
  //  sbFiltrosNome.ImageName := 'funnel-flash';
end;

function TfrmDevTools.PesquisarTaxa(aValor: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  SearchStr.Clear;
  //sbRestoreNome.Visible := False;
  //GravaLog('SEARCH', aValor);

  SearchStr.Criteria := crLike;
  aValor := Trim(aValor);

  if aValor <> '' then
  begin
    if ExecRegExpr('^=[a-z0-9/\[\]\(\)\-'#$2012'.+ ]+$', aValor) then
    begin
      SearchStr.Criteria := crEqual;
      aValor := StringReplace(aValor, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:[a-z0-9/\[\]\(\)\-'#$2012'.+ ]+$', aValor) then
    begin
      SearchStr.Criteria := crStartLike;
      aValor := StringReplace(aValor, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValor, i) then
    begin
      SearchStr.FieldNames.Add('reg_num_interno');
      SearchStr.FilterType := tcInteiro;
    end
    else
    begin
      SearchStr.FieldNames.Add('TAX_NOME');
      SearchStr.FieldNames.Add('TAX_ENGLISH');
      SearchStr.FieldNames.Add('TAX_ENGLISH_IOC');
      SearchStr.FieldNames.Add('TAX_SPANISH');
      SearchStr.FieldNames.Add('TAX_PORTUGUES');
      SearchStr.FieldNames.Add('TAX_PORTUGUES_OUTROS');
      SearchStr.FieldNames.Add('TAX_EBIRD_CODE');
      SearchStr.FieldNames.Add('TAX_QUICKCODE');
      SearchStr.FilterType := tcTexto;
    end;

    SearchStr.Value1 := aValor;
  end;

  Result := TableSearch(DMT.qTaxa, tbTaxa, SearchStr, QuickFilters, Modifier, Ordenacao, WhereSQL);
end;

procedure TfrmDevTools.ReiniciaTaxa;
var
  i: Integer;
begin
  NivelFilter := '';
  EbirdFilter := '';
  IocFilter := '';
  CbroFilter := '';
  ExtintoFilter := '';
  SinonimoFilter := '';
  ValidoFilter := '';
  for i := 0 to ckRankFilter.Count - 1 do
    ckRankFilter.CheckAll(TCheckBoxState.cbUnchecked, False, False);
  lblCountRankFilter.Caption := 'nenhum selecionado';
  tsEbirdFilter.StateOn := sw_off;
  tsIOCFilter.StateOn := sw_off;
  tsCBROFilter.StateOn := sw_off;
  tsExtinctFilter.StateOn := sw_off;
  tsSynonymFilter.StateOn := sw_off;
  tsValidFilter.StateOn := sw_off;
  //EP.Font.Color := clNavy;

  SearchStr.Clear;
  Modifier.Clear;

  TableSearch(DMT.qTaxa, tbTaxa, SearchStr, QuickFilters, Modifier, Ordenacao, WhereSQL);
end;

procedure TfrmDevTools.RoundPanels;
begin
  MakeRounded(pMarkedFilter);
  MakeRounded(pLixoFilters);

  MakeRounded(pRanksFilters);
  MakeRounded(pTaxonomiasFilters);
  MakeRounded(pIUCNFilters);
  MakeRounded(pSinonimosFilters);
end;

procedure TfrmDevTools.InitTaxa;
begin
  if pFilterTaxa.Visible then
    pFilterTaxa.Visible := False;
  sboxTaxaFilters.VertScrollBar.Position := 0;

  SearchStr.Create;
  SetLength(Ordenacao, 1);
  Ordenacao[0].FieldName := 'TAX_NOME';
  Ordenacao[0].Direction := sdAscending;
  Modifier.Clear;
  Filtrado := False;
  QuickFilters := TStringList.Create;
  WhereSQL := TStringList.Create;

  NivelFilter := '';
  EbirdFilter := '';
  IocFilter := '';
  CbroFilter := '';
  ExtintoFilter := '';
  SinonimoFilter := '';
  ValidoFilter := '';
end;

procedure TfrmDevTools.FreeTaxa;
begin
  Finalize(Ordenacao);
  FreeAndNil(QuickFilters);
  FreeAndNil(WhereSQL);
  SearchStr.Free;
end;

function TfrmDevTools.ValidaTaxa: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Campos obrigatórios
  //RequiredIsEmpty(DMT.qTaxa, 'AUX_TAXONS', 'TAX_NOME', Msgs);
  //RequiredIsEmpty(DMT.qTaxa, 'AUX_TAXONS', 'TAX_NIVEL', Msgs);

  // Registro duplicado
  //RegistroDuplicado('AUX_TAXONS', 'TAX_NOME', DMT.qTaxaTAX_NOME.AsString, DMT.qTaxaREG_NUM_INTERNO.AsLargeInt);

  // Chaves estrangeiras
  //ForeignValueExists('AUX_TAXONS', 'reg_num_interno', DMT.qTaxaTAX_SUPERIOR.AsLargeInt, Msgs, 'Nível superior');
  //ForeignValueExists('AUX_TAXONS', 'reg_num_interno', DMT.qTaxaTAX_VALIDO.AsLargeInt, Msgs, 'Nível válido');

  if Msgs.Count > 0 then
  begin
    Result := False;
    //ValidaDlg(Msgs);
  end;
  Msgs.Free;
end;

procedure TfrmDevTools.FormShow(Sender: TObject);
begin
  PG.ActivePageIndex := 0;
end;

procedure TfrmDevTools.sbFiltersTaxaClick(Sender: TObject);
begin
  pFilterTaxa.Visible := sbFiltersTaxa.Down;
end;

procedure TfrmDevTools.sbSuperiorCBROClick(Sender: TObject);
var
  Fltr: TTaxonFilter;
begin
  if (cbNivelCBRO.Text = 'Variedade') or (cbNivelCBRO.Text = 'Subvariedade') or
    (cbNivelCBRO.Text = 'Forma') or (cbNivelCBRO.Text = 'Subforma') then
    Fltr := tfSppInfrasp
  else
  if (cbNivelCBRO.Text = 'Subesp'#233'cie') then
    Fltr := tfSpecies
  else
  if (cbNivelCBRO.Text = 'Esp'#233'cie') or (cbNivelCBRO.Text = 'Subg'#234'nero') then
    Fltr := tfGenera
  else
  if (cbNivelCBRO.Text = 'G'#234'nero') then
    Fltr := tfFamTribes
  else
  if (cbNivelCBRO.Text = 'Subtribo') then
    Fltr := tfTribes
  else
  if (cbNivelCBRO.Text = 'Tribo') or (cbNivelCBRO.Text = 'Subfam'#237'lia') then
    Fltr := tfFamilies
  else
  if (cbNivelCBRO.Text = 'Fam'#237'lia') or (cbNivelCBRO.Text = 'Subordem') then
    Fltr := tfOrders
  else
  if (cbNivelCBRO.Text = 'Ordem') or (cbNivelCBRO.Text = 'Superordem') or (cbNivelCBRO.Text = 'Subclasse') then
    Fltr := tfClasses
  else
  if (cbNivelCBRO.Text = 'Classe') or (cbNivelCBRO.Text = 'Subdivis'#227'o') then
    Fltr := tfPhyla
  else
  if (cbNivelCBRO.Text = 'Divis'#227'o') or (cbNivelCBRO.Text = 'Subreino') then
    Fltr := tfKingdoms
  else
    Fltr := tfNone;

  if BuscarTaxon('', Fltr, etSuperiorCBRO, DMT.qTaxa, 'TAX_SUPERIOR_CBRO', 'NOME_SUPERIOR_CBRO', False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmDevTools.sbSuperiorClick(Sender: TObject);
var
  Fltr: TTaxonFilter;
begin
  if (cbNivel.Text = 'Variedade') or (cbNivel.Text = 'Subvariedade') or
    (cbNivel.Text = 'Forma') or (cbNivel.Text = 'Subforma') then
    Fltr := tfSppInfrasp
  else
  if (cbNivel.Text = 'Subesp'#233'cie') then
    Fltr := tfSpeciesGrp
  else
  if (cbNivel.Text = 'Esp'#233'cie') or (cbNivel.Text = 'Subg'#234'nero') then
    Fltr := tfGenera
  else
  if (cbNivel.Text = 'G'#234'nero') then
    Fltr := tfFamTribes
  else
  if (cbNivel.Text = 'Subtribo') then
    Fltr := tfTribes
  else
  if (cbNivel.Text = 'Tribo') or (cbNivel.Text = 'Subfam'#237'lia') then
    Fltr := tfFamilies
  else
  if (cbNivel.Text = 'Fam'#237'lia') or (cbNivel.Text = 'Subordem') then
    Fltr := tfOrders
  else
  if (cbNivel.Text = 'Ordem') or (cbNivel.Text = 'Superordem') or (cbNivel.Text = 'Subclasse') then
    Fltr := tfClasses
  else
  if (cbNivel.Text = 'Classe') or (cbNivel.Text = 'Subdivis'#227'o') then
    Fltr := tfPhyla
  else
  if (cbNivel.Text = 'Divis'#227'o') or (cbNivel.Text = 'Subreino') then
    Fltr := tfKingdoms
  else
    Fltr := tfNone;

  if BuscarTaxon('', Fltr, etSuperior, DMT.qTaxa, 'TAX_SUPERIOR', 'NOME_SUPERIOR', False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmDevTools.sbSuperiorIOCClick(Sender: TObject);
var
  Fltr: TTaxonFilter;
begin
  if (cbNivelIOC.Text = 'Variedade') or (cbNivelIOC.Text = 'Subvariedade') or
    (cbNivelIOC.Text = 'Forma') or (cbNivelIOC.Text = 'Subforma') then
    Fltr := tfSppInfrasp
  else
  if (cbNivelIOC.Text = 'Subesp'#233'cie') then
    Fltr := tfSpecies
  else
  if (cbNivelIOC.Text = 'Esp'#233'cie') or (cbNivelIOC.Text = 'Subg'#234'nero') then
    Fltr := tfGenera
  else
  if (cbNivelIOC.Text = 'G'#234'nero') then
    Fltr := tfFamTribes
  else
  if (cbNivelIOC.Text = 'Subtribo') then
    Fltr := tfTribes
  else
  if (cbNivelIOC.Text = 'Tribo') or (cbNivelIOC.Text = 'Subfam'#237'lia') then
    Fltr := tfFamilies
  else
  if (cbNivelIOC.Text = 'Fam'#237'lia') or (cbNivelIOC.Text = 'Subordem') then
    Fltr := tfOrders
  else
  if (cbNivelIOC.Text = 'Ordem') or (cbNivelIOC.Text = 'Superordem') or (cbNivelIOC.Text = 'Subclasse') then
    Fltr := tfClasses
  else
  if (cbNivelIOC.Text = 'Classe') or (cbNivelIOC.Text = 'Subdivis'#227'o') then
    Fltr := tfPhyla
  else
  if (cbNivelIOC.Text = 'Divis'#227'o') or (cbNivelIOC.Text = 'Subreino') then
    Fltr := tfKingdoms
  else
    Fltr := tfNone;

  if BuscarTaxon('', Fltr, etSuperiorIOC, DMT.qTaxa, 'TAX_SUPERIOR_IOC', 'NOME_SUPERIOR_IOC', False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmDevTools.sbtClearFiltersClick(Sender: TObject);
begin
  if Filtrado then
  begin
    tsfMarcados.Checked := False;
    tsfDesmarcados.Checked := False;
    tsfLixo.Checked := False;
    sbRestoreNome.Visible := False;

    lblCountNivelFilter.Caption := 'nenhum selecionado';
    ckNivelFilter.CheckAll(TCheckBoxState.cbUnchecked, False);

    tsEbirdFilter.Checked := False;
    tsIOCFilter.Checked := False;
    tsCBROFilter.Checked := False;

    tsExtinctFilter.Checked := False;

    tsSynonymFilter.Checked := False;
    tsValidFilter.Checked := False;

    Filtrado := False;
  end;
  NivelFilter := '';
  EbirdFilter := '';
  IocFilter := '';
  CbroFilter := '';
  ExtintoFilter := '';
  SinonimoFilter := '';

  EP.Clear;
  TimerFind.Enabled := False;
  SearchStr.Clear;
  QuickFilters.Clear;
  Modifier.Clear;

  GetFiltersTaxa(QuickFilters);

  PesquisarTaxa(EP.Text);
end;

procedure TfrmDevTools.sbValidoCBROClick(Sender: TObject);
begin
  if BuscarTaxon('', tfNone, etValidoCBRO, DMT.qTaxa, 'TAX_VALIDO_CBRO', 'NOME_VALIDO_CBRO', False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmDevTools.sbValidoClick(Sender: TObject);
begin
  if BuscarTaxon('', tfNone, etValido, DMT.qTaxa, 'TAX_VALIDO', 'NOME_VALIDO', False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmDevTools.sbValidoIOCClick(Sender: TObject);
begin
  if BuscarTaxon('', tfNone, etValidoIOC, DMT.qTaxa, 'TAX_VALIDO_IOC', 'NOME_VALIDO_IOC', False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmDevTools.ckRankFilterClickCheck(Sender: TObject);
var
  i, t: Integer;
begin
  TimerFind.Enabled := False;
  NivelFilter := '';
  t := 0;
  for i := 0 to ckRankFilter.Count - 1 do
    if ckRankFilter.Checked[i] = True then
      Inc(t);

  if t > 0 then
  begin
    for i := 0 to ckRankFilter.Count - 1 do
    begin
      if ckRankFilter.Checked[i] = True then
      begin
        if (NivelFilter <> '') then
          NivelFilter := NivelFilter + ' or ';
        NivelFilter := NivelFilter + '(t.TAX_NIVEL = ' +
          IntToStr(CarregaCodigo('AUX_TAX_NIVEIS', 'NIV_NOME', ckRankFilter.Items[i])) + ')';
      end;
    end;
    NivelFilter := '(' + NivelFilter + ')';

    if t = 1 then
      lblCountNivelFilter.Caption := IntToStr(t) + ' selecionado'
    else
      lblCountNivelFilter.Caption := IntToStr(t) + ' selecionados';

  end
  else
  begin
    NivelFilter := '';
    lblCountNivelFilter.Caption := 'nenhum selecionado';

  end;
  GetFiltersTaxa(QuickFilters);
  TimerFind.Enabled := True;
end;

procedure TfrmDevTools.etAutoriaChange(Sender: TObject);
var aut, comp, nome: String;
    ps, sl: Integer;
begin
  if SkipCompleteAutor then
    Exit;

  if (DMT.qTaxa.State in [dsInsert, dsEdit]) and (Length(eAutor.Text) >= 3) then
  begin
    with TSQLQuery.Create(DMT.taxCon), SQL do
    try
      Clear;
      Add('select distinct TAX_AUTOR from AUX_TAXONS where TAX_AUTOR like :NAUTOR order by TAX_AUTOR asc');
      ParamByName('NAUTOR').DataType := ftString;
      ParamByName('NAUTOR').AsWideString := eAutoria.Text + '%';
      Open;
      if RecordCount > 0 then
      begin
        First;
        comp:= FieldByName('TAX_AUTOR').AsString;
        nome:= comp;
        aut:= eAutoria.Text;
        comp:= StringReplace(comp, aut, '', [rfIgnoreCase]);
        ps:= Length(aut);
        sl:= Length(comp);
        eAutor.Text:= nome;
        eAutor.SelStart:= ps;
        eAutor.SelLength:= sl;
      end;
      Close;
    finally
      Free;
    end;
  end;
end;

procedure TfrmDevTools.etAutoriaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (DMT.qTaxa.State in [dsInsert, dsEdit]) then
  begin
    if ((Key = VK_DELETE)) then
    begin
      if eAutoria.Focused then
        SkipCompleteAutor:= True;
    end;
  end;
end;

procedure TfrmDevTools.etNomeExit(Sender: TObject);
var t: Integer;
    aLista: TStringList;
begin
  if not (DMT.qTaxa.State in [dsInsert, dsEdit]) then
    Exit;

  if not (Trim(DMT.qTaxaTAX_NOME.AsString) = '') then
  begin
    aLista:= TStringList.Create;
    t:= ExtractStrings([' '], [' '], PChar(DMT.qTaxaTAX_NOME.AsString), aLista);
    case t of
      1:
      begin
        if TRegEx.IsMatch(aLista[0],'^[a-z/]+ini$',[roIgnoreCase]) then
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Tribo')
        else
        if TRegEx.IsMatch(aLista[0],'^[a-z/]+inae$',[roIgnoreCase]) then
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Subfam'#237'lia')
        else
        if TRegEx.IsMatch(aLista[0],'^[a-z/]+idae$',[roIgnoreCase]) then
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Fam'#237'lia')
        else
        if TRegEx.IsMatch(aLista[0],'^[a-z/]+formes$',[roIgnoreCase]) then
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Ordem')
        else
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','G'#234'nero');
        DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
      end;
      2:
      begin
        if (aLista[1] = 'sp.') then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Spuh');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
        end else
        if (Pos('/',aLista[1]) > 0) then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Slash');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
        end else
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Esp'#233'cie');
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
        end;
        if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
        begin
          DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]);
          DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
        end;
      end;
      3:
      begin
        if (aLista[1] = 'sp.') then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Spuh');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
          if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
          begin
            DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]);
            DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
          end;
        end else
        if (Pos('/',aLista[1]) > 0) then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Slash');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.Clear;
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
        end else
        if (Pos('/',aLista[2]) > 0) then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Grupo (polit'#237'pico)');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
          if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
          begin
            DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]+' '+aLista[1]);
            DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
          end;
        end else
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Subesp'#233'cie');
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
          DMT.qTaxaTAX_NOME_SUBESPECIE.AsString:= aLista[2];
          if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
          begin
            DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]+' '+aLista[1]);
            DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
          end;
        end;
      end;
      4:
      begin
        if (aLista[1] = 'sp.') then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Spuh');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
          if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
          begin
            DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]);
            DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
          end;
        end else
        if (aLista[2] = 'x') then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','H'#237'brido');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
          if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
          begin
            DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]);
            DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
          end;
        end else
        if (Pos('[',aLista[2]) > 0) then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS','NIV_NOME','Grupo (polit'#237'pico)');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
          if DMT.qTaxaTAX_SUPERIOR.AsLargeInt = 0 then
          begin
            DMT.qTaxaTAX_SUPERIOR.AsLargeInt:= CarregaCodigo('AUX_TAXONS','TAX_NOME',aLista[0]+' '+aLista[1]);
            DMT.qTaxaNOME_SUPERIOR.AsString:= CarregaNome('AUX_TAXONS','TAX_NOME','reg_num_interno',DMT.qTaxaTAX_SUPERIOR.AsLargeInt);
          end;
        end else
        begin
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
          DMT.qTaxaTAX_NOME_SUBESPECIE.AsString:= aLista[2]+' '+aLista[3];
        end;
      end;
      5:
      begin
        if (aLista[2] = 'x') then
        begin
          DMT.qTaxaTAX_NIVEL.AsLargeInt:= CarregaCodigo('AUX_TAX_NIVEIS', 'NIV_NOME', 'H'#237'brido');
          DMT.qTaxaTAX_NOME_EBIRD.AsBoolean:= True;
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.Clear;
        end else
        begin
          DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
          DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
          DMT.qTaxaTAX_NOME_SUBESPECIE.AsString:= aLista[2]+' '+aLista[3]+' '+aLista[4];
        end;
      end;
      6:
      begin
        DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
        DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
        DMT.qTaxaTAX_NOME_SUBESPECIE.AsString:= aLista[2]+' '+aLista[3]+' '+aLista[4]+' '+aLista[5];
      end;
      7:
      begin
        DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
        DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
        DMT.qTaxaTAX_NOME_SUBESPECIE.AsString:= aLista[2]+' '+aLista[3]+' '+aLista[4]+' '+aLista[5]+' '+aLista[6];
      end;
      8:
      begin
        DMT.qTaxaTAX_NOME_GENERO.AsString:= aLista[0];
        DMT.qTaxaTAX_NOME_ESPECIE.AsString:= aLista[1];
        DMT.qTaxaTAX_NOME_SUBESPECIE.AsString:= aLista[2]+' '+aLista[3]+' '+aLista[4]+' '+aLista[5]+' '+aLista[6]+' '+aLista[7];
      end;
    end;
    FreeAndNil(aLista);
  end;
end;

procedure TfrmDevTools.etNomeKeyPress(Sender: TObject; var Key: char);
var
  Fltr: TTaxonFilter;
  aCombo: TDBLookupComboBox;
begin
//  FormKeyPress(Sender,Key);
  // Pesquisa alfabética em campo numérico
  if ((Key.IsLetter) or (Key.IsNumber) or (Key.IsPunctuation) or (Key.IsSeparator) or
     (Key.IsSymbol)) then
  begin
    SkipCompleteAutor := False;

    if eSuperiorIOC.Focused then
      aCombo:= cbNivelIOC
    else
    if eSuperiorCBRO.Focused then
      aCombo:= cbNivelCBRO
    else
      aCombo:= cbNivel;

    if (aCombo.Text = 'Variedade') or (aCombo.Text = 'Subvariedade') or
      (aCombo.Text = 'Forma') or (aCombo.Text = 'Subforma') then
      Fltr := tfSppInfrasp
    else
    if (aCombo.Text = 'Subesp'#233'cie') then
      Fltr := tfSpeciesGrp
    else
    if (aCombo.Text = 'Esp'#233'cie') or (aCombo.Text = 'Subg'#234'nero') then
      Fltr := tfGenera
    else
    if (aCombo.Text = 'G'#234'nero') then
      Fltr := tfFamTribes
    else
    if (aCombo.Text = 'Subtribo') then
      Fltr := tfTribes
    else
    if (aCombo.Text = 'Tribo') or (aCombo.Text = 'Subfam'#237'lia') then
      Fltr := tfFamilies
    else
    if (aCombo.Text = 'Fam'#237'lia') or (aCombo.Text = 'Subordem') then
      Fltr := tfOrders
    else
    if (aCombo.Text = 'Ordem') or (aCombo.Text = 'Superordem') or (aCombo.Text = 'Subclasse') then
      Fltr := tfClasses
    else
    if (aCombo.Text = 'Classe') or (aCombo.Text = 'Subdivis'#227'o') then
      Fltr := tfPhyla
    else
    if (aCombo.Text = 'Divis'#227'o') or (aCombo.Text = 'Subreino') then
      Fltr := tfKingdoms
    else
      Fltr := tfNone;

    if eSuperior.Focused then
    begin
      BuscarTaxon(Key, Fltr, eSuperior, DMT.qTaxa, 'TAX_SUPERIOR', 'NOME_SUPERIOR', False);
      Key:= #0;
    end;
    if eValido.Focused then
    begin
      BuscarTaxon(Key, tfNone, eValido, DMT.qTaxa, 'TAX_VALIDO', 'NOME_VALIDO', False);
      Key:= #0;
    end;
    if eSuperiorIOC.Focused then
    begin
      BuscarTaxon(Key, Fltr, eSuperiorIOC, DMT.qTaxa, 'TAX_SUPERIOR_IOC', 'NOME_SUPERIOR_IOC', False);
      Key:= #0;
    end;
    if eValidoIOC.Focused then
    begin
      BuscarTaxon(Key, tfNone, eValidoIOC, DMT.qTaxa, 'TAX_VALIDO_IOC', 'NOME_VALIDO_IOC', False);
      Key:= #0;
    end;
    if eSuperiorCBRO.Focused then
    begin
      BuscarTaxon(Key, Fltr, eSuperiorCBRO, DMT.qTaxa, 'TAX_SUPERIOR_CBRO', 'NOME_SUPERIOR_CBRO', False);
      Key:= #0;
    end;
    if eValidoCBRO.Focused then
    begin
      BuscarTaxon(Key, tfNone, eValidoCBRO, DMT.qTaxa, 'TAX_VALIDO_CBRO', 'NOME_VALIDO_CBRO', False);
      Key:= #0;
    end;
  end;
  { LIMPA CAMPO = Backspace }
  if (Key = #8) then
  begin
    if eAutor.Focused then
      SkipCompleteAutor := True;

    if eSuperior.Focused then
    begin
      DMT.qTaxaTAX_SUPERIOR.Clear;
      DMT.qTaxaNOME_SUPERIOR.Clear;
      Key:= #0;
    end;
    if eValido.Focused then
    begin
      DMT.qTaxaTAX_VALIDO.Clear;
      DMT.qTaxaNOME_VALIDO.Clear;
      Key:= #0;
    end;
    if eSuperiorIOC.Focused then
    begin
      DMT.qTaxaTAX_SUPERIOR_IOC.Clear;
      DMT.qTaxaNOME_SUPERIOR_IOC.Clear;
      Key:= #0;
    end;
    if eValidoIOC.Focused then
    begin
      DMT.qTaxaTAX_VALIDO_IOC.Clear;
      DMT.qTaxaNOME_VALIDO_IOC.Clear;
      Key:= #0;
    end;
    if eSuperiorCBRO.Focused then
    begin
      DMT.qTaxaTAX_SUPERIOR_CBRO.Clear;
      DMT.qTaxaNOME_SUPERIOR_CBRO.Clear;
      Key:= #0;
    end;
    if eValidoCBRO.Focused then
    begin
      DMT.qTaxaTAX_VALIDO_CBRO.Clear;
      DMT.qTaxaNOME_VALIDO_CBRO.Clear;
      Key:= #0;
    end;
  end;
  { PROXIMO CAMPO = Enter }
  if Key = #13 then
  begin
    //if Sender = eOutrosNomes then
    //  sbSaveNomeClick(nil)
    //else
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

end.

