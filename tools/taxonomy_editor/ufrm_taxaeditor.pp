unit ufrm_TaxaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, Menus, DB, DBCtrls,
  ActnList, StdCtrls, attabs, ATStatusBar, BCPanel, BCButton, ColorSpeedButton,
  BGRABitmap, SQLDB, CheckLst, rxswitch, Grids, DBGrids;

type

  { TfrmTaxaEditor }

  TfrmTaxaEditor = class(TForm)
    actExit: TAction;
    actBatchActions: TAction;
    actFormatSciNames: TAction;
    actImport: TAction;
    actExport: TAction;
    actAbout: TAction;
    actList: TActionList;
    AppProperties: TApplicationProperties;
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    BCPanel3: TBCPanel;
    BCPanel4: TBCPanel;
    BCPanel5: TBCPanel;
    BCPanel6: TBCPanel;
    cbtCbroRank: TDBLookupComboBox;
    cbtIocRank: TDBLookupComboBox;
    cbtRank: TDBLookupComboBox;
    cktCbro: TDBCheckBox;
    cktClements: TDBCheckBox;
    cktExtinct: TDBCheckBox;
    cktIoc: TDBCheckBox;
    etAuthorship: TDBEdit;
    etCbroOtherPtNames: TDBEdit;
    etCbroParentTaxon: TDBEdit;
    etCbroSortNr: TDBEdit;
    etCbroValidName: TDBEdit;
    etEbirdCode: TDBEdit;
    etEnglishName: TDBEdit;
    etExtinctionYear: TDBEdit;
    etFullname: TDBEdit;
    etIocEnglishName: TDBEdit;
    etIocParentTaxon: TDBEdit;
    etIocSortNr: TDBEdit;
    etIocValidName: TDBEdit;
    etParentTaxon: TDBEdit;
    etPortugueseName: TDBEdit;
    etQuickcode: TDBEdit;
    etSortNr: TDBEdit;
    etSpanishName: TDBEdit;
    etSubspecificGroup: TDBEdit;
    etValidName: TDBEdit;
    lbltAuthorship: TLabel;
    lbltCbroOtherPtNames: TLabel;
    lbltCbroParentTaxon: TLabel;
    lbltCbroRank: TLabel;
    lbltCbroSortNr: TLabel;
    lbltCbroValidName: TLabel;
    lbltDistribution: TLabel;
    lbltEbirdCode: TLabel;
    lbltEnglishName: TLabel;
    lbltFullname: TLabel;
    lbltIocDistribution: TLabel;
    lbltIocEnglishName: TLabel;
    lbltIocParentTaxon: TLabel;
    lbltIocRank: TLabel;
    lbltIocSortNr: TLabel;
    lbltIocValidName: TLabel;
    lbltParentTaxon: TLabel;
    lbltPortugueseName: TLabel;
    lbltQuickCode: TLabel;
    lbltRank: TLabel;
    lbltSortNr: TLabel;
    lbltSpanishName: TLabel;
    lbltSubspecificGroup: TLabel;
    lbltValidName: TLabel;
    mtDistribution: TDBMemo;
    mtIocDistribution: TDBMemo;
    peTaxa: TPanel;
    ptAuthorship: TBCPanel;
    ptCbroOtherPtNames: TBCPanel;
    ptCbroParentTaxon: TBCPanel;
    ptCbroRank: TBCPanel;
    ptCbroSortNr: TBCPanel;
    ptCbroValidName: TBCPanel;
    ptContent: TBCPanel;
    gridTaxa: TDBGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    ptDistribution: TBCPanel;
    ptEbirdCode: TBCPanel;
    ptEnglishName: TBCPanel;
    ptExtinct: TBCPanel;
    ptFullName: TBCPanel;
    ptIocDistribution: TBCPanel;
    ptIocEnglishName: TBCPanel;
    ptIocParentTaxon: TBCPanel;
    ptIocRank: TBCPanel;
    ptIocSortNr: TBCPanel;
    ptIocValidName: TBCPanel;
    ptParentTaxon: TBCPanel;
    ptPortugueseName: TBCPanel;
    ptQuickCode: TBCPanel;
    ptRank: TBCPanel;
    ptSortNr: TBCPanel;
    ptSpanishName: TBCPanel;
    ptSubspecificGroup: TBCPanel;
    ptToolbar: TBCPanel;
    ptValidName: TBCPanel;
    sbClearSearch: TColorSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sboxTaxa: TScrollBox;
    sbRecordHistory: TSpeedButton;
    sbRecordHistory1: TSpeedButton;
    sbRecordHistory2: TSpeedButton;
    sbRecordHistory3: TSpeedButton;
    sbtCbroParentTaxon: TSpeedButton;
    sbtCbroValidName: TSpeedButton;
    sbtIocParentTaxon: TSpeedButton;
    sbtIocValidName: TSpeedButton;
    sbtParentTaxon: TSpeedButton;
    sbtValidName: TSpeedButton;
    Separator2: TMenuItem;
    mmBatchActions: TMenuItem;
    mmExit: TMenuItem;
    mmFormatSciNames: TMenuItem;
    pmMain: TPopupMenu;
    clbTaxonRanksFilter: TCheckListBox;
    eFindTaxa: TEdit;
    icoBandSizeFilter11: TImage;
    icoBandSizeFilter12: TImage;
    icoBandSizeFilter9: TImage;
    icoExtinctFilter: TImage;
    icoMarkedFilter: TImage;
    iconFindTaxa: TImage;
    icoSynonymsFilter: TImage;
    icoTaxonomiesFilter: TImage;
    icoTaxonRanksFilter: TImage;
    icoUnmarkedFilter: TImage;
    lblClementsFilter: TLabel;
    lblCountTaxonRanksFilter: TLabel;
    lblExtinctFilter: TLabel;
    lblHasSynonymsFilter: TLabel;
    lblMarkedFilter: TLabel;
    lblSynonymFilter: TLabel;
    lblTaxonomyCbroFilter: TLabel;
    lblTaxonomyIocFilter: TLabel;
    lblTaxonRanksFilter: TLabel;
    lblUnmarkedFilter: TLabel;
    navTabs: TATTabs;
    nbTaxaSide: TNotebook;
    pFindTaxa: TBCPanel;
    pExtinctFilter: TBCPanel;
    pgTaxaFilters: TPage;
    pHasSynonymsFilter: TBCPanel;
    pIsSynonymFilter: TBCPanel;
    pMainMenu: TBCPanel;
    bMenu: TImageList;
    bStatusBar: TImageList;
    nbPages: TNotebook;
    pgRanks: TPage;
    pgTaxonomies: TPage;
    pgTaxa: TPage;
    pMarkedFilter: TBCPanel;
    pTaxaList: TBCPanel;
    pTaxaRightBar: TBCPanel;
    pTaxaToolbar: TBCPanel;
    pTaxonomyCbroFilter: TBCPanel;
    pTaxonomyClementsFilter: TBCPanel;
    pTaxonomyIocFilter: TBCPanel;
    pTaxonRanksFilters: TBCPanel;
    pTitleTaxonRanksFilter: TPanel;
    pUnmarkedFilter: TBCPanel;
    sbAdvancedFilters: TSpeedButton;
    SBar: TATStatus;
    sbCancelRecord: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbFileMenu: TBCButton;
    sbFirstRecord: TSpeedButton;
    sbGroupRecords: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbMoreOptions: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sboxTaxaFilters: TScrollBox;
    sbPriorRecord: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbSaveRecord: TSpeedButton;
    sbShowAudio: TSpeedButton;
    sbShowDocs: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbSortRecords: TSpeedButton;
    Separator1: TMenuItem;
    splitTaxaLeft: TSplitter;
    splitTaxaRight: TSplitter;
    TimerFind: TTimer;
    tsfMarked: TRxSwitch;
    tsfUnmarked: TRxSwitch;
    tsHasSynonyms: TRxSwitch;
    tsIsSynonym: TRxSwitch;
    tsTaxonExtinct: TRxSwitch;
    tsTaxonomyCbro: TRxSwitch;
    tsTaxonomyClements: TRxSwitch;
    tsTaxonomyIoc: TRxSwitch;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure eFindTaxaChange(Sender: TObject);
    procedure etAuthorshipChange(Sender: TObject);
    procedure etAuthorshipKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure navTabsTabChanged(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
  private
    SkipCompleteAutor: Boolean;
  public

  end;

var
  frmTaxaEditor: TfrmTaxaEditor;

implementation

uses
  lib_taxa, udm_taxa, udlg_about;

{$R *.lfm}

{ TfrmTaxaEditor }

procedure TfrmTaxaEditor.actAboutExecute(Sender: TObject);
begin
  dlgAbout := TdlgAbout.Create(nil);
  try
    ShowModal;
  finally
    FreeAndNil(dlgAbout);
  end;
end;

procedure TfrmTaxaEditor.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmTaxaEditor.eFindTaxaChange(Sender: TObject);
begin
  sbClearFindTaxa.Visible := Length(Trim(eFindTaxa.Text)) > 0;
end;

procedure TfrmTaxaEditor.etAuthorshipChange(Sender: TObject);
var toComplete, completeName: String;
    Qry: TSQLQuery;
begin
  inherited;
  if SkipCompleteAutor then
    Exit;

  if (dmTaxa.qTaxa.State in [dsInsert, dsEdit]) and (Length(etAuthorship.Text) >= 3) then
  begin
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      Database := dmTaxa.sqlCon;
      Clear;
      Add('SELECT DISTINCT authorship FROM zoo_taxa');
      Add('WHERE authorship LIKE :author');
      Add('ORDER BY authorship ASC');
      ParamByName('author').AsString := etAuthorship.Text + '%';
      Open;
      if RecordCount > 0 then
      begin
        First;
        toComplete := FieldByName('authorship').AsString;
        completeName := toComplete;
        toComplete := StringReplace(toComplete, etAuthorship.Text, '', [rfIgnoreCase]);
        etAuthorship.Text := completeName;
        etAuthorship.SelStart := Length(etAuthorship.Text);
        etAuthorship.SelLength := Length(toComplete);
      end;
    finally
      Close;
      FreeAndNil(Qry);
    end;
  end;
end;

procedure TfrmTaxaEditor.etAuthorshipKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //FormKeyDown(Sender, Key, Shift);
  if (dmTaxa.qTaxa.State in [dsInsert, dsEdit]) then
  begin
    if (Key = VK_DELETE) then
    begin
      if etAuthorship.Focused then
        SkipCompleteAutor := True;
    end;
  end;
end;

procedure TfrmTaxaEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  dmTaxa.sqlCon.CloseDataSets;
end;

procedure TfrmTaxaEditor.FormShow(Sender: TObject);
begin
  dmTaxa.lookRanks.Open;
  dmTaxa.qTaxa.Open;
end;

procedure TfrmTaxaEditor.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
//var
//  aRank: Integer;
begin
  //aRank := TDBGrid(Sender).DataSource.DataSet.FieldByName('rank_id').AsInteger;
  //if GetRankType(aRank) >= trSuperGenus then
  //  Column.Font.Style := [fsItalic]
  //else
  //  Column.Font.Style := [fsBold];
end;

procedure TfrmTaxaEditor.navTabsTabChanged(Sender: TObject);
begin
  nbPages.PageIndex := navTabs.TabIndex;
end;

procedure TfrmTaxaEditor.sbCancelRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Cancel;
end;

procedure TfrmTaxaEditor.sbEditRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Edit;
end;

procedure TfrmTaxaEditor.sbFirstRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.First;
end;

procedure TfrmTaxaEditor.sbInsertRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Insert;
end;

procedure TfrmTaxaEditor.sbLastRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Last;
end;

procedure TfrmTaxaEditor.sbNextRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Next;
end;

procedure TfrmTaxaEditor.sbPriorRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Prior;
end;

procedure TfrmTaxaEditor.sbRefreshRecordsClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.sbSaveRecordClick(Sender: TObject);
begin
  dmTaxa.qTaxa.Post;
end;

end.

