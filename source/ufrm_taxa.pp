unit ufrm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  ATLinkLabel, BCPanel, Buttons, Classes, ComCtrls, DB, SQLDB, DBCtrls, DBGrids,
  ExtCtrls, Menus, StdCtrls, ColorSpeedButton, SysUtils, Forms, RegExpr,
  Controls, Graphics, Dialogs, cbs_datatypes, Grids, Types;

type

  { TfrmTaxa }

  TfrmTaxa = class(TForm)
    ckCBRO: TDBCheckBox;
    ckClements: TDBCheckBox;
    ckIOC: TDBCheckBox;
    dsSynonyms: TDataSource;
    dsChilds: TDataSource;
    eSearch: TEdit;
    lblLinkCaptures: TLabel;
    lblLinkEggs: TLabel;
    lblLinkNests: TLabel;
    lblLinkSpecimens: TLabel;
    lblLinkSightings: TLabel;
    lblLinkIndividuals: TLabel;
    pTaxonData: TFlowPanel;
    gridChilds: TDBGrid;
    gridSynonyms: TDBGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    iconSearch: TImage;
    imgConservation: TImage;
    imgIUCN: TImageList;
    iSearch: TImageList;
    iSearchDark: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblValidName: TLabel;
    pConservation: TPanel;
    pConstrainedBox: TPanel;
    pContent: TPanel;
    pDetails: TPanel;
    pmPrint: TPopupMenu;
    pmPrintTaxa: TMenuItem;
    pmPrintTaxaHierarchical: TMenuItem;
    pmPrintTaxaRecorded: TMenuItem;
    pmPrintTaxaRecordedByLocality: TMenuItem;
    pScientificName: TPanel;
    pSideToolbar: TPanel;
    pTaxonInfo: TPanel;
    pTaxonomies: TPanel;
    pToolbar: TBCPanel;
    pSearch: TBCPanel;
    pTop: TPanel;
    pViews: TPanel;
    sbClearSearch: TColorSpeedButton;
    sbBirdsOfTheWorld: TSpeedButton;
    sbPrint: TSpeedButton;
    sbShareRecords: TSpeedButton;
    sbGoogleScholar: TSpeedButton;
    sbEbird: TSpeedButton;
    sbGoogleImages: TSpeedButton;
    sbWikiaves: TSpeedButton;
    sbIUCNRedList: TSpeedButton;
    sbGBIF: TSpeedButton;
    sbGoogleSearch: TSpeedButton;
    scrollData: TScrollBox;
    SplitRight: TSplitter;
    TimerData: TTimer;
    TimerFind: TTimer;
    dsLink: TDataSource;
    gridTaxa: TDBGrid;
    tvHierarchy: TTreeView;
    txtAuthorship: TDBText;
    txtDistribution: TDBText;
    txtDistributionIOC: TDBText;
    txtEnglishName: TDBText;
    txtExtinctionYear: TDBText;
    txtPortugueseName: TDBText;
    txtRank: TDBText;
    txtScientificName: TDBText;
    txtSpanishName: TDBText;
    txtValidName: TDBText;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eSearchChange(Sender: TObject);
    procedure eSearchEnter(Sender: TObject);
    procedure eSearchExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure gridTaxaPrepareCanvas
      (sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState
      );
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure TimerDataTimer(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure txtValidNameClick(Sender: TObject);
    procedure txtValidNameMouseEnter(Sender: TObject);
    procedure txtValidNameMouseLeave(Sender: TObject);
  private
    FSearch: TCustomSearch;
    FSearchString, OldSearchString: String;
    CanToggle: Boolean;
    procedure ApplyDarkMode;
    procedure GetSightingsCount;
    procedure GetIndividualsCount;
    procedure GetCapturesCount;
    procedure GetSpecimensCount;
    procedure GetNestsCount;
    procedure GetEggsCount;
    function Search(AValue: String): Boolean;
    procedure SetSearchString(aValue: String);
  public
    property SearchString: String read FSearchString write SetSearchString;
  end;

var
  frmTaxa: TfrmTaxa;

implementation

uses
  cbs_global, cbs_locale, cbs_themes, cbs_datasearch, cbs_taxonomy, cbs_getvalue, udm_main, udm_grid,
  uDarkStyleParams;

{$R *.lfm}

{ TfrmTaxa }

procedure TfrmTaxa.ApplyDarkMode;
begin
  pSearch.Background.Color := clCardBGDefaultDark;
  pSearch.Border.Color := clSolidBGSecondaryDark;
  pSearch.ParentBackground := True;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateHover.Color := clSolidBGSecondaryDark;
  sbClearSearch.StateActive.Color := clSolidBGTertiaryDark;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
  iconSearch.Images := iSearchDark;
  sbClearSearch.Images := iSearchDark;

  pToolbar.Background.Color := clCardBGDefaultDark;
  pToolbar.Border.Color := clCardBGSecondaryDark;
  sbShareRecords.Images := iButtonsDark;
  sbPrint.Images := iButtonsDark;

  sbGoogleSearch.Images := DMM.iWebDark;
  sbGoogleImages.Images := DMM.iWebDark;
  sbGoogleScholar.Images := DMM.iWebDark;
  sbEbird.Images := DMM.iWebDark;
  sbWikiaves.Images := DMM.iWebDark;
  sbIUCNRedList.Images := DMM.iWebDark;
  sbGBIF.Images := DMM.iWebDark;
end;

procedure TfrmTaxa.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  TimerData.Enabled := False;
  TimerData.Enabled := True;

end;

procedure TfrmTaxa.eSearchChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  sbClearSearch.Visible := Length(eSearch.Text) > 0;
end;

procedure TfrmTaxa.eSearchEnter(Sender: TObject);
begin
  //if eSearch.Text = EmptyStr then
  //  pSearch.Width := ClientWidth div 4;
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := clSolidBGBaseDark;
    pSearch.Border.Color := clSolidBGTertiaryDark;
  end
  else
  begin
    pSearch.Background.Color := clWhite;
    pSearch.Border.Color := clAccentFillTertiaryLight;
  end;
  //pSearch.Border.Width := 2;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TfrmTaxa.eSearchExit(Sender: TObject);
begin
  //if eSearch.Text = EmptyStr then
  //  pSearch.Width := 148;
  if IsDarkModeEnabled then
  begin
    pSearch.Background.Color := clCardBGDefaultDark;
    pSearch.Border.Color := clSolidBGSecondaryDark;
  end
  else
  begin
    pSearch.Background.Color := $00FAFAFA;
    pSearch.Border.Color := clDefaultBorderLight;
  end;
  pSearch.Border.Width := 1;
  eSearch.Color := pSearch.Background.Color;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TfrmTaxa.FormCreate(Sender: TObject);
begin
  CanToggle := True;
end;

procedure TfrmTaxa.FormDestroy(Sender: TObject);
begin
  dsChilds.DataSet.Close;
  dsSynonyms.DataSet.Close;
  dsLink.DataSet.Close;

  FreeAndNil(FSearch);
end;

procedure TfrmTaxa.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    sbClearSearchClick(nil);  { Clear search }

    Key := #0;
  end;
end;

procedure TfrmTaxa.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FSearch := TCustomSearch.Create(tbZooTaxa);
  FSearch.DataSet := DMG.qTaxa;

  SetZooTaxaSQL(DMG.qTaxa.SQL, fvReset);

  dsLink.DataSet.Open;
  dsSynonyms.DataSet.Open;
  dsChilds.DataSet.Open;
end;

procedure TfrmTaxa.GetCapturesCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM captures');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    lblLinkCaptures.Caption := Format(rsTitleCaptures + ' (%d)', [C]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetEggsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM eggs');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    lblLinkEggs.Caption := Format(rsTitleEggs + ' (%d)', [C]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetIndividualsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM individuals');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    lblLinkIndividuals.Caption := Format(rsTitleIndividuals + ' (%d)', [C]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetNestsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM nests');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    lblLinkNests.Caption := Format(rsTitleNests + ' (%d)', [C]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetSightingsCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM sightings');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    lblLinkSightings.Caption := Format(rsTitleSightings + ' (%d)', [C]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.GetSpecimensCount;
var
  Qry: TSQLQuery;
  C: Integer;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;

    Add('SELECT COUNT(active_status) AS counter FROM specimens');
    Add('WHERE (taxon_id = :ataxon) AND (active_status = 1)');
    ParamByName('ATAXON').AsInteger := DMG.qTaxa.FieldByName('taxon_id').AsInteger;

    Open;
    C := FieldByName('counter').AsInteger;
    Close;

    lblLinkSpecimens.Caption := Format(rsTitleSpecimens + ' (%d)', [C]);
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxa.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  aRank: TZooRank;
begin
  if (Column.FieldName = 'full_name') and (Assigned(Column.Field)) then
  begin
    aRank := GetRankType(TDBGrid(Sender).Columns[2].Field.AsInteger);
    if aRank >= trSuperGenus then
      TDBGrid(Sender).Canvas.Font.Style := [fsItalic]
    else
      TDBGrid(Sender).Canvas.Font.Style := [fsBold];

    if not (gdSelected in AState) then
    begin
      if aRank = trSpecies then
        TDBGrid(Sender).Canvas.Font.Color := clNavy;

      if aRank in [trMonotypicGroup, trPolitypicGroup, trForm, trSpuh, trHybrid, trIntergrade, trDomestic, trSlash] then
        TDBGrid(Sender).Canvas.Font.Color := clGreen;

      if (TDBGrid(Sender).Columns[1].Field.AsInteger > 0) then
        TDBGrid(Sender).Canvas.Font.Color := $00646464;
    end;
  end;
end;

procedure TfrmTaxa.sbClearSearchClick(Sender: TObject);
begin
  eSearch.Clear;
  if eSearch.CanSetFocus then
    eSearch.SetFocus;
end;

procedure TfrmTaxa.sbPrintClick(Sender: TObject);
begin
  with TSpeedButton(Sender).ClientToScreen(point(0, TSpeedButton(Sender).Height + 1)) do
    pmPrint.Popup(X, Y);
end;

function TfrmTaxa.Search(AValue: String): Boolean;
var
  i, g: Longint;
  Crit: TCriteriaType;
begin
  Result := False;

  {$IFDEF DEBUG}
  LogDebug('Search value: ' + aValue);
  {$ENDIF}
  FSearch.Fields.Clear;
  FSearch.QuickFilters.Clear;

  gridTaxa.BeginUpdate;
  try

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
        g := FSearch.Fields.Add(TSearchGroup.Create);
        FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon (ID)', sdtInteger, crEqual,
          False, aValue));
      end
      else
      begin
        g := FSearch.Fields.Add(TSearchGroup.Create);
        FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Scientific name', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('english_name', 'English name', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('ioc_english_name', 'English name (IOC)', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('spanish_name', 'Spanish name', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('portuguese_name', 'Portuguese name', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('other_portuguese_names', 'Other portuguese names', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('ebird_code', 'eBird code', sdtText, Crit,
          False, aValue));
        FSearch.Fields[g].Fields.Add(TSearchField.Create('quick_code', 'Quick code', sdtText, Crit,
          False, aValue));
      end;
    end;

    //GetFilters;

  finally
    gridTaxa.EndUpdate;
  end;

  //UpdateButtons(dsLink.DataSet);

  Result := FSearch.RunSearch > 0;
end;

procedure TfrmTaxa.SetSearchString(aValue: String);
begin
  if not CanToggle then
    Exit;

  if FSearchString <> OldSearchString then
    OldSearchString := FSearchString;
  FSearchString := aValue;

  if FSearchString = EmptyStr then
    SetZooTaxaSQL(DMG.qTaxa.SQL, fvReset)
  else
    Search(FSearchString);
end;

procedure TfrmTaxa.TimerDataTimer(Sender: TObject);
var
  nOrder, nFamily, nGenus, nSpecies, nGroup: TTreeNode;
  aRank: TZooRank;
begin
  TimerData.Enabled := False;

  aRank := GetRankType(DMG.qTaxa.FieldByName('rank_id').AsInteger);
  if aRank >= trSuperGenus then
    txtScientificName.Font.Style := [fsItalic]
  else
    txtScientificName.Font.Style := [fsBold];

  case DMG.qTaxa.FieldByName('iucn_status').AsString of
    'LC': imgConservation.ImageIndex := 1;
    'NT': imgConservation.ImageIndex := 2;
    'VU': imgConservation.ImageIndex := 3;
    'EN': imgConservation.ImageIndex := 4;
    'CR': imgConservation.ImageIndex := 5;
    'EW': imgConservation.ImageIndex := 6;
    'EX': imgConservation.ImageIndex := 7;
    'DD': imgConservation.ImageIndex := 8;
  else
    imgConservation.ImageIndex := 0;
  end;

  lblValidName.Visible := DMG.qTaxa.FieldByName('valid_id').AsInteger > 0;
  txtValidName.Visible := lblValidName.Visible;

  tvHierarchy.Items.Clear;
  if dsLink.DataSet.FieldByName('order_id').AsInteger > 0 then
  begin
    nOrder := tvHierarchy.Items.Add(nil, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('order_id').AsInteger));
    if dsLink.DataSet.FieldByName('family_id').AsInteger > 0 then
    begin
      nFamily := tvHierarchy.Items.AddChild(nOrder, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('family_id').AsInteger));
      if dsLink.DataSet.FieldByName('genus_id').AsInteger > 0 then
      begin
        nGenus := tvHierarchy.Items.AddChild(nFamily, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('genus_id').AsInteger));
        if dsLink.DataSet.FieldByName('species_id').AsInteger > 0 then
        begin
          nSpecies := tvHierarchy.Items.AddChild(nGenus, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('species_id').AsInteger));
          if dsLink.DataSet.FieldByName('subspecies_group_id').AsInteger > 0 then
          begin
            nGroup := tvHierarchy.Items.AddChild(nSpecies, GetName('zoo_taxa', 'full_name', 'taxon_id', dsLink.DataSet.FieldByName('subspecies_group_id').AsInteger));
          end;
        end;
      end;
    end;
    tvHierarchy.FullExpand;
  end;

  GetSightingsCount;
  GetIndividualsCount;
  GetCapturesCount;
  GetSpecimensCount;
  GetNestsCount;
  GetEggsCount;

end;

procedure TfrmTaxa.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  SetSearchString(eSearch.Text);
end;

procedure TfrmTaxa.txtValidNameClick(Sender: TObject);
begin
  if (not DMG.qTaxa.FieldByName('valid_id').IsNull) then
    eSearch.Text := DMG.qTaxa.FieldByName('valid_id').AsString;
end;

procedure TfrmTaxa.txtValidNameMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := txtValidName.Font.Style + [fsUnderline];
end;

procedure TfrmTaxa.txtValidNameMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := txtValidName.Font.Style - [fsUnderline];
end;

end.

