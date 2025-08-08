unit udlg_import;

{$mode ObjFPC}{$H+}

interface

uses
  BCPanel, Classes, SysUtils, SdfData, fpjson, fpjsondataset, ExtJSDataSet, LCLIntf, fgl,
  dbf, csvdataset, DB, BufDataset, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Buttons, EditBtn, ComCtrls, Menus, fpsDataset, fpsTypes,
  atshapelinebgra, data_import, data_types;

type

  { TdlgImport }

  TdlgImport = class(TForm)
    btnHelp: TBitBtn;
    btnOptions: TBitBtn;
    dsMem: TBufDataset;
    cbTarget: TComboBox;
    dsDbf: TDbf;
    eSourceFile: TEditButton;
    gridConfirm: TStringGrid;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    imgFinished: TImageList;
    imgFinishedDark: TImageList;
    lblProgressInstruction: TLabel;
    lblFieldsInstruction: TLabel;
    lblConfirmInstruction: TLabel;
    lblSourceInstruction: TLabel;
    lblSourceFile: TLabel;
    lblSubtitleImportFinished: TLabel;
    lblTarget: TLabel;
    lblTitleImportFinished: TLabel;
    lblTitleProgress: TLabel;
    lblTitleFields: TLabel;
    lblTitleConfirm: TLabel;
    lblTitleSource: TLabel;
    lineBottom: TShapeLineBGRA;
    pContentFinished: TBCPanel;
    pgFinished: TPage;
    pmfSelectAll: TMenuItem;
    pmfDeselectAll: TMenuItem;
    mProgress: TMemo;
    nbPages: TNotebook;
    OpenDlg: TOpenDialog;
    pContentProgress: TPanel;
    pgProgress: TPage;
    pContentConfirm: TPanel;
    pgConfirm: TPage;
    pBottom: TPanel;
    pContentFields: TPanel;
    pContentSource: TPanel;
    pgFields: TPage;
    pgSource: TPage;
    PBar: TProgressBar;
    pmFields: TPopupMenu;
    pRetry: TBCPanel;
    pSourceOptions: TPanel;
    pTitleProgress: TPanel;
    pTitleFields: TPanel;
    pTitleConfirm: TPanel;
    pTitleSource: TPanel;
    SaveDlg: TSaveDialog;
    sbCancel: TButton;
    sbNext: TButton;
    sbPrior: TButton;
    gridFields: TStringGrid;
    dsSdf: TSdfDataSet;
    dsWorksheet: TsWorksheetDataset;
    sbRetry: TBitBtn;
    sbSaveLog: TBitBtn;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure cbTargetSelect(Sender: TObject);
    procedure eSourceFileButtonClick(Sender: TObject);
    procedure eSourceFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmfDeselectAllClick(Sender: TObject);
    procedure pmfSelectAllClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbPriorClick(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
  private
    FDataSet: TDataSet;
    dsJSON: TExtJSJSONObjectDataSet;
    FFieldMap: TFieldsMap;
    FTableType: TTableType;
    procedure ApplyDarkMode;
    function IsRequiredFilledSource: Boolean;
    procedure LoadFields;
    procedure LoadSearchTables;
    procedure LoadTargetFields;
    procedure LoadTargetTables;
    function ValidateFields: Boolean;
  public

  end;

var
  dlgImport: TdlgImport;

implementation

uses
  utils_locale, utils_global, udm_grid, udm_sampling, ucfg_delimiters, uDarkStyleParams;

{$R *.lfm}

{ TdlgImport }

procedure TdlgImport.ApplyDarkMode;
begin
  btnOptions.Images := iButtonsDark;
  btnHelp.Images := iButtonsDark;
  eSourceFile.Images := iButtonsDark;
  pmFields.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;
  icoImportFinished.Images := imgFinishedDark;
end;

procedure TdlgImport.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_IMPORTING_DATA, 'import-wizard');
end;

procedure TdlgImport.btnOptionsClick(Sender: TObject);
begin
  cfgDelimiters := TcfgDelimiters.Create(nil);
  with cfgDelimiters do
  try
    //QuotedAsText := dsSdf.QuoteChar = '"';
    Delimiter := dsSdf.Delimiter;
    //DecimalSeparator := dsSdf.DecimalSeparator;
    HaveHeader := dsSdf.FirstLineAsSchema;
    if ShowModal = mrOk then
    begin
      //if QuotedAsText then
      //  dsSdf.QuoteChar := '"'
      //else
      //  dsSdf.QuoteChar := #0;
      dsSdf.Delimiter := Delimiter;
      //dsSdf.DecimalSeparator := DecimalSeparator;
      dsSdf.FirstLineAsSchema := HaveHeader;
    end;
  finally
    FreeAndNil(cfgDelimiters);
  end;
end;

procedure TdlgImport.cbTargetSelect(Sender: TObject);
begin
  if TablesDict.IndexOf(cbTarget.Text) >= 0 then
    FTableType := TablesDict[cbTarget.Text];
end;

procedure TdlgImport.eSourceFileButtonClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    eSourceFile.Text := OpenDlg.FileName;
  end;
end;

procedure TdlgImport.eSourceFileChange(Sender: TObject);
var
  jData: TJSONArray;
begin
  sbPrior.Enabled := False;
  sbNext.Enabled := IsRequiredFilledSource;

  if not FileExists(eSourceFile.Text) then
    Exit;

  if eSourceFile.Text <> EmptyStr then
    case ExtractFileExt(eSourceFile.Text) of
      '.csv', '.tsv':
      begin
        FDataSet := dsSdf;
        if ExtractFileExt(eSourceFile.Text) = '.tsv' then
          dsSdf.Delimiter := #9; // Tab character
        cfgDelimiters := TcfgDelimiters.Create(nil);
        with cfgDelimiters do
        try
          Delimiter := dsSdf.Delimiter;
          HaveHeader := dsSdf.FirstLineAsSchema;
          if ShowModal = mrOk then
          begin
            dsSdf.Delimiter := Delimiter;
            dsSdf.FirstLineAsSchema := HaveHeader;
          end;
        finally
          FreeAndNil(cfgDelimiters);
        end;
        dsSdf.FileName := eSourceFile.Text;
      end;
      '.xlsx', '.xls', '.ods':
      begin
        FDataSet := dsWorksheet;
        dsWorksheet.FileName := eSourceFile.Text;
        case ExtractFileExt(eSourceFile.Text) of
          '.xlsx': dsWorksheet.FileFormat := sfOOXML;
          '.xls': dsWorksheet.FileFormat := sfExcel8;
          '.ods': dsWorksheet.FileFormat := sfOpenDocument;
        end;
      end;
      '.json':
      begin
        dsJSON := TExtJSJSONObjectDataSet.Create(nil);
        FDataSet := dsJSON;
        dsJSON.LoadFromFile(eSourceFile.Text);
      end;
      '.dbf':
      begin
        FDataSet := dsDbf;
        dsDbf.FilePathFull := ExtractFilePath(eSourceFile.Text);
        dsDbf.TableName := ExtractFileName(eSourceFile.Text);
      end;
      '.kml', '.kmz': ;
      '.gpx': ;
      '.geojson': ;
    else
      FDataSet := dsMem;
    end;
end;

procedure TdlgImport.FormCreate(Sender: TObject);
begin
  FFieldMap := TFieldsMap.Create;
  FTableType := tbNone;
  LoadTablesDict;
end;

procedure TdlgImport.FormDestroy(Sender: TObject);
begin
  if Assigned(dsJSON) then
  begin
    dsJSON.Close;
    dsJSON.Free;
  end;

  if Assigned(FFieldMap) then
    FFieldMap.Free;
  if Assigned(TablesDict) then
    FreeAndNil(TablesDict);
end;

procedure TdlgImport.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  LoadTargetTables;
end;

function TdlgImport.IsRequiredFilledSource: Boolean;
begin
  Result := False;

  if (eSourceFile.Text <> EmptyStr) and (cbTarget.ItemIndex >= 0) then
    Result := True;
end;

procedure TdlgImport.LoadFields;
var
  i: Integer;
begin
  if not FDataSet.Active then
    FDataSet.Open;

  gridFields.BeginUpdate;
  try
    gridFields.ColWidths[0] := 40;
    gridFields.RowCount := 1; // Clear rows
    gridFields.RowCount := FDataSet.FieldCount + 1;

    for i := 0 to FDataSet.FieldCount - 1 do
    begin
      gridFields.Cells[1, i+1] := FDataSet.Fields[i].DisplayLabel;
    end;

    // Target field picklist
    LoadTargetFields;

    // Search table picklist
    LoadSearchTables;
  finally
    gridFields.EndUpdate;
  end;
end;

procedure TdlgImport.LoadSearchTables;
begin
  with gridFields.Columns[3].PickList do
  begin
    Add(rsCaptionExpeditions);
    Add(rsTitleSurveys);
    Add(rsTitleSurveyTeam);
    Add(rsTitleNetsEffort);
    Add(rsTitleWeather);
    Add(rsTitleVegetation);
    Add(rsTitleMethods);
    Add(rsTitleSightings);
    Add(rsTitleSpecimens);
    Add(rsTitleSamplePreps);
    Add(rsTitleCollectors);
    Add(rsTitleBands);
    Add(rsTitleIndividuals);
    Add(rsTitleCaptures);
    Add(rsTitleMolts);
    Add(rsTitleNests);
    Add(rsTitleNestOwners);
    Add(rsTitleNestRevisions);
    Add(rsTitleEggs);
    Add(rsTitleInstitutions);
    Add(rsTitleResearchers);
    Add(rsTitleProjects);
    Add(rsTitleProjectMembers);
    Add(rsTitlePermits);
    Add(rsTitleGazetteer);
    Add(rsTitleSamplingPlots);
    Add(rsTitlePermanentNets);
    Add(rsTitleBotanicalTaxa);
  end;
end;

procedure TdlgImport.LoadTargetFields;
var
  FDS: TDataSet;
  i: Integer;
begin
  FDS := nil;

  if not (Assigned(DMS)) then
    DMS := TDMS.Create(Application);

  case FTableType of
    tbNone: ;
    tbUsers: ;
    tbRecordHistory: ;
    tbRecordVerifications: ;
    tbGazetteer:            FDS := DMG.qGazetteer;
    tbSamplingPlots:        FDS := DMG.qSamplingPlots;
    tbPermanentNets:        FDS := DMG.qPermanentNets;
    tbInstitutions:         FDS := DMG.qInstitutions;
    tbPeople:               FDS := DMG.qPeople;
    tbProjects:             FDS := DMG.qProjects;
    tbProjectTeams:         FDS := DMG.qProjectTeam;
    tbPermits:              FDS := DMG.qPermits;
    tbTaxonRanks: ;
    tbZooTaxa: ;
    tbBotanicTaxa:          FDS := DMG.qBotany;
    tbBands:                FDS := DMG.qBands;
    tbBandHistory: ;
    tbIndividuals:          FDS := DMG.qIndividuals;
    tbCaptures:             FDS := DMG.qCaptures;
    tbFeathers:             FDS := DMG.qFeathers;
    tbNests:                FDS := DMG.qNests;
    tbNestOwners: ;
    tbNestRevisions:        FDS := DMG.qNestRevisions;
    tbEggs:                 FDS := DMG.qEggs;
    tbMethods:              FDS := DMG.qMethods;
    tbExpeditions:          FDS := DMG.qExpeditions;
    tbSurveys:              FDS := DMG.qSurveys;
    tbSurveyTeams: ;
    tbNetsEffort:           FDS := DMS.qNetsEffort;
    tbWeatherLogs:          FDS := DMS.qWeatherLogs;
    tbSightings:            FDS := DMG.qSightings;
    tbSpecimens:            FDS := DMG.qSpecimens;
    tbSamplePreps:          FDS := DMG.qSamplePreps;
    tbSpecimenCollectors:   FDS := DMG.qSampleCollectors;
    tbImages:               FDS := DMG.qImages;
    tbAudioLibrary:         FDS := DMG.qAudio;
    tbDocuments:            FDS := DMG.qDocuments;
    tbVegetation:           FDS := DMS.qVegetation;
  end;

  gridFields.Columns[3].PickList.Clear;
  for i := 0 to FDS.FieldCount - 1 do
  begin
    FFieldMap.Add(FDS.Fields[i].DisplayLabel, FDS.Fields[i].FieldName);
    gridFields.Columns[3].PickList.Add(FDS.Fields[i].DisplayLabel);
  end;

end;

procedure TdlgImport.LoadTargetTables;
begin
  with cbTarget.Items do
  begin
    Add(rsCaptionExpeditions);
    Add(rsTitleSurveys);
    Add(rsTitleSurveyTeam);
    Add(rsTitleNetsEffort);
    Add(rsTitleWeather);
    Add(rsTitleVegetation);
    Add(rsTitleMethods);
    Add(rsTitleSightings);
    Add(rsTitleSpecimens);
    Add(rsTitleSamplePreps);
    Add(rsTitleCollectors);
    Add(rsTitleBands);
    Add(rsTitleIndividuals);
    Add(rsTitleCaptures);
    Add(rsTitleMolts);
    Add(rsTitleNests);
    Add(rsTitleNestOwners);
    Add(rsTitleNestRevisions);
    Add(rsTitleEggs);
    Add(rsTitleInstitutions);
    Add(rsTitleResearchers);
    Add(rsTitleProjects);
    Add(rsTitleProjectMembers);
    Add(rsTitlePermits);
    Add(rsTitleGazetteer);
    Add(rsTitleSamplingPlots);
    Add(rsTitlePermanentNets);
    Add(rsTitleBotanicalTaxa);
  end;
end;

procedure TdlgImport.pmfDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  gridFields.BeginUpdate;
  try
    for i := 1 to gridFields.RowCount - 1 do
      gridFields.Cells[2, i] := '0';
  finally
    gridFields.EndUpdate;
  end;
end;

procedure TdlgImport.pmfSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  gridFields.BeginUpdate;
  try
    for i := 1 to gridFields.RowCount - 1 do
      gridFields.Cells[2, i] := '1';
  finally
    gridFields.EndUpdate;
  end;
end;

procedure TdlgImport.sbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdlgImport.sbNextClick(Sender: TObject);
begin
  nbPages.PageIndex := nbPages.PageIndex + 1;

  if nbPages.PageIndex = 2 then
  begin

  end;

  if nbPages.PageIndex = 1 then
  begin
    LoadFields;
  end;

  sbPrior.Enabled := nbPages.PageIndex > 0;
  sbNext.Enabled := nbPages.PageIndex < (nbPages.PageCount - 1);
end;

procedure TdlgImport.sbPriorClick(Sender: TObject);
begin
  nbPages.PageIndex := nbPages.PageIndex - 1;

  sbPrior.Enabled := nbPages.PageIndex > 0;
  sbNext.Enabled := nbPages.PageIndex < (nbPages.PageCount - 1);
end;

procedure TdlgImport.sbSaveLogClick(Sender: TObject);
begin
  if SaveDlg.Execute then
  begin
    mProgress.Lines.SaveToFile(SaveDlg.FileName);
    OpenDocument(SaveDlg.FileName);
  end;
end;

function TdlgImport.ValidateFields: Boolean;
begin
  Result := True;
  { #todo : Validate values before import }
end;

end.

