{ Xolmis Main form

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

unit ufrm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LCLIntf, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, DB, Buttons,
  ActnList, ExtCtrls, StdCtrls, atTabs, atshapelinebgra, BCPanel, BCButton, ColorSpeedButton, DateUtils,
  DefaultTranslator, ufrm_customgrid, TDICardPanel, udlg_rechistory,
  cbs_datatypes, Types, ImgList;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actInsertRecord: TAction;
    actAbout: TAction;
    actGiveFeedback: TAction;
    actCoordinatesEditor: TAction;
    actCoordinatesConverter: TAction;
    actExit: TAction;
    actCheckUpdates: TAction;
    actDBSettings: TAction;
    actDBConnect: TAction;
    actExport: TAction;
    actImportEbird: TAction;
    actImportCaptures: TAction;
    actImport: TAction;
    actImportNests: TAction;
    actImportWizard: TAction;
    actImportCoordinates: TAction;
    actMaintenance: TAction;
    actOpenBandHistory: TAction;
    actOpenTaxonRanks: TAction;
    actViewBandsBalance: TAction;
    actPrint: TAction;
    actOpenMolts: TAction;
    actOpenEggs: TAction;
    actOpenNestRevisions: TAction;
    actNewBands: TAction;
    actLogoff: TAction;
    actManageUsers: TAction;
    actOpenDocumentsList: TAction;
    actOpenRecordingsList: TAction;
    actOpenImagesList: TAction;
    actOpenManual: TAction;
    actOpenMethods: TAction;
    actOpenBotany: TAction;
    actOpenTaxa: TAction;
    actOpenNetStations: TAction;
    actOpenGazetteer: TAction;
    actOpenPermits: TAction;
    actOpenProjects: TAction;
    actOpenInstitutions: TAction;
    actOpenResearchers: TAction;
    actOpenLiterature: TAction;
    actOpenSpecimens: TAction;
    actOpenNests: TAction;
    actOpenCaptures: TAction;
    actOpenIndividuals: TAction;
    actOpenSightings: TAction;
    actOpenSurveys: TAction;
    actOpenExpeditions: TAction;
    actSettings: TAction;
    actOpenBands: TAction;
    ActList: TActionList;
    AppEvents: TApplicationProperties;
    bStatusBarDark: TImageList;
    iMenuDark: TImageList;
    iPopupDark: TImageList;
    iSearch: TImageList;
    iMenu: TImageList;
    iPopup: TImageList;
    iSearchDark: TImageList;
    mmhCheckUpdates: TMenuItem;
    mmfImportCoordinates: TMenuItem;
    mmFile: TMenuItem;
    mmHelp: TMenuItem;
    mmsExpeditions: TMenuItem;
    mmsSurveys: TMenuItem;
    mmsSightings: TMenuItem;
    mmsMethods: TMenuItem;
    mmsSpecimens: TMenuItem;
    mmiBands: TMenuItem;
    mmiNewBandsBatch: TMenuItem;
    mmiBandsHistory: TMenuItem;
    mmiBandsBalance: TMenuItem;
    mmSampling: TMenuItem;
    mmiIndividuals: TMenuItem;
    mmiCaptures: TMenuItem;
    mmiMolts: TMenuItem;
    mmbNests: TMenuItem;
    mmbNestRevisions: TMenuItem;
    mmbEggs: TMenuItem;
    mmeInstitutions: TMenuItem;
    mmeResearchers: TMenuItem;
    mmeProjects: TMenuItem;
    mmePermits: TMenuItem;
    mmIndividuals: TMenuItem;
    mmgGazetteer: TMenuItem;
    mmgSamplingPlots: TMenuItem;
    mmgCoordinatesConverter: TMenuItem;
    mmtTaxa: TMenuItem;
    mmtBotanicTaxa: TMenuItem;
    mmhHelp: TMenuItem;
    mmhFeedback: TMenuItem;
    mmhAbout: TMenuItem;
    mmMedia: TMenuItem;
    mmmImageGallery: TMenuItem;
    mmBreeding: TMenuItem;
    mmmAudioLibrary: TMenuItem;
    mmmAttachments: TMenuItem;
    mmfConnectDB: TMenuItem;
    mmfManageConnections: TMenuItem;
    mmfExport: TMenuItem;
    mmfImport: TMenuItem;
    mmfImportEbird: TMenuItem;
    mmfImportCaptures: TMenuItem;
    mmfImportNests: TMenuItem;
    mmfImportWizard: TMenuItem;
    mmEntities: TMenuItem;
    mmfSettings: TMenuItem;
    mmfMaintenance: TMenuItem;
    mmfManageUsers: TMenuItem;
    mmfLogoff: TMenuItem;
    mmfExit: TMenuItem;
    mmGeo: TMenuItem;
    mmTaxonomy: TMenuItem;
    mMenu: TMainMenu;
    navTabs: TATTabs;
    sbClearSearch: TColorSpeedButton;
    icoSbarDatabase: TImage;
    icoSbarUser: TImage;
    icoSbarTaxonomy: TImage;
    imgSplash: TImage;
    lblSbarDatabase: TLabel;
    lblSbarUser: TLabel;
    lblSbarTaxonomy: TLabel;
    lblLoading: TLabel;
    progressBar: TProgressBar;
    pSplash: TPanel;
    sbarDatabase: TBCPanel;
    SBar: TBCPanel;
    eSkip: TEdit;
    eSearch: TEdit;
    iconSearch: TImage;
    pmaNewExpedition: TMenuItem;
    pmaNewSighting: TMenuItem;
    pmaNewSpecimen: TMenuItem;
    pmaNewBand: TMenuItem;
    pmaNewIndividual: TMenuItem;
    pmaNewCapture: TMenuItem;
    pmaNewMolt: TMenuItem;
    pmaNewNest: TMenuItem;
    pmaNewEgg: TMenuItem;
    pmaNewInstitution: TMenuItem;
    pmaNewResearcher: TMenuItem;
    pmaNewProject: TMenuItem;
    pmaNewPermit: TMenuItem;
    pmaNewToponym: TMenuItem;
    pmaNewNetStation: TMenuItem;
    pmaNewSurvey: TMenuItem;
    pmAddMenu: TPopupMenu;
    pSearch: TBCPanel;
    sbarProgress: TBCPanel;
    sbarUser: TBCPanel;
    sbarTaxonomy: TBCPanel;
    sbarStatus: TBCPanel;
    pMainMenu: TPanel;
    pmtCloseTab: TMenuItem;
    pmtCloseAllTabs: TMenuItem;
    pmtCloseAllOtherTabs: TMenuItem;
    pmTabs: TPopupMenu;
    bStatusBar: TImageList;
    pLeftTabs: TPanel;
    sbHome: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    Separator1: TMenuItem;
    Separator11: TMenuItem;
    Separator12: TMenuItem;
    Separator13: TMenuItem;
    Separator14: TMenuItem;
    Separator15: TMenuItem;
    Separator16: TMenuItem;
    Separator17: TMenuItem;
    Separator18: TMenuItem;
    Separator19: TMenuItem;
    Separator2: TMenuItem;
    Separator20: TMenuItem;
    Separator21: TMenuItem;
    Separator22: TMenuItem;
    Separator23: TMenuItem;
    Separator24: TMenuItem;
    Separator25: TMenuItem;
    Separator26: TMenuItem;
    Separator27: TMenuItem;
    Separator29: TMenuItem;
    Separator31: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    PGW: TTDICardPanel;
    TimerScreen: TTimer;
    TimerAnimSearch: TTimer;
    TimerFind: TTimer;
    procedure actAboutExecute(Sender: TObject);
    procedure actCheckUpdatesExecute(Sender: TObject);
    procedure actCoordinatesConverterExecute(Sender: TObject);
    procedure actDBConnectExecute(Sender: TObject);
    procedure actDBSettingsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actGiveFeedbackExecute(Sender: TObject);
    procedure actImportCapturesExecute(Sender: TObject);
    procedure actImportCoordinatesExecute(Sender: TObject);
    procedure actImportEbirdExecute(Sender: TObject);
    procedure actImportNestsExecute(Sender: TObject);
    procedure actInsertRecordExecute(Sender: TObject);
    procedure actLogoffExecute(Sender: TObject);
    procedure actMaintenanceExecute(Sender: TObject);
    procedure actManageUsersExecute(Sender: TObject);
    procedure actNewBandsExecute(Sender: TObject);
    procedure actOpenBandHistoryExecute(Sender: TObject);
    procedure actOpenBandsExecute(Sender: TObject);
    procedure actOpenBotanyExecute(Sender: TObject);
    procedure actOpenCapturesExecute(Sender: TObject);
    procedure actOpenEggsExecute(Sender: TObject);
    procedure actOpenExpeditionsExecute(Sender: TObject);
    procedure actOpenGazetteerExecute(Sender: TObject);
    procedure actOpenIndividualsExecute(Sender: TObject);
    procedure actOpenInstitutionsExecute(Sender: TObject);
    procedure actOpenManualExecute(Sender: TObject);
    procedure actOpenMethodsExecute(Sender: TObject);
    procedure actOpenNestRevisionsExecute(Sender: TObject);
    procedure actOpenNestsExecute(Sender: TObject);
    procedure actOpenNetStationsExecute(Sender: TObject);
    procedure actOpenPermitsExecute(Sender: TObject);
    procedure actOpenProjectsExecute(Sender: TObject);
    procedure actOpenResearchersExecute(Sender: TObject);
    procedure actOpenSightingsExecute(Sender: TObject);
    procedure actOpenSpecimensExecute(Sender: TObject);
    procedure actOpenSurveysExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actViewBandsBalanceExecute(Sender: TObject);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure eSearchChange(Sender: TObject);
    procedure eSearchEnter(Sender: TObject);
    procedure eSearchExit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure iSearchGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer;
      var AResultWidth: Integer);
    procedure navTabsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure navTabsTabChanged(Sender: TObject);
    procedure navTabsTabClose(Sender: TObject; ATabIndex: integer; var ACanClose,
      ACanContinue: boolean);
    procedure pmaNewBandClick(Sender: TObject);
    procedure pmaNewCaptureClick(Sender: TObject);
    procedure pmaNewEggClick(Sender: TObject);
    procedure pmaNewExpeditionClick(Sender: TObject);
    procedure pmaNewIndividualClick(Sender: TObject);
    procedure pmaNewInstitutionClick(Sender: TObject);
    procedure pmaNewMoltClick(Sender: TObject);
    procedure pmaNewNestClick(Sender: TObject);
    procedure pmaNewNetStationClick(Sender: TObject);
    procedure pmaNewPermitClick(Sender: TObject);
    procedure pmaNewProjectClick(Sender: TObject);
    procedure pmaNewResearcherClick(Sender: TObject);
    procedure pmaNewSightingClick(Sender: TObject);
    procedure pmaNewSpecimenClick(Sender: TObject);
    procedure pmaNewSurveyClick(Sender: TObject);
    procedure pmaNewToponymClick(Sender: TObject);
    procedure pmtCloseAllOtherTabsClick(Sender: TObject);
    procedure pmtCloseAllTabsClick(Sender: TObject);
    procedure pmtCloseTabClick(Sender: TObject);
    procedure sbClearSearchClick(Sender: TObject);
    procedure sbHomeClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure TimerScreenTimer(Sender: TObject);
  private
    //ActiveQuery: TSQLQuery;
    ActiveGrid: TfrmCustomGrid;
    procedure OpenTab(Sender: TObject; aForm: TForm; aFormClass: TComponentClass; aCaption: String;
      Pinned: Boolean);
    procedure OpenForm(Sender: TObject; var aForm: TfrmCustomGrid; aTableType: TTableType;
      aCaption: String; aIcon: Integer = -1); overload;
    //procedure OpenIndividuals(Sender: TObject; var aForm: TfrmIndividuals; aTableType: TTableType;
    //  aCaption: String; aIcon: Integer = -1); overload;
    procedure CloseAllTabs(ClosePinned: Boolean = False; ExceptIndex: Integer = -1);
    procedure ApplyDarkMode;
  public
    procedure CarregaPref;
    procedure UpdateStatusBar;
    procedure UpdateMenu(aTab: TPage);
  end;

var
  frmMain: TfrmMain;
  fGazetteer: TfrmCustomGrid;
  fNetStations: TfrmCustomGrid;
  fInstitutions: TfrmCustomGrid;
  fPeople: TfrmCustomGrid;
  fProjects: TfrmCustomGrid;
  fPermits: TfrmCustomGrid;
  fTaxonRanks: TfrmCustomGrid;
  fBotanicTaxa: TfrmCustomGrid;
  fZooTaxa: TfrmCustomGrid;
  fBands: TfrmCustomGrid;
  fIndividuals: TfrmCustomGrid;
  fCaptures: TfrmCustomGrid;
  fMolts: TfrmCustomGrid;
  fNests: TfrmCustomGrid;
  fNestRevisions: TfrmCustomGrid;
  fEggs: TfrmCustomGrid;
  fMethods: TfrmCustomGrid;
  fExpeditions: TfrmCustomGrid;
  fSurveys: TfrmCustomGrid;
  fSightings: TfrmCustomGrid;
  fSpecimens: TfrmCustomGrid;
  fViewHistory: TdlgRecHistory;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_system, cbs_import, cbs_autoupdate, cbs_permissions, cbs_backup,
  cbs_gis, cbs_taxonomy, cbs_editdialogs, cbs_themes, uDarkStyleParams,
  udm_main, udm_lookup, udm_grid, udm_sampling, udm_individuals, udm_breeding,
  ucfg_database, ucfg_users, ucfg_options,
  ubatch_bands, udlg_about, udlg_bandsbalance, udlg_bandhistory, udlg_importcaptures, udlg_importnests,
  ufrm_geoconverter, ufrm_dashboard, ufrm_maintenance;

{$R *.lfm}

//procedure SetDarkStyle;
//begin
//  case XSettings.SelectedTheme of
//    0: PreferredAppMode := pamDefault;
//    1: PreferredAppMode := pamAllowDark;
//    2: PreferredAppMode := pamForceDark;
//    3: PreferredAppMode := pamForceLight;
//  end;
//
//  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
//end;

{ TfrmMain }

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  AbreForm(TdlgAbout, dlgAbout);
end;

procedure TfrmMain.actCheckUpdatesExecute(Sender: TObject);
begin
  case CheckUpdates of
    ckrNone: ;
    ckrUpdated: MsgDlg(rsCheckUpdates, rsIsUpToDate, mtInformation);
    ckrNewVersion:
    begin
      if MsgDlg(rsCheckUpdates, Format(rsNewUpdateAvailable, [NomeApp]), mtConfirmation) then
        RunUpdate;
    end;
    ckrError: ;
  end;
end;

procedure TfrmMain.actCoordinatesConverterExecute(Sender: TObject);
begin
  OpenTab(Sender, frmGeoConverter, TfrmGeoConverter, rsTitleCoordinateConverter, False);
end;

procedure TfrmMain.actDBConnectExecute(Sender: TObject);
begin
  if ConnectDatabase then
  begin
    CloseAllTabs;

    UpdateMenu(PGW.ActivePageComponent);
    UpdateStatusBar;
  end;
end;

procedure TfrmMain.actDBSettingsExecute(Sender: TObject);
begin
  AbreForm(TcfgDatabase, cfgDatabase);
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actGiveFeedbackExecute(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/issues');
end;

procedure TfrmMain.actImportCapturesExecute(Sender: TObject);
begin
  //DMM.OpenCsvDlg.Title := rsTitleImportFile;
  //if DMM.OpenCsvDlg.Execute then
  //begin
  //  ImportBandingDataV1(DMM.OpenCsvDlg.Filename);
  //end;

  dlgImportCaptures := TdlgImportCaptures.Create(nil);
  with dlgImportCaptures do
  try
    ShowModal;
  finally
    FreeAndNil(dlgImportCaptures);
  end;
end;

procedure TfrmMain.actImportCoordinatesExecute(Sender: TObject);
var
  Ext: String;
begin
  DMM.OpenKmlDlg.Title := rsTitleImportFile;
  if DMM.OpenKmlDlg.Execute then
  begin
    Ext := ExtractFileExt(DMM.OpenKmlDlg.FileName);
    case Ext of
      '.kml', '.kmz': LoadKMLPoints(DMM.OpenKmlDlg.FileName);
      '.csv', '.txt': LoadCSVPoints(DMM.OpenKmlDlg.FileName);
      '.gpx':         LoadGPXPoints(DMM.OpenKmlDlg.FileName);
      '.geojson':     LoadGeoJSONPoints(DMM.OpenKmlDlg.FileName);
    end;
  end;
end;

procedure TfrmMain.actImportEbirdExecute(Sender: TObject);
begin
  DMM.OpenCsvDlg.Title := rsTitleImportFile;
  if DMM.OpenCsvDlg.Execute then
  begin
    ImportEbirdData(DMM.OpenCsvDlg.Filename);
  end;
end;

procedure TfrmMain.actImportNestsExecute(Sender: TObject);
begin
  dlgImportNests := TdlgImportNests.Create(nil);
  with dlgImportNests do
  try
    ShowModal;
  finally
    FreeAndNil(dlgImportNests);
  end;
end;

procedure TfrmMain.actInsertRecordExecute(Sender: TObject);
begin
  with sbInsertRecord.ClientToScreen(point(0, sbInsertRecord.Height + 1)) do
    pmAddMenu.Popup(X, Y);
end;

procedure TfrmMain.actLogoffExecute(Sender: TObject);
var
  OldUser: TUser;
begin
  GravaStat(Name, (Sender as TComponent).Name, 'click');
  // Change user
  OldUser := TUser.Create(ActiveUser.Id);
  try
    if UserLogin(0) then
    begin
      {$IFDEF DEBUG}
      LogDebug(Format('User changed from %d to %d', [OldUser.Id, ActiveUser.Id]));
      {$ENDIF}
      CarregaPref;
    end;
  finally
    FreeAndNil(OldUser);
  end;
end;

procedure TfrmMain.actMaintenanceExecute(Sender: TObject);
begin
  AbreForm(TfrmMaintenance, frmMaintenance);
end;

procedure TfrmMain.actManageUsersExecute(Sender: TObject);
begin
  AbreForm(TcfgUsers, cfgUsers);
end;

procedure TfrmMain.actNewBandsExecute(Sender: TObject);
begin
  Application.CreateForm(TbatchBands, batchBands);
  with batchBands do
  try
    ShowModal;
  finally
    FreeAndNil(batchBands);
  end;
end;

procedure TfrmMain.actOpenBandHistoryExecute(Sender: TObject);
begin
  AbreForm(TdlgBandHistory, dlgBandHistory);
end;

procedure TfrmMain.actOpenBandsExecute(Sender: TObject);
begin
  OpenForm(Sender, fBands, tbBands, rsTitleBands, actOpenBands.ImageIndex);
end;

procedure TfrmMain.actOpenBotanyExecute(Sender: TObject);
begin
  OpenForm(Sender, fBotanicTaxa, tbBotanicTaxa, rsTitleBotanicTaxa, actOpenBotany.ImageIndex);
end;

procedure TfrmMain.actOpenCapturesExecute(Sender: TObject);
begin
  OpenForm(Sender, fCaptures, tbCaptures, rsTitleCaptures, actOpenCaptures.ImageIndex);
end;

procedure TfrmMain.actOpenEggsExecute(Sender: TObject);
begin
  OpenForm(Sender, fEggs, tbEggs, rsTitleEggs, actOpenEggs.ImageIndex);
end;

procedure TfrmMain.actOpenExpeditionsExecute(Sender: TObject);
begin
  OpenForm(Sender, fExpeditions, tbExpeditions, rsCaptionExpeditions, actOpenExpeditions.ImageIndex);
end;

procedure TfrmMain.actOpenGazetteerExecute(Sender: TObject);
begin
  OpenForm(Sender, fGazetteer, tbGazetteer, rsTitleGazetteer, actOpenGazetteer.ImageIndex);
end;

procedure TfrmMain.actOpenIndividualsExecute(Sender: TObject);
begin
  OpenForm(Sender, fIndividuals, tbIndividuals, rsTitleIndividuals, actOpenIndividuals.ImageIndex);
  //OpenIndividuals(Sender, fIndividuals, tbIndividuals, rsTitleIndividuals, actOpenIndividuals.ImageIndex);
end;

procedure TfrmMain.actOpenInstitutionsExecute(Sender: TObject);
begin
  OpenForm(Sender, fInstitutions, tbInstitutions, rsTitleInstitutions, actOpenInstitutions.ImageIndex);
end;

procedure TfrmMain.actOpenManualExecute(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/wiki');
end;

procedure TfrmMain.actOpenMethodsExecute(Sender: TObject);
begin
  OpenForm(Sender, fMethods, tbMethods, rsTitleMethods, actOpenMethods.ImageIndex);
end;

procedure TfrmMain.actOpenNestRevisionsExecute(Sender: TObject);
begin
  OpenForm(Sender, fNestRevisions, tbNestRevisions, rsTitleNestRevisions, actOpenNestRevisions.ImageIndex);
end;

procedure TfrmMain.actOpenNestsExecute(Sender: TObject);
begin
  OpenForm(Sender, fNests, tbNests, rsTitleNests, actOpenNests.ImageIndex);
end;

procedure TfrmMain.actOpenNetStationsExecute(Sender: TObject);
begin
  OpenForm(Sender, fNetStations, tbNetStations, rsTitleSamplingPlots, actOpenNetStations.ImageIndex);
end;

procedure TfrmMain.actOpenPermitsExecute(Sender: TObject);
begin
  OpenForm(Sender, fPermits, tbPermits, rsTitlePermits, actOpenPermits.ImageIndex);
end;

procedure TfrmMain.actOpenProjectsExecute(Sender: TObject);
begin
  OpenForm(Sender, fProjects, tbProjects, rsTitleProjects, actOpenProjects.ImageIndex);
end;

procedure TfrmMain.actOpenResearchersExecute(Sender: TObject);
begin
  OpenForm(Sender, fPeople, tbPeople, rsTitleResearchers, actOpenResearchers.ImageIndex);
end;

procedure TfrmMain.actOpenSightingsExecute(Sender: TObject);
begin
  OpenForm(Sender, fSightings, tbSightings, rsTitleSightings, actOpenSightings.ImageIndex);
end;

procedure TfrmMain.actOpenSpecimensExecute(Sender: TObject);
begin
  OpenForm(Sender, fSpecimens, tbSpecimens, rsTitleSpecimens, actOpenSpecimens.ImageIndex);
end;

procedure TfrmMain.actOpenSurveysExecute(Sender: TObject);
begin
  OpenForm(Sender, fSurveys, tbSurveys, rsTitleSurveys, actOpenSurveys.ImageIndex);
end;

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  AbreForm(TcfgOptions, cfgOptions);

  CarregaPref;
end;

procedure TfrmMain.actViewBandsBalanceExecute(Sender: TObject);
begin
  AbreForm(TdlgBandsBalance, dlgBandsBalance);
end;

procedure TfrmMain.AppEventsException(Sender: TObject; E: Exception);
begin
  { Log error message }
  LogError(E.Message);
  MsgDlg(rsTitleError, E.Message, mtError);
end;

procedure TfrmMain.ApplyDarkMode;
begin
  pSplash.Color := clSmokeBGDefaultDark;
  pMainMenu.Color := clSolidBGQuaternaryDark;
  navTabs.ColorBg := clSolidBGQuaternaryDark;
  navTabs.ColorTabActive := clSolidBGBaseDark;
  navTabs.ColorTabOver := clSolidBGTertiaryDark;
  navTabs.ColorTabPassive := clSolidBGBaseDark;
  navTabs.ColorFontActive := clTextPrimaryDark;
  navTabs.ColorFontHot := clTextPrimaryDark;

  mMenu.Images := iMenuDark;
  pmTabs.Images := iPopupDark;
  pmAddMenu.Images := DMM.iAddMenuDark;

  icoSbarDatabase.Images := bStatusBarDark;
  icoSbarUser.Images := bStatusBarDark;
  icoSbarTaxonomy.Images := bStatusBarDark;
  sbarDatabase.Border.Color := clSolidBGSecondaryDark;
  sbarUser.Border.Color := clSolidBGSecondaryDark;
  sbarTaxonomy.Border.Color := clSolidBGSecondaryDark;
  sbarStatus.Border.Color := clSolidBGSecondaryDark;
  sbarStatus.FontEx.Color := clTextPrimaryDark;
  sbarProgress.Border.Color := clSolidBGSecondaryDark;

  pSearch.Background.Color := clCardBGDefaultDark;
  pSearch.Border.Color := clSolidBGSecondaryDark;
  pSearch.ParentBackground := True;
  eSearch.Color := pSearch.Background.Color;
  iconSearch.Images := iSearchDark;
  sbClearSearch.Images := iSearchDark;
  sbClearSearch.StateHover.Color := clSolidBGSecondaryDark;
  sbClearSearch.StateActive.Color := clSolidBGTertiaryDark;
  sbClearSearch.StateNormal.Color := pSearch.Background.Color;
end;

procedure TfrmMain.CarregaPref;
begin
  // Update active taxonomy
  ActiveTaxonomy := XSettings.Taxonomy;
  // SBarTaxonomy.Caption:= TaxonomyName[ActiveTaxonomy];

  // Get user permissions
  actMaintenance.Enabled := ActiveUser.Rank = 'A';
  actExport.Enabled := ActiveUser.AllowExport;
  actImport.Enabled := ActiveUser.AllowImport;
  actPrint.Enabled := ActiveUser.AllowPrint;

  // Update status bar
  UpdateStatusBar;
end;

procedure TfrmMain.CloseAllTabs(ClosePinned: Boolean; ExceptIndex: Integer);
var
  i: Integer;
begin
  if navTabs.TabCount < 1 then
    Exit;

  navTabs.Tabs.BeginUpdate;
  try
    for i := (navTabs.TabCount - 1) downto 0 do
      if not (navTabs.GetTabData(i).TabPinned) or (ClosePinned) or (Closing) then
        if not (navTabs.GetTabData(i).Index = ExceptIndex) then
          if not navTabs.DeleteTab(i, True, False, aocNone) then
            Break;
  finally
    navTabs.Tabs.EndUpdate;
  end;

  if not (Closing) and (navTabs.TabCount > 0) then
  begin
    navTabs.TabIndex := PGW.PageIndex;
    UpdateMenu(PGW.ActivePageComponent);
  end;
end;

procedure TfrmMain.eSearchChange(Sender: TObject);
begin
  TimerFind.Enabled := False;
  TimerFind.Enabled := True;

  sbClearSearch.Visible := Length(eSearch.Text) > 0;
end;

procedure TfrmMain.eSearchEnter(Sender: TObject);
begin
  if eSearch.Text = EmptyStr then
    pSearch.Width := ClientWidth div 4;
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
  //TimerAnimSearch.Enabled := True;
end;

procedure TfrmMain.eSearchExit(Sender: TObject);
begin
  if eSearch.Text = EmptyStr then
    pSearch.Width := 148;
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
  //TimerAnimSearch.Enabled := True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Closing := True;
  TimerScreen.Enabled := False;

  CloseAllTabs(True);

  lblLoading.Caption := rsClosing;
  pSplash.Visible := True;

  { Run backup }
  case XSettings.AutomaticBackup of
    0: ;
    1:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 1 then
        NewBackup;
    end;
    2:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 7 then
        NewBackup;
    end;
    3:
    begin
      if DaysBetween(Now, ConexaoDB.LastBackup) >= 30 then
        NewBackup;
    end;
  end;

  LogInfo('END -----------------------------------------');
  if Assigned(XSettings) then
  begin
    XSettings.AppTerminatedOk := True;
    XSettings.SaveToFile;
    XSettings.Free;
  end;

  CloseAction := caFree;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Closing := False;
  Opening := False;
  Working := False;

  {$IFDEF DEBUG}
  LogDebug('Opening the main form');
  {$ENDIF}

  { Get the last session termination status }
  Finalizado := XSettings.AppTerminatedOk;

  { Delete temporary files }
  //if FileExists(ConcatPaths([TempDir, 'xolmis_setup.exe'])) then
  //begin
  //  DeleteFile(ConcatPaths([TempDir, 'xolmis_setup.exe']));
  //  LogInfo('Xolmis updated to the newest version ' + GetBuildInfoAsString);
    //MsgDlg(Format(rsSuccessfulUpdate, [NomeApp]),
    //  Format(rsUpdatedNewVersion, [GetBuildInfoAsString]), mtInformation);
  //end;
  Application.ProcessMessages;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(DMB) then
    FreeAndNil(DMB);
  if Assigned(DMI) then
    FreeAndNil(DMI);
  if Assigned(DMS) then
    FreeAndNil(DMS);

  if Assigned(ActiveUser) then
    FreeAndNil(ActiveUser);
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    sbClearSearchClick(nil);  { Clear search }

    Key := #0;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  pSplash.Top := 0;
  pSplash.Left := 0;
  pSplash.Height := Self.ClientHeight;
  pSplash.Width := Self.ClientWidth;
  pSplash.Visible := True;
  Application.ProcessMessages;

  OldPPI := Self.PixelsPerInch;
  if OldPPI <> 96 then
  begin
    navTabs.OptScalePercents := (OldPPI * 100) div 96;
    //navTabs.OptFontScale := (OldPPI * 100) div 96;
  end;
  //navTabs.Height := (navTabs.OptTabHeight + navTabs.OptSpacer);
  //TimerScreen.Enabled := True;

  { Check if there are connections available }
  DMM.qsConn.Open;
  if DMM.qsConn.RecordCount = 0 then
    if not FirstConfig then
      Application.Terminate;
  DMM.qsConn.Refresh;
  Application.ProcessMessages;

  { Check if there are users available }
  //if not Application.Terminated then
  //  if DMM.qUsers.RecordCount = 0 then
  //    if not DatabaseConfig then
  //      Application.Terminate;
  //Application.ProcessMessages;

  { Open connection dialog and try to connect to database }
  if not Application.Terminated then
    if not ConnectDatabase then
      Application.Terminate;

  if not Application.Terminated then
  begin
    { Apply the active user settings }
    CarregaPref;

    { Load the start page }
    //if not Assigned(DMC) then
    //  DMC := TDMC.Create(Application);
    OpenTab(Sender, frmDashboard, TfrmDashboard, rsHome, True);
    Application.ProcessMessages;

    { Load data module for the main forms }
    if not Assigned(DMG) then
      DMG := TDMG.Create(Application);
    Application.ProcessMessages;

    { Check for updates }
    case XSettings.AutoUpdates of
      0: ;
      1:
      begin
        if DaysBetween(Now, XSettings.LastAutoUpdate) >= 1 then
          actCheckUpdatesExecute(nil);
      end;
      2:
      begin
        if DaysBetween(Now, XSettings.LastAutoUpdate) >= 7 then
          actCheckUpdatesExecute(nil);
      end;
      3:
      begin
        if DaysBetween(Now, XSettings.LastAutoUpdate) >= 30 then
          actCheckUpdatesExecute(nil);
      end;
    end;

    {$IFDEF DEBUG}
    LogDebug('Main form opened');
    {$ENDIF}
    Finalizado := False;
    XSettings.AppTerminatedOk := False;
    XSettings.SaveToFile;

    pSplash.Visible := False;
    lblLoading.Caption := rsLoading;
    imgSplash.ImageIndex := 5;
    imgSplash.ImageWidth := 128;
  end
  else
  begin
    LogInfo('END -----------------------------------------');
    if Assigned(XSettings) then
      XSettings.Free;
    if Assigned(ActiveUser) then
      FreeAndNil(ActiveUser);
  end;

  //SmokeScreen.Visible := False;
end;

procedure TfrmMain.iSearchGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer;
  var AResultWidth: Integer);
begin
  AResultWidth := AImageWidth * APPI div 96;
end;

procedure TfrmMain.navTabsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  pmtCloseTab.Visible := not (navTabs.GetTabData(navTabs.TabIndex).TabPinned);
end;

procedure TfrmMain.navTabsTabChanged(Sender: TObject);
var
  aPage: TPage;
begin
  if Closing then
    Exit;
  if navTabs.TabCount = 0 then
    Exit;

  PGW.PageIndex := navTabs.TabIndex;

  //if (TTDIPage(PGW.ActivePage).FormInPage <> nil) and
  //  (TTDIPage(PGW.ActivePage).FormInPage is TfrmIndividuals) then
  //begin
  //  aPage := PGW.ActivePage;
  //  {$IFDEF DEBUG}
  //  LogInfo('ACTIVE TAB:' + aPage.Caption);
  //  {$ENDIF}
  //  ActiveQuery := DMG.qIndividuals;
  //  if fIndividuals = nil then
  //    fIndividuals := TTDIPage(PGW.ActivePage).FormInPage as TfrmIndividuals;
  //  ActiveList := fIndividuals;
  //  ActiveGrid := nil;
  //end
  //else
  if (TTDIPage(PGW.ActivePageComponent).FormInPage <> nil) and
    (TTDIPage(PGW.ActivePageComponent).FormInPage is TfrmCustomGrid) then
  begin
    aPage := PGW.ActivePageComponent;
    ActiveGrid := TTDIPage(PGW.ActivePageComponent).FormInPage as TfrmCustomGrid;
    {$IFDEF DEBUG}
    LogInfo('ACTIVE TAB:' + aPage.Caption);
    {$ENDIF}
    case TTableType(aPage.Tag) of
      //tbNone: ;
      tbGazetteer:      ActiveQuery := DMG.qGazetteer;
      tbNetStations:    ActiveQuery := DMG.qNetStations;
      tbZooTaxa:        ActiveQuery := DMG.qTaxa;
      tbBotanicTaxa:    ActiveQuery := DMG.qBotany;
      tbInstitutions:   ActiveQuery := DMG.qInstitutions;
      tbPeople:         ActiveQuery := DMG.qPeople;
      tbProjects:       ActiveQuery := DMG.qProjects;
      tbPermits:        ActiveQuery := DMG.qPermits;
      tbMethods:        ActiveQuery := DMG.qMethods;
      tbExpeditions:    ActiveQuery := DMG.qExpeditions;
      tbSurveys:        ActiveQuery := DMG.qSurveys;
      tbSightings:      ActiveQuery := DMG.qSightings;
      tbSpecimens:      ActiveQuery := DMG.qSpecimens;
      tbBands:          ActiveQuery := DMG.qBands;
      tbIndividuals:    ActiveQuery := DMG.qIndividuals;
      tbCaptures:       ActiveQuery := DMG.qCaptures;
      tbMolts:          ActiveQuery := DMG.qMolts;
      tbNests:          ActiveQuery := DMG.qNests;
      tbNestRevisions:  ActiveQuery := DMG.qNestRevisions;
      tbEggs:           ActiveQuery := DMG.qEggs;
      //tbImages: ;
      //tbAudioLibrary: ;
      //tbTaxonRanks: ;
      //tbProjectTeams: ;
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      //tbSamplePreps: ;
      //tbPermanentNets: ;
    end;
    { #TODO : Set focus on active form }
  end else
  begin
    ActiveQuery := nil;
    ActiveGrid := nil;
  end;
  UpdateMenu(PGW.ActivePageComponent);

  //if (ActiveList <> nil) then
  //begin
  //  eSearch.Text := (TTDIPage(PGW.ActivePage).FormInPage as TfrmIndividuals).SearchString;
  //  TimerFind.Enabled := False;
  //  UpdateQueryButtons;
  //end else
  if (ActiveGrid <> nil) then
  begin
    eSearch.Text := ActiveGrid.SearchString;
    TimerFind.Enabled := False;
  end else
  begin
    eSearch.Text := EmptyStr;
    TimerFind.Enabled := False;
  end;
end;

procedure TfrmMain.navTabsTabClose(Sender: TObject; ATabIndex: integer; var ACanClose,
  ACanContinue: boolean);
begin
  if Closing then
    ACanClose := Closing
  else
    ACanClose := PGW.CanCloseAPage(ATabIndex);

  if ACanClose then
  begin
    case TTableType(PGW.Page[ATabIndex].Tag) of
      //tbNone: ;
      tbGazetteer:      fGazetteer := nil;
      tbNetStations:    fNetStations := nil;
      tbZooTaxa:        fZooTaxa := nil;
      tbBotanicTaxa:    fBotanicTaxa := nil;
      tbInstitutions:   fInstitutions := nil;
      tbPeople:         fPeople := nil;
      tbProjects:       fProjects := nil;
      tbPermits:        fPermits := nil;
      tbMethods:        fMethods := nil;
      tbExpeditions:    fExpeditions := nil;
      tbSurveys:        fSurveys := nil;
      tbSightings:      fSightings := nil;
      tbSpecimens:      fSpecimens := nil;
      tbBands:          fBands := nil;
      tbIndividuals:    fIndividuals := nil;
      tbCaptures:       fCaptures := nil;
      tbMolts:          fMolts := nil;
      tbNests:          fNests := nil;
      tbNestRevisions:  fNestRevisions := nil;
      tbEggs:           fEggs := nil;
      //tbImages: ;
      //tbAudioLibrary: ;
      //tbTaxonRanks: ;
      //tbProjectTeams: ;
      //tbSurveyTeams: ;
      //tbNetsEffort: ;
      //tbSamplePreps: ;
      //tbPermanentNets: ;
    end;
    PGW.DoCloseTabClicked(PGW.Page[ATabIndex]);
  end;

  if (PGW.PageCount > 0) and not (Closing) then
    UpdateMenu(PGW.ActivePageComponent);
end;

procedure TfrmMain.OpenForm(Sender: TObject; var aForm: TfrmCustomGrid; aTableType: TTableType;
  aCaption: String; aIcon: Integer = -1);
var
  pag: Integer;
begin
  if Opening then
    Exit;

  Opening := True;

  { Create data module }
  if not Assigned(DML) then
    DML := TDML.Create(Application);
  if not Assigned(DMG) then
    DMG := TDMG.Create(Application);

  { Check if form is already open and show it }
  pag := PGW.FindFormInPages(aForm);
  if pag >= 0 then
  begin
    navTabs.TabIndex := pag;
    PGW.PageIndex := pag;
    if not (ActiveQuery.State in [dsInsert, dsEdit]) then
      ActiveQuery.Refresh;
    Opening := False;
    Exit;
  end;

  { If form is not open, create it }
  pSplash.Top := PGW.Top + pMainMenu.Height + 1;
  pSplash.Height :=  PGW.Height;
  pSplash.Width :=  PGW.Width;
  pSplash.Visible := True;
  Screen.BeginTempCursor(crAppStart);
  sbarStatus.Caption := Format(rsLoadingForm, [LowerCase(aCaption)]);
  aForm := TfrmCustomGrid.Create(Application);
  aForm.Caption := aCaption;
  aForm.TableType := aTableType;
  {$IFDEF DEBUG}
  LogDebug('OPEN: ' + aForm.Caption);
  {$ENDIF}
  PGW.ShowFormInPage(aForm, aIcon);
  Application.ProcessMessages;
  PGW.ActivePageComponent.Tag := Ord(aTableType);
  navTabs.AddTab(PGW.PageIndex, PGW.ActivePageComponent.Caption);
  navTabs.TabIndex := navTabs.TabCount - 1;
  navTabs.GetTabData(navTabs.TabIndex).TabPopupMenu := pmTabs;
  //atTabsTabChanged(Sender);

  //mMenu.TabIndex := 3;
  UpdateMenu(PGW.ActivePageComponent);
  sbarStatus.Caption := EmptyStr;
  Screen.EndTempCursor(crAppStart);
  pSplash.Visible := False;
  Opening := False;
end;

procedure TfrmMain.OpenTab(Sender: TObject; aForm: TForm; aFormClass: TComponentClass; aCaption: String;
  Pinned: Boolean);
var
  pag: Integer;
begin
  if Opening then
    Exit;

  Opening := True;
  { Check if form is already open and show it }
  pag := PGW.FindFormInPages(aForm);
  if pag >= 0 then
  begin
    navTabs.TabIndex := pag;
    PGW.PageIndex := pag;
    if (ActiveQuery <> nil) then
      if not (ActiveQuery.State in [dsInsert, dsEdit]) then
        ActiveQuery.Refresh;
    Opening := False;
    Exit;
  end;

  { If form is not open, create it }
  Screen.BeginTempCursor(crAppStart);
  sbarStatus.Caption := Format(rsLoadingForm, [LowerCase(aCaption)]);
  Application.CreateForm(aFormClass, aForm);
  aForm.Caption := aCaption;
  {$IFDEF DEBUG}
  LogDebug('OPEN: ' + aForm.Caption);
  {$ENDIF}
  PGW.ShowFormInPage(aForm);
  navTabs.AddTab(PGW.PageIndex, PGW.ActivePageComponent.Caption);
  navTabs.TabIndex := navTabs.TabCount - 1;
  navTabs.GetTabData(navTabs.TabIndex).TabPopupMenu := pmTabs;

  if Pinned then
  begin
    navTabs.GetTabData(PGW.PageIndex).TabPinned := Pinned;
    navTabs.GetTabData(PGW.PageIndex).TabHideXButton := Pinned;
  end;
  UpdateMenu(PGW.ActivePageComponent);
  sbarStatus.Caption := EmptyStr;
  Screen.EndTempCursor(crAppStart);
  Opening := False;
end;

procedure TfrmMain.pmaNewBandClick(Sender: TObject);
begin
  EditBand(DMG.qBands, True);
end;

procedure TfrmMain.pmaNewCaptureClick(Sender: TObject);
begin
  EditCapture(DMG.qCaptures, 0, True);
end;

procedure TfrmMain.pmaNewEggClick(Sender: TObject);
begin
  EditEgg(DMG.qEggs, 0, True);
end;

procedure TfrmMain.pmaNewExpeditionClick(Sender: TObject);
begin
  EditExpedition(DMG.qExpeditions, True);
end;

procedure TfrmMain.pmaNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

procedure TfrmMain.pmaNewInstitutionClick(Sender: TObject);
begin
  EditInstitution(DMG.qInstitutions, True);
end;

procedure TfrmMain.pmaNewMoltClick(Sender: TObject);
begin
  EditMolt(DMG.qMolts, 0, True);
end;

procedure TfrmMain.pmaNewNestClick(Sender: TObject);
begin
  EditNest(DMG.qNests, True);
end;

procedure TfrmMain.pmaNewNetStationClick(Sender: TObject);
begin
  EditNetStation(DMG.qNetStations, True);
end;

procedure TfrmMain.pmaNewPermitClick(Sender: TObject);
begin
  EditPermit(DMG.qPermits, 0, True);
end;

procedure TfrmMain.pmaNewProjectClick(Sender: TObject);
begin
  EditProject(DMG.qProjects, True);
end;

procedure TfrmMain.pmaNewResearcherClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TfrmMain.pmaNewSightingClick(Sender: TObject);
begin
  EditSighting(DMG.qSightings, 0, True);
end;

procedure TfrmMain.pmaNewSpecimenClick(Sender: TObject);
begin
  EditSpecimen(DMG.qSpecimens, True);
end;

procedure TfrmMain.pmaNewSurveyClick(Sender: TObject);
begin
  EditSurvey(DMG.qSurveys, True);
end;

procedure TfrmMain.pmaNewToponymClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TfrmMain.pmtCloseAllOtherTabsClick(Sender: TObject);
begin
  CloseAllTabs(False, navTabs.TabIndex);
end;

procedure TfrmMain.pmtCloseAllTabsClick(Sender: TObject);
begin
  CloseAllTabs(False);
end;

procedure TfrmMain.pmtCloseTabClick(Sender: TObject);
begin
  navTabs.DeleteTab(navTabs.TabIndex, True, False);
end;

procedure TfrmMain.sbClearSearchClick(Sender: TObject);
begin
  {$IFDEF DEBUG}
  LogDebug('Search cleared');
  {$ENDIF}

  eSearch.Clear;
  if eSearch.CanSetFocus then
    eSearch.SetFocus;

  //ClearSearch;
end;

procedure TfrmMain.sbHomeClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to navTabs.TabCount - 1 do
    if navTabs.GetTabData(i).TabCaption = rsHome then
    begin
      navTabs.TabIndex := i;
      Break;
    end;
end;

procedure TfrmMain.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;

  if TTDIPage(PGW.ActivePageComponent).FormInPage is TfrmCustomGrid then
    ActiveGrid.SearchString := eSearch.Text
  else
  //if TTDIPage(PGW.ActivePage).FormInPage is TfrmIndividuals then
  //  (TTDIPage(PGW.ActivePage).FormInPage as TfrmIndividuals).SearchString := eSearch.Text
  //else
    Exit;
end;

procedure TfrmMain.TimerScreenTimer(Sender: TObject);
begin
  Self.AutoScale;
  if OldPPI = Self.PixelsPerInch then
    Exit;

  OldPPI := Self.PixelsPerInch;
  {$IFDEF DEBUG}
  LogDebug('DPI changed: ' + IntToStr(OldPPI) + '; Monitor: ' + IntToStr(Self.Monitor.MonitorNum));
  {$ENDIF}
  //Self.PixelsPerInch := OldPPI;
  //Self.AutoScale;
  if Self.PixelsPerInch <> 96 then
  begin
    navTabs.OptScalePercents := (OldPPI * 100) div 96;
    navTabs.OptFontScale := (OldPPI * 100) div 96;
  end
  else
  begin
    navTabs.OptScalePercents := 100;
    navTabs.OptFontScale := 100;
  end;
end;

procedure TfrmMain.UpdateMenu(aTab: TPage);
begin
  if (TTDIPage(aTab).FormInPage is TfrmCustomGrid) then
  begin
    pSearch.Visible := True;
    actOpenBandHistory.Visible := ActiveQuery = DMG.qBands;
  end else
  begin
    pSearch.Visible := False;
    actOpenBandHistory.Visible := False;
  end;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  lblSbarDatabase.Caption := ConexaoDB.Name;
  sbarDatabase.Hint := ConexaoDB.Database;
  lblSbarDatabase.Hint := sbarDatabase.Hint;
  icoSbarDatabase.Hint := sbarDatabase.Hint;
  lblSbarUser.Caption := ActiveUser.UserName;
  lblSbarTaxonomy.Caption := TaxonomyName[ActiveTaxonomy];
end;

end.

