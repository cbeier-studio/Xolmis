unit ucfg_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, ComCtrls,
  atshapelinebgra, BCPanel, rxswitch;

type

  { TcfgOptions }

  TcfgOptions = class(TForm)
    cbCheckUpdates: TComboBox;
    cbClearDeleted: TComboBox;
    cbMainTaxonomy: TComboBox;
    cbVernacularNames: TComboBox;
    eAttachmentsPath: TDirectoryEdit;
    eAudiosPath: TDirectoryEdit;
    eBackupPath: TDirectoryEdit;
    eImagesPath: TDirectoryEdit;
    Image1: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    Image17: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    iPanels: TImageList;
    lblAllowUsageData: TLabel;
    lblAllowUsageData1: TLabel;
    lblAllowWriteLog: TLabel;
    lblAttachmentsPath: TLabel;
    lblAudiosPath: TLabel;
    lblBackupPath: TLabel;
    lblCheckUpdates: TLabel;
    lblClearDeleted: TLabel;
    lblConfirmCancel: TLabel;
    lblEnterAsTab: TLabel;
    lblImagesPath: TLabel;
    lblMainTaxonomy: TLabel;
    lblManageBackups: TLabel;
    lblRememberConnection: TLabel;
    lblRememberUser: TLabel;
    lblShowSynonyms: TLabel;
    lblStartupBackup: TLabel;
    lblTitleBackup: TLabel;
    lblTitleCollection: TLabel;
    lblTitleInterface: TLabel;
    lblTitleMedia: TLabel;
    lblTitleSecurity: TLabel;
    lblVernacularNames: TLabel;
    nbPages: TNotebook;
    pAllowUsageData: TBCPanel;
    pAllowUsageData1: TBCPanel;
    pAllowWriteLog: TBCPanel;
    pBackupPath: TBCPanel;
    pCheckUpdates: TBCPanel;
    pgSecurity: TPage;
    pgBackup: TPage;
    pContentSecurity: TPanel;
    pContentBackup: TPanel;
    pAttachmentsPath: TBCPanel;
    pAudiosPath: TBCPanel;
    pContentGeneral: TPanel;
    pContentCollection: TPanel;
    pContentMedia: TPanel;
    pClearDeleted: TBCPanel;
    pConfirmCancel: TBCPanel;
    pEnterAsTab: TBCPanel;
    pgGeneral: TPage;
    pgCollection: TPage;
    pgMedia: TPage;
    pImagesPath: TBCPanel;
    pMainTaxonomy: TBCPanel;
    pManageBackups: TBCPanel;
    pRememberConnection: TBCPanel;
    pRememberUser: TBCPanel;
    pShowSynonyms: TBCPanel;
    OpenDlg: TOpenDialog;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pStartupBackup: TBCPanel;
    pVernacularNames: TBCPanel;
    sbCheckUpdatesNow: TBitBtn;
    sbClose: TButton;
    sbNewBackup: TBitBtn;
    sbRestoreBackup: TBitBtn;
    scrollGeneral: TScrollBox;
    scrollCollection: TScrollBox;
    scrollMedia: TScrollBox;
    scrollSecurity: TScrollBox;
    scrollBackup: TScrollBox;
    tvMenu: TTreeView;
    tsAllowUsageData: TRxSwitch;
    tsConfirmCancel: TRxSwitch;
    tsEnterAsTab: TRxSwitch;
    tsRememberConnection: TRxSwitch;
    tsRememberUser: TRxSwitch;
    tsShowSynonyms: TRxSwitch;
    tsStartupBackup: TRxSwitch;
    tsWriteLogs: TRxSwitch;
    procedure cbCheckUpdatesChange(Sender: TObject);
    procedure cbClearDeletedChange(Sender: TObject);
    procedure cbMainTaxonomyChange(Sender: TObject);
    procedure cbVernacularNamesChange(Sender: TObject);
    procedure eAttachmentsPathChange(Sender: TObject);
    procedure eAudiosPathChange(Sender: TObject);
    procedure eBackupPathChange(Sender: TObject);
    procedure eImagesPathChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbCheckUpdatesNowClick(Sender: TObject);
    procedure sbNewBackupClick(Sender: TObject);
    procedure sbRestoreBackupClick(Sender: TObject);
    procedure tsAllowUsageDataOn(Sender: TObject);
    procedure tsWriteLogsOn(Sender: TObject);
    procedure tsConfirmCancelOn(Sender: TObject);
    procedure tsEnterAsTabOn(Sender: TObject);
    procedure tsRememberConnectionOn(Sender: TObject);
    procedure tsRememberUserOn(Sender: TObject);
    procedure tsShowSynonymsOn(Sender: TObject);
    procedure tsStartupBackupOn(Sender: TObject);
    procedure tvMenuSelectionChanged(Sender: TObject);
  private
    procedure LoadConfig;
  public

  end;

var
  cfgOptions: TcfgOptions;

implementation

uses cbs_locale, cbs_global, cbs_dialogs, cbs_backup, cbs_autoupdate, cbs_system;

{$R *.lfm}

{ TcfgOptions }

procedure TcfgOptions.tsWriteLogsOn(Sender: TObject);
begin
  XSettings.AllowWriteLogs := tsWriteLogs.StateOn = sw_on;
end;

procedure TcfgOptions.tvMenuSelectionChanged(Sender: TObject);
begin
  nbPages.PageIndex := tvMenu.Selected.Index;
end;

procedure TcfgOptions.tsAllowUsageDataOn(Sender: TObject);
begin
  XSettings.AllowSendUsageData := tsAllowUsageData.StateOn = sw_on;
end;

procedure TcfgOptions.sbRestoreBackupClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');
  if not MsgDlg(rsTitleRestore, rsRestoreBackupPrompt, mtConfirmation) then
    Exit;

  OpenDlg.InitialDir:= XSettings.BackupFolder;
  if OpenDlg.Execute then
    RestoreBackup(OpenDlg.FileName);
end;

procedure TcfgOptions.sbNewBackupClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');

  NewBackup;
end;

procedure TcfgOptions.eAttachmentsPathChange(Sender: TObject);
begin
  XSettings.DocumentsFolder := eAttachmentsPath.Text;
end;

procedure TcfgOptions.cbMainTaxonomyChange(Sender: TObject);
begin
  XSettings.Taxonomy := cbMainTaxonomy.ItemIndex;
end;

procedure TcfgOptions.cbClearDeletedChange(Sender: TObject);
begin
  XSettings.ClearDeletedPeriod := cbClearDeleted.ItemIndex;
end;

procedure TcfgOptions.cbCheckUpdatesChange(Sender: TObject);
begin
  XSettings.AutoUpdates := cbCheckUpdates.ItemIndex;
end;

procedure TcfgOptions.cbVernacularNamesChange(Sender: TObject);
begin
  XSettings.VernacularNamesLanguage := cbVernacularNames.ItemIndex;
end;

procedure TcfgOptions.eAudiosPathChange(Sender: TObject);
begin
  XSettings.AudiosFolder := eAudiosPath.Text;
end;

procedure TcfgOptions.eBackupPathChange(Sender: TObject);
begin
  XSettings.BackupFolder := eBackupPath.Text;
end;

procedure TcfgOptions.eImagesPathChange(Sender: TObject);
begin
  XSettings.ImagesFolder := eImagesPath.Text;
end;

procedure TcfgOptions.FormShow(Sender: TObject);
begin
  //SBox.VertScrollBar.Position := 0;
  tvMenu.Selected := tvMenu.Items.GetFirstNode;
  pManageBackups.Enabled := not (ActiveUser.IsVisitor);

  LoadConfig;
end;

procedure TcfgOptions.sbCheckUpdatesNowClick(Sender: TObject);
begin
  LogInfo('Check Xolmis updates');
  if CheckUpdates then
    if MsgDlg(rsTitleAutoUpdate, rsNewUpdateAvailable, mtConfirmation) then
      RunUpdate
    else
      MsgDlg(rsTitleAutoUpdate, rsIsUpToDate, mtInformation);
end;

procedure TcfgOptions.tsConfirmCancelOn(Sender: TObject);
begin
  XSettings.ConfirmCancel := tsConfirmCancel.StateOn = sw_on;
end;

procedure TcfgOptions.tsEnterAsTabOn(Sender: TObject);
begin
  XSettings.UseEnterAsTab := tsEnterAsTab.StateOn = sw_on;
end;

procedure TcfgOptions.tsRememberConnectionOn(Sender: TObject);
begin
  XSettings.RememberConnection := tsRememberConnection.StateOn = sw_on;
  if tsRememberConnection.StateOn = sw_on then
    XSettings.LastConnection := ConexaoDB.Name
  else
    XSettings.Delete('SECURITY', 'LastConnection');
end;

procedure TcfgOptions.tsRememberUserOn(Sender: TObject);
begin
  XSettings.RememberUser := tsRememberUser.StateOn = sw_on;
  if tsRememberUser.StateOn = sw_on then
    XSettings.LastUser := ActiveUser.UserName
  else
    XSettings.Delete('SECURITY', 'LastUser');
end;

procedure TcfgOptions.tsShowSynonymsOn(Sender: TObject);
begin
  XSettings.ShowSynonyms := tsShowSynonyms.StateOn = sw_on;
end;

procedure TcfgOptions.tsStartupBackupOn(Sender: TObject);
begin
  XSettings.StartupBackup := tsStartupBackup.StateOn = sw_on;
end;

procedure TcfgOptions.LoadConfig;
begin
  { GENERAL PARAMETERS AND INTERFACE }
  if XSettings.ConfirmCancel then
    tsConfirmCancel.StateOn := sw_on
  else
    tsConfirmCancel.StateOn := sw_off;
  cbClearDeleted.ItemIndex := XSettings.ClearDeletedPeriod;
  if XSettings.UseEnterAsTab then
    tsEnterAsTab.StateOn := sw_on
  else
    tsEnterAsTab.StateOn := sw_off;

  { COLLECTION }
  cbVernacularNames.ItemIndex := XSettings.VernacularNamesLanguage;
  cbMainTaxonomy.ItemIndex := XSettings.Taxonomy;
  if XSettings.ShowSynonyms then
    tsShowSynonyms.StateOn := sw_on
  else
    tsShowSynonyms.StateOn := sw_off;

  { MEDIA }
  eImagesPath.Text := XSettings.ImagesFolder;
  eAudiosPath.Text := XSettings.AudiosFolder;
  eAttachmentsPath.Text := XSettings.DocumentsFolder;

  { SECURITY }
  if XSettings.RememberUser then
    tsRememberUser.StateOn := sw_on
  else
    tsRememberUser.StateOn := sw_off;
  if XSettings.RememberConnection then
    tsRememberConnection.StateOn := sw_on
  else
    tsRememberConnection.StateOn := sw_off;
  cbCheckUpdates.ItemIndex := XSettings.AutoUpdates;

  { PRIVACY }
  if XSettings.AllowWriteLogs then
    tsWriteLogs.StateOn := sw_on
  else
    tsWriteLogs.StateOn := sw_off;
  if XSettings.AllowSendUsageData then
    tsAllowUsageData.StateOn := sw_on
  else
    tsAllowUsageData.StateOn := sw_off;

  { BACKUP AND RESTORE }
  eBackupPath.Text := XSettings.BackupFolder;
  if XSettings.StartupBackup then
    tsStartupBackup.StateOn := sw_on
  else
    tsStartupBackup.StateOn := sw_off;

end;

end.

