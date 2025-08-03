{ Xolmis Settings dialog

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

unit ucfg_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, ComCtrls, lclintf,
  ToggleSwitch, atshapelinebgra, BCPanel;

type

  { TcfgOptions }

  TcfgOptions = class(TForm)
    cbCheckUpdates: TComboBox;
    cbClearDeleted: TComboBox;
    cbStartPage: TComboBox;
    cbMainTaxonomy: TComboBox;
    cbSelectedTheme: TComboBox;
    cbVernacularNames: TComboBox;
    cbStartupBackup: TComboBox;
    eAttachmentsPath: TDirectoryEdit;
    eAudiosPath: TDirectoryEdit;
    eBackupPath: TDirectoryEdit;
    eImagesPath: TDirectoryEdit;
    icoSelectedTheme: TImage;
    iIconsDark: TImageList;
    icoConfirmCancel: TImage;
    icoCheckUpdates: TImage;
    icoRememberConnection: TImage;
    icoRememberUser: TImage;
    icoAllowWriteLog: TImage;
    icoAllowUsageData: TImage;
    icoBackupPath: TImage;
    icoStartupBackup: TImage;
    icoOpenAfterExport: TImage;
    icoStartPage: TImage;
    imgPrivacyTerms: TImage;
    icoUseConditionalFormatting: TImage;
    icoShowOutliers: TImage;
    icoClearDeleted: TImage;
    icoMainTaxonomy: TImage;
    icoEnterAsTab: TImage;
    icoVernacularNames: TImage;
    icoShowSynonyms: TImage;
    icoImagesPath: TImage;
    icoAudiosPath: TImage;
    icoDocumentsPath: TImage;
    iIcons: TImageList;
    lblAllowUsageData: TLabel;
    lblStartPage: TLabel;
    lblOpenAfterExport: TLabel;
    lblPrivacyTerms: TLabel;
    lblAllowWriteLog: TLabel;
    lblAttachmentsPath: TLabel;
    lblAudiosPath: TLabel;
    lblBackupPath: TLabel;
    lblCheckUpdates: TLabel;
    lblClearDeleted: TLabel;
    lblShowOutliers: TLabel;
    lblUseConditionalFormatting: TLabel;
    lblConfirmCancel: TLabel;
    lblEnterAsTab: TLabel;
    lblImagesPath: TLabel;
    lblMainTaxonomy: TLabel;
    lblManageBackups: TLabel;
    lblRememberConnection: TLabel;
    lblRememberUser: TLabel;
    lblSelectedTheme: TLabel;
    lblSelectedThemeRestart: TLabel;
    lblShowSynonyms: TLabel;
    lblStartupBackup: TLabel;
    lblTitleBackup: TLabel;
    lblTitleCollection: TLabel;
    lblTitleInterface: TLabel;
    lblTitleAppearance: TLabel;
    lblTitleMedia: TLabel;
    lblTitleSecurity: TLabel;
    lblVernacularNames: TLabel;
    nbPages: TNotebook;
    pStartPage: TBCPanel;
    pOpenAfterExport: TBCPanel;
    pShowOutliers: TBCPanel;
    pContentAppearance: TPanel;
    pUseConditionalFormatting: TBCPanel;
    pgAppearance: TPage;
    pAllowUsageData: TBCPanel;
    pPrivacyTerms: TBCPanel;
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
    pSelectedTheme: TBCPanel;
    pShowSynonyms: TBCPanel;
    OpenDlg: TOpenDialog;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pStartupBackup: TBCPanel;
    pVernacularNames: TBCPanel;
    sbCheckUpdatesNow: TBitBtn;
    sbClearLogFiles: TBitBtn;
    sbClearUsageData: TBitBtn;
    sbClose: TButton;
    sbNewBackup: TBitBtn;
    sbRestoreBackup: TBitBtn;
    scrollGeneral: TScrollBox;
    scrollCollection: TScrollBox;
    scrollAppearance: TScrollBox;
    scrollMedia: TScrollBox;
    scrollSecurity: TScrollBox;
    scrollBackup: TScrollBox;
    tsRememberConnection: TToggleSwitch;
    tsRememberUser: TToggleSwitch;
    tsWriteLogs: TToggleSwitch;
    tsAllowUsageData: TToggleSwitch;
    tsOpenAfterExport: TToggleSwitch;
    tsShowSynonyms: TToggleSwitch;
    tsShowOutliers: TToggleSwitch;
    tsUseConditionalFormatting: TToggleSwitch;
    tsEnterAsTab: TToggleSwitch;
    tsConfirmCancel: TToggleSwitch;
    tvMenu: TTreeView;
    procedure cbCheckUpdatesChange(Sender: TObject);
    procedure cbClearDeletedChange(Sender: TObject);
    procedure cbMainTaxonomyChange(Sender: TObject);
    procedure cbSelectedThemeChange(Sender: TObject);
    procedure cbStartPageChange(Sender: TObject);
    procedure cbStartupBackupSelect(Sender: TObject);
    procedure cbVernacularNamesChange(Sender: TObject);
    procedure eAttachmentsPathChange(Sender: TObject);
    procedure eAudiosPathChange(Sender: TObject);
    procedure eBackupPathChange(Sender: TObject);
    procedure eImagesPathChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblPrivacyTermsClick(Sender: TObject);
    procedure sbCheckUpdatesNowClick(Sender: TObject);
    procedure sbClearLogFilesClick(Sender: TObject);
    procedure sbNewBackupClick(Sender: TObject);
    procedure sbRestoreBackupClick(Sender: TObject);
    procedure tsAllowUsageDataChange(Sender: TObject);
    procedure tsConfirmCancelChange(Sender: TObject);
    procedure tsEnterAsTabChange(Sender: TObject);
    procedure tsOpenAfterExportChange(Sender: TObject);
    procedure tsRememberConnectionChange(Sender: TObject);
    procedure tsRememberUserChange(Sender: TObject);
    procedure tsShowOutliersChange(Sender: TObject);
    procedure tsShowSynonymsChange(Sender: TObject);
    procedure tsUseConditionalFormattingChange(Sender: TObject);
    procedure tsWriteLogsChange(Sender: TObject);
    procedure tvMenuSelectionChanged(Sender: TObject);
  private
    procedure ApplyDarkMode;
    procedure LoadConfig;
  public

  end;

var
  cfgOptions: TcfgOptions;

implementation

uses
  utils_locale, utils_global, models_users, utils_dialogs, utils_backup, utils_autoupdate, utils_system,
  utils_themes, udm_main,
  uDarkStyleParams;

{$R *.lfm}

{ TcfgOptions }

procedure TcfgOptions.tvMenuSelectionChanged(Sender: TObject);
begin
  nbPages.PageIndex := tvMenu.Selected.Index;
end;

procedure TcfgOptions.tsConfirmCancelChange(Sender: TObject);
begin
  xSettings.ConfirmCancel := tsConfirmCancel.Checked;
end;

procedure TcfgOptions.sbRestoreBackupClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');
  if not MsgDlg(rsTitleRestore, rsRestoreBackupPrompt, mtConfirmation) then
    Exit;

  OpenDlg.InitialDir:= xSettings.BackupFolder;
  if OpenDlg.Execute then
    RestoreBackup(OpenDlg.FileName);
end;

procedure TcfgOptions.tsAllowUsageDataChange(Sender: TObject);
begin
  xSettings.AllowSendUsageData := tsAllowUsageData.Checked;
end;

procedure TcfgOptions.sbNewBackupClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');

  VacuumIntoBackup; //NewBackup;
end;

procedure TcfgOptions.eAttachmentsPathChange(Sender: TObject);
begin
  xSettings.DocumentsFolder := eAttachmentsPath.Text;
end;

procedure TcfgOptions.cbMainTaxonomyChange(Sender: TObject);
begin
  xSettings.Taxonomy := cbMainTaxonomy.ItemIndex;
end;

procedure TcfgOptions.cbSelectedThemeChange(Sender: TObject);
begin
  xSettings.SelectedTheme := cbSelectedTheme.ItemIndex;
  lblSelectedThemeRestart.Visible := True;
end;

procedure TcfgOptions.cbStartPageChange(Sender: TObject);
begin
  xSettings.StartPage := cbStartPage.ItemIndex;
end;

procedure TcfgOptions.cbStartupBackupSelect(Sender: TObject);
begin
  xSettings.AutomaticBackup := cbStartupBackup.ItemIndex;
end;

procedure TcfgOptions.cbClearDeletedChange(Sender: TObject);
begin
  xSettings.ClearDeletedPeriod := cbClearDeleted.ItemIndex;
end;

procedure TcfgOptions.ApplyDarkMode;
begin
  tvMenu.Images := iIconsDark;
  icoStartPage.Images := iIconsDark;
  icoEnterAsTab.Images := iIconsDark;
  icoConfirmCancel.Images := iIconsDark;
  icoClearDeleted.Images := iIconsDark;
  icoCheckUpdates.Images := iIconsDark;
  icoSelectedTheme.Images := iIconsDark;
  icoUseConditionalFormatting.Images := iIconsDark;
  icoShowOutliers.Images := iIconsDark;
  icoVernacularNames.Images := iIconsDark;
  icoMainTaxonomy.Images := iIconsDark;
  icoShowSynonyms.Images := iIconsDark;
  icoImagesPath.Images := iIconsDark;
  icoAudiosPath.Images := iIconsDark;
  icoDocumentsPath.Images := iIconsDark;
  icoRememberConnection.Images := iIconsDark;
  icoRememberUser.Images := iIconsDark;
  icoAllowWriteLog.Images := iIconsDark;
  icoAllowUsageData.Images := iIconsDark;
  imgPrivacyTerms.Images := iIconsDark;
  icoBackupPath.Images := iIconsDark;
  icoStartupBackup.Images := iIconsDark;
  icoOpenAfterExport.Images := iIconsDark;

  pStartPage.Background.Color := clSolidBGSecondaryDark;
  pStartPage.Border.Color := clSystemSolidNeutralFGDark;
  pEnterAsTab.Background.Color := clSolidBGSecondaryDark;
  pEnterAsTab.Border.Color := clSystemSolidNeutralFGDark;
  pConfirmCancel.Background.Color := clSolidBGSecondaryDark;
  pConfirmCancel.Border.Color := clSystemSolidNeutralFGDark;
  pClearDeleted.Background.Color := clSolidBGSecondaryDark;
  pClearDeleted.Border.Color := clSystemSolidNeutralFGDark;
  pCheckUpdates.Background.Color := clSolidBGSecondaryDark;
  pCheckUpdates.Border.Color := clSystemSolidNeutralFGDark;
  pSelectedTheme.Background.Color := clSolidBGSecondaryDark;
  pSelectedTheme.Border.Color := clSystemSolidNeutralFGDark;
  pUseConditionalFormatting.Background.Color := clSolidBGSecondaryDark;
  pUseConditionalFormatting.Border.Color := clSystemSolidNeutralFGDark;
  pShowOutliers.Background.Color := clSolidBGSecondaryDark;
  pShowOutliers.Border.Color := clSystemSolidNeutralFGDark;
  pVernacularNames.Background.Color := clSolidBGSecondaryDark;
  pVernacularNames.Border.Color := clSystemSolidNeutralFGDark;
  pMainTaxonomy.Background.Color := clSolidBGSecondaryDark;
  pMainTaxonomy.Border.Color := clSystemSolidNeutralFGDark;
  pShowSynonyms.Background.Color := clSolidBGSecondaryDark;
  pShowSynonyms.Border.Color := clSystemSolidNeutralFGDark;
  pImagesPath.Background.Color := clSolidBGSecondaryDark;
  pImagesPath.Border.Color := clSystemSolidNeutralFGDark;
  pAudiosPath.Background.Color := clSolidBGSecondaryDark;
  pAudiosPath.Border.Color := clSystemSolidNeutralFGDark;
  pAttachmentsPath.Background.Color := clSolidBGSecondaryDark;
  pAttachmentsPath.Border.Color := clSystemSolidNeutralFGDark;
  pOpenAfterExport.Background.Color := clSolidBGSecondaryDark;
  pOpenAfterExport.Border.Color := clSystemSolidNeutralFGDark;
  pRememberConnection.Background.Color := clSolidBGSecondaryDark;
  pRememberConnection.Border.Color := clSystemSolidNeutralFGDark;
  pRememberUser.Background.Color := clSolidBGSecondaryDark;
  pRememberUser.Border.Color := clSystemSolidNeutralFGDark;
  pAllowWriteLog.Background.Color := clSolidBGSecondaryDark;
  pAllowWriteLog.Border.Color := clSystemSolidNeutralFGDark;
  pAllowUsageData.Background.Color := clSolidBGSecondaryDark;
  pAllowUsageData.Border.Color := clSystemSolidNeutralFGDark;
  pPrivacyTerms.Background.Color := clSolidBGSecondaryDark;
  pPrivacyTerms.Border.Color := clSystemSolidNeutralFGDark;
  pBackupPath.Background.Color := clSolidBGSecondaryDark;
  pBackupPath.Border.Color := clSystemSolidNeutralFGDark;
  pStartupBackup.Background.Color := clSolidBGSecondaryDark;
  pStartupBackup.Border.Color := clSystemSolidNeutralFGDark;
  pManageBackups.Background.Color := clSolidBGSecondaryDark;
  pManageBackups.Border.Color := clSystemSolidNeutralFGDark;

  tsEnterAsTab.Color := pEnterAsTab.Background.Color;
  tsConfirmCancel.Color := pConfirmCancel.Background.Color;
  tsUseConditionalFormatting.Color := pConfirmCancel.Background.Color;
  tsShowOutliers.Color := pConfirmCancel.Background.Color;
  tsShowSynonyms.Color := pConfirmCancel.Background.Color;
  tsOpenAfterExport.Color := pConfirmCancel.Background.Color;
  tsRememberConnection.Color := pConfirmCancel.Background.Color;
  tsRememberUser.Color := pConfirmCancel.Background.Color;
  tsWriteLogs.Color := pConfirmCancel.Background.Color;
  tsAllowUsageData.Color := pConfirmCancel.Background.Color;

  eImagesPath.Images := DMM.iEditsDark;
  eAudiosPath.Images := DMM.iEditsDark;
  eAttachmentsPath.Images := DMM.iEditsDark;
  eBackupPath.Images := DMM.iEditsDark;
end;

procedure TcfgOptions.cbCheckUpdatesChange(Sender: TObject);
begin
  xSettings.AutoUpdates := cbCheckUpdates.ItemIndex;
end;

procedure TcfgOptions.cbVernacularNamesChange(Sender: TObject);
begin
  xSettings.VernacularNamesLanguage := cbVernacularNames.ItemIndex;
end;

procedure TcfgOptions.eAudiosPathChange(Sender: TObject);
begin
  xSettings.AudiosFolder := eAudiosPath.Text;
end;

procedure TcfgOptions.eBackupPathChange(Sender: TObject);
begin
  xSettings.BackupFolder := eBackupPath.Text;
end;

procedure TcfgOptions.eImagesPathChange(Sender: TObject);
begin
  xSettings.ImagesFolder := eImagesPath.Text;
end;

procedure TcfgOptions.FormDestroy(Sender: TObject);
begin
  xSettings.SaveToFile;
end;

procedure TcfgOptions.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  //SBox.VertScrollBar.Position := 0;
  tvMenu.Selected := tvMenu.Items.GetFirstNode;
  tvMenu.Items[0].Text := rsSettingsGeneral;
  tvMenu.Items[1].Text := rsSettingsAppearance;
  tvMenu.Items[2].Text := rsSettingsCollection;
  tvMenu.Items[3].Text := rsSettingsMedia;
  tvMenu.Items[4].Text := rsSettingsSecurityAndPrivacy;
  tvMenu.Items[5].Text := rsSettingsBackup;

  cbStartPage.Items.Clear;
  cbStartPage.Items.Add(rsCaptionExpeditions);
  cbStartPage.Items.Add(rsTitleSurveys);
  cbStartPage.Items.Add(rsTitleSightings);
  cbStartPage.Items.Add(rsTitleSpecimens);
  cbStartPage.Items.Add(rsTitleBands);
  cbStartPage.Items.Add(rsTitleIndividuals);
  cbStartPage.Items.Add(rsTitleCaptures);
  cbStartPage.Items.Add(rsTitleNests);
  cbStartPage.Items.Add(rsTitleResearchers);
  cbStartPage.Items.Add(rsTitleProjects);
  cbStartPage.Items.Add(rsTitlePermits);
  cbStartPage.Items.Add(rsTitleGazetteer);
  cbStartPage.Items.Add(rsTitleCoordinateConverter);

  cbClearDeleted.Items[0] := rsNever;

  cbCheckUpdates.Items.Clear;
  cbCheckUpdates.Items.Add(rsNever);
  cbCheckUpdates.Items.Add(rsDaily);
  cbCheckUpdates.Items.Add(rsWeekly);
  cbCheckUpdates.Items.Add(rsMonthly);

  cbSelectedTheme.Items.Clear;
  cbSelectedTheme.Items.Add(rsDefault);
  cbSelectedTheme.Items.Add(rsAuto);
  cbSelectedTheme.Items.Add(rsDark);
  cbSelectedTheme.Items.Add(rsLight);

  cbVernacularNames.Items.Clear;
  cbVernacularNames.Items.Add(rsEnglish);
  cbVernacularNames.Items.Add(rsPortuguese);
  cbVernacularNames.Items.Add(rsSpanish);

  cbStartupBackup.Items.Assign(cbCheckUpdates.Items);

  pManageBackups.Enabled := not (ActiveUser.IsVisitor);

  LoadConfig;
end;

procedure TcfgOptions.lblPrivacyTermsClick(Sender: TObject);
begin
  OpenUrl('https://github.com/cbeier-studio/Xolmis/blob/main/PRIVACY.md');
end;

procedure TcfgOptions.sbCheckUpdatesNowClick(Sender: TObject);
begin
  LogInfo('Check Xolmis updates');
  case CheckUpdates of
    ckrNone: ;
    ckrUpdated: MsgDlg(rsCheckUpdates, rsIsUpToDate, mtInformation);
    ckrNewVersion:
    begin
      if MsgDlg(rsCheckUpdates, Format(rsNewUpdateAvailable, [APP_NAME]), mtConfirmation) then
        RunUpdate;
    end;
    ckrError: ;
  end;
end;

procedure TcfgOptions.sbClearLogFilesClick(Sender: TObject);
var
  FLog: String;
begin
  FLog := ConcatPaths([AppDataDir, LOG_FILE]);
  if FileExists(FLog) then
    DeleteFile(FLog);
end;

procedure TcfgOptions.tsEnterAsTabChange(Sender: TObject);
begin
  xSettings.UseEnterAsTab := tsEnterAsTab.Checked;
end;

procedure TcfgOptions.tsOpenAfterExportChange(Sender: TObject);
begin
  xSettings.OpenFileAfterExport := tsOpenAfterExport.Checked;
end;

procedure TcfgOptions.tsRememberConnectionChange(Sender: TObject);
begin
  xSettings.RememberConnection := tsRememberConnection.Checked;
  if tsRememberConnection.Checked then
    xSettings.LastConnection := databaseConnection.Name
  else
    xSettings.Delete('SECURITY', 'LastConnection');
end;

procedure TcfgOptions.tsRememberUserChange(Sender: TObject);
begin
  xSettings.RememberUser := tsRememberUser.Checked;
  if tsRememberUser.Checked then
    xSettings.LastUser := ActiveUser.UserName
  else
    xSettings.Delete('SECURITY', 'LastUser');
end;

procedure TcfgOptions.tsShowOutliersChange(Sender: TObject);
begin
  xSettings.ShowOutliersOnGrid := tsShowOutliers.Checked;
end;

procedure TcfgOptions.tsShowSynonymsChange(Sender: TObject);
begin
  xSettings.ShowSynonyms := tsShowSynonyms.Checked;
end;

procedure TcfgOptions.tsUseConditionalFormattingChange(Sender: TObject);
begin
  xSettings.UseConditionalFormatting := tsUseConditionalFormatting.Checked;

  lblShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
  tsShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
end;

procedure TcfgOptions.tsWriteLogsChange(Sender: TObject);
begin
  xSettings.AllowWriteLogs := tsWriteLogs.Checked;
end;

procedure TcfgOptions.LoadConfig;
begin
  { GENERAL PARAMETERS AND INTERFACE }
  cbStartPage.ItemIndex := xSettings.StartPage;
  tsConfirmCancel.Checked := xSettings.ConfirmCancel;
  cbClearDeleted.ItemIndex := xSettings.ClearDeletedPeriod;
  tsEnterAsTab.Checked := xSettings.UseEnterAsTab;

  { APPEARANCE }
  cbSelectedTheme.ItemIndex := xSettings.SelectedTheme;
  tsUseConditionalFormatting.Checked := xSettings.UseConditionalFormatting;
  tsShowOutliers.Checked := xSettings.ShowOutliersOnGrid;

  { COLLECTION }
  cbVernacularNames.ItemIndex := xSettings.VernacularNamesLanguage;
  cbMainTaxonomy.ItemIndex := xSettings.Taxonomy;
  tsShowSynonyms.Checked := xSettings.ShowSynonyms;

  { MEDIA }
  eImagesPath.Text := xSettings.ImagesFolder;
  eAudiosPath.Text := xSettings.AudiosFolder;
  eAttachmentsPath.Text := xSettings.DocumentsFolder;

  { SECURITY }
  tsRememberUser.Checked := xSettings.RememberUser;
  tsRememberConnection.Checked := xSettings.RememberConnection;
  cbCheckUpdates.ItemIndex := xSettings.AutoUpdates;

  { PRIVACY }
  tsWriteLogs.Checked := xSettings.AllowWriteLogs;
  tsAllowUsageData.Checked := xSettings.AllowSendUsageData;

  { BACKUP AND RESTORE }
  eBackupPath.Text := xSettings.BackupFolder;
  cbStartupBackup.ItemIndex := xSettings.AutomaticBackup;

end;

end.

