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
  cbs_locale, cbs_global, cbs_dialogs, cbs_backup, cbs_autoupdate, cbs_system, cbs_themes, udm_main,
  uDarkStyleParams;

{$R *.lfm}

{ TcfgOptions }

procedure TcfgOptions.tvMenuSelectionChanged(Sender: TObject);
begin
  nbPages.PageIndex := tvMenu.Selected.Index;
end;

procedure TcfgOptions.tsConfirmCancelChange(Sender: TObject);
begin
  XSettings.ConfirmCancel := tsConfirmCancel.Checked;
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

procedure TcfgOptions.tsAllowUsageDataChange(Sender: TObject);
begin
  XSettings.AllowSendUsageData := tsAllowUsageData.Checked;
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

procedure TcfgOptions.cbSelectedThemeChange(Sender: TObject);
begin
  XSettings.SelectedTheme := cbSelectedTheme.ItemIndex;
  lblSelectedThemeRestart.Visible := True;
end;

procedure TcfgOptions.cbStartPageChange(Sender: TObject);
begin
  XSettings.StartPage := cbStartPage.ItemIndex;
end;

procedure TcfgOptions.cbStartupBackupSelect(Sender: TObject);
begin
  XSettings.AutomaticBackup := cbStartupBackup.ItemIndex;
end;

procedure TcfgOptions.cbClearDeletedChange(Sender: TObject);
begin
  XSettings.ClearDeletedPeriod := cbClearDeleted.ItemIndex;
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

  pStartPage.Background.Color := clCardBGDefaultDark;
  pStartPage.Border.Color := clSystemSolidNeutralFGDark;
  pEnterAsTab.Background.Color := clCardBGDefaultDark;
  pEnterAsTab.Border.Color := clSystemSolidNeutralFGDark;
  pConfirmCancel.Background.Color := clCardBGDefaultDark;
  pConfirmCancel.Border.Color := clSystemSolidNeutralFGDark;
  pClearDeleted.Background.Color := clCardBGDefaultDark;
  pClearDeleted.Border.Color := clSystemSolidNeutralFGDark;
  pCheckUpdates.Background.Color := clCardBGDefaultDark;
  pCheckUpdates.Border.Color := clSystemSolidNeutralFGDark;
  pSelectedTheme.Background.Color := clCardBGDefaultDark;
  pSelectedTheme.Border.Color := clSystemSolidNeutralFGDark;
  pUseConditionalFormatting.Background.Color := clCardBGDefaultDark;
  pUseConditionalFormatting.Border.Color := clSystemSolidNeutralFGDark;
  pShowOutliers.Background.Color := clCardBGDefaultDark;
  pShowOutliers.Border.Color := clSystemSolidNeutralFGDark;
  pVernacularNames.Background.Color := clCardBGDefaultDark;
  pVernacularNames.Border.Color := clSystemSolidNeutralFGDark;
  pMainTaxonomy.Background.Color := clCardBGDefaultDark;
  pMainTaxonomy.Border.Color := clSystemSolidNeutralFGDark;
  pShowSynonyms.Background.Color := clCardBGDefaultDark;
  pShowSynonyms.Border.Color := clSystemSolidNeutralFGDark;
  pImagesPath.Background.Color := clCardBGDefaultDark;
  pImagesPath.Border.Color := clSystemSolidNeutralFGDark;
  pAudiosPath.Background.Color := clCardBGDefaultDark;
  pAudiosPath.Border.Color := clSystemSolidNeutralFGDark;
  pAttachmentsPath.Background.Color := clCardBGDefaultDark;
  pAttachmentsPath.Border.Color := clSystemSolidNeutralFGDark;
  pOpenAfterExport.Background.Color := clCardBGDefaultDark;
  pOpenAfterExport.Border.Color := clSystemSolidNeutralFGDark;
  pRememberConnection.Background.Color := clCardBGDefaultDark;
  pRememberConnection.Border.Color := clSystemSolidNeutralFGDark;
  pRememberUser.Background.Color := clCardBGDefaultDark;
  pRememberUser.Border.Color := clSystemSolidNeutralFGDark;
  pAllowWriteLog.Background.Color := clCardBGDefaultDark;
  pAllowWriteLog.Border.Color := clSystemSolidNeutralFGDark;
  pAllowUsageData.Background.Color := clCardBGDefaultDark;
  pAllowUsageData.Border.Color := clSystemSolidNeutralFGDark;
  pPrivacyTerms.Background.Color := clCardBGDefaultDark;
  pPrivacyTerms.Border.Color := clSystemSolidNeutralFGDark;
  pBackupPath.Background.Color := clCardBGDefaultDark;
  pBackupPath.Border.Color := clSystemSolidNeutralFGDark;
  pStartupBackup.Background.Color := clCardBGDefaultDark;
  pStartupBackup.Border.Color := clSystemSolidNeutralFGDark;
  pManageBackups.Background.Color := clCardBGDefaultDark;
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
  if IsDarkModeEnabled then
    ApplyDarkMode;

  //SBox.VertScrollBar.Position := 0;
  tvMenu.Selected := tvMenu.Items.GetFirstNode;
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
      if MsgDlg(rsCheckUpdates, Format(rsNewUpdateAvailable, [NomeApp]), mtConfirmation) then
        RunUpdate;
    end;
    ckrError: ;
  end;
end;

procedure TcfgOptions.sbClearLogFilesClick(Sender: TObject);
var
  FLog: String;
begin
  FLog := ConcatPaths([AppDataDir, LogFile]);
  if FileExists(FLog) then
    DeleteFile(FLog);
end;

procedure TcfgOptions.tsEnterAsTabChange(Sender: TObject);
begin
  XSettings.UseEnterAsTab := tsEnterAsTab.Checked;
end;

procedure TcfgOptions.tsOpenAfterExportChange(Sender: TObject);
begin
  XSettings.OpenFileAfterExport := tsOpenAfterExport.Checked;
end;

procedure TcfgOptions.tsRememberConnectionChange(Sender: TObject);
begin
  XSettings.RememberConnection := tsRememberConnection.Checked;
  if tsRememberConnection.Checked then
    XSettings.LastConnection := ConexaoDB.Name
  else
    XSettings.Delete('SECURITY', 'LastConnection');
end;

procedure TcfgOptions.tsRememberUserChange(Sender: TObject);
begin
  XSettings.RememberUser := tsRememberUser.Checked;
  if tsRememberUser.Checked then
    XSettings.LastUser := ActiveUser.UserName
  else
    XSettings.Delete('SECURITY', 'LastUser');
end;

procedure TcfgOptions.tsShowOutliersChange(Sender: TObject);
begin
  XSettings.ShowOutliersOnGrid := tsShowOutliers.Checked;
end;

procedure TcfgOptions.tsShowSynonymsChange(Sender: TObject);
begin
  XSettings.ShowSynonyms := tsShowSynonyms.Checked;
end;

procedure TcfgOptions.tsUseConditionalFormattingChange(Sender: TObject);
begin
  XSettings.UseConditionalFormatting := tsUseConditionalFormatting.Checked;

  lblShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
  tsShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
end;

procedure TcfgOptions.tsWriteLogsChange(Sender: TObject);
begin
  XSettings.AllowWriteLogs := tsWriteLogs.Checked;
end;

procedure TcfgOptions.LoadConfig;
begin
  { GENERAL PARAMETERS AND INTERFACE }
  cbStartPage.ItemIndex := XSettings.StartPage;
  tsConfirmCancel.Checked := XSettings.ConfirmCancel;
  cbClearDeleted.ItemIndex := XSettings.ClearDeletedPeriod;
  tsEnterAsTab.Checked := XSettings.UseEnterAsTab;

  { APPEARANCE }
  cbSelectedTheme.ItemIndex := XSettings.SelectedTheme;
  tsUseConditionalFormatting.Checked := XSettings.UseConditionalFormatting;
  tsShowOutliers.Checked := XSettings.ShowOutliersOnGrid;

  { COLLECTION }
  cbVernacularNames.ItemIndex := XSettings.VernacularNamesLanguage;
  cbMainTaxonomy.ItemIndex := XSettings.Taxonomy;
  tsShowSynonyms.Checked := XSettings.ShowSynonyms;

  { MEDIA }
  eImagesPath.Text := XSettings.ImagesFolder;
  eAudiosPath.Text := XSettings.AudiosFolder;
  eAttachmentsPath.Text := XSettings.DocumentsFolder;

  { SECURITY }
  tsRememberUser.Checked := XSettings.RememberUser;
  tsRememberConnection.Checked := XSettings.RememberConnection;
  cbCheckUpdates.ItemIndex := XSettings.AutoUpdates;

  { PRIVACY }
  tsWriteLogs.Checked := XSettings.AllowWriteLogs;
  tsAllowUsageData.Checked := XSettings.AllowSendUsageData;

  { BACKUP AND RESTORE }
  eBackupPath.Text := XSettings.BackupFolder;
  cbStartupBackup.ItemIndex := XSettings.AutomaticBackup;

end;

end.

