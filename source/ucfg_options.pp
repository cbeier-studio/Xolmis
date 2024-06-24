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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, ComCtrls,
  atshapelinebgra, BCPanel, rxswitch;

type

  { TcfgOptions }

  TcfgOptions = class(TForm)
    cbCheckUpdates: TComboBox;
    cbClearDeleted: TComboBox;
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
    Image1: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    Image17: TImage;
    icoUseConditionalFormatting: TImage;
    icoShowOutliers: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    iIcons: TImageList;
    lblAllowUsageData: TLabel;
    lblAllowUsageData1: TLabel;
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
    pShowOutliers: TBCPanel;
    pContentAppearance: TPanel;
    pUseConditionalFormatting: TBCPanel;
    pgAppearance: TPage;
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
    pSelectedTheme: TBCPanel;
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
    scrollAppearance: TScrollBox;
    scrollMedia: TScrollBox;
    scrollSecurity: TScrollBox;
    scrollBackup: TScrollBox;
    tsShowOutliers: TRxSwitch;
    tsUseConditionalFormatting: TRxSwitch;
    tvMenu: TTreeView;
    tsAllowUsageData: TRxSwitch;
    tsConfirmCancel: TRxSwitch;
    tsEnterAsTab: TRxSwitch;
    tsRememberConnection: TRxSwitch;
    tsRememberUser: TRxSwitch;
    tsShowSynonyms: TRxSwitch;
    tsWriteLogs: TRxSwitch;
    procedure cbCheckUpdatesChange(Sender: TObject);
    procedure cbClearDeletedChange(Sender: TObject);
    procedure cbMainTaxonomyChange(Sender: TObject);
    procedure cbSelectedThemeChange(Sender: TObject);
    procedure cbStartupBackupSelect(Sender: TObject);
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
    procedure tsShowOutliersOn(Sender: TObject);
    procedure tsUseConditionalFormattingOn(Sender: TObject);
    procedure tsWriteLogsOn(Sender: TObject);
    procedure tsConfirmCancelOn(Sender: TObject);
    procedure tsEnterAsTabOn(Sender: TObject);
    procedure tsRememberConnectionOn(Sender: TObject);
    procedure tsRememberUserOn(Sender: TObject);
    procedure tsShowSynonymsOn(Sender: TObject);
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

procedure TcfgOptions.cbSelectedThemeChange(Sender: TObject);
begin
  XSettings.SelectedTheme := cbSelectedTheme.ItemIndex;
  lblSelectedThemeRestart.Visible := True;
end;

procedure TcfgOptions.cbStartupBackupSelect(Sender: TObject);
begin
  XSettings.StartupBackup := cbStartupBackup.ItemIndex;
end;

procedure TcfgOptions.cbClearDeletedChange(Sender: TObject);
begin
  XSettings.ClearDeletedPeriod := cbClearDeleted.ItemIndex;
end;

procedure TcfgOptions.ApplyDarkMode;
begin
  tvMenu.Images := iIconsDark;
  Image4.Images := iIconsDark;
  Image1.Images := iIconsDark;
  Image2.Images := iIconsDark;
  Image10.Images := iIconsDark;
  icoSelectedTheme.Images := iIconsDark;
  icoUseConditionalFormatting.Images := iIconsDark;
  icoShowOutliers.Images := iIconsDark;
  Image5.Images := iIconsDark;
  Image3.Images := iIconsDark;
  Image6.Images := iIconsDark;
  Image7.Images := iIconsDark;
  Image8.Images := iIconsDark;
  Image9.Images := iIconsDark;
  Image11.Images := iIconsDark;
  Image12.Images := iIconsDark;
  Image13.Images := iIconsDark;
  Image14.Images := iIconsDark;
  Image17.Images := iIconsDark;
  Image15.Images := iIconsDark;
  Image16.Images := iIconsDark;

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
  pRememberConnection.Background.Color := clCardBGDefaultDark;
  pRememberConnection.Border.Color := clSystemSolidNeutralFGDark;
  pRememberUser.Background.Color := clCardBGDefaultDark;
  pRememberUser.Border.Color := clSystemSolidNeutralFGDark;
  pAllowWriteLog.Background.Color := clCardBGDefaultDark;
  pAllowWriteLog.Border.Color := clSystemSolidNeutralFGDark;
  pAllowUsageData.Background.Color := clCardBGDefaultDark;
  pAllowUsageData.Border.Color := clSystemSolidNeutralFGDark;
  pAllowUsageData1.Background.Color := clCardBGDefaultDark;
  pAllowUsageData1.Border.Color := clSystemSolidNeutralFGDark;
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

procedure TcfgOptions.tsShowOutliersOn(Sender: TObject);
begin
  XSettings.ShowOutliersOnGrid := tsShowOutliers.StateOn = sw_on;
end;

procedure TcfgOptions.tsShowSynonymsOn(Sender: TObject);
begin
  XSettings.ShowSynonyms := tsShowSynonyms.StateOn = sw_on;
end;

procedure TcfgOptions.tsUseConditionalFormattingOn(Sender: TObject);
begin
  XSettings.UseConditionalFormatting := tsUseConditionalFormatting.StateOn = sw_on;

  lblShowOutliers.Enabled := tsUseConditionalFormatting.StateOn = sw_on;
  tsShowOutliers.Enabled := tsUseConditionalFormatting.StateOn = sw_on;
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

  { APPEARANCE }
  cbSelectedTheme.ItemIndex := XSettings.SelectedTheme;
  if XSettings.UseConditionalFormatting then
    tsUseConditionalFormatting.StateOn := sw_on
  else
    tsUseConditionalFormatting.StateOn := sw_off;
  if XSettings.ShowOutliersOnGrid then
    tsShowOutliers.StateOn := sw_on
  else
    tsShowOutliers.StateOn := sw_off;

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
  cbStartupBackup.ItemIndex := XSettings.StartupBackup;

end;

end.

