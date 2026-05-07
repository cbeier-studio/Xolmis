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
  LazFileUtils, ToggleSwitch, atshapelinebgra, BCPanel, BCFluentSlider, Character;

type

  { TcfgOptions }

  TcfgOptions = class(TForm)
    btnHelp: TSpeedButton;
    btnClearBandSupplier: TButton;
    eBandSupplier: TEditButton;
    eVideosPath: TDirectoryEdit;
    icoAutoSizeColumns: TImage;
    icoAutoFillCoordinates: TImage;
    icoRememberCollectionInfo: TImage;
    icoBandSupplier: TImage;
    icoWriteDetailedLogs: TImage;
    icoVideosPath: TImage;
    lblAutoSizeColumns: TLabel;
    lblAutoFillCoordinates: TLabel;
    lblBandSupplierInfo: TLabel;
    lblRememberCollectionInfo: TLabel;
    lblBandSupplier: TLabel;
    lblWriteDetailedLogs: TLabel;
    lblTitleAppearance: TLabel;
    lblTitleBackup: TLabel;
    lblTitleCollection: TLabel;
    lblTitleInterface: TLabel;
    lblTitleMedia: TLabel;
    lblTitleSecurity: TLabel;
    lblVideosPath: TLabel;
    pAutoSizeColumns: TBCPanel;
    pAutoFillCoordinates: TBCPanel;
    pRememberCollectionInfo: TBCPanel;
    pBandSupplier: TBCPanel;
    pWriteDetailedLogs: TBCPanel;
    pVideosPath: TBCPanel;
    sliderRowHeight: TBCFluentSlider;
    btnDefaultRowHeight: TButton;
    cbCheckUpdates: TComboBox;
    cbClearDeleted: TComboBox;
    cbStartPage: TComboBox;
    cbSelectedTheme: TComboBox;
    cbVernacularNames: TComboBox;
    cbStartupBackup: TComboBox;
    eAttachmentsPath: TDirectoryEdit;
    eAudiosPath: TDirectoryEdit;
    eBackupPath: TDirectoryEdit;
    eImagesPath: TDirectoryEdit;
    icoSelectedTheme: TImage;
    icoRowHeight: TImage;
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
    icoEnterAsTab: TImage;
    icoVernacularNames: TImage;
    icoShowSynonyms: TImage;
    icoImagesPath: TImage;
    icoAudiosPath: TImage;
    icoDocumentsPath: TImage;
    iIcons: TImageList;
    lblAllowUsageData: TLabel;
    lblRowHeight: TLabel;
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
    lblManageBackups: TLabel;
    lblRememberConnection: TLabel;
    lblRememberUser: TLabel;
    lblSelectedTheme: TLabel;
    lblSelectedThemeRestart: TLabel;
    lblShowSynonyms: TLabel;
    lblStartupBackup: TLabel;
    lblVernacularNames: TLabel;
    nbPages: TNotebook;
    pRowHeight: TBCPanel;
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
    tsAutoSizeColumns: TToggleSwitch;
    tsAutoFillCoordinates: TToggleSwitch;
    tsRememberCollectionInfo: TToggleSwitch;
    tsWriteDetailedLogs: TToggleSwitch;
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
    procedure btnClearBandSupplierClick(Sender: TObject);
    procedure btnDefaultRowHeightClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbCheckUpdatesChange(Sender: TObject);
    procedure cbClearDeletedChange(Sender: TObject);
    procedure cbSelectedThemeChange(Sender: TObject);
    procedure cbStartPageChange(Sender: TObject);
    procedure cbStartupBackupSelect(Sender: TObject);
    procedure cbVernacularNamesChange(Sender: TObject);
    procedure eAttachmentsPathChange(Sender: TObject);
    procedure eAudiosPathChange(Sender: TObject);
    procedure eBackupPathChange(Sender: TObject);
    procedure eBandSupplierButtonClick(Sender: TObject);
    procedure eBandSupplierKeyPress(Sender: TObject; var Key: char);
    procedure eImagesPathChange(Sender: TObject);
    procedure eVideosPathChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblPrivacyTermsClick(Sender: TObject);
    procedure sbCheckUpdatesNowClick(Sender: TObject);
    procedure sbClearLogFilesClick(Sender: TObject);
    procedure sbNewBackupClick(Sender: TObject);
    procedure sbRestoreBackupClick(Sender: TObject);
    procedure sliderRowHeightChangeValue(Sender: TObject);
    procedure tsAllowUsageDataChange(Sender: TObject);
    procedure tsAutoFillCoordinatesChange(Sender: TObject);
    procedure tsAutoSizeColumnsChange(Sender: TObject);
    procedure tsConfirmCancelChange(Sender: TObject);
    procedure tsEnterAsTabChange(Sender: TObject);
    procedure tsOpenAfterExportChange(Sender: TObject);
    procedure tsRememberCollectionInfoChange(Sender: TObject);
    procedure tsRememberConnectionChange(Sender: TObject);
    procedure tsRememberUserChange(Sender: TObject);
    procedure tsShowOutliersChange(Sender: TObject);
    procedure tsShowSynonymsChange(Sender: TObject);
    procedure tsUseConditionalFormattingChange(Sender: TObject);
    procedure tsWriteDetailedLogsChange(Sender: TObject);
    procedure tsWriteLogsChange(Sender: TObject);
    procedure tvMenuSelectionChanged(Sender: TObject);
  private
    FLoadingConfig: Boolean;
    FChangingMediaPath: Boolean;
    procedure ApplyDarkMode;
    function IsLikelyUrl(const aValue: String): Boolean;
    function MigrateMediaPaths(const aTableName, aIdField, aOldBaseFolder, aNewBaseFolder: String;
      aSkipUrls: Boolean = False): Integer;
    function ConfirmAndMigrateMediaPaths(const aMediaLabel, aOldBaseFolder, aNewBaseFolder,
      aTableName, aIdField: String; aSkipUrls: Boolean = False): Boolean;
    procedure LoadConfig;
  public

  end;

var
  cfgOptions: TcfgOptions;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_backup, utils_autoupdate, utils_system, utils_themes,
  utils_finddialogs, utils_conversions,
  data_getvalue, data_types, data_consts,
  models_users, udm_main,
  uDarkStyleParams, SQLDB;

{$R *.lfm}

{ TcfgOptions }

procedure TcfgOptions.ApplyDarkMode;
begin
  tvMenu.Images := iIconsDark;
  icoStartPage.Images := iIconsDark;
  icoEnterAsTab.Images := iIconsDark;
  icoConfirmCancel.Images := iIconsDark;
  icoClearDeleted.Images := iIconsDark;
  icoCheckUpdates.Images := iIconsDark;
  icoSelectedTheme.Images := iIconsDark;
  icoAutoSizeColumns.Images := iIconsDark;
  icoRowHeight.Images := iIconsDark;
  icoUseConditionalFormatting.Images := iIconsDark;
  icoShowOutliers.Images := iIconsDark;
  icoVernacularNames.Images := iIconsDark;
  //icoMainTaxonomy.Images := iIconsDark;
  icoShowSynonyms.Images := iIconsDark;
  icoBandSupplier.Images := iIconsDark;
  icoRememberCollectionInfo.Images := iIconsDark;
  icoAutoFillCoordinates.Images := iIconsDark;
  icoImagesPath.Images := iIconsDark;
  icoAudiosPath.Images := iIconsDark;
  icoVideosPath.Images := iIconsDark;
  icoDocumentsPath.Images := iIconsDark;
  icoRememberConnection.Images := iIconsDark;
  icoRememberUser.Images := iIconsDark;
  icoAllowWriteLog.Images := iIconsDark;
  icoWriteDetailedLogs.Images := iIconsDark;
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
  pAutoSizeColumns.Background.Color := clSolidBGSecondaryDark;
  pAutoSizeColumns.Border.Color := clSystemSolidNeutralFGDark;
  pRowHeight.Background.Color := clSolidBGSecondaryDark;
  pRowHeight.Border.Color := clSystemSolidNeutralFGDark;
  pUseConditionalFormatting.Background.Color := clSolidBGSecondaryDark;
  pUseConditionalFormatting.Border.Color := clSystemSolidNeutralFGDark;
  pShowOutliers.Background.Color := clSolidBGSecondaryDark;
  pShowOutliers.Border.Color := clSystemSolidNeutralFGDark;
  pVernacularNames.Background.Color := clSolidBGSecondaryDark;
  pVernacularNames.Border.Color := clSystemSolidNeutralFGDark;
  //pMainTaxonomy.Background.Color := clSolidBGSecondaryDark;
  //pMainTaxonomy.Border.Color := clSystemSolidNeutralFGDark;
  pShowSynonyms.Background.Color := clSolidBGSecondaryDark;
  pShowSynonyms.Border.Color := clSystemSolidNeutralFGDark;
  pBandSupplier.Background.Color := clSolidBGSecondaryDark;
  pBandSupplier.Border.Color := clSystemSolidNeutralFGDark;
  pRememberCollectionInfo.Background.Color := clSolidBGSecondaryDark;
  pRememberCollectionInfo.Border.Color := clSystemSolidNeutralFGDark;
  pAutoFillCoordinates.Background.Color := clSolidBGSecondaryDark;
  pAutoFillCoordinates.Border.Color := clSystemSolidNeutralFGDark;
  pImagesPath.Background.Color := clSolidBGSecondaryDark;
  pImagesPath.Border.Color := clSystemSolidNeutralFGDark;
  pAudiosPath.Background.Color := clSolidBGSecondaryDark;
  pAudiosPath.Border.Color := clSystemSolidNeutralFGDark;
  pVideosPath.Background.Color := clSolidBGSecondaryDark;
  pVideosPath.Border.Color := clSystemSolidNeutralFGDark;
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
  pWriteDetailedLogs.Background.Color := clSolidBGSecondaryDark;
  pWriteDetailedLogs.Border.Color := clSystemSolidNeutralFGDark;
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

  lblTitleInterface.Font.Color := clVioletFG1Dark;
  lblTitleAppearance.Font.Color := clVioletFG1Dark;
  lblTitleCollection.Font.Color := clVioletFG1Dark;
  lblTitleMedia.Font.Color := clVioletFG1Dark;
  lblTitleSecurity.Font.Color := clVioletFG1Dark;
  lblTitleBackup.Font.Color := clVioletFG1Dark;

  tsEnterAsTab.Color := pEnterAsTab.Background.Color;
  tsConfirmCancel.Color := pConfirmCancel.Background.Color;
  tsAutoSizeColumns.Color := pAutoSizeColumns.Background.Color;
  tsUseConditionalFormatting.Color := pConfirmCancel.Background.Color;
  tsShowOutliers.Color := pConfirmCancel.Background.Color;
  tsShowSynonyms.Color := pConfirmCancel.Background.Color;
  tsRememberCollectionInfo.Color := pConfirmCancel.Background.Color;
  tsAutoFillCoordinates.Color := pConfirmCancel.Background.Color;
  tsOpenAfterExport.Color := pConfirmCancel.Background.Color;
  tsRememberConnection.Color := pConfirmCancel.Background.Color;
  tsRememberUser.Color := pConfirmCancel.Background.Color;
  tsWriteLogs.Color := pConfirmCancel.Background.Color;
  tsWriteDetailedLogs.Color := pConfirmCancel.Background.Color;
  tsAllowUsageData.Color := pConfirmCancel.Background.Color;

  eBandSupplier.Images := DMM.iEditsDark;
  eImagesPath.Images := DMM.iEditsDark;
  eAudiosPath.Images := DMM.iEditsDark;
  eVideosPath.Images := DMM.iEditsDark;
  eAttachmentsPath.Images := DMM.iEditsDark;
  eBackupPath.Images := DMM.iEditsDark;
end;

procedure TcfgOptions.btnClearBandSupplierClick(Sender: TObject);
begin
  xSettings.DefaultBandSupplier := 0;
  eBandSupplier.Clear;
end;

procedure TcfgOptions.btnDefaultRowHeightClick(Sender: TObject);
begin
  //xSettings.DefaultRowHeight := DEFAULT_ROW_HEIGHT;
  sliderRowHeight.Value := (DEFAULT_ROW_HEIGHT - MIN_ROW_HEIGHT) div 2;
end;

procedure TcfgOptions.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_SETTINGS);
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

procedure TcfgOptions.cbCheckUpdatesChange(Sender: TObject);
begin
  xSettings.AutoUpdates := cbCheckUpdates.ItemIndex;
end;

procedure TcfgOptions.cbVernacularNamesChange(Sender: TObject);
begin
  xSettings.VernacularNamesLanguage := cbVernacularNames.ItemIndex;
end;

procedure TcfgOptions.eAttachmentsPathChange(Sender: TObject);
var
  OldPath, NewPath: String;
begin
  if FChangingMediaPath then
    Exit;

  OldPath := xSettings.DocumentsFolder;
  NewPath := eAttachmentsPath.Text;

  if FLoadingConfig or SameText(ExcludeTrailingPathDelimiter(OldPath), ExcludeTrailingPathDelimiter(NewPath)) then
  begin
    xSettings.DocumentsFolder := NewPath;
    Exit;
  end;

  if ConfirmAndMigrateMediaPaths(LowerCase(rsTitleDocuments), OldPath, NewPath, TBL_DOCUMENTS, COL_DOCUMENT_ID, True) then
    xSettings.DocumentsFolder := NewPath
  else
  begin
    FChangingMediaPath := True;
    try
      eAttachmentsPath.Text := OldPath;
    finally
      FChangingMediaPath := False;
    end;
  end;
end;

procedure TcfgOptions.eAudiosPathChange(Sender: TObject);
var
  OldPath, NewPath: String;
begin
  if FChangingMediaPath then
    Exit;

  OldPath := xSettings.AudiosFolder;
  NewPath := eAudiosPath.Text;

  if FLoadingConfig or SameText(ExcludeTrailingPathDelimiter(OldPath), ExcludeTrailingPathDelimiter(NewPath)) then
  begin
    xSettings.AudiosFolder := NewPath;
    Exit;
  end;

  if ConfirmAndMigrateMediaPaths(LowerCase(rsTitleAudioLibrary), OldPath, NewPath, TBL_AUDIO_LIBRARY, COL_AUDIO_ID) then
    xSettings.AudiosFolder := NewPath
  else
  begin
    FChangingMediaPath := True;
    try
      eAudiosPath.Text := OldPath;
    finally
      FChangingMediaPath := False;
    end;
  end;
end;

procedure TcfgOptions.eBackupPathChange(Sender: TObject);
begin
  xSettings.BackupFolder := eBackupPath.Text;
end;

procedure TcfgOptions.eBandSupplierButtonClick(Sender: TObject);
var
  FSupplierId: Integer;
begin
  FSupplierId := xSettings.DefaultBandSupplier;
  FindDlg(tbInstitutions, eBandSupplier, FSupplierId, '', COL_ABBREVIATION);
  xSettings.DefaultBandSupplier := FSupplierId;
end;

procedure TcfgOptions.eBandSupplierKeyPress(Sender: TObject; var Key: char);
var
  FSupplierId: Integer;
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FSupplierId := xSettings.DefaultBandSupplier;
    FindDlg(tbInstitutions, eBandSupplier, FSupplierId, Key, COL_ABBREVIATION);
    xSettings.DefaultBandSupplier := FSupplierId;
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    xSettings.DefaultBandSupplier := 0;
    eBandSupplier.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  //if (Key = #13) and (xSettings.UseEnterAsTab) then
  //begin
  //  if (Sender is TEditButton) then
  //    Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
  //  else
  //    SelectNext(Sender as TWinControl, True, True);
  //  Key := #0;
  //end;
end;

procedure TcfgOptions.eImagesPathChange(Sender: TObject);
var
  OldPath, NewPath: String;
begin
  if FChangingMediaPath then
    Exit;

  OldPath := xSettings.ImagesFolder;
  NewPath := eImagesPath.Text;

  if FLoadingConfig or SameText(ExcludeTrailingPathDelimiter(OldPath), ExcludeTrailingPathDelimiter(NewPath)) then
  begin
    xSettings.ImagesFolder := NewPath;
    Exit;
  end;

  if ConfirmAndMigrateMediaPaths(LowerCase(rsTitleImages), OldPath, NewPath, TBL_IMAGES, COL_IMAGE_ID) then
    xSettings.ImagesFolder := NewPath
  else
  begin
    FChangingMediaPath := True;
    try
      eImagesPath.Text := OldPath;
    finally
      FChangingMediaPath := False;
    end;
  end;
end;

procedure TcfgOptions.eVideosPathChange(Sender: TObject);
var
  OldPath, NewPath: String;
begin
  if FChangingMediaPath then
    Exit;

  OldPath := xSettings.VideosFolder;
  NewPath := eVideosPath.Text;

  if FLoadingConfig or SameText(ExcludeTrailingPathDelimiter(OldPath), ExcludeTrailingPathDelimiter(NewPath)) then
  begin
    xSettings.VideosFolder := NewPath;
    Exit;
  end;

  if ConfirmAndMigrateMediaPaths(LowerCase(rsTitleVideos), OldPath, NewPath, TBL_VIDEOS, COL_VIDEO_ID) then
    xSettings.VideosFolder := NewPath
  else
  begin
    FChangingMediaPath := True;
    try
      eVideosPath.Text := OldPath;
    finally
      FChangingMediaPath := False;
    end;
  end;
end;

procedure TcfgOptions.FormDestroy(Sender: TObject);
begin
  xSettings.SaveToFile;
end;

function TcfgOptions.IsLikelyUrl(const aValue: String): Boolean;
var
  S: String;
begin
  S := LowerCase(Trim(aValue));
  Result := (Pos('://', S) > 0) or (Pos('mailto:', S) = 1) or (Pos('www.', S) = 1);
end;

function TcfgOptions.MigrateMediaPaths(const aTableName, aIdField, aOldBaseFolder, aNewBaseFolder: String;
  aSkipUrls: Boolean): Integer;
var
  QrySel, QryUpd: TSQLQuery;
  PrevTransActive: Boolean;
  OldRelPath, OldAbsPath, NewRelPath: String;
begin
  Result := 0;

  PrevTransActive := DMM.sqlTrans.Active;
  if not PrevTransActive then
    DMM.sqlTrans.StartTransaction;

  QrySel := TSQLQuery.Create(nil);
  QryUpd := TSQLQuery.Create(nil);
  try
    QrySel.SQLConnection := DMM.sqlCon;
    QryUpd.SQLConnection := DMM.sqlCon;

    try
      QrySel.SQL.Text := 'SELECT ' + aIdField + ', ' + COL_FILE_PATH + ' FROM ' + aTableName;
      QrySel.Open;

      QryUpd.SQL.Text := 'UPDATE ' + aTableName + ' SET ' + COL_FILE_PATH + ' = :new_path WHERE ' + aIdField + ' = :id';

      while not QrySel.EOF do
      begin
        OldRelPath := Trim(QrySel.FieldByName(COL_FILE_PATH).AsString);
        if (OldRelPath <> EmptyStr) and (not (aSkipUrls and IsLikelyUrl(OldRelPath))) then
        begin
          OldAbsPath := CreateAbsolutePath(OldRelPath, aOldBaseFolder);
          NewRelPath := ExtractRelativePath(aNewBaseFolder, OldAbsPath);

          if not SameText(OldRelPath, NewRelPath) then
          begin
            QryUpd.ParamByName('new_path').AsString := NewRelPath;
            QryUpd.ParamByName('id').AsInteger := QrySel.FieldByName(aIdField).AsInteger;
            QryUpd.ExecSQL;
            Inc(Result);
          end;
        end;

        QrySel.Next;
      end;
    finally
      QrySel.Free;
      QryUpd.Free;
    end;

    if not PrevTransActive then
      DMM.sqlTrans.CommitRetaining;
  except
    if not PrevTransActive then
      DMM.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

function TcfgOptions.ConfirmAndMigrateMediaPaths(const aMediaLabel, aOldBaseFolder, aNewBaseFolder,
  aTableName, aIdField: String; aSkipUrls: Boolean): Boolean;
var
  UpdatedCount: Integer;
  Msg: String;
begin
  Result := False;

  Msg := Format(rsPromptMigrateMediaPath, [aMediaLabel, aOldBaseFolder, aNewBaseFolder]);

  if not MsgDlg(rsMediaFolderChanged, Msg, mtConfirmation) then
    Exit(False);

  try
    UpdatedCount := MigrateMediaPaths(aTableName, aIdField, aOldBaseFolder, aNewBaseFolder, aSkipUrls);
    MsgDlg(rsMediaMigrationCompleted, Format(rsMigratedMediaPaths, [UpdatedCount, aMediaLabel]), mtInformation);
    Result := True;
  except
    on E: Exception do
    begin
      MsgDlg(rsTitleError, Format(rsMediaMigrationError, [E.Message]), mtWarning);
      Result := False;
    end;
  end;
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

  lblShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
  tsShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
  lblWriteDetailedLogs.Enabled := tsWriteLogs.Checked;
  tsWriteDetailedLogs.Enabled := tsWriteLogs.Checked;
end;

procedure TcfgOptions.lblPrivacyTermsClick(Sender: TObject);
begin
  OpenHelp(HELP_PRIVACY);
end;

procedure TcfgOptions.LoadConfig;
begin
  FLoadingConfig := True;
  try
  { GENERAL PARAMETERS AND INTERFACE }
  cbStartPage.ItemIndex := xSettings.StartPage;
  tsConfirmCancel.Checked := xSettings.ConfirmCancel;
  cbClearDeleted.ItemIndex := xSettings.ClearDeletedPeriod;
  tsEnterAsTab.Checked := xSettings.UseEnterAsTab;

  { APPEARANCE }
  cbSelectedTheme.ItemIndex := xSettings.SelectedTheme;
  tsAutoSizeColumns.Checked := xSettings.AutoAdjustColumns;
  sliderRowHeight.Value := (xSettings.DefaultRowHeight - MIN_ROW_HEIGHT) div 2;
  tsUseConditionalFormatting.Checked := xSettings.UseConditionalFormatting;
  tsShowOutliers.Checked := xSettings.ShowOutliersOnGrid;

  { COLLECTION }
  cbVernacularNames.ItemIndex := xSettings.VernacularNamesLanguage;
  //cbMainTaxonomy.ItemIndex := xSettings.Taxonomy;
  tsShowSynonyms.Checked := xSettings.ShowSynonyms;
  if xSettings.DefaultBandSupplier > 0 then
    eBandSupplier.Text := GetName(TBL_INSTITUTIONS, COL_ABBREVIATION, COL_INSTITUTION_ID, xSettings.DefaultBandSupplier);
  tsRememberCollectionInfo.Checked := xSettings.RememberCollectionInfo;
  tsAutoFillCoordinates.Checked := xSettings.AutoFillCoordinates;

  { MEDIA }
  eImagesPath.Text := xSettings.ImagesFolder;
  eAudiosPath.Text := xSettings.AudiosFolder;
  eVideosPath.Text := xSettings.VideosFolder;
  eAttachmentsPath.Text := xSettings.DocumentsFolder;

  { SECURITY }
  tsRememberUser.Checked := xSettings.RememberUser;
  tsRememberConnection.Checked := xSettings.RememberConnection;
  cbCheckUpdates.ItemIndex := xSettings.AutoUpdates;

  { PRIVACY }
  tsWriteLogs.Checked := xSettings.AllowWriteLogs;
  tsWriteDetailedLogs.Checked := xSettings.WriteDetailedLogs;
  tsAllowUsageData.Checked := xSettings.AllowSendUsageData;

  { BACKUP AND RESTORE }
  eBackupPath.Text := xSettings.BackupFolder;
  cbStartupBackup.ItemIndex := xSettings.AutomaticBackup;
  finally
    FLoadingConfig := False;
  end;

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

procedure TcfgOptions.sbNewBackupClick(Sender: TObject);
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');

  VacuumIntoBackup; //NewBackup;
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

procedure TcfgOptions.sliderRowHeightChangeValue(Sender: TObject);
begin
  xSettings.DefaultRowHeight := MIN_ROW_HEIGHT + (sliderRowHeight.Value * 2);
end;

procedure TcfgOptions.tsAllowUsageDataChange(Sender: TObject);
begin
  xSettings.AllowSendUsageData := tsAllowUsageData.Checked;
end;

procedure TcfgOptions.tsAutoFillCoordinatesChange(Sender: TObject);
begin
  xSettings.AutoFillCoordinates := tsAutoFillCoordinates.Checked;
end;

procedure TcfgOptions.tsAutoSizeColumnsChange(Sender: TObject);
begin
  xSettings.AutoAdjustColumns := tsAutoSizeColumns.Checked;
end;

procedure TcfgOptions.tsConfirmCancelChange(Sender: TObject);
begin
  xSettings.ConfirmCancel := tsConfirmCancel.Checked;
end;

procedure TcfgOptions.tsEnterAsTabChange(Sender: TObject);
begin
  xSettings.UseEnterAsTab := tsEnterAsTab.Checked;
end;

procedure TcfgOptions.tsOpenAfterExportChange(Sender: TObject);
begin
  xSettings.OpenFileAfterExport := tsOpenAfterExport.Checked;
end;

procedure TcfgOptions.tsRememberCollectionInfoChange(Sender: TObject);
begin
  xSettings.RememberCollectionInfo := tsRememberCollectionInfo.Checked;
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

procedure TcfgOptions.tsWriteDetailedLogsChange(Sender: TObject);
begin
  xSettings.WriteDetailedLogs := tsWriteDetailedLogs.Checked;
end;

procedure TcfgOptions.tsWriteLogsChange(Sender: TObject);
begin
  xSettings.AllowWriteLogs := tsWriteLogs.Checked;

  lblWriteDetailedLogs.Enabled := tsWriteLogs.Checked;
  tsWriteDetailedLogs.Enabled := tsWriteLogs.Checked;
end;

procedure TcfgOptions.tvMenuSelectionChanged(Sender: TObject);
begin
  nbPages.PageIndex := tvMenu.Selected.Index;
end;

end.

