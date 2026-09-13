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
  LazFileUtils, ToggleSwitch, atshapelinebgra, BCPanel, BCFluentSlider, Character, FileUtil, FileCtrl,
  utils_media;

type

  { TcfgOptions }

  TcfgOptions = class(TForm)
    btnForceMediaMigration: TButton;
    btnClearThumbnailCache: TButton;
    btnHelp: TSpeedButton;
    btnClearBandSupplier: TButton;
    btnChangeMediaPath: TButton;
    cbMoveMediaFile: TComboBox;
    eBandSupplier: TEditButton;
    icoAutoSizeColumns: TImage;
    icoAutoFillCoordinates: TImage;
    icoRememberCollectionInfo: TImage;
    icoBandSupplier: TImage;
    icoWriteDetailedLogs: TImage;
    icoDeleteMedia: TImage;
    lblAutoSizeColumns: TLabel;
    lblAutoFillCoordinates: TLabel;
    lblBandSupplierInfo: TLabel;
    lblThumbnailCache: TLabel;
    SelectDirDlg: TSelectDirectoryDialog;
    tsDeleteMediaFile: TToggleSwitch;
    txtMediaPath: TLabel;
    lblRememberCollectionInfo: TLabel;
    lblBandSupplier: TLabel;
    lblWriteDetailedLogs: TLabel;
    lblTitleAppearance: TLabel;
    lblTitleBackup: TLabel;
    lblTitleCollection: TLabel;
    lblTitleInterface: TLabel;
    lblTitleMedia: TLabel;
    lblTitleSecurity: TLabel;
    lblDeleteMedia: TLabel;
    pAutoSizeColumns: TBCPanel;
    pAutoFillCoordinates: TBCPanel;
    pRememberCollectionInfo: TBCPanel;
    pBandSupplier: TBCPanel;
    pWriteDetailedLogs: TBCPanel;
    pDeleteMedia: TBCPanel;
    sliderRowHeight: TBCFluentSlider;
    btnDefaultRowHeight: TButton;
    cbCheckUpdates: TComboBox;
    cbClearDeleted: TComboBox;
    cbStartPage: TComboBox;
    cbSelectedTheme: TComboBox;
    cbVernacularNames: TComboBox;
    cbStartupBackup: TComboBox;
    eBackupPath: TDirectoryEdit;
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
    icoMediaPath: TImage;
    icoMoveMedia: TImage;
    iIcons: TImageList;
    lblAllowUsageData: TLabel;
    lblRowHeight: TLabel;
    lblStartPage: TLabel;
    lblOpenAfterExport: TLabel;
    lblPrivacyTerms: TLabel;
    lblAllowWriteLog: TLabel;
    lblMoveMedia: TLabel;
    lblBackupPath: TLabel;
    lblCheckUpdates: TLabel;
    lblClearDeleted: TLabel;
    lblShowOutliers: TLabel;
    lblUseConditionalFormatting: TLabel;
    lblConfirmCancel: TLabel;
    lblEnterAsTab: TLabel;
    lblMediaPath: TLabel;
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
    pMoveMedia: TBCPanel;
    pContentGeneral: TPanel;
    pContentCollection: TPanel;
    pContentMedia: TPanel;
    pClearDeleted: TBCPanel;
    pConfirmCancel: TBCPanel;
    pEnterAsTab: TBCPanel;
    pgGeneral: TPage;
    pgCollection: TPage;
    pgMedia: TPage;
    pMediaPath: TBCPanel;
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
    procedure btnChangeMediaPathClick(Sender: TObject);
    procedure btnClearBandSupplierClick(Sender: TObject);
    procedure btnClearThumbnailCacheClick(Sender: TObject);
    procedure btnDefaultRowHeightClick(Sender: TObject);
    procedure btnForceMediaMigrationClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbCheckUpdatesChange(Sender: TObject);
    procedure cbClearDeletedChange(Sender: TObject);
    procedure cbMoveMediaFileChange(Sender: TObject);
    procedure cbSelectedThemeChange(Sender: TObject);
    procedure cbStartPageChange(Sender: TObject);
    procedure cbStartupBackupSelect(Sender: TObject);
    procedure cbVernacularNamesChange(Sender: TObject);
    procedure eBackupPathChange(Sender: TObject);
    procedure eBandSupplierButtonClick(Sender: TObject);
    procedure eBandSupplierKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
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
    procedure tsDeleteMediaFileChange(Sender: TObject);
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
    FThumbManager: TThumbnailManager;
    procedure ApplyDarkMode;
    function IsLikelyUrl(const aValue: String): Boolean;
    function MigrateMediaPaths(const aTableName, aOldBaseFolder, aNewBaseFolder: String; MustMoveFiles: Boolean = True;
      aSkipUrls: Boolean = True): Integer;
    function ConfirmAndMigrateMediaPaths(aOldBaseFolder, aNewBaseFolder: String): Integer;
    procedure LoadConfig;
    procedure RefreshCacheSizeDisplay;
  public

  end;

var
  cfgOptions: TcfgOptions;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_backup, utils_autoupdate, utils_themes,
  utils_finddialogs, utils_conversions,
  data_getvalue, data_types, data_consts, data_management,
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
  icoMediaPath.Images := iIconsDark;
  icoMoveMedia.Images := iIconsDark;
  icoDeleteMedia.Images := iIconsDark;
  icoRememberConnection.Images := iIconsDark;
  icoRememberUser.Images := iIconsDark;
  icoAllowWriteLog.Images := iIconsDark;
  icoWriteDetailedLogs.Images := iIconsDark;
  icoAllowUsageData.Images := iIconsDark;
  imgPrivacyTerms.Images := iIconsDark;
  icoBackupPath.Images := iIconsDark;
  icoStartupBackup.Images := iIconsDark;
  icoOpenAfterExport.Images := iIconsDark;

  pStartPage.Background.Color := ActiveTheme.Background.SolidSecondary;
  pStartPage.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pEnterAsTab.Background.Color := ActiveTheme.Background.SolidSecondary;
  pEnterAsTab.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pConfirmCancel.Background.Color := ActiveTheme.Background.SolidSecondary;
  pConfirmCancel.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pClearDeleted.Background.Color := ActiveTheme.Background.SolidSecondary;
  pClearDeleted.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pCheckUpdates.Background.Color := ActiveTheme.Background.SolidSecondary;
  pCheckUpdates.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pSelectedTheme.Background.Color := ActiveTheme.Background.SolidSecondary;
  pSelectedTheme.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pAutoSizeColumns.Background.Color := ActiveTheme.Background.SolidSecondary;
  pAutoSizeColumns.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pRowHeight.Background.Color := ActiveTheme.Background.SolidSecondary;
  pRowHeight.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pUseConditionalFormatting.Background.Color := ActiveTheme.Background.SolidSecondary;
  pUseConditionalFormatting.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pShowOutliers.Background.Color := ActiveTheme.Background.SolidSecondary;
  pShowOutliers.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pVernacularNames.Background.Color := ActiveTheme.Background.SolidSecondary;
  pVernacularNames.Border.Color := ActiveTheme.System.SolidNeutralFG;
  //pMainTaxonomy.Background.Color := clSolidBGSecondaryDark;
  //pMainTaxonomy.Border.Color := clSystemSolidNeutralFGDark;
  pShowSynonyms.Background.Color := ActiveTheme.Background.SolidSecondary;
  pShowSynonyms.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pBandSupplier.Background.Color := ActiveTheme.Background.SolidSecondary;
  pBandSupplier.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pRememberCollectionInfo.Background.Color := ActiveTheme.Background.SolidSecondary;
  pRememberCollectionInfo.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pAutoFillCoordinates.Background.Color := ActiveTheme.Background.SolidSecondary;
  pAutoFillCoordinates.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pMediaPath.Background.Color := ActiveTheme.Background.SolidSecondary;
  pMediaPath.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pMoveMedia.Background.Color := ActiveTheme.Background.SolidSecondary;
  pMoveMedia.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pDeleteMedia.Background.Color := ActiveTheme.Background.SolidSecondary;
  pDeleteMedia.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pOpenAfterExport.Background.Color := ActiveTheme.Background.SolidSecondary;
  pOpenAfterExport.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pRememberConnection.Background.Color := ActiveTheme.Background.SolidSecondary;
  pRememberConnection.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pRememberUser.Background.Color := ActiveTheme.Background.SolidSecondary;
  pRememberUser.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pAllowWriteLog.Background.Color := ActiveTheme.Background.SolidSecondary;
  pAllowWriteLog.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pWriteDetailedLogs.Background.Color := ActiveTheme.Background.SolidSecondary;
  pWriteDetailedLogs.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pAllowUsageData.Background.Color := ActiveTheme.Background.SolidSecondary;
  pAllowUsageData.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pPrivacyTerms.Background.Color := ActiveTheme.Background.SolidSecondary;
  pPrivacyTerms.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pBackupPath.Background.Color := ActiveTheme.Background.SolidSecondary;
  pBackupPath.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pStartupBackup.Background.Color := ActiveTheme.Background.SolidSecondary;
  pStartupBackup.Border.Color := ActiveTheme.System.SolidNeutralFG;
  pManageBackups.Background.Color := ActiveTheme.Background.SolidSecondary;
  pManageBackups.Border.Color := ActiveTheme.System.SolidNeutralFG;

  lblTitleInterface.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleAppearance.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleCollection.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleMedia.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleSecurity.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleBackup.Font.Color := ActiveTheme.Interactive.WindowTitle;

  tsEnterAsTab.Color := pEnterAsTab.Background.Color;
  tsConfirmCancel.Color := pConfirmCancel.Background.Color;
  tsAutoSizeColumns.Color := pAutoSizeColumns.Background.Color;
  tsUseConditionalFormatting.Color := pConfirmCancel.Background.Color;
  tsShowOutliers.Color := pConfirmCancel.Background.Color;
  tsShowSynonyms.Color := pConfirmCancel.Background.Color;
  tsRememberCollectionInfo.Color := pConfirmCancel.Background.Color;
  tsAutoFillCoordinates.Color := pConfirmCancel.Background.Color;
  tsDeleteMediaFile.Color := pConfirmCancel.Background.Color;
  tsOpenAfterExport.Color := pConfirmCancel.Background.Color;
  tsRememberConnection.Color := pConfirmCancel.Background.Color;
  tsRememberUser.Color := pConfirmCancel.Background.Color;
  tsWriteLogs.Color := pConfirmCancel.Background.Color;
  tsWriteDetailedLogs.Color := pConfirmCancel.Background.Color;
  tsAllowUsageData.Color := pConfirmCancel.Background.Color;

  eBandSupplier.Images := DMM.iEditsDark;
  eBackupPath.Images := DMM.iEditsDark;

  tvMenu.SelectionColor := clVioletBrand1Dark;
end;

procedure TcfgOptions.btnChangeMediaPathClick(Sender: TObject);
var
  OldPath, NewPath: String;
begin
  OldPath := xSettings.MediaStorageFolder;
  SelectDirDlg.InitialDir := OldPath;
  if SelectDirDlg.Execute then
  begin
    NewPath := SelectDirDlg.FileName;
  end;

  if SameText(ExcludeTrailingPathDelimiter(OldPath), ExcludeTrailingPathDelimiter(NewPath)) then
    Exit;

  case ConfirmAndMigrateMediaPaths(OldPath, NewPath) of
    101, 102, 103:
    begin
      xSettings.MediaStorageFolder := NewPath;
    end
    else
    begin
      xSettings.MediaStorageFolder := OldPath;
    end;
  end;
  txtMediaPath.Caption := MinimizeName(xSettings.MediaStorageFolder, txtMediaPath.Canvas, txtMediaPath.Width);
end;

procedure TcfgOptions.btnClearBandSupplierClick(Sender: TObject);
begin
  xSettings.DefaultBandSupplier := 0;
  eBandSupplier.Clear;
end;

procedure TcfgOptions.btnClearThumbnailCacheClick(Sender: TObject);
begin
  if MsgDlg(rsClearThumbnailCache, rsClearThumbnailCachePrompt, mtConfirmation) then
  begin
    Screen.Cursor := crHourGlass;
    try
      FThumbManager.ClearAllCache;
      RefreshCacheSizeDisplay;
      ShowMessage(rsSuccessfulThumbnailCacheCleared);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TcfgOptions.btnDefaultRowHeightClick(Sender: TObject);
begin
  //xSettings.DefaultRowHeight := DEFAULT_ROW_HEIGHT;
  sliderRowHeight.Value := (DEFAULT_ROW_HEIGHT - MIN_ROW_HEIGHT) div 2;
end;

procedure TcfgOptions.btnForceMediaMigrationClick(Sender: TObject);
begin
  MigrateMediaToManagedFolder;
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

procedure TcfgOptions.cbMoveMediaFileChange(Sender: TObject);
begin
  xSettings.MoveOriginalFile := TMoveMediaType(cbMoveMediaFile.ItemIndex);
end;

procedure TcfgOptions.cbCheckUpdatesChange(Sender: TObject);
begin
  xSettings.AutoUpdates := cbCheckUpdates.ItemIndex;
end;

procedure TcfgOptions.cbVernacularNamesChange(Sender: TObject);
begin
  xSettings.VernacularNamesLanguage := cbVernacularNames.ItemIndex;
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

procedure TcfgOptions.FormCreate(Sender: TObject);
begin
  FThumbManager := TThumbnailManager.Create;
end;

procedure TcfgOptions.FormDestroy(Sender: TObject);
begin
  xSettings.SaveToFile;

  FThumbManager.Free;
end;

function TcfgOptions.IsLikelyUrl(const aValue: String): Boolean;
var
  S: String;
begin
  S := LowerCase(Trim(aValue));
  Result := (Pos('://', S) > 0) or (Pos('mailto:', S) = 1) or (Pos('www.', S) = 1);
end;

function TcfgOptions.MigrateMediaPaths(const aTableName, aOldBaseFolder, aNewBaseFolder: String;
  MustMoveFiles: Boolean; aSkipUrls: Boolean): Integer;
var
  QrySel: TSQLQuery;
  PrevTransActive, WasMoved: Boolean;
  OldRelPath, OldAbsPath, NewAbsPath: String;
begin
  Result := 0;

  QrySel := TSQLQuery.Create(nil);
  QrySel.SQLConnection := DMM.sqlCon;
  QrySel.MacroCheck := True;

  try
    QrySel.SQL.Text := 'SELECT file_path FROM %atable';
    //if (aTableName = TBL_DOCUMENTS) and (aSkipUrls) then
    //  QrySel.SQL.Add('WHERE document_type != ''url''');
    QrySel.MacroByName('atable').Value := aTableName;
    QrySel.Open;

    while not QrySel.EOF do
    begin
      OldRelPath := Trim(QrySel.FieldByName(COL_FILE_PATH).AsString);
      if (OldRelPath <> EmptyStr) and (not (aSkipUrls and IsLikelyUrl(OldRelPath))) then
      begin
        OldAbsPath := CreateAbsolutePath(OldRelPath, aOldBaseFolder);
        NewAbsPath := CreateAbsolutePath(OldRelPath, aNewBaseFolder);

        if not FileExists(OldAbsPath) then
          raise Exception.CreateFmt(rsErrorFileNotFound, [OldAbsPath]);

        if not SameText(OldAbsPath, NewAbsPath) then
        begin
          if not ForceDirectory(ExtractFileDir(NewAbsPath)) then
            raise Exception.Create(rsErrorCreateFolder + ' ' + NewAbsPath);
          if not CopyFile(OldAbsPath, NewAbsPath, True) then
            raise Exception.CreateFmt(rsErrorCopyingFile, [OldAbsPath, NewAbsPath]);

          WasMoved := FileExists(NewAbsPath);
          if WasMoved and MustMoveFiles then
          begin
            DeleteFile(OldAbsPath);
          end;

          Inc(Result);
        end;
      end;

      QrySel.Next;
    end;
    QrySel.Close;
  finally
    QrySel.Free;
  end;
end;

procedure TcfgOptions.RefreshCacheSizeDisplay;
var
  SizeBytes: Int64;
begin
  if Assigned(FThumbManager) then
  begin
    SizeBytes := FThumbManager.GetCacheSizeBytes;
    lblThumbnailCache.Caption := Format(rsThumbnailCacheSize, [FormatBytes(SizeBytes)]);
    btnClearThumbnailCache.Enabled := SizeBytes > 0;
  end;
end;

function TcfgOptions.ConfirmAndMigrateMediaPaths(aOldBaseFolder, aNewBaseFolder: String): Integer;
var
  ImgMovedCount, AudMovedCount, VidMovedCount, DocMovedCount, TotalMovedCount: Integer;
  Msg: String;
  Dlg: TTaskDialog;
  MustMoveFiles: Boolean;
begin
  Result := mrCancel;

  Msg := Format(rsPromptMigrateMediaPath, [aOldBaseFolder, aNewBaseFolder]);

  Dlg := TTaskDialog.Create(Self);
  try
    Dlg.Caption := APP_NAME;
    Dlg.Title := rsMediaFolderChanged;
    Dlg.Text := Msg;
    Dlg.CommonButtons := [tcbCancel];
    Dlg.Flags := Dlg.Flags + [tfUseCommandLinks];
    with TTaskDialogButtonItem(Dlg.Buttons.Add) do
    begin
      Caption := rsChangeFolderAndMoveFiles;
      CommandLinkHint := rsHintChangeFolderAndMoveFiles;
      ModalResult := 101;
    end;
    with TTaskDialogButtonItem(Dlg.Buttons.Add) do
    begin
      Caption := rsChangeFolderAndCopyFiles;
      CommandLinkHint := rsHintChangeFolderAndCopyFiles;
      ModalResult := 102;
    end;
    with TTaskDialogButtonItem(Dlg.Buttons.Add) do
    begin
      Caption := rsChangeFolderOnly;
      CommandLinkHint := rsHintChangeFolderOnly;
      ModalResult := 103;
    end;
    if not Dlg.Execute then
      Exit(mrCancel);
    Result := Dlg.ModalResult;
  finally
    Dlg.Free;
  end;

  if not Result = 103 then
  begin
    MustMoveFiles := Result = 101;
    try
      ImgMovedCount := MigrateMediaPaths(TBL_IMAGES, aOldBaseFolder, aNewBaseFolder, MustMoveFiles, False);
      AudMovedCount := MigrateMediaPaths(TBL_AUDIO_LIBRARY, aOldBaseFolder, aNewBaseFolder, MustMoveFiles, False);
      VidMovedCount := MigrateMediaPaths(TBL_VIDEOS, aOldBaseFolder, aNewBaseFolder, MustMoveFiles, False);
      DocMovedCount := MigrateMediaPaths(TBL_DOCUMENTS, aOldBaseFolder, aNewBaseFolder, MustMoveFiles, True);
      TotalMovedCount := ImgMovedCount + AudMovedCount + VidMovedCount + DocMovedCount;
      MsgDlg(rsMediaMigrationCompleted, Format(rsMigratedMediaPaths, [TotalMovedCount]), mtInformation);
    except
      on E: Exception do
      begin
        MsgDlg(rsTitleError, Format(rsMediaMigrationError, [E.Message]), mtWarning);
        Result := mrCancel;
      end;
    end;
  end;
end;

procedure TcfgOptions.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  btnForceMediaMigration.Visible := ActiveUser.IsAdmin;

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

  cbMoveMediaFile.Items.Clear;
  cbMoveMediaFile.Items.Add(rsMoveMediaFiles);
  cbMoveMediaFile.Items.Add(rsCopyMediaFiles);

  cbStartupBackup.Items.Assign(cbCheckUpdates.Items);

  pManageBackups.Enabled := not (ActiveUser.IsVisitor);

  LoadConfig;

  lblShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
  tsShowOutliers.Enabled := tsUseConditionalFormatting.Checked;
  lblWriteDetailedLogs.Enabled := tsWriteLogs.Checked;
  tsWriteDetailedLogs.Enabled := tsWriteLogs.Checked;

  RefreshCacheSizeDisplay;
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
  txtMediaPath.Caption := MinimizeName(xSettings.MediaStorageFolder, txtMediaPath.Canvas, txtMediaPath.Width);
  cbMoveMediaFile.ItemIndex := Ord(xSettings.MoveOriginalFile);
  tsDeleteMediaFile.Checked := xSettings.DeleteMediaFile;
  tsOpenAfterExport.Checked := xSettings.OpenFileAfterExport;

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
  // Notify modules to update row height
  EventBus.Publish(evDefaultRowHeightChanged);
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
  // Notify modules to auto adjust columns or not
  EventBus.Publish(evAutoAdjustColumnsChanged);
end;

procedure TcfgOptions.tsConfirmCancelChange(Sender: TObject);
begin
  xSettings.ConfirmCancel := tsConfirmCancel.Checked;
end;

procedure TcfgOptions.tsDeleteMediaFileChange(Sender: TObject);
begin
  xSettings.DeleteMediaFile := tsDeleteMediaFile.Checked;
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

