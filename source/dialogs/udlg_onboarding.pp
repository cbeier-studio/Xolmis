{ Xolmis First Execution Onboarding dialog

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_onboarding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, EditBtn,
  ATShapeLineBGRA, BCPanel, ColorSpeedButton, BCButton, ATLinkLabel;

type

  { TdlgOnboarding }

  TdlgOnboarding = class(TForm)
    btnBackBackup: TBitBtn;
    btnNextDatabase: TBitBtn;
    btnSavePreferences: TBitBtn;
    btnSaveBackup: TBitBtn;
    btnBackPreferences: TBitBtn;
    cbBackupWhenClosing: TComboBox;
    cbCheckUpdates: TComboBox;
    cbClearDeleted: TComboBox;
    cbStartPage: TComboBox;
    dsConn: TDataSource;
    eBackupPath: TDirectoryEdit;
    icoBackupPath: TImage;
    icoBackupWhenClosing: TImage;
    icoCheckUpdates: TImage;
    icoClearDeleted: TImage;
    icoStartPage: TImage;
    icoFinished: TImage;
    imgWelcome: TImage;
    iFinished: TImageList;
    iFinishedDark: TImageList;
    lblBackupInstruction: TLabel;
    lblBackupPath: TLabel;
    lblCheckUpdates: TLabel;
    lblClearDeleted: TLabel;
    lblPreferencesInstruction: TLabel;
    lblBackupWhenClosing: TLabel;
    lblStartPage: TLabel;
    lblTitleBackup: TLabel;
    lblTitlePreferences: TLabel;
    lblFinishedInstruction: TLabel;
    lblTitleFinished: TLabel;
    pBackupPath: TBCPanel;
    pCheckUpdates: TBCPanel;
    pClearDeleted: TBCPanel;
    pgPreferences: TPage;
    pgBackup: TPage;
    pBackupWhenClosing: TBCPanel;
    pStartPage: TBCPanel;
    pgFinished: TPage;
    pNewDatabase: TBCPanel;
    pOpenDatabase: TBCPanel;
    btnStart: TBitBtn;
    btnHelp: TBitBtn;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoNewDatabase: TImage;
    arrowNewDatabase: TImage;
    icoOpenDatabase: TImage;
    arrowOpenDatabase: TImage;
    lblNewDatabase: TLabel;
    sbClose: TButton;
    txtNewDatabase: TLabel;
    lblOpenDatabase: TLabel;
    txtOpenDatabase: TLabel;
    lblTitleDatabase: TLabel;
    lblWelcomeInstruction: TLabel;
    lblTitleWelcome: TLabel;
    lblDatabaseInstruction: TLabel;
    lineBottom: TShapeLineBGRA;
    nbPages: TNotebook;
    pgDatabase: TPage;
    pgWelcome: TPage;
    pBottom: TPanel;
    sbCancel: TButton;
    procedure btnBackBackupClick(Sender: TObject);
    procedure btnBackMediaClick(Sender: TObject);
    procedure btnBackPreferencesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnNextDatabaseClick(Sender: TObject);
    procedure btnSaveBackupClick(Sender: TObject);
    procedure btnSavePreferencesClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pNewDatabaseClick(Sender: TObject);
    procedure pNewDatabaseMouseEnter(Sender: TObject);
    procedure pNewDatabaseMouseLeave(Sender: TObject);
    procedure pOpenDatabaseClick(Sender: TObject);
    procedure pOpenDatabaseMouseEnter(Sender: TObject);
    procedure pOpenDatabaseMouseLeave(Sender: TObject);
  private
    FConnectionName: String;
    FNewDB: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  dlgOnboarding: TdlgOnboarding;

implementation

uses
  utils_global, utils_locale, utils_themes, utils_dialogs, data_types, uDarkStyleParams,
  udm_main, udlg_newdatabase, uedt_database;

{$R *.lfm}

{ TdlgOnboarding }

procedure TdlgOnboarding.ApplyDarkMode;
begin
  btnHelp.Images := iButtonsDark;
  btnStart.Images := iButtonsDark;
  btnSaveBackup.Images := iButtonsDark;
  btnSavePreferences.Images := iButtonsDark;

  imgWelcome.Images := iFinishedDark;
  icoNewDatabase.Images := iButtonsDark;
  icoOpenDatabase.Images := iButtonsDark;
  arrowNewDatabase.Images := iButtonsDark;
  arrowOpenDatabase.Images := iButtonsDark;
  icoBackupPath.Images := iButtonsDark;
  icoBackupWhenClosing.Images := iButtonsDark;
  icoStartPage.Images := iButtonsDark;
  icoClearDeleted.Images := iButtonsDark;
  icoCheckUpdates.Images := iButtonsDark;
  icoFinished.Images := iFinishedDark;

  eBackupPath.Images := DMM.iEditsDark;

  pNewDatabase.Background.Color := clSolidBGSecondaryDark;
  pNewDatabase.Border.Color := clSystemSolidNeutralFGDark;
  pOpenDatabase.Background.Color := clSolidBGSecondaryDark;
  pOpenDatabase.Border.Color := clSystemSolidNeutralFGDark;
  pBackupPath.Background.Color := clSolidBGSecondaryDark;
  pBackupPath.Border.Color := clSystemSolidNeutralFGDark;
  pBackupWhenClosing.Background.Color := clSolidBGSecondaryDark;
  pBackupWhenClosing.Border.Color := clSystemSolidNeutralFGDark;
  pStartPage.Background.Color := clSolidBGSecondaryDark;
  pStartPage.Border.Color := clSystemSolidNeutralFGDark;
  pClearDeleted.Background.Color := clSolidBGSecondaryDark;
  pClearDeleted.Border.Color := clSystemSolidNeutralFGDark;
  pCheckUpdates.Background.Color := clSolidBGSecondaryDark;
  pCheckUpdates.Border.Color := clSystemSolidNeutralFGDark;

  lblTitleWelcome.Font.Color := clVioletFG1Dark;
  lblTitleDatabase.Font.Color := clVioletFG1Dark;
  lblTitleBackup.Font.Color := clVioletFG1Dark;
  lblTitlePreferences.Font.Color := clVioletFG1Dark;
  lblTitleFinished.Font.Color := clVioletFG1Dark;
end;

procedure TdlgOnboarding.btnBackBackupClick(Sender: TObject);
begin
  nbPages.PageIndex := pgDatabase.PageIndex;
end;

procedure TdlgOnboarding.btnBackMediaClick(Sender: TObject);
begin
  nbPages.PageIndex := pgDatabase.PageIndex;
  btnNextDatabase.Visible := FConnectionName <> EmptyStr;
end;

procedure TdlgOnboarding.btnBackPreferencesClick(Sender: TObject);
begin
  nbPages.PageIndex := pgBackup.PageIndex;
end;

procedure TdlgOnboarding.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_INSTALLING, 'first-steps');
end;

procedure TdlgOnboarding.btnNextDatabaseClick(Sender: TObject);
begin
  nbPages.PageIndex := pgBackup.PageIndex;
end;

procedure TdlgOnboarding.btnSaveBackupClick(Sender: TObject);
begin
  xSettings.BackupFolder := eBackupPath.Directory;
  xSettings.AutomaticBackup := cbBackupWhenClosing.ItemIndex;
  xSettings.SaveToFile;

  nbPages.PageIndex := pgPreferences.PageIndex;
end;

procedure TdlgOnboarding.btnSavePreferencesClick(Sender: TObject);
begin
  xSettings.StartPage := cbStartPage.ItemIndex;
  xSettings.ClearDeletedPeriod := cbClearDeleted.ItemIndex;
  xSettings.AutoUpdates := cbCheckUpdates.ItemIndex;
  xSettings.SaveToFile;

  if FNewDB then
  begin
    //lblFinishedInstruction.Caption := ;
  end
  else
  begin
    //lblFinishedInstruction.Caption := ;
  end;

  nbPages.PageIndex := pgFinished.PageIndex;

  sbCancel.Visible := False;
  sbClose.Visible := True;
end;

procedure TdlgOnboarding.btnStartClick(Sender: TObject);
begin
  nbPages.PageIndex := pgDatabase.PageIndex;
end;

procedure TdlgOnboarding.FormDestroy(Sender: TObject);
begin
  if dsConn.DataSet.Active then
    dsConn.DataSet.Close;
end;

procedure TdlgOnboarding.FormShow(Sender: TObject);
begin
  FNewDB := False;

  // Translate lists
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

  cbBackupWhenClosing.Items.Assign(cbCheckUpdates.Items);

  // Load settings
  eBackupPath.Text := xSettings.BackupFolder;
  cbBackupWhenClosing.ItemIndex := xSettings.AutomaticBackup;

  cbStartPage.ItemIndex := xSettings.StartPage;
  cbClearDeleted.ItemIndex := xSettings.ClearDeletedPeriod;
  cbCheckUpdates.ItemIndex := xSettings.AutoUpdates;
end;

procedure TdlgOnboarding.pNewDatabaseClick(Sender: TObject);
var
  Conn: TDBParams;
begin
  // Paint panel clicked
  pNewDatabase.Border.Color := clVioletFGLight;
  pNewDatabase.Border.Width := 2;

  // Open new database dialog
  LogEvent(leaOpen, 'New database');
  Application.CreateForm(TdlgNewDatabase, dlgNewDatabase);
  with dlgNewDatabase do
  try
    if ShowModal = mrOK then
    begin
      // Test connection
      Conn.Name := ConnectionName;
      Conn.LoadParams;

      if Conn.TestConnection then
      begin
        //MsgDlg(rsTitleConnectionTest, rsSuccessfulConnectionTest, mtInformation)
        FConnectionName := ConnectionName;
        FNewDB := True;
        nbPages.PageIndex := pgBackup.PageIndex;
      end
      else
        MsgDlg(rsTitleConnectionTest, rsErrorConnectingDatabase, mtError);
    end;
  finally
    FreeAndNil(dlgNewDatabase);
    LogEvent(leaClose, 'New database');
  end;

  // Paint panel normal
  if IsDarkModeEnabled then
    pNewDatabase.Border.Color := clSolidBGTertiaryDark
  else
    pNewDatabase.Border.Color := $00D1D1D1;
  pNewDatabase.Border.Width := 1;
end;

procedure TdlgOnboarding.pNewDatabaseMouseEnter(Sender: TObject);
begin
  if IsDarkModeEnabled then
    pNewDatabase.Background.Color := clVioletBG1Dark
  else
    pNewDatabase.Background.Color := $00E0C0C0;
end;

procedure TdlgOnboarding.pNewDatabaseMouseLeave(Sender: TObject);
begin
  if IsDarkModeEnabled then
    pNewDatabase.Background.Color := clCardBGSecondaryDark
  else
    pNewDatabase.Background.Color := clCardBGSecondaryLight;
end;

procedure TdlgOnboarding.pOpenDatabaseClick(Sender: TObject);
var
  Conn: TDBParams;
begin
  // Paint panel clicked
  pOpenDatabase.Border.Color := clVioletFGLight;
  pOpenDatabase.Border.Width := 2;

  // Open new connection dialog
  if not dsConn.DataSet.Active then
    dsConn.DataSet.Open;

  edtDatabase := TedtDatabase.Create(Application);
  with edtDatabase do
  try
    dsConn.DataSet.Append;
    dsConn.DataSet.FieldByName('database_type').AsInteger := 0;
    if ShowModal = mrOk then
    begin
      if not FileExists(dsConn.DataSet.FieldByName('database_name').AsString) then
      begin
        MsgDlg(rsTitleCreateDatabase, rsUseNewDatabaseOption, mtWarning);
        dsConn.DataSet.Cancel;
      end
      else
        dsConn.DataSet.Post;

      // Test connection
      Conn.Name := ConnectionName;
      Conn.LoadParams;

      if Conn.TestConnection then
      begin
        //MsgDlg(rsTitleConnectionTest, rsSuccessfulConnectionTest, mtInformation)
        FConnectionName := ConnectionName;
        FNewDB := False;
        nbPages.PageIndex := pgBackup.PageIndex;
      end
      else
        MsgDlg(rsTitleConnectionTest, rsErrorConnectingDatabase, mtError);
    end
    else
      dsConn.DataSet.Cancel;
  finally
    FreeAndNil(edtDatabase);
  end;

  // Paint panel normal
  if IsDarkModeEnabled then
    pOpenDatabase.Border.Color := clSolidBGTertiaryDark
  else
    pOpenDatabase.Border.Color := $00D1D1D1;
  pOpenDatabase.Border.Width := 1;
end;

procedure TdlgOnboarding.pOpenDatabaseMouseEnter(Sender: TObject);
begin
  if IsDarkModeEnabled then
    pOpenDatabase.Background.Color := clVioletBG1Dark
  else
    pOpenDatabase.Background.Color := $00E0C0C0;
end;

procedure TdlgOnboarding.pOpenDatabaseMouseLeave(Sender: TObject);
begin
  if IsDarkModeEnabled then
    pOpenDatabase.Background.Color := clCardBGSecondaryDark
  else
    pOpenDatabase.Background.Color := clCardBGSecondaryLight;
end;

end.

