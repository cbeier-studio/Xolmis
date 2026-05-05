{ Xolmis Import Banding Data dialog

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

unit udlg_importcaptures;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLIntf, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, ComCtrls, StdCtrls,
  Buttons, StrUtils, atshapelinebgra, BCPanel;

type

  { TdlgImportCaptures }

  TdlgImportCaptures = class(TForm)
    barProgress: TProgressBar;
    btnGenerateFiles: TButton;
    btnHelp: TBitBtn;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    iIcons: TImageList;
    iButtons: TImageList;
    iIconsDark: TImageList;
    imgFinishedDark: TImageList;
    lblGenerateFiles: TLabel;
    lblSubtitleImportFinished: TLabel;
    lblTitleImportFinished: TLabel;
    mProgress: TMemo;
    pGenerateFiles: TBCPanel;
    imgFinished: TImageList;
    eCaptureFile: TFileNameEdit;
    eEffortFile: TFileNameEdit;
    eJournalFile: TFileNameEdit;
    icoCaptureFile: TImage;
    icoEffortFile: TImage;
    icoJournalFile: TImage;
    lblCaptureFile: TLabel;
    lblEffortFile: TLabel;
    lblJournalFile: TLabel;
    lblSubtitleImportFiles: TLabel;
    lblTitleImportFiles: TLabel;
    lineBottom: TShapeLineBGRA;
    nbContent: TNotebook;
    pgImportProgress: TPage;
    pgImportFiles: TPage;
    pBottom: TPanel;
    pCaptureFile: TBCPanel;
    pContentFiles: TPanel;
    pEffortFile: TBCPanel;
    pJournalFile: TBCPanel;
    SaveDlg: TSaveDialog;
    sbCancel: TButton;
    sbClearEffortFile: TSpeedButton;
    sbClearCaptureFile: TSpeedButton;
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbRun: TButton;
    sbClearJournalFile: TSpeedButton;
    procedure btnGenerateFilesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure eCaptureFileChange(Sender: TObject);
    procedure eEffortFileChange(Sender: TObject);
    procedure eJournalFileButtonClick(Sender: TObject);
    procedure eJournalFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearJournalFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    procedure ApplyDarkMode;
    procedure UpdateButtons;
  public

  end;

var
  dlgImportCaptures: TdlgImportCaptures;

implementation

uses
  utils_locale, utils_global, io_core, io_banding_csv, utils_themes, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgImportCaptures }

function HasImportFileSelected(const AFileName: String): Boolean;
var
  S: String;
begin
  S := Trim(AFileName);
  Result := (S <> EmptyStr) and (S <> '.') and (S <> '..');
end;

procedure TdlgImportCaptures.ApplyDarkMode;
begin
  eJournalFile.Images := iButtonsDark;
  eEffortFile.Images := iButtonsDark;
  eCaptureFile.Images := iButtonsDark;
  sbClearJournalFile.Images := iButtonsDark;
  sbClearEffortFile.Images := iButtonsDark;
  sbClearCaptureFile.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  btnHelp.Images := iButtonsDark;

  icoJournalFile.Images := iIconsDark;
  icoEffortFile.Images := iIconsDark;
  icoCaptureFile.Images := iIconsDark;

  icoImportFinished.Images := imgFinishedDark;

  pJournalFile.Background.Color := clSolidBGSecondaryDark;
  pJournalFile.Border.Color := clSystemSolidNeutralFGDark;
  pEffortFile.Background.Color := clSolidBGSecondaryDark;
  pEffortFile.Border.Color := clSystemSolidNeutralFGDark;
  pCaptureFile.Background.Color := clSolidBGSecondaryDark;
  pCaptureFile.Border.Color := clSystemSolidNeutralFGDark;

  pGenerateFiles.Background.Color := clSystemCautionBGDark;
  pGenerateFiles.Border.Color := clSystemCautionFGDark;

  lblTitleImportFiles.Font.Color := clVioletFG1Dark;
  lblTitleImportFinished.Font.Color := clVioletFG1Dark;
end;

procedure TdlgImportCaptures.btnGenerateFilesClick(Sender: TObject);
var
  Csv: TStrings;
  journalFilename, weatherFilename, effortFilename, capturesFilename: String;
begin
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  if SaveDlg.Execute then
  begin
    journalFilename := StringReplace(SaveDlg.FileName, '.csv', '_journal.csv', []);
    weatherFilename := StringReplace(SaveDlg.FileName, '.csv', '_weather.csv', []);
    effortFilename := StringReplace(SaveDlg.FileName, '.csv', '_effort.csv', []);
    capturesFilename := SaveDlg.FileName;

    Csv := TStringList.Create;
    try
      Csv.Add(BANDING_JOURNAL_SCHEMA);
      Csv.SaveToFile(journalFilename);
      if xSettings.OpenFileAfterExport then
        OpenDocument(journalFilename);

      Csv.Clear;
      Csv.Add(WEATHER_LOG_SCHEMA);
      Csv.SaveToFile(weatherFilename);
      if xSettings.OpenFileAfterExport then
        OpenDocument(weatherFilename);

      Csv.Clear;
      Csv.Add(NET_EFFORT_SCHEMA);
      Csv.SaveToFile(effortFilename);
      if xSettings.OpenFileAfterExport then
        OpenDocument(effortFilename);

      Csv.Clear;
      Csv.Add(BANDING_SCHEMA);
      Csv.SaveToFile(capturesFilename);
      if xSettings.OpenFileAfterExport then
        OpenDocument(capturesFilename);
    finally
      FreeAndNil(Csv);
    end;
  end;
end;

procedure TdlgImportCaptures.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_IMPORTING_BANDING_DATA);
end;

procedure TdlgImportCaptures.eCaptureFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportCaptures.eEffortFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportCaptures.eJournalFileButtonClick(Sender: TObject);
begin
  if Sender is TFileNameEdit then
    TFileNameEdit(Sender).InitialDir := xSettings.LastPathUsed;
end;

procedure TdlgImportCaptures.eJournalFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportCaptures.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eEffortFile.Width := eJournalFile.Width;
  eCaptureFile.Width := eJournalFile.Width;
end;

procedure TdlgImportCaptures.sbCancelClick(Sender: TObject);
begin
  stopProcess := True;

  sbCancel.Visible := False;
  sbClose.Visible := True;
  sbRun.Visible := True;
  UpdateButtons;
end;

procedure TdlgImportCaptures.sbClearJournalFileClick(Sender: TObject);
begin
  case TControl(Sender).Tag of
    0: eJournalFile.Clear;
    1: eEffortFile.Clear;
    2: eCaptureFile.Clear;
  end;

  UpdateButtons;
end;

procedure TdlgImportCaptures.sbRetryClick(Sender: TObject);
begin
  nbContent.PageIndex := 0;

  sbCancel.Visible := False;
  sbClose.Visible := True;
  sbRun.Visible := True;
  UpdateButtons;
end;

procedure TdlgImportCaptures.sbRunClick(Sender: TObject);
begin
  { #todo : Validate fields }

  sbRetry.Visible := False;
  barProgress.Visible := True;
  icoImportFinished.ImageIndex := 2;
  lblTitleImportFinished.Caption := rsImportingFiles;
  lblSubtitleImportFinished.Caption := rsPleaseWaitWhileImporting;

  stopProcess := False;
  mProgress.Lines.Clear;

  nbContent.PageIndex := 1;

  sbCancel.Visible := True;
  sbClose.Visible := False;
  sbRun.Visible := False;

  try
    if (not stopProcess) and HasImportFileSelected(eJournalFile.FileName) then
    begin
      mProgress.Lines.Add(rsProgressImportBandingJournal);
      ImportBandingJournalV1(eJournalFile.FileName, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eEffortFile.FileName) then
    begin
      mProgress.Lines.Add(rsProgressImportBandingEffort);
      ImportBandingEffortV1(eEffortFile.FileName, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eCaptureFile.FileName) then
    begin
      mProgress.Lines.Add(rsProgressImportCaptures);
      ImportBandingDataV1(eCaptureFile.FileName, barProgress);
    end;
  except
    on E: Exception do
    begin
      mProgress.Lines.Add('Error: ' + E.Message);
      lblTitleImportFinished.Caption := rsImportCanceled;
      lblSubtitleImportFinished.Caption := rsImportCanceledByError;
      icoImportFinished.ImageIndex := 1;
      sbRetry.Visible := True;
      barProgress.Visible := False;
    end;
  end;

  if stopProcess then
  begin
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
    sbRetry.Visible := True;
    barProgress.Visible := False;
  end
  else
  begin
    DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
    sbRetry.Visible := True;
    barProgress.Visible := False;
  end;

end;

procedure TdlgImportCaptures.UpdateButtons;
begin
  sbRun.Enabled := HasImportFileSelected(eJournalFile.FileName) or
                   HasImportFileSelected(eEffortFile.FileName) or
                   HasImportFileSelected(eCaptureFile.FileName);
end;

end.

