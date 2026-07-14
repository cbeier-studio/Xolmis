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
  Buttons, StrUtils, atshapelinebgra, BCPanel, io_core;

type

  { TdlgImportCaptures }

  TdlgImportCaptures = class(TForm)
    barProgress: TProgressBar;
    btnGenerateFiles: TButton;
    btnHelp: TBitBtn;
    cbExistingRecordPolicy: TComboBox;
    cbUnknownTaxa: TComboBox;
    eWeatherFile: TFileNameEdit;
    iButtonsDark: TImageList;
    icoWeatherFile: TImage;
    icoImportFinished: TImage;
    iIcons: TImageList;
    iButtons: TImageList;
    iIconsDark: TImageList;
    imgFinishedDark: TImageList;
    lblWeatherFile: TLabel;
    lblExistingRecordPolicy: TLabel;
    lblGenerateFiles: TLabel;
    lblSubtitleImportFinished: TLabel;
    lblTitleImportFinished: TLabel;
    lblTitleImportSettings: TLabel;
    lblUnknownTaxa: TLabel;
    mProgress: TMemo;
    pWeatherFile: TBCPanel;
    pExistingRecordPolicy: TBCPanel;
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
    pUnknownTaxa: TBCPanel;
    SaveDlg: TSaveDialog;
    SaveLogDlg: TSaveDialog;
    sbCancel: TButton;
    sbClearEffortFile: TSpeedButton;
    sbClearCaptureFile: TSpeedButton;
    sbClearWeatherFile: TSpeedButton;
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbRun: TButton;
    sbClearJournalFile: TSpeedButton;
    sbSaveLog: TBitBtn;
    procedure btnGenerateFilesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure eCaptureFileChange(Sender: TObject);
    procedure eEffortFileChange(Sender: TObject);
    procedure eJournalFileButtonClick(Sender: TObject);
    procedure eJournalFileChange(Sender: TObject);
    procedure eWeatherFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearJournalFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
    procedure sbSaveLogClick(Sender: TObject);
  private
    FImportSettings: TImportOptions;
    procedure AppendLog(const aMsg: String);
    procedure ApplyDarkMode;
    procedure GetImportSettings;
    procedure SetImportSettings;
    procedure UpdateButtons;
    function ValidateFields: Boolean;
  public

  end;

var
  dlgImportCaptures: TdlgImportCaptures;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_themes,
  io_banding_csv,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgImportCaptures }

function HasImportFileSelected(const AFileName: String): Boolean;
var
  S: String;
begin
  S := Trim(AFileName);
  Result := (S <> EmptyStr) and (S <> '.') and (S <> '..');
end;

procedure TdlgImportCaptures.AppendLog(const aMsg: String);
begin
  mProgress.Lines.Append(aMsg);

  //mProgress.SelStart := Length(mProgress.Text);
  //mProgress.SelLength := 0;
  mProgress.CaretPos := Point(0, mProgress.Lines.Count - 1);
end;

procedure TdlgImportCaptures.ApplyDarkMode;
begin
  eJournalFile.Images := iButtonsDark;
  eWeatherFile.Images := iButtonsDark;
  eEffortFile.Images := iButtonsDark;
  eCaptureFile.Images := iButtonsDark;
  sbClearJournalFile.Images := iButtonsDark;
  sbClearEffortFile.Images := iButtonsDark;
  sbClearCaptureFile.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  btnHelp.Images := iButtonsDark;

  icoJournalFile.Images := iIconsDark;
  icoWeatherFile.Images := iIconsDark;
  icoEffortFile.Images := iIconsDark;
  icoCaptureFile.Images := iIconsDark;

  icoImportFinished.Images := imgFinishedDark;

  pJournalFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pJournalFile.Border.Color := ActiveTheme.Border.Default;
  pWeatherFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pWeatherFile.Border.Color := ActiveTheme.Border.Default;
  pEffortFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pEffortFile.Border.Color := ActiveTheme.Border.Default;
  pCaptureFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pCaptureFile.Border.Color := ActiveTheme.Border.Default;
  pExistingRecordPolicy.Background.Color := ActiveTheme.Background.SolidSecondary;
  pExistingRecordPolicy.Border.Color := ActiveTheme.Border.Default;
  pUnknownTaxa.Background.Color := ActiveTheme.Background.SolidSecondary;
  pUnknownTaxa.Border.Color := ActiveTheme.Border.Default;

  pGenerateFiles.Background.Color := ActiveTheme.System.CautionBG;
  pGenerateFiles.Border.Color := ActiveTheme.System.CautionFG;

  lblTitleImportFiles.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleImportSettings.Font.Color := ActiveTheme.Interactive.WindowTitle;
  lblTitleImportFinished.Font.Color := ActiveTheme.Interactive.WindowTitle;
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
      except
        on E: Exception do
          MsgDlg(rsTitleError, Format(rsErrorGeneratingFiles, [E.Message]), mtError);
      end;
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

procedure TdlgImportCaptures.eWeatherFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportCaptures.FormCreate(Sender: TObject);
begin
  FImportSettings.ExistingRecordPolicy := erpUpdateExisting;
  FImportSettings.UnknownTaxonPolicy := utpAsk;
  FImportSettings.ErrorHandling := iehAbort;
end;

procedure TdlgImportCaptures.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    if sbCancel.Caption = rsCaptionCancel then
    begin
      if nbContent.ActivePageComponent = pgImportProgress then
        stopProcess := True
      else
        ModalResult := mrCancel;
    end
    else
      ModalResult := mrClose;
  end;
end;

procedure TdlgImportCaptures.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eWeatherFile.Width := eJournalFile.Width;
  eEffortFile.Width := eJournalFile.Width;
  eCaptureFile.Width := eJournalFile.Width;

  with cbExistingRecordPolicy.Items do
  begin
    Clear;
    Add(rsImportIgnoreExisting);
    Add(rsImportReplaceExisting);
  end;
  with cbUnknownTaxa.Items do
  begin
    Clear;
    //Add(rsImportAddTemporaryTaxon);
    Add(rsImportAskUnknownTaxon);
    Add(rsImportAbortUnknownTaxon);
  end;
  GetImportSettings;
end;

procedure TdlgImportCaptures.GetImportSettings;
begin
  case FImportSettings.ExistingRecordPolicy of
    erpIgnoreExisting:  cbExistingRecordPolicy.ItemIndex := 0;
    erpUpdateExisting:  cbExistingRecordPolicy.ItemIndex := 1;
    //erpAllowDuplicates: cbExistingRecordPolicy.ItemIndex := 2;
  end;
  case FImportSettings.UnknownTaxonPolicy of
    //utpAddCustomTaxon: cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAddTemporaryTaxon);
    utpAsk:     cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAskUnknownTaxon);
    utpAbort:   cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAbortUnknownTaxon);
  end;
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
    3: eWeatherFile.Clear;
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
var
  HadError: Boolean;
begin
  if not ValidateFields then
    Exit;

  HadError := False;

  sbSaveLog.Visible := False;
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

  SetImportSettings;

  try
    if (not stopProcess) and HasImportFileSelected(eJournalFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingJournal);
      ImportBandingJournalV1(eJournalFile.FileName, FImportSettings, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eWeatherFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingWeatherLog);
      ImportBandingWeatherLogV1(eWeatherFile.FileName, FImportSettings, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eEffortFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingEffort);
      ImportBandingEffortV1(eEffortFile.FileName, FImportSettings, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eCaptureFile.FileName) then
    begin
      AppendLog(rsProgressImportCaptures);
      ImportBandingDataV1(eCaptureFile.FileName, FImportSettings, barProgress);
    end;
  except
    on E: Exception do
    begin
      AppendLog(Format(rsErrorImporting, [E.Message]));
      lblTitleImportFinished.Caption := rsImportCanceled;
      lblSubtitleImportFinished.Caption := rsImportCanceledByError;
      icoImportFinished.ImageIndex := 1;
      sbSaveLog.Visible := True;
      sbRetry.Visible := True;
      sbCancel.Visible := False;
      sbClose.Visible := True;
      barProgress.Visible := False;
    end;
  end;

  if stopProcess then
  begin
    AppendLog(rsImportCanceledByUser);
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
    sbSaveLog.Visible := True;
    sbRetry.Visible := True;
    barProgress.Visible := False;
  end
  else
  begin
    AppendLog(rsSuccessfulImport);
    DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
    sbSaveLog.Visible := True;
    sbRetry.Visible := True;
    barProgress.Visible := False;
  end;

  sbCancel.Visible := False;
  sbClose.Visible := True;
  //sbRun.Visible := True;
  UpdateButtons;
end;

procedure TdlgImportCaptures.sbSaveLogClick(Sender: TObject);
begin
  if SaveLogDlg.Execute then
  begin
    mProgress.Lines.SaveToFile(SaveLogDlg.FileName);
    if xSettings.OpenFileAfterExport then
      OpenDocument(SaveLogDlg.FileName);
  end;
end;

procedure TdlgImportCaptures.SetImportSettings;
begin
  case cbExistingRecordPolicy.ItemIndex of
    0: FImportSettings.ExistingRecordPolicy := erpIgnoreExisting;
    1: FImportSettings.ExistingRecordPolicy := erpUpdateExisting;
    //2: FImportSettings.ExistingRecordPolicy := erpAllowDuplicates;
  end;
  case cbUnknownTaxa.ItemIndex of
    //0: FImportSettings.UnknownTaxonPolicy := utpAddCustomTaxon;
    0: FImportSettings.UnknownTaxonPolicy := utpAsk;
    1: FImportSettings.UnknownTaxonPolicy := utpAbort;
    //3: FImportSettings.UnknownTaxonPolicy := utpIgnore;
  end;
end;

procedure TdlgImportCaptures.UpdateButtons;
begin
  sbRun.Enabled := HasImportFileSelected(eJournalFile.FileName) or
                   HasImportFileSelected(eWeatherFile.FileName) or
                   HasImportFileSelected(eEffortFile.FileName) or
                   HasImportFileSelected(eCaptureFile.FileName);
end;

function TdlgImportCaptures.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  if HasImportFileSelected(eJournalFile.FileName) then
    if not FileExists(eJournalFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eJournalFile.FileName]));
  if HasImportFileSelected(eWeatherFile.FileName) then
    if not FileExists(eWeatherFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eWeatherFile.FileName]));
  if HasImportFileSelected(eEffortFile.FileName) then
    if not FileExists(eEffortFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eEffortFile.FileName]));
  if HasImportFileSelected(eCaptureFile.FileName) then
    if not FileExists(eCaptureFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eCaptureFile.FileName]));

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

