{ Xolmis Import Nest Data dialog

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

unit udlg_importnests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLIntf, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, ComCtrls, StdCtrls,
  Buttons, StrUtils, atshapelinebgra, BCPanel, io_core;

type

  { TdlgImportNests }

  TdlgImportNests = class(TForm)
    barProgress: TProgressBar;
    btnGenerateFiles: TButton;
    btnHelp: TBitBtn;
    cbExistingRecordPolicy: TComboBox;
    cbUnknownTaxa: TComboBox;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    iIcons: TImageList;
    iIconsDark: TImageList;
    imgFinishedDark: TImageList;
    lblExistingRecordPolicy: TLabel;
    lblGenerateFiles: TLabel;
    lblSubtitleImportFinished: TLabel;
    lblTitleImportFinished: TLabel;
    lblTitleImportSettings: TLabel;
    lblUnknownTaxa: TLabel;
    mProgress: TMemo;
    pExistingRecordPolicy: TBCPanel;
    pGenerateFiles: TBCPanel;
    imgFinished: TImageList;
    eEggFile: TFileNameEdit;
    eRevisionFile: TFileNameEdit;
    eNestFile: TFileNameEdit;
    icoEggFile: TImage;
    icoRevisionFile: TImage;
    icoNestFile: TImage;
    lblEggFile: TLabel;
    lblRevisionFile: TLabel;
    lblNestFile: TLabel;
    lblSubtitleImportFiles: TLabel;
    lblTitleImportFiles: TLabel;
    lineBottom: TShapeLineBGRA;
    nbContent: TNotebook;
    pgImportProgress: TPage;
    pgImportFiles: TPage;
    pBottom: TPanel;
    pEggFile: TBCPanel;
    pContentFiles: TPanel;
    pRevisionFile: TBCPanel;
    pNestFile: TBCPanel;
    pUnknownTaxa: TBCPanel;
    SaveDlg: TSaveDialog;
    SaveLogDlg: TSaveDialog;
    sbCancel: TButton;
    sbClearRevisionFile: TSpeedButton;
    sbClearEggFile: TSpeedButton;
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbSaveLog: TBitBtn;
    sbRun: TButton;
    sbClearNestFile: TSpeedButton;
    procedure btnGenerateFilesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure eEggFileChange(Sender: TObject);
    procedure eRevisionFileChange(Sender: TObject);
    procedure eNestFileButtonClick(Sender: TObject);
    procedure eNestFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearNestFileClick(Sender: TObject);
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
  dlgImportNests: TdlgImportNests;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_themes,
  io_nesting_csv,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgImportNests }

function HasImportFileSelected(const AFileName: String): Boolean;
var
  S: String;
begin
  S := Trim(AFileName);
  Result := (S <> EmptyStr) and (S <> '.') and (S <> '..');
end;

procedure TdlgImportNests.AppendLog(const aMsg: String);
begin
  mProgress.Lines.Append(aMsg);

  //mProgress.SelStart := Length(mProgress.Text);
  //mProgress.SelLength := 0;
  mProgress.CaretPos := Point(0, mProgress.Lines.Count - 1);
end;

procedure TdlgImportNests.ApplyDarkMode;
begin
  eNestFile.Images := iButtonsDark;
  eRevisionFile.Images := iButtonsDark;
  eEggFile.Images := iButtonsDark;
  sbClearNestFile.Images := iButtonsDark;
  sbClearRevisionFile.Images := iButtonsDark;
  sbClearEggFile.Images := iButtonsDark;
  sbSaveLog.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;
  btnHelp.Images := iButtonsDark;

  icoNestFile.Images := iIconsDark;
  icoRevisionFile.Images := iIconsDark;
  icoEggFile.Images := iIconsDark;

  icoImportFinished.Images := imgFinishedDark;

  pNestFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pNestFile.Border.Color := ActiveTheme.Border.Default;
  pRevisionFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pRevisionFile.Border.Color := ActiveTheme.Border.Default;
  pEggFile.Background.Color := ActiveTheme.Background.SolidSecondary;
  pEggFile.Border.Color := ActiveTheme.Border.Default;
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

procedure TdlgImportNests.btnGenerateFilesClick(Sender: TObject);
var
  Csv: TStrings;
  nestsFilename, revisionsFilename, eggsFilename: String;
begin
  SaveDlg.InitialDir := xSettings.LastPathUsed;
  if SaveDlg.Execute then
  begin
    nestsFilename := SaveDlg.FileName;
    revisionsFilename := StringReplace(SaveDlg.FileName, '.csv', '_revisions.csv', []);
    eggsFilename := StringReplace(SaveDlg.FileName, '.csv', '_eggs.csv', []);

    Csv := TStringList.Create;
    try
      try
        Csv.Add(NEST_SCHEMA);
        Csv.SaveToFile(nestsFilename);
        if xSettings.OpenFileAfterExport then
          OpenDocument(nestsFilename);

        Csv.Clear;
        Csv.Add(NEST_REVISION_SCHEMA);
        Csv.SaveToFile(revisionsFilename);
        if xSettings.OpenFileAfterExport then
          OpenDocument(revisionsFilename);

        Csv.Clear;
        Csv.Add(EGG_SCHEMA);
        Csv.SaveToFile(eggsFilename);
        if xSettings.OpenFileAfterExport then
          OpenDocument(eggsFilename);
      except
        on E: Exception do
          MsgDlg(rsTitleError, Format(rsErrorGeneratingFiles, [E.Message]), mtError);
      end;
    finally
      FreeAndNil(Csv);
    end;

  end;
end;

procedure TdlgImportNests.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_IMPORTING_NESTING_DATA);
end;

procedure TdlgImportNests.eEggFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportNests.eNestFileButtonClick(Sender: TObject);
begin
  if Sender is TFileNameEdit then
    TFileNameEdit(Sender).InitialDir := xSettings.LastPathUsed;
end;

procedure TdlgImportNests.eNestFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportNests.eRevisionFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportNests.FormCreate(Sender: TObject);
begin
  FImportSettings.ExistingRecordPolicy := erpUpdateExisting;
  FImportSettings.UnknownTaxonPolicy := utpAsk;
  FImportSettings.ErrorHandling := iehAbort;
end;

procedure TdlgImportNests.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TdlgImportNests.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eNestFile.Width := eRevisionFile.Width;
  eEggFile.Width := eRevisionFile.Width;

  with cbExistingRecordPolicy.Items do
  begin
    Clear;
    Add(rsImportIgnoreExisting);
    Add(rsImportReplaceExisting);
  end;
  with cbUnknownTaxa.Items do
  begin
    Clear;
    Add(rsImportAddTemporaryTaxon);
    Add(rsImportAskUnknownTaxon);
    Add(rsImportAbortUnknownTaxon);
  end;
  GetImportSettings;
end;

procedure TdlgImportNests.GetImportSettings;
begin
  case FImportSettings.ExistingRecordPolicy of
    erpIgnoreExisting:  cbExistingRecordPolicy.ItemIndex := 0;
    erpUpdateExisting:  cbExistingRecordPolicy.ItemIndex := 1;
    //erpAllowDuplicates: cbExistingRecordPolicy.ItemIndex := 2;
  end;
  case FImportSettings.UnknownTaxonPolicy of
    utpAddCustomTaxon: cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAddTemporaryTaxon);
    utpAsk:     cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAskUnknownTaxon);
    utpAbort:   cbUnknownTaxa.ItemIndex := cbUnknownTaxa.Items.IndexOf(rsImportAbortUnknownTaxon);
  end;
end;

procedure TdlgImportNests.sbCancelClick(Sender: TObject);
begin
  stopProcess := True;

  sbCancel.Visible := False;
  sbClose.Visible := True;
  sbRun.Visible := True;
  UpdateButtons;
end;

procedure TdlgImportNests.sbClearNestFileClick(Sender: TObject);
begin
  case TControl(Sender).Tag of
    0: eNestFile.Clear;
    1: eRevisionFile.Clear;
    2: eEggFile.Clear;
  end;

  UpdateButtons;
end;

procedure TdlgImportNests.sbRetryClick(Sender: TObject);
begin
  nbContent.PageIndex := 0;

  sbCancel.Visible := False;
  sbClose.Visible := True;
  sbRun.Visible := True;
  UpdateButtons;
end;

procedure TdlgImportNests.sbRunClick(Sender: TObject);
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
    if (not stopProcess) and HasImportFileSelected(eNestFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingJournal);
      ImportNestDataV1(eNestFile.FileName, FImportSettings, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eRevisionFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingEffort);
      ImportNestRevisionsV1(eRevisionFile.FileName, FImportSettings, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eEggFile.FileName) then
    begin
      AppendLog(rsProgressImportCaptures);
      ImportEggDataV1(eEggFile.FileName, FImportSettings, barProgress);
    end;
  except
    on E: Exception do
    begin
      HadError := True;
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

  if HadError then
  begin
    // UI state already configured in exception handler.
  end
  else
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

procedure TdlgImportNests.sbSaveLogClick(Sender: TObject);
begin
  if SaveLogDlg.Execute then
  begin
    mProgress.Lines.SaveToFile(SaveLogDlg.FileName);
    if xSettings.OpenFileAfterExport then
      OpenDocument(SaveLogDlg.FileName);
  end;
end;

procedure TdlgImportNests.SetImportSettings;
begin
  case cbExistingRecordPolicy.ItemIndex of
    0: FImportSettings.ExistingRecordPolicy := erpIgnoreExisting;
    1: FImportSettings.ExistingRecordPolicy := erpUpdateExisting;
    //2: FImportSettings.ExistingRecordPolicy := erpAllowDuplicates;
  end;
  case cbUnknownTaxa.ItemIndex of
    0: FImportSettings.UnknownTaxonPolicy := utpAddCustomTaxon;
    1: FImportSettings.UnknownTaxonPolicy := utpAsk;
    2: FImportSettings.UnknownTaxonPolicy := utpAbort;
    //3: FImportSettings.UnknownTaxonPolicy := utpIgnore;
  end;
end;

procedure TdlgImportNests.UpdateButtons;
begin
  sbRun.Enabled := HasImportFileSelected(eNestFile.FileName) or
                   HasImportFileSelected(eRevisionFile.FileName) or
                   HasImportFileSelected(eEggFile.FileName);
end;

function TdlgImportNests.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  if HasImportFileSelected(eNestFile.FileName) then
    if not FileExists(eNestFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eNestFile.FileName]));
  if HasImportFileSelected(eRevisionFile.FileName) then
    if not FileExists(eRevisionFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eRevisionFile.FileName]));
  if HasImportFileSelected(eEggFile.FileName) then
    if not FileExists(eEggFile.FileName) then
      Msgs.Add(Format(rsErrorFileNotFound, [eEggFile.FileName]));

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

