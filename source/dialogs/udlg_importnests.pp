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
  Buttons, StrUtils, atshapelinebgra, BCPanel;

type

  { TdlgImportNests }

  TdlgImportNests = class(TForm)
    barProgress: TProgressBar;
    btnGenerateFiles: TButton;
    btnHelp: TBitBtn;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    icoImportFinished: TImage;
    iIcons: TImageList;
    iIconsDark: TImageList;
    imgFinishedDark: TImageList;
    lblGenerateFiles: TLabel;
    lblSubtitleImportFinished: TLabel;
    lblTitleImportFinished: TLabel;
    mProgress: TMemo;
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
    SaveDlg: TSaveDialog;
    sbCancel: TButton;
    sbClearRevisionFile: TSpeedButton;
    sbClearEggFile: TSpeedButton;
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbRun: TButton;
    sbClearNestFile: TSpeedButton;
    procedure btnGenerateFilesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure eEggFileChange(Sender: TObject);
    procedure eRevisionFileChange(Sender: TObject);
    procedure eNestFileButtonClick(Sender: TObject);
    procedure eNestFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearNestFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    procedure AppendLog(const aMsg: String);
    procedure ApplyDarkMode;
    procedure UpdateButtons;
  public

  end;

var
  dlgImportNests: TdlgImportNests;

implementation

uses
  utils_locale, utils_global, io_core, io_nesting_csv, utils_themes, udm_main, uDarkStyleParams;

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
  sbRetry.Images := iButtonsDark;
  btnHelp.Images := iButtonsDark;

  icoNestFile.Images := iIconsDark;
  icoRevisionFile.Images := iIconsDark;
  icoEggFile.Images := iIconsDark;

  icoImportFinished.Images := imgFinishedDark;

  pNestFile.Background.Color := clSolidBGSecondaryDark;
  pNestFile.Border.Color := clSystemSolidNeutralFGDark;
  pRevisionFile.Background.Color := clSolidBGSecondaryDark;
  pRevisionFile.Border.Color := clSystemSolidNeutralFGDark;
  pEggFile.Background.Color := clSolidBGSecondaryDark;
  pEggFile.Border.Color := clSystemSolidNeutralFGDark;

  pGenerateFiles.Background.Color := clSystemCautionBGDark;
  pGenerateFiles.Border.Color := clSystemCautionFGDark;

  lblTitleImportFiles.Font.Color := clVioletFG1Dark;
  lblTitleImportFinished.Font.Color := clVioletFG1Dark;
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

procedure TdlgImportNests.eRevisionFileChange(Sender: TObject);
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

procedure TdlgImportNests.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eNestFile.Width := eRevisionFile.Width;
  eEggFile.Width := eRevisionFile.Width;
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
    if (not stopProcess) and HasImportFileSelected(eNestFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingJournal);
      ImportNestDataV1(eNestFile.FileName, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eRevisionFile.FileName) then
    begin
      AppendLog(rsProgressImportBandingEffort);
      ImportNestRevisionsV1(eRevisionFile.FileName, barProgress);
    end;

    if (not stopProcess) and HasImportFileSelected(eEggFile.FileName) then
    begin
      AppendLog(rsProgressImportCaptures);
      ImportEggDataV1(eEggFile.FileName, barProgress);
    end;
  except
    on E: Exception do
    begin
      AppendLog(Format(rsErrorImporting, [E.Message]));
      lblTitleImportFinished.Caption := rsImportCanceled;
      lblSubtitleImportFinished.Caption := rsImportCanceledByError;
      icoImportFinished.ImageIndex := 1;
      sbRetry.Visible := True;
      barProgress.Visible := False;
    end;
  end;

  if stopProcess then
  begin
    AppendLog(rsImportCanceledByUser);
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
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
    sbRetry.Visible := True;
    barProgress.Visible := False;
  end;

end;

procedure TdlgImportNests.UpdateButtons;
begin
  sbRun.Enabled := HasImportFileSelected(eNestFile.FileName) or
                   HasImportFileSelected(eRevisionFile.FileName) or
                   HasImportFileSelected(eEggFile.FileName);
end;

end.

