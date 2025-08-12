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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, ComCtrls, StdCtrls,
  Buttons, StrUtils, atshapelinebgra, BCPanel, HtmlView;

type

  { TdlgImportNests }

  TdlgImportNests = class(TForm)
    barProgress: TProgressBar;
    iButtons: TImageList;
    iButtonsDark: TImageList;
    iIcons: TImageList;
    iIconsDark: TImageList;
    imgFinishedDark: TImageList;
    pRetry: TBCPanel;
    imgFinished: TImageList;
    hvProgress: THtmlViewer;
    icoImportFinished: TImage;
    lblSubtitleImportFinished: TLabel;
    lblTitleImportFinished: TLabel;
    pContentProgress: TBCPanel;
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
    pContentFinished: TBCPanel;
    pgImportFinished: TPage;
    pgImportProgress: TPage;
    pgImportFiles: TPage;
    pBottom: TPanel;
    pEggFile: TBCPanel;
    pContentFiles: TPanel;
    pRevisionFile: TBCPanel;
    pNestFile: TBCPanel;
    pProgress: TBCPanel;
    sbCancel: TButton;
    sbClearRevisionFile: TSpeedButton;
    sbClearEggFile: TSpeedButton;
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbRun: TButton;
    sbClearNestFile: TSpeedButton;
    procedure eEggFileChange(Sender: TObject);
    procedure eRevisionFileChange(Sender: TObject);
    procedure eNestFileButtonClick(Sender: TObject);
    procedure eNestFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearNestFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    FProgressList: TStrings;
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

procedure TdlgImportNests.ApplyDarkMode;
begin
  eNestFile.Images := iButtonsDark;
  eRevisionFile.Images := iButtonsDark;
  eEggFile.Images := iButtonsDark;
  sbClearNestFile.Images := iButtonsDark;
  sbClearRevisionFile.Images := iButtonsDark;
  sbClearEggFile.Images := iButtonsDark;
  sbRetry.Images := iButtonsDark;

  icoNestFile.Images := iIconsDark;
  icoRevisionFile.Images := iIconsDark;
  icoEggFile.Images := iIconsDark;

  icoImportFinished.Images := imgFinishedDark;

  pProgress.Background.Color := clCardBGDefaultDark;
  pProgress.Border.Color := clSystemSolidNeutralFGDark;
  pProgress.Color := pContentProgress.Background.Color;

  pNestFile.Background.Color := clCardBGDefaultDark;
  pNestFile.Border.Color := clSystemSolidNeutralFGDark;
  pRevisionFile.Background.Color := clCardBGDefaultDark;
  pRevisionFile.Border.Color := clSystemSolidNeutralFGDark;
  pEggFile.Background.Color := clCardBGDefaultDark;
  pEggFile.Border.Color := clSystemSolidNeutralFGDark;
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

procedure TdlgImportNests.FormCreate(Sender: TObject);
begin
  FProgressList := TStringList.Create;
end;

procedure TdlgImportNests.FormDestroy(Sender: TObject);
begin
  FProgressList.Free;
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

  stopProcess := False;
  FProgressList.Clear;

  nbContent.PageIndex := 1;

  sbCancel.Visible := True;
  sbClose.Visible := False;
  sbRun.Visible := False;

  FProgressList.Add(rsProgressImportBandingJournal);
  hvProgress.LoadFromString(FProgressList.Text);
  ImportNestDataV1(eNestFile.FileName, barProgress);

  if not stopProcess then
  begin
    FProgressList.Add(rsProgressImportBandingEffort);
    hvProgress.LoadFromString(FProgressList.Text);
    ImportNestRevisionsV1(eRevisionFile.FileName, barProgress);

    if not stopProcess then
    begin
      FProgressList.Add(rsProgressImportCaptures);
      hvProgress.LoadFromString(FProgressList.Text);
      ImportEggDataV1(eEggFile.FileName, barProgress);
    end;
  end;

  nbContent.PageIndex := 2;
  if stopProcess then
  begin
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
  end
  else
  begin
    DMM.sqlCon.ExecuteDirect('PRAGMA optimize;');
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
  end;

end;

procedure TdlgImportNests.UpdateButtons;
begin
  sbRun.Enabled := not IsEmptyStr(eNestFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eRevisionFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eEggFile.FileName, [' ', '.']);
end;

end.

