unit udlg_importcaptures;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, ComCtrls, StdCtrls,
  Buttons, StrUtils, atshapelinebgra, BCPanel, HtmlView;

type

  { TdlgImportCaptures }

  TdlgImportCaptures = class(TForm)
    barProgress: TProgressBar;
    eWeatherFile: TFileNameEdit;
    icoWeatherFile: TImage;
    lblWeatherFile: TLabel;
    pEffortFile1: TBCPanel;
    pRetry: TBCPanel;
    imgFinished: TImageList;
    hvProgress: THtmlViewer;
    icoImportFinished: TImage;
    lblSubtitleImportFinished: TLabel;
    lblTitleImportFinished: TLabel;
    pContentProgress: TBCPanel;
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
    pContentFinished: TBCPanel;
    pgImportFinished: TPage;
    pgImportProgress: TPage;
    pgImportFiles: TPage;
    pBottom: TPanel;
    pCaptureFile: TBCPanel;
    pContentFiles: TPanel;
    pEffortFile: TBCPanel;
    pJournalFile: TBCPanel;
    pProgress: TBCPanel;
    sbCancel: TButton;
    sbClearEffortFile: TSpeedButton;
    sbClearCaptureFile: TSpeedButton;
    sbClearWeatherFile: TSpeedButton;
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbRun: TButton;
    sbClearJournalFile: TSpeedButton;
    procedure eCaptureFileChange(Sender: TObject);
    procedure eEffortFileChange(Sender: TObject);
    procedure eJournalFileButtonClick(Sender: TObject);
    procedure eJournalFileChange(Sender: TObject);
    procedure eWeatherFileChange(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearJournalFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    procedure UpdateButtons;
  public

  end;

var
  dlgImportCaptures: TdlgImportCaptures;

implementation

uses
  cbs_global;

{ TdlgImportCaptures }

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
    TFileNameEdit(Sender).InitialDir := XSettings.LastPathUsed;
end;

procedure TdlgImportCaptures.eJournalFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportCaptures.eWeatherFileChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TdlgImportCaptures.sbCancelClick(Sender: TObject);
begin
  Parar := True;

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
    2: eWeatherFile.Clear;
    3: eCaptureFile.Clear;
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

  Parar := False;

  nbContent.PageIndex := 1;

  sbCancel.Visible := True;
  sbClose.Visible := False;
  sbRun.Visible := False;
end;

procedure TdlgImportCaptures.UpdateButtons;
begin
  sbRun.Enabled := not IsEmptyStr(eJournalFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eEffortFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eWeatherFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eCaptureFile.FileName, [' ', '.']);
end;

end.

