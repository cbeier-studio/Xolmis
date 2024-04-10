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
    sbClose: TButton;
    sbRetry: TBitBtn;
    sbRun: TButton;
    sbClearJournalFile: TSpeedButton;
    procedure eCaptureFileChange(Sender: TObject);
    procedure eEffortFileChange(Sender: TObject);
    procedure eJournalFileButtonClick(Sender: TObject);
    procedure eJournalFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearJournalFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    FProgressList: TStrings;
    procedure UpdateButtons;
  public

  end;

var
  dlgImportCaptures: TdlgImportCaptures;

implementation

uses
  cbs_locale, cbs_global, cbs_import;

{$R *.lfm}

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

procedure TdlgImportCaptures.FormCreate(Sender: TObject);
begin
  FProgressList := TStringList.Create;
end;

procedure TdlgImportCaptures.FormDestroy(Sender: TObject);
begin
  FProgressList.Free;
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

  Parar := False;
  FProgressList.Clear;

  nbContent.PageIndex := 1;

  sbCancel.Visible := True;
  sbClose.Visible := False;
  sbRun.Visible := False;

  FProgressList.Add(rsProgressImportBandingJournal);
  hvProgress.LoadFromString(FProgressList.Text);
  ImportBandingJournalV1(eJournalFile.FileName, barProgress);

  if not Parar then
  begin
    FProgressList.Add(rsProgressImportBandingEffort);
    hvProgress.LoadFromString(FProgressList.Text);
    ImportBandingEffortV1(eEffortFile.FileName, barProgress);

    if not Parar then
    begin
      FProgressList.Add(rsProgressImportCaptures);
      hvProgress.LoadFromString(FProgressList.Text);
      ImportBandingDataV1(eCaptureFile.FileName, barProgress);
    end;
  end;

  nbContent.PageIndex := 2;
  if Parar then
  begin
    lblTitleImportFinished.Caption := rsImportCanceled;
    lblSubtitleImportFinished.Caption := rsImportCanceledByUser;
    icoImportFinished.ImageIndex := 1;
  end
  else
  begin
    lblTitleImportFinished.Caption := rsFinishedImporting;
    lblSubtitleImportFinished.Caption := rsSuccessfulImport;
    icoImportFinished.ImageIndex := 0;
  end;

end;

procedure TdlgImportCaptures.UpdateButtons;
begin
  sbRun.Enabled := not IsEmptyStr(eJournalFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eEffortFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eCaptureFile.FileName, [' ', '.']);
end;

end.

