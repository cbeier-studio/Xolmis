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
    procedure sbCancelClick(Sender: TObject);
    procedure sbClearNestFileClick(Sender: TObject);
    procedure sbRetryClick(Sender: TObject);
    procedure sbRunClick(Sender: TObject);
  private
    FProgressList: TStrings;
    procedure UpdateButtons;
  public

  end;

var
  dlgImportNests: TdlgImportNests;

implementation

uses
  cbs_locale, cbs_global, cbs_import;

{$R *.lfm}

{ TdlgImportNests }

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
    TFileNameEdit(Sender).InitialDir := XSettings.LastPathUsed;
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

procedure TdlgImportNests.sbCancelClick(Sender: TObject);
begin
  Parar := True;

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

  Parar := False;
  FProgressList.Clear;

  nbContent.PageIndex := 1;

  sbCancel.Visible := True;
  sbClose.Visible := False;
  sbRun.Visible := False;

  FProgressList.Add(rsProgressImportBandingJournal);
  hvProgress.LoadFromString(FProgressList.Text);
  ImportNestDataV1(eNestFile.FileName, barProgress);

  if not Parar then
  begin
    FProgressList.Add(rsProgressImportBandingEffort);
    hvProgress.LoadFromString(FProgressList.Text);
    ImportNestRevisionsV1(eRevisionFile.FileName, barProgress);

    if not Parar then
    begin
      FProgressList.Add(rsProgressImportCaptures);
      hvProgress.LoadFromString(FProgressList.Text);
      ImportEggDataV1(eEggFile.FileName, barProgress);
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

procedure TdlgImportNests.UpdateButtons;
begin
  sbRun.Enabled := not IsEmptyStr(eNestFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eRevisionFile.FileName, [' ', '.']) or
                   not IsEmptyStr(eEggFile.FileName, [' ', '.']);
end;

end.

