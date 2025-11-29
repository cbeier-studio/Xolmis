unit udlg_onboarding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, EditBtn,
  ATShapeLineBGRA, BCPanel;

type

  { TdlgOnboarding }

  TdlgOnboarding = class(TForm)
    btnSaveLocations: TBitBtn;
    dsConn: TDataSource;
    eVideosPath: TDirectoryEdit;
    eDocumentsPath: TDirectoryEdit;
    eAudiosPath: TDirectoryEdit;
    eImagesPath: TDirectoryEdit;
    icoAudiosPath: TImage;
    icoVideosPath: TImage;
    icoDocumentsPath: TImage;
    icoImagesPath: TImage;
    imgWelcome: TImage;
    iFinished: TImageList;
    iFinishedDark: TImageList;
    imgFinished: TImage;
    lblVideosPath: TLabel;
    lblFinishedInstruction: TLabel;
    lblDocumentsPath: TLabel;
    lblAudiosPath: TLabel;
    lblMediaInstruction: TLabel;
    lblImagesPath: TLabel;
    lblTitleFinished: TLabel;
    lblTitleMedia: TLabel;
    pVideosPath: TBCPanel;
    pgFinished: TPage;
    pgMedia: TPage;
    pDocumentsPath: TBCPanel;
    pAudiosPath: TBCPanel;
    pImagesPath: TBCPanel;
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
    procedure btnSaveLocationsClick(Sender: TObject);
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
    procedure ApplyDarkMode;
  public

  end;

var
  dlgOnboarding: TdlgOnboarding;

implementation

uses
  utils_global, utils_locale, utils_themes, utils_dialogs, udm_main, uDarkStyleParams, udlg_newdatabase, uedt_database;

{$R *.lfm}

{ TdlgOnboarding }

procedure TdlgOnboarding.ApplyDarkMode;
begin
  btnHelp.Images := iButtonsDark;
  btnStart.Images := iButtonsDark;
  btnSaveLocations.Images := iButtonsDark;

  imgWelcome.Images := iFinishedDark;
  imgFinished.Images := iFinishedDark;
  icoNewDatabase.Images := iButtonsDark;
  icoOpenDatabase.Images := iButtonsDark;
  arrowNewDatabase.Images := iButtonsDark;
  arrowOpenDatabase.Images := iButtonsDark;
  icoImagesPath.Images := iButtonsDark;
  icoAudiosPath.Images := iButtonsDark;
  icoVideosPath.Images := iButtonsDark;
  icoDocumentsPath.Images := iButtonsDark;

  eImagesPath.Images := DMM.iEditsDark;
  eAudiosPath.Images := DMM.iEditsDark;
  eVideosPath.Images := DMM.iEditsDark;
  eDocumentsPath.Images := DMM.iEditsDark;

  pNewDatabase.Background.Color := clSolidBGSecondaryDark;
  pNewDatabase.Border.Color := clSystemSolidNeutralFGDark;
  pOpenDatabase.Background.Color := clSolidBGSecondaryDark;
  pOpenDatabase.Border.Color := clSystemSolidNeutralFGDark;
  pImagesPath.Background.Color := clSolidBGSecondaryDark;
  pImagesPath.Border.Color := clSystemSolidNeutralFGDark;
  pAudiosPath.Background.Color := clSolidBGSecondaryDark;
  pAudiosPath.Border.Color := clSystemSolidNeutralFGDark;
  pVideosPath.Background.Color := clSolidBGSecondaryDark;
  pVideosPath.Border.Color := clSystemSolidNeutralFGDark;
  pDocumentsPath.Background.Color := clSolidBGSecondaryDark;
  pDocumentsPath.Border.Color := clSystemSolidNeutralFGDark;

  lblTitleWelcome.Font.Color := clVioletFG1Dark;
  lblTitleDatabase.Font.Color := clVioletFG1Dark;
  lblTitleMedia.Font.Color := clVioletFG1Dark;
  lblTitleFinished.Font.Color := clVioletFG1Dark;
end;

procedure TdlgOnboarding.btnSaveLocationsClick(Sender: TObject);
begin
  xSettings.ImagesFolder := eImagesPath.Directory;
  xSettings.AudiosFolder := eAudiosPath.Directory;
  xSettings.VideosFolder := eVideosPath.Directory;
  xSettings.DocumentsFolder := eDocumentsPath.Directory;
  xSettings.SaveToFile;

  nbPages.PageIndex := pgFinished.PageIndex;
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
  eImagesPath.Directory := xSettings.ImagesFolder;
  eAudiosPath.Directory := xSettings.AudiosFolder;
  eVideosPath.Directory := xSettings.VideosFolder;
  eDocumentsPath.Directory := xSettings.DocumentsFolder;
end;

procedure TdlgOnboarding.pNewDatabaseClick(Sender: TObject);
begin
  pNewDatabase.Border.Color := clVioletFGLight;
  pNewDatabase.Border.Width := 2;

  if NewDatabase then
    nbPages.PageIndex := pgMedia.PageIndex;

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
begin
  pOpenDatabase.Border.Color := clVioletFGLight;
  pOpenDatabase.Border.Width := 2;

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

      nbPages.PageIndex := pgMedia.PageIndex;
    end
    else
      dsConn.DataSet.Cancel;
  finally
    FreeAndNil(edtDatabase);
  end;

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

