unit udlg_bigtip;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, utils_dialogs;

type

  { TdlgBigTip }

  TdlgBigTip = class(TForm)
    btnClose: TButton;
    iTip: TImageList;
    imgTip: TImage;
    iTipDark: TImageList;
    lblTitle: TLabel;
    lblText: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FTipType: TBigTipType;
    function GetTitle: String;
    function GetTextHint: String;
    procedure ApplyDarkMode;
    procedure SetTitle(aValue: String);
    procedure SetTextHint(aValue: String);
  public
    property Title: String read GetTitle write SetTitle;
    property TextHint: String read GetTextHint write SetTextHint;
    property TipType: TBigTipType read FTipType write FTipType;
  end;

var
  dlgBigTip: TdlgBigTip;

implementation

uses
  utils_locale, utils_system, utils_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgBigTip }

procedure TdlgBigTip.ApplyDarkMode;
begin
  Color := clVioletBG1Dark;

  imgTip.Images := iTipDark;
end;

procedure TdlgBigTip.btnCloseClick(Sender: TObject);
begin
  //Close;
end;

procedure TdlgBigTip.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  case FTipType of
    obtDeleteRecord:
    begin
      lblTitle.Caption := rsDeleteRecordTitle;
      lblText.Caption := rsOnboardingDeleteRecord;
      imgTip.ImageIndex := 0;
    end;
    obtAddMedia:
    begin
      lblTitle.Caption := rsAddMediaFiles;
      lblText.Caption := rsOnboardingAddMedia;
      imgTip.ImageIndex := 1;
    end;
    obtSummary:
    begin
      lblTitle.Caption := rsSummary;
      lblText.Caption := rsOnboardingSummary;
      imgTip.ImageIndex := 2;
    end;
    obtGridSettings:
    begin
      lblTitle.Caption := rsColumns;
      lblText.Caption := rsOnboardingGridSettings;
      imgTip.ImageIndex := 3;
    end;
    obtQuickExport:
    begin
      lblTitle.Caption := rsQuickExport;
      lblText.Caption := rsOnboardingQuickExport;
      imgTip.ImageIndex := 4;
    end;
    obtMap:
    begin
      lblTitle.Caption := rsMapView;
      lblText.Caption := rsOnboardingMap;
      imgTip.ImageIndex := 5;
    end;
    obtNewBatchBands:
    begin
      lblTitle.Caption := rsNewBatchOfBands;
      lblText.Caption := rsOnboardingNewBatchBands;
      imgTip.ImageIndex := 6;
    end;
    obtTransferBands:
    begin
      lblTitle.Caption := rsTransferBands;
      lblText.Caption := rsOnboardingTransferBands;
      imgTip.ImageIndex := 7;
    end;
    obtBands:
    begin
      lblTitle.Caption := rsTitleBands;
      lblText.Caption := rsOnboardingBands;
      imgTip.ImageIndex := 8;
    end;
    obtFeathers:
    begin
      lblTitle.Caption := rsTitleFeathersAndMolt;
      lblText.Caption := rsOnboardingFeathers;
      imgTip.ImageIndex := 9;
    end;
    obtNewBatchNets:
    begin
      lblTitle.Caption := rsNewBatchOfNets;
      lblText.Caption := rsOnboardingNewBatchNets;
      imgTip.ImageIndex := 10;
    end;
    obtProjects:
    begin
      lblTitle.Caption := rsTitleProjects;
      lblText.Caption := rsOnboardingProjects;
      imgTip.ImageIndex := 11;
    end;
    obtGazetteer:
    begin
      lblTitle.Caption := rsTitleGazetteer;
      lblText.Caption := rsOnboardingGazetteer;
      imgTip.ImageIndex := 12;
    end;
    obtBotanicalTaxa:
    begin
      lblTitle.Caption := rsTitleBotanicalTaxa;
      lblText.Caption := rsOnboardingBotanicalTaxa;
      imgTip.ImageIndex := 13;
    end;
    obtTaxa:
    begin
      lblTitle.Caption := rsTitleZooTaxa;
      lblText.Caption := rsOnboardingTaxa;
      imgTip.ImageIndex := 14;
    end;
    obtCoordinatesConverter:
    begin
      lblTitle.Caption := rsTitleCoordinateConverter;
      lblText.Caption := rsOnboardingCoordinatesConverter;
      imgTip.ImageIndex := 15;
    end;
    obtGeoAssist:
    begin
      lblTitle.Caption := rsGeoAssist;
      lblText.Caption := rsOnboardingGeoAssist;
      imgTip.ImageIndex := 16;
    end;
    obtQuickEntry:
    begin
      lblTitle.Caption := rsQuickEntry;
      lblText.Caption := rsOnboardingQuickEntry;
      imgTip.ImageIndex := 17;
    end;
    obtUsers:
    begin
      lblTitle.Caption := rsTitleUsers;
      lblText.Caption := rsOnboardingUsers;
      imgTip.ImageIndex := 18;
    end;
    obtImportWizard:
    begin
      lblTitle.Caption := rsImportWizard;
      lblText.Caption := rsOnboardingImportWizard;
      imgTip.ImageIndex := 19;
    end;
    obtImportMobile:
    begin
      lblTitle.Caption := rsImportXolmisMobile;
      lblText.Caption := rsOnboardingImportMobile;
      imgTip.ImageIndex := 20;
    end;
    obtImportEbird:
    begin
      lblTitle.Caption := rsImportEbirdData;
      lblText.Caption := rsOnboardingImportEbird;
      imgTip.ImageIndex := 21;
    end;
    obtDarkMode:
    begin
      lblTitle.Caption := rsDarkMode;
      lblText.Caption := rsOnboardingDarkMode;
      imgTip.ImageIndex := 22;
    end;
    obtCaptureOutliers:
    begin
      lblTitle.Caption := rsMeasurementsOutliers;
      lblText.Caption := rsOnboardingCaptureOutliers;
      imgTip.ImageIndex := 23;
    end;
    obtAutomaticBackup:
    begin
      lblTitle.Caption := rsAutomaticBackup;
      lblText.Caption := rsOnboardingAutomaticBackup;
      imgTip.ImageIndex := 24;
    end;
    obtClearDeletedRecords:
    begin
      lblTitle.Caption := rsCleaningDeletedRecords;
      lblText.Caption := rsOnboardingClearDeletedRecords;
      imgTip.ImageIndex := 25;
    end;
    obtSearch:
    begin
      lblTitle.Caption := rsSearch;
      lblText.Caption := rsOnboardingSearch;
      imgTip.ImageIndex := 26;
    end;
    obtFeedback:
    begin
      lblTitle.Caption := rsFeedback;
      lblText.Caption := rsOnboardingFeedback;
      imgTip.ImageIndex := 27;
    end;
    obtNewDatabase:
    begin
      lblTitle.Caption := rsNewDatabase;
      lblText.Caption := rsOnboardingNewDatabase;
      imgTip.ImageIndex := 28;
    end;
  end;

  Top := (Screen.Height div 2) - (Height div 2);
end;

function TdlgBigTip.GetTextHint: String;
begin
  Result := lblText.Caption;
end;

function TdlgBigTip.GetTitle: String;
begin
  Result := lblTitle.Caption;
end;

procedure TdlgBigTip.SetTextHint(aValue: String);
begin
  lblText.Caption := aValue;
end;

procedure TdlgBigTip.SetTitle(aValue: String);
begin
  lblTitle.Caption := aValue;
end;

end.

