{ Xolmis Geographical Coordinates Converter form

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

unit ufrm_geoconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  Menus, SynEdit, BCPanel, SynEditTypes, SynEditMarks, SynHighlighterAny, StrUtils,
  RegExpr;

type

  { TfrmGeoConverter }

  TfrmGeoConverter = class(TForm)
    cbConvertFrom: TComboBox;
    cbConvertTo: TComboBox;
    ckAddUnitsZone: TCheckBox;
    eUTMZone: TEdit;
    iButtonsDark: TImageList;
    icoConverter: TImage;
    iButtons: TImageList;
    imgSBarDark: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    lblLineCol: TLabel;
    lblUTMHemisphere: TLabel;
    lblUTMZone: TLabel;
    Panel2: TPanel;
    pConvertFromToolbar: TBCPanel;
    imgSBar: TImageList;
    Label1: TLabel;
    pConverted: TPanel;
    pConvertFrom: TPanel;
    pConvertedToolbar: TBCPanel;
    pConvert: TBCPanel;
    pUtmZone: TBCPanel;
    pUtmHemisphere: TBCPanel;
    pAddUnitsZone: TBCPanel;
    pFormatInfo: TBCPanel;
    pCaretPos: TBCPanel;
    pLeft: TPanel;
    pmtSaveFile: TMenuItem;
    pmtCopy: TMenuItem;
    pmtAddToGeoBank: TMenuItem;
    pmtClear: TMenuItem;
    pmfOpenFile: TMenuItem;
    pmfPaste: TMenuItem;
    pmfClear: TMenuItem;
    OpenDlg: TOpenDialog;
    PBar: TProgressBar;
    pmConvertFrom: TPopupMenu;
    pmConverted: TPopupMenu;
    rbNorth: TRadioButton;
    rbSouth: TRadioButton;
    SaveDlg: TSaveDialog;
    sbAddToGeoEditor: TSpeedButton;
    sbClear: TSpeedButton;
    sbConvert: TSpeedButton;
    sbCopy: TSpeedButton;
    sbOpenFile: TSpeedButton;
    sbPaste: TSpeedButton;
    sbSaveFile: TSpeedButton;
    seConverted: TSynEdit;
    seConvertFrom: TSynEdit;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    SplitSynEdit: TSplitter;
    SynGeo: TSynAnySyn;
    procedure cbConvertToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbAddToGeoEditorClick(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbConvertClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure sbOpenFileClick(Sender: TObject);
    procedure sbPasteClick(Sender: TObject);
    procedure sbSaveFileClick(Sender: TObject);
    procedure seConvertFromChange(Sender: TObject);
    procedure seConvertFromClick(Sender: TObject);
    procedure seConvertFromKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure seConvertFromPaste(Sender: TObject; var AText: String; var AMode: TSynSelectionMode;
      ALogStartPos: TPoint; var AnAction: TSynCopyPasteAction);
  private
    procedure AddMark(aLine: Integer);
    procedure ApplyDarkMode;
    procedure FormatCoordinates;
    procedure UpdateButtons;
    procedure UpdateCurrentCaretPos;
  public

  end;

var
  frmGeoConverter: TfrmGeoConverter;

implementation

uses cbs_locale, cbs_global, cbs_dialogs, cbs_gis, cbs_themes, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TfrmGeoConverter }

procedure TfrmGeoConverter.FormCreate(Sender: TObject);
begin
  cbConvertFrom.ItemIndex := 0;
  cbConvertTo.ItemIndex := 1;
  UpdateButtons;
end;

procedure TfrmGeoConverter.FormResize(Sender: TObject);
begin
  pConvertFrom.Width := ((ClientWidth - pLeft.Width) div 2) - (SplitSynEdit.Width div 2);
end;

procedure TfrmGeoConverter.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  UpdateCurrentCaretPos;
end;

procedure TfrmGeoConverter.cbConvertToChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmGeoConverter.sbAddToGeoEditorClick(Sender: TObject);
var
  i: Integer;
  aTemp: TStringList;
  S, Nome: String;
begin
  if seConverted.Lines.Count = 0 then
    Exit;

  Nome := InputBox(rsTitleImportCoordinates, rsCoordinatesNameInput, '');
  for i := 0 to seConverted.Lines.Count - 1 do
  begin
    aTemp := TStringList.Create;
    try
      S := seConverted.Lines[i];
      ExtractStrings([';'], [' '], PChar(S), aTemp);
      with DMM.TabGeoBank do
      begin
        Append;
        FieldByName('coordinate_name').AsString := Nome + ' ' + IntToStr(i + 1);
        FieldByName('longitude').AsFloat := StrToFloat(aTemp[0]);
        FieldByName('latitude').AsFloat := StrToFloat(aTemp[1]);
        Post;
      end;
    finally
      FreeAndNil(aTemp);
    end;
  end;
  MsgDlg(rsSuccessfulImportCoordinates, rsCoordinatesAvailableToUse, mtInformation);
end;

procedure TfrmGeoConverter.sbClearClick(Sender: TObject);
begin
  seConvertFrom.Lines.Clear;
  seConverted.Lines.Clear;
  seConverted.Marks.ClearLine(1);
  UpdateButtons;
end;

procedure TfrmGeoConverter.sbConvertClick(Sender: TObject);
type
  TConvertCoord = (cvSame, cvDecimalDms, cvDecimalUtm, cvDmsDecimal, cvDmsUtm, cvUtmDecimal, cvUtmDms);
var
  i: Integer;
  l: String;
  pDec: TMapPoint;
  pDMS: TDMSPoint;
  pUTM: TUTMPoint;
  Conv: TConvertCoord;
begin
  Conv := cvSame;

  if seConvertFrom.Lines.Count = 0 then
    Exit;
  if (cbConvertFrom.ItemIndex < 0) or (cbConvertTo.ItemIndex < 0) then
  begin
    MsgDlg('', rsSelectCoordinatesTypes, mtInformation);
    Exit;
  end;
  if (cbConvertFrom.ItemIndex = 2) and (eUTMZone.Text = '') then
  begin
    MsgDlg('', rsInformUTMZone, mtInformation);
    if eUTMZone.CanSetFocus then
      eUTMZone.SetFocus;
    Exit;
  end;

  case cbConvertFrom.ItemIndex of
    0:  // Decimal Degrees
      case cbConvertTo.ItemIndex of
        0: Conv := cvSame;
        1: Conv := cvDecimalDms;
        2: Conv := cvDecimalUtm;
      end;
    1:  // Degrees Minutes Seconds
      case cbConvertTo.ItemIndex of
        0: Conv := cvDmsDecimal;
        1: Conv := cvSame;
        2: Conv := cvDmsUtm;
      end;
    2:  // UTM
      case cbConvertTo.ItemIndex of
        0: Conv := cvUtmDecimal;
        1: Conv := cvUtmDms;
        2: Conv := cvSame;
      end;
  end;

  // "Conversion" to the same format
  if Conv = cvSame then
  begin
    MsgDlg('', rsSameCoordinateFormat, mtInformation);
//    mTo.Lines.Assign(seConvertFrom.Lines);
    Exit;
  end;

  try
    PBar.Position := 0;
    PBar.Max := seConvertFrom.Lines.Count;
    PBar.Visible := True;
    //frm_Main.Taskbar.ProgressValue := 0;
    //frm_Main.Taskbar.ProgressState := TTaskBarProgressState.Normal;
    seConverted.Lines.Clear;

    for i := 0 to seConvertFrom.Lines.Count - 1 do
    begin
      l := seConvertFrom.Lines[i];
      case Conv of
        cvSame: { nothing } ;
        cvDecimalDms:          // Decimal -> DMS
          begin
            if pDec.FromString(l) then
              seConverted.Lines.Add(DecimalToDms(pDec).ToString(ckAddUnitsZone.Checked))
            else
              seConverted.Lines.Add(Format('%s: %s [%d]',[rsTitleError, l, i + 1]));
          end;
        cvDecimalUtm:          // Decimal -> UTM
          begin
            if pDec.FromString(l) then
              seConverted.Lines.Add(DecimalToUtm(pDec).ToString(ckAddUnitsZone.Checked))
            else
              seConverted.Lines.Add(Format('%s: %s [%d]',[rsTitleError, l, i + 1]));
          end;
        cvDmsDecimal:          // DMS -> Decimal
          begin
            if pDms.FromString(l) then
              seConverted.Lines.Add(DmsToDecimal(pDms).ToString)
            else
              seConverted.Lines.Add(Format('%s: %s [%d]',[rsTitleError, l, i + 1]));
          end;
        cvDmsUtm:              // DMS -> UTM
          begin
            if pDms.FromString(l) then
              seConverted.Lines.Add(DmsToUtm(pDms).ToString(ckAddUnitsZone.Checked))
            else
              seConverted.Lines.Add(Format('%s: %s [%d]',[rsTitleError, l, i + 1]));
          end;
        cvUtmDecimal:          // UTM -> Decimal
          begin
            if pUtm.FromString(l) then
              seConverted.Lines.Add(UtmToDecimal(pUtm).ToString)
            else
              seConverted.Lines.Add(Format('%s: %s [%d]',[rsTitleError, l, i + 1]));
          end;
        cvUtmDms:              // UTM -> DMS
          begin
            if pUtm.FromString(l) then
              seConverted.Lines.Add(UtmToDms(pUtm).ToString(ckAddUnitsZone.Checked))
            else
              seConverted.Lines.Add(Format('%s: %s [%d]',[rsTitleError, l, i + 1]));
          end;
      end;
      if seConverted.Lines[seConverted.Lines.Count - 1].StartsWith(rsTitleError, True) then
      begin
        AddMark(seConverted.Lines.Count - 1);
      end;

      PBar.Position := i + 1;
      //frm_Main.Taskbar.ProgressValue := PBar.Position;
    end;
    PBar.Position := PBar.Max;
    //frm_Main.Taskbar.ProgressValue := PBar.Max;
  finally
    PBar.Visible := False;
    PBar.Position := 0;
    //frm_Main.Taskbar.ProgressState := TTaskBarProgressState.None;
    //frm_Main.Taskbar.ProgressValue := 0;
  end;
end;

procedure TfrmGeoConverter.sbCopyClick(Sender: TObject);
begin
  if seConverted.Lines.Count > 0 then
  begin
    seConverted.SelectAll;
    seConverted.CopyToClipboard;
  end;
end;

procedure TfrmGeoConverter.sbOpenFileClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    seConvertFrom.Lines.LoadFromFile(OpenDlg.FileName);
    FormatCoordinates;
  end;
end;

procedure TfrmGeoConverter.sbPasteClick(Sender: TObject);
begin
  seConvertFrom.PasteFromClipboard;
end;

procedure TfrmGeoConverter.sbSaveFileClick(Sender: TObject);
begin
  if SaveDlg.Execute then
  begin
    seConverted.Lines.SaveToFile(SaveDlg.FileName);
  end;
end;

procedure TfrmGeoConverter.seConvertFromChange(Sender: TObject);
begin
  UpdateCurrentCaretPos;
end;

procedure TfrmGeoConverter.seConvertFromClick(Sender: TObject);
begin
  UpdateCurrentCaretPos;
end;

procedure TfrmGeoConverter.seConvertFromKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdateCurrentCaretPos;
end;

procedure TfrmGeoConverter.seConvertFromPaste(Sender: TObject; var AText: String;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint; var AnAction: TSynCopyPasteAction);
begin
  FormatCoordinates;
  UpdateCurrentCaretPos;
end;

procedure TfrmGeoConverter.AddMark(aLine: Integer);
var
  m: TSynEditMark;
begin
  m := TSynEditMark.Create(seConverted);
  m.Line := aLine;
  m.ImageList := imgSBar;
  m.ImageIndex := 1;
  m.Visible := True;
  seConverted.Marks.Add(m);
end;

procedure TfrmGeoConverter.ApplyDarkMode;
begin
  pLeft.Color := clSolidBGBaseDark;
  pConvertFrom.Color := clSolidBGBaseDark;
  pConverted.Color := clSolidBGBaseDark;

  pConvert.Background.Color := clCardBGDefaultDark;
  pConvert.Border.Color := clCardBGSecondaryDark;
  pUtmZone.Background.Color := clCardBGDefaultDark;
  pUtmZone.Border.Color := clCardBGSecondaryDark;
  pUtmHemisphere.Background.Color := clCardBGDefaultDark;
  pUtmHemisphere.Border.Color := clCardBGSecondaryDark;
  Panel2.Color := clCardBGDefaultDark;
  pAddUnitsZone.Background.Color := clCardBGDefaultDark;
  pAddUnitsZone.Border.Color := clCardBGSecondaryDark;
  pCaretPos.Background.Color := clCardBGDefaultDark;
  pCaretPos.Border.Color := clCardBGSecondaryDark;
  pConvertFromToolbar.Background.Color := clCardBGDefaultDark;
  pConvertFromToolbar.Border.Color := clCardBGSecondaryDark;
  pConvertedToolbar.Background.Color := clCardBGDefaultDark;
  pConvertedToolbar.Border.Color := clCardBGSecondaryDark;

  pFormatInfo.Background.Color := clVioletBG1Dark;
  pFormatInfo.Border.Color := clCardBGSecondaryDark;

  SplitSynEdit.ParentColor := True;
  seConvertFrom.Color := clSolidBGSecondaryDark;
  seConvertFrom.Gutter.Color := clSolidBGSecondaryDark;
  seConvertFrom.Gutter.Parts[1].MarkupInfo.Background := clSolidBGSecondaryDark;
  seConvertFrom.Gutter.Parts[1].MarkupInfo.Foreground := clTextTertiaryDark;
  seConvertFrom.Gutter.Parts[3].MarkupInfo.Background := clSolidBGSecondaryDark;
  seConvertFrom.Gutter.Parts[4].MarkupInfo.Background := clSolidBGSecondaryDark;
  seConvertFrom.Font.Color := clTextPrimaryDark;
  seConverted.Color := clSolidBGSecondaryDark;
  seConverted.Gutter.Color := clSolidBGSecondaryDark;
  seConverted.Gutter.Parts[1].MarkupInfo.Background := clSolidBGSecondaryDark;
  seConverted.Gutter.Parts[1].MarkupInfo.Foreground := clTextTertiaryDark;
  seConverted.Font.Color := clTextPrimaryDark;

  pmConvertFrom.Images := iButtonsDark;
  pmConverted.Images := iButtonsDark;
  sbConvert.Images := iButtonsDark;
  sbOpenFile.Images := iButtonsDark;
  sbPaste.Images := iButtonsDark;
  sbClear.Images := iButtonsDark;
  sbSaveFile.Images := iButtonsDark;
  sbCopy.Images := iButtonsDark;
  sbAddToGeoEditor.Images := iButtonsDark;
end;

procedure TfrmGeoConverter.FormatCoordinates;
var
  p: Extended;
  X: String;
  pcaret: TPoint;
  aFrom: TMapCoordinateType;
begin
  if Trim(seConvertFrom.Text) = EmptyStr then
    Exit;

  aFrom := mcDecimal;
  p := 0.0;
  pcaret := seConvertFrom.CaretXY;

  // replace <Tab> separator
  seConvertFrom.Text := ReplaceRegExpr('\t+', seConvertFrom.Text, '; ');
  // replace DecimalSeparator if it is a Comma
  if FormatSettings.DecimalSeparator = ',' then
    seConvertFrom.Text := StringReplace(seConvertFrom.Text, '.', ',', [rfReplaceAll, rfIgnoreCase]);
  // replace double spaces
  seConvertFrom.Text := ReplaceRegExpr('\h+', seConvertFrom.Text, ' ');

  X := Trim(ExtractDelimited(1, seConvertFrom.Lines[0], [';']));
  if WordCount(X, DmsSymbols + [' ']) > 2 then
    aFrom := mcDMS
  else
  if (ExecRegExpr('^[0-9]{1,3}[a-zA-Z]{1}$', X)) or ((TryStrToFloat(X, p)) and (p > 180)) then
    aFrom := mcUTM;

  case aFrom of
    mcDecimal:
      begin
        cbConvertFrom.ItemIndex := 0;
        cbConvertTo.ItemIndex := 1;
      end;
    mcDMS:
      begin
        seConvertFrom.Text := RemoveSymbolsDMS(seConvertFrom.Text);
        cbConvertFrom.ItemIndex := 1;
        cbConvertTo.ItemIndex := 0;
      end;
    mcUTM:
      begin
        cbConvertFrom.ItemIndex := 2;
        cbConvertTo.ItemIndex := 0;
      end;
  end;

  seConvertFrom.CaretXY := pcaret;
  UpdateButtons;
  UpdateCurrentCaretPos;
end;

procedure TfrmGeoConverter.UpdateButtons;
begin
  sbConvert.Enabled := Trim(seConvertFrom.Lines.Text) <> EmptyStr;
  sbClear.Enabled := (Trim(seConvertFrom.Lines.Text) <> EmptyStr) or
    (Trim(seConverted.Lines.Text) <> EmptyStr);
  sbSaveFile.Enabled := Trim(seConverted.Lines.Text) <> EmptyStr;
  sbCopy.Enabled := Trim(seConverted.Lines.Text) <> EmptyStr;
  sbAddToGeoEditor.Enabled := Trim(seConverted.Lines.Text) <> EmptyStr;
end;

procedure TfrmGeoConverter.UpdateCurrentCaretPos;
begin
  lblLineCol.Caption := Format('Ln: %d : Col: %d',
    [seConvertFrom.CaretY, seConvertFrom.CaretX]);
end;

end.

