{ Xolmis Capture Editor dialog

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

unit uedt_capture;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, EditBtn, Spin, SysUtils, DB, SQLDB, Forms, Controls,
  Graphics, Dialogs, Character, DateUtils, ExtCtrls, StdCtrls, Menus, atshapelinebgra,
  models_birds, models_record_types;

type

  { TedtCapture }

  TedtCapture = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbCamera: TComboBox;
    ckEscaped: TCheckBox;
    ckBloodSample: TCheckBox;
    ckFeathers: TCheckBox;
    ckFeces: TCheckBox;
    ckParasites: TCheckBox;
    ckAudioRecordings: TCheckBox;
    ckPhotos: TCheckBox;
    ckClaw: TCheckBox;
    ckWholeSpecimen: TCheckBox;
    cbSex: TComboBox;
    cbSkullOssification: TComboBox;
    cbFlightFeatherMolt: TComboBox;
    cbFlightFeatherWear: TComboBox;
    cbCloacalProtuberance: TComboBox;
    cbBroodPatch: TComboBox;
    cbFat: TComboBox;
    cbBodyMolt: TComboBox;
    cbAge: TComboBox;
    cbStatus: TComboBox;
    cbCaptureType: TComboBox;
    eCycleCode: TEditButton;
    eCaptureTime: TEdit;
    eCaptureDate: TEditButton;
    eBander: TEditButton;
    eAnnotator: TEditButton;
    eSurvey: TEditButton;
    eLocality: TEditButton;
    eNet: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eIndividual: TEditButton;
    eTaxon: TEditButton;
    eBand: TEditButton;
    eRemovedBand: TEditButton;
    eRightTarsus: TEditButton;
    eLeftTarsus: TEditButton;
    eMoltLimits: TEditButton;
    eHowAged: TEditButton;
    eHowSexed: TEditButton;
    eStartPhoto: TEdit;
    eEndPhoto: TEdit;
    eFieldNumber: TEdit;
    ePhotographer1: TEditButton;
    ePhotographer2: TEditButton;
    dsLink: TDataSource;
    eHemoglobin: TFloatSpinEdit;
    eHematocrit: TFloatSpinEdit;
    eGlucose: TFloatSpinEdit;
    eKippsIndex: TFloatSpinEdit;
    eBillHeight: TFloatSpinEdit;
    eTarsusDiameter: TFloatSpinEdit;
    eWeight: TFloatSpinEdit;
    eTotalLength: TFloatSpinEdit;
    eTotalCulmen: TFloatSpinEdit;
    eSkullLength: TFloatSpinEdit;
    eExposedCulmen: TFloatSpinEdit;
    eNostrilBillTip: TFloatSpinEdit;
    eBillWidth: TFloatSpinEdit;
    eTailLength: TFloatSpinEdit;
    eTarsusLength: TFloatSpinEdit;
    eRightWingChord: TFloatSpinEdit;
    eFirstSecondaryChord: TFloatSpinEdit;
    lblIndividual1: TLabel;
    lblIndividual: TLabel;
    pmnNewLocality: TMenuItem;
    pmnNewIndividual: TMenuItem;
    pmnNewBand: TMenuItem;
    pmnNewSurvey: TMenuItem;
    pmnNewPerson: TMenuItem;
    mNotes: TMemo;
    lblAge: TLabel;
    Label10: TLabel;
    lblRightLegBelow: TLabel;
    Label12: TLabel;
    lblLeftLegBelow: TLabel;
    Label14: TLabel;
    lblCaptureTime: TLabel;
    lblTaxon: TLabel;
    lblNotes: TLabel;
    lblPhotographer1: TLabel;
    lblStartPhoto: TLabel;
    lblHemoglobin: TLabel;
    lblBander: TLabel;
    lblAnnotator: TLabel;
    lblNet: TLabel;
    lblLongitude: TLabel;
    lblLatitude: TLabel;
    lblBand: TLabel;
    lblRemovedBand: TLabel;
    lblCloacalProtuberance: TLabel;
    lblBroodPatch: TLabel;
    lblCaptureDate: TLabel;
    lblFat: TLabel;
    lblBodyMolt: TLabel;
    lblFlightFeatherMolt: TLabel;
    lblFlightFeatherWear: TLabel;
    lblRightWingChord: TLabel;
    lblFirstSecondaryChord: TLabel;
    lblTailLength: TLabel;
    lblTarsusLength: TLabel;
    lblTarsusDiameter: TLabel;
    lblWeight: TLabel;
    lblBillHeight: TLabel;
    lblTotalLength: TLabel;
    lblTotalCulmen: TLabel;
    lblPhilornisLarvae: TLabel;
    Label44: TLabel;
    lblExposedCulmen: TLabel;
    lblNostrilBillTip: TLabel;
    lblBillWidth: TLabel;
    lblKippsIndex: TLabel;
    lblMoltLimits: TLabel;
    lblSurvey: TLabel;
    lblSkullOssification: TLabel;
    Label51: TLabel;
    lblCycleCode: TLabel;
    lblHowAged: TLabel;
    lblSex: TLabel;
    lblHowSexed: TLabel;
    lblPhotographer2: TLabel;
    lblCamera: TLabel;
    Label58: TLabel;
    lblEndPhoto: TLabel;
    lblLocality: TLabel;
    Label60: TLabel;
    lblFieldNumber: TLabel;
    Label62: TLabel;
    lblHematocrit: TLabel;
    lblGlucose: TLabel;
    Label65: TLabel;
    lblCaptureType: TLabel;
    Label8: TLabel;
    lblStatus: TLabel;
    lblTitleCapture: TLabel;
    lblTitleCollection: TLabel;
    lblTitleMetrics: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pLeftBands: TPanel;
    pmNew: TPopupMenu;
    pRightBands: TPanel;
    pSamples: TPanel;
    pPhotographersCamera: TPanel;
    pPhotosFieldNumber: TPanel;
    pHemoglobinGlucose: TPanel;
    pAgeEscapedStatus: TPanel;
    pNotes: TPanel;
    pDateTimeBander: TPanel;
    pColorBands: TPanel;
    pIndividual: TPanel;
    pTypeNetLongLat: TPanel;
    pMetrics1: TPanel;
    pMetrics2: TPanel;
    pMetrics3: TPanel;
    pMetrics5: TPanel;
    pMetrics4: TPanel;
    pKippMoltLimitsSkull: TPanel;
    pCycleSex: TPanel;
    pSurveyLocality: TPanel;
    pTaxonBand: TPanel;
    pTitleCapture: TPanel;
    pTitleCollection: TPanel;
    pTitleMetrics: TPanel;
    sbCancel: TButton;
    sBox: TScrollBox;
    sbSave: TButton;
    shpLeftBelowBand1: TShape;
    shpLeftBelowBand2: TShape;
    shpLeftBelowBand3: TShape;
    shpLeftBelowBand4: TShape;
    shpRightBelowBand1: TShape;
    shpRightBelowBand2: TShape;
    shpRightBelowBand3: TShape;
    shpRightBelowBand4: TShape;
    ePhilornisLarvae: TSpinEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure cbAgeKeyPress(Sender: TObject; var Key: char);
    procedure cbCaptureTypeKeyPress(Sender: TObject; var Key: char);
    procedure cbCaptureTypeSelect(Sender: TObject);
    procedure cbSexKeyPress(Sender: TObject; var Key: char);
    procedure cbStatusKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAnnotatorButtonClick(Sender: TObject);
    procedure eAnnotatorKeyPress(Sender: TObject; var Key: char);
    procedure eBandButtonClick(Sender: TObject);
    procedure eBanderButtonClick(Sender: TObject);
    procedure eBanderKeyPress(Sender: TObject; var Key: char);
    procedure eBandKeyPress(Sender: TObject; var Key: char);
    procedure eCaptureDateButtonClick(Sender: TObject);
    procedure eCaptureTimeKeyPress(Sender: TObject; var Key: char);
    procedure eCycleCodeButtonClick(Sender: TObject);
    procedure eHowAgedButtonClick(Sender: TObject);
    procedure eHowSexedButtonClick(Sender: TObject);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualEditingDone(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eLeftTarsusButtonClick(Sender: TObject);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeExit(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eMoltLimitsButtonClick(Sender: TObject);
    procedure eNetButtonClick(Sender: TObject);
    procedure eNetExit(Sender: TObject);
    procedure eNetKeyPress(Sender: TObject; var Key: char);
    procedure ePhotographer1ButtonClick(Sender: TObject);
    procedure ePhotographer1EditingDone(Sender: TObject);
    procedure ePhotographer1KeyPress(Sender: TObject; var Key: char);
    procedure ePhotographer2ButtonClick(Sender: TObject);
    procedure ePhotographer2KeyPress(Sender: TObject; var Key: char);
    procedure eRemovedBandButtonClick(Sender: TObject);
    procedure eRemovedBandEditingDone(Sender: TObject);
    procedure eRemovedBandKeyPress(Sender: TObject; var Key: char);
    procedure eRightTarsusButtonClick(Sender: TObject);
    procedure eRightTarsusExit(Sender: TObject);
    procedure eRightWingChordEditingDone(Sender: TObject);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyExit(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewBandClick(Sender: TObject);
    procedure pmnNewIndividualClick(Sender: TObject);
    procedure pmnNewLocalityClick(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure pmnNewSurveyClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FCapture: TCapture;
    FIndividualId: Integer;
    FSurveyId, FLocalityId, FNetId, FTaxonId, FBandId, FRemovedBandId: Integer;
    FBanderId, FAnnotatorId, FPhotographer1Id, FPhotographer2Id: Integer;
    procedure SetCapture(Value: TCapture);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
    procedure PaintColorBands(aLeg: TBodyPart);
    procedure GetCameras;
    procedure GetIndividualData;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Capture: TCapture read FCapture write SetCapture;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtCapture: TedtCapture;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_gis, utils_validations,
  utils_editdialogs, data_types, data_consts, data_getvalue, data_columns, models_taxonomy,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtCapture }

procedure TedtCapture.ApplyDarkMode;
begin
  eIndividual.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  eSurvey.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eCaptureDate.Images := DMM.iEditsDark;
  eBander.Images := DMM.iEditsDark;
  eAnnotator.Images := DMM.iEditsDark;
  eNet.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eBand.Images := DMM.iEditsDark;
  eRemovedBand.Images := DMM.iEditsDark;
  eRightTarsus.Images := DMM.iEditsDark;
  eLeftTarsus.Images := DMM.iEditsDark;
  eMoltLimits.Images := DMM.iEditsDark;
  eCycleCode.Images := DMM.iEditsDark;
  eHowAged.Images := DMM.iEditsDark;
  eHowSexed.Images := DMM.iEditsDark;
  ePhotographer1.Images := DMM.iEditsDark;
  ePhotographer2.Images := DMM.iEditsDark;
end;

procedure TedtCapture.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_CAPTURES);
end;

procedure TedtCapture.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtCapture.cbAgeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    cbAge.ItemIndex := -1;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.cbCaptureTypeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    cbCaptureType.ItemIndex := -1;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.cbCaptureTypeSelect(Sender: TObject);
begin
  if cbCaptureType.Text = rsCaptureChangeBand then
    lblRemovedBand.Caption := rsCaptionRemovedBand + ': *'
  else
    lblRemovedBand.Caption := rsCaptionRemovedBand + ':';

  if cbCaptureType.Text = rsCaptureUnbanded then
    lblBand.Caption := rsCaptionBand + ':'
  else
    lblBand.Caption := rsCaptionBand + ': *';
end;

procedure TedtCapture.cbSexKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    cbSex.ItemIndex := -1;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.cbStatusKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    cbStatus.ItemIndex := -1;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtCapture.eCycleCodeButtonClick(Sender: TObject);
begin
  MoltCycleDialog(eCycleCode);
end;

procedure TedtCapture.eAnnotatorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAnnotator, FAnnotatorId, '', COL_ABBREVIATION);
end;

procedure TedtCapture.eAnnotatorKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eAnnotator, FAnnotatorId, Key, COL_ABBREVIATION);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FAnnotatorId := 0;
    eAnnotator.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtCapture.eBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eBand, FBandId);
end;

procedure TedtCapture.eBanderButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eBander, FBanderId, '', COL_ABBREVIATION);
end;

procedure TedtCapture.eBanderKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eBander, FBanderId, Key, COL_ABBREVIATION);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FBanderId := 0;
    eBander.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbBands, eBand, FBandId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FBandId := 0;
    eBand.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eCaptureDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eCaptureDate.Text, eCaptureDate, Dt);
end;

procedure TedtCapture.eCaptureTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eHowAgedButtonClick(Sender: TObject);
begin
  HowAgedDialog(eHowAged);
end;

procedure TedtCapture.eHowSexedButtonClick(Sender: TObject);
begin
  HowAgedDialog(eHowSexed);
end;

procedure TedtCapture.eIndividualButtonClick(Sender: TObject);
begin
  if FindDlg(tbIndividuals, eIndividual, FIndividualId) then
    GetIndividualData;
end;

procedure TedtCapture.eIndividualEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtCapture.eIndividualKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbIndividuals, eIndividual, FIndividualId, Key) then
      GetIndividualData;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FIndividualId := 0;
    eIndividual.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eLeftTarsusButtonClick(Sender: TObject);
begin
  EditColorBands(eLeftTarsus);
  PaintColorBands(bpLeftTarsus);
  if eLeftTarsus.CanSetFocus then
    eLeftTarsus.SetFocus;
end;

procedure TedtCapture.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId, '', COL_SITE_NAME);
end;

procedure TedtCapture.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key, COL_SITE_NAME);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eLongitudeExit(Sender: TObject);
var
  Ax: TMapAxis;
  aField, aDMS: String;
  C: Extended;
begin
  aDMS := '';
  if Sender = eLongitude then
  begin
    aField := COL_LONGITUDE;
    Ax := maLongitude;
  end
  else
  if Sender = eLatitude then
  begin
    aField := COL_LATITUDE;
    Ax := maLatitude;
  end
  else
    Exit;

  C := dsLink.DataSet.FieldByName(aField).AsFloat;
  if C <> 0.0 then
    aDMS := AxisDecToDMS(FloatToStr(C), Ax, True);

  case Ax of
    maBoth:
      ;
    maLongitude:
      if aDMS = '' then
        lblLongitude.Caption := rsLongitude + ':'
      else
        lblLongitude.Caption := Format(rsLongitudeCaption, [aDMS]);
    maLatitude:
      if aDMS = '' then
        lblLatitude.Caption := rsLatitude + ':'
      else
        lblLatitude.Caption := Format(rsLatitudeCaption, [aDMS]);
  end;
end;

procedure TedtCapture.eLongitudeKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', ',', '.', '+', '-', #8, #13, #27];
var
  EditText: String;
  PosDecimal: Integer;
  DecimalValue: Extended;
begin
  FormKeyPress(Sender, Key);

  EditText := EmptyStr;
  PosDecimal := 0;
  DecimalValue := 0;

  if not (Key in AllowedChars) then
  begin
    Key := #0;
    Exit;
  end;

  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
    Exit;
  end;

  if (Sender is TEdit) then
    EditText := TEdit(Sender).Text
  else
  if (Sender is TEditButton) then
    EditText := TEditButton(Sender).Text;
  PosDecimal := Pos(FormatSettings.DecimalSeparator, EditText);

  // Decimal separator
  if (Key in [',', '.']) then
  begin
    if (PosDecimal = 0) then
      Key := FormatSettings.DecimalSeparator
    else
      Key := #0;
    Exit;
  end;

  // Numeric signal
  if (Key in ['+', '-']) then
  begin
    if (Length(EditText) > 0) then
    begin
      if TryStrToFloat(EditText, DecimalValue) then
      begin
        if ((DecimalValue > 0) and (Key = '-')) or ((DecimalValue < 0) and (Key = '+')) then
          DecimalValue := DecimalValue * -1.0;
        EditText := FloatToStr(DecimalValue);

        if (Sender is TEdit) then
        begin
          TEdit(Sender).Text := EditText;
          TEdit(Sender).SelStart := Length(EditText);
        end
        else
        if (Sender is TEditButton) then
        begin
          TEditButton(Sender).Text := EditText;
          TEditButton(Sender).SelStart := Length(EditText);
        end;
      end;
      Key := #0;
    end
    else
    begin
      if (Key = '+') then
        Key := #0;
    end;

    Exit;
  end;
end;

procedure TedtCapture.eMoltLimitsButtonClick(Sender: TObject);
begin
  MoltLimitsDialog(eMoltLimits);
end;

procedure TedtCapture.eNetButtonClick(Sender: TObject);
begin
  FindDlg(tbNetsEffort, eNet, FNetId);
end;

procedure TedtCapture.eNetExit(Sender: TObject);
var
  NetCoord: TMapPoint;
begin
  if FNetId > 0 then
  begin
    if GetLatLong('nets_effort', COL_LONGITUDE, COL_LATITUDE, COL_FULL_NAME, COL_NET_ID, FNetId, NetCoord) then
    begin
      if (NetCoord.X = 0) and (NetCoord.Y = 0) then
        Exit;

      if (eLongitude.Text = EmptyStr) then
        eLongitude.Text := FloatToStr(NetCoord.X);
      if (eLatitude.Text = EmptyStr) then
        eLatitude.Text := FloatToStr(NetCoord.Y);
    end;
  end;
end;

procedure TedtCapture.eNetKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNetsEffort, eNet, FNetId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FNetId := 0;
    eNet.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.ePhotographer1ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePhotographer1, FPhotographer1Id, '', COL_ABBREVIATION);
end;

procedure TedtCapture.ePhotographer1EditingDone(Sender: TObject);
begin
  if (FPhotographer1Id > 0) or (FPhotographer2Id > 0) or (cbCamera.Text <> EmptyStr) or
    (eStartPhoto.Text <> EmptyStr) or (eEndPhoto.Text <> EmptyStr) then
    ckPhotos.Checked := True;
end;

procedure TedtCapture.ePhotographer1KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePhotographer1, FPhotographer1Id, Key, COL_ABBREVIATION);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FPhotographer1Id := 0;
    ePhotographer1.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.ePhotographer2ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePhotographer2, FPhotographer2Id, '', COL_ABBREVIATION);
end;

procedure TedtCapture.ePhotographer2KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePhotographer2, FPhotographer2Id, Key, COL_ABBREVIATION);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FPhotographer2Id := 0;
    ePhotographer2.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eRemovedBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eRemovedBand, FRemovedBandId);
end;

procedure TedtCapture.eRemovedBandEditingDone(Sender: TObject);
begin
  if (FRemovedBandId > 0) then
    cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureChangeBand);
end;

procedure TedtCapture.eRemovedBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbBands, eRemovedBand, FRemovedBandId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FRemovedBandId := 0;
    eRemovedBand.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eRightTarsusButtonClick(Sender: TObject);
begin
  EditColorBands(eRightTarsus);
  PaintColorBands(bpRightTarsus);
  if eRightTarsus.CanSetFocus then
    eRightTarsus.SetFocus;
end;

procedure TedtCapture.eRightTarsusExit(Sender: TObject);
begin
  if Sender = eRightTarsus then
    PaintColorBands(bpRightTarsus)
  else
  if Sender = eLeftTarsus then
    PaintColorBands(bpLeftTarsus);
end;

procedure TedtCapture.eRightWingChordEditingDone(Sender: TObject);
begin
  if (eRightWingChord.Value > 0) and (eFirstSecondaryChord.Value > 0) then
    eKippsIndex.Value := eRightWingChord.Value - eFirstSecondaryChord.Value;
end;

procedure TedtCapture.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, FSurveyId);
end;

procedure TedtCapture.eSurveyExit(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  if FSurveyId > 0 then
  begin
    Qry := TSQLQuery.Create(DMM.sqlCon);
    try
      Qry.Database := DMM.sqlCon;
      Qry.Transaction := DMM.sqlTrans;
      Qry.SQL.Add('SELECT locality_id, expedition_id, survey_date FROM surveys');
      Qry.SQL.Add('WHERE survey_id = :survey_id');
      Qry.ParamByName('SURVEY_ID').AsInteger := FSurveyId;
      Qry.Open;
      if Qry.RecordCount > 0 then
      begin
        FLocalityId := Qry.FieldByName(COL_LOCALITY_ID).AsInteger;
        eLocality.Text := GetName('gazetteer', COL_FULL_NAME, COL_SITE_ID, FLocalityId);
        eCaptureDate.Text := DateToStr(Qry.FieldByName(COL_SURVEY_DATE).AsDateTime);
      end;
      Qry.Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

procedure TedtCapture.eSurveyKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSurveys, eSurvey, FSurveyId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSurveyId := 0;
    eSurvey.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, True, FTaxonId);
end;

procedure TedtCapture.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, True, FTaxonId, key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.FormCreate(Sender: TObject);
begin
  GetCameras;
  cbAge.Items.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
  cbSex.Items.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  cbCaptureType.Items.CommaText := rsCaptureTypeList;
  cbStatus.Items.CommaText := '"' + rsStatusNormal + '","' + rsStatusInjured + '","' +
    rsStatusWingSprain + '","' + rsStatusStressed + '","' + rsStatusDead + '"';
end;

procedure TedtCapture.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not sbSave.Enabled then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtCapture.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtCapture.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionCapture)]);
    if not DateIsNull(FCapture.CaptureDate) then
      eCaptureDate.Text := DateToStr(FCapture.CaptureDate)
    else
      eCaptureDate.Text := DateToStr(Today);
    cbCaptureType.ItemIndex := 0;
    if FCapture.LocalityId > 0 then
    begin
      FLocalityId := FCapture.LocalityId;
      eLocality.Text := GetName('gazetteer', COL_FULL_NAME, COL_SITE_ID, FLocalityId);
    end;
    if FCapture.SurveyId > 0 then
    begin
      FSurveyId := FCapture.SurveyId;
      eSurvey.Text := GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, FSurveyId);
    end;
    if FCapture.TaxonId > 0 then
    begin
      FTaxonId := FCapture.TaxonId;
      eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
    end;
    if FCapture.BandId > 0 then
    begin
      FBandId := FCapture.BandId;
      eBand.Text := GetName('bands', COL_FULL_NAME, COL_BAND_ID, FBandId);
    end;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionCapture)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;

  sBox.VertScrollBar.Position := 0;

  PaintColorBands(bpRightTarsus);
  PaintColorBands(bpLeftTarsus);
  //eSurvey.SetFocus;

  //eLongitudeExit(eLongitude);
  //eLongitudeExit(eLatitude);
end;

procedure TedtCapture.GetCameras;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT camera_name');
    Add('FROM captures');
    Add('WHERE (active_status = 1) AND (camera_name NOT NULL) AND (camera_name <> '''')');
    Add('GROUP BY camera_name');
    Add('ORDER BY camera_name ASC');
    //GravaLogSQL(SQL);
    Open;
    First;
    try
      cbCamera.Items.BeginUpdate;
      cbCamera.Items.Clear;
      repeat
        cbCamera.Items.Add(Fields[0].AsString);
        Next;
      until Eof;
      cbCamera.Sorted := True;
    finally
      cbCamera.Items.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TedtCapture.GetIndividualData;
var
  FIndividual: TIndividual;
begin
  if FIndividualId = 0 then
    Exit;

  FIndividual := TIndividual.Create(FIndividualId);
  try
    FTaxonId := FIndividual.TaxonId;
    eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
    FBandId := FIndividual.BandId;
    eBand.Text := GetName('bands', COL_FULL_NAME, COL_BAND_ID, FBandId);
    eRightTarsus.Text := FIndividual.RightLegBelow;
    eLeftTarsus.Text := FIndividual.LeftLegBelow;
    FCapture.SubjectAge := FIndividual.Age;
    case FCapture.SubjectAge of
      ageUnknown:     cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeUnknown);
      ageNestling:    cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeNestling);
      ageFledgling:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFledgling);
      ageJuvenile:    cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeJuvenile);
      ageAdult:       cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeAdult);
      ageFirstYear:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFirstYear);
      ageSecondYear:  cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeSecondYear);
      ageThirdYear:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeThirdYear);
      ageFourthYear:  cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFourthYear);
      ageFifthYear:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFifthYear);
    else
      cbAge.ItemIndex := -1;
    end;
    FCapture.SubjectSex := FIndividual.Sex;
    case FCapture.SubjectSex of
      sexUnknown: cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexUnknown);
      sexMale:    cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexMale);
      sexFemale:  cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexFemale);
    else
      cbSex.ItemIndex := -1;
    end;
  finally
    FIndividual.Free;
  end;
end;

procedure TedtCapture.GetRecord;
begin
  FIndividualId := FCapture.IndividualId;
  eIndividual.Text := GetName('individuals', COL_FULL_NAME, COL_INDIVIDUAL_ID, FIndividualId);
  FSurveyId := FCapture.SurveyId;
  eSurvey.Text := GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, FSurveyId);
  FLocalityId := FCapture.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_FULL_NAME, COL_SITE_ID, FLocalityId);
  if not DateIsNull(FCapture.CaptureDate) then
    eCaptureDate.Text := DateToStr(FCapture.CaptureDate);
  if (FCapture.CaptureTime <> NullTime) then
    eCaptureTime.Text := FormatDateTime('hh:nn', FCapture.CaptureTime);
  FBanderId := FCapture.BanderId;
  eBander.Text := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, FBanderId);
  FAnnotatorId := FCapture.AnnotatorId;
  eAnnotator.Text := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, FAnnotatorId);
  case FCapture.CaptureType of
    cptNew:         cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureNew);
    cptRecapture:   cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureRecapture);
    cptSameDay:     cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureSameDay);
    cptChangeBand:  cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureChangeBand);
    cptUnbanded:    cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureUnbanded);
  else
    cbCaptureType.ItemIndex := -1;
  end;
  FNetId := FCapture.NetId;
  eNet.Text := GetName('nets_effort', COL_NET_NUMBER, COL_NET_ID, FNetId);
  if (FCapture.Longitude <> 0.0) and (FCapture.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FCapture.Longitude);
    eLatitude.Text := FloatToStr(FCapture.Latitude);
  end;
  FTaxonId := FCapture.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  FBandId := FCapture.BandId;
  eBand.Text := GetName('bands', COL_FULL_NAME, COL_BAND_ID, FBandId);
  FRemovedBandId := FCapture.RemovedBandId;
  eRemovedBand.Text := GetName('bands', COL_FULL_NAME, COL_BAND_ID, FRemovedBandId);
  eRightTarsus.Text := FCapture.RightLegBelow;
  eLeftTarsus.Text := FCapture.LeftLegBelow;
  case FCapture.SubjectAge of
    ageUnknown:     cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeUnknown);
    ageNestling:    cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeNestling);
    ageFledgling:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFledgling);
    ageJuvenile:    cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeJuvenile);
    ageAdult:       cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeAdult);
    ageFirstYear:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFirstYear);
    ageSecondYear:  cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeSecondYear);
    ageThirdYear:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeThirdYear);
    ageFourthYear:  cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFourthYear);
    ageFifthYear:   cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFifthYear);
  else
    cbAge.ItemIndex := -1;
  end;
  ckEscaped.Checked := FCapture.Escaped;
  case FCapture.SubjectStatus of
    sstNormal:      cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusNormal);
    sstInjured:     cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusInjured);
    sstWingSprain:  cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusWingSprain);
    sstStressed:    cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusStressed);
    sstDead:        cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusDead);
  else
    cbStatus.ItemIndex := -1;
  end;
  cbCloacalProtuberance.ItemIndex := cbCloacalProtuberance.Items.IndexOf(FCapture.CloacalProtuberance);
  cbBroodPatch.ItemIndex := cbBroodPatch.Items.IndexOf(FCapture.BroodPatch);
  cbFat.ItemIndex := cbFat.Items.IndexOf(FCapture.Fat);
  cbBodyMolt.ItemIndex := cbBodyMolt.Items.IndexOf(FCapture.BodyMolt);
  cbFlightFeatherMolt.ItemIndex := cbFlightFeatherMolt.Items.IndexOf(FCapture.FlightFeathersMolt);
  cbFlightFeatherWear.ItemIndex := cbFlightFeatherWear.Items.IndexOf(FCapture.FlightFeathersWear);
  eRightWingChord.Value := FCapture.RightWingChord;
  eFirstSecondaryChord.Value := FCapture.FirstSecondaryChord;
  eTailLength.Value := FCapture.TailLength;
  eTarsusLength.Value := FCapture.TarsusLength;
  eTarsusDiameter.Value := FCapture.TarsusDiameter;
  eWeight.Value := FCapture.Weight;
  eSkullLength.Value := FCapture.SkullLength;
  eExposedCulmen.Value := FCapture.ExposedCulmen;
  eNostrilBillTip.Value := FCapture.NostrilBillTip;
  eBillWidth.Value := FCapture.BillWidth;
  eBillHeight.Value := FCapture.BillHeight;
  eTotalLength.Value := FCapture.TotalLength;
  eTotalCulmen.Value := FCapture.CulmenLength;
  ePhilornisLarvae.Value := FCapture.PhilornisLarvaeTally;
  eKippsIndex.Value := FCapture.KippsIndex;
  eMoltLimits.Text := FCapture.MoltLimits;
  cbSkullOssification.ItemIndex := cbSkullOssification.Items.IndexOf(FCapture.SkullOssification);
  eCycleCode.Text := FCapture.CycleCode;
  eHowAged.Text := FCapture.HowAged;
  case FCapture.SubjectSex of
    sexUnknown: cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexUnknown);
    sexMale:    cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexMale);
    sexFemale:  cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexFemale);
  else
    cbSex.ItemIndex := -1;
  end;
  eHowSexed.Text := FCapture.HowSexed;
  mNotes.Text := FCapture.Notes;
  ckBloodSample.Checked := FCapture.BloodSample;
  ckFeathers.Checked := FCapture.FeatherSample;
  ckFeces.Checked := FCapture.FecesSample;
  ckParasites.Checked := FCapture.ParasiteSample;
  ckAudioRecordings.Checked := FCapture.SubjectRecorded;
  ckPhotos.Checked := FCapture.SubjectPhotographed;
  ckClaw.Checked := FCapture.ClawSample;
  ckWholeSpecimen.Checked := FCapture.SubjectCollected;
  FPhotographer1Id := FCapture.Photographer1Id;
  ePhotographer1.Text := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, FPhotographer1Id);
  FPhotographer2Id := FCapture.Photographer2Id;
  ePhotographer2.Text := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, FPhotographer2Id);
  cbCamera.ItemIndex := cbCamera.Items.IndexOf(FCapture.CameraName);
  if (FCapture.StartPhotoNumber <> EmptyStr) and (FCapture.StartPhotoNumber <> '0') then
    eStartPhoto.Text := FCapture.StartPhotoNumber;
  if (FCapture.EndPhotoNumber <> EmptyStr) and (FCapture.EndPhotoNumber <> '0') then
    eEndPhoto.Text := FCapture.EndPhotoNumber;
  eFieldNumber.Text := FCapture.FieldNumber;
  eHemoglobin.Value := FCapture.Hemoglobin;
  eHematocrit.Value := FCapture.Hematocrit;
  eGlucose.Value := FCapture.Glucose;
end;

function TedtCapture.IsRequiredFilled: Boolean;
begin
  Result := False;

  if pIndividual.Visible then
  begin
    if (FLocalityId > 0) and
      (FBanderId > 0) and
      (FAnnotatorId > 0) and
      (eCaptureDate.Text <> EmptyStr) and
      (cbCaptureType.ItemIndex >= 0) and
      ((cbCaptureType.Text <> rsCaptureUnbanded) and (FBandId > 0)) and
      (FIndividualId > 0) then
      Result := True;
  end
  else
  begin
    if (FLocalityId > 0) and
      (FBanderId > 0) and
      (FAnnotatorId > 0) and
      (eCaptureDate.Text <> EmptyStr) and
      (cbCaptureType.ItemIndex >= 0) and
      ((cbCaptureType.Text <> rsCaptureUnbanded) and (FBandId > 0)) then
      Result := True;
  end;

  if (cbCaptureType.Text = rsCaptureUnbanded) then
    lblBand.Caption := rsCaptionBand + ':'
  else
    lblBand.Caption := rsCaptionBand + ': *';
end;

procedure TedtCapture.PaintColorBands(aLeg: TBodyPart);
var
  i: Integer;
  B: TStrings;
  Band: TShape;
  //aField: String;
  aEdit: TEditButton;

  procedure PaintBand(aColor: String; aShape: TShape);
  var
    Cores: array of String;
    cBrush: TColor;
  begin
    if aColor = '' then
    begin
      aShape.Brush.Style := bsClear;
      aShape.Pen.Style := psDot;
      aShape.Pen.Color := clSilver;
      Exit;
    end;

    for Cores in BAND_COLORS do
    begin
      if Cores[0].Equals(aColor) then
      begin
        cBrush := StringToColor(Cores[1]);
        aShape.Brush.Color := cBrush;
        aShape.Pen.Style := psSolid;

        Break;
      end;
    end;
  end;

  procedure ClearBand(aShape: TShape);
  begin
    aShape.Brush.Style := bsClear;
    aShape.Pen.Style := psDot;
  end;

begin
  case aLeg of
    bpRightTarsus:
      begin
        ClearBand(shpRightBelowBand1);
        ClearBand(shpRightBelowBand2);
        ClearBand(shpRightBelowBand3);
        ClearBand(shpRightBelowBand4);
        //aField := 'right_leg_below';
        aEdit := eRightTarsus;
      end;
    bpLeftTarsus:
      begin
        ClearBand(shpLeftBelowBand1);
        ClearBand(shpLeftBelowBand2);
        ClearBand(shpLeftBelowBand3);
        ClearBand(shpLeftBelowBand4);
        //aField := 'left_leg_below';
        aEdit := eLeftTarsus;
      end;
    bpRightTibia:
      Exit;
    bpLeftTibia:
      Exit;
    bpRightWing:
      Exit;
    bpLeftWing:
      Exit;
    bpNeck:
      Exit;
  end;

  if (aEdit.Text <> EmptyStr) then
  begin
    B := TStringList.Create;
    B.CommaText := aEdit.Text;
    for i := 0 to B.Count - 1 do
    begin
      case i of
        0:
          case aLeg of
            bpRightTarsus:
              Band := shpRightBelowBand1;
            bpLeftTarsus:
              Band := shpLeftBelowBand1;
          end;
        1:
          case aLeg of
            bpRightTarsus:
              Band := shpRightBelowBand2;
            bpLeftTarsus:
              Band := shpLeftBelowBand2;
          end;
        2:
          case aLeg of
            bpRightTarsus:
              Band := shpRightBelowBand3;
            bpLeftTarsus:
              Band := shpLeftBelowBand3;
          end;
        3:
          case aLeg of
            bpRightTarsus:
              Band := shpRightBelowBand4;
            bpLeftTarsus:
              Band := shpLeftBelowBand4;
          end;
      end;
      PaintBand(B[i], Band);
    end;
    B.Free;
  end;
end;

procedure TedtCapture.pmnNewBandClick(Sender: TObject);
begin
  EditBand(DMG.qBands, True);
end;

procedure TedtCapture.pmnNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

procedure TedtCapture.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtCapture.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtCapture.pmnNewSurveyClick(Sender: TObject);
begin
  EditSurvey(DMG.qSurveys, 0, True);
end;

procedure TedtCapture.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtCapture.SetCapture(Value: TCapture);
begin
  if Assigned(Value) then
    FCapture := Value;
end;

procedure TedtCapture.SetRecord;
begin
  FCapture.IndividualId := FIndividualId;
  FCapture.SurveyId    := FSurveyId;
  FCapture.LocalityId  := FLocalityId;
  FCapture.CaptureDate := StrToDate(eCaptureDate.Text);
  if (Length(eCaptureTime.Text) > 0) then
    FCapture.CaptureTime := StrToTime(eCaptureTime.Text)
  else
    FCapture.CaptureTime := NullTime;
  FCapture.BanderId    := FBanderId;
  FCapture.AnnotatorId := FAnnotatorId;
  case cbCaptureType.ItemIndex of
    0: FCapture.CaptureType := cptNew;
    1: FCapture.CaptureType := cptRecapture;
    2: FCapture.CaptureType := cptSameDay;
    3: FCapture.CaptureType := cptChangeBand;
    4: FCapture.CaptureType := cptUnbanded;
  end;
  FCapture.NetId         := FNetId;
  if (Length(eLongitude.Text) > 0) then
    FCapture.Longitude   := StrToFloat(eLongitude.Text)
  else
    FCapture.Longitude := 0;
  if (Length(eLatitude.Text) > 0) then
    FCapture.Latitude   := StrToFloat(eLatitude.Text)
  else
    FCapture.Latitude := 0;
  FCapture.TaxonId       := FTaxonId;
  FCapture.BandId        := FBandId;
  FCapture.RemovedBandId := FRemovedBandId;
  FCapture.RightLegBelow := eRightTarsus.Text;
  FCapture.LeftLegBelow  := eLeftTarsus.Text;
  case cbAge.ItemIndex of
    0: FCapture.SubjectAge := ageUnknown;
    1: FCapture.SubjectAge := ageAdult;
    2: FCapture.SubjectAge := ageJuvenile;
    3: FCapture.SubjectAge := ageFledgling;
    4: FCapture.SubjectAge := ageNestling;
    5: FCapture.SubjectAge := ageFirstYear;
    6: FCapture.SubjectAge := ageSecondYear;
    7: FCapture.SubjectAge := ageThirdYear;
    8: FCapture.SubjectAge := ageFourthYear;
    9: FCapture.SubjectAge := ageFifthYear;
  else
    FCapture.SubjectAge := ageUnknown;
  end;
  FCapture.Escaped := ckEscaped.Checked;
  case cbStatus.ItemIndex of
    0: FCapture.SubjectStatus := sstNormal;
    1: FCapture.SubjectStatus := sstInjured;
    2: FCapture.SubjectStatus := sstWingSprain;
    3: FCapture.SubjectStatus := sstStressed;
    4: FCapture.SubjectStatus := sstDead;
  end;
  FCapture.CloacalProtuberance  := cbCloacalProtuberance.Text;
  FCapture.BroodPatch           := cbBroodPatch.Text;
  FCapture.Fat                  := cbFat.Text;
  FCapture.BodyMolt             := cbBodyMolt.Text;
  FCapture.FlightFeathersMolt   := cbFlightFeatherMolt.Text;
  FCapture.FlightFeathersWear   := cbFlightFeatherWear.Text;
  FCapture.RightWingChord       := eRightWingChord.Value;
  FCapture.FirstSecondaryChord  := eFirstSecondaryChord.Value;
  FCapture.TailLength           := eTailLength.Value;
  FCapture.TarsusLength         := eTarsusLength.Value;
  FCapture.TarsusDiameter       := eTarsusDiameter.Value;
  FCapture.Weight               := eWeight.Value;
  FCapture.SkullLength          := eSkullLength.Value;
  FCapture.ExposedCulmen        := eExposedCulmen.Value;
  FCapture.NostrilBillTip       := eNostrilBillTip.Value;
  FCapture.BillWidth            := eBillWidth.Value;
  FCapture.BillHeight           := eBillHeight.Value;
  FCapture.TotalLength          := eTotalLength.Value;
  FCapture.CulmenLength         := eTotalCulmen.Value;
  FCapture.PhilornisLarvaeTally := ePhilornisLarvae.Value;
  FCapture.KippsIndex           := eKippsIndex.Value;
  FCapture.MoltLimits           := eMoltLimits.Text;
  FCapture.SkullOssification    := cbSkullOssification.Text;
  FCapture.CycleCode            := eCycleCode.Text;
  FCapture.HowAged              := eHowAged.Text;
  case cbSex.ItemIndex of
    0: FCapture.SubjectSex := sexMale;
    1: FCapture.SubjectSex := sexFemale;
    2: FCapture.SubjectSex := sexUnknown;
  else
    FCapture.SubjectSex := sexUnknown;
  end;
  FCapture.HowSexed            := eHowSexed.Text;
  FCapture.Notes               := mNotes.Text;
  FCapture.BloodSample         := ckBloodSample.Checked;
  FCapture.FeatherSample       := ckFeathers.Checked;
  FCapture.FecesSample         := ckFeces.Checked;
  FCapture.ParasiteSample      := ckParasites.Checked;
  FCapture.SubjectRecorded     := ckAudioRecordings.Checked;
  FCapture.SubjectPhotographed := ckPhotos.Checked;
  FCapture.ClawSample          := ckClaw.Checked;
  FCapture.SubjectCollected    := ckWholeSpecimen.Checked;
  FCapture.Photographer1Id     := FPhotographer1Id;
  FCapture.Photographer2Id     := FPhotographer2Id;
  FCapture.CameraName          := cbCamera.Text;
  FCapture.StartPhotoNumber    := eStartPhoto.Text;
  FCapture.EndPhotoNumber      := eEndPhoto.Text;
  FCapture.FieldNumber         := eFieldNumber.Text;
  FCapture.Hemoglobin          := eHemoglobin.Value;
  FCapture.Hematocrit          := eHematocrit.Value;
  FCapture.Glucose             := eGlucose.Value;
end;

function TedtCapture.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (FIndividualId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscIndividual]));
  if (FLocalityId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscLocality]));
  if (eCaptureDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rsDateCapture]));
  if (FBanderId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscBander]));
  if (FAnnotatorId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscAnnotator]));
  if (cbCaptureType.ItemIndex < 0) then
    Msgs.Add(Format(rsRequiredField, [rscType]));
  // Conditional required fields
  if (eLongitude.Text <> EmptyStr) and (eLatitude.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscLatitude]));
  if (eLatitude.Text <> EmptyStr) and (eLongitude.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscLongitude]));
  if (cbCaptureType.Caption <> rsCaptureUnbanded) and (FBandId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscBand]));
  if (cbCaptureType.Caption = rsCaptureChangeBand) and (FRemovedBandId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscRemovedBand]));
  if (cbAge.ItemIndex >= 0) and (eHowAged.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscHowWasAged]));
  if (cbSex.ItemIndex >= 0) and (eHowSexed.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscHowWasSexed]));

  // Dates
  if ValidDate(eCaptureDate.Text, rsDateCapture, Msgs) then
    IsFutureDate(StrToDate(eCaptureDate.Text), Today, rsDateCapture, rsDateToday);

  // Time
  if (eCaptureTime.Text <> EmptyStr) then
    ValidTime(eCaptureTime.Text, rsTimeCapture, Msgs);

  // Set of values
  ValueInSet(cbCloacalProtuberance.Text, rscCloacalProtuberance, CLOACAL_PROTUBERANCE_VALUES, Msgs);
  ValueInSet(cbBroodPatch.Text, rscBroodPatch, BROOD_PATCH_VALUES, Msgs);
  ValueInSet(cbFat.Text, rsSubcutaneousFat, FAT_VALUES, Msgs);
  ValueInSet(cbBodyMolt.Text, rscBodyMolt, BODY_MOLT_VALUES, Msgs);
  ValueInSet(cbFlightFeatherMolt.Text, rsFlightMolt, FLIGHT_MOLT_VALUES, Msgs);
  ValueInSet(cbFlightFeatherWear.Text, rsFlightWear, FEATHER_WEAR_VALUES, Msgs);
  ValueInSet(cbSkullOssification.Text, rscSkullOssification, SKULL_OSSIFICATION_VALUES, Msgs);

  // Outliers
  { #todo : Validate capture measurements for outliers }

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

