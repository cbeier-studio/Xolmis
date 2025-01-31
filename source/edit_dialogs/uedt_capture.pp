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
  Classes, EditBtn, Spin, SysUtils, DB, SQLDB, Forms, Controls, Graphics,
  Dialogs, Character, DateUtils, ExtCtrls, StdCtrls, DBCtrls, DBEditButton,
  atshapelinebgra, cbs_birds;

type

  { TedtCapture }

  TedtCapture = class(TForm)
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
    procedure cbAgeKeyPress(Sender: TObject; var Key: char);
    procedure cbCaptureTypeKeyPress(Sender: TObject; var Key: char);
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
    procedure ePhotographer1KeyPress(Sender: TObject; var Key: char);
    procedure ePhotographer2ButtonClick(Sender: TObject);
    procedure ePhotographer2KeyPress(Sender: TObject; var Key: char);
    procedure eRemovedBandButtonClick(Sender: TObject);
    procedure eRemovedBandKeyPress(Sender: TObject; var Key: char);
    procedure eRightTarsusButtonClick(Sender: TObject);
    procedure eRightTarsusExit(Sender: TObject);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyExit(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FCapture: TCapture;
    FIndividualId: Integer;
    FSurveyId, FLocalityId, FNetId, FTaxonId, FBandId, FRemovedBandId: Integer;
    FBanderId, FAnnotatorId, FPhotographer1Id, FPhotographer2Id: Integer;
    procedure SetCapture(Value: TCapture);
    procedure SetIndividualId(Value: Integer);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
    procedure AssembleFullName;
    procedure PaintColorBands(aLeg: TBodyPart);
    procedure GetCameras;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Capture: TCapture read FCapture write SetCapture;
    property IndividualId: Integer read FIndividualId write SetIndividualId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtCapture: TedtCapture;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_getvalue, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_taxonomy,
  cbs_validations, cbs_fullnames, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtCapture }

procedure TedtCapture.ApplyDarkMode;
begin
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

procedure TedtCapture.AssembleFullName;
var
  Tax, Ani: Integer;
  Sex, Natur, Cycle: String;
  dt: TDate;
begin
  if dsLink.DataSet.FieldByName('capture_date').IsNull then
    Exit;

  with dsLink.DataSet do
  begin
    Ani := FieldByName('band_id').AsInteger;
    Tax := FieldByName('taxon_id').AsInteger;
    Sex := FieldByName('subject_sex').AsString;
    Natur := FieldByName('capture_type').AsString;
    Cycle := FieldByName('cycle_code').AsString;
    dt := FieldByName('capture_date').AsDateTime;

    FieldByName('full_name').AsString := GetCaptureFullname(dt, Tax, Ani, Sex, Natur, Cycle, False);
  end;
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtCapture.eCycleCodeButtonClick(Sender: TObject);
begin
  MoltCycleDialog(dsLink.DataSet.FieldByName('cycle_code').AsString, dsLink.DataSet, 'cycle_code');
end;

procedure TedtCapture.eAnnotatorButtonClick(Sender: TObject);
begin
  if FindDlg(tbPeople, eAnnotator, FAnnotatorId) then
    eAnnotator.Text := GetName('people', 'acronym', 'person_id', FAnnotatorId);
end;

procedure TedtCapture.eAnnotatorKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbPeople, eAnnotator, FAnnotatorId, Key) then
      eAnnotator.Text := GetName('people', 'acronym', 'person_id', FAnnotatorId);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if FindDlg(tbPeople, eBander, FBanderId) then
    eBander.Text := GetName('people', 'acronym', 'person_id', FBanderId);
end;

procedure TedtCapture.eBanderKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbPeople, eBander, FBanderId, Key) then
      eBander.Text := GetName('people', 'acronym', 'person_id', FBanderId);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  HowAgedDialog(dsLink.DataSet.FieldByName('how_aged').AsString, dsLink.DataSet, 'how_aged');
end;

procedure TedtCapture.eHowSexedButtonClick(Sender: TObject);
begin
  HowAgedDialog(dsLink.DataSet.FieldByName('how_sexed').AsString, dsLink.DataSet, 'how_sexed');
end;

procedure TedtCapture.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TedtCapture.eIndividualKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, FIndividualId, Key);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  EditColorBands(dsLink.DataSet, 'left_leg_below', eLeftTarsus);
  PaintColorBands(bpLeftTarsus);
  if eLeftTarsus.CanSetFocus then
    eLeftTarsus.SetFocus;
end;

procedure TedtCapture.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtCapture.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
    aField := 'longitude';
    Ax := maLongitude;
  end
  else
    if Sender = eLatitude then
    begin
      aField := 'latitude';
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  MoltLimitsDialog(dsLink.DataSet.FieldByName('molt_limits').AsString, dsLink.DataSet, 'molt_limits');
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
    if GetLatLong('nets_effort', 'longitude', 'latitude', 'full_name', 'net_id', FNetId, NetCoord) then
    begin
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if FindDlg(tbPeople, ePhotographer1, FPhotographer1Id) then
    ePhotographer1.Text := GetName('people', 'acronym', 'person_id', FPhotographer1Id);
end;

procedure TedtCapture.ePhotographer1KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbPeople, ePhotographer1, FPhotographer1Id, Key) then
      ePhotographer1.Text := GetName('people', 'acronym', 'person_id', FPhotographer1Id);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if FindDlg(tbPeople, ePhotographer2, FPhotographer2Id) then
    ePhotographer2.Text := GetName('people', 'acronym', 'person_id', FPhotographer2Id);
end;

procedure TedtCapture.ePhotographer2KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbPeople, ePhotographer2, FPhotographer2Id, Key) then
      ePhotographer2.Text := GetName('people', 'acronym', 'person_id', FPhotographer2Id);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  EditColorBands(dsLink.DataSet, 'right_leg_below', eRightTarsus);
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
        FLocalityId := Qry.FieldByName('locality_id').AsInteger;
        eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
        eCaptureDate.Text := DateToStr(Qry.FieldByName('survey_date').AsDateTime);
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  if (Key = #13) and (XSettings.UseEnterAsTab) then
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
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionCapture)]);
    GetRecord;
  end;

  sBox.VertScrollBar.Position := 0;

  PaintColorBands(bpRightTarsus);
  PaintColorBands(bpLeftTarsus);
  eSurvey.SetFocus;

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
    Add('WHERE (active_status = 1)');
    Add('GROUP BY camera_name');
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

procedure TedtCapture.GetRecord;
begin
  eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
  FSurveyId := FCapture.SurveyId;
  eSurvey.Text := GetName('surveys', 'full_name', 'survey_id', FSurveyId);
  FLocalityId := FCapture.LocalityId;
  eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
  eCaptureDate.Text := DateToStr(FCapture.CaptureDate);
  eCaptureTime.Text := TimeToStr(FCapture.CaptureTime);
  FBanderId := FCapture.BanderId;
  eBander.Text := GetName('people', 'acronym', 'person_id', FBanderId);
  FAnnotatorId := FCapture.AnnotatorId;
  eAnnotator.Text := GetName('people', 'acronym', 'person_id', FAnnotatorId);
  case FCapture.CaptureType of
    cptNew:         cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureNew);
    cptRecapture:   cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureRecapture);
    cptSameDay:     cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureSameDay);
    cptChangeBand:  cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureChangeBand);
    cptUnbanded:    cbCaptureType.ItemIndex := cbCaptureType.Items.IndexOf(rsCaptureUnbanded);
  end;
  FNetId := FCapture.NetId;
  eNet.Text := GetName('nets_effort', 'full_name', 'net_id', FNetId);
  eLongitude.Text := FloatToStr(FCapture.Longitude);
  eLatitude.Text := FloatToStr(FCapture.Latitude);
  FTaxonId := FCapture.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
  FBandId := FCapture.BandId;
  eBand.Text := GetName('bands', 'full_name', 'band_id', FBandId);
  FRemovedBandId := FCapture.RemovedBandId;
  eRemovedBand.Text := GetName('bands', 'full_name', 'band_id', FRemovedBandId);
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
  end;
  ckEscaped.Checked := FCapture.Escaped;
  case FCapture.SubjectStatus of
    sstNormal:      cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusNormal);
    sstInjured:     cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusInjured);
    sstWingSprain:  cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusWingSprain);
    sstStressed:    cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusStressed);
    sstDead:        cbStatus.ItemIndex := cbStatus.Items.IndexOf(rsStatusDead);
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
  ePhotographer1.Text := GetName('people', 'acronym', 'person_id', FPhotographer1Id);
  FPhotographer2Id := FCapture.Photographer2Id;
  ePhotographer2.Text := GetName('people', 'acronym', 'person_id', FPhotographer2Id);
  cbCamera.ItemIndex := cbCamera.Items.IndexOf(FCapture.CameraName);
  eStartPhoto.Text := FCapture.StartPhotoNumber;
  eEndPhoto.Text := FCapture.EndPhotoNumber;
  eFieldNumber.Text := FCapture.FieldNumber;
  eHemoglobin.Value := FCapture.Hemoglobin;
  eHematocrit.Value := FCapture.Hematocrit;
  eGlucose.Value := FCapture.Glucose;
end;

function TedtCapture.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (FLocalityId > 0) and
    (FBanderId > 0) and
    (FAnnotatorId > 0) and
    (eCaptureDate.Text <> EmptyStr) and
    (cbCaptureType.ItemIndex >= 0) and
    ((cbCaptureType.Text <> rsCaptureUnbanded) and (FBandId <> 0)) then
    Result := True;

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

    for Cores in BandColors do
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

procedure TedtCapture.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;
  AssembleFullName;

  // Workaround to not post zero value when it is null
  with dsLink.DataSet do
  begin
    if (FieldByName('longitude').AsFloat = 0.0) and (FieldByName('latitude').AsFloat = 0.0)
    then
    begin
      FieldByName('longitude').Clear;
      FieldByName('latitude').Clear;
    end;
  end;

  ModalResult := mrOk;
end;

procedure TedtCapture.SetCapture(Value: TCapture);
begin
  if Assigned(Value) then
    FCapture := Value;
end;

procedure TedtCapture.SetIndividualId(Value: Integer);
begin
  FIndividualId := Value;
  FCapture.IndividualId := Value;
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
    1: FCapture.SubjectAge := ageNestling;
    2: FCapture.SubjectAge := ageFledgling;
    3: FCapture.SubjectAge := ageJuvenile;
    4: FCapture.SubjectAge := ageAdult;
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
    0: FCapture.SubjectSex := sexUnknown;
    1: FCapture.SubjectSex := sexMale;
    2: FCapture.SubjectSex := sexFemale;
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
  //D := dsLink.DataSet;

  // Required fields
  //RequiredIsEmpty(D, tbCaptures, 'locality_id', Msgs);
  //RequiredIsEmpty(D, tbCaptures, 'capture_date', Msgs);
  //RequiredIsEmpty(D, tbCaptures, 'bander_id', Msgs);
  //RequiredIsEmpty(D, tbCaptures, 'annotator_id', Msgs);
  //RequiredIsEmpty(D, tbCaptures, 'capture_type', Msgs);
  //if (D.FieldByName('capture_type').AsString <> 'U') then
  //  RequiredIsEmpty(D, tbCaptures, 'band_id', Msgs);

  // Dates
  ValidDate(eCaptureDate.Text, rsDateCapture, Msgs);
  IsFutureDate(StrToDate(eCaptureDate.Text), Today, rsDateCapture, rsDateToday);

  // Set of values
  ValueInSet(cbCloacalProtuberance.Text, rsCloacalProtuberance, CloacalProtuberanceValues, Msgs);
  ValueInSet(cbBroodPatch.Text, rsBroodPatch, BroodPatchValues, Msgs);
  ValueInSet(cbFat.Text, rsSubcutaneousFat, FatValues, Msgs);
  ValueInSet(cbBodyMolt.Text, rsBodyMolt, BodyMoltValues, Msgs);
  ValueInSet(cbFlightFeatherMolt.Text, rsFlightMolt, FlightMoltValues, Msgs);
  ValueInSet(cbFlightFeatherWear.Text, rsFlightWear, FeatherWearValues, Msgs);
  ValueInSet(cbSkullOssification.Text, rsSkullOssification, SkullValues, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

