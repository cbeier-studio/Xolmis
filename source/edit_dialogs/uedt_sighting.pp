{ Xolmis Sighting Editor dialog

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

unit uedt_sighting;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, Spin, SysUtils, DB, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Character, DBCtrls, StdCtrls, DBEditButton, atshapelinebgra,
  BCPanel, cbs_birds;

type

  { TedtSighting }

  TedtSighting = class(TForm)
    ckCaptured: TCheckBox;
    ckSeen: TCheckBox;
    ckHeard: TCheckBox;
    ckPhotographed: TCheckBox;
    ckAudioRecording: TCheckBox;
    ckIsInEbird: TCheckBox;
    ckNotSurveying: TCheckBox;
    eAdultsTally: TEdit;
    eImmaturesTally: TEdit;
    eNotAgedTally: TEdit;
    eMalesTally: TEdit;
    eFemalesTally: TEdit;
    eNotSexedTally: TEdit;
    eMackinnonListNumber: TEdit;
    eDetectionType: TEditButton;
    eBreedingStatus: TEditButton;
    eTaxon: TEditButton;
    eIndividual: TEditButton;
    eTime: TEdit;
    eObserver: TEditButton;
    eMethod: TEditButton;
    eLocality: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eDate: TEditButton;
    eSurvey: TEditButton;
    dsLink: TDataSource;
    eDistance: TFloatSpinEdit;
    lblDistance: TLabel;
    lblLatitude: TLabel;
    lblDate: TLabel;
    lblLongitude: TLabel;
    lblDetectionType: TLabel;
    lblMackinnonListNumber: TLabel;
    lblMackinnonListNumber1: TLabel;
    lblTime: TLabel;
    lblQuantity: TLabel;
    lblMethod: TLabel;
    lblNotes: TLabel;
    lblNewCapturesTally: TLabel;
    lblRecapturesTally: TLabel;
    lblUnbandedTally: TLabel;
    lblBreedingStatus: TLabel;
    lblIndividual: TLabel;
    lblMalesTally: TLabel;
    lblFemalesTally: TLabel;
    lblNotSexedTally: TLabel;
    lblAdultsTally: TLabel;
    lblImmaturesTally: TLabel;
    lblNotAgedTally: TLabel;
    lblRequester: TLabel;
    lblTaxon: TLabel;
    lblLocality: TLabel;
    lblSurvey: TLabel;
    lblHowWasRecorded: TLabel;
    lblUseDate1: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TMemo;
    pBottom: TPanel;
    pMethod: TPanel;
    pClient: TPanel;
    pCapturesRecapturesUnbanded: TPanel;
    pMalesFemalesNotSexed: TPanel;
    pAdultsImmaturesNotAged: TPanel;
    pHowWasRecorded: TPanel;
    pInEbirdNotSurveying: TPanel;
    pNotes: TPanel;
    pDateTime: TPanel;
    pIndividual: TPanel;
    pObserver: TPanel;
    pTaxon: TPanel;
    pLongitudeLatitude: TPanel;
    pDetectionBreeding: TPanel;
    pMackinnonListNumber: TPanel;
    pLocality: TPanel;
    pSurvey: TBCPanel;
    pQuantityDistance: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    eNewCapturesTally: TSpinEdit;
    eRecapturesTally: TSpinEdit;
    eUnbandedTally: TSpinEdit;
    eQuantity: TSpinEdit;
    procedure eDateKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eBreedingStatusButtonClick(Sender: TObject);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDetectionTypeButtonClick(Sender: TObject);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eMalesTallyKeyPress(Sender: TObject; var Key: char);
    procedure eMethodButtonClick(Sender: TObject);
    procedure eMethodEditingDone(Sender: TObject);
    procedure eMethodKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FSighting: TSighting;
    FSurveyId, FObserverId, FMethodId, FLocalityId, FTaxonId, FIndividualId: Integer;
    procedure SetSighting(Value: TSighting);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Sighting: TSighting read FSighting write SetSighting;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtSighting: TedtSighting;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations,
  cbs_getvalue, cbs_themes, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtSighting }

procedure TedtSighting.ApplyDarkMode;
begin
  pSurvey.Background.Color := clCardBGDefaultDark;
  pSurvey.Border.Color := clCardBGSecondaryDark;

  eSurvey.Images := DMM.iEditsDark;
  eObserver.Images := DMM.iEditsDark;
  eMethod.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eDate.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  eDetectionType.Images := DMM.iEditsDark;
  eBreedingStatus.Images := DMM.iEditsDark;
end;

procedure TedtSighting.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSighting.eBreedingStatusButtonClick(Sender: TObject);
begin
  BreedingDialog(eBreedingStatus);
end;

procedure TedtSighting.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TedtSighting.eDateKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eDetectionTypeButtonClick(Sender: TObject);
begin
  DetectionDialog(dsLink.DataSet.FieldByName('detection_type').AsString, dsLink.DataSet, 'detection_type');
end;

procedure TedtSighting.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TedtSighting.eIndividualKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtSighting.eLocalityKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtSighting.eLongitudeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eMalesTallyKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', 'X', 'x', #8, #13, #27];
var
  EditText: String;
  PosX: Integer;
begin
  FormKeyPress(Sender, Key);

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
  PosX := Pos('X', EditText);

  if (PosX > 0) and (Key in ['0'..'9']) then
  begin
    if (Sender is TEdit) then
      TEdit(Sender).Clear
    else
    if (Sender is TEditButton) then
      TEditButton(Sender).Clear;
    //Key := #0;
    Exit;
  end;

  if (Key in ['X', 'x']) then
  begin
    if (Sender is TEdit) then
      TEdit(Sender).Text := AnsiUpperCase(Key)
    else
    if (Sender is TEditButton) then
      TEditButton(Sender).Text := AnsiUpperCase(Key);
    Key := #0;
    Exit;
  end;
end;

procedure TedtSighting.eMethodButtonClick(Sender: TObject);
begin
  FindDlg(tbMethods, eMethod, FMethodId);
end;

procedure TedtSighting.eMethodEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSighting.eMethodKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbMethods, eMethod, FMethodId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FMethodId := 0;
    eMethod.Clear;
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

procedure TedtSighting.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverId);
end;

procedure TedtSighting.eObserverKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, FObserverId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserverId := 0;
    eObserver.Clear;
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

procedure TedtSighting.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, FSurveyId);
end;

procedure TedtSighting.eSurveyKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtSighting.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, True, FTaxonId, Key);
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

procedure TedtSighting.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSighting.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSighting.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSighting)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSighting)]);
    GetRecord;
  end;
end;

procedure TedtSighting.GetRecord;
begin
  FSurveyId := FSighting.SurveyId;
  eSurvey.Text := GetName('surveys', 'full_name', 'survey_id', FSurveyId);
  FObserverId := FSighting.ObserverId;
  eObserver.Text := GetName('people', 'full_name', 'person_id', FObserverId);
  FMethodId := FSighting.MethodId;
  eMethod.Text := GetName('methods', 'method_name', 'method_id', FMethodId);
  FLocalityId := FSighting.LocalityId;
  eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
  if (FSighting.Longitude <> 0.0) or (FSighting.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FSighting.Longitude);
    eLatitude.Text := FloatToStr(FSighting.Latitude);
  end;
  if not DateIsNull(FSighting.SightingDate) then
    eDate.Text := DateToStr(FSighting.SightingDate);
  if (FSighting.SightingTime <> NullTime) then
    eTime.Text := FormatDateTime('hh:nn', FSighting.SightingTime);
  FTaxonId := FSighting.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
  FIndividualId := FSighting.IndividualId;
  eIndividual.Text := GetName('individuals', 'full_name', 'individual_id', FIndividualId);
  eQuantity.Value := FSighting.SubjectTally;
  eDistance.Value := FSighting.SubjectDistance;
  eDetectionType.Text := FSighting.DetectionType;
  eBreedingStatus.Text := FSighting.BreedingStatus;
  if (FSighting.MackinnonListNumber > 0) then
    eMackinnonListNumber.Text := IntToStr(FSighting.MackinnonListNumber);
  ckCaptured.Checked := FSighting.SubjectCaptured;
  ckSeen.Checked := FSighting.SubjectSeen;
  ckHeard.Checked := FSighting.SubjectHeard;
  ckPhotographed.Checked := FSighting.SubjectPhotographed;
  ckAudioRecording.Checked := FSighting.SubjectRecorded;
  eNewCapturesTally.Value := FSighting.NewCapturesTally;
  eRecapturesTally.Value := FSighting.RecapturesTally;
  eUnbandedTally.Value := FSighting.UnbandedTally;
  eAdultsTally.Text := FSighting.AdultsTally;
  eImmaturesTally.Text := FSighting.ImmatureTally;
  eNotAgedTally.Text := FSighting.NotAgedTally;
  eMalesTally.Text := FSighting.MalesTally;
  eFemalesTally.Text := FSighting.FemalesTally;
  eNotSexedTally.Text := FSighting.NotSexedTally;
  ckIsInEbird.Checked := FSighting.IsOnEbird;
  ckNotSurveying.Checked := FSighting.NotSurveying;
  mNotes.Text := FSighting.Notes;
end;

function TedtSighting.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (FLocalityId > 0) and
    (FMethodId > 0) and
    (FTaxonId > 0) and
    (eDate.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtSighting.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtSighting.SetRecord;
begin
  FSighting.SurveyId            := FSurveyId;
  FSighting.ObserverId          := FObserverId;
  FSighting.MethodId            := FMethodId;
  FSighting.LocalityId          := FLocalityId;
  FSighting.Longitude           := StrToFloatDef(eLongitude.Text, 0.0);
  FSighting.Latitude            := StrToFloatDef(eLatitude.Text, 0.0);
  FSighting.SightingDate        := StrToDateDef(eDate.Text, NullDate);
  FSighting.SightingTime        := StrToTimeDef(eTime.Text, NullTime);
  FSighting.TaxonId             := FTaxonId;
  FSighting.IndividualId        := FIndividualId;
  FSighting.SubjectTally        := eQuantity.Value;
  FSighting.SubjectDistance     := eDistance.Value;
  FSighting.DetectionType       := eDetectionType.Text;
  FSighting.BreedingStatus      := eBreedingStatus.Text;
  FSighting.MackinnonListNumber := StrToIntDef(eMackinnonListNumber.Text, 0);
  FSighting.SubjectCaptured     := ckCaptured.Checked;
  FSighting.SubjectSeen         := ckSeen.Checked;
  FSighting.SubjectHeard        := ckHeard.Checked;
  FSighting.SubjectPhotographed := ckPhotographed.Checked;
  FSighting.SubjectRecorded     := ckAudioRecording.Checked;
  FSighting.NewCapturesTally    := eNewCapturesTally.Value;
  FSighting.RecapturesTally     := eRecapturesTally.Value;
  FSighting.UnbandedTally       := eUnbandedTally.Value;
  FSighting.AdultsTally         := eAdultsTally.Text;
  FSighting.ImmatureTally       := eImmaturesTally.Text;
  FSighting.NotAgedTally        := eNotAgedTally.Text;
  FSighting.MalesTally          := eMalesTally.Text;
  FSighting.FemalesTally        := eFemalesTally.Text;
  FSighting.NotSexedTally       := eNotSexedTally.Text;
  FSighting.IsOnEbird           := ckIsInEbird.Checked;
  FSighting.NotSurveying        := ckNotSurveying.Checked;
  FSighting.Notes               := mNotes.Text;
end;

procedure TedtSighting.SetSighting(Value: TSighting);
begin
  if Assigned(Value) then
    FSighting := Value;
end;

function TedtSighting.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbSightings, 'taxon_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSightings, 'method_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSightings, 'locality_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSightings, 'sighting_date', Msgs);

  // Dates
  if eDate.Text <> EmptyStr then
    ValidDate(eDate.Text, rsCaptionDate, Msgs);

  // Geographical coordinates
  ValueInRange(StrToFloatDef(eLongitude.Text, 0.0), -180.0, 180.0, rsLongitude, Msgs, Msg);
  ValueInRange(StrToFloatDef(eLatitude.Text, 0.0), -90.0, 90.0, rsLatitude, Msgs, Msg);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

