{ Xolmis Occurrence Point Editor dialog

  Copyright (C) 2026 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit uedt_poi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, Menus, EditBtn, DateUtils,
  Character, Spin, ATShapeLineBGRA, models_geo;

type

  { TedtPoi }

  TedtPoi = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbCoordinatePrecision: TComboBox;
    dsLink: TDataSource;
    eDate: TEditButton;
    eSurvey: TEditButton;
    eName: TEdit;
    eAltitude: TFloatSpinEdit;
    eIndividual: TEditButton;
    eLatitude: TEditButton;
    eLongitude: TEditButton;
    eSighting: TEditButton;
    eObserver: TEditButton;
    eTaxon: TEditButton;
    eTime: TEdit;
    lblBandStatus4: TLabel;
    lblBandStatus9: TLabel;
    lblCoordinatesPrecision: TLabel;
    lblDate: TLabel;
    lblObserver: TLabel;
    lblSurvey: TLabel;
    lblName: TLabel;
    lblAltitude: TLabel;
    lblIndividual: TLabel;
    lblSighting: TLabel;
    lblNotes: TLabel;
    lblTaxon: TLabel;
    lblTime: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewObserver: TMenuItem;
    pmnNewSighting: TMenuItem;
    pmnNewSurvey: TMenuItem;
    mNotes: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pCoordinatesPrecision: TPanel;
    pDateTime: TPanel;
    pSurvey: TPanel;
    pName: TPanel;
    pAltitudeObserver: TPanel;
    pIndividual: TPanel;
    pmNew: TPopupMenu;
    pmnNewIndividual: TMenuItem;
    pSighting: TPanel;
    pNotes: TPanel;
    pStatus4: TPanel;
    pTaxon: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDateEditingDone(Sender: TObject);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverKeyPress(Sender: TObject; var Key: char);
    procedure eSightingButtonClick(Sender: TObject);
    procedure eSightingKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewIndividualClick(Sender: TObject);
    procedure pmnNewObserverClick(Sender: TObject);
    procedure pmnNewSightingClick(Sender: TObject);
    procedure pmnNewSurveyClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FPoi: TPoi;
    FObserverId, FTaxonId, FIndividualId, FSightingId, FSurveyId: Integer;
    procedure SetPoi(Value: TPoi);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Poi: TPoi read FPoi write SetPoi;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SightingId: Integer read FSightingId write FSightingId;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtPoi: TedtPoi;

implementation

uses
  utils_locale, utils_global, utils_system, utils_dialogs, utils_finddialogs, utils_validations,
  utils_editdialogs, utils_gis, utils_conversions,
  data_types, data_consts, data_getvalue, data_columns, models_record_types, models_taxonomy,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtPoi }

procedure TedtPoi.ApplyDarkMode;
begin
  eObserver.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  eSighting.Images := DMM.iEditsDark;
  eSurvey.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtPoi.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_OCCURRENCE_POINTS);
end;

procedure TedtPoi.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtPoi.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TedtPoi.eDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPoi.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TedtPoi.eIndividualKeyPress(Sender: TObject; var Key: char);
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
    eIndividual.Text := EmptyStr;
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

procedure TedtPoi.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtPoi.eLongitudeKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', ',', '.', '+', '-', #8, #13, #27];
var
  EditText: String;
  PosDecimal: Integer;
  DecimalValue: Extended;
begin
  FormKeyPress(Sender, Key);

  sbSave.Enabled := IsRequiredFilled;

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

procedure TedtPoi.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtPoi.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverId, '', COL_ABBREVIATION);
end;

procedure TedtPoi.eObserverKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, FObserverId, Key, COL_ABBREVIATION);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserverId := 0;
    eObserver.Text := EmptyStr;
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

procedure TedtPoi.eSightingButtonClick(Sender: TObject);
begin
  FindDlg(tbSightings, eSighting, FSightingId);
end;

procedure TedtPoi.eSightingKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSightings, eSighting, FSightingId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSightingId := 0;
    eSighting.Text := EmptyStr;
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

procedure TedtPoi.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, FSurveyId);
end;

procedure TedtPoi.eSurveyKeyPress(Sender: TObject; var Key: char);
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
    eSurvey.Text := EmptyStr;
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

procedure TedtPoi.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtPoi.eTaxonKeyPress(Sender: TObject; var Key: char);
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
    eTaxon.Text := EmptyStr;
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

procedure TedtPoi.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not (sbSave.Enabled) then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtPoi.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtPoi.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  with cbCoordinatePrecision.Items do
  begin
    Add(rsExactCoordinate);
    Add(rsApproximatedCoordinate);
    Add(rsReferenceCoordinate);
  end;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionOccurrencePoint)]);
    if FPoi.TaxonId > 0 then
    begin
      FTaxonId := FPoi.TaxonId;
      eTaxon.Text := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, FTaxonId);
    end;
    if FPoi.IndividualId > 0 then
    begin
      FIndividualId := FPoi.IndividualId;
      eIndividual.Text := GetName(TBL_INDIVIDUALS, COL_FULL_NAME, COL_INDIVIDUAL_ID, FIndividualId);
    end;
    if FPoi.SightingId > 0 then
    begin
      FSightingId := FPoi.SightingId;
      eSighting.Text := GetName(TBL_SIGHTINGS, COL_FULL_NAME, COL_SIGHTING_ID, FSightingId);
    end;
    if FPoi.SurveyId > 0 then
    begin
      FSurveyId := FPoi.SurveyId;
      eSurvey.Text := GetName(TBL_SURVEYS, COL_FULL_NAME, COL_SURVEY_ID, FSurveyId);
    end;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionOccurrencePoint)]);
    GetRecord;
  end;
end;

procedure TedtPoi.GetRecord;
begin
  eName.Text := FPoi.PoiName;
  if not DateIsNull(FPoi.SampleDate) then
    eDate.Text := DateToStr(FPoi.SampleDate);
  if (FPoi.SampleTime <> NullTime) then
    eTime.Text := FormatDateTime('hh:nn', FPoi.SampleTime);
  FObserverId := FPoi.ObserverId;
  eObserver.Text := GetName(TBL_PEOPLE, COL_ABBREVIATION, COL_PERSON_ID, FObserverId);
  if (FPoi.Longitude <> 0.0) or (FPoi.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FPoi.Longitude);
    eLatitude.Text := FloatToStr(FPoi.Latitude);
  end;
  case FPoi.CoordinatePrecision of
    cpExact:        cbCoordinatePrecision.ItemIndex := cbCoordinatePrecision.Items.IndexOf(rsExactCoordinate);
    cpApproximated: cbCoordinatePrecision.ItemIndex := cbCoordinatePrecision.Items.IndexOf(rsApproximatedCoordinate);
    cpReference:    cbCoordinatePrecision.ItemIndex := cbCoordinatePrecision.Items.IndexOf(rsReferenceCoordinate);
  else
    cbCoordinatePrecision.ItemIndex := -1;
  end;
  eAltitude.Value := FPoi.Altitude;
  FTaxonId := FPoi.TaxonId;
  eTaxon.Text := GetName(TBL_ZOO_TAXA, COL_SCIENTIFIC_NAME, COL_TAXON_ID, FTaxonId);
  FIndividualId := FPoi.IndividualId;
  eIndividual.Text := GetName(TBL_INDIVIDUALS, COL_FULL_NAME, COL_INDIVIDUAL_ID, FIndividualId);
  FSightingId := FPoi.SightingId;
  eSighting.Text := GetName(TBL_SIGHTINGS, COL_FULL_NAME, COL_SIGHTING_ID, FSightingId);
  FSurveyId := FPoi.SurveyId;
  eSurvey.Text := GetName(TBL_SURVEYS, COL_FULL_NAME, COL_SURVEY_ID, FSurveyId);
  mNotes.Text := FPoi.Notes;
end;

function TedtPoi.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eName.Text <> EmptyStr) and
    (eDate.Text <> EmptyStr) and
    (FTaxonId > 0) then
    Result := True;
end;

procedure TedtPoi.pmnNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

procedure TedtPoi.pmnNewObserverClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtPoi.pmnNewSightingClick(Sender: TObject);
begin
  EditSighting(DMG.qSightings, FSurveyId, FIndividualId, True);
end;

procedure TedtPoi.pmnNewSurveyClick(Sender: TObject);
begin
  EditSurvey(DMG.qSurveys, 0, True);
end;

procedure TedtPoi.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtPoi.SetPoi(Value: TPoi);
begin
  if Assigned(Value) then
    FPoi := Value;
end;

procedure TedtPoi.SetRecord;
begin
  FPoi.PoiName := eName.Text;
  FPoi.SampleDate := StrToDateDef(eDate.Text, NullDate);
  FPoi.SampleTime := StrToTimeDef(eTime.Text, NullTime);
  FPoi.ObserverId := FObserverId;
  if (Length(eLongitude.Text) > 0) then
    FPoi.Longitude := StrToFloat(eLongitude.Text)
  else
    FPoi.Longitude := 0;
  if (Length(eLatitude.Text) > 0) then
    FPoi.Latitude := StrToFloat(eLatitude.Text)
  else
    FPoi.Latitude := 0;
  FPoi.CoordinatePrecision := StrToCoordinatePrecision(cbCoordinatePrecision.Text);
  FPoi.Altitude := eAltitude.Value;
  FPoi.TaxonId := FTaxonId;
  FPoi.IndividualId := FIndividualId;
  FPoi.SightingId := FSightingId;
  FPoi.SurveyId := FSurveyId;
  FPoi.Notes := mNotes.Text;
end;

function TedtPoi.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eName.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscName]));
  if (eDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscDate]));
  if (FTaxonId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscTaxon]));
  // Conditional required fields
  if (eLongitude.Text <> EmptyStr) and (eLatitude.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscLatitude]));
  if (eLatitude.Text <> EmptyStr) and (eLongitude.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscLongitude]));

  // Dates
  if eDate.Text <> EmptyStr then
    if ValidDate(eDate.Text, rscDate, Msgs) then
      IsFutureDate(StrToDate(eDate.Text), Today, rscDate, LowerCase(rsDateToday), Msgs);

  // Time
  if eTime.Text <> EmptyStr then
    ValidTime(eTime.Text, rscTime, Msgs);

  // Geographical coordinates
  if (eLongitude.Text <> EmptyStr) then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if (eLatitude.Text <> EmptyStr) then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

