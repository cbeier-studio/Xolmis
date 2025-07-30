{ Xolmis Mistnet Effort Editor dialog

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

unit uedt_neteffort;

{$mode objfpc}{$H+}

interface

uses
  Classes, EditBtn, Spin, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DateUtils,
  ExtCtrls, Character, StdCtrls, Buttons, Menus, atshapelinebgra,
  BCPanel, cbs_sampling;

type

  { TedtNetEffort }

  TedtNetEffort = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbNetMesh: TComboBox;
    eNetClose2: TEdit;
    eNetClose3: TEdit;
    eNetClose4: TEdit;
    eNetOpen1: TEdit;
    eNetClose1: TEdit;
    eNetNumber: TEdit;
    eNetOpen2: TEdit;
    eNetOpen3: TEdit;
    eNetOpen4: TEdit;
    eSurvey: TEditButton;
    ePermanentNet: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eDate: TEditButton;
    eNetLength: TFloatSpinEdit;
    eNetHeight: TFloatSpinEdit;
    pmnNewSurvey: TMenuItem;
    pmNew: TPopupMenu;
    txtTotalOpenTime: TLabel;
    txtNetArea: TLabel;
    lblNetClose4: TLabel;
    lblNetOpen4: TLabel;
    lblSurvey: TLabel;
    mNotes: TMemo;
    pNetOpenClose4: TPanel;
    pSurvey: TBCPanel;
    dsLink: TDataSource;
    lblNetNumber: TLabel;
    lblLatitude: TLabel;
    lblNetHeight: TLabel;
    lblNetMesh: TLabel;
    lblTotalOpenTime: TLabel;
    lblNetClose1: TLabel;
    lblNetClose2: TLabel;
    lblNetClose3: TLabel;
    lblNotes: TLabel;
    lblPermanentNet: TLabel;
    lblLongitude: TLabel;
    lblNetLength: TLabel;
    lblNetArea: TLabel;
    lblDate: TLabel;
    lblNetOpen1: TLabel;
    lblNetOpen2: TLabel;
    lblNetOpen3: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pPermanentNetNumber: TPanel;
    pLongLat: TPanel;
    pNetLengthHeight: TPanel;
    pNetMeshArea: TPanel;
    pDateTotalTime: TPanel;
    pNetOpenClose1: TPanel;
    pNetOpenClose2: TPanel;
    pNetOpenClose3: TPanel;
    pNotes: TPanel;
    sbCancel: TButton;
    sBox: TScrollBox;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDateButtonClick(Sender: TObject);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eNetLengthEditingDone(Sender: TObject);
    procedure eNetNumberEditingDone(Sender: TObject);
    procedure eNetNumberKeyPress(Sender: TObject; var Key: char);
    procedure ePermanentNetButtonClick(Sender: TObject);
    procedure ePermanentNetKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewSurveyClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FNetEffort: TNetEffort;
    FSurveyId, FPermanentNetId: Integer;
    procedure SetNetEffort(Value: TNetEffort);
    procedure GetRecord;
    procedure SetRecord;
    procedure GetFullName;
    procedure AutoCalcFields;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property NetEffort: TNetEffort read FNetEffort write SetNetEffort;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtNetEffort: TedtNetEffort;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations, cbs_getvalue,
  cbs_fullnames, cbs_datacolumns, cbs_themes, cbs_dataconst, cbs_editdialogs,
  udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtNetEffort }

procedure TedtNetEffort.ApplyDarkMode;
begin
  pSurvey.Background.Color := clCardBGDefaultDark;
  pSurvey.Border.Color := clCardBGSecondaryDark;

  eSurvey.Images := DMM.iEditsDark;
  ePermanentNet.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eDate.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtNetEffort.AutoCalcFields;
var
  Interval1, Interval2, Interval3, Interval4: Double;
begin
  if (eNetLength.Value > 0) and (eNetHeight.Value > 0) then
  begin
    txtNetArea.Caption := FloatToStr(eNetLength.Value * eNetHeight.Value);
  end;

  Interval1 := 0;
  Interval2 := 0;
  Interval3 := 0;
  Interval4 := 0;

  if (eNetOpen1.Text <> EmptyStr) and (eNetClose1.Text <> EmptyStr) then
    if ValidTime(eNetOpen1.Text, rscOpenTime1) and ValidTime(eNetClose1.Text, rscCloseTime1) then
      Interval1 := HourSpan(StrToTime(eNetOpen1.Text), StrToTime(eNetClose1.Text));
  if (eNetOpen2.Text <> EmptyStr) and (eNetClose2.Text <> EmptyStr) then
    if ValidTime(eNetOpen2.Text, rscOpenTime2) and ValidTime(eNetClose2.Text, rscCloseTime2) then
      Interval2 := HourSpan(StrToTime(eNetOpen2.Text), StrToTime(eNetClose2.Text));
  if (eNetOpen3.Text <> EmptyStr) and (eNetClose3.Text <> EmptyStr) then
    if ValidTime(eNetOpen3.Text, rscOpenTime3) and ValidTime(eNetClose3.Text, rscCloseTime3) then
      Interval3 := HourSpan(StrToTime(eNetOpen3.Text), StrToTime(eNetClose3.Text));
  if (eNetOpen4.Text <> EmptyStr) and (eNetClose4.Text <> EmptyStr) then
    if ValidTime(eNetOpen4.Text, rscOpenTime4) and ValidTime(eNetClose4.Text, rscCloseTime4) then
      Interval4 := HourSpan(StrToTime(eNetOpen4.Text), StrToTime(eNetClose4.Text));

  txtTotalOpenTime.Caption := FloatToStr(Interval1 + Interval2 + Interval3 + Interval4);
end;

procedure TedtNetEffort.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtNetEffort.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetEffort.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TedtNetEffort.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtNetEffort.eLongitudeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtNetEffort.eNetLengthEditingDone(Sender: TObject);
begin
  AutoCalcFields;
end;

procedure TedtNetEffort.eNetNumberEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetEffort.eNetNumberKeyPress(Sender: TObject; var Key: char);
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

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetEffort.ePermanentNetButtonClick(Sender: TObject);
begin
  FindDlg(tbPermanentNets, ePermanentNet, FPermanentNetId);
  FNetEffort.PermanentNetId := FPermanentNetId;
end;

procedure TedtNetEffort.ePermanentNetKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPermanentNets, ePermanentNet, FPermanentNetId, Key);
    FNetEffort.PermanentNetId := FPermanentNetId;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FPermanentNetId := 0;
    FNetEffort.PermanentNetId := 0;
    ePermanentNet.Text := EmptyStr;
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

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetEffort.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, FSurveyId);
  FNetEffort.SurveyId := FSurveyId;
end;

procedure TedtNetEffort.eSurveyKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSurveys, eSurvey, FSurveyId, Key);
    FNetEffort.SurveyId := FSurveyId;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSurveyId := 0;
    FNetEffort.SurveyId := 0;
    eSurvey.Text := EmptyStr;
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

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetEffort.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtNetEffort.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNetEffort.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionMistnet)]);
    if not DateIsNull(FNetEffort.SampleDate) then
      eDate.Text := DateToStr(FNetEffort.SampleDate);
    AutoCalcFields;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionMistnet)]);
    GetRecord;
    AutoCalcFields;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtNetEffort.GetFullName;
begin
  FNetEffort.FullName := GetNetEffortFullname(FNetEffort.SampleDate, FNetEffort.NetStationId, FNetEffort.NetNumber);
end;

procedure TedtNetEffort.GetRecord;
begin
  FSurveyId := FNetEffort.SurveyId;
  eSurvey.Text := GetName('surveys', COL_FULL_NAME, COL_SURVEY_ID, FSurveyId);
  FPermanentNetId := FNetEffort.PermanentNetId;
  ePermanentNet.Text := GetName('permanent_nets', COL_FULL_NAME, COL_PERMANENT_NET_ID, FPermanentNetId);
  eNetNumber.Text := IntToStr(FNetEffort.NetNumber);
  if (FNetEffort.Longitude <> 0.0) and (FNetEffort.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FNetEffort.Longitude);
    eLatitude.Text := FloatToStr(FNetEffort.Latitude);
  end;
  eNetLength.Value := FNetEffort.NetLength;
  eNetHeight.Value := FNetEffort.NetHeight;
  cbNetMesh.ItemIndex := cbNetMesh.Items.IndexOf(FNetEffort.NetMesh);
  txtNetArea.Caption := FloatToStr(FNetEffort.NetArea);
  if not DateIsNull(FNetEffort.SampleDate) then
    eDate.Text := DateToStr(FNetEffort.SampleDate);
  txtTotalOpenTime.Caption := FloatToStr(FNetEffort.TotalOpenTime);
  if not TimeIsNull(FNetEffort.NetOpen1) then
    eNetOpen1.Text := FormatDateTime('hh:nn', FNetEffort.NetOpen1);
  if not TimeIsNull(FNetEffort.NetClose1) then
    eNetClose1.Text := FormatDateTime('hh:nn', FNetEffort.NetClose1);
  if not TimeIsNull(FNetEffort.NetOpen2) then
    eNetOpen2.Text := FormatDateTime('hh:nn', FNetEffort.NetOpen2);
  if not TimeIsNull(FNetEffort.NetClose2) then
    eNetClose2.Text := FormatDateTime('hh:nn', FNetEffort.NetClose2);
  if not TimeIsNull(FNetEffort.NetOpen3) then
    eNetOpen3.Text := FormatDateTime('hh:nn', FNetEffort.NetOpen3);
  if not TimeIsNull(FNetEffort.NetClose3) then
    eNetClose3.Text := FormatDateTime('hh:nn', FNetEffort.NetClose3);
  if not TimeIsNull(FNetEffort.NetOpen4) then
    eNetOpen4.Text := FormatDateTime('hh:nn', FNetEffort.NetOpen4);
  if not TimeIsNull(FNetEffort.NetClose4) then
    eNetClose4.Text := FormatDateTime('hh:nn', FNetEffort.NetClose4);
  mNotes.Text := FNetEffort.Notes;
end;

function TedtNetEffort.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('net_number').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('sample_date').IsNull = False) and
  //  (dsLink.DataSet.FieldByName('net_open_1').IsNull = False) and
  //  (dsLink.DataSet.FieldByName('net_close_1').IsNull = False) then
  if (eNetNumber.Text <> EmptyStr) and
    (eDate.Text <> EmptyStr) and
    (eNetOpen1.Text <> EmptyStr) and
    (eNetClose1.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtNetEffort.pmnNewSurveyClick(Sender: TObject);
begin
  EditSurvey(DMG.qSurveys, 0, True);
end;

procedure TedtNetEffort.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtNetEffort.SetNetEffort(Value: TNetEffort);
begin
  if Assigned(Value) then
    FNetEffort := Value;
end;

procedure TedtNetEffort.SetRecord;
begin
  FNetEffort.SurveyId       := FSurveyId;
  FNetEffort.PermanentNetId := FPermanentNetId;
  if eNetNumber.Text <> EmptyStr then
    FNetEffort.NetNumber    := StrToInt(eNetNumber.Text)
  else
    FNetEffort.NetNumber    := 0;
  if eLongitude.Text <> EmptyStr then
    FNetEffort.Longitude    := StrToFloat(eLongitude.Text)
  else
    FNetEffort.Longitude    := 0;
  if eLatitude.Text <> EmptyStr then
    FNetEffort.Latitude     := StrToFloat(eLatitude.Text)
  else
    FNetEffort.Latitude    := 0;
  FNetEffort.NetLength      := eNetLength.Value;
  FNetEffort.NetHeight      := eNetHeight.Value;
  FNetEffort.NetMesh        := cbNetMesh.Text;
  FNetEffort.SampleDate     := StrToDate(eDate.Text);
  FNetEffort.NetOpen1       := StrToTime(eNetOpen1.Text);
  FNetEffort.NetClose1      := StrToTime(eNetClose1.Text);
  if eNetOpen2.Text <> EmptyStr then
    FNetEffort.NetOpen2     := StrToTime(eNetOpen2.Text)
  else
    FNetEffort.NetOpen2     := StrToTime('00:00:00');
  if eNetClose2.Text <> EmptyStr then
    FNetEffort.NetClose2    := StrToTime(eNetClose2.Text)
  else
    FNetEffort.NetClose2     := StrToTime('00:00:00');
  if eNetOpen3.Text <> EmptyStr then
    FNetEffort.NetOpen3     := StrToTime(eNetOpen3.Text)
  else
    FNetEffort.NetOpen3     := StrToTime('00:00:00');
  if eNetClose3.Text <> EmptyStr then
    FNetEffort.NetClose3    := StrToTime(eNetClose3.Text)
  else
    FNetEffort.NetClose3     := StrToTime('00:00:00');
  if eNetOpen4.Text <> EmptyStr then
    FNetEffort.NetOpen4     := StrToTime(eNetOpen4.Text)
  else
    FNetEffort.NetOpen4     := StrToTime('00:00:00');
  if eNetClose4.Text <> EmptyStr then
    FNetEffort.NetClose4    := StrToTime(eNetClose4.Text)
  else
    FNetEffort.NetClose4     := StrToTime('00:00:00');
  FNetEffort.Notes          := mNotes.Text;

  GetFullName;
end;

function TedtNetEffort.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'revision_date', Msgs);
  //RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'net_number', Msgs);
  //RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'net_open_1', Msgs);
  //RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'net_close_1', Msgs);

  // Geographical coordinates
  if eLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);
  //CoordenadaIsOk(DSIO.Dataset, 'longitude', maLongitude, Msgs);
  //CoordenadaIsOk(DSIO.Dataset, 'latitude', maLatitude, Msgs);

  // Date and time
  ValidDate(eDate.Text, rscDate, Msgs);
  ValidTime(eNetOpen1.Text, rscOpenTime1, Msgs);
  ValidTime(eNetClose1.Text, rscCloseTime1, Msgs);
  if eNetOpen2.Text <> EmptyStr then
    ValidTime(eNetOpen2.Text, rscOpenTime2, Msgs);
  if eNetClose2.Text <> EmptyStr then
    ValidTime(eNetClose2.Text, rscCloseTime2, Msgs);
  if eNetOpen3.Text <> EmptyStr then
    ValidTime(eNetOpen3.Text, rscOpenTime3, Msgs);
  if eNetClose3.Text <> EmptyStr then
    ValidTime(eNetClose3.Text, rscCloseTime3, Msgs);
  if eNetOpen4.Text <> EmptyStr then
    ValidTime(eNetOpen4.Text, rscOpenTime4, Msgs);
  if eNetClose4.Text <> EmptyStr then
    ValidTime(eNetClose4.Text, rscCloseTime4, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

