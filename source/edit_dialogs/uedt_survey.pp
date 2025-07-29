{ Xolmis Survey Editor dialog

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

unit uedt_survey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, Spin, SysUtils, Character, DB, SQLDB, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, atshapelinebgra, BCPanel, cbs_sampling;

type

  { TedtSurvey }

  TedtSurvey = class(TForm)
    eSampleId: TEdit;
    eEndLatitude: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eEndLongitude: TEditButton;
    eStartTime: TEdit;
    eEndTime: TEdit;
    eDate: TEditButton;
    eExpedition: TEditButton;
    eMethod: TEditButton;
    eLocality: TEditButton;
    eNetStation: TEditButton;
    eProject: TEditButton;
    dsLink: TDataSource;
    eArea: TFloatSpinEdit;
    eDistance: TFloatSpinEdit;
    txtNetEffort: TLabel;
    lblNetStation: TLabel;
    lblEndLatitude: TLabel;
    lblDistance: TLabel;
    lblNetEffort: TLabel;
    lblExpedition: TLabel;
    lblSurveyDate: TLabel;
    lblStartLongitude: TLabel;
    lblEndLongitude: TLabel;
    lblArea: TLabel;
    lblNetsTotal: TLabel;
    lblDuration: TLabel;
    lblStartLatitude: TLabel;
    lblNotes: TLabel;
    lblHabitat: TLabel;
    lblNetRounds: TLabel;
    lblStartTime: TLabel;
    lblMethod: TLabel;
    lblObserversTally: TLabel;
    lblEndTime: TLabel;
    lblSampleId: TLabel;
    lblLocality: TLabel;
    lblProject: TLabel;
    lineBottom: TShapeLineBGRA;
    mHabitat: TMemo;
    mNotes: TMemo;
    mNetRounds: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pHabitat: TPanel;
    pNetRounds: TPanel;
    pStartEndDate: TPanel;
    pMethod: TPanel;
    pObserversSampleId: TPanel;
    pNetStation: TPanel;
    pDateDuration: TPanel;
    pStartCoordinate: TPanel;
    pEndCoordinate: TPanel;
    pAreaDistance: TPanel;
    pNetsTotalEffort: TPanel;
    pLocality: TPanel;
    pProject: TPanel;
    pExpedition: TBCPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    eDuration: TSpinEdit;
    eObserversTally: TSpinEdit;
    eTotalNets: TSpinEdit;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDateEditingDone(Sender: TObject);
    procedure eDurationKeyPress(Sender: TObject; var Key: char);
    procedure eEndLongitudeButtonClick(Sender: TObject);
    procedure eExpeditionButtonClick(Sender: TObject);
    procedure eExpeditionKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eMethodButtonClick(Sender: TObject);
    procedure eMethodKeyPress(Sender: TObject; var Key: char);
    procedure eNetStationButtonClick(Sender: TObject);
    procedure eNetStationKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FSurvey: TSurvey;
    FExpeditionId, FMethodId, FLocalityId, FSamplingPlotId, FProjectId: Integer;
    procedure SetSurvey(Value: TSurvey);
    procedure SetExpeditionId(Value: Integer);
    procedure GetRecord;
    procedure SetRecord;
    procedure GetFullName;
    procedure AutoCalcFields;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Survey: TSurvey read FSurvey write SetSurvey;
    property ExpeditionId: Integer read FExpeditionId write SetExpeditionId;
  end;

var
  edtSurvey: TedtSurvey;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations, cbs_themes,
  cbs_getvalue, cbs_fullnames, cbs_dataconst, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtSurvey }

procedure TedtSurvey.ApplyDarkMode;
begin
  pExpedition.Background.Color := clSolidBGSecondaryDark;
  pExpedition.Border.Color := clSystemSolidNeutralFGDark;

  eExpedition.Images := DMM.iEditsDark;
  eDate.Images := DMM.iEditsDark;
  eMethod.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eNetStation.Images := DMM.iEditsDark;
  eProject.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eEndLongitude.Images := DMM.iEditsDark;
  eEndLatitude.Images := DMM.iEditsDark;
end;

procedure TedtSurvey.AutoCalcFields;
var
  Qry: TSQLQuery;
begin
  if not FIsNew then
  begin
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      SQLConnection := DMM.sqlCon;
      SQLTransaction := DMM.sqlTrans;

      Add('SELECT CAST(SUM(net_area * open_time_total) AS REAL)');
      Add('FROM nets_effort');
      Add('WHERE (survey_id = :survey_id) AND (active_status = 1)');
      ParamByName('survey_id').AsInteger := FSurvey.Id;
      Open;
      if (RecordCount > 0) and not (Fields[0].IsNull) then
        txtNetEffort.Caption := FloatToStr(Fields[0].AsFloat)
      else
        txtNetEffort.Caption := '0';
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end else
    txtNetEffort.Caption := '0';
end;

procedure TedtSurvey.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSurvey.eDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eDate.Text, eDate, Dt);
end;

procedure TedtSurvey.eDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSurvey.eDurationKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // <ENTER/RETURN> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eEndLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eEndLongitude, eEndLatitude);
end;

procedure TedtSurvey.eExpeditionButtonClick(Sender: TObject);
begin
  FindDlg(tbExpeditions, eExpedition, FExpeditionId);
end;

procedure TedtSurvey.eExpeditionKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbExpeditions, eExpedition, FExpeditionId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FExpeditionId := 0;
    eExpedition.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtSurvey.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtSurvey.eLongitudeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSurvey.eMethodButtonClick(Sender: TObject);
begin
  FindDlg(tbMethods, eMethod, FMethodId);
end;

procedure TedtSurvey.eMethodKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbMethods, eMethod, FMethodId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FMethodId := 0;
    eMethod.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eNetStationButtonClick(Sender: TObject);
begin
  FindDlg(tbSamplingPlots, eNetStation, FSamplingPlotId);
end;

procedure TedtSurvey.eNetStationKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbSamplingPlots, eNetStation, FSamplingPlotId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSamplingPlotId := 0;
    eNetStation.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TedtSurvey.eProjectKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, FProjectId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FProjectId := 0;
    eProject.Text := EmptyStr;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSurvey.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSurvey.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  pExpedition.Visible := FExpeditionId = 0;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSurvey)]);
    AutoCalcFields;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSurvey)]);
    GetRecord;
    AutoCalcFields;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtSurvey.GetFullName;
begin
  FSurvey.FullName := GetSurveyFullname(FSurvey.SurveyDate, FLocalityId, FMethodId, FSamplingPlotId, FSurvey.SampleId);
end;

procedure TedtSurvey.GetRecord;
begin
  FExpeditionId := FSurvey.ExpeditionId;
  eExpedition.Text := GetName('expeditions', COL_EXPEDITION_NAME, COL_EXPEDITION_ID, FExpeditionId);
  if not DateIsNull(FSurvey.SurveyDate) then
    eDate.Text := DateToStr(FSurvey.SurveyDate);
  eDuration.Value := FSurvey.Duration;
  if (FSurvey.StartTime <> NullTime) then
    eStartTime.Text := FormatDateTime('hh:nn', FSurvey.StartTime);
  if (FSurvey.EndTime <> NullTime) then
    eEndTime.Text := FormatDateTime('hh:nn', FSurvey.EndTime);
  FMethodId := FSurvey.MethodId;
  eMethod.Text := GetName('methods', COL_METHOD_NAME, COL_METHOD_ID, FMethodId);
  FLocalityId := FSurvey.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
  FSamplingPlotId := FSurvey.NetStationId;
  eNetStation.Text := GetName('sampling_plots', COL_FULL_NAME, COL_SAMPLING_PLOT_ID, FSamplingPlotId);
  FProjectId := FSurvey.ProjectId;
  eProject.Text := GetName('projects', COL_SHORT_TITLE, COL_PROJECT_ID, FProjectId);
  if (FSurvey.StartLongitude <> 0.0) or (FSurvey.StartLatitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FSurvey.StartLongitude);
    eLatitude.Text := FloatToStr(FSurvey.StartLatitude);
  end;
  if (FSurvey.EndLongitude <> 0.0) or (FSurvey.EndLatitude <> 0.0) then
  begin
    eEndLongitude.Text := FloatToStr(FSurvey.EndLongitude);
    eEndLatitude.Text := FloatToStr(FSurvey.EndLatitude);
  end;
  eObserversTally.Value := FSurvey.ObserversTally;
  eSampleId.Text := FSurvey.SampleId;
  eArea.Value := FSurvey.TotalArea;
  eDistance.Value := FSurvey.TotalDistance;
  eTotalNets.Value := FSurvey.TotalNets;
  mHabitat.Text := FSurvey.Habitat;
  mNetRounds.Text := FSurvey.NetRounds;
  mNotes.Text := FSurvey.Notes;
end;

function TedtSurvey.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('method_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('survey_date').IsNull = False) then
  if (eDate.Text <> EmptyStr) and
    (FMethodId > 0) and
    (FLocalityId > 0) then
    Result := True;
end;

procedure TedtSurvey.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtSurvey.SetExpeditionId(Value: Integer);
begin
  if Value > 0 then
  begin
    FExpeditionId := Value;
    FSurvey.ExpeditionId := Value;
  end;
end;

procedure TedtSurvey.SetRecord;
begin
  FSurvey.ExpeditionId   := FExpeditionId;
  FSurvey.SurveyDate     := StrToDate(eDate.Text);
  FSurvey.Duration       := eDuration.Value;
  FSurvey.StartTime      := StrToTime(eStartTime.Text);
  FSurvey.EndTime        := StrToTime(eEndTime.Text);
  FSurvey.MethodId       := FMethodId;
  FSurvey.LocalityId     := FLocalityId;
  FSurvey.NetStationId   := FSamplingPlotId;
  FSurvey.ProjectId      := FProjectId;
  FSurvey.StartLongitude := StrToFloatDef(eLongitude.Text, 0.0);
  FSurvey.StartLatitude  := StrToFloatDef(eLatitude.Text, 0.0);
  FSurvey.EndLongitude   := StrToFloatDef(eEndLongitude.Text, 0.0);
  FSurvey.EndLatitude    := StrToFloatDef(eEndLatitude.Text, 0.0);
  FSurvey.ObserversTally := eObserversTally.Value;
  FSurvey.SampleId       := eSampleId.Text;
  FSurvey.TotalArea      := eArea.Value;
  FSurvey.TotalDistance  := eDistance.Value;
  FSurvey.TotalNets      := eTotalNets.Value;
  FSurvey.Habitat        := mHabitat.Text;
  FSurvey.NetRounds      := mNetRounds.Text;
  FSurvey.Notes          := mNotes.Text;

  GetFullName;
end;

procedure TedtSurvey.SetSurvey(Value: TSurvey);
begin
  if Assigned(Value) then
    FSurvey := Value;
end;

function TedtSurvey.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'survey_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'locality_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'method_id', Msgs);

  // Duplicated record
  // RegistroDuplicado(WorkingTable.TableName,'PES_NOME',cdsConsultaPES_NOME.AsWideString,cdsConsultaPES_CODIGO.AsInteger);

  // Foreign keys
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
  //  rsCaptionLocality, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('municipality_id').AsInteger,
  //  rsCaptionMunicipality, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('state_id').AsInteger,
  //  rsCaptionState, Msgs);
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('country_id').AsInteger,
  //  rsCaptionCountry, Msgs);
  //ForeignValueExists(tbMethods, 'method_id', dsLink.DataSet.FieldByName('method_id').AsInteger,
  //  rsCaptionMethod, Msgs);
  //ForeignValueExists(tbSamplingPlots, 'net_station_id', dsLink.DataSet.FieldByName('net_station_id').AsInteger,
  //  rsCaptionSamplingPlot, Msgs);
  //ForeignValueExists(tbProjects, 'project_id', dsLink.DataSet.FieldByName('project_id').AsInteger,
  //  rsCaptionProject, Msgs);

  // Dates
  if eDate.Text <> EmptyStr then
    ValidDate(eDate.Text, rsCaptionDate, Msgs);

  // Geographical coordinates
  if eLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);
  if eEndLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eEndLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eEndLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eEndLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

