{ Xolmis Weather Log Editor dialog

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

unit uedt_weatherlog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, MaskEdit, Spin, SysUtils, DB, LResources, Forms, Controls, DateUtils,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, atshapelinebgra,
  models_sampling;

type

  { TedtWeatherLog }

  TedtWeatherLog = class(TForm)
    btnHelp: TSpeedButton;
    cbSampleMoment: TComboBox;
    cbPrecipitation: TComboBox;
    cbWindDirection: TComboBox;
    dsLink: TDataSource;
    eSampleTime: TEdit;
    eSampleDate: TEditButton;
    eTemperature: TFloatSpinEdit;
    eWindSpeedKmh: TFloatSpinEdit;
    eRelativeHumidity: TFloatSpinEdit;
    eAtmosphericPressure: TFloatSpinEdit;
    lblCloudCover: TLabel;
    lblAtmosphericPressure: TLabel;
    lblRelativeHumidity: TLabel;
    lblSampleMoment: TLabel;
    lblSampleMoment1: TLabel;
    lblWindDirection: TLabel;
    lblWindDirection1: TLabel;
    lblTemperature: TLabel;
    lblSampleDate: TLabel;
    lblWindSpeedBft: TLabel;
    lblPrecipitation: TLabel;
    lblSampleTime: TLabel;
    lblWindSpeedKmh: TLabel;
    lblRainfall: TLabel;
    lblNotes: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TMemo;
    pSampleMoment: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pSampleDateTime: TPanel;
    pWindDirection: TPanel;
    pWindSpeed: TPanel;
    pPrecipitationRainfall: TPanel;
    pCloudCoverTemperature: TPanel;
    pHumidityPressure: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    eCloudCover: TSpinEdit;
    eRainfall: TSpinEdit;
    eWindSpeedBft: TSpinEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eSampleDateButtonClick(Sender: TObject);
    procedure eSampleTimeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FWeather: TWeatherLog;
    FSurveyId: Integer;
    procedure SetWeather(Value: TWeatherLog);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property WeatherLog: TWeatherLog read FWeather write SetWeather;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtWeatherLog: TedtWeatherLog;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_validations,
  data_columns, data_consts, models_record_types,
  udm_main, uDarkStyleParams;

{ TedtWeatherLog }

procedure TedtWeatherLog.ApplyDarkMode;
begin
  eSampleDate.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtWeatherLog.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_SURVEYS);
end;

procedure TedtWeatherLog.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtWeatherLog.eSampleDateButtonClick(Sender: TObject);
var
  Dt: TDateTime;
begin
  CalendarDlg(eSampleDate.Text, eSampleDate, Dt);
end;

procedure TedtWeatherLog.eSampleTimeKeyPress(Sender: TObject; var Key: char);
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

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtWeatherLog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtWeatherLog.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtWeatherLog.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbSampleMoment.Items.CommaText := rsMomentStart + ',' + rsMomentMiddle + ',' + rsMomentEnd;
  cbPrecipitation.Items.CommaText := rsPrecipitationNone + ',' +
                                     rsPrecipitationFog + ',' +
                                     rsPrecipitationMist + ',' +
                                     rsPrecipitationDrizzle + ',' +
                                     rsPrecipitationRain;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionWeatherLogEntry)]);
    if not DateIsNull(FWeather.SampleDate) then
      eSampleDate.Text := DateToStr(FWeather.SampleDate);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionWeatherLogEntry)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtWeatherLog.GetRecord;
begin
  if not DateIsNull(FWeather.SampleDate) then
    eSampleDate.Text := DateToStr(FWeather.SampleDate);
  if not TimeIsNull(FWeather.SampleTime) then
    eSampleTime.Text := FormatDateTime('hh:nn', FWeather.SampleTime);
  case FWeather.SampleMoment of
    wmStart:  cbSampleMoment.ItemIndex := 0;
    wmMiddle: cbSampleMoment.ItemIndex := 1;
    wmEnd:    cbSampleMoment.ItemIndex := 2;
  else
    cbSampleMoment.ItemIndex := -1;
  end;
  eCloudCover.Value := FWeather.CloudCover;
  eTemperature.Value := FWeather.Temperature;
  case FWeather.Precipitation of
    wpNone:    cbPrecipitation.ItemIndex := 0;
    wpFog:     cbPrecipitation.ItemIndex := 1;
    wpMist:    cbPrecipitation.ItemIndex := 2;
    wpDrizzle: cbPrecipitation.ItemIndex := 3;
    wpRain:    cbPrecipitation.ItemIndex := 4;
  else
    cbPrecipitation.ItemIndex := -1;
  end;
  eRainfall.Value := FWeather.Rainfall;
  eWindSpeedBft.Value := FWeather.WindSpeedBft;
  eWindSpeedKmh.Value := FWeather.WindSpeedKmH;
  if (FWeather.WindDirection <> EmptyStr) then
    cbWindDirection.ItemIndex := cbWindDirection.Items.IndexOf(FWeather.WindDirection);
  eRelativeHumidity.Value := FWeather.RelativeHumidity;
  eAtmosphericPressure.Value := FWeather.AtmosphericPressure;
  mNotes.Text := FWeather.Notes;
end;

function TedtWeatherLog.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('sample_date').IsNull = False) and
  //  (dsLink.DataSet.FieldByName('sample_moment').AsString <> EmptyStr) then
  if (eSampleDate.Text <> EmptyStr) and
    (eSampleTime.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtWeatherLog.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtWeatherLog.SetRecord;
begin
  FWeather.SurveyId := FSurveyId;
  FWeather.SampleDate := StrToDate(eSampleDate.Text);
  FWeather.SampleTime := StrToTime(eSampleTime.Text);
  case cbSampleMoment.ItemIndex of
    0: FWeather.SampleMoment := wmStart;
    1: FWeather.SampleMoment := wmMiddle;
    2: FWeather.SampleMoment := wmEnd;
  else
    FWeather.SampleMoment := wmNone;
  end;
  FWeather.CloudCover  := eCloudCover.Value;
  FWeather.Temperature := eTemperature.Value;
  case cbPrecipitation.ItemIndex of
    0: FWeather.Precipitation := wpNone;
    1: FWeather.Precipitation := wpFog;
    2: FWeather.Precipitation := wpMist;
    3: FWeather.Precipitation := wpDrizzle;
    4: FWeather.Precipitation := wpRain;
  else
    FWeather.Precipitation := wpEmpty;
  end;
  FWeather.Rainfall            := eRainfall.Value;
  FWeather.WindSpeedBft        := eWindSpeedBft.Value;
  FWeather.WindSpeedKmH        := eWindSpeedKmh.Value;
  FWeather.WindDirection       := cbWindDirection.Text;
  FWeather.RelativeHumidity    := eRelativeHumidity.Value;
  FWeather.AtmosphericPressure := eAtmosphericPressure.Value;
  FWeather.Notes               := mNotes.Text;
end;

procedure TedtWeatherLog.SetWeather(Value: TWeatherLog);
begin
  if Assigned(Value) then
    FWeather := Value;
end;

function TedtWeatherLog.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (eSampleDate.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscDate]));
  if (cbSampleMoment.ItemIndex < 0) then
    Msgs.Add(Format(rsRequiredField, [rscMoment]));
  { #todo : Required at least one measurement of weather log }

  // Dates
  if (eSampleDate.Text <> EmptyStr) then
    if ValidDate(eSampleDate.Text, rscDate, Msgs) then
      IsFutureDate(StrToDate(eSampleDate.Text), Today, rscDate, rsDateToday, Msgs);

  // Time
  if (eSampleTime.Text <> EmptyStr) then
    ValidTime(eSampleTime.Text, rscTime, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_weatherlog.lrs}

end.

