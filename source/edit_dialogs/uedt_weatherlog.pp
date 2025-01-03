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
  Classes, SysUtils, DB, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls, StdCtrls,
  DBEditButton, atshapelinebgra;

type

  { TedtWeatherLog }

  TedtWeatherLog = class(TForm)
    cbPrecipitation: TDBComboBox;
    cbSampleMoment: TDBComboBox;
    dsLink: TDataSource;
    eCloudCover: TDBEdit;
    eAtmosphericPressure: TDBEdit;
    eRelativeHumidity: TDBEdit;
    eTemperature: TDBEdit;
    eSampleDate: TDBEditButton;
    eSampleTime: TDBEdit;
    eWindSpeedBft: TDBEdit;
    eRainfall: TDBEdit;
    eWindSpeedKmh: TDBEdit;
    lblCloudCover: TLabel;
    lblAtmosphericPressure: TLabel;
    lblRelativeHumidity: TLabel;
    lblSampleMoment: TLabel;
    lblSampleMoment1: TLabel;
    lblTemperature: TLabel;
    lblSampleDate: TLabel;
    lblWindSpeedBft: TLabel;
    lblPrecipitation: TLabel;
    lblSampleTime: TLabel;
    lblWindSpeedKmh: TLabel;
    lblRainfall: TLabel;
    lblNotes: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pSampleMoment: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pSampleDateTime: TPanel;
    pWindSpeed: TPanel;
    pPrecipitationRainfall: TPanel;
    pCloudCoverTemperature: TPanel;
    pHumidityPressure: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eSampleDateButtonClick(Sender: TObject);
    procedure eSampleTimeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  edtWeatherLog: TedtWeatherLog;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, udm_main, uDarkStyleParams;

{ TedtWeatherLog }

procedure TedtWeatherLog.ApplyDarkMode;
begin
  eSampleDate.Images := DMM.iEditsDark;
end;

procedure TedtWeatherLog.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtWeatherLog.eSampleDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eSampleDate, dsLink.DataSet, 'sample_date');
end;

procedure TedtWeatherLog.eSampleTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtWeatherLog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not (dsLink.State in [dsInsert, dsEdit]) then
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

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionWeatherLogEntry)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionWeatherLogEntry)]);

  cbSampleMoment.Items.CommaText := rsMomentStart + ',' + rsMomentMiddle + ',' + rsMomentEnd;
  cbPrecipitation.Items.CommaText := rsPrecipitationNone + ',' +
                                     rsPrecipitationFog + ',' +
                                     rsPrecipitationMist + ',' +
                                     rsPrecipitationDrizzle + ',' +
                                     rsPrecipitationRain;
end;

function TedtWeatherLog.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('sample_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('sample_moment').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtWeatherLog.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtWeatherLog.ValidateFields: Boolean;
begin
  Result := True;

end;

initialization
  {$I uedt_weatherlog.lrs}

end.

