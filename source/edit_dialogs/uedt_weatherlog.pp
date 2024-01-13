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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtWeatherLog: TedtWeatherLog;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs;

{ TedtWeatherLog }

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

procedure TedtWeatherLog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
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

end;

initialization
  {$I uedt_weatherlog.lrs}

end.

