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
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DBCtrls, DBEditButton, atshapelinebgra, BCPanel;

type

  { TedtSurvey }

  TedtSurvey = class(TForm)
    eDate: TDBEditButton;
    eDuration: TDBEdit;
    eMethod: TDBEditButton;
    eObserversTally: TDBEdit;
    eLatitude: TDBEditButton;
    eEndLatitude: TDBEditButton;
    eLongitude: TDBEditButton;
    eEndLongitude: TDBEditButton;
    eArea: TDBEdit;
    eDistance: TDBEdit;
    eExpedition: TDBEditButton;
    eTotalNets: TDBEdit;
    eSampleId: TDBEdit;
    eStartTime: TDBEdit;
    eEndTime: TDBEdit;
    eLocality: TDBEditButton;
    eNetStation: TDBEditButton;
    eProject: TDBEditButton;
    dsLink: TDataSource;
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
    mNotes: TDBMemo;
    mHabitat: TDBMemo;
    mNetRounds: TDBMemo;
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
    txtNetEffort: TDBText;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDurationKeyPress(Sender: TObject; var Key: char);
    procedure eEndLongitudeButtonClick(Sender: TObject);
    procedure eExpeditionButtonClick(Sender: TObject);
    procedure eExpeditionDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eMethodButtonClick(Sender: TObject);
    procedure eMethodKeyPress(Sender: TObject; var Key: char);
    procedure eNetStationButtonClick(Sender: TObject);
    procedure eNetStationDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtSurvey: TedtSurvey;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations, cbs_themes,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtSurvey }

procedure TedtSurvey.ApplyDarkMode;
begin
  pExpedition.Background.Color := clCardBGDefaultDark;
  pExpedition.Border.Color := clCardBGSecondaryDark;

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

procedure TedtSurvey.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSurvey.eDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDate, dsLink.DataSet, 'survey_date');
end;

procedure TedtSurvey.eDurationKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // <ENTER/RETURN> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eEndLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'end_longitude', 'end_latitude');
end;

procedure TedtSurvey.eExpeditionButtonClick(Sender: TObject);
begin
  FindDlg(tbExpeditions, eExpedition, dsLink.DataSet, 'expedition_id', 'expedition_name');
end;

procedure TedtSurvey.eExpeditionDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbExpeditions, eExpedition, dsLink.DataSet, 'expedition_id', 'expedition_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('expedition_id').Clear;
    dsLink.DataSet.FieldByName('expedition_name').Clear;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtSurvey.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('locality_id').Clear;
    dsLink.DataSet.FieldByName('locality_name').Clear;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'start_longitude', 'start_latitude');
end;

procedure TedtSurvey.eMethodButtonClick(Sender: TObject);
begin
  FindDlg(tbMethods, eMethod, dsLink.DataSet, 'method_id', 'method_name');
end;

procedure TedtSurvey.eMethodKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbMethods, eMethod, dsLink.DataSet, 'method_id', 'method_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('method_id').Clear;
    dsLink.DataSet.FieldByName('method_name').Clear;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eNetStationButtonClick(Sender: TObject);
begin
  FindDlg(tbSamplingPlots, eNetStation, dsLink.DataSet, 'net_station_id', 'net_station_name');
end;

procedure TedtSurvey.eNetStationDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbSamplingPlots, eNetStation, dsLink.DataSet, 'net_station_id', 'net_station_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('net_station_id').Clear;
    dsLink.DataSet.FieldByName('net_station_name').Clear;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSurvey.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name');
end;

procedure TedtSurvey.eProjectDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  // Alphabetic search in numeric fields
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('project_id').Clear;
    dsLink.DataSet.FieldByName('project_name').Clear;
    Key := #0;
  end;

  // <ENTER> Key
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
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
    if not (dsLink.State in [dsInsert, dsEdit]) then
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

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSurvey)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSurvey)]);
end;

function TedtSurvey.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('method_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('survey_date').IsNull = False) then
    Result := True;
end;

procedure TedtSurvey.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtSurvey.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'survey_date', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'locality_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSurveys, 'method_id', Msgs);

  // Duplicated record
  // RegistroDuplicado(WorkingTable.TableName,'PES_NOME',cdsConsultaPES_NOME.AsWideString,cdsConsultaPES_CODIGO.AsInteger);

  // Foreign keys
  ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
    rsCaptionLocality, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('municipality_id').AsInteger,
    rsCaptionMunicipality, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('state_id').AsInteger,
    rsCaptionState, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('country_id').AsInteger,
    rsCaptionCountry, Msgs);
  ForeignValueExists(tbMethods, 'method_id', dsLink.DataSet.FieldByName('method_id').AsInteger,
    rsCaptionMethod, Msgs);
  ForeignValueExists(tbSamplingPlots, 'net_station_id', dsLink.DataSet.FieldByName('net_station_id').AsInteger,
    rsCaptionSamplingPlot, Msgs);
  ForeignValueExists(tbProjects, 'project_id', dsLink.DataSet.FieldByName('project_id').AsInteger,
    rsCaptionProject, Msgs);

  // Dates
  if dsLink.DataSet.FieldByName('survey_date').AsString <> EmptyStr then
    ValidDate(dsLink.DataSet.FieldByName('survey_date').AsString, rsCaptionDate, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

