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
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, Character, DateUtils,
  ExtCtrls, StdCtrls, DBCtrls, DBEditButton, atshapelinebgra,
  cbs_birds;

type

  { TedtCapture }

  TedtCapture = class(TForm)
    cbAge: TDBComboBox;
    cbCamera: TDBComboBox;
    cbSex: TDBComboBox;
    cbSkullOssification: TDBComboBox;
    cbStatus: TDBComboBox;
    eAnnotator: TDBEditButton;
    eBander: TDBEditButton;
    eLeftTarsus: TDBEditButton;
    eRightWingChord: TDBEdit;
    eGlucose: TDBEdit;
    eHematocrit: TDBEdit;
    eHemoglobin: TDBEdit;
    eHowSexed: TDBEditButton;
    ePhotographer2: TDBEditButton;
    eFirstSecondaryChord: TDBEdit;
    eExposedCulmen: TDBEdit;
    eNostrilBillTip: TDBEdit;
    eBillWidth: TDBEdit;
    eBillHeight: TDBEdit;
    ePhilornisLarvae: TDBEdit;
    eCaptureTime: TDBEdit;
    eTotalCulmen: TDBEdit;
    eTotalLength: TDBEdit;
    eSkullLength: TDBEdit;
    eWeight: TDBEdit;
    eTarsusDiameter: TDBEdit;
    eTarsusLength: TDBEdit;
    eTailLength: TDBEdit;
    eSurvey: TDBEditButton;
    dsLink: TDataSource;
    ckEscaped: TDBCheckBox;
    ckBloodSample: TDBCheckBox;
    ckFeathers: TDBCheckBox;
    ckFeces: TDBCheckBox;
    ckParasites: TDBCheckBox;
    ckAudioRecordings: TDBCheckBox;
    ckPhotos: TDBCheckBox;
    ckClaw: TDBCheckBox;
    ckWholeSpecimen: TDBCheckBox;
    cbCaptureType: TDBComboBox;
    cbCloacalProtuberance: TDBComboBox;
    cbBroodPatch: TDBComboBox;
    cbFat: TDBComboBox;
    cbBodyMolt: TDBComboBox;
    cbFlightFeatherMolt: TDBComboBox;
    cbFlightFeatherWear: TDBComboBox;
    eKippsIndex: TDBEdit;
    eLocality: TDBEditButton;
    eCaptureDate: TDBEditButton;
    eStartPhoto: TDBEdit;
    eEndPhoto: TDBEdit;
    eFieldNumber: TDBEdit;
    eNet: TDBEditButton;
    eLongitude: TDBEditButton;
    eLatitude: TDBEditButton;
    eRightTarsus: TDBEditButton;
    ePhotographer1: TDBEditButton;
    eCycleCode: TDBEditButton;
    eHowAged: TDBEditButton;
    eMoltLimits: TDBEditButton;
    eTaxon: TDBEditButton;
    eBand: TDBEditButton;
    eRemovedBand: TDBEditButton;
    mNotes: TDBMemo;
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
    procedure eLatitudeButtonClick(Sender: TObject);
    procedure eLeftLegBelowButtonClick(Sender: TObject);
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
    procedure eRightLegBelowButtonClick(Sender: TObject);
    procedure eRightLegBelowExit(Sender: TObject);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyExit(Sender: TObject);
    procedure eSurveyKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
    procedure AssembleFullName;
    procedure PaintColorBands(aLeg: TBodyPart);
    procedure GetCameras;
  public

  end;

var
  edtCapture: TedtCapture;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_getvalue, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_taxonomy,
  cbs_validations, cbs_fullnames, cbs_themes, udm_main, uDarkStyleParams;

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
    dsLink.DataSet.FieldByName('subject_age').Clear;
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
    dsLink.DataSet.FieldByName('capture_type').Clear;
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
    dsLink.DataSet.FieldByName('subject_sex').Clear;
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
    dsLink.DataSet.FieldByName('subject_status').Clear;
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
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtCapture.eLatitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtCapture.eCycleCodeButtonClick(Sender: TObject);
begin
  MoltCycleDialog(dsLink.DataSet.FieldByName('cycle_code').AsString, dsLink.DataSet, 'cycle_code');
end;

procedure TedtCapture.eAnnotatorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eAnnotator, dsLink.DataSet, 'annotator_id', 'annotator_name');
  dsLink.DataSet.FieldByName('annotator_name').AsString :=
    GetName('people', 'acronym', 'person_id',
    dsLink.DataSet.FieldByName('annotator_id').AsInteger);
end;

procedure TedtCapture.eAnnotatorKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbPeople, eAnnotator, dsLink.DataSet, 'annotator_id', 'annotator_name', False, Key) then
      dsLink.DataSet.FieldByName('annotator_name').AsString :=
        GetName('people', 'acronym', 'person_id', dsLink.DataSet.FieldByName('annotator_id').AsInteger);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('annotator_id').Clear;
    dsLink.DataSet.FieldByName('annotator_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtCapture.eBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eBand, dsLink.DataSet, 'band_id', 'band_name');
end;

procedure TedtCapture.eBanderButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eBander, dsLink.DataSet, 'bander_id', 'bander_name');
  dsLink.DataSet.FieldByName('bander_name').AsString :=
    GetName('people', 'acronym', 'person_id',
    dsLink.DataSet.FieldByName('bander_id').AsInteger);
end;

procedure TedtCapture.eBanderKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindDlg(tbPeople, eBander, dsLink.DataSet, 'bander_id', 'bander_name', False, Key) then
      dsLink.DataSet.FieldByName('bander_name').AsString :=
        GetName('people', 'acronym', 'person_id', dsLink.DataSet.FieldByName('bander_id').AsInteger);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('bander_id').Clear;
    dsLink.DataSet.FieldByName('bander_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
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
    FindDlg(tbBands, eBand, dsLink.DataSet, 'band_id', 'band_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('band_id').Clear;
    dsLink.DataSet.FieldByName('band_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eCaptureDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eCaptureDate, dsLink.DataSet, 'capture_date');
end;

procedure TedtCapture.eCaptureTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
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

procedure TedtCapture.eLeftLegBelowButtonClick(Sender: TObject);
begin
  EditColorBands(dsLink.DataSet, 'left_leg_below', eLeftTarsus);
  PaintColorBands(bpLeftTarsus);
  if eLeftTarsus.CanSetFocus then
    eLeftTarsus.SetFocus;
end;

procedure TedtCapture.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtCapture.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
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
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
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
var
  s, d: String;
  f: Extended;
begin
  FormKeyPress(Sender, Key);
  if (CharInSet(Key, ['0' .. '9', '.', ',', '-', '+', #8, #13, #27])) then
  begin
    if eLongitude.Focused then
      s := dsLink.DataSet.FieldByName('longitude').AsString
    else
    if eLatitude.Focused then
      s := dsLink.DataSet.FieldByName('latitude').AsString
    else
    if (Length(s) > 0) then
    begin
      TryStrToFloat(s, f);
      if (Key = '-') then
      begin
        if (f > 0.0) then
        begin
          f := f * -1.0;
          with dsLink.DataSet do
          begin
            if eLongitude.Focused then
              FieldByName('longitude').AsFloat := f
            else
            if eLatitude.Focused then
              FieldByName('latitude').AsFloat := f;
          end;
        end;
        Key := #0;
      end;
      if (Key = '+') then
      begin
        if (f < 0.0) then
        begin
          f := f * -1.0;
          with dsLink.DataSet do
          begin
            if eLongitude.Focused then
              FieldByName('longitude').AsFloat := f
            else
            if eLatitude.Focused then
              FieldByName('latitude').AsFloat := f;
          end;
        end;
        Key := #0;
      end;
    end;
    if ((Key = ',') or (Key = '.')) then
    begin
      d := FormatSettings.DecimalSeparator;
      if (Pos(d, s) = 0) then
        Key := FormatSettings.DecimalSeparator
      else
        Key := #0;
    end;
  end
  else
    Key := #0;

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eMoltLimitsButtonClick(Sender: TObject);
begin
  MoltLimitsDialog(dsLink.DataSet.FieldByName('molt_limits').AsString, dsLink.DataSet, 'molt_limits');
end;

procedure TedtCapture.eNetButtonClick(Sender: TObject);
begin
  FindDlg(tbNetsEffort, eNet, dsLink.DataSet, 'net_id', 'net_number');
end;

procedure TedtCapture.eNetExit(Sender: TObject);
var
  NetCoord: TMapPoint;
begin
  with dsLink.DataSet do
  begin
    if FieldByName('net_id').AsInteger > 0 then
    begin
      if GetLatLong('nets_effort', 'longitude', 'latitude', 'full_name',
        'net_id', FieldByName('net_id').AsInteger, NetCoord) then
      begin
        if FieldByName('longitude').IsNull then
          FieldByName('longitude').AsFloat := NetCoord.X;
        if FieldByName('latitude').IsNull then
          FieldByName('latitude').AsFloat := NetCoord.Y;
      end;
    end;
  end;
end;

procedure TedtCapture.eNetKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNetsEffort, eNet, dsLink.DataSet, 'net_id', 'net_number', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('net_id').Clear;
    dsLink.DataSet.FieldByName('net_number').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.ePhotographer1ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePhotographer1, dsLink.DataSet, 'photographer_1_id', 'photographer_1_name');
  dsLink.DataSet.FieldByName('photographer_1_name').AsString :=
    GetName('people', 'acronym', 'person_id',
    dsLink.DataSet.FieldByName('photographer_1_id').AsInteger);
end;

procedure TedtCapture.ePhotographer1KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePhotographer1, dsLink.DataSet, 'photographer_1_id', 'photographer_1_name', False, Key);
    dsLink.DataSet.FieldByName('photographer_1_name').AsString :=
      GetName('people', 'acronym', 'person_id',
      dsLink.DataSet.FieldByName('photographer_1_id').AsInteger);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('photographer_1_id').Clear;
    dsLink.DataSet.FieldByName('photographer_1_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.ePhotographer2ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePhotographer2, dsLink.DataSet, 'photographer_2_id', 'photographer_2_name');
  dsLink.DataSet.FieldByName('photographer_2_name').AsString :=
    GetName('people', 'acronym', 'person_id',
    dsLink.DataSet.FieldByName('photographer_2_id').AsInteger);
end;

procedure TedtCapture.ePhotographer2KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePhotographer2, dsLink.DataSet, 'photographer_2_id', 'photographer_2_name', False, Key);
    dsLink.DataSet.FieldByName('photographer_2_name').AsString :=
      GetName('people', 'acronym', 'person_id',
      dsLink.DataSet.FieldByName('photographer_2_name').AsInteger);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('photographer_2_id').Clear;
    dsLink.DataSet.FieldByName('photographer_2_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eRemovedBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eRemovedBand, dsLink.DataSet, 'removed_band_id', 'removed_band_name');
end;

procedure TedtCapture.eRemovedBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbBands, eRemovedBand, dsLink.DataSet, 'removed_band_id', 'removed_band_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('removed_band_id').Clear;
    dsLink.DataSet.FieldByName('removed_band_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eRightLegBelowButtonClick(Sender: TObject);
begin
  EditColorBands(dsLink.DataSet, 'right_leg_below', eRightTarsus);
  PaintColorBands(bpRightTarsus);
  if eRightTarsus.CanSetFocus then
    eRightTarsus.SetFocus;
end;

procedure TedtCapture.eRightLegBelowExit(Sender: TObject);
begin
  if Sender = eRightTarsus then
    PaintColorBands(bpRightTarsus)
  else
  if Sender = eLeftTarsus then
    PaintColorBands(bpLeftTarsus);
end;

procedure TedtCapture.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name');
end;

procedure TedtCapture.eSurveyExit(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  with dsLink.DataSet do
  begin
    if FieldByName('survey_id').AsInteger > 0 then
    begin
      Qry := TSQLQuery.Create(DMM.sqlCon);
      try
        Qry.Database := DMM.sqlCon;
        Qry.Transaction := DMM.sqlTrans;
        Qry.SQL.Add('SELECT locality_id, expedition_id, survey_date FROM surveys');
        Qry.SQL.Add('WHERE survey_id = :survey_id');
        Qry.ParamByName('SURVEY_ID').AsInteger := FieldByName('survey_id').AsInteger;
        Qry.Open;
        if Qry.RecordCount > 0 then
        begin
          FieldByName('locality_id').AsInteger := Qry.FieldByName('locality_id').AsInteger;
          //FieldByName('locality_name').AsString :=
          //  GetName('gazetteer', 'full_name', 'site_id', FieldByName('locality_id').AsInteger);
          FieldByName('capture_date').AsDateTime := Qry.FieldByName('survey_date').AsDateTime;
        end;
        Qry.Close;
      finally
        FreeAndNil(Qry);
      end;
    end;
  end;
end;

procedure TedtCapture.eSurveyKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('survey_id').Clear;
    dsLink.DataSet.FieldByName('survey_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, dsLink.DataSet,
    'taxon_id', 'taxon_name', True);
end;

procedure TedtCapture.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, dsLink.DataSet,
      'taxon_id', 'taxon_name', True, key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('taxon_id').Clear;
    dsLink.DataSet.FieldByName('taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtCapture.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtCapture.FormCreate(Sender: TObject);
begin
  GetCameras;
  cbAge.Items.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeImmature + ',' +
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
    if not (dsLink.State in [dsInsert, dsEdit]) then
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

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionCapture)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionCapture)]);

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

function TedtCapture.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('bander_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('annotator_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('capture_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('capture_type').AsString <> EmptyStr) and
    ((dsLink.DataSet.FieldByName('capture_type').AsString <> 'U') and
      (dsLink.DataSet.FieldByName('band_id').AsInteger <> 0)) then
    Result := True;

  if (dsLink.DataSet.FieldByName('capture_type').AsString = 'U') then
    lblBand.Caption := rsCaptionBand + ':'
  else
    lblBand.Caption := rsCaptionBand + ': *';
end;

procedure TedtCapture.PaintColorBands(aLeg: TBodyPart);
var
  i: Integer;
  B: TStrings;
  Band: TShape;
  aField: String;

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
        aField := 'right_leg_below';
      end;
    bpLeftTarsus:
      begin
        ClearBand(shpLeftBelowBand1);
        ClearBand(shpLeftBelowBand2);
        ClearBand(shpLeftBelowBand3);
        ClearBand(shpLeftBelowBand4);
        aField := 'left_leg_below';
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

  if (dsLink.DataSet.FieldByName(aField).AsString <> '') then
  begin
    B := TStringList.Create;
    B.CommaText := dsLink.DataSet.FieldByName(aField).AsString;
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

function TedtCapture.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbCaptures, 'locality_id', Msgs);
  RequiredIsEmpty(D, tbCaptures, 'capture_date', Msgs);
  RequiredIsEmpty(D, tbCaptures, 'bander_id', Msgs);
  RequiredIsEmpty(D, tbCaptures, 'annotator_id', Msgs);
  RequiredIsEmpty(D, tbCaptures, 'capture_type', Msgs);
  if (D.FieldByName('capture_type').AsString <> 'U') then
    RequiredIsEmpty(D, tbCaptures, 'band_id', Msgs);

  // Dates
  IsFutureDate(D.FieldByName('capture_date').AsDateTime, Today, rsDateCapture, rsDateToday);

  // Set of values
  ValueInSet(D.FieldByName('cloacal_protuberance').AsString, rsCloacalProtuberance,
    CloacalProtuberanceValues, Msgs);
  ValueInSet(D.FieldByName('brood_patch').AsString, rsBroodPatch, BroodPatchValues, Msgs);
  ValueInSet(D.FieldByName('fat').AsString, rsSubcutaneousFat, FatValues, Msgs);
  ValueInSet(D.FieldByName('body_molt').AsString, rsBodyMolt, BodyMoltValues, Msgs);
  ValueInSet(D.FieldByName('flight_feathers_molt').AsString, rsFlightMolt, FlightMoltValues, Msgs);
  ValueInSet(D.FieldByName('flight_feathers_wear').AsString, rsFlightWear, FeatherWearValues, Msgs);
  ValueInSet(D.FieldByName('skull_ossification').AsString, rsSkullOssification, SkullValues, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

