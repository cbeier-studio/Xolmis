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
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Character, DBCtrls, StdCtrls, DBEditButton, atshapelinebgra, BCPanel;

type

  { TedtSighting }

  TedtSighting = class(TForm)
    ckCaptured: TDBCheckBox;
    ckSeen: TDBCheckBox;
    ckHeard: TDBCheckBox;
    ckPhotographed: TDBCheckBox;
    ckAudioRecording: TDBCheckBox;
    ckIsInEbird: TDBCheckBox;
    ckNotSurveying: TDBCheckBox;
    eMethod: TDBEditButton;
    eMackinnonListNumber: TDBEdit;
    eSurvey: TDBEditButton;
    eUnbandedTally: TDBEdit;
    eMalesTally: TDBEdit;
    eFemalesTally: TDBEdit;
    eNotSexedTally: TDBEdit;
    eAdultsTally: TDBEdit;
    eImmaturesTally: TDBEdit;
    eNotAgedTally: TDBEdit;
    eNewCaptureTally: TDBEdit;
    eRecapturesTally: TDBEdit;
    eTime: TDBEdit;
    dsLink: TDataSource;
    eDate: TDBEditButton;
    eLongitude: TDBEditButton;
    eLatitude: TDBEditButton;
    eDetectionType: TDBEditButton;
    eBreedingStatus: TDBEditButton;
    eIndividual: TDBEditButton;
    eTaxon: TDBEditButton;
    eLocality: TDBEditButton;
    eObserver: TDBEditButton;
    eQuantity: TDBEdit;
    eDistance: TDBEdit;
    lblDistance: TLabel;
    lblLatitude: TLabel;
    lblDate: TLabel;
    lblLongitude: TLabel;
    lblDetectionType: TLabel;
    lblMackinnonListNumber: TLabel;
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
    mNotes: TDBMemo;
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
    procedure cbMethodKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eBreedingStatusButtonClick(Sender: TObject);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDetectionTypeButtonClick(Sender: TObject);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eMethodButtonClick(Sender: TObject);
    procedure eMethodDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtSighting: TedtSighting;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations;

{$R *.lfm}

{ TedtSighting }

procedure TedtSighting.cbMethodKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSighting.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSighting.eBreedingStatusButtonClick(Sender: TObject);
begin
  BreedingDialog(dsLink.DataSet.FieldByName('breeding_status').AsString, dsLink.DataSet, 'breeding_status');
end;

procedure TedtSighting.eDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDate, dsLink.DataSet, 'sighting_date');
end;

procedure TedtSighting.eDetectionTypeButtonClick(Sender: TObject);
begin
  DetectionDialog(dsLink.DataSet.FieldByName('detection_type').AsString, dsLink.DataSet, 'detection_type');
end;

procedure TedtSighting.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtSighting.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('individual_id').Clear;
    dsLink.DataSet.FieldByName('individual_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSighting.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtSighting.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtSighting.eMethodButtonClick(Sender: TObject);
begin
  FindDlg(tbMethods, eMethod, dsLink.DataSet, 'method_id', 'method_name');
end;

procedure TedtSighting.eMethodDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
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
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSighting.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, dsLink.DataSet, 'observer_id', 'observer_name');
end;

procedure TedtSighting.eObserverDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, dsLink.DataSet, 'observer_id', 'observer_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('observer_id').Clear;
    dsLink.DataSet.FieldByName('observer_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSighting.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name');
end;

procedure TedtSighting.eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSighting.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True);
end;

procedure TedtSighting.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True, Key);
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

procedure TedtSighting.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtSighting.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSighting)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSighting)]);
end;

function TedtSighting.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('method_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('sighting_date').IsNull = False) then
    Result := True;
end;

procedure TedtSighting.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtSighting.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Campos obrigatórios
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'taxon_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'method_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'locality_id', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbSightings, 'sighting_date', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

