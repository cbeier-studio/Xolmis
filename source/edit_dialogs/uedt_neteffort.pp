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
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls,
  Character, StdCtrls, DBEditButton, atshapelinebgra, BCPanel;

type

  { TedtNetEffort }

  TedtNetEffort = class(TForm)
    cbNetMesh: TDBComboBox;
    eLatitude: TDBEditButton;
    eLongitude: TDBEditButton;
    eNetClose4: TDBEdit;
    eNetOpen4: TDBEdit;
    eSurvey: TDBEditButton;
    ePermanentNet: TDBEditButton;
    eNetClose2: TDBEdit;
    eNetClose3: TDBEdit;
    eNetLength: TDBEdit;
    eNetHeight: TDBEdit;
    eNetNumber: TDBEdit;
    eNetOpen1: TDBEdit;
    eNetClose1: TDBEdit;
    eNetOpen2: TDBEdit;
    eNetOpen3: TDBEdit;
    lblNetClose4: TLabel;
    lblNetOpen4: TLabel;
    lblSurvey: TLabel;
    pNetOpenClose4: TPanel;
    pSurvey: TBCPanel;
    txtNetArea: TDBText;
    txtTotalOpenTime: TDBText;
    dsLink: TDataSource;
    eDate: TDBEditButton;
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
    mNotes: TDBMemo;
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
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDateButtonClick(Sender: TObject);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eNetNumberKeyPress(Sender: TObject; var Key: char);
    procedure ePermanentNetButtonClick(Sender: TObject);
    procedure ePermanentNetDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eSurveyButtonClick(Sender: TObject);
    procedure eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtNetEffort: TedtNetEffort;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations;

{$R *.lfm}

{ TedtNetEffort }

procedure TedtNetEffort.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetEffort.eDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDate, dsLink.DataSet, 'sample_date');
end;

procedure TedtNetEffort.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtNetEffort.eNetNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNetEffort.ePermanentNetButtonClick(Sender: TObject);
begin
  FindDlg(tbPermanentNets, ePermanentNet, dsLink.DataSet, 'permanent_net_id', 'permanent_net_name');
end;

procedure TedtNetEffort.ePermanentNetDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPermanentNets, ePermanentNet, dsLink.DataSet, 'permanent_net_id', 'permanent_net_name',
      False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('permanent_net_id').Clear;
    dsLink.DataSet.FieldByName('permanent_net_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNetEffort.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name');
end;

procedure TedtNetEffort.eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtNetEffort.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtNetEffort.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionMistnet)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionMistnet)]);
end;

function TedtNetEffort.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('net_number').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('sample_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('net_open_1').IsNull = False) and
    (dsLink.DataSet.FieldByName('net_close_1').IsNull = False) then
    Result := True;
end;

procedure TedtNetEffort.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtNetEffort.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'revision_date', Msgs);
  RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'net_number', Msgs);
  RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'net_open_1', Msgs);
  RequiredIsEmpty(dsLink.Dataset, tbPermanentNets, 'net_close_1', Msgs);

  // Geographical coordinates
  //CoordenadaIsOk(DSIO.Dataset, 'longitude', maLongitude, Msgs);
  //CoordenadaIsOk(DSIO.Dataset, 'latitude', maLatitude, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

