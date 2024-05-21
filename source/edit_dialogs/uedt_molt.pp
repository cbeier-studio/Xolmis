{ Xolmis Molt Editor dialog

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

unit uedt_molt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DBCtrls,
  Character, DateUtils, DBEditButton, atshapelinebgra;

type

  { TedtMolt }

  TedtMolt = class(TForm)
    eP8: TDBEdit;
    eP10: TDBEdit;
    eS1: TDBEdit;
    eS2: TDBEdit;
    eS3: TDBEdit;
    eS4: TDBEdit;
    eS5: TDBEdit;
    eS6: TDBEdit;
    eS7: TDBEdit;
    eS8: TDBEdit;
    eP1: TDBEdit;
    eS9: TDBEdit;
    eR1: TDBEdit;
    eR2: TDBEdit;
    eR3: TDBEdit;
    eR4: TDBEdit;
    eR5: TDBEdit;
    eR6: TDBEdit;
    eP2: TDBEdit;
    ePC1: TDBEdit;
    ePC2: TDBEdit;
    ePC3: TDBEdit;
    ePC4: TDBEdit;
    ePC5: TDBEdit;
    ePC6: TDBEdit;
    ePC7: TDBEdit;
    ePC8: TDBEdit;
    eP3: TDBEdit;
    ePC9: TDBEdit;
    eAl1: TDBEdit;
    eAl2: TDBEdit;
    eAl3: TDBEdit;
    eP4: TDBEdit;
    eGC1: TDBEdit;
    eGC2: TDBEdit;
    eGC3: TDBEdit;
    eGC4: TDBEdit;
    eGC5: TDBEdit;
    eGC6: TDBEdit;
    eGC7: TDBEdit;
    eGC8: TDBEdit;
    eP5: TDBEdit;
    eGC9: TDBEdit;
    eGC10: TDBEdit;
    eMC: TDBEdit;
    eLC: TDBEdit;
    eCC: TDBEdit;
    eP7: TDBEdit;
    eGrowthBarWidth: TDBEdit;
    eP9: TDBEdit;
    eP6: TDBEdit;
    eBander: TDBEditButton;
    dsLink: TDataSource;
    eCaptureTime: TDBEdit;
    eDate: TDBEditButton;
    eSurvey: TDBEditButton;
    lblP2: TLabel;
    lblP3: TLabel;
    lblS4: TLabel;
    lblS5: TLabel;
    lblS7: TLabel;
    lblS8: TLabel;
    lblS9: TLabel;
    lblBandColor15: TLabel;
    lblR2: TLabel;
    lblR3: TLabel;
    lblR4: TLabel;
    lblR5: TLabel;
    lblP4: TLabel;
    lblBandColor20: TLabel;
    lblBandColor21: TLabel;
    lblBandColor22: TLabel;
    lblBandColor23: TLabel;
    lblPC2: TLabel;
    lblPC3: TLabel;
    lblPC4: TLabel;
    lblPC5: TLabel;
    lblPC7: TLabel;
    lblPC8: TLabel;
    lblP5: TLabel;
    lblPC9: TLabel;
    lblBandColor31: TLabel;
    lblAl2: TLabel;
    lblAl3: TLabel;
    lblP7: TLabel;
    lblGC2: TLabel;
    lblGC3: TLabel;
    lblGC4: TLabel;
    lblGC5: TLabel;
    lblGC7: TLabel;
    lblGC8: TLabel;
    lblGC9: TLabel;
    lblGC10: TLabel;
    lblLC: TLabel;
    lblSpacer1: TLabel;
    lblP8: TLabel;
    lblCC: TLabel;
    lblSpacer2: TLabel;
    lblP9: TLabel;
    lblP10: TLabel;
    lblS2: TLabel;
    lblS3: TLabel;
    lblBandPrefix: TLabel;
    lblBandSuffix: TLabel;
    lblP1: TLabel;
    lblP6: TLabel;
    lblGC1: TLabel;
    lblGC6: TLabel;
    lblMC: TLabel;
    lblGrowthBarWidth: TLabel;
    lblS1: TLabel;
    lblS6: TLabel;
    lblR1: TLabel;
    lblR6: TLabel;
    lblPC1: TLabel;
    lblPC6: TLabel;
    lblAl1: TLabel;
    lblNotes: TLabel;
    lblRequester: TLabel;
    lblSupplier: TLabel;
    lblTitlePrimaries: TLabel;
    lblTitleSecondaries: TLabel;
    lblTitleRectrices: TLabel;
    lblTitleAlula: TLabel;
    lblTitlePrimaryCoverts: TLabel;
    lblTitleGreatCoverts: TLabel;
    lblTitleOtherCoverts: TLabel;
    lblTitleGrowthBars: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pDateTime: TPanel;
    pBander: TPanel;
    pSurvey: TPanel;
    pTitlePrimaries: TPanel;
    pTitleSecondaries: TPanel;
    pTitleRectrices: TPanel;
    pTitleAlula: TPanel;
    pTitlePrimaryCoverts: TPanel;
    pTitleGreatCoverts: TPanel;
    pTitleOtherCoverts: TPanel;
    pTitleGrowthBars: TPanel;
    pPrimaries: TPanel;
    pSecondaries: TPanel;
    pRectrices: TPanel;
    pPrimaryCoverts: TPanel;
    pAlula: TPanel;
    pGreatCoverts: TPanel;
    pOtherCoverts: TPanel;
    pGrowthBars: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eBanderButtonClick(Sender: TObject);
    procedure eBanderDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eDateButtonClick(Sender: TObject);
    procedure eDateDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtMolt: TedtMolt;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations;

{$R *.lfm}

{ TedtMolt }

procedure TedtMolt.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtMolt.eBanderButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eBander, dsLink.DataSet, 'bander_id', 'bander_name');
end;

procedure TedtMolt.eBanderDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eBander, dsLink.DataSet, 'bander_id', 'bander_name', False, Key);
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

procedure TedtMolt.eDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDate, dsLink.DataSet, 'sample_date');
end;

procedure TedtMolt.eDateDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtMolt.eSurveyButtonClick(Sender: TObject);
begin
  FindDlg(tbSurveys, eSurvey, dsLink.DataSet, 'survey_id', 'survey_name');
end;

procedure TedtMolt.eSurveyDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtMolt.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtMolt.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtMolt.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtMolt.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionMolt)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionMolt)]);
end;

function TedtMolt.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('bander_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('sample_date').IsNull = False) then
    Result := True;
end;

procedure TedtMolt.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtMolt.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbMolts, 'sample_date', Msgs);
  RequiredIsEmpty(D, tbMolts, 'bander_id', Msgs);

  // Values in range
  ValueInRange(D.FieldByName('p1_molt').AsFloat, 0.0, 1.0, 'P1', Msgs, Msg);
  ValueInRange(D.FieldByName('p2_molt').AsFloat, 0.0, 1.0, 'P2', Msgs, Msg);
  ValueInRange(D.FieldByName('p3_molt').AsFloat, 0.0, 1.0, 'P3', Msgs, Msg);
  ValueInRange(D.FieldByName('p4_molt').AsFloat, 0.0, 1.0, 'P4', Msgs, Msg);
  ValueInRange(D.FieldByName('p5_molt').AsFloat, 0.0, 1.0, 'P5', Msgs, Msg);
  ValueInRange(D.FieldByName('p6_molt').AsFloat, 0.0, 1.0, 'P6', Msgs, Msg);
  ValueInRange(D.FieldByName('p7_molt').AsFloat, 0.0, 1.0, 'P7', Msgs, Msg);
  ValueInRange(D.FieldByName('p8_molt').AsFloat, 0.0, 1.0, 'P8', Msgs, Msg);
  ValueInRange(D.FieldByName('p9_molt').AsFloat, 0.0, 1.0, 'P9', Msgs, Msg);
  ValueInRange(D.FieldByName('p10_molt').AsFloat, 0.0, 1.0, 'P10', Msgs, Msg);
  ValueInRange(D.FieldByName('s1_molt').AsFloat, 0.0, 1.0, 'S1', Msgs, Msg);
  ValueInRange(D.FieldByName('s2_molt').AsFloat, 0.0, 1.0, 'S2', Msgs, Msg);
  ValueInRange(D.FieldByName('s3_molt').AsFloat, 0.0, 1.0, 'S3', Msgs, Msg);
  ValueInRange(D.FieldByName('s4_molt').AsFloat, 0.0, 1.0, 'S4', Msgs, Msg);
  ValueInRange(D.FieldByName('s5_molt').AsFloat, 0.0, 1.0, 'S5', Msgs, Msg);
  ValueInRange(D.FieldByName('s6_molt').AsFloat, 0.0, 1.0, 'S6', Msgs, Msg);
  ValueInRange(D.FieldByName('s7_molt').AsFloat, 0.0, 1.0, 'S7', Msgs, Msg);
  ValueInRange(D.FieldByName('s8_molt').AsFloat, 0.0, 1.0, 'S8', Msgs, Msg);
  ValueInRange(D.FieldByName('s9_molt').AsFloat, 0.0, 1.0, 'S9', Msgs, Msg);
  ValueInRange(D.FieldByName('r1_molt').AsFloat, 0.0, 1.0, 'R1', Msgs, Msg);
  ValueInRange(D.FieldByName('r2_molt').AsFloat, 0.0, 1.0, 'R2', Msgs, Msg);
  ValueInRange(D.FieldByName('r3_molt').AsFloat, 0.0, 1.0, 'R3', Msgs, Msg);
  ValueInRange(D.FieldByName('r4_molt').AsFloat, 0.0, 1.0, 'R4', Msgs, Msg);
  ValueInRange(D.FieldByName('r5_molt').AsFloat, 0.0, 1.0, 'R5', Msgs, Msg);
  ValueInRange(D.FieldByName('r6_molt').AsFloat, 0.0, 1.0, 'R6', Msgs, Msg);
  ValueInRange(D.FieldByName('pc1_molt').AsFloat, 0.0, 1.0, 'PC1', Msgs, Msg);
  ValueInRange(D.FieldByName('pc2_molt').AsFloat, 0.0, 1.0, 'PC2', Msgs, Msg);
  ValueInRange(D.FieldByName('pc3_molt').AsFloat, 0.0, 1.0, 'PC3', Msgs, Msg);
  ValueInRange(D.FieldByName('pc4_molt').AsFloat, 0.0, 1.0, 'PC4', Msgs, Msg);
  ValueInRange(D.FieldByName('pc5_molt').AsFloat, 0.0, 1.0, 'PC5', Msgs, Msg);
  ValueInRange(D.FieldByName('pc6_molt').AsFloat, 0.0, 1.0, 'PC6', Msgs, Msg);
  ValueInRange(D.FieldByName('pc7_molt').AsFloat, 0.0, 1.0, 'PC7', Msgs, Msg);
  ValueInRange(D.FieldByName('pc8_molt').AsFloat, 0.0, 1.0, 'PC8', Msgs, Msg);
  ValueInRange(D.FieldByName('pc9_molt').AsFloat, 0.0, 1.0, 'PC9', Msgs, Msg);
  ValueInRange(D.FieldByName('gc1_molt').AsFloat, 0.0, 1.0, 'GC1', Msgs, Msg);
  ValueInRange(D.FieldByName('gc2_molt').AsFloat, 0.0, 1.0, 'GC2', Msgs, Msg);
  ValueInRange(D.FieldByName('gc3_molt').AsFloat, 0.0, 1.0, 'GC3', Msgs, Msg);
  ValueInRange(D.FieldByName('gc4_molt').AsFloat, 0.0, 1.0, 'GC4', Msgs, Msg);
  ValueInRange(D.FieldByName('gc5_molt').AsFloat, 0.0, 1.0, 'GC5', Msgs, Msg);
  ValueInRange(D.FieldByName('gc6_molt').AsFloat, 0.0, 1.0, 'GC6', Msgs, Msg);
  ValueInRange(D.FieldByName('gc7_molt').AsFloat, 0.0, 1.0, 'GC7', Msgs, Msg);
  ValueInRange(D.FieldByName('gc8_molt').AsFloat, 0.0, 1.0, 'GC8', Msgs, Msg);
  ValueInRange(D.FieldByName('gc9_molt').AsFloat, 0.0, 1.0, 'GC9', Msgs, Msg);
  ValueInRange(D.FieldByName('gc10_molt').AsFloat, 0.0, 1.0, 'GC10', Msgs, Msg);
  ValueInRange(D.FieldByName('cc_molt').AsFloat, 0.0, 1.0, 'CC', Msgs, Msg);
  ValueInRange(D.FieldByName('mc_molt').AsFloat, 0.0, 1.0, 'MC', Msgs, Msg);
  ValueInRange(D.FieldByName('lc_molt').AsFloat, 0.0, 1.0, 'LC', Msgs, Msg);
  ValueInRange(D.FieldByName('al1_molt').AsFloat, 0.0, 1.0, 'AL1', Msgs, Msg);
  ValueInRange(D.FieldByName('al2_molt').AsFloat, 0.0, 1.0, 'AL2', Msgs, Msg);
  ValueInRange(D.FieldByName('al3_molt').AsFloat, 0.0, 1.0, 'AL3', Msgs, Msg);

  // Dates
  if D.FieldByName('sample_date').AsString <> '' then
    ValidDate(D.FieldByName('sample_date').AsString, rsCaptionDate, Msgs);

  if (D.FieldByName('sample_date').AsString <> '') then
    IsFutureDate(D.FieldByName('sample_date').AsDateTime, Today,
      AnsiLowerCase(rsCaptionDate), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

