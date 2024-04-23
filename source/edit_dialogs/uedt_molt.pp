unit uedt_molt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DBCtrls,
  Character, DateUtils, DBEditButton, atshapelinebgra;

type

  { TedtMolt }

  TedtMolt = class(TForm)
    DBEdit10: TDBEdit;
    DBEdit11: TDBEdit;
    DBEdit12: TDBEdit;
    DBEdit13: TDBEdit;
    DBEdit14: TDBEdit;
    DBEdit15: TDBEdit;
    DBEdit16: TDBEdit;
    DBEdit17: TDBEdit;
    DBEdit18: TDBEdit;
    DBEdit19: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit20: TDBEdit;
    DBEdit22: TDBEdit;
    DBEdit23: TDBEdit;
    DBEdit24: TDBEdit;
    DBEdit25: TDBEdit;
    DBEdit26: TDBEdit;
    DBEdit27: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit32: TDBEdit;
    DBEdit33: TDBEdit;
    DBEdit34: TDBEdit;
    DBEdit35: TDBEdit;
    DBEdit36: TDBEdit;
    DBEdit37: TDBEdit;
    DBEdit38: TDBEdit;
    DBEdit39: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit40: TDBEdit;
    DBEdit42: TDBEdit;
    DBEdit43: TDBEdit;
    DBEdit44: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit52: TDBEdit;
    DBEdit53: TDBEdit;
    DBEdit54: TDBEdit;
    DBEdit55: TDBEdit;
    DBEdit56: TDBEdit;
    DBEdit57: TDBEdit;
    DBEdit58: TDBEdit;
    DBEdit59: TDBEdit;
    DBEdit6: TDBEdit;
    DBEdit60: TDBEdit;
    DBEdit61: TDBEdit;
    DBEdit62: TDBEdit;
    DBEdit63: TDBEdit;
    DBEdit65: TDBEdit;
    DBEdit7: TDBEdit;
    DBEdit72: TDBEdit;
    DBEdit8: TDBEdit;
    DBEdit9: TDBEdit;
    eBander: TDBEditButton;
    dsLink: TDataSource;
    eCaptureTime: TDBEdit;
    eDate: TDBEditButton;
    eSurvey: TDBEditButton;
    lblBandColor: TLabel;
    lblBandColor1: TLabel;
    lblBandColor10: TLabel;
    lblBandColor11: TLabel;
    lblBandColor12: TLabel;
    lblBandColor13: TLabel;
    lblBandColor14: TLabel;
    lblBandColor15: TLabel;
    lblBandColor16: TLabel;
    lblBandColor17: TLabel;
    lblBandColor18: TLabel;
    lblBandColor19: TLabel;
    lblBandColor2: TLabel;
    lblBandColor20: TLabel;
    lblBandColor21: TLabel;
    lblBandColor22: TLabel;
    lblBandColor23: TLabel;
    lblBandColor24: TLabel;
    lblBandColor25: TLabel;
    lblBandColor26: TLabel;
    lblBandColor27: TLabel;
    lblBandColor28: TLabel;
    lblBandColor29: TLabel;
    lblBandColor3: TLabel;
    lblBandColor30: TLabel;
    lblBandColor31: TLabel;
    lblBandColor32: TLabel;
    lblBandColor33: TLabel;
    lblBandColor4: TLabel;
    lblBandColor40: TLabel;
    lblBandColor41: TLabel;
    lblBandColor42: TLabel;
    lblBandColor43: TLabel;
    lblBandColor44: TLabel;
    lblBandColor45: TLabel;
    lblBandColor46: TLabel;
    lblBandColor47: TLabel;
    lblBandColor48: TLabel;
    lblBandColor49: TLabel;
    lblBandColor5: TLabel;
    lblBandColor50: TLabel;
    lblBandColor51: TLabel;
    lblBandColor6: TLabel;
    lblBandColor7: TLabel;
    lblBandColor8: TLabel;
    lblBandColor9: TLabel;
    lblBandPrefix: TLabel;
    lblBandSuffix: TLabel;
    lblBandType: TLabel;
    lblBandType1: TLabel;
    lblBandType10: TLabel;
    lblBandType11: TLabel;
    lblBandType12: TLabel;
    lblBandType14: TLabel;
    lblBandType2: TLabel;
    lblBandType3: TLabel;
    lblBandType4: TLabel;
    lblBandType5: TLabel;
    lblBandType6: TLabel;
    lblBandType7: TLabel;
    lblBandType8: TLabel;
    lblNotes: TLabel;
    lblRequester: TLabel;
    lblSupplier: TLabel;
    lblTitleMetrics: TLabel;
    lblTitleMetrics1: TLabel;
    lblTitleMetrics2: TLabel;
    lblTitleMetrics3: TLabel;
    lblTitleMetrics4: TLabel;
    lblTitleMetrics5: TLabel;
    lblTitleMetrics6: TLabel;
    lblTitleMetrics7: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pPrefixSuffix: TPanel;
    pRequester: TPanel;
    pSupplier: TPanel;
    pTitleMetrics: TPanel;
    pTitleMetrics1: TPanel;
    pTitleMetrics2: TPanel;
    pTitleMetrics3: TPanel;
    pTitleMetrics4: TPanel;
    pTitleMetrics5: TPanel;
    pTitleMetrics6: TPanel;
    pTitleMetrics7: TPanel;
    pTypeColor: TPanel;
    pTypeColor1: TPanel;
    pTypeColor2: TPanel;
    pTypeColor3: TPanel;
    pTypeColor4: TPanel;
    pTypeColor5: TPanel;
    pTypeColor6: TPanel;
    pTypeColor7: TPanel;
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

