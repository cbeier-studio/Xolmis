unit uedt_individual;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBCtrls, atshapelinebgra, Buttons, LCLType, DBEditButton;

type

  { TedtIndividual }

  TedtIndividual = class(TForm)
    cbSex: TDBComboBox;
    cbAge: TDBComboBox;
    eBirthDay: TDBEdit;
    eBirthMonth: TDBEdit;
    eBirthYear: TDBEdit;
    eBand: TDBEditButton;
    eBandingDate: TDBEditButton;
    eDoubleBand: TDBEditButton;
    eRemovedBand: TDBEditButton;
    eBandChangeDate: TDBEditButton;
    eRightTarsus: TDBEditButton;
    eLeftTarsus: TDBEditButton;
    eRightTibia: TDBEditButton;
    eLeftTibia: TDBEditButton;
    eDeathDay: TDBEdit;
    eDeathMonth: TDBEdit;
    eDeathYear: TDBEdit;
    dsLink: TDataSource;
    eTaxon: TDBEditButton;
    eNest: TDBEditButton;
    eFather: TDBEditButton;
    eMother: TDBEditButton;
    lblTaxon: TLabel;
    lblRightTibia: TLabel;
    lblLeftTibia: TLabel;
    lblBirthDate: TLabel;
    lblRightTarsus: TLabel;
    lblDeathDate: TLabel;
    lblLeftTarsus: TLabel;
    lblNotes: TLabel;
    lblRecognizableMarkings: TLabel;
    lblBand: TLabel;
    lblFather: TLabel;
    lblRemovedBand: TLabel;
    lblBandChangeDate: TLabel;
    lblDoubleBand: TLabel;
    lblSex: TLabel;
    lblAge: TLabel;
    lblBandingDate: TLabel;
    lblMother: TLabel;
    lblNest: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    mRecognizableMarks: TDBMemo;
    pBirthDate: TPanel;
    pDeathDate: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pRecognizableMarkings: TPanel;
    pBand: TPanel;
    pFather: TPanel;
    pRemovedBand: TPanel;
    pDoubleBand: TPanel;
    pSexAge: TPanel;
    pTaxon: TPanel;
    pColorTibiae: TPanel;
    pBirthDeath: TPanel;
    pColorTarsi: TPanel;
    pMother: TPanel;
    pNest: TPanel;
    sbCancel: TButton;
    scrollContent: TScrollBox;
    sbSave: TButton;
    procedure cbSexKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eBandButtonClick(Sender: TObject);
    procedure eBandChangeDateButtonClick(Sender: TObject);
    procedure eBandingDateButtonClick(Sender: TObject);
    procedure eBandKeyPress(Sender: TObject; var Key: char);
    procedure eDoubleBandButtonClick(Sender: TObject);
    procedure eDoubleBandKeyPress(Sender: TObject; var Key: char);
    procedure eFatherButtonClick(Sender: TObject);
    procedure eFatherKeyPress(Sender: TObject; var Key: char);
    procedure eLeftTarsusButtonClick(Sender: TObject);
    procedure eLeftTibiaButtonClick(Sender: TObject);
    procedure eMotherButtonClick(Sender: TObject);
    procedure eMotherKeyPress(Sender: TObject; var Key: char);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestKeyPress(Sender: TObject; var Key: char);
    procedure eRemovedBandButtonClick(Sender: TObject);
    procedure eRemovedBandKeyPress(Sender: TObject; var Key: char);
    procedure eRightTarsusButtonClick(Sender: TObject);
    procedure eRightTibiaButtonClick(Sender: TObject);
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
  public

  end;

var
  edtIndividual: TedtIndividual;

implementation

uses
  cbs_locale, cbs_global, cbs_system, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy,
  cbs_validations;

{$R *.lfm}

{ TedtIndividual }

procedure TedtIndividual.cbSexKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtIndividual.eBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eBand, dsLink.DataSet, 'band_id', 'band_full_name');
end;

procedure TedtIndividual.eBandChangeDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eBandChangeDate, dsLink.DataSet, 'band_change_date');
end;

procedure TedtIndividual.eBandingDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eBandingDate, dsLink.DataSet, 'banding_date');
end;

procedure TedtIndividual.eBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbIndividuals, eBand, dsLink.DataSet, 'band_id', 'band_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('band_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eDoubleBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eDoubleBand, dsLink.DataSet, 'double_band_id', 'double_band_name');
end;

procedure TedtIndividual.eDoubleBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eDoubleBand, dsLink.DataSet, 'double_band_id', 'double_band_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('double_band_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eFatherButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eFather, dsLink.DataSet, 'father_id', 'father_name');
end;

procedure TedtIndividual.eFatherKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eFather, dsLink.DataSet, 'father_id', 'father_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('father_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eLeftTarsusButtonClick(Sender: TObject);
begin
  EditColorBands(dsLink.DataSet, 'left_leg_below', eLeftTarsus);
end;

procedure TedtIndividual.eLeftTibiaButtonClick(Sender: TObject);
begin
  EditColorBands(dsLink.DataSet, 'left_leg_above', eLeftTibia);
end;

procedure TedtIndividual.eMotherButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eMother, dsLink.DataSet, 'mother_id', 'mother_name');
end;

procedure TedtIndividual.eMotherKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eMother, dsLink.DataSet, 'mother_id', 'mother_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('mother_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, dsLink.DataSet, 'nest_id', 'nest_name');
end;

procedure TedtIndividual.eNestKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eNest, dsLink.DataSet, 'nest_id', 'nest_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('nest_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eRemovedBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eRemovedBand, dsLink.DataSet, 'removed_band_id', 'removed_band_name');
end;

procedure TedtIndividual.eRemovedBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eRemovedBand, dsLink.DataSet, 'removed_band_id', 'removed_band_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('removed_band_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eRightTarsusButtonClick(Sender: TObject);
begin
  EditColorBands(dsLink.DataSet, 'right_leg_below', eRightTarsus);
end;

procedure TedtIndividual.eRightTibiaButtonClick(Sender: TObject);
begin
  EditColorBands(dsLink.DataSet, 'right_leg_above', eRightTibia);
end;

procedure TedtIndividual.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, dsLink.DataSet,
    'taxon_id', 'taxon_name', True);
end;

procedure TedtIndividual.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies, tfSubspecies, tfSubspeciesGroups], eTaxon, dsLink.DataSet,
      'taxon_id', 'taxon_name', True, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('taxon_id').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtIndividual.FormCreate(Sender: TObject);
begin
  cbSex.Items.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  cbAge.Items.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeImmature + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',' + rsAgeFirstYear + ',' + rsAgeSecondYear + ',' +
    rsAgeThirdYear + ',' + rsAgeFourthYear + ',' + rsAgeFifthYear;
end;

procedure TedtIndividual.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtIndividual.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtIndividual.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionIndividual)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionIndividual)]);
end;

function TedtIndividual.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) then
    Result := True;
end;

procedure TedtIndividual.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtIndividual.ValidateFields: Boolean;
var
  Msgs: TStrings;
  DataNasc, DataOb, Hoje: TPartialDate;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  { Required fields }
  RequiredIsEmpty(D, tbIndividuals, 'taxon_id', Msgs);
  // RequiredIsEmpty(D, tbIndividuals, 'band_id', Msgs);

  { Duplicated record }
  if (D.FieldByName('band_id').AsInteger > 0) then
    RecordDuplicated(tbIndividuals, 'individual_id', 'full_name',
      D.FieldByName('full_name').AsString,
      D.FieldByName('individual_id').AsInteger, Msgs);

  { Foreign keys }
  ForeignValueExists(tbZooTaxa, 'taxon_id', D.FieldByName('taxon_id').AsInteger,
    rsCaptionTaxon, Msgs);
  ForeignValueExists(tbBands, 'band_id', D.FieldByName('band_id').AsInteger,
    rsCaptionBand, Msgs);
  ForeignValueExists(tbBands, 'band_id', D.FieldByName('double_band_id').AsInteger,
    rsCaptionDoubleBand, Msgs);
  ForeignValueExists(tbBands, 'band_id', D.FieldByName('removed_band_id').AsInteger,
    rsCaptionRemovedBand, Msgs);
  ForeignValueExists(tbNests, 'nest_id', D.FieldByName('nest_id').AsInteger,
    rsCaptionNest, Msgs);
  ForeignValueExists(tbIndividuals, 'individual_id', D.FieldByName('father_id').AsInteger,
    rsCaptionFather, Msgs);
  ForeignValueExists(tbIndividuals, 'individual_id', D.FieldByName('mother_id').AsInteger,
    rsCaptionMother, Msgs);

  { Dates }
  Hoje.Today;
  if D.FieldByName('birth_year').AsInteger > 0 then
  begin
    DataNasc.Year := D.FieldByName('birth_year').AsInteger;
    DataNasc.Month := D.FieldByName('birth_month').AsInteger;
    DataNasc.Day := D.FieldByName('birth_day').AsInteger;
    if ValidPartialDate(DataNasc, rsDateBirth, Msgs) then
      IsFuturePartialDate(DataNasc, Hoje, rsDateBirth, LowerCase(rsDateToday), Msgs);
  end;
  if D.FieldByName('death_year').AsInteger > 0 then
  begin
    DataOb.Year := D.FieldByName('death_year').AsInteger;
    DataOb.Month := D.FieldByName('death_month').AsInteger;
    DataOb.Day := D.FieldByName('death_day').AsInteger;
    if ValidPartialDate(DataOb, rsDateDeath, Msgs) then
      IsFuturePartialDate(DataOb, Hoje, rsDateDeath, LowerCase(rsDateToday), Msgs);
  end;
  if D.FieldByName('banding_date').AsString <> '' then
    ValidDate(D.FieldByName('banding_date').AsString, rsCaptionDate, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

