{ Xolmis Individual Editor dialog

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

unit uedt_individual;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, atshapelinebgra, Buttons, LCLType, cbs_birds;

type

  { TedtIndividual }

  TedtIndividual = class(TForm)
    cbSex: TComboBox;
    cbAge: TComboBox;
    eBand: TEditButton;
    eBandingDate: TEditButton;
    eDeathDay: TEdit;
    eDeathMonth: TEdit;
    eBirthYear: TEdit;
    eBirthMonth: TEdit;
    eBirthDay: TEdit;
    eDeathYear: TEdit;
    eRightTarsus: TEditButton;
    eLeftTarsus: TEditButton;
    eRightTibia: TEditButton;
    eLeftTibia: TEditButton;
    eRemovedBand: TEditButton;
    eBandChangeDate: TEditButton;
    eDoubleBand: TEditButton;
    eTaxon: TEditButton;
    eMother: TEditButton;
    eFather: TEditButton;
    eNest: TEditButton;
    dsLink: TDataSource;
    lblDoubleBand1: TLabel;
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
    mRecognizableMarkings: TMemo;
    mNotes: TMemo;
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
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FIndividual: TIndividual;
    FTaxonId, FNestId, FFatherId, FMotherId: Integer;
    FBandId, FDoubleBandId, FRemovedBandId: Integer;
    procedure SetIndividual(Value: TIndividual);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Individual: TIndividual read FIndividual write SetIndividual;
  end;

var
  edtIndividual: TedtIndividual;

implementation

uses
  cbs_locale, cbs_global, cbs_system, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_getvalue,
  cbs_validations, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtIndividual }

procedure TedtIndividual.ApplyDarkMode;
begin
  eTaxon.Images := DMM.iEditsDark;
  eBand.Images := DMM.iEditsDark;
  eBandingDate.Images := DMM.iEditsDark;
  eDoubleBand.Images := DMM.iEditsDark;
  eRemovedBand.Images := DMM.iEditsDark;
  eBandChangeDate.Images := DMM.iEditsDark;
  eRightTarsus.Images := DMM.iEditsDark;
  eLeftTarsus.Images := DMM.iEditsDark;
  eRightTibia.Images := DMM.iEditsDark;
  eLeftTibia.Images := DMM.iEditsDark;
  eNest.Images := DMM.iEditsDark;
  eFather.Images := DMM.iEditsDark;
  eMother.Images := DMM.iEditsDark;
end;

procedure TedtIndividual.cbSexKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtIndividual.eBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eBand, FBandId);
end;

procedure TedtIndividual.eBandChangeDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eBandChangeDate.Text, eBandChangeDate, Dt);
end;

procedure TedtIndividual.eBandingDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eBandingDate.Text, eBandingDate, Dt);
end;

procedure TedtIndividual.eBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbBands, eBand, FBandId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FBandId := 0;
    eBand.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eDoubleBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eDoubleBand, FDoubleBandId);
end;

procedure TedtIndividual.eDoubleBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbBands, eDoubleBand, FDoubleBandId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FDoubleBandId := 0;
    eDoubleBand.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eFatherButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eFather, FFatherId);
end;

procedure TedtIndividual.eFatherKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eFather, FFatherId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FFatherId := 0;
    eFather.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eLeftTarsusButtonClick(Sender: TObject);
begin
  EditColorBands(eLeftTarsus);
end;

procedure TedtIndividual.eLeftTibiaButtonClick(Sender: TObject);
begin
  EditColorBands(eLeftTibia);
end;

procedure TedtIndividual.eMotherButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eMother, FMotherId);
end;

procedure TedtIndividual.eMotherKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbIndividuals, eMother, FMotherId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FMotherId := 0;
    eMother.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, FNestId);
end;

procedure TedtIndividual.eNestKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbNests, eNest, FNestId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FNestId := 0;
    eNest.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eRemovedBandButtonClick(Sender: TObject);
begin
  FindDlg(tbBands, eRemovedBand, FRemovedBandId);
end;

procedure TedtIndividual.eRemovedBandKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if ((IsLetter(Key)) or (IsNumber(Key)) or (IsPunctuation(Key)) or
    (IsSeparator(Key)) or (IsSymbol(Key))) then
  begin
    FindDlg(tbBands, eRemovedBand, FRemovedBandId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FRemovedBandId := 0;
    eRemovedBand.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.eRightTarsusButtonClick(Sender: TObject);
begin
  EditColorBands(eRightTarsus);
end;

procedure TedtIndividual.eRightTibiaButtonClick(Sender: TObject);
begin
  EditColorBands(eRightTibia);
end;

procedure TedtIndividual.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, True, FTaxonId);
end;

procedure TedtIndividual.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies, tfSubspecies, tfSubspeciesGroups], eTaxon, True, FTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtIndividual.FormCreate(Sender: TObject);
begin
  cbSex.Items.CommaText := rsSexMale + ',' + rsSexFemale + ',' + rsSexUnknown;
  cbAge.Items.CommaText := rsAgeUnknown + ',' + rsAgeAdult + ',' + rsAgeJuvenile + ',' +
    rsAgeFledgling + ',' + rsAgeNestling + ',"' + rsAgeFirstYear + '","' + rsAgeSecondYear + '","' +
    rsAgeThirdYear + '","' + rsAgeFourthYear + '","' + rsAgeFifthYear + '"';
end;

procedure TedtIndividual.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionIndividual)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionIndividual)]);
    GetRecord;
  end;
end;

procedure TedtIndividual.GetRecord;
begin
  FTaxonId := FIndividual.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
  FBandId := FIndividual.BandId;
  eBand.Text := GetName('bands', 'full_name', 'band_id', FBandId);
  if DateIsNull(FIndividual.BandingDate) then
    eBandingDate.Text := EmptyStr
  else
    eBandingDate.Text := DateToStr(FIndividual.BandingDate);
  FDoubleBandId := FIndividual.DoubleBandId;
  eDoubleBand.Text := GetName('bands', 'full_name', 'band_id', FDoubleBandId);
  FRemovedBandId := FIndividual.RemovedBandId;
  eRemovedBand.Text := GetName('bands', 'full_name', 'band_id', FRemovedBandId);
  if DateIsNull(FIndividual.BandChangeDate) then
    eBandChangeDate.Text := EmptyStr
  else
    eBandChangeDate.Text := DateToStr(FIndividual.BandChangeDate);
  eRightTarsus.Text := FIndividual.RightLegBelow;
  eLeftTarsus.Text := FIndividual.LeftLegBelow;
  eRightTibia.Text := FIndividual.RightLegAbove;
  eLeftTibia.Text := FIndividual.LeftLegAbove;
  eBirthYear.Text := IntToStr(FIndividual.BirthYear);
  eBirthMonth.Text := IntToStr(FIndividual.BirthMonth);
  eBirthDay.Text := IntToStr(FIndividual.BirthDay);
  eDeathYear.Text := IntToStr(FIndividual.DeathYear);
  eDeathMonth.Text := IntToStr(FIndividual.DeathMonth);
  eDeathDay.Text := IntToStr(FIndividual.DeathDay);
  case FIndividual.Sex of
    sexMale: cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexMale);
    sexFemale: cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexFemale);
    sexUnknown: cbSex.ItemIndex := cbSex.Items.IndexOf(rsSexUnknown);
  end;
  case FIndividual.Age of
    ageUnknown: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeUnknown);
    ageAdult: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeAdult);
    ageJuvenile: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeJuvenile);
    ageFledgling: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFledgling);
    ageNestling: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeNestling);
    ageFirstYear: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFirstYear);
    ageSecondYear: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeSecondYear);
    ageThirdYear: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeThirdYear);
    ageFourthYear: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFourthYear);
    ageFifthYear: cbAge.ItemIndex := cbAge.Items.IndexOf(rsAgeFifthYear);
  end;
  FNestId := FIndividual.NestId;
  eNest.Text := GetName('nests', 'full_name', 'nest_id', FNestId);
  FFatherId := FIndividual.FatherId;
  eFather.Text := GetName('individuals', 'full_name', 'individual_id', FFatherId);
  FMotherId := FIndividual.MotherId;
  eMother.Text := GetName('individuals', 'full_name', 'individual_id', FMotherId);
  mRecognizableMarkings.Text := FIndividual.RecognizableMarkings;
  mNotes.Text := FIndividual.Notes;
end;

function TedtIndividual.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) then
  if (FTaxonId > 0) then
    Result := True;
end;

procedure TedtIndividual.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtIndividual.SetIndividual(Value: TIndividual);
begin
  if Assigned(Value) then
    FIndividual := Value;
end;

procedure TedtIndividual.SetRecord;
begin
  FIndividual.TaxonId        := FTaxonId;
  FIndividual.BandId         := FBandId;
  if eBandingDate.Text = EmptyStr then
    FIndividual.BandingDate  := NullDate
  else
    FIndividual.BandingDate  := StrToDate(eBandingDate.Text);
  FIndividual.DoubleBandId   := FDoubleBandId;
  FIndividual.RemovedBandId  := FRemovedBandId;
  if eBandChangeDate.Text = EmptyStr then
    FIndividual.BandChangeDate    := NullDate
  else
    FIndividual.BandChangeDate := StrToDate(eBandChangeDate.Text);
  FIndividual.RightLegBelow  := eRightTarsus.Text;
  FIndividual.LeftLegBelow   := eLeftTarsus.Text;
  FIndividual.RightLegAbove  := eRightTibia.Text;
  FIndividual.LeftLegAbove   := eLeftTibia.Text;
  FIndividual.BirthYear      := StrToInt(eBirthYear.Text);
  FIndividual.BirthMonth     := StrToInt(eBirthMonth.Text);
  FIndividual.BirthDay       := StrToInt(eBirthDay.Text);
  FIndividual.DeathYear      := StrToInt(eDeathYear.Text);
  FIndividual.DeathMonth     := StrToInt(eDeathMonth.Text);
  FIndividual.DeathDay       := StrToInt(eDeathDay.Text);
  case cbSex.ItemIndex of
    0: FIndividual.Sex := sexMale;
    1: FIndividual.Sex := sexFemale;
    2: FIndividual.Sex := sexUnknown;
  end;
  case cbAge.ItemIndex of
    0: FIndividual.Age := ageUnknown;
    1: FIndividual.Age := ageAdult;
    2: FIndividual.Age := ageJuvenile;
    3: FIndividual.Age := ageFledgling;
    4: FIndividual.Age := ageNestling;
    5: FIndividual.Age := ageFirstYear;
    6: FIndividual.Age := ageSecondYear;
    7: FIndividual.Age := ageThirdYear;
    8: FIndividual.Age := ageFourthYear;
    9: FIndividual.Age := ageFifthYear;
  end;
  FIndividual.NestId               := FNestId;
  FIndividual.FatherId             := FFatherId;
  FIndividual.MotherId             := FMotherId;
  FIndividual.RecognizableMarkings := mRecognizableMarkings.Text;
  FIndividual.Notes                := mNotes.Text;
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
  //RequiredIsEmpty(D, tbIndividuals, 'taxon_id', Msgs);
  // RequiredIsEmpty(D, tbIndividuals, 'band_id', Msgs);

  { Duplicated record }
  //if (FBandId > 0) then
  //  RecordDuplicated(tbIndividuals, 'individual_id', 'band_id', FBandId, FIndividual.Id, Msgs);

  { Foreign keys }
  //ForeignValueExists(tbZooTaxa, 'taxon_id', D.FieldByName('taxon_id').AsInteger,
  //  rsCaptionTaxon, Msgs);
  //ForeignValueExists(tbBands, 'band_id', D.FieldByName('band_id').AsInteger,
  //  rsCaptionBand, Msgs);
  //ForeignValueExists(tbBands, 'band_id', D.FieldByName('double_band_id').AsInteger,
  //  rsCaptionDoubleBand, Msgs);
  //ForeignValueExists(tbBands, 'band_id', D.FieldByName('removed_band_id').AsInteger,
  //  rsCaptionRemovedBand, Msgs);
  //ForeignValueExists(tbNests, 'nest_id', D.FieldByName('nest_id').AsInteger,
  //  rsCaptionNest, Msgs);
  //ForeignValueExists(tbIndividuals, 'individual_id', D.FieldByName('father_id').AsInteger,
  //  rsCaptionFather, Msgs);
  //ForeignValueExists(tbIndividuals, 'individual_id', D.FieldByName('mother_id').AsInteger,
  //  rsCaptionMother, Msgs);

  { Dates }
  Hoje.Today;
  if eBirthYear.Text <> EmptyStr then
  begin
    DataNasc.Year := StrToInt(eBirthYear.Text);
    if eBirthMonth.Text = EmptyStr then
      DataNasc.Month := 0
    else
      DataNasc.Month := StrToInt(eBirthMonth.Text);
    if eBirthDay.Text = EmptyStr then
      DataNasc.Day := 0
    else
      DataNasc.Day := StrToInt(eBirthDay.Text);
    if ValidPartialDate(DataNasc, rsDateBirth, Msgs) then
      IsFuturePartialDate(DataNasc, Hoje, rsDateBirth, LowerCase(rsDateToday), Msgs);
  end;
  if eDeathYear.Text <> EmptyStr then
  begin
    DataOb.Year := StrToInt(eDeathYear.Text);
    if eDeathMonth.Text = EmptyStr then
      DataOb.Month := 0
    else
      DataOb.Month := StrToInt(eDeathMonth.Text);
    if eDeathDay.Text = EmptyStr then
      DataOb.Day := 0
    else
      DataOb.Day := StrToInt(eDeathDay.Text);
    if ValidPartialDate(DataOb, rsDateDeath, Msgs) then
      IsFuturePartialDate(DataOb, Hoje, rsDateDeath, LowerCase(rsDateToday), Msgs);
  end;
  if eBandingDate.Text <> EmptyStr then
    ValidDate(eBandingDate.Text, rsDateBanding, Msgs);
  if eBandChangeDate.Text <> EmptyStr then
    ValidDate(eBandChangeDate.Text, rsDateBandChange, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

