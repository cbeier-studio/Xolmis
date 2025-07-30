{ Xolmis Egg Editor dialog

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

unit uedt_egg;

{$mode objfpc}{$H+}

interface

uses
  BCPanel, Classes, EditBtn, Spin, SysUtils, DB, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Menus, DateUtils, Character,
  atshapelinebgra, cbs_breeding;

type

  { TedtEgg }

  TedtEgg = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    ckHatched: TCheckBox;
    cbShape: TComboBox;
    cbShellPattern: TComboBox;
    cbShellTexture: TComboBox;
    eNest: TEditButton;
    eStage: TEdit;
    eShellColor: TEdit;
    eMeasureDate: TEditButton;
    eTaxon: TEditButton;
    eObserver: TEditButton;
    eIndividual: TEditButton;
    eFieldNumber: TEdit;
    eMass: TFloatSpinEdit;
    eWidth: TFloatSpinEdit;
    eLength: TFloatSpinEdit;
    eEggSeq: TSpinEdit;
    lblNest: TLabel;
    pmnNewNest: TMenuItem;
    pmnNewPerson: TMenuItem;
    pmnNewIndividual: TMenuItem;
    pNest: TBCPanel;
    pmNew: TPopupMenu;
    txtVolume: TLabel;
    lblFieldNumber1: TLabel;
    lblLength: TLabel;
    lblMeasureDate: TLabel;
    lblShellPattern: TLabel;
    lblShellTexture1: TLabel;
    lblStage: TLabel;
    dsLink: TDataSource;
    lblFieldNumber: TLabel;
    lblVolume: TLabel;
    lblWeight: TLabel;
    lblWidth: TLabel;
    lblEggSeq: TLabel;
    lblTaxon: TLabel;
    lblObserver: TLabel;
    lblIndividual: TLabel;
    lblShape: TLabel;
    lblShellColor: TLabel;
    lblNotes: TLabel;
    lblShellTexture: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TMemo;
    pClient: TPanel;
    pBottom: TPanel;
    pEggSeq: TPanel;
    pFieldNumber: TPanel;
    pHatched: TPanel;
    pIndividual: TPanel;
    pLength: TPanel;
    pNotes: TPanel;
    pObserver: TPanel;
    pShape: TPanel;
    pShellColor: TPanel;
    pShellTexture: TPanel;
    pTaxon: TPanel;
    pWeight: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    sBox: TScrollBox;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEggSeqEditingDone(Sender: TObject);
    procedure eFieldNumberKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eMeasureDateButtonClick(Sender: TObject);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure eWidthEditingDone(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewIndividualClick(Sender: TObject);
    procedure pmnNewNestClick(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FEgg: TEgg;
    FNestId, FObserverId, FTaxonId, FIndividualId: Integer;
    procedure SetEgg(Value: TEgg);
    procedure GetRecord;
    procedure SetRecord;
    procedure GetVolume;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Egg: TEgg read FEgg write SetEgg;
    property NestId: Integer read FNestId write FNestId;
  end;

var
  edtEgg: TedtEgg;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_validations, cbs_getvalue,
  cbs_dataconst, cbs_themes, cbs_editdialogs, udm_main, udm_grid, udm_breeding, uDarkStyleParams;

{$R *.lfm}

{ TedtEgg }

procedure TedtEgg.ApplyDarkMode;
begin
  pNest.Background.Color := clSolidBGSecondaryDark;
  pNest.Border.Color := clSystemSolidNeutralFGDark;

  eNest.Images := DMM.iEditsDark;
  eMeasureDate.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eObserver.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtEgg.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtEgg.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtEgg.eEggSeqEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtEgg.eFieldNumberKeyPress(Sender: TObject; var Key: char);
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

procedure TedtEgg.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TedtEgg.eIndividualKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, FIndividualId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FIndividualId := 0;
    eIndividual.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eMeasureDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eMeasureDate.Text, eMeasureDate, Dt);
end;

procedure TedtEgg.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, FNestId);
end;

procedure TedtEgg.eNestKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNests, eNest, FNestId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FNestId := 0;
    eNest.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverId);
end;

procedure TedtEgg.eObserverKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, FObserverId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserverId := 0;
    eObserver.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtEgg.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, True, FTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtEgg.eWidthEditingDone(Sender: TObject);
begin
  GetVolume;
end;

procedure TedtEgg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not sbSave.Enabled then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtEgg.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtEgg.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  pNest.Visible := FNestId = 0;
  pmnNewNest.Visible := FNestId = 0;

  with cbShape.Items do
  begin
    Clear;
    Add(rsEggSpherical);
    Add(rsEggElliptical);
    Add(rsEggOval);
    Add(rsEggPyriform);
    Add(rsEggConical);
    Add(rsEggBiconical);
    Add(rsEggCylindrical);
    Add(rsEggLongitudinal);
    Add(rsEggUnknown);
  end;
  with cbShellTexture.Items do
  begin
    Clear;
    Add(rsEggChalky);
    Add(rsEggShiny);
    Add(rsEggGlossy);
    Add(rsEggPitted);
    Add(rsEggUnknown);
  end;
  with cbShellPattern.Items do
  begin
    Clear;
    Add(rsEggSpots);
    Add(rsEggBlotches);
    Add(rsEggSquiggles);
    Add(rsEggStreaks);
    Add(rsEggScrawls);
    Add(rsEggSpotsSquiggles);
    Add(rsEggBlotchesSquiggles);
    Add(rsEggUnknown);
  end;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionEgg)]);
    if FEgg.TaxonId > 0 then
    begin
      FTaxonId := FEgg.TaxonId;
      eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
    end;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionEgg)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtEgg.GetRecord;
begin
  FNestId := FEgg.NestId;
  if pNest.Visible then
    eNest.Text := GetName('nests', COL_FULL_NAME, COL_NEST_ID, FNestId);
  eFieldNumber.Text := FEgg.FieldNumber;
  eEggSeq.Value := FEgg.EggSeq;
  if not DateIsNull(FEgg.MeasureDate) then
    eMeasureDate.Text := DateToStr(FEgg.MeasureDate);
  FTaxonId := FEgg.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  FObserverId := FEgg.ResearcherId;
  eObserver.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FObserverId);
  case FEgg.EggShape of
    esUnknown: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggUnknown);
    esSpherical: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggSpherical);
    esElliptical: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggElliptical);
    esOval: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggOval);
    esPiriform: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggPyriform);
    esConical: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggConical);
    esBiconical: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggBiconical);
    esCylindrical: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggCylindrical);
    esLongitudinal: cbShape.ItemIndex := cbShape.Items.IndexOf(rsEggLongitudinal);
  end;
  eStage.Text := FEgg.EggStage;
  eShellColor.Text := FEgg.EggshellColor;
  case FEgg.EggshellPattern of
    espUnknown: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggUnknown);
    espSpots: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggSpots);
    espBlotches: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggBlotches);
    espSquiggles: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggSquiggles);
    espStreaks: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggStreaks);
    espScrawls: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggScrawls);
    espSpotsSquiggles: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggSpotsSquiggles);
    espBlotchesSquiggles: cbShellPattern.ItemIndex := cbShellPattern.Items.IndexOf(rsEggBlotchesSquiggles);
  end;
  case FEgg.EggshellTexture of
    estUnknown: cbShellTexture.ItemIndex := cbShellTexture.Items.IndexOf(rsEggUnknown);
    estChalky: cbShellTexture.ItemIndex := cbShellTexture.Items.IndexOf(rsEggChalky);
    estShiny: cbShellTexture.ItemIndex := cbShellTexture.Items.IndexOf(rsEggShiny);
    estGlossy: cbShellTexture.ItemIndex := cbShellTexture.Items.IndexOf(rsEggGlossy);
    estPitted: cbShellTexture.ItemIndex := cbShellTexture.Items.IndexOf(rsEggPitted);
  end;
  eWidth.Value := FEgg.Width;
  eLength.Value := FEgg.Length;
  eMass.Value := FEgg.Mass;
  txtVolume.Caption := FormatFloat('##0.000', FEgg.Volume);
  ckHatched.Checked := FEgg.EggHatched;
  FIndividualId := FEgg.IndividualId;
  eIndividual.Text := GetName('individuals', COL_FULL_NAME, COL_INDIVIDUAL_ID, FIndividualId);
  mNotes.Text := FEgg.Notes;
end;

procedure TedtEgg.GetVolume;
var
  Vol: Double;
begin
  Vol := 0.0;

  if (eWidth.Value > 0) and (eLength.Value > 0) then
    Vol := 0.51 * eLength.Value * Sqr(eWidth.Value);

  txtVolume.Caption := FormatFloat('##0.000', Vol);
end;

function TedtEgg.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('egg_seq').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('researcher_id').AsInteger <> 0) then
  if (eEggSeq.Value > 0) and
    (FTaxonId > 0) and
    (FObserverId > 0) then
    Result := True;
end;

procedure TedtEgg.pmnNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

procedure TedtEgg.pmnNewNestClick(Sender: TObject);
begin
  EditNest(DMG.qNests, 0, True);
end;

procedure TedtEgg.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtEgg.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtEgg.SetEgg(Value: TEgg);
begin
  if Assigned(Value) then
    FEgg := Value;
end;

procedure TedtEgg.SetRecord;
begin
  FEgg.NestId       := FNestId;
  FEgg.FieldNumber  := eFieldNumber.Text;
  FEgg.EggSeq       := eEggSeq.Value;
  FEgg.MeasureDate  := StrToDate(eMeasureDate.Text);
  FEgg.TaxonId      := FTaxonId;
  FEgg.ResearcherId := FObserverId;
  case cbShape.ItemIndex of
    0: FEgg.EggShape := esSpherical;
    1: FEgg.EggShape := esElliptical;
    2: FEgg.EggShape := esOval;
    3: FEgg.EggShape := esPiriform;
    4: FEgg.EggShape := esConical;
    5: FEgg.EggShape := esBiconical;
    6: FEgg.EggShape := esCylindrical;
    7: FEgg.EggShape := esLongitudinal;
    8: FEgg.EggShape := esUnknown;
  end;
  FEgg.EggStage      := eStage.Text;
  FEgg.EggshellColor := eShellColor.Text;
  case cbShellPattern.ItemIndex of
    0: FEgg.EggshellPattern := espSpots;
    1: FEgg.EggshellPattern := espBlotches;
    2: FEgg.EggshellPattern := espSquiggles;
    3: FEgg.EggshellPattern := espStreaks;
    4: FEgg.EggshellPattern := espScrawls;
    5: FEgg.EggshellPattern := espSpotsSquiggles;
    6: FEgg.EggshellPattern := espBlotchesSquiggles;
    7: FEgg.EggshellPattern := espUnknown;
  end;
  case cbShellTexture.ItemIndex of
    0: FEgg.EggshellTexture := estChalky;
    1: FEgg.EggshellTexture := estShiny;
    2: FEgg.EggshellTexture := estGlossy;
    3: FEgg.EggshellTexture := estPitted;
    4: FEgg.EggshellTexture := estUnknown;
  end;
  FEgg.Width        := eWidth.Value;
  FEgg.Length       := eLength.Value;
  FEgg.Mass         := eMass.Value;
  FEgg.EggHatched   := ckHatched.Checked;
  FEgg.IndividualId := FIndividualId;
  FEgg.Notes        := mNotes.Text;
end;

function TedtEgg.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  //RequiredIsEmpty(D, tbEggs, 'egg_seq', Msgs);
  //RequiredIsEmpty(D, tbEggs, 'taxon_id', Msgs);
  //RequiredIsEmpty(D, tbEggs, 'researcher_id', Msgs);

  // Duplicated record
  //RecordDuplicated(tbEggs, 'egg_id', 'full_name', FEgg.FullName, FEgg.Id);

  // Dates
  if (eMeasureDate.Text <> EmptyStr) then
  begin
    ValidDate(eMeasureDate.Text, rsDateMeasured, Msgs);
    IsFutureDate(StrToDate(eMeasureDate.Text), Today, AnsiLowerCase(rsDateMeasured), AnsiLowerCase(rsDateToday), Msgs);
  end;

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

