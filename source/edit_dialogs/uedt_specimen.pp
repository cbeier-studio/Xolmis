{ Xolmis Specimen Editor dialog

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

unit uedt_specimen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, Character, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Menus, DB, DateUtils,
  atshapelinebgra, models_specimens;

type

  { TedtSpecimen }

  TedtSpecimen = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbSampleType: TComboBox;
    dsLink: TDataSource;
    eCollectionYear: TEdit;
    eCollectionMonth: TEdit;
    eCollectionDay: TEdit;
    eLocality: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eTaxon: TEditButton;
    eIndividual: TEditButton;
    eNest: TEditButton;
    eEgg: TEditButton;
    eFieldNumber: TEdit;
    lblBandStatus4: TLabel;
    lblBandStatus9: TLabel;
    lblCollectionDate1: TLabel;
    lblNotes: TLabel;
    lblFieldNumber: TLabel;
    lblCollectionDate: TLabel;
    lblIndividual: TLabel;
    lblNest: TLabel;
    lblEgg: TLabel;
    lblLocality: TLabel;
    lblSampleType: TLabel;
    lblTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewLocality: TMenuItem;
    pmnNewIndividual: TMenuItem;
    pmnNewNest: TMenuItem;
    pmnNewEgg: TMenuItem;
    mNotes: TMemo;
    pBirthDate: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pFieldNumber: TPanel;
    pCollectionDate: TPanel;
    pIndividual: TPanel;
    pNest: TPanel;
    pEgg: TPanel;
    pLocality: TPanel;
    pmNew: TPopupMenu;
    pStatus4: TPanel;
    pTaxon: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEggButtonClick(Sender: TObject);
    procedure eEggKeyPress(Sender: TObject; var Key: char);
    procedure eFieldNumberEditingDone(Sender: TObject);
    procedure eFieldNumberKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualKeyPress(Sender: TObject; var Key: char);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewEggClick(Sender: TObject);
    procedure pmnNewIndividualClick(Sender: TObject);
    procedure pmnNewLocalityClick(Sender: TObject);
    procedure pmnNewNestClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FSpecimen: TSpecimen;
    FLocalityId, FTaxonId, FIndividualId, FNestId, FEggId: Integer;
    procedure SetSpecimen(Value: TSpecimen);
    procedure GetRecord;
    procedure SetRecord;
    procedure GetFullName;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Specimen: TSpecimen read FSpecimen write SetSpecimen;
    property IndividualId: Integer read FIndividualId write FIndividualId;
  end;

var
  edtSpecimen: TedtSpecimen;

implementation

uses
  utils_locale, utils_global, utils_system, data_types, utils_dialogs, utils_finddialogs, models_geo, models_taxonomy,
  utils_validations, data_consts, data_getvalue, utils_editdialogs, utils_gis, models_record_types,
  udm_main, udm_grid, uDarkStyleParams;

{ TedtSpecimen }

procedure TedtSpecimen.ApplyDarkMode;
begin
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  eNest.Images := DMM.iEditsDark;
  eEgg.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtSpecimen.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtSpecimen.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSpecimen.eEggButtonClick(Sender: TObject);
begin
  FindDlg(tbEggs, eEgg, FEggId);
end;

procedure TedtSpecimen.eEggKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbEggs, eEgg, FEggId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FEggId := 0;
    eEgg.Text := EmptyStr;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eFieldNumberEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSpecimen.eFieldNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TedtSpecimen.eIndividualKeyPress(Sender: TObject; var Key: char);
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
    eIndividual.Text := EmptyStr;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtSpecimen.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Text := EmptyStr;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtSpecimen.eLongitudeKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', ',', '.', '+', '-', #8, #13, #27];
var
  EditText: String;
  PosDecimal: Integer;
  DecimalValue: Extended;
begin
  FormKeyPress(Sender, Key);

  sbSave.Enabled := IsRequiredFilled;

  EditText := EmptyStr;
  PosDecimal := 0;
  DecimalValue := 0;

  if not (Key in AllowedChars) then
  begin
    Key := #0;
    Exit;
  end;

  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
    Exit;
  end;

  if (Sender is TEdit) then
    EditText := TEdit(Sender).Text
  else
  if (Sender is TEditButton) then
    EditText := TEditButton(Sender).Text;
  PosDecimal := Pos(FormatSettings.DecimalSeparator, EditText);

  // Decimal separator
  if (Key in [',', '.']) then
  begin
    if (PosDecimal = 0) then
      Key := FormatSettings.DecimalSeparator
    else
      Key := #0;
    Exit;
  end;

  // Numeric signal
  if (Key in ['+', '-']) then
  begin
    if (Length(EditText) > 0) then
    begin
      if TryStrToFloat(EditText, DecimalValue) then
      begin
        if ((DecimalValue > 0) and (Key = '-')) or ((DecimalValue < 0) and (Key = '+')) then
          DecimalValue := DecimalValue * -1.0;
        EditText := FloatToStr(DecimalValue);

        if (Sender is TEdit) then
        begin
          TEdit(Sender).Text := EditText;
          TEdit(Sender).SelStart := Length(EditText);
        end
        else
        if (Sender is TEditButton) then
        begin
          TEditButton(Sender).Text := EditText;
          TEditButton(Sender).SelStart := Length(EditText);
        end;
      end;
      Key := #0;
    end
    else
    begin
      if (Key = '+') then
        Key := #0;
    end;

    Exit;
  end;
end;

procedure TedtSpecimen.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, FNestId);
end;

procedure TedtSpecimen.eNestKeyPress(Sender: TObject; var Key: char);
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
    eNest.Text := EmptyStr;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, True, FTaxonId);
end;

procedure TedtSpecimen.eTaxonKeyPress(Sender: TObject; var Key: char);
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
    eTaxon.Text := EmptyStr;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSpecimen.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSpecimen.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbSampleType.Items.Clear;
  cbSampleType.Items.Add(rsSpecimenCarcassWhole);
  cbSampleType.Items.Add(rsSpecimenCarcassPartial);
  cbSampleType.Items.Add(rsSpecimenNest);
  cbSampleType.Items.Add(rsSpecimenBones);
  cbSampleType.Items.Add(rsSpecimenEgg);
  cbSampleType.Items.Add(rsSpecimenParasites);
  cbSampleType.Items.Add(rsSpecimenFeathers);
  cbSampleType.Items.Add(rsSpecimenBlood);
  cbSampleType.Items.Add(rsSpecimenClaw);
  cbSampleType.Items.Add(rsSpecimenSwab);
  cbSampleType.Items.Add(rsSpecimenTissues);
  cbSampleType.Items.Add(rsSpecimenFeces);
  cbSampleType.Items.Add(rsSpecimenRegurgite);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionEgg)]);
    if FSpecimen.TaxonId > 0 then
    begin
      FTaxonId := FSpecimen.TaxonId;
      eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
    end;
    if FSpecimen.IndividualId > 0 then
    begin
      FIndividualId := FSpecimen.IndividualId;
      eIndividual.Text := GetName('individuals', COL_FULL_NAME, COL_INDIVIDUAL_ID, FIndividualId);
    end;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionEgg)]);
    GetRecord;
  end;
end;

procedure TedtSpecimen.GetFullName;
var
  TaxonName, SiteName: String;
begin
  TaxonName := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  SiteName := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);

  FSpecimen.FullName := Format('%s-%s, %s, %s', [FSpecimen.FieldNumber, FSpecimen.SampleType, TaxonName, SiteName]);
end;

procedure TedtSpecimen.GetRecord;
begin
  eFieldNumber.Text := FSpecimen.FieldNumber;
  case FSpecimen.SampleType of
    sptWholeCarcass:   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenCarcassWhole);
    sptPartialCarcass: cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenCarcassPartial);
    sptNest:           cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenNest);
    sptBones:          cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenBones);
    sptEgg:            cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenEgg);
    sptParasites:      cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenParasites);
    sptFeathers:       cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenFeathers);
    sptBlood:          cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenBlood);
    sptClaw:           cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenClaw);
    sptSwab:           cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenSwab);
    sptTissues:        cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenTissues);
    sptFeces:          cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenFeces);
    sptRegurgite:      cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSpecimenRegurgite);
  end;
  if (FSpecimen.CollectionYear > 0) then
  begin
    eCollectionYear.Text := IntToStr(FSpecimen.CollectionYear);
    eCollectionMonth.Text := IntToStr(FSpecimen.CollectionMonth);
    eCollectionDay.Text := IntToStr(FSpecimen.CollectionDay);
  end;
  FLocalityId := FSpecimen.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
  if (FSpecimen.Longitude <> 0.0) or (FSpecimen.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FSpecimen.Longitude);
    eLatitude.Text := FloatToStr(FSpecimen.Latitude);
  end;
  FTaxonId := FSpecimen.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FTaxonId);
  FIndividualId := FSpecimen.IndividualId;
  eIndividual.Text := GetName('individuals', COL_FULL_NAME, COL_INDIVIDUAL_ID, FIndividualId);
  FNestId := FSpecimen.NestId;
  eNest.Text := GetName('nests', COL_FULL_NAME, COL_NEST_ID, FNestId);
  FEggId := FSpecimen.EggId;
  eEgg.Text := GetName('eggs', COL_FULL_NAME, COL_EGG_ID, FEggId);
  mNotes.Text := FSpecimen.Notes;
end;

function TedtSpecimen.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('field_number').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('sample_type').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('collection_year').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) then
  if (eFieldNumber.Text <> EmptyStr) and
    (cbSampleType.ItemIndex >= 0) and
    (eCollectionYear.Text <> EmptyStr) and
    (FLocalityId > 0) and
    (FTaxonId > 0) then
    Result := True;
end;

procedure TedtSpecimen.pmnNewEggClick(Sender: TObject);
begin
  EditEgg(DMG.qEggs, 0, True);
end;

procedure TedtSpecimen.pmnNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

procedure TedtSpecimen.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtSpecimen.pmnNewNestClick(Sender: TObject);
begin
  EditNest(DMG.qNests, 0, True);
end;

procedure TedtSpecimen.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtSpecimen.SetRecord;
begin
  FSpecimen.FieldNumber := eFieldNumber.Text;
  case cbSampleType.ItemIndex of
    0: FSpecimen.SampleType := sptWholeCarcass;
    1: FSpecimen.SampleType := sptPartialCarcass;
    2: FSpecimen.SampleType := sptNest;
    3: FSpecimen.SampleType := sptBones;
    4: FSpecimen.SampleType := sptEgg;
    5: FSpecimen.SampleType := sptParasites;
    6: FSpecimen.SampleType := sptFeathers;
    7: FSpecimen.SampleType := sptBlood;
    8: FSpecimen.SampleType := sptClaw;
    9: FSpecimen.SampleType := sptSwab;
   10: FSpecimen.SampleType := sptTissues;
   11: FSpecimen.SampleType := sptFeces;
   12: FSpecimen.SampleType := sptRegurgite;
  end;
  FSpecimen.CollectionYear := StrToInt(eCollectionYear.Text);
  if eCollectionMonth.Text <> EmptyStr then
    FSpecimen.CollectionMonth := StrToInt(eCollectionMonth.Text)
  else
    FSpecimen.CollectionMonth := 0;
  if eCollectionDay.Text <> EmptyStr then
    FSpecimen.CollectionDay := StrToInt(eCollectionDay.Text)
  else
    FSpecimen.CollectionDay := 0;
  FSpecimen.LocalityId := FLocalityId;
  if (Length(eLongitude.Text) > 0) then
    FSpecimen.Longitude := StrToFloat(eLongitude.Text)
  else
    FSpecimen.Longitude := 0;
  if (Length(eLatitude.Text) > 0) then
    FSpecimen.Latitude := StrToFloat(eLatitude.Text)
  else
    FSpecimen.Latitude := 0;
  FSpecimen.TaxonId := FTaxonId;
  FSpecimen.IndividualId := FIndividualId;
  FSpecimen.NestId := FNestId;
  FSpecimen.EggId := FEggId;
  FSpecimen.Notes := mNotes.Text;

  GetFullName;
end;

procedure TedtSpecimen.SetSpecimen(Value: TSpecimen);
begin
  if Assigned(Value) then
    FSpecimen := Value;
end;

function TedtSpecimen.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
  DateColl, Hoje: TPartialDate;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  //RequiredIsEmpty(D, tbSpecimens, 'field_number', Msgs);
  //RequiredIsEmpty(D, tbSpecimens, 'sample_type', Msgs);
  //RequiredIsEmpty(D, tbSpecimens, 'collection_year', Msgs);
  //RequiredIsEmpty(D, tbSpecimens, 'taxon_id', Msgs);
  //RequiredIsEmpty(D, tbSpecimens, 'locality_id', Msgs);

  // Duplicated record
  //RecordDuplicated(tbSpecimens, 'specimen_id', 'full_name',
  //  D.FieldByName('full_name').AsString, D.FieldByName('specimen_id').AsInteger);

  // Dates
  Hoje.Today;
  if eCollectionYear.Text <> EmptyStr then
  begin
    DateColl.Year := StrToInt(eCollectionYear.Text);
    if eCollectionMonth.Text <> EmptyStr then
      DateColl.Month := StrToInt(eCollectionMonth.Text);
    if eCollectionDay.Text <> EmptyStr then
      DateColl.Day := StrToInt(eCollectionDay.Text);
    if ValidPartialDate(DateColl, rsDateCollection, Msgs) then
      IsFuturePartialDate(DateColl, Hoje, rsDateCollection, LowerCase(rsDateToday), Msgs);
  end;

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_specimen.lrs}

end.

