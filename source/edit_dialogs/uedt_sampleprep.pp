{ Xolmis Sample Preparation Editor dialog

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

unit uedt_sampleprep;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, DB, Character, DateUtils,
  atshapelinebgra, models_specimens;

type

  { TedtSamplePrep }

  TedtSamplePrep = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbSampleType: TComboBox;
    dsLink: TDataSource;
    eAccessionNumber: TEdit;
    eAccessionSeq: TEdit;
    ePreparationDate: TEditButton;
    ePreparer: TEditButton;
    lblAccessionSeq: TLabel;
    lblNotes: TLabel;
    lblAccessionNumber: TLabel;
    lblPreparationDate: TLabel;
    lblPreparer: TLabel;
    lblSampleType: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewPerson: TMenuItem;
    mNotes: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pAccessionNumber: TPanel;
    pAccessionSeq: TPanel;
    pmNew: TPopupMenu;
    pPreparationDate: TPanel;
    pPreparer: TPanel;
    pSampleType: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAccessionNumberEditingDone(Sender: TObject);
    procedure eAccessionNumberKeyPress(Sender: TObject; var Key: char);
    procedure ePreparationDateButtonClick(Sender: TObject);
    procedure ePreparerButtonClick(Sender: TObject);
    procedure ePreparerKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FSamplePrep: TSamplePrep;
    FSpecimen: TSpecimen;
    FSpecimenId, FPreparerId: Integer;
    procedure SetSamplePrep(Value: TSamplePrep);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property SamplePrep: TSamplePrep read FSamplePrep write SetSamplePrep;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
  end;

var
  edtSamplePrep: TedtSamplePrep;

implementation

uses
  utils_locale, utils_global, data_types, utils_dialogs, utils_finddialogs, utils_validations, data_getvalue,
  data_consts, utils_editdialogs, udm_main, udm_grid,
  uDarkStyleParams;

{ TedtSamplePrep }

procedure TedtSamplePrep.ApplyDarkMode;
begin
  ePreparationDate.Images := DMM.iEditsDark;
  ePreparer.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtSamplePrep.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtSamplePrep.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSamplePrep.eAccessionNumberEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSamplePrep.eAccessionNumberKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSamplePrep.ePreparationDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(ePreparationDate.Text, ePreparationDate, Dt);
end;

procedure TedtSamplePrep.ePreparerButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, ePreparer, FPreparerId);
  FSamplePrep.PreparerId := FPreparerId;
end;

procedure TedtSamplePrep.ePreparerKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, ePreparer, FPreparerId, Key);
    FSamplePrep.PreparerId := FPreparerId;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FPreparerId := 0;
    FSamplePrep.PreparerId := FPreparerId;
    ePreparer.Text := EmptyStr;
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

procedure TedtSamplePrep.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSamplePrep.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSamplePrep.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbSampleType.Items.Clear;
  cbSampleType.Items.Add(rsSampleSkinStandard);
  cbSampleType.Items.Add(rsSampleSkinShmoo);
  cbSampleType.Items.Add(rsSampleSkinMounted);
  cbSampleType.Items.Add(rsSampleOpenedWing);
  cbSampleType.Items.Add(rsSampleSkeletonWhole);
  cbSampleType.Items.Add(rsSampleSkeletonPartial);
  cbSampleType.Items.Add(rsSampleNest);
  cbSampleType.Items.Add(rsSampleEgg);
  cbSampleType.Items.Add(rsSampleParasites);
  cbSampleType.Items.Add(rsSampleFeathers);
  cbSampleType.Items.Add(rsSampleBloodDry);
  cbSampleType.Items.Add(rsSampleBloodWet);
  cbSampleType.Items.Add(rsSampleBloodSmear);
  cbSampleType.Items.Add(rsSampleSexing);
  cbSampleType.Items.Add(rsSampleGeneticSequence);
  cbSampleType.Items.Add(rsSampleMicrobialCulture);
  cbSampleType.Items.Add(rsSampleTissues);
  cbSampleType.Items.Add(rsSampleEyes);
  cbSampleType.Items.Add(rsSampleTongue);
  cbSampleType.Items.Add(rsSampleSyrinx);
  cbSampleType.Items.Add(rsSampleGonads);
  cbSampleType.Items.Add(rsSampleStomach);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionEgg)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionEgg)]);
    GetRecord;
  end;
end;

procedure TedtSamplePrep.GetRecord;
begin
  eAccessionNumber.Text := FSamplePrep.AccessionNum;
  eAccessionSeq.Text := IntToStr(FSamplePrep.AccessionSeq);
  case FSamplePrep.AccessionType of
    'NS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSkinStandard);
    'SS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSkinShmoo);
    'MS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSkinMounted);
    'OW':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleOpenedWing);
    'WS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSkeletonWhole);
    'PS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSkeletonPartial);
    'N':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleNest);
    'EGG': cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleEgg);
    'P':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleParasites);
    'F':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleFeathers);
    'BD':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleBloodDry);
    'BL':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleBloodWet);
    'BS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleBloodSmear);
    'SX':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSexing);
    'GS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleGeneticSequence);
    'MC':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleMicrobialCulture);
    'TS':  cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleTissues);
    'EYE': cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleEyes);
    'T':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleTongue);
    'S':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleSyrinx);
    'G':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleGonads);
    'M':   cbSampleType.ItemIndex := cbSampleType.Items.IndexOf(rsSampleStomach);
  end;
  if not DateIsNull(FSamplePrep.PreparationDate) then
    ePreparationDate.Text := DateToStr(FSamplePrep.PreparationDate);
  FPreparerId := FSamplePrep.PreparerId;
  ePreparer.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FPreparerId);
  mNotes.Text := FSamplePrep.Notes;
end;

function TedtSamplePrep.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('accession_num').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('accession_type').AsString <> EmptyStr) then
  if (eAccessionNumber.Text <> EmptyStr) and
    (eAccessionSeq.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtSamplePrep.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtSamplePrep.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtSamplePrep.SetRecord;
begin
  FSamplePrep.AccessionNum := eAccessionNumber.Text;
  if (eAccessionSeq.Text <> EmptyStr) then
    FSamplePrep.AccessionSeq := StrToInt(eAccessionSeq.Text)
  else
    FSamplePrep.AccessionSeq := 0;
  case cbSampleType.ItemIndex of
    0: FSamplePrep.AccessionType := 'NS';
    1: FSamplePrep.AccessionType := 'SS';
    2: FSamplePrep.AccessionType := 'MS';
    3: FSamplePrep.AccessionType := 'OW';
    4: FSamplePrep.AccessionType := 'WS';
    5: FSamplePrep.AccessionType := 'PS';
    6: FSamplePrep.AccessionType := 'N';
    7: FSamplePrep.AccessionType := 'EGG';
    8: FSamplePrep.AccessionType := 'P';
    9: FSamplePrep.AccessionType := 'F';
   10: FSamplePrep.AccessionType := 'BD';
   11: FSamplePrep.AccessionType := 'BL';
   12: FSamplePrep.AccessionType := 'BS';
   13: FSamplePrep.AccessionType := 'SX';
   14: FSamplePrep.AccessionType := 'GS';
   15: FSamplePrep.AccessionType := 'MC';
   16: FSamplePrep.AccessionType := 'TS';
   17: FSamplePrep.AccessionType := 'EYE';
   18: FSamplePrep.AccessionType := 'T';
   19: FSamplePrep.AccessionType := 'S';
   20: FSamplePrep.AccessionType := 'G';
   21: FSamplePrep.AccessionType := 'M';
  end;
  if (ePreparationDate.Text <> EmptyStr) then
    FSamplePrep.PreparationDate := StrToDate(ePreparationDate.Text);
  FSamplePrep.PreparerId := FPreparerId;
  FSamplePrep.Notes := mNotes.Text;

  if FIsNew then
  begin
    FSpecimen := TSpecimen.Create(FSpecimenId);
    try
      FSamplePrep.SpecimenId := FSpecimen.Id;
      FSamplePrep.TaxonId := FSpecimen.TaxonId;
      FSamplePrep.IndividualId := FSpecimen.IndividualId;
      FSamplePrep.NestId := FSpecimen.NestId;
      FSamplePrep.EggId := FSpecimen.EggId;
    finally
      FSpecimen.Free;
    end;
  end;
end;

procedure TedtSamplePrep.SetSamplePrep(Value: TSamplePrep);
begin
  if Assigned(Value) then
    FSamplePrep := Value;
end;

function TedtSamplePrep.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  //RequiredIsEmpty(D, tbSamplePreps, 'accession_num', Msgs);
  //RequiredIsEmpty(D, tbSamplePreps, 'accession_type', Msgs);

  // Duplicated record
  //RecordDuplicated(tbSamplePreps, 'sample_prep_id', 'full_name',
  //  FSamplePrep.FullName, FSamplePrep.Id);

  // Dates
  if ePreparationDate.Text <> EmptyStr then
    ValidDate(ePreparationDate.Text, rsDatePreparation, Msgs);

  if (ePreparationDate.Text <> EmptyStr) then
    IsFutureDate(StrToDate(ePreparationDate.Text), Today,
      AnsiLowerCase(rsDatePreparation), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_sampleprep.lrs}

end.

