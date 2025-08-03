{ Xolmis Nest Revision Editor dialog

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

unit uedt_nestrevision;

{$mode objfpc}{$H+}

interface

uses
  BCPanel, Classes, EditBtn, Spin, SysUtils, Character, DB, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, Menus, DateUtils,
  atshapelinebgra, models_breeding;

type

  { TedtNestRevision }

  TedtNestRevision = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    ckHasPhilornisLarvae: TCheckBox;
    cbNestStage: TComboBox;
    cbNestStatus: TComboBox;
    eNest: TEditButton;
    eRevisionTime: TEdit;
    eRevisionDate: TEditButton;
    eObserver1: TEditButton;
    eObserver2: TEditButton;
    eNidoparasite: TEditButton;
    dsLink: TDataSource;
    lblHostNestlingsTally: TLabel;
    lblNest: TLabel;
    lblNestStage: TLabel;
    lblNestStatus: TLabel;
    lblNidoparasiteEggsTally: TLabel;
    lblHostEggsTally: TLabel;
    lblNidoparasiteNestlingsTally: TLabel;
    lblObserver2: TLabel;
    lblRevisionDate: TLabel;
    lblNidoparasite: TLabel;
    lblObserver1: TLabel;
    lblNotes: TLabel;
    lblRevisionTime: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewNest: TMenuItem;
    pmnNewPerson: TMenuItem;
    mNotes: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pHostEggsTally: TPanel;
    pNest: TBCPanel;
    pNidoparasiteEggsTally: TPanel;
    pNestStage: TPanel;
    pmNew: TPopupMenu;
    pPhilornis: TPanel;
    pRevisionDate: TPanel;
    pNotes: TPanel;
    pObserver1: TPanel;
    pNidoparasite: TPanel;
    sbCancel: TButton;
    sBox: TScrollBox;
    sbSave: TButton;
    eHostEggsTally: TSpinEdit;
    eHostNestlingsTally: TSpinEdit;
    eNidoparasiteEggsTally: TSpinEdit;
    eNidoparasiteNestlingsTally: TSpinEdit;
    procedure btnNewClick(Sender: TObject);
    procedure cbNestStageSelect(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestKeyPress(Sender: TObject; var Key: char);
    procedure eNidoparasiteButtonClick(Sender: TObject);
    procedure eNidoparasiteKeyPress(Sender: TObject; var Key: char);
    procedure eObserver1ButtonClick(Sender: TObject);
    procedure eObserver1KeyPress(Sender: TObject; var Key: char);
    procedure eObserver2ButtonClick(Sender: TObject);
    procedure eObserver2KeyPress(Sender: TObject; var Key: char);
    procedure eRevisionDateButtonClick(Sender: TObject);
    procedure eRevisionDateEditingDone(Sender: TObject);
    procedure eRevisionTimeKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewNestClick(Sender: TObject);
    procedure pmnNewPersonClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FRevision: TNestRevision;
    FNestId, FObserver1Id, FObserver2Id, FNidoparasiteId: Integer;
    procedure SetRevision(Value: TNestRevision);
    procedure GetRecord;
    procedure SetRecord;
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property NestRevision: TNestRevision read FRevision write SetRevision;
    property NestId: Integer read FNestId write FNestId;
  end;

var
  edtNestRevision: TedtNestRevision;

implementation

uses
  utils_locale, utils_global, data_types, utils_dialogs, utils_finddialogs, models_taxonomy, data_getvalue, data_consts,
  utils_themes, utils_validations, utils_editdialogs, udm_breeding, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtNestRevision }

procedure TedtNestRevision.ApplyDarkMode;
begin
  pNest.Background.Color := clSolidBGSecondaryDark;
  pNest.Border.Color := clSystemSolidNeutralFGDark;

  eNest.Images := DMM.iEditsDark;
  eRevisionDate.Images := DMM.iEditsDark;
  eObserver1.Images := DMM.iEditsDark;
  eObserver2.Images := DMM.iEditsDark;
  eNidoparasite.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtNestRevision.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtNestRevision.cbNestStageSelect(Sender: TObject);
begin
  if cbNestStage.Text = rsNestInactive then
    cbNestStatus.ItemIndex := cbNestStatus.Items.IndexOf(rsNestInactive);
end;

procedure TedtNestRevision.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNestRevision.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, FNestId);
end;

procedure TedtNestRevision.eNestKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestRevision.eNidoparasiteButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eNidoparasite, True, FNidoparasiteId);
end;

procedure TedtNestRevision.eNidoparasiteKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eNidoparasite, True, FNidoparasiteId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FNidoparasiteId := 0;
    eNidoparasite.Clear;
    Key := #0;
  end;
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

procedure TedtNestRevision.eObserver1ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver1, FObserver1Id);
end;

procedure TedtNestRevision.eObserver1KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver1, FObserver1Id, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserver1Id := 0;
    eObserver1.Clear;
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

procedure TedtNestRevision.eObserver2ButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver2, FObserver2Id);
end;

procedure TedtNestRevision.eObserver2KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver2, FObserver2Id, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserver2Id := 0;
    eObserver2.Clear;
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

procedure TedtNestRevision.eRevisionDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eRevisionDate.Text, eRevisionDate, Dt);
end;

procedure TedtNestRevision.eRevisionDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNestRevision.eRevisionTimeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtNestRevision.FormCreate(Sender: TObject);
begin
  cbNestStage.Items.CommaText := '"' + rsNestBuilding + '","' + rsNestLaying + '","' + rsNestIncubating +
    '","' + rsNestHatching + '","' + rsNestNestling + '","' + rsNestInactive + '","' + rsNestUnknown+ '"';
  cbNestStatus.Items.CommaText := '"' + rsNestActive + '","' + rsNestInactive + '","' + rsNestUnknown + '"';
end;

procedure TedtNestRevision.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtNestRevision.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNestRevision.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  pNest.Visible := FNestId = 0;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionNestRevision)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionNestRevision)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;

  sBox.VertScrollBar.Position := 0;

  eRevisionDate.SetFocus;
end;

procedure TedtNestRevision.GetRecord;
begin
  FNestId := FRevision.NestId;
  if pNest.Visible then
    eNest.Text := GetName('nests', COL_FULL_NAME, COL_NEST_ID, FNestId);
  if not DateIsNull(FRevision.RevisionDate) then
    eRevisionDate.Text := DateToStr(FRevision.RevisionDate);
  if not TimeIsNull(FRevision.RevisionTime) then
    eRevisionTime.Text := FormatDateTime('hh:nn', FRevision.RevisionTime);
  FObserver1Id := FRevision.Observer1Id;
  eObserver1.Text := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, FRevision.Observer1Id);
  FObserver2Id := FRevision.Observer2Id;
  eObserver2.Text := GetName('people', COL_ABBREVIATION, COL_PERSON_ID, FRevision.Observer2Id);
  case FRevision.NestStage of
    nsgInactive:      cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestInactive);
    nsgConstruction:  cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestBuilding);
    nsgLaying:        cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestLaying);
    nsgIncubation:    cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestIncubating);
    nsgHatching:      cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestHatching);
    nsgNestling:      cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestNestling);
    nsgUnknown:       cbNestStage.ItemIndex := cbNestStage.Items.IndexOf(rsNestUnknown);
  end;
  case FRevision.NestStatus of
    nstInactive:  cbNestStatus.ItemIndex := cbNestStatus.Items.IndexOf(rsNestInactive);
    nstActive:    cbNestStatus.ItemIndex := cbNestStatus.Items.IndexOf(rsNestActive);
    nstUnknown:   cbNestStatus.ItemIndex := cbNestStatus.Items.IndexOf(rsNestUnknown);
  end;
  eHostEggsTally.Value := FRevision.HostEggsTally;
  eHostNestlingsTally.Value := FRevision.HostNestlingsTally;
  FNidoparasiteId := FRevision.NidoparasiteId;
  eNidoparasite.Text := GetName('zoo_taxa', COL_FULL_NAME, COL_TAXON_ID, FNidoparasiteId);
  eNidoparasiteEggsTally.Value := FRevision.NidoparasiteEggsTally;
  eNidoparasiteNestlingsTally.Value := FRevision.NidoparasiteNestlingsTally;
  ckHasPhilornisLarvae.Checked := FRevision.HavePhilornisLarvae;
  mNotes.Text := FRevision.Notes;
end;

function TedtNestRevision.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('revision_date').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('observer_1_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('nest_stage').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('nest_status').AsString <> EmptyStr) then
  if (eRevisionDate.Text <> EmptyStr) and
    (FObserver1Id > 0) and
    (cbNestStage.ItemIndex >= 0) and
    (cbNestStatus.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtNestRevision.pmnNewNestClick(Sender: TObject);
begin
  EditNest(DMG.qNests, 0, True);
end;

procedure TedtNestRevision.pmnNewPersonClick(Sender: TObject);
begin
  EditPerson(DMG.qPeople, True);
end;

procedure TedtNestRevision.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtNestRevision.SetRecord;
begin
  FRevision.NestId := FNestId;
  FRevision.RevisionDate := StrToDate(eRevisionDate.Text);
  FRevision.RevisionTime := StrToTime(eRevisionTime.Text);
  FRevision.Observer1Id  := FObserver1Id;
  FRevision.Observer2Id  := FObserver2Id;
  case cbNestStage.ItemIndex of
    0: FRevision.NestStage := nsgConstruction;
    1: FRevision.NestStage := nsgLaying;
    2: FRevision.NestStage := nsgIncubation;
    3: FRevision.NestStage := nsgHatching;
    4: FRevision.NestStage := nsgNestling;
    5: FRevision.NestStage := nsgInactive;
    6: FRevision.NestStage := nsgUnknown;
  end;
  case cbNestStatus.ItemIndex of
    0: FRevision.NestStatus := nstActive;
    1: FRevision.NestStatus := nstInactive;
    2: FRevision.NestStatus := nstUnknown;
  end;
  FRevision.HostEggsTally              := eHostEggsTally.Value;
  FRevision.HostNestlingsTally         := eHostNestlingsTally.Value;
  FRevision.NidoparasiteId             := FNidoparasiteId;
  FRevision.NidoparasiteEggsTally      := eNidoparasiteEggsTally.Value;
  FRevision.NidoparasiteNestlingsTally := eNidoparasiteNestlingsTally.Value;
  FRevision.HavePhilornisLarvae        := ckHasPhilornisLarvae.Checked;
  FRevision.Notes                      := mNotes.Text;
end;

procedure TedtNestRevision.SetRevision(Value: TNestRevision);
begin
  if Assigned(Value) then
    FRevision := Value;
end;

function TedtNestRevision.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'revision_date', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'observer_1_id', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'nest_status', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbNestRevisions, 'nest_stage', Msgs);

  // Datas
  if (eRevisionDate.Text <> EmptyStr) then
  begin
    ValidDate(eRevisionDate.Text, rsDateNestRevision, Msgs);
    IsFutureDate(StrToDate(eRevisionDate.Text), Today,
      AnsiLowerCase(rsDateNestRevision), AnsiLowerCase(rsDateToday), Msgs);
  end;

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

