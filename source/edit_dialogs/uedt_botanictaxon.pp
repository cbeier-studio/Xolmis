{ Xolmis Botanical Taxon Editor dialog

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

unit uedt_botanictaxon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, atshapelinebgra, models_botany;

type

  { TedtBotanicTaxon }

  TedtBotanicTaxon = class(TForm)
    btnHelp: TSpeedButton;
    cbRank: TComboBox;
    eAuthorship: TEdit;
    eVernacularName: TEdit;
    eName: TEditButton;
    eParentTaxon: TEditButton;
    eValidName: TEditButton;
    dsLink: TDataSource;
    lblAuthorship: TLabel;
    lblVernacularName: TLabel;
    lblValidName: TLabel;
    lblRank: TLabel;
    lblName: TLabel;
    lblParentTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pVernacularName: TPanel;
    pValidName: TPanel;
    pRank: TPanel;
    pName: TPanel;
    pAuthorship: TPanel;
    pParentTaxon: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAuthorshipKeyPress(Sender: TObject; var Key: char);
    procedure eNameButtonClick(Sender: TObject);
    procedure eNameEditingDone(Sender: TObject);
    procedure eParentTaxonButtonClick(Sender: TObject);
    procedure eParentTaxonKeyPress(Sender: TObject; var Key: char);
    procedure eValidNameButtonClick(Sender: TObject);
    procedure eValidNameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FTaxon: TBotanicalTaxon;
    FParentTaxonId, FValidId: Integer;
    procedure SetTaxon(Value: TBotanicalTaxon);
    procedure GetRecord;
    procedure SetRecord;
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Taxon: TBotanicalTaxon read FTaxon write SetTaxon;
  end;

var
  edtBotanicTaxon: TedtBotanicTaxon;

implementation

uses
  utils_locale, utils_global, utils_finddialogs, utils_dialogs, utils_validations,
  data_types, data_getvalue, data_consts, data_columns, models_record_types, models_taxonomy,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtBotanicTaxon }

procedure TedtBotanicTaxon.ApplyDarkMode;
begin
  eName.Images := DMM.iEditsDark;
  eParentTaxon.Images := DMM.iEditsDark;
  eValidName.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtBotanicTaxon.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_BOTANICAL_TAXA);
end;

procedure TedtBotanicTaxon.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtBotanicTaxon.eAuthorshipKeyPress(Sender: TObject; var Key: char);
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

procedure TedtBotanicTaxon.eNameButtonClick(Sender: TObject);
begin
  FindPlantminerDlg(eName.Text, eName, eAuthorship, eName);
end;

procedure TedtBotanicTaxon.eNameEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtBotanicTaxon.eParentTaxonButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eParentTaxon, FParentTaxonId);
end;

procedure TedtBotanicTaxon.eParentTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eParentTaxon, FParentTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FParentTaxonId := 0;
    eParentTaxon.Text := EmptyStr;
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

procedure TedtBotanicTaxon.eValidNameButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eValidName, FValidId);
end;

procedure TedtBotanicTaxon.eValidNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eValidName, FValidId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FValidId := 0;
    eValidName.Text := EmptyStr;
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

procedure TedtBotanicTaxon.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtBotanicTaxon.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtBotanicTaxon.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FillComboBox(cbRank, 'taxon_ranks', COL_RANK_NAME, COL_RANK_SEQUENCE, COL_ICBN);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionTaxon)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionTaxon)]);
    GetRecord;
  end;
end;

procedure TedtBotanicTaxon.GetRecord;
var
  aRankId: Integer;
  FRankName: String;
begin
  eName.Text := FTaxon.FullName;
  eAuthorship.Text := FTaxon.Authorship;
  eVernacularName.Text := FTaxon.VernacularName;
  aRankId := GetKey('taxon_ranks', COL_RANK_ID, COL_RANK_ABBREVIATION, BOTANICAL_RANKS[FTaxon.RankId]);
  FRankName := GetName('taxon_ranks', COL_RANK_NAME, COL_RANK_ID, aRankId);
  cbRank.ItemIndex := cbRank.Items.IndexOf(FRankName);
  FParentTaxonId := FTaxon.ParentTaxonId;
  eParentTaxon.Text := GetName('botanic_taxa', COL_TAXON_NAME, COL_TAXON_ID, FParentTaxonId);
  FValidId := FTaxon.ValidId;
  eValidName.Text := GetName('botanic_taxa', COL_TAXON_NAME, COL_TAXON_ID, FValidId);
end;

function TedtBotanicTaxon.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('taxon_name').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('rank_id').AsInteger <> 0) then
  if (eName.Text <> EmptyStr) and
    (cbRank.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtBotanicTaxon.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtBotanicTaxon.SetRecord;
var
  aRankId: Integer;
  aRankAbbrev: String;
begin
  FTaxon.FullName := eName.Text;
  FTaxon.Authorship := eAuthorship.Text;
  FTaxon.VernacularName := eVernacularName.Text;
  aRankId := GetKey('taxon_ranks', COL_RANK_ID, COL_RANK_NAME, cbRank.Text);
  aRankAbbrev := GetName('taxon_ranks', COL_RANK_ABBREVIATION, COL_RANK_ID, aRankId);
  FTaxon.RankId := StringToBotanicRank(aRankAbbrev);
  FTaxon.ParentTaxonId := FParentTaxonId;
  FTaxon.ValidId := FValidId;
end;

procedure TedtBotanicTaxon.SetTaxon(Value: TBotanicalTaxon);
begin
  if Assigned(Value) then
    FTaxon := Value;
end;

function TedtBotanicTaxon.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  if (eName.Text = EmptyStr) then
    Msgs.Add(Format(rsRequiredField, [rscName]));
  if (cbRank.ItemIndex < 0) then
    Msgs.Add(Format(rsRequiredField, [rscTaxonomicRank]));
  // Conditional required fields
  { #todo : Required Parent taxon when Rank is lower than Order }

  // Unique fields
  if (eName.Text <> EmptyStr) then
    RecordDuplicated(tbBotanicTaxa, COL_TAXON_ID, COL_TAXON_NAME, eName.Text, FTaxon.Id, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

