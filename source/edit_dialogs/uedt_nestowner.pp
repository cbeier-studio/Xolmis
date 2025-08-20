{ Xolmis Nest Owner Editor dialog

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

unit uedt_nestowner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, atshapelinebgra, models_breeding;

type

  { TedtNestOwner }

  TedtNestOwner = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    cbRole: TComboBox;
    dsLink: TDataSource;
    eIndividual: TEditButton;
    lblRole: TLabel;
    lblIndividual: TLabel;
    lineBottom: TShapeLineBGRA;
    pmnNewIndividual: TMenuItem;
    pBottom: TPanel;
    pContent: TPanel;
    pmNew: TPopupMenu;
    pRole: TPanel;
    pIndividual: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure cbRoleChange(Sender: TObject);
    procedure cbRoleKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewIndividualClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FNestOwner: TNestOwner;
    FNestId, FIndividualId: Integer;
    procedure SetNestOwner(Value: TNestOwner);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property NestOwner: TNestOwner read FNestOwner write SetNestOwner;
    property NestId: Integer read FNestId write FNestId;
  end;

var
  edtNestOwner: TedtNestOwner;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_editdialogs,
  data_types, data_getvalue, data_consts, data_columns, models_record_types, models_taxonomy,
  udm_breeding, udm_main, udm_grid, uDarkStyleParams;

{$R *.lfm}

{ TedtNestOwner }

procedure TedtNestOwner.ApplyDarkMode;
begin
  eIndividual.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtNestOwner.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_NESTS);
end;

procedure TedtNestOwner.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtNestOwner.cbRoleChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNestOwner.cbRoleKeyPress(Sender: TObject; var Key: char);
begin
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    cbRole.ItemIndex := -1;
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

procedure TedtNestOwner.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNestOwner.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, FIndividualId);
end;

procedure TedtNestOwner.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNestOwner.FormCreate(Sender: TObject);
begin
  cbRole.Items.CommaText := rsNestOwnersRoleList;
end;

procedure TedtNestOwner.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    if not sbSave.Enabled then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtNestOwner.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNestOwner.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FIndividualId := 0;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionNestOwner)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionNestOwner)]);
    GetRecord;
  end;

  cbRole.SetFocus;
end;

procedure TedtNestOwner.GetRecord;
begin
  FNestOwner.NestId := FNestId;
  case FNestOwner.Role of
    nrlMale:      cbRole.ItemIndex := 0;
    nrlFemale:    cbRole.ItemIndex := 1;
    nrlHelper:    cbRole.ItemIndex := 2;
    nrlOffspring: cbRole.ItemIndex := 3;
    nrlUnknown:   cbRole.ItemIndex := 4;
  end;
  FIndividualId := FNestOwner.IndividualId;
  eIndividual.Text := GetName('individuals', COL_FULL_NAME, COL_INDIVIDUAL_ID, FNestOwner.IndividualId);
end;

procedure TedtNestOwner.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtNestOwner.SetNestOwner(Value: TNestOwner);
begin
  if Assigned(Value) then
    FNestOwner := Value;
end;

procedure TedtNestOwner.SetRecord;
begin
  FNestOwner.NestId := FNestId;
  case cbRole.ItemIndex of
    0: FNestOwner.Role := nrlMale;
    1: FNestOwner.Role := nrlFemale;
    2: FNestOwner.Role := nrlHelper;
    3: FNestOwner.Role := nrlOffspring;
    4: FNestOwner.Role := nrlUnknown;
  end;
  FNestOwner.IndividualId := FIndividualId;
end;

function TedtNestOwner.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('role').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('individual_id').AsInteger <> 0) then
  if (cbRole.ItemIndex >= 0) and
    (FIndividualId > 0) then
    Result := True;
end;

procedure TedtNestOwner.pmnNewIndividualClick(Sender: TObject);
begin
  EditIndividual(DMG.qIndividuals, True);
end;

function TedtNestOwner.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (cbRole.ItemIndex < 0) then
    Msgs.Add(Format(rsRequiredField, [rscRole]));
  if (FIndividualId = 0) then
    Msgs.Add(Format(rsRequiredField, [rscIndividual]));

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

