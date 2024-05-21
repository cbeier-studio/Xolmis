{ Xolmis Institution Editor dialog

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

unit uedt_institution;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  atshapelinebgra, DBEditButton;

type

  { TedtInstitution }

  TedtInstitution = class(TForm)
    eCountry: TDBEditButton;
    eFullname: TDBEdit;
    dsLink: TDataSource;
    eAcronym: TDBEdit;
    ePostalCode: TDBEdit;
    eMunicipality: TDBEditButton;
    eState: TDBEditButton;
    eComplement: TDBEdit;
    eAddress: TDBEdit;
    eNeighborhood: TDBEdit;
    eManagerName: TDBEdit;
    eEmail: TDBEdit;
    ePhone: TDBEdit;
    lblPhone: TLabel;
    lblAcronym: TLabel;
    lblPostalCode: TLabel;
    lblNotes: TLabel;
    lblComplement: TLabel;
    lblAddress: TLabel;
    lblNeighborhood: TLabel;
    lblManagerName: TLabel;
    lblEmail: TLabel;
    lblFullname: TLabel;
    lblMunicipality: TLabel;
    lblState: TLabel;
    lblCountry: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pComplement: TPanel;
    pAddress: TPanel;
    pNeighborhood: TPanel;
    pManagerName: TPanel;
    pEmail: TPanel;
    pFullname: TPanel;
    pPhone: TPanel;
    pAcronym: TPanel;
    pPostalCode: TPanel;
    pMunicipality: TPanel;
    pState: TPanel;
    pCountry: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eCountryButtonClick(Sender: TObject);
    procedure eCountryDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eFullnameKeyPress(Sender: TObject; var Key: char);
    procedure eMunicipalityButtonClick(Sender: TObject);
    procedure eMunicipalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eStateButtonClick(Sender: TObject);
    procedure eStateDBEditKeyPress(Sender: TObject; var Key: char);
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
  edtInstitution: TedtInstitution;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations;

{$R *.lfm}

{ TedtInstitution }

procedure TedtInstitution.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtInstitution.eCountryButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCountries], eCountry, dsLink.Dataset, 'country_id', 'country_name');
end;

procedure TedtInstitution.eCountryDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCountries], eCountry, dsLink.Dataset, 'country_id', 'country_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('country_id').Clear;
    dsLink.DataSet.FieldByName('country_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.eFullnameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.eMunicipalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfCities], eMunicipality, dsLink.Dataset, 'municipality_id', 'municipality_name');
end;

procedure TedtInstitution.eMunicipalityDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfCities], eMunicipality, dsLink.Dataset, 'municipality_id', 'municipality_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('municipality_id').Clear;
    dsLink.DataSet.FieldByName('municipality_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.eStateButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfStates], eState, dsLink.Dataset, 'state_id', 'state_name');
end;

procedure TedtInstitution.eStateDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindSiteDlg([gfStates], eState, dsLink.Dataset, 'state_id', 'state_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('state_id').Clear;
    dsLink.DataSet.FieldByName('state_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtInstitution.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtInstitution.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtInstitution.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtInstitution.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionInstitution)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionInstitution)]);
end;

function TedtInstitution.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('full_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('acronym').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtInstitution.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtInstitution.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Campos obrigatórios
  RequiredIsEmpty(D, tbInstitutions, 'full_name', Msgs);
  RequiredIsEmpty(D, tbInstitutions, 'acronym', Msgs);

  // Registro duplicado
  RecordDuplicated(tbInstitutions, 'institution_id', 'full_name', D.FieldByName('full_name').AsString,
    D.FieldByName('institution_id').AsInteger, Msgs);

  // Chaves estrangeiras
  ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('municipality_id').AsInteger,
    rsCaptionMunicipality, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('state_id').AsInteger, rsCaptionState, Msgs);
  ForeignValueExists(tbGazetteer, 'site_id', D.FieldByName('country_id').AsInteger, rsCaptionCountry, Msgs);

  // Email
  if (D.FieldByName('email_addr').AsString <> '') then
    CheckEmail(D.FieldByName('email_addr').AsString, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

