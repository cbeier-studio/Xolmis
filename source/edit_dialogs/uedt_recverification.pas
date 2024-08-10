{ Xolmis Record Verification Editor dialog

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit uedt_recverification;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DateUtils, StdCtrls, ExtCtrls, atshapelinebgra,
  DBCtrls, EditBtn, dbeditbutton, DB, SQLDB, Character, cbs_datatypes;

type

  { TedtRecVerification }

  TedtRecVerification = class(TForm)
    cbStatus: TComboBox;
    eDate: TDateEdit;
    eResearcher: TEditButton;
    lblNotes: TLabel;
    lblResearcher: TLabel;
    lblDate: TLabel;
    lblStatus: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pResearcher: TPanel;
    pNotes: TPanel;
    pDate: TPanel;
    pStatus: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure eDateKeyPress(Sender: TObject; var Key: char);
    procedure eResearcherButtonClick(Sender: TObject);
    procedure eResearcherKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FTableType, FChildType: TTableType;
    FId, FPersonId: Integer;
    procedure AddVerification;
    procedure ApplyDarkMode;
    function ValidateData(aDate: String): Boolean;
  public
    property TableType: TTableType read FTableType write FTableType default tbNone;
    property ChildType: TTableType read FChildType write FChildType default tbNone;
    property Id: Integer read FId write FId;
  end;

var
  edtRecVerification: TedtRecVerification;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_finddialogs, cbs_validations,
  udm_main, udm_grid, udm_breeding, udm_individuals, udm_sampling,
  uDarkStyleParams;

{$R *.lfm}

{ TedtRecVerification }

procedure TedtRecVerification.AddVerification;
const
  VerificationTypes: array of String = ('OK', 'WT', 'WL', 'WC', 'WM', 'WV', 'MD');
var
  Qry: TSQLQuery;
  Tabela, Tipo: String;
begin
  if FChildType <> tbNone then
    Tabela := TableNames[FChildType]
  else
    Tabela := TableNames[FTableType];
  Tipo := VerificationTypes[cbStatus.ItemIndex];

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Add('INSERT INTO record_verifications');
      Add('  (table_name, record_id, verification_date, verification_status, person_id, notes)');
      Add('VALUES (:atable, :aid, :adate, :astatus, :aperson, :anote)');
      ParamByName('ATABLE').AsString := Tabela;
      ParamByName('AID').AsInteger := FId;
      ParamByName('ADATE').AsDateTime := eDate.Date;
      ParamByName('ASTATUS').AsString := Tipo;
      ParamByName('APERSON').AsInteger := FPersonId;
      ParamByName('ANOTE').AsString := mNotes.Lines.Text;
      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TedtRecVerification.ApplyDarkMode;
begin
  eDate.Images := DMM.iEditsDark;
  eResearcher.Images := DMM.iEditsDark;
end;

procedure TedtRecVerification.eDateKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtRecVerification.eResearcherButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eResearcher, FPersonId);
end;

procedure TedtRecVerification.eResearcherKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbPeople, eResearcher, FPersonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FPersonId := 0;
    eResearcher.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtRecVerification.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    sbSaveClick(nil);
  end;
end;

procedure TedtRecVerification.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CLOSE = Esc }
  if (Key = #27) then
  begin
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrClose;
  end;
end;

procedure TedtRecVerification.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  eDate.Date := Today;
  FPersonId := 0;
end;

procedure TedtRecVerification.sbSaveClick(Sender: TObject);
begin
  if not ValidateData(DateToStr(eDate.Date)) then
    Exit;

  AddVerification;

  ModalResult := mrOk;
end;

function TedtRecVerification.ValidateData(aDate: String): Boolean;
var
  Msgs: TStringList;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  if (cbStatus.ItemIndex < 0) or (cbStatus.Text = '') then
    Msgs.Add(rsRequiredVerificationStatus);
  if Length(aDate) < 10 then
    Msgs.Add(Format(rsRequiredField, [rsCaptionDate]));
  if FPersonId <= 0 then
    Msgs.Add(Format(rsRequiredField, [rsCaptionPerson]));

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

