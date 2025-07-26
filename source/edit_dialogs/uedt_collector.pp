{ Xolmis Specimen Collector Editor dialog

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit uedt_collector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, DB, ExtCtrls, SysUtils, Forms, Controls, Graphics, EditBtn, atshapelinebgra, Buttons, StdCtrls,
  Character, Dialogs, cbs_sampling;

type

  { TedtCollector }

  TedtCollector = class(TForm)
    dsLink: TDataSource;
    eCollector: TEditButton;
    lblCollector: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pContent: TPanel;
    pCollector: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eCollectorButtonClick(Sender: TObject);
    procedure eCollectorChange(Sender: TObject);
    procedure eCollectorKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FCollector: TSpecimenCollector;
    FSpecimenId, FCollectorId: Integer;
    procedure SetCollector(Value: TSpecimenCollector);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property SpecimenCollector: TSpecimenCollector read FCollector write SetCollector;
    property SpecimenId: Integer read FSpecimenId write FSpecimenId;
  end;

var
  edtCollector: TedtCollector;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_getvalue, cbs_finddialogs, cbs_dataconst, cbs_dialogs,
  udm_sampling, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtCollector }

procedure TedtCollector.ApplyDarkMode;
begin
  eCollector.Images := DMM.iEditsDark;
end;

procedure TedtCollector.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtCollector.eCollectorButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eCollector, FCollectorId);
end;

procedure TedtCollector.eCollectorChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtCollector.eCollectorKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eCollector, FCollectorId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FCollectorId := 0;
    eCollector.Clear;
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

procedure TedtCollector.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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

procedure TedtCollector.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtCollector.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FCollectorId := 0;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionCollector)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionCollector)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtCollector.GetRecord;
begin
  FCollectorId := FCollector.PersonId;
  eCollector.Text := GetName('people', COL_FULL_NAME, COL_PERSON_ID, FCollector.PersonId);
end;

function TedtCollector.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (FCollectorId > 0) then
    Result := True;
end;

procedure TedtCollector.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtCollector.SetCollector(Value: TSpecimenCollector);
begin
  if Assigned(Value) then
    FCollector := Value;
end;

procedure TedtCollector.SetRecord;
begin
  FCollector.SpecimenId := FSpecimenId;
  FCollector.PersonId := FCollectorId;
end;

function TedtCollector.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbNestOwners, 'role', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbNestOwners, 'individual_id', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

