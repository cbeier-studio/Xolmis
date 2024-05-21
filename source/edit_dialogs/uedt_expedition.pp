{ Xolmis Expedition Editor dialog

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

unit uedt_expedition;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  Character, DateUtils, Buttons, DBEditButton, atshapelinebgra;

type

  { TedtExpedition }

  TedtExpedition = class(TForm)
    eEndDate: TDBEditButton;
    eStartDate: TDBEditButton;
    eLocality: TDBEditButton;
    eProject: TDBEditButton;
    eName: TDBEdit;
    dsLink: TDataSource;
    lblDuration: TLabel;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblDescription: TLabel;
    lblLocality: TLabel;
    lblName: TLabel;
    lblProject: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pDescription: TPanel;
    pLocality: TPanel;
    pName: TPanel;
    pProject: TPanel;
    pStartDate: TPanel;
    pEndDate: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    txtDuration: TDBText;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEndDateButtonClick(Sender: TObject);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eStartDateButtonClick(Sender: TObject);
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
  edtExpedition: TedtExpedition;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations;

{$R *.lfm}

{ TedtExpedition }

procedure TedtExpedition.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtExpedition.eEndDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eEndDate, dsLink.DataSet, 'end_date');
end;

procedure TedtExpedition.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtExpedition.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('locality_id').Clear;
    dsLink.DataSet.FieldByName('locality_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtExpedition.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtExpedition.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name');
end;

procedure TedtExpedition.eProjectDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbProjects, eProject, dsLink.DataSet, 'project_id', 'project_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('project_id').Clear;
    dsLink.DataSet.FieldByName('project_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtExpedition.eStartDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eStartDate, dsLink.DataSet, 'start_date');
end;

procedure TedtExpedition.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtExpedition.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtExpedition.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtExpedition.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionExpedition)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionExpedition)]);
end;

function TedtExpedition.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('expedition_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('start_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('end_date').IsNull = False) then
    Result := True;
end;

procedure TedtExpedition.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtExpedition.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbExpeditions, 'expedition_name', Msgs);
  RequiredIsEmpty(D, tbExpeditions, 'locality_id', Msgs);
  RequiredIsEmpty(D, tbExpeditions, 'start_date', Msgs);
  RequiredIsEmpty(D, tbExpeditions, 'end_date', Msgs);

  // Duplicated record
  RecordDuplicated(tbExpeditions, 'expedition_id', 'full_name',
    D.FieldByName('full_name').AsString, D.FieldByName('expedition_id').AsInteger);

  // Dates
  if D.FieldByName('start_date').AsString <> '' then
    ValidDate(D.FieldByName('start_date').AsString, rsDateStart, Msgs);
  if D.FieldByName('end_date').AsString <> '' then
    ValidDate(D.FieldByName('end_date').AsString, rsDateEnd, Msgs);

  if (D.FieldByName('start_date').AsString <> '') then
    IsFutureDate(D.FieldByName('start_date').AsDateTime, Today,
      AnsiLowerCase(rsDateStart), AnsiLowerCase(rsDateToday), Msgs);
  if (D.FieldByName('end_date').AsString <> '') then
    IsFutureDate(D.FieldByName('end_date').AsDateTime, Today,
      AnsiLowerCase(rsDateEnd), AnsiLowerCase(rsDateToday), Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

