{ Xolmis Method Editor dialog

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

unit uedt_method;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  atshapelinebgra, cbs_sampling;

type

  { TedtMethod }

  TedtMethod = class(TForm)
    dsLink: TDataSource;
    eName: TEdit;
    eAbbreviation: TEdit;
    eEbirdName: TEdit;
    lblAcronym: TLabel;
    lblDescription: TLabel;
    lblEbirdName: TLabel;
    lblName: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pDescription: TPanel;
    pEbirdName: TPanel;
    pName: TPanel;
    pAcronym: TPanel;
    sbCancel: TButton;
    scrollContent: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FMethod: TMethod;
    procedure SetMethod(Value: TMethod);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Method: TMethod read FMethod write SetMethod;
  end;

var
  edtMethod: TedtMethod;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{ TedtMethod }

procedure TedtMethod.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtMethod.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtMethod.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtMethod.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtMethod.FormShow(Sender: TObject);
begin
  if IsNewRecord then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionMethod)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionMethod)]);
    GetRecord;
  end;
end;

procedure TedtMethod.GetRecord;
begin
  eName.Text         := FMethod.Name;
  eAbbreviation.Text := FMethod.Abbreviation;
  eEbirdName.Text    := FMethod.EbirdName;
  mDescription.Text  := FMethod.Description;
end;

function TedtMethod.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('method_name').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('method_acronym').AsString <> EmptyStr) then
  if (eName.Text <> EmptyStr) and
    (eAbbreviation.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtMethod.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtMethod.SetMethod(Value: TMethod);
begin
  if Assigned(Value) then
    FMethod := Value;
end;

procedure TedtMethod.SetRecord;
begin
  FMethod.Name := eName.Text;
  FMethod.Abbreviation := eAbbreviation.Text;
  FMethod.EbirdName := eEbirdName.Text;
  FMethod.Description := mDescription.Text;
end;

function TedtMethod.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_name', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_rank', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_method.lrs}

end.

