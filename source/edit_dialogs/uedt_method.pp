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
  Classes, SysUtils, DB, SQLDB, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  atshapelinebgra, models_sampling;

type

  { TedtMethod }

  TedtMethod = class(TForm)
    btnHelp: TSpeedButton;
    cbCategory: TComboBox;
    dsLink: TDataSource;
    eName: TEdit;
    eAbbreviation: TEdit;
    eEbirdName: TEdit;
    lblAcronym: TLabel;
    lblCategory: TLabel;
    lblDescription: TLabel;
    lblRecommendedUses: TLabel;
    lblNotes: TLabel;
    lblEbirdName: TLabel;
    lblName: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    mRecommendedUses: TMemo;
    mNotes: TMemo;
    pBottom: TPanel;
    pContent: TPanel;
    pDescription: TPanel;
    pRecommendedUses: TPanel;
    pNotes: TPanel;
    pEbirdName: TPanel;
    pName: TPanel;
    pAcronym: TPanel;
    sbCancel: TButton;
    scrollContent: TScrollBox;
    sbSave: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eNameEditingDone(Sender: TObject);
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
    procedure ApplyDarkMode;
    procedure GetCategories;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Method: TMethod read FMethod write SetMethod;
  end;

var
  edtMethod: TedtMethod;

implementation

uses utils_locale, utils_global, utils_dialogs, data_consts, udm_main, uDarkStyleParams;

{ TedtMethod }

procedure TedtMethod.ApplyDarkMode;
begin
  btnHelp.Images := DMM.iEditsDark;
end;

procedure TedtMethod.btnHelpClick(Sender: TObject);
begin
  OpenHelp(HELP_METHODS);
end;

procedure TedtMethod.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtMethod.eNameEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtMethod.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
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
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionMethod)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionMethod)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtMethod.GetCategories;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    Clear;
    Add('SELECT category');
    Add('FROM methods');
    Add('WHERE (active_status = 1)');
    Add('GROUP BY category');
    //GravaLogSQL(SQL);
    Open;
    First;
    try
      cbCategory.Items.BeginUpdate;
      cbCategory.Items.Clear;
      repeat
        cbCategory.Items.Add(Fields[0].AsString);
        Next;
      until Eof;
      cbCategory.Sorted := True;
    finally
      cbCategory.Items.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TedtMethod.GetRecord;
begin
  eName.Text            := FMethod.Name;
  eAbbreviation.Text    := FMethod.Abbreviation;
  cbCategory.Text       := FMethod.Category;
  eEbirdName.Text       := FMethod.EbirdName;
  mDescription.Text     := FMethod.Description;
  mRecommendedUses.Text := FMethod.RecommendedUses;
  mNotes.Text           := FMethod.Notes;
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
  FMethod.Category := cbCategory.Text;
  FMethod.EbirdName := eEbirdName.Text;
  FMethod.Description := mDescription.Text;
  FMethod.RecommendedUses := mRecommendedUses.Text;
  FMethod.Notes := mNotes.Text;
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

