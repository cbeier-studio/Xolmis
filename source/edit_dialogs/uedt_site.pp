{ Xolmis Toponym Editor dialog

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

unit uedt_site;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, RTTICtrls, SysUtils, DB, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBCtrls, StdCtrls, Character, DBEditButton, atshapelinebgra, cbs_gis;

type

  { TedtSite }

  TedtSite = class(TForm)
    cbRank: TComboBox;
    eAbbreviation: TEdit;
    eAltitude: TEdit;
    eFullname: TEdit;
    eEbirdName: TEdit;
    eParentSite: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eName: TEdit;
    dsLink: TDataSource;
    lblAltitude1: TLabel;
    lblLatitude: TLabel;
    lblLongitude: TLabel;
    lblAcronym: TLabel;
    lblAltitude: TLabel;
    lblEbirdName: TLabel;
    lblFullname: TLabel;
    lblType: TLabel;
    lblName: TLabel;
    lblParentToponym: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pClient: TPanel;
    pEbirdName: TPanel;
    pFullname: TPanel;
    pName: TPanel;
    pLongitudeLatitude: TPanel;
    pAcronymType: TPanel;
    pAltitude: TPanel;
    pParentToponym: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eAltitudeKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eParentSiteButtonClick(Sender: TObject);
    procedure eParentSiteDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eParentSiteEditingDone(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FSite: TSite;
    FParentSiteId: Integer;
    procedure SetSite(Value: TSite);
    procedure GetRecord;
    procedure SetRecord;
    procedure GetFullName;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Site: TSite read FSite write SetSite;
  end;

var
  edtSite: TedtSite;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations, cbs_getvalue,
  udm_main,
  uDarkStyleParams;

{$R *.lfm}

{ TedtSite }

procedure TedtSite.ApplyDarkMode;
begin
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eParentSite.Images := DMM.iEditsDark;
end;

procedure TedtSite.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSite.eAltitudeKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', ',', '.', '+', '-', #8, #13, #27];
var
  EditText: String;
  PosDecimal: Integer;
  DecimalValue: Extended;
begin
  FormKeyPress(Sender, Key);

  sbSave.Enabled := IsRequiredFilled;

  EditText := EmptyStr;
  PosDecimal := 0;
  DecimalValue := 0;

  if not (Key in AllowedChars) then
  begin
    Key := #0;
    Exit;
  end;

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
    Exit;
  end;

  if (Sender is TEdit) then
    EditText := TEdit(Sender).Text
  else
  if (Sender is TEditButton) then
    EditText := TEditButton(Sender).Text;
  PosDecimal := Pos(FormatSettings.DecimalSeparator, EditText);

  // Decimal separator
  if (Key in [',', '.']) then
  begin
    if (PosDecimal = 0) then
      Key := FormatSettings.DecimalSeparator
    else
      Key := #0;
    Exit;
  end;

  // Numeric signal
  if (Key in ['+', '-']) then
  begin
    if (Length(EditText) > 0) then
    begin
      if TryStrToFloat(EditText, DecimalValue) then
      begin
        if ((DecimalValue > 0) and (Key = '-')) or ((DecimalValue < 0) and (Key = '+')) then
          DecimalValue := DecimalValue * -1.0;
        EditText := FloatToStr(DecimalValue);

        if (Sender is TEdit) then
        begin
          TEdit(Sender).Text := EditText;
          TEdit(Sender).SelStart := Length(EditText);
        end
        else
        if (Sender is TEditButton) then
        begin
          TEditButton(Sender).Text := EditText;
          TEditButton(Sender).SelStart := Length(EditText);
        end;
      end;
      Key := #0;
    end
    else
    begin
      if (Key = '+') then
        Key := #0;
    end;

    Exit;
  end;
end;

procedure TedtSite.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtSite.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSite.eParentSiteButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eParentSite, FParentSiteId);
  FSite.ParentSiteId := FParentSiteId;
  GetFullName;
end;

procedure TedtSite.eParentSiteDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eParentSite, FParentSiteId, Key);
    FSite.ParentSiteId := FParentSiteId;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FSite.ParentSiteId := 0;
    eParentSite.Text := EmptyStr;
    GetFullName;
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

  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSite.eParentSiteEditingDone(Sender: TObject);
begin
  GetFullName;
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSite.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSite.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSite.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  FParentSiteId := 0;

  cbRank.Items.Clear;
  cbRank.Items.Add(rsCaptionCountry);
  cbRank.Items.Add(rsCaptionState);
  cbRank.Items.Add(rsCaptionRegion);
  cbRank.Items.Add(rsCaptionMunicipality);
  cbRank.Items.Add(rsCaptionDistrict);
  cbRank.Items.Add(rsCaptionLocality);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionToponym)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionToponym)]);
    GetRecord;
  end;
end;

procedure TedtSite.GetFullName;
var
  S: String;
begin
  if (Length(eParentSite.Text) > 0) then
  begin
    S := eName.Text + ', ';
    S := S + GetName('gazetteer', 'full_name', 'site_id', FSite.ParentSiteId);
  end
  else
    S := eName.Text;

  //FSite.FullName := S;
  eFullname.Text := S;
end;

procedure TedtSite.GetRecord;
begin
  eName.Text := FSite.Name;
  eAbbreviation.Text := FSite.Abbreviation;
  case FSite.Rank of
    srCountry:      cbRank.ItemIndex := 0;
    srState:        cbRank.ItemIndex := 1;
    srRegion:       cbRank.ItemIndex := 2;
    srMunicipality: cbRank.ItemIndex := 3;
    srDistrict:     cbRank.ItemIndex := 4;
    srLocality:     cbRank.ItemIndex := 5;
  end;
  eLongitude.Text := FloatToStr(FSite.Longitude);
  eLatitude.Text := FloatToStr(FSite.Latitude);
  eAltitude.Text := FloatToStr(FSite.Altitude);
  FParentSiteId := FSite.ParentSiteId;
  eParentSite.Text := GetName('gazetteer', 'site_name', 'site_id', FSite.ParentSiteId);
  eFullname.Text := FSite.FullName;
  eEbirdName.Text := FSite.EbirdName;
end;

function TedtSite.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('site_name').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('full_name').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('site_rank').AsString <> EmptyStr) then
  if (eName.Text <> EmptyStr) and
    (eFullname.Text <> EmptyStr) and
    (cbRank.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtSite.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtSite.SetRecord;
begin
  FSite.Name := eName.Text;
  FSite.Abbreviation := eAbbreviation.Text;
  case cbRank.ItemIndex of
    0: FSite.Rank := srCountry;
    1: FSite.Rank := srState;
    2: FSite.Rank := srRegion;
    3: FSite.Rank := srMunicipality;
    4: FSite.Rank := srDistrict;
    5: FSite.Rank := srLocality;
  end;
  if (eLongitude.Text <> EmptyStr) then
    FSite.Longitude := StrToFloat(eLongitude.Text)
  else
    FSite.Longitude := 0.0;
  if (eLatitude.Text <> EmptyStr) then
    FSite.Latitude := StrToFloat(eLatitude.Text)
  else
    FSite.Latitude := 0.0;
  if (eAltitude.Text <> EmptyStr) then
    FSite.Altitude := StrToFloat(eAltitude.Text)
  else
    FSite.Altitude := 0.0;
  //FSite.ParentSiteId := GetKey('gazetteer', 'site_id', 'site_name', eParentSite.Text);
  if (Length(eFullname.Text) = 0) then
    GetFullName;
  FSite.FullName := eFullname.Text;
  FSite.EbirdName := eEbirdName.Text;
end;

procedure TedtSite.SetSite(Value: TSite);
begin
  if Assigned(Value) then
    FSite := Value;
end;

function TedtSite.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Unique values

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_name', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_rank', Msgs);

  // Foreign keys
  //ForeignValueExists(tbGazetteer, 'parent_site_id',
  //  dsLink.DataSet.FieldByName('parent_site_id').AsInteger, rsCaptionParentSite, Msgs);

  // Geographical coordinates
  if eLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);
  //CoordenadaIsOk(dsLink.DataSet, 'longitude', maLongitude, Msgs);
  //CoordenadaIsOk(dsLink.DataSet, 'latitude', maLatitude, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

