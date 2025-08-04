{ Xolmis Sampling Plot Editor dialog

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

unit uedt_samplingplot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus, Character, atshapelinebgra, models_sampling;

type

  { TedtSamplingPlot }

  TedtSamplingPlot = class(TForm)
    btnHelp: TSpeedButton;
    btnNew: TBitBtn;
    eName: TEdit;
    eAbbreviation: TEdit;
    eLocality: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    dsLink: TDataSource;
    lblAcronym: TLabel;
    lblAcronym1: TLabel;
    lblLongitude: TLabel;
    lblLatitude: TLabel;
    lblNotes: TLabel;
    lblDescription: TLabel;
    lblName: TLabel;
    lblLocality: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    pmnNewLocality: TMenuItem;
    mNotes: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pDescription: TPanel;
    pName: TPanel;
    pAcronym: TPanel;
    pLongitudeLatitude: TPanel;
    pLocality: TPanel;
    pmNew: TPopupMenu;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eNameEditingDone(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure pmnNewLocalityClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FSamplingPlot: TSamplingPlot;
    FLocalityId: Integer;
    procedure SetSamplingPlot(Value: TSamplingPlot);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property SamplingPlot: TSamplingPlot read FSamplingPlot write SetSamplingPlot;
  end;

var
  edtSamplingPlot: TedtSamplingPlot;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, utils_validations, utils_editdialogs, utils_gis,
  data_types, data_getvalue, data_consts, models_record_types,
  udm_main, udm_grid,
  uDarkStyleParams;

{$R *.lfm}

{ TedtSamplingPlot }

procedure TedtSamplingPlot.ApplyDarkMode;
begin
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  btnHelp.Images := DMM.iEditsDark;
  btnNew.Images := DMM.iEditsDark;
end;

procedure TedtSamplingPlot.btnNewClick(Sender: TObject);
begin
  with TBitBtn(Sender).ClientToScreen(point(0, TBitBtn(Sender).Height + 1)) do
    pmNew.Popup(X, Y);
end;

procedure TedtSamplingPlot.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSamplingPlot.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
  FSamplingPlot.LocalityId := FLocalityId;
end;

procedure TedtSamplingPlot.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    FSamplingPlot.LocalityId := FLocalityId;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    FSamplingPlot.LocalityId := 0;
    eLocality.Text := EmptyStr;
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

procedure TedtSamplingPlot.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtSamplingPlot.eLongitudeKeyPress(Sender: TObject; var Key: char);
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
  if (Key = #13) and (xSettings.UseEnterAsTab) then
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

procedure TedtSamplingPlot.eNameEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSamplingPlot.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

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

procedure TedtSamplingPlot.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSamplingPlot.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSamplingPlot.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSamplingPlot)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSamplingPlot)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtSamplingPlot.GetRecord;
begin
  eName.Text := FSamplingPlot.FullName;
  eAbbreviation.Text := FSamplingPlot.Acronym;
  FLocalityId := FSamplingPlot.LocalityId;
  eLocality.Text := GetName('gazetteer', COL_SITE_NAME, COL_SITE_ID, FLocalityId);
  if (FSamplingPlot.Longitude <> 0.0) or (FSamplingPlot.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FSamplingPlot.Longitude);
    eLatitude.Text := FloatToStr(FSamplingPlot.Latitude);
  end;
  mDescription.Text := FSamplingPlot.Description;
  mNotes.Text := FSamplingPlot.Notes;
end;

function TedtSamplingPlot.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('full_name').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('acronym').AsString <> EmptyStr) then
  if (eName.Text <> EmptyStr) and
    (eAbbreviation.Text <> EmptyStr) and
    (FLocalityId > 0) then
    Result := True;
end;

procedure TedtSamplingPlot.pmnNewLocalityClick(Sender: TObject);
begin
  EditSite(DMG.qGazetteer, True);
end;

procedure TedtSamplingPlot.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtSamplingPlot.SetRecord;
begin
  FSamplingPlot.FullName := eName.Text;
  FSamplingPlot.Acronym := eAbbreviation.Text;
  FSamplingPlot.LocalityId := FLocalityId;
  if (Length(eLongitude.Text) > 0) then
    FSamplingPlot.Longitude := StrToFloat(eLongitude.Text)
  else
    FSamplingPlot.Longitude := 0;
  if (Length(eLatitude.Text) > 0) then
    FSamplingPlot.Latitude := StrToFloat(eLatitude.Text)
  else
    FSamplingPlot.Latitude := 0;
  FSamplingPlot.Description := mDescription.Text;
  FSamplingPlot.Notes := mNotes.Text;
end;

procedure TedtSamplingPlot.SetSamplingPlot(Value: TSamplingPlot);
begin
  if Assigned(Value) then
    FSamplingPlot := Value;
end;

function TedtSamplingPlot.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbSamplingPlots, 'full_name', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSamplingPlots, 'acronym', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbSamplingPlots, 'locality_id', Msgs);

  // Duplicated record
  RecordDuplicated(tbSamplingPlots, COL_SAMPLING_PLOT_ID, COL_ABBREVIATION,
    eAbbreviation.Text, FSamplingPlot.Id, Msgs);

  // Foreign keys
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
  //  rsCaptionLocality, Msgs);

  // Geographical coordinates
  if eLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);
  //CoordenadaIsOk(cdsConsulta, 'longitude', maLongitude, Msgs);
  //CoordenadaIsOk(cdsConsulta, 'latitude', maLatitude, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

