{ Xolmis Permanent Mistnet Editor dialog

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

unit uedt_permanentnet;

{$mode objfpc}{$H+}

interface

uses
  Classes, EditBtn, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, atshapelinebgra, cbs_sampling;

type

  { TedtPermanentNet }

  TedtPermanentNet = class(TForm)
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    eNetNumber: TEdit;
    dsLink: TDataSource;
    lblNetNumber: TLabel;
    lblLatitude: TLabel;
    lblNetNumber1: TLabel;
    lblNotes: TLabel;
    lblLongitude: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNetNumber: TPanel;
    pLongitudeLatitude: TPanel;
    pNotes: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eNetNumberEditingDone(Sender: TObject);
    procedure eNetNumberKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FNet: TPermanentNet;
    FSamplingPlotId: Integer;
    procedure SetPermanentNet(Value: TPermanentNet);
    procedure GetRecord;
    procedure SetRecord;
    procedure GetFullName;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property PermanentNet: TPermanentNet read FNet write SetPermanentNet;
    property SamplingPlotId: Integer read FSamplingPlotId write FSamplingPlotId;
  end;

var
  edtPermanentNet: TedtPermanentNet;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_gis, cbs_validations, cbs_fullnames,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtPermanentNet }

procedure TedtPermanentNet.ApplyDarkMode;
begin
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
end;

procedure TedtPermanentNet.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPermanentNet.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtPermanentNet.eLongitudeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtPermanentNet.eNetNumberEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtPermanentNet.eNetNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

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

procedure TedtPermanentNet.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtPermanentNet.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtPermanentNet.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionPermanentNet)]);
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionPermanentNet)]);
    GetRecord;
  end;
end;

procedure TedtPermanentNet.GetFullName;
begin
  FNet.FullName := GetPermanentNetFullName(FNet.SamplingPlotId, FNet.NetNumber);
end;

procedure TedtPermanentNet.GetRecord;
begin
  if FNet.NetNumber > 0 then
    eNetNumber.Text := IntToStr(FNet.NetNumber);
  if (FNet.Longitude <> 0) and (FNet.Latitude <> 0) then
  begin
    eLongitude.Text := FloatToStr(FNet.Longitude);
    eLatitude.Text := FloatToStr(FNet.Latitude);
  end;
  mNotes.Text := FNet.Notes;
end;

function TedtPermanentNet.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('net_number').AsInteger <> 0) and
  //  ((dsLink.DataSet.FieldByName('longitude').AsFloat <> 0.0) and
  //    (dsLink.DataSet.FieldByName('latitude').AsFloat <> 0.0)) then
  if (eNetNumber.Text <> EmptyStr) and
    (eLongitude.Text <> EmptyStr) and
    (eLatitude.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtPermanentNet.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtPermanentNet.SetPermanentNet(Value: TPermanentNet);
begin
  if Assigned(Value) then
    FNet := Value;
end;

procedure TedtPermanentNet.SetRecord;
begin
  FNet.SamplingPlotId := FSamplingPlotId;
  FNet.NetNumber := StrToInt(eNetNumber.Text);
  if (eLongitude.Text <> EmptyStr) and (eLatitude.Text <> EmptyStr) then
  begin
    FNet.Longitude := StrToFloat(eLongitude.Text);
    FNet.Latitude := StrToFloat(eLatitude.Text);
  end else
  begin
    FNet.Longitude := 0;
    FNet.Latitude := 0;
  end;
  FNet.Notes := mNotes.Text;

  GetFullName;
end;

function TedtPermanentNet.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbPermanentNets, 'net_number', Msgs);

  // Geographical coordinates
  if (eLongitude.Text <> EmptyStr) then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if (eLatitude.Text <> EmptyStr) then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);
  //CoordenadaIsOk(DSP.DataSet, 'longitude', maLongitude, Msgs);
  //CoordenadaIsOk(DSP.DataSet, 'latitude', maLatitude, Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

