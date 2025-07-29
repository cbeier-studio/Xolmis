unit uedt_vegetation;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, EditBtn, ExtCtrls, Spin, SysUtils, Forms,
  Controls, Graphics, StdCtrls, Dialogs, cbs_sampling;

type

  { TedtVegetation }

  TedtVegetation = class(TForm)
    cbShrubsDistribution: TComboBox;
    cbTreesDistribution: TComboBox;
    cbHerbsDistribution: TComboBox;
    dsLink: TDataSource;
    eShrubsAvgHeight: TSpinEdit;
    eTreesAvgHeight: TSpinEdit;
    eShrubsProportion: TSpinEdit;
    eTreesProportion: TSpinEdit;
    eSampleTime: TEdit;
    eSampleDate: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    lblCloudCover: TLabel;
    lblCloudCover1: TLabel;
    lblCloudCover2: TLabel;
    lblLatitude: TLabel;
    lblLongitude: TLabel;
    lblNotes: TLabel;
    lblSampleDate: TLabel;
    lblSampleMoment: TLabel;
    lblSampleMoment1: TLabel;
    lblSampleMoment2: TLabel;
    lblSampleTime: TLabel;
    lblTemperature: TLabel;
    lblTemperature1: TLabel;
    lblTemperature2: TLabel;
    lblTitleHerbs: TLabel;
    lblTitleShrubs: TLabel;
    lblTitleTrees: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TMemo;
    pBottom: TPanel;
    pHerbsProportionHeight: TPanel;
    pShrubsProportionHeight: TPanel;
    pTreesProportionHeight: TPanel;
    pContent: TPanel;
    pLongitudeLatitude: TPanel;
    pNotes: TPanel;
    pSampleDateTime: TPanel;
    pHerbsDIstribution: TPanel;
    pShrubsDistribution: TPanel;
    pTreesDistribution: TPanel;
    pTitleHerbs: TPanel;
    pTitleShrubs: TPanel;
    pTitleTrees: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    eHerbsProportion: TSpinEdit;
    eHerbsAvgHeight: TSpinEdit;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eSampleDateButtonClick(Sender: TObject);
    procedure eSampleDateEditingDone(Sender: TObject);
    procedure eSampleTimeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FVegetation: TVegetation;
    FSurveyId: Integer;
    procedure SetVegetation(Value: TVegetation);
    procedure GetRecord;
    procedure SetRecord;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Vegetation: TVegetation read FVegetation write SetVegetation;
    property SurveyId: Integer read FSurveyId write FSurveyId;
  end;

var
  edtVegetation: TedtVegetation;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_gis, cbs_validations, cbs_datacolumns, cbs_dataconst,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtVegetation }

procedure TedtVegetation.ApplyDarkMode;
begin
  eSampleDate.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
end;

procedure TedtVegetation.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtVegetation.eLongitudeButtonClick(Sender: TObject);
begin
  GeoAssistDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtVegetation.eLongitudeKeyPress(Sender: TObject; var Key: char);
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

procedure TedtVegetation.eSampleDateButtonClick(Sender: TObject);
var
  Dt: TDateTime;
begin
  CalendarDlg(eSampleDate.Text, eSampleDate, Dt);
end;

procedure TedtVegetation.eSampleDateEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtVegetation.eSampleTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
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

procedure TedtVegetation.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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

procedure TedtVegetation.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtVegetation.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbHerbsDistribution.Items.Add(rsDistributionNone);
  cbHerbsDistribution.Items.Add(rsDistributionRare);
  cbHerbsDistribution.Items.Add(rsDistributionFewSparse);
  cbHerbsDistribution.Items.Add(rsDistributionOnePatch);
  cbHerbsDistribution.Items.Add(rsDistributionOnePatchFewSparse);
  cbHerbsDistribution.Items.Add(rsDistributionManySparse);
  cbHerbsDistribution.Items.Add(rsDistributionOnePatchManySparse);
  cbHerbsDistribution.Items.Add(rsDistributionFewPatches);
  cbHerbsDistribution.Items.Add(rsDistributionFewPatchesSparse);
  cbHerbsDistribution.Items.Add(rsDistributionManyPatches);
  cbHerbsDistribution.Items.Add(rsDistributionManyPatchesSparse);
  cbHerbsDistribution.Items.Add(rsDistributionEvenHighDensity);
  cbHerbsDistribution.Items.Add(rsDistributionContinuousFewGaps);
  cbHerbsDistribution.Items.Add(rsDistributionContinuousDense);
  cbHerbsDistribution.Items.Add(rsDistributionContinuousDenseEdge);

  cbShrubsDistribution.Items.Assign(cbHerbsDistribution.Items);
  cbTreesDistribution.Items.Assign(cbHerbsDistribution.Items);

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionVegetation)]);
    if not DateIsNull(FVegetation.SampleDate) then
      eSampleDate.Text := DateToStr(FVegetation.SampleDate);
    cbHerbsDistribution.ItemIndex := 0;
    cbShrubsDistribution.ItemIndex := 0;
    cbTreesDistribution.ItemIndex := 0;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionVegetation)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;
end;

procedure TedtVegetation.GetRecord;
begin
  if not DateIsNull(FVegetation.SampleDate) then
    eSampleDate.Text := DateToStr(FVegetation.SampleDate);
  if not TimeIsNull(FVegetation.SampleTime) then
    eSampleTime.Text := FormatDateTime('hh:nn', FVegetation.SampleTime);
  if (FVegetation.Longitude <> 0.0) and (FVegetation.Latitude <> 0.0) then
  begin
    eLongitude.Text := FloatToStr(FVegetation.Longitude);
    eLatitude.Text := FloatToStr(FVegetation.Latitude);
  end;
  cbHerbsDistribution.ItemIndex := Ord(FVegetation.HerbsDistribution);
  eHerbsProportion.Value := FVegetation.HerbsProportion;
  eHerbsAvgHeight.Value := FVegetation.HerbsAvgHeight;
  cbShrubsDistribution.ItemIndex := Ord(FVegetation.ShrubsDistribution);
  eShrubsProportion.Value := FVegetation.ShrubsProportion;
  eShrubsAvgHeight.Value := FVegetation.ShrubsAvgHeight;
  cbTreesDistribution.ItemIndex := Ord(FVegetation.TreesDistribution);
  eTreesProportion.Value := FVegetation.TreesProportion;
  eTreesAvgHeight.Value := FVegetation.TreesAvgHeight;
  mNotes.Text := FVegetation.Notes;
end;

function TedtVegetation.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eSampleDate.Text <> EmptyStr) and
    (cbHerbsDistribution.ItemIndex >= 0) and
    (cbShrubsDistribution.ItemIndex >= 0) and
    (cbTreesDistribution.ItemIndex >= 0) then
    Result := True;
end;

procedure TedtVegetation.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtVegetation.SetRecord;
begin
  FVegetation.SurveyId           := FSurveyId;
  FVegetation.SampleDate         := StrToDate(eSampleDate.Text);
  if eSampleTime.Text <> EmptyStr then
    FVegetation.SampleTime         := StrToTime(eSampleTime.Text);
  if eLongitude.Text <> EmptyStr then
    FVegetation.Longitude          := StrToFloat(eLongitude.Text);
  if eLatitude.Text <> EmptyStr then
    FVegetation.Latitude           := StrToFloat(eLatitude.Text);
  FVegetation.HerbsDistribution  := TStratumDistribution(cbHerbsDistribution.ItemIndex);
  FVegetation.HerbsProportion    := eHerbsProportion.Value;
  FVegetation.HerbsAvgHeight     := eHerbsAvgHeight.Value;
  FVegetation.ShrubsDistribution := TStratumDistribution(cbShrubsDistribution.ItemIndex);
  FVegetation.ShrubsProportion   := eShrubsProportion.Value;
  FVegetation.ShrubsAvgHeight    := eShrubsAvgHeight.Value;
  FVegetation.TreesDistribution  := TStratumDistribution(cbTreesDistribution.ItemIndex);
  FVegetation.TreesProportion    := eTreesProportion.Value;
  FVegetation.TreesAvgHeight     := eTreesAvgHeight.Value;
  FVegetation.Notes              := mNotes.Text;
end;

procedure TedtVegetation.SetVegetation(Value: TVegetation);
begin
  if Assigned(Value) then
    FVegetation := Value;
end;

function TedtVegetation.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_name', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_rank', Msgs);
  if cbHerbsDistribution.ItemIndex > 0 then
  begin
    if eHerbsProportion.Value = 0 then
      Msgs.Add(Format(rsRequiredField, [rscProportionOfHerbs]));
    if eHerbsAvgHeight.Value = 0 then
      Msgs.Add(Format(rsRequiredField, [rscAvgHeightOfHerbs]));
  end;
  if cbShrubsDistribution.ItemIndex > 0 then
  begin
    if eShrubsProportion.Value = 0 then
      Msgs.Add(Format(rsRequiredField, [rscProportionOfShrubs]));
    if eShrubsAvgHeight.Value = 0 then
      Msgs.Add(Format(rsRequiredField, [rscAvgHeightOfShrubs]));
  end;
  if cbTreesDistribution.ItemIndex > 0 then
  begin
    if eTreesProportion.Value = 0 then
      Msgs.Add(Format(rsRequiredField, [rscProportionOfTrees]));
    if eTreesAvgHeight.Value = 0 then
      Msgs.Add(Format(rsRequiredField, [rscAvgHeightOfTrees]));
  end;

  // Date and time
  ValidDate(eSampleDate.Text, rscDate, Msgs);
  if eSampleTime.Text <> EmptyStr then
    ValidTime(eSampleTime.Text, rscTime, Msgs);

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

