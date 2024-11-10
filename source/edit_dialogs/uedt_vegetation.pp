unit uedt_vegetation;

{$mode ObjFPC}{$H+}

interface

uses
  atshapelinebgra, Classes, DB, ExtCtrls, SysUtils, Forms, Controls, Graphics, StdCtrls, DBCtrls,
  Dialogs, dbeditbutton;

type

  { TedtVegetation }

  TedtVegetation = class(TForm)
    cbHerbsDistribution: TDBComboBox;
    cbShrubsDistribution: TDBComboBox;
    cbTreesDistribution: TDBComboBox;
    dsLink: TDataSource;
    eHerbsProportion: TDBEdit;
    eShrubsProportion: TDBEdit;
    eTreesProportion: TDBEdit;
    eLatitude: TDBEditButton;
    eLongitude: TDBEditButton;
    eSampleDate: TDBEditButton;
    eSampleTime: TDBEdit;
    eHerbsAvgHeight: TDBEdit;
    eShrubsAvgHeight: TDBEdit;
    eTreesAvgHeight: TDBEdit;
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
    mNotes: TDBMemo;
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
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eSampleDateButtonClick(Sender: TObject);
    procedure eSampleTimeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
    procedure ApplyDarkMode;
  public

  end;

var
  edtVegetation: TedtVegetation;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs, cbs_gis, udm_main, uDarkStyleParams;

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
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtVegetation.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtVegetation.eLongitudeKeyPress(Sender: TObject; var Key: char);
var
  s, d: String;
  f: Extended;
begin
  FormKeyPress(Sender, Key);
  if (CharInSet(Key, ['0' .. '9', '.', ',', '-', '+', #8, #13, #27])) then
  begin
    if eLongitude.Focused then
      s := dsLink.DataSet.FieldByName('longitude').AsString
    else
    if eLatitude.Focused then
      s := dsLink.DataSet.FieldByName('latitude').AsString
    else
    if (Length(s) > 0) then
    begin
      TryStrToFloat(s, f);
      if (Key = '-') then
      begin
        if (f > 0.0) then
        begin
          f := f * -1.0;
          with dsLink.DataSet do
          begin
            if eLongitude.Focused then
              FieldByName('longitude').AsFloat := f
            else
            if eLatitude.Focused then
              FieldByName('latitude').AsFloat := f;
          end;
        end;
        Key := #0;
      end;
      if (Key = '+') then
      begin
        if (f < 0.0) then
        begin
          f := f * -1.0;
          with dsLink.DataSet do
          begin
            if eLongitude.Focused then
              FieldByName('longitude').AsFloat := f
            else
            if eLatitude.Focused then
              FieldByName('latitude').AsFloat := f;
          end;
        end;
        Key := #0;
      end;
    end;
    if ((Key = ',') or (Key = '.')) then
    begin
      d := FormatSettings.DecimalSeparator;
      if (Pos(d, s) = 0) then
        Key := FormatSettings.DecimalSeparator
      else
        Key := #0;
    end;
  end
  else
    Key := #0;

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtVegetation.eSampleDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eSampleDate, dsLink.DataSet, 'sample_date');
end;

procedure TedtVegetation.eSampleTimeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtVegetation.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionVegetation)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionVegetation)]);

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
end;

function TedtVegetation.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('sample_date').IsNull = False) then
    Result := True;
end;

procedure TedtVegetation.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtVegetation.ValidateFields: Boolean;
begin
  Result := True;

end;

end.

