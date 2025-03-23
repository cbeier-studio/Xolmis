{ Xolmis Nest Editor dialog

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

unit uedt_nest;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EditBtn, Spin, SysUtils, Character, DB, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, atshapelinebgra,
  cbs_breeding;

type

  { TedtNest }

  TedtNest = class(TForm)
    cbNestFate: TComboBox;
    cbNestShape: TComboBox;
    cbSupportType: TComboBox;
    eOtherSupport: TEdit;
    eSupportPlant1: TEditButton;
    eSupportPlant2: TEditButton;
    eFieldNumber: TEdit;
    eTaxon: TEditButton;
    eFoundDate: TEditButton;
    eLastDate: TEditButton;
    eProject: TEditButton;
    eObserver: TEditButton;
    eLocality: TEditButton;
    eLongitude: TEditButton;
    eLatitude: TEditButton;
    dsLink: TDataSource;
    ePlantHeight: TFloatSpinEdit;
    ePlantDbh: TFloatSpinEdit;
    ePlantMaxDiameter: TFloatSpinEdit;
    ePlantMinDiameter: TFloatSpinEdit;
    eInternalMinDiameter: TFloatSpinEdit;
    eInternalMaxDiameter: TFloatSpinEdit;
    eExternalMinDiameter: TFloatSpinEdit;
    eExternalMaxDiameter: TFloatSpinEdit;
    eInternalHeight: TFloatSpinEdit;
    eExternalHeight: TFloatSpinEdit;
    eEdgeDistance: TFloatSpinEdit;
    eCenterDistance: TFloatSpinEdit;
    eBuildingDays: TFloatSpinEdit;
    eHeightAboveGround: TFloatSpinEdit;
    eIncubatingDays: TFloatSpinEdit;
    eNestlingDays: TFloatSpinEdit;
    eActiveDays: TFloatSpinEdit;
    lblBandStatus1: TLabel;
    lblExternalMinDiameter: TLabel;
    lblExternalMaxDiameter: TLabel;
    lblHeightAboveGround: TLabel;
    lblNestCover1: TLabel;
    lblPlantDbh: TLabel;
    lblPlantMinDiameter: TLabel;
    lblConstructionDays: TLabel;
    lblIncubationDays: TLabel;
    lblNestlingDays: TLabel;
    lblActiveDays: TLabel;
    lblInternalMinDiameter: TLabel;
    lblInternalMaxDiameter: TLabel;
    lblEdgeDistance: TLabel;
    lblCenterDistance: TLabel;
    lblInternalHeight: TLabel;
    lblExternalHeight: TLabel;
    lblBandStatus3: TLabel;
    lblBandStatus4: TLabel;
    lblRequester5: TLabel;
    lblSupportType: TLabel;
    lblPlantHeight: TLabel;
    lblPlantMaxDiameter: TLabel;
    lblBandStatus8: TLabel;
    lblBandStatus9: TLabel;
    lblNotes: TLabel;
    lblNotes1: TLabel;
    lblRequester: TLabel;
    lblRequester1: TLabel;
    lblSupportPlant1: TLabel;
    lblOtherSupport: TLabel;
    lblSupportPlant2: TLabel;
    lblRequester2: TLabel;
    lblRequester3: TLabel;
    lblNestCover: TLabel;
    lblLocality: TLabel;
    lblSupplier1: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    mNotes: TMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pDescription: TPanel;
    pFieldNumberFate: TPanel;
    pObserver: TPanel;
    pExternalDiameter: TPanel;
    pSupportPlant2: TPanel;
    pNestCover: TPanel;
    pProductivity: TPanel;
    pSupportPlant1: TPanel;
    pOtherSupport: TPanel;
    pTaxon: TPanel;
    pInternalDiameter: TPanel;
    pEdgeCenterDistance: TPanel;
    pInternalExternalHeight: TPanel;
    pDateFoundLast: TPanel;
    pLongLat: TPanel;
    pTypeHeight: TPanel;
    pPlantHeightDbh: TPanel;
    pPlantDiameter: TPanel;
    pConstructionIncubationDays: TPanel;
    pNestlingActiveDays: TPanel;
    pLocality: TPanel;
    pProject: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    eNestCover: TSpinEdit;
    eProductivity: TSpinEdit;
    procedure cbSupportTypeSelect(Sender: TObject);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eFieldNumberKeyPress(Sender: TObject; var Key: char);
    procedure eFoundDateButtonClick(Sender: TObject);
    procedure eLastDateButtonClick(Sender: TObject);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eLongitudeKeyPress(Sender: TObject; var Key: char);
    procedure eObserverButtonClick(Sender: TObject);
    procedure eObserverKeyPress(Sender: TObject; var Key: char);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectKeyPress(Sender: TObject; var Key: char);
    procedure eSupportPlant1ButtonClick(Sender: TObject);
    procedure eSupportPlant1KeyPress(Sender: TObject; var Key: char);
    procedure eSupportPlant2ButtonClick(Sender: TObject);
    procedure eSupportPlant2KeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonEditingDone(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FNest: TNest;
    FTaxonId, FProjectId, FObserverId, FLocalityId: Integer;
    FSupportPlant1Id, FSupportPlant2Id: Integer;
    procedure SetNest(Value: TNest);
    procedure GetRecord;
    procedure SetRecord;
    procedure ApplyDarkMode;
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public
    property IsNewRecord: Boolean read FIsNew write FIsNew default False;
    property Nest: TNest read FNest write SetNest;
  end;

var
  edtNest: TedtNest;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_validations, cbs_fullnames, cbs_getvalue,
  cbs_taxonomy, cbs_gis, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtNest }

procedure TedtNest.ApplyDarkMode;
begin
  eTaxon.Images := DMM.iEditsDark;
  eFoundDate.Images := DMM.iEditsDark;
  eLastDate.Images := DMM.iEditsDark;
  eProject.Images := DMM.iEditsDark;
  eObserver.Images := DMM.iEditsDark;
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eSupportPlant1.Images := DMM.iEditsDark;
  eSupportPlant2.Images := DMM.iEditsDark;
end;

procedure TedtNest.cbSupportTypeSelect(Sender: TObject);
begin
  if cbSupportType.ItemIndex = cbSupportType.Items.Count - 1 then
    pOtherSupport.Visible := True
  else
    pOtherSupport.Visible := False;
end;

procedure TedtNest.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  //if dsLink.State = dsEdit then
  //  sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  //else
  //  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNest.eFieldNumberKeyPress(Sender: TObject; var Key: char);
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
end;

procedure TedtNest.eFoundDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eFoundDate.Text, eFoundDate, Dt);
end;

procedure TedtNest.eLastDateButtonClick(Sender: TObject);
var
  Dt: TDate;
begin
  CalendarDlg(eLastDate.Text, eLastDate, Dt);
end;

procedure TedtNest.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, FLocalityId);
end;

procedure TedtNest.eLocalityKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eLocality, FLocalityId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FLocalityId := 0;
    eLocality.Clear;
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

procedure TedtNest.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), eLongitude, eLatitude);
end;

procedure TedtNest.eLongitudeKeyPress(Sender: TObject; var Key: char);
const
  AllowedChars = ['0'..'9', ',', '.', '+', '-', #8, #13, #27];
var
  EditText: String;
  PosDecimal: Integer;
  DecimalValue: Extended;
begin
  FormKeyPress(Sender, Key);

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

procedure TedtNest.eObserverButtonClick(Sender: TObject);
begin
  FindDlg(tbPeople, eObserver, FObserverId);
end;

procedure TedtNest.eObserverKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbPeople, eObserver, FObserverId, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FObserverId := 0;
    eObserver.Clear;
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

procedure TedtNest.eProjectButtonClick(Sender: TObject);
begin
  FindDlg(tbProjects, eProject, FProjectId);
end;

procedure TedtNest.eProjectKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindDlg(tbProjects, eProject, FProjectId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FProjectId := 0;
    eProject.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eSupportPlant1ButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eSupportPlant1, FSupportPlant1Id);
end;

procedure TedtNest.eSupportPlant1KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eSupportPlant1, FSupportPlant1Id, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FSupportPlant1Id := 0;
    eSupportPlant1.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eSupportPlant2ButtonClick(Sender: TObject);
begin
  FindBotanicDlg([tfAll], eSupportPlant2, FSupportPlant2Id);
end;

procedure TedtNest.eSupportPlant2KeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindBotanicDlg([tfAll], eSupportPlant2, FSupportPlant2Id, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FSupportPlant2Id := 0;
    eSupportPlant2.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies,tfSubspecies,tfSubspeciesGroups], eTaxon, True, FTaxonId);
end;

procedure TedtNest.eTaxonEditingDone(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNest.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies, tfSubspecies, tfSubspeciesGroups], eTaxon, True, FTaxonId, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    if (Sender is TEditButton) then
      Screen.ActiveForm.SelectNext(Screen.ActiveControl, True, True)
    else
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNest.FormCreate(Sender: TObject);
begin
  cbNestFate.Items.CommaText := '"' + rsNestLost + '","' + rsNestSuccess + '","' + rsNestUnknown + '"';
  cbSupportType.Items.CommaText := '"' + rsSupportGround + '","' +
    rsSupportHerbBush + '","' + rsSupportBranchFork + '","' + rsSupportLeaves + '","' +
    rsSupportLedge + '","' + rsSupportRockCliff + '","' + rsSupportRavine + '","' + rsSupportNestBox + '","' +
    rsSupportAnthropic + '","' + rsSupportOther + '"';
  cbNestShape.Items.CommaText := '"' + rsNestShapeScrape + '","' + rsNestShapeCup + '","' +
    rsNestShapePlate + '","' + rsNestShapeSphere + '","' + rsNestShapePendent + '","' +
    rsNestShapePlatform + '","' + rsNestShapeMound + '","' + rsNestShapeBurrow + '","' + rsNestShapeCavity + '"';
end;

procedure TedtNest.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsLink.State in [dsInsert, dsEdit]) then
    if not sbSave.Enabled then
      Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtNest.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNest.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if FIsNew then
  begin
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionNest)]);
    if FNest.TaxonId > 0 then
    begin
      FTaxonId := FNest.TaxonId;
      eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
    end;
  end
  else
  begin
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionNest)]);
    GetRecord;
    sbSave.Enabled := IsRequiredFilled;
  end;

  sBox.VertScrollBar.Position := 0;

  eTaxon.SetFocus;

end;

procedure TedtNest.GetRecord;
begin
  FTaxonId := FNest.TaxonId;
  eTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FTaxonId);
  eFieldNumber.Text := FNest.FieldNumber;
  case FNest.NestFate of
    nfLoss:     cbNestFate.ItemIndex := cbNestFate.Items.IndexOf(rsNestLost);
    nfSuccess:  cbNestFate.ItemIndex := cbNestFate.Items.IndexOf(rsNestSuccess);
    nfUnknown:  cbNestFate.ItemIndex := cbNestFate.Items.IndexOf(rsNestUnknown);
  end;
  if not DateIsNull(FNest.FoundDate) then
    eFoundDate.Text := DateToStr(FNest.FoundDate);
  if not DateIsNull(FNest.LastDate) then
    eLastDate.Text := DateToStr(FNest.LastDate);
  FProjectId := FNest.ProjectId;
  eProject.Text := GetName('projects', 'short_title', 'project_id', FProjectId);
  FObserverId := FNest.ObserverId;
  eObserver.Text := GetName('people', 'full_name', 'person_id', FObserverId);
  FLocalityId := FNest.LocalityId;
  eLocality.Text := GetName('gazetteer', 'full_name', 'site_id', FLocalityId);
  if (FNest.Longitude <> 0) and (FNest.Latitude <> 0) then
  begin
    eLongitude.Text := FloatToStr(FNest.Longitude);
    eLatitude.Text := FloatToStr(FNest.Latitude);
  end;
  mDescription.Text := FNest.Description;
  eProductivity.Value := FNest.NestProductivity;
  case FNest.NestShape of
    'SC': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapeScrape);
    'CP': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapeCup);
    'PT': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapePlate);
    'SP': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapeSphere);
    'PD': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapePendent);
    'PL': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapePlatform);
    'MN': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapeMound);
    'BR': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapeBurrow);
    'CV': cbNestShape.ItemIndex := cbNestShape.Items.IndexOf(rsNestShapeCavity);
  end;
  case FNest.SupportType of
    'G': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportGround);
    'H': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportHerbBush);
    'F': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportBranchFork);
    'L': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportLeaves);
    'D': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportLedge);
    'C': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportRockCliff);
    'R': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportRavine);
    'B': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportNestBox);
    'A': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportAnthropic);
    'O': cbSupportType.ItemIndex := cbSupportType.Items.IndexOf(rsSupportOther);
  end;
  eHeightAboveGround.Value := FNest.HeightAboveGround;
  FSupportPlant1Id := FNest.SupportPlant1Id;
  eSupportPlant1.Text := GetName('botanic_taxa', 'taxon_name', 'taxon_id', FSupportPlant1Id);
  FSupportPlant2Id := FNest.SupportPlant2Id;
  eSupportPlant2.Text := GetName('botanic_taxa', 'taxon_name', 'taxon_id', FSupportPlant2Id);
  eOtherSupport.Text := FNest.OtherSupport;
  ePlantHeight.Value := FNest.PlantHeight;
  ePlantDbh.Value := FNest.PlantDbh;
  ePlantMaxDiameter.Value := FNest.PlantMaxDiameter;
  ePlantMinDiameter.Value := FNest.PlantMinDiameter;
  eBuildingDays.Value := FNest.ConstructionDays;
  eIncubatingDays.Value := FNest.IncubationDays;
  eNestlingDays.Value := FNest.NestlingDays;
  eActiveDays.Value := FNest.ActiveDays;
  eInternalMinDiameter.Value := FNest.InternalMinDiameter;
  eInternalMaxDiameter.Value := FNest.InternalMaxDiameter;
  eExternalMinDiameter.Value := FNest.ExternalMinDiameter;
  eExternalMaxDiameter.Value := FNest.ExternalMaxDiameter;
  eInternalHeight.Value := FNest.InternalHeight;
  eExternalHeight.Value := FNest.ExternalHeight;
  eEdgeDistance.Value := FNest.EdgeDistance;
  eCenterDistance.Value := FNest.CenterDistance;
  eNestCover.Value := FNest.NestCover;
  mNotes.Text := FNest.Notes;
end;

function TedtNest.IsRequiredFilled: Boolean;
begin
  Result := False;

  //if (dsLink.DataSet.FieldByName('field_number').AsString <> EmptyStr) and
  //  (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('observer_id').AsInteger <> 0) and
  //  (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) then
  if (eFieldNumber.Text <> EmptyStr) and
    (FTaxonId > 0) and
    (FObserverId > 0) and
    (FLocalityId > 0) then
    Result := True;
end;

procedure TedtNest.sbSaveClick(Sender: TObject);
begin
  // Data validation
  if not ValidateFields then
    Exit;

  // Workaround to not post zero value when it is null
  //with dsLink.DataSet do
  //begin
  //  if (FieldByName('longitude').AsFloat = 0.0) and (FieldByName('latitude').AsFloat = 0.0)
  //  then
  //  begin
  //    FieldByName('longitude').Clear;
  //    FieldByName('latitude').Clear;
  //  end;
  //end;

  SetRecord;

  ModalResult := mrOk;
end;

procedure TedtNest.SetNest(Value: TNest);
begin
  if Assigned(Value) then
    FNest := Value;
end;

procedure TedtNest.SetRecord;
begin
  FNest.TaxonId     := FTaxonId;
  FNest.FieldNumber := eFieldNumber.Text;
  case cbNestFate.ItemIndex of
    0: FNest.NestFate := nfLoss;
    1: FNest.NestFate := nfSuccess;
    2: FNest.NestFate := nfUnknown;
  end;
  if eFoundDate.Text <> EmptyStr then
    FNest.FoundDate         := StrToDate(eFoundDate.Text);
  if eLastDate.Text <> EmptyStr then
    FNest.LastDate          := StrToDate(eLastDate.Text);
  FNest.ProjectId         := FProjectId;
  FNest.ObserverId        := FObserverId;
  FNest.LocalityId        := FLocalityId;
  if eLongitude.Text <> EmptyStr then
    FNest.Longitude         := StrToFloat(eLongitude.Text);
  if eLatitude.Text <> EmptyStr then
    FNest.Latitude          := StrToFloat(eLatitude.Text);
  FNest.Description       := mDescription.Text;
  FNest.NestProductivity  := eProductivity.Value;
  case cbNestShape.ItemIndex of
    0: FNest.NestShape := 'SC';
    1: FNest.NestShape := 'CP';
    2: FNest.NestShape := 'PT';
    3: FNest.NestShape := 'SP';
    4: FNest.NestShape := 'PD';
    5: FNest.NestShape := 'PL';
    6: FNest.NestShape := 'MN';
    7: FNest.NestShape := 'BR';
    8: FNest.NestShape := 'CV';
  end;
  case cbSupportType.ItemIndex of
    0: FNest.SupportType := 'G';
    1: FNest.SupportType := 'H';
    2: FNest.SupportType := 'F';
    3: FNest.SupportType := 'L';
    4: FNest.SupportType := 'D';
    5: FNest.SupportType := 'C';
    6: FNest.SupportType := 'R';
    7: FNest.SupportType := 'B';
    8: FNest.SupportType := 'A';
    9: FNest.SupportType := 'O';
  end;
  FNest.HeightAboveGround   := eHeightAboveGround.Value;
  FNest.SupportPlant1Id     := FSupportPlant1Id;
  FNest.SupportPlant2Id     := FSupportPlant2Id;
  FNest.OtherSupport        := eOtherSupport.Text;
  FNest.PlantHeight         := ePlantHeight.Value;
  FNest.PlantDbh            := ePlantDbh.Value;
  FNest.PlantMaxDiameter    := ePlantMaxDiameter.Value;
  FNest.PlantMinDiameter    := ePlantMinDiameter.Value;
  FNest.ConstructionDays    := eBuildingDays.Value;
  FNest.IncubationDays      := eIncubatingDays.Value;
  FNest.NestlingDays        := eNestlingDays.Value;
  FNest.ActiveDays          := eActiveDays.Value;
  FNest.InternalMinDiameter := eInternalMinDiameter.Value;
  FNest.InternalMaxDiameter := eInternalMaxDiameter.Value;
  FNest.ExternalMinDiameter := eExternalMinDiameter.Value;
  FNest.ExternalMaxDiameter := eExternalMaxDiameter.Value;
  FNest.InternalHeight      := eInternalHeight.Value;
  FNest.ExternalHeight      := eExternalHeight.Value;
  FNest.EdgeDistance        := eEdgeDistance.Value;
  FNest.CenterDistance      := eCenterDistance.Value;
  FNest.NestCover           := eNestCover.Value;
  FNest.Notes               := mNotes.Text;
end;

function TedtNest.ValidateFields: Boolean;
var
  Msgs: TStrings;
  Msg: String;
begin
  Result := True;
  Msg := EmptyStr;
  Msgs := TStringList.Create;

  // Required fields
  //RequiredIsEmpty(dsLink.DataSet, tbNests, 'field_number', Msgs);
  //RequiredIsEmpty(dsLink.DataSet, tbNests, 'locality_id', Msgs);

  // Duplicated record
  RecordDuplicated(tbNests, 'nest_id', 'field_number', eFieldNumber.Text, FNest.Id);

  // Foreign keys
  //ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
  //  rsCaptionLocality, Msgs);
  //ForeignValueExists(tbZooTaxa, 'taxon_id', dsLink.DataSet.FieldByName('taxon_id').AsInteger,
  //  rsCaptionTaxon, Msgs);
  //ForeignValueExists(tbPeople, 'person_id', dsLink.DataSet.FieldByName('observer_id').AsInteger,
  //  rsCaptionObserver, Msgs);
  //ForeignValueExists(tbProjects, 'project_id', dsLink.DataSet.FieldByName('project_id').AsInteger,
  //  rsCaptionProject, Msgs);

  // Dates
  if eFoundDate.Text <> EmptyStr then
    ValidDate(eFoundDate.Text, rsDateFound, Msgs);
  if eLastDate.Text <> EmptyStr then
    ValidDate(eLastDate.Text, rsDateLast, Msgs);

  // Geographical coordinates
  if eLongitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLongitude.Text), -180.0, 180.0, rsLongitude, Msgs, Msg);
  if eLatitude.Text <> EmptyStr then
    ValueInRange(StrToFloat(eLatitude.Text), -90.0, 90.0, rsLatitude, Msgs, Msg);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

