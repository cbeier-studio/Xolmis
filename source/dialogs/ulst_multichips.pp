{ Xolmis Detection Type Editor dialog

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

unit ulst_multichips;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, chipspanel,
  atshapelinebgra, fpjson, jsonparser, utils_dialogs;

type

  { TlstMultiChips }

  TlstMultiChips = class(TForm)
    lblChips: TLabel;
    pChips: TChipsPanel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    strJson: String;
    dlgType: TMultiSelectionDataType;
    procedure ApplyDarkMode;
  public
    property DialogDataType: TMultiSelectionDataType read dlgType write dlgType;
    property JsonString: String read strJson write strJson;
  end;

var
  lstMultiChips: TlstMultiChips;

implementation

uses
  utils_locale, utils_global, utils_themes, data_columns, uDarkStyleParams;

{$R *.lfm}

{ TlstMultiChips }

procedure TlstMultiChips.ApplyDarkMode;
begin
  pChips.DarkMode := True;
end;

procedure TlstMultiChips.FormCreate(Sender: TObject);
begin
  JsonString := '';
end;

procedure TlstMultiChips.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCELAR = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { APLICAR = Enter }
  if Key = #13 then
  begin
    GravaStat(Name, '', 'Enter');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Enter/Return');
    {$ENDIF}
    Key := #0;
    sbOKClick(nil);
  end;
end;

procedure TlstMultiChips.FormShow(Sender: TObject);
var
  Arr: TJSONArray;
  i: Integer;
  Value: String;
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  pChips.BeginUpdate;
  try
    pChips.SingleChipColorDark := ActiveTheme.Brand.JacarandaDeep;
    pChips.SingleChipColorLight := ActiveTheme.Brand.JacarandaContainer;

    case dlgType of
      cdtDetectionModes:
      begin
        Caption := rscDetectionModes;

        with pChips do
        begin
          AddChip(rsDetectionVisual, 'visual');
          AddChip(rsDetectionSong, 'song');
          AddChip(rsDetectionCall, 'call');
          AddChip(rsDetectionWingFlapping, 'wing_flapping');
          AddChip(rsDetectionDrumming, 'drumming');
          AddChip(rsDetectionCapture, 'capture');
          AddChip(rsDetectionRemote, 'remote');
          AddChip(rsDetectionTrack, 'track');
          AddChip(rsDetectionOther, 'other');
        end;
      end;
      cdtEvidences:
      begin
        Caption := rscEvidences;

        with pChips do
        begin
          AddChip(rsEvidencePhoto, 'photo');
          AddChip(rsEvidenceAudio, 'audio');
          AddChip(rsEvidenceVideo, 'video');
          AddChip(rsEvidenceCapture, 'capture');
          AddChip(rsEvidenceSample, 'sample');
          AddChip(rsEvidenceOther, 'other');
        end;
      end;
      cdtHabitats:
      begin
        Caption := rscHabitats;

        with pChips do
        begin
          AddChip(rsHabitatForest, 'forest');
          AddChip(rsHabitatWoods, 'woods');
          AddChip(rsHabitatSavanna, 'savanna');
          AddChip(rsHabitatShrubs, 'shrubs');
          AddChip(rsHabitatGrassland, 'grassland');
          AddChip(rsHabitatWetland, 'wetland');
          AddChip(rsHabitatCoastal, 'coastal');
          AddChip(rsHabitatMarine, 'marine');
          AddChip(rsHabitatMangrove, 'mangrove');
          AddChip(rsHabitatFreshwater, 'freshwater');
          AddChip(rsHabitatCropland, 'cropland');
          AddChip(rsHabitatUrban, 'urban');
          AddChip(rsHabitatRocky, 'rocky');
          AddChip(rsHabitatDesert, 'desert');
          AddChip(rsHabitatOther, 'other');
        end;
      end;
      cdtActivities:
      begin
        Caption := rscActivities;

        with pChips do
        begin
          AddChip(rsActivityResting, 'resting');
          AddChip(rsActivityFeeding, 'feeding');
          AddChip(rsActivityPreening, 'preening');
          AddChip(rsActivityFlying, 'flying');
          AddChip(rsActivityWalking, 'walking');
          AddChip(rsActivitySwimming, 'swimming');
          AddChip(rsActivityVocalizing, 'vocalizing');
          AddChip(rsActivitySocial, 'social');
          AddChip(rsActivityCourtship, 'courtship');
          AddChip(rsActivityNestBuilding, 'nesting');
          AddChip(rsActivityIncubationBroodCare, 'brooding');
          AddChip(rsActivityTerritorial, 'territorial');
          AddChip(rsActivityVigilance, 'vigilance');
          AddChip(rsActivityPredatorAvoidance, 'predator');
          AddChip(rsActivityOther, 'other');
        end;
      end;
    end;

    pChips.AutoSize := True;
  finally
    pChips.EndUpdate;
  end;

  if JsonString = EmptyStr then
    JsonString := '[]';
  if JsonString <> '' then
  begin
    with pChips do
    begin
      Arr := GetJSON(JsonString) as TJSONArray;
      try
        for i := 0 to Arr.Count - 1 do
        begin
          Value := Arr.Strings[i];

          SelectByID(Value);
        end;
      finally
        Arr.Free;
      end;
    end;
  end;
end;

procedure TlstMultiChips.sbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TlstMultiChips.sbOKClick(Sender: TObject);
var
  Arr: TJSONArray;
  I: Integer;
  Chip: TChip;
begin
  Arr := TJSONArray.Create;
  try
    for I := 0 to pChips.GetSelectedChips.Count - 1 do
    begin
      Chip := TChip(pChips.GetSelectedChips[I]);
      Arr.Add(Chip.ID);
    end;

    JsonString := Arr.AsJSON;
  finally
    Arr.Free;
  end;

  ModalResult := mrOK;
end;

end.

