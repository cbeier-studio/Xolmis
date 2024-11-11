{ Xolmis Specimen Editor dialog

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

unit uedt_specimen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  DBEditButton, DB, DateUtils, atshapelinebgra;

type

  { TedtSpecimen }

  TedtSpecimen = class(TForm)
    cbSampleType: TDBComboBox;
    dsLink: TDataSource;
    eCollectionDay: TDBEdit;
    eCollectionMonth: TDBEdit;
    eCollectionYear: TDBEdit;
    eFieldNumber: TDBEdit;
    eLatitude: TDBEditButton;
    eLongitude: TDBEditButton;
    eIndividual: TDBEditButton;
    eNest: TDBEditButton;
    eEgg: TDBEditButton;
    eLocality: TDBEditButton;
    eTaxon: TDBEditButton;
    lblBandStatus4: TLabel;
    lblBandStatus9: TLabel;
    lblCollectionDate1: TLabel;
    lblNotes: TLabel;
    lblFieldNumber: TLabel;
    lblCollectionDate: TLabel;
    lblIndividual: TLabel;
    lblNest: TLabel;
    lblEgg: TLabel;
    lblLocality: TLabel;
    lblSampleType: TLabel;
    lblTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    pBirthDate: TPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pNotes: TPanel;
    pFieldNumber: TPanel;
    pCollectionDate: TPanel;
    pIndividual: TPanel;
    pNest: TPanel;
    pEgg: TPanel;
    pLocality: TPanel;
    pStatus4: TPanel;
    pTaxon: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    scrollContent: TScrollBox;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eEggButtonClick(Sender: TObject);
    procedure eEggDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eFieldNumberKeyPress(Sender: TObject; var Key: char);
    procedure eIndividualButtonClick(Sender: TObject);
    procedure eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLatitudeButtonClick(Sender: TObject);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eNestButtonClick(Sender: TObject);
    procedure eNestDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
  edtSpecimen: TedtSpecimen;

implementation

uses
  cbs_locale, cbs_global, cbs_system, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_taxonomy,
  cbs_validations, udm_main, uDarkStyleParams;

{ TedtSpecimen }

procedure TedtSpecimen.ApplyDarkMode;
begin
  eLocality.Images := DMM.iEditsDark;
  eLongitude.Images := DMM.iEditsDark;
  eLatitude.Images := DMM.iEditsDark;
  eTaxon.Images := DMM.iEditsDark;
  eIndividual.Images := DMM.iEditsDark;
  eNest.Images := DMM.iEditsDark;
  eEgg.Images := DMM.iEditsDark;
end;

procedure TedtSpecimen.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSpecimen.eEggButtonClick(Sender: TObject);
begin
  FindDlg(tbEggs, eEgg, dsLink.DataSet, 'egg_id', 'egg_name');
end;

procedure TedtSpecimen.eEggDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbEggs, eEgg, dsLink.DataSet, 'egg_id', 'egg_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('egg_id').Clear;
    dsLink.DataSet.FieldByName('egg_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eFieldNumberKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eIndividualButtonClick(Sender: TObject);
begin
  FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name');
end;

procedure TedtSpecimen.eIndividualDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbIndividuals, eIndividual, dsLink.DataSet, 'individual_id', 'individual_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('individual_id').Clear;
    dsLink.DataSet.FieldByName('individual_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eLatitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtSpecimen.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtSpecimen.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
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

procedure TedtSpecimen.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtSpecimen.eNestButtonClick(Sender: TObject);
begin
  FindDlg(tbNests, eNest, dsLink.DataSet, 'nest_id', 'nest_name');
end;

procedure TedtSpecimen.eNestDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindDlg(tbNests, eNest, dsLink.DataSet, 'nest_id', 'nest_name', False, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('nest_id').Clear;
    dsLink.DataSet.FieldByName('nest_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.eTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True);
end;

procedure TedtSpecimen.eTaxonDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindTaxonDlg([tfAll], eTaxon, dsLink.DataSet, 'taxon_id', 'taxon_name', True, Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('taxon_id').Clear;
    dsLink.DataSet.FieldByName('taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSpecimen.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtSpecimen.FormCreate(Sender: TObject);
begin
  cbSampleType.Items.Clear;
  cbSampleType.Items.Add(rsSpecimenCarcassWhole);
  cbSampleType.Items.Add(rsSpecimenCarcassPartial);
  cbSampleType.Items.Add(rsSpecimenNest);
  cbSampleType.Items.Add(rsSpecimenBones);
  cbSampleType.Items.Add(rsSpecimenEgg);
  cbSampleType.Items.Add(rsSpecimenParasites);
  cbSampleType.Items.Add(rsSpecimenFeathers);
  cbSampleType.Items.Add(rsSpecimenBlood);
  cbSampleType.Items.Add(rsSpecimenClaw);
  cbSampleType.Items.Add(rsSpecimenSwab);
  cbSampleType.Items.Add(rsSpecimenTissues);
  cbSampleType.Items.Add(rsSpecimenFeces);
  cbSampleType.Items.Add(rsSpecimenRegurgite);
end;

procedure TedtSpecimen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtSpecimen.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtSpecimen.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionEgg)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionEgg)]);
end;

function TedtSpecimen.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('field_number').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('sample_type').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('collection_year').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('taxon_id').AsInteger <> 0) then
    Result := True;
end;

procedure TedtSpecimen.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtSpecimen.ValidateFields: Boolean;
var
  Msgs: TStrings;
  D: TDataSet;
  DateColl, Hoje: TPartialDate;
begin
  Result := True;
  Msgs := TStringList.Create;
  D := dsLink.DataSet;

  // Required fields
  RequiredIsEmpty(D, tbSpecimens, 'field_number', Msgs);
  RequiredIsEmpty(D, tbSpecimens, 'sample_type', Msgs);
  RequiredIsEmpty(D, tbSpecimens, 'collection_year', Msgs);
  RequiredIsEmpty(D, tbSpecimens, 'taxon_id', Msgs);
  RequiredIsEmpty(D, tbSpecimens, 'locality_id', Msgs);

  // Duplicated record
  RecordDuplicated(tbSpecimens, 'specimen_id', 'full_name',
    D.FieldByName('full_name').AsString, D.FieldByName('specimen_id').AsInteger);

  // Dates
  Hoje.Today;
  if D.FieldByName('collection_year').AsInteger > 0 then
  begin
    DateColl.Year := D.FieldByName('collection_year').AsInteger;
    DateColl.Month := D.FieldByName('collection_month').AsInteger;
    DateColl.Day := D.FieldByName('collection_day').AsInteger;
    if ValidPartialDate(DateColl, rsDateCollection, Msgs) then
      IsFuturePartialDate(DateColl, Hoje, rsDateCollection, LowerCase(rsDateToday), Msgs);
  end;

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

initialization
  {$I uedt_specimen.lrs}

end.

