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
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBCtrls, StdCtrls, Character,
  DBEditButton, atshapelinebgra;

type

  { TedtSite }

  TedtSite = class(TForm)
    cbRank: TDBComboBox;
    eLatitude: TDBEditButton;
    eLongitude: TDBEditButton;
    eName: TDBEdit;
    dsLink: TDataSource;
    eAcronym: TDBEdit;
    eAltitude: TDBEdit;
    eParentSite: TDBEditButton;
    eEbirdName: TDBEdit;
    eFullName: TDBEdit;
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
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eParentSiteButtonClick(Sender: TObject);
    procedure eParentSiteDBEditKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  edtSite: TedtSite;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations, udm_main,
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
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtSite.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
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
end;

procedure TedtSite.eParentSiteButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eParentSite, dsLink.DataSet, 'parent_site_id', 'parent_site_name');
end;

procedure TedtSite.eParentSiteDBEditKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    FindSiteDlg([gfAll], eParentSite, dsLink.DataSet, 'parent_site_id', 'parent_site_name', Key);
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    dsLink.DataSet.FieldByName('parent_site_id').Clear;
    dsLink.DataSet.FieldByName('parent_site_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtSite.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtSite.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionToponym)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionToponym)]);

  cbRank.Items.Clear;
  cbRank.Items.Add(rsCaptionCountry);
  cbRank.Items.Add(rsCaptionState);
  cbRank.Items.Add(rsCaptionRegion);
  cbRank.Items.Add(rsCaptionMunicipality);
  cbRank.Items.Add(rsCaptionDistrict);
  cbRank.Items.Add(rsCaptionLocality);
end;

function TedtSite.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('site_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('full_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('site_rank').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtSite.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtSite.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_name', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbGazetteer, 'site_rank', Msgs);

  // Foreign keys
  ForeignValueExists(tbGazetteer, 'parent_site_id',
    dsLink.DataSet.FieldByName('parent_site_id').AsInteger, rsCaptionParentSite, Msgs);

  // Geographical coordinates
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

