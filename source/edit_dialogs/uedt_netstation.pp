unit uedt_netstation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls,
  Character, DBEditButton, atshapelinebgra;

type

  { TedtNetStation }

  TedtNetStation = class(TForm)
    eName: TDBEdit;
    eAcronym: TDBEdit;
    dsLink: TDataSource;
    eLatitude: TDBEditButton;
    eLocality: TDBEditButton;
    eLongitude: TDBEditButton;
    lblBandStatus3: TLabel;
    lblBandStatus4: TLabel;
    lblBandStatus9: TLabel;
    lblNotes: TLabel;
    lblNotes1: TLabel;
    lblRequester1: TLabel;
    lblSupplier: TLabel;
    lineBottom: TShapeLineBGRA;
    mNotes: TDBMemo;
    mDescription: TDBMemo;
    pBottom: TPanel;
    pClient: TPanel;
    pNotes: TPanel;
    pNotes1: TPanel;
    pRequester1: TPanel;
    pStatus3: TPanel;
    pStatus4: TPanel;
    pSupplier: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eLocalityButtonClick(Sender: TObject);
    procedure eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
    procedure eLongitudeButtonClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    function IsRequiredFilled: Boolean;
    function ValidateFields: Boolean;
  public

  end;

var
  edtNetStation: TedtNetStation;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_gis, cbs_validations;

{$R *.lfm}

{ TedtNetStation }

procedure TedtNetStation.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtNetStation.eLocalityButtonClick(Sender: TObject);
begin
  FindSiteDlg([gfAll], eLocality, dsLink.DataSet, 'locality_id', 'locality_name');
end;

procedure TedtNetStation.eLocalityDBEditKeyPress(Sender: TObject; var Key: char);
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
    Key := #0;
  end;
  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNetStation.eLongitudeButtonClick(Sender: TObject);
begin
  GeoEditorDlg(TControl(Sender), dsLink.DataSet, 'longitude', 'latitude');
end;

procedure TedtNetStation.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtNetStation.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // CloseAction := caFree;
end;

procedure TedtNetStation.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtNetStation.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtNetStation.FormShow(Sender: TObject);
begin
  if dsLink.State = dsInsert then
    Caption := Format(rsTitleNew, [AnsiLowerCase(rsCaptionSamplingPlot)])
  else
    Caption := Format(rsTitleEditing, [AnsiLowerCase(rsCaptionSamplingPlot)]);
end;

function TedtNetStation.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('locality_id').AsInteger <> 0) and
    (dsLink.DataSet.FieldByName('station_name').AsString <> EmptyStr) and
    (dsLink.DataSet.FieldByName('station_acronym').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtNetStation.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtNetStation.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbNetStations, 'station_name', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNetStations, 'station_acronym', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbNetStations, 'locality_id', Msgs);

  // Duplicated record
  RecordDuplicated(tbNetStations, 'net_station_id', 'station_acronym',
    dsLink.DataSet.FieldByName('station_acronym').AsString,
    dsLink.DataSet.FieldByName('net_station_id').AsInteger, Msgs);

  // Foreign keys
  ForeignValueExists(tbGazetteer, 'site_id', dsLink.DataSet.FieldByName('locality_id').AsInteger,
    rsCaptionLocality, Msgs);

  // Geographical coordinates
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

