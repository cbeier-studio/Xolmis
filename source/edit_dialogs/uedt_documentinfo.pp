unit uedt_documentinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBCtrls, dbeditbutton,
  Character, atshapelinebgra;

type

  { TedtDocumentInfo }

  TedtDocumentInfo = class(TForm)
    cbDocumentType: TDBComboBox;
    cbLicenseType: TDBComboBox;
    dsLink: TDataSource;
    eDocumentPath: TDBEditButton;
    eLicenseNotes: TDBEdit;
    eLicenseOwner: TDBEdit;
    eDocumentTitle: TDBEdit;
    eLicenseUri: TDBEdit;
    eDocumentDate: TDBEditButton;
    eDocumentTime: TDBEdit;
    eLicenseYear: TDBEdit;
    lblDocumentPath: TLabel;
    lblDocumentType: TLabel;
    lblLicenseYear: TLabel;
    lblLicenseNotes: TLabel;
    lblLicenseOwner: TLabel;
    lblDocumentTitle: TLabel;
    lblLicenseUri: TLabel;
    lblDocumentDate: TLabel;
    lblDocumentTime: TLabel;
    lblLicenseType: TLabel;
    lineBottom: TShapeLineBGRA;
    pDocumentPath: TPanel;
    pDocumentType: TPanel;
    pBottom: TPanel;
    pClient: TPanel;
    pDateTime: TPanel;
    pLicenseNotes: TPanel;
    pLicenseOwner: TPanel;
    pDocumentTitle: TPanel;
    pLicenseUri: TPanel;
    pLicenseTypeYear: TPanel;
    sbCancel: TButton;
    SBox: TScrollBox;
    sbSave: TButton;
    procedure cbDocumentTypeKeyPress(Sender: TObject; var Key: char);
    procedure dsLinkDataChange(Sender: TObject; Field: TField);
    procedure eDocumentDateButtonClick(Sender: TObject);
    procedure eDocumentPathButtonClick(Sender: TObject);
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
  edtDocumentInfo: TedtDocumentInfo;

implementation

uses
  cbs_locale, cbs_global, cbs_datatypes, cbs_dialogs, cbs_finddialogs, cbs_taxonomy, cbs_gis, cbs_validations,
  udm_main, uDarkStyleParams;

{$R *.lfm}

{ TedtDocumentInfo }

procedure TedtDocumentInfo.ApplyDarkMode;
begin
  eDocumentDate.Images := DMM.iEditsDark;
  eDocumentPath.Images := DMM.iEditsDark;
end;

procedure TedtDocumentInfo.cbDocumentTypeKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> Key }
  if (Key = #13) and (XSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtDocumentInfo.dsLinkDataChange(Sender: TObject; Field: TField);
begin
  if dsLink.State = dsEdit then
    sbSave.Enabled := IsRequiredFilled and dsLink.DataSet.Modified
  else
    sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtDocumentInfo.eDocumentDateButtonClick(Sender: TObject);
begin
  CalendarDlg(eDocumentDate, dsLink.DataSet, 'document_date');
end;

procedure TedtDocumentInfo.eDocumentPathButtonClick(Sender: TObject);
begin
  DMM.OpenDocs.InitialDir := XSettings.LastPathUsed;
  if DMM.OpenDocs.Execute then
    dsLink.DataSet.FieldByName('document_path').AsString := DMM.OpenDocs.FileName;
end;

procedure TedtDocumentInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TedtDocumentInfo.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtDocumentInfo.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  cbDocumentType.Items.Clear;
  cbDocumentType.Items.Add(rsDocUrl);
  cbDocumentType.Items.Add(rsDocDocument);
  cbDocumentType.Items.Add(rsDocSpreadsheet);
  cbDocumentType.Items.Add(rsDocPresentation);
  cbDocumentType.Items.Add(rsDocPdf);
  cbDocumentType.Items.Add(rsDocCode);
  cbDocumentType.Items.Add(rsDocImage);
  cbDocumentType.Items.Add(rsDocAudio);
  cbDocumentType.Items.Add(rsDocDatabase);
  cbDocumentType.Items.Add(rsDocGis);
  cbDocumentType.Items.Add(rsDocOther);
end;

function TedtDocumentInfo.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (dsLink.DataSet.FieldByName('document_date').IsNull = False) and
    (dsLink.DataSet.FieldByName('document_path').AsString <> EmptyStr) then
    Result := True;
end;

procedure TedtDocumentInfo.sbSaveClick(Sender: TObject);
begin
  // Validate data
  if not ValidateFields then
    Exit;

  ModalResult := mrOk;
end;

function TedtDocumentInfo.ValidateFields: Boolean;
var
  Msgs: TStrings;
begin
  Result := True;
  Msgs := TStringList.Create;

  // Required fields
  RequiredIsEmpty(dsLink.DataSet, tbDocuments, 'document_date', Msgs);
  RequiredIsEmpty(dsLink.DataSet, tbDocuments, 'document_path', Msgs);

  if Msgs.Count > 0 then
  begin
    Result := False;
    ValidateDlg(Msgs);
  end;
  Msgs.Free;
end;

end.

