unit udlg_taxonnotfound;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Character, StdCtrls, ExtCtrls, Buttons,
  EditBtn, ATShapeLineBGRA;

type

  { TdlgTaxonNotFound }

  TdlgTaxonNotFound = class(TForm)
    eTaxon: TEditButton;
    lblMessage: TLabel;
    lblChoose: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    rbTemporaryTaxon: TRadioButton;
    rbValidTaxon: TRadioButton;
    rbAbort: TRadioButton;
    sbOK: TBitBtn;
    txtTaxonName: TLabel;
    procedure eTaxonButtonClick(Sender: TObject);
    procedure eTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    FTaxonName: String;
    FSelectedOption, FTaxonId: Integer;
    FShowCustomTaxonOption: Boolean;
    procedure ApplyDarkMode;
  public
    property TaxonName: String read FTaxonName write FTaxonName;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property SelectedOption: Integer read FSelectedOption write FSelectedOption;
    property ShowCustomTaxonOption: Boolean read FShowCustomTaxonOption write FShowCustomTaxonOption;
  end;

var
  dlgTaxonNotFound: TdlgTaxonNotFound;

implementation

uses
  utils_locale, utils_global, utils_dialogs, utils_finddialogs, models_record_types, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgTaxonNotFound }

procedure TdlgTaxonNotFound.ApplyDarkMode;
begin
  eTaxon.Images := DMM.iEditsDark;
end;

procedure TdlgTaxonNotFound.eTaxonButtonClick(Sender: TObject);
begin
  if FindTaxonDlg([tfAll], eTaxon, True, FTaxonId) then
    rbValidTaxon.Checked := True;
end;

procedure TdlgTaxonNotFound.eTaxonKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key) then
  begin
    if FindTaxonDlg([tfAll], eTaxon, True, FTaxonId, Key) then
      rbValidTaxon.Checked := True;
    Key := #0;
  end;
  { CLEAR FIELD = Backspace }
  if (Key = #8) then
  begin
    FTaxonId := 0;
    eTaxon.Clear;
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

procedure TdlgTaxonNotFound.FormCreate(Sender: TObject);
begin
  FShowCustomTaxonOption := True;
end;

procedure TdlgTaxonNotFound.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  txtTaxonName.Caption := FTaxonName;
  rbTemporaryTaxon.Visible := FShowCustomTaxonOption;
  if not FShowCustomTaxonOption then
    rbValidTaxon.Checked := True;
end;

procedure TdlgTaxonNotFound.sbOKClick(Sender: TObject);
begin
  if rbTemporaryTaxon.Checked then
    FSelectedOption := 0
  else
  if rbValidTaxon.Checked then
  begin
    if eTaxon.Text = EmptyStr then
    begin
      MsgDlg(rsTitleInformation, rsErrorTaxonMustBeSelected, mtInformation);
      Exit;
    end;
    FSelectedOption := 1
  end
  else
    FSelectedOption := 2;

  ModalResult := mrOK;
end;

end.

