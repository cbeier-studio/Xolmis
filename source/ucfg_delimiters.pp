unit ucfg_delimiters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, atshapelinebgra, BCPanel,
  rxswitch;

type

  { TcfgDelimiters }

  TcfgDelimiters = class(TForm)
    pDecimalSeparator: TBCPanel;
    cbDecimalSeparator: TComboBox;
    lblDecimalSeparator: TLabel;
    pDelimiter: TBCPanel;
    cbDelimiter: TComboBox;
    eOther: TEdit;
    lblDelimiter: TLabel;
    pQuotedAsText: TBCPanel;
    lblQuotedAsText: TLabel;
    pHaveHeader: TBCPanel;
    lblHaveHeader: TLabel;
    lblTitleFields: TLabel;
    lblTitleNumbers: TLabel;
    lineBottom: TShapeLineBGRA;
    pClient: TPanel;
    pBottom: TPanel;
    sbCancel: TButton;
    sbOK: TButton;
    tsHaveHeader: TRxSwitch;
    tsQuotedAsText: TRxSwitch;
    procedure cbDelimiterSelect(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    FDelimiter, FDecimal: Char;
    FQuotes, FHeader: Boolean;
    procedure SetQuotedAsText(aValue: Boolean);
    procedure SetDelimiter(aValue: Char);
    procedure SetDecimalSeparator(aValue: Char);
    procedure SetHaveHeader(aValue: Boolean);
  public
    property QuotedAsText: Boolean read FQuotes write FQuotes default False;
    property Delimiter: Char read FDelimiter write FDelimiter default ';';
    property DecimalSeparator: Char read FDecimal write FDecimal default ',';
    property HaveHeader: Boolean read FHeader write FHeader default True;
  end;

var
  cfgDelimiters: TcfgDelimiters;

implementation

uses cbs_global, cbs_graphics;

{$R *.lfm}

{ TcfgDelimiters }

procedure TcfgDelimiters.cbDelimiterSelect(Sender: TObject);
begin
  eOther.Visible := cbDelimiter.ItemIndex = 3;
end;

procedure TcfgDelimiters.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCEL = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, TComponent(Sender).Name, 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TcfgDelimiters.FormShow(Sender: TObject);
begin
  { #todo : Get the values from config }
  RoundPanels(pClient);
end;

procedure TcfgDelimiters.sbOKClick(Sender: TObject);
var
  cOther: Char;
  sOther: String;
begin
  GravaStat(Name, TComponent(Sender).Name, 'click');

  { #todo : Validate values }

  { Double quoted values as text }
  QuotedAsText := tsQuotedAsText.StateOn = sw_on;
  { Delimiter }
  cOther := #0;
  if (cbDelimiter.ItemIndex = 3) and (Length(Trim(eOther.Text)) > 0) then
  begin
    sOther := eOther.Text;
    cOther := sOther[1];
  end;
  case cbDelimiter.ItemIndex of
    0: Delimiter := ';'; { semicolon }
    1: Delimiter := ','; { comma }
    2: Delimiter := #9;  { <Tab> }
    3: Delimiter := cOther;   { other delimiter }
  end;
  { Decimal separator }
  case cbDecimalSeparator.ItemIndex of
    0: DecimalSeparator := ',';  { comma }
    1: DecimalSeparator := '.';  { period/point }
  end;
  { Have a header line with column names }
  HaveHeader := tsHaveHeader.StateOn = sw_on;

  ModalResult := mrOK;
end;

procedure TcfgDelimiters.SetQuotedAsText(aValue: Boolean);
begin
  FQuotes := aValue;
  if FQuotes then
    tsQuotedAsText.StateOn := sw_on
  else
    tsQuotedAsText.StateOn := sw_off;
end;

procedure TcfgDelimiters.SetDelimiter(aValue: Char);
begin
  FDelimiter := aValue;
  case FDelimiter of
    ';': cbDelimiter.ItemIndex := 0;
    ',': cbDelimiter.ItemIndex := 1;
    #9 : cbDelimiter.ItemIndex := 2;
  else
    cbDelimiter.ItemIndex := 3;
    eOther.Visible := True;
    eOther.Text := FDelimiter;
  end;
end;

procedure TcfgDelimiters.SetDecimalSeparator(aValue: Char);
begin
  FDecimal := aValue;
  case FDecimal of
    ',': cbDelimiter.ItemIndex := 0;
    '.': cbDelimiter.ItemIndex := 1;
  end;
end;

procedure TcfgDelimiters.SetHaveHeader(aValue: Boolean);
begin
  FHeader := aValue;
  if FHeader then
    tsHaveHeader.StateOn := sw_on
  else
    tsHaveHeader.StateOn := sw_off;
end;

end.

