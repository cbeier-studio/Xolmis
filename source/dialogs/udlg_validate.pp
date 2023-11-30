unit udlg_validate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, atshapelinebgra, HtmlView;

type

  { TdlgValidate }

  TdlgValidate = class(TForm)
    LV: THtmlViewer;
    lineBottom: TShapeLineBGRA;
    sbOK: TButton;
    pBottom: TPanel;
    procedure sbOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    lst: TStrings;
    FList: TStrings;
  public
    property Lista: TStrings read lst write lst;
  end;

var
  dlgValidate: TdlgValidate;

implementation

uses cbs_locale, cbs_global;

{$R *.lfm}

{ TdlgValidate }

procedure TdlgValidate.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBOK', 'click');
  ModalResult := mrOK;
end;

procedure TdlgValidate.FormCreate(Sender: TObject);
begin
  lst := TStringList.Create;
  FList := TStringList.Create;
end;

procedure TdlgValidate.FormDestroy(Sender: TObject);
begin
  lst.Free;
  FList.Free;
end;

procedure TdlgValidate.FormKeyPress(Sender: TObject; var Key: char);
begin
  { FECHAR = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, '', 'Esc');
    Key := #0;
    ModalResult := mrOK;
  end;
end;

procedure TdlgValidate.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if Lista.Count > 1 then
    FList.Add(Format(rsErrorsFound, [Lista.Count]))
  else
    FList.Add(Format(rsErrorFound, [Lista.Count]));

  FList.Add('<ul>');
  for i := 0 to Lista.Count - 1 do
  begin
    FList.Add('<li>' {'<font color="$001C2BC4">' + #$26A0 + '</font> '} + Lista[i] + '</li>');
    // Application.ProcessMessages;
  end;
  FList.Add('</ul>');

  LV.Caption := FList.Text;
end;

end.

