unit udlg_loading;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCFluentProgressRing;

type

  { TdlgLoading }

  TdlgLoading = class(TForm)
    ringProgress: TBCFluentProgressRing;
    lblLoading: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public
    procedure UpdateProgress(const aMessage: String; aPercent: Integer);
  end;

var
  dlgLoading: TdlgLoading;

implementation

uses
  cbs_system;

{$R *.lfm}

{ TdlgLoading }

procedure TdlgLoading.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}
end;

procedure TdlgLoading.UpdateProgress(const aMessage: String; aPercent: Integer);
begin
  lblLoading.Caption := aMessage;
  if aPercent < 0 then
  begin
    ringProgress.Indeterminate := True;
  end
  else
  begin
    ringProgress.Indeterminate := False;
    ringProgress.Value := aPercent;
  end;
  //Application.ProcessMessages;
end;

end.

