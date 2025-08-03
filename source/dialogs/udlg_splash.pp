unit udlg_splash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, BCFluentProgressRing,
  BCMaterialProgressBarMarquee;

type

  { TdlgSplash }

  TdlgSplash = class(TForm)
    icoAppIcon: TImage;
    lblProgress: TLabel;
    lblAppName: TLabel;
    PBar: TProgressBar;
    procedure FormShow(Sender: TObject);
  private

  public
    procedure UpdateProgress(const Msg: String; Percent: Integer);
  end;

var
  dlgSplash: TdlgSplash;

implementation

uses
  utils_system;

{$R *.lfm}

{ TdlgSplash }

procedure TdlgSplash.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}
end;

procedure TdlgSplash.UpdateProgress(const Msg: String; Percent: Integer);
begin
  lblProgress.Caption := Msg;
  PBar.Position := Percent;
  Application.ProcessMessages;
end;

end.

