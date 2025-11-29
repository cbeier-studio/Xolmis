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
    procedure ApplyDarkMode;
  public
    procedure UpdateProgress(const Msg: String; Percent: Integer);
  end;

var
  dlgSplash: TdlgSplash;

implementation

uses
  utils_system, utils_themes, uDarkStyleParams;

{$R *.lfm}

{ TdlgSplash }

procedure TdlgSplash.ApplyDarkMode;
begin
  lblAppName.Font.Color := clVioletFG1Dark;
end;

procedure TdlgSplash.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;
end;

procedure TdlgSplash.UpdateProgress(const Msg: String; Percent: Integer);
begin
  lblProgress.Caption := Msg;
  PBar.Position := Percent;
  Application.ProcessMessages;
end;

end.

