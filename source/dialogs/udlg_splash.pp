{ Xolmis Splash screen

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit udlg_splash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls;

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

