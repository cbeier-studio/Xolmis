{ Xolmis Debug library

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

unit utils_debug;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type

  { TElapsedTimer }

  TElapsedTimer = class
  private
    FStart: TDateTime;
    FStartPart, FStopPart: TDateTime;
    FStop: TDateTime;
    FProcess: String;
    FPart: String;
  public
    constructor Create(aProcess: String; aPart: String = '');
    procedure StartTimer;
    procedure AddPart(NewPart: String);
    procedure StopTimer;
  end;

implementation

uses
  utils_global;

{ TElapsedTimer }

constructor TElapsedTimer.Create(aProcess: String; aPart: String);
begin
  FProcess := aProcess;
  FPart := aPart;
  StartTimer;
end;

procedure TElapsedTimer.StartTimer;
begin
  FStart := Now;
  FStartPart := FStart;
end;

procedure TElapsedTimer.AddPart(NewPart: String);
begin
  FStopPart := Now;
  {$IFDEF DEBUG}
  LogDebug(Format('%s, %s: %f ms', [FProcess, FPart, MilliSecondSpan(FStartPart, FStopPart)]));
  {$ENDIF}
  FStartPart := FStopPart;
  FPart := NewPart;
end;

procedure TElapsedTimer.StopTimer;
begin
  FStop := Now;
  FStopPart := FStop;
  {$IFDEF DEBUG}
  if (FPart <> '') and (FStartPart <> FStart) then
    LogDebug(Format('%s, %s: %f ms', [FProcess, FPart, MilliSecondSpan(FStartPart, FStopPart)]));
  LogDebug(Format('%s total elapsed time: %f ms', [FProcess, MilliSecondSpan(FStart, FStop)]));
  {$ENDIF}
end;

end.

