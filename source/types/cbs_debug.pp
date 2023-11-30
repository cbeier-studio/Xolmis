unit cbs_debug;

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
  cbs_global;

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

