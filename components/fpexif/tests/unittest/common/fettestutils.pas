unit fetTestUtils;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
 {$ELSE}
  Windows,
 {$ENDIF}
  Classes, SysUtils;


{$IFDEF FPC}
{$ELSE}
function CopyFile(AFilename1, AFileName2: String): Boolean;
{$ENDIF}

implementation

{$IFDEF FPC}
{$ELSE}
function CopyFile(AFileName1, AFileName2: String): Boolean;
begin
  Result := Windows.CopyFile(PChar(AFilename1), PChar(AFilename2), true);
end;
{$ENDIF}

end.

