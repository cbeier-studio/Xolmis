unit utils_web;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, RegExpr;

  function GetPageTitle(const AURL: String): String;

implementation

function GetPageTitle(const AURL: String): String;
var
  Client: TFPHTTPClient;
  HTMLContent: String;
  RegEx: TRegExpr;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AllowRedirect := True;
    Client.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; Lazarus)');
    HTMLContent := Client.Get(AURL);

    RegEx := TRegExpr.Create;
    try
      RegEx.Expression := '\<title\>(.*?)\</title\>';
      RegEx.ModifierI := True; // Case insensitive
      if RegEx.Exec(HTMLContent) then
        Result := Trim(RegEx.Match[1]);
    finally
      RegEx.Free;
    end;
  finally
    Client.Free;
  end;
end;

end.

