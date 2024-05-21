{ Xolmis Themes library

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

unit cbs_themes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics
  {$IFDEF MSWINDOWS}, Windows, Win32Proc, Registry {$ENDIF}{$IFDEF DARWIN},
  CocoaAll, CocoaUtils, MacOSAll {$ENDIF};

const
  { Light mode }
  clSolidBGBaseLight       = TColor($00F3F3F3); // rgb(243, 243, 243)
  clSolidBGSecondaryLight  = TColor($00EEEEEE); // rgb(238, 238, 238)
  clSolidBGTertiaryLight   = TColor($00F9F9F9); // rgb(249, 249, 249)
  clSolidBGQuaternaryLight = TColor($00FFFFFF); // rgb(255, 255, 255)
  clSmokeBGDefaultLight    = TColor($00B3B3B3); // rgb(179, 179, 179)
  clCardBGDefaultLight     = TColor($00FFFFFF); // rgb(255, 255, 255)
  clCardBGSecondaryLight   = TColor($00FAFAFA); // rgb(250, 250, 250)

  clAccentFillDefaultLight    = TColor($00B85F00); // rgb(0, 95, 184)
  clAccentFillSecondaryLight  = TColor($00C06F1A); // rgb(26, 111, 192)
  clAccentFillTertiaryLight   = TColor($00C67F33); // rgb(51, 127, 198)
  clAccentFillDisabledLight   = TColor($00C8C8C8); // rgb(200, 200, 200)
  clAccentSelectedTextBGLight = TColor($00D47800); // rgb(0, 120, 212)

  clDefaultBGLight         = TColor($00FFFFFF); // RGB(255, 255, 255)
  clDefaultFG2Light        = TColor($00616161); // rgb(97, 97, 97)
  clDefaultFG1Light        = TColor($00424242); // rgb(66, 66, 66)
  clDefaultFGLight         = TColor($00242424); // rgb(36, 36, 36)
  clDefaultBorderLight     = TColor($00D1D1D1);
  clDefaultBG2Light        = TColor($00F5F5F5);
  clDefaultFGDisabledLight = TColor($00C7C7C7);

  clTextPrimaryLight   = TColor($001B1B1B); // RGB(27, 27, 27)
  clTextSecondaryLight = TColor($00646464); // RGB(100, 100, 100)
  clTextTertiaryLight  = TColor($008D8D8D); // RGB(141, 141, 141)
  clTextDisabledLight  = TColor($00A3A3A3); // RGB(163, 163, 163)

  clAccentTextPrimaryLight   = TColor($00923E00); // rgb(0, 62, 146)
  clAccentTextSecondaryLight = TColor($00681A00); // rgb(0, 26, 104)
  clAccentTextTertiaryLight  = TColor($00B85F00); // rgb(0, 95, 184)
  clAccentTextDisabledLight  = TColor($00A3A3A3); // RGB(163, 163, 163)

  clTextOnAccentPrimaryLight   = TColor($00FFFFFF); // RGB(255, 255, 255)
  clTextOnAccentSecondaryLight = TColor($00FFFFFF); // RGB(255, 255, 255)
  clTextOnAccentTertiaryLight  = TColor($00FFFFFF); // RGB(255, 255, 255)
  clTextOnAccentDisabledLight  = TColor($00FFFFFF); // RGB(255, 255, 255)

  clSystemAttentionFGLight    = TColor($00B75F00); // rgb(0, 95, 183)
  clSystemSuccessFGLight      = TColor($000F7B0F); // rgb(15, 123, 15)
  clSystemCautionFGLight      = TColor($00005D9D); // rgb(157, 93, 0)
  clSystemCriticalFGLight     = TColor($001C2BC4); // rgb(196, 43, 28)
  clSystemNeutralFGLight      = TColor($008D8D8D); // rgb(141, 141, 141)
  clSystemSolidNeutralFGLight = TColor($008A8A8A); // rgb(138, 138, 138)
  clSystemMediumFGLight       = TColor($000046BF);

  clSystemAttentionBGLight      = TColor($00FAFAFA); // rgb(250, 250, 250)
  clSystemSolidAttentionBGLight = TColor($00F7F7F7); // rgb(247, 247, 247)
  clSystemSuccessBGLight        = TColor($00DDF6DF); // rgb(223, 246, 221)
  clSystemCautionBGLight        = TColor($00CEF4FF); // rgb(255, 244, 206)
  clSystemCriticalBGLight       = TColor($00E9E7FD); // rgb(253, 231, 233)
  clSystemNeutralBGLight        = TColor($00F9F9F9); // rgb(249, 249, 249)
  clSystemSolidNeutralBGLight   = TColor($00F3F3F3); // rgb(243, 243, 243)
  clSystemMediumBGLight         = TColor($00DBEBFF);

  clVioletFGLight       = TColor($00C75F5B); // rgb(91, 95, 199)
  clVioletFG2Light      = TColor($00B2524F); // rgb(79, 82, 178)
  clVioletBG1Light      = TColor($00FAEBE8); // rgb(232, 235, 250)
  clVioletBrand1Light   = TColor($00F79992); // rgb(146, 153, 247)
  clVioletBrandBG1Light = TColor($00FAEBE8); // rgb(232, 235, 250)
  clVioletBrandBG4Light = TColor($00914744); // rgb(68, 71, 145)
  clVioletBrandBG2Light = TColor($00783E3D); // rgb(61, 62, 120)
  clRedFGLight          = TColor($004B31C4); // rgb(196, 49, 75)
  clRedBGLight          = TColor($004B31C4);
  clRedBG1Light         = TColor($00F6F4FC); // rgb(252, 244, 246)
  clRedBG2Light         = TColor($004B31C4);
  clYellowFG4Light      = TColor($00005C83); // rgb(131, 92, 0)
  clYellowBGLight       = TColor($0044AAFF); // rgb(255, 170, 68)
  clYellowBG1Light      = TColor($00D9F6FB); // rgb(251, 246, 217)
  clYellowBG2Light      = TColor($0000B9FF); // rgb(255, 185, 0)
  clGreenFGLight        = TColor($004B7B23); // rgb(35, 123, 75)
  clGreenFG2Light       = TColor($000EA113); // rgb(19, 161, 14)
  clGreenBGLight        = TColor($0000B76B); // rgb(107, 183, 0)
  clGreenBG2Light       = TColor($00DAF2E7); // rgb(231, 242, 218)
  clBlueBGLight         = TColor($00F6EBDE);

  clAccentLight3 = TColor($00FEEC98); // rgb(152, 236, 254)
  clAccentLight2 = TColor($00FECC60); // rgb(96, 204, 254)
  clAccentLight1 = TColor($00F99300); // rgb(0, 147, 249)
  clAccentBase   = TColor($00D47800); // rgb(0, 120, 212)
  clAccentDark1  = TColor($00B75E00); // rgb(0, 94, 183)
  clAccentDark2  = TColor($00923D00); // rgb(0, 61, 146)
  clAccentDark3  = TColor($00681900); // rgb(0, 25, 104)

const
  { Dark mode }
  clDefaultFGDark    = TColor($00FFFFFF); // RGB(255, 255, 255)
  clDefaultBG1Dark   = TColor($00242424); // rgb(36, 36, 36)
  clDefaultBG6Dark   = TColor($001F1F1F); // rgb(31, 31, 31)
  clDefaultBRDFWDark = TColor($00000000); // rgb(0, 0, 0)

  clTextPrimaryDark   = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextSecondaryDark = TColor($00D1D1D1); // rgb(209, 209, 209)
  clTextTertiaryDark  = TColor($009F9F9F); // rgb(159, 159, 159)
  clTextDisabledDark  = TColor($00797979); // rgb(121, 121, 121)

  clAccentTextPrimaryDark   = TColor($00FFEB99); // rgb(153, 235, 255)
  clAccentTextSecondaryDark = TColor($00FFEB99); // rgb(153, 235, 255)
  clAccentTextTertiaryDark  = TColor($00FFCD60); // rgb(96, 205, 255)
  clAccentTextDisabledDark  = TColor($00797979); // rgb(121, 121, 121)

  clTextOnAccentPrimaryDark   = TColor($00000000); // rgb(0, 0, 0)
  clTextOnAccentSecondaryDark = TColor($00161616); // rgb(22, 22, 22)
  clTextOnAccentTertiaryDark  = TColor($00161616); // rgb(22, 22, 22)
  clTextOnAccentDisabledDark  = TColor($00FFFFFF); // RGB(255, 255, 255)

  clSystemAttentionFGDark    = TColor($00FFCD60); // rgb(96, 205, 255)
  clSystemSuccessFGDark      = TColor($005FCB6C); // rgb(108, 203, 95)
  clSystemCautionFGDark      = TColor($0000E1FC); // rgb(252, 225, 0)
  clSystemCriticalFGDark     = TColor($00A499FF); // rgb(255, 153, 164)
  clSystemNeutralFGDark      = TColor($009F9F9F); // rgb(159, 159, 159)
  clSystemSolidNeutralFGDark = TColor($008A8A8A); // rgb(138, 138, 138)

  clSystemAttentionBGDark      = TColor($00333333); // rgb(51, 51, 51)
  clSystemSolidAttentionBGDark = TColor($002E2E2E); // rgb(46, 46, 46)
  clSystemSuccessBGDark        = TColor($001B3D39); // rgb(57, 61, 27)
  clSystemCautionBGDark        = TColor($00193543); // rgb(67, 53, 25)
  clSystemCriticalBGDark       = TColor($00262744); // rgb(68, 39, 38)
  clSystemNeutralBGDark        = TColor($00333333); // rgb(51, 51, 51)
  clSystemSolidNeutralBGDark   = TColor($002E2E2E); // rgb(46, 46, 46)

  clVioletFG1Dark    = TColor($00F5857F); // rgb(127, 133, 245)
  clVioletBG2Dark    = TColor($00783E3D); // rgb(61, 62, 120)
  clVioletBG1Dark    = TColor($004A2F2F); // rgb(47, 47, 74)
  clVioletBrand1Dark = TColor($00914744); // rgb(68, 71, 145)
  clRedFGDark        = TColor($006B52F9); // rgb(249, 82, 107)
  clRedBG1Dark       = TColor($00251F3E); // rgb(62, 31, 37)
  clRedBG2Dark       = TColor($005035E7); // rgb(231, 53, 80)
  clRedBG3Dark       = TColor($005035E7);
  clYellowFG4Dark    = TColor($0084E3F2); // rgb(242, 227, 132)
  clYellowBGDark     = TColor($002AD2F8); // rgb(248, 210, 42)
  clYellowBG1Dark    = TColor($00003146); // rgb(70, 49, 0)
  clYellowBG2Dark    = TColor($0000B9FF); // rgb(255, 185, 0)
  clGreenFGDark      = TColor($0053C392); // rgb(146, 195, 83)
  clGreenFG2Dark     = TColor($0053C392);
  clGreenBGDark      = TColor($0053C392);
  clGreenBG2Dark     = TColor($000D2E0D); // rgb(13, 46, 13)

type
  TColorTheme = record
    Name: String;
    IsDark: Boolean;
    Background: record
      SolidBase: TColor;
      SolidSecondary: TColor;
      SolidTertiary: TColor;
      SolidQuaternary: TColor;
      SmokeDefault: TColor;
      CardDefault: TColor;
      CardSecondary: TColor;
      SystemAttention: TColor;
      SystemSolidAttention: TColor;
      SystemSuccess: TColor;
      SystemCaution: TColor;
      SystemCritical: TColor;
      SystemNeutral: TColor;
      SystemSolidNeutral: TColor;
    end;
    Foreground: record
      TextPrimary: TColor;
      TextSecondary: TColor;
      TextTertiary: TColor;
      TextDisabled: TColor;
      AccentTextPrimary: TColor;
      AccentTextSecondary: TColor;
      AccentTextTertiary: TColor;
      AccentTextDisabled: TColor;
      TextOnAccentPrimary: TColor;
      TextOnAccentSecondary: TColor;
      TextOnAccentTertiary: TColor;
      TextOnAccentDisabled: TColor;
    end;
  end;

  {$IFDEF DARWIN}
  function MojaveOrNewer: Boolean;
  function GetPrefString(const KeyName: String): String;
  function IsMacDarkMode: Boolean;
  {$ENDIF}
  function IsDarkTheme: Boolean;

implementation

{$IFDEF DARWIN}
// by "trev" from Lazarus forum
function MojaveOrNewer: Boolean;
var
  minOsVer: NSOperatingSystemVersion;
begin
  //Setup minimum version (Mojave)
  minOsVer.majorVersion:= 10;
  minOsVer.minorVersion:= 14;
  minOsVer.patchVersion:= 0;

  // Check minimum version
  if(NSProcessInfo.ProcessInfo.isOperatingSystemAtLeastVersion(minOSVer)) then
    Result := True
  else
    Result := False;
end;

{ The following two functions were suggested by Hansaplast at https://forum.lazarus.freepascal.org/index.php/topic,43111.msg304366.html }

function GetPrefString(const KeyName: String): String;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;

// by "Clover" from Lazarus forum
function IsMacDarkMode: Boolean;
var
  sMode: string;
begin
  // Doesn't work in auto mode
  //sMode := CFStringToStr( CFStringRef( NSUserDefaults.StandardUserDefaults.stringForKey( NSSTR('AppleInterfaceStyle') )));
  sMode  := CFStringToStr( CFStringRef( NSApp.effectiveAppearance.name ));
  Result := Pos('Dark', sMode) > 0;
end;
{$ENDIF}

function IsDarkTheme: Boolean;
{$IFDEF MSWINDOWS}
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
var
  LightKey: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
  Result := False;

  {$IFDEF DARWIN}
  if MojaveOrNewer then
    Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  // by "jwdietrich" from Lazarus forum
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEYPATH) then
      begin
        if Registry.ValueExists(KEYNAME) then
          LightKey := Registry.ReadBool(KEYNAME)
        else
          LightKey := true;
      end
    else
      LightKey := true;
    Result := not LightKey
  finally
    Registry.Free;
  end;
  {$ENDIF}
end;

end.

