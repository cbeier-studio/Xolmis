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

unit utils_themes;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}
{$IFDEF DARWIN}
  {$modeswitch objectivec1}
{$ENDIF}

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

  clDefaultBGLight         = TColor($00FFFFFF); // rgb(255, 255, 255)
  clDefaultFG2Light        = TColor($00616161); // rgb(97, 97, 97)
  clDefaultFG1Light        = TColor($00424242); // rgb(66, 66, 66)
  clDefaultFGLight         = TColor($00242424); // rgb(36, 36, 36)
  clDefaultBorderLight     = TColor($00D1D1D1);
  clDefaultBG2Light        = TColor($00F5F5F5);
  clDefaultFGDisabledLight = TColor($00C7C7C7);

  clTextPrimaryLight   = TColor($001B1B1B); // rgb(27, 27, 27)
  clTextSecondaryLight = TColor($00646464); // rgb(100, 100, 100)
  clTextTertiaryLight  = TColor($008D8D8D); // rgb(141, 141, 141)
  clTextDisabledLight  = TColor($00A3A3A3); // rgb(163, 163, 163)

  clAccentTextPrimaryLight   = TColor($00923E00); // rgb(0, 62, 146)
  clAccentTextSecondaryLight = TColor($00681A00); // rgb(0, 26, 104)
  clAccentTextTertiaryLight  = TColor($00B85F00); // rgb(0, 95, 184)
  clAccentTextDisabledLight  = TColor($00A3A3A3); // rgb(163, 163, 163)

  clTextOnAccentPrimaryLight   = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextOnAccentSecondaryLight = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextOnAccentTertiaryLight  = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextOnAccentDisabledLight  = TColor($00FFFFFF); // rgb(255, 255, 255)

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

  // Tag colors
  clTagBlueBGLight = TColor($00FDF2E3);
  clTagBlueFGLight = TColor($00A1470D);
  clTagLeafGreenBGLight = TColor($00E9F5E8);
  clTagLeafGreenFGLight = TColor($00205E1B);
  clTagGoldBGLight = TColor($00E1F8FF);
  clTagGoldFGLight = TColor($00006E8D);
  clTagBurntOrangeBGLight = TColor($00E7E9FB);
  clTagBurntOrangeFGLight = TColor($000C36BF);
  clTagRedBGLight = TColor($00EEEBFF);
  clTagRedFGLight = TColor($001C1CB7);
  clTagVioletBGLight = TColor($00F5E5F3);
  clTagVioletFGLight = TColor($008C144A);
  clTagCyanBGLight = TColor($00FAF7E0);
  clTagCyanFGLight = TColor($00646000);
  clTagWaterGreenBGLight = TColor($00F1F2E0);
  clTagWaterGreenFGLight = TColor($00404D00);
  clTagGreyBGLight = TColor($00F5F5F5);
  clTagGreyFGLight = TColor($00424242);
  clTagTerracotaBGLight = TColor($00E9EBEF);
  clTagTerracotaFGLight = TColor($002E344E);
  clTagNavyBlueBGLight = TColor($00FEF5E1);
  clTagNavyBlueFGLight = TColor($009B5701);
  clTagMossGreenBGLight = TColor($00E9F8F1);
  clTagMossGreenFGLight = TColor($001E6933);
  clTagPinkBGLight = TColor($00ECE4FC);
  clTagPinkFGLight = TColor($004F0E88);
  clTagBrightOrangeBGLight = TColor($00E0F3FF);
  clTagBrightOrangeFGLight = TColor($000051E6);
  clTagTealBGLight = TColor($00F1EFEC);
  clTagTealFGLight = TColor($00383226);

const
  { Dark mode }
  clSolidBGBaseDark       = TColor($00202020); // rgb(32, 32, 32)
  clSolidBGSecondaryDark  = TColor($001C1C1C); // rgb(28, 28, 28)
  clSolidBGTertiaryDark   = TColor($00282828); // rgb(40, 40, 40)
  clSolidBGQuaternaryDark = TColor($002C2C2C); // rgb(44, 44, 44)
  clSmokeBGDefaultDark    = TColor($001F1F1F); // rgb(31, 31, 31)
  clCardBGDefaultDark     = TColor($00373737); // rgb(55, 55, 55)
  clCardBGSecondaryDark   = TColor($00333333); // rgb(51, 51, 51)

  clDefaultFGDark    = TColor($00FFFFFF); // rgb(255, 255, 255)
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
  clSystemMediumFGDark       = TColor($003E9EFF);

  clSystemAttentionBGDark      = TColor($00333333); // rgb(51, 51, 51)
  clSystemSolidAttentionBGDark = TColor($002E2E2E); // rgb(46, 46, 46)
  clSystemSuccessBGDark        = TColor($001B3D39); // rgb(57, 61, 27)
  clSystemCautionBGDark        = TColor($00193543); // rgb(67, 53, 25)
  clSystemCriticalBGDark       = TColor($00262744); // rgb(68, 39, 38)
  clSystemNeutralBGDark        = TColor($00333333); // rgb(51, 51, 51)
  clSystemSolidNeutralBGDark   = TColor($002E2E2E); // rgb(46, 46, 46)
  clSystemMediumBGDark         = TColor($0000376F);

  clVioletFG1Dark    = TColor($00F5857F); // rgb(127, 133, 245)
  clVioletFG2Dark    = TColor($00F79992); // rgb(146, 153, 247)
  clVioletFG3Dark    = TColor($00FAEBE8); // rgb(232, 235, 250)
  clVioletBG2Dark    = TColor($00783E3D); // rgb(61, 62, 120)
  clVioletBG1Dark    = TColor($004A2F2F); // rgb(47, 47, 74)
  clVioletBrand1Dark = TColor($00914744); // rgb(68, 71, 145)
  clVioletBrandBG1Dark = TColor($004A2F2F); // rgb(47, 47, 74)
  clVioletBrandBG4Dark = TColor($00914744); // rgb(68, 71, 145)
  clVioletBrandBG2Dark = TColor($00783E3D); // rgb(61, 62, 120)
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
  clBlueBGDark       = TColor($00482400);

  // from Chart.js Default Palette
  //clBlueChart   = TColor($00EBA236); // rgb(54, 162, 235)
  //clRedChart    = TColor($008463FF); // rgb(255, 99, 132)
  //clGreenChart  = TColor($00C0C04B); // rgb(75, 192, 192)
  //clOrangeChart = TColor($00409FFF); // rgb(255, 159, 64)
  //clPurpleChart = TColor($00FF6699); // rgb(153, 102, 255)
  //clYellowChart = TColor($0056CDFF); // rgb(255, 205, 86)
  //clGrayChart   = TColor($00CFCBC9); // rgb(201, 203, 207)

  // from HomeBank Default Palette
  clBlueChart             = TColor($00B07648); // rgb(72, 118, 176)
  clLightBlueChart        = TColor($00E6C6B4); // rgb(180, 198, 230)
  clOrangeChart           = TColor($00237EE3); // rgb(227, 126, 35)
  clLightOrangeChart      = TColor($007BBAEE); // rgb(238, 186, 123)
  clPineGreenChart        = TColor($003A9E61); // rgb(97, 158, 58)
  clLightPineGreenChart   = TColor($008EDEAF); // rgb(175, 222, 142)
  clRedChart              = TColor($002C2BB8); // rgb(184, 43, 44)
  clLightRedChart         = TColor($009597E7); // rgb(231, 151, 149)
  clVioletChart           = TColor($00B96788); // rgb(136, 103, 185)
  clLightVioletChart      = TColor($00D2AEBE); // rgb(190, 174, 210)
  clBrownChart            = TColor($004D577F); // rgb(127, 87, 77)
  clLightBrownChart       = TColor($00939BB8); // rgb(184, 155, 147)
  clMauveChart            = TColor($00BE76CA); // rgb(202, 118, 190)
  clLightMauveChart       = TColor($00D0B5E6); // rgb(230, 181, 208)
  clGrayChart             = TColor($007E7E7E); // rgb(126, 126, 126)
  clLightGrayChart        = TColor($00C6C6C6); // rgb(198, 198, 198)
  clOliveGreenChart       = TColor($0038BCBB); // rgb(187, 188, 56)
  clLightOliveGreenChart  = TColor($0090DADA); // rgb(218, 218, 144)
  clTurquoiseChart        = TColor($00CDBD6D); // rgb(109, 189, 205)
  clLightTurquoiseChart   = TColor($00E4D9B0); // rgb(176, 217, 228)
  clYellowChart           = TColor($0000D4ED); // rgb(237, 212, 0)
  clLightYellowChart      = TColor($0065EFFF); // rgb(255, 239, 101)
  clCrimsonChart          = TColor($00605DCF); // rgb(207, 93, 96)
  clLightCrimsonChart     = TColor($00BBBAEA); // rgb(234, 186, 187)
  clOchreChart            = TColor($00117CC1); // rgb(193, 124, 17)
  clLightOchreChart       = TColor($005AB5F0); // rgb(240, 181, 90)
  clSilverChart           = TColor($00B6BDBA); // rgb(186, 189, 182)
  clLightSilverChart      = TColor($00DFE3E1); // rgb(225, 227, 223)
  clLimeGreenChart        = TColor($0016D273); // rgb(115, 210, 22)
  clLightLimeGreenChart   = TColor($0070F0AF); // rgb(175, 240, 112)
  clPeachChart            = TColor($005A8CFF); // rgb(255, 140, 90)
  clLightPeachChart       = TColor($00A5BFFF); // rgb(255, 191, 165)

  // Tag colors
  clTagBlueBGDark = TColor($00A1470D);
  clTagBlueFGDark = TColor($00FDF2E3);
  clTagLeafGreenBGDark = TColor($00205E1B);
  clTagLeafGreenFGDark = TColor($00E9F5E8);
  clTagGoldBGDark = TColor($00006E8D);
  clTagGoldFGDark = TColor($00E1F8FF);
  clTagBurntOrangeBGDark = TColor($000C36BF);
  clTagBurntOrangeFGDark = TColor($00E7E9FB);
  clTagRedBGDark = TColor($001C1CB7);
  clTagRedFGDark = TColor($00EEEBFF);
  clTagVioletBGDark = TColor($008C144A);
  clTagVioletFGDark = TColor($00F5E5F3);
  clTagCyanBGDark = TColor($00646000);
  clTagCyanFGDark = TColor($00FAF7E0);
  clTagWaterGreenBGDark = TColor($00404D00);
  clTagWaterGreenFGDark = TColor($00F1F2E0);
  clTagGreyBGDark = TColor($00424242);
  clTagGreyFGDark = TColor($00F5F5F5);
  clTagTerracotaBGDark = TColor($002E344E);
  clTagTerracotaFGDark = TColor($00E9EBEF);
  clTagNavyBlueBGDark = TColor($009B5701);
  clTagNavyBlueFGDark = TColor($00FEF5E1);
  clTagMossGreenBGDark = TColor($001E6933);
  clTagMossGreenFGDark = TColor($00E9F8F1);
  clTagPinkBGDark = TColor($004F0E88);
  clTagPinkFGDark = TColor($00ECE4FC);
  clTagBrightOrangeBGDark = TColor($000051E6);
  clTagBrightOrangeFGDark = TColor($00E0F3FF);
  clTagTealBGDark = TColor($00383226);
  clTagTealFGDark = TColor($00F1EFEC);

const
  { Dark mode - extended default/control colors }
  clDefaultBGDark             = TColor($00242424); // = clDefaultBG1Dark
  clDefaultBG2Dark            = TColor($001F1F1F); // = clDefaultBG6Dark
  clDefaultFG1Dark            = TColor($00E3E3E3); // rgb(227, 227, 227)
  clDefaultFG2Dark            = TColor($00C3C3C3); // rgb(195, 195, 195)
  clDefaultFGDisabledDark     = TColor($00656565); // rgb(101, 101, 101)

  { Dark mode - border colors }
  clDefaultBorderDark         = TColor($00484848); // rgb(72, 72, 72)
  clFocusedBorderLight        = TColor($00D47800); // = clAccentBase
  clFocusedBorderDark         = TColor($00F99300); // = clAccentLight1
  clDisabledBorderLight       = TColor($00E0E0E0); // rgb(224, 224, 224)
  clDisabledBorderDark        = TColor($00383838); // rgb(56, 56, 56)

  { Dark mode - accent fill colors }
  clAccentFillDefaultDark     = TColor($00F99300); // = clAccentLight1
  clAccentFillSecondaryDark   = TColor($00D47800); // = clAccentBase
  clAccentFillTertiaryDark    = TColor($00B75E00); // = clAccentDark1
  clAccentFillDisabledDark    = TColor($00515151); // rgb(81, 81, 81)
  clAccentSelectedTextBGDark  = TColor($00F99300); // = clAccentLight1

type
  { A pair of background + foreground colors, used for tag/badge components }
  TTagColorPair = record
    Background: TColor;
    Foreground: TColor;
  end;

  { Solid/card/smoke window background layers }
  TBackgroundColors = record
    SolidBase: TColor;
    SolidSecondary: TColor;
    SolidTertiary: TColor;
    SolidQuaternary: TColor;
    SmokeDefault: TColor;
    CardDefault: TColor;
    CardSecondary: TColor;
    ControlDefault: TColor;
  end;

  { Text and on-accent foreground colors }
  TForegroundColors = record
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

  { System status colors (attention, success, caution, critical, neutral) }
  TSystemColors = record
    AttentionFG: TColor;
    SuccessFG: TColor;
    CautionFG: TColor;
    CriticalFG: TColor;
    NeutralFG: TColor;
    SolidNeutralFG: TColor;
    MediumFG: TColor;
    AttentionBG: TColor;
    SolidAttentionBG: TColor;
    SuccessBG: TColor;
    CautionBG: TColor;
    CriticalBG: TColor;
    NeutralBG: TColor;
    SolidNeutralBG: TColor;
    MediumBG: TColor;
  end;

  { Accent fill colors for interactive/highlighted elements }
  TAccentFillColors = record
    Default: TColor;
    Secondary: TColor;
    Tertiary: TColor;
    Disabled: TColor;
    SelectedTextBG: TColor;
  end;

  { Full 7-step accent color ramp, from lightest to darkest }
  TAccentPalette = record
    Light3: TColor;
    Light2: TColor;
    Light1: TColor;
    Base: TColor;
    Dark1: TColor;
    Dark2: TColor;
    Dark3: TColor;
  end;

  { Colors for interactive text and selection states }
  TInteractiveColors = record
    WindowTitle: TColor;
    Link: TColor;
    LinkHover: TColor;
    SelectionText: TColor;
    SelectionBackground: TColor;
    SelectionBackgroundStrong: TColor;
    SelectionBorder: TColor;
    { Text drawn on top of an interactive/accent background — contrasts with SelectionBackground }
    AccentText: TColor;
  end;

  { Border/stroke colors for controls }
  TBorderColors = record
    Default: TColor;
    Focused: TColor;
    Disabled: TColor;
  end;

  { Generic control surface colors (backgrounds, foregrounds, disabled states) }
  TControlColors = record
    DefaultBG: TColor;
    DefaultBG2: TColor;
    DefaultFG: TColor;
    DefaultFG1: TColor;
    DefaultFG2: TColor;
    FGDisabled: TColor;
  end;

  { Tag/badge color pairs for all available tag variants }
  TTagColors = record
    Blue: TTagColorPair;
    LeafGreen: TTagColorPair;
    Gold: TTagColorPair;
    BurntOrange: TTagColorPair;
    Red: TTagColorPair;
    Violet: TTagColorPair;
    Cyan: TTagColorPair;
    WaterGreen: TTagColorPair;
    Grey: TTagColorPair;
    Terracota: TTagColorPair;
    NavyBlue: TTagColorPair;
    MossGreen: TTagColorPair;
    Pink: TTagColorPair;
    BrightOrange: TTagColorPair;
    Teal: TTagColorPair;
  end;

  { Complete color theme — all groups that UI controls should reference }
  TColorTheme = record
    Name: String;
    IsDark: Boolean;
    { Window and panel background layers }
    Background: TBackgroundColors;
    { Text and foreground colors }
    Foreground: TForegroundColors;
    { System status colors (attention, success, caution, critical) }
    System: TSystemColors;
    { Accent fill colors for interactive elements }
    AccentFill: TAccentFillColors;
    { 7-step accent color ramp }
    AccentPalette: TAccentPalette;
    { Titles, links and selection colors }
    Interactive: TInteractiveColors;
    { Control border/stroke colors }
    Border: TBorderColors;
    { Generic control surface colors }
    Control: TControlColors;
    { Tag/badge background+foreground pairs }
    Tags: TTagColors;
  end;

var
  ActiveTheme: TColorTheme;


  {$IFDEF DARWIN}
  function MojaveOrNewer: Boolean;
  function GetPrefString(const KeyName: String): String;
  function IsMacDarkMode: Boolean;
  {$ENDIF}
  function IsDarkTheme: Boolean;

  procedure SetupDarkTheme;
  procedure SetupLightTheme;

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
  if (NSProcessInfo.ProcessInfo.isOperatingSystemAtLeastVersion(minOSVer)) then
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

procedure SetupDarkTheme;
begin
  ActiveTheme.Name   := 'Xolmis Dark';
  ActiveTheme.IsDark := True;

  // Background layers
  with ActiveTheme.Background do
  begin
    SolidBase      := clSolidBGBaseDark;
    SolidSecondary := clSolidBGSecondaryDark;
    SolidTertiary  := clSolidBGTertiaryDark;
    SolidQuaternary:= clSolidBGQuaternaryDark;
    SmokeDefault   := clSmokeBGDefaultDark;
    CardDefault    := clCardBGDefaultDark;
    CardSecondary  := clCardBGSecondaryDark;
    ControlDefault := TColor($002A2A2A);
  end;

  // Text / foreground
  with ActiveTheme.Foreground do
  begin
    TextPrimary          := clTextPrimaryDark;
    TextSecondary        := clTextSecondaryDark;
    TextTertiary         := clTextTertiaryDark;
    TextDisabled         := clTextDisabledDark;
    AccentTextPrimary    := clAccentTextPrimaryDark;
    AccentTextSecondary  := clAccentTextSecondaryDark;
    AccentTextTertiary   := clAccentTextTertiaryDark;
    AccentTextDisabled   := clAccentTextDisabledDark;
    TextOnAccentPrimary  := clTextOnAccentPrimaryDark;
    TextOnAccentSecondary:= clTextOnAccentSecondaryDark;
    TextOnAccentTertiary := clTextOnAccentTertiaryDark;
    TextOnAccentDisabled := clTextOnAccentDisabledDark;
  end;

  // System status colors
  with ActiveTheme.System do
  begin
    AttentionFG    := clSystemAttentionFGDark;
    SuccessFG      := clSystemSuccessFGDark;
    CautionFG      := clSystemCautionFGDark;
    CriticalFG     := clSystemCriticalFGDark;
    NeutralFG      := clSystemNeutralFGDark;
    SolidNeutralFG := clSystemSolidNeutralFGDark;
    MediumFG       := clSystemMediumFGDark;
    AttentionBG     := clSystemAttentionBGDark;
    SolidAttentionBG:= clSystemSolidAttentionBGDark;
    SuccessBG       := clSystemSuccessBGDark;
    CautionBG       := clSystemCautionBGDark;
    CriticalBG      := clSystemCriticalBGDark;
    NeutralBG       := clSystemNeutralBGDark;
    SolidNeutralBG  := clSystemSolidNeutralBGDark;
    MediumBG        := clSystemMediumBGDark;
  end;

  // Accent fill
  with ActiveTheme.AccentFill do
  begin
    Default       := clAccentFillDefaultDark;
    Secondary     := clAccentFillSecondaryDark;
    Tertiary      := clAccentFillTertiaryDark;
    Disabled      := clAccentFillDisabledDark;
    SelectedTextBG:= clAccentSelectedTextBGDark;
  end;

  // Accent palette (same ramp for all themes)
  with ActiveTheme.AccentPalette do
  begin
    Light3 := clAccentLight3;
    Light2 := clAccentLight2;
    Light1 := clAccentLight1;
    Base   := clAccentBase;
    Dark1  := clAccentDark1;
    Dark2  := clAccentDark2;
    Dark3  := clAccentDark3;
  end;

  // Interactive colors (titles, links and selection)
  with ActiveTheme.Interactive do
  begin
    WindowTitle               := clVioletFG1Dark;
    Link                      := clVioletFG2Dark;
    LinkHover                 := clVioletFG1Dark;
    SelectionText             := clVioletFG2Dark;
    SelectionBackground       := clVioletBG1Dark; // clVioletBrandBG1Dark;
    SelectionBackgroundStrong := clVioletBrandBG4Dark;
    SelectionBorder           := clVioletBrandBG2Dark;
    AccentText                := clVioletFG3Dark;   // light text on dark violet background
  end;

  // Borders
  with ActiveTheme.Border do
  begin
    Default  := clDefaultBorderDark;
    Focused  := clFocusedBorderDark;
    Disabled := clDisabledBorderDark;
  end;

  // Control surface colors
  with ActiveTheme.Control do
  begin
    DefaultBG  := clDefaultBGDark;
    DefaultBG2 := clDefaultBG2Dark;
    DefaultFG  := clDefaultFGDark;
    DefaultFG1 := clDefaultFG1Dark;
    DefaultFG2 := clDefaultFG2Dark;
    FGDisabled := clDefaultFGDisabledDark;
  end;

  // Tag color pairs
  with ActiveTheme.Tags do
  begin
    Blue.Background        := clTagBlueBGDark;
    Blue.Foreground        := clTagBlueFGDark;
    LeafGreen.Background   := clTagLeafGreenBGDark;
    LeafGreen.Foreground   := clTagLeafGreenFGDark;
    Gold.Background        := clTagGoldBGDark;
    Gold.Foreground        := clTagGoldFGDark;
    BurntOrange.Background := clTagBurntOrangeBGDark;
    BurntOrange.Foreground := clTagBurntOrangeFGDark;
    Red.Background         := clTagRedBGDark;
    Red.Foreground         := clTagRedFGDark;
    Violet.Background      := clTagVioletBGDark;
    Violet.Foreground      := clTagVioletFGDark;
    Cyan.Background        := clTagCyanBGDark;
    Cyan.Foreground        := clTagCyanFGDark;
    WaterGreen.Background  := clTagWaterGreenBGDark;
    WaterGreen.Foreground  := clTagWaterGreenFGDark;
    Grey.Background        := clTagGreyBGDark;
    Grey.Foreground        := clTagGreyFGDark;
    Terracota.Background   := clTagTerracotaBGDark;
    Terracota.Foreground   := clTagTerracotaFGDark;
    NavyBlue.Background    := clTagNavyBlueBGDark;
    NavyBlue.Foreground    := clTagNavyBlueFGDark;
    MossGreen.Background   := clTagMossGreenBGDark;
    MossGreen.Foreground   := clTagMossGreenFGDark;
    Pink.Background        := clTagPinkBGDark;
    Pink.Foreground        := clTagPinkFGDark;
    BrightOrange.Background:= clTagBrightOrangeBGDark;
    BrightOrange.Foreground:= clTagBrightOrangeFGDark;
    Teal.Background        := clTagTealBGDark;
    Teal.Foreground        := clTagTealFGDark;
  end;
end;

procedure SetupLightTheme;
begin
  ActiveTheme.Name   := 'Xolmis Light';
  ActiveTheme.IsDark := False;

  // Background layers
  with ActiveTheme.Background do
  begin
    SolidBase      := clSolidBGBaseLight;
    SolidSecondary := clSolidBGSecondaryLight;
    SolidTertiary  := clSolidBGTertiaryLight;
    SolidQuaternary:= clSolidBGQuaternaryLight;
    SmokeDefault   := clSmokeBGDefaultLight;
    CardDefault    := clCardBGDefaultLight;
    CardSecondary  := clCardBGSecondaryLight;
    ControlDefault := clWindow;
  end;

  // Text / foreground
  with ActiveTheme.Foreground do
  begin
    TextPrimary          := clTextPrimaryLight;
    TextSecondary        := clTextSecondaryLight;
    TextTertiary         := clTextTertiaryLight;
    TextDisabled         := clTextDisabledLight;
    AccentTextPrimary    := clAccentTextPrimaryLight;
    AccentTextSecondary  := clAccentTextSecondaryLight;
    AccentTextTertiary   := clAccentTextTertiaryLight;
    AccentTextDisabled   := clAccentTextDisabledLight;
    TextOnAccentPrimary  := clTextOnAccentPrimaryLight;
    TextOnAccentSecondary:= clTextOnAccentSecondaryLight;
    TextOnAccentTertiary := clTextOnAccentTertiaryLight;
    TextOnAccentDisabled := clTextOnAccentDisabledLight;
  end;

  // System status colors
  with ActiveTheme.System do
  begin
    AttentionFG    := clSystemAttentionFGLight;
    SuccessFG      := clSystemSuccessFGLight;
    CautionFG      := clSystemCautionFGLight;
    CriticalFG     := clSystemCriticalFGLight;
    NeutralFG      := clSystemNeutralFGLight;
    SolidNeutralFG := clSystemSolidNeutralFGLight;
    MediumFG       := clSystemMediumFGLight;
    AttentionBG     := clSystemAttentionBGLight;
    SolidAttentionBG:= clSystemSolidAttentionBGLight;
    SuccessBG       := clSystemSuccessBGLight;
    CautionBG       := clSystemCautionBGLight;
    CriticalBG      := clSystemCriticalBGLight;
    NeutralBG       := clSystemNeutralBGLight;
    SolidNeutralBG  := clSystemSolidNeutralBGLight;
    MediumBG        := clSystemMediumBGLight;
  end;

  // Accent fill
  with ActiveTheme.AccentFill do
  begin
    Default       := clAccentFillDefaultLight;
    Secondary     := clAccentFillSecondaryLight;
    Tertiary      := clAccentFillTertiaryLight;
    Disabled      := clAccentFillDisabledLight;
    SelectedTextBG:= clAccentSelectedTextBGLight;
  end;

  // Accent palette (same ramp for all themes)
  with ActiveTheme.AccentPalette do
  begin
    Light3 := clAccentLight3;
    Light2 := clAccentLight2;
    Light1 := clAccentLight1;
    Base   := clAccentBase;
    Dark1  := clAccentDark1;
    Dark2  := clAccentDark2;
    Dark3  := clAccentDark3;
  end;

  // Interactive colors (titles, links and selection)
  with ActiveTheme.Interactive do
  begin
    WindowTitle               := clVioletFGLight;
    Link                      := clVioletFG2Light;
    LinkHover                 := clVioletBrand1Light;
    SelectionText             := clVioletBrandBG4Light;
    SelectionBackground       := clVioletBG1Light; // clVioletBrandBG1Light;
    SelectionBackgroundStrong := clVioletBrand1Light;
    SelectionBorder           := clVioletBrandBG2Light;
    AccentText                := clVioletBG1Light; // light/contrasting text on dark violet background
  end;

  // Borders
  with ActiveTheme.Border do
  begin
    Default  := clDefaultBorderLight;
    Focused  := clFocusedBorderLight;
    Disabled := clDisabledBorderLight;
  end;

  // Control surface colors
  with ActiveTheme.Control do
  begin
    DefaultBG  := clDefaultBGLight;
    DefaultBG2 := clDefaultBG2Light;
    DefaultFG  := clDefaultFGLight;
    DefaultFG1 := clDefaultFG1Light;
    DefaultFG2 := clDefaultFG2Light;
    FGDisabled := clDefaultFGDisabledLight;
  end;

  // Tag color pairs
  with ActiveTheme.Tags do
  begin
    Blue.Background        := clTagBlueBGLight;
    Blue.Foreground        := clTagBlueFGLight;
    LeafGreen.Background   := clTagLeafGreenBGLight;
    LeafGreen.Foreground   := clTagLeafGreenFGLight;
    Gold.Background        := clTagGoldBGLight;
    Gold.Foreground        := clTagGoldFGLight;
    BurntOrange.Background := clTagBurntOrangeBGLight;
    BurntOrange.Foreground := clTagBurntOrangeFGLight;
    Red.Background         := clTagRedBGLight;
    Red.Foreground         := clTagRedFGLight;
    Violet.Background      := clTagVioletBGLight;
    Violet.Foreground      := clTagVioletFGLight;
    Cyan.Background        := clTagCyanBGLight;
    Cyan.Foreground        := clTagCyanFGLight;
    WaterGreen.Background  := clTagWaterGreenBGLight;
    WaterGreen.Foreground  := clTagWaterGreenFGLight;
    Grey.Background        := clTagGreyBGLight;
    Grey.Foreground        := clTagGreyFGLight;
    Terracota.Background   := clTagTerracotaBGLight;
    Terracota.Foreground   := clTagTerracotaFGLight;
    NavyBlue.Background    := clTagNavyBlueBGLight;
    NavyBlue.Foreground    := clTagNavyBlueFGLight;
    MossGreen.Background   := clTagMossGreenBGLight;
    MossGreen.Foreground   := clTagMossGreenFGLight;
    Pink.Background        := clTagPinkBGLight;
    Pink.Foreground        := clTagPinkFGLight;
    BrightOrange.Background:= clTagBrightOrangeBGLight;
    BrightOrange.Foreground:= clTagBrightOrangeFGLight;
    Teal.Background        := clTagTealBGLight;
    Teal.Foreground        := clTagTealFGLight;
  end;
end;

end.

