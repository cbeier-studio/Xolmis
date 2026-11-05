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

  2026-11-02 - Revised for Xolmis Design System v2 (Jacarandá brand, categorical palette, WCAG-checked status).
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
  {$IFDEF MSWINDOWS}, Win32Proc, Registry {$ENDIF}{$IFDEF DARWIN},
  CocoaAll, CocoaUtils, MacOSAll {$ENDIF};

const
  { ===== Xolmis Design System v2 — new tokens ===== }

  { Categorical palette (10 + Outros). BG/FG are used for tags and grid cells.
    In charts use FG (vivid in light, light tone in dark) as the series colour. }
  clCatJacarandaBGLight      = TColor($00FDF2F5); // #F5F2FD
  clCatJacarandaFGLight      = TColor($009B4248); // #48429B
  clCatJacarandaBGDark       = TColor($0065373E); // #3E3765
  clCatJacarandaFGDark       = TColor($00F2CDD5); // #D5CDF2
  clCatVerdeFolhaBGLight     = TColor($00E3F6F1); // #F1F6E3
  clCatVerdeFolhaFGLight     = TColor($000B7854); // #54780B
  clCatVerdeFolhaBGDark      = TColor($00134333); // #334313
  clCatVerdeFolhaFGDark      = TColor($007EDDBE); // #BEDD7E
  clCatVinhoBGLight          = TColor($00F2F1FC); // #FCF1F2
  clCatVinhoFGLight          = TColor($00350D84); // #840D35
  clCatVinhoBGDark           = TColor($00362B64); // #642B36
  clCatVinhoFGDark           = TColor($00CBC4F9); // #F9C4CB
  clCatTealBGLight           = TColor($00F6F7E8); // #E8F7F6
  clCatTealFGLight           = TColor($007A791F); // #1F797A
  clCatTealBGDark            = TColor($0044441C); // #1C4444
  clCatTealFGDark            = TColor($00E0DF8D); // #8DDFE0
  clCatAmbarTerraBGLight     = TColor($00E9F2FC); // #FCF2E9
  clCatAmbarTerraFGLight     = TColor($00044672); // #724604
  clCatAmbarTerraBGDark      = TColor($001A3852); // #52381A
  clCatAmbarTerraFGDark      = TColor($009CCAF6); // #F6CA9C
  clCatAzulBGLight           = TColor($00FFF4ED); // #EDF4FF
  clCatAzulFGLight           = TColor($00A27136); // #3671A2
  clCatAzulBGDark            = TColor($00584026); // #264058
  clCatAzulFGDark            = TColor($00F7D4B8); // #B8D4F7
  clCatRosaOrquideaBGLight   = TColor($00F4F1FC); // #FCF1F4
  clCatRosaOrquideaFGLight   = TColor($006826BD); // #BD2668
  clCatRosaOrquideaBGDark    = TColor($003E2966); // #66293E
  clCatRosaOrquideaFGDark    = TColor($00D2C4F7); // #F7C4D2
  clCatAmeixaBGLight         = TColor($00F8F0FC); // #FCF0F8
  clCatAmeixaFGLight         = TColor($00621C75); // #751C62
  clCatAmeixaBGDark          = TColor($004F2E5A); // #5A2E4F
  clCatAmeixaFGDark          = TColor($00E9C0FB); // #FBC0E9
  clCatOlivaCinzaBGLight     = TColor($00ECF4F5); // #F5F4EC
  clCatOlivaCinzaFGLight     = TColor($0045676B); // #6B6745
  clCatOlivaCinzaBGDark      = TColor($002D3E40); // #403E2D
  clCatOlivaCinzaFGDark      = TColor($00ACD3D8); // #D8D3AC
  clCatPinhoBGLight          = TColor($00F3F6EB); // #EBF6F3
  clCatPinhoFGLight          = TColor($0048511C); // #1C5148
  clCatPinhoBGDark           = TColor($003D4229); // #29423D
  clCatPinhoFGDark           = TColor($00D1DCA6); // #A6DCD1
  clCatOutrosBGLight         = TColor($00F5F3F1); // #F1F3F5
  clCatOutrosFGLight         = TColor($0063594F); // #4F5963
  clCatOutrosBGDark          = TColor($00403A36); // #363A40
  clCatOutrosFGDark          = TColor($00D4CEC9); // #C9CED4

  { Solid status colours (icons, indicators, solid badges). Container/on-container remain in clSystem*BG/FG. }
  clAttentionSolidLight      = TColor($00BD7702); // #0277BD
  clAttentionSolidDark       = TColor($00F0B85C); // #5CB8F0
  clSuccessSolidLight        = TColor($00327D2E); // #2E7D32
  clSuccessSolidDark         = TColor($0076CF6F); // #6FCF76
  clCautionSolidLight        = TColor($000953B4); // #B45309
  clCautionSolidDark         = TColor($0054B4FF); // #FFB454
  clCriticalSolidLight       = TColor($001E26B3); // #B3261E
  clCriticalSolidDark        = TColor($00808AFF); // #FF8A80
  clOnSolidLight             = TColor($00FFFFFF); // #FFFFFF
  clOnSolidDark              = TColor($00231B1A); // #1A1B23

  { Interactive text and selection }
  clWindowTitleLight         = TColor($009B4248); // #48429B
  clWindowTitleDark          = TColor($00E09BA7); // #A79BE0
  clLinkLight                = TColor($009B4248); // #48429B
  clLinkDark                 = TColor($00E09BA7); // #A79BE0
  clLinkHoverLight           = TColor($0066252A); // #2A2566
  clLinkHoverDark            = TColor($00F2CDD5); // #D5CDF2
  clSelectionTextLight       = TColor($009B4248); // #48429B
  clSelectionTextDark        = TColor($00F2CDD5); // #D5CDF2
  clSelectionBGLight         = TColor($00FAECEC); // #ECECFA
  clSelectionBGDark          = TColor($004A2C2E); // #2E2C4A
  clSelectionBGStrongLight   = TColor($00F2CDD5); // #D5CDF2
  clSelectionBGStrongDark    = TColor($0065373E); // #3E3765
  clSelectionBorderLight     = TColor($00C5666B); // #6B66C5
  clSelectionBorderDark      = TColor($00E09BA7); // #A79BE0
  clSelectionAccentTextLight = TColor($0066252A); // #2A2566
  clSelectionAccentTextDark  = TColor($00FDF2F5); // #F5F2FD

  { Control stroke (>= 3:1 against the surface). clDefaultBorder* stays as the decorative divider. }
  clBorderControlLight       = TColor($00988D8A); // #8A8D98
  clBorderControlDark        = TColor($008E7F7C); // #7C7F8E

  { Sequential (magnitude) and diverging (deviation) data ramps — theme independent }
  clSeq0                     = TColor($00FDF2F5); // #F5F2FD
  clSeq1                     = TColor($00F2CDD5); // #D5CDF2
  clSeq2                     = TColor($00E09BA7); // #A79BE0
  clSeq3                     = TColor($00C5666B); // #6B66C5
  clSeq4                     = TColor($009B4248); // #48429B
  clSeq5                     = TColor($0066252A); // #2A2566
  clDiv0                     = TColor($00A27136); // #3671A2
  clDiv1                     = TColor($00DABC9D); // #9DBCDA
  clDiv2                     = TColor($00EAF0F2); // #F2F0EA
  clDiv3                     = TColor($007EB2E3); // #E3B27E
  clDiv4                     = TColor($000F61A5); // #A5610F

  { Chart series palette v2.3 (8 + Others), in order of use. Do NOT reorder: odd/even series alternate in
    lightness so that neighbours in a stack stay distinguishable. Light = >= 3:1 on light surfaces;
    Dark = >= 4.8:1 on dark cards. Min dE2000 between any two series: 18.4 (light) / 11.3 (dark), with
    colour-vision-deficiency simulation. Names: Jacaranda, Laranja, Pinho, RosaAntigo, Ameixa, Salvia, OlivaEscuro, AzulLago. }
  clChartSeries1Light        = TColor($004F852B); // #2B854F Mandacaru
  clChartSeries1Dark         = TColor($004F852B); // #2B854F Mandacaru
  clChartSeries2Light        = TColor($00C67129); // #2971C6 Genciana
  clChartSeries2Dark         = TColor($00C67129); // #2971C6 Genciana
  clChartSeries3Light        = TColor($000290AC); // #AC9002 Ipê-amarelo
  clChartSeries3Dark         = TColor($000290AC); // #AC9002 Ipê-amarelo
  clChartSeries4Light        = TColor($003E3FC4); // #C43F3E Pitanga
  clChartSeries4Dark         = TColor($003E3FC4); // #C43F3E Pitanga
  clChartSeries5Light        = TColor($00B2A10D); // #0DA1B2 Bromélia
  clChartSeries5Dark         = TColor($00B2A10D); // #0DA1B2 Bromélia
  clChartSeries6Light        = TColor($00B85185); // #8551B8 Quaresmeira
  clChartSeries6Dark         = TColor($00B85185); // #8551B8 Quaresmeira
  clChartSeries7Light        = TColor($00763EBD); // #BD3E76 Hibisco
  clChartSeries7Dark         = TColor($00763EBD); // #BD3E76 Hibisco
  clChartSeries8Light        = TColor($00196FCF); // #CF6F19 Caju
  clChartSeries8Dark         = TColor($00196FCF); // #CF6F19 Caju
  clChartOthersLight         = TColor($00859199); // #999185 Pedra / Outros
  clChartOthersDark          = TColor($00859199); // #999185 Pedra / Outros

  { Print theme (XRS Print Optimized): black on white, categories by hatch pattern + label }
  clPrintInk                 = TColor($00000000); // #000000
  clPrintPaper               = TColor($00FFFFFF); // #FFFFFF
  clPrintGrid                = TColor($008A8A8A); // #8A8A8A

const
  { Light mode }
  clSolidBGBaseLight       = TColor($00FEF9F8); // #F8F9FE
  clSolidBGSecondaryLight  = TColor($00F8F1F0); // #F0F1F8
  clSolidBGTertiaryLight   = TColor($00FFFCFB); // #FBFCFF
  clSolidBGQuaternaryLight = TColor($00FFFFFF); // #FFFFFF
  clSmokeBGDefaultLight    = TColor($00B3B3B3); // rgb(179, 179, 179)
  clCardBGDefaultLight     = TColor($00FFFFFF); // #FFFFFF
  clCardBGSecondaryLight   = TColor($00FEF9F8); // #F8F9FE

  clAccentFillDefaultLight    = TColor($00C5666B); // #6B66C5
  clAccentFillSecondaryLight  = TColor($00B55E62); // #625EB5
  clAccentFillTertiaryLight   = TColor($00A5565A); // #5A56A5
  clAccentFillDisabledLight   = TColor($00EAE5E4); // #E4E5EA
  clAccentSelectedTextBGLight = TColor($00C5666B); // #6B66C5

  clDefaultBGLight         = TColor($00FFFFFF); // #FFFFFF
  clDefaultFG2Light        = TColor($00685F5C); // #5C5F68
  clDefaultFG1Light        = TColor($004F4744); // #44474F
  clDefaultFGLight         = TColor($00201C19); // #191C20
  clDefaultBorderLight     = TColor($00ECE2E0); // #E0E2EC
  clDefaultBG2Light        = TColor($00F8F1F0); // #F0F1F8
  clDefaultFGDisabledLight = TColor($00A9A19E); // #9EA1A9

  clTextPrimaryLight   = TColor($00201C19); // #191C20
  clTextSecondaryLight = TColor($004F4744); // #44474F
  clTextTertiaryLight  = TColor($00685F5C); // #5C5F68
  clTextDisabledLight  = TColor($00A9A19E); // #9EA1A9

  clAccentTextPrimaryLight   = TColor($009B4248); // #48429B
  clAccentTextSecondaryLight = TColor($0066252A); // #2A2566
  clAccentTextTertiaryLight  = TColor($00C5666B); // #6B66C5
  clAccentTextDisabledLight  = TColor($00A9A19E); // #9EA1A9

  clTextOnAccentPrimaryLight   = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextOnAccentSecondaryLight = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextOnAccentTertiaryLight  = TColor($00FFFFFF); // rgb(255, 255, 255)
  clTextOnAccentDisabledLight  = TColor($00FFFFFF); // rgb(255, 255, 255)

  clSystemAttentionFGLight    = TColor($006E4201); // #01426E
  clSystemSuccessFGLight      = TColor($00205E1B); // #1B5E20
  clSystemCautionFGLight      = TColor($00003E7A); // #7A3E00
  clSystemCriticalFGLight     = TColor($000B0E41); // #410E0B
  clSystemNeutralFGLight      = TColor($0063594F); // #4F5963
  clSystemSolidNeutralFGLight = TColor($0063594F); // #4F5963
  clSystemMediumFGLight       = TColor($00003B8F); // #8F3B00

  clSystemAttentionBGLight      = TColor($00FDF2E1); // #E1F2FD
  clSystemSolidAttentionBGLight = TColor($00FDF2E1); // #E1F2FD
  clSystemSuccessBGLight        = TColor($00E9F5E8); // #E8F5E9
  clSystemCautionBGLight        = TColor($00D6F0FF); // #FFF0D6
  clSystemCriticalBGLight       = TColor($00DCDEF9); // #F9DEDC
  clSystemNeutralBGLight        = TColor($00F5F3F1); // #F1F3F5
  clSystemSolidNeutralBGLight   = TColor($00F5F3F1); // #F1F3F5
  clSystemMediumBGLight         = TColor($00D5E6FF); // #FFE6D5

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

  clAccentLight3 = TColor($00F9E0E2); // #E2E0F9
  clAccentLight2 = TColor($00F2CDD5); // #D5CDF2
  clAccentLight1 = TColor($00E09BA7); // #A79BE0
  clAccentBase   = TColor($00C5666B); // #6B66C5
  clAccentDark1  = TColor($00B0545A); // #5A54B0
  clAccentDark2  = TColor($009B4248); // #48429B
  clAccentDark3  = TColor($0066252A); // #2A2566

  // Tag colors
  clTagBlueBGLight = clCatAzulBGLight;
  clTagBlueFGLight = clCatAzulFGLight;
  clTagLeafGreenBGLight = clCatVerdeFolhaBGLight;
  clTagLeafGreenFGLight = clCatVerdeFolhaFGLight;
  clTagGoldBGLight = clCatAmbarTerraBGLight;
  clTagGoldFGLight = clCatAmbarTerraFGLight;
  clTagBurntOrangeBGLight = clCatAmbarTerraBGLight;
  clTagBurntOrangeFGLight = clCatAmbarTerraFGLight;
  clTagRedBGLight = clCatVinhoBGLight;
  clTagRedFGLight = clCatVinhoFGLight;
  clTagVioletBGLight = clCatAmeixaBGLight;
  clTagVioletFGLight = clCatAmeixaFGLight;
  clTagCyanBGLight = clCatTealBGLight;
  clTagCyanFGLight = clCatTealFGLight;
  clTagWaterGreenBGLight = clCatOlivaCinzaBGLight;
  clTagWaterGreenFGLight = clCatOlivaCinzaFGLight;
  clTagGreyBGLight = clCatOutrosBGLight;
  clTagGreyFGLight = clCatOutrosFGLight;
  clTagTerracotaBGLight = clCatAmbarTerraBGLight;
  clTagTerracotaFGLight = clCatAmbarTerraFGLight;
  clTagNavyBlueBGLight = clCatJacarandaBGLight;
  clTagNavyBlueFGLight = clCatJacarandaFGLight;
  clTagMossGreenBGLight = clCatPinhoBGLight;
  clTagMossGreenFGLight = clCatPinhoFGLight;
  clTagPinkBGLight = clCatRosaOrquideaBGLight;
  clTagPinkFGLight = clCatRosaOrquideaFGLight;
  clTagBrightOrangeBGLight = clCatAmbarTerraBGLight;
  clTagBrightOrangeFGLight = clCatAmbarTerraFGLight;
  clTagTealBGLight = clCatTealBGLight;
  clTagTealFGLight = clCatTealFGLight;

const
  { Dark mode }
  clSolidBGBaseDark       = TColor($00231B1A); // #1A1B23
  clSolidBGSecondaryDark  = TColor($001C1615); // #15161C
  clSolidBGTertiaryDark   = TColor($0029201F); // #1F2029
  clSolidBGQuaternaryDark = TColor($002E2423); // #23242E
  clSmokeBGDefaultDark    = TColor($00181312); // #121318
  clCardBGDefaultDark     = TColor($002E2423); // #23242E
  clCardBGSecondaryDark   = TColor($00372B2A); // #2A2B37

  clDefaultFGDark    = TColor($00EEE5E4); // #E4E5EE
  clDefaultBG1Dark   = TColor($002E2423); // #23242E
  clDefaultBG6Dark   = TColor($001C1615); // #15161C
  clDefaultBRDFWDark = TColor($00000000); // rgb(0, 0, 0)

  clTextPrimaryDark   = TColor($00EEE5E4); // #E4E5EE
  clTextSecondaryDark = TColor($00C0B3B0); // #B0B3C0
  clTextTertiaryDark  = TColor($00AB9D9A); // #9A9DAB
  clTextDisabledDark  = TColor($007B6E6B); // #6B6E7B

  clAccentTextPrimaryDark   = TColor($00E09BA7); // #A79BE0
  clAccentTextSecondaryDark = TColor($00F2CDD5); // #D5CDF2
  clAccentTextTertiaryDark  = TColor($00D6848F); // #8F84D6
  clAccentTextDisabledDark  = TColor($007B6E6B); // #6B6E7B

  clTextOnAccentPrimaryDark   = TColor($00231B1A); // #1A1B23
  clTextOnAccentSecondaryDark = TColor($00231B1A); // #1A1B23
  clTextOnAccentTertiaryDark  = TColor($00231B1A); // #1A1B23
  clTextOnAccentDisabledDark  = TColor($00AB9D9A); // #9A9DAB

  clSystemAttentionFGDark    = TColor($00F8DDB5); // #B5DDF8
  clSystemSuccessFGDark      = TColor($00BBE4B7); // #B7E4BB
  clSystemCautionFGDark      = TColor($00A0D9FF); // #FFD9A0
  clSystemCriticalFGDark     = TColor($00C4C9FF); // #FFC9C4
  clSystemNeutralFGDark      = TColor($00D4CEC9); // #C9CED4
  clSystemSolidNeutralFGDark = TColor($00D4CEC9); // #C9CED4
  clSystemMediumFGDark       = TColor($00A0C2FF); // #FFC2A0

  clSystemAttentionBGDark      = TColor($004F3512); // #12354F
  clSystemSolidAttentionBGDark = TColor($004F3512); // #12354F
  clSystemSuccessBGDark        = TColor($00243D1F); // #1F3D24
  clSystemCautionBGDark        = TColor($0010304A); // #4A3010
  clSystemCriticalBGDark       = TColor($001B1F5C); // #5C1F1B
  clSystemNeutralBGDark        = TColor($00403A36); // #363A40
  clSystemSolidNeutralBGDark   = TColor($00403A36); // #363A40
  clSystemMediumBGDark         = TColor($0010224E); // #4E2210

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
  clTagBlueBGDark = clCatAzulBGDark;
  clTagBlueFGDark = clCatAzulFGDark;
  clTagLeafGreenBGDark = clCatVerdeFolhaBGDark;
  clTagLeafGreenFGDark = clCatVerdeFolhaFGDark;
  clTagGoldBGDark = clCatAmbarTerraBGDark;
  clTagGoldFGDark = clCatAmbarTerraFGDark;
  clTagBurntOrangeBGDark = clCatAmbarTerraBGDark;
  clTagBurntOrangeFGDark = clCatAmbarTerraFGDark;
  clTagRedBGDark = clCatVinhoBGDark;
  clTagRedFGDark = clCatVinhoFGDark;
  clTagVioletBGDark = clCatAmeixaBGDark;
  clTagVioletFGDark = clCatAmeixaFGDark;
  clTagCyanBGDark = clCatTealBGDark;
  clTagCyanFGDark = clCatTealFGDark;
  clTagWaterGreenBGDark = clCatOlivaCinzaBGDark;
  clTagWaterGreenFGDark = clCatOlivaCinzaFGDark;
  clTagGreyBGDark = clCatOutrosBGDark;
  clTagGreyFGDark = clCatOutrosFGDark;
  clTagTerracotaBGDark = clCatAmbarTerraBGDark;
  clTagTerracotaFGDark = clCatAmbarTerraFGDark;
  clTagNavyBlueBGDark = clCatJacarandaBGDark;
  clTagNavyBlueFGDark = clCatJacarandaFGDark;
  clTagMossGreenBGDark = clCatPinhoBGDark;
  clTagMossGreenFGDark = clCatPinhoFGDark;
  clTagPinkBGDark = clCatRosaOrquideaBGDark;
  clTagPinkFGDark = clCatRosaOrquideaFGDark;
  clTagBrightOrangeBGDark = clCatAmbarTerraBGDark;
  clTagBrightOrangeFGDark = clCatAmbarTerraFGDark;
  clTagTealBGDark = clCatTealBGDark;
  clTagTealFGDark = clCatTealFGDark;

const
  { Dark mode - extended default/control colors }
  clDefaultBGDark             = TColor($002E2423); // #23242E
  clDefaultBG2Dark            = TColor($001C1615); // #15161C
  clDefaultFG1Dark            = TColor($00C0B3B0); // #B0B3C0
  clDefaultFG2Dark            = TColor($00AB9D9A); // #9A9DAB
  clDefaultFGDisabledDark     = TColor($007B6E6B); // #6B6E7B

  { Dark mode - border colors }
  clDefaultBorderDark         = TColor($004A3C3A); // #3A3C4A
  clFocusedBorderLight        = TColor($009B4248); // #48429B
  clFocusedBorderDark         = TColor($00E09BA7); // #A79BE0
  clDisabledBorderLight       = TColor($00F3EDEC); // #ECEDF3
  clDisabledBorderDark        = TColor($0039302E); // #2E3039

  { Dark mode - accent fill colors }
  clAccentFillDefaultDark     = TColor($00E09BA7); // #A79BE0
  clAccentFillSecondaryDark   = TColor($00E2A3AE); // #AEA3E2
  clAccentFillTertiaryDark    = TColor($00C58893); // #9388C5
  clAccentFillDisabledDark    = TColor($00423634); // #343642
  clAccentSelectedTextBGDark  = TColor($00E09BA7); // #A79BE0

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

  { System status colours. FG/BG = on-container text and container (Design System v2);
    *Solid = solid colour for icons, indicators and solid badges; OnSolid = text drawn over a Solid colour }
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
    AttentionSolid: TColor;
    SuccessSolid: TColor;
    CautionSolid: TColor;
    CriticalSolid: TColor;
    OnSolid: TColor;
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
    Default: TColor;   // decorative divider
    Control: TColor;   // stroke of interactive controls (>= 3:1)
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

  { LEGACY tag variants — now aliases of the categorical palette. New code should use Categorical. }
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

  { Primary and support colors (Xolmis brand) }
  TBrandColors = record
    JacarandaCore: TColor;
    JacarandaDeep: TColor;
    JacarandaLight: TColor;
    JacarandaContainer: TColor;
    RosaOrquideaCore: TColor;
    RosaOrquideaContainer: TColor;
    RosaOrquideaDeep: TColor;
    FolhaCampo: TColor;
  end;

  { Categorical palette (10 + Outros). Background/Foreground are ready to use in tags and
    grid cells for the active theme; Foreground is also the chart series colour.
    Items gives access by index (0..10) for charts and dynamic categories. }
  TCategoricalColors = record
    case Integer of
      0: (Jacaranda, VerdeFolha, Vinho, Teal, AmbarTerra, Azul, RosaOrquidea,
          Ameixa, OlivaCinza, Pinho, Outros: TTagColorPair);
      1: (Items: array[0..10] of TTagColorPair);
  end;

  { Complete color theme — all groups that UI controls should reference }
  TColorTheme = record
    Name: String;
    IsDark: Boolean;
    { Brand Colors }
    Brand: TBrandColors;
    { Categorical palette }
    Categorical: TCategoricalColors;
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
    { Chart series colours in order of use (index 0..7) and the neutral colour for "Others" }
    ChartSeries: array[0..7] of TColor;
    ChartOthers: TColor;
  end;

var
  ActiveTheme: TColorTheme;


  {$IFDEF DARWIN}
  function MojaveOrNewer: Boolean;
  function GetPrefString(const KeyName: String): String;
  function IsMacDarkMode: Boolean;
  {$ENDIF}
  function IsDarkTheme: Boolean;

  { Category colour by index (0..10) for the active theme; indexes wrap over the 10 main colours }
  function CategoryColor(AIndex: Integer): TTagColorPair;
  { Chart series colour (readable on the active theme's surface) }
  function ChartSeriesColor(AIndex: Integer): TColor;
  { Hover/pressed state for a solid fill: darkens in light theme (keeps white text >= 4.5:1), lightens in dark }
  function StateColor(AColor: TColor; APressed: Boolean): TColor;

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

  // Brand colors
  with ActiveTheme.Brand do
  begin
    JacarandaCore         := TColor($00C5666B); // HEX #6B66C5
    JacarandaDeep         := TColor($009B4248); // HEX #48429B
    JacarandaLight        := TColor($00FAECEC); // HEX #ECECFA
    JacarandaContainer    := TColor($00F9E0E2); // HEX #E2E0F9
    RosaOrquideaCore      := TColor($008352C2); // HEX #C25283
    RosaOrquideaContainer := TColor($00E3D7F8); // HEX #F8D7E3
    RosaOrquideaDeep      := TColor($00592A8E); // HEX #8E2A59
    FolhaCampo            := TColor($00527D3A); // HEX #3A7D52
  end;

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
    AttentionSolid  := clAttentionSolidDark;
    SuccessSolid    := clSuccessSolidDark;
    CautionSolid    := clCautionSolidDark;
    CriticalSolid   := clCriticalSolidDark;
    OnSolid         := clOnSolidDark;
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
    WindowTitle               := clWindowTitleDark;
    Link                      := clLinkDark;
    LinkHover                 := clLinkHoverDark;
    SelectionText             := clSelectionTextDark;
    SelectionBackground       := clSelectionBGDark;
    SelectionBackgroundStrong := clSelectionBGStrongDark;
    SelectionBorder           := clSelectionBorderDark;
    AccentText                := clSelectionAccentTextDark; // text drawn over SelectionBackgroundStrong
  end;

  // Borders
  with ActiveTheme.Border do
  begin
    Default  := clDefaultBorderDark;
    Focused  := clFocusedBorderDark;
    Control  := clBorderControlDark;
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

  // Chart series palette
  ActiveTheme.ChartSeries[0] := clChartSeries1Dark;
  ActiveTheme.ChartSeries[1] := clChartSeries2Dark;
  ActiveTheme.ChartSeries[2] := clChartSeries3Dark;
  ActiveTheme.ChartSeries[3] := clChartSeries4Dark;
  ActiveTheme.ChartSeries[4] := clChartSeries5Dark;
  ActiveTheme.ChartSeries[5] := clChartSeries6Dark;
  ActiveTheme.ChartSeries[6] := clChartSeries7Dark;
  ActiveTheme.ChartSeries[7] := clChartSeries8Dark;
  ActiveTheme.ChartOthers := clChartOthersDark;

  // Categorical palette (dark: tonal background + light text)
  with ActiveTheme.Categorical do
  begin
    Jacaranda.Background    := clCatJacarandaBGDark;
    Jacaranda.Foreground    := clCatJacarandaFGDark;
    VerdeFolha.Background   := clCatVerdeFolhaBGDark;
    VerdeFolha.Foreground   := clCatVerdeFolhaFGDark;
    Vinho.Background        := clCatVinhoBGDark;
    Vinho.Foreground        := clCatVinhoFGDark;
    Teal.Background         := clCatTealBGDark;
    Teal.Foreground         := clCatTealFGDark;
    AmbarTerra.Background   := clCatAmbarTerraBGDark;
    AmbarTerra.Foreground   := clCatAmbarTerraFGDark;
    Azul.Background         := clCatAzulBGDark;
    Azul.Foreground         := clCatAzulFGDark;
    RosaOrquidea.Background := clCatRosaOrquideaBGDark;
    RosaOrquidea.Foreground := clCatRosaOrquideaFGDark;
    Ameixa.Background       := clCatAmeixaBGDark;
    Ameixa.Foreground       := clCatAmeixaFGDark;
    OlivaCinza.Background   := clCatOlivaCinzaBGDark;
    OlivaCinza.Foreground   := clCatOlivaCinzaFGDark;
    Pinho.Background        := clCatPinhoBGDark;
    Pinho.Foreground        := clCatPinhoFGDark;
    Outros.Background       := clCatOutrosBGDark;
    Outros.Foreground       := clCatOutrosFGDark;
  end;
end;

procedure SetupLightTheme;
begin
  ActiveTheme.Name   := 'Xolmis Light';
  ActiveTheme.IsDark := False;

  // Brand colors
  with ActiveTheme.Brand do
  begin
    JacarandaCore         := TColor($00C5666B); // HEX #6B66C5
    JacarandaDeep         := TColor($009B4248); // HEX #48429B
    JacarandaLight        := TColor($00FAECEC); // HEX #ECECFA
    JacarandaContainer    := TColor($00F9E0E2); // HEX #E2E0F9
    RosaOrquideaCore      := TColor($008352C2); // HEX #C25283
    RosaOrquideaContainer := TColor($00E3D7F8); // HEX #F8D7E3
    RosaOrquideaDeep      := TColor($00592A8E); // HEX #8E2A59
    FolhaCampo            := TColor($00527D3A); // HEX #3A7D52
  end;

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
    AttentionSolid  := clAttentionSolidLight;
    SuccessSolid    := clSuccessSolidLight;
    CautionSolid    := clCautionSolidLight;
    CriticalSolid   := clCriticalSolidLight;
    OnSolid         := clOnSolidLight;
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
    WindowTitle               := clWindowTitleLight;
    Link                      := clLinkLight;
    LinkHover                 := clLinkHoverLight;
    SelectionText             := clSelectionTextLight;
    SelectionBackground       := clSelectionBGLight;
    SelectionBackgroundStrong := clSelectionBGStrongLight;
    SelectionBorder           := clSelectionBorderLight;
    AccentText                := clSelectionAccentTextLight; // text drawn over SelectionBackgroundStrong
  end;

  // Borders
  with ActiveTheme.Border do
  begin
    Default  := clDefaultBorderLight;
    Focused  := clFocusedBorderLight;
    Control  := clBorderControlLight;
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

  // Chart series palette
  ActiveTheme.ChartSeries[0] := clChartSeries1Light;
  ActiveTheme.ChartSeries[1] := clChartSeries2Light;
  ActiveTheme.ChartSeries[2] := clChartSeries3Light;
  ActiveTheme.ChartSeries[3] := clChartSeries4Light;
  ActiveTheme.ChartSeries[4] := clChartSeries5Light;
  ActiveTheme.ChartSeries[5] := clChartSeries6Light;
  ActiveTheme.ChartSeries[6] := clChartSeries7Light;
  ActiveTheme.ChartSeries[7] := clChartSeries8Light;
  ActiveTheme.ChartOthers := clChartOthersLight;

  // Categorical palette (light: pastel background + vivid text)
  with ActiveTheme.Categorical do
  begin
    Jacaranda.Background    := clCatJacarandaBGLight;
    Jacaranda.Foreground    := clCatJacarandaFGLight;
    VerdeFolha.Background   := clCatVerdeFolhaBGLight;
    VerdeFolha.Foreground   := clCatVerdeFolhaFGLight;
    Vinho.Background        := clCatVinhoBGLight;
    Vinho.Foreground        := clCatVinhoFGLight;
    Teal.Background         := clCatTealBGLight;
    Teal.Foreground         := clCatTealFGLight;
    AmbarTerra.Background   := clCatAmbarTerraBGLight;
    AmbarTerra.Foreground   := clCatAmbarTerraFGLight;
    Azul.Background         := clCatAzulBGLight;
    Azul.Foreground         := clCatAzulFGLight;
    RosaOrquidea.Background := clCatRosaOrquideaBGLight;
    RosaOrquidea.Foreground := clCatRosaOrquideaFGLight;
    Ameixa.Background       := clCatAmeixaBGLight;
    Ameixa.Foreground       := clCatAmeixaFGLight;
    OlivaCinza.Background   := clCatOlivaCinzaBGLight;
    OlivaCinza.Foreground   := clCatOlivaCinzaFGLight;
    Pinho.Background        := clCatPinhoBGLight;
    Pinho.Foreground        := clCatPinhoFGLight;
    Outros.Background       := clCatOutrosBGLight;
    Outros.Foreground       := clCatOutrosFGLight;
  end;
end;

function CategoryColor(AIndex: Integer): TTagColorPair;
begin
  if AIndex < 0 then
    Result := ActiveTheme.Categorical.Outros
  else
    Result := ActiveTheme.Categorical.Items[AIndex mod 10];
end;

function ChartSeriesColor(AIndex: Integer): TColor;
begin
  // Series 0..7 follow the chart palette; negative index (or "Others") uses the neutral grey
  if AIndex < 0 then
    Result := ActiveTheme.ChartOthers
  else
    Result := ActiveTheme.ChartSeries[AIndex mod 8];
end;

function BlendColors(AFrom, ATo: TColor; APercent: Integer): TColor;
var
  f, t: LongInt;
  function Mix(a, b: Byte): Byte;
  begin
    Result := a + ((Integer(b) - Integer(a)) * APercent) div 100;
  end;
begin
  f := ColorToRGB(AFrom);
  t := ColorToRGB(ATo);
  Result := RGBToColor(Mix(Red(f), Red(t)), Mix(Green(f), Green(t)), Mix(Blue(f), Blue(t)));
end;

function StateColor(AColor: TColor; APressed: Boolean): TColor;
begin
  if ActiveTheme.IsDark then
  begin
    if APressed then
      Result := BlendColors(AColor, clBlack, 12)
    else
      Result := BlendColors(AColor, clWhite, 8);
  end
  else
  begin
    if APressed then
      Result := BlendColors(AColor, clBlack, 16)
    else
      Result := BlendColors(AColor, clBlack, 8);
  end;
end;

end.

