{ Xolmis Conversions library

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

unit utils_conversions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, RegExpr, DateUtils, models_record_types;

  function WildcardWords(aText: String; aWildcard: String = '%'): String;
  function WildcardSyllables(aText: String; aWildcard: String = '%'): String;

  function TrimList(aList: TStrings): String; overload;
  function TrimList(aList: String): String; overload;

  function NormalizeWhitespace(const aText: String; RemoveLineBreaks: Boolean): String;

  function RemoveDiacritics(const aText: String): String;

  function SentenceCase(aText: String): String;

  // Boolean treatment
  function TextToBool(aValue, aTrue, aFalse: String): Boolean;
  function BoolToText(const aValue: Boolean; aTrue, aFalse: String): String;

  // Date and time treatment
  function TextToDate(aValue: String): TDate;
  function TextToTime(aValue: String): TTime;
  function DartISO8601ToDate(aValue: String): TDateTime;

  // Numeric treatment
  function StrToIntOrZero(aValue: String): Integer;
  function StrToFloatOrZero(aValue: String): Double;

  // Convert to specific enum types
  function StrToAccessionType(const AValue: String): String;
  function StrToActivityStatus(const AValue: String): TActivityStatus;
  function StrToAge(const AValue: String): TAge;
  function StrToAudioType(const AValue: String): TAudioType;
  function StrToBandType(const AValue: String): TMarkType;
  function StrToBandStatus(const AValue: String): TBandStatus;
  function StrToBandSource(const AValue: String): TBandSource;
  function StrToBodySide(const AValue: String): TBodySide;
  function StrToCaptureType(const AValue: String): TCaptureType;
  function StrToCoordinatePrecision(const AValue: String): TCoordinatePrecision;
  function StrToDocumentType(const AValue: String): TFileCategory;
  function StrToEggPattern(const AValue: String): TEggshellPattern;
  function StrToEggShape(const AValue: String): TEggShape;
  function StrToEggTexture(const AValue: String): TEggshellTexture;
  function StrToFeatherAge(const AValue: String): TFeatherAge;
  function StrToFeatherSource(const AValue: String): TFeatherDataSource;
  function StrToFeatherTrait(const AValue: String): TFeatherTrait;
  function StrToGoalStatus(const AValue: String): TGoalStatus;
  function StrToImageType(const AValue: String): TImageType;
  function StrToLossCause(const AValue: String): TLossCause;
  function StrToNestFate(const AValue: String): TNestFate;
  function StrToNestRole(const AValue: String): TNestRole;
  function StrToNestShape(const AValue: String): String;
  function StrToNestStage(const AValue: String): TNestStage;
  function StrToNestStatus(const AValue: String): TNestStatus;
  function StrToPermitType(const AValue: String): String;
  function StrToPrecipitation(const AValue: String): TPrecipitation;
  function StrToSampleMoment(const AValue: String): TWeatherSampleMoment;
  function StrToSex(const AValue: String): TSex;
  function StrToSiteRank(const AValue: String): TSiteRank;
  function StrToSpecimenType(const AValue: String): TSpecimenType;
  function StrToStratumDistribution(const AValue: String): TStratumDistribution;
  function StrToSubjectStatus(const AValue: String): TSubjectStatus;
  function StrToSupportType(const AValue: String): String;
  function StrToSymmetry(const AValue: String): TSymmetry;
  function StrToUserRank(const AValue: String): TUserRank;
  function StrToVideoType(const AValue: String): TVideoType;

implementation

uses utils_global, utils_locale;

function WildcardWords(aText: String; aWildcard: String): String;
var
  i, total: Integer;
  Words: TStringList;
begin
  Result := EmptyStr;

  Words := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  try
    total := ExtractStrings([' '], [' '], PAnsiChar(aText), Words);
    if total > 1 then
      for i := 0 to Words.Count - 1 do
        if ExecRegExpr('^#[a-z]+$', Words[i]) then
          Result := Result + ''
        else
          if i = 0 then
            Result := AnsiDequotedStr(Words[i], '"') + aWildcard + ' '
          else
            if i = Words.Count - 1 then
              Result := Result + aWildcard + AnsiDequotedStr(Words[i], '"')
            else
              Result := Result + aWildcard + AnsiDequotedStr(Words[i], '"') + aWildcard + ' ';
    if total = 1 then
      if ExecRegExpr('^#[a-z]+$', aText) then
        Result := ''
      else
        Result := AnsiDequotedStr(aText, '"');
  finally
    FreeAndNil(Words);
  end;
end;

function WildcardSyllables(aText: String; aWildcard: String): String;
var
  i, total: Integer;
  Syllables: TStringList;
begin
  Result := EmptyStr;

  Syllables := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  try
    total := ExtractStrings([' ', '+'], [' '], PAnsiChar(aText), Syllables);
    if total > 1 then
      for i := 0 to Syllables.Count - 1 do
        if ExecRegExpr('^#[a-z]+$', Syllables[i]) then
          Result := Result + ''
        else
          if i = 0 then
            Result := AnsiDequotedStr(Syllables[i], '"') + aWildcard
          else
            if i = Syllables.Count - 1 then
              Result := Result + AnsiDequotedStr(Syllables[i], '"')
            else
              Result := Result + AnsiDequotedStr(Syllables[i], '"') + aWildcard;
    if total = 1 then
      if ExecRegExpr('^#[a-z]+$', aText) then
        Result := ''
      else
        Result := AnsiDequotedStr(aText, '"');
  finally
    FreeAndNil(Syllables);
  end;
end;

function TrimList(aList: TStrings): String;
var
  i: Integer;
  S: String;
begin
  Result := EmptyStr;
  S := EmptyStr;
  if aList.Count = 0 then
    Exit;

  S := aList[0];
  if aList.Count > 1 then
    for i := 1 to aList.Count - 1 do
    begin
      S := Format('%s %s', [S, Trim(aList[i])]);
      Application.ProcessMessages;
    end;

  Result := S;
end;

function TrimList(aList: String): String;
var
  S: String;
begin
  S := aList;
  S := StringReplace(S, LineEnding, ' ', [rfReplaceAll]);

  Result := S;
end;

function NormalizeWhitespace(const aText: String; RemoveLineBreaks: Boolean): String;
var
  i: Integer;
  ch: Char;
  InSpace: Boolean;
begin
  Result := EmptyStr;
  InSpace := False;

  for i := 1 to Length(aText) do
  begin
    ch := aText[i];

    // Case 1: "light" whitespace (space or tab)
    if ch in [' ', #9] then
    begin
      if not InSpace then
      begin
        Result := Result + ' ';
        InSpace := True;
      end;
      Continue;
    end;

    // Case 2: line breaks
    if ch in [#10, #13] then
    begin
      if RemoveLineBreaks then
      begin
        // Treat as regular space
        if not InSpace then
        begin
          Result := Result + ' ';
          InSpace := True;
        end;
      end
      else
      begin
        // Keep line break
        Result := Result + ch;
        InSpace := False;
      end;
      Continue;
    end;

    // Case 3: regular character
    Result := Result + ch;
    InSpace := False;
  end;

  // Remove spaces from start and from end
  //Result := Trim(Result);
end;

function RemoveDiacritics(const aText: String): String;
const
  AccentedChars: array[1..34] of string = (
    'á', 'à', 'â', 'ã', 'ä', 'Á', 'À', 'Â', 'Ã', 'Ä',
    'é', 'ê', 'ë', 'É', 'Ê', 'Ë',
    'í', 'Í',
    'ó', 'ô', 'õ', 'ö', 'Ó', 'Ô', 'Õ', 'Ö',
    'ú', 'ü', 'Ú', 'Ü',
    'ç', 'Ç',
    'ñ', 'Ñ');
  PlainChars: array[1..34] of string = (
    'a', 'a', 'a', 'a', 'a', 'A', 'A', 'A', 'A', 'A',
    'e', 'e', 'e', 'E', 'E', 'E',
    'i', 'I',
    'o', 'o', 'o', 'o', 'O', 'O', 'O', 'O',
    'u', 'u', 'U', 'U',
    'c', 'C',
    'n', 'N');
var
  I: Integer;
begin
  Result := aText;
  for I := Low(AccentedChars) to High(AccentedChars) do
    Result := StringReplace(Result, AccentedChars[I], PlainChars[I], [rfReplaceAll, rfIgnoreCase]);
end;

function SentenceCase(aText: String): String;
begin
  aText := AnsiLowerCase(aText);

  if aText = EmptyStr then
    Exit('');

  aText[1] := UpCase(aText[1]);

  Result := aText;
end;

{ --------------------------------------------------------- }
{ Boolean treatment }
{ --------------------------------------------------------- }

function TextToBool(aValue, aTrue, aFalse: String): Boolean;
begin
  Result := False;

  if AnsiUpperCase(aValue) = AnsiUpperCase(aTrue) then
    Result := True
  else
    if AnsiUpperCase(aValue) = AnsiUpperCase(aFalse) then
      Result := False;
end;

function BoolToText(const aValue: Boolean; aTrue, aFalse: String): String;
begin
  Result:= '';

  if aValue = True then
    Result := aTrue
  else
    Result := aFalse;
end;

{ --------------------------------------------------------- }
{ Date and time treatment }
{ --------------------------------------------------------- }

function TextToDate(aValue: String): TDate;
var
  Dt: TDate;
begin
  if TryStrToDate(aValue, Dt) then
    Result := Dt
  else
    Result := NullDate;
end;

function TextToTime(aValue: String): TTime;
var
  Tm: TTime;
begin
  if TryStrToTime(aValue, Tm) then
    Result := Tm
  else
    Result := NullTime;
end;

function DartISO8601ToDate(aValue: String): TDateTime;
var
  Dt: TDateTime;
begin
  Result := NullDateTime;
  if Pos('.', aValue) > 0 then
    if TryISO8601ToDate(Copy(aValue, 1, Pos('.', aValue) - 1), Dt, True) then
      Result := Dt;
end;

{ --------------------------------------------------------- }
{ Numeric treatment }
{ --------------------------------------------------------- }

function StrToIntOrZero(aValue: String): Integer;
var
  I: Integer;
begin
  if TryStrToInt(aValue, I) then
    Result := I
  else
    Result := 0;
end;

function StrToFloatOrZero(avalue: String): Double;
var
  Fl: Double;
begin
  if TryStrToFloat(aValue, Fl) then
    Result := Fl
  else
    Result := 0.0;
end;

{ --------------------------------------------------------- }
{ Conversion to specific enum types }
{ --------------------------------------------------------- }

function StrToAccessionType(const AValue: String): String;
begin
  if (AValue = rsSampleSkinStandard) then
    Result := 'NS'
  else
  if (AValue = rsSampleSkinShmoo) then
    Result := 'SS'
  else
  if (AValue = rsSampleSkinMounted) then
    Result := 'MS'
  else
  if (AValue = rsSampleOpenedWing) then
    Result := 'OW'
  else
  if (AValue = rsSampleSkeletonWhole) then
    Result := 'WS'
  else
  if (AValue = rsSampleSkeletonPartial) then
    Result := 'PS'
  else
  if (AValue = rsSampleNest) then
    Result := 'N'
  else
  if (AValue = rsSampleEgg) then
    Result := 'EGG'
  else
  if (AValue = rsSampleParasites) then
    Result := 'P'
  else
  if (AValue = rsSampleFeathers) then
    Result := 'F'
  else
  if (AValue = rsSampleBloodDry) then
    Result := 'BD'
  else
  if (AValue = rsSampleBloodWet) then
    Result := 'BL'
  else
  if (AValue = rsSampleBloodSmear) then
    Result := 'BS'
  else
  if (AValue = rsSampleSexing) then
    Result := 'SX'
  else
  if (AValue = rsSampleGeneticSequence) then
    Result := 'GS'
  else
  if (AValue = rsSampleMicrobialCulture) then
    Result := 'MC'
  else
  if (AValue = rsSampleTissues) then
    Result := 'TS'
  else
  if (AValue = rsSampleEyes) then
    Result := 'EYE'
  else
  if (AValue = rsSampleTongue) then
    Result := 'T'
  else
  if (AValue = rsSampleSyrinx) then
    Result := 'S'
  else
  if (AValue = rsSampleGonads) then
    Result := 'G'
  else
  if (AValue = rsSampleStomach) then
    Result := 'M';
end;

function StrToActivityStatus(const AValue: String): TActivityStatus;
begin
  if (AValue = rsActivityToDo) or (AValue = 'T') then
    Result := astToDo
  else
  if (AValue = rsActivityInProgress) or (AValue = 'P') then
    Result := astInProgress
  else
  if (AValue = rsActivityNeedsReview) or (AValue = 'R') then
    Result := astNeedsReview
  else
  if (AValue = rsActivityBlocked) or (AValue = 'B') then
    Result := astBlocked
  else
  if (AValue = rsActivityDelayed) or (AValue = 'D') then
    Result := astDelayed
  else
  if (AValue = rsActivityCanceled) or (AValue = 'C') then
    Result := astCanceled
  else
  if (AValue = rsActivityDone) or (AValue = 'F') then
    Result := astDone;
end;

function StrToAge(const AValue: String): TAge;
begin
  if (AValue = rsAgeNestling) or (AValue = 'N') then
    Result := ageNestling
  else
  if (AValue = rsAgeFledgling) or (AValue = 'F') then
    Result := ageFledgling
  else
  if (AValue = rsAgeJuvenile) or (AValue = 'J') then
    Result := ageJuvenile
  else
  if (AValue = rsAgeAdult) or (AValue = 'A') then
    Result := ageAdult
  else
  if (AValue = rsAgeFirstYear) or (AValue = 'Y') then
    Result := ageFirstYear
  else
  if (AValue = rsAgeSecondYear) or (AValue = 'S') then
    Result := ageSecondYear
  else
  if (AValue = rsAgeThirdYear) or (AValue = 'T') then
    Result := ageThirdYear
  else
  if (AValue = rsAgeFourthYear) or (AValue = '4') then
    Result := ageFourthYear
  else
  if (AValue = rsAgeFifthYear) or (AValue = '5') then
    Result := ageFifthYear
  else
    Result := ageUnknown;
end;

function StrToAudioType(const AValue: String): TAudioType;
begin
  if (AValue = rsAudioSong) or (AValue = 'SON') then
    Result := atSong
  else
  if (AValue = rsAudioCall) or (AValue = 'CAL') then
    Result := atCall
  else
  if (AValue = rsAudioAlarm) or (AValue = 'ALM') then
    Result := atAlarm
  else
  if (AValue = rsAudioTerritorial) or (AValue = 'TER') then
    Result := atTerritorial
  else
  if (AValue = rsAudioCourtship) or (AValue = 'CRT') then
    Result := atCourtship
  else
  if (AValue = rsAudioAggression) or (AValue = 'AGR') then
    Result := atAggression
  else
  if (AValue = rsAudioContact) or (AValue = 'CNT') then
    Result := atContact
  else
  if (AValue = rsAudioFlock) or (AValue = 'FLK') then
    Result := atFlock
  else
  if (AValue = rsAudioFlight) or (AValue = 'FLT') then
    Result := atFlight
  else
  if (AValue = rsAUdioNestling) or (AValue = 'NST') then
    Result := atNestling
  else
  if (AValue = rsAudioNonVocal) or (AValue = 'NVL') then
    Result := atNonVocal
  else
    Result := atUnknown;
end;

function StrToBandType(const AValue: String): TMarkType;
begin
  if (AValue = rsBandOpen) or (AValue = 'A') then
    Result := mkButtEndBand
  else
  if (AValue = rsBandFlag) or (AValue = 'F') then
    Result := mkFlag
  else
  if (AValue = rsBandNeck) or (AValue = 'N') then
    Result := mkCollar
  else
  if (AValue = rsBandWingTag) or (AValue = 'W') then
    Result := mkWingTag
  else
  if (AValue = rsBandTriangular) or (AValue = 'T') then
    Result := mkTriangularBand
  else
  if (AValue = rsBandLockOn) or (AValue = 'L') then
    Result := mkLockOnBand
  else
  if (AValue = rsBandRivet) or (AValue = 'R') then
    Result := mkRivetBand
  else
  if (AValue = rsBandClosed) or (AValue = 'C') then
    Result := mkClosedBand
  else
    Result := mkOther;
end;

function StrToBandStatus(const AValue: String): TBandStatus;
begin
  if (AValue = rsBandOrdered) or (AValue = 'O') then
    Result := bstOrdered
  else
  if (AValue = rsBandAvailable) or (AValue = 'A') then
    Result := bstAvailable
  else
  if (AValue = rsBandUsed) or (AValue = 'U') then
    Result := bstUsed
  else
  if (AValue = rsBandRemoved) or (AValue = 'R') then
    Result := bstRemoved
  else
  if (AValue = rsBandBroken) or (AValue = 'B') then
    Result := bstBroken
  else
  if (AValue = rsBandLost) or (AValue = 'L') then
    Result := bstLost
  else
  if (AValue = rsBandReturned) or (AValue = 'X') then
    Result := bstReturned
  else
  if (AValue = rsBandTransferred) or (AValue = 'T') then
    Result := bstTransferred
  else
    Result := bstNone;
end;

function StrToBandSource(const AValue: String): TBandSource;
begin
  if (AValue = rsBandAcquiredFromSupplier) or (AValue = 'A') then
    Result := bscAcquiredFromSupplier
  else
  if (AValue = rsBandTransferBetweenBanders) or (AValue = 'T') then
    Result := bscTransferBetweenBanders
  else
  if (AValue = rsBandLivingBirdBandedByOthers) or (AValue = 'L') then
    Result := bscLivingBirdBandedByOthers
  else
  if (AValue = rsBandDeadBirdBandedByOthers) or (AValue = 'D') then
    Result := bscDeadBirdBandedByOthers
  else
  if (AValue = rsBandFoundLoose) or (AValue = 'F') then
    Result := bscFoundLoose
  else
    Result := bscNone;
end;

function StrToBodySide(const AValue: String): TBodySide;
begin
  if (AValue = rsSideRight) or (AValue = 'R') then
    Result := bsdRight
  else
  if (AValue = rsSideLeft) or (AValue = 'L') then
    Result := bsdLeft
  else
    Result := bsdNotApplicable;
end;

function StrToCaptureType(const AValue: String): TCaptureType;
begin
  if (AValue = rsCaptureNew) or (AValue = 'N') then
    Result := cptNew
  else
  if (AValue = rsCaptureRecapture) or (AValue = 'R') then
    Result := cptRecapture
  else
  if (AValue = rsCaptureSameDay) or (AValue = 'S') then
    Result := cptSameDay
  else
  if (AValue = rsCaptureChangeBand) or (AValue = 'C') then
    Result := cptChangeBand
  else
    Result := cptUnbanded;
end;

function StrToCoordinatePrecision(const AValue: String): TCoordinatePrecision;
begin
  if (AValue = rsExactCoordinate) or (AValue = 'E') then
    Result := cpExact
  else
  if (AValue = rsApproximatedCoordinate) or (AValue = 'A') then
    Result := cpApproximated
  else
  if (AValue = rsReferenceCoordinate) or (AValue = 'R') then
    Result := cpReference
  else
    Result := cpEmpty;
end;

function StrToDocumentType(const AValue: String): TFileCategory;
begin
  if (AValue = rsDocUrl) or (AValue = 'url') then
    Result := fcUrl
  else
  if (AValue = rsDocDocument) or (AValue = 'doc') then
    Result := fcText
  else
  if (AValue = rsDocSpreadsheet) or (AValue = 'spr') then
    Result := fcSpreadsheet
  else
  if (AValue = rsDocPresentation) or (AValue = 'prs') then
    Result := fcPresentation
  else
  if (AValue = rsDocPdf) or (AValue = 'pdf') then
    Result := fcPdf
  else
  if (AValue = rsDocImage) or (AValue = 'img') then
    Result := fcImage
  else
  if (AValue = rsDocAudio) or (AValue = 'aud') then
    Result := fcAudio
  else
  if (AValue = rsDocVideo) or (AValue = 'vid') then
    Result := fcVideo
  else
  if (AValue = rsDocCode) or (AValue = 'cod') then
    Result := fcSourceCode
  else
  if (AValue = rsDocDatabase) or (AValue = 'db') then
    Result := fcDatabase
  else
  if (AValue = rsDocGis) or (AValue = 'gis') then
    Result := fcGis
  else
  if (AValue = rsDocScript) or (AValue = 'scr') then
    Result := fcScript
  else
  if (AValue = rsDocWebpage) or (AValue = 'web') then
    Result := fcWebpage
  else
  if (AValue = rsDocDataset) or (AValue = 'ds') then
    Result := fcDataset
  else
  if (AValue = rsDocStatistic) or (AValue = 'sta') then
    Result := fcStatistic
  else
  if (AValue = rsDocVectorial) or (AValue = 'vec') then
    Result := fcVectorial
  else
  if (AValue = rsDocArchive) or (AValue = 'arc') then
    Result := fcArchive
  else
  if (AValue = rsDocBibliography) or (AValue = 'bib') then
    Result := fcBibliography
  else
  if (AValue = rsDocMetadata) or (AValue = 'met') then
    Result := fcMetadata
  else
  if (AValue = rsDocBioinformatic) or (AValue = 'gen') then
    Result := fcBioinformatic
  else
  if (AValue = rsDocEbook) or (AValue = 'ebk') then
    Result := fcEbook
  else
  if (AValue = rsDocNote) or (AValue = 'not') then
    Result := fcNote
  else
    Result := fcOther;
end;

function StrToEggPattern(const AValue: String): TEggshellPattern;
begin
  if (AValue = rsEggSpots) or (AValue = 'P') then
    Result := espSpots
  else
  if (AValue = rsEggBlotches) or (AValue = 'B') then
    Result := espBlotches
  else
  if (AValue = rsEggScrawls) or (AValue = 'W') then
    Result := espScrawls
  else
  if (AValue = rsEggSquiggles) or (AValue = 'S') then
    Result := espSquiggles
  else
  if (AValue = rsEggStreaks) or (AValue = 'T') then
    Result := espStreaks
  else
  if (AValue = rsEggBlotchesSquiggles) or (AValue = 'BS') then
    Result := espBlotchesSquiggles
  else
  if (AValue = rsEggSpotsSquiggles) or (AValue = 'PS') then
    Result := espSpotsSquiggles
  else
    Result := espUnknown;
end;

function StrToEggShape(const AValue: String): TEggShape;
begin
  if (AValue = rsEggSpherical) or (AValue = 'S') then
    Result := esSpherical
  else
  if (AValue = rsEggOval) or (AValue = 'O') then
    Result := esOval
  else
  if (AValue = rsEggElliptical) or (AValue = 'E') then
    Result := esElliptical
  else
  if (AValue = rsEggConical) or (AValue = 'C') then
    Result := esConical
  else
  if (AValue = rsEggBiconical) or (AValue = 'B') then
    Result := esBiconical
  else
  if (AValue = rsEggCylindrical) or (AValue = 'Y') then
    Result := esCylindrical
  else
  if (AValue = rsEggLongitudinal) or (AValue = 'L') then
    Result := esLongitudinal
  else
  if (AValue = rsEggPyriform) or (AValue = 'P') then
    Result := esPiriform
  else
    Result := esUnknown;
end;

function StrToEggTexture(const AValue: String): TEggshellTexture;
begin
  if (AValue = rsEggChalky) or (AValue = 'C') then
    Result := estChalky
  else
  if (AValue = rsEggGlossy) or (AValue = 'G') then
    Result := estGlossy
  else
  if (AValue = rsEggPitted) or (AValue = 'P') then
    Result := estPitted
  else
  if (AValue = rsEggShiny) or (AValue = 'S') then
    Result := estShiny
  else
    Result := estUnknown;
end;

function StrToFeatherAge(const AValue: String): TFeatherAge;
begin
  if (AValue = rsAgeNestling) or (AValue = 'N') then
    Result := fageNestling
  else
  if (AValue = rsAgeFledgling) or (AValue = 'F') then
    Result := fageFledgling
  else
  //if (AValue = rsAgeJuvenile) or (AValue = 'J') then
  //  Result := fageJuvenile
  //else
  if (AValue = rsAgeAdult) or (AValue = 'A') then
    Result := fageAdult
  else
  if (AValue = rsAgeFirstYear) or (AValue = 'Y') then
    Result := fageFirstYear
  else
  if (AValue = rsAgeSecondYear) or (AValue = 'S') then
    Result := fageSecondYear
  else
  if (AValue = rsAgeThirdYear) or (AValue = 'T') then
    Result := fageThirdYear
  else
  if (AValue = rsAgeFourthYear) or (AValue = '4') then
    Result := fageFourthYear
  else
  if (AValue = rsAgeFifthYear) or (AValue = '5') then
    Result := fageFifthYear
  else
    Result := fageUnknown;
end;

function StrToFeatherSource(const AValue: String): TFeatherDataSource;
begin
  if (AValue = rsFeatherCapture) or (AValue = 'C') then
    Result := fdsCapture
  else
  if (AValue = rsFeatherSighting) or (AValue = 'S') then
    Result := fdsSighting
  else
  if (AValue = rsFeatherPhoto) or (AValue = 'P') then
    Result := fdsPhoto
  else
    Result := fdsUnknown;
end;

function StrToFeatherTrait(const AValue: String): TFeatherTrait;
begin
  if (AValue = rsTraitBody) or (AValue = 'B') then
    Result := ftrBody
  else
  if (AValue = rsTraitPrimary) or (AValue = 'P') then
    Result := ftrPrimary
  else
  if (AValue = rsTraitSecondary) or (AValue = 'S') then
    Result := ftrSecondary
  else
  if (AValue = rsTraitRectrix) or (AValue = 'R') then
    Result := ftrRectrix
  else
  if (AValue = rsTraitPrimaryCovert) or (AValue = 'PC') then
    Result := ftrPrimaryCovert
  else
  if (AValue = rsTraitGreatCovert) or (AValue = 'GC') then
    Result := ftrGreatCovert
  else
  if (AValue = rsTraitMedianCovert) or (AValue = 'MC') then
    Result := ftrMedianCovert
  else
  if (AValue = rsTraitLesserCovert) or (AValue = 'LC') then
    Result := ftrLesserCovert
  else
  if (AValue = rsTraitCarpalCovert) or (AValue = 'CC') then
    Result := ftrCarpalCovert
  else
  if (AValue = rsTraitAlula) or (AValue = 'AL') then
    Result := ftrAlula;
end;

function StrToGoalStatus(const AValue: String): TGoalStatus;
begin
  if (AValue = rsGoalPending) or (AValue = 'P') then
    Result := gstPending
  else
  if (AValue = rsGoalReached) or (AValue = 'R') then
    Result := gstReached
  else
  if (AValue = rsGoalCanceled) or (AValue = 'C') then
    Result := gstCanceled;
end;

function StrToImageType(const AValue: String): TImageType;
begin
  if (AValue = rsBirdInHandFlank) or (AValue = 'flank') then
    Result := itBirdInHandFlank
  else
  if (AValue = rsBirdInHandBelly) or (AValue = 'belly') then
    Result := itBirdInHandBelly
  else
  if (AValue = rsBirdInHandBack) or (AValue = 'back') then
    Result := itBirdInHandBack
  else
  if (AValue = rsBirdInHandWing) or (AValue = 'wing') then
    Result := itBirdInHandWing
  else
  if (AValue = rsBirdInHandTail) or (AValue = 'tail') then
    Result := itBirdInHandTail
  else
  if (AValue = rsBirdInHandHead) or (AValue = 'head') then
    Result := itBirdInHandHead
  else
  if (AValue = rsBirdInHandFeet) or (AValue = 'feet') then
    Result := itBirdInHandFeet
  else
  if (AValue = rsFreeBirdStanding) or (AValue = 'stand') then
    Result := itFreeBirdStanding
  else
  if (AValue = rsFreeBirdFlying) or (AValue = 'fly') then
    Result := itFreeBirdFlying
  else
  if (AValue = rsFreeBirdSwimming) or (AValue = 'swim') then
    Result := itFreeBirdSwimming
  else
  if (AValue = rsFreeBirdForraging) or (AValue = 'forr') then
    Result := itFreeBirdForraging
  else
  if (AValue = rsFreeBirdCopulating) or (AValue = 'copul') then
    Result := itFreeBirdCopulating
  else
  if (AValue = rsFreeBirdBuildingNest) or (AValue = 'build') then
    Result := itFreeBirdBuildingNest
  else
  if (AValue = rsFreeBirdDisplaying) or (AValue = 'disp') then
    Result := itFreeBirdDisplaying
  else
  if (AValue = rsFreeBirdIncubating) or (AValue = 'incub') then
    Result := itFreeBirdIncubating
  else
  if (AValue = rsFreeBirdVocalizing) or (AValue = 'vocal') then
    Result := itFreeBirdVocalizing
  else
  if (AValue = rsFreeBirdAgonistic) or (AValue = 'agon') then
    Result := itFreeBirdAgonistic
  else
  if (AValue = rsDeadBird) or (AValue = 'dead') then
    Result := itDeadBird
  else
  if (AValue = rsBirdFlock) or (AValue = 'flock') then
    Result := itBirdFlock
  else
  if (AValue = rsBirdNest) or (AValue = 'nest') then
    Result := itBirdNest
  else
  if (AValue = rsBirdEgg) or (AValue = 'egg') then
    Result := itBirdEgg
  else
  if (AValue = rsBirdNestling) or (AValue = 'nstln') then
    Result := itBirdNestling
  else
  if (AValue = rsEctoparasite) or (AValue = 'paras') then
    Result := itEctoparasite
  else
  if (AValue = rsFootprint) or (AValue = 'fprnt') then
    Result := itFootprint
  else
  if (AValue = rsFeather) or (AValue = 'feath') then
    Result := itFeather
  else
  if (AValue = rsFeces) or (AValue = 'feces') then
    Result := itFeces
  else
  if (AValue = rsFood) or (AValue = 'food') then
    Result := itFood
  else
  if (AValue = rsEnvironment) or (AValue = 'envir') then
    Result := itEnvironment
  else
  if (AValue = rsFieldwork) or (AValue = 'fwork') then
    Result := itFieldwork
  else
  if (AValue = rsTeam) or (AValue = 'team') then
    Result := itTeam
  else
    Result := itEmpty;
end;

function StrToLossCause(const AValue: String): TLossCause;
begin
  if (AValue = rsLossPredation) or (AValue = 'PRE') then
    Result := nlcPredation
  else
  if (AValue = rsLossParasitism) or (AValue = 'PAR') then
    Result := nlcParasitism
  else
  if (AValue = rsLossDisease) or (AValue = 'DIS') then
    Result := nlcDisease
  else
  if (AValue = rsLossWeather) or (AValue = 'WEA') then
    Result := nlcWeather
  else
  if (AValue = rsLossFire) or (AValue = 'FIR') then
    Result := nlcFire
  else
  if (AValue = rsLossAbandonment) or (AValue = 'ABD') then
    Result := nlcAbandonment
  else
  if (AValue = rsLossPollution) or (AValue = 'POL') then
    Result := nlcPollution
  else
  if (AValue = rsLossHumanDisturbance) or (AValue = 'HDT') then
    Result := nlcHumanDisturbance
  else
  if (AValue = rsLossImproperManagement) or (AValue = 'IMN') then
    Result := nlcImproperManagement
  else
    Result := nlcUnknown;
end;

function StrToNestFate(const AValue: String): TNestFate;
begin
  if (AValue = rsNestLost) or (AValue = 'L') then
    Result := nfLoss
  else
  if (AValue = rsNestSuccess) or (AValue = 'S') then
    Result := nfSuccess
  else
    Result := nfUnknown;
end;

function StrToNestRole(const AValue: String): TNestRole;
begin
  if (AValue = rsNestMale) or (AValue = 'M') then
    Result := nrlMale
  else
  if (AValue = rsNestFemale) or (AValue = 'F') then
    Result := nrlFemale
  else
  if (AValue = rsNestHelper) or (AValue = 'H') then
    Result := nrlHelper
  else
  if (AValue = rsNestOffspring) or (AValue = 'O') then
    Result := nrlOffspring
  else
    Result := nrlUnknown;
end;

function StrToNestShape(const AValue: String): String;
begin
  if (AValue = rsNestShapeScrape) then
    Result := 'SC'
  else
  if (AValue = rsNestShapeCup) then
    Result := 'CP'
  else
  if (AValue = rsNestShapePlate) then
    Result := 'PT'
  else
  if (AValue = rsNestShapeSphere) then
    Result := 'SP'
  else
  if (AValue = rsNestShapePendent) then
    Result := 'PD'
  else
  if (AValue = rsNestShapePlatform) then
    Result := 'PL'
  else
  if (AValue = rsNestShapeMound) then
    Result := 'MN'
  else
  if (AValue = rsNestShapeBurrow) then
    Result := 'BR'
  else
  if (AValue = rsNestShapeCavity) then
    Result := 'CV';
end;

function StrToNestStage(const AValue: String): TNestStage;
begin
  if (AValue = rsNestBuilding) or (AValue = 'C') then
    Result := nsgConstruction
  else
  if (AValue = rsNestLaying) or (AValue = 'L') then
    Result := nsgLaying
  else
  if (AValue = rsNestIncubating) or (AValue = 'I') then
    Result := nsgIncubation
  else
  if (AValue = rsNestHatching) or (AValue = 'H') then
    Result := nsgHatching
  else
  if (AValue = rsNestNestling) or (AValue = 'N') then
    Result := nsgNestling
  else
  if (AValue = rsNestInactive) or (AValue = 'X') then
    Result := nsgInactive
  else
    Result := nsgUnknown;
end;

function StrToNestStatus(const AValue: String): TNestStatus;
begin
  if (AValue = rsNestInactive) or (AValue = 'I') then
    Result := nstInactive
  else
  if (AValue = rsNestActive) or (AValue = 'A') then
    Result := nstActive
  else
    Result := nstUnknown;
end;

function StrToPermitType(const AValue: String): String;
begin
  if (AValue = rsPermitBanding) then
    Result := 'B'
  else
  if (AValue = rsPermitCollection) then
    Result := 'C'
  else
  if (AValue = rsPermitResearch) then
    Result := 'R'
  else
  if (AValue = rsPermitEntry) then
    Result := 'E'
  else
  if (AValue = rsPermitTransport) then
    Result := 'T'
  else
    Result := 'O';
end;

function StrToPrecipitation(const AValue: String): TPrecipitation;
begin
  if (AValue = rsPrecipitationNone) then
    Result := wpNone
  else
  if (AValue = rsPrecipitationFog) then
    Result := wpFog
  else
  if (AValue = rsPrecipitationMist) then
    Result := wpMist
  else
  if (AValue = rsPrecipitationDrizzle) then
    Result := wpDrizzle
  else
  if (AValue = rsPrecipitationRain) then
    Result := wpRain
  else
    Result := wpEmpty;
end;

function StrToSampleMoment(const AValue: String): TWeatherSampleMoment;
begin
  if (AValue = rsMomentStart) then
    Result := wmStart
  else
  if (AValue = rsMomentMiddle) then
    Result := wmMiddle
  else
  if (AValue = rsMomentEnd) then
    Result := wmEnd
  else
    Result := wmNone;
end;

function StrToSex(const AValue: String): TSex;
begin
  if (AValue = rsSexMale) or (AValue = 'M') then
    Result := sexMale
  else
  if (AValue = rsSexFemale) or (AValue = 'F') then
    Result := sexFemale
  else
    Result := sexUnknown;
end;

function StrToSiteRank(const AValue: String): TSiteRank;
begin
  { #todo : Translate site rank also from numbers }
  if (AValue = rsCaptionCountry) or (AValue = 'P') then
    Result := srCountry
  else
  if (AValue = rsCaptionState) or (AValue = 'E') then
    Result := srState
  else
  if (AValue = rsCaptionRegion) or (AValue = 'R') then
    Result := srRegion
  else
  if (AValue = rsCaptionMunicipality) or (AValue = 'M') then
    Result := srMunicipality
  else
  if (AValue = rsCaptionDistrict) or (AValue = 'D') then
    Result := srDistrict
  else
  if (AValue = rsCaptionLocality) or (AValue = 'L') then
    Result := srLocality
  else
    Result := srNone;
end;

function StrToSpecimenType(const AValue: String): TSpecimenType;
begin
  if (AValue = rsSpecimenCarcassWhole) or (AValue = 'WS') then
    Result := sptWholeCarcass
  else
  if (AValue = rsSpecimenCarcassPartial) or (AValue = 'PS') then
    Result := sptPartialCarcass
  else
  if (AValue = rsSpecimenNest) or (AValue = 'N') then
    Result := sptNest
  else
  if (AValue = rsSpecimenBones) or (AValue = 'B') then
    Result := sptBones
  else
  if (AValue = rsSpecimenEgg) or (AValue = 'E') then
    Result := sptEgg
  else
  if (AValue = rsSpecimenParasites) or (AValue = 'P') then
    Result := sptParasites
  else
  if (AValue = rsSpecimenFeathers) or (AValue = 'F') then
    Result := sptFeathers
  else
  if (AValue = rsSpecimenBlood) or (AValue = 'BS') then
    Result := sptBlood
  else
  if (AValue = rsSpecimenClaw) or (AValue = 'C') then
    Result := sptClaw
  else
  if (AValue = rsSpecimenSwab) or (AValue = 'S') then
    Result := sptSwab
  else
  if (AValue = rsSpecimenTissues) or (AValue = 'T') then
    Result := sptTissues
  else
  if (AValue = rsSpecimenFeces) or (AValue = 'D') then
    Result := sptFeces
  else
  if (AValue = rsSpecimenRegurgite) or (AValue = 'R') then
    Result := sptRegurgite;
end;

function StrToStratumDistribution(const AValue: String): TStratumDistribution;
begin
  if (AValue = rsDistributionNone) or (AValue = '0') then
    Result := disNone
  else
  if (AValue = rsDistributionRare) or (AValue = '1') then
    Result := disRare
  else
  if (AValue = rsDistributionFewSparse) or (AValue = '2') then
    Result := disFewSparseIndividuals
  else
  if (AValue = rsDistributionOnePatch) or (AValue = '3') then
    Result := disOnePatch
  else
  if (AValue = rsDistributionOnePatchFewSparse) or (AValue = '4') then
    Result := disOnePatchFewSparseIndividuals
  else
  if (AValue = rsDistributionManySparse) or (AValue = '5') then
    Result := disManySparseIndividuals
  else
  if (AValue = rsDistributionOnePatchManySparse) or (AValue = '6') then
    Result := disOnePatchManySparseIndividuals
  else
  if (AValue = rsDistributionFewPatches) or (AValue = '7') then
    Result := disFewPatches
  else
  if (AValue = rsDistributionFewPatchesSparse) or (AValue = '8') then
    Result := disFewPatchesSparseIndividuals
  else
  if (AValue = rsDistributionManyPatches) or (AValue = '9') then
    Result := disManyPatches
  else
  if (AValue = rsDistributionManyPatchesSparse) or (AValue = '10') then
    Result := disManyPatchesSparseIndividuals
  else
  if (AValue = rsDistributionEvenHighDensity) or (AValue = '11') then
    Result := disHighDensityIndividuals
  else
  if (AValue = rsDistributionContinuousFewGaps) or (AValue = '12') then
    Result := disContinuousCoverWithGaps
  else
  if (AValue = rsDistributionContinuousDense) or (AValue = '13') then
    Result := disContinuousDenseCover
  else
  if (AValue = rsDistributionContinuousDenseEdge) or (AValue = '14') then
    Result := disContinuousDenseCoverWithEdge
  else
    Result := disNone;
end;

function StrToSubjectStatus(const AValue: String): TSubjectStatus;
begin
  if (AValue = rsStatusNormal) or (AValue = 'N') then
    Result := sstNormal
  else
  if (AValue = rsStatusStressed) or (AValue = 'X') then
    Result := sstStressed
  else
  if (AValue = rsStatusInjured) or (AValue = 'I') then
    Result := sstInjured
  else
  if (AValue = rsStatusWingSprain) or (AValue = 'W') then
    Result := sstWingSprain
  else
  if (AValue = rsStatusDead) or (AValue = 'D') then
    Result := sstDead;
end;

function StrToSupportType(const AValue: String): String;
begin
  if (AValue = rsSupportGround) then
    Result := 'G'
  else
  if (AValue = rsSupportHerbBush) then
    Result := 'H'
  else
  if (AValue = rsSupportBranchFork) then
    Result := 'F'
  else
  if (AValue = rsSupportLeaves) then
    Result := 'L'
  else
  if (AValue = rsSupportLedge) then
    Result := 'D'
  else
  if (AValue = rsSupportRockCliff) then
    Result := 'C'
  else
  if (AValue = rsSupportRavine) then
    Result := 'R'
  else
  if (AValue = rsSupportNestBox) then
    Result := 'B'
  else
  if (AValue = rsSupportAnthropic) then
    Result := 'A'
  else
  if (AValue = rsSupportOther) then
    Result := 'O';
end;

function StrToSymmetry(const AValue: String): TSymmetry;
begin
  if (AValue = rsSymmetrical) or (AValue = 'S') then
    Result := symSymmetrical
  else
  if (AValue = rsAsymmetrical) or (AValue = 'A') then
    Result := symAsymmetrical
  else
    Result := symUnknown;
end;

function StrToUserRank(const AValue: String): TUserRank;
begin
  if (AValue = rsAdminUser) or (AValue = 'A') then
    Result := urAdministrator
  else
  if (AValue = rsGuestUser) or (AValue = 'V') then
    Result := urVisitor
  else
    Result := urStandard;
end;

function StrToVideoType(const AValue: String): TVideoType;
begin
  if (AValue = rsVideoGeneral) or (AValue = 'GEN') then
    Result := vtGeneral
  else
  if (AValue = rsVideoForaging) or (AValue = 'FOR') then
    Result := vtForaging
  else
  if (AValue = rsVideoVocalizing) or (AValue = 'VOC') then
    Result := vtVocalizing
  else
  if (AValue = rsVideoCourtship) or (AValue = 'CRT') then
    Result := vtCourtship
  else
  if (AValue = rsVideoAggression) or (AValue = 'AGR') then
    Result := vtAggression
  else
  if (AValue = rsVideoSocial) or (AValue = 'SOC') then
    Result := vtSocial
  else
  if (AValue = rsVideoParentalCare) or (AValue = 'PAR') then
    Result := vtParentalCare
  else
  if (AValue = rsVideoNestBuilding) or (AValue = 'NBU') then
    Result := vtNestBuilding
  else
  if (AValue = rsVideoIncubation) or (AValue = 'INC') then
    Result := vtIncubation
  else
  if (AValue = rsVideoNestlings) or (AValue = 'NLG') then
    Result := vtNestlings
  else
  if (AValue = rsVideoFlight) or (AValue = 'FLT') then
    Result := vtFlight
  else
  if (AValue = rsVideoLocomotion) or (AValue = 'LOC') then
    Result := vtLocomotion
  else
  if (AValue = rsVideoHygiene) or (AValue = 'HYG') then
    Result := vtHygiene
  else
  if (AValue = rsVideoPredation) or (AValue = 'PRD') then
    Result := vtPredation
  else
  if (AValue = rsVideoMortality) or (AValue = 'MOR') then
    Result := vtMortality
  else
    Result := vtUnknown;
end;

end.

