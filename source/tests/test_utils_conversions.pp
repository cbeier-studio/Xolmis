unit test_utils_conversions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testutils, testregistry,
  models_record_types, utils_conversions, utils_global;

type
  TUtilsConversionsTest = class(TTestCase)
  published
    procedure TestTextHelpers;
    procedure TestNameFormatting;
    procedure TestBooleanAndNumericHelpers;
    procedure TestDateAndTimeHelpers;
    procedure TestEnumConversions;
  end;

implementation

procedure TUtilsConversionsTest.TestTextHelpers;
begin
  CheckEquals('one two three', NormalizeWhitespace('one  two'#13#10'three', True));
  CheckEquals('a e i o u c n', RemoveDiacritics('a e i o u c n'));
  CheckEquals('a e i o u c n', RemoveDiacritics('á é í ó ú ç ñ'));
  CheckEquals('Example text', SentenceCase('eXAMPLE TEXT'));
end;

procedure TUtilsConversionsTest.TestNameFormatting;
begin
  CheckEquals('Silva, J. C.', GenerateCitation('Joao da Costa Silva'));
  CheckEquals('Silva, Joao da Costa', GenerateCitation('Joao da Costa Silva', False));
  CheckEquals('JCS', GenerateAbbreviation('Joao da Costa Silva'));
  CheckEquals('JOCS', GenerateAbbreviation('Joao da Costa Silva', False, ablSecondLetterFirst));
end;

procedure TUtilsConversionsTest.TestBooleanAndNumericHelpers;
begin
  CheckTrue(TextToBool('YES', 'yes', 'no'));
  CheckFalse(TextToBool('no', 'yes', 'no'));
  CheckEquals('yes', BoolToText(True, 'yes', 'no'));
  CheckEquals('no', BoolToText(False, 'yes', 'no'));
end;

procedure TUtilsConversionsTest.TestDateAndTimeHelpers;
var
  DateValue: TDate;
  TimeValue: TTime;
begin
  DateValue := TextToDate(DateToStr(EncodeDate(2024, 1, 15)));
  CheckTrue(DateValue <> NullDate);
  CheckEquals(2024, YearOf(DateValue));
  TimeValue := TextToTime(TimeToStr(EncodeTime(13, 45, 0, 0)));
  CheckTrue(TimeValue <> NullTime);
  CheckEquals(13, HourOf(TimeValue));
end;

procedure TUtilsConversionsTest.TestEnumConversions;
begin
  CheckEquals(Ord(sexMale), Ord(StrToSex('M')));
  CheckEquals(Ord(bsdLeft), Ord(StrToBodySide('L')));
  CheckEquals(Ord(cptRecapture), Ord(StrToCaptureType('R')));
  CheckEquals(Ord(cpReference), Ord(StrToCoordinatePrecision('R')));
  CheckEquals('PC', FeatherTraitToStr(StrToFeatherTrait('PC')));
  CheckEquals('R', CaptureTypeToStr(cptRecapture));
end;

initialization
  RegisterTest(TUtilsConversionsTest);

end.
