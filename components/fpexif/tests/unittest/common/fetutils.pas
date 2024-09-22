unit fetUtils;

{$IFDEF FPC}
 {$mode delphi} //objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
 {$IFDEF FPC}
  fpcunit, testutils, testregistry;
 {$ELSE}
  TestFrameWork;
 {$ENDIF}

type
  TstUtils = class(TTestCase)
  published
    procedure TestCountChar;
    procedure TestFloatToRational;
    procedure TestInsertSpaces;
    procedure TestLookup;
    procedure TestSplit;
    procedure TestSplitGPS;
    procedure TestStrToGPS;
    procedure TestStrToRational;
  end;

implementation

uses
  math,
  fpeGlobal, fpeUtils;

type
  TCountCharParam = record
    TestString: String;
    ch: Char;
    Count: Integer;
  end;

  TInsertSpacesParam = record
    TestString: String;
    ResultString: String;
  end;

  TFloatToRationalParam = record
    Value, Precision: Double;
    Num, Denom: Integer;
    Error: Integer;  // 0:ok, 1: Num is wrong, 2: Denom is wrong
  end;

  TLookupParam = record
    SearchForKey: Boolean;
    SearchStr: String;
    ResultStr: String;
  end;

  TSplitGpsParam = record
    Value: Double;
    Degs: Double;
    Mins: Double;
    Secs: Double
  end;

  TSplitParam = record
    Text: String;
    Sep: String;
    NumParts: Integer;
    Parts: Array[0..2] of string;
  end;

  TStrToGpsParam = record
    Text: String;
    Degs: Double;
    Valid: Boolean;
  end;

  TStrToRationalParam = record
    Value: String;
    Num, Denom: Integer;
  end;

const
  CountCharParams: array[0..5] of TCountCharParam = (
    (TestString:'';   ch:'a'; Count:0),
    (TestString:'a';  ch:'a'; Count:1),
    (TestString:'aa'; ch:'a'; Count:2),
    (TestString:'b';  ch:'a'; Count:0),
    (TestString:'ab'; ch:'a'; Count:1),
    (TestString:'ba'; ch:'a'; Count:1)
  );

  InsertSpacesParams: array[0..14] of TInsertSpacesParam = (
    (TestString: 'Artist';                ResultString: 'Artist'),
    (TestString: 'ShutterSpeed';          ResultString: 'Shutter Speed'),
    (TestString: 'ThumbnailXResolution';  ResultString: 'Thumbnail X Resolution'),
    (TestString: 'YCbCrPositioning';      ResultString: 'Y Cb Cr Positioning'),
    (TestString: 'ISO';                   ResultString: 'ISO'),
    (TestString: 'GPSInfo';               ResultString: 'GPS Info'),
    (TestString: 'IPTC/NAA';              ResultString: 'IPTC/NAA'),
    (TestString: 'XPTitle';               ResultString: 'XP Title'),
    (TestString: 'PrintIM';               ResultString: 'Print IM'),
    (TestString: 'ResolutionX';           ResultString: 'Resolution X'),
    (TestString: 'XResolution';           ResultString: 'X Resolution'),
    (TestString: 'CCD ISO';               ResultString: 'CCD ISO'),
    (TestString: 'AE setting';            ResultString: 'AE setting'),
    (TestString: 'abc ABC';               ResultString: 'abc ABC'),
    (TestString: 'abc Abc';               ResultString: 'abc Abc')
  );

  FloatToRationalParams: array[0..8] of TFloatToRationalParam = (
    (Value:0.0;         Precision: 1E-6; Num:0; Denom:1;     Error:0),    // 0
    (Value:1.0;         Precision: 1E-6; Num:1; Denom:1;     Error:0),    // 1
    (Value:0.5;         Precision: 1E-6; Num:1; Denom:2;     Error:0),    // 2
    (Value:0.01;        Precision: 1E-6; Num:1; Denom:100;   Error:0),    // 3
    (Value:0.333333333; Precision: 1E-6; Num:1; Denom:3;     Error:0),    // 4
    (value:1.166666667; Precision: 1E-6; Num:7; Denom:6;     Error:0),    // 5
    (Value:NaN;         Precision: 1E-6; Num:1; Denom:0;     Error:0),    // 6
    (Value:0.3333;      Precision: 1E-6; Num:1; Denom:3;     Error:2),    // 7
    (Value:0.1;         Precision: 1E-6; Num:1; Denom:3;     Error:2)     // 8
  );

  LkupTbl: String = '0:Zero,1:One,2:Two';
  LookupParams: array[0..8] of TLookupParam = (
    (SearchForKey:true;  SearchStr:'0';     ResultStr:'Zero'),
    (SearchForKey:true;  SearchStr:'1';     ResultStr:'One'),
    (SearchForKey:true;  SearchStr:'2';     ResultStr:'Two'),
    (SearchForKey:true;  SearchStr:'$2';    ResultStr:'Two'),
    (SearchForKey:true;  SearchStr:'3';     ResultStr:'3'),
    (SearchForKey:false; SearchStr:'Zero';  ResultStr:'0'),
    (SearchForKey:false; SearchStr:'One';   ResultStr:'1'),
    (SearchForKey:false; SearchStr:'Two';   ResultStr:'2'),
    (SearchForKey:false; SearchStr:'Three'; ResultStr:'')
  );

  SplitGpsParams: array[0..3] of TSplitGpsParam = (
    (Value:0.5;              Degs: 0; Mins:30; Secs: 0),
    (Value:2.777777E-4;      Degs: 0; Mins: 0; Secs: 1),
    (Value:50.2527777777777; Degs:50; Mins:15; Secs:10),
    (Value:50.2583333333333; Degs:50; Mins:15; Secs:30)
  );

  SplitParams: array[0..3] of TSplitParam = (
    (Text:'One';        Sep: ';';  NumParts: 1; Parts:('One', '',    '')),
    (Text:'One,Two';    Sep: ',';  NumParts: 2; Parts:('One', 'Two', '')),
    (Text:'One, Two';   Sep: ', '; NumParts: 2; Parts:('One', 'Two', '')),
    (Text:'One'#0'Two'; Sep: #0;   NumParts: 2; Parts:('One', 'Two', ''))
  );

  // 1/3600 = 2.77777777777E-4,  1/60 = 0,01666666666666667
  StrToGpsParams: array[0..11] of TStrToGpsParam = (
    (Text:'0 deg 30'' 0"';     Degs: 0.5;              Valid: true),
    (Text:'0 deg  0'' 1"';     Degs: 2.777777E-4;      Valid: true),
    (Text:'50 deg 15'' 10"';   Degs: 50.2527777777777; Valid: true),
    (Text:'50 deg 15'' 30"';   Degs: 50.2583333333333; Valid: true),
    (Text:'50 deg 15.5''';     Degs: 50.2583333333333; Valid: true),
    (Text:'50 deg 60'' 30"';   Degs: NaN;              Valid: false),
    (Text:'50 deg 15'' 70"';   Degs: NaN;              Valid: false),
    (Text:'50.1° 15'' 70"';    Degs: NaN;              Valid: false),
    (Text:'50 deg 15.3'' 50"'; Degs: NaN;              Valid: false),
    (Text:'50 deg -15'' 50"';  Degs: NaN;              Valid: false),
    (Text:'50 deg 15'' -50"';  Degs: NaN;              Valid: false),
    (Text:'-50 deg 15'' 30"';  Degs: 50.2583333333333; Valid: true)
  );

  StrToRationalParams: array[0..9] of TStrToRationalParam = (
    (Value:'0';         Num:0; Denom:1),     // 0
    (Value:'1';         Num:1; Denom:1),     // 1
    (Value:'1/2';       Num:1; Denom:2),     // 2
    (Value:'1/ 2';      Num:1; Denom:2),     // 3
    (Value:'1 /2';      Num:1; Denom:2),     // 4
    (Value:'1 / 2';     Num:1; denom:2),     // 5
    (Value:' 1/2';      Num:1; Denom:2),     // 6
    (Value:'1/2 ';      Num:1; Denom:2),     // 7
    (Value:' 1/2 ';     Num:1; Denom:2),     // 8
    (value:'';          Num:1; Denom:0)      // 9
  );

procedure TstUtils.TestCountChar;
var
  currCount: Integer;
  i: Integer;
begin
  for i:=Low(CountCharParams) to High(CountCharParams) do begin
    currCount := CountChar(CountCharParams[i].ch, CountCharParams[i].TestString);
    CheckEquals(CountCharParams[i].Count, currCount,
      'CountChar mismatch, test case ' + IntToStr(i));
  end;
end;

procedure TstUtils.TestFloatToRational;
var
  currR: TExifRational;
  i: Integer;
begin
  for i:=Low(FloatToRationalParams) to High(FloatToRationalParams) do
    with FloatToRationalParams[i] do begin
      currR := FloatToRational(Value, Precision);
      case Error of
        0: begin
             CheckEquals(currR.Numerator, Num,
               'FloatToRational numerator mismatch, test case ' + IntToStr(i));
             CheckEquals(currR.Denominator, Denom,
               'FloatToRational denominator mismatch, test case ' + IntToStr(i));
           end;
        1: CheckNotEquals(currR.Numerator, Num,
             'Unexpected FloatToRational numerator match, test case ' + IntToStr(i));
        2: CheckNotEquals(currR.Denominator, Denom,
             'Unexpected FloatToRational denominator match, test case ' + IntToStr(i));
      end;
    end;
end;

procedure TstUtils.TestInsertSpaces;
var
  currStr: String;
  i: Integer;
begin
  for i:=Low(InsertSpacesParams) to High(InsertSpacesParams) do begin
    currStr := InsertSpaces(InsertSpacesParams[i].TestString);
    CheckEquals(InsertSpacesParams[i].ResultString, currStr,
      'InsertSpaces mismatch, test case ' + IntToStr(i));
  end;
end;

function SameIntegerKey(AKey1, AKey2: String): Boolean;
var
  k1, k2: Integer;
begin
  Result := TryStrToInt(AKey1, k1) and TryStrToInt(AKey2, k2) and (k1 = k2);
end;

function SameStringKey(AKey1, AKey2: String): Boolean;
begin
  Result := SameText(AKey1, AKey2);
end;

procedure TstUtils.TestLookup;
var
  currResult: String;
  i: Integer;
begin
  for i:=Low(LookupParams) to High(LookupParams) do
    with LookupParams[i] do begin
      if SearchForKey then
        currResult := LookupValue(SearchStr, LkupTbl, @SameIntegerKey)
      else
        currResult := LookupKey(SearchStr, LkupTbl, @SameStringKey);
    CheckEquals(ResultStr, currResult,
      'Lookup mismatch, test case ' + IntToStr(i));
  end;
end;

procedure TstUtils.TestSplit;
var
  currResult: TStringArray;
  i, j: Integer;
begin
  for i:=Low(SplitParams) to High(SplitParams) do
    with SplitParams[i] do begin
      currResult := Split(Text, Sep);
      CheckEquals(NumParts, Length(currResult), 'Split count mismatch');
      for j:=0 to NumParts-1 do
        CheckEquals(Parts[j], currResult[j], 'Split mismatch in array element #' + IntToStr(j));
    end;
end;

procedure TstUtils.TestSplitGPS;
const
  EPS = 1E-6;
var
  currDeg, currMin, currSec: Double;
  i: Integer;
begin
  for i:=Low(SplitGPSParams) to High(SplitGPSParams) do
    with SplitGPSParams[i] do begin
      SplitGPS(Value, currDeg, currMin, currSec);
      CheckEquals(Degs, currDeg, EPS, 'Degree value mismatch, test case ' + IntToStr(i));
      CheckEquals(Mins, currMin, EPS, 'Minutes mismatch, test case ' + IntToStr(i));
      CheckEquals(Secs, currSec, EPS, 'Seconds value mismatch, test case ' + IntToStr(i));
    end;
end;

procedure TstUtils.TestStrToGPS;
const
  EPS = 1E-8;
var
  currDeg: Double;
  i: Integer;
  currOK: Boolean;
begin
  for i:=Low(StrToGpsParams) to High(StrToGpsParams) do begin
    with StrToGpsParams[i] do begin
      currOK := TryStrToGps(Text, currDeg);
      CheckEquals(Valid, currOK, 'GPS result validity mismatch, test case ' + IntToStr(i));
      if Valid then
        CheckEquals(Degs, currDeg, EPS, 'GPS degress mismatch, test case ' + IntToStr(i));
    end;
  end;
end;

procedure TstUtils.TestStrToRational;
var
  currR: TExifRational;
  i: Integer;
begin
  for i:=Low(StrToRationalParams) to High(StrToRationalParams) do
    with StrToRationalParams[i] do begin
      currR := StrToRational(Value);
      CheckEquals(currR.Numerator, Num,
        'StrToRational numerator mismatch, test case ' + IntToStr(i));
      CheckEquals(currR.Denominator, Denom,
        'StrToRational denominator mismatch, test case ' + IntToStr(i));
    end;
end;

initialization
 {$IFDEF FPC}
  RegisterTest(TstUtils);
 {$ELSE}
  TestFramework.RegisterTest(TstUtils.Suite);
 {$ENDIF}

end.

