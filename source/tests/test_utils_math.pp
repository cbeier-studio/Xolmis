unit test_utils_math;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  utils_math;

type
  TUtilsMathTest = class(TTestCase)
  published
    procedure TestMedianOddCount;
    procedure TestMedianEvenCount;
    procedure TestMedianNegativeValues;
  end;

implementation

procedure TUtilsMathTest.TestMedianOddCount;
begin
  CheckEquals(3.0, Median([1.0, 3.0, 5.0]), 0.00001);
end;

procedure TUtilsMathTest.TestMedianEvenCount;
begin
  CheckEquals(2.5, Median([1.0, 2.0, 3.0, 4.0]), 0.00001);
end;

procedure TUtilsMathTest.TestMedianNegativeValues;
begin
  CheckEquals(-3.0, Median([-5.0, -3.0, -1.0]), 0.00001);
end;

initialization
  RegisterTest(TUtilsMathTest);

end.