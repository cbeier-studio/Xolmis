unit test_utils_gis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  utils_gis;

type
  TUtilsGisTest = class(TTestCase)
  published
    procedure TestMapPointParsing;
    procedure TestDmsParsingAndConversion;
    procedure TestCoordinateClassConversions;
  end;

implementation

procedure TUtilsGisTest.TestMapPointParsing;
var
  Point: TMapPoint;
begin
  CheckTrue(Point.FromString(FloatToStr(12.5) + '; ' + FloatToStr(-3.25)));
  CheckEquals(12.5, Point.X, 0.00001);
  CheckEquals(-3.25, Point.Y, 0.00001);
  CheckFalse(Point.FromString('12.5'));
end;

procedure TUtilsGisTest.TestDmsParsingAndConversion;
var
  Dms: TDMSPoint;
begin
  CheckTrue(Dms.FromString('46 57 0 W; 23 33 0 S'));
  CheckFalse(Dms.FromString('46 57 W'));
end;

procedure TUtilsGisTest.TestCoordinateClassConversions;
var
  Source, ResultPoint: TMapPoint;
  DecimalCoord, DmsCoord, UtmCoord: IMapCoordinate;
begin
  Source.X := -46.95;
  Source.Y := 23.55;
  DecimalCoord := TDecimalCoord.Create(Source);

  DmsCoord := TCoordinateConverter.Convert(DecimalCoord, mcDMS);
  CheckEquals(Ord(mcDMS), Ord(DmsCoord.GetCoordinateType));
  ResultPoint := DmsCoord.ToDecimal;
  CheckEquals(Source.X, ResultPoint.X, 0.001);
  CheckEquals(Source.Y, ResultPoint.Y, 0.001);

  UtmCoord := TCoordinateConverter.Convert(DecimalCoord, mcUTM);
  CheckEquals(Ord(mcUTM), Ord(UtmCoord.GetCoordinateType));
  ResultPoint := UtmCoord.ToDecimal;
  CheckEquals(Source.X, ResultPoint.X, 0.001);
  CheckEquals(Source.Y, ResultPoint.Y, 0.001);

  Source.Y := -23.55;
  DecimalCoord := TDecimalCoord.Create(Source);
  DmsCoord := TCoordinateConverter.Convert(DecimalCoord, mcDMS);
  ResultPoint := DmsCoord.ToDecimal;
  CheckEquals(Source.X, ResultPoint.X, 0.001);
  CheckEquals(Source.Y, ResultPoint.Y, 0.001);
  UtmCoord := TCoordinateConverter.Convert(DecimalCoord, mcUTM);
  ResultPoint := UtmCoord.ToDecimal;
  CheckEquals(Source.X, ResultPoint.X, 0.001);
  CheckEquals(Source.Y, ResultPoint.Y, 0.001);
end;

initialization
  RegisterTest(TUtilsGisTest);

end.