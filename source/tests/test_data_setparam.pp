unit test_data_setparam;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, DB, fpcunit, testregistry,
  utils_global, data_setparam;

type
  TDataSetParamTest = class(TTestCase)
  published
    procedure TestIntegerAndForeignParameters;
    procedure TestStringAndFloatParameters;
    procedure TestCoordinateParameters;
    procedure TestDateAndTimeParameters;
  end;

implementation

procedure TDataSetParamTest.TestIntegerAndForeignParameters;
var
  Param: TParam;
begin
  Param := TParam.Create(nil, ptInput);
  try
    SetIntParam(Param, 42);
    CheckEquals(42, Param.AsInteger);
    SetIntParam(Param, 0);
    CheckTrue(Param.IsNull);
    SetForeignParam(Param, 7);
    CheckEquals(7, Param.AsInteger);
    SetForeignParam(Param, -1);
    CheckTrue(Param.IsNull);
  finally
    Param.Free;
  end;
end;

procedure TDataSetParamTest.TestStringAndFloatParameters;
var
  StringParam, FloatParam: TParam;
begin
  StringParam := TParam.Create(nil, ptInput);
  FloatParam := TParam.Create(nil, ptInput);
  try
    SetStrParam(StringParam, 'value');
    CheckEquals('value', StringParam.AsString);
    SetStrParam(StringParam, '');
    CheckTrue(StringParam.IsNull);
    SetFloatParam(FloatParam, 2.5);
    CheckEquals(2.5, FloatParam.AsFloat, 0.00001);
    SetFloatParam(FloatParam, 0.0);
    CheckTrue(FloatParam.IsNull);
  finally
    StringParam.Free;
    FloatParam.Free;
  end;
end;

procedure TDataSetParamTest.TestCoordinateParameters;
var
  Longitude, Latitude: TParam;
begin
  Longitude := TParam.Create(nil, ptInput);
  Latitude := TParam.Create(nil, ptInput);
  try
    SetCoordinateParam(Longitude, Latitude, -46.95, -23.55);
    CheckEquals(-46.95, Longitude.AsFloat, 0.00001);
    CheckEquals(-23.55, Latitude.AsFloat, 0.00001);
    SetCoordinateParam(Longitude, Latitude, 0.0, -23.55);
    CheckTrue(Longitude.IsNull);
    CheckTrue(Latitude.IsNull);
  finally
    Longitude.Free;
    Latitude.Free;
  end;
end;

procedure TDataSetParamTest.TestDateAndTimeParameters;
var
  DateParam, TimeParam: TParam;
begin
  DateParam := TParam.Create(nil, ptInput);
  TimeParam := TParam.Create(nil, ptInput);
  try
    SetDateParam(DateParam, EncodeDate(2024, 2, 3));
    CheckEquals('2024-02-03', DateParam.AsString);
    SetDateParam(DateParam, NullDate);
    CheckTrue(DateParam.IsNull);
    SetTimeParam(TimeParam, EncodeTime(14, 30, 0, 0));
    CheckEquals('14:30:00', TimeParam.AsString);
    SetTimeParam(TimeParam, NullTime);
    CheckTrue(TimeParam.IsNull);
  finally
    DateParam.Free;
    TimeParam.Free;
  end;
end;

initialization
  RegisterTest(TDataSetParamTest);

end.