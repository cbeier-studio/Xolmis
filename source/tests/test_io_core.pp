unit test_io_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  data_types, io_core;

type
  TIOCoreTest = class(TTestCase)
  published
    procedure TestTransformationSetRoundTrip;
    procedure TestFieldMappingValidation;
    procedure TestFieldMappingJSONRoundTrip;
  end;

implementation

procedure TIOCoreTest.TestTransformationSetRoundTrip;
var
  Transformations, Parsed: TValueTransformationSet;
begin
  Transformations := [vtrTrim, vtrUpperCase, vtrRound, vtrConvertCoordinates];
  Parsed := StringToTransformations(TransformationsToString(Transformations));
  CheckTrue(Transformations = Parsed);
  CheckTrue(vtrTrim in Parsed);
  CheckTrue(vtrConvertCoordinates in Parsed);
end;

procedure TIOCoreTest.TestFieldMappingValidation;
var
  Mapping: TFieldMapping;
begin
  Mapping := TFieldMapping.Create;
  try
    Mapping.Reset;
    CheckFalse(Mapping.IsMapped);
    CheckTrue(Mapping.IsValid);

    Mapping.Import := True;
    CheckFalse(Mapping.IsValid);
    Mapping.TargetField := 'name';
    CheckTrue(Mapping.IsMapped);
    CheckTrue(Mapping.IsValid);

    Mapping.DataType := sdtLookup;
    CheckFalse(Mapping.IsValid);
    Mapping.LookupTable := tbPeople;
    Mapping.LookupField := 'full_name';
    CheckTrue(Mapping.IsValid);
  finally
    Mapping.Free;
  end;
end;

procedure TIOCoreTest.TestFieldMappingJSONRoundTrip;
var
  Mapping, Parsed: TFieldMapping;
begin
  Mapping := TFieldMapping.Create;
  Parsed := TFieldMapping.Create;
  try
    Mapping.Reset;
    Mapping.SourceField := 'source_name';
    Mapping.TargetField := 'full_name';
    Mapping.Import := True;
    Mapping.Transformations := [vtrTrim, vtrLowerCase];
    Mapping.RoundPrecision := 2;
    Parsed.FromJSON(Mapping.ToJSON);
    CheckEquals(Mapping.SourceField, Parsed.SourceField);
    CheckEquals(Mapping.TargetField, Parsed.TargetField);
    CheckTrue(Parsed.IsMapped);
    CheckTrue(Parsed.HasTransformation(vtrTrim));
    CheckTrue(Parsed.HasTransformation(vtrLowerCase));
    CheckEquals(Mapping.RoundPrecision, Parsed.RoundPrecision);
  finally
    Mapping.Free;
    Parsed.Free;
  end;
end;

initialization
  RegisterTest(TIOCoreTest);

end.