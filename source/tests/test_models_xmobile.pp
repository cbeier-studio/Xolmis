unit test_models_xmobile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fpcunit, testregistry,
  models_xmobile;

type
  TModelsXMobileTest = class(TTestCase)
  published
    procedure TestMobilePoiFromJSON;
  end;

implementation

procedure TModelsXMobileTest.TestMobilePoiFromJSON;
var
  Poi: TMobilePoi;
begin
  Poi := TMobilePoi.Create;
  try
    Poi.FromJSON(GetJSON('{"id":12,"speciesId":34,"longitude":-46.95,' +
      '"latitude":-23.55,"notes":"field note"}'));
    CheckEquals(12, Poi.FId);
    CheckEquals(34, Poi.FSpeciesId);
    CheckEquals(-46.95, Poi.FLongitude, 0.00001);
    CheckEquals(-23.55, Poi.FLatitude, 0.00001);
    CheckEquals('field note', Poi.FNotes);
  finally
    Poi.Free;
  end;
end;

initialization
  RegisterTest(TModelsXMobileTest);

end.