{ Xolmis Geographical Data library

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

unit cbs_gis;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  { System }
  Classes, Types, SysUtils, Math, LazUTF8, StrUtils, RegExpr,
  { VCL }
  Forms, Controls, ExtCtrls, laz.VirtualTrees, mvMapViewer,
  { Data }
  DB, SQLDB, cbs_record_types, cbs_datatypes;

const
  datumA: Extended = 6378137;          // equatorial radius (in meters), semi major axis
  datumB: Extended = 6356752.31424518; // semi minor axis
  N0: Integer = 10000000;              // in meters (10000 km)
  k0: Double = 0.9996;
  E0: Integer = 500000;                // in meters (500 km)
  GlobeHemispheres: array[0..3] of Char = ('N', 'S', 'E', 'W');
  UtmBands: array[-9..10] of Char = ('C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X');
  DmsSymbols: set of Char = [#176, #186, #39, #146, #180, #34, #148];  { °, º, ', ’, ´, ", ” }

type
  TMapCoordinateType = (mcDecimal, mcDMS, mcUTM);
  TMapAxis = (maBoth, maLongitude, maLatitude);
  TMapHemisphere = (mhNorth, mhSouth, mhEast, mhWest);

  { TMapPoint }

  TMapPoint = record
    X, Y: Extended;
    Name: String;
    function FromString(aCoord: String; aSeparator: TSeparator = spSemicolon): Boolean;
    function ToString(aSeparator: TSeparator = spSemicolon): String;
  end;

  { TDMS }

  TDMS = record
    Degrees: Integer;
    Minutes: Integer;
    Seconds: Extended;
    Hemisphere: Char;
    Text: String;
    function FromString(aCoord: String): Boolean;
    function ToString(WithSymbols: Boolean = False): String;
  end;

  { TDMSPoint }

  TDMSPoint = record
    X, Y: TDMS;
    function FromString(aCoord: String; aSeparator: TSeparator = spSemicolon): Boolean;
    function ToString(WithSymbols: Boolean = False; aSeparator: TSeparator = spSemicolon): String;
  end;

  { TUTMPoint }

  TUTMPoint = record
    X: Extended;
    Y: Extended;
    Zone: Integer;
    Band: Char;
    Hemisphere: Char;
    function FromString(aCoord: String; aSeparator: TSeparator = spSemicolon): Boolean;
    function ToString(WithZone: Boolean = False; aSeparator: TSeparator = spNone): String;
  end;

type
  TSiteRank = (srCountry, srState, srRegion, srMunicipality, srDistrict, srLocality);
  TGazetteerFilter = (gfAll, gfCountries, gfStates, gfRegions, gfCities, gfDistricts, gfLocalities);
  TGazetteerFilters = set of TGazetteerFilter;

type

  { TSite }

  TSite = class(TXolmisRecord)
  protected
    FName: String;
    FAcronym: String;
    FRank: String;
    FParentSiteId: Integer;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FFullName: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAltitude: Double;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer); overload;
    procedure GetData(aDataSet: TDataSet); overload;
    function Diff(aOld: TSite; var aList: TStrings): Boolean;
  published
    property Name: String read FName write FName;
    property Acronym: String read FAcronym write FAcronym;
    property Rank: String read FRank write FRank;
    property ParentSiteId: Integer read FParentSiteId write FParentSiteId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property FullName: String read FFullName write FFullName;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Altitude: Double read FAltitude write FAltitude;
  end;

  { Geographic coordinates conversion routines }

  function RemoveSymbolsDMS(aCoord: String): String;

  { >> returns Map Points }
  function DecimalToDms(aDec: TMapPoint): TDMSPoint;
  function UtmToDms(aUtm: TUTMPoint): TDMSPoint;
  function DmsToDecimal(aDms: TDMSPoint): TMapPoint;
  function UtmToDecimal(aUtm: TUTMPoint): TMapPoint;
  function DecimalToUtm(aDec: TMapPoint): TUTMPoint;
  function DmsToUtm(aDms: TDMSPoint): TUTMPoint;

  { >> returns only Longitude or Latitude }
  function AxisDecToDMS(aCoord: String; aAxis: TMapAxis; WithSymbols: Boolean = False): String; overload;
  function AxisDecToDMS(aCoord: TMapPoint; aAxis: TMapAxis; WithSymbols: Boolean = False): String; overload;
  function AxisDMSToDec(aCoord: String; aAxis: TMapAxis): Extended; overload;
  function AxisDMSToDec(aCoord: TDMS; aAxis: TMapAxis): Extended; overload;

  { Maps and KML }
  procedure SaveToKML(MapPoints: array of TMapPoint; aFile: String);
  procedure ViewMapPoint(aMapPoint: TMapPoint; aZoom: Integer; aMap: TMapView); overload;

  { Dialogs }
  function GeoEditorDlg(aDMS: String; aAxis: TMapAxis; aControl: TControl;
    var aDecimal: Extended): Boolean; overload;
  function GeoEditorDlg(aDMS: TDMS; aAxis: TMapAxis; aControl: TControl; var aDecimal: Extended)
    : Boolean; overload;
  function GeoEditorDlg(aControl: TControl; aDataSet: TDataSet; LongitudeField, LatitudeField: String)
    : Boolean; overload;

implementation

uses cbs_locale, cbs_global, cbs_conversions, cbs_validations, cbs_datacolumns, udm_main, udlg_geoeditor;

function RemoveSymbolsDMS(aCoord: String): String;
begin
  Result := ReplaceRegExpr('[°º''"]\h*', aCoord, ' ');
  //Result := UTF8StringReplace(aCoord, #176, '', [rfReplaceAll]);    // Remove degrees symbol
  //Result := UTF8StringReplace(Result, #186, '', [rfReplaceAll]);    // Remove male ordinal symbol
  //Result := UTF8StringReplace(Result, #39, '', [rfReplaceAll]);     // Remove minutes symbol
  //Result := UTF8StringReplace(Result, '"', '', [rfReplaceAll]);     // Remove seconds symbol
end;

{ Conversion routines >> returns Map Points }

function DecimalToDms(aDec: TMapPoint): TDMSPoint;
var
  f, m: Extended;
  IsNegative: Boolean;
begin
  // Longitude
  f := aDec.X;
  IsNegative := Sign(f) = NegativeValue;
  if IsNegative then
    f := Abs(f);
  with Result.X do
  begin
    Hemisphere := BoolToText(IsNegative, 'W', 'E')[1];
    Degrees := Trunc(f);
    m := Frac(f) * 60;
    Minutes := Trunc(m);
    Seconds := Frac(m) * 60;
    Text := Format('%d %2.2d %s %s', [Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere]);
  end;

  // Latitude
  f := aDec.Y;
  IsNegative := Sign(f) = NegativeValue;
  if IsNegative then
    f := Abs(f);
  with Result.Y do
  begin
    Hemisphere := BoolToText(IsNegative, 'S', 'N')[1];
    Degrees := Trunc(f);
    m := Frac(f) * 60;
    Minutes := Trunc(m);
    Seconds := Frac(m) * 60;
    Text := Format('%d %2.2d %s %s', [Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere]);
  end;
end;

function UtmToDms(aUtm: TUTMPoint): TDMSPoint;
begin
  Result := DecimalToDms(UtmToDecimal(aUtm));
end;

function DmsToDecimal(aDms: TDMSPoint): TMapPoint;
begin
  // Longitude
  with aDms.X do
  begin
    Result.X := ((Seconds + (Minutes * 60)) / 3600) + Degrees;
    if Hemisphere = 'W' then
      Result.X := -Abs(Result.X);
  end;

  // Latitude
  with aDms.Y do
  begin
    Result.Y := ((Seconds + (Minutes * 60)) / 3600) + Degrees;
    if Hemisphere = 'S' then
      Result.Y := -Abs(Result.Y);
  end;
end;

function UtmToDecimal(aUtm: TUTMPoint): TMapPoint;
var
  X, Y, DeltaLambda, A, Xi, Eta, Ni, Zeta, A1, A2, J2, J4, J6, Alfa, Beta, Gamma, Bfi: Extended;
  MeridCentral: Integer;
  E2S, cPolar, Fi, FiRad, b, SenhXi, Tau: Extended;
  IsNegative: Boolean;
begin
  // Eccent:= (Sqrt(sqr(datumA)-sqr(datumB))/datumA);
  // Eccent2:= (Sqrt(sqr(datumA)-sqr(datumB))/datumB);
  E2S := Sqr(Sqrt(Sqr(datumA) - Sqr(datumB)) / datumB); // 2a Eccentricity
  cPolar := +(Sqr(datumA)) / datumB;                    // Polar radius of curvature

  IsNegative:= Ord(aUtm.Band) > Ord('N');
  X := aUtm.X;
  Y := aUtm.Y;
  MeridCentral := 6 * aUtm.Zone - 183;
  if IsNegative then
    Y := Y - N0;

  Fi := Y / (6366197.724 * k0);
  Ni := (cPolar / Power(1 + E2S * Sqr(cos(Fi)), 0.5)) * k0;
  A := (X - E0) / Ni;
  A1 := sin(2 * Fi);
  A2 := A1 * Sqr(cos(Fi));
  J2 := Fi + (A1 / 2);
  J4 := ((3 * J2) + A2) / 4;
  J6 := (5 * J4 + A2 * Sqr(cos(Fi))) / 3;
  Alfa := (3 / 4) * E2S;
  Beta := (5 / 3) * Sqr(Alfa);
  Gamma := (35 / 27) * Power(Alfa, 3);
  Bfi := k0 * cPolar * (Fi - (Alfa * J2) + (Beta * J4) - (Gamma * J6));
  b := (Y - Bfi) / Ni;
  Zeta := ((E2S * Sqr(A)) / 2) * Sqr(cos(Fi));
  Xi := A * (1 - (Zeta / 3));
  Eta := (b * (1 - Zeta)) + Fi;
  SenhXi := (Exp(Xi) - Exp(-Xi)) / 2;
  DeltaLambda := ArcTan(SenhXi / cos(Eta));
  Tau := ArcTan(cos(DeltaLambda) * Tan(Eta));
  FiRad := Fi + (1 + E2S * Sqr(cos(Fi)) - (3 / 2) * E2S * sin(Fi) * cos(Fi) * (Tau - Fi)) *
    (Tau - Fi);

  Result.Y := +(FiRad / pi) * 180;
  Result.X := +((DeltaLambda / pi) * 180) + MeridCentral;
end;

function DecimalToUtm(aDec: TMapPoint): TUTMPoint;
var
  RadX, RadY, DeltaLambda, Xi, Eta, Ni, Zeta, A2, J2, J4, J6, Alfa, Beta, Gamma, Bfi: Extended;
  MeridHuso: Integer;
  E2S, cPolar: Extended;
  IsNegative: Boolean;
begin
  // Eccent:= (Sqrt(sqr(datumA)-sqr(datumB))/datumA);
  // Eccent2:= (Sqrt(sqr(datumA)-sqr(datumB))/datumB);
  E2S := Sqr(Sqrt(Sqr(datumA) - Sqr(datumB)) / datumB); // 2a Eccentricity
  cPolar := +(Sqr(datumA)) / datumB;                    // Polar radius of curvature

  IsNegative := Sign(aDec.Y) = NegativeValue;
  Result.Band := UtmBands[Trunc(aDec.Y / 8)];
  Result.Zone := Trunc((aDec.X / 6) + 31);

  RadX := (aDec.X * pi) / 180;   // Longitude in radians
  RadY := (aDec.Y * pi) / 180;   // Latitude in radians

  MeridHuso := 6 * Result.Zone - 183;
  DeltaLambda := +(RadX) - ((MeridHuso * pi) / 180);
  Xi := (0.5) * ln((1 + (cos(RadY) * sin(DeltaLambda))) / (1 - (cos(RadY) * sin(DeltaLambda))));
  Eta := ArcTan(Tan(RadY) / cos(DeltaLambda)) - RadY;
  Ni := (cPolar / Power(1 + E2S * Sqr(cos(RadY)), 0.5)) * k0;
  Zeta := (E2S / 2) * Sqr(Xi) * Sqr(cos(RadY));
  A2 := +(sin(2 * RadY)) * Sqr(cos(RadY));
  J2 := RadY + (sin(2 * RadY) / 2);
  J4 := ((3 * J2) + A2) / 4;
  J6 := (5 * J4 + A2 * Sqr(cos(RadY))) / 3;
  Alfa := (3 / 4) * E2S;
  Beta := (5 / 3) * Sqr(Alfa);
  Gamma := (35 / 27) * Power(Alfa, 3);
  Bfi := k0 * cPolar * (RadY - (Alfa * J2) + (Beta * J4) - (Gamma * J6));

  Result.X := Xi * Ni * (1 + Zeta / 3) + E0;
  Result.Y := Eta * Ni * (1 + Zeta) + Bfi;
  if IsNegative then
    Result.Y := Result.Y + N0;
end;

function DmsToUtm(aDms: TDMSPoint): TUTMPoint;
begin
  Result := DecimalToUtm(DmsToDecimal(aDms));
end;

{ Conversion routines >> returns only Longitude or Latitude }

function AxisDecToDMS(aCoord: String; aAxis: TMapAxis; WithSymbols: Boolean): String;
var
  d: Integer;
  f, m, s: Extended;
  h: String;
  IsNegative: Boolean;
begin
  Result := '';

  if (Length(aCoord) = 0) then
    Exit;

  if TryStrToFloat(aCoord, f) then
  begin
    LogError(Format('%s is not a valid coordinate', [aCoord]));
    Exit;
  end;

  IsNegative := Sign(f) = NegativeValue;
  case aAxis of
    maBoth: { nothing } ;
    maLongitude: h := BoolToText(IsNegative, 'W', 'E');
    maLatitude: h := BoolToText(IsNegative, 'S', 'N');
  end;
  if IsNegative then
    f := Abs(f);

  d := Trunc(f);
  m := Frac(f) * 60;   { Frac() returns the fractional part of a float number }
  s := Frac(m) * 60;

  if WithSymbols then
    Result := Format('%d'#176' %2.2d'' %s" %s',[d, Trunc(m), FormatFloat('00.00', s), h])
  else
    Result := Format('%d %2.2d %s %s',[d, Trunc(m), FormatFloat('00.00', s), h]);

  {$IFDEF DEBUG}
  LogDebug('Converted decimal to DMS axis: ' + aCoord + ' -> ' + Result);
  {$ENDIF}
end;

function AxisDecToDMS(aCoord: TMapPoint; aAxis: TMapAxis; WithSymbols: Boolean): String;
var
  f, m: Extended;
  IsNegative: Boolean;
  aDMS: TDMSPoint;
begin
  Result := '';

  // Longitude
  if (aAxis = maLongitude) or (aAxis = maBoth) then
  begin
    f := aCoord.X;
    IsNegative := Sign(f) = NegativeValue;
    if IsNegative then
      f := Abs(f);

    with aDMS.X do
    begin
      Hemisphere := BoolToText(IsNegative, 'W', 'E')[1];
      Degrees := Trunc(f);
      m := Frac(f) * 60;
      Minutes := Trunc(m);
      Seconds := Frac(m) * 60;
      Text := Format('%d %2.2d %s %s', [Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere]);
    end;
  end;

  // Latitude
  if (aAxis = maLatitude) or (aAxis = maBoth) then
  begin
    f := aCoord.Y;
    IsNegative := Sign(f) = NegativeValue;
    if IsNegative then
      f := Abs(f);

    with aDMS.Y do
    begin
      Hemisphere := BoolToText(IsNegative, 'S', 'N')[1];
      Degrees := Trunc(f);
      m := Frac(f) * 60;
      Minutes := Trunc(m);
      Seconds := Frac(m) * 60;
      Text := Format('%d %2.2d %s %s', [Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere]);
    end;
  end;

  case aAxis of
    maBoth:
      Result := aDMS.ToString(WithSymbols);
    maLongitude:
      Result := aDMS.X.ToString(WithSymbols);
    maLatitude:
      Result := aDMS.Y.ToString(WithSymbols);
  end;
  {$IFDEF DEBUG}
  LogDebug('Converted Decimal to DMS axis: ' + aCoord.ToString + ' -> ' + Result);
  {$ENDIF}
end;

function AxisDMSToDec(aCoord: String; aAxis: TMapAxis): Extended; overload;
var
  Sx, DecX: Extended;
  H: String;
  Dx, Mx, Hx: Integer;
  DMSx: TStringDynArray;
  IsNegative: Boolean;
begin
  Result:= 500.0;

  DMSx := SplitString(RemoveSymbolsDMS(aCoord), ' ');
  if not TryStrToInt(DMSx[0], Dx) then
  begin
    LogError(Format('%s is not a valid coordinate', [aCoord]));
    Exit;
  end;
  if not TryStrToInt(DMSx[1], Mx) then
  begin
    LogError(Format('%s is not a valid coordinate', [aCoord]));
    Exit;
  end;

  if Length(DMSx) = 3 then
  begin
    if not TryStrToFloat(DMSx[2], Sx) then
    begin
      Sx := 0.0;
      H := DMSx[2];
    end
    else
    begin
      LogError(Format('The coordinate %s does not have a hemisphere', [aCoord]));
      Exit;
    end;
  end
  else
  begin
    if not TryStrToFloat(DMSx[2], Sx) then
    begin
      LogError(Format('%s is not a valid coordinate', [aCoord]));
      Exit;
    end;
    H := DMSx[3];
  end;

  if not CharInSet(H[1], ['N', 'S', 'E', 'W']) then
  begin
    LogError(Format('The coordinate %s does not have a valid hemisphere', [aCoord]));
    Exit;
  end;

  case aAxis of
    maBoth: { nothing } ;
    maLongitude: IsNegative := TextToBool(H, 'W', 'E');
    maLatitude: IsNegative := TextToBool(H, 'S', 'N');
  end;

  DecX := Dx + ((Sx + (Mx * 60)) / 3600);
  if IsNegative then
    DecX := -Abs(DecX);

  Result := DecX;
  {$IFDEF DEBUG}
  LogDebug('Converted DMS to Decimal axis: ' + aCoord + ' -> ' + FloatToStr(Result));
  {$ENDIF}
end;

function AxisDMSToDec(aCoord: TDMS; aAxis: TMapAxis): Extended; overload;
var
  DecX: Extended;
  IsNegative: Boolean;
begin
  case aAxis of
    maBoth: { nothing } ;
    maLongitude: IsNegative := TextToBool(aCoord.Hemisphere, 'W', 'E');
    maLatitude: IsNegative := TextToBool(aCoord.Hemisphere, 'S', 'N');
  end;

  DecX := aCoord.Degrees + ((aCoord.Seconds + (aCoord.Minutes * 60)) / 3600);
  if IsNegative then
    DecX := -Abs(DecX);

  Result := DecX;
  {$IFDEF DEBUG}
  LogDebug('Converted DMS to Decimal axis: ' + aCoord.Text + ' -> ' + FloatToStr(Result));
  {$ENDIF}
end;

procedure SaveToKML(MapPoints: array of TMapPoint; aFile: String);
var
  Kml, Coords: TStrings;
  i: Integer;
begin
  Coords := TStringList.Create;
  Kml := TStringList.Create;
  with Kml do
  begin
    Add('<?xml version="1.0" encoding="UTF-8"?>');
    Add('<kml xmlns="http://www.opengis.net/kml/2.2">');
    Add('<Document>');
    Add('  <name>Xolmis</name>');
    for i := 0 to Length(MapPoints) - 1 do
    begin
      Add('  <Placemark>');
      Add('    <name>' + MapPoints[i].Name + '</name>');
      // Add('    <description>'+''+'</description>');
      Add('    <Point>');
      Add('      <coordinates>' + MapPoints[i].ToString + '</coordinates>');
      Add('    </Point>');
      Add('  </Placemark>');
      Coords.Add(MapPoints[i].ToString);
    end;
    Add('</Document>');
    Add('</kml>');
    SaveToFile(aFile);
    Free;
  end;
  {$IFDEF DEBUG}
  LogDebug('Exported KML file: ' + aFile);
  {$ENDIF}
  Coords.Free;
end;

procedure ViewMapPoint(aMapPoint: TMapPoint; aZoom: Integer; aMap: TMapView);
begin

  {$IFDEF DEBUG}
  LogDebug('View map with coordinates ' + aMapPoint.ToString);
  {$ENDIF}

end;

function GeoEditorDlg(aDMS: String; aAxis: TMapAxis; aControl: TControl; var aDecimal: Extended): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  if aAxis = maLatitude then
    LogDebug('OPEN DIALOG: GeoEditor (Latitude)')
  else
    LogDebug('OPEN DIALOG: GeoEditor (Longitude)');
  {$ENDIF}
  dlgGeoEditor := TdlgGeoEditor.Create(nil);
  with dlgGeoEditor do
  try
    Axis := aAxis;
    Linha := aDMS;
    PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      aDecimal := CoordDec;
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoEditor);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: GeoEditor');
    {$ENDIF}
  end;
end;

function GeoEditorDlg(aDMS: TDMS; aAxis: TMapAxis; aControl: TControl; var aDecimal: Extended): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  if aAxis = maLatitude then
    LogDebug('OPEN DIALOG: GeoEditor (Latitude)')
  else
    LogDebug('OPEN DIALOG: GeoEditor (Longitude)');
  {$ENDIF}

  dlgGeoEditor := TdlgGeoEditor.Create(nil);
  with dlgGeoEditor do
  try
    Axis := aAxis;
    Linha := '';
    CoordDMS := aDMS;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      aDecimal := CoordDec;
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoEditor);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: GeoEditor');
    {$ENDIF}
  end;
end;

function GeoEditorDlg(aControl: TControl; aDataSet: TDataSet; LongitudeField, LatitudeField: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: GeoEditor');
  {$ENDIF}

  dlgGeoEditor := TdlgGeoEditor.Create(nil);
  with dlgGeoEditor do
  try
    PointStr := EmptyStr;
    DecimalPoint.FromString(aDataSet.FieldByName(LongitudeField).AsString + '; ' +
      aDataSet.FieldByName(LatitudeField).AsString);
    if Assigned(aControl) then
    begin
      //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
      PControl := aControl.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    end
    else
      Position := poScreenCenter;
    //SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      aDataSet.FieldByName(LongitudeField).AsFloat := DecimalPoint.X;
      aDataSet.FieldByName(LatitudeField).AsFloat := DecimalPoint.Y;
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoEditor);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: GeoEditor');
    {$ENDIF}
  end;
end;

{ TSite }

constructor TSite.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TSite.Clear;
begin
  inherited;
  FName := EmptyStr;
  FAcronym := EmptyStr;
  FRank := EmptyStr;
  FParentSiteId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FFullName := EmptyStr;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FAltitude := 0.0;
end;

procedure TSite.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT * FROM gazetteer');
    Add('WHERE site_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
    begin
      FId := FieldByName('site_id').AsInteger;
      FName := FieldByName('site_name').AsString;
      FAcronym := FieldByName('site_acronym').AsString;
      FRank := FieldByName('site_rank').AsString;
      FParentSiteId := FieldByName('parent_site_id').AsInteger;
      FMunicipalityId := FieldByName('municipality_id').AsInteger;
      FStateId := FieldByName('state_id').AsInteger;
      FCountryId := FieldByName('country_id').AsInteger;
      FFullName := FieldByName('full_name').AsString;
      FLatitude := FieldByName('latitude').AsFloat;
      FLongitude := FieldByName('longitude').AsFloat;
      FAltitude := FieldByName('altitude').AsFloat;
      FInsertDate := FieldByName('insert_date').AsDateTime;
      FUserInserted := FieldByName('user_inserted').AsInteger;
      FUpdateDate := FieldByName('update_date').AsDateTime;
      FUserUpdated := FieldByName('user_updated').AsInteger;
      FExported := FieldByName('exported_status').AsBoolean;
      FMarked := FieldByName('marked_status').AsBoolean;
      FActive := FieldByName('active_status').AsBoolean;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSite.GetData(aDataSet: TDataSet);
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('site_id').AsInteger;
    FName := FieldByName('site_name').AsString;
    FAcronym := FieldByName('site_acronym').AsString;
    FRank := FieldByName('site_rank').AsString;
    FParentSiteId := FieldByName('parent_site_id').AsInteger;
    FMunicipalityId := FieldByName('municipality_id').AsInteger;
    FStateId := FieldByName('state_id').AsInteger;
    FCountryId := FieldByName('country_id').AsInteger;
    FFullName := FieldByName('full_name').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAltitude := FieldByName('altitude').AsFloat;
    FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

function TSite.Diff(aOld: TSite; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscSiteName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Acronym, FAcronym, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.Rank, FRank, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentSiteId, aOld.ParentSiteId, FParentSiteId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAltitude, aOld.Altitude, FAltitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscMunicipalityID, aOld.MunicipalityId, FMunicipalityId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscStateID, aOld.StateId, FStateId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscCountryID, aOld.CountryId, FCountryId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

{ TUTMPoint }

function TUTMPoint.FromString(aCoord: String; aSeparator: TSeparator): Boolean;
var
  Z: String;
  Sep: set of Char;
begin
  Result := False;
  Sep := [StrSeparators[aSeparator]];

  if (WordCount(aCoord, Sep) < 2) then
    Exit;

  { Zone X Y }
  if (WordCount(aCoord, Sep) = 3) then
  begin
    Z := StringReplace(ExtractWord(1, aCoord, Sep), ' ', '', [rfReplaceAll]);
    if not TryStrToInt(Trim(Copy(Z, 1, Length(Z) - 1)), Zone) then
      Exit;
    Band := Z[Length(Z)];

    if not TryStrToFloat(Trim(ExtractWord(2, aCoord, Sep)), X) then
      Exit;
    if not TryStrToFloat(Trim(ExtractWord(3, aCoord, Sep)), Y) then
      Exit;
  end
  else
  { X Y }
  begin
    if not TryStrToFloat(Trim(ExtractWord(1, aCoord, Sep)), X) then
      Exit;
    if not TryStrToFloat(Trim(ExtractWord(2, aCoord, Sep)), Y) then
      Exit;
  end;

  Result := True;
end;

function TUTMPoint.ToString(WithZone: Boolean; aSeparator: TSeparator): String;
var
  sX, sY, sep: String;
begin
  sep:= StrSeparators[aSeparator];

  if aSeparator = spComma then
  begin
    sX := StringReplace(FormatFloat('#####0.000', X), ',', '.', []);
    sY := StringReplace(FormatFloat('#######0.000', Y), ',', '.', []);
  end
  else
  begin
    sX := FormatFloat('#####0.000', X);
    sY := FormatFloat('#######0.000', Y);
  end;

  if WithZone then
    Result := Format('%d %s%s %s%s %s',[Zone, Band, sep, sX, sep, sY])
  else
    Result := Format('%s%s %s', [sX, sep, sY]);
end;

{ TDMSPoint }

function TDMSPoint.FromString(aCoord: String; aSeparator: TSeparator): Boolean;
var
  Sep: set of Char;
begin
  Result := False;
  Sep := [StrSeparators[aSeparator]];
  aCoord := RemoveSymbolsDMS(aCoord);

  if (WordCount(aCoord, Sep) <> 2) then
    Exit;

  { Longitude }
  if not X.FromString(ExtractWord(1, aCoord, Sep)) then
    Exit;
  { Latitude }
  if not Y.FromString(ExtractWord(2, aCoord, Sep)) then
    Exit;

  Result := True;
end;

function TDMSPoint.ToString(WithSymbols: Boolean; aSeparator: TSeparator): String;
var
  sX, sY, sep: String;
begin
  sep:= StrSeparators[aSeparator];

  if aSeparator = spComma then
  begin
    sX := StringReplace(X.ToString(WithSymbols), ',', '.', []);
    sY := StringReplace(Y.ToString(WithSymbols), ',', '.', []);
  end
  else
  begin
    sX := X.ToString(WithSymbols);
    sY := Y.ToString(WithSymbols);
  end;

  Result := Format('%s%s %s', [sX, sep, sY]);
end;

{ TDMS }

function TDMS.FromString(aCoord: String): Boolean;
begin
  Result := False;
  aCoord := RemoveSymbolsDMS(aCoord);

  if (WordCount(aCoord, [' ']) < 3) then
    Exit;

  if not TryStrToInt(ExtractWord(1, aCoord, [' ']), Degrees) then
    Exit;
  if not TryStrToInt(ExtractWord(2, aCoord, [' ']), Minutes) then
    Exit;

  { Degrees Minutes Seconds Hemisphere }
  if (WordCount(aCoord, [' ']) = 4) then
  begin
    if not TryStrToFloat(ExtractWord(3, aCoord, [' ']), Seconds) then
      Exit;
    if not CharInSet(ExtractWord(4, aCoord, [' '])[1], ['N', 'S', 'E', 'W']) then
      Exit;

    Hemisphere := ExtractWord(4, aCoord, [' '])[1];
  end
  else
  { Degrees Minutes Hemisphere }
  if (WordCount(aCoord, [' ']) = 3) then
  begin
    if not CharInSet(ExtractWord(3, aCoord, [' '])[1], ['N', 'S', 'E', 'W']) then
      Exit;

    Hemisphere := ExtractWord(3, aCoord, [' '])[1];
  end;

  Text := Format('%d %2.2d %s %s', [Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere]);

  Result := True;
end;

function TDMS.ToString(WithSymbols: Boolean): String;
begin
  if WithSymbols then
    Result := Format('%d'#176' %2.2d'' %s" %s',[Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere])
  else
    Result := Format('%d %2.2d %s %s',[Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere]);
end;

{ TMapPoint }

function TMapPoint.FromString(aCoord: String; aSeparator: TSeparator): Boolean;
var
  Sep: set of Char;
begin
  Result := False;
  Sep := [StrSeparators[aSeparator]];

  if (WordCount(aCoord, Sep) <> 2) then
    Exit;

  if not TryStrToFloat(Trim(ExtractWord(1, aCoord, Sep)), X) then
    Exit;
  if not TryStrToFloat(Trim(ExtractWord(2, aCoord, Sep)), Y) then
    Exit;

  Result := True;
end;

function TMapPoint.ToString(aSeparator: TSeparator): String;
var
  sX, sY, sep: String;
begin
  sep:= StrSeparators[aSeparator];

  if aSeparator = spComma then
  begin
    sX := StringReplace(FloatToStr(X), ',', '.', []);
    sY := StringReplace(FloatToStr(Y), ',', '.', []);
  end
  else
  begin
    sX := FloatToStr(X);
    sY := FloatToStr(Y);
  end;

  Result := Format('%s%s %s', [sX, sep, sY]);
end;

end.
