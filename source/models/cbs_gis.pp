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
  Classes, Types, SysUtils, Math, LazUTF8, StrUtils, RegExpr, XMLRead, XMLWrite, DOM, Zipper, fgl,
  fpjson, jsonparser, CSVDocument, DateUtils, TypInfo,
  { VCL }
  Forms, Controls, ExtCtrls, laz.VirtualTrees, mvMapViewer, EditBtn,
  { Data }
  DB, SQLDB, cbs_record_types, cbs_datatypes;

const
  datumA: Extended = 6378137;          // equatorial radius (in meters), semi major axis
  datumB: Extended = 6356752.31424518; // semi minor axis
  N0: Integer = 10000000;              // in meters (10000 km)
  k0: Double = 0.9996;
  E0: Integer = 500000;                // in meters (500 km)

type
  TMapCoordinateType = (mcDecimal, mcDMS, mcUTM);
  TMapAxis = (maBoth, maLongitude, maLatitude);
  TMapHemisphere = (mhNorth, mhSouth, mhEast, mhWest);

const
  GlobeHemispheres: array[TMapHemisphere] of Char = ('N', 'S', 'E', 'W');
  UtmBands: array[-9..10] of Char = ('C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X');
  DmsSymbols: set of Char = [#176, #186, #39, #146, #180, #34, #148];  { °, º, ', ’, ´, ", ” }

type

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

  TMercatorPoint = record
    X, Y: Extended;
  end;

  { TMapPointObject }

  TMapPointObject = class
  private
    FData: TMapPoint;
  public
    constructor Create(aX, aY: Extended; aName: String = '');
    property Data: TMapPoint read FData write FData;
  end;

  TMapPointList = specialize TFPGObjectList<TMapPointObject>;

type
  TSiteRank = (srNone, srCountry, srState, srRegion, srMunicipality, srDistrict, srLocality);
  TGazetteerFilter = (gfAll, gfCountries, gfStates, gfRegions, gfCities, gfDistricts, gfLocalities);
  TGazetteerFilters = set of TGazetteerFilter;

  TCoordinatePrecision = (cpEmpty = -1, cpExact, cpApproximated, cpReference);

const
  SiteRankStr: array[TSiteRank] of String = ('', 'P', 'E', 'R', 'M', 'D', 'L');
  CoordinatePrecisionStr: array[TCoordinatePrecision] of String = ('', 'E', 'A', 'R');

type

  { TSite }

  TSite = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FRank: TSiteRank;
    FParentSiteId: Integer;
    FMunicipalityId: Integer;
    FStateId: Integer;
    FCountryId: Integer;
    FFullName: String;
    FEbirdName: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAltitude: Double;
    FLanguage: String;
    FDescription: String;
    FNotes: String;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TSite; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TSite);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property Rank: TSiteRank read FRank write FRank;
    property ParentSiteId: Integer read FParentSiteId write FParentSiteId;
    property MunicipalityId: Integer read FMunicipalityId write FMunicipalityId;
    property StateId: Integer read FStateId write FStateId;
    property CountryId: Integer read FCountryId write FCountryId;
    property FullName: String read FFullName write FFullName;
    property EbirdName: String read FEbirdName write FEbirdName;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Altitude: Double read FAltitude write FAltitude;
    property Language: String read FLanguage write FLanguage;
    property Description: String read FDescription write FDescription;
    property Notes: String read FNotes write FNotes;
  end;

  { TPoi }

  TPoi = class(TXolmisRecord)
  protected
    FSampleDate: TDate;
    FSampleTime: TTime;
    FPoiName: String;
    FLongitude: Extended;
    FLatitude: Extended;
    FAltitude: Double;
    FObserverId: Integer;
    FTaxonId: Integer;
    FIndividualId: Integer;
    FSightingId: Integer;
    FSurveyId: Integer;
  public
    constructor Create(aValue: Integer = 0);
    procedure Clear; override;
    procedure GetData(aKey: Integer);
    procedure LoadFromDataSet(aDataSet: TDataSet);
    function Diff(aOld: TPoi; var aList: TStrings): Boolean;
    procedure Insert;
    procedure Update;
    procedure Save;
    procedure Delete;
    procedure Copy(aFrom: TPoi);
    function ToJSON: String;
    function Find(const FieldName: String; const Value: Variant): Boolean;
  published
    property SampleDate: TDate read FSampleDate write FSampleDate;
    property SampleTime: TTime read FSampleTime write FSampleTime;
    property PoiName: String read FPoiName write FPoiName;
    property Longitude: Extended read FLongitude write FLongitude;
    property Latitude: Extended read FLatitude write FLatitude;
    property Altitude: Double read FAltitude write FAltitude;
    property ObserverId: Integer read FObserverId write FObserverId;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property IndividualId: Integer read FIndividualId write FIndividualId;
    property SightingId: Integer read FSightingId write FSightingId;
    property SurveyId: Integer read FSurveyId write FSurveyId;

  end;

var
  SitePropsDict: specialize TFPGMap<String, String>;
  PoiPropsDict: specialize TFPGMap<String, String>;

  { Classes helpers }
  procedure InitSitePropsDict;
  procedure InitPoiPropsDict;

  { Geographic coordinates conversion routines }

  function RemoveSymbolsDMS(aCoord: String): String;

  { >> returns Map Points }
  function DecimalToDms(aDec: TMapPoint): TDMSPoint;
  function UtmToDms(aUtm: TUTMPoint): TDMSPoint;
  function DmsToDecimal(aDms: TDMSPoint): TMapPoint;
  function UtmToDecimal(aUtm: TUTMPoint): TMapPoint;
  function DecimalToUtm(aDec: TMapPoint): TUTMPoint;
  function DmsToUtm(aDms: TDMSPoint): TUTMPoint;
  function DecimalToMercator(aDec: TMapPoint): TMercatorPoint;
  function MercatorToDecimal(aMerc: TMercatorPoint): TMapPoint;

  { >> returns only Longitude or Latitude }
  function AxisDecToDMS(aCoord: String; aAxis: TMapAxis; WithSymbols: Boolean = False): String; overload;
  function AxisDecToDMS(aCoord: TMapPoint; aAxis: TMapAxis; WithSymbols: Boolean = False): String; overload;
  function AxisDMSToDec(aCoord: String; aAxis: TMapAxis): Extended; overload;
  function AxisDMSToDec(aCoord: TDMS; aAxis: TMapAxis): Extended; overload;

  { Maps and KML }
  procedure AddKMLPoint(Doc: TXMLDocument; RootNode: TDOMNode; aMapPoint: TMapPoint);
  procedure SavePointsToKML(MapPoints: TMapPointList; aFileName: String; Compressed: Boolean = False);
  procedure SavePointsToGPX(MapPoints: TMapPointList; aFileName: String);
  procedure SavePointsToCSV(MapPoints: TMapPointList; aFileName: String);
  procedure SavePointsToGeoJSON(MapPoints: TMapPointList; aFileName: String);
  procedure LoadKMLPoints(const aFileName: String);
  procedure LoadGPXPoints(const aFileName: String);
  procedure LoadCSVPoints(const aFileName: String);
  procedure LoadGeoJSONPoints(const aFileName: String);
  function ReadGeoJSONToString(const FileName: String): String;
  procedure DBToMapPoints(const aTableType: TTableType; aDataSet: TDataSet; ClearList: Boolean; var aMapPointList: TMapPointList);
  procedure ViewMapPoint(aMapPoint: TMapPoint; aZoom: Integer; aMap: TMapView);

  { Dialogs }
  function GeoAssistDlg(aDMS: String; aAxis: TMapAxis; aControl: TControl;
    var aDecimal: Extended): Boolean; overload;
  function GeoAssistDlg(aDMS: TDMS; aAxis: TMapAxis; aControl: TControl; var aDecimal: Extended)
    : Boolean; overload;
  function GeoAssistDlg(aControl: TControl; aDataSet: TDataSet; LongitudeField, LatitudeField: String)
    : Boolean; overload;
  function GeoAssistDlg(aControl: TControl; LongitudeEdit, LatitudeEdit: TCustomEditButton)
    : Boolean; overload;

implementation

uses
  cbs_locale, cbs_global, cbs_users, cbs_conversions, cbs_validations, cbs_datacolumns, cbs_setparam,
  udm_main, udlg_geoassist;

procedure InitSitePropsDict;
begin
  if Assigned(SitePropsDict) then
    Exit;

  SitePropsDict := specialize TFPGMap<String, String>.Create;
  SitePropsDict.Add('Name', rscName);
  SitePropsDict.Add('Abbreviation', rscAbbreviation);
  SitePropsDict.Add('Rank', rscType);
  SitePropsDict.Add('ParentSiteId', rscParentSiteID);
  SitePropsDict.Add('MunicipalityId', rscMunicipalityID);
  SitePropsDict.Add('StateId', rscStateID);
  SitePropsDict.Add('CountryId', rscCountryID);
  SitePropsDict.Add('FullName', rscFullName);
  SitePropsDict.Add('EbirdName', rscEBirdName);
  SitePropsDict.Add('Longitude', rscLongitude);
  SitePropsDict.Add('Latitude', rscLatitude);
  SitePropsDict.Add('Altitude', rscAltitude);
  SitePropsDict.Add('Language', rscLanguage);
  SitePropsDict.Add('Description', rscDescription);
  SitePropsDict.Add('Notes', rscNotes);
end;

procedure InitPoiPropsDict;
begin
  if Assigned(PoiPropsDict) then
    Exit;

  PoiPropsDict := specialize TFPGMap<String, String>.Create;
  PoiPropsDict.Add('SampleDate', rscDate);
  PoiPropsDict.Add('SampleTime', rscTime);
  PoiPropsDict.Add('PoiName', rscName);
  PoiPropsDict.Add('Longitude', rscLongitude);
  PoiPropsDict.Add('Latitude', rscLatitude);
  PoiPropsDict.Add('Altitude', rscAltitude);
  PoiPropsDict.Add('ObserverId', rscObserverID);
  PoiPropsDict.Add('TaxonId', rscTaxonID);
  PoiPropsDict.Add('IndividualId', rscIndividualID);
  PoiPropsDict.Add('SightingId', rscSightingID);
  PoiPropsDict.Add('SurveyId', rscSurveyID);
end;

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

procedure AddKMLPoint(Doc: TXMLDocument; RootNode: TDOMNode; aMapPoint: TMapPoint);
var
  PlacemarkNode, nameNode, PointNode, CoordNode: TDOMNode;
begin
  // Create node <Placemark>
  PlacemarkNode := Doc.CreateElement('Placemark');
  RootNode.AppendChild(PlacemarkNode);
  // Create node <name> within <Placemark>
  nameNode := Doc.CreateElement('name');
  nameNode.TextContent := aMapPoint.Name;
  PlacemarkNode.AppendChild(nameNode);

  // Create node <Point> within <Placemark>
  PointNode := Doc.CreateElement('Point');
  PlacemarkNode.AppendChild(PointNode);

  // Create node <coordinates> within <Point>
  CoordNode := Doc.CreateElement('coordinates');
  CoordNode.TextContent := aMapPoint.ToString(spComma);
  PointNode.AppendChild(CoordNode);
end;

procedure SavePointsToKML(MapPoints: TMapPointList; aFileName: String; Compressed: Boolean);
var
  Doc: TXMLDocument;
  RootNode, DocNode, nameNode: TDOMNode;
  Zip: TZipper;
  i: Integer;
begin
  // Create a new XML document
  Doc := TXMLDocument.Create;
  try
    // Create the root node <kml>
    RootNode := Doc.CreateElement('kml');
    TDOMElement(RootNode).SetAttribute('xmlns', 'http://www.opengis.net/kml/2.2');
    Doc.AppendChild(RootNode);
    RootNode := Doc.DocumentElement;
    // Create node <Document> within the root node
    DocNode := Doc.CreateElement('Document');
    RootNode.AppendChild(DocNode);
    // Create node <name> within <Document>
    nameNode := Doc.CreateElement('name');
    nameNode.TextContent := 'Xolmis';
    DocNode.AppendChild(nameNode);

    // Add multiple points
    for i := 0 to (MapPoints.Count - 1) do
    begin
      AddKMLPoint(Doc, DocNode, MapPoints[i].Data);
    end;

    // Save the XML document to file
    WriteXMLFile(Doc, aFileName);
    LogDebug('Exported KML file: ' + aFileName);
  finally
    Doc.Free;
  end;

  if Compressed then
  begin
    // Compress the KML file to a KMZ
    Zip := TZipper.Create;
    try
      Zip.FileName := ChangeFileExt(aFileName, '.kmz');
      Zip.Entries.AddFileEntry(aFileName);
      Zip.ZipAllFiles;
    finally
      Zip.Free;
      DeleteFile(aFileName); // Remove the KML file
    end;
  end;
end;

procedure SavePointsToGPX(MapPoints: TMapPointList; aFileName: String);
var
  Doc: TXMLDocument;
  RootNode, NameNode, WptNode: TDOMNode;
  i: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('gpx');
    TDOMElement(RootNode).SetAttribute('version', '1.1');
    TDOMElement(RootNode).SetAttribute('creator', 'Xolmis');
    Doc.AppendChild(RootNode);

    for i := 0 to (MapPoints.Count - 1) do
    begin
      WptNode := Doc.CreateElement('wpt');
      TDOMElement(WptNode).SetAttribute('lat', StringReplace(FloatToStr(MapPoints[i].Data.Y), FormatSettings.DecimalSeparator, '.', []));
      TDOMElement(WptNode).SetAttribute('lon', StringReplace(FloatToStr(MapPoints[i].Data.X), FormatSettings.DecimalSeparator, '.', []));

      NameNode := Doc.CreateElement('name');
      NameNode.TextContent := MapPoints[i].Data.Name;
      WptNode.AppendChild(NameNode);

      RootNode.AppendChild(WptNode);
    end;

    WriteXMLFile(Doc, aFileName);
  finally
    Doc.Free;
  end;
end;

procedure SavePointsToCSV(MapPoints: TMapPointList; aFileName: String);
var
  CSV: TCSVDocument;
  i: Integer;
begin
  CSV := TCSVDocument.Create;
  try
    CSV.Delimiter := StrSeparators[spSemicolon];
    CSV.AddRow('Name'); // Header
    CSV.AddCell(0, 'Longitude');
    CSV.AddCell(0, 'Latitude');
    for i := 0 to (MapPoints.Count - 1) do
    begin
      CSV.AddRow(MapPoints[i].Data.Name);
      CSV.AddCell(i + 1, FloatToStr(MapPoints[i].Data.X));
      CSV.AddCell(i + 1, FloatToStr(MapPoints[i].Data.Y));
    end;
    CSV.SaveToFile(aFileName);
  finally
    CSV.Free;
  end;
end;

procedure SavePointsToGeoJSON(MapPoints: TMapPointList; aFileName: String);
var
  JSONObject, Geometry, Properties, Feature: TJSONObject;
  Features: TJSONArray;
  JSONString: TStringStream;
  JSONPretty: String;
  i: Integer;

  function PrettyPrintJSON(const JSONStr: string): string;
  var
    Parser: TJSONParser;
    JSONData: TJSONData;
  begin
    Parser := TJSONParser.Create(JSONStr);
    try
      JSONData := Parser.Parse;
      try
        Result := JSONData.FormatJSON([], 2); // Indentation with 2 spaces
      finally
        JSONData.Free;
      end;
    finally
      Parser.Free;
    end;
  end;

begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('type', 'FeatureCollection');
    Features := TJSONArray.Create;
    JSONObject.Add('features', Features);

    for i := 0 to (MapPoints.Count - 1) do
    begin
      Feature := TJSONObject.Create;
      Feature.Add('type', 'Feature');

      Geometry := TJSONObject.Create;
      Geometry.Add('type', 'Point');
      Geometry.Add('coordinates', TJSONArray.Create([MapPoints[i].Data.X, MapPoints[i].Data.Y]));
      Feature.Add('geometry', Geometry);

      Properties := TJSONObject.Create;
      Properties.Add('name', MapPoints[i].Data.Name);
      Feature.Add('properties', Properties);

      Features.Add(Feature);
    end;

    // Format JSON
    JSONPretty := PrettyPrintJSON(JSONObject.AsJSON);

    // Save GeoJSON to file using TStringStream
    JSONString := TStringStream.Create(JSONPretty);
    try
      JSONString.SaveToFile(aFileName);
    finally
      JSONString.Free;
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure LoadKMLPoints(const aFileName: String);
var
  Doc: TXMLDocument;
  PlacemarkNode, nameNode, PointNode, CoordNode: TDOMNode;
  PlacemarkList, nameList, PointList: TDOMNodeList;
  Unzip: TUnZipper;
  aKMLFile: String;
  i: Integer;
  Zipped: Boolean;
  CoordList: TStringList;
begin
  Zipped := False;
  if ExtractFileExt(aFileName) = '.kmz' then
  begin
    Zipped := True;
    // Decompress KMZ file
    Unzip := TUnZipper.Create;
    try
      Unzip.FileName := aFileName;
      Unzip.OutputPath := TempDir;
      Unzip.Examine;
      aKMLFile := ConcatPaths([TempDir, Unzip.Entries[0].ArchiveFileName]); // Assuming that the first file is the KML
      Unzip.UnZipAllFiles;
    finally
      Unzip.Free;
    end;
  end
  else
    aKMLFile := aFileName;

  // Load a KML file
  ReadXMLFile(Doc, aKMLFile);
  DMM.tabGeoBank.DisableControls;
  try
    // Find all <Placemark> nodes
    PlacemarkList := Doc.GetElementsByTagName('Placemark');
    for i := 0 to PlacemarkList.Count - 1 do
    begin
      PlacemarkNode := PlacemarkList[i];
      // Find the node <name> within <Placemark>
      nameNode := PlacemarkNode.FindNode('name');
      // Find the node <Point> within <Placemark>
      PointNode := PlacemarkNode.FindNode('Point');
      if Assigned(PointNode) then
      begin
        //PointNode := PointList[0];
        // Find the node <coordinates> within <Point>
        CoordNode := PointNode.FindNode('coordinates');
        if Assigned(CoordNode) then
        begin
          // Add record to the GeoBank table
          DMM.tabGeoBank.Append;
          if Assigned(nameNode) then
            DMM.tabGeoBank.FieldByName('coordinate_name').AsString := nameNode.TextContent;
          CoordList := TStringList.Create;
          try
            CoordList.CommaText := CoordNode.TextContent;
            DMM.tabGeoBank.FieldByName('longitude').AsFloat := StrToFloat(StringReplace(CoordList[0], '.', FormatSettings.DecimalSeparator, []));
            DMM.tabGeoBank.FieldByName('latitude').AsFloat := StrToFloat(StringReplace(CoordList[1], '.', FormatSettings.DecimalSeparator, []));
            if CoordList.Count = 3 then
              DMM.tabGeoBank.FieldByName('altitude').AsFloat := StrToFloat(StringReplace(CoordList[2], '.', FormatSettings.DecimalSeparator, []));
          finally
            CoordList.Free;
          end;
          DMM.tabGeoBank.Post;
        end;
      end;
    end;
  finally
    DMM.tabGeoBank.EnableControls;
    Doc.Free;
  end;

  // Delete the extracted KML file
  if Zipped then
    DeleteFile(aKMLFile);
end;

procedure LoadGPXPoints(const aFileName: string);
var
  Doc: TXMLDocument;
  RootNode, Node: TDOMNode;
  i: Integer;
begin
  ReadXMLFile(Doc, aFileName);
  DMM.tabGeoBank.DisableControls;
  try
    RootNode := Doc.DocumentElement;
    Node := RootNode.FirstChild;
    while Assigned(Node) do
    begin
      if Node.NodeName = 'wpt' then
      begin
        DMM.tabGeoBank.Append;
        DMM.tabGeoBank.FieldByName('latitude').AsFloat := StrToFloat(StringReplace(Node.Attributes.GetNamedItem('lat').NodeValue, '.', FormatSettings.DecimalSeparator, []));
        DMM.tabGeoBank.FieldByName('longitude').AsFloat := StrToFloat(StringReplace(Node.Attributes.GetNamedItem('lon').NodeValue, '.', FormatSettings.DecimalSeparator, []));
        DMM.tabGeoBank.FieldByName('coordinate_name').AsString := Node.FindNode('name').TextContent;
        DMM.tabGeoBank.Post;
      end;
      Node := Node.NextSibling;
    end;
  finally
    DMM.tabGeoBank.EnableControls;
    Doc.Free;
  end;
end;

procedure LoadCSVPoints(const aFileName: string);
var
  CSV: TCSVDocument;
  i: Integer;
begin
  CSV := TCSVDocument.Create;
  DMM.tabGeoBank.DisableControls;
  try
    CSV.LoadFromFile(aFileName);
    for i := 1 to CSV.RowCount - 1 do
    begin
      DMM.tabGeoBank.Append;
      DMM.tabGeoBank.FieldByName('coordinate_name').AsString := CSV.Cells[0, i];
      DMM.tabGeoBank.FieldByName('longitude').AsFloat := StrToFloat(CSV.Cells[1, i]);
      DMM.tabGeoBank.FieldByName('latitude').AsFloat := StrToFloat(CSV.Cells[2, i]);
      DMM.tabGeoBank.Post;
    end;
  finally
    DMM.tabGeoBank.EnableControls;
    CSV.Free;
  end;
end;

procedure LoadGeoJSONPoints(const aFileName: string);
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  Coordinates: TJSONArray;
  i: Integer;
begin
  JSONData := GetJSON(ReadGeoJSONToString(aFileName));
  DMM.tabGeoBank.DisableControls;
  try
    JSONObject := TJSONObject(JSONData);
    JSONArray := JSONObject.Arrays['features'];
    for i := 0 to JSONArray.Count - 1 do
    begin
      if JSONArray.Objects[i].Objects['geometry'].Get('type', '') = 'Point' then
      begin
        DMM.tabGeoBank.Append;
        Coordinates := JSONArray.Objects[i].Objects['geometry'].Arrays['coordinates'];
        DMM.tabGeoBank.FieldByName('longitude').AsFloat := Coordinates.Floats[0];
        DMM.tabGeoBank.FieldByName('latitude').AsFloat := Coordinates.Floats[1];
        DMM.tabGeoBank.FieldByName('coordinate_name').AsString := JSONArray.Objects[i].Objects['properties'].Get('name', '');
        DMM.tabGeoBank.Post;
      end;
    end;
  finally
    DMM.tabGeoBank.EnableControls;
    JSONData.Free;
  end;
end;

function ReadGeoJSONToString(const FileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Result := StringStream.DataString;
  finally
    FileStream.Free;
    StringStream.Free;
  end;
end;

procedure DBToMapPoints(const aTableType: TTableType; aDataSet: TDataSet; ClearList: Boolean; var aMapPointList: TMapPointList);
var
  mapObj: TMapPointObject;
  nameField, longField, latField: String;
  BM: TBookmark;
begin
  if aTableType = tbSurveys then
  begin
    longField := 'start_longitude';
    latField := 'start_latitude';
  end
  else
  begin
    longField := 'longitude';
    latField := 'latitude';
  end;

  if aTableType = tbGazetteer then
    nameField := 'site_name'
  else
    nameField := 'full_name';

  BM := aDataSet.Bookmark;
  aDataSet.DisableControls;
  try
    if ClearList then
      aMapPointList.Clear;

    aDataSet.First;
    while not aDataSet.EOF do
    begin
      if (aDataSet.FieldByName(longField).AsFloat <> 0) and (aDataSet.FieldByName(latField).AsFloat <> 0) then
      begin
        mapObj := TMapPointObject.Create(aDataSet.FieldByName(longField).AsFloat,
                                         aDataSet.FieldByName(latField).AsFloat,
                                         aDataSet.FieldByName(nameField).AsString);
        aMapPointList.Add(mapObj);
      end;

      aDataSet.Next;
    end;
  finally
    aDataSet.EnableControls;
    aDataSet.Bookmark := BM;
  end;
end;

procedure ViewMapPoint(aMapPoint: TMapPoint; aZoom: Integer; aMap: TMapView);
begin

  {$IFDEF DEBUG}
  LogDebug('View map with coordinates ' + aMapPoint.ToString);
  {$ENDIF}

end;

function GeoAssistDlg(aDMS: String; aAxis: TMapAxis; aControl: TControl; var aDecimal: Extended): Boolean;
//var
//  PControl: TPoint;
begin
  Result := False;
  if aAxis = maLatitude then
    LogEvent(leaOpen, 'GeoAssist (Latitude)')
  else
    LogEvent(leaOpen, 'GeoAssist (Longitude)');
  dlgGeoAssist := TdlgGeoAssist.Create(nil);
  with dlgGeoAssist do
  try
    Axis := aAxis;
    Linha := aDMS;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    //SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      aDecimal := CoordDec;
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoAssist);
    LogEvent(leaClose, 'GeoAssist');
  end;
end;

function GeoAssistDlg(aDMS: TDMS; aAxis: TMapAxis; aControl: TControl; var aDecimal: Extended): Boolean;
//var
//  PControl: TPoint;
begin
  Result := False;
  if aAxis = maLatitude then
    LogEvent(leaOpen, 'GeoAssist (Latitude)')
  else
    LogEvent(leaOpen, 'GeoAssist (Longitude)');

  dlgGeoAssist := TdlgGeoAssist.Create(nil);
  with dlgGeoAssist do
  try
    Axis := aAxis;
    Linha := '';
    CoordDMS := aDMS;
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    //PControl := aControl.ClientOrigin;
    //SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      aDecimal := CoordDec;
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoAssist);
    LogEvent(leaClose, 'GeoAssist');
  end;
end;

function GeoAssistDlg(aControl: TControl; aDataSet: TDataSet; LongitudeField, LatitudeField: String): Boolean;
//var
//  PControl: TPoint;
begin
  Result := False;
  LogEvent(leaOpen, 'GeoAssist');

  dlgGeoAssist := TdlgGeoAssist.Create(nil);
  with dlgGeoAssist do
  try
    PointStr := EmptyStr;
    DecimalPoint.FromString(aDataSet.FieldByName(LongitudeField).AsString + '; ' +
      aDataSet.FieldByName(LatitudeField).AsString);
    //if Assigned(aControl) then
    //begin
      //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    //  PControl := aControl.ClientOrigin;
    //  SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    //end
    //else
    //  Position := poScreenCenter;
    //SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      aDataSet.FieldByName(LongitudeField).AsFloat := DecimalPoint.X;
      aDataSet.FieldByName(LatitudeField).AsFloat := DecimalPoint.Y;
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoAssist);
    LogEvent(leaClose, 'GeoAssist');
  end;
end;

function GeoAssistDlg(aControl: TControl; LongitudeEdit, LatitudeEdit: TCustomEditButton): Boolean;
begin
  Result := False;
  LogEvent(leaOpen, 'GeoAssist');

  dlgGeoAssist := TdlgGeoAssist.Create(nil);
  with dlgGeoAssist do
  try
    PointStr := EmptyStr;
    DecimalPoint.FromString(LongitudeEdit.Text + '; ' + LatitudeEdit.Text);
    //if Assigned(aControl) then
    //begin
      //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    //  PControl := aControl.ClientOrigin;
    //  SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    //end
    //else
    //  Position := poScreenCenter;
    //SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    if ShowModal = mrOK then
    begin
      LongitudeEdit.Text := FormatFloat(maskLongitudeZeroBlank, DecimalPoint.X);
      LatitudeEdit.Text := FormatFloat(maskLatitudeZeroBlank, DecimalPoint.Y);
      Result := True;
    end;
  finally
    FreeAndNil(dlgGeoAssist);
    LogEvent(leaClose, 'GeoAssist');
  end;
end;

function DecimalToMercator(aDec: TMapPoint): TMercatorPoint;
var
  lat, lon: Extended;
begin
  lat := DegToRad(aDec.Y);
  lon := DegToRad(aDec.X);

  Result.X := datumA * lon;
  Result.Y := datumA * ln(tan(Pi/4 + lat/2));
end;

function MercatorToDecimal(aMerc: TMercatorPoint): TMapPoint;
begin
  Result.X := aMerc.X / datumA;
  Result.Y := RadToDeg(2 * ArcTan(Exp(aMerc.Y / datumA)) - Pi / 2);
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
  FAbbreviation := EmptyStr;
  FRank := srNone;
  FParentSiteId := 0;
  FMunicipalityId := 0;
  FStateId := 0;
  FCountryId := 0;
  FFullName := EmptyStr;
  FEbirdName := EmptyStr;
  FLatitude := 0.0;
  FLongitude := 0.0;
  FAltitude := 0.0;
  FLanguage := EmptyStr;
  FDescription := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TSite.Copy(aFrom: TSite);
begin
  FName := aFrom.Name;
  FAbbreviation := aFrom.Abbreviation;
  FRank := aFrom.Rank;
  FParentSiteId := aFrom.ParentSiteId;
  FMunicipalityId := aFrom.MunicipalityId;
  FStateId := aFrom.StateId;
  FCountryId := aFrom.CountryId;
  FFullName := aFrom.FullName;
  FEbirdName := aFrom.EbirdName;
  FLatitude := aFrom.Latitude;
  FLongitude := aFrom.Longitude;
  FAltitude := aFrom.Altitude;
  FLanguage := aFrom.Language;
  FDescription := aFrom.Description;
  FNotes := aFrom.Notes;
end;

procedure TSite.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSite.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM gazetteer');
      Add('WHERE (site_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
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
    Add('SELECT ' +
        'site_id, ' +
        'site_name, ' +
        'site_acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'site_rank, ' +
        'parent_site_id, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'full_name, ' +
        'ebird_name, ' +
        'language, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM gazetteer');
    Add('WHERE site_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry)
    else
      Self.Clear;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSite.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('site_id').AsInteger;
    FName := FieldByName('site_name').AsString;
    FAbbreviation := FieldByName('site_acronym').AsString;
    case FieldByName('site_rank').AsString of
      'P': FRank := srCountry;
      'E': FRank := srState;
      'R': FRank := srRegion;
      'M': FRank := srMunicipality;
      'D': FRank := srDistrict;
      'L': FRank := srLocality;
    else
      FRank := srNone;
    end;
    FParentSiteId := FieldByName('parent_site_id').AsInteger;
    FMunicipalityId := FieldByName('municipality_id').AsInteger;
    FStateId := FieldByName('state_id').AsInteger;
    FCountryId := FieldByName('country_id').AsInteger;
    FLanguage := FieldByName('language').AsString;
    FFullName := FieldByName('full_name').AsString;
    FEbirdName := FieldByName('ebird_name').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAltitude := FieldByName('altitude').AsFloat;
    FDescription := FieldByName('description').AsString;
    FNotes := FieldByName('notes').AsString;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSite.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO gazetteer (' +
        'site_name, ' +
        'site_acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'site_rank, ' +
        'parent_site_id, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'full_name, ' +
        'ebird_name, ' +
        'language, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        ':site_name, ' +
        ':site_acronym, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':altitude, ' +
        ':site_rank, ' +
        ':parent_site_id, ' +
        ':country_id, ' +
        ':state_id, ' +
        ':municipality_id, ' +
        ':full_name, ' +
        ':ebird_name, ' +
        ':language, ' +
        ':description, ' +
        ':notes, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      ParamByName('site_name').AsString := FName;
      SetStrParam(ParamByName('site_acronym'), FAbbreviation);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      ParamByName('site_rank').AsString := SiteRankStr[FRank];
      SetForeignParam(ParamByName('parent_site_id'), FParentSiteId);
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetStrParam(ParamByName('ebird_name'), FEbirdName);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('language'), FLanguage);
      SetStrParam(ParamByName('description'), FDescription);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

      // Get the site hierarchy
      if (FParentSiteId > 0) then
      begin
        Clear;
        Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
        Add('WHERE site_id = :asite');
        ParamByName('ASITE').AsInteger := FParentSiteId;
        Open;
        FCountryId := FieldByName('country_id').AsInteger;
        FStateId := FieldByName('state_id').AsInteger;
        FMunicipalityId := FieldByName('municipality_id').AsInteger;
        Close;
      end;
      case FRank of
        srCountry:      FCountryId := FId;
        srState:        FStateId := FId;
        srMunicipality: FMunicipalityId := FId;
      end;
      // Save the site hierarchy
      Clear;
      Add('UPDATE gazetteer SET');
      Add('  country_id = :country_id,');
      Add('  state_id = :state_id,');
      Add('  municipality_id = :municipality_id');
      Add('WHERE site_id = :aid');
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      ParamByName('aid').AsInteger := FId;
      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSite.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TSite.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', FName);
    JSONObject.Add('Acronym', FAbbreviation);
    JSONObject.Add('Rank', SiteRankStr[FRank]);
    JSONObject.Add('ParentSiteId', FParentSiteId);
    JSONObject.Add('MunicipalityId', FMunicipalityId);
    JSONObject.Add('StateId', FStateId);
    JSONObject.Add('CountryId', FCountryId);
    JSONObject.Add('FullName', FFullName);
    JSONObject.Add('EbirdName', FEbirdName);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Altitude', FAltitude);
    JSONObject.Add('Language', FLanguage);
    JSONObject.Add('Description', FDescription);
    JSONObject.Add('Notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TSite.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TSite.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE gazetteer SET ' +
        'site_name = :site_name, ' +
        'site_acronym = :site_acronym, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'altitude = :altitude, ' +
        'site_rank = :site_rank, ' +
        'parent_site_id = :parent_site_id, ' +
        'country_id = :country_id, ' +
        'state_id = :state_id, ' +
        'municipality_id = :municipality_id, ' +
        'full_name = :full_name, ' +
        'ebird_name = :ebird_name, ' +
        'language = :language, ' +
        'description = :description, ' +
        'notes = :notes, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (site_id = :site_id)');

      ParamByName('site_name').AsString := FName;
      SetStrParam(ParamByName('site_acronym'), FAbbreviation);
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      ParamByName('site_rank').AsString := SiteRankStr[FRank];
      SetForeignParam(ParamByName('parent_site_id'), FParentSiteId);
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      SetStrParam(ParamByName('ebird_name'), FEbirdName);
      SetStrParam(ParamByName('full_name'), FFullName);
      SetStrParam(ParamByName('language'), FLanguage);
      SetStrParam(ParamByName('description'), FDescription);
      SetStrParam(ParamByName('notes'), FNotes);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('site_id').AsInteger := FId;

      ExecSQL;

      // Get the site hierarchy
      if (FParentSiteId > 0) then
      begin
        Clear;
        Add('SELECT country_id, state_id, municipality_id FROM gazetteer');
        Add('WHERE site_id = :asite');
        ParamByName('ASITE').AsInteger := FParentSiteId;
        Open;
        FCountryId := FieldByName('country_id').AsInteger;
        FStateId := FieldByName('state_id').AsInteger;
        FMunicipalityId := FieldByName('municipality_id').AsInteger;
        Close;
      end;
      case FRank of
        srCountry:      FCountryId :=      FId;
        srState:        FStateId :=        FId;
        srMunicipality: FMunicipalityId := FId;
      end;
      // Save the site hierarchy
      Clear;
      Add('UPDATE gazetteer SET');
      Add('  country_id = :country_id,');
      Add('  state_id = :state_id,');
      Add('  municipality_id = :municipality_id');
      Add('WHERE site_id = :aid');
      ParamByName('country_id').AsInteger := FCountryId;
      SetForeignParam(ParamByName('state_id'), FStateId);
      SetForeignParam(ParamByName('municipality_id'), FMunicipalityId);
      ParamByName('aid').AsInteger := FId;
      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSite.Diff(aOld: TSite; var aList: TStrings): Boolean;
//var
//  PropList: PPropList;
//  PropCount, I: Integer;
//  PropInfo: PPropInfo;
//  OldValue, NewValue, FriendlyName: string;
//begin
//  Result := False;
//
//  InitSitePropsDict;
//
//  PropCount := GetPropList(Self.ClassInfo, tkProperties, @PropList);
//  try
//    for I := 0 to PropCount - 1 do
//    begin
//      PropInfo := PropList^[I];
//      OldValue := GetPropValue(aOld, PropInfo, True);
//      NewValue := GetPropValue(Self, PropInfo, True);
//      if OldValue <> NewValue then
//      begin
//        if not SitePropsDict.TryGetData(PropInfo^.Name, FriendlyName) then
//          FriendlyName := PropInfo^.Name;
//        aList.Add(Format('%s;%s;%s', [FriendlyName, OldValue, NewValue]));
//        Result := True;
//      end;
//    end;
//  finally
//    if Assigned(SitePropsDict) then
//      SitePropsDict.Free;
//    FreeMem(PropList);
//  end;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscSiteName, aOld.Name, FName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAcronym, aOld.Abbreviation, FAbbreviation, R) then
    aList.Add(R);
  if FieldValuesDiff(rscType, aOld.Rank, FRank, R) then
    aList.Add(R);
  if FieldValuesDiff(rscParentSiteId, aOld.ParentSiteId, FParentSiteId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscFullName, aOld.FullName, FFullName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
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
  if FieldValuesDiff(rscLanguage, aOld.Language, FLanguage, R) then
    aList.Add(R);
  if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
    aList.Add(R);
  if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TSite.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
        'site_id, ' +
        'site_name, ' +
        'site_acronym, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'site_rank, ' +
        'parent_site_id, ' +
        'country_id, ' +
        'state_id, ' +
        'municipality_id, ' +
        'full_name, ' +
        'ebird_name, ' +
        'language, ' +
        'description, ' +
        'notes, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM gazetteer');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

{ TPoi }

constructor TPoi.Create(aValue: Integer);
begin
  if aValue > 0 then
    GetData(aValue)
  else
    Clear;
end;

procedure TPoi.Clear;
begin
  inherited Clear;
  FSampleDate := NullDate;
  FSampleTime := NullTime;
  FPoiName := EmptyStr;
  FLongitude := 0.0;
  FLatitude := 0.0;
  FAltitude := 0.0;
  FObserverId := 0;
  FTaxonId := 0;
  FIndividualId := 0;
  FSightingId := 0;
  FSurveyId := 0;
end;

procedure TPoi.Copy(aFrom: TPoi);
begin
  FSampleDate := aFrom.SampleDate;
  FSampleTime := aFrom.SampleTime;
  FPoiName := aFrom.PoiName;
  FLongitude := aFrom.Longitude;
  FLatitude := aFrom.Latitude;
  FAltitude := aFrom.Altitude;
  FObserverId := aFrom.ObserverId;
  FTaxonId := aFrom.TaxonId;
  FIndividualId := aFrom.IndividualId;
  FSightingId := aFrom.SightingId;
  FSurveyId := aFrom.SurveyId;
end;

procedure TPoi.Delete;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPoi.Delete: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    if not DMM.sqlTrans.Active then
      DMM.sqlTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM poi_library');
      Add('WHERE (poi_id = :aid)');

      ParamByName('aid').AsInteger := FId;

      ExecSQL;

      DMM.sqlTrans.CommitRetaining;
    except
      DMM.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TPoi.Diff(aOld: TPoi; var aList: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;

  if FieldValuesDiff(rscDate, aOld.SampleDate, FSampleDate, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTime, aOld.SampleTime, FSampleTime, R) then
    aList.Add(R);
  if FieldValuesDiff(rscName, aOld.PoiName, FPoiName, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLatitude, aOld.Latitude, FLatitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscLongitude, aOld.Longitude, FLongitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscAltitude, aOld.Altitude, FAltitude, R) then
    aList.Add(R);
  if FieldValuesDiff(rscObserverID, aOld.ObserverId, FObserverId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscTaxonID, aOld.TaxonId, FTaxonId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscIndividualID, aOld.IndividualId, FIndividualId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSightingID, aOld.SightingId, FSightingId, R) then
    aList.Add(R);
  if FieldValuesDiff(rscSurveyID, aOld.SurveyId, FSurveyId, R) then
    aList.Add(R);

  Result := aList.Count > 0;
end;

function TPoi.Find(const FieldName: String; const Value: Variant): Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    SQLTransaction := DMM.sqlTrans;
    MacroCheck := True;

    Add('SELECT ' +
        'poi_id, ' +
        'sample_date, ' +
        'sample_time, ' +
        'poi_name, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'sighting_id, ' +
        'survey_id, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM poi_library');
    Add('WHERE %afield = :avalue');
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      LoadFromDataSet(Qry);

      Result := True;
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TPoi.GetData(aKey: Integer);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Clear;
    Add('SELECT ' +
        'poi_id, ' +
        'sample_date, ' +
        'sample_time, ' +
        'poi_name, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'sighting_id, ' +
        'survey_id, ' +
        'user_inserted, ' +
        'user_updated, ' +
        'datetime(insert_date, ''localtime'') AS insert_date, ' +
        'datetime(update_date, ''localtime'') AS update_date, ' +
        'exported_status, ' +
        'marked_status, ' +
        'active_status ' +
      'FROM poi_library');
    Add('WHERE poi_id = :cod');
    ParamByName('COD').AsInteger := aKey;
    Open;
    if RecordCount > 0 then
      LoadFromDataSet(Qry);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPoi.LoadFromDataSet(aDataSet: TDataSet);
var
  InsertTimeStamp, UpdateTimeStamp: TDateTime;
begin
  if not aDataSet.Active then
    Exit;

  with aDataSet do
  begin
    FId := FieldByName('poi_id').AsInteger;
    FSampleDate := FieldByName('sample_date').AsDateTime;
    FSampleTime := FieldByName('sample_time').AsDateTime;
    FPoiName := FieldByName('poi_name').AsString;
    FLatitude := FieldByName('latitude').AsFloat;
    FLongitude := FieldByName('longitude').AsFloat;
    FAltitude := FieldByName('altitude').AsFloat;
    FObserverId := FieldByName('observer_id').AsInteger;
    FTaxonId := FieldByName('taxon_id').AsInteger;
    FIndividualId := FieldByName('individual_id').AsInteger;
    FSightingId := FieldByName('sighting_id').AsInteger;
    FSurveyId := FieldByName('survey_id').AsInteger;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    if not (FieldByName('insert_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('insert_date').AsString, InsertTimeStamp) then
        FInsertDate := InsertTimeStamp
      else
        FInsertDate := FieldByName('insert_date').AsDateTime;
    FUserInserted := FieldByName('user_inserted').AsInteger;
    if not (FieldByName('update_date').IsNull) then
      if TryISOStrToDateTime(FieldByName('update_date').AsString, UpdateTimeStamp) then
        FUpdateDate := UpdateTimeStamp
      else
        FUpdateDate := FieldByName('update_date').AsDateTime;
    FUserUpdated := FieldByName('user_updated').AsInteger;
    FExported := FieldByName('exported_status').AsBoolean;
    FMarked := FieldByName('marked_status').AsBoolean;
    FActive := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TPoi.Insert;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('INSERT INTO poi_library (' +
        'sample_date, ' +
        'sample_time, ' +
        'poi_name, ' +
        'longitude, ' +
        'latitude, ' +
        'altitude, ' +
        'observer_id, ' +
        'taxon_id, ' +
        'individual_id, ' +
        'sighting_id, ' +
        'survey_id, ' +
        'user_inserted, ' +
        'insert_date) ');
      Add('VALUES (' +
        'date(:sample_date), ' +
        'time(:sample_time), ' +
        ':poi_name, ' +
        ':longitude, ' +
        ':latitude, ' +
        ':altitude, ' +
        ':observer_id, ' +
        ':taxon_id, ' +
        ':individual_id, ' +
        ':sighting_id, ' +
        ':survey_id, ' +
        ':user_inserted, ' +
        'datetime(''now'', ''subsec''))');

      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('poi_name').AsString := FPoiName;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('user_inserted').AsInteger := ActiveUser.Id;

      ExecSQL;

      // Get the record ID
      Clear;
      Add('SELECT last_insert_rowid()');
      Open;
      FId := Fields[0].AsInteger;
      Close;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TPoi.Save;
begin
  if FId = 0 then
    Insert
  else
    Update;
end;

function TPoi.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Date', FSampleDate);
    JSONObject.Add('Time', FSampleTime);
    JSONObject.Add('Name', FPoiName);
    JSONObject.Add('Longitude', FLongitude);
    JSONObject.Add('Latitude', FLatitude);
    JSONObject.Add('Altitude', FAltitude);
    JSONObject.Add('Observer', FObserverId);
    JSONObject.Add('Taxon', FTaxonId);
    JSONObject.Add('Individual', FIndividualId);
    JSONObject.Add('Sighting', FSightingId);
    JSONObject.Add('Survey', FSurveyId);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

procedure TPoi.Update;
var
  Qry: TSQLQuery;
begin
  if FId = 0 then
    raise Exception.CreateFmt('TPoi.Update: %s.', [rsErrorEmptyId]);

  Qry := TSQLQuery.Create(DMM.sqlCon);
  with Qry, SQL do
  try
    DataBase := DMM.sqlCon;
    Transaction := DMM.sqlTrans;

    //if not DMM.sqlTrans.Active then
    //  DMM.sqlTrans.StartTransaction;
    //try
      Clear;
      Add('UPDATE poi_library SET ' +
        'sample_date = date(:sample_date), ' +
        'sample_time = time(:sample_time), ' +
        'poi_name = :poi_name, ' +
        'longitude = :longitude, ' +
        'latitude = :latitude, ' +
        'altitude = :altitude, ' +
        'observer_id = :observer_id, ' +
        'taxon_id = :taxon_id, ' +
        'individual_id = :individual_id, ' +
        'sighting_id = :sighting_id, ' +
        'survey_id = :survey_id, ' +
        'user_updated = :user_updated, ' +
        'update_date = datetime(''now'', ''subsec''), ' +
        'marked_status = :marked_status, ' +
        'active_status = :active_status');
      Add('WHERE (poi_id = :poi_id)');

      SetDateParam(ParamByName('sample_date'), FSampleDate);
      SetTimeParam(ParamByName('sample_time'), FSampleTime);
      ParamByName('poi_name').AsString := FPoiName;
      SetCoordinateParam(ParamByName('longitude'), ParamByName('latitude'), FLongitude, FLatitude);
      SetFloatParam(ParamByName('altitude'), FAltitude);
      SetForeignParam(ParamByName('observer_id'), FObserverId);
      SetForeignParam(ParamByName('taxon_id'), FTaxonId);
      SetForeignParam(ParamByName('individual_id'), FIndividualId);
      SetForeignParam(ParamByName('sighting_id'), FSightingId);
      SetForeignParam(ParamByName('survey_id'), FSurveyId);
      ParamByName('user_updated').AsInteger := ActiveUser.Id;
      ParamByName('marked_status').AsBoolean := FMarked;
      ParamByName('active_status').AsBoolean := FActive;
      ParamByName('site_id').AsInteger := FId;

      ExecSQL;

    //  DMM.sqlTrans.CommitRetaining;
    //except
    //  DMM.sqlTrans.RollbackRetaining;
    //  raise;
    //end;
  finally
    FreeAndNil(Qry);
  end;
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

{ TMapPointObject }

constructor TMapPointObject.Create(aX, aY: Extended; aName: String);
begin
  inherited Create;
  FData.X := aX;
  FData.Y := aY;
  FData.Name := aName;
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
    Result := Format('%d° %2.2d'' %s" %s',[Degrees, Minutes, FormatFloat('00.00', Seconds), Hemisphere])
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
