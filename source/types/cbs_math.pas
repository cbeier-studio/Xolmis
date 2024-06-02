unit cbs_math;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, DB, SQLDB, Math;

  function Median(Values: array of Extended): Extended;
  function MedianQuartiles(Values: TDoubleDynArray; out Quartile1, Quartile3: Extended): Extended;

  function Zscore(aTaxon: Integer; aField: String; aValue: Extended): Extended;
  function IsOutlier(aTaxon: Integer; aField: String; aValue: Extended): Boolean;

implementation

uses udm_main;

function Zscore(aTaxon: Integer; aField: String; aValue: Extended): Extended;
var
  Qry: TSQLQuery;
  x: array of Extended;
  z: Extended;
  i: Integer;
begin
  Result := 0;
  z := 0;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    MacroCheck := True;
    Add('SELECT %measurement FROM captures');
    Add('WHERE (taxon_id = :ataxon)');
    Add('AND ((%measurement != 0) OR (%measurement NOTNULL))');
    Add('ORDER BY %measurement ASC');
    MacroByName('MEASUREMENT').AsString := aField;
    ParamByName('ATAXON').AsInteger := aTaxon;
    Open;
    if RecordCount > 1 then
    begin
      SetLength(x, RecordCount);
      First;
      for i := Low(x) to High(x) do
      begin
        x[i] := FieldByName(aField).AsFloat;
        Next;
      end;

      z := (aValue - Mean(x)) / StdDev(x);
    end
    else
      z := 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  Result := z;
end;

function IsOutlier(aTaxon: Integer; aField: String; aValue: Extended): Boolean;
var
  Qry: TSQLQuery;
  x: array of Extended;
  M, IQR, Q1, Q3, ol1, ol3: Extended;
  i: Integer;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := DMM.sqlCon;
    MacroCheck := True;
    Add('SELECT %measurement FROM captures');
    Add('WHERE (taxon_id = :ataxon)');
    Add('AND ((%measurement != 0) OR (%measurement NOTNULL))');
    Add('ORDER BY %measurement ASC');
    MacroByName('MEASUREMENT').AsString := aField;
    ParamByName('ATAXON').AsInteger := aTaxon;
    Open;
    if RecordCount > 3 then
    begin
      SetLength(x, RecordCount);
      First;
      for i := Low(x) to High(x) do
      begin
        x[i] := FieldByName(aField).AsFloat;
        Next;
      end;

      M := MedianQuartiles(x, Q1, Q3);
      IQR := Q3 - Q1;
      ol1 := Q1 - (1.5 * IQR);
      ol3 := Q3 + (1.5 * IQR);

      Result := (aValue <= ol1) or (aValue >= ol3);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function Median(Values: array of Extended): Extended;
var
  n, i: Integer;
begin
  Result := 0;

  n := High(Values);
  if n > 1 then
  begin
    if Odd(n) then
      i := (n + 1) div 2
    else
      i := n div 2;
  end
  else
    i := n;

  if Odd(n) then
    Result := Values[i - 1]
  else
    Result := (Values[i - 1] + Values[i]) / 2;
end;

function MedianQuartiles(Values: TDoubleDynArray; out Quartile1, Quartile3: Extended): Extended;
var
  n, iq1, iq3: Integer;
  M, Q1, Q3: Extended;
  vq1, vq3: TDoubleDynArray;
begin
  Result := 0;
  M := 0;
  Q1 := 0;
  Q3 := 0;

  // Median
  n := High(Values);
  M := Median(Values);

  // Q1
  if n > 1 then
  begin
    if Odd(n) then
      iq1 := ((n + 1) div 2) - 1
    else
      iq1 := n div 2;
    //while Values[iq1] = M do
    //  Dec(iq1);
    SetLength(vq1, iq1);
    vq1 := Copy(Values, 0, iq1);
    Q1 := Median(vq1);
  end
  else
    Q1 := Values[0];

  // Q3
  if n > 1 then
  begin
    if Odd(n) then
      iq3 := iq1 + 2
    else
      iq3 := iq1 + 1;
    //while Values[iq3] = M do
    //  Inc(iq3);
    SetLength(vq3, iq1);
    vq3 := Copy(Values, iq3 - 1, iq1);
    Q3 := Median(vq3);
  end
  else
    Q3 := Values[0];

  Quartile1 := Q1;
  Quartile3 := Q3;
  Result := M;

end;

end.

