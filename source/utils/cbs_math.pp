unit cbs_math;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, DB, SQLDB, Math, Generics.Collections;

  function Median(Values: array of Extended): Extended;
  function MedianQuartiles(Values: TDoubleDynArray; out Quartile1, Quartile3: Extended): Extended;

  function Zscore(aTaxon: Integer; aField: String; aValue: Extended): Extended;
  function ModifiedZScore(aTaxon: Integer; aField: String; aValue: Extended): Extended;
  function IsOutlier(aTaxon: Integer; aField: String; aValue: Extended; Factor: Double = 1.5): Boolean;
  function IsOutlierZscore(aTaxon: Integer; aField: String; aValue: Extended): Boolean;

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

function IsOutlier(aTaxon: Integer; aField: String; aValue: Extended; Factor: Double): Boolean;
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
      //Sort(x);

      //M := MedianQuartiles(x, Q1, Q3);
      Q1 := x[Length(x) div 4];
      Q3 := x[3 * Length(x) div 4];
      IQR := Q3 - Q1;
      ol1 := Q1 - (Factor * IQR);
      ol3 := Q3 + (Factor * IQR);

      Result := (aValue < ol1) or (aValue > ol3);
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function Median(Values: array of Extended): Extended;
var
  Middle: Integer;
begin
  Result := 0;

  // The function assumes the input array is already sorted.
  Middle := Length(Values) div 2;
  if Length(Values) mod 2 = 0 then
    // If the number of values is even, the median is the average of the two middle values
    Result := (Values[Middle - 1] + Values[Middle]) / 2
  else
    // If the number of values is odd, the median is the middle value
    Result := Values[Middle];
end;

function MedianQuartiles(Values: TDoubleDynArray; out Quartile1, Quartile3: Extended): Extended;
var
  n, i: Integer;
  sortedValues: TDoubleDynArray;
  lowerHalf, upperHalf: TDoubleDynArray;
begin
  Result := 0;
  Quartile1 := 0;
  Quartile3 := 0;

  n := High(Values);

  // 1. Sort the input array.
  SetLength(sortedValues, n);
  for i := 0 to n - 1 do
    sortedValues[i] := Values[i];
  specialize TArrayHelper<Extended>.Sort(sortedValues);

  // 2. Calculate the median first using the now-sorted array.
  Result := Median(sortedValues);

  // Guard against small datasets that don't have quartiles.
  if n < 4 then
  begin
    Quartile1 := sortedValues[0];
    Quartile3 := sortedValues[n-1];
    Exit;
  end;

  // 3. Create the lower half of the dataset to find the first quartile (Q1).
  // The lower half includes the median for odd-sized datasets.
  SetLength(lowerHalf, n div 2);
  for i := 0 to (n div 2) - 1 do
    lowerHalf[i] := sortedValues[i];

  // Calculate the median of the lower half to get Q1.
  Quartile1 := Median(lowerHalf);

  // 4. Create the upper half of the dataset to find the third quartile (Q3).
  // The upper half also includes the median for odd-sized datasets.
  SetLength(upperHalf, n div 2);
  for i := 0 to (n div 2) - 1 do
    upperHalf[i] := sortedValues[i + (n - (n div 2))]; // Adjust index for upper half

  // Calculate the median of the upper half to get Q3.
  Quartile3 := Median(upperHalf);
end;

function ModifiedZScore(aTaxon: Integer; aField: String; aValue: Extended): Extended;
var
  Med, MAD, Sum: Extended;
  i: Integer;
  ColumnValues, Differences: array of Extended;
  Qry: TSQLQuery;
begin
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
    { #todo : Method to convert a data column in array. }
    SetLength(ColumnValues, RecordCount);
    First;
    for i := Low(ColumnValues) to High(ColumnValues) do
    begin
      ColumnValues[i] := FieldByName(aField).AsFloat;
      Next;
    end;

    // Calcular a mediana
    Med := Median(ColumnValues);

    // Calcular as diferenças absolutas em relação à mediana
    SetLength(Differences, Length(ColumnValues));
    for i := 0 to High(ColumnValues) do
      Differences[i] := Abs(ColumnValues[i] - Med);

    // Calcular a MAD (mediana das diferenças absolutas)
    MAD := Median(Differences);

    // Calcular o Z-Score modificado
    if MAD = 0 then
      Result := 0
    else
      Result := 0.6745 * (aValue - Med) / MAD;

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function IsOutlierZscore(aTaxon: Integer; aField: String; aValue: Extended): Boolean;
const
  Threshold = 3.5; // Limite comum para Z-Score modificado
begin
  Result := Abs(ModifiedZScore(aTaxon, aField, aValue)) > Threshold;
end;

end.

