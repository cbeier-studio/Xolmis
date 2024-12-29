unit cbs_math;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, DB, SQLDB, Math;

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
  n, i: Integer;
  Middle: Integer;
begin
  Result := 0;

  // Calcular a mediana
  Middle := Length(Values) div 2;
  if Length(Values) mod 2 = 0 then
    // Se o número de valores for par, a mediana é a média dos dois valores do meio
    Result := (Values[Middle - 1] + Values[Middle]) / 2
  else
    // Se o número de valores for ímpar, a mediana é o valor do meio
    Result := Values[Middle];
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

