{ Xolmis JSON Import and Export library

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit io_json;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonreader, Math, streamex, io_core;

type

  { TJSONImporter }

  TJSONImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

  { TNDJSONImporter }

  TNDJSONImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

implementation

procedure AddJSONToRow(const Path: string; AJSON: TJSONData; Row: TXRow);
var
  i: Integer;
  child: TJSONData;
  name, key: string;
begin
  case AJSON.JSONType of
    jtObject:
      for i := 0 to TJSONObject(AJSON).Count - 1 do
      begin
        name := TJSONObject(AJSON).Names[i];
        child := TJSONObject(AJSON).Elements[name];
        if Path='' then
          key := name
        else
          key := Path + '.' + name;
        AddJSONToRow(key, child, Row);
      end;
    jtArray:
      // estratégia: “flatten” primeiro nível com índice
      for i := 0 to TJSONArray(AJSON).Count - 1 do
      begin
        child := TJSONArray(AJSON).Items[i];
        AddJSONToRow(Format('%s[%d]', [Path, i]), child, Row);
      end;
  else
    Row.Add(Path);
    Row.Values[Path] := AJSON.AsString;
  end;
end;

procedure HandleRegularJSON(root: TJSONData; RowOut: TXRowConsumer; const Options: TImportOptions);
var
  i: Integer;
  row: TXRow;
  arr: TJSONArray;
begin
  if root.JSONType = jtArray then
  begin
    arr := TJSONArray(root);
    for i := 0 to arr.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then Break;
      row := TXRow.Create;
      try
        if arr.Items[i].JSONType = jtObject then
          AddJSONToRow('', arr.Items[i], row)
        else
        begin
          row.Add('value');
          row.Values['value'] := arr.Items[i].AsString;
        end;
        if Assigned(RowOut) then RowOut(row);
      finally
        row.Free;
      end;
      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i+1)*100.0 / Max(1, arr.Count)), 'Importando JSON');
    end;
  end
  else if root.JSONType = jtObject then
  begin
    row := TXRow.Create;
    try
      AddJSONToRow('', root, row);
      if Assigned(RowOut) then RowOut(row);
    finally
      row.Free;
    end;
  end;
end;

function IsNDJSON(Stream: TStream): Boolean;
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  i, ValidLines: Integer;
begin
  Result := False;
  Stream.Position := 0;
  Reader := TStreamReader.Create(Stream);
  try
    i := 0;
    ValidLines := 0;
    while not Reader.Eof and (i < 10) do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then Continue;
      Inc(i);
      try
        Parser := TJSONParser.Create(Line);
        try
          Parser.Parse.Free;
          Inc(ValidLines);
        finally
          Parser.Free;
        end;
      except
        // ignore parsing error
      end;
    end;
  finally
    Reader.Free;
  end;
  Result := (ValidLines >= i * 0.8);
end;

{ TJSONImporter }

function TJSONImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'json') or (LowerCase(Ext) = 'ndjson');
end;

function TJSONImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  Parser: TJSONParser;
  Data: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  i: Integer;
begin
  Result := TStringList.Create;
  Stream.Position := 0;
  Parser := TJSONParser.Create(Stream);
  try
    Data := Parser.Parse;
    try
      if (Data.JSONType = jtArray) and (TJSONArray(Data).Count > 0) then
      begin
        Arr := TJSONArray(Data);
        if Arr.Items[0].JSONType = jtObject then
        begin
          Obj := TJSONObject(Arr.Items[0]);
          for i := 0 to Obj.Count - 1 do
            Result.Add(Obj.Names[i]);
        end;
      end;
    finally
      Data.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TJSONImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  parser: TJSONParser;
  root: TJSONData;
  Reader: TStreamReader;
  Line: string;
  row: TXRow;
  i, progress: Integer;
begin
  Stream.Position := 0;

  if IsNDJSON(Stream) then
  begin
    Reader := TStreamReader.Create(Stream);
    try
      i := 0;
      while not Reader.Eof do
      begin
        if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
          Break;

        Line := Trim(Reader.ReadLine);
        if Line = '' then Continue;

        parser := TJSONParser.Create(Line);
        try
          root := parser.Parse;
          try
            row := TXRow.Create;
            try
              AddJSONToRow('', root, row);
              if Assigned(RowOut) then RowOut(row);
            finally
              row.Free;
            end;
          finally
            root.Free;
          end;
        finally
          parser.Free;
        end;

        Inc(i);
        if Assigned(Options.OnProgress) then
        begin
          progress := Trunc(i * 100.0 / Max(1, 1000)); // estimativa simples
          Options.OnProgress(progress, 'Importando NDJSON');
        end;
      end;
    finally
      Reader.Free;
    end;
  end
  else
  begin
    // JSON clássico como estava
    parser := TJSONParser.Create(Stream);
    try
      root := parser.Parse;
      try
        HandleRegularJSON(root, RowOut, Options);
      finally
        root.Free;
      end;
    finally
      parser.Free;
    end;
  end;
end;

procedure TJSONImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  Parser: TJSONParser;
  Data: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  Row: TXRow;
  i, j, Count: Integer;
begin
  Stream.Position := 0;
  Parser := TJSONParser.Create(Stream);
  try
    Data := Parser.Parse;
    try
      if Data.JSONType = jtArray then
      begin
        Arr := TJSONArray(Data);
        Count := 0;
        for i := 0 to Arr.Count - 1 do
        begin
          if (Arr.Items[i].JSONType = jtObject) and (Count < MaxRows) then
          begin
            Obj := TJSONObject(Arr.Items[i]);
            Row := TXRow.Create;
            for j := 0 to Obj.Count - 1 do
              Row.Values[Obj.Names[j]] := Obj.Items[j].AsString;
            RowOut(Row);
            Row.Free;

            Inc(Count);
          end;
        end;
      end;
    finally
      Data.Free;
    end;
  finally
    Parser.Free;
  end;
end;

class function TJSONImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  Ext: String;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  if Ext = '.json' then
  begin
    if IsNDJSON(Stream) then
      Result := 80 // aceita NDJSON dentro do .json
    else
      Result := 85; // json clássico tem prioridade
  end
  else
  if Ext = '.ndjson' then
  begin
    Result := 40; // aceita, mas deixa espaço para TNDJSONImporter concorrer
  end
  else
    Result := 10;
end;

{ TNDJSONImporter }

function TNDJSONImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'ndjson');
end;

function TNDJSONImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  i: Integer;
begin
  Result := TStringList.Create;
  Stream.Position := 0;
  Reader := TStreamReader.Create(Stream);
  try
    while not Reader.Eof do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then
        Continue;

      Parser := TJSONParser.Create(Line);
      try
        Data := Parser.Parse;
        try
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            for i := 0 to Obj.Count - 1 do
              Result.Add(Obj.Names[i]);
            Exit; // só precisamos da primeira linha
          end;
        finally
          Data.Free;
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TNDJSONImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  Row: TXRow;
  i: Integer;
begin
  Reader := TStreamReader.Create(Stream);
  try
    while not Reader.Eof do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then
        Continue;

      Parser := TJSONParser.Create(Line);
      try
        Data := Parser.Parse;
        try
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            Row := TXRow.Create;
            for i := 0 to Obj.Count - 1 do
              Row.Values[Obj.Names[i]] := Obj.Items[i].AsString;
            RowOut(Row);
            Row.Free;
          end;
        finally
          Data.Free;
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TNDJSONImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  Reader: TStreamReader;
  Line: string;
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  Row: TXRow;
  i, Count: Integer;
begin
  Stream.Position := 0;
  Reader := TStreamReader.Create(Stream);
  try
    Count := 0;
    while (not Reader.Eof) and (Count < MaxRows) do
    begin
      Line := Trim(Reader.ReadLine);
      if Line = '' then
        Continue;

      Parser := TJSONParser.Create(Line);
      try
        Data := Parser.Parse;
        try
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            Row := TXRow.Create;
            for i := 0 to Obj.Count - 1 do
              Row.Values[Obj.Names[i]] := Obj.Items[i].AsString;
            RowOut(Row);
            Row.Free;

            Inc(Count);
          end;
        finally
          Data.Free;
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

class function TNDJSONImporter.Probe(const FileName: string; Stream: TStream): Integer;
begin
  if SameText(ExtractFileExt(FileName), '.ndjson') then
    Exit(95)
  else
    Exit(0);
end;

initialization
  TImporterRegistry.RegisterImporter('json', TJSONImporter);
  TImporterRegistry.RegisterImporter('ndjson', TNDJSONImporter);

end.

