{ Xolmis XML Import and Export library

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

unit io_xml;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, LConvEncoding, io_core;

type

  { TXMLImporter }

  TXMLImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
    function GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList; override;
    procedure PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer; RowOut: TXRowConsumer); override;
  end;

implementation

procedure NodeToRow(Node: TDOMNode; const Prefix: string; Row: TXRow);
var
  i: Integer;
  attr: TDOMNode;
  key: string;
begin
  // atributos
  if (Node.Attributes <> nil) then
    for i := 0 to Node.Attributes.Length - 1 do
    begin
      attr := Node.Attributes.Item[i];
      key := Prefix + '@' + attr.NodeName;
      Row.Add(key);
      Row.Values[key] := attr.NodeValue;
    end;

  // texto direto
  if (Node.FirstChild <> nil) and (Node.FirstChild.NodeType = TEXT_NODE) then
  begin
    Row.Add(Prefix);
    Row.Values[Prefix] := Node.TextContent
  end
  else
    for i := 0 to Node.ChildNodes.Count - 1 do
      NodeToRow(Node.ChildNodes[i], Prefix + Node.ChildNodes[i].NodeName + '.', Row);
end;

{ TXMLImporter }

function TXMLImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'xml');
end;

function TXMLImporter.GetFieldNames(Stream: TStream; const Options: TImportOptions): TStringList;
var
  RawBytes: TBytes;
  RawText, Utf8Text, DetectedEncoding: String;
  Utf8Stream: TStringStream;
  Doc: TXMLDocument;
  NodeList: TDOMNodeList;
  Node, Child: TDOMNode;
  Attr: TDOMAttr;
  SL: TStringList;
  i, j: Integer;

  function ResolvePath(Node: TDOMNode; const Path: String): TDOMNode;
  var
    Parts: TStringArray;
    P: String;
    K: Integer;
    Cur: TDOMNode;
  begin
    Result := Node;
    if Path = '' then Exit;

    Parts := Path.Split(['/']);
    Cur := Node;

    for K := 0 to High(Parts) do
    begin
      P := Parts[K];
      Cur := Cur.FindNode(P);
      if Cur = nil then Exit(nil);
    end;

    Result := Cur;
  end;

begin
  Result := TStringList.Create;

  // Read bytes from stream
  Stream.Position := 0;
  SetLength(RawText, Stream.Size);
  if Stream.Size > 0 then
    Stream.ReadBuffer(RawText[1], Stream.Size);

  // Convert encoding → UTF-8
  if Options.Encoding <> '' then
    Utf8Text := ConvertEncoding(RawText, Options.Encoding, 'utf-8')
  else
  begin
    DetectedEncoding := GuessEncoding(RawText);
    Utf8Text := ConvertEncoding(RawText, DetectedEncoding, 'utf-8');
  end;

  // Create stream UTF-8
  Utf8Stream := TStringStream.Create(Utf8Text, TEncoding.UTF8);

  try
    ReadXMLFile(Doc, Utf8Stream);

    // Apply RecordsPath if needed
    Node := ResolvePath(Doc.DocumentElement, Options.RecordsPath);
    if Node = nil then Exit;

    // Find RecordNodeName
    NodeList := Node.GetChildNodes;
    if NodeList.Count = 0 then Exit;

    Node := NodeList[0];

    // Node attributes
    if Node.Attributes <> nil then
      for i := 0 to Node.Attributes.Length - 1 do
      begin
        Attr := TDOMAttr(Node.Attributes.Item[i]);
        if not (Options.IgnoreNulls and (Attr.Value = '')) then
          Result.Add(Attr.Name);
      end;

    // Child elements
    for j := 0 to Node.ChildNodes.Count - 1 do
    begin
      Child := Node.ChildNodes[j];

      if Child.NodeType = ELEMENT_NODE then
      begin
        if Options.TrimFields then
          Result.Add(Trim(Child.NodeName))
        else
          Result.Add(Child.NodeName);
      end;
    end;

  finally
    Doc.Free;
    Utf8Stream.Free;
  end;
end;

procedure TXMLImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  Doc: TXMLDocument;
  Root, N: TDOMNode;
  i: Integer;
  Row: TXRow;
  RecNodeName: string;
begin
  ReadXMLFile(Doc, Stream);
  try
    Root := Doc.DocumentElement;
    // Padrão: cada child direto é um "record"
    // Ou use Options.Params.Values['recordNode'] para especificar
    RecNodeName := Options.RecordNodeName;

    if (RecNodeName <> '') then
    begin
      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        N := Root.ChildNodes[i];
        if SameText(N.NodeName, RecNodeName) then
        begin
          Row := TXRow.Create;
          try
            NodeToRow(N, '', Row);
            if Assigned(RowOut) then RowOut(Row);
          finally
            Row.Free;
          end;
        end;
      end;
    end
    else
    begin
      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        N := Root.ChildNodes[i];
        Row := TXRow.Create;
        try
          NodeToRow(N, N.NodeName + '.', Row);
          if Assigned(RowOut) then RowOut(Row);
        finally
          Row.Free;
        end;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TXMLImporter.PreviewRows(Stream: TStream; const Options: TImportOptions; MaxRows: Integer;
  RowOut: TXRowConsumer);
var
  Doc: TXMLDocument;
  NodeList: TDOMNodeList;
  Node, Child: TDOMNode;
  Row: TXRow;
  i, j, Count: Integer;
begin
  Stream.Position := 0;
  ReadXMLFile(Doc, Stream);
  try
    NodeList := Doc.GetElementsByTagName(Options.RecordNodeName);
    Count := 0;
    for i := 0 to NodeList.Count - 1 do
    begin
      if Count >= MaxRows then Break;
      Node := NodeList[i];
      Row := TXRow.Create;
      for j := 0 to Node.ChildNodes.Count - 1 do
      begin
        Child := Node.ChildNodes[j];
        if (Child.NodeType = ELEMENT_NODE) then
          Row.Values[Child.NodeName] := Child.TextContent;
      end;
      RowOut(Row);
      Row.Free;
      Inc(Count);
    end;
  finally
    Doc.Free;
  end;
end;

class function TXMLImporter.Probe(const FileName: string; Stream: TStream): Integer;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if (ext = '.xml') then Exit(80);
  Result := 25;
end;

initialization
  TImporterRegistry.RegisterImporter('xml', TXMLImporter);

end.

