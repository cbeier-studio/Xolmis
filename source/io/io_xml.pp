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
  Classes, SysUtils, fgl, DOM, XMLRead, XMLWrite, LConvEncoding, Math, io_core;

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

  { TXolmisXMLExporter }

  TXolmisXMLExporter = class(TExporter)
  private
    FRows: specialize TFPGObjectList<TXRow>;
    function SanitizeXMLName(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRow(Row: TXRow);
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Export(Stream: TStream; const Options: TExportOptions; RowOut: TXRowConsumer); override;
  end;

implementation

uses
  utils_global;

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

  LogEvent(leaStart, 'Get XML file column names');

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
    LogEvent(leaFinish, 'Get XML file column names');
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
  LogEvent(leaStart, 'Import XML file');

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

        if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
          Break;
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

        if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
          Break;
      end;
    end;
  finally
    Doc.Free;
    LogEvent(leaFinish, 'Import XML file');
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

  LogEvent(leaStart, 'Preview XML file');
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
    LogEvent(leaFinish, 'Preview XML file');
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

{ TXolmisXMLExporter }

constructor TXolmisXMLExporter.Create;
begin
  inherited Create;
  FRows := specialize TFPGObjectList<TXRow>.Create(True);
end;

destructor TXolmisXMLExporter.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

procedure TXolmisXMLExporter.AddRow(Row: TXRow);
begin
  FRows.Add(Row);
end;

function TXolmisXMLExporter.SanitizeXMLName(const S: string): string;
var
  i: Integer;
  C: Char;
begin
  Result := Trim(S);
  if Result = '' then
    Exit('field');

  for i := 1 to Length(Result) do
  begin
    C := Result[i];
    if not (C in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.']) then
      Result[i] := '_';
  end;

  if not (Result[1] in ['A'..'Z', 'a'..'z', '_']) then
    Result := '_' + Result;
end;

function TXolmisXMLExporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := SameText(Ext, 'xml');
end;

procedure TXolmisXMLExporter.Export(Stream: TStream; const Options: TExportOptions;
  RowOut: TXRowConsumer);
var
  FieldList: TStringList;
  Doc: TXMLDocument;
  RootNode, RecordNode, FieldNode: TDOMElement;
  Row: TXRow;
  RootName, RecordName, FieldName, FieldValue: string;
  i, j: Integer;
begin
  LogEvent(leaStart, 'Export XML file');

  FieldList := nil;
  if Options.ExportFields <> '' then
  begin
    FieldList := TStringList.Create;
    FieldList.CommaText := Options.ExportFields;
  end;

  RootName := Trim(Options.RootNodeName);
  if RootName = '' then
    RootName := 'records';

  RecordName := Trim(Options.RecordNodeName);
  if RecordName = '' then
    RecordName := 'record';

  RootName := SanitizeXMLName(RootName);
  RecordName := SanitizeXMLName(RecordName);

  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement(RootName);
    Doc.AppendChild(RootNode);

    for i := 0 to FRows.Count - 1 do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then
        Break;

      Row := FRows[i];
      RecordNode := Doc.CreateElement(RecordName);
      RootNode.AppendChild(RecordNode);

      for j := 0 to Row.Count - 1 do
      begin
        FieldName := Row.Names[j];
        FieldValue := Row.ValueFromIndex[j];

        if Assigned(FieldList) and (FieldList.IndexOf(FieldName) < 0) then
          Continue;

        if Options.TrimFields then
          FieldValue := Trim(FieldValue);

        if Options.IgnoreNulls and (FieldValue = '') then
          Continue;

        FieldNode := Doc.CreateElement(SanitizeXMLName(FieldName));
        FieldNode.AppendChild(Doc.CreateTextNode(FieldValue));
        RecordNode.AppendChild(FieldNode);
      end;

      if Assigned(RowOut) then
        RowOut(Row);

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((i + 1) * 100.0 / Max(1, FRows.Count)), 'Exportando XML');
    end;

    WriteXMLFile(Doc, Stream);
  finally
    Doc.Free;
    FieldList.Free;
    LogEvent(leaFinish, 'Export XML file');
  end;
end;

initialization
  TImporterRegistry.RegisterImporter('xml', TXMLImporter);
  TExporterRegistry.RegisterExporter('xml', TXolmisXMLExporter);

end.

