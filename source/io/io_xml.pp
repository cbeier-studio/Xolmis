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
  Classes, SysUtils, DOM, XMLRead, io_core;

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
  Doc: TXMLDocument;
  NodeList: TDOMNodeList;
  Node, Child: TDOMNode;
  j: Integer;
begin
  Result := TStringList.Create;
  Stream.Position := 0;
  ReadXMLFile(Doc, Stream);
  try
    NodeList := Doc.GetElementsByTagName(Options.RecordNodeName);
    if NodeList.Count > 0 then
    begin
      Node := NodeList[0];
      for j := 0 to Node.ChildNodes.Count - 1 do
      begin
        Child := Node.ChildNodes[j];
        if (Child.NodeType = ELEMENT_NODE) then
          Result.Add(Child.NodeName);
      end;
    end;
  finally
    Doc.Free;
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

