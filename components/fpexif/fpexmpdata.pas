{ Implements access to XMP metadata.
  Reads the XMP segment into an internal string field.
  Use LoadFromStream and SaveToStream methods to read or write this field.
  There is also a simple "tag-like" interface, however, it is implemented only
  for reading the meta data.

  File good for testing:
  https://commons.wikimedia.org/wiki/File:Metadata_test_file_-_includes_data_in_IIM,_XMP,_and_Exif.jpg
}

unit fpeXMPData;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs,
  {$IFDEF FPC}
  laz2_dom, laz2_xmlread,
  {$ELSE}
  XMLDoc, XMLIntf,
  {$ENDIF}
  fpeGlobal, fpeTags;

type
  TXMPData = class
  private
    FData: AnsiString;
    {$IFDEF FPC}
    FDoc: TXMLDocument;
    {$ELSE}
    FDoc: IXMLDocument;
    {$ENDIF}
    FTags: TStringList;
    function GetTagByIndex(AIndex: Integer): String;
    function GetTagByName(ATagName: String): String;
    function GetTagName(AIndex: Integer): String;
    function GetTagCount: Integer;
  protected
    procedure Create_RDFDescription_Tags(ANode: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF});
    procedure CreateTags;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExportToStrings(AList: TStrings; AOptions: TExportOptions; ASeparator: String = '=');
    procedure LoadFromStream(AStream: TStream; ASize: Integer = -1);
    procedure SaveToStream(AStream: TStream);
    property TagByIndex[AIndex: Integer]: String read GetTagByIndex;
    property TagByName[ATagName: String]: String read GetTagByName;
    property TagName[AIndex: Integer]: String read GetTagName;
    property TagCount: Integer read GetTagCount;
  end;
      

implementation
  
{ TXMPData }
  
constructor TXMPData.Create;
begin
  inherited;
  FTags := TStringList.Create; 
end;

destructor TXMPData.Destroy; 
begin
  {$IFDEF FPC}
  FDoc.Free;
  {$ENDIF}
  FTags.Free;
  inherited;
end;

procedure TXMPData.Create_RDFDescription_Tags(ANode: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF});
var
  {$IFDEF FPC}
  node: TDOMNode;
  attr: TDOMNode;
  {$ELSE}
  node: IXMLNode;
  attr: IXMLNode;
  {$ENDIF}
  nodeName: String;
  i: Integer;
  lTagName, lTagValue: String;
begin
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    {$IFDEF FPC}
    for i := 0 to ANode.Attributes.Length-1 do
    begin
      attr := ANode.Attributes.Item[i];
      lTagName := attr.NodeName;
      lTagValue := attr.nodeValue;
      FTags.Add(lTagName + '=' + lTagValue);
    end;
    if ANode.HasChildNodes then
    begin
      node := ANode.FirstChild;
      while node <> nil do
      begin
        nodeName := node.NodeName;
        {$IFDEF FPC}
        lTagValue := node.TextContent;
        {$ELSE}
        lTagValue := node.NodeValue;
        {$ENDIF}
        if lTagName <> '' then
          FTags.Add(nodeName + '=' + lTagValue);
        node := node.NextSibling;
      end;
    end;
    ANode := ANode.NextSibling;
    {$ELSE}
    for i := 0 to ANode.AttributeNodes.Count-1 do
    begin
      attr := ANode.AttributeNodes[i];
      lTagName := attr.NodeName;
      lTagValue := attr.NodeValue;
      FTags.Add(lTagName + '=' + lTagValue);
    end;
    if ANode.HasChildNodes then
    begin
      node := ANode.ChildNodes.First;
      while node <> nil do 
      begin
        nodeName := node.NodeName;
        {$IFDEF FPC}
        lTagValue := node.TextContent;
        {$ELSE}
        lTagValue := node.NodeValue;
        {$ENDIF}
        if lTagName <> '' then
          FTags.Add(nodeName + '=' + lTagValue);
        node := node.NextSibling;
      end;
    end;
    ANode := ANode.NextSibling;
    {$ENDIF}
  end;
end;
    
procedure TXMPData.CreateTags;
var
  {$IFDEF FPC}
  node: TDOMNode;
  {$ELSE}
  node: IXMLNode;
  {$ENDIF}
  nodeName: String;
  stream: TStringStream;
begin
  {$IFDEF FPC}
  FDoc.Free;
  {$ENDIF}
  stream := TStringStream.Create(FData);
  try
    {$IFDEF FPC}
    ReadXMLFile(FDoc, stream);
    {$ELSE}
    FDoc := TXMLDocument.Create(nil);
    FDoc.Options := FDoc.Options - [doNodeAutoCreate];
    FDoc.LoadFromStream(stream, xetUTF_8);
    {$ENDIF}
  finally
    stream.Free;
  end;
  
  FTags.Clear;
  try
    {$IFDEF FPC}
    node := FDoc.DocumentElement.FindNode('rdf:RDF');
    if node = nil then exit;
    node := node.FirstChild;
    {$ELSE}

    node := FDoc.DocumentElement;
    if node.ChildNodes.Count = 0 then
      exit;
    node := node.ChildNodes.First;
    nodename :=node.NodeName;
    if nodeName <> 'rdf:RDF' then
      exit;
    node := node.ChildNodes.First;
    {$ENDIF}
    while node <> nil do
    begin
      nodeName := node.NodeName;
      if nodeName = 'rdf:Description' then
        Create_RDFDescription_Tags(node);
      node := node.NextSibling;
    end;
  except
    FTags.Clear;
    {$IFDEF FPC}
    FreeAndNil(FDoc);
    {$ENDIF}
    raise;
  end;
end;

procedure TXMPData.ExportToStrings(AList: TStrings; AOptions: TExportOptions;
  ASeparator: String = '=');
var
  i: Integer;
  tagnam: String;
  tagval: String;
  usedExportOptions: TExportOptions;
begin
  Assert(AList <> nil);

  if TagCount = 0 then
    exit;

  if AList.Count > 0 then
    AList.Add('');
  AList.Add('*** XMP ***');

  for i := 0 to TagCount-1 do begin
    tagNam := TagName[i];
    tagVal := TagByIndex[i];
    {
    usedExportOptions := AOptions * [eoShowDecimalTagID, eoShowHexTagID];
    if usedExportOptions = [eoShowDecimalTagID] then
      nam := Format('[%d] %s', [tag.TagID, tag.Description])
    else
    if usedExportOptions = [eoShowHexTagID] then
      nam := Format('[$%.4x] %s', [tag.TagID, tag.Description])
    else
      nam := tag.Description;
    tagval := tag.AsString;
    }
    if tagval <> '' then
      AList.Add(tagnam + ASeparator + tagval);
  end;
end;

function TXMPData.GetTagByIndex(AIndex: Integer):  String;
begin
  Result := FTags.ValueFromIndex[AIndex];
end;

function TXMPData.GetTagByName(ATagName: String): String;
begin
  Result := FTags.Values[ATagName];;
end;

function TXMPData.GetTagCount: Integer;
begin
  Result := FTags.Count;
end;

function TXMPData.GetTagName(AIndex: Integer): String;
begin
  Result := FTags.Names[AIndex];
end;

procedure TXMPData.LoadFromStream(AStream: TStream; ASize: Integer = -1);
var
  p: Int64;
  i: Cardinal;
begin
  if ASize = -1 then 
    ASize := AStream.Size;
  SetLength(FData, ASize);
  p := AStream.Position;
  AStream.Read(FData[1], ASize);

  // Sometimes there are incomplete xml files, missing the initial '<'.
  // https://superuser.com/questions/1389971/error-0x80070057-the-parameter-is-incorrect-when-editing-jpeg-metadata
  // Fixing this is better than rejecting the file...
  if pos('?xpacket', FData) = 1 then
  begin
    SetLength(FData, ASize+1);
    for i := ASize downto 1 do
      FData[i+1] := FData[i];
    FData[1] := '<';
  end;

  // The xml parser does not like zero bytes at the end --> remove them
  i := Length(FData);
  while (FData[i] = #0) and (i > 0) do
  begin
    dec(i);
    SetLength(FData, i);
  end;

  AStream.Position := p;
  CreateTags;
end;

procedure TXMPData.SaveToStream(AStream: TStream);
begin
  if FData <> '' then
    AStream.Write(FData[1], Length(FData));
end;

end.

