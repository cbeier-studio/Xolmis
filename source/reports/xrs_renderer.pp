unit xrs_renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, xrs_core;

type
  { TReportRenderer }
  // Componente responsável pela renderização final e apresentação dos relatórios
  TReportRenderer = class
  private
    FTempFileName: string;
    procedure ClearTempFiles;
    function CreateTempHTML(const AHTMLContent: string): string;
  public
    destructor Destroy; override;
    procedure ShowPreview(AReport: TXolmisReport);

    // Assinaturas preparadas para o futuro (Fase 6)
    procedure ExportToPDF(AReport: TXolmisReport; const AFileName: string);
    procedure PrintReport(AReport: TXolmisReport);
  end;

implementation

destructor TReportRenderer.Destroy;
begin
  ClearTempFiles;
  inherited Destroy;
end;

procedure TReportRenderer.ClearTempFiles;
begin
  if FTempFileName = EmptyStr then
    Exit;
  if FileExists(FTempFileName) and not DeleteFile(FTempFileName) then
    raise Exception.CreateFmt('Could not remove temporary report file "%s".', [FTempFileName]);
  FTempFileName := EmptyStr;
end;

function TReportRenderer.CreateTempHTML(const AHTMLContent: string): string;
var
  Guid: TGUID;
  FileStream: TFileStream;
  Bom: array[0..2] of Byte = ($EF, $BB, $BF);
  Utf8Content: RawByteString;
begin
  ClearTempFiles;

  CreateGUID(Guid);
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'xrs_report_' +
    StringReplace(Copy(GUIDToString(Guid), 2, 36), '-', '', [rfReplaceAll]) + '.html';
  FTempFileName := Result;
  Utf8Content := UTF8Encode(AHTMLContent);
  FileStream := TFileStream.Create(Result, fmCreate);
  try
    FileStream.WriteBuffer(Bom, SizeOf(Bom));
    if Length(Utf8Content) > 0 then
      FileStream.WriteBuffer(Utf8Content[1], Length(Utf8Content));
  finally
    FileStream.Free;
  end;
end;

procedure TReportRenderer.ShowPreview(AReport: TXolmisReport);
begin
  if not Assigned(AReport) then
    raise EArgumentNilException.Create('The report cannot be nil.');

  AReport.GenerateHTML;
  CreateTempHTML(AReport.HTMLContent);

  if not OpenDocument(FTempFileName) then
    raise Exception.CreateFmt('Could not open report preview "%s".', [FTempFileName]);
end;

procedure TReportRenderer.ExportToPDF(AReport: TXolmisReport; const AFileName: string);
begin
  // A ser implementado na Fase 6 com o Chromium
  raise Exception.Create('Exportação PDF estará disponível na Fase 6.');
end;

procedure TReportRenderer.PrintReport(AReport: TXolmisReport);
begin
  // A ser implementado na Fase 6
  raise Exception.Create('Impressão direta estará disponível na Fase 6.');
end;

end.
