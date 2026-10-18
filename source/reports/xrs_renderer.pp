unit xrs_renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, xrs_core;

type
  { TReportRenderer }
  // Componente responsável pela renderização final e ciclo de vida do arquivo físico
  TReportRenderer = class
  private
    FTempFileName: string;
    procedure ClearTempFiles;
    function CreateTempHTML(const AHTMLContent: string): string;
  public
    destructor Destroy; override;

    // Na Fase 1, abre no navegador do SO. Na Fase 6, abrirá o form com Chromium
    procedure ShowPreview(AReport: TXolmisReport);

    // Assinaturas preparadas para o futuro (Fase 6)
    procedure ExportToPDF(AReport: TXolmisReport; const AFileName: string);
    procedure PrintReport(AReport: TXolmisReport);
  end;

implementation

destructor TReportRenderer.Destroy;
begin
  // Limpeza automática de artefatos temporários ao destruir o renderizador
  ClearTempFiles;
  inherited Destroy;
end;

procedure TReportRenderer.ClearTempFiles;
begin
  if (FTempFileName <> '') and FileExists(FTempFileName) then
  begin
    DeleteFile(FTempFileName);
    FTempFileName := '';
  end;
end;

function TReportRenderer.CreateTempHTML(const AHTMLContent: string): string;
var
  StringList: TStringList;
begin
  // Limpa arquivo anterior se o renderizador for reaproveitado
  ClearTempFiles;

  // Cria um arquivo HTML temporário no diretório de sistema
  Result := GetTempDir(False) + 'xrs_report_' + FormatDateTime('yyyymmddhhnnss', Now) + '.html';

  StringList := TStringList.Create;
  try
    StringList.Text := AHTMLContent;
    StringList.SaveToFile(Result);
    FTempFileName := Result; // Guarda a referência para limpeza futura
  finally
    StringList.Free;
  end;
end;

procedure TReportRenderer.ShowPreview(AReport: TXolmisReport);
begin
  // 1. Pede ao relatório para compilar os dados e gerar o HTML
  AReport.GenerateHTML;

  // 2. Salva o HTML em um arquivo temporário
  CreateTempHTML(AReport.HTMLContent);

  // 3. Exibe o documento.
  // Na Fase 1: Usa o navegador padrão do Windows/Linux/macOS para visualização e testes
  // Na Fase 6: Será TFormChromiumPreview.Create(...)
  OpenDocument(FTempFileName);
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

