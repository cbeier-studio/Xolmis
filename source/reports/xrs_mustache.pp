unit xrs_mustache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynMustache, SynCommons;

type
  { TXolmisMustacheRenderer }

  TXolmisMustacheRenderer = class
  private
    FMustache: TSynMustache;
    FContext: variant;
  public
    constructor Create(const ATemplate: RawUTF8);
    destructor Destroy; override;

    procedure SetContextData(const AVariantContext: variant);
    function RenderHTML: string;
  end;

implementation

{ TXolmisMustacheRenderer }

constructor TXolmisMustacheRenderer.Create(const ATemplate: RawUTF8);
begin
  // Cache compartilhado via SynMustache.Parse
  FMustache := TSynMustache.Parse(ATemplate);
end;

destructor TXolmisMustacheRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TXolmisMustacheRenderer.SetContextData(const AVariantContext: variant);
begin
  // Agora recebemos a estrutura de dados já montada em memória
  FContext := AVariantContext;
end;

function TXolmisMustacheRenderer.RenderHTML: string;
begin
  if FMustache <> nil then
    Result := UTF8ToString(FMustache.Render(FContext))
  else
    Result := '';
end;

end.
