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
    FContextJSON: RawUTF8;
    FHasContext: Boolean;
    FUseJSONContext: Boolean;
  public
    constructor Create(const ATemplate: RawUTF8);
    destructor Destroy; override;

    procedure SetContextData(const AVariantContext: variant);
    procedure SetContextJSON(const AJSONContext: RawUTF8);
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
  FContext := AVariantContext;
  FHasContext := True;
  FUseJSONContext := False;
end;

procedure TXolmisMustacheRenderer.SetContextJSON(const AJSONContext: RawUTF8);
begin
  FContextJSON := AJSONContext;
  FHasContext := True;
  FUseJSONContext := True;
end;

function TXolmisMustacheRenderer.RenderHTML: string;
begin
  if FMustache = nil then
    raise Exception.Create('The Mustache template could not be parsed.');
  if not FHasContext then
    raise Exception.Create('Mustache context data has not been set.');

  // Evita UTF8ToString (converte para a code page ANSI); mantém os bytes UTF-8
  if FUseJSONContext then
    Result := FMustache.RenderJSON(FContextJSON)
  else
    Result := FMustache.Render(FContext);
end;

end.
