unit udlg_markdown;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Pixie.MarkdownView, Pixie.Types;

const
  CDefaultDarkCss =
    'body { ' +
      'font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", ' +
      'Roboto, Helvetica, Arial, sans-serif; ' +
      'font-size: 16px; ' +
      'line-height: 1.5; ' +
      'color: #e6edf3; ' +
      'background-color: #0d1117; ' +
      'max-width: 980px; ' +
      'margin: 0 auto; ' +
      'padding: 32px 40px; ' +
    '}'#10 +

    'h1, h2, h3, h4, h5, h6 { ' +
      'margin-top: 24px; ' +
      'margin-bottom: 16px; ' +
      'font-weight: 600; ' +
      'line-height: 1.25; ' +
    '}'#10 +
    'h1 { font-size: 2em; ' +
      'border-bottom: 1px solid #30363d; padding-bottom: 0.3em; }'#10 +
    'h2 { font-size: 1.5em; ' +
      'border-bottom: 1px solid #30363d; padding-bottom: 0.3em; }'#10 +
    'h3 { font-size: 1.25em; }'#10 +
    'h4 { font-size: 1em; }'#10 +
    'h5 { font-size: 0.875em; }'#10 +
    'h6 { font-size: 0.85em; color: #8d96a0; }'#10 +

    'p { margin-top: 0; margin-bottom: 16px; }'#10 +

    'a { color: #4493f8; text-decoration: none; }'#10 +
    'a:hover { text-decoration: underline; }'#10 +

    'strong { font-weight: 600; }'#10 +
    'em { font-style: italic; }'#10 +
    'del { text-decoration: line-through; }'#10 +

    'code { ' +
      'font-family: ui-monospace, "SF Mono", "Cascadia Code", ' +
      '"Source Code Pro", Menlo, Consolas, "Liberation Mono", monospace; ' +
      'font-size: 85%; ' +
      'padding: 0.2em 0.4em; ' +
      'background-color: #6e768166; ' +
      'border-radius: 6px; ' +
    '}'#10 +
    'pre { ' +
      'font-family: ui-monospace, "SF Mono", "Cascadia Code", ' +
      '"Source Code Pro", Menlo, Consolas, "Liberation Mono", monospace; ' +
      'font-size: 85%; ' +
      'line-height: 1.45; ' +
      'padding: 16px; ' +
      'overflow: auto; ' +
      'background-color: #161b22; ' +
      'border-radius: 6px; ' +
      'margin-top: 0; margin-bottom: 16px; ' +
    '}'#10 +
    'pre code { ' +
      'padding: 0; ' +
      'background-color: transparent; ' +
      'border-radius: 0; ' +
      'font-size: 100%; ' +
    '}'#10 +

    'blockquote { ' +
      'margin: 0 0 16px 0; ' +
      'padding: 0 1em; ' +
      'color: #8d96a0; ' +
      'border-left: 0.25em solid #30363d; ' +
    '}'#10 +
    'blockquote > :last-child { margin-bottom: 0; }'#10 +

    'ul, ol { margin-top: 0; margin-bottom: 16px; padding-left: 2em; }'#10 +
    'li { margin-bottom: 0.25em; }'#10 +
    'li > p { margin-top: 16px; margin-bottom: 16px; }'#10 +
    'ul ul, ul ol, ol ul, ol ol { margin-top: 0; margin-bottom: 0; }'#10 +
    'li.task-list-item { list-style-type: none; }'#10 +
    'li.task-list-item input[type="checkbox"] { ' +
      'margin: 0 0.2em 0.25em -1.6em; ' +
      'vertical-align: middle; ' +
    '}'#10 +

    'hr { ' +
      'height: 0.25em; ' +
      'padding: 0; ' +
      'margin: 24px 0; ' +
      'background-color: #30363d; ' +
      'border: 0; ' +
    '}'#10 +

    'table { ' +
      'border-collapse: collapse; ' +
      'border-spacing: 0; ' +
      'margin-top: 0; margin-bottom: 16px; ' +
    '}'#10 +
    'table th, table td { ' +
      'padding: 6px 13px; ' +
      'border: 1px solid #30363d; ' +
    '}'#10 +
    'table th { ' +
      'font-weight: 600; ' +
      'background-color: #161b22; ' +
    '}'#10 +
    'table tr:nth-child(2n) { background-color: #161b22; }'#10 +

    'img { max-width: 100%; height: auto; }'#10 +

    'kbd { ' +
      'display: inline-block; ' +
      'padding: 3px 5px; ' +
      'font-family: ui-monospace, "SF Mono", Menlo, Consolas, monospace; ' +
      'font-size: 11px; ' +
      'line-height: 10px; ' +
      'color: #b1bac4; ' +
      'vertical-align: middle; ' +
      'background-color: #161b22; ' +
      'border: solid 1px #30363d; ' +
      'border-bottom-color: #21262d; ' +
      'border-radius: 6px; ' +
      'box-shadow: inset 0 -1px 0 #21262d; ' +
    '}'#10;

type

  { TdlgMarkdown }

  TdlgMarkdown = class(TForm)
    MdView: TPixieMarkdownView;
    procedure FormShow(Sender: TObject);
    procedure MdViewClick(Sender: TObject);
  private
    FTitle, FMdText: String;
    procedure SetMarkdownText(AValue: String);
    procedure SetTitle(AValue: String);
  public
    property Title: String read FTitle write SetTitle;
    property MarkdownText: String read FMdText write SetMarkdownText;
  end;

var
  dlgMarkdown: TdlgMarkdown;

implementation

uses
  uDarkStyleParams;

{$R *.lfm}

{ TdlgMarkdown }

procedure TdlgMarkdown.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    MdView.UserCss := CDefaultDarkCss;
end;

procedure TdlgMarkdown.MdViewClick(Sender: TObject);
begin
  Close;
end;

procedure TdlgMarkdown.SetMarkdownText(AValue: String);
begin
  FMdText := AValue;
  MdView.Markdown.Text := FMdText;
end;

procedure TdlgMarkdown.SetTitle(AValue: String);
begin
  FTitle := AValue;
  Caption := FTitle;
end;

end.

