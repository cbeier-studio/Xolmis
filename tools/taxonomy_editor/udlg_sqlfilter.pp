unit udlg_sqlfilter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, SynEdit, SynCompletion,
  SynHighlighterSQL, atshapelinebgra;

type

  { TdlgSqlFilter }

  TdlgSqlFilter = class(TForm)
    pBottom: TPanel;
    sbApply: TBitBtn;
    sbClose: TButton;
    seScript: TSynEdit;
    SynCompletion1: TSynCompletion;
    SynSQLSyn1: TSynSQLSyn;
  private

  public

  end;

var
  dlgSqlFilter: TdlgSqlFilter;

implementation

initialization
  {$I unit1.lrs}

end.

