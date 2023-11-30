unit udlg_edithierarchy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, atshapelinebgra, BCPanel;

type

  { TdlgEditHierarchy }

  TdlgEditHierarchy = class(TForm)
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    BCPanel3: TBCPanel;
    BCPanel4: TBCPanel;
    BCPanel5: TBCPanel;
    BCPanel6: TBCPanel;
    BCPanel7: TBCPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    eDestinationTaxon: TEditButton;
    eDestinationTaxon1: TEditButton;
    eDestinationTaxon2: TEditButton;
    eDestinationTaxon3: TEditButton;
    eDestinationTaxon4: TEditButton;
    eDestinationTaxon5: TEditButton;
    eDestinationTaxon6: TEditButton;
    lblDestinationTaxon: TLabel;
    lblDestinationTaxon1: TLabel;
    lblDestinationTaxon2: TLabel;
    lblDestinationTaxon3: TLabel;
    lblDestinationTaxon4: TLabel;
    lblDestinationTaxon5: TLabel;
    lblDestinationTaxon6: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbClose: TButton;
    sbApply: TBitBtn;
  private

  public

  end;

var
  dlgEditHierarchy: TdlgEditHierarchy;

implementation

initialization
  {$I unit1.lrs}

end.

