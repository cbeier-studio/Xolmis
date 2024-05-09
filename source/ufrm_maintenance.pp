unit ufrm_maintenance;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons,
  attabs, atshapelinebgra, BCPanel;

type

  { TfrmMaintenance }

  TfrmMaintenance = class(TForm)
    icoRewriteGeoHierarchy: TImage;
    icoRewriteGeoHierarchy1: TImage;
    lblRewriteGeoHierarchy: TLabel;
    lblRewriteGeoHierarchy1: TLabel;
    lblTitleGeoHierarchy: TLabel;
    lblTitleGeoHierarchy1: TLabel;
    pGeo1: TPanel;
    pRewriteGeoHierarchy1: TBCPanel;
    pRewriteTaxaHierarchy: TBCPanel;
    pRewriteGeoHierarchy: TBCPanel;
    pRewriteTaxaSequence: TBCPanel;
    cbTaxaHierarchy: TComboBox;
    cbTaxaSequence: TComboBox;
    icoRewriteTaxaHierarchy: TImage;
    icoRewriteTaxaSequence: TImage;
    lblRewriteTaxaHierarchy: TLabel;
    lblRewriteTaxaSequence: TLabel;
    pGeo: TPanel;
    sbRewriteIndividualsFullname: TBitBtn;
    sbRewriteTaxaHierarchy: TBitBtn;
    sbRewriteGeoHierarchy: TBitBtn;
    sbRewriteTaxaSequence: TBitBtn;
    scrollGeo: TScrollBox;
    tabGeo: TPage;
    pRecreateImageThumbnails: TBCPanel;
    icoRecreateImageThumbnails: TImage;
    lblRecreateImageThumbnails: TLabel;
    lblTitleTaxaHierarchy: TLabel;
    lblTitleImageThumbnails: TLabel;
    lineBottom: TShapeLineBGRA;
    navTabs: TATTabs;
    pBottom: TPanel;
    pImages: TPanel;
    sbClose: TButton;
    sbRecreateImageThumbnails: TBitBtn;
    scrollImages: TScrollBox;
    tabImages: TPage;
    pTaxonomy: TPanel;
    PG: TNotebook;
    scrollTaxonomy: TScrollBox;
    tabIndividuals: TPage;
    tabTaxonomy: TPage;
    procedure navTabsTabChanged(Sender: TObject);
    procedure sbRecreateImageThumbnailsClick(Sender: TObject);
    procedure sbRewriteTaxaHierarchyClick(Sender: TObject);
  private

  public

  end;

var
  frmMaintenance: TfrmMaintenance;

implementation

uses cbs_locale, cbs_dialogs, cbs_taxonomy, cbs_blobs, cbs_graphics, cbs_fullnames;

{$R *.lfm}

{ TfrmMaintenance }

procedure TfrmMaintenance.sbRewriteTaxaHierarchyClick(Sender: TObject);
begin
  case cbTaxaHierarchy.ItemIndex of
    0: RewriteTaxonHierarchy;
    1: ;
  end;
end;

procedure TfrmMaintenance.navTabsTabChanged(Sender: TObject);
begin
  PG.PageIndex := navTabs.TabIndex;
end;

procedure TfrmMaintenance.sbRecreateImageThumbnailsClick(Sender: TObject);
begin
  if MsgDlg(rsTitleRecreateThumbnails, rsRecreateThumbnailsPrompt, mtConfirmation) then
    RecreateThumbnails;
end;

end.

