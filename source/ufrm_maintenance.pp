{ Xolmis Data Maintenance dialog

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit ufrm_maintenance;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,
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
    tvMenu: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure sbRecreateImageThumbnailsClick(Sender: TObject);
    procedure sbRewriteTaxaHierarchyClick(Sender: TObject);
    procedure tvMenuSelectionChanged(Sender: TObject);
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

procedure TfrmMaintenance.tvMenuSelectionChanged(Sender: TObject);
begin
  PG.PageIndex := tvMenu.Selected.Index;
end;

procedure TfrmMaintenance.FormShow(Sender: TObject);
begin
  tvMenu.Selected := tvMenu.Items.GetFirstNode;
end;

procedure TfrmMaintenance.sbRecreateImageThumbnailsClick(Sender: TObject);
begin
  if MsgDlg(rsTitleRecreateThumbnails, rsRecreateThumbnailsPrompt, mtConfirmation) then
    RecreateThumbnails;
end;

end.

