unit ufrm_customlist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, Buttons,
  atshapelinebgra, CheckLst, DBCtrls, DBGrids, BCPanel, BCButton, ColorSpeedButton, DBControlGrid, rxswitch,
  cbs_datatypes;

type

  { TfrmCustomList }

  TfrmCustomList = class(TForm)
    sbRecycleRestore: TColorSpeedButton;
    sbRecycleDelete: TColorSpeedButton;
    navGrid: TDBGrid;
    pViewsToggle: TBCPanel;
    bvSpacerFilters: TBevel;
    gridView: TDBGrid;
    dsLink: TDataSource;
    dbgRecycle: TDBControlGrid;
    lblRecordStatus: TLabel;
    icoMarkedFilter: TImage;
    icoUnmarkedFilter: TImage;
    lblMarkedFilter: TLabel;
    lblRecycleName: TDBText;
    lblRecycleModifiedDate: TDBText;
    DBText3: TDBText;
    lblUnmarkedFilter: TLabel;
    nbViews: TNotebook;
    pgListView: TPage;
    pgGridView: TPage;
    pMarkedFilter: TBCPanel;
    pMarksFilters: TBCPanel;
    pRecordStatus: TBCPanel;
    pRecycleContent: TPanel;
    pUnmarkedFilter: TBCPanel;
    pRecordToolbar: TBCPanel;
    sbCancelRecord: TSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbFirstRecord: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sbPriorRecord: TSpeedButton;
    sbRecordHistory: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbSaveRecord: TSpeedButton;
    scrollFields: TScrollBox;
    Separator7: TShapeLineBGRA;
    Separator8: TShapeLineBGRA;
    sbListView: TSpeedButton;
    sbGridView: TSpeedButton;
    SplitLeft: TSplitter;
    titleImages: TLabel;
    pgAudio: TPage;
    pgDocs: TPage;
    pgImages: TPage;
    pgQuickFilters: TPage;
    pgRecycle: TPage;
    pgSummary: TPage;
    nbSide: TNotebook;
    lineRight: TShapeLineBGRA;
    pContent: TPanel;
    pmgDel: TMenuItem;
    pmgEdit: TMenuItem;
    pmgRecordHistory: TMenuItem;
    pmgRefresh: TMenuItem;
    pmGrid: TPopupMenu;
    pSide: TPanel;
    pSideToolbar: TPanel;
    sbShowAudio: TSpeedButton;
    sbShowDocs: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbShowSummary: TSpeedButton;
    scrollFilter: TScrollBox;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    SplitRight: TSplitter;
    TimerUpdate: TTimer;
    titleAudio: TLabel;
    titleImages1: TLabel;
    titleRecycle: TLabel;
    titleSummary: TLabel;
    titleQuickFilters: TLabel;
    tsfMarked: TRxSwitch;
    tsfUnmarked: TRxSwitch;
    procedure dsLinkStateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure pmgRefreshClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbGridViewClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbListViewClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShowQuickFiltersClick(Sender: TObject);
  private
    //FSearchString, OldSearchString: String;
    //procedure SetSearchString(aValue: String);
    procedure UpdateButtons(aDataSet: TDataSet);
  public
    //CanToggle: Boolean;
    //property SearchString: String read FSearchString write SetSearchString;
  end;

var
  frmCustomList: TfrmCustomList;

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs,{$IFDEF DEBUG} cbs_debug,{$ENDIF} udm_main;

{ TfrmCustomList }

procedure TfrmCustomList.dsLinkStateChange(Sender: TObject);
begin
  UpdateButtons(dsLink.DataSet);
end;

procedure TfrmCustomList.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TimerUpdate.Enabled := False;
end;

procedure TfrmCustomList.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { If editing when closing, ask what the user want to do }
  if (dsLink.State in [dsInsert, dsEdit]) then
  begin
    case MessageDlg(rsModificationsNotSaved, rsPostBeforeClosePrompt, mtConfirmation,
      mbYesNoCancel, 0, mbCancel) of
      { Save modifications and close }
      mrYes:
        begin
          sbSaveRecordClick(nil);
          CanClose := True;
        end;
      { Cancel modifications and close }
      mrNo:
        begin
          sbCancelRecordClick(nil);
          CanClose := True;
        end;
      { Do not close }
      mrCancel:
        CanClose := False;
    end;
  end;
end;

procedure TfrmCustomList.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #27) then     { <ESC> key }
  begin
    //GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    if (dsLink.DataSet.State in [dsInsert, dsEdit]) then
    begin
      { CANCEL }
      sbCancelRecordClick(nil);
    //end else
    //begin
      { CLEAR SEARCH }
      //if (pSearch.Visible) then
        //sbClearSearchClick(nil);
    end;
    Key := #0;
  end;
end;

procedure TfrmCustomList.pmgRefreshClick(Sender: TObject);
begin
  dsLink.DataSet.Refresh;
end;

procedure TfrmCustomList.sbCancelRecordClick(Sender: TObject);
begin
  if (dsLink.DataSet.Modified) and (XSettings.ConfirmCancel) then
  begin
    if not MsgDlg(rsDiscardChangesTitle, rsCancelEditingPrompt, mtConfirmation) then
      Exit;
  end;

  dsLink.DataSet.Cancel;
end;

procedure TfrmCustomList.sbDelRecordClick(Sender: TObject);
begin
  dsLink.DataSet.Refresh;
  UpdateButtons(dsLink.DataSet);
end;

procedure TfrmCustomList.sbEditRecordClick(Sender: TObject);
begin
  if nbViews.PageIndex > 0 then
    nbViews.PageIndex := 0;
  sbListView.Down := nbViews.PageIndex = 0;

  dsLink.DataSet.Edit;
end;

procedure TfrmCustomList.sbFirstRecordClick(Sender: TObject);
begin
  dsLink.DataSet.First;
end;

procedure TfrmCustomList.sbGridViewClick(Sender: TObject);
begin
  if sbGridView.Down then
  begin
    sbGridView.Flat := False;
    sbListView.Flat := True;
    nbViews.PageIndex := 1;
  end;
end;

procedure TfrmCustomList.sbInsertRecordClick(Sender: TObject);
begin
  if nbViews.PageIndex > 0 then
    nbViews.PageIndex := 0;
  sbListView.Down := nbViews.PageIndex = 0;

  dsLink.DataSet.Insert;
end;

procedure TfrmCustomList.sbLastRecordClick(Sender: TObject);
begin
  dsLink.DataSet.Last;
end;

procedure TfrmCustomList.sbListViewClick(Sender: TObject);
begin
  if sbListView.Down then
  begin
    sbListView.Flat := False;
    sbGridView.Flat := True;
    nbViews.PageIndex := 0;
  end;
end;

procedure TfrmCustomList.sbNextRecordClick(Sender: TObject);
begin
  dsLink.DataSet.Next;
end;

procedure TfrmCustomList.sbPriorRecordClick(Sender: TObject);
begin
  dsLink.DataSet.Prior;
end;

procedure TfrmCustomList.sbRefreshRecordsClick(Sender: TObject);
begin
  dsLink.DataSet.Refresh;
end;

procedure TfrmCustomList.sbSaveRecordClick(Sender: TObject);
begin
  dsLink.DataSet.Post;
end;

procedure TfrmCustomList.sbShowQuickFiltersClick(Sender: TObject);
begin
  pSide.Visible := (Sender as TSpeedButton).Down;

  if (Sender as TSpeedButton).Down then
    nbSide.PageIndex := (Sender as TSpeedButton).Tag;
end;

//procedure TfrmCustomList.SetSearchString(aValue: String);
//begin
//  if not CanToggle then
//    Exit;
//
//  if FSearchString <> OldSearchString then
//    OldSearchString := FSearchString;
//  FSearchString := aValue;
//
//  Search(FSearchString);
//end;

procedure TfrmCustomList.UpdateButtons(aDataSet: TDataSet);
begin
  if (aDataSet.State in [dsInsert, dsEdit]) then
  begin
    sbEditRecord.Enabled := False;
    sbDelRecord.Enabled := False;
    sbFirstRecord.Enabled := False;
    sbPriorRecord.Enabled := False;
    sbNextRecord.Enabled := False;
    sbLastRecord.Enabled := False;
    sbRecordHistory.Enabled := False;

    sbShowQuickFilters.Enabled := False;
    sbShowImages.Enabled := False;
    sbShowAudio.Enabled := False;
    sbShowDocs.Enabled := False;
    sbShowSummary.Enabled := False;
    sbShowRecycle.Enabled := False;

    sbCancelRecord.Visible := True;
    sbSaveRecord.Visible := True;

    pmgRefresh.Enabled := False;

    navGrid.Enabled := False;
    pSide.Enabled := False;
  end
  else
  begin
    if (aDataSet.Active) and not (TSQLQuery(aDataSet).ReadOnly) then
    begin
      sbEditRecord.Enabled := (aDataSet.RecordCount > 0);
      sbDelRecord.Enabled := (aDataSet.RecordCount > 0);
      sbRecordHistory.Enabled := (aDataSet.RecordCount > 0);
    end
    else
    begin
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;
      sbRecordHistory.Enabled := False;
    end;
    sbFirstRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbPriorRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbNextRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
    sbLastRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

    sbShowQuickFilters.Enabled := True;
    sbShowImages.Enabled := True;
    sbShowAudio.Enabled := True;
    sbShowDocs.Enabled := True;
    sbShowSummary.Enabled := True;
    sbShowRecycle.Enabled := True;

    pmgRefresh.Enabled := True;

    sbSaveRecord.Visible := False;
    sbCancelRecord.Visible := False;

    navGrid.Enabled := True;
    pSide.Enabled := True;
  end;
  pmgEdit.Enabled := sbEditRecord.Enabled;
  pmgDel.Enabled := sbDelRecord.Enabled;
  pmgRecordHistory.Enabled := sbRecordHistory.Enabled;
end;

initialization
  {$I ufrm_customlist.lrs}

end.

