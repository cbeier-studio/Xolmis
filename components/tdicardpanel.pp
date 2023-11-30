(*
  TDI Card Panel
  Based on:

  TDI - Tabbed Document Interface for Lazarus - Show multiple forms in Tabs
  Copyright (C) 2012  Daniel Simões de Almeida

  You can get the latest version of this file in Lazarus CCR, located in:
  https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/components/tdi

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
  You can also get a copy of the license accessing the address:
  http://www.opensource.org/licenses/lgpl-license.php

  Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br
       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170
*)

unit TDICardPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, LMessages, LCLVersion;

resourcestring
  sOwnerIsNotWinControl = 'TDICardPanel.Owner is not a TWinControl descendant';
  sFormNotAssigned = 'Parameter AForm not Assigned';
  sMainMenuNotAssigned = 'TTDINoteBook.MainMenu not assigned';
  sActionTabsMenu = 'Tabs';
  sActionCloseTab = 'Close Tab';
  sActionCloseAllTabs = 'Close All Tabs';
  sActionNextTab = 'Next Tab';
  sActionPreviousTab = 'Previous Tab';

const
  TDIM_CLOSEPAGE = LM_INTERFACELAST + 500;

type
  ETDIError = class( Exception ) ;

  TTDIBackgroundCorner = (coTopLeft, coTopRight, coBottomLeft, coBottomRight);

  { TTDIAction }

  TTDIAction = class( TPersistent )
  private
    FCaption : String ;
    FImageIndex : Integer ;
    FVisible : Boolean ;
  public
    Constructor Create ;
  published
    property Caption    : String  read FCaption    write FCaption ;
    property ImageIndex : Integer read FImageIndex write FImageIndex ;
    property Visible    : Boolean read FVisible    write FVisible;
  end ;

  { TTDIActions }

  TTDIActions = Class( TPersistent )
  private
    FCloseAllTabs : TTDIAction ;
    FCloseTab : TTDIAction ;
    FNextTab : TTDIAction ;
    FPreviousTab : TTDIAction ;
    FTabsMenu : TTDIAction ;
  public
    Constructor Create ;
    Destructor Destroy ; override;
  published
    property TabsMenu     : TTDIAction read FTabsMenu     write FTabsMenu ;
    property CloseTab     : TTDIAction read FCloseTab     write FCloseTab ;
    property CloseAllTabs : TTDIAction read FCloseAllTabs write FCloseAllTabs ;
    property NextTab      : TTDIAction read FNextTab      write FNextTab ;
    property PreviousTab  : TTDIAction read FPreviousTab  write FPreviousTab ;
  end ;

  { TTDIPage }

  TTDIPage = class(TPage)
  private
    fsFormInPage : TForm ;
    fsFormOldParent: TWinControl;
    fsFormOldCloseEvent : TCloseEvent;
    fsFormOldAlign : TAlign;
    fsFormOldClientRect : TRect;
    fsFormOldBorderStyle : TFormBorderStyle;
    fsLastActiveControl: TWinControl;

    procedure OnResizeTDIPage(Sender : TObject) ;
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure SaveFormProperties ;
    procedure RestoreFormProperties ;

    procedure SetFormInPage(AValue : TForm) ;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckFormAlign ;

  public
    constructor Create(TheOwner: TComponent );  override;
    destructor Destroy ; override;

    procedure RestoreLastFocusedControl ;

    property FormInPage : TForm read fsFormInPage write SetFormInPage ;
    property LastActiveControl : TWinControl read fsLastActiveControl write fsLastActiveControl ;
  end ;

  TTDIOption = ( tdiRestoreLastActiveControl, tdiVerifyIfCanChangePage, tdiEmulateFormOnActivate ) ;
  TTDIOptions = set of TTDIOption ;

  { TTDICardPanel }

  TTDICardPanel = class(TNotebook)
  private
    FBackgroundImage : TImage ;
    FFixedPages : Integer ;
    FBackgroundCorner : TTDIBackgroundCorner ;
    FTDIActions : TTDIActions ;
    FTDIOptions : TTDIOptions ;
    FShortCutClosePage: TShortCut;

    FTimerRestoreLastControl : TTimer;
    FIsRemovingAPage : Boolean;

    procedure SetBackgroundImage(AValue : TImage) ;
    procedure SetBackgroundCorner(AValue : TTDIBackgroundCorner) ;
    procedure SetFixedPages(AValue : Integer) ;

    procedure DrawBackgroundImage ;

    procedure CloseTabClicked( Sender: TObject );
    procedure CloseAllTabsClicked( Sender: TObject );
    procedure NextPageClicked( Sender: TObject );
    procedure PreviousPageClicked( Sender: TObject );

    procedure TimerRestoreLastFocus( Sender: TObject );

    procedure RemoveInvalidPages ;
  protected
    function CanChange: Boolean;
       //{$if (lcl_major > 0) or (lcl_release > 30)} override; {$endif}
    procedure DoChange;
    procedure Loaded;
    procedure RemovePage(Index: Integer);
       //{$if (lcl_major > 0) or (lcl_release > 30)} override; {$endif}

    procedure msg_ClosePage(var Msg: TLMessage); message TDIM_CLOSEPAGE;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent);  override;
    destructor Destroy ; override;
    procedure DoCloseTabClicked(APage: TPage);

    function CreateFormInNewPage( AFormClass: TFormClass; ImageIndex : Integer = -1 ) : TForm;
    procedure ShowFormInPage( AForm: TForm; ImageIndex : Integer = -1 );
    Function FindFormInPages( AForm: TForm): Integer ;

    Function CanCloseAllPages: Boolean ;
    Function CanCloseAPage( APageIndex: Integer): Boolean;

    procedure RestoreLastFocusedControl ;
    procedure ScrollPage( ToForward: Boolean );
    procedure CheckInterface;

    procedure CloseAllTabs;
  published
    property BackgroundImage : TImage read FBackgroundImage write SetBackgroundImage  ;
    property BackgroundCorner : TTDIBackgroundCorner read FBackgroundCorner write SetBackgroundCorner default coBottomRight ;

    property TDIActions : TTDIActions read FTDIActions write FTDIActions ;

    property TDIOptions : TTDIOptions read FTDIOptions write FTDIOptions
      default [ tdiRestoreLastActiveControl, tdiVerifyIfCanChangePage ];
    property ShortCutClosePage: TShortCut read FShortCutClosePage write FShortCutClosePage default 16499;  // Ctrl+F4

    property FixedPages : Integer read FFixedPages write SetFixedPages default 0;
  end;

procedure Register;

implementation

uses
  LCLType;

procedure Register;
begin
  RegisterComponents('CBS',[TTDICardPanel]);
end;

{ TTDIAction }

constructor TTDIAction.Create;
begin
  FCaption    := '';
  FImageIndex := -1;
  FVisible    := True;
end;

{ TTDIActions }

constructor TTDIActions.Create;
begin
  FCloseAllTabs := TTDIAction.Create;
  FCloseAllTabs.Caption := sActionCloseAllTabs;

  FCloseTab := TTDIAction.Create;
  FCloseTab.Caption := sActionCloseTab;

  FNextTab := TTDIAction.Create;
  FNextTab.Caption := sActionNextTab;
  FNextTab.Visible := False;

  FPreviousTab := TTDIAction.Create;
  FPreviousTab.Caption := sActionPreviousTab;
  FPreviousTab.Visible := False;
end;

destructor TTDIActions.Destroy;
begin
  FCloseAllTabs.Free;
  FCloseTab.Free;
  FNextTab.Free;
  FPreviousTab.Free;

  inherited Destroy;
end;

{ TTDIPage }

constructor TTDIPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Self.Parent   := TWinControl( TheOwner ) ;
  Self.OnResize := @OnResizeTDIPage ;

  fsLastActiveControl := nil ;
end;

procedure TTDIPage.CheckFormAlign;
var
  Maximize: Boolean ;
begin
  if not Assigned(fsFormInPage) then exit ;

  Maximize := not (( fsFormInPage.Constraints.MaxWidth <> 0 ) and (fsFormInPage.Width < Width)) ;
  if Maximize then
     Maximize := not (( fsFormInPage.Constraints.MaxHeight <> 0 ) and (fsFormInPage.Height < Height));

  { If Form has MaxConstrains and doesn't fill all the Screen, Centralize on
    TabSheet }
  if not Maximize then
  begin
    fsFormInPage.Align := alNone;

    if (fsFormInPage.Width < Width) then
      fsFormInPage.Left := Trunc( (Width - fsFormInPage.Width) / 2 )
    else
      fsFormInPage.Left := 0 ;

    if (fsFormInPage.Height < Height) then
      fsFormInPage.Top := Trunc( (Height - fsFormInPage.Height) / 2 )
    else
      fsFormInPage.Top := 0 ;
  end
  else
    fsFormInPage.Align := alClient;
end;

destructor TTDIPage.Destroy;
begin
  inherited Destroy;
end;

procedure TTDIPage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  if (Operation = opRemove) and (AComponent = fsFormInPage) then
  begin
    fsFormInPage := nil;
  end ;
end;

procedure TTDIPage.OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Msg: TLMessage;
begin
  if Assigned( fsFormOldCloseEvent ) then
     fsFormOldCloseEvent( Sender, CloseAction );

  if {(CloseAction <> caFree) and} Assigned( fsFormInPage ) then
    RestoreFormProperties;

  fsFormInPage := nil;

  if Assigned( Parent ) then
  begin
    Msg.msg    := TDIM_CLOSEPAGE;
    Msg.lParam := PageIndex;

    Parent.Dispatch( Msg );
  end ;
end;

procedure TTDIPage.OnResizeTDIPage(Sender: TObject);
begin
  CheckFormAlign;
end;

procedure TTDIPage.RestoreFormProperties;
begin
  if not Assigned( fsFormInPage ) then exit ;

{  if ([csDesigning, csDestroying] * fsFormInPage.ComponentState <> []) then
     exit ;}

  fsFormInPage.Visible     := False;  // This prevent OnFormShow be fired
  fsFormInPage.Parent      := fsFormOldParent;
  fsFormInPage.Align       := fsFormOldAlign;
  fsFormInPage.BorderStyle := fsFormOldBorderStyle;
  fsFormInPage.Top         := fsFormOldClientRect.Top;
  fsFormInPage.Left        := fsFormOldClientRect.Left;
  fsFormInPage.Width       := fsFormOldClientRect.Right;
  fsFormInPage.Height      := fsFormOldClientRect.Bottom;
  fsFormInPage.OnClose     := fsFormOldCloseEvent;
end;

procedure TTDIPage.RestoreLastFocusedControl;
var
  FocusRestored: Boolean;
begin
  FocusRestored := False;

  if Assigned( fsLastActiveControl ) then
  begin
    if fsLastActiveControl <> Screen.ActiveControl then
    begin
      if fsLastActiveControl.CanSetFocus then
      begin
        try
           fsLastActiveControl.SetFocus;
           FocusRestored := True;
           //FormInPage.ActiveControl := fsLastActiveControl;
        except
        end ;
      end ;
    end
  end;

  if not FocusRestored then
  begin
    { No LastActiveControle ? Ok, if current Screen control isn't in TabSheet,
      go to first Control on TabSheet... }
    if not Self.ContainsControl( Screen.ActiveControl ) then
      Self.SelectNext( Self, True, True);
  end
end;

procedure TTDIPage.SaveFormProperties;
begin
  if not Assigned( fsFormInPage ) then exit ;

  fsFormOldParent            := fsFormInPage.Parent;
  fsFormOldCloseEvent        := fsFormInPage.OnClose;
  fsFormOldAlign             := fsFormInPage.Align;
  fsFormOldBorderStyle       := fsFormInPage.BorderStyle;
  fsFormOldClientRect.Top    := fsFormInPage.Top;
  fsFormOldClientRect.Left   := fsFormInPage.Left;
  fsFormOldClientRect.Right  := fsFormInPage.Width;
  fsFormOldClientRect.Bottom := fsFormInPage.Height;
end;

procedure TTDIPage.SetFormInPage(AValue: TForm);
begin
  fsFormInPage := AValue ;

  // Saving Form Properties //
  SaveFormProperties ;

  // Adjusting Page Caption and Color as the Form //
  Caption := fsFormInPage.Caption;
  //Color := fsFormInPage.Color;

  // HiJacking the Form.OnClose Event, to detect Form Closed from Inside //
  fsFormInPage.OnClose := @OnFormClose;

  // Adjusting AForm Border Style and Align //
  fsFormInPage.BorderStyle := bsNone ;
  fsFormInPage.Align       := alClient ;

  // Change Form Parent to the Page //
  fsFormInPage.Parent := Self;
end;

{ TTDICardPanel }

constructor TTDICardPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FBackgroundCorner      := coBottomRight;
  FFixedPages            := 0;
  FIsRemovingAPage       := False;
  FShortCutClosePage     := 16499;
  FBackgroundImage       := nil;
  FTDIActions            := TTDIActions.Create;
  FTDIOptions            := [ tdiRestoreLastActiveControl,
                              tdiVerifyIfCanChangePage ] ;

  { This is ugly, I know... but I didn't found a best solution to restore Last
    Focused Control of TDIPage }
  FTimerRestoreLastControl := TTimer.Create(Self);
  FTimerRestoreLastControl.Enabled  := False;
  FTimerRestoreLastControl.Interval := 10;
  FTimerRestoreLastControl.OnTimer  := @TimerRestoreLastFocus;
end;

function TTDICardPanel.CanChange: Boolean;
var
  AWinControl : TWinControl ;
begin
  Result := True;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
  begin
    if Assigned( ActivePageComponent ) then
    begin
      // Saving Last Active Control in Page //
      AWinControl := Screen.ActiveControl;

      if ActivePageComponent is TTDIPage then
      begin
        if ActivePageComponent.ContainsControl( AWinControl ) then
        begin
          TTDIPage( ActivePageComponent ).LastActiveControl := AWinControl;

          if tdiVerifyIfCanChangePage in FTDIOptions then
          begin
            { Try to detect if occurs some exception when leaving current
              control focus. This may occurs in TWinControl.OnExit Validation }
            Self.SetFocus;

            { If still on same ActiveControl, maybe Focus Control was trapped on
              some OnExit Validation }
            Result := ( AWinControl <> Screen.ActiveControl );
          end ;
        end ;
      end ;
    end ;
  end ;

  //{$if (lcl_major > 0) or (lcl_release > 30)}
  //Result := Result and (inherited CanChange)
  //{$endif};

  // Emulate FormInPage.OnDeactivate //
  if Result and (tdiRestoreLastActiveControl in FTDIOptions) then
  begin
    if (not FIsRemovingAPage) and
       ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
    begin
      if (ActivePageComponent is TTDIPage) then
      begin
        with TTDIPage(ActivePageComponent) do
        begin
          if Assigned( FormInPage ) then
            if ([csDesigning, csDestroying, csFreeNotification] * FormInPage.ComponentState = []) then
              if Assigned( FormInPage.OnDeactivate ) then
                if FormInPage.Visible then
                  FormInPage.OnDeactivate( Self );
        end ;
      end ;
    end ;
  end;
end;

function TTDICardPanel.CanCloseAllPages: Boolean;
var
  I : Integer ;
begin
  Result := True;
  if PageCount < 1 then exit ;

  I := 0;
  while Result and ( I < PageCount ) do
  begin
    Result := CanCloseAPage( I );
    Inc(I)
  end ;
end;

function TTDICardPanel.CanCloseAPage(APageIndex: Integer): Boolean;
begin
  Result := True;

  if (Page[APageIndex] is TTDIPage) then
    with TTDIPage(Page[APageIndex]) do
    begin
      if Assigned( FormInPage ) then
        Result := FormInPage.CloseQuery;
    end ;
end;

procedure TTDICardPanel.CheckInterface;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  Visible := (PageCount > 0);

  // Checking for Close Button visibility //
  //if (FCloseTabButtom <> tbNone) then
  //begin
  //  if Visible then
  //    ShowCloseButtom
  //  else
  //    HideCloseButtom;
  //end ;

  // Checking for Tabs Menu visibility //
  //if Visible and (FTabsMenuItem <> nil) then
  //begin
  //  with FTabsMenuItem do
  //  begin
  //    Caption    := TDIActions.TabsMenu.Caption;
  //    Visible    := TDIActions.TabsMenu.Visible;
  //    ImageIndex := TDIActions.TabsMenu.ImageIndex;
  //  end ;
  //end ;

  // Drawing Background Image //
  if Visible then
    DrawBackgroundImage;
end;

procedure TTDICardPanel.CloseAllTabs;
begin
  CloseAllTabsClicked(Nil);
end;

procedure TTDICardPanel.CloseAllTabsClicked(Sender: TObject);
var
  LastPageCount : Integer ;
begin
   if PageCount < 1 then exit ;

   LastPageCount := -1 ;
   PageIndex     := PageCount-1;  // Go to Last page
   // Close while have pages, and Pages still being closed //
   while (PageCount > FFixedPages) and (LastPageCount <> PageCount) do
   begin
     LastPageCount := PageCount ;
     RemovePage( PageIndex );
     Application.ProcessMessages;
   end;
end;

procedure TTDICardPanel.CloseTabClicked(Sender: TObject);
begin
  RemovePage( PageIndex );
end;

function TTDICardPanel.CreateFormInNewPage(AFormClass: TFormClass; ImageIndex: Integer): TForm;
begin
  Result := AFormClass.Create(Application);

  ShowFormInPage( Result, ImageIndex );
end;

destructor TTDICardPanel.Destroy;
begin
  //if Assigned( FCloseBitBtn )  then
  //  FCloseBitBtn.Free ;

  { // Don't Destroy Menu Items... They will be destroyed by MainMenu //

  if Assigned( FCloseMenuItem )  then
    FCloseMenuItem.Free ;

  if Assigned( FTabsMenuItem )  then
  begin
    FTabsMenuItem.Free ;
    FCloseMenuItem2.Free;
    FCloseAllTabsMenuItem.Free;
  end ;
  }

  FTDIActions.Free;

  FTimerRestoreLastControl.Free;

  inherited Destroy;
end;

procedure TTDICardPanel.DoChange;
begin
  //inherited DoChange;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  // Emulate FormInPage.OnActivate //
  if tdiRestoreLastActiveControl in FTDIOptions then
  begin
    if (not FIsRemovingAPage) and (ActivePageComponent is TTDIPage) then
    begin
      with TTDIPage(ActivePageComponent) do
      begin
        if Assigned( FormInPage ) then
          if ([csDesigning, csDestroying, csFreeNotification] * FormInPage.ComponentState = []) then
            if Assigned( FormInPage.OnActivate ) then
              if FormInPage.Visible then
                FormInPage.OnActivate( Self );
      end;
    end ;
  end;

  CheckInterface;

  {
  // This doesn't work on Win32, Focus always go to first control on Page //
  if FRestoreActiveControl then
    if (ActivePage is TTDIPage) then
      TTDIPage( ActivePage ).RestoreLastFocusedControl;
  }

  // This is a ugly workaround.. but it works :) //
  if tdiRestoreLastActiveControl in FTDIOptions then
    RestoreLastFocusedControl;
end;

procedure TTDICardPanel.DoCloseTabClicked(APage: TPage);
var
  LastPageCount: Integer;
begin
  LastPageCount := PageCount;

  //inherited DoCloseTabClicked(APage);

  if Assigned( APage ) and (LastPageCount = PageCount) then  // If Page was not closed...
  begin
    PageIndex := APage.PageIndex;

    if PageIndex >= FixedPages then
      RemovePage( APage.PageIndex );
  end;
end;

procedure TTDICardPanel.DrawBackgroundImage;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  if not Assigned( FBackgroundImage ) then exit ;

  if not Assigned( ActivePageComponent ) then exit ;

  FBackgroundImage.Parent  := ActivePageComponent;
  FBackgroundImage.Anchors := [];
  FBackgroundImage.AnchorSideBottom.Control := nil;
  FBackgroundImage.AnchorSideTop.Control    := nil;
  FBackgroundImage.AnchorSideRight.Control  := nil;
  FBackgroundImage.AnchorSideLeft.Control   := nil;

  if FBackgroundCorner in [coBottomRight, coBottomLeft] then
  begin
     FBackgroundImage.AnchorSideBottom.Control := ActivePageComponent;
     FBackgroundImage.AnchorSideBottom.Side    := asrBottom;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akBottom];
  end
  else
  begin
     FBackgroundImage.AnchorSideTop.Control := ActivePageComponent;
     FBackgroundImage.AnchorSideTop.Side    := asrTop;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akTop];
  end ;

  if FBackgroundCorner in [coBottomRight, coTopRight] then
  begin
     FBackgroundImage.AnchorSideRight.Control := ActivePageComponent;
     FBackgroundImage.AnchorSideRight.Side    := asrBottom;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akRight];
  end
  else
  begin
     FBackgroundImage.AnchorSideLeft.Control := ActivePageComponent;
     FBackgroundImage.AnchorSideLeft.Side    := asrTop;
     FBackgroundImage.Anchors := FBackgroundImage.Anchors + [akLeft];
  end ;

  FBackgroundImage.Visible := True ;
end;

function TTDICardPanel.FindFormInPages(AForm: TForm): Integer;
var
  I : Integer ;
begin
  Result := -1;

  I := 0;
  while (Result < 0) and (I < PageCount) do
  begin
     if Page[I] is TTDIPage then
       with TTDIPage( Page[I] ) do
       begin
         if AForm = FormInPage then
           Result := I;
       end ;

     Inc( I ) ;
  end ;
end;

procedure TTDICardPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //if (FTabsMenuItem = nil) then  // Is already Handled by TabsMenu itens?
  //begin
  //  if (PageIndex >= FFixedPages) and
  //     (ShortCut(Key, Shift) = FShortCutClosePage) then
  //  begin
  //    Key := 0;
  //    RemovePage( PageIndex );
  //    exit;
  //  end;
  //end
  //else
  if (Key = VK_TAB) and (ssCtrl in Shift) then   // TabsMenu will do it...
    exit ;

  if ActivePageComponent is TTDIPage then
  begin
    with TTDIPage( ActivePageComponent ) do
    begin
      RestoreLastFocusedControl;

      // TODO: Propagate Key Pressed to FormInPage //
      //FormInPage.OnKeyDown(Self,Key,Shift);
    end ;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TTDICardPanel.Loaded;
begin
  //inherited Loaded ;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  //if Assigned( FMainMenu ) then
  //   CreateTabsMenuItem;

  CheckInterface;
end;

procedure TTDICardPanel.msg_ClosePage(var Msg: TLMessage);
begin
  RemovePage( Msg.lParam );
end;

procedure TTDICardPanel.NextPageClicked(Sender: TObject);
begin
  ScrollPage( True );
end;

procedure TTDICardPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) then
  begin
     if (AComponent = FBackgroundImage) then
       FBackgroundImage := nil

     //else if (AComponent = FMainMenu) then
     //  FMainMenu := nil

     else if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then

     else if (AComponent is TForm) then
       RemoveInvalidPages ;
  end ;
end;

procedure TTDICardPanel.PreviousPageClicked(Sender: TObject);
begin
  ScrollPage( False );
end;

procedure TTDICardPanel.RemoveInvalidPages;
var
  I : Integer ;
begin
  // Remove all TTDIPage with FormInPage not assigned //;
  I := 0 ;
  while I < PageCount do
  begin
    if Page[I] is TTDIPage then
    begin
      with TTDIPage( Page[I] ) do
      begin
        if FormInPage = nil then
        begin
          RemovePage( I );
          Dec( I ) ;
        end ;
      end ;
    end ;

    Inc( I ) ;
  end ;
end;

procedure TTDICardPanel.RemovePage(Index: Integer);
var
  CanRemovePage: Boolean ;
  APage: TPage;
begin
  if (Index >= PageCount) or (Index < 0) then
    Exit;

  CanRemovePage    := True;
  FIsRemovingAPage := True;
  APage            := Page[Index] ;
  try
    if ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
    begin
      if APage is TTDIPage then
      begin
        with TTDIPage(APage) do
        begin
          if Assigned( FormInPage ) then
          begin
            CanRemovePage := False;
            FormInPage.Close ;
          end ;
        end ;
      end ;
    end ;

    if CanRemovePage then
    begin
      //{$if (lcl_major > 0) or (lcl_release > 30)}
      //  inherited RemovePage(APage.PageIndex) ;
      //{$else}
        APage.Free;
      //{$endif}

      if (PageCount <= 1) then  // In this situation... DoChange is not fired //
        CheckInterface;
    end ;
  finally
    FIsRemovingAPage := False;
  end ;
end;

procedure TTDICardPanel.RestoreLastFocusedControl;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then exit ;

  FTimerRestoreLastControl.Enabled := True;
end;

procedure TTDICardPanel.ScrollPage(ToForward: Boolean);
var
  NewPage : Integer ;
begin
  if ToForward then
  begin
    NewPage := PageIndex + 1 ;
    if NewPage >= PageCount then
      NewPage := 0;
  end
  else
  begin
    NewPage := PageIndex - 1 ;
    if NewPage < 0 then
      NewPage := PageCount-1 ;
  end ;

  PageIndex := NewPage;
end;

procedure TTDICardPanel.SetBackgroundCorner(AValue: TTDIBackgroundCorner);
begin
  if FBackgroundCorner = AValue then Exit ;
  FBackgroundCorner := AValue ;

  if Visible then
    DrawBackgroundImage;
end;

procedure TTDICardPanel.SetBackgroundImage(AValue: TImage);
begin
  if FBackgroundImage = AValue then Exit ;
  FBackgroundImage := AValue ;

  if Visible then
    DrawBackgroundImage;
end;

procedure TTDICardPanel.SetFixedPages(AValue: Integer);
begin
  if FFixedPages = AValue then Exit ;
  FFixedPages := AValue ;

  CheckInterface;
end;

procedure TTDICardPanel.ShowFormInPage(AForm: TForm; ImageIndex: Integer);
var
  NewPage : TTDIPage ;
  AlreadyExistingPage : Integer ;
  DoCheckInterface: Boolean;
begin
  if not Assigned( AForm ) then
    raise ETDIError.Create( sFormNotAssigned ) ;

  // Looking for a Page with same AForm Object //
  AlreadyExistingPage := FindFormInPages( AForm );
  if AlreadyExistingPage >= 0 then
  begin
    PageIndex := AlreadyExistingPage;
    exit ;
  end ;

  DoCheckInterface := (PageCount <= 1);

  // Create a new Page
  NewPage := TTDIPage.Create(Self);
  //NewPage.ImageIndex := ImageIndex;

  Visible := True;

  // This will call TTDIPage.SetFormInPage, who does the magic //
  NewPage.FormInPage := AForm;

  // Activate the new Page
  PageIndex := NewPage.PageIndex;

  // Show the Form //
  AForm.Visible := True ;

  // Saving the current ActiveControl in the Form //
  NewPage.LastActiveControl := AForm.ActiveControl;

  // Checking Form alignment //
  if (AForm.Constraints.MaxHeight <= 0) or
     (AForm.Constraints.MaxWidth <= 0) then
    AForm.Align := alClient;                   // Try to expand the Form
  NewPage.CheckFormAlign ;

  if DoCheckInterface then
    CheckInterface;
end;

procedure TTDICardPanel.TimerRestoreLastFocus(Sender: TObject);
begin
  FTimerRestoreLastControl.Enabled := False;

  if Assigned( ActivePageComponent ) then
    if ActivePageComponent is TTDIPage then
      TTDIPage( ActivePageComponent ).RestoreLastFocusedControl;
end;

end.
