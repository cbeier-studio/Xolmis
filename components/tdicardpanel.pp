{ TDI Card Panel
  - Show multiple forms in Pages.
  - Pages navigation must be coded separatly.

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

  Modified from:

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
}

unit TDICardPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, LMessages, LCLVersion;

resourcestring
  sOwnerIsNotWinControl = 'TDICardPanel.Owner is not a TWinControl descendant';
  sFormNotAssigned = 'Parameter AForm not Assigned';

const
  TDIM_CLOSEPAGE = LM_INTERFACELAST + 500;

type
  ETDIError = class( Exception ) ;

  TTDIPageChangeEvent = procedure(Sender: TObject; AOldPageIndex, ANewPageIndex: Integer) of object;
  TTDIBeforeClosePageEvent = procedure(Sender: TObject; APageIndex: Integer; var CanClose: Boolean) of object;
  TTDIAfterClosePageEvent = procedure(Sender: TObject; APageIndex: Integer) of object;

  { TTDIPage }

  TTDIPage = class(TPage)
  private
    fsFormInPage: TForm ;
    fsFormOldParent: TWinControl;
    fsFormOldCloseEvent: TCloseEvent;
    fsFormOldAlign: TAlign;
    fsFormOldClientRect: TRect;
    fsFormOldBorderStyle: TFormBorderStyle;
    fsLastActiveControl: TWinControl;

    procedure OnResizeTDIPage(Sender: TObject);
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure SaveFormProperties;
    procedure RestoreFormProperties;
    procedure SetFormInPage(AValue: TForm);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckFormAlign;
  public
    constructor Create(TheOwner: TComponent);  override;
    destructor Destroy; override;

    procedure RestoreLastFocusedControl;

    property FormInPage: TForm read fsFormInPage write SetFormInPage;
    property LastActiveControl: TWinControl read fsLastActiveControl write fsLastActiveControl;
  end;

  TTDIOption = (tdiRestoreLastActiveControl, tdiVerifyIfCanChangePage, tdiEmulateFormOnActivate);
  TTDIOptions = set of TTDIOption;

  { TTDICardPanel }

  TTDICardPanel = class(TNotebook)
  private
    FFixedPages: Integer;
    FTDIOptions: TTDIOptions;
    FShortCutClosePage: TShortCut;
    FIsRemovingAPage: Boolean;

    FOnPageChange: TTDIPageChangeEvent;
    FOnBeforeClosePage: TTDIBeforeClosePageEvent;
    FOnAfterClosePage: TTDIAfterClosePageEvent;

    procedure SetFixedPages(AValue: Integer);

    procedure AsyncRestoreFocus(Data: PtrInt);
    procedure RemoveInvalidPages ;
  protected
    function CanChange: Boolean;
    procedure DoChange;
    procedure Loaded;
    procedure RemovePage(Index: Integer);
    procedure msg_ClosePage(var Msg: TLMessage); message TDIM_CLOSEPAGE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent);  override;
    destructor Destroy; override;

    function CreateFormInNewPage(AFormClass: TFormClass; ImageIndex: Integer = -1): TForm;
    procedure ShowFormInPage(AForm: TForm; ImageIndex: Integer = -1);
    Function FindFormInPages(AForm: TForm): Integer ;
    function GetFormByClass(AFormClass: TFormClass): TForm;
    function SelectForm(AForm: TForm): Boolean;
    function SelectFormByClass(AFormClass: TFormClass): Boolean;

    Function CanCloseAllPages: Boolean ;
    Function CanCloseAPage( APageIndex: Integer): Boolean;

    procedure RestoreLastFocusedControl;
    procedure ScrollPage(ToForward: Boolean);
    procedure CheckInterface;
    procedure CloseAllTabs;
    procedure CloseTab(Index: Integer);
  published
    property TDIOptions: TTDIOptions read FTDIOptions write FTDIOptions default [tdiRestoreLastActiveControl, tdiVerifyIfCanChangePage];
    property ShortCutClosePage: TShortCut read FShortCutClosePage write FShortCutClosePage default 16499;  // Ctrl+F4
    property FixedPages : Integer read FFixedPages write SetFixedPages default 0;

    property OnPageChange: TTDIPageChangeEvent read FOnPageChange write FOnPageChange;
    property OnBeforeClosePage: TTDIBeforeClosePageEvent read FOnBeforeClosePage write FOnBeforeClosePage;
    property OnAfterClosePage: TTDIAfterClosePageEvent read FOnAfterClosePage write FOnAfterClosePage;
  end;

procedure Register;

implementation

uses
  LCLType;

procedure Register;
begin
  RegisterComponents('CBS',[TTDICardPanel]);
end;

{ TTDIPage }

constructor TTDIPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Self.Parent   := TWinControl(TheOwner);
  Self.OnResize := @OnResizeTDIPage;

  fsLastActiveControl := nil;
end;

procedure TTDIPage.CheckFormAlign;
var
  Maximize: Boolean ;
begin
  if not Assigned(fsFormInPage) then
    Exit;

  Maximize := not ((fsFormInPage.Constraints.MaxWidth <> 0) and (fsFormInPage.Width < Width));
  if Maximize then
     Maximize := not ((fsFormInPage.Constraints.MaxHeight <> 0) and (fsFormInPage.Height < Height));

  { If Form has MaxConstrains and doesn't fill all the Screen, Centralize on
    TabSheet }
  if not Maximize then
  begin
    fsFormInPage.Align := alNone;

    if (fsFormInPage.Width < Width) then
      fsFormInPage.Left := (Width - fsFormInPage.Width) div 2
    else
      fsFormInPage.Left := 0;

    if (fsFormInPage.Height < Height) then
      fsFormInPage.Top := (Height - fsFormInPage.Height) div 2
    else
      fsFormInPage.Top := 0;
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

  if ([csDesigning, csDestroying] * ComponentState <> []) then
    Exit;

  if (Operation = opRemove) and (AComponent = fsFormInPage) then
    fsFormInPage := nil;
end;

procedure TTDIPage.OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Msg: TLMessage;
begin
  if Assigned(fsFormOldCloseEvent) then
    fsFormOldCloseEvent(Sender, CloseAction);

  if Assigned(fsFormInPage) then
    RestoreFormProperties;

  fsFormInPage := nil;

  if Assigned(Parent) then
  begin
    Msg.msg    := TDIM_CLOSEPAGE;
    Msg.lParam := PageIndex;
    Parent.Dispatch(Msg);
  end;
end;

procedure TTDIPage.OnResizeTDIPage(Sender: TObject);
begin
  CheckFormAlign;
end;

procedure TTDIPage.RestoreFormProperties;
begin
  if not Assigned(fsFormInPage) then
    Exit;

  fsFormInPage.Visible     := False;  // This prevent OnFormShow be fired
  fsFormInPage.Parent      := fsFormOldParent;
  fsFormInPage.Align       := fsFormOldAlign;
  fsFormInPage.BorderStyle := fsFormOldBorderStyle;
  fsFormInPage.Top         := fsFormOldClientRect.Top;
  fsFormInPage.Left        := fsFormOldClientRect.Left;
  fsFormInPage.Width       := fsFormOldClientRect.Right;
  fsFormInPage.Height      := fsFormOldClientRect.Bottom;
  fsFormInPage.OnClose     := fsFormOldCloseEvent;

  fsFormInPage.RemoveFreeNotification(Self);
  fsFormInPage := nil;
end;

procedure TTDIPage.RestoreLastFocusedControl;
var
  FocusRestored: Boolean;
begin
  FocusRestored := False;

  if Assigned(fsLastActiveControl) then
  begin
    if (fsLastActiveControl <> Screen.ActiveControl) and (fsLastActiveControl.CanSetFocus) then
    begin
      try
        fsLastActiveControl.SetFocus;
        FocusRestored := True;
      except
      end;
    end;
  end;

  if not FocusRestored then
  begin
    { No LastActiveControle ? Ok, if current Screen control isn't in TabSheet,
      go to first Control on TabSheet... }
    if not Self.ContainsControl(Screen.ActiveControl) then
      Self.SelectNext(Self, True, True);
  end
end;

procedure TTDIPage.SaveFormProperties;
begin
  if not Assigned(fsFormInPage) then
    Exit;

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
  fsFormInPage := AValue;
  if not Assigned(fsFormInPage) then
    Exit;

  fsFormInPage.FreeNotification(Self);
  SaveFormProperties;

  Caption := fsFormInPage.Caption;
  // HiJacking the Form.OnClose Event, to detect Form Closed from Inside
  fsFormInPage.OnClose := @OnFormClose;
  // Adjusting AForm Border Style and Align
  fsFormInPage.BorderStyle := bsNone;
  fsFormInPage.Align       := alClient;
  // Change Form Parent to the Page
  fsFormInPage.Parent := Self;
end;

{ TTDICardPanel }

constructor TTDICardPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFixedPages            := 0;
  FIsRemovingAPage       := False;
  FShortCutClosePage     := 16499;
  FTDIOptions            := [tdiRestoreLastActiveControl, tdiVerifyIfCanChangePage];
end;

procedure TTDICardPanel.AsyncRestoreFocus(Data: PtrInt);
begin
  if Assigned(ActivePageComponent) and (ActivePageComponent is TTDIPage) then
    TTDIPage(ActivePageComponent).RestoreLastFocusedControl;
end;

function TTDICardPanel.CanChange: Boolean;
var
  AWinControl: TWinControl;
begin
  Result := True;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
  begin
    if Assigned(ActivePageComponent) then
    begin
      // Saving Last Active Control in Page
      AWinControl := Screen.ActiveControl;

      if ActivePageComponent is TTDIPage then
      begin
        if ActivePageComponent.ContainsControl(AWinControl) then
        begin
          TTDIPage(ActivePageComponent).LastActiveControl := AWinControl;

          if tdiVerifyIfCanChangePage in FTDIOptions then
          begin
            { Try to detect if occurs some exception when leaving current
              control focus. This may occurs in TWinControl.OnExit Validation }
            Self.SetFocus;

            { If still on same ActiveControl, maybe Focus Control was trapped on
              some OnExit Validation }
            Result := (AWinControl <> Screen.ActiveControl);
          end;
        end;
      end;
    end;
  end;

  // Emulate FormInPage.OnDeactivate
  if Result and (tdiRestoreLastActiveControl in FTDIOptions) then
  begin
    if (not FIsRemovingAPage) and ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
    begin
      if (ActivePageComponent is TTDIPage) then
      begin
        with TTDIPage(ActivePageComponent) do
        begin
          if Assigned(FormInPage) and ([csDesigning, csDestroying, csFreeNotification] * FormInPage.ComponentState = []) then
            if Assigned(FormInPage.OnDeactivate) and FormInPage.Visible then
              FormInPage.OnDeactivate(Self);
        end;
      end;
    end;
  end;
end;

function TTDICardPanel.CanCloseAllPages: Boolean;
var
  I : Integer ;
begin
  Result := True;
  if PageCount < 1 then
    Exit;

  I := 0;
  while Result and (I < PageCount) do
  begin
    Result := CanCloseAPage(I);
    Inc(I)
  end;
end;

function TTDICardPanel.CanCloseAPage(APageIndex: Integer): Boolean;
begin
  Result := True;

  if (Page[APageIndex] is TTDIPage) then
    with TTDIPage(Page[APageIndex]) do
    begin
      if Assigned(FormInPage) then
        Result := FormInPage.CloseQuery;
    end;
end;

procedure TTDICardPanel.CheckInterface;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then
    Exit;

  Visible := (PageCount > 0);
end;

procedure TTDICardPanel.CloseAllTabs;
var
  I: Integer;
begin
  if PageCount < 1 then
    Exit;

  for I := PageCount - 1 downto FFixedPages do
    RemovePage(I);
end;

procedure TTDICardPanel.CloseTab(Index: Integer);
begin
  RemovePage(Index);
end;

function TTDICardPanel.CreateFormInNewPage(AFormClass: TFormClass; ImageIndex: Integer): TForm;
begin
  Result := AFormClass.Create(Application);

  ShowFormInPage(Result, ImageIndex);
end;

destructor TTDICardPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TTDICardPanel.DoChange;
var
  OldIdx: Integer;
begin
  OldIdx := PageIndex;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then
    Exit;

  // Emulate FormInPage.OnActivate
  if tdiRestoreLastActiveControl in FTDIOptions then
  begin
    if (not FIsRemovingAPage) and (ActivePageComponent is TTDIPage) then
    begin
      with TTDIPage(ActivePageComponent) do
      begin
        if Assigned(FormInPage) and ([csDesigning, csDestroying, csFreeNotification] * FormInPage.ComponentState = []) then
          if Assigned(FormInPage.OnActivate) and FormInPage.Visible then
            FormInPage.OnActivate(Self);
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

  // This is a ugly workaround.. but it works :)
  if tdiRestoreLastActiveControl in FTDIOptions then
    RestoreLastFocusedControl;

  if Assigned(FOnPageChange) then
    FOnPageChange(Self, OldIdx, PageIndex);
end;

function TTDICardPanel.FindFormInPages(AForm: TForm): Integer;
var
  I: Integer ;
begin
  Result := -1;

  I := 0;
  while (Result < 0) and (I < PageCount) do
  begin
    if Page[I] is TTDIPage then
      if AForm = TTDIPage(Page[I]).FormInPage then
        Result := I;

    Inc(I);
  end ;
end;

function TTDICardPanel.GetFormByClass(AFormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to PageCount - 1 do
  begin
    if Page[I] is TTDIPage then
    begin
      if TTDIPage(Page[I]).FormInPage is AFormClass then
        Exit(TTDIPage(Page[I]).FormInPage);
    end;
  end;
end;

procedure TTDICardPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) and (ssCtrl in Shift) then
    Exit;

  if ActivePageComponent is TTDIPage then
  begin
    TTDIPage(ActivePageComponent).RestoreLastFocusedControl;

      // TODO: Propagate Key Pressed to FormInPage //
      //FormInPage.OnKeyDown(Self,Key,Shift);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TTDICardPanel.Loaded;
begin
  //inherited Loaded ;

  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then
    Exit;

  CheckInterface;
end;

procedure TTDICardPanel.msg_ClosePage(var Msg: TLMessage);
begin
  RemovePage(Msg.lParam);
end;

procedure TTDICardPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) then
  begin
     if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then

     else if (AComponent is TForm) then
       RemoveInvalidPages ;
  end ;
end;

procedure TTDICardPanel.RemoveInvalidPages;
var
  I: Integer ;
begin
  // Remove all TTDIPage with FormInPage not assigned
  I := 0 ;
  while I < PageCount do
  begin
    if Page[I] is TTDIPage then
    begin
      if TTDIPage(Page[I]).FormInPage = nil then
      begin
        RemovePage(I);
        Dec(I);
      end;
    end;
    Inc(I);
  end;
end;

procedure TTDICardPanel.RemovePage(Index: Integer);
var
  CanRemovePage: Boolean;
  APage: TPage;
begin
  if (Index >= PageCount) or (Index < 0) then
    Exit;

  CanRemovePage := True;

  if Assigned(FOnBeforeClosePage) then
    FOnBeforeClosePage(Self, Index, CanRemovePage);

  if not CanRemovePage then
    Exit;

  FIsRemovingAPage := True;
  APage := Page[Index];
  try
    if ([csDesigning, csDestroying, csFreeNotification] * ComponentState = []) then
    begin
      if APage is TTDIPage then
      begin
        with TTDIPage(APage) do
        begin
          if Assigned(FormInPage) then
          begin
            CanRemovePage := False;
            FormInPage.Close;
          end;
        end;
      end;
    end;

    if CanRemovePage then
    begin
      APage.Free;

      if (PageCount <= 1) then  // In this situation... DoChange is not fired
        CheckInterface;

      if Assigned(FOnAfterClosePage) then
        FOnAfterClosePage(Self, Index);
    end;
  finally
    FIsRemovingAPage := False;
  end;
end;

procedure TTDICardPanel.RestoreLastFocusedControl;
begin
  if ([csDesigning, csDestroying, csFreeNotification] * ComponentState <> []) then
    Exit;

  Application.QueueAsyncCall(@AsyncRestoreFocus, 0);
end;

procedure TTDICardPanel.ScrollPage(ToForward: Boolean);
var
  NewPage : Integer ;
begin
  if ToForward then
  begin
    NewPage := PageIndex + 1;
    if NewPage >= PageCount then
      NewPage := 0;
  end
  else
  begin
    NewPage := PageIndex - 1;
    if NewPage < 0 then
      NewPage := PageCount - 1;
  end;

  PageIndex := NewPage;
end;

function TTDICardPanel.SelectForm(AForm: TForm): Boolean;
var
  Idx: Integer;
begin
  Idx := FindFormInPages(AForm);
  Result := (Idx >= 0);
  if Result then
    PageIndex := Idx;
end;

function TTDICardPanel.SelectFormByClass(AFormClass: TFormClass): Boolean;
var
  F: TForm;
begin
  F := GetFormByClass(AFormClass);
  Result := Assigned(F) and SelectForm(F);
end;

procedure TTDICardPanel.SetFixedPages(AValue: Integer);
begin
  if FFixedPages = AValue then
    Exit;
  FFixedPages := AValue;

  CheckInterface;
end;

procedure TTDICardPanel.ShowFormInPage(AForm: TForm; ImageIndex: Integer);
var
  NewPage: TTDIPage;
  AlreadyExistingPage: Integer;
  DoCheckInterface: Boolean;
begin
  if not Assigned(AForm) then
    raise ETDIError.Create(sFormNotAssigned);

  // Looking for a Page with same AForm Object
  AlreadyExistingPage := FindFormInPages(AForm);
  if AlreadyExistingPage >= 0 then
  begin
    PageIndex := AlreadyExistingPage;
    Exit;
  end;

  DoCheckInterface := (PageCount <= 1);

  // Create a new Page
  NewPage := TTDIPage.Create(Self);
  Visible := True;

  // This will call TTDIPage.SetFormInPage, who does the magic
  NewPage.FormInPage := AForm;
  // Activate the new Page
  PageIndex := NewPage.PageIndex;
  AForm.Visible := True;

  // Saving the current ActiveControl in the Form
  NewPage.LastActiveControl := AForm.ActiveControl;

  // Checking Form alignment
  if (AForm.Constraints.MaxHeight <= 0) or (AForm.Constraints.MaxWidth <= 0) then
    AForm.Align := alClient; // Try to expand the Form

  NewPage.CheckFormAlign;

  if DoCheckInterface then
    CheckInterface;
end;

end.
