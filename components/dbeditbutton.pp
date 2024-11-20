{ DBEditButton
  - DBEdit with a Button.

  Copyright (C) 2024 Christian Beier <hello@christianbeier.studio>

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

unit DBEditButton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons,
  DB, DBCtrls, ImgList, LCLType;

type

  { TDBEditButton }

  TDBEditButton = class(TCustomControl)
  private
    FButtonOnlyWhenFocused: Boolean;
    FDBEdit: TDBEdit ;
    FButton: TSpeedButton;
    FButtonWidth: Integer;
    FDirectInput: Boolean;
    FFocusOnButtonClick: Boolean;
    FOnButtonClick: TNotifyEvent;
    FChangeEvent : TNotifyEvent;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FShowHint: Boolean;
    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetButtonCaption: TTranslateString;
    function GetButtonCursor: TCursor;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetCharCase: TEditCharCase;
    function GetColor: TColor;
    function GetCustomEditMask: Boolean;
    function GetDataField: String;
    function GetDataSource: TDataSource;
    function GetDisabledImageIndex: Integer;
    function GetEditMask: String;
    function GetField: TField;
    function GetFlat: Boolean;
    function GetHint: TTranslateString;
    function GetHotImageIndex: Integer;
    function GetImageIndex: Integer;
    function GetImages: TCustomImageList;
    function GetImageWidth: Integer;
    function GetMaxLength: Integer;
    function GetPressedImageIndex: Integer;
    function GetSelectedImageIndex: Integer;
    function GetSpacing: Integer;
    function GetTextHint: TTranslateString;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAutoSelect(AValue: Boolean);
    procedure SetButtonCaption(AValue: TTranslateString);
    procedure SetButtonCursor(AValue: TCursor);
    procedure SetButtonHint(AValue: TTranslateString);
    procedure SetButtonOnlyWhenFocused(AValue: Boolean);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetCustomEditMask(AValue: Boolean);
    procedure SetDataField(AValue: String);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDirectInput(AValue: Boolean);
    procedure SetDisabledImageIndex(AValue: Integer);
    procedure SetEditMask(AValue: String);
    procedure SetFlat(AValue: Boolean);
    procedure SetFocusOnButtonClick(AValue: Boolean);
    procedure SetHotImageIndex(AValue: Integer);
    procedure SetImageIndex(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImageWidth(AValue: Integer);
    procedure SetMaxLength(AValue: Integer);
    procedure SetPasswordChar(AValue: Char);
    procedure SetPressedImageIndex(AValue: Integer);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSelectedImageIndex(AValue: Integer);
    procedure SetShowHint(AValue: Boolean);
    procedure SetSpacing(AValue: Integer);
    procedure SetTextHint(AValue: TTranslateString);
    procedure ButtonClick(Sender: TObject);
    procedure FDBEditClick(Sender: TObject);
    procedure FDBEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure FDBEditDblClick(Sender: TObject);
    procedure FDBEditDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FDBEditDragOver
      (Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure FDBEditEditingDone(Sender: TObject);
    procedure FDBEditEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FDBEditEnter(Sender: TObject);
    procedure FDBEditExit(Sender: TObject);
    procedure FDBEditMouseDown
      (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      );
    procedure FDBEditMouseEnter(Sender: TObject);
    procedure FDBEditMouseLeave(Sender: TObject);
    procedure FDBEditMouseMove
      (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FDBEditMouseUp
      (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      );
    procedure FDBEditMouseWheel
      (Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure FDBEditMouseWheelDown
      (Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FDBEditMouseWheelUp
      (Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FDBEditStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FDBEditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FDBEditKeyPress(Sender: TObject; var Key: Char);
    procedure FDBEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FDBEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure FDBEditChange(Sender:TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteAction(AAction: TBasicAction): Boolean;
    function UpdateAction(AAction: TBasicAction): Boolean;
    property Field: TField read GetField;
  published
    property Align;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Anchors;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property ButtonCaption: TTranslateString read GetButtonCaption write SetButtonCaption;
    property ButtonCursor: TCursor read GetButtonCursor write SetButtonCursor;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property ButtonOnlyWhenFocused: Boolean read FButtonOnlyWhenFocused write SetButtonOnlyWhenFocused default False;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property Color: TColor read GetColor write SetColor;
    property Constraints;
    property Cursor: TCursor read GetCursor write SetCursor;
    property CustomEditMask: Boolean read GetCustomEditMask write SetCustomEditMask;
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DirectInput: Boolean read FDirectInput write SetDirectInput default True;
    property DisabledImageIndex: Integer read GetDisabledImageIndex write SetDisabledImageIndex default -1;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditMask: String read GetEditMask write SetEditMask;
    property Enabled;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property FocusOnButtonClick: Boolean read FFocusOnButtonClick write SetFocusOnButtonClick default False;
    property Font;
    //property Glyph;
    property Hint: TTranslateString read GetHint write SetHint;
    property HotImageIndex: Integer read GetHotImageIndex write SetHotImageIndex default -1;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth default 0;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    //property NumGlyphs;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property PressedImageIndex: Integer read GetPressedImageIndex write SetPressedImageIndex default -1;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property SelectedImageIndex: Integer read GetSelectedImageIndex write SetSelectedImageIndex default -1;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property Spacing: Integer read GetSpacing write SetSpacing;
    property TabOrder;
    property TabStop;
    property TextHint: TTranslateString read GetTextHint write SetTextHint;
    property Visible;

    property OnButtonClick:TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnChange:TNotifyEvent read FChangeEvent write FChangeEvent;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CBS',[TDBEditButton]);
end;

{ TDBEditButton }

constructor TDBEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDBEdit := TDBEdit.Create(Self);
  FDBEdit.Parent := Self;
  FDBEdit.Align := alClient;
  FDBEdit.OnChange := @FDBEditChange;
  FDBEdit.OnKeyPress := @FDBEditKeyPress;
  FDBEdit.OnKeyDown := @FDBEditKeyDown;
  FDBEdit.OnKeyUp := @FDBEditKeyUp;
  FDBEdit.OnClick := @FDBEditClick;
  FDBEdit.OnContextPopup := @FDBEditContextPopup;
  FDBEdit.OnDblClick := @FDBEditDblClick;
  FDBEdit.OnDragDrop := @FDBEditDragDrop;
  FDBEdit.OnDragOver := @FDBEditDragOver;
  FDBEdit.OnEditingDone := @FDBEditEditingDone;
  FDBEdit.OnEndDrag := @FDBEditEndDrag;
  FDBEdit.OnEnter := @FDBEditEnter;
  FDBEdit.OnExit := @FDBEditExit;
  FDBEdit.OnMouseDown := @FDBEditMouseDown;
  FDBEdit.OnMouseEnter := @FDBEditMouseEnter;
  FDBEdit.OnMouseLeave := @FDBEditMouseLeave;
  FDBEdit.OnMouseMove := @FDBEditMouseMove;
  FDBEdit.OnMouseUp := @FDBEditMouseUp;
  FDBEdit.OnMouseWheel := @FDBEditMouseWheel;
  FDBEdit.OnMouseWheelDown := @FDBEditMouseWheelDown;
  FDBEdit.OnMouseWheelUp := @FDBEditMouseWheelUp;
  FDBEdit.OnStartDrag := @FDBEditStartDrag;
  FDBEdit.OnUTF8KeyPress := @FDBEditUTF8KeyPress;

  FButton := TSpeedButton.Create(Self);
  FButton.Parent := Self;
  FButton.Caption := '';
  FButton.OnClick := @ButtonClick;
  FButton.Align := alRight;
  FButtonWidth := 25;
  FButton.Width := FButtonWidth;

  Height := FDBEdit.Height;
  Width := 150;
end;

function TDBEditButton.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result:= FDBEdit.ExecuteAction(AAction);
end;

procedure TDBEditButton.ButtonClick(Sender: TObject);
begin
  if FFocusOnButtonClick then
    FDBEdit.SetFocus;
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

destructor TDBEditButton.Destroy;
begin
  FDBEdit.Free;
  FButton.Free;
  inherited Destroy;
end;

procedure TDBEditButton.FDBEditChange(Sender: TObject);
begin
  if Assigned(FChangeEvent) then
    FChangeEvent(Self);
end;

procedure TDBEditButton.FDBEditClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TDBEditButton.FDBEditContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnContextPopup) then
    OnContextPopup(Self, MousePos, Handled);
end;

procedure TDBEditButton.FDBEditDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TDBEditButton.FDBEditDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y);
end;

procedure TDBEditButton.FDBEditDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TDBEditButton.FDBEditEditingDone(Sender: TObject);
begin
  if Assigned(OnEditingDone) then
    OnEditingDone(Self);
end;

procedure TDBEditButton.FDBEditEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(OnEndDrag) then
    OnEndDrag(Self, Target, X, Y);
end;

procedure TDBEditButton.FDBEditEnter(Sender: TObject);
begin
  if FButtonOnlyWhenFocused then
    FButton.Visible := FDBEdit.Focused;

  if Assigned(OnEnter) then
    OnEnter(Self);
end;

procedure TDBEditButton.FDBEditExit(Sender: TObject);
begin
  if Assigned(OnExit) then
    OnExit(Self);
end;

procedure TDBEditButton.FDBEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TDBEditButton.FDBEditMouseEnter(Sender: TObject);
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TDBEditButton.FDBEditMouseLeave(Sender: TObject);
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure TDBEditButton.FDBEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;

procedure TDBEditButton.FDBEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TDBEditButton.FDBEditMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnMouseWheel) then
    OnMouseWheel(Self, Shift, WheelDelta, MousePos, Handled);
end;

procedure TDBEditButton.FDBEditMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Handled);
end;

procedure TDBEditButton.FDBEditMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Handled);
end;

procedure TDBEditButton.FDBEditStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if Assigned(OnStartDrag) then
    OnStartDrag(Self, DragObject);
end;

procedure TDBEditButton.FDBEditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Assigned(OnUTF8KeyPress) then
    OnUTF8KeyPress(Self, UTF8Key);
end;

procedure TDBEditButton.FDBEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TDBEditButton.FDBEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TDBEditButton.FDBEditKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

function TDBEditButton.GetAlignment: TAlignment;
begin
  Result := FDBEdit.Alignment;
end;

function TDBEditButton.GetAutoSelect: Boolean;
begin
  Result := FDBEdit.AutoSelect;
end;

function TDBEditButton.GetButtonCaption: TTranslateString;
begin
  Result := FButton.Caption;
end;

function TDBEditButton.GetButtonCursor: TCursor;
begin
  Result := FButton.Cursor;
end;

function TDBEditButton.GetButtonHint: TTranslateString;
begin
  Result := FButton.Hint;
end;

function TDBEditButton.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

function TDBEditButton.GetCharCase: TEditCharCase;
begin
  Result := FDBEdit.CharCase;
end;

function TDBEditButton.GetColor: TColor;
begin
  Result := FDBEdit.Color;
end;

function TDBEditButton.GetCustomEditMask: Boolean;
begin
  Result := FDBEdit.CustomEditMask;
end;

function TDBEditButton.GetDataField: String;
begin
  Result := FDBEdit.DataField;
end;

function TDBEditButton.GetDataSource: TDataSource;
begin
  Result := FDBEdit.DataSource;
end;

function TDBEditButton.GetDisabledImageIndex: Integer;
begin
  Result := FButton.DisabledImageIndex;
end;

function TDBEditButton.GetEditMask: String;
begin
  Result := FDBEdit.EditMask;
end;

function TDBEditButton.GetField: TField;
begin
  Result := FDBEdit.Field;
end;

function TDBEditButton.GetFlat: Boolean;
begin
  Result := FButton.Flat;
end;

function TDBEditButton.GetHint: TTranslateString;
begin
  Result := FDBEdit.Hint;
end;

function TDBEditButton.GetHotImageIndex: Integer;
begin
  Result := FButton.HotImageIndex;
end;

function TDBEditButton.GetImageIndex: Integer;
begin
  Result := FButton.ImageIndex;
end;

function TDBEditButton.GetImages: TCustomImageList;
begin
  Result := FButton.Images;
end;

function TDBEditButton.GetImageWidth: Integer;
begin
  Result := FButton.ImageWidth;
end;

function TDBEditButton.GetMaxLength: Integer;
begin
  Result := FDBEdit.MaxLength;
end;

function TDBEditButton.GetPressedImageIndex: Integer;
begin
  Result := FButton.PressedImageIndex;
end;

function TDBEditButton.GetSelectedImageIndex: Integer;
begin
  Result := FButton.SelectedImageIndex;
end;

function TDBEditButton.GetSpacing: Integer;
begin
  Result := FButton.BorderSpacing.Left;
end;

function TDBEditButton.GetTextHint: TTranslateString;
begin
  Result := FDBEdit.TextHint;
end;

procedure TDBEditButton.SetAlignment(AValue: TAlignment);
begin
  FDBEdit.Alignment := AValue;
end;

procedure TDBEditButton.SetAutoSelect(AValue: Boolean);
begin
  FDBEdit.AutoSelect := AValue;
end;

procedure TDBEditButton.SetButtonCaption(AValue: TTranslateString);
begin
  FButton.Caption := AValue;
end;

procedure TDBEditButton.SetButtonCursor(AValue: TCursor);
begin
  FButton.Cursor := AValue;
end;

procedure TDBEditButton.SetButtonHint(AValue: TTranslateString);
begin
  FButton.Hint := AValue;
end;

procedure TDBEditButton.SetButtonOnlyWhenFocused(AValue: Boolean);
begin
  FButtonOnlyWhenFocused := AValue;
  if FButtonOnlyWhenFocused then
    FButton.Visible := FDBEdit.Focused;
end;

procedure TDBEditButton.SetButtonWidth(AValue: Integer);
begin
  if FButton.Width <> AValue then
  begin
    FButtonWidth := AValue;
    FButton.Width := AValue;
    Resize; // Call Resize to adjust the size of the TDBEdit
  end;
end;

procedure TDBEditButton.SetCharCase(AValue: TEditCharCase);
begin
  FDBEdit.CharCase := AValue;
end;

procedure TDBEditButton.SetCustomEditMask(AValue: Boolean);
begin
  FDBEdit.CustomEditMask := AValue;
end;

procedure TDBEditButton.SetDataField(AValue: String);
begin
  FDBEdit.DataField := AValue;
end;

procedure TDBEditButton.SetDataSource(AValue: TDataSource);
begin
  FDBEdit.DataSource := AValue;
end;

procedure TDBEditButton.SetDirectInput(AValue: Boolean);
begin
  FDirectInput := AValue;
  FDBEdit.ReadOnly := not FDirectInput or FReadOnly;
end;

procedure TDBEditButton.SetDisabledImageIndex(AValue: Integer);
begin
  FButton.DisabledImageIndex := AValue;
end;

procedure TDBEditButton.SetEditMask(AValue: String);
begin
  FDBEdit.EditMask := AValue;
end;

procedure TDBEditButton.SetFlat(AValue: Boolean);
begin
  FButton.Flat := AValue;
end;

procedure TDBEditButton.SetFocusOnButtonClick(AValue: Boolean);
begin
  FFocusOnButtonClick := AValue;
end;

procedure TDBEditButton.SetHotImageIndex(AValue: Integer);
begin
  FButton.HotImageIndex := AValue;
end;

procedure TDBEditButton.SetImageIndex(AValue: Integer);
begin
  FButton.ImageIndex := AValue;
end;

procedure TDBEditButton.SetImages(AValue: TCustomImageList);
begin
  FButton.Images := AValue;
end;

procedure TDBEditButton.SetImageWidth(AValue: Integer);
begin
  FButton.ImageWidth := AValue;
end;

procedure TDBEditButton.SetMaxLength(AValue: Integer);
begin
  FDBEdit.MaxLength := AValue;
end;

procedure TDBEditButton.SetPasswordChar(AValue: Char);
begin
  if FPasswordChar = AValue then
    Exit;
  FPasswordChar := AValue;
  FDBEdit.PasswordChar := AValue;
end;

procedure TDBEditButton.SetPressedImageIndex(AValue: Integer);
begin
  FButton.PressedImageIndex := AValue;
end;

procedure TDBEditButton.SetReadOnly(AValue: Boolean);
begin
  FDBEdit.ReadOnly := AValue;
end;

procedure TDBEditButton.SetSelectedImageIndex(AValue: Integer);
begin
  FButton.SelectedImageIndex := AValue;
end;

procedure TDBEditButton.SetShowHint(AValue: Boolean);
begin
  if FShowHint = AValue then Exit;
    FShowHint := AValue;

  FDBEdit.ShowHint := FShowHint;
  FButton.ShowHint := FShowHint;
end;

procedure TDBEditButton.SetSpacing(AValue: Integer);
begin
  FButton.BorderSpacing.Left := AValue;
end;

procedure TDBEditButton.SetTextHint(AValue: TTranslateString);
begin
  FDBEdit.TextHint := AValue;
end;

function TDBEditButton.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := FDBEdit.UpdateAction(AAction);
end;

end.
