{****************************
DBEditButton

Based on:
F1DBEditButton.
Simple Edit Control with a Button
by Bambang Pranoto
*****************************}

unit DBEditButton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons,
  DB, DBCtrls, ImgList, LCLType;

type

  { TDBEditButton }

  TDBEditButton = class(TPanel)
  private
    FDBEdit: TDBEdit ;
    FButton: TSpeedButton;
    FButtonWidth: Integer;
    FButtonClickEvent : TNotifyEvent;
    FChangeEvent : TNotifyEvent;
    FDBEditKeyDownEvent: TKeyEvent;
    FDBEditKeyPressEvent: TKeyPressEvent;
    FDBEditKeyUpEvent: TKeyEvent;
    FPasswordChar: Char;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetCharCase: TEditCharCase;
    function GetDataField: String;
    function GetDataSource: TDataSource;
    function GetDisabledImageIndex: Integer;
    function GetField: TField;
    function GetHotImageIndex: Integer;
    function GetImageIndex: Integer;
    function GetImages: TCustomImageList;
    function GetImageWidth: Integer;
    function GetPressedImageIndex: Integer;
    function GetReadOnly: Boolean;
    function GetSelectedImageIndex: Integer;
    function GetTextHint: TTranslateString;
    procedure SetButtonHint(AValue: TTranslateString);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetDataField(AValue: String);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDisabledImageIndex(AValue: Integer);
    procedure SetHotImageIndex(AValue: Integer);
    procedure SetImageIndex(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImageWidth(AValue: Integer);
    procedure SetPasswordChar(AValue: Char);
    procedure SetPressedImageIndex(AValue: Integer);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSelectedImageIndex(AValue: Integer);
    procedure SetTextHint(AValue: TTranslateString);
  protected
    procedure FButtonClick(Sender: TObject);
    procedure FDBEditChange(Sender:TObject);
    procedure FDBEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FDBEditKeyPress(Sender: TObject; var Key: Char);
    procedure FDBEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;

    function ExecuteAction(AAction: TBasicAction): Boolean;
    function UpdateAction(AAction: TBasicAction): Boolean;
    property Field: TField read GetField;
  published
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 29;
    property DisabledImageIndex: Integer read GetDisabledImageIndex write SetDisabledImageIndex default -1;
    property HotImageIndex: Integer read GetHotImageIndex write SetHotImageIndex default -1;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth default 0;
    property PressedImageIndex: Integer read GetPressedImageIndex write SetPressedImageIndex default -1;
    property SelectedImageIndex: Integer read GetSelectedImageIndex write SetSelectedImageIndex default -1;
    property TextHint: TTranslateString read GetTextHint write SetTextHint;

    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange:TNotifyEvent read FChangeEvent write FChangeEvent;
    property OnButtonClick:TNotifyEvent read FButtonClickEvent write FButtonClickEvent;
    property OnDBEditKeyDown: TKeyEvent read FDBEditKeyDownEvent write FDBEditKeyDownEvent;
    property OnDBEditKeyPress: TKeyPressEvent read FDBEditKeyPressEvent write FDBEditKeyPressEvent;
    property OnDBEditKeyUp: TKeyEvent read FDBEditKeyUpEvent write FDBEditKeyUpEvent;
    property OnClick;
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

  Self.Width := 105;
  Self.Height := 28;
  Self.FButtonWidth := 29;
  Self.BevelOuter := bvNone;

  Self.FButton := TSpeedButton.Create(Self);
  Self.FButton.Parent := Self;
  Self.FButton.AutoSize := False;
  Self.FButton.Align := alRight;
  Self.FButton.Width := Self.FButtonWidth;
  //Self.FButton.Constraints.MinWidth := Self.FButtonWidth;
  Self.FButton.Caption := EmptyStr;
  Self.FButton.Hint := EmptyStr;

  Self.FButton.OnClick := @FButtonClick;

  Self.FDBEdit := TDBEdit.Create(Self);
  Self.FDBEdit.Parent := Self;
  //Self.Height := Self.FDBEdit.Height;
  Self.Constraints.MaxHeight := Self.Height;
  Self.FDBEdit.Top := 0;
  Self.FDBEdit.Left := 0;
  Self.FDBEdit.Align := alClient;
  Self.FDBEdit.ParentFont := True;
  Self.FDBEdit.OnChange := FChangeEvent;
  Self.FDBEdit.OnKeyDown := @FDBEditKeyDown;
  Self.FDBEdit.OnKeyPress := @FDBEditKeyPress;
  Self.FDBEdit.OnKeyUp := @FDBEditKeyUp;

  Self.Caption := '';
end;

function TDBEditButton.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result:= Self.FDBEdit.ExecuteAction(AAction);
end;

procedure TDBEditButton.FButtonClick(Sender: TObject);
begin
  Self.FDBEdit.SetFocus;
  if Assigned( Self.FButtonClickEvent) then
  begin
    Self.FButtonClickEvent(Self);
  end;
end;

procedure TDBEditButton.FDBEditChange(Sender: TObject);
begin
  if Assigned(Self.FChangeEvent) then
  begin
    Self.FChangeEvent(Self);
  end;
end;

procedure TDBEditButton.FDBEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Self.FDBEditKeyDownEvent) then
  begin
    Self.FDBEditKeyDownEvent(Self, Key, Shift);
  end;
end;

procedure TDBEditButton.FDBEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(Self.FDBEditKeyPressEvent) then
  begin
    Self.FDBEditKeyPressEvent(Self, Key);
  end;
end;

procedure TDBEditButton.FDBEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Self.FDBEditKeyUpEvent) then
  begin
    Self.FDBEditKeyUpEvent(Self, Key, Shift);
  end;
end;

function TDBEditButton.GetButtonHint: TTranslateString;
begin
  Result := Self.FButton.Hint;
end;

function TDBEditButton.GetButtonWidth: Integer;
begin
  Result := Self.FButtonWidth;
end;

function TDBEditButton.GetCharCase: TEditCharCase;
begin
  Result := Self.FDBEdit.CharCase;
end;

function TDBEditButton.GetDataField: String;
begin
  Result := Self.FDBEdit.DataField;
end;

function TDBEditButton.GetDataSource: TDataSource;
begin
  Result := Self.FDBEdit.DataSource;
end;

function TDBEditButton.GetDisabledImageIndex: Integer;
begin
  Result := Self.FButton.DisabledImageIndex;
end;

function TDBEditButton.GetField: TField;
begin
  Result := Self.FDBEdit.Field;
end;

function TDBEditButton.GetHotImageIndex: Integer;
begin
  Result := Self.FButton.HotImageIndex;
end;

function TDBEditButton.GetImageIndex: Integer;
begin
  Result := Self.FButton.ImageIndex;
end;

function TDBEditButton.GetImages: TCustomImageList;
begin
  Result := Self.FButton.Images;
end;

function TDBEditButton.GetImageWidth: Integer;
begin
  Result := Self.FButton.ImageWidth;
end;

function TDBEditButton.GetPressedImageIndex: Integer;
begin
  Result := Self.FButton.PressedImageIndex;
end;

function TDBEditButton.GetReadOnly: Boolean;
begin
  Result := Self.FDBEdit.ReadOnly;
end;

function TDBEditButton.GetSelectedImageIndex: Integer;
begin
  Result := Self.FButton.SelectedImageIndex;
end;

function TDBEditButton.GetTextHint: TTranslateString;
begin
  Result := Self.FDBEdit.TextHint;
end;

procedure TDBEditButton.SetButtonHint(AValue: TTranslateString);
begin
  Self.FButton.Hint := AValue;
end;

procedure TDBEditButton.SetButtonWidth(AValue: Integer);
begin
  //if Self.FButtonWidth = AValue then
  //  Exit;

  Self.FButtonWidth := AValue;
  Self.FButton.Width := Self.FButtonWidth;
end;

procedure TDBEditButton.SetCharCase(AValue: TEditCharCase);
begin
  Self.FDBEdit.CharCase := AValue;
end;

procedure TDBEditButton.SetDataField(AValue: String);
begin
  Self.FDBEdit.DataField := AValue;
end;

procedure TDBEditButton.SetDataSource(AValue: TDataSource);
begin
  Self.FDBEdit.DataSource := AValue;
end;

procedure TDBEditButton.SetDisabledImageIndex(AValue: Integer);
begin
  Self.FButton.DisabledImageIndex := AValue;
end;

procedure TDBEditButton.SetHotImageIndex(AValue: Integer);
begin
  Self.FButton.HotImageIndex := AValue;
end;

procedure TDBEditButton.SetImageIndex(AValue: Integer);
begin
  Self.FButton.ImageIndex := AValue;
end;

procedure TDBEditButton.SetImages(AValue: TCustomImageList);
begin
  Self.FButton.Images := AValue;
end;

procedure TDBEditButton.SetImageWidth(AValue: Integer);
begin
  Self.FButton.ImageWidth := AValue;
end;

procedure TDBEditButton.SetPasswordChar(AValue: Char);
begin
  if FPasswordChar = AValue then
    Exit;
  FPasswordChar := AValue;
end;

procedure TDBEditButton.SetPressedImageIndex(AValue: Integer);
begin
  Self.FButton.PressedImageIndex := AValue;
end;

procedure TDBEditButton.SetReadOnly(AValue: Boolean);
begin
  Self.FDBEdit.ReadOnly := AValue;
end;

procedure TDBEditButton.SetSelectedImageIndex(AValue: Integer);
begin
  Self.FButton.SelectedImageIndex := AValue;
end;

procedure TDBEditButton.SetTextHint(AValue: TTranslateString);
begin
  Self.FDBEdit.TextHint := AValue;
end;

function TDBEditButton.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := Self.FDBEdit.UpdateAction(AAction);
end;

end.
