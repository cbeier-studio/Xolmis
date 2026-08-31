unit chipspanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, StdCtrls, ExtCtrls, BCPanel, BCTypes;

const
  { Same hue order in both palettes, so index N in ChipPaletteLight and ChipPaletteDark represents the same color family. }
  ChipPaletteLight: array[0..14] of TColor = (
    $00FDF2E3, $00E9F5E8, $00E1F8FF, $00E7E9FB, $00EEEBFF,
    $00F5E5F3, $00FAF7E0, $00F1F2E0, $00F5F5F5, $00E9EBEF,
    $00FEF5E1, $00E9F8F1, $00ECE4FC, $00E0F3FF, $00F1EFEC
  );
  ChipPaletteDark: array[0..14] of TColor = (
    $00A1470D, $00205E1B, $00006E8D, $000C36BF, $001C1CB7,
    $008C144A, $00646000, $00404D00, $00424242, $002E344E,
    $009B5701, $001E6933, $004F0E88, $000051E6, $00383226
  );

type
  TChipColorMode = (cmPalette, cmSingleColor);

  { TChip }

  TChip = class(TBCPanel)
  private
    FCaption: String;
    FIndex: Integer;
    FColorIndex: Integer;
    FData: Pointer;
    FID: String;
    FLabel: TLabel;
    FSelected: Boolean;
    FCloseBtn: TLabel;
    procedure SetCaption(const AValue: String);
    procedure SetIndex(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetID(AValue: String);
    procedure LabelClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: String read FCaption write SetCaption;
    property Index: Integer read FIndex write SetIndex;
    property ColorIndex: Integer read FColorIndex write FColorIndex default -1;
    property ID: String read FID write SetID;
    property Data: Pointer read FData write FData;
    property Selected: Boolean read FSelected write SetSelected default False;
  end;

  TOnChipClick = procedure(Sender: TObject; AChip: TChip) of object;

  { TChipsPanel }

  TChipsPanel = class(TFlowPanel)
  private
    FDarkMode: Boolean;
    FColorMode: TChipColorMode;
    FSelectable: Boolean;
    FMultiSelect: Boolean;
    FChipCornerRadius: Integer;
    FChipSpacing: Integer;
    FChipPadding: Integer;
    FSelectedTextColorLight: TColor;
    FSelectedTextColorDark: TColor;
    FSingleChipColorLight: TColor;
    FSingleChipColorDark: TColor;
    FSingleTextColorLight: TColor;
    FSingleTextColorDark: TColor;
    FUnselectedFillLight: TColor;
    FUnselectedFillDark: TColor;
    FUnselectedBorderLight: TColor;
    FUnselectedBorderDark: TColor;
    FUnselectedTextLight: TColor;
    FUnselectedTextDark: TColor;
    FOnChipClick: TOnChipClick;

    FPaletteLight: array of TColor;
    FPaletteDark: array of TColor;

    FChips: TList;
    FSelectedChips: TList;

    procedure SetDarkMode(AValue: Boolean);
    procedure SetColorMode(AValue: TChipColorMode);
    procedure SetSingleChipColorLight(AValue: TColor);
    procedure SetSingleChipColorDark(AValue: TColor);
    procedure SetSingleTextColorLight(AValue: TColor);
    procedure SetSingleTextColorDark(AValue: TColor);
    procedure SetSelectedTextColorLight(AValue: TColor);
    procedure SetSelectedTextColorDark(AValue: TColor);
    procedure SetUnselectedFillLight(AValue: TColor);
    procedure SetUnselectedFillDark(AValue: TColor);
    procedure SetUnselectedBorderLight(AValue: TColor);
    procedure SetUnselectedBorderDark(AValue: TColor);
    procedure SetUnselectedTextLight(AValue: TColor);
    procedure SetUnselectedTextDark(AValue: TColor);
    procedure SetSelectable(AValue: Boolean);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetChipCornerRadius(AValue: Integer);
    procedure SetChipSpacing(AValue: Integer);
    procedure SetChipPadding(AValue: Integer);
    procedure InitializePalette;
    procedure ApplyColorsToChips;
    procedure UpdateChipsList;
    procedure UpdateSelectedChipsList;
    procedure RemoveChipAsync(Data: PtrInt);
  protected
    procedure Loaded; override;
    procedure DoChipClick(AChip: TChip);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddChip(const AText: String; const AID: String = ''): TChip;
    procedure ClearChips;

    procedure SelectByCaption(const ACaption: String; ASelected: Boolean = True);
    procedure SelectByID(const AID: String; ASelected: Boolean = True);
    procedure SelectByCaptions(const ACaptions: TStrings);
    procedure SelectByIDs(const AIDs: TStrings);
    procedure ClearSelection;
    function GetChips: TList;
    function GetSelectedChips: TList;
    function GetSelectedIDs(AStrings: TStrings): TStrings;
  published
    property DarkMode: Boolean read FDarkMode write SetDarkMode default False;
    property ColorMode: TChipColorMode read FColorMode write SetColorMode default cmPalette;
    property Selectable: Boolean read FSelectable write SetSelectable default True;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ChipCornerRadius: Integer read FChipCornerRadius write SetChipCornerRadius default 12;
    property ChipSpacing: Integer read FChipSpacing write SetChipSpacing default 6;
    property ChipPadding: Integer read FChipPadding write SetChipPadding default 8;

    property Chips: TList read FChips;
    property SelectedChips: TList read FSelectedChips;

    property SelectedTextColorLight: TColor read FSelectedTextColorLight write SetSelectedTextColorLight default $00000000;
    property SelectedTextColorDark: TColor read FSelectedTextColorDark write SetSelectedTextColorDark default $00FFFFFF;

    property SingleChipColorLight: TColor read FSingleChipColorLight write SetSingleChipColorLight default $00FDE2E3;
    property SingleChipColorDark: TColor read FSingleChipColorDark write SetSingleChipColorDark default $00C06515;
    property SingleTextColorLight: TColor read FSingleTextColorLight write SetSingleTextColorLight default $00000000;
    property SingleTextColorDark: TColor read FSingleTextColorDark write SetSingleTextColorDark default $00FFFFFF;

    property UnselectedFillLight: TColor read FUnselectedFillLight write SetUnselectedFillLight default $00F0F0F0;
    property UnselectedFillDark: TColor read FUnselectedFillDark write SetUnselectedFillDark default $00404040;
    property UnselectedBorderLight: TColor read FUnselectedBorderLight write SetUnselectedBorderLight default $00808080;
    property UnselectedBorderDark: TColor read FUnselectedBorderDark write SetUnselectedBorderDark default $00C0C0C0;
    property UnselectedTextLight: TColor read FUnselectedTextLight write SetUnselectedTextLight default $00000000;
    property UnselectedTextDark: TColor read FUnselectedTextDark write SetUnselectedTextDark default $00FFFFFF;

    property OnChipClick: TOnChipClick read FOnChipClick write FOnChipClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CBS', [TChipsPanel]);
end;

{ TChip }

constructor TChip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Chip';
  FIndex := 0;
  FColorIndex := -1;
  FData := nil;
  FID := '';
  FSelected := False;

  BorderBCStyle := bpsBorder;
  Rounding.RoundX := 12;
  Rounding.RoundY := 12;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height := 32;
  Width := 100;
  ChildSizing.TopBottomSpacing := 4;
  ChildSizing.LeftRightSpacing := 4;
  ChildSizing.HorizontalSpacing := 8;
  //Alignment := taCenter;
  Cursor := crHandPoint;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := FCaption;
  FLabel.AnchorAsAlign(alLeft, 8);
  FLabel.Cursor := crHandPoint;
  FLabel.OnClick := @LabelClick;

  FCloseBtn := TLabel.Create(Self);
  FCloseBtn.Parent := Self;
  FCloseBtn.Caption := '×';
  //FCloseBtn.AnchorAsAlign(alLeft, 8);
  FCloseBtn.AnchorToNeighbour(akLeft, 8, FLabel);
  FCloseBtn.AnchorSide[akTop].Control := Self;
  FCloseBtn.OnClick := @CloseClick;

  AutoSize := True;
end;

procedure TChip.LabelClick(Sender: TObject);
begin
  Click;
end;

procedure TChip.CloseClick(Sender: TObject);
begin
  Visible := False;

  if Parent is TChipsPanel then
    TChipsPanel(Parent).DoChipClick(Self);
end;

procedure TChip.SetIndex(AValue: Integer);
begin
  if FIndex <> AValue then
    FIndex := AValue;
end;

procedure TChip.SetID(AValue: String);
begin
  if FID <> AValue then
    FID := AValue;
end;

procedure TChip.SetSelected(AValue: Boolean);
begin
  if FSelected <> AValue then
  begin
    FSelected := AValue;
    if Parent is TChipsPanel then
      TChipsPanel(Parent).ApplyColorsToChips;
  end;
end;

procedure TChip.Click;
begin
  inherited Click;
  if Parent is TChipsPanel then
    TChipsPanel(Parent).DoChipClick(Self);
end;

procedure TChip.SetCaption(const AValue: String);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    if Assigned(FLabel) then
      FLabel.Caption := FCaption;
  end;
end;

{ TChipsPanel }

constructor TChipsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FChips := TList.Create;
  FSelectedChips := TList.Create;

  AutoWrap := True;
  FlowStyle := fsLeftRightTopBottom;
  BevelOuter := bvNone;

  FDarkMode := False;
  FColorMode := cmPalette;
  FSelectable := True;
  FMultiSelect := False;
  FChipCornerRadius := 12;
  FChipSpacing := 6;
  FChipPadding := 8;

  FSelectedTextColorLight := $00000000;
  FSelectedTextColorDark  := $00FFFFFF;

  FSingleChipColorLight := $00FDE2E3;
  FSingleChipColorDark  := $00C06515;
  FSingleTextColorLight := $00000000;
  FSingleTextColorDark  := $00FFFFFF;

  FUnselectedFillLight   := $00F0F0F0;
  FUnselectedFillDark    := $00404040;
  FUnselectedBorderLight := $00808080;
  FUnselectedBorderDark  := $00C0C0C0;
  FUnselectedTextLight   := $00000000;
  FUnselectedTextDark    := $00FFFFFF;

  InitializePalette;
end;

destructor TChipsPanel.Destroy;
begin
  FSelectedChips.Free;
  FChips.Free;

  SetLength(FPaletteLight, 0);
  SetLength(FPaletteDark, 0);
  inherited Destroy;
end;

procedure TChipsPanel.InitializePalette;
var
  I: Integer;
begin
  SetLength(FPaletteLight, Length(ChipPaletteLight));
  SetLength(FPaletteDark, Length(ChipPaletteDark));
  for I := Low(ChipPaletteLight) to High(ChipPaletteLight) do
  begin
    FPaletteLight[I] := ChipPaletteLight[I];
    FPaletteDark[I] := ChipPaletteDark[I];
  end;
end;

procedure TChipsPanel.ApplyColorsToChips;
var
  I, Idx: Integer;
  Chip: TChip;
  BgColor, TextColor: TColor;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if Controls[I] is TChip then
    begin
      Chip := TChip(Controls[I]);
      Chip.Index := I;
      Chip.Rounding.RoundX := FChipCornerRadius;
      Chip.Rounding.RoundY := FChipCornerRadius;
      Chip.BorderSpacing.Bottom := FChipSpacing;
      Chip.BorderSpacing.Left := FChipSpacing;
      Chip.BorderSpacing.Right := FChipSpacing;
      Chip.BorderSpacing.Top := FChipSpacing;
      Chip.ChildSizing.TopBottomSpacing := FChipPadding;
      Chip.ChildSizing.LeftRightSpacing := FChipPadding;
      Chip.FLabel.AnchorAsAlign(alLeft, FChipPadding);
      //Chip.FCloseBtn.AnchorAsAlign(alLeft, FChipPadding);
      Chip.FCloseBtn.AnchorToNeighbour(akLeft, FChipPadding, Chip.FLabel);
      Chip.FCloseBtn.Visible := not FSelectable;

      if Chip.Selected then
      begin
        // Chip selected: without border, full color with contrasting text
        Chip.Border.Width := 0;
        if FColorMode = cmSingleColor then
        begin
          if FDarkMode then
          begin
            BgColor := FSingleChipColorDark;
            TextColor := FSingleTextColorDark;
          end
          else
          begin
            BgColor := FSingleChipColorLight;
            TextColor := FSingleTextColorLight;
          end;
        end
        else
        begin
          if Chip.ColorIndex >= 0 then
            Idx := Chip.ColorIndex
          else
            Idx := I;
          if FDarkMode then
          begin
            BgColor := FPaletteDark[Idx mod Length(FPaletteDark)];
            TextColor := FSelectedTextColorDark;
          end
          else
          begin
            BgColor := FPaletteLight[Idx mod Length(FPaletteLight)];
            TextColor := FSelectedTextColorLight;
          end;
        end;
      end
      else
      begin
        // Chip not selected: always neutral (gray), independent from ColorMode and palette
        Chip.Border.Width := 1;
        Chip.Border.Style := bboSolid;
        if FDarkMode then
        begin
          BgColor := FUnselectedFillDark;
          TextColor := FUnselectedTextDark;
          Chip.Border.Color := FUnselectedBorderDark;
        end
        else
        begin
          BgColor := FUnselectedFillLight;
          TextColor := FUnselectedTextLight;
          Chip.Border.Color := FUnselectedBorderLight;
        end;
      end;

      Chip.Background.Color := BgColor;
      Chip.Font.Color := TextColor;
      Chip.FLabel.Font.Color := TextColor;
      Chip.FCloseBtn.Font.Color := TextColor;
    end;
  end;
end;

procedure TChipsPanel.DoChipClick(AChip: TChip);
begin
  if not FSelectable then
  begin
    if Assigned(FOnChipClick) then
      FOnChipClick(Self, AChip);
    AChip.Visible := False;
    Application.QueueAsyncCall(@RemoveChipAsync, PtrInt(AChip));
    Exit;
  end;

  if FMultiSelect then
    AChip.Selected := not AChip.Selected
  else if AChip.Selected then
    AChip.Selected := False
  else
  begin
    ClearSelection;
    AChip.Selected := True;
  end;

  UpdateSelectedChipsList;

  if Assigned(FOnChipClick) then
    FOnChipClick(Self, AChip);
end;

procedure TChipsPanel.Loaded;
begin
  inherited Loaded;
  ApplyColorsToChips;
end;

procedure TChipsPanel.RemoveChipAsync(Data: PtrInt);
begin
  TChip(Data).Free;
  UpdateChipsList;
  UpdateSelectedChipsList;
  ApplyColorsToChips;
end;

function TChipsPanel.AddChip(const AText: String; const AID: String = ''): TChip;
begin
  Result := TChip.Create(Self);
  Result.Caption := AText;

  if AID <> '' then
    Result.ID := AID
  else
    Result.ID := AText;

  Result.Parent := Self;
  ApplyColorsToChips;
  UpdateChipsList;
end;

procedure TChipsPanel.ClearChips;
var
  I: Integer;
begin
  for I := ControlCount - 1 downto 0 do
  begin
    if Controls[I] is TChip then
      Controls[I].Free;
  end;
  FChips.Clear;
  FSelectedChips.Clear;
end;

{ --- MÉTODOS DE SELEÇÃO PROGRAMÁTICA --- }

procedure TChipsPanel.SelectByCaption(const ACaption: String; ASelected: Boolean = True);
var
  I: Integer;
  Chip: TChip;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if Controls[I] is TChip then
    begin
      Chip := TChip(Controls[I]);
      if SameText(Chip.Caption, ACaption) then
      begin
        if not FMultiSelect and ASelected then
          ClearSelection;
        Chip.Selected := ASelected;
        UpdateSelectedChipsList;
        if not FMultiSelect and ASelected then Break;
      end;
    end;
  end;
end;

procedure TChipsPanel.SelectByID(const AID: String; ASelected: Boolean = True);
var
  I: Integer;
  Chip: TChip;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if Controls[I] is TChip then
    begin
      Chip := TChip(Controls[I]);
      if SameText(Chip.ID, AID) then
      begin
        if not FMultiSelect and ASelected then
          ClearSelection;
        Chip.Selected := ASelected;
        UpdateSelectedChipsList;
        if not FMultiSelect and ASelected then Break;
      end;
    end;
  end;
end;

procedure TChipsPanel.SelectByCaptions(const ACaptions: TStrings);
var
  I: Integer;
begin
  if not FMultiSelect then ClearSelection;
  for I := 0 to ACaptions.Count - 1 do
    SelectByCaption(ACaptions[I], True);
end;

procedure TChipsPanel.SelectByIDs(const AIDs: TStrings);
var
  I: Integer;
begin
  if not FMultiSelect then ClearSelection;
  for I := 0 to AIDs.Count - 1 do
    SelectByID(AIDs[I], True);
end;

procedure TChipsPanel.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    if Controls[I] is TChip then
      TChip(Controls[I]).FSelected := False;
  end;
  UpdateSelectedChipsList;
  ApplyColorsToChips;
end;

function TChipsPanel.GetChips: TList;
begin
  UpdateChipsList;
  Result := FChips;
end;

function TChipsPanel.GetSelectedChips: TList;
begin
  UpdateSelectedChipsList;
  Result := FSelectedChips;
end;

function TChipsPanel.GetSelectedIDs(AStrings: TStrings): TStrings;
var
  I: Integer;
begin
  if not Assigned(AStrings) then Exit(nil);

  AStrings.Clear;
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[I] is TChip) and TChip(Controls[I]).Selected then
      AStrings.Add(TChip(Controls[I]).ID);
  end;
  Result := AStrings;
end;

{ Setters }

procedure TChipsPanel.SetDarkMode(AValue: Boolean);
begin
  if FDarkMode <> AValue then
  begin
    FDarkMode := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSelectable(AValue: Boolean);
begin
  if FSelectable <> AValue then
  begin
    FSelectable := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetMultiSelect(AValue: Boolean);
begin
  if FMultiSelect <> AValue then
  begin
    FMultiSelect := AValue;
    if not FMultiSelect then
      ClearSelection;
  end;
end;

procedure TChipsPanel.SetChipCornerRadius(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FChipCornerRadius <> AValue then
  begin
    FChipCornerRadius := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetChipSpacing(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FChipSpacing <> AValue then
  begin
    FChipSpacing := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetChipPadding(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FChipPadding <> AValue then
  begin
    FChipPadding := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetColorMode(AValue: TChipColorMode);
begin
  if FColorMode <> AValue then
  begin
    FColorMode := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSingleChipColorLight(AValue: TColor);
begin
  if FSingleChipColorLight <> AValue then
  begin
    FSingleChipColorLight := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSingleChipColorDark(AValue: TColor);
begin
  if FSingleChipColorDark <> AValue then
  begin
    FSingleChipColorDark := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSingleTextColorLight(AValue: TColor);
begin
  if FSingleTextColorLight <> AValue then
  begin
    FSingleTextColorLight := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSingleTextColorDark(AValue: TColor);
begin
  if FSingleTextColorDark <> AValue then
  begin
    FSingleTextColorDark := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSelectedTextColorLight(AValue: TColor);
begin
  if FSelectedTextColorLight <> AValue then
  begin
    FSelectedTextColorLight := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetSelectedTextColorDark(AValue: TColor);
begin
  if FSelectedTextColorDark <> AValue then
  begin
    FSelectedTextColorDark := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetUnselectedFillLight(AValue: TColor);
begin
  if FUnselectedFillLight <> AValue then
  begin
    FUnselectedFillLight := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetUnselectedFillDark(AValue: TColor);
begin
  if FUnselectedFillDark <> AValue then
  begin
    FUnselectedFillDark := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetUnselectedBorderLight(AValue: TColor);
begin
  if FUnselectedBorderLight <> AValue then
  begin
    FUnselectedBorderLight := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetUnselectedBorderDark(AValue: TColor);
begin
  if FUnselectedBorderDark <> AValue then
  begin
    FUnselectedBorderDark := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.SetUnselectedTextLight(AValue: TColor);
begin
  if FUnselectedTextLight <> AValue then
  begin
    FUnselectedTextLight := AValue;
    ApplyColorsToChips;
  end;
end;

procedure TChipsPanel.UpdateChipsList;
var
  I: Integer;
begin
  FChips.Clear;
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[I] is TChip) then
      FChips.Add(Controls[I]);
  end;
end;

procedure TChipsPanel.UpdateSelectedChipsList;
var
  I: Integer;
begin
  FSelectedChips.Clear;
  for I := 0 to ControlCount - 1 do
  begin
    if (Controls[I] is TChip) and TChip(Controls[I]).Selected then
      FSelectedChips.Add(Controls[I]);
  end;
end;

procedure TChipsPanel.SetUnselectedTextDark(AValue: TColor);
begin
  if FUnselectedTextDark <> AValue then
  begin
    FUnselectedTextDark := AValue;
    ApplyColorsToChips;
  end;
end;

end.
