{ Xolmis Breeding/Behavior Code Editor dialog

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

unit ulst_breedingstatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, laz.VirtualTrees, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, atshapelinebgra, BCPanel;

type

  { TlstBreedingStatus }

  TlstBreedingStatus = class(TForm)
    vtBreeding: TLazVirtualStringTree;
    lineBottom: TShapeLineBGRA;
    pCode: TBCPanel;
    sbOK: TBitBtn;
    sbCancel: TBitBtn;
    pBottom: TPanel;
    procedure cklBreedClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
    procedure vtBreedingChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtBreedingGetText
      (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vtBreedingInitChildren
      (Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtBreedingInitNode
      (Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FBreedingStatus: String;
    procedure ApplyDarkMode;
    procedure UpdateBreedingCode;
    procedure UpdateCheckedByCode;
  public
    property BreedingStatus: String read FBreedingStatus write FBreedingStatus;
  end;

var
  lstBreedingStatus: TlstBreedingStatus;

implementation

uses utils_locale, utils_global, utils_dialogs, utils_themes, uDarkStyleParams;

{$R *.lfm}

type
  PNodeData = ^TNodeData;
  TNodeData = record
    ItemCaption: String;
    IsParent: Boolean;
  end;

{ TlstBreedingStatus }

procedure TlstBreedingStatus.ApplyDarkMode;
begin
  pCode.Background.Color := ActiveTheme.Interactive.SelectionBackground;
end;

procedure TlstBreedingStatus.cklBreedClickCheck(Sender: TObject);
begin
  GravaStat(Name, 'cklBreed', 'clickcheck');
  pCode.Caption := FBreedingStatus;
end;

procedure TlstBreedingStatus.FormCreate(Sender: TObject);
begin
  FBreedingStatus := '';

  vtBreeding.NodeDataSize := SizeOf(TNodeData);
  vtBreeding.RootNodeCount := 5;
end;

procedure TlstBreedingStatus.FormKeyPress(Sender: TObject; var Key: char);
begin
  { CANCELAR = Esc }
  if Key = #27 then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { APLICAR = Enter }
  if Key = #13 then
  begin
    GravaStat(Name, '', 'Enter');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Enter/Return');
    {$ENDIF}
    Key := #0;
    sbOKClick(nil);
  end;
end;

procedure TlstBreedingStatus.FormShow(Sender: TObject);
begin
  if IsDarkModeEnabled then
    ApplyDarkMode;

  if Length(FBreedingStatus) > 0 then
  begin
    pCode.Caption := FBreedingStatus;
    UpdateCheckedByCode;
  end;

  vtBreeding.FullExpand();
end;

procedure TlstBreedingStatus.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstBreedingStatus.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if FBreedingStatus = '' then
  begin
    MsgDlg('', rsListCheckedNone, mtError);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TlstBreedingStatus.UpdateBreedingCode;
var
  Node: PVirtualNode;
  Data: PNodeData;
  CheckedString: String;
  CodePart: String;
begin
  CheckedString := '';
  Node := vtBreeding.GetFirst;
  while Assigned(Node) do
  begin
    Data := vtBreeding.GetNodeData(Node);
    if not Data^.IsParent and (Node^.CheckState = csCheckedNormal) then
    begin
      CodePart := Trim(Copy(Data^.ItemCaption, 1, Pos(' -', Data^.ItemCaption) - 1));
      if CheckedString <> '' then
        CheckedString := CheckedString + ',';
      CheckedString := CheckedString + CodePart;
    end;
    Node := vtBreeding.GetNext(Node);
  end;
  FBreedingStatus := CheckedString;
  pCode.Caption := FBreedingStatus;
end;

procedure TlstBreedingStatus.UpdateCheckedByCode;
var
  Node: PVirtualNode;
  Data: PNodeData;
  i: Integer;
  CodePart: String;
  Codes: TStringList;
begin
  Codes := TStringList.Create;
  try
    Codes.Delimiter := ',';
    Codes.StrictDelimiter := True;
    Codes.DelimitedText := FBreedingStatus;

    Node := vtBreeding.GetFirst;
    while Assigned(Node) do
    begin
      Data := vtBreeding.GetNodeData(Node);
      if not Data^.IsParent then
      begin
        CodePart := Trim(Copy(Data^.ItemCaption, 1, Pos(' -', Data^.ItemCaption) - 1));

        for i := 0 to Codes.Count - 1 do
        begin
          if CodePart = Codes[i] then
          begin
            vtBreeding.CheckState[Node] := csCheckedNormal;
            //Break;
          end;
        end;
      end;
      Node := vtBreeding.GetNext(Node);
    end;
  finally
    Codes.Free;
  end;
end;

procedure TlstBreedingStatus.vtBreedingChecked
  (Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateBreedingCode;
end;

procedure TlstBreedingStatus.vtBreedingGetText
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PNodeData;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data^.ItemCaption;
end;

procedure TlstBreedingStatus.vtBreedingInitChildren
  (Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
begin
  case Node^.Index of
    0: ChildCount := 9; // Confirmed
    1: ChildCount := 2;  // Confirmed/Probable
    2: ChildCount := 9;  // Probable
    3: ChildCount := 2;  // Possible
    4: ChildCount := 1;  // Observed
  end;
end;

procedure TlstBreedingStatus.vtBreedingInitNode
  (Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(ParentNode) then
  begin
    Data^.IsParent := True;
    case Node^.Index of
      0: Data^.ItemCaption := 'Confirmed';
      1: Data^.ItemCaption := 'Confirmed/Probable';
      2: Data^.ItemCaption := 'Probable';
      3: Data^.ItemCaption := 'Possible';
      4: Data^.ItemCaption := 'Observed';
    end;
    InitialStates := [ivsHasChildren];
  end
  else
  begin
    Data^.IsParent := False;
    Node^.CheckType := ctCheckBox;
    ParentData := Sender.GetNodeData(ParentNode);
    case ParentData^.ItemCaption of
      'Confirmed':
        case Node^.Index of
          0: Data^.ItemCaption := 'NY - Nest with Young';
          1: Data^.ItemCaption := 'NE - Nest with Eggs';
          2: Data^.ItemCaption := 'FS - carrying Fecal Sac';
          3: Data^.ItemCaption := 'FY - Feeding Young';
          4: Data^.ItemCaption := 'CF - Carrying Food';
          5: Data^.ItemCaption := 'FL - recently Fledged young';
          6: Data^.ItemCaption := 'ON - Occupied Nest';
          7: Data^.ItemCaption := 'UN - Used Nest';
          8: Data^.ItemCaption := 'DD - Distraction Display';
        end;
      'Confirmed/Probable':
        case Node^.Index of
          0: Data^.ItemCaption := 'NB - Nest Building';
          1: Data^.ItemCaption := 'CN - Carrying Nesting material';
        end;
      'Probable':
        case Node^.Index of
          0: Data^.ItemCaption := 'PE - Physiological Evidence';
          1: Data^.ItemCaption := 'B - wren/woodpecker nest Building';
          2: Data^.ItemCaption := 'A - Agitated behavior';
          3: Data^.ItemCaption := 'N - visiting probable Nest site';
          4: Data^.ItemCaption := 'C - Sourtship, display or copulation';
          5: Data^.ItemCaption := 'T - Territorial defense';
          6: Data^.ItemCaption := 'P - Pair in suitable habitat';
          7: Data^.ItemCaption := 'M - Multiple singing birds (7+)';
          8: Data^.ItemCaption := 'S7 - Singing bird present 7+ days';
        end;
      'Possible':
        case Node^.Index of
          0: Data^.ItemCaption := 'S - Singing bird';
          1: Data^.ItemCaption := 'H - in appropriate Habitat';
        end;
      'Observed':
        case Node^.Index of
          0: Data^.ItemCaption := 'F - Flyover';
        end;
    end;
  end;
end;

end.

