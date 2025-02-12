{ Xolmis How Was Aged/Sexed Editor dialog

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

unit ulst_howsexedaged;

{$mode objfpc}{$H+}

interface

uses
  Classes, laz.VirtualTrees, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, CheckLst, Buttons, atshapelinebgra, BCPanel, Types, StdCtrls;

type

  { TlstHowSexedAged }

  TlstHowSexedAged = class(TForm)
    vtAged: TLazVirtualStringTree;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pCode: TBCPanel;
    sbCancel: TBitBtn;
    sbOK: TBitBtn;
    procedure cklAgedClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
    procedure vtAgedChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtAgedGetText
      (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vtAgedInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure vtAgedInitNode
      (Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FHowAged: String;
    //function HowAgedCode: String;
    procedure UpdateHowAgedCode;
    procedure UpdateCheckedByCode;
  public
    property HowAged: String read FHowAged write FHowAged;
  end;

var
  lstHowSexedAged: TlstHowSexedAged;

implementation

uses cbs_locale, cbs_global, cbs_dialogs;

{$R *.lfm}

type
  PNodeData = ^TNodeData;
  TNodeData = record
    ItemCaption: String;
    IsParent: Boolean;
  end;

{ TlstHowSexedAged }

procedure TlstHowSexedAged.cklAgedClickCheck(Sender: TObject);
begin
  //GravaStat(Name, 'cklAged', 'clickcheck');
  //pCode.Caption := HowAgedCode;
end;

procedure TlstHowSexedAged.FormCreate(Sender: TObject);
begin
  HowAged := '';

  vtAged.NodeDataSize := SizeOf(TNodeData);
  vtAged.RootNodeCount := 4;

  //cklAged.Header[0] := True;
  //cklAged.Header[11] := True;
  //cklAged.Header[19] := True;
  //cklAged.Header[25] := True;
end;

procedure TlstHowSexedAged.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TlstHowSexedAged.FormShow(Sender: TObject);
var
  C: Char;
  i: Integer;
begin
  //cklAged.Items.CommaText := rsHowAgedSexedList;

  if Length(FHowAged) > 0 then
  begin
    //for i := 1 to Length(FHowAged) do
    //begin
    //  C := FHowAged[i];
    //  case C of
    //    'B': cklAged.Checked[1] := True;
    //    'C': cklAged.Checked[2] := True;
    //    '@': cklAged.Checked[3] := True;
    //    'E': cklAged.Checked[4] := True;
    //    'I': cklAged.Checked[5] := True;
    //    'G': cklAged.Checked[6] := True;
    //    '$': cklAged.Checked[7] := True;
    //    'S': cklAged.Checked[8] := True;
    //    'Q': cklAged.Checked[9] := True;
    //    'Y': cklAged.Checked[10] := True;
    //    'K': cklAged.Checked[12] := True;
    //    'A': cklAged.Checked[13] := True;
    //    'F': cklAged.Checked[14] := True;
    //    'J': cklAged.Checked[15] := True;
    //    'M': cklAged.Checked[16] := True;
    //    'P': cklAged.Checked[17] := True;
    //    'L': cklAged.Checked[18] := True;
    //    'W': cklAged.Checked[20] := True;
    //    'V': cklAged.Checked[21] := True;
    //    'R': cklAged.Checked[22] := True;
    //    '=': cklAged.Checked[23] := True;
    //    '#': cklAged.Checked[24] := True;
    //    'O': cklAged.Checked[26] := True;
    //    'U': cklAged.Checked[27] := True;
    //    'X': cklAged.Checked[28] := True;
    //    'Z': cklAged.Checked[29] := True;
    //  end;
    //end;
    pCode.Caption := FHowAged;
    UpdateCheckedByCode;
  end;

  vtAged.FullExpand();
end;

procedure TlstHowSexedAged.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'SBCancel', 'click');
  ModalResult := mrCancel;
end;

procedure TlstHowSexedAged.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'SBSave', 'click');
  if FHowAged = '' then
  begin
    MsgDlg('', rsListCheckedNone, mtError);
    Exit;
  end;

  //HowAged := HowAgedCode;

  ModalResult := mrOK;
end;

procedure TlstHowSexedAged.UpdateCheckedByCode;
var
  Node: PVirtualNode;
  Data: PNodeData;
  i: Integer;
  TempStr: String;
begin
  TempStr := FHowAged;
  Node := vtAged.GetFirst;
  while Assigned(Node) do
  begin
    Data := vtAged.GetNodeData(Node);
    if not Data^.IsParent then
    begin
      vtAged.CheckState[Node] := csUncheckedNormal;
      for i := 1 to Length(TempStr) do
      begin
        if Data^.ItemCaption[1] = TempStr[i] then
        begin
          vtAged.CheckState[Node] := csCheckedNormal;
          //Break;
        end;
      end;
    end;
    Node := vtAged.GetNext(Node);
  end;
end;

procedure TlstHowSexedAged.UpdateHowAgedCode;
var
  Node: PVirtualNode;
  Data: PNodeData;
  CheckedString: String;
begin
  CheckedString := '';
  Node := vtAged.GetFirst;
  while Assigned(Node) do
  begin
    Data := vtAged.GetNodeData(Node);
    if not Data^.IsParent and (Node^.CheckState = csCheckedNormal) then
    begin
      CheckedString := CheckedString + Data^.ItemCaption[1];
    end;
    Node := vtAged.GetNext(Node);
  end;
  FHowAged := CheckedString;
  pCode.Caption := FHowAged;
end;

procedure TlstHowSexedAged.vtAgedChecked
  (Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateHowAgedCode;
end;

procedure TlstHowSexedAged.vtAgedGetText
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PNodeData;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data^.ItemCaption;
end;

procedure TlstHowSexedAged.vtAgedInitChildren
  (Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
begin
  case Node^.Index of
    0: ChildCount := 10; // Physical differences
    1: ChildCount := 7;  // Plumage characters
    2: ChildCount := 5;  // Feather characters
    3: ChildCount := 4;  // Undetermined or remaining
  end;
end;

procedure TlstHowSexedAged.vtAgedInitNode
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
      0: Data^.ItemCaption := 'Physical differences';
      1: Data^.ItemCaption := 'Plumage characters';
      2: Data^.ItemCaption := 'Feather characters';
      3: Data^.ItemCaption := 'Undetermined or remaining';
    end;
    InitialStates := [ivsHasChildren];
  end
  else
  begin
    Data^.IsParent := False;
    Node^.CheckType := ctCheckBox;
    ParentData := Sender.GetNodeData(ParentNode);
    case ParentData^.ItemCaption of
      'Physical differences':
        case Node^.Index of
          0: Data^.ItemCaption := 'B - Brood patch';
          1: Data^.ItemCaption := 'C - Cloacal protuberance';
          2: Data^.ItemCaption := '@ - egg in oviduct';
          3: Data^.ItemCaption := 'E - Eye color';
          4: Data^.ItemCaption := 'I - mouth/bill color or striations on bill (in hummingbirds)';
          5: Data^.ItemCaption := 'G - Gape';
          6: Data^.ItemCaption := '$ - feet or legs';
          7: Data^.ItemCaption := 'S - Skull ossification';
          8: Data^.ItemCaption := 'Q - measurements (details in notes)';
          9: Data^.ItemCaption := 'Y - symmetrical flight feather molt';
        end;
      'Plumage characters':
        case Node^.Index of
          0: Data^.ItemCaption := 'K - definitive basic plumage';
          1: Data^.ItemCaption := 'A - definitve Alternate plumage';
          2: Data^.ItemCaption := 'F - Formative plumage (applies to first alternate plumage as well)';
          3: Data^.ItemCaption := 'J - Juvenal plumage';
          4: Data^.ItemCaption := 'M - Molt limits';
          5: Data^.ItemCaption := 'P - Plumage (only for sexual dimorphism)';
          6: Data^.ItemCaption := 'L - plumage color patch Length or extent (details in notes)';
        end;
      'Feather characters':
        case Node^.Index of
          0: Data^.ItemCaption := 'W - feather Wear';
          1: Data^.ItemCaption := 'V - feather shape';
          2: Data^.ItemCaption := 'R - Prejuvenal (first prebasic) molt';
          3: Data^.ItemCaption := '= - fault bar alignment';
          4: Data^.ItemCaption := '# - growth bar alignment';
        end;
      'Undetermined or remaining':
        case Node^.Index of
          0: Data^.ItemCaption := 'O - Other (such as behavior/copulation; put in notes)';
          1: Data^.ItemCaption := 'U - Undetermined after examination';
          2: Data^.ItemCaption := 'X - age or sex determination not attempted';
          3: Data^.ItemCaption := 'Z - less precise age (< 95%) but high certainty';
        end;
    end;
  end;
end;

//function TlstHowSexedAged.HowAgedCode: String;
//var
//  L: String;
//  i: Integer;
//begin
//  Result := '';
//
//  L := '';
//  for i := 0 to cklAged.Count - 1 do
//    if cklAged.Checked[i] then
//      case i of
//        0:  ;
//        1:  L := L + 'B';
//        2:  L := L + 'C';
//        3:  L := L + '@';
//        4:  L := L + 'E';
//        5:  L := L + 'I';
//        6:  L := L + 'G';
//        7:  L := L + '$';
//        8:  L := L + 'S';
//        9:  L := L + 'Q';
//        10: L := L + 'Y';
//        11: ;
//        12: L := L + 'K';
//        13: L := L + 'A';
//        14: L := L + 'F';
//        15: L := L + 'J';
//        16: L := L + 'M';
//        17: L := L + 'P';
//        18: L := L + 'L';
//        19: ;
//        20: L := L + 'W';
//        21: L := L + 'V';
//        22: L := L + 'R';
//        23: L := L + '=';
//        24: L := L + '#';
//        25: ;
//        26: L := L + 'O';
//        27: L := L + 'U';
//        28: L := L + 'X';
//        29: L := L + 'Z';
//      end;
//
//  Result := L;
//end;

end.

