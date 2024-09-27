{ Xolmis Graphics library

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

unit cbs_graphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, ImgList, DB, EditBtn, DateUtils,
  StrUtils, LCLIntf, LCLType, cbs_datatypes;

  { Modify the graphic interface elements }
  procedure MakeRounded(Control: TWinControl);
  procedure RoundPanels(aParentControl: TWinControl);
  procedure PaintColorBand(aColor: String; aShape: TShape; aLabel: TLabel);
  procedure LoadGlyph(aImgLst: TCustomImageList; aIndex: Integer; aGlyph: TBitmap);
  procedure LoadIcone(aImgLst: TCustomImageList; aIndex: Integer; aIcon: TIcon);
  procedure LoadEditButton(aButtonedEdit: TEditButton; aIndex: Integer);
  //procedure UpdateHeaderCheck(aDataset: TDataset; aTabela: TTableType; aModifier: TRecordStatus;
  //  aWhere: TStrings; aColumn: TRxColumn);
  procedure TogglePassView(aEdit: TEditButton);

  function TryExifStringToDateTime(const S: string; var aDateTime: TDateTime): Boolean;

  // Color treatment
  function GenerateRandomColor(const Mix: TColor = clWhite): TColor;

implementation

uses
  cbs_themes, cbs_birds, cbs_count;

procedure MakeRounded(Control: TWinControl);
var
  R: TRect;
  Rgn: HRGN;
begin
  R := Control.ClientRect;
  Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 20, 20);
  SetWindowRgn(Control.Handle, Rgn, True);
  DeleteObject(Rgn);
end;

procedure RoundPanels(aParentControl: TWinControl);
var
  i: Integer;
begin
  for i := 0 to (aParentControl.ComponentCount - 1) do
    if (aParentControl.Components[i] is TPanel) then
      //if (aParentControl.Components[i] as TPanel).Visible then
        MakeRounded(aParentControl.Components[i] as TPanel);
end;

procedure PaintColorBand(aColor: String; aShape: TShape; aLabel: TLabel);
var
  Cores: array of String;
  cBrush: TColor;
begin
  if aColor = '' then
  begin
    aLabel.Caption := '';
    aShape.Brush.Style := bsClear;
    aShape.Pen.Style := psDot;
    aShape.Pen.Color := clSilver;
    Exit;
  end;

  aLabel.Caption := aColor;
  aLabel.Font.Color := clTextPrimaryLight;

  for Cores in BandColors do
  begin
    if AnsiString(Cores[0]).Equals(aColor) then
    begin
      cBrush := StringToColor(Cores[1]);
      aShape.Brush.Color := cBrush;
      aShape.Pen.Style := psSolid;
      if MatchStr(aColor, ['A', 'V', 'B', 'G', 'U', 'S', 'N']) then
        aLabel.Font.Color := clTextOnAccentPrimaryLight;

      Break;
    end;
  end;
end;

procedure LoadGlyph(aImgLst: TCustomImageList; aIndex: Integer; aGlyph: TBitmap);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  aImgLst.GetBitmap(aIndex, bmp);
  aGlyph.Assign(bmp);
  bmp.Free;
end;

procedure LoadIcone(aImgLst: TCustomImageList; aIndex: Integer; aIcon: TIcon);
var
  ico: TIcon;
begin
  ico := TIcon.Create;
  aImgLst.GetIcon(aIndex, ico);
  aIcon.Assign(ico);
  ico.Free;
end;

procedure LoadEditButton(aButtonedEdit: TEditButton; aIndex: Integer);
begin
  aButtonedEdit.ImageIndex := aIndex;
end;

//procedure UpdateHeaderCheck(aDataset: TDataset; aTabela: TTableType; aModifier: TRecordStatus;
//  aWhere: TStrings; aColumn: TRxColumn);
//var
//  nMark: Integer;
//begin
//  { DataSet is empty }
//  if (aDataset.RecordCount = 0) then
//  begin
//    aColumn.Title.ImageIndex := 6;
//    //aColumn.Title.Hint := rs_HintHeaderAllUnmarked;
//    Exit;
//  end;
//
//  nMark := MarkedCount(aTabela, aModifier, aWhere);
//
//  { All unmarked }
//  if nMark = 0 then
//  begin
//    aColumn.Title.ImageIndex := 6;
//    //aColumn.Title.Hint := rs_HintHeaderAllUnmarked;
//  end else
//  { Some marked }
//  if (nMark > 0) and (nMark < aDataset.RecordCount) then
//  begin
//    aColumn.Title.ImageIndex := 7;
//    //aColumn.Title.Hint := Format(rs_HintHeaderSomeMarked, [nMark, aDataSet.RecordCount]);
//  end else
//  { All marked }
//  if nMark = aDataset.RecordCount then
//  begin
//    aColumn.Title.ImageIndex := 8;
//    //aColumn.Title.Hint := Format(rs_HintHeaderAllMarked, [nMark]);
//  end;
//end;

procedure TogglePassView(aEdit: TEditButton);
begin
  if aEdit.EchoMode = emNormal then
  begin
    { oculta senha }
    //aEdit.PasswordChar := #$25CF;
    aEdit.EchoMode := emPassword;
    aEdit.ImageIndex := 0;
  end
  else
  begin
    { mostra senha }
    //aEdit.PasswordChar := #0;
    aEdit.EchoMode := emNormal;
    aEdit.ImageIndex := 1;
  end;
end;

function TryExifStringToDateTime(const S: string; var aDateTime: TDateTime): Boolean;
var
  Ano, Mes, Dia, Hora, Minuto, Segundo: Integer;
begin // '2007:09:02 02:30:49'
  Result := (Length(S) >= 19) and (S[5] = ':') and (S[8] = ':') and TryStrToInt(Copy(S, 1, 4), Ano)
    and TryStrToInt(Copy(S, 6, 2), Mes) and TryStrToInt(Copy(S, 9, 2), Dia) and
    TryStrToInt(Copy(S, 12, 2), Hora) and TryStrToInt(Copy(S, 15, 2), Minuto) and
    TryStrToInt(Copy(S, 18, 2), Segundo) and TryEncodeDateTime(Ano, Mes, Dia, Hora, Minuto, Segundo,
    0, aDateTime);
end;

function GenerateRandomColor(const Mix: TColor): TColor;
var
  Red, Green, Blue: Integer;
begin
  Red := Random(256);
  Green := Random(256);
  Blue := Random(256);

  Red := (Red + GetRValue(ColorToRGB(Mix))) div 2;
  Green := (Green + GetGValue(ColorToRGB(Mix))) div 2;
  Blue := (Blue + GetBValue(ColorToRGB(Mix))) div 2;
  Result := RGB(Red, Green, Blue);
end;

end.

