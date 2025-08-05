{ Xolmis GeoAssist dialog

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

unit udlg_geoassist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, MaskEdit, Spin, DBGrids,
  ComCtrls, mvMapViewer, mvTypes, mvGpsObj, mvDE_BGRA, atshapelinebgra, BCPanel, SpinEx, Types,
  utils_system, utils_gis, DB, mvDrawingEngine, mvPluginCommon, mvMapScalePlugin, mvPlugins;

type

  { TdlgGeoAssist }

  TdlgGeoAssist = class(TForm)
    cbLatHem: TComboBox;
    cbLongHem: TComboBox;
    dsGeoBank: TDataSource;
    eLatDeg: TSpinEdit;
    eLatMin: TSpinEdit;
    eLatSec: TFloatSpinEdit;
    eLongDeg: TSpinEdit;
    eLongMin: TSpinEdit;
    eLongSec: TFloatSpinEdit;
    gridBank: TDBGrid;
    lblLatitude: TLabel;
    lblLongitude: TLabel;
    mapGeo: TMapView;
    MvBGRADraw: TMvBGRADrawingEngine;
    MvPluginManager: TMvPluginManager;
    MvPluginManagerLegalNoticePlugin1: TLegalNoticePlugin;
    MvPluginManagerMapScalePlugin1: TMapScalePlugin;
    PG: TPageControl;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pContent: TBCPanel;
    pDecimal: TBCPanel;
    pLat: TBCPanel;
    pLong: TBCPanel;
    sbCancel: TButton;
    sbOK: TButton;
    tabConvert: TTabSheet;
    tabImported: TTabSheet;
    procedure eLongDegEditingDone(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure gridBankCellClick(Column: TColumn);
    procedure longSecKeyPress(Sender: TObject; var Key: char);
    procedure mapGeoDrawGpsPoint(Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure PGChange(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbOKClick(Sender: TObject);
  private
    xAxis: TMapAxis;
    xLinha: String;
    xDMS: TDMS;
    xDec: Extended;
    FChangeZoom: Boolean;
    FDecPoint: TMapPoint;
    FDmsPoint: TDMSPoint;
    FPointStr: String;
    procedure ApplyDarkMode;
    procedure ConvertCoordinate;
    procedure RefreshMap;
    procedure UpdateButtons;
  public
    procedure SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);

    property DecimalPoint: TMapPoint read FDecPoint write FDecPoint;
    property DMSPoint: TDMSPoint read FDmsPoint write FDmsPoint;
    property PointStr: String read FPointStr write FPointStr;

    property Axis: TMapAxis read xAxis write xAxis;
    property Linha: String read xLinha write xLinha;
    property CoordDec: Extended read xDec write xDec;
    property CoordDMS: TDMS read xDMS write xDMS;
  end;

var
  dlgGeoAssist: TdlgGeoAssist;

implementation

uses
  utils_global, utils_themes, udm_main, uDarkStyleParams;

{$R *.lfm}

{ TdlgGeoAssist }

procedure TdlgGeoAssist.longSecKeyPress(Sender: TObject; var Key: char);
var
  s, d: String;
  // f: Extended;
begin
  FormKeyPress(Sender, Key);
  if (CharInSet(Key, ['0' .. '9', '.', ',', #8, #13, #27])) then
  begin
    if ((Key = ',') or (Key = '.')) then
    begin
      s := TFloatSpinEditEx(Sender).Text;
      d := DefaultFormatSettings.DecimalSeparator;
      if (Pos(d, s) = 0) then
        Key := DefaultFormatSettings.DecimalSeparator
      else
        Key := #0;
    end;
  end
  else
    Key := #0;
end;

procedure TdlgGeoAssist.mapGeoDrawGpsPoint(Sender: TObject; ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
const
  R = 8;
var
  P: TPoint;
  ext: TSize;
  //img: TBitmap;
begin
  // Screen coordinates of the GPS point
  P := TMapView(Sender).LonLatToScreen(APoint.RealPoint);

  // Draw the GPS point with MapMarker bitmap
  //img := TBitmap.Create;
  //try
  //  img.TransparentColor := clBlack;
  //  img.Transparent := True;
  //  img.Width := 16;
  //  img.Height := 16;
  //  DMM.iMaps.DrawForPPI(img.Canvas, 0, 0, APoint.IdOwner, 16, 96, 1);
  //  ADrawer.DrawBitmap(P.X - mapGeo.POIImagesWidth div 2, P.Y - mapGeo.POIImagesWidth, img, True);
  //finally
  //  img.Free;
  //end;
  //end
  //else
  //begin
    // Draw the GPS point as a circle
    if APoint.IdOwner = 1 then
    begin
      ADrawer.BrushColor := clRedFGDark;
      ADrawer.PenColor := clRedBGLight;
    end
    else
    begin
      ADrawer.BrushColor := clYellowFG4Dark;
      ADrawer.PenColor := clYellowBGLight;
    end;
    ADrawer.BrushStyle := bsSolid;
    ADrawer.PenWidth := 2;
    ADrawer.Ellipse(P.X - R, P.Y - R, P.X + R, P.Y + R);
    P.Y := P.Y + R;
  //end;

  // Draw the caption of the GPS point
  ext := ADrawer.TextExtent(APoint.Name);
  ADrawer.BrushColor := clWhite;
  ADrawer.BrushStyle := bsClear;
  ADrawer.TextOut(P.X - ext.CX div 2, P.Y + 5, APoint.Name);
end;

procedure TdlgGeoAssist.PGChange(Sender: TObject);
begin
  FChangeZoom := True;
  RefreshMap;

  UpdateButtons;
end;

procedure TdlgGeoAssist.RefreshMap;
var
  poi: TGpsPoint;
  rp, rpsel: TRealPoint;
  BM: TBookmark;
begin
  mapGeo.GPSItems.Clear(0);
  mapGeo.GPSItems.Clear(1);
  mapGeo.Refresh;

  if PG.ActivePage = tabConvert then
  begin
    ConvertCoordinate;

    if not (FDecPoint.X = 0) and not (FDecPoint.Y = 0) then
    begin
      //DMM.iMaps.GetBitmap(1, mapGeo.POIImage);
      rpsel.Lon := FDecPoint.X;
      rpsel.Lat := FDecPoint.Y;
      poi := TGpsPoint.CreateFrom(rpsel);
      //poi.Name := FieldByName('coordinate_name').AsString;
      mapGeo.GPSItems.Add(poi, 1);
    end;
  end
  else
  if PG.ActivePage = tabImported then
  begin
    with dsGeoBank.DataSet do
    begin
      if not dsGeoBank.DataSet.Active then
        Open;

      if RecordCount > 0 then
      try
        BM := Bookmark;
        DisableControls;
        //DMM.iMaps.GetBitmap(1, mapGeo.POIImage);
        rpsel.Lon := FieldByName('longitude').AsFloat;
        rpsel.Lat := FieldByName('latitude').AsFloat;
        if not (rpsel.Lon = 0) and not (rpsel.Lat = 0) then
        begin
          poi := TGpsPoint.CreateFrom(rpsel);
          poi.Name := FieldByName('coordinate_name').AsString;
            //+ #10 + FieldByName('locality_name').AsString + #10 +
            //FieldByName('method_name').AsString;
          mapGeo.GPSItems.Add(poi, 1, 999);
          if not FChangeZoom then
            mapGeo.Center := rpsel;
        end;
        First;
        //DMM.iMaps.GetBitmap(0, mapGeo.POIImage);
        repeat
          rp.Lon := FieldByName('longitude').AsFloat;
          rp.Lat := FieldByName('latitude').AsFloat;
          if (not (rp.Lon = 0) and not (rp.Lat = 0)) and
            (not (rp.Lon = rpsel.Lon) and not (rp.Lat = rpsel.Lat)) then
          begin
            poi := TGpsPoint.CreateFrom(rp);
            //poi.Name := FieldByName('coordinate_name').AsString;
              //+ #10 + FieldByName('locality_name').AsString + #10 +
              //FieldByName('method_name').AsString;
            mapGeo.GPSItems.Add(poi, 0);
          end;
          Next;
        until EOF;
      finally
        EnableControls;
        Bookmark := BM;
      end;
    end;
  end;

  if FChangeZoom and (mapGeo.GPSItems.Count > 0) then
  begin
    mapGeo.ZoomOnArea(mapGeo.GPSItems.BoundingBox);
    if mapGeo.Zoom > 14 then
      mapGeo.Zoom := 14
    else
      mapGeo.Zoom := mapGeo.Zoom - 1;
  end;
end;

procedure TdlgGeoAssist.sbCancelClick(Sender: TObject);
begin
  GravaStat(Name, 'sbCancel', 'click');
  // Cancelar
  ModalResult := mrCancel;
end;

procedure TdlgGeoAssist.sbOKClick(Sender: TObject);
begin
  GravaStat(Name, 'sbOK', 'click');

  if PG.ActivePage = tabImported then
  begin
    FDecPoint.X := dsGeoBank.DataSet.FieldByName('longitude').AsFloat;
    FDecPoint.Y := dsGeoBank.DataSet.FieldByName('latitude').AsFloat;
  end;
  FPointStr := FDecPoint.ToString;

  ModalResult := mrOK;
end;

procedure TdlgGeoAssist.SetDialogPosition(X, Y: Integer; ControlWidth, ControlHeight: Integer);
begin
  if ControlWidth > Self.Width then
    Self.Width := ControlWidth;

  if (X + Self.Width) > Screen.WorkAreaWidth then
    Self.Left := X - Self.Width
  else
    Self.Left := X;

  if (Y + ControlHeight + Self.Height) > (Screen.WorkAreaHeight) then
    Self.Top := Y - Self.Height
  else
    Self.Top := Y + ControlHeight;
end;

procedure TdlgGeoAssist.UpdateButtons;
begin
  sbOK.Enabled := (FDecPoint.X <> 0) and (FDecPoint.Y <> 0);
end;

procedure TdlgGeoAssist.ApplyDarkMode;
begin
  pLong.Background.Color := clCardBGDefaultDark;
  pLong.Border.Color := clSystemSolidNeutralFGDark;
  pLat.Background.Color := clCardBGDefaultDark;
  pLat.Border.Color := clSystemSolidNeutralFGDark;
  pDecimal.Background.Color := clCardBGDefaultDark;
  pDecimal.Border.Color := clSystemSolidNeutralFGDark;
end;

procedure TdlgGeoAssist.ConvertCoordinate;
begin
  FDmsPoint.X.Degrees := elongDeg.Value;
  FDmsPoint.X.Minutes := elongMin.Value;
  FDmsPoint.X.Seconds := elongSec.Value;
  if cblongHem.ItemIndex >= 0 then
    FDmsPoint.X.Hemisphere := cblongHem.Text[1]
  else
    FDmsPoint.X.Hemisphere := 'W';

  FDmsPoint.Y.Degrees := elatDeg.Value;
  FDmsPoint.Y.Minutes := elatMin.Value;
  FDmsPoint.Y.Seconds := elatSec.Value;
  if cblatHem.ItemIndex >= 0 then
    FDmsPoint.Y.Hemisphere := cblatHem.Text[1]
  else
    FDmsPoint.Y.Hemisphere := 'S';

  FDecPoint := DmsToDecimal(FDmsPoint);
  pDecimal.Caption := FDecPoint.ToString;

  FChangeZoom := True;
end;

procedure TdlgGeoAssist.eLongDegEditingDone(Sender: TObject);
begin
  ConvertCoordinate;

  RefreshMap;

  UpdateButtons;
end;

procedure TdlgGeoAssist.FormKeyPress(Sender: TObject; var Key: char);
begin
  { FECHAR = Esc }
  if (Key = #27) then
  begin
    GravaStat(Name, '', 'Esc');
    {$IFDEF DEBUG}
    LogDebug('HOTKEY: Esc');
    {$ENDIF}
    Key := #0;
    ModalResult := mrCancel;
  end;
  { PROXIMO CAMPO = Enter }
  if (Key = #13) and (xSettings.UseEnterAsTab) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TdlgGeoAssist.FormShow(Sender: TObject);
begin
  //{$IFDEF MSWINDOWS}
  //SetRoundedCorners(Self.Handle, rcSmall);
  //{$ENDIF}

  if IsDarkModeEnabled then
    ApplyDarkMode;

  if Length(FPointStr) > 0 then
    FDecPoint.FromString(FPointStr);

  if not (FDecPoint.X = 0) and not (FDecPoint.Y = 0) then
  begin
    FDmsPoint := DecimalToDms(FDecPoint);

    elongDeg.Value := FDMSPoint.X.Degrees;
    elongMin.Value := FDMSPoint.X.Minutes;
    elongSec.Value := FDMSPoint.X.Seconds;
    cblongHem.ItemIndex := cblongHem.Items.IndexOf(FDMSPoint.X.Hemisphere);

    elatDeg.Value := FDMSPoint.Y.Degrees;
    elatMin.Value := FDMSPoint.Y.Minutes;
    elatSec.Value := FDMSPoint.Y.Seconds;
    cblatHem.ItemIndex := cblatHem.Items.IndexOf(FDMSPoint.Y.Hemisphere);
  end;

  if cblongHem.ItemIndex < 0 then
    cblongHem.ItemIndex := 1;
  if cblatHem.ItemIndex < 0 then
    cblatHem.ItemIndex := 1;

  //FDecPoint := DmsToDecimal(FDmsPoint);
  pDecimal.Caption := FDecPoint.ToString;

  if not dsGeoBank.DataSet.Active then
    dsGeoBank.DataSet.Open;
  dsGeoBank.DataSet.First;

  mapGeo.CachePath := IncludeTrailingPathDelimiter(ConcatPaths([AppDataDir, 'map-cache']));
  mapGeo.Active := True;
  FChangeZoom := True;
  RefreshMap;

  UpdateButtons;
end;

procedure TdlgGeoAssist.gridBankCellClick(Column: TColumn);
begin
  FDecPoint.X := dsGeoBank.DataSet.FieldByName('longitude').AsFloat;
  FDecPoint.Y := dsGeoBank.DataSet.FieldByName('latitude').AsFloat;

  FChangeZoom := False;
  RefreshMap;

  UpdateButtons;
end;

end.

