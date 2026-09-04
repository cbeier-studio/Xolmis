unit dbimagegallery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, ExtCtrls, DB, DBCtrls, Graphics, LCLType;

type
  { TGalleryDataLink }
  TGalleryDataLink = class(TDataLink)
  private
    FGallery: TObject;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create(AGallery: TObject);
  end;

  { TDBImageGallery }
  TDBImageGallery = class(TScrollBox)
  private
    FDataLink: TGalleryDataLink;
    FImageField: string;
    FCaptionField: string;
    FZoom: Integer;
    FSelectedIndex: Integer;
    FOnChange: TNotifyEvent;

    function GetDataSource: TDataSource;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetImageField(AValue: string);
    procedure SetCaptionField(AValue: string);
    procedure SetZoom(AValue: Integer);
    procedure DataChanged;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshGallery;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ImageField: string read FImageField write SetImageField;
    property CaptionField: string read FCaptionField write SetCaptionField;
    property Zoom: Integer read FZoom write SetZoom default 100;
    property SelectedIndex: Integer read FSelectedIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CBS', [TDBImageGallery]);
end;

{ TGalleryDataLink }

constructor TGalleryDataLink.Create(AGallery: TObject);
begin
  inherited Create;
  FGallery := AGallery;
  VisualControl := True;
end;

procedure TGalleryDataLink.ActiveChanged;
begin
  TDBImageGallery(FGallery).DataChanged;
end;

procedure TGalleryDataLink.DataSetChanged;
begin
  TDBImageGallery(FGallery).DataChanged;
end;

{ TDBImageGallery }

constructor TDBImageGallery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TGalleryDataLink.Create(Self);
  FZoom := 100;
  FSelectedIndex := -1;
  Width := 300;
  Height := 200;
  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;
end;

destructor TDBImageGallery.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

function TDBImageGallery.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBImageGallery.SetDataSource(AValue: TDataSource);
begin
  if FDataLink.DataSource <> AValue then
  begin
    FDataLink.DataSource := AValue;
    Invalidate;
  end;
end;

procedure TDBImageGallery.SetImageField(AValue: string);
begin
  if FImageField <> AValue then
  begin
    FImageField := AValue;
    Invalidate;
  end;
end;

procedure TDBImageGallery.SetCaptionField(AValue: string);
begin
  if FCaptionField <> AValue then
  begin
    FCaptionField := AValue;
    Invalidate;
  end;
end;

procedure TDBImageGallery.SetZoom(AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue > 0) and (AValue <= 200) then
  begin
    FZoom := AValue;
    Invalidate;
  end;
end;

procedure TDBImageGallery.DataChanged;
begin
  Invalidate;
end;

procedure TDBImageGallery.RefreshGallery;
begin
  Invalidate;
end;

procedure TDBImageGallery.Paint;
var
  Picture: TPicture;
  BlobStream: TStream;
  sCaption: string;
  ImgField, CapField: TField;
  X, Y, W, H, ItemIndex: Integer;
  Bookmark: TBookmark;
  DataSet: TDataSet;
begin
  inherited Paint;

  if not FDataLink.Active or (FImageField = '') then
    Exit;

  DataSet := FDataLink.DataSet;
  if not Assigned(DataSet) or DataSet.IsEmpty then
    Exit;

  { Desabilita controles do dataset para não disparar eventos de interface durante a varredura }
  DataSet.DisableControls;
  Bookmark := DataSet.Bookmark;
  Picture := TPicture.Create;
  try
    X := 10 - HorzScrollBar.Position;
    Y := 10 - VertScrollBar.Position;
    ItemIndex := 0;

    DataSet.First;
    while not DataSet.EOF do
    begin
      ImgField := DataSet.FindField(FImageField);

      { Define um tamanho base padrão ajustado pelo Zoom }
      W := (120 * FZoom) div 100;
      H := (120 * FZoom) div 100;

      { Carrega a Imagem caso exista no campo BLOB }
      if (ImgField is TBlobField) and not ImgField.IsNull then
      begin
        BlobStream := DataSet.CreateBlobStream(ImgField, bmRead);
        try
          try
            Picture.LoadFromStream(BlobStream);
            { Desenha mantendo o aspecto ou esticando na área pré-definida }
            Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Picture.Graphic);
          except
            { Caso o formato do Blob não seja reconhecido }
            Canvas.TextOut(X + 5, Y + (H div 2), '[Erro Imagem]');
          end;
        finally
          BlobStream.Free;
        end;
      end
      else
      begin
        { Quadro vazio se o campo for nulo }
        Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(Rect(X, Y, X + W, Y + H));
      end;

      { Desenha o Moldura do Item Selecionado }
      if ItemIndex = FSelectedIndex then
      begin
        Canvas.Pen.Color := clHighlight;
        Canvas.Pen.Width := 3;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(X - 2, Y - 2, X + W + 2, Y + H + 2);
        Canvas.Brush.Style := bsSolid;
      end;

      { Desenha a Legenda }
      if FCaptionField <> '' then
      begin
        CapField := DataSet.FindField(FCaptionField);
        if Assigned(CapField) then
        begin
          sCaption := CapField.AsString;
          Canvas.TextOut(X, Y + H + 4, sCaption);
        end;
      end;

      { Incrementa Posições na Grid }
      Inc(X, W + 15);
      if X + W > ClientWidth then
      begin
        X := 10 - HorzScrollBar.Position;
        Inc(Y, H + 35);
      end;

      Inc(ItemIndex);
      DataSet.Next;
    end;
  finally
    { Restaura o estado e posição original do DataSet }
    if DataSet.BookmarkValid(Bookmark) then
      DataSet.Bookmark := Bookmark;
    DataSet.EnableControls;
    Picture.Free;
  end;
end;

procedure TDBImageGallery.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurX, CurY, W, H, ItemIndex: Integer;
  DataSet: TDataSet;
  ClickFound: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button <> mbLeft) or not FDataLink.Active then
    Exit;

  DataSet := FDataLink.DataSet;
  if not Assigned(DataSet) or DataSet.IsEmpty then
    Exit;

  CurX := 10 - HorzScrollBar.Position;
  CurY := 10 - VertScrollBar.Position;
  ItemIndex := 0;
  ClickFound := False;

  W := (120 * FZoom) div 100;
  H := (120 * FZoom) div 100;

  DataSet.DisableControls;
  try
    DataSet.First;
    while not DataSet.EOF do
    begin
      { Verifica se o clique do mouse ocorreu dentro do retângulo da imagem }
      if (X >= CurX) and (X <= CurX + W) and (Y >= CurY) and (Y <= CurY + H) then
      begin
        FSelectedIndex := ItemIndex;
        ClickFound := True;
        Break;
      end;

      Inc(CurX, W + 15);
      if CurX + W > ClientWidth then
      begin
        CurX := 10 - HorzScrollBar.Position;
        Inc(CurY, H + 35);
      end;

      Inc(ItemIndex);
      DataSet.Next;
    end;
  finally
    DataSet.EnableControls;
  end;

  if ClickFound then
  begin
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

end.

