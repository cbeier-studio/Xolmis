unit dbimagegallery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, ExtCtrls, DB, DBCtrls, Graphics;

type
  TDBImageGallery = class(TScrollBox)
  private
    FDataSource: TDataSource;
    FImageField: string;
    FCaptionField: string;
    FZoom: Integer;
    FSelectedIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetImageField(AValue: string);
    procedure SetCaptionField(AValue: string);
    procedure SetZoom(AValue: Integer);
    procedure DataChange(Sender: TObject; Field: TField);
    procedure DrawGallery;
  protected
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property ImageField: string read FImageField write SetImageField;
    property CaptionField: string read FCaptionField write SetCaptionField;
    property Zoom: Integer read FZoom write SetZoom default 100;
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

{ TDBImageGallery }

constructor TDBImageGallery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZoom := 100;
  FSelectedIndex := -1;
  Width := 200;
  Height := 200;
end;

destructor TDBImageGallery.Destroy;
begin
  if Assigned(FDataSource) then
    FDataSource.OnDataChange := nil;
  inherited Destroy;
end;

procedure TDBImageGallery.SetDataSource(AValue: TDataSource);
begin
  if FDataSource <> AValue then
  begin
    FDataSource := AValue;
    if Assigned(FDataSource) then
      FDataSource.OnDataChange := @DataChange;
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
  if (FZoom <> AValue) and (AValue <= 100) then
  begin
    FZoom := AValue;
    Invalidate;
  end;
end;

procedure TDBImageGallery.DataChange(Sender: TObject; Field: TField);
begin
  Invalidate;
end;

procedure TDBImageGallery.DrawGallery;
var
  Bitmap: TBitmap;
  BlobStream: TStream;
  sCaption: string;
  Field: TField;
  X, Y, W, H: Integer;
begin
  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then
    Exit;

  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);

  FDataSource.DataSet.First;
  X := 10;
  Y := 10;
  while not FDataSource.DataSet.EOF do
  begin
    Field := FDataSource.DataSet.FieldByName(FImageField);
    if Field is TBlobField then
    begin
      BlobStream := FDataSource.DataSet.CreateBlobStream(TBlobField(Field), bmRead);
      try
        Bitmap := TBitmap.Create;
        try
          Bitmap.LoadFromStream(BlobStream);
          W := (Bitmap.Width * FZoom) div 100;
          H := (Bitmap.Height * FZoom) div 100;
          Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Bitmap);
        finally
          Bitmap.Free;
        end;
      finally
        BlobStream.Free;
      end;
    end;

    if FCaptionField <> '' then
    begin
      sCaption := FDataSource.DataSet.FieldByName(FCaptionField).AsString;
      Canvas.TextOut(X, Y + H + 5, sCaption);
    end;

    if FSelectedIndex = FDataSource.DataSet.RecNo - 1 then
    begin
      Canvas.Pen.Color := clRed;
      Canvas.Pen.Width := 2;
      Canvas.Rectangle(X - 2, Y - 2, X + W + 2, Y + H + 2);
    end;

    Inc(X, W + 20);
    if X + W > ClientWidth then
    begin
      X := 10;
      Inc(Y, H + 40);
    end;

    FDataSource.DataSet.Next;
  end;
end;

procedure TDBImageGallery.Paint;
begin
  inherited Paint;
  DrawGallery;
end;

procedure TDBImageGallery.Click;
begin
  inherited Click;
  if Assigned(FDataSource) and Assigned(FDataSource.DataSet) then
  begin
    FSelectedIndex := FDataSource.DataSet.RecNo - 1;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

end.

