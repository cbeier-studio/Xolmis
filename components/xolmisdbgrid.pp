unit XolmisDBGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids;

type

  { TXolmisDBGrid }

  TXolmisDBGrid = class(TDBGrid)
  private

  protected

  public
    procedure AutoAdjustColumn(aCol: Integer); override;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CBS',[TXolmisDBGrid]);
end;

{ TXolmisDBGrid }

procedure TXolmisDBGrid.AutoAdjustColumn(aCol: Integer);
var
  wCol: TColumn;
  ExtraWidth: Integer;
const
  IMAGE_PADDING = 6; // Espaçamento extra (em pixels) entre a imagem, texto e a borda da célula
begin
  // Deixa o LCL fazer o cálculo base nativo
  inherited AutoAdjustColumn(aCol);

  if (aCol >= 0) and (aCol < Columns.Count) then
  begin
    wCol := Columns[aCol];

    // Se a coluna tem uma imagem definida no título e existe uma ImageList associada
    if (wCol.Title.ImageIndex >= 0) and Assigned(TitleImageList) then
    begin
      // Soma a largura do ícone + padding à largura atual da coluna
      ExtraWidth := TitleImageList.Width + IMAGE_PADDING;
      wCol.Width := wCol.Width + ExtraWidth;
    end;
  end;
end;

end.
