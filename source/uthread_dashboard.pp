unit uthread_dashboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Graphics, Controls, StdCtrls, BCPanel, BCTypes;

type

  { THomeLoaderThread }

  THomeLoaderThread = class(TThread)
  private
    Qry: TSQLQuery;
    FPanel: TBCPanel;
    procedure UpdateUI;
  protected
    procedure Execute; override;
  public
    constructor Create(APanel: TBCPanel);
  end;

implementation

uses
  cbs_themes, udm_main, uDarkStyleParams;

{ THomeLoaderThread }

constructor THomeLoaderThread.Create(APanel: TBCPanel);
begin
  inherited Create(True); // Cria a thread suspensa
  FPanel := APanel;
  FreeOnTerminate := True; // Libera a thread automaticamente quando terminar
end;

procedure THomeLoaderThread.Execute;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    Database := DMM.sqlCon;
    Transaction := DMM.sqlTrans;
    UniDirectional := True;
    Clear;
    Add('SELECT band_size, saldo');
    Add('FROM get_bands_running_out');
    Open;

    if Qry.RecordCount = 0 then
      Exit;

    First;
    while not EOF do
    begin
      Synchronize(@UpdateUI);

      Next;
    end;
  finally
    Close;
    FreeAndNil(Qry);
    FPanel.EndUpdate;
    FPanel.AutoSize := True;
  end;
end;

procedure THomeLoaderThread.UpdateUI;
var
  //Qry: TSQLQuery;
  B: TBCPanel;
  Q: TLabel;
  i: Integer;
begin
  //FPanel.BeginUpdate;
  //for i := (FPanel.ComponentCount - 1) downto 0 do
  //  if FPanel.Components[i] is TBCPanel then
  //    FPanel.Components[i].Free;

  //Qry := TSQLQuery.Create(nil);
  //with Qry, SQL do
  //try
  //  Database := DMM.sqlCon;
  //  Transaction := DMM.sqlTrans;
  //  UniDirectional := True;
  //  Clear;
  //  Add('SELECT band_size, saldo');
  //  Add('FROM get_bands_running_out');
  //  Open;
  //
  //  if Qry.RecordCount = 0 then
  //    Exit;
  //
  //  First;
  //  while not EOF do
  //  begin
      B := TBCPanel.Create(FPanel);
      with B do
      begin
        Constraints.MinHeight := 60;
        Constraints.MinWidth := 60;
        Width := 60;
        ChildSizing.LeftRightSpacing := 8;
        ChildSizing.TopBottomSpacing := 8;
        Caption := Qry.FieldByName('band_size').AsString;
        BorderBCStyle := bpsBorder;
        FontEx.Height := 24;
        FontEx.Style := [fsBold];
        FontEx.TextAlignment := bcaCenterTop;
        Rounding.RoundX := 8;
        Rounding.RoundY := 8;
        if Qry.FieldByName('saldo').AsInteger = 0 then
        begin
          Background.Color := clSystemCriticalFGLight;
          Border.Color := clSystemCriticalFGLight;
          FontEx.Color := clSystemCriticalBGLight;
        end
        else
        begin
          Background.Color := clSystemCautionFGLight;
          Border.Color := clSystemCautionFGLight;
          FontEx.Color := clSystemCautionBGLight;
        end;
        Border.Style := TBCBorderStyle.bboSolid;
        if IsDarkModeEnabled then
          Color := clCardBGDefaultDark
        else
          ParentBackground := True;
        B.Parent := FPanel;
      end;

      Q := TLabel.Create(B);
      with Q do
      begin
        Caption := IntToStr(Qry.FieldByName('saldo').AsInteger);
        Alignment := taRightJustify;
        BorderSpacing.Right := 8;
        if Qry.FieldByName('saldo').AsInteger = 0 then
        begin
          Font.Color := clSystemCriticalBGLight;
        end
        else
        begin
          Font.Color := clSystemCautionBGLight;
        end;
        Align := alBottom;
        Q.Parent := B;
      end;

  //    Next;
  //  end;
  //finally
  //  Close;
  //  FreeAndNil(Qry);
  //  FPanel.EndUpdate;
  //  FPanel.AutoSize := True;
  //end;
end;

end.

