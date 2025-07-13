unit udlg_loading;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCFluentProgressRing;

type

  { TdlgLoading }

  TdlgLoading = class(TForm)
    ringProgress: TBCFluentProgressRing;
    lblLoading: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BackupProgress(Sender: TObject; Remaining, PageCount: integer);
    procedure ZipperProgress(Sender: TObject; const Pct: Double);
  private

  public
    procedure UpdateProgress(const aMessage: String; aPercent: Integer);
  end;

var
  dlgLoading: TdlgLoading;

implementation

uses
  cbs_system;

{$R *.lfm}

{ TdlgLoading }

procedure TdlgLoading.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}
end;

procedure TdlgLoading.BackupProgress(Sender: TObject; Remaining, PageCount: integer);
begin
  ringProgress.Value := 100 * (Remaining - PageCount) div PageCount;
end;

procedure TdlgLoading.UpdateProgress(const aMessage: String; aPercent: Integer);
begin
  lblLoading.Caption := aMessage;
  if aPercent < 0 then
  begin
    ringProgress.Indeterminate := True;
  end
  else
  begin
    ringProgress.Indeterminate := False;
    ringProgress.Value := aPercent;
  end;
  //Application.ProcessMessages;
end;

procedure TdlgLoading.ZipperProgress(Sender: TObject; const Pct: Double);
begin
  ringProgress.Value := Round(Pct);
end;

end.

