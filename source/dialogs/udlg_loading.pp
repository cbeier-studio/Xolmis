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
    function GetMin: Integer;
    function GetMax: Integer;
    function GetProgress: Integer;
    procedure SetMin(aValue: Integer);
    procedure SetMax(aValue: Integer);
    procedure SetProgress(aValue: Integer);
  public
    procedure UpdateProgress(const aMessage: String; aPercent: Integer);

    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
    property Progress: Integer read GetProgress write SetProgress;
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

function TdlgLoading.GetMax: Integer;
begin
  Result := ringProgress.MaxValue;
end;

function TdlgLoading.GetMin: Integer;
begin
  Result := ringProgress.MinValue;
end;

function TdlgLoading.GetProgress: Integer;
begin
  Result := ringProgress.Value;
end;

procedure TdlgLoading.SetMax(aValue: Integer);
begin
  ringProgress.MaxValue := aValue;
end;

procedure TdlgLoading.SetMin(aValue: Integer);
begin
  ringProgress.MinValue := aValue;
end;

procedure TdlgLoading.SetProgress(aValue: Integer);
begin
  ringProgress.Value := aValue;
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

