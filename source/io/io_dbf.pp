{ Xolmis DBF Import library

  Copyright (C) 2025 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit io_dbf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dbf, Math, io_core;

type

  { TDBFImporter }

  TDBFImporter = class(TImporter)
  public
    class function Probe(const FileName: string; Stream: TStream): Integer; override;
    function CanHandleExtension(const Ext: string): Boolean; override;
    procedure Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer); override;
  end;

implementation

{ TDBFImporter }

function TDBFImporter.CanHandleExtension(const Ext: string): Boolean;
begin
  Result := (LowerCase(Ext) = 'dbf');
end;

procedure TDBFImporter.Import(Stream: TStream; const Options: TImportOptions; RowOut: TXRowConsumer);
var
  db: TDbf;
  row: TXRow;
  i, total: Integer;
  fname: string;
  tmp: TFileStream;
begin
  // TDbf precisa de arquivo
  fname := GetTempFileName;
  tmp := TFileStream.Create(fname, fmCreate);
  try
    tmp.CopyFrom(Stream, 0);
  finally
    tmp.Free;
  end;

  db := TDbf.Create(nil);
  try
    db.FilePathFull := ExtractFilePath(fname);
    db.TableName := ExtractFileName(fname);
    db.Open;
    total := db.RecordCount;

    db.First;
    while not db.EOF do
    begin
      if Assigned(Options.Cancel) and Options.Cancel.IsCancellationRequested then Break;

      row := TXRow.Create;
      try
        for i := 0 to db.FieldDefs.Count - 1 do
        begin
          row.Add(db.FieldDefs[i].Name);
          row.Values[db.FieldDefs[i].Name] := db.FieldByName(db.FieldDefs[i].Name).AsString;
        end;
        if Assigned(RowOut) then RowOut(row);
      finally
        row.Free;
      end;

      if Assigned(Options.OnProgress) then
        Options.OnProgress(Trunc((db.RecNo) * 100.0 / Max(1,total)), 'Importando DBF');
      db.Next;
    end;
  finally
    db.Free;
    DeleteFile(fname);
  end;
end;

class function TDBFImporter.Probe(const FileName: string; Stream: TStream): Integer;
begin
  if LowerCase(ExtractFileExt(FileName)) = '.dbf' then
    Exit(90);
  Result := 10;
end;

initialization
  TImporterRegistry.RegisterImporter('dbf', TDBFImporter);

end.

