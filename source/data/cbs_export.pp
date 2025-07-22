{ Xolmis Export Data library

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

unit cbs_export;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Dialogs;

type
  TExportFiletype = (xfCSV, xfJSON, xfODS, xfXLSX, xfXML);

const
  ExportFileExtensions: array of String = ('.csv','.json','.ods','.xlsx','.xml');
  ExportFileFilters: array of String = ('Comma Separated Values (CSV)|*.csv',
    'JavaScript Object Notation (JSON)|*.json', 'Open Document Spreadsheet|*.ods',
    'Microsoft Excel|*.xlsx', 'Extensible Markup Language (XML)|*.xml');

  // Darwin Core
  procedure ExportDarwinCore(const aFileName: String);
  // Ecological Metadata Language
  procedure ExportEMLFile(const aFileName: String);

implementation

uses
  cbs_locale, cbs_global, cbs_dialogs;

procedure ExportDarwinCore(const aFileName: String);
begin
  { #todo : Export to Darwin Core }
end;

procedure ExportEMLFile(const aFileName: String);
begin
  { #todo : Export to Ecological Metadata Language }
end;



end.
