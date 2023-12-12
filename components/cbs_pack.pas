{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cbs_pack;

{$warn 5023 off : no warning about unused units}
interface

uses
  DBEditButton, TDICardPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DBEditButton', @DBEditButton.Register);
  RegisterUnit('TDICardPanel', @TDICardPanel.Register);
end;

initialization
  RegisterPackage('cbs_pack', @Register);
end.
