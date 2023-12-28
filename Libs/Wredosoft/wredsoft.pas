{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Wredsoft;

{$warn 5023 off : no warning about unused units}
interface

uses
  CylinderMap, ColorBtn, ImageBtn, Painter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CylinderMap', @CylinderMap.Register);
  RegisterUnit('ColorBtn', @ColorBtn.Register);
  RegisterUnit('ImageBtn', @ImageBtn.Register);
  RegisterUnit('Painter', @Painter.Register);
end;

initialization
  RegisterPackage('Wredsoft', @Register);
end.
