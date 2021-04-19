{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazIdeBanner;

{$warn 5023 off : no warning about unused units}
interface

uses
  figletfont, StreamUnzipper, frmbanner, lazbannerregisteride, 
  lazbanneruiconsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazbannerregisteride', @lazbannerregisteride.Register);
end;

initialization
  RegisterPackage('lazIdeBanner', @Register);
end.
