{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VampyreImagingPackageExt;

{$warn 5023 off : no warning about unused units}
interface

uses
  ElderImagery, ElderImageryBsi, ElderImageryCif, ElderImageryImg, 
  ElderImagerySky, ElderImageryTexture, ImagingBinary, ImagingCompare, 
  ImagingExtFileFormats, ImagingJpeg2000, ImagingPcx, ImagingPsd, ImagingTiff, 
  ImagingXpm, VampyreImagingPackageExtRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VampyreImagingPackageExtRegister', 
    @VampyreImagingPackageExtRegister.Register);
end;

initialization
  RegisterPackage('VampyreImagingPackageExt', @Register);
end.
