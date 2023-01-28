{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VampyreImagingPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Imaging, ImagingBitmap, ImagingCanvases, ImagingClasses, ImagingComponents, 
  ImagingDds, ImagingFormats, ImagingIO, ImagingJpeg, ImagingNetworkGraphics, 
  ImagingTarga, ImagingTypes, ImagingUtility, ImagingPortableMaps, ImagingGif, 
  ImagingColors, ImagingRadiance, LazarusPackageIntf;

implementation

procedure Register;
begin
  // TGraphic types auto-registered in ImagingComponents
end;

initialization
  RegisterPackage('VampyreImagingPackage', @Register);
end.
