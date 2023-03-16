{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mbColorLibLaz;

interface

uses
  PalUtils, HTMLColors, RGBCIEUtils, RGBCMYKUtils, RGBHSLUtils, RGBHSVUtils, 
  mbColorList, mbTrackBarPicker, BColorPicker, GColorPicker, RColorPicker, 
  HColorPicker, KColorPicker, MColorPicker, YColorPicker, mbColorPreview, 
  Scanlines, mbColorPickerControl, BAxisColorPicker, GAxisColorPicker, 
  RAxisColorPicker, CIEAColorPicker, CIEBColorPicker, CIELColorPicker, 
  HRingPicker, HexaColorPicker, HSColorPicker, SLColorPicker, SLHColorPicker, 
  HSCirclePicker, SelPropUtils, mbOfficeColorDialog, OfficeMoreColorsDialog, 
  HSLColorPicker, mbColorPalette, CColorPicker, SColorPicker, 
  mbDeskPickerButton, ScreenWin, mbColorTree, HSLRingPicker, mbBasicPicker, 
  mbUtils, mbReg, mbColorConv, LVColorPicker, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mbReg', @mbReg.Register);
end;

initialization
  RegisterPackage('mbColorLibLaz', @Register);
end.
