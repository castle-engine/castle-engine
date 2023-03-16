unit mbReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

//{$R mbReg.res}

uses
  ActnList,
  RColorPicker, GColorPicker, BColorPicker,
  RAxisColorPicker, GAxisColorPicker, BAxisColorPicker,
  CColorPicker, MColorPicker, YColorPicker, KColorPicker,
  HRingPicker,
  HColorPicker, SColorPicker, LVColorPicker, //LColorPicker, VColorPicker,
  HSColorPicker, HSCirclePicker, HSLColorPicker, HSLRingPicker,
  SLColorPicker, SLHColorPicker,
  CIEAColorPicker, CIEBColorPicker, CIELColorPicker,
  HexaColorPicker, mbColorPreview, mbColorList, mbColorTree, mbColorPalette,
  mbOfficeColorDialog, mbDeskPickerButton,
  LResources;

procedure Register;
begin
  RegisterComponents('mbColor Lib', [
    TRColorPicker, TGColorPicker, TBColorPicker,
    TRAxisColorPicker, TGAxisColorPicker, TBAxisColorPicker,
    TCColorPicker, TMColorPicker, TYColorPicker, TKColorPicker,
    THRingPicker, THColorPicker, TSColorPicker, TLVColorPicker,
    THSColorPicker, THSCirclePicker, THSLColorPicker, THSLRingPicker,
    TSLColorPicker, TSLHColorPicker,
    TCIEAColorPicker, TCIEBColorPicker, TCIELColorPicker,
    THexaColorPicker, TmbColorPreview, TmbColorList, TmbColorTree,
    TmbColorPalette, TmbOfficeColorDialog, TmbDeskPickerButton
  ]);

  RegisterActions('mbColorLib', [TmbDeskPickerAction], nil);
end;


{$IFDEF FPC}
initialization
  {$I mbReg.lrs}
{$ENDIF}

end.

