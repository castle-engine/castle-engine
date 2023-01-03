unit FormCastleColorPicker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Spin, HSLRingPicker, SLColorPicker, HSColorPicker, HRingPicker,
  HSCirclePicker, LVColorPicker, RColorPicker, GColorPicker, BColorPicker,
  HColorPicker, SColorPicker, CastlePropEdits, CastleColors, LCLIntf;

type
  TCastleColorPickerForm = class(TForm)
    AlphaSpinEdit: TFloatSpinEdit;
    BTabColorPickerRgb: TBColorPicker;
    GTabColorPickerRgb: TGColorPicker;
    HSPanelCirclePicker: THSCirclePicker;
    HSpinEditHsv: TFloatSpinEdit;
    HTabColorPickerHsv: THColorPicker;
    HSV: TLabel;
    LabelTabHsvTitleH: TLabel;
    LabelTabHsvTitleV: TLabel;
    RLabelTitleRgb: TLabel;
    GLabelTitleRgb: TLabel;
    BLabelTitleRgb: TLabel;
    LabelTitleAlpha: TLabel;
    LabelTabHsvTitleS: TLabel;
    SSpinEditHsv: TFloatSpinEdit;
    VPanelColorPicker: TLVColorPicker;
    AlphaColorPicker: TLVColorPicker;
    VSpinEditHsv: TFloatSpinEdit;
    VTabColorPickerHsv: TLVColorPicker;
    PageControlColorModel: TPageControl;
    PanelAlpha: TPanel;
    PanelCirclePicker: TPanel;
    RTabColorPickerRgb: TRColorPicker;
    RSpinEditRgb: TSpinEdit;
    GSpinEditRgb: TSpinEdit;
    BSpinEditRgb: TSpinEdit;
    STabColorPickerHsv: TSColorPicker;
    TabSheetHsv: TTabSheet;
    TabSheetRgb: TTabSheet;
    procedure BSpinEditRgbChange(Sender: TObject);
    procedure BTabColorPickerRgbChange(Sender: TObject);
    procedure GSpinEditRgbChange(Sender: TObject);
    procedure GTabColorPickerRgbChange(Sender: TObject);
    procedure HSPanelCirclePickerChange(Sender: TObject);
    procedure HSpinEditHsvChange(Sender: TObject);
    procedure HTabColorPickerHsvChange(Sender: TObject);
    procedure PageControlColorModelChange(Sender: TObject);
    procedure RTabColorPickerRgbChange(Sender: TObject);
    procedure SSpinEditHsvChange(Sender: TObject);
    procedure STabColorPickerHsvChange(Sender: TObject);
    procedure VPanelColorPickerChange(Sender: TObject);
    procedure RSpinEditRgbChange(Sender: TObject);
    procedure VSpinEditHsvChange(Sender: TObject);
    procedure VTabColorPickerHsvChange(Sender: TObject);

  strict private
    procedure SetColorInCirclePickerPanel(NewColor: TCastleColor); overload;
    procedure SetColorInCirclePickerPanel(NewColor: TColor); overload;
    procedure SetHValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetSValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetVValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetRValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetGValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetBValueInCirclePickerPanel(const NewValue: Integer);

    procedure SetColorInRgbTab(const NewColor: TCastleColor); overload;
    procedure SetColorInRgbTab(const NewColor: TColor); overload;
    procedure SetRValueInRgbTab(const NewValue: Integer);
    procedure SetGValueInRgbTab(const NewValue: Integer);
    procedure SetBValueInRgbTab(const NewValue: Integer);
    procedure BlockEventsInRgbTab;
    procedure UnblockEventsInRgbTab;

    { Convert Hue integer value to castle float range 0..6 }
    function HueToCastleFloat(const NewValue: Integer): Double;
    function CastleFloatToHue(const NewValue: Double): Integer;
    function ByteColorValueToCastleFloat(const NewValue: Integer): Double;
    function CastleFloatToByteColorValue(const NewValue: Double): Integer;

    procedure SetColorInHsvTab(const NewColor: TCastleColor); overload;
    procedure SetColorInHsvTab(const NewColor: TColor); overload;
    procedure SetHValueInHsvTab(const NewValue: Integer);
    procedure SetSValueInHsvTab(const NewValue: Integer);
    procedure SetVValueInHsvTab(const NewValue: Integer);
    procedure BlockEventsInHsvTab;
    procedure UnblockEventsInHsvTab;

    { Used to get current color value from HSV circle, We need function like
      this because updating all controls is too expensive. }
    procedure UpdateCurrentTabFromPanel;
  public
    ColorPropertyEditor: TCastleColorPropertyEditor;
    PrevColor: TCastleColor;

    procedure Init(const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
  end;

var
  CastleColorPickerForm: TCastleColorPickerForm;

implementation

{$R *.lfm}

uses Math, CastleVectors, CastleLog, CastleUtils, mbColorConv;

{ TCastleColorPickerForm }

procedure TCastleColorPickerForm.HSPanelCirclePickerChange(Sender: TObject);
var
  ColorByte: TVector3Byte;
  NewColor: TCastleColor;
begin
  // on color change in circle
  if Assigned(ColorPropertyEditor) then
  begin
    RedGreenBlue(HSPanelCirclePicker.SelectedColor, ColorByte.X, ColorByte.Y, ColorByte.Z);

    NewColor := Vector4(Vector3(ColorByte), 1); // TODO: Alpha support

    ColorPropertyEditor.SetAllValues(NewColor);
  end;
  UpdateCurrentTabFromPanel;
end;

procedure TCastleColorPickerForm.GTabColorPickerRgbChange(Sender: TObject);
begin
  SetGValueInRgbTab(GTabColorPickerRgb.Green);
  SetGValueInCirclePickerPanel(GTabColorPickerRgb.Green);
end;

procedure TCastleColorPickerForm.BTabColorPickerRgbChange(Sender: TObject);
begin
  SetBValueInRgbTab(BTabColorPickerRgb.Blue);
  SetBValueInCirclePickerPanel(BTabColorPickerRgb.Blue);
end;

procedure TCastleColorPickerForm.BSpinEditRgbChange(Sender: TObject);
begin
  SetBValueInRgbTab(BSpinEditRgb.Value);
  SetBValueInCirclePickerPanel(BSpinEditRgb.Value);
end;

procedure TCastleColorPickerForm.GSpinEditRgbChange(Sender: TObject);
begin
  SetGValueInRgbTab(GSpinEditRgb.Value);
  SetGValueInCirclePickerPanel(GSpinEditRgb.Value);
end;

procedure TCastleColorPickerForm.HSpinEditHsvChange(Sender: TObject);
var
  NewValue: Integer;
begin
  NewValue := CastleFloatToHue(HSpinEditHsv.Value);
  SetHValueInHsvTab(NewValue);
  SetHValueInCirclePickerPanel(NewValue);
end;

procedure TCastleColorPickerForm.HTabColorPickerHsvChange(Sender: TObject);
begin
  SetHValueInHsvTab(HTabColorPickerHsv.Hue);
  SetHValueInCirclePickerPanel(HTabColorPickerHsv.Hue);
end;

procedure TCastleColorPickerForm.PageControlColorModelChange(Sender: TObject);
begin
  { Update values when tab is shown. }
  UpdateCurrentTabFromPanel;
end;

procedure TCastleColorPickerForm.RTabColorPickerRgbChange(Sender: TObject);
begin
  SetRValueInRgbTab(RTabColorPickerRgb.Red);
  SetRValueInCirclePickerPanel(RTabColorPickerRgb.Red);
end;

procedure TCastleColorPickerForm.SSpinEditHsvChange(Sender: TObject);
var
  NewValue: Integer;
begin
  WritelnLog('SSpinEditHsvChange');
  NewValue := CastleFloatToByteColorValue(SSpinEditHsv.Value);

  SetSValueInHsvTab(NewValue);
  SetSValueInCirclePickerPanel(NewValue);
end;

procedure TCastleColorPickerForm.STabColorPickerHsvChange(Sender: TObject);
begin
  WritelnLog('STabColorPickerHsvChange');
  SetSValueInHsvTab(STabColorPickerHsv.Saturation);
  SetSValueInCirclePickerPanel(STabColorPickerHsv.Saturation);
end;

procedure TCastleColorPickerForm.VPanelColorPickerChange(Sender: TObject);
begin
  if HSPanelCirclePicker.Value <> VPanelColorPicker.Value then
    HSPanelCirclePicker.Value := VPanelColorPicker.Value;
end;

procedure TCastleColorPickerForm.RSpinEditRgbChange(Sender: TObject);
begin
  SetRValueInRgbTab(RSpinEditRgb.Value);
  SetRValueInCirclePickerPanel(RSpinEditRgb.Value);
end;

procedure TCastleColorPickerForm.VSpinEditHsvChange(Sender: TObject);
var
  NewValue: Integer;
begin
  WritelnLog('VSpinEditHsvChange');
  NewValue := CastleFloatToByteColorValue(VSpinEditHsv.Value);

  SetVValueInHsvTab(NewValue);
  SetVValueInCirclePickerPanel(NewValue);
end;

procedure TCastleColorPickerForm.VTabColorPickerHsvChange(Sender: TObject);
begin
  SetVValueInHsvTab(VTabColorPickerHsv.Value);
  SetVValueInCirclePickerPanel(VTabColorPickerHsv.Value);
end;

procedure TCastleColorPickerForm.SetColorInCirclePickerPanel(NewColor: TCastleColor);
var
  ColorByte: TVector3Byte;
begin
  ColorByte := Vector3Byte(NewColor.XYZ); // edit only Color RGB
  SetColorInCirclePickerPanel(RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]));
end;

procedure TCastleColorPickerForm.SetColorInCirclePickerPanel(NewColor: TColor);
begin
  if HSPanelCirclePicker.SelectedColor <> NewColor then
    HSPanelCirclePicker.SelectedColor := NewColor;
  if VPanelColorPicker.Value <> HSPanelCirclePicker.Value then
    VPanelColorPicker.Value := HSPanelCirclePicker.Value;
end;

procedure TCastleColorPickerForm.SetHValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Hue <> NewValue then
    HSPanelCirclePicker.Hue := NewValue;
end;

procedure TCastleColorPickerForm.SetSValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Saturation <> NewValue then
    HSPanelCirclePicker.Saturation := NewValue;
end;

procedure TCastleColorPickerForm.SetVValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Value <> NewValue then
    HSPanelCirclePicker.Value := NewValue;
  if VPanelColorPicker.Value <> NewValue then
    VPanelColorPicker.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetRValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Red <> NewValue then
    HSPanelCirclePicker.Red := NewValue;
end;

procedure TCastleColorPickerForm.SetGValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Green <> NewValue then
    HSPanelCirclePicker.Green := NewValue;
end;

procedure TCastleColorPickerForm.SetBValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Blue <> NewValue then
    HSPanelCirclePicker.Blue := NewValue;
end;

procedure TCastleColorPickerForm.SetColorInRgbTab(const NewColor: TCastleColor);
var
  ColorByte: TVector3Byte;
begin
  ColorByte := Vector3Byte(NewColor.XYZ); // edit only Color RGB
  BlockEventsInRgbTab;
  try
    SetRValueInRgbTab(ColorByte[0]);
    SetGValueInRgbTab(ColorByte[1]);
    SetBValueInRgbTab(ColorByte[2]);
  finally
    UnblockEventsInRgbTab;
  end;
end;

procedure TCastleColorPickerForm.SetColorInRgbTab(const NewColor: TColor);
var
  Rgb: Longint;
begin
  Rgb := ColorToRGB(NewColor);
  BlockEventsInRgbTab;
  try
    SetRValueInRgbTab(GetRValue(Rgb));
    SetGValueInRgbTab(GetGValue(Rgb));
    SetBValueInRgbTab(GetBValue(Rgb));
  finally
    UnblockEventsInRgbTab;
  end;
end;

procedure TCastleColorPickerForm.SetRValueInRgbTab(const NewValue: Integer);
begin
  if RTabColorPickerRgb.Red <> NewValue then
    RTabColorPickerRgb.Red := NewValue;
  if GTabColorPickerRgb.Red <> NewValue then
    GTabColorPickerRgb.Red := NewValue;
  if BTabColorPickerRgb.Red <> NewValue then
    BTabColorPickerRgb.Red := NewValue;
  if RSpinEditRgb.Value <> NewValue then
    RSpinEditRgb.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetGValueInRgbTab(const NewValue: Integer);
begin
  if RTabColorPickerRgb.Green <> NewValue then
    RTabColorPickerRgb.Green := NewValue;
  if GTabColorPickerRgb.Green <> NewValue then
    GTabColorPickerRgb.Green := NewValue;
  if BTabColorPickerRgb.Green <> NewValue then
    BTabColorPickerRgb.Green := NewValue;
  if GSpinEditRgb.Value <> NewValue then
    GSpinEditRgb.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetBValueInRgbTab(const NewValue: Integer);
begin
  if RTabColorPickerRgb.Blue <> NewValue then
    RTabColorPickerRgb.Blue := NewValue;
  if GTabColorPickerRgb.Blue <> NewValue then
    GTabColorPickerRgb.Blue := NewValue;
  if BTabColorPickerRgb.Blue <> NewValue then
    BTabColorPickerRgb.Blue := NewValue;
  if BSpinEditRgb.Value <> NewValue then
    BSpinEditRgb.Value := NewValue;
end;

procedure TCastleColorPickerForm.BlockEventsInRgbTab;
begin
  RTabColorPickerRgb.OnChange := nil;
  GTabColorPickerRgb.OnChange := nil;
  BTabColorPickerRgb.OnChange := nil;
  RSpinEditRgb.OnChange := nil;
  GSpinEditRgb.OnChange := nil;
  BSpinEditRgb.OnChange := nil;
end;

procedure TCastleColorPickerForm.UnblockEventsInRgbTab;
begin
  RTabColorPickerRgb.OnChange := @RTabColorPickerRgbChange;
  GTabColorPickerRgb.OnChange := @GTabColorPickerRgbChange;
  BTabColorPickerRgb.OnChange := @BTabColorPickerRgbChange;
  RSpinEditRgb.OnChange := @RSpinEditRgbChange;
  GSpinEditRgb.OnChange := @GSpinEditRgbChange;
  BSpinEditRgb.OnChange := @BSpinEditRgbChange;
end;

function TCastleColorPickerForm.HueToCastleFloat(
  const NewValue: Integer): Double;
begin
  Result := Clamped(NewValue / HTabColorPickerHsv.MaxHue * 6, 0, 6);
  //WritelnLog('HueToCastleFloat ' + FloatToStrDot(Result) + '(' + IntToStr(NewValue) + ')');
end;

function TCastleColorPickerForm.CastleFloatToHue(
  const NewValue: Double): Integer;
begin
  Result := Round(NewValue / 6 * HTabColorPickerHsv.MaxHue);
  //WritelnLog('CastleFloatToHue ' + IntToStr(Result) + '(' + FloatToStrDot(NewValue) + ')');
end;

function TCastleColorPickerForm.ByteColorValueToCastleFloat(
  const NewValue: Integer): Double;
begin
  Result := Clamped(NewValue / 255, 0, 1);
  //WritelnLog('ByteColorValueToCastleFloat ' + FloatToStrDot(Result) + '(' + IntToStr(NewValue) + ')');
end;

function TCastleColorPickerForm.CastleFloatToByteColorValue(
  const NewValue: Double): Integer;
begin
  Result := Round(NewValue * 255);
  //WritelnLog('CastleFloatToByteColorValue ' + IntToStr(Result) + '(' + FloatToStrDot(NewValue) + ')');
end;

procedure TCastleColorPickerForm.SetColorInHsvTab(const NewColor: TCastleColor);
var
  ColorByte: TVector3Byte;
  HDouble, SDouble, VDouble: Double;
  H, S, V: Integer;
begin
  ColorByte := Vector3Byte(NewColor.XYZ); // edit only Color RGB
  RGBtoHSV(ColorByte[0], ColorByte[1], ColorByte[2], HDouble, SDouble, VDouble);

  H := Round(HDouble * HTabColorPickerHsv.MaxHue);
  S := Round(SDouble * STabColorPickerHsv.MaxSaturation);
  V := Round(VDouble * VTabColorPickerHsv.MaxValue);

  BlockEventsInHsvTab;
  try
    SetHValueInHsvTab(H);
    SetSValueInHsvTab(S);
    SetVValueInHsvTab(V);
  finally
    UnblockEventsInHsvTab;
  end;
end;

procedure TCastleColorPickerForm.SetColorInHsvTab(const NewColor: TColor);
var
  HDouble, SDouble, VDouble: Double;
  H, S, V: Integer;
begin
  ColortoHSV(NewColor, HDouble, SDouble, VDouble);
  H := Round(HDouble * HTabColorPickerHsv.MaxHue);
  S := Round(SDouble * STabColorPickerHsv.MaxSaturation);
  V := Round(VDouble * VTabColorPickerHsv.MaxValue);

  BlockEventsInHsvTab;
  try
    SetHValueInHsvTab(H);
    SetSValueInHsvTab(S);
    SetVValueInHsvTab(V);
  finally
    UnblockEventsInHsvTab;
  end;
end;

procedure TCastleColorPickerForm.SetHValueInHsvTab(const NewValue: Integer);
var
  NewDoubleValue: Double;
begin
  if HTabColorPickerHsv.Hue <> NewValue then
    HTabColorPickerHsv.Hue := NewValue;
  if STabColorPickerHsv.Hue <> NewValue then
    STabColorPickerHsv.Hue := NewValue;
  if VTabColorPickerHsv.Hue <> NewValue then
    VTabColorPickerHsv.Hue := NewValue;

  NewDoubleValue := HueToCastleFloat(NewValue);
  if not SameValue(HSpinEditHsv.Value, NewDoubleValue) then
    HSpinEditHsv.Value := NewDoubleValue;
end;

procedure TCastleColorPickerForm.SetSValueInHsvTab(const NewValue: Integer);
var
  NewDoubleValue: Double;
begin
  if HTabColorPickerHsv.Saturation <> NewValue then
    HTabColorPickerHsv.Saturation := NewValue;
  if STabColorPickerHsv.Saturation <> NewValue then
    STabColorPickerHsv.Saturation := NewValue;
  if VTabColorPickerHsv.Saturation <> NewValue then
    VTabColorPickerHsv.Saturation := NewValue;

  NewDoubleValue := ByteColorValueToCastleFloat(NewValue);
  if not SameValue(SSpinEditHsv.Value, NewDoubleValue) then
  begin
    WritelnLog('Same value: ' + FloatToStr(SSpinEditHsv.Value) + ' <> ' + FloatToStr(NewDoubleValue));
    SSpinEditHsv.Value := NewDoubleValue;
  end;
end;

procedure TCastleColorPickerForm.SetVValueInHsvTab(const NewValue: Integer);
var
  NewDoubleValue: Double;
begin
  if HTabColorPickerHsv.Value <> NewValue then
    HTabColorPickerHsv.Value := NewValue;
  if STabColorPickerHsv.Value <> NewValue then
    STabColorPickerHsv.Value := NewValue;
  if VTabColorPickerHsv.Value <> NewValue then
    VTabColorPickerHsv.Value := NewValue;

  NewDoubleValue := ByteColorValueToCastleFloat(NewValue);
  if not SameValue(VSpinEditHsv.Value, NewDoubleValue) then
    VSpinEditHsv.Value := NewDoubleValue;
end;

procedure TCastleColorPickerForm.BlockEventsInHsvTab;
begin
  HTabColorPickerHsv.OnChange := nil;
  STabColorPickerHsv.OnChange := nil;
  VTabColorPickerHsv.OnChange := nil;
  HSpinEditHsv.OnChange := nil;
  SSpinEditHsv.OnChange := nil;
  VSpinEditHsv.OnChange := nil;
end;

procedure TCastleColorPickerForm.UnblockEventsInHsvTab;
begin
  HTabColorPickerHsv.OnChange := @HTabColorPickerHsvChange;
  STabColorPickerHsv.OnChange := @STabColorPickerHsvChange;
  VTabColorPickerHsv.OnChange := @VTabColorPickerHsvChange;
  HSpinEditHsv.OnChange := @HSpinEditHsvChange;
  SSpinEditHsv.OnChange := @SSpinEditHsvChange;
  VSpinEditHsv.OnChange := @VSpinEditHsvChange;
end;

procedure TCastleColorPickerForm.UpdateCurrentTabFromPanel;
begin
  if PageControlColorModel.ActivePage = TabSheetRgb then
    SetColorInRgbTab(HSPanelCirclePicker.SelectedColor)
  else if PageControlColorModel.ActivePage = TabSheetHsv then
    SetColorInHsvTab(HSPanelCirclePicker.SelectedColor);
end;

procedure TCastleColorPickerForm.Init(
  const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
begin
  ColorPropertyEditor := ColorPropEditor;
  PrevColor := InitColor;
  SetColorInCirclePickerPanel(InitColor);
  SetColorInRgbTab(InitColor);
  SetColorInHsvTab(InitColor);
end;

end.

