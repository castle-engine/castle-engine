unit FormCastleColorPicker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Math, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Spin, HSLRingPicker, SLColorPicker, HSColorPicker, HRingPicker,
  HSCirclePicker, LVColorPicker, RColorPicker, GColorPicker, BColorPicker,
  HColorPicker, SColorPicker, CastlePropEdits, CastleColors, LCLIntf, Buttons;

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
    MemoPascalCode: TMemo;
    RLabelTitleRgb: TLabel;
    GLabelTitleRgb: TLabel;
    BLabelTitleRgb: TLabel;
    LabelTitleAlpha: TLabel;
    LabelTabHsvTitleS: TLabel;
    SSpinEditHsv: TFloatSpinEdit;
    TabSheetPascalCode: TTabSheet;
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
    procedure AlphaColorPickerChange(Sender: TObject);
    procedure AlphaSpinEditChange(Sender: TObject);
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
    { How many digits to use for comparisons }
    ColorPrecision: TRoundToRange;
    ColorEpsilon: Single;

    procedure SetColorInCirclePickerPanel(NewColor: TCastleColor); overload;
    procedure SetColorInCirclePickerPanel(NewColor: TColor); overload;
    { Sets Hue the value should be in range from 0 to 6 }
    procedure SetHValueInCirclePickerPanel(const NewValue: Single);
    { Sets Saturation value should be i range from 0 to 1 }
    procedure SetSValueInCirclePickerPanel(const NewValue: Single);
    { Sets Value, it should be in range from 0 to 1 }
    procedure SetVValueInCirclePickerPanel(const NewValue: Single);
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
    { Sets Hue in Hsv tab NewValue should be in 0..6 range. }
    procedure SetHValueInHsvTab(const NewValue: Single);
    procedure SetSValueInHsvTab(const NewValue: Single);
    procedure SetVValueInHsvTab(const NewValue: Single);
    procedure BlockEventsInHsvTab;
    procedure UnblockEventsInHsvTab;

    { Used to get current color value from HSV circle, We need function like
      this because updating all controls is too expensive. }
    procedure UpdateCurrentTabFromPanel;

    procedure SetAlphaValue(const NewValue: Single);

    procedure UpdatePropertyEditorValue;

    procedure GeneratePascalCode;

    function CurrentCastleColor: TCastleColor;
  public
    ColorPropertyEditor: TCastleColorPropertyEditor;
    PrevColor: TCastleColor;

    procedure Init(const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
  end;

var
  CastleColorPickerForm: TCastleColorPickerForm;

implementation

{$R *.lfm}

uses CastleVectors, CastleLog, CastleUtils, mbColorConv;

{ TCastleColorPickerForm }

procedure TCastleColorPickerForm.HSPanelCirclePickerChange(Sender: TObject);
begin
  UpdatePropertyEditorValue;
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

procedure TCastleColorPickerForm.AlphaColorPickerChange(Sender: TObject);
begin
  SetAlphaValue(AlphaColorPicker.RelValue);
end;

procedure TCastleColorPickerForm.AlphaSpinEditChange(Sender: TObject);
begin
  SetAlphaValue(AlphaSpinEdit.Value);
end;

procedure TCastleColorPickerForm.GSpinEditRgbChange(Sender: TObject);
begin
  SetGValueInRgbTab(GSpinEditRgb.Value);
  SetGValueInCirclePickerPanel(GSpinEditRgb.Value);
end;

procedure TCastleColorPickerForm.HSpinEditHsvChange(Sender: TObject);
begin
  SetHValueInHsvTab(HSpinEditHsv.Value); // Spin edit uses castle range from 0..6
  SetHValueInCirclePickerPanel(HSpinEditHsv.Value);
end;

procedure TCastleColorPickerForm.HTabColorPickerHsvChange(Sender: TObject);
begin
  SetHValueInHsvTab(HTabColorPickerHsv.RelHue * 6);
  SetHValueInCirclePickerPanel(HTabColorPickerHsv.RelHue * 6);
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
begin
  SetSValueInHsvTab(SSpinEditHsv.Value);
  SetSValueInCirclePickerPanel(SSpinEditHsv.Value);
end;

procedure TCastleColorPickerForm.STabColorPickerHsvChange(Sender: TObject);
begin
  SetSValueInHsvTab(STabColorPickerHsv.RelSaturation);
  SetSValueInCirclePickerPanel(STabColorPickerHsv.RelSaturation);
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
begin
  SetVValueInHsvTab(VSpinEditHsv.Value);
  SetVValueInCirclePickerPanel(VSpinEditHsv.Value);
end;

procedure TCastleColorPickerForm.VTabColorPickerHsvChange(Sender: TObject);
begin
  SetVValueInHsvTab(VTabColorPickerHsv.RelValue);
  SetVValueInCirclePickerPanel(VTabColorPickerHsv.RelValue);
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
  const NewValue: Single);
var
  NewValueRounded: Single;
  NewValueForControl: Single; // THSCirclePicker uses value from 0..1
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  NewValueForControl := RoundTo(NewValue / 6, ColorPrecision);
  if not SameValue(RoundTo(HSPanelCirclePicker.RelHue, ColorPrecision), NewValueForControl, ColorEpsilon) then
    HSPanelCirclePicker.RelHue := NewValueForControl;
end;

procedure TCastleColorPickerForm.SetSValueInCirclePickerPanel(
  const NewValue: Single);
var
  NewValueRounded: Single;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  if not SameValue(RoundTo(HSPanelCirclePicker.RelSaturation, ColorPrecision),
      NewValueRounded, ColorEpsilon) then
    HSPanelCirclePicker.RelSaturation := NewValueRounded;
end;

procedure TCastleColorPickerForm.SetVValueInCirclePickerPanel(
  const NewValue: Single);
var
  NewValueRounded: Single;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  if not SameValue(HSPanelCirclePicker.RelValue, NewValueRounded, ColorEpsilon) then
    HSPanelCirclePicker.RelValue := NewValueRounded;
  if not SameValue(VPanelColorPicker.RelValue, NewValueRounded, ColorEpsilon) then
    VPanelColorPicker.RelValue := NewValueRounded;
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
begin
  ColorByte := Vector3Byte(NewColor.XYZ); // edit only Color RGB
  RGBtoHSV(ColorByte[0], ColorByte[1], ColorByte[2], HDouble, SDouble, VDouble);

  BlockEventsInHsvTab;
  try
    SetHValueInHsvTab(HDouble * 6);
    SetSValueInHsvTab(SDouble);
    SetVValueInHsvTab(VDouble);
  finally
    UnblockEventsInHsvTab;
  end;
end;

procedure TCastleColorPickerForm.SetColorInHsvTab(const NewColor: TColor);
var
  HDouble, SDouble, VDouble: Double;
begin
  ColortoHSV(NewColor, HDouble, SDouble, VDouble);

  BlockEventsInHsvTab;
  try
    SetHValueInHsvTab(HDouble * 6);
    SetSValueInHsvTab(SDouble);
    SetVValueInHsvTab(VDouble);
  finally
    UnblockEventsInHsvTab;
  end;
end;

procedure TCastleColorPickerForm.SetHValueInHsvTab(const NewValue: Single);
var
  NewValueRounded: Single;
  NewValueForControl: Single; // controls uses 0..1 range for hue
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  NewValueForControl := RoundTo(NewValueRounded / 6, ColorPrecision);
  if not SameValue(HTabColorPickerHsv.RelHue, NewValueForControl, ColorEpsilon) then
    HTabColorPickerHsv.RelHue := NewValueForControl;
  if not SameValue(STabColorPickerHsv.RelHue, NewValueForControl, ColorEpsilon) then
    STabColorPickerHsv.RelHue := NewValueForControl;
  if not SameValue(VTabColorPickerHsv.RelHue, NewValueForControl, ColorEpsilon) then
    VTabColorPickerHsv.RelHue := NewValueForControl;

  if not SameValue(HSpinEditHsv.Value, NewValueRounded, ColorEpsilon) then
    HSpinEditHsv.Value := NewValueRounded;
end;

procedure TCastleColorPickerForm.SetSValueInHsvTab(const NewValue: Single);
var
  NewValueRounded: Single;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  if not SameValue(HTabColorPickerHsv.RelSaturation, NewValueRounded, ColorEpsilon) then
    HTabColorPickerHsv.RelSaturation := NewValueRounded;
  if not SameValue(STabColorPickerHsv.RelSaturation, NewValueRounded, ColorEpsilon) then
    STabColorPickerHsv.RelSaturation := NewValueRounded;
  if not SameValue(VTabColorPickerHsv.RelSaturation, NewValueRounded, ColorEpsilon) then
    VTabColorPickerHsv.RelSaturation := NewValueRounded;

  if not SameValue(SSpinEditHsv.Value, NewValueRounded, ColorEpsilon) then
    SSpinEditHsv.Value := NewValueRounded;
end;

procedure TCastleColorPickerForm.SetVValueInHsvTab(const NewValue: Single);
var
  NewValueRounded: Single;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  if not SameValue(HTabColorPickerHsv.RelValue, NewValueRounded, ColorEpsilon) then
    HTabColorPickerHsv.RelValue := NewValueRounded;
  if not SameValue(STabColorPickerHsv.RelValue, NewValueRounded, ColorEpsilon) then
    STabColorPickerHsv.RelValue := NewValueRounded;
  if not SameValue(VTabColorPickerHsv.RelValue, NewValueRounded, ColorEpsilon) then
    VTabColorPickerHsv.RelValue := NewValue;

  if not SameValue(VSpinEditHsv.Value, NewValueRounded, ColorEpsilon) then
    VSpinEditHsv.Value := NewValueRounded;
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
    SetColorInHsvTab(HSPanelCirclePicker.SelectedColor)
  else if PageControlColorModel.ActivePage = TabSheetPascalCode then
    GeneratePascalCode;
end;

procedure TCastleColorPickerForm.SetAlphaValue(const NewValue: Single);
var
  NewValueRounded: Single;
  ValueChanged: Boolean;
begin
  ValueChanged := false;
  NewValueRounded := RoundTo(NewValue, ColorPrecision);

  if not SameValue(AlphaColorPicker.RelValue, NewValueRounded, ColorEpsilon) then
  begin
    AlphaColorPicker.RelValue := NewValueRounded;
    ValueChanged := true;
  end;

  if not SameValue(AlphaSpinEdit.Value, NewValueRounded, ColorEpsilon) then
  begin
    AlphaSpinEdit.Value := NewValueRounded;
    ValueChanged := true;
  end;

  if ValueChanged then
  begin
    UpdatePropertyEditorValue;
    if PageControlColorModel.ActivePage = TabSheetPascalCode then
      GeneratePascalCode;
  end;
end;

procedure TCastleColorPickerForm.UpdatePropertyEditorValue;
begin
  // on color change in circle
  if Assigned(ColorPropertyEditor) then
    ColorPropertyEditor.SetAllValues(CurrentCastleColor);
end;

procedure TCastleColorPickerForm.GeneratePascalCode;
var
  RSingle, GSingle, BSingle: Single;
  HSingle, SSingle, VSingle: Single;
  RText, GText, BText, AText: String;
  HText, SText, VText: String;
begin
  MemoPascalCode.Lines.Clear;

  RSingle := RoundTo(HSPanelCirclePicker.Red / 255, ColorPrecision);
  GSingle := RoundTo(HSPanelCirclePicker.Green / 255, ColorPrecision);
  BSingle := RoundTo(HSPanelCirclePicker.Blue / 255, ColorPrecision);
  HSingle := RoundTo(HSPanelCirclePicker.RelHue * 6, ColorPrecision);
  SSingle := RoundTo(HSPanelCirclePicker.RelSaturation, ColorPrecision);
  VSingle := RoundTo(HSPanelCirclePicker.RelValue, ColorPrecision);

  RText := FloatToStrFDot(RSingle, ffFixed, 0, Abs(ColorPrecision));
  GText := FloatToStrFDot(GSingle, ffFixed, 0, Abs(ColorPrecision));
  BText := FloatToStrFDot(BSingle, ffFixed, 0, Abs(ColorPrecision));
  HText := FloatToStrFDot(HSingle, ffFixed, 0, Abs(ColorPrecision));
  SText := FloatToStrFDot(SSingle, ffFixed, 0, Abs(ColorPrecision));
  VText := FloatToStrFDot(VSingle, ffFixed, 0, Abs(ColorPrecision));

  AText := FloatToStrFDot(AlphaSpinEdit.Value, ffFixed, 0, Abs(ColorPrecision));

  MemoPascalCode.Lines.Add('// Define a constant with hard-coded color value like this:');
  MemoPascalCode.Lines.Add('const');
  MemoPascalCode.Lines.Add('  MyColor: TCastleColor = (Data:' +
    ' (X: ' + RText + ', Y: ' + GText + ', Z: ' + BText + ', W: ' + AText + '));');

  MemoPascalCode.Lines.Add('');
  MemoPascalCode.Lines.Add('// Set colors from a hard-coded value like this:');
  MemoPascalCode.Lines.Add('MyControl.Color := MyColor;');
  MemoPascalCode.Lines.Add('MyControl.Color := Vector4(' + RText + ', ' + GText +
    ', ' + BText + ', ' + AText +');');
  MemoPascalCode.Lines.Add('MyControl.Color := Vector4(HsvToRgb(' + HText +
    ', ' + SText + ', ' + VText + '), ' + AText + ');');
  MemoPascalCode.Lines.Add('HexToColor(' + ColorToHex(CurrentCastleColor) + ');');
end;

function TCastleColorPickerForm.CurrentCastleColor: TCastleColor;
var
  ColorByte: TVector4Byte;
begin
  RedGreenBlue(HSPanelCirclePicker.SelectedColor, ColorByte.X, ColorByte.Y, ColorByte.Z);

  ColorByte.W := AlphaColorPicker.Value;

  Result := Vector4(ColorByte);
end;

procedure TCastleColorPickerForm.Init(
  const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
begin
  ColorPrecision := -3;
  ColorEpsilon := 0.0009;
  ColorPropertyEditor := ColorPropEditor;
  PrevColor := InitColor;
  SetColorInCirclePickerPanel(InitColor);
  SetColorInRgbTab(InitColor);
  SetColorInHsvTab(InitColor);
  SetAlphaValue(InitColor.W);
end;

end.

