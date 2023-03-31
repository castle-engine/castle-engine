unit FormCastleColorPicker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Math, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Spin, HSLRingPicker, SLColorPicker, HSColorPicker,
  HRingPicker, HSCirclePicker, LVColorPicker, RColorPicker, GColorPicker,
  BColorPicker, HColorPicker, SColorPicker, CastlePropEdits, CastleColors,
  LCLIntf, Buttons, ButtonPanel, CastleEditorPropEdits;

type
  TCastleColorPickerForm = class(TForm)
    AlphaSpinEdit: TFloatSpinEdit;
    BSpinEditRgb: TFloatSpinEdit;
    BTabColorPickerRgb: TBColorPicker;
    ButtonCopy: TButton;
    ButtonPanel1: TButtonPanel;
    ButtonRevert: TButton;
    EditHex: TEdit;
    GSpinEditRgb: TFloatSpinEdit;
    GTabColorPickerRgb: TGColorPicker;
    HSPanelCirclePicker: THSCirclePicker;
    HSpinEditHsv: TFloatSpinEdit;
    HTabColorPickerHsv: THColorPicker;
    LabelTabHsvTitleH: TLabel;
    LabelTabHexTitleHex: TLabel;
    LabelTabHsvTitleV: TLabel;
    MemoPascalCode: TMemo;
    RLabelTitleRgb: TLabel;
    GLabelTitleRgb: TLabel;
    BLabelTitleRgb: TLabel;
    LabelTitleAlpha: TLabel;
    LabelTabHsvTitleS: TLabel;
    RSpinEditRgb: TFloatSpinEdit;
    SSpinEditHsv: TFloatSpinEdit;
    TabSheetHex: TTabSheet;
    TabSheetPascalCode: TTabSheet;
    TimerMousePos: TTimer;
    VPanelColorPicker: TLVColorPicker;
    AlphaColorPicker: TLVColorPicker;
    VSpinEditHsv: TFloatSpinEdit;
    VTabColorPickerHsv: TLVColorPicker;
    PageControlColorModel: TPageControl;
    PanelAlpha: TPanel;
    PanelCirclePicker: TPanel;
    RTabColorPickerRgb: TRColorPicker;
    STabColorPickerHsv: TSColorPicker;
    TabSheetHsv: TTabSheet;
    TabSheetRgb: TTabSheet;
    procedure AlphaColorPickerChange(Sender: TObject);
    procedure AlphaSpinEditChange(Sender: TObject);
    procedure BSpinEditRgbChange(Sender: TObject);
    procedure BTabColorPickerRgbChange(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonRevertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GSpinEditRgbChange(Sender: TObject);
    procedure GTabColorPickerRgbChange(Sender: TObject);
    procedure HSPanelCirclePickerChange(Sender: TObject);
    procedure HSpinEditHsvChange(Sender: TObject);
    procedure HTabColorPickerHsvChange(Sender: TObject);
    procedure PageControlColorModelChange(Sender: TObject);
    procedure RTabColorPickerRgbChange(Sender: TObject);
    procedure SSpinEditHsvChange(Sender: TObject);
    procedure STabColorPickerHsvChange(Sender: TObject);
    procedure TimerMousePosTimer(Sender: TObject);
    procedure VPanelColorPickerChange(Sender: TObject);
    procedure RSpinEditRgbChange(Sender: TObject);
    procedure VSpinEditHsvChange(Sender: TObject);
    procedure VTabColorPickerHsvChange(Sender: TObject);

  strict private
    { How many digits to use for comparisons }
    ColorPrecision: TRoundToRange;
    ColorEpsilon: Single;
    { A flag to not close window when mouse is outside of form but never
      was inside }
    MouseWasInForm: Boolean;
    procedure SetColorInCirclePickerPanel(NewColor: TCastleColor); overload;
    procedure SetColorInCirclePickerPanel(NewColor: TColor); overload;
    { Sets Hue the value should be in range from 0 to 6 }
    procedure SetHValueInCirclePickerPanel(const NewValue: Single);
    { Sets Saturation value should be i range from 0 to 1 }
    procedure SetSValueInCirclePickerPanel(const NewValue: Single);
    { Sets Value, it should be in range from 0 to 1 }
    procedure SetVValueInCirclePickerPanel(const NewValue: Single);
    procedure SetRValueInCirclePickerPanel(const NewValue: Single);
    procedure SetGValueInCirclePickerPanel(const NewValue: Single);
    procedure SetBValueInCirclePickerPanel(const NewValue: Single);

    procedure SetColorInRgbTab(const NewColor: TCastleColor); overload;
    procedure SetColorInRgbTab(const NewColor: TColor); overload;
    procedure SetRValueInRgbTab(const NewValue: Single);
    procedure SetGValueInRgbTab(const NewValue: Single);
    procedure SetBValueInRgbTab(const NewValue: Single);
    procedure BlockEventsInRgbTab;
    procedure UnblockEventsInRgbTab;

    procedure SetColorInHsvTab(const NewColor: TCastleColor); overload;
    procedure SetColorInHsvTab(const NewColor: TColor); overload;
    { Sets Hue in Hsv tab NewValue should be in 0..6 range. }
    procedure SetHValueInHsvTab(const NewValue: Single);
    procedure SetSValueInHsvTab(const NewValue: Single);
    procedure SetVValueInHsvTab(const NewValue: Single);
    procedure BlockEventsInHsvTab;
    procedure UnblockEventsInHsvTab;

    procedure SetColorInHexTab(const NewColor: TCastleColor); overload;
    procedure SetColorInHexTab(const NewColor: TColor); overload;

    { Used to get current color value from HSV circle, We need function like
      this because updating all controls is too expensive. }
    procedure UpdateCurrentTabFromPanel;
    procedure SetAlphaValue(const NewValue: Single);
    procedure UpdatePropertyEditorValue;
    procedure GeneratePascalCode;
  public
    ColorPropertyEditor: TCastleAbstractColorPropertyEditor;
    PrevColor: TCastleColor;

    procedure Init(const ColorPropEditor: TCastleAbstractColorPropertyEditor;
      const InitColor: TCastleColor;
      const ShowAlpha: Boolean = true); overload;

    procedure Init(const ColorPropEditor: TCastleAbstractColorPropertyEditor;
      const InitColorRGB: TCastleColorRGB); overload;

    function CurrentCastleColor: TCastleColor;
    function CurrentCastleColorRGB: TCastleColorRGB;
  end;

var
  CastleColorPickerForm: TCastleColorPickerForm;

implementation

{$R *.lfm}

uses Clipbrd, PropEdits,
  CastleVectors, CastleLog, CastleUtils, mbColorConv, CastleControl;

{ TCastleColorPickerForm }

procedure TCastleColorPickerForm.HSPanelCirclePickerChange(Sender: TObject);
begin
  UpdatePropertyEditorValue;
  UpdateCurrentTabFromPanel;
end;

procedure TCastleColorPickerForm.GTabColorPickerRgbChange(Sender: TObject);
begin
  SetGValueInRgbTab(GTabColorPickerRgb.Green / 255);
  SetGValueInCirclePickerPanel(GTabColorPickerRgb.Green / 255);
end;

procedure TCastleColorPickerForm.BTabColorPickerRgbChange(Sender: TObject);
begin
  SetBValueInRgbTab(BTabColorPickerRgb.Blue / 255);
  SetBValueInCirclePickerPanel(BTabColorPickerRgb.Blue / 255);
end;

procedure TCastleColorPickerForm.ButtonCopyClick(Sender: TObject);
begin
  Clipboard.AsText := EditHex.Text;
end;

procedure TCastleColorPickerForm.ButtonRevertClick(Sender: TObject);
begin
  SetColorInCirclePickerPanel(PrevColor);
  SetAlphaValue(PrevColor.W);
end;

procedure TCastleColorPickerForm.FormCreate(Sender: TObject);
begin
  MouseWasInForm := false;
  TimerMousePos.Enabled := true;
end;

procedure TCastleColorPickerForm.FormDestroy(Sender: TObject);
begin
  TimerMousePos.Enabled := false;
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
  SetRValueInRgbTab(RTabColorPickerRgb.Red / 255);
  SetRValueInCirclePickerPanel(RTabColorPickerRgb.Red / 255);
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

procedure TCastleColorPickerForm.TimerMousePosTimer(Sender: TObject);
var
  TestRect: TRect;
  Margin: Integer;
begin
  Margin := Round (Width / 4);

  TestRect.Top := ClientRect.Top - Margin ;
  TestRect.Left := ClientRect.Left - Margin;
  TestRect.Width := ClientRect.Width + Margin * 2;
  TestRect.Height := ClientRect.Height + Margin * 2;

  if TestRect.Contains(ScreenToClient(Mouse.CursorPos)) then
    MouseWasInForm := true
  else
  if MouseWasInForm then
    Close;
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
  if not SameValue(RoundTo(HSPanelCirclePicker.RelHue, ColorPrecision),
      NewValueForControl, ColorEpsilon) then
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
  const NewValue: Single);
var
  NewValueInt: Integer;
begin
  NewValueInt := Round(RoundTo(NewValue, ColorPrecision) * 255);
  if HSPanelCirclePicker.Red <> NewValueInt then
    HSPanelCirclePicker.Red := NewValueInt;
end;

procedure TCastleColorPickerForm.SetGValueInCirclePickerPanel(
  const NewValue: Single);
var
  NewValueInt: Integer;
begin
  NewValueInt := Round(RoundTo(NewValue, ColorPrecision) * 255);
  if HSPanelCirclePicker.Green <> NewValueInt then
    HSPanelCirclePicker.Green := NewValueInt;
end;

procedure TCastleColorPickerForm.SetBValueInCirclePickerPanel(
  const NewValue: Single);
var
  NewValueInt: Integer;
begin
  NewValueInt := Round(RoundTo(NewValue, ColorPrecision) * 255);
  if HSPanelCirclePicker.Blue <> NewValueInt then
    HSPanelCirclePicker.Blue := NewValueInt;
end;

procedure TCastleColorPickerForm.SetColorInRgbTab(const NewColor: TCastleColor);
begin
  BlockEventsInRgbTab;
  try
    SetRValueInRgbTab(NewColor.X);
    SetGValueInRgbTab(NewColor.Y);
    SetBValueInRgbTab(NewColor.Z);
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
    SetRValueInRgbTab(GetRValue(Rgb) / 255);
    SetGValueInRgbTab(GetGValue(Rgb) / 255);
    SetBValueInRgbTab(GetBValue(Rgb) / 255);
  finally
    UnblockEventsInRgbTab;
  end;
end;

procedure TCastleColorPickerForm.SetRValueInRgbTab(const NewValue: Single);
var
  NewValueRounded: Single;
  NewValueInt: Integer;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  NewValueInt := Round(NewValueRounded * 255);
  if RTabColorPickerRgb.Red <> NewValueInt then
    RTabColorPickerRgb.Red := NewValueInt;
  if GTabColorPickerRgb.Red <> NewValueInt then
    GTabColorPickerRgb.Red := NewValueInt;
  if BTabColorPickerRgb.Red <> NewValueInt then
    BTabColorPickerRgb.Red := NewValueInt;
  if not SameValue(RSpinEditRgb.Value, NewValueRounded, ColorEpsilon) then
    RSpinEditRgb.Value := NewValueRounded;
end;

procedure TCastleColorPickerForm.SetGValueInRgbTab(const NewValue: Single);
var
  NewValueRounded: Single;
  NewValueInt: Integer;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  NewValueInt := Round(NewValueRounded * 255);
  if RTabColorPickerRgb.Green <> NewValueInt then
    RTabColorPickerRgb.Green := NewValueInt;
  if GTabColorPickerRgb.Green <> NewValueInt then
    GTabColorPickerRgb.Green := NewValueInt;
  if BTabColorPickerRgb.Green <> NewValueInt then
    BTabColorPickerRgb.Green := NewValueInt;
  if not SameValue(GSpinEditRgb.Value, NewValueRounded, ColorEpsilon) then
    GSpinEditRgb.Value := NewValueRounded;
end;

procedure TCastleColorPickerForm.SetBValueInRgbTab(const NewValue: Single);
var
  NewValueRounded: Single;
  NewValueInt: Integer;
begin
  NewValueRounded := RoundTo(NewValue, ColorPrecision);
  NewValueInt := Round(NewValueRounded * 255);
  if RTabColorPickerRgb.Blue <> NewValueInt then
    RTabColorPickerRgb.Blue := NewValueInt;
  if GTabColorPickerRgb.Blue <> NewValueInt then
    GTabColorPickerRgb.Blue := NewValueInt;
  if BTabColorPickerRgb.Blue <> NewValueInt then
    BTabColorPickerRgb.Blue := NewValueInt;
  if not SameValue(BSpinEditRgb.Value, NewValueRounded, ColorEpsilon) then
    BSpinEditRgb.Value := NewValueRounded;
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

procedure TCastleColorPickerForm.SetColorInHexTab(const NewColor: TCastleColor);
begin
  EditHex.Text := ColorToHex(NewColor);
end;

procedure TCastleColorPickerForm.SetColorInHexTab(const NewColor: TColor);
var
  ColorByte: TVector4Byte;
begin
  RedGreenBlue(NewColor, ColorByte.X, ColorByte.Y, ColorByte.Z);
  ColorByte.W := AlphaColorPicker.Value;
  EditHex.Text := ColorToHex(Vector4(ColorByte));
end;

procedure TCastleColorPickerForm.UpdateCurrentTabFromPanel;
begin
  if PageControlColorModel.ActivePage = TabSheetRgb then
    SetColorInRgbTab(HSPanelCirclePicker.SelectedColor)
  else
  if PageControlColorModel.ActivePage = TabSheetHsv then
    SetColorInHsvTab(HSPanelCirclePicker.SelectedColor)
  else
  if PageControlColorModel.ActivePage = TabSheetPascalCode then
    GeneratePascalCode
  else
  if PageControlColorModel.ActivePage = TabSheetHex then
    SetColorInHexTab(HSPanelCirclePicker.SelectedColor);
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
    ColorPropertyEditor.SetValue(ColorToHex(CurrentCastleColor));
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

  { Synchronize generated snippet with tests in tests/code/testcases/testcastlecolors.pas :
    TTestCastleColors.TestColorPickerCodeRgb
    TTestCastleColors.TestColorPickerCodeRgba }

  if AlphaColorPicker.Enabled then
  begin
    MemoPascalCode.Lines.Add('// Color constant:');
    MemoPascalCode.Lines.Add('const');
    MemoPascalCode.Lines.Add('  MyColor: TCastleColor = (' +
      'X: ' + RText + '; Y: ' + GText + '; Z: ' + BText + '; W: ' + AText + ');');

    MemoPascalCode.Lines.Add('');
    MemoPascalCode.Lines.Add('// Examples how to set color:');
    MemoPascalCode.Lines.Add('MyControl.Color := MyColor;');
    MemoPascalCode.Lines.Add('MyControl.Color := Vector4(' + RText + ', ' + GText +
      ', ' + BText + ', ' + AText +');');
    MemoPascalCode.Lines.Add('MyControl.Color := HsvToRgba(Vector3(' + HText +
      ', ' + SText + ', ' + VText + '), ' + AText + ');');
    MemoPascalCode.Lines.Add('MyControl.Color := HexToColor(''' + ColorToHex(CurrentCastleColor) + ''');');
  end else
  begin
    MemoPascalCode.Lines.Add('// Color constant:');
    MemoPascalCode.Lines.Add('const');
    MemoPascalCode.Lines.Add('  MyColor: TCastleColorRGB = (' +
      'X: ' + RText + '; Y: ' + GText + '; Z: ' + BText + ');');

    MemoPascalCode.Lines.Add('');
    MemoPascalCode.Lines.Add('// Examples how to set RGB color:');
    MemoPascalCode.Lines.Add('MyControl.ColorRGB := MyColor;');
    MemoPascalCode.Lines.Add('MyControl.ColorRGB := Vector3(' + RText + ', ' + GText +
      ', ' + BText + ');');
    MemoPascalCode.Lines.Add('MyControl.ColorRGB := HsvToRgb(Vector3(' + HText +
      ', ' + SText + ', ' + VText + '));');
    MemoPascalCode.Lines.Add('MyControl.ColorRGB := HexToColorRGB(''' + ColorRGBToHex(CurrentCastleColorRGB) + ''');');
  end;

  { Without this, at least on LCL WinAPI, the memo is scrolled to the end
    after the above operations.
    We prefer to show the memo beginning. }
  MemoPascalCode.SelStart := 0;
end;

function TCastleColorPickerForm.CurrentCastleColor: TCastleColor;
var
  ColorByte: TVector4Byte;
begin
  RedGreenBlue(HSPanelCirclePicker.SelectedColor, ColorByte.X, ColorByte.Y, ColorByte.Z);
  ColorByte.W := AlphaColorPicker.Value;
  Result := Vector4(ColorByte);
end;

function TCastleColorPickerForm.CurrentCastleColorRGB: TCastleColorRGB;
var
  CurrentColor: TCastleColor;
begin
  CurrentColor := CurrentCastleColor;
  Result.X := CurrentColor.X;
  Result.Y := CurrentColor.Y;
  Result.Z := CurrentColor.Z;
end;

procedure TCastleColorPickerForm.Init(
  const ColorPropEditor: TCastleAbstractColorPropertyEditor;
  const InitColor: TCastleColor;
  const ShowAlpha: Boolean);

  function CaptionFromEditor(const PropEditor: TPropertyEditor): String;
  begin
    Result := 'Change ';
    Assert(PropEditor.PropCount > 0);

    // add component name
    if PropEditor.GetComponent(0) <> nil then
    begin
      // GetComponent returns TPersistent, not TComponent
      if PropEditor.GetComponent(0) is TComponent then
        Result := Result + TComponent(PropEditor.GetComponent(0)).Name + '.'
      else
        Result := Result + PropEditor.GetComponent(0).ClassName + '.';
    end;

    // add property name
    Result := Result + PropEditor.GetName;

    // add ... to signal multiple properties
    if PropEditor.PropCount > 1 then
      Result := Result + '...';
  end;

begin
  ColorPropertyEditor := ColorPropEditor;
  Caption := CaptionFromEditor(ColorPropEditor);

  ColorPrecision := -3;
  ColorEpsilon := 0.0009;
  PrevColor := InitColor;
  SetColorInCirclePickerPanel(InitColor);
  SetColorInRgbTab(InitColor);
  SetColorInHsvTab(InitColor);
  SetAlphaValue(InitColor.W);

  PanelAlpha.Visible:= ShowAlpha;
  AlphaColorPicker.Enabled := ShowAlpha;
  AlphaSpinEdit.Enabled := ShowAlpha;
end;

procedure TCastleColorPickerForm.Init(
  const ColorPropEditor: TCastleAbstractColorPropertyEditor;
  const InitColorRGB: TCastleColorRGB);
begin
  Init(ColorPropEditor, Vector4(InitColorRGB, 1.0), false);
end;

end.

