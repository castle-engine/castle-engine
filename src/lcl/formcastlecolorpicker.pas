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
    BTabColorPicker: TBColorPicker;
    GTabColorPicker: TGColorPicker;
    HTabColorPicker: THColorPicker;
    HSPanelCirclePicker: THSCirclePicker;
    LabelTitleL: TLabel;
    LabelTitleS: TLabel;
    LabelTitleH: TLabel;
    LabelTitleR: TLabel;
    LabelTitleG: TLabel;
    LabelTitleB: TLabel;
    LabelTitleAlpha: TLabel;
    LPanelColorPicker: TLVColorPicker;
    LTabColorPicker: TLVColorPicker;
    AlphaColorPicker: TLVColorPicker;
    PageControl1: TPageControl;
    PanelAlpha: TPanel;
    PanelCirclePicker: TPanel;
    RTabColorPicker: TRColorPicker;
    STabColorPicker: TSColorPicker;
    SpinEditH: TSpinEdit;
    SpinEditS: TSpinEdit;
    SpinEditL: TSpinEdit;
    SpinEditR: TSpinEdit;
    SpinEditG: TSpinEdit;
    SpinEditB: TSpinEdit;
    SpinEditAlpha: TSpinEdit;
    TabSheetHSL: TTabSheet;
    TabSheetRGB: TTabSheet;
    procedure HSPanelCirclePickerChange(Sender: TObject);
    procedure HTabColorPickerChange(Sender: TObject);
    procedure LPanelColorPickerChange(Sender: TObject);
    procedure LTabColorPickerChange(Sender: TObject);
    procedure SpinEditHChange(Sender: TObject);
    procedure SpinEditLChange(Sender: TObject);
    procedure SpinEditSChange(Sender: TObject);
    procedure STabColorPickerChange(Sender: TObject);
  strict private

    procedure SetColorInCirclePickerPanel(NewColor: TCastleColor); overload;
    procedure SetColorInCirclePickerPanel(NewColor: TColor); overload;
    procedure SetLValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetHValueInCirclePickerPanel(const NewValue: Integer);
    procedure SetSValueInCirclePickerPanel(const NewValue: Integer);


    procedure SetColorInRgbTab(const NewColor: TCastleColor); overload;
    procedure SetColorInRgbTab(const NewColor: TColor); overload;
    procedure SetRValueInRgbTab(const NewValue: Integer);
    procedure SetGValueInRgbTab(const NewValue: Integer);
    procedure SetBValueInRgbTab(const NewValue: Integer);

    procedure SetColorInHslTab(const NewColor: TCastleColor); overload;
    procedure SetColorInHslTab(const NewColor: TColor); overload;
    procedure SetHValueInHslTab(const NewValue: Integer);
    procedure SetSValueInHslTab(const NewValue: Integer);
    procedure SetLValueInHslTab(const NewValue: Integer);
    procedure BlockEventsInHslTab;
    procedure UnblockEventsInHslTab;
  public
    ColorPropertyEditor: TCastleColorPropertyEditor;
    PrevColor: TCastleColor;

    procedure Init(const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
  end;

var
  CastleColorPickerForm: TCastleColorPickerForm;

implementation

{$R *.lfm}

uses CastleVectors, CastleLog, mbColorConv;

{ TCastleColorPickerForm }

procedure TCastleColorPickerForm.HSPanelCirclePickerChange(Sender: TObject);
var
  ColorByte: TVector3Byte;
  NewColor: TCastleColor;
begin
  // on color change
  if Assigned(ColorPropertyEditor) then
  begin
    RedGreenBlue(HSPanelCirclePicker.SelectedColor, ColorByte.X, ColorByte.Y, ColorByte.Z);

    NewColor := Vector4(Vector3(ColorByte), 1); // TODO: Alpha support

    ColorPropertyEditor.SetAllValues(NewColor);
  end;
  SetColorInRgbTab(HSPanelCirclePicker.SelectedColor);
  SetColorInHslTab(HSPanelCirclePicker.SelectedColor);
end;

procedure TCastleColorPickerForm.HTabColorPickerChange(Sender: TObject);
begin
  SetHValueInHslTab(HTabColorPicker.Hue);
  SetHValueInCirclePickerPanel(HTabColorPicker.Hue);
end;

procedure TCastleColorPickerForm.LPanelColorPickerChange(Sender: TObject);
begin
  //WritelnLog('LVColorPicker luminance: ' + IntToStr(LPanelColorPicker.Luminance));
  //WritelnLog('HSCirclePicker luminance: ' + IntToStr(HSPanelCirclePicker.Luminance));
  if HSPanelCirclePicker.Luminance <> LPanelColorPicker.Luminance then
    HSPanelCirclePicker.Luminance := LPanelColorPicker.Luminance;
end;

procedure TCastleColorPickerForm.LTabColorPickerChange(Sender: TObject);
begin
  SetLValueInHslTab(LTabColorPicker.Luminance);
  SetLValueInCirclePickerPanel(LTabColorPicker.Luminance);
end;

procedure TCastleColorPickerForm.SpinEditHChange(Sender: TObject);
begin
  SetHValueInHslTab(SpinEditH.Value);
  SetHValueInCirclePickerPanel(SpinEditH.Value);
end;

procedure TCastleColorPickerForm.SpinEditLChange(Sender: TObject);
begin
  SetLValueInHslTab(SpinEditL.Value);
  SetLValueInCirclePickerPanel(SpinEditL.Value);
end;

procedure TCastleColorPickerForm.SpinEditSChange(Sender: TObject);
begin
  SetSValueInHslTab(SpinEditS.Value);
  SetSValueInCirclePickerPanel(SpinEditS.Value);
end;

procedure TCastleColorPickerForm.STabColorPickerChange(Sender: TObject);
begin
  SetSValueInHslTab(STabColorPicker.Saturation);
  SetSValueInCirclePickerPanel(STabColorPicker.Saturation);
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
  HSPanelCirclePicker.SelectedColor := NewColor;
  if LPanelColorPicker.Luminance <> HSPanelCirclePicker.Luminance then
    LPanelColorPicker.Luminance := HSPanelCirclePicker.Luminance;
end;

procedure TCastleColorPickerForm.SetLValueInCirclePickerPanel(
  const NewValue: Integer);
begin
  if HSPanelCirclePicker.Luminance <> NewValue then
    HSPanelCirclePicker.Luminance := NewValue;
  if LPanelColorPicker.Luminance <> NewValue then
    LPanelColorPicker.Luminance := NewValue;
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

procedure TCastleColorPickerForm.SetColorInRgbTab(const NewColor: TCastleColor);
var
  ColorByte: TVector3Byte;
begin
  RTabColorPicker.OnChange := nil;
  GTabColorPicker.OnChange := nil;
  BTabColorPicker.OnChange := nil;
  try
    ColorByte := Vector3Byte(NewColor.XYZ); // edit only Color RGB
    SetRValueInRgbTab(ColorByte[0]);
    SetGValueInRgbTab(ColorByte[1]);
    SetBValueInRgbTab(ColorByte[2]);
  finally
    // to do
    RTabColorPicker.OnChange := nil;
    GTabColorPicker.OnChange := nil;
    BTabColorPicker.OnChange := nil;
  end;
end;

procedure TCastleColorPickerForm.SetColorInRgbTab(const NewColor: TColor);
var
  Rgb: Longint;
begin
  Rgb := ColorToRGB(NewColor);
  SetRValueInRgbTab(GetRValue(Rgb));
  SetGValueInRgbTab(GetGValue(Rgb));
  SetBValueInRgbTab(GetBValue(Rgb));
end;

procedure TCastleColorPickerForm.SetRValueInRgbTab(const NewValue: Integer);
begin
  if RTabColorPicker.Red <> NewValue then
    RTabColorPicker.Red := NewValue;
  if GTabColorPicker.Red <> NewValue then
    GTabColorPicker.Red := NewValue;
  if BTabColorPicker.Red <> NewValue then
    BTabColorPicker.Red := NewValue;
  if SpinEditR.Value <> NewValue then
    SpinEditR.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetGValueInRgbTab(const NewValue: Integer);
begin
  if RTabColorPicker.Green <> NewValue then
    RTabColorPicker.Green := NewValue;
  if GTabColorPicker.Green <> NewValue then
    GTabColorPicker.Green := NewValue;
  if BTabColorPicker.Green <> NewValue then
    BTabColorPicker.Green := NewValue;
  if SpinEditG.Value <> NewValue then
    SpinEditG.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetBValueInRgbTab(const NewValue: Integer);
begin
  if RTabColorPicker.Blue <> NewValue then
    RTabColorPicker.Blue := NewValue;
  if GTabColorPicker.Blue <> NewValue then
    GTabColorPicker.Blue := NewValue;
  if BTabColorPicker.Blue <> NewValue then
    BTabColorPicker.Blue := NewValue;
  if SpinEditB.Value <> NewValue then
    SpinEditB.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetColorInHslTab(const NewColor: TCastleColor);
var
  ColorByte: TVector3Byte;
  HDouble, SDouble, LDouble: Double;
  H, S, L: Integer;
begin
  ColorByte := Vector3Byte(NewColor.XYZ); // edit only Color RGB
  RGBtoHSL(ColorByte[0], ColorByte[1], ColorByte[2], HDouble, SDouble, LDouble);

  H := Round(HDouble * HTabColorPicker.MaxHue);
  S := Round(SDouble * STabColorPicker.MaxSaturation);
  L := Round(LDouble * LTabColorPicker.MaxLuminance);

  SetHValueInHslTab(H);
  SetSValueInHslTab(S);
  SetLValueInHslTab(L);
end;

procedure TCastleColorPickerForm.SetColorInHslTab(const NewColor: TColor);
var
  HDouble, SDouble, LDouble: Double;
  H, S, L: Integer;
begin
  ColortoHSL(NewColor, HDouble, SDouble, LDouble);
  H := Round(HDouble * HTabColorPicker.MaxHue);
  S := Round(SDouble * STabColorPicker.MaxSaturation);
  L := Round(LDouble * LTabColorPicker.MaxLuminance);

  BlockEventsInHslTab;
  try
    SetHValueInHslTab(H);
    SetSValueInHslTab(S);
    SetLValueInHslTab(L);
  finally
    UnblockEventsInHslTab;
  end;
end;

procedure TCastleColorPickerForm.SetHValueInHslTab(const NewValue: Integer);
begin
  if HTabColorPicker.Hue <> NewValue then
    HTabColorPicker.Hue := NewValue;
  if STabColorPicker.Hue <> NewValue then
    STabColorPicker.Hue := NewValue;
  if LTabColorPicker.Hue <> NewValue then
    LTabColorPicker.Hue := NewValue;
  if SpinEditH.Value <> NewValue then
    SpinEditH.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetSValueInHslTab(const NewValue: Integer);
begin
  if HTabColorPicker.Saturation <> NewValue then
    HTabColorPicker.Saturation := NewValue;
  if STabColorPicker.Saturation <> NewValue then
    STabColorPicker.Saturation := NewValue;
  if LTabColorPicker.Saturation <> NewValue then
    LTabColorPicker.Saturation := NewValue;
  if SpinEditS.Value <> NewValue then
    SpinEditS.Value := NewValue;
end;

procedure TCastleColorPickerForm.SetLValueInHslTab(const NewValue: Integer);
begin
  if HTabColorPicker.Luminance <> NewValue then
    HTabColorPicker.Luminance := NewValue;
  if STabColorPicker.Luminance <> NewValue then
    STabColorPicker.Luminance := NewValue;
  if LTabColorPicker.Luminance <> NewValue then
    LTabColorPicker.Luminance := NewValue;
  if SpinEditL.Value <> NewValue then
    SpinEditL.Value := NewValue;
end;

procedure TCastleColorPickerForm.BlockEventsInHslTab;
begin
  HTabColorPicker.OnChange := nil;
  STabColorPicker.OnChange := nil;
  LTabColorPicker.OnChange := nil;
  SpinEditH.OnChange := nil;
  SpinEditS.OnChange := nil;
  SpinEditL.OnChange := nil;
end;

procedure TCastleColorPickerForm.UnblockEventsInHslTab;
begin
  HTabColorPicker.OnChange := @HTabColorPickerChange;
  STabColorPicker.OnChange := @STabColorPickerChange;
  LTabColorPicker.OnChange := @LTabColorPickerChange;
  SpinEditH.OnChange := @SpinEditHChange;
  SpinEditS.OnChange := @SpinEditSChange;
  SpinEditL.OnChange := @SpinEditLChange;
end;

procedure TCastleColorPickerForm.Init(
  const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
begin
  ColorPropertyEditor := ColorPropEditor;
  PrevColor := InitColor;
  SetColorInCirclePickerPanel(InitColor);
  SetColorInRgbTab(InitColor);
  SetColorInHslTab(InitColor);
end;

end.

