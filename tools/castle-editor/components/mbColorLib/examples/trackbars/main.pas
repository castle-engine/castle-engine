unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, BColorPicker, GColorPicker, RColorPicker, CColorPicker,
  YColorPicker, MColorPicker, KColorPicker, HColorPicker, SColorPicker,
  LVColorPicker, mbColorPreview;

type

  { TForm1 }

  TForm1 = class(TForm)
    BColorPickerV: TBColorPicker;
    BColorPickerH: TBColorPicker;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    CColorPickerH: TCColorPicker;
    CColorPickerV: TCColorPicker;
    GColorPickerV: TGColorPicker;
    GColorPickerH: TGColorPicker;
    HColorPickerH: THColorPicker;
    HColorPickerV: THColorPicker;
    KColorPickerH: TKColorPicker;
    KColorPickerV: TKColorPicker;
    LblR: TLabel;
    lblLVv: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    lblLVh: TLabel;
    Label3: TLabel;
    LblC: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LblH: TLabel;
    Label9: TLabel;
    CMYKh: TmbColorPreview;
    HSLVh: TmbColorPreview;
    Panel1: TPanel;
    Panel2: TPanel;
    RGBv: TmbColorPreview;
    CMYKv: TmbColorPreview;
    HSLVv: TmbColorPreview;
    rbHSLv: TRadioButton;
    rbHSVv: TRadioButton;
    rbHSVh: TRadioButton;
    rbHSLh: TRadioButton;
    RGBh: TmbColorPreview;
    MColorPickerH: TMColorPicker;
    MColorPickerV: TMColorPicker;
    PageControl1: TPageControl;
    RColorPickerV: TRColorPicker;
    RColorPickerH: TRColorPicker;
    SColorPickerH: TSColorPicker;
    SColorPickerV: TSColorPicker;
    tabVertical: TTabSheet;
    tabHorizontal: TTabSheet;
    LVColorPickerH: TLVColorPicker;
    LVColorPickerV: TLVColorPicker;
    YColorPickerH: TYColorPicker;
    YColorPickerV: TYColorPicker;
    procedure CMYKPickerV_Change(Sender: TObject);
    procedure CMYKPickerH_Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HPickerH_Change(Sender: TObject);
    procedure HPickerV_Change(Sender: TObject);
    procedure rbHSLv_Change(Sender: TObject);
    procedure rbHSLh_Change(Sender: TObject);
    procedure RGBPickerH_Change(Sender: TObject);
    procedure RGBPickerV_Change(Sender: TObject);
    procedure SLVPickerH_Change(Sender: TObject);
    procedure SLVPickerV_Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

const
  MaxHue = 360;
  MaxSat = 255;
  MaxLum = 255;
  MaxVal = 255;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, ScanLines, RGBCMYKUtils, mbColorConv;

{ TForm1 }

procedure TForm1.CMYKPickerH_Change(Sender: TObject);
var
  c: TColor;
begin
  if (CColorPickerH = nil) or (YColorPickerH = nil) or (MColorPickerH = nil) or
     (KColorPickerH = nil) or (CMYKh = nil) then
    exit;
  CMYKh.Color := CMYKToColor(
    CColorPickerH.Cyan,
    MColorPickerH.Magenta,
    YColorPickerH.Yellow,
    KColorPickerH.Black
  );

  c := CMYKh.Color;
  CMYKh.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYValue(c), GetKValue(c),
    round(GetRelHValue(c)*MaxHue), round(GetRelSValueHSL(c)*MaxSat),
    round(GetRelLValue(c)*MaxLum), round(GetRelVValue(c)*MaxVal)
  ]);
end;

procedure TForm1.CMYKPickerV_Change(Sender: TObject);
var
  c: TColor;
begin
  if (CColorPickerV = nil) or (YColorPickerV = nil) or (MColorPickerV = nil) or
     (KColorPickerV = nil) or (CMYKv = nil) then
    exit;
  CMYKv.Color := CMYKToColor(
    CColorPickerV.Cyan,
    MColorPickerV.Magenta,
    YColorPickerV.Yellow,
    KColorPickerV.Black
  );

  c := CMYKv.Color;
  CMYKv.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    round(GetRelHValue(c)*MaxHue), round(GetRelSValueHSL(c)*MaxSat),
    round(GetRelLValue(c)*MaxLum), round(GetRelVValue(c)*MaxVal)
  ]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RGBPickerH_Change(nil);
  CMYKPickerH_Change(nil);
  SLVPickerH_Change(nil);

  RGBPickerV_Change(nil);
  CMYKPickerV_Change(nil);
  SLVPickerV_Change(nil);
end;

procedure TForm1.HPickerH_Change(Sender: TObject);
begin
  if ComponentState <> [] then
    exit;

  SLVPickerH_Change(nil);
  SColorPickerH.Hue := HColorPickerH.Hue;
  LVColorPickerH.Hue := HColorPickerH.Hue;
end;

procedure TForm1.HPickerV_Change(Sender: TObject);
begin
  if ComponentState <> [] then
    exit;
  SLVPickerV_Change(nil);
  SColorPickerV.Hue := HColorPickerV.Hue;
  LVColorPickerV.Hue := HColorPickerV.Hue;
end;

procedure TForm1.rbHSLv_Change(Sender: TObject);
begin
  if rbHSLv.Checked then
  begin
    lblLVv.Caption := 'L';
    HColorPickerV.BrightnessMode := bmLuminance;
    SColorPickerV.BrightnessMode := bmLuminance;
    LVColorPickerV.BrightnessMode := bmLuminance;
  end;
  if rbHSVv.Checked then
  begin
    lblLVv.Caption := 'V';
    HColorPickerV.BrightnessMode := bmValue;
    SColorPickerV.BrightnessMode := bmValue;
    LVColorPickerV.BrightnessMode := bmValue;
  end;
  HPickerV_Change(nil);
end;

procedure TForm1.rbHSLh_Change(Sender: TObject);
begin
  if rbHSLh.Checked then
  begin
    lblLVh.Caption := 'L';
    HColorPickerH.BrightnessMode := bmLuminance;
    SColorPickerH.BrightnessMode := bmLuminance;
    LVColorPickerH.BrightnessMode := bmLuminance;
  end;
  if rbHSVh.Checked then
  begin
    lblLVh.Caption := 'V';
    HColorPickerH.BrightnessMode := bmValue;
    SColorPickerH.BrightnessMode := bmValue;
    LVColorPickerH.BrightnessMode := bmValue;
  end;
  HPickerH_Change(nil);
end;

procedure TForm1.RGBPickerH_Change(Sender: TObject);
var
  c: TColor;
begin
  if (RColorPickerH = nil) or (GColorPickerH = nil) or (BColorPickerH = nil) or (RGBh = nil) then
    exit;
  RGBh.Color := RGBToColor(
    RColorPickerH.Red,
    GColorPickerH.Green,
    BColorPickerH.Blue
  );

  c := RGBh.Color;
  RGBh.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    round(GetRelHValue(c)*MaxHue), round(GetRelSValueHSL(c)*MaxSat),
    round(GetRelLValue(c)*MaxLum), round(GetRelVValue(c)*MaxVal)
  ]);
end;

procedure TForm1.RGBPickerV_Change(Sender: TObject);
var
  c: TColor;
begin
  if (RColorPickerV = nil) or (GColorPickerV = nil) or (BColorPickerV = nil) or (RGBv = nil) then
    exit;
  RGBv.Color := RGBToColor(
    RColorPickerV.Red,
    GColorPickerV.Green,
    BColorPickerV.Blue
  );

  c := RGBv.Color;
  RGBv.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    round(GetRelHValue(c)*MaxHue), round(GetRelSValueHSL(c)*MaxSat),
    round(GetRelLValue(c)*MaxLum), round(GetRelVValue(c)*MaxVal)
  ]);
end;

procedure TForm1.SLVPickerH_Change(Sender: TObject);
var
  triple: TRGBTriple;
  c: TColor;
begin
  if (HSLVh = nil) or (HColorPickerH = nil) or (SColorPickerH = nil) or
    (LVColorPickerH = nil)
  then
    exit;

  if rbHSLh.Checked then
    HSLVh.Color := HSLToColor(HColorPickerH.RelHue, SColorPickerH.RelSaturation, LVColorPickerH.RelLuminance);

  if rbHSVh.Checked then
    HSLVh.Color := HSVToColor(HColorPickerH.RelHue, SColorPickerH.RelSaturation, LVColorPickerH.RelValue);

  c := HSLVh.Color;
  HSLVh.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    round(GetRelHValue(c)*MaxHue), round(GetRelSValueHSL(c)*MaxSat),
    round(GetRelLValue(c)*MaxLum), round(GetRelVValue(c)*MaxVal)
  ]);
end;

procedure TForm1.SLVPickerV_Change(Sender: TObject);
var
  c: TColor;
begin
  if (HSLVv = nil) or (HColorPickerV = nil) or (SColorPickerV = nil) or
     (LVColorPickerV = nil)
  then
    exit;

  if rbHSLv.Checked then begin
    if (LVColorPickerV = nil) then
      exit;
    c := HSLToColor(HColorPickerV.RelHue, SColorPickerV.RelSaturation, LVColorPickerV.RelLuminance);
    HSLVv.Color := c;
  end;
  if rbHSVv.Checked then begin
    if (LVColorPickerV = nil) then
      exit;
    c := HSVtoColor(HColorPickerV.RelHue, SColorPickerV.RelSaturation, LVColorPickerV.RelValue);
    HSLVv.Color := c;
  end;

  c := HSLVv.Color;
  HSLVv.Hint := Format('Red: %d - Green: %d - Blue: %d'#13 +
    'Cyan: %d - Magenta: %d - Yellow: %d - Black: %d'#13 +
    'Hue: %d - Saturation: %d - Luminance: %d - Value: %d', [
    GetRValue(c), GetGValue(c), GetBValue(c),
    GetCValue(c), GetMValue(c), GetYvalue(c), GetKValue(c),
    round(GetRelHValue(c)*MaxHue), round(GetRelSValueHSL(c)*MaxSat),
    round(GetRelLValue(c)*MaxLum), round(GetRelVValue(c)*MaxVal)
  ]);
end;

end.

