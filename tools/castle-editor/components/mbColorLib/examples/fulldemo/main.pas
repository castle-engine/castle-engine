unit main;

interface


uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HSLColorPicker, ComCtrls, StdCtrls, ExtCtrls, mbColorPreview,
  HexaColorPicker, mbColorPalette, HSLRingPicker, HSCirclePicker, PalUtils,
  SLHColorPicker, mbDeskPickerButton, mbOfficeColorDialog, SColorPicker,
  HColorPicker, LVColorPicker, mbTrackBarPicker, HRingPicker, SLColorPicker,
  HSColorPicker, IniFiles, mbColorPickerControl, BColorPicker, GColorPicker,
  RColorPicker, KColorPicker, YColorPicker, MColorPicker, CColorPicker,
  CIEBColorPicker, CIEAColorPicker, Typinfo, CIELColorPicker, BAxisColorPicker,
  GAxisColorPicker, RAxisColorPicker, mbColorTree, mbColorList;

  { vmbColorList, mbBasicPicker, }

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    CbShowHints: TCheckBox;
    CbEnabled: TCheckBox;
    LblGAxisPicker: TLabel;
    LblRAxisPicker: TLabel;
    LblBAxisPicker: TLabel;
    LblCIELColorPicker: TLabel;
    LblCIEAColorPicker: TLabel;
    LblCIEBColorPicker: TLabel;
    PageControl1: TPageControl;
    RightPanel: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    HSLColorPicker1: THSLColorPicker;
    sc: TmbColorPreview;
    uc: TmbColorPreview;
    LblSelectedColor: TLabel;
    tb1: TTrackBar;
    tb2: TTrackBar;
    LblColorUnderCursor: TLabel;
    HexaColorPicker1: THexaColorPicker;
    mbColorPalette1: TmbColorPalette;
    BtnBluePalette: TButton;
    BtnGradientPal: TButton;
    HSLRingPicker1: THSLRingPicker;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    HSCirclePicker1: THSCirclePicker;
    SLHColorPicker1: TSLHColorPicker;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    mbDeskPickerButton1: TmbDeskPickerButton;
    mbOfficeColorDialog1: TmbOfficeColorDialog;
    OfficeColorDialogButton: TButton;
    LVColorPicker2: TLVColorPicker;
    LVColorPicker3: TLVColorPicker;
    HColorPicker1: THColorPicker;
    SColorPicker1: TSColorPicker;
    HSColorPicker1: THSColorPicker;
    SLColorPicker1: TSLColorPicker;
    HRingPicker1: THRingPicker;
    CheckBox1: TCheckBox;
    CbMarker: TComboBox;
    Label4: TLabel;
    CheckBox2: TCheckBox;
    LblInfo: TLabel;
    BtnLoadPal: TButton;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
    LblSort: TLabel;
    CbSortDir: TComboBox;
    CbSortMode: TComboBox;
    LblStyle: TLabel;
    CbStyle: TComboBox;
    LblSize: TLabel;
    udSize: TUpDown;
    TabSheet9: TTabSheet;
    CColorPicker1: TCColorPicker;
    MColorPicker1: TMColorPicker;
    LVColorPicker1: TLVColorPicker;
    YColorPicker1: TYColorPicker;
    KColorPicker1: TKColorPicker;
    Label8: TLabel;
    RColorPicker1: TRColorPicker;
    GColorPicker1: TGColorPicker;
    BColorPicker1: TBColorPicker;
    KColorPicker2: TKColorPicker;
    MColorPicker2: TMColorPicker;
    CColorPicker2: TCColorPicker;
    YColorPicker2: TYColorPicker;
    TabSheet10: TTabSheet;
    RAxisColorPicker1: TRAxisColorPicker;
    GAxisColorPicker1: TGAxisColorPicker;
    BAxisColorPicker1: TBAxisColorPicker;
    CIELColorPicker1: TCIELColorPicker;
    CIEAColorPicker1: TCIEAColorPicker;
    CIEBColorPicker1: TCIEBColorPicker;
    CbWebSsafe: TCheckBox;
    TabSheet11: TTabSheet;
    mbColorList1: TmbColorList;
    mbColorTree1: TmbColorTree;
    Button5: TButton;
    Memo1: TMemo;
    Label9: TLabel;
    CbSwatchStyle: TCheckBox;
    procedure CbEnabledChange(Sender: TObject);
    procedure CbShowHintsChange(Sender: TObject);
    procedure HColorPicker1GetHintStr(Sender: TObject; X, Y: Integer;
      var AText: String);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure tb1Change(Sender: TObject);
    procedure tb2Change(Sender: TObject);
    procedure HSLColorPicker1Change(Sender: TObject);
    procedure HSLColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HexaColorPicker1Change(Sender: TObject);
    procedure HexaColorPicker1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnBluePaletteClick(Sender: TObject);
    procedure BtnGradientPalClick(Sender: TObject);
    procedure mbColorPalette1SelColorChange(Sender: TObject);
    procedure mbColorPalette1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HSLRingPicker1Change(Sender: TObject);
    procedure HSLRingPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HSVColorPicker1Change(Sender: TObject);
    procedure SLHColorPicker1Change(Sender: TObject);
    procedure SLHColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure mbDeskPickerButton1SelColorChange(Sender: TObject);
    procedure OfficeColorDialogButtonClick(Sender: TObject);
    procedure HSColorPicker1Change(Sender: TObject);
    procedure HSColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SLColorPicker1Change(Sender: TObject);
    procedure SLColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HRingPicker1Change(Sender: TObject);
    procedure HRingPicker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure udSizeChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure LVColorPicker1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CbMarkerChange(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BtnLoadPalClick(Sender: TObject);
    procedure CbSortDirChange(Sender: TObject);
    procedure CbSortModeChange(Sender: TObject);
    procedure CbStyleChange(Sender: TObject);
    procedure CbWebSsafeClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CbSwatchStyleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
{$R mxico.res} //MXS icon resource file, for internet shortcut only

uses
  mbColorConv;
//  RGBHSLUtils;

procedure TForm1.tb1Change(Sender: TObject);
begin
  sc.opacity := tb1.position;
end;

procedure TForm1.tb2Change(Sender: TObject);
begin
  uc.opacity := tb2.position;
end;

procedure TForm1.HSLColorPicker1Change(Sender: TObject);
begin
  sc.color := HSLColorPicker1.SelectedColor;
end;

procedure TForm1.HSLColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := HSLColorPicker1.ColorUnderCursor;
end;

procedure TForm1.HexaColorPicker1Change(Sender: TObject);
begin
  sc.color := hexacolorpicker1.selectedcolor;
end;

procedure TForm1.HexaColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := hexacolorpicker1.ColorUnderCursor;
end;

procedure TForm1.BtnBluePaletteClick(Sender: TObject);
begin
  mbColorPalette1.GeneratePalette(clblue);
end;

procedure TForm1.BtnGradientPalClick(Sender: TObject);
begin
  mbColorpalette1.GenerateGradientPalette([clblue, clred]);
end;

procedure TForm1.mbColorPalette1SelColorChange(Sender: TObject);
begin
  sc.Color := mbColorPalette1.SelectedColor;
end;

procedure TForm1.mbColorPalette1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := mbcolorpalette1.ColorUnderCursor;
end;

procedure TForm1.HSLRingPicker1Change(Sender: TObject);
begin
  sc.color := HSLRingPicker1.SelectedColor;
end;

procedure TForm1.HSLRingPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := HSLRingPicker1.ColorUnderCursor;
end;

procedure TForm1.HSVColorPicker1Change(Sender: TObject);
begin
  LVColorPicker1.Saturation := HSCirclePicker1.Saturation;
  LVColorPicker1.Hue := HSCirclePicker1.Hue;
  sc.color := LVColorPicker1.SelectedColor;
end;

procedure TForm1.SLHColorPicker1Change(Sender: TObject);
begin
  sc.color := SLHColorPicker1.SelectedColor;
end;

procedure TForm1.SLHColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := SLHColorPicker1.ColorUnderCursor;
end;

procedure TForm1.mbDeskPickerButton1SelColorChange(Sender: TObject);
begin
  sc.color := mbDeskPickerButton1.SelectedColor;
  uc.color := mbDeskPickerButton1.SelectedColor;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  CbEnabledChange(nil);
end;

procedure TForm1.PageControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  uc.color := HSLColorpicker1.ColorUnderCursor;
end;

procedure TForm1.OfficeColorDialogButtonClick(Sender: TObject);
begin
  if mbOfficeColorDialog1.Execute then
    sc.color := mbOfficeColorDialog1.SelectedColor;
end;

procedure TForm1.HSColorPicker1Change(Sender: TObject);
begin
  sc.color := HSColorPicker1.SelectedColor;
end;

procedure TForm1.HSColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := HSColorpicker1.ColorUnderCursor;
end;

procedure TForm1.SLColorPicker1Change(Sender: TObject);
begin
  sc.color := SLColorPicker1.SelectedColor;
end;

procedure TForm1.SLColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  uc.color := slcolorpicker1.ColorUnderCursor;
end;

procedure TForm1.HRingPicker1Change(Sender: TObject);
begin
  sc.color := hringpicker1.SelectedColor;
end;

procedure TForm1.HRingPicker1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  uc.color := hringpicker1.ColorUnderCursor;
end;

procedure TForm1.LVColorPicker1Change(Sender: TObject);
begin
  if (sc = nil) or (uc = nil) or (LVColorPicker1 = nil) or (HSCirclePicker1 = nil) then
    exit;
  LVColorPicker1.Saturation := HSCirclePicker1.Saturation;
  LVColorPicker1.Hue := HSCirclePicker1.Hue;
  sc.Color := LVColorPicker1.SelectedColor;
  uc.Color := HSVtoColor(HSCirclePicker1.RelHue, HSCirclePicker1.RelSaturation, HSCirclePicker1.RelValue);
end;

// only for internet shortcuts
procedure TForm1.FormCreate(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + '\MXS Website.url') do
  try
    WriteString('InternetShortcut','URL', 'http://mxs.bergsoft.net');
    WriteInteger('InternetShortcut','IconIndex', 1);
    WriteString('InternetShortcut','IconFile', '"' + Application.ExeName + '"');
  finally
    Free;
  end;
end;

procedure TForm1.HColorPicker1GetHintStr(Sender: TObject; X, Y: Integer;
  var AText: String);
begin
  AText := FormatHint(HColorPicker1.HintFormat, HColorPicker1.GetColorAtPoint(X, Y));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  HexaColorPicker1.SliderVisible := checkbox1.Checked;
end;

procedure TForm1.CbMarkerChange(Sender: TObject);
begin
  hexacolorpicker1.SliderMarker := TMArker(CbMarker.ItemIndex);
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  hexacolorpicker1.NewArrowStyle := checkbox2.checked;
end;

procedure TForm1.BtnLoadPalClick(Sender: TObject);
begin
  if opendialog1.Execute then
    mbcolorpalette1.Palette := opendialog1.FileName;
end;

procedure TForm1.CbSortDirChange(Sender: TObject);
begin
  mbcolorpalette1.SortOrder := tsortorder(CbSortDir.itemindex);
end;

procedure TForm1.CbSortModeChange(Sender: TObject);
begin
  mbcolorpalette1.Sortmode := tsortmode(CbSortMode.ItemIndex);
end;

procedure TForm1.CbStyleChange(Sender: TObject);
begin
  mbcolorpalette1.CellStyle := tcellstyle(CbStyle.ItemIndex);
end;

procedure TForm1.udSizeChangingEx(Sender: TObject; var AllowChange: Boolean;
  NewValue: SmallInt; Direction: TUpDownDirection);
begin
  allowchange := true;
  mbcolorpalette1.CellSize := abs(NewValue);
end;

procedure TForm1.CbWebSsafeClick(Sender: TObject);
var
 i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if IsPublishedProp(components[i], 'WebSafe') = true then
      SetOrdProp(components[i], 'WebSafe', integer(CbWebSsafe.checked));
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i: integer;
begin
  mbcolortree1.ClearColors;
  mbcolorlist1.ClearColors;
  for i := 0 to mbcolorpalette1.Colors.Count - 1 do
  begin
     mbcolortree1.AddColor('Color '+inttostr(i), StringtoColor(mbcolorpalette1.colors.Strings[i]), false);
     mbcolorlist1.AddColor('Color '+inttostr(i), StringtoColor(mbcolorpalette1.colors.Strings[i]), false);
  end;
  mbcolortree1.UpdateColors;
  mbcolorlist1.UpdateColors;
end;

procedure TForm1.CbEnabledChange(Sender: TObject);
begin
  if PageControl1.ActivePage = Tabsheet1 then
    HSLColorPicker1.Enabled := CbEnabled.Checked
  else if PageControl1.ActivePage = Tabsheet2 then
    HexaColorPicker1.Enabled := CbEnabled.Checked
  else if PageControl1.ActivePage = Tabsheet3 then
    mbColorPalette1.Enabled := CbEnabled.Checked
  else if PageControl1.ActivePage = Tabsheet4 then
    HSLRingPicker1.Enabled := CbEnabled.Checked
  else if PageControl1.ActivePage = Tabsheet5 then
  begin
    HSCirclePicker1.Enabled := CbEnabled.Checked;
    LVColorPicker1.Enabled := CbEnabled.Checked;
  end
  else if PageControl1.ActivePage = Tabsheet6 then
    SLHColorPicker1.Enabled := CbEnabled.Checked
  else if PageControl1.ActivePage = Tabsheet11 then
  begin
    mbColorList1.Enabled := CbEnabled.Checked;
    mbColorTree1.Enabled := CbEnabled.Checked;
  end
  else if PageControl1.ActivePage = Tabsheet7 then
  begin
    mbDeskPickerButton1.Enabled := CbEnabled.Checked;
    OfficeColorDialogButton.Enabled := CbEnabled.Checked;
    LVColorPicker2.Enabled := CbEnabled.Checked;
    LVColorPicker3.Enabled := CbEnabled.Checked;
    HColorPicker1.Enabled := CbEnabled.Checked;
    SColorPicker1.Enabled := CbEnabled.Checked;
  end
  else if PageControl1.ActivePage = Tabsheet8 then
  begin
    HSColorPicker1.Enabled := CbEnabled.Checked;
    SLColorPicker1.Enabled := CbEnabled.Checked;
    HRingPicker1.Enabled := CbEnabled.Checked;
  end
  else if PageControl1.ActivePage = Tabsheet9 then
  begin
    CColorPicker1.Enabled := CbEnabled.Checked;
    MColorPicker1.Enabled := CbEnabled.Checked;
    YColorPicker1.Enabled := CbEnabled.Checked;
    KColorPicker1.Enabled := CbEnabled.Checked;
    RColorPicker1.Enabled := CbEnabled.Checked;
    BColorPicker1.Enabled := CbEnabled.Checked;
    GColorPicker1.Enabled := CbEnabled.Checked;
    KColorPicker2.Enabled := CbEnabled.Checked;
    MColorPicker2.Enabled := CbEnabled.Checked;
    CColorPicker2.Enabled := CbEnabled.Checked;
    YColorPicker2.Enabled := CbEnabled.Checked;
  end
  else if PageControl1.ActivePage = Tabsheet10 then
  begin
    RAxisColorPicker1.Enabled := CbEnabled.Checked;
    GAxisColorPicker1.Enabled := CbEnabled.Checked;
    BAxisColorPicker1.Enabled := CbEnabled.Checked;
    CIELColorPicker1.Enabled := CbEnabled.Checked;
    CIEAColorPicker1.Enabled := CbEnabled.Checked;
    CIEBColorPicker1.Enabled := CbEnabled.Checked;
  end;
end;

procedure TForm1.CbSwatchStyleClick(Sender: TObject);
begin
  sc.swatchstyle := CbSwatchStyle.Checked;
  uc.swatchstyle := CbSwatchStyle.checked;
end;

procedure TForm1.CbShowHintsChange(Sender: TObject);
begin
  PageControl1.ShowHint := CbShowHints.Checked;
  mbOfficeColorDialog1.UseHints := CbShowHints.Checked;
  mbDeskPickerButton1.ShowScreenHint := CbShowHints.Checked;
end;

end.
