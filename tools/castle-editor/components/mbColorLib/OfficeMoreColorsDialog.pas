unit OfficeMoreColorsDialog;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls,
  HexaColorPicker, HSLColorPicker, mbColorConv, mbColorPreview,
  {$IFDEF mbXP_Lib}mbXPSpinEdit, mbXPSizeGrip,{$ELSE} Spin,{$ENDIF}
  HTMLColors, SLHColorPicker, HSLRingPicker, RColorPicker, GColorPicker,
  BColorPicker;

type

  { TOfficeMoreColorsWin }

  TOfficeMoreColorsWin = class(TForm)
    Bevel2: TBevel;
    BTrackbar: TBColorPicker;
    Bevel1: TBevel;
    EdGREEN: TSpinEdit;
    EdBLUE: TSpinEdit;
    EdHUE: TSpinEdit;
    EdSAT: TSpinEdit;
    EdLUMVAL: TSpinEdit;
    GTrackbar: TGColorPicker;
    HSLRing: THSLRingPicker;
    LblR: TLabel;
    LblG: TLabel;
    LblB: TLabel;
    LblCurrent: TLabel;
    LblLumVal: TLabel;
    LblSat: TLabel;
    LblHue: TLabel;
    nbRGB: TPage;
    Panel1: TPanel;
    SidePanel: TPanel;
    PickerNotebook: TNotebook;
    nbHSL: TPage;
    nbHSLRing: TPage;
    nbSLH: TPage;
    Pages: TPageControl;
    RTrackbar: TRColorPicker;
    SLH: TSLHColorPicker;
    EdRED: TSpinEdit;
    Standard: TTabSheet;
    Custom: TTabSheet;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    LblCustomColors: TLabel;
    LblStandardColors: TLabel;
    LblPicker: TLabel;
    cbColorDisplay: TComboBox;
    LblRed: TLabel;
    LblGreen: TLabel;
    LblBlue: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    NewSwatch: TmbColorPreview;
    OldSwatch: TmbColorPreview;
    procedure cbColorDisplayChange(Sender: TObject);
    procedure ColorPickerChange(Sender: TObject);
    procedure EdBlueChange(Sender: TObject);
    procedure EdGreenChange(Sender: TObject);
    procedure EdHueChange(Sender: TObject);
    procedure EdLumValChange(Sender: TObject);
    procedure EdRedChange(Sender: TObject);
    procedure EdSatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetHint(c: TColor): string;
    procedure HexaChange(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure HSLRingChange(Sender: TObject);
    procedure nbHSLRingResize(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure PagesChanging(Sender: TObject; var {%H-}AllowChange: Boolean);
    procedure SLHChange(Sender: TObject);
  private
    FMaxHue: Integer;
    FMaxSat: Integer;
    FMaxLum: Integer;
    FMaxVal: Integer;
    FSelectedColor: TColor;
    FBrightnessMode: TBrightnessMode;
    FLockChange: Integer;
    function GetPickerIndex: Integer;
    function GetSelectedColor: TColor;
    function GetShowHint: Boolean;
    procedure SetAllCustom(c: TColor);
    procedure SetAllToSel(c: TColor);
    procedure SetBrightnessMode(AMode: TBrightnessMode);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxVal(V: Integer);
    procedure SetPickerIndex(AValue: Integer);
    procedure SetSelectedColor(c: TColor);
    procedure SetShowHint(AValue: boolean);
  protected
    procedure BeginUpdate;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure EndUpdate;
  public
    property PickerIndex: Integer read GetPickerIndex write SetPickerIndex;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property MaxHue: Integer read FMaxHue write SetMaxHue;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum;
    property MaxValue: Integer read FMaxVal write SetMaxVal;
  published
    property ShowHint: Boolean read GetShowHint write SetShowHint;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;

implementation

{$R *.lfm}

procedure TOfficeMoreColorsWin.BeginUpdate;
begin
  inc(FLockChange);
end;

procedure TOfficeMoreColorsWin.ColorPickerChange(Sender: TObject);
begin
  if FLockChange > 0 then
    exit;

  if Sender = HSL then
    SetAllCustom(HSL.SelectedColor);
  if Sender = HSLRing then
    SetAllCustom(HSLRing.SelectedColor);
  if Sender = SLH then
    SetAllCustom(SLH.SelectedColor);
  if Sender = RTrackbar then
    SetAllCustom(RTrackbar.SelectedColor);
  if Sender = GTrackbar then
    SetAllCustom(GTrackbar.SelectedColor);
  if Sender = BTrackbar then
    SetAllCustom(BTrackbar.SelectedColor);
end;

procedure TOfficeMoreColorsWin.CreateParams(var Params: TCreateParams);
begin 
  inherited CreateParams(Params); 
  Params.Style := WS_CAPTION or WS_SIZEBOX or WS_SYSMENU; 
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE; 
end; 

procedure TOfficeMoreColorsWin.cbColorDisplayChange(Sender: TObject);
begin
  PickerNotebook.PageIndex := cbColorDisplay.ItemIndex;
  SetAllCustom(NewSwatch.Color);
  {
  HSL.Visible := cbColorDisplay.ItemIndex = 0;
  HSLRing.Visible := cbColorDisplay.ItemIndex = 1;
  SLH.Visible := cbColorDisplay.ItemIndex = 2;
  }
  if HSL.Visible then
    HSL.SelectedColor := NewSwatch.Color;
  if HSLRing.Visible then
    HSLRing.SelectedColor := NewSwatch.Color;
  if SLH.Visible then
    SLH.SelectedColor := NewSwatch.Color;
end;

procedure TOfficeMoreColorsWin.EdBlueChange(Sender: TObject);
begin
  if (EdBlue.Text <> '') and EdBlue.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Blue := EdBlue.Value;
      SLH.Blue := EdBlue.Value;
      NewSwatch.Color := RGB(EdRed.Value, EdGreen.Value, EdBlue.Value);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EdGreenChange(Sender: TObject);
begin
  if (EdGreen.Text <> '') and EdGreen.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Green := EdGreen.Value;
      SLH.Green := EdGreen.Value;
      NewSwatch.Color := RGB(EdRed.Value, EdGreen.Value, EdBlue.Value);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EdHueChange(Sender: TObject);
begin
  if (EdHue.Text <> '') and EdHue.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Hue := EdHue.Value;
      SLH.Hue := EdHue.Value;
      case FBrightnessMode of
        bmLuminance:
          NewSwatch.Color := HSLToColor(EdHue.Value/FMaxHue, EdSat.Value/FMaxSat, EdLumVal.Value/FMaxLum);
        bmValue:
          NewSwatch.Color := HSVtoColor(EdHue.Value/FMaxHue, EdSat.Value/FMaxSat, EdLumVal.Value/FMaxVal);
      end;
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EdLumValChange(Sender: TObject);
begin
  if (EdLumVal.Text <> '') and EdLumVal.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Luminance := EdLumVal.Value;
      case FBrightnessMode of
        bmLuminance:
          NewSwatch.Color := HSLToColor(EdHue.Value/FMaxHue, EdSat.Value/FMaxSat, EdLumVal.Value/FMaxLum);
        bmValue:
          NewSwatch.Color := HSVtoColor(EdHue.Value/FMaxHue, EdSat.Value/FMaxSat, EdLumVal.Value/FMaxVal);
      end;
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EndUpdate;
begin
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.EdRedChange(Sender: TObject);
begin
  if (EdRed.Text <> '') and EdRed.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Red := EdRed.Value;
      SLH.Red := EdRed.Value;
      NewSwatch.Color := RGB(EdRed.Value, EdGreen.Value, EdBlue.Value);
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.EdSatChange(Sender: TObject);
begin
  if (EdSat.Text <> '') and EdSat.Focused and (FLockChange = 0) then
  begin
    inc(FLockChange);
    try
      HSL.Saturation := EdSat.Value;
      SLH.Saturation := EdSat.Value;
      case FBrightnessMode of
        bmLuminance:
          NewSwatch.Color := HSLToColor(EdHue.Value/FMaxHue, EdSat.Value/FMaxSat, EdLumval.Value/FMaxLum);
        bmValue:
          NewSwatch.Color := HSVtoColor(EdHue.Value/FMaxHue, EdSat.Value/FMaxSat, EdLumval.Value/FMaxVal);
      end;
    finally
      dec(FLockChange);
    end;
  end;
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
const
  DESIGN_DPI = 96;
begin
  Width := ScaleX(Width, DESIGN_DPI);
  Height := ScaleY(Height, DESIGN_DPI);
  RTrackbar.Height := ScaleY(RTrackbar.Height, DESIGN_DPI);
  BTrackbar.Height := RTrackbar.Height;
  GTrackbar.Height := RTrackbar.Height;

  FBrightnessMode := bmLuminance;

  FMaxHue := 360;
  FMaxSat := 255;
  FMaxLum := 255;
  FMaxVal := 255;

  HSL.MaxHue := FMaxHue;
  HSL.MaxSaturation := FMaxSat;
  HSL.MaxLuminance := FMaxLum;
  HSL.BrightnessMode := FBrightnessMode;

  HSLRing.MaxHue := FMaxHue;
  HSLRing.MaxSaturation := FMaxSat;
  HSLRing.MaxLuminance := FMaxLum;
  HSLRing.BrightnessMode := FBrightnessMode;

  SLH.MaxHue := FMaxHue;
  SLH.MaxSaturation := FMaxSat;
  SLH.MaxLuminance := FMaxLum;
  SLH.BrightnessMode := FBrightnessMode;
  (*
 {$IFDEF mbXP_Lib}
  ERed := TmbXPSpinEdit.CreateParented(Custom.Handle);
  EGreen := TmbXPSpinEdit.CreateParented(Custom.Handle);
  EBlue := TmbXPSpinEdit.CreateParented(Custom.Handle);
  grip := TmbXPSizeGrip.CreateParented(Self.Handle);
 {$ELSE}
  ERed := TSpinEdit.CreateParented(Custom.Handle);
  EGreen := TSpinEdit.CreateParented(Custom.Handle);
  EBlue := TSpinEdit.CreateParented(Custom.Handle);
  EHue := TSpinEdit.CreateParented(Custom.Handle);
  ESat := TSpinEdit.CreateParented(Custom.Handle);
  ELumVal := TSpinEdit.CreateParented(Custom.Handle);
 {$ENDIF}
  with ERed do
  begin
    Name := 'ERed';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left;
    Top := LblRed.Top - 4;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := 255;
    MinValue := 0;
    Value := 0;
    OnChange := @ERedChange;
//   TabOrder := cbColorDisplay.TabOrder + 1;
  end;
  with EGreen do
  begin
    Name := 'EGreen';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left;
    Top := LblGreen.Top - 3;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := 255;
    MinValue := 0;
    Value := 0;
    OnChange := @EGreenChange;
//   TabOrder := ERed.TabOrder + 1;
  end;
  with EBlue do
  begin
    Name := 'EBlue';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left;
    Top := LblBlue.Top - 4;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := 255;
    MinValue := 0;
    Value := 0;
    OnChange := @EBlueChange;
//   TabOrder := EGreen.TabOrder + 1;
  end;
  with EHue do
  begin
    Name := 'EHue';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := ERed.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxHue;
    MinValue := 0;
    Value := 0;
    OnChange := @EHueChange;
//   TabOrder := EBlue.TabOrder + 1;
  end;
  with ESat do
  begin
    Name := 'ESat';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := EGreen.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxSat;
    MinValue := 0;
    Value := 0;
    OnChange := @ESatChange;
//   TabOrder := EHue.TabOrder + 1;
  end;
  with ELumVal do
  begin
    Name := 'ELumVal';
    Width := 47;
    Height := 22;
    Left := cbColorDisplay.Left + cbColorDisplay.Width - Width;
    Top := EBlue.Top;
    Alignment := taRightJustify;
    Anchors := [akLeft, akBottom];
    MaxValue := FMaxLum;
    MinValue := 0;
    Value := 0;
    OnChange := @ELumValChange;
//   TabOrder := ESat.TabOrder + 1;
  end;
  Custom.InsertControl(ERed);
  Custom.InsertControl(EGreen);
  Custom.InsertControl(EBlue);
  Custom.InsertControl(EHue);
  Custom.InsertControl(ESat);
  Custom.InsertControl(ELumVal);

 {$IFDEF mbXP_Lib}
  with grip do
  begin
    Name := 'grip';
    Width := 15;
    Height := 15;
    Left := 308;
    Top := 314;
    Anchors := [akRight, akBottom];
  end;
  InsertControl(grip);
 {$ENDIF}
                   *)
  BtnOK.TabOrder := EdLumVal.TabOrder + 1;
  BtnCancel.TabOrder := BtnOK.TabOrder + 1;
end;

procedure TOfficeMoreColorsWin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ModalResult := mrOK;
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TOfficeMoreColorsWin.FormResize(Sender: TObject);
begin
{$IFDEF mbXP_Lib}
  grip.Left := ClientWidth - 15;
  grip.Top := ClientHeight - 15;
{$ENDIF}
end;

procedure TOfficeMoreColorsWin.FormShow(Sender: TObject);
var
  h: Integer;
begin
  BtnOK.AutoSize := true;
  h := BtnOK.Height;
  BtnOK.AutoSize := false;
  BtnOK.Height := h;
  BtnOK.Width := BtnCancel.Width;
  BtnCancel.Height := h;
end;

function TOfficeMoreColorsWin.GetHint(c: TColor): string;
begin
  Result := Format('RGB(%u, %u, %u)'#13'Hex: %s', [
    GetRValue(c), GetGValue(c), GetBValue(c), ColorToHex(c)
  ]);
end;

function TOfficeMoreColorsWin.GetPickerIndex: Integer;
begin
  Result := PickerNotebook.PageIndex + 1;
  if Pages.PageIndex = 0 then
    Result := -Result;
end;

procedure TOfficeMoreColorsWin.SetPickerIndex(AValue: Integer);
begin
  if AValue = 0 then begin
    Pages.PageIndex := 0;
    PickerNotebook.PageIndex := 0;
  end else
  begin
    PickerNotebook.PageIndex := abs(AValue) - 1;
    if AValue > 0 then
      Pages.PageIndex := 1 else
      Pages.PageIndex := 0;
  end;
end;

function TOfficeMoreColorsWin.GetSelectedColor: TColor;
begin
  Result := NewSwatch.Color;
end;

function TOfficeMoreColorsWin.GetShowHint: Boolean;
begin
  Result := inherited ShowHint;
end;

procedure TOfficeMoreColorsWin.HexaChange(Sender: TObject);
begin
  NewSwatch.Color := Hexa.SelectedColor;
end;

procedure TOfficeMoreColorsWin.HSLChange(Sender: TObject);
begin
  SetAllCustom(HSL.SelectedColor);
end;

procedure TOfficeMoreColorsWin.HSLRingChange(Sender: TObject);
begin
  SetAllCustom(HSLRing.SelectedColor);
end;

procedure TOfficeMoreColorsWin.nbHSLRingResize(Sender: TObject);
begin
  HSLRing.Width := HSLRing.Height;
end;

procedure TOfficeMoreColorsWin.NewSwatchColorChange(Sender: TObject);
begin
  NewSwatch.Hint := GetHint(NewSwatch.Color);

  exit;


  if (EdRed = nil) or (EdBlue = nil) or (EdGreen = nil) or
     (EdHue = nil) or (EdSat = nil) or (EdLumVal = nil)
  then
    exit;

  SetAllCustom(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.OldSwatchColorChange(Sender: TObject);
begin
  OldSwatch.Hint := GetHint(OldSwatch.Color);



  //SetAllToSel(OldSwatch.Color);
end;

procedure TOfficeMoreColorsWin.PagesChange(Sender: TObject);
begin
  SetAllToSel(FSelectedColor); //NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.PagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  FSelectedColor := NewSwatch.Color;
  {
  case Pages.PageIndex of
    0: FSelectedColor := Hexa.SelectedColor;
    1: case PickerNotebook.PageIndex of
         0: FSelectedColor := HSL.SelectedColor;
         1: FSelectedColor := HSLRing.SelectedColor;
         2: FSelectedColor := SLH.SelectedColor;
         3: FSelectedColor := RgbToColor(RTrackbar.Red, GTrackbar.Green, BTrackbar.Blue);
       end;
  end;
  }
end;

procedure TOfficeMoreColorsWin.SetAllCustom(c: TColor);
var
  r, g, b: Integer;
  H, S, L, V: Double;
begin
  if (EdRed = nil) or (EdGreen = nil) or (EdBlue = nil) or
     (EdHue = nil) or (EdSat = nil) or (EdLumVal = nil) or
     (PickerNotebook = nil) or (HSL = nil) or (HSLRing = nil) or (SLH = nil)
     or (FLockChange > 0)
  then
    exit;

  BeginUpdate;

  NewSwatch.Color := c;

  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  case FBrightnessMode of
    bmLuminance : ColorToHSL(c, H, S, L);
    bmValue     : ColortoHSV(c, H, S, V);
  end;

  if PickerNotebook.ActivePage = nbHSL.Name then begin
  //  HSL.Lock;
    HSL.SelectedColor := c;
  //  HSL.Unlock;
  end else
  if PickerNotebook.ActivePage = nbHSLRing.Name then
    HSLRing.SelectedColor := c
  else
  if PickerNotebook.ActivePage = nbSLH.Name then
    SLH.SelectedColor := c
  else
  if PickerNotebook.ActivePage = nbRGB.Name then
  begin
    RTrackbar.SelectedColor := c;
    GTrackbar.SelectedColor := c;
    BTrackbar.SelectedColor := c;
  end
  else
    exit; //raise Exception.Create('Notbook page not prepared for color pickers');

  EdRed.Value := r;
  EdGreen.Value := g;
  EdBlue.Value := b;
  EdHue.Value := H * HSL.MaxHue;
  EdSat.Value := S * HSL.MaxSaturation;
  case FBrightnessMode of
    bmLuminance: EdLumVal.Value := L * HSL.MaxLuminance;
    bmValue    : EdLumVal.Value := V * HSL.MaxValue;
  end;

  EndUpdate;
end;

procedure TOfficeMoreColorsWin.SetAllToSel(c: TColor);
begin
  //inc(FLockChange);
  case Pages.ActivePageIndex of
    // Standard Page
    0: Hexa.SelectedColor := c;
    // Custom Page
    1: SetAllCustom(c);
  end;
  NewSwatch.Color := c;
  //dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetBrightnessMode(AMode: TBrightnessMode);
begin
  FBrightnessMode := AMode;
  case AMode of
    bmLuminance: LblLumVal.Caption := 'Lum:';
    bmValue    : LblLumVal.Caption := 'Val:';
  end;
end;

procedure TOfficeMoreColorsWin.SetMaxHue(H: Integer);
var
  hh: Double;
begin
  inc(FLockChange);
  hh := EdHue.Value / FMaxHue;
  FMaxHue := H;
  EdHue.MaxValue := H;
  EdHue.Value := round(hh * FMaxHue);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetMaxLum(L: Integer);
var
  ll: Double;
begin
  inc(FLockChange);
  ll := EdLumVal.Value / FMaxLum;
  FMaxLum := L;
  EdLumVal.MaxValue := L;
  EdLumVal.Value := round(ll * FMaxLum);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetMaxSat(S: Integer);
var
  ss: Double;
begin
  inc(FLockChange);
  ss := EdSat.Value / FMaxSat;
  FMaxSat := S;
  EdSat.MaxValue := S;
  EdSat.Value := round(ss * FMaxSat);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetMaxVal(V: Integer);
var
  vv: Double;
begin
  inc(FLockChange);
  vv := EdLumVal.Value / FMaxVal;
  FMaxVal := V;
  EdLumVal.MaxValue := V;
  EdLumVal.Value := round(vv * FMaxVal);
  dec(FLockChange);
end;

procedure TOfficeMoreColorsWin.SetSelectedColor(c: TColor);
begin
  FSelectedColor := c;
  OldSwatch.Color := c;
  SetAllToSel(FSelectedColor);
end;

procedure TOfficeMoreColorsWin.SetShowHint(AValue: Boolean);
begin
  inherited ShowHint := AValue;
  // Unfortunately Notebook does not have a Hint and ParentHint...
  HSL.ShowHint := AValue;
  HSLRing.ShowHint := AValue;
  SLH.ShowHint := AValue;
  RTrackbar.ShowHint := AValue;
  GTrackbar.ShowHint := AValue;
  BTrackbar.ShowHint := AValue;
end;

procedure TOfficeMoreColorsWin.SLHChange(Sender: TObject);
begin
  SetAllCustom(SLH.SelectedColor);
end;

end.
