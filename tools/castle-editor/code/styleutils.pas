unit StyleUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, Graphics;

type
  { Menus with FontSize <> default must be OwnerDrawn }
  TCastleEditorColorCategory = (
    catMenuFont,
    catMenu,
    catMenuHighlight,
    catMenuDisabledFont,
    catMenuSeparator
  );

  TCastleEditorColorArray = array[TCastleEditorColorCategory]of TColor;

  TCastleEditorStyle = record
    UsePlugin: Boolean;
    SheduledStyleDisable: Boolean;
    FontSize: Integer;
    SplitterSize: Integer;
    ChangeMenu: Boolean;
    Colors: TCastleEditorColorArray;
  end;

const
  EditorFontSizeMin = 7;
  EditorFontSizeMax = 18;
  EditorFontSizeDef = 9;
  SplitterSizeMin   = 6;
  SplitterSizeMax   = 20;
  SplitterSizeDef   = 6;

var
  CurrentStyle: TCastleEditorStyle;

procedure LoadStyleSettings;
procedure SaveStyleSettings;
procedure SetDefaults;
{ Styles are loaded from UserConfig. You typically don't need to enable them manually }
procedure EnableStyles;
{ Reset can be done after restarting the app }
procedure DisableStyles;

{ Update all controls and menus in one go. You can choose to update
  controls or menus separately using the UpdateControlStyle or UpdateMenu procedures }
procedure UpdateAll(AControl: TControl);
{ Update the style of the AControl and its children.
  IgnoreParentSettings - force change even when ParentFont is true
  LabelsWithItalicAreSmaller - some labels are less important, like these on
    PreferencesForm, so using Italic font style can help us determine if we want
    the label's font size smaller }
procedure UpdateControlStyle(AControl: TControl; IgnoreParentSettings: Boolean;
  LabelsWithItalicAreSmaller: Boolean);
{ Set the font size for the given control, no children afftected. }
procedure SetSingleControlStyle(AControl: TControl; IgnoreParentSettings: Boolean;
  LabelsWithItalicAreSmaller: Boolean);

procedure UpdateMenu(AMenuItem: TMenuItem); overload;
procedure UpdateMenu(AMenu: TMenu); overload;

implementation

uses
  CastleConfig,
  LCLType, LCLProc, StdCtrls, Buttons, ExtCtrls, ObjectInspector;

type
  TCustomDrawMenu = class
  private
  public
    CheckMark: String;
    BulletMark: string;
    constructor Create; virtual;
    procedure MenuItemOnDraw(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure MenuItemOnMeasure(Sender: TObject; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure SetMenu(AMenuItem: TMenuItem); overload;
    procedure SetMenu(AMenu: TMenu); overload;
  end;

var
  PreferencesCustomDrawMenu: TCustomDrawMenu;

procedure LoadStyleSettings;
begin
  CurrentStyle.UsePlugin  := UserConfig.GetValue('CustomStyle_UsePlugin', false);
  CurrentStyle.SheduledStyleDisable := UserConfig.GetValue('CustomStyle_SheduledStyleDisable', false);
  CurrentStyle.ChangeMenu := UserConfig.GetValue('CustomStyle_ChangeMenu', false);
  CurrentStyle.FontSize := UserConfig.GetValue('CustomStyle_FontSize', EditorFontSizeDef);
  CurrentStyle.SplitterSize := UserConfig.GetValue('CustomStyle_SplitterSize', SplitterSizeDef);
  if CurrentStyle.SheduledStyleDisable then
    begin
      CurrentStyle.SheduledStyleDisable := False;
      CurrentStyle.UsePlugin := False;
      UserConfig.SetValue('CustomStyle_SheduledStyleDisable', false);
    end;
end;

procedure SaveStyleSettings;
begin
  UserConfig.SetValue('CustomStyle_UsePlugin', CurrentStyle.UsePlugin);
  UserConfig.SetValue('CustomStyle_SheduledStyleDisable', CurrentStyle.SheduledStyleDisable);
  UserConfig.SetValue('CustomStyle_ChangeMenu', CurrentStyle.ChangeMenu);
  UserConfig.SetValue('CustomStyle_FontSize', CurrentStyle.FontSize);
  UserConfig.SetValue('CustomStyle_SplitterSize', CurrentStyle.SplitterSize);
end;

procedure SetDefaults;
begin
  CurrentStyle.UsePlugin:= False;
  CurrentStyle.FontSize := EditorFontSizeDef;
  CurrentStyle.SplitterSize := SplitterSizeDef;
  CurrentStyle.ChangeMenu := False;
  CurrentStyle.Colors[catMenuFont]:= clMenuText;
  CurrentStyle.Colors[catMenu] := clMenu;
  CurrentStyle.Colors[catMenuHighlight] := clMenuHighlight;
  CurrentStyle.Colors[catMenuDisabledFont] := clGrayText;
  CurrentStyle.Colors[catMenuSeparator] := clGrayText;
end;

procedure EnableStyles;
begin
  if CurrentStyle.SheduledStyleDisable then
    CurrentStyle.SheduledStyleDisable := False;
  CurrentStyle.UsePlugin := True;
  SaveStyleSettings;
end;

procedure DisableStyles;
begin
  { Don't reset to all-default setting. User may want to come back to where they were before disabling styles }
  // SetDefaults;
  CurrentStyle.SheduledStyleDisable := True;
  CurrentStyle.ChangeMenu := False;
end;

procedure UpdateAll(AControl: TControl);
var
  i: Integer;
  Comp: TComponent;
begin
  if (CurrentStyle.UsePlugin) and (AControl <> nil) then
  begin
    for i := 0 to AControl.ComponentCount-1 do
    begin
      Comp := AControl.Components[i];
      if (Comp is TWinControl) then
        UpdateControlStyle(TWinControl(Comp), false, true);
      if (Comp is TMenu) then
        UpdateMenu(TMenu(Comp));
    end;

    SetSingleControlStyle(AControl, false, true);
  end;
end;

procedure UpdateControlStyle(AControl: TControl; IgnoreParentSettings: Boolean;
  LabelsWithItalicAreSmaller: Boolean);
var
  i,w,h: Integer;
begin
  if (CurrentStyle.UsePlugin) and (AControl <> nil) then
  begin
    if (AControl is TWinControl) then
      for i := 0 to TWinControl(AControl).ControlCount-1 do
        UpdateControlStyle(TWinControl(AControl).Controls[i], IgnoreParentSettings,
          LabelsWithItalicAreSmaller);

    if not AControl.AutoSize then
    begin
      w := AControl.Width;
      h := AControl.Height;
    end;
    SetSingleControlStyle(AControl, IgnoreParentSettings, LabelsWithItalicAreSmaller);
    if not AControl.AutoSize then
    begin
      AControl.Width := w;
      AControl.Height := h;
    end;
  end;
end;

procedure SetSingleControlStyle(AControl: TControl; IgnoreParentSettings: Boolean;
  LabelsWithItalicAreSmaller: Boolean);
var
  CanDoFont: Boolean;
  ParFont: Boolean;
begin
  if CurrentStyle.UsePlugin then
  begin
    ParFont := AControl.IsParentFont;
    CanDoFont := false;
    { SpeedButton and BitBtn need their Font.Size limited to 15,
      otherwise they may not fit or have their Caption clipped.
      At the moment it's hardcoded and not configurable, but works on all forms well }
    if (CurrentStyle.FontSize > 15) and ((AControl is TSpeedButton) or (AControl is TBitBtn)) then
      AControl.Font.Size := 15
    else CanDoFont := (not ParFont) or (ParFont and IgnoreParentSettings);

    if CanDoFont then
    begin
      if not (AControl is TCustomLabel) then
        AControl.Font.Size := CurrentStyle.FontSize;

      // Don't decrease Labels font size if they're very small already
      if (AControl is TCustomLabel) and LabelsWithItalicAreSmaller
          and (CurrentStyle.FontSize > EditorFontSizeMin+1) then
        AControl.Font.Size := CurrentStyle.FontSize-2
      else
        AControl.Font.Size := CurrentStyle.FontSize;
    end;

    { Now hande the Splitter size. It's independent of ParetFont/Color and is always changed.
       Constraints.MinWidth/Height are set because Width/Height sometimes aren't enough }
    if (TCustomSplitter(AControl).Align = alLeft) or (TCustomSplitter(AControl).Align = alRight) then
    begin
      TCustomSplitter(AControl).Constraints.MinWidth := CurrentStyle.SplitterSize;
      TCustomSplitter(AControl).Width := CurrentStyle.SplitterSize;
    end;
    if (TCustomSplitter(AControl).Align = alTop) or (TCustomSplitter(AControl).Align = alBottom) then
    begin
      TCustomSplitter(AControl).Constraints.MinHeight := CurrentStyle.SplitterSize;
      TCustomSplitter(AControl).Height := CurrentStyle.SplitterSize;
    end;
  end;
end;

procedure UpdateMenu(AMenuItem: TMenuItem);
begin
  if PreferencesCustomDrawMenu = nil
    then PreferencesCustomDrawMenu := TCustomDrawMenu.Create;
  PreferencesCustomDrawMenu.SetMenu(AMenuItem);
end;

procedure UpdateMenu(AMenu: TMenu);
begin
  if PreferencesCustomDrawMenu = nil
    then PreferencesCustomDrawMenu := TCustomDrawMenu.Create;
  PreferencesCustomDrawMenu.SetMenu(AMenu);
end;

constructor TCustomDrawMenu.Create;
begin
  inherited;
  CheckMark := '✓';
  BulletMark := '●'; // real bullet = '•' but is very small
end;

procedure TCustomDrawMenu.MenuItemOnDraw(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
var
  MenuItem: TMenuItem;
  TxtStyle: TTextStyle;
  TxtRect: TRect;
  Mark,s: String;
  i,w: Integer;
begin
  MenuItem := TMenuItem(Sender);
  ACanvas.Brush.Color := CurrentStyle.Colors[catMenu];
  ACanvas.Font.Size := CurrentStyle.FontSize;
  ACanvas.FillRect(ARect);
  w := 34; // Give some space for CheckMark/Bullet or image

  if MenuItem.Caption = '-' then //not MenuItem.IsLine then
  begin
    ACanvas.Pen.Color := CurrentStyle.Colors[catMenuSeparator];
    ACanvas.MoveTo(ARect.Left+w, ARect.Top+8);
    ACanvas.LineTo(ARect.Right, ARect.Top+8);
  end
  else
  begin
    TxtStyle := ACanvas.TextStyle;
    TxtStyle.ShowPrefix := true;
    TxtStyle.Layout := tlTop;

    // set font color
    if (odDisabled in AState) then
      ACanvas.Font.Color := CurrentStyle.Colors[catMenuDisabledFont]
    else
      ACanvas.Font.Color := CurrentStyle.Colors[catMenuFont];
    ACanvas.Font.Bold := (odDefault in AState);

    { odSelected with odDisabled use the same backgroud color as just odSelected }
    if (odSelected in AState) then
      ACanvas.Brush.Color := CurrentStyle.Colors[catMenuHighlight];

    ACanvas.FillRect(ARect);

    TxtRect := ARect;
    TxtRect.Left := ARect.Left+w;
    ACanvas.TextRect(TxtRect, TxtRect.Left, TxtRect.Top, MenuItem.Caption, TxtStyle);

    if MenuItem.ShortCut <> 0 then
    begin
      s := ShortCutToText(MenuItem.ShortCut);
      i := ACanvas.TextWidth(s);
      TxtRect.Left := TxtRect.Right-i-22;
      ACanvas.TextRect(TxtRect, TxtRect.Left, TxtRect.Top, s, TxtStyle);
    end;

    if (odChecked in AState) then
    begin
      Mark := CheckMark;
      ACanvas.Font.Size := ACanvas.Font.Size - 3;
      i := ACanvas.TextHeight(Mark);
      if MenuItem.RadioItem then
        Mark := BulletMark;
      ACanvas.TextOut(ARect.Left+8, ARect.Top+(ARect.Height-i)div 2, #32+Mark);
    end;
  end;
end;

procedure TCustomDrawMenu.MenuItemOnMeasure(Sender: TObject; ACanvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  w,h: Integer;
  MenuItem: TMenuItem;
  TxtStyle: TTextStyle;
  s: String;
begin
  MenuItem := TMenuItem(Sender);

  if MenuItem.Caption <> '-' then
  begin
    TxtStyle.ShowPrefix := true;
    ACanvas.TextStyle := TxtStyle;
    ACanvas.Font.Size := CurrentStyle.FontSize;
    ACanvas.Font.Bold := True;
    if MenuItem.ShortCut <> 0 then
      s := ShortCutToText(MenuItem.ShortCut)
    else s := '';
    h := ACanvas.TextHeight('Wq');
    w := 34+22+ACanvas.TextWidth(MenuItem.Caption+s);
  end
  else
  begin
    w := 32;
    h := 16; //ACanvas.TextHeight('Wq') div 2;
  end;

  AWidth := w;
  AHeight := h;
end;

procedure TCustomDrawMenu.SetMenu(AMenuItem: TMenuItem);
var
  i: Integer;
begin
  { Not changing MenuBar as it can't be resized.
    However, it can be hijacked and replaced with buttons if needed }
  if not AMenuItem.IsInMenuBar then
  begin
    { Don't replace existing OwnerDraw and OnMeasureItem.
      If new style has ChangeMenu = false, revert our changes.
      We also need to overwrite the OnMeasure because if font size changed,
      we need to update item height }
    if AMenuItem.OnDrawItem = {$ifdef FPC}@{$endif} MenuItemOnDraw then
      AMenuItem.OnDrawItem := nil;
    if not CurrentStyle.ChangeMenu then
    begin
      if AMenuItem.OnMeasureItem = {$ifdef FPC}@{$endif} MenuItemOnMeasure then
        AMenuItem.OnMeasureItem := nil;
    end
    else if AMenuItem.OnDrawItem = nil then
    begin
      AMenuItem.OnDrawItem := {$ifdef FPC}@{$endif} MenuItemOnDraw;
      AMenuItem.OnMeasureItem := {$ifdef FPC}@{$endif} MenuItemOnMeasure;
    end;
  end;
  for i := 0 to AMenuItem.Count - 1 do
    SetMenu(AMenuItem.Items[i]);
end;

procedure TCustomDrawMenu.SetMenu(AMenu: TMenu);
begin
  if CurrentStyle.UsePlugin then
    SetMenu(AMenu.Items);
end;

procedure InitPreferences;
begin
  // Now when we can disable this plugin, the menu handler will be created on demand
  PreferencesCustomDrawMenu := nil;
  SetDefaults;
end;

initialization
  InitPreferences;

finalization
  PreferencesCustomDrawMenu.Free;

end.

