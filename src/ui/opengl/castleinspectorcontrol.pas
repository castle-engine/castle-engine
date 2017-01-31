{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Inspector of 2D controls (@link(TCastleInspectorControl)). }
unit CastleInspectorControl;

{$I castleconf.inc}

interface

uses Classes,
  CastleControls, CastleUIControls, CastleColors, CastleRectangles,
  CastleVectors, CastleKeysMouse;

type
  { Add this on top of your 2D controls to have a nice inspector
    displaying names and borders of stuff under the mouse cursor. }
  TCastleInspectorControl = class(TUIControlFont)
  private
    FColor: TCastleColor;
    FPadding: Integer;
    FText: TStringList;
    FControlsUnderMouse: TUIControlList;
    FControlsInitialized: boolean;
    FRectWhenControlsInitialized: TRectangle;
    FShowNotExisting: boolean;
    function ControlColor(const C: TUIControl): TCastleColor;
    function ControlDescription(const C: TUIControl): string;
  public
    const
      DefaultPadding = 10;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Rect: TRectangle; override;
    procedure BeforeRender; override;
    procedure Render; override;
    function CapturesEventsAtPosition(const Position: TVector2Single): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    { Text color. By default it's white. }
    property Color: TCastleColor read FColor write FColor;
  published
    { Padding between rect borders and text. }
    property Padding: Integer read FPadding write FPadding
      default DefaultPadding;
    property HasHorizontalAnchor stored false;
    property HasVerticalAnchor stored false;
    property HorizontalAnchorSelf stored false;
    property HorizontalAnchorParent stored false;
    property VerticalAnchorSelf stored false;
    property VerticalAnchorParent stored false;
    property KeepInFront stored false;
  end;

implementation

uses SysUtils,
  CastleGLUtils;

constructor TCastleInspectorControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
  FPadding := DefaultPadding;
  FRectWhenControlsInitialized := TRectangle.Empty;
  Anchor(hpLeft);
  Anchor(vpBottom);
  KeepInFront := true;

  FText := TStringList.Create;
  FControlsUnderMouse := TUIControlList.Create(false);
end;

destructor TCastleInspectorControl.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FControlsUnderMouse);
  inherited;
end;

function TCastleInspectorControl.Rect: TRectangle;
var
  US: Single;
  PaddingScaled: Integer;
begin
  if FControlsInitialized then
  begin
    US := UIScale;
    PaddingScaled := Round(US * Padding);
    FRectWhenControlsInitialized := Rectangle(
      LeftBottomScaled,
      Font.MaxTextWidth(FText, true) + 2 * PaddingScaled,
      Font.RowHeight * FText.Count + Font.Descend + 2 * PaddingScaled);
  end;
  { note that when FControlsInitialized = false, this simply returns last value calculated
    when FControlsInitialized = true. }
  Result := FRectWhenControlsInitialized;
end;

function TCastleInspectorControl.ControlColor(const C: TUIControl): TCastleColor;
const
  Transparency = 0.1;
var
  Hash: PtrUInt;
begin
  { simple hash from https://gist.github.com/badboy/6267743
    Note that the default GetHashCode in FPC simply returns PtrInt(C),
    so is not really distributed in any interesting way. }
  {$I norqcheckbegin.inc}
  Hash := PtrUInt(Pointer(C));
  Hash := (not Hash) + (Hash shl 15);
  Hash := Hash xor (Hash shr 12);
  Hash := Hash + (Hash shl 2);
  Hash := Hash xor (Hash shr 4);
  Hash := Hash * 2057;
  Hash := Hash xor (Hash shr 16);
  {$I norqcheckend.inc}

  Result := Vector4Single(
    0.5 + (Hash and $FF) / 128,
    0.5 + ((Hash shr 8) and $FF) / 128,
    0.5 + ((Hash shr 16) and $FF) / 128,
    Transparency);
end;

function TCastleInspectorControl.ControlDescription(const C: TUIControl): string;
begin
  if C.Name <> '' then
    Result := C.Name + ':' + C.ClassName else
    Result := C.ClassName;
end;

procedure TCastleInspectorControl.Render;

  function InvertColorRGB(const C: TCastleColor): TCastleColor;
  begin
    Result := C;
    Result[0] := 1 - Result[0];
    Result[1] := 1 - Result[1];
    Result[2] := 1 - Result[2];
  end;

var
  US: Single;
  SR: TRectangle;
  PaddingScaled, I: Integer;
  C: TCastleColor;
begin
  inherited;
  SR := ScreenRect;
  US := UIScale;
  PaddingScaled := Round(US * Padding);

  if FText.Count <> 0 then
  begin
    DrawRectangle(SR, Black);
    //Theme.Draw(SR, tiLabel, UIScale);
    Font.PrintStrings(
      SR.Left + PaddingScaled,
      SR.Bottom + PaddingScaled + Font.Descend,
      Color, FText, true, 0, hpLeft);

    for I := 0 to FControlsUnderMouse.Count - 1 do
    begin
      SR := FControlsUnderMouse[I].ScreenRect;
      if SR.IsEmpty then Continue;
      C := ControlColor(FControlsUnderMouse[I]);
      DrawRectangle(SR, InvertColorRGB(C));
      C[3] := 1.0;
      Theme.Draw(SR, tiActiveFrame, UIScale, C);
      Font.Print(SR.Left + PaddingScaled,
        SR.Top - PaddingScaled - Font.RowHeight, C,
        ControlDescription(FControlsUnderMouse[I]));
    end;
  end;

  { will be initialized again in next BeforeRender,
    for now clear it --- to not keep references to controls that could
    be freed. }
  FText.Clear;
  FControlsUnderMouse.Clear;
  FControlsInitialized := false;
end;

procedure TCastleInspectorControl.BeforeRender;

  procedure CheckControl(C: TUIControl; const Level: Integer);
  var
    I: Integer;
    Col: TCastleColor;
    S: string;
  begin
    Col := ControlColor(C);
    Col[3] := 1.0;
    if (C <> Self) and
        C.CapturesEventsAtPosition(Container.MousePosition) and
        (C.GetExists or FShowNotExisting) then
    begin
      S := ControlDescription(C) + ' ' + C.ScreenRect.ToString;
      if not C.GetExists then
        S += ' (hidden)';
      if C.Focused then
        S += ' (focused)';
      S := StringOfChar(' ', Level) + S;
      S := '<font color="#' + ColorToHex(Col) + '">' + S + '</font>';
      FText.Add(S);
      FControlsUnderMouse.Add(C);
    end;
    if C.GetExists or FShowNotExisting then
      for I := 0 to C.ControlsCount - 1 do
        CheckControl(C.Controls[I], Level + 1);
  end;

var
  I: Integer;
begin
  inherited;

  FText.Clear;
  FControlsUnderMouse.Clear;
  for I := 0 to Container.Controls.Count - 1 do
    CheckControl(Container.Controls[I], 0);
  if Container.Focus.Count <> 0 then
  begin
    FText.Add('');
    FText.Add('Container.Focus:');
    for I := 0 to Container.Focus.Count - 1 do
      FText.Add('  ' + ControlDescription(Container.Focus[I]));
  end;
  FControlsInitialized := true;
end;

function TCastleInspectorControl.CapturesEventsAtPosition(const Position: TVector2Single): boolean;
begin
  Result := true;
end;

function TCastleInspectorControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

  { escape to bottom or top, to not be under mouse }
  if Event.FingerIndex = 0 then
  begin
    if ScreenRect.Contains(Event.Position) then
    begin
      if HorizontalAnchorSelf = hpLeft then
        Anchor(hpRight) else
        Anchor(hpLeft);
    end;
  end;

  VisibleChange;
end;

end.
