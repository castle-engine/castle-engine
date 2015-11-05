{
  Copyright 2015-2015 Michalis Kamburelis.

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
    function ControlColor(const C: TUIControl): TCastleColor;
    function ControlDescription(const C: TUIControl): string;
  protected
    function KeepInFront: boolean; override;
  public
    const
      DefaultPadding = 10;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Rect: TRectangle; override;
    procedure BeforeRender; override;
    procedure Render; override;
    function CapturesEventsAtPosition(const Position: TVector2Single): boolean; override;

    { Text color. By default it's white. }
    property Color: TCastleColor read FColor write FColor;
  published
    { Padding between rect borders and text. }
    property Padding: Integer read FPadding write FPadding
      default DefaultPadding;
    property HasHorizontalAnchor default true;
    property HasVerticalAnchor default true;
    property HorizontalAnchor default hpMiddle;
    property VerticalAnchor default vpMiddle;
  end;

implementation

uses SysUtils,
  CastleGLUtils;

constructor TCastleInspectorControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
  FPadding := DefaultPadding;
  HasHorizontalAnchor := true;
  HasVerticalAnchor := true;
  HorizontalAnchor := hpMiddle;
  VerticalAnchor := vpMiddle;

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
  US := UIScale;
  PaddingScaled := Round(US * Padding);

  Result := Rectangle(
    LeftBottomScaled,
    Font.MaxTextWidth(FText, true) + 2 * PaddingScaled,
    Font.RowHeight * FText.Count + Font.Descend + 2 * PaddingScaled);
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
    (Hash and $FF) / 255,
    ((Hash shr 8) and $FF) / 255,
    ((Hash shr 16) and $FF) / 255,
    Transparency);
end;

function TCastleInspectorControl.ControlDescription(const C: TUIControl): string;
begin
  if C.Name <> '' then
    Result := C.Name + ':' + C.ClassName else
    Result := C.ClassName;
end;

procedure TCastleInspectorControl.Render;
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
    Theme.Draw(SR, tiLabel, UIScale);
    Font.PrintStrings(
      SR.Left + PaddingScaled,
      SR.Bottom + PaddingScaled + Font.Descend,
      Color, FText, true, 0, hpLeft);

    for I := 0 to FControlsUnderMouse.Count - 1 do
    begin
      SR := FControlsUnderMouse[I].ScreenRect;
      if SR.IsEmpty then Continue;
      C := ControlColor(FControlsUnderMouse[I]);
      DrawRectangle(SR, C);
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
    if (C <> Self) and C.CapturesEventsAtPosition(Container.MousePosition) then
    begin
      S := ControlDescription(C) + ' ' + C.ScreenRect.ToString;
      if not C.GetExists then
        S += ' (hidden)';
      S := StringOfChar(' ', Level) + S;
      S := '<font color="#' + ColorToHex(Col) + '">' + S + '</font>';
      FText.Add(S);
      FControlsUnderMouse.Add(C);
    end;
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
end;

function TCastleInspectorControl.CapturesEventsAtPosition(const Position: TVector2Single): boolean;
begin
  Result := true;
end;

function TCastleInspectorControl.KeepInFront: boolean;
begin
  Result := true;
end;

end.
