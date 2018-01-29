{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Fill a rectangle on screen with given color. }
  TCastleRectangleControl = class(TUIControlSizeable)
  strict private
    FColor: TCastleColor;
    FInterceptInput: boolean;
    procedure SetColor(const Value: TCastleColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    { Rectangle color. By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    { Prevents passing mouse/keyboard events to the controls underneath.
      More precisely, when this property is @true, then the
      @link(Press), @link(Release) and @link(Motion) events are marked as
      "handled" by this control.  }
    property InterceptInput: boolean read FInterceptInput write FInterceptInput
      default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleRectangleControl ---------------------------------------------------- }

constructor TCastleRectangleControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
end;

procedure TCastleRectangleControl.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleRectangleControl.Render;
begin
  inherited;
  DrawRectangle(ScreenRect, Color);
end;

function TCastleRectangleControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TCastleRectangleControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

function TCastleRectangleControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  Result := Result or InterceptInput;
end;

{$endif read_implementation}
