{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Fill a rectangle on screen with given color or theme image. }
  TCastleRectangleControl = class(TCastleUserInterface)
  strict private
    FColor: TCastleColor;
    FThemeImage: TThemeImage;
    FUseThemeImage: boolean;
    FInterceptInput: boolean;
    procedure SetColor(const Value: TCastleColor);
    procedure SetThemeImage(const Value: TThemeImage);
    procedure SetUseThemeImage(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Rectangle color. By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    { Fill rectangle with theme image. You have to set @link(UseThemeImage) to true to take effect. }
    property ThemeImage: TThemeImage read FThemeImage write SetThemeImage;
      {$ifdef FPC}deprecated 'use TCastleImageControl to display images';{$endif}

    { Use theme image for drawing instead of color. }
    property UseThemeImage: boolean read FUseThemeImage write SetUseThemeImage default false;
      {$ifdef FPC}deprecated 'use TCastleImageControl to display images';{$endif}

    { Prevents passing mouse/keyboard events to the controls underneath.
      More precisely, when this property is @true, then the
      @link(Press), @link(Release) and @link(Motion) events are marked as
      "handled" by this control.  }
    property InterceptInput: boolean read FInterceptInput write FInterceptInput
      default false;
      {$ifdef FPC}deprecated 'in each case, there should be a cleaner way to disable something from processing the input';{$endif}

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlerectanglecontrol_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleRectangleControl ---------------------------------------------------- }

constructor TCastleRectangleControl.Create(AOwner: TComponent);
begin
  inherited;
  FColor := White;
  FThemeImage := tiWindow;
  FUseThemeImage := false;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlerectanglecontrol_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleRectangleControl.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlerectanglecontrol_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleRectangleControl.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleRectangleControl.SetThemeImage(const Value: TThemeImage);
begin
  if FThemeImage <> Value then
  begin
    FThemeImage := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleRectangleControl.SetUseThemeImage(const Value: boolean);
begin
  if FUseThemeImage <> Value then
  begin
    FUseThemeImage := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleRectangleControl.Render;
begin
  inherited;
  {$warnings off} // using deprecated just to keep it working
  if FUseThemeImage then
    Theme.Draw(RenderRect, ThemeImage, UIScale, Color)
  else
  {$warnings on}
    DrawRectangle(RenderRect, Color);
end;

function TCastleRectangleControl.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  {$warnings off} // using deprecated just to keep it working
  Result := Result or InterceptInput;
  {$warnings on}
end;

function TCastleRectangleControl.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  {$warnings off} // using deprecated just to keep it working
  Result := Result or InterceptInput;
  {$warnings on}
end;

function TCastleRectangleControl.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  {$warnings off} // using deprecated just to keep it working
  Result := Result or InterceptInput;
  {$warnings on}
end;

function TCastleRectangleControl.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ColorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlerectanglecontrol_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
