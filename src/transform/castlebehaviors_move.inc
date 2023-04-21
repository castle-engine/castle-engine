{%MainUnit castlebehaviors.pas}
{
  Copyright 2032-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Move the parent @link(TCastleTransform) when a given input (like a key or mouse button)
    is pressed in the given @link(Direction).

    You can add multiple behaviors of this class to one @link(TCastleTransform),
    each reacting to different keys, to react to e.g. 4 arrow keys or AWSD keys.
    The idea of this class is to enable simple object / player manipulation in 2D and 3D.

    The move may be done by directly changing @link(TCastleTransform.Translation)
    or using physics, see @link(ChangeTransformation) for options.
    There may be an acceleration / deceleration applied, see
    @link(AccelerationTime), @link(AccelerationEasing),
    @link(DecelerationTime), @link(DecelerationEasing).

    This behavior works both at runtime, and in CGE editor when the "simulation"
    is running.

    Remember that you can also implement your own full-featured navigation using code
    (as behaviors, see https://castle-engine.io/behaviors ,
    or just controlling any transformation (e.g. TCastleCamera instance)
    from TCastleView methods or other components,
    see https://castle-engine.io/camera and https://castle-engine.io/navigation ).
    This class provides a simple way to map input -> to movement,
    but it is certainly not the only way to do it. }
  TCastleMoveBehavior = class(TCastleBehavior)
  strict private
    FDirection: TVector3;
    FInput: TInputShortcut;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Direction in which to move.
      The length of this vector also affects the effective speed.
      By default this is (1,0,0). }
    property Direction: TVector3 read FDirection write FDirection;

    // TODO: Useful to easily scale Direction? Also make
    // property Speed: Single read FSpeed write FSpeed {$ifdef FPC}default 1.0{$endif};

    // TODO: Allow to control this using physics
    // { How to change the transformation - directly or using physics. }
    // property ChangeTransformation: TChangeTransformation ...;

    // TODO: acceleration / deceleration:
    // { Time in seconds to accelerate to full speed, once input is pressed. }
    // property AccelerationTime: Single default {$ifdef FPC}0{$endif};
    // { How smoothly is the acceleration performed - linear or others. See https://easings.net/. }
    // property AccelerationEasing: TCastleEasing;
    // { Time in seconds to decelerate to 0 speed, once input is released. }
    // property DecelerationTime: Single default {$ifdef FPC}0{$endif}; in seconds
    // { How smoothly is the deceleration performed - linear or others. See https://easings.net/. }
    // property DecelerationEasing: TCastleEasing;
  published
    { Input (key, mouse) that triggers the movement.
      By default this is completely empty (default), so it cannot be triggered. }
    property Input: TInputShortcut read FInput;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlemovebehavior_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlemovebehavior_persistent_vectors.inc}
{$undef read_implementation_methods}

constructor TCastleMoveBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FDirection := Vector3(1, 0, 0);

  FInput := TInputShortcut.Create(Self);
  FInput.SetSubComponent(true);
  FInput.Name := 'Input';

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlemovebehavior_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleMoveBehavior.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlemovebehavior_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleMoveBehavior.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Input', 'DirectionPersistent'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleMoveBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  FocusedViewport: TCastleUserInterface;
  FocusedContainer: TCastleContainer;
begin
  inherited;

  if not (CastleApplicationMode in [appRunning, appSimulation]) then
    Exit;

  // TODO: hack
  if (Parent <> nil) and
     (Parent.World <> nil) and
     (Parent.World.Owner is TCastleUserInterface) then
    FocusedViewport := TCastleUserInterface(Parent.World.Owner)
  else
    FocusedViewport := nil;

  if FocusedViewport <> nil then
    FocusedContainer := FocusedViewport.Container
  else
    FocusedContainer := nil;

  if (FocusedContainer <> nil) and
     Input.IsPressed(FocusedContainer) then
    Parent.Translation := Parent.Translation + FDirection * SecondsPassed;
end;

{$endif read_implementation}
