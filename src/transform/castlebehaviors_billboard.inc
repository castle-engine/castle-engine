{%MainUnit castlebehaviors.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

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
  { Behavior to make parent TCastleTransform a billboard, that always rotates
    to face the current camera.

    The front of the parent, that always turns to the camera,
    is determined by the @link(TCastleTransform.Orientation),
    which in turn by default is taken from @link(TCastleTransform.DefaultOrientation),
    which by default just means "positive Z axis" (otUpYDirectionZ).
    The idea is that @link(TCastleTransform.Direction) is updated
    to point toward the camera.

    The axis around which billboard rotates is determined by @link(AxisOfRotation).
    When non-zero, this axis is set as @link(TCastleTransform.Up). }
  TCastleBillboard = class(TCastleBehavior)
  strict private
    FAxisOfRotation: TVector3;
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Axis around which the billboard rotates to face the camera.
      The rotation is free in 3D (with undefined up vector) when this is zero.

      When non-zero, this axis is set as @link(TCastleTransform.Up).

      Similar to X3D @link(TBillboardNode.AxisOfRotation).
      By default +Y (that is, (0, 1, 0)).

      Note that this axis is expressed in the coordinate system
      of "parent of our parent" transform.
      That is, if you have instance X of a TCastleTransform, with TCastleBehavior attached,
      and it is a child of TCastleTransform instance Y,
      then you can rotate Y and it effectively rotates also the billboard's
      axis of rotation. The billboard still works correctly (it will rotate the X
      to look at camera as much as possible, though it cannot change rotation on Y). }
    property AxisOfRotation: TVector3 read FAxisOfRotation write FAxisOfRotation;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlebillboard_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlebillboard_persistent_vectors.inc}
{$undef read_implementation_methods}

constructor TCastleBillboard.Create(AOwner: TComponent);
begin
  inherited;

  FAxisOfRotation := Vector3(0, 1, 0);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlebillboard_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleBillboard.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlebillboard_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleBillboard.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'AxisOfRotationPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleBillboard.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  Camera: TCastleCamera;
  DirectionToCamera: TVector3;
begin
  inherited;

  // TODO: do it before Render, knowing camera from particular view?

  if Parent.HasWorldTransform and
     { this condition is not obvious from HasWorldTransform,
       as TCastleAbstractRootTransform also has world transform }
     (Parent.Parent <> nil) and
     (Parent.World <> nil) and
     (Parent.World.MainCamera <> nil) then
  begin
    Camera := Parent.World.MainCamera;

    { The transformation which interests us is the whole Parent.Parent transformation,
      and the Parent.Translation.
      We will modify the rotation.

    WorldInverseTransform := TranslationMatrix(-Parent.Translation) *
      Parent.Parent.WorldInverseTransform;

    // calculate vector from (0, 0, 0) to camera Position, in local coords
    DirectionToCamera := WorldInverseTransform.MultPoint(Camera.Position);

    More optimized version (just revert translation directly,
    instead of as a matrix): }

    DirectionToCamera := Parent.Parent.WorldToLocal(Camera.WorldTranslation)
      - Parent.Translation;

    Parent.Center := TVector3.Zero; // no other value makes sense for billboard implementation

    if AxisOfRotation.IsPerfectlyZero then
    begin
      Parent.SetView(
        DirectionToCamera,
        { Note that here we don't care about Parent.Translation,
          because we only transform direction. }
        Parent.Parent.WorldToLocalDirection(Camera.Up)
      );
    end else
    begin
      Parent.SetView(
        DirectionToCamera,
        AxisOfRotation,
        { adjust direction, keep up constant } false
      );
    end;
  end;
end;

function TCastleBillboard.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleBillboard) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleBillboard behavior can be added to a given TCastleTransform';
    Result := false;
  end;
end;

{$endif read_implementation}
