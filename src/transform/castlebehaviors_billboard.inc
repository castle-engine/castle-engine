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
    FMatchCameraDirection: Boolean;
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeRender(
      const CameraView: TViewVectors;
      const ParentParentWorldTransform, ParentParentWorldInverseTransform,
            ParentWorldTransform, ParentWorldInverseTransform: TMatrix4); override;
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
      to look at camera as much as possible, though it cannot change rotation on Y).

      See @link(MatchCameraDirection) for details how this axis affects
      object transformation. }
    property AxisOfRotation: TVector3 read FAxisOfRotation write FAxisOfRotation;

  published
    { Whether the billboard direction should match the camera direction
      (@true, default) or direction from camera to the billboard origin (@false).

      The difference between these 2 settings is subtle.

      @unorderedList(
        @item(
          When @true, all billboards in the scene have the same orientation
          (if they have the same @link(AxisOfRotation)).
          They just follow camera orientation.
          Billboard orientation is unrelated to billboard position,
          and unrelated to camera position.

          Strafing (moving camera) left/right doesn't change the billboard orientation.
          Moving billboard left/right doesn't change the billboard orientation.

          This may look better or worse -- really depends on what you want to achieve.

          The major advantage of this approach (and the reason why it is the default)
          is that blending sorting for objects with thin layers, like on Spine models,
          can be made perfectly reliable, even in 3D.

          @unorderedList(
            @item(
              When @link(AxisOfRotation) is zero (on all billboards) then
              @link(TCastleViewport.BlendingSort) set to @link(sort3D)
              will be perfect. It will work even
              if camera can make free rotations (downward and upward) in 3D.

              @link(sort3D) is also a good choice if @link(AxisOfRotation) is non-zero,
              but your camera "up" is locked (you don't let player to rotate downward or upward)
              to the same direction.
            )

            @item(
              When @link(AxisOfRotation) is +Y (0,1,0) (on all billboards) then
              @link(TCastleViewport.BlendingSort) set to @link(sort3DVerticalBillboards)
              will be perfect. It will work even
              if camera can make free rotations (downward and upward) in 3D.
            )

            @item(
              Many other possibilities can be accounted for using @link(sortCustom)
              and @link(TCastleViewport.OnCustomShapeSort) event.
              If only your @link(AxisOfRotation) is consistent for all billboards,
              you can implement something similar to @link(sort3DVerticalBillboards)
              to account for given up vector.
            )
          )
        )

        @item(
          When @false, billboard orientation adjusts to the direction
          from camera to the origin of the billboard transformation.
          This means that the billboard position affects the billboard orientation,
          e.g. billboards on the edge of the field of view
          will have a different orientation than in the middle of the view.

          This is consistent with X3D Billboard,
          https://www.web3d.org/documents/specifications/19775-1/V4.0/Part01/components/navigation.html#Billboard .
        )
      )
    }
    property MatchCameraDirection: Boolean
      read FMatchCameraDirection write FMatchCameraDirection default true;

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

  FMatchCameraDirection := true;
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
  if ArrayContainsString(PropertyName, [
       'AxisOfRotationPersistent', 'MatchCameraDirection'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleBillboard.BeforeRender(
  const CameraView: TViewVectors;
  const ParentParentWorldTransform, ParentParentWorldInverseTransform,
        ParentWorldTransform, ParentWorldInverseTransform: TMatrix4);

  { Like Parent.Parent.WorldToLocalDirection, but uses current
    transformation knowledge. }
  function ParentParentWorldToLocalDirection(const D: TVector3): TVector3;
  begin
    Result := ParentParentWorldInverseTransform.MultDirection(D);
  end;

  { Like Parent.Parent.WorldToLocal, but uses current
    transformation knowledge. }
  function ParentParentWorldToLocal(const P: TVector3): TVector3;
  begin
    Result := ParentParentWorldInverseTransform.MultPoint(P);
  end;

var
  DirectionToCamera: TVector3;
begin
  inherited;

  if MatchCameraDirection then
    DirectionToCamera := -ParentParentWorldToLocalDirection(CameraView.Direction)
  else
  begin
    { The transformation which interests us is the whole Parent.Parent transformation,
      and the Parent.Translation.
      We will modify the rotation.

    WorldInverseTransform := TranslationMatrix(-Parent.Translation) *
      Parent.Parent.WorldInverseTransform;

    // calculate vector from (0, 0, 0) to camera Position, in local coords
    DirectionToCamera := WorldInverseTransform.MultPoint(CameraView.Translation);

    More optimized version (just revert translation directly,
    instead of as a matrix): }

    DirectionToCamera := ParentParentWorldToLocal(CameraView.Translation)
      - Parent.Translation;
  end;

  Parent.Center := TVector3.Zero; // no other value makes sense for billboard implementation

  if AxisOfRotation.IsPerfectlyZero then
  begin
    Parent.SetView(
      DirectionToCamera,
      { Note that here we don't care about Parent.Translation,
        because we only transform direction. }
      ParentParentWorldToLocalDirection(CameraView.Up)
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
