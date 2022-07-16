{%MainUnit castletransform.pas}
{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ TCastleCamera and friends. }

{$ifdef read_interface}
  { Value of @link(TCastlePerspective.FieldOfViewAxis). }
  TFieldOfViewAxis = (
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the smaller viewport axis.

      E.g. on a full-screen viewport, on a typical desktop screen,
      with a typical panoramic window (wide, not tall),
      this will determine the vertical axis angle.
      The horizontal axis will be adjusted following the aspect ratio. }
    faSmallest,
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the larger viewport axis.
      The other axis will be adjusted, following the aspect ratio. }
    faLargest,
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the horizontal axis.
      The vertical axis will be adjusted, following the aspect ratio. }
    faHorizontal,
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the vertical axis.
      The horizontal axis will be adjusted, following the aspect ratio. }
    faVertical
  );

  { Subcomponent used in @link(TCastleCamera.Perspective) to set perspective
    projection parameters.

    Do not create instances of this class yourself,
    these are automatically created by TCastleCamera. }
  TCastlePerspective = class(TCastleComponent)
  strict private
    FFieldOfView: Single;
    FFieldOfViewAxis: TFieldOfViewAxis;
    FEffectiveFieldOfView: TVector2;
    procedure SetFieldOfView(const Value: Single);
    procedure SetFieldOfViewAxis(const Value: TFieldOfViewAxis);
    function IsStoredFieldOfView: Boolean;
  private
    Camera: TCastleCamera;
    // @exclude
    procedure InternalSetEffectiveFieldOfView(const AEffectiveFieldOfView: TVector2);
  public
    const
      DefaultFieldOfView = Pi / 4;
      DefaultFieldOfViewAxis = faSmallest;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Effective field of view, horizontal and vertical, in radians.
      Calculated looking at @link(FieldOfView) and @link(FieldOfViewAxis),
      taking into account sizes of the viewport using this camera. }
    property EffectiveFieldOfView: TVector2 read FEffectiveFieldOfView;
  published
    { Perspective field of view angle, in radians.
      The @link(FieldOfViewAxis) determines whether this is horizontal
      or vertical angle. }
    property FieldOfView: Single read FFieldOfView write SetFieldOfView
      stored IsStoredFieldOfView {$ifdef FPC}default DefaultFieldOfView{$endif};

    { Which axis is determined explicitly by @link(FieldOfView).
      @seealso TFieldOfViewAxis }
    property FieldOfViewAxis: TFieldOfViewAxis
      read FFieldOfViewAxis write SetFieldOfViewAxis default DefaultFieldOfViewAxis;
  end;

  { Subcomponent used in @link(TCastleCamera.Orthographic) to set orthographic
    projection parameters.

    Do not create instances of this class yourself,
    these are automatically created by TCastleCamera. }
  TCastleOrthographic = class(TCastleComponent)
  strict private
    FOrigin: TVector2;
    FWidth, FHeight, FScale: Single;
    FEffectiveRect: TFloatRectangle;
    FStretch, WarningEffectiveSizeZeroDone: Boolean;
    procedure SetOrigin(const Value: TVector2);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetScale(const Value: Single);
    procedure SetStretch(const Value: Boolean);
  private
    Camera: TCastleCamera;
    // @exclude
    procedure InternalSetEffectiveRect(const AEffectiveRect: TFloatRectangle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;

    { Additional translation of the camera.
      The camera movement applied here is always scaled by
      the calculated orthographic projection width and height.

      By default this equals (0,0) which means that @link(TCastleTransform.Translation Camera.Translation)
      determines what is visible in the left-bottom corner of the viewport.
      This matches the typical 2D drawing coordinates used throughout our engine.
      In other words, if the camera is at position (0,0,whatever),
      then the (0,0) position in 2D is in the left-bottom corner of the TCastleViewport.

      You can change it e.g. to (0.5,0.5) to move the camera to
      the middle of the viewport.
      In effect, if the camera is at position (0,0,whatever),
      then the (0,0) position is in the center of the TCastleViewport.

      Both values of @name make sense,
      it depends on the game type and how you prefer to think in 2D coordinates.
      And how do you want the result to behave when aspect ratio changes:

      @unorderedList(
        @item(With (0.5,0.5), things will stay "glued"
          to the center.)
        @item(With (0,0), things will stay "glued"
          to the left-bottom corner.)
      )
    }
    property Origin: TVector2 read FOrigin write SetOrigin;

    { Currently used projection dimensions,
      calculated following the algorithm described at @link(Width) and @link(Height),
      taking into account sizes of the viewport using this camera. }
    property EffectiveRect: TFloatRectangle read FEffectiveRect;
    function EffectiveWidth: Single; deprecated 'use EffectiveRect.Width';
    function EffectiveHeight: Single; deprecated 'use EffectiveRect.Height';

    { Scales the projection size derived from @link(Width) and @link(Height).

      The effect of this scale is also affected by the @link(Origin).
      When @link(Origin) is zero, this behaves like scaling around left-botttom corner
      of the viewport.
      When @link(Origin) is (0.5,0.5), this behaves like scaling around
      the middle of the viewport. }
    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1{$endif};
      {$ifdef FPC} deprecated 'just multiply Width or Height instead of using this'; {$endif}

    { Allow non-proportional stretch of projection.
      In effect the @link(Width) and @link(Height)
      (if both non-zero) are applied directly, without correcting them to follow
      aspect ratio of the viewport. }
    property Stretch: Boolean read FStretch write SetStretch default false;
      {$ifdef FPC} experimental; {$endif}
  published
    { Orthographic projection width and height.

      You can leave one or both of them as zero (default) to automatically
      calculate effective projection width and height
      (in @link(EffectiveWidth) and @link(EffectiveHeight)):

      @unorderedList(
        @item(When both @link(Width) and @link(Height) are zero,
          then the effective projection width and height
          are based on the viewport width and height.
          That is, they will follow
          @link(TCastleUserInterface.EffectiveWidth TCastleViewport.EffectiveWidth)
          and
          @link(TCastleUserInterface.EffectiveHeight TCastleViewport.EffectiveHeight).
        )

        @item(When exactly one of @link(Width) and @link(Height) is non-zero,
          then it explicitly determines the projection width or height accordingly.
          This allows to easily display the same piece of the game world,
          regardless of the viewport size.

          The other size is then calculated to follow the aspect ratio
          of the viewport control.
        )

        @item(When both @link(Width) and @link(Height) are non-zero,
          they determine the projection width and height following the algorithm outlined below.
          This also allows to easily display the same piece of the game world,
          regardless of the viewport size.

          @unorderedList(
            @item(When @link(Stretch) = @false (default), they determine the @italic(minimum)
              projection width and height along the given axis.

              If the displayed viewport aspect ratio will be different than given
              @link(Width) and @link(Height) ratio, then these values will be
              treated as minimum values, and they will be adjusted (one of them will be increased)
              for the purposes of rendering.
              You can read the @link(EffectiveWidth), @link(EffectiveHeight) to know
              the adjusted values.

              Note that the @link(TCastleTransform.Translation Camera.Translation) is considered to be relative
              to unadjusted @link(Width) and @link(Height), not to the adjusted
              @link(EffectiveWidth), @link(EffectiveHeight).
              In effect, when @link(Origin) is zero, the @link(TCastleTransform.Translation Camera.Translation) does not point
              to the left-bottom of the whole viewport.
              It points to the left-bottom of the rectangle of aspect ratio
              @link(Width) / @link(Height) within the viewport.
              This way the enlarged viewport shows equal amount of additional space on the left and
              the right (or bottom and top) of the @link(Width) / @link(Height) rectangle within.
            )

            @item(When @link(Stretch) = @true, these values are used directly,
              even if it means that aspect ratio of the projection
              will not reflect the aspect ratio of the viewport on screen.

              This allows to implement some tricks, like @italic(Military Projection),
              https://github.com/castle-engine/castle-engine/issues/290 .)
          )
        )
      )

      In all the cases, the resulting size is also multiplied by @link(Scale),
      by default 1.0.

      You can read @link(EffectiveWidth) and @link(EffectiveHeight)
      to learn the actual projection width and height, calculated using
      the above algorithm.

      @groupBegin }
    property Width: Single read FWidth write SetWidth {$ifdef FPC}default 0{$endif};
    property Height: Single read FHeight write SetHeight {$ifdef FPC}default 0{$endif};
    { @groupEnd }

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Camera determines viewer position and orientation in the viewport.

    You can create instances of this class yourself,
    and add them to @link(TCastleViewport.Items),
    and set them as @link(TCastleViewport.Camera).

    Note that this class does not handle any user input to modify the camera.
    For this, see TCastleNavigation descendants. }
  TCastleCamera = class(TCastleTransform)
  strict private
    FGravityUp: TVector3;
    FProjectionMatrix: TMatrix4;
    FProjectionNear, FProjectionFar: Single;
    FEffectiveProjectionNear, FEffectiveProjectionFar: Single;
    FProjectionType: TProjectionType;

    FAnimation: boolean;
    AnimationEndTime: TFloatTime;
    AnimationCurrentTime: TFloatTime;

    AnimationBeginPosition: TVector3;
    AnimationBeginDirection: TVector3;
    AnimationBeginUp: TVector3;
    AnimationEndPosition: TVector3;
    AnimationEndDirection: TVector3;
    AnimationEndUp: TVector3;

    FFrustum: TFrustum;
    FPerspective: TCastlePerspective;
    FOrthographic: TCastleOrthographic;

    FGizmoCoord: TObject; //< This is TCoordinateNode but we cannot declare it as such
    LastGizmoEffectiveProjectionNear: Single;
    LastGizmoEffectiveRect: TFloatRectangle;
    LastGizmoEffectiveFov: TVector2;

    procedure CommonCreate(const AOwner: TComponent; const AShowGizmo: Boolean);
    procedure RecalculateFrustum;
    procedure SetGravityUp(const Value: TVector3);
    procedure UpdateGizmoCoord;

    { Setter of the @link(ProjectionMatrix) property. }
    procedure SetProjectionMatrix(const Value: TMatrix4);

    procedure SetProjectionNear(const Value: Single);
    procedure SetProjectionFar(const Value: Single);
    procedure SetProjectionType(const Value: TProjectionType);
  private
    procedure VisibleParameterChange;
    // @exclude
    procedure InternalSetEffectiveProjection(
      const AEffectiveProjectionNear, AEffectiveProjectionFar: Single);
  public
    InternalOnSceneBoundViewpointChanged,
    InternalOnSceneBoundViewpointVectorsChanged,
    InternalOnSceneBoundNavigationInfoChanged,
    InternalOnCameraChanged: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    { @exclude Create camera that doesn't show any gizmo in CGE editor. }
    constructor InternalCreateNonDesign(const AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure LocalRender(const Params: TRenderParams); override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;

    { "Up" direction of the world in which player moves.
      Always normalized (when setting this property, we take
      care to normalize the provided vector).

      This determines in which direction @link(TCastleWalkNavigation.Gravity) works.

      This is also the "normal" value for "up" vector in world coordinates.
      Navigation uses it, e.g. features like PreferGravityUpForRotations
      and/or PreferGravityUpForMoving.

      The default value of this vector is (0, 1, 0) (same as the default
      @link(Up) vector). }
    property GravityUp: TVector3 read FGravityUp write SetGravityUp;

    { Camera matrix, transforming from world space into camera space. }
    function Matrix: TMatrix4;

    { Inverse of @link(Matrix), transforming from camera space into world space. }
    function MatrixInverse: TMatrix4;

    { Extract only rotation from your current camera @link(Matrix).
      This is useful for rendering skybox in 3D programs
      (e.g. for VRML/X3D Background node) and generally to transform
      directions between world and camera space.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    function RotationMatrix: TMatrix4;

    { The current camera (viewing frustum, based on
      @link(ProjectionMatrix) (set by you) and @link(Matrix) (calculated here).
      This is recalculated whenever one of these two properties change.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read FFrustum;

    { Projection matrix of the camera.
      Camera needs to know this to calculate @link(Frustum),
      which in turn allows rendering code to use frustum culling.

      In normal circumstances, if you use @link(TCastleViewport) for rendering,
      this is automatically correctly set for you. }
    property ProjectionMatrix: TMatrix4
      read FProjectionMatrix write SetProjectionMatrix;

    { Calculate a ray picked by WindowPosition position on the viewport,
      assuming current viewport dimensions are as given.
      This doesn't look at our container sizes at all.

      Projection (read-only here) describe projection,
      required for calculating the ray properly.

      Resulting RayDirection is always normalized.

      WindowPosition is given in the same style as TCastleContainer.MousePosition:
      (0, 0) is bottom-left. }
    procedure CustomRay(
      const ViewportRect: TFloatRectangle;
      const WindowPosition: TVector2;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3);

    { Animate a camera smoothly into another camera settings.
      This will gradually change our settings (only the most important
      settings, that determine actual camera view, i.e. @link(Matrix) result)
      into another camera.

      Current OtherCamera settings will be internally copied during this call.
      So you can even free OtherCamera instance immediately after calling this.

      Calling AnimateTo while the previous animation didn't finish yet
      is OK. This simply cancels the previous animation,
      and starts the new animation from the current position.

      @groupBegin }
    procedure AnimateTo(const OtherCamera: TCastleCamera;
      const Time: TFloatTime); overload;
    procedure AnimateTo(const APos, ADir, AUp: TVector3;
      const Time: TFloatTime); overload;
    { @groupEnd }

    { Are we currently during animation (caused by @link(AnimateTo)).

      TCastleNavigation descendants may use it to abort normal
      input handling. E.g. when camera is animating,
      then the gravity in TCastleWalkNavigation should not work,
      key/mouse handling in TCastleWalkNavigation shoult not work etc. }
    function Animation: boolean;

    procedure Init(const AInitialPosition, AInitialDirection, AInitialUp,
      AGravityUp: TVector3); deprecated 'use SetWorldView, set GravityUp directly';

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Currently used projection near.
      Derived from @link(ProjectionNear) and possibly scene sizes. }
    function EffectiveProjectionNear: Single;

    { Currently used projection far.
      Derived from @link(ProjectionFar) and possibly scene sizes.
      May be equal ZFarInfinity. }
    function EffectiveProjectionFar: Single;

    procedure VisibleChangeHere(const Changes: TVisibleChanges); override;

    property Orientation default TCastleTransform.DefaultCameraOrientation;

    { Calculate current projection based on this camera parameters and viewport
      sizes and update EffectiveXxx properties like EffectiveProjectionNear,
      EffectiveProjectionFar.

      Note that we get TBox3D as an event, we only use it if necessary (usually
      not necessary).

      @exclude }
    function InternalProjection(const BoxEvent: TBox3DEvent;
      const ViewportWidth, ViewportHeight: Single;
      const DesignCamera: Boolean): TProjection; virtual;
  published
    { Projection near plane distance.

      0 (default) means to automatically calculate a correct value.
      Note that automatic calculation differs between
      perspective projection (looks at the world size and whether shadow volumes are possible)
      and orthographic projection (uses Default2DProjectionNear).

      For perspective projection, values <= 0 are invalid.
      So both value = 0 and value < 0 behave the same:
      they indicate we should automatically calculate the effective ProjectionNear.

      For orthographic projection, all values are valid and reasonable.
      So value < 0 is just used literally,
      it doesn't cause the effective value to be autocalculated. }
    property ProjectionNear: Single read FProjectionNear write SetProjectionNear {$ifdef FPC}default 0{$endif};

    { Projection far plane distance.

      0 (default) means to automatically calculate a correct value.
      Note that automatic calculation differs between
      perspective projection (looks at the world size and whether shadow volumes are possible)
      and orthographic projection (uses Default2DProjectionNear). }
    property ProjectionFar: Single read FProjectionFar write SetProjectionFar {$ifdef FPC}default 0{$endif};

    { Perspective or orthographic projection.
      Depending on it, we either use @link(Perspective) or @link(Orthographic) settings. }
    property ProjectionType: TProjectionType
      read FProjectionType write SetProjectionType default ptPerspective;

    { Perspective projection properties, used only if @link(ProjectionType) is ptPerspective. }
    property Perspective: TCastlePerspective read FPerspective;

    { Orthographic projection properties, used only if @link(ProjectionType) is ptOrthographic. }
    property Orthographic: TCastleOrthographic read FOrthographic;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastlePerspective --------------------------------------------------------- }

constructor TCastlePerspective.Create(AOwner: TComponent);
begin
  inherited;
  FFieldOfView := DefaultFieldOfView;
  FFieldOfViewAxis := DefaultFieldOfViewAxis;
end;

procedure TCastlePerspective.SetFieldOfView(const Value: Single);
begin
  if FFieldOfView <> Value then
  begin
    FFieldOfView := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastlePerspective.SetFieldOfViewAxis(const Value: TFieldOfViewAxis);
begin
  if FFieldOfViewAxis <> Value then
  begin
    FFieldOfViewAxis := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastlePerspective.InternalSetEffectiveFieldOfView(const AEffectiveFieldOfView: TVector2);
begin
  FEffectiveFieldOfView := AEffectiveFieldOfView;
end;

function TCastlePerspective.IsStoredFieldOfView: Boolean;
begin
  { Seems like this is the only way to avoid always serializing FieldOfView.
    Possibly displaying it in object inspector always modifies it a bit,
    due to rounding when displaying?
    TODO: Is this really necessary still? }
  Result := not SameValue(FFieldOfView, DefaultFieldOfView);
end;

function TCastlePerspective.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'FieldOfView') or
     (PropertyName = 'FieldOfViewAxis') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleOrthographic --------------------------------------------------------- }

constructor TCastleOrthographic.Create(AOwner: TComponent);
begin
  inherited;
  FScale := 1;
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleOrthographic.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleOrthographic.SetOrigin(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FOrigin, Value) then
  begin
    FOrigin := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastleOrthographic.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastleOrthographic.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastleOrthographic.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    if Value <= 0 then
      WritelnWarning('Orthographic projection scale (Camera.Orthographic.Scale) should be > 0, but is being set to %f', [
        Value
      ]);
    FScale := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastleOrthographic.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Camera.VisibleParameterChange;
  end;
end;

procedure TCastleOrthographic.InternalSetEffectiveRect(const AEffectiveRect: TFloatRectangle);
begin
  if ((AEffectiveRect.Width <= 0) or (AEffectiveRect.Height <= 0)) and (not WarningEffectiveSizeZeroDone) then
  begin
    WritelnWarning('Orthographic projection effective width and height (calculated based on Camera.Orthographic.Width,Height,Scale and viewport size) should be > 0, but are %f x %f (further warnings about it will be supressed, to not spam log)', [
      AEffectiveRect.Width,
      AEffectiveRect.Height
    ]);
    WarningEffectiveSizeZeroDone := true;
  end;
  FEffectiveRect := AEffectiveRect;
end;

function TCastleOrthographic.EffectiveWidth: Single;
begin
  Result := FEffectiveRect.Width;
end;

function TCastleOrthographic.EffectiveHeight: Single;
begin
  Result := FEffectiveRect.Height;
end;

function TCastleOrthographic.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Width') or
     (PropertyName = 'Height') or
     (PropertyName = 'OriginPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleOrthographic.CustomSerialization(const SerializationProcess: TSerializationProcess);
var
  ScaleValue: Single;
begin
  inherited;
  ScaleValue := 1;
  SerializationProcess.ReadWriteSingle('Scale', ScaleValue, false);
  if ScaleValue <> 1 then
  begin
    WritelnWarning('Upgrading deprecated TCastleOrthographic.Scale to just multiply TCastleOrthographic.Width and TCastleOrthographic.Height. Width x Height: old %f x %f -> new %f x %f.', [
      Width,
      Height,
      Width * ScaleValue,
      Height * ScaleValue
    ]);
    Width  := Width * ScaleValue;
    Height := Height * ScaleValue;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleCamera -------------------------------------------------------------- }

constructor TCastleCamera.Create(AOwner: TComponent);
begin
  CommonCreate(AOwner, CastleDesignMode);
end;

constructor TCastleCamera.InternalCreateNonDesign(const AOwner: TComponent);
begin
  CommonCreate(AOwner, false);
end;

procedure TCastleCamera.CommonCreate(const AOwner: TComponent; const AShowGizmo: Boolean);
var
  Gizmo: TInternalCastleEditorGizmo;

  function BuildGizmo: TX3DRootNode;
  begin
    Result := TX3DRootNode.Create;
    Result.AddChildren(Gizmo.LinesShape(TCoordinateNode(FGizmoCoord), [
      2, 2, 2, 2,
      2, 2, 2, 2,
      2, 2
    ]));
    UpdateGizmoCoord;
  end;

begin
  inherited Create(AOwner);

  { Override orientation, this is more intuitive for cameras. }
  Orientation := DefaultCameraOrientation;

  FProjectionType := ptPerspective;

  { This is not necessary,
    as this is actually the effect of default state of TCastleTransform.

    Moreover, this would set Rotation to (0,0,1,0)
    (because OrientationQuaternionFromDirectionUp uses DefaultUp for axis
    when there's no need rotation), causing bugs when reading back
    for serialized JSON. As RotationPersistent.InternalDefaultValue
    would remain at (0,0,0,0). Thus Z=0 is not saved to JSON (as it's default)
    but after creation+deserialization Z=1.

    We could workaround it by changing RotationPersistent.InternalDefaultValue,
    but actually it's easiest to not modify it at all.
  SetView(
    TVector3.Zero,
    DefaultCameraDirection,
    DefaultCameraUp); }

  FGravityUp := DefaultCameraUp;
  FProjectionMatrix := TMatrix4.Identity; // any sensible initial value
  FFrustum.Init(TMatrix4.Identity); // any sensible initial value

  FPerspective := TCastlePerspective.Create(Self);
  FPerspective.Camera := Self;
  FPerspective.Name := 'Perspective';
  FPerspective.SetSubComponent(true);

  FOrthographic := TCastleOrthographic.Create(Self);
  FOrthographic.Camera := Self;
  FOrthographic.Name := 'Orthographic';
  FOrthographic.SetSubComponent(true);

  if AShowGizmo then
  begin
    Gizmo := TInternalCastleEditorGizmo.Create(Self);
    Gizmo.SetParent(Self);
    Gizmo.LoadVisualization(BuildGizmo);
    Gizmo.SelectBoxTranslation := CastleVectors.Vector3(0, 0, -0.5); // matches nicely our visualization set by UpdateGizmoCoord
    { No need for icon anymore, gizmo is recognizeable enough.
      And it removes worry about icon size in 2D games, where it would be too small. }
    // Gizmo.SetIconUrl(InternalCastleDesignData + 'gizmos/camera/camera.png');
    Add(Gizmo);
  end;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleCamera.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleCamera.Assign(Source: TPersistent);
var
  SourceCamera: TCastleCamera;
begin
  if Source is TCastleCamera then
  begin
    SourceCamera := TCastleCamera(Source);

    { Copies non-temporary properties (in particular, the published properties). }
    Translation                 := SourceCamera.Translation;
    Rotation                    := SourceCamera.Rotation;
    GravityUp                   := SourceCamera.GravityUp;
    ProjectionNear              := SourceCamera.ProjectionNear;
    ProjectionFar               := SourceCamera.ProjectionFar;
    ProjectionType              := SourceCamera.ProjectionType;
    Perspective.FieldOfView     := SourceCamera.Perspective.FieldOfView;
    Perspective.FieldOfViewAxis := SourceCamera.Perspective.FieldOfViewAxis;
    Orthographic.Origin         := SourceCamera.Orthographic.Origin;
    Orthographic.Width          := SourceCamera.Orthographic.Width;
    Orthographic.Height         := SourceCamera.Orthographic.Height;
  end else
    { Call inherited ONLY when you cannot handle Source class,
      to raise EConvertError from TPersistent.Assign. }
    inherited Assign(Source);
end;

procedure TCastleCamera.SetGravityUp(const Value: TVector3);
begin
  FGravityUp := Value.Normalize;
end;

function TCastleCamera.Matrix: TMatrix4;
var
  P, D, U: TVector3;
begin
  GetWorldView(P, D, U);
  Result := LookDirMatrix(P, D, U);
end;

function TCastleCamera.RotationMatrix: TMatrix4;
var
  P, D, U: TVector3;
begin
  GetWorldView(P, D, U);
  Result := FastLookDirMatrix(D, U);
end;

function TCastleCamera.MatrixInverse: TMatrix4;
begin
  if not Matrix.TryInverse(Result) then
    raise Exception.Create('Cannot invert camera matrix, possibly it contains scaling to zero');
end;

procedure TCastleCamera.RecalculateFrustum;
begin
  FFrustum.Init(ProjectionMatrix, Matrix);
end;

procedure TCastleCamera.SetProjectionMatrix(const Value: TMatrix4);
begin
  FProjectionMatrix := Value;
  RecalculateFrustum;
end;

procedure TCastleCamera.CustomRay(
  const ViewportRect: TFloatRectangle;
  const WindowPosition: TVector2;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
var
  APos, ADir, AUp: TVector3;
begin
  GetWorldView(APos, ADir, AUp);

  PrimaryRay(
    WindowPosition[0] - ViewportRect.Left,
    WindowPosition[1] - ViewportRect.Bottom,
    ViewportRect.Width, ViewportRect.Height,
    APos, ADir, AUp,
    Projection,
    RayOrigin, RayDirection);
end;

procedure TCastleCamera.Init(
  const AInitialPosition, AInitialDirection, AInitialUp, AGravityUp: TVector3);
begin
  SetWorldView(AInitialPosition, AInitialDirection, AInitialUp);
  GravityUp := AGravityUp;
end;

procedure TCastleCamera.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if FAnimation then
  begin
    AnimationCurrentTime := AnimationCurrentTime + SecondsPassed;
    if AnimationCurrentTime > AnimationEndTime then
    begin
      FAnimation := false;
      { When animation ended, make sure you're exactly at the final view. }
      SetWorldView(AnimationEndPosition, AnimationEndDirection, AnimationEndUp);
    end else
    begin
      SetWorldView(
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginPosition , AnimationEndPosition),
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginDirection, AnimationEndDirection),
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginUp       , AnimationEndUp));
    end;
  end;
end;

procedure TCastleCamera.AnimateTo(const APos, ADir, AUp: TVector3; const Time: TFloatTime);
begin
  GetWorldView(
    AnimationBeginPosition,
    AnimationBeginDirection,
    AnimationBeginUp);

  AnimationEndPosition := APos;
  AnimationEndDirection := ADir;
  AnimationEndUp := AUp;

  AnimationEndTime := Time;
  AnimationCurrentTime := 0;
  { No point in doing animation (especially since it blocks camera movement
    for Time seconds) if we're already there. }
  FAnimation := not (
    TVector3.Equals(AnimationBeginPosition , AnimationEndPosition) and
    TVector3.Equals(AnimationBeginDirection, AnimationEndDirection) and
    TVector3.Equals(AnimationBeginUp       , AnimationEndUp));
end;

procedure TCastleCamera.AnimateTo(const OtherCamera: TCastleCamera; const Time: TFloatTime);
var
  APos, ADir, AUp: TVector3;
begin
  OtherCamera.GetWorldView(APos, ADir, AUp);
  AnimateTo(APos, ADir, AUp, Time);
end;

function TCastleCamera.Animation: boolean;
begin
  Result := FAnimation;
end;

procedure TCastleCamera.SetProjectionNear(const Value: Single);
begin
  if FProjectionNear <> Value then
  begin
    FProjectionNear := Value;
    VisibleParameterChange;
  end;
end;

procedure TCastleCamera.SetProjectionFar(const Value: Single);
begin
  if FProjectionFar <> Value then
  begin
    FProjectionFar := Value;
    VisibleParameterChange;
  end;
end;

procedure TCastleCamera.SetProjectionType(const Value: TProjectionType);
begin
  if FProjectionType <> Value then
  begin
    FProjectionType := Value;
    VisibleParameterChange;
  end;
end;

function TCastleCamera.EffectiveProjectionNear: Single;
begin
  { Sometimes we can return something valid (usually non-zero)
    even when ProjectionNear is zero (indicating "autocalculate")
    before the 1st InternalSetEffectiveProjection call.

    This allows to depend on EffectiveProjectionNear more,
    which matters for CGE editor design-time viewport manipulations in 2D,
    that refer to Camera.EffectiveProjectionNear.

    So instead of just returning FEffectiveProjectionNear,
    we duplicate part of the logic that TCastleViewport.ApplyProjection does.

    Still, trust FEffectiveProjectionNear more (when it is set),
    e.g. because here (actually in EffectiveProjectionFar)
    we don't account for special logic for design-time cameras having GetDefaultProjectionFar.
  }
  if FEffectiveProjectionNear <> 0 then
    Exit(FEffectiveProjectionNear);

  if FProjectionNear <> 0 then
    Result := FProjectionNear
  else
  if FProjectionType = ptOrthographic then
    Result := Default2DProjectionNear
  else
    Result := FEffectiveProjectionNear;
end;

function TCastleCamera.EffectiveProjectionFar: Single;
begin
  if FEffectiveProjectionFar <> 0 then
    Exit(FEffectiveProjectionFar);

  if FProjectionFar <> 0 then
    Result := FProjectionFar
  else
  if FProjectionType = ptOrthographic then
    Result := Default2DProjectionFar
  else
    Result := FEffectiveProjectionFar;
end;

procedure TCastleCamera.InternalSetEffectiveProjection(
  const AEffectiveProjectionNear, AEffectiveProjectionFar: Single);
begin
  FEffectiveProjectionNear := AEffectiveProjectionNear;
  FEffectiveProjectionFar := AEffectiveProjectionFar;
end;

function TCastleCamera.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'GravityUpPersistent') or
     (PropertyName = 'ProjectionFar') or
     (PropertyName = 'ProjectionNear') or
     (PropertyName = 'ProjectionType') or
     (PropertyName = 'Orthographic') or
     (PropertyName = 'Perspective') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCamera.CustomSerialization(const SerializationProcess: TSerializationProcess);
const
  DefaultCameraPosition: TVector3 = (X: 0; Y: 0; Z: 0);
var
  InitialPosition, InitialDirection, InitialUp: TVector3;
begin
  inherited;

  { Support old designs, before new-cameras branch, with old camera properties. }

  InitialPosition := DefaultCameraPosition;
  InitialDirection := DefaultCameraDirection;
  InitialUp := DefaultCameraUp;

  SerializationProcess.ReadWriteVector('InitialPositionPersistent', InitialPosition, DefaultCameraPosition, false);
  SerializationProcess.ReadWriteVector('InitialDirectionPersistent', InitialDirection, DefaultCameraDirection, false);
  SerializationProcess.ReadWriteVector('InitialUpPersistent', InitialUp, DefaultCameraUp, false);

  { Apply deprecated InitialXxx props only if they have been modified by the design.
    Otherwise, they would override the values read
    from non-deprecated Translation and Rotation. }
  if (not TVector3.PerfectlyEquals(InitialPosition, DefaultCameraPosition)) or
     (not TVector3.PerfectlyEquals(InitialDirection, DefaultCameraDirection)) or
     (not TVector3.PerfectlyEquals(InitialUp, DefaultCameraUp)) then
  begin
    { Old cameras with InitialPosition/Direction/Up could not be a child
      of some TCastleTransform.
      Does it matter if we use SetView or SetWorldView here?
      Only when Viewport.Items had some transformation. }
    SetWorldView(InitialPosition, InitialDirection, InitialUp);
  end;
end;

procedure TCastleCamera.VisibleParameterChange;
begin
  { What to do when camera parameter, like projection fov, changes?
    For now we just cause vcVisibleGeometry because we just want to cause
    InternalOnCameraChanged. }
  VisibleChangeHere([vcVisibleGeometry]);
end;

procedure TCastleCamera.VisibleChangeHere(const Changes: TVisibleChanges);
begin
  inherited;

  { When e.g. Translation of camera changes, make sure to call InternalOnCameraChanged.
    This way e.g. CGE editor gizmo is updated when we get closer/further to it. }
  if vcVisibleGeometry in Changes then
    if Assigned(InternalOnCameraChanged) then
      InternalOnCameraChanged(Self);
end;

procedure TCastleCamera.UpdateGizmoCoord;
const
  { Camera plane is shown at EffectiveProjectionNear, unless it is smaller than this. }
  MinDistanceToPlane = 1;
  UpArrowSize = 0.1; // fraction of upper plane line to take for arrow
var
  PlanePoints: array [0..3] of TVector3;
  UpArrowPoints: array [0..2] of TVector3;
  PerspX, PerspY, PlaneZ: Single;
begin
  { We use LastXxx to avoid repeating this calculation, and updating gizmo points,
    every frame. In practice gizmo changes seldom -- e.g. if camera params change
    or viewport sizes change. }
  if (LastGizmoEffectiveProjectionNear <> EffectiveProjectionNear) or
     (not LastGizmoEffectiveRect.PerfectlyEquals(Orthographic.EffectiveRect)) or
     (not TVector2.PerfectlyEquals(LastGizmoEffectiveFov, Perspective.EffectiveFieldOfView)) then
  begin
    LastGizmoEffectiveProjectionNear := EffectiveProjectionNear;
    LastGizmoEffectiveRect := Orthographic.EffectiveRect;
    LastGizmoEffectiveFov := Perspective.EffectiveFieldOfView;
    //WritelnLog('Camera %s gizmo update', [Name]);

    PlaneZ := -Max(EffectiveProjectionNear, MinDistanceToPlane);

    case ProjectionType of
      ptOrthographic:
        begin
          PlanePoints[0] := CastleVectors.Vector3(Orthographic.EffectiveRect.Left , Orthographic.EffectiveRect.Bottom, PlaneZ);
          PlanePoints[1] := CastleVectors.Vector3(Orthographic.EffectiveRect.Right, Orthographic.EffectiveRect.Bottom, PlaneZ);
          PlanePoints[2] := CastleVectors.Vector3(Orthographic.EffectiveRect.Right, Orthographic.EffectiveRect.Top   , PlaneZ);
          PlanePoints[3] := CastleVectors.Vector3(Orthographic.EffectiveRect.Left , Orthographic.EffectiveRect.Top   , PlaneZ);
        end;
      ptPerspective:
        begin
          PerspX := Tan(Perspective.EffectiveFieldOfView.X / 2) * Abs(PlaneZ);
          PerspY := Tan(Perspective.EffectiveFieldOfView.Y / 2) * Abs(PlaneZ);
          PlanePoints[0] := CastleVectors.Vector3(-PerspX, -PerspY, PlaneZ);
          PlanePoints[1] := CastleVectors.Vector3( PerspX, -PerspY, PlaneZ);
          PlanePoints[2] := CastleVectors.Vector3( PerspX,  PerspY, PlaneZ);
          PlanePoints[3] := CastleVectors.Vector3(-PerspX,  PerspY, PlaneZ);
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleCamera.UpdateGizmoCoord:ProjectionType?');
      {$endif}
    end;
    UpArrowPoints[0] := CastleVectors.Vector3(Lerp(0.5 - UpArrowSize, PlanePoints[3].X, PlanePoints[2].X), PlanePoints[3].Y, PlaneZ);
    UpArrowPoints[2] := CastleVectors.Vector3(Lerp(0.5 + UpArrowSize, PlanePoints[3].X, PlanePoints[2].X), PlanePoints[3].Y, PlaneZ);
    UpArrowPoints[1] := CastleVectors.Vector3(Lerp(0.5              , PlanePoints[3].X, PlanePoints[2].X),
      PlanePoints[3].Y + PointsDistance(UpArrowPoints[0], UpArrowPoints[2]) / 2,
      PlaneZ);
    TCoordinateNode(FGizmoCoord).SetPoint([
      CastleVectors.Vector3(0, 0, 0), PlanePoints[0],
      CastleVectors.Vector3(0, 0, 0), PlanePoints[1],
      CastleVectors.Vector3(0, 0, 0), PlanePoints[2],
      CastleVectors.Vector3(0, 0, 0), PlanePoints[3],
      PlanePoints[0], PlanePoints[1],
      PlanePoints[1], PlanePoints[2],
      PlanePoints[2], PlanePoints[3],
      PlanePoints[3], PlanePoints[0],
      UpArrowPoints[0], UpArrowPoints[1],
      UpArrowPoints[1], UpArrowPoints[2]
    ]);
  end;
end;

function TCastleCamera.InternalProjection(const BoxEvent: TBox3DEvent; const ViewportWidth, ViewportHeight: Single;
  const DesignCamera: Boolean): TProjection;

  { Update Result.Dimensions and Orthographic.EffectiveXxx
    based on Orthographic.Width, Height and viewport size. }
  procedure UpdateOrthographicDimensions;
  var
    EffectiveProjectionWidth, EffectiveProjectionHeight: Single;

    procedure CalculateDimensions;
    begin
      { Apply Orthographic.Scale here,
        this way it scales around Origin (e.g. around middle, when Origin is 0.5,0.5) }
      {$warnings off} // using deprecated to keep it working
      EffectiveProjectionWidth  := EffectiveProjectionWidth  * Orthographic.Scale;
      EffectiveProjectionHeight := EffectiveProjectionHeight * Orthographic.Scale;
      {$warnings on}

      Result.Dimensions.Width  := EffectiveProjectionWidth;
      Result.Dimensions.Height := EffectiveProjectionHeight;
      Result.Dimensions.Left   := - Orthographic.Origin.X * EffectiveProjectionWidth;
      Result.Dimensions.Bottom := - Orthographic.Origin.Y * EffectiveProjectionHeight;
    end;

  begin
    if (Orthographic.Width = 0) and
       (Orthographic.Height = 0) then
    begin
      EffectiveProjectionWidth := ViewportWidth;
      EffectiveProjectionHeight := ViewportHeight;
      CalculateDimensions;
    end else
    if Orthographic.Width = 0 then
    begin
      EffectiveProjectionWidth := Orthographic.Height * ViewportWidth / ViewportHeight;
      EffectiveProjectionHeight := Orthographic.Height;
      CalculateDimensions;
    end else
    if Orthographic.Height = 0 then
    begin
      EffectiveProjectionWidth := Orthographic.Width;
      EffectiveProjectionHeight := Orthographic.Width * ViewportHeight / ViewportWidth;
      CalculateDimensions;
    end else
    begin
      EffectiveProjectionWidth := Orthographic.Width;
      EffectiveProjectionHeight := Orthographic.Height;

      CalculateDimensions;

      {$warnings off} // using experimental to make it working
      if not Orthographic.Stretch then
        Result.Dimensions := TOrthoViewpointNode.InternalFieldOfView(
          Result.Dimensions,
          ViewportWidth,
          ViewportHeight);
      {$warnings on}

      EffectiveProjectionWidth := Result.Dimensions.Width;
      EffectiveProjectionHeight := Result.Dimensions.Height;
    end;

    Assert(Result.Dimensions.Width  = EffectiveProjectionWidth);
    Assert(Result.Dimensions.Height = EffectiveProjectionHeight);

    Orthographic.InternalSetEffectiveRect(Result.Dimensions);
  end;

  procedure GetBoxDistances(out MinBoxDistance, MaxBoxDistance: Single);
  var
    Box: TBox3D;
    Pos, Dir, Up: TVector3;
  begin
    Box := BoxEvent();
    GetWorldView(Pos, Dir, Up);
    Box.DirectionDistances(Pos, Dir, MinBoxDistance, MaxBoxDistance);
    { Multiply by 1.1, just to make it a bit larger to avoid imprecision at max depth }
    MaxBoxDistance := MaxBoxDistance * 1.1;
  end;

var
  MinBoxDistanceForOrtho, MaxBoxDistanceForOrtho: Single;

  { Calculate reasonable perspective projection near. }
  function GetDefaultProjectionNear(const CurrentProjectionType: TProjectionType): Single;
  var
    Radius: Single;
  begin
    if CurrentProjectionType = ptOrthographic then
    begin
      Result := MinBoxDistanceForOrtho;
    end else
    begin
      Radius := DefaultCameraRadius;
      Result := Radius * RadiusToProjectionNear;
    end;
  end;

  { Calculate reasonable perspective projection far.
    May return ZFarInfinity (0). }
  function GetDefaultProjectionFar(const CurrentProjectionType: TProjectionType): Single;
  //var
    //MinBoxDistance, MaxBoxDistance: Single;
  begin
    if CurrentProjectionType = ptOrthographic then
    begin
      Result := MaxBoxDistanceForOrtho;

      { Note that the design-time camera in 2D should have larger ProjectionFar than
        at run-time, to really see everything that runtime sees,
        because design-time camera Z is larger than runtime.

        But it doesn't require any special code anymore, as we adjust to Box. }
    end else
    begin
      { We used to have a smart calculation of it here, based on Box.
        And override with ZFarInfinity if ShadowVolumesPossible (usually true).

        But it is simpler to just use ZFarInfinity always:

        - It means that ShadowVolumesPossible don't matter,
          in particular it doesn't matter whether context has stencil buffer.
          This removes often "weird" differences due to subtle differences how
          TCastleControl and TCastleWindow initialize OpenGL context, we usually had
          ShadowVolumesPossible = false in editor and ShadowVolumesPossible = true
          in TCastleWindow. So ProjectionFarFinite was only observed in editor.

        - Minor reason: It avoids the need to calculate Viewport.ItemsBoundingBox
          at each frame, at least for the purpose of automatic projection
          for ProjectionFar for perspective.
          It was never observed in profiler,
          but in theory Viewport.ItemsBoundingBox may be costly when you have a lot of scenes.

        - Super-Minor reason: It's much easier, for implementation and from the user point of view,
          to just use ZFarInfinity.

        - Super-Minor reason: It also means user has a way to choose ZFarInfinity,
          despite our unfortunate choice of Camera.ProjectionFar = 0 meaning "autocalculate"
          and ZFarInfinity = 0. So user has no way to explictly request ZFarInfinity...
          except right now, "autocalculate" actually just implies ZFarInfinity.

          We could have solved it better though, using e.g. ZFarInfinity = -1.
          But no need anymore.

        Note: Only perspective projection supports ZFarInfinity, there's just no equivalent
        equation for orthographic projection to implement infinite z far.

        Old Note (when we had calculation based on Box): when box is empty (or has 0 sizes),
        ProjectionFar cannot be simply "any dummy value".
        It must be appropriately larger than GetDefaultProjectionNear
        to provide sufficient space for rendering Background node.
      }

      { Just for test, this is a proper code to calculate projection far based on world Box.
        Test this if you worry that ZFarInfinity causes visible imprecision in depth buffer
        (results of my tests: it does not). }
      {
      Box := BoxEvent();
      Box.PointDistances(WorldTranslation, MinBoxDistance, MaxBoxDistance);
      Result := MaxBoxDistance * 2;
      WritelnLog('Autocalculated ProjectionFar %f', [Result]);
      }

      Result := ZFarInfinity;
    end;
  end;

begin
  Result.ProjectionType := ProjectionType;

  Result.PerspectiveAnglesRad := TViewpointNode.InternalFieldOfView(
    Perspective.FieldOfView,
    Perspective.FieldOfViewAxis,
    ViewportWidth,
    ViewportHeight);
  Perspective.InternalSetEffectiveFieldOfView(
    Result.PerspectiveAnglesRad);

  if (ProjectionType = ptOrthographic) and
     ( (ProjectionNear <= 0) or
       (ProjectionFar <= 0) ) then
    GetBoxDistances(MinBoxDistanceForOrtho, MaxBoxDistanceForOrtho);

  { calculate Result.ProjectionNear }
  Result.ProjectionNear := ProjectionNear;
  if (Result.ProjectionNear = 0) or
     ((Result.ProjectionType = ptPerspective) and
       (Result.ProjectionNear <= 0) ) then
  begin
    Result.ProjectionNear := GetDefaultProjectionNear(Result.ProjectionType);
    // in perspective, effective ProjectionNear must be > 0
    Assert((Result.ProjectionNear > 0) or (Result.ProjectionType <> ptPerspective));
  end;

  { calculate Result.ProjectionFar }
  if ProjectionFar > 0 then
  begin
    { When ProjectionFar is non-zero on camera, use it unconditionally
      (do not override e.g. with ZFarInfinity, even if shadow volumes are possible).
      Reasons:

      - This is more intuitive for developer. We do what was requested.
        Maybe developer wants ProjectionFar to really limit player's view,
        for gameplay purposes.

      - Overriding with ZFarInfinity when shadow volumes are possible
        was also bad, because "shadow volumes are possible" may differ between
        CGE editor and runtime.

        TCastleControl is initialized without stencil, TCastleWindow with stencil.
        So CGE editor preview was honoring ProjectionFar,
        while runtime TCastleWindow was ignoring it, overriding with ZFarInfinity.
        Testcase: physics-joints in physics_j branch.
    }
    Result.ProjectionFar := ProjectionFar;
  end else
  begin
    Result.ProjectionFar := GetDefaultProjectionFar(Result.ProjectionType);
  end;

  InternalSetEffectiveProjection(
    Result.ProjectionNear,
    Result.ProjectionFar);

  { Calculate Result.Dimensions regardless of Result.ProjectionType,
    this way OnProjection can easily change projection type to orthographic. }
  UpdateOrthographicDimensions;

{
  WritelnLogMultiline('Projection', Format(
    'ProjectionType: %s' + NL +
    'Perspective Field of View (in degrees): %f x %f' + NL +
    'Orthographic Dimensions: %s' + NL +
    'Near: %f' + NL +
    'Far: %f', [
    ProjectionTypeToStr(Result.ProjectionType),
    Result.PerspectiveAngles.X,
    Result.PerspectiveAngles.Y,
    Result.Dimensions.ToString,
    Result.ProjectionNear,
    Result.ProjectionFar
  ]));
}
end;

procedure TCastleCamera.LocalRender(const Params: TRenderParams);
begin
  if FGizmoCoord <> nil then
  begin
    { Update EffectiveProjection to show latest projection values. }
    InternalProjection(Params.ProjectionBox,
      Params.ProjectionViewportWidth, Params.ProjectionViewportHeight,
      false);
    UpdateGizmoCoord;
  end;

  inherited;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
