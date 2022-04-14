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
    procedure SetFieldOfView(const Value: Single);
    procedure SetFieldOfViewAxis(const Value: TFieldOfViewAxis);
    function IsStoredFieldOfView: Boolean;
  private
    Camera: TCastleCamera;
  public
    const
      DefaultFieldOfView = Pi / 4;
      DefaultFieldOfViewAxis = faSmallest;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
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
    FEffectiveWidth, FEffectiveHeight: Single;
    FStretch, WarningEffectiveSizeZeroDone: Boolean;
    procedure SetOrigin(const Value: TVector2);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetScale(const Value: Single);
    procedure SetStretch(const Value: Boolean);
  private
    Camera: TCastleCamera;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Additional translation of the camera.
      The camera movement applied here is always scaled by
      the calculated orthographic projection width and height.

      By default this equals (0,0) which means that TCastleCamera.Position
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

    procedure InternalSetEffectiveSize(const W, H: Single);

    { Currently used projection width and height, calculated following
      the algorithm described at @link(Width) and @link(Height).
      @groupBegin }
    property EffectiveWidth: Single read FEffectiveWidth;
    property EffectiveHeight: Single read FEffectiveHeight;
    { @groupEnd }
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

              Note that the @link(TCastleCamera.Position) is considered to be relative
              to unadjusted @link(Width) and @link(Height), not to the adjusted
              @link(EffectiveWidth), @link(EffectiveHeight).
              In effect, when @link(Origin) is zero, the @link(TCastleCamera.Position) does not point
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

    { Scales the projection size derived from @link(Width) and @link(Height).

      The effect of this scale is also affected by the @link(Origin).
      When @link(Origin) is zero, this behaves like scaling around left-botttom corner
      of the viewport.
      When @link(Origin) is (0.5,0.5), this behaves like scaling around
      the middle of the viewport. }
    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1{$endif};

    { Allow non-proportional stretch of projection.
      In effect the @link(Width) and @link(Height)
      (if both non-zero) are applied directly, without correcting them to follow
      aspect ratio of the viewport. }
    property Stretch: Boolean read FStretch write SetStretch default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Camera determines viewer position and orientation in the viewport.

    You can create instances of this class yourself,
    and add them to @link(TCastleViewport.Items),
    and set them as @link(TCastleViewport.Camera).

    Also, an instance of this class is automatically created inside
    @link(TCastleViewport.Camera) whenever you access it.
    And @link(TCastleViewport.Camera) is automatically added
    to @link(TCastleViewport.Items), if it's not there already
    (this also covers the case when you changed @link(TCastleViewport.Items)
    to something completely different, to watch other world in the viewport
    --- the camera will move there).

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
    FInitialPosition: TVector3;
    FInitialDirection: TVector3;
    FInitialUp: TVector3;

    procedure RecalculateFrustum;

    procedure SetGravityUp(const Value: TVector3);

    { Setter of the @link(ProjectionMatrix) property. }
    procedure SetProjectionMatrix(const Value: TMatrix4);

    procedure SetProjectionNear(const Value: Single);
    procedure SetProjectionFar(const Value: Single);
    procedure SetProjectionType(const Value: TProjectionType);

    { These are only used when deserializing, to support old design files.
      We'll later set Translation and Rotation based on them. }
    property InitialPosition: TVector3 read FInitialPosition write FInitialPosition;
    property InitialDirection: TVector3 read FInitialDirection write FInitialDirection;
    property InitialUp: TVector3 read FInitialUp write FInitialUp;
  private
    procedure VisibleChange;
  protected
    procedure Loaded; override;
  public
    { Associated viewport.
      Do not set this directly, instead always set @link(TCastleViewport.Navigation).
      @exclude }
    InternalViewport: TCastleUserInterface;

    InternalOnSceneBoundViewpointChanged,
    InternalOnSceneBoundViewpointVectorsChanged,
    InternalOnSceneBoundNavigationInfoChanged,
    InternalOnCameraChanged: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Express current view as camera vectors: position, direction, up and gravity.

      Returned Dir and Up must be orthogonal.
      Returned Dir and Up and GravityUp are already normalized. }
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3); overload;

    { Set camera view from vectors: position, direction, up and gravity.

      Direction, Up and GravityUp do not have to be normalized,
      we will normalize them internally if necessary.
      But make sure they are non-zero.

      We will automatically fix Direction and Up to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3;
      const AdjustUp: boolean = true); overload;

    { "Up" direction of the world in which player moves.
      Always normalized (when setting this property, we take
      care to normalize the provided vector).

      This determines in which direction @link(TCastleWalkNavigation.Gravity) works.

      This is also the "normal" value for @link(Up) --- one that means that player is looking
      straight foward. This is used for features like PreferGravityUpForRotations
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
      AGravityUp: TVector3); deprecated 'use SetView';

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    procedure Free; deprecated 'do not Free camera instance explicitly, only the TCastleViewport should create and destroy TCastleViewport.Camera; this method does nothing now';

    { Currently used projection near.
      Derived from @link(ProjectionNear) and possibly scene sizes. }
    property EffectiveProjectionNear: Single read FEffectiveProjectionNear;
    { Currently used projection far.
      Derived from @link(ProjectionFar) and possibly scene sizes.
      May be equal ZFarInfinity. }
    property EffectiveProjectionFar: Single read FEffectiveProjectionFar;
    // @exclude
    procedure InternalSetEffectiveProjection(
      const AEffectiveProjectionNear, AEffectiveProjectionFar: Single);
  published
    { Projection near plane distance.

      For perspective projection, values <= 0 are invalid,
      and imply that a suitable value should actually be automatically
      calculated (looking at the world size).

      For orthographic projection, all values are valid and reasonable,
      including 0 and < 0 values. }
    property ProjectionNear: Single read FProjectionNear write SetProjectionNear {$ifdef FPC}default 0{$endif};

    { Projection far plane distance.
      Use 0 to auto-calculate this each frame, based on world bounding box.
      If shadow volumes are used, this may be overridden to be infinite. }
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
    Camera.VisibleChange;
  end;
end;

procedure TCastlePerspective.SetFieldOfViewAxis(const Value: TFieldOfViewAxis);
begin
  if FFieldOfViewAxis <> Value then
  begin
    FFieldOfViewAxis := Value;
    Camera.VisibleChange;
  end;
end;

function TCastlePerspective.IsStoredFieldOfView: Boolean;
begin
  { Seems like this is the only way to avoid always serializing FieldOfView.
    Possibly displaying it in object inspector always modifies it a bit,
    due to rounding when displaying? }
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
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Camera.VisibleChange;
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
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.InternalSetEffectiveSize(const W, H: Single);
begin
  if ((W <= 0) or (H <= 0)) and (not WarningEffectiveSizeZeroDone) then
  begin
    WritelnWarning('Orthographic projection effective width and height (calculated based on Camera.Orthographic.Width,Height,Scale and viewport size) should be > 0, but are %f x %f (further warnings about it will be supressed, to not spam log)', [
      W, H
    ]);
    WarningEffectiveSizeZeroDone := true;
  end;
  FEffectiveWidth := W;
  FEffectiveHeight := H;
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

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleCamera -------------------------------------------------------------- }

constructor TCastleCamera.Create(AOwner: TComponent);
begin
  inherited;

  { Override orientation, this is more intuitive for cameras. }
  Orientation := otUpYDirectionMinusZ;

  FProjectionType := ptPerspective;
  SetView(
    TVector3.Zero,
    DefaultCameraDirection,
    DefaultCameraUp,
    DefaultCameraUp);
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

  FInitialPosition := TVector3.Zero;
  FInitialDirection := DefaultCameraDirection;
  FInitialUp := DefaultCameraUp;

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
    SetView(
      SourceCamera.Position,
      SourceCamera.Direction,
      SourceCamera.Up,
      SourceCamera.GravityUp);
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

procedure TCastleCamera.GetView(out APos, ADir, AUp, AGravityUp: TVector3);
begin
  GetView(APos, ADir, AUp);
  AGravityUp := FGravityUp;
end;

procedure TCastleCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3;
  const AdjustUp: boolean = true);
begin
  GravityUp := AGravityUp;
  SetView(APos, ADir, AUp, AdjustUp);
end;

procedure TCastleCamera.SetGravityUp(const Value: TVector3);
begin
  FGravityUp := Value.Normalize;
end;

procedure TCastleCamera.VisibleChange;
begin
  if Assigned(InternalOnCameraChanged) then
    InternalOnCameraChanged(Self);
end;

function TCastleCamera.Matrix: TMatrix4;
begin
  Result := LookDirMatrix(Position, Direction, Up);
end;

function TCastleCamera.RotationMatrix: TMatrix4;
begin
  Result := FastLookDirMatrix(Direction, Up);
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
  GetView(APos, ADir, AUp);

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
  SetView(AInitialPosition, AInitialDirection, AInitialUp, AGravityUp);
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
      SetView(AnimationEndPosition, AnimationEndDirection, AnimationEndUp);
    end else
    begin
      SetView(
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginPosition , AnimationEndPosition),
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginDirection, AnimationEndDirection),
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginUp       , AnimationEndUp));
    end;
  end;
end;

procedure TCastleCamera.AnimateTo(const APos, ADir, AUp: TVector3; const Time: TFloatTime);
begin
  GetView(
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
  OtherCamera.GetView(APos, ADir, AUp);
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
    VisibleChange;
  end;
end;

procedure TCastleCamera.SetProjectionFar(const Value: Single);
begin
  if FProjectionFar <> Value then
  begin
    FProjectionFar := Value;
    VisibleChange;
  end;
end;

procedure TCastleCamera.SetProjectionType(const Value: TProjectionType);
begin
  if FProjectionType <> Value then
  begin
    FProjectionType := Value;
    VisibleChange;
  end;
end;

procedure TCastleCamera.Free;
begin
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

procedure TCastleCamera.Loaded;
begin
  inherited; //< important, as inherited removes csLoading from ComponentState

  { Apply deprecated InitialXxx props only if they have been modified by the design.
    Otherwise, they would override the values read
    from non-deprecated Translation and Rotation. }
  if (not TVector3.PerfectlyEquals(FInitialPosition, TVector3.Zero)) or
     (not TVector3.PerfectlyEquals(FInitialDirection, DefaultCameraDirection)) or
     (not TVector3.PerfectlyEquals(FInitialUp, DefaultCameraUp)) then
  begin
    SetView(InitialPosition, InitialDirection, InitialUp);
    // reset to defaults, to not store in design on save
    FInitialPosition := TVector3.Zero;
    FInitialDirection := DefaultCameraDirection;
    FInitialUp := DefaultCameraUp;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
