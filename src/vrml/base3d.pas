{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base 3D object (TBase3D). }
unit Base3D;

interface

uses Classes, VectorMath, Frustum, Boxes3D,
  KambiClassUtils, KeysMouse, VRMLTriangle;

type
  { Various things that TBase3D.PrepareRender may prepare. }
  TPrepareRenderOption = (prBackground, prBoundingBox,
    prTrianglesListNotOverTriangulate,
    prTrianglesListOverTriangulate,
    prTrianglesListShadowCasters,
    prManifoldAndBorderEdges);
  TPrepareRenderOptions = set of TPrepareRenderOption;

  TTransparentGroup = (tgTransparent, tgOpaque, tgAll);
  TTransparentGroups = set of TTransparentGroup;

  { Shadow volumes helper, not depending on OpenGL. }
  TBaseShadowVolumes = class
  end;

  TBase3DListCore = class;

  { Represents a collision with a 3D objects (TBase3D descendants) tree. }
  T3DCollision = class
  public
    constructor Create;
    destructor Destroy; override;
  public
    { The path in the 3D objects tree leading from the root to the
      final colliding 3D object.

      For example, if your 3D tree is a list, and within
      this list is another list, and within this another list is your final
      colliding object (for example, some TVRMLGLScene instance),
      then Hierarchy will contain three items (in order: 1st list, 2nd list,
      TVRMLGLScene instance).

      For TBase3D.RayCollision and overrides, Hierarchy is never empty. }
    Hierarchy: TBase3DListCore;

    { The triangle that collides. This triangle is always a part of the last
      item on @link(Hierarchy) list. }
    Triangle: PVRMLTriangle;
  end;

  { Base 3D object, that can be managed by TKamSceneManager.
    All 3D objects should descend from this, this way we can easily
    insert them into the TKamSceneManager. }
  TBase3D = class(TComponent)
  private
    FCastsShadow: boolean;
    FExists: boolean;
    FCollides: boolean;
    FOnVisibleChange: TNotifyEvent;
    FCursor: TMouseCursor;
    FOnCursorChange: TNotifyEvent;
    procedure SetCursor(const Value: TMouseCursor);
  protected
    { In TBase3D class, just calls OnCursorChange event. }
    procedure CursorChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Is this object visible and colliding.

      Setting this to @false pretty much turns everything of this 3D object
      to "off". This is useful for objects that disappear completely from
      the level when something happens. You could just as well remove
      this object from TKamSceneManager.Items tree, but sometimes it's more
      comfortable to simply turn this property to @false.

      @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

    { Should this 3D object participate in collision detection.
      You can turn this off, useful to make e.g. "fake" walls
      (to some secret places on level).

      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection).

      @noAutoLinkHere }
    property Collides: boolean read FCollides write FCollides default true;

    { Bounding box of the 3D object.

      Should take into account both collidable and visible objects.
      For examples, invisible walls (not visible) and fake walls (not collidable)
      should all be accounted here.

      As it's a @italic(bounding) volume, it may naturally be slightly too large
      (although, for the same of various optimizations, you should try
      to make it as tight as reasonably possible.) For now, it's also OK
      to make it a little too small (nothing bad will happen).
      Although all currently implemeted descendants (TVRMLScene, TVRMLAnimation,
      more) guarantee it's never too small. }
    function BoundingBox: TBox3D; virtual; abstract;

    { Render given object. This is done only if @link(Exists).

      @param(Frustum May be used to optimize rendering, to not
        render the parts outside the Frustum.)

      @param(TransparentGroup
        Used to indicate that only opaque or only transparent
        parts should be rendered, just like for TVRMLGLScene.Render.)

      @param(InShadow If @true, means that we're using multi-pass
        shadowing technique (like shadow volumes),
        and currently doing the "shadowed" pass.

        Which means that most lights (ones with kambiShadows = TRUE)
        should be turned off, see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadows].)
    }
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); virtual;

    property CastsShadow: boolean read FCastsShadow write FCastsShadow
      default true;

    { Render shadow quads for all the things rendered by @link(Render).
      This is done only if @link(Exists) and @link(CastsShadow).

      It does shadow volumes culling inside (so ShadowVolumes should
      have FrustumCullingInit already initialized).

      ParentTransform and ParentTransformIsIdentity describe the transformation
      of this object in the 3D world.
      TBase3D objects may be organized in a hierarchy when
      parent transforms it's children. When ParentTransformIsIdentity,
      ParentTransform must be IdentityMatrix4Single (it's not guaranteed
      that when ParentTransformIsIdentity = @true, Transform value will be
      ignored !).

      @italic(Implementation note:) In @link(Render), it is usually possible
      to implement ParentTransform* by glPush/PopMatrix and Frustum.Move tricks.
      But RenderShadowVolume needs actual transformation explicitly:
      ShadowMaybeVisible needs actual box position in world coordinates,
      so bounding box has to be transformed by ParentTransform.
      And TVRMLGLScene.RenderShadowVolumeCore needs explicit ParentTransform
      to correctly detect front/back sides (for silhouette edges and
      volume capping). }
    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); virtual;

    { Prepare rendering (and other stuff) to execute fast.

      This prepares some things, making sure that
      appropriate methods execute as fast as possible.
      It's never required to call this method
      --- all things will be prepared "as needed" anyway.
      But this means that some calls may sometimes take a long time,
      e.g. the first @link(Render) call may take a long time because it may
      have to prepare display lists that will be reused in next @link(Render)
      calls. This may cause a strange behavior of the program: rendering of the
      first frame takes unusually long time (which confuses user, and
      also makes things like TGLWindow.DrawSpeed strange for a short
      time). So calling this procedure may be desirable.
      You may want to show to user that "now we're preparing
      the VRML scene --- please wait".

      For OpenGL rendered objects, this method ties this object
      to the current OpenGL context.
      But it doesn't change any OpenGL state or buffers contents
      (at most, it allocates some texture and display list names).

      @param(TransparentGroups For what TransparentGroup value
        we should prepare rendering resources. The idea is that
        you're often interested in rendering only with tgAll, or
        only with [tgTransparent, tgOpaque] --- so it would be a waste of
        resources and time to prepare for every possible TransparentGroup value.

        Note for TVRMLGLScene only:

        Note that for Optimizations <> roSceneAsAWhole
        preparing for every possible TransparentGroup value
        is actually not harmful. There's no additional use of resources,
        as the sum of [tgTransparent, tgOpaque] uses
        the same resources as [tgAll]. In other words,
        there's no difference in resource (and time) used between
        preparing for [tgTransparent, tgOpaque], [tgAll] or
        [tgTransparent, tgOpaque, tgAll] --- they'll all prepare the same
        things.)

      @param(Options What additional features (besides rendering)
        should be prepared to execute fast. See TPrepareRenderOption,
        the names should be self-explanatory (they refer to appropriate
        methods of TBase3D, TVRMLScene or TVRMLGLScene).)

      @param(ProgressStep Says that we should make Progress.Step calls
        (exactly PrepareRenderSteps times) during preparation.
        Useful to show progress bar to the user during long preparation.)  }
    procedure PrepareRender(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions;
      ProgressStep: boolean); virtual;

    { How many times PrepareRender will call Progress.Step.
      Useful only if you want to pass ProgressStep = @true to PrepareRender.
      In the base class TBase3D this just returns 0.  }
    function PrepareRenderSteps: Cardinal; virtual;

    { Key and mouse events. Return @true if you handled them.
      See also TUIControl analogous events.
      Note that our MouseMove gets 3D ray corresponding to mouse
      position on the screen (this is the ray for "picking" 3D objects
      pointed by the mouse).

      @groupBegin }
    function KeyDown(Key: TKey; C: char): boolean; virtual;
    function KeyUp(Key: TKey; C: char): boolean; virtual;
    function MouseDown(const Button: TMouseButton): boolean; virtual;
    function MouseUp(const Button: TMouseButton): boolean; virtual;
    function MouseMove(const RayOrigin, RayDirection: TVector3Single): boolean; virtual;
    { @groupEnd }

    { Idle event, for continously repeated tasks. }
    procedure Idle(const CompSpeed: Single); virtual;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      In this class this simply calls OnVisibleChange (if assigned). }
    procedure VisibleChange; virtual;

    { Called when some visible part of this control changes.
      This is usually used by the scene manager
      (to know when we need to redraw the control),
      so don't use it in your own programs directly.

      Be careful when handling this event, various changes may cause this,
      so be prepared to handle OnVisibleChange at every time.

      @seealso VisibleChange }
    property OnVisibleChange: TNotifyEvent
      read FOnVisibleChange write FOnVisibleChange;

    { Mouse cursor over this object. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Called when the @link(Cursor) of this control changes.
      This is usually used by the scene manager
      (to know when we need to redraw the control),
      so don't use it in your own programs directly. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;

    { Called when OpenGL context of the window is destroyed.
      This will be also automatically called from destructor.

      Control should clear here any resources that are tied to the GL context. }
    procedure GLContextClose; virtual;

    procedure GetCameraHeight(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle); virtual;

    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual;
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual;

    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual;
    function BoxCollision(const Box: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; virtual;

    { Check collision with a ray, building a T3DCollision result.
      Returns a collision as T3DCollision instance, or @nil if no collision.
      Caller is responsible for freeing the returned T3DCollision instance.

      This always returns the first collision with the 3D world, that is
      the one with smallest IntersectionDistance. For example, when
      implemented in TBase3DList, this checks collisions for all list items,
      and chooses the closest one. }
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision; virtual;
  end;

  TBase3DList = class;

  { List of base 3D objects (TBase3D instances).
    This allows you to group many 3D objects, and treat them as one TBase3D
    descendant (for example, to translate many 3D objects by a single
    TCustomTranslated3D.Child).

    This inherits from TObjectsList, getting many
    features like TList notification mechanism (useful in some situations).
    Usually you want to use TBase3DList instead, which is a wrapper around
    this class. }
  TBase3DListCore = class(TKamObjectList)
  private
    FOwner: TBase3DList;

    function GetItem(const I: Integer): TBase3D;
    procedure SetItem(const I: Integer; const Item: TBase3D);
  public
    constructor Create(const FreeObjects: boolean; const AOwner: TBase3DList);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    property Items[I: Integer]: TBase3D read GetItem write SetItem; default;

    { TBase3DList instance that owns this list.
      May be @nil, for example when this list is used by T3DCollision. }
    property Owner: TBase3DList read FOwner;
  end;

  { List of base 3D objects (TBase3D instances).

    This inherits from TBase3D class, so this list is itself a 3D object:
    it's a sum of all it's children 3D objects. }
  TBase3DList = class(TBase3D)
  private
    FList: TBase3DListCore;
    procedure ListVisibleChange(Sender: TObject);
    procedure ListCursorChange(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Add and remove items to the @link(List).
      @groupBegin }
    procedure Add(const Item: TBase3D);
    procedure Insert(const Index: Integer; const Item: TBase3D);
    procedure Remove(const Item: TBase3D);
    procedure Clear;
    { @groupEnd }

    function BoundingBox: TBox3d; override;
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); override;
    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;
    procedure PrepareRender(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions;
      ProgressStep: boolean); override;
    function PrepareRenderSteps: Cardinal; override;
    function KeyDown(Key: TKey; C: char): boolean; override;
    function KeyUp(Key: TKey; C: char): boolean; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    function MouseMove(const RayOrigin, RayDirection: TVector3Single): boolean; override;
    procedure Idle(const CompSpeed: Single); override;
    procedure GLContextClose; override;
    procedure GetCameraHeight(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
      out GroundItem: PVRMLTriangle); override;
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3d;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean; override;
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision; override;
  published
    { 3D objects inside.
      Freeing these items automatically removes them from this list. }
    property List: TBase3DListCore read FList;
  end;

implementation

uses SysUtils, KambiUtils, GL, KambiGLUtils;

{ T3DCollision ------------------------------------------------------------- }

constructor T3DCollision.Create;
begin
  inherited;
  Hierarchy := TBase3DListCore.Create(false, nil);
end;

destructor T3DCollision.Destroy;
begin
  FreeAndNil(Hierarchy);
end;

{ TBase3D -------------------------------------------------------------------- }

constructor TBase3D.Create(AOwner: TComponent);
begin
  inherited;
  FCastsShadow := true;
  FExists := true;
  FCollides := true;
  FCursor := mcDefault;
end;

destructor TBase3D.Destroy;
begin
  GLContextClose;
  inherited;
end;

procedure TBase3D.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup;
  InShadow: boolean);
begin
end;

procedure TBase3D.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
end;

procedure TBase3D.PrepareRender(TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions; ProgressStep: boolean);
begin
end;

function TBase3D.PrepareRenderSteps: Cardinal;
begin
  Result := 0;
end;

function TBase3D.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := false;
end;

function TBase3D.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := false;
end;

function TBase3D.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := false;
end;

function TBase3D.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := false;
end;

function TBase3D.MouseMove(const RayOrigin, RayDirection: TVector3Single): boolean;
begin
  Result := false;
end;

procedure TBase3D.Idle(const CompSpeed: Single);
begin
end;

procedure TBase3D.VisibleChange;
begin
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self);
end;

procedure TBase3D.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    CursorChange;
  end;
end;

procedure TBase3D.CursorChange;
begin
  if Assigned(OnCursorChange) then OnCursorChange(Self);
end;

procedure TBase3D.GLContextClose;
begin
end;

procedure TBase3D.GetCameraHeight(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);
begin
  IsAboveTheGround := false;
  { HeightAboveTheGround and GroundItem are undefined when
    IsAboveTheGround = false, but let's set them anyway (to ease debugging,
    things should better be predictable). }
  SqrHeightAboveTheGround := 0;
  GroundItem := nil;
end;

function TBase3D.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := true;
  NewPos := ProposedNewPos;
end;

function TBase3D.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := true;
end;

function TBase3D.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := true;
end;

function TBase3D.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := false;
end;

function TBase3D.SphereCollision(const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := false;
end;

function TBase3D.BoxCollision(const Box: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
begin
  Result := false;
end;

function TBase3D.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision;
begin
  Result := nil;
end;

{ TBase3DListCore ------------------------------------------------------------ }

constructor TBase3DListCore.Create(const FreeObjects: boolean; const AOwner: TBase3DList);
begin
  inherited Create(FreeObjects);
  FOwner := AOwner;
end;

procedure TBase3DListCore.Notify(Ptr: Pointer; Action: TListNotification);
var
  B: TBase3D;
begin
  inherited;

  if Owner <> nil then
  begin
    B := TBase3D(Ptr);

    case Action of
      lnAdded:
        begin
          { Make sure Owner.ListVisibleChange will be called
            when an item calls OnVisibleChange. }
          if B.OnVisibleChange = nil then
            B.OnVisibleChange := @Owner.ListVisibleChange;
          if B.OnCursorChange = nil then
            B.OnCursorChange := @Owner.ListCursorChange;

          { Register Owner to be notified of item destruction. }
          B.FreeNotification(Owner);
        end;
      lnExtracted, lnDeleted:
        begin
          if B.OnVisibleChange = @Owner.ListVisibleChange then
            B.OnVisibleChange := nil;
          if B.OnCursorChange = @Owner.ListCursorChange then
            B.OnCursorChange := nil;

          B.RemoveFreeNotification(Owner);
        end;
      else raise EInternalError.Create('TBase3DListCore.Notify action?');
    end;

    { This notification may get called during FreeAndNil(FList)
      in TBase3DList.Destroy. Then FList is already nil (as FreeAndNil
      first sets object to nil), and Owner.ListCursorChange
      may not be ready for this. }
    if Owner.FList <> nil then
      Owner.ListCursorChange(nil);
  end;
end;

function TBase3DListCore.GetItem(const I: Integer): TBase3D;
begin
  Result := TBase3D(inherited Items[I]);
end;

procedure TBase3DListCore.SetItem(const I: Integer; const Item: TBase3D);
begin
  (inherited Items[I]) := Item;
end;

{ TBase3DList ---------------------------------------------------------------- }

constructor TBase3DList.Create(AOwner: TComponent);
begin
  inherited;
  FList := TBase3DListCore.Create(false, Self);
end;

destructor TBase3DList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TBase3DList.Add(const Item: TBase3D);
begin
  List.Add(Item);
end;

procedure TBase3DList.Insert(const Index: Integer; const Item: TBase3D);
begin
  List.Insert(Index, Item);
end;

procedure TBase3DList.Remove(const Item: TBase3D);
begin
  List.Remove(Item);
end;

procedure TBase3DList.Clear;
begin
  List.Clear;
end;

function TBase3DList.BoundingBox: TBox3d;
var
  I: Integer;
begin
  Result := EmptyBox3D;
  if Exists then
    for I := 0 to List.Count - 1 do
      Box3dSumTo1st(Result, List[I].BoundingBox);
end;

procedure TBase3DList.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup; InShadow: boolean);
var
  I: Integer;
begin
  inherited;
  if Exists then
    for I := 0 to List.Count - 1 do
      List[I].Render(Frustum, TransparentGroup, InShadow);
end;

procedure TBase3DList.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  I: Integer;
begin
  inherited;
  if Exists and CastsShadow then
    for I := 0 to List.Count - 1 do
      List[I].RenderShadowVolume(ShadowVolumes,
        ParentTransformIsIdentity, ParentTransform);
end;

procedure TBase3DList.PrepareRender(TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions; ProgressStep: boolean);
var
  I: Integer;
begin
  inherited;
  for I := 0 to List.Count - 1 do
    List[I].PrepareRender(TransparentGroups, Options, ProgressStep);
end;

function TBase3DList.PrepareRenderSteps: Cardinal;
var
  I: Integer;
begin
  Result := inherited;
  for I := 0 to List.Count - 1 do
    Result += List[I].PrepareRenderSteps;
end;

function TBase3DList.KeyDown(Key: TKey; C: char): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].KeyDown(Key, C) then Exit(true);
end;

function TBase3DList.KeyUp(Key: TKey; C: char): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].KeyUp(Key, C) then Exit(true);
end;

function TBase3DList.MouseDown(const Button: TMouseButton): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].MouseDown(Button) then Exit(true);
end;

function TBase3DList.MouseUp(const Button: TMouseButton): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].MouseUp(Button) then Exit(true);
end;

function TBase3DList.MouseMove(const RayOrigin, RayDirection: TVector3Single): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  for I := 0 to List.Count - 1 do
    if List[I].MouseMove(RayOrigin, RayDirection) then Exit(true);
end;

procedure TBase3DList.Idle(const CompSpeed: Single);
var
  I: Integer;
begin
  inherited;

  for I := 0 to List.Count - 1 do
    List[I].Idle(CompSpeed);
end;

procedure TBase3DList.ListVisibleChange(Sender: TObject);
begin
  { when an Item calls OnVisibleChange, we'll call our own OnVisibleChange,
    to pass it up the tree (eventually, to the scenemanager, that will
    pass it by TUIControl similar OnVisibleChange mechanism to the container). }
  VisibleChange;
end;

procedure TBase3DList.ListCursorChange(Sender: TObject);
begin
  { when an Item calls OnCursorChange, we'll call our own OnCursorChange,
    to pass it up the tree (eventually, to the scenemanager, that will
    pass it by TUIControl similar OnCursorChange mechanism to the container). }

  { Hm, we could alternatively set our own Cursor := property,
    instead of directly calling CursorChange. SceneManager could
    then just use Items.Cursor as it's own Cursor value. }

  CursorChange;
end;

procedure TBase3DList.GLContextClose;
var
  I: Integer;
begin
  { this is called from inherited destrudtor, so check <> nil carefully }
  if FList <> nil then
  begin
    for I := 0 to List.Count - 1 do
      List[I].GLContextClose;
  end;

  inherited;
end;

procedure TBase3DList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { We have to remove a reference to the object from the List.
    This is crucial: TBase3DListCore.Notify,
    and e.g. GLContextClose call, assume that all objects on
    the List are always valid objects (no invalid references,
    even for a short time). }

  if (Operation = opRemove) and (AComponent is TBase3D) then
    List.DeleteAll(AComponent);
end;

procedure TBase3DList.GetCameraHeight(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);
var
  I: Integer;
  IsAboveThis: boolean;
  SqrHeightAboveThis: Single;
  GroundItemThis: PVRMLTriangle;
begin
  inherited;

  if Exists and Collides then
    for I := 0 to List.Count - 1 do
    begin
      List[I].GetCameraHeight(Position, GravityUp, TrianglesToIgnoreFunc,
        IsAboveThis, SqrHeightAboveThis, GroundItemThis);

      if IsAboveThis and
        ((not IsAboveTheGround) or (SqrHeightAboveThis < SqrHeightAboveTheGround)) then
      begin
        IsAboveTheGround := true;
        SqrHeightAboveTheGround := SqrHeightAboveThis;
        GroundItem := GroundItemThis;
      end;
    end;
end;

function TBase3DList.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  if Exists and Collides and (List.Count <> 0) then
  begin
    { We call MoveAllowed only one time, on the first scene.
      This means that only first scene collisions provide wall sliding.
      Collisions with other 3D objects will simply block the player.

      Otherwise, various MoveAllowed could modify NewPos
      making it colliding with other items, already checked. This would
      be wrong. So MoveAllowed is used only once, and for the others
      we use simple MoveAllowedSimple.

      TODO: this could be improved, to call MoveAllowed on the first scene
      where the simple move is not allowed. This would make it more general,
      although also slower. Is there any way to make it as fast and
      more general? }
    Result := List[0].MoveAllowed(OldPos, ProposedNewPos, NewPos,
      CameraRadius, TrianglesToIgnoreFunc);
    if not Result then Exit;

    for I := 1 to List.Count - 1 do
    begin
      Result := List[I].MoveAllowedSimple(OldPos, NewPos,
        CameraRadius, TrianglesToIgnoreFunc);
      if not Result then Exit;
    end;
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;
end;

function TBase3DList.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;

  if Exists and Collides then
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].MoveAllowedSimple(OldPos, ProposedNewPos,
        CameraRadius, TrianglesToIgnoreFunc);
      if not Result then Exit;
    end;
end;

function TBase3DList.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;

  if Exists and Collides then
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].MoveBoxAllowedSimple(OldPos, ProposedNewPos,
        ProposedNewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;
    end;
end;

function TBase3DList.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if Exists and Collides then
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
end;

function TBase3DList.SphereCollision(const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if Exists and Collides then
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
end;

function TBase3DList.BoxCollision(const Box: TBox3d;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if Exists and Collides then
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].BoxCollision(Box, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
end;

function TBase3DList.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): T3DCollision;
var
  I: Integer;
  NewIntersectionDistance: Single;
  NewResult: T3DCollision;
begin
  Result := nil;
  IntersectionDistance := 0; { Only to silence compiler warning }

  if Exists and Collides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      NewResult := List[I].RayCollision(
        NewIntersectionDistance, Ray0, RayVector, nil);
      if NewResult <> nil then
      begin
        if (Result = nil) or (NewIntersectionDistance < IntersectionDistance) then
        begin
          IntersectionDistance := NewIntersectionDistance;
          SysUtils.FreeAndNil(Result);
          Result := NewResult;
        end else
          FreeAndNil(NewResult);
      end;
    end;

    if Result <> nil then
      Result.Hierarchy.Insert(0, Self);
  end;
end;

end.
