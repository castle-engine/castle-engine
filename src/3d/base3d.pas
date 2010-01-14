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

uses Classes, VectorMath, Frustum, Boxes3D, UIControls, Contnrs,
  KambiClassUtils;

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

  { Base 3D object, that can be managed by TSceneManager.
    All 3D objects should descend from this, this way we can easily
    insert them into TSceneManager. }
  TBase3D = class(TUIControl)
  private
    FCastsShadow: boolean;
    FExists: boolean;
    FCollides: boolean;
  public
    constructor Create(AOwner: TComponent); override;

    { @noAutoLinkHere }
    property Exists: boolean read FExists write FExists default true;

    { @noAutoLinkHere
      Note that if not @link(Exists) then this doesn't matter
      (not existing objects never participate in collision detection). }
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
    function BoundingBox: TBox3d; virtual; abstract;

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

      @param(TransparentGroups specifies for what TransparentGroup value
        it should prepare rendering resources. The idea is that
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

      @param(Options says what additional features (besides rendering)
        should be prepared to execute fast. See TPrepareRenderOption,
        the names should be self-explanatory (they refer to appropriate
        methods of TBase3D, TVRMLScene or TVRMLGLScene).) }
    procedure PrepareRender(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions); virtual;
  end;

  { List of base 3D objects (TBase3D instances).

    This inherits from TObjectsList, getting many
    features like TList notification mechanism (useful in some situations).
    Usually you want to use TBase3DList instead, which is a wrapper around
    this class. }
  TBase3DListCore = class(TKamObjectList)
  private
    function GetItem(const I: Integer): TBase3D;
    procedure SetItem(const I: Integer; const Item: TBase3D);
  public
    property Items[I: Integer]: TBase3D read GetItem write SetItem; default;
  end;

  { List of base 3D objects (TBase3D instances).

    This inherits from TBase3D class, so this list is itself a 3D object:
    it's a sum of all it's children 3D objects. }
  TBase3DList = class(TBase3D)
  private
    FList: TBase3DListCore;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BoundingBox: TBox3d; override;
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); override;
    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;
    procedure PrepareRender(
      TransparentGroups: TTransparentGroups;
      Options: TPrepareRenderOptions); override;
  published
    property List: TBase3DListCore read FList;
  end;

implementation

uses SysUtils;

constructor TBase3D.Create(AOwner: TComponent);
begin
  inherited;
  FCastsShadow := true;
  FExists := true;
  FCollides := true;
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

procedure TBase3D.PrepareRender(
  TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions);
begin
end;

{ TBase3DListCore ------------------------------------------------------------ }

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
  FList := TBase3DListCore.Create(false);
end;

destructor TBase3DList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
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

procedure TBase3DList.PrepareRender(
  TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions);
var
  I: Integer;
begin
  inherited;
  for I := 0 to List.Count - 1 do
    List[I].PrepareRender(TransparentGroups, Options);
end;

end.
