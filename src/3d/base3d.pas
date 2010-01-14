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

uses Classes, VectorMath, Frustum, Boxes3D, UIControls;

type
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

    { Render given object.

      It can be optimized to not
      render the object if it's not inside the Frustum.

      TransparentGroup may indicate that only opaque or only transparent
      parts should be rendered, just like for TVRMLGLScene.Render comments.

      This is done only if @link(Exists). }
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup); virtual;

    property CastsShadow: boolean read FCastsShadow write FCastsShadow
      default true;

    { Render shadow quads for all the things rendered by @link(Render).
      Does nothing if not CastsShadow.
      It does shadow volumes culling inside  (so ShadowVolumes should
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
      volume capping).

      This is done only if @link(Exists) and @link(CastsShadow). }
    procedure RenderShadowVolume(
      ShadowVolumes: TBaseShadowVolumes;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); virtual;
  end;

implementation

constructor TBase3D.Create(AOwner: TComponent);
begin
  inherited;
  FCastsShadow := true;
  FExists := true;
  FCollides := true;
end;

procedure TBase3D.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup);
begin
end;

procedure TBase3D.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
begin
end;

end.
