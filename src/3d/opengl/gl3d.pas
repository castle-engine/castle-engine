{
  Copyright 2010-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base 3D objects depending on OpenGL.
  For now, T3DCustomTranslated (that uses OpenGL matrix push/translate/pop
  during rendering). }
unit GL3D;

interface

uses Classes, VectorMath, Boxes3D, KeysMouse, Frustum, Base3D;

type
  { Translates other T3D objects.

    Descends from T3DList, translating all it's children.
    So it can be used to translate any T3D descendants (including
    another T3DList, TVRMLGLScene, TVRMLGLAnimation).

    Actual translation is defined by an abstract method GetTranslation.
    You have to create descendant of this class, and override GetTranslation.
    Alternatively, you can use T3DTranslated that overrides GetTranslation
    for you, and gives you a simple
    T3DTranslated.Translation property that you can use. }
  T3DCustomTranslated = class(T3DList)
  public
    function GetTranslation: TVector3Single; virtual; abstract;

    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;
    function MouseMove(const RayOrigin, RayDirection: TVector3Single;
      RayHit: T3DCollision): boolean; override;
    procedure GetHeightAbove(const Position, GravityUp: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
      out IsAbove: boolean; out AboveHeight: Single;
      out AboveGround: P3DTriangle); override;
    function MoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const CameraRadius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function MoveBoxAllowedSimple(
      const OldPos, ProposedNewPos: TVector3Single;
      const ProposedNewBox: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SegmentCollision(const Pos1, Pos2: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function SphereCollision(const Pos: TVector3Single; const Radius: Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function BoxCollision(const Box: TBox3D;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean; override;
    function RayCollision(
      out IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single;
      const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): T3DCollision; override;
  end;

  { Translates other T3D object.
    This simplifies the T3DCustomTranslated, giving you simple
    @link(Translation) property to move your child 3D object. }
  T3DTranslated = class(T3DCustomTranslated)
  private
    FTranslation: TVector3Single;
  public
    function GetTranslation: TVector3Single; override;
    property Translation: TVector3Single read GetTranslation write FTranslation;
  end;

implementation

uses GL, KambiGLUtils;

{ T3DCustomTranslated -------------------------------------------------------- }

function T3DCustomTranslated.BoundingBox: TBox3D;
begin
  Result := (inherited BoundingBox).Translate(GetTranslation);
end;

procedure T3DCustomTranslated.Render(const Frustum: TFrustum; const Params: TRenderParams);
var
  T: TVector3Single;
begin
  T := GetTranslation;

  { We assume that Translation = 0,0,0 is the most common case
    (this is true e.g. for TDoomLevelDoor,
    since all doors close automatically, and initially all are closed...).

    In this case we can avoid Frustum.Move (although I didn't do any tests,
    maybe this check is not worth the effort and we don't need to worry
    about Frustum.Move time so much ?). }

  if ZeroVector(T) then
    inherited Render(Frustum, Params) else
    begin
      glPushMatrix;
        glTranslatev(T);
        { Child.Render expects Frustum in it's local coordinates,
          that's why we subtract Translation here. }
        inherited Render(Frustum.Move(-T), Params);
      glPopMatrix;
    end;
end;

procedure T3DCustomTranslated.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  T: TVector3Single;
begin
  T := GetTranslation;

  { We assume that Translation = 0,0,0 is the most common case
    (this is true e.g. for TDoomLevelDoor,
    since all doors close automatically, and initially all are closed...).

    In this case we can avoid matrix multiplication. }

  if ZeroVector(T) then
    inherited RenderShadowVolume(ShadowVolumeRenderer,
      ParentTransformIsIdentity, ParentTransform) else
    inherited RenderShadowVolume(ShadowVolumeRenderer,
      false, MatrixMult(TranslationMatrix(T), ParentTransform));
end;

function T3DCustomTranslated.MouseMove(const RayOrigin, RayDirection: TVector3Single;
  RayHit: T3DCollision): boolean;
begin
  Result := inherited MouseMove(RayOrigin - GetTranslation,
    RayDirection, RayHit);
end;

procedure T3DCustomTranslated.GetHeightAbove(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: P3DTriangle);
begin
  inherited GetHeightAbove(
    Position - GetTranslation, GravityUp, TrianglesToIgnoreFunc,
    IsAbove, AboveHeight, AboveGround);
end;

function T3DCustomTranslated.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  T := GetTranslation;
  Result := inherited MoveAllowed(OldPos - T, ProposedNewPos - T, NewPos,
    CameraRadius, TrianglesToIgnoreFunc);
  { translate calculated NewPos back }
  if Result then
    NewPos += T;
end;

function T3DCustomTranslated.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  { I have to check collision between
      Items + Translation and (OldPos, ProposedNewPos).
    So it's equivalent to checking for collision between
      Items and (OldPos, ProposedNewPos) - Translation
    And this way I can use Child.MoveAllowedSimple. }

  T := GetTranslation;
  Result := inherited MoveAllowedSimple(
    OldPos - T, ProposedNewPos - T, CameraRadius, TrianglesToIgnoreFunc);
end;

function T3DCustomTranslated.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
  B: TBox3D;
begin
  { I have to check collision between
      Items + Translation and (OldPos, ProposedNewPos).
    So it's equivalent to checking for collision between
      Items and (OldPos, ProposedNewPos) - Translation
    And this way I can use "inherited MoveBoxAllowedSimple". }

  T := GetTranslation;
  B.Data[0] := ProposedNewBox.Data[0] - T;
  B.Data[1] := ProposedNewBox.Data[1] - T;
  Result := inherited MoveBoxAllowedSimple(
    OldPos - T, ProposedNewPos - T, B, TrianglesToIgnoreFunc);
end;

function T3DCustomTranslated.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  T := GetTranslation;
  Result := inherited SegmentCollision(Pos1 - T, Pos2 - T, TrianglesToIgnoreFunc);
end;

function T3DCustomTranslated.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := inherited SphereCollision(
    Pos - GetTranslation, Radius, TrianglesToIgnoreFunc);
end;

function T3DCustomTranslated.BoxCollision(
  const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := Exists and Collides;

  { We could just call inherited. But, for optimization, check
    Exists and Collides first, to avoid calling Box3DAntiTranslate
    when not necessary. }

  if Result then
  begin
    { We use the same trick as in T3DCustomTranslated.MoveAllowedSimple to
      use "inherited BoxCollsion" with Translation. }

    Result := inherited BoxCollision(
      Box.AntiTranslate(GetTranslation), TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTranslated.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): T3DCollision;
begin
  Result := inherited RayCollision(IntersectionDistance,
    Ray0 - GetTranslation,
    RayVector, TrianglesToIgnoreFunc);
end;

{ T3DTranslated -------------------------------------------------------------- }

function T3DTranslated.GetTranslation: TVector3Single;
begin
  Result := FTranslation;
end;

end.
