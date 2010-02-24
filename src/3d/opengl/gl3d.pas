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

{ Base 3D objects depending on OpenGL.
  For now, T3DCustomTranslated (that uses OpenGL matrix push/translate/pop
  during rendering). }
unit GL3D;

interface

uses Classes, VectorMath, Boxes3D, KeysMouse, Frustum, Base3D;

type
  { Translates other T3D object.

    Since "other T3D object" may be any T3D descendant (including
    T3DList, TVRMLGLScene, TVRMLGLAnimation), you can use this
    to translate a group of any 3D objects.

    It has a GetTranslation function (that must be actually defined
    in a descendant) that always says how this object is translated from
    it's original position. All methods (that render, query collisions etc.)
    take into account this GetTranslation. }
  T3DCustomTranslated = class(T3D)
  private
    FChild: T3D;
    procedure ChildVisibleChange(Sender: TObject);
    procedure ChildCursorChange(Sender: TObject);
    procedure SetChild(const Value: T3D);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function GetTranslation: TVector3Single; virtual; abstract;

    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup; InShadow: boolean); override;
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
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
    function MouseMove(const RayOrigin, RayDirection: TVector3Single;
      RayHit: T3DCollision): boolean; override;
    procedure Idle(const CompSpeed: Single); override;
    procedure GLContextClose; override;
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
  published
    { Translated 3D object. }
    property Child: T3D read FChild write SetChild;
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
  if Exists and Collides and (Child <> nil) then
    Result := Box3DTranslate(Child.BoundingBox, GetTranslation) else
    Result := EmptyBox3D;
end;

procedure T3DCustomTranslated.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup; InShadow: boolean);
var
  T: TVector3Single;
begin
  inherited;
  if Exists and (Child <> nil) then
  begin
    T := GetTranslation;

    { We assume that Translation = 0,0,0 is the most common case
      (this is true e.g. for TDoomLevelDoor,
      since all doors close automatically, and initially all are closed...).

      In this case we can avoid Frustum.Move (although I didn't do any tests,
      maybe this check is not worth the effort and we don't need to worry
      about Frustum.Move time so much ?). }

    if ZeroVector(T) then
      Child.Render(Frustum, TransparentGroup, InShadow) else
      begin
        glPushMatrix;
          glTranslatev(T);
          { Child.Render expects Frustum in it's local coordinates,
            that's why we subtract Translation here. }
          Child.Render(Frustum.Move(VectorNegate(T)), TransparentGroup, InShadow);
        glPopMatrix;
      end;
  end;
end;

procedure T3DCustomTranslated.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  T: TVector3Single;
begin
  inherited;
  if Exists and CastsShadow and (Child <> nil) then
  begin
    T := GetTranslation;

    { We assume that Translation = 0,0,0 is the most common case
      (this is true e.g. for TDoomLevelDoor,
      since all doors close automatically, and initially all are closed...).

      In this case we can avoid matrix multiplication. }

    if ZeroVector(T) then
      Child.RenderShadowVolume(ShadowVolumeRenderer,
        ParentTransformIsIdentity, ParentTransform) else
      Child.RenderShadowVolume(ShadowVolumeRenderer,
        false, MatrixMult(TranslationMatrix(T), ParentTransform));
  end;
end;

procedure T3DCustomTranslated.PrepareRender(TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions; ProgressStep: boolean);
begin
  inherited;
  if Child <> nil then
    Child.PrepareRender(TransparentGroups, Options, ProgressStep);
end;

function T3DCustomTranslated.PrepareRenderSteps: Cardinal;
begin
  Result := inherited;
  if Child <> nil then
    Result += Child.PrepareRenderSteps;
end;

function T3DCustomTranslated.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.KeyDown(Key, C);
end;

function T3DCustomTranslated.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.KeyUp(Key, C);
end;

function T3DCustomTranslated.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.MouseDown(Button);
end;

function T3DCustomTranslated.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.MouseUp(Button);
end;

function T3DCustomTranslated.MouseMove(const RayOrigin, RayDirection: TVector3Single;
  RayHit: T3DCollision): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.MouseMove(VectorSubtract(RayOrigin, GetTranslation),
      RayDirection, RayHit);
end;

procedure T3DCustomTranslated.Idle(const CompSpeed: Single);
begin
  inherited;
  if Child <> nil then
    Child.Idle(CompSpeed);
end;

procedure T3DCustomTranslated.GLContextClose;
begin
  if Child <> nil then
    Child.GLContextClose;
  inherited;
end;

procedure T3DCustomTranslated.GetHeightAbove(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out IsAbove: boolean; out AboveHeight: Single;
  out AboveGround: P3DTriangle);
var
  T: TVector3Single;
begin
  inherited;
  if Exists and Collides and (Child <> nil) then
  begin
    T := GetTranslation;

    Child.GetHeightAbove(
      VectorSubtract(Position, T), GravityUp, TrianglesToIgnoreFunc,
      IsAbove, AboveHeight, AboveGround);
  end;
end;

function T3DCustomTranslated.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := (not Exists) or (not Collides) or (Child = nil);
  if Result then
    NewPos := ProposedNewPos else
  begin
    T := GetTranslation;
    Result := Child.MoveAllowed(
      VectorSubtract(OldPos, T),
      VectorSubtract(ProposedNewPos, T), NewPos,
      CameraRadius, TrianglesToIgnoreFunc);
    { translate calculated NewPos back }
    if Result then
      VectorAddTo1st(NewPos, T);
  end;
end;

function T3DCustomTranslated.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := (not Exists) or (not Collides) or (Child = nil);
  if not Result then
  begin
    { I have to check collision between
        Child + Translation and (OldPos, ProposedNewPos).
      So it's equivalent to checking for collision between
        Child and (OldPos, ProposedNewPos) - Translation
      And this way I can use Child.MoveAllowedSimple. }

    T := GetTranslation;
    Result := Child.MoveAllowedSimple(
      VectorSubtract(OldPos, T),
      VectorSubtract(ProposedNewPos, T),
      CameraRadius, TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTranslated.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
  B: TBox3D;
begin
  Result := (not Exists) or (not Collides) or (Child = nil);
  if not Result then
  begin
    { I have to check collision between
        Child + Translation and (OldPos, ProposedNewPos).
      So it's equivalent to checking for collision between
        Child and (OldPos, ProposedNewPos) - Translation
      And this way I can use Child.MoveBoxAllowedSimple. }

    T := GetTranslation;
    B[0] := VectorSubtract(ProposedNewBox[0], T);
    B[1] := VectorSubtract(ProposedNewBox[1], T);
    Result := Child.MoveBoxAllowedSimple(
      VectorSubtract(OldPos, T),
      VectorSubtract(ProposedNewPos, T),
      B, TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTranslated.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := Exists and Collides and (Child <> nil);
  if Result then
  begin
    { We use the same trick as in T3DCustomTranslated.MoveAllowedSimple to
      use Child.SegmentCollsion with Translation. }

    T := GetTranslation;
    Result := Child.SegmentCollision(
      VectorSubtract(Pos1, T),
      VectorSubtract(Pos2, T), TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTranslated.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := Exists and Collides and (Child <> nil);
  if Result then
  begin
    { We use the same trick as in T3DCustomTranslated.MoveAllowedSimple to
      use Child.SphereCollsion with Translation. }

    T := GetTranslation;
    Result := Child.SphereCollision(
      VectorSubtract(Pos, T),  Radius, TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTranslated.BoxCollision(
  const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := Exists and Collides and (Child <> nil);
  if Result then
  begin
    { We use the same trick as in T3DCustomTranslated.MoveAllowedSimple to
      use Child.BoxCollsion with Translation. }

    T := GetTranslation;
    Result := Child.BoxCollision(
      Box3DAntiTranslate(Box, T),  TrianglesToIgnoreFunc);
  end;
end;

function T3DCustomTranslated.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): T3DCollision;
var
  T: TVector3Single;
begin
  Result := nil;
  if Exists and Collides and (Child <> nil) then
  begin
    { We use the same trick as in T3DCustomTranslated.MoveAllowedSimple to
      use Child.RayCollsion with Translation. }

    T := GetTranslation;
    Result := Child.RayCollision(IntersectionDistance,
      VectorSubtract(Ray0, T),
      RayVector, TrianglesToIgnoreFunc);

    if Result <> nil then
      Result.Hierarchy.Insert(0, Self);
  end;
end;

procedure T3DCustomTranslated.SetChild(const Value: T3D);
begin
  if FChild <> Value then
  begin
    if FChild <> nil then
    begin
      if FChild.OnVisibleChange = @ChildVisibleChange then
	FChild.OnVisibleChange := nil;
      if FChild.OnCursorChange = @ChildCursorChange then
	FChild.OnCursorChange := nil;
      FChild.RemoveFreeNotification(Self);
    end;

    FChild := Value;

    if FChild <> nil then
    begin
      if FChild.OnVisibleChange = nil then
	FChild.OnVisibleChange := @ChildVisibleChange;
      if FChild.OnCursorChange = nil then
	FChild.OnCursorChange := @ChildCursorChange;
      FChild.FreeNotification(Self);
    end;

    ChildCursorChange(nil);
  end;
end;

procedure T3DCustomTranslated.ChildVisibleChange(Sender: TObject);
begin
  VisibleChange;
end;

procedure T3DCustomTranslated.ChildCursorChange(Sender: TObject);
begin
  CursorChange;
end;

procedure T3DCustomTranslated.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FChild) then
    FChild := nil;
end;

{ T3DTranslated -------------------------------------------------------------- }

function T3DTranslated.GetTranslation: TVector3Single;
begin
  Result := FTranslation;
end;

end.
