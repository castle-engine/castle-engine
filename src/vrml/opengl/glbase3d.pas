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
  For now, TCustomTranslated3D (that uses OpenGL matrix push/translate/pop
  during rendering). }
unit GLBase3D;

interface

uses Classes, VectorMath, Boxes3D, KeysMouse, Frustum, Base3D, VRMLTriangle;

type
  { Translates other TBase3D object.

    Since "other TBase3D object" may be any TBase3D descendant (including
    TBase3DList, TVRMLGLScene, TVRMLGLAnimation), you can use this
    to translate a group of any 3D objects.

    It has a GetTranslation function (that must be actually defined
    in a descendant) that always says how this object is translated from
    it's original position. All methods (that render, query collisions etc.)
    take into account this GetTranslation. }
  TCustomTranslated3D = class(TBase3D)
  private
    FChild: TBase3D;
    procedure ChildVisibleChange(Sender: TObject);
    procedure ChildCursorChange(Sender: TObject);
    procedure SetChild(const Value: TBase3D);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function GetTranslation: TVector3Single; virtual; abstract;

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
  published
    { Translated 3D object. }
    property Child: TBase3D read FChild write SetChild;
  end;

  { Translates other TBase3D object.
    This simplifies the TCustomTranslated3D, giving you simple
    @link(Translation) property to move your child 3D object. }
  TTranslated3D = class(TCustomTranslated3D)
  private
    FTranslation: TVector3Single;
  public
    function GetTranslation: TVector3Single; override;
    property Translation: TVector3Single read GetTranslation write FTranslation;
  end;

implementation

uses GL, KambiGLUtils;

{ TCustomTranslated3D -------------------------------------------------------- }

function TCustomTranslated3D.BoundingBox: TBox3D;
begin
  if Exists and Collides and (Child <> nil) then
    Result := Box3DTranslate(Child.BoundingBox, GetTranslation) else
    Result := EmptyBox3D;
end;

procedure TCustomTranslated3D.Render(const Frustum: TFrustum;
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

procedure TCustomTranslated3D.RenderShadowVolume(
  ShadowVolumes: TBaseShadowVolumes;
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
      Child.RenderShadowVolume(ShadowVolumes,
        ParentTransformIsIdentity, ParentTransform) else
      Child.RenderShadowVolume(ShadowVolumes,
        false, MatrixMult(TranslationMatrix(T), ParentTransform));
  end;
end;

procedure TCustomTranslated3D.PrepareRender(TransparentGroups: TTransparentGroups;
  Options: TPrepareRenderOptions; ProgressStep: boolean);
begin
  inherited;
  if Child <> nil then
    Child.PrepareRender(TransparentGroups, Options, ProgressStep);
end;

function TCustomTranslated3D.PrepareRenderSteps: Cardinal;
begin
  Result := inherited;
  if Child <> nil then
    Result += Child.PrepareRenderSteps;
end;

function TCustomTranslated3D.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.KeyDown(Key, C);
end;

function TCustomTranslated3D.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.KeyUp(Key, C);
end;

function TCustomTranslated3D.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.MouseDown(Button);
end;

function TCustomTranslated3D.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.MouseUp(Button);
end;

function TCustomTranslated3D.MouseMove(const RayOrigin, RayDirection: TVector3Single): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Child <> nil then
    Result := Child.MouseMove(VectorSubtract(RayOrigin, GetTranslation), RayDirection);
end;

procedure TCustomTranslated3D.Idle(const CompSpeed: Single);
begin
  inherited;
  if Child <> nil then
    Child.Idle(CompSpeed);
end;

procedure TCustomTranslated3D.GLContextClose;
begin
  if Child <> nil then
    Child.GLContextClose;
  inherited;
end;

procedure TCustomTranslated3D.GetCameraHeight(const Position, GravityUp: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single;
  out GroundItem: PVRMLTriangle);
var
  T: TVector3Single;
begin
  inherited;
  if Exists and Collides and (Child <> nil) then
  begin
    T := GetTranslation;

    Child.GetCameraHeight(
      VectorSubtract(Position, T), GravityUp, TrianglesToIgnoreFunc,
      IsAboveTheGround, SqrHeightAboveTheGround, GroundItem);
  end;
end;

function TCustomTranslated3D.MoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
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

function TCustomTranslated3D.MoveAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const CameraRadius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
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

function TCustomTranslated3D.MoveBoxAllowedSimple(
  const OldPos, ProposedNewPos: TVector3Single;
  const ProposedNewBox: TBox3D;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
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

function TCustomTranslated3D.SegmentCollision(const Pos1, Pos2: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := Exists and Collides and (Child <> nil);
  if Result then
  begin
    { We use the same trick as in TCustomTranslated3D.MoveAllowedSimple to
      use Child.SegmentCollsion with Translation. }

    T := GetTranslation;
    Result := Child.SegmentCollision(
      VectorSubtract(Pos1, T),
      VectorSubtract(Pos2, T), TrianglesToIgnoreFunc);
  end;
end;

function TCustomTranslated3D.SphereCollision(
  const Pos: TVector3Single; const Radius: Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := Exists and Collides and (Child <> nil);
  if Result then
  begin
    { We use the same trick as in TCustomTranslated3D.MoveAllowedSimple to
      use Child.SphereCollsion with Translation. }

    T := GetTranslation;
    Result := Child.SphereCollision(
      VectorSubtract(Pos, T),  Radius, TrianglesToIgnoreFunc);
  end;
end;

function TCustomTranslated3D.BoxCollision(
  const Box: TBox3D;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): boolean;
var
  T: TVector3Single;
begin
  Result := Exists and Collides and (Child <> nil);
  if Result then
  begin
    { We use the same trick as in TCustomTranslated3D.MoveAllowedSimple to
      use Child.BoxCollsion with Translation. }

    T := GetTranslation;
    Result := Child.BoxCollision(
      Box3DAntiTranslate(Box, T),  TrianglesToIgnoreFunc);
  end;
end;

(*
function TCustomTranslated3D.RayCollision(
  out IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single;
  const TrianglesToIgnoreFunc: TVRMLTriangleIgnoreFunc): TCollisionInfo;
var
  T: TVector3Single;
begin
  Result := nil;
  if Exists and Collides and (Child <> nil) then
  begin
    { We use the same trick as in TCustomTranslated3D.MoveAllowedSimple to
      use Child.RayCollsion with Translation. }

    T := GetTranslation;
    Result := Child.RayCollision(IntersectionDistance,
      VectorSubtract(Ray0, T),
      RayVector, TrianglesToIgnoreFunc);

    if Result <> nil then
      Result.Hierarchy.Insert(0, Self);
  end;
end;
*)

procedure TCustomTranslated3D.SetChild(const Value: TBase3D);
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

procedure TCustomTranslated3D.ChildVisibleChange(Sender: TObject);
begin
  VisibleChange;
end;

procedure TCustomTranslated3D.ChildCursorChange(Sender: TObject);
begin
  CursorChange;
end;

procedure TCustomTranslated3D.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FChild) then
    FChild := nil;
end;

{ TTranslated3D -------------------------------------------------------------- }

function TTranslated3D.GetTranslation: TVector3Single;
begin
  Result := FTranslation;
end;

end.
