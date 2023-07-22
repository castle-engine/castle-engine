{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shape class for OpenGL rendering.
  @exclude Internal unit for CastleScene. }
unit CastleSceneInternalShape;

{$I castleconf.inc}

interface

uses Generics.Collections,
  X3DNodes, X3DFields, CastleImages, CastleVectors, CastleClassUtils,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleGLUtils, CastleInternalRenderer, CastleRenderOptions, CastleShapes;

type
  { Shape within a scene rendered using OpenGL.
    This is TShape extended with some information needed by TCastleScene.
    Non-internal units never expose instances of this class. }
  TGLShape = class(TX3DRendererShape)
  strict private
    TexturesPrepared: Boolean;
    { Request from parent TCastleScene to call our PrepareResources at nearest
      convenient time. }
    procedure SchedulePrepareResources;
    function PrepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;
    function UnprepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;
  public
    PassedFrustumAndDistanceCulling: Boolean;

    { Used only when TCastleViewport.OcclusionCulling.
      OcclusionQueryId is 0 if not initialized yet.
      When it's 0, value of OcclusionQueryAsked doesn't matter,
      OcclusionQueryAsked is always reset to @false when initializing
      OcclusionQueryId. }
    OcclusionQueryId: TGLint;
    OcclusionQueryAsked: boolean;

    { For Hierarchical Occlusion Culling. }
    RenderedFrameId: Cardinal;

    { Do not share the cache of this shape with other shapes.
      Offers tiny optimization when you know that this shape cannot be shared anyway.
      Never change it after initial render. }
    DisableSharedCache: Boolean;

    OverrideRenderOptions: TCastleRenderOptions;

    destructor Destroy; override;
    procedure Changed(const InactiveOnly: boolean;
      const Changes: TX3DChanges); override;
    procedure PrepareResources;
    procedure GLContextClose;

    function UseBlending: Boolean;
  end;

  { Shape with additional information how to render it inside a world,
    that allows to render it independently of the containing TCastleScene. }
  TCollectedShape = class
    Shape: TGLShape;
    RenderOptions: TCastleRenderOptions;
    SceneTransform: TMatrix4;
    DepthRange: TDepthRange;
  end;

  TCollectedShapeList = class({$ifdef FPC}specialize{$endif} TObjectList<TCollectedShape>)
  strict private
    Camera: TViewVectors;

    function CompareBackToFront2D(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} ShapeA, ShapeB: TCollectedShape): Integer;
    function CompareBackToFront3DBox(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} ShapeA, ShapeB: TCollectedShape): Integer;
    function CompareBackToFront3DOrigin(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareBackToFront3DGround(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareBackToFrontCustom(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;

    function CompareFrontToBack2D(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBack3DBox(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBack3DOrigin(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBack3DGround(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBackCustom(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
  public
    { Used when ShapeSort is sortCustom for SortBackToFront or SortFrontToBack. }
    OnCustomShapeSort: TShapeSortEvent;

    { Sort shapes by distance to camera, farthest first.
      ShapeSort determines the sorting algorithm.
      See @link(TShapeSort) documentation. }
    procedure SortBackToFront(
      const ACamera: TViewVectors;
      const ShapeSort: TShapeSortNoAuto);

    { Sort shapes by distance to camera, closest first.
      ShapeSort determines the sorting algorithm.
      See @link(TShapeSort) documentation. }
    procedure SortFrontToBack(
      const ACamera: TViewVectors;
      const ShapeSort: TShapeSortNoAuto);
  end;

implementation

uses Generics.Defaults, Math, SysUtils,
  CastleScene, CastleBoxes;

{ TGLShape --------------------------------------------------------------- }

destructor TGLShape.Destroy;
begin
  inherited;
end;

procedure TGLShape.Changed(const InactiveOnly: boolean;
  const Changes: TX3DChanges);
begin
  inherited;

  if Cache <> nil then
  begin
    { Ignore changes that don't affect prepared arrays,
      like transformation, clip planes and everything else that is applied
      by renderer every time, and doesn't affect TGeometryArrays. }

    if Changes * [
         chCoordinate,
         chNormal,
         chTangent
       ] <> [] then
    begin
      Cache.InvalidateVertexData([vtCoordinate]);

      { Note: When bump mapping is used, upon changing normals -> we need to recalculate tangents.
        But that is already covered: tangents are also part of vtCoordinate. }
    end;

    { Note that Changes may contain both chCoordinate and chTextureCoordinate
      (e.g. in case of batching)
      in which case both "if" clauses should be entered.

      About chTextureImage:
      We regenerate arrays when chTextureImage occurred,
      because it means that potentially non-existing texture (e.g. ImageTexture
      with empty url, or invalid url) changed to existing (if you set correct
      ImageTexture.url). This means that number of texture coordinates
      we need to make has changed.
      Testcase "animate_symbols", using Unholy spell effect animations.

      TODO: Actually Cache.InvalidateVertexData is often not necessary in case of chTextureImage.
      It's only necessary when texture existence changed.
      This could be optimized more.
    }
    if Changes * [
         chTextureImage,
         chVisibleVRML1State,
         chGeometryVRML1State,
         chColorNode,
         chTextureCoordinate,
         chGeometry,
         chGeometryFontChanged,
         chFontStyle,
         chFontStyleFontChanged,
         chWireframe
       ] <> [] then
      Cache.InvalidateVertexData(AllVboTypes);
  end;

  if Changes * [
       chTextureImage,
       chTextureRendererProperties,
       { Needed to make TCastleText.CustomFont change applied OK,
         otherwise TCastleText could be rendered without blending. }
       chFontStyleFontChanged
     ] <> [] then
  begin
    TTextureResources.Unprepare(State.MainTexture);
    { Make next PrepareResources prepare all textures.
      Testcase:
      - view3dscene
      - open demo-models/rendered_texture/rendered_texture_tweak_size.x3dv
      - use s / S
      - without this fix, texture would change to none, and never again
        update RenderedTexture contents. }
    TexturesPrepared := false;
    { Make sure PrepareResources is actually called soon. }
    SchedulePrepareResources;
  end;
end;

function TGLShape.UnprepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;
begin
  Result := nil; // let EnumerateTextures to enumerate all textures
  TTextureResources.Unprepare(Texture);
end;

function TGLShape.PrepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;
var
  RenderOptions: TCastleRenderOptions;
begin
  Result := nil; // let EnumerateTextures to enumerate all textures

  // This method only captures textures on this shape
  Assert(Shape = Self);

  // TODO: add and always depend on local RenderOptions field, not ParentScene? or we need ParentScene anyway?
  if OverrideRenderOptions <> nil then
    RenderOptions := OverrideRenderOptions
  else
    RenderOptions := TCastleScene(ParentScene).RenderOptions;
  Assert(RenderOptions <> nil);

  TTextureResources.Prepare(RenderOptions, Texture);
end;

procedure TGLShape.PrepareResources;
begin
  if not TexturesPrepared then
  begin
    TexturesPrepared := true;
    EnumerateTextures({$ifdef FPC}@{$endif} PrepareTexture);
  end;
end;

procedure TGLShape.GLContextClose;

  { Free Arrays and Vbo of all shapes. }
  procedure FreeCaches;
  var
    Pass: TTotalRenderingPass;
  begin
    if Cache <> nil then
    begin
      RendererCache.Shape_DecReference(Self, Cache);
      Assert(Cache = nil, 'At the end of GLContextClose, Cache should be nil');
    end;

    for Pass := Low(Pass) to High(Pass) do
    begin
      if ProgramCache[Pass] <> nil then
        RendererCache.Program_DecReference(ProgramCache[Pass]);
      Assert(ProgramCache[Pass] = nil, 'At the end of GLContextClose, all ProgramCache[Pass] should be nil');
    end;
  end;

begin
  if TexturesPrepared then
  begin
    TexturesPrepared := false;
    EnumerateTextures({$ifdef FPC}@{$endif} UnprepareTexture);
  end;

  FreeCaches;

  if OcclusionQueryId <> 0 then
  begin
    glDeleteQueries(1, @OcclusionQueryId);
    OcclusionQueryId := 0;
  end;
end;

function TGLShape.UseBlending: Boolean;
begin
  Result := AlphaChannel = acBlending;
end;

procedure TGLShape.SchedulePrepareResources;
begin
  if ParentScene <> nil then
    TCastleScene(ParentScene).InternalSchedulePrepareResources;
end;

{ TCollectedShape ---------------------------------------------------------- }

type
  TCollectedShapeComparer = {$ifdef FPC}specialize{$endif} TComparer<TCollectedShape>;

function TCollectedShapeList.CompareBackToFront2D(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} ShapeA, ShapeB: TCollectedShape): Integer;
var
  A, B: TBox3D;

  { Representative point of ShapeA in world coordinates.
    Call only if ShapeA bbox is not empty. }
  function APoint: TVector3;
  begin
    Result := ShapeA.SceneTransform.MultPoint(A.Data[0]);
  end;

  { Representative point of ShapeA in world coordinates.
    Call only if ShapeB bbox is not empty. }
  function BPoint: TVector3;
  begin
    Result := ShapeB.SceneTransform.MultPoint(B.Data[0]);
  end;

begin
  A := ShapeA.Shape.BoundingBox;
  B := ShapeB.Shape.BoundingBox;

  { Note that we ignore camera position/direction for 2D comparison.
    We merely look at Z coordinates.
    This way looking at 2D Spine scene from the other side is also Ok.

    For speed, we don't look at bounding box .Center, only at .Data[0].
    The assumption here is that shape is thin 2D, and it doesn't really
    matter if we look at min,max or center point if we only compare Z.

      BoundingBox.Data[0].Z ~=
      BoundingBox.Data[1].Z ~=
      BoundingBox.Center.Z .
  }

  if (not A.IsEmpty) and
    ( B.IsEmpty or
      ( APoint.Z < BPoint.Z )) then
    Result := -1
  else
  if (not B.IsEmpty) and
    ( A.IsEmpty or
      ( BPoint.Z < APoint.Z )) then
    Result :=  1
  else
    Result :=  0;
end;

function TCollectedShapeList.CompareBackToFront3DBox(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} ShapeA, ShapeB: TCollectedShape): Integer;
var
  A, B: TBox3D;

  { Center of ShapeA in world coordinates.
    Call only if ShapeA bbox is not empty. }
  function ACenter: TVector3;
  begin
    Result := ShapeA.SceneTransform.MultPoint(A.Center);
  end;

  { Center of ShapeB in world coordinates.
    Call only if ShapeB bbox is not empty. }
  function BCenter: TVector3;
  begin
    Result := ShapeB.SceneTransform.MultPoint(B.Center);
  end;

begin
  { We always treat empty box as closer than non-empty.
    And two empty boxes are always equal.

    Remember that code below must make sure that Result = 0
    for equal elements (Sort may depend on this). So A > B only when:
    - A empty, and B non-empty
    - both non-empty, and A closer

    We compare distances using
      TVector3.DotProduct(..., Camera.Direction)
    instead of
      PointsDistance(...., Camera.Translation)

    This way algorithm works reliably also for completely flat models
    (like Spine animations or images) aligned to camera as billboards,
    in 3D, with any camera direction.
  }

  A := ShapeA.Shape.BoundingBox;
  B := ShapeB.Shape.BoundingBox;

  if (not A.IsEmpty) and
    ( B.IsEmpty or
      ( TVector3.DotProduct(ACenter, Camera.Direction) >
        TVector3.DotProduct(BCenter, Camera.Direction))) then
    Result := -1
  else
  if (not B.IsEmpty) and
    ( A.IsEmpty or
      ( TVector3.DotProduct(BCenter, Camera.Direction) >
        TVector3.DotProduct(ACenter, Camera.Direction))) then
    Result :=  1
  else
    Result :=  0;
end;

function TCollectedShapeList.CompareBackToFront3DOrigin(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
var
  PointA, PointB: TVector3;
begin
  PointA := (A.SceneTransform * A.Shape.OriginalState.Transformation.Transform).MultPoint(TVector3.Zero);
  PointB := (B.SceneTransform * B.Shape.OriginalState.Transformation.Transform).MultPoint(TVector3.Zero);
  Result := Sign(
    TVector3.DotProduct(PointB, Camera.Direction) -
    TVector3.DotProduct(PointA, Camera.Direction));
end;

function TCollectedShapeList.CompareBackToFront3DGround(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
var
  PointA, PointB: TVector3;
begin
  { TODO: what's more efficient - MultTransform 2x, or multiply matrix + MultTransform? }
  PointA := (A.SceneTransform * A.Shape.OriginalState.Transformation.Transform).MultPoint(TVector3.Zero);
  PointB := (B.SceneTransform * B.Shape.OriginalState.Transformation.Transform).MultPoint(TVector3.Zero);
  PointA.Y := 0;
  PointB.Y := 0;
  Result := Sign(
    TVector3.DotProduct(PointB, Camera.Direction) -
    TVector3.DotProduct(PointA, Camera.Direction));
end;

function TCollectedShapeList.CompareBackToFrontCustom(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := OnCustomShapeSort(Camera,
    A.Shape, B.Shape,
    A.RenderOptions, B.RenderOptions,
    A.SceneTransform, B.SceneTransform
  );
end;

procedure TCollectedShapeList.SortBackToFront(
  const ACamera: TViewVectors;
  const ShapeSort: TShapeSortNoAuto);
begin
  Camera := ACamera;

  { sort3DVerticalBillboards is just like sort3D,
    but project camera direction on Y=0 plane. }
  if ShapeSort = sort3DVerticalBillboards then
    Camera.Direction.Y := 0;

  case ShapeSort of
    sort2D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront2D));
    sort3DVerticalBillboards,
    sort3D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront3DBox));
    sort3DOrigin: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront3DOrigin));
    sort3DGround: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront3DGround));
    sortCustom  :
      begin
        // does not sort if OnCustomShapeSort unassigned
        if Assigned(OnCustomShapeSort) then
          Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFrontCustom));
      end;
    else ;
  end;
end;

function TCollectedShapeList.CompareFrontToBack2D(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := -CompareBackToFront2D(A, B);
end;

function TCollectedShapeList.CompareFrontToBack3DBox(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := -CompareBackToFront3DBox(A, B);
end;

function TCollectedShapeList.CompareFrontToBack3DOrigin(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := -CompareBackToFront3DOrigin(A, B);
end;

function TCollectedShapeList.CompareFrontToBack3DGround(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := -CompareBackToFront3DGround(A, B);
end;

function TCollectedShapeList.CompareFrontToBackCustom(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := -CompareBackToFrontCustom(A, B);
end;

procedure TCollectedShapeList.SortFrontToBack(
  const ACamera: TViewVectors;
  const ShapeSort: TShapeSortNoAuto);
begin
  Camera := ACamera;

  { sort3DVerticalBillboards is just like sort3D,
    but project camera direction on Y=0 plane. }
  if ShapeSort = sort3DVerticalBillboards then
    Camera.Direction.Y := 0;

  case ShapeSort of
    sort2D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack2D));
    sort3DVerticalBillboards,
    sort3D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack3DBox));
    sort3DOrigin: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack3DOrigin));
    sort3DGround: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack3DGround));
    sortCustom  :
      begin
        // does not sort if OnCustomShapeSort unassigned
        if Assigned(OnCustomShapeSort) then
          Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBackCustom));
      end;
    else ;
  end;
end;

end.
