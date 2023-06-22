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
  CastleGLUtils, CastleInternalRenderer, CastleRenderOptions;

type
  { Shape within a scene rendered using OpenGL.
    This is TShape extended with some information needed by TCastleScene.
    Non-internal units never expose instances of this class. }
  TGLShape = class(TX3DRendererShape)
  strict private
    { TODO: having reference from shape to renderer is not optimal.
      Shapes needs to observe Renderer - the TRenderer "free notification
      list" is likely long.
      Ultimately shapes should not need renderer to prepare / unprepare.
      Renderer instance should only be used for rendering,
      rest (prepare) should be stored in cache independent of renderer. }
    FRenderer: TRenderer;
    FRendererObserver: TFreeNotificationObserver;

    procedure RendererFreeNotification(const Sender: TFreeNotificationObserver);

    { Unassociate with Renderer and set it to nil, if non-nil. }
    procedure RendererDetach;

    { Associate with ARenderer and assign it as Renderer.
      Doesn't take care of deinitializing previous Renderer value,
      or even checking is it different than ARenderer. }
    procedure RendererAttach(const ARenderer: TRenderer);

    { Request from parent TCastleScene to call our PrepareResources at next time. }
    procedure SchedulePrepareResources;
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
    procedure PrepareResources(const ARenderer: TRenderer);
    procedure GLContextClose;

    function UseBlending: Boolean;

    { Shape resources are associated with (prepared for) this renderer,
      if non-nil.
      - PrepareResources (and internal RendererAttach) set it to non-nil,
      - GLContextClose (and internal RendererDetach) sets it to nil. }
    property Renderer: TRenderer read FRenderer;
  end;

  { Shape with additional information how to render it inside a world,
    that allows to render it independently of the containing TCastleScene. }
  TCollectedShape = class
    Shape: TGLShape;
    RenderOptions: TCastleRenderOptions;
    SceneTransform: TMatrix4;
  end;

  TCollectedShapeList = class({$ifdef FPC}specialize{$endif} TObjectList<TCollectedShape>)
  strict private
    SortPosition: TVector3;

    function CompareBackToFront2D(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareBackToFront3DBox(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareBackToFront3DOrigin(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareBackToFront3DGround(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;

    function CompareFrontToBack2D(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBack3DBox(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBack3DOrigin(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function CompareFrontToBack3DGround(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
  public
    { Sort shapes by distance to given Position point, farthest first.
      BlendingSort determines the sorting algorithm.
      See @link(TBlendingSort) documentation. }
    procedure SortBackToFront(const Position: TVector3;
      const BlendingSort: TBlendingSort);

    { Sort shapes by distance to given Position point, closest first.
      BlendingSort determines the sorting algorithm.
      See @link(TBlendingSort) documentation. }
    procedure SortFrontToBack(const Position: TVector3;
      const BlendingSort: TBlendingSort);
  end;

implementation

uses Generics.Defaults, Math, SysUtils,
  CastleScene, CastleBoxes;

{ TGLShape --------------------------------------------------------------- }

destructor TGLShape.Destroy;
begin
  FreeAndNil(FRendererObserver);
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
    if Renderer <> nil then
    begin
      Renderer.UnprepareTexture(State.MainTexture);
      RendererDetach;
    end;
    SchedulePrepareResources;
  end;
end;

procedure TGLShape.RendererDetach;
begin
  if Renderer <> nil then
  begin
    Renderer.UnprepareShape(Self);
    FRenderer := nil;
  end;
end;

procedure TGLShape.RendererFreeNotification(const Sender: TFreeNotificationObserver);
begin
  RendererDetach;
end;

procedure TGLShape.RendererAttach(const ARenderer: TRenderer);
var
  RenderOptions: TCastleRenderOptions;
begin
  // create FRendererObserver on-demand
  if FRendererObserver = nil then
  begin
    FRendererObserver := TFreeNotificationObserver.Create(nil);
    FRendererObserver.OnFreeNotification := {$ifdef FPC}@{$endif} RendererFreeNotification;
  end;

  FRenderer := ARenderer;
  FRendererObserver.Observed := ARenderer;

  // TODO: always depend on local RenderOptions field
  if OverrideRenderOptions <> nil then
    RenderOptions := OverrideRenderOptions
  else
    RenderOptions := TCastleScene(ParentScene).RenderOptions;
  Assert(RenderOptions <> nil);
  Renderer.PrepareShape(Self, RenderOptions);
end;

procedure TGLShape.PrepareResources(const ARenderer: TRenderer);
var
  RenderOptions: TCastleRenderOptions;
begin
  if Renderer <> ARenderer then
  begin
    RendererDetach;
    if ARenderer <> nil then
      RendererAttach(ARenderer);
  end;

  // TODO: always depend on local RenderOptions field
  if OverrideRenderOptions <> nil then
    RenderOptions := OverrideRenderOptions
  else
    RenderOptions := TCastleScene(ParentScene).RenderOptions;
  Assert(RenderOptions <> nil);
end;

procedure TGLShape.GLContextClose;

  { Free Arrays and Vbo of all shapes. }
  procedure FreeCaches;
  var
    Pass: TTotalRenderingPass;
  begin
    { We don't put this in Renderer.UnprepareShape, even though it would make
      sense for consistency, because

      - In the long run, we want to make prepare/unprepare independent from
        Renderer.
      - This makes escape-universe assertion fail at exit:
        When Renderer = nil, Cache should also be nil (castlesceneinternalshape.pas, line 246)
    }
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
  RendererDetach;
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
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(
    A.Shape.BoundingBox.Transform(A.SceneTransform),
    B.Shape.BoundingBox.Transform(B.SceneTransform));
end;

function TCollectedShapeList.CompareBackToFront3DBox(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(
    A.Shape.BoundingBox.Transform(A.SceneTransform),
    B.Shape.BoundingBox.Transform(B.SceneTransform),
    SortPosition);
end;

function TCollectedShapeList.CompareBackToFront3DOrigin(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
var
  PointA, PointB: TVector3;
begin
  PointA := (A.SceneTransform * A.Shape.OriginalState.Transformation.Transform).MultPoint(TVector3.Zero);
  PointB := (B.SceneTransform * B.Shape.OriginalState.Transformation.Transform).MultPoint(TVector3.Zero);
  Result := Sign(
    PointsDistanceSqr(PointB, SortPosition) -
    PointsDistanceSqr(PointA, SortPosition));
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
    PointsDistanceSqr(PointB, SortPosition) -
    PointsDistanceSqr(PointA, SortPosition));
end;

procedure TCollectedShapeList.SortBackToFront(const Position: TVector3;
  const BlendingSort: TBlendingSort);
begin
  SortPosition := Position;
  case BlendingSort of
    bs2D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront2D));
    bs3D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront3DBox));
    bs3DOrigin: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront3DOrigin));
    bs3DGround: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareBackToFront3DGround));
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

procedure TCollectedShapeList.SortFrontToBack(const Position: TVector3;
  const BlendingSort: TBlendingSort);
begin
  SortPosition := Position;
  case BlendingSort of
    bs2D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack2D));
    bs3D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack3DBox));
    bs3DOrigin: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack3DOrigin));
    bs3DGround: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif} CompareFrontToBack3DGround));
    else ;
  end;
end;

end.
