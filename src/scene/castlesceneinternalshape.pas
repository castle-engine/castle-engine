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
    FRenderer: TGLRenderer;
    FRendererObserver: TFreeNotificationObserver;

    procedure RendererFreeNotification(const Sender: TFreeNotificationObserver);

    { Unassociate with Renderer and set it to nil, if non-nil. }
    procedure RendererDetach;

    { Associate with ARenderer and assign it as Renderer.
      Doesn't take care of deinitializing previous Renderer value,
      or even checking is it different than ARenderer. }
    procedure RendererAttach(const ARenderer: TGLRenderer);
  public
    UseAlphaChannel: TAlphaChannel;
    { Is UseAlphaChannel calculated and current. }
    PreparedUseAlphaChannel: boolean;

    PassedFrustumAndDistanceCulling: Boolean;

    { Used only when RenderOptions.ReallyOcclusionQuery.
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
    procedure PrepareResources(const ARenderer: TGLRenderer);
    procedure GLContextClose;

    { Request from parent TCastleScene to call our PrepareResources at next time. }
    // TODO: restore somehow?
    //procedure SchedulePrepareResources; virtual; abstract;

    function UseBlending: Boolean;

    { Shape resources are associated with (prepared for) this renderer,
      if non-nil.
      - PrepareResources (and internal RendererAttach) set it to non-nil,
      - GLContextClose (and internal RendererDetach) sets it to nil. }
    property Renderer: TGLRenderer read FRenderer;
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
    function IsSmallerBackToFront2D(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function IsSmallerBackToFront3DBox(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function IsSmallerBackToFront3DOrigin(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
    function IsSmallerBackToFront3DGround(
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
  public
    { Sort shapes by distance to given Position point, farthest first.
      BlendingSort determines the sorting algorithm.
      See @link(TBlendingSort) documentation. }
    procedure SortBackToFront(const Position: TVector3;
      const BlendingSort: TBlendingSort);
  end;

{ Checks that any occlusion query algorithm should be used,
  equivalent to
  "ReallyOcclusionQuery(RenderOptions) or ReallyHierarchicalOcclusionQuery(RenderOptions)". }
function ReallyAnyOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;

{ Checks OcclusionQuery, existence of GLFeatures.OcclusionQuery,
  and GLFeatures.OcclusionQueryCounterBits > 0. If @false,
  occlusion query cannot be used.

  Also, returns @false when HierarchicalOcclusionQuery is @true
  --- because then HierarchicalOcclusionQuery should take precedence.

  @exclude Internal. }
function ReallyOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;

{ Checks HierarchicalOcclusionQuery, existence of GLFeatures.OcclusionQuery,
  and GLFeatures.OcclusionQueryCounterBits > 0. If @false,
  occlusion query cannot be used.

  @exclude Internal. }
function ReallyHierarchicalOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;

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
    PreparedUseAlphaChannel := false;
    //TODO:SchedulePrepareResources;
  end;

  { When Material.transparency changes, recalculate UseAlphaChannel. }
  if chAlphaChannel in Changes then
  begin
    PreparedUseAlphaChannel := false;
    //TODO:SchedulePrepareResources;
  end;
end;

procedure TGLShape.RendererDetach;

  { When Renderer is nil, we should not hold any resources connected to it. }
  procedure CheckNoCaches;
  var
    Pass: TTotalRenderingPass;
  begin
    { This follows from TGLRenderer.UnprepareShape implementation. }
    Assert(Cache = nil, 'When Renderer = nil, Cache should also be nil');
    for Pass := Low(Pass) to High(Pass) do
      Assert(ProgramCache[Pass] = nil, 'When Renderer = nil, all ProgramCache[Pass] should also be nil');
  end;

begin
  if Renderer <> nil then
  begin
    Renderer.UnprepareShape(Self);
    FRenderer := nil;
  end;

  CheckNoCaches;
end;

procedure TGLShape.RendererFreeNotification(const Sender: TFreeNotificationObserver);
begin
  RendererDetach;
end;

procedure TGLShape.RendererAttach(const ARenderer: TGLRenderer);
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
  Renderer.PrepareShape(Self, RenderOptions);
end;

procedure TGLShape.PrepareResources(const ARenderer: TGLRenderer);
var
  RenderOptions: TCastleRenderOptions;
begin
  if Renderer <> ARenderer then
  begin
    RendererDetach;
    if ARenderer <> nil then
      RendererAttach(ARenderer);
  end;

  if not PreparedUseAlphaChannel then
  begin
    { UseAlphaChannel is used by RenderScene to decide is Blending used for given
      shape. }
    UseAlphaChannel := AlphaChannel;
    PreparedUseAlphaChannel := true;
  end;

  // TODO: always depend on local RenderOptions field
  if OverrideRenderOptions <> nil then
    RenderOptions := OverrideRenderOptions
  else
    RenderOptions := TCastleScene(ParentScene).RenderOptions;

  if ReallyOcclusionQuery(RenderOptions) and
     (OcclusionQueryId = 0) then
  begin
    glGenQueries(1, @OcclusionQueryId);
    OcclusionQueryAsked := false;
  end;
end;

procedure TGLShape.GLContextClose;
begin
  RendererDetach;

  PreparedUseAlphaChannel := false;

  if OcclusionQueryId <> 0 then
  begin
    glDeleteQueries(1, @OcclusionQueryId);
    OcclusionQueryId := 0;
  end;
end;

function TGLShape.UseBlending: Boolean;
begin
  Result := UseAlphaChannel = acBlending;
end;

{ global routines ------------------------------------------------------------ }

function ReallyAnyOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;
begin
  {$warnings off}
  Result :=
    (RenderOptions.OcclusionQuery or RenderOptions.HierarchicalOcclusionQuery) and
    (GLFeatures <> nil) and // this can be called when GL context not initialized, like from TCastleScene.ViewChangedSuddenly
    GLFeatures.OcclusionQuery and
    GLFeatures.VertexBufferObject and
    (GLFeatures.OcclusionQueryCounterBits > 0);
  {$warnings on}

  // unoptimal version
  Assert(Result = (
    ReallyOcclusionQuery(RenderOptions) or
    ReallyHierarchicalOcclusionQuery(RenderOptions)
  ));
end;

function ReallyOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;
begin
  {$warnings off}
  Result := RenderOptions.OcclusionQuery and
    (not RenderOptions.HierarchicalOcclusionQuery) and
    (GLFeatures <> nil) and // this can be called when GL context not initialized, like from TCastleScene.ViewChangedSuddenly
    GLFeatures.OcclusionQuery and
    GLFeatures.VertexBufferObject and
    (GLFeatures.OcclusionQueryCounterBits > 0);
  {$warnings on}
end;

function ReallyHierarchicalOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;
begin
  {$warnings off}
  Result := RenderOptions.HierarchicalOcclusionQuery and
    (GLFeatures <> nil) and // this can be called when GL context not initialized, like from TCastleScene.ViewChangedSuddenly
    GLFeatures.OcclusionQuery and
    GLFeatures.VertexBufferObject and
    (GLFeatures.OcclusionQueryCounterBits > 0);
  {$warnings on}
end;

{ TCollectedShape ---------------------------------------------------------- }

type
  TCollectedShapeComparer = {$ifdef FPC}specialize{$endif} TComparer<TCollectedShape>;

(*
TODO: unused now.
If anything, this should just revert other sort method?

function TCollectedShapeList.IsSmallerFrontToBack(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  { To revert the order, we revert the order of A and B as passed to CompareBackToFront3D. }
  Result := TBox3D.CompareBackToFront3D(
    B.BoundingBox,
    A.BoundingBox,
    SortPosition);
end;
*)

function TCollectedShapeList.IsSmallerBackToFront2D(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(
    A.Shape.BoundingBox.Transform(A.SceneTransform),
    B.Shape.BoundingBox.Transform(B.SceneTransform));
end;

function TCollectedShapeList.IsSmallerBackToFront3DBox(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TCollectedShape): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(
    A.Shape.BoundingBox.Transform(A.SceneTransform),
    B.Shape.BoundingBox.Transform(B.SceneTransform),
    SortPosition);
end;

function TCollectedShapeList.IsSmallerBackToFront3DOrigin(
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

function TCollectedShapeList.IsSmallerBackToFront3DGround(
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
    bs2D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront2D));
    bs3D      : Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront3DBox));
    bs3DOrigin: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront3DOrigin));
    bs3DGround: Sort(TCollectedShapeComparer.Construct({$ifdef FPC}@{$endif}IsSmallerBackToFront3DGround));
    else ;
  end;
end;

end.
