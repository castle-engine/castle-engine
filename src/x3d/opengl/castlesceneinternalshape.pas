{
  Copyright 2003-2018 Michalis Kamburelis.

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

uses X3DNodes, X3DFields, CastleImages,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleInternalRenderer, CastleRenderOptions;

type
  { Shape within a scene rendered using OpenGL.
    This is TShape extended with some information needed by TCastleScene.
    Non-internal units never expose instances of this class. }
  TGLShape = class abstract(TX3DRendererShape)
  public
    { Keeps track if this shape was passed to Renderer.Prepare. }
    PreparedForRenderer: boolean;

    UseAlphaChannel: TAlphaChannel;
    { Is UseAlphaChannel calculated and current. }
    PreparedUseAlphaChannel: boolean;

    PassedShapeCulling: Boolean;

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

    procedure Changed(const InactiveOnly: boolean;
      const Changes: TX3DChanges); override;
    procedure PrepareResources;
    procedure GLContextClose;

    { Access the Renderer of parent TCastleScene. }
    function Renderer: TGLRenderer; virtual; abstract;

    { Request from parent TCastleScene to call our PrepareResources at next time. }
    procedure SchedulePrepareResources; virtual; abstract;

    function UseBlending: Boolean;
  end;

{ Checks OcclusionQuery, existence of GL_ARB_occlusion_query,
  and GLQueryCounterBits > 0. If @false, ARB_occlusion_query just cannot
  be used.

  Also, returns @false when HierarchicalOcclusionQuery is @true
  --- because then HierarchicalOcclusionQuery should take precedence.

  @exclude Internal. }
function ReallyOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;

{ Checks HierarchicalOcclusionQuery, existence of GL_ARB_occlusion_query,
  and GLQueryCounterBits > 0. If @false, ARB_occlusion_query just cannot
  be used.

  @exclude Internal. }
function ReallyHierarchicalOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;

implementation

uses CastleScene, CastleVectors, CastleGLUtils;

{ TGLShape --------------------------------------------------------------- }

procedure TGLShape.Changed(const InactiveOnly: boolean;
  const Changes: TX3DChanges);
begin
  inherited;

  if Cache <> nil then
  begin
    { Ignore changes that don't affect prepared arrays,
      like transformation, clip planes and everything else that is applied
      by renderer every time, and doesn't affect TGeometryArrays. }

    if Changes * [chCoordinate, chNormal, chTangent] <> [] then
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
    if Changes * [chTextureImage, chVisibleVRML1State, chGeometryVRML1State,
      chColorNode, chTextureCoordinate, chGeometry, chFontStyle, chWireframe] <> [] then
      Cache.InvalidateVertexData(AllVboTypes);
  end;

  if Changes * [chTextureImage, chTextureRendererProperties] <> [] then
  begin
    Renderer.UnprepareTexture(State.MainTexture);
    PreparedForRenderer := false;
    PreparedUseAlphaChannel := false;
    SchedulePrepareResources;
  end;

  { When Material.transparency changes, recalculate UseAlphaChannel. }
  if chAlphaChannel in Changes then
  begin
    PreparedUseAlphaChannel := false;
    SchedulePrepareResources;
  end;
end;

procedure TGLShape.PrepareResources;
begin
  if not PreparedForRenderer then
  begin
    Renderer.Prepare(Self);
    PreparedForRenderer := true;
  end;

  if not PreparedUseAlphaChannel then
  begin
    { UseAlphaChannel is used by RenderScene to decide is Blending used for given
      shape. }
    UseAlphaChannel := AlphaChannel;
    PreparedUseAlphaChannel := true;
  end;

  {$ifndef OpenGLES}
  if ReallyOcclusionQuery(TCastleScene(ParentScene).RenderOptions) and
     (OcclusionQueryId = 0) then
  begin
    glGenQueriesARB(1, @OcclusionQueryId);
    OcclusionQueryAsked := false;
  end;
  {$endif}
end;

procedure TGLShape.GLContextClose;
var
  Pass: TTotalRenderingPass;
begin
  PreparedForRenderer := false;
  PreparedUseAlphaChannel := false;

  {$ifndef OpenGLES}
  if OcclusionQueryId <> 0 then
  begin
    glDeleteQueriesARB(1, @OcclusionQueryId);
    OcclusionQueryId := 0;
  end;
  {$endif}

  { Free Arrays and Vbo of all shapes. }
  if Cache <> nil then
    Renderer.Cache.Shape_DecReference(Self, Cache);
  for Pass := Low(Pass) to High(Pass) do
    if ProgramCache[Pass] <> nil then
      Renderer.Cache.Program_DecReference(ProgramCache[Pass]);
end;

function TGLShape.UseBlending: Boolean;
begin
  Result := UseAlphaChannel = acBlending;
end;

{ global routines ------------------------------------------------------------ }

function ReallyOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;
begin
  {$warnings off}
  Result := RenderOptions.OcclusionQuery and
    (not RenderOptions.HierarchicalOcclusionQuery) and
    GLFeatures.ARB_occlusion_query and
    GLFeatures.VertexBufferObject and
    (GLFeatures.QueryCounterBits > 0);
  {$warnings on}
end;

function ReallyHierarchicalOcclusionQuery(const RenderOptions: TCastleRenderOptions): boolean;
begin
  {$warnings off}
  Result := RenderOptions.HierarchicalOcclusionQuery and
    GLFeatures.ARB_occlusion_query and
    GLFeatures.VertexBufferObject and
    (GLFeatures.QueryCounterBits > 0);
  {$warnings on}
end;

end.
