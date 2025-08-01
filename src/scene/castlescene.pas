{
  Copyright 2003-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering of scenes (TCastleScene). }
unit CastleScene;

{$I castleconf.inc}

{ TComponent.Height is being hidden by TCastleCone.Height, TCastleCylinder.Height.
  This is OK. }
{$ifndef FPC}{$warn HIDING_MEMBER off}{$endif}

{ By default, we don't define TCastleEnvironmentLight -- implementation is not finished yet
  (neither of TCastleEnvironmentLight, nor of underlying TEnvironmentLightNode). }
{.$define CASTLE_EXPERIMENTAL_ENVIRONMENT_LIGHT}

interface

uses SysUtils, Classes, Generics.Collections,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleVectors, CastleBoxes, X3DNodes, CastleClassUtils, CastleFonts,
  CastleUtils, CastleSceneCore, CastleInternalBackgroundRenderer,
  CastleGLUtils, CastleInternalShapeOctree, CastleInternalGLShadowVolumes, X3DFields,
  CastleTriangles, CastleShapes, CastleFrustum, CastleTransform, CastleGLShaders,
  CastleRectangles, CastleCameras, CastleRendererInternalShader, CastleColors,
  CastleSceneInternalShape, CastleInternalFileMonitor,
  CastleRenderOptions, CastleTimeUtils, CastleImages,
  CastleBehaviors, CastleInternalShapesRenderer, CastleSceneInternalBlending,
  CastleInternalPrimitiveMaterial;

{$define read_interface}

const
  prRenderSelf = CastleTransform.prRenderSelf;
  prRenderClones = CastleTransform.prRenderClones;
  prBackground = CastleTransform.prBackground;
  prBoundingBox = CastleTransform.prBoundingBox;
  prShadowVolume = CastleTransform.prShadowVolume;

type
  TCastleSceneList = class;

  TBeforeShapeRenderProc = procedure (Shape: TShape) of object;

  TRenderingAttributesEvent = TCastleRenderOptionsEvent deprecated 'use TCastleRenderOptionsEvent';
  TSceneRenderingAttributes = TCastleRenderOptions deprecated 'use TCastleRenderOptions';

  TPrepareResourcesOption = CastleTransform.TPrepareResourcesOption;
  TPrepareResourcesOptions = CastleTransform.TPrepareResourcesOptions;

  { Possible values for @link(TCastleScene.TransformOptimization). }
  TTransformOptimization = (
    { Automatically decide whether the transformation of this scene
      changes often enough to prefer optimizations for toStatic or toDynamic
      case.

      Right now, this behaves like toDynamic when the scene is present multiple
      times in the @link(TCastleAbstractRootTransform) hierarchy, for example
      when using @link(TCastleTransformReference) to render the same scene many times.
      Otherwise this behaves like toStatic.

      This automatic detection may make unoptimal decision,
      which is why you can set @link(TCastleScene.TransformOptimization) explicitly
      to toStatic or toDynamic if you "know better". Examples when it makes
      sense to manually select optimization:

      @unorderedList(
        @item(When the scene is present multiple times in the
          @link(TCastleAbstractRootTransform) hierarchy (e.g. when using
          @link(TCastleTransformReference)) but all these occurrences are
          very close to each other, thus are within radiuses of the same lights.

          In this case, automatic detection makes unoptimal decision
          to behave like toDynamic, which means shaders calculate more light
          sources. Setting @link(TCastleScene.TransformOptimization)
          to toStatic in this case is better.
        )

        @item(When the scene is present only once in the
          @link(TCastleAbstractRootTransform) hierarchy, but you know that
          you will change the scene translation very often (e.g. almost every frame)
          and the translation change will be large enough to make the scene
          affected by different light sources. Or maybe you know you will
          change the light's positions or radiuses very often by large values.

          In this case, automatic detection makes unoptimal decision
          to behave like toStatic, which means that shaders will be often recreated
          when the scene (or light sources) move.
          Setting @link(TCastleScene.TransformOptimization) to toDynamic
          in this case is better,
          it means that shaders for this shape will not change.
        )
      )
    }
    toAutomatic,

    { Choose optimizations that are beneficial for TCastleScene instances
      whose world transformation (translation, rotation, scale of this
      and all parents) never changes, or rarely changes, or changes only by
      a very small amount. }
    toStatic,

    { Choose optimizations that are beneficial for TCastleScene instances
      whose world transformation (translation, rotation, scale of this
      and all parents) changes often and by large values. }
    toDynamic
  );

  { Complete loading, processing and rendering of a scene.
    This is a descendant of @link(TCastleSceneCore) that adds efficient rendering. }
  TCastleScene = class(TCastleSceneCore)
  strict private
    type
      TSceneRenderOptions = class(TCastleRenderOptions)
      private
        OwnerScene: TCastleScene;
      protected
        procedure ReleaseCachedResources; override;
      end;

    var
      { Used by UpdateGeneratedTextures, to prevent rendering the shape
        for which reflection texture is generated. (This wouldn't cause
        recursive loop in our engine, but still it's bad --- rendering
        from the inside of the object usually obscures the world around...). }
      AvoidShapeRendering: TGLShape;

      { Used by UpdateGeneratedTextures, to prevent rendering non-shadow casters
        for shadow maps. }
      AvoidNonShadowCasterRendering: boolean;

      { Used by UpdateGeneratedTextures, to avoid updating twice during the same render. }
      UpdateGeneratedTexturesFrameId: TFrameId;

      FDistanceCulling: Single;

      FReceiveShadowVolumes: Boolean;
      FTempPrepareParams: TPrepareParams;

      { Camera position, in local scene coordinates, known during
        the LocalRender or LocalRenderShadowVolume calls. }
      RenderCameraPosition: TVector3;
      { Copy of TRenderParams.UsingShadowVolumes, known during
        the LocalRender or LocalRenderShadowVolume calls. }
      RenderUsingShadowVolumes: Boolean;

      FCastGlobalLights: Boolean;
      FWasVisibleFrameId: TFrameId;

      { Used by LocalRenderInside }
      FilteredShapes: TShapeList;

      { Valid only during TCastleScene.LocalRender.
        Callbacks assigned to ShapeCullingFunc and ShapeCullingOctreeFunc may use it. }
      FrustumForShapeCulling: PFrustum;

      FShapeFrustumCulling, FSceneFrustumCulling: Boolean;
      FRenderOptions: TCastleRenderOptions;
      FTransformOptimization: TTransformOptimization;

      { These fields are valid only during LocalRenderInside and CollectShape_ methods. }
      Render_Params: TRenderParams;
      Render_TestShapeVisibility: TTestShapeVisibility;
      Render_Collector: TShapesCollector;

    { Collect Shape, adding it to Render_Collector. }
    procedure CollectShape_NoTests(const Shape: TGLShape);
    { Render Shape if all tests pass.
      Checks everything except TestShapeVisibility callback,
      so it assumes that filtering by TestShapeVisibility is already done. }
    procedure CollectShape_SomeTests(const Shape: TGLShape);

    procedure ResetShapeVisible(const Shape: TShape);

    { Collect all shapes for rendering, into Params.Collector.
      The actual rendering will be done using TCastleViewport and TShapesRenderer.

      Adds all potentially visible shapes to Params.Collector.
      "Potentially visible" is decided by TestShapeVisibility
      (shape is visible if TestShapeVisibility is @nil or returns
      @true for this shape).

      Updates Params.Statistics. }
    procedure LocalRenderInside(const TestShapeVisibility: TTestShapeVisibility;
      const Params: TRenderParams);

    { Render everything using LocalRenderInside.
      The rendering parameters are configurable by @link(RenderOptions).

      The shapes are only filtered here:

      - frustum culling,
      - distance culling,
      - filtering using TestShapeVisibility.

      Shapes that pass send to Params.Collector.

      The actual rendering will be done using TCastleViewport and TShapesRenderer. }
    procedure LocalRenderOutside(
      const TestShapeVisibility: TTestShapeVisibility;
      const Params: TRenderParams);

    { Fog for this shape. @nil if none. }
    function ShapeFog(const Shape: TShape; const GlobalFog: TFogNode): TFogFunctionality;

    { Check frustum and distance culling. }
    function ShapePossiblyVisible(Shape: TShape): boolean;

    { Should given shape be rendered, according to distance culling.
      Call only when DistanceCulling > 0. }
    function DistanceCullingCheckShape(const Shape: TShape): Boolean;

    { Should given scene be rendered, according to distance culling.
      Call only when DistanceCulling > 0. }
    function DistanceCullingCheckScene: Boolean;

    procedure SetShapeFrustumCulling(const Value: Boolean);
    procedure SetDistanceCulling(const Value: Single);

    function RenderFrustumOctree_TestShape(Shape: TShape): boolean;
    procedure RenderWithOctree_CheckShapeCulling(
      ShapeIndex: Integer; CollidesForSure: boolean);
    procedure SetCastGlobalLights(const Value: Boolean);

    { Treating the scene as "whole scene 2-manifold", because of
      InternalDetectedWholeSceneManifold or RenderOptions.WholeSceneManifold.
      Calling this may have a cost: on-demand calculation of
      InternalDetectedWholeSceneManifold, so use this only when a light source
      casting shadow volumes is present. }
    function EffectiveWholeSceneManifold: Boolean;

    { Can this be shadow caster for shadow volumes.
      This is not concerned whether we have light using shadow volumes,
      and this is not concerned whether the scene or shapes are 2-manifold. }
    function EffectiveCastShadowVolumes: Boolean;
  private
    PreparedShapesResources, PreparedRender: Boolean;
  protected
    (*Override TCastleRenderOptions passed to TShapesCollector when rendering
      these shapes. May be tweaked during rendering,
      to render the same shape/scene with multiple render options.

      Sample usage:

      @longCode(#
      procedure TCastleSceneChromaticAberration.LocalRender(const Params: TRenderParams);

        { Like MultMatricesTranslation, but the translation is applied by multiplying
          from the other side. }
        procedure GlobalMultMatricesTranslation(var M, MInvert: TMatrix4;
          const Transl: TVector3);
        begin
          MultMatricesTranslation(MInvert, M, Transl);
        end;

      var
        // Saved Params.Xxx before applying our transformation
        SavedTransformationPtr: PTransformation;
        SavedFrustumPtr: PFrustum;
        // Values after applying our transformation
        NewTransformation: TTransformation;
        NewFrustum: TFrustum;

        { Shift and render (based on TCastleTransform.Render). }
        procedure RenderShiftedBegin(const Shift: TVector3);
        begin
          SavedTransformationPtr := Params.Transformation;
          SavedFrustumPtr        := Params.Frustum;
          Assert(SavedFrustumPtr <> nil);

          NewTransformation := SavedTransformationPtr^;
          NewFrustum        := SavedFrustumPtr^;

          Params.Transformation := @NewTransformation;
          Params.Frustum        := @NewFrustum;

          GlobalMultMatricesTranslation(
            NewTransformation.Transform, NewTransformation.InverseTransform, Shift);
          // Use old frustum, as shifting it is not so easy
          //NewFrustumValue.MoveVar(-Shift);
        end;

        procedure RenderShiftedEnd;
        begin
          { Restore SavedXxxPtr values.
            They can be restored fast, thanks to restoring pointers, not content. }
          Params.Transformation := SavedTransformationPtr;
          Params.Frustum        := SavedFrustumPtr;
        end;

      var
        SavedChannels: TColorChannels;
      begin
        if FChromaticAberrationStrength > 0 then
        begin
          { Create another instance of TCastleRenderOptions,
            because engine ShapesCollector will only store references to TCastleRenderOptions,
            and we need to pass 2 copies of the scene with 2 different render options. }
          if AltRenderOptions = nil then
            AltRenderOptions := TCastleRenderOptions.Create(Self);

          // red-green not shifted
          AltRenderOptions.InternalColorChannels := [0..1, 3];
          InternalOverrideRenderOptions := AltRenderOptions;
          inherited;
          InternalOverrideRenderOptions := nil;

          // blue shifted
          RenderOptions.InternalColorChannels := [2, 3];
          RenderShiftedBegin(FChromaticAberrationShift);
          // Note: we avoid calling "inherited" in a nested procedure, this seems broken with FPC 3.0.4.
          inherited LocalRender(Params);
          RenderShiftedEnd;
        end else
        begin
          RenderOptions.InternalColorChannels := [0..3];
          inherited;
        end;
      end;
      #)
    *)
    InternalOverrideRenderOptions: TCastleRenderOptions;

    function CreateShape(const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState;
      const ParentInfo: PTraversingInfo): TShape; override;
    procedure InternalInvalidateBackgroundRenderer; override;

    procedure LocalRender(const Params: TRenderParams); override;

    { Render shadow volume (sides and caps) of this scene, for shadow volume
      algorithm.

      Uses ShadowVolumeRenderer for rendering, and to detect if rendering
      is necessary at all.

      It always uses silhouette optimization. This is the usual,
      fast method of rendering shadow volumes.

      All shadow quads are generated from scene triangles transformed
      by ParentTransform. We must be able to correctly detect front and
      back facing triangles with respect to light position,
      so ShadowVolumeRenderer.LightPosition and
      "this scene transformed by ParentTransform" must be in the same coordinate system.
      If ParentTransformIsIdentity then ParentTransform value is ignored and
      everything works like ParentTransform = identity matrix (and is a little
      faster in this special case).

      Uses TrianglesListShadowCasters and ManifoldEdges and BorderEdges
      (so you may prefer to prepare it before, e.g. by calling PrepareResources
      with prShadowVolume included).

      We look at some RenderOptions, like RenderOptions.Blending, because transparent
      triangles have to be handled a little differently, and when
      RenderOptions.Blending = false then all triangles are forced to be opaque.
      In other words, this takes RenderOptions into account, to cooperate with
      our Render method.

      ShadowVolumeRenderer.LightPosition is the light position.
      ShadowVolumeRenderer.LightPosition[3] must be 1
      (to indicate positional light) or 0 (a directional light).
      It's expected that ShadowVolumeRenderer is already initialized by
      ShadowVolumeRenderer.InitFrustumAndLight.

      Faces (both shadow quads and caps) are rendered such that
      CCW <=> you're looking at it from outside
      (i.e. it's considered front face of this shadow volume). }
    procedure LocalRenderShadowVolume(const Params: TRenderParams;
      const ShadowVolumeRenderer: TBaseShadowVolumeRenderer); override;

    procedure ChangeWorld(const Value: TCastleAbstractRootTransform); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Destroy any associations of this object with current OpenGL context.
      For example, release any allocated texture names.

      Generally speaking, destroys everything that is allocated by
      PrepareResources call. It's harmless to call this
      method when there are already no associations with current OpenGL context.
      This is called automatically from the destructor. }
    procedure GLContextClose; override;

    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const Params: TPrepareParams); override;
      deprecated 'this is internal; use TCastleViewport.PrepareResources to prepare transformations';

    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); override;

    { Does this transform have a collision mesh that TCastleMeshCollider can use. }
    function HasColliderMesh: Boolean; override;
    { Enumerate triangles for a collision mesh that TCastleMeshCollider can use. }
    procedure ColliderMesh(const TriangleEvent: TTriangleEvent); override;
  private
    { Node for which FBackground is currently prepared. }
    FBackgroundNode: TAbstractBindableNode;
    { Cached BackgroundRenderer value }
    FBackgroundRenderer: TBackgroundRenderer;
    { Is FBackgroundRenderer valid?

      We can't use "FBackgroundRenderer = nil" to detect this,
      bacause nil is valid value for FBackgroundRenderer in case there's no
      (supported) FBackgroundNode.

      If not FBackgroundRendererValid then FBackgroundRenderer must always be nil.
      Never set FBackgroundRendererValid to false directly - use InternalInvalidateBackgroundRenderer,
      this will automatically call FreeAndNil(FBackgroundRenderer) before setting
      FBackgroundRendererValid to false. }
    FBackgroundRendererValid: boolean;
    procedure PrepareBackground;
  public
    { Internal override test visibility. }
    InternalVisibilityTest: TTestShapeVisibility;

    procedure FreeResources(Resources: TSceneFreeResources); override;

    { TBackgroundRenderer instance to render the background defined in this scene.
      Current background is the top node on the BackgroundStack of this scene,
      following X3D specifications, and can be animated.
      The TCastleViewport should use this to render background.

      You should not access the background this way in your own code.
      This is public only because our own TCastleViewport needs to access it.

      If you want to change the background,
      instead of using this internal reference,
      access X3D background nodes in @code(BackgroundStack).
      You can modify existing background node by @code(BackgroundStack.Top),
      or you can push a different background node by adding @link(TBackgroundNode)
      to @link(RootNode) and setting @link(TAbstractBindableNode.Bound Background.Bound) to @true.

      Returns @nil if there is no currently bound (and supported) background node
      in this scene.

      This instance is managed (automatically created/freed
      and so on) by this TCastleScene instance. It is cached
      (so that it's recreated only when relevant things change,
      like X3D nodes affecting this background).

      @exclude }
    function InternalBackgroundRenderer: TBackgroundRenderer;

    function Attributes: TCastleRenderOptions; deprecated 'use RenderOptions';

    procedure InternalCameraChanged; override;

    { Screen effects information, used by TCastleViewport.ScreenEffects.
      ScreenEffectsCount may actually prepare screen effects.
      @exclude
      @groupBegin }
    function InternalScreenEffects(Index: Integer): TGLSLProgram;
    function InternalScreenEffectsCount: Integer;
    function InternalScreenEffectsNeedDepth: boolean;
    { @groupEnd }

    { Make TGLShape.PrepareResources call on all shapes before next render.
      @exclude }
    procedure InternalSchedulePrepareResources;

    { Create a scene with the same contents (X3D scene graph) as this one.
      The created scene has exactly the same class as this one
      (we use ClassType.Create to call a virtual constructor).

      Note that this @bold(does not copy other scene properties),
      like @link(ProcessEvents) or @link(RenderOptions) contents. }
    function Clone(const AOwner: TComponent): TCastleScene;

    { Whether the scene was (potentially, at least partially) visible
      in the last rendering event.

      The "was visible" means that "some shape was visible", that is:
      some shape passed frustum culling, distance culling
      and occlusion culling (see https://castle-engine.io/occlusion_culling )
      tests.

      The result of this method is not affected by:

      @unorderedList(
        @item(Lights.

          That is, it doesn't matter whether this scene contains some lights
          (X3D light nodes in @link(RootNode))
          that possibly make some other scenes brighter.
        )

        @item(Background.

          It doesn't matter whether the scene contains some background
          that affects TCastleViewport skybox.)

        @item(Children scenes.

          It doesn't matter if you have any children scenes
          (TCastleScene instances that are children of this)
          and whether they are visible.
          This follows the general rule that TCastleScene-specific features
          do not look/affect children scenes, including children TCastleScene.
          Only TCastleTransform features, like @link(TCastleTransform.Exists),
          are applied recursively, i.e. they generally affect the children
          transformations.
        )
      )

      To summarize and emphasize: for this method,
      @bold(only the visibility of shapes within this scene matters).

      If this scene instance is used multiple times within some viewport,
      or when multiple viewports render the same scene,
      then it is enough that at least one shape in one of the scene instances
      was visible last frame.

      TODO: For now, occlusion culling doesn't affect this, i.e. if the scene
      is not visible because occlusion culling. }
    function WasVisible: Boolean;

    { Add shader effects to configure how is the component rendered.
      See https://castle-engine.io/shaders for documentation
      how shader effects work in Castle Game Engine. }
    procedure SetEffects(const Value: array of TEffectNode);
  published
    { Improve performance of rendering by checking for each shape whether
      it is inside frustum (camera pyramid of view) before rendering.

      This is almost always a good idea.
      Exception may be when, in the most common scene position,
      all the shapes are inside the frustum,
      or all the shapes are outside the frustum.
      In this case this check is wasting time,
      and it matters if you have a @italic(lot of shapes).
      In such case, @link(SceneFrustumCulling) will be enough. }
    property ShapeFrustumCulling: Boolean
      read FShapeFrustumCulling write SetShapeFrustumCulling default true;

    { Improve performance of rendering by checking for the whole scene
      whether it is inside frustum (camera pyramid of view) before rendering.

      This is almost always a good idea.
      Exception may be when the scene is almost always within the frustum,
      and you have a @italic(lot of scenes). In such case, this check may be
      a waste of time. }
    property SceneFrustumCulling: Boolean
      read FSceneFrustumCulling write FSceneFrustumCulling default true;

    { Does this scene receive shadows by shadow volumes. }
    property ReceiveShadowVolumes: boolean
      read FReceiveShadowVolumes write FReceiveShadowVolumes default true;

    { Cull shapes farther than this distance. Ignored if <= 0. }
    property DistanceCulling: Single
      read FDistanceCulling write SetDistanceCulling {$ifdef FPC}default 0{$endif};

    { Rendering options.
      You are free to change them at any time. }
    property RenderOptions: TCastleRenderOptions read FRenderOptions;

    { Lights defines by given scene shine on everything in the viewport, including all other TCastleScene. }
    property CastGlobalLights: Boolean
      read FCastGlobalLights write SetCastGlobalLights default false;

    { Optimization hint, determines which optimizations to use for this scene.
      Some optimizations are beneficial for scenes whose world transformation
      (translation, rotation, scale of this and all parents) never changes,
      or rarely changes, or changes only by a small amount.
      Some optimizations are the reverse: they make sense for scenes whose
      world transformation changes often and by large values.

      See @link(TTransformOptimization) for possible values and more information.

      The default value is @link(toAutomatic), which means that we auto-detect
      the optimal approach.

      For now, this only determines whether generated shaders...

      @unorderedList(
        @item(...include code to calculate all the light sources.

          This is more optimal for dynamic transformations,
          avoids the need to recreate shaders when transformation changes.)

        @item(...include code only for light sources whose radius includes the
          particular shape in the scene.

          This is more optimal for static transformations, makes shader code
          smaller, no point recalculating light sources whose influence is zero.)
      )

      An unofficial way (we don't guarantee it will always be exposed this way)
      to test how often shaders are recreated is by setting
      @code(LogRendererCache := true) from unit @code(CastleInternalRenderer).
      Observe the log output to see how often shaders are recreated.
      If you tweak this property, be sure to check whether this is improved. }
    property TransformOptimization: TTransformOptimization
      read FTransformOptimization write FTransformOptimization default toAutomatic;
  end;

  TCastleSceneClass = class of TCastleScene;

  TCastleSceneList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleScene>)
  end;

  TTriangle4List = {$ifdef FPC}specialize{$endif} TStructList<TTriangle4>;

  { @exclude Internal.

    Basic non-abstract implementation of render params for calling
    TCastleTransform.LocalRender.

    @bold(This is exposed here only to support some experiments with non-standard
    rendering in engine example programs. Do not use this in your own code.)

    This can be used when you have to call TCastleTransform.LocalRender,
    but you don't use TCastleViewport.
    Usually this should not be needed.
    This class may be removed at some point!
    You should always try to use TCastleViewport to manage and render
    3D stuff in new programs, and then TCastleViewport will take care of creating
    proper render params instance for you. }
  TBasicRenderParams = class(TRenderParams)
  public
    FGlobalLights: TLightInstancesList;
    constructor Create;
    destructor Destroy; override;
    function GlobalLights: TAbstractLightInstancesList; override;
  end;

var
  InternalEnableRendering: Boolean = true;

  DynamicBatching: Boolean = false
    deprecated 'use TCastleViewport.DynamicBatching';

const
  { We recommend using CastleRenderOptions unit to get these types.
    But for backward compatibility, they are also available here. }

  { }
  weNormal = CastleRenderOptions.weNormal;
  weWireframeOnly = CastleRenderOptions.weWireframeOnly;
  weSolidWireframe = CastleRenderOptions.weSolidWireframe;
  weSilhouette = CastleRenderOptions.weSilhouette;

{$define read_interface}
{$I castlescene_roottransform.inc}
{$I castlescene_abstractprimitive.inc}
{$I castlescene_text.inc}
{$I castlescene_box.inc}
{$I castlescene_sphere.inc}
{$I castlescene_plane.inc}
{$I castlescene_cone.inc}
{$I castlescene_cylinder.inc}
{$I castlescene_imagetransform.inc}
{$I castlescene_background.inc}
{$I castlescene_fog.inc}
{$I castlescene_editorgizmo.inc}
{$I castlescene_abstractlight.inc}
{$I castlescene_punctuallight.inc}
{$I castlescene_pointlight.inc}
{$I castlescene_directionallight.inc}
{$I castlescene_spotlight.inc}
{$ifdef CASTLE_EXPERIMENTAL_ENVIRONMENT_LIGHT}
  {$I castlescene_environmentlight.inc}
{$endif}
{$undef read_interface}

implementation

uses Math,
  CastleGLVersion, CastleLog, CastleStringUtils, CastleApplicationProperties,
  CastleShapeInternalRenderShadowVolumes, CastleUriUtils, CastleProjection,
  CastleComponentSerialize, CastleRenderContext, CastleFilesUtils,
  CastleInternalGLUtils, CastleInternalRenderer, X3DCameraUtils;

{$define read_implementation}
{$I castlescene_roottransform.inc}
{$I castlescene_abstractprimitive.inc}
{$I castlescene_text.inc}
{$I castlescene_box.inc}
{$I castlescene_sphere.inc}
{$I castlescene_plane.inc}
{$I castlescene_cone.inc}
{$I castlescene_cylinder.inc}
{$I castlescene_imagetransform.inc}
{$I castlescene_background.inc}
{$I castlescene_fog.inc}
{$I castlescene_editorgizmo.inc}
{$I castlescene_abstractlight.inc}
{$I castlescene_punctuallight.inc}
{$I castlescene_pointlight.inc}
{$I castlescene_directionallight.inc}
{$I castlescene_spotlight.inc}
{$ifdef CASTLE_EXPERIMENTAL_ENVIRONMENT_LIGHT}
  {$I castlescene_environmentlight.inc}
{$endif}
{$undef read_implementation}

{ TCastleScene.TSceneRenderOptions ------------------------------------------- }

procedure TCastleScene.TSceneRenderOptions.ReleaseCachedResources;
begin
  inherited;

  { Secure, in case this is called from TCastleRenderOptions constructor --
    possible if you assign TCastleRenderOptions.OnCreate there that e.g. changes
    PhongShading.
    Testcase: castle-game. }
  if OwnerScene = nil then
    Exit;

  { TCastleScene must be disconnected from OpenGL, to release
    - resources from shapes (shaders, textures, VBOs)
    - screen effects (referencing renderer shaders, maybe also textures if used)
    So full GLContextClose is needed. }
  OwnerScene.GLContextClose;
end;

{ TCastleScene ------------------------------------------------------------ }

constructor TCastleScene.Create(AOwner: TComponent);
begin
  { inherited Create *may* call some virtual things overriden here
    (although right now it doesn't): it may bind new viewpoint which
    may call ViewChangedSuddenly which is overridden here and uses RenderOptions.
    That's why I have to initialize them *before* "inherited Create" }

  { Setup RenderOptions as proper sub-component. }
  FRenderOptions := TSceneRenderOptions.Create(Self);
  (FRenderOptions as TSceneRenderOptions).OwnerScene := Self;
  FRenderOptions.SetSubComponent(true);
  FRenderOptions.Name := 'RenderOptions';

  inherited Create(AOwner);

  FBackgroundRendererValid := false;
  FBackgroundNode := nil;
  FBackgroundRenderer := nil;

  FSceneFrustumCulling := true;
  FShapeFrustumCulling := true;

  FReceiveShadowVolumes := true;

  FilteredShapes := TShapeList.Create;
  FTempPrepareParams := TPrepareParams.Create;
end;

destructor TCastleScene.Destroy;
begin
  FreeAndNil(FilteredShapes);
  FreeAndNil(FTempPrepareParams);

  { Make sure to free TCastleScene resources now, even though TCastleTransform.Destroy
    will also call it later -- but then we are in more "uninitialized" state. }
  GLContextClose;

  { Release now all connections between RootNode and Renderer.

    Old reason: when freeing RootNode in "inherited"
    (if OwnsRootNode = false), image references (from texture nodes)
    are decremented. So cache used when loading these images must be
    available.

    This old reason is no longer relevant: cache is now global RendererCache,
    it doesn't go away when we free Renderer.
    And X3D nodes should have no other links to renderer.

    And since TShapesRenderer took over, we don't even have Renderer
    instance here.

    Maybe we can remove this call one day, after testing. }
  FreeResources([frTextureDataInNodes, frBackgroundImageInNodes]);

  FreeAndNil(FRenderOptions);

  inherited;
end;

function TCastleScene.CreateShape(const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState; const ParentInfo: PTraversingInfo): TShape;
begin
  Result := TGLShape.Create(Self, AGeometry, AState, ParentInfo);
end;

procedure TCastleScene.GLContextClose;

  { This must be coded carefully, because
    - it's called by ChangedAll, and so may be called when our constructor
      didn't do it's work yet.
    - moreover it's called from destructor, so may be called if our
      constructor terminated with exception.
    So e.g. we have to check Renderer <> nil, Shapes <> nil here. }

  { Call TGLShape.GLContextClose. }
  procedure ShapesGLContextClose;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    if Shapes <> nil then
    begin
      { Iterate even over non-visible shapes too, for safety:
        since this GLContextClose may happen after some
        "visibility" changed, that is you changed proxy
        or such by event. }
      ShapeList := Shapes.TraverseList(false, false);
      for Shape in ShapeList do
        TGLShape(Shape).GLContextClose;
    end;
  end;

  { Release screen effects OpenGL stuff. }
  procedure ScreenEffectsGLContextClose;
  var
    I: Integer;
  begin
    if ScreenEffectNodes <> nil then
      for I := 0 to ScreenEffectNodes.Count - 1 do
        TScreenEffectNode(ScreenEffectNodes[I]).InternalRendererResourceFree;
  end;

  { When the OpenGL(ES) context is lost, generated textures contents are lost.
    Make sure to regenerate them when entering context again.
    Testcase: Silhouette on Android, switch from application and back. }
  procedure ScheduleUpdateGeneratedTextures;
  var
    I: Integer;
  begin
    if GeneratedTextures <> nil then
      for I := 0 to GeneratedTextures.Count - 1 do
        GeneratedTextures.L[I].Functionality.InternalUpdateNeeded := true;
  end;

begin
  inherited;

  PreparedRender := false;
  PreparedShapesResources := false;

  ScreenEffectsGLContextClose;

  ShapesGLContextClose;

  ScheduleUpdateGeneratedTextures;

  InternalInvalidateBackgroundRenderer;
end;

function TCastleScene.ShapeFog(const Shape: TShape; const GlobalFog: TFogNode): TFogFunctionality;
begin
  Result := nil;

  if {(Result = nil) and} (Shape.State.LocalFog <> nil) then
    Result := Shape.State.LocalFog.Functionality(TFogFunctionality) as TFogFunctionality;
  if (Result = nil) and (FogStack.Top <> nil) then
    Result := FogStack.Top.Functionality(TFogFunctionality) as TFogFunctionality;
  if (Result = nil) and (GlobalFog <> nil) then
    Result := GlobalFog.Functionality(TFogFunctionality) as TFogFunctionality;
end;

procedure TCastleScene.CollectShape_NoTests(const Shape: TGLShape);

  function SceneTransformDynamic: Boolean;
  begin
    case TransformOptimization of
      { Determine TCollectedShape.SceneTransformDynamic by considering
        is this TCastleScene present multiple times in TAbstactRootTransform.

        This means we have SceneTransformDynamic = true for TCastleTransformReference,
        and don't recreate shaders each frame when the scene is inside/outside
        the light radius, see
        https://github.com/castle-engine/castle-engine/issues/664 }
      toAutomatic: Result := InternalWorldReferences > 1;
      toStatic   : Result := false;
      toDynamic  : Result := true;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TransformOptimization?');
      {$endif}
    end;
  end;

  function EffectiveRenderOptions: TCastleRenderOptions;
  begin
    if InternalOverrideRenderOptions <> nil then
      Result := InternalOverrideRenderOptions
    else
      Result := RenderOptions;
  end;

begin
  { Whether the Shape is rendered directly or through batching,
    mark it "was visible this frame".
    Shape passed the frustum culling, distance culling
    and (TODO) occlusion culling tests at this point. }
  FWasVisibleFrameId := TFramesPerSecond.RenderFrameId;
  if Shape.Node <> nil then
    Shape.Node.InternalWasVisibleFrameId := TFramesPerSecond.RenderFrameId;

  Shape.Fog := ShapeFog(Shape, Render_Params.GlobalFog as TFogNode);

  Render_Collector.Add(Shape, EffectiveRenderOptions,
    Render_Params.Transformation^.Transform, SceneTransformDynamic,
    Render_Params.DepthRange, ReceiveShadowVolumes);
  IsVisibleNow := true;
end;

procedure TCastleScene.CollectShape_SomeTests(const Shape: TGLShape);
begin
  if (Shape <> AvoidShapeRendering) and
     ( (not AvoidNonShadowCasterRendering) or Shape.ShadowCaster) and
     ( { implement TAbstractShapeNode.Visible here, by a trivial check }
       (Shape.Node = nil) or Shape.Node.Visible
     ) then
  begin
    CollectShape_NoTests(Shape);
  end;
end;

procedure TCastleScene.LocalRenderInside(
  const TestShapeVisibility: TTestShapeVisibility;
  const Params: TRenderParams);

  procedure UpdateVisibilitySensors;
  var
    J: Integer;
    Instances: TVisibilitySensorInstanceList;
    NewActive: boolean;
    VisibilitySensorsPair: {$ifdef FPC}TVisibilitySensors.TDictionaryPair{$else}TPair<TVisibilitySensorNode, TVisibilitySensorInstanceList>{$endif};
  begin
    { optimize for common case: exit early if nothing to do }
    if (VisibilitySensors.Count = 0) or
       (Params.Frustum = nil) then
      Exit;

    if ProcessEvents then
    begin
      BeginChangesSchedule;
      try
        for VisibilitySensorsPair in VisibilitySensors do
          if VisibilitySensorsPair.Key.Enabled then
          begin
            { calculate NewActive }
            NewActive := false;
            Instances := VisibilitySensorsPair.Value;
            for J := 0 to Instances.Count - 1 do
              if Params.Frustum^.Box3DCollisionPossibleSimple(Instances[J].Box) then
              begin
                NewActive := true;
                Break;
              end;
            { Note that NextEventTime below increases time tick for every
              VisibilitySensor, which is good,
              otherwise sensors_environmental/visibility_sensor.x3dv
              has a problem at initialization, when multiple sensors
              send isActive = TRUE, and X3D mechanism to avoid loops
              kicks in. }
            VisibilitySensorsPair.Key.SetIsActive(NewActive, NextEventTime);
          end;
      finally EndChangesSchedule; end;
    end;
  end;

var
  I: Integer;
begin
  { We update XxxVisible only for one value of Params.Transparent.
    Otherwise, we would increase it twice.
    This method is always called first with Params.Transparent = false,
    then Params.Transparent = true during a single frame. }
  if Params.InternalPass = 0 then
  begin
    Params.Statistics.ShapesVisible := Params.Statistics.ShapesVisible +
      ShapesActiveVisibleCount;
    { also do this only once per frame }
    UpdateVisibilitySensors;
  end;

  Render_Params := Params;
  Render_TestShapeVisibility := TestShapeVisibility;
  Render_Collector := Params.Collector as TShapesCollector;

  ShapesFilter(Shapes, true, true, false, TestShapeVisibility, FilteredShapes);
  for I := 0 to FilteredShapes.Count - 1 do
    CollectShape_SomeTests(TGLShape(FilteredShapes[I]));
end;

procedure TCastleScene.PrepareResources(
  const Options: TPrepareResourcesOptions;
  const Params: TPrepareParams);
var
  Renderer: TRenderer;

  procedure PrepareShapesResources;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Shapes.TraverseList(false, false);
    for Shape in ShapeList do
      TGLShape(Shape).PrepareResources;
  end;

  procedure PrepareRenderShapes;
  var
    ShapeList: TShapeList;
    Shape: TShape;
    ReceivedGlobalLights: TLightInstancesList;
    DummyCamera: TRenderingCamera;
    DummyStatistics: TRenderStatistics;
  begin
    if LogRenderer then
      WritelnLog('Renderer', 'Preparing rendering of all shapes');

    FillChar(DummyStatistics, SizeOf(DummyStatistics), #0);

    { Note: we prepare also not visible shapes, in case they become visible. }
    ShapeList := Shapes.TraverseList(false, false);

    { Prepare resources by doing rendering.
      But with Renderer.RenderMode set to rmPrepareRenderXxx so nothing will be actually drawn. }

    if prRenderSelf in Options then
      Renderer.RenderMode := rmPrepareRenderSelf
    else
    begin
      Assert(prRenderClones in Options);
      Renderer.RenderMode := rmPrepareRenderClones;
    end;

    ReceivedGlobalLights := Params.GlobalLights as TLightInstancesList;

    { We need some non-nil TRenderingCamera instance to be able
      to render with lights. }
    DummyCamera := TRenderingCamera.Create;
    try
      { Set camera vectors to be anything sensible.
        Otherwise (if we pass zero matrix) opening a scene with shadow maps makes a warning
        that camera matrix is all 0,
        and cannot be inverted, since
        TTextureCoordinateRenderer.RenderCoordinateBegin does
        RenderingCamera.InverseMatrixNeeded.
        Testcase: silhouette. }
      DummyCamera.FromViewVectors(DefaultX3DCameraView, TMatrix4.Identity);

      Renderer.RenderBegin(ReceivedGlobalLights, DummyCamera, nil, 0, 0, 0,
        @DummyStatistics);

      for Shape in ShapeList do
      begin
        TGLShape(Shape).Fog := ShapeFog(Shape, Params.GlobalFog as TFogNode);
        Renderer.RenderShape(TGLShape(Shape), RenderOptions,
          { Pass sensible SceneTransform parameter below,
            so that TShader.EnableClipPlane will not raise an exception.
            PlaneTransform(Plane, SceneModelView) should not fail,
            so matrix should be sensible for homegeneous coordinate transformation
            (so identity is OK, zero is not OK). }
          TMatrix4.Identity,
          false, drFull);
      end;

      Renderer.RenderEnd;
    finally FreeAndNil(DummyCamera) end;

    Renderer.RenderMode := rmRender; // restore Renderer.RenderMode
  end;

var
  I: Integer;
  PossiblyTimeConsuming: Boolean;
  TimeStart: TCastleProfilerTime;
begin
  inherited;

  { Use InternalDirty to prevent trying to render this scene while it has
    some resources not ready.

    Right now, there's actually nothing that could cause such rendering.
    In the past, we experimented with initializing+updating progress bar
    inside the PrepareResources (e.g. when preparing a shape requires
    prepararing a texture, but the texture URL is online so we need
    to display download progress of the texture).
    The progress started by taking a screenshot of the current window,
    so it caused rendering.

    This is not relevant now. No callback is caused now by preparing
    and rendering, so nothing should cause a render in the middle of preparation.
    But let the check InternalDirty remain, for future security. }
  if InternalDirty <> 0 then Exit;

  if not ApplicationProperties.IsGLContextOpen then
  begin
    WritelnLog('PrepareResources', 'Rendering context not available, skipping preparing TCastleScene rendering resources for "%s"', [
      UriDisplay(URL)
    ]);
    Exit;
  end;

  Inc(InternalDirty);
  try
    PossiblyTimeConsuming := (not PreparedShapesResources) or (not PreparedRender);

    if PossiblyTimeConsuming then
      TimeStart := Profiler.Start('Prepare Scene Resources ' + URL);

    Assert(Params <> nil);
    Renderer := Params.RendererToPrepareShapes as TRenderer;
    Assert(Renderer <> nil);

    if not PreparedShapesResources then
    begin
      { Use PreparedShapesResources to avoid expensive (for large scenes)
        iteration over all shapes in every TCastleScene.PrepareResources call. }
      PreparedShapesResources := true;
      PrepareShapesResources;
    end;

    if ([prRenderSelf, prRenderClones] * Options <> []) and not PreparedRender then
    begin
      { We use PreparedRender to avoid potentially expensive iteration
        over shapes and expensive Renderer.RenderBegin/End. }
      PreparedRender := true;
      PrepareRenderShapes;
    end;

    if prBackground in Options then
      PrepareBackground;

    if prScreenEffects in Options then
    begin
      for I := 0 to ScreenEffectNodes.Count - 1 do
        TScreenEffectResources.Prepare(RenderOptions,
          ScreenEffectNodes[I] as TScreenEffectNode);
    end;

    if PossiblyTimeConsuming then
      Profiler.Stop(TimeStart);
  finally Dec(InternalDirty) end;
end;

procedure TCastleScene.LocalRenderOutside(
  const TestShapeVisibility: TTestShapeVisibility;
  const Params: TRenderParams);

  procedure RenderNormal;
  begin
    LocalRenderInside(TestShapeVisibility, Params);
  end;

  { Render, doing some special tricks when rendering to shadow maps. }
  procedure RenderWithShadowMaps;
  var
    SavedMode: TRenderingMode;
  begin
    { For shadow maps, speed up rendering by using only features that affect
      depth output.  }
    if Params.RenderingCamera.Target in [rtVarianceShadowMap, rtShadowMap] then
    begin
      { This save/restore of RenderOptions around RenderWithWireframeEffect
        works, because rmDepth is actually applied at "collection time"
        (not at real rendering time, that happens in TShapesRenderer later). }
      SavedMode := RenderOptions.Mode;
      RenderOptions.Mode := rmDepth;

      RenderNormal;

      RenderOptions.Mode := SavedMode;
    end else
    begin
      RenderNormal;
    end;
  end;

begin
  { This is usually called by LocalRender(Params) that probably
    already did tests below. But it may also be called directly,
    so do the checks below anyway. (The checks are trivial, so no speed harm.) }
  if CheckVisible and
     (InternalDirty = 0) then
  begin
    { I used to make here more complex "prepare" mechanism, that was trying
      to prepare for particular shapes only right before they are rendered
      (so instead of calling PrepareResources below, I was calling PrepareShape
      at the beginning of each RenderShape and such).

      After a while, it turns out this was a useless complication of code
      logic. There are many things that *have* to be prepared before whole
      rendering, for example
      - Occlusion query id must be generated (as we may start occlusion query
        before actually rendering the shape).

      It's much simpler to just call PrepareResources at the beginning.
      The PrepareResources is already optimized to do nothing,
      if everything is ready. }
    FTempPrepareParams.GlobalLights := Params.GlobalLights;
    FTempPrepareParams.GlobalFog := Params.GlobalFog;
    Assert(Params.RendererToPrepareShapes <> nil);
    FTempPrepareParams.RendererToPrepareShapes := Params.RendererToPrepareShapes;
    {$warnings off} // calling deprecated, but for internal purpose -- do not warn
    PrepareResources([prRenderSelf], FTempPrepareParams);
    {$warnings on}

    RenderWithShadowMaps;
  end;
end;

procedure TCastleScene.BeforeNodesFree(const InternalChangedAll: boolean);
begin
  { Release all associations with OpenGL context before freeing the nodes.
    This means vrml nodes are still valid during GLRenderer unprepare
    calls.

    Although we don't really want to lose our connection with OpenGL
    context, in fact that's the only sensible thing to do now: since
    everything possibly changed, we have to unprepare all now.

    This is done before inherited, as inherited may clear Shapes tree
    (clearing per-shape information about referenced vbos etc.). }
  GLContextClose;

  inherited;
end;

function TCastleScene.HasColliderMesh: Boolean;
begin
  Result := true;
end;

procedure TCastleScene.ColliderMesh(const TriangleEvent: TTriangleEvent);
var
  ShapesList: TShapeList;
  I: Integer;
begin
  inherited ColliderMesh(TriangleEvent);

  ShapesList := Shapes.TraverseList(true);
    for I := 0 to ShapesList.Count - 1 do
      if ShapesList[I].Collidable then
        ShapesList[I].Triangulate(TriangleEvent, true);
end;

{ Shadow volumes ------------------------------------------------------------- }

function TCastleScene.EffectiveCastShadowVolumes: Boolean;
begin
  Result :=
    CheckVisible and
    CastShadows and
    { Do not render shadow volumes when rendering wireframe.
      Shadow volumes assume that object is closed (2-manifold),
      otherwise weird artifacts are visible. }
    (RenderOptions.WireframeEffect <> weWireframeOnly);
end;

function TCastleScene.EffectiveWholeSceneManifold: Boolean;
begin
  { Test RenderOptions.WholeSceneManifold first, as it's instant
    (if marked by user), while InternalDetectedWholeSceneManifold may trigger
    on-demand analysis of borders. }

  Result := RenderOptions.WholeSceneManifold or InternalDetectedWholeSceneManifold;

  { Remove the warning, as it would trigger often, in cases when it's acceptable.
    Assume that "user knows what (s)he's doing" when toggling manually
    RenderOptions.WholeSceneManifold.

    Testcases where InternalDetectedWholeSceneManifold=false but
    RenderOptions.WholeSceneManifold=true works OK:

    - examples/physics/physics_throw_chickens
    - examples/animations/split_long_animation
    - examples/creature_behaviors
    - examples/cpp_builder/window
    - https://github.com/castle-engine/conference-delphi-summit-2025/tree/master/walk_3d_game_controllers
  }

  {
  if RenderOptions.WholeSceneManifold and not InternalDetectedWholeSceneManifold then
  begin
    WritelnWarningOnce(DoneWarningWholeSceneManifold,
      'Rendering shadow volumes for shadow caster "%s" because forced by RenderOptions.WholeSceneManifold=true. ' +
      'But our detection showed that this scene is not really 2-manifold. ' +
      'Shadows artifacts are possible in this case. ' +
      'We advise to fix the 3D model to be really 2-manifold.', [
      Name
    ]);
  end;
  }
end;

procedure TCastleScene.LocalRenderShadowVolume(const Params: TRenderParams;
  const ShadowVolumeRenderer: TBaseShadowVolumeRenderer);

  function NiceName: String;
  begin
    Result := Name;
    if (Name = '') and
       (csTransient in ComponentStyle) and
       (Parent <> nil) then
      Result := 'child(' + Parent.Name + ')';
  end;

var
  SceneBox, ShapeBox: TBox3D;
  SVRenderer: TGLShadowVolumeRenderer;
  ShapeList: TShapeList;
  Shape: TShape;
  ShapeWorldTransform: TMatrix4;
  ForceOpaque: boolean;
begin
  { Call inherited to render shadow quads of children,
    in case one TCastleScene is a child of another.
    See https://forum.castle-engine.io/t/shadow-ignors-distanceculling/670/14 for testcase.

    Note that inherited also checks our own "CheckVisible and CastShadows",
    so they work recursively.
    Below EffectiveCastShadowVolumes also checks our own
    "CheckVisible and CastShadows". }
  inherited;

  if EffectiveCastShadowVolumes then
  begin
    SVRenderer := ShadowVolumeRenderer as TGLShadowVolumeRenderer;

    ForceOpaque := not (RenderOptions.Blending and (RenderOptions.Mode = rmFull));

    // DistanceCullingCheck* uses this value, and it may be called here
    RenderCameraPosition := Params.LocalCameraPosition;
    RenderUsingShadowVolumes := Params.UsingShadowVolumes;

    { calculate and check SceneBox }
    SceneBox := LocalBoundingBox.Transform(Params.Transformation^.Transform);
    if SVRenderer.GetCasterShadowPossiblyVisible(SceneBox) then
    begin
      { Do not render shadows for objects eliminated by DistanceCulling.
        This checks per-scene. }
      if (DistanceCulling > 0) and (not DistanceCullingCheckScene) then
        Exit;

      { Using below OnlyVisible=true,
        because shadows are cast only by visible scene parts. }
      ShapeList := Shapes.TraverseList({ OnlyActive } true, { OnlyVisible } true);
      for Shape in ShapeList do
      begin
        { Do not render shadows for shapes eliminated by DistanceCulling.

          Otherwise: Not only shadows for invisible objects would look weird,
          but they would actually show errors.
          Shadow volumes *assume* that shadow caster is also rendered (shadow quads
          are closed) if that shadow caster is visible in frustum.

          This is done per-shape when EffectiveWholeSceneManifold=false.
          When EffectiveWholeSceneManifold=true, we cannot do per-shape check:
          the whole scene should be rendered. }
        if not EffectiveWholeSceneManifold then
        begin
          if (DistanceCulling > 0) and (not DistanceCullingCheckShape(Shape)) then
            Continue;
        end;

        { Do not render shadows when frustum+light check says it is definitely
          not visible.

          This is done per-shape when EffectiveWholeSceneManifold=false.

          When EffectiveWholeSceneManifold=true, we render all shapes here.
          The per-scene check already passed above. }
        ShapeBox := Shape.BoundingBox.Transform(Params.Transformation^.Transform);
        SVRenderer.InitCaster(ShapeBox);
        if EffectiveWholeSceneManifold or
           SVRenderer.CasterShadowPossiblyVisible then
        begin
          ShapeWorldTransform := Params.Transformation^.Transform *
            Shape.State.Transformation.Transform;
          Shape.InternalShadowVolumes.RenderSilhouetteShadowVolume(
            Params,
            SVRenderer.Mesh,
            SVRenderer.LightPosition,
            ShapeWorldTransform,
            SVRenderer.ZFailAndLightCap,
            SVRenderer.ZFail,
            ForceOpaque,
            EffectiveWholeSceneManifold);
        end;
      end;
    end;
  end;
end;

function TCastleScene.ShapePossiblyVisible(Shape: TShape): boolean;

  function FrustumCullingCheck(Shape: TShape): Boolean;
  begin
    Result := FrustumForShapeCulling^.Box3DCollisionPossibleSimple(Shape.BoundingBox);

    // Alternative: sphere
    // Result := Shape.FrustumBoundingSphereCollisionPossibleSimple(FrustumForShapeCulling^);

    // Alternative: sphere and box
    // Result := Shape.FrustumBoundingSphereCollisionPossibleSimple(FrustumForShapeCulling^) and
    //   FrustumForShapeCulling^.Box3DCollisionPossibleSimple(Shape.BoundingBox);

    { We used to allow users to configure the check using FrustumCulling
      and OctreeFrustumCulling, but in the end this was a lot of effort
      and actually not useful. }
  end;

begin
  Result :=
    // frustum culling
    ( (not FShapeFrustumCulling) or FrustumCullingCheck(Shape) ) and
    // distance culling
    ( (DistanceCulling <= 0 ) or DistanceCullingCheckShape(Shape) );
end;

function TCastleScene.DistanceCullingCheckScene: Boolean;
var
  Box: TBox3D;
begin
  // This should be only called when DistanceCulling indicates this check is necessary
  Assert(DistanceCulling > 0);
  Box := LocalBoundingBoxNoChildren;
  Result :=
    (not Box.IsEmpty) and
    (Box.PointDistanceSqr(RenderCameraPosition) <=
     Sqr(DistanceCulling));
end;

function TCastleScene.DistanceCullingCheckShape(const Shape: TShape): boolean;
begin
  { When this is shadow caster for shadow volumes and this has
    EffectiveWholeSceneManifold, we have to render whole scene, or nothing.

    Shadow volumes work correctly only if shadow caster (at least the part of it
    in frustum, that affects the screen) is also rendered.

    So distance culling cannot eliminate particular shapes.

    Testcase that this check is necessary: Open
    examples/viewport_and_scenes/shadows_distance_culling/ (in editor is OK),
    look at one of the houses with multiple materials, get closer/further from it.
    Without this check, if *part* of shapes (but not all) would be eliminated
    by distance culling, weird shadow artifacts would be rendered.

    We take care to only do this (exit early, eliminating per-shape
    DistanceCulling check)
    when really necessary, so only when actually using shadow volumes light source
    (RenderUsingShadowVolumes). And we check EffectiveWholeSceneManifold
    at the end, as it potentially causes CalculateDetectedWholeSceneManifold,
    which may on-demand do some calculation. }
  if RenderUsingShadowVolumes and
     EffectiveCastShadowVolumes and
     EffectiveWholeSceneManifold then
    Exit(true);

  // This should be only called when DistanceCulling indicates this check is necessary
  Assert(DistanceCulling > 0);
  Result :=
    (PointsDistanceSqr(Shape.BoundingSphereCenter, RenderCameraPosition) <=
     Sqr(DistanceCulling + Shape.BoundingSphereRadius))
end;

procedure TCastleScene.SetShapeFrustumCulling(const Value: Boolean);
begin
  if FShapeFrustumCulling <> Value then
    FShapeFrustumCulling := Value;
end;

procedure TCastleScene.SetDistanceCulling(const Value: Single);
begin
  if FDistanceCulling <> Value then
    FDistanceCulling := Value;
end;

{ Render --------------------------------------------------------------------- }

function TCastleScene.RenderFrustumOctree_TestShape(
  Shape: TShape): boolean;
begin
  { We know that all shapes passed here are TGLShape, so we can cast }
  Result := TGLShape(Shape).PassedFrustumAndDistanceCulling;
end;

procedure TCastleScene.RenderWithOctree_CheckShapeCulling(
  ShapeIndex: Integer; CollidesForSure: boolean);
var
  Shape: TGLShape;
begin
  Shape := TGLShape(InternalOctreeRendering.ShapesList[ShapeIndex]);

  if not Shape.PassedFrustumAndDistanceCulling then
  begin
    if CollidesForSure then
      // frustum culling already passed, but still check distance culling
      Shape.PassedFrustumAndDistanceCulling := (DistanceCulling <= 0) or DistanceCullingCheckShape(Shape)
    else
      // this function performs frustum culling and distance culling too
      Shape.PassedFrustumAndDistanceCulling := ShapePossiblyVisible(Shape);
  end;
end;

procedure TCastleScene.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  { Update generated texture for this shape.

    The given camera position, direction, up should be in world space
    (that is, in TCastleRootTransform space,
    not in space local to this TCastleScene).
    These camera vectors are used to update TRenderedTextureNode, if any.

    This does not change current viewport or projection matrix. }
  procedure UpdateOneGeneratedTexture(const Shape: TX3DRendererShape;
    const TextureNode: TAbstractTextureNode;
    const Render: TRenderFromViewFunction;
    const ProjectionNear, ProjectionFar: Single;
    const CurrentViewpoint: TAbstractViewpointNode;
    const CameraViewKnown: boolean;
    const CameraView: TViewVectors);

    procedure UpdateGeneratedCubeMap(const TexNode: TGeneratedCubeMapTextureNode);
    var
      TextureRes: TGeneratedCubeMapTextureResource;
    begin
      if TexNode.GenTexFunctionality.NeedsUpdate then
      begin
        { Shape.BoundingBox must be non-empty, otherwise we don't know from what
          3D point to capture environment.

          Note: check Shape.BoundingBox only after CheckUpdate passed.
          This is more optimal, as Shape.BoundingBox may need to iterate over mesh.
          Testcase: examples/mobile/simple_3d_demo/gameinitialize.pas with "toggle cubemap updates" = "off",
          look at how much UpdateOneGeneratedTexture is eating. Should be 0% if off. }
        if Shape.BoundingBox.IsEmpty then Exit;

        TextureRes := TGeneratedCubeMapTextureResource(TTextureResources.Get(TexNode));
        if TextureRes <> nil then
        begin
          TextureRes.Update(Render, ProjectionNear, ProjectionFar,
            Shape.BoundingBox.Center + TexNode.FdBias.Value);

          TexNode.GenTexFunctionality.PostUpdate;

          if LogRenderer then
            WritelnLog('CubeMap', TexNode.NiceName + ' texture regenerated');
        end;
      end;
    end;

    procedure UpdateGeneratedShadowMap(TexNode: TGeneratedShadowMapNode);
    var
      TextureRes: TGeneratedShadowMapResource;
    begin
      if TexNode.GenTexFunctionality.NeedsUpdate then
      begin
        if TexNode.FdLight.Value is TAbstractPunctualLightNode then
        begin
          TextureRes := TGeneratedShadowMapResource(TTextureResources.Get(TexNode));
          if TextureRes <> nil then
          begin
            TextureRes.Update(Render, ProjectionNear, ProjectionFar,
              TAbstractPunctualLightNode(TexNode.FdLight.Value));

            TexNode.GenTexFunctionality.PostUpdate;

            if LogRenderer then
              WritelnLog('GeneratedShadowMap', TexNode.NiceName + ' texture regenerated');
          end;
        end else
          WritelnWarning('VRML/X3D', TexNode.NiceName + ' needs updating, but light = NULL or incorrect');
      end;
    end;

    procedure UpdateRenderedTexture(TexNode: TRenderedTextureNode);
    var
      TextureRes: TRenderedTextureResource;
    begin
      if TexNode.GenTexFunctionality.NeedsUpdate then
      begin
        TextureRes := TRenderedTextureResource(TTextureResources.Get(TexNode));
        if TextureRes <> nil then
        begin
          TextureRes.Update(Render, ProjectionNear, ProjectionFar, CurrentViewpoint,
            CameraViewKnown, CameraView, Shape);

          TexNode.GenTexFunctionality.PostUpdate;

          if LogRenderer then
            WritelnLog('RenderedTexture', TexNode.NiceName + ' texture regenerated');
        end;
      end;
    end;

  begin
    if TextureNode is TGeneratedCubeMapTextureNode then
      UpdateGeneratedCubeMap(TGeneratedCubeMapTextureNode(TextureNode))
    else
    if TextureNode is TGeneratedShadowMapNode then
      UpdateGeneratedShadowMap(TGeneratedShadowMapNode(TextureNode))
    else
    if TextureNode is TRenderedTextureNode then
      UpdateRenderedTexture(TRenderedTextureNode(TextureNode));
  end;

  { Update generated textures, like generated cubemaps/shadow maps. }
  procedure UpdateGeneratedTextures(
    const RenderFunc: TRenderFromViewFunction;
    const ProjectionNear, ProjectionFar: Single);
  var
    I: Integer;
    Shape: TGLShape;
    TextureNode: TAbstractTextureNode;
    GenTexFunctionality: TGeneratedTextureFunctionality;
    CamView: TViewVectors;
  begin
    if GeneratedTextures.Count = 0 then
      Exit; // optimize away common case

    FrameProfiler.Start(fmUpdateGeneratedTextures);

    { Avoid doing this two times within the same FrameId.
      Important if
      - the same scene is present multiple times in one viewport,
      - or when Viewport.Items are shared across multiple viewports
        (thus scene is present in multiple viewports). }
    if UpdateGeneratedTexturesFrameId = TFramesPerSecond.FrameId then
      Exit;
    UpdateGeneratedTexturesFrameId := TFramesPerSecond.FrameId;

    if World.MainCamera <> nil then
    begin
      CamView := World.MainCamera.WorldView;
    end else
    begin
      CamView := DefaultX3DCameraView;
    end;

    for I := 0 to GeneratedTextures.Count - 1 do
    begin
      Shape := TGLShape(GeneratedTextures.L[I].Shape);
      TextureNode := GeneratedTextures.L[I].TextureNode;
      GenTexFunctionality := GeneratedTextures.L[I].Functionality;

      { update GenTexFunctionality.InternalUpdateNeeded }
      if TextureNode is TGeneratedShadowMapNode then
      begin
        { For TGeneratedShadowMapNode, only geometry change requires to regenerate it. }
        if GenTexFunctionality.InternalLastStateId < World.InternalVisibleGeometryStateId then
        begin
          GenTexFunctionality.InternalLastStateId := World.InternalVisibleGeometryStateId;
          GenTexFunctionality.InternalUpdateNeeded := true;
        end;
      end else
      begin
        { For TRenderedTextureNode, TGeneratedCubeMapTextureNode etc.
          any visible change indicates to regenerate it. }
        if GenTexFunctionality.InternalLastStateId < World.InternalVisibleStateId then
        begin
          GenTexFunctionality.InternalLastStateId := World.InternalVisibleStateId;
          GenTexFunctionality.InternalUpdateNeeded := true;
        end;
      end;

      if TextureNode is TGeneratedCubeMapTextureNode then
        AvoidShapeRendering := Shape else
      if TextureNode is TGeneratedShadowMapNode then
        AvoidNonShadowCasterRendering := true;

      UpdateOneGeneratedTexture(Shape, TextureNode,
        RenderFunc, ProjectionNear, ProjectionFar,
        ViewpointStack.Top,
        World.MainCamera <> nil, CamView);

      AvoidShapeRendering := nil;
      AvoidNonShadowCasterRendering := false;
    end;

    FrameProfiler.Stop(fmUpdateGeneratedTextures);
  end;

begin
  inherited;

  { This will do FrameProfiler.Start/Stop with fmUpdateGeneratedTextures }
  if World <> nil then
    UpdateGeneratedTextures(
      World.InternalRenderEverythingEvent,
      World.InternalProjectionNear,
      World.InternalProjectionFar);
end;

procedure TCastleScene.ResetShapeVisible(const Shape: TShape);
begin
  TGLShape(Shape).PassedFrustumAndDistanceCulling := false;
end;

procedure TCastleScene.LocalRender(const Params: TRenderParams);

{ Call LocalRenderOutside, choosing TTestShapeVisibility function
  suitable for our Params.Frustum, octrees and some settings.

  If InternalOctreeRendering is initialized (so be sure to include
  ssRendering in @link(Spatial)), this octree will be used to quickly
  find visible Shapes. Otherwise, we will just enumerate all
  Shapes (which may be slower if you really have a lot of Shapes). }

  procedure TestOctreeWithFrustum(Octree: TShapeOctree);
  begin
    Shapes.Traverse({$ifdef FPC}@{$endif}ResetShapeVisible, false, true);
    Octree.EnumerateCollidingOctreeItems(Params.Frustum^,
      {$ifdef FPC}@{$endif}RenderWithOctree_CheckShapeCulling);
  end;

begin
  inherited;

  if InternalEnableRendering and
     CheckVisible and
     (InternalDirty = 0) then
  begin
    FrameProfiler.Start(fmRenderScene);

    if Params.InternalPass = 0 then
      Inc(Params.Statistics.ScenesVisible);

    if FSceneFrustumCulling and
       (Params.Frustum <> nil) and
       (not Params.Frustum^.Box3DCollisionPossibleSimple(LocalBoundingBox)) then
    begin
      FrameProfiler.Stop(fmRenderScene);
      Exit;
    end;

    // RenderCameraPosition is used by DistanceCullingCheck* below
    RenderCameraPosition := Params.LocalCameraPosition;
    RenderUsingShadowVolumes := Params.UsingShadowVolumes;

    { Do distance culling for whole scene.
      When EffectiveWholeSceneManifold=true, this is the only place where
      we check distance culling, we cannot do per-shape distance culling then. }
    if (DistanceCulling > 0) and (not DistanceCullingCheckScene) then
    begin
      FrameProfiler.Stop(fmRenderScene);
      Exit;
    end;

    if Params.InternalPass = 0 then
      Inc(Params.Statistics.ScenesRendered);

    FrustumForShapeCulling := Params.Frustum;

    if Assigned(InternalVisibilityTest) then
      LocalRenderOutside(InternalVisibilityTest, Params)
    else
    if Params.Frustum = nil then
      LocalRenderOutside(nil, Params)
    else
    if (InternalOctreeRendering <> nil) and
       ShapeFrustumCulling then
    begin
      { Check above ShapeFrustumCulling, since the InternalOctreeRendering
        does per-shape frustum culling automatically, even before
        ShapeCullingOctreeFunc test. Thanks to octree, many shapes
        don't even reach the stage when ShapeCullingOctreeFunc could be called. }
      TestOctreeWithFrustum(InternalOctreeRendering);
      LocalRenderOutside({$ifdef FPC}@{$endif} RenderFrustumOctree_TestShape, Params);
    end else
      LocalRenderOutside({$ifdef FPC}@{$endif} ShapePossiblyVisible, Params);

    FrameProfiler.Stop(fmRenderScene);
  end;
end;

{ Background-related things -------------------------------------------------- }

procedure TCastleScene.InternalInvalidateBackgroundRenderer;
begin
  FreeAndNil(FBackgroundRenderer);
  FBackgroundNode := nil;
  FBackgroundRendererValid := false;
end;

procedure TCastleScene.PrepareBackground;
{ Always after PrepareBackground => FBackgroundRendererValid = true }
begin
  if FBackgroundRendererValid and (BackgroundStack.Top = FBackgroundNode) then
    Exit;

  { Background is created, but not suitable for current
    BackgroundStack.Top. So destroy it. }
  if FBackgroundRendererValid then
    InternalInvalidateBackgroundRenderer;

  if BackgroundStack.Top <> nil then
    FBackgroundRenderer := CreateBackgroundRenderer(BackgroundStack.Top)
  else
    FBackgroundRenderer := nil;

  FBackgroundNode := BackgroundStack.Top;
  FBackgroundRendererValid := true;
end;

function TCastleScene.InternalBackgroundRenderer: TBackgroundRenderer;
var
  BackgroundNode: TAbstractBackgroundNode;
begin
  PrepareBackground;
  Result := FBackgroundRenderer;

  { If background transform changed, we have to update the FBackgroundRenderer
    scene. Note that we check Result <> nil always, since not every
    TAbstractBackgroundNode may be supported. }
  BackgroundNode := BackgroundStack.Top;
  if (BackgroundNode <> nil) and (Result <> nil) then
    Result.UpdateRotation(BackgroundNode.TransformRotation);
end;

function TCastleScene.Attributes: TCastleRenderOptions;
begin
  Result := RenderOptions;
end;

procedure TCastleScene.InternalCameraChanged;
var
  I: Integer;
begin
  inherited;
  for I := 0 to GeneratedTextures.Count - 1 do
    if GeneratedTextures.L[I].TextureNode is TRenderedTextureNode then
      { Camera change causes regenerate of RenderedTexture,
        as RenderedTexture with viewpoint = NULL uses current camera.
        See demo_models/rendered_texture/rendered_texture_no_headlight.x3dv
        testcase. }
      GeneratedTextures.L[I].Functionality.InternalUpdateNeeded := true;
end;

function TCastleScene.InternalScreenEffectsCount: Integer;
var
  I: Integer;
  SE: TScreenEffectNode;
begin
  Result := 0;

  { This ties our scene to OpenGL,
    so we must be notified when OpenGL is closed.
    Testcase: otherwise the noise1 texture of the screen effect in
    "The Unholy Society" is not released from OpenGL, we get warning from
    TextureMemoryProfiler. }
  RegisterGLContextClose;

  for I := 0 to ScreenEffectNodes.Count - 1 do
  begin
    SE := TScreenEffectNode(ScreenEffectNodes[I]);
    // Note: TScreenEffectResources.Prepare exits fast if already was done on this shape
    TScreenEffectResources.Prepare(RenderOptions, SE);
    if TScreenEffectResources.Get(SE).ShaderProgram <> nil then
      Inc(Result);
  end;
end;

function TCastleScene.InternalScreenEffects(Index: Integer): TGLSLProgram;
var
  I: Integer;
  SeNode: TScreenEffectNode;
  SeNodeRes: TScreenEffectResource;
begin
  { No need for TScreenEffectResources.Prepare here, ScreenEffectsCount (that does
    TScreenEffectResources.Prepare) is always called first, otherwise the caller
    would not know that this Index is valid. }

  for I := 0 to ScreenEffectNodes.Count - 1 do
  begin
    SeNode := TScreenEffectNode(ScreenEffectNodes[I]);
    SeNodeRes := TScreenEffectResources.Get(SeNode);
    if SeNodeRes.ShaderProgram <> nil then
      if Index = 0 then
        Exit(TGLSLProgram(SeNodeRes.ShaderProgram))
      else
        Dec(Index);
  end;

  raise EInternalError.Create('TCastleScene.ScreenEffects: Invalid index');
end;

function TCastleScene.InternalScreenEffectsNeedDepth: boolean;
var
  I: Integer;
  SeNode: TScreenEffectNode;
  SeNodeRes: TScreenEffectResource;
begin
  { For now: No need for TScreenEffectResources.Prepare here, ScreenEffectsCount
    is always called first. But actually for some scenarios we should do
    here TScreenEffectResources.Prepare? }

  for I := 0 to ScreenEffectNodes.Count - 1 do
  begin
    SeNode := TScreenEffectNode(ScreenEffectNodes[I]);
    SeNodeRes := TScreenEffectResources.Get(SeNode);
    if (SeNodeRes.ShaderProgram <> nil) and
        SeNode.NeedsDepth then
      Exit(true);
  end;
  Exit(false);
end;

procedure TCastleScene.FreeResources(Resources: TSceneFreeResources);
begin
  inherited;

  if (frBackgroundImageInNodes in Resources) and
     (FBackgroundRenderer <> nil) then
    FBackgroundRenderer.FreeResources;
end;

function TCastleScene.Clone(const AOwner: TComponent): TCastleScene;
begin
  Result := (inherited Clone(AOwner)) as TCastleScene;
end;

procedure TCastleScene.ChangeWorld(const Value: TCastleAbstractRootTransform);
begin
  if World <> Value then
  begin
    if World <> nil then
    begin
      if CastGlobalLights then
        (World as TCastleRootTransform).UnregisterCastGlobalLights(Self);
    end;

    inherited;

    if World <> nil then
    begin
      if CastGlobalLights then
        (World as TCastleRootTransform).RegisterCastGlobalLights(Self);
    end;
  end else
  begin
    inherited;
  end;
end;

procedure TCastleScene.SetCastGlobalLights(const Value: Boolean);
begin
  if FCastGlobalLights <> Value then
  begin
    FCastGlobalLights := Value;
    if World <> nil then
    begin
      if Value then
        (World as TCastleRootTransform).RegisterCastGlobalLights(Self)
      else
        (World as TCastleRootTransform).UnregisterCastGlobalLights(Self);
    end;
  end;
end;

function TCastleScene.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'RenderOptions') or
     (PropertyName = 'CastGlobalLights') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleScene.WasVisible: Boolean;
begin
  { Note: FWasVisibleFrameId is 0 by default,
    and TFramesPerSecond.RenderFrameId is always >= 1,
    which means that this will return (correctly) false at the beginning before any rendering run. }
  Result := TFramesPerSecond.RenderFrameId = FWasVisibleFrameId;
end;

procedure TCastleScene.InternalSchedulePrepareResources;
begin
  PreparedShapesResources := false;
end;

procedure TCastleScene.SetEffects(const Value: array of TEffectNode);

  { Check if RootNode.FdChildren, limited to TEffectNode instances,
    match the Value array. }
  function EffectsAlreadySet: Boolean;
  var
    I: Integer;
    IndexOfValue: Integer;
  begin
    IndexOfValue := 0; // looking for Value[IndexOfValue] now
    for I := 0 to RootNode.FdChildren.Count - 1 do
      if RootNode.FdChildren[I] is TEffectNode then
      begin
        if IndexOfValue >= Length(Value) then
          Exit(false); // we have more effect nodes than in Value

        if RootNode.FdChildren[I] <> Value[IndexOfValue] then
          Exit(false); // effect node does not match

        Inc(IndexOfValue);
      end;
    Result := IndexOfValue = Length(Value);
  end;

  { Insert Value array at the beginning of RootNode.FdChildren. }
  procedure AddEffects(out IndexBeyondAdded: Integer);
  var
    I: Integer;
  begin
    IndexBeyondAdded := Length(Value);
    if IndexBeyondAdded = 0 then Exit; // nothing to add

    for I := 0 to IndexBeyondAdded - 1 do
      RootNode.FdChildren.Add(I, Value[I]);
  end;

  { Remove all TEffectNode instances from RootNode.FdChildren,
    except those that are in Value array. }
  procedure RemoveUnwantedEffects(const IndexBeyondAdded: Integer; out RemovedSomething: Boolean);
  var
    I: Integer;
  begin
    RemovedSomething := false;
    I := IndexBeyondAdded;
    while I < RootNode.FdChildren.Count do
      if RootNode.FdChildren[I] is TEffectNode then
      begin
        { This effect node is not in the Value array, so remove it. }
        RootNode.FdChildren.Delete(I);
        RemovedSomething := true;
      end else
      begin
        { This is not an effect node, so just skip it. }
        Inc(I);
      end;
  end;

var
  IndexBeyondAdded: Integer;
  RemovedSomething: Boolean;
begin
  if RootNode = nil then
    raise Exception.Create('Adding effects requires RootNode to be set, which means you should load the scene model, setting TCastleScene.Url or calling TCastleScene.Load');

  { We want to modify RootNode.FdChildren such that it contains only
    the TEffectNode instances from Value (and only in the specified order,
    because the order matters for rendering).

    First we check do we need to do anything, and if yes -- first we just
    add all nodes, then remove the rest (this makes sure we will not free
    some effect node in the middle of work due to refcount dropping to 0). }

  if not EffectsAlreadySet then
  begin
    AddEffects(IndexBeyondAdded);
    RemoveUnwantedEffects(IndexBeyondAdded, RemovedSomething);

    { Call the most optimal (none, if possible!) InternalChangedField variant. }
    if (IndexBeyondAdded <> 0) or // added something
       RemovedSomething then
    begin
      // WritelnLog('SetEffects modified the scene, adding %d effect nodes, removing old: %s', [
      //   IndexBeyondAdded,
      //   BoolToStr(RemovedSomething, true)
      // ]);
      if RemovedSomething then
        InternalChangedField(RootNode.FdChildren)
      else
        InternalChangedField(RootNode.FdChildren, chGroupChildrenAdd);
    end;
  end;
end;

{ TBasicRenderParams --------------------------------------------------------- }

constructor TBasicRenderParams.Create;
begin
  inherited;
  FGlobalLights := TLightInstancesList.Create;
end;

destructor TBasicRenderParams.Destroy;
begin
  FreeAndNil(FGlobalLights);
  inherited;
end;

function TBasicRenderParams.GlobalLights: TAbstractLightInstancesList;
begin
  Result := FGlobalLights;
end;

initialization
  RegisterSerializableComponent(TCastleScene, 'Scene');
  RegisterSerializableComponent(TCastleBox, 'Box');
  RegisterSerializableComponent(TCastleSphere, 'Sphere');
  RegisterSerializableComponent(TCastlePlane, 'Plane');
  RegisterSerializableComponent(TCastleText, 'Text');
  RegisterSerializableComponent(TCastleCone, 'Cone');
  RegisterSerializableComponent(TCastleCylinder, 'Cylinder');
  RegisterSerializableComponent(TCastleImageTransform, 'Image');
  RegisterSerializableComponent(TCastleBackground, 'Background');
  RegisterSerializableComponent(TCastleFog, 'Fog');
  RegisterSerializableComponent(TCastlePointLight, ['Light', 'Point']);
  RegisterSerializableComponent(TCastleDirectionalLight, ['Light', 'Directional']);
  RegisterSerializableComponent(TCastleSpotLight, ['Light', 'Spot']);
  {$ifdef CASTLE_EXPERIMENTAL_ENVIRONMENT_LIGHT}
  RegisterSerializableComponent(TCastleEnvironmentLight, ['Light', 'Environment']);
  {$endif}
end.
