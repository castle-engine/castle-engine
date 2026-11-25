{
  Copyright 2001-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering context state. }
unit CastleRenderContext;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections, Classes,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleUtils, CastleVectors, CastleRectangles, CastleGLShaders, CastleColors,
  CastleRenderOptions, CastleGLUtils, CastleInternalRHI;

type
  TClearBuffer = (cbColor, cbDepth, cbStencil);

  TClearBuffers = set of TClearBuffer;

  { Scissor to clip displayed things, in addition to the global scissor
    affected by TRenderContext.ScissorEnable / TRenderContext.ScissorDisable.
    Always disable an enabled scissor (destructor does it automatically).

    Make sure to enable and disable the scissor when the same OpenGL context
    is current. In practice, the only reliable way to do this is to enable
    and then disable the scissor within the same OnRender event. }
  TScissor = class
  strict private
    procedure SetEnabled(const Value: boolean);
  private
    FEnabled: boolean;
  public
    { Rectangle to which we clip rendering. Empty by default (will clip everything,
      if you don't assign this!). Do not change this when scissor is enabled. }
    Rect: TRectangle;
    constructor Create;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

  { Possible values of @link(TRenderContext.DepthFunc).
    Note: For now, the values of this enum correspond to OpenGL(ES) constants,
    but do not depend on this outside (and it may change in the future).
    Treat it like opaque enum. }
  TDepthFunction = (
    dfNever = $0200,
    dfLess = $0201,
    dfEqual = $0202,
    dfLessEqual = $0203,
    dfGreater = $0204,
    dfNotEqual = $0205,
    dfGreaterEqual = $0206,
    dfAlways = $0207
  );

  TPolygonOffset = record
    Enabled: Boolean;
    Scale, Bias: Single;
  end;

  { Target of bound buffer, see https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBindBuffer.xhtml . }
  TBufferTarget = (
    { Vertex attributes. }
    btArray,
    { Indexes. }
    btElementArray
  );

  { The OpenGL / OpenGLES context state.
    We try hard to make this a @bold(very, very) small class,
    because usually it's better to introduce a clean higher-level API
    than to track the OpenGL context state in a simple global @link(RenderContext)
    instance.

    Use the methods and properties of this class
    only when this context is @italic(current),
    which means it's set as @link(RenderContext) value.

    Do not depend on the context state being persistent.
    The @link(RenderContext) does not change during a single TCastleUserInterface.OnRender
    method (with all 2D and 3D stuff rendered inside),
    but that's all we guarantee. On desktops, you control the context
    creation / destruction explicitly (by opening / closing the TCastleWindow).
    But on mobile devices -- the context may get destroyed and created at almost
    any moment. So do not use the instance of @link(RenderContext) to store
    anything you rely on being stored. Instead, use your own variables for this,
    and only synchronize @link(RenderContext) with your variables.
  }
  TRenderContext = class(TComponent)
  strict private
    type
      TScissorList = class({$ifdef FPC}specialize{$endif} TObjectList<TScissor>)
      public
        FFinalScissor: TRectangle; // only defined when Count <> 0
        ParentContext: TRenderContext;
        procedure Update;
      end;
    var
      FClearColor: TCastleColor;
      FLineWidth, FPointSize: Single;
      FGlobalAmbient: TCastleColorRGB;
      FGlobalScissor: TScissor;
      FProjectionMatrix: TMatrix4;
      FDepthRange: TDepthRange;
      FCullFace, FFrontFaceCcw: boolean;
      FDepthTest: Boolean;
      FDepthFunc: TDepthFunction;
      FColorChannels: TColorChannels;
      FDepthBufferUpdate: Boolean;
      FViewport: TRectangle;
      FViewportDelta: TVector2Integer;
      WarningViewportTooLargeDone: Boolean;
      FCurrentProgram: TGLSLProgram;
      FCurrentVao: TVertexArrayObject;
      FBlendingSourceFactor: TBlendingSourceFactor;
      FBlendingDestinationFactor: TBlendingDestinationFactor;
      FBlendingEnabled: Boolean;
      FFixedFunctionAlphaTest: Boolean;
      FFixedFunctionAlphaCutoff: Single;
      FFixedFunctionLighting: boolean;
      FLineType: TLineType;
      FPolygonOffset: TPolygonOffset;
      FBoundBuffer: array [TBufferTarget] of TGLBuffer;
      FInternalControlRectIgnore: Cardinal;
      FRHIClearCount: Cardinal;
      FRHILastClearBuffers: TClearBuffers;
      FRHILastClearColor: TCastleColor;
      FRHIViewportUpdateCount: Cardinal;
      FRHILastViewport: TFloatRectangle;
      FRHILastMinDepth: Single;
      FRHILastMaxDepth: Single;
      FRHIScissorUpdateCount: Cardinal;
      FRHILastScissor: TRectangle;
      FRHIScissorActive: Boolean;
      FRHIDepthBiasUpdateCount: Cardinal;
      FRHILastLineWidth: Single;
      FRHILineWidthUpdateCount: Cardinal;
      FRHILastLineType: TLineType;
      FRHILastLineStipplePattern: Word;
      FRHILineTypeUpdateCount: Cardinal;
      FBlendingConstantColor: TCastleColor;
      FRHILastBlendConstantColor: TCastleColor;
      FRHIBlendConstantColorUpdateCount: Cardinal;
      FStencilTest: Boolean;
      FStencilCompareOp: TRHICompareOp;
      FStencilReference: Byte;
      FStencilCompareMask: Byte;
      FStencilWriteMask: Byte;
      FStencilFailOp: TRHIStencilOp;
      FStencilDepthFailOp: TRHIStencilOp;
      FStencilPassOp: TRHIStencilOp;

    procedure SetLineWidth(const Value: Single);
    procedure SetBlendingConstantColor(const Value: TCastleColor);
    procedure SetStencilTest(const Value: Boolean);
    procedure SetPointSize(const Value: Single);
    procedure SetGlobalAmbient(const Value: TCastleColorRGB);
    procedure WarnContextNotCurrent;
    procedure SetProjectionMatrix(const Value: TMatrix4);
    procedure SetDepthRange(const Value: TDepthRange);
    procedure SetCullFace(const Value: boolean);
    procedure SetFrontFaceCcw(const Value: boolean);
    procedure SetDepthTest(const Value: Boolean);
    procedure SetDepthFunc(const Value: TDepthFunction);
    function GetColorMask: boolean;
    procedure SetColorMask(const Value: boolean);
    procedure SetColorChannels(const Value: TColorChannels);
    procedure SetDepthBufferUpdate(const Value: boolean);
    procedure SetViewport(const Value: TRectangle);
    procedure SetViewportDelta(const Value: TVector2Integer);
    procedure UpdateViewport;
    procedure WarningViewportTooLarge;
    procedure SetCurrentProgram(const Value: TGLSLProgram);
    procedure SetCurrentVao(const Value: TVertexArrayObject);
    procedure SetFixedFunctionLighting(const Value: boolean);
    procedure SetLineType(const Value: TLineType);
    procedure SetPolygonOffset(const Value: TPolygonOffset);
    function GetBoundBuffer(const Target: TBufferTarget): TGLBuffer;
    procedure SetBoundBuffer(const Target: TBufferTarget; const Value: TGLBuffer);
    procedure SetInternalControlRectIgnore(const Value: Cardinal);
    procedure GetEffectiveDepthRange(out AMinDepth, AMaxDepth: Single);

    { Update OpenGL(ES) state to match the fields of this class.
      Used by SetXxx methods and by SynchronizeState. }
    procedure UpdateClearColor;
    procedure UpdateLineWidth;
    procedure UpdatePointSize;
    procedure UpdateGlobalAmbient;
    procedure UpdateProjectionMatrix;
    procedure UpdateDepthRange;
    procedure UpdateCullFace;
    procedure UpdateFrontFace;
    procedure UpdateDepthTest;
    procedure UpdateDepthFunc;
    procedure UpdateColorChannels;
    procedure UpdateDepthBufferUpdate;
    procedure UpdateCurrentProgram;
    procedure UpdateCurrentVao;
    procedure UpdateBlendingEnabled;
    procedure UpdateBlendingFunction;
    procedure UpdateBlendingConstantColor;
    procedure UpdateFixedFunctionAlphaTest;
    procedure UpdateFixedFunctionAlphaCutoff;
    procedure UpdateFixedFunctionLighting;
    procedure UpdateLineType;
    procedure UpdatePolygonOffset;
  private
    FEnabledScissors: TScissorList;
  public
    { If @true, @link(Clear) will clear color buffer by DrawRectangle instead
      of OpenGL glClear call.
      This is a bit slower, but makes it honor the stencil test. }
    InternalClearColorsByDraw: Boolean;

    { Used only when we render into a rectangle within the context,
      that doesn't necessarily cover the whole context.
      In this case InternalControlRect has some useful (not empty) value.

      Practically: this is used when rendering a TCastleControl in FMX
      on Android or iOS, in which case we have context from the FMX framework
      (that covers whole form), but we only want to render in our area.

      All viewport and scissor coordinates should take this rect into account,
      when it is not empty and InternalControlRectIgnore = 0.
      Scissors must be shifted and cut according to this rect.
      Viewport must be shifted according to this rect.
      GL commands like glReadPixels should also take this rect into account.

      This is applied when calling OpenGL(ES), but remains invisible
      otherwise, e.g. our FinalScissor returns scissor @italic(without)
      this applied. This way returned scissor is valid in TCastleContainer
      coordinates.

      We assume you change this field only before SynchronizeState,
      then call SynchronizeState, and never change this during normal rendering.
      So this is just a field, with no setter, it will just affect
      all future viewport and scissor setting (including the ones done
      by SynchronizeState). }
    InternalControlRect: TRectangle;

    { When non-zero, we are rendering to FBO and thus InternalControlRect
      should be ignored. }
    property InternalControlRectIgnore: Cardinal
      read FInternalControlRectIgnore
      write SetInternalControlRectIgnore;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Effective viewport rectangle after applying @link(ViewportDelta)
      and @link(InternalControlRect) (when @link(InternalControlRectIgnore) = 0). }
    function EffectiveViewportRect: TRectangle;

    { Clear the whole buffer contents.

      Never call OpenGL glClear or glClearColor, always use this method. }
    procedure Clear(Buffers: TClearBuffers; const ClearColor: TCastleColor);

    { The rendered line width.
      Never call OpenGL glLineWidth directly.

      Do not access this property directly, unless you make direct
      OpenGL/OpenGLES calls. In normal circumstances, engine API
      (like DrawPrimitive2D or TCastleScene) set this automatically. }
    property LineWidth: Single read FLineWidth write SetLineWidth {$ifdef FPC}default 1{$endif};

    { The rendered point size.
      Never call OpenGL glPointSize directly.

      Do not access this property directly, unless you make direct
      OpenGL/OpenGLES calls. In normal circumstances, engine API
      (like DrawPrimitive2D or TCastleScene) set this automatically. }
    property PointSize: Single read FPointSize write SetPointSize {$ifdef FPC}default 1{$endif};

    { Global ambient lighting. This is added to every 3D object color,
      multiplied by material ambient.

      The default value is (0.2, 0.2, 0.2). It matches default
      GL_LIGHT_MODEL_AMBIENT in fixed-function OpenGL.
      It also matches the required value of VRML 1.0 specification.
      For VRML 2.0 / X3D, lighting equations suggest that it should be zero. }
    property GlobalAmbient: TCastleColorRGB read FGlobalAmbient write SetGlobalAmbient;

    { Enable or disable scissor.

      Never call OpenGL glScissor or glEnable(GL_SCISSOR_TEST) / glDisable(GL_SCISSOR_TEST)
      directly, or push/pop the related attrib (in case of fixed-function pipeline).
      @groupBegin }
    procedure ScissorEnable(const Rect: TRectangle);
    procedure ScissorDisable;
    { @groupEnd }

    { Current projection matrix.

      When GLFeatures.EnableFixedFunction = true, setting this also
      sets fixed-function projection matrix. }
    property ProjectionMatrix: TMatrix4
      read FProjectionMatrix write SetProjectionMatrix;

    { Use this to operate on OpenGL glDepthRange. For now, our engine has
      very simple use for this, for TPlayer.RenderOnTop. }
    property DepthRange: TDepthRange read FDepthRange write SetDepthRange;

    { Should we use backface-culling (ignore some faces during rendering). }
    property CullFace: boolean read FCullFace write SetCullFace
      default false;

    { Is the front face ordered counter-clockwise.
      The "front face" is important to interpreting the @link(CullFace)
      and to interpret the normal vectors (they point outward from front face). }
    property FrontFaceCcw: boolean read FFrontFaceCcw write SetFrontFaceCcw
      default true;

    { Depth testing means we reject things that don't pass @link(DepthFunc). }
    property DepthTest: Boolean read FDepthTest write SetDepthTest default false;

    { Function to use when comparing depth, only if @link(DepthTest). }
    property DepthFunc: TDepthFunction read FDepthFunc write SetDepthFunc default dfLess;

    {$ifdef FPC}
    { Are color buffer channels changed by rendering. }
    property ColorMask: boolean
      read GetColorMask write SetColorMask default true;
      deprecated 'use ColorChannels';
    {$endif}

    { Which color buffer channels are written by rendering.
      This affects all rendering, including @link(TDrawableImage.Draw).

      Note that this state may be internally modified by various engine rendering
      functions too, e.g. by @link(TCastleScene.LocalRender), depending on rendering
      attributes.
      So it is only reliable for you to modify this temporarily,
      and during your own rendering.
      E.g. change ColorChannels right before and right after calling a couple of
      @link(TDrawableImage.Draw). Always restore it afterwards to default [0..3].

      Example uses:

      @unorderedList(
        @item(
          Set this to ccNone to temporarily turn off writing to the color buffer.
          This is useful e.g. if you only want to write
          to the delph and stencil buffer.)
        @item(
          Set this to [0..2] to temporarily turn off writing to the alpha channel,
          or to [3] to temporarily turn off writing to the RGB channels.)
        @item(
          Set this [0..3] to go back to default (write all channels).)
      )
    }
    property ColorChannels: TColorChannels
      read FColorChannels write SetColorChannels default AllColorChannels;

    { Is depth buffer updated by rendering.
      This affects all rendering that enables depth testing
      (which in practice means only TCastleScene in CGE). }
    property DepthBufferUpdate: Boolean
      read FDepthBufferUpdate write SetDepthBufferUpdate default true;

    { Controls OpenGL viewport. This is always shifted by ViewportDelta. }
    property Viewport: TRectangle read FViewport write SetViewport;

    { Shifts the rectangle passed to @link(Viewport).
      Only the @link(TCastleScreenEffects) should control this. }
    property ViewportDelta: TVector2Integer read FViewportDelta write SetViewportDelta;

    { Scissor rectangle to which we clip the view.
      This is determined by all @link(TScissor) that are currently enabled.
      Returns @true and sets Rect if any scissor is active.
      Returns @false if we don't clip the view now. }
    function FinalScissor(out Rect: TRectangle): Boolean;

    { Currently enabled GLSL program.
      @nil if none (rendering is not possible, unless ancient fixed-function pipeline is used).
      Setting this property encapsulates the OpenGL glUseProgram. }
    property CurrentProgram: TGLSLProgram read FCurrentProgram write SetCurrentProgram;

    { Currently bound vertex array object.
      @nil if none.. }
    property CurrentVao: TVertexArrayObject read FCurrentVao write SetCurrentVao;

    { Does the current color buffer have any alpha channel.
      Some blending features depend on storing alpha in the color channel. }
    function ColorBufferHasAlpha: Boolean; deprecated 'use GLFeatures.AlphaBits > 0';

    { Enable blending, sets also blending function.
      See https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml
      for blending equations.
      You @italic(must) use this to change blending parameters, this class
      assumes it knows the blending state always. }
    procedure BlendingEnable(
      const SourceFactor: TBlendingSourceFactor = bsSrcAlpha;
      const DestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha);
    procedure BlendingDisable;
    property BlendingEnabled: Boolean read FBlendingEnabled;
    property BlendingSourceFactor: TBlendingSourceFactor read FBlendingSourceFactor;
    property BlendingDestinationFactor: TBlendingDestinationFactor read FBlendingDestinationFactor;
    property BlendingConstantColor: TCastleColor read FBlendingConstantColor write SetBlendingConstantColor;
    class function MapBlendSourceFactorToRHI(const AFactor: TBlendingSourceFactor): TRHIBlendFactor; static;
    class function MapBlendDestFactorToRHI(const AFactor: TBlendingDestinationFactor): TRHIBlendFactor; static;
    class function MapDepthFuncToRHI(const ADepthFunc: TDepthFunction): TRHICompareOp; static;
    class function LineTypeToStipplePattern(const ALineType: TLineType): Word; static;

    { Enable alpha test with old/buggy OpenGL implementations that use deprecated fixed-function pipeline.
      This is simply ignored on modern GPUs when "not GLFeatures.EnableFixedFunction". }
    procedure FixedFunctionAlphaTestEnable(const AlphaCutoff: Single = 0.5);
    procedure FixedFunctionAlphaTestDisable;

    { Fixed-function GL_LIGHTING state.
      This is simply ignored on modern GPUs when "not GLFeatures.EnableFixedFunction". }
    property FixedFunctionLighting: boolean read FFixedFunctionLighting write SetFixedFunctionLighting;

    { Line type determines OpenGL glLineStipple, GL_LINE_STIPPLE enable state.
      Note: This is not supported on OpenGLES. }
    property LineType: TLineType read FLineType write SetLineType;

    { Current polygon offset state. You can read and write it. }
    property PolygonOffset: TPolygonOffset read FPolygonOffset write SetPolygonOffset;
    { Shortcut for setting @link(PolygonOffset) with PolygonOffset.Enabled = @true. }
    procedure PolygonOffsetEnable(const Scale, Bias: Single);
    { Shortcut for setting @link(PolygonOffset) with PolygonOffset.Enabled = @false. }
    procedure PolygonOffsetDisable;

    { Bind buffer to target, just like glBindBuffer.

      Optimized to do nothing if given buffer is already bound.
      This optimization actually matters for optimization (Android Samsung Galaxy Tab,
      castle-model-viewer-mobile displaying inspector).
      Without it, TDrawableImage does a lot of redundant glBindBuffer calls. }
    property BindBuffer[const Target: TBufferTarget]: TGLBuffer
      read GetBoundBuffer write SetBoundBuffer;

    { Make sure the current OpenGL(ES) state corresponds to what this class
      expects. Use this after some code, like FMX, did OpenGL(ES)
      calls that possibly changed the OpenGL(ES) state without going
      through this class. }
    procedure SynchronizeState;

    { RHI state synchronization diagnostics (Phase 12.1). }
    property RHIClearCount: Cardinal read FRHIClearCount;
    property RHILastClearBuffers: TClearBuffers read FRHILastClearBuffers;
    property RHILastClearColor: TCastleColor read FRHILastClearColor;
    property RHIViewportUpdateCount: Cardinal read FRHIViewportUpdateCount;
    property RHILastViewport: TFloatRectangle read FRHILastViewport;
    property RHILastMinDepth: Single read FRHILastMinDepth;
    property RHILastMaxDepth: Single read FRHILastMaxDepth;
    property RHIScissorUpdateCount: Cardinal read FRHIScissorUpdateCount;
    property RHILastScissor: TRectangle read FRHILastScissor;
    property RHIScissorActive: Boolean read FRHIScissorActive;
    property RHIDepthBiasUpdateCount: Cardinal read FRHIDepthBiasUpdateCount;
    property RHILastLineWidth: Single read FRHILastLineWidth;
    property RHILineWidthUpdateCount: Cardinal read FRHILineWidthUpdateCount;
    property RHILastLineType: TLineType read FRHILastLineType;
    property RHILastLineStipplePattern: Word read FRHILastLineStipplePattern;
    property RHILineTypeUpdateCount: Cardinal read FRHILineTypeUpdateCount;
    property RHILastBlendConstantColor: TCastleColor read FRHILastBlendConstantColor;
    property RHIBlendConstantColorUpdateCount: Cardinal read FRHIBlendConstantColorUpdateCount;

    { Stencil test state and RHI pipeline synchronization (used by TCastleMask). }
    property StencilTest: Boolean read FStencilTest write SetStencilTest default False;
    property StencilCompareOp: TRHICompareOp read FStencilCompareOp;
    property StencilReference: Byte read FStencilReference;
    property StencilCompareMask: Byte read FStencilCompareMask;
    property StencilWriteMask: Byte read FStencilWriteMask;
    property StencilFailOp: TRHIStencilOp read FStencilFailOp;
    property StencilDepthFailOp: TRHIStencilOp read FStencilDepthFailOp;
    property StencilPassOp: TRHIStencilOp read FStencilPassOp;
    procedure StencilFunc(const ACompareOp: TRHICompareOp; const AReference, ACompareMask: Byte);
    procedure StencilOp(const AFailOp, ADepthFailOp, APassOp: TRHIStencilOp; const AWriteMask: Byte = $FF);
    procedure ApplyRHIStencilAndColorMask(var ADepthStencil: TRHIDepthStencilStateDesc; var ABlendState: TRHIBlendStateDesc);
  end;

var
  { Current OpenGL / OpenGLES context state.
    @bold(Only access it during the rendering, i.e. in TCastleUserInterface.Render.)

    TODO: In the future, this global singleton may be removed,
    and this may be accessible instead through a new TCastleUserInterface.Render parameter. }
  RenderContext: TRenderContext;

{ Projection matrix -------------------------------------------------------- } { }

{ Calculate projection matrix, and set
  @link(TRenderContext.ProjectionMatrix RenderContext.ProjectionMatrix)
  to given value.

  For PerspectiveProjection, ZFar may have special ZFarInfinity value
  to create a perspective projection with far plane set at infinity.
  Useful e.g. for z-fail shadow volumes.

  @groupBegin }
function PerspectiveProjection(const fovy, aspect, ZNear, ZFar: Single): TMatrix4;
function OrthoProjection(const Dimensions: TFloatRectangle;
  const ZNear: Single = -1; const ZFar: Single = 1): TMatrix4;
function FrustumProjection(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;
{ @groupEnd }

const
  BufferTargetGL: array [TBufferTarget] of TGLenum = (
    GL_ARRAY_BUFFER,
    GL_ELEMENT_ARRAY_BUFFER
  );

implementation

uses CastleLog, CastleProjection, CastleInternalGLUtils, CastleGLImages,
  CastleApplicationProperties;

function SkipOpenGLStateWhenRHIActive: Boolean; inline;
begin
  Result := (RHIDevice <> nil) and
    ((ActiveCommandList <> nil) or (GLFeatures = nil) or (ApplicationProperties = nil) or (not ApplicationProperties.IsGLContextOpen));
end;

constructor TRenderContext.Create(AOwner: TComponent);
begin
  inherited;
  FLineWidth := 1;
  FPointSize := 1;
  FGlobalAmbient := Vector3(0.2, 0.2, 0.2);
  FEnabledScissors := TScissorList.Create(false);
  FEnabledScissors.ParentContext := Self;
  FProjectionMatrix := TMatrix4.Identity;
  FDepthRange := drFull;
  FRHILastMinDepth := 0.0;
  FRHILastMaxDepth := 1.0;
  FCullFace := false;
  FFrontFaceCcw := true;
  FColorChannels := AllColorChannels;
  FDepthBufferUpdate := true;
  FDepthTest := false;
  FDepthFunc := dfLess;
  FViewport := TRectangle.Empty;
  FBlendingEnabled := false;
  // initial blending factors, see https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml
  FBlendingSourceFactor := bsOne;
  FBlendingDestinationFactor := bdZero;
  { the initial glAlphaFunc state is (GL_ALWAYS, 0), see https://docs.gl/gl3/glAlphaFunc,
    so be sure to call glAlphaFunc in 1st FixedFunctionAlphaTestEnable. }
  FFixedFunctionAlphaCutoff := -1;
  FInternalControlRectIgnore := 0;
  InternalControlRect := TRectangle.Empty;
  FStencilTest := False;
  FStencilCompareOp := coAlways;
  FStencilReference := 0;
  FStencilCompareMask := $FF;
  FStencilWriteMask := $FF;
  FStencilFailOp := soKeep;
  FStencilDepthFailOp := soKeep;
  FStencilPassOp := soKeep;
  FRHILastLineType := ltSolid;
  FRHILastLineStipplePattern := $FFFF;
  FBlendingConstantColor := Vector4(0, 0, 0, 0);
  FRHILastBlendConstantColor := Vector4(0, 0, 0, 0);
end;

destructor TRenderContext.Destroy;
begin
  FreeAndNil(FEnabledScissors);
  FreeAndNil(FGlobalScissor);
  inherited;
end;

procedure TRenderContext.WarnContextNotCurrent;
begin
  WritelnWarning('RenderContext', 'Do not access TRenderContext properties and methods when this context is not the "current" one. Always access the properties and methods through the RenderContext singleton to avoid this warning.');
end;

procedure TRenderContext.Clear(Buffers: TClearBuffers;
  const ClearColor: TCastleColor);
const
  ClearBufferMask: array [TClearBuffer] of TGLbitfield =
  ( GL_COLOR_BUFFER_BIT,
    GL_DEPTH_BUFFER_BIT,
    GL_STENCIL_BUFFER_BIT );
var
  Mask: TGLbitfield;
  B: TClearBuffer;
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  TDrawableImage.BatchingFlush;

  if InternalClearColorsByDraw and (cbColor in Buffers) then
  begin
    DrawRectangle(Viewport, ClearColor,
      { Using these blending factors means that ClearColor overrides screen,
        just like glClear does. So we don't do blending, in case ClearColor.W is <> 1,
        which is consistent with our behavior when InternalClearColorsByDraw = false. }
      bsOne, bdZero, false);
    Exclude(Buffers, cbColor);
  end;

  if (RHIDevice <> nil) and (Buffers <> []) then
  begin
    Inc(FRHIClearCount);
    FRHILastClearBuffers := Buffers;
    if cbColor in Buffers then
      FRHILastClearColor := ClearColor;
    if ActiveCommandList <> nil then
    begin
      if ActiveCommandList.IsRendering and (not FRHIScissorActive) then
      begin
        ActiveCommandList.SetScissor(ActiveCommandList.ActiveRenderingInfo.RenderArea);
        try
          ActiveCommandList.ClearAttachments(
            cbColor in Buffers, ClearColor,
            cbDepth in Buffers, 1.0,
            cbStencil in Buffers, 0);
        finally
          if (not EffectiveViewportRect.IsEmpty) and
             ActiveCommandList.ActiveRenderingInfo.RenderArea.Collides(EffectiveViewportRect) then
            ActiveCommandList.SetScissor(EffectiveViewportRect);
        end;
      end else
        ActiveCommandList.ClearAttachments(
          cbColor in Buffers, ClearColor,
          cbDepth in Buffers, 1.0,
          cbStencil in Buffers, 0);
    end;
  end;

  if (cbColor in Buffers) and
     not TVector4.PerfectlyEquals(FClearColor, ClearColor) then
  begin
    FClearColor := ClearColor;
    UpdateClearColor;
  end;

  if SkipOpenGLStateWhenRHIActive then
    Exit;

  Mask := 0;
  for B in Buffers do
    Mask := Mask or ClearBufferMask[B];
  if Mask <> 0 then
    glClear(Mask);
end;

procedure TRenderContext.UpdateClearColor;
begin
  if SkipOpenGLStateWhenRHIActive then
    Exit;
  glClearColor(FClearColor.X, FClearColor.Y, FClearColor.Z, FClearColor.W);
end;

procedure TRenderContext.SetLineWidth(const Value: Single);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    UpdateLineWidth;
  end;
end;

procedure TRenderContext.UpdateLineWidth;
begin
  if RHIDevice <> nil then
  begin
    FRHILastLineWidth := FLineWidth;
    Inc(FRHILineWidthUpdateCount);
    if ActiveCommandList <> nil then
      ActiveCommandList.SetLineWidth(FLineWidth);
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  {$if (not defined(DARWIN)) or defined(OpenGLES)}
  // on desktop macOS, glLineWidth raise GL_INVALID_VALUE for parameters <> 1.0
  glLineWidth(FLineWidth);
  {$endif}
end;

procedure TRenderContext.SetPointSize(const Value: Single);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FPointSize <> Value then
  begin
    FPointSize := Value;
    UpdatePointSize;
  end;
end;

procedure TRenderContext.UpdatePointSize;
begin
  if SkipOpenGLStateWhenRHIActive then
    Exit;
  { Not possible with OpenGL ES.
    See http://stackoverflow.com/questions/9381562/using-gl-points-in-glkit-ios-5
    http://www.idevgames.com/forums/thread-3.html :
    "You must write gl_PointSize in the vertex shader, per point." }

  {$ifndef OpenGLES}
  glPointSize(FPointSize);
  {$endif}
end;

procedure TRenderContext.SetGlobalAmbient(const Value: TCastleColorRGB);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if not TVector3.PerfectlyEquals(FGlobalAmbient, Value) then
  begin
    FGlobalAmbient := Value;
    UpdateGlobalAmbient;
  end;
end;

procedure TRenderContext.UpdateGlobalAmbient;
begin
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    { We always set "1" as global ambient alpha.
      This alpha does not have any useful interpretation, it seems,
      so don't let it change. }
    glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4(FGlobalAmbient, 1));
    {$endif}
  end;
end;

procedure TRenderContext.ScissorEnable(const Rect: TRectangle);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FGlobalScissor = nil then
    FGlobalScissor := TScissor.Create
  else
    FGlobalScissor.Enabled := false; // disable previously enabled scissor, if any
  FGlobalScissor.Rect := Rect;
  FGlobalScissor.Enabled := true;
end;

procedure TRenderContext.ScissorDisable;
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FGlobalScissor <> nil then // secure in case FGlobalScissor was already fred
    FGlobalScissor.Enabled := false;
end;

procedure TRenderContext.SetProjectionMatrix(const Value: TMatrix4);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  FProjectionMatrix := Value;
  UpdateProjectionMatrix;
end;

procedure TRenderContext.UpdateProjectionMatrix;
begin
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    glMatrixMode(GL_PROJECTION);
    {$warnings off}
    glLoadMatrix(FProjectionMatrix); // consciously using deprecated stuff; this should be internal in this unit
    {$warnings on}
    glMatrixMode(GL_MODELVIEW);
    {$endif}
  end;
end;

procedure TRenderContext.GetEffectiveDepthRange(out AMinDepth, AMaxDepth: Single);
begin
  case FDepthRange of
    drFull: begin AMinDepth := 0.0; AMaxDepth := 1.0; end;
    drNear: begin AMinDepth := 0.0; AMaxDepth := 0.1; end;
    drFar : begin AMinDepth := 0.1; AMaxDepth := 1.0; end;
  end;
end;

procedure TRenderContext.SetDepthRange(const Value: TDepthRange);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthRange <> Value then
  begin
    FDepthRange := Value;
    UpdateDepthRange;
  end;
end;

procedure TRenderContext.UpdateDepthRange;

  {$if defined(OpenGLES) and not defined(CASTLE_WEBGL)}
  // Define glDepthRange (not existing in OpenGLES) as alias to glDepthRangef
  procedure glDepthRange(const zNear, zFar: GLclampf);
  begin
    glDepthRangef(zNear, zFar);
  end;
  {$endif}

var
  MinD, MaxD: Single;
begin
  GetEffectiveDepthRange(MinD, MaxD);
  if RHIDevice <> nil then
  begin
    FRHILastMinDepth := MinD;
    FRHILastMaxDepth := MaxD;
    FRHILastViewport := FloatRectangle(EffectiveViewportRect);
    Inc(FRHIViewportUpdateCount);
    if ActiveCommandList <> nil then
      ActiveCommandList.SetViewport(FRHILastViewport, MinD, MaxD);
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;

  case FDepthRange of
    drFull: glDepthRange(0  , 1);
    drNear: glDepthRange(0  , 0.1);
    drFar : glDepthRange(0.1, 1);
  end;
end;

procedure TRenderContext.SetCullFace(const Value: boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FCullFace <> Value then
  begin
    FCullFace := Value;
    UpdateCullFace;
  end;
end;

procedure TRenderContext.UpdateCullFace;
begin
  if RHIDevice <> nil then
  begin
    if ActiveCommandList <> nil then
    begin
      if FCullFace then
        ActiveCommandList.SetCullMode(cmBack)
      else
        ActiveCommandList.SetCullMode(cmNone);
    end;
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  GLSetEnabled(GL_CULL_FACE, FCullFace);
end;

procedure TRenderContext.SetFrontFaceCcw(const Value: boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FFrontFaceCcw <> Value then
  begin
    FFrontFaceCcw := Value;
    UpdateFrontFace;
  end;
end;

procedure TRenderContext.UpdateFrontFace;
begin
  if RHIDevice <> nil then
  begin
    if ActiveCommandList <> nil then
    begin
      if FFrontFaceCcw then
        ActiveCommandList.SetFrontFace(ffCounterClockwise)
      else
        ActiveCommandList.SetFrontFace(ffClockwise);
    end;
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  if FFrontFaceCcw then
    glFrontFace(GL_CCW)
  else
    glFrontFace(GL_CW);
end;

procedure TRenderContext.SetDepthTest(const Value: Boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthTest <> Value then
  begin
    FDepthTest := Value;
    UpdateDepthTest;
  end;
end;

procedure TRenderContext.UpdateDepthTest;
begin
  if RHIDevice <> nil then
  begin
    if ActiveCommandList <> nil then
      ActiveCommandList.SetDepthTestEnable(FDepthTest);
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  GLSetEnabled(GL_DEPTH_TEST, FDepthTest);
end;

procedure TRenderContext.SetDepthFunc(const Value: TDepthFunction);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthFunc <> Value then
  begin
    FDepthFunc := Value;
    UpdateDepthFunc;
  end;
end;

procedure TRenderContext.UpdateDepthFunc;
begin
  if RHIDevice <> nil then
  begin
    if ActiveCommandList <> nil then
    begin
      case FDepthFunc of
        dfNever:        ActiveCommandList.SetDepthCompareOp(coNever);
        dfLess:         ActiveCommandList.SetDepthCompareOp(coLess);
        dfEqual:        ActiveCommandList.SetDepthCompareOp(coEqual);
        dfLessEqual:    ActiveCommandList.SetDepthCompareOp(coLessOrEqual);
        dfGreater:      ActiveCommandList.SetDepthCompareOp(coGreater);
        dfNotEqual:     ActiveCommandList.SetDepthCompareOp(coNotEqual);
        dfGreaterEqual: ActiveCommandList.SetDepthCompareOp(coGreaterOrEqual);
        dfAlways:       ActiveCommandList.SetDepthCompareOp(coAlways);
      end;
    end;
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  glDepthFunc(Ord(FDepthFunc));
end;

function TRenderContext.GetColorMask: boolean;
begin
  Result := ColorChannels <> [];
end;

procedure TRenderContext.SetColorMask(const Value: boolean);
begin
  if Value then
    ColorChannels := AllColorChannels
  else
    ColorChannels := [];
end;

procedure TRenderContext.SetColorChannels(const Value: TColorChannels);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FColorChannels <> Value then
  begin
    TDrawableImage.BatchingFlush;
    FColorChannels := Value;
    UpdateColorChannels;
  end;
end;

procedure TRenderContext.SetStencilTest(const Value: Boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FStencilTest <> Value then
  begin
    TDrawableImage.BatchingFlush;
    FStencilTest := Value;
    if not SkipOpenGLStateWhenRHIActive then
      GLSetEnabled(GL_STENCIL_TEST, FStencilTest);
  end;
end;

procedure TRenderContext.StencilFunc(const ACompareOp: TRHICompareOp; const AReference, ACompareMask: Byte);
const
  CompareOpToGL: array [TRHICompareOp] of TGLenum = (
    GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL,
    GL_GREATER, GL_NOTEQUAL, GL_GEQUAL, GL_ALWAYS
  );
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  TDrawableImage.BatchingFlush;
  FStencilCompareOp := ACompareOp;
  FStencilReference := AReference;
  FStencilCompareMask := ACompareMask;
  if not SkipOpenGLStateWhenRHIActive then
    glStencilFunc(CompareOpToGL[ACompareOp], AReference, ACompareMask);
end;

procedure TRenderContext.StencilOp(const AFailOp, ADepthFailOp, APassOp: TRHIStencilOp; const AWriteMask: Byte);
const
  StencilOpToGL: array [TRHIStencilOp] of TGLenum = (
    GL_KEEP, GL_ZERO, GL_REPLACE, GL_INCR,
    GL_DECR, GL_INVERT, GL_INCR_WRAP, GL_DECR_WRAP
  );
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  TDrawableImage.BatchingFlush;
  FStencilFailOp := AFailOp;
  FStencilDepthFailOp := ADepthFailOp;
  FStencilPassOp := APassOp;
  FStencilWriteMask := AWriteMask;
  if not SkipOpenGLStateWhenRHIActive then
  begin
    glStencilMask(AWriteMask);
    glStencilOp(StencilOpToGL[AFailOp], StencilOpToGL[ADepthFailOp], StencilOpToGL[APassOp]);
  end;
end;

class function TRenderContext.MapBlendSourceFactorToRHI(
  const AFactor: TBlendingSourceFactor): TRHIBlendFactor;
begin
  case AFactor of
    bsZero:                   Result := bfZero;
    bsOne:                    Result := bfOne;
    bsSrcColor:               Result := bfSrcColor;
    bsOneMinusSrcColor:       Result := bfOneMinusSrcColor;
    bsDstColor:               Result := bfDstColor;
    bsOneMinusDstColor:       Result := bfOneMinusDstColor;
    bsSrcAlpha:               Result := bfSrcAlpha;
    bsOneMinusSrcAlpha:       Result := bfOneMinusSrcAlpha;
    bsDstAlpha:               Result := bfDstAlpha;
    bsOneMinusDstAlpha:       Result := bfOneMinusDstAlpha;
    bsConstantColor:          Result := bfConstantColor;
    bsOneMinusConstantColor:  Result := bfOneMinusConstantColor;
    bsConstantAlpha:          Result := bfConstantAlpha;
    bsOneMinusConstantAlpha:  Result := bfOneMinusConstantAlpha;
    bsSrcAlphaSaturate:       Result := bfSrcAlphaSaturate;
    else                      Result := bfSrcAlpha;
  end;
end;

class function TRenderContext.MapBlendDestFactorToRHI(
  const AFactor: TBlendingDestinationFactor): TRHIBlendFactor;
begin
  case AFactor of
    bdZero:                   Result := bfZero;
    bdOne:                    Result := bfOne;
    bdSrcColor:               Result := bfSrcColor;
    bdOneMinusSrcColor:       Result := bfOneMinusSrcColor;
    bdDstColor:               Result := bfDstColor;
    bdOneMinusDstColor:       Result := bfOneMinusDstColor;
    bdSrcAlpha:               Result := bfSrcAlpha;
    bdOneMinusSrcAlpha:       Result := bfOneMinusSrcAlpha;
    bdDstAlpha:               Result := bfDstAlpha;
    bdOneMinusDstAlpha:       Result := bfOneMinusDstAlpha;
    bdConstantColor:          Result := bfConstantColor;
    bdOneMinusConstantColor:  Result := bfOneMinusConstantColor;
    bdConstantAlpha:          Result := bfConstantAlpha;
    bdOneMinusConstantAlpha:  Result := bfOneMinusConstantAlpha;
    else                      Result := bfOneMinusSrcAlpha;
  end;
end;

class function TRenderContext.MapDepthFuncToRHI(
  const ADepthFunc: TDepthFunction): TRHICompareOp;
begin
  case ADepthFunc of
    dfNever:        Result := coNever;
    dfLess:         Result := coLess;
    dfEqual:        Result := coEqual;
    dfLessEqual:    Result := coLessOrEqual;
    dfGreater:      Result := coGreater;
    dfNotEqual:     Result := coNotEqual;
    dfGreaterEqual: Result := coGreaterOrEqual;
    dfAlways:       Result := coAlways;
    else            Result := coLess;
  end;
end;

procedure TRenderContext.ApplyRHIStencilAndColorMask(
  var ADepthStencil: TRHIDepthStencilStateDesc;
  var ABlendState: TRHIBlendStateDesc);
var
  Mask: Byte;
begin
  if FColorChannels <> AllColorChannels then
  begin
    Mask := 0;
    if 0 in FColorChannels then Mask := Mask or $01;
    if 1 in FColorChannels then Mask := Mask or $02;
    if 2 in FColorChannels then Mask := Mask or $04;
    if 3 in FColorChannels then Mask := Mask or $08;
    ABlendState.ColorWriteMask := Mask;
  end;

  if FBlendingEnabled then
  begin
    ABlendState.BlendEnable := True;
    ABlendState.SrcColorBlendFactor := MapBlendSourceFactorToRHI(FBlendingSourceFactor);
    ABlendState.DstColorBlendFactor := MapBlendDestFactorToRHI(FBlendingDestinationFactor);
    ABlendState.SrcAlphaBlendFactor := ABlendState.SrcColorBlendFactor;
    ABlendState.DstAlphaBlendFactor := ABlendState.DstColorBlendFactor;
  end;

  if ADepthStencil.DepthTestEnable then
    ADepthStencil.DepthCompareOp := MapDepthFuncToRHI(FDepthFunc);

  if FStencilTest then
  begin
    ADepthStencil.StencilTestEnable := True;
    ADepthStencil.FrontStencil.CompareOp := FStencilCompareOp;
    ADepthStencil.FrontStencil.Reference := FStencilReference;
    ADepthStencil.FrontStencil.CompareMask := FStencilCompareMask;
    ADepthStencil.FrontStencil.WriteMask := FStencilWriteMask;
    ADepthStencil.FrontStencil.FailOp := FStencilFailOp;
    ADepthStencil.FrontStencil.DepthFailOp := FStencilDepthFailOp;
    ADepthStencil.FrontStencil.PassOp := FStencilPassOp;
    ADepthStencil.BackStencil := ADepthStencil.FrontStencil;
  end;
end;

procedure TRenderContext.UpdateColorChannels;
begin
  if SkipOpenGLStateWhenRHIActive then
    Exit;
  glColorMask(
    BoolToGL(0 in FColorChannels),
    BoolToGL(1 in FColorChannels),
    BoolToGL(2 in FColorChannels),
    BoolToGL(3 in FColorChannels)
  );
end;

procedure TRenderContext.SetDepthBufferUpdate(const Value: boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthBufferUpdate <> Value then
  begin
    FDepthBufferUpdate := Value;
    UpdateDepthBufferUpdate;
  end;
end;

procedure TRenderContext.UpdateDepthBufferUpdate;
begin
  if RHIDevice <> nil then
  begin
    if ActiveCommandList <> nil then
      ActiveCommandList.SetDepthWriteEnable(FDepthBufferUpdate);
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  glDepthMask(BoolToGL(FDepthBufferUpdate));
end;

function TRenderContext.EffectiveViewportRect: TRectangle;
var
  Delta: TVector2Integer;
begin
  Delta := FViewportDelta;
  if (InternalControlRectIgnore = 0) and
     (not InternalControlRect.IsEmpty) then
    Delta := Delta + InternalControlRect.LeftBottom;

  Result := Rectangle(
    FViewport.Left   + Delta.X,
    FViewport.Bottom + Delta.Y,
    FViewport.Width,
    FViewport.Height);
end;

procedure TRenderContext.UpdateViewport;
var
  EffRect: TRectangle;
  MinD, MaxD: Single;
begin
  EffRect := EffectiveViewportRect;
  if RHIDevice <> nil then
  begin
    GetEffectiveDepthRange(MinD, MaxD);
    FRHILastViewport := FloatRectangle(EffRect);
    FRHILastMinDepth := MinD;
    FRHILastMaxDepth := MaxD;
    Inc(FRHIViewportUpdateCount);
    if ActiveCommandList <> nil then
      ActiveCommandList.SetViewport(FRHILastViewport, MinD, MaxD);
    { When no explicit scissor is active, keep the dynamic RHI scissor aligned with the viewport }
    if not FRHIScissorActive then
    begin
      FRHILastScissor := EffRect;
      if ActiveCommandList <> nil then
        ActiveCommandList.SetScissor(EffRect);
    end;
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;

  glViewport(
    EffRect.Left,
    EffRect.Bottom,
    EffRect.Width,
    EffRect.Height);
end;

procedure TRenderContext.WarningViewportTooLarge;
begin
  if GLFeatures = nil then
    Exit;
  WritelnWarningOnce(WarningViewportTooLargeDone,
    'Setting viewport to %s, which has dimensions larger than maximum allowed %s. (Further warnings of the same type will not be shown.)', [
    FViewport.ToString,
    GLFeatures.MaxViewportDimensions.ToString
  ]);
end;

procedure TRenderContext.SetViewport(const Value: TRectangle);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if not FViewport.Equals(Value) then
  begin
    FViewport := Value;

    if (GLFeatures <> nil) and
       ((FViewport.Width > GLFeatures.MaxViewportDimensions.X) or
        (FViewport.Height > GLFeatures.MaxViewportDimensions.Y)) then
      WarningViewportTooLarge;

    UpdateViewport;
  end;
end;

procedure TRenderContext.SetViewportDelta(const Value: TVector2Integer);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if not TVector2Integer.Equals(FViewportDelta, Value) then
  begin
    FViewportDelta := Value;
    UpdateViewport;
  end;
end;

function TRenderContext.FinalScissor(out Rect: TRectangle): Boolean;
begin
  Result :=
    (FEnabledScissors <> nil) and
    (FEnabledScissors.Count <> 0);
  if Result then
    Rect := FEnabledScissors.FFinalScissor;
end;

procedure TRenderContext.SetCurrentProgram(const Value: TGLSLProgram);
begin
  if FCurrentProgram <> Value then
  begin
    FCurrentProgram := Value;
    UpdateCurrentProgram;
  end;
end;

procedure TRenderContext.UpdateCurrentProgram;
begin
  if SkipOpenGLStateWhenRHIActive then
    Exit;
  InternalSetCurrentProgram(FCurrentProgram);
end;

procedure TRenderContext.SetCurrentVao(const Value: TVertexArrayObject);
begin
  if FCurrentVao <> Value then
  begin
    FCurrentVao := Value;
    UpdateCurrentVao;
  end;
end;

procedure TRenderContext.UpdateCurrentVao;
begin
  if (not SkipOpenGLStateWhenRHIActive) and (GLFeatures <> nil) and GLFeatures.VertexArrayObject then
  begin
    if FCurrentVao <> nil then
      glBindVertexArray(FCurrentVao.InternalHandle(Self))
    else
      glBindVertexArray(GLObjectNone);
  end;
end;

function TRenderContext.ColorBufferHasAlpha: Boolean;
begin
  Result := (RHIDevice <> nil) or ((GLFeatures <> nil) and (GLFeatures.AlphaBits > 0));
end;

procedure TRenderContext.BlendingEnable(
  const SourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const DestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha);
begin
  if not FBlendingEnabled then
  begin
    FBlendingEnabled := true;
    UpdateBlendingEnabled;
  end;

  if (FBlendingSourceFactor <> SourceFactor) or
     (FBlendingDestinationFactor <> DestinationFactor) then
  begin
    FBlendingSourceFactor := SourceFactor;
    FBlendingDestinationFactor := DestinationFactor;
    UpdateBlendingFunction;
  end;
end;

procedure TRenderContext.BlendingDisable;
begin
  if FBlendingEnabled then
  begin
    FBlendingEnabled := false;
    UpdateBlendingEnabled;
  end;
end;

procedure TRenderContext.UpdateBlendingEnabled;
begin
  if SkipOpenGLStateWhenRHIActive then
    Exit;
  GLSetEnabled(GL_BLEND, FBlendingEnabled);
end;

procedure TRenderContext.UpdateBlendingFunction;
begin
  if SkipOpenGLStateWhenRHIActive then
    Exit;
  {$warnings off} // using deprecated routine that should be internal here
  GLBlendFunction(FBlendingSourceFactor, FBlendingDestinationFactor);
  {$warnings on}
end;

procedure TRenderContext.SetBlendingConstantColor(const Value: TCastleColor);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  FBlendingConstantColor := Value;
  UpdateBlendingConstantColor;
end;

procedure TRenderContext.UpdateBlendingConstantColor;
begin
  if RHIDevice <> nil then
  begin
    FRHILastBlendConstantColor := FBlendingConstantColor;
    Inc(FRHIBlendConstantColorUpdateCount);
    if ActiveCommandList <> nil then
      ActiveCommandList.SetBlendConstants(FBlendingConstantColor);
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  if (GLFeatures <> nil) and GLFeatures.BlendConstant then
    glBlendColor(FBlendingConstantColor.X, FBlendingConstantColor.Y,
      FBlendingConstantColor.Z, FBlendingConstantColor.W);
end;

procedure TRenderContext.FixedFunctionAlphaTestEnable(const AlphaCutoff: Single = 0.5);
begin
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
  begin
    if not FFixedFunctionAlphaTest then
    begin
      FFixedFunctionAlphaTest := true;
      UpdateFixedFunctionAlphaTest;
    end;

    if FFixedFunctionAlphaCutoff <> AlphaCutoff then
    begin
      FFixedFunctionAlphaCutoff := AlphaCutoff;
      UpdateFixedFunctionAlphaCutoff;
    end;
  end;
end;

procedure TRenderContext.FixedFunctionAlphaTestDisable;
begin
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
  begin
    if FFixedFunctionAlphaTest then
    begin
      FFixedFunctionAlphaTest := false;
      UpdateFixedFunctionAlphaTest;
    end;
  end;
end;

procedure TRenderContext.UpdateFixedFunctionAlphaTest;
begin
  {$ifndef OpenGLES}
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
    GLSetEnabled(GL_ALPHA_TEST, FFixedFunctionAlphaTest);
  {$endif}
end;

procedure TRenderContext.UpdateFixedFunctionAlphaCutoff;
begin
  {$ifndef OpenGLES}
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
    glAlphaFunc(GL_GEQUAL, FFixedFunctionAlphaCutoff);
  {$endif}
end;

procedure TRenderContext.SetFixedFunctionLighting(const Value: boolean);
begin
  if FFixedFunctionLighting <> Value then
  begin
    FFixedFunctionLighting := Value;
    UpdateFixedFunctionLighting;
  end;
end;

procedure TRenderContext.UpdateFixedFunctionLighting;
begin
  {$ifndef OpenGLES}
  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
    GLSetEnabled(GL_LIGHTING, FFixedFunctionLighting);
  {$endif}
end;

class function TRenderContext.LineTypeToStipplePattern(const ALineType: TLineType): Word;
begin
  case ALineType of
    ltSolid       : Result := $FFFF;
    ltDashed      : Result := $00FF;
    ltDotted      : Result := $CCCC;
    ltDashedDotted: Result := $FFCC;
    ltDashDotDot  : Result := $FCCC;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else Result := $FFFF;
    {$endif}
  end;
end;

procedure TRenderContext.SetLineType(const Value: TLineType);
begin
  if FLineType <> Value then
  begin
    FLineType := Value;
    UpdateLineType;
  end;
end;

procedure TRenderContext.UpdateLineType;
begin
  if RHIDevice <> nil then
  begin
    FRHILastLineType := FLineType;
    FRHILastLineStipplePattern := LineTypeToStipplePattern(FLineType);
    Inc(FRHILineTypeUpdateCount);
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;
  {$ifndef OpenGLES}
  case FLineType of
    ltSolid: glDisable(GL_LINE_STIPPLE);
    ltDashed      : begin glLineStipple(1, $00FF); glEnable(GL_LINE_STIPPLE); end;
    ltDotted      : begin glLineStipple(1, $CCCC); glEnable(GL_LINE_STIPPLE); end;
    ltDashedDotted: begin glLineStipple(1, $FFCC); glEnable(GL_LINE_STIPPLE); end;
    ltDashDotDot  : begin glLineStipple(1, $FCCC); glEnable(GL_LINE_STIPPLE); end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('LineType?');
    {$endif}
  end;
  {$endif}
end;

procedure TRenderContext.SetPolygonOffset(const Value: TPolygonOffset);
begin
  if (FPolygonOffset.Enabled <> Value.Enabled) or
     (FPolygonOffset.Scale <> Value.Scale) or
     (FPolygonOffset.Bias <> Value.Bias) then
  begin
    FPolygonOffset := Value;
    UpdatePolygonOffset;
  end;
end;

procedure TRenderContext.UpdatePolygonOffset;
begin
  if RHIDevice <> nil then
  begin
    Inc(FRHIDepthBiasUpdateCount);
    if ActiveCommandList <> nil then
    begin
      if FPolygonOffset.Enabled then
        ActiveCommandList.SetDepthBias(FPolygonOffset.Bias, FPolygonOffset.Scale)
      else
        ActiveCommandList.SetDepthBias(0, 0);
    end;
    if SkipOpenGLStateWhenRHIActive then
      Exit;
  end;

  if FPolygonOffset.Enabled then
  begin
    glEnable(GL_POLYGON_OFFSET_FILL);
    {$ifndef OpenGLES} // These do not exist on OpenGLES
    glEnable(GL_POLYGON_OFFSET_LINE);
    glEnable(GL_POLYGON_OFFSET_POINT);
    {$endif}
    glPolygonOffset(FPolygonOffset.Scale, FPolygonOffset.Bias);
  end else
  begin
    glDisable(GL_POLYGON_OFFSET_FILL);
    {$ifndef OpenGLES} // These do not exist on OpenGLES
    glDisable(GL_POLYGON_OFFSET_LINE);
    glDisable(GL_POLYGON_OFFSET_POINT);
    {$endif}
  end;
end;

procedure TRenderContext.PolygonOffsetEnable(const Scale, Bias: Single);
var
  NewState: TPolygonOffset;
begin
  NewState.Enabled := true;
  NewState.Scale := Scale;
  NewState.Bias := Bias;
  PolygonOffset := NewState;
end;

procedure TRenderContext.PolygonOffsetDisable;
var
  NewState: TPolygonOffset;
begin
  NewState.Enabled := false;
  { Whatever, will not be set anyway. }
  NewState.Scale := 0;
  NewState.Bias := 0;
  PolygonOffset := NewState;
end;

function TRenderContext.GetBoundBuffer(const Target: TBufferTarget): TGLBuffer;
begin
  Result := FBoundBuffer[Target];
end;

procedure TRenderContext.SetBoundBuffer(const Target: TBufferTarget; const Value: TGLBuffer);
begin
  FBoundBuffer[Target] := Value;
  if SkipOpenGLStateWhenRHIActive then
    Exit;

  { TODO: Optimization (avoiding glBindBuffer) disabled.
    Reason: castle-model-viewer (rock.gltf, triangulatio.x3dv) shows
    that we cannot optimize out glBindBuffer calls like this,
    it's causing crashes (Linux "guardia" system, Nvidia GPU).

  if FBoundBuffer[Target] <> Value then
  begin
    FBoundBuffer[Target] := Value;
    glBindBuffer(BufferTargetGL[Target], Value);
  end;
  }

  glBindBuffer(BufferTargetGL[Target], Value);
end;

procedure TRenderContext.SetInternalControlRectIgnore(const Value: Cardinal);
begin
  if FInternalControlRectIgnore <> Value then
  begin
    FInternalControlRectIgnore := Value;
    FEnabledScissors.Update;
    UpdateViewport;
  end;
end;

procedure TRenderContext.SynchronizeState;
var
  S: TScissor;
begin
  // mark all scissors as disabled (in our memory, and in OpenGL(ES) state)
  for S in FEnabledScissors do
    S.FEnabled := false; // set using internal field
  FEnabledScissors.Clear; // FEnabledScissors should only contain instances with FEnabled=true
  FEnabledScissors.Update; // will call glDisable(GL_SCISSOR_TEST)

  UpdateClearColor;
  UpdateLineWidth;
  UpdatePointSize;
  UpdateGlobalAmbient;
  UpdateProjectionMatrix;
  UpdateDepthRange;
  UpdateCullFace;
  UpdateFrontFace;
  UpdateDepthTest;
  UpdateDepthFunc;
  UpdateColorChannels;
  UpdateDepthBufferUpdate;
  UpdateViewport; // accounts for both FViewport and FViewportDelta
  UpdateCurrentProgram;
  UpdateCurrentVao;
  UpdateBlendingEnabled;
  UpdateBlendingFunction;
  UpdateBlendingConstantColor;
  UpdateFixedFunctionAlphaTest;
  UpdateFixedFunctionAlphaCutoff;
  UpdateFixedFunctionLighting;
  UpdateLineType;
  UpdatePolygonOffset;
  { BindBuffer doesn't need synchronization:
    SetBoundBuffer always calls glBindBuffer, not relying on FBoundBuffer. }
end;

{ TRenderContext.TScissorList ------------------------------------------------------------------- }

procedure TRenderContext.TScissorList.Update;
var
  R: TRectangle;
  I: Integer;
  UseControlRect: Boolean;
begin
  // we need to flush batched things, before scissor change
  TDrawableImage.BatchingFlush;

  UseControlRect :=
    (ParentContext.InternalControlRectIgnore = 0) and
    (not ParentContext.InternalControlRect.IsEmpty);
  R := TRectangle.Empty;

  if Count <> 0 then
  begin
    R := Items[0].Rect;
    for I := 1 to Count - 1 do
      R := R * Items[I].Rect;
    FFinalScissor := R;

    { Apply InternalControlRect after FFinalScissor is set,
      so that FFinalScissor is not affected by the InternalControlRect value. }
    if UseControlRect then
    begin
      R.Left   := R.Left   + ParentContext.InternalControlRect.Left;
      R.Bottom := R.Bottom + ParentContext.InternalControlRect.Bottom;
      R := R * ParentContext.InternalControlRect;
    end;
  end else
  if UseControlRect then
    R := ParentContext.InternalControlRect;

  if (Count <> 0) or UseControlRect then
  begin
    if RHIDevice <> nil then
    begin
      Inc(ParentContext.FRHIScissorUpdateCount);
      ParentContext.FRHILastScissor := R;
      ParentContext.FRHIScissorActive := True;
      if ActiveCommandList <> nil then
        ActiveCommandList.SetScissor(R);
    end;
    if not SkipOpenGLStateWhenRHIActive then
    begin
      glScissor(R.Left, R.Bottom, R.Width, R.Height);
      glEnable(GL_SCISSOR_TEST);
    end;
  end else
  begin
    if RHIDevice <> nil then
    begin
      Inc(ParentContext.FRHIScissorUpdateCount);
      ParentContext.FRHILastScissor := ParentContext.EffectiveViewportRect;
      ParentContext.FRHIScissorActive := False;
      if ActiveCommandList <> nil then
        ActiveCommandList.SetScissor(ParentContext.FRHILastScissor);
    end;
    if not SkipOpenGLStateWhenRHIActive then
      glDisable(GL_SCISSOR_TEST);
  end;
end;

{ TScissor ------------------------------------------------------------------- }

constructor TScissor.Create;
begin
  inherited;
  Rect := TRectangle.Empty;
end;

destructor TScissor.Destroy;
begin
  Enabled := false;
  inherited;
end;

procedure TScissor.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if (RenderContext <> nil) and (RenderContext.FEnabledScissors <> nil) then
    begin
      if Value then
        RenderContext.FEnabledScissors.Add(Self) else
        RenderContext.FEnabledScissors.Remove(Self);
      RenderContext.FEnabledScissors.Update;
    end;
  end;
end;

{ projection matrix ---------------------------------------------------------- }

function PerspectiveProjection(const fovy, aspect, ZNear, ZFar: Single): TMatrix4;
begin
  Result := PerspectiveProjectionMatrixDeg(fovy, aspect, ZNear, ZFar);
  if RenderContext <> nil then
    RenderContext.ProjectionMatrix := Result;
end;

function OrthoProjection(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;
begin
  Result := OrthoProjectionMatrix(Dimensions, ZNear, ZFar);
  if RenderContext <> nil then
    RenderContext.ProjectionMatrix := Result;
end;

function FrustumProjection(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;
begin
  Result := FrustumProjectionMatrix(Dimensions, ZNear, ZFar);
  if RenderContext <> nil then
    RenderContext.ProjectionMatrix := Result;
end;

end.
