{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
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

uses SysUtils, Generics.Collections,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleUtils, CastleVectors, CastleRectangles, CastleGLShaders, CastleColors,
  CastleRenderOptions, CastleGLUtils;

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
    FEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    { Rectangle to which we clip rendering. Empty by default (will clip everything,
      if you don't assign this!). Do not change this when scissor is enabled. }
    Rect: TRectangle;
    constructor Create;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

  { Possible values of @link(TRenderContext.DepthRange). }
  TDepthRange = (drFull, drNear, drFar);

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

  TColorChannel = 0..3;
  TColorChannels = set of TColorChannel;

  TPolygonOffset = record
    Enabled: Boolean;
    Scale, Bias: Single;
  end;

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
  TRenderContext = class
  strict private
    type
      TScissorList = class({$ifdef FPC}specialize{$endif} TObjectList<TScissor>)
      public
        FFinalScissor: TRectangle; // only defined when Count <> 0
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

    procedure SetLineWidth(const Value: Single);
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
  private
    FEnabledScissors: TScissorList;
  public
    constructor Create;
    destructor Destroy; override;

    { Clear the whole buffer contents.

      Never call OpenGL glClear or glClearColor, always use this method. }
    procedure Clear(const Buffers: TClearBuffers; const ClearColor: TCastleColor);

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
      read FColorChannels write SetColorChannels default [0..3];

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

implementation

uses CastleLog, CastleProjection, CastleInternalGLUtils;

constructor TRenderContext.Create;
begin
  inherited;
  FLineWidth := 1;
  FPointSize := 1;
  FGlobalAmbient := Vector3(0.2, 0.2, 0.2);
  FEnabledScissors := TScissorList.Create(false);
  FProjectionMatrix := TMatrix4.Identity;
  FDepthRange := drFull;
  FCullFace := false;
  FFrontFaceCcw := true;
  FColorChannels := [0..3];
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

procedure TRenderContext.Clear(const Buffers: TClearBuffers;
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

  if (cbColor in Buffers) and
     not TVector4.PerfectlyEquals(FClearColor, ClearColor) then
  begin
    FClearColor := ClearColor;
    glClearColor(FClearColor.X, FClearColor.Y, FClearColor.Z, FClearColor.W);
  end;
  Mask := 0;
  for B in Buffers do
    Mask := Mask or ClearBufferMask[B];
  if Mask <> 0 then
    {$ifndef OpenGLES} {$ifdef FPC} GL {$else} OpenGL {$endif} {$else} CastleGL {$endif}.GLClear(Mask);
end;

procedure TRenderContext.SetLineWidth(const Value: Single);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    {$if (not defined(DARWIN)) or defined(OpenGLES)}
    // on desktop macOS, glLineWidth raise GL_INVALID_VALUE for parameters <> 1.0
    glLineWidth(Value);
    {$endif}
  end;
end;

procedure TRenderContext.SetPointSize(const Value: Single);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FPointSize <> Value then
  begin
    FPointSize := Value;

    { Not possible with OpenGL ES.
      See http://stackoverflow.com/questions/9381562/using-gl-points-in-glkit-ios-5
      http://www.idevgames.com/forums/thread-3.html :
      "You must write gl_PointSize in the vertex shader, per point." }

    {$ifndef OpenGLES}
    glPointSize(Value);
    {$endif}
  end;
end;

procedure TRenderContext.SetGlobalAmbient(const Value: TCastleColorRGB);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if not TVector3.PerfectlyEquals(FGlobalAmbient, Value) then
  begin
    FGlobalAmbient := Value;

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      { We always set "1" as global ambient alpha.
        This alpha does not have any useful interpretation, it seems,
        so don't let it change. }
      glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4(FGlobalAmbient, 1));
      {$endif}
    end;
  end;
end;

procedure TRenderContext.ScissorEnable(const Rect: TRectangle);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FGlobalScissor = nil then
    FGlobalScissor := TScissor.Create else
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

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    glMatrixMode(GL_PROJECTION);
    {$warnings off}
    glLoadMatrix(Value); // consciously using deprecated stuff; this should be internal in this unit
    {$warnings on}
    glMatrixMode(GL_MODELVIEW);
    {$endif}
  end;
end;


procedure TRenderContext.SetDepthRange(const Value: TDepthRange);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthRange <> Value then
  begin
    {$ifdef OpenGLES} {$define glDepthRange := glDepthRangef} {$endif}
    FDepthRange := Value;
    case Value of
      drFull: glDepthRange(0  , 1);
      drNear: glDepthRange(0  , 0.1);
      drFar : glDepthRange(0.1, 1);
    end;
  end;
end;

procedure TRenderContext.SetCullFace(const Value: boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FCullFace <> Value then
  begin
    FCullFace := Value;
    GLSetEnabled(GL_CULL_FACE, FCullFace);
  end;
end;

procedure TRenderContext.SetFrontFaceCcw(const Value: boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FFrontFaceCcw <> Value then
  begin
    FFrontFaceCcw := Value;
    if Value then
      glFrontFace(GL_CCW)
    else
      glFrontFace(GL_CW);
  end;
end;

procedure TRenderContext.SetDepthTest(const Value: Boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthTest <> Value then
  begin
    FDepthTest := Value;
    GLSetEnabled(GL_DEPTH_TEST, FDepthTest);
  end;
end;

procedure TRenderContext.SetDepthFunc(const Value: TDepthFunction);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthFunc <> Value then
  begin
    FDepthFunc := Value;
    glDepthFunc(Ord(FDepthFunc));
  end;
end;

function TRenderContext.GetColorMask: boolean;
begin
  Result := ColorChannels <> [];
end;

procedure TRenderContext.SetColorMask(const Value: boolean);
begin
  if Value then
    ColorChannels := [0..3]
  else
    ColorChannels := [];
end;

procedure TRenderContext.SetColorChannels(const Value: TColorChannels);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FColorChannels <> Value then
  begin
    FColorChannels := Value;
    glColorMask(
      BoolToGL(0 in FColorChannels),
      BoolToGL(1 in FColorChannels),
      BoolToGL(2 in FColorChannels),
      BoolToGL(3 in FColorChannels)
    );
  end;
end;

procedure TRenderContext.SetDepthBufferUpdate(const Value: boolean);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FDepthBufferUpdate <> Value then
  begin
    FDepthBufferUpdate := Value;
    glDepthMask(BoolToGL(Value));
  end;
end;

procedure TRenderContext.UpdateViewport;
begin
  {$ifndef OpenGLES} {$ifdef FPC} GL {$else} OpenGL {$endif} {$else} CastleGL {$endif}
    .glViewport(
      FViewport.Left   + FViewportDelta.X,
      FViewport.Bottom + FViewportDelta.Y,
      FViewport.Width,
      FViewport.Height);
end;

procedure TRenderContext.WarningViewportTooLarge;
begin
  if not WarningViewportTooLargeDone then
  begin
    WritelnWarning('Setting viewport %s, with has dimensions larger than maximum allowed %s. (Further warnings of the same type will not be shown.)', [
      FViewport.ToString,
      GLFeatures.MaxViewportDimensions.ToString
    ]);
    WarningViewportTooLargeDone := true;
  end;
end;

procedure TRenderContext.SetViewport(const Value: TRectangle);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if not FViewport.Equals(Value) then
  begin
    if (GLFeatures <> nil) and
       ((FViewport.Width > GLFeatures.MaxViewportDimensions.X) or
        (FViewport.Height > GLFeatures.MaxViewportDimensions.Y)) then
      WarningViewportTooLarge;

    FViewport := Value;
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
    InternalSetCurrentProgram(Value);
  end;
end;

procedure TRenderContext.SetCurrentVao(const Value: TVertexArrayObject);
begin
  if FCurrentVao <> Value then
  begin
    FCurrentVao := Value;
    if GLFeatures.VertexArrayObject then
    begin
      if Value <> nil then
        glBindVertexArray(Value.InternalHandle)
      else
        glBindVertexArray(0);
    end;
  end;
end;

function TRenderContext.ColorBufferHasAlpha: Boolean;
begin
  Result := GLFeatures.AlphaBits > 0;
end;

procedure TRenderContext.BlendingEnable(
  const SourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const DestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha);
begin
  if not FBlendingEnabled then
  begin
    FBlendingEnabled := true;
    glEnable(GL_BLEND);
  end;

  if (FBlendingSourceFactor <> SourceFactor) or
     (FBlendingDestinationFactor <> DestinationFactor) then
  begin
    FBlendingSourceFactor := SourceFactor;
    FBlendingDestinationFactor := DestinationFactor;
    {$warnings off} // using deprecated routine that should be internal here
    GLBlendFunction(SourceFactor, DestinationFactor);
    {$warnings on}
  end;
end;

procedure TRenderContext.BlendingDisable;
begin
  if FBlendingEnabled then
  begin
    FBlendingEnabled := false;
    glDisable(GL_BLEND);
  end;
end;

procedure TRenderContext.FixedFunctionAlphaTestEnable(const AlphaCutoff: Single = 0.5);
begin
  if GLFeatures.EnableFixedFunction then
  begin
    if not FFixedFunctionAlphaTest then
    begin
      FFixedFunctionAlphaTest := true;
      {$ifndef OpenGLES}
      glEnable(GL_ALPHA_TEST);
      {$endif}
    end;

    if FFixedFunctionAlphaCutoff <> AlphaCutoff then
    begin
      FFixedFunctionAlphaCutoff := AlphaCutoff;
      {$ifndef OpenGLES}
      glAlphaFunc(GL_GEQUAL, AlphaCutoff);
      {$endif}
    end;
  end;
end;

procedure TRenderContext.FixedFunctionAlphaTestDisable;
begin
  if GLFeatures.EnableFixedFunction then
  begin
    if FFixedFunctionAlphaTest then
    begin
      FFixedFunctionAlphaTest := false;
      {$ifndef OpenGLES}
      glDisable(GL_ALPHA_TEST);
      {$endif}
    end;
  end;
end;

procedure TRenderContext.SetFixedFunctionLighting(const Value: boolean);
begin
  if FFixedFunctionLighting <> Value then
  begin
    FFixedFunctionLighting := Value;
    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
      GLSetEnabled(GL_LIGHTING, Value);
    {$endif}
  end;
end;

procedure TRenderContext.SetLineType(const Value: TLineType);
begin
  if FLineType <> Value then
  begin
    FLineType := Value;
    {$ifndef OpenGLES}
    case LineType of
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
end;

procedure TRenderContext.SetPolygonOffset(const Value: TPolygonOffset);
begin
  if (FPolygonOffset.Enabled <> Value.Enabled) or
     (FPolygonOffset.Scale <> Value.Scale) or
     (FPolygonOffset.Bias <> Value.Bias) then
  begin
    FPolygonOffset := Value;
    if Value.Enabled then
    begin
      glEnable(GL_POLYGON_OFFSET_FILL);
      {$ifndef OpenGLES} // These do not exist on OpenGLES
      glEnable(GL_POLYGON_OFFSET_LINE);
      glEnable(GL_POLYGON_OFFSET_POINT);
      {$endif}
      glPolygonOffset(Value.Scale, Value.Bias);
    end else
    begin
      glDisable(GL_POLYGON_OFFSET_FILL);
      {$ifndef OpenGLES} // These do not exist on OpenGLES
      glDisable(GL_POLYGON_OFFSET_LINE);
      glDisable(GL_POLYGON_OFFSET_POINT);
      {$endif}
    end;
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

{ TRenderContext.TScissorList ------------------------------------------------------------------- }

procedure TRenderContext.TScissorList.Update;
var
  R: TRectangle;
  I: Integer;
begin
  if Count <> 0 then
  begin
    R := Items[0].Rect;
    for I := 1 to Count - 1 do
      R := R * Items[I].Rect;
    FFinalScissor := R;
    glScissor(R.Left, R.Bottom, R.Width, R.Height);
    glEnable(GL_SCISSOR_TEST);
  end else
    glDisable(GL_SCISSOR_TEST);
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
    if RenderContext.FEnabledScissors <> nil then
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
  RenderContext.ProjectionMatrix := Result;
end;

function OrthoProjection(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;
begin
  Result := OrthoProjectionMatrix(Dimensions, ZNear, ZFar);
  RenderContext.ProjectionMatrix := Result;
end;

function FrustumProjection(const Dimensions: TFloatRectangle; const ZNear, ZFar: Single): TMatrix4;
begin
  Result := FrustumProjectionMatrix(Dimensions, ZNear, ZFar);
  RenderContext.ProjectionMatrix := Result;
end;

end.
