{
  Copyright 2001-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: TRenderContext and friends. }

{$ifdef read_interface}

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

  TColorChannel = 0..3;
  TColorChannels = set of TColorChannel;

  { The OpenGL / OpenGLES context state.
    We try hard to make this a @bold(very, very) small class,
    because usually it's better to introduce a clean higher-level API
    than to track the OpenGL context state in a simple global @link(RenderContext)
    instance.

    Use the methods and properties of this class
    only when this context is @italic(current),
    which means it's set as @link(RenderContext) value.

    Do not depend on the context state being persistent.
    The @link(RenderContext) does not change during a single TUIControl.OnRender
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
      TScissorList = class(specialize TObjectList<TScissor>)
      public
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
      FColorChannels: TColorChannels;
      FViewport: TRectangle;
      procedure SetLineWidth(const Value: Single);
      procedure SetPointSize(const Value: Single);
      procedure SetGlobalAmbient(const Value: TCastleColorRGB);
      procedure WarnContextNotCurrent;
      procedure SetProjectionMatrix(const Value: TMatrix4);
      procedure SetDepthRange(const Value: TDepthRange);
      procedure SetCullFace(const Value: boolean);
      procedure SetFrontFaceCcw(const Value: boolean);
      function GetColorMask: boolean;
      procedure SetColorMask(const Value: boolean);
      procedure SetColorChannels(const Value: TColorChannels);
      procedure SetViewport(const Value: TRectangle);
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
    property LineWidth: Single read FLineWidth write SetLineWidth default 1;

    { The rendered point size.
      Never call OpenGL glPointSize directly.

      Do not access this property directly, unless you make direct
      OpenGL/OpenGLES calls. In normal circumstances, engine API
      (like DrawPrimitive2D or TCastleScene) set this automatically. }
    property PointSize: Single read FPointSize write SetPointSize default 1;

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

    { Should we use backface-culling (ignore some faces during rendering).
      This controls whether OpenGL GL_CULL_FACE flag is enabled or not. }
    property CullFace: boolean read FCullFace write SetCullFace
      default false;

    { Is the front face ordered counter-clockwise.
      The "front face" is important to interpreting the @link(CullFace)
      and to interpret the normal vectors (they point outward from front face). }
    property FrontFaceCcw: boolean read FFrontFaceCcw write SetFrontFaceCcw
      default true;

    { Which color buffer channels are touched by rendering. }
    property ColorMask: boolean
      read GetColorMask write SetColorMask default true;
      deprecated 'use ColorChannels';

    { Which color buffer channels are touched by rendering.
      This affects all rendering, including @link(TGLImage.Draw).

      Note that this state may be internally modified by various engine rendering
      functions too, e.g. by @link(TCastleScene.LocalRender), depending on rendering
      attributes.
      So it is only reliable for you to modify this temporarily,
      and during your own rendering.
      E.g. change ColorChannels right before and right after calling a couple of
      @link(TGLImage.Draw). Always restore it afterwards to default [0..3].

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

    { Setting this sets OpenGL glViewport. }
    property Viewport: TRectangle read FViewport write SetViewport;
  end;

var
  { Current OpenGL / OpenGLES context state.
    @bold(Only access it during the rendering, i.e. in TUIControl.Render.)

    TODO: In the future, this global singleton may be removed,
    and this may be accessible instead through a new TUIControl.Render parameter. }
  RenderContext: TRenderContext;

procedure GLClear(const Buffers: TClearBuffers;
  const ClearColor: TCastleColor); deprecated 'use RenderContext.Clear';

{ Projection matrix -------------------------------------------------------- } { }

{$ifdef CASTLE_OBJFPC}
function GetProjectionMatrix: TMatrix4;
  deprecated 'use RenderContext.ProjectionMatrix';
procedure SetProjectionMatrix(const Value: TMatrix4);
  deprecated 'use RenderContext.ProjectionMatrix';

{ Current projection matrix.
  @deprecated Use RenderContext.ProjectionMatrix instead. }
property ProjectionMatrix: TMatrix4
  read GetProjectionMatrix write SetProjectionMatrix;
{$endif}

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

{ depth range ---------------------------------------------------------------- } { }

{$ifdef CASTLE_OBJFPC}
function GetDepthRange: TDepthRange;
  deprecated 'use RenderContext.DepthRange';
procedure SetDepthRange(const Value: TDepthRange);
  deprecated 'use RenderContext.DepthRange';
{ @deprecated Use @link(TRenderContext.DepthRange RenderContext.DepthRange) instead. }
property DepthRange: TDepthRange read GetDepthRange write SetDepthRange;
{$endif CASTLE_OBJFPC}

{$endif read_interface}

{$ifdef read_implementation}

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
  FViewport := TRectangle.Empty;
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
    glClearColor(FClearColor[0], FClearColor[1], FClearColor[2], FClearColor[3]);
  end;
  Mask := 0;
  for B in Buffers do
    Mask := Mask or ClearBufferMask[B];
  if Mask <> 0 then
    {$ifndef OpenGLES} GL {$else} CastleGLES20 {$endif}.GLClear(Mask);
end;

procedure TRenderContext.SetLineWidth(const Value: Single);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    glLineWidth(Value);
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

procedure TRenderContext.SetViewport(const Value: TRectangle);
begin
  if Self <> RenderContext then
    WarnContextNotCurrent;

  if not FViewport.Equals(Value) then
  begin
    FViewport := Value;
    {$ifndef OpenGLES} GL {$else} CastleGLES20 {$endif}
      .glViewport(Value.Left, Value.Bottom, Value.Width, Value.Height);
  end;
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

{ GLClear -------------------------------------------------------------------- }

procedure GLClear(const Buffers: TClearBuffers; const ClearColor: TCastleColor);
begin
  RenderContext.Clear(Buffers, ClearColor);
end;

{ projection matrix ---------------------------------------------------------- }

{$ifdef CASTLE_OBJFPC}
function GetProjectionMatrix: TMatrix4;
begin
  Result := RenderContext.ProjectionMatrix;
end;

procedure SetProjectionMatrix(const Value: TMatrix4);
begin
  RenderContext.ProjectionMatrix := Value;
end;
{$endif}

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

{ depth range ---------------------------------------------------------------- }

{$ifdef CASTLE_OBJFPC}
function GetDepthRange: TDepthRange;
begin
  Result := RenderContext.DepthRange;
end;

procedure SetDepthRange(const Value: TDepthRange);
begin
  RenderContext.DepthRange := Value;
end;
{$endif CASTLE_OBJFPC}

{$endif read_implementation}
