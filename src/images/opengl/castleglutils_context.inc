{
  Copyright 2001-2017 Michalis Kamburelis.

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

  { The OpenGL / OpenGLES context state.
    We try hard to make this a @bold(very, very) small class,
    because usually it's better to introduce a clean higher-level API
    than to track the OpenGL context state in a simple global @link(RenderContext)
    instance.

    Use the methods and properties of this class
    only when this context is @italic(current),
    which means it's set as @link(RenderContext) value.

    Do not depend on the context state being persistent.
    The @link(RenderContext) is constant during a single TUIControl.OnRender
    method, and that's all we guarantee. On desktops, you control the context
    creation / destruction explicitly (by opening / closing the TCastleWindow),
    but on mobile devices -- the context may get destroyed and created at almost
    any moment. }
  TRenderContext = class
  strict private
    type
      TScissorList = class(TFPObjectList)
      //class(specialize TFPGObjectList<TScissor>) // see comments in "uses" clause -- we cannot use FGL unit here
      public
        procedure Update;
      end;

    var
    FClearColor: TCastleColor;
    FLineWidth, FPointSize: Single;
    FGlobalAmbient: TCastleColorRGB;
    FGlobalScissor: TScissor;
    procedure SetLineWidth(const Value: Single);
    procedure SetPointSize(const Value: Single);
    procedure SetGlobalAmbient(const Value: TCastleColorRGB);
    procedure WarnContextNotCurrent;
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
  end;

var
  { Current OpenGL / OpenGLES context state.
    @bold(Only access it during the rendering, i.e. in TUIControl.Render.) }
  RenderContext: TRenderContext;

procedure GLClear(const Buffers: TClearBuffers;
  const ClearColor: TCastleColor); deprecated 'use RenderContext.Clear';

{$endif read_interface}

{$ifdef read_implementation}

constructor TRenderContext.Create;
begin
  inherited;
  FLineWidth := 1;
  FPointSize := 1;
  FGlobalAmbient := Vector3Single(0.2, 0.2, 0.2);
  FEnabledScissors := TScissorList.Create(false);
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

  if not VectorsPerfectlyEqual(FClearColor, ClearColor) then
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

  if not VectorsPerfectlyEqual(FGlobalAmbient, Value) then
  begin
    FGlobalAmbient := Value;

    {$ifndef OpenGLES}
    { We always set "1" as global ambient alpha.
      This alpha does not have any useful interpretation, it seems,
      so don't let it change. }
    glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(FGlobalAmbient, 1));
    {$endif}
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

{ TRenderContext.TScissorList ------------------------------------------------------------------- }

procedure TRenderContext.TScissorList.Update;
var
  R: TRectangle;
  I: Integer;
begin
  if Count <> 0 then
  begin
    R := TScissor(Items[0]).Rect;
    for I := 1 to Count - 1 do
      R := R * TScissor(Items[I]).Rect;
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

{ globals -------------------------------------------------------------------- }

procedure GLClear(const Buffers: TClearBuffers; const ClearColor: TCastleColor);
begin
  RenderContext.Clear(Buffers, ClearColor);
end;

{$endif read_implementation}
