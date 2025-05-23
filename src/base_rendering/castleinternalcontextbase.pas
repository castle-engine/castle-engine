{
  Copyright 2013-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cross-platform OpenGL(ES) context types. }
unit CastleInternalContextBase;

{$i castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleVectors, CastleRenderOptions;

type
  EGLContextNotPossible = class(Exception);

  { Required OpenGL(ES) context capabilities.
    These context requirements are cross-platform,
    i.e. they should make sense for all ways how one can create OpenGL context
    (EGL, wgl, glX, etc.) }
  TGLContextRequirements = class(TComponent)
  strict private
    FDoubleBuffer: Boolean;
    FRedBits, FGreenBits, FBlueBits: Cardinal;
    FDepthBits: Cardinal;
    FAlphaBits: Cardinal;
    FStencilBits: Cardinal;
    FMultiSampling: Cardinal;
    function GetColorBits: Cardinal;
    procedure SetColorBits(const Value: Cardinal);
  public
    const
      DefaultDepthBits = 24;

    constructor Create(AOwner: TComponent); override;

    { Check do given OpenGL buffers configuration satisfies the
      requested configuration.

      So it checks does

      @preformatted(
        ProvidedStencilBits >= StencilBits and
        ProvidedDepthBits >= DepthBits ...
      )

      and so on. If not, EGLContextNotPossible is raised with detailed
      description (which buffer constraint is not satisfied -- e.g. maybe
      the stencil buffer is not available).

      Note that ProvidedMultiSampling is not checked if MultiSampling is <= 1.
      In other words, if multisampling was not required, ProvidedMultiSampling
      doesn't matter --- it's Ok even ProvidedMultiSampling = 0 and
      MultiSampling = 1, which happens commonly (since our MultiSampling = 1 means
      "no multisampling" and is default, but most backends returns num_samples
      (or something equivalent) as = 0 when multisampling not supported). }
    procedure CheckRequestedBufferAttributes(const ProviderName: string;
      ProvidedStencilBits, ProvidedDepthBits, ProvidedAlphaBits,
      ProvidedMultiSampling: Cardinal);

    { Current OpenGL buffers configuration required.
      Stuff like DoubleBuffer, AlphaBits, DepthBits, StencilBits etc.
      This simply returns a text description of these properties.

      It does not describe the current OpenGL context parameters.
      (It doesn't even need an OpenGL context open.)

      Useful for constructing messages e.g. for EGLContextNotPossible exceptions. }
    function RequestedBufferAttributes: String;
  published
    { Should we request and use the double buffer.
      After every draw, we automatically swap buffers (if DoubleBuffer)
      or call glFlush (if not DoubleBuffer). }
    property DoubleBuffer: Boolean read FDoubleBuffer write FDoubleBuffer default true;

    { Required red / green / blue color buffer precision for this window.
      When 0, the default window system color precision will be used.

      You can either set them by separate red / green / blue properties.
      Or you can use ColorBits that reads / writes all three channels bits.
      Reading ColorBits simply returns the sum of
      @code(RedBits + GreenBits + BlueBits).
      Writing ColorBits simply set RedBits and BlueBits to
      @code(ColorBits div 3), and sets GreenBits to the remainder.
      This way green channel has always the best resolution (as is usual,
      since it's perceived most), and the sum is always as requested.
      This way setting ColorBits to values like 16 or 24 works as expected.

      @groupBegin }
    property RedBits: Cardinal read FRedBits write FRedBits default 0;
    property GreenBits: Cardinal read FGreenBits write FGreenBits default 0;
    property BlueBits: Cardinal read FBlueBits write FBlueBits default 0;
    property ColorBits: Cardinal read GetColorBits write SetColorBits stored false default 0;
    { @groupEnd }

    { Required depth buffer precision. Zero means that we don't need
      depth buffer at all. We may get depth buffer with more precision
      than requested (we may even get depth buffer when we set
      DepthBits = 0), this all depends on graphic card.

      Default value is @link(DefaultDepthBits),
      which is non-zero and a reasonable default for 3D programs
      that want to work with depth test enabled.

      @italic(Design notes:) Why default value is not 0?

      @orderedList(
        @item(
          Most programs using OpenGL use 3D and so use depth testing.
          So many programs
          would have to call something like @code(Window.DepthBits := DefaultDepthBits).
        )

        @item(
          Often graphic cards / window systems / OSes give you an OpenGL
          context with depth buffer @italic(even if you don't need depth buffer).
          This makes it easy to forget about setting DepthBits to something
          non-zero, because on @italic(your) system you may happen
          to always get some depth buffer.
        )
      )

      If you are writing a program that does not need depth buffer
      you can set Window.DepthBits := 0, to inform OpenGL it doesn't need
      to allocate any depth buffer.
    }
    property DepthBits: Cardinal read FDepthBits write FDepthBits default DefaultDepthBits;

    { Required number of bits in alpha channel of color buffer.
      Zero means that alpha channel is not needed.

      Just like with other XxxBits property, we may get more
      bits than we requested. But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      we will raise an error. }
    property AlphaBits: Cardinal read FAlphaBits write FAlphaBits default 0;

    { Required stencil buffer precision, zero means that stencil buffer is
      not needed.

      Just like with other XxxBits property, we may get more
      bits than we requested. But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      we will raise an error.

      Note that after initializing OpenGL context (when opening the window),
      StencilBits is @italic(not) updated to the current (provided)
      stencil buffer bit size. For example, if you requested StencilBits := 8,
      and you got 16-bits stencil buffer: StencilBits value will still remain 8.
      This is sensible in case you close the window, tweak some settings
      and try to open it again. Use @code(glGetInteger(GL_STENCIL_BITS))
      when window is open to query current (actual) buffer size. }
    property StencilBits: Cardinal read FStencilBits write FStencilBits default DefaultStencilBits;

    { How many samples are required for multi-sampling (anti-aliasing).

      1 means that no multi-sampling is required.
      Values larger than 1 mean that we require OpenGL context with
      multi-sampling capabilities. Various GPUs may support various
      values (it's a trade-off between quality and speed),
      try typical values 2 or 4.

      You can enable/disable anti-aliasing in your program by code like

      @longCode(#
        if GLFeatures.Multisample then glEnable(GL_MULTISAMPLE);
        if GLFeatures.Multisample then glDisable(GL_MULTISAMPLE);
      #)

      But usually that's not needed, as it is "on" by default
      (GL_ARB_multisample spec says so) if you requested multi-sampling context
      (that is, if this property is > 1). See GL_ARB_multisample spec for details:
      [http://opengl.org/registry/specs/ARB/multisample.txt].

      Just like with other XxxBits property, we may get more
      samples than we requested (e.g. if you request 3, you will most probably
      get 4). But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      we will raise an error.

      TODO: this may change to be similar to Lazarus
      TOpenGLControl.MultiSampling, and also be more comfortable --- to retry
      initialization with no multi-sampling. In this case this property will
      not be changed, to be nice.

      You can read @link(TGLFeatures.CurrentMultiSampling GLFeatures.CurrentMultiSampling)
      after initializing the rendering context to know how many samples did you
      actually get (and did you get multi-sampling at all). }
    property MultiSampling: Cardinal read FMultiSampling write FMultiSampling default 1;
  end;

  { OpenGL(ES) context. }
  TGLContext = class
  private
    type
      TGLContextList = {$ifdef FPC}specialize{$endif} TObjectList<TGLContext>;
    class var
      { TGLContext instances between @link(Initialize) (without exception) and
        @link(Finalize).

        We use this to share rendering contexts,
        as all rendering contexts in our engine must share rendering resources,
        like OpenGL textures, shaders, VBOs.
        This sharing makes caching easier, e.g. TCastleScene and textures cache
        is not per-context but just one global cache.

        May be @nil if empty. }
      FInitializedContexts: TGLContextList;
  strict private
    FInitialized: Boolean;
  protected
    { Any initialized context. A new context
      should share data with it. May be @nil if no sharing necessary
      (which means this is 1st context). }
    function SharedContext: TGLContext;

    { Override in descendants to initialize rendering context.

      No need to worry in descendants about what happens when this raises
      an exception (non-virtual @link(Initialize) will make sure to call
      @link(FinalizeCore) in such case).

      No need to worry in descendants whether this is called on already-initialized
      context. (non-virtual @link(Initialize) will make sure to never call
      this when context is already initialized.) }
    procedure InitializeCore(const Requirements: TGLContextRequirements); virtual; abstract;

    { Override in descendants to finalize rendering context,
      reverting work done by @link(InitializeCore).

      Has to be implemented to tolerate without errors a partially open context,
      because this will be called also when @link(InitializeCore)
      raised an exception. }
    procedure FinalizeCore; virtual; abstract;

    { Override in descendants to make this rendering context current.

      Implementation can assume context is initialized. }
    procedure MakeCurrentCore; virtual; abstract;

    { Override in descendants to swap buffers.

      Implementation can assume context is initialized.

      Implementation can assume context was created with DoubleBuffer. }
    procedure SwapBuffersCore; virtual; abstract;
  public
    class function InitializedContextsCount: Cardinal;

    destructor Destroy; override;

    { Initialize rendering context.
      Called after creating native window.
      Calling this is ignored when used on already-initialized context.
      If context cannot be initialized, this raises an exception.
      But it leaves this class in reliable (not initialized) state. }
    procedure Initialize(const Requirements: TGLContextRequirements);

    { Finalize rendering context, reverting work done by @link(Initialize).
      Called before destroying native window.
      Calling this is ignored when used on non-initialized context. }
    procedure Finalize;

    { Make the GL context current.
      Should only be called on initialized context. }
    procedure MakeCurrent;

    { Swap buffers.
      Should only be called on initialized context.
      Should only be called on context created with DoubleBuffer. }
    procedure SwapBuffers;

    { Is the context initialized now,
      that is between @link(Initialize) and @link(Finalize). }
    property Initialized: Boolean read FInitialized;
  end;

implementation

{ Note: We plan to use this approach (TGLContext class)
  to initialize OpenGL(ES) contexts everywhere,
  for both TCastleWindow and TCastleControl.

  It is almost done at this point: we have
  - wgl,
  - EGL,
  - glX descendants.
  And they are used by TCastleWindow and Delphi + TCastleControl.

  TODO: But we have some notable exceptions:
  - TCastleWindow on Cocoa uses custom code
  - TCastleWindow on Android and iOS use special organization (library)
  - TCastleControl on LCL uses LCL TOpenGLControl

  At some point, everything will likely be adjusted to TGLContext,
  and we'll be able to simplify some API.
}

uses {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleUtils, CastleStringUtils, CastleGLUtils, CastleLog;

{ TGLContextRequirements ----------------------------------------------------- }

constructor TGLContextRequirements.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffer := true;
  DepthBits := DefaultDepthBits;
  StencilBits := DefaultStencilBits;
  MultiSampling := 1;
end;

function TGLContextRequirements.GetColorBits: Cardinal;
begin
  Result := RedBits + GreenBits + BlueBits;
end;

procedure TGLContextRequirements.SetColorBits(const Value: Cardinal);
begin
  RedBits := Value div 3;
  BlueBits := Value div 3;
  GreenBits := Value - RedBits - BlueBits;
  Assert(Value = ColorBits);
end;

function TGLContextRequirements.RequestedBufferAttributes: String;
begin
  if DoubleBuffer then
    Result := 'double buffered'
  else
    Result := 'single buffered';
  if ColorBits > 0 then
    Result := Result + Format(', with RGB colors bits (%d, %d, %d) (total %d color bits)', [RedBits, GreenBits, BlueBits, ColorBits]);
  if DepthBits > 0 then
    Result := Result + Format(', with %d-bits sized depth buffer', [DepthBits]);
  if StencilBits > 0 then
    Result := Result + Format(', with %d-bits sized stencil buffer', [StencilBits]);
  if AlphaBits > 0 then
    Result := Result + Format(', with %d-bits sized alpha channel', [AlphaBits]);
  if MultiSampling > 1 then
    Result := Result + Format(', with multisampling (%d samples)', [MultiSampling]);
end;

procedure TGLContextRequirements.CheckRequestedBufferAttributes(
  const ProviderName: string; ProvidedStencilBits, ProvidedDepthBits,
  ProvidedAlphaBits, ProvidedMultiSampling: Cardinal);

  procedure CheckRequestedBits(const Name: string; RequestedBits, ProvidedBits: Cardinal);
  begin
    if ProvidedBits < RequestedBits then
      raise EGLContextNotPossible.CreateFmt('%s provided OpenGL context with %s'
        +' %d-bits sized but at least %d-bits sized is required',
        [ ProviderName, Name, ProvidedBits, RequestedBits ]);
  end;

 begin
  CheckRequestedBits('stencil buffer', StencilBits, ProvidedStencilBits);
  CheckRequestedBits('depth buffer', DepthBits, ProvidedDepthBits);
  CheckRequestedBits('alpha channel', AlphaBits, ProvidedAlphaBits);

  { If MultiSampling <= 1, this means that multisampling not required,
    so don't check it. Even if MultiSampling = 1 and ProvidedMultiSampling = 0
    (as most backends report no multisampling as num samples = 0), it's all Ok. }

  if MultiSampling > 1 then
  begin
    if ProvidedMultiSampling < MultiSampling then
     raise EGLContextNotPossible.CreateFmt('%s provided OpenGL context with %d ' +
       'samples for multisampling (<= 1 means that no multisampling was provided) ' +
       'but at last %d samples for multisampling is required',
       [ ProviderName, ProvidedMultiSampling, MultiSampling ]);
  end;
end;

{ TGLContext --------------------------------------------------------------- }

procedure TGLContext.Initialize(const Requirements: TGLContextRequirements);
begin
  if FInitialized then
    Exit;

  try
    InitializeCore(Requirements);
  except
    FinalizeCore;
    raise;
  end;

  FInitialized := true;

  if FInitializedContexts = nil then
    FInitializedContexts := TGLContextList.Create(false);
  Assert(not FInitializedContexts.Contains(Self));
  FInitializedContexts.Add(Self);

  // This should always be true, FInitialized is just more efficient than checking Contains
  Assert(FInitialized = FInitializedContexts.Contains(Self));
end;

procedure TGLContext.Finalize;
begin
  if not FInitialized then
    Exit;

  FinalizeCore;
  FInitialized := false;
  if FInitializedContexts <> nil then
  begin
    FInitializedContexts.Remove(Self);

    // This should always be true, FInitialized is just more efficient than checking Contains
    Assert(FInitialized = FInitializedContexts.Contains(Self));
  end else
  begin
    WritelnWarning('TGLContext', 'Finalizing TGLContext instance after finalization of CastleInternalContextBase unit, we no longer track currently initialized contexts');
  end;
end;

destructor TGLContext.Destroy;
begin
  { We do not Finalize here automatically, since possibly native window
    was already destroyed, and trying to do Finalize now would cause further errors.
    So we just warn.
    Outside code should always make sure to call Finalize before destroying.

    We remove from FInitializedContexts, to prevent danling pointers on this list. }

  if (FInitializedContexts <> nil) and
     (FInitializedContexts.Contains(Self)) then
  begin
    WritelnWarning('TGLContext', 'Destroying TGLContext instance with initialized OpenGL context; you should call Finalize first');
    FInitializedContexts.Remove(Self);
  end;

  inherited;
end;

class function TGLContext.InitializedContextsCount: Cardinal;
begin
  if FInitializedContexts <> nil then
    Result := FInitializedContexts.Count
  else
    Result := 0;
end;

function TGLContext.SharedContext: TGLContext;
begin
  if InitializedContextsCount >= 1 then // also checks FInitializedContexts <> nil
  begin
    Result := FInitializedContexts.First;
    Assert(Result <> Self, 'TGLContext.SharedContext should be used before adding context to FInitializedContexts, so Self should not be on the list yet');
  end else
    Result := nil;
end;

procedure TGLContext.MakeCurrent;
begin
  Assert(Initialized);
  MakeCurrentCore;
end;

procedure TGLContext.SwapBuffers;
begin
  Assert(Initialized);
  SwapBuffersCore;
end;

initialization
finalization
  FreeAndNil(TGLContext.FInitializedContexts);
end.
