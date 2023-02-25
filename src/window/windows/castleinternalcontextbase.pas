{
  Copyright 2013-2023 Michalis Kamburelis.

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

uses SysUtils, Classes,
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
      ProvidedAccumRedBits, ProvidedAccumGreenBits, ProvidedAccumBlueBits,
      ProvidedAccumAlphaBits, ProvidedMultiSampling: Cardinal);

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

      You can always read OpenGL GL_SAMPLE_BUFFERS_ARB and GL_SAMPLES_ARB
      values after initializing OpenGL context, to know exactly
      how many samples did you actually get, and did you get multi-sampling at all.
      Actually, we already initialize global CastleGLUtils.GLCurrentMultiSampling
      for you, you can use this. }
    property MultiSampling: Cardinal read FMultiSampling write FMultiSampling default 1;
  end;

  { OpenGL(ES) context. }
  TGLContext = class
  public
    // Set this before using ContextCreate
    SharedContext: TGLContext; //< leave nil to not share

    { Create GL context. }
    procedure ContextCreate(const Requirements: TGLContextRequirements); virtual; abstract;

    { Destroy GL context. }
    procedure ContextDestroy; virtual; abstract;

    { Make the GL context current. }
    procedure MakeCurrent; virtual; abstract;

    { Swap buffers (if context was created with DoubleBuffer) or glFlush. }
    procedure SwapBuffers; virtual; abstract;
  end;

implementation

{ TODO: Use this approach to initialize OpenGL contexts everywhere for TCastleWindow. }

uses {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
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
  ProvidedAlphaBits, ProvidedAccumRedBits, ProvidedAccumGreenBits,
  ProvidedAccumBlueBits, ProvidedAccumAlphaBits,
  ProvidedMultiSampling: Cardinal);

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

end.
