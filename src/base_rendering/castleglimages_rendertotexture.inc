{%MainUnit castleglimages.pas}
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

{ Part of CastleGLImages unit: rendering to texture (TGLRenderToTexture). }

{$ifdef read_interface}

{$ifdef read_interface_type}
//type - don't add this Delphi can't parse it correctly
  EFramebufferError = class(Exception);
  EFramebufferSizeTooLow = class(EFramebufferError);
  EFramebufferInvalid  = class(EFramebufferError);

  TGLRenderToTextureBuffer = (tbColor, tbDepth, tbColorAndDepth, tbNone);

  { Rendering to texture with OpenGL.
    Uses framebuffer (if available), and has fallback to glCopyTexSubImage2D
    for (really) old OpenGL implementations. }
  TGLRenderToTexture = class
  private
    FWidth: Cardinal;
    FHeight: Cardinal;

    FTexture: TGLTextureId;
    FTextureTarget: TGLenum;
    FCompleteTextureTarget: TGLenum;
    FDepthTextureTarget: TGLenum;
    FBuffer: TGLRenderToTextureBuffer;
    FStencil: boolean;
    FDepthTexture: TGLTextureId;

    FGLInitialized: boolean;
    Framebuffer, RenderbufferColor, RenderbufferDepth, RenderbufferStencil: TGLuint;

    FramebufferBound: boolean;
    FColorBufferAlpha: boolean;
    FMultiSampling: Cardinal;
  public
    { Constructor. Doesn't require OpenGL context,
      and doesn't initialize the framebuffer.
      You'll have to use GLContextOpen before actually making Render. }
    constructor Create(const AWidth, AHeight: Cardinal);

    destructor Destroy; override;

    { Width and height must correspond to texture initialized width / height.
      You cannot change them when OpenGL stuff is already initialized
      (after GLContextOpen and before GLContextClose or destructor).
      @groupBegin }
    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;
    { @groupEnd }

    { Texture associated with the rendered buffer image.
      If @link(Buffer) is tbColor or tbColorAndDepth then we will capture
      here color contents. If @link(Buffer) is tbDepth then we will capture
      here depth contents (useful e.g. for shadow maps).
      If If @link(Buffer) is tbNone, this is ignored.

      We require this texture to be set to a valid texture (not 0)
      before GLContextOpen (unless Buffer is tbNone).
      Also, if you later change it,
      be careful to assign here other textures of only the same size and format.
      This allows us to call glCheckFramebufferStatusEXT (and eventually
      fallback to non-stencil version) right at GLContextOpen call, and no need
      to repeat it (e.g. at each RenderBegin).

      Changed by SetTexture. }
    property Texture: TGLTextureId read FTexture default 0;

    { Target of texture associated with rendered buffer.
      This is GL_TEXTURE_2D for normal 2D textures, but may also be
      GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP_POSITIVE_X etc. for
      other texture types.

      Companion to @link(Texture) property, changed together by SetTexture. }
    property TextureTarget: TGLenum read FTextureTarget default GL_TEXTURE_2D;

    { Change @link(Texture) and @link(TextureTarget).

      May be changed also when OpenGL stuff (framebuffer) is already
      initialized. This is useful, as it allows you to reuse framebuffer
      setup for rendering to different textures (as long as other settings
      are Ok, like Width and Height).

      It may even be changed between RenderBegin and RenderEnd.
      In fact, this is advised, if you have to call SetTexture often:
      SetTexture call outside of RenderBegin / RenderEnd causes two
      costly BindFramebuffer calls, that may be avoided when you're
      already between RenderBegin / RenderEnd. }
    procedure SetTexture(const ATexture: TGLTextureId;
      const ATextureTarget: TGLenum);

    { Bind target of texture associated with rendered color buffer.
      "Bind target" means that it describes the whole texture, for example
      for cube map it should be GL_TEXTURE_CUBE_MAP. }
    property CompleteTextureTarget: TGLenum
      read FCompleteTextureTarget write FCompleteTextureTarget default GL_TEXTURE_2D;

    { Depth texture used when @link(Buffer) = tbColorAndDepth.
      Note that this is not used when @link(Buffer) = tbDepth
      (the @link(Texture) and TextureTarget are used then).
      This must be set before GLContextOpen, and not modified later
      until GLContextClose. }
    property DepthTexture: TGLTextureId read FDepthTexture write FDepthTexture;
    property DepthTextureTarget: TGLenum read FDepthTextureTarget write FDepthTextureTarget
      default GL_TEXTURE_2D;

    { Which buffer (color and/or depth) should we catch to the texture.

      @unorderedList(
        @item(tbColor: the @link(Texture) will contain color contents.)
        @item(tbDepth: the @link(Texture) will contain depth contents.)
        @item(tbColorAndDepth: the @link(Texture) will contain color
          contents, the @link(DepthTexture) will contain depth contents.)
        @item(tbNone: we will not capture screen contents to any texture at all.
          This is useful for rendering a screen that you want
          to capture to normal image with @link(SaveScreen).
          Be sure to capture the screen before RenderEnd.)
      )

      For tbDepth and tbColorAndDepth, the texture that will receive
      depth contents must have GL_DEPTH_COMPONENT* format,
      and we'll render depth buffer contents to it.

      For tbDepth, if the framebuffer is used (normal on recent GPUs),
      we will not write to the color buffer at all,
      so this is quite optimal for rendering shadow maps.

      This must be set before GLContextOpen, cannot be changed later. }
    property Buffer: TGLRenderToTextureBuffer
      read FBuffer write FBuffer default tbColor;

    { Should we require stencil buffer.

      This is usually safe, as FBO spec even requires that some format
      with stencil buffer must be available.

      However, @italic(this has a high chance to fail if you need
      @link(Buffer) = tbDepth or tbColorAndDepth).
      Reason: on GPU with packed depth and stencil buffer
      (see http://www.opengl.org/registry/specs/EXT/packed_depth_stencil.txt
      and https://www.khronos.org/registry/OpenGL/extensions/OES/OES_packed_depth_stencil.txt)
      FBO with separate depth and stencil may not be possible.
      And when your texture is GL_DEPTH_COMPONENT, this is a must.
      In the future, we could allow some flag to allow you to use texture
      with GL_DEPTH_STENCIL format, this would work with packed depth/stencil
      (actually, even require it). For now, @italic(it's advised to turn
      off @name when you use @link(Buffer) = tbDepth or tbColorAndDepth). }
    property Stencil: boolean
      read FStencil write FStencil default true;

    { Initialize OpenGL stuff (framebuffer).

      When OpenGL stuff is initialized (from GLContextOpen until
      GLContextClose or destruction) this class is tied to the current OpenGL context.

      @raises(EFramebufferSizeTooLow When required @link(Width) x @link(Height)
        is larger than maximum renderbuffer (single buffer within framebuffer)
        size.)

      @raises(EFramebufferInvalid When framebuffer is used,
        and check glCheckFramebufferStatusEXT fails. This should not happen,
        it means a programmer error. Or "unsupported" result
        of glCheckFramebufferStatusEXT (that is possible regardless of programmer)
        we have a nice fallback to non-FBO implementation.) }
    procedure GLContextOpen;

    { Release all OpenGL stuff (if anything initialized).
      This is also automatically called in destructor. }
    procedure GLContextClose;

    { Begin rendering into the texture. Commands following this will
      render to the texture image.

      When framebuffer is used, it's bound here.

      When framebuffer is not used, this doesn't do anything.
      So note that all rendering will be done to normal screen in this case. }
    procedure RenderBegin;

    { End rendering into the texture.

      When framebuffer is used, this binds the normal screen back.

      When framebuffer is not used, this does actual copying from the
      screen to the texture using glCopyTexSubImage2D. We use
      glCopyTexSubImage2D --- which means texture internal format
      should already be initialized! If you don't have any initial texture data,
      you can always initialize by glTexImage2D with @nil as pointer to data.

      During copying, we may change OpenGL bound 2D texture and read buffer.
      So their values are ignored, and may be changed arbitrarily, by this
      method.

      @param(RenderBeginFollows This allows for an optimizaion,
        to minimize the number of BindFramebuffer calls when you render
        many textures in the row using the same TGLRenderToTexture.
        If @true, then you @bold(must) call RenderBegin after this
        (before drawing anything else to OpenGL).
        We will internally leave framebuffer bound, which means that
        this RenderEnd and the very next RenderBegin will actually do nothing.)
    }
    procedure RenderEnd(const RenderBeginFollows: boolean = false);

    { Generate mipmaps for the texture.
      This will use glGenerateMipmap call, which is actually
      a part of EXT_framebuffer_object extension (or GL core together
      with framebuffer in GL core), so it will always
      raise EGenerateMipmapNotAvailable if framebuffer is not available.

      You should use HasGenerateMipmap and never call this
      if not HasGenerateMipmap, if you don't want to get this exception.

      @raises(EGenerateMipmapNotAvailable If glGenerateMipmap not available.) }
    procedure GenerateMipmap;

    { Color buffer name. Use only when Buffer = tbNone, between GLContextOpen
      and GLContextClose. This is the buffer name that you should pass to
      SaveScreen_NoFlush, currently it's just rbColorAttachment0
      if we actually have FBO or rbBack if not. }
    function ColorBuffer: TColorBuffer;

    { Do we require color buffer with alpha channel.
      Relevant only when Buffer = tbNone (as in all other cases,
      we do not have the color buffer --- colors either go into some texture
      or are ignored).

      This must be set before GLContextOpen, cannot be changed later. }
    property ColorBufferAlpha: boolean read FColorBufferAlpha write FColorBufferAlpha
      default false;

    { All buffers (color and such) will be created with the
      specified number of samples for multisampling.
      Values greater than 1 mean that multisampling is used, which enables
      anti-aliasing.
      Note that all your textures (in @link(Texture), @link(DepthTexture))
      must be created with the same number of samples.

      Ignored if not GLFBOMultiSampling. }
    property MultiSampling: Cardinal
      read FMultiSampling write FMultiSampling default 1;

    { Save the screen to an image.
      Call this between RenderBegin and RenderEnd. }
    function SaveScreen(const ImageClass: TCastleImageClass;
      const Rect: TRectangle): TCastleImage;
  end;
{$endif read_interface_type}

{$ifdef read_interface_func}

{ Are we currently doing off-screen rendering.
  This is @true if we're between
  TGLRenderToTexture.RenderBegin and TGLRenderToTexture.RenderEnd. }
function OffscreenRendering: Boolean;

{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

var
  FOffscreenRendering: Cardinal;

function OffscreenRendering: Boolean;
begin
  Result := FOffscreenRendering <> 0;
end;

{ BindFramebuffer stack ------------------------------------------------------ }

var
  { We may want to use an FBO, while another FBO is already used.

    Right now, this situation happens only when we use view3dscene
    with --screenshot option, and we load a scene that uses a generated
    texture (like RenderedTexture or GeneratedShadowMap).

    It's important in such cases that we should restore at the end
    previously bound FBO --- not necessarily just FBO number 0. }
  BoundFboStack: TUInt32List;

  { The FBO id meaning "the screen".
    This is 0 in usual circumstances, except on iOS and iPhone Simulator it can be non-zero
    (see https://stackoverflow.com/questions/11617013/why-would-glbindframebuffergl-framebuffer-0-result-in-blank-screen-in-cocos2d)
  }
  ScreenFbo: TGLuint;
  ScreenFboQueried: boolean;

{ Use instead of glBindFramebuffer(GL_FRAMEBUFFER, Fbo),
  for non-zero Fbo. This will bind and add this Fbo to stack. }
procedure BindFramebuffer(const Fbo: TGLuint);
begin
  Assert(Fbo <> 0);
  Assert(Fbo <> ScreenFbo);
  if BoundFboStack = nil then
    BoundFboStack := TUInt32List.Create;
  { See the ScreenFbo documentation for reason why we need to query it now.
    We use ScreenFboQueried to
    1. optimize (no need for glGetIntegerv every time we do BindFramebuffer)
    2. avoid spamming the console with a warning each frame on iOS and iPhone Simulator }
  if (BoundFboStack.Count = 0) and (not ScreenFboQueried) then
  begin
    ScreenFboQueried := true;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @ScreenFbo);
    if ScreenFbo <> 0 then
      { See about GTK3:
        https://stackoverflow.com/questions/62422683/framebuffer-issue-render-to-texture-with-gtk3-glarea-vs-glfw-identical-opengl#62436181

        See about Qt6:
        https://doc.qt.io/qt-6/qopenglfunctions.html
        "Note that Qt will translate a framebuffer argument of 0 to the currently bound QOpenGLContext's defaultFramebufferObject()."
        https://doc.qt.io/qt-6/qopenglcontext.html#defaultFramebufferObject
        "On some platforms (for instance, iOS) the default framebuffer object depends on the surface being rendered to, and might be different from 0."
      }
      WritelnLog('The screen FBO is not 0, it is %d. This known to happen on iOS, iPhone Simulator, GTK 3, Qt6.',
        [ScreenFbo]);
  end;
  BoundFboStack.Add(Fbo);

  if GLFeatures.Framebuffer then
    glBindFramebuffer(GL_FRAMEBUFFER, Fbo);
end;

{ Remove the top Fbo from the stack, and bind previous (new top) Fbo.
  Binds FBO number 0 (normal OpenGL buffer, actually uses ScreenFbo as it may
  be <> 0 on iOS / iPhone Simulator) if stack becomes empty.

  PreviousFboDefaultBuffer is set to the default draw buffer suitable
  for currently (after this call) bound FBO. It's GL_BACK if we're
  now in normal rendering to window (TODO: we assume you always use double-buffer then),
  or GL_COLOR_ATTACHMENT0 if we're in another non-window FBO.
  TODO: it should be GL_NONE if we're in another non-window FBO for tbDepth.
  Without this, if you would blindly try SetDrawBuffer(GL_BACK)
  after UnbindFramebuffer, and you are in another single-buffered FBO,
  OpenGL (at least NVidia and fglrx) will (rightly) report OpenGL
  "invalid enum" error. }
procedure UnbindFramebuffer(out PreviousFboDefaultBuffer: TGLenum); overload;
var
  PreviousFbo: TGLuint;
begin
  if (BoundFboStack <> nil) and (BoundFboStack.Count <> 0) then
  begin
    BoundFboStack.Count := BoundFboStack.Count - 1;
    if BoundFboStack.Count <> 0 then
      PreviousFbo := BoundFboStack.Last
    else
      PreviousFbo := ScreenFbo;
  end else
    PreviousFbo := ScreenFbo;

  if PreviousFbo = ScreenFbo then
    PreviousFboDefaultBuffer := GL_BACK
  else
    PreviousFboDefaultBuffer := GL_COLOR_ATTACHMENT0;

  if GLFeatures.Framebuffer then
    glBindFramebuffer(GL_FRAMEBUFFER, PreviousFbo);
end;

procedure UnbindFramebuffer; overload;
var
  PreviousFboDefaultBuffer: TGLenum;
begin
  UnbindFramebuffer(PreviousFboDefaultBuffer);
  { ignore PreviousFboDefaultBuffer }
end;

{ TGLRenderToTexture --------------------------------------------------------- }

{ Fortunately, all constants with equal meanings have also equal values
  both for EXT_framebuffer_object and ARB_framebuffer_object (as "core extension"
  in OpenGL 3). Checked for
  - FramebufferStatusToString error statuses
    (except ARB version simply removed some constans (so they will only
    occur if we happen to use EXT version))
  - GL_STENCIL_ATTACHMENT
  - GL_DEPTH_STENCIL
  - GL_DEPTH_ATTACHMENT
  - GL_FRAMEBUFFER
  - GL_COLOR_ATTACHMENT0
}

{ Wrapper around glFramebufferTexture2D }
procedure FramebufferTexture2D(const Target: TGLenum;
  const AttachmentDepthAndStencil: boolean;
  Attachment, TexTarget: TGLenum; const Texture: TGLTextureId; const Level: TGLint);
begin
  Assert(Texture <> 0, 'Texture 0 assigned to framebuffer, FBO will be incomplete');
  if GLFeatures.Framebuffer then
  begin
    if AttachmentDepthAndStencil then
    begin
      WritelnLog('FBO', 'Setting GL_DEPTH_ATTACHMENT and GL_STENCIL_ATTACHMENT to the same texture');
      glFramebufferTexture2D(Target, GL_DEPTH_ATTACHMENT  , TexTarget, Texture, Level);
      glFramebufferTexture2D(Target, GL_STENCIL_ATTACHMENT, TexTarget, Texture, Level);
    end else
      glFramebufferTexture2D(Target, Attachment, TexTarget, Texture, Level);
  end;
end;

constructor TGLRenderToTexture.Create(const AWidth, AHeight: Cardinal);
begin
  inherited Create;

  if not ApplicationProperties.IsGLContextOpen then
    raise EFramebufferError.Create('Cannot create TGLRenderToTexture when OpenGL context is not available. Do this later -- e.g. from events like TCastleControl.OnOpen, TCastleWindow.OnOpen, Application.OnInitialize, or after an explicit call to TCastleWindow.Open.');

  FTextureTarget := GL_TEXTURE_2D;
  FCompleteTextureTarget := GL_TEXTURE_2D;
  FDepthTextureTarget := GL_TEXTURE_2D;
  FStencil := true;

  FWidth := AWidth;
  FHeight := AHeight;
  FMultiSampling := 1;
end;

destructor TGLRenderToTexture.Destroy;
begin
  GLContextClose;
  inherited;
end;

procedure TGLRenderToTexture.SetTexture(
  const ATexture: TGLTextureId;
  const ATextureTarget: TGLenum);
begin
  if (ATexture <> FTexture) or (ATextureTarget <> FTextureTarget) then
  begin
    FTexture := ATexture;
    FTextureTarget := ATextureTarget;
    if Framebuffer <> 0 then
    begin
      if not FramebufferBound then
        BindFramebuffer(Framebuffer);
      if GLFeatures.Framebuffer then
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
      if not FramebufferBound then
        UnbindFramebuffer;
    end;
  end;
end;

procedure TGLRenderToTexture.GLContextOpen;

  function FramebufferStatusToString(const Status: TGLenum): string;
  {$ifndef OpenGLES}
  const
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT;
  {$endif}
  begin
    { some of these messages based on spec wording
      http://oss.sgi.com/projects/ogl-sample/registry/EXT/framebuffer_object.txt ,
      http://www.opengl.org/registry/specs/ARB/framebuffer_object.txt }
    case Status of
      GL_FRAMEBUFFER_COMPLETE                          : Result := 'Complete (no error)';
      GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             : Result := 'INCOMPLETE_ATTACHMENT: Not all framebuffer attachment points are "framebuffer attachment complete"';
      GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     : Result := 'INCOMPLETE_MISSING_ATTACHMENT: None image attached to the framebuffer. On some GPUs/drivers (fglrx) it may also mean that desired image size is too large';
      GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS             : Result := 'INCOMPLETE_DIMENSIONS: Not all attached images have the same width and height';
      {$ifndef OpenGLES}
      GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT            : Result := 'INCOMPLETE_FORMATS: Not all images attached to the attachment points COLOR_ATTACHMENT* have the same internal format';
      GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            : Result := 'INCOMPLETE_DRAW_BUFFER: The value of FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is NONE for some color attachment point(s) named by DRAW_BUFFERi';
      GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            : Result := 'INCOMPLETE_READ_BUFFER: READ_BUFFER is not NONE, and the value of FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is NONE for the color attachment point named by READ_BUFFER';
      GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            : Result := 'INCOMPLETE_MULTISAMPLE: The value of RENDERBUFFER_SAMPLES is not the same for all attached images.';
      {$endif}
      GL_FRAMEBUFFER_UNSUPPORTED                       : Result := 'UNSUPPORTED: The combination of internal formats of the attached images violates an implementation-dependent set of restrictions';
      0: Result := 'OpenGL error during CheckFramebufferStatus';
      else Result := 'Unknown FramebufferStatus error: ' + GLErrorString(Status);
    end;
  end;

  { Create and bind and set renderbuffer storage.
    If AttachmentDepthAndStencil, we attach to both depth/stencil,
    and provided Attachment value is ignored. Otherwise, we attach to the given
    Attachment. }
  procedure GenBindRenderbuffer(var RenderbufferId: TGLuint;
    const InternalFormat: TGLenum; const AttachmentDepthAndStencil: boolean;
    Attachment: TGLenum);
  begin
    if GLFeatures.Framebuffer then
    begin
      glGenRenderbuffers(1, @RenderbufferId);
      glBindRenderbuffer(GL_RENDERBUFFER, RenderbufferId);
      {$ifndef OpenGLES}
      if (MultiSampling > 1) and GLFeatures.FBOMultiSampling then
        glRenderbufferStorageMultisample(GL_RENDERBUFFER, MultiSampling, InternalFormat, Width, Height) else
      {$endif}
        glRenderbufferStorage(GL_RENDERBUFFER, InternalFormat, Width, Height);
      if AttachmentDepthAndStencil then
      begin
        // WritelnLog('FBO', 'Setting GL_DEPTH_ATTACHMENT and GL_STENCIL_ATTACHMENT to the same renderbuffer');
        { Radeon drivers (ATI Mobility Radeon HD 4330) throw OpenGL error "invalid enum"
          when trying to use GL_DEPTH_STENCIL_ATTACHMENT. }
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, RenderbufferId);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, RenderbufferId);
      end else
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, Attachment, GL_RENDERBUFFER, RenderbufferId);
    end;
  end;

  function ColorBufferFormat: TGLenum;
  begin
    if ColorBufferAlpha then
      Result := GL_RGBA
    else
      Result := GL_RGB;
  end;

var
  Status: TGLenum;
  DepthBufferFormatPacked, DepthAttachmentPacked: TGLenum;
  DepthAttachmentWithStencil: boolean;
  Success: boolean;
  PreviousFboDefaultBuffer: TGLenum;
begin
  Assert(not FGLInitialized, 'You cannot call TGLRenderToTexture.GLContextInit on already OpenGL-initialized instance. Call GLContextClose first if this is really what you want.');

  if GLFeatures.Framebuffer and
     (not (GLVersion.BuggyFBOCubeMap and TargetIsCubeMapSide(TextureTarget))) then
  begin
    if (Width > GLFeatures.MaxRenderbufferSize) or
       (Height > GLFeatures.MaxRenderbufferSize) then
      raise EFramebufferSizeTooLow.CreateFmt('Maximum renderbuffer (within framebuffer) size is %d x %d in your OpenGL implementation, while we require %d x %d',
        [ GLFeatures.MaxRenderbufferSize, GLFeatures.MaxRenderbufferSize, Width, Height ]);

    glGenFramebuffers(1, @Framebuffer);
    BindFramebuffer(Framebuffer);

    { When GLFeatures.PackedDepthStencil and stencil is wanted
      (a very common case!, as most GPUs have EXT_packed_depth_stencil
      and for shadow volumes we want stencil) we desperately want to
      use one renderbuffer or one texture with combined depth/stencil info.
      Other possibilities may be not available at all,
      as OpenGL only guarantees we can create FBO matching current screen configuration. }
    {$ifndef OpenGLES}
    if Stencil and GLFeatures.PackedDepthStencil then
    begin
      DepthBufferFormatPacked := GL_DEPTH_STENCIL;
      DepthAttachmentWithStencil := true;
      DepthAttachmentPacked := GL_DEPTH_STENCIL_ATTACHMENT;
    end else
    // TODO-es This is probably needed on gles too?
    // we have GL_DEPTH_STENCIL_OES.
    {$endif}
    begin
      DepthBufferFormatPacked :=
        { OpenGLES notes:
          - When depth is a texture, GL_DEPTH_COMPONENT works just as well
            as GL_DEPTH_COMPONENT16. Our depth textures use UNSIGNED_SHORT,
            so they match 16 bits. Although when we use UNSIGNED_INT for
            depth for screen effects it also works...
            So it really doesn't seem to matter, at least for Mesa OpenGLES
            everything works.
          - For renderbuffer, we really need to use the enum with 16 suffix.
            Otherwise (on Mesa OpenGLES) we'll get FBO incomplete errors. }
        {$ifdef OpenGLES} GL_DEPTH_COMPONENT16
        {$else} GL_DEPTH_COMPONENT
        {$endif};
      DepthAttachmentPacked := GL_DEPTH_ATTACHMENT;
      DepthAttachmentWithStencil := false;
    end;

    case Buffer of
      tbColor:
        begin
          FramebufferTexture2D(GL_FRAMEBUFFER, false, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
          GenBindRenderbuffer(RenderbufferDepth, DepthBufferFormatPacked, DepthAttachmentWithStencil, DepthAttachmentPacked);
        end;
      tbDepth:
        begin
          { Needed to consider FBO "complete" }
          SetDrawBuffer(GL_NONE);
          SetReadBuffer(GL_NONE);

          FramebufferTexture2D(GL_FRAMEBUFFER, DepthAttachmentWithStencil, DepthAttachmentPacked, TextureTarget, Texture, 0);
        end;
      tbColorAndDepth:
        begin
          FramebufferTexture2D(GL_FRAMEBUFFER, false, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
          FramebufferTexture2D(GL_FRAMEBUFFER, DepthAttachmentWithStencil, DepthAttachmentPacked, DepthTextureTarget, DepthTexture, 0);
        end;
      tbNone:
        begin
          GenBindRenderbuffer(RenderbufferColor, ColorBufferFormat, false, GL_COLOR_ATTACHMENT0);
          GenBindRenderbuffer(RenderbufferDepth, DepthBufferFormatPacked, DepthAttachmentWithStencil, DepthAttachmentPacked);
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Buffer 1?');
      {$endif}
    end;

    { setup separate stencil buffer }
    if Stencil and not GLFeatures.PackedDepthStencil then
      { initialize RenderbufferStencil, attach it to FBO stencil }
      GenBindRenderbuffer(RenderbufferStencil, GL_STENCIL_INDEX, false, GL_STENCIL_ATTACHMENT);

    Success := false;
    try
      CheckGLErrors('Check errors before checking FBO status');
      Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
      case Status of
        GL_FRAMEBUFFER_COMPLETE: Success := true;
        GL_FRAMEBUFFER_UNSUPPORTED: WritelnWarning('FBO', 'Unsupported framebuffer configuration, will fallback to glCopyTexSubImage2D approach. If your window is invisible (like for "view3dscene --screenshot"), you may get only a black screen.');
        else raise EFramebufferInvalid.CreateFmt('Framebuffer check failed: %s (FBO error number %d)',
          [ FramebufferStatusToString(Status), Status]);
      end;
    finally
      { Always, regardless of Success, unbind FBO and restore normal gl*Buffer }
      glBindRenderbuffer(GL_RENDERBUFFER, 0);
      UnbindFramebuffer(PreviousFboDefaultBuffer);

      if Buffer = tbDepth then
      begin
        SetDrawBuffer(PreviousFboDefaultBuffer);
        SetReadBuffer(PreviousFboDefaultBuffer);
      end;

      { If failure, release resources. In particular, this sets Framebuffer = 0,
        which will be a signal to other methods that FBO is not supported. }
      if not Success then
        GLContextClose;
    end;
  end;

  FGLInitialized := true;
end;

procedure TGLRenderToTexture.GLContextClose;

  procedure FreeRenderbuffer(var Buf: TGLuint);
  begin
    if Buf <> 0 then
    begin
      if GLFeatures.Framebuffer then
        glDeleteRenderbuffers(1, @Buf);
      Buf := 0;
    end;
  end;

  procedure FreeFramebuffer(var Buf: TGLuint);
  begin
    if Buf <> 0 then
    begin
      if GLFeatures.Framebuffer then
        glDeleteFramebuffers(1, @Buf);
      Buf := 0;
    end;
  end;

begin
  FreeRenderbuffer(RenderbufferColor);
  FreeRenderbuffer(RenderbufferDepth);
  FreeRenderbuffer(RenderbufferStencil);
  FreeFramebuffer(Framebuffer);
end;

procedure TGLRenderToTexture.RenderBegin;
begin
  Inc(FOffscreenRendering);

  if Framebuffer <> 0 then
  begin
    if not FramebufferBound then
    begin
      BindFramebuffer(Framebuffer);
      FramebufferBound := true;

      if Buffer = tbDepth then
      begin
        SetDrawBuffer(GL_NONE);
        SetReadBuffer(GL_NONE);
      end;
    end;
    Assert(FramebufferBound);
  end;
end;

{ A debug trick, saves color or depth buffer of the generated framebuffer image
  to a URL (file:///tmp/framebuffer_color/depth.png, change the code below
  if you want other URL). Useful e.g. to visualize captured shadow maps. }
{ $define DEBUG_SAVE_FRAMEBUFFER_DEPTH}
{ $define DEBUG_SAVE_FRAMEBUFFER_COLOR}

procedure TGLRenderToTexture.RenderEnd(const RenderBeginFollows: boolean);

{$ifdef DEBUG_SAVE_FRAMEBUFFER_COLOR}
  procedure SaveColor(const URL: string);
  var
    Image: TCastleImage;
  begin
    Image := TRGBImage.Create(Width, Height);
    try
      BeforePackImage(Image);
      try
        glReadPixels(0, 0, Width, Height, ImageGLFormat(Image),
          ImageGLType(Image), Image.RawPixels);
      finally AfterPackImage(Image) end;

      SaveImage(Image, URL);
    finally FreeAndNil(Image) end;
  end;
{$endif DEBUG_SAVE_FRAMEBUFFER_COLOR}

{$ifdef DEBUG_SAVE_FRAMEBUFFER_DEPTH}
  procedure SaveDepth(const URL: string);
  var
    Image: TGrayscaleImage;
  begin
    Image := TGrayscaleImage.Create(Width, Height);
    try
      BeforePackImage(Image);
      try
        glReadPixels(0, 0, Width, Height, GL_DEPTH_COMPONENT,
          ImageGLType(Image), Image.RawPixels);
      finally AfterPackImage(Image) end;

      SaveImage(Image, URL);
    finally FreeAndNil(Image) end;
  end;
{$endif DEBUG_SAVE_FRAMEBUFFER_DEPTH}

var
  PreviousFboDefaultBuffer: TGLenum;
begin
{$ifdef DEBUG_SAVE_FRAMEBUFFER_COLOR}
  if Buffer <> tbDepth then
    SaveColor('file:///tmp/framebuffer_color.png');
{$endif DEBUG_SAVE_FRAMEBUFFER_COLOR}
{$ifdef DEBUG_SAVE_FRAMEBUFFER_DEPTH}
  SaveDepth('file:///tmp/framebuffer_depth.png');
{$endif DEBUG_SAVE_FRAMEBUFFER_DEPTH}

  if Framebuffer <> 0 then
  begin
    Assert(FramebufferBound);
    if not RenderBeginFollows then
    begin
      UnbindFramebuffer(PreviousFboDefaultBuffer);
      FramebufferBound := false;

      if Buffer = tbDepth then
      begin
        SetDrawBuffer(PreviousFboDefaultBuffer);
        SetReadBuffer(PreviousFboDefaultBuffer);
      end;
    end;
  end else
  if Buffer <> tbNone then
  begin
    { Actually update OpenGL texture }
    glBindTexture(CompleteTextureTarget, Texture);
    SetReadBuffer(GL_BACK);
    glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, Width, Height);

    if Buffer = tbColorAndDepth then
    begin
      glBindTexture(DepthTextureTarget, DepthTexture);
      SetReadBuffer(GL_BACK);
      glCopyTexSubImage2D(DepthTextureTarget, 0, 0, 0, 0, 0, Width, Height);
    end;
  end;

  Dec(FOffscreenRendering);
end;

procedure TGLRenderToTexture.GenerateMipmap;
begin
  glBindTexture(CompleteTextureTarget, Texture);
  CastleInternalGLUtils.GenerateMipmap(CompleteTextureTarget);
  { TODO: size of these mipmaps is not accounted for in texture memory profiler }
end;

function TGLRenderToTexture.ColorBuffer: TColorBuffer;
begin
  if Framebuffer <> 0 then
    Result := cbColorAttachment0 else
    Result := cbBack;
end;

function TGLRenderToTexture.SaveScreen(const ImageClass: TCastleImageClass;
  const Rect: TRectangle): TCastleImage;
var
  NonMultiFbo: TGLRenderToTexture;
begin
  Assert(FramebufferBound);

  if (MultiSampling > 1) and
     GLFeatures.FBOMultiSampling then
  begin
    if not GLFeatures.FramebufferBlit then
      raise Exception.Create('Cannot get FBO contents without glBlitFramebuffer supported. You need a newer GPU for this application to work.');

    { In case we used multi-sampling FBO for rendering,
      we cannot directly use glReadPixels on it. See the bottom of
      https://www.khronos.org/opengl/wiki/GL_EXT_framebuffer_multisample :
      we need to blit the Fbo into another non-multisampling FBO first. }
    NonMultiFbo := TGLRenderToTexture.Create(Width, Height);
    try
      NonMultiFbo.Buffer := tbNone;
      NonMultiFbo.ColorBufferAlpha := ColorBufferAlpha;
      NonMultiFbo.GLContextOpen;

      glBindFramebuffer(GL_READ_FRAMEBUFFER, Framebuffer);
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER, NonMultiFbo.Framebuffer);
      glBlitFramebuffer(0, 0, Width, Height, 0, 0, Width, Height, GL_COLOR_BUFFER_BIT, GL_NEAREST);

      glBindFramebuffer(GL_FRAMEBUFFER, NonMultiFbo.Framebuffer);
      Result := SaveScreen_NoFlush(ImageClass, Rect, ColorBuffer);

      // bind again our own FBO, to be consistent with behavior in non-multi-sampling cases
      glBindFramebuffer(GL_FRAMEBUFFER, Framebuffer);
    finally FreeAndNil(NonMultiFbo) end;
  end else
    Result := SaveScreen_NoFlush(ImageClass, Rect, ColorBuffer);
end;

{$endif read_implementation}
