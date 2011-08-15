{ Add ARB_framebuffer_object, missing in older FPC GLExt unit. }

{$ifdef VER2_0}   {$define ADD_ARB_framebuffer_object} {$endif}
{$ifdef VER2_2}   {$define ADD_ARB_framebuffer_object} {$endif}
{$ifdef VER2_4_0} {$define ADD_ARB_framebuffer_object} {$endif}

{$ifdef ADD_ARB_framebuffer_object}

{$ifdef read_interface}

//**** GL_ARB_framebuffer_object *****//
const
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_DRAW_FRAMEBUFFER_BINDING = GL_FRAMEBUFFER_BINDING;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;
var
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glDeleteRenderbuffers: procedure(n: GLsizei; const renderbuffers: PGLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glDeleteFramebuffers: procedure(n: GLsizei; const framebuffers: PGLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glCheckFramebufferStatus: function(target: GLenum): GLenum; {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFramebufferTexture1D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFramebufferTexture3D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glGenerateMipmap: procedure(target: GLenum); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glBlitFramebuffer: procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glRenderbufferStorageMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFramebufferTextureLayer: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}

{ Using LoadAsCore = true means that we will *not* check
  if the extension is advertised in glGetString(GL_EXTENSIONS) string.
  This allows to successfully Load_GL_version_3_0 in an OpenGL 3.0
  forward-compatible context, where the "core extensions" do not have
  to be mentioned inside glGetString(GL_EXTENSIONS). }

function Load_GL_ARB_framebuffer_object(LoadAsCore: boolean = false): Boolean;
{$endif read_interface}

{$ifdef read_implementation}
function Load_GL_ARB_framebuffer_object(LoadAsCore: boolean): Boolean;
var
  extstring: String;
begin
  Result := False;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_framebuffer_object', extstring) then
  begin
    Pointer(glIsRenderbuffer) := wglGetProcAddress('glIsRenderbuffer');
    if not Assigned(glIsRenderbuffer) then Exit;
    Pointer(glBindRenderbuffer) := wglGetProcAddress('glBindRenderbuffer');
    if not Assigned(glBindRenderbuffer) then Exit;
    Pointer(glDeleteRenderbuffers) := wglGetProcAddress('glDeleteRenderbuffers');
    if not Assigned(glDeleteRenderbuffers) then Exit;
    Pointer(glGenRenderbuffers) := wglGetProcAddress('glGenRenderbuffers');
    if not Assigned(glGenRenderbuffers) then Exit;
    Pointer(glRenderbufferStorage) := wglGetProcAddress('glRenderbufferStorage');
    if not Assigned(glRenderbufferStorage) then Exit;
    Pointer(glGetRenderbufferParameteriv) := wglGetProcAddress('glGetRenderbufferParameteriv');
    if not Assigned(glGetRenderbufferParameteriv) then Exit;
    Pointer(glIsFramebuffer) := wglGetProcAddress('glIsFramebuffer');
    if not Assigned(glIsFramebuffer) then Exit;
    Pointer(glBindFramebuffer) := wglGetProcAddress('glBindFramebuffer');
    if not Assigned(glBindFramebuffer) then Exit;
    Pointer(glDeleteFramebuffers) := wglGetProcAddress('glDeleteFramebuffers');
    if not Assigned(glDeleteFramebuffers) then Exit;
    Pointer(glGenFramebuffers) := wglGetProcAddress('glGenFramebuffers');
    if not Assigned(glGenFramebuffers) then Exit;
    Pointer(glCheckFramebufferStatus) := wglGetProcAddress('glCheckFramebufferStatus');
    if not Assigned(glCheckFramebufferStatus) then Exit;
    Pointer(glFramebufferTexture1D) := wglGetProcAddress('glFramebufferTexture1D');
    if not Assigned(glFramebufferTexture1D) then Exit;
    Pointer(glFramebufferTexture2D) := wglGetProcAddress('glFramebufferTexture2D');
    if not Assigned(glFramebufferTexture2D) then Exit;
    Pointer(glFramebufferTexture3D) := wglGetProcAddress('glFramebufferTexture3D');
    if not Assigned(glFramebufferTexture3D) then Exit;
    Pointer(glFramebufferRenderbuffer) := wglGetProcAddress('glFramebufferRenderbuffer');
    if not Assigned(glFramebufferRenderbuffer) then Exit;
    Pointer(glGetFramebufferAttachmentParameteriv) := wglGetProcAddress('glGetFramebufferAttachmentParameteriv');
    if not Assigned(glGetFramebufferAttachmentParameteriv) then Exit;
    Pointer(glGenerateMipmap) := wglGetProcAddress('glGenerateMipmap');
    if not Assigned(glGenerateMipmap) then Exit;
    Pointer(glBlitFramebuffer) := wglGetProcAddress('glBlitFramebuffer');
    if not Assigned(glBlitFramebuffer) then Exit;
    Pointer(glRenderbufferStorageMultisample) := wglGetProcAddress('glRenderbufferStorageMultisample');
    if not Assigned(glRenderbufferStorageMultisample) then Exit;
    Pointer(glFramebufferTextureLayer) := wglGetProcAddress('glFramebufferTextureLayer');
    if not Assigned(glFramebufferTextureLayer) then Exit;
    Result := True;
  end;
end;
{$endif read_implementation}

{$endif ADD_ARB_framebuffer_object}
