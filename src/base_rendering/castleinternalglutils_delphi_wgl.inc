{%MainUnit castleimages.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: compatibility stuff for Delphi. }
{ TODO: make some of this cross-platform (e.g. Linux should also have necessary Load_GL_Xxx). }

{$ifndef FPC}
{$ifdef MSWINDOWS}

{$ifdef read_interface}

const
  // WGL constants
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;
  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB = $2042;

var
  wglGetExtensionsStringARB: function(hdc: HDC): PAnsiChar; stdcall;
  wglGetPixelFormatAttribivARB: function(hdc: HDC; iPixelFormat: GLint; iLayerPlane: GLint; nAttributes: GLuint; const piAttributes: PGLint; piValues: PGLint): BOOL; stdcall;
  wglGetPixelFormatAttribfvARB: function(hdc: HDC; iPixelFormat: GLint; iLayerPlane: GLint; nAttributes: GLuint; const piAttributes: PGLint; pfValues: PGLfloat): BOOL; stdcall;
  wglChoosePixelFormatARB: function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLuint): BOOL; stdcall;

function glext_ExtensionSupported(const extension: AnsiString; const searchIn: AnsiString): Boolean;
function Load_GL_ARB_texture_cube_map: Boolean;
function Load_GL_EXT_texture3D: Boolean;
function Load_GL_EXT_texture_filter_anisotropic: Boolean;
function Load_GL_ARB_occlusion_query: Boolean;
function Load_GL_ARB_framebuffer_object(LoadAsCore: boolean = false): Boolean;
function Load_GL_EXT_framebuffer_object: Boolean;
function Load_GL_ARB_texture_multisample(LoadAsCore: boolean = false): Boolean;
function Load_GL_ARB_multisample: Boolean;
function Load_GL_EXT_packed_depth_stencil: Boolean;
function Load_GL_ARB_texture_non_power_of_two: Boolean;
function Load_GL_ARB_texture_compression: Boolean;
function Load_GL_ARB_imaging: Boolean;
function Load_GL_ATI_texture_float: Boolean;
function Load_GL_ARB_texture_float: Boolean;
function Load_GL_ARB_depth_texture: Boolean;
function Load_GL_ARB_shader_objects: Boolean;
function Load_GL_ARB_vertex_shader: Boolean;
function Load_GL_ARB_fragment_shader: Boolean;
function Load_GL_ARB_shading_language_100: Boolean;
function Load_GL_ARB_window_pos: Boolean;
function Load_GL_MESA_window_pos: Boolean;
function Load_GL_ARB_shadow: Boolean;
function Load_GL_EXT_fog_coord: Boolean;
function Load_GL_NV_multisample_filter_hint: Boolean;
function Load_WGL_ARB_extensions_string: Boolean;
function Load_WGL_ARB_pixel_format: Boolean;
function Load_GL_EXT_stencil_wrap: Boolean;
function Load_GL_ATI_separate_stencil: Boolean;

{$endif read_interface}

{$ifdef read_implementation}

function glext_ExtensionSupported(const extension: AnsiString; const searchIn: AnsiString): Boolean;
var
  extensions: PAnsiChar;
  start: PAnsiChar;
  where, terminator: PAnsiChar;
begin

  if (Pos(' ', extension) <> 0) or (extension = '') then
  begin
    Result := FALSE;
    Exit;
  end;

  if searchIn = '' then extensions := PAnsiChar(glGetString(GL_EXTENSIONS))
  else extensions := PAnsiChar(searchIn);
  start := extensions;
  while TRUE do
  begin
    where := StrPos(start, PAnsiChar(extension));
    if where = nil then Break;
    terminator := Pointer(PtrUInt(where) + Length(extension));
    if (where = start) or (PAnsiChar(PtrUInt(where) - 1)^ = ' ') then
    begin
      if (terminator^ = ' ') or (terminator^ = #0) then
      begin
        Result := TRUE;
        Exit;
      end;
    end;
    start := terminator;
  end;
  Result := FALSE;
end;

function Load_GL_ARB_texture_cube_map: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_texture_cube_map', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture3D: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_texture3D', extstring) then
  begin
    glTexImage3DEXT := wglGetProcAddress('glTexImage3DEXT');
    if not Assigned(glTexImage3DEXT) then Exit;
    Result := TRUE;
  end;
end;

function Load_GL_EXT_texture_filter_anisotropic: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_texture_filter_anisotropic', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_occlusion_query: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_occlusion_query', extstring) then
  begin
    @glGenQueriesARB := wglGetProcAddress('glGenQueriesARB');
    if not Assigned(glGenQueriesARB) then Exit;
    @glDeleteQueriesARB := wglGetProcAddress('glDeleteQueriesARB');
    if not Assigned(glDeleteQueriesARB) then Exit;
    @glIsQueryARB := wglGetProcAddress('glIsQueryARB');
    if not Assigned(glIsQueryARB) then Exit;
    @glBeginQueryARB := wglGetProcAddress('glBeginQueryARB');
    if not Assigned(glBeginQueryARB) then Exit;
    @glEndQueryARB := wglGetProcAddress('glEndQueryARB');
    if not Assigned(glEndQueryARB) then Exit;
    @glGetQueryivARB := wglGetProcAddress('glGetQueryivARB');
    if not Assigned(glGetQueryivARB) then Exit;
    @glGetQueryObjectivARB := wglGetProcAddress('glGetQueryObjectivARB');
    if not Assigned(glGetQueryObjectivARB) then Exit;
    @glGetQueryObjectuivARB := wglGetProcAddress('glGetQueryObjectuivARB');
    if not Assigned(glGetQueryObjectuivARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_framebuffer_object(LoadAsCore: boolean): Boolean;
var
  extstring: AnsiString;
begin
  Result := False;
  extstring := glGetString(GL_EXTENSIONS);

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_framebuffer_object', extstring) then
  begin
    glIsRenderbuffer := wglGetProcAddress('glIsRenderbuffer');
    if not Assigned(glIsRenderbuffer) then Exit;
    glBindRenderbuffer := wglGetProcAddress('glBindRenderbuffer');
    if not Assigned(glBindRenderbuffer) then Exit;
    glDeleteRenderbuffers := wglGetProcAddress('glDeleteRenderbuffers');
    if not Assigned(glDeleteRenderbuffers) then Exit;
    glGenRenderbuffers := wglGetProcAddress('glGenRenderbuffers');
    if not Assigned(glGenRenderbuffers) then Exit;
    glRenderbufferStorage := wglGetProcAddress('glRenderbufferStorage');
    if not Assigned(glRenderbufferStorage) then Exit;
    glGetRenderbufferParameteriv := wglGetProcAddress('glGetRenderbufferParameteriv');
    if not Assigned(glGetRenderbufferParameteriv) then Exit;
    glIsFramebuffer := wglGetProcAddress('glIsFramebuffer');
    if not Assigned(glIsFramebuffer) then Exit;
    glBindFramebuffer := wglGetProcAddress('glBindFramebuffer');
    if not Assigned(glBindFramebuffer) then Exit;
    glDeleteFramebuffers := wglGetProcAddress('glDeleteFramebuffers');
    if not Assigned(glDeleteFramebuffers) then Exit;
    glGenFramebuffers := wglGetProcAddress('glGenFramebuffers');
    if not Assigned(glGenFramebuffers) then Exit;
    glCheckFramebufferStatus := wglGetProcAddress('glCheckFramebufferStatus');
    if not Assigned(glCheckFramebufferStatus) then Exit;
    glFramebufferTexture1D := wglGetProcAddress('glFramebufferTexture1D');
    if not Assigned(glFramebufferTexture1D) then Exit;
    glFramebufferTexture2D := wglGetProcAddress('glFramebufferTexture2D');
    if not Assigned(glFramebufferTexture2D) then Exit;
    glFramebufferTexture3D := wglGetProcAddress('glFramebufferTexture3D');
    if not Assigned(glFramebufferTexture3D) then Exit;
    glFramebufferRenderbuffer := wglGetProcAddress('glFramebufferRenderbuffer');
    if not Assigned(glFramebufferRenderbuffer) then Exit;
    glGetFramebufferAttachmentParameteriv := wglGetProcAddress('glGetFramebufferAttachmentParameteriv');
    if not Assigned(glGetFramebufferAttachmentParameteriv) then Exit;
    glGenerateMipmap := wglGetProcAddress('glGenerateMipmap');
    if not Assigned(glGenerateMipmap) then Exit;
    glBlitFramebuffer := wglGetProcAddress('glBlitFramebuffer');
    if not Assigned(glBlitFramebuffer) then Exit;
    glRenderbufferStorageMultisample := wglGetProcAddress('glRenderbufferStorageMultisample');
    if not Assigned(glRenderbufferStorageMultisample) then Exit;
    glFramebufferTextureLayer := wglGetProcAddress('glFramebufferTextureLayer');
    if not Assigned(glFramebufferTextureLayer) then Exit;
    Result := True;
  end;
end;

function Load_GL_EXT_framebuffer_object: Boolean;
var
  extstring: PAnsiChar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_framebuffer_object', extstring) then
  begin
    glIsRenderbufferEXT := wglGetProcAddress('glIsRenderbufferEXT');
    if not Assigned(glIsRenderbufferEXT) then Exit;
    glBindRenderbufferEXT := wglGetProcAddress('glBindRenderbufferEXT');
    if not Assigned(glBindRenderbufferEXT) then Exit;
    glDeleteRenderbuffersEXT := wglGetProcAddress('glDeleteRenderbuffersEXT');
    if not Assigned(glDeleteRenderbuffersEXT) then Exit;
    glGenRenderbuffersEXT := wglGetProcAddress('glGenRenderbuffersEXT');
    if not Assigned(glGenRenderbuffersEXT) then Exit;
    glRenderbufferStorageEXT := wglGetProcAddress('glRenderbufferStorageEXT');
    if not Assigned(glRenderbufferStorageEXT) then Exit;
    glGetRenderbufferParameterivEXT := wglGetProcAddress('glGetRenderbufferParameterivEXT');
    if not Assigned(glGetRenderbufferParameterivEXT) then Exit;
    glIsFramebufferEXT := wglGetProcAddress('glIsFramebufferEXT');
    if not Assigned(glIsFramebufferEXT) then Exit;
    glBindFramebufferEXT := wglGetProcAddress('glBindFramebufferEXT');
    if not Assigned(glBindFramebufferEXT) then Exit;
    glDeleteFramebuffersEXT := wglGetProcAddress('glDeleteFramebuffersEXT');
    if not Assigned(glDeleteFramebuffersEXT) then Exit;
    glGenFramebuffersEXT := wglGetProcAddress('glGenFramebuffersEXT');
    if not Assigned(glGenFramebuffersEXT) then Exit;
    glCheckFramebufferStatusEXT := wglGetProcAddress('glCheckFramebufferStatusEXT');
    if not Assigned(glCheckFramebufferStatusEXT) then Exit;
    glFramebufferTexture1DEXT := wglGetProcAddress('glFramebufferTexture1DEXT');
    if not Assigned(glFramebufferTexture1DEXT) then Exit;
    glFramebufferTexture2DEXT := wglGetProcAddress('glFramebufferTexture2DEXT');
    if not Assigned(glFramebufferTexture2DEXT) then Exit;
    glFramebufferTexture3DEXT := wglGetProcAddress('glFramebufferTexture3DEXT');
    if not Assigned(glFramebufferTexture3DEXT) then Exit;
    glFramebufferRenderbufferEXT := wglGetProcAddress('glFramebufferRenderbufferEXT');
    if not Assigned(glFramebufferRenderbufferEXT) then Exit;
    glGetFramebufferAttachmentParameterivEXT := wglGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
    if not Assigned(glGetFramebufferAttachmentParameterivEXT) then Exit;
    glGenerateMipmapEXT := wglGetProcAddress('glGenerateMipmapEXT');
    if not Assigned(glGenerateMipmapEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_multisample(LoadAsCore: boolean): Boolean;
var
  extstring: AnsiString;
begin
  Result := False;
  extstring := glGetString(GL_EXTENSIONS);

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_texture_multisample', extstring) then
  begin
    glTexImage2DMultisample := wglGetProcAddress('glTexImage2DMultisample');
    if not Assigned(glTexImage2DMultisample) then Exit;
    glTexImage3DMultisample := wglGetProcAddress('glTexImage3DMultisample');
    if not Assigned(glTexImage3DMultisample) then Exit;
    glGetMultisamplefv := wglGetProcAddress('glGetMultisamplefv');
    if not Assigned(glGetMultisamplefv) then Exit;
    glSampleMaski := wglGetProcAddress('glSampleMaski');
    if not Assigned(glSampleMaski) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_multisample: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_multisample', extstring) then
  begin
    glSampleCoverageARB := wglGetProcAddress('glSampleCoverageARB');
    if not Assigned(glSampleCoverageARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_packed_depth_stencil: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_packed_depth_stencil', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_non_power_of_two: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_texture_non_power_of_two', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_compression: Boolean;
var
  extstring: AnsiString;
begin
  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_texture_compression', extstring) then
  begin
    glCompressedTexImage3DARB := wglGetProcAddress('glCompressedTexImage3DARB');
    if not Assigned(glCompressedTexImage3DARB) then Exit;
    glCompressedTexImage2DARB := wglGetProcAddress('glCompressedTexImage2DARB');
    if not Assigned(glCompressedTexImage2DARB) then Exit;
    glCompressedTexImage1DARB := wglGetProcAddress('glCompressedTexImage1DARB');
    if not Assigned(glCompressedTexImage1DARB) then Exit;
    glCompressedTexSubImage3DARB := wglGetProcAddress('glCompressedTexSubImage3DARB');
    if not Assigned(glCompressedTexSubImage3DARB) then Exit;
    glCompressedTexSubImage2DARB := wglGetProcAddress('glCompressedTexSubImage2DARB');
    if not Assigned(glCompressedTexSubImage2DARB) then Exit;
    glCompressedTexSubImage1DARB := wglGetProcAddress('glCompressedTexSubImage1DARB');
    if not Assigned(glCompressedTexSubImage1DARB) then Exit;
    glGetCompressedTexImageARB := wglGetProcAddress('glGetCompressedTexImageARB');
    if not Assigned(glGetCompressedTexImageARB) then Exit;
    Result := TRUE;
  end;
end;


function Load_GL_ARB_imaging: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);
  if glext_ExtensionSupported('GL_ARB_imaging', extstring) then
  begin
    glColorTable := wglGetProcAddress('glColorTable');
    if not Assigned(glColorTable) then Exit;
    glColorTableParameterfv := wglGetProcAddress('glColorTableParameterfv');
    if not Assigned(glColorTableParameterfv) then Exit;
    glColorTableParameteriv := wglGetProcAddress('glColorTableParameteriv');
    if not Assigned(glColorTableParameteriv) then Exit;
    glCopyColorTable := wglGetProcAddress('glCopyColorTable');
    if not Assigned(glCopyColorTable) then Exit;
    glGetColorTable := wglGetProcAddress('glGetColorTable');
    if not Assigned(glGetColorTable) then Exit;
    glGetColorTableParameterfv := wglGetProcAddress('glGetColorTableParameterfv');
    if not Assigned(glGetColorTableParameterfv) then Exit;
    glGetColorTableParameteriv := wglGetProcAddress('glGetColorTableParameteriv');
    if not Assigned(glGetColorTableParameteriv) then Exit;
    glColorSubTable := wglGetProcAddress('glColorSubTable');
    if not Assigned(glColorSubTable) then Exit;
    glCopyColorSubTable := wglGetProcAddress('glCopyColorSubTable');
    if not Assigned(glCopyColorSubTable) then Exit;
    glConvolutionFilter1D := wglGetProcAddress('glConvolutionFilter1D');
    if not Assigned(glConvolutionFilter1D) then Exit;
    glConvolutionFilter2D := wglGetProcAddress('glConvolutionFilter2D');
    if not Assigned(glConvolutionFilter2D) then Exit;
    glConvolutionParameterf := wglGetProcAddress('glConvolutionParameterf');
    if not Assigned(glConvolutionParameterf) then Exit;
    glConvolutionParameterfv := wglGetProcAddress('glConvolutionParameterfv');
    if not Assigned(glConvolutionParameterfv) then Exit;
    glConvolutionParameteri := wglGetProcAddress('glConvolutionParameteri');
    if not Assigned(glConvolutionParameteri) then Exit;
    glConvolutionParameteriv := wglGetProcAddress('glConvolutionParameteriv');
    if not Assigned(glConvolutionParameteriv) then Exit;
    glCopyConvolutionFilter1D := wglGetProcAddress('glCopyConvolutionFilter1D');
    if not Assigned(glCopyConvolutionFilter1D) then Exit;
    glCopyConvolutionFilter2D := wglGetProcAddress('glCopyConvolutionFilter2D');
    if not Assigned(glCopyConvolutionFilter2D) then Exit;
    glGetConvolutionFilter := wglGetProcAddress('glGetConvolutionFilter');
    if not Assigned(glGetConvolutionFilter) then Exit;
    glGetConvolutionParameterfv := wglGetProcAddress('glGetConvolutionParameterfv');
    if not Assigned(glGetConvolutionParameterfv) then Exit;
    glGetConvolutionParameteriv := wglGetProcAddress('glGetConvolutionParameteriv');
    if not Assigned(glGetConvolutionParameteriv) then Exit;
    glGetSeparableFilter := wglGetProcAddress('glGetSeparableFilter');
    if not Assigned(glGetSeparableFilter) then Exit;
    glSeparableFilter2D := wglGetProcAddress('glSeparableFilter2D');
    if not Assigned(glSeparableFilter2D) then Exit;
    glGetHistogram := wglGetProcAddress('glGetHistogram');
    if not Assigned(glGetHistogram) then Exit;
    glGetHistogramParameterfv := wglGetProcAddress('glGetHistogramParameterfv');
    if not Assigned(glGetHistogramParameterfv) then Exit;
    glGetHistogramParameteriv := wglGetProcAddress('glGetHistogramParameteriv');
    if not Assigned(glGetHistogramParameteriv) then Exit;
    glGetMinmax := wglGetProcAddress('glGetMinmax');
    if not Assigned(glGetMinmax) then Exit;
    glGetMinmaxParameterfv := wglGetProcAddress('glGetMinmaxParameterfv');
    if not Assigned(glGetMinmaxParameterfv) then Exit;
    glGetMinmaxParameteriv := wglGetProcAddress('glGetMinmaxParameteriv');
    if not Assigned(glGetMinmaxParameteriv) then Exit;
    glHistogram := wglGetProcAddress('glHistogram');
    if not Assigned(glHistogram) then Exit;
    glMinmax := wglGetProcAddress('glMinmax');
    if not Assigned(glMinmax) then Exit;
    glResetHistogram := wglGetProcAddress('glResetHistogram');
    if not Assigned(glResetHistogram) then Exit;
    glResetMinmax := wglGetProcAddress('glResetMinmax');
    if not Assigned(glResetMinmax) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_texture_float: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ATI_texture_float', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_float: Boolean;
var
  extstring: PAnsiChar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_texture_float', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_depth_texture: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_depth_texture', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_shader_objects: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_shader_objects', extstring) then
  begin

    @glDeleteObjectARB := wglGetProcAddress('glDeleteObjectARB');
    if not Assigned(glDeleteObjectARB) then Exit;
    @glGetHandleARB := wglGetProcAddress('glGetHandleARB');
    if not Assigned(glGetHandleARB) then Exit;
    @glDetachObjectARB := wglGetProcAddress('glDetachObjectARB');
    if not Assigned(glDetachObjectARB) then Exit;
    @glCreateShaderObjectARB := wglGetProcAddress('glCreateShaderObjectARB');
    if not Assigned(glCreateShaderObjectARB) then Exit;
    @glShaderSourceARB := wglGetProcAddress('glShaderSourceARB');
    if not Assigned(glShaderSourceARB) then Exit;
    @glCompileShaderARB := wglGetProcAddress('glCompileShaderARB');
    if not Assigned(glCompileShaderARB) then Exit;
    @glCreateProgramObjectARB := wglGetProcAddress('glCreateProgramObjectARB');
    if not Assigned(glCreateProgramObjectARB) then Exit;
    @glAttachObjectARB := wglGetProcAddress('glAttachObjectARB');
    if not Assigned(glAttachObjectARB) then Exit;
    @glLinkProgramARB := wglGetProcAddress('glLinkProgramARB');
    if not Assigned(glLinkProgramARB) then Exit;
    @glUseProgramObjectARB := wglGetProcAddress('glUseProgramObjectARB');
    if not Assigned(glUseProgramObjectARB) then Exit;
    @glValidateProgramARB := wglGetProcAddress('glValidateProgramARB');
    if not Assigned(glValidateProgramARB) then Exit;
    @glUniform1fARB := wglGetProcAddress('glUniform1fARB');
    if not Assigned(glUniform1fARB) then Exit;
    @glUniform2fARB := wglGetProcAddress('glUniform2fARB');
    if not Assigned(glUniform2fARB) then Exit;
    @glUniform3fARB := wglGetProcAddress('glUniform3fARB');
    if not Assigned(glUniform3fARB) then Exit;
    @glUniform4fARB := wglGetProcAddress('glUniform4fARB');
    if not Assigned(glUniform4fARB) then Exit;
    @glUniform1iARB := wglGetProcAddress('glUniform1iARB');
    if not Assigned(glUniform1iARB) then Exit;
    @glUniform2iARB := wglGetProcAddress('glUniform2iARB');
    if not Assigned(glUniform2iARB) then Exit;
    @glUniform3iARB := wglGetProcAddress('glUniform3iARB');
    if not Assigned(glUniform3iARB) then Exit;
    @glUniform4iARB := wglGetProcAddress('glUniform4iARB');
    if not Assigned(glUniform4iARB) then Exit;
    @glUniform1fvARB := wglGetProcAddress('glUniform1fvARB');
    if not Assigned(glUniform1fvARB) then Exit;
    @glUniform2fvARB := wglGetProcAddress('glUniform2fvARB');
    if not Assigned(glUniform2fvARB) then Exit;
    @glUniform3fvARB := wglGetProcAddress('glUniform3fvARB');
    if not Assigned(glUniform3fvARB) then Exit;
    @glUniform4fvARB := wglGetProcAddress('glUniform4fvARB');
    if not Assigned(glUniform4fvARB) then Exit;
    @glUniform1ivARB := wglGetProcAddress('glUniform1ivARB');
    if not Assigned(glUniform1ivARB) then Exit;
    @glUniform2ivARB := wglGetProcAddress('glUniform2ivARB');
    if not Assigned(glUniform2ivARB) then Exit;
    @glUniform3ivARB := wglGetProcAddress('glUniform3ivARB');
    if not Assigned(glUniform3ivARB) then Exit;
    @glUniform4ivARB := wglGetProcAddress('glUniform4ivARB');
    if not Assigned(glUniform4ivARB) then Exit;
    @glUniformMatrix2fvARB := wglGetProcAddress('glUniformMatrix2fvARB');
    if not Assigned(glUniformMatrix2fvARB) then Exit;
    @glUniformMatrix3fvARB := wglGetProcAddress('glUniformMatrix3fvARB');
    if not Assigned(glUniformMatrix3fvARB) then Exit;
    @glUniformMatrix4fvARB := wglGetProcAddress('glUniformMatrix4fvARB');
    if not Assigned(glUniformMatrix4fvARB) then Exit;
    @glGetObjectParameterfvARB := wglGetProcAddress('glGetObjectParameterfvARB');
    if not Assigned(glGetObjectParameterfvARB) then Exit;
    @glGetObjectParameterivARB := wglGetProcAddress('glGetObjectParameterivARB');
    if not Assigned(glGetObjectParameterivARB) then Exit;
    @glGetInfoLogARB := wglGetProcAddress('glGetInfoLogARB');
    if not Assigned(glGetInfoLogARB) then Exit;
    @glGetAttachedObjectsARB := wglGetProcAddress('glGetAttachedObjectsARB');
    if not Assigned(glGetAttachedObjectsARB) then Exit;
    @glGetUniformLocationARB := wglGetProcAddress('glGetUniformLocationARB');
    if not Assigned(glGetUniformLocationARB) then Exit;
    @glGetActiveUniformARB := wglGetProcAddress('glGetActiveUniformARB');
    if not Assigned(glGetActiveUniformARB) then Exit;
    @glGetUniformfvARB := wglGetProcAddress('glGetUniformfvARB');
    if not Assigned(glGetUniformfvARB) then Exit;
    @glGetUniformivARB := wglGetProcAddress('glGetUniformivARB');
    if not Assigned(glGetUniformivARB) then Exit;
    @glGetShaderSourceARB := wglGetProcAddress('glGetShaderSourceARB');
    if not Assigned(glGetShaderSourceARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_vertex_shader: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_vertex_shader', extstring) then
  begin
    @glVertexAttrib1fARB := wglGetProcAddress('glVertexAttrib1fARB');
    if not Assigned(glVertexAttrib1fARB) then Exit;
    @glVertexAttrib1sARB := wglGetProcAddress('glVertexAttrib1sARB');
    if not Assigned(glVertexAttrib1sARB) then Exit;
    @glVertexAttrib1dARB := wglGetProcAddress('glVertexAttrib1dARB');
    if not Assigned(glVertexAttrib1dARB) then Exit;
    @glVertexAttrib2fARB := wglGetProcAddress('glVertexAttrib2fARB');
    if not Assigned(glVertexAttrib2fARB) then Exit;
    @glVertexAttrib2sARB := wglGetProcAddress('glVertexAttrib2sARB');
    if not Assigned(glVertexAttrib2sARB) then Exit;
    @glVertexAttrib2dARB := wglGetProcAddress('glVertexAttrib2dARB');
    if not Assigned(glVertexAttrib2dARB) then Exit;
    @glVertexAttrib3fARB := wglGetProcAddress('glVertexAttrib3fARB');
    if not Assigned(glVertexAttrib3fARB) then Exit;
    @glVertexAttrib3sARB := wglGetProcAddress('glVertexAttrib3sARB');
    if not Assigned(glVertexAttrib3sARB) then Exit;
    @glVertexAttrib3dARB := wglGetProcAddress('glVertexAttrib3dARB');
    if not Assigned(glVertexAttrib3dARB) then Exit;
    @glVertexAttrib4fARB := wglGetProcAddress('glVertexAttrib4fARB');
    if not Assigned(glVertexAttrib4fARB) then Exit;
    @glVertexAttrib4sARB := wglGetProcAddress('glVertexAttrib4sARB');
    if not Assigned(glVertexAttrib4sARB) then Exit;
    @glVertexAttrib4dARB := wglGetProcAddress('glVertexAttrib4dARB');
    if not Assigned(glVertexAttrib4dARB) then Exit;
    @glVertexAttrib4NubARB := wglGetProcAddress('glVertexAttrib4NubARB');
    if not Assigned(glVertexAttrib4NubARB) then Exit;
    @glVertexAttrib1fvARB := wglGetProcAddress('glVertexAttrib1fvARB');
    if not Assigned(glVertexAttrib1fvARB) then Exit;
    @glVertexAttrib1svARB := wglGetProcAddress('glVertexAttrib1svARB');
    if not Assigned(glVertexAttrib1svARB) then Exit;
    @glVertexAttrib1dvARB := wglGetProcAddress('glVertexAttrib1dvARB');
    if not Assigned(glVertexAttrib1dvARB) then Exit;
    @glVertexAttrib2fvARB := wglGetProcAddress('glVertexAttrib2fvARB');
    if not Assigned(glVertexAttrib2fvARB) then Exit;
    @glVertexAttrib2svARB := wglGetProcAddress('glVertexAttrib2svARB');
    if not Assigned(glVertexAttrib2svARB) then Exit;
    @glVertexAttrib2dvARB := wglGetProcAddress('glVertexAttrib2dvARB');
    if not Assigned(glVertexAttrib2dvARB) then Exit;
    @glVertexAttrib3fvARB := wglGetProcAddress('glVertexAttrib3fvARB');
    if not Assigned(glVertexAttrib3fvARB) then Exit;
    @glVertexAttrib3svARB := wglGetProcAddress('glVertexAttrib3svARB');
    if not Assigned(glVertexAttrib3svARB) then Exit;
    @glVertexAttrib3dvARB := wglGetProcAddress('glVertexAttrib3dvARB');
    if not Assigned(glVertexAttrib3dvARB) then Exit;
    @glVertexAttrib4fvARB := wglGetProcAddress('glVertexAttrib4fvARB');
    if not Assigned(glVertexAttrib4fvARB) then Exit;
    @glVertexAttrib4svARB := wglGetProcAddress('glVertexAttrib4svARB');
    if not Assigned(glVertexAttrib4svARB) then Exit;
    @glVertexAttrib4dvARB := wglGetProcAddress('glVertexAttrib4dvARB');
    if not Assigned(glVertexAttrib4dvARB) then Exit;
    @glVertexAttrib4ivARB := wglGetProcAddress('glVertexAttrib4ivARB');
    if not Assigned(glVertexAttrib4ivARB) then Exit;
    @glVertexAttrib4bvARB := wglGetProcAddress('glVertexAttrib4bvARB');
    if not Assigned(glVertexAttrib4bvARB) then Exit;
    @glVertexAttrib4ubvARB := wglGetProcAddress('glVertexAttrib4ubvARB');
    if not Assigned(glVertexAttrib4ubvARB) then Exit;
    @glVertexAttrib4usvARB := wglGetProcAddress('glVertexAttrib4usvARB');
    if not Assigned(glVertexAttrib4usvARB) then Exit;
    @glVertexAttrib4uivARB := wglGetProcAddress('glVertexAttrib4uivARB');
    if not Assigned(glVertexAttrib4uivARB) then Exit;
    @glVertexAttrib4NbvARB := wglGetProcAddress('glVertexAttrib4NbvARB');
    if not Assigned(glVertexAttrib4NbvARB) then Exit;
    @glVertexAttrib4NsvARB := wglGetProcAddress('glVertexAttrib4NsvARB');
    if not Assigned(glVertexAttrib4NsvARB) then Exit;
    @glVertexAttrib4NivARB := wglGetProcAddress('glVertexAttrib4NivARB');
    if not Assigned(glVertexAttrib4NivARB) then Exit;
    @glVertexAttrib4NubvARB := wglGetProcAddress('glVertexAttrib4NubvARB');
    if not Assigned(glVertexAttrib4NubvARB) then Exit;
    @glVertexAttrib4NusvARB := wglGetProcAddress('glVertexAttrib4NusvARB');
    if not Assigned(glVertexAttrib4NusvARB) then Exit;
    @glVertexAttrib4NuivARB := wglGetProcAddress('glVertexAttrib4NuivARB');
    if not Assigned(glVertexAttrib4NuivARB) then Exit;
    @glVertexAttribPointerARB := wglGetProcAddress('glVertexAttribPointerARB');
    if not Assigned(glVertexAttribPointerARB) then Exit;
    @glEnableVertexAttribArrayARB := wglGetProcAddress('glEnableVertexAttribArrayARB');
    if not Assigned(glEnableVertexAttribArrayARB) then Exit;
    @glDisableVertexAttribArrayARB := wglGetProcAddress('glDisableVertexAttribArrayARB');
    if not Assigned(glDisableVertexAttribArrayARB) then Exit;
    @glBindAttribLocationARB := wglGetProcAddress('glBindAttribLocationARB');
    if not Assigned(glBindAttribLocationARB) then Exit;
    @glGetActiveAttribARB := wglGetProcAddress('glGetActiveAttribARB');
    if not Assigned(glGetActiveAttribARB) then Exit;
    @glGetAttribLocationARB := wglGetProcAddress('glGetAttribLocationARB');
    if not Assigned(glGetAttribLocationARB) then Exit;
    @glGetVertexAttribdvARB := wglGetProcAddress('glGetVertexAttribdvARB');
    if not Assigned(glGetVertexAttribdvARB) then Exit;
    @glGetVertexAttribfvARB := wglGetProcAddress('glGetVertexAttribfvARB');
    if not Assigned(glGetVertexAttribfvARB) then Exit;
    @glGetVertexAttribivARB := wglGetProcAddress('glGetVertexAttribivARB');
    if not Assigned(glGetVertexAttribivARB) then Exit;
    @glGetVertexAttribPointervARB := wglGetProcAddress('glGetVertexAttribPointervARB');
    if not Assigned(glGetVertexAttribPointervARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_fragment_shader: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_fragment_shader', extstring) then
  begin
    Result := TRUE;
  end;

end;
function Load_GL_ARB_shading_language_100: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_shading_language_100', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_window_pos: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_window_pos', extstring) then
  begin
    glWindowPos2dARB := wglGetProcAddress('glWindowPos2dARB');
    if not Assigned(glWindowPos2dARB) then Exit;
    glWindowPos2fARB := wglGetProcAddress('glWindowPos2fARB');
    if not Assigned(glWindowPos2fARB) then Exit;
    glWindowPos2iARB := wglGetProcAddress('glWindowPos2iARB');
    if not Assigned(glWindowPos2iARB) then Exit;
    glWindowPos2sARB := wglGetProcAddress('glWindowPos2sARB');
    if not Assigned(glWindowPos2sARB) then Exit;
    glWindowPos2dvARB := wglGetProcAddress('glWindowPos2dvARB');
    if not Assigned(glWindowPos2dvARB) then Exit;
    glWindowPos2fvARB := wglGetProcAddress('glWindowPos2fvARB');
    if not Assigned(glWindowPos2fvARB) then Exit;
    glWindowPos2ivARB := wglGetProcAddress('glWindowPos2ivARB');
    if not Assigned(glWindowPos2ivARB) then Exit;
    glWindowPos2svARB := wglGetProcAddress('glWindowPos2svARB');
    if not Assigned(glWindowPos2svARB) then Exit;
    glWindowPos3dARB := wglGetProcAddress('glWindowPos3dARB');
    if not Assigned(glWindowPos3dARB) then Exit;
    glWindowPos3fARB := wglGetProcAddress('glWindowPos3fARB');
    if not Assigned(glWindowPos3fARB) then Exit;
    glWindowPos3iARB := wglGetProcAddress('glWindowPos3iARB');
    if not Assigned(glWindowPos3iARB) then Exit;
    glWindowPos3sARB := wglGetProcAddress('glWindowPos3sARB');
    if not Assigned(glWindowPos3sARB) then Exit;
    glWindowPos3dvARB := wglGetProcAddress('glWindowPos3dvARB');
    if not Assigned(glWindowPos3dvARB) then Exit;
    glWindowPos3fvARB := wglGetProcAddress('glWindowPos3fvARB');
    if not Assigned(glWindowPos3fvARB) then Exit;
    glWindowPos3ivARB := wglGetProcAddress('glWindowPos3ivARB');
    if not Assigned(glWindowPos3ivARB) then Exit;
    glWindowPos3svARB := wglGetProcAddress('glWindowPos3svARB');
    if not Assigned(glWindowPos3svARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_MESA_window_pos: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_MESA_window_pos', extstring) then
  begin
    glWindowPos2dMESA := wglGetProcAddress('glWindowPos2dMESA');
    if not Assigned(glWindowPos2dMESA) then Exit;
    glWindowPos2fMESA := wglGetProcAddress('glWindowPos2fMESA');
    if not Assigned(glWindowPos2fMESA) then Exit;
    glWindowPos2iMESA := wglGetProcAddress('glWindowPos2iMESA');
    if not Assigned(glWindowPos2iMESA) then Exit;
    glWindowPos2sMESA := wglGetProcAddress('glWindowPos2sMESA');
    if not Assigned(glWindowPos2sMESA) then Exit;
    glWindowPos2ivMESA := wglGetProcAddress('glWindowPos2ivMESA');
    if not Assigned(glWindowPos2ivMESA) then Exit;
    glWindowPos2svMESA := wglGetProcAddress('glWindowPos2svMESA');
    if not Assigned(glWindowPos2svMESA) then Exit;
    glWindowPos2fvMESA := wglGetProcAddress('glWindowPos2fvMESA');
    if not Assigned(glWindowPos2fvMESA) then Exit;
    glWindowPos2dvMESA := wglGetProcAddress('glWindowPos2dvMESA');
    if not Assigned(glWindowPos2dvMESA) then Exit;
    glWindowPos3iMESA := wglGetProcAddress('glWindowPos3iMESA');
    if not Assigned(glWindowPos3iMESA) then Exit;
    glWindowPos3sMESA := wglGetProcAddress('glWindowPos3sMESA');
    if not Assigned(glWindowPos3sMESA) then Exit;
    glWindowPos3fMESA := wglGetProcAddress('glWindowPos3fMESA');
    if not Assigned(glWindowPos3fMESA) then Exit;
    glWindowPos3dMESA := wglGetProcAddress('glWindowPos3dMESA');
    if not Assigned(glWindowPos3dMESA) then Exit;
    glWindowPos3ivMESA := wglGetProcAddress('glWindowPos3ivMESA');
    if not Assigned(glWindowPos3ivMESA) then Exit;
    glWindowPos3svMESA := wglGetProcAddress('glWindowPos3svMESA');
    if not Assigned(glWindowPos3svMESA) then Exit;
    glWindowPos3fvMESA := wglGetProcAddress('glWindowPos3fvMESA');
    if not Assigned(glWindowPos3fvMESA) then Exit;
    glWindowPos3dvMESA := wglGetProcAddress('glWindowPos3dvMESA');
    if not Assigned(glWindowPos3dvMESA) then Exit;
    glWindowPos4iMESA := wglGetProcAddress('glWindowPos4iMESA');
    if not Assigned(glWindowPos4iMESA) then Exit;
    glWindowPos4sMESA := wglGetProcAddress('glWindowPos4sMESA');
    if not Assigned(glWindowPos4sMESA) then Exit;
    glWindowPos4fMESA := wglGetProcAddress('glWindowPos4fMESA');
    if not Assigned(glWindowPos4fMESA) then Exit;
    glWindowPos4dMESA := wglGetProcAddress('glWindowPos4dMESA');
    if not Assigned(glWindowPos4dMESA) then Exit;
    glWindowPos4ivMESA := wglGetProcAddress('glWindowPos4ivMESA');
    if not Assigned(glWindowPos4ivMESA) then Exit;
    glWindowPos4svMESA := wglGetProcAddress('glWindowPos4svMESA');
    if not Assigned(glWindowPos4svMESA) then Exit;
    glWindowPos4fvMESA := wglGetProcAddress('glWindowPos4fvMESA');
    if not Assigned(glWindowPos4fvMESA) then Exit;
    glWindowPos4dvMESA := wglGetProcAddress('glWindowPos4dvMESA');
    if not Assigned(glWindowPos4dvMESA) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_shadow: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_shadow', extstring) then
  begin
    Result := TRUE;
  end;
end;

function Load_GL_EXT_fog_coord: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_fog_coord', extstring) then
  begin
    glFogCoordfEXT := wglGetProcAddress('glFogCoordfEXT');
    if not Assigned(glFogCoordfEXT) then Exit;
    glFogCoorddEXT := wglGetProcAddress('glFogCoorddEXT');
    if not Assigned(glFogCoorddEXT) then Exit;
    glFogCoordfvEXT := wglGetProcAddress('glFogCoordfvEXT');
    if not Assigned(glFogCoordfvEXT) then Exit;
    glFogCoorddvEXT := wglGetProcAddress('glFogCoorddvEXT');
    if not Assigned(glFogCoorddvEXT) then Exit;
    glFogCoordPointerEXT := wglGetProcAddress('glFogCoordPointerEXT');
    if not Assigned(glFogCoordPointerEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_multisample_filter_hint: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_NV_multisample_filter_hint', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_WGL_ARB_extensions_string: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := wglGetExtensionsStringARB(wglGetCurrentDC);

  if glext_ExtensionSupported('WGL_ARB_extensions_string', extstring) then
  begin
    wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
    if not Assigned(wglGetExtensionsStringARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_ARB_pixel_format: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := wglGetExtensionsStringARB(wglGetCurrentDC);

  if glext_ExtensionSupported('WGL_ARB_pixel_format', extstring) then
  begin
    wglGetPixelFormatAttribivARB := wglGetProcAddress('wglGetPixelFormatAttribivARB');
    if not Assigned(wglGetPixelFormatAttribivARB) then Exit;
    wglGetPixelFormatAttribfvARB := wglGetProcAddress('wglGetPixelFormatAttribfvARB');
    if not Assigned(wglGetPixelFormatAttribfvARB) then Exit;
    wglChoosePixelFormatARB := wglGetProcAddress('wglChoosePixelFormatARB');
    if not Assigned(wglChoosePixelFormatARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_stencil_wrap: Boolean;
var
  extstring: AnsiString;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_stencil_wrap', extstring) then
  begin
    Result := TRUE;
  end;
end;

function Load_GL_ATI_separate_stencil: Boolean;
var
  extstring: AnsiString;
begin
  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ATI_separate_stencil', extstring) then
  begin
    @glStencilOpSeparateATI := wglGetProcAddress('glStencilOpSeparateATI');
    if not Assigned(glStencilOpSeparateATI) then Exit;
    @glStencilFuncSeparateATI := wglGetProcAddress('glStencilFuncSeparateATI');
    if not Assigned(glStencilFuncSeparateATI) then Exit;
    Result := TRUE;
  end;
end;

{$endif read_implementation}

{$endif}
{$endif}
