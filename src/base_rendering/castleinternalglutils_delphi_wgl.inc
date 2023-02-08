{%MainUnit castleinternalglutils.pas}
{
  Copyright 2021-2023 Michalis Kamburelis.

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
function Load_GL_ARB_texture_multisample(LoadAsCore: boolean = false): Boolean;
function Load_WGL_ARB_extensions_string: Boolean;
function Load_WGL_ARB_pixel_format: Boolean;

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

{$endif read_implementation}

{$endif}
{$endif}
