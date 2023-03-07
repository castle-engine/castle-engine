{%MainUnit castleinternalglutils.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: Missing WGL stuff (for both Delphi and FPC). }

{$ifdef MSWINDOWS}

{$ifdef read_interface}

const
  // WGL_ARB_create_context
  WGL_CONTEXT_DEBUG_BIT_ARB = $00000001;
  WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $00000002;
  WGL_CONTEXT_MAJOR_VERSION_ARB = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB = $2092;
  WGL_CONTEXT_LAYER_PLANE_ARB = $2093;
  WGL_CONTEXT_FLAGS_ARB = $2094;
  ERROR_INVALID_VERSION_ARB = $2095;

  // WGL_ARB_create_context_profile
  WGL_CONTEXT_PROFILE_MASK_ARB = $9126;
  WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;
  ERROR_INVALID_PROFILE_ARB = $2096;

var
  wglCreateContextAttribsARB: function(hDC: HDC; hShareContext: HGLRC; const attribList: PGLint): HGLRC; stdcall;

function Load_WGL_ARB_create_context(const WglExtensions: String): Boolean;
function Load_WGL_ARB_create_context_profile(const WglExtensions: String): Boolean;
function Load_WGL_ARB_multisample(const WglExtensions: String): Boolean;


{$endif read_interface}

{$ifdef read_implementation}

function Load_WGL_ARB_create_context(const WglExtensions: String): Boolean;
begin
  Result := false;
  if glext_ExtensionSupported('WGL_ARB_create_context', WglExtensions) then
  begin
    Pointer({$ifndef FPC}@{$endif} wglCreateContextAttribsARB) := wglGetProcAddress('wglCreateContextAttribsARB');
    if not Assigned(wglCreateContextAttribsARB) then Exit;
    Result := true;
  end;
end;

function Load_WGL_ARB_create_context_profile(const WglExtensions: String): Boolean;
begin
  Result := glext_ExtensionSupported('WGL_ARB_create_context_profile', WglExtensions);
end;

function Load_WGL_ARB_multisample(const WglExtensions: String): Boolean;
begin
  Result := glext_ExtensionSupported('WGL_ARB_multisample', WglExtensions);
end;

{$endif read_implementation}

{$endif MSWINDOWS}
