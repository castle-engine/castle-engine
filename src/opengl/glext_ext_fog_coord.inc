{ See
  http://www.freepascal.org/mantis/view.php?id=10460
  for NEEDS_FOG_COORD_FIX explanation. }

{$ifdef VER2_0} {$define NEEDS_FOG_COORD_FIX} {$endif}
{$ifdef VER2_2} {$define NEEDS_FOG_COORD_FIX} {$endif}

{$ifdef NEEDS_FOG_COORD_FIX}

{$ifdef read_interface}
var
  glFogCoordfEXT: procedure(coord: GLfloat); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFogCoorddEXT: procedure(coord: GLdouble); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFogCoordfvEXT: procedure(coord: PGLfloat); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFogCoorddvEXT: procedure(coord: PGLdouble); {$ifdef OPENGL_CDECL} cdecl; {$endif} {$ifdef OPENGL_STDCALL} stdcall; {$endif}
{$endif read_interface}

{$ifdef read_implementation}
function Load_GL_EXT_fog_coord: Boolean;
var
  extstring: String;
begin

  Result := FALSE;
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_fog_coord', extstring) then
  begin
    Pointer(glFogCoordfEXT) := wglGetProcAddress('glFogCoordfEXT');
    if not Assigned(glFogCoordfEXT) then Exit;
    Pointer(glFogCoorddEXT) := wglGetProcAddress('glFogCoorddEXT');
    if not Assigned(glFogCoorddEXT) then Exit;
    Pointer(glFogCoordfvEXT) := wglGetProcAddress('glFogCoordfvEXT');
    if not Assigned(glFogCoordfvEXT) then Exit;
    Pointer(glFogCoorddvEXT) := wglGetProcAddress('glFogCoorddvEXT');
    if not Assigned(glFogCoorddvEXT) then Exit;
    Pointer(glFogCoordPointerEXT) := wglGetProcAddress('glFogCoordPointerEXT');
    if not Assigned(glFogCoordPointerEXT) then Exit;
    Result := TRUE;
  end;

end;
{$endif read_implementation}

{$endif NEEDS_FOG_COORD_FIX}
