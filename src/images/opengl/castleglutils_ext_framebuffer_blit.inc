{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: EXT_framebuffer_blit constants and routines. }

{$ifdef read_interface}

const
  GL_READ_FRAMEBUFFER_EXT         = $8CA8;
  GL_DRAW_FRAMEBUFFER_EXT         = $8CA9;
  GL_DRAW_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_READ_FRAMEBUFFER_BINDING_EXT = $8CAA;

var
  glBlitFramebufferEXT: procedure (
    srcX0, srcY0, srcX1, srcY1,
    dstX0, dstY0, dstX1, dstY1: TGLint;
    mask: TGLbitfield;
    filter: TGLenum);

{ Check the existence of OpenGL EXT_framebuffer_blit extension.
  See https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_framebuffer_blit.txt . }
function Load_GL_EXT_framebuffer_blit: Boolean;

{$endif read_interface}

{$ifdef read_implementation}

{ Written with the same formatting as FPC glext.pp unit. }

function Load_GL_EXT_framebuffer_blit: Boolean;
{$ifndef OpenGLES}
var
  extstring: String;
{$endif}
begin

  Result := FALSE;

  {$ifndef OpenGLES}
  extstring := String(PChar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_framebuffer_blit', extstring) then
  begin
    {$ifdef FPC}Pointer({$endif}glBlitFramebufferEXT{$ifdef FPC}){$endif} := wglGetProcAddress('glBlitFramebufferEXT');
    if not Assigned(glBlitFramebufferEXT) then Exit;
    Result := TRUE;
  end;
  {$endif}

end;

{$endif read_implementation}
