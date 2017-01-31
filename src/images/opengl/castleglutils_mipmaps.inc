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

{ Part of CastleGLUtils unit: mipmaps utils. }

{$ifdef read_interface}

type
  EGenerateMipmapNotAvailable = class(Exception);

{ Is GenerateMipmap avaiable. This checks some GL extensions/versions that
  give us glGenerateMipmap or glGenerateMipmapEXT call, used by GenerateMipmap. }
function HasGenerateMipmap: boolean;

{ Call glGenerateMipmap (or analogous function from some OpenGL extension).

  @raises(EGenerateMipmapNotAvailable If no glGenerateMipmap version
    is available on this OpenGL version. If you don't want to get
    this exception, you can always check HasGenerateMipmap
    before calling this.) }
procedure GenerateMipmap(target: TGLenum);

{$endif read_interface}

{$ifdef read_implementation}

{ GenerateMipmap ------------------------------------------------------------- }

{ $define TEST_NO_GENERATE_MIPMAP}

function HasGenerateMipmap: boolean;
{$ifdef TEST_NO_GENERATE_MIPMAP}
begin
  Result := false;
{$else}
begin
  Result := (GLFeatures.Framebuffer <> gsNone) and (not GLVersion.BuggyGenerateMipmap);
{$endif}
end;

procedure GenerateMipmap(target: TGLenum);
begin
  {$ifndef TEST_NO_GENERATE_MIPMAP}
  if GLFeatures.Framebuffer <> gsNone then
  begin
    {$ifndef OpenGLES}
    glPushAttrib(GL_ENABLE_BIT);
      { To work under fglrx (confirmed on chantal (ATI Mobility Radeon X1600)),
        we have to temporarily enable target.
        At least with EXT_framebuffer_object.
        This is a known ATI drivers problem:
        http://www.opengl.org/discussion_boards/ubbthreads.php?ubb=showflat&Number=237052 }
      glEnable(Target);
      case GLFeatures.Framebuffer of
        gsExtension: glGenerateMipmapEXT(Target);
        gsStandard : glGenerateMipmap   (Target);
      end;
    glPopAttrib;
    {$else}
    glGenerateMipmap(Target);
    {$endif}
  end else
  {$endif}
    raise EGenerateMipmapNotAvailable.Create('Framebuffer not supported, glGenerateMipmap[EXT] not available');
end;

{$endif read_implementation}
