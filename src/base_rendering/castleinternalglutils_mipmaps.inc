{%MainUnit castleinternalglutils.pas}
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

{ Part of CastleGLUtils unit: mipmaps utils. }

{$ifdef read_interface}

type
  EGenerateMipmapNotAvailable = class(Exception);

{ Is GenerateMipmap call avaiable. This checks some GL extensions/versions that
  give us glGenerateMipmap. }
function HasGenerateMipmap: boolean;

{ Call glGenerateMipmap.

  @raises(EGenerateMipmapNotAvailable If glGenerateMipmap
    is available on this OpenGL version. If you don't want to get
    this exception, you can always check HasGenerateMipmap
    before calling this.) }
procedure GenerateMipmap(target: TGLenum);

{$endif read_interface}

{$ifdef read_implementation}

{ GenerateMipmap ------------------------------------------------------------- }

function HasGenerateMipmap: boolean;
begin
  Result := GLFeatures.Framebuffer;
end;

procedure GenerateMipmap(target: TGLenum);
begin
  if GLFeatures.Framebuffer then
  begin
    Assert(Assigned(glGenerateMipmap));
    glGenerateMipmap(Target);
  end else
    raise EGenerateMipmapNotAvailable.Create('Framebuffer not supported, glGenerateMipmap not available');
end;

{$endif read_implementation}
