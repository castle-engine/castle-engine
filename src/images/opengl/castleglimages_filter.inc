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

{ Part of CastleGLImages unit: texture filtering (TTextureFilter and friends). }

{$ifdef read_interface}

type
  { Texture minification filter (what happens when many texture pixels
    are squeezed in one screen pixel). }
  TMinificationFilter = (
    minNearest,
    minLinear,
    minNearestMipmapNearest,
    minNearestMipmapLinear,
    minLinearMipmapNearest,
    minLinearMipmapLinear);

  { Texture magnification filter (what happens when a single texture pixel
    in stretched over many screen pixels). }
  TMagnificationFilter = (magNearest, magLinear);

  TTextureFilter = object
  public
    Magnification: TMagnificationFilter;
    Minification: TMinificationFilter;
    function NeedsMipmaps: boolean;
    procedure DisableMipmaps;
  end;

operator = (const V1, V2: TTextureFilter): boolean;

function TextureFilter(const Minification: TMinificationFilter;
  const Magnification: TMagnificationFilter): TTextureFilter;

{ Set current texture minification and magnification filter.

  This is just a thin wrapper for calling
  @longCode(#
    glTexParameteri(Target, GL_TEXTURE_MIN_FILTER, ...);
    glTexParameteri(Target, GL_TEXTURE_MAG_FILTER, ...);
  #) }
procedure SetTextureFilter(const Target: TGLenum; const Filter: TTextureFilter);

{$endif read_interface}

{$ifdef read_implementation}

procedure SetTextureFilter(const Target: TGLenum; const Filter: TTextureFilter);
const
  MinFilterGL: array [TMinificationFilter] of TGLint =
  ( GL_NEAREST,
    GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_LINEAR );
  MagFilterGL: array [TMagnificationFilter] of TGLint =
  ( GL_NEAREST,
    GL_LINEAR );
begin
  glTexParameteri(Target, GL_TEXTURE_MIN_FILTER, MinFilterGL[Filter.Minification]);
  glTexParameteri(Target, GL_TEXTURE_MAG_FILTER, MagFilterGL[Filter.Magnification]);
end;

function TTextureFilter.NeedsMipmaps: boolean;
begin
  Result := Minification in
    [ minNearestMipmapNearest,
      minNearestMipmapLinear,
      minLinearMipmapNearest,
      minLinearMipmapLinear ];
end;

procedure TTextureFilter.DisableMipmaps;
begin
  if NeedsMipmaps then
  begin
    Minification := minLinear;
    Assert(not NeedsMipmaps);
  end;
end;

operator = (const V1, V2: TTextureFilter): boolean;
begin
  Result :=
    (V1.Minification  = V2.Minification ) and
    (V1.Magnification = V2.Magnification);
end;

function TextureFilter(const Minification: TMinificationFilter;
  const Magnification: TMagnificationFilter): TTextureFilter;
begin
  Result.Minification := Minification;
  Result.Magnification := Magnification;
end;

{$endif read_implementation}
