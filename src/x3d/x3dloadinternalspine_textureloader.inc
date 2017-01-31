{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  TQuadTexCoord = array [0..3] of TVector2Single;
  TQuadTexRect = array [0..1] of TVector2Single;

  { Abstract provider of X3D Apperance nodes based on Spine attachment name
    (using atlas or not, depending on descendant, like TAtlas or
    TSimpleTextureLoader). }
  TTextureLoader = class abstract
  public
    { Create an X3D node for given AttachmentName.
      May return @nil, and then TexCoord and TexRect are left undefined. }
    function UseNode(const AttachmentName: string;
      out TexCoord: TQuadTexCoord; out TexRect: TQuadTexRect;
      out TexRotated: boolean): TImageTextureNode; virtual; abstract;
  end;
