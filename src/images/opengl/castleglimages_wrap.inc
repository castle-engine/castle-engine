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

{ Part of CastleGLImages unit: texture wrapping modes. }

{$ifdef read_interface}

type
  { }
  TTextureWrap2D = array [0..1] of TGLenum;
  TTextureWrap3D = array [0..2] of TGLenum;

operator = (const W1, W2: TTextureWrap2D): boolean;
operator = (const W1, W2: TTextureWrap3D): boolean;

const
  Texture2DRepeat: TTextureWrap2D = (GL_REPEAT, GL_REPEAT);

{ Return wrap GL_CLAMP_TO_EDGE in both directions. }
function Texture2DClampToEdge: TTextureWrap2D;

{$endif read_interface}

{$ifdef read_implementation}

operator = (const W1, W2: TTextureWrap2D): boolean;
begin
  Result := CompareMem(@W1, @W2, SizeOf(W1));
end;

operator = (const W1, W2: TTextureWrap3D): boolean;
begin
  Result := CompareMem(@W1, @W2, SizeOf(W1));
end;

function Texture2DClampToEdge: TTextureWrap2D;
begin
  Result[0] := GLFeatures.CLAMP_TO_EDGE;
  Result[1] := Result[0];
end;

{$endif read_implementation}
