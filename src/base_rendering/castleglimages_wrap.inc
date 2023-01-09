{%MainUnit castleglimages.pas}
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

{ Part of CastleGLImages unit: texture wrapping modes. }

{$ifdef read_interface}

{$ifdef read_interface_type}
//type - don't add type Delphi can't parse that correctly
  { }
  TTextureWrap2D = packed record
    Data: packed array [0..1] of TGLenum;
    class operator {$ifdef FPC}={$else}Equal{$endif} (const W1, W2: TTextureWrap2D): boolean;
  end;

  TTextureWrap3D = packed record
    Data: packed array [0..2] of TGLenum;
    class operator {$ifdef FPC}={$else}Equal{$endif} (const W1, W2: TTextureWrap3D): boolean;
  end;
{$endif read_interface_type}

{$ifdef read_interface_const}
const
  Texture2DRepeat: TTextureWrap2D = (Data: (GL_REPEAT, GL_REPEAT));
{$endif read_interface_const}

{$ifdef read_interface_func}
{ Return wrap GL_CLAMP_TO_EDGE in both directions. }
function Texture2DClampToEdge: TTextureWrap2D;
{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

class operator TTextureWrap2D.{$ifdef FPC}={$else}Equal{$endif} (const W1, W2: TTextureWrap2D): boolean;
begin
  Result := CompareMem(@W1, @W2, SizeOf(W1));
end;

class operator TTextureWrap3D.{$ifdef FPC}={$else}Equal{$endif} (const W1, W2: TTextureWrap3D): boolean;
begin
  Result := CompareMem(@W1, @W2, SizeOf(W1));
end;

function Texture2DClampToEdge: TTextureWrap2D;
begin
  Result.Data[0] := GLFeatures.CLAMP_TO_EDGE;
  Result.Data[1] := Result.Data[0];
end;

{$endif read_implementation}
