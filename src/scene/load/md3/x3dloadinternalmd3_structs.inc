{%MainUnit x3dloadinternalmd3.pas}
{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Structures matching MD3 file binary layout. }

const
  Md3MaxQPath = 64;
  Md3Fps = 30;

type
  TMd3Triangle = record
    Indexes: array [0..2] of UInt32;
  end;
  TMd3TriangleList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Triangle>;

  TMd3Vertex = record
    Position: array [0..2] of SmallInt;
    Normal: SmallInt;
  end;
  PMd3Vertex = ^TMd3Vertex;
  TMd3VertexList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Vertex>;

  TMd3TexCoord = TVector2;
  TMd3TexCoordList = TVector2List;

  TMd3Shader = record
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    ShaderIndex: UInt32;
  end;
  TMd3ShaderList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Shader>;

  TMd3Tag = record
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    Origin: TVector3;
    Orientation: TMatrix3;
  end;
  PMd3Tag = ^TMd3Tag;
  TMd3TagList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Tag>;
