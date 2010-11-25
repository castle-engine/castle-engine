{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Low-level helpers and constants for reading 3DS files. @exclude }
unit Object3DsChunks;

interface

uses VectorMath, Classes, SysUtils;

{ The indentation below corresponds to chunk relations in 3DS file.
  Based on example 3dsRdr.c, with new needed chunks added, based
  on various Internal sources about 3DS and MLI, with some new comments.

  Some subchunks that are required within parent chunks are marked as such. }
const
  { Color chunks may be in various places in 3DS file.
    _GAMMA suffix means that these colors are already gamma corrected.
    @groupBegin }
  CHUNK_RGBF       = $0010;
  CHUNK_RGBB       = $0011;
  CHUNK_RGBB_GAMMA = $0012;
  CHUNK_RGBF_GAMMA = $0013;
  { @groupEnd }

  { CHUNK_DOUBLE_BYTE from MLI specification. Experiments show
    that this has 0..100 range, at least when used for shininess subchunks
    (in other uses, like transparency, the range @italic(may) be smaller).

    lib3ds confirms this, by even calling this as INT_PERCENTAGE instead
    of DOUBLE_BYTE. Used for CHUNK_SHININESS, CHUNK_SHININESS_STRENTH,
    CHUNK_TRANSPARENCY, CHUNK_TRANSPARENCY_FALLOFF, CHUNK_REFLECT_BLUR. }
  CHUNK_DOUBLE_BYTE = $0030;

  { Root file chunks.
    MAIN chunk is the whole 3DS file.
    MLI chunk is the whole MLI (Material-Library) file.
    Probably PRJ chunk is also something like that (some "project" ?).
    @groupBegin }
  CHUNK_PRJ       = $C23D;
  CHUNK_MLI       = $3DAA;
  CHUNK_MAIN      = $4D4D;
  { @groupEnd }

    CHUNK_VERSION   = $0002;
    CHUNK_OBJMESH   = $3D3D;
      CHUNK_BKGCOLOR  = $1200;
      CHUNK_AMBCOLOR  = $2100;
      { As I understand, exactly one of the subchunks TRIMESH, LIGHT
        and CAMERA appears in one OBJBLOCK chunk. } { }
      CHUNK_OBJBLOCK  = $4000;
        CHUNK_TRIMESH   = $4100;
          CHUNK_VERTLIST  = $4110;
          CHUNK_FACELIST  = $4120;
            CHUNK_FACEMAT   = $4130;
          CHUNK_MAPLIST   = $4140; {< texture coordinates }
            CHUNK_SMOOLIST  = $4150;
          CHUNK_TRMATRIX  = $4160;
        CHUNK_LIGHT     = $4600;
          CHUNK_SPOTLIGHT = $4610;
        CHUNK_CAMERA    = $4700;
          CHUNK_HIERARCHY = $4F00;
    CHUNK_VIEWPORT  = $7001;

    CHUNK_MATERIAL  = $AFFF;
      CHUNK_MATNAME   = $A000; {< required; asciiz, no subchunks }
      CHUNK_AMBIENT   = $A010; {< required; subchunks are RGB chunk[s] }
      CHUNK_DIFFUSE   = $A020; {< required; subchunks are RGB chunk[s] }
      CHUNK_SPECULAR  = $A030; {< required; subchunks are RGB chunk[s] }
      CHUNK_SHININESS = $A040;            {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_SHININESS_STRENTH = $A041;    {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_TRANSPARENCY = $A050;         {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_TRANSPARENCY_FALLOFF = $A052; {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_REFLECT_BLUR = $A053;         {< required; subchunks are DOUBLE_BYTE chunk[s] }

      CHUNK_TEXMAP_1  = $A200; {< two texture maps }
      CHUNK_TEXMAP_2  = $A33A;
      CHUNK_BUMPMAP   = $A230;
        { All MAP chunks below can be subchunks of all MAP chunks above. } { }
        CHUNK_MAP_FILE   = $A300; {< asciiz, no subchunks }
        CHUNK_MAP_USCALE = $A356; {< single, no subchunks }
        CHUNK_MAP_VSCALE = $A354; {< single, no subchunks }
        CHUNK_MAP_UOFFSET = $A358; {< single, no subchunks }
        CHUNK_MAP_VOFFSET = $A35A; {< single, no subchunks }

    CHUNK_KEYFRAMER = $B000;
      CHUNK_AMBIENTKEY    = $B001;
      CHUNK_TRACKINFO = $B002;
        CHUNK_TRACKOBJNAME  = $B010;
        CHUNK_TRACKPIVOT    = $B013;
        CHUNK_TRACKPOS      = $B020;
        CHUNK_TRACKROTATE   = $B021;
        CHUNK_TRACKSCALE    = $B022;
        CHUNK_OBJNUMBER     = $B030;
      CHUNK_TRACKCAMERA = $B003;
        CHUNK_TRACKFOV  = $B023;
        CHUNK_TRACKROLL = $B024;
      CHUNK_TRACKCAMTGT = $B004;
      CHUNK_TRACKLIGHT  = $B005;
      CHUNK_TRACKLIGTGT = $B006;
      CHUNK_TRACKSPOTL  = $B007;
      CHUNK_FRAMES    = $B008;

type
  TChunkHeader = packed record
    id: Word;
    len: LongWord;
  end;

  EInvalid3dsFile = class(Exception);

procedure Check3dsFile(TrueValue: boolean; const ErrMessg: string);

{ Read 3DS subchunks until EndPos, ignoring everything except color information.
  It's guaranteed that Stream.Position is EndPos at the end.

  Returns did we find any color information. If @false,
  Col is left not modified (it's intentionally a "var" parameter,
  not an "out" parameter).

  Overloaded version with 4 components always returns alpha = 1.
  @groupBegin }
function TryReadColorInSubchunks(var Col: TVector3Single;
  Stream: TStream; EndPos: Int64): boolean; overload;
function TryReadColorInSubchunks(var Col: TVector4Single;
  Stream: TStream; EndPos: Int64): boolean; overload;
{ @groupEnd }

{ Read 3DS subchunks until EndPos, ignoring everything except CHUNK_DOUBLE_BYTE
  value. Returns the value / 100.
  Similar comments as for TryReadColorInSubchunks. }
function TryReadPercentageInSubchunks(var Value: Single;
  Stream: TStream; EndPos: Int64): boolean;

implementation

procedure Check3dsFile(TrueValue: boolean; const ErrMessg: string);
begin
 if not TrueValue then raise EInvalid3dsFile.Create(ErrMessg);
end;

function TryReadColorInSubchunks(var Col: TVector3Single;
  Stream: TStream; EndPos: Int64): boolean;
var h: TChunkHeader;
    hEnd: Int64;
    Col3Byte: TVector3Byte;
begin
 result := false;
 while Stream.Position < EndPos do
 begin
  Stream.ReadBuffer(h, SizeOf(h));
  hEnd := Stream.Position -SizeOf(TChunkHeader) + h.len;
  { TODO: we ignore gamma correction entirely so we don't distinct
    gamma corrected and not corrected colors }
  case h.id of
   CHUNK_RGBF, CHUNK_RGBF_GAMMA:
     begin
      Stream.ReadBuffer(Col, SizeOf(Col));
      result := true;
      break;
     end;
   CHUNK_RGBB, CHUNK_RGBB_GAMMA:
     begin
      Stream.ReadBuffer(Col3Byte, SizeOf(Col3Byte));
      Col := Vector3Single(Col3Byte);
      result := true;
      break;
     end;
   else Stream.Position := hEnd;
  end;
 end;
 Stream.Position := EndPos;
end;

function TryReadColorInSubchunks(var Col: TVector4Single;
  Stream: TStream; EndPos: Int64): boolean;
var Col3Single: TVector3Single;
begin
 result := TryReadColorInSubchunks(Col3Single, Stream, EndPos);
 if result then Col := Vector4Single(Col3Single);
end;

function TryReadPercentageInSubchunks(var Value: Single;
  Stream: TStream; EndPos: Int64): boolean;
type T3dsDoubleByte = SmallInt;
var h: TChunkHeader;
    hEnd: Int64;
    DoubleByte: T3dsDoubleByte;
begin
 result := false;
 while Stream.Position < EndPos do
 begin
  Stream.ReadBuffer(h, SizeOf(h));
  hEnd := Stream.Position -SizeOf(TChunkHeader) + h.len;
  if h.id = CHUNK_DOUBLE_BYTE then
  begin
   Stream.ReadBuffer(DoubleByte, SizeOf(DoubleByte));
   result := true;
   break;
  end else
   Stream.Position := hEnd;
 end;
 Stream.Position := EndPos;
 if result then Value := DoubleByte/100;
end;

end.
