{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    PNG reader/writer common code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit PNGComn;

interface

uses SysUtils, FPImage, FPImgCmn;

type

  PNGImageException = class (FPImageException);

  TChunkTypes = (
    ctIHDR,  ctcHRM,  ctgAMA,  ctsBIT,
    ctPLTE,  ctbKGD,  cthIST,  cttRNS,
    ctoFFs,  ctpHYs,  ctIDAT,  cttIME,
    ctsCAL,  cttEXt,  ctzTXt,  ctIEND,
    ctsRGB,  ctiCCP,  ctiTXt,  ctsPLT,
    ctUnknown
    );

  EightLong = array[0..7] of longword;
  TChunkCode = array[0..3] of char;

  TChunk = record
    acapacity, alength, CRC : longword;
    ReadType : TChunkCode;
    data : PByteArray;
    aType : TChunkTypes;
  end;

  TChunkHeader = record
    CLength : longword;
    CType : TChunkCode;
  end;

  THeaderChunk = record
    Width, height : longword;
    BitDepth, ColorType, Compression, Filter, Interlace : byte;
  end;

const

  Signature    : Array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);

  MaxChunkLength = $7FFFFFFF;
  All1Bits : longword = $FFFFFFFF;

  ChunkTypes : array[TChunkTypes] of TChunkCode = (
    'IHDR',  'cHRM',  'gAMA',  'sBIT',
    'PLTE',  'bKGD',  'hIST',  'tRNS',
    'oFFs',  'pHYs',  'IDAT',  'tIME',
    'sCAL',  'tEXt',  'zTXt',  'IEND',
    'sRGB',  'iCCP',  'iTXt',  'sPLT',
    'Unkn'
    );

  ChunkAncillary = $10000000;
  ChunkPrivate   = $00100000;
  ChunkReserved  = $00001000;
  ChunkSafeCopy  = $00000010;


  StartRow     : Array[0..7] of Integer = (0, 0, 0, 4, 0, 2, 0, 1);
  StartCol     : Array[0..7] of Integer = (0, 0, 4, 0, 2, 0, 1, 0);
  RowInc       : Array[0..7] of Integer = (1, 8, 8, 8, 4, 4, 2, 2);
  ColInc       : Array[0..7] of Integer = (1, 8, 8, 4, 4, 2, 2, 1);
  BlockHght    : Array[0..7] of Integer = (1, 8, 8, 4, 4, 2, 2, 1);
  BlockWdth    : Array[0..7] of Integer = (1, 8, 4, 4, 2, 2, 1, 1);

implementation

end.
