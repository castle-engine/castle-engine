{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Michael Van Canneyt of the Free Pascal development team

    TARGA common definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{$mode objfpc}
{$h+}
unit targacmn;

interface

Const
  KeyIdentification = 'ID';

Type
  TWordRec = Packed Record
    Lo,Hi : byte;
  end;

  TTargaHeader = packed record
    IDLen        : Byte;
    MapType      : Byte;
    ImgType      : Byte;
    MapStart     : TWordRec;
    MapLength    : TWordRec;
    MapEntrySize : Byte;
    OriginX      : TWordrec;
    OriginY      : TWordRec;
    Width        : TWordRec;
    Height       : TWordRec;
    PixelSize    : Byte;
    Flags        : Byte;
  end;

  TBGREntry = packed record
    Blue, Green, Red : Byte;
  end;

  TBGRAEntry = packed record
    Blue, Green, Red, Alpha : Byte;
  end;

Function ToWord(AWord : TWordRec) : Word;
Function FromWord(AWord : Word) : TWordRec;

implementation

Function ToWord(AWord : TWordRec) : Word;

begin
  Result:=(AWord.Lo) or (AWord.Hi shl 8);
end;

Function FromWord(AWord : Word) : TWordRec;

begin
  Result.Lo:=AWord and $FF;
  Result.Hi:=(AWord shr 8) and $FF;
end;

end.
