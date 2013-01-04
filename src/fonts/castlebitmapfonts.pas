{
  Copyright 2002-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Bitmap fonts types.

  Bitmap fonts are a 2D bit array, indicating which pixel is drawn.
  They may be represented using types in this unit.
  We also have CastleFont2Pascal unit that can even convert such fonts
  to Pascal units, to embed fonts inside Pascal code.

  Types in this unit were defined with the idea to
  easily use them with OpenGL @code(glBitmap) and @code(glPixelStorei)
  procedures (see e.g. unit @link(CastleGLBitmapFonts) that does exactly this).
  However, this unit does @italic(not) depend on OpenGL, it's generally-usable.
}

unit CastleBitmapFonts;

interface

type
  TBitmapCharInfo = record
    { Alignment can be used e.g. with OpenGL's
      glPixelStorei(GL_UNPACK_ALIGNMENT, ...).
      This is always  1, 2, 4 or 8. }
    Alignment: Cardinal;

    { @groupbegin
      These fields can be used e.g. with OpenGL's glBitmap }
    XOrig, YOrig, XMove, YMove: Single;
    Width, Height: Cardinal;
    { @groupend }
  end;

  { @name record defines the bitmap character.
    @name will not be directly used as a type of some variable.
    Instead, we use it only to define @link(PBitmapChar).

    Sample character may look like this :

    @longcode(#
      CharX = record
        Info: TBitmapCharInfo
        Data: packed array[0..23]of byte;
      end =
      ( Info : ( Alignment : 2;
                 XOrig : 0; YOrig : 0;
                 XMove : 10; YMove : 0;
                 Width : 8; Height : 12 );
        Data : ($AA, $00,
                $BB, $00,
                ....
               );
      );
    #)

    Notes about example above: The row has
    @link(TBitmapCharInfo.Width Width) = 8 which means that
    we need only 8 bits (one byte) to store it.
    But since @link(TBitmapCharInfo.Alignment Alignment) = 2,
    we need to use some multiply of 2 bytes
    to store a row, which means that each row actually uses 2 bytes
    (even though the contents of 2nd byte are always meaningless).
    E.g. if you will pass this data to OpenGL, you should appropriately
    use @code(glPixelStorei), so that OpenGL knows that data is aligned
    to 2 bytes, and then 2nd byte of each row will indeed be ignored
    by OpenGL.

    Sure, this is rather extreme example, since in this
    case we're wasting half memory just to get Alignment = 2.
    Well, this is only an example. Also, Alignment may sometimes
    give us better speed. E.g. for OpenGL's glBitmap (even though
    if you will use OpenGL's display list, it will not actually
    matter that much, since then OpenGL will probably internally convert
    to the best alignment anyway).

    Summing the example, we have 2 bytes and 12 rows
    (@link(TBitmapCharInfo.Height Height) = 2)
    so we need 24 bytes to store this character.

    The precise formula to calculate how many bytes are used to store
    a character is

    @unorderedList(
      @item( @code(RoundUpToMultiple( (Char.Info.Width + 7) div 8,
        Char.Info.Alignment ) * Char.Info.Height) )
      @item( Or, more readable, @code(
        BitmapCharRowByteLength(Char.Info.Width, Char.Info.Alignment)
          * Char.Info.Height ) )
      @item( Or, even more readable, just @code(
        BitmapCharRowByteLength(Char) * Char.Info.Height) )
    )

    Rows are specified in @link(Data) from lower to upper
    (just like OpenGL's @code(glBitmap) likes).

    Note that this is @italic(packed) record so I'm sure that

    @longcode(#
      SizeOf(record
        Info: TBitmapCharInfo;
        Data: packed array[0..n]of byte;
      ) = SizeOf(TBitmapCharInfo) + (n+1) * SizeOf(Byte)
    #)

    I.e. there's no padding between Info and Data. }
  TBitmapChar = packed record
    Info: TBitmapCharInfo;
    Data: packed array[0 .. MaxInt - 1 - SizeOf(TBitmapCharInfo)]of byte;
  end;
  PBitmapChar = ^TBitmapChar;

  TBitmapFontArray = array [char] of PBitmapChar;
  TBitmapFont = class
  public
    Data: TBitmapFontArray;
  end;

function BitmapCharRowByteLength(Width, Alignment: Cardinal): Cardinal; overload;
function BitmapCharRowByteLength(BitmapChar: PBitmapChar): Cardinal; overload;

implementation

uses CastleUtils;

function BitmapCharRowByteLength(Width, Alignment: Cardinal): Cardinal;
begin
 Result := RoundUpToMultiply((Width + 7) div 8, Alignment);
end;

function BitmapCharRowByteLength(BitmapChar: PBitmapChar): Cardinal;
begin
 Result := BitmapCharRowByteLength(BitmapChar^.Info.Width, BitmapChar^.Info.Alignment);
end;

end.
