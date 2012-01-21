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

{ Types to define bitmap fonts as constants in the Pascal code.

  Concrete examples of fonts defined using these types
  may be found in bfnt_xxx units.
  You can generate such bfnt_xxx units from *.ttf fonts
  using my font2pascal program.

  Types in this unit were defined with the idea to
  easily use them with OpenGL @code(glBitmap) and @code(glPixelStorei)
  procedures (see e.g. unit @link(OpenGLBmpFonts) that does exactly this).
  However, this unit does @italic(not) depend on OpenGL, it's generally-usable.
}

unit BmpFontsTypes;

interface

type
  TBFNTCharInfo = record
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
    Instead, we use it only to define @link(PBFNTChar).

    Sample character may look like this :

    @longcode(#
      CharX = record
        Info: TBFNTCharInfo
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
    @link(TBFNTCharInfo.Width Width) = 8 which means that
    we need only 8 bits (one byte) to store it.
    But since @link(TBFNTCharInfo.Alignment Alignment) = 2,
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
    (@link(TBFNTCharInfo.Height Height) = 2)
    so we need 24 bytes to store this character.

    The precise formula to calculate how many bytes are used to store
    a character is

    @unorderedList(
      @item( @code(RoundUpToMultiple( (Char.Info.Width + 7) div 8,
        Char.Info.Alignment ) * Char.Info.Height) )
      @item( Or, more readable, @code(
        BFNTCharRowByteLength(Char.Info.Width, Char.Info.Alignment)
          * Char.Info.Height ) )
      @item( Or, even more readable, just @code(
        BFNTCharRowByteLength(Char) * Char.Info.Height) )
    )

    Rows are specified in @link(Data) from lower to upper
    (just like OpenGL's @code(glBitmap) likes).

    Note that this is @italic(packed) record so I'm sure that

    @longcode(#
      SizeOf(record
        Info: TBFNTCharInfo;
        Data: packed array[0..n]of byte;
      ) = SizeOf(TBFNTCharInfo) + (n+1) * SizeOf(Byte)
    #)

    I.e. there's no padding between Info and Data. }
  TBFNTChar = packed record
    Info: TBFNTCharInfo;
    Data: packed array[0 .. MaxInt - 1 - SizeOf(TBFNTCharInfo)]of byte;
  end;
  PBFNTChar = ^TBFNTChar;

  TBmpFont = array[char]of PBFNTChar;
  PBmpFont = ^TBmpFont;

function BFNTCharRowByteLength(Width, Alignment: Cardinal): Cardinal; overload;
function BFNTCharRowByteLength(BFNTChar: PBFNTChar): Cardinal; overload;

implementation

uses CastleUtils;

function BFNTCharRowByteLength(Width, Alignment: Cardinal): Cardinal;
begin
 Result := RoundUpToMultiply((Width + 7) div 8, Alignment);
end;

function BFNTCharRowByteLength(BFNTChar: PBFNTChar): Cardinal;
begin
 Result := BFNTCharRowByteLength(BFNTChar^.Info.Width, BFNTChar^.Info.Alignment);
end;

end.
