{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains the generics collections library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Generics.Helpers;

{$MODE DELPHI}{$H+}
{$MODESWITCH TYPEHELPERS}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  Classes, SysUtils;

type
  { TValueAnsiStringHelper }

  TValueAnsiStringHelper = record helper for AnsiString
    function ToLower: AnsiString; inline;
  end;

  { TValuewideStringHelper }

  TValueWideStringHelper = record helper for WideString
    function ToLower: WideString; inline;
  end;

  { TValueUnicodeStringHelper }

  TValueUnicodeStringHelper = record helper for UnicodeString
    function ToLower: UnicodeString; inline;
  end;

  { TValueShortStringHelper }

  TValueShortStringHelper = record helper for ShortString
    function ToLower: ShortString; inline;
  end;

  { TValueUTF8StringHelper }

  TValueUTF8StringHelper = record helper for UTF8String
    function ToLower: UTF8String; inline;
  end;

  { TValueRawByteStringHelper }

  TValueRawByteStringHelper = record helper for RawByteString
    function ToLower: RawByteString; inline;
  end;

  { TValueUInt32Helper }

  TValueUInt32Helper = record helper for UInt32
    class function GetSignMask: UInt32; static; inline;
    class function GetSizedSignMask(ABits: Byte): UInt32; static; inline;
    class function GetBitsLength: Byte; static; inline;

    const
      SIZED_SIGN_MASK: array[1..32] of UInt32 = (
        $80000000, $C0000000, $E0000000, $F0000000, $F8000000, $FC000000, $FE000000, $FF000000,
        $FF800000, $FFC00000, $FFE00000, $FFF00000, $FFF80000, $FFFC0000, $FFFE0000, $FFFF0000,
        $FFFF8000, $FFFFC000, $FFFFE000, $FFFFF000, $FFFFF800, $FFFFFC00, $FFFFFE00, $FFFFFF00,
        $FFFFFF80, $FFFFFFC0, $FFFFFFE0, $FFFFFFF0, $FFFFFFF8, $FFFFFFFC, $FFFFFFFE, $FFFFFFFF);
      BITS_LENGTH = 32;
  end;

implementation

{ TRawDataStringHelper }

function TValueAnsiStringHelper.ToLower: AnsiString;
begin
  Result := LowerCase(Self);
end;

{ TValueWideStringHelper }

function TValueWideStringHelper.ToLower: WideString;
begin
  Result := LowerCase(Self);
end;

{ TValueUnicodeStringHelper }

function TValueUnicodeStringHelper.ToLower: UnicodeString;
begin
  Result := LowerCase(Self);
end;

{ TValueShortStringHelper }

function TValueShortStringHelper.ToLower: ShortString;
begin
  Result := LowerCase(Self);
end;

{ TValueUTF8StringHelper }

function TValueUTF8StringHelper.ToLower: UTF8String;
begin
  Result := LowerCase(Self);
end;

{ TValueRawByteStringHelper }

function TValueRawByteStringHelper.ToLower: RawByteString;
begin
  Result := LowerCase(Self);
end;

{ TValueUInt32Helper }

class function TValueUInt32Helper.GetSignMask: UInt32;
begin
  Result := $80000000;
end;

class function TValueUInt32Helper.GetSizedSignMask(ABits: Byte): UInt32;
begin
  Result := SIZED_SIGN_MASK[ABits];
end;

class function TValueUInt32Helper.GetBitsLength: Byte;
begin
  Result := BITS_LENGTH;
end;

end.

