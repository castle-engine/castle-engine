{
  Copyright 2015 Sven Barth.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TStream utilities and helpers }
unit CastleStreamUtils;

{$I castleconf.inc}

interface

uses
  Classes, CastleVectors;

type
  { Helper class for streams that allows to correct read and write either little
    or big endian values. }
  TStreamHelper = class helper for TStream
  public
    { Reads a little endian value from the stream and converts it to native
      Byte order.

      @groupBegin }
    function ReadLE(out Value: Word): Boolean; overload;
    function ReadLE(out Value: LongWord): Boolean; overload;
    function ReadLE(out Value: QWord): Boolean; overload;
    function ReadLE(out Value: SmallInt): Boolean; overload;
    function ReadLE(out Value: LongInt): Boolean; overload;
    function ReadLE(out Value: Int64): Boolean; overload;
    function ReadLE(out Value: Single): Boolean; overload;
    function ReadLE(out Value: Double): Boolean; overload;
    function ReadLE(out Value: TVector2Single): Boolean; overload;
    function ReadLE(out Value: TVector2Double): Boolean; overload;
    function ReadLE(out Value: TVector3Single): Boolean; overload;
    function ReadLE(out Value: TVector3Double): Boolean; overload;
    function ReadLE(out Value: TVector4Single): Boolean; overload;
    function ReadLE(out Value: TVector4Double): Boolean; overload;
    { @groupEnd }

    { Reads a big endian value from the stream and converts it to native
      Byte order.

      @groupBegin }
    function ReadBE(out Value: Word): Boolean; overload;
    function ReadBE(out Value: LongWord): Boolean; overload;
    function ReadBE(out Value: QWord): Boolean; overload;
    function ReadBE(out Value: SmallInt): Boolean; overload;
    function ReadBE(out Value: LongInt): Boolean; overload;
    function ReadBE(out Value: Int64): Boolean; overload;
    function ReadBE(out Value: Single): Boolean; overload;
    function ReadBE(out Value: Double): Boolean; overload;
    { @groupEnd }

    { Writes a value in native Byte order as little endian value to the stream.

      @groupBegin }
    function WriteLE(const Value: Word): Boolean; overload;
    function WriteLE(const Value: LongWord): Boolean; overload;
    function WriteLE(const Value: QWord): Boolean; overload;
    function WriteLE(const Value: SmallInt): Boolean; overload;
    function WriteLE(const Value: LongInt): Boolean; overload;
    function WriteLE(const Value: Int64): Boolean; overload;
    function WriteLE(const Value: Single): Boolean; overload;
    function WriteLE(const Value: Double): Boolean; overload;
    { @groupEnd }

    { Writes a value in native Byte order as big endian value to the stream.

      @groupBegin }
    function WriteBE(const Value: Word): Boolean; overload;
    function WriteBE(const Value: LongWord): Boolean; overload;
    function WriteBE(const Value: QWord): Boolean; overload;
    function WriteBE(const Value: SmallInt): Boolean; overload;
    function WriteBE(const Value: LongInt): Boolean; overload;
    function WriteBE(const Value: Int64): Boolean; overload;
    function WriteBE(const Value: Single): Boolean; overload;
    function WriteBE(const Value: Double): Boolean; overload;
    { @groupEnd }
  end;

implementation

uses
  CastleUtils;

{ TStreamHelper -------------------------------------------------------------- }

function TStreamHelper.ReadLE(out Value: Word): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: LongWord): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: QWord): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: SmallInt): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: LongInt): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: Int64): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: Single): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: Double): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: TVector2Single): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: TVector2Double): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: TVector3Single): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: TVector3Double): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: TVector4Single): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadLE(out Value: TVector4Double): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := LEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: Word): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: LongWord): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: QWord): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: SmallInt): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: LongInt): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: Int64): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: Single): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.ReadBE(out Value: Double): Boolean;
begin
  if Read(Value, SizeOf(Value)) <> SizeOf(Value) then
    Exit(False);
  Value := BEtoN(Value);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: Word): Boolean;
var
  tmp: Word;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: LongWord): Boolean;
var
  tmp: LongWord;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: QWord): Boolean;
var
  tmp: QWord;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: SmallInt): Boolean;
var
  tmp: SmallInt;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: LongInt): Boolean;
var
  tmp: LongInt;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: Int64): Boolean;
var
  tmp: Int64;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(Value) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: Single): Boolean;
var
  tmp: Single;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteLE(const Value: Double): Boolean;
var
  tmp: Double;
begin
  tmp := NToLE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: Word): Boolean;
var
  tmp: Word;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: LongWord): Boolean;
var
  tmp: LongWord;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: QWord): Boolean;
var
  tmp: QWord;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: SmallInt): Boolean;
var
  tmp: SmallInt;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: LongInt): Boolean;
var
  tmp: LongInt;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: Int64): Boolean;
var
  tmp: Int64;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: Single): Boolean;
var
  tmp: Single;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

function TStreamHelper.WriteBE(const Value: Double): Boolean;
var
  tmp: Double;
begin
  tmp := NToBE(Value);
  if Write(tmp, SizeOf(tmp)) <> SizeOf(tmp) then
    Exit(False);
  Result := True;
end;

end.

