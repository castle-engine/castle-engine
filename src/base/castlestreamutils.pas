{
  Copyright 2015-2018 Sven Barth.

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
  Classes, CastleVectors, CastleUtils;

type
  { Helper class for streams that allows to correctly read and write either little
    or big endian values. }
  TStreamHelper = class helper for TStream
  public
    { Reads a little endian value from the stream and converts it to native
      Byte order.

      @groupBegin }
    procedure ReadLE(out Value: Word); overload;
    procedure ReadLE(out Value: UInt32); overload;
    procedure ReadLE(out Value: QWord); overload;
    procedure ReadLE(out Value: SmallInt); overload;
    procedure ReadLE(out Value: Int32); overload;
    procedure ReadLE(out Value: Int64); overload;
    procedure ReadLE(out Value: Single); overload;
    procedure ReadLE(out Value: Double); overload;
    procedure ReadLE(out Value: TVector2); overload;
    procedure ReadLE(out Value: TVector3); overload;
    procedure ReadLE(out Value: TVector4); overload;
    { @groupEnd }

    { Reads a big endian value from the stream and converts it to native
      Byte order.

      @groupBegin }
    procedure ReadBE(out Value: Word); overload;
    procedure ReadBE(out Value: UInt32); overload;
    procedure ReadBE(out Value: QWord); overload;
    procedure ReadBE(out Value: SmallInt); overload;
    procedure ReadBE(out Value: Int32); overload;
    procedure ReadBE(out Value: Int64); overload;
    procedure ReadBE(out Value: Single); overload;
    procedure ReadBE(out Value: Double); overload;
    { @groupEnd }

    { Writes a value in native Byte order as little endian value to the stream.

      @groupBegin }
    procedure WriteLE(const Value: Word); overload;
    procedure WriteLE(const Value: UInt32); overload;
    procedure WriteLE(const Value: QWord); overload;
    procedure WriteLE(const Value: SmallInt); overload;
    procedure WriteLE(const Value: Int32); overload;
    procedure WriteLE(const Value: Int64); overload;
    procedure WriteLE(const Value: Single); overload;
    procedure WriteLE(const Value: Double); overload;
    { @groupEnd }

    { Writes a value in native Byte order as big endian value to the stream.

      @groupBegin }
    procedure WriteBE(const Value: Word); overload;
    procedure WriteBE(const Value: UInt32); overload;
    procedure WriteBE(const Value: QWord); overload;
    procedure WriteBE(const Value: SmallInt); overload;
    procedure WriteBE(const Value: Int32); overload;
    procedure WriteBE(const Value: Int64); overload;
    procedure WriteBE(const Value: Single); overload;
    procedure WriteBE(const Value: Double); overload;
    { @groupEnd }
  end;

implementation

{ TStreamHelper -------------------------------------------------------------- }

procedure TStreamHelper.ReadLE(out Value: Word);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: UInt32);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: QWord);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: SmallInt);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: Int32);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: Int64);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: Single);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: Double);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector2);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector3);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector4);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: Word);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: UInt32);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: QWord);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: SmallInt);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: Int32);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: Int64);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: Single);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: Double);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.WriteLE(const Value: Word);
var
  tmp: Word;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: UInt32);
var
  tmp: UInt32;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: QWord);
var
  tmp: QWord;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: SmallInt);
var
  tmp: SmallInt;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: Int32);
var
  tmp: Int32;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: Int64);
var
  tmp: Int64;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: Single);
var
  tmp: Single;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteLE(const Value: Double);
var
  tmp: Double;
begin
  tmp := NToLE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: Word);
var
  tmp: Word;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: UInt32);
var
  tmp: UInt32;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: QWord);
var
  tmp: QWord;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: SmallInt);
var
  tmp: SmallInt;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: Int32);
var
  tmp: Int32;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: Int64);
var
  tmp: Int64;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: Single);
var
  tmp: Single;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

procedure TStreamHelper.WriteBE(const Value: Double);
var
  tmp: Double;
begin
  tmp := NToBE(Value);
  WriteBuffer(tmp, SizeOf(tmp));
end;

end.
