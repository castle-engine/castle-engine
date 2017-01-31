{
  Copyright 2015-2017 Sven Barth.

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
    procedure ReadLE(out Value: Word); overload;
    procedure ReadLE(out Value: LongWord); overload;
    procedure ReadLE(out Value: QWord); overload;
    procedure ReadLE(out Value: SmallInt); overload;
    procedure ReadLE(out Value: LongInt); overload;
    procedure ReadLE(out Value: Int64); overload;
    procedure ReadLE(out Value: Single); overload;
    procedure ReadLE(out Value: Double); overload;
    procedure ReadLE(out Value: TVector2Single); overload;
    procedure ReadLE(out Value: TVector2Double); overload;
    procedure ReadLE(out Value: TVector3Single); overload;
    procedure ReadLE(out Value: TVector3Double); overload;
    procedure ReadLE(out Value: TVector4Single); overload;
    procedure ReadLE(out Value: TVector4Double); overload;
    { @groupEnd }

    { Reads a big endian value from the stream and converts it to native
      Byte order.

      @groupBegin }
    procedure ReadBE(out Value: Word); overload;
    procedure ReadBE(out Value: LongWord); overload;
    procedure ReadBE(out Value: QWord); overload;
    procedure ReadBE(out Value: SmallInt); overload;
    procedure ReadBE(out Value: LongInt); overload;
    procedure ReadBE(out Value: Int64); overload;
    procedure ReadBE(out Value: Single); overload;
    procedure ReadBE(out Value: Double); overload;
    { @groupEnd }

    { Writes a value in native Byte order as little endian value to the stream.

      @groupBegin }
    procedure WriteLE(const Value: Word); overload;
    procedure WriteLE(const Value: LongWord); overload;
    procedure WriteLE(const Value: QWord); overload;
    procedure WriteLE(const Value: SmallInt); overload;
    procedure WriteLE(const Value: LongInt); overload;
    procedure WriteLE(const Value: Int64); overload;
    procedure WriteLE(const Value: Single); overload;
    procedure WriteLE(const Value: Double); overload;
    { @groupEnd }

    { Writes a value in native Byte order as big endian value to the stream.

      @groupBegin }
    procedure WriteBE(const Value: Word); overload;
    procedure WriteBE(const Value: LongWord); overload;
    procedure WriteBE(const Value: QWord); overload;
    procedure WriteBE(const Value: SmallInt); overload;
    procedure WriteBE(const Value: LongInt); overload;
    procedure WriteBE(const Value: Int64); overload;
    procedure WriteBE(const Value: Single); overload;
    procedure WriteBE(const Value: Double); overload;
    { @groupEnd }
  end;

implementation

uses
  CastleUtils;

{ TStreamHelper -------------------------------------------------------------- }

procedure TStreamHelper.ReadLE(out Value: Word);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: LongWord);
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

procedure TStreamHelper.ReadLE(out Value: LongInt);
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

procedure TStreamHelper.ReadLE(out Value: TVector2Single);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector2Double);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector3Single);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector3Double);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector4Single);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadLE(out Value: TVector4Double);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := LEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: Word);
begin
  ReadBuffer(Value, SizeOf(Value));
  Value := BEtoN(Value);
end;

procedure TStreamHelper.ReadBE(out Value: LongWord);
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

procedure TStreamHelper.ReadBE(out Value: LongInt);
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

procedure TStreamHelper.WriteLE(const Value: LongWord);
var
  tmp: LongWord;
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

procedure TStreamHelper.WriteLE(const Value: LongInt);
var
  tmp: LongInt;
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

procedure TStreamHelper.WriteBE(const Value: LongWord);
var
  tmp: LongWord;
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

procedure TStreamHelper.WriteBE(const Value: LongInt);
var
  tmp: LongInt;
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

