{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  TRGBBlock = packed record
    Color0, Color1: Word;
    Row0, Row1, Row2, Row3: Byte;
  end;
  PRGBBlock = ^TRGBBlock;

  TS3TCFlipBlockProcedure = procedure (var Block: PtrUInt);

procedure DXT1_Flip4Block(var Block: PtrUInt);
var
  Tmp: Byte;
begin
  Tmp := PRGBBlock(Block)^.Row0;
  PRGBBlock(Block)^.Row0 := PRGBBlock(Block)^.Row3;
  PRGBBlock(Block)^.Row3 := Tmp;

  Tmp := PRGBBlock(Block)^.Row1;
  PRGBBlock(Block)^.Row1 := PRGBBlock(Block)^.Row2;
  PRGBBlock(Block)^.Row2 := Tmp;

  Block += SizeOf(TRGBBlock);
end;

procedure DXT1_Flip3Block(var Block: PtrUInt);
var
  Tmp: Byte;
begin
  Tmp := PRGBBlock(Block)^.Row0;
  PRGBBlock(Block)^.Row0 := PRGBBlock(Block)^.Row2;
  PRGBBlock(Block)^.Row2 := Tmp;

  Block += SizeOf(TRGBBlock);
end;

procedure DXT1_Flip2Block(var Block: PtrUInt);
var
  Tmp: Byte;
begin
  Tmp := PRGBBlock(Block)^.Row0;
  PRGBBlock(Block)^.Row0 := PRGBBlock(Block)^.Row1;
  PRGBBlock(Block)^.Row1 := Tmp;

  Block += SizeOf(TRGBBlock);
end;

type
  TAlphaDXT23Block = packed record
    Row0, Row1, Row2, Row3: Word;
  end;
  PAlphaDXT23Block = ^TAlphaDXT23Block;

procedure DXT23_Flip4Block(var Block: PtrUInt);
var
  Tmp: Word;
begin
  Tmp := PAlphaDXT23Block(Block)^.Row0;
  PAlphaDXT23Block(Block)^.Row0 := PAlphaDXT23Block(Block)^.Row3;
  PAlphaDXT23Block(Block)^.Row3 := Tmp;

  Tmp := PAlphaDXT23Block(Block)^.Row1;
  PAlphaDXT23Block(Block)^.Row1 := PAlphaDXT23Block(Block)^.Row2;
  PAlphaDXT23Block(Block)^.Row2 := Tmp;

  Block += SizeOf(TAlphaDXT23Block);
  DXT1_Flip4Block(Block);
end;

procedure DXT23_Flip3Block(var Block: PtrUInt);
var
  Tmp: Word;
begin
  Tmp := PAlphaDXT23Block(Block)^.Row0;
  PAlphaDXT23Block(Block)^.Row0 := PAlphaDXT23Block(Block)^.Row2;
  PAlphaDXT23Block(Block)^.Row2 := Tmp;

  Block += SizeOf(TAlphaDXT23Block);
  DXT1_Flip4Block(Block);
end;

procedure DXT23_Flip2Block(var Block: PtrUInt);
var
  Tmp: Word;
begin
  Tmp := PAlphaDXT23Block(Block)^.Row0;
  PAlphaDXT23Block(Block)^.Row0 := PAlphaDXT23Block(Block)^.Row1;
  PAlphaDXT23Block(Block)^.Row1 := Tmp;

  Block += SizeOf(TAlphaDXT23Block);
  DXT1_Flip4Block(Block);
end;

type
  TAlphaDXT45Block = packed record
    Alpha0, Alpha1: Byte;
    { 3 bits per pixel -> we cannot divide this easily into separate bytes }
    EncRow5, EncRow4, EncRow3, EncRow2, EncRow1, EncRow0: Byte;
  end;
  PAlphaDXT45Block = ^TAlphaDXT45Block;

const
  { 12 bits (4 * 3) for each row. }
  RowMask0: QWord =          $FFF;
  RowMask1: QWord =       $FFF000;
  RowMask2: QWord =    $FFF000000;
  RowMask3: QWord = $FFF000000000;

function GetRows(const Block: TAlphaDXT45Block): QWord;
begin
  { Do it by combining bytes, this way it works on both endianesses. }
  with Block do
    Result := ( (QWord(EncRow0) shl 40) or
                (QWord(EncRow1) shl 32) or
                (QWord(EncRow2) shl 24) or
                (QWord(EncRow3) shl 16) or
                (QWord(EncRow4) shl 8) or
                 QWord(EncRow5) );
end;

procedure SetRows(var Block: TAlphaDXT45Block; const Rows: QWord);
begin
  with Block do
  begin
    EncRow0 := (Rows shr 40) { and $FF - no need };
    EncRow1 := (Rows shr 32) and $FF;
    EncRow2 := (Rows shr 24) and $FF;
    EncRow3 := (Rows shr 16) and $FF;
    EncRow4 := (Rows shr 8 ) and $FF;
    EncRow5 :=  Rows         and $FF;
  end;
end;

procedure DXT45_Flip4Block(var Block: PtrUInt);
var
  Rows, Tmp: QWord;
begin
  Rows := GetRows(PAlphaDXT45Block(Block)^);

  Tmp := (Rows and RowMask0) shl (12*3);
  Rows := (Rows and (not RowMask0)) or (Rows shr (12*3));
  Rows := (Rows and (not RowMask3)) or Tmp;

  Tmp := (Rows and RowMask1) shl 12;
  Rows := (Rows and (not RowMask1)) or ((Rows and RowMask2) shr 12);
  Rows := (Rows and (not RowMask2)) or Tmp;

  SetRows(PAlphaDXT45Block(Block)^, Rows);

  Block += SizeOf(TAlphaDXT45Block);
  DXT1_Flip4Block(Block);
end;

procedure DXT45_Flip3Block(var Block: PtrUInt);
var
  Rows, Tmp: QWord;
begin
  Rows := GetRows(PAlphaDXT45Block(Block)^);

  Tmp := (Rows and RowMask0) shl (12*2);
  Rows := (Rows and (not RowMask0)) or ((Rows and RowMask2) shr (12*2));
  Rows := (Rows and (not RowMask2)) or Tmp;

  SetRows(PAlphaDXT45Block(Block)^, Rows);

  Block += SizeOf(TAlphaDXT45Block);
  DXT1_Flip4Block(Block);
end;

procedure DXT45_Flip2Block(var Block: PtrUInt);
var
  Rows, Tmp: QWord;
begin
  Rows := GetRows(PAlphaDXT45Block(Block)^);

  Tmp := (Rows and RowMask0) shl 12;
  Rows := (Rows and (not RowMask0)) or ((Rows and RowMask1) shr 12);
  Rows := (Rows and (not RowMask1)) or Tmp;

  SetRows(PAlphaDXT45Block(Block)^, Rows);

  Block += SizeOf(TAlphaDXT45Block);
  DXT1_Flip4Block(Block);
end;

procedure TGPUCompressedImage.FlipVertical;
var
  WidthInBlocks: PtrUInt;

  procedure FlipRow(Row: PtrUInt; FlipBlock: TS3TCFlipBlockProcedure);
  var
    X: PtrUInt;
  begin
    for X := 1 to WidthInBlocks do
      FlipBlock(Row);
      { FlipBlock will advance Row pointer by itself }
  end;

var
  TempRow: Pointer;
  RowSize, BlockSize, LowerRow, UpperRow: PtrUInt;
  FlipBlock: array [2..4] of TS3TCFlipBlockProcedure;
begin
  case Compression of
    tcDxt1_RGB, tcDxt1_RGBA:
      begin
        BlockSize := 8;
        FlipBlock[2] := @DXT1_Flip2Block;
        FlipBlock[3] := @DXT1_Flip3Block;
        FlipBlock[4] := @DXT1_Flip4Block;
      end;
    tcDxt3:
      begin
        BlockSize := 16;
        FlipBlock[2] := @DXT23_Flip2Block;
        FlipBlock[3] := @DXT23_Flip3Block;
        FlipBlock[4] := @DXT23_Flip4Block;
      end;
    tcDxt5:
      begin
        BlockSize := 16;
        FlipBlock[2] := @DXT45_Flip2Block;
        FlipBlock[3] := @DXT45_Flip3Block;
        FlipBlock[4] := @DXT45_Flip4Block;
      end;
    { Don't even make a warning about it, just silently exit.
      Image author must be aware that we cannot flip when creating the image. }
    // else raise ECannotFlipCompressedImage.CreateFmt('Cannot flip image compressed with GPU compression %s',
    //   [TextureCompressionInfo[Compression].Name]);
    else Exit;
  end;

  WidthInBlocks := DivRoundUp(Width, 4);

  if Height mod 4 = 0 then
  begin
    RowSize := WidthInBlocks * BlockSize;

    TempRow := GetMem(RowSize);

    LowerRow := PtrUInt(RawPixels);
    UpperRow := PtrUInt(RawPixels) + RowSize * (Height div 4 - 1);

    while LowerRow < UpperRow do
    begin
      FlipRow(LowerRow, FlipBlock[4]);

      Move(Pointer(UpperRow)^, TempRow^, RowSize);
      Move(Pointer(LowerRow)^, Pointer(UpperRow)^, RowSize);
      Move(TempRow^, Pointer(LowerRow)^, RowSize);

      FlipRow(LowerRow, FlipBlock[4]);
      LowerRow += RowSize;
      UpperRow -= RowSize;
    end;

    FreeMem(TempRow);
  end else
  if Height = 3 then
  begin
    FlipRow(PtrUInt(RawPixels), FlipBlock[3]);
  end else
  if Height = 2 then
  begin
    FlipRow(PtrUInt(RawPixels), FlipBlock[2]);
  end else
  if Height = 1 then
  begin
    { Nothing to do }
  end else
  begin
    WritelnWarning('S3TC', 'Cannot flip image compressed with S3TC when image height is not a multiple of 4 (or 1, 2, 3). Image height is %d',
      [Height]);
    Exit;
  end;
end;
