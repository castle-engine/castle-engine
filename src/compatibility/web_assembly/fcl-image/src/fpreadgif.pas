{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    GIF reader for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  ToDo: read further images
}
unit FPReadGif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage;

type
  TGifRGB = packed record
    Red, Green, Blue : Byte;
  end;

  TGIFHeader = packed record
    Signature:array[0..2] of Char;    //* Header Signature (always "GIF") */
    Version:array[0..2] of Char;      //* GIF format version("87a" or "89a") */
    // Logical Screen Descriptor
    ScreenWidth:word;                 //* Width of Display Screen in Pixels */
    ScreenHeight:word;                //* Height of Display Screen in Pixels */
    Packedbit,                        //* Screen and Color Map Information */
    BackgroundColor,                  //* Background Color Index */
    AspectRatio:byte;                 //* Pixel Aspect Ratio */
  end;

  TGifImageDescriptor = packed record
    Left,              //* X position of image on the display */
    Top,               //* Y position of image on the display */
    Width,             //* Width of the image in pixels */
    Height:word;       //* Height of the image in pixels */
    Packedbit:byte;    //* Image and Color Table Data Information */
  end;

  TGifGraphicsControlExtension = packed record
    BlockSize,         //* Size of remaining fields (always 04h) */
    Packedbit:byte;    //* Method of graphics disposal to use */
    DelayTime:word;    //* Hundredths of seconds to wait	*/
    ColorIndex,        //* Transparent Color Index */
    Terminator:byte;   //* Block Terminator (always 0) */
  end;

  TFPReaderGif = class;

  TGifCreateCompatibleImgEvent = procedure(Sender: TFPReaderGif;
                                        var NewImage: TFPCustomImage) of object;

  { TFPReaderGif }

  TFPReaderGif = class(TFPCustomImageReader)
  protected
    FHeader: TGIFHeader;
    FDescriptor: TGifImageDescriptor;
    FGraphicsCtrlExt: TGifGraphicsControlExtension;
    FTransparent: Boolean;
    FGraphCtrlExt: Boolean;
    FScanLine: PByte;
    FLineSize: Integer;
    FPalette: TFPPalette;
    FWidth: integer;
    FHeight: Integer;
    FInterlace: boolean;
    FBitsPerPixel: byte;
    FBackground: byte;
    FResolution: byte;
    FOnCreateImage: TGifCreateCompatibleImgEvent;
    procedure ReadPalette(Stream: TStream; Size: integer);
    function AnalyzeHeader: Boolean;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function ReadScanLine(Stream: TStream): boolean; virtual;
    function WriteScanLine(Img: TFPCustomImage): Boolean; virtual;
    function InternalCheck (Stream: TStream) : boolean; override;
    function SkipBlock(Stream: TStream): byte;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Header: TGIFHeader read FHeader;
    property Descriptor: TGifImageDescriptor read FDescriptor;
    property GraphCtrlExt: Boolean read FGraphCtrlExt;
    property GraphicsCtrlExt: TGifGraphicsControlExtension read FGraphicsCtrlExt;
    property Transparent: Boolean read FTransparent;
    property Palette: TFPPalette read FPalette;
    property Width: integer read FWidth;
    property Height: Integer read FHeight;
    property Interlace: boolean read FInterlace;
    property BitsPerPixel: byte read FBitsPerPixel;
    property Background: byte read FBackground;
    property Resolution: byte read FResolution;
    property OnCreateImage: TGifCreateCompatibleImgEvent read FOnCreateImage write FOnCreateImage;
  end;

implementation

{ TFPReaderGif }

procedure TFPReaderGif.ReadPalette(Stream: TStream; Size: integer);
Var
  RGBEntry : TGifRGB;
  I : Integer;
  c : TFPColor;
begin
  FPalette.count := 0;
  For I:=0 To Size-1 Do
  Begin
    Stream.Read(RGBEntry, SizeOf(RGBEntry));
    With c do
    begin
      Red:=RGBEntry.Red or (RGBEntry.Red shl 8);
      Green:=RGBEntry.Green or (RGBEntry.Green shl 8);
      Blue:=RGBEntry.Blue or (RGBEntry.Blue shl 8);
      Alpha:=alphaOpaque;
    end;
    FPalette.Add(C);
  End;
end;

function TFPReaderGif.AnalyzeHeader: Boolean;
var
    C : TFPColor;
begin
  Result:=false;
  With FHeader do
  begin
    if (Signature = 'GIF') and
       ((Version = '87a') or
       (Version = '89a')) then
    else
    Raise Exception.Create('Unknown/Unsupported GIF image type');

    FResolution := Packedbit and $70 shr 5 + 1;
    FBitsPerPixel:=Packedbit and 7 + 1;
    FBackground := BackgroundColor;

    With FDescriptor do
    begin
      fWidth:=Width;
      fHeight:=Height;
      FInterlace := (Packedbit and $40 = $40);
    end;
    FTransparent:= FBackground <> 0;
    if FGraphCtrlExt then
    begin
      FTransparent:=(FGraphicsCtrlExt.Packedbit and $01)<>0;
      If FTransparent then
        FBackground:=FGraphicsCtrlExt.ColorIndex;
    end;
    FLineSize:=FWidth*(FHeight+1);
    GetMem(FScanLine,FLineSize);
    If FTransparent then
    begin
      C:=FPalette.Color[FBackground];
      C.alpha:=alphaTransparent;
      FPalette.Color[FBackground]:=C;
    end;
  end;
  Result:=true;
end;

procedure TFPReaderGif.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Introducer:byte;
  ColorTableSize :Integer;
  ContProgress: Boolean;
begin
  FPalette:=nil;
  FScanLine:=nil;
  try
    ContProgress:=true;
    Progress(psStarting, 0, False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;

    FPalette := TFPPalette.Create(0);

    Stream.Position:=0;
    // header
    Stream.Read(FHeader,SizeOf(FHeader));
    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)), False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit;
     
    // Endian Fix Mantis 8541. Gif is always little endian
    {$IFDEF ENDIAN_BIG}    
      with FHeader do 
        begin
          ScreenWidth := LEtoN(ScreenWidth);
          ScreenHeight := LEtoN(ScreenHeight);
        end; 
    {$ENDIF}
    // global palette
    if (FHeader.Packedbit and $80) <> 0 then
    begin
      ColorTableSize := FHeader.Packedbit and 7 + 1;
      ReadPalette(stream, 1 shl ColorTableSize);
    end;

    // skip extensions
    Repeat
      Introducer:=SkipBlock(Stream);
    until (Introducer = $2C) or (Introducer = $3B) or (Stream.Position>=Stream.Size);
    
    if Stream.Position>=Stream.Size then 
      Exit;

    // descriptor
    Stream.Read(FDescriptor, SizeOf(FDescriptor));
    {$IFDEF ENDIAN_BIG}
      with FDescriptor do 
        begin
          Left := LEtoN(Left);
          Top := LEtoN(Top);
          Width := LEtoN(Width);
          Height := LEtoN(Height);
        end;
    {$ENDIF}
    // local palette
    if (FDescriptor.Packedbit and $80) <> 0 then
    begin
      ColorTableSize := FDescriptor.Packedbit and 7 + 1;
      ReadPalette(stream, 1 shl ColorTableSize);
    end;

    // parse header
    if not AnalyzeHeader then exit;

    // create image
    if Assigned(OnCreateImage) then
      OnCreateImage(Self,Img);
    Img.SetSize(FWidth,FHeight);

    // read pixels
    if not ReadScanLine(Stream) then exit;
    if not WriteScanLine(Img) then exit;

    // ToDo: read further images
  finally
    FreeAndNil(FPalette);
    ReAllocMem(FScanLine,0);
  end;
  Progress(FPimage.psEnding, 100, false, Rect(0,0,FWidth,FHeight), '', ContProgress);
end;

function TFPReaderGif.ReadScanLine(Stream: TStream): Boolean;
var
  OldPos,
  UnpackedSize,
  PackedSize:longint;
  I: Integer;
  Data,
  Bits,
  Code: Cardinal;
  SourcePtr: PByte;
  InCode: Cardinal;

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal;
  Suffix,
  Stack: array [0..4095] of Byte;
  StackPointer: PByte;
  DataComp,
  Target: PByte;
  B,
  FInitialCodeSize,
  FirstChar: Byte;
  ClearCode,
  EOICode: Word;
  ContProgress: Boolean;

begin
  DataComp:=nil;
  ContProgress:=true;
  try
    // read dictionary size
    Stream.read(FInitialCodeSize, 1);

    // search end of compressor table
    OldPos:=Stream.Position;
    PackedSize := 0;
    Repeat
      Stream.read(B, 1);
      if B > 0 then
      begin
        inc(PackedSize, B);
        Stream.Seek(B, soFromCurrent);
        CodeMask := (1 shl CodeSize) - 1;
      end;
    until (B = 0)  or (Stream.Position>=Stream.Size);
    
   { if Stream.Position>=Stream.Size then 
      Exit(False); }

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    Getmem(DataComp, PackedSize);
    // read compressor table
    SourcePtr:=DataComp;
    Stream.Position:=OldPos;
    Repeat
      Stream.read(B, 1);
      if B > 0 then
      begin
         Stream.ReadBuffer(SourcePtr^, B);
         Inc(SourcePtr,B);
      end;
    until (B = 0) or (Stream.Position>=Stream.Size);
    
   { if Stream.Position>=Stream.Size then
       Exit(False); }
              

    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);

    SourcePtr:=DataComp;
    Target := FScanLine;
    CodeSize := FInitialCodeSize + 1;
    ClearCode := 1 shl FInitialCodeSize;
    EOICode := ClearCode + 1;
    FreeCode := ClearCode + 2;
    OldCode := 4096;
    CodeMask := (1 shl CodeSize) - 1;
    UnpackedSize:=FWidth * FHeight;
    for I := 0 to ClearCode - 1 do
    begin
      Prefix[I] := 4096;
      Suffix[I] := I;
    end;
    StackPointer := @Stack;
    FirstChar := 0;
    Data := 0;
    Bits := 0;
    // LZW decompression gif
    while (UnpackedSize > 0) and (PackedSize > 0) do
    begin
      Inc(Data, SourcePtr^ shl Bits);
      Inc(Bits, 8);
      while Bits >= CodeSize do
      begin
        Code := Data and CodeMask;
        Data := Data shr CodeSize;
        Dec(Bits, CodeSize);
        if Code = EOICode then Break;
        if Code = ClearCode then
        begin
          CodeSize := FInitialCodeSize + 1;
          CodeMask := (1 shl CodeSize) - 1;
          FreeCode := ClearCode + 2;
          OldCode := 4096;
          Continue;
        end;
        if Code > FreeCode then Break;
        if OldCode = 4096 then
        begin
          FirstChar := Suffix[Code];
          Target^ := FirstChar;
          Inc(Target);
          Dec(UnpackedSize);
          OldCode := Code;
          Continue;
        end;
        InCode := Code;
        if Code = FreeCode then
        begin
          StackPointer^ := FirstChar;
          Inc(StackPointer);
          Code := OldCode;
        end;
        while Code > ClearCode do
        begin
          StackPointer^ := Suffix[Code];
          Inc(StackPointer);
          Code := Prefix[Code];
        end;
        FirstChar := Suffix[Code];
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Prefix[FreeCode] := OldCode;
        Suffix[FreeCode] := FirstChar;
        if (FreeCode = CodeMask) and
           (CodeSize < 12) then
        begin
          Inc(CodeSize);
          CodeMask := (1 shl CodeSize) - 1;
        end;
        if FreeCode < 4095 then Inc(FreeCode);
        OldCode := InCode;
        repeat
          Dec(StackPointer);
          Target^ := StackPointer^;
          Inc(Target);
          Dec(UnpackedSize);
        until StackPointer = @Stack;
      end;
      Inc(SourcePtr);
      Dec(PackedSize);
    end;
    Progress(psRunning, trunc(100.0 * (Stream.position / Stream.size)),
             False, Rect(0,0,0,0), '', ContProgress);
    if not ContProgress then exit(false);
  finally
    if DataComp<>nil then
      FreeMem(DataComp);
  end;
  Result:=true;
end;

function TFPReaderGif.WriteScanLine(Img: TFPCustomImage): Boolean;
Var
  Row, Col : Integer;
  Pass, Every : byte;
  P : PByte;
  function IsMultiple(NumberA, NumberB: Integer): Boolean;
  begin
    Result := (NumberA >= NumberB) and
              (NumberB > 0) and
              (NumberA mod NumberB = 0);
  end;
begin
  Result:=false;
  P:=FScanLine;
  If FInterlace then
  begin
    For Pass := 1 to 4 do
    begin
      Case Pass of
         1 : begin
               Row := 0;
               Every := 8;
             end;
         2 : begin
               Row := 4;
               Every := 8;
             end;
         3 : begin
               Row := 2;
               Every := 4;
             end;
         4 : begin
               Row := 1;
               Every := 2;
             end;
        end;
      Repeat
        for Col:=0 to Img.Width-1 do
        begin
          Img.Colors[Col,Row]:=FPalette[P^];
          Inc(P);
        end;
        Inc(Row, Every);
      until Row >= Img.Height;
    end;
  end
  else
  begin
    for Row:=0 to Img.Height-1 do
      for Col:=0 to Img.Width-1 do
      begin
        Img.Colors[Col,Row]:=FPalette[P^];
        Inc(P);
      end;
  end;
  Result:=true;
end;

function TFPReaderGif.InternalCheck(Stream: TStream): boolean;

var
  OldPos: Int64;
  n: Int64;
  
begin
  Result:=False;
  if Stream = nil then
    exit;
  OldPos:=Stream.Position;
  try
    n := SizeOf(FHeader);
    Result:=(Stream.Read(FHeader,n)=n)
            and (FHeader.Signature = 'GIF') 
            and ((FHeader.Version = '87a') or (FHeader.Version = '89a'));
  finally
    Stream.Position := OldPos;
  end;
end;

function TFPReaderGif.SkipBlock(Stream: TStream): byte;
var
  Introducer,
  Labels,
  SkipByte : byte;
begin
  Stream.read(Introducer,1);
  if Introducer = $21 then
  begin
     Stream.read(Labels,1);
     Case Labels of
       $FE, $FF :     // Comment Extension block or Application Extension block
            while true do
            begin
              Stream.Read(SkipByte, 1);
              if SkipByte = 0 then Break;
              Stream.Seek(SkipByte, soFromCurrent);
            end;
       $F9 :         // Graphics Control Extension block
            begin
              Stream.Read(FGraphicsCtrlExt, SizeOf(FGraphicsCtrlExt));
              FGraphCtrlExt:=True;
            end;
       $01 :        // Plain Text Extension block
            begin
              Stream.Read(SkipByte, 1);
              Stream.Seek(SkipByte, soFromCurrent);
              while true do
              begin
                Stream.Read(SkipByte, 1);
                if SkipByte = 0 then Break;
                Stream.Seek(SkipByte, soFromCurrent);
              end;
            end;
      end;
  end;
  Result:=Introducer;
end;

constructor TFPReaderGif.Create;
begin
  inherited Create;

end;

destructor TFPReaderGif.Destroy;
begin

  inherited Destroy;
end;

initialization
  ImageHandlers.RegisterImageReader ('GIF Graphics', 'gif', TFPReaderGif);
end.

