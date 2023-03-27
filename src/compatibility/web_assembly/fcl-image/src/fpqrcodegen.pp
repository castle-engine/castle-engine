{ **********************************************************************
    This file is part of the Free Pascal class library FCL.
    Pascal translation and additions (c) 2017 by Michael Van Canneyt,
    member of the Free Pascal development team.

    Ported from Nayuki's library with permission (see below).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright of the Pascal version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    QR Code generator library (C language)
    Copyright (c) Project Nayuki. (MIT License)
    https://www.nayuki.io/page/qr-code-generator-library
  **********************************************************************}
{$mode objfpc}
unit FPQRCodeGen;

interface

uses sysutils;

{---- Enum and struct types----}

Type
  TQRString = UTF8String;
  // The error correction level used in a QR Code symbol.
  TQRErrorLevelCorrection = (EccLOW,EccMEDIUM,EccQUARTILE,EccHIGH);
  // The mask pattern used in a QR Code symbol.
  TQRMask = (mp0,mp1,mp2,mp3,mp4,mp5,mp6,mp7,mpAuto);
  // The mode field of a segment.
  TQRMode = (mNUMERIC,mALPHANUMERIC,mBYTE,mKANJI,mECI);
  // Buffer to hold the bitmask.
  TQRBuffer = TBytes;


{ 
 * A segment of user/application data that a QR Code symbol can convey.
 * Each segment has a mode, a character count, and character/general data that is
 * already encoded as a sequence of bits. The maximum allowed bit length is 32767,
 * because even the largest QR Code (version 40) has only 31329 modules.
 }
  TQRSegment = record
    // The mode indicator for this segment.
    mode : TQRMode;
    // The length of this segment's unencoded data. Always in the range [0, 32767].
    // for numeric, alphanumeric, and kanji modes, this measures in Unicode code points.
    // for byte mode, this measures in bytes (raw binary data, text in UTF-8, or other encodings).
    // for ECI mode, this is always zero.
    numChars : word;
    // The data bits of this segment, packed in bitwise big endian.
    // Can be null if the bit length is zero.
    data : TQRBuffer;
    // The number of valid data bits used in the buffer. Requires
    // 0 <= bitLength <= 32767, and bitLength <= (capacity of data array) * 8.
    bitLength : integer; // Can be -1
  end;
  TQRSegmentArray = Array of TQRSegment;


{---- Macro constants and functions ----}

// The minimum and maximum defined QR Code version numbers for Model 2.
Type
  TQRVersion = 1..40;

Const
  QRVERSIONMIN  = Low(TQRversion);
  QRVERSIONMAX  = High(TQRVersion);

// Calculates the number of bytes needed to store any QR Code up to and including the given version number,
// as a compile-time constant. for example, 'uint8_t buffer[qrcodegen_BUFFER_LEN_FOR_VERSION(25)];'
// can store any single QR Code from version 1 to 25, inclusive.
// Requires qrcodegen_VERSION_MIN <= n <= qrcodegen_VERSION_MAX.
Function QRBUFFER_LEN_FOR_VERSION(n : TQRVersion) : integer;

// The worst-case number of bytes needed to store one QR Code, up to and including
// version 40. This value equals 3918, which is just under 4 kilobytes.
// Use this more convenient value to avoid calculating tighter memory bounds for buffers.
Const
  QRBUFFER_LEN_MAX = 3918;

Type

  { TQRCodeGenerator }

  TQRCodeGenerator = Class
  private
    FBECL: Boolean;
    FBufferLength: Word;
    FBytes: TQRBuffer;
    FECL: TQRErrorLevelCorrection;
    FMask: TQRMask;
    FMaxVersion: TQRVersion;
    FMinVersion: TQRVersion;
    function GetBits(X : Word; Y : Word): Boolean;
    function GetSize: Integer;
    procedure SetBufferLength(AValue: Word);
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure Generate(aText : TQRString);
    Procedure Generate(aNumber : Int64);
    // Input
    Property ErrorCorrectionLevel : TQRErrorLevelCorrection Read FECL Write FECL;
    Property MinVersion : TQRVersion Read FMinVersion Write FMinVersion;
    Property MaxVersion : TQRVersion Read FMaxVersion Write FMaxVersion;
    Property Mask : TQRMask Read FMask Write FMask;
    Property BoostErrorCorrectionLevel : Boolean Read FBECL Write FBECL;
    Property BufferLength : Word Read FBufferLength Write SetBufferLength;
    // Result
    Property Size : Integer Read GetSize;
    Property Bytes : TQRBuffer Read FBytes;
    Property Bits[X : Word; Y : Word] : Boolean Read GetBits;
  end;
  EQRCode = Class(Exception);

{---- Functions to generate QR Codes ----}

{ 
 * Encodes the given text string to a QR Code symbol, returning true if encoding succeeded.
 * If the data is too long to fit in any version in the given range
 * at the given ECC level, then false is returned.
 * - The input text must be encoded in UTF-8 and contain no NULs.
 * - The variables ecl and mask must correspond to enum constant values.
 * - Requires 1 <= minVersion <= maxVersion <= 40.
 * - The arrays tempBuffer and qrcode must each have a length
 *   of at least qrcodegen_BUFFER_LEN_FOR_VERSION(maxVersion).
 * - After the function returns, tempBuffer contains no useful data.
 * - If successful, the resulting QR Code may use numeric,
 *   alphanumeric, or byte mode to encode the text.
 * - In the most optimistic case, a QR Code at version 40 with low ECC
 *   can hold any UTF-8 string up to 2953 bytes, or any alphanumeric string
 *   up to 4296 characters, or any digit string up to 7089 characters.
 *   These numbers represent the hard upper limit of the QR Code standard.
 * - Please consult the QR Code specification for information on
 *   data capacities per version, ECC level, and text encoding mode.
 }
function QREncodeText(aText : TQRString; tempBuffer, qrcode : TQRBuffer;
	ecl : TQRErrorLevelCorrection; minVersion, maxVersion : TQRVersion;  mask : TQRMask;  boostEcl : Boolean) : boolean;


{ 
 * Encodes the given binary data to a QR Code symbol, returning true if encoding succeeded.
 * If the data is too long to fit in any version in the given range
 * at the given ECC level, then false is returned.
 * - The input array range dataAndTemp[0 : dataLen] should normally be
 *   valid UTF-8 text, but is not required by the QR Code standard.
 * - The variables ecl and mask must correspond to enum constant values.
 * - Requires 1 <= minVersion <= maxVersion <= 40.
 * - The arrays dataAndTemp and qrcode must each have a length
 *   of at least QRBUFFER_LEN_FOR_VERSION(maxVersion).
 * - After the function returns, the contents of dataAndTemp may have changed,
 *   and does not represent useful data anymore.
 * - If successful, the resulting QR Code will use byte mode to encode the data.
 * - In the most optimistic case, a QR Code at version 40 with low ECC can hold any byte
 *   sequence up to length 2953. This is the hard upper limit of the QR Code standard.
 * - Please consult the QR Code specification for information on
 *   data capacities per version, ECC level, and text encoding mode.
 }
function QREncodeBinary(dataAndTemp : TQRBuffer; dataLen : Integer; qrcode : TQRBuffer;
	ecl: TQRErrorLevelCorrection; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl : Boolean) : Boolean;


{ 
 * Tests whether the given string can be encoded as a segment in alphanumeric mode.
 }
Function QRIsAlphanumeric(aText : TQRString) : Boolean;


{ 
 * Tests whether the given string can be encoded as a segment in numeric mode.
 }
Function QRIsNumeric(atext : TQRString) : Boolean;


{ 
 * Returns the number of bytes (uint8_t) needed for the data buffer of a segment
 * containing the given number of characters using the given mode. Notes:
 * - Returns SIZE_MAX on failure, i.e. numChars > INT16_MAX or
 *   the number of needed bits exceeds INT16_MAX (i.e. 32767).
 * - Otherwise, all valid results are in the range [0, ceil(INT16_MAX / 8)], i.e. at most 4096.
 * - It is okay for the user to allocate more bytes for the buffer than needed.
 * - for byte mode, numChars measures the number of bytes, not Unicode code points.
 * - for ECI mode, numChars must be 0, and the worst-case number of bytes is returned.
 *   An actual ECI segment can have shorter data. for non-ECI modes, the result is exact.
 }
Function QRCalcSegmentBufferSize(aMode: TQRMode; numChars : Cardinal) : Cardinal;


{ 
 * Returns a segment representing the given binary data encoded in byte mode.
 }
Function QRmakeBytes(data: TQRBuffer; Buf : TQRBuffer) : TQRSegment;

{ 
 * Returns a segment representing the given string of decimal digits encoded in numeric mode.
 }
Function QRMakeNumeric(digits : TQRString; buf : TQRBuffer) :  TQRSegment;

{ 
 * Returns a segment representing the given text string encoded in alphanumeric mode.
 * The characters allowed are: 0 to 9, A to Z (uppercase only), space,
 * dollar, percent, asterisk, plus, hyphen, period, slash, colon.
 }
Function QRMakeAlphanumeric(aText : TQRString; buf : TQRBuffer) : TQRSegment;

{ 
 * Returns a segment representing an Extended Channel Interpretation
 * (ECI) designator with the given assignment value.
 }
Function QRMakeECI(assignVal : Integer;  buf: TQRBuffer) : TQRSegment;


{ 
 * Renders a QR Code symbol representing the given data segments at the given error correction
 * level or higher. The smallest possible QR Code version is automatically chosen for the output.
 * Returns true if QR Code creation succeeded, or false if the data is too long to fit in any version.
 * This function allows the user to create a custom sequence of segments that switches
 * between modes (such as alphanumeric and binary) to encode text more efficiently.
 * This function is considered to be lower level than simply encoding text or binary data.
 * To save memory, the segments' data buffers can alias/overlap tempBuffer, and will
 * result in them being clobbered, but the QR Code output will still be correct.
 * But the qrcode array must not overlap tempBuffer or any segment's data buffer.
 }
Function QREncodeSegments(Segs : TQRSegmentArray; ecl: TQRErrorLevelCorrection; tempBuffer, qrcode : TQRBuffer) : Boolean;

{ 
 * Renders a QR Code symbol representing the given data segments with the given encoding parameters.
 * Returns true if QR Code creation succeeded, or false if the data is too long to fit in the range of versions.
 * The smallest possible QR Code version within the given range is automatically chosen for the output.
 * This function allows the user to create a custom sequence of segments that switches
 * between modes (such as alphanumeric and binary) to encode text more efficiently.
 * This function is considered to be lower level than simply encoding text or binary data.
 * To save memory, the segments' data buffers can alias/overlap tempBuffer, and will
 * result in them being clobbered, but the QR Code output will still be correct.
 * But the qrcode array must not overlap tempBuffer or any segment's data buffer.
 }
Function QREncodeSegmentsAdvanced(Segs : TQRSegmentArray; ecl: TQRErrorLevelCorrection;
	minVersion, maxVersion : TQRVersion; mask : TQRMask; boostEcl : Boolean; tempBuffer, qrcode : TQRBuffer) : Boolean;


{---- Functions to extract raw data from QR Codes ----}

{ 
 * Returns the side length of the given QR Code, assuming that encoding succeeded.
 * The result is in the range [21, 177]. Note that the length of the array buffer
 * is related to the side length - every 'uint8_t qrcode[]' must have length at least
 * QRBUFFER_LEN_FOR_VERSION(version), which equals ceil(size^2 / 8 + 1).
 }
Function QRgetSize(qrcode : TQRBuffer) : Byte;


{ 
 * Returns the color of the module (pixel) at the given coordinates, which is either
 * false for white or true for black. The top left corner has the coordinates (x=0, y=0).
 * If the given coordinates are out of bounds, then false (white) is returned.
 }
Function QRgetModule(qrcode : TQRBuffer; x, y : word) : Boolean;

Implementation

Type
  TDegree = 1..30;
  TGenerator = Array[0..29] of byte;
  TPatternPositions = array[0..6] of byte;

{---- Forward declarations for private functions ----}

procedure appendBitsToBuffer(val : cardinal; numBits : integer; buffer : TQRBuffer; var bitLen : integer); forward;
procedure appendErrorCorrection(data : TQRBuffer; version: TQRVersion; ecl:  TQRErrorLevelCorrection; Result: TQRBuffer); forward;
function getNumDataCodewords(version : TQRVersion; ecl : TQRErrorLevelCorrection) : integer; forward;
function getNumRawDataModules(version : TQRVersion): integer; forward;
procedure calcReedSolomonGenerator(degree : TDegree; out result : TGenerator); forward;
procedure calcReedSolomonRemainder(const data : PByte; dataLen : Integer; constref generator : TGenerator; degree : TDegree; result : PByte); forward;
function  finiteFieldMultiply(x,y : Byte) : Byte; forward;
procedure initializeFunctionModules(version : TQRVersion; qrcode : TQRBuffer); forward;
procedure drawWhiteFunctionModules(qrcode : TQRBuffer; version : TQRVersion); forward;
procedure drawFormatBits(ecl : TQRErrorLevelCorrection; mask : TQRMask; qrcode : TQRBuffer); forward;
function getAlignmentPatternPositions(version : TQRVersion; var res : TPatternPositions) : Integer; forward;
procedure fillRectangle(left,top,width,height : Integer; qrcode : TQRBuffer); forward;
procedure drawCodewords(const data : TQRBuffer; dataLen : integer; qrcode : TQRBuffer); forward;
procedure applyMask(Modules : TQRBuffer; qrcode : TQRBuffer; mask : TQRMask); forward;
function getPenaltyScore(const qrcode : TQRBuffer) : int64; forward;
function getModule(qrcode : TQRBuffer; x, y : word) : Boolean; forward;
procedure setModule(qrcode : TQRBuffer; x,y : Word; isBlack : boolean); forward;
procedure setModuleBounded(qrcode : TQRBuffer; x,y : Word; isBlack : Boolean); forward;
function calcSegmentBitLength(mode : TQRMode; numChars : Integer) : integer; forward;
function getTotalBits(segs : TQRSegmentArray; version : TQRVersion) : integer; forward;
function numCharCountBits(mode : TQRMode; version : TQRVersion) : integer; forward;



{---- Private tables of constants ----}

// for checking text and encoding segments.
const
  ALPHANUMERIC_CHARSET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';

// for generating error correction codes.
const
  ECC_CODEWORDS_PER_BLOCK : Array[0..3,0..40] of shortint = (
	// Version: (note that index 0 is for padding, and is set to an illegal value)
	//0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
	(-1,  7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Low
	(-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28),  // Medium
	(-1, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Quartile
	(-1, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)  // High
  );

// for generating error correction codes.
  NUM_ERROR_CORRECTION_BLOCKS : Array [0..3,0..40] of shortint = (
	// Version: (note that index 0 is for padding, and is set to an illegal value)
	//0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
	(-1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),  // Low
	(-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),  // Medium
	(-1, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68),  // Quartile
	(-1, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81)  // High
 );

// for automatic mask pattern selection.
const
  PENALTY_N1 = 3;
  PENALTY_N2 = 3;
  PENALTY_N3 = 40;
  PENALTY_N4 = 10;


{---- High-level QR Code encoding functions ----}

// Public function - see documentation comment in header file.
function QREncodeText(aText : TQRString; tempBuffer, qrcode : TQRBuffer;
	ecl : TQRErrorLevelCorrection; minVersion, maxVersion : TQRVersion;  mask : TQRMask;  boostEcl : Boolean) : boolean;

var
  i, buflen, textLen : Integer;
  seg : TQRSegmentArray;
  failed : Boolean;

begin
  Result:=False;
  textLen:=Length(aText);
  if (textLen=0) then
    exit(QRencodeSegmentsAdvanced(Nil,ecl,minVersion, maxVersion, mask, boostEcl, tempBuffer, qrcode));
  bufLen:=QRBUFFER_LEN_FOR_VERSION(maxVersion);
  SetLength(Seg,1);
  if (QRisNumeric(aText)) then
    begin
    Failed:=(QRcalcSegmentBufferSize(mNUMERIC, textLen) > bufLen);
    if not failed then
      seg[0]:=QRmakeNumeric(aText,tempBuffer);
    end
  else if (QRisAlphanumeric(aText)) then
    begin
    Failed:=(QRcalcSegmentBufferSize(mALPHANUMERIC, textLen) > bufLen);
    if not Failed then
      Seg[0]:=QRMakeAlphanumeric(aText, tempBuffer);
    end
  else
    begin
    Failed:=(textLen > bufLen);
    if not Failed then
      begin
      for I:=1 to Textlen do
        tempBuffer[i-1]:=Ord(aText[i]);
      seg[0].mode:=mBYTE;
      seg[0].bitLength:=calcSegmentBitLength(seg[0].mode, textLen);
      Failed:=seg[0].bitLength=-1;
      if not Failed then
        begin
	seg[0].numChars:=textLen;
        seg[0].data:=tempBuffer;
        end;
      end;
    end;
  Result:=Not Failed;
  if failed then
    Qrcode[0]:=0  // Set size to invalid value for safety
  else
    Result:=QRencodeSegmentsAdvanced(seg, ecl, minVersion, maxVersion, mask, boostEcl, tempBuffer, qrcode);
end;


// Public function - see documentation comment in header file.
function QREncodeBinary(dataAndTemp : TQRBuffer; dataLen : Integer; qrcode : TQRBuffer;
	ecl: TQRErrorLevelCorrection; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl : Boolean) : Boolean;

var
  seg : TQRSegmentArray;

begin
  Result:=False;
  SetLength(Seg,1);
  seg[0].mode:=mBYTE;
  seg[0].bitLength:=calcSegmentBitLength(seg[0].mode, dataLen);
  if (seg[0].bitLength=-1) then
    begin
    qrcode[0]:=0;  // Set size to invalid value for safety
    exit;
    end;
  seg[0].numChars:=dataLen;
  seg[0].data:=dataAndTemp;
  Result:=QRencodeSegmentsAdvanced(seg, ecl, minVersion, maxVersion, mask, boostEcl, dataAndTemp, qrcode);
end;


// Appends the given sequence of bits to the given byte-based bit buffer, increasing the bit length.
procedure appendBitsToBuffer(val : cardinal; numBits : integer; buffer : TQRBuffer; var bitLen : integer);

var
  I,idx : integer;

begin
  assert((0 <= numBits) and (numBits <= 16) and ((val shr numBits) = 0));
  for I:=numBits-1 downto 0 do
    begin
    idx:=bitLen shr 3;
    buffer[idx]:=buffer[idx]  or ((val shr i) and 1) shl (7 - (bitLen and 7));
    Inc(Bitlen);
    end;
end;

{---- Error correction code generation functions ----}

// Appends error correction bytes to each block of the given data array, then interleaves bytes
// from the blocks and stores them in the result array. data[0 : rawCodewords - totalEcc] contains
// the input data. data[rawCodewords - totalEcc : rawCodewords] is used as a temporary work area
// and will be clobbered by this function. The final answer is stored in result[0 : rawCodewords].
procedure appendErrorCorrection(data : TQRBuffer; version: TQRVersion; ecl:  TQRErrorLevelCorrection; Result: TQRBuffer);

var
  numBlocks : Shortint;
  blockEccLen : Shortint;
  blocklen,I,J,K,L : integer;
  rawCodewords : Integer;
  dataLen : Integer;
  numShortBlocks : Integer;
  shortBlockDataLen : Integer;
  generator : TGenerator;

begin
  numBlocks:=NUM_ERROR_CORRECTION_BLOCKS[Ord(ecl)][version];
  blockEccLen:=ECC_CODEWORDS_PER_BLOCK[Ord(ecl)][version];
  rawCodewords:=getNumRawDataModules(version) div 8;
  dataLen := rawCodewords - blockEccLen * numBlocks;
  numShortBlocks := numBlocks - (rawCodewords mod numBlocks);
  shortBlockDataLen := (rawCodewords div numBlocks) - blockEccLen;
  // Split data into blocks and append ECC after all data
  calcReedSolomonGenerator(blockEccLen, generator);
  j:=Datalen;
  k:=0;
  for I:=0 to Numblocks-1 do
    begin
    blockLen:=shortBlockDataLen;
    if (i>=numShortBlocks) then
      Inc(blockLen);
    calcReedSolomonRemainder(@data[k],blockLen,generator,blockEccLen, @data[j]);
    Inc(j,blockEccLen);
    Inc(k,blockLen);
    end;
  // Interleave (not concatenate) the bytes from every block into a single sequence
  K:=0;
  for I:=0 to numBlocks-1 do
    begin
    l:=I;
    for J:=0 to shortBlockDataLen-1 do
      begin
      result[l]:=data[k];
      Inc(k);
      Inc(L,numblocks);
      end;
    if (i>=numShortBlocks) then
    Inc(k);
    end;
  k:=(numShortBlocks + 1)* shortBlockDataLen;
  l:=numBlocks * shortBlockDataLen;
  for i:=numShortBlocks to Numblocks-1 do
    begin
    result[l]:=data[k];
    Inc(k,shortBlockDataLen+1);
    Inc(l);
    end;
  k:=datalen;
  for I:=0 to Numblocks-1 do
    begin
    l:=dataLen + i;
    for j:=0 to blockEccLen-1 do
      //(int j = 0, ; j < blockEccLen; j++, )
      begin
      result[l]:=data[k];
      Inc(k);
      Inc(l,numBlocks);
      end;
    end;
end;


// Returns the number of 8-bit codewords that can be used for storing data (not ECC),
// for the given version number and error correction level. The result is in the range [9, 2956].
function getNumDataCodewords(version : TQRVersion; ecl : TQRErrorLevelCorrection) : integer;

var
  v,e : integer;

begin
  v:=version;
  e:=Ord(ecl);
  result:=(getNumRawDataModules(v) div 8) - (ECC_CODEWORDS_PER_BLOCK[e][v] * NUM_ERROR_CORRECTION_BLOCKS[e][v]);
end;


// Returns the number of data bits that can be stored in a QR Code of the given version number, after
// all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
// The result is in the range [208, 29648]. This could be implemented as a 40-entry lookup table.
function getNumRawDataModules(version : TQRVersion): integer;

var
  numAlign: integer;

begin
  result := (16 * version + 128) * version + 64;
  if (version >= 2) then
    begin
    numAlign := version div 7 + 2;
    Dec(Result, (25 * numAlign - 10) * numAlign - 55);
    if (version >= 7) then
      Dec(result, 18 * 2);  // Subtract version information
    end;
end;

{---- Reed-Solomon ECC generator functions ----}

// Calculates the Reed-Solomon generator polynomial of the given degree, storing in result[0 : degree].
procedure calcReedSolomonGenerator(degree : TDegree; out result : TGenerator);

var
  I,J : byte;
  Root : Byte;

begin
  // Start with the monomial x^0
  Result[0]:=0; // Avoid warning
  FillChar(result,sizeof(TGenerator),0);
  result[degree-1]:= 1;
  // Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
  // drop the highest term, and store the rest of the coefficients in order of descending powers.
  // Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
  root:=1;
  for I:=0 to degree-1 do
    begin
    // Multiply the current product by (x - r^i)
    for j:=0 to Degree-1 do
      begin
      result[j] := finiteFieldMultiply(result[j], root);
      if (j+1<degree) then
        result[j] := result[j] xor result[j + 1];
      end;
    root:=finiteFieldMultiply(root, $02);
    end;
end;


// Calculates the remainder of the polynomial data[0 : dataLen] when divided by the generator[0 : degree], where all
// polynomials are in big endian and the generator has an implicit leading 1 term, storing the result in result[0 : degree].
procedure calcReedSolomonRemainder(const data : PByte; dataLen : Integer; constref generator : TGenerator; degree : TDegree; result : PByte);

var
  I,J : Integer;
  factor : byte ;

begin
  FillChar(Result^,degree,0);
  for I:=0 to Datalen-1 do
    begin
    factor:=data[i] xor result[0];
    move( result[1],result[0],(degree - 1));
    result[degree-1] := 0;
    for j:=0 to degree-1 do
      begin
      result[j]:=result[j] xor finiteFieldMultiply(generator[j], factor);
      end;
    end;
end;


// Returns the product of the two given field elements modulo GF(2^8/0x11D).
// All inputs are valid. This could be implemented as a 256*256 lookup table.
function  finiteFieldMultiply(x,y : Byte) : Byte;

var
  Z : Byte;
  I : shortint;

begin
  // Russian peasant multiplication
  z:=0;
  for I:=7 downto 0 do
    begin
    z := (z shl 1) xor ((z shr 7) * $11D);
    z := z xor ((y >> i) and 1) * x;
    end;
  Result:=Z;
end;



{---- Drawing function modules ----}

// Clears the given QR Code grid with white modules for the given
// version's size, then marks every function module as black.
procedure initializeFunctionModules(version : TQRVersion; qrcode : TQRBuffer);

var
  qrsize : byte;
  alignPatPos : TPatternPositions;
  i,j,numAlign : integer;

begin
  // Initialize QR Code
  qrsize:= version * 4 + 17;
  FillChar(qrcode[0], ((qrsize * qrsize + 7) div 8 + 1),0);
  qrcode[0]:=qrsize;

  // Fill horizontal and vertical timing patterns
  fillRectangle(6, 0, 1, qrsize, qrcode);
  fillRectangle(0, 6, qrsize, 1, qrcode);

  // Fill 3 finder patterns (all corners except bottom right) and format bits
  fillRectangle(0, 0, 9, 9, qrcode);
  fillRectangle(qrsize - 8, 0, 8, 9, qrcode);
  fillRectangle(0, qrsize - 8, 9, 8, qrcode);

  // Fill numerous alignment patterns
  alignPatPos[0]:=0; // Avoid warning
  FillChar(alignPatPos,SizeOf(TPatternPositions),0);
  numAlign:=getAlignmentPatternPositions(version, alignPatPos);
  for i:=0 to numAlign-1 do
    for j:=0 to NumAlign-1 do
      begin
      if ((i=0) and (j=0)) or ((i=0) and (j=(numAlign-1))) or ((i=(numAlign-1)) and (j=0)) then
        continue;  // Skip the three finder corners
      fillRectangle(alignPatPos[i]-2, alignPatPos[j]-2,5,5, qrcode);
      end;

  // Fill version blocks
  if (version >= 7) then
    begin
    fillRectangle(qrsize - 11, 0, 3, 6, qrcode);
    fillRectangle(0, qrsize - 11, 6, 3, qrcode);
    end;
end;


// Draws white function modules and possibly some black modules onto the given QR Code, without changing
// non-function modules. This does not draw the format bits. This requires all function modules to be previously
// marked black (namely by initializeFunctionModules()), because this may skip redrawing black function modules.
procedure drawWhiteFunctionModules(qrcode : TQRBuffer; version : TQRVersion);

var
  rem,i,j,k,l,dist,qrsize, numalign : integer;
  data : int64;
  alignPatPos : TPatternPositions;

begin
  // Draw horizontal and vertical timing patterns
  qrsize:=QRgetSize(qrcode);
  I:=7;
  While (i < qrsize - 7) do
    begin
    setModule(qrcode, 6, i, false);
    setModule(qrcode, i, 6, false);
    Inc(I,2);
    end;

  // Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
  for I:=-4 to 4 do
    for J:=-4 to 4 do
      begin
      dist:=abs(i);
      if (abs(j) > dist) then
        dist:=abs(j);
      if ((dist=2) or (dist=4)) then
        begin
        if (3+I>=0) then
          begin
          if (3+J>=0) then
            setModuleBounded(qrcode, 3 + j, 3 + i, false);
          setModuleBounded(qrcode, qrsize - 4 + j, 3 + i, false);
          end;
        if (3+J>=0) then
          setModuleBounded(qrcode, 3 + j, qrsize - 4 + i, false);
        end;
      end;

  // Draw numerous alignment patterns
  alignPatPos[0]:=0; // Avoid warning
  FillChar(alignPatPos,SizeOf(TPatternPositions),0);
  numAlign:=getAlignmentPatternPositions(version, alignPatPos);
  for i:=0 to numAlign-1 do
    for j:=0 to NumAlign-1 do
      begin
      if ((i=0) and (j=0)) or ((i=0) and (j=(numAlign-1))) or ((i=(numAlign-1)) and (j=0)) then
        continue;  // Skip the three finder corners
      for k:=-1 to 1 do
        for l:=-1 to 1 do
          setModule(qrcode, alignPatPos[i] + l, alignPatPos[j] + k, (k = 0) and (l = 0));
      end;

  if (version < 7) then
    exit;
  // Draw version blocks
  // Calculate error correction code and pack bits
  rem:=version;  // version is uint6, in the range [7, 40]
  for I:=0 to 11 do
    rem := (rem shl 1) xor ((rem shr 11) * $1F25);
  data := (version shl 12) or rem;  // uint18
  assert((data shr 18) = 0);

  // Draw two copies
  for I:=0 to 5 do
    for j:=0 to 2 do
      begin
      k := qrsize - 11 + j;
      setModule(qrcode, k, i, (data and 1) <> 0);
      setModule(qrcode, i, k, (data and 1) <> 0);
      data := data shr 1;
      end;
end;


// Draws two copies of the format bits (with its own error correction code) based
// on the given mask and error correction level. This always draws all modules of
// the format bits, unlike drawWhiteFunctionModules() which might skip black modules.
procedure drawFormatBits(ecl : TQRErrorLevelCorrection; mask : TQRMask; qrcode : TQRBuffer);

var
  qrsize,i,rem,data : integer;

begin
  // Calculate error correction code and pack bits
  Case ecl of
    EccLOW     :  data := 1;
    EccMEDIUM  :  data := 0;
    EccQUARTILE:  data := 3;
    EccHIGH    :  data := 2;
  end;
  data:=data shl 3 or ord(mask);  // ecl-derived value is uint2, mask is uint3
  rem:=data;
  for I:=0 to 9 do
    rem := (rem shl 1) xor ((rem shr 9) * $537);
  data := (data shl 10) or rem;
  data := data xor $5412;  // uint15
  assert((data shr 15)= 0);

  for i:=0 to 5 do
    setModule(qrcode, 8, i, ((data shr i) and 1) <> 0);
  setModule(qrcode, 8, 7, ((data shr 6) and 1) <> 0);
  setModule(qrcode, 8, 8, ((data shr 7) and 1) <> 0);
  setModule(qrcode, 7, 8, ((data shr 8) and 1) <> 0);
  for i:=9 to 14 do
    setModule(qrcode, 14 - i, 8, ((data  shr  i) and 1) <> 0);

  // Draw second copy
  qrsize := QRgetSize(qrcode);
  for i:=0 to 7 do
    setModule(qrcode, qrsize - 1 - i, 8, ((data  shr  i) and 1) <> 0);
  for i:=8 to 14 do
    setModule(qrcode, 8, qrsize - 15 + i, ((data  shr  i) and 1) <> 0);
  setModule(qrcode, 8, qrsize - 8, true);
end;


// Calculates the positions of alignment patterns in ascending order for the given version number,
// storing them to the given array and returning an array length in the range [0, 7].
function getAlignmentPatternPositions(version : TQRVersion; var res : TPatternPositions) : Integer;

var
  i,numalign, step, pos : Integer;

begin
  if (version = 1) then
    Exit(0);
  numAlign:=version div 7 + 2;
  if (version <> 32) then
    // ceil((size - 13) / (2*numAlign - 2)) * 2
    step := (version * 4 + numAlign * 2 + 1) div (2 * numAlign - 2) * 2
  else  // C-C-C-Combo breaker!
    step := 26;
  pos := version * 4 + 10;
  for i:=numAlign-1 downto 1 do
    begin
    res[i]:= pos;
    Dec(Pos,Step);
    end;
  res[0]:=6;
  Result:=numAlign;
end;


// Sets every pixel in the range [left : left + width] * [top : top + height] to black.
Procedure fillRectangle(left,top,width,height : Integer; qrcode : TQRBuffer);

var
  dy,dx : integer;

begin
  for dy:=0 to height-1 do
    for dx:=0 to width-1 do
      setModule(qrcode, left + dx, top + dy, true);
end;



{---- Drawing data modules and masking ----}

// Draws the raw codewords (including data and ECC) onto the given QR Code. This requires the initial state of
// the QR Code to be black at function modules and white at codeword modules (including unused remainder bits).

procedure drawCodewords(const data : TQRBuffer; dataLen : integer; qrcode : TQRBuffer);

var
  i,right,vert,j,y,x,qrsize : integer;
  black,upward : boolean;

begin
  qrsize := QRgetSize(qrcode);
  i := 0;  // Bit index into the data
  // Do the funny zigzag scan
  right :=qrsize - 1;
  While (right >= 1) do
    begin
    if (right=6) then
      right:=5;
    for vert:=0 to qrsize-1 do
      begin
      for j:=0 to 1 do
        begin
        x:=right - j;  // Actual x coordinate
        upward := ((right + 1) and 2) = 0;
        if upward then
          y:= qrsize - 1 - vert
        else
          y:= vert;  // Actual y coordinate
        if (not getModule(qrcode, x, y)) and (i < dataLen * 8) then
          begin
          black :=((data[i  shr  3]  shr  (7 - (i and 7))) and 1) <> 0;
          setModule(qrcode, x, y, black);
          Inc(i);
          end;
        // If there are any remainder bits (0 to 7), they are already
        // set to 0/false/white when the grid of modules was initialized
        end;
      end;
    Dec(right,2);
    end;
  assert(i = dataLen * 8);
end;


// XORs the data modules in this QR Code with the given mask pattern. Due to XOR's mathematical
// properties, calling applyMask(..., m) twice with the same value is equivalent to no change at all.
// This means it is possible to apply a mask, undo it, and try another mask. Note that a final
// well-formed QR Code symbol needs exactly one mask applied (not zero, not two, etc.).
procedure applyMask(Modules : TQRBuffer; qrcode : TQRBuffer; mask : TQRMask);

var
  x,y,qrsize : integer;
  invert,val : boolean;

begin
//  assert(0 <= (int)mask && (int)mask <= 7);  // Disallows mpAUTO
  qrsize:=QRgetSize(qrcode);
  for y:=0 to qrsize-1 do
    for x:=0 to qrsize-1 do
      begin
      if (getModule(Modules, x, y)) then
        continue;
      case mask of
        mp0: invert := (x + y) mod 2 = 0;
        mp1: invert := y mod 2 = 0;
        mp2: invert := x mod 3 = 0;
        mp3: invert := (x + y) mod 3 = 0;
        mp4: invert := (x div 3 + y div 2) mod 2 = 0;
        mp5: invert := x * y mod 2 + x * y mod 3 = 0;
        mp6: invert := (x * y mod 2 + x * y mod 3) mod 2 = 0;
        mp7: invert := ((x + y) mod 2 + x * y mod 3) mod 2 = 0;
      end;
      val:=getModule(qrcode, x, y);
      setModule(qrcode, x, y, val xor invert);
      end;
end;


// Calculates and returns the penalty score based on state of the given QR Code's current modules.
// This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
function getPenaltyScore(const qrcode : TQRBuffer) : int64;

var
  k,total,black,bits,y,x,runx,runy,qrsize : integer;
  color,colory,colorx : boolean;

begin
  qrsize := QRgetSize(qrcode);
  result := 0;
  // Adjacent modules in row having same color
  for y:=0 to qrsize-1 do
    begin
    runx:=0;
    colorx:=False;
    for x := 0 to qrsize-1 do
      begin
      if ((x = 0) or (getModule(qrcode,x,y) <> colorX)) then
        begin
        colorX := getModule(qrcode, x, y);
        runX := 1;
        end
      else
        begin
        inc(runx);
        if (runX = 5) then
          Inc(result,PENALTY_N1)
        else if (runX > 5) then
          Inc(result);
        end;
      end;
    end;
  // Adjacent modules in column having same color
  for x:=0 to qrsize-1 do
    begin
    runy:=0;
    colorY:=false;
    for y:=0 to qrsize-1 do
      begin
      if ((y= 0) or (getModule(qrcode, x, y) <> colorY)) then
        begin
        colorY := getModule(qrcode, x, y);
        runY := 1;
        end
      else
        begin
        inc(runY);
        if (runY = 5) then
          Inc(result,PENALTY_N1)
        else if (runY > 5) then
          inc(result);
        end;
      end;
    end;
  // 2*2 blocks of modules having same color
  for y:=0 to qrsize-2 do
    for x := 0 to qrsize-2 do
      begin
      color:=getModule(qrcode, x, y);
      if ((color= getModule(qrcode, x + 1, y)) and
          (color= getModule(qrcode, x, y + 1)) and
          (color= getModule(qrcode, x + 1, y + 1))) then
         begin
         Inc(Result,PENALTY_N2);
         end;
      end;
  // Finder-like pattern in rows
  for y:=0 to qrsize-1 do
    begin
    bits:=0;
    for x := 0 to qrsize-1 do
      begin
      bits:=((bits shl 1) and $7FF) or Ord(getModule(qrcode, x, y));
      if ((x>=10) and ((bits= $05D) or (bits=$5D0))) then  // Needs 11 bits accumulated
        Inc(result,PENALTY_N3);
      end;
    end;
  // Finder-like pattern in columns
  for x:=0 to qrsize-1 do
    begin
    bits:=0;
    for y := 0 to qrsize-1 do
      begin
      bits := ((bits shl 1) and $7FF) or Ord((getModule(qrcode, x, y)));
      if ((y>=10) and ((bits=$05D) or (bits=$5D0))) then
        Inc(result,PENALTY_N3);
      end;
    end;
  // Balance of black and white modules
  black:=0;
  for y:=0 to qrsize-1 do
    for x := 0 to qrsize-1 do
      if (getModule(qrcode, x, y)) then
        inc(black);
  Total:=qrsize * qrsize;
  // Find smallest k such that (45-5k)% <= dark/total <= (55+5k)%
  K:=0;
  black:=black*20;
  While (black < ((9-k)*total)) or (black > ((11+k)*total)) do
    begin
    Inc(result,PENALTY_N4);
    Inc(k);
    end;
end;



{---- Basic QR Code information ----}

// Public function - see documentation comment in header file.
Function QRgetSize(qrcode : TQRBuffer) : Byte;

begin
  assert(Length(qrcode)>0);
  result:=qrcode[0];
  assert(((QRVERSIONMIN * 4 + 17) <= result) and (result <= (QRVERSIONMAX * 4 + 17)));
end;


// Public function - see documentation comment in header file.
function QRgetModule(qrcode : TQRBuffer; x,y : Word) : Boolean;

var
  QrSize : Integer;
begin
  assert(Length(qrcode)>0);
  qrsize := qrcode[0];
  Result:= (x < qrsize) and (y < qrsize) and getModule(qrcode, x, y);
end;


// Gets the module at the given coordinates, which must be in bounds.
Function getModule(qrcode : TQRBuffer; x, y : word) : Boolean;

var
  index,bitindex,byteindex,qrsize : integer;

begin
  qrsize := qrcode[0];
  assert((21 <= qrsize) and (qrsize <= 177) and (x < qrsize) and (y < qrsize));
  index := y * qrsize + x;
  bitIndex := index and 7;
  byteIndex := (index  shr  3) + 1;
  result:= ((qrcode[byteIndex]  shr  bitIndex) and 1) <> 0;
end;


// Sets the module at the given coordinates, which must be in bounds.
procedure setModule(qrcode : TQRBuffer; x,y : Word; isBlack : boolean);

var
  index,bitindex,byteindex,qrsize : integer;

begin
  qrsize := qrcode[0];
  assert((21 <= qrsize) and (qrsize <= 177) and (x < qrsize) and (y < qrsize));
  index := y * qrsize + x;
  bitIndex := index and 7;
  byteIndex := (index  shr  3) + 1;
  if isBlack then
    qrcode[byteIndex] := qrcode[byteIndex] or (1 shl bitIndex)
  else
    qrcode[byteIndex] := qrcode[byteIndex] and ((1 shl bitIndex) xor $FF);
end;


// Sets the module at the given coordinates, doing nothing if out of bounds.
procedure setModuleBounded(qrcode : TQRBuffer; x,y : Word; isBlack : Boolean);

var
  qrsize : word;

begin
  qrsize := qrcode[0];
  if ((x < qrsize) and (y < qrsize)) then
    setModule(qrcode, x, y, isBlack);
end;



{---- Segment handling ----}

// Public function - see documentation comment in header file.
Function QRIsNumeric(atext : TQRString) : Boolean;

var
  L,I : integer;

begin
  Result:=True;
  I:=1;
  L:=Length(aText);
  While Result and (I<=L) do
    begin
    Result:=aText[I] in ['0'..'9'];
    Inc(I);
    end;
end;

Function QRIsAlphanumeric(aText : TQRString) : Boolean;

var
  L,I : integer;

begin
  Result:=True;
  I:=1;
  L:=Length(aText);
  While Result and (I<=L) do
    begin
    Result:=Pos(aText[I],ALPHANUMERIC_CHARSET)<>0;
    Inc(I);
    end;
end;



// Public function - see documentation comment in header file.
Function QRCalcSegmentBufferSize(aMode: TQRMode; numChars : Cardinal) : Cardinal;

var
  Temp : Integer;

begin
  temp:=calcSegmentBitLength(aMode, numChars);
  if (temp = -1) then
    Exit(MaxInt)
  else
    Result:=(temp + 7) div 8;
end;


// Returns the number of data bits needed to represent a segment
// containing the given number of characters using the given mode. Notes:
// - Returns -1 on failure, i.e. numChars > INT16_MAX or
//   the number of needed bits exceeds INT16_MAX (i.e. 32767).
// - Otherwise, all valid results are in the range [0, INT16_MAX].
// - for byte mode, numChars measures the number of bytes, not Unicode code points.
// - for ECI mode, numChars must be 0, and the worst-case number of bits is returned.
//   An actual ECI segment can have shorter data. for non-ECI modes, the result is exact.
function calcSegmentBitLength(mode : TQRMode; numChars : Integer) : integer;

var
  temp,N,Limit: integer;

begin
  Limit:=High(Smallint);
  if (numChars > Limit) then
    Exit(-1);
  n := numChars;
  result := -2;
  if (mode = mNUMERIC) then
    begin
    // n * 3 + ceil(n / 3)
    if (n > LIMIT / 3) then
      Exit(-1);
    result := n * 3;
    if n mod 3 = 0 then
      temp := n div 3
    else
      temp := n div 3 +1;
    if (temp > LIMIT - result) then
      Exit(-1);
    Inc(result,temp);
    end
  else if (mode = mALPHANUMERIC) then
    begin
    // n * 5 + ceil(n / 2)
    if (n > LIMIT / 5) then
      Exit(-1);
    result := n * 5;
    temp := n div 2 + n mod 2;
    if (temp > LIMIT - result) then
      Exit(-1);
    Inc(result,temp);
    end
  else if (mode = mBYTE) then
    begin
    if (n > LIMIT / 8) then
      Exit(-1);
    result := n * 8;
    end
  else if (mode = mKANJI) then
    begin
    if (n > LIMIT / 13)  then
      Exit(-1);
    result := n * 13;
    end
  else if ((mode = mECI) and (numChars = 0)) then
    result := 3 * 8;
  assert((0 <= result) and (result <= LIMIT));
end;


// Public function - see documentation comment in header file.
Function QRmakeBytes(data: TQRBuffer;Buf : TQRBuffer) : TQRSegment;

begin
  assert(Length(data)>0);
  result.mode := mBYTE;
  result.bitLength := calcSegmentBitLength(result.mode, length(data));
  assert(result.bitLength <> -1);
  result.numChars:= length(data);
  if (length(Data) > 0) then
    Move(data[0],buf[0], Length(Data));
  result.data := buf;
end;


// Public function - see documentation comment in header file.
Function QRMakeNumeric(digits : TQRString; buf : TQRBuffer) :  TQRSegment;

var
  accumcount, bitlen,len: integer;
  accumData : Cardinal;
  c : ansichar;

begin
  assert(Length(digits)>0);
  len := length(digits);
  result.mode := mNUMERIC;
  bitLen := calcSegmentBitLength(result.mode, len);
  assert(bitLen <> -1);
  result.numChars := len;
  if (bitLen > 0) then
    fillchar(buf[0], (bitLen + 7) div 8, 0);
  result.bitLength := 0;
  accumData := 0;
  accumCount := 0;
  for c in digits do
    begin
    assert(c in ['0'..'9']);
    accumData := accumData * 10 + (Ord(c) - Ord('0'));
    Inc(accumCount);
    if (accumCount = 3) then
      begin
      appendBitsToBuffer(accumData, 10, buf, result.bitLength);
      accumData := 0;
      accumCount := 0;
      end;
    end;
  if (accumCount > 0)  then // 1 or 2 digits remaining
    appendBitsToBuffer(accumData, accumCount * 3 + 1, buf, result.bitLength);
  assert(result.bitLength = bitLen);
  result.data := buf;
end;


// Public function - see documentation comment in header file.
Function QRMakeAlphanumeric(aText : TQRString; buf : TQRBuffer) : TQRSegment;

var
  p,accumcount, bitlen,len: integer;
  accumData : Cardinal;
  c : ansichar;

begin
  assert(atext<>'');
  len := length(atext);
  result.mode := mALPHANUMERIC;
  bitLen := calcSegmentBitLength(result.mode, len);
  assert(bitLen <> -1);
  result.numChars := len;
  fillchar(buf[0],Length(Buf), 0);
  result.bitLength := 0;
  accumData := 0;
  accumCount := 0;
  for c in atext do
    begin
    P:=Pos(C,ALPHANUMERIC_CHARSET);
    assert(P>0);
    accumData := accumData * 45 + (P - 1);
    Inc(accumCount);
    if (accumCount = 2) then
      begin
      appendBitsToBuffer(accumData, 11, buf, result.bitLength);
      accumData := 0;
      accumCount := 0;
      end;
    end;
  if (accumCount > 0) then // 1 character remaining
    appendBitsToBuffer(accumData, 6, buf, result.bitLength);
  assert(result.bitLength = bitLen);
  result.data := buf;
end;


// Public function - see documentation comment in header file.
Function QRMakeECI(assignVal : Integer;  buf: TQRBuffer) : TQRSegment;

begin
  result.mode := mECI;
  result.numChars := 0;
  result.bitLength := 0;
  if ((0 <= assignVal) and (assignVal < (1 shl 7))) then
    begin
    FillChar(buf[0],1,0);
    appendBitsToBuffer(assignVal, 8, buf, result.bitLength);
    end
  else if (((1 shl 7) <= assignVal) and (assignVal < (1 shl 14))) then
    begin
    FillChar(buf[0],2,0);
    appendBitsToBuffer(2, 2, buf, result.bitLength);
    appendBitsToBuffer(assignVal, 14, buf, result.bitLength);
    end
  else if (((1 shl 14) <= assignVal) and (assignVal < 1000000)) then
    begin
    FillChar(buf[0],3,0);
    appendBitsToBuffer(6, 3, buf, result.bitLength);
    appendBitsToBuffer(assignVal shr 10, 11, buf, result.bitLength);
    appendBitsToBuffer(assignVal and $3FF, 10, buf, result.bitLength);
    end
  else
    begin
    assert(false);
    end;
  result.data := buf;
end;


// Public function - see documentation comment in header file.
Function QREncodeSegments(Segs : TQRSegmentArray; ecl: TQRErrorLevelCorrection; tempBuffer, qrcode : TQRBuffer) : Boolean;

begin
  Result:=QRencodeSegmentsAdvanced(segs, ecl, QRVERSIONMIN, QRVERSIONMAX, mpAuto, True, tempBuffer, qrcode);
end;


// Public function - see documentation comment in header file.
Function QREncodeSegmentsAdvanced(Segs : TQRSegmentArray; ecl: TQRErrorLevelCorrection;
	minVersion, maxVersion : TQRVersion; mask : TQRMask; boostEcl : Boolean; tempBuffer, qrcode : TQRBuffer) : Boolean;

var
  modebits : byte;
  bitlen, I,j : Integer;
  Version : TQRVersion;
  dataUsedBits  : Integer;
  dataCapacityBits: integer; 
  terminatorBits : Integer;
  E: TQRErrorLevelCorrection; 
  padbyte : byte;
  m  :TQRMask;
  penalty,minpenalty : integer;
  
begin
  Result:=False;
  assert((segs <> Nil) and (length(segs) <> 0));

  // Find the minimal version number to use
  for version := minVersion to maxVersion do
    begin
    dataCapacityBits := getNumDataCodewords(version, ecl) * 8;  // Number of data bits available
    dataUsedBits := getTotalBits(segs, version);
    if ((dataUsedBits <> -1) and (dataUsedBits <= dataCapacityBits)) then
      break;  // This version number is found to be suitable
    if (version >= maxVersion) then
      begin  // All versions in the range could not fit the given data
      qrcode[0] := 0;  // Set size to invalid value for safety
      Exit;
      end;
    end;
  assert(dataUsedBits <> -1);

  // Increase the error correction level while the data still fits in the current version number
  for E:=EccMEDIUM to EccHIGH do
    begin
    if (boostEcl and (dataUsedBits <= getNumDataCodewords(version,E) * 8)) then
      ecl := E;
    end;
  // Create the data bit string by concatenating all segments
  dataCapacityBits := getNumDataCodewords(version, ecl) * 8;
  FillChar(qrcode[0], QRBUFFER_LEN_FOR_VERSION(version), 0);
  bitLen := 0;
  for I:=0 to Length(segs)-1 do
    begin
    case (segs[i].mode) of 
      mNUMERIC     :  modeBits := $1;
      mALPHANUMERIC:  modeBits := $2;
      mBYTE        :  modeBits := $4;
      mKANJI       :  modeBits := $8;
      mECI         :  modeBits := $7;
    else
      assert(false);
    end;
    appendBitsToBuffer(modeBits, 4, qrcode, bitLen);
    appendBitsToBuffer(segs[i].numChars, numCharCountBits(segs[i].mode, version), qrcode, bitLen);
    for j:=0 to segs[i].bitLength-1 do
      appendBitsToBuffer((segs[i].data[j  shr  3]  shr  (7 - (j and 7))) and 1, 1, qrcode, bitLen);
    end;

  // Add terminator and pad up to a byte if applicable
  terminatorBits := dataCapacityBits - bitLen;
  if (terminatorBits > 4) then
    terminatorBits := 4;
  appendBitsToBuffer(0, terminatorBits, qrcode, bitLen);
  appendBitsToBuffer(0, (8 - bitLen mod 8) mod 8, qrcode, bitLen);

  // Pad with alternate bytes until data capacity is reached
  padByte := $EC;
  While (bitLen < dataCapacityBits) do
    begin
    appendBitsToBuffer(padByte, 8, qrcode, bitLen);
    padbyte:=padbyte xor ($EC xor $11)
    end;
  assert(bitLen mod 8 = 0);
  // Draw function and data codeword modules
  appendErrorCorrection(qrcode, version, ecl, tempBuffer);
  // Draw function and data codeword modules
  initializeFunctionModules(version, qrcode);
  drawCodewords(tempBuffer, getNumRawDataModules(version) div 8, qrcode);
  drawWhiteFunctionModules(qrcode, version);
  initializeFunctionModules(version, tempBuffer);
  // Handle masking
  if (mask = mpAUTO) then
    begin  // Automatically choose best mask
    minPenalty := MaxInt;
    for m:=mp0 to mp7 do
      begin
      drawFormatBits(ecl, m, qrcode);
      applyMask(tempBuffer, qrcode, m);
      penalty := getPenaltyScore(qrcode);
      if (penalty < minPenalty) then
        begin
        mask := m;
        minPenalty := penalty;
        end;
      applyMask(tempBuffer, qrcode, m);  // Undoes the mask due to XOR
      end;
    end;
  assert(mask<>mpAuto);
  drawFormatBits(ecl, mask, qrcode);
  applyMask(tempBuffer, qrcode, mask);
  Result:= true;
end;


// Returns the number of bits needed to encode the given list of segments at the given version.
// The result is in the range [0, 32767] if successful. Otherwise, -1 is returned if any segment
// has more characters than allowed by that segment's mode's character count field at the version,
// or if the actual answer exceeds INT16_MAX.
function getTotalBits(segs : TQRSegmentArray; version : TQRVersion): integer;

var
  ccbits,I,numChars,bitLength : integer;
  temp : integer;

begin
  assert(Length(segs)>0);
  result := 0;
  for I:=0 to Length(segs)-1 do
    begin
    numChars := segs[i].numChars;
    bitLength := segs[i].bitLength;
    assert((0 <= numChars) and (numChars <= High(Smallint)));
    assert((0 <= bitLength) and (bitLength <= High(Smallint)));
    ccbits := numCharCountBits(segs[i].mode, version);
    assert((0 <= ccbits) and (ccbits <= 16));
    // Fail if segment length value doesn't fit in the length field's bit-width
    if (numChars >= (1 shl ccbits)) then
      exit(-1);
    temp := 4 + ccbits + bitLength;
    if (temp > High(SmallInt) - result) then
      Exit(-1);
    Inc(result, temp);
    end;
  assert((0 <= result) and (result <= High(Smallint)));
end;


// Returns the bit width of the segment character count field for the
// given mode at the given version number. The result is in the range [0, 16].
function numCharCountBits(mode : TQRMode; version : TQRVersion) : integer;

Type
  T3Bytes = array[0..2] of Integer;

Const
   bmNumeric : T3Bytes = (10, 12, 14);
   bmALPHANUMERIC : T3Bytes =  ( 9, 11, 13);
   bmBYTE : T3Bytes = ( 8, 16, 16);
   bmKANJI : T3Bytes = (8, 10, 12);

var
  I : Integer;

begin
  if (version<=9) then
    i:=0
  else if ((10 <= version) and (version <= 26)) then 
    i:=1
  else if ((27 <= version)) then
    i:=2
  else  
    begin
    assert(false);
    end;
  case (mode) of
    mNUMERIC     : Result:=bmNumeric[i]; 
    mALPHANUMERIC: Result:=bmALPHANUMERIC[i]; 
    mBYTE        : Result:=bmBYTE[i]; 
    mKANJI       : Result:=bmKANJI[i];
    mECI         : Result:=0;
  else
    assert(false);
  end
end;

Function QRBUFFER_LEN_FOR_VERSION(n : TQRVersion) : integer;
begin
  Result:=((((n) * 4 + 17) * ((n) * 4 + 17) + 7) div 8 + 1)
end;

{ ---------------------------------------------------------------------
  TQRCodeGenerator
  ---------------------------------------------------------------------}

function TQRCodeGenerator.GetBits(X : Word; Y : Word): Boolean;

begin
  if Assigned(FBytes) then
    Result:=getModule(FBytes,X,Y)
  else
    Result:=False;
end;

function TQRCodeGenerator.GetSize: Integer;

begin
  if Assigned(FBytes) then
    Result:=QRgetSize(FBytes)
  else
    Result:=-1;
end;

procedure TQRCodeGenerator.SetBufferLength(AValue: Word);

begin
  if AValue>QRBUFFER_LEN_MAX then
    AValue:=QRBUFFER_LEN_MAX;
  if FBufferLength=AValue then Exit;
  FBufferLength:=AValue;
end;

constructor TQRCodeGenerator.Create;

begin
  FMinVersion:=QRVersionMin;
  FMaxVersion:=QRVersionMax;
  FECL:=EccMEDIUM;
  FBufferLength:=QRBUFFER_LEN_MAX;
  SetLength(FBytes,0);
end;

destructor TQRCodeGenerator.Destroy;

begin
  SetLength(FBytes,0);
  inherited Destroy;
end;

procedure TQRCodeGenerator.Generate(aText: TQRString);

var
  Tmp : TQRBuffer;

begin
  SetLength(Tmp,FBufferLength);
  SetLength(FBytes,FBufferLength);
  if not QREncodeText(aText,tmp,FBytes,FECL,FMinVersion,FMaxVersion,FMask,FBECL) then
    Raise EQRCode.CreateFmt('Failed to generate QR Code for text "%s"',[aText]);
end;

procedure TQRCodeGenerator.Generate(aNumber: Int64);

var
  Tmp : TQRBuffer;
  aText : TQRString;

begin
  SetLength(Tmp,FBufferLength);
  SetLength(FBytes,FBufferLength);
  aText:=IntToStr(aNumber);
  if not QREncodeText(aText,tmp,FBytes,FECL,FMinVersion,FMaxVersion,FMask,FBECL) then
    Raise EQRCode.CreateFmt('Failed to generate QR Code for text "%s"',[aText]);
end;

end.
