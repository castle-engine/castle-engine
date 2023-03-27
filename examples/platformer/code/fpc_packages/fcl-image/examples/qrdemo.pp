{* 
 * QR Code generator demo (Pascal)
 * 
 * Run this command-line program with no arguments. The program
 * computes a demonstration QR Codes and print it to the console.
 *
 * Pascal Version: Copyright (c) Michael Van Canneyt (michael@freepascal.org)
 * Copyright (c) Project Nayuki. (MIT License)
 * https://www.nayuki.io/page/qr-code-generator-library
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 * - The above copyright notice and this permission notice shall be included in
 *   all copies or substantial portions of the Software.
 * - The Software is provided "as is", without warranty of any kind, express or
 *   implied, including but not limited to the warranties of merchantability,
 *   fitness for a particular purpose and noninfringement. In no event shall the
 *   authors or copyright holders be liable for any claim, damages or other
 *   liability, whether in an action of contract, tort or otherwise, arising from,
 *   out of or in connection with the Software or the use or other dealings in the
 *   Software.
 */
}
{$mode objfpc}
{$h+}
{$CODEPAGE UTF8}
uses fpqrcodegen, sysutils;

// Prints the given QR Code to the console.
Procedure printqr (qrcode : TQRBuffer);

var
  size : cardinal;
  border: byte;
  x,y : Integer;
  
begin
  Size:=QRgetSize(qrcode);
  border:=4;
  For Y:=-Border to size+Border-1 do
    begin
    For x:=-Border to size+Border-1 do
      if (X>=0) and (Y>=0) and QRgetModule(qrcode, x, y) then
        write('##') 
      else
        Write('  '); 
    writeln;
    end;
end;

// Creates a single QR Code, then prints it to the console.
procedure doBasicDemo;

var
  aText : string;
  errCorLvl : TQRErrorLevelCorrection;
  tempbuffer,
  qrcode: TQRBuffer;

begin
  SetLength(tempBuffer,QRBUFFER_LEN_MAX);
  SetLength(qrCode,QRBUFFER_LEN_MAX);
  aText:='Hello, world!';  // User-supplied text
  errCorLvl:=EccLOW;  // Error correction level
  if QRencodeText(atext, tempBuffer, qrcode, errCorLvl, QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
    printQr(qrcode);
end;

// Creates a variety of QR Codes that exercise different features of the library, and prints each one to the console.

procedure doVarietyDemo;

const
  UTF8Encoded : Array[0..34] of byte =
  ($E3,$81,$93,$E3,$82,$93,$E3,$81,$AB,$E3,$81,$A1,Ord('w'),Ord('a'),$E3,$80,$81,$E4,$B8,$96,$E7,$95,$8C,$EF,$BC,$81,$20,$CE,$B1,$CE,$B2,$CE,$B3,$CE,$B4);

var
  atext : UTF8String;
  tempbuffer,
  qrcode: TQRBuffer;

  procedure ResetBuffer;

  begin
    FillChar(tempBuffer[0],QRBUFFER_LEN_MAX,0);
    FillChar(qrCode[0],QRBUFFER_LEN_MAX,0);
  end;

begin
  // Project Nayuki URL
  SetLength(tempBuffer,QRBUFFER_LEN_MAX);
  SetLength(qrCode,QRBUFFER_LEN_MAX);
  if QRencodeText('https://www.nayuki.io/', tempBuffer, qrcode,
                        EccHIGH, QRVERSIONMIN, QRVERSIONMAX, mp3, true) then
    PrintQr(qrCode);
  // Numeric mode encoding (3.33 bits per digit)
  ResetBuffer;
  if QRencodeText('314159265358979323846264338327950288419716939937510', tempBuffer, qrcode,
        EccMEDIUM, QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
      printQr(qrcode);

  // Alphanumeric mode encoding (5.5 bits per character)
  ResetBuffer;
  if QRencodeText('DOLLAR-AMOUNT:$39.87 PERCENTAGE:100.00% OPERATIONS:+-*/', tempBuffer, qrcode,
      eccHIGH, QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
      printQr(qrcode);
  ResetBuffer;

  // Unicode text as UTF-8, and different masks
  SetLength(aText,Length(UTF8Encoded));
  Move(UTF8Encoded[0],atext[1],Length(UTF8Encoded));

  if QRencodeText(atext, tempBuffer, qrcode,
    eccQUARTILE, QRVERSIONMIN, QRVERSIONMAX, mp0, true) then
    printQr(qrcode);
  ResetBuffer;

  if QRencodeText(atext, tempBuffer, qrcode,
    eccQUARTILE, QRVERSIONMIN, QRVERSIONMAX, mp1, true) then
    printQr(qrcode);
  ResetBuffer;

  if QRencodeText(atext, tempBuffer, qrcode,
    eccQUARTILE, QRVERSIONMIN, QRVERSIONMAX, mp5, true) then
    printQr(qrcode);
  ResetBuffer;

  if QRencodeText(atext, tempBuffer, qrcode,
    eccQUARTILE, QRVERSIONMIN, QRVERSIONMAX, mp7, true) then
    printQr(qrcode);
  ResetBuffer;

  // Moderately large QR Code using longer text (from Lewis Carroll's Alice in Wonderland)
  atext :=
      'Alice was beginning to get very tired of sitting by her sister on the bank, '
      +'and of having nothing to do: once or twice she had peeped into the book her sister was reading, '
      +'but it had no pictures or conversations in it, ''and what is the use of a book,'' thought Alice '
      +'''without pictures or conversations?'' So she was considering in her own mind (as well as she could, '
      +'for the hot day made her feel very sleepy and stupid), whether the pleasure of making a '
      +'daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly '
      +'a White Rabbit with pink eyes ran close by her.';
  Writeln(atext);
    if QRencodeText(atext, tempBuffer, qrcode, eccHIGH, QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
      printQr(qrcode);

end;


procedure doSegmentDemo;

const
  kanjiChars : Array[0..28] of word = (  // Kanji mode encoding (13 bits per character)
        $0035, $1002, $0FC0, $0AED, $0AD7,
        $015C, $0147, $0129, $0059, $01BD,
        $018D, $018A, $0036, $0141, $0144,
        $0001, $0000, $0249, $0240, $0249,
        $0000, $0104, $0105, $0113, $0115,
        $0000, $0208, $01FF, $0008);


var
  aText,silver0,silver1,golden0,golden1,golden2 : String;
  tempbuffer,
  qrcode: TQRBuffer;
  bytes,
  segbuf0,
  segbuf1,
  segbuf2 : TQRBuffer;
  seg : TQRSegment;
  segs : TQRSegmentArray;
  segs2 : TQRSegmentArray;
  len, I,j : integer;
      
begin
  SetLength(tempBuffer,QRBUFFER_LEN_MAX);
  SetLength(qrCode,QRBUFFER_LEN_MAX);
  // Illustration 'silver'
  silver0 := 'THE SQUARE ROOT OF 2 IS 1.';
  silver1 := '41421356237309504880168872420969807856967187537694807317667973799';
  
  aText:=silver0+Silver1;
  if QRencodeText(aText, tempBuffer, qrcode, EccLOW,  QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
    printQr(qrcode);

  SetLength(segBuf0,QRcalcSegmentBufferSize(mALPHANUMERIC, length(silver0)));
  SetLength(segBuf1,QRcalcSegmentBufferSize(mNUMERIC, length(silver1)));
  SetLength(Segs,2);
  segs[0]:=QRmakeAlphanumeric(silver0, segBuf0);
  segs[1]:=QRmakeNumeric(silver1, segBuf1);
  if QRencodeSegments(segs, eccLOW, tempBuffer, qrcode) then
    printQr(qrcode);

  // Illustration "golden"
  SetLength(Segbuf0,0);
  SetLength(Segbuf1,0);
  golden0 := 'Golden ratio '#$CF#$86' = 1.';
  golden1 := '6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374';
  golden2 := '......';
  atext:=Golden0+Golden1+Golden2;
  if QRencodeText(aText, tempBuffer, qrcode, EccLOW,    QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
      printQr(qrcode);

  SetLength(bytes,Length(golden0));
  for I:=1 to Length(golden0) do
     bytes[i-1]:=Ord(golden0[i]);
  SetLength(segBuf0,QRcalcSegmentBufferSize(mBYTE, length(golden0)));
  SetLength(segBuf1,QRcalcSegmentBufferSize(mNUMERIC, length(golden1)));
  SetLength(segBuf2,QRcalcSegmentBufferSize(mALPHANUMERIC, length(golden2)));
  SetLength(Segs2,3);
  segs2[0]:=QRmakeBytes(bytes, segBuf0);
  segs2[1]:=QRmakeNumeric(golden1, segBuf1);
  segs2[2]:=QRmakeAlphanumeric(golden2, segBuf2);
  SetLength(bytes,0);
  if QRencodeSegments(segs2,EccLOW, tempBuffer, qrcode) then
    PrintQR(qrCode);

  // Illustration "Madoka": kanji, kana, Greek, Cyrillic, full-width Latin characters
  SetLength(segBuf0,0);
  SetLength(segBuf1,0);
  SetLength(segBuf2,0);
  atext:= // Encoded in UTF-8
        #$E3#$80#$8C#$E9#$AD#$94#$E6#$B3#$95#$E5+
        #$B0#$91#$E5#$A5#$B3#$E3#$81#$BE#$E3#$81+
        #$A9#$E3#$81#$8B#$E2#$98#$86#$E3#$83#$9E+
        #$E3#$82#$AE#$E3#$82#$AB#$E3#$80#$8D#$E3+
        #$81#$A3#$E3#$81#$A6#$E3#$80#$81#$E3#$80+
        #$80#$D0#$98#$D0#$90#$D0#$98#$E3#$80#$80+
        #$EF#$BD#$84#$EF#$BD#$85#$EF#$BD#$93#$EF+
        #$BD#$95#$E3#$80#$80#$CE#$BA#$CE#$B1#$EF+
        #$BC#$9F;
  if  QRencodeText(aText, tempBuffer, qrcode, eccLOW, QRVERSIONMIN, QRVERSIONMAX, mpAUTO, true) then
    printQr(qrcode);
    
  len:= SizeOf(kanjiChars) div sizeof(Word);
  SetLength(segBuf0,QRcalcSegmentBufferSize(mKANJI, len));
  seg.mode := mKANJI;
  seg.numChars := len;
  seg.bitLength := 0;
  for I:=0 to Len-1 do
    for j:=12 downto 0 do
      begin
      segBuf0[seg.bitLength shr 3]:=segBuf0[seg.bitLength shr 3] or  ((kanjiChars[i] shr j) and 1) shl (7 - (seg.bitLength and 7));
      inc(seg.bitLength);
      end;
  seg.data:=segBuf0;
  SetLength(segs,1);
  segs[0]:=Seg;
  if QRencodeSegments(segs,eccLOW, tempBuffer, qrcode) then
    printQr(qrcode);
end;

begin
  doBasicDemo();
  doVarietyDemo();
  doSegmentDemo();
end.
