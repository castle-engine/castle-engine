{
    This file is part of the Free Pascal FCL library.
    Copyright (c) 2017 by Michael Van Canneyt
    member of the Free Pascal development team

    Barcode encoding routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpbarcode;

{$mode objfpc}{$H+}

interface

uses
  sysutils;

Type
  // Various encodings. Sorted
  TBarcodeEncoding = (
    be128A, be128B, be128C,
    be2of5industrial, be2of5interleaved, be2of5matrix,
    be39, be39Extended,
    be93, be93Extended,
    beCodabar,
    beEAN13, beEAN8,
    beMSI,
    bePostNet
  );
  TBarcodeEncodings = Set of TBarcodeEncoding;

  {
    Various types of known bars in a barcode.
    Each type encapsulates 3 parameters.
    Color: black/white
    width: 100, (weighted) 150 or 200 % of unit width
    Height: full height or 2/5th (the latter is for postnet)
  }
  TBarColor = (bcWhite,bcBlack);
  TBarWidth = (bw100,bwWeighted,bw150,bw200);
  TBarheight = (bhFull,bhTwoFifth);
  TBarWidthArray = Array[TBarWidth] of Integer;

  TBarParams = record
    c : TBarColor;
    w : TBarWidth;
    h : TBarHeight;
  end;

  TBarType = 0..11;
  // auxiliary type for the constant
  TBarTypeParams = Array[TBarType] of TBarParams;
  // This
  TBarTypeArray = array of TBarType;
  TBarParamsArray = Array of TBarParams;
  EBarEncoding = class(exception);

Const
  NumericalEncodings = [beEAN8,beEAN13,be2of5industrial,be2of5interleaved, be2of5matrix,bePostNet,beMSI,be128C];
  BarcodeEncodingNames: array[TBarcodeEncoding] of string =
    (
      '128 A', '128 B', '128 C',
      '2 of 5 industrial', '2 of 5 interleaved', '2 of 5 matrix',
      '39', '39 Extended',
      '93', '93 Extended',
      'Codabar',
      'EAN 13', 'EAN 8',
      'MSI',
      'PostNet'
    );

Function StringAllowsBarEncoding(S : AnsiString; aEncoding : TBarcodeEncoding) : Boolean;
Function StringToBarTypeArray(S : AnsiString; aEncoding : TBarcodeEncoding) : TBarTypeArray;
Function StringToBarcodeParams(S : AnsiString; aEncoding : TBarcodeEncoding) : TBarParamsArray;
Function IntToBarTypeArray(I: Int64; aEncoding : TBarcodeEncoding; aWidth : Integer = 0) : TBarTypeArray;
Function IntToBarcodeParams(I : Int64; aEncoding : TBarcodeEncoding; aWidth : Integer = 0) : TBarParamsArray;
Function BarTypeToBarParams(aType : TBarType) : TBarParams;
Function BarTypeArrayToBarParamsArray(anArray : TBarTypeArray) : TBarParamsArray;
Function CalcBarWidths(aEncoding : TBarcodeEncoding; aUnit : Integer; AWeight : Double) : TBarWidthArray;
Function CalcStringWidthInBarCodeEncoding(S : String;aEncoding : TBarcodeEncoding; aUnit : Integer; AWeight : Double) : Cardinal;

// Check with barcode unit

implementation

Const
  NumChars = ['0'..'9'];


Procedure IllegalChar(C : AnsiChar;E : TBarcodeEncoding);

Var
  S : AnsiString;

begin
  Str(E,S);
  Raise EBarEncoding.CreateFmt('%s is an illegal character for encoding %s',[C,S]);
end;

Const
  BarTypes : TBarTypeParams = (
  { 0}   (c: bcWhite; w: bw100;      h: bhFull),
  { 1}   (c: bcWhite; w: bwWeighted; h: bhFull),
  { 2}   (c: bcWhite; w: bw150;      h: bhFull),
  { 3}   (c: bcWhite; w: bw200;      h: bhFull),
  { 4}   (c: bcBlack; w: bw100;      h: bhFull),
  { 5}   (c: bcBlack; w: bwWeighted; h: bhFull),
  { 6}   (c: bcBlack; w: bw150;      h: bhFull),
  { 7}   (c: bcBlack; w: bw200;      h: bhFull),
  { 8}   (c: bcBlack; w: bw100;      h: bhTwoFifth),
  { 9}   (c: bcBlack; w: bwWeighted; h: bhTwoFifth),
  {10}   (c: bcBlack; w: bw150;      h: bhTwoFifth),
  {11}   (c: bcBlack; w: bw200;      h: bhTwoFifth)
  );

{ ---------------------------------------------------------------------
  EAN 8
  ---------------------------------------------------------------------}
Type
  TEANChar = array[1..4] of TBarType;
  TEanParity = array[1..6] of TBarType;

Const
  EANStartStop : array[1..3] of TBarType = (4,0,4);
  EANSep : array[1..5] of TBarType = (0,4,0,4,0);

  EANEncodingA : array['0'..'9'] of TEANChar = (
    ( 2, 5, 0, 4),   // 0
    ( 1, 5, 1, 4),   // 1
    ( 1, 4, 1, 5),   // 2
    ( 0, 7, 0, 4),   // 3
    ( 0, 4, 2, 5),   // 4
    ( 0, 5, 2, 4),   // 5
    ( 0, 4, 0, 7),   // 6
    ( 0, 6, 0, 5),   // 7
    ( 0, 5, 0, 6),   // 8
    ( 2, 4, 0, 5)    // 9
  );

  EANEncodingC : array['0'..'9'] of TEANChar = (
    ( 6, 1, 4, 0),   // 0
    ( 5, 1, 5, 0),   // 1
    ( 5, 0, 5, 1),   // 2
    ( 4, 3, 4, 0),   // 3
    ( 4, 0, 6, 1),   // 4
    ( 4, 1, 6, 0),   // 5
    ( 4, 0, 4, 3),   // 6
    ( 4, 2, 4, 1),   // 7
    ( 4, 1, 4, 2),   // 8
    ( 6, 0, 4, 1)    // 9
  );

  EANEncodingB : array['0'..'9'] of TEANChar = (
    ( 0, 4, 1, 6),   // 0
    ( 0, 5, 1, 5),   // 1
    ( 1, 5, 0, 5),   // 2
    ( 0, 4, 3, 4),   // 3
    ( 1, 6, 0, 4),   // 4
    ( 0, 6, 1, 4),   // 5
    ( 3, 4, 0, 4),   // 6
    ( 1, 4, 2, 4),   // 7
    ( 2, 4, 1, 4),   // 8
    ( 1, 4, 0, 6)    // 9
  );

  EANEncodingParity : array[0..9] of TEanParity = (
    ( 8, 8, 8, 8, 8, 8),   // 0
    ( 8, 8, 9, 8, 9, 9),   // 1
    ( 8, 8, 9, 9, 8, 9),   // 2
    ( 8, 8, 9, 9, 9, 8),   // 3
    ( 8, 9, 8, 8, 9, 9),   // 4
    ( 8, 9, 9, 8, 8, 9),   // 5
    ( 8, 9, 9, 9, 8, 8),   // 6
    ( 8, 9, 8, 9, 8, 9),   // 7
    ( 8, 9, 8, 9, 9, 8),   // 8
    ( 8, 9, 9, 8, 9, 8)    // 9
  );

Procedure AddToArray(A : TBarTypeArray; var aPos : integer; Elements : Array of TBarType);

Var
  I,L : Integer;
begin
  L:=Length(Elements);
  // Safety check
  if ((aPos+L)>Length(A)) then
    Raise EBarEncoding.CreateFmt('Cannot add %d elements to array of length %d at pos %d,',[L,Length(A),aPos]);
  For I:=0 to L-1 do
    begin
    A[aPos]:=Elements[i];
    inc(aPos);
    end;
end;

function CheckEANValue(const AValue:AnsiString; const ASize: Byte): AnsiString;

var
  L,I : Integer;

begin
  Result:=AValue;
  UniqueString(Result);
  L:=Length(Result);
  for i:=1 to L do
    if not (Result[i] in NumChars) then
      Result[i]:='0';
  if L<ASize then
    Result:=StringOfChar('0', ASize-L-1)+Result+'0';
end;

function EncodeEAN8(S : AnsiString) : TBarTypeArray;

var
  i, p: integer;

begin
  S:=CheckEANValue(S,8);
  SetLength(Result,2*Length(EANStartStop)+Length(EANSep)+8*4);
  P:=0;
  AddToArray(Result,P,EANStartStop); // start
  for I:=1 to 4 do
    AddToArray(Result,P,EANEncodingA[S[i]]);
  AddToArray(Result,P,EANSep); // Separator
  for i := 5 to 8 do
    AddToArray(Result,P,EANEncodingC[S[i]]);
  AddToArray(Result,P,EANStartStop); // Stop
end;

function EnCodeEAN13(S : AnsiString) : TBarTypeArray;

var
  i, p, cc : integer;

begin
  S:=CheckEanValue(S, 13);
  SetLength(Result,2*Length(EANStartStop)+Length(EANSep)+12*4);
  cc:=Ord(S[1])-Ord('0');
  Delete(S,1,1);
  P:=0;
  AddToArray(Result,P,EANStartStop); // start
  for i := 1 to 6 do
    case EANEncodingParity[cc,i] of
      8: AddToArray(Result,P,EANEncodingA[s[i]]);
      9: AddToArray(Result,P,EANEncodingB[s[i]]);
      10: AddToArray(Result,P,EANEncodingC[s[i]]);// will normally not happen...
    end;
  AddToArray(Result,P,EANSep); // Separator
  for i := 7 to 12 do
    AddToArray(Result,P,EANEncodingC[s[i]]);
  AddToArray(Result,P,EANStartStop); // stop
end;

{ ---------------------------------------------------------------------
  Encoding 39 (+ extended)
  ---------------------------------------------------------------------}

Type
  TCode39Char = array[0..9] of TBarType;
  TCode39Data = record
    c: AnsiChar;
    ck: byte;
    Data:  TCode39Char;
  end;

Const
  Encoding39 : array[0..43] of TCode39Data = (
    (c: '0'; ck:  0; data: ( 4, 0, 4, 1, 5, 0, 5, 0, 4, 0)),
    (c: '1'; ck:  1; data: ( 5, 0, 4, 1, 4, 0, 4, 0, 5, 0)),
    (c: '2'; ck:  2; data: ( 4, 0, 5, 1, 4, 0, 4, 0, 5, 0)),
    (c: '3'; ck:  3; data: ( 5, 0, 5, 1, 4, 0, 4, 0, 4, 0)),
    (c: '4'; ck:  4; data: ( 4, 0, 4, 1, 5, 0, 4, 0, 5, 0)),
    (c: '5'; ck:  5; data: ( 5, 0, 4, 1, 5, 0, 4, 0, 4, 0)),
    (c: '6'; ck:  6; data: ( 4, 0, 5, 1, 5, 0, 4, 0, 4, 0)),
    (c: '7'; ck:  7; data: ( 4, 0, 4, 1, 4, 0, 5, 0, 5, 0)),
    (c: '8'; ck:  8; data: ( 5, 0, 4, 1, 4, 0, 5, 0, 4, 0)),
    (c: '9'; ck:  9; data: ( 4, 0, 5, 1, 4, 0, 5, 0, 4, 0)),
    (c: 'A'; ck: 10; data: ( 5, 0, 4, 0, 4, 1, 4, 0, 5, 0)),
    (c: 'B'; ck: 11; data: ( 4, 0, 5, 0, 4, 1, 4, 0, 5, 0)),
    (c: 'C'; ck: 12; data: ( 5, 0, 5, 0, 4, 1, 4, 0, 4, 0)),
    (c: 'D'; ck: 13; data: ( 4, 0, 4, 0, 5, 1, 4, 0, 5, 0)),
    (c: 'E'; ck: 14; data: ( 5, 0, 4, 0, 5, 1, 4, 0, 4, 0)),
    (c: 'F'; ck: 15; data: ( 4, 0, 5, 0, 5, 1, 4, 0, 4, 0)),
    (c: 'G'; ck: 16; data: ( 4, 0, 4, 0, 4, 1, 5, 0, 5, 0)),
    (c: 'H'; ck: 17; data: ( 5, 0, 4, 0, 4, 1, 5, 0, 4, 0)),
    (c: 'I'; ck: 18; data: ( 4, 0, 5, 0, 4, 1, 5, 0, 0, 0)),
    (c: 'J'; ck: 19; data: ( 4, 0, 4, 0, 5, 1, 5, 0, 4, 0)),
    (c: 'K'; ck: 20; data: ( 5, 0, 4, 0, 4, 0, 4, 1, 5, 0)),
    (c: 'L'; ck: 21; data: ( 4, 0, 5, 0, 4, 0, 4, 1, 5, 0)),
    (c: 'M'; ck: 22; data: ( 5, 0, 5, 0, 4, 0, 4, 1, 4, 0)),
    (c: 'N'; ck: 23; data: ( 4, 0, 4, 0, 5, 0, 4, 1, 5, 0)),
    (c: 'O'; ck: 24; data: ( 5, 0, 4, 0, 5, 0, 4, 1, 4, 0)),
    (c: 'P'; ck: 25; data: ( 4, 0, 5, 0, 5, 0, 4, 1, 4, 0)),
    (c: 'Q'; ck: 26; data: ( 4, 0, 4, 0, 4, 0, 5, 1, 5, 0)),
    (c: 'R'; ck: 27; data: ( 5, 0, 4, 0, 4, 0, 5, 1, 4, 0)),
    (c: 'S'; ck: 28; data: ( 4, 0, 5, 0, 4, 0, 5, 1, 4, 0)),
    (c: 'T'; ck: 29; data: ( 4, 0, 4, 0, 5, 0, 5, 1, 4, 0)),
    (c: 'U'; ck: 30; data: ( 5, 1, 4, 0, 4, 0, 4, 0, 5, 0)),
    (c: 'V'; ck: 31; data: ( 4, 1, 5, 0, 4, 0, 4, 0, 5, 0)),
    (c: 'W'; ck: 32; data: ( 5, 1, 5, 0, 4, 0, 4, 0, 4, 0)),
    (c: 'X'; ck: 33; data: ( 4, 1, 4, 0, 5, 0, 4, 0, 5, 0)),
    (c: 'Y'; ck: 34; data: ( 5, 1, 4, 0, 5, 0, 4, 0, 4, 0)),
    (c: 'Z'; ck: 35; data: ( 4, 1, 5, 0, 5, 0, 4, 0, 4, 0)),
    (c: '-'; ck: 36; data: ( 4, 1, 4, 0, 4, 0, 5, 0, 5, 0)),
    (c: '.'; ck: 37; data: ( 5, 1, 4, 0, 4, 0, 5, 0, 4, 0)),
    (c: ' '; ck: 38; data: ( 4, 1, 5, 0, 4, 0, 5, 0, 4, 0)),
    (c: '*'; ck:  0; data: ( 4, 1, 4, 0, 5, 0, 5, 0, 4, 0)),
    (c: '$'; ck: 39; data: ( 4, 1, 4, 1, 4, 1, 4, 0, 4, 0)),
    (c: '/'; ck: 40; data: ( 4, 1, 4, 1, 4, 0, 4, 1, 4, 0)),
    (c: '+'; ck: 41; data: ( 4, 1, 4, 0, 4, 1, 4, 1, 4, 0)),
    (c: '%'; ck: 42; data: ( 4, 0, 4, 1, 4, 1, 4, 1, 4, 0))
  );

function IndexOfCode39Char(c: AnsiChar): integer;

begin
  Result:=High(Encoding39);
  While (Result>=0) and (c<>Encoding39[Result].c) do
    Dec(Result);
end;

Function AllowEncode39 (S : AnsiString) : boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=IndexOfCode39Char(S[i])>=0;
    Inc(I);
    end;
end;

Function Encode39(S : AnsiString; aCheckSum : Boolean) : TBarTypeArray;

Const
  StartStopIndex = 39;


  function IndexOfCC(cs: byte): integer;

  Var
    H : integer;

  begin
    Result:=0;
    H:=High(Encoding39);
    While (Result<=H) and (cs<>Encoding39[Result].ck) do
      Inc(Result);
    if Result>=H then
      Result:=StartStopIndex;
  end;

var
  cs, p, Idx: integer;
  c : AnsiChar;

begin
  cs:=0;
  // Length = (length text + startstop * 2) * (length of data)
  SetLength(Result,(Length(S)+2)*10);
  P:=0;
  // Startcode
  AddToArray(Result,P,Encoding39[StartStopIndex].Data);
  for C in S do
    begin
    Idx:=IndexOfCode39Char(C);
    if Idx<0 then
      IllegalChar(C,be39);
    AddToArray(Result,P,Encoding39[Idx].Data);
    Inc(cs, Encoding39[Idx].ck);
    end;
  // Calculate Checksum if requested and add.
  if aCheckSum then
    begin
    AddToArray(Result,P,Encoding39[IndexOfCc(cs mod 43)].Data);
    SetLength(Result,P); // Correct result
    end
  else // No checksum: add startcode, minus last 0 !
    begin
    AddToArray(Result,P,Encoding39[StartStopIndex].Data);
    SetLength(Result,P-1); // Correct result
    end;
end;

function AllowEncode39Extended(S : AnsiString) : boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=Ord(S[i])<128;
    Inc(I);
    end;
end;

function Encode39Extended(S : AnsiString; aCheckSum : boolean): TBarTypeArray;

// Extended uses an encoding for the first 127 characters...

const
  CharEncoding : array[0..127] of String[2] = (
    '%U', '$A', '$B', '$C', '$D', '$E', '$F', '$G',
    '$H', '$I', '$J', '$K', '$L', '$M', '$N', '$O',
    '$P', '$Q', '$R', '$S', '$T', '$U', '$V', '$W',
    '$X', '$Y', '$Z', '%A', '%B', '%C', '%D', '%E',
    ' ',  '/A', '/B', '/C', '/D', '/E', '/F', '/G',
    '/H', '/I', '/J', '/K', '/L', '/M', '/N', '/O',
    '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
    '8',  '9',  '/Z', '%F', '%G', '%H', '%I', '%J',
    '%V', 'A',  'B',  'C',  'D',  'E',  'F',  'G',
    'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
    'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
    'X',  'Y',  'Z',  '%K', '%L', '%M', '%N', '%O',
    '%W', '+A', '+B', '+C', '+D', '+E', '+F', '+G',
    '+H', '+I', '+J', '+K', '+L', '+M', '+N', '+O',
    '+P', '+Q', '+R', '+S', '+T', '+U', '+V', '+W',
    '+X', '+Y', '+Z', '%P', '%Q', '%R', '%S', '%T'
  );

var
  T : AnsiString;
  O,i: integer;

begin
  T:='';
  for I:=1 to Length(S) do
    begin
    O:=Ord(S[i]);
    if (O>127) then
      IllegalChar(S[i],be39Extended);
    T:=T+CharEncoding[O];
    end;
  Result:=Encode39(T,aChecksum);
end;

{ ---------------------------------------------------------------------
  Code 93
  ---------------------------------------------------------------------}
Type
  TCode93Char = array[0..5] of TBarType;
  TCode93Data = record
    c: AnsiChar;
    Data:  TCode93Char;
  end;

Const
  Encoding93 : array[0..46] of TCode93Data = (
    (c: '0'; data: ( 4, 2, 4, 0, 4, 1)),
    (c: '1'; data: ( 4, 0, 4, 1, 4, 2)),
    (c: '2'; data: ( 4, 0, 4, 2, 4, 1)),
    (c: '3'; data: ( 4, 0, 4, 3, 4, 0)),
    (c: '4'; data: ( 4, 1, 4, 0, 4, 2)),
    (c: '5'; data: ( 4, 1, 4, 1, 4, 1)),
    (c: '6'; data: ( 4, 1, 4, 2, 4, 0)),
    (c: '7'; data: ( 4, 0, 4, 0, 4, 3)),
    (c: '8'; data: ( 4, 2, 4, 1, 4, 0)),
    (c: '9'; data: ( 4, 3, 4, 0, 4, 0)),
    (c: 'A'; data: ( 5, 0, 4, 0, 4, 2)),
    (c: 'B'; data: ( 5, 0, 4, 1, 4, 1)),
    (c: 'C'; data: ( 5, 0, 4, 2, 4, 0)),
    (c: 'D'; data: ( 5, 1, 4, 0, 4, 1)),
    (c: 'E'; data: ( 5, 1, 4, 1, 4, 0)),
    (c: 'F'; data: ( 5, 2, 4, 0, 4, 0)),
    (c: 'G'; data: ( 4, 0, 5, 0, 4, 2)),
    (c: 'H'; data: ( 4, 0, 5, 1, 4, 1)),
    (c: 'I'; data: ( 4, 0, 5, 2, 4, 0)),
    (c: 'J'; data: ( 4, 1, 5, 0, 4, 1)),
    (c: 'K'; data: ( 4, 2, 5, 0, 4, 0)),
    (c: 'L'; data: ( 4, 0, 4, 0, 5, 2)),
    (c: 'M'; data: ( 4, 0, 4, 1, 5, 1)),
    (c: 'N'; data: ( 4, 0, 4, 2, 5, 0)),
    (c: 'O'; data: ( 4, 1, 4, 0, 5, 1)),
    (c: 'P'; data: ( 4, 2, 4, 0, 5, 0)),
    (c: 'Q'; data: ( 5, 0, 5, 0, 4, 1)),
    (c: 'R'; data: ( 5, 0, 5, 1, 4, 0)),
    (c: 'S'; data: ( 5, 0, 4, 0, 5, 1)),
    (c: 'T'; data: ( 5, 0, 4, 1, 5, 0)),
    (c: 'U'; data: ( 5, 1, 4, 0, 5, 0)),
    (c: 'V'; data: ( 5, 1, 5, 0, 4, 0)),
    (c: 'W'; data: ( 4, 0, 5, 0, 5, 1)),
    (c: 'X'; data: ( 4, 0, 5, 1, 5, 0)),
    (c: 'Y'; data: ( 4, 1, 5, 0, 5, 0)),
    (c: 'Z'; data: ( 4, 1, 6, 0, 4, 0)),
    (c: '-'; data: ( 4, 1, 4, 0, 6, 0)),
    (c: '.'; data: ( 6, 0, 4, 0, 4, 1)),
    (c: ' '; data: ( 6, 0, 4, 1, 4, 0)),
    (c: '$'; data: ( 6, 1, 4, 0, 4, 0)),
    (c: '/'; data: ( 4, 0, 5, 0, 6, 0)),
    (c: '+'; data: ( 4, 0, 6, 0, 5, 0)),
    (c: '%'; data: ( 5, 0, 4, 0, 6, 0)),
    (c: '['; data: ( 4, 1, 4, 1, 5, 0)),
    (c: ']'; data: ( 6, 0, 5, 0, 4, 0)),
    (c: '{'; data: ( 6, 0, 4, 0, 5, 0)),
    (c: '}'; data: ( 4, 1, 5, 1, 4, 0))
  );

function IndexOfCode93Char(c: AnsiChar): integer;

begin
  Result:=High(Encoding93);
  While (Result>=0) and (c<>Encoding93[Result].c) do
    Dec(Result);
end;

Function AllowEncode93 (S : AnsiString) : boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=IndexOfCode93Char(S[i])>=0;
    Inc(I);
    end;
end;

Function Encode93(S : AnsiString) : TBarTypeArray;

Const
  Code93Start : Array[1..6] of TBarType =  ( 4, 0, 4, 0, 7, 0);
  Code93Stop : Array[1..7] of TBarType = ( 4, 0, 4, 0, 7, 0, 4);

var
  L,i, P, Idx, CC, CK, WC, WK  : integer;
  C : Char;

begin
  L:=Length(S);
  // Length String * 6 + Start + Stop + Checksum
  SetLength(Result,L*6+6+7+2*6);
  P:=0;
  AddToArray(Result,P,Code93Start);
  for C in S do
    begin
    Idx:=IndexOfCode93Char(C);
    if Idx<0 then
      IllegalChar(C,be93);
    AddToArray(Result,P,Encoding93[Idx].Data);
    end;
  CC:=0;
  CK:=0;
  WC:=1;
  WK:=2;
  for i:=L downto 1 do
    begin
    Idx:=IndexOfCode93Char(S[i]);
    Inc(CC,Idx*WC);
    Inc(CK,Idx*WK);
    Inc(WC);
    if (WC>20) then
      WC:=1;
    Inc(WK);
    if (WK>15) then
      WK:=1;
    end;
  Inc(CK,CC);
  CC:=CC mod 47;
  CK:=CK mod 47;
  AddToArray(Result,P,Encoding93[CC].Data);
  AddToArray(Result,P,Encoding93[CK].Data);
  AddToArray(Result,P,Code93Stop);
end;

function AllowEncode93Extended(S : AnsiString) : boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=Ord(S[i])<128;
    Inc(I);
    end;
end;


function Encode93Extended(S: string) : TBarTypeArray;

const
  CharEncoding: array[0..127] of string[2] = (
    ']U', '[A', '[B', '[C', '[D', '[E', '[F', '[G',
    '[H', '[I', '[J', '[K', '[L', '[M', '[N', '[O',
    '[P', '[Q', '[R', '[S', '[T', '[U', '[V', '[W',
    '[X', '[Y', '[Z', ']A', ']B', ']C', ']D', ']E',
    ' ',  '{A', '{B', '{C', '{D', '{E', '{F', '{G',
    '{H', '{I', '{J', '{K', '{L', '{M', '{N', '{O',
    '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
    '8',  '9',  '{Z', ']F', ']G', ']H', ']I', ']J',
    ']V', 'A',  'B',  'C',  'D',  'E',  'F',  'G',
    'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
    'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
    'X',  'Y',  'Z',  ']K', ']L', ']M', ']N', ']O',
    ']W', '}A', '}B', '}C', '}D', '}E', '}F', '}G',
    '}H', '}I', '}J', '}K', '}L', '}M', '}N', '}O',
    '}P', '}Q', '}R', '}S', '}T', '}U', '}V', '}W',
    '}X', '}Y', '}Z', ']P', ']Q', ']R', ']S', ']T'
  );

var
  T : AnsiString;
  O,i: integer;

begin
  T:='';
  for I:=1 to Length(S) do
    begin
    O:=Ord(S[i]);
    if (O>127) then
      IllegalChar(S[i],be93Extended);
    T:=T+CharEncoding[O];
    end;
  Result:=Encode93(T);
end;

{ ---------------------------------------------------------------------
  MSI
  ---------------------------------------------------------------------}

Type
  TMSIChar = Array[1..8] of TBarType;

Const
  EncodingMSI : array['0'..'9'] of TMSIChar = (
    ( 4, 1, 4, 1, 4, 1, 4, 1),   // 0
    ( 4, 1, 4, 1, 4, 1, 5, 0),   // 1
    ( 4, 1, 4, 1, 5, 0, 4, 1),   // 2
    ( 4, 1, 4, 1, 5, 0, 5, 0),   // 3
    ( 4, 1, 5, 0, 4, 1, 4, 1),   // 4
    ( 4, 1, 5, 0, 4, 1, 5, 0),   // 5
    ( 4, 1, 5, 0, 5, 0, 4, 1),   // 6
    ( 4, 1, 5, 0, 5, 0, 5, 0),   // 7
    ( 5, 0, 4, 1, 4, 1, 4, 1),   // 8
    ( 5, 0, 4, 1, 4, 1, 5, 0)    // 9
  );

function EncodeMSI(S : AnsiString) : TBarTypeArray;

  function SumDigits(D: integer): integer;

  begin
    Result:=0;
    while (D>0) do
      begin
      Result:=Result+(D mod 10);
      D:=D div 10;
      end;
  end;


Const
  MSIPrefix : Array [1..2] of TBarType  = (5,0);
  MSISuffix : Array [1..3] of TBarType  = (4,1,4);

var
  P,I,CSE,CSO,CS : integer;
  C : AnsiChar;

begin
  // Length(Prefix)+Length(Suffix)+Length(S)+CheckSum
  SetLength(Result,(Length(S)+1)*8+2+3);
  P:=0;
  AddToArray(Result,P,MSIPrefix); // Prefix
  CSE:=0;
  CSO:=0;
  for i:=1 to Length(s) do
    begin
    C:=S[i];
    if Not (C in NumChars) then
      IllegalChar(S[i],beMSI);
    if odd(i-1) then
      CSO:=CSO*10+Ord(C)
    else
      CSE:=CSE+Ord(c);
    AddToArray(Result,P,EncodingMSI[C]);
    end;
  // Add checksum
  CS:=(SumDigits(CSO*2) + CSE) mod 10;
  if CS>0 then
    CS:=10-CS;
  AddToArray(Result,P,EncodingMSI[chr(Ord('0')+CS)]);
  AddToArray(Result,P,MSISuffix); // Suffix
end;

{ ---------------------------------------------------------------------
  CodaBar
  ---------------------------------------------------------------------}

Type
  TCodabarChar = array[0..6] of TBarType;
  TCodabarCharZero = array[0..7] of TBarType;

  TCodaBarData = record
    c: AnsiChar;
    Data: TCodabarChar;
  end;

Var
  EncodingCodaBar : array[0..19] of TCodaBarData = (
    (c: '1'; data: ( 4, 0, 4, 0, 5, 1, 4)),
    (c: '2'; data: ( 4, 0, 4, 1, 4, 0, 5)),
    (c: '3'; data: ( 5, 1, 4, 0, 4, 0, 4)),
    (c: '4'; data: ( 4, 0, 5, 0, 4, 1, 4)),
    (c: '5'; data: ( 5, 0, 4, 0, 4, 1, 4)),
    (c: '6'; data: ( 4, 1, 4, 0, 4, 0, 5)),
    (c: '7'; data: ( 4, 1, 4, 0, 5, 0, 4)),
    (c: '8'; data: ( 4, 1, 5, 0, 4, 0, 4)),
    (c: '9'; data: ( 5, 0, 4, 1, 4, 0, 4)),
    (c: '0'; data: ( 4, 0, 4, 0, 4, 1, 5)),
    (c: '-'; data: ( 4, 0, 4, 1, 5, 0, 4)),
    (c: '$'; data: ( 4, 0, 5, 1, 4, 0, 4)),
    (c: ':'; data: ( 5, 0, 4, 0, 5, 0, 5)),
    (c: '/'; data: ( 5, 0, 5, 0, 4, 0, 5)),
    (c: '.'; data: ( 5, 0, 5, 0, 5, 0, 4)),
    (c: '+'; data: ( 4, 0, 5, 0, 5, 0, 5)),
    (c: 'A'; data: ( 4, 0, 5, 1, 4, 1, 4)),
    (c: 'B'; data: ( 4, 1, 4, 1, 4, 0, 5)),
    (c: 'C'; data: ( 4, 0, 4, 1, 4, 1, 5)),
    (c: 'D'; data: ( 4, 0, 4, 1, 5, 1, 4))
  );


function IndexOfCodaChar(c: AnsiChar): integer;

begin
  Result:=High(EncodingCodaBar);
  While (Result>=0) and (c<>EncodingCodaBar[Result].c) do
    Dec(Result);
end;

Function AllowEncodeCodaBar (S : AnsiString) : boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=IndexOfCodaChar(S[i])>=0;
    Inc(I);
    end;
end;


Function EncodeCodaBar(S : AnsiString) : TBarTypeArray;

  Function AddZero(C :TCodaBarChar) : TCodabarCharZero;

  begin
    Move(C,result,SizeOf(C));
    Result[7]:=0;
  end;

var
  i, P, Idx: integer;

begin
  // (Length(S)+1)*8+7
  Setlength(Result,(Length(S)+1)*8+7);
  P:=0;
  AddToArray(Result,P,AddZero(EncodingCodaBar[IndexOfCodaChar('A')].Data));
  for i:=1 to Length(S) do
    begin
    Idx:=IndexOfCodaChar(S[i]);
    if Idx<0 then
      IllegalChar(S[i],beCodabar);
    AddToArray(Result,P,AddZero(EncodingCodaBar[Idx].Data));
    end;
  AddToArray(Result,P,EncodingCodaBar[IndexOfCodaChar('B')].Data);
end;

{ ---------------------------------------------------------------------
  Postnet
  ---------------------------------------------------------------------}
Type
  TPostNetChar = Packed Array[1..10] of TBarType;

Const
  EncodingPostNet : Packed array['0'..'9'] of TPostNetChar = (
    ( 4, 1, 4, 1, 8, 1, 8, 1, 8, 1),   // 0
    ( 8, 1, 8, 1, 8, 1, 4, 1, 4, 1),   // 1
    ( 8, 1, 8, 1, 4, 1, 8, 1, 4, 1),   // 2
    ( 8, 1, 8, 1, 4, 1, 4, 1, 8, 1),   // 3
    ( 8, 1, 4, 1, 8, 1, 8, 1, 4, 1),   // 4
    ( 8, 1, 4, 1, 8, 1, 4, 1, 8, 1),   // 5
    ( 8, 1, 4, 1, 4, 1, 8, 1, 8, 1),   // 6
    ( 4, 1, 8, 1, 8, 1, 8, 1, 4, 1),   // 7
    ( 4, 1, 8, 1, 8, 1, 4, 1, 8, 1),   // 8
    ( 4, 1, 8, 1, 4, 1, 8, 1, 8, 1)    // 9
  );


Function EncodePostNet (S : AnsiString) : TBarTypeArray;

var
  i,P : integer;

begin
  SetLength(Result,Length(S)*10+2+1);
  P:=0;
  AddToArray(Result,P,[4,1]);
  for i := 1 to Length(S) do
    begin
    if Not (S[I] in NumChars) then
      IllegalChar(S[i],bePostNet);
    AddToArray(Result,P,EncodingPostNet[S[i]]);
    end;
  AddToArray(Result,P,[4]);
end;

{ ---------------------------------------------------------------------
  Code 128
  ---------------------------------------------------------------------}

Type
  TCode128Char = Packed Array[1..6] of TBarType;
  TCode128StopChar = Packed Array[1..7] of TBarType;

Const

  // The order of these elements must be the same as for
  // the Encoding128A,Encoding128B,Encoding128C arrays below !

  Encoding128Data : Packed array[0..102] of TCode128Char = (
    ( 5, 0, 5, 1, 5, 1),   // 0
    ( 5, 1, 5, 0, 5, 1),   // 1
    ( 5, 1, 5, 1, 5, 0),   // 2
    ( 4, 1, 4, 1, 5, 2),   // 3
    ( 4, 1, 4, 2, 5, 1),   // 4
    ( 4, 2, 4, 1, 5, 1),   // 5
    ( 4, 1, 5, 1, 4, 2),   // 6
    ( 4, 1, 5, 2, 4, 1),   // 7
    ( 4, 2, 5, 1, 4, 1),   // 8
    ( 5, 1, 4, 1, 4, 2),   // 9
    ( 5, 1, 4, 2, 4, 1),   // 10
    ( 5, 2, 4, 1, 4, 1),   // 11
    ( 4, 0, 5, 1, 6, 1),   // 12
    ( 4, 1, 5, 0, 6, 1),   // 13
    ( 4, 1, 5, 1, 6, 0),   // 14
    ( 4, 0, 6, 1, 5, 1),   // 15
    ( 4, 1, 6, 0, 5, 1),   // 16
    ( 4, 1, 6, 1, 5, 0),   // 17
    ( 5, 1, 6, 1, 4, 0),   // 18
    ( 5, 1, 4, 0, 6, 1),   // 19
    ( 5, 1, 4, 1, 6, 0),   // 20
    ( 5, 0, 6, 1, 4, 1),   // 21
    ( 5, 1, 6, 0, 4, 1),   // 22
    ( 6, 0, 5, 0, 6, 0),   // 23
    ( 6, 0, 4, 1, 5, 1),   // 24
    ( 6, 1, 4, 0, 5, 1),   // 25
    ( 6, 1, 4, 1, 5, 0),   // 26
    ( 6, 0, 5, 1, 4, 1),   // 27
    ( 6, 1, 5, 0, 4, 1),   // 28
    ( 6, 1, 5, 1, 4, 0),   // 29
    ( 5, 0, 5, 0, 5, 2),   // 30
    ( 5, 0, 5, 2, 5, 0),   // 31
    ( 5, 2, 5, 0, 5, 0),   // 32
    ( 4, 0, 4, 2, 5, 2),   // 33
    ( 4, 2, 4, 0, 5, 2),   // 34
    ( 4, 2, 4, 2, 5, 0),   // 35
    ( 4, 0, 5, 2, 4, 2),   // 36
    ( 4, 2, 5, 0, 4, 2),   // 37
    ( 4, 2, 5, 2, 4, 0),   // 38
    ( 5, 0, 4, 2, 4, 2),   // 39
    ( 5, 2, 4, 0, 4, 2),   // 40
    ( 5, 2, 4, 2, 4, 0),   // 41
    ( 4, 0, 5, 0, 6, 2),   // 42
    ( 4, 0, 5, 2, 6, 0),   // 43
    ( 4, 2, 5, 0, 6, 0),   // 44
    ( 4, 0, 6, 0, 5, 2),   // 45
    ( 4, 0, 6, 2, 5, 0),   // 46
    ( 4, 2, 6, 0, 5, 0),   // 47
    ( 6, 0, 6, 0, 5, 0),   // 48
    ( 5, 0, 4, 2, 6, 0),   // 49
    ( 5, 2, 4, 0, 6, 0),   // 50
    ( 5, 0, 6, 0, 4, 2),   // 51
    ( 5, 0, 6, 2, 4, 0),   // 52
    ( 5, 0, 6, 0, 6, 0),   // 53
    ( 6, 0, 4, 0, 5, 2),   // 54
    ( 6, 0, 4, 2, 5, 0),   // 55
    ( 6, 2, 4, 0, 5, 0),   // 56
    ( 6, 0, 5, 0, 4, 2),   // 57
    ( 6, 0, 5, 2, 4, 0),   // 58
    ( 6, 2, 5, 0, 4, 0),   // 59
    ( 6, 0, 7, 0, 4, 0),   // 60
    ( 5, 1, 4, 3, 4, 0),   // 61
    ( 7, 2, 4, 0, 4, 0),   // 62
    ( 4, 0, 4, 1, 5, 3),   // 63
    ( 4, 0, 4, 3, 5, 1),   // 64
    ( 4, 1, 4, 0, 5, 3),   // 65
    ( 4, 1, 4, 3, 5, 0),   // 66
    ( 4, 3, 4, 0, 5, 1),   // 67
    ( 4, 3, 4, 1, 5, 0),   // 68
    ( 4, 0, 5, 1, 4, 3),   // 69
    ( 4, 0, 5, 3, 4, 1),   // 70
    ( 4, 1, 5, 0, 4, 3),   // 71
    ( 4, 1, 5, 3, 4, 0),   // 72
    ( 4, 3, 5, 0, 4, 1),   // 73
    ( 4, 3, 5, 1, 4, 0),   // 74
    ( 5, 3, 4, 1, 4, 0),   // 75
    ( 5, 1, 4, 0, 4, 3),   // 76
    ( 7, 0, 6, 0, 4, 0),   // 77
    ( 5, 3, 4, 0, 4, 1),   // 78
    ( 4, 2, 7, 0, 4, 0),   // 79
    ( 4, 0, 4, 1, 7, 1),   // 80
    ( 4, 1, 4, 0, 7, 1),   // 81
    ( 4, 1, 4, 1, 7, 0),   // 82
    ( 4, 0, 7, 1, 4, 1),   // 83
    ( 4, 1, 7, 0, 4, 1),   // 84
    ( 4, 1, 7, 1, 4, 0),   // 85
    ( 7, 0, 4, 1, 4, 1),   // 86
    ( 7, 1, 4, 0, 4, 1),   // 87
    ( 7, 1, 4, 1, 4, 0),   // 88
    ( 5, 0, 5, 0, 7, 0),   // 89
    ( 5, 0, 7, 0, 5, 0),   // 90
    ( 7, 0, 5, 0, 5, 0),   // 91
    ( 4, 0, 4, 0, 7, 2),   // 92
    ( 4, 0, 4, 2, 7, 0),   // 93
    ( 4, 2, 4, 0, 7, 0),   // 94
    ( 4, 0, 7, 0, 4, 2),   // 95
    ( 4, 0, 7, 2, 4, 0),   // 96
    ( 7, 0, 4, 0, 4, 2),   // 97
    ( 7, 0, 4, 2, 4, 0),   // 98
    ( 4, 0, 6, 0, 7, 0),   // 99
    ( 4, 0, 7, 0, 6, 0),   // 100
    ( 6, 0, 4, 0, 7, 0),   // 101
    ( 7, 0, 4, 0, 6, 0)    // 102
  );


Const
  Encoding128ACount        = 64;
  Encoding128AChecksumInit = 103;

  Encoding128BCount        = 95;
  Encoding128BChecksumInit = 104;

  Encoding128CChecksumInit = 105;

Type
  /// 0 based, checksum relies on 0-based index
  TEncoding128AArray = Packed Array[0..Encoding128ACount-1] of Ansichar;
  TEncoding128BArray = Packed Array[0..Encoding128BCount-1] of Ansichar;

Const
   StartEncoding128A : TCode128Char = ( 5, 0, 4, 3, 4, 1);
   StartEncoding128B : TCode128Char = ( 5, 0, 4, 1, 4, 3);
   StartEncoding128C : TCode128Char = ( 5, 0, 4, 1, 6, 1);
   StopEncoding128   : TCode128StopChar = ( 5, 2, 6, 0, 4, 0, 5);

  // The order of these elements must be the same as on Encoding128Data

  Encoding128A : TEncoding128AArray = (
    ' ','!','"','#','$','%','&','''','(',')',
    '*','+',',','-','.','/','0','1','2','3',
    '4','5','6','7','8','9',':',';','<','=',
    '>','?','@','A','B','C','D','E','F','G',
    'H','I','J','K','L','M','N','O','P','Q',
    'R','S','T','U','V','W','X','Y','Z','[',
    '\',']','^','_'
  );

  Encoding128B : TEncoding128BArray = (
    ' ','!','"','#','$','%','&','''','(',')',
    '*','+',',','-','.','/','0','1','2','3',
    '4','5','6','7','8','9',':',';','<','=',
    '>','?','@','A','B','C','D','E','F','G',
    'H','I','J','K','L','M','N','O','P','Q',
    'R','S','T','U','V','W','X','Y','Z','[',
    '\',']','^','_','`','a','b','c','d','e',
    'f','g','h','i','j','k','l','m','n','o',
    'p','q','r','s','t','u','v','w','x','y',
    'z','{','|','}','~'
  );

function IndexOf128AChar(c: AnsiChar): integer;

begin
  Result:=0;
  While (Result<Encoding128ACount) and (c<>Encoding128A[Result]) do
    Inc(Result);
  if Result>=Encoding128ACount then
    Result:=-1;
end;

Function AllowEncode128A(S : String) : Boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=IndexOf128AChar(S[i])>=0;
    Inc(I);
    end;
end;

Function Encode128A(S : AnsiString) : TBarTypeArray;

Var
  CS,I,P,Idx : integer;

begin
  // Length(S)+StartCode+CheckSum+StopCode (stopcode has 7 bars)
  SetLength(Result,(Length(S)+2)*6+7);
  P:=0;
  AddToArray(Result,P,StartEncoding128A);
  CS:=Encoding128AChecksumInit;
  For I:=1 to Length(S) do
    begin
    Idx:=IndexOf128AChar(S[i]);
    if Idx<0 then
      IllegalChar(S[i],be128a);
    AddToArray(Result,P,Encoding128Data[Idx]);
    Inc(CS,Idx*I);
    end;
  // Cap CS
  CS:=CS mod 103;
  AddToArray(Result,P,Encoding128Data[CS]);
  AddToArray(Result,P,StopEncoding128);
end;

function IndexOf128BChar(c: AnsiChar): integer;

begin
  Result:=1;
  While (Result<=Encoding128BCount) and (c<>Encoding128B[Result]) do
    Inc(Result);
  if Result>Encoding128BCount then
    Result:=-1;
end;

Function AllowEncode128B(S : String) : Boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=IndexOf128BChar(S[i])>=0;
    Inc(I);
    end;
end;

Function Encode128B(S : AnsiString) : TBarTypeArray;


Var
  CS,I,P,Idx : integer;

begin
  // Length(S)+StartCode+CheckSum+StopCode (stopcode has 7 bars)
  SetLength(Result,(Length(S)+2)*6+7);
  P:=0;
  AddToArray(Result,P,StartEncoding128B);
  CS:=Encoding128BChecksumInit;
  For I:=1 to Length(S) do
    begin
    Idx:=IndexOf128BChar(S[i]);
    if Idx<0 then
      IllegalChar(S[i],be128b);
    AddToArray(Result,P,Encoding128Data[Idx]);
    Inc(CS,Idx*I);
    end;
  // Cap CS
  CS:=CS mod 103;
  AddToArray(Result,P,Encoding128Data[CS]);
  AddToArray(Result,P,StopEncoding128);
end;

Function C(S : AnsiString) : TBarTypeArray;

  function IndexOfChar(c: AnsiChar): integer;

  begin
    Result:=1;
    While (Result<=Encoding128BCount) and (c<>Encoding128A[Result]) do
      Inc(Result);
    if Result>Encoding128BCount then
      Result:=-1;
  end;

Var
  CS,I,P,Idx : integer;

begin
  // Length(S)+StartCode+CheckSum+StopCode (stopcode has 7 bars)
  SetLength(Result,(Length(S)+2)*6+7);
  P:=0;
  AddToArray(Result,P,StartEncoding128B);
  CS:=Encoding128BChecksumInit;
  For I:=1 to Length(S) do
    begin
    Idx:=IndexOfChar(S[i]);
    if Idx<0 then
      IllegalChar(S[i],be128b);
    AddToArray(Result,P,Encoding128Data[Idx]);
    Inc(CS,Idx*I);
    end;
  // Cap CS
  CS:=CS mod 103;
  AddToArray(Result,P,Encoding128Data[CS]);
  AddToArray(Result,P,StopEncoding128);
end;

Function Encode128C(S : AnsiString) : TBarTypeArray;

Var
  CS,I,CC,P,Idx : integer;
  T : AnsiString;

begin
  // Length(S)+StartCode+CheckSum+StopCode (stopcode has 7 bars)
  if Odd(Length(S)) then
    S:='0'+S;
  I:=1;
  T:='';
  // construct a AnsiString with codes.
  while i<Length(S) do
    begin
    CC:=StrToIntDef(Copy(S,i,2),-1);
    if CC=-1 then
      IllegalChar(S[i],be128C);
    T:=T+Chr(CC);
    Inc(I,2);
    end;
  // With the new AnsiString, construct barcode
  SetLength(Result,(Length(T)+2)*6+7);
  P:=0;
  AddToArray(Result,P,StartEncoding128C);
  CS:=Encoding128CChecksumInit;
  For I:=1 to Length(T) do
    begin
    Idx:=Ord(T[i]);
    AddToArray(Result,P,Encoding128Data[Idx]);
    Inc(CS,Idx*I);
    end;
  // Cap CS
  CS:=CS mod 103;
  AddToArray(Result,P,Encoding128Data[CS]);
  AddToArray(Result,P,StopEncoding128);
end;

{ ---------------------------------------------------------------------
  Barcode 2 of 5
  ---------------------------------------------------------------------}
Type
  TCode2of5Char = Packed array [1..5] of boolean;

Const
  Encoding2of5 : array['0'..'9'] of TCode2of5Char = (
    (false, false, True, True, false),    // 0
    (True, false, false, false, True),    // 1
    (false, True, false, false, True),    // 2
    (True, True, false, false, false),    // 3
    (false, false, True, false, True),    // 4
    (True, false, True, false, false),    // 5
    (false, True, True, false, false),    // 6
    (false, false, false, True, True),    // 7
    (True, false, false, True, false),    // 8
    (false, True, false, True, false)     // 9
  );

Function Encode2of5Interleaved(S : AnsiString) : TBarTypeArray;

Const
  Encode2of5Start : Array [1..4] of TBarType = (4,0,4,0);
  Encode2of5Stop : Array [1..3] of TBarType = (5,0,4);

  COdd : Array [Boolean] of TBarType = (4,5);
  CEven : Array [Boolean] of TBarType = (0,1);

var
  P, i, j: integer;
  CC : Array[1..2] of TBarType;

begin
  SetLength(Result,(Length(S)*5)+4+3);
  P:=0;
  AddToArray(Result,P,Encode2of5Start);
  for i := 1 to Length(S) div 2 do
    for j:=1 to 5 do
      begin
      if not (S[i*2-1] in NumChars) then
        IllegalChar(S[i*2-1],be2of5interleaved);
      if not (S[i*2] in NumChars) then
        IllegalChar(S[i*2],be2of5interleaved);
      CC[1]:=COdd[Encoding2of5[S[i*2-1],j]];
      CC[2]:=CEven[Encoding2of5[S[i*2],j]];
      AddToArray(Result,P,CC);
      end;
  AddToArray(Result,P,Encode2of5Stop);
end;

Function Encode2of5Industrial(S : AnsiString) : TBarTypeArray;

Const
  Encode2of5Start : Array [1..6] of TBarType = (5,0,5,0,4,0);
  Encode2of5Stop : Array [1..6] of TBarType = (5,0,4,0,5,0);

  Codes : Array [Boolean] of Array[1..2] of TBarType = ((4,0),(5,0));

var
  P,I,J : integer;
  C : Char;
begin
  // Length of AnsiString * 2 + StartCode+StopCode
  SetLength(Result,Length(S)*10+6+6);
  P:=0;
  AddToArray(Result,P,Encode2of5Start);
  for i := 1 to Length(S) do
    for j := 1 to 5 do
      begin
      C:=S[i];
      if not (C in NumChars) then
        IllegalChar(C,be2of5industrial);
      AddToArray(Result,P,Codes[Encoding2of5[S[i],j]]);
      end;
  AddToArray(Result,P,Encode2of5Stop);
end;

Function Encode2of5Matrix(S : AnsiString) : TBarTypeArray;

Const
  Encode2of5Start : Array [1..6] of TBarType = (6,0,4,0,4,0);
  Encode2of5Stop : Array [1..5] of TBarType = (6,0,4,0,4);

var
  P,I,J : integer;
  C : Char;
  BT : TBarType;
begin
  // Length of AnsiString  + StartCode+StopCode
  SetLength(Result,Length(S)*6+6+5);
  P:=0;
  AddToArray(Result,P,Encode2of5Start);
  for i:=1 to Length(S) do
    begin
    for j:=1 to 5 do
      begin
      C:=S[i];
      if not (C in NumChars) then
        IllegalChar(C,be2of5industrial);
      BT:=Ord(Encoding2of5[S[i],j]); // 0 or 1
      if odd(J) then
        BT:=BT+4;
      AddToArray(Result,P,[BT]);
      end;
    AddToArray(Result,P,[0]);
    end;
  AddToArray(Result,P,Encode2of5Stop);
end;

{ ---------------------------------------------------------------------
  Global routines
  ---------------------------------------------------------------------}

Function AllNumerical (S : AnsiString) : boolean;

Var
  I,L : integer;

begin
  L:=Length(S);
  Result:=L>0;
  I:=1;
  While Result and (I<=L) do
    begin
    Result:=S[i] in Numchars;
    Inc(I);
    end;
end;

Function StringAllowsBarEncoding(S : AnsiString; aEncoding : TBarcodeEncoding) : Boolean;

begin
  if (AEncoding in NumericalEncodings) then
    Result:=AllNumerical(S)
  else
    Case aEncoding of
      be128A : Result:=AllowEncode128A(S);
      be128B : Result:=AllowEncode128B(S);
      be39: Result:=AllowEncode39(S);
      be39Extended: Result:=AllowEncode39Extended(S);
      be93: Result:=AllowEncode93(S);
      be93Extended: Result:=AllowEncode93Extended(S);
      beCodabar: Result:=AllowEncodeCodaBar(S);
    else
      Raise EBarEncoding.CreateFmt('Unknown/Unhandled encoding, ordinal value : %d',[ord(aEncoding)]);
    end;
end;


Function StringToBarTypeArray(S : AnsiString; aEncoding : TBarcodeEncoding) : TBarTypeArray;

begin
  SetLength(Result,0);
  Case aEncoding of
    beEAN8 : Result:=EncodeEan8(S);
    beEAN13 : Result:=EncodeEan13(S);
    be128A : Result:=Encode128A(S);
    be128B : Result:=Encode128B(S);
    be128C: Result:=Encode128C(S);
    be2of5industrial: Result:=Encode2of5Industrial(S);
    be2of5interleaved: Result:=Encode2of5Interleaved(S);
    be2of5matrix: Result:=Encode2of5Matrix(S);
    be39: Result:=Encode39(S,False);
    be39Extended: Result:=Encode39Extended(S,False);
    be93: Result:=Encode93(S);
    be93Extended: Result:=Encode93Extended(S);
    beCodabar: Result:=EncodeCodaBar(S);
    beMSI: Result:=EncodeMSI(S);
    bePostNet : Result:=EncodePostNet(S);
  else
    Raise EBarEncoding.CreateFmt('Unknown/Unhandled encoding, ordinal value : %d',[ord(aEncoding)]);
  end;
end;

Function StringToBarcodeParams(S : AnsiString; aEncoding : TBarcodeEncoding) : TBarParamsArray;

begin
  Result:=BarTypeArrayToBarParamsArray(StringToBarTypeArray(S,aEncoding));
end;

Function IntToBarTypeArray(I: Int64; aEncoding : TBarcodeEncoding; aWidth : Integer = 0) : TBarTypeArray;

Var
  S : AnsiString;
  L : integer;

begin
  S:=IntToStr(i);
  L:=Length(S);
  if (AWidth>0) and (L<AWidth) then
    S:=StringOfChar('0',AWidth-L)+S;
  Result:=StringToBarTypeArray(S,aEncoding);
end;

Function IntToBarcodeParams(I : Int64; aEncoding : TBarcodeEncoding; aWidth : Integer = 0) : TBarParamsArray;

begin
  Result:=BarTypeArrayToBarParamsArray(IntToBarTypeArray(I,aEncoding,aWidth));
end;

Function BarTypeToBarParams(aType : TBarType) : TBarParams;

begin
  Result:=BarTypes[aType];
end;

Function BarTypeArrayToBarParamsArray(anArray : TBarTypeArray) : TBarParamsArray;

Var
  I: Integer;

begin
  Setlength(Result,Length(anArray));
  For I:=0 to length(AnArray)-1 do
    Result[i]:=BarTypeToBarParams(anArray[i]);
end;

function CalcBarWidths(aEncoding: TBarcodeEncoding; aUnit: Integer; AWeight: Double): TBarWidthArray;

Const
  Weight2to3Encodings  =
    [be2of5interleaved, be2of5industrial, be39, beEAN8, beEAN13, be39Extended, beCodabar];
  Weight225to3Encodings = [be2of5matrix];

begin
  if aEncoding in Weight2to3Encodings then
    begin
    if aWeight < 2.0 then
      aWeight := 2.0;
    if aWeight > 3.0 then
      aWeight := 3.0;
    end
  else if aEncoding in Weight225to3Encodings then
    begin
      if aWeight < 2.25 then
        aWeight := 2.25;
      if aWeight > 3.0 then
        aWeight := 3.0;
    end;

  Result[bw100]:=aUnit;
  Result[bwWeighted]:=Round(aUnit*aWeight);
  Result[bw150]:=Result[bwWeighted]*3 div 2;
  Result[bw200]:=Result[bwWeighted]*2;
end;

function CalcStringWidthInBarCodeEncoding(S : String;aEncoding: TBarcodeEncoding; aUnit: Integer; AWeight: Double): Cardinal;

Var
  BP : TBarParams;
  Data : TBarTypeArray;
  BWT : TBarWidthArray;
  I : integer;

begin
  Result:=0;
  BWT:=CalcBarWidths(aEncoding,aUnit,aWeight);
  Data:=StringToBarTypeArray(S,aEncoding);
  for i:=0 to Length(Data)-1 do  // examine the pattern string
    begin
    BP:=BarTypeToBarParams(Data[i]);
    Result:=Result+BWT[BP.w];
    end;
end;

end.

