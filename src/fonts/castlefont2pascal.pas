{
  Copyright 2004-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Converting fonts (TTrueTypeFont or TBmpFont) to Pascal source code. }
unit CastleFont2Pascal;

interface

uses TTFontsTypes, BmpFontsTypes, Classes;

{ @noAutoLinkHere }
procedure Font2Pascal(const TTFont: TTrueTypeFont;
  const UnitName, PrecedingComment, FontConstantName: string; Stream: TStream);
  overload;

{ @noAutoLinkHere }
procedure Font2Pascal(const TTFont: TTrueTypeFont;
  const UnitName, PrecedingComment, FontConstantName: string;
  const OutFileName: string); overload;

{ @noAutoLinkHere }
procedure Font2Pascal(const BmpFont: TBmpFont;
  const UnitName, PrecedingComment, FontConstantName: string; Stream: TStream);
  overload;

{ @noAutoLinkHere }
procedure Font2Pascal(const BmpFont: TBmpFont;
  const UnitName, PrecedingComment, FontConstantName: string;
  const OutFileName: string); overload;

implementation

uses SysUtils, CastleUtils, CastleStringUtils, CastleClassUtils;

{ WriteUnit* ---------------------------------------------------------- }

procedure WriteUnitBegin(Stream: TStream; const UnitName, PrecedingComment,
  UsesUnitName: string);
begin
 WriteStr(Stream,
   'unit ' + UnitName + ';' +nl+
   nl+
   '{' +nl+
   PrecedingComment+
   '}' +nl+
   nl+
   'interface'+nl+
   nl+
   'uses ' + UsesUnitName + ';' +nl+
   nl+
   'const' +nl);
end;

procedure WriteUnitCharHeader(Stream: TStream; c: char);
var CharName: string;
begin
 case c of
  ' ':CharName := 'space = ';
  '}':CharName := 'right curly brace = ';
  else
   (* Avoid C = '{' or '}', to not activate accidentaly
      ObjFpc nested comments feature. *)
   if c in ([' '..#255] - ['{', '}']) then
     CharName := ''''+c+''' = ' else
     CharName := '';
 end;

 WriteStr(Stream, Format('  Char%d : packed record { %s#%d }' +nl,
   [Ord(c), CharName, Ord(c)]));
end;

procedure WriteUnitEnd(Stream: TStream; const FontConstantName, FontTypeName: string);
var i: Integer;
begin
 WriteStr(Stream, '  ' + FontConstantName + ' : ' + FontTypeName + ' = (' +nl+
   '    ');
 for i := 0 to 255 do
 begin
  WriteStr(Stream, '@Char' + IntToStr(i));
  if i < 255 then WriteStr(Stream, ', ');
  if (i+1) mod 20 = 0 then WriteStr(Stream, nl+'    ');
 end;

 WriteStr(Stream, ');' +nl+ nl+ 'implementation' +nl+ nl+ 'end.'+nl);
end;

{ Write*Char ------------------------------------------------------ }

procedure WriteTTFChar(Stream: TStream; TTChar: PTTFChar);

  function PolygonKindToString(PolygonKind: TPolygonKind): string;
  begin
   case PolygonKind of
    pkNewPolygon : Result := 'pkNewPolygon';
    pkLines : Result := 'pkLines';
    pkBezier : Result := 'pkBezier';
    pkPoint : Result := 'pkPoint';
    else raise EInternalError.Create(
      'PolygonKindToString: Undefined value of TPolygonKind');
   end;
  end;

var i, WrittenInLine, WriteItemsCount: Cardinal;
begin
 if TTChar^.Info.ItemsCount = 0 then
  WriteItemsCount := 1 else
  WriteItemsCount := TTChar^.Info.ItemsCount;

 WriteStr(Stream, Format(
   '    Info : TTTFCharInfo;'+nl+
   '    Items : array[0..%d] of TTTFCharItem;'+nl+
   '  end ='+nl+
   '  ( Info : ( MoveX:%g; MoveY:%g; Height:%g;'+nl+
   '             PolygonsCount:%d;'+nl+
   '             ItemsCount:%d );' +nl+
   '    Items :'+nl+
   '    ( ',
   [ WriteItemsCount-1,
     TTChar^.Info.MoveX, TTChar^.Info.MoveY, TTChar^.Info.Height,
     TTChar^.Info.PolygonsCount,
     TTChar^.Info.ItemsCount ]));

 if TTChar^.Info.ItemsCount = 0 then
  WriteStr(Stream, '(Kind: pkPoint; x:0; y:0) { dummy item }') else
 begin
  WrittenInLine := 0;
  { TTChar^.Info.ItemsCount > 0 so TTChar^.Info.ItemsCount-1 >= 0.
    So i may be Cardinal. }
  for i := 0 to TTChar^.Info.ItemsCount-1 do
  begin
   WriteStr(Stream, '(Kind:' + PolygonKindToString(TTChar^.Items[i].Kind) + '; ');
   if TTChar^.Items[i].Kind = pkPoint then
    WriteStr(Stream, Format('x:%g; y:%g)', [TTChar^.Items[i].x, TTChar^.Items[i].y])) else
    WriteStr(Stream, Format('Count:%d)', [TTChar^.Items[i].Count]));

   Inc(WrittenInLine);
   if i < TTChar^.Info.ItemsCount-1 then
   begin
    WriteStr(Stream, ', ');
    if TTChar^.Items[i+1].Kind <> pkPoint then
     begin WriteStr(Stream, nl+'      '); WrittenInLine := 0 end else
    if WrittenInLine mod 30 = 0 then
     begin WriteStr(Stream, nl+'          '); WrittenInLine := 0 end;
   end;
  end;
 end;
 WriteStr(Stream, nl+'    )'+nl+'  );'+nl+nl);
end;

procedure WriteBFNTChar(Stream: TStream; BmpChar: PBFNTChar);
var
  RowByteLength: Cardinal;
  i, j, DataSize: Cardinal;
const
  DataIndent = '             ';
begin
 RowByteLength := BFNTCharRowByteLength(BmpChar);
 DataSize := RowByteLength * BmpChar^.Info.Height;

 { DataSize = 0 only when BmpChar^.Info.Width = 0 or BmpChar^.Info.Height = 0.
   We must change it to 1, simply because array cannot have 0 elements
   in Pascal. }
 if DataSize = 0 then DataSize := 1;

 WriteStr(Stream, Format(
   '    Info: TBFNTCharInfo;'+nl+
   '    Data: packed array[0..%d]of Byte;'+nl+
   '  end ='+nl+
   '  ( Info : ( Alignment : %d;'+nl+
   '             XOrig : %g; YOrig : %g;'+nl+
   '             XMove : %g; YMove : %g;'+nl+
   '             Width : %d; Height : %d );'+nl+
   '    Data : (' +nl,
   [ DataSize-1,
     BmpChar^.Info.Alignment,
     BmpChar^.Info.XOrig, BmpChar^.Info.YOrig,
     BmpChar^.Info.XMove, BmpChar^.Info.YMove,
     BmpChar^.Info.Width, BmpChar^.Info.Height ]));

 { write BmpChar^.Data, row by row }
 if (BmpChar^.Info.Width = 0) or (BmpChar^.Info.Height = 0) then
  WriteStr(Stream, DataIndent + '0' + nl) else
 begin
  { because BmpChar^.Info.Width > 0 and BmpChar^.Info.Height > 0
    we have also RowByteLength > 0.
    So BmpChar^.Info.Height-1 and RowByteLength-1 are >= 0.
    So i, j can be Cardinal values. }
  for j := 0 to BmpChar^.Info.Height-1 do
  begin
   WriteStr(Stream, DataIndent);
   for i := 0 to RowByteLength-1 do
   begin
    WriteStr(Stream, '$' + IntToStr16(BmpChar^.Data[i + j * RowByteLength], 2));
    if (j < BmpChar^.Info.Height-1) or (i < RowByteLength-1) then WriteStr(Stream, ', ');
   end;
   WriteStr(Stream, nl);
  end;
 end;

 WriteStr(Stream, '           );' +nl+ '  );' +nl +nl);
end;

{ Font2Pascal ----------------------------------------------------- }

procedure Font2Pascal(const TTFont: TTrueTypeFont;
  const UnitName, PrecedingComment, FontConstantName: string; Stream: TStream);
var c: char;
begin
 WriteUnitBegin(Stream, UnitName, PrecedingComment, 'TTFontsTypes');

 for c := #0 to #255 do
 begin
  WriteUnitCharHeader(Stream, c);
  WriteTTFChar(Stream, TTFont[c]);
 end;

 WriteUnitEnd(Stream, FontConstantName, 'TTrueTypeFont');
end;

procedure Font2Pascal(const BmpFont: TBmpFont;
  const UnitName, PrecedingComment, FontConstantName: string; Stream: TStream);
var c: char;
begin
 WriteUnitBegin(Stream, UnitName, PrecedingComment, 'BmpFontsTypes');

 for c := #0 to #255 do
 begin
  WriteUnitCharHeader(Stream, c);
  WriteBFNTChar(Stream, BmpFont[c]);
 end;

 WriteUnitEnd(Stream, FontConstantName, 'TBmpFont');
end;

{ OutFileName versions ---------------------------------------------------- }

procedure Font2Pascal(const TTFont: TTrueTypeFont;
  const UnitName, PrecedingComment, FontConstantName: string;
  const OutFileName: string); overload;
var Stream: TStream;
begin
 Stream := TFileStream.Create(OutFileName, fmCreate);
 try
  Font2Pascal(TTFont, UnitName, PrecedingComment, FontConstantName, Stream);
 finally Stream.Free end;
end;

procedure Font2Pascal(const BmpFont: TBmpFont;
  const UnitName, PrecedingComment, FontConstantName: string;
  const OutFileName: string); overload;
var Stream: TStream;
begin
 Stream := TFileStream.Create(OutFileName, fmCreate);
 try
  Font2Pascal(BmpFont, UnitName, PrecedingComment, FontConstantName, Stream);
 finally Stream.Free end;
end;

end.
