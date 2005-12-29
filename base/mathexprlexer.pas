{
  Copyright 2001-2004 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Lekser wyrazenia matematycznego, na uzytek @link(MathExprParser).)

  Specyfikacja tokenow jakie lekser generuje jest podana w dokumentacji
  @link(MathExprParser). }

unit MathExprLexer;

interface

uses KambiUtils, MathExpr, SysUtils, Math;

type
  TToken = (tokKoniec,
    tokConst, { wartosc w token_float }
    tokVariable, { nazwa w token_string }
    tokFuncName, { funkcja w token_funckind }
    tokMinus, tokPlus,
    tokRazy, tokDziel, tokUp, tokModulo,
    tokWieksze, tokMniejsze, tokWiekszeRowne, tokMniejszeRowne, tokRowne, tokNieRowne,
    tokLParen, tokRParen, tokComma, tokLQaren, tokRQaren);

  TMathLexer = class
  private
    ftoken: TToken;
    ftoken_float: float;
    ftoken_string: string;
    ftoken_funckind: TFunctionKind;

    ftext_pos: integer;
    ftext: string;
  public
    { @noAutoLinkHere }
    property token:TToken read ftoken;
    
    property token_string:string read ftoken_string;
    property token_float:float read ftoken_float;
    property token_funckind:TFunctionKind read ftoken_funckind;

    property text_pos:integer read ftext_pos; { aktualna pozycja leksera w stringu text }
    property text:string read ftext; { tekst czytany przez leksera }

    { NextToken przesuwa sie na nastepny token (uaktualniajac pola token i
      ew. token_float, token_string) i zwraca pole token.
      Jesli w pewnym momencie token bedzie tokKoniec to juz zawsze bedzie
      tokKoniec i wywolywanie NextToken tego nie zmieni. }
    function NextToken:TToken;

    constructor Create(const atext:string);
  end;

  { A common class for EMathLexerError and EMathParserError }
  EMathSyntaxError = class(EWrongMathExpr)
  private
    FLexerTextPos:Integer;
    FLexerText:string;
  public
    { Those things are copied from Lexer at exception creation.
      We do not copy reference to Lexer since this would be too dangerous
      in usual situation (you would have to be always sure that you will
      not access it before you Freed it; too troublesome, usually) }
    property LexerTextPos:Integer read FLexerTextPos;
    property LexerText:string read FLexerText;
    constructor Create(Lexer:TMathLexer; const s:string);
    constructor CreateFmt(Lexer:TMathLexer; const s:string; const args:array of const);
  end;

  EMathLexerError = class(EMathSyntaxError);

implementation

function Int64Power(base:integer; power:cardinal):int64;
begin
 result:=1;
 while power>0 do begin
  result:=result*base;
  Dec(power);
 end;
end;

constructor TMathLexer.Create(const atext: string);
begin
 inherited Create;
 ftext:=atext;
 ftext_pos:=1;
 NextToken;
end;

function TMathLexer.NextToken: TToken;
const
  whiteChars = [' ', #9, #10, #13];
  digits = ['0'..'9'];
  litery = ['a'..'z', 'A'..'Z', '_'];

  function ReadSimpleToken:boolean;
  const
    { kolejnosc w toks_strs MA znaczenie - pierwszy zostanie dopasowany string dluzszy,
      wiec aby Lexer pracowal zachlannnie stringi dluzsze musza byc pierwsze. }
    toks_strs : array[0..16] of string=
     ('<>', '<=', '>=', '<', '>', '=', '+', '-', '*', '/', ',', '(', ')', '^', '[', ']', '%');
    toks_tokens : array[0..High(toks_strs)]of TToken =
     (tokNieRowne, tokMniejszeRowne, tokWiekszeRowne, tokMniejsze, tokWieksze,
      tokRowne, tokPlus, tokMinus, tokRazy, tokDziel, tokComma, tokLParen, tokRParen,
      tokUp, tokLQaren,tokRQaren, tokModulo);
  var i:integer;
  begin
   for i:=0 to High(toks_strs) do
    if Copy(text, text_pos, Length(toks_strs[i])) = toks_strs[i] then
    begin
     ftoken:=toks_tokens[i];
     Inc(ftext_pos,Length(toks_strs[i]));
     result:=true;
     exit;
    end;
   result:=false;
  end;

  function ReadConstant:boolean;
  { czytaj constant od aktualnego miejsca (a wiec uaktualnij
    ftoken i ftoken_float). Zwraca false jesli nie stoimy na constant. }
  var digitsCount:cardinal;
      val:Int64;
  begin
   result:=text[ftext_pos] in digits;
   if not result then exit;

   ftoken:=tokConst;
   val:=DigitAsByte(text[ftext_pos]);
   Inc(ftext_pos);
   while SCharIs(text,ftext_pos,digits) do
   begin
    val:=10*val+DigitAsByte(text[ftext_pos]);
    Inc(ftext_pos);
   end;
   ftoken_float:=val;

   { czytaj czesc ulamkowa }
   if SCharIs(text,ftext_pos,'.') then
   begin
    Inc(ftext_pos);
    if not SCharIs(text,ftext_pos,digits) then
     raise EMathLexerError.Create(Self, 'digit expected');
    digitsCount:=1;
    val:=DigitAsByte(text[ftext_pos]);
    Inc(ftext_pos);
    while SCharIs(text,ftext_pos,digits) do
    begin
     val:=10*val+DigitAsByte(text[ftext_pos]);
     Inc(digitsCount);
     Inc(ftext_pos);
    end;
    ftoken_float:=ftoken_float + (val / Int64Power(10, digitsCount));
   end;
  end;

  function ReadIdentifier:string;
  { czytaj identyfikator - to znaczy, czytaj nazwe zmiennej co do ktorej nie
    jestesmy pewni czy nie jest przypadkiem nazwa funkcji. Uwaga - powinien
    zbadac kazdy znak, poczynajac od text[ftext_pos], czy rzeczywiscie
    nalezy do identChars.

    Always returns non-empty string (length >= 1) }
  const identStartChars = litery;
        identChars = identStartChars + digits;
  var startPos:integer;
  begin
   if not (text[ftext_pos] in identStartChars) then
    raise EMathLexerError.Create(Self, 'wrong token');
   startPos:=ftext_pos;
   Inc(ftext_pos);
   while SCharIs(text, ftext_pos, identChars) do Inc(ftext_pos);
   result:=CopyPos(text, startPos, ftext_pos-1);
  end;

const
  consts_str: array[0..1]of string = ('pi', 'enat');
  consts_values: array[0..High(consts_str)]of float = (pi,enatural);
var p:integer;
    fk:TFunctionKind;
begin
 while SCharIs(text,text_pos,whiteChars) do Inc(ftext_pos);
 if text_pos>Length(text) then
  ftoken:=tokKoniec else
 begin
  if not ReadSimpleToken then
  if not ReadConstant then
  begin
   { jest to zmienna, nazwa funkcji lub stalej }
   ftoken:=tokVariable;
   ftoken_string:=ReadIdentifier;

   { sprawdzamy czy jest to nazwa funkcji }
   for fk:=Low(TFunctionKind) to High(TFunctionKind) do
   begin
    if SameText(FunctionKinds[fk].FunctionName, ftoken_string) then
    begin
     ftoken:=tokFuncName;
     ftoken_funckind:=fk;
     break;
    end;
   end;

   { jesli nie jest to nazwa funkcji sprawdzamy czy jest to nazwa stalej }
   if ftoken=tokVariable then
   begin
    p:=ArrayPosText(ftoken_string, consts_str);
    if p>=0 then
    begin
     ftoken:=tokConst;
     ftoken_float:=consts_values[p];
    end;
   end;

  end;
 end;
 result:=token;
end;

{ EMathSyntaxError --------------------------------------- }

constructor EMathSyntaxError.Create(Lexer: TMathLexer; const s: string);
begin
 inherited Create(s);
 FLexerTextPos:=Lexer.Text_Pos;
 FLexerText:=Lexer.Text;
end;

constructor EMathSyntaxError.CreateFmt(Lexer: TMathLexer; const s: string; const args: array of const);
begin
 Create(Lexer, Format(s, args))
end;

end.

(*
---------------------------------------------------------------
testy leksera :

function ParseMathExpr(const s:string):TMathExpr;
var lekser:TMathLekser;
begin
 lekser:=TMathLekser.Create(s);
 repeat
  case lekser.token of
   tokConst: Writeln('const ',lekser.token_float);
   tokVariable: Writeln('var ',lekser.token_string);
   tokFuncName: Writeln('funkcja nr ',ord(lekser.token_funckind));
   tokMinus: Writeln('-');
   tokPlus: Writeln('+');
   tokRazy: Writeln('*');
   tokDziel: Writeln('/');
   tokLParen: Writeln('(');
   tokRParen: Writeln(')');
   tokComma: Writeln(',');
   tokKoniec: Writeln('koniec');
   else Writeln('wrong token');
  end;
  lekser.nexttoken;
 until lekser.token=tokKoniec;
 lekser.free;

 result:=TMathConst.Create(10);
end;

--------------------------------------------
*)

