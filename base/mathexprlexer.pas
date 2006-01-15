{
  Copyright 2001-2006 Michalis Kamburelis.

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

{ @abstract(Lexer of mathematical expression, for MathExprParser.)

  For specification of tokens that this lexer understands,
  see documentation of MathExprParser unit. }

unit MathExprLexer;

interface

uses KambiUtils, MathExpr, SysUtils, Math;

type
  TToken = (tokEnd,
    tokConst, {< Value of given constant will be in w TMathLexer.TokenFloat. }
    tokVariable, {< Name of given variable will be in TMathLexer.TokenString. }
    tokFuncName, {< Function kind of given function will be in TMathLexer.TokenFunctionKind. }

    tokMinus, tokPlus,

    tokMultiply, tokDivide, tokPower, tokModulo,

    tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual,

    tokLParen, tokRParen, tokComma, tokLQaren, tokRQaren);

  TMathLexer = class
  private
    FToken: TToken;
    FTokenFloat: Float;
    FTokenString: string;
    FTokenFunctionKind: TFunctionKind;

    FTextPos: Integer;
    FText: string;
  public
    { @noAutoLinkHere }
    property Token: TToken read FToken;

    property TokenString: string read FTokenString;
    property TokenFloat: Float read FTokenFloat;
    property TokenFunctionKind: TFunctionKind read FTokenFunctionKind;

    { Position of lexer in the @link(Text) string. }
    property TextPos: Integer read FTextPos;

    { Text that this lexer reads.
      @noAutoLinkHere }
    property Text: string read FText;

    { NextToken moves to next token (updating fields @link(Token),
      and eventually TokenFloat, TokenString and TokenFunctionKind)
      and returns the value of field @link(Token).

      When @link(Token) is tokEnd, then NextToken doesn't do anything,
      i.e. @link(Token) will remain tokEnd forever. }
    function NextToken: TToken;

    constructor Create(const AText: string);
  end;

  { A common class for EMathLexerError and EMathParserError }
  EMathSyntaxError = class(EWrongMathExpr)
  private
    FLexerTextPos: Integer;
    FLexerText: string;
  public
    { Those things are copied from Lexer at exception creation.
      We do not copy reference to Lexer since this would be too dangerous
      in usual situation (you would have to be always sure that you will
      not access it before you Freed it; too troublesome, usually) }
    property LexerTextPos: Integer read FLexerTextPos;
    property LexerText: string read FLexerText;
    constructor Create(Lexer: TMathLexer; const s: string);
    constructor CreateFmt(Lexer: TMathLexer; const s: string;
      const args: array of const);
  end;

  EMathLexerError = class(EMathSyntaxError);

implementation

function Int64Power(base: Integer; power: Cardinal): Int64;
begin
 result := 1;
 while power > 0 do begin
  result := result*base;
  Dec(power);
 end;
end;

constructor TMathLexer.Create(const atext: string);
begin
 inherited Create;
 ftext := atext;
 fTextPos := 1;
 NextToken;
end;

function TMathLexer.NextToken: TToken;
const
  whiteChars = [' ', #9, #10, #13];
  digits = ['0'..'9'];
  litery = ['a'..'z', 'A'..'Z', '_'];

  function ReadSimpleToken: boolean;
  const
    { kolejnosc w toks_strs MA znaczenie - pierwszy zostanie dopasowany string dluzszy,
      wiec aby Lexer pracowal zachlannnie stringi dluzsze musza byc pierwsze. }
    toks_strs : array[0..16] of string=
     ('<>', '<=', '>=', '<', '>', '=', '+', '-', '*', '/', ',', '(', ')', '^', '[', ']', '%');
    toks_tokens : array[0..High(toks_strs)]of TToken =
     (tokNotEqual, tokLesserEqual, tokGreaterEqual, tokLesser, tokGreater,
      tokEqual, tokPlus, tokMinus, tokMultiply, tokDivide, tokComma, tokLParen, tokRParen,
      tokPower, tokLQaren, tokRQaren, tokModulo);
  var i: integer;
  begin
   for i := 0 to High(toks_strs) do
    if Copy(text, TextPos, Length(toks_strs[i])) = toks_strs[i] then
    begin
     ftoken := toks_tokens[i];
     Inc(fTextPos, Length(toks_strs[i]));
     result := true;
     exit;
    end;
   result := false;
  end;

  function ReadConstant: boolean;
  { czytaj constant od aktualnego miejsca (a wiec uaktualnij
    ftoken i fTokenFloat). Zwraca false jesli nie stoimy na constant. }
  var digitsCount: cardinal;
      val: Int64;
  begin
   result := text[fTextPos] in digits;
   if not result then exit;

   ftoken := tokConst;
   val := DigitAsByte(text[fTextPos]);
   Inc(fTextPos);
   while SCharIs(text, fTextPos, digits) do
   begin
    val := 10*val+DigitAsByte(text[fTextPos]);
    Inc(fTextPos);
   end;
   fTokenFloat := val;

   { czytaj czesc ulamkowa }
   if SCharIs(text, fTextPos, '.') then
   begin
    Inc(fTextPos);
    if not SCharIs(text, fTextPos, digits) then
     raise EMathLexerError.Create(Self, 'digit expected');
    digitsCount := 1;
    val := DigitAsByte(text[fTextPos]);
    Inc(fTextPos);
    while SCharIs(text, fTextPos, digits) do
    begin
     val := 10*val+DigitAsByte(text[fTextPos]);
     Inc(digitsCount);
     Inc(fTextPos);
    end;
    fTokenFloat := fTokenFloat + (val / Int64Power(10, digitsCount));
   end;
  end;

  function ReadIdentifier: string;
  { czytaj identyfikator - to znaczy, czytaj nazwe zmiennej co do ktorej nie
    jestesmy pewni czy nie jest przypadkiem nazwa funkcji. Uwaga - powinien
    zbadac kazdy znak, poczynajac od text[fTextPos], czy rzeczywiscie
    nalezy do identChars.

    Always returns non-empty string (length >= 1) }
  const identStartChars = litery;
        identChars = identStartChars + digits;
  var startPos: integer;
  begin
   if not (text[fTextPos] in identStartChars) then
    raise EMathLexerError.Create(Self, 'wrong token');
   startPos := fTextPos;
   Inc(fTextPos);
   while SCharIs(text, fTextPos, identChars) do Inc(fTextPos);
   result := CopyPos(text, startPos, fTextPos-1);
  end;

const
  consts_str: array[0..1]of string = ('pi', 'enat');
  consts_values: array[0..High(consts_str)]of float = (pi, enatural);
var p: integer;
    fk: TFunctionKind;
begin
 while SCharIs(text, TextPos, whiteChars) do Inc(fTextPos);
 if TextPos > Length(text) then
  ftoken := tokEnd else
 begin
  if not ReadSimpleToken then
  if not ReadConstant then
  begin
   { jest to zmienna, nazwa funkcji lub stalej }
   ftoken := tokVariable;
   fTokenString := ReadIdentifier;

   { sprawdzamy czy jest to nazwa funkcji }
   for fk := Low(TFunctionKind) to High(TFunctionKind) do
   begin
    if SameText(FunctionKinds[fk].FunctionName, fTokenString) then
    begin
     ftoken := tokFuncName;
     fTokenFunctionKind := fk;
     break;
    end;
   end;

   { jesli nie jest to nazwa funkcji sprawdzamy czy jest to nazwa stalej }
   if ftoken = tokVariable then
   begin
    p := ArrayPosText(fTokenString, consts_str);
    if p >= 0 then
    begin
     ftoken := tokConst;
     fTokenFloat := consts_values[p];
    end;
   end;

  end;
 end;
 result := token;
end;

{ EMathSyntaxError --------------------------------------- }

constructor EMathSyntaxError.Create(Lexer: TMathLexer; const s: string);
begin
 inherited Create(s);
 FLexerTextPos := Lexer.TextPos;
 FLexerText := Lexer.Text;
end;

constructor EMathSyntaxError.CreateFmt(Lexer: TMathLexer; const s: string;
  const args: array of const);
begin
 Create(Lexer, Format(s, args))
end;

end.

(*
---------------------------------------------------------------
tests of lexer :

function ParseMathExpr(const s: string): TMathExpr;
var lekser: TMathLekser;
begin
 lekser := TMathLekser.Create(s);
 repeat
  case lekser.token of
   tokConst: Writeln('const ',lekser.TokenFloat);
   tokVariable: Writeln('var ',lekser.TokenString);
   tokFuncName: Writeln('funkcja nr ',ord(lekser.TokenFunctionKind));
   tokMinus: Writeln('-');
   tokPlus: Writeln('+');
   tokMultiply: Writeln('*');
   tokDivide: Writeln('/');
   tokLParen: Writeln('(');
   tokRParen: Writeln(')');
   tokComma: Writeln(',');
   tokEnd: Writeln('koniec');
   else Writeln('wrong token');
  end;
  lekser.nexttoken;
 until lekser.token = tokEnd;
 lekser.free;

 result := TMathConst.Create(10);
end;

--------------------------------------------
*)

