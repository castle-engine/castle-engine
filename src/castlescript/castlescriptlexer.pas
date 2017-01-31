{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Lexer for CastleScript language, see
  [http://castle-engine.sourceforge.net/castle_script.php].)

  For specification of tokens that this lexer understands,
  see documentation of CastleScriptParser unit. }

unit CastleScriptLexer;

{$I castleconf.inc}

interface

uses CastleUtils, CastleScript, SysUtils, Math;

type
  TToken = (tokEnd,
    tokInteger, {< Value of constant integer will be in w TCasScriptLexer.TokenInteger. }
    tokFloat, {< Value of constant float will be in w TCasScriptLexer.TokenFloat. }
    tokBoolean, {< Value of constant boolean will be in w TCasScriptLexer.TokenBoolean. }
    tokString, {< Value of constant string will be in w TCasScriptLexer.TokenString. }

    tokIdentifier, {< Identifier will be in TCasScriptLexer.TokenString. }
    tokFuncName, {< Function class of given function will be in TCasScriptLexer.TokenFunctionClass. }
    tokFunctionKeyword,

    tokMinus, tokPlus,

    tokMultiply, tokDivide, tokPower, tokModulo,

    tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual,

    tokLParen, tokRParen,
    tokLQaren, tokRQaren,
    tokComma, tokSemicolon, tokAssignment);

  TCasScriptLexer = class
  private
    FToken: TToken;
    FTokenInteger: Int64;
    FTokenFloat: Float;
    FTokenBoolean: boolean;
    FTokenString: string;
    FTokenFunctionClass: TCasScriptFunctionClass;

    FTextPos: Integer;
    FText: string;
  public
    property Token: TToken read FToken;

    property TokenInteger: Int64 read FTokenInteger;
    property TokenFloat: Float read FTokenFloat;
    property TokenString: string read FTokenString;
    property TokenBoolean: boolean read FTokenBoolean;
    property TokenFunctionClass: TCasScriptFunctionClass read FTokenFunctionClass;

    { Position of lexer in the @link(Text) string. }
    property TextPos: Integer read FTextPos;

    { Text that this lexer reads. }
    property Text: string read FText;

    { NextToken moves to next token (updating fields @link(Token),
      and eventually TokenFloat, TokenString and TokenFunctionClass)
      and returns the value of field @link(Token).

      When @link(Token) is tokEnd, then NextToken doesn't do anything,
      i.e. @link(Token) will remain tokEnd forever.

      @raises ECasScriptLexerError }
    function NextToken: TToken;

    constructor Create(const AText: string);

    { Current token textual description. Useful mainly for debugging lexer. }
    function TokenDescription: string;

    { Check is current token Tok, eventually rise parser error.
      This is an utility for parser.

      @raises(ECasScriptParserError
        if current Token doesn't match required Tok.) }
    procedure CheckTokenIs(Tok: TToken);
  end;

  { A common class for ECasScriptLexerError and ECasScriptParserError }
  ECasScriptSyntaxError = class(ECasScriptError)
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
    constructor Create(Lexer: TCasScriptLexer; const s: string);
    constructor CreateFmt(Lexer: TCasScriptLexer; const s: string;
      const args: array of const);
  end;

  ECasScriptLexerError = class(ECasScriptSyntaxError);

  ECasScriptParserError = class(ECasScriptSyntaxError);

implementation

uses StrUtils,
  CastleStringUtils;

function Int64Power(base: Integer; power: Cardinal): Int64;
begin
 result := 1;
 while power > 0 do begin
  result := result*base;
  Dec(power);
 end;
end;

constructor TCasScriptLexer.Create(const atext: string);
begin
 inherited Create;
 ftext := atext;
 fTextPos := 1;
 NextToken;
end;

function TCasScriptLexer.NextToken: TToken;
const
  whiteChars = [' ', #9, #10, #13];
  digits = ['0'..'9'];
  Letters = ['a'..'z', 'A'..'Z', '_'];

  procedure OmitWhiteSpace;
  begin
    while SCharIs(text, TextPos, whiteChars) do Inc(fTextPos);
    if SCharIs(text, TextPos, '{') then
    begin
      while Text[TextPos] <> '}' do
      begin
        Inc(fTextPos);
        if TextPos > Length(Text) then
          raise ECasScriptLexerError.Create(Self, 'Unfinished comment');
      end;
      Inc(FTextPos);
      OmitWhiteSpace; { recusively omit the rest of whitespace }
    end;
  end;

  function ReadSimpleToken: boolean;
  const
    { kolejnosc w toks_strs MA znaczenie - pierwszy zostanie dopasowany string dluzszy,
      wiec aby Lexer pracowal zachlannnie stringi dluzsze musza byc pierwsze. }
    toks_strs: array [0..18] of string=
     ('<>', '<=', '>=', '<', '>', '=', '+', '-', '*', '/', ',',
      '(', ')', '^', '[', ']', '%', ';', ':=');
    toks_tokens: array[0..High(toks_strs)]of TToken =
     (tokNotEqual, tokLesserEqual, tokGreaterEqual, tokLesser, tokGreater,
      tokEqual, tokPlus, tokMinus, tokMultiply, tokDivide, tokComma, tokLParen, tokRParen,
      tokPower, tokLQaren, tokRQaren, tokModulo, tokSemicolon, tokAssignment);
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

  { Read a string, to a tokString token.
    Read from current TexPos.
    Update ftoken and fTokenString, and advance TextPos, and return true
    if success.

    Results in false if we're not standing at an apostrophe now. }
  function ReadString: boolean;
  var
    NextApos: Integer;
  begin
    Result := Text[FTextPos] = '''';
    if not Result then Exit;

    FToken := tokString;
    FTokenString := '';

    repeat
      NextApos := PosEx('''', Text, FTextPos + 1);
      if NextApos = 0 then
        raise ECasScriptLexerError.Create(Self, 'Unfinished string');
      FTokenString += CopyPos(Text, FTextPos + 1, NextApos - 1);
      FTextPos := NextApos + 1;

      if SCharIs(Text, FTextPos, '''') then
        FTokenString += '''' else
        Break;
    until false;
  end;

  { Read a number, to a tokFloat or tokInteger token.
    Read from current TexPos.
    Update ftoken and fTokenFloat, and advance TextPos, and return true
    if success.

    Results in false if we're not standing at a digit now. }
  function ReadNumber: boolean;
  var
    digitsCount: cardinal;
    val: Int64;
  begin
   result := text[fTextPos] in digits;
   if not result then exit;

   { Assume it's an integer token at first, until we will encounter the dot. }

   Ftoken := tokInteger;
   FTokenInteger := DigitAsByte(text[fTextPos]);
   Inc(fTextPos);
   while SCharIs(text, fTextPos, digits) do
   begin
    FTokenInteger := 10 * FTokenInteger + DigitAsByte(text[fTextPos]);
    Inc(fTextPos);
   end;

   if SCharIs(text, fTextPos, '.') then
   begin
    { So it's a float. Read fractional part. }
    FToken := tokFloat;
    FTokenFloat := FTokenInteger;

    Inc(fTextPos);
    if not SCharIs(text, fTextPos, digits) then
     raise ECasScriptLexerError.Create(Self, 'Digit expected');
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
  const identStartChars = Letters;
        identChars = identStartChars + digits;
  var startPos: integer;
  begin
   if not (text[fTextPos] in identStartChars) then
    raise ECasScriptLexerError.CreateFmt(Self,
      'Invalid character "%s" not allowed in CastleScript', [text[fTextPos]]);
   startPos := fTextPos;
   Inc(fTextPos);
   while SCharIs(text, fTextPos, identChars) do Inc(fTextPos);
   result := CopyPos(text, startPos, fTextPos-1);
  end;

const
  FloatConsts: array [0..1] of string = ('pi', 'enat');
  FloatConstsValues: array [0..High(FloatConsts)] of float = (pi, enatural);
  BooleanConsts: array [0..1] of string = ('false', 'true');
  BooleanConstsValues: array [0..High(BooleanConsts)] of boolean = (false, true);
  IntConsts: array [0..19] of string = (
    'ACTION_KEY_F1',
    'ACTION_KEY_F2',
    'ACTION_KEY_F3',
    'ACTION_KEY_F4',
    'ACTION_KEY_F5',
    'ACTION_KEY_F6',
    'ACTION_KEY_F7',
    'ACTION_KEY_F8',
    'ACTION_KEY_F9',
    'ACTION_KEY_F10',
    'ACTION_KEY_F11',
    'ACTION_KEY_F12',
    'ACTION_KEY_HOME',
    'ACTION_KEY_END',
    'ACTION_KEY_PGUP',
    'ACTION_KEY_PGDN',
    'ACTION_KEY_UP',
    'ACTION_KEY_DOWN',
    'ACTION_KEY_LEFT',
    'ACTION_KEY_RIGHT'
  );
  IntConstsValues: array [0..High(IntConsts)] of Integer = (
    1, 2, 3, 4, 5, 6, 7, 8, 9,10,
   11,12,13,14,15,16,17,18,19,20 );
var
  p: integer;
  fc: TCasScriptFunctionClass;
begin
 OmitWhiteSpace;

 if TextPos > Length(text) then
  ftoken := tokEnd else
 begin
  if not ReadString then
  if not ReadNumber then
  if not ReadSimpleToken then
  begin
   { It's something that *may* be an identifier.
     Unless it matches some keyword, built-in function or constant. }
   ftoken := tokIdentifier;
   fTokenString := ReadIdentifier;

   { Maybe it's tokFunctionKeyword (the only keyword for now) }
   if ftoken = tokIdentifier then
   begin
     if SameText(fTokenString, 'function') then
     begin
       ftoken := tokFunctionKeyword;
     end;
   end;

   { Maybe it's tokFuncName }
   if ftoken = tokIdentifier then
   begin
     fc := FunctionHandlers.SearchFunctionShortName(fTokenString);
     if fc <> nil then
     begin
      ftoken := tokFuncName;
      fTokenFunctionClass := fc;
     end;
   end;

   { Maybe it's a named constant float }
   if ftoken = tokIdentifier then
   begin
    p := ArrayPosText(fTokenString, FloatConsts);
    if p >= 0 then
    begin
     ftoken := tokFloat;
     fTokenFloat := FloatConstsValues[p];
    end;
   end;

   { Maybe it's a named constant boolean }
   if ftoken = tokIdentifier then
   begin
    p := ArrayPosText(fTokenString, BooleanConsts);
    if p >= 0 then
    begin
     ftoken := tokBoolean;
     fTokenBoolean := BooleanConstsValues[p];
    end;
   end;

   { Maybe it's a named constant integer }
   if ftoken = tokIdentifier then
   begin
    p := ArrayPosText(fTokenString, IntConsts);
    if p >= 0 then
    begin
     ftoken := tokInteger;
     fTokenInteger := IntConstsValues[p];
    end;
   end;
  end;
 end;
 result := token;
end;

const
  TokenShortDescription: array [TToken] of string =
  ( 'end of stream',
    'integer',
    'float',
    'boolean',
    'string',
    'identifier',
    'built-in function',
    'function',
    '-', '+',
    '*', '/', '^', '%',
    '>', '<', '>=', '<=', '=', '<>',
    '(', ')',
    '[', ']',
    ',', ';', ':=');

function TCasScriptLexer.TokenDescription: string;
begin
  Result := TokenShortDescription[Token];
  case Token of
    tokInteger: Result += Format(' %d', [TokenInteger]);
    tokFloat: Result += Format(' %g', [TokenFloat]);
    tokBoolean: Result += Format(' %s', [BoolToStr(TokenBoolean, true)]);
    tokString: Result += Format(' ''%s''', [TokenString]);
    tokIdentifier: Result += Format(' %s', [TokenString]);
    tokFuncName: Result += Format(' %s', [TokenFunctionClass.Name]);
  end;
end;

procedure TCasScriptLexer.CheckTokenIs(Tok: TToken);
begin
  if Token <> Tok then
    raise ECasScriptParserError.CreateFmt(Self,
      'Expected "%s", but got "%s"',
      [ TokenShortDescription[Tok], TokenDescription ]);
end;

{ ECasScriptSyntaxError --------------------------------------- }

constructor ECasScriptSyntaxError.Create(Lexer: TCasScriptLexer; const s: string);
begin
 inherited Create(s);
 FLexerTextPos := Lexer.TextPos;
 FLexerText := Lexer.Text;
end;

constructor ECasScriptSyntaxError.CreateFmt(Lexer: TCasScriptLexer; const s: string;
  const args: array of const);
begin
 Create(Lexer, Format(s, args))
end;

end.
