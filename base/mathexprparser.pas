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

(*
  @abstract(Parser wyrazenia matematycznego.
  Zrobiony specjalnie aby obejmowac mozliwie
  wygodna skladnia wszystko co moze zaoferowac klasa TMathExpr.)

  Gramatyka w EBNF :
  (wiele komentarzy jest w dokumentacji gen_funkcja.php na camelot.
  Usunalem je stad zeby nie duplikowac informacji.)

  czynnik = NAZWA_ZMIENNEJ | STALA | "-" czynnik | "(" wyrazenie_math ")" |
            NAZWA_FUNKCJI [ "(" wyrazenie_math [{"," wyrazenie_math}] ")" ] |
            "[" wyrazenie_math operator_relacyjny wyrazenie_math "]"
  skladnik = czynnik [{operator_multiplikatywny czynnik}]
  wyrazenie_math = skladnik [{operator_addytywny skladnik}]
  operator_multiplikatywny = "^" | "*" | "/" | "%"
  operator_addytywny = "+" | "-"
  operator_relacyjny = "<" | ">" | "<=" | ">=" | "=" | "<>"

  (czesc rozwiazywana przez Lexer :)
  NAZWA_ZMIENNEJ = LITERA [{LITERA | CYFRA}]
  STALA = "pi" | "enat" | CYFRA [{CYFRA}] ["." CYFRA [{CYFRA}] ]
  NAZWA_FUNKCJI = (see gen_funkcja docs)
  LITERA = nieformalnie 'a' .. 'z' | 'A' .. 'Z' | "_"
                  (tak, znaku podkreslenia mozna uzyc wszedzie tam gdzie litery)
  CYFRA = '0' .. '9'

  (token = terminal w czesci gramatyki rozwiazywanej przez parser
           lub nieterminal w czesci gramatyki rozwiazywanej przez Lexer.
   Generalnie pomiedzy kazdymi dwoma tokenami musi byc przynajmniej jeden
   bialy znak - spacja, #10, #13, lub tab. Ale sa wyjatki gdy mozna
   jednoznacznie rozpoznac koniec jednego tokenu, np. gdy dwa tokeny
   pochodza z dwoch roznych zbiorow - jeden sposrod cyfr/liter/_ a drugi
   z reszty. W razie watpliwosci Lexer bedzie pobieral tokeny zachlannie.
  )
*)

unit MathExprParser;

interface

uses MathExpr, MathExprLexer, Math;

{ Creates and returns instance of TMathExpr, that represents parsed tree
  of expression in S. }
function ParseMathExpr(const S: string): TMathExpr;

{ This can be used as a great replacement for StrToFloat.
  This takes a string with any constant mathematical expression,
  parses it and calculates (with all variable names undefined,
  i.e. EUndefinedVariable will be raised if expression will try
  to use some variable name). }
function EvalConstMathExpr(const S: string): Float;

type
  { Reexported in this unit, so that the identifier EMathSyntaxError
    will be visible when using this unit. }
  EMathSyntaxError = MathExprLexer.EMathSyntaxError;

  EMathParserError = class(EMathSyntaxError);

implementation

const
  SErrRightParenExpected = 'right paren ")" expected';
  SErrWrongCzynnik = 'wrong czynnik (expected variable, constant, "-", "(" or function name)';
  SErrKoniecExpected = 'end of expression expected';
  SErrOperRelacExpected = 'operator relacyjny (>, <, >=, <=, = or <>) expected';
  SErrRightQarenExpected = 'right paren "]" expected';

function ParseMathExpr(const S: string): TMathExpr;
var Lexer: TMathLexer;

  const operatory_multi = [tokMultiply, tokDivide, tokPower, tokModulo];
        operatory_addy = [tokPlus, tokMinus];
        operatory_relac = [tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual];

  function binaryOper(tok: TToken): TFunctionKind;
  begin
   case tok of
    tokPlus: result := fkAdd;
    tokMinus: result := fkSubtract;

    tokMultiply: result := fkMultiply;
    tokDivide: result := fkDivide;
    tokPower: result := fkPower;
    tokModulo: result := fkModulo;

    tokGreater: result := fkGreater;
    tokLesser: result := fkLesser;
    tokGreaterEqual: result := fkGreaterEq;
    tokLesserEqual: result := fkLesserEq;
    tokEqual: result := fkEqual;
    tokNotEqual: result := fkNotEqual;
    else raise EMathParserError.Create(Lexer,
      'internal error : token not a binary operator');
   end
  end;

  procedure checkTokenIs(tok: TToken; const errString: string);
  begin
   if Lexer.token <> tok then raise EMathParserError.Create(Lexer, errString);
  end;

  function skladnik: TMathExpr; forward;
  function czynnik: TMathExpr; forward;

  function wyrazenie_math: TMathExpr;
  var fk: TFunctionKind;
  begin
   result := nil;
   try
    result := skladnik;
    while Lexer.token in operatory_addy do
    begin
     fk := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := TMathFunction.Create(fk, [result, skladnik]);
    end;
   except result.free; raise end;
  end;

  function skladnik: TMathExpr;
  var fk: TFunctionKind;
  begin
   result := nil;
   try
    result := czynnik;
    while Lexer.token in operatory_multi do
    begin
     fk := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := TMathFunction.Create(fk, [result, czynnik]);
    end;
   except result.free; raise end;
  end;

  function czynnik: TMathExpr;
  var fk: TFunctionKind;
      fparams: TMathExprList;
  begin
   result := nil;
   try
    case Lexer.token of
     tokVariable: begin Lexer.nexttoken; result := TMathVar.Create(Lexer.TokenString) end;
     tokConst: begin Lexer.nexttoken; result := TMathConst.Create(Lexer.TokenFloat) end;
     tokMinus: begin Lexer.nexttoken; result := TMathFunction.Create(fkNegate, [czynnik]) end;
     tokLParen: begin
        Lexer.nexttoken;
        result := wyrazenie_math;
        checkTokenIs(tokRParen, SErrRightParenExpected);
        Lexer.nexttoken;
       end;
     tokFuncName: begin
        fk := Lexer.TokenFunctionKind;
        Lexer.nexttoken;
        fparams := TMathExprList.Create;
        try
         if Lexer.token = tokLParen then
         repeat
          Lexer.nexttoken; { pomin ostatni "," lub "(" }
          fparams.Add(wyrazenie_math);
         until Lexer.token <> tokComma;
         checkTokenIs(tokRParen, SErrRightParenExpected);
         Lexer.nexttoken;
         result := TMathFunction.Create(fk, fparams);
        finally fparams.free end;
       end;
     tokLQaren: begin
        Lexer.nexttoken;
        fparams := TMathExprList.Create;
        try
         fparams.Add(wyrazenie_math);
         if not (Lexer.token in operatory_relac) then
          raise EMathParserError.Create(Lexer, SErrOperRelacExpected);
         fk := binaryOper(Lexer.token);
         Lexer.nexttoken;
         fparams.Add(wyrazenie_math);
         checkTokenIs(tokRQaren, SErrRightQarenExpected);
         Lexer.nexttoken;
         result := TMathFunction.Create(fk, fparams);
        finally fparams.free end;
       end;
     else raise EMathParserError.Create(Lexer, SErrWrongCzynnik);
    end;
   except result.free; raise end;
  end;

begin
 Lexer := TMathLexer.Create(s);
 try
  result := nil;
  try
   result := wyrazenie_math;
   CheckTokenIs(tokEnd, SErrKoniecExpected);
  except result.Free; raise end;
 finally Lexer.Free end;
end;

{ EvalConstMathExpr ---------------------------------------- }

function EvalConstMathExpr(const S: string): Float;
var Expr: TMathExpr;
begin
 Expr := ParseMathExpr(s);
 try
  result := Expr.Value(@ReturnNoVariable);
 finally Expr.Free end;
end;

end.
