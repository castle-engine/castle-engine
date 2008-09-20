{
  Copyright 2001-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

(*
  @abstract(Parser for KambiScript language, see
  [http://vrmlengine.sourceforge.net/kambi_script.php].)

  Can parse whole program in KambiScript language, is also prepared
  to parse only a single expression (usefull for cases when I need
  to input only a mathematical expression, like for glplotter function
  expression).
*)

unit KambiScriptParser;

interface

uses MathExpr, KambiScriptLexer, Math;

{ Creates and returns instance of TMathExpr, that represents parsed tree
  of expression in S. }
function ParseFloatExpression(const S: string): TMathExpr;

{ This can be used as a great replacement for StrToFloat.
  This takes a string with any constant mathematical expression,
  parses it and calculates (with all variable names undefined,
  i.e. EUndefinedVariable will be raised if expression will try
  to use some variable name). }
function ParseConstantFloatExpression(const S: string): Float;

type
  { Reexported in this unit, so that the identifier EMathSyntaxError
    will be visible when using this unit. }
  EMathSyntaxError = KambiScriptLexer.EMathSyntaxError;

  EMathParserError = class(EMathSyntaxError);

implementation

uses SysUtils;

const
  SErrRightParenExpected = 'right paren ")" expected';
  SErrWrongFactor = 'wrong factor (expected variable, constant, "-", "(" or function name)';
  SErrKoniecExpected = 'end of expression expected, but "%s" found';
  SErrOperRelacExpected = 'comparison operator (>, <, >=, <=, = or <>) expected';
  SErrRightQarenExpected = 'right paren "]" expected';

function ParseFloatExpression(const S: string): TMathExpr;
var Lexer: TMathLexer;

  const
    FactorOperator = [tokMultiply, tokDivide, tokPower, tokModulo];
    TermOperator = [tokPlus, tokMinus];
    RelationalOperator = [tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual];

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
   if Lexer.token <> tok then
     raise EMathParserError.Create(Lexer, errString +
       ', but got ' + Lexer.TokenDescription);
  end;

  function Term: TMathExpr; forward;
  function Factor: TMathExpr; forward;

  function SimpleExpression: TMathExpr;
  var fk: TFunctionKind;
  begin
   result := nil;
   try
    result := Term;
    while Lexer.token in TermOperator do
    begin
     fk := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := TMathFunction.Create(fk, [result, Term]);
    end;
   except result.free; raise end;
  end;

  function Expression: TMathExpr;
  var fk: TFunctionKind;
  begin
   result := nil;
   try
    result := SimpleExpression;
    while Lexer.token in RelationalOperator do
    begin
     fk := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := TMathFunction.Create(fk, [result, SimpleExpression]);
    end;
   except result.free; raise end;
  end;

  function Term: TMathExpr;
  var fk: TFunctionKind;
  begin
   result := nil;
   try
    result := Factor;
    while Lexer.token in FactorOperator do
    begin
     fk := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := TMathFunction.Create(fk, [result, Factor]);
    end;
   except result.free; raise end;
  end;

  function Factor: TMathExpr;
  var fk: TFunctionKind;
      fparams: TMathExprList;
  begin
   result := nil;
   try
    case Lexer.token of
     tokVariable: begin Lexer.nexttoken; result := TMathVar.Create(Lexer.TokenString) end;
     tokConst: begin Lexer.nexttoken; result := TMathConst.Create(Lexer.TokenFloat); end;
     tokMinus: begin Lexer.nexttoken; result := TMathFunction.Create(fkNegate, [Factor()]) end;
     tokLParen: begin
        Lexer.nexttoken;
        result := Expression;
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
          fparams.Add(Expression);
         until Lexer.token <> tokComma;
         checkTokenIs(tokRParen, SErrRightParenExpected);
         Lexer.nexttoken;
         result := TMathFunction.Create(fk, fparams);
        finally fparams.free end;
       end;
     else raise EMathParserError.Create(Lexer, SErrWrongFactor +
       ', but got ' + Lexer.TokenDescription);
    end;
   except result.free; raise end;
  end;

begin
  Lexer := TMathLexer.Create(s);
  try
    result := nil;
    try
      result := Expression;
      if Lexer.token <> tokEnd then
        raise EMathParserError.Create(Lexer,
          Format(SErrKoniecExpected, [Lexer.TokenDescription]));
    except result.Free; raise end;
  finally Lexer.Free end;
end;

{ ParseConstantFloatExpression ----------------------------------------------- }

function ParseConstantFloatExpression(const S: string): Float;
var
  Expr: TMathExpr;
begin
  try
    Expr := ParseFloatExpression(s);
  except
    on E: EMathSyntaxError do
    begin
      E.Message := 'Error when parsing constant expression: ' + E.Message;
      raise;
    end;
  end;

  try
    Result := Expr.Value(@ReturnNoVariable);
  finally Expr.Free end;
end;

end.
