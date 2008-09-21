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

uses KambiScript, KambiScriptLexer, Math;

{ Creates and returns instance of TKamScriptExpression,
  that represents parsed tree of expression in S.

  @param(Variables contains a list of named values you want
    to allow in this expression.

    Important: They will all have
    OwnedByParentExpression set to @false, and you will have to
    free them yourself.
    That's because given expression may use the same variable more than once
    (so freeing it twice would cause bugs), or not use it at all
    (so it will be automatically freed at all).

    So setting OwnedByParentExpression and freeing it yourself
    is the only sensible thing to do.) }
function ParseFloatExpression(const S: string;
  Variables: array of TKamScriptValue): TKamScriptExpression;

{ Parse constant float expression.
  This can be used as a great replacement for StrToFloat.
  Takes a string with any constant mathematical expression,
  according to KambiScript syntax, parses it and calculates. }
function ParseConstantFloatExpression(const S: string): Float;

type
  { Reexported in this unit, so that the identifier EKamScriptSyntaxError
    will be visible when using this unit. }
  EKamScriptSyntaxError = KambiScriptLexer.EKamScriptSyntaxError;

  EKamScriptParserError = class(EKamScriptSyntaxError);

implementation

uses SysUtils, KambiScriptMathFunctions;

const
  SErrRightParenExpected = 'right paren ")" expected';
  SErrWrongFactor = 'wrong factor (expected identifier, constant, "-", "(" or function name)';
  SErrKoniecExpected = 'end of expression expected, but "%s" found';
  SErrOperRelacExpected = 'comparison operator (>, <, >=, <=, = or <>) expected';
  SErrRightQarenExpected = 'right paren "]" expected';

function ParseFloatExpression(const S: string;
  Variables: array of TKamScriptValue): TKamScriptExpression;
var Lexer: TKamScriptLexer;

  const
    FactorOperator = [tokMultiply, tokDivide, tokPower, tokModulo];
    TermOperator = [tokPlus, tokMinus];
    RelationalOperator = [tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual];

  function binaryOper(tok: TToken): TKamScriptFunctionClass;
  begin
   case tok of
    tokPlus: result := TKamScriptAdd;
    tokMinus: result := TKamScriptSubtract;

    tokMultiply: result := TKamScriptMultiply;
    tokDivide: result := TKamScriptDivide;
    tokPower: result := TKamScriptPower;
    tokModulo: result := TKamScriptModulo;

    tokGreater: result := TKamScriptGreater;
    tokLesser: result := TKamScriptLesser;
    tokGreaterEqual: result := TKamScriptGreaterEq;
    tokLesserEqual: result := TKamScriptLesserEq;
    tokEqual: result := TKamScriptEqual;
    tokNotEqual: result := TKamScriptNotEqual;

    else raise EKamScriptParserError.Create(Lexer,
      'internal error : token not a binary operator');
   end
  end;

  procedure checkTokenIs(tok: TToken; const errString: string);
  begin
   if Lexer.token <> tok then
     raise EKamScriptParserError.Create(Lexer, errString +
       ', but got ' + Lexer.TokenDescription);
  end;

  function Term: TKamScriptExpression; forward;
  function Factor: TKamScriptExpression; forward;

  function SimpleExpression: TKamScriptExpression;
  var FC: TKamScriptFunctionClass;
  begin
   result := nil;
   try
    result := Term;
    while Lexer.token in TermOperator do
    begin
     FC := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := FC.Create([result, Term]);
    end;
   except result.free; raise end;
  end;

  function Expression: TKamScriptExpression;
  var FC: TKamScriptFunctionClass;
  begin
   result := nil;
   try
    result := SimpleExpression;
    while Lexer.token in RelationalOperator do
    begin
     FC := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := FC.Create([result, SimpleExpression]);
    end;
   except result.free; raise end;
  end;

  function Term: TKamScriptExpression;
  var FC: TKamScriptFunctionClass;
  begin
   result := nil;
   try
    result := Factor;
    while Lexer.token in FactorOperator do
    begin
     FC := binaryOper(Lexer.token);
     Lexer.nexttoken;
     result := FC.Create([result, Factor]);
    end;
   except result.free; raise end;
  end;

  function VariableFromName(const Name: string): TKamScriptValue;
  var
    I: Integer;
  begin
    for I := 0 to Length(Variables) - 1 do
      if SameText(Variables[I].Name, Name) then
        Exit(Variables[I]);
    Result := nil;
  end;

  function Factor: TKamScriptExpression;
  var FC: TKamScriptFunctionClass;
      fparams: TKamScriptExpressionsList;
  begin
   result := nil;
   try
    case Lexer.token of
     tokIdentifier: begin
        Lexer.nexttoken;
        Result := VariableFromName(Lexer.TokenString);
        if Result = nil then
          raise EKamScriptParserError.CreateFmt(Lexer, 'Undefined identifier "%s"',
            [Lexer.TokenString]);
       end;
     tokConst: begin
        Lexer.nexttoken;
        result := TKamScriptFloat.Create(Lexer.TokenFloat);
       end;
     tokMinus: begin
        Lexer.nexttoken;
        result := TKamScriptNegate.Create([Factor()])
       end;
     tokLParen: begin
        Lexer.nexttoken;
        result := Expression;
        checkTokenIs(tokRParen, SErrRightParenExpected);
        Lexer.nexttoken;
       end;
     tokFuncName: begin
        FC := Lexer.TokenFunctionClass;
        Lexer.nexttoken;
        fparams := TKamScriptExpressionsList.Create;
        try
         if Lexer.token = tokLParen then
         repeat
          Lexer.nexttoken; { pomin ostatni "," lub "(" }
          fparams.Add(Expression);
         until Lexer.token <> tokComma;
         checkTokenIs(tokRParen, SErrRightParenExpected);
         Lexer.nexttoken;
         result := FC.Create(fparams);
        finally fparams.free end;
       end;
     else raise EKamScriptParserError.Create(Lexer, SErrWrongFactor +
       ', but got ' + Lexer.TokenDescription);
    end;
   except result.free; raise end;
  end;

var
  I: Integer;
begin
  for I := 0 to Length(Variables) - 1 do
    Variables[I].OwnedByParentExpression := false;

  Lexer := TKamScriptLexer.Create(s);
  try
    result := nil;
    try
      result := Expression;
      if Lexer.token <> tokEnd then
        raise EKamScriptParserError.Create(Lexer,
          Format(SErrKoniecExpected, [Lexer.TokenDescription]));
    except result.Free; raise end;
  finally Lexer.Free end;
end;

{ ParseConstantFloatExpression ----------------------------------------------- }

function ParseConstantFloatExpression(const S: string): Float;
var
  Expr: TKamScriptExpression;
begin
  try
    Expr := ParseFloatExpression(s, []);
  except
    on E: EKamScriptSyntaxError do
    begin
      E.Message := 'Error when parsing constant expression: ' + E.Message;
      raise;
    end;
  end;

  try
    Result := (Expr.Execute as TKamScriptFloat).Value;
  finally Expr.Free end;
end;

end.
