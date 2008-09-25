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
    is the only sensible thing to do.)

  @raises(EKamScriptParserError in case of error when parsing expression.) }
function ParseFloatExpression(const S: string;
  Variables: array of TKamScriptValue): TKamScriptExpression;

{ Parse constant float expression.
  This can be used as a great replacement for StrToFloat.
  Takes a string with any constant mathematical expression,
  according to KambiScript syntax, parses it and calculates.

  @raises(EKamScriptParserError in case of error when parsing expression.) }
function ParseConstantFloatExpression(const S: string): Float;

type
  { Reexported in this unit, so that the identifier EKamScriptSyntaxError
    will be visible when using this unit. }
  EKamScriptSyntaxError = KambiScriptLexer.EKamScriptSyntaxError;

  EKamScriptParserError = class(EKamScriptSyntaxError);

implementation

uses SysUtils, KambiScriptMathFunctions;

function ParseFloatExpression(const S: string;
  Variables: array of TKamScriptValue): TKamScriptExpression;

const
  SErrRightParenExpected = 'right paren ")" expected';
  SErrWrongFactor = 'wrong factor (expected identifier, constant, "-", "(" or function name)';
  SErrKoniecExpected = 'end of expression expected, but "%s" found';
  SErrOperRelacExpected = 'comparison operator (>, <, >=, <=, = or <>) expected';
  SErrRightQarenExpected = 'right paren "]" expected';

var
  Lexer: TKamScriptLexer;

  const
    FactorOperator = [tokMultiply, tokDivide, tokPower, tokModulo];
    TermOperator = [tokPlus, tokMinus];
    RelationalOperator = [tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual];

  function BinaryOper(tok: TToken): TKamScriptFunctionClass;
  begin
    case tok of
      tokPlus: Result := TKamScriptAdd;
      tokMinus: Result := TKamScriptSubtract;

      tokMultiply: Result := TKamScriptMultiply;
      tokDivide: Result := TKamScriptDivide;
      tokPower: Result := TKamScriptPower;
      tokModulo: Result := TKamScriptModulo;

      tokGreater: Result := TKamScriptGreater;
      tokLesser: Result := TKamScriptLesser;
      tokGreaterEqual: Result := TKamScriptGreaterEq;
      tokLesserEqual: Result := TKamScriptLesserEq;
      tokEqual: Result := TKamScriptEqual;
      tokNotEqual: Result := TKamScriptNotEqual;

      else raise EKamScriptParserError.Create(Lexer,
        'internal error : token not a binary operator');
    end
  end;

  procedure CheckTokenIs(tok: TToken; const errString: string);
  begin
    if Lexer.Token <> tok then
      raise EKamScriptParserError.Create(Lexer, errString +
        ', but got ' + Lexer.TokenDescription);
  end;

  function Expression: TKamScriptExpression; forward;

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
  var
    FC: TKamScriptFunctionClass;
    FParams: TKamScriptExpressionsList;
  begin
    Result := nil;
    try
      case Lexer.Token of
        tokIdentifier: begin
            Lexer.NextToken;
            Result := VariableFromName(Lexer.TokenString);
            if Result = nil then
              raise EKamScriptParserError.CreateFmt(Lexer, 'Undefined identifier "%s"',
                [Lexer.TokenString]);
          end;
        tokConst: begin
            Lexer.NextToken;
            Result := TKamScriptFloat.Create(Lexer.TokenFloat);
          end;
        tokMinus: begin
            Lexer.NextToken;
            Result := TKamScriptNegate.Create([Factor()])
          end;
        tokLParen: begin
            Lexer.NextToken;
            Result := Expression;
            CheckTokenIs(tokRParen, SErrRightParenExpected);
            Lexer.NextToken;
          end;
        tokFuncName: begin
            FC := Lexer.TokenFunctionClass;
            Lexer.NextToken;
            FParams := TKamScriptExpressionsList.Create;
            try
              if Lexer.Token = tokLParen then
              repeat
                Lexer.NextToken; { pomin ostatni "," lub "(" }
                FParams.Add(Expression);
              until Lexer.Token <> tokComma;
              CheckTokenIs(tokRParen, SErrRightParenExpected);
              Lexer.NextToken;
              Result := FC.Create(FParams);
            finally FParams.Free end;
          end;
        else raise EKamScriptParserError.Create(Lexer, SErrWrongFactor +
          ', but got ' + Lexer.TokenDescription);
      end;
    except Result.Free; raise end;
  end;

  function Term: TKamScriptExpression;
  var
    FC: TKamScriptFunctionClass;
  begin
    Result := nil;
    try
      Result := Factor;
      while Lexer.Token in FactorOperator do
      begin
        FC := BinaryOper(Lexer.Token);
        Lexer.NextToken;
        Result := FC.Create([Result, Factor]);
      end;
    except Result.Free; raise end;
  end;

  function SimpleExpression: TKamScriptExpression;
  var
    FC: TKamScriptFunctionClass;
  begin
    Result := nil;
    try
      Result := Term;
      while Lexer.Token in TermOperator do
      begin
        FC := BinaryOper(Lexer.Token);
        Lexer.NextToken;
        Result := FC.Create([Result, Term]);
      end;
    except Result.Free; raise end;
  end;

  function Expression: TKamScriptExpression;
  var
    FC: TKamScriptFunctionClass;
  begin
    Result := nil;
    try
      Result := SimpleExpression;
      while Lexer.Token in RelationalOperator do
      begin
        FC := BinaryOper(Lexer.Token);
        Lexer.NextToken;
        Result := FC.Create([Result, SimpleExpression]);
      end;
    except Result.Free; raise end;
  end;

var
  I: Integer;
begin
  for I := 0 to Length(Variables) - 1 do
    Variables[I].OwnedByParentExpression := false;

  Lexer := TKamScriptLexer.Create(s);
  try
    Result := nil;
    try
      try
        Result := Expression;
        if Lexer.Token <> tokEnd then
          raise EKamScriptParserError.Create(Lexer,
            Format(SErrKoniecExpected, [Lexer.TokenDescription]));
      except
        { Change EKamScriptFunctionArgumentsError (raised when
          creating functions) to EKamScriptParserError.
          This allows the caller to catch only EKamScriptParserError,
          and adds position information to error message. }
        on E: EKamScriptFunctionArgumentsError do
          raise EKamScriptParserError.Create(Lexer, E.Message);
      end;
    except Result.Free; raise end;
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
