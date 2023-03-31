{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Parser for @url(https://castle-engine.io/castle_script.php CastleScript),
  along with high-level utilities like @link(ParseConstantFloatExpression).

  This can parse whole program in CastleScript language, is also prepared
  to parse only a single expression (usefull for cases when I need
  to input only a mathematical expression, like for glplotter function
  expression). }
unit CastleScriptParser;

{$I castleconf.inc}

interface

uses CastleScript, CastleScriptLexer, Math, CastleUtils;

type
  { Error when parsing CastleScript expression. }
  ECasScriptSyntaxError = CastleScriptLexer.ECasScriptSyntaxError;

{ Parse a CastleScript expression that should be calculated to a float value.
  The easiest way to evaluate such expression
  is to call @link(TCasScriptExpression.AsFloat) method.

  This creates and returns an instance of TCasScriptExpression,
  that represents parsed tree of expression in S,
  casted to float.
  This parses a subset of CastleScript language, that allows you
  to define only one expression without any assignments.
  Also the end result is always casted to the float() type
  (just like it would be wrapped inside float() function call ---
  in fact this is exactly what happens.)

  The end result is that this is perfect for describing things
  like function expressions, ideal e.g. for
  [https://castle-engine.io/glplotter_and_gen_function.php].

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

  @raises(ECasScriptSyntaxError in case of error when parsing expression.) }
function ParseFloatExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;

{ Parse a CastleScript expression that should be calculated to a float value.
  The easiest way to evaluate such expression
  is to call @link(TCasScriptExpression.AsInt) method.

  See @link(ParseFloatExpression) for more details, this procedure is equivalent
  but it operates on floats. }
function ParseIntExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;

{ Parse a CastleScript expression that should be calculated to a string value.
  The easiest way to evaluate such expression
  is to call @link(TCasScriptExpression.AsString) method.

  See @link(ParseFloatExpression) for more details, this procedure is equivalent
  but it operates on strings. }
function ParseStringExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;

{ Parse a CastleScript expression that should be calculated to a boolean value.
  The easiest way to evaluate such expression
  is to call @link(TCasScriptExpression.AsBool) method.

  See @link(ParseFloatExpression) for more details, this procedure is equivalent
  but it operates on booleans. }
function ParseBoolExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;

{ Creates and returns instance of TCasScriptExpression,
  that represents parsed tree of expression in S.
  @param(Variables contains a list of named values you want
    to allow in this expression.
    See ParseFloatExpression for description.)
  @raises(ECasScriptSyntaxError in case of error when parsing expression.) }
function ParseExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;

{ Parse constant float expression.
  This can be used as a drop-in replacement for StrToFloatDot.
  Takes a string with any constant mathematical expression,
  according to CastleScript syntax, parses it and calculates.

  @raises(ECasScriptSyntaxError in case of error when parsing expression.) }
function ParseConstantFloatExpression(const S: string): Float;

{ Parse constant Int64 expression.
  This can be used as a drop-in replacement for StrToInt.
  Takes a string with any constant mathematical expression,
  according to CastleScript syntax, parses it and calculates.

  @raises(ECasScriptSyntaxError in case of error when parsing expression.) }
function ParseConstantIntExpression(const S: string): Int64;

{ Parse CastleScript program.

  Variable list works like for ParseFloatExpression, see there for
  description.

  @raises(ECasScriptSyntaxError in case of error when parsing expression.)

  @groupBegin }
function ParseProgram(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptProgram; overload;
function ParseProgram(const S: string;
  const Variables: TCasScriptValueList): TCasScriptProgram; overload;
{ @groupEnd }

implementation

uses SysUtils, CastleScriptCoreFunctions;

function Expression(
  const Lexer: TCasScriptLexer;
  Environment: TCasScriptEnvironment;
  const Variables: array of TCasScriptValue): TCasScriptExpression; forward; overload;

function NonAssignmentExpression(
  const Lexer: TCasScriptLexer;
  Environment: TCasScriptEnvironment;
  const AllowFullExpressionInFactor: boolean;
  const Variables: array of TCasScriptValue): TCasScriptExpression;

  function BinaryOper(tok: TToken): TCasScriptFunctionClass;
  begin
    case tok of
      tokPlus: Result := TCasScriptAdd;
      tokMinus: Result := TCasScriptSubtract;

      tokMultiply: Result := TCasScriptMultiply;
      tokFloatDivide: Result := TCasScriptFloatDivide;
      tokIntDivide: Result := TCasScriptIntDivide;
      tokPower: Result := TCasScriptPower;
      tokModulo: Result := TCasScriptModulo;

      tokGreater: Result := TCasScriptGreater;
      tokLesser: Result := TCasScriptLesser;
      tokGreaterEqual: Result := TCasScriptGreaterEq;
      tokLesserEqual: Result := TCasScriptLesserEq;
      tokEqual: Result := TCasScriptEqual;
      tokNotEqual: Result := TCasScriptNotEqual;

      else raise ECasScriptParserError.Create(Lexer,
        'internal error : token not a binary operator');
    end
  end;

const
  FactorOperator = [tokMultiply, tokFloatDivide, tokIntDivide, tokPower, tokModulo];
  TermOperator = [tokPlus, tokMinus];
  ComparisonOperator = [tokGreater, tokLesser, tokGreaterEqual, tokLesserEqual, tokEqual, tokNotEqual];

  function Operand: TCasScriptValue;
  var
    I: Integer;
  begin
    Lexer.CheckTokenIs(tokIdentifier);

    Result := nil;
    for I := 0 to Length(Variables) - 1 do
      if SameText(Variables[I].Name, Lexer.TokenString) then
      begin
        Result := Variables[I];
        Break;
      end;

    if Result = nil then
      raise ECasScriptParserError.CreateFmt(Lexer, 'Undefined identifier "%s"',
        [Lexer.TokenString]);

    Lexer.NextToken;
  end;

  { Returns either Expression or NonAssignmentExpression, depending on
    AllowFullExpressionInFactor value. }
  function ExpressionInsideFactor: TCasScriptExpression;
  begin
    if AllowFullExpressionInFactor then
      Result := Expression(Lexer, Environment, Variables) else
      Result := NonAssignmentExpression(Lexer, Environment,
        AllowFullExpressionInFactor, Variables);
  end;

  function Factor: TCasScriptExpression;
  var
    FC: TCasScriptFunctionClass;
    FParams: TCasScriptExpressionList;
  begin
    Result := nil;
    try
      case Lexer.Token of
        tokIdentifier: Result := Operand;
        tokInteger: begin
            Result := TCasScriptInteger.Create(false, Lexer.TokenInteger);
            Result.Environment := Environment;
            Lexer.NextToken;
          end;
        tokFloat: begin
            Result := TCasScriptFloat.Create(false, Lexer.TokenFloat);
            Result.Environment := Environment;
            Lexer.NextToken;
          end;
        tokBoolean: begin
            Result := TCasScriptBoolean.Create(false, Lexer.TokenBoolean);
            Result.Environment := Environment;
            Lexer.NextToken;
          end;
        tokString: begin
            Result := TCasScriptString.Create(false, Lexer.TokenString);
            Result.Environment := Environment;
            Lexer.NextToken;
          end;
        tokMinus: begin
            Lexer.NextToken;
            Result := TCasScriptNegate.Create([Factor()]);
            Result.Environment := Environment;
          end;
        tokLParen: begin
            Lexer.NextToken;
            Result := ExpressionInsideFactor;
            Lexer.CheckTokenIs(tokRParen);
            Lexer.NextToken;
          end;
        tokFuncName: begin
            FC := Lexer.TokenFunctionClass;
            Lexer.NextToken;
            FParams := TCasScriptExpressionList.Create(false);
            try
              try
                Lexer.CheckTokenIs(tokLParen);
                Lexer.NextToken;

                if Lexer.Token <> tokRParen then
                begin
                  repeat
                    FParams.Add(ExpressionInsideFactor);
                    if Lexer.Token = tokRParen then
                      Break;
                    Lexer.CheckTokenIs(tokComma);
                    Lexer.NextToken;
                  until false;
                end;

                Lexer.CheckTokenIs(tokRParen);
                Lexer.NextToken;
              except FParams.FreeContentsByParentExpression; raise; end;
              Result := FC.Create(FParams);
              Result.Environment := Environment;
            finally FParams.Free end;
          end;
        else raise ECasScriptParserError.Create(Lexer, 'Incorrect expression factor: "' + Lexer.TokenDescription + '"');
      end;
    except Result.FreeByParentExpression; raise end;
  end;

  function Term: TCasScriptExpression;
  var
    FC: TCasScriptFunctionClass;
  begin
    Result := nil;
    try
      Result := Factor;
      while Lexer.Token in FactorOperator do
      begin
        FC := BinaryOper(Lexer.Token);
        Lexer.NextToken;
        Result := FC.Create([Result, Factor]);
        Result.Environment := Environment;
      end;
    except Result.FreeByParentExpression; raise end;
  end;

  function ComparisonArgument: TCasScriptExpression;
  var
    FC: TCasScriptFunctionClass;
  begin
    Result := nil;
    try
      Result := Term;
      while Lexer.Token in TermOperator do
      begin
        FC := BinaryOper(Lexer.Token);
        Lexer.NextToken;
        Result := FC.Create([Result, Term]);
        Result.Environment := Environment;
      end;
    except Result.FreeByParentExpression; raise end;
  end;

var
  FC: TCasScriptFunctionClass;
begin
  Result := nil;
  try
    Result := ComparisonArgument;
    while Lexer.Token in ComparisonOperator do
    begin
      FC := BinaryOper(Lexer.Token);
      Lexer.NextToken;
      Result := FC.Create([Result, ComparisonArgument]);
      Result.Environment := Environment;
    end;
  except Result.FreeByParentExpression; raise end;
end;

function Expression(
  const Lexer: TCasScriptLexer;
  Environment: TCasScriptEnvironment;
  const Variables: TCasScriptValueList): TCasScriptExpression; overload;
begin
  Result := Expression(Lexer, Environment, Variables.ToArray);
end;

function Expression(
  const Lexer: TCasScriptLexer;
  Environment: TCasScriptEnvironment;
  const Variables: array of TCasScriptValue): TCasScriptExpression; overload;

  function PossiblyAssignmentExpression: TCasScriptExpression;
  { How to parse this?

    Straighforward approach is to try parsing
    Operand, then check is it followed by ":=".
    In case of parsing errors (we can catch them by ECasScriptParserError),
    or something else than ":=", we rollback and parse NonAssignmentExpression.

    The trouble with this approach: "rollback". This is uneasy,
    as you have to carefully remember all tokens eaten during
    Operand parsing, and unget them to lexer (or otherwise reparse them).

    Simpler and faster approach used: just always parse an
    NonAssignmentExpression. This uses the fact that Operand is
    also a valid NonAssignmentExpression, and NonAssignmentExpression
    will not eat anything after ":=" (following the grammar, ":="
    cannot occur within NonAssignmentExpression without parenthesis).
    After parsing NonAssignmentExpression, we can check for ":=". }
  var
    Operand, AssignedValue: TCasScriptExpression;
  begin
    Result := NonAssignmentExpression(Lexer, Environment, true, Variables);
    try
      if Lexer.Token = tokAssignment then
      begin
        Lexer.NextToken;

        AssignedValue := PossiblyAssignmentExpression();

        Operand := Result;
        { set Result to nil, in case of exception from TCasScriptAssignment
          constructor. }
        Result := nil;

        { TCasScriptAssignment in constructor checks that
          Operand is actually a simple writeable operand. }
        Result := TCasScriptAssignment.Create([Operand, AssignedValue]);
        Result.Environment := Environment;
      end;
    except Result.FreeByParentExpression; raise end;
  end;

var
  SequenceArgs: TCasScriptExpressionList;
begin
  Result := nil;
  try
    Result := PossiblyAssignmentExpression;

    if Lexer.Token = tokSemicolon then
    begin
      SequenceArgs := TCasScriptExpressionList.Create(false);
      try
        try
          SequenceArgs.Add(Result);
          Result := nil;

          while Lexer.Token = tokSemicolon do
          begin
            Lexer.NextToken;
            SequenceArgs.Add(PossiblyAssignmentExpression);
          end;
        except SequenceArgs.FreeContentsByParentExpression; raise end;

        Result := TCasScriptSequence.Create(SequenceArgs);
        Result.Environment := Environment;
      finally FreeAndNil(SequenceArgs) end;
    end;
  except Result.FreeByParentExpression; raise end;
end;

function AProgram(
  const Lexer: TCasScriptLexer;
  const GlobalVariables: array of TCasScriptValue): TCasScriptProgram;
var
  Environment: TCasScriptEnvironment;

  function AFunction: TCasScriptUserFunction;
  var
    BodyVariables: TCasScriptValueList;
    Parameter: TCasScriptValue;
  begin
    Result := TCasScriptUserFunction.Create;
    try
      Lexer.CheckTokenIs(tokIdentifier);
      Result.Name := Lexer.TokenString;
      Lexer.NextToken;

      BodyVariables := TCasScriptValueList.Create(false);
      try
        Lexer.CheckTokenIs(tokLParen);
        Lexer.NextToken;

        if Lexer.Token <> tokRParen then
        begin
          repeat
            Lexer.CheckTokenIs(tokIdentifier);
            Parameter := TCasScriptParameterValue.Create(true);
            Parameter.Environment := Environment;
            Parameter.Name := Lexer.TokenString;
            Parameter.OwnedByParentExpression := false;
            Result.Parameters.Add(Parameter);
            BodyVariables.Add(Parameter);
            Lexer.NextToken;

            if Lexer.Token = tokRParen then
              Break else
              begin
                Lexer.CheckTokenIs(tokComma);
                Lexer.NextToken;
              end;
          until false;
        end;

        Lexer.NextToken; { eat ")" }

        { We first added parameters, then added GlobalVariables,
          so when resolving, parameter names will hide global
          variable names, just like they should in normal language. }
        BodyVariables.AddRange(GlobalVariables);

        Result.Body := Expression(Lexer, Environment, BodyVariables);
      finally FreeAndNil(BodyVariables); end;
    except FreeAndNil(Result); raise end;
  end;

begin
  Result := TCasScriptProgram.Create;
  try
    Environment := Result.Environment;
    while Lexer.Token = tokFunctionKeyword do
    begin
      Lexer.NextToken;
      Result.Functions.Add(AFunction);
    end;
  except FreeAndNil(Result); raise end;
end;

{ ParseXxxExpression ------------------------------------------------------- }

function ParseSimpleExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;
var
  Lexer: TCasScriptLexer;
  I: Integer;
begin
  for I := 0 to Length(Variables) - 1 do
    Variables[I].OwnedByParentExpression := false;

  Lexer := TCasScriptLexer.Create(s);
  try
    Result := nil;
    try
      try
        try
          Result := NonAssignmentExpression(Lexer, nil { no Environment }, false, Variables);
          Lexer.CheckTokenIs(tokEnd);
        except
          { Change ECasScriptFunctionArgumentsError (raised when
            creating functions) to ECasScriptParserError.
            This allows the caller to catch only ECasScriptSyntaxError,
            and adds position information to error message. }
          on E: ECasScriptFunctionArgumentsError do
            raise ECasScriptParserError.Create(Lexer, E.Message);
        end;
      except
        on E: ECasScriptError do
        begin
          if ScriptVerboseMessages then
            E.Message := E.Message + NL +
              'The error above is inside expression:' + NL + S;
          raise;
        end;
      end;
    except Result.FreeByParentExpression; raise end;
  finally Lexer.Free end;
end;

function ParseFloatExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;
begin
  Result := ParseSimpleExpression(S, Variables);
  { At the end, wrap Result in float() cast. }
  Result := TCasScriptFloatFun.Create([Result]);
end;

function ParseIntExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;
begin
  Result := ParseSimpleExpression(S, Variables);
  { At the end, wrap Result in int() cast. }
  Result := TCasScriptInt.Create([Result]);
end;

function ParseStringExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;
begin
  Result := ParseSimpleExpression(S, Variables);
  { At the end, wrap Result in string() cast. }
  Result := TCasScriptStringFun.Create([Result]);
end;

function ParseBoolExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;
begin
  Result := ParseSimpleExpression(S, Variables);
  { At the end, wrap Result in bool() cast. }
  Result := TCasScriptBool.Create([Result]);
end;

function ParseExpression(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptExpression;
var
  Lexer: TCasScriptLexer;
  I: Integer;
begin
  for I := 0 to Length(Variables) - 1 do
    Variables[I].OwnedByParentExpression := false;

  Lexer := TCasScriptLexer.Create(s);
  try
    Result := nil;
    try
      try
        Result := Expression(Lexer, nil { no Environment }, Variables);
        Lexer.CheckTokenIs(tokEnd);
      except
        { Change ECasScriptFunctionArgumentsError (raised when
          creating functions) to ECasScriptParserError.
          This allows the caller to catch only ECasScriptSyntaxError,
          and adds position information to error message. }
        on E: ECasScriptFunctionArgumentsError do
          raise ECasScriptParserError.Create(Lexer, E.Message);
      end;
    except Result.FreeByParentExpression; raise end;
  finally Lexer.Free end;
end;

{ ParseConstant*Expression ----------------------------------------------- }

function ParseConstantFloatExpression(const S: string): Float;
var
  Expr: TCasScriptExpression;
begin
  try
    Expr := ParseFloatExpression(s, []);
  except
    on E: ECasScriptSyntaxError do
    begin
      if Trim(S) = '' then
        // special message in this case, to be friendly to humans
        E.Message := 'Empty string is not a valid float value'
      else
        E.Message := 'Error when parsing constant float expression: ' + E.Message;
      raise;
    end;
  end;

  try
    Result := (Expr.Execute as TCasScriptFloat).Value;
  finally Expr.Free end;
end;

function ParseConstantIntExpression(const S: string): Int64;
var
  Expr: TCasScriptExpression;
begin
  try
    Expr := ParseIntExpression(s, []);
  except
    on E: ECasScriptSyntaxError do
    begin
      if Trim(S) = '' then
        // special message in this case, to be friendly to humans
        E.Message := 'Empty string is not a valid integer value'
      else
        E.Message := 'Error when parsing constant integer expression: ' + E.Message;
      raise;
    end;
  end;

  try
    Result := (Expr.Execute as TCasScriptInteger).Value;
  finally Expr.Free end;
end;

{ ParseProgram --------------------------------------------------------------- }

function ParseProgram(const S: string;
  const Variables: TCasScriptValueList): TCasScriptProgram;
begin
  Result := ParseProgram(S, Variables.ToArray);
end;

function ParseProgram(const S: string;
  const Variables: array of TCasScriptValue): TCasScriptProgram;
var
  Lexer: TCasScriptLexer;
  I: Integer;
begin
  for I := 0 to Length(Variables) - 1 do
    Variables[I].OwnedByParentExpression := false;

  Lexer := TCasScriptLexer.Create(s);
  try
    Result := nil;
    try
      try
        Result := AProgram(Lexer, Variables);
        Lexer.CheckTokenIs(tokEnd);
      except
        { Change ECasScriptFunctionArgumentsError (raised when
          creating functions) to ECasScriptParserError.
          This allows the caller to catch only ECasScriptSyntaxError,
          and adds position information to error message. }
        on E: ECasScriptFunctionArgumentsError do
          raise ECasScriptParserError.Create(Lexer, E.Message);
      end;
    except Result.Free; raise end;
  finally Lexer.Free end;
end;

end.
