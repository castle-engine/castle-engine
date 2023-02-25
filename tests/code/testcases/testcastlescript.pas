// -*- compile-command: "./test_single_testcase.sh TTestCastleScript" -*-
{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleScript unit. }
unit TestCastleScript;

{ Parts of CastleScript implementation right now use Pascal generics
  in ways that don't compile with Delphi.
  Symbol CASTLE_SCRIPT_FPC is in practice equal to just FPC,
  but having a dedicated symbol allows to easier grep for it and eventually fix. }
{$ifdef FPC}
  {$define CASTLE_SCRIPT_FPC}
{$endif}

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleScript = class(TCastleTestCase)
  published
    procedure Test1;
    procedure TestCodeCreatedExprs;
    procedure TestInheritsFrom;
    procedure TestFloatPrograms;
    procedure TestVariousTypesPrograms;
    {$ifdef CASTLE_SCRIPT_FPC}
    procedure TestArrays;
    {$endif CASTLE_SCRIPT_FPC}
    procedure TestBools;
    procedure TestParseConstantIntExpression;
    procedure TestInvalidOps;
    procedure TestTryExecuteMath;
    procedure TestCoalesce;
    procedure TestDivision;
  end;

implementation

uses CastleVectors, CastleScript, CastleScriptLexer, CastleScriptParser,
  CastleStringUtils, CastleScriptCoreFunctions, CastleClassUtils,
  CastleFilesUtils, CastleScriptArrays;

procedure TTestCastleScript.Test1;

  procedure WritelnLexer(const s: string);
  var
    Lexer: TCasScriptLexer;
  begin
    Lexer := TCasScriptLexer.Create(s);
    repeat
      Writeln(Lexer.TokenDescription);
      Lexer.NextToken;
    until Lexer.token = tokEnd;
    Lexer.Free;
  end;

begin
{ Interactive test:
  WritelnLexer('-10 * Pi');
}
  AssertSameValue(-10 * Pi, ParseConstantFloatExpression('-10 * Pi'));
end;

procedure TTestCastleScript.TestCodeCreatedExprs;
var
  Expr: TCasScriptExpression;
  MyVariable: TCasScriptFloat;
begin
  Expr := TCasScriptAdd.Create([
      TCasScriptSin.Create([TCasScriptFloat.Create(false, 3)]),
      TCasScriptFloat.Create(false, 10),
      TCasScriptFloat.Create(false, 1)
    ]);
  try
    AssertTrue((Expr.Execute as TCasScriptFloat).Value = sin(3) + 10 + 1);
  finally FreeAndNil(Expr) end;

  { BTW, this should not compile (the parameter-less constructor should be
    hidden by TCasScriptValue, so it's not a problem that descendants have
    overloaded constructors with "overload" keyword). }
  // MyVariable := TCasScriptFloat.Create;

  MyVariable := TCasScriptFloat.Create(false, 3);
  Expr := TCasScriptAdd.Create([
      TCasScriptSin.Create([MyVariable]),
      TCasScriptFloat.Create(false, 10),
      TCasScriptFloat.Create(false, 1)
    ]);
  try
    AssertTrue((Expr.Execute as TCasScriptFloat).Value = sin(3) + 10 + 1);

    MyVariable.Value := 4;
    AssertTrue((Expr.Execute as TCasScriptFloat).Value = sin(4) + 10 + 1);

    MyVariable.Value := 5;
    AssertTrue((Expr.Execute as TCasScriptFloat).Value = sin(5) + 10 + 1);
  finally FreeAndNil(Expr) end;
end;

procedure TTestCastleScript.TestInheritsFrom;
begin
  AssertTrue(TCasScriptFloat.InheritsFrom(TCasScriptFloat));
  AssertTrue(TCasScriptValue.InheritsFrom(TCasScriptValue));
  AssertTrue(TCasScriptFloat.InheritsFrom(TCasScriptValue));
  AssertTrue(not TCasScriptValue.InheritsFrom(TCasScriptFloat));
end;

procedure TTestCastleScript.TestFloatPrograms;
var
  Vars: array [0..3] of TCasScriptFloat;
  VarsAsValue: array [Low(Vars)..High(Vars)] of TCasScriptValue absolute Vars;
  Prog: TCasScriptProgram;
  I: Integer;
begin
  for I := 0 to High(Vars) do
  begin
    Vars[I] := TCasScriptFloat.Create(true);
    Vars[I].Value := I;
    Vars[I].Name := 'x' + IntToStr(I);
  end;

  try
    Prog := ParseProgram(FileToString('castle-data:/test_script.kscript'), VarsAsValue);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    AssertTrue(Vars[0].Value = 0);
    AssertTrue(Vars[1].Value = 3);
    AssertTrue(Vars[2].Value = 100 + 100 + 2 * 3);
    AssertTrue(Vars[3].Value = 666);
  finally
    for I := 0 to High(Vars) do FreeAndNil(Vars[I]);
  end;
end;

procedure TTestCastleScript.TestVariousTypesPrograms;
const
  Epsilon = 0.01;

var
  Prog: TCasScriptProgram;

  procedure ExecuteExpectError;
  begin
    try
      Prog.ExecuteFunction('main', []);
      Fail('should not get here');
    except
      on ECasScriptError do ;
    end;
  end;

var
  Vars: TCasScriptValueList;
begin
  Vars := TCasScriptValueList.Create(true);
  try
    Vars.Add(TCasScriptInteger.Create(true, 23));
    Vars.Add(TCasScriptFloat.Create(true, 3.14));
    Vars.Add(TCasScriptBoolean.Create(true, false));
    Vars.Add(TCasScriptString.Create(true, 'foo'));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_bool';
    Vars[3].Name := 'my_string';

    Prog := ParseProgram('function main() 666', Vars);
    { return any dummy value }
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    AssertTrue((Vars[0] as TCasScriptInteger).Value = 23);
    AssertTrue((Vars[1] as TCasScriptFloat).Value = 3.14);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'foo');

    Prog := ParseProgram(FileToString('castle-data:/test_script2.kscript'), Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    AssertTrue((Vars[0] as TCasScriptInteger).Value = 23 + 12);
    AssertSameValue(Sqrt(3.14 + 2.0), (Vars[1] as TCasScriptFloat).Value);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = true);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'barfooxyz');

    { should raise ECasScriptError }

    Prog := ParseProgram('function main() my_int := 123.0', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() my_int := string(123.0)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    { test int() }

    Prog := ParseProgram('function main() my_int := int(3.14)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 3);

    Prog := ParseProgram('function main() my_int := int(-3.14)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = -3);

    Prog := ParseProgram('function main() my_int := int(666)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 666);

    Prog := ParseProgram('function main() my_int := int(''44'')', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    AssertTrue((Vars[0] as TCasScriptInteger).Value = 44);
    Prog := ParseProgram('function main() my_int := int(''blah'')', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() my_int := int(false)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);

    Prog := ParseProgram('function main() my_int := int(true)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 1);

    Prog := ParseProgram('function main() my_int := int(5 < 6)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 1);

    { test float() }

    Prog := ParseProgram('function main() my_float := float(3.14)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 3.14, Epsilon);

    Prog := ParseProgram('function main() my_float := float(-3.14)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, -3.14, Epsilon);

    Prog := ParseProgram('function main() my_float := float(666)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 666, Epsilon);

    Prog := ParseProgram('function main() my_float := 123', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 123, Epsilon);

    Prog := ParseProgram('function main() my_float := float(''44.456'')', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 44.456, Epsilon);

    Prog := ParseProgram('function main() my_float := float(false)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 0, Epsilon);

    Prog := ParseProgram('function main() my_float := float(true)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 1, Epsilon);

    Prog := ParseProgram('function main() my_float := float(0 <> 0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertSameValue((Vars[1] as TCasScriptFloat).Value, 0, Epsilon);

    { test bool() }

    Prog := ParseProgram('function main() my_bool := bool(3.14)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = true);

    Prog := ParseProgram('function main() my_bool := bool(0.0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);

    Prog := ParseProgram('function main() my_bool := bool(0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);

    Prog := ParseProgram('function main() my_bool := bool(''44.456'')', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() my_bool := bool(''faLSE'')', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);

    Prog := ParseProgram('function main() my_bool := bool(''true'')', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = true);

    Prog := ParseProgram('function main() my_bool := bool(false)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);

    Prog := ParseProgram('function main() my_bool := bool(true)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = true);

    Prog := ParseProgram('function main() my_bool := bool(0 <> 0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);

    { test string() }

    Prog := ParseProgram('function main() my_string := string(3.14)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = '3.14');

    Prog := ParseProgram('function main() my_string := string(0.0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = '0');

    Prog := ParseProgram('function main() my_string := string(0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = '0');

    Prog := ParseProgram('function main() my_string := string(''44.456hoho'')', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = '44.456hoho');

    Prog := ParseProgram('function main() my_string := string(true)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'true');

    Prog := ParseProgram('function main() my_string := string(false)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'false');

    Prog := ParseProgram('function main() my_string := string(0 <> 0)', Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'false');

    { test if() }

    Prog := ParseProgram(FileToString('castle-data:/test_script3.kscript'), Vars);
    Prog.ExecuteFunction('main', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 12);
    AssertTrue((Vars[1] as TCasScriptFloat).Value = 0);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = true);

    Prog.ExecuteFunction('main_alt', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 44);
    AssertTrue((Vars[1] as TCasScriptFloat).Value = 13);
    AssertTrue((Vars[2] as TCasScriptBoolean).Value = false);

    { test while() }

    Prog.ExecuteFunction('main_alt_while', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 13);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'foo 1 2 3 4 5 6 7 8 9 10 11 12');

    { test for() }

    Prog.ExecuteFunction('main_alt_for', []);
    AssertTrue((Vars[3] as TCasScriptString).Value = 'xxxxxxxxxxxfooxxxxxxxxxxx');

    FreeAndNil(Prog);

    { test not Writeable }
    Vars[0].Writeable := false;
    try
      Prog := ParseProgram('function main() my_int := 123', Vars);
      Fail('should not get here');
    except
      on ECasScriptError do ;
    end;
    FreeAndNil(Prog);

    { test "if" with missing else is caught correctly }
    Prog := ParseProgram('function main() if(true, 123)', []);
    ExecuteExpectError;
    FreeAndNil(Prog);
  finally
    FreeAndNil(Vars);
  end;
end;

{$ifdef CASTLE_SCRIPT_FPC}
procedure TTestCastleScript.TestArrays;
var
  Prog: TCasScriptProgram;

  procedure ExecuteExpectError(const FuncName: string);
  begin
    try
      Prog.ExecuteFunction(FuncName, []);
      Fail('should not get here');
    except
      on ECasScriptError do ;
    end;
  end;

var
  Vars: TCasScriptValueList;
begin
  Vars := TCasScriptValueList.Create(true);
  try
    Vars.Add(TCasScriptInteger.Create(true, 23));
    Vars.Add(TCasScriptFloat.Create(true, 3.14));
    Vars.Add(TCasScriptBoolean.Create(true, false));
    Vars.Add(TCasScriptString.Create(true, 'foo'));
    Vars.Add(TCasScriptInt32Array.Create(true));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_bool';
    Vars[3].Name := 'my_string';
    Vars[4].Name := 'a_int';

    Prog := ParseProgram(FileToString('castle-data:/test_script_array.kscript'), Vars);

    Prog.ExecuteFunction('main', []);
    AssertTrue(TCasScriptInteger(Vars[0]).Value = 1 + 4 + 9 + 1 + 1 + 1);

    Prog.ExecuteFunction('main_array_d_test', []);
    AssertTrue(TCasScriptFloat(Vars[1]).Value = 3.0);

    ExecuteExpectError('main_test_invalid_index_get');
    ExecuteExpectError('main_test_invalid_index_get_2');
    ExecuteExpectError('main_test_invalid_index_set');

    FreeAndNil(Prog);

    Prog := ParseProgram(FileToString('castle-data:/test_script_string_as_array.kscript'), Vars);
    Prog.ExecuteFunction('main', []);
    AssertTrue(TCasScriptString(Vars[3]).Value = 'bbbbbbbbbbbb' + #123 + '13');

    ExecuteExpectError('error1');
    ExecuteExpectError('error2');
    ExecuteExpectError('error3');

    FreeAndNil(Prog);
  finally
    FreeAndNil(Vars);
  end;
end;
{$endif CASTLE_SCRIPT_FPC}

procedure TTestCastleScript.TestBools;
begin
  AssertTrue(ParseConstantFloatExpression('or(false, false, false)') = 0);
  AssertTrue(ParseConstantFloatExpression('or(false, false, true)') = 1);
  AssertTrue(ParseConstantFloatExpression('or(false, true, false)') = 1);
  AssertTrue(ParseConstantFloatExpression('or(true, false, false)') = 1);
  AssertTrue(ParseConstantFloatExpression('or(true, true, false)') = 1);
  AssertTrue(ParseConstantFloatExpression('or(false)') = 0);
  AssertTrue(ParseConstantFloatExpression('or(false, false)') = 0);

  AssertTrue(ParseConstantFloatExpression('and(false, false, false)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(false, false, true)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(false, true, false)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(true, false, false)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(true, true, false)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(false)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(false, false)') = 0);
  AssertTrue(ParseConstantFloatExpression('and(true)') = 1);
  AssertTrue(ParseConstantFloatExpression('and(true, true)') = 1);

  AssertTrue(ParseConstantFloatExpression('not(false)') = 1);
  AssertTrue(ParseConstantFloatExpression('not(true)') = 0);
end;

procedure TTestCastleScript.TestParseConstantIntExpression;
begin
  AssertTrue(ParseConstantIntExpression('123') = 123);
  AssertTrue(ParseConstantIntExpression('123 + 78 * 2') = 123 + 78 * 2);
  AssertTrue(ParseConstantIntExpression('false') = 0);
  AssertTrue(ParseConstantIntExpression('true') = 1);
end;

procedure TTestCastleScript.TestInvalidOps;

  { Executing (but not parsing) of Expr should raise ECasScriptError }
  procedure ExpectMathErrors(const Expr: string);
  var
    Ex: TCasScriptExpression;
  begin
    Ex := ParseFloatExpression(Expr, []);
    try
      try
        Ex.Execute;
        Fail(Expr + ' should raise ECasScriptAnyMathError, but didn''t raise anything');
      except
        on ECasScriptAnyMathError do ;
      end;
    finally FreeAndNil(Ex) end;
  end;

  { Executing (but not parsing) of Expr should raise ECasScriptError,
    but not ECasScriptAnyMathError. }
  procedure ExpectNonMathErrors(const Expr: string);
  var
    Ex: TCasScriptExpression;
  begin
    Ex := ParseFloatExpression(Expr, []);
    try
      try
        Ex.Execute;
        Fail(Expr + ' should raise ECasScriptError, but didn''t raise anything');
      except
        on E: ECasScriptError do
        begin
          AssertTrue(not (E is ECasScriptAnyMathError));
        end;
      end;
    finally FreeAndNil(Ex) end;
  end;

begin
  ExpectMathErrors('0.1 / 0.0');
  ExpectMathErrors('float(1 / 0)');
  ExpectMathErrors('ln(-3)');
  ExpectMathErrors('sqrt(-3)');

  ExpectNonMathErrors('false / true');
  ExpectNonMathErrors('or(123)');
  ExpectNonMathErrors('1 + true');
  {$ifdef CASTLE_SCRIPT_FPC}
  ExpectNonMathErrors('image_load(''blah blah'')');
  {$endif}
end;

procedure TTestCastleScript.TestTryExecuteMath;

  { Executing (but not parsing) of Expr should raise ECasScriptError.
    TryExecute will return @nil then. }
  procedure ExpectMathErrors(const Expr: string);
  var
    Ex: TCasScriptExpression;
  begin
    Ex := ParseFloatExpression(Expr, []);
    try
      AssertTrue(Ex.TryExecuteMath = nil);
    finally FreeAndNil(Ex) end;
  end;

  { Executing (but not parsing) of Expr should raise ECasScriptError,
    but not ECasScriptAnyMathError. TryExecute let's this error through then. }
  procedure ExpectNonMathErrors(const Expr: string);
  var
    Ex: TCasScriptExpression;
  begin
    Ex := ParseFloatExpression(Expr, []);
    try
      try
        Ex.TryExecuteMath;
        Fail(Expr + ' should raise ECasScriptError, but didn''t raise anything');
      except
        on E: ECasScriptError do
        begin
          AssertTrue(not (E is ECasScriptAnyMathError));
        end;
      end;
    finally FreeAndNil(Ex) end;
  end;

begin
  ExpectMathErrors('0.1 / 0.0');
  ExpectMathErrors('float(1 / 0)');
  ExpectMathErrors('ln(-3)');
  ExpectMathErrors('sqrt(-3)');

  ExpectNonMathErrors('false / true');
  ExpectNonMathErrors('or(123)');
  ExpectNonMathErrors('or(sin(123))');
  ExpectNonMathErrors('1 + true');
  {$ifdef CASTLE_SCRIPT_FPC}
  ExpectNonMathErrors('image_load(''blah blah'')');
  {$endif}
end;

procedure TTestCastleScript.TestCoalesce;

  function StringExpression(const S: String): String;
  var
    E: TCasScriptExpression;
  begin
    E := ParseStringExpression(S, []);
    try
      Result := E.AsString;
    finally FreeAndNil(E) end;
  end;

begin
  AssertEquals('asd', StringExpression('coalesce(''asd'')'));
  AssertEquals('', StringExpression('coalesce('''')'));
  AssertEquals('asd', StringExpression('coalesce(''asd'', ''dfgds'')'));
  AssertEquals('4rfgdf', StringExpression('coalesce('''', ''4rfgdf'')'));
  AssertEquals('dfgds', StringExpression('coalesce(''dfgds'', ''asd'')'));
  AssertEquals('4rfgdf', StringExpression('coalesce(''4rfgdf'', '''')'));
  AssertEquals('poip', StringExpression('coalesce('''' + '''', ''poip'')'));
end;

procedure TTestCastleScript.TestDivision;
begin
  AssertSameValue(0, ParseConstantFloatExpression('1 div 2'));
  AssertSameValue(1, ParseConstantFloatExpression('2 div 2'));
  AssertSameValue(1, ParseConstantFloatExpression('3 div 2'));
  AssertSameValue(2, ParseConstantFloatExpression('4 div 2'));

  AssertSameValue(0.5, ParseConstantFloatExpression('1 / 2'));
  AssertSameValue(1, ParseConstantFloatExpression('2 / 2'));
  AssertSameValue(1.5, ParseConstantFloatExpression('3 / 2'));
  AssertSameValue(2, ParseConstantFloatExpression('4 / 2'));
end;

initialization
  RegisterTest(TTestCastleScript);
end.
