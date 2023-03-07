// -*- compile-command: "./test_single_testcase.sh TTestCastleScriptVectors" -*-
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleScriptVectors unit. }
unit TestCastleScriptVectors;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleScriptVectors = class(TCastleTestCase)
  published
    {$ifdef CASTLE_SCRIPT_FPC}
    procedure TestVecSingle;
    procedure TestVecDouble;
    procedure TestMatrixSingle;
    {$endif CASTLE_SCRIPT_FPC}
  end;

implementation

uses CastleVectors, CastleScript, CastleScriptLexer, CastleScriptParser,
  CastleStringUtils, CastleFilesUtils, CastleScriptVectors, CastleClassUtils, Math;

{$ifdef CASTLE_SCRIPT_FPC}

procedure TTestCastleScriptVectors.TestVecSingle;
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
    Vars.Add(TCasScriptInteger.Create(true));
    Vars.Add(TCasScriptFloat.Create(true));
    Vars.Add(TCasScriptVec2f.Create(true));
    Vars.Add(TCasScriptVec3f.Create(true));
    Vars.Add(TCasScriptVec4f.Create(true));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_vec2';
    Vars[3].Name := 'my_vec3';
    Vars[4].Name := 'my_vec4';

    { test 2 }

    Prog := ParseProgram(FileToString('castle-data:/test_script_vectors.kscript'), Vars);
    Prog.ExecuteFunction('test_2', []);

    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);
    AssertTrue((Vars[1] as TCasScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0));
    AssertVectorEquals(
      (Vars[2] as TCasScriptVec2f).Value,
      Vector2(456 + 44, Vector2(456 + 44, 10 + 13).Length));

    { test 3 }

    Prog.ExecuteFunction('test_3', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);
    AssertTrue((Vars[1] as TCasScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0) +
      Single(33.0) * Single(91.0));
    AssertVectorEquals(
      (Vars[3] as TCasScriptVec3f).Value,
      Vector3(456 + 44, 10 + 13,
        Vector3(456 + 44, 10 + 13, 33).Length));

    { test 3 cross }

    Prog.ExecuteFunction('test_cross', []);
    AssertVectorEquals(
      (Vars[3] as TCasScriptVec3f).Value, Vector3(0, 0, 1));

    { test 4 }

    Prog.ExecuteFunction('test_4', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);
    AssertTrue((Vars[1] as TCasScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0) +
      Single(33.0) * Single(91.0) +
      Single(123.0) * Single(890.0));
    AssertVectorEquals(
      (Vars[4] as TCasScriptVec4f).Value,
      Vector4(456 + 44, 10 + 13, 33,
        Vector4(456 + 44, 10 + 13, 33, 123).Length));

    FreeAndNil(Prog);

    { test invalid index for vector_get/set is caught }

    Prog := ParseProgram('function main() vector_get(my_vec2, -1)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_get(my_vec2, 100)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_set(my_vec2, -1, 123)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_set(my_vec2, 100, 123)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);
  finally
    FreeAndNil(Vars);
  end;
end;

procedure TTestCastleScriptVectors.TestVecDouble;
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
    Vars.Add(TCasScriptInteger.Create(true));
    Vars.Add(TCasScriptFloat.Create(true));
    Vars.Add(TCasScriptVec2d.Create(true));
    Vars.Add(TCasScriptVec3d.Create(true));
    Vars.Add(TCasScriptVec4d.Create(true));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_vec2';
    Vars[3].Name := 'my_vec3';
    Vars[4].Name := 'my_vec4';

    { test 2 }

    Prog := ParseProgram(FileToString('castle-data:/test_script_vectors_double.kscript'), Vars);
    Prog.ExecuteFunction('test_2', []);

    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);
    AssertTrue((Vars[1] as TCasScriptFloat).Value =
      Double(44.0) * Double(666.0) +
      Double(10.0) * Double(777.0));
    AssertVectorEqualsDouble(
      (Vars[2] as TCasScriptVec2d).Value,
      Vector2Double(456 + 44, Vector2Double(456 + 44, 10 + 13).Length));

    { test 3 }

    Prog.ExecuteFunction('test_3', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);
    AssertTrue((Vars[1] as TCasScriptFloat).Value =
      Double(44.0) * Double(666.0) +
      Double(10.0) * Double(777.0) +
      Double(33.0) * Double(91.0));
    AssertVectorEqualsDouble(
      (Vars[3] as TCasScriptVec3d).Value,
      Vector3Double(456 + 44, 10 + 13,
        Vector3Double(456 + 44, 10 + 13, 33).Length));

    { test 3 cross }

    Prog.ExecuteFunction('test_cross', []);
    AssertVectorEqualsDouble(
      (Vars[3] as TCasScriptVec3d).Value, Vector3Double(0, 0, 1));

    { test 4 }

    Prog.ExecuteFunction('test_4', []);
    AssertTrue((Vars[0] as TCasScriptInteger).Value = 0);
    AssertTrue((Vars[1] as TCasScriptFloat).Value =
      Double(44.0) * Double(666.0) +
      Double(10.0) * Double(777.0) +
      Double(33.0) * Double(91.0) +
      Double(123.0) * Double(890.0));
    AssertVectorEqualsDouble(
      (Vars[4] as TCasScriptVec4d).Value,
      Vector4Double(456 + 44, 10 + 13, 33,
        Vector4Double(456 + 44, 10 + 13, 33, 123).Length));

    FreeAndNil(Prog);

    { test invalid index for vector_get/set is caught }

    Prog := ParseProgram('function main() vector_get(my_vec2, -1)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_get(my_vec2, 100)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_set(my_vec2, -1, 123)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_set(my_vec2, 100, 123)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);
  finally
    FreeAndNil(Vars);
  end;
end;

procedure TTestCastleScriptVectors.TestMatrixSingle;
var
  Vars: TCasScriptValueList;
  Prog: TCasScriptProgram;
begin
  Vars := TCasScriptValueList.Create(true);
  try
    Vars.Add(TCasScriptFloat.Create(true));
    Vars.Add(TCasScriptVec3f.Create(true));
    Vars.Add(TCasScriptVec4f.Create(true));
    Vars.Add(TCasScriptMatrix3f.Create(true));
    Vars.Add(TCasScriptMatrix4f.Create(true));

    Vars[0].Name := 'my_float';
    Vars[1].Name := 'my_vec3';
    Vars[2].Name := 'my_vec4';
    Vars[3].Name := 'my_mat3';
    Vars[4].Name := 'my_mat4';

    Prog := ParseProgram(FileToString('castle-data:/test_script_matrix.kscript'), Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    AssertVectorEquals((Vars[1] as TCasScriptVec3f).Value,
      Vector3(11 * 5 * 2, 22 * 3 * 2, 33 * 1 * 2));
    AssertVectorEquals((Vars[2] as TCasScriptVec4f).Value,
      Vector4(11 * 5 * 2, 22 * 3 * 2, 33 * 1 * 2, 44 * 666));
  finally
    FreeAndNil(Vars);
  end;
end;

{$endif CASTLE_SCRIPT_FPC}

initialization
  RegisterTest(TTestCastleScriptVectors);
end.
