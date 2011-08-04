{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestKambiScriptVectors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestKambiScriptVectors = class(TTestCase)
  published
    procedure TestVecSingle;
    procedure TestVecDouble;
    procedure TestMatrixSingle;
  end;

implementation

uses VectorMath, KambiScript, KambiScriptLexer, KambiScriptParser,
  KambiStringUtils, KambiScriptVectors, KambiClassUtils, Math;

procedure TTestKambiScriptVectors.TestVecSingle;
var
  Prog: TKamScriptProgram;

  procedure ExecuteExpectError;
  begin
    try
      Prog.ExecuteFunction('main', []);
      Assert(false, 'should not get here');
    except
      on EKamScriptError do ;
    end;
  end;

var
  Vars: TKamScriptValueList;
begin
  Vars := TKamScriptValueList.Create(true);
  try
    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Add(TKamScriptVec2f.Create(true));
    Vars.Add(TKamScriptVec3f.Create(true));
    Vars.Add(TKamScriptVec4f.Create(true));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_vec2';
    Vars[3].Name := 'my_vec3';
    Vars[4].Name := 'my_vec4';

    { test 2 }

    Prog := ParseProgram(FileToString('data' + PathDelim + 'test_script_vectors.kscript'), Vars);
    Prog.ExecuteFunction('test_2', []);

    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0));
    Assert(VectorsEqual(
      (Vars[2] as TKamScriptVec2f).Value,
      Vector2Single(456 + 44, VectorLen(Vector2Single(456 + 44, 10 + 13)))));

    { test 3 }

    Prog.ExecuteFunction('test_3', []);
    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0) +
      Single(33.0) * Single(91.0));
    Assert(VectorsEqual(
      (Vars[3] as TKamScriptVec3f).Value,
      Vector3Single(456 + 44, 10 + 13,
        VectorLen(Vector3Single(456 + 44, 10 + 13, 33)))));

    { test 3 cross }

    Prog.ExecuteFunction('test_cross', []);
    Assert(VectorsEqual(
      (Vars[3] as TKamScriptVec3f).Value, Vector3Single(0, 0, 1)));

    { test 4 }

    Prog.ExecuteFunction('test_4', []);
    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0) +
      Single(33.0) * Single(91.0) +
      Single(123.0) * Single(890.0));
    Assert(VectorsEqual(
      (Vars[4] as TKamScriptVec4f).Value,
      Vector4Single(456 + 44, 10 + 13, 33,
        VectorLen(Vector4Single(456 + 44, 10 + 13, 33, 123)))));

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

procedure TTestKambiScriptVectors.TestVecDouble;
var
  Prog: TKamScriptProgram;

  procedure ExecuteExpectError;
  begin
    try
      Prog.ExecuteFunction('main', []);
      Assert(false, 'should not get here');
    except
      on EKamScriptError do ;
    end;
  end;

var
  Vars: TKamScriptValueList;
begin
  Vars := TKamScriptValueList.Create(true);
  try
    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Add(TKamScriptVec2d.Create(true));
    Vars.Add(TKamScriptVec3d.Create(true));
    Vars.Add(TKamScriptVec4d.Create(true));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_vec2';
    Vars[3].Name := 'my_vec3';
    Vars[4].Name := 'my_vec4';

    { test 2 }

    Prog := ParseProgram(FileToString('data' + PathDelim + 'test_script_vectors_double.kscript'), Vars);
    Prog.ExecuteFunction('test_2', []);

    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Double(44.0) * Double(666.0) +
      Double(10.0) * Double(777.0));
    Assert(VectorsEqual(
      (Vars[2] as TKamScriptVec2d).Value,
      Vector2Double(456 + 44, VectorLen(Vector2Double(456 + 44, 10 + 13)))));

    { test 3 }

    Prog.ExecuteFunction('test_3', []);
    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Double(44.0) * Double(666.0) +
      Double(10.0) * Double(777.0) +
      Double(33.0) * Double(91.0));
    Assert(VectorsEqual(
      (Vars[3] as TKamScriptVec3d).Value,
      Vector3Double(456 + 44, 10 + 13,
        VectorLen(Vector3Double(456 + 44, 10 + 13, 33)))));

    { test 3 cross }

    Prog.ExecuteFunction('test_cross', []);
    Assert(VectorsEqual(
      (Vars[3] as TKamScriptVec3d).Value, Vector3Double(0, 0, 1)));

    { test 4 }

    Prog.ExecuteFunction('test_4', []);
    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Double(44.0) * Double(666.0) +
      Double(10.0) * Double(777.0) +
      Double(33.0) * Double(91.0) +
      Double(123.0) * Double(890.0));
    Assert(VectorsEqual(
      (Vars[4] as TKamScriptVec4d).Value,
      Vector4Double(456 + 44, 10 + 13, 33,
        VectorLen(Vector4Double(456 + 44, 10 + 13, 33, 123)))));

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

procedure TTestKambiScriptVectors.TestMatrixSingle;
var
  Vars: TKamScriptValueList;
  Prog: TKamScriptProgram;
begin
  Vars := TKamScriptValueList.Create(true);
  try
    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Add(TKamScriptVec3f.Create(true));
    Vars.Add(TKamScriptVec4f.Create(true));
    Vars.Add(TKamScriptMatrix3f.Create(true));
    Vars.Add(TKamScriptMatrix4f.Create(true));

    Vars[0].Name := 'my_float';
    Vars[1].Name := 'my_vec3';
    Vars[2].Name := 'my_vec4';
    Vars[3].Name := 'my_mat3';
    Vars[4].Name := 'my_mat4';

    Prog := ParseProgram(FileToString('data' + PathDelim + 'test_script_matrix.kscript'), Vars);
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    Assert(VectorsEqual((Vars[1] as TKamScriptVec3f).Value,
      Vector3Single(11 * 5 * 2, 22 * 3 * 2, 33 * 1 * 2)));
    Assert(VectorsEqual((Vars[2] as TKamScriptVec4f).Value,
      Vector4Single(11 * 5 * 2, 22 * 3 * 2, 33 * 1 * 2, 44 * 666)));
  finally
    FreeAndNil(Vars);
  end;
end;

initialization
  RegisterTest(TTestKambiScriptVectors);
end.
