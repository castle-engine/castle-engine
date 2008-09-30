{
  Copyright 2008 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestKambiScriptVectors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestKambiScriptVectors = class(TTestCase)
  published
    procedure Test1;
  end;

implementation

uses VectorMath, KambiScript, KambiScriptLexer, KambiScriptParser,
  KambiStringUtils, KambiScriptVectors, KambiClassUtils, Math;

procedure TTestKambiScriptVectors.Test1;
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
  Vars: TKamScriptValuesList;
begin
  Vars := TKamScriptValuesList.Create;
  try
    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Add(TKamScriptVec2f.Create(true));

    Vars[0].Name := 'my_int';
    Vars[1].Name := 'my_float';
    Vars[2].Name := 'my_vec2f';

    Prog := ParseProgram(FileToString('test_script_vectors.kscript'), Vars);
    { return any dummy value }
    Prog.ExecuteFunction('main', []);
    FreeAndNil(Prog);

    Assert((Vars[0] as TKamScriptInteger).Value = 0);
    Assert((Vars[1] as TKamScriptFloat).Value =
      Single(44.0) * Single(666.0) +
      Single(10.0) * Single(777.0));
    Assert(VectorsEqual(
      (Vars[2] as TKamScriptVec2f).Value,
      Vector2Single(456 + 44, VectorLen(Vector2Single(456 + 44, 10 + 13)))));

    Prog := ParseProgram('function main() vector_get(my_vec2f, -1)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_get(my_vec2f, 100)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_set(my_vec2f, -1, 123)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);

    Prog := ParseProgram('function main() vector_set(my_vec2f, 100, 123)', Vars);
    ExecuteExpectError;
    FreeAndNil(Prog);
  finally
    FreeWithContentsAndNil(Vars);
  end;
end;

initialization
  RegisterTest(TTestKambiScriptVectors);
end.
