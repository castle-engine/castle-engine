{
  Copyright 2007-2008 Michalis Kamburelis.

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

unit TestKambiScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestKambiScript = class(TTestCase)
  published
    procedure Test1;
    procedure TestCodeCreatedExprs;
    procedure TestInheritsFrom;
    procedure TestFloatPrograms;
  end;

implementation

uses VectorMath, KambiScript, KambiScriptLexer, KambiScriptParser,
  KambiStringUtils, KambiScriptMathFunctions;

procedure TTestKambiScript.Test1;

  procedure WritelnLexer(const s: string);
  var
    Lexer: TKamScriptLexer;
  begin
    Lexer := TKamScriptLexer.Create(s);
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
  Assert(FloatsEqual(ParseConstantFloatExpression('-10 * Pi'), -10 * Pi));
end;

procedure TTestKambiScript.TestCodeCreatedExprs;
var
  Expr: TKamScriptExpression;
  MyVariable: TKamScriptFloat;
begin
  Expr := TKamScriptAdd.Create([
      TKamScriptSin.Create([TKamScriptFloat.Create(3)]),
      TKamScriptFloat.Create(10),
      TKamScriptFloat.Create(1)
    ]);
  try
    Assert((Expr.Execute as TKamScriptFloat).Value = sin(3) + 10 + 1);
  finally FreeAndNil(Expr) end;

  MyVariable := TKamScriptFloat.Create(3);
  Expr := TKamScriptAdd.Create([
      TKamScriptSin.Create([MyVariable]),
      TKamScriptFloat.Create(10),
      TKamScriptFloat.Create(1)
    ]);
  try
    Assert((Expr.Execute as TKamScriptFloat).Value = sin(3) + 10 + 1);

    MyVariable.Value := 4;
    Assert((Expr.Execute as TKamScriptFloat).Value = sin(4) + 10 + 1);

    MyVariable.Value := 5;
    Assert((Expr.Execute as TKamScriptFloat).Value = sin(5) + 10 + 1);
  finally FreeAndNil(Expr) end;
end;

procedure TTestKambiScript.TestInheritsFrom;
begin
  Assert(TKamScriptFloat.InheritsFrom(TKamScriptFloat));
  Assert(TKamScriptValue.InheritsFrom(TKamScriptValue));
  Assert(TKamScriptFloat.InheritsFrom(TKamScriptValue));
  Assert(not TKamScriptValue.InheritsFrom(TKamScriptFloat));
end;

procedure TTestKambiScript.TestFloatPrograms;
var
  Vars: array [0..3] of TKamScriptFloat;
  VarsAsValue: array [Low(Vars)..High(Vars)] of TKamScriptValue absolute Vars;
  Prog: TKamScriptProgram;
  I: Integer;
begin
  for I := 0 to High(Vars) do
  begin
    Vars[I] := TKamScriptFloat.Create;
    Vars[I].Value := I;
    Vars[I].Name := 'x' + IntToStr(I);
  end;

  try
    Prog := ParseProgram(FileToString('test_script.kscript'), VarsAsValue);

    Prog.ExecuteFunction('main', []);

    Assert(Vars[0].Value = 0);
    Assert(Vars[1].Value = 3);
    Assert(Vars[2].Value = 100 + 100 + 2 * 3);
    Assert(Vars[3].Value = 666);
  finally
    for I := 0 to High(Vars) do FreeAndNil(Vars[I]);
  end;
end;

initialization
  RegisterTest(TTestKambiScript);
end.
