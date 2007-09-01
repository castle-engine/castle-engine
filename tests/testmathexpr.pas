{
  Copyright 2007 Michalis Kamburelis.

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

unit TestMathExpr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestMathExpr= class(TTestCase)
  published
    procedure Test1;
  end;

implementation

uses VectorMath, MathExpr, MathExprLexer, MathExprParser;

procedure TTestMathExpr.Test1;

  procedure WritelnMathLexer(const s: string);
  var
    Lexer: TMathLexer;
  begin
    Lexer := TMathLexer.Create(s);
    repeat
      Writeln(Lexer.TokenDescription);
      Lexer.NextToken;
    until Lexer.token = tokEnd;
    Lexer.Free;
  end;

begin
{ Interactive test:
  WritelnMathLexer('-10 * Pi');
}
  Assert(FloatsEqual(EvalConstMathExpr('-10 * Pi'), -10 * Pi));
end;

initialization
  RegisterTest(TTestMathExpr);
end.