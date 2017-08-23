{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helper utilities, not exposed in X3DFields interface. }
{$ifdef read_interface}

{$endif read_interface}

{$ifdef read_implementation}

{ Parse float value.
  This returns Float, not just Single, because it's used by
  TSFDouble and by ParseVector(double version),
  that want double-precision preserved. }
function ParseFloat(Lexer: TX3DLexer): Float;
begin
  Lexer.CheckTokenIs(TokenNumbers, 'float number');
  result := Lexer.TokenFloat;
  Lexer.NextToken;
end;

procedure ParseVector(var Vector: array of Single; Lexer: TX3DLexer); overload;
var
  i: integer;
begin
  for i := 0 to High(Vector) do Vector[i] := ParseFloat(Lexer);
end;

procedure ParseVector(var Vector: array of Double; Lexer: TX3DLexer); overload;
var
  i: integer;
begin
  for i := 0 to High(Vector) do Vector[i] := ParseFloat(Lexer);
end;

function ParseLongWord(Lexer: TX3DLexer): LongWord;
begin
  Lexer.CheckTokenIs(vtInteger);
  result := Lexer.TokenInteger;
  Lexer.NextToken;
end;

{$endif read_implementation}
