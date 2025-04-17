{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various utilities of check_packages. }
unit PackageUtils;

interface

uses Classes;

{ Does given String match a regular expression, and if yes -- add all matches
  to Matches.
  Note that Matches are not cleared at the beginning.

  Matches are added in the order of regular expression sub-expressions.
  Match number 0 is the entire matched string (substring of S).

  Regardless of the compiler (FPC or Delphi) and the built-in regular expression
  in that compiler's RTL, this uses the CastleRegexpr unit,
  originally developed by Andrey V. Sorokin.
  This way our regular expression handling is fully consistent
  between FPC and Delphi. }
function StringMatchesRegexp(const S, RegexpPattern: String;
  const Matches: TStrings): Boolean;

{ Compare 2 lists of files. May change the order.
  The CaseSensitive property of lists doesn't matter.
  Makes exception in case they differ. }
procedure CompareFilesLists(const A, B: TStringList; const FailureMessage: String);

implementation

uses SysUtils, CastleRegexpr;

function StringMatchesRegexp(const S, RegexpPattern: String;
  const Matches: TStrings): Boolean;
var
  R:  TRegExpr;
  I: Integer;
begin
  R := TRegExpr.Create;
  try
    R.Expression := RegexpPattern;
    Result := R.Exec(S);
    if Result then
    begin
      for I := 0 to R.SubExprMatchCount do
        Matches.Add(R.Match[I]);
    end;

    {.$define CASTLE_DEBUG_REGEXP}
    {$ifdef CASTLE_DEBUG_REGEXP}
    if Result then
    begin
      Writeln(Format('String "%s" matches "%s" with %d subexpressions:', [
        S,
        RegexpPattern,
        R.SubExprMatchCount
      ]));
      for I := 0 to R.SubExprMatchCount do
        Writeln(Format('  Match[%d] = "%s"', [
          I,
          R.Match[I]
        ]));
    end;
    {$endif CASTLE_DEBUG_REGEXP}
  finally FreeAndNil(R) end;
end;

procedure CompareFilesLists(const A, B: TStringList; const FailureMessage: String);
begin
  A.Sort;
  B.Sort;
  if A.Count <> B.Count then
    raise Exception.CreateFmt('%s: different number of files: %d vs %d',
      [FailureMessage, A.Count, B.Count]);
  if AnsiCompareFileName(A.Text, B.Text) <> 0 then
    raise Exception.CreateFmt('%s: different files', [FailureMessage]);
end;

end.