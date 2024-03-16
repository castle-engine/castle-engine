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

  When compiled with FPC, this uses FPC RegExpr unit with TRegExpr class.
  When compiled with Delphi, this uses Delphi RegularExpressions unit with TRegEx record.

  @bold(The FPC and Delphi implementations are not guaranteed to be perfectly compatible.)
  Using this routine is only safe for the subset of regular expressions that are compatible
  between FPC and Delphi implementations.
  That is also why we don't add such thing to CGE, it would be too fragile.
  But it is good enough for check_package tool. }
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