{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Castle Game Engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ See FPC web bug 3097 }

unit TestFNMatch;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestFNMatch = class(TTestCase)
  published
    procedure TestFNMatch;
  end;

implementation

{$ifdef UNIX}

{ Based on
  /win/docs/fpc/kambi_fpc_bugs/fnmatch_test/fnmatch_test.pas
}

uses
  {$ifdef VER1_0} Linux {$else} UnixUtil {$endif}
  {$ifdef USE_LIBC}, Libc {$endif};

procedure TTestFNMatch.TestFNMatch;

  procedure CheckMatch(const Pattern, Name: string; GoodResult: boolean);
  var UnixUtilResult, LibcResult: boolean;
  begin
   UnixUtilResult:=
     {$ifdef VER1_0} Linux {$else} UnixUtil {$endif} .FNMatch(Pattern, Name);
   {$ifdef USE_LIBC}
   LibcResult := Libc.FNMatch(PChar(Pattern), PChar(Name), 0) = 0;
   {$else}
   LibcResult := UnixUtilResult; { just fake LibcResult as UnixUtilResult }
   {$endif}

   { We have 3 results. All should be equal. }
   Assert(UnixUtilResult = LibcResult);
   Assert(LibcResult = GoodResult);
  end;

begin
 { Those tests fail with original FNMatch code, because FNMatch
   allowed '*x' (for any 'x') to match anything (ending with 'x' or not,
   zero length or not). }
 CheckMatch('*~', 'foo', false);
 CheckMatch('*b', 'foo', false);
 CheckMatch('*?', '', false);
 CheckMatch('???*o', 'foo', false);
 CheckMatch('*???*o', 'foo', false);

 (*This test fails with original FNMatch code, because after line
     'inc(j);{We didn't find one, need to look further}'
   found is still assumed to be true (while it should be false) *)
 CheckMatch('*o', 'blah.ow', false);

 { This fails with original FNMatch code because subsequent tries
   to match char right after '*' (i.e., 'o' in this case) actually
   can miss that 'x' <> 'o' when 'x' is the last char of Name. }
 CheckMatch('*o', 'fox', false);

 { When first error is solved, we can see other problem
   (that was hidden by previous bug):
   When the '?' in Pattern matches last char of Name,
   some problems arise. That's because of original FNMatch code
     '?' : begin
             inc(j);
             Found:=(j <= LenName);
           end;
   Nonsense ?
   This should check FIRST if (j <= LenName).
   If not, if should terminate whole DoFNMatch with false,
   not only the loop labeled
   'find the next character in pattern, different of ? and *'.
   And in that loop, variable i should get a chance to be > LenPat.

   Tests below ensure that these additional fixes are also applied.
   I.e. these tests worked before my fixes were applied AND they
   work after my fixes are applied. But they we're causing trouble
   when I was working on this and my fixes we're applied only partially. }
 CheckMatch('*?', '?', true);
 CheckMatch('*?', 'a', true);

 { Some additional tests, they worked before my fix and they work
   after my fix. Just to be sure that everything is OK now. }
 CheckMatch('*o', 'foo', true);
 CheckMatch('*.~', 'foo', false);
 CheckMatch('*.b', 'foo', false);
 CheckMatch('*.o', 'foo', false);
 CheckMatch('*??*o', 'foo', true);
 CheckMatch('?o', 'foo', false);
 CheckMatch('??o', 'foo', true);
 CheckMatch('?o?', 'foo', true);
 CheckMatch('o??', 'foo', false);
 CheckMatch('*', 'foo', true);
end;

{$else UNIX}

procedure TTestFNMatch.TestFNMatch;
begin
end;

{$endif UNIX}

initialization
 RegisterTest(TTestFNMatch);
end.
