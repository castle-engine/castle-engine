{
  Copyright 2000-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Time utilities.

  Note that the initialization of this unit calls @link(CastleRandomize)
  (which on most platforms just calls standard @code(Randomize))
  to initialize random sequence of the standard @code(Random). }
unit CastleTimeUtils;

{$I castleconf.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX}
    {$ifdef FPC} BaseUnix, Unix, Dl,
    {$else} Posix.SysTypes, Posix.SysTime, Posix.SysTimes, {$endif}
  {$endif}
  {$ifdef ANDROID} Linux, {$endif}
  SysUtils, Classes, Math, Generics.Collections,
  CastleUtils;

{$define read_interface}
{$I castletimeutils_gettickcount64.inc}
{$I castletimeutils_miscellaneous.inc}
{$I castletimeutils_timer.inc}
{$I castletimeutils_processtimer.inc}
{$I castletimeutils_framespersecond.inc}
{$I castletimeutils_profiler.inc}
{$I castletimeutils_frameprofiler.inc}
{$I castletimeutils_now.inc}
{$undef read_interface}

implementation

uses Generics.Defaults, DateUtils,
  {$ifdef FPC}
    {$ifdef LINUX} Linux, {$endif} // for clock_gettime
    {$ifdef FREEBSD} FreeBSD, {$endif} // for clock_gettime
  {$endif}
  CastleLog;

{$define read_implementation}
{$I castletimeutils_miscellaneous.inc}
{$I castletimeutils_gettickcount64.inc} // must be included before castletimeutils_timer.inc on Windows or Nintendo Switch
{$I castletimeutils_timer.inc}
{$I castletimeutils_processtimer.inc}
{$I castletimeutils_framespersecond.inc}
{$I castletimeutils_now.inc}
{$I castletimeutils_profiler.inc}
{$I castletimeutils_frameprofiler.inc}

initialization
  { Required by Random and all stuff on top of it }
  CastleRandomize;
finalization
  FreeAndNil(FProfiler);
  FreeAndNil(FFrameProfiler);
end.
