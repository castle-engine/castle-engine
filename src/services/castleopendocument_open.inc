{
  Copyright 2012-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on Lazarus LCL code, which has
  exactly the same license as our "Castle Game Engine":
  LGPL with static linking exception, see COPYING.txt for details.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Switch between various implementations of OpenUrl and OpenDocument. }

{$if defined(MSWINDOWS)}
  {$I castleopendocument_shellexecute.inc}
{$elseif defined(ANDROID) or defined(CASTLE_IOS)}
  {$I castleopendocument_messaging.inc}
{$elseif defined(FPC) and defined(DARWIN) and not defined(CASTLE_IOS)}
  {$I castleopendocument_macos.inc}
{$elseif defined(UNIX)}
  {$I castleopendocument_freedesktop.inc}
{$else}
  {$I castleopendocument_unimplemented.inc}
{$endif}
