{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Disable loading dynamic libraries. Use this unit early. }
unit ToolDisableDynamicLibraries;

{$I castleconf.inc}

interface

implementation

uses CastleDynLib;

initialization
  { Important on Windows.
    This avoids the build tool locking DLL files of a project,
    when it is run inside the project's directory.
    This would prevent compile/clean options from removing/overwriting
    the DLL files.

    Since build tool right now doesn't actually need Zlib, Libpng, OpenAL
    libraries (but it would load them otherwise) this is a simple solution.

    Note: This is actually reverted in ToolFonts (for web) now,
    which needs FreeType. It's only used when building for web,
    so it not a problem that it may lock project's DLLs, as it will not
    try to remove/replace them.
  }
  {$ifdef MSWINDOWS}
  InternalDisableDynamicLibraries := true;
  {$endif}
end.
