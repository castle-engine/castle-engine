{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Hack to workaround FPC 3.0.0 bug: we have to define Double-precision lists
  in a separate unit. }
unit CastleInternalDoubleLists;

{$I castleconf.inc}

interface

{$ifdef BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

uses CastleUtils, CastleVectors;

{$define read_interface}
{$I castlevectors_lists_double.inc}
{$undef read_interface}

{$endif BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

implementation

{$ifdef BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

{$define read_implementation}
{$I castlevectors_lists_double.inc}
{$undef read_implementation}

{$endif BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}

end.
