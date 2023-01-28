{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Deprecated unit, only to provide backward compatibility. }
unit CastleWindowModes deprecated 'use TCastleView(s) instances to organize user interface of your game';

{$I castleconf.inc}

interface

uses CastleUIControls, CastleInternalWindowModes;

type
  TGLMode = CastleInternalWindowModes.TGLMode;
  TGLModeFrozenScreen = CastleInternalWindowModes.TGLModeFrozenScreen;

procedure NoClose(Container: TCastleContainer);

implementation

procedure NoClose(Container: TCastleContainer);
begin
  CastleInternalWindowModes.NoClose(Container);
end;

end.
