{
  Copyright 2018 Benedikt Magnus.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example for usage of TCastleTCPClient and TCastleTCPServer in CastleClientServer. }
program client;

uses
{$ifdef UNIX}{$ifndef ANDROID}
  cthreads,
{$endif}{$endif}
{$ifdef LCL}
  // add Interfaces to workaround linking error
  // "undefined reference to `WSRegisterCustomPage'"
  Interfaces,
{$endif}
  CastleWindow, GameInitialize;

begin
  Window.OpenAndRun;
end.
