{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global consts, variables at the end of unit interface. }

{$ifdef read_interface}

const
  AllVboTypes = [Low(TVboType) .. High(TVboType)];

var
  { Log renderer cache events. Allows to see how the cache performs.
    A @italic(lot) of log messages.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogRendererCache: boolean = false;

  { Log various renderer information.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogRenderer: boolean = false;

  { Global renderer cache.
    This caches common things, like textures, shapes, and much more.
    Our OpenGL resources are shared across all OpenGL contexts,
    and everything automatically uses this cache. }
  RendererCache: TRendererCache;

{$endif read_interface}

{$ifdef read_implementation}
{$endif}
