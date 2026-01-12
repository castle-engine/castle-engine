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

{ Initial simple types for CastleInternalRenderer unit. }

{$ifdef read_interface}
//type
  { Callback type for @link(TLightsRenderer.LightRenderEvent).
    Modify whether the light is "on" right before it's rendered.
    By default, LightOn is the value of Light.LightNode.FdOn field.
    This is useful to filter lights. }
  TLightRenderEvent = CastleRendererInternalLights.TLightRenderEvent;

  TRenderer = class;
{$endif read_interface}

{$ifdef read_implementation}
{$endif}
