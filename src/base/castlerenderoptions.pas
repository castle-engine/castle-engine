{
  Copyright 2016-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Configure rendering options.
  The @link(TCastleRenderOptions) component configures the rendering at each particular scene,
  and is usually accessed through @link(TCastleScene.RenderOptions).
  This unit contains also related types, constants and some variables. }
unit CastleRenderOptions;

{$I castleconf.inc}

interface

uses Classes, CastleColors;

{$define read_interface}
{$I castlerenderoptions_globals.inc}
{$I castlerenderoptions_renderoptions.inc}
{$undef read_interface}

var
  { Render options used by editor in various situations, e.g.
    change rendering to wireframe }
  InternalGlobalRenderOptions: TCastleRenderOptions;

implementation

uses SysUtils;

{$define read_implementation}
{$I castlerenderoptions_globals.inc}
{$I castlerenderoptions_renderoptions.inc}

initialization
  InternalGlobalRenderOptions := nil;

finalization
  FreeAndNil(InternalGlobalRenderOptions);

end.
