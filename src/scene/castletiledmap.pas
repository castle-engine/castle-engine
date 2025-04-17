﻿{
  Copyright 2015-2024 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and rendering maps created in Tiled.
  See @url(https://castle-engine.io/tiled_maps manual about using Tiled maps)
  for more details.

  Most basic usage is to add @link(TCastleTiledMap) component to a viewport
  and set @link(TCastleTiledMap.URL) to load your Tiled map (TMX file).
  You can test it using CGE editor.

  See the examples in Castle Game Engine:
  @url(https://github.com/castle-engine/castle-engine/tree/master/examples/tiled examples/tiled/). }
unit CastleTiledMap;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, DOM, XMLRead, base64,
  {$ifdef FPC} zstream {$else} { from Vampyre } DZLib {$endif},
  Generics.Collections, CastleInternalFileMonitor,
  CastleVectors, CastleColors, CastleUtils, CastleUriUtils, CastleXmlUtils,
  CastleLog, CastleStringUtils, CastleUIControls, CastleGLImages, CastleTransform,
  CastleRectangles, CastleClassUtils, CastleRenderOptions, CastleScene, X3DNodes;

{$define read_interface}
{$I castletiledmap_data.inc}
{$I castletiledmap_control.inc}
{$I castletiledmap_scene.inc}
{$undef read_interface}

const
  LoadTiledMap_FileFilters = 'Tiled Map (*.tmx)|*.tmx|All Files|*';

implementation

uses Math,
  CastleComponentSerialize, CastleImages, CastleRenderContext,
  X3DLoadInternalTiledMap, CastleGLUtils, CastleDownload;

{$define read_implementation}
{$I castletiledmap_data.inc}
{$I castletiledmap_control.inc}
{$I castletiledmap_scene.inc}
{$undef read_implementation}

var
  R: TRegisteredComponent;
initialization
  R := TRegisteredComponent.Create;
  {$warnings off} // using deprecated, to keep reading it from castle-user-interface working
  R.ComponentClass := TCastleTiledMapControl;
  {$warnings on}
  R.Caption := ['Tiled Map Control'];
  R.IsDeprecated := true;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleTiledMap, 'Tiled Map');
end.
