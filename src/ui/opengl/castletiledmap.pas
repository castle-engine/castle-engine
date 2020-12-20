{
  Copyright 2015-2018 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and rendering maps created in Tiled (https://www.mapeditor.org/).

  In the usual case,
  create @link(TCastleTiledMapControl) and set @link(TCastleTiledMapControl.URL)
  to indicate your Tiled map (TMX file).
  @link(TCastleTiledMapControl) is a standard Castle Game Engine user interface
  control (descendant of @link(TCastleUserInterface)), you can display it
  like any other UI control, you can also design it using CGE Editor.

  See the examples/tiled/ directory of Castle Game Engine
  ( https://github.com/castle-engine/castle-engine/tree/master/examples/tiled ). }
unit CastleTiledMap;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, DOM, XMLRead, base64, zstream, Generics.Collections,
  CastleVectors, CastleColors, CastleUtils, CastleURIUtils, CastleXMLUtils,
  CastleLog, CastleStringUtils, CastleUIControls, CastleGLImages,
  CastleRectangles, CastleClassUtils, CastleRenderOptions;

{$define read_interface}
{$I castletiledmap_map.inc}
{$I castletiledmap_control.inc}
{$undef read_interface}

implementation

uses Math,
  CastleComponentSerialize, CastleImages,
  CastleGLUtils;

{$define read_implementation}
{$I castletiledmap_map.inc}
{$I castletiledmap_control.inc}
{$undef read_implementation}

initialization
  RegisterSerializableComponent(TCastleTiledMapControl, 'Tiled Map');
end.
