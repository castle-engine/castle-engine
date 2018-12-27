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

{ TMX files processing unit. Based on Tiled v0.17. }
unit CastleTiledMap;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, DOM, XMLRead, base64, zstream, Generics.Collections,
  CastleVectors, CastleColors, CastleUtils, CastleURIUtils, CastleXMLUtils,
  CastleLog, CastleStringUtils, CastleUIControls, CastleGLImages;

{$define read_interface}
{$I castletiledmap_map.inc}
{$I castletiledmap_control.inc}
{$undef read_interface}

implementation

uses CastleComponentSerialize, CastleRectangles, CastleImages;

{$define read_implementation}
{$I castletiledmap_map.inc}
{$I castletiledmap_control.inc}
{$undef read_implementation}

initialization
  RegisterSerializableComponent(TCastleTiledMapControl, 'Tiled Map');
end.
