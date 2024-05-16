{
  Copyright 2002-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D fields (TX3DField and many descendants). }
unit X3DFields;

{$I castleconf.inc}

interface

uses Classes, SysUtils, DOM, Generics.Collections,
  CastleVectors, CastleInternalX3DLexer, CastleUtils, CastleClassUtils,
  CastleImages, CastleStringUtils, X3DTime, CastleColors, CastleQuaternions;

{$define read_interface}

type
  {$I castlefields_misctypes.inc}
  {$I castlefields_x3dwriter.inc}
  {$I castlefields_x3dreader.inc}
  {$I castlefields_x3dfileitem.inc}
  {$I castlefields_x3dfieldorevent.inc}
  {$I castlefields_x3dfield.inc}
  {$I castlefields_x3devent.inc}
  {$I castlefields_x3devent_descendants.inc}
  {$I castlefields_x3dsinglefield.inc}
  {$I castlefields_x3dsinglefield_descendants.inc}
  {$I castlefields_x3dmultfield.inc}
  {$I castlefields_x3dsimplemultfield.inc}
  {$I castlefields_x3dsimplemultfield_descendants.inc}
  {$I castlefields_x3dfieldsmanager.inc}

{$I castlefields_miscglobals.inc}

{$undef read_interface}

implementation

uses Math, Generics.Defaults,
  X3DNodes, CastleXmlUtils, CastleLog;

{$define read_implementation}

{$I castlefields_internalglobals.inc}

{$I castlefields_misctypes.inc}
{$I castlefields_x3dwriter.inc}
{$I castlefields_x3dreader.inc}
{$I castlefields_x3dfileitem.inc}
{$I castlefields_x3dfieldorevent.inc}
{$I castlefields_x3dfield.inc}
{$I castlefields_x3devent.inc}
{$I castlefields_x3devent_descendants.inc}
{$I castlefields_x3dsinglefield.inc}
{$I castlefields_x3dsinglefield_descendants.inc}
{$I castlefields_x3dmultfield.inc}
{$I castlefields_x3dsimplemultfield.inc}
{$I castlefields_x3dsimplemultfield_descendants.inc}
{$I castlefields_x3dfieldsmanager.inc}
{$I castlefields_miscglobals.inc}

initialization // empty but needed by Delphi

finalization
  FreeAndNil(FX3DFieldsManager);
end.
