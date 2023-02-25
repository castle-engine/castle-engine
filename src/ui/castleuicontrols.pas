{
  Copyright 2009-2023 Michalis Kamburelis, Tomasz Wojtyś.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ User interface basic classes: @link(TCastleUserInterface), @link(TCastleContainer). }
unit CastleUIControls;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleKeysMouse, CastleUtils, CastleClassUtils, CastleGLUtils, CastleFonts,
  CastleRectangles, CastleTimeUtils, CastleInternalPk3DConnexion, CastleColors,
  CastleImages, CastleVectors, CastleJoysticks, CastleApplicationProperties,
  CastleGLImages, CastleRenderContext, CastleComponentSerialize;

{$define read_interface}

{$I castleuicontrols_initial_constants.inc}

type
  {$I castleuicontrols_initial_types.inc}
  {$I castleuicontrols_touchlist.inc}
  {$I castleuicontrols_inputinspector.inc}
  {$I castleuicontrols_container.inc}
  {$I castleuicontrols_userinterface.inc}
  {$I castleuicontrols_userinterfacelist.inc}
  {$I castleuicontrols_view.inc}
  {$I castleuicontrols_internalchildrencontrols.inc}
  {$I castleuicontrols_deprecated.inc}
  {$I castleuicontrols_theme.inc} // ends the "type" clause

{$I castleuicontrols_serialize.inc}
{$I castleuicontrols_miscellaneous_globals.inc}

{$undef read_interface}

implementation

uses DOM, TypInfo, Math,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleLog, CastleXMLUtils, CastleStringUtils,
  CastleInternalSettings, CastleFilesUtils, CastleURIUtils, CastleRenderOptions,
  CastleInternalInspector, CastleInternalControlsImages, CastleInternalGLUtils;

{$define read_implementation}
{$I castleuicontrols_initial_types.inc}
{$I castleuicontrols_touchlist.inc}
{$I castleuicontrols_inputinspector.inc}
{$I castleuicontrols_container.inc}
{$I castleuicontrols_userinterface.inc}
{$I castleuicontrols_userinterfacelist.inc}
{$I castleuicontrols_view.inc}
{$I castleuicontrols_internalchildrencontrols.inc}
{$I castleuicontrols_deprecated.inc}
{$I castleuicontrols_theme.inc}
{$I castleuicontrols_serialize.inc}
{$I castleuicontrols_miscellaneous_globals.inc}
{$undef read_implementation}

initialization
  InitializationTheme;
  RegisterSerializableComponent(TCastleUserInterface, 'Empty Rectangle');
finalization
  FinalizationTheme;
end.
