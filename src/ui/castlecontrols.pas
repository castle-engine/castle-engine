{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Standard 2D controls: buttons, labels, sliders etc. }
unit CastleControls;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleVectors, CastleUIControls, CastleFonts, CastleTextureFontData,
  CastleKeysMouse, CastleImages, CastleUtils, CastleGLImages, CastleRectangles,
  CastleColors, CastleTimeUtils, CastleInternalRichText, CastleGLUtils,
  CastleUriUtils, CastleLog, CastleStringUtils, CastleGLShaders, CastleClassUtils,
  CastleRenderContext, CastleInternalFileMonitor;

type
  {$define read_interface}
  {$I castlecontrols_initial_types.inc} // this must be included first

  {$I castlecontrols_userinterfacefont.inc}
  {$I castlecontrols_button.inc}
  {$I castlecontrols_panel.inc}
  {$I castlecontrols_imagecontrol.inc}
  {$I castlecontrols_rectanglecontrol.inc}
  {$I castlecontrols_shape.inc}
  {$I castlecontrols_simplebackground.inc}
  {$I castlecontrols_label.inc}
  {$I castlecontrols_crosshair.inc}
  {$I castlecontrols_sliders.inc}
  {$I castlecontrols_scrollview.inc}
  {$I castlecontrols_switchcontrol.inc}
  {$I castlecontrols_checkbox.inc}
  {$I castlecontrols_timer.inc}
  {$I castlecontrols_edit.inc}
  {$I castlecontrols_groups.inc}
  {$I castlecontrols_design.inc}
  {$I castlecontrols_mask.inc}
  // Add more UI controls include files here.

  // Keep the following (uifont...) at the end, as they end the "type" clause.
  {$I castlecontrols_uifont.inc}
  {$I castlecontrols_clipboard.inc}
  {$undef read_interface}

implementation

uses Math, CastleTextureFont_DefaultUi,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleTextureImages,
  CastleApplicationProperties, CastleMessaging, CastleComponentSerialize,
  CastleUnicode, CastleRenderOptions;

{$define read_implementation}
{$I castlecontrols_uifont.inc} //< Keep this on top, to allow castlecontrols_userinterfacefont.inc to access internals
{$I castlecontrols_userinterfacefont.inc}
{$I castlecontrols_button.inc}
{$I castlecontrols_panel.inc}
{$I castlecontrols_imagecontrol.inc}
{$I castlecontrols_rectanglecontrol.inc}
{$I castlecontrols_shape.inc}
{$I castlecontrols_simplebackground.inc}
{$I castlecontrols_label.inc}
{$I castlecontrols_crosshair.inc}
{$I castlecontrols_sliders.inc}
{$I castlecontrols_scrollview.inc}
{$I castlecontrols_switchcontrol.inc}
{$I castlecontrols_checkbox.inc}
{$I castlecontrols_timer.inc}
{$I castlecontrols_edit.inc}
{$I castlecontrols_groups.inc}
{$I castlecontrols_design.inc}
{$I castlecontrols_mask.inc}
{$I castlecontrols_clipboard.inc}
{$undef read_implementation}

initialization
  RegisterSerializableComponent(TCastleButton, 'Button');
  RegisterSerializableComponent(TCastleImageControl, 'Image');
  RegisterSerializableComponent(TCastleRectangleControl, 'Color Rectangle');
  RegisterSerializableComponent(TCastleLabel, 'Label');
  RegisterSerializableComponent(TCastleShape, 'Shape');
  RegisterSerializableComponent(TCastleIntegerSlider, 'Integer Slider');
  RegisterSerializableComponent(TCastleFloatSlider, 'Float Slider');
  RegisterSerializableComponent(TCastleTimer, 'Timer');
  RegisterSerializableComponent(TCastleEdit, 'Edit');
  RegisterSerializableComponent(TCastleFloatEdit, 'Edit (Float)');
  RegisterSerializableComponent(TCastleIntegerEdit, 'Edit (Integer)');
  RegisterSerializableComponent(TCastleVerticalGroup, 'Vertical Group');
  RegisterSerializableComponent(TCastleHorizontalGroup, 'Horizontal Group');
  RegisterSerializableComponent(TCastleCrosshair, 'Crosshair');
  RegisterSerializableComponent(TCastleScrollView, 'Scroll View');
  RegisterSerializableComponent(TCastleScrollViewManual, 'Scroll View Manual');
  RegisterSerializableComponent(TCastleCheckbox, 'Checkbox');
  RegisterSerializableComponent(TCastleDesign, 'Design (Use Another castle-user-interface File)');
  RegisterSerializableComponent(TCastleMask, 'Mask');
finalization
  FinalizationUIFonts;
  FinalizationClipboard;
end.
