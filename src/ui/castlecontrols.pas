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

uses Classes, Generics.Collections,
  CastleVectors, CastleUIControls, CastleFonts, CastleTextureFontData,
  CastleKeysMouse, CastleImages, CastleUtils, CastleGLImages, CastleRectangles,
  CastleColors, CastleTimeUtils, CastleInternalRichText, CastleGLUtils,
  CastleURIUtils, CastleLog, CastleStringUtils, CastleGLShaders, CastleClassUtils,
  CastleRenderContext;

type
  {$define read_interface}
  {$I castlecontrols_initial_types.inc} // this must be included first

  {$I castlecontrols_userinterfacefont.inc}
  {$I castlecontrols_button.inc}
  {$I castlecontrols_panel.inc}
  {$I castlecontrols_imagecontrol.inc}
  {$I castlecontrols_touchcontrol.inc}
  {$I castlecontrols_rectanglecontrol.inc}
  {$I castlecontrols_shape.inc}
  {$I castlecontrols_simplebackground.inc}
  {$I castlecontrols_label.inc}
  {$I castlecontrols_crosshair.inc}
  {$I castlecontrols_sliders.inc}
  {$I castlecontrols_scrollview.inc}
  {$I castlecontrols_switchcontrol.inc}
  {$I castlecontrols_checkbox.inc}
  {$I castlecontrols_tableview.inc}
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

uses SysUtils, Math, CastleTextureFont_DjvSans_20,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleTextureFont_DejaVuSans_10, CastleTextureImages,
  CastleApplicationProperties, CastleMessaging, CastleComponentSerialize,
  CastleUnicode;

{$define read_implementation}
{$I castlecontrols_uifont.inc} //< Keep this on top, to allow castlecontrols_userinterfacefont.inc to access internals
{$I castlecontrols_userinterfacefont.inc}
{$I castlecontrols_button.inc}
{$I castlecontrols_panel.inc}
{$I castlecontrols_imagecontrol.inc}
{$I castlecontrols_touchcontrol.inc}
{$I castlecontrols_rectanglecontrol.inc}
{$I castlecontrols_shape.inc}
{$I castlecontrols_simplebackground.inc}
{$I castlecontrols_label.inc}
{$I castlecontrols_crosshair.inc}
{$I castlecontrols_sliders.inc}
{$I castlecontrols_scrollview.inc}
{$I castlecontrols_switchcontrol.inc}
{$I castlecontrols_checkbox.inc}
{$I castlecontrols_tableview.inc}
{$I castlecontrols_timer.inc}
{$I castlecontrols_edit.inc}
{$I castlecontrols_groups.inc}
{$I castlecontrols_design.inc}
{$I castlecontrols_mask.inc}
{$I castlecontrols_clipboard.inc}
{$undef read_implementation}

var
  R: TRegisteredComponent;
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

  R := TRegisteredComponent.Create;
  {$warnings off} // using deprecated, to keep reading it from castle-user-interface working
  R.ComponentClass := TCastleSwitchControl;
  {$warnings on}
  R.Caption := ['Switch'];
  R.IsDeprecated := true;
  RegisterSerializableComponent(R);
finalization
  FinalizationUIFonts;
  FinalizationClipboard;
end.
