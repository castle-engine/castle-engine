{
  Copyright 2010-2018 Michalis Kamburelis.

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
  CastleColors, CastleProgress, CastleTimeUtils, CastleFontFamily, CastleGLUtils,
  CastleURIUtils, CastleLog, CastleStringUtils, CastleGLShaders;

type
  TCastleLabel = class;
  TCastleScrollView = class;

  TThemeImage = (
    tiPanel, tiPanelSeparator, tiProgressBar, tiProgressFill,
    tiButtonPressed, tiButtonDisabled, tiButtonFocused, tiButtonNormal,
    tiWindow, tiScrollbarFrame, tiScrollbarSlider,
    tiSlider, tiSliderPosition, tiLabel, tiGroup, tiActiveFrame, tiTooltip,
    tiTouchCtlInner, tiTouchCtlOuter, tiTouchCtlFlyInner, tiTouchCtlFlyOuter,
    tiCrosshair1, tiCrosshair2, tiCheckmark, tiDisclosure,
    tiSwitchControl, tiSwitchControlFill,

    { Image displayed when the application is initializing,
      during @link(TCastleApplication.OnInitialize Application.OnInitialize)
      and @link(TCastleWindowCustom.OnOpen Window.OnOpen).
      And @link(TUIControl.GLContextOpen) for all initially present UI controls.
      This "loading image" is loaded and displayed first,
      so that user does not see a black screen while the resources are prepared.

      It is especially useful on Android, where we can lose the OpenGL context
      at any moment, as user may switch applications in the middle of the game.
      When getting back to the application, we need to initiailize some
      resources, and during this process we also show this image.
      So this serves as a universal "please wait, we're loading" screen.

      You can customize this image, by setting
      @link(TCastleTheme.Images Theme.Images[tiLoading]),
      @link(TCastleTheme.LoadingBackgroundColor LoadingBackgroundColor),
      @link(TCastleTheme.LoadingTextColor LoadingTextColor).
      See https://castle-engine.io/tutorial_player_2d_controls.php
      for a sample code that sets a theme image.

      Note that the customization of this image should be done before
      @link(TCastleApplication.OnInitialize Application.OnInitialize) has
      started, so it has to be usually done from the "initialization" section
      of some unit. And in the "initialization" section of a unit,
      you cannot load files (doing @link(LoadImage) at this point may fail on
      some Android devices, as we cannot load assets before activity is started).
      So you can only assign images already available in code ---
      use image-to-pascal tool to convert any image to a Pascal code for this purpose. }
    tiLoading,

    { TCastleEdit frame. }
    tiEdit);

  {$define read_interface}
  {$I castlecontrols_uicontrolfont.inc}
  {$I castlecontrols_button.inc}
  {$I castlecontrols_panel.inc}
  {$I castlecontrols_imagecontrol.inc}
  {$I castlecontrols_touchcontrol.inc}
  {$I castlecontrols_rectanglecontrol.inc}
  {$I castlecontrols_shape.inc}
  {$I castlecontrols_simplebackground.inc}
  {$I castlecontrols_label.inc}
  {$I castlecontrols_crosshair.inc}
  {$I castlecontrols_progressbar.inc}
  {$I castlecontrols_sliders.inc}
  {$I castlecontrols_scrollview.inc}
  {$I castlecontrols_switchcontrol.inc}
  {$I castlecontrols_tableview.inc}
  {$I castlecontrols_timer.inc}
  {$I castlecontrols_edit.inc}
  {$I castlecontrols_groups.inc}
  // Add more UI controls include files here.

  // Keep the following (theme, uifont...) at the end, as they end the "type" clause.
  {$I castlecontrols_theme.inc}
  {$I castlecontrols_uifont.inc}
  {$I castlecontrols_clipboard.inc}
  {$undef read_interface}

procedure Register;

implementation

uses SysUtils, Math, CastleControlsImages, CastleTextureFont_DjvSans_20,
  CastleTextureFont_DejaVuSans_10, CastleTextureImages,
  CastleApplicationProperties, CastleMessaging;

{$define read_implementation}
{$I castlecontrols_uifont.inc} //< Keep this on top, to allow uicontrolfont.inc to access internals
{$I castlecontrols_uicontrolfont.inc}
{$I castlecontrols_button.inc}
{$I castlecontrols_panel.inc}
{$I castlecontrols_imagecontrol.inc}
{$I castlecontrols_touchcontrol.inc}
{$I castlecontrols_rectanglecontrol.inc}
{$I castlecontrols_shape.inc}
{$I castlecontrols_simplebackground.inc}
{$I castlecontrols_label.inc}
{$I castlecontrols_crosshair.inc}
{$I castlecontrols_progressbar.inc}
{$I castlecontrols_sliders.inc}
{$I castlecontrols_scrollview.inc}
{$I castlecontrols_switchcontrol.inc}
{$I castlecontrols_tableview.inc}
{$I castlecontrols_timer.inc}
{$I castlecontrols_edit.inc}
{$I castlecontrols_groups.inc}
{$I castlecontrols_theme.inc}
{$I castlecontrols_clipboard.inc}
{$undef read_implementation}

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleButton, TCastleImageControl, TCastleRectangleControl,
    TCastleLabel, TCastleCrosshair, TCastleIntegerSlider, TCastleFloatSlider,
    TCastleScrollView, TCastleSwitchControl]);
end;

initialization
  FTheme := TCastleTheme.Create;
finalization
  FreeAndNil(FTheme);
  FinalizationUIFonts;
  FinalizationClipboard;
end.
