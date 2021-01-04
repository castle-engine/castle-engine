{%MainUnit castlecontrols.pas}
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

{$ifdef read_interface}
  TCastleLabel = class;
  TCastleScrollView = class;

  TThemeImage = (
    tiPanel, tiPanelSeparator, tiProgressBar, tiProgressFill,
    tiButtonPressed, tiButtonDisabled, tiButtonFocused, tiButtonNormal,
    tiWindow, tiScrollbarFrame, tiScrollbarSlider,
    tiSlider, tiSliderPosition, tiLabel, tiGroup, tiActiveFrame, tiTooltip,
    tiTouchCtlInner, tiTouchCtlOuter, tiTouchCtlFlyInner, tiTouchCtlFlyOuter,
    tiCrosshair1, tiCrosshair2, tiCheckmark,
    tiSquareEmpty, tiSquareChecked, tiSquarePressedBackground,
    tiDisclosure,
    tiSwitchControl, tiSwitchControlFill,

    { Image displayed when the application is initializing,
      during @link(TCastleApplication.OnInitialize Application.OnInitialize)
      and @link(TCastleWindowBase.OnOpen Window.OnOpen).
      And @link(TCastleUserInterface.GLContextOpen) for all initially present UI controls.
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
      See https://castle-engine.io/manual_2d_user_interface.php#section_theme
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
{$endif read_interface}
