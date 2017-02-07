.PHONY: all clean

ALL_UNITS=../castlecontrolsimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castlecontrolsimages.pas: Panel.png PanelSeparator.png \
  Tooltip.png TooltipRounded.png \
  ButtonPressed.png ButtonDisabled.png ButtonFocused.png ButtonNormal.png \
  WindowDark.png WindowGray.png WindowDarkTransparent.png \
  ScrollbarFrame.png ScrollbarSlider.png Slider.png SliderPosition.png \
  FrameWhite.png FrameWhiteBlack.png FrameYellow.png FrameYellowBlack.png \
  FrameThickWhite.png FrameThickYellow.png \
  ProgressBar.png ProgressFill.png \
  TouchCtlInner.png TouchCtlOuter.png TouchCtlFlyInner.png TouchCtlFlyOuter.png \
  Crosshair1.png Crosshair2.png ErrorBackground.png Loading.png
	image-to-pascal --output .. CastleControlsImages \
	  Panel.png WindowDarkTransparent.png Slider.png \
	  Tooltip.png TooltipRounded.png \
	  ButtonPressed.png ButtonDisabled.png ButtonFocused.png ButtonNormal.png \
	  FrameWhite.png FrameWhiteBlack.png FrameYellow.png FrameYellowBlack.png \
	  FrameThickWhite.png FrameThickYellow.png ProgressBar.png ProgressFill.png \
	  TouchCtlInner.png TouchCtlOuter.png TouchCtlFlyInner.png TouchCtlFlyOuter.png \
	  Loading.png Crosshair1.png Crosshair2.png ScrollbarSlider.png \
	  @alpha=strip \
	  PanelSeparator.png WindowDark.png WindowGray.png \
	  ScrollbarFrame.png \
	  SliderPosition.png ErrorBackground.png
