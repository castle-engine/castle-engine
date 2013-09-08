.PHONY: all clean

ALL_UNITS=../castlecontrolsimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castlecontrolsimages.pas: Panel.png PanelSeparator.png \
  Tooltip.png TooltipRounded.png \
  ButtonPressed.png ButtonFocused.png ButtonNormal.png \
  WindowDark.png WindowGray.png WindowDarkTransparent.png \
  ScrollbarFrame.png ScrollbarSlider.png Slider.png SliderPosition.png \
  FrameWhite.png FrameWhiteBlack.png FrameYellow.png FrameYellowBlack.png \
  FrameThickWhite.png FrameThickYellow.png
	image_to_pas --output .. CastleControlsImages \
	  Panel.png WindowDarkTransparent.png Slider.png \
	  Tooltip.png TooltipRounded.png \
	  ButtonPressed.png ButtonFocused.png ButtonNormal.png \
	  FrameWhite.png FrameWhiteBlack.png FrameYellow.png FrameYellowBlack.png \
	  FrameThickWhite.png FrameThickYellow.png \
	  @alpha=strip \
	  PanelSeparator.png WindowDark.png WindowGray.png \
	  ScrollbarFrame.png ScrollbarSlider.png \
	  SliderPosition.png
