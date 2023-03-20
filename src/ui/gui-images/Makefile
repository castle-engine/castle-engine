.PHONY: all clean

ALL_UNITS=../castleinternalcontrolsimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castleinternalcontrolsimages.pas: Panel.png PanelSeparator.png \
  Tooltip.png TooltipRounded.png \
  ButtonPressed.png ButtonDisabled.png ButtonFocused.png ButtonNormal.png \
  WindowDark.png WindowGray.png WindowDarkTransparent.png \
  ScrollbarFrame.png ScrollbarSlider.png SliderBackground.png SliderThumb.png \
  FrameWhite.png FrameWhiteBlack.png FrameYellow.png FrameYellowBlack.png \
  FrameThickWhite.png FrameThickYellow.png \
  ProgressBar.png ProgressFill.png \
  TouchCtlInner.png TouchCtlOuter.png TouchCtlFlyInner.png TouchCtlFlyOuter.png \
  Crosshair1.png Crosshair2.png ScrollbarSlider.png \
  Checkmark.png Disclosure.png SquareEmpty.png SquarePressedBackground.png SquareChecked.png \
  Loading.png Edit.png
	image-to-pascal --output .. CastleInternalControlsImages \
	  Panel.png WindowDarkTransparent.png \
	  Tooltip.png TooltipRounded.png \
	  ButtonPressed.png ButtonDisabled.png ButtonFocused.png ButtonNormal.png \
	  FrameWhite.png FrameWhiteBlack.png FrameYellow.png FrameYellowBlack.png \
	  FrameThickWhite.png FrameThickYellow.png ProgressBar.png ProgressFill.png \
	  TouchCtlInner.png TouchCtlOuter.png TouchCtlFlyInner.png TouchCtlFlyOuter.png \
	  Loading.png Crosshair1.png Crosshair2.png ScrollbarSlider.png \
	  Checkmark.png Disclosure.png SquareEmpty.png SquarePressedBackground.png SquareChecked.png \
	  @alpha=keep-and-bleed \
	  SliderBackground.png SliderThumb.png \
	  @alpha=strip \
	  PanelSeparator.png WindowDark.png WindowGray.png \
	  ScrollbarFrame.png Edit.png
