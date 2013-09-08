.PHONY: all clean

ALL_UNITS=../castlecontrolsimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castlecontrolsimages.pas: Panel.png PanelSeparator.png \
  ButtonPressed.png ButtonFocused.png ButtonNormal.png \
  WindowDark.png WindowGray.png WindowDarkTransparent.png \
  ScrollbarFrame.png ScrollbarSlider.png Slider.png SliderPosition.png
	image_to_pas --output .. CastleControlsImages \
	  Panel.png WindowDarkTransparent.png Slider.png \
	  ButtonPressed.png ButtonFocused.png ButtonNormal.png \
	  @alpha=strip \
	  PanelSeparator.png WindowDark.png WindowGray.png \
	  ScrollbarFrame.png ScrollbarSlider.png \
	  SliderPosition.png
