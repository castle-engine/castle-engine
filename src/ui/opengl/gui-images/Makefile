.PHONY: all clean

ALL_UNITS=../castlecontrolsimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castlecontrolsimages.pas: panel.png panel_separator.png \
  button_pressed.png button_focused.png button_normal.png \
  WindowDark.png WindowGray.png WindowDarkTransparent.png \
  ScrollbarFrame.png ScrollbarSlider.png \
  Slider.png SliderPosition.png
	image_to_pas --output .. CastleControlsImages \
	  panel.png WindowDarkTransparent.png Slider.png \
	  @alpha=strip \
	  panel_separator.png button_pressed.png button_focused.png \
	  button_normal.png WindowDark.png WindowGray.png \
	  ScrollbarFrame.png ScrollbarSlider.png \
	  SliderPosition.png
