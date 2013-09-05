.PHONY: all clean

ALL_UNITS=../castleonscreenmenuimages.pas ../castlecontrolsimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castleonscreenmenuimages.pas: slider_base.png slider_position.png
	image_to_pas --output .. CastleOnScreenMenuImages $^

../castlecontrolsimages.pas: panel.png panel_separator.png \
  button_pressed.png button_focused.png button_normal.png WindowDark.png \
  WindowGray.png WindowDarkTransparent.png
	image_to_pas --output .. CastleControlsImages \
	panel.png WindowDarkTransparent.png \
	@alpha=strip \
	panel_separator.png button_pressed.png button_focused.png \
	button_normal.png WindowDark.png WindowGray.png
