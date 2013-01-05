.PHONY: all clean

ALL_UNITS=../castleonscreenmenuimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../castleonscreenmenuimages.pas: slider_base.png slider_position.png
	image_to_pas --output .. CastleOnScreenMenuImages $^
