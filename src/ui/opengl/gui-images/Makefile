.PHONY: all clean

ALL_UNITS=../glmenuimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../glmenuimages.pas: slider_base.png slider_position.png
	image_to_pas --output .. GLMenuImages $^
