.PHONY: all clean

ALL_FILES=../imageslider_base.pas ../imageslider_position.pas

all: $(ALL_FILES)

clean:
	rm -f $(ALL_FILES)

../image%.pas: %.png
	image_to_pas $< > $@
