.PHONY: all clean

ALL_UNITS=../toolembeddedimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../toolembeddedimages.pas: DefaultIcon.png
	image-to-pascal --output .. ToolEmbeddedImages DefaultIcon.png
