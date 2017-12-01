.PHONY: all clean

ALL_UNITS=toolembeddedimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

toolembeddedimages.pas: DefaultIcon.png DefaultIconSquare.png
	image-to-pascal --output .. ToolEmbeddedImages DefaultIcon.png \
	@alpha=strip DefaultIconSquare.png
