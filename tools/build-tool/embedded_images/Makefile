.PHONY: all clean

ALL_UNITS=../toolembeddedimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../toolembeddedimages.pas: DefaultIcon.png
	image2pascal --output .. ToolEmbeddedImages DefaultIcon.png
