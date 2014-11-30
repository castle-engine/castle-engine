.PHONY: all clean

ALL_UNITS=../toolembeddedimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../toolembeddedimages.pas: DefaultIcon.png
	image_to_pas --output .. ToolEmbeddedImages DefaultIcon.png
