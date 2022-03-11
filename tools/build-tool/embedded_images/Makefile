.PHONY: all clean

ALL_OUTPUTS=toolembeddedimages.pas tooldefaulticonxpm.inc

all: $(ALL_OUTPUTS)

clean:
	rm -f $(ALL_OUTPUTS)

toolembeddedimages.pas: DefaultIcon.png DefaultIconSquare.png
	image-to-pascal --output .. ToolEmbeddedImages DefaultIcon.png \
	@alpha=strip DefaultIconSquare.png

tooldefaulticonxpm.inc: DefaultIcon.xpm
	file_to_pascal_string $< $@
