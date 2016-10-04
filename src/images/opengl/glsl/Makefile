# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.

ALL_OUTPUT := image.fs.inc image.vs.inc primitive_2.fs.inc primitive_2.vs.inc

.PHONY: all
all: $(ALL_OUTPUT)

%.inc: %
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
