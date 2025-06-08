# Use "make" to regenerate exported/ contents.

# ----------------------------------------------------------------------------
# Extend this section when adding new icons

# Generate single 32x32 PNG from these
SOURCE_FOR_32:=check.svg \
	heart-solid.svg \
	log-out.svg \
	save.svg \
	settings.svg \
	x.svg

# Generate single 48x48 PNG from these
SOURCE_FOR_48:=tool-interact.svg \
	tool-rotate.svg \
	tool-scale.svg \
	tool-select.svg \
	tool-translate.svg

# ----------------------------------------------------------------------------
# Implementation

OUTPUT_32:=$(patsubst %.svg,exported/%_32x32.png,$(SOURCE_FOR_32))

OUTPUT_48:=$(patsubst %.svg,exported/%_48x48.png,$(SOURCE_FOR_48))

OUTPUT_ALL:=$(OUTPUT_32) $(OUTPUT_48)

.PHONY: default
default: $(OUTPUT_ALL)

.PHONY: info
info:
	@echo "Makefile for generating PNG from SVG for icons"
	@echo "  Source SVG files for 32x32: $(SOURCE_FOR_32)"
	@echo "  Source SVG files for 48x48: $(SOURCE_FOR_48)"
	@echo "  Output PNG files: $(OUTPUT_ALL)"
	@echo "  To regenerate, run 'make'"

.PHONY: clean
clean:
	rm -Rf $(OUTPUT_ALL)

exported/%_32x32.png: %.svg
	inkscape --export-width=32 --export-height=32 $< --export-filename=$@
exported/%_48x48.png: %.svg
	inkscape --export-width=48 --export-height=48 $< --export-filename=$@
