# Use "make" to regenerate exported/ contents.

# ----------------------------------------------------------------------------
# Extend this section when adding new icons

# Generate single 32x32 PNG from these
SOURCE_FOR_32:=$(wildcard *.svg)

# ----------------------------------------------------------------------------
# Implementation

OUTPUT_32:=$(patsubst %.svg,exported/%_32x32.png,$(SOURCE_FOR_32))

OUTPUT_ALL:=$(OUTPUT_32)

.PHONY: default
default: $(OUTPUT_ALL)

.PHONY: info
info:
	@echo "Makefile for generating PNG from SVG for icons"
	@echo "  Source SVG files: $(SOURCE_FOR_32)"
	@echo "  Output PNG files: $(OUTPUT_ALL)"
	@echo "  To regenerate, run 'make'"

.PHONY: clean
clean:
	rm -Rf $(OUTPUT_ALL)

exported/%_32x32.png: %.svg
	inkscape --export-width=32 --export-height=32 $< --export-filename=$@
