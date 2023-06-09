# Use "make" to regenerate exported/ contents.

# ----------------------------------------------------------------------------
# Extend this section when adding new icons

# Generate single 24x24 PNG from these
SOURCE_FOR_24:=folder.svg \
  trash-2.svg

# Generate single 32x32 PNG from these
SOURCE_FOR_32:=alert-triangle.svg \
  border-all.svg \
  edit.svg \
  heart-solid.svg \
  menu.svg \
  settings.svg

# Generate multiple PNG sizes from these
SOURCE_FOR_MULTIPLE_SIZES:=pause-solid.svg \
  play-solid.svg   \
  stop-solid.svg \
  pause-solid-physics.svg \
  play-solid-physics.svg \
  stop-solid-physics.svg \
  file-plus.svg \
  info-solid.svg \
  tool-translate.svg \
  tool-rotate.svg \
  tool-scale.svg \
  tool-select.svg \
  tool-interact.svg \
  list.svg \
  grid.svg

# ----------------------------------------------------------------------------
# Implementation

OUTPUT_24:=$(patsubst %.svg,exported/%_24x24.png,$(SOURCE_FOR_24))

OUTPUT_32:=$(patsubst %.svg,exported/%_32x32.png,$(SOURCE_FOR_32))

OUTPUT_MULTIPLE_SIZES:=\
  $(patsubst %.svg,exported/%_16x16.png,$(SOURCE_FOR_MULTIPLE_SIZES)) \
  $(patsubst %.svg,exported/%_20x20.png,$(SOURCE_FOR_MULTIPLE_SIZES)) \
  $(patsubst %.svg,exported/%_24x24.png,$(SOURCE_FOR_MULTIPLE_SIZES)) \
  $(patsubst %.svg,exported/%_32x32.png,$(SOURCE_FOR_MULTIPLE_SIZES)) \
  $(patsubst %.svg,exported/%_48x48.png,$(SOURCE_FOR_MULTIPLE_SIZES))

OUTPUT_ALL:=$(OUTPUT_24) $(OUTPUT_32) $(OUTPUT_MULTIPLE_SIZES)

.PHONY: default
default: $(OUTPUT_ALL)

.PHONY: clean
clean:
	rm -Rf $(OUTPUT_ALL)

exported/%_16x16.png: %.svg
	inkscape --export-width=16 --export-height=16 $< --export-filename=$@
exported/%_20x20.png: %.svg
	inkscape --export-width=20 --export-height=20 $< --export-filename=$@
exported/%_24x24.png: %.svg
	inkscape --export-width=24 --export-height=24 $< --export-filename=$@
exported/%_32x32.png: %.svg
	inkscape --export-width=32 --export-height=32 $< --export-filename=$@
exported/%_48x48.png: %.svg
	inkscape --export-width=48 --export-height=48 $< --export-filename=$@
