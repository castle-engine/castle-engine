# This requires file_to_pascal_string that is a trivial program
# from pasdoc project, see
# https://github.com/pasdoc/pasdoc/wiki
# https://github.com/pasdoc/pasdoc/blob/master/source/tools/file_to_pascal_string.dpr

ALL_TARGETS := \
  windows/castle-automatic-windows.manifest.inc \
  template-castle-engine-output-warning.txt.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

%.inc: %
	file_to_pascal_string $< $@
