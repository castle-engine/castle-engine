# This requires file_to_pascal_string that is a trivial program
# from pasdoc project, see
# https://pasdoc.github.io/
# https://github.com/pasdoc/pasdoc/blob/master/source/tools/file_to_pascal_string.dpr

.PHONY: all clean

ALL_OUTPUT=inspector_ui.castle-user-interface.inc \
  components_hierarchy.castle-user-interface.inc \
  component_properties.castle-user-interface.inc

all: $(ALL_OUTPUT)

clean:
	rm -f $(ALL_OUTPUT)

%.inc: %
	file_to_pascal_string $< $@
