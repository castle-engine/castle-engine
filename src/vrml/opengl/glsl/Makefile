# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.

ALL_OUTPUT := bump_mapping.vs.inc bump_mapping.fs.inc \
  parallax_bump_mapping.vs.inc parallax_bump_mapping.fs.inc \
  shadow_map_1.fs.inc shadow_map_0.fs.inc shadow_map_0_show_depth.fs.inc shadow_map_1_show_depth.fs.inc shadow_map_common.fs.inc \
  variance_shadow_map_1.fs.inc variance_shadow_map_0.fs.inc variance_shadow_map_0_show_depth.fs.inc variance_shadow_map_1_show_depth.fs.inc variance_shadow_map_common.fs.inc \
  variance_shadow_map_generate.fs.inc \
  template.vs.inc template.fs.inc

.PHONY: all
all: $(ALL_OUTPUT)

%.inc: %
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
