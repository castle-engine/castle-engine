# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.

ALL_OUTPUT := shadow_map_common.fs.inc \
  variance_shadow_map_common.fs.inc \
  variance_shadow_map_generate.fs.inc \
  template.vs.inc template.fs.inc template.gs.inc \
  template_add_light.glsl.inc \
  ssao.glsl.inc screen_effect_library.glsl.inc \
  template_mobile.vs.inc template_mobile.fs.inc template_mobile_add_light.glsl.inc

.PHONY: all
all: $(ALL_OUTPUT)

%.inc: %
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
