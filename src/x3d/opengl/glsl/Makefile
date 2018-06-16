# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.

ALL_OUTPUT := common.fs.inc \
              common.vs.inc \
              shadow_map_common.fs.inc \
	      shadow_map_generate.fs.inc \
	      shadow_map_generate.vs.inc \
	      variance_shadow_map_common.fs.inc \
	      template_phong.vs.inc \
	      template_phong.fs.inc \
	      template.gs.inc \
	      ssao.glsl.inc \
	      screen_effect_library.glsl.inc \
	      screen_effect.vs.inc \
	      template_gouraud.vs.inc \
	      template_gouraud.fs.inc \
	      template_light.glsl.inc \
	      fallback.vs.inc \
	      fallback.fs.inc \
	      simplest.vs.inc \
	      simplest.fs.inc \
	      bump_mapping.fs.inc \
	      bump_mapping.vs.inc \
	      bump_mapping_parallax.fs.inc \
	      bump_mapping_parallax.vs.inc \
	      bump_mapping_steep_parallax_shadowing.fs.inc \
	      bump_mapping_steep_parallax_shadowing.vs.inc

.PHONY: all
all: $(ALL_OUTPUT)

%.inc: %
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
