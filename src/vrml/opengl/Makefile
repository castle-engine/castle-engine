
# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.

ALL_OUTPUT := glsl_bump_mapping.vs.inc glsl_bump_mapping.fs.inc \
  glsl_parallax_bump_mapping.vs.inc glsl_parallax_bump_mapping.fs.inc

.PHONY: all
all: $(ALL_OUTPUT)

glsl_bump_mapping.vs.inc: glsl_bump_mapping.vs
	file_to_pascal_string $< $@

glsl_bump_mapping.fs.inc: glsl_bump_mapping.fs
	file_to_pascal_string $< $@

glsl_parallax_bump_mapping.vs.inc: glsl_parallax_bump_mapping.vs
	file_to_pascal_string $< $@

glsl_parallax_bump_mapping.fs.inc: glsl_parallax_bump_mapping.fs
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
