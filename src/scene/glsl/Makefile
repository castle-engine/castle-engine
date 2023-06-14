# Generate Pascal include files from all GLSL code in source/ .
#
# Uses file_to_pascal_string, which is part of PasDoc. See:
# - PasDoc main webpage:
#   https://github.com/pasdoc/pasdoc/wiki
# - file_to_pascal_string trivial source code (you can just get this one file and compile it yourself):
#   https://github.com/pasdoc/pasdoc/blob/master/source/tools/file_to_pascal_string.dpr
#
# Placing source/ and generated-pascal/ in separate subdirectories makes it easier
# to browse, and to grep inside GLSL files, since you can easily grep only within source/ .
#
# Note that we flatten directory structure in generated-pascal/,
# to avoid the need to specify many include paths.
# It just seems easier to extend and reorganize the source/ directory freely this way.

ifneq (,$(filter $(OS),Windows_NT win64))
  $(info Detected OS: Windows)
  # Hack for Cygwin, to avoid using Windows built-in "find" program.
  FIND:=`cygpath --mixed /bin/find`
else
  $(info Detected OS: not Windows)
  FIND:=find
endif

ALL_SOURCES := $(shell $(FIND) source/ \
  -iname '*.fs' -or \
  -iname '*.vs' -or \
  -iname '*.gs' -or \
  -iname '*.glsl')
ALL_OUTPUT := $(addsuffix .inc, \
  $(subst lighting_model_phong/,lighting_model_phong_, \
  $(subst lighting_model_physical/,lighting_model_physical_, \
  $(subst lighting_model_unlit/,lighting_model_unlit_, \
  $(subst source/,generated-pascal/, \
  $(ALL_SOURCES))))))

.PHONY: all
all: $(ALL_OUTPUT)

generated-pascal/%.inc: source/%
	file_to_pascal_string $< $@
generated-pascal/lighting_model_phong_%.inc: source/lighting_model_phong/%
	file_to_pascal_string $< $@
generated-pascal/lighting_model_physical_%.inc: source/lighting_model_physical/%
	file_to_pascal_string $< $@
generated-pascal/lighting_model_unlit_%.inc: source/lighting_model_unlit/%
	file_to_pascal_string $< $@

.PHONY: clean
clean:
#	rm -f $(ALL_OUTPUT)
# Clean all *.inc files, in case we renamed some files in source/
	rm -f generated-pascal/*.inc
