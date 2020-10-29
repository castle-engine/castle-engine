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

ALL_SOURCES := $(wildcard source/*.fs) \
               $(wildcard source/*.vs) \
	       $(wildcard source/*.gs) \
	       $(wildcard source/*.glsl)
ALL_OUTPUT := $(addsuffix .inc, $(subst source/,generated-pascal/,$(ALL_SOURCES)))

.PHONY: all
all: $(ALL_OUTPUT)

generated-pascal/%.inc: source/%
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
