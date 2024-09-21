ALL_EXPORTED := $(wildcard exported/*.x3dv) $(wildcard exported/*.glb)

ALL_OUTPUT := $(addsuffix .inc, \
  $(subst exported/,generated-pascal/, \
  $(ALL_EXPORTED)))

.PHONY: all
all: $(ALL_OUTPUT)

.PHONY: info
info:
	@echo "ALL_EXPORTED: $(ALL_EXPORTED)"
	@echo "ALL_OUTPUT: $(ALL_OUTPUT)"

generated-pascal/%.inc: exported/%
	file_to_pascal_data $< $@

.PHONY: clean
clean:
#	rm -f $(ALL_OUTPUT)
# Clean all *.inc files, in case we renamed some files in source/
	rm -f generated-pascal/*.inc
