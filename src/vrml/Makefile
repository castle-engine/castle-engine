ALL_OUTPUT := shadow_map_1.fs.inc shadow_map_0.fs.inc \
  shadow_map_1_show_depth.fs.inc

.PHONY: all
all: $(ALL_OUTPUT)

%.fs.inc: %.fs
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
