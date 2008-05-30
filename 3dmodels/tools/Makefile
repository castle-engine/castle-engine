ALL_OUTPUT := x3d_nodes_spec_to_pascal_preamble.inc

.PHONY: all
all: $(ALL_OUTPUT);

x3d_nodes_spec_to_pascal_preamble.inc: x3d_nodes_spec_to_pascal_preamble.txt
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
