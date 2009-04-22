# This requires file_to_pascal_string that is a trivial program
# that I made for pasdoc [http://pasdoc.sf.net/] project, see
# [https://pasdoc.svn.sourceforge.net/svnroot/pasdoc/trunk/source/tools/file_to_pascal_string.dpr]

ALL_OUTPUT := x3d_nodes_spec_to_pascal_preamble.inc \
              xmlportals_to_x3d_suffix.inc \
	      kanim_to_interpolators_suffix.inc

.PHONY: all
all: $(ALL_OUTPUT);

x3d_nodes_spec_to_pascal_preamble.inc: x3d_nodes_spec_to_pascal_preamble.txt
	file_to_pascal_string $< $@

xmlportals_to_x3d_suffix.inc: xmlportals_to_x3d_suffix.x3dv
	file_to_pascal_string $< $@

kanim_to_interpolators_suffix.inc: kanim_to_interpolators_suffix.wrl
	file_to_pascal_string $< $@

.PHONY: clean
clean:
	rm -f $(ALL_OUTPUT)
