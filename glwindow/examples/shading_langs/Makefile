ALL_TARGETS := vertex_program_from_cg_sample.txt fragment_program_from_cg_brighter.txt

.PHONY: default
default: $(ALL_TARGETS)

vertex_program_from_cg_sample.txt: cg_sample.cg
	cgc $< -profile arbvp1 -o $@

fragment_program_from_cg_brighter.txt: cg_brighter.cg
	cgc $< -profile arbfp1 -entry brighter  -o $@

.PHONY: clean
clean:
	rm -f $(ALL_TARGETS)
