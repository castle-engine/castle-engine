# Use this Makefile to generate HTML files
# for inclusion in the HTML API documentation.
#
# It requires
# - command-line php
# - cge-www repository (https://github.com/castle-engine/cge-www)
#   cloned and placed as a sibling to castle-engine, in ../../../../cge-www/

ALL_TARGETS := body-end.html body-begin.html head.html

.PHONY: default
default: $(ALL_TARGETS)

.PHONY: clean
clean:
	rm -f $(ALL_TARGETS)

%.html: %.php
	php $< > $@
