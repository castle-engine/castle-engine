# Use this Makefile to generate HTML files
# for inclusion in the HTML API documentation.
#
# It requires
# - command-line php
# - cge-www repository (https://github.com/castle-engine/cge-www)
#   cloned and placed as a sibling to castle-engine, in ../../../../cge-www/

ALL_TARGETS := body-end.html body-begin.html head.html images castle-engine-website-base

.PHONY: default
default: $(ALL_TARGETS)

.PHONY: clean
clean:
	rm -Rf $(ALL_TARGETS)

%.html: %.php
	php $< > $@

# Copy some static things (images, CSS, JS files) from cge-www.

CGE_WWW_PATH := ../../../../cge-www/

images:
	mkdir -p images
	cp -f $(CGE_WWW_PATH)htdocs/images/header_icon.png \
	      $(CGE_WWW_PATH)htdocs/images/patreonlogoorange_45px.png \
	      images/

castle-engine-website-base:
	mkdir -p castle-engine-website-base
	cp -Rf $(CGE_WWW_PATH)htdocs/castle-engine-website-base/castle-engine.css \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/castle-engine.js \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/bootstrap \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/colorbox \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/js \
	       castle-engine-website-base/
