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
	rm -Rf images
	mkdir -p images
	cp -Rf $(CGE_WWW_PATH)htdocs/images/header_icon.png \
	       $(CGE_WWW_PATH)htdocs/images/patreon-brand \
	       images/
# The $(CGE_WWW_PATH)htdocs/images/castle_game_engine_icon* are symlinks now,
# copy instead the actual files to make things simple for distributing these API docs.
	cp -f  $(CGE_WWW_PATH)htdocs/images/not_resized_original/castle_game_engine_icon.png \
	       images/
	cp -f  $(CGE_WWW_PATH)htdocs/images/not_resized_original/castle_game_engine_icon_fit_in_square.png \
	       images/

castle-engine-website-base:
	rm -Rf castle-engine-website-base
	mkdir -p castle-engine-website-base
	cp -Rf $(CGE_WWW_PATH)htdocs/castle-engine-website-base/castle-engine.css \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/castle-asciidoctor.css \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/castle-engine.js \
	       $(CGE_WWW_PATH)htdocs/castle-engine-website-base/node_modules \
	       castle-engine-website-base/
