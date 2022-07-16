# Main targets to gen final docs ----------------------------------------

.PHONY: default html pdf latex php

# html is shown on https://castle-engine.io/apidoc/html/
default: html

# Run "make" in logo/ first,
# if you have inkscape installed,
# and you want to regenerate logo from png.
#
# Run "make" in html-parts/ first,
# if you have cge-www repository (https://github.com/castle-engine/cge-www)
# cloned and placed as a sibling to castle-engine, in ../../../cge-www/

html:
	./mk_docs.sh html
# For quick testing of docs look (CSS etc.), you can run this instead
#	./mk_docs.sh html base/castleutils.pas
	rm -Rf ../reference/
	mv html ../reference/
	cp -R logo/castle_game_engine_icon.png \
	      html-parts/images/ \
	      html-parts/castle-engine-website-base/ \
	      html-parts/cge-pasdoc.js \
	      ../reference/

# Just some shortcuts to "Helpful targets" below
latex: latex/docs.tex
pdf: latex/docs.pdf

# Helpful targets to gen docs ----------------------------------------

latex/docs.tex:
	./mk_docs.sh latex

TEX_BATCH_OPTS := --file-line-error-style -interaction=nonstopmode

# At the end of pdf generating, we clean useless junk
# (because it's quite large, and I may want to upload latex/
# dir to WWW page of my units)
latex/docs.pdf: latex/docs.tex
	-cd latex; pdflatex $(TEX_BATCH_OPTS) docs.tex
	-cd latex; pdflatex $(TEX_BATCH_OPTS) docs.tex
	rm -f latex/docs.aux \
	      latex/docs.log \
	      latex/docs.out \
	      latex/docs.toc

# PHP update -----------------------------------------------------------------

# Generate PHP map, copy it to cge-www repo (you should commit it there).
.PHONY: php
php:
	./mk_docs.sh php
	cp -f php/docs.php ../../../cge-www/htdocs/apidoc_map_latest.php

# Cleaning ------------------------------------------------------------

.PHONY: clean-cache clean

clean-cache:
	rm -fR cache/

clean: clean-cache
	rm -Rf html/ latex/ php/ ../reference/

# upload ---------------------------------------------------------------------

SSH_USERNAME=michalis
SSH_HOST=ssh.castle-engine.io
SSH_PATH=/home/michalis/cge-html/apidoc/

# Prepare and upload all the stuff referenced under
# https://castle-engine.io/apidoc/html/ .
# Uploads as tar.gz and unpacks on server, this is *much* faster than
# uploading separate HTML files.
.PHONY: upload
upload: clean html
	rm -Rf ../reference/html
	mv ../reference/ html
	tar czf html.tar.gz html
	scp -r html.tar.gz $(SSH_USERNAME)@$(SSH_HOST):$(SSH_PATH)
	rm -f html.tar.gz
	./ssh_upload_finalize.sh $(SSH_USERNAME) $(SSH_HOST) $(SSH_PATH)
