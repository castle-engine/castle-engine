# Main targets to gen final docs ----------------------------------------

.PHONY: default html pdf ps rtf latex

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
pdf: latex/docs.pdf
ps: latex/docs.ps
rtf: latex2rtf/docs.rtf
latex: latex/docs.tex

# Helpful targets to gen docs ----------------------------------------

latex2rtf/docs.tex:
	./mk_docs.sh latex2rtf

latex2rtf/docs.rtf: latex2rtf/docs.tex
	cd latex2rtf; latex2rtf docs.tex

latex/docs.tex:
	./mk_docs.sh latex

TEX_BATCH_OPTS := --file-line-error-style -interaction=nonstopmode

latex/docs.dvi: latex/docs.tex
	-cd latex; latex $(TEX_BATCH_OPTS) docs.tex
	-cd latex; latex $(TEX_BATCH_OPTS) docs.tex

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

latex/docs.ps: latex/docs.dvi
	cd latex; dvips docs.dvi -o docs.ps

# Cleaning ------------------------------------------------------------

.PHONY: clean-cache clean

clean-cache:
	rm -fR cache/

clean: clean-cache
	rm -Rf html/ latex/ latex2rtf/ ../reference/

# upload ---------------------------------------------------------------------

SSH_USERNAME=michalis
SSH_HOST=castle-engine.io
SSH_PATH=/home/michalis/cge-html/apidoc/
# Use this for unstable docs:
#SSH_PATH=/home/michalis/cge-html/apidoc-unstable/

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
