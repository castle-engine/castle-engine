# Main targets to gen final docs ----------------------------------------

.PHONY: default html pdf ps rtf latex

# html and pdf are two most useful versions of documentation,
# shown on [http://castle-engine.sourceforge.net/reference.php]
# Later: pdf now removed, useless and ugly.
default: html

# Run "make" in ../logo/ first,
# if you have inkscape installed,
# and you want to regenerate logo from png.

html:
	./mk_docs.sh html
	cp ../logo/castle_game_engine_icon.png html/

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
	rm -Rf html/ latex/ latex2rtf/

# upload ---------------------------------------------------------------------

SF_USERNAME=kambi
SF_HOST=web.sourceforge.net
SF_PATH=/home/project-web/castle-engine/htdocs/apidoc/

SF_CONNECT=$(SF_USERNAME),castle-engine@$(SF_HOST):$(SF_PATH)
SCP_OPTIONS=

# Prepare and upload all the stuff referenced under
# [http://castle-engine.sourceforge.net/reference.php].
# Uploads as tar.gz and unpacks on server, this is *much* faster than
# uploading separate HTML files.
.PHONY: upload
upload: clean html
	tar czvf html.tar.gz html/
	scp $(SCP_OPTIONS) -r html.tar.gz $(SF_CONNECT)
	./sf_upload_finalize.sh $(SF_USERNAME) $(SF_PATH)
