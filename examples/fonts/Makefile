# This uses file_to_pascal_string, a trivial program
# from the pasdoc [http://pasdoc.sf.net/] project, see
# [https://svn.code.sf.net/p/pasdoc/code/trunk/source/tools/file_to_pascal_string.dpr]

ALL_TARGETS := html_text_demo.html.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

%.inc: %
	file_to_pascal_string $< $@
