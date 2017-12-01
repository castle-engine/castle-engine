# This requires file_to_pascal_string that is a trivial program
# that I made for pasdoc [http://pasdoc.sf.net/] project, see
# [https://pasdoc.svn.sourceforge.net/svnroot/pasdoc/trunk/source/tools/file_to_pascal_string.dpr]

ALL_TARGETS := \
  windows/plugin-automatic-windows-resources.rc.inc \
  windows/automatic-windows-resources.rc.inc \
  windows/automatic-windows.manifest.inc \
  template-castle-engine-output-warning.txt.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

%.inc: %
	file_to_pascal_string $< $@
