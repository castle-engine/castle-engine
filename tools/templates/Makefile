# This requires file_to_pascal_string that is a trivial program
# that I made for pasdoc [http://pasdoc.sf.net/] project, see
# [https://pasdoc.svn.sourceforge.net/svnroot/pasdoc/trunk/source/tools/file_to_pascal_string.dpr]

ALL_TARGETS := automatic-windows-resources.rc.inc automatic-windows.manifest.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

automatic-windows-resources.rc.inc: automatic-windows-resources.rc
	file_to_pascal_string $< $@
automatic-windows.manifest.inc: automatic-windows.manifest
	file_to_pascal_string $< $@
