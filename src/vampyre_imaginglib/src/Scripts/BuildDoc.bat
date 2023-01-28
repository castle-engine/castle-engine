@echo OFF

echo Building XHTML documentation
..\Bin\VampDoc -doc -f=xhtml -i=..\Doc\VampyreDoc\Imaging.vdocproj -o=..\Doc

echo Building HTMLHelp documentation
..\Bin\VampDoc -doc -f=htmlhelp -i=..\Doc\VampyreDoc\Imaging.vdocproj -o=..\Doc