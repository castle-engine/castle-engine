# Most useful targets:
#
#   all (default target) --
#     Compile all units
#   any subdirectory name, like base or 3dgraph --
#     Compile all units inside that subdirectory (and all used units
#     in other directories)
#
#   info --
#     Some information about what this Makefile sees, how will it work etc.
#
#   clean --
#     Delete FPC 1.0.x Win32 trash (*.ppw, *.ow), FPC trash, Delphi trash,
#     binaries of programs in examples/ subdirectories,
#     also FPC compiled trash in packages/*/lib/
#
# Not-so-commonly-useful targets:
#
#   all_allunits_files --
#     Create special All*Units.pas units in all subdirectories.
#     Note that regenerating these units is not so easy -- you'll
#     need Emacs and my kambi-pascal-functions.el Elisp
#     code to do it (not publicly distributed yet, but this will change
#     in the future; tell me if you want it).
#
#   cleanmore --
#     Same as clean + delete Emacs backup files (*~) and Delphi backup files
#     (*.~??? (using *.~* would be too unsafe ?))
#
#   clean_special_allunits --
#     Cleans special units All*Units.pas.
#     This may be uneasy to undo, look at comments at all_allunits_files.
#
#   cleanall --
#     Same as cleanmore + clean_special_allunits.
#     This target should not be called unless you know
#     that you really want to get rid of ALL files that can be automatically
#     regenerated. Note that some of the files deleted by this target may
#     be not easy to regenerate -- see comments at all_allunits_files.
#
# Internal notes (not important if you do not want to read/modify
# this Makefile):
#
# Note: In many places in this Makefile I'm writing some special code
# to not descend to 'private' and 'old' subdirectories.
# This is something that is usable only for me (Michalis),
# if you're trying to understand this Makefile you can just ignore
# such things (you may be sure that I will never have here directory
# called 'private' or 'old').
#
# This Makefile must be updated when adding new subdirectory to my units.
# To add new subdirectory foo, add rules
# 1. rule 'foo' to compile everything in foo
# 2. rule to recreate file foo/allkambifoounits.pas
# 3. add to $(ALL_ALLUNITS_FILES) to delete on clean_special_allunits

# compiling ------------------------------------------------------------

.PHONY: all

UNITS_SUBDIRECTORIES := $(shell \
  find * -maxdepth 0 -type d \
    '(' -not -name 'private' ')' '(' -not -name 'old' ')' -print)

all: $(UNITS_SUBDIRECTORIES)

# compiling rules for each subdirectory

.PHONY: $(UNITS_SUBDIRECTORIES)

# Note: I have to use here cd ..; first because the base directory
# must be the directory where kambi.cfg file is.
# This is because kambi.cfg contains paths (in -Fu and -Fi)
# relative to that directory.
COMPILE_ALL_DIR_UNITS=cd ../; fpc -dRELEASE @kambi.cfg units/$<

3dgraph: 3dgraph/allkambi3dgraphunits.pas
	$(COMPILE_ALL_DIR_UNITS)

3dmodels.gl: 3dmodels.gl/allkambi3dmodelsglunits.pas
	$(COMPILE_ALL_DIR_UNITS)

3dmodels: 3dmodels/allkambi3dmodelsunits.pas
	$(COMPILE_ALL_DIR_UNITS)

audio: audio/allkambiaudiounits.pas
	$(COMPILE_ALL_DIR_UNITS)

base: base/allkambibaseunits.pas
	$(COMPILE_ALL_DIR_UNITS)

fonts: fonts/allkambifontsunits.pas
	$(COMPILE_ALL_DIR_UNITS)

images: images/allkambiimagesunits.pas
	$(COMPILE_ALL_DIR_UNITS)

opengl: opengl/allkambiopenglunits.pas
	$(COMPILE_ALL_DIR_UNITS)

# creating All*Units.pas files ----------------------------------------

EMACS_BATCH := emacs -batch --eval="(require 'kambi-pascal-functions)"

ALL_ALLUNITS_FILES := 3dgraph/allkambi3dgraphunits.pas \
  3dmodels.gl/allkambi3dmodelsglunits.pas \
  3dmodels/allkambi3dmodelsunits.pas \
  audio/allkambiaudiounits.pas \
  base/allkambibaseunits.pas \
  fonts/allkambifontsunits.pas \
  images/allkambiimagesunits.pas \
  opengl/allkambiopenglunits.pas

# This is a nice target to call before doing a distribution of my sources,
# because I always want to distribute these All*Units.pas units.
# (so noone except me should ever need to run emacs to generate them)
all_allunits_files: $(ALL_ALLUNITS_FILES)

3dgraph/allkambi3dgraphunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"3dgraph/\" \"AllKambi3dGraphUnits\") \
  (save-buffer))"

3dmodels.gl/allkambi3dmodelsglunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"3dmodels.gl/\" \"AllKambi3dModelsGLUnits\") \
  (save-buffer))"

3dmodels/allkambi3dmodelsunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"3dmodels/\" \"AllKambi3dModelsUnits\") \
  (save-buffer))"

audio/allkambiaudiounits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"audio/\" \"AllKambiAudioUnits\") \
  (save-buffer))"

base/allkambibaseunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"base/\" \"AllKambiBaseUnits\") \
  (save-buffer))"

# FIXME: kam-simple-replace-buffer here is dirty hack to correct problems
# with all-units-in-dir
fonts/allkambifontsunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"fonts/\" \"AllKambiFontsUnits\") \
  (kam-simple-replace-buffer \"ttfontstypes,\" \"ttfontstypes {\$$ifdef WIN32}, {\$$endif}\") \
  (save-buffer))"

images/allkambiimagesunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"images/\" \"AllKambiImagesUnits\") \
  (save-buffer))"

opengl/allkambiopenglunits.pas:
	$(EMACS_BATCH) --eval="(progn \
  (write-unit-all-units-in-dir \"opengl/\" \"AllKambiOpenGLUnits\") \
  (save-buffer))"

# information ------------------------------------------------------------

.PHONY: info

info:
	@echo "All available units subdirectories (they are also targets"
	@echo "for this Makefile):"
	@echo $(UNITS_SUBDIRECTORIES)

# cleaning ------------------------------------------------------------

.PHONY: clean cleanmore cleanall clean_special_allunits

clean:
	find . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' ')' \
	     -print \
	     | xargs rm -f
	rm -f base/examples/demo_parsingpars \
	      base/examples/demo_parsingpars.exe \
	      base/examples/demo_textreader \
	      base/examples/demo_textreader.exe \
	      base/examples/kambi_calc \
	      base/examples/kambi_calc.exe \
	      opengl/examples/glWinEvents \
	      opengl/examples/glWinEvents.exe \
	      opengl/examples/menu_test_alternative \
	      opengl/examples/menu_test_alternative.exe \
	      opengl/examples/menuTest \
	      opengl/examples/menuTest.exe \
	      opengl/examples/test_glwindow_gtk_mix \
	      opengl/examples/test_glwindow_gtk_mix.exe \
	      3dmodels/examples/many2vrml \
	      3dmodels/examples/many2vrml.exe \
	      3dmodels/tools/gen_light_map \
	      3dmodels/tools/gen_light_map.exe \
	      3dmodels.gl/examples/simpleViewModel_2 \
	      3dmodels.gl/examples/simpleViewModel_2.exe \
	      3dmodels.gl/examples/simpleViewModel \
	      3dmodels.gl/examples/simpleViewModel.exe
	rm -Rf packages/unix/lib/ packages/unix/kambi_units.pas \
	  packages/win32/lib/ packages/win32/kambi_units.pas

cleanmore: clean
	find . -type f '(' -iname '*~' -or -iname '*.~???' ')' -exec rm -f '{}' ';'

clean_special_allunits:
	rm -f $(ALL_ALLUNITS_FILES)

cleanall: cleanmore clean_special_allunits

# eof ------------------------------------------------------------
