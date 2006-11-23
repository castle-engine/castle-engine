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
#   examples --
#     Compile all examples and tools (things inside examples/ and tools/
#     subdirectories). Note that you can also compile each example separately,
#     just run appropriate xxx_compile.sh scripts.
#
#   clean --
#     Delete FPC 1.0.x Win32 trash (*.ppw, *.ow), FPC trash, Delphi trash,
#     binaries of example programs,
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

COMPILE_ALL_DIR_UNITS=fpc -dRELEASE @kambi.cfg $<

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

# examples and tools -----------------------------------------------------------

EXAMPLES_BASE_NAMES := base/examples/demo_parseparameters \
  base/examples/demo_textreader \
  base/examples/kambi_calc \
  images/examples/image_convert \
  opengl/examples/glWinEvents \
  opengl/examples/menu_test_alternative \
  opengl/examples/menuTest \
  opengl/examples/test_glwindow_gtk_mix \
  opengl/examples/test_font_break \
  opengl/examples/multi_glwindow \
  opengl/examples/demo_matrix_navigation \
  3dgraph/examples/draw_space_filling_curve \
  3dmodels/examples/many2vrml \
  3dmodels/tools/gen_light_map \
  3dmodels.gl/examples/simpleViewModel_2 \
  3dmodels.gl/examples/simpleViewModel \
  3dmodels.gl/examples/demo_animation \
  3dmodels.gl/examples/fog_culling

EXAMPLES_UNIX_EXECUTABLES := $(EXAMPLES_BASE_NAMES) \
  audio/examples/test_al_source_allocator \
  3dmodels.gl/examples/view3dscene_mini_by_lazarus/view3dscene_mini_by_lazarus

EXAMPLES_WINDOWS_EXECUTABLES := $(addsuffix .exe,$(EXAMPLES_BASE_NAMES)) \
  audio/examples/test_al_source_allocator.exe \
  3dmodels.gl/examples/view3dscene_mini_by_lazarus/view3dscene_mini_by_lazarus.exe

.PHONY: examples
examples:
	cd ../; $(foreach NAME,$(EXAMPLES_BASE_NAMES),units/$(NAME)_compile.sh && ) true

.PHONY: cleanexamples
cleanexamples:
	rm -f $(EXAMPLES_UNIX_EXECUTABLES) $(EXAMPLES_WINDOWS_EXECUTABLES)

# information ------------------------------------------------------------

.PHONY: info

info:
	@echo "All available units subdirectories (they are also targets"
	@echo "for this Makefile):"
	@echo $(UNITS_SUBDIRECTORIES)

# cleaning ------------------------------------------------------------

.PHONY: clean cleanmore cleanall clean_special_allunits

clean: cleanexamples
	find . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' ')' \
	     -print \
	     | xargs rm -f
	rm -Rf packages/unix/lib/ packages/unix/kambi_units.pas \
	  packages/unix/kambi_glwindow.pas \
	  packages/unix/kambi_glwindow_navigated.pas \
	  packages/win32/lib/ packages/win32/kambi_units.pas \
	  packages/components/lib/ packages/components/kambi_components.pas

cleanmore: clean
	find . -type f '(' -iname '*~' -or \
	                   -iname '*.bak' -or \
	                   -iname '*.~???' -or \
			   -iname '*.blend1' \
			')' -exec rm -f '{}' ';'

clean_special_allunits:
	rm -f $(ALL_ALLUNITS_FILES)

cleanall: cleanmore clean_special_allunits

# eof ------------------------------------------------------------
