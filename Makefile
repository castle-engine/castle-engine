# Makefile to build and perform other useful operations on the Castle Game Engine.
#
# Requires using GNU make.
# - On Linux and most other Unixes, just call "make".
# - On FreeBSD call "gmake".
# - On Windows, install Cygwin ( http://www.cygwin.com/ ) with "make" package.
#   Make sure that when you type "make" it executes Cygwin's "make":
#   Cygwin's directory (like c:/cygwin64/bin/) should be on $PATH environment variable
#   *before* other directories with "make" implementations, like
#   - FPC directory that includes "make" from MinGW (it is GNU make but cannot execute bash scripts)
#   - Delphi directory that includes Embarcadero "make" (it's not GNU make and will fail).
#
# Most useful targets of this Makefile:
#
#   all (default target) --
#     Compile all units, uses fpmake.
#
#   examples --
#     Compile most examples and tools (that don't use Lazarus LCL).
#     Lazarus is not required (LCL dependent examples are not compiled).
#
#     This compilation method calls our "build tool" to compile examples
#     and other tools (build tool, in turn, calls a compiler like FPC or Delphi).
#
#     The exception is when compiling the "build tool" itself,
#     then we call FPC directly. (Although we *could* use
#     the "build tool" to compile (bootstrap) itself, but it's not what
#     people expect by default, so we don't do it for now.)
#
#   examples-laz --
#     Compile all examples and tools using Lazarus (lazbuild).
#     This compilation method uses our .lpi project files,
#     and compiles every program by the lazbuild utility.
#     Lazarus and FPC installation is required.
#
#   clean --
#     Delete FPC temporary files, Delphi temporary files,
#     Lazarus temporary files (*.compiled),
#     binaries of example programs and tools.
#
# Not-so-commonly-useful targets:
#
#   cleanmore --
#     Same as clean, but also delete:
#     - Backup files from Emacs (*~), Lazarus (backup), Delphi (*.~???), Blender,
#       QtCreator (*.pro.user)...
#     - pasdoc generated documentation in doc/pasdoc/ and doc/reference/
#     - closed-source libs you may have left in tools/build-tool/data
#     This is a useful step when packing the release of CGE.
#
#   cleanall --
#     Same as cleanmore for now.
#     Intention is to remove *everything* that can be manually recreated,
#     even if it's somewhat hard to recreate.

# detect platform-specific things --------------------------------------------

# FIND := Unix/Cygwin "find" utility (*not* Windows "find" command which has completely different use)
# SED := GNU sed (*not* other sed implementations)
# EXE_EXTENSION := extension of executable files (with leading dot, if any)

SED := sed
FIND := find
INSTALL := install
EXE_EXTENSION :=

ifeq ($(OS),Windows_NT)
  # On Windows avoid using Windows built-in "find" program. Use the Cygwin "find".
  FIND := `cygpath --mixed /bin/find`
  EXE_EXTENSION := .exe
else
  # Only on Unix, you can use "uname" to further detect Unix variants,
  # see https://stackoverflow.com/questions/714100/os-detecting-makefile
  UNAME_S := $(shell uname -s)

  # On macOS, use gsed and ginstall (e.g. from Homebrew).
  # See https://www.topbug.net/blog/2013/04/14/install-and-use-gnu-command-line-tools-in-mac-os-x/
  # http://www.legendu.net/en/blog/install-gnu-utils-using-homebrew/
  ifeq ($(UNAME_S),Darwin)
    SED := gsed
    INSTALL := ginstall
  endif
endif

# config ---------------------------------------------------------------------

# By default, use build tool compiled by tools/build-tool/castle-engine_compile.sh .
# This is easy, and when not cross-compiling, things will just work out-of-the-box,
# Makefile will always first build the build tool, then use it.
#
# You can override it on the command-line like "make BUILD_TOOL=castle-engine"
# to use build tool on $PATH .
# This is more reliable in some scenarios:
#
# - We will use build tool for current (source) system, even if CASTLE_FPC_OPTIONS
#   uses -T${OS} -P${CPU} to indicate a different (target) system,
#   so castle-engine_compile.sh doesn't build for current system.
#
# - Build tool can exist outside of CGE, and thus it exists regardless of "make clean" etc.
#
BUILD_TOOL = ./tools/build-tool/castle-engine$(EXE_EXTENSION)

# compile ------------------------------------------------------------

.PHONY: all
all:
	$(MAKE) --no-print-directory build-using-fpmake
	$(MAKE) tools

.PHONY: tools
tools:
# Compile build tool first, used to compile other tools and examples
	tools/build-tool/castle-engine_compile.sh
	$(BUILD_TOOL) --project tools/castle-curves/ compile
	$(BUILD_TOOL) --project tools/image-to-pascal/ compile
	$(BUILD_TOOL) --project tools/texture-font-to-pascal/ compile
	$(BUILD_TOOL) --project tools/to-data-uri/ compile

.PHONY: build-using-fpmake
build-using-fpmake:
	fpc fpmake.pp
	@echo 'Running fpmake. If this fails saying that "rtl" is not found, remember to set FPCDIR environment variable, see http://wiki.freepascal.org/FPMake .'
# Workaround FPC >= 3.x problem (bug?) --- it ignores $FPCDIR, but --globalunitdir works
	if [ '(' -n "$(FPCDIR)" ')' ]; then \
	   ./fpmake --globalunitdir="$(FPCDIR)"; \
	else \
	   ./fpmake; \
	fi

# Full test that fpmake compilation process works
# (see https://github.com/castle-engine/castle-engine/wiki/FpMake )
.PHONY: test-fpmake
test-fpmake: build-using-fpmake
# Test fpmake with --nofpccfg, to make sure our dependencies in fpmake.pp are correct
	./fpmake clean --verbose
	if [ '(' -n "$(FPCDIR)" ')' ]; then \
	   ./fpmake --globalunitdir="$(FPCDIR)" --nofpccfg --verbose; \
	else \
	   ./fpmake --nofpccfg --verbose; \
	fi

# install / uninstall --------------------------------------------------------
#
# Note that this *does not* take care of installing the unit files.
# So it does not copy .ppu/.o files, it does not change your /etc/fpc.cfg
# or ~/.fpc.cfg. There are many ways how to install unit files,
# we leave this step up to you.
# See https://castle-engine.io/engine.php for documentation.
#
# Below we only take care of installing the tools.
# By default they are installed system-wide to /usr/local ,
# so you can run "make" followed by "sudo make install" to have the tools
# ready on a typical Unix system.

# Standard installation dirs, following conventions on
# http://www.gnu.org/prep/standards/html_node/Directory-Variables.html#Directory-Variables
PREFIX=$(DESTDIR)/usr/local
EXEC_PREFIX=$(PREFIX)
BINDIR=$(EXEC_PREFIX)/bin
DATAROOTDIR=$(PREFIX)/share
DATADIR=$(DATAROOTDIR)

.PHONY: install
install:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) tools/texture-font-to-pascal/texture-font-to-pascal$(EXE_EXTENSION) $(BINDIR)
	$(INSTALL) tools/image-to-pascal/image-to-pascal$(EXE_EXTENSION) $(BINDIR)
	$(INSTALL) tools/castle-curves/castle-curves$(EXE_EXTENSION) $(BINDIR)
	$(INSTALL) tools/build-tool/castle-engine$(EXE_EXTENSION) $(BINDIR)
	$(INSTALL) tools/to-data-uri/to-data-uri$(EXE_EXTENSION) $(BINDIR)
#	cp -R tools/build-tool/data $(DATADIR)/castle-engine
	$(INSTALL) -d  $(DATADIR)
	cd tools/build-tool/data/ && \
	  $(FIND) . -type f -exec $(INSTALL) --mode 644 -D '{}' $(DATADIR)/castle-engine/'{}' ';'

.PHONY: uninstall
uninstall:
	rm -f  $(BINDIR)/texture-font-to-pascal$(EXE_EXTENSION) \
	       $(BINDIR)/image-to-pascal$(EXE_EXTENSION) \
	       $(BINDIR)/castle-curves$(EXE_EXTENSION) \
	       $(BINDIR)/castle-engine$(EXE_EXTENSION) \
	       $(BINDIR)/to-data-uri$(EXE_EXTENSION)
	rm -Rf $(DATADIR)/castle-engine

# Strip libraries that cannot be distributed in Debian package of CGE.
# - Some of them (some bundled Windows, Android libs) cannot be recompiled
#   automatically and easily for now (although they are open-source),
#   and are not of sufficient interest to the Debian users.
# - Some (Gradle) should be better used by depending on
#   the appropriate Debian package.
.PHONY: strip-precompiled-libraries
strip-precompiled-libraries:
	rm -Rf tools/build-tool/data/external_libraries/ \
	       tools/build-tool/data/android/integrated-services/sound/ \
	       tools/build-tool/data/android/integrated-services/ogg_vorbis/ \
	       tools/build-tool/data/android/integrated-services/freetype/ \
	       tools/build-tool/data/android/base/gradle/ \
	       tools/build-tool/data/android/base/gradlew \
	       tools/build-tool/data/android/base/gradlew.bat \
	       tools/build-tool/data/android/integrated/gradle/ \
	       tools/build-tool/data/android/integrated/gradlew \
	       tools/build-tool/data/android/integrated/gradlew.bat

# examples and tools -----------------------------------------------------------

# Note that images/examples/fft_tests is not included here,
# and unit images/imagesfftw.pas is not included in fpmake.pp,
# because
# 1. it requires to compile FPC > 2.2.x, and we try to work also with earlier FPC.
# 2. to link the example, you need the fftw library. I don't want
#    to force everyone who wants to execute "make examples" to install
#    fftw library (especially since it's really not needed by my engine,
#    currently my fftw code is just for testing, it's not actually used
#    by our engine or any game for anything).
#
# Note that examples with CastleEngineManifest.xml are not listed here.
# They will be found and compiled by a Makefile rule that searches using
# "find ... -iname CastleEngineManifest.xml ..." .
#
# TODO: In the long run, it would be best if *all* examples had CastleEngineManifest.xml,
# then the need to maintain EXAMPLES_BASE_NAMES would disappear.

EXAMPLES_BASE_NAMES := \
  examples/3d_rendering_processing/animate_3d_model_by_code \
  examples/3d_rendering_processing/animate_3d_model_by_code_2 \
  examples/3d_rendering_processing/build_3d_object_by_code \
  examples/3d_rendering_processing/build_3d_tunnel \
  examples/3d_rendering_processing/call_pascal_code_from_3d_model_script \
  examples/3d_rendering_processing/cars_demo \
  examples/3d_rendering_processing/combine_multiple_x3d_into_one \
  examples/3d_rendering_processing/custom_input_shortcuts_saved_to_config \
  examples/3d_rendering_processing/display_box_custom_shaders \
  examples/3d_rendering_processing/fog_culling \
  examples/3d_rendering_processing/listen_on_x3d_events \
  examples/3d_rendering_processing/multiple_viewports \
  examples/3d_rendering_processing/placeholder_names \
  examples/3d_rendering_processing/render_3d_to_image \
  examples/3d_rendering_processing/render_3d_to_texture_and_use_as_quad \
  examples/3d_rendering_processing/transform_scenes_demos \
  examples/3d_rendering_processing/show_bounding_rect_in_2d \
  examples/3d_rendering_processing/switch_projection \
  examples/3d_rendering_processing/transform_feedback \
  examples/3d_rendering_processing/triangulate_demo \
  examples/3d_rendering_processing/view_3d_model_advanced \
  examples/3d_rendering_processing/view_3d_model_basic \
  examples/curves/simplest_curve_read \
  examples/images_videos/background_tiling \
  examples/images_videos/dds_decompose \
  examples/images_videos/draw_images_on_gpu \
  examples/images_videos/drawing_modes_test \
  examples/images_videos/image_compare \
  examples/images_videos/image_convert \
  examples/images_videos/image_identify \
  examples/images_videos/image_paint \
  examples/images_videos/image_render_custom_shader \
  examples/images_videos/simple_video_editor \
  examples/images_videos/test_castleimage_draw3x3 \
  examples/research_special_rendering_methods/radiance_transfer/precompute_radiance_transfer \
  examples/research_special_rendering_methods/radiance_transfer/radiance_transfer \
  examples/research_special_rendering_methods/radiance_transfer/show_sh \
  examples/simple_command_line_utilities/dircleaner \
  examples/simple_command_line_utilities/stringoper

# Note that src/library/castleengine must be compiled before
# cge_dynlib_tester, otherwise linking cge_dynlib_tester will fail.
EXAMPLES_LAZARUS_BASE_NAMES := \
  src/library/castleengine \
  examples/library/lazarus_library_tester/cge_dynlib_tester

EXAMPLES_UNIX_EXECUTABLES := $(EXAMPLES_BASE_NAMES) \
  $(EXAMPLES_LAZARUS_BASE_NAMES)

EXAMPLES_WINDOWS_EXECUTABLES := $(addsuffix .exe,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .exe,$(EXAMPLES_LAZARUS_BASE_NAMES))

# Test compiling single CGE editor template.
# Requires EDITOR_TEMPLATE_PATH to be defined.
.PHONY: test-editor-template
test-editor-template:
	$(SED) --in-place=.backup \
	  -e 's|standalone_source="$${PROJECT_PASCAL_NAME}_standalone.lpr"||' \
	  -e 's|qualified_name="$${PROJECT_QUALIFIED_NAME}"||' \
	  -e 's|$${PROJECT_NAME}|test_template_project_name|' \
	  $(EDITOR_TEMPLATE_PATH)CastleEngineManifest.xml
	$(SED)  --in-place=.backup \
	  -e 's|$${PROJECT_NAME}|test_template_project_name|' \
	  -e 's|$${MAIN_STATE}|Main|g' \
	  -e 's|$${MAIN_STATE_LOWERCASE}|main|g' \
	  $(EDITOR_TEMPLATE_PATH)code/gameinitialize.pas
ifdef CASTLE_HAS_MAIN_STATE
	$(SED)  --in-place=.backup \
	  -e 's|$${PROJECT_NAME}|test_template_project_name|' \
	  -e 's|$${MAIN_STATE}|Main|g' \
	  -e 's|$${MAIN_STATE_LOWERCASE}|main|g' \
	  $(EDITOR_TEMPLATE_PATH)code/gamestatemain.pas
endif
	cp tools/build-tool/data/castleautogenerated_template.pas $(EDITOR_TEMPLATE_PATH)castleautogenerated.pas
	$(SED)  --in-place \
	  -e 's|$${NAME}|test_template_project_name|' \
	  -e 's|$${CAPTION}|Test Template Project Caption|' \
	  -e 's|$${VERSION}|0.1|' \
	  $(EDITOR_TEMPLATE_PATH)castleautogenerated.pas
	$(BUILD_TOOL) --project $(EDITOR_TEMPLATE_PATH) compile
	$(BUILD_TOOL) --project $(EDITOR_TEMPLATE_PATH) clean
	mv -f $(EDITOR_TEMPLATE_PATH)CastleEngineManifest.xml.backup $(EDITOR_TEMPLATE_PATH)CastleEngineManifest.xml
	mv -f $(EDITOR_TEMPLATE_PATH)code/gameinitialize.pas.backup $(EDITOR_TEMPLATE_PATH)code/gameinitialize.pas
ifdef CASTLE_HAS_MAIN_STATE
	mv -f $(EDITOR_TEMPLATE_PATH)code/gamestatemain.pas.backup $(EDITOR_TEMPLATE_PATH)code/gamestatemain.pas
endif
	rm -f $(EDITOR_TEMPLATE_PATH)castleautogenerated.pas

# Test compiling CGE editor templates
.PHONY: test-editor-templates
test-editor-templates:
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/3d_fps_game/files/
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/2d_game/files/
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/empty/files/ CASTLE_HAS_MAIN_STATE=true
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/3d_model_viewer/files/ CASTLE_HAS_MAIN_STATE=true

.PHONY: examples
examples:
# Compile build tool first, used to compile other tools and examples.
# Also copy it, as below it will recompile itself (which would be trouble on Windows).
	tools/build-tool/castle-engine_compile.sh
	cp -f $(BUILD_TOOL) castle-engine-copy$(EXE_EXTENSION)

# Compile all examples using xxx_compile.sh shell script (calls build tool), TODO: to remove
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh && ) true

# Compile all examples with CastleEngineManifest.xml inside.
# Use xargs (not "find ... -execdir") because we want the "make examples" to fail
# if something failed to compile.
# We make a copy of castle-engine, otherwise it would fail on Windows
# (as you cannot replace your own exe).
# Exceptions:
# - We do not compile examples/network/tcp_connection/ here,
#   as it requires Indy which may not be installed.
	$(FIND) . \
	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -o \
	  '(' -path ./tools/build-tool/tests/data -prune ')' -o \
	  '(' -iname CastleEngineManifest.xml -print0 ')' | \
	  xargs -0 -n1 ./castle-engine-copy$(EXE_EXTENSION) \
	    $(CASTLE_ENGINE_TOOL_OPTIONS) compile --project

# Compile editor templates
	 $(MAKE) test-editor-templates

.PHONY: cleanexamples
cleanexamples:
	rm -f $(EXAMPLES_UNIX_EXECUTABLES) $(EXAMPLES_WINDOWS_EXECUTABLES)

.PHONY: examples-laz
examples-laz:
	lazbuild packages/castle_base.lpk
	lazbuild packages/castle_window.lpk
	lazbuild packages/castle_components.lpk
	for LPI_FILENAME in $(EXAMPLES_BASE_NAMES) $(EXAMPLES_LAZARUS_BASE_NAMES); do \
	  ./tools/internal/lazbuild_retry $${LPI_FILENAME}.lpi; \
	done
	$(FIND) . \
	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -o \
	  '(' -path ./tools/build-tool/tests/data -prune ')' -o \
	  '(' -iname '*.lpi' -exec ./tools/internal/lazbuild_retry '{}' ';' ')'

# cleaning ------------------------------------------------------------

.PHONY: clean cleanmore cleanall

clean: cleanexamples
	$(FIND) . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
			   -iname '*.or'  -or \
			   -iname '*.res' -or \
			   -iname '*.rsj' -or \
			   -iname '*.compiled' -or \
			   -iname '*.lps' -or \
			   -iname '*.libimp*.a' -or \
			   -iname '*.apk' -or \
			   -iname '*.dbg' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' -or \
			   -iname 'automatic-windows-resources.res' -or \
			   -iname 'castle-auto-generated-resources.res' -or \
	                   -iname '*.log' ')' \
	     -print \
	     | xargs rm -f
# Note: *.app directory is a macOS bundle
	$(FIND) . -type d '(' -name 'lib' -or \
	                      -name 'castle-engine-output' -or \
			      -name '*.app' ')' \
	     -exec rm -Rf '{}' ';' -prune
	rm -Rf bin/ \
	  castle-engine-copy$(EXE_EXTENSION) \
	  packages/castle_base.pas \
	  packages/castle_window.pas \
	  packages/castle_components.pas \
	  packages/alternative_castle_window_based_on_lcl.pas \
	  tests/test_castle_game_engine \
	  tests/test_castle_game_engine.exe \
	  examples/fonts/font_draw_over_image_output.png \
	  examples/short_api_samples/transform_save_load/aaa.castle-transform
	$(MAKE) -C doc/man/man1/ clean
# fpmake stuff (binary, units/ produced by fpmake compilation, configs)
	rm -Rf fpmake fpmake.exe units/ *.fpm .fppkg .config
# lazarus produces lib/ subdirectories during compilation
	$(FIND) examples/ -type d -name lib -prune -exec rm -Rf '{}' ';'
	rm -Rf src/library/ios-output/\
	       src/library/libcastleengine.dylib \
	       src/library/castleengine.dll \
	       src/library/libcastleengine.so
# Clean every project with CastleEngineManifest.xml .
#
# Avoid a project in project_templates,
# that has CastleEngineManifest.xml with macros, and would cause errors:
# """ Project name contains invalid characters: "${PROJECT_NAME}" """ .
#
# Avoid project in build-tool/tests/data that is not a real project
# (will never be compiled).
#
# Note: This may cause errors if build tool doesn't exist anymore, ignore them.
	$(FIND) . \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -or \
	  '(' -path ./tools/build-tool/tests/data -prune ')' -or \
	  '(' -iname CastleEngineManifest.xml \
	      -execdir $(BUILD_TOOL) clean ';' ')'

cleanmore: clean
	$(FIND) . -type f '(' -iname '*~' -or \
	                   -iname '*.bak' -or \
	                   -iname '*.~???' -or \
	                   -iname '*.pro.user' -or \
			   -iname '*.blend1' \
			')' -exec rm -f '{}' ';'
	$(FIND) . -type d '(' -iname 'backup' \
			')' -exec rm -Rf '{}' ';' -prune
	$(MAKE) -C doc/pasdoc/ clean
	rm -Rf tools/build-tool/data/android/integrated-services/giftiz/app/libs/*.jar \
	       tools/build-tool/data/android/integrated-services/chartboost/app/libs/*.jar \
	       tools/build-tool/data/android/integrated-services/heyzap/app/libs/*.jar \
	       tools/build-tool/data/android/integrated-services/heyzap/app/libs/*.aar \
	       tools/build-tool/data/android/integrated-services/startapp/app/libs/*.jar \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/GameAnalytics.h \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/libGameAnalytics.a
	rm -f castle-engine*.zip tools/internal/pack_release/castle-engine*.zip

cleanall: cleanmore

# tests ----------------------------------------

# Build and run tests.
.PHONY: tests
tests:
	tools/build-tool/castle-engine_compile.sh
# Build and run check_lazarus_packages
	$(BUILD_TOOL) --project tools/internal/check_lazarus_packages/ clean
	$(BUILD_TOOL) --project tools/internal/check_lazarus_packages/ --mode=debug compile
	$(BUILD_TOOL) --project tools/internal/check_lazarus_packages/ run -- ../../../
# Run in debug mode
	$(BUILD_TOOL) --project tests/ clean
	$(BUILD_TOOL) --project tests/ --mode=debug --compiler-option=-dNO_WINDOW_SYSTEM compile
	$(BUILD_TOOL) --project tests/ run -- -a
# Run in debug mode without LibPng
# (useful to test image processing, e.g. TTestImages.TestLoadImage, using fcl-image, which matters for mobile now)
	$(BUILD_TOOL) --project tests/ clean
	$(BUILD_TOOL) --project tests/ --mode=debug --compiler-option=-dNO_WINDOW_SYSTEM --compiler-option=-dCASTLE_DISABLE_LIBPNG compile
	$(BUILD_TOOL) --project tests/ run -- -a
# Run in release mode, since all tests must pass the same when optimizations are enabled
	$(BUILD_TOOL) --project tests/ clean
	$(BUILD_TOOL) --project tests/ --mode=release --compiler-option=-dNO_WINDOW_SYSTEM compile
	$(BUILD_TOOL) --project tests/ run -- -a

# eof ------------------------------------------------------------
