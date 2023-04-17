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
#   default (default target) --
#     Build the most useful things:
#     - tools not depending on LCL (including build tool)
#     - editor
#     Also, register all Lazarus packages.
#     Also, place all tools and editor in bin/ subdirectory.
#
#   tools --
#     Build all tools that don't depend on LCL.
#     Right now it means: all tools except castle-editor.
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
#   build-using-fpmake:
#     Compile all units using FpMake.
#     We support this as an optional build approach to CGE and applications using CGE.
#     See https://castle-engine.io/fpmake .
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
#     - FPC from cge-fpc
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

# Detect Windows when $OS is Windows_NT or win64.
# Other $OS, including when $OS is empty / undefined, must be non-Windows.
# See https://stackoverflow.com/questions/7656425/makefile-ifeq-logical-or#9802777
# to how this trick to detect an alternative works.
ifneq (,$(filter $(OS),Windows_NT win64))
  $(info Detected Windows, OS is $(OS))

  # On Windows avoid using Windows built-in "find" program. Use the Cygwin "find".
  FIND := `cygpath --mixed /bin/find`
  EXE_EXTENSION := .exe
else
  $(info Detected non-Windows, OS is $(OS))

  # Only on Unix, you can use "uname" to further detect Unix variants,
  # see https://stackoverflow.com/questions/714100/os-detecting-makefile
  UNAME_S := $(shell uname -s)

  # On macOS, use gsed and ginstall (e.g. from Homebrew, use "brew install gnu-sed coreutils").
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

# compiling tools ------------------------------------------------------------

.PHONY: default
default: tools
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) --add-package-link packages/castle_base.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) --add-package-link packages/castle_window.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) --add-package-link packages/castle_components.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) --add-package-link packages/castle_editor_components.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) tools/castle-editor/castle_editor.lpi
# move binaries to bin/
	$(INSTALL) -d bin/
	$(INSTALL) tools/texture-font-to-pascal/texture-font-to-pascal$(EXE_EXTENSION) bin/
	$(INSTALL) tools/image-to-pascal/image-to-pascal$(EXE_EXTENSION) bin/
	$(INSTALL) tools/castle-curves/castle-curves$(EXE_EXTENSION) bin/
	$(INSTALL) tools/build-tool/castle-engine$(EXE_EXTENSION) bin/
	$(INSTALL) tools/to-data-uri/to-data-uri$(EXE_EXTENSION) bin/
	$(INSTALL) tools/castle-editor/castle-editor$(EXE_EXTENSION) bin/

.PHONY: tools
tools:
# Compile build tool first, used to compile other tools and examples
	tools/build-tool/castle-engine_compile.sh
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/castle-curves/ compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/image-to-pascal/ compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/texture-font-to-pascal/ compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/to-data-uri/ compile

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
	  "$(FIND)" . -type f -exec $(INSTALL) --mode 644 -D '{}' $(DATADIR)/castle-engine/'{}' ';'

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

# compiling examples -----------------------------------------------------------

# For GitHub Actions, we need to conserve disk space in some cases, as it is limited to ~15 GB.
#
# When CASTLE_CONSERVE_DISK_SPACE is
# - true -> prefixing command with $(DO_IF_CONSERVE_DISK_SPACE) makes this command execute.
# - anything else -> prefixing command with $(DO_IF_CONSERVE_DISK_SPACE) makes this command not execute.
#
ifeq ($(CASTLE_CONSERVE_DISK_SPACE),true)
DO_IF_CONSERVE_DISK_SPACE:=
else
DO_IF_CONSERVE_DISK_SPACE:=true
endif

# Note that examples with CastleEngineManifest.xml are not listed here.
# They will be found and compiled by a Makefile rule that searches using
# "find ... -iname CastleEngineManifest.xml ..." .

EXAMPLES_BASE_NAMES :=

# Note that src/deprecated_library/castleengine must be compiled before
# cge_dynlib_tester, otherwise linking cge_dynlib_tester will fail.
EXAMPLES_LAZARUS_BASE_NAMES := \
  src/deprecated_library/castleengine \
  examples/deprecated_library/lazarus_library_tester/cge_dynlib_tester

EXAMPLES_UNIX_EXECUTABLES := $(EXAMPLES_BASE_NAMES) \
  $(EXAMPLES_LAZARUS_BASE_NAMES)

EXAMPLES_WINDOWS_EXECUTABLES := $(addsuffix .exe,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .exe,$(EXAMPLES_LAZARUS_BASE_NAMES))

# Test compiling single CGE editor template.
# Requires EDITOR_TEMPLATE_PATH to be defined.
.PHONY: test-editor-template
test-editor-template:
	$(SED) --in-place=.backup \
	  -e 's|standalone_source="$${XmlQuote(PROJECT_PASCAL_NAME)}_standalone.dpr"||' \
	  -e 's|$${XmlQuote(PROJECT_QUALIFIED_NAME)}|test.project.castle.engine.io|' \
	  -e 's|$${XmlQuote(PROJECT_CAPTION)}|Test Template Project Caption|' \
	  -e 's|$${XmlQuote(PROJECT_NAME)}|test_template_project_name|' \
	  $(EDITOR_TEMPLATE_PATH)CastleEngineManifest.xml
	$(SED)  --in-place=.backup \
	  -e 's|$${PROJECT_NAME}|test_template_project_name|' \
	  -e 's|$${MAIN_VIEW}|Main|g' \
	  -e 's|$${MAIN_VIEW_LOWERCASE}|main|g' \
	  $(EDITOR_TEMPLATE_PATH)code/gameinitialize.pas
ifdef CASTLE_HAS_MAIN_VIEW
	$(SED)  --in-place=.backup \
	  -e 's|$${PROJECT_NAME}|test_template_project_name|' \
	  -e 's|$${MAIN_VIEW}|Main|g' \
	  -e 's|$${MAIN_VIEW_LOWERCASE}|main|g' \
	  $(EDITOR_TEMPLATE_PATH)code/gameviewmain.pas
endif
	cp tools/build-tool/data/castleautogenerated_template.pas $(EDITOR_TEMPLATE_PATH)castleautogenerated.pas
	$(SED)  --in-place \
	  -e 's|$${NAME}|test_template_project_name|' \
	  -e 's|$${CAPTION}|Test Template Project Caption|' \
	  -e 's|$${VERSION}|0.1|' \
	  $(EDITOR_TEMPLATE_PATH)castleautogenerated.pas
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project $(EDITOR_TEMPLATE_PATH) compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project $(EDITOR_TEMPLATE_PATH) clean
	mv -f $(EDITOR_TEMPLATE_PATH)CastleEngineManifest.xml.backup $(EDITOR_TEMPLATE_PATH)CastleEngineManifest.xml
	mv -f $(EDITOR_TEMPLATE_PATH)code/gameinitialize.pas.backup $(EDITOR_TEMPLATE_PATH)code/gameinitialize.pas
ifdef CASTLE_HAS_MAIN_VIEW
	mv -f $(EDITOR_TEMPLATE_PATH)code/gameviewmain.pas.backup $(EDITOR_TEMPLATE_PATH)code/gameviewmain.pas
endif
	rm -f $(EDITOR_TEMPLATE_PATH)castleautogenerated.pas

# Test compiling CGE editor templates
.PHONY: test-editor-templates
test-editor-templates:
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/3d_fps_game/files/
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/2d_game/files/
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/empty/files/ CASTLE_HAS_MAIN_VIEW=true
	$(MAKE) --no-print-directory test-editor-template EDITOR_TEMPLATE_PATH=tools/castle-editor/data/project_templates/3d_model_viewer/files/ CASTLE_HAS_MAIN_VIEW=true

.PHONY: examples
examples:
# Compile build tool first, used to compile other tools and examples.
	tools/build-tool/castle-engine_compile.sh

# Compile all examples and tools with CastleEngineManifest.xml inside.
#
# We want this to fail if some application failed to compile.
# Unfortunately find seems to ignore -exec result.
# (see e.g. https://unix.stackexchange.com/questions/392970/how-to-get-the-exit-code-of-commands-started-by-find )
# And we cannot use -execdir.
# We also cannot use find ... | xargs, we need to execute 2 commands for each (compile and clean).
#
# So we use find to generate list of projects, then run using simple "for" over them.
#
# Exceptions: We do not process:
#
# - examples/network/tcp_connection/: because it requires Indy which may not be installed.
#
# - tests/delphi_tests: because it requires Delphi, which is not available on non-Windows.
#
# - examples/deprecated_library: bacause it requires library first (we could build it..
#   but it is already tested by examples-laz).
#
# - tools/build-tool: because
#   - compilation is tested by "make tools" already,
#   - we don't want to clean it, to have it available for "make test-editor-templates" after this
#   - on Windows, we'd have to make a copy of castle-engine, as you cannot replace own exe.
	"$(FIND)" . \
	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -o \
	  '(' -path ./tools/build-tool -prune ')' -o \
	  '(' -path ./tests/delphi_tests -prune ')' -o \
	  '(' -path ./examples/delphi -prune ')' -o \
	  '(' -path ./examples/deprecated_library/lazarus_library_tester -prune ')' -o \
	  '(' -type d -iname castle-engine-output -prune ')' -o \
	  '(' -type f -iname CastleEngineManifest.xml -print ')' > \
	  /tmp/cge-projects.txt
	echo 'Found projects: '`wc -l < /tmp/cge-projects.txt`
	set -e && for MANIFEST in `cat /tmp/cge-projects.txt`; do \
	  echo 'Compiling project '$${MANIFEST}; \
	  $(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project $${MANIFEST} compile; \
	  $(DO_IF_CONSERVE_DISK_SPACE) $(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project $${MANIFEST} clean; \
	done

# Compile editor templates
	 $(MAKE) test-editor-templates

# This a bit simpler than "make examples", it assumes
# - build tool is already build, in $(BUILD_TOOL)
# - doesn't deal with template tests
# - only searches examples/ subdir
.PHONY: examples-delphi
examples-delphi:
	"$(FIND)" ./examples/ \
	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
	  '(' -path ./examples/castlescript/image_make_by_script -prune ')' -o \
	  '(' -path ./examples/localization -prune ')' -o \
	  '(' -path ./examples/network/custom_url_handler -prune ')' -o \
	  '(' -path ./examples/research_special_rendering_methods -prune ')' -o \
	  '(' -path ./examples/visualize_triangulation -prune ')' -o \
	  '(' -path ./examples/audio/audio_player -prune ')' -o \
	  '(' -path ./examples/audio/test_sound_source_allocator -prune ')' -o \
	  '(' -path ./examples/deprecated_random_generator -prune ')' -o \
	  '(' -path ./examples/deprecated_library -prune ')' -o \
	  '(' -path ./examples/lazarus -prune ')' -o \
	  '(' -iname CastleEngineManifest.xml -print ')' > \
	  /tmp/cge-delphi-projects.txt
	echo 'Found projects: '`wc -l < /tmp/cge-delphi-projects.txt`
	set -e && for MANIFEST in `cat /tmp/cge-delphi-projects.txt`; do \
	  echo 'Compiling project '$${MANIFEST}; \
	  $(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project $${MANIFEST} --compiler=delphi compile; \
	done

.PHONY: examples-laz
examples-laz:
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) packages/castle_base.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) packages/castle_window.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) packages/castle_components.lpk
	lazbuild $(CASTLE_LAZBUILD_OPTIONS) packages/castle_editor_components.lpk
	set -e && for PROJECT_LPI in $(EXAMPLES_BASE_NAMES) $(EXAMPLES_LAZARUS_BASE_NAMES); do \
	  ./tools/internal/lazbuild_retry $${PROJECT_LPI}.lpi; \
	done
	"$(FIND)" . \
	  '(' -path ./examples/network/tcp_connection -prune ')' -o \
	  '(' -path ./src/vampyre_imaginglib -prune ')' -o \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -o \
	  '(' -path ./tools/build-tool/tests/data -prune ')' -o \
	  '(' -path ./tools/build-tool/data -prune ')' -o \
	  '(' -path ./examples/deprecated_library -prune ')' -o \
	  '(' -path ./tools/castle-editor/components/mbColorLib/examples -prune ')' -o \
	  '(' -iname '*.lpi' -print ')'  > \
	  /tmp/cge-laz-projects.txt
	echo 'Found projects: '`wc -l < /tmp/cge-laz-projects.txt`
	set -e && for PROJECT_LPI in `cat /tmp/cge-laz-projects.txt`; do \
	  echo 'Compiling project '$${PROJECT_LPI}; \
	  ./tools/internal/lazbuild_retry $${PROJECT_LPI}; \
	  $(DO_IF_CONSERVE_DISK_SPACE) git clean --force -d -x "`dirname $${PROJECT_LPI}`"; \
	done

# Cleanup things in examples/ subdir,
# produced by compilation (using "make examples*" or other methods of compilation)
# and execution of the examples.
.PHONY: cleanexamples
cleanexamples:
	rm -f $(EXAMPLES_UNIX_EXECUTABLES) $(EXAMPLES_WINDOWS_EXECUTABLES)
	rm -Rf \
	  examples/deprecated_library/build-qt_library_tester-* \
	  examples/deprecated_library/lazarus_library_tester/*.app  \
	  examples/fonts/font_draw_over_image_output.png \
	  examples/short_api_samples/transform_save_load/aaa.castle-transform
# lazarus produces lib/ subdirectories during compilation
	"$(FIND)" examples/ -type d -name lib -prune -exec rm -Rf '{}' ';'

# cleaning ------------------------------------------------------------

.PHONY: clean cleanmore cleanall

clean: cleanexamples
	"$(FIND)" . -type f '(' -iname '*.ow'  -or \
	                   -iname '*.ppw' -or \
			   -iname '*.aw' -or \
	                   -iname '*.o'   -or \
			   -iname '*.or'  -or \
			   -iname '*.ppu' -or \
			   '(' -iname '*.a' -and -not -iwholename '*/vampyre_imaginglib/*' ')' -or \
			   '(' -iname '*.res' -and \
			       -not -iwholename '*/vampyre_imaginglib/*' -and \
			       -not -iwholename '*/mbColorLib/*' -and \
			       -not -iwholename '*/examples/delphi/*' ')' \
			       -or \
			   -iname '*.rsj' -or \
			   -iname '*.compiled' -or \
			   -iname '*.lps' -or \
			   -iname '*.libimp*.a' -or \
			   -iname '*.apk' -or \
			   -iname '*.dbg' -or \
	                   -iname '*.dcu' -or \
			   -iname '*.dpu' -or \
			   -iname '*.dproj.local' -or \
			   -iname '*.identcache' -or \
			   -iname '*.rsm' -or \
	                   -iname '*.log' ')' \
	     -print \
	     | xargs rm -f
# Note: *.app directory is a macOS bundle,
# we *do not* remove it here anymore as it would break pack_release.
	"$(FIND)" . -type d '(' -name 'lib' -or \
	                      -name 'backup' -or \
	                      -name 'castle-engine-output' -or \
			      -name '__recovery' ')' \
	     -exec rm -Rf '{}' ';' -prune
	rm -Rf bin/ \
	  castle-engine-copy$(EXE_EXTENSION) \
	  packages/castle_base.pas \
	  packages/castle_window.pas \
	  packages/castle_components.pas \
	  packages/alternative_castle_window_based_on_lcl.pas \
	  tests/test_castle_game_engine \
	  tests/test_castle_game_engine.exe \
	  tests/castle-tester \
	  tests/castle-tester.exe
	$(MAKE) -C doc/man/man1/ clean
# fpmake stuff (binary, units/ produced by fpmake compilation, configs)
	rm -Rf fpmake fpmake.exe units/ *.fpm .fppkg .config
	rm -Rf src/deprecated_library/ios-output/\
	       src/deprecated_library/libcastleengine.dylib \
	       src/deprecated_library/castleengine.dll \
	       src/deprecated_library/libcastleengine.so
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
	"$(FIND)" . \
	  '(' -path ./tools/castle-editor/data/project_templates -prune ')' -or \
	  '(' -path ./tools/build-tool/tests/data -prune ')' -or \
	  '(' -iname CastleEngineManifest.xml \
	      -execdir $(BUILD_TOOL) clean ';' ')'

cleanmore: clean
	"$(FIND)" . -type f '(' -iname '*~' -or \
	                   -iname '*.bak' -or \
	                   -iname '*.~???' -or \
	                   -iname '*.pro.user' -or \
			   -iname '*.blend1' \
			')' -exec rm -f '{}' ';'
	"$(FIND)" . -type d '(' -iname 'backup' \
			')' -exec rm -Rf '{}' ';' -prune
	$(MAKE) -C doc/pasdoc/ clean
	rm -Rf tools/build-tool/data/android/integrated-services/chartboost/app/libs/*.jar \
	       tools/build-tool/data/android/integrated-services/heyzap/app/libs/*.jar \
	       tools/build-tool/data/android/integrated-services/heyzap/app/libs/*.aar \
	       tools/build-tool/data/android/integrated-services/startapp/app/libs/*.jar \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/GameAnalytics.h \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/libGameAnalytics.a
	rm -f castle-engine*.zip tools/internal/pack_release/castle-engine*.zip
	rm -Rf fpc-*.zip tools/contrib/fpc/

cleanall: cleanmore

# tests ----------------------------------------

# Build and run tests.
.PHONY: tests
tests:
	tools/build-tool/castle-engine_compile.sh
# Build and run check_lazarus_packages
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/internal/check_lazarus_packages/ clean
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/internal/check_lazarus_packages/ --mode=debug compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/internal/check_lazarus_packages/ run -- ../../../
# Run in debug mode
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ clean
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ --mode=debug --compiler-option=-dNO_WINDOW_SYSTEM compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ run -- --console
# Run in debug mode, testing DecimalSeparator = comma
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ clean
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ --mode=debug --compiler-option=-dNO_WINDOW_SYSTEM --compiler-option=-dCASTLE_TEST_DECIMAL_SEPARATOR_COMMA compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ run -- --console
# Run in debug mode without LibPng
# (useful to test image processing, e.g. TTestImages.TestLoadImage, without libpng, which matters for mobile now)
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ clean
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ --mode=debug --compiler-option=-dNO_WINDOW_SYSTEM --compiler-option=-dCASTLE_DISABLE_LIBPNG compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ run -- --console
# Run in release mode, since all tests must pass the same when optimizations are enabled
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ clean
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ --mode=release --compiler-option=-dNO_WINDOW_SYSTEM compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tests/ run -- --console
# Run tests in tools/build-tool/tests
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/build-tool/tests/ clean
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/build-tool/tests/ --mode=debug compile
	$(BUILD_TOOL) $(CASTLE_ENGINE_TOOL_OPTIONS) --project tools/build-tool/tests/ run -- --all

# fpmake ---------------------------------------------------------------------

.PHONY: build-using-fpmake
build-using-fpmake:
	fpc fpmake.pp
	@echo 'Running fpmake.'
	@echo '  If this fails saying that "rtl" is not found -> set FPCDIR environment variable, see http://wiki.freepascal.org/FPMake .'
	@echo '  If this fails saying that "opengl" package is not found -> you maybe have broken FPC installation without Package.fpc files, consider using "export CASTLE_PACKAGE_NO_DEPENDENCIES=true" as a workaround.'
# Workaround FPC >= 3.x problem (bug?) --- it ignores $FPCDIR, but --globalunitdir works
	if [ '(' -n "$(FPCDIR)" ')' ]; then \
	   ./fpmake --globalunitdir="$(FPCDIR)"; \
	else \
	   ./fpmake; \
	fi

# Full test that fpmake compilation process works
# (see https://castle-engine.io/fpmake )
.PHONY: test-fpmake
test-fpmake: build-using-fpmake
# Test fpmake with --nofpccfg, to make sure our dependencies in fpmake.pp are correct
	./fpmake clean --verbose
	if [ '(' -n "$(FPCDIR)" ')' ]; then \
	   ./fpmake --globalunitdir="$(FPCDIR)" --nofpccfg --verbose; \
	else \
	   ./fpmake --nofpccfg --verbose; \
	fi

# eof ------------------------------------------------------------
