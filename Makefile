# Most useful targets:
#
#   all (default target) --
#     Compile all units, uses fpmake.
#
#   examples --
#     Compile examples and tools (inside examples/ subdirectory).
#     This compilation method uses our xxx_compile.sh Unix scripts,
#     and requires only pure FPC installation.
#     Lazarus is not required (LCL dependent examples are not compiled).
#     Note that you can also compile each example separately,
#     just execute directly appropriate xxx_compile.sh scripts.
#
#   examples-laz --
#     Compile examples and tools (inside examples/ subdirectory).
#     This compilation method uses our .lpi project files,
#     and compiles every program by the lazbuild utility.
#     Lazarus and FPC installation is required, and Lazarus must know
#     about the castle_* packages (compile them from Lazarus first).
#
#   clean --
#     Delete FPC 1.0.x Windows trash (*.ppw, *.ow), FPC trash, Delphi trash,
#     Lazarus trash (*.compiled),
#     binaries of example programs,
#     also FPC compiled trash in packages/*/lib/.
#
# Not-so-commonly-useful targets:
#
#   cleanmore --
#     Same as clean, but also delete:
#     - Emacs backup files (*~) and
#     - Delphi backup files (*.~???)
#     - pasdoc generated documentation in doc/pasdoc/ and doc/reference/
#     - closed-source libs you may have left in tools/build-tool/data
#     - QtCreator *.pro.user
#     This is a useful step when packing the release of CGE.
#
#   cleanall --
#     Same as cleanmore for now.
#     Intention is to remove *everything* that can be manually recreated,
#     even if somewhat hard, and clean editor backup.

# Hack for Cygwin, to avoid using Windows built-in "find" program.
#FIND:=/bin/find
FIND:=find

# compile ------------------------------------------------------------

.PHONY: all
all:
	$(MAKE) --no-print-directory build-using-fpmake
	tools/texture-font-to-pascal/texture-font-to-pascal_compile.sh
	tools/image-to-pascal/image-to-pascal_compile.sh
	tools/castle-curves/castle-curves_compile.sh
	tools/build-tool/castle-engine_compile.sh
	tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d_compile.sh

.PHONY: build-using-fpmake
build-using-fpmake:
	fpc fpmake.pp
	@echo 'Running fpmake. If this fails saying that "rtl" is not found, remember to set FPCDIR environment variable, see http://wiki.freepascal.org/FPMake .'
# Workaround FPC >= 3.x problem (bug?) --- it ignores $FPCDIR, but --globalunitdir works
	if [ '(' -n "$(FPCDIR)" ')' -a \
	     '(' $(shell fpc -iV) '!=' '2.6.4' ')' -a \
	     '(' $(shell fpc -iV) '!=' '2.6.2' ')' ]; then \
	   ./fpmake --globalunitdir="$(FPCDIR)"; \
	else \
	   ./fpmake; \
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
	install -d $(BINDIR)
	install tools/texture-font-to-pascal/texture-font-to-pascal $(BINDIR)
	install tools/image-to-pascal/image-to-pascal $(BINDIR)
	install tools/castle-curves/castle-curves $(BINDIR)
	install tools/build-tool/castle-engine $(BINDIR)
	install tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d $(BINDIR)
#	cp -R tools/build-tool/data $(DATADIR)/castle-engine
	install -d  $(DATADIR)
	cd tools/build-tool/data/ && \
	  $(FIND) . -type f -exec install --mode 644 -D '{}' $(DATADIR)/castle-engine/'{}' ';'

.PHONY: uninstall
uninstall:
	rm -f  $(BINDIR)/texture-font-to-pascal \
	       $(BINDIR)/image-to-pascal \
	       $(BINDIR)/castle-curves \
	       $(BINDIR)/castle-engine \
	       $(BINDIR)/sprite-sheet-to-x3d
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

EXAMPLES_BASE_NAMES := \
  examples/audio/algets \
  examples/audio/alplay \
  examples/audio/doppler_demo \
  examples/audio/efx_demo \
  examples/tools/dircleaner \
  examples/tools/stringoper \
  examples/tools/castle_download \
  examples/tools/to_data_uri \
  examples/castlescript/castle_calculator \
  examples/castlescript/image_make_by_script \
  examples/images_videos/image_convert \
  examples/images_videos/dds_decompose \
  examples/images_videos/image_identify \
  examples/images_videos/image_paint \
  examples/images_videos/image_compare \
  examples/images_videos/simple_video_editor \
  examples/images_videos/drawing_modes_test \
  examples/images_videos/draw_images_on_gpu \
  examples/images_videos/test_castleimage_draw3x3 \
  examples/images_videos/image_render_custom_shader \
  examples/joystick/joystick_demo \
  examples/fonts/test_font_break \
  examples/fonts/font_from_texture \
  examples/fonts/test_local_characters/test_local_characters \
  examples/fonts/html_text \
  examples/fonts/font_draw_over_image \
  examples/tiled/tiled_demo_standalone \
  examples/window/window_events \
  examples/window/window_menu \
  examples/window/window_gtk_mix \
  examples/window/multi_window \
  examples/curves/simplest_curve_read \
  examples/space_filling_curve/draw_space_filling_curve \
  examples/research_special_rendering_methods/radiance_transfer/radiance_transfer \
  examples/research_special_rendering_methods/radiance_transfer/precompute_radiance_transfer \
  examples/research_special_rendering_methods/radiance_transfer/show_sh \
  examples/research_special_rendering_methods/dynamic_ambient_occlusion/dynamic_ambient_occlusion \
  examples/terrain/terrain \
  examples/3d_rendering_processing/triangulate_demo \
  examples/3d_rendering_processing/placeholder_names \
  examples/3d_rendering_processing/multiple_viewports \
  examples/3d_rendering_processing/fog_culling \
  examples/3d_rendering_processing/animate_3d_model_by_code \
  examples/3d_rendering_processing/animate_3d_model_by_code_2 \
  examples/3d_rendering_processing/call_pascal_code_from_3d_model_script \
  examples/3d_rendering_processing/view_3d_model_advanced \
  examples/3d_rendering_processing/scene_manager_demos \
  examples/3d_rendering_processing/view_3d_model_basic \
  examples/3d_rendering_processing/build_3d_object_by_code \
  examples/3d_rendering_processing/build_3d_tunnel \
  examples/3d_rendering_processing/combine_multiple_x3d_into_one \
  examples/3d_rendering_processing/display_box_custom_shaders \
  examples/3d_rendering_processing/listen_on_x3d_events \
  examples/3d_rendering_processing/cars_demo \
  examples/3d_rendering_processing/render_3d_to_texture_and_use_as_quad \
  examples/3d_rendering_processing/show_bounding_rect_in_2d \
  src/x3d/teapot/teapot_3d_to_pascal \
  src/x3d/nodes_specification/x3d-nodes-to-pascal/x3d-nodes-to-pascal \
  examples/3d_sound_game/lets_take_a_walk \
  examples/resource_animations/resource_animations \
  examples/fps_game/fps_game \
  examples/2d_standard_ui/show_various_ui_controls/show_various_ui_controls \
  examples/2d_standard_ui/edit_test/edit_test \
  examples/2d_standard_ui/timer_test/timer_test \
  examples/2d_standard_ui/zombie_fighter/zombie_fighter \
  examples/2d_standard_ui/quick_2d_game/quick_2d_game \
  examples/mobile/simple_3d_demo/simple_3d_demo_standalone \
  tools/image-to-pascal/image-to-pascal \
  tools/texture-font-to-pascal/texture-font-to-pascal \
  tools/castle-curves/castle-curves \
  tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d \
  examples/random_generator/globalrandom \
  examples/random_generator/random_speed_test \
  examples/random_generator/random_threads_test \
  examples/localization/localization_test

EXAMPLES_LAZARUS_BASE_NAMES := \
  examples/audio/test_al_source_allocator \
  examples/audio/audio_player_scrubber/audio_player_scrubber \
  examples/lazarus/model_3d_viewer/model_3d_viewer \
  examples/lazarus/model_3d_with_2d_controls/model_3d_with_2d_controls \
  examples/lazarus/load_model_and_camera_manually/load_model_and_camera_manually \
  examples/lazarus/two_controls/two_controls \
  examples/lazarus/quick_2d_game/quick_2d_game_lazarus \
  tests/test_castle_game_engine \
  src/library/castleengine \
  examples/library/lazarus_library_tester/cge_dynlib_tester \
  examples/random_generator/graphics_random_test \
  tools/build-tool/code/castle-engine

EXAMPLES_UNIX_EXECUTABLES := $(EXAMPLES_BASE_NAMES) \
  $(EXAMPLES_LAZARUS_BASE_NAMES)

EXAMPLES_WINDOWS_EXECUTABLES := $(addsuffix .exe,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .exe,$(EXAMPLES_LAZARUS_BASE_NAMES))

EXAMPLES_MACOSX_APPS := $(addsuffix .app,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .app,$(EXAMPLES_LAZARUS_BASE_NAMES))

EXAMPLES_RES_FILES := $(addsuffix .res,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .res,$(EXAMPLES_LAZARUS_BASE_NAMES))

.PHONY: examples
examples:
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh && ) true
# compile all examples with CastleEngineManifest.xml inside
	$(FIND) . -iname CastleEngineManifest.xml -execdir castle-engine $(CASTLE_ENGINE_TOOL_OPTIONS) compile ';'

.PHONY: examples-ignore-errors
examples-ignore-errors:
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh ; ) true

.PHONY: cleanexamples
cleanexamples:
	rm -f $(EXAMPLES_UNIX_EXECUTABLES) $(EXAMPLES_WINDOWS_EXECUTABLES) $(EXAMPLES_RES_FILES)
	rm -Rf $(EXAMPLES_MACOSX_APPS)

.PHONY: examples-laz
examples-laz:
	lazbuild packages/castle_base.lpk
	lazbuild packages/castle_window.lpk
	lazbuild packages/castle_components.lpk
	$(foreach NAME,$(EXAMPLES_BASE_NAMES) $(EXAMPLES_LAZARUS_BASE_NAMES),lazbuild $(NAME).lpi && ) true

# Compile only Lazarus-specific examples (that depend on LCL)
.PHONY: examples-only-laz
examples-only-laz:
	lazbuild packages/castle_base.lpk
	lazbuild packages/castle_window.lpk
	lazbuild packages/castle_components.lpk
	$(foreach NAME,$(EXAMPLES_LAZARUS_BASE_NAMES),lazbuild $(NAME).lpi && ) true

# cleaning ------------------------------------------------------------

.PHONY: clean cleanmore cleanall

clean: cleanexamples
	$(FIND) . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
			   -iname '*.or'  -or \
			   -iname '*.rsj' -or \
			   -iname '*.compiled' -or \
			   -iname '*.lps' -or \
			   -iname '*.libimp*.a' -or \
			   -iname '*.apk' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' -or \
	                   -iname '*.log' ')' \
	     -print \
	     | xargs rm -f
	$(FIND) . -type d '(' -name 'lib' -or \
	                      -name 'castle-engine-output' ')' \
	     -exec rm -Rf '{}' ';' -prune
	rm -Rf packages/castle_base.pas \
	  packages/castle_window.pas \
	  packages/castle_components.pas \
	  packages/alternative_castle_window_based_on_lcl.pas \
	  tests/test_castle_game_engine \
	  tests/test_castle_game_engine.exe \
	  examples/mobile/drawing_toy/drawing_toy \
	  examples/mobile/drawing_toy/drawing_toy.exe \
	  examples/portable_game_skeleton/my_fantastic_game \
	  examples/portable_game_skeleton/my_fantastic_game.exe \
	  examples/fonts/font_draw_over_image_output.png
	$(MAKE) -C doc/man/man1/ clean
# fpmake binary, and units/ produced by fpmake compilation
	rm -Rf fpmake fpmake.exe units/ *.fpm
# lazarus produces lib/ subdirectories during compilation
	$(FIND) examples/ -type d -name lib -prune -exec rm -Rf '{}' ';'
	rm -Rf src/library/ios-output/\
	       src/library/libcastleengine.dylib \
	       src/library/castleengine.dll \
	       src/library/libcastleengine.so
# clean every project with CastleEngineManifest.xml
	$(FIND) . -iname CastleEngineManifest.xml -execdir castle-engine clean ';'

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
	       tools/build-tool/data/android/integrated-services/startapp/app/libs/*.jar \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/GameAnalytics.h \
	       tools/build-tool/data/ios/services/game_analytics/cge_project_name/game_analytics/libGameAnalytics.a

cleanall: cleanmore

# Clean compiled versions of CastleWindow unit.
# Makes sure that unit CastleWindow will be *always* *rebuild* in next compilation.
#
# This is useful, since CastleWindow unit may be compiled with various
# back-ends (e.g. under Unices two most useful back-ends
# are XLIB and GTK). To make sure that compilation of some program
# will produce exactly what you need, it's useful to force rebuild of CastleWindow.
#
# Of course this means that compilation time will suffer a little,
# since CastleWindow unit will be possibly rebuild without any real need.
clean-window:
	rm -f src/window/castlewindow.o \
	      src/window/castlewindow.ppu \
	      src/window/CastleWindow.o \
	      src/window/CastleWindow.ppu

# ----------------------------------------
# Set SVN tag.

# Don't use anymore, we use GIT now.

# svntag:
# 	source ../www/pack/generated_versions.sh && \
# 	  svn copy https://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine \
# 	           https://svn.code.sf.net/p/castle-engine/code/tags/castle_game_engine/"$$GENERATED_VERSION_CASTLE_GAME_ENGINE" \
# 	  -m "Tagging the $$GENERATED_VERSION_CASTLE_GAME_ENGINE version of 'Castle Game Engine'."

# eof ------------------------------------------------------------
