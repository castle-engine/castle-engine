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
#     - pasdoc generated documentation in doc/pasdoc/
#     - closed-source libs you may have left in tools/build-tool/data
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
	tools/texturefont2pascal/texturefont2pascal_compile.sh
	tools/image2pascal/image2pascal_compile.sh
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
# See http://castle-engine.sourceforge.net/engine.php for documentation.
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
	install tools/texturefont2pascal/texturefont2pascal $(BINDIR)
	install tools/image2pascal/image2pascal $(BINDIR)
	install tools/castle-curves/castle-curves $(BINDIR)
	install tools/build-tool/castle-engine $(BINDIR)
	install tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d $(BINDIR)
#	cp -R tools/build-tool/data $(DATADIR)/castle-engine
	cd tools/build-tool/data/ && \
	  $(FIND) . -type f -exec install -D '{}' $(DATADIR)/castle-engine/'{}' ';'

.PHONY: uninstall
uninstall:
	rm -f  $(BINDIR)/texturefont2pascal \
	       $(BINDIR)/image2pascal \
	       $(BINDIR)/castle-curves \
	       $(BINDIR)/castle-engine \
	       $(BINDIR)/sprite-sheet-to-x3d
	rm -Rf $(DATADIR)/castle-engine

# Strip libraries that cannot be distributed in Debian package of CGE for now,
# because they (possibly) cannot be recompiled by Debian software right now
# (or maybe they can, but noone had time to try it yet, and wrap in a script).
# This concerns some Windows and Android libs.
.PHONY: strip-precompiled-libraries
strip-precompiled-libraries:
	rm -Rf tools/build-tool/data/external_libraries/ \
	       tools/build-tool/data/android/integrated-components/sound/

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
  examples/images_videos/image_compare \
  examples/images_videos/simple_video_editor \
  examples/images_videos/drawing_modes_test \
  examples/joystick/joystick_demo \
  examples/fonts/test_font_break \
  examples/fonts/font_from_texture \
  examples/fonts/html_text \
  examples/tiled/tiled_demo_standalone \
  examples/window/window_events \
  examples/window/window_menu \
  examples/window/window_gtk_mix \
  examples/window/multi_window \
  examples/curves_surfaces/bezier_surfaces/animate_surface \
  examples/curves_surfaces/bezier_surfaces/design_surface \
  examples/curves_surfaces/interpolated_curves \
  examples/space_filling_curve/draw_space_filling_curve \
  examples/research_special_rendering_methods/radiance_transfer/radiance_transfer \
  examples/research_special_rendering_methods/radiance_transfer/precompute_radiance_transfer \
  examples/research_special_rendering_methods/radiance_transfer/show_sh \
  examples/research_special_rendering_methods/shadow_fields/precompute_shadow_field \
  examples/research_special_rendering_methods/shadow_fields/shadow_fields \
  examples/research_special_rendering_methods/dynamic_ambient_occlusion/dynamic_ambient_occlusion \
  examples/terrain/terrain \
  examples/3d_rendering_processing/custom_3d_object \
  examples/3d_rendering_processing/triangulate_demo \
  examples/3d_rendering_processing/placeholder_names \
  examples/3d_rendering_processing/tools/gen_light_map \
  examples/3d_rendering_processing/tools/castle_anim_frames_to_interpolators \
  examples/3d_rendering_processing/multiple_viewports \
  examples/3d_rendering_processing/fog_culling \
  examples/research_special_rendering_methods/plane_mirror_and_shadow/plane_mirror_and_shadow \
  examples/3d_rendering_processing/animate_3d_model_by_code \
  examples/3d_rendering_processing/animate_3d_model_by_code_2 \
  examples/3d_rendering_processing/call_pascal_code_from_3d_model_script \
  examples/3d_rendering_processing/view_3d_model_advanced \
  examples/3d_rendering_processing/scene_manager_demos \
  examples/3d_rendering_processing/view_3d_model_basic \
  examples/3d_rendering_processing/build_3d_object_by_code \
  examples/3d_rendering_processing/build_3d_tunnel \
  examples/3d_rendering_processing/combine_multiple_x3d_into_one \
  examples/3d_rendering_processing/listen_on_x3d_events \
  examples/3d_rendering_processing/cars_demo \
  examples/3d_rendering_processing/render_3d_to_texture_and_use_as_quad \
  src/x3d/teapot/teapot_3d_to_pascal \
  src/x3d/nodes_specification/x3d_nodes_spec_to_pascal/x3d_nodes_spec_to_pascal \
  src/x3d/nodes_specification/generate_x3d_nodes_helpers/generate_x3d_nodes_to_pascal \
  examples/fixed_camera_game/rift \
  examples/isometric_game/sandbox \
  examples/3d_sound_game/lets_take_a_walk \
  examples/3d_sound_game/data/levels/base/devel/process_base_b \
  examples/resource_animations/resource_animations \
  examples/fps_game/fps_game \
  examples/2d_standard_ui/show_various_ui_controls/show_various_ui_controls \
  examples/2d_standard_ui/timer_test/timer_test \
  examples/2d_standard_ui/zombie_fighter/zombie_fighter \
  examples/android/android_demo/androiddemo_standalone \
  tools/build-tool/castle-engine \
  tools/image2pascal/image2pascal \
  tools/texturefont2pascal/texturefont2pascal \
  tools/castle-curves/castle-curves \
  tools/sprite-sheet-to-x3d/sprite-sheet-to-x3d \
  examples/random_generator/random_speed_test \
  examples/random_generator/random_threads_test

EXAMPLES_LAZARUS_BASE_NAMES := \
  examples/audio/test_al_source_allocator \
  examples/lazarus/model_3d_viewer/model_3d_viewer \
  examples/lazarus/model_3d_with_2d_controls/model_3d_with_2d_controls \
  examples/lazarus/load_model_and_camera_manually/load_model_and_camera_manually \
  examples/lazarus/two_controls/two_controls \
  tests/test_castle_game_engine \
  src/library/castleengine \
  examples/library/lcl_dynlib_tester/cge_dynlib_tester \
  examples/random_generator/graphics_random_test

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
	                   -iname '*.log' -or \
			   -iname 'castleengine.dll' -or -iname 'libcastleengine.so' ')' \
	     -print \
	     | xargs rm -f
	$(FIND) . -type d -name lib -exec rm -Rf '{}' ';' -prune
	rm -Rf packages/castle_base.pas \
	  packages/castle_window.pas \
	  packages/castle_components.pas \
	  packages/alternative_castle_window_based_on_lcl.pas \
	  tests/test_castle_game_engine \
	  tests/test_castle_game_engine.exe \
	  examples/android/drawing_toy/drawing_toy \
	  examples/android/drawing_toy/drawing_toy.exe \
	  examples/portable_game_skeleton/my_fantastic_game \
	  examples/portable_game_skeleton/my_fantastic_game.exe \
	  examples/fonts/font_draw_over_image_output.png
# fpmake binary, and units/ produced by fpmake compilation
	rm -Rf fpmake fpmake.exe units/ *.fpm
# lazarus produces lib/ subdirectories during compilation
	$(FIND) examples/ -type d -name lib -prune -exec rm -Rf '{}' ';'
# clean every project with CastleEngineManifest.xml
	$(FIND) . -iname CastleEngineManifest.xml -execdir castle-engine clean ';'

cleanmore: clean
	$(FIND) . -type f '(' -iname '*~' -or \
	                   -iname '*.bak' -or \
	                   -iname '*.~???' -or \
			   -iname '*.blend1' \
			')' -exec rm -f '{}' ';'
	$(MAKE) -C doc/pasdoc/ clean
	rm -Rf tools/build-tool/data/android/integrated-components/google_play_services/google-play-services_lib/ \
	       tools/build-tool/data/android/integrated-components/google_play_services/libs/*.jar \
	       tools/build-tool/data/android/integrated-components/giftiz/GiftizSDKLibrary/ \
	       tools/build-tool/data/android/integrated-components/chartboost/libs/*.jar \
	       tools/build-tool/data/android/integrated-components/heyzap/AudienceNetwork/ \
	       tools/build-tool/data/android/integrated-components/heyzap/unity-ads/ \
	       tools/build-tool/data/android/integrated-components/heyzap/libs/*.jar \
	       tools/build-tool/data/android/integrated-components/startapp/libs/*.jar \
	       tools/build-tool/data/android/integrated-components/game_analytics/libs/*.jar \
	       tools/build-tool/data/android/integrated-components/game_analytics/jni/*/*.so

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
