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
#     also FPC compiled trash in packages/*/lib/,
#     and finally pasdoc generated documentation in doc/pasdoc/
#
# Not-so-commonly-useful targets:
#
#   cleanmore --
#     Same as clean + delete Emacs backup files (*~) and Delphi backup files
#     (*.~??? (using *.~* would be too unsafe ?))
#
#   cleanall --
#     Same as cleanmore for now.
#     Intention is to remove *everything* that can be manually recreated,
#     even if somewhat hard, and clean editor backup.

# compiling ------------------------------------------------------------

.PHONY: all
all:
	fpc fpmake.pp
	@echo 'Running fpmake. If this fails saying that "rtl" is not found, remember to set FPCDIR environment variable, see http://wiki.freepascal.org/FPMake .'
	./fpmake

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
  examples/castlescript/castle_calculator \
  examples/castlescript/image_make_by_script \
  examples/images_videos/image_convert \
  examples/images_videos/dds_decompose \
  examples/images_videos/image_identify \
  examples/images_videos/image_to_pas \
  examples/images_videos/image_compare \
  examples/images_videos/simple_video_editor \
  examples/fonts/test_font_break \
  examples/window/window_events \
  examples/window/window_menu \
  examples/window/window_gtk_mix \
  examples/window/multi_window \
  examples/curves_surfaces/bezier_surfaces/animate_surface \
  examples/curves_surfaces/bezier_surfaces/design_surface \
  examples/curves_surfaces/interpolated_curves \
  examples/space_filling_curve/draw_space_filling_curve \
  examples/bump_mapping/bump_mapping \
  examples/radiance_transfer/radiance_transfer \
  examples/radiance_transfer/precompute_radiance_transfer \
  examples/radiance_transfer/show_sh \
  examples/shadow_fields/precompute_shadow_field \
  examples/shadow_fields/shadow_fields \
  examples/dynamic_ambient_occlusion/dynamic_ambient_occlusion \
  examples/terrain/terrain \
  examples/3d_rendering_processing/custom_3d_object \
  examples/3d_rendering_processing/triangulate_demo \
  examples/3d_rendering_processing/test_blender_exported_hierarchy \
  examples/3d_rendering_processing/tools/gen_light_map \
  examples/3d_rendering_processing/tools/kanim_to_interpolators \
  examples/3d_rendering_processing/multiple_viewports \
  examples/3d_rendering_processing/demo_animation \
  examples/3d_rendering_processing/fog_culling \
  examples/3d_rendering_processing/plane_mirror_and_shadow \
  examples/3d_rendering_processing/animate_3d_model_by_code \
  examples/3d_rendering_processing/animate_3d_model_by_code_2 \
  examples/3d_rendering_processing/call_pascal_code_from_3d_model_script \
  examples/3d_rendering_processing/view_3d_model_advanced \
  examples/3d_rendering_processing/gl_primitive_performance \
  examples/3d_rendering_processing/scene_manager_demos \
  examples/3d_rendering_processing/view_3d_model_basic \
  examples/3d_rendering_processing/build_3d_object_by_code \
  src/x3d/teapot/teapot_3d_to_pascal \
  examples/fixed_camera_game/rift \
  examples/isometric_game/sandbox \
  examples/images_videos/fft/gaussian_write

EXAMPLES_LAZARUS_BASE_NAMES := \
  examples/audio/test_al_source_allocator \
  examples/lazarus/model_3d_viewer/model_3d_viewer \
  examples/lazarus/model_3d_with_2d_controls/model_3d_with_2d_controls \
  examples/lazarus/load_model_and_camera_manually/load_model_and_camera_manually \
  tests/test_castle_game_engine

EXAMPLES_UNIX_EXECUTABLES := $(EXAMPLES_BASE_NAMES) \
  $(EXAMPLES_LAZARUS_BASE_NAMES)

EXAMPLES_WINDOWS_EXECUTABLES := $(addsuffix .exe,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .exe,$(EXAMPLES_LAZARUS_BASE_NAMES))

EXAMPLES_RES_FILES := $(addsuffix .res,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .res,$(EXAMPLES_LAZARUS_BASE_NAMES))

.PHONY: examples
examples:
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh && ) true

.PHONY: examples-ignore-errors
examples-ignore-errors:
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh ; ) true

.PHONY: cleanexamples
cleanexamples:
	rm -f $(EXAMPLES_UNIX_EXECUTABLES) $(EXAMPLES_WINDOWS_EXECUTABLES) $(EXAMPLES_RES_FILES)

.PHONY: examples-laz
examples-laz:
	lazbuild packages/castle_base.lpk
	lazbuild packages/castle_window.lpk
	lazbuild packages/castle_components.lpk
	$(foreach NAME,$(EXAMPLES_BASE_NAMES) $(EXAMPLES_LAZARUS_BASE_NAMES),lazbuild $(NAME).lpi && ) true

# cleaning ------------------------------------------------------------

.PHONY: clean cleanmore cleanall

clean: cleanexamples
	find . -type f '(' -iname '*.ow'  -or -iname '*.ppw' -or -iname '*.aw' -or \
	                   -iname '*.o'   -or -iname '*.ppu' -or -iname '*.a' -or \
			   -iname '*.or'  -or \
			   -iname '*.compiled' -or \
			   -iname '*.libimp*.a' -or \
	                   -iname '*.dcu' -or -iname '*.dpu' ')' \
	     -print \
	     | xargs rm -f
	find . -type d -name lib -exec rm -Rf '{}' ';' -prune
	rm -Rf packages/castle_base.pas \
	  packages/castle_window.pas \
	  packages/castle_components.pas \
	  tests/test_castle_game_engine tests/test_castle_game_engine.exe
# fpmake binary, and units/ produced by fpmake compilation
	rm -Rf fpmake fpmake.exe units/
	$(MAKE) -C doc/pasdoc/ clean
# lazarus produces lib/ subdirectories during compilation
	find examples/ -type d -name lib -prune -exec rm -Rf '{}' ';'

cleanmore: clean
	find . -type f '(' -iname '*~' -or \
	                   -iname '*.bak' -or \
	                   -iname '*.~???' -or \
			   -iname '*.blend1' \
			')' -exec rm -f '{}' ';'

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

svntag:
	source ../www/scripts/update_archives/generated_versions.sh && \
	  svn copy https://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine \
	           https://svn.code.sf.net/p/castle-engine/code/tags/castle_game_engine/"$$GENERATED_VERSION_CASTLE_GAME_ENGINE" \
	  -m "Tagging the $$GENERATED_VERSION_CASTLE_GAME_ENGINE version of 'Castle Game Engine'."

# eof ------------------------------------------------------------
