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
#     about the kambi_* packages (compile them from Lazarus first).
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
#    by VRML engine or any game for anything).

EXAMPLES_BASE_NAMES := \
  examples/audio/algets \
  examples/audio/alplay \
  examples/audio/doppler_demo \
  examples/audio/efx_demo \
  examples/base/dircleaner \
  examples/base/test_platform_specific_utils \
  examples/kambiscript/kambi_calc \
  examples/kambiscript/image_make_by_script \
  examples/base/stringoper \
  examples/images/image_convert \
  examples/images/dds_decompose \
  examples/images/image_identify \
  examples/images/image_to_pas \
  examples/images/image_compare \
  examples/glwindow/gl_win_events \
  examples/glwindow/menu_test_alternative \
  examples/glwindow/menu_test \
  examples/glwindow/test_glwindow_gtk_mix \
  examples/glwindow/test_font_break \
  examples/glwindow/multi_glwindow \
  examples/glwindow/multi_texturing_demo \
  examples/glwindow/shading_langs/shading_langs_demo \
  examples/glwindow/demo_camera \
  examples/glwindow/simple_video_editor \
  examples/glwindow/bezier_surfaces/animate_surface \
  examples/glwindow/bezier_surfaces/design_surface \
  examples/glwindow/interpolated_curves \
  examples/3d/draw_space_filling_curve \
  examples/vrml/triangulate_demo \
  examples/vrml/test_blender_exported_hierarchy \
  examples/vrml/tools/gen_light_map \
  examples/vrml/tools/kanim_to_interpolators \
  examples/vrml/multiple_viewports \
  examples/vrml/demo_animation \
  examples/vrml/fog_culling \
  examples/vrml/bump_mapping/bump_mapping \
  examples/vrml/radiance_transfer/radiance_transfer \
  examples/vrml/radiance_transfer/precompute_radiance_transfer \
  examples/vrml/radiance_transfer/show_sh \
  examples/vrml/plane_mirror_and_shadow \
  examples/vrml/change_vrml_by_code \
  examples/vrml/change_vrml_by_code_2 \
  examples/vrml/vrml_browser_script_compiled \
  examples/vrml/simplest_vrml_browser \
  examples/vrml/simplest_vrml_browser_with_shadow_volumes \
  examples/vrml/shadow_fields/precompute_shadow_field \
  examples/vrml/shadow_fields/shadow_fields \
  examples/vrml/dynamic_ambient_occlusion/dynamic_ambient_occlusion \
  examples/vrml/gl_primitive_performance \
  examples/vrml/terrain/terrain \
  examples/vrml/scene_manager_demos \
  examples/vrml/scene_manager_basic \
  examples/vrml/build_3d_object_by_code

EXAMPLES_LAZARUS_BASE_NAMES := \
  examples/audio/test_al_source_allocator \
  examples/lazarus/vrml_browser/vrml_browser \
  examples/lazarus/vrml_with_2d_controls/vrml_with_2d_controls \
  examples/lazarus/camera/camera \
  examples/lazarus/load_model_and_camera_manually/load_model_and_camera_manually \
  tests/test_kambi_units

EXAMPLES_UNIX_EXECUTABLES := $(EXAMPLES_BASE_NAMES) \
  $(EXAMPLES_LAZARUS_BASE_NAMES)

EXAMPLES_WINDOWS_EXECUTABLES := $(addsuffix .exe,$(EXAMPLES_BASE_NAMES)) \
  $(addsuffix .exe,$(EXAMPLES_LAZARUS_BASE_NAMES))

.PHONY: examples
examples:
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh && ) true

.PHONY: examples-ignore-errors
examples-ignore-errors:
	$(foreach NAME,$(EXAMPLES_BASE_NAMES),$(NAME)_compile.sh ; ) true

.PHONY: cleanexamples
cleanexamples:
	rm -f $(EXAMPLES_UNIX_EXECUTABLES) $(EXAMPLES_WINDOWS_EXECUTABLES)

.PHONY: examples-laz
examples-laz:
	lazbuild packages/kambi_base.lpk
	lazbuild packages/kambi_glwindow.lpk
	lazbuild packages/kambi_components.lpk
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
	rm -Rf packages/lib/ \
	  packages/kambi_base.pas \
	  packages/kambi_glwindow.pas \
	  packages/kambi_components.pas \
	  tests/test_kambi_units tests/test_kambi_units.exe
# fpmake binary, and units/ produced by fpmake compilation
	rm -Rf fpmake fpmake.exe units/
	$(MAKE) -C doc/pasdoc/ clean
# lazarus produces lib/ subdirectories during compilation
	find examples/ -type d -name lib -prune -exec rm -Rf '{}' ';'
# some .res files that are known to be useless
	rm -f examples/base/dircleaner.res \
	      examples/lazarus/camera/camera.res \
	      examples/vrml/triangulate_demo.res \
	      examples/vrml/simplest_vrml_browser_with_shadow_volumes.res

cleanmore: clean
	find . -type f '(' -iname '*~' -or \
	                   -iname '*.bak' -or \
	                   -iname '*.~???' -or \
			   -iname '*.blend1' \
			')' -exec rm -f '{}' ';'

cleanall: cleanmore

# Clean compiled versions of GLWindow unit.
# Makes sure that unit GLWindow will be *always* *rebuild* in next compilation.
#
# This is useful, since GLWindow unit may be compiled with various
# back-ends (e.g. under Unices two most useful back-ends
# are XLIB and GTK). To make sure that compilation of some program
# will produce exactly what you need, it's useful to force rebuild of GLWindow.
#
# Of course this means that compilation time will suffer a little,
# since GLWindow unit will be possibly rebuild without any real need.
clean-glwindow:
	rm -f src/glwindow/glwindow.o \
	      src/glwindow/glwindow.ppu \
	      src/glwindow/GLWindow.o \
	      src/glwindow/GLWindow.ppu

# ----------------------------------------
# Set SVN tag.

svntag:
	source ../www/scripts/update_archives/generated_versions.sh && \
	  svn copy https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine \
	           https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/tags/kambi_vrml_game_engine/"$$GENERATED_VERSION_KAMBI_VRML_GAME_ENGINE" \
	  -m "Tagging the $$GENERATED_VERSION_KAMBI_VRML_GAME_ENGINE version of 'Kambi VRML game engine'."

# eof ------------------------------------------------------------
