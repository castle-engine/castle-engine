#!/bin/bash
set -eu

TEXTUREFONT2PASCAL='texture-font-to-pascal'

# Add option to have common Unicode characters available (not just ASCII),
# this way we cover many Latin languages out-of-the-box,
# see https://gist.github.com/ivandrofly/0fe20773bd712b303f78 .
# Note: CGE applications can always define fonts that use more glyphs,
# e.g. to support Japanese or Chinese see https://castle-engine.io/manual_text.php .
TEXTUREFONT2PASCAL="${TEXTUREFONT2PASCAL} --sample-text-file=common_unicode_chars.txt"

# Some texture-font-to-pascal options are useful for debugging what's going on.
#TEXTUREFONT2PASCAL="${TEXTUREFONT2PASCAL} --debug-log --debug-font-image"

# Used by Text node in X3DNodes. This font is drawn in 3D,
# always automatically stretched to desired size, so you can freely
# adjust TEXT_NODE_FONT_SIZE to balance the font texture size vs nice font look.
#
# The default value is the maximum value where we still fit all in (at most)
# 2048x2048 texture.
# We deliberately use unit and function names that make the unit/function indendepent of the size,
# so that changing sizes used by default by CGE can be easily done by changing +
# running this shell script.
TEXT_NODE_FONT_SIZE=25
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSans.ttf --unit-name CastleTextureFont_Default3D_Sans    --function-name Font_Default3D_Sans

# For other 11 font variants used by X3D,
# see view3dscene view3dscene/embedded_data/fonts/ .
# We no longer generate / embed them in CGE core.

# Used by FallbackFont and (default) UIFont.
# Note that changing the default size here will change the default font size in UI,
# so we actually cannot do this, to not break compatibility.
$TEXTUREFONT2PASCAL --size 20 data/DejaVuSans.ttf --unit-name CastleTextureFont_DefaultUi --function-name Font_DefaultUi
