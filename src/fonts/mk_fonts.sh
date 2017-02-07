#!/bin/bash
set -eu

TEXTUREFONT2PASCAL='texture-font-to-pascal'
# Some texture-font-to-pascal options are useful for debugging what's going on.
#TEXTUREFONT2PASCAL='texture-font-to-pascal --debug-log --debug-font-image'

# Used by Text node in X3DNodes. This font is drawn in 3D,
# always automatically stretched to desired size, so you can freely
# adjust TEXT_NODE_FONT_SIZE to balance the font texture size vs nice font look.
# The default value is the maximum value where we still fit on 256x256 texture.
TEXT_NODE_FONT_SIZE=20
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSans-BoldOblique.ttf      --unit-name CastleTextureFont_DjvSansBO_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSans-Bold.ttf             --unit-name CastleTextureFont_DjvSansB_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSans-Oblique.ttf          --unit-name CastleTextureFont_DjvSansO_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSans.ttf                  --unit-name CastleTextureFont_DjvSans_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSansMono-BoldOblique.ttf  --unit-name CastleTextureFont_DjvMonoBO_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSansMono-Bold.ttf         --unit-name CastleTextureFont_DjvMonoB_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSansMono-Oblique.ttf      --unit-name CastleTextureFont_DjvMonoO_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSansMono.ttf              --unit-name CastleTextureFont_DjvMono_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSerif-BoldItalic.ttf      --unit-name CastleTextureFont_DjvSerifBI_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSerif-Bold.ttf            --unit-name CastleTextureFont_DjvSerifB_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSerif-Italic.ttf          --unit-name CastleTextureFont_DjvSerifI_"${TEXT_NODE_FONT_SIZE}"
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" data/DejaVuSerif.ttf                 --unit-name CastleTextureFont_DjvSerif_"${TEXT_NODE_FONT_SIZE}"

# Used by CastleControls.UIFont.
# (already created above, as TEXT_NODE_FONT_SIZE matches our desired size for this)
# $TEXTUREFONT2PASCAL --size 20 data/DejaVuSans.ttf

# Used by CastleControls.UIFontSmall
$TEXTUREFONT2PASCAL --size 10 data/DejaVuSans.ttf

# Used by CastleControls as MessageFont, for TCastleDialog
$TEXTUREFONT2PASCAL --size 18 data/DejaVuSansMono.ttf

# Used by view3dscene
$TEXTUREFONT2PASCAL --size 15 data/DejaVuSansMono-Bold.ttf
