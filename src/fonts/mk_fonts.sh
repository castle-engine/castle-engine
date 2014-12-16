#!/bin/bash
set -eu

# Used by X3DNodes.
# The regular sans version is also used by CastleControls.UIFont.
texturefont2pascal --size 20 data/DejaVuSans-BoldOblique.ttf      --unit-name CastleTextureFont_DjvSansBO_20
texturefont2pascal --size 20 data/DejaVuSans-Bold.ttf             --unit-name CastleTextureFont_DjvSansB_20
texturefont2pascal --size 20 data/DejaVuSans-Oblique.ttf          --unit-name CastleTextureFont_DjvSansO_20
texturefont2pascal --size 20 data/DejaVuSans.ttf                  --unit-name CastleTextureFont_DjvSans_20
texturefont2pascal --size 20 data/DejaVuSansMono-BoldOblique.ttf  --unit-name CastleTextureFont_DjvMonoBO_20
texturefont2pascal --size 20 data/DejaVuSansMono-Bold.ttf         --unit-name CastleTextureFont_DjvMonoB_20
texturefont2pascal --size 20 data/DejaVuSansMono-Oblique.ttf      --unit-name CastleTextureFont_DjvMonoO_20
texturefont2pascal --size 20 data/DejaVuSansMono.ttf              --unit-name CastleTextureFont_DjvMono_20
texturefont2pascal --size 20 data/DejaVuSerif-BoldItalic.ttf      --unit-name CastleTextureFont_DjvSerifBI_20
texturefont2pascal --size 20 data/DejaVuSerif-Bold.ttf            --unit-name CastleTextureFont_DjvSerifB_20
texturefont2pascal --size 20 data/DejaVuSerif-Italic.ttf          --unit-name CastleTextureFont_DjvSerifI_20
texturefont2pascal --size 20 data/DejaVuSerif.ttf                 --unit-name CastleTextureFont_DjvSerif_20

# Used by CastleControls.UIFontSmall
texturefont2pascal --size 10 data/DejaVuSans.ttf

# Used by CastleControls as MessageFont, for TCastleDialog
texturefont2pascal --size 18 data/DejaVuSansMono.ttf

# Used by view3dscene
texturefont2pascal --size 15 data/DejaVuSansMono-Bold.ttf
