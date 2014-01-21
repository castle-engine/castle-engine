#!/bin/bash
set -eu

do_outlinefont2pascal ()
{
  outlinefont2pascal "$@" --dir .
}

mk_4_outline_versions ()
{
  do_outlinefont2pascal "$@"
  do_outlinefont2pascal "$@" -i 1
  do_outlinefont2pascal "$@" -b 1
  do_outlinefont2pascal "$@" -b 1 -i 1
}

# Outline fonts used by X3DNodes
mk_4_outline_versions --font-name 'Bitstream Vera Sans'
mk_4_outline_versions --font-name 'Bitstream Vera Sans Mono'
mk_4_outline_versions --font-name 'Bitstream Vera Serif'

# Used by CastleControls.UIFont
texturefont2pascal --size 20 data/DejaVuSans.ttf

# Used by CastleControls.UIFontSmall
texturefont2pascal --size 10 data/DejaVuSans.ttf

# Used by CastleControls as MessageFont, for TCastleDialog
texturefont2pascal --size 18 data/DejaVuSansMono.ttf

# Used by view3dscene
texturefont2pascal --size 15 data/DejaVuSansMono-Bold.ttf
