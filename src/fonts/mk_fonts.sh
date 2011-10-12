#!/bin/bash
set -eu

do_font2pascal ()
{
  font2pascal "$@" --dir .
}

mk_4_versions ()
{
  do_font2pascal "$@"
  do_font2pascal "$@" -i 1
  do_font2pascal "$@" -b 1
  do_font2pascal "$@" -b 1 -i 1
}

# Used by VRMLNodes
mk_4_versions --font-name 'Bitstream Vera Sans'
mk_4_versions --font-name 'Bitstream Vera Sans Mono'
mk_4_versions --font-name 'Bitstream Vera Serif'

# Used by CastleNotifications and GLProgress
do_font2pascal --font-name 'Bitstream Vera Sans' --grab-to bfnt

# Used by GLWinMessages
do_font2pascal --font-name 'Bitstream Vera Sans Mono' --font-height -18 --grab-to bfnt

# Used by view3dscene
do_font2pascal --font-name 'Bitstream Vera Sans Mono' --font-height -15 --grab-to bfnt -b 1

# Used by GLMenu
do_font2pascal --font-name 'Bitstream Vera Sans' --grab-to bfnt --font-height -10
