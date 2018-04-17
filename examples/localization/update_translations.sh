#!/bin/bash
set -eu

do_language ()
{
  echo "Generating MO file for language ${1}:"
  msgfmt --verbose po_files/game."$1".po --output-file=data/locale/game."$1".mo
}

do_language en
do_language de
do_language pl
do_language ru
do_language ua

echo 'Everything done OK.'
