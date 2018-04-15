#!/bin/bash
set -eu

# Include all the special Polish, German, Russian and Ukrainian chars to include in a font.
SAMPLE_TEXT='ŚĆĘĄŹŁŻÓŃśćęąźłżóńÄäÖöÜüẞßйцукенгшщзхъфывапролджэячсмитьбюёЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁїієґЇІЄҐ'

# Note that we create font with a large size (50),
# this makes it look good in the 3D view when the letters are huge.

texture-font-to-pascal DejaVuSans.ttf \
  --size 50 \
  --unit-name Font_DejaVuSans \
  --sample-text "${SAMPLE_TEXT}"
mv -f font_dejavusans.pas ../
