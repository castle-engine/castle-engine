#!/bin/bash
set -eu

# include all the special chars to include in font
SAMPLE_TEXT='你好世界ΓειασουκόσμεЗдравствуймирŚĆĘĄŹŁŻÓŃśćęąźłżóń'

texture-font-to-pascal 'data/DejaVuSans.ttf'        --size 20 --unit-name Font_DejaVuSans        --sample-text "${SAMPLE_TEXT}"
texture-font-to-pascal 'data/DroidSansFallback.ttf' --size 20 --unit-name Font_DroidSansFallback --sample-text "${SAMPLE_TEXT}"
