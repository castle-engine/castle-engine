#!/bin/bash
set -eu

# Call this from ../../ (or just use `make examples').

fpc -dRELEASE @kambi.cfg glwindow/examples/test_menu_change_from_keyup.pasprogram
