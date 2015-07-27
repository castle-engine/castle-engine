#!/bin/bash
set -eu

sudo chmod a+rwX /usr/lib/mozilla/plugins/
cp -f npalienoutpost.linux-i386.so /usr/lib/mozilla/plugins/
echo 'Installed to /usr/lib/mozilla/plugins/'
