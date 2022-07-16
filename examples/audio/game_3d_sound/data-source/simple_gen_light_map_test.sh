#!/bin/bash

cd ../../vrml/
gen_light_map base.wrl /tmp/base_shadowed.rgbe \
    128 128 \
    -4.55 -4.57 0.5 \
    5.45 -4.57 0.5 \
    5.45 4.23 0.5 \
    -4.55 4.23 0.5 \
    0 0 1