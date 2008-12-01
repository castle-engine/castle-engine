#!/bin/bash
set -eu

precompute_shadow_field --light sphere.wrl
precompute_shadow_field --light stretched_cube.wrl
precompute_shadow_field humanoid_stand.wrl
precompute_shadow_field teapot.x3dv
