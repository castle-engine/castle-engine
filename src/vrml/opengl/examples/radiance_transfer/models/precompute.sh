#!/bin/bash
set -eu

SH_BASIS=25
RAYS_PER_VERTEX=10000

# precompute_radiance_transfer \
#   ../../../../../castle/data/creatures/spider_queen/spider_queen_stand.wrl \
#    ugly-low-poly/spider_queen_radiance.wrl \
#   --sh-basis-count="$SH_BASIS" \
#   --rays-per-vertex="$RAYS_PER_VERTEX"

# precompute_radiance_transfer \
#   ../../../../../castle/data/creatures/alien/alien_still.wrl \
#    ugly-low-poly/alien_radiance.wrl \
#   --sh-basis-count="$SH_BASIS" \
#   --rays-per-vertex="$RAYS_PER_VERTEX"

precompute_radiance_transfer \
  chinchilla.wrl.gz chinchilla_with_prt.wrl \
  --sh-basis-count="$SH_BASIS" \
  --rays-per-vertex="$RAYS_PER_VERTEX"
gzip -f chinchilla_with_prt.wrl

precompute_radiance_transfer \
  chinchilla.wrl.gz chinchilla_with_prt_rays1000.wrl \
  --sh-basis-count="$SH_BASIS" \
  --rays-per-vertex=1000
gzip -f chinchilla_with_prt_rays1000.wrl

precompute_radiance_transfer \
  horns.wrl horns_with_prt.wrl \
  --sh-basis-count="$SH_BASIS" \
  --rays-per-vertex="$RAYS_PER_VERTEX"
gzip -f horns_with_prt.wrl

precompute_radiance_transfer \
  towers.wrl towers_with_prt.wrl \
  --sh-basis-count="$SH_BASIS" \
  --rays-per-vertex="$RAYS_PER_VERTEX"
gzip -f towers_with_prt.wrl
