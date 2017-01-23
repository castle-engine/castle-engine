.PHONY: default
default: castle_game_engine_icon.png

.PHONY: clean
clean:
	rm -f castle_game_engine_icon.png

castle_game_engine_icon.png: castle_game_engine_icon.svg
	inkscape $< \
	  --export-width=256 \
	  --export-height=256 \
	  --export-background=white \
	  --export-background-opacity=0 \
	  --export-png=$@

# Unused anymore.
# This made sense cause old IE didn't interpret alpha channel in PNG Ok.
# Fortunately, IE is dead now:)
#
# castle_game_engine_icon_nontransparent.png: castle_game_engine_icon.svg
# 	inkscape $< \
# 	  --export-width=256 \
# 	  --export-height=256 \
# 	  --export-background=white \
# 	  --export-background-opacity=255 \
# 	  --export-png=$@
