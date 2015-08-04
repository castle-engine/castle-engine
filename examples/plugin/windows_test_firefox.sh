# disabled dom.ipc.something (see firebreath docs) in about:config for single process

export MOZ_CRASHREPORTER_DISABLE=1
/cygdrive/c/Program\ Files/Mozilla\ Firefox/firefox.exe  --console --jsconsole  --no-remote -P test
#d:/michalis/firefox-40-debug/firefox/firefox.exe  --console --jsconsole  --no-remote -P test
# open
# file:///D:/michalis/sources/castle-engine/trunk/castle_game_engine/examples/plugin/index_win.html
