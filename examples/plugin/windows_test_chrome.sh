# Helper script to run Google Chrome with some debug options.
# Adjust as necessary for your system!

# See https://www.chromium.org/for-testers/enable-logging
# Be sure to enable NPAPI first on chrome://enable-flags

"c:/Program Files/Google/Chrome/Application/chrome.exe" --log-level 3 --debug-plugin-loading -log-plugin-messages  --enable-logging --in-process-plugins file:///D:/michalis/sources/castle-engine/trunk/castle_game_engine/examples/plugin/index_win.html

# And look for Chrome logs in
#   (win 7)           c:/Users/mkamburelis/AppData/Local/Google/Chrome/User Data/chrome_debug.log
#   (win xp chrome)   c:/Documents and Settings/m/Ustawienia lokalne/Dane aplikacji/Google/Chrome/User Data/chrome_debug.log
#   (win xp chromium) c:/Documents and Settings/m/Ustawienia lokalne/Dane aplikacji/Chromium/User Data/chrome_debug.log

# It would be cool to be able to use
#   --single-process
# but it doesn't seem to work with any Google Chrome build I found.
