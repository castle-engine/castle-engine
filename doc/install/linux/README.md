To install on Linux or FreeBSD (any desktop Unix using Xorg and following standard XDG specifications)
you can use the files provided here.
They will make the "Castle Game Engine" nicely listed in the applications list
of any desktop (GNOME, MATE, KDE, ...).

1. To make the desktop file work, first make sure that the system can find the relevant executables.
   It is simplest to just edit the desktop file to add full path to the "Exec" and "Icon" lines,
   like this:

     Exec=/home/michalis/castle-engine/bin/castle-editor
     Icon=/home/michalis/castle-engine/doc/pasdoc/logo/castle_game_engine_icon.svg

   Alternative ways are possible, e.g. you can symlink executables to /usr/local/bin

     sudo ln -s /home/michalis/castle-engine/bin/castle-editor /usr/local/bin/castle-editor
     sudo ln -s /home/michalis/castle-engine/bin/view3dscene /usr/local/bin/view3dscene

   Alternative is to put the programs on $PATH
   (but make sure that X session manager actually sees the $PATH;
   it is *not* enough to add the modified $PATH to .bashrc ).

2. Run this:

     desktop-file-install --dir=$HOME/.local/share/applications castle-editor.desktop
     update-desktop-database -q ~/.local/share/applications/ # or just relogin

   Alternative: instead of running desktop-file-install, you can just copy the desktop file
   to ~/.local/share/applications/ . desktop-file-install doesn't do much more.
   You could also do

     install -d ~/.local/share/applications/
     install --mode 644 castle-editor.desktop ~/.local/share/applications/

   And running update-desktop-database is actually not necessary on new systems.
