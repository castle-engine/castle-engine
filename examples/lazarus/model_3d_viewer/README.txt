This is a simple 3D model viewer (VRML/X3D browser, and viewer for other
3D formats) in Lazarus, based on TCastleControl component
of our engine. Something like "mini-view3dscene" using Lazarus.

Possibly real view3dscene in the future will be based on Lazarus.
There are still small problems with Lazarus OpenGL control
(event log sometimes gets clogged and mouse look stutters,
lack of advanced OpenGL context initialization flags -- multi-sampling,
OpenGL 3 etc.) that for now make our TCastleWindow slightly better than Lazarus
form + Lazarus OpenGL control.
But it's possible in the future.

Compiling:

- Install standard Lazarus package with TOpenGLControl

- Compile and install packages castle_base and castle_components,
  see ../../../packages/

- Then just open and compile this program as usual from Lazarus.

Usage:

- Open any 3D file supported by view3dscene (X3D, VRML 1.0/2.0, Collada, 3DS ---
  see view3dscene docs [http://castle-engine.sourceforge.net/view3dscene.php]).
  You can open using "File -> Open" menu item
  or just pass filename on command-line.

- Navigation in the scene follows NavigationInfo.type encoded in VRML/X3D.
  (if none, Examine will be used).
  See view3dscene docs for keys to control Walk/Examine navigation
  [http://castle-engine.sourceforge.net/view3dscene.php], or just try
  moving with arrows, mouse etc.
  Remember that you must have focus on 3D area to use them ---
  press Escape (menu item "View -> Switch focus to 3D area") when needed.

- You can also change camera vectors by hand if you like ---
  just modify values in edit boxes at the bottom and press "Change".

----------------------------------------
TODO:

- Ctrl+O key shortcut doesn't work when standing on TEdit boxes

- GLControl.ReleaseAllKeysAndMouse should be called when
  user expands the menu with mouse. How to catch this?

- When Position was poDefault the form kept getting small size
  each time when I open it under Windows. It seems that under Windows
  when Position = poDefault, it is used like that even in design-time
  (in Linux/GTK it was working correctly).
  Investigate, submit Lazazarus bug.

Michalis
