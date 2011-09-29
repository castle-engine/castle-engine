This is a simple VRML browser in Lazarus, based on TCastleControl component
of our engine. Something like "mini-view3dscene" using Lazarus.

Possibly real view3dscene in the future will be based on Lazarus.
This will not happen soon (TOpenGLControl lacks stable GTK 2 support,
anti-aliasing, and see various quirks in "TODO" section below ---
this is all much better/stable with our TCastleWindow).
But it's possible in the future.

Compiling:

- Install standard Lazarus package with TOpenGLControl

- Compile and install packages castle_base and castle_components,
  see ../../../packages/

- Then just open and compile this program as usual from Lazarus.

Usage:

- Open any 3D file supported by view3dscene (VRML 1.0, 2.0, X3D, 3DS ---
  see view3dscene docs [http://castle-engine.sourceforge.net/view3dscene.php]).
  You can open using "File -> Open" menu item
  or just pass filename on command-line.

- Navigation in the scene follows NavigationInfo.type encoded in VRML.
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

------------------------------------------------------------------------------
Old notes:

- At some point, I had a FocusableControl hack implemented in
  TCastleControlCustom to allow VRML browser control to receive key events.
  Reason: TOpenGLControl couldn't catch focus, so when I placed some
  focusable controls (edit boxes, buttons)
  on the form, it was not possible to pass key presses to GLControl.

  So I created FFocusableControl that could have focus, but was not visible
  --- so I set it's size to minimum (1, 1 in Lazarus)
  (I can't set Visible to false, then it would not be focusable).
  Then the only purpose of FFocusableControl is to call
  appropriate GLControl events.

  Fortunately, with Lazarus 0.9.26, it seems that all these problems
  are gone. TOpenGLControl can receive normal focus
  (reacts to SetFocus, has working DoExit, receives key events).
  So the whole hack with FocusableControl is removed.
  See revision 3847 for last revision with FocusableControl present.
  Tested with: GTK1, WinAPI.

Michalis
