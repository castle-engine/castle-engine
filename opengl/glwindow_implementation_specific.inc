{ Include files appropriate for current GLWindow implementation. }

{$ifdef GLWINDOW_GLUT}
  {$I glwindow_glut.inc}
{$endif}

{$ifdef GLWINDOW_WINAPI}
  {$I glwindow_winapi.inc}
  {$I glwindow_winsystem.inc}
{$endif}

{$ifdef GLWINDOW_XLIB}
  {$I glwindow_xlib.inc}
  {$I glwindow_winsystem.inc}
{$endif}

{$ifdef GLWINDOW_GTK_ANY}
  {$I glwindow_gtk.inc}
{$endif}

{$ifdef GLWINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  {$I glwindow_private_modifiers_down.inc}
{$endif}