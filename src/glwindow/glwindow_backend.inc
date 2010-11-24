{ Include files appropriate for current GLWindow implementation. }

{ In various include files here, we dereference memory allocated
  with various libraries, so pointer checks will make false errors. }
{$checkpointer off}

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

{$ifdef GLWINDOW_USE_XF86VMODE}
  {$I glwindow_xf86vmode.inc}
{$endif GLWINDOW_USE_XF86VMODE}

{$ifdef GLWINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  {$I glwindow_private_modifiers_down.inc}
{$endif}

{$ifdef GLWINDOW_TEMPLATE}
  {$I glwindow_backend_template.inc}
{$endif}

{$ifdef KAMBI_CHECK_POINTER}
{ Turn checkpointer back on }
{$checkpointer on}
{$endif}
