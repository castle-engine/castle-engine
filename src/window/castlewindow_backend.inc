{ Include files appropriate for current CastleWindow implementation. }

{ In various include files here, we dereference memory allocated
  with various libraries, so pointer checks will make false errors. }
{$checkpointer off}

{$ifdef CASTLE_ENGINE_PLUGIN}
  {$I castlewindow_npapi.inc}
{$endif}

{$ifdef CASTLE_WINDOW_WINAPI}
  {$I castlewindow_winapi.inc}
  {$I castlewindow_winsystem.inc}
{$endif}

{$ifdef CASTLE_WINDOW_XLIB}
  {$I castlewindow_xlib.inc}
  {$I castlewindow_winsystem.inc}
{$endif}

{$ifdef CASTLE_WINDOW_GTK_ANY}
  {$I castlewindow_gtk.inc}
{$endif}

{$ifdef CASTLE_WINDOW_USE_XF86VMODE}
  {$I castlewindow_xf86vmode.inc}
{$endif CASTLE_WINDOW_USE_XF86VMODE}

{$ifdef CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  {$I castlewindow_private_modifiers_down.inc}
{$endif}

{$ifdef CASTLE_WINDOW_LCL}
  {$I castlewindow_lcl.inc}
{$endif}

{$ifdef CASTLE_WINDOW_ANDROID}
  {$I castlewindow_android.inc}
{$endif}

{$ifdef CASTLE_WINDOW_LIBRARY}
  {$I castlewindow_library.inc}
{$endif}

{$ifdef CASTLE_WINDOW_TEMPLATE}
  {$I castlewindow_backend_template.inc}
{$endif}

{$ifdef KAMBI_CHECK_POINTER}
{ Turn checkpointer back on }
{$checkpointer on}
{$endif}
