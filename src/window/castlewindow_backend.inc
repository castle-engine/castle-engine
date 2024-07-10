{%MainUnit castlewindow.pas}

{ Include files appropriate for current CastleWindow implementation. }

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

{$ifdef CASTLE_WINDOW_FORM}
  {$I castlewindow_form.inc}
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

{$ifdef CASTLE_WINDOW_COCOA}
  {$I castlewindow_cocoa.inc}
{$endif}

