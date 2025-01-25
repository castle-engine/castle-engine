{ Some OpenGL defines. }

{ OPENGL_CALLBACK_xxx define calling convention for OpenGL callbacks.
  Right now this is only used for CallbackGLDebug with glDebugMessageCallback. }
{$if defined(MSWINDOWS)}
  {$define OPENGL_CALLBACK_STDCALL}
{$elseif defined(UNIX)}
  {$define OPENGL_CALLBACK_CDECL}
{$elseif defined(WASI)}
  {$define OPENGL_CALLBACK_DEFAULT}
{$else}
  {$message fatal 'Define OPENGL_CALLBACK_xxx in openglmac.inc for this platform'}
{$endif}
