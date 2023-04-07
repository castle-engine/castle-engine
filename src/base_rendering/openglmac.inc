{ Some OpenGL defines. }

{$if defined(MSWINDOWS)}
  {$define OPENGL_STDCALL}
  {$define OPENGL_CALLBACK_STDCALL}
{$elseif defined(UNIX)}
  {$define OPENGL_CDECL}
  {$define OPENGL_CALLBACK_CDECL}
{$elseif defined(WASI)}
  // TODO: WebAssembly: what should be calling convention for OpenGL callbacks?
  {$define OPENGL_CDECL}
  {$define OPENGL_CALLBACK_CDECL}
{$else}
  {$fatal Define OPENGL_CALLBACK_CALL in OpenGLMac.inc}
{$endif}
