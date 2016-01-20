{ Some OpenGL defines. }

{$ifdef MSWINDOWS}
  {$define OPENGL_CALL := stdcall;}
  {$define OPENGL_CALLBACK_CALL := stdcall;}
  {$define OPENGL_STDCALL}
  {$define OPENGL_CALLBACK_STDCALL}
{$else}
  {$ifdef UNIX}
    {$define OPENGL_CALL := cdecl;}
    {$define OPENGL_CALLBACK_CALL := cdecl;}
    {$define OPENGL_CDECL}
    {$define OPENGL_CALLBACK_CDECL}
  {$else}
    {$fatal Define OPENGL_CALLBACK_CALL in OpenGLMac.inc}
  {$endif}
{$endif}
