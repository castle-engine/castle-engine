{ some OpenGL Macros }

{ Define this to automatically use GL, GLU, GLExt FPC units everywhere
  instead of my OpenGLh unit. }
{$define USE_GL_GLU_UNITS}

{$ifdef USE_GL_GLU_UNITS}
  {$define OpenGLh := gl, glu, glext}
{$endif}

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
