{ A couple of Windows WGL extensions.

  TODO: Will be submitted to be incorporated in FPC Windows unit. }
unit KambiWGL;

TODO: use this.

 { calculate WglARBMultisample --- is WGL_ARB_multisample suported }
 WglARBMultisample := false;
 if wglGetExtensionsStringARB <> nil then
 begin
   WglARBMultisample := glext_ExtensionSupported('WGL_ARB_multisample',
     wglGetExtensionsStringARB(TODO: Hdc));
 end;


interface

uses Windows;

const
  { WGL_ARB_multisample extension: }
  WGL_SAMPLE_BUFFERS_ARB  = $2041;
  WGL_SAMPLES_ARB         = $2042;

var
  { Functions below will be initialized in initialization of this unit,
    set to @nil if not available. }

  { WGL_ARB_extensions_string extension: }
  wglGetExtensionsStringARB: function (HDC hdc): PChar; stdcall;

implementation

initialization
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
end.