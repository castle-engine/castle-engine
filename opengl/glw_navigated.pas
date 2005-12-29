unit GLW_Navigated;

interface

uses SysUtils, GLWindow;

var
  { @noAutoLinkHere }
  Glw: TGLWindowNavigated;

implementation

initialization
 Glw := TGLWindowNavigated.Create;
finalization
 FreeAndNil(Glw);
end.
