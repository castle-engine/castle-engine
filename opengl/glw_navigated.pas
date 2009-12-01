unit GLW_Navigated;

interface

uses SysUtils, GLWindow;

var
  Glw: TGLWindowNavigated;

implementation

initialization
 Glw := TGLWindowNavigated.Create(nil);
finalization
 FreeAndNil(Glw);
end.
