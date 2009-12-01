unit GLW_Demo;

interface

uses SysUtils, GLWindow;

var 
  Glw: TGLWindowDemo;

implementation

initialization
 Glw := TGLWindowDemo.Create(nil);
finalization
 FreeAndNil(Glw);
end.
