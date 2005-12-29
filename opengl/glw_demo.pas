unit GLW_Demo;

interface

uses SysUtils, GLWindow;

var 
  { @noAutoLinkHere }
  Glw: TGLWindowDemo;

implementation

initialization
 Glw := TGLWindowDemo.Create;
finalization
 FreeAndNil(Glw);
end.
