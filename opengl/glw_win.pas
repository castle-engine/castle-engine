unit GLW_win;

interface

uses SysUtils, GLWindow;

var
  { The only purpose of this unit is to provide Glw object of class TGLWindow,
    initializaed in initialization and freed in finalization.

    If you wish you @italic(can) modify Glw variable
    (no standard unit will use @name unit, this is only for end-programs)
    --- but remember that in finalization
    we do FreeAndNil(Glw). So if you want to free Glw object you should than
    clear Glw variable (or just do FreeAndNil(Glw)). }
  Glw: TGLWindow;

implementation

initialization
 Glw := TGLWindow.Create(nil);
finalization
 FreeAndNil(Glw);
end.
