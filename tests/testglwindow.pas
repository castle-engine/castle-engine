{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestGLWindow;

interface

uses fpcunit, testutils, testregistry;

type
  TTestGLWindow = class(TTestCase)
  published
    procedure Test1;
  end;

implementation

uses SysUtils, GLWindow;

procedure TTestGLWindow.Test1;
var
  Window: TGLUIWindow;
begin
  Window := TGLUIWindow.Create(nil);
  try
    Window.Open;
    Window.Close;
  finally FreeAndNil(Window) end;
end;

initialization
  RegisterTest(TTestGLWindow);
end.
