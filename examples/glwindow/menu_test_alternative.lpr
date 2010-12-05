{
  Copyright 2005-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo that you can change TGLWindow.MainMenu value at runtime,
  this is a simple way to temporary change whole menu structure
  to something else.
}

program menu_test_alternative;

uses GL, GLU, GLWindow;

var
  Window: TGLWindow;
  FirstMainMenu, SecondMainMenu: TMenu;

procedure Draw(Window: TGLWindow);
begin
  glClear(GL_COlOR_BUFFER_BIT);
end;

procedure MenuCommand(Window: TGLWindow; Item: TMenuItem);
begin
 case Item.IntData of
  1: Window.MainMenu := FirstMainMenu;
  2: Window.MainMenu := SecondMainMenu;
  else Writeln('You clicked menu item "', Item.Caption, '"');
 end;
end;

var M: TMenu;
begin
 Window := TGLWindow.Create(Application);
 try
  { init FirstMainMenu }
  FirstMainMenu := TMenu.Create('Main menu');
  M := TMenu.Create('First menu');
    M.Append(TMenuItem.Create('Change to second menu', 2));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Foo', 10, 'f'));
    M.Append(TMenuItem.Create('Bar', 11));
    FirstMainMenu.Append(M);

  { init SecondMainMenu }
  SecondMainMenu := TMenu.Create('Main menu');
  M := TMenu.Create('Second menu');
    M.Append(TMenuItem.Create('Change back to first menu', 1));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Xyz' , 20, 'x'));
    M.Append(TMenuItem.Create('Blah', 21));
    SecondMainMenu.Append(M);

  { init glw properties related to menu }
  Window.OwnsMainMenu := false;
  Window.MainMenu := FirstMainMenu;
  Window.OnMenuCommand := @MenuCommand;

  { init other glw properties }
  Window.OnResize := @Resize2D;
  Window.OnDraw := @Draw;
  Window.Width := 300;
  Window.Height := 300;
  Window.ParseParameters;
  Window.DepthBufferBits := 0;
  Window.Caption := 'Test changing MainMenu';

  { start }
  Window.OpenAndRun;
 finally
  Window.MainMenu := nil;
  FirstMainMenu.Free;
  SecondMainMenu.Free;
 end;
end.