{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This a trivial 2D application described at the beginning
  of Castle Game Engine manual:
  https://castle-engine.io/manual_quick_2d_game.php
}

program quick_2d_game;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, CastleWindow, CastleGLImages, CastleFilesUtils, CastleKeysMouse;

var
  Window: TCastleWindowBase;
  Image: TDrawableImage;
  X: Single = 0.0;
  Y: Single = 0.0;

procedure WindowRender(Container: TUIContainer);
begin
  Image.Draw(X, Y);
end;

procedure WindowUpdate(Container: TUIContainer);
var
  SecondsPassed: Single;
begin
  SecondsPassed := Container.Fps.SecondsPassed;
  Y := Y + SecondsPassed * 100.0;
  if Container.Pressed[keyArrowLeft] then
    X := X - SecondsPassed * 200.0;
  if Container.Pressed[keyArrowRight] then
    X := X + SecondsPassed * 200.0;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(keySpace) then
    Y := Y - 200.0;
end;

begin
  Image := TDrawableImage.Create('castle-data:/my_image.png');
  try
    Window := TCastleWindowBase.Create(Application);
    Window.OnRender := @WindowRender;
    Window.OnUpdate := @WindowUpdate;
    Window.OnPress := @WindowPress;
    Window.Open;
    Application.Run;
  finally FreeAndNil(Image) end;
end.
