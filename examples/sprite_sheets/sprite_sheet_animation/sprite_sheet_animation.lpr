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

{ Demo of rendering using TSprite. See README.md. }
program sprite_sheet_animation;

uses SysUtils,
  CastleWindow, CastleGLImages, CastleControls, CastleVectors,
  CastleColors, CastleTimeUtils, CastleRectangles;

var
  Window: TCastleWindowBase;
  Sprite: TSprite;
  X, Y: Single;

procedure Initialize;
var
  Background: TCastleRectangleControl;
begin
  Background := TCastleRectangleControl.Create(Application);
  Background.Color := Vector4(0.2, 0.2, 0.2, 1);
  Background.FullSize := true;
  Window.Controls.InsertFront(Background);

  { $define TEST_FRAMES_PER_SECOND_24}
  {$ifdef TEST_FRAMES_PER_SECOND_24}
    // Sprite sheet version with 24 frames per second
    Sprite := TSprite.CreateFrameSize('castle-data:/hero_animation/hero_sprite_sheet_24fps.png',
      23, // frames count
      6, // columns (number of frames in a row in the image)
      291, 477, // size of a single frame
      true, // smooth scaling
      true // time loop
    );
    Sprite.FramesPerSecond := 24;
  {$else}
    // Sprite sheet version with 60 frames per second
    Sprite := TSprite.CreateFrameSize('castle-data:/hero_animation/hero_sprite_sheet_60fps.png',
      55, // frames count
      12, // columns (number of frames in a row in the image)
      296, 477, // size of a single frame
      true, // smooth scaling
      true // time loop
    );
    Sprite.FramesPerSecond := 60;
  {$endif}

  Sprite.Play; // calling Sprite.Update will move animation forward

  // Default position
  X := 0;
  Y := 100;
end;

procedure WindowUpdate(Container: TUIContainer);
var
  SecondsPassed: TFloatTime;
begin
  SecondsPassed := Container.Fps.SecondsPassed;
  Sprite.Update(SecondsPassed);

  X := X + SecondsPassed * 200;
  if X > Container.Width then X := 0;
end;

procedure WindowRender(Container: TUIContainer);
const
  Scale = 1;
begin
  Sprite.Draw(FloatRectangle(X, Y,
    Sprite.FrameWidth * Scale,
    Sprite.FrameHeight * Scale));
  UIFont.Print(10, 10, Yellow, 'FPS: ' + Container.Fps.ToString);
end;

begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;
  Initialize;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
  Application.Run;

  FreeAndNil(Sprite);
end.
