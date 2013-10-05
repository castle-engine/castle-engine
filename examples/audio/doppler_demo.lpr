{
  Copyright 2009-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo of Doppler effect using OpenAL.
  You move the sound source by dragging with mouse over the window,
  velocity is automatically calculated. Try to drag a horizontal line
  through the window to hear the Doppler effect, listener is positioned
  in the center of the window.

  Accepts command-line options from
  http://castle-engine.sourceforge.net/openal.php }
program doppler_demo;

uses SysUtils, CastleVectors, CastleWindow, CastleColors, CastleGLUtils,
  CastleALUtils, CastleSoundEngine, CastleStringUtils, CastleKeysMouse,
  CastleRectangles, CastleUIControls;

const
  ALDistanceScaling = 0.02;

var
  Window: TCastleWindowCustom;
  PreviousSoundPosition, SoundPosition, ListenerPosition: TVector3Single;
  { Playing sound. It may be @nil if we couldn't allocate it,
    which practically will happen only when OpenAL is not installed
    or --no-sound command-line option is used. }
  Sound: TSound;

procedure Draw(Window: TCastleWindowBase);

  { Trivial visualization of a point. }
  procedure DrawPoint(const V: TVector3Single; const Color: TCastleColor);
  var
    R: TRectangle;
  begin
    R := Rectangle(Round(V[0]), Round(V[1]), 0, 0);
    R := R.Grow(10);
    DrawRectangle(R, Color);
  end;

begin
  GLClear([cbColor], Black);
  DrawPoint(ListenerPosition, Yellow);
  DrawPoint(SoundPosition, White);
end;

procedure Timer(Window: TCastleWindowBase);
begin
  if Sound <> nil then
    Sound.Velocity := (SoundPosition - PreviousSoundPosition) * ALDistanceScaling;
  PreviousSoundPosition := SoundPosition;
end;

procedure MouseMove(Window: TCastleWindowBase; NewX, NewY: Integer);
begin
  if mbLeft in Window.MousePressed then
  begin
    SoundPosition := Vector3Single(NewX, Window.Height - NewY);
    if Sound <> nil then
      Sound.Position := SoundPosition * ALDistanceScaling;
    Window.PostRedisplay;
  end;
end;

var
  Buffer: TSoundBuffer;
begin
  Window := TCastleWindowCustom.Create(Application);

  SoundEngine.ParseParameters;
  SoundEngine.MinAllocatedSources := 1;
  SoundEngine.ALContextOpen;
  try
    Buffer := SoundEngine.LoadBuffer('tone.wav');

    //alDopplerFactor(3.0);

    SoundPosition := Vector3Single(200, 300, 0);
    PreviousSoundPosition := SoundPosition;
    Sound := SoundEngine.PlaySound(Buffer, true, true, 0, 1, 0, 1,
      SoundPosition * ALDistanceScaling);

    ListenerPosition := Vector3Single(300, 300, 0);
    SoundEngine.UpdateListener(ListenerPosition * ALDistanceScaling,
      Vector3Single(0, 1, 0), Vector3Single(0, 0, 1));

    Application.TimerMilisec := 1000;
    Window.OnTimer := @Timer;
    Window.OnDraw := @Draw;
    Window.OnDrawStyle := ds2D;
    Window.OnMouseMove := @MouseMove;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally
    SoundEngine.StopAllSources;
    SoundEngine.FreeBuffer(Buffer);
    SoundEngine.ALContextClose;
  end;
end.
