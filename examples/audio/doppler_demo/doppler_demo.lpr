{
  Copyright 2009-2018 Michalis Kamburelis.

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
  https://castle-engine.io/openal.php }
program doppler_demo;

uses SysUtils, CastleVectors, CastleWindow, CastleColors, CastleGLUtils,
  CastleSoundEngine, CastleStringUtils, CastleKeysMouse,
  CastleRectangles, CastleUIControls;

const
  ALDistanceScaling = 0.02;

var
  Window: TCastleWindowBase;
  PreviousSoundPosition, SoundPosition, ListenerPosition: TVector3;
  { Playing sound. It may be @nil if we couldn't allocate it,
    which practically will happen only when OpenAL is not installed
    or --no-sound command-line option is used. }
  Sound: TSound;

procedure Render(Container: TUIContainer);

  { Trivial visualization of a point. }
  procedure DrawPoint(const V: TVector3; const Color: TCastleColor);
  var
    R: TRectangle;
  begin
    R := Rectangle(Round(V[0]), Round(V[1]), 0, 0);
    R := R.Grow(10);
    DrawRectangle(R, Color);
  end;

begin
  DrawPoint(ListenerPosition, Yellow);
  DrawPoint(SoundPosition, White);
end;

procedure Timer(Container: TUIContainer);
begin
  if Sound <> nil then
    Sound.Velocity := (SoundPosition - PreviousSoundPosition) * ALDistanceScaling;
  PreviousSoundPosition := SoundPosition;
end;

procedure Motion(Container: TUIContainer; const Event: TInputMotion);
begin
  if buttonLeft in Event.Pressed then
  begin
    SoundPosition := Vector3(Event.Position[0], Event.Position[1], 0);
    if Sound <> nil then
      Sound.Position := SoundPosition * ALDistanceScaling;
    Window.Invalidate;
  end;
end;

var
  Buffer: TSoundBuffer;
  Parameters: TSoundParameters;
begin
  Window := TCastleWindowBase.Create(Application);

  SoundEngine.ParseParameters;
  Buffer := SoundEngine.LoadBuffer('castle-data:/tone.wav');

  //alDopplerFactor(3.0);

  SoundPosition := Vector3(200, 300, 0);
  PreviousSoundPosition := SoundPosition;
  Parameters := TSoundParameters.Create;
  try
    Parameters.Buffer := Buffer;
    Parameters.Spatial := true;
    Parameters.Looping := true;
    Parameters.Position := SoundPosition * ALDistanceScaling;
    Sound := SoundEngine.PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;

  ListenerPosition := Vector3(300, 300, 0);
  SoundEngine.UpdateListener(ListenerPosition * ALDistanceScaling,
    Vector3(0, 1, 0), Vector3(0, 0, 1));

  Application.TimerMilisec := 1000;
  Window.OnTimer := @Timer;
  Window.OnRender := @Render;
  Window.OnMotion := @Motion;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.OpenAndRun;
end.
