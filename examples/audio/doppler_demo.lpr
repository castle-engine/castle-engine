{
  Copyright 2009-2012 Michalis Kamburelis.

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

uses SysUtils, CastleVectors, CastleWindow, GL, GLU, CastleGLUtils,
  ALUtils, CastleSoundEngine, CastleStringUtils, CastleKeysMouse;

const
  ALDistanceScaling = 0.02;

var
  Window: TCastleWindowDemo;
  PreviousSoundPosition, SoundPosition, ListenerPosition: TVector3Single;
  Sound: TSound;

procedure Draw(Window: TCastleWindowBase);
begin
  glClear(GL_COLOR_BUFFER_BIT);

  glPointSize(20.0);

  glColor3f(1, 1, 0);
  glBegin(GL_POINTS);
    glVertexv(ListenerPosition);
  glEnd;

  glColor3f(1, 1, 1);
  glBegin(GL_POINTS);
    glVertexv(SoundPosition);
  glEnd;
end;

procedure Timer(Window: TCastleWindowBase);
begin
  Sound.Velocity := (SoundPosition - PreviousSoundPosition) * ALDistanceScaling;
  PreviousSoundPosition := SoundPosition;
end;

procedure MouseMove(Window: TCastleWindowBase; NewX, NewY: Integer);
begin
  if mbLeft in Window.MousePressed then
  begin
    SoundPosition := Vector3Single(NewX, Window.Height - NewY);
    Sound.Position := SoundPosition * ALDistanceScaling;
    Window.PostRedisplay;
  end;
end;

var
   Buffer: TSoundBuffer;
begin
  Window := TCastleWindowDemo.Create(Application);

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
    Window.OnResize := @Resize2D;
    Window.OnMouseMove := @MouseMove;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally
    SoundEngine.StopAllSources;
    SoundEngine.FreeBuffer(Buffer);
    SoundEngine.ALContextClose;
  end;
end.
