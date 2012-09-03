{
  Copyright 2007-2012 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftIntro;

interface

uses RiftWindow;

procedure DoIntro;

implementation

uses SysUtils, GL, CastleWindow, CastleFilesUtils,
  CastleGLUtils, RiftData, WindowModes, DOM, Images, CastleSoundEngine,
  GLImages, UIControls, CastleStringUtils, RiftSound, RiftVideoOptions;

{ $define DEBUG_INTRO_FAST}

type
  TIntroPart = record
    CorrodeDuration, IdleDuration: Single;
    Image: TGLuint;
    ImageCorroded: TGLuint;
  end;

var
  IntroPart: Integer;
  IntroPartTime: Single;
  IntroParts: array of TIntroPart;

  UserQuit: boolean;

procedure Resize(Window: TCastleWindowBase);
begin
  OrthoProjection(0, Window.Width, 0, Window.Height);
end;

procedure NextIntroPart;
begin
  Inc(IntroPart);
  IntroPartTime := 0.0;

  if IntroPart > High(IntroParts) then
   UserQuit := true;
end;

procedure Idle(Window: TCastleWindowBase);
begin
  IntroPartTime := IntroPartTime + Window.Fps.IdleSpeed;
  if IntroPartTime >
      IntroParts[IntroPart].CorrodeDuration +
      IntroParts[IntroPart].IdleDuration then
    NextIntroPart;
end;

procedure Draw(Window: TCastleWindowBase);
var
  Corrosion: Single;
begin
  { Are we on the way to exit intro }
  if IntroPart > High(IntroParts) then Exit;

  glLoadIdentity();
  glRasterPos2i(0, 0);

  if IntroPartTime >= IntroParts[IntroPart].CorrodeDuration then
  begin
    glCallList(IntroParts[IntroPart].ImageCorroded);
  end else
  begin
    Corrosion := IntroPartTime / IntroParts[IntroPart].CorrodeDuration;

    { I use glPixelTransferf to affect whole drawn image by
      Corrosion factor. In newer OpenGLs I could do it with appropriate
      glBlendFunc and glBlendColor, but I prefer to stick to oldest
      OpenGL possible if I can... so I do it by glPixelTransferf. }

    glPixelTransferf(GL_RED_SCALE, 1 - Corrosion);
    glPixelTransferf(GL_GREEN_SCALE, 1 - Corrosion);
    glPixelTransferf(GL_BLUE_SCALE, 1 - Corrosion);

    glCallList(IntroParts[IntroPart].Image);

    glPixelTransferf(GL_RED_SCALE, Corrosion);
    glPixelTransferf(GL_GREEN_SCALE, Corrosion);
    glPixelTransferf(GL_BLUE_SCALE, Corrosion);

    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE);
    glCallList(IntroParts[IntroPart].ImageCorroded);
    glDisable(GL_BLEND);

    glPixelTransferf(GL_RED_SCALE, 1);
    glPixelTransferf(GL_GREEN_SCALE, 1);
    glPixelTransferf(GL_BLUE_SCALE, 1);
  end;
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
begin
  if C = CharEscape then
    UserQuit := true else
  { Don't let player skip to next part before watching the corrode animation.
    Later: this was bothersome, just let player advance to next part always. }
  { if IntroPartTime > IntroParts[IntroPart].CorrodeDuration then }
    NextIntroPart;
end;

procedure MouseDown(Window: TCastleWindowBase; Button: TMouseButton);
begin
  { Don't let player skip to next part before watching the corrode animation }
  if IntroPartTime > IntroParts[IntroPart].CorrodeDuration then
    NextIntroPart;
end;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  UserQuit := true;
end;

procedure DoIntro;
var
  SavedMode: TGLMode;
begin
  IntroPart := 0;
  IntroPartTime := 0.0;
  SavedMode := TGLMode.CreateReset(Window, 0, false, @Draw, @Resize, @CloseQuery);
  try
    SoundEngine.MusicPlayer.Sound := stIntroMusic;

    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;
    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;
    Window.OnIdle := @Idle;
    { actually we draw in 2D, but it's the current projection anyway }
    Window.OnDrawStyle := ds3D;

    Window.EventResize;

    UserQuit := false;

    repeat
      Application.ProcessMessage(true, true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

procedure WindowOpen(const Container: IUIContainer);

  procedure InitializeIntroParts;
  var
    I: Integer;
    Element: TDOMElement;
    ElementPath, ImageName: string;
  begin
    SetLength(IntroParts, DataConfig.GetValue('intro/parts/count', 0));
    if Length(IntroParts) = 0 then
      raise Exception.Create('No intro parts defined in data/index.xml');
    for I := 0 to High(IntroParts) do
    begin
      ElementPath := 'intro/parts/part' + IntToStr(I);

      Element := DataConfig.PathElement(ElementPath);
      if Element = nil then
        raise Exception.CreateFmt('Unable to find XML element by path "%s"',
          [ElementPath]);

      { calculate IntroParts[I].Durations }
      IntroParts[I].CorrodeDuration := DataConfig.GetFloat(ElementPath + '/corrode_duration', 0.0);
      IntroParts[I].IdleDuration := DataConfig.GetFloat(ElementPath + '/idle_duration', 0.0);
      {$ifdef DEBUG_INTRO_FAST}
      IntroParts[I].CorrodeDuration /= 10;
      IntroParts[I].IdleDuration /= 10;
      {$endif DEBUG_INTRO_FAST}

      { calculate IntroParts[I].Image and ImageCorroded }
      ImageName := DataConfig.GetValue(ElementPath + '/image',
        'require_image_name_missing');
      IntroParts[I].Image := LoadImageToDisplayList(
        ProgramDataPath + 'data' + PathDelim + 'images' + PathDelim +
        ImageName + '.png', [TRGBImage], [], Window.Width, Window.Height);
      IntroParts[I].ImageCorroded := LoadImageToDisplayList(
        ProgramDataPath + 'data' + PathDelim + 'images' + PathDelim +
        ImageName + '_corroded.png', [TRGBImage], [], Window.Width, Window.Height);
    end;
  end;

begin
  InitializeIntroParts;
end;

procedure WindowClose(const Container: IUIContainer);

  procedure FinalizeIntroParts;
  var
    I: Integer;
  begin
    for I := 0 to High(IntroParts) do
    begin
      glFreeDisplayList(IntroParts[I].Image);
    end;
  end;

begin
  FinalizeIntroParts;
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
end.
