{
  Copyright 2007-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftIntro;

{$I castleconf.inc}

interface

uses RiftWindow;

procedure DoIntro;

implementation

uses SysUtils, CastleGL, CastleWindow, CastleFilesUtils,
  CastleGLUtils, RiftData, CastleWindowModes, DOM, CastleImages, CastleSoundEngine,
  CastleGLImages, CastleUIControls, CastleStringUtils, RiftSound, RiftVideoOptions,
  CastleKeysMouse, CastleColors, CastleApplicationProperties;

{ $define DEBUG_INTRO_FAST}

type
  TIntroPart = record
    CorrodeDuration, IdleDuration: Single;
    Image: TGLImage;
    ImageCorroded: TGLImage;
  end;

var
  IntroPart: Integer;
  IntroPartTime: Single;
  IntroParts: array of TIntroPart;

  UserQuit: boolean;

procedure Resize(Container: TUIContainer);
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

procedure Update(Container: TUIContainer);
begin
  IntroPartTime := IntroPartTime + Window.Fps.UpdateSecondsPassed;
  if IntroPartTime >
      IntroParts[IntroPart].CorrodeDuration +
      IntroParts[IntroPart].IdleDuration then
    NextIntroPart;
end;

procedure Draw(Container: TUIContainer);
var
  Corrosion: Single;
begin
  { Are we on the way to exit intro }
  if IntroPart > High(IntroParts) then Exit;

  RenderContext.Clear([cbColor], Black);

  if (IntroPartTime >= IntroParts[IntroPart].CorrodeDuration) or
     not GLFeatures.BlendConstant then
  begin
    IntroParts[IntroPart].ImageCorroded.Draw(0, 0);
  end else
  begin
    Corrosion := IntroPartTime / IntroParts[IntroPart].CorrodeDuration;

    glBlendFunc(GL_CONSTANT_COLOR, GL_ONE);
    glBlendColor(1 - Corrosion, 1 - Corrosion, 1 - Corrosion, 1);
    glEnable(GL_BLEND);

    IntroParts[IntroPart].Image.Draw(0, 0);

    glBlendColor(Corrosion, Corrosion, Corrosion, 1);
    IntroParts[IntroPart].ImageCorroded.Alpha := acNone;
    IntroParts[IntroPart].ImageCorroded.Draw(0, 0);

    glDisable(GL_BLEND);
  end;
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(CharEscape) then
    UserQuit := true else
  { Don't let player skip to next part before watching the corrode animation.
    Later: this was bothersome, just let player advance to next part always. }
  { if IntroPartTime > IntroParts[IntroPart].CorrodeDuration then }
  if Event.EventType in [itKey, itMouseButton] then
    NextIntroPart;
end;

procedure CloseQuery(Container: TUIContainer);
begin
  UserQuit := true;
end;

procedure DoIntro;
var
  SavedMode: TGLMode;
begin
  IntroPart := 0;
  IntroPartTime := 0.0;
  SavedMode := TGLMode.CreateReset(Window, @Draw, @Resize, @CloseQuery);
  try
    SoundEngine.MusicPlayer.Sound := stIntroMusic;

    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;
    Window.OnPress := @Press;
    Window.OnUpdate := @Update;

    UserQuit := false;

    repeat
      Application.ProcessMessage(true, true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

procedure ContextOpen;

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
      IntroParts[I].Image := TGLImage.Create(
        ApplicationData('images/' + ImageName + '.png'), [TRGBImage],
          Window.Width, Window.Height);
      IntroParts[I].ImageCorroded := TGLImage.Create(
        ApplicationData('images/' + ImageName + '_corroded.png'), [TRGBImage],
          Window.Width, Window.Height);
    end;
  end;

begin
  InitializeIntroParts;
end;

procedure ContextClose;

  procedure FinalizeIntroParts;
  var
    I: Integer;
  begin
    for I := 0 to High(IntroParts) do
    begin
      FreeAndNil(IntroParts[I].Image);
      FreeAndNil(IntroParts[I].ImageCorroded);
    end;
  end;

begin
  FinalizeIntroParts;
end;

initialization
  ApplicationProperties.OnGLContextOpen.Add(@ContextOpen);
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
finalization
  { ContextClose will be done anyway at GL context close,
    but it may be after finalization of this unit, when IntroParts is
    automatically set to zero length. So do ContextClose now,
    to free TGLImage instances inside. }
  ContextClose;
end.
