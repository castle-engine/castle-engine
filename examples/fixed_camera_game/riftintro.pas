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

{ Show the intro images. }
unit RiftIntro;

{$I castleconf.inc}

interface

uses RiftWindow;

procedure DoIntro;

implementation

uses SysUtils, DOM, FGL,
  CastleWindow, CastleFilesUtils, CastleVectors,
  CastleGLUtils, CastleWindowModes, CastleImages, CastleSoundEngine,
  CastleGLImages, CastleUIControls, CastleStringUtils, CastleXMLUtils,
  CastleKeysMouse, CastleColors, CastleApplicationProperties,
  RiftData, RiftSound, RiftVideoOptions;

{ $define DEBUG_INTRO_FAST}

type
  TIntroPart = class
    CorrodeDuration, IdleDuration: Single;
    Image: TGLImage;
    ImageCorroded: TGLImage;
    destructor Destroy; override;
  end;
  TIntroPartList = specialize TFPGObjectList<TIntroPart>;

destructor TIntroPart.Destroy;
begin
  FreeAndNil(Image);
  FreeAndNil(ImageCorroded);
  inherited;
end;

var
  IntroPart: Integer;
  IntroPartTime: Single;
  IntroParts: TIntroPartList;

  UserQuit: boolean;

procedure Resize(Container: TUIContainer);
begin
  OrthoProjection(0, Window.Width, 0, Window.Height);
end;

procedure NextIntroPart;
begin
  Inc(IntroPart);
  IntroPartTime := 0.0;

  if IntroPart >= IntroParts.Count then
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
  if IntroPart >= IntroParts.Count then Exit;

  RenderContext.Clear([cbColor], Black);

  if IntroPartTime >= IntroParts[IntroPart].CorrodeDuration then
  begin
    IntroParts[IntroPart].ImageCorroded.Draw(0, 0);
  end else
  begin
    Corrosion := IntroPartTime / IntroParts[IntroPart].CorrodeDuration;

    IntroParts[IntroPart].Image.Alpha := acBlending;
    IntroParts[IntroPart].Image.BlendingSourceFactor := bsConstantColor;
    IntroParts[IntroPart].Image.BlendingDestinationFactor := bdOne;
    IntroParts[IntroPart].Image.BlendingConstantColor :=
      Vector4Single(1 - Corrosion, 1 - Corrosion, 1 - Corrosion, 1);
    IntroParts[IntroPart].Image.Draw(0, 0);

    IntroParts[IntroPart].ImageCorroded.Alpha := acBlending;
    IntroParts[IntroPart].ImageCorroded.BlendingSourceFactor := bsConstantColor;
    IntroParts[IntroPart].ImageCorroded.BlendingDestinationFactor := bdOne;
    IntroParts[IntroPart].ImageCorroded.BlendingConstantColor :=
      Vector4Single(Corrosion, Corrosion, Corrosion, 1);
    IntroParts[IntroPart].ImageCorroded.Draw(0, 0);
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
    Element: TDOMElement;
    ImageName: string;
    I: TXMLElementIterator;
    Part: TIntroPart;
  begin
    IntroParts := TIntroPartList.Create(true);

    I := DataConfig.PathChildrenIterator('intro/parts', 'part');
    try
      while I.GetNext do
      begin
        Part := TIntroPart.Create;
        IntroParts.Add(Part);

        Element := I.Current;

        { calculate Part durations }
        Part.CorrodeDuration := Element.AttributeFloat('corrode_duration');
        Part.IdleDuration := Element.AttributeFloat('idle_duration');
        {$ifdef DEBUG_INTRO_FAST}
        Part.CorrodeDuration /= 10;
        Part.IdleDuration /= 10;
        {$endif DEBUG_INTRO_FAST}

        { calculate Part.Image and ImageCorroded }
        ImageName := Element.AttributeString('image');
        Part.Image := TGLImage.Create(
          ApplicationData('images/' + ImageName + '.png'), [TRGBImage],
            Window.Width, Window.Height);
        Part.ImageCorroded := TGLImage.Create(
          ApplicationData('images/' + ImageName + '_corroded.png'), [TRGBImage],
            Window.Width, Window.Height);
      end;
    finally FreeAndNil(I) end;

    if IntroParts.Count = 0 then
      raise Exception.Create('No intro parts defined in data/index.xml');
  end;

begin
  InitializeIntroParts;
end;

procedure ContextClose;
begin
  FreeAndNil(IntroParts);
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
