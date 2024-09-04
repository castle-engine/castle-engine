{
  Copyright 2008-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple example of loading a video file.
  See README.md for details. }
program simple_video_editor;

{ Console, not GUI, since loading files through ffmpeg will output some
  info on stdout anyway. }
{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, Math,
  CastleUtils, CastleWindow, CastleGLImages, CastleControls,
  CastleVideos, CastleStringUtils, CastleMessages, CastleColors,
  CastleParameters, CastleGLUtils, CastleVectors, Classes,
  CastleTimeUtils, CastleKeysMouse, CastleUriUtils,
  CastleUIControls, CastleRectangles;

var
  Window: TCastleWindow;

{ TViewMain ------------------------------------------------------------------ }

type
  { View to handle events. }
  TViewMain = class(TCastleView)
  private
    Video: TVideo;
    GLVideo: TGLVideo2D;
    VideoUrl: String;

    Time: TFloatTime;
    TimePlaying: boolean;

    MenuEdit: TMenu;
    MenuTimeBackwards: TMenuItemChecked;
    MenuRevert, MenuSave: TMenuItem;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;

    procedure RemakeGLVideo;
    procedure SaveVideo(const NewVideoUrl: String);
    procedure LoadVideo(const NewVideoUrl: String);

    procedure MenuClick(const MenuItem: TMenuItem);
    function CreateMainMenu: TMenu;
  end;

procedure TViewMain.RemakeGLVideo;
begin
  FreeAndNil(GLVideo);
  GLVideo := TGLVideo2D.Create(Video);
end;

procedure TViewMain.Render;
var
  RR: TFloatRectangle;
const
  TimeBarHeight = 10;
  TimeBarMargin = 2;

  procedure DrawStatus;
  var
    S: string;
    Strs: TStringList;
  begin
    Strs := TStringList.Create;
    try
      S := Format('Video time: %f', [Time]);
      if not TimePlaying then
        S := S + ' (paused)';
      Strs.Append(S);

      if Video.Loaded then
      begin
        Strs.Append(Format('Video URL: %s', [VideoUrl]));
        Strs.Append(Format('Size: %d x %d', [Video.Width, Video.Height]));
        Strs.Append(Format('Time: %f seconds (%d frames, with %f frames per second)',
          [Video.TimeDuration, Video.Count, Video.FramesPerSecond]));
      end else
        Strs.Append('Video not loaded');

      FallbackFont.PrintStrings(15,
        RR.Height - FallbackFont.Height * Strs.Count - TimeBarHeight, Yellow,
        Strs, false, 2);
    finally FreeAndNil(Strs) end;
  end;

begin
  inherited;

  RR := RenderRect;

  if Video.Loaded then
  begin
    GLVideo.DrawableImageFromTime(Time).Draw(0, 0);

    { draw time of the video bar }
    DrawRectangle(FloatRectangle(0, RR.Height - TimeBarHeight,
      RR.Width, TimeBarHeight), Black);
    DrawRectangle(FloatRectangle(TimeBarMargin, RR.Height - TimeBarHeight + TimeBarMargin,
      MapRange(Video.IndexFromTime(Time), 0, Video.Count - 1,
        0, RR.Width - 2 * TimeBarMargin), TimeBarHeight - 2 * TimeBarMargin),
      Gray);
  end;

  DrawStatus;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if TimePlaying then
    Time := Time + Container.Fps.SecondsPassed;
end;

procedure TViewMain.LoadVideo(const NewVideoUrl: String);
begin
  try
    Video.LoadFromFile(NewVideoUrl);
    VideoUrl := NewVideoUrl;
    Time := 0;
    Assert(MenuEdit <> nil); // CreateMainMenu must be done earlier
    MenuEdit.Enabled := Video.Loaded;
    MenuRevert.Enabled := Video.Loaded;
    MenuSave.Enabled := Video.Loaded;
    RemakeGLVideo;
  except
    on E: Exception do
      MessageOk(Window, 'Loading of "' + VideoUrl + '" failed:' + NL + E.Message);
  end;
end;

procedure TViewMain.SaveVideo(const NewVideoUrl: String);
begin
  try
    Video.SaveToFile(NewVideoUrl);
    VideoUrl := NewVideoUrl;
  except
    on E: Exception do
      MessageOk(Window, 'Saving of "' + NewVideoUrl + '" failed:' + NL + E.Message);
  end;
end;

procedure TViewMain.Start;
begin
  inherited;
  TimePlaying := true; // default
  Video := TVideo.Create;

  Window.MainMenu := CreateMainMenu;
  Window.OnMenuItemClick := {$ifdef FPC}@{$endif} MenuClick;

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    LoadVideo(Parameters[1])
  else
    LoadVideo('castle-data:/flame_seamless/@counter(4).png');
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(GLVideo);
  FreeAndNil(Video);
  inherited;
end;

procedure TViewMain.MenuClick(const MenuItem: TMenuItem);
var
  S: string;
  I: Integer;
  FadeFrames: Cardinal;
begin
  case MenuItem.IntData of
    10:
      begin
        S := VideoUrl;
        if Window.FileDialog('Open file', S, true) then
          LoadVideo(S);
      end;
    13:
      begin
        S := VideoUrl;
        if Window.FileDialog('Save to file', S, false) then
          SaveVideo(S);
      end;
    15:
      begin
        if Video.Loaded then
          LoadVideo(VideoUrl);
      end;
    20: Window.Close;
    110: TimePlaying := not TimePlaying;
    120: Time := 0;
    130:
      begin
        Video.TimeLoop := not Video.TimeLoop;
        GLVideo.TimeLoop := Video.TimeLoop;
      end;
    140:
      begin
        Video.TimeBackwards := not Video.TimeBackwards;
        GLVideo.TimeBackwards := Video.TimeBackwards;
      end;

    { Editing operations.
      Should be implemented nicer, as a single procedure with
      TImageFunction callback... too lazy to do this now. }

    410: begin
           Assert(Video.Loaded);
           for I := 0 to Video.Count - 1 do
             Video.Items[I].Grayscale;
           RemakeGLVideo;
         end;
    420..422:
         begin
           Assert(Video.Loaded);
           for I := 0 to Video.Count - 1 do
             Video.Items[I].ConvertToChannelRGB(MenuItem.IntData - 420);
           RemakeGLVideo;
         end;
    430..432:
         begin
           Assert(Video.Loaded);
           for I := 0 to Video.Count - 1 do
             Video.Items[I].StripToChannelRGB(MenuItem.IntData - 430);
           RemakeGLVideo;
         end;
    440: begin
           Assert(Video.Loaded);
           for I := 0 to Video.Count - 1 do
             Video.Items[I].FlipHorizontal;
           RemakeGLVideo;
         end;

    445: begin
           FadeFrames := Min(10, Video.Count div 2);
           if MessageInputQueryCardinal(Window,
             'How many frames to use for fading?', FadeFrames) then
           begin
             Video.FadeWithSelf(FadeFrames);
             RemakeGLVideo;
           end;
         end;

    450: begin
           Video.MixWithSelfBackwards;
           RemakeGLVideo;
           { MixWithSelfBackwards changes TimeBackwards, we have to reflect
             this in our menu item. }
           MenuTimeBackwards.Checked := Video.TimeBackwards;
         end;
  end;
end;

function TViewMain.CreateMainMenu: TMenu;
var
  M: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('_Open ...',   10, CtrlO));
    M.Append(TMenuSeparator.Create);
    MenuSave := TMenuItem.Create('_Save As ...',  13);
    MenuSave.Enabled := Video.Loaded;
    M.Append(MenuSave);
    MenuRevert := TMenuItem.Create('_Revert',     15);
    MenuRevert.Enabled := Video.Loaded;
    M.Append(MenuRevert);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit',       20, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_Playback');
    M.Append(TMenuItemChecked.Create('_Playing / Paused', 110, CtrlP,
      TimePlaying, true));
    M.Append(TMenuItem.Create('_Rewind to Beginning',     120, keyHome));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Loop',             130,
      Video.TimeLoop, true));
    MenuTimeBackwards := TMenuItemChecked.Create('_Play Backwards after Playing Forward', 140,
      Video.TimeBackwards, true);
    M.Append(MenuTimeBackwards);
    Result.Append(M);
  M := TMenu.Create('_Edit');
    MenuEdit := M;
    MenuEdit.Enabled := Video.Loaded;
    M.Append(TMenuItem.Create('_Fade with Self (Makes Video Loop Seamless) ...', 445));
    M.Append(TMenuItem.Create('_Mix with Self Backwards (Makes Video Loop Seamless)', 450));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Grayscale',                          410));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Convert to _Red Channel',             420));
    M.Append(TMenuItem.Create('Convert to Gr_een Channel',           421));
    M.Append(TMenuItem.Create('Convert to _Blue Channel',            422));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Strip to Red Channel',                430));
    M.Append(TMenuItem.Create('Strip to Green Channel',              431));
    M.Append(TMenuItem.Create('Strip to Blue Channel',               432));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Mirror Horizontally',                440));
    Result.Append(M);
end;

{ initialization ------------------------------------------------------------- }

var
  ViewMain: TViewMain;
begin
  LoadAnimatedGifs := true;

  Window := TCastleWindow.Create(Application);
  Window.SetDemoOptions(keyF11, #0, true);
  Window.AutoRedisplay := true;
  Application.MainWindow := Window;

  ViewMain := TViewMain.Create(Application);
  Window.Container.View := ViewMain;

  Window.Open;
  Application.Run;
end.
