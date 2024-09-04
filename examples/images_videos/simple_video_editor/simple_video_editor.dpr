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
  end;

var
  ViewMain: TViewMain;

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
    { Check if menu items are <> nil before using,
      because when this is called from Start then menu is not created yet. }
    if MenuEdit <> nil then
      MenuEdit.Enabled := Video.Loaded;
    if MenuRevert <> nil then
      MenuRevert.Enabled := Video.Loaded;
    if MenuSave <> nil then
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

{ menu ----------------------------------------------------------------------- }

procedure MenuClick(Container: TCastleContainer; MenuItem: TMenuItem);
var
  S: string;
  I: Integer;
  FadeFrames: Cardinal;
begin
  case MenuItem.IntData of
    10:
      begin
        S := ViewMain.VideoUrl;
        if Window.FileDialog('Open file', S, true) then
          ViewMain.LoadVideo(S);
      end;
    13:
      begin
        S := ViewMain.VideoUrl;
        if Window.FileDialog('Save to file', S, false) then
          ViewMain.SaveVideo(S);
      end;
    15:
      begin
        if ViewMain.Video.Loaded then
          ViewMain.LoadVideo(ViewMain.VideoUrl);
      end;
    20: Window.Close;
    110: ViewMain.TimePlaying := not ViewMain.TimePlaying;
    120: ViewMain.Time := 0;
    130:
      begin
        ViewMain.Video.TimeLoop := not ViewMain.Video.TimeLoop;
        ViewMain.GLVideo.TimeLoop := ViewMain.Video.TimeLoop;
      end;
    140:
      begin
        ViewMain.Video.TimeBackwards := not ViewMain.Video.TimeBackwards;
        ViewMain.GLVideo.TimeBackwards := ViewMain.Video.TimeBackwards;
      end;

    { Editing operations.
      Should be implemented nicer, as a single procedure with
      TImageFunction callback... too lazy to do this now. }

    410: begin
           Assert(ViewMain.Video.Loaded);
           for I := 0 to ViewMain.Video.Count - 1 do
             ViewMain.Video.Items[I].Grayscale;
           ViewMain.RemakeGLVideo;
         end;
    420..422:
         begin
           Assert(ViewMain.Video.Loaded);
           for I := 0 to ViewMain.Video.Count - 1 do
             ViewMain.Video.Items[I].ConvertToChannelRGB(MenuItem.IntData - 420);
           ViewMain.RemakeGLVideo;
         end;
    430..432:
         begin
           Assert(ViewMain.Video.Loaded);
           for I := 0 to ViewMain.Video.Count - 1 do
             ViewMain.Video.Items[I].StripToChannelRGB(MenuItem.IntData - 430);
           ViewMain.RemakeGLVideo;
         end;
    440: begin
           Assert(ViewMain.Video.Loaded);
           for I := 0 to ViewMain.Video.Count - 1 do
             ViewMain.Video.Items[I].FlipHorizontal;
           ViewMain.RemakeGLVideo;
         end;

    445: begin
           FadeFrames := Min(10, ViewMain.Video.Count div 2);
           if MessageInputQueryCardinal(Window,
             'How many frames to use for fading?', FadeFrames) then
           begin
             ViewMain.Video.FadeWithSelf(FadeFrames);
             ViewMain.RemakeGLVideo;
           end;
         end;

    450: begin
           ViewMain.Video.MixWithSelfBackwards;
           ViewMain.RemakeGLVideo;
           { MixWithSelfBackwards changes TimeBackwards, we have to reflect
             this in our menu item. }
           ViewMain.MenuTimeBackwards.Checked := ViewMain.Video.TimeBackwards;
         end;
  end;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('_Open ...',   10, CtrlO));
    M.Append(TMenuSeparator.Create);
    ViewMain.MenuSave := TMenuItem.Create('_Save As ...',  13);
    ViewMain.MenuSave.Enabled := ViewMain.Video.Loaded;
    M.Append(ViewMain.MenuSave);
    ViewMain.MenuRevert := TMenuItem.Create('_Revert',     15);
    ViewMain.MenuRevert.Enabled := ViewMain.Video.Loaded;
    M.Append(ViewMain.MenuRevert);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit',       20, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_Playback');
    M.Append(TMenuItemChecked.Create('_Playing / Paused', 110, CtrlP,
      ViewMain.TimePlaying, true));
    M.Append(TMenuItem.Create('_Rewind to Beginning',     120, keyHome));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Loop',             130,
      ViewMain.Video.TimeLoop, true));
    ViewMain.MenuTimeBackwards := TMenuItemChecked.Create('_Play Backwards after Playing Forward', 140,
      ViewMain.Video.TimeBackwards, true);
    M.Append(ViewMain.MenuTimeBackwards);
    Result.Append(M);
  M := TMenu.Create('_Edit');
    ViewMain.MenuEdit := M;
    ViewMain.MenuEdit.Enabled := ViewMain.Video.Loaded;
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

begin
  LoadAnimatedGifs := true;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  ViewMain := TViewMain.Create(Application);
  Window.Container.View := ViewMain;

  Window.SetDemoOptions(keyF11, #0, true);
  Window.AutoRedisplay := true;
  Window.MainMenu := CreateMainMenu;
  Window.OnMenuClick := @MenuClick;

  Window.Open;

  Application.Run;
end.
