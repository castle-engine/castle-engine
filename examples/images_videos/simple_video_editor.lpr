{
  Copyright 2008-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple example of loading a video file and playing it in OpenGL
  window, with some simple editing features.
  Also handles normal image files, since we can load all image
  formats to TVideo by just treating them as movies with 1 frame. }
program simple_video_editor;

{ Console, not GUI, since loading files through ffmpeg will output some
  info on stdout anyway. }
{$apptype CONSOLE}

uses CastleUtils, SysUtils, CastleWindow, CastleGLImages, CastleControls,
  CastleVideos, CastleStringUtils, CastleMessages, CastleColors,
  CastleParameters, CastleGLUtils, CastleVectors, Classes, CastleProgress,
  CastleWindowProgress, CastleTimeUtils, CastleKeysMouse, CastleURIUtils,
  CastleUIControls, CastleRectangles;

var
  Window: TCastleWindowCustom;

  Video: TVideo;
  GLVideo: TGLVideo2D;
  VideoURL: string;

  Time: TFloatTime;
  TimePlaying: boolean = true;

  MenuEdit: TMenu;
  MenuTimeBackwards: TMenuItemChecked;
  MenuRevert, MenuSave: TMenuItem;

procedure RemakeGLVideo;
begin
  FreeAndNil(GLVideo);
  GLVideo := TGLVideo2D.Create(Video);
end;

procedure Render(Container: TUIContainer);
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
        S += ' (paused)';
      Strs.Append(S);

      if Video.Loaded then
      begin
        Strs.Append(Format('Video URL: %s', [VideoURL]));
        Strs.Append(Format('Size: %d x %d', [Video.Width, Video.Height]));
        Strs.Append(Format('Time: %f seconds (%d frames, with %f frames per second)',
          [Video.TimeDuration, Video.Count, Video.FramesPerSecond]));
      end else
        Strs.Append('Video not loaded');

      UIFont.PrintStrings(15,
        Window.Height - UIFont.RowHeight * Strs.Count - TimeBarHeight, Yellow,
        Strs, false, 2);
    finally FreeAndNil(Strs) end;
  end;

begin
  GLClear([cbColor], Black);

  if Video.Loaded then
  begin
    GLVideo.GLImageFromTime(Time).Draw(0, 0);

    { draw time of the video bar }
    DrawRectangle(Rectangle(0, Window.Height - TimeBarHeight,
      Window.Width, TimeBarHeight), Black);
    DrawRectangle(Rectangle(TimeBarMargin, Window.Height - TimeBarHeight + TimeBarMargin,
      Round(MapRange(Video.IndexFromTime(Time), 0, Video.Count - 1,
        0, Window.Width - 2 * TimeBarMargin)), TimeBarHeight - 2 * TimeBarMargin),
      Gray);
  end;

  DrawStatus;
end;

procedure Update(Container: TUIContainer);
begin
  if TimePlaying then
    Time += Window.Fps.UpdateSecondsPassed;
end;

procedure LoadVideo(const NewVideoURL: string);
begin
  try
    Video.LoadFromFile(NewVideoURL);
    VideoURL := NewVideoURL;
    Time := 0;
    MenuEdit.Enabled := Video.Loaded;
    MenuRevert.Enabled := Video.Loaded;
    MenuSave.Enabled := Video.Loaded;
    RemakeGLVideo;
  except
    on E: Exception do
      MessageOk(Window, 'Loading of "' + VideoURL + '" failed:' + NL + E.Message);
  end;
end;

procedure SaveVideo(const NewVideoURL: string);
begin
  try
    Video.SaveToFile(NewVideoURL);
    VideoURL := NewVideoURL;
  except
    on E: Exception do
      MessageOk(Window, 'Saving of "' + NewVideoURL + '" failed:' + NL + E.Message);
  end;
end;

procedure Open(Container: TUIContainer);
begin
  if Parameters.High = 1 then
    LoadVideo(Parameters[1]);
end;

procedure Close(Container: TUIContainer);
begin
  FreeAndNil(GLVideo);
end;

procedure MenuClick(Container: TUIContainer; MenuItem: TMenuItem);
var
  S: string;
  I: Integer;
  FadeFrames: Cardinal;
begin
  case MenuItem.IntData of
    10:
      begin
        S := VideoURL;
        if Window.FileDialog('Open file', S, true) then
          LoadVideo(S);
      end;
    13:
      begin
        S := VideoURL;
        if Window.FileDialog('Save to file', S, false) then
          SaveVideo(S);
      end;
    15:
      begin
        if Video.Loaded then
          LoadVideo(VideoURL);
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
           Progress.Init(Video.Count, 'Grayscale');
           try
             for I := 0 to Video.Count - 1 do
             begin
               Video.Items[I].Grayscale;
               Progress.Step;
             end;
             RemakeGLVideo;
           finally Progress.Fini end;
         end;
    420..422:
         begin
           Assert(Video.Loaded);
           Progress.Init(Video.Count, 'Convert to single channel');
           try
             for I := 0 to Video.Count - 1 do
             begin
               Video.Items[I].ConvertToChannelRGB(MenuItem.IntData - 420);
               Progress.Step;
             end;
             RemakeGLVideo;
           finally Progress.Fini end;
         end;
    430..432:
         begin
           Assert(Video.Loaded);
           Progress.Init(Video.Count, 'Strip to single channel');
           try
             for I := 0 to Video.Count - 1 do
             begin
               Video.Items[I].StripToChannelRGB(MenuItem.IntData - 430);
               Progress.Step;
             end;
             RemakeGLVideo;
           finally Progress.Fini end;
         end;
    440: begin
           Assert(Video.Loaded);
           Progress.Init(Video.Count, 'Flip horizontal');
           try
             for I := 0 to Video.Count - 1 do
             begin
               Video.Items[I].FlipHorizontal;
               Progress.Step;
             end;
             RemakeGLVideo;
           finally Progress.Fini end;
         end;

    445: begin
           FadeFrames := Min(10, Video.Count div 2);
           if MessageInputQueryCardinal(Window,
             'How many frames to use for fading?', FadeFrames) then
           begin
             Video.FadeWithSelf(FadeFrames, 'Fade with self');
             RemakeGLVideo;
           end;
         end;

    450: begin
           Video.MixWithSelfBackwards('Mix with self backwards');
           RemakeGLVideo;
           { MixWithSelfBackwards changes TimeBackwards, we have to reflect
             this in our menu item. }
           MenuTimeBackwards.Checked := Video.TimeBackwards;
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
    MenuSave := TMenuItem.Create('_Save As ...',  13);
    MenuSave.Enabled := false;
    M.Append(MenuSave);
    MenuRevert := TMenuItem.Create('_Revert',     15);
    MenuRevert.Enabled := false;
    M.Append(MenuRevert);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit',       20, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_Playback');
    M.Append(TMenuItemChecked.Create('_Playing / Paused', 110, CtrlP,
      TimePlaying, true));
    M.Append(TMenuItem.Create('_Rewind to Beginning',     120, K_Home));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Loop',             130,
      Video.TimeLoop, true));
    MenuTimeBackwards := TMenuItemChecked.Create('_Play Backwards after Playing Forward', 140,
      Video.TimeBackwards, true);
    M.Append(MenuTimeBackwards);
    Result.Append(M);
  M := TMenu.Create('_Edit');
    MenuEdit := M;
    MenuEdit.Enabled := false;
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

begin
  Window := TCastleWindowCustom.Create(Application);

  try
    Application.MainWindow := Window;
    Progress.UserInterface := WindowProgressInterface;

    Video := TVideo.Create;

    { We will actually handle 1st param in Init. }
    Parameters.CheckHighAtMost(1);

    Window.SetDemoOptions(K_F11, #0, true);
    Window.AutoRedisplay := true;
    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;
    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.OnRender := @Render;
    Window.RenderStyle := rs2D;
    Window.OnUpdate := @Update;
    Window.OnResize := @Resize2D;

    Window.OpenAndRun;
  finally
    FreeAndNil(Video);
  end;
end.
