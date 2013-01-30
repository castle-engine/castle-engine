{
  Copyright 2002-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Progress bar displayed in a TCastleWindowBase.

  Simply set @code(WindowProgressInterface.Window) to your TCastleWindowBase
  instance, and assign

@longCode(#  Progress.UserInterface := WindowProgressInterface;#)

  Between Progress.Init and Fini you shouldn't do anything with
  window set as @code(WindowProgressInterface.Window).
  It's callbacks will be temporarily swapped and it will be used
  to render progress bar.

  As usual, remember to always call Progress.Fini if you called
  Progress.Init. Progress.Fini restores original callbacks and OpenGL
  state of your window. Usually it's best and safest to use try..finally
  block like

@longCode(#  Progress.Init; try.....finally Progress.Fini; end; #) }


unit CastleWindowProgress;

{$I castleconf.inc}

interface

uses GL, CastleWindow, CastleProgress, CastleWindowModes, CastleGLUtils, CastleImages,
  CastleGLImages;

type
  TWindowProgressInterface = class(TProgressUserInterface)
  private
    { Background image. }
    GLImage: TGLImage;
    BarYPosition: Single;
    FWindow: TCastleWindowBase;
    SavedMode: TGLMode;
    FOpacity: Single;
  public
    { Opacity (1 - transparency) with which control is drawn.
      When this is < 1, we draw control with nice blending. }
    property Opacity: Single read FOpacity write FOpacity default 1.0;

    { Window used to render the progress bar.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TCastleWindowBase read FWindow write FWindow;

    constructor Create;
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { Assign this to Progress.UserInterface to use OpenGL progress bar.
    This instance is created in initialization, freed in finalization. }
  WindowProgressInterface: TWindowProgressInterface;

implementation

uses SysUtils, CastleUtils, CastleKeysMouse, CastleControls, CastleGLBitmapFonts;

{ display -------------------------------------------------------------------- }

procedure DisplayProgress(Window: TCastleWindowBase);
var
  Margin: integer;
  BarHeight, y1, y2, YMiddle: TGLfloat;
  Progress: TProgress;
  ProgressInterface: TWindowProgressInterface;
  PositionFill: Single;
  Font: TGLBitmapFontAbstract;
begin
  Progress := TProgress(Window.UserData);
  ProgressInterface := Progress.UserInterface as TWindowProgressInterface;

  glLoadIdentity;
  glRasterPos2i(0, 0);
  ProgressInterface.GLImage.Draw;

  Margin := 100 * Window.width div 800;
  BarHeight := 50 * Window.height div 600;
  YMiddle := Window.Height * ProgressInterface.BarYPosition;
  y1 := YMiddle + BarHeight/2;
  y2 := YMiddle - BarHeight/2;

  PositionFill := Margin + (Cardinal(Window.Width) - 2 * Margin) *
    Progress.Position / Progress.Max;

  glColorOpacity(Theme.BarEmptyColor, ProgressInterface.Opacity);
  glRectf(PositionFill, y1, Window.Width - Margin, y2);

  glColorOpacity(Theme.BarFilledColor, ProgressInterface.Opacity);
  glRectf(Margin, y1, PositionFill, y2);

  glColorOpacity(Theme.TextColor, ProgressInterface.Opacity);
  if UIFont.RowHeight < BarHeight then
    Font := UIFont else
    Font := UIFontSmall;
  glRasterPos2f(Margin + 20, YMiddle - Font.RowHeight div 2);
  Font.Print(Progress.Title + ' ...');
end;

{ TWindowProgressInterface  ------------------------------------------------ }

constructor TWindowProgressInterface.Create;
begin
  inherited;
  FOpacity := 1;
end;

procedure TWindowProgressInterface.Init(Progress: TProgress);
var
  GoodSizeImage: TCastleImage;
begin
  Check(Window <> nil,
    'TWindowProgressInterface: You must assign Window before doing Init');

  { calculate GLImage }
  if Image <> nil then
  begin
    if (Image.Width <> Window.Width) or
       (Image.Height <> Window.Height) then
    begin
      GoodSizeImage := Image.MakeResized(Window.Width, Window.Height, riBilinear);
      try
        GLImage := TGLImage.Create(GoodSizeImage);
      finally FreeAndNil(GoodSizeImage) end;
    end else
      GLImage := TGLImage.Create(Image);
    BarYPosition := ImageBarYPosition;
  end else
  begin
    GLImage := Window.SaveScreenToGL;
    BarYPosition := DefaultImageBarYPosition;
  end;

  SavedMode := TGLMode.CreateReset(Window,
    GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT or GL_COLOR_BUFFER_BIT
    or GL_VIEWPORT_BIT,
    false, @DisplayProgress, nil, @NoClose);

  { init our window state }
  Window.UserData := Progress;
  Window.AutoRedisplay := true;

  Window.Cursor := mcWait;

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_SCISSOR_TEST);

  if Opacity < 1 then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
    glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
  end;

  { Set normal 2D projection.
    This is done by container for TUIControl with DrawStyle = ds2D, we have
    to repeat it here too. }
  glViewport(0, 0, Window.Width, Window.Height); // saved by GL_VIEWPORT_BIT
  OrthoProjection(0, Window.Width, 0, Window.Height);

  { To actually draw progress start. }
  Window.PostRedisplay;
  Window.FlushRedisplay;

  Application.ProcessMessage(false, false);
end;

procedure TWindowProgressInterface.Update(Progress: TProgress);
begin
  Application.ProcessAllMessages;
end;

procedure TWindowProgressInterface.Fini(Progress: TProgress);
begin
  FreeAndNil(GLImage);

  FreeAndNil(SavedMode);
end;

initialization
  WindowProgressInterface := TWindowProgressInterface.Create;
finalization
  FreeAndNil(WindowProgressInterface);
end.
