{
  Copyright 2002-2012 Michalis Kamburelis.

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


unit CastleProgress;

{$I castleconf.inc}

interface

uses GL, OpenGLFonts, CastleWindow, ProgressUnit, WindowModes, CastleGLUtils,
  Images;

type
  TWindowProgressInterface = class(TProgressUserInterface)
  private
    { Background image, as OpenGL list. }
    ImageList: TGLuint;
    BarYPosition: Single;
    FWindow: TCastleWindowBase;
    SavedMode: TGLMode;
  public
    { Window used to render the progress bar.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TCastleWindowBase read FWindow write FWindow;

    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { Assign this to Progress.UserInterface to use OpenGL progress bar.
    This instance is created in initialization, freed in finalization. }
  WindowProgressInterface: TWindowProgressInterface;

implementation

uses SysUtils, CastleUtils, KeysMouse, GLImages, CastleControls;

{ display -------------------------------------------------------------------- }

procedure DisplayProgress(Window: TCastleWindowBase);
var
  Margin: integer;
  BarHeight, y1, y2, YMiddle: TGLfloat;
  Progress: TProgress;
  ProgressInterface: TWindowProgressInterface;
begin
  Progress := TProgress(Window.UserData);
  ProgressInterface := Progress.UserInterface as TWindowProgressInterface;

  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(ProgressInterface.ImageList);

  Margin := 100 * Window.width div 800;
  BarHeight := 50 * Window.height div 600;
  YMiddle := Window.Height * ProgressInterface.BarYPosition;
  y1 := YMiddle + BarHeight/2;
  y2 := YMiddle - BarHeight/2;

  glColor3ub(192, 192, 192);
  glRectf(Margin, y1, Window.width-Margin, y2);
  glColor3f(0.2, 0.5, 0);
  glRectf(Margin, y1,
    Margin + (Cardinal(Window.width)-2*Margin) * Progress.Position/Progress.Max, y2);

  glColor3f(0, 0,0);
  glRasterPos2f(Margin + 20, YMiddle - UIFont.TextHeight('M') div 2);
  UIFont.Print(Progress.TitleWithPosition(true));
end;

{ TWindowProgressInterface  ------------------------------------------------ }

procedure TWindowProgressInterface.Init(Progress: TProgress);
var
  GoodSizeImage: TCastleImage;
begin
  Check(Window <> nil,
    'TWindowProgressInterface: You must assign Window before doing Init');

  { calculate ImageList }
  if Image <> nil then
  begin
    if (Image.Width <> Window.Width) or
       (Image.Height <> Window.Height) then
    begin
      GoodSizeImage := Image.MakeResized(Window.Width, Window.Height);
      try
        ImageList := ImageDrawToDisplayList(GoodSizeImage);
      finally FreeAndNil(GoodSizeImage) end;
    end else
      ImageList := ImageDrawToDisplayList(Image);
    BarYPosition := ImageBarYPosition;
  end else
  begin
    ImageList := Window.SaveScreen_ToDisplayList;
    BarYPosition := DefaultImageBarYPosition;
  end;

  SavedMode := TGLMode.CreateReset(Window,
    GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT, false,
    @DisplayProgress, nil, @NoClose);

  { init our window state }
  Window.UserData := Progress;
  Window.AutoRedisplay := true;

  Window.Cursor := mcWait;

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

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
  glFreeDisplayList(ImageList);

  FreeAndNil(SavedMode);
end;

initialization
  WindowProgressInterface := TWindowProgressInterface.Create;
finalization
  FreeAndNil(WindowProgressInterface);
end.
