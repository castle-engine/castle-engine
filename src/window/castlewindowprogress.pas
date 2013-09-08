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

const
  Dots = '...';

{ Make Text shorter to fit the text width (as rendered using Font)
  inside MaxWidth (in pixels). }
procedure MakeTextFit(var Text: string; const Font: TGLBitmapFontAbstract;
  const MaxWidth: Integer);
var
  DotsWidth: Integer;

  { Make Text shorter by preserving first and last words, inserting
    dots inside, and preserving as much as possible text between first and last
    words. }
  function TrimIntelligent: boolean;
  begin
    Result := false;

    { Not implemented for now, not needed. The idea of algorithm below.
      Separator characters are whitespace or / or \. They include slash
      and backslash, to work nicely with URLs and filenames, to show
      the last (usually most relevant) part of URL / filename.

      Find first separator in Text
      if not found, exit false

      Find last separator in Text
      if not found (should not happen) or <= first separator, exit false

      Prefix := Text up to and including first separator
      Suffix := Text suffix, including last separator

      NewWidth := Font.TextWidth(Prefix) + Font.TextWidth(Suffix) + DotsWidth;
      if NewWidth > MaxWidth then exit false

      // We know that we're OK now, using Prefix + ... + Suffix is good.
      // See how many additional characters we can add and still fit in MaxWidth.
      Result := true;
      NextIndex := Length(Prefix) + 1;
      while NextIndex < LastSeparator then
        PotentialPrefix := Prefix + Text[NextIndex]
        PotentialNewWidth := NewWidth + Font.TextWidth(Text[NextIndex])
        if PotentialNewWidth > MaxWidth then Break;
        NewWidth := PotentialNewWidth;
        Prefix := PotentialPrefix;
      end;
      Text := Prefix + Dots + Suffix;
    }
  end;

  { Make Text shorter by taking as long prefix as possible to fit
    the prefix + Dots. }
  procedure TrimSimple;
  var
    NewTextDotsWidth, PotentialNewTextDotsWidth: Integer;
    NewText, PotentialNewText: string;
    C: char;
  begin
    NewText := '';
    NewTextDotsWidth := DotsWidth;
    while Length(NewText) < Length(Text) do
    begin
      C := Text[Length(NewText) + 1];
      PotentialNewText := NewText + C;
      PotentialNewTextDotsWidth := NewTextDotsWidth + Font.TextWidth(C);
      if PotentialNewTextDotsWidth > MaxWidth then Break;
      NewText := PotentialNewText;
      NewTextDotsWidth := PotentialNewTextDotsWidth;
    end;
    Text := NewText + Dots;
  end;

var
  TextWidth: Integer;
begin
  TextWidth := Font.TextWidth(Text);
  if TextWidth <= MaxWidth then
  begin
    { No trimming needs to be done. Add dots at the end, if we have space. }
    if Font.TextWidth(Text + Dots) < MaxWidth then
      Text += Dots;
    Exit;
  end;

  DotsWidth := Font.TextWidth(Dots);

  if not TrimIntelligent then
    TrimSimple;
end;

{ display -------------------------------------------------------------------- }

procedure DisplayProgress(Window: TCastleWindowBase);
const
  InsideMargin = 20;
var
  Margin, MaxTextWidth, BarHeight, y1, y2, YMiddle: Integer;
  Progress: TProgress;
  ProgressInterface: TWindowProgressInterface;
  PositionFill: Single;
  Font: TGLBitmapFontAbstract;
  Caption: string;
begin
  Progress := TProgress(Window.UserData);
  ProgressInterface := Progress.UserInterface as TWindowProgressInterface;

  glLoadIdentity;
  ProgressInterface.GLImage.Draw(0, 0);

  Margin := 100 * Window.width div 800;
  BarHeight := 50 * Window.height div 600;
  YMiddle := Round(Window.Height * ProgressInterface.BarYPosition);
  y1 := YMiddle + BarHeight div 2;
  y2 := YMiddle - BarHeight div 2;

  PositionFill := Margin + (Cardinal(Window.Width) - 2 * Margin) *
    Progress.Position / Progress.Max;

  glColorOpacity(Theme.BarEmptyColor, ProgressInterface.Opacity);
  glRectf(PositionFill, y1, Window.Width - Margin, y2);

  glColorOpacity(Theme.BarFilledColor, ProgressInterface.Opacity);
  glRectf(Margin, y1, PositionFill, y2);

  MaxTextWidth := Window.Width - Margin * 2 - InsideMargin;

  glColorOpacity(Theme.TextColor, ProgressInterface.Opacity);
  Caption := Progress.Title;
  if (UIFont.RowHeight < BarHeight) and
     (UIFont.TextWidth(Caption) < MaxTextWidth) then
  begin
    Font := UIFont;
    if UIFont.TextWidth(Caption + Dots) < MaxTextWidth then
      Caption += Dots;
  end else
  begin
    Font := UIFontSmall;
    MakeTextFit(Caption, Font, MaxTextWidth);
  end;
  Font.Print(Margin + InsideMargin, YMiddle - Font.RowHeight div 2, Caption);
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
    @DisplayProgress, nil, @NoClose);

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
  glViewport(Window.Rect); // saved by GL_VIEWPORT_BIT
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
