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

uses CastleWindow, CastleProgress, CastleWindowModes,
  CastleImages, CastleGLImages, CastleUIControls;

type
  TCastleProgressBar = class(TUIControlPos)
  private
    { Background image. }
    Background: TGLImage;
    YPosition: Single;
    Progress: TProgress;
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

  TWindowProgressInterface = class(TProgressUserInterface)
  private
    Bar: TCastleProgressBar;
    FWindow: TCastleWindowCustom;
    SavedMode: TGLMode;
  public
    { Window used to render the progress bar.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TCastleWindowCustom read FWindow write FWindow;

    constructor Create;
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { Assign this to Progress.UserInterface to use progress bar
    drawn on TCastleWindow.
    This instance is created in initialization, freed in finalization. }
  WindowProgressInterface: TWindowProgressInterface;

implementation

uses SysUtils, CastleUtils, CastleKeysMouse, CastleControls, CastleGLBitmapFonts,
  CastleVectors, CastleRectangles;

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

{ TCastleProgressBar --------------------------------------------------------- }

function TCastleProgressBar.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleProgressBar.Draw;
const
  Padding = 20;
var
  XMargin, MaxTextWidth, Height, YMiddle: Integer;
  Font: TGLBitmapFontAbstract;
  Caption: string;
  BarRect, FillRect: TRectangle;
begin
  if not GetExists then Exit;

  Background.Draw(0, 0);

  XMargin := ContainerWidth div 8;
  Height := ContainerHeight div 12;
  YMiddle := Round(ContainerHeight * YPosition);
  Bottom := YMiddle - Height div 2;
  BarRect := Rectangle(XMargin, Bottom, ContainerWidth - 2 * XMargin, Height);
  Theme.Draw(BarRect, tiProgressBar);

  FillRect := BarRect.LeftPart(Round(BarRect.Width * Progress.Position / Progress.Max));
  Theme.Draw(FillRect, tiProgressFill);

  MaxTextWidth := BarRect.Width - Padding;
  Caption := Progress.Title;
  if (UIFont.RowHeight < Height) and
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
  Font.Print(XMargin + Padding, YMiddle - Font.RowHeight div 2,
    Theme.TextColor, Caption);
end;

{ TWindowProgressInterface  ------------------------------------------------ }

constructor TWindowProgressInterface.Create;
begin
  inherited;
end;

procedure TWindowProgressInterface.Init(Progress: TProgress);
var
  GoodSizeImage: TCastleImage;
begin
  Check(Window <> nil,
    'TWindowProgressInterface: You must assign Window before doing Init');

  Bar := TCastleProgressBar.Create(nil);
  Bar.Progress := Progress;

  { calculate GLImage }
  if Image <> nil then
  begin
    if (Image.Width <> Window.Width) or
       (Image.Height <> Window.Height) then
    begin
      GoodSizeImage := Image.MakeResized(Window.Width, Window.Height, riBilinear);
      try
        Bar.Background := TGLImage.Create(GoodSizeImage);
      finally FreeAndNil(GoodSizeImage) end;
    end else
      Bar.Background := TGLImage.Create(Image);
    Bar.YPosition := ImageBarYPosition;
  end else
  begin
    Bar.Background := Window.SaveScreenToGL;
    Bar.YPosition := DefaultImageBarYPosition;
  end;

  SavedMode := TGLMode.CreateReset(Window, 0, nil, nil, @NoClose);

  Window.Controls.InsertFront(Bar);

  { init our window state }
  Window.AutoRedisplay := true;
  Window.Cursor := mcWait;
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
  FreeAndNil(Bar.Background);
  FreeAndNil(Bar);
  FreeAndNil(SavedMode);
end;

initialization
  WindowProgressInterface := TWindowProgressInterface.Create;
finalization
  FreeAndNil(WindowProgressInterface);
end.
