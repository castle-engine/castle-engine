{
  Copyright 2004-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleFont.BreakLines method.
  Resize the window and watch how the text lines are automatically broken.

  By default we use standard UIFont.
  Call with command-line option --custom-font to use a custom font, like this:

    ./test_font_break --custom-font data/PARPG.ttf
}

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

program test_font_break;

uses CastleWindow, CastleGLUtils, SysUtils, Classes, CastleParameters,
  CastleUtils, CastleFonts, CastleVectors, CastleStringUtils, CastleColors,
  CastleControls, CastleKeysMouse, CastleRectangles, CastleControlsImages,
  CastleLog, CastleApplicationProperties;

var
  Window: TCastleWindowBase;
  BoxWidth: Integer;

procedure Render(Container: TUIContainer);
var
  X1: Integer;
begin
  X1 := (Window.Width - BoxWidth) div 2;
  Theme.Draw(Rectangle(X1, 0, BoxWidth, Window.Height), tiActiveFrame);
  UIFont.PrintBrokenString(x1, UIFont.Descend, White,
    'blah blah blah, I''m a long long long text and'
    +' I''m very curious how I will be broken to fit nicely between those'
    +' two yellow lines on the screen.' +nl+
    'BTW: Note that line breaks will be correctly preserved in the broken'
    +' text.' +nl+
    nl+
    'You can resize this window and watch how this text breaks at'
    +' various line widths.', BoxWidth,
    false, 0);
end;

procedure Resize(Container: TUIContainer);
begin
  BoxWidth := Window.Width * 2 div 3;
end;

var
  CustomFont: string;

const
  Options: array [0..0] of TOption =
  ( (Short:'c'; Long: 'custom-font'; Argument: oaRequired) );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: CustomFont := Argument;
  end;
end;

var
  NewFont: TTextureFont;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  Window := TCastleWindowBase.Create(Application);

  Window.ParseParameters(StandardParseOptions);
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(0);

  Theme.ImagesPersistent[tiActiveFrame].Image := FrameYellow;
  Theme.ImagesPersistent[tiActiveFrame].OwnsImage := false;

  if CustomFont <> '' then
  try
    NewFont := TTextureFont.Create(Application);
    NewFont.Load(CustomFont, 20, true);
    UIFont := NewFont;
  except
    { in case FreeType library is not available:
      make a warning, leave UIFont unchanged, and just continue }
    on E: EFreeTypeLibraryNotFound do
      WritelnWarning('Font', 'FreeType library not found, cannot use custom font');
  end;

  Window.OnResize := @Resize;
  Window.DepthBits := 0;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.Caption := 'Font.BreakLines demo';
  Window.OnRender := @Render;
  Window.OpenAndRun;
end.
