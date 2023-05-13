// -*- compile-command: "./test_single_testcase.sh TTestCastleFonts" -*-
{
  Copyright 2011-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleFonts;

interface

uses {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleFonts = class(TCastleTestCase)
  strict private
    procedure FailIfFreeTypeMissing;
  published
    procedure TestMaxTextWidthHtml;
    procedure TestMaxTextWidthHtmlInWindow;
    procedure TestSizeFontFamily;
    procedure TestOverrideFont;
    procedure TestSizeChangeNotificationFontFamily;
    procedure TestSizeChangeNotificationCustomized;
  end;

implementation

{$ifdef TEXT_RUNNER}
  {$ifndef NO_WINDOW_SYSTEM}
    {$define TEST_CASTLE_WINDOW}
  {$endif}
{$endif}

uses SysUtils, Classes,
  {$ifdef TEST_CASTLE_WINDOW} CastleWindow, {$endif}
  CastleFonts, CastleTextureFont_DejaVuSansMonoBold_15, CastleLog,
  Font_LatoRegular_300, CastleInternalFreeTypeH;

procedure TTestCastleFonts.TestMaxTextWidthHtml;
var
  F: TCastleFont;
  SList: TStringList;
  W1, W2: Single;
begin
  F := TCastleFont.Create(nil);
  try
    F.Load(TextureFont_DejaVuSansMonoBold_15);

    SList := TStringList.Create;
    try
      SList.Append('blah');
      W1 := F.MaxTextWidth(SList);

      SList.Clear;
      SList.Append('<font color="#aabbcc">blah</font>');
      W2 := F.MaxTextWidth(SList, true);

      AssertTrue(W1 > 0);
      AssertTrue(W1 = W2);
    finally FreeAndNil(SList) end;
  finally FreeAndNil(F) end;
end;

procedure TTestCastleFonts.TestMaxTextWidthHtmlInWindow;
{$ifdef TEST_CASTLE_WINDOW}
var
  Window: TCastleWindow;
begin
  // should work with OpenGL context too, actually it doesn't matter now
  Window := TCastleWindow.Create(nil);
  try
    Window.Visible := false;
    Window.Open;
    TestMaxTextWidthHtml;
    Window.Close;
  finally FreeAndNil(Window) end;
{$else}
begin
{$endif}
end;

procedure TTestCastleFonts.TestSizeFontFamily;
var
  Font: TCastleFont;
  Family: TCastleFontFamily;
  Customized: TCustomizedFont;
begin
  Font := TCastleFont.Create(nil);
  try
    Font.Load(TextureFont_DejaVuSansMonoBold_15);

    AssertEquals(15, Font.Size);
    AssertEquals(15, Font.EffectiveSize);
    AssertEquals(14, Font.Height);

    Family := TCastleFontFamily.Create(nil);
    try
      Family.Regular := Font;

      AssertEquals(0, Family.Size);
      AssertEquals(15, Family.EffectiveSize);
      AssertEquals(14, Family.Height);

      Family.Size := 30;
      AssertEquals(30, Family.Size);
      AssertEquals(30, Family.EffectiveSize);
      AssertEquals(28, Family.Height);
    finally FreeAndNil(Family) end;

    Customized := TCustomizedFont.Create(nil);
    try
      Customized.SourceFont := Font;

      AssertEquals(0, Customized.Size);
      AssertEquals(15, Customized.EffectiveSize);
      AssertEquals(14, Customized.Height);

      Customized.Size := 30;
      AssertEquals(30, Customized.Size);
      AssertEquals(30, Customized.EffectiveSize);
      AssertEquals(28, Customized.Height);
    finally FreeAndNil(Customized) end;

    Font.Size := 60;
    AssertEquals(60, Font.Size);
    AssertEquals(60, Font.EffectiveSize);
    AssertEquals(56, Font.Height);

    Family := TCastleFontFamily.Create(nil);
    try
      Family.Regular := Font;

      AssertEquals(0, Family.Size);
      AssertEquals(60, Family.EffectiveSize);
      AssertEquals(56, Family.Height);

      Family.Size := 30;
      AssertEquals(30, Family.Size);
      AssertEquals(30, Family.EffectiveSize);
      AssertEquals(28, Family.Height);
    finally FreeAndNil(Family) end;

    Customized := TCustomizedFont.Create(nil);
    try
      Customized.SourceFont := Font;

      AssertEquals(0, Customized.Size);
      AssertEquals(60, Customized.EffectiveSize);
      AssertEquals(56, Customized.Height);

      Customized.Size := 30;
      AssertEquals(30, Customized.Size);
      AssertEquals(30, Customized.EffectiveSize);
      AssertEquals(28, Customized.Height);
    finally FreeAndNil(Customized) end;
  finally FreeAndNil(Font) end;
end;

procedure TTestCastleFonts.TestOverrideFont;
var
  LargeDigitsFont: TCastleFont;
  CustomizedFont: TCustomizedFont;
  FontFamily: TCastleFontFamily;
begin
  LargeDigitsFont := TCastleFont.Create(nil);
  try
    LargeDigitsFont.Load(TextureFont_LatoRegular_300);
    LargeDigitsFont.MeasureHeight := '123';
    LargeDigitsFont.MeasureCapHeight := '123';
    LargeDigitsFont.MeasureDescenderHeight := '';
    LargeDigitsFont.FontData.UseFallbackGlyph := false;
    AssertEquals(300, TextureFont_LatoRegular_300.Size);

    AssertSameValue(300, LargeDigitsFont.Size);
    AssertSameValue(522, LargeDigitsFont.TextWidth('123'), 1);
    AssertSameValue(221, LargeDigitsFont.TextHeight('123'), 1);
    AssertSameValue(0, LargeDigitsFont.TextWidth('abc'));
    AssertSameValue(0, LargeDigitsFont.TextHeight('abc'));
    AssertSameValue(221, LargeDigitsFont.Height);
    AssertSameValue(219, LargeDigitsFont.CapHeight);
    AssertSameValue(0, LargeDigitsFont.DescenderHeight);

    CustomizedFont := TCustomizedFont.Create(nil);
    try
      CustomizedFont.SourceFont := LargeDigitsFont;
      AssertSameValue(0, CustomizedFont.Size); // not customized yet
      AssertSameValue(522, CustomizedFont.TextWidth('123'), 1);
      AssertSameValue(221, CustomizedFont.TextHeight('123'), 1);
      AssertSameValue(0, CustomizedFont.TextWidth('abc'));
      AssertSameValue(0, CustomizedFont.TextHeight('abc'));
      AssertSameValue(221, CustomizedFont.Height);
      AssertSameValue(219, CustomizedFont.CapHeight);
      AssertSameValue(0, CustomizedFont.DescenderHeight);
    finally FreeAndNil(CustomizedFont) end;

    FontFamily := TCastleFontFamily.Create(nil);
    try
      FontFamily.Regular := LargeDigitsFont;
      AssertSameValue(0, FontFamily.Size); // not customized yet
      AssertSameValue(522, FontFamily.TextWidth('123'), 1);
      AssertSameValue(221, FontFamily.TextHeight('123'), 1);
      AssertSameValue(0, FontFamily.TextWidth('abc'));
      AssertSameValue(0, FontFamily.TextHeight('abc'));
      AssertSameValue(221, FontFamily.Height);
      AssertSameValue(219, FontFamily.CapHeight);
      AssertSameValue(0, FontFamily.DescenderHeight);
    finally FreeAndNil(FontFamily) end;

    LargeDigitsFont.Size := 1000;
    AssertSameValue(1000, LargeDigitsFont.Size);
    AssertSameValue(522 * 10/3, LargeDigitsFont.TextWidth('123'), 10/3);
    AssertSameValue(221 * 10/3, LargeDigitsFont.TextHeight('123'), 10/3);
    AssertSameValue(0, LargeDigitsFont.TextWidth('abc'));
    AssertSameValue(0, LargeDigitsFont.TextHeight('abc'));
    AssertSameValue(221 * 10/3, LargeDigitsFont.Height, 10/3);
    AssertSameValue(219 * 10/3, LargeDigitsFont.CapHeight, 10/3);
    AssertSameValue(0, LargeDigitsFont.DescenderHeight);

    CustomizedFont := TCustomizedFont.Create(nil);
    try
      CustomizedFont.SourceFont := LargeDigitsFont;
      CustomizedFont.Size := 2000;
      AssertSameValue(2000, CustomizedFont.Size); // not customized yet
      AssertSameValue(522 * 20/3, CustomizedFont.TextWidth('123'), 20/3);
      AssertSameValue(221 * 20/3, CustomizedFont.TextHeight('123'), 20/3);
      AssertSameValue(0, CustomizedFont.TextWidth('abc'));
      AssertSameValue(0, CustomizedFont.TextHeight('abc'));
      AssertSameValue(221 * 20/3, CustomizedFont.Height, 20/3);
      AssertSameValue(219 * 20/3, CustomizedFont.CapHeight, 20/3);
      AssertSameValue(0, CustomizedFont.DescenderHeight);
    finally FreeAndNil(CustomizedFont) end;

    FontFamily := TCastleFontFamily.Create(nil);
    try
      FontFamily.Regular := LargeDigitsFont;
      FontFamily.Size := 2000;
      AssertSameValue(2000, FontFamily.Size); // not customized yet
      AssertSameValue(522 * 20/3, FontFamily.TextWidth('123'), 20/3);
      AssertSameValue(221 * 20/3, FontFamily.TextHeight('123'), 20/3);
      AssertSameValue(0, FontFamily.TextWidth('abc'));
      AssertSameValue(0, FontFamily.TextHeight('abc'));
      AssertSameValue(221 * 20/3, FontFamily.Height, 20/3);
      AssertSameValue(219 * 20/3, FontFamily.CapHeight, 20/3);
      AssertSameValue(0, FontFamily.DescenderHeight);
    finally FreeAndNil(FontFamily) end;
  finally FreeAndNil(LargeDigitsFont) end;
end;

procedure TTestCastleFonts.FailIfFreeTypeMissing;
begin
  { Without FreeType, we cannot load .ttf so some tests have to fail.
    We don't want to ignore this problem and turn it into hard-to-notice
    warning (it's better to force you to setup proper DLLs to have clean tests output),
    but we do this check to generate exception with more obvious error message.
    This way users of this test on Windows (where FreeType most often may be missing)
    will know what's going on. }
  if not FreeTypeLibraryInitialized then
  begin
    { TODO: Why doing
        raise Exception.Create(...)
      here, instead of Fail, does not report test as failed in castle_tester GUI?
      Using "Fail" is OK, it's actually cleaner, but still raising exception
      should also be reported.
      Observed on Delphi 10.2.3 / Win64. }

    Fail('FreeType library not available, so TTestCastleFonts.TestSizeChangeNotificationFontFamily has to fail.'
      {$ifdef MSWINDOWS}
      + ' On Windows, be sure to place proper DLL files alongside EXE. It is easiest to build using CGE editor that will place proper DLLs automatically.'
      {$endif}
    );
  end;
end;

procedure TTestCastleFonts.TestSizeChangeNotificationFontFamily;
var
  F: TCastleFont;
  FF: TCastleFontFamily;
begin
  FailIfFreeTypeMissing;

  F := TCastleFont.Create(nil);
  AssertEquals(0, F.Height);

  FF := TCastleFontFamily.Create(nil);
  AssertEquals(0, FF.Height);
  FF.Regular := F;
  AssertEquals(0, FF.Height);

  F.Url := 'castle-data:/fonts/PARPG.ttf';
  AssertSameValue(22, F.Height);
  AssertSameValue(22, FF.Height);
  // writeln(F.Height:1:2);
  // writeln(FF.Height:1:2);

  FreeAndNil(F);
  FreeAndNil(FF);
end;

procedure TTestCastleFonts.TestSizeChangeNotificationCustomized;
var
  F: TCastleFont;
  CF: TCastleFontFamily;
begin
  FailIfFreeTypeMissing;

  F := TCastleFont.Create(nil);
  AssertEquals(0, F.Height);

  CF := TCastleFontFamily.Create(nil);
  AssertEquals(0, CF.Height);
  CF.Regular := F;
  AssertEquals(0, CF.Height);

  F.Url := 'castle-data:/fonts/PARPG.ttf';
  AssertSameValue(22, F.Height);
  AssertSameValue(22, CF.Height);
  // writeln(F.Height:1:2);
  // writeln(CF.Height:1:2);

  FreeAndNil(F);
  FreeAndNil(CF);
end;

initialization
  RegisterTest(TTestCastleFonts);
end.
