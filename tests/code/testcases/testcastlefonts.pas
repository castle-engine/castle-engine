// -*- compile-command: "./test_single_testcase.sh TTestCastleFonts" -*-
{
  Copyright 2011-2022 Michalis Kamburelis.

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
    AssertEquals(14, Font.RowHeight);

    Family := TCastleFontFamily.Create(nil);
    try
      Family.Regular := Font;

      AssertEquals(0, Family.Size);
      AssertEquals(15, Family.EffectiveSize);
      AssertEquals(14, Family.RowHeight);

      Family.Size := 30;
      AssertEquals(30, Family.Size);
      AssertEquals(30, Family.EffectiveSize);
      AssertEquals(28, Family.RowHeight);
    finally FreeAndNil(Family) end;

    Customized := TCustomizedFont.Create(nil);
    try
      Customized.SourceFont := Font;

      AssertEquals(0, Customized.Size);
      AssertEquals(15, Customized.EffectiveSize);
      AssertEquals(14, Customized.RowHeight);

      Customized.Size := 30;
      AssertEquals(30, Customized.Size);
      AssertEquals(30, Customized.EffectiveSize);
      AssertEquals(28, Customized.RowHeight);
    finally FreeAndNil(Customized) end;

    Font.Size := 60;
    AssertEquals(60, Font.Size);
    AssertEquals(60, Font.EffectiveSize);
    AssertEquals(56, Font.RowHeight);

    Family := TCastleFontFamily.Create(nil);
    try
      Family.Regular := Font;

      AssertEquals(0, Family.Size);
      AssertEquals(60, Family.EffectiveSize);
      AssertEquals(56, Family.RowHeight);

      Family.Size := 30;
      AssertEquals(30, Family.Size);
      AssertEquals(30, Family.EffectiveSize);
      AssertEquals(28, Family.RowHeight);
    finally FreeAndNil(Family) end;

    Customized := TCustomizedFont.Create(nil);
    try
      Customized.SourceFont := Font;

      AssertEquals(0, Customized.Size);
      AssertEquals(60, Customized.EffectiveSize);
      AssertEquals(56, Customized.RowHeight);

      Customized.Size := 30;
      AssertEquals(30, Customized.Size);
      AssertEquals(30, Customized.EffectiveSize);
      AssertEquals(28, Customized.RowHeight);
    finally FreeAndNil(Customized) end;
  finally FreeAndNil(Font) end;
end;

type
  TLargeDigitsFont = class(TCastleFont)
    { The "Font_LatoRegular_300" font has only digits, misses other chars.
      So default calculation of RowHeight and friends doesn't work. }
    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Single); override;
  end;

procedure TLargeDigitsFont.Measure(
  out ARowHeight, ARowHeightBase, ADescend: Single);
const
  ForSize = 300;
begin
  ARowHeight := 220; // this is font digit height, assuming font Size = 300
  ARowHeightBase := ARowHeight;
  ADescend := 0;

  // make the values valid for current size
  ARowHeight := ARowHeight * (Size / ForSize);
  ARowHeightBase := ARowHeightBase * (Size / ForSize);
  ADescend := ADescend * (Size / ForSize);
end;

procedure TTestCastleFonts.TestOverrideFont;
var
  LargeDigitsFont: TLargeDigitsFont;
  CustomizedFont: TCustomizedFont;
  FontFamily: TCastleFontFamily;
begin
  LargeDigitsFont := TLargeDigitsFont.Create(nil);
  try
    LargeDigitsFont.Load(TextureFont_LatoRegular_300);
    LargeDigitsFont.FontData.UseFallbackGlyph := false;
    AssertEquals(300, TextureFont_LatoRegular_300.Size);

    AssertSameValue(300, LargeDigitsFont.Size);
    AssertSameValue(522, LargeDigitsFont.TextWidth('123'), 1);
    AssertSameValue(221, LargeDigitsFont.TextHeight('123'), 1);
    AssertSameValue(0, LargeDigitsFont.TextWidth('abc'));
    AssertSameValue(0, LargeDigitsFont.TextHeight('abc'));
    AssertSameValue(220, LargeDigitsFont.RowHeight);
    AssertSameValue(220, LargeDigitsFont.RowHeightBase);
    AssertSameValue(0, LargeDigitsFont.Descend);

    CustomizedFont := TCustomizedFont.Create(nil);
    try
      CustomizedFont.SourceFont := LargeDigitsFont;
      AssertSameValue(0, CustomizedFont.Size); // not customized yet
      AssertSameValue(522, CustomizedFont.TextWidth('123'), 1);
      AssertSameValue(221, CustomizedFont.TextHeight('123'), 1);
      AssertSameValue(0, CustomizedFont.TextWidth('abc'));
      AssertSameValue(0, CustomizedFont.TextHeight('abc'));
      AssertSameValue(220, CustomizedFont.RowHeight);
      AssertSameValue(220, CustomizedFont.RowHeightBase);
      AssertSameValue(0, CustomizedFont.Descend);
    finally FreeAndNil(CustomizedFont) end;

    FontFamily := TCastleFontFamily.Create(nil);
    try
      FontFamily.Regular := LargeDigitsFont;
      AssertSameValue(0, FontFamily.Size); // not customized yet
      AssertSameValue(522, FontFamily.TextWidth('123'), 1);
      AssertSameValue(221, FontFamily.TextHeight('123'), 1);
      AssertSameValue(0, FontFamily.TextWidth('abc'));
      AssertSameValue(0, FontFamily.TextHeight('abc'));
      AssertSameValue(220, FontFamily.RowHeight);
      AssertSameValue(220, FontFamily.RowHeightBase);
      AssertSameValue(0, FontFamily.Descend);
    finally FreeAndNil(FontFamily) end;

    LargeDigitsFont.Size := 1000;
    AssertSameValue(1000, LargeDigitsFont.Size);
    AssertSameValue(522 * 10/3, LargeDigitsFont.TextWidth('123'), 10/3);
    AssertSameValue(221 * 10/3, LargeDigitsFont.TextHeight('123'), 10/3);
    AssertSameValue(0, LargeDigitsFont.TextWidth('abc'));
    AssertSameValue(0, LargeDigitsFont.TextHeight('abc'));
    AssertSameValue(220 * 10/3, LargeDigitsFont.RowHeight, 10/3);
    AssertSameValue(220 * 10/3, LargeDigitsFont.RowHeightBase, 10/3);
    AssertSameValue(0, LargeDigitsFont.Descend);

    CustomizedFont := TCustomizedFont.Create(nil);
    try
      CustomizedFont.SourceFont := LargeDigitsFont;
      CustomizedFont.Size := 2000;
      AssertSameValue(2000, CustomizedFont.Size); // not customized yet
      AssertSameValue(522 * 20/3, CustomizedFont.TextWidth('123'), 20/3);
      AssertSameValue(221 * 20/3, CustomizedFont.TextHeight('123'), 20/3);
      AssertSameValue(0, CustomizedFont.TextWidth('abc'));
      AssertSameValue(0, CustomizedFont.TextHeight('abc'));
      AssertSameValue(220 * 20/3, CustomizedFont.RowHeight, 20/3);
      AssertSameValue(220 * 20/3, CustomizedFont.RowHeightBase, 20/3);
      AssertSameValue(0, CustomizedFont.Descend);
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
      AssertSameValue(220 * 20/3, FontFamily.RowHeight, 20/3);
      AssertSameValue(220 * 20/3, FontFamily.RowHeightBase, 20/3);
      AssertSameValue(0, FontFamily.Descend);
    finally FreeAndNil(FontFamily) end;
  finally FreeAndNil(LargeDigitsFont) end;
end;

procedure TTestCastleFonts.TestSizeChangeNotificationFontFamily;
var
  F: TCastleFont;
  FF: TCastleFontFamily;
begin
  // if not FreeTypeLibraryInitialized then
  // begin
  //   WritelnWarning('FreeType library not available, aborting TTestCastleFonts.TestSizeChangeNotificationFontFamily');
  //   Exit;
  // end;

  F := TCastleFont.Create(nil);
  AssertEquals(0, F.RowHeight);

  FF := TCastleFontFamily.Create(nil);
  AssertEquals(0, FF.RowHeight);
  FF.Regular := F;
  AssertEquals(0, FF.RowHeight);

  F.Url := 'castle-data:/fonts/PARPG.ttf';
  AssertSameValue(21, F.RowHeight);
  AssertSameValue(21, FF.RowHeight);
  // writeln(F.RowHeight:1:2);
  // writeln(FF.RowHeight:1:2);

  FreeAndNil(F);
  FreeAndNil(FF);
end;

procedure TTestCastleFonts.TestSizeChangeNotificationCustomized;
var
  F: TCastleFont;
  CF: TCastleFontFamily;
begin
  // if not FreeTypeLibraryInitialized then
  // begin
  //   WritelnWarning('FreeType library not available, aborting TTestCastleFonts.TestSizeChangeNotificationCustomized');
  //   Exit;
  // end;

  F := TCastleFont.Create(nil);
  AssertEquals(0, F.RowHeight);

  CF := TCastleFontFamily.Create(nil);
  AssertEquals(0, CF.RowHeight);
  CF.Regular := F;
  AssertEquals(0, CF.RowHeight);

  F.Url := 'castle-data:/fonts/PARPG.ttf';
  AssertSameValue(21, F.RowHeight);
  AssertSameValue(21, CF.RowHeight);
  // writeln(F.RowHeight:1:2);
  // writeln(CF.RowHeight:1:2);

  FreeAndNil(F);
  FreeAndNil(CF);
end;

initialization
  RegisterTest(TTestCastleFonts);
end.
