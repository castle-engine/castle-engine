{
  Copyright 2011-2018 Michalis Kamburelis.

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

uses fpcunit, testutils, testregistry;

type
  TTestCastleFonts = class(TTestCase)
  published
    procedure TestMaxTextWidthHtml;
  end;

implementation

uses SysUtils, Classes, CastleWindow,
  CastleFonts, CastleTextureFont_DejaVuSansMonoBold_15, CastleFontFamily;

procedure TTestCastleFonts.TestMaxTextWidthHtml;
var
  Window: TCastleWindow;

  procedure TestMeasuring;
  var
    F: TCastleFont;
    SList: TStringList;
    W1, W2: Single;
  begin
    F := TTextureFont.Create(TextureFont_DejaVuSansMonoBold_15);

    SList := TStringList.Create;

    SList.Append('blah');
    W1 := F.MaxTextWidth(SList);

    SList.Clear;
    SList.Append('<font color="#aabbcc">blah</font>');
    W2 := F.MaxTextWidth(SList, true);

    AssertTrue(W1 > 0);
    AssertTrue(W1 = W2);
    FreeAndNil(SList);
    FreeAndNil(F);
  end;

begin
  TestMeasuring; // should work without OpenGL context too

  Window := TCastleWindow.Create(nil);
  try
    Window.Visible := false;
    Window.Open;
    TestMeasuring;
    Window.Close;
  finally FreeAndNil(Window) end;
end;

initialization
  RegisterTest(TTestCastleFonts);
end.
