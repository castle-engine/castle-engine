{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2011-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleGLFonts;

interface

uses fpcunit, testutils, testregistry;

type
  TTestCastleGLFonts = class(TTestCase)
  published
    procedure TestMaxTextWidthTags;
  end;

implementation

uses SysUtils, Classes, CastleWindow,
  CastleGLBitmapFonts, CastleBitmapFont_BVSansMono_Bold_m15;

procedure TTestCastleGLFonts.TestMaxTextWidthTags;
var
  Window: TCastleWindow;
  F: TGLBitmapFontAbstract;
  SList: TStringList;
  W1, W2: Single;
begin
  Window := TCastleWindow.Create(nil);
  try
    Window.Visible := false;
    Window.Open;
    F := TGLBitmapFont.Create(BitmapFont_BVSansMono_Bold_m15);

    SList := TStringList.Create;

    SList.Append('blah');
    W1 := F.MaxTextWidth(SList);

    SList.Clear;
    SList.Append('<font color="#aabbcc">blah</font>');
    W2 := F.MaxTextWidth(SList, true);

    Assert(W1 = W2);
    FreeAndNil(SList);
    FreeAndNil(F);
    Window.Close;
  finally FreeAndNil(Window) end;
end;

initialization
  RegisterTest(TTestCastleGLFonts);
end.
