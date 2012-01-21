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

unit TestOpenGLFonts;

interface

uses fpcunit, testutils, testregistry;

type
  TTestOpenGLFonts = class(TTestCase)
  published
    procedure TestMaxTextWidthTags;
  end;

implementation

uses SysUtils, Classes, CastleWindow,
  OpenGLFonts, OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_Bold_m15_Unit;

procedure TTestOpenGLFonts.TestMaxTextWidthTags;
var
  Window: TCastleWindow;
  F: TGLBitmapFont_Abstract;
  SList: TStringList;
  W1, W2: Single;
begin
  Window := TCastleWindow.Create(nil);
  try
    Window.Visible := false;
    Window.Open;
    F := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);

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
  RegisterTest(TTestOpenGLFonts);
end.
