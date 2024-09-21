{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalEgl unit. }
unit TestCastleInternalEgl;

interface

uses CastleTester;

type
  TTestCastleInternalEgl = class(TCastleTestCase)
  published
    procedure TestEglTypeSizes;
  end;

implementation

uses SysUtils, CastleInternalEgl;

procedure TTestCastleInternalEgl.TestEglTypeSizes;
begin
  { All EGLNative* types have (so far, on platforms we tested)
    always size of pointer, and this is safest,
    see castleinternalegl.pas comments.

    If in doubt, use tests/egl_sizes_test/egl_sizes_test.c
    to see sizes from C headers.

    This testcase may then be used to test that our Pascal bindings
    follow C sizes. For now, it is trivial, all sizes match Pointer sizes,
    but it's not guaranteed by EGL to be always like this. }

  AssertEquals(SizeOf(POinter), SizeOf(EGLNativeDisplayType));
  AssertEquals(SizeOf(POinter), SizeOf(EGLNativeWindowType));
  AssertEquals(SizeOf(POinter), SizeOf(EGLNativePixmapType));
end;

initialization
  RegisterTest(TTestCastleInternalEgl);
end.
