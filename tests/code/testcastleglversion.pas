// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestGLVersion" -*-
{
  Copyright 2008-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleGLVersion;

interface

uses {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry
  {$else}CastleTester{$endif};

type
  TTestGLVersion = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

uses SysUtils, CastleGLVersion;

procedure TTestGLVersion.Test1;
var
  G: TGLVersion;
begin
  G := TGLVersion.Create('1.4 (2.1 Mesa 7.0.4)',
    'Mesa project: www.mesa3d.org', 'Mesa GLX Indirect');
  try
    AssertTrue(G.VendorVersion = '(2.1 Mesa 7.0.4)');
    AssertTrue(G.Vendor = 'Mesa project: www.mesa3d.org');
    AssertTrue(G.Renderer = 'Mesa GLX Indirect');

    AssertTrue(G.Major = 1);
    AssertTrue(G.Minor = 4);

    { Test AtLeast method }
    AssertTrue(G.AtLeast(0, 9));
    AssertTrue(G.AtLeast(1, 0));
    AssertTrue(G.AtLeast(1, 3));
    AssertTrue(G.AtLeast(1, 4));
    AssertTrue(not G.AtLeast(1, 5));
    AssertTrue(not G.AtLeast(2, 0));

    AssertTrue(not G.ReleaseExists);

    AssertTrue(G.Mesa);
    AssertTrue(G.VendorMajor = 7);
    AssertTrue(G.VendorMinor = 0);
    AssertTrue(G.VendorRelease = 4);
  finally FreeAndNil(G) end;

  G := TGLVersion.Create('2.1 Mesa 7.1', 'Brian Paul',
    'Mesa DRI Intel(blah blah blah)');
  try
    AssertTrue(G.VendorVersion = 'Mesa 7.1');
    AssertTrue(G.Vendor = 'Brian Paul');
    AssertTrue(G.Renderer = 'Mesa DRI Intel(blah blah blah)');

    AssertTrue(G.Major = 2);
    AssertTrue(G.Minor = 1);

    AssertTrue(not G.ReleaseExists);

    AssertTrue(G.Mesa);
    AssertTrue(G.VendorMajor = 7);
    AssertTrue(G.VendorMinor = 1);
    AssertTrue(G.VendorRelease = 0);
  finally FreeAndNil(G) end;

  G := TGLVersion.Create('1.2.3', 'foobar', '');
  try
    AssertTrue(G.VendorVersion = '');
    AssertTrue(G.Vendor = 'foobar');
    AssertTrue(G.Renderer = '');

    AssertTrue(G.Major = 1);
    AssertTrue(G.Minor = 2);
    AssertTrue(G.ReleaseExists);
    AssertTrue(G.Release = 3);
    AssertTrue(not G.Mesa);
  finally FreeAndNil(G) end;

  G := TGLVersion.Create('4.2.0 NVIDIA 304.108', 'NVIDIA Corporation', 'GeForce GTS 450/PCIe/SSE2/3DNOW!');
  try
    AssertEquals('NVIDIA 304.108', G.VendorVersion);
    AssertEquals('NVIDIA Corporation', G.Vendor);
    AssertEquals('GeForce GTS 450/PCIe/SSE2/3DNOW!', G.Renderer);

    AssertEquals(4, G.Major);
    AssertEquals(2, G.Minor);
    AssertEquals(true, G.ReleaseExists);
    AssertEquals(0, G.Release);

    AssertEquals(304, G.VendorMajor);
    AssertEquals(108, G.VendorMinor);
    AssertEquals(0, G.VendorRelease);

    AssertTrue(not G.Mesa);
    AssertTrue(G.VendorType = gvNvidia);
  finally FreeAndNil(G) end;
end;

procedure TTestGLVersion.Test2;
var
  G: TGLVersion;
begin
  G := TGLVersion.Create(
    '4.5 (Compatibility Profile) Mesa 19.3.4',
    'X.Org',
    'AMD Radeon HD 8600 Series (OLAND, DRM 3.36.0, 5.5.7-150.current, LLVM 9.0.0)');
  try
    // parsed from version string
    AssertTrue(G.Major = 4);
    AssertTrue(G.Minor = 5);
    AssertTrue(not G.ReleaseExists);
    AssertTrue(G.VendorVersion = '(Compatibility Profile) Mesa 19.3.4');
    AssertTrue(G.Mesa);
    // AssertTrue(G.VendorMajor = 19);
    // AssertTrue(G.VendorMinor = 3);
    // AssertTrue(G.VendorRelease = 4);

    // AssertTrue(G.Vendor = 'X.Org');
    // AssertTrue(G.Renderer = 'AMD Radeon HD 8600 Series (OLAND, DRM 3.36.0, 5.5.7-150.current, LLVM 9.0.0)');
  finally FreeAndNil(G) end;
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestGLVersion);
{$endif}
end.
