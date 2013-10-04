{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2008-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestGLVersion;

interface

uses fpcunit, testutils, testregistry;

type
  TTestGLVersion = class(TTestCase)
  published
    procedure Test1;
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
    Assert(G.VendorInfo = '(2.1 Mesa 7.0.4)');
    Assert(G.Vendor = 'Mesa project: www.mesa3d.org');
    Assert(G.Renderer = 'Mesa GLX Indirect');

    Assert(G.Major = 1);
    Assert(G.Minor = 4);

    { Test AtLeast method }
    Assert(G.AtLeast(0, 9));
    Assert(G.AtLeast(1, 0));
    Assert(G.AtLeast(1, 3));
    Assert(G.AtLeast(1, 4));
    Assert(not G.AtLeast(1, 5));
    Assert(not G.AtLeast(2, 0));

    Assert(not G.ReleaseExists);

    Assert(G.Mesa);
    Assert(G.VendorMajor = 7);
    Assert(G.VendorMinor = 0);
    Assert(G.VendorRelease = 4);
  finally FreeAndNil(G) end;

  G := TGLVersion.Create('2.1 Mesa 7.1', 'Brian Paul',
    'Mesa DRI Intel(blah blah blah)');
  try
    Assert(G.VendorInfo = 'Mesa 7.1');
    Assert(G.Vendor = 'Brian Paul');
    Assert(G.Renderer = 'Mesa DRI Intel(blah blah blah)');

    Assert(G.Major = 2);
    Assert(G.Minor = 1);

    Assert(not G.ReleaseExists);

    Assert(G.Mesa);
    Assert(G.VendorMajor = 7);
    Assert(G.VendorMinor = 1);
    Assert(G.VendorRelease = 0);
  finally FreeAndNil(G) end;

  G := TGLVersion.Create('1.2.3', 'foobar', '');
  try
    Assert(G.VendorInfo = '');
    Assert(G.Vendor = 'foobar');
    Assert(G.Renderer = '');

    Assert(G.Major = 1);
    Assert(G.Minor = 2);
    Assert(G.ReleaseExists);
    Assert(G.Release = 3);
    Assert(not G.Mesa);
  finally FreeAndNil(G) end;

  G := TGLVersion.Create('4.2.0 NVIDIA 304.108', 'NVIDIA Corporation', 'GeForce GTS 450/PCIe/SSE2/3DNOW!');
  try
    AssertEquals('NVIDIA 304.108', G.VendorInfo);
    AssertEquals('NVIDIA Corporation', G.Vendor);
    AssertEquals('GeForce GTS 450/PCIe/SSE2/3DNOW!', G.Renderer);

    AssertEquals(4, G.Major);
    AssertEquals(2, G.Minor);
    AssertEquals(true, G.ReleaseExists);
    AssertEquals(0, G.Release);

    AssertEquals(304, G.VendorMajor);
    AssertEquals(108, G.VendorMinor);
    AssertEquals(0, G.VendorRelease);

    Assert(not G.Mesa);
    Assert(not G.VendorATI);
    Assert(not G.VendorIntel);
    Assert(G.VendorNVidia);
  finally FreeAndNil(G) end;
end;

initialization
  RegisterTest(TTestGLVersion);
end.
