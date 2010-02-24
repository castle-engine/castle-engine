{
  Copyright 2009-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestURLUtils;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, KambiURLUtils;

type
  TTestURLUtils = class(TTestCase)
  published
    procedure TestUrlProtocol;
  end;

implementation

procedure TTestURLUtils.TestUrlProtocol;
begin
  Assert(UrlProtocol('data:blah:foo') = 'data');
  Assert(UrlProtocolIs('data:blah:foo', 'data'));
  Assert(not UrlProtocolIs('data:blah:foo', 'data1'));
  Assert(not UrlProtocolIs('data:blah:foo', 'dat'));
  Assert(not UrlProtocolIs('data', 'data'));
  Assert(not UrlProtocolIs('', 'data'));
end;

initialization
  RegisterTest(TTestURLUtils);
end.
