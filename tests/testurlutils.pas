{
  Copyright 2009-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestURLUtils;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleURLUtils;

type
  TTestURLUtils = class(TTestCase)
  published
    procedure TestUrlProtocol;
  end;

implementation

uses CastleUtils;

procedure TTestURLUtils.TestUrlProtocol;
var
  Colon: Integer;
begin
  Assert(UrlProtocol('data:blah:foo') = 'data');
  Assert(UrlProtocolIs('data:blah:foo', 'data', Colon));
  Assert(not UrlProtocolIs('data:blah:foo', 'data1', Colon));
  Assert(not UrlProtocolIs('data:blah:foo', 'dat', Colon));
  Assert(not UrlProtocolIs('data', 'data', Colon));
  Assert(not UrlProtocolIs('', 'data', Colon));

  Assert(UrlProtocol('ecmascript:xyz') = 'ecmascript');
  Assert(UrlDeleteProtocol('ecmascript:xyz') = 'xyz');

  Assert(UrlProtocol('     ' + NL + '    ecmascript:xyz') = 'ecmascript');
  Assert(UrlDeleteProtocol('     ' + NL + '    ecmascript:xyz') = 'xyz');

  Assert(UrlProtocol('void main()' + NL + 'ecmascript:xyz') = '');
  Assert(UrlDeleteProtocol('void main()' + NL + 'ecmascript:xyz') = 'void main()' + NL + 'ecmascript:xyz');
end;

initialization
  RegisterTest(TTestURLUtils);
end.
