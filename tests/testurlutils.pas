{
  Copyright 2009-2014 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleURIUtils;

type
  TTestURLUtils = class(TTestCase)
  published
    procedure TestURIProtocol;
  end;

implementation

uses CastleUtils;

procedure TTestURLUtils.TestURIProtocol;
var
  Colon: Integer;
begin
  AssertTrue(URIProtocol('data:blah:foo') = 'data');
  AssertTrue(URIProtocolIs('data:blah:foo', 'data', Colon));
  AssertTrue(not URIProtocolIs('data:blah:foo', 'data1', Colon));
  AssertTrue(not URIProtocolIs('data:blah:foo', 'dat', Colon));
  AssertTrue(not URIProtocolIs('data', 'data', Colon));
  AssertTrue(not URIProtocolIs('', 'data', Colon));

  AssertTrue(URIProtocol('ecmascript:xyz') = 'ecmascript');
  AssertTrue(URIDeleteProtocol('ecmascript:xyz') = 'xyz');

  AssertTrue(URIProtocol('     ' + NL + '    ecmascript:xyz') = 'ecmascript');
  AssertTrue(URIDeleteProtocol('     ' + NL + '    ecmascript:xyz') = 'xyz');

  AssertTrue(URIProtocol('void main()' + NL + 'ecmascript:xyz') = '');
  AssertTrue(URIDeleteProtocol('void main()' + NL + 'ecmascript:xyz') = 'void main()' + NL + 'ecmascript:xyz');
end;

initialization
  RegisterTest(TTestURLUtils);
end.
