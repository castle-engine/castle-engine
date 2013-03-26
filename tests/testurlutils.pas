{
  Copyright 2009-2013 Michalis Kamburelis.

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
  Assert(URIProtocol('data:blah:foo') = 'data');
  Assert(URIProtocolIs('data:blah:foo', 'data', Colon));
  Assert(not URIProtocolIs('data:blah:foo', 'data1', Colon));
  Assert(not URIProtocolIs('data:blah:foo', 'dat', Colon));
  Assert(not URIProtocolIs('data', 'data', Colon));
  Assert(not URIProtocolIs('', 'data', Colon));

  Assert(URIProtocol('ecmascript:xyz') = 'ecmascript');
  Assert(URIDeleteProtocol('ecmascript:xyz') = 'xyz');

  Assert(URIProtocol('     ' + NL + '    ecmascript:xyz') = 'ecmascript');
  Assert(URIDeleteProtocol('     ' + NL + '    ecmascript:xyz') = 'xyz');

  Assert(URIProtocol('void main()' + NL + 'ecmascript:xyz') = '');
  Assert(URIDeleteProtocol('void main()' + NL + 'ecmascript:xyz') = 'void main()' + NL + 'ecmascript:xyz');
end;

initialization
  RegisterTest(TTestURLUtils);
end.
