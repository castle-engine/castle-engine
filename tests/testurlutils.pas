{
  Copyright 2009 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
