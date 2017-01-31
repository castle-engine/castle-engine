{
  Copyright 2007-2017 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftData;

interface

uses CastleXMLConfig;

var
  DataConfig: TCastleConfig;

implementation

uses SysUtils, CastleFilesUtils, CastleURIUtils;

initialization
  DataConfig := TCastleConfig.Create(nil);
  DataConfig.URL := ApplicationData('index.xml');
finalization
  FreeAndNil(DataConfig);
end.
