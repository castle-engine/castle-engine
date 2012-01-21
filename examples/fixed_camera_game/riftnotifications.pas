{
  Copyright 2006-2012 Michalis Kamburelis.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Game notifications.
  Just a copy of analogous "castle" unit for now. }
unit RiftNotifications;

interface

uses CastleNotifications;

var
  Notifications: TCastleNotifications;

implementation

uses SysUtils, VectorMath;

initialization
  Notifications := TCastleNotifications.Create(nil);
  Notifications.MaxMessages := 4;
  Notifications.Color := Vector3Single(0.8, 0.8, 0.8);
finalization
  FreeAndNil(Notifications);
end.
