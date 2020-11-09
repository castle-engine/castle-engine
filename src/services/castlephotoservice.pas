{
  Copyright 2017-2018 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Operating system Photo library integration (TPhotoService). }
unit CastlePhotoService;

{$I castleconf.inc}

interface

uses Classes;

type
  { TPhotoService enables to store/retrieve images to/from the system Photos
    app. It may also request user to take a new photo with the camera.

    At the moment, only storing images is implemented.
    }
  TPhotoService = class(TComponent)
  public
    class procedure StoreImage(APath: String);
  end;

implementation

uses CastleMessaging;

class procedure TPhotoService.StoreImage(APath: String);
begin
  Messaging.Send(['photoservice-store-image', APath]);
end;

end.
