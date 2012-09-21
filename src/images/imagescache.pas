{
  Copyright 2008-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A cache for loading images (TImagesCache). }
unit ImagesCache;

interface

uses CastleUtils, Images, FGL;

type
  TImagesCache = class
  private
    FOnEmpty: TProcedure;
  protected
    { If cache is empty, calls OnEmpty. Note that OnEmpty may destroy current
      instance, so call CheckEmpty only when you finished processing
      --- Self may be invalid afterwards. }
    procedure CheckEmpty;
  public
    function Empty: boolean; virtual;

    { Called when cache becomes empty. This is only for internal usage
      by X3DNodes unit for now. }
    property OnEmpty: TProcedure read FOnEmpty write FOnEmpty;
  end;

implementation

uses SysUtils, CastleStringUtils;

{ $define DEBUG_CACHE}

procedure TImagesCache.CheckEmpty;
begin
  { Check Assigned(OnEmpty) first, as it's usually not assigned. }
  if Assigned(OnEmpty) and Empty then
    OnEmpty();
end;

function TImagesCache.Empty: boolean;
begin
  Result := true;
end;

end.