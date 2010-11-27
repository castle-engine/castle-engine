{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3D backgrounds. In the simplest case, this is just a
  textured cube around the player.
  This unit provides common stuff, used by OpenGL programs
  (see unit BackgroundGL) and for VRML TNodeBackground. }
unit BackgroundBase;

interface

uses Images, TextureImages;

type
  { }
  TBackgroundSide = (bsBack, bsBottom, bsFront, bsLeft, bsRight, bsTop);
  TBackgroundSides = set of TBackgroundSide;
  TBackgroundImages = array [TBackgroundSide] of TEncodedImage;

const
  BGAllSides: TBackgroundSides = [Low(TBackgroundSide) .. High(TBackgroundSide)];
  BGHorizonSides: TBackgroundSides = [bsBack, bsFront, bsLeft, bsRight];

  BackgroundImagesNone: TBackgroundImages =
  ( nil, nil, nil, nil, nil, nil );

function BackgroundImages(const BackImg, BottomImg, FrontImg, LeftImg,
  RightImg, TopImg: TImage): TBackgroundImages;

{ Release and set to @nil all images in BgImages.
  If Cache is @nil, release images by simple FreeAndNil,
  otherwise they are released from the cache by TextureImage_DecReference. }
procedure BackgroundImagesFreeAll(var BGImages: TBackgroundImages;
  Cache: TTexturesImagesVideosCache);

implementation

uses SysUtils, KambiUtils;

{ TBackgroundImages -------------------------------------------------------- }

function BackgroundImages(const BackImg, BottomImg, FrontImg, LeftImg,
  RightImg, TopImg: TImage): TBackgroundImages;
begin
 result[bsBack  ] := BackImg;
 result[bsBottom] := BottomImg;
 result[bsFront ] := FrontImg;
 result[bsLeft  ] := LeftImg;
 result[bsRight ] := RightImg;
 result[bsTop   ] := TopImg;
end;

procedure BackgroundImagesFreeAll(var BGImages: TBackgroundImages;
  Cache: TTexturesImagesVideosCache);
var
  bs: TBackgroundSide;
begin
  if Cache <> nil then
  begin
    for bs := Low(bs) to High(bs) do
      { Cache.TextureImage_DecReference is not prepared for nil parameters,
        and some of our BGImages[bs] may be nil. }
      if BGImages[bs] <> nil then
        Cache.TextureImage_DecReference(BGImages[bs]);
  end else
  begin
    for bs := Low(bs) to High(bs) do FreeAndNil(BGImages[bs]);
  end;
end;

end.
