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

{ Load background textures from files named in old "panoramaToSzescian"
  convention. This is deprecated, used only by old "szklane lasy"
  and old "malfunction" versions. @deprecated

  SkyNamePattern is the base filename. To construct actual filename,
  we will append to them '_' (underscore character) followed by one
  letter indicating cube side:

  @unorderedList(
    @item 'u'/ = (up) top
    @item 'd'/ = (down) bottom
    @item 'l'/ = right (!)
    @item 'r'/ = left  (!)
    @item 'f'/ = front
    @item 'b'  = back
  )

  If file for any cube side will not exist, we will try appending
  '_any' (useful if some sides use the same texture, for example top
  and bottom are sometimes just one black pixel).

  Some reasoning:

  @orderedList(
    @item('u' / 'd' were chosen to name up / down, more commonly
      (in VRML/X3D) named top / bottom. Reason: "bottom" and "back"
      would otherwise start with the same letter.)

    @item(Note (!) that left / right textures are swapped.
      Reason: I defined it like this in "panoramaToSzescian" and
      much later realized VRML Background node (and so my TBackground class)
      has it exactly inverted.

      (In "panoramaToSzescian" one the images sequence
      @italic(front, left, back, right) were matching
      when show in that order. In VRML/X3D the matching image sequence
      is @italic(front, right, back, left).))
  )

  Filename extension for textures is guessed by FindExistingImageExt.
  Images will be loaded by LoadTextureImage(FileName)
  so they will be forced into some format renderable as OpenGL texture. }
function BackgroundImagesLoadFromOldNamePattern(
  const SkyNamePattern: string): TBackgroundImages;

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

function BackgroundImagesLoadFromOldNamePattern(
  const SkyNamePattern: string): TBackgroundImages;
const
  names_suffix: array[TBackgroundSide]of string = ('b', 'd', 'f', 'r', 'l', 'u');
var
  ImgFileName: string;
  bs: TBackgroundSide;
begin
 for bs := Low(bs) to High(bs) do
 begin
  ImgFileName :=
    TryFindExistingImageExt(SkyNamePattern +'_' +names_suffix[bs], true);
  if ImgFileName = '' then
   ImgFileName := FindExistingImageExt(SkyNamePattern +'_any', true);
  result[bs] := LoadTextureImage(ImgFileName);
 end;
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
